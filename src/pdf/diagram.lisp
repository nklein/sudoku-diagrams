(in-package #:sudoku-diagrams-pdf)

(defun maximum-string-width (strings size)
  (let ((font (pdf:get-font *row-column-label-font*)))
    (loop :for string :in strings
       :maximizing (compute-string-metrics string font size))))

(defun get-extra-rows-columns (diagram)
  (let* ((row-labels (sudoku-diagram-row-labels diagram))
         (extra-left (if (sudoku-diagram-row-labels diagram)
                        (+ (maximum-string-width row-labels *row-column-label-font-proportion*) 1/4)
                        0))
         (extra-right 0)
         (extra-top (if (sudoku-diagram-name diagram) (+ *title-font-proportion* 1/4) 0))
         (extra-bottom (if (sudoku-diagram-column-labels diagram) (+ *row-column-label-font-proportion* 1/4) 0)))
    (values extra-left extra-right extra-top extra-bottom)))

(defun aspect-ratio-for-diagram (diagram)
  (multiple-value-bind (left right top bottom) (get-extra-rows-columns diagram)
    (/ (+ +sudoku-rows+ left right)
       (+ +sudoku-columns+ top bottom))))

(defun draw-sudoku-grid-cells (cell-width cell-height)
  (let ((oo (/ *cell-stroke-width* 4)))
    (loop :for y :below +sudoku-rows+
       :for cy := (* y cell-height)
       :do (loop :for x :below 9
              :for cx := (* x cell-width)
              :do (pdf:rectangle (- cx oo) (- cy oo)
                                 (+ cell-width oo oo)
                                 (+ cell-height oo oo)))))
  (pdf:close-and-stroke))

(defun draw-sudoku-grid-boxes (box-width box-height)
  (let ((oo (/ *box-stroke-width* 4)))
    (loop :for y :below 3
       :for by := (* y box-height)
       :do (loop :for x :below 3
              :for bx := (* x box-width)
              :do (pdf:rectangle (- bx oo) (- by oo)
                                 (+ box-width oo oo)
                                 (+ box-height oo oo)))))
  (pdf:close-and-stroke))

(defun draw-sudoku-grid-outline (width height)
  (let ((oo (/ *grid-stroke-width* 4)))
    (pdf:rectangle (- oo) (- oo) (+ width oo oo) (+ height oo oo)))
  (pdf:close-and-stroke))

(defun draw-sudoku-grid-background (width height)
  (apply #'pdf:set-rgb-fill *cell-fill-rgb*)
  (pdf:rectangle 0 0 width height)
  (pdf:close-and-fill))

(defun draw-sudoku-grid (width height)
  (let ((box-width (/ width +sudoku-box-columns+))
        (box-height (/ height +sudoku-box-rows+))
        (cell-width (/ width +sudoku-columns+))
        (cell-height (/ height +sudoku-rows+)))

    (apply #'pdf:set-rgb-stroke *cell-stroke-rgb*)
    (pdf:set-line-width *cell-stroke-width*)
    (draw-sudoku-grid-cells cell-width cell-height)

    (apply #'pdf:set-rgb-stroke *box-stroke-rgb*)
    (pdf:set-line-width *box-stroke-width*)
    (draw-sudoku-grid-boxes box-width box-height)
    (pdf:close-and-stroke)

    (apply #'pdf:set-rgb-stroke *grid-stroke-rgb*)
    (pdf:set-line-width *grid-stroke-width*)
    (draw-sudoku-grid-outline width height)))

(defun draw-highlighted (highlighted-areas width height)
  (let ((cell-width (/ width +sudoku-columns+))
        (cell-height (/ height +sudoku-rows+)))

    (apply #'pdf:set-rgb-fill *highlight-fill-rgb*)
    (dolist (area highlighted-areas)
      (dolist (index (sudoku-grid-area-indexes area))
        (with-index-row-col (r c) index
          (pdf:rectangle (* (1- c) cell-width) (* (- +sudoku-rows+ r) cell-height) cell-width cell-height))))
    (pdf:close-and-fill)))

(defun draw-marked (marked width height)
  (let* ((cell-width (/ width +sudoku-columns+))
         (cell-height (/ height +sudoku-rows+)))

    (apply #'pdf:set-rgb-fill *digit-font-rgb*)
    (loop :for (marker areas) :on marked :by #'cddr
       :for marker-fn := (get-marker-function marker)
       :do (dolist (area (alexandria:ensure-list areas))
             (dolist (index (sudoku-grid-area-indexes area))
               (invoke-with-cell-bounds marker-fn index cell-width cell-height))))))

(defun draw-givens (givens width height)
  (let* ((cell-width (/ width +sudoku-columns+))
         (cell-height (/ height +sudoku-rows+))
         (font (pdf:get-font "Helvetica"))
         (font-size (* *given-digit-font-proportion* cell-height)))

    (apply #'pdf:set-rgb-fill *digit-font-rgb*)
    (loop :for (digit area) :on givens :by #'cddr
       :for digit-string := (format nil "~D" digit)
       :do (dolist (index (sudoku-grid-area-indexes area))
             (let ((labeler (make-cell-labeler digit-string font font-size)))
               (invoke-with-cell-bounds labeler index cell-width cell-height))))))

(defun draw-centered-title (title diagram-width cell-height)
  (when title
    (let* ((font (pdf:get-font *title-font*))
           (font-size (* *title-font-proportion* cell-height))
           (width (compute-string-metrics title font font-size)))
      (pdf:in-text-mode
        (pdf:set-font font font-size)
        (apply #'pdf:set-rgb-fill *title-font-rgb*)
        (pdf:move-text (/ (- diagram-width width) 2) (- (/ cell-height 4) font-size))
        (pdf:draw-text title)))))

(defun draw-sudoku-grid-itself (diagram grid-width grid-height)
  (draw-sudoku-grid-background grid-width grid-height)
  (draw-highlighted (sudoku-diagram-highlighted diagram) grid-width grid-height)
  (draw-marked (sudoku-diagram-marked diagram) grid-width grid-height)
  (draw-givens (sudoku-diagram-givens diagram) grid-width grid-height)
  (draw-sudoku-grid grid-width grid-height))

(defun draw-column-labels (strings cell-width cell-height x-offset)
  (let ((font (pdf:get-font *row-column-label-font*))
        (font-size (* *row-column-label-font-proportion* cell-height)))
    (pdf:in-text-mode
      (pdf:set-font font font-size)
      (apply #'pdf:set-rgb-fill *row-column-label-font-rgb*)
      (loop :for string :in strings
         :for cx :from x-offset :by cell-width
         :do (let ((width (compute-string-metrics string font font-size)))
               (pdf:with-saved-state
                 (pdf:move-text (+ cx (/ (- cell-width width) 2))
                                (- font-size *grid-stroke-width* (/ cell-height 4)))
                 (pdf:draw-text string)))))))

(defun draw-row-labels (strings cell-width cell-height)
  (let* ((font (pdf:get-font *row-column-label-font*))
         (font-size (* *row-column-label-font-proportion* cell-height))
         (max-width (maximum-string-width strings font-size)))
    (pdf:in-text-mode
      (pdf:set-font font font-size)
      (apply #'pdf:set-rgb-fill *row-column-label-font-rgb*)
      (loop :for string :in strings
         :for cy :from (* +sudoku-rows+ cell-height) :by (- cell-height)
         :for cx :from (/ cell-width 2) :by cell-width
         :do (let ((width (compute-string-metrics string font font-size)))
               (pdf:with-saved-state
                 (pdf:move-text (- max-width width *grid-stroke-width*)
                                cy)
                 #+not
                 (pdf:move-text (- cell-width (+ (/ cell-width 4) *grid-stroke-width* width))
                                cy)
                 (pdf:draw-text string)))))))

(defun sudoku-diagram-as-pdf (diagram nominal-width nominal-height)
  (pdf:with-saved-state
    (labels ((limit (n max-n)
               (cond
                 ((minusp n)
                  (- (limit (- n) max-n)))
                 (max-n
                  (min n max-n))
                 (t
                  n)))

             (limit-width (w aspect-ratio)
               (let ((by-width (limit w nominal-width))
                     (by-height (* (limit (/ w aspect-ratio) nominal-height)
                                   aspect-ratio)))
                 (format t "~D + ~D => ~D, ~D~%"
                         (* 1.0 w) (* 1.0 aspect-ratio)
                         (* 1.0 by-width) (* 1.0 by-height))
                 (min by-width by-height))))

      (multiple-value-bind (left right top bottom) (get-extra-rows-columns diagram)
        (let* ((effective-rows (+ +sudoku-rows+ top bottom))
               (effective-columns (+ +sudoku-columns+ left right))
               (aspect-ratio (/ effective-columns effective-rows))
               (width (limit-width nominal-width aspect-ratio))
               (height (/ width aspect-ratio))
               (cell-width (/ width effective-columns))
               (cell-height (/ height effective-rows))
               (grid-width (* cell-width +sudoku-columns+))
               (grid-height (* cell-height +sudoku-rows+)))

          (pdf:translate (/ (- nominal-width width) 2) (/ (- height nominal-height) 2))

          (draw-centered-title (sudoku-diagram-name diagram) width cell-height)

          (pdf:translate 0 (- height))

          (pdf:with-saved-state
            (pdf:translate (* cell-width left) (* cell-height bottom))
            (draw-sudoku-grid-itself diagram grid-width grid-height))

          (draw-row-labels (sudoku-diagram-row-labels diagram) cell-width cell-height)
          (draw-column-labels (sudoku-diagram-column-labels diagram)
                              cell-width cell-height
                              (* left cell-width))
          (values width height))))))

(defmacro destructure-bounds ((x y w h) bounds &body body)
  (let ((b (gensym "BOUNDS")))
    `(let* ((,b ,bounds)
            (,x (elt ,b 0))
            (,y (elt ,b 1))
            (,w (- (elt ,b 2) ,x))
            (,h (- (elt ,b 3) ,y)))
       ,@body)))

(defun sudoku-diagram-to-pdf-file (filename diagram width height)
  (pdf:with-document ()
    (pdf:with-page (:bounds (vector 0 0 (+ width (inches 1)) (+ height (inches 1))))
      (pdf:set-line-width 2)
      (destructure-bounds (x y w h) (pdf:bounds pdf:*page*)
        (pdf:translate (+ x (/ w 2)) (+ y h (inches -0.5))))
      (pdf:translate (/ (- width) 2) 0)
      (sudoku-diagram-as-pdf diagram width height))
    (pdf:write-document filename)))
