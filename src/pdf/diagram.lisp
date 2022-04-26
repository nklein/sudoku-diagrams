(in-package #:sudoku-diagrams-pdf)

(defun aspect-ratio-for-diagram (diagram)
  (declare (ignore diagram))
  1)

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

(defun sudoku-diagram-as-pdf (diagram nominal-width nominal-height)
  (pdf:with-saved-state
    (pdf:translate 0 (- nominal-height))

    (labels ((limit (n max-n)
               (cond
                 ((minusp n)
                  (- (limit (- n) max-n)))
                 (max-n
                  (min n max-n))
                 (t
                  n)))

             (limit-width (w aspect-ratio)
               (min (limit w nominal-width)
                    (* (limit (/ w aspect-ratio) nominal-height)
                       aspect-ratio))))

      (let* ((aspect-ratio (aspect-ratio-for-diagram diagram))
             (width (limit-width nominal-width aspect-ratio))
             (height (* width aspect-ratio))
             (grid-width width)
             (grid-height height))
        (pdf:translate (/ (- nominal-width width) 2) (/ (- height nominal-height) 2))
        (draw-sudoku-grid-background grid-width grid-height)
        (draw-highlighted (sudoku-diagram-highlighted diagram) grid-width grid-height)
        (draw-marked (sudoku-diagram-marked diagram) grid-width grid-height)
        (draw-givens (sudoku-diagram-givens diagram) grid-width grid-height)
        (draw-sudoku-grid grid-width grid-height)))))

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
