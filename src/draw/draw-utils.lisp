(in-package #:sudoku-diagrams-draw)

(defun compute-string-metrics (string font font-size)
  (let ((ascender 0)
        (descender (draw:get-font-descender font font-size))
        (width 0))
    (loop :for prev-char := nil :then char
       :for char :across string
       :do (when prev-char
             (incf width (draw:get-kerning prev-char char font font-size)))
       :do (multiple-value-bind (char-width char-ascender char-descender)
               (draw:get-char-size char font font-size)
             (setf ascender (max ascender char-ascender))
             (setf descender (min descender char-descender))
             (incf width char-width)))
    (values width ascender descender)))

(defun invoke-with-cell-bounds (fn index cell-width cell-height)
  (with-index-row-col (r c) index
    (let ((cx (* (1- c) cell-width))
          (cy (* (- +sudoku-rows+ r) cell-height)))
      (funcall fn cx cy cell-width cell-height))))

(defun make-cell-labeler (string font font-size)
  (lambda (cx cy cell-width cell-height)
    (let* ((string-width (compute-string-metrics string font font-size))
           (x (+ cx (/ (- cell-width string-width) 2)))
           (y (+ cy (- cell-height font-size))))
      (draw:in-text-mode
        (draw:set-font font font-size)
        (draw:move-text x y)
        (draw:draw-text string)))))

(defun make-cell-diamond-marker (proportion stroke-color stroke-width fill-color)
  (lambda (cx cy cell-width cell-height)
    (let* ((ox (/ (* cell-width (- 1 proportion)) 2))
           (oy (/ (* cell-height (- 1 proportion)) 2))
           (left (+ cx ox))
           (right (+ cx cell-width (- ox)))
           (top (+ cy cell-height (- oy)))
           (bottom (+ cy oy))
           (hmid (/ (+ left right) 2))
           (vmid (/ (+ top bottom) 2)))
      (apply #'draw:set-rgb-stroke stroke-color)
      (apply #'draw:set-rgb-fill fill-color)
      (draw:set-line-width stroke-width)
      (draw:move-to hmid bottom)
      (draw:line-to right vmid)
      (draw:line-to hmid top)
      (draw:line-to left vmid)
      (draw:close-fill-and-stroke))))

(defun make-cell-circle-marker (proportion stroke-color stroke-width fill-color)
  (lambda (cx cy cell-width cell-height)
    (let ((hmid (+ cx (/ cell-width 2)))
          (vmid (+ cy (/ cell-height 2)))
          (radius (* (/ (+ cell-width cell-height) 4)
                     proportion)))
      (apply #'draw:set-rgb-stroke stroke-color)
      (apply #'draw:set-rgb-fill fill-color)
      (draw:set-line-width stroke-width)
      (draw:circle hmid vmid radius)
      (draw:close-fill-and-stroke))))

(defun get-marker-function (marker)
  (alexandria:eswitch (marker :test #'string=)
    ("light-diamond"
     (make-cell-diamond-marker *diamond-proportion*
                               *diamond-stroke-rgb*
                               *diamond-stroke-width*
                               *light-diamond-fill-rgb*))
    ("dark-diamond"
     (make-cell-diamond-marker *diamond-proportion*
                               *diamond-stroke-rgb*
                               *diamond-stroke-width*
                               *dark-diamond-fill-rgb*))
    ("light-circle"
     (make-cell-circle-marker *circle-proportion*
                               *circle-stroke-rgb*
                               *circle-stroke-width*
                               *light-circle-fill-rgb*))
    ("dark-circle"
     (make-cell-circle-marker *circle-proportion*
                               *circle-stroke-rgb*
                               *circle-stroke-width*
                               *dark-circle-fill-rgb*))))
