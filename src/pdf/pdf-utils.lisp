(in-package #:sudoku-diagrams-pdf)

(defun invoke-with-cell-bounds (fn index cell-width cell-height)
  (with-index-row-col (r c) index
    (let ((cx (* (1- c) cell-width))
          (cy (* (- +sudoku-rows+ r) cell-height)))
      (funcall fn cx cy cell-width cell-height))))

(defun make-cell-labeler (string font font-size)
  (lambda (cx cy cell-width cell-height)
    (let* ((char-width (pdf:get-char-width (elt string 0) font font-size))
           (x (+ cx (/ (- cell-width char-width) 2)))
           (y (+ cy (- cell-height font-size))))
      (pdf:in-text-mode
        (pdf:set-font font font-size)
        (pdf:move-text x y)
        (pdf:draw-text string)))))

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
      (apply #'pdf:set-rgb-stroke stroke-color)
      (apply #'pdf:set-rgb-fill fill-color)
      (pdf:set-line-width stroke-width)
      (pdf:move-to hmid bottom)
      (pdf:line-to right vmid)
      (pdf:line-to hmid top)
      (pdf:line-to left vmid)
      (pdf:close-fill-and-stroke))))

(defun make-cell-circle-marker (proportion stroke-color stroke-width fill-color)
  (lambda (cx cy cell-width cell-height)
    (let ((hmid (+ cx (/ cell-width 2)))
          (vmid (+ cy (/ cell-height 2)))
          (radius (* (/ (+ cell-width cell-height) 4)
                     proportion)))
      (apply #'pdf:set-rgb-stroke stroke-color)
      (apply #'pdf:set-rgb-fill fill-color)
      (pdf:set-line-width stroke-width)
      (pdf:circle hmid vmid radius)
      (pdf:close-fill-and-stroke))))

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
