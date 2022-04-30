(ql:quickload '(:sudoku-diagrams-draw
                :draw-pdf
                :draw-vecto))

(use-package :sudoku-diagrams)
(use-package :sudoku-diagrams-draw)

(defparameter *font-directory* #P"/Users/pat/Library/Fonts/")
(defparameter *dpi* 150)
(defparameter *vecto-page-color* '(1.0 1.0 1.0 1.0))

(defparameter *d*
  (make-sudoku-diagram :name "Toroidal Miracle Sudoku"
                       :givens '(1 :r6c6)
                       :highlighted '(:b1 :b2c4 :b2c5 :b3
                                      :b4r4 :b4r5 :r4c8 :r4c9 :r5c9
                                      :b7 :r8c4 :r9c4 :r9c5 :r7c9 :r8c8 :r8c9 :b9r9)
                       :marked '("light-diamond" (:r4c8 :r4c9 :r5c9)
                                 "dark-circle" (:r2c9 :r3c8 :r3c9
                                                :r4c1 :r4c2 :r5c1))
                       :row-labels '("r1" "r2" "r3" "r4" "r5" "r6" "r7" "r8" "r9")
                       :column-labels '("c1" "c2" "c3" "c4" "c5" "c6" "c7" "c8" "c9")))

(defun font-path (filename)
  (merge-pathnames filename *font-directory*))

(defun preload-fonts ()
  ;; NotoSans: https://fonts.google.com/noto/specimen/Noto+Sans
  (let ((fonts '(#P"NotoSans-Light.ttf")))
    (dolist (font fonts)
      (draw:load-ttf-font (font-path font)))))

(defun draw-pdf-render (filename diagram width height &optional margin)
  (draw:with-renderer (draw-pdf:pdf-renderer)
    (draw-sudoku-diagram-to-file filename diagram width height
                                 :margin margin
                                 :document-thunk #'preload-fonts)))

(defun draw-vecto-render (filename diagram width height &optional margin)
  (draw:with-renderer (draw-vecto:vecto-renderer :dpi *dpi*
                                                 :vecto-page-color *vecto-page-color*)
    (draw-sudoku-diagram-to-file filename diagram width height
                                 :margin margin
                                 :document-thunk #'preload-fonts)))

(defun inches (x) (* x 72))

(let ((filename "images/sample-output.out")
      (diagram *d*)
      (width (inches 4))
      (height (inches (+ 4 3/8)))
      (margin (inches 1/4))
      (*digit-font* "NotoSans-Light")
      (*title-font* "NotoSans-Light")
      (*row-column-label-font* "NotoSans-Light"))
  (draw-pdf-render filename diagram width height margin)
  (draw-vecto-render filename diagram width height margin))
