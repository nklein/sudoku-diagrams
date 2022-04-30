(in-package #:sudoku-diagrams-draw)

(import '(sudoku-diagrams::row-col-index
          sudoku-diagrams::index-row
          sudoku-diagrams::index-column
          sudoku-diagrams::sudoku-grid-area-indexes
          sudoku-diagrams::with-index-row-col))

(alexandria:define-constant +points-per-inch+ 72)

(declaim (inline points))
(defun points (x)
  x)

(declaim (inline inches))
(defun inches (x)
  (points (* x +points-per-inch+)))
