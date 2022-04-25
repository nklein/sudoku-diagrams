(in-package #:sudoku-diagrams)

(alexandria:define-constant +sudoku-digits+ 9)
(alexandria:define-constant +sudoku-rows+ 9)
(alexandria:define-constant +sudoku-columns+ 9)
(alexandria:define-constant +sudoku-boxes+ 9)
(alexandria:define-constant +sudoku-cells+ (* +sudoku-rows+ +sudoku-columns+))

;;; other code here depends on these being equal
(assert (= +sudoku-digits+ +sudoku-rows+ +sudoku-columns+ +sudoku-boxes+))

;;; digits
(deftype sudoku-digit ()
  `(integer 1 ,+sudoku-digits+))

(deftypep sudoku-digit-p sudoku-digit)
(deflisttype list-of-sudoku-digit list-of-sudoku-digit-p sudoku-digit-p)

;;; rows
(deftype sudoku-row-number ()
  `(integer 1 ,+sudoku-rows+))

(deftypep sudoku-row-number-p sudoku-row-number)
(deflisttype list-of-sudoku-row-number list-of-sudoku-row-number-p sudoku-row-number-p)

;;; columns
(deftype sudoku-column-number ()
  `(integer 1 ,+sudoku-columns+))

(deftypep sudoku-column-number-p sudoku-column-number)
(deflisttype list-of-sudoku-column-number list-of-sudoku-column-number-p sudoku-column-number-p)

;;; indexes
(deftype sudoku-index ()
  `(integer 0 ,(1- +sudoku-cells+)))

(deftypep sudoku-index-p sudoku-index)
(deflisttype list-of-sudoku-index list-of-sudoku-index-p sudoku-index-p)

(declaim (inline row-col-index))
(defun row-col-index (r c)
  (check-type r sudoku-row-number)
  (check-type c sudoku-column-number)
  (+ (* (1- r) +sudoku-columns+) (1- c)))
