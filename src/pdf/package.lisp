(defpackage #:sudoku-diagrams-pdf
  (:use :cl :sudoku-diagrams)

  (:export :points
           :inches)

  (:export :*cell-stroke-rgb*
           :*cell-stroke-width*
           :*cell-fill-width*
           :*box-stroke-rgb*
           :*box-stroke-width*
           :*grid-stroke-rgb*
           :*grid-stroke-width*

           :*highlight-fill-rgb*

           :*digit-font*
           :*digit-font-rgb*
           :*given-digit-font-proportion*

           :*diamond-stroke-rgb*
           :*diamond-stroke-width*
           :*diamond-proportion*
           :*dark-diamond-fill-rgb*
           :*light-diamond-fill-rgb*)

  (:export :sudoku-diagram-as-pdf
           :sudoku-diagram-to-pdf-file))
