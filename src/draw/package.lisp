(defpackage #:sudoku-diagrams-draw
  (:use :cl :sudoku-diagrams)

  (:export :points
           :inches)

  (:export :*title-font*
           :*title-font-rgb*
           :*title-font-proportion*

           :*row-column-label-font*
           :*row-column-label-font-rgb*
           :*row-column-label-font-proportion*

           :*digit-font*
           :*digit-font-rgb*
           :*given-digit-font-proportion*

           :*cell-stroke-rgb*
           :*cell-stroke-width*
           :*cell-fill-rgb*
           :*box-stroke-rgb*
           :*box-stroke-width*
           :*grid-stroke-rgb*
           :*grid-stroke-width*

           :*highlight-fill-rgb*

           :*diamond-stroke-rgb*
           :*diamond-stroke-width*
           :*diamond-proportion*
           :*dark-diamond-fill-rgb*
           :*light-diamond-fill-rgb*)

  (:export :draw-sudoku-diagram
           :draw-sudoku-diagram-to-file))
