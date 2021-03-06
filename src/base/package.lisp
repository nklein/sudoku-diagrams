(defpackage #:sudoku-diagrams
  (:use :cl)

  (:export :+sudoku-digits+
           :+sudoku-rows+
           :+sudoku-columns+
           :+sudoku-box-rows+
           :+sudoku-box-columns+
           :+sudoku-boxes+
           :+sudoku-cells+
           :sudoku-digit
           :sudoku-digit-p
           :list-of-sudoku-digit-p
           :list-of-sudoku-digit
           :sudoku-row-number
           :sudoku-row-number-p
           :list-of-sudoku-row-number-p
           :list-of-sudoku-row-number
           :sudoku-column-number
           :sudoku-column-number-p
           :list-of-sudoku-column-number-p
           :list-of-sudoku-column-number)

  (:export :sudoku-grid-area
           :sudoku-grid-area-handle
           :sudoku-grid-area-description
           :make-sudoku-grid-area
           :make-partial-row-area
           :make-partial-column-area
           :sudoku-grid-area-p
           :list-of-sudoku-grid-area-p
           :list-of-sudoku-grid-area)

  (:export :get-known-area
           :add-known-area
           :add-known-areas)

  (:export :sudoku-diagram
           :sudoku-diagram-name
           :sudoku-diagram-givens
           :sudoku-diagram-highlighted
           :sudoku-diagram-marked
           :sudoku-diagram-column-labels
           :sudoku-diagram-row-labels
           :make-sudoku-diagram
           :get-alt-text))
