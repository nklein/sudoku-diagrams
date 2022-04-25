SUDOKU-DIAGRAMS
===============

This library defines a way to declare sudoku diagrams and export them in various formats.

Currently, the library is just getting started and only supports declaring the diagram and having
the diagram described in text.

Declaring A Diagram
-------------------

Here is an example of defining a diagram:

    (make-sudoku-diagram :name "test diagram"
                         :givens '(9 :r3c5 3 :r6c8)
                         :highlighted '(:b4 :b2r3)
                         :marked '("diamond" :r3c4))

This object will be pretty-printed as a description suitable for `alt` text on the diagram:

    Test diagram
     - These cells are given:
       - 9 in row 3 column 5.
       - 3 in row 6 column 8.
     - These cells are highlighted:
       - All of box 4.
       - Row 3 in box 2.
     - These cells are marked with a diamond:
       - Row 3 column 4.
