;;;; sudoku-diagrams-draw.asd

(asdf:defsystem #:sudoku-diagrams-draw
  :description "Utilities for outputting sudoku diagrams to DRAW"
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.3.20220429"
  :license "UNLICENSE"
  :depends-on (#:alexandria
               (:version #:draw "0.1.20220428")
               (:version #:sudoku-diagrams "0.2.20220426"))
  :components
  ((:static-file "README.md")
   (:static-file "UNLICENSE.txt")
   (:module "src/draw"
    :components ((:file "package")
                 (:file "utils" :depends-on ("package"))
                 (:file "style" :depends-on ("package"
                                             "utils"))
                 (:file "draw-utils" :depends-on ("package"
                                                 "utils"
                                                 "style"))
                 (:file "diagram" :depends-on ("package"
                                               "utils"
                                               "draw-utils"
                                               "style"))))))
