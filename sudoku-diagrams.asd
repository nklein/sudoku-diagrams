;;;; sudoku-diagrams.asd

(asdf:defsystem #:sudoku-diagrams
  :description "Utilities for generating sudoku diagrams"
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.2.20220426"
  :license "UNLICENSE"
  :depends-on (#:alexandria)
  :components
  ((:static-file "README.md")
   (:static-file "UNLICENSE.txt")
   (:module "src"
    :components ((:file "package")
                 (:file "utils" :depends-on ("package"))
                 (:file "sudoku-utils" :depends-on ("package"))
                 (:file "areas" :depends-on ("package"
                                             "utils"
                                             "sudoku-utils"))
                 (:file "known-areas" :depends-on ("package"
                                                   "utils"
                                                   "sudoku-utils"
                                                   "areas"))
                 (:file "diagram" :depends-on ("package"
                                               "sudoku-utils"
                                               "areas"
                                               "known-areas"))))))
