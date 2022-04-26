;;;; sudoku-diagrams-pdf.asd

(asdf:defsystem #:sudoku-diagrams-pdf
  :description "Utilities for outputting sudoku diagrams to PDF"
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20220425"
  :license "UNLICENSE"
  :depends-on (#:alexandria #:cl-pdf #:sudoku-diagrams)
  :components
  ((:static-file "README.md")
   (:static-file "UNLICENSE.txt")
   (:module "src/pdf"
    :components ((:file "package")
                 (:file "utils" :depends-on ("package"))
                 (:file "style" :depends-on ("package"
                                             "utils"))
                 (:file "pdf-utils" :depends-on ("package"
                                                 "utils"
                                                 "style"))
                 (:file "diagram" :depends-on ("package"
                                               "utils"
                                               "pdf-utils"
                                               "style"))))))
