(in-package #:sudoku-diagrams)

(defclass sudoku-diagram ()
  ((name :initarg :name :reader sudoku-diagram-name)
   (givens :initarg :givens :reader sudoku-diagram-givens)
   (highlighted :initarg :highlighted :reader sudoku-diagram-highlighted)
   (marked :initarg :marked :reader sudoku-diagram-marked)
   (column-labels :initarg :column-labels :reader sudoku-diagram-column-labels)
   (rows-labels :initarg :row-labels :reader sudoku-diagram-row-labels)))

(defun make-sudoku-diagram (&key
                              name
                              givens
                              highlighted
                              marked
                              column-labels
                              row-labels
                            &allow-other-keys)
  (check-type name (or null string))
  (check-type givens list)
  (check-type highlighted list)
  (check-type marked list)
  (check-type column-labels list-of-string)
  (check-type row-labels list-of-string)

  (make-instance 'sudoku-diagram
                 :name name
                 :givens (loop :for (given handle-or-area) :on givens :by #'cddr
                            :appending (list given
                                             (ensure-sudoku-grid-area handle-or-area)))
                 :highlighted (ensure-list-of-sudoku-grid-areas highlighted)
                 :marked (loop :for (mark areas) :on marked :by #'cddr
                            :appending (list mark
                                             (ensure-list-of-sudoku-grid-areas
                                              (alexandria:ensure-list areas))))
                 :column-labels column-labels
                 :row-labels row-labels))

(defmethod print-object ((diagram sudoku-diagram) stream)
  (if *print-readably*
      (call-next-method diagram stream)
      (print-unreadable-object (diagram stream :type t)
        (format stream "~S" (sudoku-diagram-name diagram)))))

(defun get-alt-text (diagram)
  (with-output-to-string (stream)
    (with-accessors ((name sudoku-diagram-name)
                     (givens sudoku-diagram-givens)
                     (highlighted sudoku-diagram-highlighted)
                     (marked sudoku-diagram-marked)
                     (rows sudoku-diagram-row-labels)
                     (columns sudoku-diagram-column-labels)) diagram

      (format stream "~A~%" name)

      (when rows
        (format stream " - Rows: ~A.~%" (format-list rows)))
      (when columns
        (format stream " - Columns: ~A.~%" (format-list columns)))

      (when givens
        (format stream " - Given: ~A.~%"
                (format-list (loop :for (value area) :on givens :by #'cddr
                                :collecting (format nil "~D in ~A" value area)))))

      (when highlighted
        (format stream " - Highlighted: ~A.~%"
                (format-list (loop :for area :in highlighted
                                :collecting (format nil "~A" area)))))

      (loop :for (mark areas) :on marked :by #'cddr
         :do (when areas
               (format stream " - Marked with a ~A: ~A.~%"
                       mark
                       (format-list areas)))))))

(defparameter *d* (make-sudoku-diagram :name "Toroidal Miracle Sudoku"
                                       :givens '(1 :r6c6)
                                       :highlighted '(:b1 :b2c4 :b2c5 :b3
                                                      :b4r4 :b4r5 :r4c8 :r4c9 :r5c9
                                                      :b7 :r8c4 :r9c4 :r9c5 :r7c9 :r8c8 :r8c9 :b9r9)
                                       :marked '("light-diamond" (:r4c8 :r4c9 :r5c9)
                                                 "dark-circle" (:r2c9 :r3c8 :r3c9
                                                                :r4c1 :r4c2 :r5c1))
                                       :row-labels '("r1" "r2" "r3" "r4" "r5" "r6" "r7" "r8" "r9")
                                       :column-labels '("c1" "c2" "c3" "c4" "c5" "c6" "c7" "c8" "c9")))
