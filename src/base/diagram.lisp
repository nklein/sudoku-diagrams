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
