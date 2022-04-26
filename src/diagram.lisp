(in-package #:sudoku-diagrams)

(defclass sudoku-diagram ()
  ((name :initarg :name :reader sudoku-diagram-name)
   (givens :initarg :givens :reader sudoku-diagram-givens)
   (highlighted :initarg :highlighted :reader sudoku-diagram-highlighted)
   (marked :initarg :marked :reader sudoku-diagram-marked)))

(defun make-sudoku-diagram (&key
                              (name (error "Must supply diagram :NAME"))
                              givens
                              highlighted
                              marked
                            &allow-other-keys)
  (check-type name string)
  (check-type givens list)
  (check-type highlighted list)
  (check-type marked list)

  (make-instance 'sudoku-diagram
                 :name name
                 :givens (loop :for (given handle-or-area) :on givens :by #'cddr
                            :appending (list given
                                             (ensure-sudoku-grid-area handle-or-area)))
                 :highlighted (ensure-list-of-sudoku-grid-areas highlighted)
                 :marked (loop :for (mark areas) :on marked :by #'cddr
                            :appending (list mark
                                             (ensure-list-of-sudoku-grid-areas
                                              (alexandria:ensure-list areas))))))

(defmethod print-object ((diagram sudoku-diagram) stream)
  (if *print-readably*
      (call-next-method diagram stream)
      (let ((name (sudoku-diagram-name diagram))
            (givens (sudoku-diagram-givens diagram))
            (highlighted (sudoku-diagram-highlighted diagram))
            (marked (sudoku-diagram-marked diagram)))
        (format stream "~@(~A~)~%" name)
        (when givens
          (format stream " - These cells are given:~%")
          (loop :for (value area) :on givens :by #'cddr
             :do (format stream "   - ~D in ~A.~%" value area)))
        (when highlighted
          (format stream " - These cells are highlighted:~%")
          (loop :for area :in highlighted
             :do (format stream "   - ~@(~A~).~%" area)))
        (loop :for (mark areas) :on marked :by #'cddr
           :do (when areas
                 (format stream " - These cells are marked with a ~A:~%" mark)
                 (loop :for area :in areas
                    :do (format stream "   - ~@(~A~).~%" area)))))))

(defparameter *d* (make-sudoku-diagram :name "Toroidal Miracle Sudoku"
                                       :givens '(1 :r6c6)
                                       :highlighted '(:b1 :b2c4 :b2c5 :b3
                                                      :b4r4 :b4r5 :r4c8 :r4c9 :r5c9
                                                      :b7 :r8c4 :r9c4 :r9c5 :r7c9 :r8c8 :r8c9 :b9r9)
                                       :marked '("light-diamond" (:r4c8 :r4c9 :r5c9)
                                                 "dark-circle" (:r2c9 :r3c8 :r3c9
                                                                :r4c1 :r4c2 :r5c1))))
