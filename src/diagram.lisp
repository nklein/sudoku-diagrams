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
                 :marked (loop :for (mark areas) :on marked
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

(let ((*print-pretty* t))
  (format t "~A~%" (make-sudoku-diagram :name "test diagram"
                                        :givens '(9 :r3c5 3 :r6c8)
                                        :highlighted '(:b4 :b2r3)
                                        :marked '("diamond" :r3c4))))
