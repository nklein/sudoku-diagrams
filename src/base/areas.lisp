(in-package #:sudoku-diagrams)

(defclass sudoku-grid-area ()
  ((handle :initarg :handle :reader sudoku-grid-area-handle)
   (indexes :initarg :indexes :reader sudoku-grid-area-indexes)
   (description :initarg :description :reader sudoku-grid-area-description)))

(deftypep sudoku-grid-area-p sudoku-grid-area)
(deflisttype list-of-sudoku-grid-area-p list-of-sudoku-grid-area sudoku-grid-area-p)

(defmethod print-object ((area sudoku-grid-area) stream)
  (if *print-readably*
      (call-next-method area stream)
      (print-object (sudoku-grid-area-description area) stream)))

(defun make-sudoku-grid-area (handle indexes description)
  (check-type handle keyword)
  (check-type indexes list-of-sudoku-index)
  (check-type description string)
  (make-instance 'sudoku-grid-area
                 :handle handle
                 :indexes indexes
                 :description description))

(defun make-row-area (r)
  (check-type r sudoku-row-number)
  (make-sudoku-grid-area (kw "R~D" r)
                         (mapcar (lambda (c)
                                   (row-col-index r c))
                                 (make-range 1 +sudoku-columns+))
                         (format nil "all of row ~D" r)))

(defun make-column-area (c)
  (make-sudoku-grid-area (kw "C~D" c)
                         (mapcar (lambda (r)
                                   (row-col-index r c))
                                 (make-range 1 +sudoku-rows+))
                         (format nil "all of column ~D" c)))

(defun describe-range (common-area range-name ns)
  (let ((ns (remove-duplicates ns :test #'= :from-end t)))
    (cond
      ((= (length ns) +sudoku-rows+)
       (format nil "all of ~A" common-area))

      ((and (< 2 (length ns))
            (consecutivep ns))
       (format nil "~A~P ~D through ~D in ~A"
               range-name
               (length ns)
               (1+ (first ns))
               (1+ (first (last ns)))
               common-area))

      (t
       (format nil "~A~P ~A in ~A"
               range-name
               (length ns)
               (format-list (mapcar #'1+ ns))
               common-area)))))

(defun make-partial-row-area (r a b)
  (check-type r sudoku-row-number)
  (check-type a sudoku-column-number)
  (check-type b sudoku-column-number)
  (assert (< a b))
  (make-sudoku-grid-area (kw "R~DC~A-~A" r a b)
                         (mapcar (lambda (c)
                                   (row-col-index r c))
                                 (make-range a b))
                         (format nil "row ~D columns ~D through ~D" r a b)))

(defun make-partial-column-area (c a b)
  (check-type c sudoku-column-number)
  (check-type a sudoku-row-number)
  (check-type b sudoku-row-number)
  (assert (< a b))
  (make-sudoku-grid-area (kw "R~D-~DC~A" a b c)
                         (mapcar (lambda (r)
                                   (row-col-index r c))
                                 (make-range a b))
                         (format nil "rows ~D through ~D of column ~D" a b c)))

(defun make-box-area (b)
  (let ((rs (make-range 1 +sudoku-rows+))
        (cs (make-range 1 +sudoku-columns+)))
    (make-sudoku-grid-area (kw "B~D" b)
                           (multiple-value-bind (r c) (floor (1- b) 3)
                             (let ((br (subseq rs (* r 3) (* (1+ r) 3)))
                                   (bc (subseq cs (* c 3) (* (1+ c) 3))))
                               (alexandria:map-product #'row-col-index br bc)))
                           (format nil "all of box ~D" b))))

(defun make-cell-area (r c)
  (make-sudoku-grid-area (kw "R~DC~D" r c)
                         (list (row-col-index r c))
                         (format nil "row ~D column ~D" r c)))
