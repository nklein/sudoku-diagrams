(in-package #:sudoku-diagrams)

(defvar *known-areas* (make-hash-table :test 'eql))

(defun get-known-area (handle)
  (check-type handle keyword)
  (multiple-value-bind (area found) (gethash handle *known-areas*)
    (unless found
      (error "Could not find area ~A" handle))
    area))

(defun ensure-sudoku-grid-area (handle-or-area)
  (if (typep handle-or-area 'sudoku-grid-area)
      handle-or-area
      (get-known-area handle-or-area)))

(defun ensure-list-of-sudoku-grid-areas (list-of-handle-or-area)
  (mapcar #'ensure-sudoku-grid-area list-of-handle-or-area))

(defun add-known-area (area)
  (check-type area sudoku-grid-area)
  (setf (gethash (sudoku-grid-area-handle area) *known-areas*) area))

(defun add-known-areas (areas)
  (map 'null #'add-known-area areas))

(defun add-standard-grid-areas ()
  (let* ((rs (make-range 1 +sudoku-rows+))
         (cs (make-range 1 +sudoku-columns+))
         (bs (make-range 1 +sudoku-boxes+))
         (rows (mapcar #'make-row-area rs))
         (columns (mapcar #'make-column-area cs))
         (boxes (mapcar #'make-box-area bs)))

    (add-known-areas rows)
    (add-known-areas columns)
    (add-known-areas boxes)
    (add-known-areas (alexandria:map-product #'make-cell-area rs cs))

    (flet ((numbered-area (n a)
             (list n a)))

      (alexandria:map-product (lambda (blist rlist)
                                (destructuring-bind (b bs) blist
                                  (destructuring-bind (r rs) rlist
                                    (let ((indexes (intersection bs rs :test #'=)))
                                      (when indexes
                                        (add-known-area
                                         (make-sudoku-grid-area (kw "B~DR~D" b r)
                                                                indexes
                                                                (format nil "row ~D in box ~D" r b))))))))
                              (mapcar #'numbered-area
                                      bs
                                      (mapcar #'sudoku-grid-area-indexes boxes))
                              (mapcar #'numbered-area
                                      rs
                                      (mapcar #'sudoku-grid-area-indexes rows)))

      (alexandria:map-product (lambda (blist clist)
                                (destructuring-bind (b bs) blist
                                  (destructuring-bind (c cs) clist
                                    (let ((indexes (intersection bs cs :test #'=)))
                                      (when indexes
                                        (add-known-area
                                         (make-sudoku-grid-area (kw "B~DC~D" b c)
                                                                indexes
                                                                (format nil "column ~D in box ~D" c b))))))))
                              (mapcar #'numbered-area
                                      bs
                                      (mapcar #'sudoku-grid-area-indexes boxes))
                              (mapcar #'numbered-area
                                      cs
                                      (mapcar #'sudoku-grid-area-indexes columns))))))

(add-standard-grid-areas)
