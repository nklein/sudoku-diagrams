(in-package #:sudoku-diagrams)

(defun kw (fmt-string &rest args)
  (alexandria:make-keyword (apply #'format nil fmt-string args)))

(defun make-range (a b)
  (check-type a integer)
  (check-type b integer)
  (assert (< a b))
  (loop :for x :from a :to b :collecting x))

(defun consecutivep (ns)
  (labels ((rec (x ns)
             (or (null ns)
                 (and (= x (first ns))
                      (rec (1+ x) (rest ns))))))
    (rec (first ns) ns)))

(defmacro deftypep (predicate type)
  "Create a function named PREDICATE that checks if its argument is the given TYPE"
  (let ((x (gensym "X")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (declaim (inline ,predicate))
       (defun ,predicate (,x)
         (typep ,x ',type)))))

(defmacro deflisttype (type predicate element-predicate)
  "Create a function named PREDICATE where that ensures that its argument is a list where every element satisfies the given ELEMENT-PREDICATE. Also, create a type name TYPE for lists that satisfy this PREDICATE."
  (let ((xs (gensym "XS")))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defun ,predicate (,xs)
           (and (listp ,xs)
                (every #',element-predicate ,xs))))
       (deftype ,type ()
         `(and list
               (satisfies ,',predicate))))))


(deflisttype list-of-string list-of-string-p stringp)


(defun format-list (items)
  (format nil "~{~#[~;~A~;~A and ~A~:;~@{~A~#[~;, and ~:;, ~]~}~]~}" items))
