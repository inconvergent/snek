
(defpackage :vec
  (:use :common-lisp)
  (:export
    :make)
  (:import-from :common-lisp-user
    ;:dst2
    ;:to-dfloat*
    ;:get-atup
    ;:to-dfloat*
    ;:with-struct
    )
  )


(in-package :vec)


(defun -to-dfloat* (xx)
  (mapcar (lambda (x) (coerce x 'double-float)) xx))


(defun make (a)
  (make-array 2
              :initial-contents (-to-dfloat* a)
              :element-type 'double-float))



(defun sub (a b)
  (mapcar #'- a b))


(defun isub (a b)
  (mapcar #'- b a))


(defun add (a b)
  (map '(vector double-float 2) #'+ a b))

(defun -add (a b)
  (mapcar #'+ a b))


(defun sum (a)
  (reduce #'+ a))


(let ((a (make (list 2 3)))
      (b (make (list 4 3)))
      (c (-to-dfloat* (list 2 3)))
      (d (-to-dfloat* (list 4 6))))

  (print a)
  (print b)
  (print (add a b))

  (time (loop for i from 0 below 10000000 do
        (add c d)))

  (time (loop for i from 0 below 10000000 do
        (-add c d)))

  )







