
(in-package :math)


; TYPES

(defun int (x)
  (the integer
       (coerce x 'integer)))


(defun int* (xx)
  (mapcar (lambda (x) (int x)) xx))


(defun sfloat (x)
  (the float
       (coerce x 'float)))


(defun sfloat* (xx)
  (mapcar (lambda (x) (sfloat x)) xx))


(defun dfloat (x)
  (the double-float
       (coerce x 'double-float)))


(defun dfloat* (xx)
  (mapcar (lambda (x) (dfloat x)) xx))


; RANGES


(defmacro rep ((i itt) &body body)
  `(loop for ,i in ,itt collect (progn ,@body)))


(defmacro nrep (n &body body)
  (with-gensyms (i nname)
    `(let ((,nname ,n))
      (loop for ,i integer from 1 to ,nname collect (progn ,@body)))))


(defun range (a &optional (b nil))
  ; TODO
  ; (declare (integer a))
  (if (not b)
    (loop for x integer from 0 below a collect x)
    (loop for x integer from a below b collect x)))


(defun lget (l ii)
  "
  avoid using this; it is slow if l is large.
  "
  (declare (list l ii))
  (loop for i in ii collect (nth i l)))


(defun inc (x stp)
  (mod (+ x stp) 1d0))


(defmacro with-linspace ((n a b rn &key (end t)) &body body)
  (with-gensyms (a* b* n* nn i ba)
  `(let* ((,n* (int ,n))
          (,nn (dfloat (if ,end (1- ,n*) ,n*)))
          (,a* (dfloat ,a))
          (,b* (dfloat ,b))
          (,ba (- ,b* ,a*)))
    (loop for ,i from 0 below ,n* do
      (let ((,rn (dfloat (+ ,a* (* ,i (/ ,ba ,nn))))))
        (progn ,@body))))))



(defun linspace (n a b &key (end t))
  ; TODO
  ; (declare (double-float a b))
  ; (declare (integer n))
  ; (declare (boolean end))
  (if (> n 1)
    (let ((nn (if end (1- n) n)))
      (loop for i from 0 below n
        collect (dfloat (+ a (* i (/ (- b a) nn))))))
    (list (dfloat a))))


; LIST MATH


(defun add (a b)
  (declare (list a b))
  (mapcar #'+ a b))


(defun sub (a b)
  (declare (list a b))
  (mapcar #'- a b))


(defun mult (a b)
  (declare (list a b))
  (mapcar #'* a b))


(defun div (a b)
  (declare (list a b))
  (mapcar #'/ a b))


(defun scale (aa bb)
  (declare (list aa bb))
  (mapcar (lambda (a b) (* a b)) aa bb))


(defun scale* (aa s)
  (declare (list aa))
  (declare (double-float s))
  (mapcar (lambda (a) (* a s)) aa))


(defun iscale (aa bb)
  (declare (list aa bb))
  (mapcar (lambda (a b) (/ a b)) aa bb))


(defun iscale* (aa s)
  (declare (list aa))
  (declare (double-float s))
  (mapcar (lambda (a) (/ a s)) aa))


(defun sum (a)
  (declare (list a))
  (reduce #'+ a))

; TODO: expt, sqrt, ...

