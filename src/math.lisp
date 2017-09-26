
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


(defun dst (a b)
  (declare (list a b))
  (mapcar #'vec:dst a b))


(defun vdst (aa b)
  (declare (list aa))
  (declare (vec:vec b))
  (loop for a in aa collect (vec:dst a b)))


(defun add (a b)
  (declare (list a b))
  (mapcar #'+ a b))


(defun vadd (aa b)
  (declare (list aa))
  (declare (vec:vec b))
  (mapcar (lambda (a) (vec:add a b))
          aa))


(defun sub (a b)
  (declare (list a b))
  (mapcar #'- a b))


(defun vsub (aa b)
  (declare (list aa))
  (declare (vec:vec b))
  (mapcar (lambda (a) (vec:sub a b))
          aa))


(defun mult (a b)
  (declare (list a b))
  (mapcar #'* a b))


(defun vmult (aa b)
  (declare (list aa))
  (declare (vec:vec b))
  (mapcar (lambda (a) (vec:mult a b))
          aa))


(defun div (a b)
  (declare (list a b))
  (mapcar #'/ a b))


(defun vdiv (aa b)
  (declare (list aa))
  (declare (vec:vec b))
  (mapcar (lambda (a) (vec:div a b))
          aa))


; TODO: this is inconsistent
(defun scale (a s)
  (declare (list a))
  (declare (double-float s))
  (mapcar (lambda (i) (* i s)) a))


; TODO: this is inconsistent
(defun iscale (a s)
  (declare (list a))
  (declare (double-float s))
  (mapcar (lambda (i) (/ i s)) a))


(defun sum (a)
  (declare (list a))
  (reduce #'+ a))


; SHAPES
; TODO: new package


(defun on-circ (p rad &key (xy (vec:zero)))
  (declare (double-float p rad))
  (declare (vec:vec xy))
  (vec:add xy (vec:scale (vec:cos-sin (* p PII)) rad)))


(defun on-line (p a b)
  (declare (double-float p))
  (declare (vec:vec a b))
  (vec:add a (vec:scale (vec:sub b a) p)))


(defun on-spiral (p rad &key (xy (vec:zero)) (rot 0.0d0))
  (declare (double-float p rad rot))
  (declare (vec:vec xy))
  (vec:add xy (vec:scale (vec:cos-sin (+ rot (* p PII)))
                         (* p rad))))


(defun polygon (n rad &key (xy (vec:zero)) (rot 0.0d0))
  (declare (integer n))
  (declare (double-float rad rot))
  (declare (vec:vec xy))
  (loop for i from 0 below n collect (vec:add xy
    (vec:scale
      (vec:cos-sin (+ rot (* (/ i n) PII)))
      rad))))

