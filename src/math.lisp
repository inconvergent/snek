
(in-package :math)


; TYPES

(defun int (x)
  (the integer
    (coerce (floor x) 'integer)))


(defun int* (xx)
  (mapcar (lambda (x) (int (floor x))) xx))


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
      (loop for ,i of-type integer from 1 to ,nname collect (progn ,@body)))))


(defun range (a &optional (b nil))
  ; TODO
  ; (declare (integer a))
  (if (not b)
    (loop for x of-type integer from 0 below a collect x)
    (loop for x of-type integer from a below b collect x)))


(defun lget (l ii)
  "
  avoid using this; it is slow if l is large.
  "
  (declare (list l ii))
  (loop for i in ii collect (nth i l)))


(defun inc (x stp)
  (mod (+ x stp) 1d0))


(defun mod- (i n)
  (declare (integer i n))
  (mod (+ n i -1) n))


(defun mod+ (i n)
  (declare (integer i n))
  (mod (+ n i 1) n))


(defun mod2 (i)
  (declare (integer i))
  (mod i 2))


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


(defun dst (a b)
  (declare (list a b))
  (sqrt (loop for ai in a and bi in b sum (expt (- ai bi) 2d0))))


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


; PATHS

(defun path-angles (pts)
  (let ((res (make-generic-array)))
    (loop for i from 0 below (1- (length pts))
          do (array-push (vec:norm (vec:sub (aref pts (1+ i))
                                            (aref pts i))) res))
    (array-push (aref res (1- (length res))) res)
    res))


(defun path-simplify (pts &optional (lim -0.989d0))
  ;very naive paty simplification
  (let* ((n (length pts))
         (i 1)
         (res (make-generic-array))
         (pts* (if (equal (type-of pts) 'cons) (to-array pts) pts))
         (angles (path-angles pts*)))

    (array-push (aref pts* 0) res)

    (loop while (< i (1- n)) do
      (if (< (vec:dot (aref angles (1- i))
                      (aref angles i)) lim)
        (progn
          (array-push (aref pts* (1+ i)) res)
          (incf i 2))
        (progn
          (array-push (aref pts* i) res)
          (incf i))))

    (if (< i n)
      (array-push (aref pts* i) res))

    (if (< (length res) n)
      (path-simplify res)
      res)))


(defun -perp (a b &optional (dir 0.5d0))
(let ((df (vec:add a b)))
  (if (<= (vec:len df) 1d-7)
    (vec:perp a)
    (vec:perp (vec:norm df)))))


(defun path-normals-open (angles)
  (let ((res (make-generic-array)))
    (array-push (vec:perp (aref angles 0)) res)
    (loop for i from 0 below (1- (length angles)) do
      (array-push (-perp (aref angles i)
                        (aref angles (1+ i)))
                  res))
    res))


(defun path-normals-closed (angles)
  (let ((ss (-perp (aref angles 0)
                   (aref angles (1- (length angles)))))
        (res (make-generic-array)))

    (array-push ss res)

    (loop for i from 0 below (1- (length angles)) do
      (array-push (-perp (aref angles i)
                         (aref angles (1+ i))) res))
    (setf (aref res (1- (length res))) ss)
    res))

