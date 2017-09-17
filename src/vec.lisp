
(in-package :vec)


(defmacro cos-sin (a)
  (with-gensyms (aname)
    `(let ((,aname ,a))
        (vec (cos ,aname) (sin ,aname)))))


(defmacro sin-cos (a)
  (with-gensyms (aname)
    `(let ((,aname ,a))
        (vec (sin ,aname) (cos ,aname)))))


(defmacro inside* ((size xy x y) &body body)
  (with-gensyms (xname sname)
    `(let ((,sname ,size))
      (destructuring-bind (,x ,y)
        (vround ,xy)
        (if (and (>= ,x 0) (< ,x ,sname)
                 (>= ,y 0) (< ,y ,sname))
          (progn ,@body))))))


(defmacro inside ((size xy x y) &body body)
  (with-gensyms (sname xyname)
    `(let* ((,sname (math:dfloat ,size))
            (,xyname ,xy)
            (,x (vec::vec-x ,xyname))
            (,y (vec::vec-y ,xyname)))
      (if (and (>= ,x 0d0) (< ,x ,sname)
               (>= ,y 0d0) (< ,y ,sname))
        (progn ,@body)))))


(defmacro with-xy ((v x y) &body body)
  `(let ((,x (vec-x ,v))
         (,y (vec-y ,v)))
    (progn ,@body)))


(defmacro rep (&body body)
  `(vec
     (progn ,@body)
     (progn ,@body)))


(defstruct (vec
    (:constructor vec (x y)))
  (x nil :type double-float :read-only t)
  (y nil :type double-float :read-only t))


(defun zero ()
  (vec 0d0 0d0))


(defun one ()
  (vec 1d0 1d0))


(defun v (v)
  (declare (double-float v))
  (vec v v))


(defun copy (v)
  (declare (vec v))
  (vec (vec-x v) (vec-y v)))


(defun vec-coerce (x y)
  (vec (math:dfloat x) (math:dfloat y)))


(defun flip (v)
  (declare (vec v))
  (vec (vec-y v) (vec-x v)))


(defun perp (v)
  (declare (vec v))
  (vec (vec-y v) (- (vec-x v))))


(defun vround (v)
  (declare (vec v))
  (list
    (round (vec-x v))
    (round (vec-y v))))


(defun vec* (xy)
  (declare (list xy))
  (destructuring-bind (x y)
    (coerce 'double-float xy)
    (declare (double-float x y))
    (vec x y)))


(defun arr-get (a i)
  (declare (integer i))
  (declare (type (array double-float) a))
  (vec (aref a i 0) (aref a i 1)))


(defun arr-set (a i v)
  (declare (vec:vec v))
  (declare (integer i))
  (declare (type (array double-float) a))
  (setf (aref a i 0) (vec::vec-x v)
        (aref a i 1) (vec::vec-y v)))


(defun scale (a s)
  (declare (vec a))
  (declare (double-float s))
  (vec (* (vec-x a) s)
       (* (vec-y a) s)))


(defun iscale (a s)
  (declare (vec a))
  (declare (double-float s))
  (vec (/ (vec-x a) s)
       (/ (vec-y a) s)))


(defun sub (a b)
  (declare (vec a b))
  (vec (- (vec-x a) (vec-x b))
      (- (vec-y a) (vec-y b))))


(defun isub (a b)
  (declare (vec a b))
  (vec (- (vec-x b) (vec-x a))
      (- (vec-y b) (vec-y a))))


(defun add (a b)
  (declare (vec a b))
  (vec (+ (vec-x a) (vec-x b))
      (+ (vec-y a) (vec-y b))))


(defun mult (a b)
  (declare (vec a b))
  (vec (* (vec-x a) (vec-x b))
      (* (vec-y a) (vec-y b))))


(defun dot (a b)
  (declare (vec a b))
  (+ (* (vec-x a) (vec-x b))
     (* (vec-y a) (vec-y b))))


(defun div (a b)
  (declare (vec a b))
  (vec (/ (vec-x a) (vec-x b))
      (/ (vec-y a) (vec-y b))))


(defun idiv (a b)
  (declare (vec a b))
  (vec (/ (vec-x b) (vec-x a))
      (/ (vec-y b) (vec-y a))))


(defun len2 (a)
  (declare (vec a))
  (+ (expt (vec-x a) 2.d0)
     (expt (vec-y a) 2.d0)))

(defun len (a)
  (declare (vec a))
  (sqrt (len2 a)))


(defun mid (a b)
  (declare (vec a b))
  (iscale (add a b) 2.0d0))


(defun dst2 (a b)
  (declare (vec a b))
  (len2 (sub a b)))


(defun dst (a b)
  (declare (vec a b))
  (len (sub a b)))


(defun norm (a)
  (declare (vec a))
  (let ((l (len a)))
    (if (> l 0.0d0) (iscale a l) a)))


(defun nsub (a b)
  (declare (vec a b))
  (norm (sub a b)))


(defun rot (v a)
  (with-xy (v x y)
    (vec (- (* x (cos a)) (* y (sin a)))
         (+ (* x (sin a)) (* y (cos a))))))

