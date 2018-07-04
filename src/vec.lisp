
(in-package :vec)


(declaim (optimize (speed 3)))


(defconstant PII  (* PI 2d0))
(declaim (type double-float PII))


(defmacro with-xy ((v x y) &body body)
  (declare (symbol x y))
  (with-gensyms (vname)
   `(let* ((,vname ,v)
           (,x (vec-x ,vname))
           (,y (vec-y ,vname)))
     (declare (double-float ,x ,y))
     (progn ,@body))))


(defmacro with-xy-short ((v x y) &body body)
  (declare (symbol x y))
  (with-gensyms (vname)
     `(let* ((,vname ,v)
             (,x (math:sfloat (vec-x ,vname)))
             (,y (math:sfloat (vec-y ,vname))))
       (progn ,@body))))


(defmacro with-loop-grid ((grid xy) &body body)
  (declare (symbol xy))
  (with-gensyms (grid* x y)
    `(let ((,grid* ,grid))
      (loop for ,y of-type double-float in ,grid*
            do (loop for ,x of-type double-float in ,grid*
                     do (let ((,xy (vec ,x ,y)))
                          (progn ,@body)))))))


(defmacro with-loop-grid* ((grid xy) &body body)
  (declare (symbol xy))
  (with-gensyms (x y)
    `(loop for ,y of-type double-float in ,grid
           do (loop for ,x of-type double-float in ,grid
                    do (let ((,xy (vec ,x ,y)))
                         (progn ,@body))))))


(defmacro rep (&body body)
  `(vec (progn ,@body)
        (progn ,@body)))


(defstruct (vec (:constructor -make-vec))
  (x nil :type double-float :read-only t)
  (y nil :type double-float :read-only t))


(declaim (ftype (function () vec) zero one))
(declaim (ftype (function (double-float &optional double-float) vec) vec))
(declaim (ftype (function (double-float) vec) cos-sin sin-cos))

(declaim (ftype (function (vec double-float) vec) scale iscale))
(declaim (ftype (function (vec vec double-float) vec) add-scaled))

(declaim (ftype (function (vec vec) double-float) dot cross dst dst2))

(declaim (ftype (function (vec vec) vec) sub add mult div mid))

(declaim (ftype (function (vec) double-float) angle))

(declaim (ftype (function (vec) vec) perp flip copy neg))

(declaim (ftype (function (double-float vec vec) vec) on-line))


(defun vec (x &optional y)
  (declare (double-float x))
  (if y (-make-vec :x x :y y)
        (-make-vec :x x :y x)))

(defun zero ()
  (vec 0d0 0d0))

(defun one ()
  (vec 1d0 1d0))


(defparameter *one*  (vec:vec 1d0))
(defparameter *half*  (vec:vec 0.5d0))
(defparameter *zero*  (vec:vec 0d0))
(declaim (type vec *one* *half* *zero*))


(defun copy (v)
  (declare (vec v))
  (vec (vec-x v) (vec-y v)))


(defun tolist (v)
  (declare (vec v))
  (list (vec-x v) (vec-y v)))


(defun vec-coerce (x y)
  (vec (math:dfloat x) (math:dfloat y)))


(defun flip (v)
  (declare (vec v))
  (vec (vec-y v) (vec-x v)))


(defun perp (v)
  (declare (vec v))
  (vec (vec-y v) (- (vec-x v))))


; TODO: this is probably unexpected behaviour (returning list, not vec)
(defun vround (v)
  (declare (vec v))
  (list (round (vec-x v)) (round (vec-y v))))


(declaim (inline -vfloor*)
         (ftype (function (vec) (values fixnum fixnum)) -vround*))
(defun -vround* (v)
  (declare (optimize (safety 0) speed (debug 0))
           (vec v))
  (values (round (vec-x v)) (round (vec-y v))))

(declaim (inline -vfloor*)
         (ftype (function (vec) (values fixnum fixnum)) -vfloor*))
(defun -vfloor* (v)
  (declare (optimize (safety 0) speed (debug 0))
           (vec v))
  (values (floor (vec-x v)) (floor (vec-y v))))


;(defun -voutward-round (xy mid)
;  (declare (vec xy mid))
;  (with-xy (mid mx my)
;    (with-xy (xy x y)
;      (values (math:int (if (<= x mx) (floor x) (ceiling x)))
;              (math:int (if (<= y my) (floor y) (ceiling y)))))))


(defun vec* (xy)
  (declare (list xy))
  (destructuring-bind (x y) (math:dfloat* xy)
    (declare (double-float x y))
    (vec x y)))


(defun arr-get (a i)
  (declare (integer i))
  (declare (type (array double-float) a))
  (vec (aref a i 0) (aref a i 1)))


(defun arr-set (a i v)
  (declare (vec v))
  (declare (integer i))
  (declare (type (array double-float) a))
  (setf (aref a i 0) (the double-float (vec-x v))
        (aref a i 1) (the double-float (vec-y v))))


; MATHS



(defun cos-sin (a)
  (declare (double-float a))
  (vec (cos a) (sin a)))


(defun sin-cos (a)
  (declare (double-float a))
  (vec (sin a) (cos a)))


(defun angle (v)
  (declare (vec v))
  (with-xy ((norm v) x y)
    (atan y x)))


(defun add-scaled (a b s)
  (declare (double-float s))
  (declare (vec a b))
  (vec (+ (vec-x a) (* s (vec-x b)))
       (+ (vec-y a) (* s (vec-y b)))))


(declaim (inline scale))
(defun scale (a s)
  (declare (optimize (safety 0) speed (debug 0))
           (vec a)
           (double-float s))
  (vec (* (vec-x a) s)
       (* (vec-y a) s)))

(defun neg (a)
  (declare (vec a))
  (scale a -1d0))


(defun iscale (a s)
  (declare (vec a))
  (declare (double-float s))
  (vec (/ (vec-x a) s)
       (/ (vec-y a) s)))


(declaim (inline sub))
(defun sub (a b)
  (declare (optimize (safety 0) speed (debug 0))
           (vec a b))
  (vec (- (vec-x a) (vec-x b))
       (- (vec-y a) (vec-y b))))


(defun lsub (aa bb)
  (declare (list aa bb))
  (mapcar (lambda (a b) (declare (type vec a b)) (sub a b)) aa bb))


(defun lsub* (aa b)
  (declare (list aa))
  (declare (vec b))
  (mapcar (lambda (a) (declare (type vec a)) (sub a b)) aa))


(defun isub (a b)
  (declare (vec a b))
  (vec (- (vec-x b) (vec-x a))
       (- (vec-y b) (vec-y a))))


(defun op (fx a b)
  (declare (vec a b))
  (vec (funcall fx (vec-x a) (vec-x b))
       (funcall fx (vec-y a) (vec-y b))))


(defun vabs (a)
  (declare (vec a))
  (vec (abs (vec-x a)) (abs (vec-y a))))

(declaim (inline add))
(defun add (a b)
  (declare (optimize (safety 0) speed (debug 0))
           (vec a b))
  (vec (+ (vec-x a) (vec-x b))
       (+ (vec-y a) (vec-y b))))


(defun ladd (aa bb)
  (declare (list aa bb))
  (mapcar (lambda (a b) (declare (type vec a b)) (add a b)) aa bb))


(defun ladd* (aa b)
  (declare (list aa))
  (declare (vec b))
  (mapcar (lambda (a) (declare (type vec a)) (add a b)) aa))


(defun lscale* (aa s)
  (declare (list aa))
  (declare (double-float s))
  (mapcar (lambda (a) (declare (type vec a)) (scale a s)) aa))


(defun mult (a b)
  (declare (vec a b))
  (vec (* (vec-x a) (vec-x b))
       (* (vec-y a) (vec-y b))))


(defun lmult (aa bb)
  (declare (list aa bb))
  (mapcar (lambda (a b) (declare (type vec a b)) (mult a b)) aa bb))


(defun lmult* (aa b)
  (declare (list aa))
  (declare (vec b))
  (mapcar (lambda (a) (declare (type vec a)) (mult a b)) aa))


(defun dot (a b)
  (declare (vec a b))
  (+ (* (vec-x a) (vec-x b))
     (* (vec-y a) (vec-y b))))


(defun ldot (aa bb)
  (declare (list aa bb))
  (mapcar (lambda (a b) (declare (type vec a b)) (dot a b)) aa bb))


(defun div (a b)
  (declare (vec a b))
  (vec (/ (vec-x a) (vec-x b))
       (/ (vec-y a) (vec-y b))))


(defun ldiv (aa bb)
  (declare (list aa bb))
  (mapcar (lambda (a b) (declare (type vec a b)) (div a b)) aa bb))


(defun ldiv* (aa b)
  (declare (list aa))
  (declare (vec b))
  (mapcar (lambda (a) (declare (type vec a)) (div a b)) aa))


(defun idiv (a b)
  (declare (vec a b))
  (vec (/ (vec-x b) (vec-x a))
       (/ (vec-y b) (vec-y a))))


(defun len2 (a)
  (declare (vec a))
  (+ (expt (vec-x a) 2) (expt (vec-y a) 2)))

(defun len (a)
  (declare (vec a))
  (sqrt (len2 a)))


(defun mid (a b)
  (declare (vec a b))
  (iscale (add a b) 2d0))


(defun lmid (aa)
  (declare (list aa))
  (let ((n 1))
    (iscale
      (reduce (lambda (a b) (declare (type vec a b)) (incf n) (add a b)) aa)
      (math:dfloat n))))


(defun dst2 (a b)
  (declare (vec a b))
  (len2 (sub a b)))


(defun dst (a b)
  (declare (vec a b))
  (len (sub a b)))

(defun dst* (aa)
  (declare (list aa))
  (dst (first aa) (second aa)))


(defun ldst (a b)
  (declare (list a b))
  (mapcar #'dst a b))


(defun ldst* (aa b)
  (declare (list aa))
  (declare (vec b))
  (loop for a in aa collect (dst a b)))


(defun norm (a &key (s 1d0) (default *zero*))
  (declare (vec a))
  (declare (double-float s))
  (let ((l (len a)))
    (if (> l 0d0) (scale a (/ s l)) default)))


(defun nsub (a b &key (s 1d0) (default *zero*))
  (declare (vec a b))
  (norm (sub a b) :s s :default default))


(defun sum (aa)
  (declare (list aa))
  (reduce (lambda (a b) (declare (vec a b)) (add a b)) aa))


(defun rot (v a &key (xy *zero*))
  (declare (vec v))
  (declare (double-float a))
  (let ((cosa (cos a))
        (sina (sin a)))
    (with-xy ((sub v xy) x y)
      (add xy (vec (- (* x cosa) (* y sina))
                   (+ (* x sina) (* y cosa)))))))


(defun lrot (pts a &key (xy *zero*))
  (declare (list pts))
  (declare (double-float a))
  (mapcar (lambda (p) (declare (vec p)) (rot p a :xy xy)) pts))


(defun shift-scale (pt shift s &optional (unshift *zero*))
  "shift scale (unshift)"
  (declare (vec pt shift))
  (declare (double-float s))
  (add (scale (sub pt shift) s) unshift))


(defun shift-scale* (pts shift s &optional unshift)
  (declare (list pts))
  (declare (vec shift))
  (declare (double-float s))
  (mapcar (lambda (pt) (declare (vec pt))
            (shift-scale pt shift s unshift)) pts))


(defun all-inside (path &optional (lim 1d6))
  (if (every #'identity
             (loop for v in path
                   collect (and (>= (vec-x v) (- lim)) (<= (vec-x v) lim)
                                (>= (vec-y v) (- lim)) (<= (vec-y v) lim))))
      path
      nil))


(defun segdst (aa v)
  (declare (list aa))
  (declare (vec v))
  (destructuring-bind (va vb)
    aa
    (let ((l2 (dst2 va vb)))
      (if (<= l2 0d0)
        ; line is a point
        (values (dst va v) 0d0)
        ; else
        (let ((tt (/ (+ (* (- (vec-x v) (vec-x va)) (- (vec-x vb) (vec-x va)))
                        (* (- (vec-y v) (vec-y va)) (- (vec-y vb) (vec-y va))))
                     l2)))
          (if (> tt 1d0) (setf tt 1d0))
          (if (< tt 0d0) (setf tt 0d0))
          (values (dst v (on-line tt va vb)) tt))))))


(defun segx (aa bb &key parallel)
  (declare (list aa))
  (declare (list bb))
  (destructuring-bind (a1 a2 b1 b2)
    (concatenate 'list aa bb)
    (let* ((sa (sub a2 a1))
           (sb (sub b2 b1))
           (u (+ (* (- (vec-x sb)) (vec-y sa)) (* (vec-x sa) (vec-y sb)))))
      (if (= u 0d0)
        ; return parallel if the lines are parallel (default: nil)
        (values parallel nil nil)
        ; otherwise check if they intersect
        (let ((p (/ (+ (* (- (vec-y sa)) (- (vec-x a1) (vec-x b1)))
                       (*    (vec-x sa)  (- (vec-y a1) (vec-y b1)))) u))

              (q (/ (- (*    (vec-x sb)  (- (vec-y a1) (vec-y b1)))
                       (*    (vec-y sb)  (- (vec-x a1) (vec-x b1)))) u)))
          ; t if intersection
          ; nil otherwise
          (values (and (>= p 0d0) (<= p 1d0) (>= q 0d0) (<= q 1d0))
                  q p))))))


(defun segx* (l &key parallel)
  (declare (list l))
  (destructuring-bind (a b) l
    (segx a b :parallel parallel)))


; TODO: incomplete
(defun lsegx* (lines line &key parallel)
  (declare (list lines line))
  (loop with res = (make-generic-array)
        for l of-type list in lines
        ; TODO: sort and rearrange
        do (multiple-value-bind (x p q) (segx l line)
             (when x (array-push (list x p q) res)))
        finally (return res)))


(defun cross (a b)
  (declare (vec a))
  (declare (vec b))
  (- (* (vec-x a) (vec-y b)) (* (vec-y a) (vec-x b))))


(defun ptinside (convex v)
  (declare (list convex))
  (declare (vec v))
  (loop for a in (close-path convex)
        and b in (cdr (close-path convex))
        always (>= (cross (sub b a) (sub v b)) 0d0)))


; SHAPES

(defun on-circ (p rad &key (xy *zero*))
  (declare (double-float p rad))
  (declare (vec xy))
  (add-scaled xy (cos-sin (* p PII)) rad))


(defun on-line (p a b)
  (declare (double-float p))
  (declare (vec a b))
  (add-scaled a (sub b a) p))


(defun on-line* (p ab)
  (declare (double-float p))
  (declare (list ab))
  (destructuring-bind (a b) ab
    (on-line p a b)))


(defun on-spiral (p rad &key (xy *zero*) (rot 0d0))
  (declare (double-float p rad rot))
  (declare (vec xy))
  (add xy (scale (cos-sin (+ rot (* p PII)))
                 (* p rad))))


(defun rect (w h &key (xy *zero*))
  (declare (double-float w h))
  (declare (vec xy))
  (list (add xy (vec w (- h)))
        (add xy (vec w h))
        (add xy (vec (- w) h))
        (sub xy (vec w h))))


(defun square (bs &key xy)
  (declare (double-float bs))
  (declare (vec xy))
  (rect bs bs :xy xy))


(defun polygon (n rad &key (xy *zero*) (rot 0d0))
  (declare (integer n))
  (declare (double-float rad rot))
  (declare (vec xy))
  (loop for i from 0 below n
        collect (add (scale (cos-sin (+ rot (* (/ i n) PII))) rad)
                     xy)))

