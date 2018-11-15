
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
  "
  loop over grid as xy in a 2d grid.
  "
  (declare (symbol xy))
  (with-gensyms (grid* x y)
    `(let ((,grid* ,grid))
      (loop for ,y of-type double-float in ,grid*
            do (loop for ,x of-type double-float in ,grid*
                     do (let ((,xy (vec ,x ,y)))
                          (progn ,@body)))))))


(defmacro with-loop-grid* ((grid xy) &body body)
  "
  loop over grid as xy in a 2d grid.
  grid form is executed independently for each dimension.
  "
  (declare (symbol xy))
  (with-gensyms (x y)
    `(loop for ,y of-type double-float in ,grid
           do (loop for ,x of-type double-float in ,grid
                    do (let ((,xy (vec ,x ,y)))
                         (progn ,@body))))))


(defmacro rep (&body body)
  `(vec (progn ,@body) (progn ,@body)))


(defstruct (vec (:constructor -make-vec))
  (x nil :type double-float :read-only t)
  (y nil :type double-float :read-only t))


(declaim (ftype (function () vec) zero one))
(declaim (ftype (function (double-float &optional double-float) vec) vec))
(declaim (ftype (function (double-float) vec) cos-sin sin-cos))

(declaim (ftype (function (vec double-float) vec) scale iscale))
(declaim (ftype (function (vec vec double-float) vec) from))

(declaim (ftype (function (vec vec) double-float) dot cross dst dst2))

(declaim (ftype (function (vec vec) vec) sub add mult div mid))

(declaim (ftype (function (vec) double-float) angle))

(declaim (ftype (function (vec) vec) perp flip copy neg))

(declaim (ftype (function (double-float vec vec) vec) on-line))


(defun vec (x &optional y)
  (declare (optimize (safety 0) speed (debug 0))
           (double-float x))
  (if y (-make-vec :x x :y y)
        (-make-vec :x x :y x)))

(defun zero () (vec 0d0 0d0))

(defun one () (vec 1d0 1d0))


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
  (declare (optimize (safety 0) speed (debug 0)) (vec v))
  (values (round (vec-x v)) (round (vec-y v))))

(declaim (inline -vfloor*)
         (ftype (function (vec) (values fixnum fixnum)) -vfloor*))
(defun -vfloor* (v)
  (declare (optimize (safety 0) speed (debug 0)) (vec v))
  (values (floor (vec-x v)) (floor (vec-y v))))


;(defun -voutward-round (xy mid)
;  (declare (vec xy mid))
;  (with-xy (mid mx my)
;    (with-xy (xy x y)
;      (values (math:int (if (<= x mx) (floor x) (ceiling x)))
;              (math:int (if (<= y my) (floor y) (ceiling y)))))))


(defun vec* (xy)
  "
  create (coerce) vec from list
  "
  (declare (optimize (safety 0) speed (debug 0))
           (list xy))
  (destructuring-bind (x y) (math:dfloat* xy)
    (declare (double-float x y))
    (vec x y)))


(defun sarr-get (a i &aux (ii (* 2 i)))
  "
  returns simple array (as 2d array) ind i.
  "
  (declare (optimize (safety 0) speed (debug 0))
           (fixnum i ii) (type (simple-array double-float) a))
  (vec (aref a ii) (aref a (1+ ii))))


(defun sarr-set (a i v &aux (ii (* 2 i)))
  "
  set simple array (as 2d array) in i to vec v.
  returns v.
  "
  (declare (optimize (safety 0) speed (debug 0))
           (vec v) (fixnum i ii) (type (simple-array double-float) a))
  (setf (aref a ii) (the double-float (vec-x v))
        (aref a (1+ ii)) (the double-float (vec-y v)))
  v)



; MATHS


(defun cos-sin (a)
  (declare (optimize (safety 0) speed (debug 0)) (double-float a))
  (vec (cos a) (sin a)))


(defun sin-cos (a)
  (declare (optimize (safety 0) speed (debug 0)) (double-float a))
  (vec (sin a) (cos a)))


(defun angle (v)
  (declare (optimize (safety 0) speed (debug 0)) (vec v))
  (with-xy ((norm v) x y)
    (atan y x)))


(defun from (a b s)
  (declare (optimize (safety 0) speed (debug 0)) (double-float s) (vec a b))
  (vec (+ (vec-x a) (* s (vec-x b)))
       (+ (vec-y a) (* s (vec-y b)))))


(declaim (inline scale))
(defun scale (a s)
  (declare (optimize (safety 0) speed (debug 0)) (vec a) (double-float s))
  (vec (* (vec-x a) s)
       (* (vec-y a) s)))

(defun neg (a)
  (declare (optimize (safety 0) speed (debug 0)) (vec a))
  (scale a -1d0))


(defun iscale (a s)
  (declare (optimize (safety 0) speed (debug 0)) (vec a) (double-float s))
  (vec (/ (vec-x a) s) (/ (vec-y a) s)))


(declaim (inline sub))
(defun sub (a b)
  (declare (optimize (safety 0) speed (debug 0)) (vec a b))
  (vec (- (vec-x a) (vec-x b)) (- (vec-y a) (vec-y b))))


(defun lsub (aa bb)
  (declare (optimize (safety 0) speed (debug 0)) (list aa bb))
  (mapcar (lambda (a b) (declare (type vec a b)) (sub a b)) aa bb))


(defun lsub* (aa b)
  (declare (optimize (safety 0) speed (debug 0)) (list aa) (vec b))
  (mapcar (lambda (a) (declare (type vec a)) (sub a b)) aa))


(defun isub (a b)
  (declare (optimize (safety 0) speed (debug 0)) (vec a b))
  (vec (- (vec-x b) (vec-x a)) (- (vec-y b) (vec-y a))))


(defun op (fx a b)
  (declare (optimize (safety 0) speed (debug 0)) (vec a b))
  (vec (funcall fx (vec-x a) (vec-x b))
       (funcall fx (vec-y a) (vec-y b))))


(defun vabs (a)
  (declare (optimize (safety 0) speed (debug 0)) (vec a))
  (vec (abs (vec-x a)) (abs (vec-y a))))

(declaim (inline add))
(defun add (a b)
  (declare (optimize (safety 0) speed (debug 0)) (vec a b))
  (vec (+ (vec-x a) (vec-x b)) (+ (vec-y a) (vec-y b))))


(defun ladd (aa bb)
  (declare (optimize (safety 0) speed (debug 0)) (list aa bb))
  (mapcar (lambda (a b) (declare (type vec a b)) (add a b)) aa bb))


(defun ladd* (aa b)
  (declare (optimize (safety 0) speed (debug 0)) (list aa) (vec b))
  (mapcar (lambda (a) (declare (type vec a)) (add a b)) aa))


(defun lscale* (aa s)
  (declare (optimize (safety 0) speed (debug 0)) (list aa) (double-float s))
  (mapcar (lambda (a) (declare (type vec a)) (scale a s)) aa))


(defun mult (a b)
  (declare (optimize (safety 0) speed (debug 0)) (vec a b))
  (vec (* (vec-x a) (vec-x b)) (* (vec-y a) (vec-y b))))


(defun lmult (aa bb)
  (declare (optimize (safety 0) speed (debug 0)) (list aa bb))
  (mapcar (lambda (a b) (declare (type vec a b)) (mult a b)) aa bb))


(defun lmult* (aa b)
  (declare (optimize (safety 0) speed (debug 0)) (list aa) (vec b))
  (mapcar (lambda (a) (declare (type vec a)) (mult a b)) aa))


(defun dot (a b)
  (declare (optimize (safety 0) speed (debug 0)) (vec a b))
  (+ (* (vec-x a) (vec-x b)) (* (vec-y a) (vec-y b))))


(defun ldot (aa bb)
  (declare (optimize (safety 0) speed (debug 0)) (list aa bb))
  (mapcar (lambda (a b) (declare (type vec a b)) (dot a b)) aa bb))


(defun div (a b)
  (declare (optimize (safety 0) speed (debug 0)) (vec a b))
  (vec (/ (vec-x a) (vec-x b)) (/ (vec-y a) (vec-y b))))


(defun ldiv (aa bb)
  (declare (optimize (safety 0) speed (debug 0)) (list aa bb))
  (mapcar (lambda (a b) (declare (type vec a b)) (div a b)) aa bb))


(defun ldiv* (aa b)
  (declare (optimize (safety 0) speed (debug 0)) (list aa) (vec b))
  (mapcar (lambda (a) (declare (type vec a)) (div a b)) aa))


(defun idiv (a b)
  (declare (optimize (safety 0) speed (debug 0)) (vec a b))
  (vec (/ (vec-x b) (vec-x a)) (/ (vec-y b) (vec-y a))))


(defun len2 (a)
  (declare (optimize (safety 0) speed (debug 0)) (vec a))
  (+ (expt (vec-x a) 2) (expt (vec-y a) 2)))

(defun len (a)
  (declare (optimize (safety 0) speed (debug 0)) (vec a))
  (sqrt (len2 a)))


(defun mid (a b)
  (declare (optimize (safety 0) speed (debug 0)) (vec a b))
  (iscale (add a b) 2d0))


(defun lmid (aa)
  (declare (optimize (safety 0) speed (debug 0)) (list aa))
  (let ((n 1))
    (iscale
      (reduce (lambda (a b) (declare (type vec a b)) (incf n) (add a b)) aa)
      (math:dfloat n))))


(defun dst2 (a b)
  (declare (optimize (safety 0) speed (debug 0)) (vec a b))
  (+ (expt (- (vec-x a) (vec-x b)) 2d0) (expt (- (vec-y a) (vec-y b)) 2d0)))

(defun dst (a b)
  (declare (optimize (safety 0) speed (debug 0)) (vec a b))
  (sqrt (dst2 a b)))


(defun dst* (aa)
  (declare (optimize (safety 0) speed (debug 0)) (list aa))
  (dst (first aa) (second aa)))


(defun ldst (a b)
  (declare (optimize (safety 0) speed (debug 0)) (list a b))
  (mapcar #'dst a b))


(defun ldst* (aa b)
  (declare (optimize (safety 0) speed (debug 0)) (list aa) (vec b))
  (loop for a of-type vec in aa collect (dst a b)))


(defun norm (a &key (s 1d0) (default *zero*))
  (declare (optimize (safety 0) speed (debug 0)) (vec a) (double-float s))
  (let ((l (len a)))
    (if (> l 0d0) (scale a (/ s l)) default)))


(defun nsub (a b &key (s 1d0) (default *zero*))
  (declare (vec a b))
  (norm (sub a b) :s s :default default))


(defun sum (aa)
  (declare (optimize (safety 0) speed (debug 0)) (list aa))
  (reduce (lambda (a b) (declare (vec a b)) (add a b)) aa))


(defun rot (v a &key (xy *zero*))
  (declare (vec v) (double-float a))
  (let ((cosa (cos a))
        (sina (sin a)))
    (with-xy ((sub v xy) x y)
      (add xy (vec (- (* x cosa) (* y sina))
                   (+ (* x sina) (* y cosa)))))))


(defun lrot (pts a &key (xy *zero*))
  (declare (list pts) (double-float a))
  (mapcar (lambda (p) (declare (vec p)) (rot p a :xy xy)) pts))


(defun shift-scale (pt shift s &optional (unshift *zero*))
  "shift scale (unshift)"
  (declare (vec pt shift) (double-float s))
  (add (scale (sub pt shift) s) unshift))


(defun shift-scale* (pts shift s &optional unshift)
  (declare (list pts) (vec shift) (double-float s))
  (mapcar (lambda (pt) (declare (vec pt))
            (shift-scale pt shift s unshift)) pts))


(defun all-inside (path &optional (lim 1d6))
  (if (every #'identity
             (loop for v in path
                   collect (and (>= (vec-x v) (- lim)) (<= (vec-x v) lim)
                                (>= (vec-y v) (- lim)) (<= (vec-y v) lim))))
      path
      nil))


(defun segdst (line v)
  "
  find distance between line and v.
  returns values (distance s) where is is the interpolation value that will
  yield the closest point on line.
  "
  (declare (list line) (vec v))
  (destructuring-bind (va vb) line
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


; TODO: this is slow?
(defun segx (aa bb &key parallel)
  (declare (list aa bb))
  (destructuring-bind (a1 a2) aa
    (declare (vec:vec a1 a2))
    (destructuring-bind (b1 b2) bb
      (declare (vec:vec b1 b2))
      (let* ((sa (sub a2 a1))
             (sb (sub b2 b1))
             (u (vec:cross sa sb)))
        (declare (vec:vec sa sb) (double-float u))
        (if (<= (abs u) 0d0)
          ; return parallel if the lines are parallel (default: nil)
          ; this is just a div0 guard. it's not a good way to test.
          (values parallel nil nil)
          ; otherwise check if they intersect
          (let ((p (/ (vec:cross sa #1=(vec:sub a1 b1)) u))
                (q (/ (vec:cross sb #1#) u)))
            (declare (double-float p q))
            ; t if intersection
            ; nil otherwise
            (values (and (>= p 0d0) (<= p 1d0) (>= q 0d0) (<= q 1d0))
                    q p)))))))


(defun segx* (l &key parallel)
  (declare (list l))
  (destructuring-bind (a b) l
    (segx a b :parallel parallel)))


; TODO: this is slow?
(defun psegx (aa bb &key parallel
                    &aux (aa* (ensure-vector aa))
                         (bb* (ensure-vector bb)))

  (loop with res = (make-adjustable-vector)
        for i of-type fixnum from 0 below (1- (length aa*))
        do (loop for j of-type fixnum from 0 below (1- (length bb*))
                 do (multiple-value-bind (x s p)
                      (vec:segx (list (aref aa* i) (aref aa* (1+ i)))
                                (list (aref bb* j) (aref bb* (1+ j)))
                                :parallel parallel)
                      (when x (vextend (list i j s p) res))))
        finally (return (if (> (length res) 0) res nil))))


; TODO: incomplete
;(defun lsegx* (lines line &key parallel)
;  (declare (list lines line))
;  (loop with res = (make-adjustable-vector)
;        for l of-type list in lines
;        ; TODO: sort and rearrange
;        do (multiple-value-bind (x p q) (segx l line)
;             (when x (vextend (list x p q) res)))
;        finally (return res)))


(defun cross (a b)
  (declare (vec a b))
  (- (* (vec-x a) (vec-y b)) (* (vec-y a) (vec-x b))))


(defun ptinside (convex v)
  (declare (list convex) (vec v))
  (loop for a of-type vec in (close-path convex)
        and b of-type vec in (cdr (close-path convex))
        always (>= (cross (sub b a) (sub v b)) 0d0)))


; SHAPES

(defun on-circ (p rad &key (xy *zero*))
  (declare (double-float p rad) (vec xy))
  (from xy (cos-sin (* p PII)) rad))


(defun on-line (p a b)
  (declare (optimize (safety 0) speed (debug 0))
           (double-float p) (vec a b))
  (vec (+ (vec-x a) (* p (- (vec-x b) (vec-x a))))
       (+ (vec-y a) (* p (- (vec-y b) (vec-y a))))))


(defun on-line* (p ab)
  (declare (double-float p) (list ab))
  (destructuring-bind (a b) ab
    (on-line p a b)))


(defun lon-line* (pp ab)
  (declare (sequence pp) (list ab))
  (destructuring-bind (a b) ab
    (if (equal (type-of pp) 'cons)
      (loop for p of-type double-float in pp collect (on-line p a b))
      (loop for p of-type double-float across pp collect (on-line p a b)))))


(defun on-spiral (p rad &key (xy *zero*) (rot 0d0))
  (declare (double-float p rad rot) (vec xy))
  (add xy (scale (cos-sin (+ rot (* p PII)))
                 (* p rad))))


(defun rect (w h &key (xy *zero*))
  (declare (double-float w h) (vec xy))
  (list (add xy (vec w (- h)))
        (add xy (vec w h))
        (add xy (vec (- w) h))
        (sub xy (vec w h))))


(defun square (bs &key xy)
  (declare (double-float bs) (vec xy))
  (rect bs bs :xy xy))


(defun polygon (n rad &key (xy *zero*) (rot 0d0))
  (declare (fixnum n) (double-float rad rot) (vec xy))
  (loop for i from 0 below n
        collect (from xy (cos-sin (+ rot (* (/ (math:dfloat i) n) PII))) rad)))

(defun fan (n rad &key (xy *zero*) (rot 0d0))
  (declare (fixnum n) (double-float rad rot) (vec xy))
  (loop for p in (polygon n rad :xy xy :rot rot)
        collect (list xy p)))

