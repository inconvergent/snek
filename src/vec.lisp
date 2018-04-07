
(in-package :vec)


(defmacro inside* ((size xy x y) &body body)
  (with-gensyms (sname)
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


(defmacro with-xy-short ((v x y) &body body)
  `(let ((,x (math:sfloat (vec-x ,v)))
         (,y (math:sfloat (vec-y ,v))))
    (progn ,@body)))


(defmacro with-loop-grid ((grid xy) &body body)
  (with-gensyms (grid* x y)
    `(let ((,grid* ,grid))
      (loop for ,y in ,grid* do
        (loop for ,x in ,grid* do
          (let ((,xy (vec ,x ,y)))
            (progn ,@body)))))))


(defmacro with-loop-grid* ((grid xy) &body body)
  (with-gensyms (x y)
    `(loop for ,y in ,grid do
      (loop for ,x in ,grid do
        (let ((,xy (vec ,x ,y)))
          (progn ,@body))))))


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


(defun vround (v)
  (declare (vec v))
  (list
    (round (vec-x v))
    (round (vec-y v))))


(defun vec* (xy)
  (declare (list xy))
  (destructuring-bind (x y)
    (math:dfloat* xy)
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
  (setf (aref a i 0) (vec::vec-x v)
        (aref a i 1) (vec::vec-y v)))


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


(defun scale (a s)
  (declare (vec a))
  (declare (double-float s))
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


(defun sub (a b)
  (declare (vec a b))
  (vec (- (vec-x a) (vec-x b))
      (- (vec-y a) (vec-y b))))


(defun lsub (aa bb)
  (declare (list aa bb))
  (mapcar (lambda (a b) (sub a b))
          aa bb))


(defun lsub* (aa b)
  (declare (list aa))
  (declare (vec b))
  (mapcar (lambda (a) (sub a b))
          aa))


(defun isub (a b)
  (declare (vec a b))
  (vec (- (vec-x b) (vec-x a))
      (- (vec-y b) (vec-y a))))


(defun op (fx a b)
  (declare (vec a b))
  (vec (funcall fx (vec-x a) (vec-x b))
       (funcall fx (vec-y a) (vec-y b))))


(defun add (a b)
  (declare (vec a b))
  (vec (+ (vec-x a) (vec-x b))
       (+ (vec-y a) (vec-y b))))


(defun ladd (aa bb)
  (declare (list aa bb))
  (mapcar (lambda (a b) (add a b))
          aa bb))


(defun ladd* (aa b)
  (declare (list aa))
  (declare (vec b))
  (mapcar (lambda (a) (add a b))
          aa))


(defun lscale* (aa s)
  (declare (list aa))
  (declare (double-float s))
  (mapcar (lambda (a) (scale a s))
          aa))


(defun mult (a b)
  (declare (vec a b))
  (vec (* (vec-x a) (vec-x b))
      (* (vec-y a) (vec-y b))))


(defun lmult (aa bb)
  (declare (list aa bb))
  (mapcar (lambda (a b) (mult a b))
          aa bb))


(defun lmult* (aa b)
  (declare (list aa))
  (declare (vec b))
  (mapcar (lambda (a) (mult a b))
          aa))


(defun dot (a b)
  (declare (vec a b))
  (+ (* (vec-x a) (vec-x b))
     (* (vec-y a) (vec-y b))))


(defun div (a b)
  (declare (vec a b))
  (vec (/ (vec-x a) (vec-x b))
      (/ (vec-y a) (vec-y b))))


(defun ldiv (aa bb)
  (declare (list aa bb))
  (mapcar (lambda (a b) (div a b))
          aa bb))


(defun ldiv* (aa b)
  (declare (list aa))
  (declare (vec b))
  (mapcar (lambda (a) (div a b))
          aa))


(defun idiv (a b)
  (declare (vec a b))
  (vec (/ (vec-x b) (vec-x a))
      (/ (vec-y b) (vec-y a))))


(defun len2 (a)
  (declare (vec a))
  (+ (expt (vec-x a) 2)
     (expt (vec-y a) 2)))

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
      (reduce (lambda (a b) (incf n) (add a b))
              aa)
      (math:dfloat n))))


(defun dst2 (a b)
  (declare (vec a b))
  (len2 (sub a b)))


(defun dst (a b)
  (declare (vec a b))
  (len (sub a b)))


(defun ldst (a b)
  (declare (list a b))
  (mapcar #'dst a b))


(defun ldst* (aa b)
  (declare (list aa))
  (declare (vec b))
  (loop for a in aa collect (dst a b)))


(defun norm (a)
  (declare (vec a))
  (let ((l (len a)))
    (if (> l 0d0) (iscale a l) a)))


(defun nsub (a b)
  (declare (vec a b))
  (norm (sub a b)))


(defun sum (aa)
  (declare (list aa))
  (reduce (lambda (a b) (declare (vec a b)) (add a b))
          aa))


(defun rot (v a &key (xy (zero)))
  (declare (vec v))
  (declare (double-float a))
  (let ((cosa (cos a))
        (sina (sin a)))
    (with-xy ((sub v xy) x y)
      (add xy (vec (- (* x cosa) (* y sina))
                   (+ (* x sina) (* y cosa)))))))


(defun lrot (pts a &key (xy (zero)))
  (declare (list pts))
  (declare (double-float a))
  (mapcar (lambda (p) (declare (vec p))
            (rot p a :xy xy)) pts))


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
          (values
            (and (>= p 0d0) (<= p 1d0) (>= q 0d0) (<= q 1d0))
            q p))))))


(defun segx* (l &key parallel)
  (declare (list l))
  (destructuring-bind (a b)
    l
    (vec:segx a b :parallel parallel)))


(defun cross (a b)
  (declare (vec a))
  (declare (vec b))
  (- (* (vec-x a) (vec-y b)) (* (vec-y a) (vec-x b))))


(defun ptinside (convex v)
  (declare (list convex))
  (declare (vec v))
  (loop for a in (close-path convex)
        and b in (cdr (close-path convex)) always
    (>= (cross (sub b a) (sub v b)) 0d0)))


; SHAPES

(defun on-circ (p rad &key (xy (vec:zero)))
  (declare (double-float p rad))
  (declare (vec xy))
  (vec:add xy (vec:scale (vec:cos-sin (* p PII)) rad)))


(defun on-line (p a b)
  (declare (double-float p))
  (declare (vec a b))
  (vec:add a (vec:scale (vec:sub b a) p)))


(defun on-line* (p ab)
  (declare (double-float p))
  (declare (list ab))
  (destructuring-bind (a b)
    ab
    (on-line p a b)))


(defun on-spiral (p rad &key (xy (vec:zero)) (rot 0d0))
  (declare (double-float p rad rot))
  (declare (vec xy))
  (vec:add xy (vec:scale (vec:cos-sin (+ rot (* p PII)))
                         (* p rad))))


(defun rect (w h &key (xy (vec:zero)))
  (declare (double-float w h))
  (declare (vec xy))
  (list
    (vec:add xy (vec:vec w (- h)))
    (vec:add xy (vec:vec w h))
    (vec:add xy (vec:vec (- w) h))
    (vec:sub xy (vec:vec w h))))


(defun square (bs &key xy)
  (declare (double-float bs))
  (declare (vec xy))
  (rect bs bs :xy xy))


(defun polygon (n rad &key (xy (vec:zero)) (rot 0d0))
  (declare (integer n))
  (declare (double-float rad rot))
  (declare (vec xy))
  (loop for i from 0 below n
    collect (vec:add (vec:scale (vec:cos-sin (+ rot (* (/ i n) PII))) rad) xy)))

