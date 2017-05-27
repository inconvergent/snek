
(defmacro cos-sin (a)
  (with-gensyms (aname)
    `(let ((,aname ,a))
        (list (cos ,aname) (sin ,aname)))))

(defmacro sin-cos (a)
  (with-gensyms (aname)
    `(let ((,aname ,a))
        (list (sin ,aname) (cos ,aname)))))


(defmacro square-loop ((x y s) &body body)
  (with-gensyms (sname)
    `(let ((,sname ,s))
      (loop for ,x from 0 below ,sname do
        (loop for ,y from 0 below ,sname do
          ,@body)))))


; VEC


(defun diff-scale (a b s) (/ (- b a) s))


(defun lexpt (xx p)
  (mapcar (lambda (x) (expt x p)) xx))


(defun scale (a s)
  (mapcar (lambda (x) (* x s)) a))


(defun iscale (a s)
  (mapcar (lambda (x) (/ x s)) a))


(defun sub (a b)
  (mapcar #'- a b))


(defun isub (a b)
  (mapcar #'- b a))


(defun add (a b)
  (mapcar #'+ a b))


(defun sum (a)
  (reduce #'+ a))


(defun mult (a b)
  (mapcar #'* a b))


(defun dot (a b)
  (apply #'+ (mult a b)))


(defun div (a b)
  (mapcar #'/ a b))


(defun idiv (a b)
  (mapcar #'/ b a))


(defun len2 (a)
  (reduce #'+ (mapcar (lambda (x) (* x x)) a)))


(defun lmid (a)
  (iscale (reduce #'add a) (length a)))


(defun mid (a b)
  (iscale (add a b) 2.0d0))


(defun len (a)
  (sqrt (len2 a)))


(defun dst (a b)
  (len (sub a b)))


(defun dst2 (a b)
  (len2 (sub a b)))


(defun norm (a)
  (let ((l (len a)))
    (cond
      ((<= l 0d0) a)
      (t (scale a (/ 1.0d0 l))))))


(defun nsub (a b)
  (norm (sub a b)))


(defun nadd (a b)
  (norm (add a b)))


(defun lround (l)
  (mapcar #'round l))


(defun lget (a ii)
  (mapcar (lambda (i) (nth i a)) ii))


; RANGES


(defmacro rep ((i itt) &body body)
  `(loop for ,i in ,itt
    collect ,@body))


(defmacro nrep (n &body body)
  (with-gensyms (i nname)
    `(let ((,nname ,n))
      (loop for ,i from 0 below ,nname
            collect ,@body))))


(defun range (a &optional (b nil))
  (if (not b)
    (loop for x from 0 below a collect x)
    (loop for x from a below b collect x)))


(defun inc (x stp) (mod (+ x stp) 1.0d0))


(defun linspace (a b n &key (end t))
  (if (> n 1)
    (let ((nn (if end (1- n) n)))
      (loop for i from 0 below n
          collect (to-dfloat (+ a (* i (/ (- b a) nn))))))
    (list (to-dfloat a))))


(defun range (a &optional (b nil))
  (if (not b)
    (loop for x from 0 below a collect x)
    (loop for x from a below b collect x)))


; GET SET


(defun get-as-list (arr row &optional (dims (range 2)))
  (mapcar
    (lambda (d) (aref arr row d))
    dims))


(defun set-from-list (arr row vals)
  (mapcar
    (lambda (d v) (setf (aref arr row d) v))
    (list 0 1)
    vals))


; SHAPES


(defun on-spiral (p rad &key (xy (list 0.0d0 0.0d0)) (rot 1.0d0))
  (add
    xy
    (scale
      (cos-sin (* p PI rot))
      (* p rad))))


(defun on-circ (p rad &key (xy (list 0.0d0 0.0d0)))
  (add
    xy
    (scale
      (cos-sin (* p PI 2.0d0))
      rad)))


(defun on-line (p x1 x2)
  (add
    x1
    (scale (sub x2 x1) p)))


(defun polygon (n rad &key (xy (list 0.0d0 0.0d0)) (rot 0.0d0))
  (loop for i from 0 below n
    collect
      (add
        xy
        (scale
          (cos-sin (+ rot (* (/ i n) 2.0d0 PI)))
          rad))))


; THREE POINT


(defun -make-front-path (aa bb cc as bs)
  (let ((p1 (add cc (scale (sub aa cc) as)))
        (p2 (add cc (scale (sub bb cc) bs))))
    (list p1 cc p2)))


(defun -make-full-path (aa bb cc as bs)
  (let ((p1 (add cc (scale (sub aa cc) as)))
        (p2 (add cc (scale (sub bb cc) bs))))
    (list p1 cc p2 (add p2 (scale (sub aa p2) as)) p1)))


(defun make-perspective-transform (A B C)
  (lambda (P a* b* u* d*)
    (let ((PC (sub C P)))
      (let ((U (sub P (scale PC u*)))
            (D (add P (scale PC d*))))
        (append (-make-full-path A B U a* b*) (-make-full-path A B D a* b*))))))


; OTHER


(defun close-path (p)
  (append p (list (nth 0 p))))


(defmacro inside-border ((size xy b) &body body)
  (with-gensyms (xname yname sname small large)
    `(let* ((,sname ,size)
            (,small ,b)
            (,large (- ,sname ,small)))
      (destructuring-bind (,xname ,yname)
        ,xy
        (if (and (>= ,xname ,small) (< ,xname ,large)
                 (>= ,yname ,small) (< ,yname ,large))
          (progn
            ,@body))))))


; TODO: remove external use where lround does not make sense.
(defmacro inside ((size xy) &body body)
  (with-gensyms (xname yname sname)
    `(let ((,sname ,size))
      (destructuring-bind (,xname ,yname)
        (lround ,xy)
        (if (and (>= ,xname 0) (< ,xname ,sname)
                 (>= ,yname 0) (< ,yname ,sname))
          (progn
            ,@body))))))


(defmacro inside* ((size xy x y) &body body)
  (with-gensyms (sname)
    `(let ((,sname ,size))
      (destructuring-bind (,x ,y)
        (lround ,xy)
        (if (and (>= ,x 0) (< ,x ,sname)
                 (>= ,y 0) (< ,y ,sname))
          (progn
            ,@body))))))

