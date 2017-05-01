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


(defun lgetrnd (l)
  (nth (random (length l)) l))


(defun agetrnd (l)
  (aref l (random (length l))))


; RANGES

(defun range (a &optional (b nil))
  (if (not b)
    (loop for x from 0 below a collect x)
    (loop for x from a below b collect x)))


(defun rndi (a &optional (b nil))
  (let ((a (to-int a)))
    (if (not b)
      (random a)
      (+ a (random (- (to-int b) a))))))


(defun rnd (&optional (x 1.0d0))
  (random (to-dfloat x)))


(defun rnd* (&optional (x 1.0d0))
  (- x (* 2.0d0 (random (to-dfloat x)))))


(defmacro nrep (n &body body)
  (with-gensyms (i nname)
    `(let ((,nname ,n))
      (loop for ,i from 0 below ,nname collect ,@body))))


(defun inc (x stp) (mod (+ x stp) 1.0d0))


(defmacro get-rnd-lin-stp (&optional (init 0.0d0))
  (with-gensyms (x stp)
    `(let ((,x (to-dfloat ,init)))
      (lambda (,stp)
        (setf ,x (inc ,x (rnd* ,stp)))))))

(defmacro get-rnd-lin-stp* (&optional (init 0.0d0))
  (with-gensyms (x stp)
    `(let ((,x (to-dfloat ,init)))
      (lambda (,stp)
        (incf ,x (rnd* ,stp))))))


(defmacro get-acc-rnd-lin-stp (&optional (init-x 0.0d0) (init-a 0.0d0))
  (with-gensyms (x a s)
    `(let ((,a (to-dfloat ,init-a))
           (,x (to-dfloat ,init-x)))
      (lambda (,s)
        (setf ,x (inc ,x (incf ,a (rnd* ,s))))))))


(defmacro get-acc-rnd-lin-stp* (&optional (init-x 0.0d0) (init-a 0.0d0))
  (with-gensyms (x a s)
    `(let ((,a (to-dfloat ,init-a))
           (,x (to-dfloat ,init-x)))
      (lambda (,s)
        (incf ,x (incf ,a (rnd* ,s)))))))


(defmacro get-rnd-circ-stp* (&optional (init '(list 0.0d0 0.0d0)))
  (with-gensyms (xy stp)
    `(let ((,xy (to-dfloat* ,init)))
      (lambda (,stp)
        (add ,xy (rnd-in-circ ,stp))))))


(defmacro get-acc-rnd-circ-stp* (&optional (init '(list 0.0d0 0.0d0))
                                           (init-a '(list 0.0d0 0.0d0)))
  (with-gensyms (xy stp a)
    `(let ((,a (to-dfloat* ,init-a))
           (,xy (to-dfloat* ,init)))
      (lambda (,stp)
        (setf ,xy
              (add ,xy
                   (setf ,a (add ,a (rnd-in-circ ,stp)))))))))


(defun linspace (a b n &key (end t))
  (if (> n 1)
    (let ((nn (if end (1- n) n)))
      (loop for i from 0 below n
          collect (to-dfloat (+ a (* i (/ (- b a) nn))))))
    (list (to-dfloat a))))


(defun rndspace (a b n)
  (let ((d (to-dfloat (- b a))))
    (sort (nrep n (+ a (random d))) #'<)))


(defun rndspace* (a b n)
  (let ((d (to-dfloat (- b a))))
    (nrep n (+ a (random d)))))


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


(defun rnd-on-circ (rad &key (xy (list 0.0d0 0.0d0)))
  (add
    xy
    (scale
      (cos-sin (random (* PI 2.0d0)))
      rad)))


(defun rnd-in-circ (rad &key (xy (list 0.0d0 0.0d0)))
  (let ((ab (sort (list (random 1.0d0) (random 1.0d0)) #'<)))
    (add
      xy
      (scale
        (cos-sin (* 2 PI (apply #'/ ab)))
        (* (second ab) rad)))))


(defun rnd-in-box (sx sy &key (xy (list 0.0d0 0.0d0)))
  (add
    (list (rnd* (to-dfloat sx))
          (rnd* (to-dfloat sy)))
    xy))


(defun rnd-on-line (x1 x2)
  (add
    x1
    (scale (sub x2 x1) (random 1.0d0))))


(defun rnd-on-spiral (rad &key (xy (list 0.0d0 0.0d0)) (rot 1.0d0))
  (let ((i (random 1.0)))
    (add
      xy
      (scale
        (cos-sin (* i (* PI rot)))
        (* i rad)))))


(defun polygon (n rad &key (xy (list 0.0d0 0.0d0)) (rot 0.0d0))
  (loop for i from 0 below n
    collect
      (add
        xy
        (scale
          (cos-sin (+ rot (* (/ i n) 2.0d0 PI)))
          rad))))


(defun -make-front-path (aa bb cc as bs)
  (let ((p1 (add cc (scale (sub aa cc) as)))
        (p2 (add cc (scale (sub bb cc) bs))))
    (list p1 cc p2)))


(defun -make-full-path (aa bb cc as bs)
  (let ((p1 (add cc (scale (sub aa cc) as)))
        (p2 (add cc (scale (sub bb cc) bs))))
    (list p1 cc p2 (add p2 (scale (sub aa p2) as)) p1)))


; three point perspective

(defun make-perspective-transform (A B C)
  (lambda (P a* b* u* d*)
    (let ((PC (sub C P)))
      (let ((U (sub P (scale PC u*)))
            (D (add P (scale PC d*))))
        (append (-make-full-path A B U a* b*) (-make-full-path A B D a* b*))))))


; OTHER

(defun close-path (p)
  (append p (list (nth 0 p))))


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

