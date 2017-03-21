
(defmacro cos-sin (a)
  (with-gensyms (aname)
    `(let ((,aname ,a))
        (list (cos ,aname) (sin ,aname)))))

(defmacro sin-cos (a)
  (with-gensyms (aname)
    `(let ((,aname ,a))
        (list (sin ,aname) (cos ,aname)))))


(defmacro 2d-square-loop ((x y s) &body body)
  (with-gensyms (sname)
    `(let ((,sname ,s))
      (loop for ,x from 0 below ,sname do
        (loop for ,y from 0 below ,sname do
          ,@body)))))


(defun diff-scale (a b s) (/ (- b a) s))


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


(defun lenn (a)
  (reduce #'+ (mapcar (lambda (x) (* x x)) a)))


(defun lmid (a)
  (iscale (reduce #'add a) (length a)))


(defun mid (a b)
  (iscale (add a b) 2.0))


(defun len (a)
  (sqrt (lenn a)))


(defun dst (a b)
  (len (sub a b)))


(defun norm (a)
  (let ((l (len a)))
    (cond
      ((<= l 0) a)
      (t (scale a (/ 1.0 l))))))


(defun nsub (a b)
  (norm (sub a b)))


(defun nadd (a b)
  (norm (add a b)))


(defun lround (l)
  (mapcar #'round l))


(defun lget (ii a)
  (mapcar (lambda (i) (nth i a)) ii))


(defun lgetrnd (l)
  (nth (random (list-length l)) l))


; TODO: add dim option
(defun get-as-list (arr row)
  (mapcar
    (lambda (d) (aref arr row d))
    (list 0 1)))


(defun set-from-list (arr row vals)
  (mapcar
    (lambda (d v) (setf (aref arr row d) v))
    (list 0 1)
    vals))



; SHAPES AND RANGES



(defun rnd-float (&optional (x 1.0))
  (random (to-float x)))


(defun rnd-float* (&optional (x 1.0))
  (- x (* 2.0 (random (to-float x)))))


(defmacro nrep (n &body body)
  (with-gensyms (i nname)
    `(let ((,nname ,n))
      (loop for ,i from 0 below ,nname collect ,@body))))


(defun inc (x stp) (mod (+ x stp) 1.0))


(defmacro get-rnd-lin-stp (&optional (init 0.0))
  (with-gensyms (x stp)
    `(let ((,x ,init))
      (lambda (,stp)
        (setf ,x (inc ,x (rnd-float* ,stp)))))))

(defmacro get-rnd-lin-stp* (&optional (init 0.0))
  (with-gensyms (x stp)
    `(let ((,x ,init))
      (lambda (,stp)
        (incf ,x (rnd-float* ,stp))))))

(defmacro get-rnd-circ-stp* ()
  (with-gensyms (xy stp)
    `(let ((,xy (list 0.0 0.0)))
      (lambda (,stp)
        (add ,xy (rnd-in-circ ,stp))))))


(defun linspace (a b n)
  (if (> n 1)
    (let ((nn (1- n)))
      (loop for i from 0 below n
          collect (to-float (+ a (* i (/ (- b a) nn))))))
    (list (to-float a))))


(defun rndspace (a b n)
  (let ((d (to-float (- b a))))
    (sort (nrep n (+ a (random d))) #'<)))


(defun rndspace* (a b n)
  (let ((d (to-float (- b a))))
    (nrep n (+ a (random d)))))


(defun range (a &optional (b nil))
  (if (not b)
    (loop for x from 0 below a collect x)
    (loop for x from a below b collect x)))


; SHAPES


(defun on-spiral (i itt rad &key (x 0.0) (y 0.0) (rot 1.0))
  (add
    (list x y)
    (scale
      (cos-sin (/ (* i PI rot) itt))
      (* (/ i itt) rad))))


(defun on-circ (i itt rad &key (x 0.0) (y 0.0))
  (add
    (list x y)
    (scale
      (cos-sin (/ (* i PI 2.0) itt))
      rad)))


(defun on-line (i itt x1 x2)
  (add
    x1
    (scale
      (sub x2 x1)
      (/ i itt))))


(defun rnd-on-circ (rad &key (x 0.0) (y 0.0))
  (add
    (list x y)
    (scale
      (cos-sin (random (* PI 2.0)))
      rad)))


(defun rnd-in-circ (rad &key (x 0.0) (y 0.0))
  (let ((ab (sort (list (random 1.0) (random 1.0)) #'<)))
    (add
      (list x y)
      (scale
        (cos-sin (* 2 PI (apply #'/ ab)))
        (* (second ab) rad)))))


(defun rnd-in-box (sx sy &key (x 0.0) (y 0.0))
  (add
    (list (rnd-float* (to-float sx))
          (rnd-float* (to-float sy)))
    (list x y)))


(defun rnd-on-line (x1 x2)
  (add
    x1
    (scale (sub x2 x1) (random 1.0))))


(defun rnd-on-spiral (rad &key (x 0.0) (y 0.0) (rot 1.0))
  (let ((i (random 1.0)))
    (add
      (list x y)
        (scale
          (cos-sin (* i (* PI rot)))
          (* i rad)))))


(defun polygon (n rad &key (x 0.0) (y 0.0) (rot 0.0))
  (loop for i from 0 to n
    collect
      (add
        (list x y)
        (scale
          (cos-sin (+ rot (* (/ i n) 2 PI)))
          rad))))


