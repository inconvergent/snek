
(in-package :math)


(defmacro cos-sin (a)
  (with-gensyms (aname)
    `(let ((,aname ,a))
        (list (cos ,aname) (sin ,aname)))))


(defmacro sin-cos (a)
  (with-gensyms (aname)
    `(let ((,aname ,a))
        (list (sin ,aname) (cos ,aname)))))


; TYPES

(defun int (x)
  (the integer (coerce x 'integer)))


(defun int* (xx)
  (mapcar (lambda (x) (int x)) xx))


(defun dfloat (x)
  (the double-float (coerce x 'double-float)))


(defun dfloat* (xx)
  (mapcar (lambda (x) (dfloat x)) xx))


; VEC

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
    (if (> l 0.0d0) (iscale a l) a)))


(defun nsub (a b)
  (norm (sub a b)))


(defun lround (l)
  (mapcar #'round l))


; RANGES


(defmacro rep ((i itt) &body body)
  `(loop for ,i in ,itt collect (progn ,@body)))


(defmacro nrep (n &body body)
  (with-gensyms (i nname)
    `(let ((,nname ,n))
      (loop for ,i integer from 1 to ,nname collect (progn ,@body)))))


(defun range (a &optional (b nil))
  (if (not b)
    (loop for x integer from 0 below a collect x)
    (loop for x integer from a below b collect x)))


(defun inc (x stp)
  (mod (+ x stp) 1.0d0))


(defun linspace (a b n &key (end t))
  (if (> n 1)
    (let ((nn (if end (1- n) n)))
      (loop for i from 0 below n
        collect (dfloat (+ a (* i (/ (- b a) nn))))))
    (list (dfloat a))))


; SHAPES


(defun on-spiral (p rad &key (xy (list 0.0d0 0.0d0)) (rot 1.0d0))
  (add xy (scale (cos-sin (* p PI rot)) (* p rad))))


(defun on-circ (p rad &key (xy (list 0.0d0 0.0d0)))
  (add xy (scale (cos-sin (* p PI 2.0d0)) rad)))


(defun on-line (p x1 x2)
  (add x1 (scale (sub x2 x1) p)))


(defun polygon (n rad &key (xy (list 0.0d0 0.0d0)) (rot 0.0d0))
  (loop for i from 0 below n collect (add xy
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

