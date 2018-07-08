
(in-package :math)

(defvar dotlim 0.95d0)

(declaim (ftype (function (number) double-float) dfloat))
(declaim (ftype (function (number) fixnum) int))
(declaim (ftype (function (number) float) sfloat))


; TYPES

(defun int (x)
  (the fixnum
    (coerce (floor x) 'fixnum)))


(defun int* (xx)
  (mapcar (lambda (x) (int (the fixnum (floor x)))) xx))


(defun sfloat (x)
  (the float (coerce x 'float)))


(defun sfloat* (xx)
  (mapcar (lambda (x) (sfloat x)) xx))


(defun dfloat (x)
  (the double-float (coerce x 'double-float)))


(defun dfloat* (xx)
  (mapcar (lambda (x) (dfloat x)) xx))


; RANGES


(defmacro rep ((i itt) &body body)
  `(loop for ,i in ,itt collect (progn ,@body)))


(defmacro nrep (n &body body)
  (with-gensyms (nname)
    `(let ((,nname ,n))
      (loop repeat ,nname collect (progn ,@body)))))


(defun range (a &optional (b nil))
  (declare (fixnum a))
  (if (not b)
      (loop for x of-type fixnum from 0 below a collect x)
      (loop for x of-type fixnum from a below (the fixnum b) collect x)))


(defun lget (l ii)
  "
  get indices ii from l
  "
  (declare (list l ii))
  (loop with arr = (to-array l)
        for i of-type fixnum in ii collect (aref arr i)))


(defun inc (x stp)
  (mod (+ x stp) 1d0))


(defun mod- (i n)
  (declare (fixnum i n))
  (mod (+ n i -1) n))


(defun mod+ (i n)
  (declare (fixnum i n))
  (mod (+ n i 1) n))


(defun mod2 (i)
  (declare (fixnum i))
  (mod i 2))


(defmacro with-linspace ((n a b rn &key (end t) collect) &body body)
  (declare (symbol rn))
  (with-gensyms (a* b* n* nn i ba)
  `(let* ((,n* (int ,n))
          (,nn (dfloat (if ,end (1- ,n*) ,n*)))
          (,a* (dfloat ,a))
          (,b* (dfloat ,b))
          (,ba (- ,b* ,a*)))
    (loop for ,i from 0 below ,n* ,(if collect 'collect 'do)
      (let ((,rn (dfloat (+ ,a* (* ,i (/ ,ba ,nn))))))
        (declare (double-float ,rn))
        (progn ,@body))))))


(defun linspace (n a b &key (end t))
  (declare (fixnum n))
  (declare (double-float a b))
  (declare (boolean end))
  (if (> n 1)
    (let ((ban (dfloat (/ (- b a) (if end (1- n) n)))))
      (declare (double-float ban))
      (loop for i from 0 below n
            collect (dfloat (+ a (* (dfloat i) ban)))))
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


(defun mean (a)
  (declare (list a))
  (/ (sum a) (length a)))


(defun copy-sort (a fx &key (key #'identity)
                       &aux (aa (copy-seq a)))
  (declare (list a))
  (sort aa fx :key key))


(defun percentiles (aa)
  (declare (list aa))
  (let ((n (length aa))
        (percentiles (list 0.05d0 0.1d0 0.5d0 0.9d0 0.95d0))
        (srt (make-generic-array :init (copy-sort aa #'>))))
    (to-array (append
      (list (aref srt 0))
      (loop for m in percentiles
          collect (aref srt (floor (* n m))))
      (list (array-last srt))))))

; TODO: expt, sqrt, ...


(defun range-search (ranges f &aux (n (1- (length ranges)))
                                   (ranges* (ensure-array ranges)))
  "
  binary range search.

  range must be sorted in ascending order. f is a value inside the range you
  are looking for.
  "
  (if (or (< f (aref ranges* 0)) (> f (aref ranges* n)))
    (error "querying position outside range: ~a" f))

  (loop with l = 0
        with r = n
        with mid = 0
        until (<= (aref ranges* mid) f
                  (aref ranges* (1+ mid)))
        do (setf mid (floor (+ l r) 2))
           (cond ((> f (aref ranges* mid))
                   (setf l (progn mid)))
                 ((< f (aref ranges* mid))
                   (setf r (1+ mid))))
        finally (return mid)))


