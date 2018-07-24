
(in-package :math)

(declaim (ftype (function (number) double-float) dfloat))
(declaim (ftype (function (number) fixnum) int))
(declaim (ftype (function (number) float) sfloat))


; TYPES

(defun int (x)
  (declare (number x))
  (the fixnum (coerce (floor x) 'fixnum)))


(defun int* (xx)
  (declare (list xx))
  (mapcar (lambda (x) (int (the fixnum (floor x)))) xx))


(defun sfloat (x)
  (declare (number x))
  (the float (coerce x 'float)))


(defun sfloat* (xx)
  (declare (list xx))
  (mapcar (lambda (x) (sfloat x)) xx))


(defun dfloat (x)
  (declare (number x))
  (the double-float (coerce x 'double-float)))


(defun dfloat* (xx)
  (declare (list xx))
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
  (loop with arr = (to-vector l)
        for i of-type fixnum in ii collect (aref arr i)))


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
  (declare (fixnum n) (double-float a b) (boolean end))
  (if (> n 1)
    (let ((ban (dfloat (/ (- b a) (if end (dfloat (1- n)) (dfloat n))))))
      (declare (double-float ban))
      (loop for i of-type fixnum from 0 below n
            collect (the double-float
                         (+ a (the double-float (* (dfloat i) ban))))))
    (list a)))


; LIST INTEGER MATH


(defun add (aa bb)
  (declare (list aa bb))
  (loop for a of-type fixnum in aa and b of-type fixnum in bb
        collect (the fixnum (+ a b))))

(defun sub (aa bb)
  (declare (list aa bb))
  (loop for a of-type fixnum in aa and b of-type fixnum in bb
        collect (the fixnum (- a b))))

(defun mult (aa bb)
  (declare (list aa bb))
  (loop for a of-type fixnum in aa and b of-type fixnum in bb
        collect (the fixnum (* a b))))

(defun mod- (i n)
  (declare (fixnum i n))
  (mod (the fixnum (+ n i -1)) n))

(defun mod+ (i n)
  (declare (fixnum i n))
  (mod (the fixnum (+ n i 1)) n))

(defun mod2 (i)
  (declare (fixnum i))
  (mod i 2))


; LIST DOUBLE FLOAT MATH


(defun dadd (aa bb)
  (declare (list aa bb))
  (loop for a of-type double-float in aa
        and b of-type double-float in bb
        collect (+ a b) of-type double-float))

(defun dsub (aa bb)
  (declare (list aa bb))
  (loop for a of-type double-float in aa
        and b of-type double-float in bb
        collect (- a b) of-type double-float))

(defun dmult (aa bb)
  (declare (list aa bb))
  (loop for a of-type double-float in aa
        and b of-type double-float in bb
        collect (* a b) of-type double-float))

(defun daddmod* (aa b &optional (m 1d0))
  (declare (list aa) (double-float b m))
  (mapcar (lambda (a) (declare (double-float a))
            (the double-float (mod (the double-float (+ a b)) m))) aa))

(defun ddst (aa bb)
  (declare (list aa bb))
  (sqrt (the double-float (loop for a in aa and b in bb
                                sum (expt (the double-float (- a b)) 2d0)
                                  of-type double-float ))))

(defun ddiv (aa bb)
  (declare (list aa bb))
  (loop for a of-type double-float in aa
        and b of-type double-float in bb
        collect (/ a b) of-type double-float))

(defun dscale (aa bb)
  (declare (list aa bb))
  (mapcar (lambda (a b) (declare (double-float a b)) (* a b)) aa bb))

(defun dscale* (aa s)
  (declare (list aa) (double-float s))
  (mapcar (lambda (a) (declare (double-float a)) (* a s)) aa))

(defun dsum (aa)
  (declare (list aa))
  (loop for a of-type double-float in aa summing a of-type double-float))

(defun dmean (aa)
  (declare (list aa))
  (/ (dsum aa) (math:dfloat (length aa))))

(defun inc (x stp)
  (declare (double-float x stp))
  (mod (the double-float (+ x stp)) 1d0))


; OTHER


(defun copy-sort (a fx &key (key #'identity))
  (declare (list a))
  (sort (copy-seq a) fx :key key))


(defun percentiles (aa)
  (declare (list aa))
  (let ((n (length aa))
        (percentiles (list 0.05d0 0.1d0 0.5d0 0.9d0 0.95d0))
        (srt (make-adjustable-vector :init (copy-sort aa #'>))))
    (to-vector (append
      (list (aref srt 0))
      (loop for m in percentiles collect (aref srt (floor (* n m))))
      (list (vector-last srt))))))

; TODO: expt, sqrt, ...


(defun range-search (ranges f &aux (n (1- (length ranges)))
                                   (ranges* (ensure-vector ranges)))
  "
  binary range search.

  range must be sorted in ascending order. f is a value inside the range you
  are looking for.
  "
  (if (or (< f (aref ranges* 0)) (> f (aref ranges* n)))
    (error "querying position outside range: ~a" f))

  (loop with l of-type fixnum = 0
        with r of-type fixnum = n
        with m of-type fixnum = 0
        until (<= (aref ranges* m) f (aref ranges* (1+ m)))
        do (setf m (floor (+ l r) 2))
           (cond ((> f (aref ranges* m))
                   (setf l (progn m)))
                 ((< f (aref ranges* m))
                   (setf r (1+ m))))
        finally (return m)))


