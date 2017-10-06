
(in-package :rnd)


(defun lget (l)
  (nth (random (length l)) l))


(defun aget (l)
  (aref l (random (length l))))


; NUMBERS AND RANGES


(defun rndi (a &optional b)
  (declare (integer a))
  ;(declare (type (or integer nil) b))
  (if (not b)
    (random a)
    (+ a (random (- (math:int b) a)))))


(defun nrndi (n a &optional b)
  (loop repeat n collect (rndi a b)))


(defun rndi* (ab)
  (declare (list ab))
  (destructuring-bind (a b)
    ab
    (declare (integer a b))
    (+ a (random (- b a)))))


(defun nrndi* (n ab)
  (loop repeat n collect (rndi ab)))


(defun rnd (&optional (x 1.0d0))
  (declare (double-float x))
  (random x))


(defun nrnd (n &optional (x 1.0d0))
  (declare (integer n))
  (declare (double-float x))
  (loop repeat n collect (rnd x)))


; TODO: nnorm, with-norm
(defun norm (&key (mu 0.0d0) (sigma 1d0))
  "
  box-muller transform
  "
  (declare (double-float mu sigma))
  (let ((s (* sigma (sqrt (* -2d0 (log (rnd))))))
        (u (* 2d0 pi (rnd))))
    (values
      (+ mu (* s (cos u)))
      (+ mu (* s (sin u))))))


(defun rndbtwn (a b)
  (declare (double-float a b))
  (+ a (random (- b a))))


(defun rnd* (&optional (x 1.0d0))
  (declare (double-float x))
  (- x (* 2.0d0 (random x))))


(defun nrnd* (n &optional (x 1.0d0))
  (declare (integer n))
  (declare (double-float x))
  (loop repeat n collect (rnd* x)))


(defmacro with-rndspace ((n a b rn) &body body)
  (with-gensyms (a* b* d)
    `(destructuring-bind (,a* ,b*)
      (sort (list (math:dfloat ,a) (math:dfloat ,b)) #'<)
      (let ((,d (- ,b* ,a*)))
        (loop repeat ,n do
          (let ((,rn (+ ,a* (random ,d))))
            (progn ,@body)))))))


(defun rndspace (n a b &key order)
  (declare (integer n))
  (declare (double-float a b))
  (destructuring-bind (a b)
    (sort (list a b) #'<)
      (let ((d (- b a)))
        (let ((res (math:nrep n (+ a (random d)))))
          (if order (sort res #'<) res)))))


(defun rndspacei (n a b &key order)
  (declare (integer n a b))
  (destructuring-bind (a b)
    (sort (list a b) #'<)
      (let ((d (- b a)))
        (let ((res (math:nrep n (+ a (random d)))))
          (if order (sort res #'<) res)))))


(defun bernoulli (n p)
  (declare (integer n))
  (declare (double-float p))
  (loop repeat n collect
    (if (< (rnd:rnd) p)
      1d0
      0d0)))


; SHAPES


(defun -add-if (a xy)
  (if xy (vec:add a xy) a))


(defun on-circ (rad &key xy)
  (-add-if (vec:scale (vec:cos-sin (random PII)) rad) xy))


(defun non-circ (n rad &key xy)
  (declare (integer n))
  (declare (double-float rad))
  (loop repeat n collect (on-circ rad :xy xy)))


(defmacro with-in-circ ((n rad v &key xy) &body body)
  (with-gensyms (rad* xy*)
    `(let ((,rad* ,rad)
           (,xy* ,xy))
      (loop repeat ,n do
        (let ((,v (in-circ ,rad* :xy ,xy*)))
          (progn ,@body))))))


(defun in-circ (rad &key xy)
  (declare (double-float rad))
  (-add-if
    (let ((a (random 1.0d0))
          (b (random 1.0d0)))
      (declare (double-float a b))
      (if (< a b)
        (vec:scale (vec:cos-sin (* PII (/ a b))) (* b rad))
        (vec:scale (vec:cos-sin (* PII (/ b a))) (* a rad))))
    xy))


(defun nin-circ (n rad &key xy)
  (declare (integer n))
  (declare (double-float rad))
  (loop repeat n collect (in-circ rad :xy xy)))


(defun in-box (sx sy &key xy)
  (declare (double-float sx sy))
  (-add-if (vec:vec (rnd* sx) (rnd* sy)) xy))


(defun nin-box (n sx sy &key xy)
  (declare (integer n))
  (declare (double-float sx sy))
  (loop repeat n collect (in-box sx sy :xy xy)))


(defmacro with-on-line ((n a b rn) &body body)
  (with-gensyms (sub a*)
    `(let* ((,a* ,a)
            (,sub (vec:sub ,b ,a*)))
      (loop repeat ,n do
        (let ((,rn (vec:add ,a* (vec:scale ,sub (random 1d0)))))
          (progn ,@body))))))


(defun on-line (a b)
  (declare (vec:vec a b))
  (vec:add a (vec:scale (vec:sub b a) (random 1.0d0))))


(defun non-line (n a b)
  (declare (integer n))
  (declare (vec:vec a b))
  (loop repeat n collect (on-line a b)))


; WALKERS

(defun get-lin-stp (&optional (init 0.0d0))
  "
  random linear walker limited to (0 1)
  "
  (let ((x (math:dfloat init)))
    (lambda (stp)
      (setf x (math:inc x (rnd* stp))))))


(defun get-lin-stp* (&optional (init 0.0d0))
  "
  random linear walker
  "
  (let ((x (math:dfloat init)))
    (lambda (stp)
      (incf x (rnd* stp)))))


(defun get-acc-lin-stp (&optional (init-x 0.0d0) (init-a 0.0d0))
  "
  random accelerated linear walker limited to (0 1)
  "
  (let ((a (math:dfloat init-a))
        (x (math:dfloat init-x)))
    (lambda (s)
      (setf x (math:inc x (incf a (rnd* s)))))))


(defun get-acc-lin-stp* (&optional (init-x 0.0d0) (init-a 0.0d0))
  "
  random accelerated linear walker
  "
  (let ((a (math:dfloat init-a))
        (x (math:dfloat init-x)))
    (lambda (s)
      (incf x (incf a (rnd* s))))))


(defun get-circ-stp* (&optional (init (vec:zero)))
  (let ((xy (vec:copy init)))
    (lambda (stp)
      (setf xy (vec:add xy (in-circ stp))))))


(defun get-acc-circ-stp* (&optional (init (vec:zero))
                                    (init-a (vec:zero)))
  (let ((a (vec:copy init-a))
        (xy (vec:copy init)))
    (lambda (stp)
      (setf xy (vec:add xy (setf a (vec:add a (in-circ stp))))))))

