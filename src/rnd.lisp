
(in-package :rnd)

(defconstant PII  (* PI 2d0))
(declaim (type double-float PII))

(declaim (optimize (speed 3)))
;(declaim (optimize (safety 0)))

; MACROS


(defmacro -nrep (n &body body)
  (with-gensyms (nname)
    `(let ((,nname ,n))
      (loop repeat ,nname collect (progn ,@body)))))


(defmacro with-prob (p &body body)
  "
  executes body with probability p.
  "
  (with-gensyms (pname)
    `(let ((,pname ,p))
       (when (< (random 1d0) ,p)
         (list ,@body)))))


(defmacro prob (p a &optional b)
  `(if (< (rnd) ,p) ,a ,b))


(defmacro either (a b)
  `(prob 0.5d0 ,a ,b))


(defmacro rcond (&rest clauses)
  (with-gensyms (val)
    (let* ((tot 0d0)
           (clauses* (loop for (prob . body) in clauses
                           do (incf tot (coerce prob 'double-float))
                           collect `((< ,val ,tot) ,@body))))
    `(let ((,val (rnd ,tot)))
      (cond ,@clauses*)))))


(defmacro with-rndspace ((n a b rn &key collect) &body body)
  (declare (symbol rn))
  (with-gensyms (a* b* d)
    `(destructuring-bind (,a* ,b*)
      (sort (list (coerce ,a 'double-float) (coerce ,b 'double-float)) #'<)
      (let ((,d (- ,b* ,a*)))
        (loop repeat ,n ,(if collect 'collect 'do)
          (let ((,rn (+ ,a* (random ,d))))
            (declare (double-float ,rn))
            (progn ,@body)))))))


(declaim (ftype (function (&optional double-float) double-float) rnd rnd*))

(declaim (ftype (function (double-float double-float) double-float) rndbtwn))

(declaim (ftype (function (fixnum &optional fixnum) fixnum) rndi))


(defun set-rnd-state (i)
  (declare (fixnum i))
  (if (or #+SBCL t nil)
      (setf *random-state* (sb-ext:seed-random-state i))
      (warn "rnd:state is only implemented for SBCL. see src/rnd.lisp
             to implement state for your environment.")))


(defun make-rnd-state ()
  (setf *random-state* (make-random-state t)))


(defun -inc (x stp)
  (mod (+ x stp) 1d0))


; GENERIC

(defun rndget (l)
  (if (eql (type-of l) 'cons) (nth (random (length l)) l)
                              (aref l (random (length l)))))

(defun probsel (p a &aux (a* (ensure-array a)))
  (declare (double-float p))
  (loop with res = (make-generic-array)
        for i across a*
        do (prob p (array-push i res))
        finally (return res)))


; NUMBERS AND RANGES

; TODO: deprecate optional arg
(defun rndi (a &optional b &aux (b* (if b (the fixnum b) 0)))
  (declare (fixnum a b*))
  (if (not b) (random a)
      (+ a (the fixnum (random (the fixnum (- b* a)))))))


(defun nrndi (n a &optional b)
  (declare (fixnum n a))
  (loop repeat n collect (rndi a b)))


(defun rndi* (ab)
  (declare (list ab))
  (destructuring-bind (a b) ab
    (declare (fixnum a b))
    (+ a (random (- b a)))))


(defun nrndi* (n a)
  (declare (fixnum n a))
  (loop repeat n collect (rndi a)))


(defun rnd (&optional (x 1d0))
  (declare (double-float x))
  (random x))


(defun nrnd (n &optional (x 1d0))
  (declare (fixnum n))
  (declare (double-float x))
  (loop repeat n collect (rnd x)))


; TODO: nnorm, with-norm
(defun norm (&key (mu 0d0) (sigma 1d0))
  "
  box-muller transform
  "
  (declare (double-float mu sigma))
  (let ((s (* sigma (sqrt (* -2d0 (log (rnd))))))
        (u (* PII (rnd))))
    (declare (double-float s u))
    (values (+ mu (* s (cos u)))
            (+ mu (* s (sin u))))))


(defun rndbtwn (a b)
  (declare (double-float a b))
  (+ a (random (- b a))))


(defun nrndbtwn (n a b)
  (declare (fixnum n))
  (declare (double-float a b))
  (loop for i from 0 below n collect (rndbtwn a b)))


(defun rnd* (&optional (x 1d0))
  (declare (double-float x))
  (- x (* 2d0 (random x))))


(defun nrnd* (n &optional (x 1d0))
  (declare (fixnum n))
  (declare (double-float x))
  (loop repeat n collect (rnd* x)))


(defun rndspace (n a b &key order)
  (declare (fixnum n))
  (declare (double-float a b))
  (destructuring-bind (a b)
    (sort (list a b) #'<)
    (declare (double-float a b))
    (let ((d (- b a)))
      (declare (double-float d))
      (let ((res (-nrep n (+ a (random d)))))
        (if order (sort res #'<) res)))))


(defun rndspacei (n a b &key order)
  (declare (fixnum n a b))
  (destructuring-bind (a b)
    (sort (list a b) #'<)
    (declare (fixnum a b))
    (let ((d (- b a)))
      (declare (fixnum d))
      (let ((res (-nrep n (+ a (random d)))))
        (if order (sort res #'<) res)))))


(defun bernoulli (n p)
  (declare (fixnum n))
  (declare (double-float p))
  (loop repeat n collect (if (< (rnd:rnd) p) 1d0 0d0)))

