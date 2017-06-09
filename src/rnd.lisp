
; GET

(defpackage :rnd
  (:use :common-lisp)
  (:export
    :aget
    :get-acc-circ-stp*
    :get-acc-lin-stp
    :get-acc-lin-stp*
    :get-circ-stp*
    :get-lin-stp
    :get-lin-stp*
    :in-box
    :in-circ
    :rndi
    :lget
    :mixed
    :on-circ
    :on-line
    :on-spiral
    :rnd
    :rnd*
    :rndspace
    :rndspacei
    )
  (:import-from :common-lisp-user
    :add
    :cos-sin
    :inc
    :nrep
    :scale
    :sub
    :to-dfloat
    :to-dfloat*
    :to-int
    :with-gensyms
    ))


(in-package :rnd)


(defun lget (l)
  (nth (random (length l)) l))


(defun aget (l)
  (aref l (random (length l))))


; NUMBERS AND RANGES


(defun rndi (a &optional b)
  (let ((a (to-int a)))
    (if (not b)
      (random a)
      (+ a (random (- (to-int b) a))))))


(defun rnd (&optional (x 1.0d0))
  (random (to-dfloat x)))



(defun rnd* (&optional (x 1.0d0))
  (- x (* 2.0d0 (random (to-dfloat x)))))


(defun mixed (x f)
  (let ((x* (to-dfloat x)))
    (+
      (random (* (to-dfloat f) x*))
      (- x* (* 2.0d0 (random x*))))))


(defun rndspace (a b n &key order)
  (destructuring-bind (a b)
    (sort (list a b) #'<)
      (let ((d (to-dfloat (- b a))))
        (let ((res (nrep n (+ a (random d)))))
          (if order (sort res #'<) res)))))


(defun rndspacei (a b n &key order)
  (destructuring-bind (a b)
    (sort (list a b) #'<)
      (let ((d (to-int (- b a))))
        (let ((res (nrep n (+ a (random d)))))
          (if order (sort res #'<) res)))))


; SHAPES


(defun on-circ (rad &key (xy (list 0.0d0 0.0d0)))
  (add
    xy
    (scale
      (cos-sin (random (* PI 2.0d0)))
      rad)))


(defun in-circ (rad &key (xy (list 0.0d0 0.0d0)))
  (let ((ab (sort (list (random 1.0d0) (random 1.0d0)) #'<)))
    (add
      xy
      (scale
        (cos-sin (* 2 PI (apply #'/ ab)))
        (* (second ab) rad)))))


(defun in-box (sx sy &key (xy (list 0.0d0 0.0d0)))
  (add
    (list (rnd* (to-dfloat sx))
          (rnd* (to-dfloat sy)))
    xy))


(defun on-line (x1 x2)
  (add
    x1
    (scale (sub x2 x1) (random 1.0d0))))


(defun on-spiral (rad &key (xy (list 0.0d0 0.0d0)) (rot 1.0d0))
  (let ((i (random 1.0)))
    (add
      xy
      (scale
        (cos-sin (* i (* PI rot)))
        (* i rad)))))


; WALKERS


(defmacro get-lin-stp (&optional (init 0.0d0))
  (with-gensyms (x stp)
    `(let ((,x (to-dfloat ,init)))
      (lambda (,stp)
        (setf ,x (inc ,x (rnd* ,stp)))))))


(defmacro get-lin-stp* (&optional (init 0.0d0))
  (with-gensyms (x stp)
    `(let ((,x (to-dfloat ,init)))
      (lambda (,stp)
        (incf ,x (rnd* ,stp))))))


(defmacro get-acc-lin-stp (&optional (init-x 0.0d0) (init-a 0.0d0))
  (with-gensyms (x a s)
    `(let ((,a (to-dfloat ,init-a))
           (,x (to-dfloat ,init-x)))
      (lambda (,s)
        (setf ,x (inc ,x (incf ,a (rnd* ,s))))))))


(defmacro get-acc-lin-stp* (&optional (init-x 0.0d0) (init-a 0.0d0))
  (with-gensyms (x a s)
    `(let ((,a (to-dfloat ,init-a))
           (,x (to-dfloat ,init-x)))
      (lambda (,s)
        (incf ,x (incf ,a (rnd* ,s)))))))


(defmacro get-circ-stp* (&optional (init '(list 0.0d0 0.0d0)))
  (with-gensyms (xy stp)
    `(let ((,xy (to-dfloat* ,init)))
      (lambda (,stp)
        (setf ,xy (add ,xy (in-circ ,stp)))))))


(defmacro get-acc-circ-stp* (&optional (init '(list 0.0d0 0.0d0))
                                       (init-a '(list 0.0d0 0.0d0)))
  (with-gensyms (xy stp a)
    `(let ((,a (to-dfloat* ,init-a))
           (,xy (to-dfloat* ,init)))
      (lambda (,stp)
        (setf ,xy (add ,xy (setf ,a (add ,a (in-circ ,stp)))))))))

