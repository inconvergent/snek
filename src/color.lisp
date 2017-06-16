
(defpackage :color
  (:use :common-lisp)
  (:export
    :black
    :dark
    :gray
    :hsv
    :rgb
    :vdark
    :white)
  (:import-from :common-lisp-user
    :add
    :scale
    :sub
    :to-dfloat
    :to-dfloat*
    :to-int
    :with-gensyms))

(in-package :color)


(defmacro white (&optional (alpha 1.0d0))
  `(list 1.0d0 1.0d0 1.0d0 (to-dfloat ,alpha)))


(defmacro black (&optional (alpha 1.0d0))
  `(list 0.0d0 0.0d0 0.0d0 (to-dfloat ,alpha)))


(defmacro dark (&optional (alpha 1.0d0))
  `(list 0.2d0 0.2d0 0.2d0 (to-dfloat ,alpha)))


(defmacro vdark (&optional (alpha 1.0d0))
  `(list 0.1d0 0.1d0 0.1d0 (to-dfloat ,alpha)))


(defmacro gray (v &optional (alpha 1.0d0))
  `(list ,v ,v ,v (to-dfloat ,alpha)))


(defmacro rgb (r g b &optional (a 1.0d0))
  `(to-dfloat* (list ,r ,g ,b ,a)))


(defun hsv (h s v &optional (a 1.0d0))
  (destructuring-bind (h s v a)
    (to-dfloat* (list h s v a))
    (let ((c (* v s)))
      (let ((x (* c (- 1.0d0 (abs (- (mod (* 6.0d0 h) 2.0d0) 1.0d0)))))
            (m (- v c)))
        (append
          (add
            (list m m m)
            (case (floor (mod (* h 6.0d0) 6.0d0))
              (0 (list c x 0.0d0))
              (1 (list x c 0.0d0))
              (2 (list 0.0d0 c x))
              (3 (list 0.0d0 x c))
              (4 (list x 0.0d0 c))
              (5 (list c 0.0d0 x))))
          (list a))))))

