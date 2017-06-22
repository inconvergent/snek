
(defpackage :color
  (:use :common-lisp)
  (:export
    :black
    :dark
    :gray
    :hsv
    :mdark
    :rgb
    :transparent
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


(defun white (&optional (alpha 1.0d0))
  (list 1.0d0 1.0d0 1.0d0 (to-dfloat alpha)))


(defun black (&optional (alpha 1.0d0))
  (list 0.0d0 0.0d0 0.0d0 (to-dfloat alpha)))


(defun mdark (&optional (alpha 1.0d0))
  (list 0.3d0 0.3d0 0.3d0 (to-dfloat alpha)))


(defun dark (&optional (alpha 1.0d0))
  (list 0.2d0 0.2d0 0.2d0 (to-dfloat alpha)))


(defun vdark (&optional (alpha 1.0d0))
  (list 0.1d0 0.1d0 0.1d0 (to-dfloat alpha)))


(defun gray (v &optional (alpha 1.0d0))
  (let ((v* (to-dfloat v)))
    (list v* v* v* (to-dfloat alpha))))


(defun transparent ()
  (list 1.0d0 1.0d0 1.0d0 0.0d0))


(defun rgb (r g b &optional (alpha 1.0d0))
  (to-dfloat* (list r g b alpha)))


(defun hsv (h s v &optional (alpha 1.0d0))
  (destructuring-bind (h s v alpha)
    (to-dfloat* (list h s v alpha))
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
          (list alpha))))))

