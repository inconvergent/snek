
(defpackage :color
  (:use :common-lisp)
  (:export
    :black
    :dark
    :gray
    :hsv-to-rgb
    :rgb
    :rgba
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


(defmacro rgb (rgb &optional (alpha 1.0d0))
  `(append (to-dfloat* ,rgb) (list (to-dfloat ,alpha))))


(defmacro rgba (rgba)
  `(to-dfloat* ,rgba))


(defun hsv-to-rgb (hsv &optional (alpha 1.0d0))
  (destructuring-bind (h s v)
    (to-dfloat* hsv)
    (let ((c (* v s)))
      (let ((x (* c (- 1.0d0 (abs (- (mod (* 6.0d0 h) 2.0d0) 1.0d0)))))
            (m (- v c)))

        (append
          (add
            (list m m m)
            (cond ((<= h (/ 1.0d0 6.0d0))
                    (list c x 0.0d0))

                  ((<= h (/ 1.0d0 3.0d0))
                    (list x c 0.0d0))

                  ((<= h 0.5d0)
                    (list 0.0d0 c x))

                  ((<= h (/ 2.0d0 3.0d0))
                    (list 0.0d0 x c))

                  ((<= h (/ 5.0d0 6.0d0))
                    (list x 0.0d0 c))

                  (t
                    (list c 0.0d0 x))))
          (list (to-dfloat alpha)))))))

