
(in-package :pigment)

"""
Colors are stored internally with premultiplied alpha.
Package was renamed from 'color' because of a package name collision.
"""

(defmacro with ((c r g b a) &body body)
  (with-gensyms (c*)
    `(let* ((,c* ,c)
            (,r (rgba-r ,c*))
            (,g (rgba-g ,c*))
            (,b (rgba-b ,c*))
            (,a (rgba-a ,c*)))
      (declare (double-float ,r ,g ,b ,a))
      (progn ,@body))))


(defstruct (rgba (:constructor -make-rgba))
  (r 0d0 :type double-float :read-only t)
  (g 0d0 :type double-float :read-only t)
  (b 0d0 :type double-float :read-only t)
  (a 1d0 :type double-float :read-only t))


(defun make-rgba (r g b &optional (a 1d0)
                        &aux (r* (math:dfloat r))
                             (g* (math:dfloat g))
                             (b* (math:dfloat b))
                             (a* (math:dfloat a)))
  (-make-rgba :r (* a* r*) :g (* a* g*) :b (* a* b*) :a a*))


(defun show (c)
  (declare (rgba c))
  (let ((a (rgba-a c)))
    (format nil "(r ~A g ~A b ~A a ~A" (/ (rgba-r c) a) (/ (rgba-g c) a)
                                       (/ (rgba-b c) a) a))
  c)


(defun to-list (c)
  (declare (rgba c))
  (let ((a (rgba-a c)))
    (list (/ (rgba-r c) a) (/ (rgba-g c) a) (/ (rgba-b c) a) a)))


(defun to-list* (c)
  (declare (rgba c))
  (list (rgba-r c) (rgba-g c) (rgba-b c) (rgba-a c)))


(defun white (&optional (a 1d0))
  (make-rgba 1d0 1d0 1d0 a))


(defun black (&optional (a 1d0))
  (make-rgba 0.0d0 0.0d0 0.0d0 a))


(defun mdark (&optional (a 1d0))
  (make-rgba 0.3d0 0.3d0 0.3d0 a))


(defun dark (&optional (a 1d0))
  (make-rgba 0.2d0 0.2d0 0.2d0 a))


(defun vdark (&optional (a 1d0))
  (make-rgba 0.1d0 0.1d0 0.1d0 a))


(defun gray (v &optional (a 1d0))
  (make-rgba v v v a))


(defun transparent (&optional (v 1d0))
  (make-rgba v v v 0d0))


(defun rgb (r g b &optional (a 1d0))
  (make-rgba r g b a))


(defun -hex (d)
  (declare (double-float d))
  (let ((res (format nil "~X" (min 255 (max 0 (floor (* d 256)))))))
    (if (< (length res) 2) (concatenate 'string "0" res) res)))

(defun to-hex (c)
  (declare (rgba c))
  (destructuring-bind (r g b a) (to-list c)
    (values (apply #'concatenate 'string (list "#" (-hex r) (-hex g) (-hex b)))
            a)))


(defun cmyk (c m y k &optional (a 1d0))
  (let ((ik (- 1d0 (math:dfloat k))))
    (make-rgba (* (- 1d0 (math:dfloat c)) ik) (* (- 1d0 (math:dfloat m)) ik)
               (* (- 1d0 (math:dfloat y)) ik) a)))


(defun hsv (h s v &optional (a 1d0))
  (destructuring-bind (h s v)
    (math:dfloat* (list h s v))
    (let* ((c (* v s))
           (x (* c (- 1d0 (abs (- (mod (* 6d0 h) 2d0) 1d0)))))
           (m (- v c)))
      (destructuring-bind (r g b)
        (math:dadd (list m m m)
                   (case (floor (mod (* h 6d0) 6d0))
                         (0 (list c x 0d0))
                         (1 (list x c 0d0))
                         (2 (list 0d0 c x))
                         (3 (list 0d0 x c))
                         (4 (list x 0d0 c))
                         (5 (list c 0d0 x))))
        (make-rgba r g b a)))))

