
(in-package :color)



(defun white (&optional (alpha 1d0))
  (list 1d0 1d0 1d0 (math:dfloat alpha)))


(defun black (&optional (alpha 1d0))
  (list 0.0d0 0.0d0 0.0d0 (math:dfloat alpha)))


(defun mdark (&optional (alpha 1d0))
  (list 0.3d0 0.3d0 0.3d0 (math:dfloat alpha)))


(defun dark (&optional (alpha 1d0))
  (list 0.2d0 0.2d0 0.2d0 (math:dfloat alpha)))


(defun vdark (&optional (alpha 1d0))
  (list 0.1d0 0.1d0 0.1d0 (math:dfloat alpha)))


(defun gray (v &optional (alpha 1d0))
  (let ((v* (math:dfloat v)))
    (list v* v* v* (math:dfloat alpha))))


(defun transparent ()
  (list 1d0 1d0 1d0 0d0))


(defun rgb (r g b &optional (alpha 1d0))
  (math:dfloat* (list r g b alpha)))


(defun cmyk (c m y k &optional (alpha 1d0))
  (let ((ik (- 1d0 (math:dfloat k))))
    (list
      (* (- 1d0 (math:dfloat c)) ik)
      (* (- 1d0 (math:dfloat m)) ik)
      (* (- 1d0 (math:dfloat y)) ik)
      (math:dfloat alpha))))


(defun hsv (h s v &optional (alpha 1d0))
  (destructuring-bind (h s v alpha)
    (math:dfloat* (list h s v alpha))
    (let ((c (* v s)))
      (let ((x (* c (- 1d0 (abs (- (mod (* 6d0 h) 2d0) 1d0)))))
            (m (- v c)))
        (append
          (math:add
            (list m m m)
            (case (floor (mod (* h 6d0) 6d0))
              (0 (list c x 0d0))
              (1 (list x c 0d0))
              (2 (list 0d0 c x))
              (3 (list 0d0 x c))
              (4 (list x 0d0 c))
              (5 (list c 0d0 x))))
          (list alpha))))))

