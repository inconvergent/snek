
(defpackage :sandpaint
  (:use :common-lisp)
  (:export
    :chromatic-aberration
    :make
    :pixel-hack
    :strokes
    :path
    :pix
    :pix*
    :save
    :set-rgba)
  (:import-from :common-lisp-user
    :2d-square-loop
    :rnd-in-circ
    :rnd-on-line
    :add
    :iscale
    :lround
    :sub
    :to-dfloat
    :with-struct))


(ql:quickload "ZPNG")

(in-package :sandpaint)


(defun make-rgba-array (size)
  (make-array
    (list size size 4)
    :adjustable nil
    :initial-element 0.0d0
    :element-type 'double-float))


(defun -scale-convert (v &key (s 1.0d0) (gamma 1.0d0))
  (setf v (expt (/ v s) gamma)))


(defun -unsigned-256 (v)
  (cond
    ((> v 1.0d0) 255)
    ((< v 0.0d0) 0)
    (t (round (* 255 v)))))


(defun -setf-operator-over (vals x y i -alpha color)
  (setf
    (aref vals x y i)
    (+ (* (aref vals x y i) -alpha) color)))

(defun -operator-over (size vals x y r g b a)
  (if (and (>= x 0) (< x size) (>= y 0) (< y size))
    (let ((ia (- 1.0 a)))
      (-setf-operator-over vals x y 0 ia r)
      (-setf-operator-over vals x y 1 ia g)
      (-setf-operator-over vals x y 2 ia b)
      (-setf-operator-over vals x y 3 ia a))))


(defun -draw-stroke (vals size grains v1 v2 r g b a)
  (loop for i from 1 to grains
    do
      (destructuring-bind (x y)
        (lround (rnd-on-line v1 v2))
        (-operator-over size vals x y r g b a))))


(defun copy-rgba-array-to-from (target source size)
  (2d-square-loop (x y size)
    (loop for i from 0 to 3 do
      (setf (aref target x y i) (aref source x y i)))))



(defstruct sandpaint
  (vals nil :read-only nil)
  (size -1 :type integer :read-only nil)
  (r 0.0d0 :type double-float :read-only nil)
  (g 0.0d0 :type double-float :read-only nil)
  (b 0.0d0 :type double-float :read-only nil)
  (a 1.0d0 :type double-float :read-only nil))


; TODO implement wrapper
;(defun -sandpaint-vert-box (vals size mx my grains w h r g b a)
;  (loop for i from 0 to grains do
;    (destructuring-bind (x y)
;      (mapcar #'round (rnd-in-box w h :x mx :y my))
;      (-operator-over size vals x y r g b a))))

(defun -draw-pix (vals size x y r g b a)
  (-operator-over size vals x y r g b a))


(defun -offset-rgba (new-vals old-vals size x y nxy i)
  (destructuring-bind (nx ny)
    (mapcar #'round nxy)
    (if (and (>= nx 0) (< nx size) (>= ny 0) (< ny size))
      (setf (aref new-vals nx ny i) (aref old-vals x y i)))))

(defun chromatic-aberration (sand C s)
  (with-struct (sandpaint- size vals) sand
    (let ((new-vals (make-rgba-array size)))
      (copy-rgba-array-to-from new-vals vals size)

      (2d-square-loop (x y size)
        (let* ((xy (list x y))
               (dx (iscale
                     (sub
                       (add (rnd-in-circ 1.0) xy)
                       C)
                     s)))
          (-offset-rgba new-vals vals size x y (add xy dx) 0)
          (-offset-rgba new-vals vals size x y (sub xy dx) 2)))

      (setf (sandpaint-vals sand) new-vals))))


(defun -png-tuple (vals x y gamma)
  (let ((a (aref vals x y 3)))
    (list
      (-unsigned-256 (-scale-convert (aref vals x y 0) :s a :gamma gamma))
      (-unsigned-256 (-scale-convert (aref vals x y 1) :s a :gamma gamma))
      (-unsigned-256 (-scale-convert (aref vals x y 2) :s a :gamma gamma))
      (-unsigned-256 (-scale-convert a :gamma gamma)))))


(defun make
    (size
     &key
       (active '(0.0d0 0.0d0 0.0d0 1.0d0))
       (bg '(1.0d0 1.0d0 1.0d0 1.0d0)))
  (destructuring-bind (ar ag ab aa br bg bb ba)
    (mapcar
      (lambda (x) (to-dfloat x))
      (append active bg))

    (let ((vals (make-rgba-array size)))
      (2d-square-loop (x y size)
        (setf (aref vals x y 0) (* ba br))
        (setf (aref vals x y 1) (* ba bg))
        (setf (aref vals x y 2) (* ba bb))
        (setf (aref vals x y 3) ba))

      (make-sandpaint
        :size size
        :r (* ar aa)
        :g (* ag aa)
        :b (* ab aa)
        :a aa
        :vals vals))))


(defun set-rgba (sand rgba)
  (destructuring-bind (r g b a)
    (mapcar (lambda (x) (to-dfloat x)) rgba)
      (setf (sandpaint-r sand) (* r a))
      (setf (sandpaint-g sand) (* g a))
      (setf (sandpaint-b sand) (* b a))
      (setf (sandpaint-a sand) a)))


(defun pixel-hack (sand)
  (let ((vals (sandpaint-vals sand)))
    (setf (aref vals 0 0 3) 0.5d0)))


(defun pix (sand vv)
  (with-struct (sandpaint- size vals r g b a) sand
    (loop for v in vv do
      (destructuring-bind (x y)
        v
        (-draw-pix vals size (round x) (round y) r g b a)))))


(defun pix* (sand vv n)
  (with-struct (sandpaint- size vals r g b a) sand
    (loop for i from 0 below n do
      (-draw-pix vals size
                 (round (aref vv i 0))
                 (round (aref vv i 1))
                 r g b a))))


(defun strokes (sand lines grains)
  (with-struct (sandpaint- size vals r g b a) sand
    (loop for line in lines do
      (destructuring-bind (u v)
        line
        (-draw-stroke vals size grains u v r g b a)))))


(defun path (sand path grains)
  (with-struct (sandpaint- size vals r g b a) sand
    (loop
      for u in path
      for w in (cdr path)
      do
        (-draw-stroke vals size grains u w r g b a))))


(defun save (sand name &key (gamma 1.0))
  (if (not name) (error "missing result file name."))
  (with-struct (sandpaint- size vals) sand
    (let ((png
             (make-instance
               'zpng::pixel-streamed-png
               :color-type :truecolor-alpha
               :width size
               :height size)))

      (with-open-file
        (stream name
          :direction :output
          :if-exists :supersede
          :if-does-not-exist :create
          :element-type '(unsigned-byte 8))
        (zpng:start-png png stream)
        (2d-square-loop (x y size)
          (zpng:write-pixel (-png-tuple vals y x gamma) png))
        (zpng:finish-png png)))))

