
(in-package :sandpaint)


(defun make-rgba-array (size)
  (make-array (list size size 4)
              :adjustable nil
              :initial-element 0d0
              :element-type 'double-float))


(defun -scale-convert (v &key (s 1d0) (gamma 1d0))
  (declare (double-float v s gamma))
  (setf v (expt (max 0d0 (/ v s)) gamma)))


(defun -unsigned-256 (v)
  (declare (double-float v))
  (cond ((> v 1d0) 255)
        ((< v 0d0) 0)
        (t (round (* 255 v)))))


(defun -setf-operator-over (vals x y i a color)
  (declare (integer x y i))
  (declare (double-float a color))
  (declare (type (array double-float) vals))
  (setf (aref vals x y i)
        (+ (* (aref vals x y i) a) color)))


(defun -operator-over (vals x y rgba)
  (declare (integer x y))
  (declare (color:rgba rgba))
  (declare (type (array double-float) vals))
  (color:with (rgba r g b a)
    (let ((ia (- 1d0 a)))
      (declare (double-float ia))
      (-setf-operator-over vals x y 0 ia r)
      (-setf-operator-over vals x y 1 ia g)
      (-setf-operator-over vals x y 2 ia b)
      (-setf-operator-over vals x y 3 ia a))))


(defun -draw-stroke (vals size grains v1 v2 rgba)
  (declare (integer grains))
  (declare (color:rgba rgba))
  (declare (type (array double-float) vals))
  (rnd:with-on-line (grains v1 v2 rn)
    (vec:inside* (size rn x y)
      (-operator-over vals x y rgba))))


(defun -draw-dens-stroke (vals size dens v1 v2 rgba)
  (declare (double-float dens))
  (declare (color:rgba rgba))
  (declare (type (array double-float) vals))
  (rnd:with-on-line ((ceiling (* dens (vec:dst v1 v2))) v1 v2 rn)
    (vec:inside* (size rn x y)
      (-operator-over vals x y rgba))))


(defun copy-rgba-array-to-from (target source size)
  (square-loop (x y size)
    (setf (aref target x y 0) (aref source x y 0)
          (aref target x y 1) (aref source x y 1)
          (aref target x y 2) (aref source x y 2)
          (aref target x y 3) (aref source x y 3))))


(defstruct sandpaint
  (size nil :type integer :read-only t)
  (vals nil  :type array :read-only nil)
  (rgba nil :type color:rgba :read-only nil))


; TODO: with-in-circ
(defun -draw-circ (vals size xy rad grains rgba)
  (declare (integer grains))
  (declare (double-float rad))
  (declare (color:rgba rgba))
  (declare (type (array double-float) vals))
  (loop repeat grains do
    (vec:inside* (size (vec:add xy (rnd:in-circ rad)) x y)
      (-operator-over vals x y rgba))))


(defun -offset-rgba (new-vals old-vals size x y nxy i)
  (declare (type (array double-float) new-vals old-vals))
  (let ((rx (round (vec::vec-x nxy)))
        (ry (round (vec::vec-y nxy))))
    (if (and (>= rx 0) (< rx size)
             (>= ry 0) (< ry size))
        (setf (aref new-vals rx ry i)
              (aref old-vals x y i)))))


(defun chromatic-aberration (sand center &key (s 1d0) (noise 1d0))
  (declare (double-float s noise))
  (with-struct (sandpaint- size vals) sand
    (declare (type (array double-float) vals))
    (let ((new-vals (make-rgba-array size)))
      (declare (type (array double-float) new-vals))
      (copy-rgba-array-to-from new-vals vals size)

      (square-loop (x y size)
        (let* ((xy (vec:vec-coerce x y))
               (dx (vec:iscale
                     (vec:sub (vec:add (rnd:in-circ noise) xy) center)
                     s)))
          (-offset-rgba new-vals vals size x y (vec:add xy dx) 0)
          (-offset-rgba new-vals vals size x y (vec:sub xy dx) 2)))

      (setf (sandpaint-vals sand) new-vals))))


(defun -png-tuple (vals x y gamma)
  (declare (integer x y))
  (declare (double-float gamma))
  (declare (type (array double-float) vals))
  (let ((a (aref vals x y 3)))
    (declare (double-float a))
    (if (> a 0.0d0)
      (list
        (-unsigned-256 (-scale-convert (aref vals x y 0) :s a :gamma gamma))
        (-unsigned-256 (-scale-convert (aref vals x y 1) :s a :gamma gamma))
        (-unsigned-256 (-scale-convert (aref vals x y 2) :s a :gamma gamma))
        (-unsigned-256 (-scale-convert a :gamma gamma)))
      (list 0 0 0 0))))


(defun clear (sand rgba)
  (declare (color:rgba rgba))
  (color:with (rgba r g b a)
    (with-struct (sandpaint- size vals) sand
        (declare (type (array double-float) vals))
        (square-loop (x y size)
          (setf (aref vals x y 0) r
                (aref vals x y 1) g
                (aref vals x y 2) b
                (aref vals x y 3) a)))))

(defun make (size &key (fg (color:rgb 0.0d0 0.0d0 0.0d0))
                       (bg (color:rgb 1.0d0 1.0d0 1.0d0)))

  (color:with (bg r g b a)
    (let ((vals (make-rgba-array size)))
      (declare (type (array double-float) vals))
      (square-loop (x y size)
        (setf (aref vals x y 0) r
              (aref vals x y 1) g
              (aref vals x y 2) b
              (aref vals x y 3) a))
      (make-sandpaint :size size :rgba fg :vals vals))))


(defun set-fg-color (sand rgba)
  (declare (color:rgba rgba))
  (setf (sandpaint-rgba sand) rgba))


(defun pixel-hack (sand &optional (sa 0.9d0))
  "
  scale opacity of pix (0 0) by sa.
  "
  (let ((vals (sandpaint-vals sand)))
    (declare (type (array double-float) vals))
    (destructuring-bind (r g b a)
      (loop for i from 0 below 4 collect (aref vals 0 0 i))
      (declare (double-float r g b a))
      (if (>= a 1.0d0)
        (let ((na (* a (math:dfloat sa))))
          (setf (aref vals 0 0 0) (* (/ r a) na)
                (aref vals 0 0 1) (* (/ g a) na)
                (aref vals 0 0 2) (* (/ b a) na)
                (aref vals 0 0 3) na))))))


(defun pix (sand vv)
  (declare (list vv))
  (with-struct (sandpaint- size vals rgba) sand
    (declare (type (array double-float) vals))
    (loop for v of-type vec:vec in vv do
      (vec:inside* (size v x y)
        (-operator-over vals x y rgba)))))


(defun arr-pix (sand vv n)
  (declare (type (array double-float) vv))
  (declare (integer n))
  (with-struct (sandpaint- size vals rgba) sand
    (declare (type (array double-float) vals))
    (loop for i of-type integer from 0 below n do
      (vec:inside* (size (vec:arr-get vv i) x y)
        (-operator-over vals x y rgba)))))


(defun circ (sand vv rad n)
  (declare (list vv))
  (declare (double-float rad))
  (declare (integer n))
  (with-struct (sandpaint- size vals rgba) sand
    (declare (type (array double-float) vals))
    (loop for v of-type vec:vec in vv do
      (-draw-circ vals size v rad n rgba))))


(defun bzspl-stroke (sand bz n)
  (declare (integer n))
  (with-struct (sandpaint- size vals rgba) sand
    (bzspl:with-rndpos (bz n v)
      (vec:inside* (size v x y)
        (-operator-over vals x y rgba)))))


(defun arr-circ (sand vv num rad grains)
  (declare (type (array double-float) vv))
  (declare (integer grains num))
  (declare (double-float rad))
  (with-struct (sandpaint- size vals rgba) sand
    (declare (type (array double-float) vals))
    (loop for i of-type integer from 0 below num do
      (-draw-circ vals size (vec:arr-get vv i)
                  rad grains rgba))))


(defun strokes (sand lines grains)
  (declare (integer grains))
  (declare (list lines))
  (with-struct (sandpaint- size vals rgba) sand
    (declare (type (array double-float) vals))
    (loop for (u v) in lines do
      (-draw-stroke vals size grains u v rgba))))


(defun stroke (sand line grains)
  (declare (integer grains))
  (declare (list line))
  (with-struct (sandpaint- size vals rgba) sand
    (declare (type (array double-float) vals))
    (destructuring-bind (u v) line
      (-draw-stroke vals size grains u v rgba))))


(defun dens-stroke (sand line &optional (dens 1d0))
  (declare (double-float dens))
  (declare (list line))
  (with-struct (sandpaint- size vals rgba) sand
    (declare (type (array double-float) vals))
    (destructuring-bind (u v) line
      (-draw-dens-stroke vals size dens u v rgba))))


; TODO: this is slow
(defun lin-path (sand path rad grains &key (dens 1d0))
  (declare (double-float rad dens))
  (declare (integer grains))
  (with-struct (sandpaint- size vals rgba) sand
    (declare (type (array double-float) vals))
    (loop for u of-type vec:vec in path and w of-type vec:vec in (cdr path) do
      (let ((stps (math:int (+ 1 (* dens (vec:dst u w))))))
        (declare (integer stps))
        (math:rep (p (math:linspace stps 0 1 :end nil))
          (-draw-circ vals size (vec:on-line p u w) rad grains rgba))))))


; TODO: 16 bit?
(defun save (sand fn &key (gamma 1.0d0)
                     &aux (gamma* (math:dfloat gamma)))
  (with-struct (sandpaint- size vals) sand
    (declare (type (array double-float) vals))
    (let ((png (make-instance
                 'zpng::pixel-streamed-png
                 :color-type :truecolor-alpha
                 :width size
                 :height size)))

      (with-open-file
        (stream (ensure-filename fn ".png")
          :direction :output
          :if-exists :supersede
          :if-does-not-exist :create
          :element-type '(unsigned-byte 8))
        (zpng:start-png png stream)
        (square-loop (x y size)
          (zpng:write-pixel (-png-tuple vals y x gamma*) png))
        (zpng:finish-png png)))))

