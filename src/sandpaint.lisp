
(in-package :sandpaint)

(declaim (optimize (speed 3)))


(defmacro -inside-round ((size xy x y) &body body)
  (declare (symbol x y))
  (with-gensyms (sname)
    `(let ((,sname ,size))
      (declare (fixnum ,sname))
      (multiple-value-bind (,x ,y) (vec::-vround* ,xy)
        (declare (fixnum ,x ,y))
        (when (and (< -1 ,x ,sname) (< -1 ,y ,sname))
              (progn ,@body))))))


(defmacro -inside-floor ((size xy x y) &body body)
  (declare (symbol x y))
  (with-gensyms (sname)
    `(let ((,sname ,size))
      (declare (fixnum ,sname))
      (multiple-value-bind (,x ,y) (vec::-vfloor* ,xy)
        (declare (fixnum ,x ,y))
        (when (and (< -1 ,x ,sname) (< -1 ,y ,sname))
              (progn ,@body))))))


(defmacro -square-loop ((x y n) &body body)
  (declare (symbol x y))
  (with-gensyms (nname)
    `(let ((,nname ,n))
      (loop for ,y of-type fixnum from 0 below ,nname
            do (loop for ,x of-type fixnum from 0 below ,nname
                     do (progn ,@body))))))


(defmacro -do-op ((sand size vals indfx &key name) &body body)
  (declare (symbol sand size vals indfx))
  (with-gensyms (sname)
    `(let* ((,sname ,sand)
            (,size (sandpaint-size ,sname))
            (,vals (sandpaint-vals ,sname))
            (,indfx (sandpaint-indfx ,sname)))
      (declare (type (simple-array double-float) ,vals)
               (function ,indfx) (fixnum ,size))
      ,(when name `(format t "applying:~a...~%" ,name))
      (progn ,@body)
      ,(when name `(format t "done.~%")))))


(defmacro -do-op* ((sand size vals indfx) &body body)
  (declare (symbol sand size vals indfx))
  (with-gensyms (sname)
    `(let* ((,sname ,sand)
            (,size (sandpaint-size ,sname))
            (,vals (sandpaint-vals ,sname))
            (,indfx (sandpaint-indfx ,sname)))
      (declare (type (simple-array double-float) ,vals)
               (function ,indfx) (fixnum ,size))
      (progn ,@body))))


(defun get-ind-fx (size)
  (declare (fixnum size))
  (lambda (x y &optional (c 0))
    (declare (optimize (safety 0) speed (debug 0)) (fixnum x y c))
    (+ c (the fixnum (* 4 (the fixnum (+ x (the fixnum (* size y)))))))))


(defstruct sandpaint
  (size nil :type fixnum :read-only t)
  (vals nil :type (simple-array double-float) :read-only t)
  (fg nil :type pigment:rgba :read-only nil)
  (bg nil :type pigment:rgba :read-only nil)
  (indfx nil :type function :read-only t))


(defun make-rgba-array (size &key (init 0d0))
  (declare (fixnum size))
  (make-array (* size size 4) :adjustable nil
                              :initial-element init
                              :element-type 'double-float))


(defun -rgb-from (vals ind &optional (a 1d0))
  (declare (optimize (safety 0) speed (debug 0))
           (fixnum ind)
           (type (simple-array double-float (*)) vals))
  (pigment:rgb (aref vals ind) (aref vals (1+ ind)) (aref vals (+ ind 2)) a))


(defun sample (sand xy &key (alpha 1d0))
  (-do-op* (sand size vals indfx)
    (-inside-floor (size xy x y)
      (let* ((ind (funcall indfx x y))
             (a (aref vals (+ ind 3))))
        (pigment:rgb (/ (aref vals ind) a) (/ (aref vals (1+ ind)) a)
                     (/ (aref vals (+ ind 2)) a) alpha)))))


(defun get-size (sand) (sandpaint-size sand))


(declaim (inline -scale-convert))
(defun -scale-convert (v &key (s 1d0) (gamma 1d0))
  (declare (double-float s gamma))
  (setf v (expt (max 0d0 (/ v s)) gamma)))

(declaim (inline -operator-over))
(defun -operator-over (indfx vals x y fg)
  (declare (optimize (safety 0) speed (debug 0))
           (function indfx)
           (type (simple-array double-float (*)) vals)
           (fixnum x y) (pigment:rgba fg))
  (pigment:with (fg r g b a)
    (let ((ind (funcall indfx x y))
          (ia (- 1d0 a)))
      (declare (fixnum ind)
               (double-float ia))
      (setf (aref vals ind)
              (the double-float (+ (the double-float
                                        (* (the double-float (aref vals ind)) ia)) r))
            (aref vals (+ ind 1))
              (the double-float (+ (the double-float
                                        (* (the double-float (aref vals (+ ind 1))) ia)) g))
            (aref vals (+ ind 2))
              (the double-float (+ (the double-float
                                        (* (the double-float (aref vals (+ ind 2))) ia)) b))
            (aref vals (+ ind 3))
              (the double-float (+ (the double-float
                                        (* (the double-float (aref vals (+ ind 3))) ia)) a))))))


(defun -draw-stroke (indfx vals size grains v1 v2 fg)
  (declare (function indfx)
           (type (simple-array double-float) vals)
           (fixnum size grains) (pigment:rgba fg))
  (rnd:with-on-line (grains v1 v2 rn)
    (-inside-round (size rn x y)
      (-operator-over indfx vals x y fg))))


(defun -draw-stroke-overlap (indfx vals size grains v1 v2 fg)
  (declare (function indfx)
           (type (simple-array double-float) vals)
           (fixnum size grains) (pigment:rgba fg))
  (rnd:with-on-line (grains v1 v2 pt)
    (-pix-overlap indfx vals size pt fg)))


(defun -draw-dens-stroke (indfx vals size dens v1 v2 fg)
  (declare (function indfx)
           (type (simple-array double-float) vals)
           (fixnum size) (double-float dens) (pigment:rgba fg))
  (rnd:with-on-line ((ceiling (* dens (vec:dst v1 v2))) v1 v2 rn)
    (-inside-round (size rn x y)
      (-operator-over indfx vals x y fg))))


(defun -draw-circ (indfx vals size xy rad grains fg)
  (declare (function indfx)
           (type (simple-array double-float) vals)
           (fixnum size grains) (double-float rad) (pigment:rgba fg))
  (rnd:with-in-circ (grains rad p :xy xy)
    (-inside-round (size p x y)
      (-operator-over indfx vals x y fg))))


(declaim (inline -u8) (ftype (function (double-float) fixnum) -u8))
(defun -u8 (v)
  (declare (optimize (safety 0) speed (debug 0)) (double-float v))
  (cond ((> v 1d0) 255)
        ((< v 0d0) 0)
        (t (floor (the float (* 255 v))))))

(declaim (inline -ui8) (ftype (function (fixnum) double-float) -ui8))
(defun -ui8 (v)
  (declare (optimize (safety 0) speed (debug 0)) (fixnum v))
  (cond ((>= v 255) 1d0)
        ((< v 0) 0d0)
        (t (/ (math:dfloat v) 255d0))))


(declaim (inline -u16) (ftype (function (double-float) fixnum) -u16))
(defun -u16 (v)
  (declare (optimize (safety 0) speed (debug 0)) (double-float v))
  (cond ((> v 1d0) 65535)
        ((< v 0d0) 0)
        (t (floor (the float (* 65535d0 v))))))


(declaim (inline -png-vals))
(defun -png-vals (indfx vals x y g bitfx)
  (declare (function indfx bitfx)
           (type (simple-array double-float) vals)
           (fixnum x y) (double-float g))
  (let* ((ind (funcall indfx x y))
         (a (aref vals (+ 3 ind))))
    (declare (double-float a) (fixnum ind))
    (if (> a 0d0)
      (values (funcall bitfx (-scale-convert (aref vals (+ ind 0)) :s a :gamma g))
              (funcall bitfx (-scale-convert (aref vals (+ ind 1)) :s a :gamma g))
              (funcall bitfx (-scale-convert (aref vals (+ ind 2)) :s a :gamma g))
              (funcall bitfx (-scale-convert a :gamma g)))
      (values 0 0 0 0))))


(defun clear (sand &optional c)
  (declare (sandpaint sand))
  (pigment:with ((if c c (sandpaint-bg sand)) r g b a)
    (-do-op (sand size vals indfx)
      (-square-loop (x y size)
        (let ((ind (funcall indfx x y)))
          (declare (fixnum ind))
          (setf (aref vals ind) r
                (aref vals (+ ind 1)) g
                (aref vals (+ ind 2)) b
                (aref vals (+ ind 3)) a))))))


(defun make (size &key (fg (pigment:rgb 0.0d0 0.0d0 0.0d0))
                       (bg (pigment:rgb 1.0d0 1.0d0 1.0d0)))
  (pigment:with (bg r g b a)
    (let ((vals (make-rgba-array size))
          (indfx (get-ind-fx size)))
      (declare (function indfx))
      (-square-loop (x y size)
        (let ((ind (funcall indfx x y)))
          (declare (fixnum ind))
          (setf (aref vals ind) r
                (aref vals (+ ind 1)) g
                (aref vals (+ ind 2)) b
                (aref vals (+ ind 3)) a)))
      (make-sandpaint :size size :fg fg :bg bg :vals vals :indfx indfx))))


(defun set-fg-color (sand c)
  (declare (pigment:rgba c))
  (setf (sandpaint-fg sand) c))


(defun set-bg-color (sand c)
  (declare (pigment:rgba c))
  (setf (sandpaint-bg sand) c))


(defun pix (sand vv)
  (declare (list vv))
  (-do-op (sand size vals indfx)
    (loop with fg = (sandpaint-fg sand)
          for v of-type vec:vec in vv
          do (-inside-round (size v x y)
               (-operator-over indfx vals x y fg)))))


(defun arr-pix (sand vv n)
  (declare (fixnum n))
  (-do-op (sand size vals indfx)
    (loop with fg = (sandpaint-fg sand)
          for i of-type fixnum from 0 below n
          do (-inside-round (size (vec:sarr-get vv i) x y)
               (-operator-over indfx vals x y fg)))))


(declaim (inline -floor-fract)
         (ftype (function (vec:vec) (values fixnum fixnum
                                            double-float double-float)) -floor-fract))
(defun -floor-fract (pt)
  (declare (optimize (safety 0) speed (debug 0)) (vec:vec pt))
  (vec:with-xy (pt x y)
    (multiple-value-bind (ix fx) (floor x)
      (declare (fixnum ix) (double-float fx))
      (multiple-value-bind (iy fy) (floor y)
        (declare (fixnum iy) (double-float fy))
        (values ix iy fx fy)))))

(declaim (inline -fract-overlap)
         (ftype (function (double-float double-float)
                          (values double-float double-float
                                  double-float double-float)) -fract-overlap))
(defun -fract-overlap (x y)
  (declare (optimize (safety 0) speed (debug 0))
           (double-float x y))
  (let ((x2 (- 1 x))
        (y2 (- 1 y)))
    (declare (double-float x2 y2))
    (values (* x2 y2) (* x y2) (* x2 y) (* x y))))

;suggested by
;https://twitter.com/porglezomp/status/1014612499315003392
(defun -pix-overlap (indfx vals size pt fg)
  (declare (type (simple-array double-float) vals)
           (fixnum size) (function indfx)
           (vec:vec pt) (pigment:rgba fg))
  (pigment:with (fg r g b a)
    (labels

      ((-operator-over-overlap (ix iy s)
        (declare (optimize (safety 0) speed (debug 0))
                 (fixnum ix iy) (double-float s))
        (when (and (< -1 ix size) (< -1 iy size))
          (-operator-over indfx vals ix iy
                          (pigment::-make-rgba :r (* s r) :g (* s g)
                                             :b (* s b) :a (* s a))))))

        (multiple-value-bind (ix iy fx fy) (-floor-fract pt)
          (multiple-value-bind (s1 s2 s3 s4) (-fract-overlap fx fy)
            (declare (double-float s1 s2 s3 s4))
            (-operator-over-overlap ix iy s1)
            (-operator-over-overlap #1=(+ ix 1) iy s2)
            (-operator-over-overlap ix #2=(+ iy 1) s3)
            (-operator-over-overlap #1# #2# s4))))))

(defun pix-overlap (sand pts)
  (declare (sandpaint sand) (list pts))
  (-do-op (sand size vals indfx)
    (loop with fg = (sandpaint-fg sand)
          for pt of-type vec:vec in pts
          do (-pix-overlap indfx vals size pt fg))))


(defun pix-overlap* (sand pt)
  (declare (sandpaint sand) (vec:vec pt))
  (-do-op (sand size vals indfx)
    (-pix-overlap indfx vals size pt (sandpaint-fg sand))))


(defun circ (sand vv rad n)
  (declare (list vv) (double-float rad) (fixnum n))
  (-do-op (sand size vals indfx)
    (loop with fg = (sandpaint-fg sand)
          for v of-type vec:vec in vv do
      (-draw-circ indfx vals size v rad n fg))))


(defun bzspl-stroke (sand bz n)
  (declare (fixnum n))
  (-do-op (sand size vals indfx)
    (let ((fg (sandpaint-fg sand)))
      (bzspl:with-rndpos (bz n v)
        (-inside-round (size v x y)
          (-operator-over indfx vals x y fg))))))


(defun arr-circ (sand vv num rad grains)
  (declare (type (simple-array double-float) vv)
           (fixnum grains num) (double-float rad))
  (-do-op (sand size vals indfx)
    (loop with fg = (sandpaint-fg sand)
          for i of-type fixnum from 0 below num
          do (-draw-circ indfx vals size (vec:sarr-get vv i)
                         rad grains fg))))


(defun strokes (sand lines grains)
  (declare (fixnum grains) (list lines))
  (-do-op (sand size vals indfx)
    (loop with fg = (sandpaint-fg sand)
          for (u v) in lines
          do (-draw-stroke indfx vals size grains u v fg))))


(defun stroke (sand line grains &key overlap)
  (declare (fixnum grains) (list line))
  (-do-op (sand size vals indfx)
    (destructuring-bind (u v) line
      (if overlap
        (-draw-stroke-overlap indfx vals size grains u v (sandpaint-fg sand))
        (-draw-stroke indfx vals size grains u v (sandpaint-fg sand))))))


(defun dens-stroke (sand line &optional (dens 1d0))
  (declare (double-float dens) (list line))
  (-do-op (sand size vals indfx)
    (destructuring-bind (u v) line
      (-draw-dens-stroke indfx vals size dens u v (sandpaint-fg sand)))))


(defun lin-path (sand path rad grains &key (dens 1d0))
  (declare (double-float rad dens) (fixnum grains))
  (-do-op (sand size vals indfx)
    (loop with fg = (sandpaint-fg sand)
          for u of-type vec:vec in path
          and w of-type vec:vec in (cdr path)
          do (math:with-linspace ((* (vec:dst u w) dens) 0d0 1d0 p :end nil)
               (-draw-circ indfx vals size (vec:on-line p u w) rad grains fg)))))


(defun -save8 (sand fn &key gamma)
  "
  save as 8 bits. supports alpha.
  "
  (-do-op (sand size vals indfx)
    (let ((png (make-instance 'zpng::pixel-streamed-png
                              :color-type :truecolor-alpha
                              :width size
                              :height size)))
      (with-open-file
        (fstream (ensure-filename fn ".png") :direction :output
                                             :if-exists :supersede
                                             :if-does-not-exist :create
                                             :element-type '(unsigned-byte 8))
        (declare (stream fstream))
        (zpng:start-png png fstream)
        (-square-loop (x y size)
          (multiple-value-bind (r g b a) (-png-vals indfx vals x y gamma #'-u8)
            (declare (fixnum r g b))
            (zpng:write-pixel (list r g b a) png)))
        (zpng:finish-png png)))))

(defun -save16 (sand fn &key gamma)
  "
  save as 16 bits. does not support alpha.
  "
  (-do-op (sand size vals indfx)
    (let ((img (png:make-image size ;width
                               size ;height
                               3 16)))
      (-square-loop (x y size)
        (multiple-value-bind (r g b) (-png-vals indfx vals x y gamma #'-u16)
          (declare (fixnum r g b))
          (setf (aref img y x 0) r
                (aref img y x 1) g
                (aref img y x 2) b)))
      (with-open-file (output (ensure-filename fn ".png")
                              :element-type '(unsigned-byte 8)
                              :direction :output :if-exists :supersede)
        (png:encode img output)))))


(defun save (sand fn &key (gamma 1d0) (bits 8)
                     &aux (gamma* (math:dfloat gamma)))
  (declare (sandpaint sand) (fixnum bits))
  (case bits (8 (-save8 sand fn :gamma gamma*))
             (16 (-save16 sand fn :gamma gamma*))
             (otherwise (error "bits must be 8 or 16. default is 8."))))


(defun -init-rgb-from-png (indfx vals img s)
  (loop for i from 0 below s do
    (loop for j from 0 below s do
          (setf (aref vals (funcall indfx j i 0)) (-ui8 (aref img i j 0))
                (aref vals (funcall indfx j i 1)) (-ui8 (aref img i j 1))
                (aref vals (funcall indfx j i 2)) (-ui8 (aref img i j 2))
                (aref vals (funcall indfx j i 3)) 1d0))))

; TODO: indexed/grayscale channel only
(defun png-open (fn)
  "read a png image."
  (let ((img (with-open-file (input fn :element-type '(unsigned-byte 8))
               (png:decode input))))
    (destructuring-bind (h w c) (array-dimensions img)
      (declare (ignore c))
      (when (not (= h w)) (error "can only load square images"))
      (let ((sand (make h)))
        (-init-rgb-from-png (sandpaint-indfx sand) (sandpaint-vals sand) img h)
        sand))))

