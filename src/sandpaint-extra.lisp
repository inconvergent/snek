(in-package :sandpaint)


(defun pixel-hack (sand &optional (sa 0.9d0))
  "
  scale opacity of pix (0 0) by sa.
  "
  (let ((vals (sandpaint-vals sand)))
    (destructuring-bind (r g b a)
      (loop for i from 0 below 4 collect (aref vals i))
      (declare (double-float r g b a))
      (if (>= a 1.0d0)
        (let ((na (* a (math:dfloat sa))))
          (declare (double-float na))
          (setf (aref vals 0) (* (/ r a) na)
                (aref vals 1) (* (/ g a) na)
                (aref vals 2) (* (/ b a) na)
                (aref vals 3) na))))))


(defun copy-rgba-array-to-from (target source size)
  (declare (optimize (safety 0) speed (debug 0))
           (type (simple-array double-float) target source)
           (fixnum size))
  (loop for i of-type fixnum from 0 below (* size size 4)
        do (setf (aref target i) (the double-float (aref source i)))))


(defun copy-scale-rgba-array-to-from (target source scale size)
  (declare (optimize (safety 0) speed (debug 0))
           (type (simple-array double-float) target source)
           (fixnum size))
  (loop for i of-type fixnum from 0 below (* size size 4)
        do (if (<= (aref scale i) 0)
             (setf (aref target i) (aref source i))
             (setf (aref target i) (/ (aref source i) (aref scale i))))))


;TODO something ...?
(defun nvecstep (n v &key (end t))
  (if (> n 1)
    (let ((nn (if end (1- n) n)))
      (loop for i from 0 below n
            collect (vec:scale v (math:dfloat (expt (* i (/ 1d0 nn)) 3d0)))))
    (if end (list v)
            (list vec:*zero*))))


; experimental CA
; TODO there is a bug here. good luck.
(defun chromatic-aberration (sand &key mid (s 1d0))
  (declare (optimize (safety 0) speed (debug 0)))
  (format t "WARN: CA is currently not working and will produce strange results.~%")
  (-do-op (sand size vals indfx :name "chromatic-aberration")
    (labels
      ((-channel-operator-over (new-vals new-counts sx sy ix iy w channel)
        (when (and (< -1 sx size) (< -1 sy size) (< -1 ix size) (< -1 iy size))
          (let ((sind (funcall indfx sx sy channel))
                (ind (funcall indfx ix iy channel))
                (iw (- 1d0 w)))
            ;(setf (aref new-vals ind) (+ (* (aref vals sind))
            ;                             (* (aref vals ind) w)))
            ;(setf (aref new-vals ind) (aref vals sind))
            ;(+ (* (aref vals ind) ia) (aref vals sind))
            (if (<= (aref new-counts ind) 0d0)
              (setf (aref new-vals ind) (* (aref vals sind) w)
                    (aref new-counts ind) w)
              (setf (aref new-vals ind) (+ (* (aref vals ind) iw)
                                           (* (aref vals sind) w))
                    (aref new-counts ind) (+ (aref new-counts ind) w))))))

       (-point-sample-channel (new-vals new-counts sx sy pt dx channel)
         (declare (optimize (safety 0) speed (debug 0))
                  (type (simple-array double-float) new-vals)
                  (fixnum channel)
                  (vec:vec pt dx))

         (loop ;with len = (vec:len dx)
               ;for dpt in (nvecstep (ceiling (* 3d0 len)) dx)
               for dpt in (nvecstep 1 dx :end t)
               do (multiple-value-bind (ix iy fx fy) (-floor-fract
                                                       (rnd:in-circ 0.25d0 :xy (vec:add pt dpt)))
                    (multiple-value-bind (w1 w2 w3 w4) (-fract-overlap fx fy)
                      (declare (double-float w1 w2 w3 w4))
                      (-channel-operator-over new-vals new-counts sx sy ix iy w1 channel)
                      (-channel-operator-over new-vals new-counts sx sy #1=(+ ix 1) iy w2 channel)
                      (-channel-operator-over new-vals new-counts sx sy ix #2=(+ iy 1) w3 channel)
                      (-channel-operator-over new-vals new-counts sx sy #1# #2# w4 channel))))))

      (let ((center (if mid mid (vec:vec (* 0.5d0 (sandpaint-size sand)))))
            (new-vals (make-rgba-array size :init 1d0))
            (new-counts (make-rgba-array size :init 0d0))
            (base-size (/ s (math:dfloat size) 2d0)))

        (copy-rgba-array-to-from new-vals vals size)

        (-square-loop (sx sy size)
          (let* ((pt (vec:add vec:*half* (vec:vec-coerce sx sy)))
                 (dx (vec:scale (vec:sub pt center) base-size)))
            (-point-sample-channel new-vals new-counts sx sy pt dx 0)
            ;(-point-sample-channel new-vals sx sy pt vec:*zero* 1)
            (-point-sample-channel new-vals new-counts sx sy pt (vec:neg dx) 2)))
        (copy-scale-rgba-array-to-from vals new-vals new-counts size)))))


; TODO: incomplete
(defun filter-walk (sand walker noise &key (alpha 0.5d0))
  (declare (optimize (safety 0) speed (debug 0)))
  (-do-op (sand size vals indfx :name "rot-cw")
    (loop for j from 0 below size
          do (loop for i from 0 to (/ size 2)
                   do (let ((v (funcall walker noise)))
                        (vec:with-xy ((vec:add (vec:vec-coerce i j) v) x y)
                          (-inside-round (size (vec:vec x y) xx yy)
                           (-operator-over indfx vals xx yy
                            (-rgb-from vals (funcall indfx i j) alpha)))))))))

