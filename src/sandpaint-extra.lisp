
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


;TODO something ...?
(defun nvecstep (n v &key (end t))
  (if (> n 1)
    (let ((len (vec:len v))
          (nn (if end (1- n) n)))
      (loop for i from 0 below n
            collect (vec:scale v (math:dfloat (expt (* i (/ 1d0 nn)) 3d0)))))
    (if end (list v)
            (list vec:*zero*))))


; TODO there is a bug here. good luck.
(defun chromatic-aberration (sand &key mid (s 1d0) (noise 1.1442d0))
  (declare (optimize (safety 0) speed (debug 0))
           (double-float s noise))
  (print "WARN: CA is currently not working and will prduce strange results.")
  (format t "applying CA ...~%")
  (with-struct (sandpaint- size vals indfx) sand
    (declare (fixnum size))
    (declare (function indfx))

    ;(+ (* (aref vals ind) ia) r)

    (labels
      ((-channel-operator-over (new-vals sx sy ix iy w channel)
        (when (and (< -1 sx size) (< -1 sy size) (< -1 ix size) (< -1 iy size))
          (let ((sind (funcall indfx sx sy channel))
                (ind (funcall indfx ix iy channel)))
            (setf (aref new-vals ind) (+ (* (aref vals sind))
                                         (* (aref vals ind) w))))))

       (-point-sample-channel (new-vals sx sy pt dx channel)
         (declare (optimize (safety 0) speed (debug 0))
                  (type (simple-array double-float) new-vals)
                  (fixnum channel)
                  (vec:vec pt dx))

         (loop with len = (vec:len dx)
               ;for dpt in (nvecstep (ceiling (* 3d0 len)) dx)
               for dpt in (nvecstep 1 dx :end t)
               do (multiple-value-bind (ix iy fx fy) (-floor-fract
                                                       (rnd:in-circ 0.25d0 :xy (vec:add pt dpt)))
                    (multiple-value-bind (w1 w2 w3 w4) (-fract-overlap fx fy)
                      (declare (double-float w1 w2 w3 w4))
                      (-channel-operator-over new-vals sx sy ix iy w1 channel)
                      (-channel-operator-over new-vals sx sy #1=(+ ix 1) iy w2 channel)
                      (-channel-operator-over new-vals sx sy ix #2=(+ iy 1) w3 channel)
                      (-channel-operator-over new-vals sx sy #1# #2# w4 channel))))))

      (let ((center (if mid mid (vec:vec (* 0.5d0 (sandpaint-size sand)))))
            (new-vals (make-rgba-array size :init 1d0))
            (base-size (/ s (math:dfloat size) 2d0)))

        (copy-rgba-array-to-from new-vals vals size)

        (-square-loop (sx sy size)
          (let* ((pt (vec:add vec:*half* (vec:vec-coerce sx sy)))
                 (dx (vec:scale (vec:sub pt center) base-size)))
            (-point-sample-channel new-vals sx sy pt dx 0)
            (-point-sample-channel new-vals sx sy pt vec:*zero* 1)
            (-point-sample-channel new-vals sx sy pt (vec:neg dx) 2)))
        (copy-rgba-array-to-from vals new-vals size))))
  (format t "done.~%"))

