
(in-package :sandpaint)

(defmacro -inside-chrom ((size xy x y) &body body)
  (declare (symbol x y))
  (with-gensyms (sname)
    `(let ((,sname ,size))
      (declare (fixnum ,sname))
      (multiple-value-bind (,x ,y)
          ;(vec::-voutward-round ,xy (vec:vec (* 0.5d0 ,sname)))
          (vec::-vround* ,xy)
        (declare (fixnum ,x ,y))
        (when (and (>= ,x 0) (< ,x ,sname)
                   (>= ,y 0) (< ,y ,sname))
              (progn ,@body))))))


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
        do (when (>= (aref scale i) 0.01d0)
             (setf (aref target i) (/ (aref source i) (aref scale i))))))


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
  (print "WARN: CA is currently not working well.")
  (format t "applying CA ...~%")
  (with-struct (sandpaint- size vals indfx) sand
    (declare (fixnum size))
    (declare (function indfx))

    (labels

      ((-get-new-val (new-count new-vals rind xyind)
        (if (> (aref new-count rind) 0.1d0)
            (+ (aref new-vals rind) (aref vals xyind))
            (aref vals xyind)))

       (-point-sample-channel (new-vals new-count pt dx channel)
        (declare (optimize (safety 0) speed (debug 0))
                 (type (simple-array double-float) new-vals new-count)
                 (fixnum channel)
                 (vec:vec pt dx))
        (loop with len = (vec:len dx)
              for sdx in (nvecstep (ceiling (* 3d0 len)) dx)
              ;for sdx in (nvecstep 1 dx :end t)
              do (-inside-chrom (size pt i j)
                 (-inside-chrom (size (vec:add pt sdx) rx ry)
                   (let* ((xyind (funcall indfx i j channel))
                          (rind (funcall indfx rx ry channel)))
                     (when (not (= xyind rind))
                       (setf (aref new-vals rind)
                               (-get-new-val new-count new-vals rind xyind)
                             (aref new-count rind)
                               (+ 1d0 (aref new-count rind))))))))))

      (let ((center (vec:add vec:*half*
                             (if mid mid (vec:vec (* 0.5d0 (sandpaint-size sand))))))
            (new-vals (make-rgba-array size :init 1d0))
             (new-count (make-rgba-array size))
             (base-size (/ s (math:dfloat size) 2d0)))

        (copy-rgba-array-to-from new-vals vals size)

        (-square-loop (i j size)
          (let* ((pt (vec:add vec:*half* (vec:vec-coerce i j)))
                 (dx (vec:scale (vec:sub pt center) base-size)))
            (-point-sample-channel new-vals new-count pt dx 0)
            (-point-sample-channel new-vals new-count pt (vec:neg dx) 2)))
        (copy-scale-rgba-array-to-from vals new-vals new-count size))))
  (format t "done.~%"))

