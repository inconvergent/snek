
(in-package :sandpaint)

(declaim (optimize (speed 3)))


(defun reflect-y (sand &key (align t) (alpha 0.5d0))
  (declare (optimize (safety 0) speed (debug 0)))
  (format t "applying filter:reflect-y...~%")
  (with-struct (sandpaint- size vals indfx) sand
    (declare (fixnum size)
             (type (simple-array double-float) vals)
             (function indfx))
    (let* ((pa (list (vec:vec 0d0 (math:dfloat size)) vec:*zero*))
           (pb (list (vec:vec (math:dfloat size)) (vec:vec (math:dfloat size) 0d0))))
      (loop with ls = (math:linspace size 0d0 1d0 :end (not align))
            for i in ls
            and ii from 0
            do (loop with line = (list (vec:on-line* i pa) (vec:on-line* i pb))
                     for j in ls
                     and jj from 0
                     do (-pix-overlap indfx vals size
                          (vec:on-line* j line)
                          (-rgb-from vals (funcall indfx jj ii 0) alpha))))))
  (format t "done.~%"))


(defun reflect-x (sand &key (align t) (alpha 0.5d0))
  (declare (optimize (safety 0) speed (debug 0)))
  (format t "applying filter:reflect-x...~%")
  (with-struct (sandpaint- size vals indfx) sand
    (declare (fixnum size)
             (type (simple-array double-float) vals)
             (function indfx))
    (let* ((pa (list vec:*zero* (vec:vec (math:dfloat size) 0d0)))
           (pb (list (vec:vec 0d0 (math:dfloat size)) (vec:vec (math:dfloat size)))))
      (loop with ls = (math:linspace size 0d0 1d0 :end (not align))
            for i in ls
            and ii from 0
            do (loop with line = (list (vec:on-line* i pa) (vec:on-line* i pb))
                     for j in ls
                     and jj from 0
                     do (-pix-overlap indfx vals size
                          (vec:on-line* j line)
                          (-rgb-from vals (funcall indfx (- size ii 1) jj 0) alpha))))))
  (format t "done.~%"))


; TODO: what should this do?
(defun reflect-diag (sand &key (align t) (alpha 0.5d0))
  (declare (optimize (safety 0) speed (debug 0)))
  (format t "applying filter:reflect-diag...~%")
  (with-struct (sandpaint- size vals indfx) sand
    (declare (fixnum size)
             (type (simple-array double-float) vals)
             (function indfx))
    (let* ((pa (list vec:*zero* (vec:vec (math:dfloat size) 0d0)))
           (pb (list (vec:vec 0d0 (math:dfloat size)) (vec:vec (math:dfloat size)))))
      (loop with ls = (math:linspace size 0d0 1d0 :end (not align))
            for i in ls
            and ii from 0
            do (loop with line = (list (vec:on-line* i pa) (vec:on-line* i pb))
                     for j in ls
                     and jj from 0
                     do (-pix-overlap indfx vals size
                          (vec:on-line* j line)
                          (-rgb-from vals (funcall indfx jj (- size ii 1) 0) alpha))))))
  (format t "done.~%"))


(defun -flip (vals i j)
  (declare (optimize (safety 0) speed (debug 0))
           (type (simple-array double-float) vals))
  (let ((ri (aref vals i))
        (bi (aref vals (1+ i)))
        (gi (aref vals (+ i 2)))
        (ai (aref vals (+ i 3))))
    (setf (aref vals i) (aref vals j)
          (aref vals (1+ i)) (aref vals (1+ j))
          (aref vals (+ i 2)) (aref vals (+ j 2))
          (aref vals (+ i 3)) (aref vals (+ j 3))
          (aref vals j) ri
          (aref vals (1+ j)) bi
          (aref vals (+ j 2)) gi
          (aref vals (+ j 3)) ai)))

(defun flip-x (sand)
  (declare (optimize (safety 0) speed (debug 0)))
  (format t "applying filter:flip-x...~%")
  (with-struct (sandpaint- size vals indfx) sand
    (declare (fixnum size)
             (type (simple-array double-float) vals)
             (function indfx))
    (loop for i from 0 below size
          do (loop for j from 0 to (/ size 2)
                   do (-flip vals (funcall indfx j i)
                                  (funcall indfx (- size j 1) i)))))
  (format t "done.~%"))


(defun flip-y (sand)
  (declare (optimize (safety 0) speed (debug 0)))
  (format t "applying filter:flip-y...~%")
  (with-struct (sandpaint- size vals indfx) sand
    (declare (fixnum size)
             (type (simple-array double-float) vals)
             (function indfx))
    (loop for i from 0 below size
          do (loop for j from 0 to (/ size 2)
                   do (-flip vals (funcall indfx i j)
                                  (funcall indfx i (- size j 1))))))
  (format t "done.~%"))

