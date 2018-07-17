
(in-package :sandpaint)

(declaim (optimize (speed 3)))


(defun -swap (vals i j)
  (declare (optimize (safety 0) speed (debug 0))
           (type (simple-array double-float) vals))
  (let ((ri (aref vals i))
        (bi (aref vals (1+ i)))
        (gi (aref vals (+ i 2)))
        (ai (aref vals (+ i 3))))
    (declare (double-float ri bi gi ai))
    (setf (aref vals i) (aref vals j)
          (aref vals (1+ i)) (aref vals (1+ j))
          (aref vals (+ i 2)) (aref vals (+ j 2))
          (aref vals (+ i 3)) (aref vals (+ j 3))
          (aref vals j) ri
          (aref vals (1+ j)) bi
          (aref vals (+ j 2)) gi
          (aref vals (+ j 3)) ai)))


(defun reflect-y (sand &key (alpha 1d0) (align t))
  (declare (optimize (safety 0) speed (debug 0)))
  (-do-op (sand size vals indfx :name "reflect-y")
    (let* ((pa (list (vec:vec 0d0 (math:dfloat size)) vec:*zero*))
           (pb (list (vec:vec (math:dfloat size)) (vec:vec (math:dfloat size) 0d0))))
      (loop with ls = (math:linspace size 0d0 1d0 :end (not align))
            for i in ls and ii from 0
            do (loop with line = (list (vec:on-line* i pa) (vec:on-line* i pb))
                     for j in ls and jj from 0
                     do (-pix-overlap indfx vals size
                          (vec:on-line* j line)
                          (-rgb-from vals (funcall indfx jj ii 0) alpha)))))))


(defun reflect-x (sand &key (alpha 1d0) (align t))
  (declare (optimize (safety 0) speed (debug 0)))
  (-do-op (sand size vals indfx :name "reflect-x")
    (let* ((pa (list vec:*zero* (vec:vec (math:dfloat size) 0d0)))
           (pb (list (vec:vec 0d0 (math:dfloat size)) (vec:vec (math:dfloat size)))))
      (loop with ls = (math:linspace size 0d0 1d0 :end (not align))
            for i in ls and ii from 0
            do (loop with line = (list (vec:on-line* i pa) (vec:on-line* i pb))
                     for j in ls and jj from 0
                     do (-pix-overlap indfx vals size
                          (vec:on-line* j line)
                          (-rgb-from vals (funcall indfx (- size ii 1) jj 0) alpha)))))))


(defun flip-x (sand)
  (declare (optimize (safety 0) speed (debug 0)))
  (-do-op (sand size vals indfx :name "flip-x")
    (loop for i from 0 below size
          do (loop for j from 0 to (/ size 2)
                   do (-swap vals (funcall indfx j i)
                                  (funcall indfx (- size j 1) i))))))

(defun flip-y (sand)
  (declare (optimize (safety 0) speed (debug 0)))
  (-do-op (sand size vals indfx :name "flip-y")
    (loop for i from 0 below size
          do (loop for j from 0 to (/ size 2)
                   do (-swap vals (funcall indfx i j)
                                  (funcall indfx i (- size j 1)))))))

(defun -rot (va vb a b)
  (declare (type (simple-array double-float) va vb) (fixnum a b))
  (setf (aref vb b) (aref va a)
        (aref vb (1+ b)) (aref va (1+ a))
        (aref vb (+ b 2)) (aref va (+ a 2))
        (aref vb (+ b 3)) (aref va (+ a 3))))

(defun rot (sand dir)
  (declare (optimize (safety 0) speed (debug 0)))
  (-do-op (sand size vals indfx :name "rot")
    (let ((new-vals (make-rgba-array size :init 0d0)))
      (case dir
        (:cw (-square-loop (i j size)
               (-rot vals new-vals (funcall indfx i j)
                                   (funcall indfx (- size j 1) i))))
        (:ccw (-square-loop (i j size)
                (-rot vals new-vals (funcall indfx i j)
                                    (funcall indfx j (- size i 1)))))
        (:twice (-square-loop (i j size)
                  (-rot vals new-vals (funcall indfx i j)
                                      (funcall indfx (- size i 1) (- size j 1)))))
        (otherwise (error "use :cw :ccw or :twice")))
      (copy-rgba-array-to-from vals new-vals size))))

