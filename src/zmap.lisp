
(in-package :zmap)


(defun xy-to-zone (xy zwidth)
  (declare (vec:vec xy))
  (declare (double-float zwidth))
  (list
    (floor (vec::vec-x xy) zwidth)
    (floor (vec::vec-y xy) zwidth)))


(defun v-to-zone (verts v zwidth)
  (declare (type (array double-float) verts))
  (declare (integer v))
  (declare (double-float zwidth))
  (list
    (floor (aref verts v 0) zwidth)
    (floor (aref verts v 1) zwidth)))


(defun add-v-to-zone (zmap z v)
  (declare (hash-table zmap))
  (declare (integer v))
  (declare (list z))
  (multiple-value-bind (vals exists)
    (gethash z zmap)
    (if (not exists)
      (setf vals (make-int-vec)
            (gethash z zmap) vals))
    (vector-push-extend v vals)))


(defun make (verts num-verts zwidth &aux (zwidth* (math:dfloat zwidth)))
  (declare (double-float zwidth zwidth*))
  (declare (integer num-verts))
  (declare (type (array double-float) verts))
  (let ((zmap (make-hash-table :test #'equal)))
    (loop for v integer from 0 below num-verts do
      (add-v-to-zone
        zmap
        (v-to-zone verts v zwidth*)
        v))
    zmap))


(defmacro -extend ((za zb z) &body body)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,z nil))
      (declare (list ,z))
      (dolist (,a '(-1 0 1))
        (declare (integer ,a))
        (dolist (,b '(-1 0 1))
          (declare (integer ,b))
          (setf ,z (list (+ ,za ,a) (+ ,zb ,b)))
          (progn ,@body))))))


(defun verts-in-rad (verts zmap zwidth xy rad &aux
                           (zwidth* (math:dfloat zwidth))
                           (rad2 (expt (math:dfloat rad) 2.0d0)))
  (declare (vec:vec xy))
  (declare (double-float zwidth* rad2))
  (declare (hash-table zmap))
  (declare (type (array double-float) verts))
  (let ((inds (make-int-vec)))
    (declare (type (array integer) inds))
    (destructuring-bind (za zb)
      (xy-to-zone xy zwidth*)
      (declare (integer za zb))
      (-extend (za zb z)
        (multiple-value-bind (vals exists)
          (gethash z zmap)
          (if exists
            (map nil (lambda (zj)
                       (declare (integer zj))
                       (if (< (vec:dst2 xy (vec:arr-get verts zj)) rad2)
                         (vector-push-extend zj inds)))
                 vals)))))
    inds))

