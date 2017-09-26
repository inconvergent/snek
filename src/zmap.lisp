
(in-package :zmap)


; this macro exists so we can use the code externally in the same way
; regardless of whether we are using the parallel version of zmap (not yet
; implemented)
(defmacro with* ((zw verts num-verts context-fxn) &body body)
  (let ((zw* (gensym))
        (verts* (gensym))
        (num-verts* (gensym)))
    `(let ((,zw* ,zw))
      (when ,zw*
        (let ((,verts* ,verts)
              (,num-verts* ,num-verts))
          (funcall ,context-fxn (make ,verts* ,num-verts* ,zw*))))
      (progn ,@body))))


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


(defun -xy-to-zone (xy zwidth)
  (declare (vec:vec xy))
  (declare (double-float zwidth))
  (list
    (floor (vec::vec-x xy) zwidth)
    (floor (vec::vec-y xy) zwidth)))


(defun -v-to-zone (verts v zwidth)
  (declare (type (array double-float) verts))
  (declare (integer v))
  (declare (double-float zwidth))
  (list
    (floor (aref verts v 0) zwidth)
    (floor (aref verts v 1) zwidth)))


(defstruct (zmap (:constructor -make-zmap))
  (zwidth nil :type double-float :read-only t)
  (num-verts nil :type integer :read-only t)
  (zone-to-verts nil :type hash-table :read-only t))


(defun add-v-to-zone (zone-to-verts z v)
  (declare (hash-table zone-to-verts))
  (declare (integer v))
  (declare (list z))
  (multiple-value-bind (vals exists)
    (gethash z zone-to-verts)
    (if (not exists)
      (setf vals (make-int-vec)
            (gethash z zone-to-verts) vals))
    (vector-push-extend v vals)))


(defun make (verts num-verts zwidth)
  (declare (double-float zwidth))
  (declare (integer num-verts))
  (declare (type (array double-float) verts))
  (let ((zone-to-verts (make-hash-table :test #'equal)))
    (loop for v integer from 0 below num-verts do
      (add-v-to-zone zone-to-verts (-v-to-zone verts v zwidth) v))
    (-make-zmap
      :zwidth zwidth
      :num-verts num-verts
      :zone-to-verts zone-to-verts)))


(defun verts-in-rad (zm verts xy rad &aux
                           (rad2 (expt (math:dfloat rad) 2.0d0)))
  (declare (type (array double-float) verts))
  (declare (zmap zm))
  (declare (vec:vec xy))
  (declare (double-float rad2))
  (with-struct (zmap- zwidth zone-to-verts) zm
    (let ((inds (make-int-vec)))
      (declare (type (array integer) inds))
      (destructuring-bind (za zb)
        (-xy-to-zone xy zwidth)
        (declare (integer za zb))
        (-extend (za zb z)
          (multiple-value-bind (vals exists)
            (gethash z zone-to-verts)
            (if exists
                (map nil (lambda (zj)
                           (declare (integer zj))
                           (if (< (vec:dst2 xy (vec:arr-get verts zj)) rad2)
                               (vector-push-extend zj inds)))
                     vals)))))
      inds)))

