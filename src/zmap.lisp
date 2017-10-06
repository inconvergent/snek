
(in-package :zmap)


; this macro exists so we can use the code externally in the same way
; regardless of whether we are using the parallel version of zmap (not yet
; implemented)
(defmacro with* ((zw verts num-verts context-fxn) &body body)
  (with-gensyms (zw* verts* num-verts*)
    `(let ((,zw* ,zw))
      (when ,zw*
        (let ((,verts* ,verts)
              (,num-verts* ,num-verts))
          (funcall ,context-fxn (make ,verts* ,num-verts* ,zw*))))
      (progn ,@body))))


(defmacro npairs (a b)
  `(list
    (list (- ,a 1) (- ,b 1))
    (list (- ,a 1) ,b)
    (list (- ,a 1) (+ ,b 1))
    (list ,a (- ,b 1))
    (list ,a ,b)
    (list ,a (+ ,b 1))
    (list (+ ,a 1) (- ,b 1))
    (list (+ ,a 1) ,b)
    (list (+ ,a 1) (+ ,b 1))))


(defun -xy-to-zone (xy zwidth)
  (declare (vec:vec xy))
  (declare (double-float zwidth))
  (vec:with-xy (xy x y)
    (values
      (floor x zwidth)
      (floor y zwidth))))


(defstruct (zmap (:constructor -make-zmap))
  (zwidth nil :type double-float :read-only t)
  (num-verts nil :type integer :read-only t)
  (zone-to-verts nil :type hash-table :read-only t))


(defmacro -add-v-to-zone (verts zone-to-verts v zwidth)
  `(let ((z (list (floor (aref ,verts ,v 0) ,zwidth)
                  (floor (aref ,verts ,v 1) ,zwidth))))
    (multiple-value-bind (vals exists)
    (gethash z ,zone-to-verts)
    (when (not exists)
      (setf vals (make-int-vec 256)
            (gethash z ,zone-to-verts) vals))
    (vector-push-extend ,v vals))))


(defun make (verts num-verts zwidth)
  (declare (double-float zwidth))
  (declare (integer num-verts))
  (declare (type (array double-float) verts))
  (let ((zone-to-verts (make-hash-table :test #'equal)))
    (loop for v integer from 0 below num-verts do
      (-add-v-to-zone verts zone-to-verts v zwidth))
    (-make-zmap
      :zwidth zwidth
      :num-verts num-verts
      :zone-to-verts zone-to-verts)))


(defmacro with-verts-in-rad ((zm verts xy rad v) &body body)
  (with-gensyms (rad2 zm* zwidth zone-to-verts xy* a b za zb
                 vals verts* exists z)
    `(let* ((,rad2 (expt ,rad 2d0))
            (,verts* ,verts)
            (,zm* ,zm)
            (,xy* ,xy)
            (,zwidth (zmap-zwidth ,zm*))
            (,zone-to-verts (zmap-zone-to-verts ,zm*)))
      (multiple-value-bind (,za ,zb)
        (-xy-to-zone ,xy* ,zwidth)
        (loop for ,z in (npairs ,za ,zb) do
          (multiple-value-bind (,vals ,exists)
              (gethash ,z ,zone-to-verts)
              (when ,exists
                (map nil
                     (lambda (,v) (declare (integer ,v))
                        (when (< (vec:dst2 ,xy* (vec:arr-get ,verts* ,v)) ,rad2)
                          (progn ,@body)))
                      ,vals))))))))


(defun verts-in-rad (zm verts xy rad &aux
                           (rad2 (expt (math:dfloat rad) 2.0d0)))
  (declare (type (array double-float) verts))
  (declare (zmap zm))
  (declare (vec:vec xy))
  (declare (double-float rad2))
  (with-struct (zmap- zwidth zone-to-verts) zm
    (let ((inds (make-int-vec)))
      (declare (type (array integer) inds))
      (multiple-value-bind (za zb)
        (-xy-to-zone xy zwidth)
        (declare (integer za zb))
        (loop for z in (npairs za zb) do
          (multiple-value-bind (vals exists)
            (gethash z zone-to-verts)
            (when exists
                (map nil (lambda (zj)
                           (declare (integer zj))
                           (when (< (vec:dst2 xy (vec:arr-get verts zj)) rad2)
                             (vector-push-extend zj inds)))
                     vals)))))
      inds)))

