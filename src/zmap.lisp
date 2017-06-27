
(defpackage :zmap
  (:use :common-lisp)
  (:export
    :make
    :verts-in-rad)
  (:import-from :common-lisp-user
    :dst2
    :to-dfloat
    :get-atup
    :to-dfloat*
    :with-struct))


(in-package :zmap)


(defun xy-to-zone (xy zwidth)
  (mapcar (lambda (x) (floor x zwidth)) xy))


(defun v-to-zone (verts v zwidth)
  (list
    (floor (aref verts v 0) zwidth)
    (floor (aref verts v 1) zwidth)))


(defun add-v-to-zone (zmap z v)
  (multiple-value-bind (vals exists)
    (gethash z zmap)
    (if (not exists)
      (setf vals (make-array 20 :fill-pointer 0 :element-type 'integer)
            (gethash z zmap) vals))
    (vector-push-extend v vals)))


(defun make (verts num-verts zwidth)
  (let ((zmap (make-hash-table :test #'equal)))
    (loop for v from 0 below num-verts do
      (add-v-to-zone
        zmap
        (v-to-zone verts v (to-dfloat zwidth))
        v))
    zmap))


(defmacro -extend (x y &body body)
  `(dolist
    (,x '(-1 0 1))
    (dolist
      (,y '(-1 0 1))
      ,@body)))


(defun nearby-zones (z)
  (destructuring-bind (a b)
    z
    (let ((zs (make-array 9 :fill-pointer 0 :element-type 'integer)))
      (-extend i j (vector-push (list (+ a i) (+ b j)) zs))
      zs)))


(defun verts-in-rad (verts zmap zwidth xy rad)
  (let ((zs (nearby-zones (xy-to-zone xy zwidth)))
            (inds (make-array 20
                              :fill-pointer 0
                              :element-type 'integer))
            (rad2 (* rad rad)))
        (loop for i from 0 below 9 do
          (multiple-value-bind (vals exists)
          (gethash (aref zs i) zmap)
          (if exists
            (loop for j from 0 below (length vals) do
              (let ((zj (aref vals j)))
                (if (< (dst2 xy (get-atup verts zj)) rad2)
                  (vector-push-extend zj inds)))))))
        inds))

