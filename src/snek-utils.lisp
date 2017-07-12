
(in-package :snek)

(defun add-grp! (snk &key (type nil) (closed nil))
  "
  constructor for grp instances.

  edges can be associated with multiple grps.

  verts are global. that is, they do not belong to any grp.
  "

  (with-struct (snek- grps grp-size) snk
    (let ((name (gensym)))
      (setf
        (gethash name grps)
        (make-grp
          :name name
          :closed closed
          :type type
          :grph (graph:make :size grp-size)))
      name)))


(defmacro -valid-vert ((num vv &key (err t)) &body body)
  (with-gensyms (v)
    `(let ((,v ,vv))
      (declare (integer ,v))
      (if (and (> ,v -1) (< ,v ,num))
        (progn ,@body)
        (if ,err (error "vert does not exist: ~a" ,v))))))


(defmacro -valid-verts ((num vv v) &body body)
  (with-gensyms (vv*)
    `(let ((,vv* ,vv))
      (loop for ,v integer in ,vv*
        if
          (and (> ,v -1) (< ,v ,num))
        collect
          (progn ,@body)))))


(defun add-vert! (snk xy)
  (with-struct (snek- verts num-verts) snk
    (declare (type (array double-float) verts))
    (destructuring-bind (x y)
      (math:dfloat* xy)
      (declare (double-float x y))
      (setf (aref verts num-verts 0) x
            (aref verts num-verts 1) y)
      (- (incf (snek-num-verts snk)) 1))))


(defun add-verts! (snk vv)
  (loop for xy in vv collect
    (add-vert! snk xy)))


(defun get-vert (snk v)
  (with-struct (snek- verts num-verts) snk
    (declare (type (array double-float) verts))
    (-valid-vert (num-verts v)
      (get-dfloat-tup verts v))))


(defun get-verts (snk vv)
  (with-struct (snek- verts num-verts) snk
    (declare (type (array double-float) verts))
    (-valid-verts (num-verts vv v)
      (get-dfloat-tup verts v))))


(defun get-all-verts (snk)
  (with-struct (snek- verts num-verts) snk
    (declare (type (array double-float) verts))
    (loop for v integer from 0 below num-verts
      collect (get-dfloat-tup verts v))))


(defun get-grp-verts (snk &key g)
  (get-verts snk
    (get-vert-inds snk :g g)))


(defun get-vert-inds (snk &key g)
  (with-struct (snek- grps) snk
    (multiple-value-bind (grp exists)
      (gethash g grps)
      (if exists
        (graph:get-verts (grp-grph grp))
        (error "grp does not exist: ~a" grp)))))


(defun get-num-edges (snk &key g)
  (with-grp (snk grp g)
    (graph:get-num-edges (grp-grph grp))))


; TODO: option to include both directions?
(defun get-edges (snk &key g)
  (with-grp (snk grp g)
    (with-struct (grp- grph) grp
      (graph:get-edges grph))))


(defun add-edge! (snk ee &key g)
  (with-grp (snk grp g)
    (with-struct (snek- num-verts) snk
      (with-struct (grp- grph) grp
        (destructuring-bind (a b)
          ee
          (declare (integer a b))
          (if (and (< a num-verts)
                   (< b num-verts)
                   (not (eql a b)))
            (if (graph:add grph a b)
              (sort (list a b) #'<))))))))


(defun del-edge! (snk ee &key g)
  (with-grp (snk grp g)
    (with-struct (grp- grph) grp
      (destructuring-bind (a b)
        ee
        (declare (integer a b))
        (graph:del grph a b)))))


(defun verts-in-rad (snk xy rad)
  (with-struct (snek- verts zmap zwidth) snk
    (declare (type (array double-float) verts))
    (zmap:verts-in-rad verts zmap zwidth xy rad)))

