(in-package :snek)

(defun add-grp! (snk &key (type nil) (closed nil))
  "
  constructor for grp instances.

  edges can be associated with multiple grps.

  verts are global. that is, they do not belong to any grp on their own.
  however, if a vert is associated with an edge, that vert is also associated
  with whatever grp that edge belongs to.

    - to get verts in a grp: (get-grp-verts ...).
    - to get indices of verts (in a grp): (get-vert-inds ...)
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
  (declare (vec:vec xy))
  (with-struct (snek- verts num-verts) snk
    (declare (type (array double-float) verts))
    (setf (aref verts num-verts 0) (vec::vec-x xy)
          (aref verts num-verts 1) (vec::vec-y xy))
    (- (incf (snek-num-verts snk)) 1)))


(defun add-verts! (snk vv)
  (loop for xy of-type vec:vec in vv collect
    (add-vert! snk xy)))


(defun get-vert (snk v)
  (with-struct (snek- verts num-verts) snk
    (declare (type (array double-float) verts))
    (-valid-vert (num-verts v)
      (vec:arr-get verts v))))


(defun get-verts (snk vv)
  (with-struct (snek- verts num-verts) snk
    (declare (type (array double-float) verts))
    (-valid-verts (num-verts vv v)
      (vec:arr-get verts v))))


(defun get-all-verts (snk)
  (with-struct (snek- verts num-verts) snk
    (declare (type (array double-float) verts))
    (loop for v integer from 0 below num-verts
      collect (vec:arr-get verts v))))


(defun get-all-grps (snk)
  (loop for g being the hash-keys of (snek-grps snk)
    if g ; ignores nil (main) grp
    collect g))


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
  (declare (list ee))
  (with-grp (snk grp g)
    (with-struct (grp- grph) grp
      (destructuring-bind (a b)
        ee
        (declare (integer a b))
        (graph:del grph a b)))))


(defmacro with-verts-in-rad ((snk xy rad v) &body body)
  (with-gensyms (sname)
    `(let ((,sname ,snk))
      (zmap:with-verts-in-rad ((snek-zmap ,sname) (snek-verts ,sname) ,xy ,rad ,v)
        (progn ,@body)))))


(defun verts-in-rad (snk xy rad)
  (declare (vec:vec xy))
  (declare (double-float rad))
  (with-struct (snek- verts zmap) snk
    (declare (type (array double-float) verts))
    (zmap:verts-in-rad zmap verts xy rad)))

