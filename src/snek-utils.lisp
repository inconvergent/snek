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

  (declare (snek snk))
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
        (when ,err (error "vert does not exist: ~a" ,v))))))


(defmacro -valid-verts ((num vv v) &body body)
  (with-gensyms (vv*)
    `(let ((,vv* ,vv))
      (loop for ,v integer in ,vv*
        if
          (and (> ,v -1) (< ,v ,num))
        collect
          (progn ,@body)))))


(defun add-vert! (snk xy)
  "
  adds a new vertex to snek

  returns the id of the new vertex
  "
  (declare (snek snk))
  (declare (vec:vec xy))
  (with-struct (snek- verts num-verts) snk
    (declare (type (array double-float) verts))
    (setf (aref verts num-verts 0) (vec::vec-x xy)
          (aref verts num-verts 1) (vec::vec-y xy))
    (- (incf (snek-num-verts snk)) 1)))


(defun add-verts! (snk vv)
  "
  adds new vertices to snek

  returns the ids of the new vertices
  "
  (declare (snek snk))
  (declare (list vv))
  (loop for xy of-type vec:vec in vv collect
    (add-vert! snk xy)))


(defun get-vert (snk v)
  "
  get the coordinate (vec) of vertex (id) v
  "
  (declare (snek snk))
  (with-struct (snek- verts num-verts) snk
    (declare (type (array double-float) verts))
    (-valid-vert (num-verts v)
      (vec:arr-get verts v))))


(defun get-verts (snk vv)
  "
  get the coordinates (vec) of vertices (ids) vv
  "
  (declare (snek snk))
  (with-struct (snek- verts num-verts) snk
    (declare (type (array double-float) verts))
    (-valid-verts (num-verts vv v)
      (vec:arr-get verts v))))


(defun get-all-verts (snk)
  "
  returns the coordinates (vec) of all vertices.
  "
  (declare (snek snk))
  (with-struct (snek- verts num-verts) snk
    (declare (type (array double-float) verts))
    (loop for v integer from 0 below num-verts
      collect (vec:arr-get verts v))))


(defun get-all-grps (snk)
  "
  returns all grps.
  "
  (declare (snek snk))
  (loop for g being the hash-keys of (snek-grps snk)
    if g ; ignores nil (main) grp
    collect g))


(defun get-grp-verts (snk &key g)
  "
  returns all vertices in a grp
  "
  (declare (snek snk))
  (get-verts snk
    (get-vert-inds snk :g g)))


(defun is-vert-in-grp (snk v &key g)
  "
  tests whether v is in grp g
  "
  (declare (snek snk))
  (with-struct (snek- grps) snk
    (multiple-value-bind (grp exists)
      (gethash g grps)
      (if exists
        (graph:vmem (grp-grph grp) v)
        (error "grp does not exist: ~a" grp)))))


(defun get-vert-inds (snk &key g)
  "
  returns all vertex indices that belongs to a grp
  "
  (declare (snek snk))
  (with-struct (snek- grps) snk
    (multiple-value-bind (grp exists)
      (gethash g grps)
      (if exists
        (graph:get-verts (grp-grph grp))
        (error "grp does not exist: ~a" grp)))))


(defun get-num-edges (snk &key g)
  (declare (snek snk))
  (with-grp (snk grp g)
    (graph:get-num-edges (grp-grph grp))))


; TODO: option to include both directions?
(defun get-edges (snk &key g)
  (declare (snek snk))
  (with-grp (snk grp g)
    (with-struct (grp- grph) grp
      (graph:get-edges grph))))


(defun get-incident-edges (snk v &key g)
  (declare (snek snk))
  (declare (snek snk))
  (declare (integer v))
  (with-grp (snk grp g)
    (with-struct (grp- grph) grp
      (graph:get-incident-edges grph v))))

; TODO: get-all-incident-edges?


(defun add-edge! (snk ee &key g)
  "
  adds a new edge to snek. provided the edge is valid. otherwise it returns nil.

  note: returns nil if the edge exists already.
  "
  (declare (snek snk))
  (with-grp (snk grp g)
    (with-struct (snek- num-verts) snk
      (with-struct (grp- grph) grp
        (destructuring-bind (a b)
          ee
          (declare (integer a b))
          (when (and (< a num-verts)
                   (< b num-verts)
                   (not (eql a b)))
            (when (graph:add grph a b)
              (sort (list a b) #'<))))))))


(defun add-edges! (snk ee)
  "
  adds multiple edges (see above). returns a list of the results.
  "
  (declare (snek snk))
  (declare (list ee))
  (loop for e list in ee collect
    (add-edge! snk e)))


(defun del-edge! (snk ee &key g)
  (declare (snek snk))
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
  (declare (snek snk))
  (declare (vec:vec xy))
  (declare (double-float rad))
  (with-struct (snek- verts zmap) snk
    (declare (type (array double-float) verts))
    (zmap:verts-in-rad zmap verts xy rad)))

