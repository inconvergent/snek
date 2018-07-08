(in-package :snek)


(defun add-grp! (snk &key type name props &aux (name* (if name name (gensym))))
  "
  constructor for grp instances.

  grps are edge graphs.

  nil is the default grp. as such, nil is not an allowed grp name (there is
  always a default grp named nil). if name is nil, the name will be a gensym.

  edges can be associated with multiple grps.

  verts are global. that is, they do not belong to any grp on their own.
  however, if a vert is associated with an edge, that vert is also associated
  with whatever grp that edge belongs to.

    - to get verts in a grp: (get-grp-verts snk :g g).
    - to get indices of verts (in a grp): (get-vert-inds snk :g g)
    - ...

  this functionality is still experimental.
  "

  (declare (snek snk))
  (with-struct (snek- grps grp-size) snk
    (multiple-value-bind (v exists)
      (gethash name* grps)
      (when exists (error "grp name already exists: ~a" name*)))
    (setf (gethash name* grps) (-make-grp :name name*
                                          :type type
                                          :grph (graph:make :size grp-size)
                                          :props props)))
  name*)


(defun add-prm! (snk &key name type props args
                     &aux (name* (if name name (gensym))))
  "
  constructor for prm instances.

  prms (primitives) are generic entities of a particular type.  eg. a primitive
  is a path, a bzspl or a circle.

  nil is not an allowed prm name. if name is nil, the name will be a gensym.

  the default type is nil.

  a given prm can be 'rendered' using (snek:prmr snk :p p). the behaviour of
  (snek:prmr...) is determined by the rfxn functions. these functions is either
  the default function or whatever functions are provided by to the :prms when
  creating a snek instance

  this functionality is experimental.
  "

  (declare (snek snk))
  (with-struct (snek- prms) snk
    (multiple-value-bind (v exists)
      (gethash name* prms)
      (when exists (error "prm name already exists: ~a" name*)))
    (setf (gethash name* prms) (-make-prm :name name*
                                          :type type
                                          :props props
                                          :args args)))
  name*)


(defmacro -valid-vert ((num vv &key (err t)) &body body)
  (with-gensyms (v)
    `(let ((,v ,vv))
      (declare (fixnum ,v))
      (if (and (> ,v -1) (< ,v ,num))
        (progn ,@body)
        (when ,err (error "vert does not exist: ~a" ,v))))))


(defmacro -valid-verts ((num vv v) &body body)
  (with-gensyms (vv*)
    `(let ((,vv* ,vv))
      (loop for ,v of-type fixnum in ,vv*
            if (and (> ,v -1) (< ,v ,num))
            collect (progn ,@body)))))


(defun add-vert! (snk xy &key p name)
  "
  adds a new vertex to snek

  returns the id of the new vertex
  "
  (declare (snek snk))
  (declare (vec:vec xy))
  (with-struct (snek- verts num-verts) snk
    (declare (type (simple-array double-float) verts))
    (setf (aref verts (* 2 num-verts)) (vec::vec-x xy)
          (aref verts (1+ (* 2 num-verts))) (vec::vec-y xy)))

  (let ((i (1- (incf (snek-num-verts snk)))))
    (when name
      (multiple-value-bind (val exists)
        (gethash name (snek-vert-names snk))
        (when exists (error "vert name already exists: ~a" name))
        (setf (gethash name (snek-vert-names snk)) i)))
    (when p
      (with-struct (prm- verts) (gethash p (snek-prms snk))
        (array-push i verts)
        (incf (prm-num-verts (gethash p (snek-prms snk))))))
    i))


(defun add-verts! (snk vv &key p names)
  "
  adds new vertices to snek

  returns the ids of the new vertices
  "
  (declare (snek snk))
  (declare (list vv))
  (if names
    (progn
      (if (not (= (length names) (length vv)))
        (error "must provide same number of verts and names"))
      (loop for xy of-type vec:vec in vv and name in names
            collect (add-vert! snk xy :p p :name name)))
    (loop for xy of-type vec:vec in vv
          collect (add-vert! snk xy :p p))))


(defun get-prm-props (snk &key p)
  (declare (snek snk))
  (prm-props (get-prm snk :p p)))


(defun get-grp-props (snk &key g)
  (declare (snek snk))
  (grp-props (get-grp snk :g g)))


(defun set-prm-props (snk v &key p)
  (declare (snek snk))
  (setf (prm-props (get-prm snk :p p)) v))


(defun set-grp-props (snk v &key g)
  (declare (snek snk))
  (setf (grp-props (get-grp snk :g g)) v))


(defun get-args (snk &key p)
  (declare (snek snk))
  (prm-args (get-prm snk :p p)))


(defun get-vert (snk v)
  "
  get the coordinate (vec) of vertex (id) v
  "
  (declare (snek snk))
  (declare (fixnum v))
  (with-struct (snek- verts num-verts) snk
    (declare (type (simple-array double-float) verts))
    (-valid-vert (num-verts v)
      (vec:sarr-get verts v))))


(defun get-verts (snk vv &aux (vv* (if (equal (type-of vv) 'cons)
                                     vv (to-list vv))))
  "
  get the coordinates (vec) of vert indices in vv
  "
  (declare (snek snk))
  (with-struct (snek- verts num-verts) snk
    (declare (type (simple-array double-float) verts))
    (-valid-verts (num-verts vv* v)
      (vec:sarr-get verts v))))


(defun get-all-verts (snk)
  "
  returns the coordinates (vec) of all vertices.
  "
  (declare (snek snk))
  (with-struct (snek- verts num-verts) snk
    (declare (type (simple-array double-float) verts))
    (loop for v of-type fixnum from 0 below num-verts
          collect (vec:sarr-get verts v))))


(defun get-all-grps (snk &key main)
  "
  returns all grps.
  "
  (declare (snek snk))
  (declare (boolean main))
  (loop for g being the hash-keys of (snek-grps snk)
        ; ignores nil (main) grp unless overridden
        if (or g main) collect g))


(defun get-grp (snk &key g)
  "
  returns a single grp.
  "
  (declare (snek snk))
  (gethash g (snek-grps snk)) )


(defun get-all-prms (snk)
  "
  returns all prms.
  "
  (declare (snek snk))
  (loop for g being the hash-keys of (snek-prms snk) collect g))


(defun get-prm (snk &key p)
  "
  get a single prm.
  "
  (declare (snek snk))
  (when (not p) (error "must provide a prm name."))
  (gethash p (snek-prms snk)))


(defun get-grp-verts (snk &key g)
  "
  returns all vertices in grp g
  "
  (declare (snek snk))
  (get-verts snk (get-vert-inds snk :g g)))


(defun get-prm-verts (snk &key p)
  "
  returns all vertices in prm p
  "
  (declare (snek snk))
  (when (not p) (error "must provide a prm name."))
  (get-verts snk (get-prm-vert-inds snk :p p)))


(defun sel-args (snk p ea)
  (if ea ea (get-args snk :p p)))


(defun prmr (snk &key p type args)
  (declare (snek snk))
  (when (not p) (error "must provide a prm name."))
  (let* ((pr (gethash p (snek-prms snk)))
         (type* (if type type (prm-type pr))))
    (when pr
      (multiple-value-bind (fxn exists)
        (gethash type* (snek-prm-names snk))
        (when (not exists) (error "trying to use undefined prm type: ~a" type*))
        (funcall fxn snk p  (snek:sel-args snk p args))))))


(defun get-prm-vert-inds (snk &key p)
  (declare (snek snk))
  (when (not p) (error "must provide a prm name."))
  (multiple-value-bind (pr exists)
    (gethash p (snek-prms snk))
    (when (not exists) (error "prm does not exist: ~a" p))
    (with-struct (prm- verts) pr
      verts)))


(defun is-vert-in-grp (snk v &key g)
  "
  tests whether v is in grp g
  "
  (declare (snek snk))
  (declare (fixnum v))
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


(defun get-vert-ind-by-name (snk &key name)
  (declare (snek snk))
  (multiple-value-bind (i exists)
    (gethash name (snek-vert-names snk))
    (when exists i)))


(defun get-vert-by-name (snk &key name)
  (declare (snek snk))
  (multiple-value-bind (i exists)
    (gethash name (snek-vert-names snk))
    (when exists (get-vert snk i))))


(defun get-verts-by-name (snk &key names)
  (declare (snek snk))
  (declare (list names))
  (loop for name in names
        collect (get-vert-by-name snk :name name)))


(defun get-vert-inds-by-name (snk &key names)
  (declare (snek snk))
  (declare (list names))
  (loop for name in names
        collect (get-vert-ind-by-name snk :name name)))


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


; TODO: get-all-incident-edges?
(defun get-incident-edges (snk v &key g)
  (declare (snek snk))
  (declare (fixnum v))
  (with-grp (snk grp g)
    (with-struct (grp- grph) grp
      (graph:get-incident-edges grph v))))


(defun add-edge! (snk ee &key g)
  "
  adds a new edge to snek. provided the edge is valid.
  otherwise it returns nil.

  returns nil if the edge exists already.
  "
  (declare (snek snk))
  (declare (list ee))
  (with-grp (snk grp g)
    (with-struct (snek- num-verts) snk
      (with-struct (grp- grph) grp
        (destructuring-bind (a b) ee
          (declare (fixnum a b))
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
  (loop for e of-type list in ee
        collect (add-edge! snk e)))


(defun del-edge! (snk ee &key g)
  (declare (snek snk))
  (declare (list ee))
  (with-grp (snk grp g)
    (with-struct (grp- grph) grp
      (destructuring-bind (a b) ee
        (declare (fixnum a b))
        (graph:del grph a b)))))


(defun verts-in-rad (snk xy rad)
  (declare (snek snk))
  (declare (vec:vec xy))
  (declare (double-float rad))
  (with-struct (snek- verts zmap) snk
    (declare (type (simple-array double-float) verts))
    (zmap:verts-in-rad zmap verts xy rad)))

