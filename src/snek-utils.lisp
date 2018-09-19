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

  use :type :closed to create a closed graph, which allows looking up a closed
  loop.

  the grp functionality is still experimental.
  "
  (declare (snek snk))
  (with-struct (snek- grps grp-size) snk
    (multiple-value-bind (v exists) (gethash name* grps)
      (declare (ignore v) (boolean exists))
      (when exists (error "grp name already exists: ~a" name*)))
    (setf (gethash name* grps) (-make-grp
                                 :name name*
                                 :type type
                                 :grph (graph:make :size grp-size
                                                   :closed (equal type :closed))
                                 :props props)))
  name*)


(defun add-prm! (snk &key name type props args
                     &aux (name* (if name name (gensym))))
  "
  constructor for prms (primitives).

  prms are generic entities of a particular type. eg. a primitive is a path, a
  bzspl or a circle.

  nil is not an allowed prm name. if name is nil, the name will be a gensym.

  the default type is nil.

  a given prm can be 'rendered' using (snek:prmr snk :p p). the behaviour of
  (snek:prmr...) is determined by the rfxn functions. these functions are
  either the default function or whatever functions are provided by to the
  :prms when creating a snek instance

  the prm functionality is experimental.
  "
  (declare (snek snk))
  (with-struct (snek- prms) snk
    (multiple-value-bind (v exists) (gethash name* prms)
      (declare (ignore v) (boolean exists))
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
  returns the new vert ind.
  "
  (declare (snek snk) (vec:vec xy))
  (with-struct (snek- verts num-verts) snk
    (declare (type (simple-array double-float) verts))
    (setf (aref verts (* 2 num-verts)) (vec::vec-x xy)
          (aref verts (1+ (* 2 num-verts))) (vec::vec-y xy)))

  (let ((i (1- (incf (snek-num-verts snk)))))
    (when name
      (multiple-value-bind (v exists) (gethash name (snek-vert-names snk))
        (declare (ignore v) (boolean exists))
        (when exists (error "vert name already exists: ~a" name))
        (setf (gethash name (snek-vert-names snk)) i)))
    (when p (with-struct (prm- verts) (gethash p (snek-prms snk))
              (vextend i verts)
              (incf (prm-num-verts (gethash p (snek-prms snk))))))
    i))


(defun add-verts! (snk vv &key p names)
  "
  adds new vertices to snek
  returns the ids of the new vertices
  "
  (declare (snek snk) (list vv names))
  (if names
    (progn (if (not (= (length names) (length vv)))
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


(defun get-grp-loop (snk &key g)
  (declare (snek snk))
  (with-grp (snk g* g)
    (unless (equal (grp-type g*) :closed)
      (error "error in get-grp-loop: grp is not of type :closed."))
    (graph:get-loop (grp-grph g*))))


(defun set-prm-props! (snk v &key p)
  (declare (snek snk))
  (setf (prm-props (get-prm snk :p p)) v))


(defun set-grp-props! (snk v &key g)
  (declare (snek snk))
  (setf (grp-props (get-grp snk :g g)) v))


(defun get-args (snk &key p)
  (declare (snek snk))
  (prm-args (get-prm snk :p p)))


(defun get-vert (snk v)
  "
  get the coordinate (vec) of vert v.
  "
  (declare (snek snk) (fixnum v))
  (with-struct (snek- verts num-verts) snk
    (declare (type (simple-array double-float) verts) (fixnum num-verts))
    (-valid-vert (num-verts v)
      (vec:sarr-get verts v))))


(defun get-verts (snk vv &aux (vv* (if (equal (type-of vv) 'cons)
                                       vv (to-list vv))))
  "
  get the coordinates (vec) of verts in vv
  "
  (declare (snek snk) (sequence vv))
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


(defun move-vert! (snk v xy &key (rel t) &aux (v* (* 2 v)))
  (declare (snek snk) (fixnum v) (vec:vec xy) (boolean rel))
  (with-struct (snek- verts num-verts) snk
    (when (>= v num-verts)
          (error "attempting to move invalid vert, ~a (~a)" v num-verts))
    (if rel (setf (aref verts v*) (+ (vec::vec-x xy) (aref verts v*))
                  (aref verts (1+ v*)) (+ (vec::vec-y xy) (aref verts (1+ v*))))
            (setf (aref verts v*) (vec::vec-x xy)
                  (aref verts (1+ v*)) (vec::vec-y xy)))))


(defun get-all-grps (snk &key main)
  "
  returns all grps. use :main t to include main/nil grp.
  "
  (declare (snek snk) (boolean main))
  (loop for g being the hash-keys of (snek-grps snk)
        ; ignores nil (main) grp unless overridden
        if (or g main) collect g))


(defun get-grp (snk &key g)
  "
  returns the grp g. if g is not provided, the main/nil grp will be returned.
  "
  (declare (snek snk))
  (gethash g (snek-grps snk)))


(defun get-all-prms (snk)
  "
  returns all prms.
  "
  (declare (snek snk))
  (loop for g being the hash-keys of (snek-prms snk) collect g))


(defun get-prm (snk &key p)
  "
  get a single prm. there is no nil prm.
  "
  (declare (snek snk))
  (when (not p) (error "must provide a prm name."))
  (gethash p (snek-prms snk)))


(defun get-grp-verts (snk &key g)
  "
  returns all vertices in grp g.
  "
  (declare (snek snk))
  (get-verts snk (get-vert-inds snk :g g)))


(defun get-prm-verts (snk &key p)
  "
  returns all vertices in prm p.
  "
  (declare (snek snk))
  (when (not p) (error "must provide a prm name."))
  (get-verts snk (get-prm-vert-inds snk :p p)))


(defun sel-args (snk p ea)
  (if ea ea (get-args snk :p p)))


(defun prmr (snk &key p type args)
  (declare (snek snk))
  (when (not p) (error "must provide a prm name."))
  (let* ((p* (gethash p (snek-prms snk)))
         (type* (if type type (prm-type p*))))
    (when p*
      (multiple-value-bind (fxn exists) (gethash type* (snek-prm-names snk))
        (when (not exists) (error "trying to use undefined prm type: ~a" type*))
        (funcall (the function fxn) snk p  (snek:sel-args snk p args))))))


(defun get-prm-vert-inds (snk &key p)
  (declare (snek snk))
  (when (not p) (error "must provide a prm name."))
  (multiple-value-bind (p* exists) (gethash p (snek-prms snk))
    (when (not exists) (error "prm does not exist: ~a" p))
    (prm-verts p*)))


(defun is-vert-in-grp (snk v &key g)
  "
  tests whether v is in grp g
  "
  (declare (snek snk) (fixnum v))
  (with-struct (snek- grps) snk
    (multiple-value-bind (g* exists) (gethash g grps)
      (if exists (graph:vmem (grp-grph g*) v)
                 (error "grp does not exist: ~a" g)))))


(defun get-vert-inds (snk &key g)
  "
  returns all vertex indices that belongs to a grp
  "
  (declare (snek snk))
  (with-struct (snek- grps) snk
    (multiple-value-bind (g* exists) (gethash g grps)
      (if exists (graph:get-verts (grp-grph g*))
                 (error "grp does not exist: ~a" g)))))


(defun get-vert-ind-by-name (snk &key name)
  (declare (snek snk))
  (multiple-value-bind (i exists) (gethash name (snek-vert-names snk))
    (when exists i)))


(defun get-vert-by-name (snk &key name)
  (declare (snek snk))
  (multiple-value-bind (i exists) (gethash name (snek-vert-names snk))
    (when exists (get-vert snk i))))


(defun get-verts-by-name (snk &key names)
  (declare (snek snk) (list names))
  (loop for name in names collect (get-vert-by-name snk :name name)))


(defun get-vert-inds-by-name (snk &key names)
  (declare (snek snk) (list names))
  (loop for name in names collect (get-vert-ind-by-name snk :name name)))


(defun get-num-edges (snk &key g)
  (declare (snek snk))
  (with-grp (snk g* g)
    (graph:get-num-edges (grp-grph g*))))


; TODO: option to include both directions?
(defun get-edges (snk &key g)
  (declare (snek snk))
  (with-grp (snk g* g)
    (graph:get-edges (grp-grph g*))))


; TODO: get-all-incident-edges (not just in grp g)?
(defun get-incident-edges (snk v &key g)
  (declare (snek snk) (fixnum v))
  (with-grp (snk g* g)
    (graph:get-incident-edges (grp-grph g*) v)))


(defun add-edge! (snk ee &key g)
  "
  adds a new edge to snek. provided the edge is valid.
  otherwise it returns nil.

  returns nil if the edge exists already.
  "
  (declare (snek snk) (list ee))
  (with-grp (snk g* g)
    (with-struct (snek- num-verts) snk
      (declare (fixnum num-verts))
      (with-struct (grp- grph) g*
        (destructuring-bind (a b) ee
          (declare (fixnum a b))
          (when (and (< a num-verts) (< b num-verts) (not (eql a b)))
            (when (graph:add grph a b) (sort (list a b) #'<))))))))


(defun add-edges! (snk ee &key g)
  "
  adds multiple edges (see above). returns a list of the results.
  "
  (declare (snek snk) (list ee))
  (loop for e of-type list in ee collect (add-edge! snk e :g g)))


(defun del-edge! (snk ee &key g)
  (declare (snek snk) (list ee))
  (with-grp (snk g* g)
    (with-struct (grp- grph) g*
      (destructuring-bind (a b) ee
        (declare (fixnum a b))
        (graph:del grph a b)))))


(defun split-edge! (snk ee &key xy g
                           &aux (xy* (if xy xy (vec:on-line* 0.5d0
                                                 (snek:get-verts snk ee)))))
  "
  split edge at xy (or middle if xy is nil).
  returns new vert ind (and new edges).
  "
  (declare (snek snk) (list ee))
  (snek:del-edge! snk ee :g g)
  (let ((v (add-vert! snk xy*)))
    (declare (fixnum v))
    (destructuring-bind (a b) ee
      (declare (fixnum a b))
      (let ((edges (list (snek:add-edge! snk (list v a) :g g)
                         (snek:add-edge! snk (list v b) :g g))))
        (declare (list edges))
        (values v edges)))))


(defun verts-in-rad (snk xy rad)
  (declare (snek snk) (vec:vec xy) (double-float rad))
  (with-struct (snek- verts zmap) snk
    (declare (type (simple-array double-float) verts))
    (zmap:verts-in-rad zmap verts xy rad)))

