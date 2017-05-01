
(defun do-alts (alts snk)
  (let ((alt-names (snek-alt-names snk)))
    (dolist (a alts)
      (funcall (gethash (type-of a) alt-names) snk a))))


; MOVE VERT

(defstruct (move-vert-alt
    (:constructor move-vert (v xy &key (rel t))))
  (rel t :type boolean :read-only t)
  (xy nil :type list :read-only t)
  (v nil :type integer :read-only t))


(defun do-move-vert-alt (snk a)
  (let ((verts (snek-verts snk)))
    (with-struct (move-vert-alt- v xy rel) a
      (let ((fxy (to-dfloat* xy)))
        (set-from-list
          verts
          v
          (if rel
            (add (get-as-list verts v) fxy)
            fxy))))))


; TODO: consider making macros similar to this instead of current
;       alteration constructors?
;(defmacro move-grp ((snk g &key (rel t)) &body body)
;  (with-gensyms (v)
;  `(itr-verts (,snk ,v :g ,g)
;    (move-vert ,v ,@body :rel ,rel))))


; APPEND EDGE

(defstruct (append-edge-alt
    (:constructor append-edge (v xy &key (rel t))))
  (xy nil :type list :read-only t)
  (v nil :type integer :read-only t)
  (rel t :type boolean :read-only t))


(defun do-append-edge-alt (snk a)
  (with-struct (append-edge-alt- v xy rel) a
    (let ((g (get-vert-grp snk v)))
      (if rel
        (add-vert snk (add (get-vert snk v) xy) :g g)
        (add-vert snk xy :g g))
      (add-edge snk
        (list
          v
          (1- (snek-num-verts snk)))
        :g g))))


; JOIN VERTS

(defstruct (join-verts-alt
    (:constructor join-verts (v1 v2)))
  (v1 nil :type integer :read-only t)
  (v2 nil :type integer :read-only t))


(defun do-join-verts-alt (snk a)
  (with-struct (join-verts-alt- v1 v2) a
    (let ((g1 (get-vert-grp snk v1))
          (g2 (get-vert-grp snk v2)))
      (add-edge
        snk
        (list v1 v2))
        :g (val-if-eql g1 g2))))


; SPLIT EDGE

(defstruct (split-edge-alt
    (:constructor split-edge (e)))
  (e nil :type list :read-only t))


(defun do-split-edge-alt (snk a)
  (with-struct (split-edge-alt- e) a
    (let ((res (remove-edge snk e))
          (verts (snek-verts snk)))
      (destructuring-bind (a b) e
        (if (> res 1)
          ; TODO: improve this
          (let ((ga (get-vert-grp snk a))
                (gb (get-vert-grp snk b)))
            (let ((g (val-if-eql ga gb)))
              (let ((c (add-vert snk
                          (mid (get-as-list verts a)
                               (get-as-list verts b))
                          :g g)))
                (add-edge snk (list a c) :g g)
                (add-edge snk (list c b) :g g)))))))))


(defun -get-force-alterations (u v f)
  (list
    (move-vert v f)
    (move-vert u (scale f -1.0d0))))


(defmacro force (snk v1 v2 r)
  (with-gensyms (vname v1name v2name rname)
    `(let ((,vname (snek-verts ,snk))
           (,v1name ,v1)
           (,v2name ,v2)
           (,rname ,r))
      (-get-force-alterations
        ,v1 ,v2
        (scale
          (nsub
            (get-as-list ,vname ,v1name)
            (get-as-list ,vname ,v2name))
          ,rname)))))

