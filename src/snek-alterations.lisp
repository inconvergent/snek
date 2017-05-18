
(defun do-alts (alts snk)
  (let ((alt-names (snek-alt-names snk)))
    (dolist (a alts)
      (funcall (gethash (type-of a) alt-names) snk a))))


; ADD VERT

(defstruct (add-vert-alt
    (:constructor add-vert? (xy &optional g)))
  (xy nil :type list :read-only t)
  (g nil :type symbol :read-only t))


(defun do-add-vert-alt (snk a)
  (with-struct (add-vert-alt- xy g) a
    (add-vert! snk xy :g g)))


; ADD EDGE

(defstruct (add-edge-alt
    (:constructor add-edge? (e &optional g)))
  (e nil :type list :read-only t)
  (g nil :type symbol :read-only t))


(defun do-add-edge-alt (snk a)
  (with-struct (add-edge-alt- e g) a
    (add-edge! snk e :g g)))


; ADD EDGE

(defstruct (add-edge*-alt
    (:constructor add-edge*? (xya xyb &optional g)))
  (xya nil :type list :read-only t)
  (xyb nil :type list :read-only t)
  (g nil :type symbol :read-only t))


(defun do-add-edge*-alt (snk a)
  (with-struct (add-edge*-alt- xya xyb g) a
    (add-edge! snk
               (list (add-vert! snk xya :g g)
                     (add-vert! snk xyb :g g))
               :g g)))


; MOVE VERT

(defstruct (move-vert-alt
    (:constructor move-vert? (v xy &key (rel t))))
  (rel t :type boolean :read-only t)
  (xy nil :type list :read-only t)
  (v nil :type integer :read-only t))


(defun do-move-vert-alt (snk a)
  (with-struct (snek- verts num-verts) snk
    (with-struct (move-vert-alt- v xy rel) a
      (-valid-vert (num-verts v :err nil)
        (let ((fxy (to-dfloat* xy)))
          (set-from-list
            verts
            v
            (if rel
              (add (get-as-list verts v) fxy)
              fxy)))))))


; TODO: consider making macros similar to this instead of current
;       alteration constructors?
;(defmacro move-grp ((snk g &key (rel t)) &body body)
;  (with-gensyms (v)
;  `(itr-verts (,snk ,v :g ,g)
;    (move-vert? ,v ,@body :rel ,rel))))


; APPEND EDGE

(defstruct (append-edge-alt
    (:constructor append-edge? (v xy &key (rel t))))
  (xy nil :type list :read-only t)
  (v nil :type integer :read-only t)
  (rel t :type boolean :read-only t))


(defun do-append-edge-alt (snk a)
  (with-struct (snek- num-verts) snk
    (with-struct (append-edge-alt- v xy rel) a
      (-valid-vert (num-verts v :err nil)
        (let ((g (get-vert-grp snk v)))
          (let ((w (if rel
                         (add-vert! snk (add (get-vert snk v) xy) :g g)
                         (add-vert! snk xy :g g))))
            (add-edge! snk (list v w) :g g)
            w))))))


; JOIN VERTS

(defstruct (join-verts-alt
    (:constructor join-verts? (v w)))
  (v nil :type integer :read-only t)
  (w nil :type integer :read-only t))


(defun do-join-verts-alt (snk a)
  (with-struct (snek- num-verts) snk
    (with-struct (join-verts-alt- v w) a
      (-valid-vert (num-verts v :err nil)
        (-valid-vert (num-verts w :err nil)
          (let ((ga (get-vert-grp snk v))
                (gb (get-vert-grp snk w)))
            (add-edge!
              snk
              (list v w))
              :g (val-if-eql ga gb)))))))


; SPLIT EDGE

(defstruct (split-edge-alt
    (:constructor split-edge? (e)))
  (e nil :type list :read-only t))


(defun do-split-edge-alt (snk a)
  (with-struct (split-edge-alt- e) a
    (let ((res (del-edge! snk e))
          (verts (snek-verts snk)))
      (destructuring-bind (a b) e
        (if (> res 1)
          ; TODO: improve this
          (let ((ga (get-vert-grp snk a))
                (gb (get-vert-grp snk b)))
            (let ((g (val-if-eql ga gb)))
              (let ((c (add-vert! snk
                          (mid (get-as-list verts a)
                               (get-as-list verts b))
                          :g g)))
                (add-edge! snk (list a c) :g g)
                (add-edge! snk (list c b) :g g)))))))))


(defun -get-force-alterations (u v f)
  (list
    (move-vert? v f)
    (move-vert? u (scale f -1.0d0))))


(defmacro force? (snk v1 v2 r)
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

