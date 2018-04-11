
(in-package :snek)

; ADD VERT

(defstruct (add-vert-alt
    (:constructor add-vert? (xy &key p)))
  (xy nil :type vec:vec :read-only t)
  (p nil :type symbol :read-only t))


(defun do-add-vert-alt (snk a)
  "
  add vert at xy.
  "
  (with-struct (add-vert-alt- xy p) a
    (add-vert! snk xy :p p)))


; ADD EDGE

(defstruct (add-edge*-alt
    (:constructor add-edge*? (xya xyb &key g)))
  (xya nil :type vec:vec :read-only t)
  (xyb nil :type vec:vec :read-only t)
  (g nil :type symbol :read-only t))


(defun do-add-edge*-alt (snk a)
  "
  add verts xya and xyb, and create an edge between them.
  "
  (with-struct (add-edge*-alt- xya xyb g) a
    (add-edge! snk (list (add-vert! snk xya)
                         (add-vert! snk xyb))
               :g g)))


; MOVE VERT

(defstruct (move-vert-alt
    (:constructor move-vert? (v xy &key (rel t))))
  (rel t :type boolean :read-only t)
  (xy nil :type vec:vec :read-only t)
  (v nil :type integer :read-only t))


(defun do-move-vert-alt (snk a)
  "
  move vert v.
  if rel: move relative to original position.
  else: move to xy.
  "
  (with-struct (snek- verts num-verts) snk
    (declare (type (array double-float) verts))
    (with-struct (move-vert-alt- v xy rel) a
      (-valid-vert (num-verts v :err nil)
        (let ((fxy (if rel (vec:add (vec:arr-get verts v) xy) xy)))
          (setf (aref verts v 0) (vec::vec-x fxy)
                (aref verts v 1) (vec::vec-y fxy))
          fxy)))))


; APPEND EDGE

(defstruct (append-edge-alt
    (:constructor append-edge? (v xy &key (rel t) g)))
  (xy nil :type vec:vec :read-only t)
  (v nil :type integer :read-only t)
  (g nil :type symbol :read-only t)
  (rel t :type boolean :read-only t))


(defun do-append-edge-alt (snk a)
  "

  "
  (with-struct (snek- num-verts) snk
    (with-struct (append-edge-alt- v xy rel g) a
      (-valid-vert (num-verts v :err nil)
        (let ((w (if rel (add-vert! snk (vec:add (get-vert snk v) xy))
                         (add-vert! snk xy))))
          (declare (integer w))
          (add-edge! snk (list v w) :g g)
          w)))))


; JOIN VERTS

(defstruct (join-verts-alt
    (:constructor join-verts? (v w &key g)))
  (v nil :type integer :read-only t)
  (w nil :type integer :read-only t)
  (g nil :type symbol :read-only t))


(defun do-join-verts-alt (snk a)
  "
  create edge between valid verts v and w.
  "
  (with-struct (snek- num-verts) snk
    (with-struct (join-verts-alt- v w g) a
      (-valid-vert (num-verts v :err nil)
        (-valid-vert (num-verts w :err nil)
          (add-edge! snk (list v w) :g g))))))


; SPLIT EDGE

(defstruct (split-edge-alt
    (:constructor split-edge? (e &key g)))
  (e nil :type list :read-only t)
  (g nil :type symbol :read-only t))


(defun do-split-edge-alt (snk a)
  "
  insert a vert, v, at the middle of edge e = (a b)
  such that we get edges (a v) and (v b).
  "
  (with-struct (split-edge-alt- e g) a
    (let ((res (del-edge! snk e :g g))
          (verts (snek-verts snk)))
      (declare (type (array double-float) verts))
      (destructuring-bind (a b) e
        (declare (integer a b))
        (if res
          (let ((c (add-vert! snk
                      (vec:mid (vec:arr-get verts a)
                               (vec:arr-get verts b)))))
            (add-edge! snk (list a c) :g g)
            (add-edge! snk (list c b) :g g)))))))

