
(in-package :snek)

; ADD VERT

(defstruct (add-vert-alt (:constructor add-vert? (xy &key p))
                         (:predicate add-vert-alt-p*))
  (xy nil :type vec:vec :read-only t)
  (p nil :type symbol :read-only t))

(defun do-add-vert-alt (snk a)
  "
  add new vert at xy. returns the new vert ind.
  "
  (declare (snek snk) (add-vert-alt a))
  (with-struct (add-vert-alt- xy p) a
    (add-vert! snk xy :p p)))


; ADD EDGE

(defstruct (add-edge*-alt (:constructor add-edge*? (xya xyb &key g)))
  (xya nil :type vec:vec :read-only t)
  (xyb nil :type vec:vec :read-only t)
  (g nil :type symbol :read-only t))

(defun do-add-edge*-alt (snk a)
  "
  add verts xya and xyb, and creates an edge (in grp g) between them.
  "
  (declare (snek snk) (add-edge*-alt a))
  (with-struct (add-edge*-alt- xya xyb g) a
    (add-edge! snk (list (add-vert! snk xya) (add-vert! snk xyb))
               :g g)))


; MOVE VERT

(defstruct (move-vert-alt (:constructor move-vert? (v xy &key (rel t))))
  (rel t :type boolean :read-only t)
  (xy nil :type vec:vec :read-only t)
  (v nil :type fixnum :read-only t))


(defun do-move-vert-alt (snk a)
  "
  move vert v.
    if rel: move relative to original position.
    else: move to xy.
  "
  (declare (snek snk) (move-vert-alt a))
  (with-struct (snek- verts num-verts) snk
    (declare (type (simple-array double-float) verts))
    (with-struct (move-vert-alt- v xy rel) a
      (declare (fixnum v) (vec:vec xy) (boolean rel))
      (-valid-vert (num-verts v :err nil)
        (let ((x (vec::vec-x xy))
              (y (vec::vec-y xy))
              (i (* 2 v))
              (ii (1+ (* 2 v))))
          (declare (double-float x y) (fixnum i ii))
          (if rel (setf (aref verts i) (+ (aref verts i) x)
                        (aref verts ii) (+ (aref verts ii) y))
                  (setf (aref verts i) x (aref verts ii) y))
          (vec:vec (aref verts i) (aref verts ii)))))))


; APPEND EDGE

(defstruct (append-edge-alt (:constructor append-edge? (v xy &key (rel t) g)))
  (xy nil :type vec:vec :read-only t)
  (v nil :type fixnum :read-only t)
  (g nil :type symbol :read-only t)
  (rel t :type boolean :read-only t))

(defun do-append-edge-alt (snk a)
  "
  add edge between vert v and new vert xy
  "
  (declare (snek snk) (append-edge-alt a))
  (with-struct (snek- num-verts) snk
    (with-struct (append-edge-alt- v xy rel g) a
      (-valid-vert (num-verts v :err nil)
        (let ((w (if rel (add-vert! snk (vec:add (get-vert snk v) xy))
                         (add-vert! snk xy))))
          (declare (fixnum w))
          (add-edge! snk (list v w) :g g)
          w)))))


(defstruct (append-edge-segx-alt
    (:constructor append-edge-segx? (v xy &key (rel t) x g)))
  (xy nil :type vec:vec :read-only t)
  (v nil :type fixnum :read-only t)
  (g nil :type symbol :read-only t)
  (x nil :type symbol :read-only t)
  (rel t :type boolean :read-only t))


(defun -line-segx-edges (snk line &key g)
  (declare (snek snk) (list line))
  (loop for e of-type list across (get-edges snk :g g)
        if (multiple-value-bind (x s)
             (vec:segx line (get-verts snk e))
             (and x (> s 1d-7)))
        do (return t)))

(defun do-append-edge-segx-alt (snk a)
  "
  add edge between vert v and new vert xy, if the new edge does not intersect
  any of the existing edges (in grp g).

  if x, the new edge is only added if it intersects (the converse).
  "
  (declare (snek snk) (append-edge-segx-alt a))
  (with-struct (append-edge-segx-alt- v xy rel x g) a
    (let* ((p1 (get-vert snk v))
           (p2 (if rel (vec:add p1 xy) xy))
           (hit (-line-segx-edges snk (list p1 p2) :g g)))
      (when (or (and (not x) (not hit))
                (and x hit))
        (add-edge! snk (list v (add-vert! snk p2)) :g g)))))


; JOIN VERTS

(defstruct (join-verts-alt (:constructor join-verts? (v w &key g)))
  (v nil :type fixnum :read-only t)
  (w nil :type fixnum :read-only t)
  (g nil :type symbol :read-only t))


(defun do-join-verts-alt (snk a)
  "
  create edge between valid verts v and w (in grp g).
  "
  (declare (snek snk) (join-verts-alt a))
  (with-struct (snek- num-verts) snk
    (with-struct (join-verts-alt- v w g) a
      (-valid-vert (num-verts v :err nil)
        (-valid-vert (num-verts w :err nil)
          (add-edge! snk (list v w) :g g))))))


; DEL EDGE

(defstruct (del-edge-alt (:constructor del-edge? (e &key g)))
  (e nil :type list :read-only t)
  (g nil :type symbol :read-only t))

(defun do-del-edge-alt (snk a)
  "
  del edge e (of grp g).
  "
  (declare (snek snk) (del-edge-alt a))
  (with-struct (del-edge-alt- e g) a
    (del-edge! snk e :g g)))


; SPLIT EDGE
(defstruct (split-edge-alt (:constructor split-edge? (e &key xy g)))
  (e nil :type list :read-only t)
  (xy nil :read-only t)
  (g nil :type symbol :read-only t))

(defun do-split-edge-alt (snk a)
  "
  insert a vert, v, at the middle of edge e = (a b)
  if xy is not nil v will be positioned at xy.
  such that we get edges (a v) and (v b).

  returns the new edges (or nil).
  "
  (declare (snek snk) (split-edge-alt a))
  (with-struct (split-edge-alt- e xy g) a
    (let ((res (del-edge! snk e :g g))
          (verts (snek-verts snk)))
      (declare (type (simple-array double-float) verts))
      (when res
        (destructuring-bind (a b) e
          (declare (fixnum a b))
          (let ((c (add-vert! snk (if xy xy (vec:mid (vec:sarr-get verts a)
                                                     (vec:sarr-get verts b))))))
            (list (add-edge! snk (list a c) :g g)
                  (add-edge! snk (list c b) :g g))))))))


; ALT THEN

(defstruct (alt-then (:constructor
    alt-then? (alt &key (then (lambda (a r) (list a r)))
                        (else #'identity))))
  (alt nil :read-only t)
  (then nil :type function :read-only t)
  (else nil :type function :read-only t))

(defun do-alt-then (snk a)
  "
  execute then if alteration is successful, otherwise execute else.
    then gets the alteration and its result as arguments.
    else gets the alteration as its argument.
  "
  (declare (snek snk) (alt-then a))
  (with-struct (alt-then- alt then else) a
    (declare (function then else))
    (let ((r (funcall (gethash (type-of alt) (snek-alt-names snk)) snk alt)))
      (if r (funcall then alt r)
            (funcall else alt)))))

