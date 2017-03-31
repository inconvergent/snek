
; TODO: make an actual package?
(defpackage :snek (:use :common-lisp))


(defstruct (snek (:constructor -make-snek))
  (wc 0 :type integer)
  (verts nil)
  (num-verts 0 :type integer)
  (zmap nil)
  (zwidth nil)
  (main-grp nil)
  (grps (make-hash-table :test #'equal))
  (vert-to-grp nil)
  (alt-names nil :read-only t)
  (max-verts nil :type integer :read-only t)
  (max-main-grp-edges nil :type integer :read-only t)
  (max-grp-edges nil :type integer :read-only t))


(defstruct grp
  (name nil)
  (edges nil)
  (verts (make-vec))
  (num-edges 0 :type integer)
  (num-verts 0 :type integer)
  (type nil :type symbol :read-only t)
  (closed nil :read-only t))


(defun new-grp (snk &key (type nil) (closed nil))
  "
  constructor for grp instances.

  at the moment type and closed are not used for anything in particular, but
  they are there to indicate what kind of a group it is. eventually they will
  be used to indicate that a grp is eg a path, or a NURBS etc.
  "
  (with-struct (snek- max-grp-edges grps) snk
    ; TODO: probably not use gensym. that won't work when exporting structures?
    (let ((name (gensym)))
      (setf
        (gethash name grps)
        (make-grp
          :name name
          :closed closed
          :type type
          :edges (make-int-array max-grp-edges)))
      name)))


(defun make-snek (&key (max-verts 100000)
                       (max-main-grp-edges 100000)
                       (max-grp-edges 1000)
                       (alts))
  "
  constructor for snek instances.

  - max-verts is the maximum number of verts in snek (across all grps).
  - max-main-grp-edges is the max number of edges in the main grp
  - max-grp-edges is the max allowed number of edges in a group

  - alts is a list of tuples: ((alt-x 'do-alt-x) (alt-y 'do-alt-y))
    where alt-x is the name of an alteration struct and do-alt-x is the name of
    a function that applies alt-x to snek. see snek-alterations for examples.
  "
  (let ((alt-names (make-hash-table :test #'equal)))

    (dolist (a '(move-vert-alt join-verts-alt append-edge-alt split-edge-alt))
      (setf (gethash a alt-names) (symb 'do- a)))

    (dolist (pair alts)
      (destructuring-bind (a f)
        pair
        (setf (gethash a alt-names) f)))

    (let ((snk (-make-snek
                 :verts (make-dfloat-array max-verts)
                 :vert-to-grp (make-symb-array max-verts)
                 :alt-names alt-names
                 :max-verts max-verts
                 :max-main-grp-edges max-main-grp-edges
                 :max-grp-edges max-grp-edges))
          (main-grp (make-grp
                      :name 'main
                      :type 'main
                      :edges (make-int-array max-main-grp-edges))))

      (setf (gethash nil (snek-grps snk)) main-grp)
      (setf (snek-main-grp snk) main-grp)
      snk)))


(defun -edge-compare (a b c d)
  (or
    (and (>= a b) (>= c d))
    (> a b)))


(defun -binary-edge-insert-search (arr target num)
  (let ((left 0)
        (right (1- num)))
    (do () ((< right left) left)
      (let ((mid (floor (+ left right) 2)))
        (cond
          ((not (-edge-compare
                  (first target)
                  (aref arr mid 0)
                  (second target)
                  (aref arr mid 1)))
            (setf right (1- mid)))
          (t
            (setf left (1+ mid))))))))


;TODO: is this tail recursive?
(defun -binary-edge-search (arr target num &key (left 0) (right nil))
  (destructuring-bind (a c) target
    (if (eql right nil)
      (setf right (1- num)))
    (let ((mid (floor (+ left right) 2)))
        (cond
          ((< right left) nil)
          ((and
             (eql a (aref arr mid 0))
             (eql c (aref arr mid 1)))
            mid)
          ((not (-edge-compare a (aref arr mid 0) c (aref arr mid 1)))
            (-binary-edge-search arr target num :left left
                                                :right (1- mid)))
          (t (-binary-edge-search arr target num :left (1+ mid)
                                                 :right right))))))


(defun -insert-edge (edges edge pos num)
  (loop for i from 0 below (- num pos) do
    (let ((left (- num (1+ i)))
          (right (- num i)))
      (setf (aref edges right 0) (aref edges left 0))
      (setf (aref edges right 1) (aref edges left 1))))
    (setf (aref edges pos 0) (first edge))
    (setf (aref edges pos 1) (second edge)))


(defun -find-insert-edge (edges num e)
  (-insert-edge
    edges
    e
    (-binary-edge-insert-search edges e num)
    num)
  e)


(defun -remove-edge (edges pos num)
  (loop for i from pos to (- num 2) do
    (setf (aref edges i 0) (aref edges (1+ i) 0))
    (setf (aref edges i 1) (aref edges (1+ i) 1)))
  (set-from-list edges (1- num) (list 0 0)))


(defun -find-remove-edge (edges num e)
  (let ((p (-binary-edge-search edges e num)))
    (if p
      (progn
        (-remove-edge edges p num)
        1)
      0)))


(defun -add-vert-to-grp (v vert-to-grp grps g)
  (setf (aref vert-to-grp v) g)
  (multiple-value-bind (grp exists)
    (gethash g grps)
    (if exists
      (with-struct (grp- verts) grp
        (vector-push-extend v verts)
        (incf (grp-num-verts grp))))))


(defun insert-vert (snk xy &key g)
  (with-struct (snek- verts vert-to-grp grps num-verts) snk
    (-add-vert-to-grp num-verts vert-to-grp grps g)
    (destructuring-bind (x y)
      (to-dfloat* xy)
      (setf (aref verts num-verts 0) x)
      (setf (aref verts num-verts 1) y)
      (- (incf (snek-num-verts snk)) 1))))


(defun insert-verts (snk vv &key g)
  (loop for xy in vv collect
    (insert-vert snk xy :g g)))


(defun get-vert (snk v)
  (with-struct (snek- verts) snk
    (list (aref verts v 0)
          (aref verts v 1))))


(defun get-vert-grp (snk v)
  (with-struct (snek- vert-to-grp) snk
    (aref vert-to-grp v)))


(defun get-verts (snk vv)
  (let ((verts (snek-verts snk)))
    (mapcar (lambda (v) (get-as-list verts v)) vv)))


; TODO: more efficient?
(defun get-grp-vert-vals (snk g)
  (get-verts snk
    (get-grp-verts snk g)))


; TODO
;(defun get-edge-grp)


(defun get-grp-verts (snk g)
  (with-struct (snek- grps) snk
    (multiple-value-bind (grp exists)
      (gethash g grps)
      (if exists
        (to-list (grp-verts grp))
        nil))))


(defun get-num-edges (snk &key g)
  (with-grp (snk grp g)
    (grp-num-edges grp)))


(defun get-edges (snk &key g)
  (with-grp (snk grp g)
    (with-struct (grp- edges num-edges) grp
      (loop for i from 0 below num-edges collect
        (get-as-list edges i)))))


(defun get-edge-arr (snk &key g)
  (grp-edges (gethash g (snek-grps snk))))


(defun insert-edge (snk ee &key g)
  (with-grp (snk grp g)
    (with-struct (grp- edges num-edges) grp
      (destructuring-bind (a b)
        ee
        (cond
          ((-binary-edge-search edges ee num-edges) nil)
          ((eql a b) nil)
          (t
            (setf (grp-num-edges grp) (2+ num-edges))
            (-find-insert-edge edges num-edges ee)
            (sort
              (-find-insert-edge edges (1+ num-edges) (reverse ee))
              #'<)))))))


(defun remove-edge (snk ee &key g)
  (with-grp (snk grp g)
    (with-struct (grp- edges num-edges) grp
      (setf (grp-num-edges grp)
            (- num-edges
              (loop for i in
                (list
                  (-find-remove-edge edges num-edges ee)
                  (-find-remove-edge edges
                                     (1- num-edges)
                                     (reverse ee)))
                sum i)))
      (- num-edges (grp-num-edges grp)))))


; TODO: binary search
(defun get-one-ring (snk v &key g)
  "
  returns all edges connected to vert v.

  if a grp g is supplied, it will select edges from g, otherwise
  it will use the main grp.
  "
  (with-grp (snk grp g)
    (with-struct (grp- edges num-edges) grp
      (loop for i from 0 below num-edges
        if (eql v (aref edges i 0))
        collect (get-as-list edges i)))))

