
; TODO: make an actual package?
;(defpackage :snek (:use :common-lisp))


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
    (setf (gethash 'add-edge-alt alt-names) 'do-add-edge-alt
          (gethash 'add-vert-alt alt-names) 'do-add-vert-alt
          (gethash 'move-vert-alt alt-names) 'do-move-vert-alt
          (gethash 'join-verts-alt alt-names) 'do-join-verts-alt
          (gethash 'append-edge-alt alt-names) 'do-append-edge-alt
          (gethash 'split-edge-alt alt-names) 'do-split-edge-alt
          (gethash 'add-edge*-alt alt-names) 'do-add-edge*-alt)

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

      (setf (gethash nil (snek-grps snk)) main-grp
            (snek-main-grp snk) main-grp)
      snk)))

