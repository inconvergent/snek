
(in-package :snek)


(defstruct (snek (:constructor -make-snek))
  (wc 0 :type integer)
  (verts nil :type array)
  (num-verts 0 :type integer)
  (zmap nil)
  (grps (make-hash-table :test #'eql))
  (alt-names nil :read-only t)
  (max-verts nil :type integer :read-only t)
  (grp-size nil :type integer :read-only t))


(defstruct grp
  (name nil :type symbol)
  (grph nil :type graph::graph)
  (type nil :type symbol :read-only t)
  (closed nil :type boolean :read-only t))

(defun make (&key (max-verts 100000) (grp-size 100) alts)
  "
  constructor for snek instances.

  - max-verts is the maximum number of verts in snek (across all grps).

  - alts is a list of tuples: (('alt-x #'do-alt-x) ('alt-y #'do-alt-y))
    where alt-x is the name of an alteration struct and do-alt-x is the name of
    a function that applies alt-x to snek. see snek-alterations for examples.
  "
  (let ((alt-names (make-hash-table :test #'eql)))
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
        (setf (gethash a alt-names) f)
        (format t "init alt: ~a~%" a)))

    (let ((snk (-make-snek
                 :verts (make-dfloat-array max-verts)
                 :alt-names alt-names
                 :grp-size grp-size
                 :max-verts max-verts)))

      (setf (gethash nil (snek-grps snk))
            (make-grp
              :name 'main
              :closed nil
              :type 'main
              :grph (graph:make :size grp-size)))
      snk)))

