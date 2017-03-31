
(defun -roll-once (aa)
  (butlast (append (last aa) aa) 1))

(defun edge-length (snk e)
  (with-struct (snek- verts) snk
    (apply #'dst (mapcar (lambda (v) (get-as-list verts v)) e))))


(defun snek-init-circle (snk num rad &key (xy (list 0.0d0 0.0d0)) g)
  (let ((vv (loop for v from 0 below num
                  collect (insert-vert snk (on-circ v num rad :xy xy) :g g))))
    (loop for a in vv
          for b in (-roll-once vv)
          collect (insert-edge snk (list a b) :g g))))


(defun snek-init-polygon (snk rad n &key (xy (list 0.0d0 0.0d0)) (rot (* 0.25 PI)) g)
  (let ((vv (loop for v in (polygon n rad :xy xy :rot rot)
                  collect (insert-vert snk v :g g))))
    (loop for a in vv
          for b in (-roll-once vv)
          collect (insert-edge snk (list a b) :g g))))


(defun snek-init-path (snk points &key g closed)
  (let ((vv (insert-verts snk points :g g)))
    (loop for a in vv
          for b in (if closed (-roll-once vv) (cdr vv))
          collect (insert-edge snk (list a b) :g g))))


; SANDPAINT

(defun snek-draw-edges (snk sand grains &key g)
  (with-struct (snek- verts) snk
    (sandpaint:strokes
      sand
      (loop
        for ee in (get-edges snk :g g)
        collect
          (destructuring-bind (a b)
            ee
            (list
              (get-as-list verts a)
              (get-as-list verts b))))
        grains)))


; TODO: grp
(defun snek-draw-verts (snk sand)
  (with-struct (snek- verts num-verts) snk
    (sandpaint:pix* sand verts num-verts)))

