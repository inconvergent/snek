
(defun -roll-once (aa)
  (butlast (append (last aa) aa) 1))


(defun edge-length (snk e)
  (with-struct (snek- verts) snk
    (apply #'dst (mapcar (lambda (v) (get-atup verts v)) e))))


(defun snek-init-circ (snk num rad &key (xy (list 0.0d0 0.0d0)) g)
  (let ((vv (loop for p in (linspace 0.0d0 1.0d0 num)
                  collect (add-vert! snk (on-circ p rad :xy xy) :g g))))
    (loop for a in vv and b in (-roll-once vv)
          collect (add-edge! snk (list a b) :g g))))


(defun snek-init-polygon (snk rad n &key (xy (list 0.0d0 0.0d0)) (rot (* 0.25 PI)) g)
  (let ((vv (loop for v in (polygon n rad :xy xy :rot rot)
                  collect (add-vert! snk v :g g))))
    (loop for a in vv and b in (-roll-once vv)
          collect (add-edge! snk (list a b) :g g))))


(defun snek-init-path (snk points &key g closed)
  (let ((vv (add-verts! snk points :g g)))
    (if closed
      (loop for a in vv and b in (-roll-once vv)
            collect (add-edge! snk (list a b) :g g))
      (loop for a in vv and b in (cdr vv)
            collect (add-edge! snk (list a b) :g g)))))


; TODO: grp
(defun snek-draw-verts (snk sand)
  (with-struct (snek- verts num-verts) snk
    (sandpaint:pix* sand verts num-verts)))


(defun snek-draw-circ (snk sand rad grains)
  (with-struct (snek- verts num-verts) snk
    (sandpaint:circ* sand verts num-verts rad grains)))


; SANDPAINT

(defun snek-draw-edges (snk sand grains &key g)
  (with-struct (snek- verts) snk
    (sandpaint:strokes
      sand
      (loop
        for (a b) in (get-edges snk :g g)
        collect
          (list
            (get-atup verts a)
            (get-atup verts b)))
        grains)))


; EXPORT

(defun snek-export-2obj (snk fn &key g)
  (let ((verts (get-grp-vert-vals snk :g g))
        (edges (get-edges snk :g g))
        (fnobj (append-postfix fn ".2obj")))
    (with-open-file (stream
                      fnobj
                      :direction :output
                      :if-exists :supersede)
      (format stream "o mesh~%")
      (dolist (ll verts)
        (destructuring-bind (a b)
          ll
          (format stream "v ~f ~f~%" a b)))
      (dolist (ll edges)
        (destructuring-bind (a b)
          (add ll '(1 1))
          (format stream "e ~d ~d~%" a b))))

    (format t "~%num verts: ~a ~%" (length verts))
    (format t "num edges: ~a ~%" (length edges))
    (format t "~%file: ~a" fnobj)))
