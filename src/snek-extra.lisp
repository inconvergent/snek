
(in-package :snek)


(defun -roll-once (aa)
  (butlast (append (last aa) aa) 1))


; TODO: move this to primitives
(defun get-grp-as-bzspl (snk g)
  (let ((pts (snek:get-grp-verts snk :g g)))
    (when (> (length pts) 3)
          (bzspl:make pts))))


(defun edge-length (snk e)
  (declare (snek snk) (list e))
  (with-struct (snek- verts) snk
    (destructuring-bind (a b) e
      (declare (type fixnum a b))
      (vec:dst (vec:sarr-get verts a)
               (vec:sarr-get verts b)))))


(defun prune-edges-by-len (snk lim &optional (fx #'>))
  (declare (snek snk) (double-float lim) (function fx))
  (with (snk)
    (itr-edges (snk e)
      (when (funcall (the function fx) (edge-length snk e) lim)
            (del-edge? e)))))


(defun center (snk xy)
  (with-struct (snek- verts num-verts) snk
    (loop for i from 0 below (* 2 num-verts) by 2
          minimizing (aref verts i) into minx
          maximizing (aref verts i) into maxx
          minimizing (aref verts (1+ i)) into miny
          maximizing (aref verts (1+ i)) into maxy
          finally (let ((mx (* 0.5d0 (+ minx maxx)))
                        (my (* 0.5d0 (+ miny maxy))))
                    (itr-verts (snk v)
                      (vec:with-xy ((get-vert snk v) vx vy)
                        (snek:move-vert! snk v
                          (vec:vec (+ (vec::vec-x xy) (- vx mx))
                                   (+ (vec::vec-y xy) (- vy my)))
                          :rel nil)))))))


; primitives?
(defun add-circ! (snk num rad &key (xy vec:*zero*) g)
  (let ((vv (loop for p of-type double-float in (math:linspace num 0.0d0 1.0d0)
                  collect (add-vert! snk (vec:on-circ p rad :xy xy)))))
    (loop for a of-type fixnum in vv and b of-type fixnum in (-roll-once vv)
          collect (add-edge! snk (list a b) :g g))))


; primitives?
(defun add-polygon! (snk n rad &key (xy vec:*zero*)
                                    (rot (* 0.25d0 PI)) g)
  (let ((vv (loop for v of-type vec:vec in (vec:polygon n rad :xy xy :rot rot)
                  collect (add-vert! snk v))))
    (loop for a of-type fixnum in vv and b of-type fixnum in (-roll-once vv)
          collect (add-edge! snk (list a b) :g g))))


; primitives?
(defun add-path! (snk points &key g closed)
  (let ((vv (add-verts! snk points)))
    (if closed
      (loop for a of-type fixnum in vv and b of-type fixnum in (-roll-once vv)
            collect (add-edge! snk (list a b) :g g))
      (loop for a of-type fixnum in vv and b of-type fixnum in (cdr vv)
            collect (add-edge! snk (list a b) :g g)))))


; primitives?
(defun add-path*! (snk vv &key g closed)
  (if closed
    (loop for a of-type fixnum in vv and b of-type fixnum in (-roll-once vv)
          collect (add-edge! snk (list a b) :g g))
    (loop for a of-type fixnum in vv and b of-type fixnum in (cdr vv)
          collect (add-edge! snk (list a b) :g g))))


; PRIMITIVES

(defun psvg-get-prm-types (psvg)
  (declare (type plot-svg::plot-svg psvg))
  (labels ((stdfx (type fxn)
            (list type (lambda (snk p &optional ea)
              (exec-with-args fxn (list psvg (snek:get-prm-verts snk :p p))
                                  ea))))

           (circfx (snk p &optional ea)
             (declare (ignore ea))
             (exec-with-args #'plot-svg:circ
                             (list psvg (first (get-prm-verts snk :p p))
                                        (get-prm-props snk :p p))))
           (circsfx (snk p &optional ea)
             (declare (ignore ea))
             (exec-with-args #'plot-svg:circs
                             (list psvg (get-prm-verts snk :p p)
                                        (get-prm-props snk :p p)))))

    (append (mapcar #'stdfx (list :bzspl :path :hatch)
                            (list #'plot-svg:bzspl
                                  #'plot-svg:path
                                  #'plot-svg:hatch))
            (list (list :circs #'circsfx)
                  (list :circ #'circfx)))))


; SANDPAINT

(defun draw-edges (snk sand grains &key g)
  (with-struct (snek- verts) snk
    (sandpaint:strokes
      sand
      (map 'list (lambda (ab) (mapcar (lambda (i) (vec:sarr-get verts i)) ab))
                 (get-edges snk :g g))
      grains)))


(defun draw-verts (snk sand)
  (with-struct (snek- verts num-verts) snk
    (sandpaint:arr-pix sand verts num-verts)))


(defun draw-circ (snk sand rad grains)
  (with-struct (snek- verts num-verts) snk
    (sandpaint:arr-circ sand verts num-verts rad grains)))

