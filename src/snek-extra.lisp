
(in-package :snek)


(defun -roll-once (aa)
  (butlast (append (last aa) aa) 1))


; TODO: move this to primitives
(defun get-grp-as-bzspl (snk g)
  (let ((pts (snek:get-grp-verts snk :g g)))
    (when (> (length pts) 3)
          (bzspl:make pts))))


(defun edge-length (snk e)
  "
  returns the length of edge e.
  "
  (declare (snek snk) (list e))
  (with-struct (snek- verts) snk
    (destructuring-bind (a b) e
      (declare (type fixnum a b))
      (vec:dst (vec:sarr-get verts a)
               (vec:sarr-get verts b)))))


(defun prune-edges-by-len! (snk lim &optional (fx #'>))
  "
  remove edges longer than lim, use fx #'< to remove edges shorter than lim.
  "
  (declare (snek snk) (double-float lim) (function fx))
  (with (snk)
    (itr-edges (snk e)
      (when (funcall (the function fx) (edge-length snk e) lim)
            (del-edge? e)))))


(defun center! (snk &key (xy vec:*zero*))
  "
  center the verts of snk on xy. returns the previous center.
  "
  (with-struct (snek- verts num-verts) snk
    (loop for i of-type fixnum from 0 below (* 2 num-verts) by 2
          minimizing (aref verts i) into minx of-type double-float
          maximizing (aref verts i) into maxx of-type double-float
          minimizing (aref verts (1+ i)) into miny of-type double-float
          maximizing (aref verts (1+ i)) into maxy of-type double-float
          finally (let ((mx (* 0.5d0 (+ minx maxx)))
                        (my (* 0.5d0 (+ miny maxy))))
                    (declare (double-float mx my))
                    (itr-verts (snk v)
                      (vec:with-xy ((get-vert snk v) vx vy)
                        (snek:move-vert! snk v
                          (vec:vec (+ (vec::vec-x xy) (- vx mx))
                                   (+ (vec::vec-y xy) (- vy my)))
                          :rel nil)))
                    (vec:vec mx my)))))


(defun -is-rel-neigh (u v near)
  (declare (vec:vec u v) (list near))
  (loop with d of-type double-float = (vec:dst u v)
        for w of-type vec:vec in near
        if (not (> (max (vec:dst u w) (vec:dst v w)) d)) summing 1 into c of-type fixnum
        ; TODO: avoid this by stripping u from near* below
        if (> c 1) do (return-from -is-rel-neigh nil))
  t)

; TODO: this is still more than a little inefficient
(defun relative-neighborhood! (snk rad &key g)
  "
  find the relative neigborhood graph (limited by the radius rad) of verts in
  snk. the graph is made in grp g.
  "
  (declare (snek snk) (double-float rad))
  (let ((c 0)
        (tested (make-hash-table :test #'equal)))
    (declare (fixnum c))
    (zwith (snk (max 5d0 rad))
      (itr-verts (snk v :collect nil)
        (loop with v* of-type vec:vec = (get-vert snk v)
              with near of-type vector = (remove-if (lambda (x) (= x v))
                                                     (verts-in-rad snk v* rad))
              ; TODO: strip u from near*
              with near* of-type list = (get-verts snk near)
              for u of-type fixnum across near
              if (not (= u v))
              do (let ((key (sort (list u v) #'<)))
                   (if (and ; if tested is true: don't perform test.
                            (not (gethash key tested))
                            (-is-rel-neigh (get-vert snk u) v* near*))
                       (when (add-edge! snk key :g g) (incf c))
                       ; if not rel neigh: update tested
                       (setf (gethash key tested) t))))))
    c))


; primitives?
(defun add-circ! (snk num rad &key (xy vec:*zero*) g)
  (let ((vv (loop for p of-type double-float in (math:linspace num 0.0d0 1.0d0)
                  collect (add-vert! snk (vec:on-circ p rad :xy xy)))))
    (loop for a of-type fixnum in vv and b of-type fixnum in (-roll-once vv)
          collect (add-edge! snk (list a b) :g g))))


; primitives?
(defun add-polygon! (snk n rad &key (xy vec:*zero*) (rot (* 0.25d0 PI)) g)
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
  (declare (type draw-svg::draw-svg psvg))
  (labels ((stdfx (type fxn)
            (list type (lambda (snk p &optional ea)
              (exec-with-args fxn (list psvg (snek:get-prm-verts snk :p p))
                                  ea))))

           (circfx (snk p &optional ea)
             (declare (ignore ea))
             (exec-with-args #'draw-svg:circ
                             (list psvg (first (get-prm-verts snk :p p))
                                        (get-prm-props snk :p p))))
           (circsfx (snk p &optional ea)
             (declare (ignore ea))
             (exec-with-args #'draw-svg:circs
                             (list psvg (get-prm-verts snk :p p)
                                        (get-prm-props snk :p p)))))

    (append (mapcar #'stdfx (list :bzspl :path :hatch)
                            (list #'draw-svg:bzspl
                                  #'draw-svg:path
                                  #'draw-svg:hatch))
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

