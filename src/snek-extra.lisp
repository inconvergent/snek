
(in-package :snek)


(defun -roll-once (aa)
  (butlast (append (last aa) aa) 1))


; TODO: move this to primitives
(defun get-grp-as-bzspl (snk g)
  (let ((pts (snek:get-grp-verts snk :g g)))
    (when (> (length pts) 3)
          (bzspl:make pts))))


(defun edge-length (snk e)
  (with-struct (snek- verts) snk
    (destructuring-bind (a b) e
      (declare (type integer a b))
      (vec:dst (vec:arr-get verts a)
               (vec:arr-get verts b)))))


; primitives?
(defun add-circ! (snk num rad &key (xy (vec:zero)) g)
  (let ((vv (loop for p of-type double-float in (math:linspace num 0.0d0 1.0d0)
                  collect (add-vert! snk (vec:on-circ p rad :xy xy)))))
    (loop for a of-type integer in vv and b of-type integer in (-roll-once vv)
          collect (add-edge! snk (list a b) :g g))))


; primitives?
(defun add-polygon! (snk n rad &key (xy (vec:zero))
                                    (rot (* 0.25d0 PI)) g)
  (let ((vv (loop for v of-type vec:vec in (vec:polygon n rad :xy xy :rot rot)
                  collect (add-vert! snk v))))
    (loop for a of-type integer in vv and b of-type integer in (-roll-once vv)
          collect (add-edge! snk (list a b) :g g))))


; primitives?
(defun add-path! (snk points &key g closed)
  (let ((vv (add-verts! snk points)))
    (if closed
      (loop for a of-type integer in vv and b of-type integer in (-roll-once vv)
            collect (add-edge! snk (list a b) :g g))
      (loop for a of-type integer in vv and b of-type integer in (cdr vv)
            collect (add-edge! snk (list a b) :g g)))))


; primitives?
(defun add-path*! (snk vv &key g closed)
  (if closed
    (loop for a of-type integer in vv and b of-type integer in (-roll-once vv)
          collect (add-edge! snk (list a b) :g g))
    (loop for a of-type integer in vv and b of-type integer in (cdr vv)
          collect (add-edge! snk (list a b) :g g))))


; PRIMITIVES

(defun -exec-with-args (fxn args &optional ea)
  (apply fxn (if ea (append args ea) args)))

(defun -sel-args (snk p ea)
  (if ea ea (get-args snk :p p)))

(defun psvg-get-prm-types (psvg)
  (declare (type plot-svg::plot-svg psvg))
  (labels ((stdfx (type fxn)
            (list type (lambda (snk p &optional ea
                                      &aux (ea* (-sel-args snk p ea)))
              (-exec-with-args fxn (list psvg (snek:get-prm-verts snk :p p))
                                   ea*))))

           (circfx (snk p &optional ea
                          &aux (ea* (-sel-args snk p ea)))
             (-exec-with-args #'plot-svg:circ
                              (list psvg (first (get-prm-verts snk :p p))
                                         (get-props snk :p p))))
           (circsfx (snk p &optional ea
                           &aux (ea* (-sel-args snk p ea)))
             (-exec-with-args #'plot-svg:circs
                              (list psvg (get-prm-verts snk :p p)
                                         (get-props snk :p p)))))

    (append (mapcar #'stdfx (list 'bzspl 'path 'hatch)
                            (list #'plot-svg:bzspl
                                  #'plot-svg:path
                                  #'plot-svg:hatch))
            (list (list 'circs #'circsfx)
                  (list 'circ #'circfx)))))


; SANDPAINT

(defun draw-edges (snk sand grains &key g)
  (with-struct (snek- verts) snk
    (sandpaint:strokes
      sand
      (map 'list (lambda (ab) (mapcar (lambda (i) (vec:arr-get verts i)) ab))
                 (get-edges snk :g g))
      grains)))


(defun draw-verts (snk sand)
  (with-struct (snek- verts num-verts) snk
    (sandpaint:arr-pix sand verts num-verts)))


(defun draw-circ (snk sand rad grains)
  (with-struct (snek- verts num-verts) snk
    (sandpaint:arr-circ sand verts num-verts rad grains)))


; EXPORT

(defun export-2obj (snk fn)
  (let ((verts (get-all-verts snk))
        (edges (get-edges snk))
        (fnobj (append-postfix fn ".2obj")))
    (with-open-file (stream fnobj :direction :output
                                  :if-exists :supersede)
      (format stream "o mesh~%")
      (dolist (ll verts)
        (format stream "v ~f ~f~%" (vec::vec-x ll) (vec::vec-y ll)))
      (dolist (ll (coerce edges 'list))
        (destructuring-bind (a b)
          (math:add ll '(1 1))
          (format stream "e ~d ~d~%" a b))))

    (format t "~%num verts: ~a ~%" (length verts))
    (format t "num edges: ~a ~%" (length edges))
    (format t "~%file: ~a" fnobj)))

