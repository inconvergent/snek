
(in-package :plot-svg)


(defstruct plot-svg
  (layout nil :type symbol :read-only nil)
  (stroke-width nil :type float :read-only nil)
  (scene nil :read-only nil))


(defun make (&key
              (layout 'a4-landscape)
              (stroke-width 1.1))
  (make-plot-svg
    :layout layout
    :stroke-width stroke-width
    :scene (case layout ('a4-landscape
                          (cl-svg:make-svg-toplevel
                            'cl-svg:svg-1.1-toplevel
                            :height "210mm"
                            :width "297mm"
                            :view-box "0 0 1414.285 1000"))
                        ('a4-portrait
                          (cl-svg:make-svg-toplevel
                            'cl-svg:svg-1.1-toplevel
                            :height "297mm"
                            :width "210mm"
                            :view-box "0 0 1000 1414.285"))
                        (otherwise
                          (error "invalid layout. use: 'plot-svg:a4-portrait or
                                  'plot-svg:a4-landscape.")))))


(defun accumulate-path (pth a &optional b (offset (vec:zero)))
  (vector-push-extend
    (vec:with-xy-short ((vec:add a offset) x y)
      (if (> (length pth) 0)
        (cl-svg:line-to x y)
        (cl-svg:move-to x y)))
    pth)

  (when b
    (vector-push-extend
      (vec:with-xy-short ((vec:add b offset) x y)
        (cl-svg:line-to x y))
      pth)))


(defun finalize-path (pth)
  (let ((res (cl-svg:make-path)))
    (loop for x across pth do
      (cl-svg:with-path res x))
    res))


(defun path (psvg pts)
  (declare (plot-svg psvg))
  (declare (list pts))
  (with-struct (plot-svg- scene stroke-width) psvg
    (cl-svg:draw scene
      (:path :d (cl-svg:path (finalize-path
                               (let ((pth (make-vec)))
                                     (loop for p in pts do
                                       (accumulate-path pth p))
                                     pth))))
      :fill "none"
      :stroke "black"
      :stroke-width stroke-width)))

(defun wpath (psvg pts width)
  (declare (plot-svg psvg))
  (declare (list pts))
  (with-struct (plot-svg- scene stroke-width) psvg
    (let ((pth (make-vec))
          (rep (math:int (floor (* 1.5 width))))
          (rup (/ width 2d0))
          (rdown (- (/ width 2d0))))

      (loop for a in pts
            and b in (cdr pts)
            and i from 0
            do
        (loop for s in (math:linspace rep rdown rup) do
          (accumulate-path
              pth
              (if (= (mod i 2) 0) a b)
              (if (= (mod i 2) 0) b a)
              (vec:scale (vec:norm (vec:perp (vec:sub b a))) s))))

      (cl-svg:draw scene
        (:path :d (cl-svg:path (finalize-path pth)))
        :fill "none"
        :stroke "black"
        :stroke-width stroke-width))))


(defun circ (psvg xy rad &key fill)
  (declare (plot-svg psvg))
  (with-struct (plot-svg- scene stroke-width) psvg
    (let ((pth (cl-svg:make-path)))
      (vec:with-xy-short (xy x y)
        (cl-svg:draw scene (:circle :cx x :cy y :r rad)
          :fill (if fill "black" "none")
          :stroke "black" :stroke-width stroke-width)))))


(defun wcirc (psvg xy rad &optional outer-rad)
  (let* ((inner-rad (if outer-rad rad 1d0))
         (outer-rad* (if outer-rad outer-rad rad))
         (n (math:int (floor (* (abs (- outer-rad* inner-rad))
                                1.5d0)))))
    (loop for r in (math:linspace n inner-rad outer-rad*) do
      (circ psvg xy r))))


(defun save (psvg fn)
  (declare (plot-svg psvg))
  (with-struct (plot-svg- scene) psvg
    (with-open-file (s (ensure-filename fn ".svg")
                       :direction :output :if-exists :supersede)
      (cl-svg:stream-out s scene))))

