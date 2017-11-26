
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


(defun path (psvg pts)
  (declare (plot-svg psvg))
  (declare (list pts))
  (with-struct (plot-svg- scene stroke-width) psvg
    (let ((pth (cl-svg:make-path)))
      (cl-svg:with-path pth
        (vec:with-xy-short ((first pts) x y) (cl-svg:move-to x y)) )
      (loop for p in (cdr pts) do
        (cl-svg:with-path pth
          (vec:with-xy-short (p x y) (cl-svg:line-to x y))))
      (cl-svg:draw scene (:path :d (cl-svg:path pth))
        :fill "none"
        :stroke "black"
        :stroke-width stroke-width))))


(defun wpath (psvg pts width)
  (declare (plot-svg psvg))
  (declare (list pts))
  (with-struct (plot-svg- scene stroke-width) psvg
    (let ((pth (cl-svg:make-path))
          (rep (math:int (floor (* 1.5 width))))
          (rup (/ width 2d0))
          (rdown (- (/ width 2d0))))

      (loop for a in pts and b in (cdr pts) do
        (loop for s in (math:linspace rep rdown rup) do
          (let ((offset (vec:scale (vec:norm (vec:perp (vec:sub b a))) s)))
            (cl-svg:with-path pth
              (vec:with-xy-short ((vec:add a offset) x y)
                  (cl-svg:move-to x y))
              (vec:with-xy-short ((vec:add b offset) x y)
                    (cl-svg:move-to x y))))))

      (cl-svg:draw scene (:path :d (cl-svg:path pth))
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
  (let ((fn* (aif fn fn
                     (progn
                        (warn "missing file name, using: tmp.png")
                        "tmp"))))
    (let ((fnobj (append-postfix fn* ".svg")))
      (with-struct (plot-svg- scene) psvg
        (with-open-file (s fnobj :direction :output :if-exists :supersede)
          (cl-svg:stream-out s scene))
      (format t "~%file ~a~%~%" fnobj)))))

