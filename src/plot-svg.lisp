
(in-package :plot-svg)


(defstruct plot-svg
  (layout nil :type symbol :read-only t)
  (stroke-width nil :type double-float :read-only t)
  (rep-scale nil :type double-float :read-only t)
  (scene nil :read-only nil))


(defun make (&key
              (layout 'a4-landscape)
              (stroke-width 1d0)
              (rep-scale 1.5d0))
  (make-plot-svg
    :layout layout
    :stroke-width stroke-width
    :rep-scale rep-scale
    :scene (case layout (a4-landscape
                          (cl-svg:make-svg-toplevel
                            'cl-svg:svg-1.1-toplevel
                            :height "210mm"
                            :width "297mm"
                            :view-box "0 0 1414.285 1000"))
                        (a4-portrait
                          (cl-svg:make-svg-toplevel
                            'cl-svg:svg-1.1-toplevel
                            :height "297mm"
                            :width "210mm"
                            :view-box "0 0 1000 1414.285"))
                        (otherwise
                          (error "invalid layout. use: 'plot-svg:a4-portrait or
                                  'plot-svg:a4-landscape.")))))


(defun make* (&key height width (stroke-width 1.1) (rep-scale 1.3d0))
  (make-plot-svg
    :layout 'custom
    :stroke-width stroke-width
    :rep-scale rep-scale
    :scene (cl-svg:make-svg-toplevel
              'cl-svg:svg-1.1-toplevel
              :height height
              :width width)))


(defun accumulate-path (pth a &optional b (offset (vec:zero)))
  (array-push
    (vec:with-xy-short ((vec:add a offset) x y)
      (if (> (length pth) 0)
        (cl-svg:line-to x y)
        (cl-svg:move-to x y)))
    pth)

  (when b
    (array-push
      (vec:with-xy-short ((vec:add b offset) x y)
        (cl-svg:line-to x y))
      pth)))


(defun finalize-path (pth)
  (let ((res (cl-svg:make-path)))
    (loop for x across pth do
      (cl-svg:with-path res x))
    res))


(defun path (psvg pts &key sw)
  (declare (plot-svg psvg))
  (declare (list pts))
  (with-struct (plot-svg- scene stroke-width) psvg
    (cl-svg:draw scene
      (:path :d (cl-svg:path (finalize-path
                               (let ((pth (make-generic-array)))
                                     (loop for p in pts do
                                       (accumulate-path pth p))
                                     pth))))
      :fill "none"
      :stroke "black"
      :stroke-width (if sw sw stroke-width))))


(defun -move-to (res p)
  (vec:with-xy-short (p x y)
    (array-push (format nil "M~a,~a " x y) res)))


(defun -quadratric (res p q)
  (vec:with-xy-short (p ax ay)
    (vec:with-xy-short (q bx by)
      (array-push (format nil "Q~a,~a ~a,~a " ax ay bx by) res))))


; ----- BZSPL HELPERS -----

(defun -fl (a)
  (first (last a)))


(defun -roll-once (aa)
  (append (subseq aa 1) (list (first aa))))


(defun -do-open (pts pth)
  (-move-to pth (first pts))
  (if (= (length pts) 3)
    ; 3 pts
    (-quadratric pth (second pts) (third pts))
    ; more than 3 pts
    (let ((inner (subseq pts 1 (1- (length pts)))))
      (loop for a in inner
            and b in (cdr inner)
            do
        (-quadratric pth a (vec:mid a b)))
      (-quadratric pth (-fl inner) (-fl pts)))))


(defun -do-closed (pts pth)
  (-move-to pth (vec:mid (-fl pts) (first pts)))
  (loop for a in pts
        and b in (-roll-once pts)
        do
    (-quadratric pth a (vec:mid a b))))

; -----

(defun bzspl (psvg pts &key closed sw)
  (when (< (length pts) 3)
    (error "needs at least 3 pts."))

  (with-struct (plot-svg- scene stroke-width) psvg
    (let ((pth (make-generic-array)))
      (if closed (-do-closed pts pth) (-do-open pts pth))
      (cl-svg:draw scene
        (:path :d (cl-svg:path (finalize-path pth)))
         :fill "none"
         :stroke "black"
         :stroke-width (if sw sw stroke-width)))))

; -----

; TODO width == 1?
(defun wbzspl (psvg pts offset width &key closed sw)
  (with-struct (plot-svg- rep-scale) psvg
    (loop for s in (math:linspace
                     (math:int (ceiling (* rep-scale width)))
                     (- (/ width 2d0))
                     (/ width 2d0)) do
      (bzspl psvg (vec:lsub* pts (vec:scale offset s))
             :closed closed :sw sw))))

; ----- END BZSPL -----


(defun wpath (psvg pts &key width sw)
  (declare (plot-svg psvg))
  (declare (list pts))
  (with-struct (plot-svg- scene stroke-width rep-scale) psvg
    (if (not width)
      ; single path
      (path psvg pts :sw sw)
      ; multi path
      (let ((pth (make-generic-array))
            (rep (math:int (ceiling (* rep-scale width))))
            (rup (/ width 2d0))
            (rdown (- (/ width 2d0))))

        (if (= 0 (math:mod2 rep)) (setf rep (1+ rep)))
        (loop for a in pts
              and b in (cdr pts)
              do
          (accumulate-path pth a)
          (loop for s in (math:linspace rep rdown rup)
                and i from 0
                do
            (accumulate-path
                pth
                (if (= (math:mod2 i) 0) a b)
                (if (= (math:mod2 i) 0) b a)
                (vec:scale (vec:norm (vec:perp (vec:sub b a))) s)))
          (accumulate-path pth b))

        (cl-svg:draw scene
          (:path :d (cl-svg:path (finalize-path pth)))
          :fill "none"
          :stroke "black"
          :stroke-width (if sw sw stroke-width))))))


(defun -accumulate-cpath (diagonals rep closed)
  (let ((res (make-generic-array))
        (n (length diagonals)))
    (loop for s in (math:linspace rep 0d0 1d0)
          and k from 0 do
      (loop for i from 0 below n
            and i- downfrom (1- n) do
        (array-push
          (vec:on-line* s
            (aref diagonals (if closed i (if (= (math:mod2 k) 0) i i-))))
          res)))
    (to-list res)))


(defun cpath (psvg pts &key (width 1d0)
                            closed
                            (clim -0.5d0)
                            (slim -0.95d0)
                            (simplify 1d0)
                            sw
                       &aux (pts* (to-array (if closed (close-path pts) pts)))
                            (width* (* width 0.5d0)))
  (declare (plot-svg psvg))
  (declare (list pts))
  (with-struct (plot-svg- rep-scale) psvg
    (let ((rep (math:int (ceiling (* rep-scale width))))
          (diagonals (math::-get-diagonals
                       (to-array (math:path-simplify-rdp pts* simplify))
                       width* clim slim closed)))
      (path psvg (-accumulate-cpath diagonals rep closed)))))

; draw circle with arc.
(defun arccirc (x y r*)
  (let* ((r (math:sfloat r*))
         (r2 (* 2 r)))
    (format nil "M~a,~a m -~a,0 a ~a,~a 0 1,0 ~a 0 a ~a,~a 0 1,0 -~a 0"
            x y r r r r2 r r r2)))

(defun circ (psvg xy rad &key fill sw)
  (declare (plot-svg psvg))
  (with-struct (plot-svg- scene stroke-width) psvg
    (vec:with-xy-short (xy x y)
      (cl-svg:draw scene   (:path :d (arccirc x y rad))  ;(:circle :cx x :cy y :r rad)
        :fill (if fill "black" "none")
        :stroke "black" :stroke-width (if sw sw stroke-width)))))


(defun wcirc (psvg xy rad &optional outer-rad)
  (with-struct (plot-svg- rep-scale) psvg
    (let* ((inner-rad (if outer-rad rad 1d0))
         (outer-rad* (if outer-rad outer-rad rad))
         (n (math:int (* (ceiling (abs (- outer-rad* inner-rad))) rep-scale))))
    (loop for r in (math:linspace n inner-rad outer-rad*) do
      (circ psvg xy r)))))


(defun save (psvg fn)
  (declare (plot-svg psvg))
  (with-struct (plot-svg- scene) psvg
    (with-open-file (s (ensure-filename fn ".svg")
                       :direction :output :if-exists :supersede)
      (cl-svg:stream-out s scene))))

