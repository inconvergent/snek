
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


(defun make* (&key height width (stroke-width 1.1))
  (make-plot-svg
    :layout 'custom
    :stroke-width stroke-width
    :scene (cl-svg:make-svg-toplevel
              'cl-svg:svg-1.1-toplevel
              :height height
              :width width)))


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


(defun path (psvg pts &key sw)
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
      :stroke-width (if sw sw stroke-width))))


(defun -move-to (res p)
  (vec:with-xy-short (p x y)
    (vector-push-extend (format nil "M~a,~a " x y) res)))


(defun -quadratric (res p q)
  (vec:with-xy-short (p ax ay)
    (vec:with-xy-short (q bx by)
      (vector-push-extend (format nil "Q~a,~a ~a,~a " ax ay bx by) res))))


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


(defun bzspl (psvg pts &key closed sw)
  (when (< (length pts) 3)
    (error "needs at least 3 pts."))

  (with-struct (plot-svg- scene stroke-width) psvg
    (let ((pth (make-vec)))
      (if closed (-do-closed pts pth) (-do-open pts pth))
      (cl-svg:draw scene
        (:path :d (cl-svg:path (finalize-path pth)))
         :fill "none"
         :stroke "black"
         :stroke-width (if sw sw stroke-width)))))


(defun wbzspl (psvg pts offset width &key closed sw)
  (loop for s in (math:linspace
                   (math:int (* 1.5 width))
                   (- (/ width 2d0))
                   (/ width 2d0)) do
      (bzspl psvg (vec:lsub* pts (vec:scale offset s))
             :closed closed :sw sw)))


(defun wpath (psvg pts width &key sw)
  (declare (plot-svg psvg))
  (declare (list pts))
  (with-struct (plot-svg- scene stroke-width) psvg
    (if (= width 1)
      ; single path
      (path psvg pts :sw sw)
      ; multi path
      (let ((pth (make-vec))
            (rep (math:int (* 1.5 width)))
            (rup (/ width 2d0))
            (rdown (- (/ width 2d0))))

        (if (= 0 (mod rep 2)) (setf rep (1+ rep)))
        (loop for a in pts
              and b in (cdr pts)
              do
          (accumulate-path pth a)
          (loop for s in (math:linspace rep rdown rup)
                and i from 0
                do
            (accumulate-path
                pth
                (if (= (mod i 2) 0) a b)
                (if (= (mod i 2) 0) b a)
                (vec:scale (vec:norm (vec:perp (vec:sub b a))) s)))
          (accumulate-path pth b))

        (cl-svg:draw scene
          (:path :d (cl-svg:path (finalize-path pth)))
          :fill "none"
          :stroke "black"
          :stroke-width (if sw sw stroke-width))))))


(defun circ (psvg xy rad &key fill sw)
  (declare (plot-svg psvg))
  (with-struct (plot-svg- scene stroke-width) psvg
    (vec:with-xy-short (xy x y)
      (cl-svg:draw scene (:circle :cx x :cy y :r rad)
        :fill (if fill "black" "none")
        :stroke "black" :stroke-width (if sw sw stroke-width)))))


(defun wcirc (psvg xy rad &optional outer-rad)
  (let* ((inner-rad (if outer-rad rad 1d0))
         (outer-rad* (if outer-rad outer-rad rad))
         (n (math:int (* (abs (- outer-rad* inner-rad))
                               1.5d0))))
    (loop for r in (math:linspace n inner-rad outer-rad*) do
      (circ psvg xy r))))


(defun save (psvg fn)
  (declare (plot-svg psvg))
  (with-struct (plot-svg- scene) psvg
    (with-open-file (s (ensure-filename fn ".svg")
                       :direction :output :if-exists :supersede)
      (cl-svg:stream-out s scene))))

