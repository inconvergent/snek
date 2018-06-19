
(in-package :plot-svg)

(defvar *short* 1000d0)
(defvar *long* 1414.285d0)


(defstruct plot-svg
  (layout nil :type symbol :read-only t)
  (stroke-width nil :type double-float :read-only t)
  (rep-scale nil :type double-float :read-only t)
  (width 0d0 :type double-float :read-only t)
  (height 0d0 :type double-float :read-only t)
  (scene nil :read-only nil))


(defun -view-box (width height)
  (format nil "0 0 ~f ~f" width height))


(defun -get-scene (layout)
  (case layout
    (a4-landscape (cl-svg:make-svg-toplevel
                    'cl-svg:svg-1.1-toplevel
                    :height "210mm"
                    :width "297mm"
                    :view-box (-view-box *long* *short*)))
    (a4-portrait (cl-svg:make-svg-toplevel
                    'cl-svg:svg-1.1-toplevel
                    :height "297mm"
                    :width "210mm"
                    :view-box (-view-box *short* *long*)))
    (a3-landscape (cl-svg:make-svg-toplevel
                    'cl-svg:svg-1.1-toplevel
                    :height "297mm"
                    :width "420mm"
                    :view-box (-view-box *long* *short*)))
    (a3-portrait (cl-svg:make-svg-toplevel
                    'cl-svg:svg-1.1-toplevel
                    :height "420mm"
                    :width "297mm"
                    :view-box (-view-box *short* *long*)))
    (otherwise (error "invalid layout. use: 'plot-svg:a4-portrait,
      'plot-svg:a4-landscape, plot-svg:a3-landscape or plot-svg:a3-portrait."))))


(defun -get-width-height (layout)
  (case layout (a4-landscape (list *long* *short*))
               (a4-portrait (list *short* *long*))
               (a3-landscape (list *long* *short*))
               (a3-portrait (list *short* *long*))))


(defun make (&key (layout 'a4-landscape) (stroke-width 1.1d0) (rep-scale 1.3d0))
  (destructuring-bind (width height)
    (-get-width-height layout)
    (make-plot-svg :layout layout
                   :stroke-width stroke-width
                   :rep-scale rep-scale
                   :height height
                   :width width
                   :scene (-get-scene layout))))


(defun make* (&key height width (stroke-width 1.1d0) (rep-scale 1.3d0))
  (make-plot-svg :layout 'custom
                 :stroke-width stroke-width
                 :rep-scale rep-scale
                 :height height
                 :width width
                 :scene (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                                  :height height
                                                  :width width)))


(defun show-boundary (psvg &key sw (stroke "red"))
  (declare (plot-svg psvg))
  (with-struct (plot-svg- width height) psvg
    (let ((mw (* 0.5d0 width))
          (mh (* 0.5d0 height)))
      (path psvg (vec:rect mw mh :xy (vec:vec mw mh))
                                 :closed t
                                 :sw sw
                                 :stroke stroke))))


(defun show-crop (psvg &key (len 3d0) sw (stroke "red"))
  (declare (plot-svg psvg))
  (with-struct (plot-svg- width height) psvg
    (loop for m in (list (list (vec:vec 0d0 0d0) (vec:vec len 0d0))
                         (list (vec:vec width 0d0) (vec:vec (- width len) 0d0))
                         (list (vec:vec 0d0 height) (vec:vec len height))
                         (list (vec:vec width height) (vec:vec (- width len) height)))
          do (path psvg m :closed t :sw sw :stroke stroke))))


(defun accumulate-path (pth a &optional b (offset (vec:zero)))
  (array-push
    (vec:with-xy-short ((vec:add a offset) x y)
      (if (> (length pth) 0) (cl-svg:line-to x y)
                             (cl-svg:move-to x y)))
    pth)
  (when b (array-push
            (vec:with-xy-short ((vec:add b offset) x y)
              (cl-svg:line-to x y))
            pth)))


(defun finalize-path (pth)
  (loop with res = (cl-svg:make-path)
        for x across pth
        do (cl-svg:with-path res x)
        finally (return res)))


(defun path (psvg pts &key sw (fill "none") (stroke "black") closed)
  (declare (plot-svg psvg))
  (declare (list pts))
  (with-struct (plot-svg- scene stroke-width) psvg
    (cl-svg:draw scene
      (:path :d (cl-svg:path (finalize-path
                               (let ((pth (make-generic-array)))
                                 (loop for p in pts
                                       do (accumulate-path pth p))
                                 (when closed (array-push "Z" pth))
                                 pth))))
      :fill fill
      :stroke stroke
      :stroke-width (if sw sw stroke-width))))


(defun -move-to (res p)
  (vec:with-xy-short (p x y)
    (array-push (format nil "M~a,~a " x y) res)))


(defun -quadratric (res p q)
  (vec:with-xy-short (p ax ay)
    (vec:with-xy-short (q bx by)
      (array-push (format nil "Q~a,~a ~a,~a " ax ay bx by) res))))


; ----- HATCH -----


(defun -get-pts (pts closed)
  (let ((res (make-generic-array))
        (is-cons (equal (type-of pts) 'cons)))
    (if is-cons (loop for p in pts do (array-push p res))
                (loop for p across pts do (array-push p res)))
    (when closed (array-push (if is-cons (first pts) (aref pts 0)) res))
    res))

(defun hatch (psvg pts &key (angles (list 0d0 (* 0.5d0 PI)))
                            (rnd #'identity)
                            (steps (lambda (n) (math:linspace n 0d0 1d0)))
                            stitch drop closed rs sw
                       &aux (draw (if drop
                                    (lambda (p) (rnd:prob drop nil (plot-svg:path psvg p :sw sw)))
                                    (lambda (p) (plot-svg:path psvg p :sw sw)))))
  (with-struct (plot-svg- rep-scale) psvg
    (let ((res (math:hatch (-get-pts pts closed)
                           :angles angles
                           :steps steps
                           :rs (if rs rs rep-scale)
                           :rnd rnd)))

      (loop for h across (if stitch (math:stitch res) res) do
        (if (and (> (length h) 0) (every #'identity h))
          (funcall draw h))))))


(defun mhatch (psvg mpts
              &key (angles (list 0d0 (* 0.5d0 PI)))
                   (rnd #'identity)
                   (steps (lambda (n) (math:linspace n 0d0 1d0)))
                   stitch drop closed rs sw
              &aux (draw (if drop
                           (lambda (p) (rnd:prob drop nil (plot-svg:path psvg p :sw sw)))
                           (lambda (p) (plot-svg:path psvg p :sw sw)))))
  (declare (plot-svg psvg))
  (with-struct (plot-svg- rep-scale) psvg
    (let ((res (make-generic-array)))
      (loop for pts across mpts
            do (loop for h across (math:hatch (-get-pts pts closed)
                                              :angles angles
                                              :steps steps
                                              :rs (if rs rs rep-scale)
                                              :rnd rnd)
                     do (array-push h res)))

      (loop for h across (if stitch (math:stitch res) res) do
        (when (and (> (length h) 0) (every #'identity h))
              (funcall draw h))))))


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
            do (-quadratric pth a (vec:mid a b)))
      (-quadratric pth (-fl inner) (-fl pts)))))


(defun -do-closed (pts pth)
  (-move-to pth (vec:mid (-fl pts) (first pts)))
  (loop for a in pts
        and b in (-roll-once pts)
        do (-quadratric pth a (vec:mid a b))))

; -----

(defun bzspl (psvg pts &key closed sw)
  (declare (plot-svg psvg))
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
(defun wbzspl (psvg pts offset width &key closed sw rs)
  (declare (plot-svg psvg))
  (with-struct (plot-svg- rep-scale) psvg
    (loop for s in (math:linspace
                     (math:int (ceiling (* (if rs rs rep-scale) width)))
                     (- (/ width 2d0))
                     (/ width 2d0)) do
      (bzspl psvg (vec:lsub* pts (vec:scale offset s))
             :closed closed :sw sw))))

; ----- END BZSPL -----


(defun wpath (psvg pts &key width sw rs)
  (declare (plot-svg psvg))
  (declare (list pts))
  (with-struct (plot-svg- scene stroke-width rep-scale) psvg
    (if (not width)
      ; single path
      (path psvg pts :sw sw)
      ; multi path
      (let ((pth (make-generic-array))
            (rep (math:int (ceiling (* (if rs rs rep-scale) width))))
            (rup (/ width 2d0))
            (rdown (- (/ width 2d0))))

        (when (= 0 (math:mod2 rep)) (setf rep (1+ rep)))
        (loop for a in pts
              and b in (cdr pts)
              do (accumulate-path pth a)
                 (loop for s in (math:linspace rep rdown rup)
                       and i from 0
                       do (accumulate-path pth
                            (if (= (math:mod2 i) 0) a b)
                            (if (= (math:mod2 i) 0) b a)
                            (vec:scale (vec:norm (vec:perp (vec:sub b a))) s)))
                 (accumulate-path pth b))

        (cl-svg:draw scene
          (:path :d (cl-svg:path (finalize-path pth)))
          :fill "none"
          :stroke "black"
          :stroke-width (if sw sw stroke-width))))))


(defun -accumulate-cpath (diagonals rep closed &aux (n (length diagonals)))
  (loop with res = (make-generic-array)
          for s in (math:linspace rep 0d0 1d0)
          and k from 0 do
      (loop for i from 0 below n
            and i- downfrom (1- n)
            do (array-push
                 (vec:on-line* s
                   (aref diagonals (if closed i (if (= (math:mod2 k) 0) i i-))))
                 res))
    finally (return (to-list res))))


(defun cpath (psvg pts &key (width 1d0)
                            closed
                            (clim -0.5d0)
                            (slim -0.95d0)
                            (simplify 1d0)
                            sw rs
                       &aux (pts* (to-array (if closed (close-path pts) pts)))
                            (width* (* width 0.5d0)))
  (declare (plot-svg psvg))
  (declare (list pts))
  (with-struct (plot-svg- rep-scale) psvg
    (let ((rep (math:int (ceiling (* (if rs rs rep-scale) width))))
          (diagonals (math::-get-diagonals
                       (to-array (math:path-simplify pts* simplify))
                       width* clim slim closed)))
      (path psvg (-accumulate-cpath diagonals rep closed)))))


; draw circle with arc.
(defun -arccirc (x y r*)
  (let* ((r (math:sfloat r*))
         (r2 (* 2 r)))
    (format nil "M~a,~a m -~a,0 a ~a,~a 0 1,0 ~a 0 a ~a,~a 0 1,0 -~a 0"
            x y r r r r2 r r r2)))


(defun circ (psvg xy rad &key (fill "none") sw aspath)
  (declare (plot-svg psvg))
  (declare (vec:vec xy))
  (declare (double-float rad))
  (with-struct (plot-svg- scene stroke-width) psvg
    (vec:with-xy-short (xy x y)
      ; this seems unnecessary?
      (let ((sw* (if sw sw stroke-width)))
        (if aspath
          (cl-svg:draw scene (:path :d (-arccirc x y rad))
                       :fill fill :stroke "black" :stroke-width sw*)
          (cl-svg:draw scene (:circle :cx x :cy y :r rad)
                       :fill fill :stroke "black" :stroke-width sw*))))))


; TODO: create path with multiple circs
; TODO: multiple rads
(defun circs (psvg vv rad &key (fill "none") sw aspath)
  (declare (plot-svg psvg))
  (declare (list vv))
  (declare (double-float rad))
  (loop for xy of-type vec:vec in vv
        do (circ psvg xy rad :fill fill :sw sw :aspath aspath)))


(defun wcirc (psvg xy rad &key outer-rad rs aspath)
  (declare (plot-svg psvg))
  (with-struct (plot-svg- rep-scale) psvg
    (let* ((inner-rad (if outer-rad rad 1d0))
           (outer-rad* (if outer-rad outer-rad rad))
           (n (math:int (* (ceiling (abs (- outer-rad* inner-rad)))
                           (if rs rs rep-scale)))))
    (loop for r in (math:linspace n inner-rad outer-rad*)
          do (circ psvg xy r :aspath aspath)))))


(defun save (psvg fn)
  (declare (plot-svg psvg))
  (with-struct (plot-svg- scene) psvg
    (with-open-file (s (ensure-filename fn ".svg")
                       :direction :output :if-exists :supersede)
      (cl-svg:stream-out s scene))))

