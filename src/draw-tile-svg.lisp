
(in-package :draw-tile-svg)

; export svg drawings across tiled sheets of paper.
; this functinality is experimental and has limited features.


(defstruct draw-tile-svg
  (nxny nil :type list :read-only t)
  (stroke-width nil :type double-float :read-only t)
  (dw 0d0 :type double-float :read-only t)
  (dh 0d0 :type double-float :read-only t)
  (overview nil :read-only t)
  (grid nil :read-only t)
  (paper-id-fx nil :type function :read-only t)
  (paper-fx nil :type function :read-only t)
  (papers nil :read-only t))


(defun -make-papers (nxny stroke-width)
  (destructuring-bind (nx ny) nxny
    (loop with papers = (make-hash-table :test #'equal)
          for x from 0 below nx
          do (loop for y from 0 below ny
                   do (setf (gethash (list x y) papers)
                            (draw-svg:make :layout :a3-landscape
                                           :stroke-width stroke-width)))
          finally (return papers))))


(defun -make-grid (nxny dw dh)
  (destructuring-bind (nx ny) nxny
    (let ((res (make-adjustable-vector)))
      (vextend* (loop for x in (math:linspace (1+ nx) 0d0 dw)
                         collect (list (vec:vec x 0d0) (vec:vec x dh))) res)
      (vextend* (loop for y in (math:linspace (1+ ny) 0d0 dh)
                         collect (list (vec:vec 0d0 y) (vec:vec dw y))) res )
      res)))


(defun -make-paper-id-fx (dw dh nxny)
  (destructuring-bind (nx ny) nxny
    (lambda (segment)
      (vec:with-xy ((vec:div (vec:lmid segment) (vec:vec dw dh)) x y)
        (list (floor (* x nx))
              (floor (* y ny)))))))


(defun -make-paper-fx (rpw rph dw dh)
  (lambda (pt paperid)
    (destructuring-bind (idx idy) paperid
      (vec:with-xy (pt x y)
        (vec:vec (* (/ (- x (* idx rpw)) rpw) dw)
                 (* (/ (- y (* idy rph)) rph) dh))))))


(defun make (&key (nxny (list 2 2)) (stroke-width 1.1d0))
  (let* ((overview (draw-svg:make :layout :a3-landscape
                                  :stroke-width stroke-width))
         (dw (draw-svg::draw-svg-width overview))
         (dh (draw-svg::draw-svg-height overview))
         (rpw (/ dw (first nxny)))
         (rph (/ dh (second nxny))))
    (make-draw-tile-svg :nxny nxny
                        :stroke-width stroke-width
                        :overview overview
                        :dw dw
                        :dh dh
                        :paper-id-fx (-make-paper-id-fx dw dh nxny)
                        :paper-fx (-make-paper-fx rpw rph dw dh)
                        :grid (-make-grid nxny dw dh)
                        :papers (-make-papers nxny stroke-width))))


(defun -path-segments (pts)
  (loop with res = (make-adjustable-vector)
        for i from 0 below (length-1 pts) collect
        (vextend (list (aref pts i) (aref pts (1+ i))) res)
        finally (return res)))

(defun -make-linear-ratios (segment grid)
  (let ((res (make-adjustable-vector)))
    (vextend* (list 0d0 1d0) res)
    (loop for g across grid
          do (multiple-value-bind (x p) (vec:segx segment g)
               (when x (vextend p res))))
    (sort res #'<)))

(defun -split-segment (segment grid)
  (loop with res = (make-adjustable-vector)
        for s across (-make-linear-ratios segment grid)
        do (vextend (vec:on-line* s segment) res)
        finally (return res)))

(defun -split-path-segments-on-grid (grid pts)
  (loop with res = (make-adjustable-vector)
        for segment across pts do
        (loop with subseg = (-split-segment segment grid)
              for i from 0 below (1- (length subseg))
              do (vextend (list (aref subseg i) (aref subseg (1+ i))) res))
        finally (return res)))


(defun -append-paper-id (paper-id-fx segments)
  (loop for segment across segments
        collect (list segment (funcall paper-id-fx segment))))


(defun -inside (paperid nxny)
  (destructuring-bind (nx ny) nxny
    (destructuring-bind (idx idy) paperid
      (and (< -1 idx nx) (< -1 idy ny)))))


(defun path (msvg pts &key sw (stroke "black") closed
                      &aux (pts* (ensure-vector pts :fx #'to-adjustable-vector)))
  "
  draw path
  "
  (declare (draw-tile-svg msvg) (list pts))
  (with-struct (draw-tile-svg- paper-id-fx paper-fx grid
                               papers overview nxny) msvg
    (draw-svg:path overview pts :sw sw :stroke stroke :closed closed)

    (when closed (vextend (aref pts* 0) pts*))
    (loop for (segment paperid)
          in (-append-paper-id paper-id-fx (-split-path-segments-on-grid
                                             grid (-path-segments pts*)))
          if (-inside paperid nxny)
          do (draw-svg:path (gethash paperid papers)
               (mapcar (lambda (pt) (funcall paper-fx pt paperid)) segment)
               :sw sw
               :stroke stroke))))


(defun lstipple (msvg line &key (len 0.5d0) (num 10) sw (stroke "black"))
  "
  draw num stipples along line.
  the total length of the stipples will be len
  note that len is a ratio of the full length of line
  (a number between 0 and 1)
  "
  (declare (draw-tile-svg msvg) (list line) (double-float len))
  (let ((stip (math:stipple num len)))
    (loop for (a b) across stip
          do (path msvg (list (vec:on-line* a line)
                              (vec:on-line* b line))
                   :sw sw :stroke stroke))))

(defun lstipple* (msvg line &key (len 100d0) (num 10) sw (stroke "black"))
  "
  draw num stipples along line.
  the length of the stipples is limited to the (absolute) length len.

  if the length of the line is shorter than len, a single line (no stipples)
  will be drawn.
  "
  (declare (draw-tile-svg msvg) (list line) (double-float len))
  (let ((d (vec:dst* line)))
    (if (<= d len)
      (path msvg line :sw sw :stroke stroke)
      (lstipple msvg line
                :len (/ len d)
                :num num
                :sw sw :stroke stroke))))


(defun rstipple (msvg n pts &key sw (stroke "black") closed)
  "
  draw n stipples along path pts
  WARN: this is incomplete and will probably not perform as expected
  "
  (declare (draw-tile-svg msvg) (list pts) (fixnum n))
  (loop with all = (to-vector (lin-path:pos*
                               (lin-path:make pts :closed closed)
                               (sort (rnd:nrnd (* 2 n)) (rnd:either #'< #'>))))
        for i from 0 below (* 2 n) by 2
        do (path msvg (list (aref all i) (aref all (1+ i)))
                 :sw sw :stroke stroke)))


(defun bzspl (msvg pts &key sw (stroke "black") closed)
  (declare (draw-tile-svg msvg))
  (path msvg (bzspl:adaptive-pos (bzspl:make pts :closed closed))
        :sw sw :stroke stroke))


(defun save (msvg fn)
  (declare (draw-tile-svg msvg))
  (with-struct (draw-tile-svg- papers overview grid) msvg
    (draw-svg:show-crop overview)
    (loop for path across grid do (draw-svg:path overview path :stroke "blue"))
    (loop for (nx ny) being the hash-keys of papers using (hash-value paper)
          do (draw-svg:show-crop paper)
             (draw-svg:save paper
               (ensure-filename fn (format nil "-part-~ax~a" nx ny) t)))
    (draw-svg:save overview
      (append-postfix (ensure-filename fn "" t) "-overview"))))

