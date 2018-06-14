
(in-package :plot-tile-svg)

; export svg drawings across tiled sheets of paper.
; this functinality is experimental and has limited features.


(defstruct plot-tile-svg
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
                            (plot-svg:make :layout 'plot-svg:a3-landscape
                                           :stroke-width stroke-width)))
          finally (return papers))))


(defun -make-grid (nxny dw dh)
  (destructuring-bind (nx ny) nxny
    (let ((res (make-generic-array)))
      (loop for x in (math:linspace (1+ nx) 0d0 dw)
            do (array-push (list (vec:vec x 0d0) (vec:vec x dh)) res))
      (loop for y in (math:linspace (1+ ny) 0d0 dh)
            do (array-push (list (vec:vec 0d0 y) (vec:vec dw y)) res))
      res)))


(defun -make-paper-id-fx (dw dh nxny)
  (destructuring-bind (nx ny) nxny
    (lambda (segment)
      (vec:with-xy ((vec:div (vec:lmid segment) (vec:vec dw dh)) x y)
        (list (floor (* x nx))
              (floor (* y ny)))))))


(defun -make-paper-fx (rpw rph dw dh nxny)
  (destructuring-bind (nx ny) nxny
    (lambda (pt paperid)
      (destructuring-bind (idx idy) paperid
        (vec:with-xy (pt x y)
          (vec:vec (* (/ (- x (* idx rpw)) rpw) dw)
                   (* (/ (- y (* idy rph)) rph) dh)))))))


(defun make (&key (nxny (list 2 2)) (stroke-width 1.1d0))
  (let* ((overview (plot-svg:make :layout 'plot-svg:a3-landscape
                                  :stroke-width stroke-width))
         (dw (plot-svg::plot-svg-width overview))
         (dh (plot-svg::plot-svg-height overview))
         (rpw (/ dw (first nxny)))
         (rph (/ dh (second nxny))))
    (make-plot-tile-svg :nxny nxny
                        :stroke-width stroke-width
                        :overview overview
                        :dw dw
                        :dh dh
                        :paper-id-fx (-make-paper-id-fx dw dh nxny)
                        :paper-fx (-make-paper-fx rpw rph dw dh nxny)
                        :grid (-make-grid nxny dw dh)
                        :papers (-make-papers nxny stroke-width))))


(defun -path-segments (pts)
  (loop with res = (make-generic-array)
        for i from 0 below (length-1 pts) collect
        (array-push (list (aref pts i) (aref pts (1+ i))) res)
        finally (return res)))

(defun -make-linear-ratios (segment grid)
  (let ((res (make-generic-array)))
    (array-push 0d0 res)
    (array-push 1d0 res)
    (loop for g across grid
          do (multiple-value-bind (x p) (vec:segx segment g)
               (when x (array-push p res))))
    (sort res #'<)))

(defun -split-segment (segment grid)
  (loop with res = (make-generic-array)
        for s across (-make-linear-ratios segment grid)
        do (array-push (vec:on-line* s segment) res)
        finally (return res)))

(defun -split-path-segments-on-grid (grid pts)
  (loop with res = (make-generic-array)
        for segment across pts do
        (loop with subseg = (-split-segment segment grid)
              for i from 0 below (1- (length subseg))
              do (array-push (list (aref subseg i) (aref subseg (1+ i))) res))
        finally (return res)))


(defun -append-paper-id (paper-id-fx segments)
  (loop for segment across segments
        collect (list segment (funcall paper-id-fx segment))))


(defun -inside (paperid nxny)
  (destructuring-bind (nx ny) nxny
    (destructuring-bind (idx idy) paperid
      (and (< -1 idx nx) (< -1 idy ny)))))


(defun path (msvg pts &key sw (stroke "black") closed
                      &aux (pts* (if (eql (type-of pts) 'cons)
                                     (to-generic-array pts) pts)))
  (declare (plot-tile-svg msvg))
  (declare (list pts))
  (with-struct (plot-tile-svg- paper-id-fx paper-fx grid
                               papers overview nxny) msvg
    (plot-svg:path overview pts :sw sw :stroke stroke :closed closed)

    (when closed (array-push (aref pts* 0) pts*))
    (loop for (segment paperid)
          in (-append-paper-id paper-id-fx (-split-path-segments-on-grid
                                             grid (-path-segments pts*)))
          if (-inside paperid nxny)
          do (plot-svg:path (gethash paperid papers)
               (mapcar (lambda (pt) (funcall paper-fx pt paperid)) segment)
               :sw sw
               :stroke stroke))))


(defun save (msvg fn)
  (declare (plot-tile-svg msvg))
  (with-struct (plot-tile-svg- papers overview grid) msvg
    (plot-svg:show-boundary overview)
    (loop for path across grid do (plot-svg:path overview path :stroke "blue"))
    (plot-svg:save overview
      (append-postfix (ensure-filename fn "" t) "-overview"))

    (loop for (nx ny) being the hash-keys of papers using (hash-value paper)
          do (plot-svg:show-boundary paper)
             (plot-svg:save paper
               (ensure-filename fn (format nil "-part-~ax~a" nx ny) t)))))

