
(defvar *grid-near-all* (list (list -1 0) (list 1 0) (list 0 -1) (list 0 1)
                         (list 1 1) (list -1 -1) (list 1 -1) (list -1 1)))

(defvar *grid-near-obliq* (list (list 0 0) (list 1 0) (list 1 1) (list 0 1)))

(defvar *grid-near* (list (list -1 0) (list 1 0) (list 0 -1) (list 0 1)))



(defun make-dims (n left right)
  (let ((w (* 0.5d0 (- right left)))
        (mid (* 0.5d0 (+ right left))))
    (to-vector
      (loop for i from 0 below n
            collect (list (rnd:in-box w w :xy (vec:vec mid)) (rnd:on-circ 1d0))))))


(defun make-lines (left right len noise dims &key xspace)
  (let ((w (* 0.5d0 (- right left)))
        (mid (* 0.5d0 (+ right left))))

    (to-vector
      (loop for (xy dim) across dims collect
        (let ((offset (vec:scale dim len))
              (perp (vec:scale (vec:perp dim) len))
              (wa (rnd:get-acc-lin-stp* 0.5d0))
              (wb (rnd:get-acc-lin-stp* 0.5d0)))
          (to-vector
            (loop for d in xspace collect
              (let ((p (vec:on-line* d
                         (vec:ladd* (list (vec:neg offset) offset) xy))))
                (list (vec:add p (vec:scale perp (funcall wa noise)))
                      (vec:sub p (vec:scale perp (funcall wb noise))))
                ;(list (vec:add p perp)
                ;      (vec:sub p perp))
                ))))))))


(defun -isect (res lu lw)
  (loop for u across lu
        and ui from 0 do
    (loop for w across lw
          and wi from 0
          do (multiple-value-bind (int us ws)
               (vec:segx u w)
               (when int
                     (vextend (list ui wi us ws) res))))))


(defun get-intersects (lines)
  (loop with res = (make-adjustable-vector)
        for i from 0 below (length lines)
        do (loop for j from (1+ i) below (length lines)
                 do (-isect res (aref lines i) (aref lines j)))
        finally (return res)))

(defun -memtest (gm key near nearsel)
  (loop for (i j) in nearsel do
    (let ((k (math:add key (list i j))))
      (multiple-value-bind (val exists) (gethash k gm)
        (when exists (vextend k near))))))

(defun make-grid-map (lines intersects nearsel
                            &key distortfxn
                            &aux (distortfxn* (if (not distortfxn)
                                                (lambda (x) x)
                                                distortfxn)))
  (let ((gm (make-hash-table :test #'equal))
        (points (make-hash-table :test #'equal)))

    (loop for (ui wi us ws) across intersects do
      (let ((key (list ui wi)))
        (multiple-value-bind (val exists) (gethash key gm)
          (when (not exists)
            (setf (gethash key gm) (make-adjustable-vector)
                  (gethash key points)
                  (funcall distortfxn*
                    (vec:on-line* us (aref (aref lines 0) ui))))))))

    (loop for (ui wi us ws) across intersects do
      (let ((key (list ui wi)))
        (multiple-value-bind (val exists) (gethash key gm)
          (-memtest gm key val nearsel))))
    (values gm points)))

