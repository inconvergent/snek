#!/usr/bin/sbcl --script

(load "../src/load")
(load "../utils/state")


(setf *print-pretty* t)
(setf *random-state* (make-random-state t))


(defun test-centroids (counts nc ncn)
  (reduce (lambda (x y) (and x y))
          (loop for i from 0 below nc collect
                (multiple-value-bind (val exists)
                  (gethash i counts)
                  (and exists (>= val ncn))))))


(defun get-dst (centroids cand)
  (first (sort (loop for c in centroids
                     and i from 0
                     collect (list i (vec:dst cand c)))
               #'< :key #'second)))


(defun -glyph-generate-pts (xy bbox centroids nc ncn)
  (let ((counts (make-hash-table :test #'equal))
        (centroid-pts (make-hash-table :test #'equal)))

    (loop for i from 0 do
      (vec:with-xy (bbox bx by)
        (let ((cand (rnd:in-box bx by :xy xy)))
          (destructuring-bind (c dst)
            (get-dst centroids cand)
            (multiple-value-bind (val exists)
              (gethash c counts)

              (cond ((and exists (< val ncn))
                  (progn
                    (setf (gethash c centroid-pts) (append (list cand)
                                                           (gethash c centroid-pts)))
                    (incf (gethash c counts))))
                  ((not exists)
                    (progn
                      (setf (gethash c centroid-pts) (list cand))
                      (setf (gethash c counts) 1))))))))
      until (test-centroids counts nc ncn))

    (let ((pts (loop for i from 0 below nc
                          collect (gethash i centroid-pts))))
      (apply #'append pts))))


(defun make-glyph (xy bbox nc ncn)
  (vec:with-xy ((vec:scale bbox 0.5d0) bx by)
    (-glyph-generate-pts
      xy
      bbox
      (rnd:nin-box nc bx by :xy xy)
      nc
      ncn)))


(defun init-line (width line-chars sxy bbox)
  (let ((snk (snek:make)))
    (vec:with-xy (sxy sx sy)
      (vec:with-xy (bbox bx yy)
      (loop for px in (math:linspace line-chars sx (+ sx width)) collect
        (snek:add-path! snk
          (make-glyph (vec:vec px sy) bbox 2 2) :g (snek:add-grp! snk)))))
    snk))


(defun draw-grid (size border line-chars line-num sx sy rad grains sand)
  (loop for x in (math:linspace line-chars border (- size border)) do
    (loop for y in (math:linspace line-num border (- size border)) do
      (sandpaint:lin-path sand (list (vec:vec (- x sx) (- y sy))
                                     (vec:vec (+ x sx) (- y sy))
                                     (vec:vec (+ x sx) (+ y sy))
                                     (vec:vec (- x sx) (+ y sy))
                                     (vec:vec (- x sx) (- y sy))) rad grains))))


(defun draw-block-grid (size border line-chars line-num sx sy grains sand)
  (loop for x in (math:linspace line-chars border (- size border)) do
    (loop for y in (math:linspace line-num border (- size border)) do
      (sandpaint:pix sand (rnd:nin-box grains sx sy :xy (vec:vec x y))))))


(defun main (size fn)
  (let ((border 110d0)
        (char-rad 46d0)
        (line-chars 20)
        (line-num 10)
        (grains 95)
        (r (rep-list (list
                       (color:hsv 0.51 1 1 0.01)
                       (color:hsv 0.91 1 1 0.01))))
        (sand (sandpaint:make size
                :active (color:white 0.009)
                :bg (color:white))))

    (sandpaint:set-rgba sand (color:vdark 0.009))
    (draw-block-grid size border line-chars line-num 19d0 34d0 600000 sand)

    (sandpaint:set-rgba sand (color:white 0.009))

    (loop for y in (math:linspace line-num border (- size border)) do
      (format t "~a ~%" y)
      (let ((snk (init-line (- size (* 2.0 border))
                            line-chars
                            (vec:vec border y)
                            (vec:vec 17d0 30d0)))
            (state-gen (get-state-gen (lambda () (rnd:get-acc-circ-stp*)))))

        (loop for i from 0 below 500 do
          (snek:with (snk)
            (snek:itr-all-verts (snk v)
              (snek:move-vert? v (rnd:in-circ 0.1d0))
              (snek:move-vert? v (funcall state-gen v 0.00001d0))))
          (snek:itr-grps (snk g)
            ;(sandpaint:set-rgba sand (funcall r))
            (sandpaint:pix sand (bzspl:rndpos (snek:get-grp-as-bzspl snk g) grains))))))

    ;(draw-grid size border line-chars line-num 18d0 33d0 0.8d0 60 sand)

    (sandpaint:pixel-hack sand)
    (sandpaint:save sand fn :gamma 1.5)))

(time (main 1000 (second (cmd-args))))

