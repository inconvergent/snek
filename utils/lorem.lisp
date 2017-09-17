
(ql:quickload "split-sequence")


(defun -test-centroids (counts nc ncn)
  (reduce (lambda (x y) (and x y))
          (loop for i from 0 below nc collect
            (multiple-value-bind (val exists)
              (gethash i counts)
              (and exists (>= val ncn))))))


(defun -get-dst (centroids cand)
  (first (sort (loop for c in centroids
                     and i from 0
                     collect (list i (vec:dst cand c)))
            #'< :key #'second)))


(defun -glyph-generate-pts (bbox centroids nc ncn)
  (let ((counts (make-hash-table :test #'equal))
        (centroid-pts (make-hash-table :test #'equal)))

    (loop for i from 0 do
      (vec:with-xy (bbox bx by)
        (let ((cand (rnd:in-box bx by)))
          (destructuring-bind (c dst)
            (-get-dst centroids cand)
            (multiple-value-bind (val exists)
              (gethash c counts)

              (cond ((and exists (< val ncn))
                      (setf (gethash c centroid-pts)
                            (append (list cand) (gethash c centroid-pts)))
                      (incf (gethash c counts)))
                    ((not exists)
                      (setf (gethash c centroid-pts) (list cand)
                            (gethash c counts) 1)))))))
      until (-test-centroids counts nc ncn))

    (let ((pts (loop for i from 0 below nc
                          collect (gethash i centroid-pts))))
      (apply #'append pts))))


(defun make-glyph (bbox nc ncn)
  (vec:with-xy ((vec:scale bbox 0.5d0) bx by)
    (-glyph-generate-pts
      bbox
      (rnd:nin-box nc bx by :xy (vec:zero))
      nc ncn)))


(defun get-alphabet (bbox nc ncn sp sbox)
  (let ((alphabet (make-hash-table :test #'equal)))
    (loop for i from 0 and c across "abcdefghijklmnopqrstuvwxyz.,?-'" do
      (setf (gethash c alphabet)
            (make-glyph (if (< (rnd:rnd) sp)
                          (vec:mult bbox sbox)
                          bbox)
                        nc ncn)))
    ;(setf (gethash " " alphabet) nil)
    alphabet))


(defun get-words (txt)
  (loop for word in (split-sequence:split-sequence #\  txt) collect
        (list word (length word))))


(defun do-write (snk alphabet bbox top right bottom left sentence)
  (vec:with-xy ((vec:scale bbox 2d0) bx by)
    (let ((g nil)
          (cursor (vec:vec left top)))

      (block outer
        (loop for (word wl) in (get-words sentence) do
          (if (> (+ (vec::vec-x cursor) (* (+ 1 wl) bx)) right)
            (progn
              (format t "~%")
              (setf cursor (vec:vec left (+ by (vec::vec-y cursor))))))

          (if (> (vec::vec-y cursor) bottom) (return-from outer t))

          (setf g (snek:add-grp! snk))

          (loop for c across word do
            (format t "~a" c)
            (aif (gethash c alphabet)
              (snek:add-path! snk (math:vadd (gethash c alphabet) cursor) :g g))
            (setf cursor (vec:add cursor (vec:vec bx 0d0))))
          (setf cursor (vec:add cursor (vec:vec bx 0d0)))
          (format t " "))))))

