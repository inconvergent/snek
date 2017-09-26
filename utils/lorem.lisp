
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


; TODO: configurable?
(defun -pos-weight (dst scale)
  ;(> (expt (rnd:rnd) 2d0) (/ dst scale))
  t)


(defun get-centroids (fxn dst nc)
  (let ((hits 1)
        (centroids (funcall fxn 1)))
    (loop for i from 0 do
      (let ((cand (first (funcall fxn 1))))
        (if (reduce (lambda (a b) (and a b))
            (mapcar (lambda (d) (> d dst))
                    (math:vdst centroids cand)))
          (progn (setf centroids (append (list cand) centroids))
                 (incf hits))))
      until (>= hits nc))
    centroids))


(defun make-glyph (fxn scale nc ncn min-dst)
  (let ((centroids (get-centroids fxn min-dst nc))
        (counts (make-hash-table :test #'equal))
        (centroid-pts (make-hash-table :test #'equal)))

    (loop for i from 0 do
      (let ((cand (first (funcall fxn 1))))
        (destructuring-bind (c dst)
          (-get-dst centroids cand)
          (multiple-value-bind (val exists)
            (gethash c counts)

            (cond ((and exists (< val ncn) (-pos-weight dst scale))
                   (setf (gethash c centroid-pts)
                         (append (list cand) (gethash c centroid-pts)))
                   (incf (gethash c counts)))
                  ((and (not exists) (-pos-weight dst scale))
                   (setf (gethash c centroid-pts) (list cand)
                         (gethash c counts) 1))))))
                  ;else: exists and has too many pts
      until (-test-centroids counts nc ncn))

    (apply #'append (loop for i from 0 below nc
                        collect (gethash i centroid-pts)))))


(defun get-fxn (bbox)
  (vec:with-xy ((vec:scale bbox 0.5d0) bx by)
    (lambda (n) (rnd:nin-box n bx by))))


(defun scale-fxn (bbox)
  (if (< (rnd:rnd) 0.15)
    (vec:mult bbox (vec:vec 1d0 2d0))
    bbox))


(defun get-alphabet (get-fxn scale-fxn bbox nc ncn &key (min-dst 0d0))
  (let ((alphabet (make-hash-table :test #'equal)))
    (loop for i from 0 and c across "abcdefghijklmnopqrstuvwxyz.,?-'" do
      (let ((bbox* (funcall scale-fxn bbox)))
        (setf (gethash c alphabet)
              (make-glyph (funcall get-fxn bbox*)
                            (vec:len bbox*) nc ncn
                            min-dst))))
    alphabet))


(defun get-words (txt)
  (loop for word in (split-sequence:split-sequence #\  txt)
        collect (list word (length word))))


(defun do-write (snk alphabet bbox top right bottom left sentence &key (tweak-fxn))
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
              (snek:add-path! snk (math:vadd
                                    (if tweak-fxn (funcall tweak-fxn it) it)
                                    cursor) :g g))
            (setf cursor (vec:add cursor (vec:vec bx 0d0))))
          (setf cursor (vec:add cursor (vec:vec bx 0d0)))
          (format t " "))))))

