
(load "../utils/lorem-common")


(defun make-glyph (bbox-fxn estimate-nc-ncn-fxn sort-fxn
                    &key (min-dst 0d0))
  (destructuring-bind (nc ncn)
    (funcall estimate-nc-ncn-fxn bbox-fxn)

    (let* ((centroids (angle-sort-centroids
                        (get-centroids bbox-fxn min-dst nc)
                        (funcall sort-fxn))))
      (lambda ()
        (let ((counts (make-hash-table :test #'equal))
              (centroid-pts (make-hash-table :test #'equal)) )
          (loop for i from 0 do
            (let ((cand (first (funcall bbox-fxn 1))))
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
                               (gethash c counts) 1))))))
                        ;else: exists and has too many pts
            until (-test-centroids counts nc ncn))

          (apply #'append (loop for i from 0 below nc
                              collect (gethash i centroid-pts))))))))


(defun get-alphabet (letters &key bbox-fxn
                                  nc-ncn-fxn
                                  (sort-fxn (lambda () #'<))
                                  (min-dst 0d0))
  (let ((alphabet (make-hash-table :test #'equal)))
    (loop for i from 0 and c across letters do
          (format t "~a ~a ~%" c
                  (setf (gethash c alphabet)
                        (make-glyph (funcall bbox-fxn)
                                    nc-ncn-fxn
                                    sort-fxn))))
    alphabet))


(defun do-write (snk alphabet bbox trbl words &key (tweak-fxn))
  (destructuring-bind (top right bottom left)
    trbl
    (vec:with-xy ((vec:scale bbox 2d0) bx by)
      (let ((g nil)
            (cursor (vec:vec left top)))

        (block outer
          (loop for (word wl) in words and i from 0 do
            (if (> (+ (vec::vec-x cursor) (* (+ 1 wl) bx)) right)
              (progn
                (format t "~%")
                (setf cursor (vec:vec left (+ by (vec::vec-y cursor))))))

            (if (> (vec::vec-y cursor) bottom) (return-from outer i))

            (setf g (snek:add-grp! snk))

            (loop for c across word do
              (format t "~a" c)
              (aif (gethash c alphabet)
                (snek:add-path! snk (math:vadd (funcall it) cursor) :g g))
              (setf cursor (vec:add cursor (vec:vec bx 0d0))))
            (setf cursor (vec:add cursor (vec:vec bx 0d0)))
            (format t " ")))))))

