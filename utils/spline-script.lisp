
(defstruct spline-glyph
  (name nil :type character :read-only t)
  (area 0d0 :type double-float :read-only t)
  (nc 4 :type integer :read-only t)
  (ncn 1 :type integer :read-only t)
  (min-dst 0d0 :type double-float :read-only t)
  (centroids nil :type list)
  (bbox-fxn nil :type function)
  (sort-fxn nil :type function)
  (fxn (lambda (x) nil) :type function))


(defun test-centroids (counts nc ncn)
  (loop for i from 0 below nc always
        (multiple-value-bind (val exists)
          (gethash i counts)
          (and exists (>= val ncn)))))


(defun get-dst (centroids cand)
  (first (sort (loop for c in centroids
                     and i from 0
                     collect (list i (vec:dst cand c)))
               #'< :key #'second)))


(defun get-centroids (bbox-fxn dst nc)
  (let ((hits 1)
        (centroids (funcall bbox-fxn 1)))
    (loop for i from 0 do
      (let ((cand (first (funcall bbox-fxn 1))))
        (if (every (lambda (d) (> d dst))
                   (vec:ldst* centroids cand))
          (progn (setf centroids (append (list cand) centroids))
                 (incf hits))))
      until (>= hits nc))
    centroids))


(defun -do-test-cand (gl centroid-pts counts cand)
  (destructuring-bind (c dst)
    (get-dst (spline-glyph-centroids gl) cand)
    (multiple-value-bind (val exists)
      (gethash c counts)

      (cond ((and exists (< val (spline-glyph-ncn gl)))
             (setf (gethash c centroid-pts)
                   (append (list cand) (gethash c centroid-pts)))
             (incf (gethash c counts)))
            ((not exists)
             (setf (gethash c centroid-pts) (list cand)
                   (gethash c counts) 1))))))
            ;else: exists and has too many pts


(defun make-glyph (name bbox-fxn nc-ncn-fxn sort-fxn
                    &key (min-dst 0d0))
  (destructuring-bind (area nc ncn)
    (funcall nc-ncn-fxn bbox-fxn)

    (let ((gl (make-spline-glyph
                :name name
                :area area
                :nc nc
                :ncn ncn
                :min-dst min-dst
                :centroids (mapcar #'second
                                   (funcall sort-fxn
                                     (get-centroids bbox-fxn min-dst nc)))
                :bbox-fxn bbox-fxn
                :sort-fxn sort-fxn)))
      (setf (spline-glyph-fxn gl)
            (lambda ()
              (let ((counts (make-hash-table :test #'equal))
                    (centroid-pts (make-hash-table :test #'equal)) )
                (loop for i from 0 do
                  (-do-test-cand gl
                                 centroid-pts
                                 counts
                                 (first (funcall (spline-glyph-bbox-fxn gl) 1)))
                  until (test-centroids counts nc ncn))

                (apply #'append (loop for i from 0 below nc
                                    collect (gethash i centroid-pts))))))
      gl)))


(defun get-alphabet (letters &key get-bbox-fxn
                                  nc-ncn-fxn
                                  sort-fxn
                                  (min-dst 0d0))
  (let ((alphabet (make-hash-table :test #'equal)))
    (loop for i from 0 and c across letters do
          (setf (gethash c alphabet)
                (make-glyph c
                            (funcall get-bbox-fxn)
                            nc-ncn-fxn
                            (funcall sort-fxn)
                            :min-dst min-dst)))
    alphabet))


(defun show-alphabet (alphabet)
  (format t "~%")
  (loop for c being the hash-keys in alphabet do
        (let ((a (gethash c alphabet)))
          (format t "char: ~a area: ~,1f nc: ~a ncn: ~a min-dst: ~,1f~%"
            (spline-glyph-name a)
            (spline-glyph-area a)
            (spline-glyph-nc a)
            (spline-glyph-ncn a)
            (spline-glyph-min-dst a))))
  (format t "~%")
  alphabet)


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

            (when (> (vec::vec-y cursor) bottom)
                  (return-from outer i))

            (setf g (snek:add-grp! snk))

            (loop for c across word do
              (format t "~a" c)
              (aif (gethash c alphabet)
                (snek:add-path! snk (vec:ladd* (funcall (spline-glyph-fxn it)) cursor) :g g))
              (setf cursor (vec:add cursor (vec:vec bx 0d0))))
            (setf cursor (vec:add cursor (vec:vec bx 0d0)))
            (format t " ")))))))

