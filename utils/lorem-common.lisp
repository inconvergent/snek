
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


(defun get-centroids (bbox-fxn dst nc)
  (let ((hits 1)
        (centroids (funcall bbox-fxn 1)))
    (loop for i from 0 do
      (let ((cand (first (funcall bbox-fxn 1))))
        (if (reduce (lambda (a b) (and a b))
            (mapcar (lambda (d) (> d dst))
                    (math:vdst centroids cand)))
          (progn (setf centroids (append (list cand) centroids))
                 (incf hits))))
      until (>= hits nc))
    centroids))


(defun angle-sort-centroids (centroids order-fxn)
  (mapcar #'second
    (print (sort (loop for c in centroids
                collect (list (apply #'atan (reverse (vec:tolist c))) c))
          order-fxn :key #'first)) ))

