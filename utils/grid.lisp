
(defun get-grid (size edge ngrid)
  (loop for x in (math:linspace ngrid edge (- size edge)) collect
    (loop for y in (math:linspace ngrid edge (- size edge)) collect
      (vec:vec x y))))


(defun get-rnd-grid (size edge ngrid)
  (let ((a (- edge))
        (b (+ size edge)))
    (loop for x in (rnd:rndspace ngrid a b :order t) collect
      (loop for y in (rnd:rndspace ngrid a b :order t) collect
        (vec:vec x y)))))


(defun get-stroke-grid (n m rad angle v w)
  (loop for i in (math:linspace n 0d0 1d0) collect
    (let* ((ivw (math:on-line i v w))
           (p (vec:add ivw (vec:scale (vec:cos-sin angle) rad))))
      (loop for ip in (math:linspace m 0d0 1d0) collect
        (math:on-line ip ivw p)))))

