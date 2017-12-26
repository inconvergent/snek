
(defun get-grid (size edge ngrid)
  (loop for x of-type double-float in (math:linspace ngrid edge (- size edge)) collect
    (loop for y of-type double-float in (math:linspace ngrid edge (- size edge)) collect
      (vec:vec x y))))


(defun get-grid* (sx sy numx numy)
  (loop for x of-type double-float in (math:linspace numx (first sx) (second sx)) collect
    (loop for y in (math:linspace numy (first sy) (second sy)) collect
      (vec:vec x y))))


(defun get-rnd-grid (size edge ngrid)
  (let ((a (- edge))
        (b (+ size edge)))
    (loop for x of-type double-float in (rnd:rndspace ngrid a b :order t) collect
      (loop for y of-type double-float in (rnd:rndspace ngrid a b :order t) collect
        (vec:vec x y)))))


(defun get-stroke-grid (n m rad angle v w)
  (loop for i of-type double-float in (math:linspace n 0d0 1d0) collect
    (let* ((ivw (vec:on-line i v w))
           (p (vec:add ivw (vec:scale (vec:cos-sin angle) rad))))
      (loop for of-type double-float ip in (math:linspace m 0d0 1d0) collect
        (vec:on-line ip ivw p)))))


(defun get-v-grid (xy width height nums)
  (let ((inside (make-vec))
        (outside (make-vec))
        (all (make-vec)))
    (vec:with-xy (xy x y)
      (loop for b of-type double-float in (math:linspace (first nums) (- y height) (+ y height))
            and bi of-type integer from 0 do
        (loop for a of-type double-float in (math:linspace (second nums) (- x width) (+ x width))
              and ai of-type integer from 0 do
          (vector-push-extend (vec:vec a b) all)
          (if (or (< ai 1) (> ai (- (second nums) 2)))
            (vector-push-extend (vec:vec a b) outside)
            (vector-push-extend (vec:vec a b) inside)))))
    (values all inside outside)))

