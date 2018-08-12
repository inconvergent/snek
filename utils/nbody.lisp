
(defstruct body
  (xy nil :type vec:vec :read-only t)
  (v nil :type vec:vec :read-only t)
  (m 1d0 :type double-float :read-only t))


(defun init-square (rad n v)
  (loop for vel in (rnd:nin-circ n v) ;(vec:polygon n v)
        for i from 0
        for p in (lin-path:pos* (lin-path:make (vec:rect rad rad) :closed t)
                                (math:daddmod* (math:linspace n 0d0 1d0 :end nil)
                                               (rnd:rnd) 1d0))
        collect (make-adjustable-vector
                  :init (list (make-body :xy p
                                         :v (rnd:in-circ (* 0.5 v) :xy vel)
                                         :m (if (= i 0) 3d0 1d0))))))

; NOTE
; i don't think the below force calculation uses the correct formula.
; the correct equation is probably:
;   s* = (* (/ (expt d2 1.5d0)) s (body-m att))
; so that we normalise the direction vectors and divide by distance squared.
; however, the current version yields nicer results.

; also, we only multiply by attracting mass (M/bj) because
;   F = ma = const * mM / r²
; means that
;   a = const * M / r²
; so that the the affected mass is not present in the equation.
(defun force (curr att s)
  (let* ((cxy (body-xy curr))
         (axy (body-xy att))
         (dx (- (vec::vec-x axy) (vec::vec-x cxy)))
         (dy (- (vec::vec-y axy) (vec::vec-y cxy)))
         (d2 (+ (* dx dx) (* dy dy))))
    ; this is not entirely correct, but it causes the force to be zero when
    ; curr == att. it also eliminates other div0 errors.
    (if (<= d2 0.0000001d0)
      vec:*zero*
      (let ((s* (* (/ (sqrt d2)) s (body-m att))))
        (vec:vec (* dx s*) (* dy s*))))))


(defun dostep (bodies body s)
  (let* ((curr (vector-last body))
         (acc (vec:sum (loop for b in bodies collect
                             (force curr (vector-last b) s))))
         (new-vel (vec:add (vec:scale (body-v curr) 0.999d0) acc)))
    (make-body :xy (vec:add (body-xy curr) new-vel)
               :v new-vel
               :m (body-m curr))))


(defun all-steps (bodies s)
  (loop for body in bodies
        collect (dostep bodies body s) into new-steps
        finally (loop for new in new-steps
                      and body in bodies
                      do (vextend new body))))

