
(defun get-projection-fxn (proj mid s)
  (lambda (x)
    (vec:add mid
      (vec:mult (vec:sum (loop for px in proj and ix in x collect
                   (vec:scale px (math:dfloat ix))))
                s))))


(defun do-count (state ind &optional (c 1))
  (multiple-value-bind (val exists)
    (gethash ind state)
    (when (not exists)
          (setf val 0))
    (setf (gethash ind state) (+ val c))))


(defun hypercube-edges (cube)
  (let ((edges (make-adjustable-vector))
        (n (length (aref cube 0))))
    (loop for a across cube do
      (loop for b across cube do
        (when (= (1- n)
                 (loop for ai in a and bi in b count (= ai bi)))
              (vextend (list a b) edges))))
    edges))


(defun hypercube (grid &optional init sel)
  (let ((n (length grid))
        (respts (make-adjustable-vector)))
    (vextend
      (if init init
        (loop for g in grid collect
              (rnd:rndi 1 (1- g))))
      respts)

    (loop for i from 0 below n do
      (let* ((pt (aref respts 0))
             (val (+ (if sel (funcall sel) (expt -1 (rnd:rndi 2)))
                     (nth i pt))))
        (loop for x across respts do
          (let ((new (loop for p in x
                           and j from 0
                           collect (if (= j i) val p))))
            (vextend new respts)))))
    (values respts (hypercube-edges respts))))


(defun -walk (pp)
  (let ((ind (rnd:rndi 0 (length pp))))
    (loop for p in pp and i from 0 collect
      (if (= i ind) (+ p (expt -1 (rnd:rndi 0 2))) p))))


(defun surface-walk (n pts)
  ; this is a little inefficient since it wont always select viable pts.
  ; it is fast enough.
  (let ((res (make-adjustable-vector))
        (pts* (make-hash-table :test #'equal)))

    (loop for p across pts do (setf (gethash p pts*) t))

    (vextend (first (rnd:nrnd-from 1 pts)) res)
    (loop until (>= (length res) n) do
      (let ((new (-walk (aref res (1- (length res))))))
        (multiple-value-bind (val exists)
          (gethash new pts*)
          (if exists (vextend new res)))))
    (to-list res)))


; RENDER FXNS


(defun show-dots (psvg visited proj &optional (rad 1d0))
  (loop for k being the hash-keys of visited do
    (plot-svg:circ psvg (funcall proj k) rad)))


(defun tris (psvg proj n abc)
  (destructuring-bind (a b c) abc
    (loop for s in (math:linspace n 0d0 1d0) do
      (plot-svg:path psvg (list (vec:on-line s (funcall proj a)  (funcall proj b))
                                (vec:on-line s (funcall proj a)  (funcall proj c)))))))

(defun quads (psvg proj n abcd)
  (destructuring-bind (a b c d) abcd
    (loop for s in (math:linspace n 0d0 1d0) do
      (plot-svg:path psvg (list (vec:on-line s (funcall proj a)  (funcall proj b))
                                (vec:on-line s (funcall proj c)  (funcall proj d)))))))


(defun path (visited state pts selected)
  (loop for a in selected and b in (cdr selected) do
    (do-count state (list a b) (rnd:prob 0.1 3 1))
    (setf (gethash a visited) t)
    (setf (gethash b visited) t)))


(defun path* (visited state pts selected)
  (loop for a in selected and b in (cdr selected) do
    (do-count state (list a b) 1)
    (setf (gethash a visited) t)
    (setf (gethash b visited) t)))


(defun bzspl (psvg proj visited pts)
  (plot-svg:bzspl psvg (mapcar (lambda (x) (funcall proj x)) pts))
  (setf (gethash (first pts) visited) t)
  (setf (gethash  (first (last pts)) visited) t))

