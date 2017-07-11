
(in-package :rnd)


(defun lget (l)
  (nth (random (length l)) l))


(defun aget (l)
  (aref l (random (length l))))


; NUMBERS AND RANGES


(defun rndi (a &optional b)
  (let ((a (math:int a)))
    (if (not b)
      (random a)
      (+ a (random (- (math:int b) a))))))


(defun rnd (&optional (x 1.0d0))
  (random (math:dfloat x)))


(defun rndbtwn (&optional a b)
  (+ a (random (math:dfloat (- b a)))))


(defun rnd* (&optional (x 1.0d0))
  (- x (* 2.0d0 (random (math:dfloat x)))))


(defun mixed (x f)
  (let ((x* (math:dfloat x)))
    (+
      (random (* (math:dfloat f) x*))
      (- x* (* 2.0d0 (random x*))))))


(defun rndspace (a b n &key order)
  (destructuring-bind (a b)
    (sort (list a b) #'<)
      (let ((d (math:dfloat (- b a))))
        (let ((res (math:nrep n (+ a (random d)))))
          (if order (sort res #'<) res)))))


(defun rndspacei (a b n &key order)
  (destructuring-bind (a b)
    (sort (list a b) #'<)
      (let ((d (math:int (- b a))))
        (let ((res (math:nrep n (+ a (random d)))))
          (if order (sort res #'<) res)))))


; SHAPES


(defun on-circ (rad &key (xy (list 0.0d0 0.0d0)))
  (math:add
    xy
    (math:scale
      (math:cos-sin (random (* PI 2.0d0)))
      rad)))


(defun in-circ (rad &key (xy (list 0.0d0 0.0d0)))
  (let ((ab (sort (list (random 1.0d0) (random 1.0d0)) #'<)))
    (math:add
      xy
      (math:scale
        (math:cos-sin (* 2 PI (apply #'/ ab)))
        (* (second ab) rad)))))


(defun in-box (sx sy &key (xy (list 0.0d0 0.0d0)))
  (math:add
    (list (rnd* (math:dfloat sx))
          (rnd* (math:dfloat sy)))
    xy))


(defun on-line (x1 x2)
  (math:add x1
           (math:scale (math:sub x2 x1) (random 1.0d0))))


(defun on-spiral (rad &key (xy (list 0.0d0 0.0d0)) (rot 1.0d0))
  (let ((i (random 1.0)))
    (math:add xy
              (math:scale
                (math:cos-sin (* i (* PI rot)))
                (* i rad)))))


; WALKERS


(defun get-lin-stp (&optional (init 0.0d0))
  (let ((x (math:dfloat init)))
    (lambda (stp)
      (setf x (math:inc x (rnd* stp))))))


(defun get-lin-stp* (&optional (init 0.0d0))
  (let ((x (math:dfloat init)))
    (lambda (stp)
      (incf x (rnd* stp)))))


(defun get-acc-lin-stp (&optional (init-x 0.0d0) (init-a 0.0d0))
  (let ((a (math:dfloat init-a))
        (x (math:dfloat init-x)))
    (lambda (s)
      (setf x (math:inc x (incf a (rnd* s)))))))


(defun get-acc-lin-stp* (&optional (init-x 0.0d0) (init-a 0.0d0))
  (let ((a (math:dfloat init-a))
        (x (math:dfloat init-x)))
    (lambda (s)
      (incf x (incf a (rnd* s))))))


(defun get-circ-stp* (&optional (init (list 0.0d0 0.0d0)))
  (let ((xy (math:dfloat* init)))
    (lambda (stp)
      (setf xy (math:add xy (in-circ stp))))))


(defun get-acc-circ-stp* (&optional (init (list 0.0d0 0.0d0))
                                    (init-a (list 0.0d0 0.0d0)))
  (let ((a (math:dfloat* init-a))
        (xy (math:dfloat* init)))
    (lambda (stp)
      (setf xy (math:add xy (setf a (math:add a (in-circ stp))))))))

