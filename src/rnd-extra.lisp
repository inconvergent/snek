
(in-package :rnd)


(defmacro with-in-circ ((n rad v &key xy) &body body)
  (declare (symbol v))
  (with-gensyms (rad* xy*)
    `(let ((,rad* ,rad)
           (,xy* ,xy))
      (loop repeat ,n
            do (let ((,v (in-circ ,rad* :xy ,xy*)))
                 (progn ,@body))))))


(defmacro with-on-line ((n a b rn) &body body)
  (declare (symbol rn))
  (with-gensyms (sub a*)
    `(let* ((,a* ,a)
            (,sub (vec:sub ,b ,a*)))
      (loop repeat ,n
            do (let ((,rn (vec:add ,a* (vec:scale ,sub (random 1d0)))))
                 (progn ,@body))))))


(defun nrnd-u-from (n a)
  (let* ((a* (ensure-array a))
         (resind nil)
         (anum (length a*)))
    (when (> n anum) (error "not enough distinct elements in a."))
    (loop until (>= (hset:num (hset:make :init resind)) n)
          do (setf resind (nrndi n 0 anum)))
    (loop for i in resind collect (aref a* i))))


(defun nrnd-from (n a)
  (loop for i in (nrndi n 0 (length a)) collect (aref a i)))


(defun array-split (arr p)
  (let ((res (make-generic-array)))

    (array-push (make-generic-array :init (list (aref arr 0))) res)

    (loop for i from 1 below (length arr) do
      (prob p
        (array-push (make-generic-array :init (list (aref arr i))) res)
        (array-push (aref arr i) (aref res (1- (length res))))))
    res))


; SHAPES


(defun -add-if (a xy)
  (if xy (vec:add a xy) a))


(defun on-circ (rad &key xy)
  (-add-if (vec:scale (vec:cos-sin (random PII)) rad) xy))


(defun non-circ (n rad &key xy)
  (declare (integer n))
  (declare (double-float rad))
  (loop repeat n collect (on-circ rad :xy xy)))


(defun in-circ (rad &key xy)
  (declare (double-float rad))
  (-add-if
    (let ((a (random 1d0))
          (b (random 1d0)))
      (declare (double-float a b))
      (if (< a b) (vec:scale (vec:cos-sin (* PII (/ a b))) (* b rad))
                  (vec:scale (vec:cos-sin (* PII (/ b a))) (* a rad))))
    xy))


(defun nin-circ (n rad &key xy)
  (declare (integer n))
  (declare (double-float rad))
  (loop repeat n collect (in-circ rad :xy xy)))


(defun in-box (sx sy &key xy)
  (declare (double-float sx sy))
  (-add-if (vec:vec (rnd* sx) (rnd* sy)) xy))


(defun nin-box (n sx sy &key xy)
  (declare (integer n))
  (declare (double-float sx sy))
  (loop repeat n collect (in-box sx sy :xy xy)))


(defun on-line (a b)
  (declare (vec:vec a b))
  (vec:add a (vec:scale (vec:sub b a) (random 1d0))))


(defun on-line* (ab)
  (declare (list ab))
  (destructuring-bind (a b) ab
    (declare (vec:vec a b))
    (on-line a b)))


(defun non-line (n a b)
  (declare (integer n))
  (declare (vec:vec a b))
  (loop repeat n collect (on-line a b)))


(defun non-line* (n ab)
  (declare (integer n))
  (declare (list ab))
  (destructuring-bind (a b) ab
    (declare (vec:vec a b))
    (non-line n a b)))


; WALKERS

(defun get-lin-stp (&optional (init 0.0d0))
  "
  random linear walker limited to (0 1)
  "
  (let ((x (coerce init 'double-float)))
    (lambda (stp) (setf x (-inc x (rnd* stp))))))


(defun get-lin-stp* (&optional (init 0d0))
  "
  random linear walker
  "
  (let ((x (coerce init 'double-float)))
    (lambda (stp) (incf x (rnd* stp)))))


(defun get-acc-lin-stp (&optional (init-x 0d0) (init-a 0d0))
  "
  random accelerated linear walker limited to (0 1)
  "
  (let ((a (coerce init-a 'double-float))
        (x (coerce init-x 'double-float)))
    (lambda (s) (setf x (-inc x (incf a (rnd* s)))))))


(defun get-acc-lin-stp* (&optional (init-x 0d0) (init-a 0d0))
  "
  random accelerated linear walker
  "
  (let ((a (coerce init-a 'double-float))
        (x (coerce init-x 'double-float)))
    (lambda (s) (incf x (incf a (rnd* s))))))


(defun get-circ-stp* (&optional (init (vec:zero)))
  (let ((xy (vec:copy init)))
    (lambda (stp) (setf xy (vec:add xy (in-circ stp))))))


(defun get-acc-circ-stp* (&optional (init (vec:zero))
                                    (init-a (vec:zero)))
  (let ((a (vec:copy init-a))
        (xy (vec:copy init)))
    (lambda (stp) (setf xy (vec:add xy (setf a (vec:add a (in-circ stp))))))))

