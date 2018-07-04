
(in-package :rnd)


(defmacro with-in-circ ((n rad v &key xy) &body body)
  (declare (symbol v))
  (with-gensyms (rad* xy* m)
    `(let* ((,rad* ,rad)
            (,xy* ,xy)
            (,m (if ,xy* ,xy* vec:*zero*)))
      (declare (vec:vec ,m))
      (loop repeat ,n
            do (let ((,v (in-circ ,rad* :xy ,m)))
                 (declare (vec:vec ,v))
                 (progn ,@body))))))


(defmacro with-on-line ((n a b rn) &body body)
  (declare (symbol rn))
  (with-gensyms (sub a*)
    `(let* ((,a* ,a)
            (,sub (vec:sub ,b ,a*)))
      (loop repeat ,n
            do (let ((,rn  (vec:add-scaled ,a* ,sub (random 1d0))))
                 (declare (vec:vec ,rn))
                 (progn ,@body))))))


(declaim (ftype (function (double-float double-float &key (:xy vec:vec)) vec:vec) in-box))
(declaim (ftype (function (vec:vec vec:vec) vec:vec) on-line))
(declaim (ftype (function (double-float &key (:xy vec:vec)) vec:vec)
                in-circ on-circ))


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


(defun on-circ (rad &key (xy vec:*zero*))
  (declare (double-float rad))
  (declare (vec:vec xy))
  (vec:add-scaled xy (vec:cos-sin (random PII)) rad))


(defun non-circ (n rad &key (xy vec:*zero*))
  (declare (integer n))
  (declare (double-float rad))
  (loop repeat n collect (on-circ rad :xy xy)))


(defun in-circ (rad &key (xy vec:*zero*))
  (declare (double-float rad))
  (let ((a (random 1d0))
        (b (random 1d0)))
    (declare (double-float a b))
    (if (< a b) (vec:add-scaled xy (vec:cos-sin (* PII (/ a b))) (* b rad))
                (vec:add-scaled xy (vec:cos-sin (* PII (/ b a))) (* a rad)))))


(defun nin-circ (n rad &key (xy vec:*zero*))
  (declare (integer n))
  (declare (double-float rad))
  (loop repeat n collect (in-circ rad :xy xy)))


(defun in-box (sx sy &key (xy vec:*zero*))
  (declare (double-float sx sy))
  (declare (vec:vec xy))
  (vec:add xy (vec:vec (rnd* sx) (rnd* sy))))


(defun nin-box (n sx sy &key (xy vec:*zero*))
  (declare (integer n))
  (declare (double-float sx sy))
  (declare (vec:vec xy))
  (loop repeat n collect (in-box sx sy :xy xy)))


(defun on-line (a b)
  (declare (vec:vec a b))
  (vec:add-scaled a (vec:sub b a) (random 1d0)))


(defun on-line* (ab)
  (declare (list ab))
  (destructuring-bind (a b) ab
    (declare (vec:vec a b))
    (on-line a b)))


(defun non-line (n a b)
  (declare (integer n))
  (declare (vec:vec a b))
  (loop with ba = (vec:sub b a)
        repeat n
        collect (vec:add-scaled a ba (random 1d0))))


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
  (declare (double-float init))
  (let ((x init))
    (lambda (stp) (declare (double-float stp))
      (setf x (-inc x (rnd* stp))))))


(defun get-lin-stp* (&optional (init 0d0))
  "
  random linear walker
  "
  (declare (double-float init))
  (let ((x init))
    (lambda (stp) (declare (double-float stp))
      (incf x (rnd* stp)))))


(defun get-acc-lin-stp (&optional (init-x 0d0) (init-a 0d0))
  "
  random accelerated linear walker limited to (0 1)
  "
  (declare (double-float init-x init-a))
  (let ((a init-a)
        (x init-x))
    (lambda (stp) (declare (double-float stp))
      (setf x (-inc x (incf a (rnd* stp)))))))


(defun get-acc-lin-stp* (&optional (init-x 0d0) (init-a 0d0))
  "
  random accelerated linear walker
  "
  (declare (double-float init-x init-a))
  (let ((a init-a)
        (x init-x))
    (lambda (stp) (declare (double-float stp))
      (incf x (incf a (rnd* stp))))))


(defun get-circ-stp* (&optional (init vec:*zero*))
  (declare (vec:vec init))
  (let ((xy (vec:copy init)))
    (lambda (stp) (declare (double-float stp))
      (setf xy (vec:add xy (in-circ stp))))))


(defun get-acc-circ-stp* (&optional (init vec:*zero*)
                                    (init-a vec:*zero*))
  (declare (vec:vec init init-a))
  (let ((a (vec:copy init-a))
        (xy (vec:copy init)))
    (lambda (stp) (declare (double-float stp))
      (setf xy (vec:add xy (setf a (vec:add a (in-circ stp))))))))

