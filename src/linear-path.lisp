
(in-package :lin-path)


(defstruct path
  (n nil :type fixnum :read-only t)
  (closed nil :type boolean)
  (pts nil :type (simple-array double-float) :read-only t)
  (lens nil :type (simple-array double-float) :read-only t))


(defun -diff-scale (a b s)
  (declare (optimize (debug 0) speed (safety 0))
           (double-float a b s))
  (/ (- b a) s))


(defun -get-len (pts i &aux (ii (* 2 i)))
  (declare (optimize (debug 0) speed (safety 0))
           (type (simple-array double-float) pts)
           (fixnum i ii))
  (expt (+ (expt (- (aref pts (+ ii 2)) (aref pts ii)) 2d0)
           (expt (- (aref pts (+ ii 3)) (aref pts (+ ii 1))) 2d0)) 0.5d0))


(defun -set-path-lens (pts lens n)
  (declare (optimize (debug 0) speed (safety 0))
           (type (simple-array double-float) pts lens)
           (fixnum n))
  (loop with tot of-type double-float =
          (loop for i of-type fixnum from 0 below (1- n)
                sum (-get-len pts i) into tot of-type double-float
                do (setf (aref lens (1+ i)) tot)
                finally (return tot))
        for i of-type fixnum from 1 below n
        do (setf (aref lens i) (/ (aref lens i) tot))))


(defun -find-seg-ind (lens f n)
  (declare (optimize (debug 0) speed (safety 0))
           (type (simple-array double-float) lens)
           (fixnum n)
           (double-float f))
  (loop with l of-type fixnum = 0
        with r of-type fixnum = (- n 1)
        with mid of-type fixnum = 0
        until (<= (aref lens mid) f (aref lens (1+ mid)))
        do (setf mid (floor (+ l r) 2))
           (cond ((> f (aref lens mid)) (setf l (progn mid)))
                 ((< f (aref lens mid)) (setf r (1+ mid))))
        finally (return (the fixnum (1+ mid)))))


(defun -calc-pos (pts lens n f)
  (declare (optimize (debug 0) speed (safety 0))
           (type (simple-array double-float) pts lens)
           (fixnum n)
           (double-float f))
  (let* ((i (-find-seg-ind lens f n))
         (bi (* 2 i))
         (ai (- bi 2))
         (s (-diff-scale (aref lens (1- i)) f (- (aref lens i) (aref lens (1- i))))))
    (declare (fixnum i ai bi)
             (double-float s))
    (vec:vec (+ (aref pts ai) (* (- (aref pts bi) (aref pts ai)) s))
             (+ (aref pts (1+ ai)) (* (- (aref pts (1+ bi))
                                         (aref pts (1+ ai))) s)))))


(defun pos (pth f)
  (declare (path pth)
           (double-float f))
  (with-struct (path- lens pts n) pth
    (-calc-pos pts lens n (mod f 1d0))))


(defun pos* (pth ff)
  (declare (path pth)
           (list ff))
  (with-struct (path- lens pts n) pth
    (mapcar (lambda (f) (declare (double-float f))
              (-calc-pos pts lens n (mod f 1d0)))
            ff)))


(defun rndpos (pth n)
  (rnd:with-rndspace (n 0d0 1d0 p :collect t)
    (pos pth p)))


(defun make (pts &key closed
                 &aux (n (if closed (+ (length pts) 1) (length pts))))
  (declare (list pts))
  (let ((p (make-array (* 2 n) :initial-element 0d0
                               :element-type 'double-float))
        (l (make-array n :initial-element 0d0
                         :element-type 'double-float)))

    (loop for pt of-type vec:vec in pts
          and i of-type fixnum from 0 do (vec:sarr-set p i pt))
    (when closed (vec:sarr-set p (1- n) (first pts)))
    (-set-path-lens p l n)
    (make-path :n n :pts p :lens l :closed closed)))

