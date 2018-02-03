
(defun get-splits (n)
  (let ((a -1)
        (b -1))
  (loop until (not (= a b)) do
    (setf a (rnd:rndi (1- n))
          b (rnd:rndi (1- n))))
  (sort (list a b) #'<)))


(defun get-left (a b n)
  (let ((res (make-generic-array)))
    (loop for i from 0
          while (<= i a)
          do (array-push i res))
    (array-push (list a (1+ a)) res)
    (array-push (list b (1+ b)) res)
    (loop for i from (1+ b)
          while (< i n)
          do (array-push (mod i (1- n)) res))
    res))


(defun get-right (a b n)
  (let ((res (make-generic-array)))
    (loop for i from (1+ a)
          while (<= i b)
          do (array-push i res))
    (array-push (list b (1+ b)) res)
    (array-push (list a (1+ a)) res)
    (loop for i from (1+ a)
          while (<= (mod i n) (1+ a))
          do (array-push i res))
    res))


(defun do-mid (a b &optional (s 0.5d0))
  (mapcar (lambda (l r) (+ l (* s (- r l)))) a b))


(defun ind-to-pts (pts inds s)
  (to-vec
    (loop for i across inds collect
      (if (eql (type-of i) 'cons)
        (do-mid (aref pts (first i)) (aref pts (second i)) s)
        (aref pts i)))))


(defun split (pts &optional s (lim 0d0)
                  &aux (s* (if (not s) (lambda () 0.5d0) s))
                       (n (length pts)))
    (if (> (loop for i from 0 below (1- n) minimizing
             (math:dst (aref pts i) (aref pts (1+ i)))) lim)

      ; do split
      (destructuring-bind (a b)
        (get-splits n)
        (let ((sval (funcall s*)))
          (list
            t
            (ind-to-pts pts (get-left a b n) sval)
            (ind-to-pts pts (get-right a b n) sval))))
      ; do not split
      (list nil pts)))
