
(defun get-extrude (n)
  (rnd:rndi n))

(defun do-extrude (n a)
  (let ((res (make-vec)))
    (loop for i from 0
          while (<= i a)
          do (vpe i res))
    (vpe (list a (mod (1+ a) n)) res)
    (loop for i from (1+ a)
          while (<= i n)
          do (vpe (mod i n) res))
    res))


(defun normal-offset (av bv o)
  (vec:ladd*
    (list av bv)
    (vec:scale
      (vec:cos-sin
        (- (vec:angle (vec:sub bv av))
           (* PI 0.5d0)))
      o)))


(defun rot-offset (av bv o)
  (let* ((m (vec:mid av bv))
         (d (vec:sub bv av))
         (pt (vec:add m
                      (vec:scale
                        (vec:cos-sin
                          (- (vec:angle d)
                             (* PI 0.5d0)))
                        o)))
         (rot (vec:scale (vec:rot d (rnd:rnd 0.5d0))
                         0.5d0)))
    (list
      (vec:sub pt rot)
      (vec:add pt rot))))


(defun get-offsets (pts ab offset)
  (destructuring-bind (a b)
    ab
    (let ((av (aref pts a))
          (bv (aref pts b)))
      (rnd:prob 0.2
        (rot-offset av bv offset)
        (normal-offset av bv (* offset 0.8d0))))))


(defun -ind-to-pts (pts inds offset)
  (let ((offset* nil))
    (values (to-vec
      (flatten (loop for i across inds collect
        (if (eql (type-of i) 'cons)
          (setf offset* (get-offsets pts i offset))
          (aref pts i)))))
      offset*)))


(defun extrude (pts offset &key fxn
                           &aux (n (1- (length pts))))
  (let* ((ind (get-extrude n))
         (extruded (do-extrude n ind)))
    (multiple-value-bind (res extruded-pts)
      (-ind-to-pts pts extruded offset)
      (when fxn (funcall fxn pts ind extruded-pts))
      res)))

