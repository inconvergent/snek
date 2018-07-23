
; PATHS

(in-package :math)

(defun path-tangents (aa &key closed (default (vec:vec 0d0))
                         &aux (aa* (if (equal (type-of aa) 'cons)
                                       (make-adjustable-vector :init aa :type 'vec)
                                       aa)))
  (when closed (vextend (aref aa* 0) aa*))
  (loop with res = (make-adjustable-vector :type 'vec)
        for i from 0 below (length-1 aa*)
        do (vextend (vec:nsub (aref aa* (1+ i)) (aref aa* i)
                                 :default default)
                       res)
        finally (return res)))


(defun path-angles (pts)
  (let ((res (make-adjustable-vector)))
    (loop for i from 0 below (length-1 pts)
          do (vextend (vec:norm (vec:sub (aref pts (1+ i))
                                            (aref pts i))) res))
    (vextend (aref res (length-1 res)) res)
    res))


(defun -path-simplify (pts lim &optional left right)
  (declare (double-float lim))
  (let ((res (make-adjustable-vector))
        (dmax -1d0)
        (index 0))

    (let* ((l (if (not left) 0 left))
           (r (if (not right) (length-1 pts) right))
           (seg (list (aref pts l) (aref pts r))))

      (loop for i from (1+ l) below r do
        (let ((d (vec:segdst seg (aref pts i))))
          (when (> d dmax)
            (setf dmax d
                  index i))))

      (if (> dmax lim)
        (progn
          (loop with ps = (-path-simplify pts lim l index)
                for i from 0 below (length-1 ps)
                do (vextend (aref ps i) res))
          (loop for i across (-path-simplify pts lim index r)
                do (vextend i res)))
        (progn (vextend l res)
               (vextend r res))))
    (sort res #'<)))


(defun path-simplify (pts lim)
  ;https://hydra.hull.ac.uk/resources/hull:8338
  (let ((pts* (ensure-vector pts)))
    (loop for i across (-path-simplify pts* lim)
          collect (aref pts* i))))


(defun -scale-offset (w a b &key (fxn #'sin))
  (declare (double-float w) (vec:vec a b) (function fxn))
  (let ((s (abs (funcall fxn (abs (- (vec:angle a) (vec:angle b)))))))
    (declare (double-float s))
    (if (< s 0.05d0) w (/ w s))))

(defun -offset (v o)
  (list (vec:add v o) (vec:sub v o)))

(defun -chamfer (width diag pa na aa aa-)
  (let* ((x (< (vec:cross aa aa-) 0d0))
         (corner (if x (second diag) (first diag)))
         (s (-scale-offset width aa- na :fxn #'cos)))
    (loop for v in (-offset pa (vec:scale (vec:perp na) s))
          collect (if x (list v corner) (list corner v)))))

(defun -regular-perp (a b)
  (declare (vec:vec a b))
  (vec:perp (vec:norm (vec:add a b))))

(defun -sharp-perp (a)
  (declare (vec:vec a))
  (vec:perp a))

(defun -make-test-fxn-closed (angles clim slim)
  (declare (vector angles) (double-float clim slim))
  (let ((n- (length-1 angles)))
    (lambda (i)
      (let ((a (aref angles i))
            (a- (aref angles (if (< i 1) n- (1- i)))))
        (let ((dt (vec:dot a- a)))
          (cond ((<= dt slim) (list :sharp (-sharp-perp a-)))
                ((<  dt clim) (list :chamfer (-regular-perp a- a)))
                (t (list :regular (-regular-perp a- a)))))))))

(defun -make-test-fxn-open (angles clim slim)
  (declare (vector angles) (double-float clim slim))
  (let ((n- (length-1 angles)))
    (lambda (i)
      (let ((a (aref angles i)))
        (if (> n- i 0)
          (let ((dt (vec:dot (aref angles (1- i)) a)))
            (cond ((<= dt slim) (list :sharp (-sharp-perp (aref angles (1- i)))))
                  ((< dt clim) (list :chamfer (-regular-perp
                                                (aref angles (1- i)) a)))
                  (t (list :regular (-regular-perp (aref angles (1- i)) a)))))
          (cond ((< i 1) (list :regular (vec:perp a)))
                (t (list :regular (vec:perp a)))))))))

(defun -get-diagonals (pts width clim slim closed )
  (let* ((res (make-adjustable-vector))
         (n (length pts))
         (angles (math:path-angles pts))
         (corner-test (if closed (-make-test-fxn-closed angles clim slim)
                                 (-make-test-fxn-open angles clim slim))))

    (loop for i from 0 below (if closed (1- n) n) do
      (destructuring-bind (corner na) (funcall corner-test i)
        (let ((diag (-offset (aref pts i)
                             (vec:scale na (-scale-offset width
                                             (aref angles i) na)))))
          (mapcar (lambda (d) (vextend d res))
                  (case corner
                    (:chamfer (-chamfer width diag (aref pts i) na (aref angles i)
                                        (aref angles (math:mod- i n))))
                    (:regular (list diag))
                    (:sharp (list (progn diag)
                                  (reverse diag))))))))

    ; hack to handle closed path chamfering
    (when closed (vextend (aref res 0) res))
    res))

(defun path-offset (pts width &key (s 1d0) closed
                                   (clim -0.5) (slim -0.95)
                                   (simplify 1d0))
  (let ((diag (-get-diagonals (to-vector (path-simplify pts simplify))
                width clim slim closed)))
    (loop for d across diag collect (vec:on-line* s d))))


; ----- STITCH -----


(defun stitch (lines)
  "
  randomly mix the hatches in lines according to where the lines intersect.
  this is somewhat inefficient
  "
  (let ((res (make-adjustable-vector)))
    (loop for i from 0 below (length lines) do
      (let ((ss (make-adjustable-vector))
            (curr (aref lines i)))

        (vextend 0d0 ss)
        (vextend 1d0 ss)

        (loop for j from 0 below (length lines) do
          (multiple-value-bind (x s)
            (vec:segx curr (aref lines j))
            (if x (vextend s ss))))

        (setf ss (sort ss (if (< (rnd:rnd) 0.5d0) #'< #'>)))

        (loop for k from (rnd:rndi 2) below (length-1 ss) by 2 do
          (vextend (list (vec:on-line* (aref ss k) curr)
                            (vec:on-line* (aref ss (1+ k)) curr))
                      res))))
    res))


(defun mid-rad (pts &aux (pts* (to-list pts)))
  (let ((mid (vec:lmid pts*)))
    (values mid (loop for p in pts* maximize (vec:dst mid p)))))


; ----- HATCH -----

(defun -get-lines (n mid dst angle steps rnd)
  (let ((lines (make-adjustable-vector))
        (slide (vec:scale (vec:cos-sin (- angle (* 0.5 PI))) dst))
        (offset (vec:scale (vec:cos-sin angle) dst)))
    (loop for s in (funcall steps n) do
      (let ((xy (vec:on-line s (vec:add mid offset)
                               (vec:sub mid offset))))
        (vextend (funcall rnd (list (vec:add xy slide)
                                       (vec:sub xy slide)))
                      lines)))
    lines))


(defun -line-hatch (line pts)
  (let ((ixs (make-adjustable-vector))
        (res (make-adjustable-vector)))

    (loop for i from 0 below (length-1 pts) do
      (multiple-value-bind (x s)
        (vec:segx line (list (aref pts i) (aref pts (1+ i))))
        (if x (vextend s ixs))))

    (setf ixs (sort ixs #'<))

    (loop for i from 0 below (length-1 ixs) by 2 do
      (vextend (list (vec:on-line* (aref ixs i) line)
                        (vec:on-line* (aref ixs (1+ i)) line)) res))

    res))

(defun hatch (pts &key (angles (list 0d0 (* 0.5d0 PI)))
                       (steps (lambda (n) (math:linspace n 0d0 1d0)))
                       (rs 0.25d0)
                       (rnd #'identity))
  "
  draw hatches at angles inside the area enclosed by the path in pts
  "
  (multiple-value-bind (mid dst)
    (mid-rad pts)
    (let ((res (make-adjustable-vector)))
      (loop for a in angles do
        (loop for line across
              (-get-lines (math:int (ceiling (* 2.40 rs dst)))
                          mid (* 1.2d0 dst) a steps rnd) do
          (let ((hh (-line-hatch line pts)))
            (if (> (length hh) 0)
              (loop for h across (remove-if-not (lambda (h) (every #'identity h))
                                                hh)
                    do (vextend h res))))))
      res)))


; ----- CONVEX SPLIT -----

(defun -get-splits (n pts &aux (n- (1- n)))
  (let ((len (loop for i from 0 below (1- n) and ii from 1
                   summing (vec:dst (aref pts i) (aref pts ii)))))
    (flet ((lenok (i) (< (rnd:rnd) (/ (vec:dst (aref pts i) (aref pts (1+ i)))
                                      len))))
      (loop with a with b
            do (setf a (rnd:rndi n-)
                     b (rnd:rndi n-))
            until (and (not (= a b))
                       (funcall #'lenok a)
                       (funcall #'lenok b))
            finally (return (sort (list a b) #'<))))))


(defun -split-get-left (a b n)
  (let ((res (make-adjustable-vector)))
    (loop for i from 0
          while (<= i a)
          do (vextend i res))
    (vextend (list a (1+ a)) res)
    (vextend (list b (1+ b)) res)
    (loop for i from (1+ b)
          while (< i n)
          do (vextend (mod i (1- n)) res))
    res))


(defun -split-get-right (a b n)
  (let ((res (make-adjustable-vector)))
    (loop for i from (1+ a)
          while (<= i b)
          do (vextend i res))
    (vextend (list b (1+ b)) res)
    (vextend (list a (1+ a)) res)
    (loop for i from (1+ a)
          while (<= (mod i n) (1+ a))
          do (vextend i res))
    res))


(defun -split-ind-to-pts (pts inds s)
  (to-vector
    (loop for i across inds
          collect (if (eql (type-of i) 'cons)
                    (destructuring-bind (a b)
                      (mapcar (lambda (i*) (aref pts i*)) i)
                      (vec:add a (vec:scale (vec:sub b a) s)))
                    (aref pts i)))))


(defun convex-split (pts &key (s 0.5d0) (lim 0d0)
                         &aux (n (length pts)))
  (if (< (loop for i from 0 below (1- n)
               minimizing (vec:dst (aref pts i) (aref pts (1+ i)))) lim)
    (return-from convex-split (list pts nil)))

  (destructuring-bind (a b) (-get-splits n pts)
    (list (-split-ind-to-pts pts (-split-get-left a b n) s)
          (-split-ind-to-pts pts (-split-get-right a b n) s))))


; ----- STIPPLE -----

; more or less as suggested in
; https://gist.github.com/evanmiltenburg/dfd571f27372477487cb14f2bdf8b35c

(defun -stipple-get-lengths (num-lines len)
  (let* ((lens (rnd:nrnd num-lines))
         (s (math:sum lens)))
    (loop for l in lens collect (/ (* l len) s))))

(defun stipple (num-lines len)
  "
  draw num-lines stipples between (0 1) the stipples will have a total length
  of len
  "
  (declare (double-float len))
  (let ((lengths (-stipple-get-lengths num-lines len))
        (gaps (-stipple-get-lengths (1- num-lines) (- 1d0 len))))
    (loop with curr = (first lengths)
          with res = (to-adjustable-vector (list (list 0d0 curr)) :type 'vec:vec)
          for l of-type double-float in (cdr lengths)
          and g of-type double-float in gaps
          do (vextend (list curr (+ curr l)) res)
             (incf curr (+ l g))
          finally (return res))))

