
(in-package :math)

(defvar dotlim 0.95d0)


; TYPES

(defun int (x)
  (the integer
    (coerce (floor x) 'integer)))


(defun int* (xx)
  (mapcar (lambda (x) (int (floor x))) xx))


(defun sfloat (x)
  (the float
    (coerce x 'float)))


(defun sfloat* (xx)
  (mapcar (lambda (x) (sfloat x)) xx))


(defun dfloat (x)
  (the double-float
    (coerce x 'double-float)))


(defun dfloat* (xx)
  (mapcar (lambda (x) (dfloat x)) xx))


; RANGES


(defmacro rep ((i itt) &body body)
  `(loop for ,i in ,itt collect (progn ,@body)))


(defmacro nrep (n &body body)
  (with-gensyms (i nname)
    `(let ((,nname ,n))
      (loop for ,i of-type integer from 1 to ,nname collect (progn ,@body)))))


(defun range (a &optional (b nil))
  ; TODO
  ; (declare (integer a))
  (if (not b)
    (loop for x of-type integer from 0 below a collect x)
    (loop for x of-type integer from a below b collect x)))


(defun lget (l ii)
  "
  avoid using this; it is slow if l is large.
  "
  (declare (list l ii))
  (loop for i in ii collect (nth i l)))


(defun inc (x stp)
  (mod (+ x stp) 1d0))


(defun mod- (i n)
  (declare (integer i n))
  (mod (+ n i -1) n))


(defun mod+ (i n)
  (declare (integer i n))
  (mod (+ n i 1) n))


(defun mod2 (i)
  (declare (integer i))
  (mod i 2))


(defmacro with-linspace ((n a b rn &key (end t)) &body body)
  (with-gensyms (a* b* n* nn i ba)
  `(let* ((,n* (int ,n))
          (,nn (dfloat (if ,end (1- ,n*) ,n*)))
          (,a* (dfloat ,a))
          (,b* (dfloat ,b))
          (,ba (- ,b* ,a*)))
    (loop for ,i from 0 below ,n* do
      (let ((,rn (dfloat (+ ,a* (* ,i (/ ,ba ,nn))))))
        (progn ,@body))))))



(defun linspace (n a b &key (end t))
  ; TODO
  ; (declare (double-float a b))
  ; (declare (integer n))
  ; (declare (boolean end))
  (if (> n 1)
    (let ((nn (if end (1- n) n)))
      (loop for i from 0 below n
        collect (dfloat (+ a (* i (/ (- b a) nn))))))
    (list (dfloat a))))


; LIST MATH


(defun add (a b)
  (declare (list a b))
  (mapcar #'+ a b))


(defun dst (a b)
  (declare (list a b))
  (sqrt (loop for ai in a and bi in b sum (expt (- ai bi) 2d0))))


(defun sub (a b)
  (declare (list a b))
  (mapcar #'- a b))


(defun mult (a b)
  (declare (list a b))
  (mapcar #'* a b))


(defun div (a b)
  (declare (list a b))
  (mapcar #'/ a b))


(defun scale (aa bb)
  (declare (list aa bb))
  (mapcar (lambda (a b) (* a b)) aa bb))


(defun scale* (aa s)
  (declare (list aa))
  (declare (double-float s))
  (mapcar (lambda (a) (* a s)) aa))


(defun iscale (aa bb)
  (declare (list aa bb))
  (mapcar (lambda (a b) (/ a b)) aa bb))


(defun iscale* (aa s)
  (declare (list aa))
  (declare (double-float s))
  (mapcar (lambda (a) (/ a s)) aa))


(defun sum (a)
  (declare (list a))
  (reduce #'+ a))

; TODO: expt, sqrt, ...


; PATHS

(defun path-angles (pts)
  (let ((res (make-generic-array)))
    (loop for i from 0 below (1- (length pts))
          do (array-push (vec:norm (vec:sub (aref pts (1+ i))
                                            (aref pts i))) res))
    (array-push (aref res (1- (length res))) res)
    res))


(defun -path-simplify-rdp* (pts lim &optional left right)
  (declare (double-float lim))
  (let ((res (make-generic-array))
        (dmax -1d0)
        (index 0))

    (let* ((l (if (not left) 0 left))
           (r (if (not right) (1- (length pts)) right))
           (seg (list (aref pts l) (aref pts r))))

      (loop for i from (1+ l) below r do
        (let ((d (vec:segdst seg (aref pts i))))
          (if (> d dmax)
            (setf dmax d
                  index i))))

      (if (> dmax lim)
        (progn
          (loop for i in (butlast (-path-simplify-rdp* pts lim l index))
                do (array-push i res))
          (loop for i in (-path-simplify-rdp* pts lim index r)
                do (array-push i res)))
        (progn
          (array-push l res)
          (array-push r res))))
    (sort (to-list res) #'<)))


(defun path-simplify-rdp (pts lim)
  (let ((pts* (if (equal (type-of pts) 'cons) (to-array pts) pts)))
    (loop for i in (-path-simplify-rdp* pts* lim)
          collect (aref pts* i))))


(defun path-simplify-par (pts len lim)
  ; very naive paty simplification
  ; it removes backtracking sections of the path.
  ; a good default lim is -0.99d0
  (let* ((n (length pts))
         (i 1)
         (res (make-generic-array))
         (pts* (if (equal (type-of pts) 'cons) (to-array pts) pts))
         (angles (path-angles pts*)))

    (array-push (aref pts* 0) res)

    (loop while (< i (1- n)) do
      (if (and
            (< (vec:dst (aref pts* (1- i))
                        (aref pts* i)) len)
            (< (vec:dot (aref angles (1- i))
                        (aref angles i)) lim))
        (progn (array-push (aref pts* (1+ i)) res)
               (incf i 2))
        (progn (array-push (aref pts* i) res)
               (incf i))))

    (if (< i n)
      (array-push (aref pts* i) res))

    (if (< (length res) n)
      (path-simplify-par res len lim)
      res)))


(defun -scale-offset (w a b &key (fxn #'sin))
  (let ((dt (vec:dot a b))
        (s (abs (funcall fxn (abs (- (vec:angle a)
                                     (vec:angle b)))))))
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
    (vec:perp (vec:norm (vec:add a b))))

(defun -sharp-perp (a)
  (vec:perp a))

(defun -fv (i n- sel)
  (if (< i n-) sel :regular))

(defun -make-test-fxn-closed (angles clim slim)
  (let ((n- (1- (length angles))))
    (lambda (i)
      (let ((a (aref angles i))
            (a- (aref angles (if (< i 1) n- (1- i)))))
        (let ((dt (vec:dot a- a)))
          (cond
            ((<= dt slim) (list :sharp (-sharp-perp a-)))
            ((<  dt clim) (list :chamfer (-regular-perp a- a)))
            (t (list :regular (-regular-perp a- a)))))))))


(defun -make-test-fxn-open (angles clim slim)
  (let ((n- (1- (length angles))))
    (lambda (i)
      (let ((a (aref angles i)))
        (if (> n- i 0)
          (let ((dt (vec:dot (aref angles (1- i)) a)))
            (cond
              ((<= dt slim) (list :sharp (-sharp-perp (aref angles (1- i)))))
              ((<  dt clim) (list :chamfer (-regular-perp
                                             (aref angles (1- i)) a)))
              (t (list :regular (-regular-perp (aref angles (1- i)) a)))))
          (cond
            ((< i 1) (list :regular (vec:perp a)))
            (t (list :regular (vec:perp a)))))))))


(defun -get-diagonals (pts width clim slim closed )
  (let* ((res (make-generic-array))
         (n (length pts))
         (angles (math:path-angles pts))
         (corner-test (if closed
                        (-make-test-fxn-closed angles clim slim)
                        (-make-test-fxn-open angles clim slim))))

    (loop for i from 0 below (if closed (1- n) n) do
      (destructuring-bind (corner na)
        (funcall corner-test i)

        (let ((diag (-offset (aref pts i) (vec:scale na (-scale-offset width (aref angles i) na)))))
          (mapcar (lambda (d) (array-push d res))
                  (case corner
                    (:chamfer (-chamfer width diag (aref pts i) na (aref angles i)
                                        (aref angles (math:mod- i n))))
                    (:regular (list diag))
                    (:sharp (list
                              (progn diag)
                              (reverse diag))))))))

    (if closed
      ; hack to handle closed path chamfering
      (array-push (aref res 0) res))
    res))

(defun path-offset (pts width &key (s 1d0) closed
                                   (clim -0.5) (slim -0.95)
                                   (simplify 1d0))
  (let ((diag (-get-diagonals (to-array (path-simplify-rdp pts simplify))
                width clim slim closed)))
    (loop for d across diag collect (vec:on-line* s d))))


; ----- STITCH -----


(defun stitch (lines)
  (let ((res (make-generic-array)))
    (loop for i from 0 below (length lines) do
      (let ((ss (make-generic-array))
            (curr (aref lines i)))

        (array-push 0d0 ss)
        (array-push 1d0 ss)

        (loop for j from 0 below (length lines) do
          (multiple-value-bind (x s)
            (vec:segx curr (aref lines j))
            (if x (array-push s ss))))

        (setf ss (sort ss (if (< (random 1d0) 0.5d0) #'< #'>)))

        (loop for k from (random 2) below (1- (length ss)) by 2 do
          (array-push (list (vec:on-line* (aref ss k) curr)
                            (vec:on-line* (aref ss (1+ k)) curr))
                      res))))
    res))


(defun mid-rad (pts &aux (pts* (to-list pts)))
  (let ((mid (vec:lmid pts*)))
    (values mid (loop for p in pts* maximize (vec:dst mid p)))))


; ----- HATCH -----

(defun -get-lines (n mid dst angle steps rnd)
  (let ((lines (make-generic-array))
        (slide (vec:scale (vec:cos-sin (- angle (* 0.5 PI))) dst))
        (offset (vec:scale (vec:cos-sin angle) dst)))
    (loop for s in (funcall steps n) do
      (let ((xy (vec:on-line s (vec:add mid offset)
                               (vec:sub mid offset))))
        (array-push (funcall rnd (list (vec:add xy slide)
                                       (vec:sub xy slide)))
                      lines)))
    lines))


(defun -line-hatch (line pts)
  (let ((ixs (make-generic-array))
        (res (make-generic-array)))

    (loop for i from 0 below (1- (length pts)) do
      (multiple-value-bind (x s)
        (vec:segx line (list (aref pts i) (aref pts (1+ i))))
        (if x (array-push s ixs))))

    (setf ixs (sort ixs #'<))

    (loop for i from 0 below (1- (length ixs)) by 2 do
      (array-push (list (vec:on-line* (aref ixs i) line)
                        (vec:on-line* (aref ixs (1+ i)) line)) res))

    res))

(defun hatch (pts &key (angles (list 0d0 (* 0.5d0 PI)))
                       (steps (lambda (n) (math:linspace n 0d0 1d0)))
                       (rs 0.25d0)
                       (rnd #'identity))
  (multiple-value-bind (mid dst)
    (mid-rad pts)
    (let ((res (make-generic-array)))
      (loop for a in angles do
        (loop for line across
              (-get-lines (math:int (ceiling (* 2.40 rs dst)))
                          mid (* 1.2d0 dst) a steps rnd) do
          (let ((hh (-line-hatch line pts)))
            (if (> (length hh) 0)
              (loop for h across (remove-if-not (lambda (h) (every #'identity h))
                                                hh)
                    do (array-push h res))))))
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


(defun -split-get-right (a b n)
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


(defun -split-ind-to-pts (pts inds s)
  (to-array
    (loop for i across inds collect
      (if (eql (type-of i) 'cons)
        (destructuring-bind (a b)
          (mapcar (lambda (i*) (aref pts i*)) i)
          (vec:add a (vec:scale (vec:sub b a) s)))
        (aref pts i)))))


(defun convex-split (pts &key (s 0.5d0) (lim 0d0)
                         &aux (n (length pts)))
  (if (< (loop for i from 0 below (1- n) minimizing
           (vec:dst (aref pts i) (aref pts (1+ i)))) lim)
    (return-from convex-split (list pts nil)))

  (destructuring-bind (a b)
    (-get-splits n pts)
    (list (-split-ind-to-pts pts (-split-get-left a b n) s)
          (-split-ind-to-pts pts (-split-get-right a b n) s))))

