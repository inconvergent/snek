
(in-package :bzspl)

;(declaim (optimize (safety 0) speed (debug 0)))

;useful info: http://graphics.cs.ucdavis.edu/~joy/ecs178/Unit-7-Notes/MatrixBSpline.pdf

(defstruct bzspl
  (n nil :type fixnum :read-only t)
  (ns nil :type fixnum :read-only t)
  (closed nil :type boolean)
  (vpts nil :type simple-array))


(defun -do-calc (vpts x seg)
  (declare (optimize (safety 0) speed (debug 0))
           (type (simple-array double-float) vpts)
           (double-float x) (fixnum seg))
  (let* ((2x (+ x x))
         (xe2 (* x x))
         (a (+ 1d0 (- 2x) xe2))
         (b (+ 2x (* -2d0 xe2)))
         (c xe2)
         (ia (* 4 seg))
         (ib (+ ia 2))
         (ic (+ ia 4)))
    (declare (double-float 2x xe2 a b c) (fixnum ia ib ic))
    (vec:vec (+ (* (aref vpts ia) a)
                (* (aref vpts ib) b)
                (* (aref vpts ic) c))
             (+ (* (aref vpts (1+ ia)) a)
                (* (aref vpts (1+ ib)) b)
                (* (aref vpts (1+ ic)) c)))))


(defun -get-seg (ns x &aux (s (the double-float
                                (coerce (the fixnum ns) 'double-float))))
  (declare (optimize (safety 0) speed (debug 0))
           (fixnum ns) (double-float x s))
  ; TODO: wrap around
  (if (>= x 1d0) (values (1- ns) 1d0)
                 (truncate (the double-float (* x s)))))


(defun -x-to-pt (vpts ns x)
  (declare (optimize (safety 0) speed (debug 0))
           (type (simple-array double-float) vpts)
           (fixnum ns) (double-float x))
  (multiple-value-bind (seg xloc) (-get-seg ns x)
    (declare (fixnum seg) (double-float xloc))
    (-do-calc vpts xloc seg)))


(defun pos (b x)
  (declare (optimize (safety 0) speed (debug 0))
           (bzspl b) (double-float x))
  (with-struct (bzspl- ns vpts) b
    (declare (fixnum ns) (type (simple-array double-float) vpts))
    (-x-to-pt vpts ns x)))


(defun pos* (b xx)
  (declare (bzspl b) (list xx))
  (with-struct (bzspl- ns vpts) b
    (declare (fixnum ns) (type (simple-array double-float) vpts))
    (loop for x of-type double-float in xx
          collect (-x-to-pt vpts ns x))))


(defun len (b)
  (declare (bzspl b))
  (with-struct (bzspl- ns vpts) b
    (declare (fixnum ns) (type (simple-array double-float) vpts))
    (loop with pts = (adaptive-pos b)
          for a in pts
          and b in (cdr pts)
          summing (vec:dst a b) of-type double-float)))


(defun -remove-s (pts)
  (map 'list #'second pts))

(defun -resappend (res a av)
  (declare (optimize (safety 0) speed (debug 0)) (double-float a) (vec:vec av))
  (if (or (< (length res) 1)
          (destructuring-bind (s _) (vector-last res)
            (> a s)))
    (vextend (list a av) res)))

(defun -midsample (l r)
  (declare (optimize (safety 0) speed (debug 0)) (double-float l r))
  (let* ((d (+ r l))
         (s (* 0.05d0 (- r l))))
    (declare (double-float d s))
    (+ (* 0.5d0 d) (rnd:rnd* s))))

(defun -area (a b c)
  (declare (optimize (safety 0) speed (debug 0)) (vec:vec a b c))
  (abs (* 0.5d0 (+ (* (vec::vec-x a) (- (vec::vec-y b) (vec::vec-y c)))
                   (* (vec::vec-x b) (- (vec::vec-y c) (vec::vec-y a)))
                   (* (vec::vec-x c) (- (vec::vec-y a) (vec::vec-y b)))))))

(defun -adaptive-pos (ns vpts lim
                      &key l r lv rv (res (make-adjustable-vector
                                            :type 'vec:vec)))
  (declare (optimize (safety 0) speed (debug 0))
           (type (simple-array double-float) vpts)
           (fixnum ns) (double-float l r lim))
  (let* ((m (-midsample l r))
         (lv* (if lv lv (-x-to-pt vpts ns l)))
         (rv* (if rv rv (-x-to-pt vpts ns r)))
         (mv* (-x-to-pt vpts ns m))
         (a (-area lv* mv* rv*)))
    (declare (double-float m a) (vec:vec lv* rv* mv*))
    (if (< a lim)
        (progn (-resappend res l lv*)
               (-resappend res r rv*))
        (progn (-adaptive-pos ns vpts lim :l l :r m :lv lv* :rv mv* :res res)
               (-adaptive-pos ns vpts lim :l m :r r :lv mv* :rv rv :res res))))
  res)

(defun adaptive-pos (b &key (lim 0.1d0))
  (declare (bzspl b) (double-float lim))
  (with-struct (bzspl- ns vpts closed) b
    (declare (fixnum ns) (type (simple-array double-float) vpts))
    (if closed
        (let ((m (-midsample 0d0 1d0))
              (res (make-adjustable-vector :type 'vec:vec)))
          (-adaptive-pos ns vpts lim :l 0d0 :r m :res res)
          (-adaptive-pos ns vpts lim :l m :r 1d0 :res res)
          (-remove-s res))
        (-remove-s (-adaptive-pos ns vpts lim :l 0d0 :r 1d0)))))


(defmacro with-rndpos ((b n rn) &body body)
  (declare (symbol rn))
  (with-gensyms (xloc seg b* bns vpts)
    `(let* ((,b* ,b)
            (,bns (bzspl-ns ,b*))
            (,vpts (bzspl-vpts ,b*)))
      (declare (fixnum ,bns)
               (type (simple-array double-float) ,vpts))
      (loop repeat ,n
            do (multiple-value-bind (,seg ,xloc) (-get-seg ,bns (rnd:rnd))
                 (declare (double-float ,xloc) (fixnum ,seg))
                 (let ((,rn (-do-calc ,vpts ,xloc ,seg)))
                   (progn ,@body)))))))


(defun rndpos (b n &key order)
  (declare (bzspl b) (fixnum n))
  (pos* b (rnd:rndspace n 0d0 1d0 :order order)))


(defun -set-v (vpts opts a b)
  (declare (type (simple-array double-float) vpts)
           (type (simple-array vec:vec) opts)
           (fixnum a b))
  (vec:sarr-set vpts b (aref opts a)))


(defun -set-v-mean (vpts opts a b c &aux (cc (* 2 c)))
  (declare (type (simple-array double-float) vpts)
           (type (simple-array vec:vec) opts)
           (fixnum a b c cc))
  (vec:with-xy ((aref opts a) ax ay)
    (vec:with-xy ((aref opts b) bx by)
      (setf (aref vpts cc) (/ (+ ax bx) 2d0)
            (aref vpts (1+ cc)) (/ (+ ay by) 2d0)))))


(defun -set-vpts-open (vpts pts n &aux (n* (- (* (the fixnum 2) n) 3)))
  (declare (fixnum n n*))
  (let ((opts (make-array n :element-type 'vec:vec :initial-contents pts)))
    (loop for i of-type fixnum from 0 below 2
          and k of-type fixnum from (- n* 2)
          and j of-type fixnum from (- n 2)
          do (-set-v vpts opts i i)
             (-set-v vpts opts j k))

    (loop for i of-type fixnum from 1 below (- n 2)
          and i+ of-type fixnum from 1 by 2
          do (-set-v vpts opts i i+)
             (-set-v-mean vpts opts i (+ i 1) (+ i+ 1)))))


(defun -set-vpts-closed (vpts pts n &aux (n* (+ (* 2 n) 1)))
  (declare (fixnum n n*))
  (loop with opts = (make-array n :element-type 'vec:vec :initial-contents pts)
        for i of-type fixnum from 0 below n
        and ii of-type fixnum from 0 by 2
        do (-set-v-mean vpts opts i (mod (+ i 1) n) ii)
           (-set-v vpts opts (mod (+ i 1) n) (+ ii 1))
        finally (-set-v-mean vpts opts 0 1 (- n* 1))
                (return opts)))


(defun -modin (i stp)
  (declare (double-float i stp))
  (let ((a (- i stp)))
    (declare (double-float a))
    (if (< a 0d0) (+ a 1d0) a)))

; TODO: this is probably not very accurate
(defun tangent (bz x &key (scale 1d0) (offset 1d-10)
                     &aux (scale* (* 0.5d0 scale)))
  (declare (bzspl bz)
           (double-float x scale offset scale*))
  (with-struct (bzspl- closed) bz
    (let* ((left (if closed (-modin x offset) (max (- x offset) 0d0)))
           (right (if closed (math:inc x offset) (min 1d0 (+ x offset))))
           (pt (pos bz x))
           (v (vec:norm (vec:sub (pos bz right) (pos bz left)) :s scale*)))
     (list (vec:sub pt v)
           (vec:add pt v)))))


(defun normal (bz x &key (scale 1d0) (offset 1d-10)
                    &aux (scale* (* 0.5d0 scale)))
  (declare (bzspl bz)
           (double-float x scale offset scale*))
  (with-struct (bzspl- closed) bz
    (let* ((left (if closed (-modin x offset) (max (- x offset) 0d0)))
           (right (if closed (math:inc x offset) (min 1d0 (+ x offset))))
           (pt (pos bz x))
           (v (vec:perp (vec:norm (vec:sub (pos bz right) (pos bz left)) :s scale*))))
     (list (vec:sub pt v)
           (vec:add pt v)))))


(defun make (pts &key closed &aux (n (length pts)))
  (declare (list pts) (boolean closed) (fixnum n))
  (assert (>= n 3) (n) "must have at least 3 pts. has ~a." n)
  (let ((vpts (make-array (* 2 (if closed (+ (* 2 n) 1) (- (* 2 n) 3)))
                          :initial-element 0d0
                          :element-type 'double-float)))

    (if closed (-set-vpts-closed vpts pts n)
               (-set-vpts-open vpts pts n))
    (make-bzspl :n n :ns (if closed n (- n 2))
                :vpts vpts :closed closed)))


; TODO: estimate intersection pt.
; TODO: this is slow
(defun bzx (aa bb &key lim)
  (declare (bzspl aa bb))
  (let ((ptsa (adaptive-pos aa :lim lim))
        (ptsb (adaptive-pos bb :lim lim)))
    (vec:psegx ptsa ptsb)))

