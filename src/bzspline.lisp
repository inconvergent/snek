
(in-package :bzspl)

;(declaim (optimize (safety 0) speed (debug 0)))

;useful info: http://graphics.cs.ucdavis.edu/~joy/ecs178/Unit-7-Notes/MatrixBSpline.pdf

;M = 1/6
;((1 4 1 0)
;(-3 0 3 0)
;(3 -6 3 0)
;(-1 3 -3 1))


;(defvar *m*)
;(setf *m* '((1d0 4d0 1d0 0d0)
;            (-3d0 0d0 3d0 0d0)
;            (3d0 -6d0 3d0 0d0)
;            (-1d0 3d0 -3d0 1d0)))

;(setf *m* '((1d0 0d0 0d0 0d0)
;            (-3d0 3d0 0d0 0d0)
;            (3d0 -6d0 3d0 0d0)
;            (-1d0 3d0 -3d0 1d0)))

;NOTE: this is the transposed matrix compared to the above link
(defparameter *m* '((1d0 -2d0 1d0)
                    (0d0 2d0 -2d0)
                    (0d0 0d0 1d0)))

(declaim (type list *m*))

;(setf *m* '((1d0 1d0 0d0 )
;            (-2d0 2d0 0d0)
;            (1d0 -2d0 1d0)))


(defstruct bzspl
  (n nil :type fixnum :read-only t)
  (ns nil :type fixnum :read-only t)
  (closed nil :type boolean)
  (vpts nil))


(defun do-calc (x pts)
  (declare (optimize (safety 0) speed (debug 0))
           (double-float x)
           (list pts))
  (the vec:vec
    (loop with ttt of-type list = (list 1d0 x (expt x 2d0))
          for pt of-type vec:vec in pts
          and tm of-type double-float in
            (loop for mcol of-type list in *m*
                  collect (loop for mi of-type double-float in mcol
                                and tt of-type double-float in ttt
                                summing (* mi tt) of-type double-float))
          summing (* tm (vec::vec-x pt)) into sx of-type double-float
          summing (* tm (vec::vec-y pt)) into sy of-type double-float
          finally (return (the vec:vec (vec:vec sx sy))))))


(defun -get-seg (ns x &aux (s (/ 1d0 (math:dfloat ns))))
  (declare (optimize (safety 0) speed (debug 0))
           (fixnum ns) (double-float x s))
  (if (>= x 1d0) (values 1d0 (- (truncate (/ x s)) 1))
                 (values (/ (mod x s) s) (truncate (/ x s)))))


(defun -select-pts (vpts seg)
  (declare (optimize (safety 0) speed (debug 0))
           (type vector vpts)
           (fixnum seg))
  (let ((i (* 2 seg)))
    (declare (fixnum i))
    (list (aref vpts i) (aref vpts (+ i 1)) (aref vpts (+ i 2)))))


(defun -x-to-pt (vpts ns x)
  (declare (optimize (safety 0) speed (debug 0))
           (type vector vpts)
           (fixnum ns)
           (double-float x))
  (multiple-value-bind (xloc seg) (-get-seg ns x)
    (declare (fixnum seg) (double-float xloc))
    (do-calc xloc (-select-pts vpts seg))))


(defun pos (b x)
  (declare (optimize (safety 0) speed (debug 0))
           (bzspl b)
           (double-float x))
  (with-struct (bzspl- ns vpts) b
    (declare (fixnum ns) (type vector vpts))
    (-x-to-pt vpts ns x)))


(defun pos* (b xx)
  (declare (bzspl b) (list xx))
  (with-struct (bzspl- ns vpts) b
    (declare (fixnum ns) (type vector vpts))
    (loop for x of-type double-float in xx
          collect (-x-to-pt vpts ns x))))


(defun len (b)
  (declare (bzspl b))
  (with-struct (bzspl- ns vpts) b
    (declare (fixnum ns) (type vector vpts))
    (loop for seg of-type fixnum from 0 below ns
          summing (-get-segment-length vpts seg))))


(defun -get-linspace (vpts seg dens end)
  (math:linspace (ceiling (* (-get-segment-length vpts seg) dens))
                 0d0 1d0 :end end))

(defun adaptive-pos (b &key (dens 1d0) (end t))
  (declare (bzspl b) (double-float dens) (boolean end))
  (with-struct (bzspl- ns vpts) b
    (declare (fixnum ns) (type vector vpts))
    (apply #'append
      (loop for seg of-type fixnum from 0 below ns collect
        (loop for xloc of-type double-float
              in (-get-linspace vpts seg dens (and end (>= seg (1- ns))))
              collect (do-calc xloc (-select-pts vpts seg)))))))


(defmacro with-rndpos ((b n rn) &body body)
  (declare (symbol rn))
  (with-gensyms (xloc seg b* bns vpts)
    `(let* ((,b* ,b)
            (,bns (bzspl-ns ,b*))
            (,vpts (bzspl-vpts ,b*)))
      (declare (fixnum ,bns)
               (type vector ,vpts))
      (loop repeat ,n
            do (multiple-value-bind (,xloc ,seg) (-get-seg ,bns (rnd:rnd))
                 (declare (double-float ,xloc) (fixnum ,seg))
                 (let ((,rn (do-calc ,xloc (-select-pts ,vpts ,seg))))
                   (progn ,@body)))))))


(defun rndpos (b n &key order)
  (declare (bzspl b) (fixnum n))
  (pos* b (rnd:rndspace n 0d0 1d0 :order order)))


(defun -set-v (vpts opts a b)
  (declare (type vector vpts opts) (fixnum a b))
  (setf (aref vpts b) (aref opts a)))


(defun -set-v-mean (vpts opts a b c)
  (declare (type vector vpts opts) (fixnum a b c))
  (vec:with-xy ((aref opts a) ax ay)
    (vec:with-xy ((aref opts b) bx by)
      (setf (aref vpts c) (vec:vec (/ (+ ax bx) 2d0)
                                   (/ (+ ay by) 2d0))))))


(defun -set-vpts-open (vpts pts n &aux (n* (- (* (the fixnum 2) n) 3)))
  (declare (fixnum n n*))
  (let ((opts (make-array n :element-type 'list :initial-contents pts)))
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
  (loop with opts = (make-array n :element-type 'list :initial-contents pts)
        for i of-type fixnum from 0 below n
        and ii of-type fixnum from 0 by 2
        do (-set-v-mean vpts opts i (mod (+ i 1) n) ii)
           (-set-v vpts opts (mod (+ i 1) n) (+ ii 1))
        finally (-set-v-mean vpts opts 0 1 (- n* 1))
                (return opts)))


(defun -get-samples (vpts seg c)
  (declare (fixnum seg c))
  (loop for xi of-type double-float in
          (math:linspace (the fixnum (expt 2 (the fixnum c))) 0d0 1d0)
        collect (do-calc xi (-select-pts vpts seg))))

(defun -get-segment-length (vpts seg &key (lim 1d-7))
  (loop with curr of-type double-float = 0d0
        with prev of-type double-float = 0d0
        with err of-type double-float = 10d0
        for c of-type fixnum from 3
        do (setf curr (loop with samples = (-get-samples vpts seg c)
                            for sa of-type vec:vec in samples
                            and sb of-type vec:vec in (cdr samples)
                            summing (vec:dst sa sb)))
           (setf err (the double-float (abs (- prev curr)))
                 prev (the double-float curr))
           (when (< err lim) (return curr))))


(defun -modin (i stp)
  (declare (double-float i stp))
  (let ((a (- i stp)))
    (declare (double-float a))
    (if (< a 0d0) (+ a 1d0) a)))

; TODO: this is probably not very accurate
(defun tangent (bz pos &key (scale 1d0) (offset 1d-10) &aux (scale* (* 0.5d0 scale)))
  (declare (bzspl bz)
           (double-float pos scale offset scale*))
  (with-struct (bzspl- closed) bz
    (let* ((left (if closed (-modin pos offset) (max (- pos offset) 0d0)))
           (right (if closed (math:inc pos offset) (min 1d0 (+ pos offset))))
           (pt (pos bz pos))
           (v (vec:norm (vec:sub (pos bz right) (pos bz left)) :s scale*)))
     (list (vec:sub pt v)
           (vec:add pt v)))))


(defun normal (bz pos &key (scale 1d0) (offset 1d-10) &aux (scale* (* 0.5d0 scale)))
  (declare (bzspl bz)
           (double-float pos scale offset scale*))
  (with-struct (bzspl- closed) bz
    (let* ((left (if closed (-modin pos offset) (max (- pos offset) 0d0)))
           (right (if closed (math:inc pos offset) (min 1d0 (+ pos offset))))
           (pt (pos bz pos))
           (v (vec:perp (vec:norm (vec:sub (pos bz right) (pos bz left)) :s scale*))))
     (list (vec:sub pt v)
           (vec:add pt v)))))


(defun make (pts &key closed &aux (n (length pts)))
  (declare (list pts)
           (boolean closed)
           (fixnum n))
  (assert (>= n 3) (n) "must have at least 3 pts. has ~a." n)
  (let ((vpts (make-array (if closed (+ (* 2 n) 1) (- (* 2 n) 3))
                          :initial-element vec:*zero*
                          :element-type 'vec:vec)))

    (if closed (-set-vpts-closed vpts pts n)
               (-set-vpts-open vpts pts n))
    (make-bzspl :n n :ns (if closed n (- n 2))
                :vpts vpts :closed closed)))


; TODO: estimate intersection pt.
(defun bzx (aa bb &key (dens 1d0))
  (declare (bzspl aa bb)
           (double-float dens))
  (let ((ptsa (adaptive-pos aa :dens dens))
        (ptsb (adaptive-pos bb :dens dens)))
    (vec:segx ptsa ptsb)))

