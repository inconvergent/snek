
(in-package :bzspl)

;useful info: http://graphics.cs.ucdavis.edu/~joy/ecs178/Unit-7-Notes/MatrixBSpline.pdf

;M = 1/6
;((1 4 1 0)
;(-3 0 3 0)
;(3 -6 3 0)
;(-1 3 -3 1))


(defvar *m*)
;(setf *m* '((1d0 4d0 1d0 0d0)
;            (-3d0 0d0 3d0 0d0)
;            (3d0 -6d0 3d0 0d0)
;            (-1d0 3d0 -3d0 1d0)))

;(setf *m* '((1d0 0d0 0d0 0d0)
;            (-3d0 3d0 0d0 0d0)
;            (3d0 -6d0 3d0 0d0)
;            (-1d0 3d0 -3d0 1d0)))

(setf *m* '((1d0 0d0 0d0 )
            (-2d0 2d0 0d0)
            (1d0 -2d0 1d0)))

;(setf *m* '((1d0 1d0 0d0 )
;            (-2d0 2d0 0d0)
;            (1d0 -2d0 1d0)))


(defstruct bzspl
  (n nil :type integer :read-only t)
  (ns nil :type integer :read-only t)
  (closed nil :type boolean)
  (vpts nil))


(defun do-m (pts)
  (declare (list pts))
  (loop for mrow of-type list in *m*
        collect (loop with s = (vec:vec 0d0 0d0)
                      for p of-type vec:vec in pts
                      and mr of-type double-float in mrow
                      do (setf s (vec:add s (vec:scale p mr)))
                      finally (return s))))


(defun do-t (x pk)
  (declare (double-float x))
  (declare (list pk))
  (loop with s = (vec:vec 0d0 0d0)
        for p of-type vec:vec in pk
        and xi of-type double-float in (list 1d0 x (* x x))
        do (setf s (vec:add s (vec:scale p xi)))
        finally (return s)))


(defun -get-seg (ns x &aux (s (/ 1d0 (math:dfloat ns))))
  (declare (integer ns))
  (declare (double-float x))
  (if (>= x 1d0) (values 1d0 (- (floor (/ x s)) 1))
                 (values (/ (mod x s) s) (floor (/ x s)))))


(defun -select-pts (vpts seg)
  (declare (integer seg))
  (let ((i (* 2 seg)))
    (list (aref vpts i)
          (aref vpts (+ i 1))
          (aref vpts (+ i 2)))))


(defun -x-to-pt (vpts ns x)
  (multiple-value-bind (xloc seg)
    (-get-seg ns x)
    (do-t xloc (do-m (-select-pts vpts seg)))))


(defun pos (b x)
  (declare (bzspl b))
  (declare (double-float x))
  (with-struct (bzspl- ns vpts) b
    (-x-to-pt vpts ns x)))


(defun pos* (b xx)
  (declare (bzspl b))
  (declare (list xx))
  (with-struct (bzspl- ns vpts) b
    (loop for x of-type double-float in xx
          collect (-x-to-pt vpts ns x))))


(defun len (b)
  (declare (bzspl b))
  (with-struct (bzspl- ns vpts) b
    (loop for seg of-type integer from 0 below ns
          summing (-get-segment-length vpts seg))))


(defun -get-linspace (vpts seg dens end)
  (math:linspace (ceiling (* (-get-segment-length vpts seg) dens))
                 0d0 1d0 :end end))

(defun adaptive-pos (b &key (dens 1d0) (end t))
  (declare (bzspl b))
  (declare (double-float dens))
  (declare (boolean end))
  (with-struct (bzspl- ns vpts) b
    (apply #'append
      (loop for seg of-type integer from 0 below ns collect
        (loop for xloc of-type double-float
              in (-get-linspace vpts seg dens (and end (>= seg (1- ns))))
              collect (do-t xloc (do-m (-select-pts vpts seg))))))))


(defmacro with-rndpos ((b n rn) &body body)
  (with-gensyms (xloc seg b* bns vpts)
    `(let* ((,b* ,b)
            (,bns (bzspl-ns ,b*))
            (,vpts (bzspl-vpts ,b*)))
      (loop repeat ,n
            do (multiple-value-bind (,xloc ,seg)
                 (-get-seg ,bns (rnd:rnd))
                   (let ((,rn (do-t ,xloc (do-m (-select-pts ,vpts ,seg)))))
                     (progn ,@body)))))))


(defun rndpos (b n &key order)
  (declare (bzspl b))
  (declare (integer n))
  (pos* b (if order (sort (rnd:rndspace n 0d0 1d0) #'<)
                    (rnd:rndspace n 0d0 1d0))))


(defun -set-v (vpts opts a b)
  (setf (aref vpts b)
        (aref opts a)))


(defun -set-v-mean (vpts opts a b c)
  (setf (aref vpts c)
        (vec:mid (aref opts a) (aref opts b))))


(defun -set-vpts-open (vpts pts n &aux (n* (- (* 2 n) 3)))
  (let ((opts (make-array n :element-type 'list :initial-contents pts)))
    (loop for i of-type integer from 0 below 2
          and k of-type integer from (- n* 2)
          and j of-type integer from (- n 2)
          do (-set-v vpts opts i i)
             (-set-v vpts opts j k))

    (loop for i of-type integer from 1 below (- n 2)
          and i+ of-type integer from 1 by 2
          do (-set-v vpts opts i i+)
             (-set-v-mean vpts opts i (+ i 1) (+ i+ 1)))))


(defun -set-vpts-closed (vpts pts n &aux (n* (+ (* 2 n) 1)))
  (loop with opts = (make-array n :element-type 'list :initial-contents pts)
        for i of-type integer from 0 below n
        and ii of-type integer from 0 by 2
        do (-set-v-mean vpts opts i (mod (+ i 1) n) ii)
           (-set-v vpts opts (mod (+ i 1) n) (+ ii 1))
        finally (-set-v-mean vpts opts 0 1 (- n* 1))
                (return opts)))


(defun -get-samples (vpts seg c)
  (loop for xi of-type double-float in (math:linspace (expt 2 c) 0d0 1d0)
        collect (do-t xi (do-m (-select-pts vpts seg)))))

(defun -get-segment-length (vpts seg &key (lim 1d-7))
  (loop with curr
        with prev = 0d0
        with err = 10d0
        for c of-type integer from 3
        do (setf curr (loop with samples = (-get-samples vpts seg c)
                            for sa of-type vec:vec in samples
                            and sb of-type vec:vec in (cdr samples)
                            summing (vec:dst sa sb)))
           (setf err (abs (- prev curr))
                 prev curr)
           (when (< err lim) (return curr))))



(defun -modin (i stp)
  (let ((a (- i stp)))
    (if (< a 0d0) (+ a 1d0) a)))

; TODO: this is probably not very accurate
(defun tangent (bz pos &key (scale 1d0) (offset 1d-10) &aux (scale* (* 0.5d0 scale)))
  (declare (bzspl bz))
  (declare (double-float pos scale))
  (with-struct (bzspl- closed) bz
    (let* ((left (if closed (-modin pos offset) (max (- pos offset) 0d0)))
           (right (if closed (math:inc pos offset) (min 1d0 (+ pos offset))))
           (pt (pos bz pos))
           (v (vec:norm (vec:sub (pos bz right) (pos bz left)) :s scale*)))
     (list (vec:sub pt v)
           (vec:add pt v)))))


(defun normal (bz pos &key (scale 1d0) (offset 1d-10) &aux (scale* (* 0.5d0 scale)))
  (declare (bzspl bz))
  (declare (double-float pos scale))
  (with-struct (bzspl- closed) bz
    (let* ((left (if closed (-modin pos offset) (max (- pos offset) 0d0)))
           (right (if closed (math:inc pos offset) (min 1d0 (+ pos offset))))
           (pt (pos bz pos))
           (v (vec:perp (vec:norm (vec:sub (pos bz right) (pos bz left)) :s scale*))))
     (list (vec:sub pt v)
           (vec:add pt v)))))


(defun make (pts &key closed &aux (n (length pts)))
  (declare (list pts))
  (declare (boolean closed))
  (declare (integer n))
  (assert (>= n 3) (n) "must have at least 3 pts. has ~a." n)
  (let ((vpts (make-array (if closed (+ (* 2 n) 1) (- (* 2 n) 3))
                          :initial-element (vec:zero)
                          :element-type 'vec:vec)))

    (if closed (-set-vpts-closed vpts pts n)
               (-set-vpts-open vpts pts n))
    (make-bzspl :n n :ns (if closed n (- n 2))
                :vpts vpts :closed closed)))


; TODO: estimate intersection pt.
(defun bzx (aa bb &key (dens 1d0))
  (declare (bzspl aa bb))
  (declare (double-float dens))
  (let ((ptsa (adaptive-pos aa :dens dens))
        (ptsb (adaptive-pos bb :dens dens)))
    (vec:segx ptsa ptsb)))

