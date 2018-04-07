
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
  (loop for mrow of-type list in *m* collect
    (let ((s (vec:vec 0d0 0d0)))
      (loop for p of-type vec:vec in pts
            and mr of-type double-float in mrow do
        (setf s (vec:add s (vec:scale p mr))))
      s)))


(defun do-t (x pk)
  (declare (double-float x))
  (declare (list pk))
  (let ((s (vec:vec 0d0 0d0)))
    (loop for p of-type vec:vec in pk
          and xi of-type double-float in (list 1d0 x (* x x)) do
      (setf s (vec:add s (vec:scale p xi))))
    s))


(defun -get-seg (ns x)
  (declare (integer ns))
  (declare (double-float x))
  (let ((s (/ 1d0 (math:dfloat ns))))
    (if (>= x 1d0)
      (values
        1d0
        (- (floor (/ x s)) 1))
      (values
        (/ (mod x s) s)
        (floor (/ x s))))))


(defun -select-pts (vpts seg)
  (declare (integer seg))
  (let ((i (* 2 seg)))
    (list (aref vpts i)
          (aref vpts (+ i 1))
          (aref vpts (+ i 2)))))


(defmacro -x-to-pt (vpts ns x)
  `(multiple-value-bind (x-loc seg)
    (-get-seg ,ns ,x)
    (do-t x-loc (do-m (-select-pts ,vpts seg)))))


(defun pos (b x)
  (declare (bzspl b))
  (declare (double-float x))
  (with-struct (bzspl- ns vpts) b
    (-x-to-pt vpts ns x)))


(defun pos* (b xx)
  (declare (bzspl b))
  (declare (list xx))
  (with-struct (bzspl- ns vpts) b
    (loop for x of-type double-float in xx collect
      (-x-to-pt vpts ns x))))


(defun len (b)
  (declare (bzspl b))
  (let ((res 0d0))
    (with-struct (bzspl- ns vpts) b
      (loop for seg of-type integer from 0 below ns do
        (incf res (-get-segment-length vpts seg))))
    res))


; TODO: this is rather messy.
(defun adaptive-pos (b &key (dens 1d0) (end t))
  (declare (bzspl b))
  (declare (double-float dens))
  (declare (boolean end))
  (let ((res (make-generic-array :type 'vec:vec)))
    (with-struct (bzspl- ns vpts) b
      (loop for seg of-type integer from 0 below ns collect
        (loop for x-loc of-type double-float in
              (math:linspace (ceiling (* (-get-segment-length vpts seg) dens))
                             0d0 1d0 :end (and end (>= seg (1- ns)))) do
          (array-push  (do-t x-loc (do-m (-select-pts vpts seg))) res))))
       (to-list res)))


(defmacro with-rndpos ((b n rn) &body body)
  (with-gensyms (x-loc seg b* bns vpts)
    `(let* ((,b* ,b)
            (,bns (bzspl-ns ,b*))
            (,vpts (bzspl-vpts ,b*)))
      (loop repeat ,n do
        (multiple-value-bind (,x-loc ,seg)
          (-get-seg ,bns (rnd:rnd))
          (let ((,rn (do-t ,x-loc (do-m (-select-pts ,vpts ,seg)))))
            (progn ,@body)))))))


(defun rndpos (b n &key order)
  (declare (bzspl b))
  (declare (integer n))
  (pos* b (if order
            (sort (rnd:rndspace n 0d0 1d0) #'<)
            (rnd:rndspace n 0d0 1d0))))


(defun -set-v (vpts opts a b)
  (setf (aref vpts b)
        (aref opts a)))


(defun -set-v-mean (vpts opts a b c)
  (setf (aref vpts c)
        (vec:scale (vec:add (aref opts a) (aref opts b)) 0.5d0)))


(defun -set-vpts-open (vpts pts n)
  (let ((opts (make-array n :element-type 'list :initial-contents pts))
        (n* (- (* 2 n) 3)))
    (loop for i of-type integer from 0 below 2
          and k of-type integer from (- n* 2)
          and j of-type integer from (- n 2) do
      (-set-v vpts opts i i)
      (-set-v vpts opts j k))

    (loop for i of-type integer from 1 below (- n 2) do
      (let ((j (- (* 2 i) 1)))
        (-set-v vpts opts i j)
        (-set-v-mean vpts opts i (+ i 1) (+ j 1))))))


(defun -set-vpts-closed (vpts pts n)
  (let ((opts (make-array n :element-type 'list :initial-contents pts))
        (n* (+ (* 2 n) 1)))
    (loop for i of-type integer from 0 below n do
      (let ((j (* 2 i)))
        (-set-v-mean vpts opts i (mod (+ i 1) n) j)
        (-set-v vpts opts (mod (+ i 1) n) (+ j 1))))
    (-set-v-mean vpts opts 0 1 (- n* 1))))


(defun -get-segment-length (vpts seg)
  (let ((curr nil)
        (prev 0d0)
        (err 10d0))
    (block iterations
      (loop for c of-type integer from 3 do
        (setf curr
              (let ((samples (loop for xi of-type double-float in
                                     (math:linspace (expt 2 c) 0d0 1d0)
                                   collect
                                     (do-t xi (do-m (-select-pts vpts seg))))))
                (loop for sa of-type vec:vec in samples
                      and sb in (cdr samples)
                      summing (vec:dst sa sb) into l
                      finally (return l))))
        (setf err (abs (- prev curr)))
        (setf prev curr)
        (when (< err 1d-7)
          (return-from iterations))))
    curr))


(defun make (pts &key closed &aux (n (length pts)))
  (declare (list pts))
  (declare (boolean closed))
  (declare (integer n))
  (assert (>= n 3) (n) "must have at least 3 pts. has ~a." n)
  (let ((vpts (make-array (if closed (+ (* 2 n) 1) (- (* 2 n) 3))
                          :initial-element (vec:zero)
                          :element-type 'vec:vec))
        (ns (if closed n (- n 2))))

    (if closed
      (-set-vpts-closed vpts pts n)
      (-set-vpts-open vpts pts n))
    (make-bzspl :n n :ns ns :vpts vpts :closed closed)))


; TODO: estimate intersection pt.
(defun bzx (aa bb &key (dens 1d0))
  (declare (bzspl aa bb))
  (declare (double-float dens))
  (let ((ptsa (adaptive-pos aa :dens dens))
        (ptsb (adaptive-pos bb :dens dens)))
    (vec:segx aa bb)))

