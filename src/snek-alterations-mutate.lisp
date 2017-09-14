
(in-package :snek)

(defstruct (mutate (:constructor -make-mutate))
  (rules nil)
  (xy nil :type vec:vec)
  (prob 0.0d0 :type double-float)
  (noise 0.0d0 :type double-float)
  (ind nil :type integer))


(defun make-mutate (&key
                     (prob 0.1d0)
                     (noise 100.0d0)
                     (xy (vec:zero))
                     (ind 0))
  (let ((rules (make-hash-table :test #'equal)))

    (setf (gethash 'append-edge-alt rules)
          #'mutate-append-edge-alt)

    (setf (gethash 'join-verts-alt rules)
          #'mutate-join-verts-alt)

    (setf (gethash 'move-vert-alt rules)
          #'mutate-move-vert-alt)

    (setf (gethash 'add-vert-alt rules)
          #'mutate-add-vert-alt)

    (-make-mutate :rules rules
                  :prob (math:dfloat prob)
                  :ind ind
                  :noise (math:dfloat noise)
                  :xy xy)))


(defun -ok-ind (i)
  (if (> i -1) i 0))


(defun do-mutate (rules a mut)
  (multiple-value-bind (mut-f exists)
    (gethash (type-of a) rules)
    (if exists
      (funcall mut-f a mut)
      a)))


(defmacro mutate ((mutate) &body body)
  (with-gensyms (bd mut a)
    `(let ((,bd (flatten (list ,@body)))
           (,mut ,mutate))
       (mapcar (lambda (,a)
                 (if (< (rnd:rnd) (mutate-prob ,mut))
                   (progn
                     ;(setf (mutate-ind ,mut)
                     ;      (-ok-ind (+ (mutate-ind ,mut)
                     ;                 (rnd:rndi -1 2))))
                     ;(setf (mutate-xy ,mut)
                     ;      (math:add (mutate-xy ,mut) (rnd:rnd:in-circ 2.0d0)))
                     (do-mutate (mutate-rules ,mut) ,a ,mut))
                   ,a))
               ,bd))))


;(defun change-ind (i &optional (o 1))
;  (let ((ii (+ i (rnd:rndi (* -1 o) (+ 1 o)))))
;    (if (> ii -1) ii 0)))

(defun mutate-add-vert-alt (a mut)
  (with-struct (add-vert-alt- xy) a
    (add-vert? (vec:add xy (rnd:in-circ (rnd:rnd (mutate-noise mut)))))))


(defun mutate-move-vert-alt (a mut)
  (with-struct (move-vert-alt- v xy rel) a
    (move-vert? v (vec:add xy (rnd:in-circ (mutate-noise mut))) :rel rel)))


(defun mutate-append-edge-alt (a mut)
  (with-struct (mutate- ind) mut
    (with-struct (append-edge-alt- xy rel) a
      (append-edge? ind xy :rel rel))))


(defun mutate-join-verts-alt (a mut)
  (with-struct (mutate- ind) mut
    (with-struct (join-verts-alt- w) a
      (join-verts? ind w))))

