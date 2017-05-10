

(defstruct (mutate (:constructor -make-mutate))
  (rules nil)
  (ind nil :type integer))


(defun make-mutate ()
  (let ((rules (make-hash-table :test #'equal)))

    (setf
      (gethash 'append-edge-alt rules)
      'mutate-append-edge-alt)

    (setf
      (gethash 'join-verts-alt rules)
      'mutate-join-verts-alt)

    (-make-mutate :rules rules :ind 0)))


(defun do-mutate (rules a o)
  (multiple-value-bind (mut-f exists)
    (gethash (type-of a) rules)
    (if exists
      (funcall mut-f a o)
      a)))


(defmacro mutate ((mutate) &body body)
  (with-gensyms (bd mts mut a)
    `(let ((,bd (flatten (list ,@body)))
           (,mut ,mutate))
       (mapcar (lambda (,a)
                 (if (< (rnd) 0.2)
                   (progn
                     (incf (mutate-ind ,mut) (rndi -1 2))
                     (do-mutate (mutate-rules ,mut) ,a (mutate-ind ,mut))
                   )
                   ,a))
               ,bd))))


;(defun change-ind (i &optional (o 1))
;  (let ((ii (+ i (rndi (* -1 o) (+ 1 o)))))
;    (if (> ii -1) ii 0)))

(defun change-ind (i o)
  (let ((ii o))
    (if (> ii -1) ii 0)))


(defun mutate-append-edge-alt (a o)
  (with-struct (append-edge-alt- v xy rel) a
    (append-edge? (change-ind v o) xy :rel rel)))


(defun mutate-join-verts-alt (a o)
  (with-struct (join-verts-alt- v w) a
    (join-verts? (change-ind v o) w)))

