

(defstruct (mutate (:constructor -make-mutate))
  (rules nil))


(defun make-mutate ()
  (let ((rules (make-hash-table :test #'equal)))

    (setf
      (gethash 'append-edge-alt rules)
      'mutate-append-edge-alt)

    (setf
      (gethash 'join-verts-alt rules)
      'mutate-join-verts-alt)

    (-make-mutate :rules rules)))


(defun do-mutate (rules a)
  (multiple-value-bind (mut-f exists)
    (gethash (type-of a) rules)
    (if exists
      (funcall mut-f a)
      a)))


(defmacro mutate ((mutate) &body body)
  (with-gensyms (bd mts mut a)
    `(let ((,bd (flatten (list ,@body)))
           (,mut ,mutate))
       (with-struct (mutate- rules) ,mut
         (mapcar (lambda (,a)
                   (if (< (rnd) 0.1)
                     (do-mutate rules ,a)
                     ,a))
                 ,bd)))))


(defun change-ind (i &optional (o 1))
  (let ((ii (+ i (rndi (* -1 o) (+ 1 o)))))
    (if (> ii -1) ii 0)))

;(defun change-ind (i &optional (o 1))
;  (let ((ii (+ i 1)))
;    (if (> ii -1) ii 0)))


(defun mutate-append-edge-alt (a)
  (with-struct (append-edge-alt- v xy rel) a
    (append-edge (change-ind v) xy :rel rel)))


(defun mutate-join-verts-alt (a)
  (with-struct (join-verts-alt- v w) a
    (join-verts (change-ind v) (change-ind w))))

