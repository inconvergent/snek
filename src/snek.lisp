
(defpackage :snek (:use :common-lisp))



; MACROS

(defmacro with-snek ((snk) &body body)
  (with-gensyms (sname)
    `(let ((,sname ,snk))
      (incf (snek-wc ,sname))
      (do-alts
         (remove-if-not
           #'alt-p
           (flatten (list ,@body)))
         ,sname))))


(defmacro with-snek-print ((snk) &body body)
  (with-gensyms (aname sname)
    `(let ((,sname ,snk))
      (incf (snek-wc ,sname))
      (let ((,aname (remove-if-not
                       #'alt-p
                       (flatten (list ,@body)))))
        (print ,aname)
        (do-alts ,aname ,sname)))))


(defmacro with-rnd-edge ((snk i) &body body)
  (with-gensyms (num-edges edges sname)
    `(let ((,sname ,snk))
      (let ((,num-edges (snek-num-edges ,sname))
            (,edges (snek-edges ,sname)))
        (if (> ,num-edges 0)
          (let ((,i (get-as-list ,edges (random ,num-edges))))
            (list ,@body))
          nil)))))


(defmacro with-rnd-vert ((snk i) &body body)
  `(let ((,i (random (snek-num-verts ,snk))))
    (list ,@body)))


(defmacro with-rnd-vert-value ((snk i) &body body)
  `(let ((,i (rnd-get (snek-verts ,snk))))
    (list ,@body)))


(defmacro with-all-verts ((snk i) &body body)
  (with-gensyms (num-verts sname)
    `(let ((,sname ,snk))
      (let ((,num-verts (snek-num-verts,sname)))
        (loop for ,i from 0 below ,num-verts
          collect (list ,@body))))))


(defmacro with-all-edges ((snk i) &body body)
  (with-gensyms (num-edges sname kname edges)
    `(let ((,sname ,snk))
      (let ((,num-edges (snek-num-edges ,sname))
            (,edges (snek-edges ,sname)))
        (loop
          with ,i
          for ,kname from 0 below ,num-edges
          do
            (setf ,i (get-as-list ,edges ,kname))
          if (< (first ,i) (second ,i))
          collect (list ,@body))))))


(defmacro with-rnd-verts ((snk u v) &body body)
  `(with-rnd-vert (,snk ,u)
    (with-rnd-vert (,snk ,v)
      ,@body)))


(defmacro with-verts-closer-than (r (snk u v) &body body)
  (with-gensyms (rname sname)
    `(let ((,sname ,snk)
           (,rname ,r))
      (with-all-verts (,snk ,u)
        (with-all-verts (,snk ,v)
          (if (< (edge-length ,sname (list ,u ,v)) ,rname)
            (list ,@body)))))))


(defmacro with-prob (p &body body)
  (with-gensyms (pname)
    `(let ((,pname ,p))
       (if (< (random 1.0) ,p)
         (list ,@body)))))



; SNEK

(defstruct snek
  (edges nil :read-only nil)
  (verts nil :read-only nil)
  (num-edges 0 :type integer :read-only nil)
  (num-verts 0 :type integer :read-only nil)
  (wc 0 :type integer :read-only nil)
  (max-num 100000 :type integer :read-only t))


(defun snek* (&optional (max-num 100000))
  (make-snek
    :edges (make-int-array max-num)
    :verts (make-dfloat-array max-num)
    :max-num max-num))


(defun -edge-compare (a b c d)
  (or
    (and (>= a b) (>= c d))
    (> a b)))


(defun -binary-edge-insert-search (arr target num)
  (let ((left 0)
        (right (1- num)))
    (do () ((< right left) left)
      (let ((mid (floor (+ left right) 2)))
        (cond
          ((not (-edge-compare
                  (first target)
                  (aref arr mid 0)
                  (second target)
                  (aref arr mid 1)))
            (setf right (1- mid)))
          (t
            (setf left (1+ mid))))))))


;TODO: is this tail recursive?
(defun -binary-edge-search (arr target num &key (left 0) (right nil))
  (destructuring-bind (a c) target
    (if (eql right nil)
      (setf right (1- num)))
    (let ((mid (floor (+ left right) 2)))
        (cond
          ((< right left) nil)
          ((and
             (eql a (aref arr mid 0))
             (eql c (aref arr mid 1)))
            mid)
          ((not (-edge-compare a (aref arr mid 0) c (aref arr mid 1)))
            (-binary-edge-search arr target num :left left :right (1- mid)))
          (t (-binary-edge-search arr target num :left (1+ mid) :right right))))))


(defun -insert-edge (edges edge pos num)
  (loop for i from 0 below (- num pos) do
    (let ((left (- num (1+ i)))
          (right (- num i)))
      (setf (aref edges right 0) (aref edges left 0))
      (setf (aref edges right 1) (aref edges left 1))))
    (setf (aref edges pos 0) (first edge))
    (setf (aref edges pos 1) (second edge)))


(defun -find-insert-edge (edges num e)
  (-insert-edge
    edges
    e
    (-binary-edge-insert-search edges e num)
    num)
  e)


(defun -remove-edge (edges pos num)
  (loop for i from pos to (- num 2) do
    (setf (aref edges i 0) (aref edges (1+ i) 0))
    (setf (aref edges i 1) (aref edges (1+ i) 1)))
  (set-from-list edges (1- num) (list 0 0)))


(defun -find-remove-edge (edges num e)
  (let ((p (-binary-edge-search edges e num)))
    (if p
      (progn
        (-remove-edge edges p num)
        1)
      0)))


(defun insert-vert (snk xy)
  (with-struct (snek- verts num-verts) snk
    (destructuring-bind (x y)
      (to-dfloat* xy)
      (setf (aref verts num-verts 0) x)
      (setf (aref verts num-verts 1) y)
      (- (incf (snek-num-verts snk)) 1))))


(defun get-vert (snk v)
  (with-struct (snek- verts) snk
    (list (aref verts v 0)
          (aref verts v 1))))


(defun insert-edge (snk e)
  (with-struct (snek- edges num-edges) snk
    (cond
      ((-binary-edge-search edges e num-edges)
       nil)
      ((eql (first e) (second e))
       nil)
      (t
        (setf (snek-num-edges snk) (+ 2 num-edges))
        (-find-insert-edge edges num-edges e)
        (sort
          (-find-insert-edge edges (1+ num-edges) (reverse e))
          #'<)))))


(defun remove-edge (snk e)
  (with-struct (snek- edges num-edges) snk
     (setf (snek-num-edges snk) (- num-edges (loop for i in
       (list
         (-find-remove-edge edges num-edges e)
         (-find-remove-edge edges (1- num-edges) (reverse e)))
       sum i)))
     (- num-edges (snek-num-edges snk))))


; TODO: binary search
(defun get-one-ring (snk v)
  (let ((num (snek-num-edges snk))
        (edges (snek-edges snk)))
    (loop for i from 0 below num
      if (eql v (aref edges i 0))
      collect (get-as-list edges i))))



; ALTERATIONS


; MOVE VERT

(defstruct (move-vert-alt
    (:constructor move-vert (v xy &key (rel t))))
  (rel t :type boolean :read-only t)
  (xy nil :type list :read-only t)
  (v -1 :type integer :read-only t))


(defun do-move-vert-alt (a snk)
  (let ((verts (snek-verts snk)))
    (with-struct (move-vert-alt- v xy rel) a
      (let ((fxy (to-dfloat* xy)))
        (set-from-list
          verts
          v
          (if rel
            (add (get-as-list verts v) fxy)
            fxy))))))


(defun -get-force-alterations (u v f)
  (list
    (move-vert v f)
    (move-vert u (scale f -1.0d0))))


(defmacro force (snk v1 v2 r)
  (with-gensyms (vname v1name v2name rname)
    `(let ((,vname (snek-verts ,snk))
           (,v1name ,v1)
           (,v2name ,v2)
           (,rname ,r))
      (-get-force-alterations
        ,v1 ,v2
        (scale
          (nsub
            (get-as-list ,vname ,v1name)
            (get-as-list ,vname ,v2name))
          ,rname)))))



; APPEND EDGE

(defstruct (append-edge-alt
    (:constructor append-edge (v xy &key (rel t))))
  (xy nil :type list :read-only t)
  (v -1 :type integer :read-only t)
  (rel t :type boolean :read-only t))


(defun do-append-edge-alt (a snk)
  (with-struct (append-edge-alt- v xy rel) a
    (cond
      (rel (insert-vert snk (add (get-vert snk v) xy)))
      (t (insert-vert snk xy)))

    (insert-edge snk
      (list
        v
        (1- (snek-num-verts snk))))))


; JOIN VERTS

(defstruct (join-verts-alt
    (:constructor join-verts (v1 v2)))
  (v1 -1 :type integer :read-only t)
  (v2 -1 :type integer :read-only t))


(defun do-join-verts-alt (a snk)
  (with-struct (join-verts-alt- v1 v2) a
    (insert-edge snk (list v1 v2))))


; SPLIT EDGE

(defstruct (split-edge-alt
    (:constructor split-edge (e)))
  (e nil :type list :read-only t))


(defun do-split-edge-alt (a snk)
  (with-struct (split-edge-alt- e) a
    (let ((res (remove-edge snk e))
          (verts (snek-verts snk)))
      (destructuring-bind (a b) e
        (if (> res 1)
          (let ((c (insert-vert snk
                      (lmid (list (get-as-list verts a)
                                  (get-as-list verts b))))))
            (insert-edge snk (list a c))
            (insert-edge snk (list c b))))))))


; ALTERATION UTILS

; IMPORTANT: alts must only contain alterations.
(defun do-alts (alts snk)
  (dolist (a alts)
    (funcall (symb 'do- (type-of a)) a snk)))


; TODO: it should be possible to add arbitrary alterations.
(defun alt-p (a)
  (member
    (type-of a)
    '(move-vert-alt join-verts-alt
      append-edge-alt split-edge-alt)))


; OTHER UTILS

(defun edge-length (snk e)
  (with-struct (snek- verts) snk
    (apply #'dst (mapcar (lambda (v) (get-as-list verts v)) e))))


(defun snek-init-circle (snk num rad &key (x 0.0d0) (y 0.0d0))
  (let ((verts (loop for i from 0 below num collect
    (insert-vert snk
      (add
        (list x y)
        (scale (cos-sin (/ (* i PI 2.0d0) num)) rad))))))

    (loop for i from 0 below num do
      (insert-edge snk (list (nth i verts) (nth (mod (1+ i) num) verts))))))

(defun snek-init-line (snk num a b)
  (let ((verts (loop for i from 0 below num collect
    (insert-vert snk (on-line i num a b)))))

    (loop for i from 0 below num do
      (insert-edge snk (list (nth i verts) (nth (mod (1+ i) num) verts))))))


(defun show-snek-edges (snk)
  (let ((edges (snek-edges snk)))
    (loop for i from 0 below (snek-num-edges snk) do
      (print (aref edges i 0))
      (prin1 (aref edges i 1)))))


(defun show-snek-verts (snk)
  (let ((verts (snek-verts snk)))
    (loop for i from 0 below (snek-num-verts snk) do
      (print (aref verts i 0))
      (prin1 (aref verts i 1)))))


(defun snek-draw-edges (snk sand grains)
  (with-struct (snek- verts edges num-edges) snk
    (if (> num-edges 0)
      (sandpaint:strokes
        sand
          (loop
            with ee
            for i from 0 below num-edges
            do
              (setf ee (get-as-list edges i))
            if (< (first ee) (second ee))
            collect
              (list
                (get-as-list verts (first ee))
                (get-as-list verts (second ee))))
        grains))))


(defun snek-draw-verts (snk sand)
  (with-struct (snek- verts num-verts) snk
    (sandpaint:pix* sand verts num-verts)))

