
(in-package :graph)

"
a simple (undirected) graph structure based on adjacency lists.
"


(defstruct (graph (:constructor -make-graph))
  (size 0 :type integer :read-only t)
  (inc 0 :type float :read-only t)
  (num-edges 0 :type integer)
  (adj nil :type hash-table)
  (verts nil :type hash-table)
  (make-hset nil :read-only t))


(defun make (&key (size 1000) (inc 1.5))
  (-make-graph
    :size size
    :inc inc
    :num-edges 0
    :adj (make-hash-table :test #'eql :size size :rehash-size inc)
    :verts (hset:make :size size :inc inc)
    :make-hset (lambda (x) (hset:make :init x :size size :inc inc))))


(defun -add (make adj a b)
  (declare (integer a b))
  (multiple-value-bind (val exists)
    (gethash a adj)
    (if (not exists)
      (progn
        (setf val (funcall make (list b))
              (gethash a adj) val)
        t)
      (hset:add val b))))


(defun add (grph a b)
  (declare (graph grph))
  (declare (integer a b))
  (with-struct (graph- adj make-hset verts) grph
    (if
      (progn
        (hset:add* verts (list a b))
        (reduce (lambda (x y) (or x y))
                (list
                  (-add make-hset adj a b)
                  (-add make-hset adj b a))))
      (progn
        (incf (graph-num-edges grph) 2)
        t))))


(defun -del (adj a b)
  (declare (integer a b))
  (multiple-value-bind (val exists)
    (gethash a adj)
    (if exists
      (hset:del val b))))


(defun -prune (adj verts a)
  (declare (integer a))
  (multiple-value-bind (val exists)
    (gethash a adj)
    (if (not exists)
        (hset:del verts a)
          (if (< (hset:num val) 1)
            (progn
              (remhash a adj)
              (hset:del verts a))))))


(defun del (grph a b)
  (declare (graph grph))
  (declare (integer a b))
  (with-struct (graph- adj verts) grph
    (if
      (reduce (lambda (x y) (or x y))
              (list
                (-del adj a b)
                (-del adj b a)))
      (progn
        (-prune adj verts a)
        (-prune adj verts b)
        (incf (graph-num-edges grph) -2)
        t))))


(defun get-num-edges (grph)
  (declare (graph grph))
  (graph-num-edges grph))


(defun get-num-verts (grph)
  (declare (graph grph))
  (hset:num (graph-verts grph)))


(defun mem (grph a b)
  (declare (graph grph))
  (declare (integer a b))
  (with-struct (graph- adj) grph
    (multiple-value-bind (val exists)
      (gethash a adj)
      (if exists
        (hset:mem val b)))))


(defun get-edges (grph)
  (declare (graph grph))
  (let ((res (make-vec (graph-size grph)))
        (adj (graph-adj grph)))
    (declare (type (array list) res))
    (declare (hash-table adj))
    (loop for a integer being the hash-keys of adj do
      (loop for b integer in (hset:to-list (gethash a adj))
        if (<= a b)
        do (vector-push-extend (list a b) res)))
    res))


(defun get-incident-edges (grph v)
  (declare (graph grph))
  (declare (integer v))
  (with-struct (graph- adj) grph
    (let ((a (gethash v adj)))
      (if a
        (loop for w integer being the hash-keys of a collect
              (sort (list v w) #'<))
        nil))))


(defun get-verts (grph)
  (declare (graph grph))
  (hset:to-list (graph-verts grph)))


(defmacro with-graph-edges ((grph e) &body body)
  (with-gensyms (adj a b)
    `(let ((,adj (graph-adj ,grph)))
      (loop for ,a integer being the hash-keys of ,adj collect
        (loop for ,b integer in (hset:to-list (gethash ,a ,adj))
              do
                (setf ,e (list ,a ,b))
              collect
                (list ,@body))))))

