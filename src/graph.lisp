
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
  (multiple-value-bind (val exists)
    (gethash a adj)
    (if (not exists)
      (progn
        (setf val (funcall make (list b))
              (gethash a adj) val)
        t)
      (hset:add val b))))


(defun add (grph e)
  (with-struct (graph- adj make-hset verts) grph
    (if
      (destructuring-bind (a b)
        (math:int* e)
        (hset:add* verts (list a b))
        (reduce (lambda (x y) (or x y))
                (list
                  (-add make-hset adj a b)
                  (-add make-hset adj b a))))
      (progn
        (incf (graph-num-edges grph) 2)
        t))))


(defun -del (adj a b)
  (multiple-value-bind (val exists)
    (gethash a adj)
    (if exists
      (hset:del val b))))


(defun -prune (adj verts a)
  (multiple-value-bind (val exists)
    (gethash a adj)
    (if (not exists)
        (hset:del verts a)
          (if (< (hset:num val) 1)
            (progn
              (remhash a adj)
              (hset:del verts a))))))


(defun del (grph e)
  (with-struct (graph- adj verts) grph
    (destructuring-bind (a b)
      (math:int* e)
      (if
        (reduce (lambda (x y) (or x y))
                (list
                  (-del adj a b)
                  (-del adj b a)))
        (progn
          (-prune adj verts a)
          (-prune adj verts b)
          (incf (graph-num-edges grph) -2)
          t)))))


(defun get-num-edges (grph)
  (graph-num-edges grph))


(defun get-num-verts (grph)
  (hset:num (graph-verts grph)))


(defun mem (grph e)
  (with-struct (graph- adj) grph
    (destructuring-bind (a b)
      (math:int* e)
      (multiple-value-bind (val exists)
        (gethash a adj)
        (if exists
          (hset:mem val b))))))


(defun get-edges (grph)
  (let ((res (make-vec (graph-size grph)))
        (adj (graph-adj grph)))
    (loop for a being the hash-keys of adj do
      (loop for b in (hset:to-list (gethash a adj))
            if (<= a b)
            do
              (vector-push-extend (list a b) res)))
    res))


(defun get-verts (grph)
  (hset:to-list (graph-verts grph)))


(defmacro with-graph-edges ((grph e) &body body)
  (let ((adj (gensym))
        (a (gensym))
        (b (gensym)))
    `(let ((,adj (graph-adj ,grph)))
      (loop for ,a being the hash-keys of ,adj collect
        (loop for ,b in (hset:to-list (gethash ,a ,adj))
              do
                (setf ,e (list ,a ,b))
              collect
                (list ,@body))))))

