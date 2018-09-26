
(in-package :graph)

"
a simple (undirected) graph structure based on adjacency lists.
"


(defstruct (graph (:constructor -make-graph))
  (size 0 :type fixnum :read-only t)
  (inc 0 :type float :read-only t)
  (num-edges 0 :type fixnum)
  (adj nil :type hash-table)
  (verts nil :type hash-table)
  (closed nil :type boolean)
  (make-hset nil :read-only t))


(defun make (&key (closed nil) (size 1000) (inc 1.5))
  (-make-graph :size size
               :inc inc
               :num-edges 0
               :adj (make-hash-table :test #'eql :size size :rehash-size inc)
               :verts (hset:make :size size :inc inc)
               :closed closed
               :make-hset (lambda (x) (hset:make :init x :size size :inc inc))))


(defun -add (make adj a b)
  (declare (fixnum a b))
  (multiple-value-bind (val exists)
    (gethash a adj)
    (if (not exists)
        (progn (setf val (funcall make (list b))
                         (gethash a adj) val)
               t)
        (hset:add val b))))


(defun add (grph a b)
  (declare (graph grph) (fixnum a b))
  (with-struct (graph- adj make-hset verts) grph
    (if (progn (hset:add* verts (list a b))
               (reduce (lambda (x y) (or x y))
                       (list (-add make-hset adj a b)
                             (-add make-hset adj b a))))
        (progn (incf (graph-num-edges grph) 2)
               t))))


(defun -del (adj a b)
  (declare (fixnum a b))
  (multiple-value-bind (val exists) (gethash a adj)
    (when exists (hset:del val b))))


(defun -prune (adj verts a)
  (declare (fixnum a))
  (multiple-value-bind (val exists) (gethash a adj)
    (if (not exists)
        (hset:del verts a)
        (when (< (hset:num val) 1)
          (progn (remhash a adj)
                 (hset:del verts a))))))


(defun del (grph a b)
  (declare (graph grph) (fixnum a b))
  (with-struct (graph- adj verts) grph
    (if (reduce (lambda (x y) (or x y))
                (list (-del adj a b) (-del adj b a)))
        (progn (-prune adj verts a)
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
  (declare (graph grph) (fixnum a b))
  (with-struct (graph- adj) grph
    (multiple-value-bind (val exists) (gethash a adj)
      (when exists (hset:mem val b)))))


(defun get-edges (grph)
  (declare (graph grph))
  (let ((res (make-adjustable-vector :size (graph-size grph)))
        (adj (graph-adj grph)))
    (declare (type (array list) res) (hash-table adj))
    (loop for a of-type fixnum being the hash-keys of adj
          do (loop for b of-type fixnum being the hash-keys of (gethash a adj)
                   if (<= a b)
                   do (vextend (list a b) res)))
    res))


(defun get-incident-edges (grph v)
  (declare (graph grph) (fixnum v))
  (with-struct (graph- adj) grph
    (let ((a (gethash v adj)))
      (when a (loop for w of-type fixnum being the hash-keys of a
                    collect (sort (list v w) #'<))))))


(defun -do-loop-walk (grph visited path)
  (declare (graph grph) (hash-table visited) (vector path))
  (let ((edges (get-incident-edges grph (vector-last path))))
    (when (not (= (length edges) 2)) (return-from -do-loop-walk nil))
    (loop named lp
          for v of-type fixnum being the hash-keys
            of (hset:make :init (flatten edges))
          if (not (hset:mem visited v))
          do (vextend v path)
             (hset:add visited v)
             (return-from lp t))))

(defun get-loop (grph)
  "
  if the graph is closed this will return the indices of nodes that construct
  the closed loop, or nil, if there is not a single loop.
  "
  (declare (graph grph))
  (with-struct (graph- closed adj) grph
    (when (not closed)
          (error "graph is not closed. use (graph:make ... :closed t)"))
    (when (< (hash-table-count adj) 1) (return-from get-loop nil))
    (let* ((s (loop for k of-type fixnum being the hash-keys of adj
                    repeat 1 return k))
           (path (make-adjustable-vector :init (list s) :type 'fixnum))
           (visited (hset:make :init (list s))))
      (declare (vector path))
      (loop with n of-type fixnum = (hash-table-count adj)
            until (= (length path) n)
            if (not (-do-loop-walk grph visited path))
            do (return-from get-loop nil))
      ;TODO: return this?
      ;(values path ok)
      path)))


(defun get-verts (grph)
  (declare (graph grph))
  (hset:to-list (graph-verts grph)))


(defun vmem (grph v)
  (declare (graph grph) (fixnum v))
  (hset:mem (graph-verts grph) v))


; TODO: the collects here seem strange. improve?
(defmacro with-graph-edges ((grph e) &body body)
  (with-gensyms (adj a b)
    `(let ((,adj (graph-adj ,grph)))
      (loop for ,a of-type fixnum being the hash-keys of ,adj collect
        (loop for ,b of-type fixnum being the hash-keys of (gethash ,a ,adj)
              do (setf ,e (list ,a ,b))
              collect (list ,@body))))))

