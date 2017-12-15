
(in-package :hset)

"
this is a naive wrapper around hash-map. not sure how efficient it will be?
"


(defun add (s e)
  (declare (integer e))
  (the boolean
    (multiple-value-bind (val exists)
      (gethash e s)
      (declare (ignore val))
      (if exists nil (setf (gethash e s) t)))))


(defun add* (s ee)
  (loop for e in ee collect (add s e)))


(defun del (s e)
  (declare (integer e))
  (the boolean
       (remhash e s)))


(defun del* (s ee)
  (loop for e in ee collect (remhash e s)))


(defun mem (s e)
  (declare (integer e))
  (the boolean
       (multiple-value-bind (val exists)
         (gethash e s)
         (declare (ignore val))
         exists)))


(defun mem* (s ee)
  (loop for e in ee collect
    (multiple-value-bind (val exists)
      (gethash e s)
      (declare (ignore val))
      exists)))


(defun num (s)
  (hash-table-count s))


(defun to-list (s)
  (loop for e being the hash-keys of s collect e))


(defun make (&key init (size 1000) (inc 1.5))
  (let ((s (make-hash-table :test #'eql :size size :rehash-size inc)))
    (if init (add* s init))
    s))

