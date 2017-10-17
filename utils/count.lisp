
(defun make-counter (&optional ll)
  (let ((c (make-hash-table :test #'equal)))
    (loop for a in ll do (counter-add c a))
    c))


(defun counter-add (c a)
  (incf (gethash a c 0)))


(defun counter-show (c)
  (loop with tot = (float (loop for n being the hash-values of c summing n))
        for a being the hash-keys of c using (hash-value n)
        do (format t "~a: ~a, ~a~%" a n (/ (gethash a c) tot))))

