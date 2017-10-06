
(defun make-counter (&optional ll)
  (let ((c (make-hash-table :test #'equal)))
    (if ll (loop for a in ll do (counter-add c a)))
    c))


(defun counter-add (c a)
  (multiple-value-bind (val exists)
            (gethash a c)
            (if (not exists)
              (setf (gethash a c) 1)
              (incf (gethash a c)))))


(defun counter-show (c)
  (let ((tot (loop for a being the hash-keys of c
                   summing (gethash a c) into tot
                   finally (return tot))))
    (loop for a being the hash-keys of c do
      (format t "~a: ~a, ~a~%" a
              (gethash a c)
              (coerce (/ (gethash a c) tot) 'float)))))

