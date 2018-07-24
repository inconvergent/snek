
(in-package :obj)


(defstruct obj
  (verts nil :type vector :read-only t)
  (faces nil :type vector :read-only t)
  (lines nil :type vector :read-only t)
  (num-verts 0 :type fixnum :read-only nil)
  (num-lines 0 :type fixnum :read-only nil)
  (num-faces 0 :type fixnum :read-only nil))


(defun make ()
  (make-obj :verts (make-adjustable-vector)
            :lines (make-adjustable-vector)
            :faces (make-adjustable-vector)))


(defun add-verts-from-vec (o new &optional (z 0d0))
  (declare (obj o) (list new) (double-float z))
  (with-struct (obj- verts) o
    (setf (obj-num-verts o) (incf (obj-num-verts o) (length new)))
    (loop with n = (length verts)
          for v in new and i from n
          do (vextend (list (vec::vec-x v) (vec::vec-y v) z) verts)
          collect i)))


(defun add-face (o new)
  (declare (obj o) (list new))
  (with-struct (obj- faces) o
    (setf (obj-num-faces o) (incf (obj-num-faces o)))
    (vextend new faces)))


(defun add-line (o new)
  (declare (obj o) (list new))
  (with-struct (obj- lines) o
    (setf (obj-num-lines o) (incf (obj-num-lines o)))
    (vextend new lines)))


(defun save (o fn &key (mesh-name "mesh"))
  (declare (obj o))
  (with-struct (obj- verts faces lines) o
    (with-open-file (fstream (ensure-filename fn ".obj")
                             :direction :output :if-exists :supersede)
      (declare (stream fstream))
      (format fstream "o ~a~%" mesh-name)
      (loop for ll of-type list across verts
            do (destructuring-bind (x y z) ll
                 (declare (double-float x y z))
                 (format fstream "v ~f ~f ~f~%" x y z)))
      (loop for ee of-type list across faces
            do (destructuring-bind (a b c) (math:add ee '(1 1 1))
                 (declare (fixnum a b c))
                 (format fstream "f ~d ~d ~d~%" a b c)))
      (loop for ll of-type list across lines
            do (format fstream "l")
               (loop for l of-type fixnum in (math:add ll '(1 1 1))
                     do (format fstream " ~d" l))
               (format fstream "~%")))))

