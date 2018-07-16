
(in-package :obj)


(defstruct obj
  (verts nil :read-only nil)
  (faces nil :read-only nil)
  (num-verts 0 :type fixnum :read-only nil)
  (num-faces 0 :type fixnum :read-only nil))


(defun make ()
  (make-obj
    :verts (make-adjustable-vector)
    :faces (make-adjustable-vector)))


(defun add-verts-from-vec (o new &optional (z 0d0))
  (with-struct (obj- verts) o
    (setf (obj-num-verts o) (incf (obj-num-verts o) (length new)))
    (let ((n (length verts)))
      (loop for v in new and i from n do
        (vextend (list (vec::vec-x v) (vec::vec-y v) z) verts)
        collect i))))


(defun add-face (o new)
  (with-struct (obj- faces) o
    (setf (obj-num-faces o) (incf (obj-num-faces o)))
    (vextend new faces)))


(defun save (o fn &key (mesh-name "mesh"))
  (with-struct (obj- verts faces) o
    (with-open-file (stream (ensure-filename fn ".obj")
                            :direction :output :if-exists :supersede)
      (format stream "o ~a~%" mesh-name)
      (dolist (ll (coerce verts 'list))
        (destructuring-bind (x y z) ll
        (format stream "v ~f ~f ~f~%" x y z)))
      (when faces
        (dolist (ee (coerce faces 'list))
          (destructuring-bind (a b c)
            (math:add ee '(1 1 1))
            (format stream "f ~d ~d ~d~%" a b c)))))))

