
(in-package :snek)

(defmacro with ((snk &key (zwidth nil)) &body body)
  "
  creates a context for manipulating snek via alterations.
  all alterations created in this context will be flattened
  and applied to snk at the end of the context.
  "
  (with-gensyms (sname zw aname recursive-do-alts x y bdy)
    `(let* ((,sname ,snk)
            (,zw ,zwidth)
            (,aname (snek-alt-names ,sname)))

      (incf (snek-wc ,sname))

      (labels ((,recursive-do-alts (,x)
                 (cond ((null ,x))
                 ((atom ,x)
                    (if (gethash (type-of ,x) ,aname)
                      ; if atom is also alteration (else ignore):
                      (funcall (gethash (type-of ,x) ,aname) ,sname ,x)))
                 (t (,recursive-do-alts (car ,x))
                    (,recursive-do-alts (cdr ,x))))))

        (zmap:with* (,zw (snek-verts ,sname) (snek-num-verts ,sname)
                        (lambda (,y) (setf (snek-zmap ,sname) ,y)))
          ; this lets @body be executed in the context of zmap:with;
          ; useful if we want to have a parallel context inside zmap.
          (,recursive-do-alts (list ,@body)))))))


(defmacro with-dx ((snk vv dx d) &body body)
  (with-gensyms (sname)
    `(let ((,sname ,snk))
       (let* ((,dx (apply #'vec:isub (get-verts ,sname ,vv)))
              (,d (vec:len ,dx)))
         (declare (double-float ,d))
         (declare (vec:vec ,dx))
         (if (> ,d 0d0)
           (list ,@body))))))


(defmacro with-grp ((snk grp g) &body body)
  "
  select a grp from a snek instance. the grp will be available
  in the context as g.
  "
  (with-gensyms (grps exists gname sname)
    `(let ((,sname ,snk)
           (,gname ,g))
      (let ((,grps (snek-grps ,sname)))
        (multiple-value-bind (,grp ,exists)
          (gethash ,gname ,grps)
            (if (not ,exists)
              (error "attempted to access invalid group: ~a" ,gname))
            ,@body)))))


(defmacro with-rnd-edge ((snk i &key g) &body body)
  "
  select an arbitrary edge from a snek instance. the edge will be
  available in the context as i.

  if a grp is supplied it will select an edge from g, otherwise it will
  use the main grp.
  "
  (with-gensyms (grp edges grph ln)
    `(with-grp (,snk ,grp ,g)
      (let ((,grph (grp-grph ,grp)))
        (let* ((,edges (graph:get-edges ,grph))
               (,ln (length ,edges)))
          (declare (integer ,ln))
          (if (> ,ln 0)
            (let ((,i (aref ,edges (random ,ln))))
              (declare (list ,i))
              (list ,@body))))))))


(defmacro with-rnd-vert ((snk i) &body body)
  "
  select an arbitrary vert from a snek instance. the vert will be
  available in the context as i.
  "
  (with-gensyms (num)
    `(let ((,num (snek-num-verts ,snk)))
       (if (> ,num 0)
         (let ((,i (random ,num)))
           (declare (integer ,i))
           (list ,@body))))))


(defmacro itr-verts ((snk i &key g) &body body)
  "
  iterates over all verts in grp g as i.

  you should use itr-all-verts if you can, as it is faster.

  if g is not provided, the main grp wil be used.
  "
  (with-gensyms (grp sname)
    `(let ((,sname ,snk))
      (with-grp (,sname ,grp ,g)
        (mapcar (lambda (,i) (declare (integer ,i)) (list ,@body))
                (graph:get-verts (grp-grph ,grp)))))))


(defmacro itr-all-verts ((snk i) &body body)
  "
  iterates over all verts in snk as i.
  "
  (with-gensyms (sname)
    `(let ((,sname ,snk))
      (loop for ,i integer from 0 below (snek-num-verts ,sname)
        collect (list ,@body)))))


(defmacro itr-edges ((snk i &key g) &body body)
  "
  iterates over all edges in grp g as i.

  if g is not provided, the main grp will be used.
  "
  (with-gensyms (grp grph)
    `(with-grp (,snk ,grp ,g)
      (let ((,grph (grp-grph ,grp)))
        (map 'list
             (lambda (,i) (list ,@body))
             (graph:get-edges ,grph))))))


; TODO add flag to include nil grp
(defmacro itr-grps ((snk g) &body body)
  "
  iterates over all grps of snk as g.
  "
  (with-gensyms (grps sname)
    `(let ((,sname ,snk))
      (let ((,grps (snek-grps ,sname)))
        (loop for ,g being the hash-keys of ,grps
          if ,g ; ignores nil (main) grp
          collect (list ,@body))))))


(defmacro with-prob (p &body body)
  "
  executes body with probability p.
  "
  (with-gensyms (pname)
    `(let ((,pname ,p))
       (if (< (random 1d0) ,p)
         (list ,@body)))))

