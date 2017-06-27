
(defmacro with-snek ((snk &key (zwidth nil)) &body body)
  "
  creates a context for manipulating snek via alterations.
  all alterations created in this context will be flattened
  and applied to snk at the end of the context.
  "
  (with-gensyms (sname zw alt-names)
    `(let ((,sname ,snk)
           (,zw ,zwidth))
      (let ((,alt-names (snek-alt-names ,sname)))
        (incf (snek-wc ,sname))
        (if ,zw
          (setf (snek-zwidth ,sname) ,zw
                (snek-zmap ,sname) (zmap:make (snek-verts ,sname)
                                              (snek-num-verts ,sname)
                                              (to-dfloat ,zw))))
        (do-alts
          (remove-if-not
            (lambda (x) (gethash (type-of x) ,alt-names))
            (flatten (list ,@body)))
          ,sname)))))


(defmacro with-dx ((snk vv dx d) &body body)
  (with-gensyms (sname)
    `(let ((,sname ,snk))
       (let* ((,dx (apply #'isub (get-verts ,sname ,vv)))
              (,d (len ,dx)))
         (if (> d 0.0d0)
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
  (with-gensyms (grp num-edges edges)
    `(with-grp (,snk ,grp ,g)
      (let ((,num-edges (grp-num-edges ,grp))
            (,edges (grp-edges ,grp)))
        (if (> ,num-edges 0)
          (let ((,i (get-atup ,edges (random ,num-edges))))
            (list ,@body)))))))


(defmacro with-rnd-vert ((snk i &key g) &body body)
  "
  select an arbitrary vert from a snek instance. the vert will be
  available in the context as i.

  if a grp is supplied it will select a vert from g, otherwise it will
  use the main grp.
  "
  (with-gensyms (grp num-verts tmpv)
    `(with-grp (,snk ,grp ,g)
      (let ((,num-verts (length (grp-verts ,grp))))
        (if (> ,num-verts 0)
          (let ((,tmpv (random ,num-verts)))
            (let ((,i (aref (grp-verts ,grp) ,tmpv)))
              (list ,@body))))))))


(defmacro itr-verts ((snk i &key g) &body body)
  "
  iterates over all verts in grp g (either nil or a specific grp).
  the current vert is named i.
  "
  (with-gensyms (k gv grp num-verts sname)
    `(let ((,sname ,snk))
      (with-grp (,sname ,grp ,g)
        (let ((,num-verts (length (grp-verts ,grp)))
              (,gv (grp-verts ,grp)))
          (loop
            with ,i
            for ,k from 0 below ,num-verts
            do
              (setf ,i (aref ,gv ,k))
            collect
              (list ,@body)))))))


(defmacro itr-all-verts ((snk i) &body body)
  "
  iterates over all verts in snk regardless of their grp.
  the current vert is named i.
  "
  (with-gensyms (k verts num-verts sname)
    `(let ((,sname ,snk))
      (let ((,num-verts (snek-num-verts ,sname))
            (,verts (snek-verts ,sname)))
        (loop
          for ,i from 0 below ,num-verts
          collect
            (list ,@body))))))


(defmacro itr-edges ((snk i &key g) &body body)
  "
  iterates over all edges in snk as i, or all edges in grp g of snek.
  the current edge is named i
  "
  (with-gensyms (grp num-edges k edges)
    `(with-grp (,snk ,grp ,g)
      (let ((,num-edges (grp-num-edges ,grp))
            (,edges (grp-edges ,grp)))
        (loop
          with ,i
          for ,k from 0 below ,num-edges
          do
            (setf ,i (get-atup ,edges ,k))
          if (< (first ,i) (second ,i))
          collect (list ,@body))))))


(defmacro itr-grps ((snk g) &body body)
  "
  iterates over all grps of snk.
  the current grp is named g.
  "
  (with-gensyms (grps sname)
    `(let ((,sname ,snk))
      (let ((,grps (snek-grps ,sname)))
        (loop for ,g being the hash-keys of ,grps
          if ,g ; ignores nil (main) grp
          collect
          (list ,@body))))))


(defmacro with-prob (p &body body)
  "
  executes the form body with probability p.
  "
  (with-gensyms (pname)
    `(let ((,pname ,p))
       (if (< (random 1.0) ,p)
         (list ,@body)))))


