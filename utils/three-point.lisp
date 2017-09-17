
(defun -make-front-path (aa bb cc as bs)
  (let ((p1 (vec:add cc (vec:scale (vec:sub aa cc) as)))
        (p2 (vec:add cc (vec:scale (vec:sub bb cc) bs))))
    (list p1 cc p2)))


(defun -make-full-path (aa bb cc as bs)
  (let ((p1 (vec:add cc (vec:scale (vec:sub aa cc) as)))
        (p2 (vec:add cc (vec:scale (vec:sub bb cc) bs))))
    (list p1 cc p2 (vec:add p2 (vec:scale (vec:sub aa p2) as)) p1)))


(defun make-perspective-transform (a b c)
  (lambda (p a* b* u* d*)
    (let ((pc (vec:sub c p)))
      (let ((u (vec:sub p (vec:scale pc u*)))
            (d (vec:add p (vec:scale pc d*))))
        (append (-make-full-path a b u a* b*) (-make-full-path a b d a* b*))))))

