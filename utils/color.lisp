

(defun get-color-walker (n alpha &optional closed)
  (let ((spla (bzspl:make (rnd:nin-box n 0.5d0 0.5d0 :xy (vec:vec 0.5d0)) :closed closed))
        (splb (bzspl:make (rnd:nin-box n 0.5d0 0.5d0 :xy (vec:vec 0.5d0)) :closed closed)))
    (lambda (i)
      (vec:with-xy ((bzspl:pos spla i) h s)
        (vec:with-xy ((bzspl:pos splb i) v _)
          (color:hsv h s v alpha))))))

