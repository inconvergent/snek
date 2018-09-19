#!/usr/bin/sbcl --script

(load "../src/load")



(defun get-make-chr (vert-num* char-rad scale)
  (lambda (p)
    (loop for xy in (rnd:nin-circ (rnd:rndi* vert-num*) (rnd:rnd char-rad))
          collect (vec:add p (vec:mult xy scale)))))


(defun get-make-word (make-chr char-num* char-rad)
  (lambda (p)
    (vec:with-xy (p x y)
      (let ((char-num (rnd:rndi* char-num*)))
        (let ((char-pos-x (math:linspace char-num
                                         x
                                         (+ x (* char-num char-rad)))))
          (list (first (reverse char-pos-x))
                (loop for x* in char-pos-x
                      collect (funcall make-chr (vec:vec x* y)))))))))


(defun get-make-words (make-word space-width)
  (lambda (num-words p)
    (vec:with-xy (p x y)
      (let ((x* x))
        (loop for i from 0 below num-words
          collect (destructuring-bind (last-x-pos word)
                    (funcall make-word (vec:vec x* y))
                    (setf x* (+ space-width last-x-pos))
                    (flatten word)))))))



(defun main (size fn)
  (let ((border 100d0)
        (char-rad 18d0)
        (line-num 20)
        (num-words 10)
        (char-scale (vec:vec 0.5d0 1.5d0))
        (sand (sandpaint:make size
                              :fg (pigment:black 0.009)
                              :bg (pigment:white))))


    (loop for y in (math:linspace line-num border (- size border)) do
      (format t "~a ~%" y)
      (let* ((drift (rnd:get-acc-circ-stp*))
             (snk (snek:make))
             (get-words
               (get-make-words
                 (get-make-word (get-make-chr (list 2 4) char-rad char-scale)
                                (list 3 10)
                                (* 0.5 char-rad))
                 char-rad)))

        ;(snek:with (snk)
        ;  (snek:itr-verts (snk v)
        ;    (snek:move-vert? v (funcall drift 0.04d0))))

        (loop for word in (funcall get-words num-words (vec:vec border y))
              do (snek:add-path! snk word :g (snek:add-grp! snk)))

        (loop for i from 0 below 200 do
          (snek:with (snk)
            (snek:itr-verts (snk v)
              (snek:move-vert? v (rnd:in-circ 0.4d0))))
          (snek:itr-grps (snk g :collect nil)
            (sandpaint:bzspl-stroke sand
              (bzspl:make (snek:get-grp-verts snk :g g))
              200)))))

    (sandpaint:save sand fn :gamma 1.5)))

(time (main 1000 (second (cmd-args))))

