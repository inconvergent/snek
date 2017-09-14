#!/usr/bin/sbcl --script

(load "../src/load")

(setf *print-pretty* t)
;(setf *random-state* (make-random-state t))


(defun init-snek (n size xy)
  (let ((snk (snek:make :max-verts 5000000)))

    (snek:add-path! snk
                    (math:rep (p (math:linspace n 0 1 :end nil))
                         (math:on-circ p 20d0 :xy xy))
                    :closed t)
    snk))



(defun main (size fn)
  (let ((itt 1100)
        (grains 30)
        (snk (init-snek 20 size (vec:rep (math:dfloat (half size)))))
        (farl 100d0)
        (stp 0.004d0)
        (sand (sandpaint:make size
                :active (color:white 0.05)
                :bg (color:dark))))

    (start-timer "total")
    (loop for i from 0 below itt do
      (start-timer "with")
      (snek:with (snk :zwidth farl)
        (start-timer "in-with")
        (snek:itr-all-verts (snk v)
          (snek:move-vert? v (rnd:in-circ (* stp 5d0)))
          (map 'list (lambda (w)
                       (with-timer ("snek:with-dx")
                         (snek:with-dx (snk (list v w) dx d)
                           (let ((ndx (vec:iscale dx d))
                                 (s (* (- 1.0d0 (/ d farl)) stp)))
                             (list
                               (snek:move-vert? v (vec:scale ndx (* -1 s)))
                               (snek:move-vert? w (vec:scale ndx s)))))))
                     (with-timer ("in-rad")
                       (snek:verts-in-rad snk (snek:get-vert snk v) farl))))
        (snek:itr-edges (snk e)
          (snek:with-dx (snk e dx d)
            (let ((ndx (vec:iscale dx d)))
              (cond ((> d 8.d0)
                      (snek:split-edge? e))

                    ((> d 2.d0)
                      (list
                        (snek:move-vert? (first e) (vec:scale ndx stp))
                        (snek:move-vert? (second e) (vec:scale ndx (* -1 stp)))))))))
        (sum-timer "in-with"))
      (sum-timer "with")

      (if (= (mod i 100) 0)
        (progn
          (format t "itt ~a, num verts ~a ~%"
                  i (snek::snek-num-verts snk))
          ;(snek:itr-edges (snk e)
          ;  (sandpaint:lin-path sand
          ;    (close-path (snek:get-verts snk e))
          ;    3 grains))
          ;(sandpaint:save sand (append-number fn i) :gamma 1.5)
          ;(sandpaint:save sand fn :gamma 1.5)
          ;(sandpaint:clear sand (color:dark))
          )))
      (sum-timer "total")
      (show-timers)))


;(time (main 1000 (second (cmd-args))))

;(sb-profile:profile "common-lisp-user.snek:move-vert?")
;(main 1000 "asdf")
;(sb-profile:report)

(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 200000
                         :mode :cpu
                         ;:mode :alloc
                         ;:mode :time
                         :report :graph)
 (time (main 1000 "asdf")))

