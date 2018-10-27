
(setf *efficiency-note-cost-threshold* 14)


(declaim (optimize (speed 3)))
;(declaim (optimize (safety 3)))
;(declaim (optimize (space 3)))
;(declaim (optimize (debug 3)))
(declaim (inline last1 single append1 conc1 mklist))

(load "~/quicklisp/setup.lisp")


(ql:quickload "zpng")
(ql:quickload "png")
(ql:quickload "cl-svg")


(asdf:defsystem "snek"
  :description "A System for Making Generative Systems"
  :version "2.67.6"
  :author "inconvergent"
  :licence "MIT"
  :serial t
  :depends-on ("zpng" "cl-svg" "png")
  :components ((:file "pg-utils")
               (:file "various")
               (:file "packages")
               (:file "rnd")
               (:file "state")
               (:file "vec")
               (:file "math")
               (:file "math-extra")
               (:file "pigment")
               (:file "hset")
               (:file "rnd-extra")
               (:file "graph")
               (:file "bzspline")
               (:file "linear-path")
               (:file "sandpaint")
               (:file "sandpaint-flip-reflect")
               (:file "sandpaint-extra")
               (:file "draw-svg")
               (:file "draw-tile-svg")
               (:file "obj")
               (:file "zonemap")
               (:file "snek-macros")
               (:file "snek")
               (:file "snek-utils")
               (:file "snek-alterations")
               (:file "snek-extra")))

(asdf:load-system "snek")

(setf *random-state* (make-random-state t))
(setf *print-pretty* t)

