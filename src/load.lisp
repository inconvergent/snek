
(proclaim '(optimize speed))
(proclaim '(optimize (space 2)))
(proclaim '(inline last1 single append1 conc1 mklist))
;(declaim '(optimize (debug 3)))

(load "~/quicklisp/setup.lisp")


(ql:quickload "zpng")
(ql:quickload "cl-svg")


(asdf:defsystem "snek"
  :description "SNEK - A Generative System for Writing Generative Systems"
  :version "2.10.0"
  :author "inconvergent"
  :licence "MIT"
  :serial t
  :depends-on ("zpng" "cl-svg")
  :components ((:file "pg-utils")
               (:file "various")
               (:file "packages")
               (:file "vec")
               (:file "math")
               (:file "color")
               (:file "hset")
               (:file "graph")
               (:file "rnd")
               (:file "bzspline")
               (:file "linear-path")
               (:file "sandpaint")
               (:file "plot")
               (:file "plot-svg")
               (:file "obj")
               (:file "zmap")
               (:file "snek-macros")
               (:file "snek")
               (:file "snek-utils")
               (:file "snek-alterations")
               (:file "snek-alterations-mutate")
               (:file "snek-extra")))

(asdf:load-system "snek")

