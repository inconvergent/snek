
(proclaim '(optimize speed))
(proclaim '(optimize (space 0)))
(proclaim '(inline last1 single append1 conc1 mklist))
;(declaim '(optimize (debug 3)))

(load "~/quicklisp/setup.lisp")


(ql:quickload "zpng")


(asdf:defsystem "snek"
  :description "SNEK - A Generative System for Writing Generative Systems"
  :version "2.5.2"
  :author "inconvergent"
  :licence "MIT"
  :serial t
  :depends-on ("zpng")
  :components ((:file "pg-utils")
               (:file "various")
               (:file "packages")
               (:file "vec")
               (:file "math")
               (:file "rnd")
               (:file "color")
               (:file "hset")
               (:file "graph")
               (:file "bzspline")
               (:file "linear-path")
               (:file "sandpaint")
               (:file "plot")
               (:file "zmap")
               (:file "snek-macros")
               (:file "snek")
               (:file "snek-utils")
               (:file "snek-alterations")
               (:file "snek-alterations-mutate")
               (:file "snek-extra")))

(asdf:load-system "snek")

