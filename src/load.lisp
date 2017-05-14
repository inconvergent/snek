
(proclaim '(inline last1 single append1 conc1 mklist))
(proclaim '(optimize speed))

(load "../quicklisp/setup")

(ql:quickload "ZPNG")

(load "./src/pg-utils")
(load "./src/utils")
(load "./src/math-utils")
(load "./src/linear-path")
(load "./src/inv-dst-path")
(load "./src/sandpaint")
(load "./src/plot")
(load "./src/zmap")
(load "./src/snek-macros")
(load "./src/snek")
(load "./src/snek-alterations")
(load "./src/snek-alterations-mutate")
(load "./src/snek-utils")

