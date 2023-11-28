(use srfi.64)

(set! test-log-to-file "tests/gauche-cset.log")

(load "irregex.scm")
(load "gauche/test-support.scm")
(load "test-cset.scm")
(test-exit)
