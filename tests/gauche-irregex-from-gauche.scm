(use srfi.64)

(set! test-log-to-file "tests/gauche-irregex-from-gauche.log")

(load "irregex.scm")
(load "irregex-utils.scm")
(load "gauche/test-support.scm")
(load "test-irregex-from-gauche.scm")
(test-exit)
