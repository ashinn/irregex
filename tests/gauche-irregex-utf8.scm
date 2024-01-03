(use srfi.64)

(set! test-log-to-file "tests/gauche-irregex-utf8.log")

(load "irregex.scm")
(load "gauche/test-support.scm")
(load "test-irregex-utf8.scm")
(test-exit)
