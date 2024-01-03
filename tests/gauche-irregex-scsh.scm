(use srfi.64)

(set! test-log-to-file "tests/gauche-irregex-scsh.log")

(load "irregex.scm")
(load "gauche/test-support.scm")
(load "test-irregex-scsh.scm")
(test-exit)
