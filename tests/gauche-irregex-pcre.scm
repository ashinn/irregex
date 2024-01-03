(use srfi.64)

(set! test-log-to-file "tests/gauche-irregex-pcre.log")

(load "irregex.scm")
(load "gauche/test-support.scm")
(load "test-irregex-pcre.scm")
(test-exit)
