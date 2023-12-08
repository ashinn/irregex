(use-modules (srfi srfi-64))

(set! test-log-to-file "tests/guile-irregex-scsh.log")

(load-from-path "irregex")
(load "guile/test-support")
(load "test-irregex-scsh")
(test-exit)
