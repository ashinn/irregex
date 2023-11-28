(use-modules (srfi srfi-64))

(set! test-log-to-file "tests/guile-irregex-utf8.log")

(load-from-path "irregex")
(load "guile/test-support")
(load "test-irregex-utf8")
(test-exit)
