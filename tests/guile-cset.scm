(use-modules (srfi srfi-64))

(set! test-log-to-file "tests/guile-cset.log")

(load-from-path "irregex")
(load "guile/test-support")
(load "test-cset")
(test-exit)
