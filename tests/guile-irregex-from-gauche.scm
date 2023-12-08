(use-modules (srfi srfi-64))

(set! test-log-to-file "tests/guile-irregex-from-gauche.log")

(load-from-path "irregex")
(load-from-path "irregex-utils")
(load "guile/test-support")
(load "test-irregex-from-gauche")
(test-exit)
