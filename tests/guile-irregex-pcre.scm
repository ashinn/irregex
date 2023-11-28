(use-modules (srfi srfi-64))

(set! test-log-to-file "tests/guile-irregex-pcre.log")

(load-from-path "irregex")
(load "guile/test-support")
(load "test-irregex-pcre")
(test-exit)
