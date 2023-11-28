(use srfi.64)
(use util.match)

;;; Implement the bare minimum of chicken's string-split that we need.
(define %string-split string-split)
(define (string-split s delim-s keepempty)
  (if (not keepempty)
      (error "keepempty: only #t case is implemented"))
  (%string-split s delim-s))

(set! test-log-to-file "tests/gauche-irregex.log")

(load "irregex.scm")
(load "gauche/test-support.scm")
(load "test-irregex.scm")
(test-exit)
