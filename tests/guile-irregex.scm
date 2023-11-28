(use-modules (ice-9 match)
             (ice-9 rdelim)
             (srfi srfi-64))

;;; Implement the bare minimum of chicken's string-split that we need.
(define %string-split string-split)
(define (string-split s delim-s keepempty)
  (if (not keepempty)
      (error "keepempty: only #t case is implemented"))
  (if (not (= 1 (string-length delim-s)))
      (error "Delimiter string can only have a single character"))
  (%string-split s (string->char-set delim-s)))

(set! test-log-to-file "tests/guile-irregex.log")

(load-from-path "irregex")
(load "guile/test-support")
(load "test-irregex")
(test-exit)
