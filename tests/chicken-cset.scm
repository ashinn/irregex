(cond-expand
  (chicken-5 (import test (chicken irregex)))
  (else (use test extras utils irregex)))

(load "irregex.scm")
(load "tests/test-cset.scm")
(test-exit)
