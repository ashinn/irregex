(cond-expand
  (chicken-5 (import test))
  (else (use test extras utils)))

(load "irregex.scm")
(load "tests/chicken/test-support.scm")
(load "tests/test-irregex-utf8.scm")
(test-exit)
