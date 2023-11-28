(cond-expand
  (chicken-5 (import test))
  (else (use test extras utils)))

(load "irregex.scm")
(load "tests/test-irregex-pcre.scm")
(test-exit)
