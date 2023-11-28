(cond-expand
  (chicken-5 (import test))
  (else (use test)))

(load "irregex.scm")
(load "tests/test-irregex-scsh.scm")
(test-exit)
