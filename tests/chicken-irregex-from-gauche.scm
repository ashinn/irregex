(cond-expand
  (chicken-5 (import srfi-1 test))
  (else (use srfi-1 test)))

(load "irregex.scm")
(load "irregex-utils.scm")
(load "tests/test-irregex-from-gauche.scm")
(test-exit)
