(cond-expand
 (chicken-5 (import test matchable (chicken format) (chicken port) (chicken io) (rename (chicken string) (string-intersperse string-join))))
 (chicken (use test extras utils matchable))
 (else
  (import (scheme base) (scheme char) (scheme cxr)
          (scheme file) (scheme load) (scheme write)
          (srfi 130) (chibi match) (chibi test))))

(load "irregex.scm")
(load "tests/test-irregex.scm")
(test-exit)
