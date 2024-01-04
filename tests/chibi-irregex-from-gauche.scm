(import (chibi test)
        (scheme small)
        (scheme r5rs)
        (srfi 1))

(load "irregex.scm")
(load "irregex-utils.scm")
(load "tests/test-irregex-from-gauche.scm")
(test-exit)
