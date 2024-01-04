(import (chibi match)
        (chibi test)
        (scheme small)
        (scheme r5rs)
        (srfi 130))

(load "irregex.scm")
(load "tests/test-irregex.scm")
(test-exit)
