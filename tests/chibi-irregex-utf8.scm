(import (chibi test)
        (scheme small)
        (scheme r5rs))

(load "irregex.scm")
(load "tests/chibi/test-support.scm")
(load "tests/test-irregex-utf8.scm")
(test-exit)
