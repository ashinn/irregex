;; Test of the internal character-set API.  This is here so we can switch
;; representation of csets more easily.

;;; Some of these are based on Olin Shivers' SRFI 14 tests

(cond-expand
  (chicken-5 (import test (chicken irregex)))
  (else (use test extras utils irregex)))

(load "irregex.scm")

(define (vowel? c) (member c '(#\a #\e #\i #\o #\u)))

(test-begin)

(test-assert
 (cset=? (plist->cset '(#\a #\a #\e #\e #\i #\i #\o #\o #\u #\u))
         (string->cset "ioeauaiii")))

(test (plist->cset '(#\x #\y)) (string->cset "xy"))
(test-assert (not (cset=? (plist->cset '(#\x #\x #\y #\y #\z #\z))
                          (string->cset "xy"))))
(test-assert (not (cset=? (plist->cset '(#\x #\z))
                           (string->cset "xy"))))

(test-assert
 (cset=? (string->cset "abcdef12345")
         (cset-union (range->cset (integer->char 97) (integer->char 102))
                     (string->cset "12345"))))

(test-assert
 (cset=? (range->cset #\d #\j)
         (cset-union (plist->cset '(#\d #\f #\h #\j))
                     (string->cset "g"))))

(test-assert
 (cset=? (range->cset #\d #\j)
         (cset-union (string->cset "g")
                     (plist->cset '(#\d #\f #\h #\j)))))

(test-assert
 (not (cset=? (string->cset "abcef12345") ; without the 'd'
              (cset-union (range->cset (integer->char 97) (integer->char 102))
                          (string->cset "12345")))))

(test 10 (cset-size (cset-intersection (sre->cset 'ascii) (sre->cset 'digit))))

(test '(#\x #\x) (cset->plist (plist->cset '(#\x #\x))))
(test-assert (not (equal? '(#\X #\X) (cset->plist (plist->cset '(#\x #\x))))))

(test '(#\a #\d #\x #\z) (cset->plist (plist->cset '(#\a #\d #\x #\z))))

(test-assert (cset-contains? (string->cset "xyz") #\x))
(test-assert (not (cset-contains? (string->cset "xyz") #\a)))

(test-assert (cset-contains? (range->cset #\x #\z) #\x))
(test-assert (not (cset-contains? (range->cset #\x #\z) #\a)))

(let ((cs (plist->cset '(#\a #\c #\h #\j #\l #\l #\n #\n))))
  (test-assert (cset-contains? cs #\a))
  (test-assert (cset-contains? cs #\b))
  (test-assert (cset-contains? cs #\c))
  (test-assert (not (cset-contains? cs #\d)))
  
  (test-assert (cset-contains? cs #\h))
  (test-assert (cset-contains? cs #\i))
  (test-assert (cset-contains? cs #\j))
  (test-assert (not (cset-contains? cs #\k)))
  
  (test-assert (cset-contains? cs #\l))
  (test-assert (not (cset-contains? cs #\m)))

  (test-assert (cset-contains? cs #\n)))

(let ((cs (plist->cset '(#\a #\c #\l #\l #\n #\n))))
  (test-assert (cset-contains? cs #\a))
  (test-assert (cset-contains? cs #\b))
  (test-assert (cset-contains? cs #\c))
  (test-assert (not (cset-contains? cs #\d)))
  
  (test-assert (not (cset-contains? cs #\k)))
  (test-assert (cset-contains? cs #\l))
  (test-assert (not (cset-contains? cs #\m)))

  (test-assert (cset-contains? cs #\n)))

(test-assert
 (cset=? (plist->cset '(#\a #\c #\l #\l #\n #\n))
         (cset-intersection (plist->cset '(#\a #\c #\l #\l #\n #\n))
                            (plist->cset '(#\a #\e #\h #\l #\n #\p)))))

(test-assert
 (cset=? (plist->cset '(#\b #\b #\l #\l #\n #\n))
         (cset-intersection (plist->cset '(#\a #\c #\h #\l #\n #\n))
                            (plist->cset '(#\b #\b #\l #\l #\n #\n)))))

(test-assert
 (cset=? (cset-intersection (sre->cset 'hex-digit)
                            (cset-complement (sre->cset 'digit)))
         (string->cset "abcdefABCDEF")))

(test-assert
 (cset=? (cset-union (sre->cset 'hex-digit)
                     (string->cset "abcdefghijkl"))
         (string->cset "abcdefABCDEFghijkl0123456789")))

(test-assert
 (cset=? (cset-difference (string->cset "abcdefghijklmn")
                          (sre->cset 'hex-digit))
         (string->cset "ghijklmn")))

(test-assert
 (cset=? (string->cset "0123456789")
         (cset-difference (sre->cset 'hex-digit) (sre->cset 'alpha))))
(test-assert
 (cset=? (string->cset "abcdefABCDEF")
         (cset-intersection (sre->cset 'hex-digit) (sre->cset 'alpha))))

(test-end)
