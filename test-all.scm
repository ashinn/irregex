#!/usr/local/bin/csi -script

;; just run this file as "csi -script test-all.scm" to run the full
;; test suite

(use test extras utils irregex)

(test-begin)

(load "test-irregex.scm")
(load "test-irregex-gauche.scm")
(load "test-irregex-scsh.scm")
(load "test-irregex-pcre.scm")
(load "test-irregex-utf8.scm")

(test-end)
(test-exit)

