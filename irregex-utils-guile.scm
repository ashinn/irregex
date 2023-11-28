;;; Install irregex-utils-guile.scm to $GUILE_SITE_DIR/rx/irregex/utils.scm
;;; and irregex-utils.scm to $GUILE_SITE_DIR/rx/source/irregex-utils.scm

(define-module (rx irregex utils)
  #:use-module ((srfi srfi-1) #:select (every))
  #:export (irregex-opt
            irregex-quote
            sre->string))

;;; This procedure is required, but not exported by the (rx irregex).
(define sre->cset (@@ (rx irregex) sre->cset))

(load-from-path "rx/source/irregex-utils")
