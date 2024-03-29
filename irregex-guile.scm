;;; Install irregex-guile.scm to $GUILE_SITE_DIR/rx/irregex.scm
;;; and irregex.scm to $GUILE_SITE_DIR/rx/source/irregex.scm

(define-module (rx irregex)
  #:export (irregex string->irregex sre->irregex string->sre
	    maybe-string->sre irregex?  irregex-match-data?
	    irregex-new-matches irregex-reset-matches!  irregex-search
	    irregex-search/matches irregex-match
	    irregex-search/chunked irregex-match/chunked irregex-fold/chunked
	    make-irregex-chunker irregex-match-substring
	    irregex-match-subchunk irregex-match-start-chunk
	    irregex-match-start-index irregex-match-end-chunk
	    irregex-match-end-index irregex-match-num-submatches
            irregex-match-names irregex-match-valid-index?
	    irregex-fold irregex-replace irregex-replace/all
	    irregex-dfa irregex-dfa/search
	    irregex-nfa irregex-flags irregex-lengths irregex-names
            irregex-num-submatches irregex-extract irregex-split))

(load-from-path "rx/source/irregex")
