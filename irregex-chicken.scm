
(cond-expand
 (compiling
  (declare
   (export
    irregex string->irregex sre->irregex
    string->sre maybe-string->sre
    irregex? irregex-match-data?
    irregex-new-matches irregex-reset-matches!
    irregex-search irregex-search/matches irregex-match
    irregex-search/chunked irregex-match/chunked make-irregex-chunker
    irregex-match-substring irregex-match-subchunk
    irregex-match-start-source irregex-match-start-index
    irregex-match-end-source irregex-match-end-index
    irregex-match-num-submatches
    irregex-fold irregex-replace irregex-replace/all
    irregex-dfa irregex-dfa/search irregex-dfa/extract
    irregex-nfa irregex-flags irregex-lengths irregex-names
    irregex-num-submatches
    ))))

