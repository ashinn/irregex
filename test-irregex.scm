#!/usr/local/bin/csi -script

(cond-expand
 (chicken-5 (import test matchable (chicken format) (chicken port) (chicken io) (chicken string)))
 (chicken (use test extras utils matchable))
 (else
  (import (scheme base) (scheme char) (scheme cxr)
          (scheme file) (scheme load) (scheme write)
          (srfi 130) (chibi match) (chibi test))))

(load "irregex.scm")

(define (cat . args)
  (let ((out (open-output-string)))
    (for-each (lambda (x) (display x out)) args)
    (get-output-string out)))

(define (warning . args)
  (for-each (lambda (x) (display x (current-error-port))) args)
  (newline (current-error-port)))

(define (call-with-input-file file proc)
  (let* ((in (open-input-file file))
         (res (proc in)))
    (close-input-port in)
    res))

(define (call-with-input-string str proc)
  (let* ((in (open-input-string str))
         (res (proc in)))
    (close-input-port in)
    res))

(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (let ((res (get-output-string out)))
      (close-output-port out)
      res)))

(define (port-for-each proc read . o)
  (let ((in (if (pair? o) (car o) (current-input-port))))
    (let lp ()
      (let ((x (read in)))
        (unless (eof-object? x)
          (proc x)
          (lp))))))

(define (subst-matches matches subst)
  (define (submatch n)
    (if (vector? matches)
        (and (irregex-match-valid-index? matches n)
             (irregex-match-substring matches n))
        (list-ref matches n)))
  (and
   matches
   (call-with-output-string
     (lambda (out)
       (call-with-input-string subst
         (lambda (in)
           (let lp ()
             (let ((c (read-char in)))
               (cond
                ((not (eof-object? c))
                 (case c
                   ((#\&)
                    (display (or (submatch 0) "") out))
                   ((#\\)
                    (let ((c (read-char in)))
                      (if (char-numeric? c)
                          (let lp ((res (list c)))
                            (if (and (char? (peek-char in))
                                     (char-numeric? (peek-char in)))
                                (lp (cons (read-char in) res))
                                (display
                                 (or (submatch (string->number
                                                (list->string (reverse res))))
                                     "")
                                 out)))
                          (write-char c out))))
                   (else
                    (write-char c out)))
                 (lp)))))))))))

(define (test-re matcher line)
  (match (string-split line "\t" #t)
    ((pattern input result subst output)
     (let ((name (cat pattern "  " input "  " result "  " subst)))
       (cond
        ((equal? "c" result)
         (test-error name (matcher pattern input)))
        ((equal? "n" result)
         (test-assert name (not (matcher pattern input))))
        (else
         (test name output
           (subst-matches (matcher pattern input) subst))))))
    (else
     (warning "invalid regex test line" line))))

(test-begin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic irregex

(for-each
 (lambda (opts)
   (test-group (cat "irregex - " opts)
     (call-with-input-file "re-tests.txt"
       (lambda (in)
         (port-for-each
          (lambda (line)
            (test-re (lambda (pat str)
                       (irregex-search (apply irregex pat opts) str))
                     line))
          read-line
          in)))))
 '((backtrack)
   (fast)
   ))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; chunked irregex

(define (rope . args)
  (map (lambda (x) (if (pair? x) x (list x 0 (string-length x)))) args))

(define rope-chunker
  (make-irregex-chunker
   (lambda (x) (and (pair? (cdr x)) (cdr x)))
   caar
   cadar
   caddar
   (lambda (src1 i src2 j)
     (if (eq? src1 src2)
         (substring (caar src1) i j)
         (let lp ((src (cdr src1))
                  (res (list (substring (caar src1) i (caddar src1)))))
           (if (eq? src src2)
               (string-join
                (reverse (cons (substring (caar src2) (cadar src2) j) res))
                "")
               (lp (cdr src)
                   (cons (substring (caar src) (cadar src) (caddar src))
                         res))))))))

(define (make-ropes str)
  (let ((len (string-length str)))
    (case len
      ((0 1)
       (list (rope str)))
      ((2)
       (list (rope str)
             (rope (substring str 0 1) (substring str 1 2))))
      ((3)
       (list (rope str)
             (rope (substring str 0 1) (substring str 1 3))
             (rope (substring str 0 2) (substring str 2 3))
             (rope (substring str 0 1)
                   (substring str 1 2)
                   (substring str 2 3))))
      (else
       (let ((mid (quotient (+ len 1) 2)))
         (list (rope str)
               (rope (substring str 0 1) (substring str 1 len))
               (rope (substring str 0 mid) (substring str mid len))
               (rope (substring str 0 (- len 1))
                     (substring str (- len 1) len))
               (rope (substring str 0 1)
                     (substring str 1 mid)
                     (substring str mid len))
               ))))))

(define (make-shared-ropes str)
  (let ((len (string-length str)))
    (case len
      ((0 1)
       '())
      ((2)
       (list (list (list str 0 1) (list str 1 2))))
      ((3)
       (list (list (list str 0 1) (list str 1 3))
             (list (list str 0 2) (list str 2 3))
             (list (list str 0 1) (list str 1 2) (list str 2 3))))
      (else
       (let ((mid (quotient (+ len 1) 2)))
         (list (list (list str 0 1) (list str 1 len))
               (list (list str 0 mid) (list str mid len))
               (list (list str 0 (- len 1))
                     (list str (- len 1) len))
               (list (list str 0 1) (list str 1 mid) (list str mid len))
               ))))))

(for-each
 (lambda (opts)
   (test-group (cat "irregex/chunked - " opts)
     (call-with-input-file "re-tests.txt"
       (lambda (in)
         (port-for-each
          (lambda (line)
            (match (string-split line "\t" #t)
              ((pattern input result subst output)
               (let ((name
                      (cat pattern "  " input "  " result "  " subst)))
                 (cond
                  ((equal? "c" result))
                  ((equal? "n" result)
                   (for-each
                    (lambda (rope)
                      (test-assert name
                        (not (irregex-search/chunked pattern
                                                     rope-chunker
                                                     rope))))
                    (append (make-ropes input)
                            (make-shared-ropes input))))
                  (else
                   (for-each
                    (lambda (rope)
                      (test name output
                        (subst-matches (irregex-search/chunked pattern
                                                               rope-chunker
                                                               rope)
                                       subst)))
                    (append (make-ropes input)
                            (make-shared-ropes input)))))))
              (else
               (warning "invalid regex test line" line)))
            )
          read-line
          in)))))
 '((backtrack)
   (fast)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pregexp

'(test-group "pregexp"
   (call-with-input-file "re-tests.txt"
     (lambda (in)
       (port-for-each
        (lambda (line) (test-re pregexp-match line))
        read-line
        in))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default regex (PCRE)

'(test-group "regex"
   (call-with-input-file "re-tests.txt"
     (lambda (in)
       (port-for-each
        (lambda (line) (test-re string-search line))
        read-line
        in))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "unmatchable patterns"
  (test-assert (not (irregex-search '(or) "abc")))
  (test-assert (not (irregex-search '(: "ab" (or)) "abc")))
  (test-assert (not (irregex-search '(submatch "ab" (or)) "abc")))
  (test-assert (not (irregex-search '(: "ab" (submatch (or))) "abc")))
  (test-assert (not (irregex-search '(/) "abc")))
  (test-assert (not (irregex-search '(: "ab" (/)) "abc")))
  (test-assert (not (irregex-search '(~ any) "abc")))
  (test-assert (not (irregex-search '(: "ab" (~ any)) "abc")))
  (test-assert (not (irregex-search '("") "abc")))
  (test-assert (not (irregex-search '(: "ab" ("")) "abc")))
  (test-assert (not (irregex-search '(: (+ print) white) "abc")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "beginning/end of chunks"
  (test-assert
      (irregex-search/chunked '(: bos "foo") rope-chunker '((" foo" 0 4)) 1))
  (test-assert
      (irregex-search/chunked '(: bos "foo") rope-chunker '(("  foo" 1 5)) 2))
  (test-assert
      (irregex-search/chunked '(: bos "foo" eos) rope-chunker '((" foo" 1 4)) 1))
  (test-assert
      (irregex-search/chunked '(: bos "foo" eos) rope-chunker '(("  foo" 2 5)) 2))
  (test-assert
      (irregex-search/chunked '(: bos "foo" eos) rope-chunker '((" foo" 0 4)) 1))
  (test-assert
      (irregex-search/chunked '(: bos "foo" eos) rope-chunker '(("  foo" 1 5)) 2))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "Case sensitivity"
  (test-assert
   (not (irregex-match '(seq "abc") "ABC")))
  (test-assert
   (irregex-match (irregex '(seq "abc") 'case-insensitive) "ABC"))
  (test-assert
   (irregex-match '(w/nocase "abc") "ABC"))
  (test-assert
   (not (irregex-match '(w/nocase (w/case "abc")) "ABC")))
  (test-assert
   (irregex-match '(w/nocase (* ("abc"))) "ABC"))
  (test-assert
   (not (irregex-match '(w/nocase (w/case (* ("abc")))) "ABC")))
  (test-assert
   (irregex-match '(w/nocase (* (/ #\a #\c))) "ABC"))
  (test-assert
   (not (irregex-match '(w/nocase (w/case (/ #\a #\c))) "ABC")))
  (test-assert
   (not (irregex-match '(w/nocase (* (~ (/ #\a #\c)))) "abc")))
  (test-assert
   (not (irregex-match '(w/nocase (* (~ (/ #\a #\c)))) "ABC"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "API"
  (test-group "predicates"
    (test-assert (irregex? (irregex "a.*b")))
    (test-assert (irregex? (irregex '(: "a" (* any) "b"))))
    (test-assert (not (irregex? (vector '*irregex-tag* #f #f #f #f #f #f))))
    (test-assert (not (irregex? (vector #f #f #f #f #f #f #f #f))))
    (test-assert (irregex-match-data? (irregex-search "a.*b" "axxxb")))
    (test-assert (irregex-match-data? (irregex-match "a.*b" "axxxb")))
    (test-assert (not (irregex-match-data? (vector '*irregex-match-tag* #f #f #f #f #f #f #f #f #f))))
    (test-assert (not (irregex-match-data? (vector #f #f #f #f #f #f #f #f #f #f #f)))))
  (test-group "valid index"
    (test-assert
     (irregex-match-valid-index? (irregex-search "a.*b" "axxxb") 0))
    (test-assert
     (not (irregex-match-valid-index? (irregex-search "a.*b" "axxxb") 1)))
    (test-assert
     (not (irregex-match-valid-index? (irregex-search "a.*b" "axxxb") -1)))
    (test-assert
     (irregex-match-valid-index? (irregex-search "a(.*)|(b)" "axxx") 0))
    (test-assert
     (irregex-match-valid-index? (irregex-search "a(.*)|(b)" "axxx") 1))
    (test-assert
     (irregex-match-valid-index? (irregex-search "a(.*)|(b)" "axxx") 2))
    (test-assert
     (irregex-match-valid-index? (irregex-search "a(.*)|(b)" "b") 2))
    (test-assert
     (not (irregex-match-valid-index? (irregex-search "a(.*)(b)" "axxxb") 3)))
    (test-assert
     (not (irregex-match-valid-index? (irregex-search "a(.*)(b)" "axxxb") -1))))
  (test-group "number of submatches"
    (test 0 (irregex-num-submatches (irregex "a.*b")))
    (test 1 (irregex-num-submatches (irregex "a(.*)b")))
    (test 2 (irregex-num-submatches (irregex "(a(.*))b")))
    (test 2 (irregex-num-submatches (irregex "a(.*)(b)")))
    (test 10 (irregex-num-submatches (irregex "((((((((((a))))))))))")))
    (test 0 (irregex-match-num-submatches (irregex-search "a.*b" "axxxb")))
    (test 1 (irregex-match-num-submatches (irregex-search "a(.*)b" "axxxb")))
    (test 2 (irregex-match-num-submatches (irregex-search "(a(.*))b" "axxxb")))
    (test 2 (irregex-match-num-submatches (irregex-search "a(.*)(b)" "axxxb")))
    (test 10 (irregex-match-num-submatches (irregex-search "((((((((((a))))))))))" "a"))))
  (test-group "match substring"
    (test "axxxb" (irregex-match-substring (irregex-search "a.*b" "axxxb") 0))
    (test-error (irregex-match-substring (irregex-search "a.*b" "axxxb") 1))
    (test "xxx" (irregex-match-substring (irregex-search "a(.*)|b" "axxx") 1))
    (test #f (irregex-match-substring (irregex-search "a(.*)|b" "b") 1))
    (test-error (irregex-match-substring (irregex-search "a(.*)|b" "axxx") 2))
    (test-error (irregex-match-substring (irregex-search "a(.*)|b" "b") 2)))
  (test-group "match start-index"
    (test 0 (irregex-match-start-index (irregex-search "a.*b" "axxxb") 0))
    (test-error (irregex-match-start-index (irregex-search "a.*b" "axxxb") 1))
    (test 1 (irregex-match-start-index (irregex-search "a(.*)|b" "axxx") 1))
    (test #f (irregex-match-start-index (irregex-search "a(.*)|b" "b") 1))
    (test-error (irregex-match-start-index (irregex-search "a(.*)|b" "axxx") 2))
    (test-error (irregex-match-start-index (irregex-search "a(.*)|b" "b") 2)))
  (test-group "match end-index"
    (test 5 (irregex-match-end-index (irregex-search "a.*b" "axxxb") 0))
    (test-error (irregex-match-end-index (irregex-search "a.*b" "axxxb") 1))
    (test 4 (irregex-match-end-index (irregex-search "a(.*)|b" "axxx") 1))
    (test #f (irregex-match-end-index (irregex-search "a(.*)|b" "b") 1))
    (test-error (irregex-match-end-index (irregex-search "a(.*)|b" "axxx") 2))
    (test-error (irregex-match-end-index (irregex-search "a(.*)|b" "b") 2)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "utils"
  (test "h*llo world"
      (irregex-replace "[aeiou]" "hello world" "*"))
  (test "hello world"
      (irregex-replace "[xyz]" "hello world" "*"))
  (test "h*ll* w*rld"
      (irregex-replace/all "[aeiou]" "hello world" "*"))
  (test '("bob@test.com" "fred@example.com")
      (irregex-fold 'email
                    (lambda (i m s) (cons (irregex-match-substring m) s))
                    '()
                    "bob@test.com and fred@example.com"
                    (lambda (i s) (reverse s))))
  (test '("bob@test.com" "fred@example.com")
      (irregex-fold/chunked
       'email
       (lambda (src i m s) (cons (irregex-match-substring m) s))
       '()
       rope-chunker
       (rope "bob@test.com and fred@example.com")
       (lambda (src i s) (reverse s))))
  (test '("poo poo ")
      (irregex-fold '(* "poo ")
                    (lambda (i m s)
                      (if (< i (irregex-match-end-index m 0))
                          (cons (irregex-match-substring m) s)
                          s))
                    '()
                    "poo poo platter"))
  (test "*  x   "
      (irregex-replace/all
       (irregex '(: bos #\space) 'backtrack) "   x   " "*"))
  (test "*  x   "
      (irregex-replace/all
       (irregex '(: bos #\space) 'dfa) "   x   " "*"))
  (test "***x***"
      (irregex-replace/all
       (irregex '(: #\space) 'backtrack) "   x   " "*"))
  (test "***x***"
      (irregex-replace/all
       (irregex '(: #\space) 'dfa) "   x   " "*"))
  (test "xaac"
      (irregex-replace/all
       (irregex '(or (seq bos "a") (seq bos "b")) 'backtrack) "aaac" "x"))
  (test "xaac"
      (irregex-replace/all
       (irregex '(or (seq bos "a") (seq bos "b")) 'dfa) "aaac" "x"))
  (test "xaac"
      (irregex-replace/all (irregex '(or (seq bos "a") "b") 'backtrack)
                           "aaac" "x"))
  (test "xaac"
      (irregex-replace/all (irregex '(or (seq bos "a") "b") 'dfa)
                           "aaac" "x"))
  (test "*Line 1\n*Line 2"
      (irregex-replace/all 'bol "Line 1\nLine 2" "*"))
  (test "**p*l*a*t*t*e*r"
      (irregex-replace/all '(* "poo ") "poo poo platter" "*"))
  (test "x- y- z-"
      (irregex-replace/all '(: (look-behind (or "x" "y" "z")) "a")
                           "xa ya za"  "-"))
  (test '("foo" " " "foo" " " "b" "a" "r" " " "foo")
      (irregex-extract '(or (: bow "foo" eow) any) "foo foo bar foo"))
  (test '("f" "o" "o" "b" "a" "r" "b" "a" "z")
      (irregex-split (irregex "") "foobarbaz"))
  (test '("f" "b" "r" "b" "z")
      (irregex-split (irregex "[aeiou]*") "foobarbaz"))
  (test '("" "oo" "" "a" "" "" "a" "")
      (irregex-extract (irregex "[aeiou]*") "foobarbaz"))
  (test '("Line 1\n" "Line 2\n" "Line 3")
      (irregex-split 'bol "Line 1\nLine 2\nLine 3"))
  )

(define (extract name irx str)
  (irregex-match-substring (irregex-match irx str) name))
(define (valid? name irx str)
  (irregex-match-valid-index? (irregex-match irx str) name))
(define (start-idx name irx str)
  (irregex-match-start-index (irregex-match irx str) name))
(define (end-idx name irx str)
  (irregex-match-end-index (irregex-match irx str) name))

(test-group "named submatches"
  (test "matching submatch is seen and extracted"
        "first" (extract 'first `(or (submatch-named first "first")
                                     (submatch-named second "second"))
                         "first"))
  (test-assert "matching submatch index is valid"
               (valid? 'first `(or (submatch-named first "first")
                                   (submatch-named second "second"))
                       "first"))
  (test "nonmatching submatch is known but returns false"
        #f
        (extract 'second `(or (submatch-named first "first")
                              (submatch-named second "second"))
                 "first"))
  (test-assert "nonmatching submatch index is valid"
               (valid? 'second `(or (submatch-named first "first")
                                    (submatch-named second "second"))
                       "first"))
  (test-error "nonexisting submatch is unknown and raises an error"
              (extract 'third `(or (submatch-named first "first")
                                   (submatch-named second "second"))
                       "first"))
  (test-assert "nonexisting submatch index is invalid"
               (not (valid? 'third `(or (submatch-named first "first")
                                         (submatch-named second "second"))
                            "first")))
  (test "matching alternative is used"
        "first" (extract 'sub `(or (submatch-named sub "first")
                                   (submatch-named sub "second"))
                         "first"))
  (test "matching alternative is used (second match)"
        "second" (extract 'sub `(or (submatch-named sub "first")
                                    (submatch-named sub "second"))
                         "second"))
  (test "last match is used with multiple matches for a name"
        "second" (extract 'sub `(seq (submatch-named sub "first")
                                     space
                                     (submatch-named sub "second"))
                         "first second"))
  (test "submatch start"
        1 (start-idx 'xs `(seq "a" (submatch-named xs (+ "x")) "b") "axxxb"))
  (test-error "unknown submatch start"
              (start-idx 'xs `(seq "a" (submatch-named ys (+ "x")) "b") "axxxb"))
  (test "submatch end"
        4 (end-idx 'xs `(seq "a" (submatch-named xs (+ "x")) "b") "axxxb"))
  (test-error "unknown submatch start"
              (end-idx 'xs `(seq "a" (submatch-named ys (+ "x")) "b") "axxxb")))

;; This is here to help optimized implementations catch segfaults and
;; other such problems.  These calls will always return errors in plain
;; Scheme, but only because it will try to use the invalid object in a
;; way that's not supported by the operator.  Once Scheme grows a
;; standardized way of signaling and catching exceptions, these tests
;; should be changed and expanded to check for specific condition types,
;; and probably moved to the group where the procedure is being tested.
(test-group "error handling"
  (test-error (irregex 'invalid-sre))
  (test-error (string->irregex 'not-a-string))
  (test-error (sre->irregex 'invalid-sre))
  
  (test-error (irregex-search 'not-an-irx-or-sre "foo"))
  (test-error (irregex-search "foo" 'not-a-string))
  (test-error (irregex-search "foo" "foo" 'not-a-number))
  (test-error (irregex-search "foo" "foo" 0 'not-a-number))
  (test-error (irregex-search "foo" "foo" 1.0))
  (test-error (irregex-search "foo" "foo" 0 1.0))

  ;; TODO: irregex-new-matches, irregex-reset-matches!
  ;; irregex-search/matches, make-irregex-chunker?

  (test-error (irregex-match-valid-index? 'not-a-match-object 0))
  (test-error (irregex-match-valid-index?
               (irregex-search "a(.*)(b)" "axxxb") 1.0))
  (test-error (irregex-match-start-index 'not-a-match-object 0))
  (test-error (irregex-match-start-index (irregex-search "foo" "foo") -1))
  (test-error (irregex-match-start-index
               (irregex-search '(submatch "foo") "foo") 1.0))
  (test-error (irregex-match-end-index 'not-a-match-object 0))
  (test-error (irregex-match-end-index (irregex-search "foo" "foo") -1))
  (test-error (irregex-match-end-index
               (irregex-search '(submatch "foo") "foo") 1.0))
  
  (test-error (irregex-match-start-chunk 'not-a-match-object 0))
  (test-error (irregex-match-end-chunk 'not-a-match-object 0))
  (test-error (irregex-match-substring 'not-a-match-object 0))
  (test-error (irregex-match-substring (irregex-search "a(.*)(b)" "axxxb") 1.0))
  (test-error (irregex-match-subchunk 'not-a-match-object 0))
  (test-error (irregex-match-num-submatches 'not-a-match-object))
  (test-error (irregex-match-names 'not-a-match-object))
  (test-error (irregex-num-submatches 'not-an-irx))
  (test-error (irregex-names 'not-an-irx))
  
  (test-error (irregex-fold 'not-an-irx (lambda x x) 0 "foo" (lambda x x) 0 3))
  (test-error (irregex-fold "foo" 'not-a-proc 0 "foo" (lambda x x) 0 3))
  (test-error (irregex-fold "foo" (lambda (a b) b) 0 'not-a-string
                            (lambda x x) 0 3))
  (test-error (irregex-fold "foo" (lambda (a b) b) 0 "foo" 'not-a-proc 0 3))
  (test-error (irregex-fold "foo" (lambda (a b) b) 0 "foo" (lambda x x)
                            'not-a-number 3))
  (test-error (irregex-fold "foo" (lambda (a b) b) 0 "foo" (lambda x x) 0
                            'not-a-number))
  (test-error (irregex-fold "foo" (lambda (a b) b) 0 "foo" (lambda x x) 1.0 3))
  (test-error (irregex-fold "foo" (lambda (a b) b) 0 "foo" (lambda x x) 0 1.0))

  (test-error (irregex-replace 'not-an-irx "str"))
  (test-error (irregex-replace "foo" "foo" (lambda (x) 'not-a-string)))
  (test-error (irregex-replace/all 'not-an-irx "str"))
  (test-error (irregex-replace/all "foo" "foo" (lambda (x) 'not-a-string)))

  ;; Are these supposed to be exported?
  ;; irregex-nfa, irregex-dfa, irregex-dfa/search, irregex-dfa/extract
  ;; irregex-flags, irregex-lengths
  )

(test-end)

