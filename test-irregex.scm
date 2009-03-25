#!/usr/local/bin/csi -script

(use test extras utils); regexp pregexp
(load "irregex.scm")

(define (subst-matches matches subst)
  (define (submatch n)
    (if (vector? matches)
        (irregex-match-substring matches n)
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
     (let ((name (sprintf "~A  ~A  ~A  ~A" pattern input result subst)))
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
   (test-group (sprintf "irregex - ~S" opts)
     (with-input-from-file "re-tests.txt"
       (lambda ()
         (port-for-each
          (lambda (line)
            (test-re (lambda (pat str)
                       (irregex-search (apply irregex pat opts) str))
                     line))
          read-line)))))
 '((backtrack)
   (fast)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chunked irregex

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
               (string-intersperse
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
   (test-group (sprintf "irregex/chunked - ~S" opts)
     (with-input-from-file "re-tests.txt"
       (lambda ()
         (port-for-each
          (lambda (line)
            (match (string-split line "\t" #t)
              ((pattern input result subst output)
               (let ((name
                      (sprintf "~A  ~A  ~A  ~A" pattern input result subst)))
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
          read-line)))))
 '((backtrack)
   (fast)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pregexp

'(test-group "pregexp"
   (with-input-from-file "re-tests.txt"
     (lambda ()
       (port-for-each
        (lambda (line) (test-re pregexp-match line))
        read-line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default regex (PCRE)

'(test-group "regex"
   (with-input-from-file "re-tests.txt"
     (lambda ()
       (port-for-each
        (lambda (line) (test-re string-search line))
        read-line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "API"
  (test-assert (irregex? (irregex "a.*b")))
  (test-assert (irregex? (irregex '(: "a" (* any) "b"))))
  (test-assert (not (irregex? (vector '*irregex-tag* #f #f #f #f #f #f #f))))
  (test-assert (not (irregex? (vector #f #f #f #f #f #f #f #f #f))))
  (test-assert (irregex-match-data? (irregex-search "a.*b" "axxxb")))
  (test-assert (irregex-match-data? (irregex-match "a.*b" "axxxb")))
  (test-assert (not (irregex-match-data? (vector '*irregex-match-tag* #f #f #f #f #f #f #f #f #f))))
  (test-assert (not (irregex-match-data? (vector #f #f #f #f #f #f #f #f #f #f #f))))
  (test 0 (irregex-num-submatches (irregex "a.*b")))
  (test 1 (irregex-num-submatches (irregex "a(.*)b")))
  (test 2 (irregex-num-submatches (irregex "(a(.*))b")))
  (test 2 (irregex-num-submatches (irregex "a(.*)(b)")))
  (test 10 (irregex-num-submatches (irregex "((((((((((a))))))))))")))
  (test 0 (irregex-match-num-submatches (irregex-search "a.*b" "axxxb")))
  (test 1 (irregex-match-num-submatches (irregex-search "a(.*)b" "axxxb")))
  (test 2 (irregex-match-num-submatches (irregex-search "(a(.*))b" "axxxb")))
  (test 2 (irregex-match-num-submatches (irregex-search "a(.*)(b)" "axxxb")))
  (test 10 (irregex-match-num-submatches (irregex-search "((((((((((a))))))))))" "a")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "utils"
  (test "h*llo world"
      (irregex-replace "[aeiou]" "hello world" "*"))
  (test "h*ll* w*rld"
      (irregex-replace/all "[aeiou]" "hello world" "*")))

(test-end)


