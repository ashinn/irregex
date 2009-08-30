;;;; irregex-utils.scm
;;
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define rx-special-chars
  "\\|[](){}.*+?^$#")

(define (string-scan-char str c . o)
  (let ((end (string-length str)))
    (let scan ((i (if (pair? o) (car o) 0)))
      (cond ((= i end) #f)
            ((eqv? c (string-ref str i)) i)
            (else (scan (+ i 1)))))))

(define (irregex-quote str)
  (list->string
   (let loop ((ls (string->list str)) (res '()))
     (if (null? ls)
       (reverse res)
       (let ((c (car ls)))
         (if (string-scan-char rx-special-chars c)
           (loop (cdr ls) (cons c (cons #\\ res)))
           (loop (cdr ls) (cons c res))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (irregex-opt ls)
  (define (make-alt ls)
    (cond ((null? (cdr ls)) (car ls))
          ((every char? ls) (list (list->string ls)))
          (else (cons 'or ls))))
  (define (make-seq ls)
    (cond ((null? (cdr ls)) (car ls))
          ((every (lambda (x) (or (string? x) (char? x))) ls)
           (apply string-append (map (lambda (x) (if (char? x) (string x) x)) ls)))
          (else (cons 'seq ls))))
  (cond
   ((null? ls) "")
   ((null? (cdr ls)) (car ls))
   (else
    (let ((chars (make-vector 256 '())))
      (let lp1 ((ls ls) (empty? #f))
        (if (null? ls)
            (let lp2 ((i 0) (res '()))
              (if (= i 256)
                  (let ((res (make-alt (reverse res))))
                    (if empty? `(? ,res) res))
                  (let ((c (integer->char i))
                        (opts (vector-ref chars i)))
                    (lp2 (+ i 1)
                         (cond
                          ((null? opts) res)
                          ((equal? opts '("")) `(,c ,@res))
                          (else `(,(make-seq (list c (irregex-opt opts)))
                                  ,@res)))))))
            (let* ((str (car ls))
                   (len (string-length str)))
              (if (zero? len)
                  (lp1 (cdr ls) #t)
                  (let ((i (char->integer (string-ref str 0))))
                    (vector-set!
                     chars
                     i
                     (cons (substring str 1 len) (vector-ref chars i)))
                    (lp1 (cdr ls) empty?))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cset->string ls)
  (with-output-to-string
    (lambda ()
      (let lp ((ls ls))
        (unless (null? ls)
          (cond
            ((pair? (car ls))
             (display (irregex-quote (string (caar ls))))
             (display "-")
             (display (irregex-quote (string (cdar ls)))))
            (else (display (irregex-quote (string (car ls))))))
          (lp (cdr ls)))))))

(define (sre->string obj)
  (with-output-to-string
    (lambda ()
      (let lp ((x obj))
        (cond
          ((pair? x)
           (case (car x)
             ((: seq)
              (cond
                ((and (pair? (cddr x)) (pair? (cddr x)) (not (eq? x obj)))
                 (display "(?:") (for-each lp (cdr x)) (display ")"))
                (else (for-each lp (cdr x)))))
             ((submatch) (display "(") (for-each lp (cdr x)) (display ")"))
             ((or)
              (display "(?:")
              (lp (cadr x))
              (for-each (lambda (x) (display "|") (lp x)) (cddr x))
              (display ")"))
             ((* + ?)
              (cond
                ((pair? (cddr x))
                 (display "(?:") (for-each lp (cdr x)) (display ")"))
                (else (lp (cadr x))))
              (display (car x)))
             ((not)
              (cond
                ((and (pair? (cadr x)) (eq? 'cset (caadr x)))
                 (display "[^")
                 (display (cset->string (cdadr x)))
                 (display "]"))
                (else (error "can't represent general 'not' in strings" x))))
             ((cset)
              (display "[")
              (display (cset->string (cdr x)))
              (display "]"))
             ((w/case w/nocase)
              (display "(?")
              (if (eq? (car x) 'w/case) (display "-"))
              (display ":")
              (for-each lp (cdr x))
              (display ")"))
             (else
              (if (string? (car x))
                  (lp `(cset ,@(string->list (car x))))
                  (error "unknown match operator" x)))))
          ((symbol? x)
           (case x
             ((bos bol) (display "^"))
             ((eos eol) (display "$"))
             ((any nonl) (display "."))
             (else (error "unknown match symbol" x))))
          ((string? x)
           (display (irregex-quote x)))
          ((char? x)
           (display (irregex-quote (string x))))
          (else (error "unknown match pattern" x)))))))

