
(use chicken extras regex data-structures srfi-13)
(import irregex)

(define-syntax time-expr
  (syntax-rules ()
    ((time-expr expr)
     (let ((start (nth-value 0 (cpu-time))))
       expr
       (- (nth-value 0 (cpu-time)) start)))))

(define (string-replicate str reps)
  (let lp ((ls '()) (reps reps))
    (if (<= reps 0)
        (string-concatenate-reverse ls)
        (lp (cons str ls) (- reps 1)))))

(define (run-bench name pat str prefix comp-count exec-count)
  (let-syntax
      ((bench (syntax-rules ()
                ((bench variation expr count)
                 (let ((time-taken
                        (time-expr (do ((i count (- i 1)))
                                       ((< i 0))
                                     expr))))
                   (display name) (display ": ")
                   (display variation) (display ": ")
                   (write time-taken) (newline))))))
    (let ((comp-count (string->number comp-count))
          (exec-count (string->number exec-count)))
      ;; compile time
      (bench "compile-time" (string->irregex pat) comp-count)
      (let ((irx (string->irregex pat)))
        ;; match time
        (bench "match-time" (irregex-match irx str) exec-count)
        ;; search times
        (let lp ((mult 1) (reps exec-count))
          (cond
           ((>= reps 10)
            (let ((str (string-append (string-replicate prefix mult) str)))
              (bench (string-append "search prefix x " (number->string mult))
                     (irregex-search irx str)
                     reps)
              (lp (* mult 10) (quotient reps 10))))))))))

(call-with-input-file "re-benchmarks.txt"
  (lambda (in)
    (let lp ()
      (let ((line (read-line in)))
        (cond
         ((eof-object? line))
         ((string-match "^\\s*(?:#.*)?$" line)
          (lp))
         (else
          (let ((ls (string-split line "\t")))
            (apply run-bench ls)
            (lp))))))))

