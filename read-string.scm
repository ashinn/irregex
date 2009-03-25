
(define (read-string . o)
  (let ((in (if (pair? o) (car o) (current-input-port))))
    (let lp ((ls '()))
      (let ((c (read-char in)))
        (if (eof-object? c)
            (list->string (reverse ls))
            (lp (cons c ls)))))))
