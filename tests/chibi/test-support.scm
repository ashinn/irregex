(define-syntax test-equal
  (syntax-rules ()
    ((_ exp ...)
     (test exp ...))))
