(if (not (test-runner-current))
    (test-runner-current (test-runner-create)))

(define (test-exit)
  (let ((test-runner (test-runner-current)))
    (exit (if (or (> (test-runner-fail-count test-runner) 0)
                  (> (test-runner-xpass-count test-runner) 0))
              #f
              #t))))
