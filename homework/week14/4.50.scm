(define (analyze-ramb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)				
        (if (null? choices)
            (fail)
            (let ((choice (list-ref choices (random (length choices)))))
              (choice env
                      succeed
                      (lambda ()
                        (try-next (filter (lambda (curr) (not (eq? curr choice))) choices)))))))
      (try-next cprocs))))