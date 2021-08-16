(define count 0)

(define (test) (set! count (+ count 1)) count)

(define lst (cons (test) (cons (test) '())))

count

(car lst)

count

(car lst) ; would increase count further if thunks are not memoized

count

(car (cdr lst))

count

; in the stream version only the cdr part is delayed, the car part is eagerly evaluated. in this version the 2 parts are equally delayed.