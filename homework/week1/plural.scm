#lang simply-scheme
(define (plural wd)
  (define (vowel? letter)
  (member? letter '(a e i o u)))
  (if (and (equal? (last wd) 'y) (not (vowel? (last (bl wd)))))
      (word (bl wd) 'ies)
      (word wd 's)))
