#lang simply-scheme

;; each divisions' personnel-files should be structured in the following format: (make-personnel-file type-tag contents)

;a.
(define (get-record record personnel-file)
    (let ((tag (type-tag personnel-file)))
      (let ((proc (get record tag)))
        (if proc
            (apply proc (contents personnel-file))
            (error
             "No such personnel-file type" tag)))))

;b.
(define (get-salary file)
  (get-record 'salary file))

;c.
(define (find-employee-record name employees)
  (cond ((null? employees) #f)
        ((eq? (get-name (car employees)) name) (car employees))
        (else (find-employee-record name (cdr employees)))))

;d. just have to add the proper procedures to the look-up table