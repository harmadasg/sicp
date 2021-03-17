#lang simply-scheme

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

; 2.67
(define sample-tree
  (make-code-tree (make-leaf 'E 4)
                  (make-code-tree
                   (make-leaf 'I 2)
                   (make-code-tree (make-leaf 'G 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(1 1 0 0 1 1 0 1 0))
(decode sample-message sample-tree)

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; 2.68
(define (encode-symbol symbol tree)
    (let ((left (left-branch tree))
          (right (right-branch tree)))
      (cond ((leaf? tree) null)
            ((member? symbol (symbols left)) (cons 0 (encode-symbol symbol left)))
            ((member? symbol (symbols right)) (cons 1 (encode-symbol symbol right)))
            (else (error "symbol can't be found")))))

(encode '(G E G I) sample-tree)

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; 2.69
(define (successive-merge leaf-set)
    (if (null? (cdr leaf-set))
        (car leaf-set)
        (successive-merge
         (adjoin-set
          (make-code-tree (car leaf-set) (cadr leaf-set))
          (cddr leaf-set)))))

; 2.70
(define rock-alphabet '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))
(define rock-tree (generate-huffman-tree rock-alphabet))
(count (append
        (encode '(GET A JOB) rock-tree)
        (encode '(SHA NA NA NA NA NA NA NA NA) rock-tree)
        (encode '(GET A JOB) rock-tree)
        (encode '(SHA NA NA NA NA NA NA NA NA) rock-tree)
        (encode '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP) rock-tree)
        (encode '(SHA BOOM) rock-tree)))