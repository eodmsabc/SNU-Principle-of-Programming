#lang racket
(provide leaf node)
(provide is-empty-tree? is-leaf? leaf-val nth-child)

(define (leaf n) (cons 'l n))

(define (node l) (cons 'n l))

(define (is-empty-tree? t) (if (and (equal? 'n (car t)) (null? (cdr t))) #t #f))

(define (is-leaf? t) (if (equal? (car t) 'l) #t #f))

(define (leaf-val t) (cdr t))

(define (nth-child t n) (list-ref (cdr t) n))
