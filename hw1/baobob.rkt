#lang racket

(provide leaf node)
(provide is-empty-tree? is-leaf? leaf-val nth-child)

(define (leaf n)
 (list 'fakeleaf (list 'yeah n) 'hoo))

(define (node l)
 (list->vector (list 'faketree (map (lambda (x) (list x)) l)))
 )

(define (is-empty-tree? t)
 (equal? t (node '())))

(define (is-leaf? t)
 (if (not (vector? t))
 (and (equal? 'fakeleaf (car t)) (equal? '(hoo) (cdr (cdr t))) (equal? 'yeah (car (car (cdr t))))) #f))

(define (leaf-val t)
 (if (is-leaf? t) (car (cdr (car (cdr t)))) 'FAKE!))

(define (nth-child t n)
 (car (list-ref (vector-ref t 1) n)))

