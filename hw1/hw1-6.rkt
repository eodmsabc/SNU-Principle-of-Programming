#lang racket
(require "hw1-4.rkt")
(provide zero one not-c and-c or-c)
(provide is-zero? is-one? is-not? is-and? is-or? sub-circuit)

(define zero (leaf 0))
(define one (leaf 1))
(define (not-c c) (node (list (leaf 'not) c)))
(define (and-c c1 c2) (node (list (leaf 'and) c1 c2)))
(define (or-c c1 c2) (node (list (leaf 'or) c1 c2)))

(define (is-zero? c) (if (is-leaf? c) (equal? (leaf-val c) 0) #f))
(define (is-one? c) (if (is-leaf? c) (equal? (leaf-val c) 1) #f))

(define (is-not? c) (if (is-leaf? c) #f (equal? (leaf-val (nth-child c 0)) 'not)))
(define (is-and? c) (if (is-leaf? c) #f (equal? (leaf-val (nth-child c 0)) 'and)))
(define (is-or? c) (if (is-leaf? c) #f (equal? (leaf-val (nth-child c 0)) 'or)))

(define (sub-circuit c n) (nth-child c (+ n 1)))
