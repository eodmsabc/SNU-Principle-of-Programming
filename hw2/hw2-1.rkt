#lang racket

(provide zipper)

(define (zipper l1 l2) (match l1 ['() l2] [_ (cons (car l1) (zipper l2 (cdr l1)))]))
