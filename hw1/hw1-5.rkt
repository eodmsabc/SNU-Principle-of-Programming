#lang racket
(require "hw1-4.rkt")
(provide model make-branch make-mobile)
(provide weight is-balanced?)

(define (model n) (leaf n))

(define (make-branch n m) (node (list (leaf n) m)))

(define (make-mobile b1 b2) (node (list b1 b2)))

(define (weight m)
	(if (is-leaf? m)
		(leaf-val m)
		(+ (weight (nth-child (nth-child m 0) 1)) (weight (nth-child (nth-child m 1) 1)))
	)
)

(define (is-balanced? m)
	(if (is-leaf? m)
		#t
		(and
			(and (is-balanced? (nth-child (nth-child m 0) 1)) (is-balanced? (nth-child (nth-child m 1) 1)))
			(equal?
				(* (leaf-val (nth-child (nth-child m 0) 0)) (weight (nth-child (nth-child m 0) 1)))
				(* (leaf-val (nth-child (nth-child m 1) 0)) (weight (nth-child (nth-child m 1) 1)))
			)
		)
	)
)
