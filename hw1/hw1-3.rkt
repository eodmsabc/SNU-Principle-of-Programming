#lang racket

(require "hw1-2.rkt")
(provide crazy2mul)

(define (mval c) (match c ['n 'p] ['p 'n] ['z 'z]))
(define (mcrazy c) (if (null? c) '() (cons (mval (car c)) (mcrazy (cdr c)))))

(define (crazy2mul lhs rhs)
	(if (null? lhs) '()
		(let ([next (crazy2mul (cdr lhs) (cons 'z rhs))])
			(match (car lhs)
				['z next]
				['p (crazy2add rhs next)]
				['n (crazy2add (mcrazy rhs) next)]
			)
		)
	)
)
