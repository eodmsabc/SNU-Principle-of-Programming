#lang racket

(provide crazy2add)

(define (tonum c) (match c ['n -1] ['p 1] ['z 0]))
(define (tosig c) (match c [-1 'n] [1 'p] [0 'z]))

(define (crazy2add lhs rhs)
	(if (null? lhs) rhs
		(if (null? rhs) lhs
			(let ([n (+ (tonum (car lhs)) (tonum (car rhs)))])
				(case n
					[(-1 0 1) (cons (tosig n) (crazy2add (cdr lhs) (cdr rhs)))]
					[(2) (cons 'z (crazy2add '(p) (crazy2add (cdr lhs) (cdr rhs))))] 
					[(-2) (cons 'z (crazy2add '(n) (crazy2add (cdr lhs) (cdr rhs))))]
				)
			)
		)
	)
)
