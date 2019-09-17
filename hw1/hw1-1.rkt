#lang racket

(provide crazy2val)

(define (crazy2val c)
	(if (null? c) 0
		(match (car c)
			['z (* 2 (crazy2val (cdr c)))]
			['p (+ (* 2 (crazy2val (cdr c))) 1)]
			['n (- (* 2 (crazy2val (cdr c))) 1)]
		)
	)
)
