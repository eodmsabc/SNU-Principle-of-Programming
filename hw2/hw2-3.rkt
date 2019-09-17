#lang racket

(provide iter)

(define (iter n f)
	(cond
		[(positive? n) (lambda (x) ((iter (- n 1) f) (f x)))]
		[(negative? n) (lambda (x) ((iter (+ n 1) f) (f x)))]
		[else (lambda (x) x)]
	)
)
