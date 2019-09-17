#lang racket

(provide empty link case-list)

(define empty
	'()
)

(define (link v l)
	(cons v l)
)

(define (case-list f1 f2 l)
	(if (null? l)
		(f1 '())
		(f2 (car l) (cdr l))
	)
)

