#lang racket

(provide inl inr case-sum)

(define (inl v)
	(cons 'l v)
)

(define (inr v)
	(cons 'r v)
)

(define (case-sum f1 f2 s)
	(if (equal? 'l (car s))
		(f1 (cdr s))
		(f2 (cdr s))
	)
)
