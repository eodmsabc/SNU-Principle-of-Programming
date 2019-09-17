#lang racket

(provide zipperN)

(define (zipperN l)
	(if (null? l) l
		(match (car l)
			['() (zipperN (cdr l))]
			[(list a) (cons a (zipperN (cdr l)))]
			[(cons a b) (cons a (zipperN (append (cdr l) (list b))))]
		)
	)
)
