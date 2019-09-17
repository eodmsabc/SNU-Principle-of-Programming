#lang racket

;;; If these statements are omitted, your submission will be graded 0.
(provide memo-ways)

(define (memo-ways n m) ; memo-ways: int * int -> int
	(define (mem-aux n m mem)
		(cond
			[(= n 0) (cons 1 mem)]
			[(= m 0) (cons 1 mem)]
			[else (let ([fnd (assoc (cons n m) mem)])
				(if (equal? fnd #f)
					(letrec ([newv1 (mem-aux (- n 1) m mem)] [newv2 (mem-aux n (- m 1) (cdr newv1))])
						(let ([val (+ (car newv1) (car newv2))])
							(cons val (cons (cons (cons n m) val) (cdr newv2)))
						)
					)
					(cons (cdr fnd) mem)
				)
			)]
		)
	)
	(car (mem-aux n m '()))
)

