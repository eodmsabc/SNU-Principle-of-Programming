#lang racket

(provide equal)
(provide size)
(provide beautiful)

(require "hw5-4.rkt")

(define (basic? f)
	(cond
		[(equal? f black) #t]
		[(equal? f white) #t]
		[else #f]
	)
)
(define (cta f) (if (is-array? f) f (tree-to-array f)))
(define (ctt f) (if (is-tree? f) f (array-to-tree f)))

; interfaces
(define (equal f g) ; equal: form * form -> form
	(equal? (cta f) (cta g))
)

(define (size f) ; size: form -> int
	(define (recsize f) (if (basic? f) 0 (+ (recsize (car f)) 1)))
	(if (basic? f) 0 (recsize (cdr (ctt f))))
)

(define (beautiful f) ; beautiful: form -> bool
	(define (beautiful-sym f)
		(equal f (rotate (rotate f)))
	)
	(define (beautiful-neighbor f)
		(define sz (size f))
		(define (startloc s) (if (zero? s) '() (cons 0 (startloc (- s 1)))))
		(define (endloc s) (if (zero? s) '(1) (cons 0 (endloc (- s 1)))))
		(define (incloc loc)
			(if (zero? (length loc)) '(1)
				(if (equal? (car loc) 3)
					(cons 0 (incloc (cdr loc)))
					(cons (+ (car loc) 1) (cdr loc))
				)
			)
		)
		(define (isok loc f) (let ([n (neighbor loc f)]) (if (and (> n 1) (< n 6)) #t #f)))
		(define (recnb loc e f)
			(if (equal? loc e) #t
				(and
					(isok loc f)
					(recnb (incloc loc) e f)
				)
			)
		)
		(if (basic? f)
			#t
			(recnb (startloc sz) (endloc sz) f)
		)
	)
	(or (beautiful-sym f) (beautiful-neighbor f))
)

