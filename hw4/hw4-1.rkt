#lang racket

(provide vlencode)

(define (leaf str val) (list 'l str val))

(define (node lsub val rsub) (list 'n lsub val rsub))

(define (isleaf? tree) (if (equal? (car tree) 'l) #t #f))

(define (leftsub tree) (list-ref tree 1))

(define (rightsub tree) (list-ref tree 3))

(define (leafval tree) (list-ref tree 2))

(define (leafstr tree) (list-ref tree 1))

(define (makeTreeList lst tlst) #|order: length of lst|#
	(if (null? lst) tlst #|end condition: length = 0|#
		(if (zero? (cdar lst)) (makeTreeList (cdr lst) tlst) #|lst length decreased|#
			(cons
				(leaf (caar lst) (cdar lst))
				(makeTreeList (cdr lst) tlst) #|lst length decreased|#
			)
		)
	)
) #|makeTreeList always stops|#

(define (vlencode frequencies) #|non-recursive|#
	(if (null? frequencies) '() #|after this, frequencies is positive|#
		(let ([initList (makeTreeList frequencies '())])
			(define (merge lst) #|order:length of lst|#
				(if (= 1 (length lst)) (car lst) #|end condition: length =  1|#
					(let ([slst (sort lst #:key leafval <)])
						(let ([t1 (car slst)] [t2 (cadr slst)])
							(merge (cons (node t1 (+ (leafval t1) (leafval t2)) t2) (cddr slst))) #|len-1|# #|decreased|#
						)
					)
				)
			) #|merge always stops|#
			(if (null? initList) '()
				(let ([ftree (merge initList)])
					(define (makeCode tree lst codelst) #|order: level of tree|#
						(if (isleaf? tree) #|end condition|# (cons (cons (leafstr tree) (reverse codelst)) lst)
							(makeCode (rightsub tree)#|decreased|# (makeCode (leftsub tree)#|decreased|# lst (cons 0 codelst)) (cons 1 codelst))
						)
					) #|makeCode always stops|#
					(if (isleaf? ftree)
						(list (cons (leafstr ftree) '(0)))
						(makeCode ftree '() '())
					)
				)
			)
		)
	)
)
;(vlencode '())
;(vlencode (list (cons "a" 4)))
;(vlencode (list (cons "a" 4) (cons "b" 5)))
;(vlencode (list (cons "a" 4) (cons "b" 5) (cons "c" 6) (cons "d" 7) (cons "e" 0)))
;(vlencode (list (cons "a" 4) (cons "b" 10) (cons "c" 4) (cons "d" 3)))
;(vlencode (list (cons "a" 0)))
