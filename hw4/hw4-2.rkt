#lang racket

(provide react S K I v a)

; Implement react. 
;
; In the document, react has the type solution -> void.
; However, implement react: solution -> string for ease of grading.
; Return the string of the given solution printed by pprint.
;
; See hw4-2-grade.rkt for more information on what the returned string should look like.
; In short,
; S prints "S";
; K prints "K";
; I prints "I";
; variable x prints "x";
; tuple (E F) prints "(" + E + " " + F + ")".



(define (react e)
	(pprint
		(reactrec e)
	)
)

(define S 's)
(define K 'k)
(define I 'i)
(define (v str) (cons 'v str))
(define (a lhs rhs) (list 'a lhs rhs))

(define (isS? e) (equal? e S))
(define (isK? e) (equal? e K))
(define (isI? e) (equal? e I))
(define (isv? e) (if (pair? e) (equal? (car e) 'v) #f))
(define (isa? e) (if (pair? e) (equal? (car e) 'a) #f))
(define (var e) (cdr e))
(define (al e) (list-ref e 1))
(define (ar e) (list-ref e 2))

(define (canS e) (if (isa? (al e)) (if (isa? (al (al e))) (isS? (al (al (al e)))) #f) #f))
(define (canK e) (if (isa? (al e)) (isK? (al (al e))) #f))
(define (canI e) (isI? (al e)))
(define (can-eval e)
;	(or (canI e) (or (canK e) (canS e)))
	(if (isa? e) (or (canI e) (or (canK e) (canS e))) #f)
)
(define (sol-eval e)
	(cond
		[(canI e) (ar e)]
		[(canK e) (ar (al e))]
		[(canS e) (a (a (ar (al (al e))) (ar e)) (a (ar (al e)) (ar e)))]
		[else e]
	)
)

(define (reactrec e) #|order: the number of nested paranthesis|#
	(if (isa? e)
		(if (can-eval e)
			(reactrec (sol-eval e)) #|solution transition: always decrease the number of nested paranthesis|#
			(let ([after (a (reactrec (al e)) (reactrec (ar e)))])
				(if (equal? after e) e #|'e' always changes|#
					(reactrec after)
				)
			)
		)
		e
	)
)
#|the number of paranthesis cannot be negative|#
#|recursive function 'reactrec' always decrease the number of paranthesis|#
#|reactrec always stops|#

(define (pprint e)
	(cond
		[(isS? e) "S"]
		[(isK? e) "K"]
		[(isI? e) "I"]
		[(isv? e) (var e)]
		[(isa? e) (string-append "(" (pprint (al e)) " " (pprint (ar e)) ")")]
	)
)
#|pprint always decrease the number of paranthesis -> always stops|#
;(react (a (a I I) (v "a")))
