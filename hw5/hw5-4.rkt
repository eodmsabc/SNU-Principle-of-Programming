#lang racket

(provide black)
(provide white)

(provide glue-array-from-tree)
(provide glue-array-from-array)
(provide rotate-array)
(provide neighbor-array)
(provide pprint-array)
(provide is-array?)

(provide glue-tree-from-tree)
(provide glue-tree-from-array)
(provide rotate-tree)
(provide neighbor-tree)
(provide pprint-tree)
(provide is-tree?)

(provide array-to-tree)
(provide tree-to-array)

(provide glue)
(provide rotate)
(provide neighbor)
(provide pprint)

(define black 'B)
(define white 'W)

(define (basic? c)
	(cond
		[(equal? c black) #t]
		[(equal? c white) #t]
		[else #f]
	)
)
(define (arv c)
	(cond
		[(basic? c) c]
		[(equal? (car c) 'array) (cdr c)]
	)
)
(define (trv c)
	(cond
		[(basic? c) c]
		[(equal? (car c) 'tree) (cdr c)]
	)
)
(define (tostring c)
	(cond
		[(equal? c black) "B"]
		[(equal? c white) "W"]
	)
)

(define (glue-array-from-tree nw ne se sw)
	(tree-to-array (glue-tree-from-tree nw ne se sw))
)

(define (gafa nw ne se sw)
	(if (basic? nw) (list (list nw ne) (list sw se))
		(if (and (null? sw) (null? se)) '()
			(if (and (null? nw) (null? ne))
				(cons
					(append (car sw) (car se))
					(gafa nw ne (cdr se) (cdr sw))
				)
				(cons
					(append (car nw) (car ne))
					(gafa (cdr nw) (cdr ne) se sw)
				)
			)
		)
	)
)

(define (glue-array-from-array nw ne se sw)
		(cons 'array (gafa (arv nw) (arv ne) (arv se) (arv sw)))
)

(define (glue-tree-from-tree nw ne se sw)
	(cons 'tree (list (trv nw) (trv ne) (trv se) (trv sw)))
)

(define (glue-tree-from-array nw ne se sw) ; glue-tree-from-array: form * form * form * form -> form
	(glue-tree-from-tree (array-to-tree nw) (array-to-tree ne) (array-to-tree se) (array-to-tree sw))
)

(define (rotate-array f) ; rotate-array: form -> form
	(tree-to-array (rotate-tree (array-to-tree f)))
)

(define (neighbor-array loc f) ; neighbor-array: location * form -> int
	(define (avg a b) (/ (+ a b) 2))
	(define (calcloc loc a b c d)
		(if (null? loc) (cons a b)
			(case (car loc)
				[(0) (calcloc (cdr loc) a b (avg a c) (avg b d))]
				[(1) (calcloc (cdr loc) a (avg b d) (avg a c) d)]
				[(2) (calcloc (cdr loc) (avg a c) (avg b d) c d)]
				[(3) (calcloc (cdr loc) (avg a c) b c (avg b d))]
			)
		)
	)
	(define (isBlack f a b)
		(let ([len (length f)])
			(cond
				[(< a 0) 0]
				[(< b 0) 0]
				[(>= a len) 0]
				[(>= b len) 0]
				[else (if (equal? (list-ref (list-ref f a) b) black) 1 0)]
			)
		)
	)
	(define (list-sum lst) (if (null? lst) 0 (+ (car lst) (list-sum (cdr lst)))))
	
	(if (basic? f) 0
		(letrec ([f1 (arv f)] [len (length f1)] [pos (calcloc loc 0 0 len len)])
			(let ([a (car pos)] [b (cdr pos)])
				(list-sum (list
					(isBlack f1 (- a 1) (- b 1))
					(isBlack f1 (- a 1) b)
					(isBlack f1 (- a 1) (+ b 1))
					(isBlack f1 a (- b 1))
					(isBlack f1 a (+ b 1))
					(isBlack f1 (+ a 1) (- b 1))
					(isBlack f1 (+ a 1) b)
					(isBlack f1 (+ a 1) (+ b 1))
				))
			)
		)
	)
)

(define (insert-newline strlist)
	(if (null? strlist) '()
		(cons (car strlist) (cons "\n" (insert-newline (cdr strlist))))
	)
)

(define (pprint-array f)
	(define (atos l)
		(cond
			[(null? l) '()]
			[else (string-append* (map tostring l))]
		)
	)
	(cond
		[(equal? f black) "B\n"]
		[(equal? f white) "W\n"]
		[else (string-append* (insert-newline (map atos (arv f))))]
	)
)

(define (is-array? f)
	(cond
		[(equal? 'B f) #t]
		[(equal? 'W f) #t]
		[(equal? 'array (car f)) #t]
		[else #f]
	)
)

(define (rotate-tree f)
	(define (rt f)
		(if (basic? f) f
			(match f
				[(list a b c d) (list (rt d) (rt a) (rt b) (rt c))]
			)
		)
	)
	(if (basic? f) f
		(cons 'tree (rt (trv f)))
	)
)

(define (neighbor-tree loc f) ; neighbor-tree: location * form -> int
	(neighbor-array loc (tree-to-array f))
)

(define (pprint-tree f)
	(pprint-array (tree-to-array f))
)

(define (is-tree? f) ; is-tree?: form -> bool
	(cond
		[(equal? 'B f) #t]
		[(equal? 'W f) #t]
		[(equal? 'tree (car f)) #t]
		[else #f]
	)
)

; conversion
(define (array-to-tree f) ; array-to-tree: form -> form
	(define (att f)
		(define (cutoff p l)
			(if (equal? p take)
				(lambda (x) (take x l))
				(lambda (x) (drop x l))
			)
		)
		(if (basic? f) f
			(if (equal? (length f) 2)
				(match f
					[(list (list a b) (list c d)) (list a b d c)]
				)
				(let ([plen (/ (length f) 2)])
					(let ([f1 (take (map (cutoff take plen) f) plen)]
						  [f2 (take (map (cutoff drop plen) f) plen)]
						  [f3 (drop (map (cutoff drop plen) f) plen)]
						  [f4 (drop (map (cutoff take plen) f) plen)])
						(list (att f1) (att f2) (att f3) (att f4))
					)
				)
			)
		)
	)
	(if (basic? f) f
		(cons 'tree (att (arv f)))
	)
)
(define (tree-to-array f) ; tree-to-array: form -> form
	(define (tta f)
		(if (basic? f) f
			(match f
				[(list a b c d) (gafa (tta a) (tta b) (tta c) (tta d))]
				[else f]
			)
		)
	)
	(if (basic? f) f
		(cons 'array (tta (trv f)))
	)
)


; interfaces
(define (ctt f)
	(cond
		[(is-tree? f) f]
		[else (array-to-tree f)]
	)
)
(define (glue nw ne se sw) ; glue: form * form * form * form -> form
	(glue-tree-from-tree (ctt nw) (ctt ne) (ctt se) (ctt sw))
)

(define (rotate f) ; rotate: form -> form
	(if (is-array? f)
		(rotate-array f)
		(rotate-tree f)
	)
)

(define (neighbor loc f) ; neighbor: location * form -> int
	(if (is-array? f)
		(neighbor-array loc f)
		(neighbor-tree loc f)
	)
)

(define (pprint f) ; pprint: form -> string
	(if (is-array? f)
		(pprint-array f)
		(pprint-tree f)
	)
)

