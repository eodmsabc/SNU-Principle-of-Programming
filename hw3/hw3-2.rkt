#lang racket

(require "hw3-2-library.rkt")

(provide maze-check)

(define (maze-check maze start end)
	(define (dfs rlist end visited)
		(cond ((null? rlist) #f)
			  ((is-member? (car rlist) visited) (dfs (cdr rlist) end (add-element (car rlist) visited)))
			  ((same-room? (car rlist) end) #t)
			  (else (or (dfs (can-enter (car rlist) maze) end (add-element (car rlist) visited))
			  			(dfs (cdr rlist) end (add-element (car rlist) visited))
			  		)
			  )
		)
	)
	(if (same-room? start end) #t
		(dfs (can-enter start maze) end (add-element start empty-set))
	)
)
