#lang racket

(require "hw3-3-library.rkt")

(provide mazeGen)

(define (nearCells pos)
	(let ([x (car pos)] [y (cdr pos)])
		(append (list (cons (- x 1) y) (cons (+ x 1) y) (cons x (+ y 1)) (cons x (- y 1)))
			(if (even? y)
				(list (cons (+ x 1) (+ y 1)) (cons (+ x 1) (- y 1)))
				(list (cons (- x 1) (+ y 1)) (cons (- x 1) (- y 1)))
			)
		)
	)
)

(define (isValidCell n m position visited)
	(let ([x (car position)] [y (cdr position)])
		(cond
			[(or (negative? y) (negative? x)) #f]
			[(or (>= x n) (>= y m)) #f]
			[(not (equal? #f (member (cons x y) visited))) #f]
			[else #t]
		)
	)
)

(define (mix lst)
	(if (null? lst) '()
		(let ([l (length lst)])
			(let ([kth (list-ref lst (random l))])
				(cons kth (mix (remove kth lst)))
			)
		)
	)
)

(define (nextRooms n m pos visited)
	(if (= (cdr pos) (- m 1)) '()
		(mix (nearCells pos))
	)
)

(define (openBetween pos1 pos2 maze)
	(let ([x1 (car pos1)] [y1 (cdr pos1)] [x2 (car pos2)] [y2 (cdr pos2)])
		(cond
			[(and (= y1 y2) (> x1 x2)) (open-n x1 y1 maze)]
			[(and (= y1 y2) (< x1 x2)) (open-s x1 y1 maze)]
			[(and (> y1 y2) (> x1 x2)) (open-nw x1 y1 maze)]
			[(and (> y1 y2) (< x1 x2)) (open-sw x1 y1 maze)]
			[(and (< y1 y2) (> x1 x2)) (open-ne x1 y1 maze)]
			[(and (< y1 y2) (< x1 x2)) (open-se x1 y1 maze)]
			[else
				(if (even? y1)
					(if (> y1 y2) (open-nw x1 y1 maze) (open-ne x1 y1 maze))
					(if (> y1 y2) (open-sw x1 y1 maze) (open-se x1 y1 maze))
				)
			]
		)
	)
)

(define (mazeGen n m)
	(let ([maze (init-maze n m)] [start (cons (random n) (random m))])
		(define (recGen now maze visited)
			(define (dfs lst maze visited)
				(if (null? lst) (cons maze visited)
					(if (isValidCell n m (car lst) visited)
						(let ([newset (recGen (car lst) maze visited)])
							(dfs (cdr lst) (openBetween now (car lst) (car newset)) (cdr newset))
						)
						(dfs (cdr lst) maze visited)
					)
				)
			)
			(let ([nextlist (nextRooms n m now visited)])
				(dfs nextlist maze (cons now visited))
			)
		)
		(let ([complexMaze (car (recGen start maze '()))])
			(open-n 0 (random m) (open-s (- n 1) (random m) complexMaze))
		)
	)
)

(maze-pp (mazeGen 3 3))
