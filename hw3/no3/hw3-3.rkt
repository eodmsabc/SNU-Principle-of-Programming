#lang racket

(require "hw3-3-library.rkt")

(provide mazeGen)

(define (mazeGen n m)
	(let ([maze (init-maze n m)])
		(define (open x y maze)
			(cond
				[(zero? y) (open-s x y maze)]
				[(even? y) (open-nw x y maze)]
				[else (open-sw x y maze)]
			)
		)
		(define (recopen x y maze)
			(cond
				[(and (zero? x) (zero? y)) (open x y maze)]
				[(and (> x 0) (zero? y)) (open x y (recopen (- x 1) (- m 1) maze))]
				[else (open x y (recopen x (- y 1) maze))]
			)
		)
		(open-n 0 0 (recopen (- n 1) (- m 1) maze))
	)
)

(maze-pp (mazeGen 10 10))
