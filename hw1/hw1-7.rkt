#lang racket
(require "hw1-6.rkt")
(provide output)

(define (output c)
	(cond
		[(is-zero? c) 0]
		[(is-one? c) 1]
		[(is-not? c) (- 1 (output (sub-circuit c 0)))]
		[(is-and? c) (* (output (sub-circuit c 0)) (output (sub-circuit c 1)))]
		[(is-or? c) (if (zero? (+ (output (sub-circuit c 0)) (output (sub-circuit c 0)))) 0 1)]
	)
)
