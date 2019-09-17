#lang racket

(require "hw5-1.rkt")
(require "hw5-2.rkt")

(provide is-empty? fst rest length nth-elmt map reduce)

(define (is-empty? l #|'t list|#)
	(equal? empty #|'t list|# l) #|bool|#
);safe

(define (fst l #|'t list|#)
	(define f1 (lambda x (inr '())#|'t+unit|#)) #|unit->'t+unit|#
	(define f2 (lambda (x y) (inl x)#|'t+unit|#)) #|'t*'t list->'t+unit|#
	(case-list f1 f2 l) #|'t+unit|#
);safe

(define (rest l #|'t list|#)
	(define f1 (lambda x (inr '()) #|'t list+unit|#)) #|unit->'t list+unit|#
	(define f2 (lambda (x y) (inl y) #|'t list+unit|#)) #|'t*'t list->'t list+unit|#
	(case-list f1 f2 l) #|'t list+unit|#
);safe

(define (length l #|'t list|#)
	(define f1 (lambda x 0 #|int|#)) #|unit->int|#
	(define f2 (lambda (x y) (+ 1 (length y) #|int|#))) #|'t*'t list->int|#
	(case-list f1 f2 l) #|int|#
);safe

(define (nth-elmt l #|'t list|# i #|int|#)
	(define f1 (lambda x (inr '())#|'t+unit|#)) #|unit->'t+unit|#
	(define f2 (lambda (x y) (if (zero? i)#|bool|# (inl x)#|'t+unit|# (nth-elmt y (- i 1))#|'t+unit|#))) #|'t*'t list->'t+unit|#
	(case-list f1 f2 l) #|'t+unit|#
);safe

(define (map f #|'t->'s|# l #|'t list|#)
	(define f1 (lambda x empty)) #|unit->'s list|#
	(define f2 (lambda (x y) (link (f x)#|'s|# (map f y)#|'s list|#)#|'s list|#)) #|'t+'t list->'s list|#
	(case-list f1 f2 l) #|'s list|#
);safe

(define (reduce l #|'t list|# f #|'t*'s->'s|# s #|'s|#)
	(define f1 (lambda x s)) #|unit->'s|#
	(define f2 (lambda (x y) (reduce y f (f x s))#|'s|#)) #|'t+'t list->'s|#
	(case-list f1 f2 l) #|'s|#
);safe
