#lang racket
; not for execution
; write a type of each expression

#|Only_for_this_function|# #|'a:int_or_float|#
(define (sigma #|'a*'a->{'a->'a}->'a|# lower #|'a|# upper #|'a|#)
  (lambda (f) #|'a->'a|#
    (define (loop n)
      (if (> n upper #|'a|# ) #|bool|# 0 #|'a|#
          (+ (f #|'a->'a|# n ) (loop #|'a->'a|# (+ n 1)) #|'a|# )))
    (loop lower) #|'a|#
    )
  )

(define (generic-sum #|'a*'c*{'a->'b}*{'a*'c->bool}*'d*{'b*'d->'d}*{'a->'a}->'d|#
			lower #|'a|# upper #|'c|# f #|'a->'b|# larger #|'a*'c->bool|# base #|'d|# op #|'b*'d->'d|# inc #|'a->'a|#)
  (if (larger lower #|'a|# upper #|'c|#)#|bool|# base #|'d|#
      (op (f lower #|'a|#)#|'b|#
          (generic-sum (inc lower)#|'a|# upper f larger base op inc)) #|'d|#
      ) #|'d|#
  )

(define (map #|'a_list->'b_list|# f #|'a->'b|# l #|'a_list|# )
  (if (null? l #|'a_list|# ) '() #|'b_list|#
      (cons (f (car l) #|'a|#) #|'b|# (map f (cdr l) #|'a_list|# ) #|'b_list|# ) #|'b_list|#
      )
  )

(define (reduce #|'a_list*{'a*'b->'b}*'b->'b|# l #|'a_list|# op #|'a*'b->'b|# init)
  (if (null? l #|'a_list|#) init #|'b|#
      (op (car l)#|'a|# (reduce (cdr l) op init)#|'b|#) 
      ) #|'b|#
  )

(define (map-reduce #|{'a->'b}*'a_list*{'b*'c->'c}*'c->'c|# f #|'a->'b|# l op #|'b*'c->'c|#  init #|'c|#)
  (reduce (map f #|'a->'b|# l #|'a_list|#)#|'b_list|# op init)
  )
