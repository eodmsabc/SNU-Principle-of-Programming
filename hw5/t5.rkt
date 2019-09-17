#lang racket

(require racket/match)
(require "common-grade.rkt")
(require "hw5-4.rkt")
(require "hw5-5.rkt")

(define B black)
(define W white)
(define basic (glue B B B W))
(define (turn pattern i)
  (if (<= i 0) 
      pattern
      (turn (rotate pattern) (- i 1))))
(define compound1
  (glue basic (turn basic 1) (turn basic 2) (turn basic 3)))
(define compound2
  (rotate (glue basic basic (rotate basic) (rotate basic))))
(define compound3
  (glue compound1 compound2 (turn compound1 2) (turn compound2 2)))


;;; beautiful test

(sgoutput (lambda () (equal? 0 (size B))))
(sgoutput (lambda () (equal? 0 (size W))))
(sgoutput (lambda () (equal? 1 (size basic))))
(sgoutput (lambda () (equal? 2 (size compound1))))
(sgoutput (lambda () (equal? 2 (size compound2))))
(sgoutput (lambda () (equal? 3 (size compound3))))
(sgoutput (lambda () (equal? #t (beautiful compound1))))
(sgoutput (lambda () (equal? #f (beautiful compound2))))
(sgoutput (lambda () (equal? #t (beautiful compound3))))


(define (make-list num)
  (if (zero? num) '()
  (append (list (random 2)) (make-list (- num 1)))))

(define (make-sym lis i)
  (if (equal? i 0) '()
      (if (zero? (car lis)) (append (list 'B) (make-sym (cdr lis) (- i 1)))
          (append (list 'W) (make-sym (cdr lis) (- i 1))))))

(define (make-arr num i)
  (if (equal? i num) '()
  (let ([lst (make-list num)])
    (append (list (make-sym lst num)) (make-arr num (+ i 1))))))

;(cons 'array (make-arr 8 0))

(printf "testing basic\n")
(sgoutput (lambda () (not (equal black white))))
(sgoutput (lambda () (not (equal white black))))
(sgoutput (lambda () (equal black black)))
(sgoutput (lambda () (equal white white)))
(sgoutput (lambda () (equal? #t (beautiful black))))
(sgoutput (lambda () (equal? #t (beautiful white))))

(define (gen-all i)
  (define (app-one n l)
    (if (null? l) '()
        (cons (cons n (car l)) (app-one n (cdr l)))
        ))
  (if (= i 0) '(())
      (let ([pall (gen-all (- i 1))])
        (append (app-one 0 pall) (app-one 1 pall) (app-one 2 pall) (app-one 3 pall))
        )
      ))

(define (test-all test l)
  (if (null? l) #t
      (if (test (car l)) (test-all test (cdr l))
          #f)))

(define (get-nth-tree t n)
  (if (symbol? t) t
      (let ([rt (cdr t)])
        (let ([nth (list-ref rt n)])
          (if (symbol? nth) nth
              (cons 'tree nth))
          ))))

(define (beautifuls f) ; beautiful: form -> bool
  (define (beautiful-sym f)
    (if (symbol? f) #t
      (let ([ft (if (is-tree? f) f (array-to-tree f))])
        (and (equal (get-nth-tree ft 0)  (rotate-tree (rotate-tree (get-nth-tree ft 2))))
             (equal (get-nth-tree ft 1)  (rotate-tree (rotate-tree (get-nth-tree ft 3))))))
      )
     )
    
  (define (beautiful-neighbor f)
    (let ([sz (size f)])
      (test-all (lambda (x) (let ([nbr (neighbor x f)]) (and (< 1 nbr) (> 6 nbr)))) (gen-all sz))
      )
    )
  (or (beautiful-sym f) (beautiful-neighbor f)))


(printf "testing size 4\n")
(define test4 
(let ([flag 1])
(for ([i (build-list 100 values)])
	(let ([arr (cons 'array (make-arr 2 0))])
		(if (and (equal? (size arr) 1)
			 (equal? (beautifuls arr) (beautiful arr))) (set! flag (* flag 1)) (set! flag 0))))

(equal? flag 1)))

(sgoutput (lambda () (equal? test4 #t)))


(printf "testing size 16\n")
(define test16 
(let ([flag 1])
(for ([i (build-list 3000 values)])
	(let ([arr (cons 'array (make-arr 4 0))])
		(if (and (equal? (size arr) 2)
			 (equal? (beautifuls arr) (beautiful arr))) (set! flag (* flag 1)) (set! flag 0))))

(equal? flag 1)))

(sgoutput (lambda () (equal? test16 #t)))


(printf "testing size 64\n")
(define test64 
(let ([flag 1])
(for ([i (build-list 5000 values)])
	(let ([arr (cons 'array (make-arr 8 0))])
		(if (and (equal? (size arr) 3)
			 (equal? (beautifuls arr) (beautiful arr))) (set! flag (* flag 1)) (set! flag 0))))

(equal? flag 1)))

(sgoutput (lambda () (equal? test64 #t)))


(printf "testing size 256\n")
(define test256
(let ([flag 1])
(for ([i (build-list 2000 values)])
	(let ([arr (cons 'array (make-arr 16 0))])
		(if (and (equal? (size arr) 4)
			 (equal? (beautifuls arr) (beautiful arr))) (set! flag (* flag 1)) (set! flag 0))))

(equal? flag 1)))

(sgoutput (lambda () (equal? test256 #t)))


