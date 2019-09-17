#lang racket

(provide myeval)

(define (myeval expr)
  (define (auxev expr env)
    
    ; HELPER
    (define (dupcheck l)
      (match l
        ['() #t]
        [(cons (list x e) lst2)
         (if (equal? (assoc x lst2) #f)
             (dupcheck lst2)
             x
             )
         ]
        )
      )
    
    (define (larg-dupcheck l)
      (match l
        ['() #t]
        [(cons x lst2)
         (if (equal? (member x lst2) #f)
             (larg-dupcheck lst2)
             x
             )
         ]
        )
      )
    
    (define (let-addenv lst ev)
      (define (temp lst ev)
        (match lst
          ['() '()]
          [(cons (list x e) lst2)
           (cons (cons x (auxev e ev)) (temp lst2 ev))
           ]
          )
        )
      (append (temp lst ev) ev)
      ;(match lst
      ;  ['() ev]
      ;  [(cons (list x e) lst2)
      ;   (let-addenv lst2 (cons (cons x (auxev e ev)) ev))
      ;   ]
      ;  )
      )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (letrec-addenv lst ev)
      (match lst
        ['() ev]
        [(cons (list x e) lst2)
         (let ((rr (auxev e ev)))
           (match rr
             [(cons (list 'lambda arg fexp) fenv)
              (letrec-addenv lst2 (cons (cons x (cons (list 'lambda arg (list 'letrec lst fexp)) fenv)) ev))
              ]
             [else (letrec-addenv lst2 (cons (cons x rr) ev))]
             )
           )
         ;(letrec-addenv lst2 (cons (cons x e) ev))
         ]
        )
      )
    
    (define (list-merge l1 l2)
      (match l1
        ['() '()]
        [(cons e sl) (cons (list e (car l2)) (list-merge sl (cdr l2)))]
        )
      )
    
    (define (fcall-env args ev fev)
      (match args
        ['() fev]
        [(cons (list x e) args2) (fcall-env args2 ev (cons (cons x (auxev e ev)) fev))]
        )
      )
    
    ; MAIN EVALUATION
    (cond
      [(number? expr) expr]
      [(boolean? expr) expr]
      [else
       (match expr
         [(list 'if e e1 e2) (if (auxev e env) (auxev e1 env) (auxev e2 env))]
         
         ; PAIR
         [(list 'cons e1 e2) (cons (auxev e1 env) (auxev e2 env))]
         [(list 'car e)
          (let ([rr (auxev e env)])
            (match rr
              [(cons c1 _) c1]
              [x (error "car: Not a Pair")]
              )
            )
          ]
         [(list 'cdr e)
          (let ([rr (auxev e env)])
            (match rr
              [(cons _ c) c]
              [x (error "car: Not a Pair")]
              )
            )
          ]
         
         ; LAMBDA
         [(list 'lambda x e)
          (let ((cc (larg-dupcheck x)))
            (if (equal? cc #t)
                (cons (list 'lambda x e) env)
                (error 'lambda "duplicate argument name ~a" cc)
                )
            )
          ]
         
         ; LET
         [(list 'let x e)
          (let ((cc (dupcheck x)))
            (if (equal? cc #t)
                (auxev e (let-addenv x env))
                (error 'let "duplicate identifier ~a" cc)
                )
            )
          ]
         
         ; LETREC
         [(list 'letrec x e)
          (let ((cc (dupcheck x)))
            (if (equal? cc #t)
                (auxev e (letrec-addenv x env))
                (error 'letrec "duplicate identifier ~a" cc)
                )
            )
          ]
         
         ; CALCULATION
         [(list '+ e1 e2)
          (let ([n1 (auxev e1 env)] [n2 (auxev e2 env)])
            (if (and (number? n1) (number? n2)) (+ n1 n2)
                (error '+ "Cannot Evaluate - Not a Number")
                )
            )
          ]
         [(list '- e1 e2)
          (let ([n1 (auxev e1 env)] [n2 (auxev e2 env)])
            (if (and (number? n1) (number? n2)) (- n1 n2)
                (error '- "Cannot Evaluate - Not a Number")
                )
            )
          ]
         [(list '* e1 e2)
          (let ([n1 (auxev e1 env)] [n2 (auxev e2 env)])
            (if (and (number? n1) (number? n2)) (* n1 n2)
                (error '* "Cannot Evaluate - Not a Number")
                )
            )
          ]
         
         ; COMPARISON
         [(list '= e1 e2)
          (let ([n1 (auxev e1 env)] [n2 (auxev e2 env)])
            (if (and (number? n1) (number? n2))
                (= n1 n2)
                (error '= "Cannot Evaluate - Not a Number")
                )
            )
          ]
         [(list '< e1 e2)
          (let ([n1 (auxev e1 env)] [n2 (auxev e2 env)])
            (if (and (number? n1) (number? n2))
                (< n1 n2)
                (error '< "Cannot Evaluate - Not a Number")
                )
            )
          ]
         [(list '> e1 e2)
          (let ([n1 (auxev e1 env)] [n2 (auxev e2 env)])
            (if (and (number? n1) (number? n2))
                (> n1 n2)
                (error '> "Cannot Evaluate - Not a Number")
                )
            )]
         
         ; Empty List, Variable and Application
         [x
          (if (equal? x ''()) '(); Empty List
              (cond
                ; Application
                [(list? x)
                 (let ((len (length x)))
                   (if (positive? len)
                       (match (auxev (car x) env)
                         [(cons (list 'lambda arg fex) fev)
                          (if (= (- len 1) (length arg))
                              (auxev fex (fcall-env (list-merge arg (cdr x)) env fev))
                              (error 'Application "Arity Mismatch\nexpected: ~a\ngiven ~a" (length arg) (- len 1))
                              )
                          ]
                         [else (error 'Application "Not a Function")]
                         )
                       (error 'Application "Cannot Find Function")
                       )
                   )
                 ]
                
                ; Variable
                [else; Variable
                 (let ([fnd (assoc x env)])
                   (if (equal? fnd #f) (error 'Unbound "~a" x)
                       (cdr fnd)
                       )
                   )
                 ]
                )
              )
          ]
         )]
      )
    ) 
  (define (print_exp expr)
    expr
    )
  (print_exp (auxev expr '()))
  )