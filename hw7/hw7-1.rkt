#lang racket

(require "sum.rkt")
; Do not assume anything about 'sum'.
; Use the provided functions (inl, inr, case-sum) only.

(provide bstree-make bstree-add-elmt bstree-del-elmt bstree-find-elmt)

(define (empty-tree) (mcons '() (mcons '() '())))
(define (is-empty? t) (if (equal? t (empty-tree)) #t #f))
(define (child-num t)
  (cond
    [(and (equal? (mcar (mcdr t)) (empty-tree))
          (equal? (mcdr (mcdr t)) (empty-tree))) "n"]
    [(and (not (equal? (mcar (mcdr t)) (empty-tree)))
          (equal? (mcdr (mcdr t)) (empty-tree))) "l"]
    [(and (equal? (mcar (mcdr t)) (empty-tree))
          (not (equal? (mcdr (mcdr t)) (empty-tree)))) "r"]
    [else "b"]
    )
  )
(define (moverc x)
  (if (equal? (mcdr (mcdr x)) (empty-tree))
      (mcar x)
      (moverc (mcdr (mcdr x)))
      )
  )

(define (bstree-make) (empty-tree))

(define (bstree-add-elmt t k v)
  (cond
    [(is-empty? t)
     (begin
       (set-mcar! t (mcons k v))
       (set-mcdr! t (mcons (empty-tree) (empty-tree)))
       #f
       )
     ]
    [(equal? k (mcar (mcar t)))
     (begin
       (set-mcdr! (mcar t) v)
       #t
       )
     ]
    [(< k (mcar (mcar t))) (bstree-add-elmt (mcar (mcdr t)) k v)]
    [(> k (mcar (mcar t))) (bstree-add-elmt (mcdr (mcdr t)) k v)]
    )
  )

(define (bstree-del-elmt t k)
  (cond
    [(is-empty? t) #f]
    [(equal? k (mcar (mcar t)))
     (begin
       (let ([cn (child-num t)])
         (cond
           [(equal? cn "n")
            (begin
              (set-mcar! t '())
              (set-mcdr! t (mcons '() '()))
              )
            ]
           [(equal? cn "l")
            (begin
              (set-mcar! t (mcar (mcar (mcdr t))))
              (set-mcdr! t (mcdr (mcar (mcdr t))))
              )
            ]
           [(equal? cn "r")
            (begin
              (set-mcar! t (mcar (mcdr (mcdr t))))
              (set-mcdr! t (mcdr (mcdr (mcdr t))))
              )
            ]
           [(equal? cn "b")
            (begin
              (define temp (mcar (mcdr t)))
              (define lrm (moverc temp))
              (set-mcar! t lrm)
              (bstree-del-elmt temp (mcar lrm))
              (set-mcar! (mcdr t) temp)
              )
            ]
           )
         )
       #t
       )
     ]
    [(< k (mcar (mcar t)))
     (begin
       (define lch (mcar (mcdr t)))
       (define res (bstree-del-elmt lch k))
       (set-mcar! (mcdr t) lch)
       res
       )
     ]
    [(> k (mcar (mcar t)))
     (begin
       (define rch (mcdr (mcdr t)))
       (define res (bstree-del-elmt rch k))
       (set-mcdr! (mcdr t) rch)
       res
       )
     ]
    )
  )

(define (bstree-find-elmt t k)
  (cond
    [(equal? t (empty-tree)) (inr '())]
    [(= k (mcar (mcar t))) (inl (mcdr (mcar t)))]
    [(< k (mcar (mcar t))) (bstree-find-elmt (mcar (mcdr t)) k)]
    [(> k (mcar (mcar t))) (bstree-find-elmt (mcdr (mcdr t)) k)]
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define t1 (bstree-make))

;(bstree-add-elmt t1 4 "v1")
;(bstree-add-elmt t1 5 "v3")
;(bstree-add-elmt t1 1 "v2")
;(bstree-add-elmt t1 0 "a")
;(bstree-add-elmt t1 3 "b")
;(bstree-add-elmt t1 2 "xxx")