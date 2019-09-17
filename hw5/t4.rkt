#lang racket

(require racket/match)
(require "common-grade.rkt")
(require "hw5-4.rkt")

(printf "special cases\n")

(define array1 
	(cons 'array (list (list 'B 'B 'B 'B) (list 'B 'B 'B 'B) (list 'W 'W 'B 'B) (list 'W 'W 'B 'B))))

(define tree1
	(array-to-tree (cons 'array (list (list 'B 'B 'B 'B) (list 'B 'B 'B 'B) (list 'W 'W 'B 'B) (list 'W 'W 'B 'B)))))

(define (turn-array pattern i)
  (if (<= i 0) 
      pattern
      (turn-array (rotate-array pattern) (- i 1))))
(define (turn-tree pattern i)
  (if (<= i 0) 
      pattern
      (turn-tree (rotate-tree pattern) (- i 1))))

(define gluebasic1a (glue-array-from-tree black white black black))
(define gluebasic2a (glue-array-from-array black white black black))
(define gluebasic3t (glue-tree-from-array black white black black))
(define gluebasic4t (glue-tree-from-tree black white black black))
(define arr1 (cons 'array (list (list 'B 'W) (list 'B 'B))))
(define tre1 (cons 'tree (list 'B 'W 'B 'B)))


(sgoutput (lambda () (equal? gluebasic1a arr1)))
(sgoutput (lambda () (equal? gluebasic2a arr1)))
(sgoutput (lambda () (equal? gluebasic3t tre1)))
(sgoutput (lambda () (equal? gluebasic4t tre1)))

(printf "basic neighbor checking\n")
(sgoutput (lambda () (equal? 0 (neighbor (list 0) 'B))))
(sgoutput (lambda () (equal? 0 (neighbor-tree (list 0) 'B))))
(sgoutput (lambda () (equal? 0 (neighbor-array (list 0) 'W))))

(printf "basic pprint checking\n")
(sgoutput (lambda () (equal? "B\n" (pprint black))))
(sgoutput (lambda () (equal? "B\n" (pprint-array black))))
(sgoutput (lambda () (equal? "B\n" (pprint-tree black))))

(printf "basic rotate checking\n")
(sgoutput (lambda () (equal? black (rotate black))))
(sgoutput (lambda () (equal? black (rotate-array black))))
(sgoutput (lambda () (equal? black (rotate-tree black))))

(printf "Checking conversion\n")
(sgoutput (lambda () (equal? 'B (array-to-tree black))))
(sgoutput (lambda () (equal? 'B (tree-to-array black))))
(sgoutput (lambda () (equal? 'W (array-to-tree white))))
(sgoutput (lambda () (equal? 'W (tree-to-array white))))
(sgoutput (lambda () (equal? array1 (tree-to-array tree1))))

(define arr2 (cons 'array (list (list 'W 'W) (list 'B 'W))))
(define tre2 (cons 'tree (list 'W 'W 'W 'B)))
(define glue1a (glue-array-from-array arr1 arr2 arr1 arr2))
(define glue2a (glue-array-from-tree  tre1 tre2 tre2 tre1))
(sgoutput (lambda () (equal? 
'(array
  (B W W W)
  (B B B W)
  (W W B W)
  (B W B B)) glue1a)))  

(sgoutput (lambda () (equal?
'(array
  (B W W W)
  (B B B W)
  (B W W W)
  (B B B W)) glue2a)))

(printf "Checking larger conversion and glueing\n")
(define glue1t (glue-tree-from-tree tre1 tre2 tre2 tre1))
(sgoutput (lambda () (and (equal?
(array-to-tree glue2a) glue1t) (equal? glue1t
'(tree (B W B B) (W W W B) (W W W B) (B W B B))))))
(define glue2t (glue-tree-from-array arr1 arr2 arr1 arr2))
(sgoutput (lambda () (and (equal?
(array-to-tree glue1a) glue2t) (equal? glue2t
'(tree (B W B B) (W W W B) (B W B B) (W W W B))))))

(sgoutput (lambda () (equal? glue1t (array-to-tree glue2a))))
(sgoutput (lambda () (equal? glue2t (array-to-tree glue1a))))

(define largera (glue-array-from-array glue1a glue2a glue2a glue1a))
(define largerat '(tree
  ((B W B B) (W W W B) (B W B B) (W W W B))
  ((B W B B) (W W W B) (W W W B) (B W B B))
  ((B W B B) (W W W B) (W W W B) (B W B B))
  ((B W B B) (W W W B) (B W B B) (W W W B))))
(define largert (glue-tree-from-tree glue1t glue2t glue2t glue1t))
(define largerta '(array
  (B W W W B W W W)
  (B B B W B B B W)
  (B W W W W W B W)
  (B B B W B W B B)
  (B W W W B W W W)
  (B B B W B B B W)
  (B W W W W W B W)
  (B B B W B W B B)))

(printf "Checking 64 form\n")
(sgoutput (lambda () (equal? largerat (array-to-tree largera))))
(sgoutput (lambda () (equal? largerta (tree-to-array largert))))
(sgoutput (lambda () (equal? (pprint (rotate largert)) (pprint (rotate largerta)))))
(define rotate-largera '(array
  (B W B B B W B B)
  (W W B W W W B W)
  (B B B W B B B W)
  (B W W W B W W W)
  (B B B B B B B B)
  (B W B W B W B W)
  (B W B W B W B W)
  (W W W W W W W W)))
(define rotate-largert '(tree
  ((B B W B) (B B W B) (B W W W) (B W W W))
  ((B B W B) (B B W B) (B W W W) (B W W W))
  ((B W W W) (B B W B) (B W W W) (B B W B))
  ((B W W W) (B B W B) (B W W W) (B B W B))))

(sgoutput (lambda () (equal? (pprint (rotate largert)) (pprint rotate-largert))))
(sgoutput (lambda () (equal? (pprint (rotate largera)) (pprint rotate-largera))))

(define (rotate-arr f) ; rotate-array: form -> form
  (tree-to-array (rotate-tree (array-to-tree f)))
  )

(define (rotate-tree-ns l)
  (if (symbol? l) l
      (let [(nw (first l)) (ne (second l)) (se (third l)) (sw (fourth l))]
        (list (rotate-tree-ns sw) (rotate-tree-ns nw) (rotate-tree-ns ne) (rotate-tree-ns se))
        )
      ))

(define (rotate-tre f) ; rotate-tree: form -> form
  (if (pair? f)
    (cons 'tree (rotate-tree-ns (cdr f)))
    f)
  )

(define (turn-arr pattern i)
  (if (<= i 0) 
      pattern
      (turn-array (rotate-arr pattern) (- i 1))))
(define (turn-tre pattern i)
  (if (<= i 0) 
      pattern
      (turn-tree (rotate-tre pattern) (- i 1))))


(printf "CHECKING larger rotation\n")
(for ([i '(0 1 2 3 4 5 6 7 8 9)])
	(sgoutput (lambda () (equal? (turn-arr largera i) (turn-array largera i)))))

(for ([i '(0 1 2 3 4 5 6 7 8 9)])
	(sgoutput (lambda () (equal? (turn-tre largert i) (turn-tree largert i)))))

(printf "Checking larger neighbor and pprint\n")
(define list-t
(let ([lis '()])
(for ([i '(0 1 2 3)])
(for ([j '(0 1 2 3)])
(for ([k '(0 1 2 3)])
        (set! lis (append (list (neighbor (list i j k) largert)) lis)))))
  lis))
(define list-a
(let ([lis '()])
(for ([i '(0 1 2 3)])
(for ([j '(0 1 2 3)])
(for ([k '(0 1 2 3)])
        (set! lis (append (list (neighbor (list i j k) largera)) lis)))))
  lis))
(define list-t-ans
'(2 3 7 4 1 2 4 4 1 3 5 4 3 4 7 4 0 3 6 3 2 2 4 4 2 2 3 4 2 4 6 3 1 4 6 3 2 2 4 4 2 2 1 2 2 4 4 2 3 4 7 4 1 3 4 4 1 3 3 2 3 4 4 2))
(define list-a-ans
'(0 3 6 3 2 4 7 4 2 5 6 4 2 4 6 3 3 3 7 5 1 1 2 4 1 1 2 4 3 4 7 5 4 4 7 5 1 1 2 4 1 1 1 2 3 4 4 2 1 4 6 3 2 5 7 4 2 5 3 2 2 4 4 2))
(sgoutput (lambda () (equal? list-a list-a-ans)))
(sgoutput (lambda () (equal? list-t list-t-ans)))

(printf "Checking simple Rotation\n")
(for ([i (list 0 1 2 3 4 5 6 7 8)])
(sgoutput (lambda () (equal? (pprint (turn-array array1 i)) 
			     (pprint (turn-tree tree1 i))))))

(for ([i (list 1 5 9)])
 (for ([j (list 2 6 10)])
	(sgoutput (lambda () (equal? (pprint (turn-tree tree1 (+ i 1)))
				     (pprint (turn-array array1 j)))))))



(printf "1. Internal Representations\n")
; In this homework, we strictly guide the internal structure of tiles.
; This auto-grader should results in "O" (rather than "X") for the
; following test cases.
;
;
(printf "1) Basic Representations\n")
; First of all, black tile should be 'B.
(sgoutput (lambda () (equal? 'B black)))
; Likewise, white tile should be 'W.
(sgoutput (lambda () (equal? 'W white)))
; Note that these two basic tile should be both array and tree:
(sgoutput (lambda () (equal? #t (is-tree? black))))
(sgoutput (lambda () (equal? #t (is-tree? white))))
(sgoutput (lambda () (equal? #t (is-array? black))))
(sgoutput (lambda () (equal? #t (is-array? white))))
;
;
; These are for ease of reading this grader.
(define B black)
(define W white)
;
;
; Tiles are of two kinds: arrays and trees.
; These are constructed by: glue-*-from-*.
;
(printf "2) Array Representations\n")
; Array tile should look like:
(define basic-array (glue-array-from-array B B B W))
(sgoutput (lambda () (equal? '(array (B B) (W B))
                           basic-array)))
(sgoutput (lambda () (equal? '(array (B B) (W B))
                           (glue-array-from-tree B B B W))))
;
(printf "3) Tree Representations\n")
; Tree tile should look like:
(define basic-tree (glue-tree-from-tree B B B W))
(sgoutput (lambda () (equal? '(tree B B B W)
                           basic-tree)))
(sgoutput (lambda () (equal? '(tree B B B W)
                           (glue-tree-from-array B B B W))))
;
(printf "4) Bigger Examples\n")
; Try bigger ones.
(define compound1-array
  (glue-array-from-array
   basic-array
   (turn-array basic-array 1)
   (turn-array basic-array 2)
   (turn-array basic-array 3)))
(define compound2-array
  (rotate-array
   (glue-array-from-array
    basic-array
    basic-array
    (rotate-array basic-array)
    (rotate-array basic-array))))
(sgoutput (lambda () (equal? '(array (B B W B) (W B B B) (B B B W) (B W B B))
                           compound1-array)))
(sgoutput (lambda () (equal? '(array (B W W B) (B B B B) (B W W B) (B B B B))
                           compound2-array)))
(define compound1-tree
  (glue-tree-from-tree
   basic-tree
   (turn-tree basic-tree 1)
   (turn-tree basic-tree 2)
   (turn-tree basic-tree 3)))
(define compound2-tree
  (rotate-tree
   (glue-tree-from-tree
    basic-tree
    basic-tree
    (rotate-tree basic-tree)
    (rotate-tree basic-tree))))
(sgoutput (lambda () (equal? '(tree (B B B W) (W B B B) (B W B B) (B B W B))
                           compound1-tree)))
(sgoutput (lambda () (equal? '(tree (B W B B) (W B B B) (W B B B) (B W B B))
                           compound2-tree)))


(printf "2. Interface Operability\n")

(printf "1) pprint\n")

; pprint (and pprint-*) should results in string as follows.
; Black (white) tile should be represented as "B" ("W").
; Each row are separated by new line character "\n".
; The last line also should contains "\n" after it.
(sgoutput (lambda () (equal? "BBWB\nWBBB\nBBBW\nBWBB\n"
                               (pprint-array compound1-array))))
(sgoutput (lambda () (equal? "BWWB\nBBBB\nBWWB\nBBBB\n"
                               (pprint-array compound2-array))))
(sgoutput (lambda () (equal? "BBWB\nWBBB\nBBBW\nBWBB\n"
                               (pprint-tree compound1-tree))))
(sgoutput (lambda () (equal? "BWWB\nBBBB\nBWWB\nBBBB\n"
                               (pprint-tree compound2-tree))))

(printf "2) neighbor\n")

(sgoutput (lambda () (equal? 2 (neighbor-array (list 0 0) compound1-array))))
(sgoutput (lambda () (equal? 6 (neighbor-array (list 2 0) compound1-array))))
(sgoutput (lambda () (equal? 2 (neighbor-array (list 3 3) compound1-array))))
(sgoutput (lambda () (equal? 3 (neighbor-array (list 0 3) compound2-array))))
(sgoutput (lambda () (equal? 4 (neighbor-array (list 1 3) compound2-array))))
(sgoutput (lambda () (equal? 2 (neighbor-array (list 2 2) compound2-array))))

(sgoutput (lambda () (equal? 2 (neighbor-tree (list 0 0) compound1-tree))))
(sgoutput (lambda () (equal? 6 (neighbor-tree (list 2 0) compound1-tree))))
(sgoutput (lambda () (equal? 2 (neighbor-tree (list 3 3) compound1-tree))))
(sgoutput (lambda () (equal? 3 (neighbor-tree (list 0 3) compound2-tree))))
(sgoutput (lambda () (equal? 4 (neighbor-tree (list 1 3) compound2-tree))))
(sgoutput (lambda () (equal? 2 (neighbor-tree (list 2 2) compound2-tree))))

(printf "3) translation\n")
(sgoutput (lambda () (equal? compound1-tree (array-to-tree compound1-array))))
(sgoutput (lambda () (equal? compound2-tree (array-to-tree compound2-array))))
(sgoutput (lambda () (equal? compound1-array (tree-to-array compound1-tree))))
(sgoutput (lambda () (equal? compound2-array (tree-to-array compound2-tree))))


(printf "3. Casual Examples\n")


(define basic (glue B B B W))
(define (turn pattern i)
  (if (<= i 0) 
      pattern
      (turn (rotate pattern) (- i 1))))
(define compound1
  (glue basic (turn basic 1) (turn basic 2) (turn basic 3)))
(define compound2
  (rotate (glue basic basic (rotate basic) (rotate basic))))

(sgoutput (lambda () (equal? "BBWB\nWBBB\nBBBW\nBWBB\n"
                               (pprint compound1))))
(sgoutput (lambda () (equal? "BWWB\nBBBB\nBWWB\nBBBB\n"
                               (pprint compound2))))

(sgoutput (lambda () (equal? 2 (neighbor (list 0 0) compound1))))
(sgoutput (lambda () (equal? 6 (neighbor (list 2 0) compound1))))
(sgoutput (lambda () (equal? 2 (neighbor (list 3 3) compound1))))
(sgoutput (lambda () (equal? 3 (neighbor (list 0 3) compound2))))
(sgoutput (lambda () (equal? 4 (neighbor (list 1 3) compound2))))
(sgoutput (lambda () (equal? 2 (neighbor (list 2 2) compound2))))
