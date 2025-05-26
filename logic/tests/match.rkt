#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           logic/match)

  (struct blah ())
  (struct foo (bar baz))

  (chk
   ;; core
   (match 1 [_ 2])  2
   (match 2 [x x])  2
   (match (foo 1 2)
     [(blah) 'bad]
     [(foo x y) (list x y)])
   '(1 2)

   (match (blah)
     [(blah) 'good]
     [(foo x y) (list x y)])
   'good

   (match 2
     [(and2 (? odd?) (? even?)) 'bad]
     [(and2 (? even?) (? odd?)) 'bad]
     [(and2 (? even?) (? even?)) 'good])
   'good

   (match 2
     [(? odd?) 'bad]
     [(? even?) 'good])
   'good

   (match 2 [(app add1 x) x])  3
   (match 2
     [(not (? even?)) 'bad]
     [(not (? odd?)) 'good])
   'good

   ;; and
   (match 10
     [(and (? even?) (? positive?) (? integer?) x)
      (* 2 x)])
   20

   ;; equal?
   (match 1
     [(equal? 2) 'bad]
     [(equal? 1) 'good])
   'good

   ;; cons
   (match '(1 2) [(cons a _) a])  1
   (cons 1 '(2))  '(1 2)
   (match '(1 2)
     [(cons a b) (cons a b)])
   '(1 2)

   ;; list
   (match '(1 2)
     [(list a b) (list b a)])
   '(2 1)

   (match '(1 2 3)
     [(list _ _ a) a])
   3

   (match 2
     [(and2 x (app (lambda (y) x) z)) (list x z)])
   '(2 2)

   ;; quote
   (match 1
     ['2 'bad]
     ['1 'good])
   'good

   ;; quasiquote
   (match '(1 a 2)
     [`(,a a ,b) (list a b)])
   '(1 2)
   ))
