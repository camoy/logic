#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           automata/machine
           logic/dfa
           logic/match)

  (define M
    (dfa s1 (s1)
         [s1 (equal? 0) s2]
         [s1 (? even?) s1]
         [s2 (equal? 0) s1]
         [s2 (? even?) s2]))

  (chk
   #:t (machine-accepts? M (list 2 0 4 0 2))
   #:! (machine-accepts? M (list 0 4 0 2 0))
   #:t (machine-accepts? M (list 2 0 2 2 0 8))
   #:t (machine-accepts? M (list 0 2 0 0 10 0))
   #:t (machine-accepts? M (list))
   #:! (machine-accepts? M (list 4 0))
   ))
