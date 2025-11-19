#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require automata/machine
           logic/nfa
           logic/match
           chk)

  (define M0
    (nfa (s1 s3)
         (s1 s3)
         [s1 (equal? 0) s2]
         [s1 (equal? 1) s1]
         [s2 (equal? 0) s1]
         [s2 (equal? 1) s2]
         [s3 (equal? 0) s3]
         [s3 (equal? 1) s4]
         [s4 (equal? 0) s4]
         [s4 (equal? 1) s3]))

  (define M1
    (nfa (s1)
         (s3)
         [s1 (equal? 0) {s2 s3}]
         [s1 (equal? 1) s1]
         [s2 (equal? 0) s2]
         [s3 _ {}]))

  (chk
   #:t (machine-accepts? M0 (list 1 0 1 0 1))
   #:t (machine-accepts? M0 (list 0 1 0 1 0))
   #:t (machine-accepts? M0 (list 1 0 1 1 0 1))
   #:t (machine-accepts? M0 (list 0 1 0 0 1 0))
   #:t (machine-accepts? M0 (list))
   #:! (machine-accepts? M0 (list 1 0))

   #:t (machine-accepts? M1 '(0))
   #:t (machine-accepts? M1 '(1 1 1 1 0))
   #:! #:t (machine-accepts? M1 '(1 1 1 1))
   #:! #:t (machine-accepts? M1 '(1 1 1 1 0 0))
   ))
