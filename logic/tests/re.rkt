#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require automata/machine
           logic/re
           logic/match
           chk)

  (chk
   #:! (machine-accepts? (re (plus 'a)) '())
   #:t (machine-accepts? (re (plus 'a)) '(a a))
   ))
