#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require automata/machine
           chk
           logic/match
           logic/qea)

  (let ([mach
         (qea
          (∀ i)
          (start start)
          (field curr-bid 0)
          (field min-bid 0)
          [-> start (list 'put-up-item i r) item-listed
              (set curr-bid 0)
              (set min-bid r)]
          [-> item-listed (list 'bid i a) item-listed
              (when (> a curr-bid))
              (set curr-bid a)]
          [-> item-listed (list 'sell i) sold
              (when (> curr-bid min-bid))])])
    (chk
     #:t (machine-accepts? mach '((put-up-item a 10) (put-up-item b 50) (bid a 100) (sell a)))
     #:! (machine-accepts? mach '((put-up-item a 10) (put-up-item b 50) (bid a 5) (sell a)))
     )))
