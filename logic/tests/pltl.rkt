#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require automata/machine
           chk
           racket/list
           logic/pltl
           syntax/macro-testing)

  (struct next (i) #:transparent)
  (struct update (m) #:transparent)
  (struct iterator (c i) #:transparent)
  (struct create (m c) #:transparent)

  (struct fake-map (name) #:transparent)
  (struct fake-collection (name) #:transparent)
  (struct fake-iterator (name) #:transparent)

  (define-pltl ϕ
    (∃ (m c t)
       (∧ (next t)
          (♦︎ (S (update m)
                (∧ (iterator c t)
                   (♦︎ (create m c))))))))

  (define m (fake-map 'm))
  (define c1 (fake-collection 'c1))
  (define c2 (fake-collection 'c2))
  (define t1 (fake-iterator 't1))
  (define t2 (fake-iterator 't2))
  (define tr
    (list (create m c1)
          (create m c2)
          (iterator c1 t1)
          (update m)
          (iterator c2 t2)
          (next t1)))

  (struct pop () #:transparent)
  (struct has () #:transparent)

  (define-pltl ψ
    (∧ (pop)
       (¬ (● (S (¬ (pop)) (has))))))

  (chk
   #:x
   (convert-syntax-error
    (let ()
      (define-pltl f1
        (∃ (i c) (∨ (¬ (next i)) (iterator c i))))
      (void)))
   "not monitorable"

   #:x
   (convert-syntax-error
    (let ()
      (define-pltl f2
        (∃ (i c) (∨ (next i) (iterator c i))))
      (void)))
   "not monitorable"

   #:t (machine-accepts? (pltl ϕ) tr)
   #:! (machine-accepts? (pltl ϕ) (drop-right tr 1))

   #:! (machine-accepts? (pltl ψ) (list (has)))
   #:! (machine-accepts? (pltl ψ) (list (has) (pop)))
   #:! (machine-accepts? (pltl ψ) (list (has) (pop) (has)))
   #:! (machine-accepts? (pltl ψ) (list (has) (pop) (has) (has) (pop)))
   #:t (machine-accepts? (pltl ψ) (list (pop)))
   #:t (machine-accepts? (pltl ψ) (list (has) (pop) (pop)))
   #:t (machine-accepts? (pltl ψ) (list (has) (pop) (has) (has) (pop) (pop)))
   ))
