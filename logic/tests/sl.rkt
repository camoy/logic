#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support

(require racket/bool
         racket/list
         racket/set
         racket/stream)

(define (join sot1 col1 sot2 col2)
  (define pairs (cartesian-product (set->list sot1) (set->list sot2)))
  (for/set ([pair (in-stream pairs)]
            #:when (equal? (list-ref (first pair) col1)
                           (list-ref (second pair) col2)))
    (remove-duplicates
     (append (second pair)
             (remove (list-ref (first pair) col1)
                     (first pair))))))

(define (project sot col)
  (for/set ([tuple (in-stream sot)])
    (list-ref tuple col)))

(define-syntax-rule (∨ x ...) (or x ...))
(define == equal?)
(define ∪ set-union)
(define-syntax-rule (⇒ x y) (implies x y))
(define ∅ (set))
(define ¬ not)
(define ⊆ subset?)
(define π project)
(define ⋈ join)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require automata/machine
           chk
           racket/list
           logic/sl
           syntax/macro-testing)

  (struct fake-map (name) #:transparent)
  (struct fake-collection (name) #:transparent)
  (struct fake-iterator (name) #:transparent)

  (define m (fake-map 'm))
  (define c1 (fake-collection 'c1))
  (define c2 (fake-collection 'c2))
  (define t1 (fake-iterator 't1))
  (define t2 (fake-iterator 't2))
  (define tr
    (list `(create ,m ,c1)
          `(create ,m ,c2)
          `(iterator ,c1 ,t1)
          `(update ,m)
          `(iterator ,c2 ,t2)
          `(next ,t1)))

  (let ([unsafe-map-iterator
         (sl
          (create iterator next update)
          [ok       (∨ (== updated ∅)
                       (⇒ next (¬ (⊆ (π next 0) (π updated 2)))))]
          [createdC (∪ (create 0 ∅) (createdC -1 ∅))]
          [createdI (∪ (⋈ (iterator 0 ∅) 0 createdC 1) (createdI -1 ∅))]
          [updated  (∪ (⋈ (update 0 ∅) 0 createdI 0) (updated -1 ∅))])])
    (chk
     #:x
     (convert-syntax-error
      (sl (foo bar)
          [x y]
          [y x]))
     "cyclic dependency"
     #:! (machine-accepts? unsafe-map-iterator tr)
     #:t (machine-accepts? unsafe-map-iterator (drop-right tr 1))
     )))
