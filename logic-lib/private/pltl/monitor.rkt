#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out pltl-history)
         pltl-environments)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/match
         racket/set
         (submod "relation.rkt" unsafe)
         "formula.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

;; Wraps `cache` to prevent tampering.
(struct pltl-history (fml cache))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; monitor

(define (pltl-environments ϕ evt [prior #f])
  (define pcache (and prior (pltl-history-cache prior)))
  (define cache (make-hash))
  (define rel
    (let go ([ϕ ϕ])
      (match ϕ
        [(pat-fml vars proj _)
         (define r (relation vars))
         (define v (proj evt))
         (if v (relation-add r v) r)]
        [(disj-fml l r) (relation-union (go l) (go r))]
        [(or (conj-fml l (neg-fml r)) (conj-fml (neg-fml r) l))
         (relation-antijoin (go l) (go r))]
        [(conj-fml l r) (relation-natural-join (go l) (go r))]
        [(exists-fml xs e)
         (define rel (go e))
         (define ks* (set-subtract (relation-keys rel) xs))
         (relation-project rel ks*)]
        [(previous-fml e)
         (begin0
           (if (not pcache)
               (relation (set))
               (hash-ref pcache e))
           (hash-set! cache e (go e)))]
        [(since-fml (neg-fml l) r)
         (define result
           (if (not pcache)
               (go r)
               (relation-union
                (relation-antijoin (hash-ref pcache ϕ) (go l))
                (go r))))
         (hash-set! cache ϕ result)
         result]
        [(since-fml l r)
         (define result
           (if (not pcache)
               (go r)
               (relation-union
                (relation-natural-join (hash-ref pcache ϕ) (go l))
                (go r))))
         (hash-set! cache ϕ result)
         result])))
  (values (relation-elems rel)
          (pltl-history ϕ cache)))
