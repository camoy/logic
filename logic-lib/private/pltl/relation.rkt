#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  #:unprotected-submodule unsafe
  [rename relation* relation (-> (or/c set? hash?) relation?)]
  [relation-keys (-> relation? set?)]
  [relation-elems (-> relation? set?)]
  [relation-add
   (->i ([r relation?]
         [x hash?])
        #:pre (r x) (elem-compatible? r x)
        [_ relation?])]
  [relation-select
   (->i ([r relation?]
         [x hash?])
        #:pre (r x) (elem-subset? x r)
        [_ relation?])]
  [relation-union
   (->i ([r1 relation?]
         [r2 relation?])
        #:pre (r1 r2) (relation-compatible? r1 r2)
        [_ relation?])]
  [relation-natural-join
   (-> relation? relation? relation?)]
  [relation-antijoin
   (-> relation? relation? relation?)]
  [relation-project
   (->i ([r relation?]
         [ks set?])
        #:pre (r ks) (subset? ks (relation-keys r))
        [_ relation?])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/hash
         racket/match
         racket/set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct relation (keys elems)
  #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc x y _)
     (equal? (relation-elems x) (relation-elems y)))
   (define (hash-proc x _)
     (equal-hash-code (relation-elems x)))
   (define (hash2-proc x _)
     (equal-secondary-hash-code (relation-elems x)))])

(define (relation* x)
  (cond
    [(set? x) (relation x (set))]
    [(hash? x) (relation (list->set (hash-keys x)) (set x))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; predicates

(define (elem-compatible? r x)
  (equal? (relation-keys r) (list->set (hash-keys x))))

(define (elem-subset? x r)
  (subset? (list->set (hash-keys x)) (relation-keys r)))

(define (relation-compatible? r1 r2)
  (equal? (relation-keys r1) (relation-keys r2)))

(define (hash-compatible? h1 h2)
  (define common-keys
    (set-intersect (hash-keys h1) (hash-keys h2)))
  (for/and ([key (in-set common-keys)])
    (equal? (hash-ref h1 key) (hash-ref h2 key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations

(define (relation-add r x)
  (match-define (relation ks es) r)
  (relation ks (set-add es x)))

(define (relation-select r x)
  (match-define (relation ks es) r)
  (define es*
    (for/set ([e (in-set es)]
              #:when (hash-compatible? x e))
      e))
  (relation ks es*))

(define (relation-union r1 r2)
  (match-define (relation keys es1) r1)
  (match-define (relation _ es2) r2)
  (relation keys (set-union es1 es2)))

(define (relation-natural-join r1 r2)
  (match-define (relation ks1 es1) r1)
  (match-define (relation ks2 es2) r2)
  (define es
    (for*/set ([e1 (in-set es1)]
               [e2 (in-set es2)]
               #:when (hash-compatible? e1 e2))
      (hash-union e1 e2 #:combine (λ (x y) x))))
  (relation (set-union ks1 ks2) es))

(define (relation-antijoin r1 r2)
  (match-define (relation ks1 es1) r1)
  (match-define (relation _ es2) r2)
  (define (none-compatible? e1)
    (for/and ([e2 (in-set es2)])
      (not (hash-compatible? e1 e2))))
  (define es
    (for/set ([e1 (in-set es1)]
              #:when (none-compatible? e1))
      e1))
  (relation ks1 es))

(define (relation-project r ks)
  (match-define (relation _ es) r)
  (define es*
    (for/set ([e (in-set es)])
      (for/hash ([(k v) (in-hash e)]
                 #:when (set-member? ks k))
        (values k v))))
  (relation ks es*))
