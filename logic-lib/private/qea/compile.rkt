#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide compile-qea)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket
                     racket/match/parse
                     racket/match/patterns
                     syntax/id-set
                     syntax/parse)
         automata/machine
         racket/function
         racket/list
         racket/match
         "trie.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros

(begin-for-syntax
  ;; (U '∀ '∃ 'forall 'exists) -> (U '∀ '∃)
  ;; Turns the text 'forall and 'exists into their unicode character, otherwise no-op
  (define (normalize-quantifier sym)
    (match sym
      ['forall '∀]
      ['exists '∃]
      [else sym]))

  (define-syntax-class quantified-var
    #:attributes (code quantifier variable) ;; Syntax[Expr]
    (pattern (quantifier:id variable:id)
      #:fail-unless (member (syntax-e #'quantifier) '(∃ ∀)) "Quantifier must be either '∃' or '∀'"
      #:with code #'(list (normalize-quantifier (quote quantifier)) (quote variable))))

  (define-syntax-class transition-clause
    #:datum-literals (->)
    #:attributes (from ep guard  no-match to (set-field-id 1) (set-field-value 1))
    (pattern (-> from:id wrapped-ep:event-pattern
                 guard-pat:maybe-guard
                 (~optional (~seq #:set (set-field-id:id set-field-value:expr) ...)
                            #:defaults  (((set-field-id 1) (list))
                                         ((set-field-value 1) (list))))
                 no-match-pat:maybe-no-match
                 to:id)

      #:with ep #'wrapped-ep.ep
      #:with guard #'guard-pat.guard
      #:with no-match #'no-match-pat.no-match))

  (define-syntax-class event-pattern
    #:datum-literals (#%host-expression)
    #:attributes (ep)
    (pattern (#%host-expression ep:expr))
    (pattern ep:expr))

  (define-splicing-syntax-class maybe-guard
                                #:attributes (guard) ; Any -> Boolean
                                (pattern (~seq) #:with guard #'#t)
                                (pattern (~seq #:when guard:expr)))

  (define-splicing-syntax-class
   maybe-no-match
   #:attributes (no-match) ; (U 'fail 'loop)
   (pattern (~seq)
     #:with no-match #''fail)
   (pattern (~seq #:no-match no-match:expr)
     #:fail-unless (member (syntax-e #'no-match) '(fail loop)) "Must be one of 'fail or 'loop")))

(define-syntax compile-qea
  (syntax-parser
    [(_ qv:quantified-var ...
        (~optional (~seq #:start user-start-state:id))
        (~optional (~seq #:fields (field-name:id field-data:expr) ... ))
        t:transition-clause ...)
     #:with qea-start-state #'(~? user-start-state 'start)
     #:with qea-start-data #'(~? (hash (~@ 'field-name field-data) ...) (hash))
     #:with (qea-state:id ...)
     (bound-id-set->list (immutable-bound-id-set (syntax-e #`(t.from ... t.to ... qea-start-state))))
     #`(let* ([init-trie (make-init-trie 'qea-start-state qea-start-data)]
              [ρ (build-get-quantified-var-env (qv ...) (t.ep ...))]
              [δ (build-transition-function t ... (~? (~@ field-name ... field-data ...)))]
              [next (make-qea-next ρ δ '(qv.variable ...))]
              [accepting? (curryr trie-accepting '(qea-state ...))])
         (make-qea-machine init-trie next accepting?))]))

;; OLD: (qea-struct init-trie next accepting? '(qv ...))

(define (make-qea-machine trie next accepting?)
  (define make
    (if (accepting? trie)
        machine-accepting
        machine))
  (make trie (qea-machine-next trie next accepting?)))

(define ((qea-machine-next trie next accepting?) evt)
  (make-qea-machine (next trie evt) next accepting?))

(define (make-init-trie qea-start-state qea-start-data)
  (leaf qea-start-state qea-start-data))

(define ((make-qea-next get-quantified-var-env transition-function vars) trie event)
  (extend trie
          (get-quantified-var-env event)
          (λ (l)
            (define-values (state data)
              (transition-function (leaf-state l) (leaf-data l) event))
            (leaf state data))
          (λ (v1 v2)
            (let ([i1 (index-of vars v1)]
                  [i2 (index-of vars v2)])
              (and i1 (or (not i2) (< i1 i2)))))))

(define-syntax build-get-quantified-var-env
  (syntax-parser
    [(_ (qv:quantified-var ...) (ep ...))
     #:do [(define qv-var-set (syntax-e #`(qv.variable ...)))]
     #:do [(define var->quantifier (make-hash (syntax->datum #`((qv.variable . qv.quantifier) ...))))]
     #:with [((quant bv) ...) ...]
     (for/list ([ep (in-syntax #`(ep ...))])
       (for/list ([var (in-list (bound-id-set->list
                                 (bound-id-set-intersect
                                  (immutable-bound-id-set (bound-vars (parse ep)))
                                  (immutable-bound-id-set qv-var-set))))])
         (list (hash-ref var->quantifier (syntax-e var)) var)))
     #`(lambda (event)
         (match event
           [ep (hash (~@ (quote bv) (list (quote quant) bv)) ...)] ...
           [else (error (format "invalid event, got ~a" event))]))]))

(define-syntax build-transition-function
  (syntax-parser
    [(_ t:transition-clause ... field-name:id ... field-data:expr ...)
     #:with (self-looping-t:transition-clause ...)
     (for/list ([t0 (in-syntax #`(t ...))]
                [no-match (syntax->datum #`(t.no-match ...))]
                #:when (equal? no-match 'loop))
       t0)
     #`(lambda (state data event)
         (match (list state event)
           [(list (quote t.from) t.ep)

            #:when (match-let ([t.ep event])
                     (let ((field-name (hash-ref data (quote field-name))) ...)
                       t.guard))

            (transition-match t event data field-name ... field-data ...)]
           ...
           [(list (quote self-looping-t.from) _) (values state data)] ...
           [else (values #f data)]))]))

(define-syntax transition-match
  (syntax-parser
    ((_ t:transition-clause event:expr data:expr field-name:id ... field-data:expr ...)
     #'(values (and (not (equal? (quote t.to) 'failure)) (quote t.to))
               (match-let ([t.ep event])
                 (hash-set* data
                            (~? (~@  (quote t.set-field-id)
                                     (let ((field-name (hash-ref data (quote field-name))) ...)
                                       t.set-field-value)))
                            ...))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(module+ examples
  ;; The following examples use QEA macros defined lower in the file

  ;; Auction Bidding QEA
  ;; Implementation using Hash {X = (Hashof Symbol Nat)}
  (define qea-bid
    (compile-qea (∀ i)
     #:start start
     #:fields (curr-bid 0) (min-bid 0)
     [-> start (list 'put-up-item i r) #:set (curr-bid  0) (min-bid r) item-listed]
     [-> item-listed (list 'bid i a) #:when (> a curr-bid) #:set (curr-bid a) item-listed]
     [-> item-listed (list 'sell i) #:when (> curr-bid min-bid) sold]
     [-> sold #f start])) ;; Figure out what this means...

  (define qea-auction
    (compile-qea
     (∀ i)
     #:start start
     #:fields (current-bid 0)
     [-> start `(bid ,i ,amount) #:set (current-bid amount) listed]
     [-> listed
         `(bid ,i ,amount)
         #:when (> amount current-bid)
         #:set (current-bid amount)
         listed]))

  (define qea-unsafe-map-iterator
    (compile-qea
     (∀ m) (∀ c) (∀ i)
     #:start start
     [-> start `(create ,m ,c) #:no-match loop createdC]
     [-> createdC `(iterator ,c ,i) #:no-match loop createdI]
     [-> createdI `(update ,m) #:no-match loop updated]
     [-> updated `(next ,i) #:no-match loop failure]))

  (define qea-candidate-ranking
    (compile-qea
     (∀ v) (∃ c) (∀ c)
     #:start start
     [-> start `(member ,v ,p) no-candidate]
     [-> no-candidate `(candidate ,c ,p) candidate-not-selected]
     [-> candidate-not-selected `(rank ,v ,c ,r) candidate-ranked]))

  (provide (all-defined-out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require rackunit)
  (require (submod ".." examples))

  #|
  (test-equal? "applying the empty trace produces the same trie as qea-bid itself"
               (qea-struct-trie (apply-trace qea-bid '()))
               (leaf 'start  (hash 'curr-bid 0 'min-bid 0)))

  (test-equal?
   "putting up an item for action results in a new branch where d=2, and an item is listed with r=2, c=0"
   (qea-struct-trie (apply-trace qea-bid '((put-up-item d 2))))
   (node '∀ 'i (hash 'd (leaf 'item-listed (hash 'curr-bid 0 'min-bid 2))) (leaf 'start (hash 'curr-bid 0 'min-bid 0))))

  (test-equal? "putting up an item, and then bidding, results in the same as above but c=new bid"
               (qea-struct-trie (apply-trace qea-bid '((put-up-item d 2) (bid d 1))))
               (node '∀ 'i (hash 'd (leaf 'item-listed (hash 'curr-bid 1 'min-bid 2))) (leaf 'start (hash 'curr-bid 0 'min-bid 0))))
  (test-equal?
   "putting up an item, and then bidding many times, results in the same as above but c={highest bid}"
   (qea-struct-trie
    (apply-trace qea-bid '((put-up-item d 2) (bid d 1) (bid d 2) (bid d 3) (bid d 5) (bid d 8))))
   (node '∀ 'i (hash 'd (leaf 'item-listed (hash 'curr-bid 8 'min-bid 2))) (leaf 'start (hash 'curr-bid 0 'min-bid 0))))

  (test-equal?
   "putting up an item, bidding, then selling higher than initial listing results in a sold leaf"
   (qea-struct-trie (apply-trace qea-bid '((put-up-item d 2) (bid d 8) (sell d))))
   (node '∀ 'i (hash 'd (leaf 'sold (hash 'curr-bid 8 'min-bid 2))) (leaf 'start (hash 'curr-bid 0 'min-bid 0))))

  (test-equal?
   "putting up an item, bidding, then selling higher than initial listing results in a sold leaf"
   (qea-struct-trie (apply-trace qea-bid '((put-up-item d 2) (bid d 8) (sell d))))
   (node '∀ 'i (hash 'd (leaf 'sold (hash 'curr-bid 8 'min-bid 2))) (leaf 'start (hash 'curr-bid 0 'min-bid 0))))

  (test-equal? "selling, then performing another action on a sold item, is invalid [1]"
               (qea-struct-trie
                (apply-trace qea-bid '((put-up-item d 2) (bid d 8) (sell d) (bid d 8))))
               (node '∀ 'i (hash 'd (leaf #f (hash 'curr-bid 8 'min-bid 2))) (leaf 'start (hash 'curr-bid 0 'min-bid 0))))
  (test-equal? "selling, then performing another action on a sold item, is invalid [2]"
               (qea-struct-trie
                (apply-trace qea-bid '((put-up-item d 2) (bid d 8) (sell d) (sell d))))
  (node '∀ 'i (hash 'd (leaf #f (hash 'curr-bid 8 'min-bid 2))) (leaf 'start (hash 'curr-bid 0 'min-bid 0))))

  |#

  ;; Test Consistency
  ;;

  (define hat-τ-good `((bid "hat" 4) (bid "hat" 6) (bid "hat" 20)))
  (define bread-τ-good `((bid "bread" 21) (bid "bread" 22) (bid "bread" 25)))
  (define hat-τ-bad (append hat-τ-good `((bid "hat" 18))))
  (define bread-τ-bad `((bid "bread" 50) (bid "bread" 13) (bid "bread" 15)))

  (test-not-false "auction-qea hat accepts" (machine-accepts? qea-auction hat-τ-good))
  (test-not-false "auction-qea bread accepts" (machine-accepts? qea-auction bread-τ-good))
  (test-false "auction-qea hat fails" (machine-accepts? qea-auction hat-τ-bad))
  (test-false "auction-qea bread fails" (machine-accepts? qea-auction bread-τ-bad))

  (define map-example-bad
    `((create m1 c1) (create m1 c2) (iterator c1 i1) (update m1) (iterator c2 i2) (next i1)))
  (define map-example-good
    `((create m1 c1) (create m1 c2) (iterator c1 i1) (iterator c2 i2) (next i1)))

  (test-false "Unsafe Map Iterator example 1" (machine-accepts? qea-unsafe-map-iterator map-example-bad))
  (test-not-false "Unsafe Map Iterator example 2"
                  (machine-accepts? qea-unsafe-map-iterator map-example-good)))
