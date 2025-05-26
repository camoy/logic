#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide extend
         node
         leaf
         leaf-state
         leaf-data
         trie-accepting)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/list
         racket/match
         racket/syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

;; A Var is a Symbol
;; A Quantifier is a (U '∀ '∃)

(struct node (quantifier var children default) #:transparent)
;; A Node is a (node Quantifier Var (hashof Any Trie) Trie)

(struct leaf (state data) #:transparent)
;; A Leaf is a (leaf Symbol Any)

;; A QEA-Trie is one of
;; - Node
;; - Leaf
;; and represents a Trie,
;; where each node represents the "branching" of quantified vars into seperate "worlds"
;; and each leaf represents a concrete "world", with an automata state and relevant data

;; An Env is a (hash (Quantifier Var) Any)
;; and represents an environment of variables and their associated values

;; A δ is a Leaf -> Leaf
;; and represents a transition function to be applied

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

;; extend : Trie Env δ (Var Var -> Boolean) -> Trie
;; produces a trie extended with the environment
(define (extend trie env δ var<)
  (cond
    [(node? trie) (extend-node trie env δ var<)]
    [(leaf? trie) (extend-leaf trie env δ var<)]))

;; env-consistent? : Env Var Any -> Boolean
;; is this var (associated with val) consistent with env?
(define (env-consistent? env var val)
  (or (not (hash-has-key? env var))
      (equal? (second (hash-ref var)) val)))

(define (find-quantifier-before env var-target var<)
  (for/first ([(var-current val) (in-hash env)] #:when (var< var-current var-target))
    var-current))

(define (extend-node node env δ var<)
  (cond
    [(find-quantifier-before env (node-var node) var<)
     =>
     (lambda (preceeding-var) (extend-node-var-present-before node env δ preceeding-var var<))]
    [(hash-has-key? env (node-var node)) (extend-node-var-present node env δ var<)]
    [else (extend-node-var-absent node env δ var<)]))

(define (extend-node-var-present n env δ var<)
  (match-define (node _ var children _) n)
  (define val (second (hash-ref env var)))
  (if (hash-has-key? children val)
      (extend-node-var-present1 n env val δ var<)
      (extend-node-var-present2 n env val δ var<)))

(define (extend-node-var-present-before n env δ preceeding-var var<)
  ;; copy the default, set node as child with val
  ;; set default as node's default
  ;; recur on node with one less in env
  (match-define `(,quantifier ,val) (hash-ref env preceeding-var))
  (define children (hash val (extend n (hash-remove env preceeding-var) δ var<)))
  (node quantifier preceeding-var children (node-default n)))

(define (extend-node-var-present1 n env val δ var<)
  (match-define (node _ var children _) n)
  (define t (hash-ref children val))
  (define env* (extend t (hash-remove env var) δ var<))
  (struct-copy node n (children (hash-set children val env*))))

(define (extend-node-var-present2 n env val δ var<)
  (match-define (node _ _ children _) n)
  (define children* (hash-set children val (node-default n)))
  (define node* (struct-copy node n (children children*)))
  (extend node* env δ var<))

(define (extend-node-var-absent n env δ var<)
  (define children*
    (for/hash ([(val trie) (node-children n)])
      (if (env-consistent? env (node-var n) val)
          (values val (extend trie (hash-remove env (node-var n)) δ var<))
          (values val trie))))
  (define default* (extend (node-default n) (hash-remove env (node-var n)) δ var<))
  (struct-copy node n (children children*) (default default*)))

(define (extend-leaf leaf env δ var<)
  (let recur ([var/val* (sort (hash->list env) #:key car var<)])
    (match var/val*
      [`((,var . (,quantifier ,val)) . ,rst) (node quantifier var (hash val (recur rst)) leaf)]
      [`() (δ leaf)])))

(define (trie-accepting trie accept-states)
  (match trie
    [(leaf state data)
     (member state accept-states)]
    [(node '∀ _ children _)
     (for/and ([c (in-hash-values children)])
       (trie-accepting c accept-states))]
    [(node '∃ _ children _)
     (for/or ([c (in-hash-values children)])
       (trie-accepting c accept-states))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require rackunit)

  ;; Adds a '! to the end of the leaf state name
  ;; Useless little transition for testing
  (define (test-transition l)
    (leaf (format-symbol "~a!" (leaf-state l)) (leaf-data l)))

  (define vars '(x y z))
  (define (test-var< x y)
    (< (index-of vars x) (index-of vars y)))

  ;; Examples
  (define START (leaf 'q0 #f))
  (define TRIE-0 (leaf 'q0 #f))
  (define TRIE-1 (leaf 'q1 #f))
  (define TRIE-2 (leaf 'q2 #f))
  (define TRIE-3 (node '∀ 'x (hash 1 TRIE-1) TRIE-0))
  (define TRIE-4 (node '∀ 'x (hash 2 TRIE-1) TRIE-0))
  (define TRIE-5 (node '∀ 'y (hash 10 TRIE-4 20 TRIE-3) TRIE-0))

  (test-equal? "extending a leaf with an env of one var/val binding"
               (extend START (hash 'x '(∀ 1)) test-transition test-var<)
               (node '∀ 'x (hash 1 (leaf 'q0! #f)) START))

  (test-equal? "extending a leaf with an env of two var/val bindings"
               (extend START (hash 'x '(∀ 1) 'y '(∀ 2)) test-transition test-var<)
               (node '∀ 'x (hash 1 (node '∀ 'y (hash 2 (leaf 'q0! #f)) START)) START))

  (test-equal? "extending a node with its var but a different val"
               (extend (node '∀ 'x (hash 1 TRIE-1) TRIE-0) (hash 'x '(∀ 2)) test-transition test-var<)
               (node '∀ 'x (hash 1 (leaf 'q1 #f) 2 (leaf 'q0! #f)) TRIE-0))

  (test-equal? "extending a node with its var but the same val"
               (extend (node '∀ 'x (hash 1 TRIE-1) TRIE-0) (hash 'x '(∀ 1)) test-transition test-var<)
               (node '∀ 'x (hash 1 (leaf 'q1! #f)) TRIE-0))

  (test-equal? "extending a node with a different var"
               (extend (node '∀ 'x (hash 1 TRIE-1) TRIE-0) (hash 'y '(∀ 2)) test-transition test-var<)
               (node '∀
                     'x
                     (hash 1 (node '∀ 'y (hash 2 (leaf 'q1! #f)) TRIE-1))
                     (node '∀ 'y (hash 2 (leaf 'q0! #f)) TRIE-0)))
  (test-equal?
   "extending a node with a different var that should precede it"
   (extend (node '∃ 'z (hash 3 TRIE-1) TRIE-0) (hash 'y '(∀ 2)) test-transition test-var<)
   (node '∀ 'y (hash 2 (node '∃ 'z (hash 3 (leaf 'q1! #f)) (leaf 'q0! #f))) (leaf 'q0 #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visualize

(module+ visualize
  (provide trie->image)

  ;; trie->image : Trie -> Image
  ;; draws a trie, useful for debugging or visualizing QEA state
  (require pict
           pict/tree-layout
           racket/string)

  (define (trie->image t)
    (define (trie->image/acc t bindings-so-far)
      (match t
        [(node _ var children default)
         (apply tree-layout
                #:pict (text (pretty-print-env bindings-so-far) 'default 16)
                `(,@(for/list ([(val child) (in-hash children)])
                      (tree-edge (trie->image/acc child (hash-set bindings-so-far var val))))
                  ,(tree-edge (trie->image/acc default (hash-set bindings-so-far var "★")))))]
        [(leaf state _)
         (tree-layout #:pict (vc-append (text (format "~a" state) 'default 16)
                                        (text (pretty-print-env bindings-so-far) 'default 16)))]))
    (naive-layered (trie->image/acc t (hash))))

  ;; pretty-print-env :: Env -> String
  (define (pretty-print-env env)
    (for/fold ([los '()] #:result (string-append "{ " (string-join los ", ") " }"))
              ([(var val) (in-hash env)])
      (cons (format "~a=~a" var val) los))))
