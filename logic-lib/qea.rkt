#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (for-space qea-space (all-defined-out))
         qea)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
		     syntax/parse)
         (only-in racket/match match-define)
         automata/machine
         racket/hash
         racket/set
         syntax-spec-v3
         "match.rkt"
         "private/qea/trie.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar

(syntax-spec
 (binding-class qea-field
   #:description "QEA field"
   #:reference-compiler immutable-reference-compiler)

 (nonterminal qea-transition-option
   #:description "QEA transition option"
   #:bind-literal-set qea-transition-option-literals
   #:binding-space qea-space

   (when e:racket-expr)
   (set fld:qea-field e:racket-expr))

 (nonterminal qea-transition
   #:description "QEA transition"
   #:bind-literal-set qea-transition-literals
   #:binding-space qea-space

   [-> src-state:id
       pat:match-pat
       dst-state:id
       opt:qea-transition-option ...]
   #:binding (scope (import pat) opt ...))

 (nonterminal/exporting qea-option
   #:description "QEA option"
   #:bind-literal-set qea-option-literals
   #:binding-space qea-space

   (∀ var:id)
   (∃ var:id)
   (start state:id)
   (field fld:qea-field e:racket-expr)
   #:binding (export fld)
   trans:qea-transition)

 (host-interface/expression
  (qea opt:qea-option ...)
  #:binding (scope (import opt) ...)
  #'(compile-qea opt ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct qvar (name))
(struct forall qvar ())
(struct exists qvar ())

(struct start (name))
(struct field (name val))

(struct transition (from to env opts))
(struct set-opt (name val))
(struct when-opt (val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiler

(begin-for-syntax
  (define-syntax-class (qea-opt flds)
    #:datum-literals (∀ ∃ start)
    (pattern (∀ x:id)
             #:with code #'(forall 'x))
    (pattern (∃ x:id)
             #:with code #'(exists 'x))
    (pattern (start x:id)
             #:with code #'(start 'x))
    (pattern f:field
             #:with code #'(field 'f.name f.e))
    (pattern t
             #:declare t (trans flds)
             #:with code #'t.code))

  (define-syntax-class field
    #:attributes (name e)
    #:datum-literals (field)
    (pattern (field name:id e:expr)))

  (define-syntax-class (trans flds)
    #:datum-literals (->)
    (pattern (-> from:id pat:expr to:id o ...)
             #:declare o (trans-opt flds #'pat)
             #:with code
             #'(transition 'from 'to
                           (pattern->env-proc pat)
                           (list o.code ...))))

  (define-syntax-class (trans-opt flds pat)
    #:datum-literals (set when)
    (pattern (set name:id e:expr)
             #:with code
             #`(set-opt 'name (pattern->val-proc #,pat e #,flds)))
    (pattern (when e:expr)
             #:with code
             #`(when-opt (pattern->val-proc #,pat e #,flds))))

  (define (sort-names names)
    (sort (syntax-e names)
          (λ (x y)
            (symbol<? (syntax-e x) (syntax-e y))))))

(define-syntax pattern->val-proc
  (syntax-parser
    [(_ pat body (fld ...))
     #:with (var ...) (binding-identifiers #'pat #:allow-host? #t)
     #'(λ (evt fld ...)
         (match evt
           [pat body]))]))

(define-syntax pattern->env-proc
  (syntax-parser
    [(_ pat)
     #:with (var ...) (binding-identifiers #'pat #:allow-host? #t)
     #'(λ (x)
         (match x
           [pat (hash (~@ 'var var) ...)]
           [_ #f]))]))

(define-syntax (compile-qea stx)
  (define/syntax-parse (_ (~or* f:field _) ...) stx)
  (syntax-parse stx
    [(_ o ...)
     #:declare o (qea-opt (sort-names #'((~? f.name) ...)))
     #'(qea-machine o.code ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; runtime (machine)

(define (qea-machine . parts)
  (match-define (start start-name)
    (findf start? parts))
  (define qvars (make-qvars (filter qvar? parts)))
  (define data (make-data (filter field? parts)))
  (define transitions (filter transition? parts))
  (define trie (leaf start-name data))
  (define next (make-next qvars transitions trie))
  (machine-accepting trie next))

(define (make-qvars qvars)
  (for/hash ([qvar (in-list qvars)])
    (values (qvar-name qvar)
            (if (exists? qvar) '∃ '∀))))

(define (make-data flds)
  (for/hash ([fld (in-list flds)])
    (match-define (field name val) fld)
    (values name val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; runtime (next)

(define (make-next qvars transitions trie)
  (define states (transition-states transitions))
  (let go ([trie trie])
    (λ (evt)
      (define trie*
        (extend trie
                (next-env qvars transitions evt)
                (next-trans transitions evt)
                symbol<?))
      (define make
        (if (trie-accepting trie* states)
            machine-accepting
            machine))
      (make trie* (go trie*)))))

(define (next-env qvars transitions evt)
  (define evt-ht
    (for*/first ([trans (in-list transitions)]
                 [ht (in-value ((transition-env trans) evt))]
                 #:when ht)
      (for/hash ([(var quant) (in-hash qvars)])
        (values var (list quant (hash-ref ht var))))))
  (or evt-ht (hash)))

(define ((next-trans transitions evt) l)
  (match-define (leaf state data) l)
  (define flds (data->fields data))
  (or (for*/first ([trans (in-list transitions)]
                   #:when (equal? (transition-from trans) state)
                   [ht (in-value ((transition-env trans) evt))]
                   #:when ht
                   [opts (in-value (transition-opts trans))]
                   #:when (next-trans-when? flds opts data evt))
        (leaf (transition-to trans)
              (next-trans-update flds opts data evt)))
      (leaf #f data)))

(define (next-trans-when? flds opts data evt)
  (for/and ([opt (in-list opts)]
            #:when (when-opt? opt))
    (apply (when-opt-val opt) evt flds)))

(define (next-trans-update flds opts data evt)
  (for/fold ([data data])
            ([opt (in-list opts)]
             #:when (set-opt? opt))
    (hash-set data
              (set-opt-name opt)
              (apply (set-opt-val opt) evt flds))))

(define (data->fields data)
  (for/list ([k (sort (hash-keys data) symbol<?)])
    (hash-ref data k)))

(define (transition-states transitions)
  (for/fold ([acc (set)]
             #:result (set->list acc))
            ([t (in-list transitions)])
    (define new (set (transition-to t) (transition-from t)))
    (set-union acc new)))
