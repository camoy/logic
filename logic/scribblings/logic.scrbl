#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require


@require[@for-label[automata/machine
		    logic/match
		    logic/pltl
		    logic/pltl
		    logic/re
		    logic/dfa
		    logic/nfa
		    logic/qea
		    logic/sl
		    racket/base
		    racket/contract
                    racket/set]
         racket/sandbox
         scribble/example]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; example evaluator

@(define evaluator
   (make-base-eval
     '(require automata/machine
	       racket/set
	       logic/match
	       logic/pltl
	       logic/re
	       logic/dfa
	       logic/nfa
	       logic/qea
	       logic/sl)))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Logical Notations}
@author{Cameron Moy}
@author{Ryan Jung}

This library provides a number of domain-specific DSLs,
especially suited from writing domain-specific contracts.

@section{Match}

@defmodule[logic/match]

@defform[(match e [pat e ...+] ...)
         #:grammar
         [(pat (code:line id)
               (code:line (var id))
               (code:line (@#,racketid[and2] pat pat))
               (code:line (@#,racketid[?] expr))
               (code:line (@#,racketid[app] expr pat))
	       (code:line (struct-id pat ...))
	       (code:line (@#,racketid[not] pat)))]]{
  A match pattern language, similar to Racket's.
  @examples[#:eval evaluator #:label #f
    (struct foo (x y))
    (match (foo 1 2)
      [(foo z w) (+ z w)])]
}

@defform[(define-match-expander id body)]{
  Defines a match pattern macro, similar to Racket's match expander.
}

@section{Past-Time Linear Temporal Logic (PLTL)}

@defmodule[logic/pltl]

@defform[(define-pltl id ϕ)
         #:grammar
         [(ϕ (code:line (@#,racket[neg] ϕ))
             (code:line (@#,racket[disj] ϕ ϕ))
             (code:line (@#,racket[conj] ϕ ϕ))
             (code:line (@#,racket[exists] (id ...) ϕ))
             (code:line (@#,racket[previous] ϕ))
             (code:line (@#,racket[since] ϕ ϕ))
	     (code:line pat))]]{
  Defines a PLTL formula that can be used in @racket[pltl].
  PLTL formulae must be monitorable,
  otherwise a compile-time error is raised.
  Atomic propositions are match patterns.
}

@defsubform[(neg e)]{
  Represents the negation of formula @racket[e].
  A negation can only appear inside a formula
  of the following shapes:
  @racket[(conj a (neg b))],
  @racket[(conj (neg a) b)],
  @racket[(since (neg a) b)].
}

@defsubform[(disj l r)]{
  Represents the disjunction of two formulae.
  The free variables of @racket[l]
  must be equal to
  the free variables of @racket[r].
}

@defsubform[(conj l r)]{
  Represents the conjunction of two formulae.
  There is a restriction on conjunctions
  of the following shapes:
  @racket[(conj (neg a) b)],
  @racket[(conj b (neg a))].
  Namely,
  the free variables of @racket[a]
  must be a subset of
  the free variables of @racket[b].
}

@defsubform[(exists (x ...+) e)]{
  Given the a sequence of variables,
  this formula is satisfied when
  there exists some mapping
  from the variables to values
  such that @racket[e] holds.
  The free variables of the entire formula
  is the set of free variables of @racket[e]
  minus the quantified @racket[x]s.
}

@defsubform[(previous e)]{
  Requires that @racket[e] be
  satisfied at the last time step.
}

@defsubform[(since l r)]{
  Requires that there is some time
  in the past such that @racket[r] holds.
  Since the latest such time (inclusive)
  until the current time step (exclusive),
  @racket[l] holds.
  The free variables of @racket[l]
  must be a subset of
  the free variables of @racket[r].
}

@defform[(define-pltl-rule (name arg ...) body)]{
  Defines a PLTL macro.
}

@defform[(pltl formula)]{
  Compiles a PLTL formula into a @racket[machine].
  @examples[#:eval evaluator #:label #f
    (machine-accepts? (pltl (previous (? number?))) '(1 a))
    (machine-accepts? (pltl (previous (? number?))) '(1 a 2))]
}

@section{Regular Expressions (RE)}

@defmodule[logic/re]

@defform[(define-re id r)
         #:grammar
         [(r (code:line (@#,racket[complement] r))
             (code:line (@#,racket[seq] r ...))
             (code:line (@#,racket[union] r ...))
             (code:line (@#,racket[star] r))
             (code:line (@#,racket[epsilon]))
             (code:line (@#,racket[nullset]))
	     (code:line pat))]]{
  Defines a regular expression that can be used in @racket[re].
}

@defsubform[(complement r)]{
  Returns the complement of @racket[r], i.e., everything accepted by @racket[r] is no longer accepted and vice versa.
}

@defsubform[(seq r ...)]{
  Returns the concatenation of the given regular expressions.
}

@defsubform[(union r ...)]{
  Returns the union of the given regular expressions.
}

@defsubform[(star r)]{
  Returns the Kleene star, i.e. zero or more repetitions, of the given regular expression.
}

@defsubform[(epsilon)]{
  A regular expression corresponding to the singleton language with the empty string.
}

@defsubform[(nullset)]{
  A regular expression corresponding to the empty language.
}

@defform*[((define-re-rule name body)
           (define-re-rule (name arg ...) body))]{
  Defines a regular expression macro.
}

@defform[(re r)]{
  Compiles a regular expression into a machine.
  @examples[#:eval evaluator #:label #f
    (machine-accepts? (re (plus 'a)) '(a a))
    (machine-accepts? (re (plus 'a)) '(a b))]
}

@section{Deterministic Finite Automata (DFA)}

@defmodule[logic/dfa]

@defform[(dfa start
              (accept ...)
              [state evt next-state] ...)]{
  Compiles the given DFA to a @racket[machine].

  @examples[#:eval evaluator #:label #f
    (define M
      (dfa s1 (s1)
           [s1 (equal? 0) s2]
           [s1 (? even?) s1]
           [s2 (equal? 0) s1]
           [s2 (? even?) s2]))
    (machine-accepts? M (list 2 0 4 0 2))
    (machine-accepts? M (list 0 4 0 2 0))]
}

@section{Non-deterministic Finite Automata (NFA)}

@defmodule[logic/nfa]

@defform[(nfa start
              (accept ...)
              [state evt next-state] ...)]{
  Compiles the given NFA to a @racket[machine].
}

@section{Quantified Event Automata (QEA)}

@defmodule[logic/qea]

@defform[(qea opt ...)
	 #:grammar
	 [(opt (@#,racketid[∀] id)
	       (@#,racketid[∃] id)
	       (@#,racketid[start] id)
	       (@#,racketid[field] fld-id expr)
	       [@#,racketid[->] src-id pat tgt-id trans-opt ...])
	  (trans-opt (@#,racketid[when] expr)
		     (@#,racketid[set] fld-id expr))]]{
  Compiles the given QEA to a @racket[machine].
  @examples[#:eval evaluator #:label #f
    (define Q
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
            (when (> curr-bid min-bid))]))
    (machine-accepts? Q '((put-up-item a 10)
			  (put-up-item b 50)
			  (bid a 100)
			  (sell a)))
    (machine-accepts? Q '((put-up-item a 10)
			  (put-up-item b 50)
			  (bid a 5)
			  (sell a)))]
}

@defsubform[(∀ id)]{ Universally quantifies the machine over @racket[id]. }
@defsubform[(∃ id)]{ Existentially quantifies the machine over @racket[id]. }
@defsubform[(start id)]{ Specifies the start state of the QEA. }
@defsubform[(field id)]{ Specifies a field of the QEA. }
@defsubform[[-> src-id pat tgt-id trans-opt ...]]{ Specifies a transition of the QEA. }
@defsubform[(when expr)]{ The transition can only occur when @racket[expr] is @racket[#t], where @racket[expr] can reference fields. }
@defsubform[(set fld-id expr)]{ Sets the given field when the transition it taken. }

@section{Stream Logic (SL)}

@defmodule[logic/sl]

@defform[(sl (id ...) [id expr] ...)]{
  Compiles the given set of stream logic equations to a @racket[machine].
  @examples[#:eval evaluator #:label #f
    (define S
      (sl
        (in)
        [out (and (out -1 #t) (set-first in))]))
    (machine-accepts? S '((in . #t) (in . #t) (in . #t)))
    (machine-accepts? S '((in . #t) (in . #t) (in . #f)))]
}
