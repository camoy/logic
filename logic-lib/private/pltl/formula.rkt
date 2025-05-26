#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out pltl-fml)
         (struct-out pat-fml)
         (struct-out neg-fml)
         (struct-out disj-fml)
         (struct-out conj-fml)
         (struct-out exists-fml)
         (struct-out previous-fml)
         (struct-out since-fml))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/function
         racket/struct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct pltl-fml ())

(struct pat-fml pltl-fml (vars projection datum)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (const 'pat)
      (λ (self) (list (pat-fml-datum self)))))])

(struct neg-fml pltl-fml (e)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (const 'neg)
      (λ (self) (list (neg-fml-e self)))))])

(struct disj-fml pltl-fml (l r)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (const 'disj)
      (λ (self) (list (disj-fml-l self) (disj-fml-r self)))))])

(struct conj-fml pltl-fml (l r)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (const 'conj)
      (λ (self) (list (conj-fml-l self) (conj-fml-r self)))))])

(struct exists-fml pltl-fml (xs e)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (const 'exists)
      (λ (self) (list (exists-fml-xs self) (exists-fml-e self)))))])

(struct previous-fml pltl-fml (e)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (const 'previous)
      (λ (self) (list (previous-fml-e self)))))])

(struct since-fml pltl-fml (l r)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (const 'since)
      (λ (self) (list (since-fml-l self) (since-fml-r self)))))])
