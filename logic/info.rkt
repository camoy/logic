#lang info

;; general

(define name "logic")
(define collection "logic")
(define pkg-desc "Full implementation of `logic`.")
(define version "0.0")
(define scribblings
  '(["scribblings/logic.scrbl" ()]))

;; dependencies

(define deps
  '("base"
    "logic-lib"))

(define implies
  '("logic-lib"))

(define build-deps
  '("automata-doc"
    "sandbox-lib"
    "automata-lib"
    "chk-lib"
    "racket-doc"
    "scribble-lib"
    "threading-lib"))
