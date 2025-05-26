#lang info

;; general

(define name "logic")
(define collection "logic")
(define pkg-desc "Core implementation of `logic` (no tests or documentation).")
(define version "0.0")
(define pkg-authors '(camoy))

;; dependencies

(define deps
  '("automata-lib"
    "base"
    "mischief"
    "pict-lib"
    "syntax-classes-lib"
    "syntax-spec-v3"))

(define build-deps
  '("rackunit-lib"))
