#lang racket
;; Find zero weight cycles in a graph for the streams well formedness check

(provide (all-defined-out))

;; Is there a closed walk with weight 0 in the graph?
;; #f or one such path as a list of edges
(define (check-cycles graph)
  ;; This is hard, the fastest algo is probably Johnson's to list all cycles
  ;; Then see if any of those cycles have a total weight of 0
  ;; But theres no good functional impl., so we'll do it the slow way!
  ;; https://www.cs.tufts.edu/comp/150GA/homeworks/hw1/Johnson%2075.PDF
  ;; https://github.com/qpwo/python-simple-cycles/blob/master/johnson.py



  ;; For each node in the graph:
  ;; DFS to find all cycles with itself, marking edges as you go
  ;; If any of those cycles have a sum of 0, you're done
  ;; Otherwise keep looking
  ;; If we have exhausted every node, there is no cycle

  ;; Produces a list of paths
  ;; A path is a list of edges (weight to)
  (define (all-cycles-with-self origin current visited-edges)
    (define edges (hash-ref graph current (set)))
    (for/fold ([all-solutions empty])
              ([edge (in-stream edges)])
      (match-define (list to weight) edge)
      (append
       (cond [(equal? to origin) (list (list edge))]
             [(set-member? visited-edges edge) empty]
             [else
              (define solutions-from-here (all-cycles-with-self origin to (set-add visited-edges edge)))
              (for/list ([path (in-stream solutions-from-here)])
                (cons edge path))])
       all-solutions)))

  (define (path-total-weight path)
    (apply + (map second path)))

  (define (find-zero-weight-path paths)
    (for/first ([path (in-stream paths)]
                #:when (zero? (path-total-weight path)))
      path))

  (for/first ([node (in-hash-keys graph)])

    (define cycles-with-self (all-cycles-with-self node node (set)))
    (define maybe-zero-weight-path (find-zero-weight-path cycles-with-self))
    maybe-zero-weight-path))


(define (graph-path->string origin path)
  (define path-part
    (for/fold ([str ""])
              ([edge (in-stream path)])
      (string-append str (format " =~a[~a]=> ~a" (first edge) (second edge) (first edge) ))))
  (format "~a~a" origin path-part))
