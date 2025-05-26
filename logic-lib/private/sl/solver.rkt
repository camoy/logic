#lang racket/base

(provide (all-defined-out))

(require racket/function
         racket/match
         racket/promise
         racket/set
         racket/list
         racket/stream)

;; If debug-R is enabled, then we dont forget things in R
(define debug-R (make-parameter #f))
;; Maybe wrapper
(struct none ())

(struct stream-context (R U index))
(define current-stream-context (make-parameter #f))

;; An [Option X] is one of:
;; - X
;; - (none)

;; TODO:
;; - Stream Variables
;; - Stream Equations
;; - U: A set of unresolved equations (Hash Var [Eq T])
;; - R: A set of resolved equations (Hashof Var T)

(struct var (name idx) #:transparent)
;; A Stream Variable (Var) is a (var Symbol Nat)
;;
(struct _offset (name offset default) #:transparent) ;; TODO: add default

(define (offset name offset [default (none)])
  (_offset name offset default))
(define offset-name _offset-name)
(define offset-offset _offset-offset)
(define offset-default _offset-default)


;; A Stream Variable (Var) is a (var Symbol Integer)
(define create-0 (offset 'create 0))
(define createC-1 (offset 'createC -1 (set)))

(struct eqn (var dependencies get-result) #:transparent)
;; A [StreamEquationOf T] ([Eq T]) is a (eq Symbol (Listof Var) (Var ... -> T) )

(define (offset->var ofst curr-idx)
  (var (offset-name ofst) (+ curr-idx (offset-offset ofst))))

(struct monitor (R U deps equations events index) #:transparent)
;; A Monitor monitors the streams and gives results

(define (monitor-update mon . evt*)
  ;; 1. update the corresponding event stream, if none exists then error
  ;; 2. for each equation:
  ;;    - calculate the dependencies if needed
  ;;    - plug them into get-result
  ;;    - remove from U, add to R
  ;; 3. Remove irrelevant facts from R
  (for ([evt evt*]
        #:when (not (ormap (λ (p) (eq? (car evt) p))
                           (monitor-events mon))))
    (error (format "~a is not a valid event" evt)))

  (define R+event (for/fold ([R (monitor-R mon)])
                            ([evt (in-stream evt*)])
                    (hash-set R
                              (var (car evt) (monitor-index mon))
                              (set (cdr evt)))))

  (define U+eqs (for/fold ([U (monitor-U mon)])
                          ([eq (in-stream (monitor-equations mon))])
                  (hash-set U (var (eqn-var eq) (monitor-index mon)) eq)))

  (define mut-R (hash-copy R+event))
  (define mut-U (hash-copy U+eqs))

  (for ([var (in-hash-keys U+eqs)])
    (parameterize ([current-stream-context (stream-context mut-R mut-U (monitor-index mon))])
      (when (none? (solve-for-var (var-name var) 0 (none)))
        (error "Invariant failed, got default somehow when I added this to U!"))))

  ;; Equations z_{i−k} = c are removed from R, where
  ;; k := max({0} ∪ {l | l > 0 and z[−l|c] is a subexpression in E})
  ;; For each name remove its furthest lookback based on equations
  (define max-lookback (for*/fold ([to-remove (hash)])
                                  ([edge (in-stream (monitor-deps mon))])
                         (match-define (list name offset _) edge)
                         (hash-update to-remove
                                      name
                                      (curry max (- offset))
                                      (max (- offset) 0))))

  (define R^ (make-immutable-hash (hash->list mut-R)))
  (define U^ (make-immutable-hash (hash->list mut-U)))
  ;; BUG: if `x` is not a dependency of anything it will never be cleaned up
  ;;      make this iterate through all var names instead
  ;;      also, we probably want to keep the 0th of each stream to obselogice it...
  (define R^-relevant
    (if (debug-R)
        R^
        (for/fold ([R R^])
                  ([(var-name lookback) (in-hash max-lookback)])
          (hash-remove R (var var-name (- (monitor-index mon) lookback))))))

  (struct-copy monitor mon
               (R R^-relevant)
               (U U^)
               (index (add1 (monitor-index mon)))))

;; Attempts to compute the value for var, representing the value of a stream at a position
;; Produces the updated R and U, if successful, the solved value will appear in R

;; R U Variable Integer -> [Option Any]
;; Real question: When do you use the default?
;; - In an eq, when dep does not exist
;; Only the callee knows, so they are responsible for updating R and U
;; TODO: Can R and U be combined into a single lookup table of promises?
(define (solve-for-var var-name idx-offset default)
  ;(displayln (list 'solve-for-var var-name idx-offset default))
  (match-define (stream-context R U current-index) (current-stream-context))
  (define var (offset->var (offset var-name idx-offset default) current-index))

  (cond [(hash-has-key? R var) (hash-ref R var)]
        [(hash-has-key? U var)
         (define var-value ((eqn-get-result (hash-ref U var))))
         ;; Set memo
         (hash-remove! U var)
         (hash-set! R var var-value)
         var-value]
        [else default]))

(define (monitor-update* mon evt*)
  (for/fold ([mon mon])
            ([evt (in-stream evt*)])
    (monitor-update mon evt)))

(define (monitor-update** mon evts*)
  (for/fold ([mon mon])
            ([evts (in-stream evts*)])
    (apply (curry monitor-update mon) evts)))
