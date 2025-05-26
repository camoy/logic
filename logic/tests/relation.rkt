#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           threading
           racket/set
           logic/private/pltl/relation)

  (define a-rel
    (~> (relation #hash((x . 1) (y . 2)))
        (relation-add _ #hash((x . 1) (y . 4)))
        (relation-add _ #hash((x . 3) (y . 4)))))
  (define b-rel
    (~> (relation #hash((y . 2) (z . 5)))
        (relation-add _ #hash((y . 2) (z . 6)))))
  (define c-rel (relation #hash((y . 4))))

  (chk
   (relation-natural-join a-rel b-rel)
   (~> (relation #hash((x . 1) (y . 2) (z . 5)))
       (relation-add _ #hash((x . 1) (y . 2) (z . 6))))

   (relation-antijoin a-rel c-rel)
   (relation #hash((x . 1) (y . 2)))

   (relation-select a-rel #hash((x . 3)))
   (relation #hash((x . 3) (y . 4)))

   (relation-project a-rel (set 'y))
   (~> (relation #hash((y . 2)))
       (relation-add _ #hash((y . 4))))
   ))
