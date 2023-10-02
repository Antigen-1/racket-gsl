#lang racket/base
(require ffi/unsafe ffi/unsafe/alloc "library.rkt")
(provide (struct-out gsl_block) gsl:alloc-block gsl:calloc-block)

;;The C structure
(define-cstruct _gsl_block ((size _size) (data _pointer)))

;;Allocation and deallocation
(define-libgsl gsl_block_alloc (_fun _size -> _gsl_block-pointer))
(define-libgsl gsl_block_calloc (_fun _size -> _gsl_block-pointer))
(define-libgsl gsl_block_free (_fun _gsl_block-pointer -> _void))

(define gsl:alloc-block ((allocator gsl_block_free) gsl_block_alloc))
(define gsl:calloc-block ((allocator gsl_block_free) gsl_block_calloc))

;;Block readers and writers are not bound here because `FILE` structures are used in these functions

(module+ block-test
  (require rackunit)

  (define b1 (gsl:alloc-block 100))
  (define b2 (gsl:alloc-block 100))
  (check-true (= 100 (gsl_block-size b1) (gsl_block-size b2))))
