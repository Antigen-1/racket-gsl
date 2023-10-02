#lang racket
(require "library.rkt" "error.rkt" "block.rkt" "vector.rkt")
(provide (all-from-out racket)
         define-libgsl define-libgslcblas
         check/raise-code
         (struct-out gsl_block) _gsl_block-pointer _gsl_block-pointer/null gsl:alloc-block gsl:calloc-block
         (struct-out gsl_vector) _gsl_vector-pointer _gsl_vector-pointer/null gsl:alloc-vector gsl:calloc-vector gsl:vector-ptr gsl:vector-ref gsl:vector-set! gsl:vector-set-all gsl:vector-set-basis gsl:vector-set-zero)

;;Configuration
(void (gsl:turn-off-error-handler))
