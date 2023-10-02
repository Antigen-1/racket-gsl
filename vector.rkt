#lang racket/base
(require "library.rkt" "block.rkt" "error.rkt"
         ffi/unsafe ffi/unsafe/alloc
         racket/match (except-in racket/contract ->) (rename-in racket/contract (-> :->)))
(provide
 (struct-out gsl_vector) _gsl_vector-pointer _gsl_vector-pointer/null
  gsl:alloc-vector gsl:calloc-vector
 (rename-out (gsl_vector_set_all gsl:vector-set-all!)
             (gsl_vector_set_zero gsl:vector-set-zero!)
             (gsl_vector_set_basis gsl:vector-set-basis!))
 (contract-out
  ;;Range checking
  (rename gsl_vector_get gsl:vector-ref
          (opt/c
           (->i ((v gsl_vector?) (i exact-nonnegative-integer?)) ()
                #:pre (v i) (not/c (out-of-range? v i))
                any)))
  (rename gsl_vector_set gsl:vector-set!
          (opt/c
           (->i ((v gsl_vector?) (i exact-nonnegative-integer?) (_ double-flonum?)) ()
                #:pre (v i) (not/c (out-of-range? v i))
                any)))
  (rename gsl_vector_ptr gsl:vector-ptr
          (opt/c
           (->i ((v gsl_vector?) (i exact-nonnegative-integer?)) ()
                #:pre (v i) (not/c (out-of-range? v i))
                any)))

  ;;Other utilities
  (vector->gsl-vector (:-> (vectorof double-flonum?) any))
  (gsl-vector->vector (:-> gsl_vector? any))))

;;The C structure
(define-cstruct _gsl_vector
  ((size _size) (stride _size) (data _pointer) (block _gsl_block-pointer/null) (owner _int)))

;;Allocation and deallocation
(define-libgsl gsl_vector_alloc (_fun _size -> _gsl_vector-pointer))
(define-libgsl gsl_vector_calloc (_fun _size -> _gsl_vector-pointer))
(define-libgsl gsl_vector_free (_fun _gsl_vector-pointer -> _void))

(define gsl:alloc-vector ((allocator gsl_vector_free) gsl_vector_alloc))
(define gsl:calloc-vector ((allocator gsl_vector_free) gsl_vector_calloc))

;;Accessing vector elements
(define-libgsl gsl_vector_get (_fun _gsl_vector-pointer _size -> _double))
(define-libgsl gsl_vector_set (_fun _gsl_vector-pointer _size _double -> _void))
(define-libgsl gsl_vector_ptr (_fun _gsl_vector-pointer _size -> _pointer))

(define (out-of-range? gvec ind)
  (match gvec
    ((gsl_vector size _ _ _ _)
     (>= ind size))))

;;Transformer
(define (vector->gsl-vector vec)
  (let* ((len (vector-length vec))
         (new (gsl:calloc-vector len)))
    (for (((e i) (in-indexed (in-vector vec))))
      (gsl_vector_set new i e))
    new))
(define (gsl-vector->vector gvec)
  (match gvec
    ((gsl_vector size _ _ _ _)
     (let ((new (make-vector size)))
       (for ((i (in-range 0 size)))
         (vector-set! new i (gsl_vector_get gvec i)))
       new))))

;;Initializing vector elements
(define-libgsl gsl_vector_set_all (_fun _gsl_vector-pointer _double -> _void))
(define-libgsl gsl_vector_set_zero (_fun _gsl_vector-pointer -> _void))
(define-libgsl gsl_vector_set_basis (_fun _gsl_vector-pointer _size -> (r : _int)
                                          -> (check/raise-code r)))

;;Vector readers, writers and view functions are not bound here

(module* vector-instantiation-test racket/base
  (require rackunit (submod "..") racket/vector)

  (define num 100)

  (define vec (make-vector num 10.0))
  (define gvec (vector->gsl-vector vec))
  (check-equal? vec (gsl-vector->vector gvec))
  (gsl:vector-set-all! gvec 12.0)
  (check-equal? (make-vector num 12.0) (gsl-vector->vector gvec))
  (gsl:vector-set-basis! gvec 0)
  (check-equal? (vector-append (vector 1.0) (make-vector (sub1 num) 0.0)) (gsl-vector->vector gvec))
  (gsl:vector-set-zero! gvec)
  (check-equal? (make-vector num 0.0) (gsl-vector->vector gvec)))
