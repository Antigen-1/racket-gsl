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
  (gsl-vector->vector (:-> gsl_vector? any)))
 (rename-out (gsl_vector_memcpy gsl:vector-copy!)
             (gsl_vector_swap gsl:vector-swap!)
             (gsl_vector_swap_elements gsl:vector-swap-elements!)
             (gsl_vector_reverse gsl:vector-reverse!)
             (gsl_vector_add gsl:vector-add!)
             (gsl_vector_mul gsl:vector-mul!)
             (gsl_vector_sub gsl:vector-sub!)
             (gsl_vector_div gsl:vector-div!)
             (gsl_vector_scale gsl:vector-scale!)
             (gsl_vector_add_constant gsl:vector-add-constant!)
             (gsl_vector_sum gsl:vector-sum)
             (gsl_vector_axpby gsl:vector-ax+by!)
             (gsl_vector_max gsl:vector-max)
             (gsl_vector_min gsl:vector-min)
             (gsl_vector_minmax gsl:vector-minmax)
             (gsl_vector_max_index gsl:vector-max-index)
             (gsl_vector_min_index gsl:vector-min-index)
             (gsl_vector_minmax_index gsl:vector-minmax-index)
             (gsl_vector_isnull gsl:vector-null?)
             (gsl_vector_ispos gsl:vector-pos?)
             (gsl_vector_isneg gsl:vector-neg?)
             (gsl_vector_isnonneg gsl:vector-nonneg?)
             (gsl_vector_equal gsl:vector=?)))

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

(module+ vector-test
  (require rackunit (submod "..") racket/vector racket/flonum)

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

;;Copying vectors
(define-libgsl gsl_vector_memcpy (_fun _gsl_vector-pointer _gsl_vector-pointer
                                      -> (r : _int)
                                      -> (check/raise-code r)))
(define-libgsl gsl_vector_swap (_fun _gsl_vector-pointer _gsl_vector-pointer
                                     -> (r : _int)
                                     -> (check/raise-code r)))

;;Exchanging elements
(define-libgsl gsl_vector_swap_elements (_fun _gsl_vector-pointer _size _size
                                              -> (r : _int)
                                              -> (check/raise-code r)))
(define-libgsl gsl_vector_reverse (_fun _gsl_vector-pointer -> (r : _int) -> (check/raise-code r)))

;;Vector operations
;;A <- (op A B)
(define vector-operator-type (_fun _gsl_vector-pointer _gsl_vector-pointer
                                   -> (r : _int)
                                   -> (check/raise-code r)))
(define-libgsl gsl_vector_add vector-operator-type)
(define-libgsl gsl_vector_sub vector-operator-type)
(define-libgsl gsl_vector_mul vector-operator-type)
(define-libgsl gsl_vector_div vector-operator-type)
;;A <- (op A b)
(define scalar-operator-type (_fun _gsl_vector-pointer _double
                                   -> (r : _int)
                                   -> (check/raise-code r)))
(define-libgsl gsl_vector_scale scalar-operator-type)
(define-libgsl gsl_vector_add_constant scalar-operator-type)
;;op : (-> A b)
(define-libgsl gsl_vector_sum (_fun _gsl_vector-pointer -> _double))
;;Y <- (op a X b Y)
(define-libgsl gsl_vector_axpby (_fun _double _gsl_vector-pointer _double _gsl_vector-pointer
                                      -> (r : _int)
                                      -> (check/raise-code r)))

(module+ vector-test
  (define num1 10)

  (define lst (build-list num1 (lambda (_) (random))))
  (define vec1 (list->vector lst))
  (define vec2 (list->vector (reverse lst)))
  (define vec3 (vector-copy vec1))
  (define gvec1 (vector->gsl-vector vec1))
  (define gvec2 (gsl:alloc-vector num1))

  (gsl:vector-copy! gvec2 gvec1)
  (check-true (equal? vec1 (gsl-vector->vector gvec2)))
  (gsl:vector-reverse! gvec2)
  (check-true (equal? vec2 (gsl-vector->vector gvec2)))
  (gsl:vector-swap! gvec2 gvec1)
  (check-true (equal? vec1 (gsl-vector->vector gvec2)))
  (check-true (equal? vec2 (gsl-vector->vector gvec1)))
  (gsl:vector-swap-elements! gvec2 0 (sub1 num1))
  (let ((first (vector-ref vec3 0))
        (last (vector-ref vec3 (sub1 num1))))
    (vector-set! vec3 0 last)
    (vector-set! vec3 (sub1 num1) first))
  (check-true (equal? vec3 (gsl-vector->vector gvec2)))

  (void (vector-map! (lambda (n) (fl* n 2.0)) vec3))
  (gsl:vector-scale! gvec2 2.0)
  (check-true (equal? vec3 (gsl-vector->vector gvec2)))
  (void (vector-map! (lambda (n) (fl+ n 1.0)) vec3))
  (gsl:vector-add-constant! gvec2 1.0)
  (check-true (equal? vec3 (gsl-vector->vector gvec2)))
  (void (vector-map! (lambda (n) (fl+ n 1.0)) vec3))
  (gsl:vector-add! gvec2 (vector->gsl-vector (make-vector num1 1.0)))
  (check-true (equal? vec3 (gsl-vector->vector gvec2)))
  (void (vector-map! (lambda (n) (fl* n 2.0)) vec3))
  (gsl:vector-mul! gvec2 (vector->gsl-vector (make-vector num1 2.0)))
  (check-true (equal? vec3 (gsl-vector->vector gvec2)))
  (void (vector-map! (lambda (n) (fl/ n 2.0)) vec3))
  (gsl:vector-div! gvec2 (vector->gsl-vector (make-vector num1 2.0)))
  (check-true (equal? vec3 (gsl-vector->vector gvec2)))
  (void (vector-map! (lambda (n) (fl- n 2.0)) vec3))
  (gsl:vector-sub! gvec2 (vector->gsl-vector (make-vector num1 2.0)))
  (check-true (equal? vec3 (gsl-vector->vector gvec2)))
  (check-true (= (for/fold ((s 0.0)) ((fl (in-vector vec3)))
                   (fl+ s fl))
                 (gsl:vector-sum gvec2)))
  (gsl:vector-ax+by! 1.0 (vector->gsl-vector vec2) 2.0 gvec1)
  (check-true (equal? (vector-map (lambda (n) (fl* n 3.0)) vec2)
                      (gsl-vector->vector gvec1))))

;;Finding maximum and minimum elements of vectors
(define-libgsl gsl_vector_max (_fun _gsl_vector-pointer -> _double))
(define-libgsl gsl_vector_min (_fun _gsl_vector-pointer -> _double))
(define-libgsl gsl_vector_minmax (_fun _gsl_vector-pointer (min : (_ptr o _double)) (max : (_ptr o _double))
                                       -> _void
                                       -> (values min max)))
(define-libgsl gsl_vector_max_index (_fun _gsl_vector-pointer -> _size))
(define-libgsl gsl_vector_min_index (_fun _gsl_vector-pointer -> _size))
(define-libgsl gsl_vector_minmax_index (_fun _gsl_vector-pointer (max : (_ptr o _size)) (min : (_ptr o _size))
                                             -> _void -> (values max min)))

;;Vector properties
(define predicate-type (_fun _gsl_vector-pointer -> _bool))
(define-libgsl gsl_vector_isnull predicate-type)
(define-libgsl gsl_vector_ispos predicate-type)
(define-libgsl gsl_vector_isneg predicate-type)
(define-libgsl gsl_vector_isnonneg predicate-type)
(define-libgsl gsl_vector_equal (_fun _gsl_vector-pointer _gsl_vector-pointer -> _bool))

(module+ vector-test
  (define num2 10)

  (define vec4 (build-vector num2 (lambda (_) (random))))
  (define gvec3 (vector->gsl-vector vec4))

  (define (check-=? a b)
    (check-true (= a b)))

  (check-=? (vector-argmax values vec4)
            (gsl:vector-max gvec3))
  (check-=? (vector-argmin values vec4)
            (gsl:vector-min gvec3))
  (let-values (((min max) (gsl:vector-minmax gvec3)))
    (check-=? min (vector-argmin values vec4))
    (check-=? max (vector-argmax values vec4)))
  (let-values (((min max) (gsl:vector-minmax-index gvec3)))
    (check-=? min (vector-memq (vector-argmin values vec4) vec4))
    (check-=? max (vector-memq (vector-argmax values vec4) vec4)))

  (check-true (gsl:vector-null? (vector->gsl-vector (make-vector num2 0.0))))
  (check-true (gsl:vector-pos? (vector->gsl-vector (make-vector num2 1.0))))
  (check-true (gsl:vector-nonneg? gvec3))
  (check-true (gsl:vector-neg? (vector->gsl-vector (make-vector num2 -1.0))))
  (check-true (gsl:vector=? gvec3 (vector->gsl-vector vec4))))
