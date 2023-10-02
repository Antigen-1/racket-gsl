#lang racket/base
(require racket/runtime-path ffi/unsafe setup/dirs (for-syntax racket/base))
(provide libgslcblas libgsl)

(define-runtime-path lib-dir (build-path "gsl-fork" "build" "lib"))

(define (find-ffi-lib name version #:global? (global? #f))
  (ffi-lib name (list version #f) #:global? global? #:get-lib-dirs (lambda () (cons lib-dir (get-lib-search-dirs)))))

(define libgslcblas (find-ffi-lib "libgslcblas" "0.0.0" #:global? #t))
(define libgsl (find-ffi-lib "libgsl" "25.1.0"))
