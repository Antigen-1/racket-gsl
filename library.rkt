#lang racket/base
(require racket/runtime-path ffi/unsafe setup/dirs (for-syntax racket/base))

(define-runtime-path lib-dir (build-path "gsl-fork" "build" "lib"))

(define (find-ffi-lib name version)
  (ffi-lib name (list version #f) #:get-lib-dirs (lambda () (cons lib-dir (get-lib-search-dirs)))))

(define libgsl (find-ffi-lib "libgsl" "25.1.0"))
(define libgslblas (find-ffi-lib "libgslcblas" "0.0.0"))
