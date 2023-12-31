#lang racket/base
(module namespace-submodule racket
  (require ffi/unsafe "library.rkt" "error.rkt" "block.rkt" "vector.rkt")
  (provide (all-from-out racket ffi/unsafe "block.rkt" "vector.rkt")
           define-libgsl define-libgslcblas
           check/raise-code exn:fail:gsl?))

;;Create and export a new namespace corresponding to `namespace-submodule`
(require (submod "." namespace-submodule) racket/runtime-path)
(define-namespace-anchor anchor)
(define-runtime-module-path-index namespace-submodule '(submod "." namespace-submodule))
(define eval-namespace (module->namespace namespace-submodule (namespace-anchor->empty-namespace anchor)))
(provide eval-namespace)
