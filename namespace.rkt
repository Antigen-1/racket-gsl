#lang racket/base
(module namespace-submodule racket
  (require ffi/unsafe "library.rkt" "error.rkt" "block.rkt" "vector.rkt")
  (provide (all-from-out racket ffi/unsafe "block.rkt" "vector.rkt")
           define-libgsl define-libgslcblas
           check/raise-code))

;;Create and export a new namespace corresponding to `namespace-submodule`
(require 'namespace-submodule racket/runtime-path)
(define-namespace-anchor anchor)
(define-runtime-module-path-index namespace-submodule 'namespace-submodule)
(define eval-namespace (module->namespace namespace-submodule (namespace-anchor->empty-namespace anchor)))
(provide eval-namespace)
