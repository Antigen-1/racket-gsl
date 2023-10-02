#lang racket/base
(require "error.rkt")
(provide (all-from-out racket/base))

;;Configuration
(void (gsl:turn-off-error-handler))
