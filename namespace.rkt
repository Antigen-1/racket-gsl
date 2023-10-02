#lang racket
(require "error.rkt")
(provide (all-from-out racket))

;;Configuration
(void (gsl:turn-off-error-handler))
