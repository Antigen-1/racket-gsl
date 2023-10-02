#lang racket/base
(require "library.rkt" ffi/unsafe)
(provide (rename-out (gsl_set_error_handler_off gsl:turn-off-error-handler)) check/raise-code)

(struct exn:fail:gsl exn:fail ())

(define gsl_error_handler_t_p (_cpointer/null 'gsl_error_handler_t))

;;Turn off and then return the default error handler
(define-libgsl gsl_set_error_handler_off (_fun -> gsl_error_handler_t_p))

;;Return an instance of `exn:fail:gsl`
(define (check/raise-code code)
  ;;Return a string describing the error code
  (define-libgsl gsl_strerror (_fun _int -> _string))

  (cond ((not (zero? code)) (raise (exn:fail:gsl (gsl_strerror code) (current-continuation-marks))))))

(module+ error-test
  (require rackunit)

  (check-not-exn (lambda () (check/raise-code 0)))
  (check-exn exn:fail:gsl? (lambda () (check/raise-code 1))))
