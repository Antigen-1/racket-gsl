#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here



(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline racket/file racket/contract expeditor raco/command-name "namespace.rkt")

  (define history (box #f))
    
  (command-line
    #:program (short-program+command-name)
    #:once-each
    [("--history") location "Read and update the history" (set-box! history location)])

  (define-namespace-anchor anchor)
  (define history-list (cond ((unbox history) (file->value (unbox history)))
                             (else null)))

  (void (contract (listof string?) history-list (unbox history) 'expeditor))
  
  ;;The main REPL
  (parameterize ((current-expeditor-history history-list))
    (call-with-expeditor
     (lambda (read)
       (let/cc break
         (let loop ()
           (define read-result (read))
           (define eval-result (if (eof-object? read-result) (break) (eval read-result (module->namespace "namespace.rkt" (namespace-anchor->namespace anchor)))))
           (println eval-result)
           (loop)))))
    (cond ((unbox history) (write-to-file (current-expeditor-history) (unbox history) #:exists 'truncate/replace)))))
