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

  (require (submod "error.rkt" error-test)
	   (submod "block.rkt" block-test)
           (submod "vector.rkt" vector-instantiation-test)))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline racket/file racket/contract
           raco/command-name
           (submod expeditor configure) expeditor
           "namespace.rkt" "error.rkt")

  ;;Configuration
  (void (gsl:turn-off-error-handler))
  (define history (box #f))
  (command-line
    #:program (short-program+command-name)
    #:once-each
    [("--history") location "Read and update the history" (set-box! history location)])

  ;;Read and check the history list
  (define history-list (cond ((unbox history) (file->value (unbox history))) (else null)))
  (void (contract (listof string?) history-list (unbox history) 'expeditor))
  
  ;;The main REPL
  (parameterize ((current-expeditor-history history-list))
    (dynamic-wind
      void
      (lambda ()
        ;;You cannot send a break signal to the expeditor through Ctl-C
        (expeditor-bind-key! "^C" ee-reset-entry)
        ;;Save output ports
        (define $output (current-output-port))
        ;;Start the REPL
        (call-with-expeditor
         (lambda (read)
           (define new-tag (make-continuation-prompt-tag))
           (call-with-continuation-prompt
            (lambda ()
              (call/cc
               (lambda (break)
                 (let loop ()
                   ;;The REPL will not abort when something is raised.
                   (call-with-continuation-prompt
                    (lambda ()
                      (define read-result (read))
                      (define eval-result (if (eof-object? read-result) (break (newline $output)) (eval read-result eval-namespace)))
                      (println eval-result $output))
                    (default-continuation-prompt-tag)
                    void)
                   (loop)))
               new-tag))
            new-tag
            void))))
      (lambda () (cond ((unbox history) (write-to-file (current-expeditor-history) (unbox history) #:exists 'truncate/replace)))))))
