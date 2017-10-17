#lang scheme/base

(require (for-syntax scheme/base syntax/boundmap)
         scribble/scheme scribble/decode scribble/manual scribble/struct)

(begin-for-syntax
  ;; maps chunk identifiers to a counter, so we can distinguish multiple uses
  ;; of the same name
  (define chunk-numbers (make-free-identifier-mapping))
  (define (get-chunk-number id)
    (free-identifier-mapping-get chunk-numbers id (lambda () #f)))
  (define (inc-chunk-number id)
    (free-identifier-mapping-put! chunk-numbers id (+ 1 (free-identifier-mapping-get chunk-numbers id))))
  (define (init-chunk-number id)
    (free-identifier-mapping-put! chunk-numbers id 2)))

(define-syntax-rule (define-code chunk-id racketblock)
  (define-syntax (chunk-id stx)
    (syntax-case stx ()
      [(_ expr (... ...))
       (begin
         (syntax-local-lift-expression #'(quote-syntax (a-chunk expr (... ...))))
         (with-syntax ([((for-label-mod (... ...)) (... ...))
                        (map (lambda (expr)
                               (syntax-case expr (require)
                                 [(require mod (... ...))
                                  (let loop ([mods (syntax->list #'(mod (... ...)))])
                                    (cond
                                      [(null? mods) null]
                                      [else 
                                       (syntax-case (car mods) (for-syntax)
                                         [(for-syntax x (... ...))
                                          (append (loop (syntax->list #'(x (... ...))))
                                                  (loop (cdr mods)))]
                                         [x
                                          (cons #'x (loop (cdr mods)))])]))]
                                 [else null]))
                             (syntax->list #'(expr (... ...))))])
           #`(begin
               (require (for-label for-label-mod (... ...) (... ...)))
               (racketblock expr (... ...)))))])))

(define-code program racketblock)
(define-code PROGRAM RACKETBLOCK)

(define-syntax (chunkref stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (with-syntax ([tag (format "~a:1" (syntax-e #'id))]
                   [str (format "~a" (syntax-e #'id))])
       #'(elemref '(chunk tag) #:underline? #f str))]))


(provide (all-from-out scheme/base
                       scribble/manual)
         program PROGRAM)
