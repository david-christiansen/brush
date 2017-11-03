#lang racket/base

(require (for-syntax racket/base syntax/parse)
         scribble/racket scribble/manual scribble/struct)

(begin-for-syntax
  (define-splicing-syntax-class hide-opt
    (pattern (~seq #:hide)
             #:with hide? #t)
    (pattern (~seq)
             #:with hide? #f)))

(define-syntax (define-code stx)
  (syntax-parse stx
    [(define-code chunk-id racketblock)
     #'(define-syntax (chunk-id stx)
         (syntax-parse stx
           [(_ hide:hide-opt expr (... ...))
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
                    #,(if (syntax->datum (attribute hide.hide?))
                          #'(void)
                          #'(racketblock expr (... ...))))))]))]))

(define-code program racketblock)
(define-code PROGRAM RACKETBLOCK)

(define-syntax (chunkref stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (with-syntax ([tag (format "~a:1" (syntax-e #'id))]
                   [str (format "~a" (syntax-e #'id))])
       #'(elemref '(chunk tag) #:underline? #f str))]))


(provide (all-from-out racket/base
                       scribble/manual)
         program PROGRAM)
