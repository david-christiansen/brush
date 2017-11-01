#lang racket/base

(provide (except-out (all-from-out racket/base) #%module-begin)
         module-begin/plain
         module-begin/doc)

(require (for-syntax racket/base syntax/strip-context syntax/parse))

(begin-for-syntax
  (define-syntax-class whitespace-string
    (pattern
     s:str #:when (andmap char-whitespace? (string->list (syntax->datum #'s)))))

  (define module-code (box '()))
  (define (add-to-code! exprs)
    (set-box! module-code (cons exprs (unbox module-code)))))


(define-for-syntax (reconstruct orig-stx)
  (define (restore nstx d) (datum->syntax orig-stx d nstx nstx))
  (define (shift nstx) (replace-context orig-stx nstx))
  (strip-comments (restore orig-stx (apply append (reverse (unbox module-code))))))


(define-for-syntax (strip-comments body)
  (cond
    [(syntax? body)
     (define r (strip-comments (syntax-e body)))
     (if (eq? r (syntax-e body))
         body
         (datum->syntax body r body body))]
    [(pair? body)
     (define a (car body))
     (define ad (syntax-e a))
     (cond
       [(and (pair? ad)
             (memq (syntax-e (car ad))
                   '(code:comment
                     code:contract)))
        (strip-comments (cdr body))]
       [(eq? ad 'code:blank)
        (strip-comments (cdr body))]
       [(and (or (eq? ad 'code:hilite)
                 (eq? ad 'code:quote))
             (let* ([d (cdr body)]
                    [dd (if (syntax? d)
                            (syntax-e d)
                            d)])
               (and (pair? dd)
                    (or (null? (cdr dd))
                        (and (syntax? (cdr dd))
                             (null? (syntax-e (cdr dd))))))))
        (define d (cdr body))
        (define r
          (strip-comments (car (if (syntax? d) (syntax-e d) d))))
        (if (eq? ad 'code:quote)
            `(quote ,r)
            r)]
       [(and (pair? ad)
             (eq? (syntax-e (car ad))
                  'code:line))
        (strip-comments (append (cdr ad) (cdr body)))]
       [else (cons (strip-comments a)
                   (strip-comments (cdr body)))])]
    [else body]))
      
(define-for-syntax (extract-chunks exprs)
  (let loop ([exprs exprs])
    (syntax-case exprs ()
      [() (void)]
      [(expr . exprs)
       (syntax-case #'expr (define-syntax quote-syntax)
         [(define-values (lifted) (quote-syntax (a-chunk body ...)))
          (eq? (syntax-e #'a-chunk) 'a-chunk)
          (begin
            (add-to-code! (syntax->list #'(body ...)))
            (loop #'exprs))]
         [_ 
          (loop #'exprs)])])))

(define-for-syntax ((make-module-begin submod?) stx)
  (syntax-parse stx
    [(_ spaces:whitespace-string ...
        ((~alt (~optional (~seq #:program-lang prog-lang)
                          #:name "program language"
                          #:defaults ([prog-lang #'racket]))
               (~optional (~seq #:doc-lang doc-lang)
                          #:name "documentation language"
                          #:defaults ([doc-lang #'scribble/doclang2])))
         ...)
        body0 . body)
     (let ([expanded 
            (expand `(,#'module scribble-lp-tmp-name brush/private/lp
                                ,@(strip-context #'(body0 . body))))])
       (syntax-case expanded ()
         [(module name lang (mb . stuff))
          (begin (extract-chunks #'stuff)
                 (with-syntax ([(form ...) (apply append (reverse (unbox module-code)))])
                   #`(#%module-begin
                      (module program #,(strip-context #'prog-lang)
                        #,@(strip-context (reconstruct #'here)))
                      (require (submod "." program))
                      (provide (all-from-out (submod "." program)))
                      ;; The `doc` submodule allows a `scribble/lp` module
                      ;; to be provided to `scribble`:
                      #,@(if submod?
                             (list
                              (let ([submod
                                     (strip-context
                                      #`(module doc scribble/doclang2
                                          (require scribble/manual
                                                   (only-in brush/private/lp program PROGRAM))
                                          (require (for-label (submod "..")))
                                          ;; Based on https://groups.google.com/forum/#!topic/racket-users/7OrQFTOGBaw
                                          (declare-exporting #:use-sources
                                                             (,(car
                                                                (resolved-module-path-name
                                                                 (variable-reference->resolved-module-path
                                                                  (#%variable-reference))))))
                                          (begin body0 . body)))])
                                (syntax-case submod ()
                                  [(_ . rest)
                                   (datum->syntax submod (cons #'module* #'rest))])))
                             '()))))]))]
    [(m-b body0 . body)
     ((make-module-begin submod?) #'(m-b () body0 . body))]))

(define-syntax module-begin/plain (make-module-begin #f))
(define-syntax module-begin/doc (make-module-begin #t))
