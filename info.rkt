#lang info

(define collection "brush")

(define deps (list "base" "scribble"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/brush.scrbl" ())))
(define pkg-desc "Literate programming without tangles")
(define version "0.5")
