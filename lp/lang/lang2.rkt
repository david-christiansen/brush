#lang racket/base
(require "common.rkt")
(require (only-in "../../private/lp.rkt" program PROGRAM))

(provide (except-out (all-from-out "common.rkt")
                     module-begin/plain
                     module-begin/doc)
         program PROGRAM
         (rename-out [module-begin/doc #%module-begin]))
