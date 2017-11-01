#lang scribble/manual

@title{Brush: Literate Programming Without Tangling}


@(require (for-label brush))

Brush is a language for doing semi-literate programming in the style of
Literate Haskell, rather than full Knuth-style literate
programming. For that, see @racketmodname[scribble/lp2].

A Brush module is written in Scribble, with program fragments written
in any Racket language that can be typeset using
@racket[racketblock]. In practice, this is almost any language with an
S-expression syntax.



@section{Reference}

@defmodulelang[brush]{
The @racketmodname[brush] language.

The first form in a @racketmodname[brush] module may be an option list of the form
@racket[(#:program-lang LANG)]. In this case, @racketid[LANG] is used as the language
for program fragments.

The resulting module has two submodules: @racketid[doc], which
contains the Scribble document, and @racketid[program], which contains
the program. All exports of @racketid[program] are then re-provided by
the containing module. This means that, when requiring a Brush
document as Racket, it can be required directly, while including it as
a section in a Scribble document involves including the @racketid[doc]
submodule.
}

@defform*[((program form ...)
           (program #:hide form ...))]{
  Introduces a program fragment that is part of the resulting Racket
  module. The @racket[form]s are typeset using @racket[racketblock].

  If @racket[#:hide] is specified, then the Racket fragment is included in the Racket
  module, but not in the Scribble output.

  Like @racket[chunk] from @racketmodname[scribble/lp2], it is
  possible to escape from @racket[program] using @racket[unsyntax]
  without affecting the resulting Racket program.  Unlike
  @racket[chunk], @racket[program] fragments are not named. They occur
  in the resulting Racket module in the order in which they are
  written.
}

@defform*[((PROGRAM form ...)
           (PROGRAM #:hide form ...))]{
  Just like @racket[program], except @racket[UNSYNTAX] is used to escape to Scribble.
}
