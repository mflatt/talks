#lang scribble/manual
@(require (for-label (planet mflatt/princess/princess)
                     (planet mflatt/princess/castle)
                     (planet mflatt/princess/thought)
                     (planet mflatt/princess/things)
                     (planet mflatt/princess/movie)
                     slideshow
                     scheme
                     scheme/gui/base
                     "princess.ss"
                     "castle.ss"
                     "things.ss"
                     "well.ss"))

@title{Princesses for Slideshow}

The @scheme[mflatt/princess] libraries provide picts and constructors
for princesses, thought bubbles, castles, wishing wells, and a few
other items. The @schememodlink[(planet
mflatt/princess/play-movie)]{@scheme[(planet
mflatt/princess/play-movie)]} library is a slideshow program that uses
those elements for a fairy tale, which was the start of an conferenec
talk about Scribble.

@; ----------------------------------------

@section{Princesses}

@defmodule[(planet mflatt/princess/princess)]

@defproc[(make-princess [#:side side (or/c 'front 'right 'left) 'front]
                        [#:rotate angle real? 0.0]
                        [#:dress dress-color (or/c string? (is-a?/c color%)) "pink"]
                        [#:hair hair-color (or/c string? (is-a?/c color%)) "yellow"]
                        [#:pen-width pen-width real? 1]
                        [#:arm-angle arm-angle real? 0.0]
                        [#:leg-angle leg-angle real? 0.0]
                        [#:front-arm-angle front-arm-angle real? 0.0]
                        [#:smile? smile? #t]
                        [#:shake shake real? 0.0]
                        [#:clip-body clip-body (or/c #f (is-a?/c region%)) #f]
                        [#:clip-dx clip-dx real? 0]
                        [#:clip-dy clip-dy real? 0])
         pict?]{

Returns a pict for a princess---either facing forward, to the right,
or to the left, depending on @scheme[side].

Most arguments are self-explanatory, except for

@itemlist[

 @item{@scheme[angle] --- rotates the whole princess picture}

 @item{@scheme[arm-angle] and @scheme[leg-angle] --- for
       @scheme['left] and @scheme['right] mode only, rotates arm and
       leg swing relative to the body.}

 @item{@scheme[front-arm-angle] --- for @scheme['front] mode only,
       rotates arms relative to the body.}

 @item{@scheme[shake] --- for @scheme['front] mode only, an amount to
       rotate the head to ``shake'' it (as if saying ``no''); the
       number is in drawing units, rather than an angle}

 @item{@scheme[clip-body], @scheme[clip-dx], @scheme[clip-dy] --- for
       @scheme['front] mode only, a clipping region to apply to the
       princess's body, but not her neck; the @scheme[clip-dx] and
       @scheme[clip-dy] arguments move the clipping region relative to
       the princess.}

]}

@; ----------------------------------------

@section{Thought Bubbles}

@defmodule[(planet mflatt/princess/thought)]

@defproc[(thought [base pict?]
                  [from pict?]
                  [to pict?]
                  [grow-n (real-in 0.0 1.0)]
                  [gone-n (real-in 0.0 1.0)]
                  [#:wrap-thought wrap-thought (pict? . -> . pict?) values])
         pict?]{

Pins a thought balloon under @scheme[base], where @scheme[from] is the
thinker and @scheme[to] is the thinkee to have a thought balloon under
it.

The @scheme[grow-n] argument can range from @scheme[0.0] to
@scheme[1.0], where @scheme[0.0] is when @scheme[from] is about to
start thinking, @scheme[1.0] is when the thought is fully formed, and
intermediate points have the thought bubble fading in. The
@scheme[gone-n] argument starts at @scheme[0.0] with the thought
intact, and then pops away as @scheme[gone-n] changes to
@scheme[1.0]. (The @scheme[gone-n] value should be @scheme[0.0] if
@scheme[grow-n] is less than @scheme[1.0].)

The @scheme[wrap-thought] procedure can be used to adjust just the
thought-bubble pict before it is pinned under @scheme[base] for te
result.}

@; ----------------------------------------

@section{Castles}

@defmodule[(planet mflatt/princess/castle)]

@defthing[castle pict?]{A pink and purple castle.}

@defthing[window-path (is-a?/c dc-path%)]{Corresponds to the window in
@scheme[castle].}

@; ----------------------------------------

@section{Wishing Wells}

@defmodule[(planet mflatt/princess/well)]

@defthing[well pict?]{A wishing well.}

@defthing[sign pict?]{A sign for a wishing well.}

@defthing[well+sign pict?]{A combination of @scheme[well] and @scheme[sign].}

@defproc[(well+sign* [fade-n real?]) pict?]{

A combination of @scheme[well] and @scheme[sign], using
@scheme[fade-n] to control the opacity of the sign;
@scheme[(well+sign* 1.0)] is the same as @scheme[well+sign].}

@defthing[one-in-sign pict?]{The pict that is the ``1'' in
@scheme[sign], which is useful for finding its relative location.}

@; ----------------------------------------

@section{Things to Wish For}

@defmodule[(planet mflatt/princess/things)]

@defthing[fishbowl pict?]{A fish bowl with a goldfish.}

@defthing[ice-cream pict?]{Three scoops.}

@defthing[diamond pict?]{Princess-cut, of course.}

@; ----------------------------------------

@section{Fairy Tale Movie}

@defmodule[(planet mflatt/princess/movie)]

@defproc[(movie-slides) void?]{

Registers slides to play the Three Princesses movie. The movie pauses
at various points (advance with space bar or arrow) so that the
speaker can control the pace.}

@; ----------------------------------------

@section{Playing the Movie}

@defmodule[(planet mflatt/princess/play-movie)]

Calls @scheme[movie-slides].
