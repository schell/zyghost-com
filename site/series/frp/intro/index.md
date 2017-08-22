---
title: A gentle introduction to FRP
theme: templates/default.html
---

## Discovery

[FRP has been around for a long time](http://conal.net/papers/icfp97/). Originally discovered and coined by Conal Elliot
and Paul Hudak in the late 90's it represented a new approach to writing dynamic systems, wherein the programmer describes
how the system changes over time using continuous semantics. What this meant practically is that FRP code tended to look
more like interelated equations and less like a list of steps to achieve a goal.

As time went on more implementations were made and the term got thrown around freely, eventually even being applied to
seemingly unrelated technologies like Javascript's React framework. Thankfully when folks talk about React, bacon.js,
etc. they tend to drop the "functional" part of FRP in favor of "reactive", which suits the nature of those frameworks 
better. [Check out the wikipedia article on FRP for more info on what it is, specifically](https://en.wikipedia.org/wiki/Functional_reactive_programming)

## Major tenets and their requirements

An FRP system deals in terms of continuous values and discrete events. Continuous values are often referred to as `Behavior`s
while discrete events are simply ... you guessed it ... `Event`s! What this means is that your values always come 
wrapped in these contexts. For this reason I've been known to call writing FRP code as "advanced burrito making". 
The newly popular [reflex](http://hackage.haskell.org/package/reflex) package adds another context - one that 
combines `Behavior`s and `Event`s, called `Dynamic`s. Reflex's brand of FRP is "very advanced burrito making" ;) 

In any implementation there are lots of functions for turning a value in one context into a value in 
another context ... so if you are going to write in FRP I would recommend being really, really familiar with the basic 
typeclasses:

* [Functor](https://wiki.haskell.org/Typeclassopedia#Functor) 
* [Applicative](https://wiki.haskell.org/Typeclassopedia#Applicative)
* [Monad](https://wiki.haskell.org/Typeclassopedia#Monad)
* [Monad transformers](https://wiki.haskell.org/Typeclassopedia#Monad_transformers)

and if you've got the time it would really help to get these bad boys in as well -

* [MonadFix](https://wiki.haskell.org/Typeclassopedia#MonadFix)
* [Arrow](https://wiki.haskell.org/Typeclassopedia#Arrow) (but don't get too carried away with Arrows, lol)

[The haskell wiki has more info on how FRP libs use these typeclasses.](https://wiki.haskell.org/Functional_Reactive_Programming)

## A tour of FRP flavors
Evan Czaplicki of Elm fame did a great talk on the different flavors of FRP, and he does a better job describing them
than I could - plus as my "esteemed president" might put it "television is faster":

<iframe width="560" 
        height="315" 
        src="https://www.youtube.com/embed/Agu6jipKfYw" 
        frameborder="0" 
        allowfullscreen></iframe>

## Wrap up that FRP-rrito
Hopefully that gives a little background to the situation. FRP has come a long way since 1997 but it's really still
an area of active research.
Stay tuned for the next installment where we'll look at some Haskell code.        
![advanced burrito making](/imgs/frp-burrito.png "really very advanced burrito making")
