---
title: Reflex SDL2
date: 2017-08-21
description: A reflex host for sdl2 applications.
theme: templates/default.html
---

This past weekend I had a few moments to spend writing a [reflex][0] host for [sdl][1]
applications. I expected the task to be much harder - a testament to the two libraries'
authors! So - standing on the shoulders of giants is a new barebones package for starting
game and multimedia projects (in Haskell) called [reflex-sdl2][2].

The hackage docs contain a link to a [small tutorial][3] that compiles into a desktop app. If
everything works, you should see some 1980's stlye neon colored squares appear whenever
you click or release your mouse :)

Thanks to Ryan Trinkle for helping me brainstorm and letting me bug him about types at
odd hours of the night and for giving this new library an official place to live.

[r/haskell comments](https://www.reddit.com/r/haskell/comments/6v6uyj/reflexsdl2_a_minimal_reflex_frp_host_for_sdl2_apps/)

[0]: http://hackage.haskell.org/package/reflex "reflex frp"
[1]: http://hackage.haskell.org/package/sdl2 "simple direct media layer - part deux"
[2]: http://hackage.haskell.org/package/reflex-sdl2 "sdl2 reflex host"
[3]: https://github.com/reflex-frp/reflex-sdl2/blob/master/app/Main.hs "example"
