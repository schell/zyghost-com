# Mogwai 0.6

> 2023/01/09 - Released `mogwai-v0.6.0` and `mogwai-dom-v0.1.0`

I spent some time over the holidays preparing v0.6.0 of [`mogwai`][mogwai] and v0.1.0 of [`mogwai-dom`][mogwai-dom], which are libraries for writing user interface elements in a declarative style using streams and sinks.
I have been reluctant in the past to advertise my progress on these projects as the API was very experimental but I think it's settling down now as they approach 1.0.

## [mogwai][mogwai]

`mogwai` is a library that helps define user interface elements using streams and sinks.
It is platform agnostic in that it is not tied to any UI implementation.
It only provides primitives for **constructing the blueprints** of a widget, ie what it is initially, what streams it has as input and what event sinks it has as output.
The main exported type of `mogwai` is [`ViewBuilder`][viewbuilder].

Here are some design points:

* Platform agnostic - `ViewBuilder` definitions can be converted into a platform-specific UI implementation by third party libraries.
  Currently only `mogwai-dom` exists publicly as one of these third-party libs but I plan on writing `mogwai-tui` and I do have a private library `mogwai-pxy` which does this for my own custom 3d game/app engine `pxy` which itself is quite a lot like `bevy`.
* Provides convenient macros like `rsx!` and `html!` to build widgets.
* Async widget tasks can easily access the raw platform-specific UI view as needed, so you can do what you need to do even if `mogwai` hasn't thought of it.

## [mogwai-dom][mogwai-dom]

`mogwai-dom` is a library that provides a `TryFrom<ViewBuilder>` implementation for `JsDom`, which is a thin wrapper around `JsValue` `web-sys` crate, allowing the user to create browser DOM widgets that can access the browser's Javascript APIs.

Here are some design points:

* There is no VDOM, streams patch the DOM directly and asynchronously.
  This means there is no diff-phase, which cuts down some of the CPU overhead.
* Easily access the raw `web-sys` types through `JsDom`
* Provides a non-browser DOM node type `SsrDom` to build DOM server-side for rendering.
* Provides a target agnostic DOM node type `Dom` that is `JsDom` on `wasm32` and `SsrDom` otherwise, as well as hydration from existing DOM for writing "isomorphic" apps.

## Ecosystem

The `mogwai-*` libraries are not fully fledged frameworks like `dioxus` and `yew` etc, but they do exist in the same space.
`mogwai` is more like a layer in your frontend stack.
It doesn't come with a lot of batteries, but it also doesn't come with much dogma.
It's quite a "barebones" wrapper around whatever the underlying UI platform is, but I feel it gets you 80% of the way there for 20% of the effort/lock-in.

## Links

* [tutorials/cookbook](https://zyghost.com/guides/mogwai-cookbook/intro.html)
* [github repo][repo]
* [mogwai crates.io][mogwai]
* [mogwai-dom crates.io][mogwai-dom]
* [mogwai-macros crates.io][mogwai-macros] (re-exported by both `mogwai` and `mogwai-dom`)

## Thank you

Thanks for taking the time to read this!
Please let me know here or [at the repo][repo] (or on any of the other channels you may find me) if you have any concerns, bugs, feature requests, etc.

😃☕😃☕😃☕

[mogwai]: https://crates.io/crates/mogwai
[mogwai-dom]: https://crates.io/crates/mogwai-dom
[mogwai-macros]: https://crates.io/crates/mogwai-macros
[viewbuilder]: https://docs.rs/mogwai/latest/mogwai/view/struct.ViewBuilder.html
[repo]: https://github.com/schell/mogwai
