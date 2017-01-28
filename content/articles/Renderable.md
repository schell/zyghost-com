---
title: Better Renderable Types
date: 2015-10-03
description: Purely functional datatypes that can be rendered.
toc-title: Table of Contents
theme: templates/default.html
---
I've been working on a purely functional GUI and I realized that the structures
I've been using for rendering could be easily abstracted out into a
[library](http://hackage.haskell.org/package/renderable). The types may seem
complex but the ideas are simple enough.

The main idea behind `renderable` is that all graphics can be broken down into
primitives.

Rendering
=========
A rendering is simply an effectful value that draws something on the screen in
a specific place.  Also needed is an effectful value that releases any
resources allocated when creating the rendering. Since both values are created
at the same time from here on out a "rendering" will be a [tuple of the two](http://hackage.haskell.org/package/renderable-0.1.0.0/docs/Data-Renderable.html#t:Rendering). So let's dive into what we'll
be rendering.

Primitive
=========
First off we have the typeclass [Primitive](http://hackage.haskell.org/package/renderable-0.1.0.0/docs/Data-Renderable.html#t:Primitive). A primitive is an atomic unit of "graphics". In my current project I've
chosen to render boxes, polylines and text. Each of these are a primitive that
I'll use in different combinations to create my interface. `Primitive` has three
associated types - a monad, a transform type and a resource type. The monad
represents the context of the primitive rendering calls themselves and in most
cases will be the IO monad. If you're using OpenGL you'll probably use IO. The
transform represents the kind of transformations you will apply to your
primitives.  I'm using a two dimensional affine transformation but you can use
anything. It just represents how a rendering can be changed without having to
alter the underlying resources.

Lastly the resource type is whatever datatype holds the resources needed to
render primitives. This may be a record that holds shaders or references to
windows, fonts, etc. For my current project I'm using a `Rez`

```haskell
data Rez = Rez { rezGeom      :: GeomRenderSource
               , rezBez       :: BezRenderSource
               , rezMask      :: MaskRenderSource
               , rezWindow    :: Window
               , rezFont      :: Font
               , rezIcons     :: Font
               } deriving (Typeable)
```

Primitives must have Hashable instances - this is so they can be cached after
being allocated. If you're using OpenGL like I am then 'allocation' means making
some IO calls in order to send geometry and other data to the GPU. The
[compilePrimitive](http://hackage.haskell.org/package/renderable-0.1.0.0/docs/Data-Renderable.html#v:compilePrimitive)
function is where we run the initial IO calls to allocate resources for the
datatype's rendering and then return a tuple of the cleanup function and the
draw function. Since all Primitive instances are also instances of Hashable the
`renderable` package will automatically look up any needed renderings in the
cache, create new ones and release stale ones without you having to think about
it.

Here are some `Primitive` instances to give you an example - they use another
(very) experimental project of mine called [gelatin](http://hackage.haskell.org/package/gelatin),
which at this point is a thin wrapper around [gl](http://hackage.haskell.org/package/gl)
that provides some very specific things I need for my programs

```haskell
--------------------------------------------------------------------------------
-- Unit for fun
--------------------------------------------------------------------------------
instance Primitive () where
    type PrimM () = IO
    type PrimR  () = Rez
    type PrimT  () = Transform
    compilePrimitive _ _ = return (return (), const $ return ())
--------------------------------------------------------------------------------
-- Polyline
--------------------------------------------------------------------------------
instance Primitive Polyline where
    type PrimM Polyline = IO
    type PrimR  Polyline = Rez
    type PrimT  Polyline = Transform
    compilePrimitive (Rez geom _ _ win _ _) Polyline{..} = do
        let fill = solid polylineColor
            p = polyline EndCapSquare LineJoinMiter polylineWidth polylinePath
        Rendering f c <- filledTriangleRendering win geom p fill
        return (c, f)

instance Hashable Polyline where
    hashWithSalt s Polyline{..} =
        s `hashWithSalt` polylineWidth
            `hashWithSalt` polylineColor
                `hashWithSalt` polylinePath

data Polyline = Polyline { polylineWidth     :: Float
                         , polylineColor     :: Color
                         , polylinePath      :: [V2 Float]
                         } deriving (Show, Eq, Typeable, Generic)

path2Polyline :: Float -> Color -> Path -> Polyline
path2Polyline = Polyline
--------------------------------------------------------------------------------
-- Box
--------------------------------------------------------------------------------
boxPath :: Box -> Path
boxPath Box{..} = poly
    where poly = [V2 x1 y1, V2 x2 y1, V2 x2 y2, V2 x1 y2, V2 x1 y1]
          (V2 w h) = boxSize
          x1 = 0
          x2 = w
          y1 = 0
          y2 = h

boxPolyline :: Float -> Box -> Polyline
boxPolyline lw Box{..} = Polyline lw boxColor path
    where path = [V2 x1 y1, V2 x2 y1, V2 x2 y2, V2 x1 y2, V2 x1 y1]
          (V2 w h) = boxSize
          x1 = -hw
          x2 = w + hw
          y1 = -hw
          y2 = h + hw
          hw = lw/2

data Box = Box { boxSize      :: Size
               , boxColor     :: Color
               } deriving (Show, Eq, Typeable, Generic)

instance Hashable Box where
    hashWithSalt s (Box sz c) = s `hashWithSalt` sz `hashWithSalt` c

instance Primitive Box where
    type PrimM Box = IO
    type PrimR Box  = Rez
    type PrimT Box  = Transform
    compilePrimitive (Rez geom _ _ win _ _) (Box (V2 w h) c) = do
        let [tl, tr, br, bl] = [zero, V2 w 0, V2 w h, V2 0 h]
            vs = [tl, tr, br, tl, br, bl]
            cs = replicate 6 c
        Rendering f c' <- colorRendering win geom GL_TRIANGLES vs cs
        return (c',f)
--------------------------------------------------------------------------------
-- PlainText
--------------------------------------------------------------------------------
instance Primitive PlainText where
    type PrimM PlainText = IO
    type PrimR PlainText = Rez
    type PrimT PlainText = Transform
    compilePrimitive (Rez geom bz _ win font _) (PlainText str fc) = do
        Rendering f c <- stringRendering win geom bz font str fc (0,0)
        return (c,f)

instance Hashable PlainText where
    hashWithSalt s PlainText{..} =
        s `hashWithSalt` plainTxtString `hashWithSalt` plainTxtColor

data PlainText = PlainText { plainTxtString :: String
                           , plainTxtColor  :: Color
                           } deriving (Show, Eq, Generic)
```

Composite
=========
[The next step up in abstraction](http://hackage.haskell.org/package/renderable-0.1.0.0/docs/Data-Renderable.html#t:Composite) applies when you have described some adequate number of primitive types.
From here on up you can graphically represent new types as a heterogeneous list
of those more primitive types. [Element](http://hackage.haskell.org/package/renderable-0.0.0.2/docs/Data-Renderable.html#t:Element)
is used to package those primitive types in a list. `composite` simply takes
your type and "decomposes" it into transformed Primitive elements.

This is where making new renderings gets really easy

```haskell
--------------------------------------------------------------------------------
-- TextInput
--------------------------------------------------------------------------------
data TextInput = TextInput { textInputTransform :: Transform
                           , textInputText      :: PlainText
                           , textInputBox       :: Box
                           , textInputActive    :: Bool
                           } deriving (Show, Eq, Typeable)

localTextInputPath :: TextInput -> Path
localTextInputPath = boxPath . textInputBox

globalTextInputPath :: TextInput -> Path
globalTextInputPath t@TextInput{..} =
    transformPoly textInputTransform $ localTextInputPath t

textInputOutline :: TextInput -> Polyline
textInputOutline t@TextInput{..} = path2Polyline 1 white $ localTextInputPath t

instance Composite TextInput IO Rez Transform where
    composite txt@TextInput{..} =
        [ (textInputTransform, Element textInputBox)
        , (textInputTransform, Element textInputText)
        ] ++ [(textInputTransform, Element poly) | textInputActive]
            where poly = textInputOutline txt
```

Rendering a frame
=================
After you have some datatypes to render from primitives it's dead simple to
get them on the screen. All your loop has to keep around is the current data
to render and the last rendering cache. Then you can use [renderData](http://hackage.haskell.org/package/renderable-0.1.0.0/docs/Data-Renderable.html#v:renderData) to render your data to the screen.
Here is an example of the function I'm using to render to the screen. There's
only three relevant lines and the rest is GLFW noise

```haskell
renderFrame :: Workspace -> UI -> IO Workspace
renderFrame ws ui = do
        -- Get the Rez (resource type)
    let rz  = wsRez ws
        -- Get the rendering cache from last
        old = wsCache ws

    (fbw,fbh) <- getFramebufferSize $ rezWindow rz
    glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
    glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

    new <- renderData rz old ui

    pollEvents
    swapBuffers $ rezWindow rz
    shouldClose <- windowShouldClose $ rezWindow rz
    if shouldClose
    then exitSuccess
    else threadDelay 100

    return $ ws { wsCache = new }
```

As you can see [renderData](http://hackage.haskell.org/package/renderable-0.1.0.0/docs/src/Data.Renderable.html#renderData) pulls out the renderings we need from the old cache,
creates the new ones, cleans the stale ones, renders your data and returns your
new cache that you can use to render the next frame. This way if your interface
never changes you don't have to allocate any new resources - you shouldn't even
have to think about it.

All done
========
This is all a work in progress but I wanted to get these packages out to
hopefully get some feedback before I settle on an API (I've already broken it
once)! My next article will be about another project called [gooey](http://hackage.haskell.org/package/gooey).
It's a monadic layer on top of a previous FRP library I wrote called [varying](http://hackage.haskell.org/package/varying).
I just realized how funny the `gooey` lib looks on hackage

![the gooey package](/img/Screenshot 2015-10-03 20.42.39.png)

lol
