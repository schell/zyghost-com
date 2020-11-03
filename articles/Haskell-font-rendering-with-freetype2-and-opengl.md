---
title: Rendering text in Haskell using Freetype2 and OpenGL
date: 2014-08-01
description: A first attempt at rendering text
toc-title: Table of Contents
theme: templates/default.html
---

Intro
=====
My last adventure in programming was using Haskell to hook up [freetype2](http://freetype.org/freetype2/) to [OpenGL](https://www.opengl.org/). Freetype is a font rasterization library. The idea is that you use freetype to load a font and render it into an opengl texture, then render some font textured geometry in order to display strings of characters on the screen. The idea is simple enough but like many graphics projects there a a couple of gotchas that I bumped up against and dumped a significant amount of time into. I'm including code below to load a character into an OpenGL texture but I'm not including the surrounding code to render that texture. You should be able to use the loaded texture to draw a quad to see the character.

Setup
=====
So what I would like is a function that given a path to a ttf font file, a character and a pixel size - returns an opengl texture object that I can use to render a quad representing the character.

```haskell
loadCharacter :: FilePath -> Char -> Int -> IO TextureObject
loadCharacter path char px = undefined
```

Freetype
========
The first step was to find some freetype2 bindings. Jason Dagit (lispy on #haskell irc) wrote some raw bindings that are on [hackage](http://hackage.haskell.org/package/freetype2). It works quite well and he has also posted an example of [rendering a string of characters as ascii images in a terminal](https://github.com/dagit/freetype2/blob/cabalization/Main.hs). Between that and the [freetype2 tutorial](http://freetype.org/freetype2/docs/tutorial/step1.html) you should be able to get a good idea of the process behind rendering a font glyph into a freetype bitmap.

Helpers
-------
Here we have some convenience functions. The first unboxes an IO FT_Error and fails if the FT_Error is non zero.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
runFreeType :: IO FT_Error -> IO ()
runFreeType m = do
    r <- m
    unless (r == 0) $ fail $ "FreeType Error:" ++ show r
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This code is simple enough. It arises from having to check almost every freetype operation for an error. None the less, I borrowed it from a package on hackage I found by checking the [reverse package dependencies](http://packdeps.haskellers.com/reverse) on the freetype2 bindings and looking at the source of the [defunct free-game package](http://hackage.haskell.org/package/free-game-0.9.4.3/docs/src/Graphics-UI-FreeGame-Data-Font.html#freeType). This is one of the things that I love about Haskell. There are lots of high quality tools, resources and people to help you find the answers you need.

The next helper function allocs a c pointer for a FT_Library handle. This handle is needed for about half of the Freetype calls.

```haskell
freeType :: IO FT_Library
freeType = alloca $ \p -> do
    runFreeType $ ft_Init_FreeType p
    peek p
```

Then we have a helper that given the `FT_Library` and a `FilePath` returns a loaded font face as a `FT_Face`.

```haskell
fontFace :: FT_Library -> FilePath -> IO FT_Face
fontFace ft fp = withCString fp $ \str ->
    alloca $ \ptr -> do
        runFreeType $ ft_New_Face ft str 0 ptr
        peek ptr
```

Lastly this function just gives us a string from a glyph format, FT_Glyph_Format, which we use when we output our glyph info.

```haskell
glyphFormatString :: FT_Glyph_Format -> String
glyphFormatString fmt
    | fmt == ft_GLYPH_FORMAT_COMPOSITE = "ft_GLYPH_FORMAT_COMPOSITE"
    | fmt == ft_GLYPH_FORMAT_OUTLINE = "ft_GLYPH_FORMAT_OUTLINE"
    | fmt == ft_GLYPH_FORMAT_PLOTTER = "ft_GLYPH_FORMAT_PLOTTER"
    | fmt == ft_GLYPH_FORMAT_BITMAP = "ft_GLYPH_FORMAT_BITMAP"
    | otherwise = "ft_GLYPH_FORMAT_NONE"
```

###Loading a Freetype bitmap
Here we have our `loadCharacter` function above - fleshed out for loading a glyph, rendering it into a Freetype bitmap and then printing some info about it to the console.

```haskell
loadCharacter :: FilePath -> Char -> Int -> IO TextureObject
loadCharacter path char px = do
    -- FreeType (http://freetype.org/freetype2/docs/tutorial/step1.html)
    ft <- freeType

    -- Get the Ubuntu Mono fontface.
    ff <- fontFace ft path
    runFreeType $ ft_Set_Pixel_Sizes ff (fromIntegral px) 0

    -- Get the unicode char index.
    chNdx <- ft_Get_Char_Index ff $ fromIntegral $ fromEnum char

    -- Load the glyph into freetype memory.
    runFreeType $ ft_Load_Glyph ff chNdx 0

    -- Get the GlyphSlot.
    slot <- peek $ glyph ff

    -- Number of glyphs
    n <- peek $ num_glyphs ff
    putStrLn $ "glyphs:" ++ show n

    fmt <- peek $ format slot
    putStrLn $ "glyph format:" ++ glyphFormatString fmt

    -- This is [] for Ubuntu Mono, but I'm guessing for bitmap
    -- fonts this would be populated with the different font
    -- sizes.
    putStr "Sizes:"
    numSizes <- peek $ num_fixed_sizes ff
    sizesPtr <- peek $ available_sizes ff
    sizes <- forM [0 .. numSizes-1] $ \i ->
        peek $ sizesPtr `plusPtr` fromIntegral i :: IO BS.FT_Bitmap_Size
    print sizes

    l <- peek $ bitmap_left slot
    t <- peek $ bitmap_top slot
    putStrLn $ concat [ "left:"
                      , show l
                      , "\ntop:"
                      , show t
                      ]

    runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL

    -- Get the char bitmap.
    bmp <- peek $ bitmap slot
    putStrLn $ concat ["width:"
                      , show $ width bmp
                      , " rows:"
                      , show $ rows bmp
                      , " pitch:"
                      , show $ pitch bmp
                      , " num_grays:"
                      , show $ num_grays bmp
                      , " pixel_mode:"
                      , show $ pixel_mode bmp
                      , " palette_mode:"
                      , show $ palette_mode bmp
                      ]
    -- ...continued in the next section...
```

Then the next problem is getting that bitmap into an OpenGL texture.

OpenGL
======

First try
---------
Now that we have our glyph rendering into a freetype bitmap we can take that bitmap and buffer it into an OpenGL texture. The first step is to generate our texture name, activate it, etc - all the normal texture stuff.

```haskell
    -- Generate an opengl texture.
    [tex] <- genObjectNames 1
    texture Texture2D $= Enabled
    activeTexture     $= TextureUnit 0
    textureBinding Texture2D $= Just tex
    printError
```

Next we need buffer the data into the texture. We know from the freetype tutorial that the bitmap buffer is an array of 8bit chars representing a single channel of grayscale levels 0-255. We also already have a pointer to the data with `buffer bmp`. So we can use that info to set up our texImage2D to take the bitmap buffer from freetype directly.

```haskell
    putStrLn "Buffering glyph bitmap into texture."
    texImage2D
        Texture2D
        NoProxy
        0
        R8
        (TextureSize2D w' h')
        0
        (PixelData Red UnsignedByte $ buffer bmp)
    printError

    putStrLn "Texture loaded."
```

Then we need to complete the texture by setting some filter parameters and return the texture name to end our function.

```haskell
    -- Complete the texture by setting some filtering parameters.
    textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

    return tex
```

When you run that code you can either inspect your running OpenGL instance to see the buffered texture or you can draw a quad with it. If you set these filter parameters incorrectly or at the wrong time you'll still be able to see your texture in a profiler but OpenGL will consider it incomplete and will *not* render it. I found out that I was originally running into this problem because I was setting these params *before* I bound and buffered my texture. [This OpenGL wiki entry about incomplete textures helped me out](http://www.opengl.org/wiki/Common_Mistakes#Creating_a_complete_texture). After I figured that out my program spat out something that looked like

![a torn glyph](/img/Screen Shot 2014-01-12 at 12.53.26 PM.png)

If you look closely you can see that there seems to be some noise at the bottom of the texture, which made me think that maybe OpenGL is reading past the end of the freetype buffer and getting some trash input. You can also see that the texture is obviously torn. With my settings of using Ubuntu Mono to render a Z at 251 pixels my loadCharacter function outputs

```bash
glyphs:1296
glyph format:ft_GLYPH_FORMAT_OUTLINE
Sizes:[]
left:0
top:0
width:101 rows:155 pitch:101 num_grays:256 pixel_mode:2 palette_mode:0
Buffering glyph bitmap into texture.
Texture loaded.
```

### Fixing the tearing with padding
I got stuck for a while trying to figure out what was causing the tearing. I thought it may be the pixel format, OpenGL's texture storage or whatever. I ended up just playing with the input to the `loadCharacter` function for a while to see how the tearing changed and eventually figured out that some of them rendered perfectly - like Ubuntu Mono at 270 pixels.

![a perfect glyph](/img/Screen Shot 2014-01-12 at 1.01.58 PM.png)

```bash
glyphs:1296
glyph format:ft_GLYPH_FORMAT_OUTLINE
Sizes:[]
left:0
top:0
width:108 rows:167 pitch:108 num_grays:256 pixel_mode:2 palette_mode:0
Buffering glyph bitmap into texture.
Texture loaded.
```

The only difference I could see in the output info was that the width, rows and pitch were different, which makes sense because the bitmaps are different sizes. What I eventually figured out is that the latter's pitch is a multiple of four while the former's is not. Apparently (and forgive me if I'm wrong) but OpenGL likes texture widths that are divisible by four. So what I did was to change the buffering portion of `loadCharacter` to pad the texture every `width` pixels with some number of blank pixels to make the `width` a multiple of four.

First we need a pure padding function.

```haskell
addPadding :: Int -> Int -> a -> [a] -> [a]
addPadding _ _ _ [] = []
addPadding amt w val xs = a ++ b ++ c
    where a = take w xs
          b = replicate amt val
          c = addPadding amt w val (drop w xs)
```

Then we need to get our bitmap data into Haskell and pad it, then buffer that data into OpenGL.

```haskell
    let w  = fromIntegral $ width bmp
        h  = fromIntegral $ rows bmp
        w' = fromIntegral w :: Integer
        h' = fromIntegral h
        p  = 4 - w `mod` 4
        nw = p + fromIntegral w'

    putStrLn $ "padding by " ++ show p

    -- Get the raw bitmap data.
    bmpData <- peekArray (w*h) $ buffer bmp

    let data' = addPadding p w 0 bmpData

    -- Generate an opengl texture.
    [tex] <- genObjectNames 1
    texture Texture2D $= Enabled
    activeTexture     $= TextureUnit 0
    textureBinding Texture2D $= Just tex
    printError

    putStrLn "Buffering glyph bitmap into texture."
    withArray data' $ \ptr -> texImage2D
        Texture2D
        NoProxy
        0
        R8
        (TextureSize2D (fromIntegral nw) h')
        0
        (PixelData Red UnsignedByte ptr)
    printError

    putStrLn "Texture loaded."
    textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

    return tex
```

Which gives us a perfectly rendered anti-aliased glyph.

![a perfect glyph](/img/Screen Shot 2014-01-12 at 1.01.58 PM.png)

Fixing it with row alignment
----------------------------
Later thanks to reddit I found out that you can reset the row alignment in OpenGL with one call.

```haskell
rowAlignment Unpack $= 1
```

This will change the default unpacking row alignment from 4 to 1 and fix our tearing issue.

Finally
=======
Altogether the code will look something like

```haskell
module Graphics.Text.Font where

import           Control.Monad
import           Graphics.Rendering.OpenGL hiding (bitmap)
import           Graphics.Rendering.OpenGL.GL.PixelRectangles.PixelStorage
import           Graphics.Rendering.FreeType.Internal
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import           Graphics.Rendering.FreeType.Internal.Library
import           Graphics.Rendering.FreeType.Internal.FaceType
import           Graphics.Rendering.FreeType.Internal.Face
import           Graphics.Rendering.FreeType.Internal.GlyphSlot
import           Foreign
import           Foreign.C.String
import           Graphics.Rendering.FreeType.Internal.Bitmap
import           Graphics.Texture.Load
import           Graphics.Utils
import qualified Graphics.Rendering.FreeType.Internal.BitmapSize as BS

loadCharacter :: FilePath -> Char -> Int -> Int -> IO TextureObject
loadCharacter path char px texUnit = do
    -- FreeType (http://freetype.org/freetype2/docs/tutorial/step1.html)
    ft <- freeType

    -- Get the Ubuntu Mono fontface.
    ff <- fontFace ft path
    runFreeType $ ft_Set_Pixel_Sizes ff (fromIntegral px) 0

    -- Get the unicode char index.
    chNdx <- ft_Get_Char_Index ff $ fromIntegral $ fromEnum char

    -- Load the glyph into freetype memory.
    runFreeType $ ft_Load_Glyph ff chNdx 0

    -- Get the GlyphSlot.
    slot <- peek $ glyph ff

    -- Number of glyphs
    n <- peek $ num_glyphs ff
    putStrLn $ "glyphs:" ++ show n

    fmt <- peek $ format slot
    putStrLn $ "glyph format:" ++ glyphFormatString fmt

    -- This is [] for Ubuntu Mono, but I'm guessing for bitmap
    -- fonts this would be populated with the different font
    -- sizes.
    putStr "Sizes:"
    numSizes <- peek $ num_fixed_sizes ff
    sizesPtr <- peek $ available_sizes ff
    sizes <- forM [0 .. numSizes-1] $ \i ->
        peek $ sizesPtr `plusPtr` fromIntegral i :: IO BS.FT_Bitmap_Size
    print sizes

    l <- peek $ bitmap_left slot
    t <- peek $ bitmap_top slot
    putStrLn $ concat [ "left:"
                      , show l
                      , "\ntop:"
                      , show t
                      ]

    runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL

    -- Get the char bitmap.
    bmp <- peek $ bitmap slot
    putStrLn $ concat ["width:"
                      , show $ width bmp
                      , " rows:"
                      , show $ rows bmp
                      , " pitch:"
                      , show $ pitch bmp
                      , " num_grays:"
                      , show $ num_grays bmp
                      , " pixel_mode:"
                      , show $ pixel_mode bmp
                      , " palette_mode:"
                      , show $ palette_mode bmp
                      ]

    let w  = fromIntegral $ width bmp
        h  = fromIntegral $ rows bmp
        w' = fromIntegral w
        h' = fromIntegral h

    -- Set the texture params on our bound texture.
    texture Texture2D $= Enabled

    -- Set the alignment to 1 byte.
    rowAlignment Unpack $= 1

    -- Generate an opengl texture.
    tex <- newBoundTexUnit texUnit
    printError

    putStrLn "Buffering glyph bitmap into texture."
    texImage2D
        Texture2D
        NoProxy
        0
        R8
        (TextureSize2D w' h')
        0
        (PixelData Red UnsignedByte $ buffer bmp)
    printError

    putStrLn "Texture loaded."
    textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

    return tex


addPadding :: Int -> Int -> a -> [a] -> [a]
addPadding _ _ _ [] = []
addPadding amt w val xs = a ++ b ++ c
    where a = take w xs
          b = replicate amt val
          c = addPadding amt w val (drop w xs)


glyphFormatString :: FT_Glyph_Format -> String
glyphFormatString fmt
    | fmt == ft_GLYPH_FORMAT_COMPOSITE = "ft_GLYPH_FORMAT_COMPOSITE"
    | fmt == ft_GLYPH_FORMAT_OUTLINE = "ft_GLYPH_FORMAT_OUTLINE"
    | fmt == ft_GLYPH_FORMAT_PLOTTER = "ft_GLYPH_FORMAT_PLOTTER"
    | fmt == ft_GLYPH_FORMAT_BITMAP = "ft_GLYPH_FORMAT_BITMAP"
    | otherwise = "ft_GLYPH_FORMAT_NONE"


runFreeType :: IO FT_Error -> IO ()
runFreeType m = do
    r <- m
    unless (r == 0) $ fail $ "FreeType Error:" ++ show r


freeType :: IO FT_Library
freeType = alloca $ \p -> do
    runFreeType $ ft_Init_FreeType p
    peek p


fontFace :: FT_Library -> FilePath -> IO FT_Face
fontFace ft fp = withCString fp $ \str ->
    alloca $ \ptr -> do
        runFreeType $ ft_New_Face ft str 0 ptr
        peek ptr
```

The code above contains some other helper functions that I haven't mentioned. You can find them in their respective modules at [my github](https://github.com/schell/editor/tree/glyph-rendering/src/Graphics).

Links I wish I had before I wrote this
======================================
* [Text rendering OpenGL wikibook part 1](http://en.wikibooks.org/wiki/OpenGL_Programming/Modern_OpenGL_Tutorial_Text_Rendering_01)
* [Text rendering OpenGL wikibook part 2](http://en.wikibooks.org/wiki/OpenGL_Programming/Modern_OpenGL_Tutorial_Text_Rendering_02)
* [Rendering to a texture (for making an atlas)](https://developer.apple.com/library/ios/documentation/3ddrawing/conceptual/opengles_programmingguide/WorkingwithEAGLContexts/WorkingwithEAGLContexts.html#//apple_ref/doc/uid/TP40008793-CH103-SW6)
