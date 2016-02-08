-- |
--   Module:     Main
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
--
--   Creates the zyghost website generator.
--
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Hakyll
import Data.Monoid
import Control.Monad (liftM)
import System.FilePath
import System.Process
import Text.Pandoc.Options
import Text.HTML.TagSoup

config :: Configuration
config = defaultConfiguration

context :: String -> Context String
context git = constField "commit" git <> defaultContext

postCtx :: String -> Context String
postCtx git = dateField "date" "%B %e, %Y" <> context git

build :: String -> IO ()
build git = hakyllWith config $ do
    match "templates/*" $ compile templateCompiler

    -- do images
    match "img/*.*" $ do
        route idRoute
        compile copyFileCompiler

    -- do articles
    match "articles/*" $ do
        route $ setExtension ".html"
        compile $ do
            ident  <- getUnderlying
            toc <- getMetadataField ident "toc"
            let myWriterOptions = if toc == Just "yes" 
                                  then tocOptions 
                                  else defaultHakyllWriterOptions 
                tocTmpl = unwords ["<div class=\"col-md-3\" id=\"toc\">"
                                  ,"<h2>Table of Contents</h2>"
                                  ,"$toc$"
                                  ,"</div>"
                                  ,"<div class=\"col-md-9\">$body$</div>"
                                  ]
                tocOptions = defaultHakyllWriterOptions
                                 { writerTableOfContents = True
                                 , writerTemplate = tocTmpl
                                 , writerStandalone = True
                                 }

            item <- pandocCompilerWith defaultHakyllReaderOptions myWriterOptions 
            let item' = fmap demoteHeaders item 
                bootstrapTheImgs = return . fmap (withTags bootstrapTheImg)
                bootstrapTheImg t@(TagOpen "img" atts) = TagOpen "img" $ atts ++ [("class", fromAttrib "class" t ++ " img-responsive")]
                bootstrapTheImg x = x
            page <- loadAndApplyTemplate "templates/default.html" 
                                         (context git)
                                         item' 
            relativizeUrls page >>= bootstrapTheImgs

    create ["articles.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "articles/*"
            let ctx = listField "posts" (postCtx git) (return posts) <>
                      constField "title" "Articles"  <>
                      context git
            makeItem ""
                >>= loadAndApplyTemplate "templates/articles.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "content/*" $ do
        route $ customRoute $ takeFileName . flip replaceExtension ".html" . toFilePath
        compile $ liftM (fmap demoteHeaders) pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" (context git)
            >>= relativizeUrls

main :: IO ()
main = readProcess "git" ["describe","--dirty"] "" >>= build
