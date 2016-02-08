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
import System.FilePath
import Text.Pandoc.Options
import Text.HTML.TagSoup

main :: IO ()
main = hakyllWith config $ do
    -- do images
    match "img/*.*" $ do
        route idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateCompiler

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
                                         defaultContext 
                                         item' 
            relativizeUrls page >>= bootstrapTheImgs

    create ["articles.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "articles/*"
            let ctx = listField "posts" postCtx (return posts) <>
                      constField "title" "Articles"  <>
                      defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/articles.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "content/*" $ do
        route $ customRoute $ takeFileName . flip replaceExtension ".html" . toFilePath
        compile $ pandocCompiler
            >>= return . fmap demoteHeaders
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

config :: Configuration
config = defaultConfiguration { deployCommand = cmd }
    where cmd = "./upload.sh"

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext
