-- |
--   Module:     Main
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
--
--   Creates the zyghost website generator.
--
{-# LANGUAGE OverloadedStrings  #-}
module Main where

import Hakyll
import Data.Monoid
import System.FilePath

main :: IO ()
main = do
    hakyllWith config $ do
        -- do images
        match "img/*.*" $ do
            route idRoute
            compile copyFileCompiler

        match "templates/*" $ compile templateCompiler

        -- do articles
        match "articles/*" $ do
            route $ setExtension ".html"
            compile $ do
                pandocCompiler
                    >>= return . fmap demoteHeaders
                    >>= loadAndApplyTemplate "templates/default.html" defaultContext
                    >>= relativizeUrls

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
            compile $ do
                pandocCompiler
                    >>= return . fmap demoteHeaders
                    >>= loadAndApplyTemplate "templates/default.html" defaultContext
                    >>= relativizeUrls

config :: Configuration
config = defaultConfiguration { deployCommand = cmd }
    where cmd = "./upload.sh"

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext
