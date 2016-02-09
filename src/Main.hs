{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Development.Shake
import Development.Shake.FilePath
import Data.Function (on)
import Data.Time.Clock
import Data.List (sortBy)
import Data.Maybe (isJust)
import qualified Data.Set as S
import qualified Data.Text as T
import Text.Blaze.Renderer.Utf8
import Control.Monad
import Text.Pandoc
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Shared (stringify)
import System.Exit
import System.Process
import Prelude hiding ((*>))
import Lucid
import Lucid.Base

data Post = Post { postUrl :: String
                 , postBody :: Pandoc
                 , postCommit :: String
                 } deriving (Show)

postMetaString :: String -> Post -> String
postMetaString str post = f $ lookupMeta str meta 
    where (Pandoc meta _) = postBody post
          f (Just v) = stringify v
          f _ = ""

postTitle :: Post -> String
postTitle = postMetaString "title"

postDescription :: Post -> String
postDescription = postMetaString "description" 

postDate :: Post -> String
postDate = postMetaString "date"

newtype ArticleList = ArticleList [Post]

articlesCommit :: ArticleList -> String
articlesCommit (ArticleList (post:_)) = postCommit post
articlesCommit _ = ":)"

pageHtml :: (Monad m, ToHtml a) => String -> a -> String -> HtmlT m ()
pageHtml title body commit = 
    doctypehtml_ $ do 
        head_ $ do
            title_ $ toHtml title
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
            link_ [href_ "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css", rel_ "stylesheet"]
            link_ [href_ "https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css", rel_ "stylesheet"]
            link_ [href_ "http://cdn.jsdelivr.net/font-hack/2.010/css/hack.min.css", rel_ "stylesheet"]
            link_ [href_ "/css/site.css", rel_ "stylesheet"]
        body_ $ do
            div_ [class_ "container"] $ do
              div_ [class_ "row page-header"] $ do
                div_ [class_ "col-md-3"] $ do
                  h1_ $ a_ [href_ "/"] "Zyghost"
                  a_ [href_ "/articles/"] "articles"

                  span_ [class_ "space"] ""

                  a_ [class_ "social", href_ "https://github.com/schell"] $ i_ [class_ "fa fa-github"] ""
                  a_ [class_ "social", href_ "https://twitter.com/schellsan"] $ i_ [class_ "fa fa-twitter"] ""
                  a_ [class_ "social", href_ "https://instagram.com/schellsan/"] $ i_ [class_ "fa fa-instagram"] ""
                  a_ [class_ "social", href_ "https://www.facebook.com/likeaseashell"] $ i_ [class_ "fa fa-facebook"] ""
                div_ [class_ "col-md-7"] $
                    h1_ $ toHtml title

              div_ [class_ "row"] $
                div_ [class_ "content col-md-12"] $ 
                    toHtml body 

              div_ [class_ "row footer"] $
                div_ [class_ "content col-md-12"] $
                  span_ [class_ "commit"] $
                    toHtml commit 
            script_ $ T.concat 
              [ "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){"
              , "(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),"
              , "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)"
              , "})(window,document,'script','//www.google-analytics.com/analytics.js','ga');"
              , "ga('create', 'UA-47072737-1', 'auto');"
              , "ga('send', 'pageview');"
              ] 
    

instance ToHtml Post where
    toHtml p@(Post _ pandoc commit) = pageHtml (postTitle p) pandoc commit
    toHtmlRaw = toHtml

myPandocExtensions :: S.Set Extension
myPandocExtensions = S.fromList [Ext_literate_haskell, Ext_link_attributes, Ext_mmd_link_attributes]  

pandocHtml :: Monad m => Pandoc -> HtmlT m ()
pandocHtml pandoc@(Pandoc meta _) = HtmlT $ do
    let hasTOC = isJust $ lookupMeta "has-toc" meta
        blazehtml = writeHtml opts pandoc 
        opts = if hasTOC then optsTOC else optsPlain
        optsPlain = def{ writerExtensions = S.union myPandocExtensions $ 
                            writerExtensions def  
                       , writerHighlight = True
                       , writerHtml5 = True
                       }
        optsTOC = optsPlain { writerTableOfContents = True
                            , writerTemplate = tocTmpl
                            , writerStandalone = True
                            }
        tocTmpl = unwords ["<div class=\"col-md-3\" id=\"toc\">"
                          ,"<h2>Table of Contents</h2>"
                          ,"$toc$"
                          ,"</div>"
                          ,"<div class=\"col-md-9\">$body$</div>"
                          ]
    return (const $ renderMarkupBuilder blazehtml, ())

instance ToHtml Pandoc where
    toHtml = pandocHtml
    toHtmlRaw = toHtml

articleListHtml :: Monad m => ArticleList -> HtmlT m ()
articleListHtml (ArticleList posts) = ul_ $ forM_ posts $ \post -> 
    div_ [class_ "article"] $ do
        div_ [class_ "row"] $ do
            div_ [class_ "col-md-6"] $
                a_ [class_ "article-link", href_ (T.pack $ '/':postUrl post)] $ 
                    toHtml $ "\"" ++ postTitle post ++ "\""
            div_ [class_ "article-date col-md-6"] $ toHtml $ postDate post
        div_ [class_ "row"] $
            div_ [class_ "article-desc col-md-12"] $ 
                toHtml $ postDescription post

instance ToHtml UTCTime where
    toHtml = time_ . toHtml . show
    toHtmlRaw = toHtml

instance ToHtml ArticleList where
    toHtml = articleListHtml 
    toHtmlRaw = toHtml

articlesPage :: Monad m => ArticleList -> HtmlT m ()
articlesPage list = pageHtml "Articles" list $ articlesCommit list 

makeGetPost :: Rules (FilePath -> Action Post)
makeGetPost = newCache $ \path -> do
    let reader = readMarkdownWithWarnings myOpts
        myOpts = def{ readerExtensions = S.union myPandocExtensions $ 
                          readerExtensions def 
                    , readerSmart = True
                    }
    ePandoc     <- reader <$> readFile' path
    (pandoc,ws) <- case ePandoc of
        Left err -> liftIO $ do print err
                                exitFailure
        Right p -> return p

    commit <- liftIO $ readProcess "git" ["log", "--pretty=format:%h", "-n", "1"] ""
    unless (null ws) $ liftIO $ putStrLn $ unlines $ "WARNINGS: ":ws

    let url = dropExtension path
    return $ Post url pandoc commit 

filterDots :: [FilePath] -> [FilePath]
filterDots = filter (f . takeFileName) 
    where f ('.':_) = False
          f _ = True

makeGetPosts :: FilePath -> Rules (() -> Action [Post]) 
makeGetPosts dir = do
    getPost <- makeGetPost
    newCache $ \() -> do
        files <- filterDots <$> getDirectoryFiles "" [dir </> "*.*"]
        posts <- mapM getPost files 
        return $ sortBy (flip compare `on` postDate) posts

makeGetArticles :: Rules (() -> Action [Post])
makeGetArticles = makeGetPosts "articles"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build/"} $ do

    phony "clean" $ removeFilesAfter "build/" ["//*"] 

    getPost     <- makeGetPost
    getArticles <- makeGetArticles

    let needCSS = do css <- map ("build" </>) <$> 
                                getDirectoryFiles "" ["css" </> "*.css"] 
                     need css
        needImgs = do imgs <- map ("build" </>) <$>
                                  getDirectoryFiles "" ["img" </> "*.*"]
                      need imgs
        needRsrcs = needCSS >> needImgs

    phony "build" $ do
        content <- map ((-<.> "html") . ("build" </>) . dropDirectory1) . filterDots <$> 
                    getDirectoryFiles "" ["content" </> "*.*"]
        need content 

        let articlePath = ("build" </>) 
                        . ("articles" </>) 
                        . (</> "index.html")
                        . dropExtension 
                        . takeFileName
        articles <- (map articlePath . filterDots) <$> 
                        getDirectoryFiles "" ["articles" </> "*.*"]
        need articles
        need ["build" </> "articles" </> "index.html"]

    "build" </> "css" </> "*.css" *> \out -> 
        copyFile' (dropDirectory1 out) out

    "build" </> "img" </> "*" *> \out -> 
        copyFile' (dropDirectory1 out) out

    "build" </> "articles" </> "*" </> "index.html" *> \out -> do
        needRsrcs
        let mdpath = ("articles" </>)
                   . (`addExtension` "md")
                   . dropDirectory1
                   . dropDirectory1
                   . takeDirectory
                   $ out
            hspath = mdpath -<.> "lhs"
        ismd <- doesFileExist mdpath
        let path = if ismd then mdpath else hspath
        getPost path >>= liftIO . renderToFile out . toHtml

    "build" </> "articles" </> "index.html" *> \out ->
        getArticles () >>= liftIO . renderToFile out . articlesPage . ArticleList 

    "build" </> "*.html" *> \out -> do
        needRsrcs
        let path = "content" </> dropDirectory1 (out -<.> "md")
        getPost path >>= liftIO . renderToFile out . toHtml
