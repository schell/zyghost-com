{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Html where

import Data.Time.Clock
import Data.Maybe (isJust)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)
import Data.Yaml
import Data.Aeson.Types
import Data.List (intercalate, nub)
import Text.Blaze.Renderer.Utf8
import Control.Monad
import Text.Pandoc
import Text.Pandoc.Shared (stringify)
import Prelude hiding ((*>))
import Lucid
import Lucid.Base

newtype PostUrl = PostUrl { unPostUrl :: String } deriving (Show, Eq)

toLink :: PostUrl -> Text
toLink = T.pack . ('/':) . unPostUrl
--------------------------------------------------------------------------------
-- PostList
--------------------------------------------------------------------------------
newtype PostList = PostList [Post]

articlesCommit :: PostList -> String
articlesCommit (PostList ps) = intercalate ", " $ nub $ map postCommit ps

articleListHtml :: Monad m => PostList -> HtmlT m ()
articleListHtml (PostList posts) = ul_ $ forM_ posts $ \post ->
    div_ [class_ "article"] $ do
        div_ [class_ "row"] $ do
            div_ [class_ "col-md-6"] $ do
                case postSeriesIndex post of
                    Nothing -> return ()
                    Just series -> a_ [ class_ "series-link"
                                      , href_ (toLink $ postUrl series)
                                      ] $ toHtml $ postTitle series

                a_ [class_ "article-link", href_ (toLink $ postUrl post)] $
                    toHtml $ postTitle post
            div_ [class_ "article-date col-md-6"] $ toHtml $ postDate post
        div_ [class_ "row"] $
            div_ [class_ "article-desc col-md-12"] $
                toHtml $ postDescription post

instance ToHtml UTCTime where
    toHtml = time_ . toHtml . show
    toHtmlRaw = toHtml

instance ToHtml PostList where
    toHtml = articleListHtml
    toHtmlRaw = toHtml

articlesPage :: Monad m => PostList -> HtmlT m ()
articlesPage list = pageHtml "Articles" (toHtml list) $ articlesCommit list

seriesPage :: Monad m => PostList -> HtmlT m ()
seriesPage list = pageHtml "Series" (toHtml list) $ articlesCommit list

seriesIndexPage :: Monad m => PostList -> Post -> HtmlT m ()
seriesIndexPage list post = pageHtml (postTitle post) html $ postCommit post
    where html = do div_ [class_ "row"] $ toHtml $ postBody post
                    div_ [class_ "row"] $ toHtml list
--------------------------------------------------------------------------------
-- Articles & Series
--------------------------------------------------------------------------------
data Article = Article { articleLocation :: String
                       , articleFile :: FilePath
                       } deriving (Show)

instance FromJSON Article where
    parseJSON (Object v) = Article <$> v .: "location"
                                   <*> v .: "file"
    parseJSON invalid = typeMismatch "Article" invalid

data Series = Series { seriesIndex :: Article
                     , seriesArticles :: [Article]
                     } deriving (Show)

instance FromJSON Series where
    parseJSON (Object v) = Series <$> v .: "article"
                                  <*> v .: "articles"
    parseJSON invalid = typeMismatch "Series" invalid
--------------------------------------------------------------------------------
-- Post
--------------------------------------------------------------------------------
data Post = Post { postUrl :: PostUrl
                 , postBody :: Pandoc
                 , postCommit :: String
                 , postSeriesIndex :: Maybe Post
                 } deriving (Show)

instance ToHtml Post where
    toHtml p@(Post _ pandoc commit series) = pageHtml (postTitle p)
                                                      (f series)
                                                      commit
        where f Nothing = toHtml pandoc
              f (Just post) = do postBlurbHtml post
                                 toHtml pandoc

    toHtmlRaw = toHtml

postBlurbHtml :: Monad m => Post -> HtmlT m ()
postBlurbHtml p =
    blockquote_ [class_ "callout"] $ do
        "This post is part of the "
        a_ [href_ (toLink $ postUrl p)] $ toHtml $ postTitle p
        " series."

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
--------------------------------------------------------------------------------
-- Page Html
--------------------------------------------------------------------------------
pageHtml :: Monad m => String -> HtmlT m () -> String -> HtmlT m ()
pageHtml title body commit = doctypehtml_ $ do
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
            div_ [class_ "content col-md-12"] body

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
--------------------------------------------------------------------------------
-- Pandoc
--------------------------------------------------------------------------------
myPandocExtensions :: S.Set Extension
myPandocExtensions = S.fromList [ Ext_literate_haskell
                                , Ext_link_attributes
                                , Ext_mmd_link_attributes
                                ]

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
