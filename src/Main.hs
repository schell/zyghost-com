{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Html
import Development.Shake
import Development.Shake.FilePath
import Data.Function (on)
import Data.List (sortBy, nub)
import Data.Yaml
import Data.Char (toLower)
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S
import Control.Monad
import Text.Pandoc
import Text.Pandoc.Readers.Markdown
import System.Exit
import System.Process
import Prelude hiding ((*>))
import Lucid

import Data.Conduit.Binary (sinkFile) -- Exported from the package conduit-extra
import Network.HTTP.Conduit
import qualified Data.Conduit as C
import Control.Monad.Trans.Resource (runResourceT)

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

    let url = PostUrl $ dropExtension path
    return $ Post url pandoc commit Nothing

filterDots :: [FilePath] -> [FilePath]
filterDots = filter (f . takeFileName) 
    where f ('.':_) = False
          f _ = True

makeGetPosts :: (FilePath -> Action Post) -> FilePath -> Rules (() -> Action [Post]) 
makeGetPosts getPost dir = newCache $ \() -> do
    files <- filterDots <$> getDirectoryFiles "" [dir </> "*.*"]
    posts <- mapM getPost files 
    return $ sortBy (flip compare `on` postDate) posts

makeGetArticlePosts :: (FilePath -> Action Post) -> Rules (() -> Action [Post])
makeGetArticlePosts getPost = makeGetPosts getPost "articles"

makeGetSeries :: Rules (() -> Action [Series])
makeGetSeries = newCache $ \() -> do
    seriesExist <- doesFileExist "series.yaml"
    if not seriesExist 
    then return []
    else do file <- B.pack <$> readFile' "series.yaml" 
            case decodeEither' file of
                Left err -> error $ prettyPrintParseException err
                Right series -> return series

downloads :: FilePath
downloads = "build" </> "downloads"

seriesSlug :: Post -> String
seriesSlug = map (toLower . f) . postTitle
    where f ' ' = '-'
          f c = c

seriesDir :: Post -> FilePath
seriesDir = ("series" </>) . seriesSlug

seriesFile :: Series -> FilePath
seriesFile = dropExtension . takeFileName 
                           . articleFile 
                           . seriesIndex

getSeriesIndexPost :: (FilePath -> Action Post) -> Series -> Action Post
getSeriesIndexPost getPost series = do 
    -- Get the index post of the series
    let seriesNdx = seriesIndex series
        seriesLocation = articleLocationToLocal $ articleLocation seriesNdx
        seriesNdxPath = articleFile seriesNdx
        seriesNdxOut = downloads </> seriesNdxPath
    cmd "unzip -u" seriesLocation seriesNdxPath "-d" downloads :: Action CmdLine
    post <- getPost seriesNdxOut

    return $ post{postUrl = PostUrl $ seriesDir post }

getSeriesPosts :: (FilePath -> Action Post) 
               -> (() -> Action [Series]) 
               -> Action [Post]
getSeriesPosts getPost getSeries = do
    serieses <- getSeries () 
    indexes <- mapM (getSeriesIndexPost getPost) serieses
    posts <- forM (zip serieses indexes) $ \(series, index) ->
        -- Get each article post
        forM (seriesArticles series) $ \(Article loc file) -> do
            let local = articleLocationToLocal loc
                temp = downloads </> file
                slug = map toLower $ dropExtension $ takeFileName file 
            cmd "unzip -u" local file "-d" downloads :: Action CmdLine
            post <- getPost temp
            return post{postUrl = PostUrl $ seriesDir index </> slug 
                       ,postSeriesIndex = Just index
                       }
    return $ concat posts

milkShake = shakeArgs shakeOptions{shakeFiles="build/"}


seriesLocationsToLocal :: Series -> [FilePath]
seriesLocationsToLocal (Series ndx as) = loc ndx : map loc as
    where loc = articleLocationToLocal . articleLocation

articleFileToLocal :: Article -> FilePath
articleFileToLocal a = 
        articleLocationToLocal (articleLocation a) </> articleFile a

seriesFiles :: Series -> [FilePath]
seriesFiles (Series ndx as) = loc ndx : map loc as
    where loc = articleFileToLocal 

articleLocationToLocal :: String -> FilePath
articleLocationToLocal = (("build" </> "downloads") </>) . map f
    where f '/' = '^'
          f c = c

localToArticleLocation :: FilePath -> String
localToArticleLocation = map f . takeFileName
    where f '^' = '/'
          f c = c

main :: IO ()
main = newManager tlsManagerSettings >>= \mngr -> milkShake $ do

    phony "clean" $ removeFilesAfter "build/" ["//*"] 

    getPost         <- makeGetPost
    getArticlePosts <- makeGetArticlePosts getPost
    getSeries       <- makeGetSeries

    let needCSS = do css <- map ("build" </>) <$> 
                                getDirectoryFiles "" ["css" </> "*.css"] 
                     need css
        needImgs = do imgs <- map ("build" </>) <$>
                                  getDirectoryFiles "" ["img" </> "*.*"]
                      need imgs
        needRsrcs = needCSS >> needImgs
        needAndGetSeries = do serieses <- getSeries () 
                              mapM_ (need . seriesLocationsToLocal) serieses
                              return serieses

    phony "prune" $ removeFilesAfter downloads ["//*"]
    ----------------------------------------------------------------------------
    -- Build
    ----------------------------------------------------------------------------
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
        need ["build" </> "series" </> "index.html"]
        need ["prune"]
    ----------------------------------------------------------------------------
    -- Build
    ----------------------------------------------------------------------------
    "build" </> "css" </> "*.css" *> \out -> 
        copyFile' (dropDirectory1 out) out
    ----------------------------------------------------------------------------
    -- Build
    ----------------------------------------------------------------------------
    "build" </> "img" </> "*" *> \out -> 
        copyFile' (dropDirectory1 out) out
    ----------------------------------------------------------------------------
    -- Build
    ----------------------------------------------------------------------------
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
    ----------------------------------------------------------------------------
    -- Build
    ----------------------------------------------------------------------------
    "build" </> "downloads" </> "*.zip" *> \out -> do
        let loc = map f $ takeFileName out
            f '^' = '/'
            f c = c
        liftIO $ do 
            putStrLn $ unwords ["Downloading", loc]
            runResourceT $ do 
                req <- parseUrl loc
                response <- http req mngr 
                responseBody response C.$$+- sinkFile out 
    ----------------------------------------------------------------------------
    -- Build the series index file
    ----------------------------------------------------------------------------
    "build" </> "series" </> "index.html" *> \out -> do
        serieses <- getSeries ()
        -- Need the series downloads
        mapM_ (need . seriesLocationsToLocal) serieses 
        posts <- mapM (getSeriesIndexPost getPost) serieses
        -- Need the series indexes
        let indexes = map (("build" </>) . (</> "index.html") . unPostUrl . postUrl) posts
        liftIO $ print indexes
        need indexes
        liftIO $ renderToFile out $ seriesPage $ PostList posts
    ----------------------------------------------------------------------------
    -- Build and individual series index file
    ----------------------------------------------------------------------------
    "build" </> "series" </> "*" </> "index.html" *> \out -> do
        let slug = takeDirectory $ dropDirectory1 $ dropDirectory1 out 
        serieses <- needAndGetSeries 

        -- Need the individual posts in a series
        allSeriesPosts <- getSeriesPosts getPost getSeries 
        let seriesPostSlugs = map fslug allSeriesPosts 
            fslug p = splitDirectories (unPostUrl $ postUrl p) !! 1

        posts <- case filter ((== slug) . fst) $ zip seriesPostSlugs allSeriesPosts of
            [] -> error $ unwords ["Series", slug, "has no posts."] 
            posts -> return $ map snd posts

        let indexes = map (("build" </>) . (</> "index.html") . unPostUrl . postUrl) posts
        need indexes

        indexes <- mapM (getSeriesIndexPost getPost) serieses
        
        -- Get this series' index post
        post <- case filter ((== slug) . seriesSlug) indexes of
            [] -> error $ unwords ["Could not find series",slug]
            post:_ -> return post

        liftIO $ renderToFile out $ seriesIndexPage (PostList posts) post 
    ----------------------------------------------------------------------------
    -- Build a series article
    ----------------------------------------------------------------------------
    "build" </> "series" </> "*" </> "*" </> "index.html" *> \out -> do
        let seriesSlug = splitDirectories out !! 2
            articleSlug = splitDirectories out !! 3 
            url = PostUrl $ "series" </> seriesSlug </> articleSlug
        posts <- getSeriesPosts getPost getSeries
        case filter ((== url) . postUrl) posts of
            [] -> error $ unwords ["Could not find post",show url]
            post:_ -> liftIO $ renderToFile out $ toHtml post
    ----------------------------------------------------------------------------
    -- Build the articles index file
    ----------------------------------------------------------------------------
    "build" </> "articles" </> "index.html" *> \out -> do
        serieses <- needAndGetSeries 

        seriesPosts <- getSeriesPosts getPost getSeries
        articles <- getArticlePosts () 

        let posts = sortBy (flip compare `on` postDate) $ articles ++ seriesPosts
        liftIO $ renderToFile out $ articlesPage $ PostList posts
    ----------------------------------------------------------------------------
    -- Build
    ----------------------------------------------------------------------------
    "build" </> "*.html" *> \out -> do
        needRsrcs
        let path = "content" </> dropDirectory1 (out -<.> "md")
        getPost path >>= liftIO . renderToFile out . toHtml
