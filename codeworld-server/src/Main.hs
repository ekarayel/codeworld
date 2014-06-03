{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import qualified Crypto.Hash.MD5 as Crypto
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64
import           Data.Char
import           Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Snap.Core
import           Snap.Http.Server (quickHttpServe)
import           Snap.Util.FileServe
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    route [
      ("compile", compileHandler)
    ] <|>
    dir "user" (serveDirectory "user") <|>
    serveDirectory "web"

compileHandler :: Snap ()
compileHandler = do
    body <- LB.toStrict <$> readRequestBody maxSourceSize
    let hashed = hash body
    alreadyExists <- liftIO $ (&&) <$> (not <$> doesFileExist (targetFile hashed))
                                   <*> (not <$> doesFileExist (errorFile  hashed))
    when alreadyExists $ liftIO $ do
        B.writeFile (sourceFile hashed) body
        compileUserSource hashed
    hasSource <- liftIO $ doesFileExist (targetFile hashed)
    when (not hasSource) $ modifyResponse $ setResponseCode 500
    writeBS hashed

maxSourceSize :: Int64
maxSourceSize = 2000000

hash :: ByteString -> ByteString
hash = BC.cons 'P' . BC.map toWebSafe . B64.encode . Crypto.hash
  where toWebSafe '/' = '_'
        toWebSafe '+' = '-'
        toWebSafe c   = c

localSourceFile :: ByteString -> FilePath
localSourceFile hashed = BC.unpack hashed ++ ".hs"

sourceFile :: ByteString -> FilePath
sourceFile hashed = "user" </> localSourceFile hashed

targetFile :: ByteString -> FilePath
targetFile hashed = "user" </> BC.unpack hashed ++ ".jsexe" </> "all.js"

errorFile :: ByteString -> FilePath
errorFile hashed = "user" </> BC.unpack hashed ++ ".err.txt"

compileUserSource :: ByteString -> IO ()
compileUserSource hashed = do
    let ghcjsArgs = [
            "-O2",
            "-hide-package", "base",
            "-package", "codeworld-base",
            "-XRebindableSyntax",
            "-XImplicitPrelude",
            "-XOverloadedStrings",
            "-XNoTemplateHaskell",
            "-XNoUndecidableInstances",
            "-XNoQuasiQuotes",
            "-XExplicitForAll",
            "-XJavaScriptFFI",
            "./" ++ localSourceFile hashed
          ]
    result <- runCompiler "ghcjs" ghcjsArgs
    B.writeFile (errorFile hashed) $
        sanitizeError (localSourceFile hashed) result

runCompiler :: FilePath -> [String] -> IO ByteString
runCompiler cmd args = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ cwd       = Just "user",
                                       std_in    = CreatePipe,
                                       std_out   = CreatePipe,
                                       std_err   = CreatePipe,
                                       close_fds = True }

    hClose inh

    err <- B.hGetContents errh
    evaluate (B.length err)

    waitForProcess pid

    hClose outh
    return err

sanitizeError :: FilePath -> ByteString -> ByteString
sanitizeError source = T.encodeUtf8
                     . T.replace (T.pack source) "your program"
                     . T.replace "IO action main" "variable main"
                     . T.replace "in module Main" "in the program"
                     . T.replace "[GHC.Types.Char] -> " ""
                     . T.replace "base:GHC.Base.String -> " ""
                     . T.replace "IO ()" "Program"
                     . T.replace "Perhaps you intended to use TemplateHaskell" ""
                     . T.filter isAscii
                     . T.decodeUtf8
