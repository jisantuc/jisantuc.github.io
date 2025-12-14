{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Blog.Pandoc (pygmentizingPandocCompiler) where

import Control.Concurrent (threadDelay)
import Data.Functor (void)
import Data.Hashable (hash)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as T
import GHC.IO.Handle (BufferMode (NoBuffering), hSetBuffering)
import Hakyll
import System.IO (hPrint)
import System.Process (readProcess, runInteractiveCommand)
import Text.Pandoc.Definition (Block (CodeBlock, RawBlock), Pandoc)
import Text.Pandoc.Walk (Walkable, walk, walkM)

tshow :: Int -> T.Text
tshow = T.pack . show

pygmentsHighlight :: Pandoc -> Compiler Pandoc
pygmentsHighlight pandoc = unsafeCompiler do
  (hin, _, _, _) <- runInteractiveCommand "python scripts/pygmentize"
  hSetBuffering hin NoBuffering
  void $ (`walkM` pandoc) \case
    cb@(CodeBlock (_, listToMaybe -> mbLang, _) body) ->
      do
        let cod =
              mconcat
                [ "/tmp/" <> tshow (hash body),
                  "\n",
                  fromMaybe "text" mbLang <> "\n",
                  body
                ]
        hPrint hin (T.length cod)
        T.hPutStr hin cod
        pure cb
    block -> pure block
  threadDelay 3.0e6
  (`walkM` pandoc) \case
    CodeBlock _ body ->
      RawBlock "html" <$> T.readFile ("/tmp/" <> show (hash body))
    block -> pure block

pygmentizingPandocCompiler :: Compiler (Item String)
pygmentizingPandocCompiler =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    pygmentsHighlight
