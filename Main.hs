{-# LANGUAGE DeriveDataTypeable #-}

module Main ( main ) where

import           Prelude                      hiding (unwords)

import           Control.Arrow                (second, (&&&), (<<<))

import           Data.Text                    (Text, unpack, unwords)
import           Data.Text.Titlecase          (Titlecase, titlecase)
import           Data.Text.Titlecase.Internal (unTitlecase)

import           System.Console.CmdArgs

import           Text.Blaze                   (toMarkup)
import           Text.Blaze.Renderer.Pretty   (renderMarkup)

data Format = HTML
            | Plain
            deriving (Data, Show)

data Options = Options { format :: Format
                       , input  :: [Text]
                       }
             deriving (Data, Show, Typeable)

main :: IO ()
main = putStr . go =<< cmdArgs options
  where
    go = uncurry render <<<
         second (titlecase . unwords) <<<
         format &&& input

render :: Format -> Titlecase -> String
render HTML  = renderMarkup . toMarkup
render Plain = (++ "\n") . unpack . unTitlecase

options :: Options
options = Options
  { format = HTML &=
    typ "FORMAT" &=
    help "html (default) or plain"
  , input = def &=
    typ "WORDS" &=
    args
  } &=
  help "Convert English words to title case" &=
  helpArg [explicit, name "h", name "help"] &=
  summary "titlecase v0.1.1.3" &=
  details [ "Capitalize all English words except articles (a, an, the),"
          , "coordinating conjunctions (for, and, nor, but, or, yet, so),"
          , "and prepositions (unless they begin or end the title)."
          , ""
          , "The prepositions are taken from"
          , "<https://en.wikipedia.org/wiki/List_of_English_prepositions>."
          ]
