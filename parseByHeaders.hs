{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell #-}

import Prelude hiding ((.))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Text
import qualified System.Environment as SE
import qualified Data.Data as D
import Data.Monoid ((<>))
import Control.Applicative ((<*), (<$>), (<*>), (<$))
import Data.Tree
import qualified Data.Tree.Zipper as TZ
import Control.Lens
import Control.Category ((.))
import Control.Arrow ((***), first, second)
import Data.Maybe (fromJust)


main :: IO ()
main =  TIO.interact process

type HeaderName = T.Text
type Section = [T.Text]
type HeaderNumber = Int

process :: T.Text -> T.Text
process = T.concat . T.lines

data Header = Header {_name :: HeaderName,
                      _idx :: HeaderNumber}
              deriving (Show)

data HeaderSection = HeaderSection {_header :: Header,
                                    _section :: Section} deriving (Show)

makeLenses ''Header
makeLenses ''HeaderSection

type Document = [HeaderSection]

parseHeader :: Parser Header
parseHeader = do
  equals <- many1 (char '=')
  headerName <- many1 $ noneOf "="
  string equals
  newline
  return Header {_name = T.pack headerName, _idx = length equals}

bodyLine :: Parser T.Text
bodyLine = do
  firstChar <- noneOf "="
  rest <- many1 (noneOf "\n")
  newline
  return $ T.pack (firstChar : rest)


parseSection :: Parser HeaderSection
parseSection = HeaderSection <$> parseHeader <*> many1 bodyLine
  
parser :: Parser Document
parser = many1 parseSection <* notFollowedBy anyToken


peek = head
push = (:)
pop = tail


buildTree :: Document -> Tree HeaderSection
buildTree (d:ds) = TZ.toTree $ snd $ foldr step start (reverse ds)
  where
    start = ([d], TZ.fromTree $ Node d [])
    num h = view (header . idx) h
    step x z = let i = num x
                   j = num $ peek $ fst z
               in if i > j then
                    push x *** TZ.insert (Node x []) . TZ.children $ z
                  else
                    let origLen = length $ fst $ z
                        newStack = dropWhile ((>i) . num) $ fst z
                        droppedElemsCnt = origLen - length newStack
                    in (newStack,
                        fromJust $ upAndInsert droppedElemsCnt (snd z) x)
                        

iterateM n f x = if n == 0 then return x else f x >>= iterateM (n - 1) f

upAndInsert n z x = iterateM n TZ.parent z >>= return . insertBelow (Node x [])

insertBelow t = TZ.insert t . TZ.children
