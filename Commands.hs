module Commands (probe) where

import Text.XML.Light
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U

import Utils

uuid :: IO Element
uuid = do
  u <- U.nextRandom
  return $ Element (simpleName "Uuid") [] [Text $ CData CDataText (U.toString u) Nothing] Nothing

types :: Element
types = Element (simpleName "Types") [] [Text $ CData CDataText "inquiry" Nothing] Nothing

probe :: IO String
probe = do
  uuidElm <- uuid
  return $ showTopElement $ Element (simpleName "Probe") [] [Elem uuidElm, Elem types] Nothing
