module Utils (simpleName) where

import Text.XML.Light.Types

simpleName :: String -> QName
simpleName s = QName s Nothing Nothing
