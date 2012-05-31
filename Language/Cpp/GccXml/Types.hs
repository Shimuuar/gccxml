{-# LANGUAGE DeriveDataTypeable #-}
-- | Types for gcc-xml parser
module Language.Cpp.GccXml.Types (
    ID
  , Access(..)
  , Virtuality(..)
  ) where

import Data.Data (Data,Typeable)
import Data.Text (Text)


-- | Identifier used in the XML
type ID = Text

-- | Access to the member
data Access = Public
            | Protected
            | Private
            deriving (Eq,Show,Data,Typeable)

-- | Virtual/non-virtual. Used for method annotation and inheritance
-- mode
data Virtuality = NonVirtual
                | Virtual
                deriving (Eq,Show,Data,Typeable)
