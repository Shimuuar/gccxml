{-# LANGUAGE OverloadedStrings #-}
-- | Extra XML utilities
module Language.Cpp.GccXml.XML where

import Control.Applicative

import Data.XML.Types             (Name)
import Data.Text                  (unpack,words)

--import Text.XML.Enumerator.Parse
import Text.XML.Stream.Parse
import Prelude                    hiding (words)

import Language.Cpp.GccXml.Types



-- | Ignore all tags retained by parser
ignore :: AttrParser a -> AttrParser a
ignore p = p <* ignoreAttrs

-- | Check whether parameter is present
haveParam :: Name -> AttrParser Bool
haveParam nm = True <$ requireAttr nm <|> pure False

-- | Get ID
paramID :: Name -> AttrParser ID
paramID nm = requireAttr nm

-- | Get list of whitespace seprated identifiers
paramIdList :: Name -> AttrParser [ID]
paramIdList nm = do
  xs <- optionalAttr nm
  return $ case xs of 
             Nothing -> []
             Just x  -> words x

-- | Get number
paramNum :: Name -> AttrParser Int
paramNum nm = read . unpack <$> requireAttr nm

-- | Get size in bytes. gcc-xml outputs size in bit
paramSize :: Name -> AttrParser Int
paramSize nm = (`div` 8) <$> paramNum nm

-- | Get access mode
paramAccess :: Name -> AttrParser Access
paramAccess nm = do
  n <- requireAttr nm
  return $ case n of "public"    -> Public
                     "private"   -> Private
                     "protected" -> Protected
                     _           -> error "Bad access"

-- | Get virtuality
paramVirtual :: Name -> AttrParser Virtuality
paramVirtual nm = do
  v <- requireAttr nm
  return $ case v of "0" -> NonVirtual
                     _   -> Virtual

