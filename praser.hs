{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative( (<$), (<$>), Applicative(..), (<*), (*>) )

import Data.Maybe
import Data.Text                (Text,unpack)
import Data.Enumerator          (Iteratee)
import Data.XML.Types           (Event,Name)

import Text.XML.Enumerator.Parse


data Access = Public
            | Protected
            | Private
            deriving (Eq,Show)

data Inheritance = NonVirtual
                 | Virtual
                 deriving (Eq,Show)
                          
----------------------------------------------------------------
-- Parse tree
----------------------------------------------------------------

type ID = Int

{-
<ArrayType
<Class
<Constructor
<Converter
<CvQualifiedType
<Destructor
<Enumeration
<Field
<File
<Function
<FunctionType
<FundamentalType
<Method
<Namespace
<OperatorFunction
<OperatorMethod
<PointerType
<ReferenceType
<Struct
<Typedef
<Union
<Variable
-}

-- | Declaration in the header file
data Declaration = 
    Class Text [ID] [(ID, Access, Inheritance)]
    -- ^ Class declaration: name, members, superclasses
  | Function Text (Maybe Text) ID [Maybe ID]
    -- ^ Function: name, mangled name, return type, list of parameters
  | JUNK
    -- ^ Really junk
    deriving (Show)
           

----------------------------------------------------------------


ignore :: AttrParser a -> AttrParser a
ignore p = p <* ignoreAttrs

paramID nm = read . tail . unpack <$> requireAttr nm


paramIdList nm = do
  xs <- optionalAttr nm
  return $ case xs of 
             Nothing -> []
             Just x  -> map (read . tail) . words . unpack $ x

paramAccess nm = do
  n <- requireAttr nm
  return $ case n of "public"    -> Public
                     "private"   -> Private
                     "protected" -> Protected
                     _           -> error "Bad access"

paramVirtual nm = do
  v <- requireAttr nm
  return $ case v of "0" -> NonVirtual
                     _   -> Virtual

----------------------------------------------------------------


declaration :: (Monad m) 
            => Name
            -> AttrParser ([a2] -> a)
            -> Iteratee Event m (Maybe a2)
            -> Iteratee Event m (Maybe (Int, a))
declaration nm atts chld =
  tagName nm (ignore $ (,) <$> paramID "id" <*> atts) $ \(i,f) -> do
    xs <- many chld
    return (i, f xs)

simpleDecl :: Monad m
           => Name 
           -> AttrParser a
           -> Iteratee Event m (Maybe a)
simpleDecl nm att = tagName nm (ignore att) return 


ignoreTags :: Monad m => Iteratee Event m (Maybe ())
ignoreTags = tagPredicate (const True) ignoreAttrs (const $ () <$ many ignoreTags) 

----------------------------------------------------------------

parseClass :: Iteratee Event IO (Maybe (Int, Declaration))
parseClass = 
  declaration "Class" 
  (Class <$> requireAttr "name" <*> paramIdList "members")
  (simpleDecl "Base" $ (,,) <$> paramID "type" <*> paramAccess "access" <*> paramVirtual "virtual")

parseFunction :: Iteratee Event IO (Maybe (Int, Declaration))
parseFunction = 
  declaration "Function" 
  (Function <$> requireAttr "name" <*> optionalAttr "mangled" <*> paramID "returns")
  (choose [ simpleDecl "Argument" $ (Just <$> paramID "type")
          , tagNoAttr "Ellipsis" (return Nothing)
          ])

----------------------------------------------------------------
parseGccXml = tagName "GCC_XML" ignoreAttrs $ const $ many 
            $ choose [ parseClass
                     , parseFunction
                     , fmap (const (0,JUNK)) <$> ignoreTags
                     ]



go :: IO ()
go = do
  q <- parseFile_ "th1.xml" decodeEntities $ force "GCC XML" parseGccXml
  mapM_ print q
