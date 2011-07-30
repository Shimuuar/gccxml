{-# LANGUAGE OverloadedStrings #-}
-- |
-- Following tag from gcc-xml output are not parsed currently
-- 
-- > Converter
-- > Enumeration
-- > Field
-- > File
-- > FunctionType
-- > OperatorFunction
-- > OperatorMethod
-- > Struct
-- > Union
-- > Variable

import Control.Applicative( (<$), (<$>), Applicative(..), (<|>), (<*), (*>), optional )
import Control.Arrow
import Control.Monad.IO.Class

import Data.Maybe
import Data.Char
import Data.Text                (Text,split,empty,unpack)
import qualified Data.Text as T
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

type ID = Text

{-
-}


data SuperClass = SuperClass {
    superID      :: ID
  , superMode    :: Access
  , superVirtual :: Inheritance
  , superOff     :: Int
  }
  deriving (Show,Eq)

-- | Description of a class
data ClassData = ClassData {
    className    :: Text         -- ^ Name of a class
  , classAlign   :: Int          -- ^ Alignment of a class
  , classSize    :: Maybe Int    -- ^ Size of a class in bytes
  , classMembers :: [ID]         -- ^ ID's of class members
  , classSupers  :: [SuperClass] -- ^ Superclasses
  }
  deriving (Show,Eq)

  
-- | Declaration in the header file
data Declaration = 
    Namespace Text [ID]
    -- ^ Namesapce: name, list of members
  | Class ClassData
    -- ^ Class name, members, superclasses
  | Constructor       Text Access    [Maybe ID]
    -- ^ Constructor: mangled name access mode and list of parameters
  | Destructor        Text Access
    -- ^ Destructor: mangled name, access mode
  | Method       Text Text Access ID [Maybe ID]
    -- ^ Class method: name, mangled name, access mode, return type, argument types
  
  | Function Text (Maybe Text) ID [Maybe ID]
    -- ^ Function name, mangled name, return type, list of parameters
    
    -- Types
  
  | FundametalType Text
    -- ^ Basic C++ type
  | PointerType     ID
    -- ^ Declaration of pointer type
  | ReferenceType   ID
    -- ^ Reference to type 
  | ArrayType       ID Text
    -- ^ Array: type and size in textual form
  | FunctionType ID [Maybe ID]
    -- ^ Pointer to function. Return type and argument type
  | CvQualifiedType ID Bool Bool
    -- ^ Const/volatile qualifier: type
  | Typedef Text ID  
    -- ^ Typedef: typedef name, type it points to
    
  --   | CvQualifiedType ID -- FIXME: const/volatile?
  --     -- ^ Const/volatile marked type
  | JUNK
    -- ^ Really junk
    deriving (Eq,Show)
           

----------------------------------------------------------------


ignore :: AttrParser a -> AttrParser a
ignore p = p <* ignoreAttrs

haveParam :: Name -> AttrParser Bool
haveParam nm = True <$ requireAttr nm <|> pure False

paramID :: Name -> AttrParser ID
paramID nm = requireAttr nm

paramNum :: Name -> AttrParser Int
paramNum nm = read . unpack <$> requireAttr nm

paramSize :: Name -> AttrParser Int
paramSize nm = (`div` 8) <$> paramNum nm

paramIdList :: Name -> AttrParser [ID]
paramIdList nm = do
  xs <- optionalAttr nm
  return $ case xs of 
             Nothing -> []
             Just x  -> filter (not . T.null) . split isSpace $ x

paramAccess :: Name -> AttrParser Access
paramAccess nm = do
  n <- requireAttr nm
  return $ case n of "public"    -> Public
                     "private"   -> Private
                     "protected" -> Protected
                     _           -> error "Bad access"

paramVirtual :: Name -> AttrParser Inheritance
paramVirtual nm = do
  v <- requireAttr nm
  return $ case v of "0" -> NonVirtual
                     _   -> Virtual

----------------------------------------------------------------


declaration :: (Monad m) 
            => Name
            -> AttrParser ([x] -> a)
            -> Iteratee Event m (Maybe x)
            -> Iteratee Event m (Maybe (ID, a))
declaration nm atts chld =
  tagName nm (ignore $ (,) <$> paramID "id" <*> atts) $ \(i,f) -> do
    xs <- many chld
    return (i, f xs)

simpleDecl :: Monad m => Name -> AttrParser a -> Iteratee Event m (Maybe (ID,a))
simpleDecl nm att = subdecl nm ((,) <$> paramID "id" <*> att)

subdecl :: Monad m
        => Name 
        -> AttrParser a
        -> Iteratee Event m (Maybe a)
subdecl nm att = tagName nm (ignore att) return 


ignoreTags :: Monad m => Iteratee Event m (Maybe ())
ignoreTags = tagPredicate (const True) ignoreAttrs (const $ () <$ many ignoreTags) 

----------------------------------------------------------------

parseClass :: MonadIO m => Iteratee Event m (Maybe (ID, Declaration))
parseClass = 
  fmap (second Class) <$> 
  ( declaration "Class" 
    (ClassData <$> requireAttr "name" 
               <*> paramSize   "align" 
               <*> optional (paramSize   "size")
               <*> paramIdList "members")
    (subdecl "Base" $ SuperClass <$> paramID      "type" 
                                 <*> paramAccess  "access" 
                                 <*> paramVirtual "virtual" 
                                 <*> paramSize    "offset")
  )

argumentList :: MonadIO m => Iteratee Event m (Maybe (Maybe ID))
argumentList = choose [ subdecl   "Argument" $ (Just <$> paramID "type")
                      , tagNoAttr "Ellipsis" (return Nothing)
                      ]

parseMethod :: MonadIO m => Iteratee Event m (Maybe (ID, Declaration))
parseMethod = 
  declaration "Method" 
  (Method <$> requireAttr "name" <*> requireAttr "mangled" <*> paramAccess "access" <*> paramID "returns")
  argumentList

parseConstructor :: MonadIO m => Iteratee Event m (Maybe (ID, Declaration))
parseConstructor =
  declaration "Constructor"
  (Constructor <$> requireAttr "mangled" <*> paramAccess "access")
  argumentList

parseFunction :: MonadIO m => Iteratee Event m (Maybe (ID, Declaration))
parseFunction = 
  declaration "Function" 
  (Function <$> requireAttr "name" <*> optionalAttr "mangled" <*> paramID "returns")
  argumentList
  
  
----------------------------------------------------------------
parseGccXml = tagName "GCC_XML" ignoreAttrs $ const $ many 
            $ choose [ simpleDecl "Namespace" (Namespace <$> requireAttr "name" <*> paramIdList "members")
                       -- Declarations
                     , parseClass
                     , parseConstructor
                     , simpleDecl "Destructor" (Destructor <$> requireAttr "mangled" <*> paramAccess "access")
                     , parseMethod
                     , parseFunction
                       -- Types
                     , simpleDecl "FundametalType"  (FundametalType  <$> requireAttr "name")
                     , simpleDecl "PointerType"     (PointerType     <$> paramID     "type")
                     , simpleDecl "ReferenceType"   (ReferenceType   <$> paramID     "type")
                     , simpleDecl "ArrayType"       (ArrayType       <$> paramID     "type" <*> requireAttr "max")
                     , declaration "FunctionType"   (FunctionType    <$> paramID     "returns")
                                                    argumentList 
                     , simpleDecl "CvQualifiedType" (CvQualifiedType <$> paramID "type" <*> haveParam "const" <*> haveParam "volatile")
                     , simpleDecl "Typedef"         (Typedef         <$> requireAttr "name" <*> paramID "type")
                       -- Take all JUNK
                     , fmap (const (empty,JUNK)) <$> ignoreTags
                     ]



-- go :: IO ()
go s = do
  q <- parseFile_ s decodeEntities $ force "GCC XML" parseGccXml
  mapM_ print $ filter ((/= JUNK) . snd) q
