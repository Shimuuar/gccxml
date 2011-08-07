{-# LANGUAGE MultiParamTypeClasses #-}
module Foreign.Cpp.Call (
    Object
  ) where

import Foreign.Ptr

-- | Opaque representation of heap allocated C++ object. 
newtype Object a = Object (Ptr a)

-- | @a@ could be safely casted to @b@. Sadly because of multiple
-- inheritance and virtual/non-virtual pairs there is no better
-- implementation than set of all pairs.
class Cast a b where
  castObj :: Object a -> Object b

-- | Constructible object
class Constructor obj args where
  new :: args -> IO (Object obj)

-- | Object's destructor.
class Destructor obj where
  delete :: Object obj -> IO ()w

newtype Member method args = Member args

-- | Call function
class Call obj method ret args where
  (~>) :: Object obj -> Member method args -> IO return
