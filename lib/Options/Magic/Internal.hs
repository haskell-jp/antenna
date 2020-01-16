{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}

module Options.Magic.Internal where

import           Data.Extensible

class Magic a kv where
  magic :: proxy kv -> TargetOf kv -> a
