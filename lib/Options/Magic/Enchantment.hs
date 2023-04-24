{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Options.Magic.Enchantment where

import           RIO

import           Data.Extensible
import           Options.Magic.Internal (Magic (..))

newtype Enchantment a = Enchantment { enchantment :: a -> a }

withEnchantment ::
  forall a xs . Forall (Magic (Enchantment a)) xs
  => a -> Record xs -> a
withEnchantment =
  hfoldlWithIndexWith @ (Magic (Enchantment a))
    (\m acc x -> enchantment (magic m (runIdentity $ getField x)) acc)

enchantmentIfTrue ::
  (a -> a) -> Bool -> Enchantment a
enchantmentIfTrue _ False = Enchantment id
enchantmentIfTrue e True  = Enchantment e
