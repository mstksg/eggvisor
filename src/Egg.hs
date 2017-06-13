{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Egg (
  module E
  ) where

import           Egg.Action    as E
import           Egg.Commodity as E
import           Egg.Egg       as E
import           Egg.Farm      as E
import           Egg.GameData  as E
import           Egg.Habitat   as E
import           Egg.Vehicle   as E
