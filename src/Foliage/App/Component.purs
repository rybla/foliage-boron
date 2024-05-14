module Foliage.App.Component where

import Prelude
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH

component :: forall query input output. H.Component query input output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState input = {}

  eval = H.mkEval H.defaultEval

  render state = HH.div [] [ HH.text "<App.Component>" ]
