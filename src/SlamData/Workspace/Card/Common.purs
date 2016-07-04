{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.Workspace.Card.Common where

import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Deck.Common (DeckOptions)
import SlamData.Workspace.Deck.Component.Cycle (DeckComponent)

type CardOptions =
  { deck ∷ DeckOptions
  , deckComponent ∷ DeckComponent
  , id ∷ CardId
  }
