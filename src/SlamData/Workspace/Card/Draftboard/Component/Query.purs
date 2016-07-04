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

module SlamData.Workspace.Card.Draftboard.Component.Query where

import SlamData.Prelude

import Data.Map as Map
import DOM.HTML.Types (HTMLElement)

import Halogen as H
import Halogen.Component.Utils.Drag (DragEvent)
import Halogen.HTML.Events.Types (Event, MouseEvent)

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.Draftboard.Model (DeckPosition)
import SlamData.Workspace.Deck.Component.Nested.Query as DNQ
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.Model (Deck)

data Query a
  = Grabbing DeckId DragEvent a
  | Resizing DeckId DragEvent a
  | SetElement (Maybe HTMLElement) a
  | AddDeck (Event MouseEvent) a
  | LoadDeck DeckId a
  | GetDecks (Map.Map DeckId (DeckPosition × Deck) -> a)

type QueryC = Coproduct CardEvalQuery Query

type QueryP = H.ParentQuery QueryC DNQ.QueryP DeckId
