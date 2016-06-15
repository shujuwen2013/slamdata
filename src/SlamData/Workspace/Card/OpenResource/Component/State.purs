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

module SlamData.Workspace.Card.OpenResource.Component.State
  ( State
  , initialState
  , _items
  , _browsing
  , _selected
  , _levelOfDetails
  ) where

import SlamData.Prelude

import Data.Lens (LensP, lens)
import Data.Path.Pathy (rootDir)

import SlamData.FileSystem.Resource as R
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

import Utils.Path as Up

type State =
  { items ∷ Array R.Resource
  , browsing ∷ Up.DirPath
  , selected ∷ Maybe Up.FilePath
  , levelOfDetails ∷ LevelOfDetails
  }

initialState ∷ State
initialState =
  { items: [ ]
  , browsing: rootDir
  , selected: Nothing
  , levelOfDetails: High
  }


_items ∷ ∀ a r. LensP { items ∷ a |r} a
_items = lens (_.items) (_{items = _})

_browsing ∷ ∀ a r. LensP { browsing ∷ a |r} a
_browsing = lens (_.browsing) (_{browsing = _})

_selected ∷ ∀ a r. LensP { selected ∷ a|r} a
_selected = lens (_.selected) (_{selected = _})

_levelOfDetails ∷ ∀ a r. LensP {levelOfDetails ∷ a|r} a
_levelOfDetails = lens (_.levelOfDetails) (_{levelOfDetails = _})
