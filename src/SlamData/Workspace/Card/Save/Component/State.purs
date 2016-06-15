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

module SlamData.Workspace.Card.Save.Component.State where

import Data.Lens (LensP, lens)
import Data.Maybe (Maybe(..))
import Data.Path.Pathy as Path

type State =
  { pathString ∷ Maybe String
  , confirmedPath ∷ Maybe (Path.AbsFile Path.Sandboxed)
  }

initialState ∷ State
initialState =
  { pathString: Nothing
  , confirmedPath: Nothing
  }

_pathString ∷ ∀ a r. LensP {pathString ∷ a|r} a
_pathString = lens _.pathString (_{pathString = _})

_confirmedPath ∷ ∀ a r. LensP {confirmedPath ∷ a|r} a
_confirmedPath = lens _.confirmedPath (_{confirmedPath = _})
