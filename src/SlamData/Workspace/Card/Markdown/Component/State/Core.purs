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

module SlamData.Workspace.Card.Markdown.Component.State.Core where

import Control.Monad.Eff.Class (class MonadEff)

import SlamData.Prelude
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.Markdown.Interpret as MDI

import Data.BrowserFeatures (BrowserFeatures)
import Data.Date.Locale as DL
import Data.StrMap as SM

import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Halogen.Component as SDH

type State =
  { browserFeatures ∷ Maybe BrowserFeatures
  , input ∷ Maybe (SD.SlamDownP VM.VarMapValue)
  }

initialState ∷ State
initialState =
  { browserFeatures: Nothing
  , input: Nothing
  }

formStateToVarMap
  ∷ ∀ m e
  . (MonadEff (locale ∷ DL.Locale | e) m, Applicative m)
  ⇒ SDH.SlamDownFormState VM.VarMapValue
  → SDH.SlamDownFormState VM.VarMapValue
  → m VM.VarMap
formStateToVarMap desc st =
  SM.foldM
    (\m k field → do
       v ← valueForKey k field
       pure $ SM.insert k v m)
    SM.empty
    desc

  where
    valueForKey
      ∷ String
      → SDH.FormFieldValue VM.VarMapValue
      → m VM.VarMapValue
    valueForKey k field =
      fromMaybe (MDI.formFieldEmptyValue field) <$>
        case SM.lookup k st of
          Just v → MDI.formFieldValueToVarMapValue v
          Nothing → MDI.formFieldValueToVarMapValue field
