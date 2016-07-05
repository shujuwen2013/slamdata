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

module SlamData.Workspace.Card.Viz.Component (vizComponent) where

import SlamData.Prelude

import Control.Monad.Eff.Exception (Error)

import Data.Argonaut (JArray, JCursor)
import Data.Array (length, null, cons, index)
import Data.Int as Int
import Data.Lens as Lens
import Data.Lens ((.~), (^?))
import Data.List as L
import Data.Map as M
import Data.Set as Set

import CSS.Geometry (marginBottom)
import CSS.Size (px)

import Halogen as H
import Halogen.CustomProps as Cp
import Halogen.HTML.CSS.Indexed as CSS
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.Form.Select (Select, autoSelect, newSelect, (⊝), ifSelected, trySelect', _value)
import SlamData.Quasar.Query as Quasar
import SlamData.Render.Common (row)
import SlamData.Render.CSS as Rc
import SlamData.Workspace.Card.CardType (CardType(Viz))
import SlamData.Workspace.Card.Chart.Aggregation (aggregationSelect)
import SlamData.Workspace.Card.Chart.Axis (analyzeJArray, Axis)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration, depends, dependsOnArr)
import SlamData.Workspace.Card.Chart.ChartType (ChartType(..), isPie, isArea)
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as P
import SlamData.Workspace.Card.Viz.Component.Query (QueryC, Query(..))
import SlamData.Workspace.Card.Viz.Component.State as VCS
import SlamData.Workspace.Card.Viz.Form.Component (formComponent)
import SlamData.Workspace.Card.Viz.Form.Component as Form
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type VizHTML = H.ParentHTML Form.StateP QueryC Form.QueryP Slam ChartType
type VizDSL = H.ParentDSL VCS.State Form.StateP QueryC Form.QueryP Slam ChartType

-- | How does this module work?
-- | + Take a TaggedResource case of Port
-- | + Check that resource exists and if so analyze sample to extract Map from
-- |   JCursors to Axes otherwise leave it Map.empty and set available chart types
-- |   to Set.empty. Following steps don't do anything useful if sample is empty.
-- |   Localized in first part of EvalCard handler and updateForm function.
-- | + Load all resource and if it's not too big produce output port
-- |   Second part of EvalCard handler
-- |
-- | + Output port is response of currently active subcomponent which is `Form`
-- | + Form's state (ChartConfiguration) is record with `dimensions`,
-- |   `aggregations`, `series` and `measures` fields.
-- |   These fields are arrays of `Select JCursor`  where `Select α`
-- |   Is model of html combobox with maybe selected α and list of α choices.
-- | + Form can render any kind of `ChartConfiguration` and has weird logic
-- |   for doing this :)
-- |
-- | + Peeking form signals and running CardEval call `configure` func
-- |   (second through `updateForms`)
-- | + `configure` takes all subcomponent configuration and filters them
-- |   e.g. it removes already selected in first measure combobox value from
-- |   available to select choices of second measure combobox -->
-- |   nonsense output is forbidden (one shouldn't be able to make chart
-- |   from `foo` to `foo` groupped by `foo`)
-- | + After filtering (That's important!) previoiusly selected values must
-- |   be set with func `setPreviousValueFrom`.
-- | + Then we have updated config and set it back as subcomponent state.
-- |
-- | About `needToUpdate`
-- | This flag is set when we need to update `records` and `sample` fields.
-- | Basically it's true after parent in deck has changed its output.
-- | And it's false when we just re`configure`d subcomponents.
-- |
-- |   >>> TODO: update this note, or fix the code to restore the old needToUpdate
-- |       logic, if needed. -js
-- |
-- | cryogenian 04/29/2016


vizComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
vizComponent = CC.makeCardComponent
  { cardType: Viz
  , component: H.parentComponent { render, eval, peek: Just peek }
  , initialState: H.parentState VCS.initialState
  , _State: CC._VizState
  , _Query: CC.makeQueryPrism' CC._VizQuery
  }

render ∷ VCS.State → VizHTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD B.glyphiconPicture left state.levelOfDetails
    ]

renderHighLOD ∷ VCS.State → VizHTML
renderHighLOD state =
    HH.div
      [ HP.classes
          $ [ Rc.cardInput, HH.className "card-input-maximum-lod" ]
          ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
      ]
      [ renderEmpty $ not Set.isEmpty state.availableChartTypes
      , renderForm state
      ]

renderEmpty ∷ Boolean → VizHTML
renderEmpty hidden =
  HH.div
    [ HP.classes
        $ [ B.alert, B.alertDanger ]
        ⊕ (guard hidden $> B.hide)
    , CSS.style $ marginBottom $ px 12.0
    ]
    [ HH.text "There is no available chart for this dataset" ]

renderForm ∷ VCS.State → VizHTML
renderForm state =
  HH.div
    [ HP.classes
        $ [ Rc.vizCardEditor ]
        ⊕ (guard hidden $> B.hide)
    ]
    [ renderChartTypeSelector state
    , renderChartConfiguration state
    ]
  where
  hidden ∷ Boolean
  hidden = Set.isEmpty state.availableChartTypes

renderChartTypeSelector ∷ VCS.State → VizHTML
renderChartTypeSelector state =
  HH.div
    [ HP.classes [ Rc.vizChartTypeSelector ] ]
    $ foldl (foldFn state.chartType) empty state.availableChartTypes
  where
  foldFn ∷ ChartType → Array VizHTML → ChartType → Array VizHTML
  foldFn selected accum current =
    flip cons accum $
      HH.img
        [ HP.src $ src current
        , HP.classes
            $ [ cls state.chartType ]
            ⊕ (guard (selected ≡ current) $> B.active)
        , HE.onClick (HE.input_ (right ∘ SetChartType current))
        ]

  src ∷ ChartType → String
  src Pie = "img/pie.svg"
  src Line = "img/line.svg"
  src Bar = "img/bar.svg"
  src Area = "img/area.svg"

  cls ∷ ChartType → HH.ClassName
  cls Pie = Rc.pieChartIcon
  cls Line = Rc.lineChartIcon
  cls Bar = Rc.barChartIcon
  cls Area = Rc.areaChartIcon

renderChartConfiguration ∷ VCS.State → VizHTML
renderChartConfiguration state =
  HH.div
    [ HP.classes [ Rc.vizChartConfiguration ] ]
    [ renderTab Pie
    , renderTab Line
    , renderTab Bar
    , renderTab Area
    , renderDimensions state
    ]
  where
  renderTab ∷ ChartType → VizHTML
  renderTab ty =
    showIf (state.chartType ≡ ty)
    [ HH.slot ty \_ →
        { component: formComponent
        , initialState: H.parentState Form.initialState
        }
    ]

  showIf ∷ Boolean → Array VizHTML → VizHTML
  showIf ok content = HH.div [ HP.classes $ (guard (not ok) $> B.hide) ] content


renderDimensions ∷ VCS.State → VizHTML
renderDimensions state =
  row
  [ intChartInput Rc.axisLabelParam "Axis label angle"
      (_.axisLabelAngle ⋙ show) RotateAxisLabel (isPie state.chartType)
  , intChartInput Rc.axisLabelParam "Axis font size"
      (_.axisLabelFontSize ⋙ show) SetAxisFontSize (isPie state.chartType)
  , boolChartInput Rc.chartDetailParam "If stack"
      (_.areaStacked ⋙ show) SetStacked (not $ isArea state.chartType)
  , boolChartInput Rc.chartDetailParam "If smooth"
      (_.smooth ⋙ show) SetSmooth (not $ isArea state.chartType)
  ]
  where
  intChartInput
    ∷ HH.ClassName
    → String
    → (VCS.State → String)
    → (Int → Unit → Query Unit)
    → Boolean → VizHTML
  intChartInput cls labelText valueFromState queryCtor isHidden =
    HH.form
      [ HP.classes
          $ [ B.colXs6, cls ]
          ⊕ (guard isHidden $> B.hide)
      , Cp.nonSubmit
      ]
      [ label labelText
      , HH.input
          [ HP.classes [ B.formControl ]
          , HP.value $ valueFromState state
          , ARIA.label labelText
          , HE.onValueInput
              $ pure ∘ map (right ∘ flip queryCtor unit) ∘ stringToInt
          ]
      ]
  
  boolChartInput
    ∷ HH.ClassName
    → String
    → (VCS.State → String)
    → (Boolean → Unit → Query Unit)
    → Boolean → VizHTML
  boolChartInput cls labelText valueFromState queryCtor isHidden =
    HH.form
      [ HP.classes
          $ [ B.colXs6, cls ]
          ⊕ (guard isHidden $> B.hide)
      , Cp.nonSubmit
      ]
      [ label labelText
      , HH.input
          [ HP.inputType HP.InputCheckbox
          , HP.checked $ stringToBool $ valueFromState state
          , HP.checked true
          , ARIA.label labelText
          , HE.onChecked
              $ pure ∘ map (right ∘ flip queryCtor unit) ∘ boolToBool
          ]
      ]

  label ∷ String → VizHTML
  label str = HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text str ]

  showIfNeqZero ∷ ∀ a. (Eq a, Show a, Semiring a) ⇒ a → String
  showIfNeqZero a = if zero ≡ a then "" else show a

  stringToInt ∷ String → Maybe Int
  stringToInt s = if s ≡ "" then Just 0 else Int.fromString s

  boolToBool ∷ Boolean → Maybe Boolean
  boolToBool s = Just s

  stringToBool ∷ String → Boolean
  stringToBool s = if s ≡ "true" then true 
                   else false

-- Note: need to put running to state
eval ∷ QueryC ~> VizDSL
eval = coproduct cardEval vizEval

vizEval ∷ Query ~> VizDSL
vizEval q = do
  next <- case q of
    SetChartType ct n → H.modify (VCS._chartType .~ ct) $> n
    RotateAxisLabel angle n → H.modify (VCS._axisLabelAngle .~ angle) $> n
    SetAxisFontSize size n → H.modify (VCS._axisLabelFontSize .~ size) $> n
    SetStacked stacked n → H.modify (VCS._areaStacked .~ stacked) $> n
    SetSmooth smooth n → H.modify (VCS._smooth .~ smooth) $> n
  configure
  CC.raiseUpdatedP' CC.EvalModelUpdate
  pure next

cardEval ∷ CC.CardEvalQuery ~> VizDSL
cardEval = case _ of
  CC.EvalCard info output next → do
    for (output ^? Lens._Just ∘ P._ChartOptions) \opts → do
      sample ← either (const []) id <$>
        H.fromAff (Quasar.sample opts.resource 0 20 :: Slam (Either Error JArray))
      if null sample
        then H.modify (VCS._availableChartTypes .~ Set.empty)
        else H.modify (VCS._sample .~ analyzeJArray sample) *> configure
    pure next
  CC.Save k → do
    st ← H.get
    config ← H.query st.chartType $ left $ H.request Form.GetConfiguration
    pure ∘ k $ Card.Viz
      { chartConfig: fromMaybe Form.initialState config
      , options:
          { chartType: st.chartType
          , axisLabelFontSize: st.axisLabelFontSize
          , axisLabelAngle: st.axisLabelAngle
          , areaStacked: st.areaStacked
          , smooth: st.smooth
          }
      }
  CC.Load card next → do
    case card of
      Card.Viz model → do
        let st = VCS.fromModel model
        H.set st
        H.query st.chartType
          $ left
          $ H.action $ Form.SetConfiguration model.chartConfig
        pure unit
      _ → pure unit
    pure next
  CC.SetDimensions dims next → do
    H.modify
      $ VCS._levelOfDetails
      .~ if dims.width < 576.0 ∨ dims.height < 416.0
           then Low
           else High
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

type AxisAccum =
  { category ∷ Array JCursor
  , value ∷ Array JCursor
  , time ∷ Array JCursor
  }

configure ∷ VizDSL Unit
configure = void do
  axises ← getAxises
  pieConf ← getOrInitial Pie
  setConfFor Pie $ pieBarConfiguration axises pieConf
  lineConf ← getOrInitial Line
  setConfFor Line $ lineConfiguration axises lineConf
  barConf ← getOrInitial Bar
  setConfFor Bar $ pieBarConfiguration axises barConf
  areaConf ← getOrInitial Area
  setConfFor Area $ areaConfiguration axises areaConf
  let chartTypes = available axises
  H.modify (VCS._availableChartTypes .~ available axises)
  case Set.toList chartTypes of
    L.Cons ct L.Nil → H.modify (VCS._chartType .~ ct)
    _ → pure unit
  where
  getOrInitial ∷ ChartType → VizDSL ChartConfiguration
  getOrInitial ty =
    map (fromMaybe Form.initialState)
      $ H.query ty
      $ left (H.request Form.GetConfiguration)

  setConfFor ∷ ChartType → ChartConfiguration → VizDSL Unit
  setConfFor ty conf =
    void $ H.query ty $ left $ H.action $ Form.SetConfiguration conf

  available ∷ AxisAccum → Set.Set ChartType
  available axises =
    foldMap Set.singleton
    $ if null axises.value
      then []
      else if not $ null axises.category
           then [Pie, Bar, Line, Area]
           else if (null axises.time) && (length axises.value < 2)
                then []
                else [Line]

  getAxises ∷ VizDSL AxisAccum
  getAxises = do
    sample ← H.gets _.sample
    pure $ foldl axisFolder {category: [], value: [], time: [] } $ M.toList sample

  axisFolder ∷ AxisAccum → Tuple JCursor Axis → AxisAccum
  axisFolder accum (Tuple cursor axis)
    | Ax.isCatAxis axis = accum { category = cons cursor accum.category }
    | Ax.isValAxis axis = accum { value = cons cursor accum.value }
    | Ax.isTimeAxis axis = accum {time = cons cursor accum.time }
    | otherwise = accum


  setPreviousValueFrom
    ∷ ∀ a. (Eq a) ⇒ Maybe (Select a) → Select a → Select a
  setPreviousValueFrom mbSel target  =
    (maybe id trySelect' $ mbSel >>= Lens.view _value) $ target

  pieBarConfiguration ∷ AxisAccum → ChartConfiguration → ChartConfiguration
  pieBarConfiguration axises current =
    let allAxises = axises.category ⊕ axises.time ⊕ axises.value
        categories =
          setPreviousValueFrom (index current.series 0)
          $ autoSelect $ newSelect allAxises
        measures =
          setPreviousValueFrom (index current.measures 0)
          $ autoSelect $ newSelect $ depends categories axises.value
        firstSeries =
          setPreviousValueFrom (index current.series 1)
          $ newSelect $ ifSelected [categories] $ allAxises ⊝ categories
        secondSeries =
          setPreviousValueFrom (index current.series 2)
          $ newSelect $ ifSelected [categories, firstSeries]
          $ allAxises ⊝ categories ⊝ firstSeries
        aggregation =
          setPreviousValueFrom (index current.aggregations 0) aggregationSelect
    in { series: [categories, firstSeries, secondSeries]
       , dimensions: []
       , measures: [measures]
       , aggregations: [aggregation]
       }

  lineConfiguration ∷ AxisAccum → ChartConfiguration → ChartConfiguration
  lineConfiguration axises current =
    let allAxises = (axises.category ⊕ axises.time ⊕ axises.value)
        dimensions =
          setPreviousValueFrom (index current.dimensions 0)
          $ autoSelect $ newSelect $ dependsOnArr axises.value
          -- This is redundant, I've put it here to notify
          -- that this behaviour differs from pieBar and can be changed.
          $ allAxises
        firstMeasures =
          setPreviousValueFrom (index current.measures 0)
          $ autoSelect $ newSelect $ depends dimensions
          $ axises.value ⊝ dimensions
        secondMeasures =
          setPreviousValueFrom (index current.measures 1)
          $ newSelect $ ifSelected [firstMeasures]
          $ depends dimensions
          $ axises.value ⊝ firstMeasures ⊝ dimensions
        firstSeries =
          setPreviousValueFrom (index current.series 0)
          $ newSelect $ ifSelected [dimensions] $ allAxises ⊝ dimensions
        secondSeries =
          setPreviousValueFrom (index current.series 1)
          $ newSelect $ ifSelected [dimensions, firstSeries]
          $ allAxises ⊝ dimensions ⊝ firstSeries
        firstAggregation =
          setPreviousValueFrom (index current.aggregations 0) aggregationSelect
        secondAggregation =
          setPreviousValueFrom (index current.aggregations 1) aggregationSelect
    in { series: [firstSeries, secondSeries]
       , dimensions: [dimensions]
       , measures: [firstMeasures, secondMeasures]
       , aggregations: [firstAggregation, secondAggregation]
       }

  areaConfiguration ∷ AxisAccum → ChartConfiguration → ChartConfiguration
  areaConfiguration axises current =
    let allAxises = (axises.category ⊕ axises.time ⊕ axises.value)
        dimensions =
          setPreviousValueFrom (index current.dimensions 0)
          $ autoSelect $ newSelect $ dependsOnArr axises.value
          -- This is redundant, I've put it here to notify
          -- that this behaviour differs from pieBar and can be changed.
          $ allAxises
        firstMeasures =
          setPreviousValueFrom (index current.measures 0)
          $ autoSelect $ newSelect $ depends dimensions
          $ axises.value ⊝ dimensions
        secondMeasures =
          setPreviousValueFrom (index current.measures 1)
          $ newSelect $ ifSelected [firstMeasures]
          $ depends dimensions
          $ axises.value ⊝ firstMeasures ⊝ dimensions
        firstSeries =
          setPreviousValueFrom (index current.series 0)
          $ newSelect $ ifSelected [dimensions] $ allAxises ⊝ dimensions
        secondSeries =
          setPreviousValueFrom (index current.series 1)
          $ newSelect $ ifSelected [dimensions, firstSeries]
          $ allAxises ⊝ dimensions ⊝ firstSeries
        firstAggregation =
          setPreviousValueFrom (index current.aggregations 0) aggregationSelect
        secondAggregation =
          setPreviousValueFrom (index current.aggregations 1) aggregationSelect
    in { series: [firstSeries, secondSeries]
       , dimensions: [dimensions]
       , measures: [firstMeasures, secondMeasures]
       , aggregations: [firstAggregation, secondAggregation]
       }

peek ∷ ∀ a. H.ChildF ChartType Form.QueryP a → VizDSL Unit
peek _ = configure *> CC.raiseUpdatedP' CC.EvalModelUpdate
