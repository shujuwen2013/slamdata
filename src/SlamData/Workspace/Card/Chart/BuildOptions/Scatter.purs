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

module SlamData.Workspace.Card.Chart.BuildOptions.Scatter where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Array ((!!), nub, length, range)
import Data.Array as A
import Data.Function (on)
import Data.Int as Int
import Data.List (List(..), zip, catMaybes, null, head, groupBy, sortBy, fromList, singleton)
import Data.Map (Map)
import Data.Tuple (fst, snd)
import Data.Maybe (isJust)
import Data.Maybe.Unsafe (fromJust)

import ECharts as EC

import SlamData.Workspace.Card.Chart.Aggregation (Aggregation(..), runAggregation)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration)
import SlamData.Workspace.Card.Chart.BuildOptions.Common (SeriesKey, ChartAxises, colors, mixAxisLabelAngleAndFontSize, buildChartAxises, mkKey, keyName, getShadeColor, toRGBAString)


type ScatterData = Array (Tuple String (Array (Tuple Number Number)))

scatterData ∷ ChartAxises → ScatterData

scatterData axises = fromList $ 
  --output sample: ((Tuple "A" ((Tuple 1 1) : (Tuple 2 2))) : (Tuple "B" ((Tuple 1 1)))) 
  catMaybes $ map combine $
  --output sample: ((Tuple "A" (Tuple 1 1) : Tuple "A" (Tuple 2 2)) : (Tuple "B" (Tuple 1 1))) 
  groupBy ((==) `on` getGroupKey) $ sortBy (compare `on` getGroupKey) $
  -- utput sample: (Tuple "A" (Tuple 1 1) : Tuple "A" (Tuple 2 2) : Tuple "B" (Tuple 1 1))
  catMaybes $ map mkPoint 
    (tagSeriesKey seriesKeys $ zip firstValues secondValues)
  where
  firstValues ∷ List (Maybe Number)
  firstValues = fromMaybe Nil $ axises.measures !! 0

  secondValues ∷ List (Maybe Number)
  secondValues = fromMaybe Nil $ axises.measures !! 1

  firstSeries ∷ List (Maybe String)
  firstSeries = fromMaybe Nil $ axises.series !! 0

  secondSeries ∷ List (Maybe String)
  secondSeries = fromMaybe Nil $ axises.series !! 1

  firstAgg ∷ Aggregation
  firstAgg = fromMaybe None $ join (axises.aggregations !! 0)

  secondAgg ∷ Aggregation
  secondAgg = fromMaybe None $ join (axises.aggregations !! 1)

  tagSeriesKey ∷ List SeriesKey → List (Tuple (Maybe Number) (Maybe Number)) → 
    List (Tuple SeriesKey (Tuple (Maybe Number) (Maybe Number)))
  tagSeriesKey k v = case null k of
    true → map (Tuple Nothing) v
    false → zip k v

  mkPoint ∷ Tuple SeriesKey (Tuple (Maybe Number) (Maybe Number)) → 
    Maybe (Tuple String (Tuple Number Number))
  mkPoint (Tuple a (Tuple b c)) = 
    case (isJust b) && (isJust c) of
      true → Just $ Tuple 
        (keyName (Tuple "" a))
        (Tuple (fromJust b) (fromJust c))
      _ → Nothing
  
  seriesKeys ∷ List SeriesKey
  seriesKeys = case null firstSeries of
    true → Nil 
    _ → case null secondSeries of
      true → map (flip mkSeriesKey Nothing) firstSeries
      _ → map (mkSeriesKey <$> fst <*> snd) (zip firstSeries secondSeries)
  
  mkSeriesKey ∷ Maybe String → Maybe String → SeriesKey
  mkSeriesKey f s =
    f >>= \f → pure $ Tuple f s
    
  getGroupKey ∷ Tuple String (Tuple Number Number) → String
  getGroupKey (Tuple k v) = k
  
  combine ∷ List (Tuple String (Tuple Number Number)) → 
    Maybe (Tuple String (Array (Tuple Number Number)))
  combine x = case head x of
    Just t → Just $ Tuple 
      (getGroupKey t) 
      (fromList $ applyAggregation $ map snd x)
    _ → Nothing

  applyAggregation ∷ List (Tuple Number Number) → 
    List (Tuple Number Number)
  applyAggregation a =
    let
      fv = map fst a
      sv = map snd a
      fv' =
        if firstAgg == None
          then fv
          else singleton (fromMaybe zero $ runAggregation firstAgg fv)
      sv' =
        if secondAgg == None
          then sv
          else singleton (fromMaybe zero $ runAggregation secondAgg sv)
    in
      case Tuple (firstAgg == None) (secondAgg == None) of
        (Tuple true true) → a
        (Tuple false true) → map (Tuple (fromMaybe zero $ head fv')) sv'
        (Tuple true false) → map (flip Tuple (fromMaybe zero $ head sv')) fv'
        (Tuple false false) → zip fv' sv'

buildScatter
  ∷ Map JCursor Ax.Axis
   → Int
   → Int
   → ChartConfiguration
   → EC.Option
buildScatter axises angle size conf = case preSeries of
  series →
    EC.Option EC.optionDefault
      { series = Just $ map Just series
      , xAxis = Just valueAxis
      , yAxis = Just valueAxis
      , tooltip = Just tooltip
      , legend = Just $ mkLegend series
      , color = Just colors
      }
  where
  mkLegend ∷ Array EC.Series → EC.Legend
  mkLegend ss =
    EC.Legend EC.legendDefault
      { "data" = Just $ map EC.legendItemDefault $ extractNames ss
      , textStyle = Just $ EC.TextStyle EC.textStyleDefault
          { fontFamily = Just "Ubuntu" }
      }

  tooltip ∷ EC.Tooltip
  tooltip = EC.Tooltip $ EC.tooltipDefault 
    { trigger = Just EC.TriggerAxis
    , textStyle = Just $ EC.TextStyle EC.textStyleDefault 
        { fontFamily = Just "Ubuntu"
        , fontSize = Just 12.0 
        }
    , axisPointer = Just $ EC.TooltipAxisPointer EC.tooltipAxisPointerDefault 
        { "type" = Just $ EC.CrossPointer
        , crossStyle = Just $ EC.LineStyle EC.lineStyleDefault 
            { color = Just "rgba(170,170,170,0.6)"
            , width = Just 0.6
            , "type" = Just $ EC.Solid
            }
        }
    }

  extractNames ∷ Array EC.Series → Array String
  extractNames ss = nub $ A.catMaybes $ map extractName ss

  extractName ∷ EC.Series → Maybe String
  extractName (EC.ScatterSeries r) = r.common.name
  extractName _ = Nothing

  extracted ∷ ScatterData
  extracted = scatterData $ buildChartAxises axises conf

  valueAxis ∷ EC.Axises
  valueAxis = EC.OneAxis valueAxis'

  valueAxis' ∷ EC.Axis
  valueAxis' =
    EC.Axis EC.axisDefault
      { "type" = Just EC.ValueAxis
      , axisLabel = Just $ EC.AxisLabel EC.axisLabelDefault
        { textStyle = Just $ EC.TextStyle EC.textStyleDefault
          { fontFamily = Just "Ubuntu"
          , fontSize = Just $ Int.toNumber size
          }
        }
      , axisLine = Just $ EC.AxisLine EC.axisLineDefault 
        { lineStyle = Just $ EC.AxisLineStyle EC.axisLineStyleDefault 
            { color = Just ([Tuple 1.0 "rgba(184,184,184,0.8)"])
            , width = Just 1.0
            }
        }
      , splitLine = Just $ EC.AxisSplitLine EC.axisSplitLineDefault 
        { lineStyle = Just $ EC.LineStyle EC.lineStyleDefault 
          { color = Just "rgba(204,204,204,0.2)"
          , width = Just 1.0
          }
         }
      }

  preSeries ∷ Array EC.Series
  preSeries = mkSeries extracted


mkSeries
  ∷ ScatterData
   → (Array EC.Series)
mkSeries sData =
  series
  where
  series ∷ Array EC.Series
  series = map serie (A.zip (range 0 ((length sData)-1)) sData)
  
  serie ∷ Tuple Int (Tuple String (Array (Tuple Number Number))) → EC.Series
  serie (Tuple ind (Tuple name nums)) = 
    EC.ScatterSeries 
      { common: EC.universalSeriesDefault 
        { name = if name ≡ "" 
                 then Nothing 
                 else Just name
        , itemStyle = Just $ EC.ItemStyle EC.itemStyleDefault 
          { normal = Just $ EC.IStyle EC.istyleDefault 
            { color = Just $ EC.SimpleColor $ 
                fromMaybe "#000000" 
                  (colors !! (mod ind (length colors)))
            }     
          }
        }
      , scatterSeries: EC.scatterSeriesDefault
        { "data" = Just $ map simpleData nums
        , large = Just true
        , symbol = Just EC.Circle
        }
      }

  simpleData ∷ Tuple Number Number → EC.ItemData
  simpleData (Tuple a b) = EC.Value $ 
    EC.XYR { x: a
           , y: b
           , r: Nothing
           }
