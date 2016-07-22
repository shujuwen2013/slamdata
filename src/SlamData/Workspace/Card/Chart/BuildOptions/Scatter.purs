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
import Data.Array ((!!), nub, length, range, index, sort, reverse, concat)
import Data.Array as A
import Data.Function (on)
import Data.Int as Int
import Data.List (List(..), zip, catMaybes, null, head, groupBy, sortBy,fromList, singleton)
import Data.Map (Map)
import Data.Tuple (fst, snd)
import Data.Maybe (isJust)
import Data.Maybe.Unsafe (fromJust)

import ECharts as EC

import SlamData.Workspace.Card.Chart.Aggregation (Aggregation(..), runAggregation)
import SlamData.Workspace.Card.Chart.Axis as Ax
import SlamData.Workspace.Card.Chart.ChartConfiguration (ChartConfiguration)
import SlamData.Workspace.Card.Chart.BuildOptions.Common (SeriesKey, ChartAxises, colors, mixAxisLabelAngleAndFontSize, buildChartAxises, keyName, toRGBAString, getTransparentColor)


type ScatterData = Array (Tuple String (Array (Tuple (Array Number) (Maybe Number))))

scatterData ∷ ChartAxises → ScatterData

scatterData axises = fromList $ 
  --output sample: ( Tuple "A" ((Tuple [1,1] 3) : (Tuple [2,2] 6)) : Tuple "B" ((Tuple [1,1] 9)) ) 
  catMaybes $ map combine $
  --output sample: ( (Tuple "A" (Tuple [1,1] 3) : Tuple "A" (Tuple [2,2] 6)) : (Tuple "B" (Tuple [1,1] 9)) )
  groupBy ((==) `on` fst) $ sortBy (compare `on` fst) $
  --output sample: ( Tuple "A" (Tuple [1,1] 3): Tuple "A" (Tuple [2,2] 6): Tuple "B" (Tuple [1,1] 9) )
  catMaybes $ map mkPoint $
    tagSeriesKey seriesKeys $      
      map (\x → Tuple [fst $ fst x, snd $ fst x] (snd x)) (zip (zip firstValues secondValues) thirdValues)

  where
  firstValues ∷ List (Maybe Number)
  firstValues = fromMaybe Nil $ axises.measures !! 0

  secondValues ∷ List (Maybe Number)
  secondValues = fromMaybe Nil $ axises.measures !! 1

  thirdValues ∷ List (Maybe Number)
  thirdValues = fromMaybe (map (\x → Nothing) firstValues) (axises.measures !! 2)

  firstSeries ∷ List (Maybe String)
  firstSeries = fromMaybe Nil $ axises.series !! 0

  secondSeries ∷ List (Maybe String)
  secondSeries = fromMaybe Nil $ axises.series !! 1

  firstAgg ∷ Aggregation
  firstAgg = fromMaybe None $ join (axises.aggregations !! 0)

  secondAgg ∷ Aggregation
  secondAgg = fromMaybe None $ join (axises.aggregations !! 1)

  thirdAgg ∷ Aggregation
  thirdAgg = fromMaybe None $ join (axises.aggregations !! 2)

  tagSeriesKey ∷ List SeriesKey → List (Tuple (Array (Maybe Number)) (Maybe Number)) → 
    List (Tuple SeriesKey (Tuple (Array (Maybe Number)) (Maybe Number)))
  tagSeriesKey k v = case null k of
    true → map (Tuple Nothing) v
    false → zip k v

  mkPoint ∷ Tuple SeriesKey (Tuple (Array (Maybe Number)) (Maybe Number)) → 
    Maybe (Tuple String (Tuple (Array Number) (Maybe Number)))
  mkPoint (Tuple a (Tuple [v1, v2] v3)) = case isJust $ axises.measures !! 2 of
    true → 
      case (isJust v1) && (isJust v2) && (isJust v3) of
        true → Just $ Tuple 
          (keyName (Tuple "" a))
          (Tuple [fromJust v1, fromJust v2] v3)
        _ → Nothing
    false → case (isJust v1) && (isJust v2) of
        true → Just $ Tuple 
          (keyName (Tuple "" a))
          (Tuple [fromJust v1, fromJust v2] v3)
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
  
  combine ∷ List (Tuple String (Tuple (Array Number) (Maybe Number))) → 
    Maybe (Tuple String (Array (Tuple (Array Number) (Maybe Number))))
  combine x = case head x of
    Just t → Just $ Tuple 
      (fst t) 
      (fromList $ applyAggregation $ map snd x)
    _ → Nothing

  applyAggregation ∷ List (Tuple (Array Number) (Maybe Number)) → 
    List (Tuple (Array Number) (Maybe Number))
  applyAggregation l =
    let
      fv = catMaybes $ map (flip index 0 <<< fst) l
      sv = catMaybes $ map (flip index 1 <<< fst) l
      tv = map snd l
      fv' = applyAggregation' firstAgg fv
      sv' = applyAggregation' secondAgg sv
      tv' = applyAggregation'' thirdAgg tv
    in
      case [(firstAgg == None), (secondAgg == None), (thirdAgg == None)] of
        [true, true, true] → l
        [false, false, false] → 
          singleton
            (Tuple [fromMaybe zero $ head fv', fromMaybe zero $ head sv'] 
              (fromMaybe Nothing $ head tv'))
        _ → 
          map (\x → Tuple [fst $ fst x, snd $ fst x] (snd x)) (zip (zip fv' sv') tv')
    where
    applyAggregation' ∷ Aggregation → List Number → List Number
    applyAggregation' agg vs =
      if agg == None
      then vs
      else let v = fromMaybe zero $ runAggregation agg vs
           in map (\x → v) vs
    applyAggregation'' ∷ Aggregation → List (Maybe Number) → List (Maybe Number)
    applyAggregation'' agg vs =
      if agg == None
      then vs
      else if isJust $ axises.measures !! 2
           then let v = fromMaybe zero $ runAggregation agg $ map fromJust vs
                in map (\x → Just v) vs
           else vs


buildScatter
  ∷ Map JCursor Ax.Axis
   → Number
   → Number
   → ChartConfiguration
   → EC.Option
buildScatter axises bubbleMinSize bubbleMaxSize conf = case preSeries of
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
            , width = Just 0.2
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
          }
        }
      , axisLine = Just $ EC.AxisLine EC.axisLineDefault 
        { lineStyle = Just $ EC.AxisLineStyle EC.axisLineStyleDefault 
            { color = Just "rgba(184,184,184,0.8)"
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
  preSeries = mkSeries extracted bubbleMinSize bubbleMaxSize


mkSeries
  ∷ ScatterData
   → Number
   → Number
   → (Array EC.Series)
mkSeries sData bubbleMinSize bubbleMaxSize =
  series
  where
  series ∷ Array EC.Series
  series = map serie (A.zip (range 0 ((length sData)-1)) sData)
  
  serie ∷ Tuple Int (Tuple String (Array (Tuple (Array Number) (Maybe Number)))) →
     EC.Series
  serie (Tuple ind (Tuple name nums)) = 
    EC.ScatterSeries 
      { common: EC.universalSeriesDefault 
        { name = if name ≡ "" 
                 then Nothing 
                 else Just name
        , itemStyle = Just $ EC.ItemStyle EC.itemStyleDefault 
          { normal = Just $ EC.IStyle EC.istyleDefault 
            { color = Just $ EC.SimpleColor $ toRGBAString $ getTransparentColor
                (fromMaybe "#000000" (colors !! (mod ind (length colors)))) 
                0.5
            }     
          }
        }
      , scatterSeries: EC.scatterSeriesDefault
        { "data" = Just $ map xyrData nums
        , large = Just true
        , symbol = Just EC.Circle
        , symbolSize = case thirdMeasureRange of
                         Just (Tuple rMin rMax) → 
                           Just $ EC.ArrayMappingFunc 
                             (radiusMapper bubbleMinSize bubbleMaxSize rMin rMax)
                         _ → Nothing
                         
        }
      }
    where 
    xyrData ∷ Tuple (Array Number) (Maybe Number) → EC.ItemData
    xyrData a = EC.Value $ 
      EC.XYR { x: fromMaybe zero $ (fst a) !! 0
             , y: fromMaybe zero $ (fst a) !! 1
             , r: snd a
             }

    thirdMeasureRange ∷ Maybe (Tuple Number Number)
    thirdMeasureRange = case A.length thirdValues of
      0  → Nothing
      _  → Just (Tuple minVal maxVal)
      where
      thirdValues ∷ Array Number
      thirdValues = A.catMaybes $ map snd (concat $ map snd sData)

      maxVal ∷ Number 
      maxVal = fromMaybe zero (A.head $ reverse $ sort thirdValues)

      minVal ∷ Number 
      minVal = fromMaybe zero (A.head $ sort thirdValues)

    radiusMapper ∷ Number → Number → Number → Number → 
      (Array Number → Number)
    radiusMapper bMin bMax rMin rMax = 
      if rMin == rMax
      then func1
      else func2     
      where 
      func1 ∷ Array Number → Number
      func1 [x, y, r] = if r < bMin 
               then bMin
               else if r > bMax
                    then bMax
                    else r

      func2 ∷ Array Number → Number
      func2 [x, y, r] =
        bMin*(1.0-(r-rMin)/(rMax-rMin)) + bMax*(r-rMin)/(rMax-rMin)

       