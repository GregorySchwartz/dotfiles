{-# LANGUAGE FlexibleContexts #-}

module Plots.PublishTheme where

import Diagrams.Prelude

import Plots
import Plots.Axis.Line
import Plots.Style

publishableTheme :: m ()
publishableTheme = do
    -- Grid lines.
    hideGridLines

    -- Axes.
    xAxis . axisLineType .= MiddleAxisLine
    yAxis . axisLineType .= LeftAxisLine
    yAxis . axisLineStyle .= mempty # lwO 1
    xAxis . axisLineStyle .= mempty # lwO 1

    -- Ticks.
    hide (xAxis . minorTicks)
    hide (yAxis . minorTicks)
    xAxis . majorTicksAlignment .= outsideTicks
    yAxis . majorTicksAlignment .= outsideTicks
    xAxis . majorTicksStyle .= mempty # lwO 1
    yAxis . majorTicksStyle .= mempty # lwO 1

    -- Line style.
    lineStyle . _lineWidth .= 1
    lineStyle . _lineCap .= LineCapSquare
    lineStyle . _lineJoin .= LineJoinBevel

    -- Bar style.
    --areaStyle .= (mempty # fc black # lc black)

publishableFont :: m ()
publishableFont = do
    -- Fonts.
    axisLabelStyle &= do
        _fontSize .= (output 6.45) -- 8pt font
        _font .= Just "Arial"
    tickLabelStyle &= do
        _fontSize .= (output 5.63) -- 7pt font
        _font .= Just "Arial"
