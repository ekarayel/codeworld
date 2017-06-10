{-# LANGUAGE CPP                      #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE OverloadedStrings        #-}

{-
  Copyright 2017 The CodeWorld Authors. All rights reserved.

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

module CodeWorld.CanvasMonad (CanvasMonad) where

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans (liftIO)
import           Data.Text (Text, pack)

#ifdef ghcjs_HOST_OS

import           Data.JSString.Text
import qualified JavaScript.Web.Canvas as Canvas
import qualified JavaScript.Web.Canvas.Internal as Canvas

#else

import qualified Graphics.Blank as Canvas
import           Graphics.Blank (Canvas)
import           Text.Printf

#endif


class (Monad m) => CanvasMonad m where
  arc :: Double -> Double -> Double -> Double -> Double -> Bool -> m ()
  beginPath :: m ()
  bezierCurveTo :: (Double, Double) -> (Double, Double) -> (Double, Double) -> m ()
  closePath :: m ()
  fill :: m ()
  fillRect :: Double -> Double -> Double -> Double -> m ()
  fillStyle :: Int -> Int -> Int -> Double -> m ()
  fillText :: Text -> (Double, Double) -> m ()
  font :: Text -> m ()
  lineTo :: (Double, Double) -> m ()
  lineWidth :: Double -> m ()
  moveTo :: (Double, Double) -> m ()
  quadraticCurveTo :: (Double, Double) -> (Double, Double) -> m ()
  restore :: m ()
  scale :: Double -> Double -> m ()
  save :: m ()
  stroke :: m ()
  strokeStyle :: Int -> Int -> Int -> Double -> m ()
  transform :: Double -> Double -> Double -> Double -> Double -> Double -> m ()

#ifdef ghcjs_HOST_OS

instance CanvasMonad (ReaderT Canvas.Context IO) where
  arc p1 p2 p3 p4 p5 p6 = ask >>= liftIO . Canvas.arc p1 p2 p3 p4 p5 p6
  beginPath = ask >>= liftIO . Canvas.beginPath
  bezierCurveTo (ax, ay) (bx, by) (cx, cy) = ask >>= liftIO . Canvas.bezierCurveTo ax ay bx by cx cy
  closePath = ask >>= liftIO . Canvas.closePath
  fill = ask >>= liftIO . Canvas.fill
  fillRect x y w h = ask >>= liftIO . Canvas.fillRect x y w h
  fillStyle r g b a = ask >>= liftIO . Canvas.fillStyle r g b a
  fillText t (x, y) = ask >>= liftIO . Canvas.fillText (textToJSString t) x y
  font t = ask >>= liftIO . Canvas.font (textToJSString t)
  lineTo (x, y) = ask >>= liftIO . Canvas.lineTo x y
  lineWidth w = ask >>= liftIO . Canvas.lineWidth w
  moveTo (x, y) = ask >>= liftIO . Canvas.moveTo x y
  quadraticCurveTo (ax, ay) (bx, by) = ask >>= liftIO . Canvas.quadraticCurveTo ax ay bx by
  restore = ask >>= liftIO . Canvas.restore
  scale x y = ask >>= liftIO . Canvas.scale x y
  save = ask >>= liftIO . Canvas.save
  stroke = ask >>= liftIO . Canvas.stroke
  strokeStyle r g b a = ask >>= liftIO . Canvas.strokeStyle r g b a
  transform ta tb tc td te tf = ask >>= liftIO . Canvas.transform ta tb tc td te tf

#else

instance CanvasMonad Canvas where
  arc p1 p2 p3 p4 p5 p6 = Canvas.arc (p1, p2, p3, p4, p5, p6)
  beginPath = Canvas.beginPath ()
  bezierCurveTo (ax, ay) (bx, by) (cx, cy) = Canvas.bezierCurveTo (ax, ay, bx, by, cx, cy)
  closePath = Canvas.closePath ()
  fill = Canvas.fill ()
  fillRect x y w h = Canvas.fillRect (x, y, w, h)
  fillStyle r g b a = Canvas.fillStyle (rgbaStyleString r g b a)
  fillText t (x, y) = Canvas.fillText (t, x, y)
  font = Canvas.font
  lineTo (x, y) = Canvas.lineTo (x, y)
  lineWidth = Canvas.lineWidth
  moveTo (x, y) = Canvas.moveTo (x, y)
  quadraticCurveTo (ax, ay) (bx, by) = Canvas.quadraticCurveTo (ax, ay, bx, by)
  restore = Canvas.restore ()
  scale rx ry = Canvas.scale (rx, ry)
  save = Canvas.save ()
  stroke = Canvas.stroke ()
  strokeStyle r g b a = Canvas.strokeStyle (rgbaStyleString r g b a)
  transform ta tb tc td te tf = Canvas.transform (ta, tb, tc, td, te, tf)

rgbaStyleString :: Int -> Int -> Int -> Double -> Text
rgbaStyleString r g b a = pack $ printf "rgba(%d,%d,%d,%f)" r g b a

#endif



