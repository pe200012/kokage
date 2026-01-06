{-# LANGUAGE OverloadedStrings #-}

-- | Cairo rendering functions for balloon windows.
module Kokage.Balloon.Render
  ( -- * Main rendering
    drawBalloonCairo
    -- * Component rendering
  , drawSolidBackground
  , drawText
  , drawChoices
  ) where

import           Control.Monad              ( when )
import qualified Data.Text                  as T

import qualified GI.Cairo.Render            as Cairo
import qualified GI.Cairo.Render.Connector  as Cairo ( getContext )
import qualified GI.Pango                   as Pango
import qualified GI.PangoCairo              as PangoCairo

import           Kokage.Balloon.Types       ( BalloonChoice(..), BalloonConfig(..) )
import           Types.Balloon              ( ShadowStyle(..) )

-- | Cairo drawing implementation.
-- Returns the list of choice rectangles for click detection.
drawBalloonCairo :: BalloonConfig
                 -> T.Text
                 -> Int
                 -> Maybe Cairo.Surface
                 -> [BalloonChoice]
                 -> Cairo.Render [(BalloonChoice, Double, Double, Double, Double)]
drawBalloonCairo config text scrollLine mSurface choices = do
  -- Draw background
  case mSurface of
    Just surface -> do
      Cairo.setSourceSurface surface 0 0
      Cairo.paint
    Nothing -> do
      drawSolidBackground config

  -- Draw text using PangoCairo (with clipping and scrolling)
  textEndY <- drawText config text scrollLine

  -- Draw choices below the text
  drawChoices config choices textEndY scrollLine

-- | Draw solid background with rounded corners.
drawSolidBackground :: BalloonConfig -> Cairo.Render ()
drawSolidBackground config = do
  let r = 10  -- Corner radius
      w = fromIntegral (bcfOriginX config * 2 + bcfValidWidth config)
      h = fromIntegral (bcfOriginY config * 2 + bcfValidHeight config)
      pi' = pi :: Double

  -- Draw rounded rectangle path
  Cairo.newPath
  Cairo.arc (w - r) r r (-(pi'/2)) 0
  Cairo.arc (w - r) (h - r) r 0 (pi'/2)
  Cairo.arc r (h - r) r (pi'/2) pi'
  Cairo.arc r r r pi' (3*pi'/2)
  Cairo.closePath

  -- Fill with background color
  Cairo.setSourceRGBA
    (bcfBgColorR config)
    (bcfBgColorG config)
    (bcfBgColorB config)
    (bcfBgAlpha config)
  Cairo.fill

  -- Draw border
  Cairo.newPath
  Cairo.arc (w - r) r r (-(pi'/2)) 0
  Cairo.arc (w - r) (h - r) r 0 (pi'/2)
  Cairo.arc r (h - r) r (pi'/2) pi'
  Cairo.arc r r r pi' (3*pi'/2)
  Cairo.closePath
  Cairo.setSourceRGBA 0.5 0.5 0.5 1.0
  Cairo.setLineWidth 1
  Cairo.stroke

-- | Draw text using PangoCairo with clipping and scroll support.
-- Returns the Y position after the text (for choice rendering).
drawText :: BalloonConfig -> T.Text -> Int -> Cairo.Render Double
drawText config text scrollLine = do
  ctx <- Cairo.getContext

  layout <- Cairo.liftIO $ PangoCairo.createLayout ctx
  Cairo.liftIO $ Pango.layoutSetText layout text (-1)

  fontDesc <- Cairo.liftIO Pango.fontDescriptionNew
  Cairo.liftIO $ Pango.fontDescriptionSetFamily fontDesc (bcfFontName config)
  Cairo.liftIO $ Pango.fontDescriptionSetSize fontDesc (fromIntegral $ bcfFontSize config * fromIntegral Pango.SCALE)

  when (bcfFontBold config) $
    Cairo.liftIO $ Pango.fontDescriptionSetWeight fontDesc Pango.WeightBold
  when (bcfFontItalic config) $
    Cairo.liftIO $ Pango.fontDescriptionSetStyle fontDesc Pango.StyleItalic

  Cairo.liftIO $ Pango.layoutSetFontDescription layout (Just fontDesc)

  attrs <- Cairo.liftIO Pango.attrListNew
  when (bcfFontUnderline config) $ do
    attr <- Cairo.liftIO $ Pango.attrUnderlineNew Pango.UnderlineSingle
    Cairo.liftIO $ Pango.attrListInsert attrs attr
  when (bcfFontStrike config) $ do
    attr <- Cairo.liftIO $ Pango.attrStrikethroughNew True
    Cairo.liftIO $ Pango.attrListInsert attrs attr
  Cairo.liftIO $ Pango.layoutSetAttributes layout (Just attrs)

  Cairo.liftIO $ Pango.layoutSetWidth layout (fromIntegral $ bcfValidWidth config * fromIntegral Pango.SCALE)
  Cairo.liftIO $ Pango.layoutSetWrap layout Pango.WrapModeChar
  Cairo.liftIO $ Pango.layoutSetSpacing layout (fromIntegral $ bcfLineSpacing config * fromIntegral Pango.SCALE)

  lineHeight <- Cairo.liftIO $ do
    pangoCtx <- Pango.layoutGetContext layout
    metrics <- Pango.contextGetMetrics pangoCtx (Just fontDesc) Nothing
    ascent <- Pango.fontMetricsGetAscent metrics
    descent <- Pango.fontMetricsGetDescent metrics
    return $ (fromIntegral ascent + fromIntegral descent + fromIntegral (bcfLineSpacing config * fromIntegral Pango.SCALE)) / fromIntegral Pango.SCALE

  (_, textHeight) <- Cairo.liftIO $ Pango.layoutGetPixelSize layout

  Cairo.save
  Cairo.rectangle
    (fromIntegral $ bcfOriginX config)
    (fromIntegral $ bcfOriginY config)
    (fromIntegral $ bcfValidWidth config)
    (fromIntegral $ bcfValidHeight config)
  Cairo.clip

  let scrollOffset = fromIntegral scrollLine * lineHeight

  -- Draw Shadow (if enabled)
  case bcfShadowStyle config of
    ShadowNone -> return ()
    ShadowOffset -> do
      Cairo.save
      Cairo.setSourceRGB (bcfShadowColorR config) (bcfShadowColorG config) (bcfShadowColorB config)
      Cairo.moveTo
        (fromIntegral (bcfOriginX config) + 1)
        (fromIntegral (bcfOriginY config) - scrollOffset + 1)
      Cairo.liftIO $ PangoCairo.showLayout ctx layout
      Cairo.restore
    ShadowOutline -> do
      Cairo.save
      Cairo.setSourceRGB (bcfShadowColorR config) (bcfShadowColorG config) (bcfShadowColorB config)
      Cairo.moveTo
        (fromIntegral (bcfOriginX config))
        (fromIntegral (bcfOriginY config) - scrollOffset)
      Cairo.liftIO $ PangoCairo.layoutPath ctx layout
      Cairo.setLineWidth 2.0
      Cairo.stroke
      Cairo.restore

  Cairo.moveTo
    (fromIntegral $ bcfOriginX config)
    (fromIntegral (bcfOriginY config) - scrollOffset)

  Cairo.setSourceRGB
    (bcfTextColorR config)
    (bcfTextColorG config)
    (bcfTextColorB config)

  Cairo.liftIO $ PangoCairo.showLayout ctx layout
  Cairo.restore

  return $ fromIntegral (bcfOriginY config) + fromIntegral textHeight - scrollOffset + fromIntegral (bcfLineSpacing config)

-- | Draw choices below the text.
-- Returns list of choice rectangles for hit testing.
drawChoices :: BalloonConfig
            -> [BalloonChoice]
            -> Double
            -> Int
            -> Cairo.Render [(BalloonChoice, Double, Double, Double, Double)]
drawChoices config choices startY _scrollLine = do
  if null choices
    then return []
    else do
      ctx <- Cairo.getContext

      fontDesc <- Cairo.liftIO Pango.fontDescriptionNew
      Cairo.liftIO $ Pango.fontDescriptionSetFamily fontDesc (bcfFontName config)
      Cairo.liftIO $ Pango.fontDescriptionSetSize fontDesc (fromIntegral $ bcfFontSize config * fromIntegral Pango.SCALE)

      sampleLayout <- Cairo.liftIO $ PangoCairo.createLayout ctx
      Cairo.liftIO $ Pango.layoutSetFontDescription sampleLayout (Just fontDesc)
      Cairo.liftIO $ Pango.layoutSetText sampleLayout "Test" (-1)
      (_, sampleHeight) <- Cairo.liftIO $ Pango.layoutGetPixelSize sampleLayout
      let lineHeight = fromIntegral sampleHeight + fromIntegral (bcfLineSpacing config)

      Cairo.save
      Cairo.rectangle
        (fromIntegral $ bcfOriginX config)
        (fromIntegral $ bcfOriginY config)
        (fromIntegral $ bcfValidWidth config)
        (fromIntegral $ bcfValidHeight config)
      Cairo.clip

      rects <- drawChoiceLoop fontDesc config choices startY lineHeight []

      Cairo.restore
      return rects
  where
    drawChoiceLoop :: Pango.FontDescription
                   -> BalloonConfig
                   -> [BalloonChoice]
                   -> Double
                   -> Double
                   -> [(BalloonChoice, Double, Double, Double, Double)]
                   -> Cairo.Render [(BalloonChoice, Double, Double, Double, Double)]
    drawChoiceLoop _ _ [] _ _ acc = return $ reverse acc
    drawChoiceLoop fontDesc cfg (choice:rest) y lh acc = do
      ctx <- Cairo.getContext

      layout <- Cairo.liftIO $ PangoCairo.createLayout ctx
      let choiceText = "▶ " <> bcText choice
      Cairo.liftIO $ Pango.layoutSetText layout choiceText (-1)
      Cairo.liftIO $ Pango.layoutSetFontDescription layout (Just fontDesc)
      Cairo.liftIO $ Pango.layoutSetWidth layout (fromIntegral $ bcfValidWidth cfg * fromIntegral Pango.SCALE)

      (textWidth, textHeight) <- Cairo.liftIO $ Pango.layoutGetPixelSize layout

      Cairo.setSourceRGB 0.0 0.4 0.8
      Cairo.moveTo (fromIntegral $ bcfOriginX cfg) y
      Cairo.liftIO $ PangoCairo.showLayout ctx layout

      let rect = (choice, fromIntegral $ bcfOriginX cfg, y, fromIntegral textWidth, fromIntegral textHeight)

      drawChoiceLoop fontDesc cfg rest (y + lh) lh (rect : acc)
