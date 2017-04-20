{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
import Reflex
import Reflex.Dom.Core
import qualified Data.Map as Map
import Data.Map (Map)
import Safe      (readMay)
import Data.Text (pack, unpack, Text)
import Control.Applicative ((<*>), (<$>))
import qualified Language.Javascript.JSaddle                       as JS
import GHCJS.DOM.Types (MediaStream, liftJSM, MonadJSM)
import qualified GHCJS.DOM.Window                  as Window
import qualified GHCJS.DOM                         as DOM
import qualified GHCJS.DOM.Types                   as DOM
import qualified GHCJS.DOM.MediaStream             as MediaStream
import qualified GHCJS.DOM.Navigator               as Navigator
import qualified GHCJS.DOM.MediaStreamTrack        as MediaStreamTrack
import Data.Maybe
import Data.Foldable
import GHCJS.DOM.EventM (on)
import           GHCJS.DOM.MediaStreamTrack     (ended)
import Data.Monoid

main = mainWidget $ el "div" $ mdo
  clicked <- button "Load video!"
  dynStop <- widgetHold (pure never) $ renderVideo <$> leftmost [ const True <$> clicked
                                                                , const False <$> stopped
                                                                ]
  let stopped = switchPromptlyDyn dynStop
  pure ()
  where
    renderVideo False = pure never
    renderVideo True = videoWidget

videoWidget :: (MonadWidget t m) => m (Event t ())
videoWidget = do
  stopped <- button "Stop video!"
  stream <- getInitialMediaStream
  performEvent_ $ const (stopMediaStream stream) <$> stopped
  mediaVideo stream (Map.empty)
  pure stopped

mediaVideo :: (DomBuilder t m, MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace)
              => MediaStream -> Map Text Text -> m ()
mediaVideo stream attrs = do
  (videoTag, _) <- elAttr' "video" ("id" =: "myvideo" <> attrs) blank
  let rawElement =  _element_raw videoTag
  liftJSM $ do
    JS.toJSVal rawElement JS.<# ("srcObject" :: Text) $ stream
    _ <- JS.toJSVal rawElement JS.# ("play" :: Text) $ ([] :: [JS.JSVal])
    -- Does not seem to work properly right now ... :-(
    tracks <- catMaybes <$> MediaStream.getTracks stream
    -- Cleanup necessary because of DOM node leak in reflex, see: https://bugs.chromium.org/p/chromium/issues/detail?id=255456
    let registerCleanup track = do
          _ <- on track ended . liftJSM
               $ JS.toJSVal rawElement JS.<# ("srcObject" :: Text) $ JS.JSNull
          pure ()
    traverse_ registerCleanup tracks
    pure ()


getInitialMediaStream :: forall m. MonadJSM m => m MediaStream
getInitialMediaStream = do
  navigator <- Window.getNavigatorUnsafe =<< DOM.currentWindowUnchecked
  constr <- makeSimpleUserMediaDictionary True True
  Navigator.getUserMedia navigator $ Just constr

makeSimpleUserMediaDictionary :: (MonadJSM m) => Bool -> Bool -> m DOM.Dictionary
makeSimpleUserMediaDictionary audio video= liftJSM $ do
  rawDic <- JS.obj
  rawDic JS.<# ("audio" :: Text) $ audio
  rawDic JS.<# ("video" :: Text) $ video
  case rawDic of
    JS.Object val -> do
      -- eval "console" ^. jsf "log" [val^.js "video"^. js0 "toString" ]
      pure $ DOM.Dictionary val

stopMediaStream :: forall m. MonadJSM m => MediaStream -> m ()
stopMediaStream stream = do
  tracks <- catMaybes <$> MediaStream.getTracks stream
  traverse_ MediaStreamTrack.stop tracks
