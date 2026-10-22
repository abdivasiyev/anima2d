{- | How the window looks: the browser's design tokens (its dark theme) as a GTK stylesheet,
for the widgets, and as colours, for what Cairo draws; and the icons as images.
-}
module Anima.Desktop.Style
    ( applyStyle
    , iconImage
    , iconPixbuf
    , Rgb
    , workspace
    , ground
    , surface
    , hairline
    , textColour
    , dimText
    , accent
    , accentSoft
    , rgb
    , rgba
    , uiText
    , rounded
    )
where

import Control.Monad (forM_)
import Data.Text (Text)

import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GI.Cairo.Render qualified as C
import GI.Cairo.Render.Connector qualified as Connector
import GI.Gdk qualified as Gdk
import GI.GdkPixbuf qualified as Pixbuf
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango
import GI.PangoCairo qualified as PangoCairo

import Anima.Desktop.Icons (iconSvg)

type Rgb = (Double, Double, Double)

workspace, ground, surface, textColour, dimText, accent :: Rgb
workspace = (0x11 / 255, 0x11 / 255, 0x13 / 255)
ground = (0x16 / 255, 0x16 / 255, 0x18 / 255)
surface = (0x1e / 255, 0x1e / 255, 0x21 / 255)
textColour = (0xed / 255, 0xed / 255, 0xef / 255)
dimText = (0x8b / 255, 0x8b / 255, 0x93 / 255)
accent = (0x5b / 255, 0x8c / 255, 1)

-- | Borders, at the opacity they are drawn with.
hairline, accentSoft :: (Rgb, Double)
hairline = ((1, 1, 1), 0.08)
accentSoft = (accent, 0.16)

rgb :: Rgb -> C.Render ()
rgb (r, g, b) = C.setSourceRGB r g b

rgba :: (Rgb, Double) -> C.Render ()
rgba ((r, g, b), a) = C.setSourceRGBA r g b a

css :: Text
css =
    "* { font-family: -apple-system, BlinkMacSystemFont, \"SF Pro Text\", \"Inter\", system-ui, sans-serif; font-size: 13px; text-shadow: none; -gtk-icon-shadow: none; outline: none; }\n\
    \window, .background, stack, box { color: #ededef; }\n\
    \window, .background { background-color: #161618; }\n\
    \headerbar { background: #161618; border: none; border-bottom: 1px solid rgba(255,255,255,0.08); box-shadow: none; min-height: 40px; padding: 0 6px; }\n\
    \headerbar .title { font-weight: 600; }\n\
    \headerbar .subtitle { font-size: 11px; color: #8b8b93; }\n\
    \button { background: transparent; background-image: none; border: 1px solid transparent; box-shadow: none; border-radius: 6px; color: #ededef; padding: 3px 9px; min-height: 26px; transition: background-color 80ms; }\n\
    \button:hover { background-color: rgba(255,255,255,0.06); }\n\
    \button:active, button:checked { background-color: rgba(91,140,255,0.16); color: #8eb0ff; }\n\
    \button:disabled { color: rgba(237,237,239,0.3); }\n\
    \button:disabled image { opacity: 0.35; }\n\
    \button.icon { padding: 4px; min-width: 26px; }\n\
    \button.primary { background-color: #5b8cff; color: #ffffff; font-weight: 500; }\n\
    \button.primary:hover { background-color: #6e99ff; }\n\
    \.rail { background: #1e1e21; border-right: 1px solid rgba(255,255,255,0.08); padding: 6px 5px; }\n\
    \.rail button { min-width: 34px; min-height: 34px; padding: 0; }\n\
    \.bar { background: #1e1e21; border-bottom: 1px solid rgba(255,255,255,0.08); padding: 3px 8px; min-height: 34px; }\n\
    \.transport { background: #1e1e21; border-top: 1px solid rgba(255,255,255,0.08); border-bottom: 1px solid rgba(255,255,255,0.08); padding: 3px 8px; }\n\
    \.sidebar { background: #1e1e21; border-left: 1px solid rgba(255,255,255,0.08); }\n\
    \.sidebar .heading { font-weight: 600; padding: 12px 14px 4px 14px; }\n\
    \.dim { color: #8b8b93; font-size: 12px; }\n\
    \.board-top { padding: 4px 6px; border-bottom: 1px solid rgba(255,255,255,0.08); }\n\
    \.chip { padding: 2px 10px; min-height: 24px; border-radius: 6px; color: #a4a4ac; font-size: 12px; }\n\
    \.chip.on { background-color: rgba(91,140,255,0.16); color: #8eb0ff; }\n\
    \stackswitcher { background: #161618; border-radius: 7px; padding: 2px; }\n\
    \stackswitcher button { padding: 2px 14px; min-height: 24px; font-size: 12px; border-radius: 5px; }\n\
    \.counter { color: #8b8b93; font-size: 12px; font-feature-settings: \"tnum\"; min-width: 56px; }\n\
    \.sep { background: rgba(255,255,255,0.08); min-width: 1px; min-height: 18px; margin: 0 6px; }\n\
    \.segmented { background: #161618; border-radius: 7px; padding: 2px; }\n\
    \.segmented button { min-height: 24px; min-width: 24px; padding: 2px 4px; border-radius: 5px; }\n\
    \scale { padding: 0 4px; }\n\
    \scale trough { background: rgba(255,255,255,0.16); border: none; min-height: 3px; border-radius: 2px; }\n\
    \scale highlight { background: #5b8cff; border: none; border-radius: 2px; }\n\
    \scale slider { background: #ededef; border: none; box-shadow: 0 1px 2px rgba(0,0,0,0.5); min-width: 12px; min-height: 12px; margin: -5px; border-radius: 6px; }\n\
    \scale value { color: #8b8b93; font-size: 11px; min-width: 30px; margin-left: 8px; }\n\
    \paned > separator { background: rgba(255,255,255,0.08); min-width: 1px; background-image: none; }\n\
    \flowboxchild { padding: 0; }\n\
    \.card { background: #1e1e21; border: 1px solid rgba(255,255,255,0.08); border-radius: 9px; padding: 0; }\n\
    \.card:hover { background: #1e1e21; border-color: rgba(255,255,255,0.2); }\n\
    \.card .name { font-weight: 500; }\n\
    \.card .meta { color: #8b8b93; font-size: 11px; }\n\
    \.home-title { font-size: 20px; font-weight: 600; }\n\
    \entry { background: #111113; border: 1px solid rgba(255,255,255,0.16); border-radius: 6px; color: #ededef; box-shadow: none; padding: 4px 8px; min-height: 26px; }\n\
    \entry:focus { border-color: #5b8cff; }\n\
    \dialog .dialog-action-area button { min-width: 72px; }\n\
    \tooltip { background: #2a2a2e; border-radius: 6px; border: 1px solid rgba(255,255,255,0.08); }\n\
    \tooltip label { color: #ededef; font-size: 12px; }\n\
    \colorbutton { padding: 3px; }\n\
    \colorbutton colorswatch { border-radius: 5px; min-width: 18px; min-height: 18px; box-shadow: inset 0 0 0 1px rgba(255,255,255,0.22); }\n\
    \checkbutton check { background: #111113; border: 1px solid rgba(255,255,255,0.2); border-radius: 4px; }\n\
    \checkbutton check:checked { background: #5b8cff; border-color: #5b8cff; }\n\
    \scrollbar { background: transparent; border: none; }\n\
    \scrollbar slider { background: rgba(255,255,255,0.16); border-radius: 4px; min-width: 6px; min-height: 6px; }\n"

-- | The stylesheet, over the theme, for every window; and the theme's dark variant under it.
applyStyle :: IO ()
applyStyle = do
    settings <- Gtk.settingsGetDefault
    forM_ settings $ \s -> Gtk.setSettingsGtkApplicationPreferDarkTheme s True
    provider <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromData provider (Text.encodeUtf8 css)
    screen <- Gdk.screenGetDefault
    forM_ screen $ \sc -> Gtk.styleContextAddProviderForScreen sc provider (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

-- | Icon `name` at `size` points, drawn at twice that for sharp screens.
iconPixbuf :: Text -> Text -> Int -> IO (Maybe Pixbuf.Pixbuf)
iconPixbuf colour name size = case iconSvg colour name of
    Nothing -> pure Nothing
    Just svg -> do
        loader <- Pixbuf.pixbufLoaderNew
        Pixbuf.pixbufLoaderSetSize loader (fromIntegral (size * 2)) (fromIntegral (size * 2))
        Pixbuf.pixbufLoaderWrite loader (Text.encodeUtf8 svg)
        Pixbuf.pixbufLoaderClose loader
        Pixbuf.pixbufLoaderGetPixbuf loader

-- | Icon `name` as an image widget, in the text colour.
iconImage :: Text -> Int -> IO Gtk.Image
iconImage name size = do
    pb <- iconPixbuf "#d4d4d8" name size
    case pb of
        Nothing -> Gtk.imageNew
        Just p -> do
            surf <- Gdk.cairoSurfaceCreateFromPixbuf p 2 (Nothing :: Maybe Gdk.Window)
            Gtk.imageNewFromSurface (Just surf)

-- | Interface text at the current point (its top left), in the system font; its size back.
uiText :: Double -> Bool -> Text -> C.Render (Double, Double)
uiText size bold t = do
    ctx <- Connector.getContext
    C.liftIO $ do
        layout <- PangoCairo.createLayout ctx
        desc <- Pango.fontDescriptionFromString ((if bold then "-apple-system, Inter, sans-serif Semi-Bold " else "-apple-system, Inter, sans-serif ") <> Text.pack (show size) <> "px")
        Pango.layoutSetFontDescription layout (Just desc)
        Pango.layoutSetText layout t (-1)
        (_, logical) <- Pango.layoutGetPixelExtents layout
        w <- Pango.getRectangleWidth logical
        h <- Pango.getRectangleHeight logical
        PangoCairo.showLayout ctx layout
        pure (fromIntegral w, fromIntegral h)

-- | A rounded rectangle path.
rounded :: Double -> Double -> Double -> Double -> Double -> C.Render ()
rounded x y w h r = do
    let
        k = min r (min (w / 2) (h / 2))
    C.newPath
    C.arc (x + w - k) (y + k) k (-(pi / 2)) 0
    C.arc (x + w - k) (y + h - k) k 0 (pi / 2)
    C.arc (x + k) (y + h - k) k (pi / 2) pi
    C.arc (x + k) (y + k) k pi (3 * pi / 2)
    C.closePath
