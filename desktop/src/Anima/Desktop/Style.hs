{- | How the window looks: macOS's own look, as near as GTK comes to it. Two palettes from
the system's colours (window, controls, labels, separators, the accent), light and dark,
chosen as the system is set (or by ANIMA2D_APPEARANCE); a stylesheet built from the
palette for the widgets, and the same colours for what Cairo draws; the window buttons on
the left; the system font; and the icons as images.
-}
module Anima.Desktop.Style
    ( Appearance (..)
    , applyStyle
    , appearance
    , iconImage
    , iconPixbuf
    , Token
    , workspace
    , ground
    , surface
    , raised
    , hairline
    , textColour
    , dimText
    , accent
    , accentSoft
    , celFill
    , celEmpty
    , paper
    , paperShadow
    , rgb
    , rgba
    , uiText
    , rounded
    )
where

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Numeric (showHex)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import System.Info (os)
import System.Process (readProcessWithExitCode)

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

data Appearance = Light | Dark
    deriving stock (Eq, Show)

-- | A colour with its opacity.
type Rgba = (Double, Double, Double, Double)

-- | The system's colours for one appearance (names after AppKit's).
data Palette = Palette
    { windowBg :: Rgba
    -- ^ The window and its toolbar.
    , sidebarBg :: Rgba
    -- ^ Sidebars and the tool palette.
    , canvasBg :: Rgba
    -- ^ Around the paper.
    , controlBg :: Rgba
    -- ^ Push buttons and pop-ups.
    , fieldBg :: Rgba
    -- ^ Text fields.
    , selectedSegment :: Rgba
    , label :: Rgba
    , secondary :: Rgba
    , tertiary :: Rgba
    , separator :: Rgba
    , fill :: Rgba
    -- ^ Hovered borderless buttons, tracks.
    , fillStrong :: Rgba
    , accentColour :: Rgba
    , rowSelected :: Rgba
    , shadow :: Rgba
    , iconColour :: Text
    }

light, dark :: Palette
light =
    Palette
        { windowBg = hex 0xECECEC 1
        , sidebarBg = hex 0xF4F4F5 1
        , canvasBg = hex 0xDDDDE0 1
        , controlBg = hex 0xFFFFFF 1
        , fieldBg = hex 0xFFFFFF 1
        , selectedSegment = hex 0xFFFFFF 1
        , label = (0, 0, 0, 0.85)
        , secondary = (0, 0, 0, 0.5)
        , tertiary = (0, 0, 0, 0.26)
        , separator = (0, 0, 0, 0.1)
        , fill = (0, 0, 0, 0.05)
        , fillStrong = (0, 0, 0, 0.1)
        , accentColour = hex 0x007AFF 1
        , rowSelected = hex 0xE3E3E6 1
        , shadow = (0, 0, 0, 0.14)
        , iconColour = "#3c3c43"
        }
dark =
    Palette
        { windowBg = hex 0x2A2A2C 1
        , sidebarBg = hex 0x232325 1
        , canvasBg = hex 0x161618 1
        , controlBg = hex 0x434346 1
        , fieldBg = hex 0x1F1F21 1
        , selectedSegment = hex 0x636366 1
        , label = (1, 1, 1, 0.86)
        , secondary = (1, 1, 1, 0.55)
        , tertiary = (1, 1, 1, 0.25)
        , separator = (1, 1, 1, 0.1)
        , fill = (1, 1, 1, 0.07)
        , fillStrong = (1, 1, 1, 0.13)
        , accentColour = hex 0x0A84FF 1
        , rowSelected = hex 0x323235 1
        , shadow = (0, 0, 0, 0.45)
        , iconColour = "#dcdce0"
        }

hex :: Int -> Double -> Rgba
hex n a = (fromIntegral (n `div` 0x10000) / 255, fromIntegral (n `div` 0x100 `mod` 0x100) / 255, fromIntegral (n `mod` 0x100) / 255, a)

{-# NOINLINE current #-}
current :: IORef (Appearance, Palette)
current = unsafePerformIO (newIORef (Dark, dark))

-- | The appearance chosen at start.
appearance :: IO Appearance
appearance = fst <$> readIORef current

-- | What Cairo draws with: one of the palette's colours.
data Token = Workspace | Ground | Surface | Raised | Hairline | TextColour | DimText | Accent | CelFill | CelEmpty | Paper | PaperShadow

workspace, ground, surface, raised, textColour, dimText, accent, celFill, celEmpty, paper, paperShadow :: Token
workspace = Workspace
ground = Ground
surface = Surface
raised = Raised
textColour = TextColour
dimText = DimText
accent = Accent
celFill = CelFill
celEmpty = CelEmpty
paper = Paper
paperShadow = PaperShadow

-- | Borders, and the accent as a selection's wash, at the opacity they are drawn with.
hairline, accentSoft :: (Token, Double)
hairline = (Hairline, 1)
accentSoft = (Accent, 0.16)

colourOf :: Palette -> Token -> Rgba
colourOf p = \case
    Workspace -> p.canvasBg
    Ground -> p.windowBg
    Surface -> p.sidebarBg
    Raised -> p.rowSelected
    Hairline -> p.separator
    TextColour -> p.label
    DimText -> p.secondary
    Accent -> p.accentColour
    CelFill -> p.tertiary
    CelEmpty -> p.separator
    Paper -> (1, 1, 1, 1)
    PaperShadow -> p.shadow

rgb :: Token -> C.Render ()
rgb t = rgba (t, 1)

rgba :: (Token, Double) -> C.Render ()
rgba (t, k) = do
    (_, p) <- C.liftIO (readIORef current)
    let
        (r, g, b, a) = colourOf p t
    C.setSourceRGBA r g b (a * k)

-- | A colour as CSS has it.
cssColour :: Rgba -> Text
cssColour (r, g, b, a)
    | a >= 1 = "#" <> byte r <> byte g <> byte b
    | otherwise = "rgba(" <> Text.intercalate "," [num r, num g, num b, Text.pack (show a)] <> ")"
    where
        byte v = let n = max 0 (min 255 (round (v * 255) :: Int)) in Text.pack ((if n < 16 then "0" else "") <> showHex n "")
        num v = Text.pack (show (max 0 (min 255 (round (v * 255) :: Int))))

-- | The stylesheet: macOS's controls, in the palette's colours.
css :: Appearance -> Palette -> Text
css look p =
    Text.unlines
        [ "* { font-family: \"System Font\", \".SF NS\", \"Helvetica Neue\", sans-serif; font-size: 13px; text-shadow: none; -gtk-icon-shadow: none; outline: none; }"
        , "window, .background, stack, box, label { color: " <> c p.label <> "; }"
        , "window, .background { background-color: " <> c p.windowBg <> "; }"
        , -- The unified toolbar: title and tools in one bar, the window buttons on the left.
          "headerbar { background: " <> c p.windowBg <> "; background-image: none; border: none; border-bottom: 1px solid " <> c p.separator <> "; box-shadow: none; min-height: 44px; padding: 0 10px; }"
        , "headerbar .title { font-weight: 700; font-size: 13px; color: " <> c p.label <> "; }"
        , "headerbar .subtitle { font-size: 11px; color: " <> c p.secondary <> "; }"
        , "headerbar button.titlebutton { min-width: 12px; min-height: 12px; padding: 0; margin: 0 8px 0 0; border-radius: 50%; border: 1px solid rgba(0,0,0,0.12); background-image: none; box-shadow: none; }"
        , "headerbar button.titlebutton image { opacity: 0; -gtk-icon-transform: scale(0.6); color: rgba(0,0,0,0.6); }"
        , "headerbar:hover button.titlebutton image { opacity: 1; }"
        , "headerbar button.titlebutton.close { background-color: #ff5f57; }"
        , "headerbar button.titlebutton.minimize { background-color: #febc2e; }"
        , "headerbar button.titlebutton.maximize { background-color: #28c840; }"
        , "headerbar:backdrop button.titlebutton { background-color: " <> c p.tertiary <> "; border-color: transparent; }"
        , "headerbar:backdrop .title, headerbar:backdrop button { opacity: 0.7; }"
        , "decoration { border-radius: 10px; box-shadow: 0 10px 40px " <> c p.shadow <> ", 0 0 0 1px " <> c p.separator <> "; }"
        , "headerbar.titlebar, .titlebar { border-radius: 10px 10px 0 0; }"
        , -- Buttons: borderless in toolbars, bezelled where they act.
          "button { background: transparent; background-image: none; border: none; box-shadow: none; border-radius: 6px; color: " <> c p.label <> "; padding: 3px 8px; min-height: 22px; transition: background-color 90ms; }"
        , "button:hover { background-color: " <> c p.fill <> "; }"
        , "button:active { background-color: " <> c p.fillStrong <> "; }"
        , "button:checked { background-color: " <> c p.fillStrong <> "; color: " <> c p.accentColour <> "; }"
        , "button:disabled { opacity: 0.4; }"
        , "button.icon { padding: 4px; min-width: 26px; min-height: 26px; }"
        , "button.primary, dialog .dialog-action-area button:last-child { background-color: " <> c p.accentColour <> "; color: #ffffff; font-weight: 500; box-shadow: 0 1px 1px rgba(0,0,0,0.12); }"
        , "button.primary:hover, dialog .dialog-action-area button:last-child:hover { background-color: " <> c (shade p.accentColour 1.08) <> "; }"
        , "button.primary label, dialog .dialog-action-area button:last-child label { color: #ffffff; }"
        , "dialog .dialog-action-area button, combobox button, colorbutton, .bezel { background-color: " <> c p.controlBg <> "; box-shadow: 0 0 0 0.5px " <> c p.separator <> ", 0 1px 1px " <> c (shadowSoft look) <> "; padding: 2px 10px; min-height: 22px; }"
        , "dialog .dialog-action-area button:hover, combobox button:hover, colorbutton:hover { background-color: " <> c (shade p.controlBg (if look == Dark then 1.12 else 0.97)) <> "; }"
        , "combobox button { padding: 1px 6px 1px 10px; }"
        , "combobox arrow { min-width: 12px; color: " <> c p.secondary <> "; }"
        , "dialog .dialog-action-area { padding: 12px 16px 16px 16px; }"
        , "dialog .dialog-action-area button { min-width: 76px; }"
        , -- The tool palette, the accessory bar and the inspector.
          ".rail { background: " <> c p.sidebarBg <> "; border-right: 1px solid " <> c p.separator <> "; padding: 8px 6px; }"
        , ".rail button { min-width: 32px; min-height: 32px; padding: 0; border-radius: 7px; }"
        , ".rail button:checked { background-color: " <> c (withAlpha p.accentColour 0.18) <> "; }"
        , ".bar { background: " <> c p.windowBg <> "; border-bottom: 1px solid " <> c p.separator <> "; padding: 4px 10px; min-height: 34px; }"
        , ".transport { background: " <> c p.windowBg <> "; border-top: 1px solid " <> c p.separator <> "; border-bottom: 1px solid " <> c p.separator <> "; padding: 4px 10px; }"
        , ".sidebar { background: " <> c p.sidebarBg <> "; border-left: 1px solid " <> c p.separator <> "; }"
        , ".sidebar .heading { font-weight: 700; padding: 12px 14px 4px 14px; }"
        , ".dim, .dim label { color: " <> c p.secondary <> "; font-size: 12px; }"
        , ".counter { color: " <> c p.secondary <> "; font-size: 12px; font-feature-settings: \"tnum\"; min-width: 56px; }"
        , ".sep { background: " <> c p.separator <> "; min-width: 1px; min-height: 18px; margin: 0 6px; }"
        , -- Segmented controls (and the sidebar's switcher), as AppKit draws them.
          ".segmented, stackswitcher { background: " <> c p.fill <> "; border-radius: 7px; padding: 1px; }"
        , ".segmented button, stackswitcher button { min-height: 22px; min-width: 24px; padding: 1px 6px; border-radius: 6px; font-size: 12px; color: " <> c p.label <> "; background: transparent; }"
        , "stackswitcher button { padding: 1px 16px; }"
        , ".segmented button:checked, stackswitcher button:checked { background-color: " <> c p.selectedSegment <> "; color: " <> c p.label <> "; box-shadow: 0 0 0 0.5px " <> c p.separator <> ", 0 1px 2px " <> c (shadowSoft look) <> "; }"
        , ".segmented button:checked image { opacity: 1; }"
        , -- Sliders: a thin track filled with the accent, a round white knob.
          "scale { padding: 0 4px; }"
        , "scale trough { background: " <> c p.fillStrong <> "; border: none; min-height: 4px; border-radius: 2px; }"
        , "scale highlight { background: " <> c p.accentColour <> "; border: none; border-radius: 2px; }"
        , "scale slider { background: #ffffff; border: none; box-shadow: 0 0 0 0.5px rgba(0,0,0,0.15), 0 1px 3px rgba(0,0,0,0.3); min-width: 16px; min-height: 16px; margin: -7px; border-radius: 50%; }"
        , "scale value { color: " <> c p.secondary <> "; font-size: 11px; min-width: 30px; margin-left: 8px; }"
        , -- Fields, with the focus ring.
          "entry, .composer-box { background: " <> c p.fieldBg <> "; border: none; border-radius: 6px; color: " <> c p.label <> "; box-shadow: 0 0 0 0.5px " <> c p.fillStrong <> "; padding: 3px 8px; min-height: 22px; }"
        , "entry:focus { box-shadow: 0 0 0 3.5px " <> c (withAlpha p.accentColour 0.45) <> "; }"
        , ".composer, .composer text { background-color: " <> c p.fieldBg <> "; color: " <> c p.label <> "; }"
        , "checkbutton check { background: " <> c p.controlBg <> "; border: none; box-shadow: 0 0 0 0.5px " <> c p.fillStrong <> ", 0 1px 1px " <> c (shadowSoft look) <> "; border-radius: 4px; min-width: 14px; min-height: 14px; margin-right: 6px; }"
        , "checkbutton check:checked { background: " <> c p.accentColour <> "; color: #ffffff; box-shadow: none; }"
        , "colorbutton { padding: 3px; }"
        , "colorbutton colorswatch { border-radius: 4px; min-width: 22px; min-height: 16px; box-shadow: inset 0 0 0 1px rgba(0,0,0,0.12); }"
        , "paned > separator { background: " <> c p.separator <> "; min-width: 1px; background-image: none; }"
        , "scrollbar { background: transparent; border: none; }"
        , "scrollbar slider { background: " <> c p.tertiary <> "; border-radius: 4px; min-width: 7px; min-height: 7px; border: 2px solid transparent; }"
        , "tooltip { background: " <> c p.controlBg <> "; border-radius: 6px; box-shadow: 0 0 0 0.5px " <> c p.separator <> "; }"
        , "tooltip label { color: " <> c p.label <> "; font-size: 11px; }"
        , "popover { background: " <> c p.windowBg <> "; border-radius: 10px; border: none; box-shadow: 0 0 0 0.5px " <> c p.separator <> ", 0 8px 30px " <> c p.shadow <> "; padding: 4px; }"
        , "dialog { background: " <> c p.windowBg <> "; }"
        , -- The project list.
          "flowboxchild { padding: 0; }"
        , ".card { background: " <> c p.controlBg <> "; border-radius: 10px; padding: 0; box-shadow: 0 0 0 0.5px " <> c p.separator <> ", 0 1px 3px " <> c (shadowSoft look) <> "; }"
        , ".card:hover { background: " <> c p.controlBg <> "; box-shadow: 0 0 0 3px " <> c (withAlpha p.accentColour 0.35) <> "; }"
        , ".card .name { font-weight: 600; }"
        , ".card .meta { color: " <> c p.secondary <> "; font-size: 11px; }"
        , ".home-title { font-size: 26px; font-weight: 700; }"
        , -- The assistant and the board.
          ".msg-user { background: " <> c p.accentColour <> "; color: #ffffff; border-radius: 14px; padding: 7px 12px; }"
        , ".msg-assistant { padding: 0 2px; }"
        , ".msg-step, .msg-step label { color: " <> c p.secondary <> "; font-size: 12px; }"
        , ".msg-error { color: #ff453a; font-size: 12px; }"
        , ".msg-failed, .msg-failed label { color: #ff6961; font-size: 12px; }"
        , ".composer-bar { padding: 10px; border-top: 1px solid " <> c p.separator <> "; }"
        , ".board-top { padding: 6px 8px; border-bottom: 1px solid " <> c p.separator <> "; }"
        , ".chip { padding: 2px 10px; min-height: 22px; border-radius: 6px; color: " <> c p.secondary <> "; font-size: 12px; }"
        , ".chip.on { background-color: " <> c p.fillStrong <> "; color: " <> c p.label <> "; }"
        ]
    where
        c = cssColour
        shadowSoft l = if l == Dark then (0, 0, 0, 0.3) else (0, 0, 0, 0.08)
        withAlpha (r, g, b, _) a = (r, g, b, a)
        shade (r, g, b, a) k = (min 1 (r * k), min 1 (g * k), min 1 (b * k), a)

{- | The system's appearance: ANIMA2D_APPEARANCE (light or dark) if set; on macOS, its own
setting; elsewhere, dark.
-}
systemAppearance :: IO Appearance
systemAppearance = do
    wanted <- lookupEnv "ANIMA2D_APPEARANCE"
    case wanted of
        Just "light" -> pure Light
        Just "dark" -> pure Dark
        _
            | os == "darwin" -> do
                r <- try (readProcessWithExitCode "defaults" ["read", "-g", "AppleInterfaceStyle"] "")
                pure $ case r of
                    Right (_, out, _) | "Dark" `Text.isPrefixOf` Text.strip (Text.pack out) -> Dark
                    Right _ -> Light
                    Left (_ :: SomeException) -> Light
            | otherwise -> pure Dark

{- | The appearance, its stylesheet over the theme for every window, the theme's matching
variant under it, and the window buttons where macOS puts them.
-}
applyStyle :: IO ()
applyStyle = do
    look <- systemAppearance
    let
        p = if look == Dark then dark else light
    writeIORef current (look, p)
    settings <- Gtk.settingsGetDefault
    forM_ settings $ \s -> do
        Gtk.setSettingsGtkApplicationPreferDarkTheme s (look == Dark)
        Gtk.setSettingsGtkDecorationLayout s "close,minimize,maximize:"
    provider <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromData provider (Text.encodeUtf8 (css look p))
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

-- | Icon `name` as an image widget, in the appearance's icon colour.
iconImage :: Text -> Int -> IO Gtk.Image
iconImage name size = do
    (_, p) <- readIORef current
    pb <- iconPixbuf p.iconColour name size
    case pb of
        Nothing -> Gtk.imageNew
        Just px -> do
            surf <- Gdk.cairoSurfaceCreateFromPixbuf px 2 (Nothing :: Maybe Gdk.Window)
            Gtk.imageNewFromSurface (Just surf)

-- | Interface text at the current point (its top left), in the system font; its size back.
uiText :: Double -> Bool -> Text -> C.Render (Double, Double)
uiText size bold t = do
    ctx <- Connector.getContext
    C.liftIO $ do
        layout <- PangoCairo.createLayout ctx
        desc <- Pango.fontDescriptionFromString ((if bold then "System Font, Helvetica Neue, sans-serif Semi-Bold " else "System Font, Helvetica Neue, sans-serif ") <> Text.pack (show size) <> "px")
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
