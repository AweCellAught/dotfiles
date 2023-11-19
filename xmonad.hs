{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -O2 #-}

-- {-# OPTIONS_GHC -fllvm #-}
-- {-# OPTIONS_GHC -fobject-code #-}
-- {-# OPTIONS_GHC -funbox-strict-fields #-}
-- {-# OPTIONS_GHC -optc-O3 #-}
-- {-# OPTIONS_GHC -optc-ffast-math #-}
-- {-# OPTIONS_GHC -rtsopts #-}
-- {-# OPTIONS_GHC -threaded #-}

-- | PORTIONS OF CODE, COMMENTS, ETC. FROM THE FOLLOWING SOURCES ARE CURRENTLY IN USE BELOW AS OF WRITING;
--    OR HAVE OTHERWISE DIRECTLY, OR INDIRECTLY INFLUENCED THE FOLLOWING CONFIG:
--
-- https://github.com/altercation/dotfiles-tilingwm/blob/master/.xmonad/xmonad.hs
-- https://github.com/alternateved/nixos-config/blob/main/config/xmonad/xmonad.hs
-- https://github.com/arcolinux/arcolinux-xmonad-polybar/blob/master/etc/skel/.xmonad/xmonad.hs
-- https://github.com/byorgey/dotfiles/blob/master/xmonad.hs
-- https://github.com/byorgey/split/blob/master/src/Data/List/Split/Internals.hs
-- https://github.com/kwannoel/Xmonad-config/blob/master/xmonad.hs
-- https://github.com/lifer0se/dotfiles/blob/master/.config/xmonad/xmonad.hs
-- https://github.com/liskin/dotfiles/blob/home/.xmonad/XMonad/Actions/DoNotDisturb.hs
-- https://github.com/liskin/dotfiles/blob/home/.xmonad/XMonad/Hooks/LayoutHistory.hs
-- https://github.com/liskin/dotfiles/blob/home/.xmonad/XMonad/Util/My.hs
-- https://github.com/liskin/dotfiles/blob/home/.xmonad/xmonad.hs
-- https://github.com/liskin/xmonad-contrib/blob/4b315a82edbebf94daf7e0d2ecef4e65108cccbe/XMonad/Layout/Inspect.hs
-- https://github.com/liskin/xmonad-contrib/blob/4b315a82edbebf94daf7e0d2ecef4e65108cccbe/XMonad/Layout/SubLayouts.hs
-- https://github.com/nnoell/dotfiles/blob/master/v4.4/xmonad/xmonad.hs
-- https://gitlab.com/dwt1/dotfiles/-/blob/master/.xmonad/xmonad.hs
-- https://gitlab.com/slotThe/dotfiles/-/blob/master/xmonad/.config/xmonad/src/xmonad.hs
-- https://hub.darcs.net/rgm/config-xmonad/browse/lib/My/Launch.hs
-- https://wiki.haskell.org/Xmonad/Config_archive/Brent_Yorgey%27s_darcs_xmonad.hs
-- https://wiki.haskell.org/Xmonad/Config_archive/adamvo%27s_xmonad.hs
-- https://hackage.haskell.org/package/split-0.2.3.5/docs/src/Data.List.Split.Internals.html#splitOneOf
-- https://hackage.haskell.org/package/extra-1.7.14/docs/src/Data.List.Extra.html#zipFrom
module Main
  ( main,
  )
where

import qualified Codec.Binary.UTF8.String as UTF8 (encode)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad
  ( join,
    liftM2,
    unless,
    when,
  )
import Data.Bits (Bits ((.|.)))
import Data.Char (isSpace)
import Data.IORef
  ( newIORef,
    readIORef,
    writeIORef,
  )
import qualified Data.Map as M
import qualified Data.Map.Strict as StrictMap
import Data.Maybe (isNothing, listToMaybe)
import Data.Monoid
  ( All (All),
    Any (Any),
    Endo (appEndo),
  )
import Data.Ratio ((%))
import Foreign.C (CChar)
import GHC.Exts
  ( IsList (fromList),
    IsString,
  )
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Types
import Graphics.X11.Types
  ( Button,
    KeyMask,
    KeySym,
    button1,
    button2,
    button3,
    button4,
    button5,
    controlMask,
    mod1Mask,
    mod3Mask,
    mod4Mask,
    mod5Mask,
    shiftMask,
    xK_0,
    xK_1,
    xK_2,
    xK_3,
    xK_4,
    xK_5,
    xK_6,
    xK_7,
    xK_8,
    xK_9,
    xK_BackSpace,
    xK_Delete,
    xK_Down,
    xK_End,
    xK_Escape,
    xK_F1,
    xK_F12,
    xK_Home,
    xK_Insert,
    xK_KP_Add,
    xK_KP_Divide,
    xK_KP_Subtract,
    xK_Left,
    xK_Page_Down,
    xK_Page_Up,
    xK_Pause,
    xK_Print,
    xK_Return,
    xK_Right,
    xK_Tab,
    xK_Up,
    xK_a,
    xK_apostrophe,
    xK_b,
    xK_backslash,
    xK_bracketleft,
    xK_bracketright,
    xK_c,
    xK_comma,
    xK_d,
    xK_e,
    xK_equal,
    xK_f,
    xK_g,
    xK_grave,
    xK_h,
    xK_i,
    xK_j,
    xK_k,
    xK_l,
    xK_m,
    xK_minus,
    xK_o,
    xK_p,
    xK_period,
    xK_q,
    xK_r,
    xK_s,
    xK_semicolon,
    xK_slash,
    xK_space,
    xK_t,
    xK_u,
    xK_w,
    xK_x,
    xK_y,
    xK_z,
  )
import System.Directory (getCurrentDirectory)
import System.Environment (getEnv)
import System.Exit (exitSuccess)
import System.IO (hClose)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Signals
  ( sigTERM,
    signalProcessGroup,
  )
import System.Posix.Types (ProcessID)
import XMonad
import XMonad
  ( ChangeLayout (FirstLayout, NextLayout),
    Default (def),
    Event (ConfigureRequestEvent, ev_above, ev_detail, ev_value_mask, ev_window),
    ExtensionClass (..),
    Full (Full),
    IncMasterN (IncMasterN),
    Layout,
    ManageHook,
    MonadIO,
    MonadState,
    Query,
    Rectangle (rect_height, rect_width, rect_x, rect_y),
    Resize (Expand, Shrink),
    ScreenDetail (screenRect),
    StateExtension (PersistentExtension),
    Window,
    WindowChanges (WindowChanges, wc_border_width, wc_height, wc_sibling, wc_stack_mode, wc_width, wc_x, wc_y),
    WindowSet,
    WorkspaceId,
    X,
    XConf (config),
    XConfig (..),
    XState (windowset),
    appName,
    asks,
    className,
    composeAll,
    configureWindow,
    doF,
    doFloat,
    doIgnore,
    focus,
    gets,
    io,
    kill,
    modify,
    mouseMoveWindow,
    mouseResizeWindow,
    refresh,
    resource,
    runQuery,
    scaleRationalRect,
    screenWorkspace,
    sendMessage,
    setLayout,
    spawn,
    stringProperty,
    title,
    whenJust,
    windows,
    windowset,
    withDisplay,
    withFocused,
    withWindowSet,
    xmonad,
    (-->),
    (<&&>),
    (<+>),
    (<||>),
    (=?),
    (|||),
  )
import XMonad.Actions.CopyWindow (copyWindow, kill1)
import XMonad.Actions.CycleWS
  ( Direction1D (Next, Prev),
    WSType (WSIs),
    moveTo,
    nextScreen,
    nextWS,
    prevScreen,
    prevWS,
    shiftTo,
    toggleWS,
  )
import XMonad.Actions.EasyMotion
  ( ChordKeys (PerScreenKeys),
    EasyMotionConfig (borderCol, borderPx, cancelKey, emFont, sKeys, txtCol),
    selectWindow,
  )
import XMonad.Actions.FindEmptyWorkspace
  ( tagToEmptyWorkspace,
    viewEmptyWorkspace,
  )
import qualified XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Actions.GridSelect
  ( GSConfig (gs_cellheight, gs_cellpadding, gs_cellwidth, gs_font, gs_originFractX, gs_originFractY),
    bringSelected,
    buildDefaultGSConfig,
    colorRangeFromClassName,
    goToSelected,
  )
import XMonad.Actions.GroupNavigation
  ( Direction (Backward, Forward),
    nextMatchWithThis,
  )
import XMonad.Actions.Launcher
  ( LauncherConfig (..),
    defaultLauncherModes,
    launcherPrompt,
  )
import XMonad.Actions.Minimize
  ( maximizeWindow,
    minimizeWindow,
    withLastMinimized,
  )
import XMonad.Actions.MouseResize (mouseResize)
import XMonad.Actions.Navigation2D
  ( Direction2D (D, L, R, U),
    additionalNav2DKeys,
    windowGo,
    windowSwap,
    withNavigation2DConfig,
  )
import XMonad.Actions.PhysicalScreens (PhysicalScreen, getScreen)
import XMonad.Actions.Plane
import XMonad.Actions.Prefix
  ( PrefixArgument (Raw),
    usePrefixArgument,
    withPrefixArgument,
  )
import XMonad.Actions.Promote (promote)
import XMonad.Actions.RotSlaves
  ( rotAllDown,
    rotAllUp,
    rotSlavesDown,
    rotSlavesUp,
  )
import qualified XMonad.Actions.Search as S
import XMonad.Actions.Sift (siftDown, siftUp)
import XMonad.Actions.SpawnOn (manageSpawn)
import qualified XMonad.Actions.Submap as SM
import XMonad.Actions.SwapWorkspaces (swapTo, swapWithCurrent)
import XMonad.Actions.TagWindows
  ( addTag,
    delTag,
    focusDownTaggedGlobal,
    focusUpTaggedGlobal,
    getTags,
    shiftHere,
    unTag,
    withTaggedGlobalP,
  )
import XMonad.Actions.TopicSpace
  ( Topic,
    TopicConfig (defaultTopic, defaultTopicAction, topicActions, topicDirs),
    TopicItem,
    currentTopicAction,
    inHome,
    switchTopic,
    tiActions,
    tiDirs,
    topicNames,
  )
import XMonad.Actions.UpdateFocus
  ( adjustEventInput,
    focusOnMouseMove,
  )
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.Warp
  ( warpToScreen,
    warpToWindow,
  )
import XMonad.Actions.WithAll (killAll)
import qualified XMonad.Actions.WorkspaceNames as WN
import XMonad.Hooks.CurrentWorkspaceOnTop (currentWorkspaceOnTop)
import XMonad.Hooks.DebugKeyEvents (debugKeyEvents)
import XMonad.Hooks.DynamicLog
  ( PP (ppCurrent, ppExtras, ppHidden, ppLayout, ppOrder, ppOutput, ppSep, ppSort, ppTitle, ppUrgent, ppVisible),
    dynamicLogWithPP,
    wrap,
    xmobarColor,
    xmobarPP,
    xmobarProp,
  )
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.InsertPosition
  ( Focus (Newer),
    Position (Below),
    insertPosition,
  )
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageDocks
  ( Direction1D (Prev),
    avoidStruts,
    docks,
  )
import XMonad.Hooks.ManageHelpers
  ( doCenterFloat,
    doFloatDep,
    doFullFloat,
    isDialog,
    isFullscreen,
    (-?>),
  )
import XMonad.Hooks.Minimize (minimizeEventHook)
import XMonad.Hooks.RefocusLast
  ( isFloat,
    refocusLastLayoutHook,
  )
import XMonad.Hooks.Rescreen
import XMonad.Hooks.ServerMode (serverModeEventHook)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar (statusBarProp, withEasySB)
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing (swallowEventHookSub)
import XMonad.Layout.AvoidFloats
  ( AvoidFloatItemMsg (AvoidFloatToggleItem),
    AvoidFloatMsg (AvoidFloatClearItems, AvoidFloatSet, AvoidFloatToggle),
    avoidFloats,
  )
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Layout.Decoration
  ( Shrinker (..),
    Theme (activeBorderColor, activeBorderWidth, activeColor, decoHeight, inactiveBorderColor, inactiveBorderWidth, inactiveColor, urgentBorderColor, urgentBorderWidth, urgentColor),
    fi,
  )
import XMonad.Layout.Gaps (gaps)
import XMonad.Layout.IfMax (IfMax (IfMax))
import XMonad.Layout.LayoutHints
  ( layoutHints,
    layoutHintsWithPlacement,
  )
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Magnifier
  ( MagnifyThis (NoMaster),
    magnify,
  )
import XMonad.Layout.Maximize (maximize, maximizeRestore)
import XMonad.Layout.Minimize (minimize)
import XMonad.Layout.MosaicAlt
  ( MosaicAlt (MosaicAlt),
    expandWindowAlt,
    resetAlt,
    shrinkWindowAlt,
    tallWindowAlt,
    wideWindowAlt,
  )
import XMonad.Layout.MultiToggle
  ( mkToggle,
    mkToggle1,
    single,
  )
import qualified XMonad.Layout.MultiToggle as MT (Toggle (..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Reflect
  ( REFLECTX (REFLECTX),
    REFLECTY (REFLECTY),
  )
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.ResizableThreeColumns
  ( MirrorResize (MirrorExpand, MirrorShrink),
    ResizableThreeCol (ResizableThreeCol, ResizableThreeColMid),
  )
import XMonad.Layout.ResizableTile (ResizableTall (ResizableTall))
import XMonad.Layout.Simplest (Simplest (..))
import XMonad.Layout.Spacing
  ( Border (Border),
    decScreenSpacing,
    decWindowSpacing,
    incScreenSpacing,
    incWindowSpacing,
    smartSpacingWithEdge,
    spacingRaw,
  )
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.SubLayouts
  ( GroupMsg (MergeAll, UnMerge),
    mergeDir,
    pullGroup,
    subLayout,
  )
import XMonad.Layout.Tabbed (addTabs)
import XMonad.Layout.TrackFloating (trackFloating)
import XMonad.Layout.WindowArranger (windowArrange)
import XMonad.Layout.WindowNavigation (windowNavigation)
import XMonad.Prelude
import XMonad.Prompt (XPConfig)
import qualified XMonad.Prompt as P
import XMonad.Prompt.AppLauncher as AL (launchApp)
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)
import XMonad.Prompt.Window
  ( WindowPrompt (Bring, Goto),
    allWindows,
    windowPrompt,
  )
import XMonad.Prompt.Workspace (workspacePrompt)
import qualified XMonad.StackSet as W
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.EZConfig (additionalKeysP)
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.Util.Hacks as Hacks
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS),
    customFloating,
    namedScratchpadAction,
    namedScratchpadManageHook,
  )
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.Util.PureX as PX
import XMonad.Util.Run
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Ungrab (unGrab)
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import Prelude

zipFrom :: (Enum a) => a -> [b] -> [(a, b)]
zipFrom = zip . enumFrom

splitOneOf :: (Foldable t1, Foldable t2, Eq a) => t2 a -> t1 a -> [[a]]
splitOneOf separators =
  foldr
    ( \x acc ->
        if x `elem` separators
          then [] : acc
          else (x : head acc) : tail acc
    )
    [[]]

altMask :: KeyMask
altMask = mod1Mask

chatterino :: String
chatterino = "chatterino"

colorAmber, colorAqua :: String
colorAmber :: String = "#ffbf00"
colorAqua :: String = "#00ffff" ----- Aqua; inverse of Red

colorBlack, colorBlue :: String
colorBlack :: String = "#000000" -- BLACK
colorBlue :: String = "#0000ff" ----- True Blue; inverse of True Yellow

colorGray, colorGreen :: String
colorGray :: String = "#808080" --- GRAY
colorGreen :: String = "#00ff00" ---- Green; inverse of Magenta

colorMagenta :: String
colorMagenta :: String = "#ff00ff" -- Magenta; inverse of Green

colorRed :: String
colorRed :: String = "#ff0000" ------ Red; inverse of Cyan

colorTrueYellow :: String
colorTrueYellow :: String = "#ffff00"

colorWhite :: String
colorWhite :: String = "#ffffff" -- WHITE

colorYarg :: String
colorYarg :: String = "#CFCFCF" -- YARG

data Color
  = -- | TODO!!! FIXME!!!
    Amber
  | Aqua
  | Background
  | Blue
  | Comment
  | Foreground
  | Green
  | Magenta
  | Red
  | TrueYellow

colorHex :: Main.Color -> String
colorHex -- Converts a color into a hexidecimal string with a leading \'#\'
  =
  \case
    Amber -> colorAmber
    Aqua -> colorAqua
    Background -> colorBlack
    Blue -> colorBlue
    Comment -> colorGray
    Foreground -> colorWhite
    Green -> colorGreen
    Magenta -> colorMagenta
    Red -> colorRed
    TrueYellow -> colorTrueYellow

colorXmobar :: Main.Color -> String -> String
colorXmobar fg = xmobarColor (colorHex fg) "" -- \| Use xmobar escape codes to output a string with the given foreground color.

copyCat ::
  (Eq a, Eq i, Eq s) => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
copyCat n s
  | Just w <- W.peek s = copyWindow w n s
  | otherwise = s

curDirToWorkspacename :: X ()
curDirToWorkspacename = do
  name <- WN.getCurrentWorkspaceName
  when (isNothing name) $ do
    dir <- io getCurrentDirectory
    when (dir /= yHome) . WN.setCurrentWorkspaceName . last $ splitOneOf ("/" :: String) dir -- Explicitly tell the compiler that "/" should be a String, hence resolving the ambiguity.

discord :: String
discord = "discord"

emConf :: EasyMotionConfig
emConf =
  def
    { sKeys =
        PerScreenKeys $
          StrictMap.fromList
            [(0, topRowNumKeysPlusBrackets), (1, [xK_F1 .. xK_F12])],
      cancelKey = xK_Escape,
      emFont = myFontHuge,
      txtCol = colorGreen,
      borderPx = 4,
      borderCol = colorBlue
    }

floatConfReqHook :: Query (Maybe (Endo WindowSet)) -> Event -> X All
floatConfReqHook mh ev@ConfigureRequestEvent {ev_window = w} =
  runQuery (join <$> (isFloat -?> mh)) w >>= \case
    Nothing -> mempty
    Just e -> do
      windows (appEndo e)
      sendConfWindow -- if still floating, send ConfigureWindow
      pure (All False)
  where
    sendConfWindow =
      withWindowSet $ \ws ->
        whenJust (M.lookup w (W.floating ws)) $ \fr ->
          whenJust (findScreenRect ws) (confWindow fr)
    findScreenRect ws =
      listToMaybe
        [ screenRect (W.screenDetail s)
          | s <- W.current ws : W.visible ws,
            w `elem` W.integrate' (W.stack (W.workspace s))
        ]

    confWindow fr sr =
      withDisplay $ \dpy -> do
        let r = scaleRationalRect sr fr
        bw <- asks (XMonad.borderWidth . config)
        io
          . configureWindow dpy w (ev_value_mask ev)
          $ WindowChanges
            { wc_x = fi $ rect_x r,
              wc_y = fi $ rect_y r,
              wc_width = fi $ rect_width r,
              wc_height = fi $ rect_height r,
              wc_border_width = fromIntegral bw,
              wc_sibling = ev_above ev,
              wc_stack_mode = ev_detail ev
            }
floatConfReqHook _ _ = mempty

focusNthScreen,
  focusNthScreenLOL,
  focusNthScreenWUT ::
    PhysicalScreen -> Main.FocusMode -> X ()
focusNthScreen n mode = do
  screen <- getScreen def n
  ws <-
    case screen of
      Just s -> screenWorkspace s
      Nothing -> pure mempty
  whenJust ws $
    PX.defile
      . case mode of
        Greedy -> PX.greedyView
        Normal -> PX.view
focusNthScreenLOL n mode = do
  screen <- getScreen def n
  ws <-
    case screen of
      Just s -> screenWorkspace s
      Nothing -> pure mempty
  whenJust ws $
    PX.defile
      . case mode of
        Greedy -> PX.shift <> PX.greedyView
        Normal -> PX.view
focusNthScreenWUT n mode = do
  screen <- getScreen def n
  ws <-
    case screen of
      Just s -> screenWorkspace s
      Nothing -> pure mempty
  whenJust ws $
    PX.defile
      . case mode of
        Greedy -> viewWith copyCat
        Normal -> PX.view

data FocusMode
  = Greedy
  | Normal

gimp, googleMaps, hexchat :: String
gimp = "gimp"
googleMaps =
  "chromium --profile-directory=Default --app-id=mnhkaebcjjhencmpkapnbdaogjamfbcj"
hexchat = "hexchat"

hyperMask :: KeyMask
hyperMask = mod3Mask

-- hyperMask = lockMask

keyBindings :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keyBindings XConfig {..} =
  M.fromList $
    -- \||| KILL COMMANDS |||
    [ ((modMask .|. shiftMask .|. controlMask, xK_minus), kill), -- Closes all copies of currently focused window, and only of that window.
      ((modMask .|. shiftMask .|. altMask, xK_minus), killAll), --- Closes all windows on current workspace; only those windows, and only those copies.
      ((modMask .|. shiftMask, xK_minus), kill1), ----------------- Closes currently focused copy of window, and only that copy.
      ((modMask, xK_Pause), spawn "xkill"),
      -- \||| REFRESHES |||
      ((modMask, xK_i), refresh >> warpUp), -- HACK: redraw windows in order to force the log hook to update, warp cursor.
      ( (modMask .|. shiftMask, xK_r),
        spawn "xmonad --recompile; xmonad --restart" -- Recompiles, restarts xmonad.
      ),
      ((modMask, xK_w), toggleWarp),
      -- \| Sinks
      ((0, xK_Pause), withFocused (windows . W.sink) >> warpUp), -- Pushes focused window back into tiling.
      ( (altMask, xK_Pause),
        withFocused (windows . W.sink) >> warpUp ------- Pushes all windows on current workspace back into tiling.
      ),
      -- \| Easy Motion
      ( (modMask .|. altMask, xK_grave),
        selectWindow emConf >>= (`whenJust` windows . W.focusWindow)
      ),
      ( (modMask .|. lvl3Mask, xK_grave),
        selectWindow emConf >>= (`whenJust` windows . W.focusWindow)
      ),
      -- \| Window Prompt
      ((modMask .|. shiftMask, xK_g), windowPrompt def Goto allWindows),
      ((modMask .|. shiftMask, xK_b), windowPrompt def Bring allWindows),
      ((modMask .|. altMask, xK_f), nextMatchWithThis Forward className),
      ((modMask .|. altMask, xK_b), nextMatchWithThis Backward className),
      -- \| Terminals
      ((modMask, xK_Return), spawnExec terminal), -------------- Default terminal (Main)
      ((modMask .|. shiftMask, xK_Return), spawn myOtherTerminal), -- myOtherTerminal

      -- \| Commonly Used Spawns
      ((modMask, xK_x), spawnExec chromium), ---------------------- Brave
      ((modMask, xK_y), spawnExec yEditor), ----------------------- myEditor
      ((modMask, xK_t), spawnExec yFileManager), ------------------ myFileManager
      ((modMask .|. altMask, xK_t), spawnExec myRootFileManager), -- myRootFileManager
      ((modMask, xK_d), spawnExec discord), ----- Discord
      ((modMask, xK_m), spawnExec ytMusic), ----- YouTube Music

      -- \| Various rofi tools, Grid Select, etc.
      ((modMask, xK_semicolon), spawn rofiDrun), -- rofi, "run" mode; a prompt, where entries therein are named according the programs themselves--as their names would occur in a terminal, for example.
      ((modMask, xK_q), spawn rofiRun), ----------- rofi, "drun" mode; a prompt, where entries therein use the apps' respective names as they would appear on a normal menu.
      ((modMask, xK_KP_Insert), goToSelected mygridConfig), ----- Grid Select; go to,
      ((modMask, xK_KP_Delete), bringSelected mygridConfig), -- bring to.

      -- \| Other Spawns
      ((modMask .|. altMask, xK_y), unGrab >> spawn youTube), --- YouTube
      ((modMask .|. altMask, xK_m), unGrab >> spawn messages), ------- Messages App (desktop app for Google Messages; for SMS on desktop via link to phone)
      ((modMask .|. lvl3Mask, xK_y), unGrab >> spawn youTube), --- YouTube
      ((modMask .|. lvl3Mask, xK_m), unGrab >> spawn messages), ------- Messages App (desktop app for Google Messages; for SMS on desktop via link to phone)
      ((hyperMask, xK_m), unGrab >> spawn googleMaps), ------- Google Maps
      ((0, xK_Print), spawn "scrot"), --------------- Spawns scrot (in order to take a screenshot (of everything on any/all screen(s))).
      ((hyperMask, xK_Print), withPrefixArgument takeScreenshot), -- Spawns scrot: click, hold and drag mouse to select area; release to screenshot.
      ((controlMask, xK_Print), spawn "scrot -s"), -- Spawns scrot: click, hold and drag mouse to select area; release to screenshot.
      ((modMask .|. altMask, xK_space), currentTopicAction topicConfig),
      ((modMask .|. lvl3Mask, xK_space), currentTopicAction topicConfig),
      ((modMask, xK_slash), SM.submap . searchEngineMap $ S.promptSearch xpConfig),
      ((modMask .|. shiftMask, xK_f), SM.submap $ searchEngineMap S.selectSearch),
      ((hyperMask, xK_Pause), sendMessage AvoidFloatToggle),
      ( (hyperMask .|. shiftMask, xK_Pause),
        withFocused $ sendMessage . AvoidFloatToggleItem
      ),
      -- \| CONTROL + SHIFT KEYS
      ((controlMask .|. shiftMask, xK_Escape), spawn "xfce4-taskmanager"),
      -- \| PROMOTE
      ((modMask .|. altMask, xK_k), promote),
      -- \| WARP
      ((modMask, xK_z), warpToWindow (1 % 2) (1 % 2)), -- \| MOVE POINTER TO CURRENTLY FOCUSED WINDOW
      ((0, xF86XK_AudioRaiseVolume), spawnExec "~/src/shell/media.sh volume up"),
      ((0, xF86XK_AudioLowerVolume), spawnExec "~/src/shell/media.sh volume down"),
      ((0, xF86XK_AudioStop), spawnExec "~/src/shell/media.sh stop"),
      ((0, xK_F16), spawnExec "~/src/shell/media.sh mute"),
      -- \| A baby seal walks into a club:
      ( (modMask .|. shiftMask, xK_Escape),
        confirmPrompt
          prompt
          "that, \"I, [insert name here], [DATA EXPUNGED] ...lol!\""
          $ io exitSuccess -- Quits xmonad.
      )
    ]
      ++ [ ( (m .|. modMask, key),
             screenWorkspace sc >>= flip whenJust (PX.defile . f)
           )
           | (key, sc) <- zip [xK_comma, xK_period] [0 ..],
             (f, m) <- [(PX.shift, shiftMask), (PX.shift <> PX.view, hyperMask)]
         ]
      ++ [ ((m, k), PX.defile f)
           | (i, k) <- zip workspaces [xK_F1 .. xK_F12] ++ zip (drop 12 workspaces) topRowNumKeysPlusBrackets,
             (f, m) <-
               [ (PX.shift i, modMask .|. shiftMask),
                 (PX.view i, modMask),
                 (PX.shift i <> PX.view i, modMask .|. hyperMask),
                 (viewWith copyCat i, modMask .|. controlMask .|. shiftMask)
               ]
         ]
      ++ [ ((modMask .|. controlMask, k), windows $ swapWithCurrent i)
           | (i, k) <- zip workspaces [xK_F1 .. xK_F12] ++ zip (drop 12 workspaces) topRowNumKeysPlusBrackets
         ]
      ++ [ ((m, k), PX.defile f)
           | (i, k) <- zip workspaces [xK_F1 .. xK_F12] ++ zip (drop 12 workspaces) topRowNumKeysPlusBrackets,
             (f, m) <- [(viewWith copyCat i, modMask .|. controlMask .|. shiftMask)]
         ]
      ++ [ ((m .|. modMask .|. altMask, k), windows $ f i)
           | (i, k) <- zip workspaces [xK_F1 .. xK_F12] ++ zip (drop 12 workspaces) topRowNumKeysPlusBrackets,
             (f, m) <- [(W.greedyView, 0)]
         ]
      ++ [ ((m .|. modMask .|. lvl3Mask, k), windows $ f i)
           | (i, k) <- zip workspaces [xK_F1 .. xK_F12] ++ zip (drop 12 workspaces) topRowNumKeysPlusBrackets,
             (f, m) <- [(W.greedyView, 0)]
         ]
      ++ [ ( (modMask .|. m, k),
             focusNthScreen
               i
               ( if greedy
                   then Greedy
                   else Normal
               )
           )
           | (i, k) <- zipFrom 0 [xK_comma, xK_period],
             (m, greedy) <- [(0, False), (altMask, True), (lvl3Mask, True)]
         ]
      ++ [ ( (modMask .|. m, k),
             focusNthScreenLOL
               i
               ( if greedier
                   then Greedy
                   else Normal
               )
           )
           | (i, k) <- zipFrom 0 [xK_comma, xK_period],
             (m, greedier) <-
               [ (0, False),
                 (altMask .|. controlMask, True),
                 (lvl3Mask .|. controlMask, True)
               ]
         ]
      ++ [ ( (modMask .|. m, k),
             focusNthScreenWUT
               i
               ( if greedierYet
                   then Greedy
                   else Normal
               )
           )
           | (i, k) <- zipFrom 0 [xK_comma, xK_period],
             (m, greedierYet) <-
               [ (0, False),
                 (altMask .|. shiftMask, True),
                 (lvl3Mask .|. shiftMask, True)
               ]
         ]
      ++ [ ((modMask, key), spawnExec ("~/src/shell/media.sh " ++ action))
           | (key, action) <-
               [ (xK_KP_Enter, "mute"),
                 (xK_KP_Down, "volume down"),
                 (xK_KP_Up, "volume up"),
                 (xK_KP_Left, "prev"),
                 (xK_KP_Right, "next"),
                 (xK_KP_Begin, "play")
               ]
         ]
      ++ [((modMask, xK_KP_Add), spawnExec "~/src/shell/media.sh play" >> spawnExec "~/src/shell/media.sh play")]
      ++ [ ((keyMask, keySym), function (Lines 4) Circular direction)
           | (keySym, direction) <- zip [xK_Left .. xK_Down] $ enumFrom ToLeft,
             (keyMask, function) <- [(modMask, planeMove), (shiftMask .|. modMask, planeShift)]
         ]
      ++ [ ((modMask, xK_e), windows W.focusDown), ----------------- Moves focus to next window.
           ((modMask, xK_o), windows W.focusUp), ------------------- Moves focus to previous window.
           ((modMask, xK_k), windows W.focusMaster), --------------- Moves focus to master window.
           ((modMask .|. shiftMask, xK_k), windows W.swapMaster), -- Swaps focused window and master window.
           ((modMask .|. shiftMask, xK_o), windows W.swapDown), ---- Swaps focused window with next window.
           ((modMask .|. shiftMask, xK_e), windows W.swapUp) ------ Swaps focused window with previous window.
         ]
      ++ [ ((modMask .|. shiftMask, xK_Tab), setLayout layoutHook), ------- Resets layouts on current workspace to default.
           ((modMask .|. controlMask, xK_Tab), sendMessage FirstLayout), -- Cycles immediately to first layout algorithm.
           ((modMask, xK_Tab), sendMessage NextLayout) ------------------- Rotates through available layout algorithms.
         ]
      ++ [ ((modMask, xK_u), sendMessage Expand), -------------------- Expands master area.
           ((modMask, xK_a), sendMessage Shrink), ------------------- Shrinks master area.
           ((modMask, xK_apostrophe), sendMessage (IncMasterN 1)), -- Increments number of windows in master area.
           ( (modMask .|. shiftMask, xK_apostrophe),
             sendMessage (IncMasterN (-1)) ------------------------- Decrements number of windows in master area.
           ),
           ((hyperMask, xK_a), withFocused (sendMessage . expandWindowAlt)),
           ((hyperMask, xK_u), withFocused (sendMessage . shrinkWindowAlt)),
           ((hyperMask, xK_o), withFocused (sendMessage . tallWindowAlt)),
           ((hyperMask, xK_e), withFocused (sendMessage . wideWindowAlt)),
           ((hyperMask, xK_space), sendMessage resetAlt)
         ]
      ++ [ ((altMask, xK_KP_Up), incWindowSpacing 4), -- Increases window spacing.
           ((altMask, xK_KP_Down), decWindowSpacing 4), --- Decreases window spacing.
           ((altMask, xK_KP_Right), incScreenSpacing 4), -- Increases screen spacing.
           ((altMask, xK_KP_Left), decScreenSpacing 4) -- Decreases screen spacing.
         ]
      ++ [ ((modMask, xK_backslash), rotSlavesDown), ------ Rotates all windows exlusive of master down, while maintaining focus.
           ((modMask, xK_equal), rotSlavesUp), ------------ Rotates all windows exlusive of master up, while maintaining focus.
           ((altMask, xK_Tab), rotAllDown), --------------- Rotates all windows down, while maintaining focus.
           ((altMask .|. shiftMask, xK_Tab), rotAllUp), --- Rotates all windows up, while maintaining focus.
           ((lvl3Mask, xK_Tab), rotAllDown), -------------- Rotates all windows down, while maintaining focus.
           ((lvl3Mask .|. shiftMask, xK_Tab), rotAllUp), -- Rotates all windows up, while maintaining focus.
           ((modMask .|. hyperMask, xK_j), windows siftDown),
           ((modMask .|. hyperMask, xK_k), windows siftUp)
         ]
      ++ [ ((modMask .|. altMask, xK_i), sendMessage $ MT.Toggle MIRROR),
           ((modMask .|. lvl3Mask, xK_i), sendMessage $ MT.Toggle MIRROR),
           ((modMask .|. controlMask, xK_x), sendMessage $ MT.Toggle REFLECTX),
           ((modMask .|. controlMask, xK_y), sendMessage $ MT.Toggle REFLECTY),
           ((modMask .|. shiftMask, xK_space), sendMessage (MT.Toggle NBFULL)), ------- Toggles noborder/full layout.
           ( (modMask .|. controlMask, xK_space),
             sendMessage (MT.Toggle NOBORDERS) -- Toggles borders.
           ),
           ((modMask, xK_grave), viewEmptyWorkspace),
           ((modMask .|. shiftMask, xK_p), tagToEmptyWorkspace)
         ]

launcherConfig :: LauncherConfig
launcherConfig =
  LauncherConfig
    { pathToHoogle = "/home/jesse/.cabal/bin/hoogle",
      browser = chromium
    }

lvl3Mask :: KeyMask
lvl3Mask = mod5Mask

messages :: String
messages =
  "chromium --profile-directory=Default --app-id=hpfldicfbfomlpcikngkocigghgafkph"

prompt :: XPConfig
prompt =
  def
    { P.fgColor = colorWhite,
      P.fgHLight = colorTrueYellow,
      P.bgColor = colorBlack,
      P.bgHLight = colorTrueYellow,
      P.font = myFontMeh,
      P.alwaysHighlight = True, -- Current best match
      P.height = 25,
      P.position = P.Top,
      P.promptBorderWidth = 0, -- Fit in with rest of config
      P.historySize = 50,
      P.historyFilter = P.deleteAllDuplicates,
      P.maxComplRows = Just 5, -- Max rows to show in completion window
      P.promptKeymap =
        mconcat
          [ fromList
              [ ( (controlMask, xK_w),
                  P.killWord' isSpace XMonad.Hooks.ManageDocks.Prev
                ),
                ((0, xK_Left), P.moveHistory W.focusUp'),
                ((0, xK_Right), P.moveHistory W.focusDown')
              ],
            P.vimLikeXPKeymap
          ],
      P.searchPredicate = fuzzyMatch,
      P.sorter = fuzzySort
    }

reddit :: S.SearchEngine
reddit = S.searchEngine "reddit" "https://old.reddit.com/r/"

rofiDrun, rofiRun :: String
rofiDrun = "rofi -show drun"
rofiRun = "rofi -show run"

searchEngineMap ::
  (Ord a1, Num a1) => (S.SearchEngine -> a2) -> M.Map (a1, KeySym) a2
searchEngineMap method =
  M.fromList
    [ ((0, xK_g), method S.google),
      ((0, xK_h), method S.hoogle),
      ((0, xK_w), method S.wikipedia),
      ((0, xK_i), method S.images),
      ((0, xK_m), method S.maps),
      ((0, xK_o), method S.openstreetmap),
      ((0, xK_s), method S.scholar),
      ((0, xK_d), method S.duckduckgo),
      ((0, xK_y), method S.youtube),
      ((0, xK_t), method S.thesaurus),
      ((0, xK_r), method reddit)
    ]

takeScreenshot :: PrefixArgument -> X ()
takeScreenshot =
  \case
    Raw 1 -> spawn "scrot -z -u" -- Focused window
    Raw 2 -> spawn "scrot -z" -- Entire screen
    -- The mouse movement via @xdotool@ is needed because otherwise,
    -- if unclutter is active, the pointer will remain hidden.  Uff.
    _ ->
      unGrab
        *> spawn "xdotool mousemove_relative -- -1 0"
        *> spawn "scrot -z -f -s"

tinkerCAD :: String
tinkerCAD =
  "chromium --profile-directory=Default --app-id=mhgangaopklbiocngjljdcchjfbbfjai"

topRowNumKeysPlusBrackets :: [KeySym]
topRowNumKeysPlusBrackets =
  [ xK_1,
    xK_2,
    xK_3,
    xK_4,
    xK_5,
    xK_6,
    xK_7,
    xK_8,
    xK_9,
    xK_0,
    xK_bracketleft,
    xK_bracketright
  ]

topicPrompt :: XPConfig
topicPrompt =
  prompt
    { P.autoComplete = Just 3000, -- Time is in memptys.
      P.historySize = 0 -- No history in the prompt.
    }

tHSK :: Topic
tHSK :: Topic = "<fn=1>\xf120</fn>"

viewWith ::
  (MonadState XState m) =>
  (WorkspaceId -> WindowSet -> WindowSet) ->
  WorkspaceId ->
  m Data.Monoid.Any
viewWith viewer tag = do
  itag <- curTag
  when' (tag /= itag) $ do
    modifyWindowSet' (viewer tag)
    Any . (tag ==) <$> curTag
  where
    when' b ma -- \| A 'when' that accepts a monoidal return value:
      =
      if b
        then ma
        else pure mempty
    curTag = W.tag <$> curWorkspace ---------------- | Get the current tag.
    curScreen = withWindowSet' (pure . W.current) -- \| Get the current screen.
    curWorkspace = W.workspace <$> curScreen ------- | Get the current workspace.
    withWindowSet' = (=<< gets windowset) ----------------------------------- | A generalisation of 'withWindowSet'.
    modifyWindowSet' f = modify $ \xs -> xs {windowset = f (windowset xs)} -- \| A generalisation of 'modifyWindowSet'.

xpConfig :: XPConfig
xpConfig =
  def
    { P.font = myFontHuge,
      P.bgColor = "#200000",
      P.fgColor = "#CFCFCF",
      P.height = 64,
      P.position = P.CenteredAt 0.5 0.5,
      P.promptBorderWidth = 2,
      P.showCompletionOnTab = True
    }

chromium, youTube, ytMusic :: String
youTube =
  "chromium --profile-directory=Default --app-id=agimnkijcaahngcdmfeangaknmldooml"
ytMusic =
  "chromium --profile-directory=Default --app-id=cinhimbnkkaeohfgghhklpknlkffjgod"
chromium = "chromium"

yColorizer :: XMonad.Window -> Bool -> X (String, String)
yColorizer =
  colorRangeFromClassName
    (0x22, 0x22, 0x22) -- Lowest inactive BG
    (0xdd, 0xdd, 0xdd) -- Highest inactive BG
    (0x00, 0x00, 0x00) -- Active BG
    (0x00, 0x00, 0x00) -- Inactive FG
    (0xff, 0x00, 0xff) -- Active FG

yEditor, yFileManager, myFontHuge, myFontMeh :: String
-- yEditor = "emacsclient -a '' -c "
-- yEditor = "emacs"
-- yFileManager = "doublecmd"

yEditor = "nohup wezterm start -- vim &"
yFileManager = "thunar"

yFloatCRMH :: Query (Maybe (Endo WindowSet))
yFloatCRMH -- \| That is: "yFloatConfReqManageHook"
  =
  composeAll
    [ className =? "Steam" -?> doFloat -- \| Prevents Steam from moving its floats to primary screen.
    ]

myFontHuge = "xft:B612:size=32"

myFontMeh = "xft:B612:size=8"

{-# NOINLINE yHome #-}
yHome :: String
yHome = unsafePerformIO $ getEnv "HOME"

-- New data type for matching properties
data MatchProperty = MatchTitle String | MatchAppName String | MatchResource String | MatchClassName String

-- Function to match a property in a Query
matchQuery :: MatchProperty -> Query Bool
matchQuery (MatchTitle s) = title =? s
matchQuery (MatchAppName s) = appName =? s
matchQuery (MatchResource s) = resource =? s
matchQuery (MatchClassName s) = className =? s

-- Creates a manage hook with specified shift and float rules
createManageHook :: [(MatchProperty, Int)] -> [(MatchProperty, Query (Endo WindowSet))] -> Query (Endo WindowSet)
createManageHook shifts floats =
  composeAll . concat $
    [ [isDialog --> doCenterFloat],
      [isFullscreen --> doFullFloat],
      createShiftHook shifts,
      createFloatHook floats,
      additionalHooks
    ]
  where
    additionalHooks =
      [ isBrowserDialog --> forceCenterFloat,
        isRole =? gtkFile --> forceCenterFloat,
        isDialog --> doCenterFloat,
        -- pure True --> tileBelow
        pure True --> insertPosition Master Newer 
      ]

-- Role query
isRole :: Query String
isRole = stringProperty "WM_WINDOW_ROLE"

-- Browser dialog query
isBrowserDialog :: Query Bool
isBrowserDialog = isDialog <&&> className =? chromiumClass

chromiumClass, gtkFile :: String
gtkFile = "GtkFileChooserDialog"
chromiumClass = "chromium-browser"

-- Shift hook creation
createShiftHook :: [(MatchProperty, Int)] -> [Query (Endo WindowSet)]
createShiftHook shifts =
  map
    ( \(prop, i) ->
        (matchQuery prop) --> doShiftAndGo (zSpaces !! i)
    )
    shifts

-- Float hook creation
createFloatHook :: [(MatchProperty, Query (Endo WindowSet))] -> [Query (Endo WindowSet)]
createFloatHook = map (\(prop, f) -> (matchQuery prop) --> f)

-- Action to shift and focus a window
doShiftAndGo :: String -> Query (Endo WindowSet)
doShiftAndGo i = doF $ liftM2 (.) W.view W.shift i

-- Shift rules with explicit property matching
shiftRules :: [(MatchProperty, Int)]
shiftRules =
  [ (MatchAppName "gimp", 4),
    (MatchClassName "Thunar", 8),
    (MatchTitle "Untitled - Chromium", 12),
    (MatchAppName "discord", 13),
    (MatchTitle "YouTube Music", 14),
    (MatchTitle "YouTube", 15)
  ]

-- Float rules with explicit property matching
floatRules :: [(MatchProperty, Query (Endo WindowSet))]
floatRules =
  [ (MatchClassName "Arandr", doCenterFloat),
    (MatchClassName "feh", doCenterFloat),
    (MatchClassName "mpv", doCenterFloat),
    (MatchClassName "confirm", doFloat),
    (MatchClassName "file_progress", doFloat),
    (MatchClassName "dialog", doFloat),
    (MatchClassName "download", doFloat),
    (MatchClassName "error", doFloat),
    (MatchClassName "Gimp", doFloat),
    (MatchClassName "notification", doFloat),
    (MatchClassName "pinentry-gtk-2", doFloat),
    (MatchClassName "splash", doFloat),
    (MatchClassName "toolbar", doFloat),
    (MatchClassName "firefox", doFloat),
    (MatchClassName "Downloads", doFloat),
    (MatchClassName "Save As...", doFloat),
    (MatchClassName "Oracle VM VirtualBox Manager", doFloat),
    (MatchClassName "Dialog", doFloat),
    (MatchClassName "desktop_window", doIgnore)
  ]


-- Hooks for tiling and center float
forceCenterFloat, tileBelow :: Query (Endo WindowSet)
tileBelow = insertPosition Below Newer -- Insert WHERE and focus WHAT
forceCenterFloat = doFloatDep aMover

aMover :: p -> W.RationalRect
aMover _ = W.RationalRect x y w h
  where
    h = 1 / 2
    w = 1 / 3
    x = (1 - w) / 2
    y = (1 - h) / 2

myManageHook :: Query (Endo WindowSet)
-- myManageHook = insertPosition Master Newer <> createManageHook shiftRules floatRules <> manageSpawn
myManageHook = createManageHook shiftRules floatRules -- <> manageSpawn

myMouseBindings XConfig {..} =
  M.fromList
    [ ( (modMask, button1),
        \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster -- mod, mouse-1: Sets window to floating mode; move by dragging.
      ),
      ((modMask, button2), \w -> focus w >> windows W.shiftMaster), -- mod, mouse-2: Raises window to top of stack.
      ((modMask, button3), Flex.mouseWindow Flex.discrete), ---------- mod, mouse-3: Optional; (subjectively) ostensibly "nicer" than so-called "normal" mouse movement and/or resizing, at least in theory.
      ( (modMask .|. altMask, button3),
        \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster -- mod+alt, mouse-3: Sets window to floating mode aggressively; resize aggressively by dragging.
      ),
      ((modMask, button4), const $ windows W.swapDown),
      ((modMask, button5), const $ windows W.swapUp),
      ((0, 10), const $ toggleWarp)
    ]

myOtherTerminal, myRootFileManager :: String
myOtherTerminal = "kitty"
myRootFileManager = "sudo thunar"

myVisibleWorkspaces :: [String]
myVisibleWorkspaces = zSpaces -- The names of the visible workspaces.

mygridConfig :: GSConfig XMonad.Window
mygridConfig =
  (buildDefaultGSConfig yColorizer)
    { gs_cellheight = 64,
      gs_cellwidth = 256,
      gs_cellpadding = 5,
      gs_originFractX = 0.5,
      gs_originFractY = 0.5,
      gs_font = myFontMeh
    }

yTerminal :: (IsString a) => a
yTerminal = "wezterm"

zSpaces :: [String]
zSpaces = ['W' : show n | n <- [(1 :: Int) .. 12]] ++ topicNames topics

topics :: [TopicItem]
topics =
  let tNames = ['T' : show n | n <- [(1 :: Int) .. 12]]
      tActions =
        [ (spawnExec chromium),
          (spawnExec discord),
          (spawnExec "chromium --profile-directory=Default --app-id=cinhimbnkkaeohfgghhklpknlkffjgod"),
          (spawnExec "chromium --profile-directory=Default --app-id=agimnkijcaahngcdmfeangaknmldooml"),
          (spawnExec yEditor),
          (spawnExec "echo '' > /tmp/zzz_daemon_pipe"),
          (spawnExec "echo '' > /tmp/zzz_daemon_pipe"),
          (spawnExec "echo '' > /tmp/zzz_daemon_pipe"),
          (spawnExec "echo '' > /tmp/zzz_daemon_pipe"),
          (spawnExec "echo '' > /tmp/zzz_daemon_pipe"),
          (spawnExec "echo '' > /tmp/zzz_daemon_pipe"),
          (spawnExec "echo '' > /tmp/zzz_daemon_pipe")
        ]
   in map (\(name, action) -> inHome name action) $ zip tNames tActions

topicConfig :: TopicConfig
topicConfig =
  def
    { topicDirs = tiDirs topics,
      topicActions = tiActions topics,
      defaultTopicAction = const (pure ()),
      defaultTopic = tHSK
    }

isHiddenWS, isVisibleWS :: String -> Bool
isHiddenWS wsTag = wsTag `elem` myHiddenWorkspaces -- \| Check whether a given workspace is hidden or not.
isVisibleWS = not . isHiddenWS -- \| Check whether a given workspace is visible or not.

hiddenFloatWorkspaceTags, hiddenMinWorkspaceTags, myHiddenWorkspaces :: [String]
hiddenFloatWorkspaceTags = map hiddenFloatWorkspaceOf myVisibleWorkspaces ----------- The names of the workspaces used to hide floating windows.
hiddenMinWorkspaceTags = map hiddenMinWorkspaceOf myVisibleWorkspaces -- The names of the workspaces used to hide minimised windows.
myHiddenWorkspaces = hiddenMinWorkspaceTags ++ hiddenFloatWorkspaceTags -- \| The names of all hidden workspaces.

hiddenFloatWorkspaceOf, hiddenMinWorkspaceOf :: String -> String
hiddenFloatWorkspaceOf wsTag = wsTag ++ "_hf" ---------------------- The workspace used to hide floating windows from a given workspace.
hiddenMinWorkspaceOf wsTag = wsTag ++ "_hm" -- The workspace used to hide minimised windows from a given workspace.

delay :: (MonadIO m) => m ()
delay = io (threadDelay 0)

cmdExecJournal :: String -> String
cmdExecJournal s = unwords $ ["exec"] ++ cmdLogJournal ++ [s]

cmdLogJournal :: [String]
cmdLogJournal =
  [ "systemd-cat",
    "--priority=info",
    "--stderr-priority=warning",
    "--level-prefix=false",
    "--"
  ]

spawnExec :: String -> X ()
spawnExec s = spawn . unwords $ ["exec"] ++ cmdLogJournal ++ [s]

newtype ActiveSBs = ASB {getASBs :: [(ScreenId, StatusBarConfig)]}

instance ExtensionClass ActiveSBs where
  initialValue = ASB []

-- | Given the function to create status bars, update
-- the status bars by killing those that shouldn't be
-- visible anymore and creates any missing status bars
updateSBs :: (ScreenId -> IO StatusBarConfig) -> X ()
updateSBs f = do
  actualScreens <- withWindowSet $ return . map W.screen . W.screens
  (toKeep, toKill) <-
    partition ((`elem` actualScreens) . fst) . getASBs <$> XS.get
  -- Kill the status bars
  cleanSBs (map snd toKill)
  -- Create new status bars if needed
  let missing = actualScreens \\ map fst toKeep
  added <- io $ traverse (\s -> (s,) <$> f s) missing
  traverse_ (sbStartupHook . snd) added
  XS.put (ASB (toKeep ++ added))

-- | Run 'sbLogHook' for the saved 'StatusBarConfig's
logSBs :: X ()
logSBs = XS.get >>= traverse_ (sbLogHook . snd) . getASBs

-- | Kill the given 'StatusBarConfig's from the given
-- list
cleanSBs :: [StatusBarConfig] -> X ()
cleanSBs = traverse_ sbCleanupHook

ymobar :: IO StatusBarConfig
ymobar = do
  let xmobarCmdT = cmdExecJournal "xmobar -x 0 /home/jesse/.0t_xmobarrc"
      xmobarCmdB = cmdExecJournal "xmobar -x 0 /home/jesse/.0b_xmobarrc"
      xmobarCmdN = cmdExecJournal "xmobar -x 1 /home/jesse/.n_xmobarrc"
      trayerCmd =
        cmdExecJournal $
          unwords
            [ "trayer",
              "--edge bottom",
              "--align left",
              "--SetPartialStrut true",
              "--widthtype request",
              "--heighttype request",
              "--monitor 0",
              "--transparent true",
              "--alpha 0",
              "--tint 000020",
              "-l"
            ]

  xmobarRefT <- newIORef Nothing
  xmobarRefB <- newIORef Nothing
  xmobarRefN <- newIORef Nothing
  trayerRef <- newIORef Nothing

  pure $
    def
      { sbStartupHook = initializeBars xmobarRefT xmobarRefB xmobarRefN trayerRef xmobarCmdT xmobarCmdB xmobarCmdN trayerCmd,
        sbLogHook = logHookImpl xmobarRefT xmobarRefB xmobarRefN,
        sbCleanupHook = cleanupBars xmobarRefT xmobarRefB xmobarRefN trayerRef
      }
  where
    initializeBars xmobarRefT xmobarRefB xmobarRefN trayerRef xmobarCmdT xmobarCmdB xmobarCmdN trayerCmd = do
      -- Run the xrandr command first
      spawnOnce "xrandr --output DP-1 --mode 1920x1080 --rate 60.00 --output HDMI-1 --mode 2560x1440 --rate 75.00 --left-of DP-1"

      -- Add a small delay to ensure that the xrandr command has executed
      io $ threadDelay 6500

      -- Launch xmobar instances next
      hXmobarT <- io $ spawnPipe xmobarCmdT
      hXmobarB <- io $ spawnPipe xmobarCmdB
      hXmobarN <- io $ spawnPipe xmobarCmdN

      -- Update IORefs for xmobar instances
      io $ writeIORef xmobarRefT (Just hXmobarT)
      io $ writeIORef xmobarRefB (Just hXmobarB)
      io $ writeIORef xmobarRefN (Just hXmobarN)

      -- Add another small delay to ensure that xmobars have started
      io $ threadDelay 6500

      -- Then launch trayer
      hTrayer <- io $ spawnPipe trayerCmd

      -- Update IORef for trayer
      io $ writeIORef trayerRef (Just hTrayer)
    cleanupBars xmobarRefT xmobarRefB xmobarRefN trayerRef = do
      io $ do
        readIORef xmobarRefT >>= (`whenJust` hClose) >> writeIORef xmobarRefT Nothing
        readIORef xmobarRefB >>= (`whenJust` hClose) >> writeIORef xmobarRefB Nothing
        readIORef xmobarRefN >>= (`whenJust` hClose) >> writeIORef xmobarRefN Nothing
        readIORef trayerRef >>= (`whenJust` hClose) >> writeIORef trayerRef Nothing

    logHookImpl xmobarRefT xmobarRefB xmobarRefN = do
      hT' <- io $ readIORef xmobarRefT
      hB' <- io $ readIORef xmobarRefB
      hN' <- io $ readIORef xmobarRefN

      let logToHandle h pp = do
            str <- dynamicLogString =<< clickablePP (ppConfig pp)
            io $ hPutStrLn h str
            xmonadPropLog str

      -- Log to each handle if it exists
      whenJust hT' (`logToHandle` ppOrder')
      whenJust hB' (`logToHandle` ppOrder')
      whenJust hN' (`logToHandle` ppOrderN)

    ppConfig ppOrderFn =
      def
        { ppCurrent = xmobarColor "#FFFFFF" "" . wrap "-" "-",
          ppExtras =
            [ gets $
                Just
                  . show
                  . length
                  . W.integrate'
                  . W.stack
                  . W.workspace
                  . W.current
                  . windowset
            ],
          ppHidden = xmobarColor "#FFFF00" "",
          ppLayout = xmobarColor "#FFFF00" "",
          ppOrder = ppOrderFn,
          ppSep = xmobarColor "#FFFF00" "" $ " • ",
          ppSort =
            (. filter \(W.Workspace tag _ _) -> isVisibleWS tag)
              <$> getSortByIndex,
          ppTitle = xmobarColor "#FFFF00" "",
          ppUrgent = xmobarColor "#FF00FF" "",
          ppVisible = xmobarColor "#FFFF00" "" . wrap "[" "]"
        }

    ppOrder' xs
      | length xs == 4 = [head xs, xs !! 1, xs !! 3, xs !! 2]
      | otherwise = xs

    ppConfigN ppOrderFn =
      def
        { ppCurrent = xmobarColor "#FFFFFF" "" . wrap "-" "-",
          ppHidden = xmobarColor "#FFFF00" "",
          ppLayout = xmobarColor "#FFFF00" "",
          ppOrder = ppOrderFn,
          ppSep = xmobarColor "#FFFF00" "" $ " • ",
          ppSort =
            (. filter \(W.Workspace tag _ _) -> isVisibleWS tag)
              <$> getSortByIndex,
          ppTitle = xmobarColor "#FFFF00" "",
          ppUrgent = xmobarColor "#FF00FF" "",
          ppVisible = xmobarColor "#FFFF00" "" . wrap "[" "]"
        }

    ppOrderN xs
      | length xs == 4 = [head xs]
      | otherwise = xs

warpUp = do
  -- Retrieve the current screen and warp to its center
  currentScreen <- gets (W.screen . W.current . windowset)
  let screenWarp = warpToScreen currentScreen 0.5 0.5

  -- Update the cursor position
  let pointerUp = updatePointer (0.5, 0.5) (0, 0)

  -- Warp to the center of the current window
  let windowWarp = warpToWindow (1 % 2) (1 % 2)
  screenWarp <> pointerUp <> windowWarp

data WarpState = WarpState {warpEnabled :: Bool} deriving (Typeable, Read, Show)

instance ExtensionClass WarpState where
  initialValue = WarpState True

toggleWarp :: X ()
toggleWarp = do
  ws <- XS.get
  XS.put (WarpState (not $ warpEnabled ws))

main :: IO ()
main = do
  ySBs <- ymobar
  xmonad
    . docks
    . usePrefixArgument "M3-<Insert>"
    . withNavigation2DConfig def
    . addAfterRescreenHook (spawnExec "fbsetroot -mod 5 5 -fg rgb:00/255/00 -bg rgb:00/00/00")
    . additionalNav2DKeys
      (xK_comma, xK_apostrophe, xK_period, xK_p)
      [ (mod4Mask .|. controlMask, windowGo),
        (mod4Mask .|. controlMask .|. shiftMask, windowSwap)
      ]
      False
    $ ewmh
      def
        { terminal = yTerminal, -------- Sets default terminal to Kitty.
          focusFollowsMouse = True, -- Enables option to automatically focus window over which cursor is hovering.
          logHook = do
            warpState <- XS.get :: X WarpState
            when (warpEnabled warpState) warpUp
            sbLogHook ySBs >> killAllStatusBars,
          clickJustFocuses = False,
          borderWidth = 3, ----- Set window border width to three pixels;
          modMask = mod4Mask, -- modkey to super (a.k.a. the Windows key).
          normalBorderColor = "#00FFFF", --- Set the non-focused windows' borders' and focused window's border's colors to Egnaro,
          focusedBorderColor = "#FF0000", -- and to International Orange respectively.
          workspaces = zSpaces,
          keys = keyBindings,
          mouseBindings = myMouseBindings,
          layoutHook = zLayoutHook,
          manageHook = myManageHook, -- <+> manageSpawn
          startupHook = do
            sbStartupHook ySBs >> killAllStatusBars
            adjustEventInput,
          -- spawnOnce (cmdExecJournal "discord --no-startup-id --start-minimized")
          -- spawnOnce (cmdExecJournal "chromium --no-startup-window"),
          handleEventHook =
            swallowEventHookSub (className =? "Kitty") (pure True)
              <+> serverModeEventHook
              <+> focusOnMouseMove
              <+> handleEventHook def
              <+> Hacks.windowedFullscreenFixEventHook
              <+> Hacks.trayerAboveXmobarEventHook
              <+> Hacks.trayerPaddingXmobarEventHook
              <+> debugKeyEvents
              <+> minimizeEventHook
              <+> floatConfReqHook yFloatCRMH
        }
      `additionalKeysP` [
                          -- \| Changing the size of stack windows:
                          ("M3-o", sendMessage MirrorShrink),
                          ("M3-e", sendMessage MirrorExpand),
                          ("M-p", delay >> toggleWS) -- SWITCH TO PREVIOUS WORKSPACE
                        ]
  where
    zLayoutHook =
      windowNavigation
        . refocusLastLayoutHook
        . trackFloating
        . BW.boringWindows
        $ layouts
      where
        layouts = rAcclimate ||| rSpyyral
          where
            rAcclimate = renamed [Replace "∀"] acclimate
            rSpyyral = renamed [Replace "Φ"] spyyral
            acclimate = avoidFloats $ yeOldeFixer yeOldeAcclimate
            fixl =
              maximize
                . minimize
                . layoutHintsWithPlacement (0.5, 0.5)
                . smartBorders
                . toggles
                . yGaps
              where
                toggles =
                  avoidStruts
                    . mouseResize
                    . windowArrange
                    . mkToggle (single NBFULL)
                    . mkToggle1 REFLECTX
                    . mkToggle1 REFLECTY
                    . mkToggle1 NOBORDERS
                    . mkToggle1 MIRROR
                yGaps = gaps (map (,0) [U, D, L, R])
            fullS = layoutHints $ noBorders Full
            haaaxxxxx = IfMax 1 fullS $ IfMax 3 rTiledSnoMASTER hax
              where
                hax = IfMax 5 rTCsNOmaster rTCMsNOmaster
                  where
                    rTCMsNOmaster = noMASTERmag rTCMs
            magMOSalt = noMASTERmag mosALT
            magMOSaltS = spacing33 magMOSalt
            mosALT = MosaicAlt M.empty
            noMASTERmag = magnify φ (NoMaster 3) True
            ostentationTheme =
              -- \| TODO!!! FIXME!!!
              def
                { decoHeight = 4,
                  activeColor = colorTrueYellow,
                  inactiveColor = colorYarg,
                  urgentColor = colorMagenta,
                  activeBorderWidth = 1,
                  inactiveBorderWidth = 1,
                  urgentBorderWidth = 3,
                  activeBorderColor = colorBlue,
                  inactiveBorderColor = colorRed,
                  urgentBorderColor = colorBlack -- TODO
                }
            otherRT = ResizableTall 1 0.03 (φ / (1 + φ)) []
            rT = ResizableTall 1 0.03 0.5 []
            rTC = ResizableThreeCol 1 0.03 0.5 []
            rTCM = ResizableThreeColMid 1 0.03 0.5 []
            rTCs = spacing88 rTC
            rTCMs = spacing88 rTCM
            rTCsNOmaster = noMASTERmag rTCs
            rTiled = IfMax 2 rT otherRT
            rTiledS = spacing44 rTiled
            rTiledSnoMASTER = noMASTERmag rTiledS
            spacing33 =
              spacingRaw False (Border 0 3 3 3) True (Border 3 3 3 3) True
            spacing44 =
              spacingRaw False (Border 0 4 4 4) True (Border 4 4 4 4) True
            spacing88 =
              spacingRaw False (Border 0 8 8 8) True (Border 8 8 8 8) True
            sub =
              addTabs MyCustomShrink ostentationTheme
                . subLayout [] Simplest
            yeOldeAcclimate = IfMax 6 haaaxxxxx magMOSaltS
            yeOldeFixer = fixl . sub . smartSpacingWithEdge 0
            φ = realToFrac ((1.0 + sqrt 5) / 2.0 :: Double)
            spyral = spiral (φ / (1 + φ))
            magSPYRAL = noMASTERmag spyral
            magSPYRALs = goldenSpacing magSPYRAL
            goldenSpacing =
              spacingRaw False (Border 0 3 4 5) True (Border 8 8 8 8) True
            spyyral = avoidStruts $ yeOldeFixer ψ
              where
                ψ = IfMax 1 fullS magSPYRALs

data MyCustomShrink
  = MyCustomShrink

instance Show MyCustomShrink where
  show :: MyCustomShrink -> String
  show _ = ""

instance Read MyCustomShrink where
  readsPrec :: Int -> ReadS MyCustomShrink
  readsPrec _ s = [(MyCustomShrink, s)]

instance Shrinker MyCustomShrink where
  shrinkIt :: MyCustomShrink -> String -> [String]
  shrinkIt _ _ = []

newtype LockdownState
  = LockdownState Bool
  deriving stock (Read, Show)

instance ExtensionClass LockdownState where
  initialValue :: LockdownState
  initialValue = LockdownState False

  extensionType :: LockdownState -> StateExtension
  extensionType = PersistentExtension
