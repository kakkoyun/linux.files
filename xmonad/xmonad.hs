-- Author: Kemal Akkoyun
-- xmonad example config file.
-- Initialization date: 11.11.12
-- Notes: Most of the comments inherited directly from template file.
--        Personal comments have a @rev annotation above themselves.

import Control.Monad (liftM)
import Data.Ratio
import Graphics.X11.Xlib

import System.Exit
import System.IO
import System.Posix.Env

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp
import XMonad.Core
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog hiding (shorten)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Circle
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Util.Run (safeSpawn, safeSpawnProg, spawnPipe)
import XMonad.Util.WorkspaceCompare
import XMonad.Util.EZConfig (additionalKeys)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvt"

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- @rev
-- My preffered Screenlock commend.
--
myScreenLock    = "/usr/bin/gnome-screensaver-command -l"

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
-- or
-- myWorkspaces = ["console", "web", "editor", "music"] ++ map show [5..9]
-- or
-- myWorkspaces :: [WorkspaceId]
-- myWorkspaces = ["1:chat", "2:web", "3:code", "4:pdf", "5:doc", "6:vbox" ,"7:games", "8:vid", "9:gimp"]
--
myWorkspaces    = ["α", "β" ,"γ", "δ", "ε", "ζ", "η", "θ", "ι"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#ee9a00"
myFocusedBorderColor = "#000000"


-- Default offset of drawable screen boundaries from each physical
-- screen. Anything non-zero here will leave a gap of that many pixels
-- on the given edge, on the that screen. A useful gap at top of screen
-- for a menu bar (e.g. 15)
--
-- An example, to set a top gap on monitor 1, and a gap on the bottom of
-- monitor 2, you'd use a list of geometries like so:
--
-- > defaultGaps = [(18,0,0,0),(0,18,0,0)] -- 2 gaps on 2 monitors
--
-- Fields are: top, bottom, left, right.
--
myDefaultGaps   = [(22,0,0,0)]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- launch a terminal
    -- [ ((modMask,               xK_t), spawn $ XMonad.terminal conf)
    [ ((modMask,               xK_t     ), spawn $ XMonad.terminal conf)

    -- launch dmenu
    -- , ((modMask,               xK_x     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((modMask,               xK_x     ), shellPrompt defaultXPConfig)

    -- launch gmrun
    , ((modMask .|. shiftMask, xK_x     ), spawn "gmrun")

    -- Lock the screen
    -- , ((modMask .|. shiftMask, xK_l), safeSpawn "gnome-screensaver-command" ["-l"])
    , ((modMask .|. shiftMask, xK_l), spawn $ myScreenLock)

    -- close focused window
    , ((modMask .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modMask .|. shiftMask, xK_n     ), refresh)

    -- Take a note
    , ((modMask, xK_n), appendFilePrompt defaultXPConfig "/home/rev/notes.txt")

    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp)
    , ((modMask,               xK_k     ), windows W.focusUp)

    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster)

    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown)

    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp)

    -- Shrink the master area

    , ((modMask,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modMask .|. shiftMask, xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- Ssh to host
    , ((modMask .|. controlMask, xK_s), sshPrompt defaultXPConfig)

    -- toggle the status bar gap
    -- , ((modMask              , xK_b     ),
    --       modifyGap (\i n -> let x = (XMonad.defaultGaps conf ++ repeat (0,0,0,0)) !! i
    --                          in if n == x then (0,0,0,0) else x))

    , ((modMask, xK_b), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modMask              , xK_q     ), restart "xmonad" True)
    -- , ((modMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart"

    -- Shell Promt
    , ((modMask              , xK_p     ), shellPrompt revXPConfig)

    -- Cyclic WS
    , ((modMask              , xK_Right ), shiftToNext >> nextWS)
    , ((modMask              , xK_Left  ), shiftToPrev >> prevWS)

    , ((modMask              , xK_Up    ), sendMessage MagnifyMore)
    , ((modMask              , xK_Down  ), sendMessage MagnifyLess)
    -- @rev
    -- Move pointer to currently focused window
    , ((modMask              , xK_Up    ), warpToWindow (1%10) (1%10))

    -- @rev
    -- Completely personal :)
    -- , ((modMask .|. controlMask, xK_g  ), AL.launchApp defaultXPConfig "google-chrome")
    , ((modMask .|. controlMask, xK_e  ), spawn "emacsclient -c")
    , ((modMask .|. controlMask, xK_d  ), spawn "drracket")
    , ((modMask .|. controlMask, xK_f  ), spawn "firefox")
    , ((modMask .|. controlMask, xK_g  ), spawn "google-chrome")
    , ((modMask .|. controlMask, xK_l  ), spawn "sublime")
    , ((modMask .|. controlMask, xK_v  ), spawn "vlc")
    ]
    ++
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

   -- you may also bind events to the mouse scroll wheel (button4 and button5)
   -- @rev
   -- cycle focus
   --
    , ((modMask, button4), (\_ -> windows W.focusUp))
    , ((modMask, button5), (\_ -> windows W.focusDown))

    ]

------------------------------------------------------------------------
-- @rev
-- Config for Prompt
--
revXPConfig :: XPConfig
revXPConfig = defaultXPConfig { font              = "xft:Consolas-12"
                              , bgColor           = "black"
                              , fgColor           = "white"
                              , fgHLight          = "black"
                              , bgHLight          = "darkslategray4"
                              , borderColor       = "black"
                              , promptBorderWidth = 1
                              , position          = Top
                              , height            = 24
                              , defaultText       = []
                              }

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
-- avoidStruts (simpleTabbed)
myLayout = avoidStruts ( tabs ||| tiled ||| Mirror tiled ||| spiral (6/7) ||| magnify Grid ||| noBorders (fullscreenFull Full))
  where
     -- default tiling algorithm partitions the screen into two panes
     -- tiled   = Tall nmaster delta ratio
     tiled   = ResizableTall nmaster delta ratio []

     -- The default number of windows in the master pane
     nmaster = 2

     -- Default proportion of screen occupied by master pane
     -- ratio   = 3/5
     -- @rev: golden
     ratio   = toRational (2/(1+sqrt(5)::Double))

     -- Percent of screen to increment by when resizing panes
     -- delta   = 5/10
     delta   = 3/10

     -- Tabs
     tabs = tabbed shrinkText revDarkTabTheme

     -- magnification in grid
     magnify = magnifiercz (13%10)

------------------------------------------------------------------------
-- Configuration for Tabbed
revTabTheme :: Theme
revTabTheme = defaultTheme { inactiveBorderColor = "#000"
                        , activeBorderColor = myFocusedBorderColor
                        , activeColor = "#ee9a00"
                        , inactiveColor = "DarkSlateGray4"
                        , inactiveTextColor = "#222"
                        , activeTextColor = "#222"
                        , fontName = "xft:Consolas-9:bold"
                        , decoHeight = 18
                        , urgentColor = "#000"
                        , urgentTextColor = "#63b8ff"
                        }

revDarkTabTheme :: Theme
revDarkTabTheme = defaultTheme { inactiveBorderColor = "#777"
                            , activeBorderColor = myFocusedBorderColor
                            , activeColor = "#ee9a00"
                            , inactiveColor = "#444"
                            , inactiveTextColor = "aquamarine4"
                            , activeTextColor = "#000"
                            , fontName = "xft:Consolas-9"
                            , decoHeight = 16
                            , urgentColor = "#000"
                            , urgentTextColor = "#63b8ff"
                        }

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--

myManageHookOnes = composeOne [ isFullscreen -?> doFullFloat
                                , isDialog     -?> doCenterFloat ]

myManageHook = composeAll [ className =? "Chromium" --> doShift "β"
               , resource =? "desktop_window" --> doIgnore
               , className =? "Gimp" --> doFloat
               , className =? "Google-chrome" --> doShift "β"
               , className =? "Iceweasel" --> doShift "β"
               , className =? "Emacs" --> doShift "γ"
               , className =? "Sublime" --> doShift "γ"
               , className =? "Eclipse" --> doShift "γ"
               , className =? "DrRacket" --> doShift "γ"
               , className =? "Xpdf" --> doShift "δ"
               , className =? "Evince" --> doShift "δ"
               , className =? "Vlc" --> doShift "ε"
               , resource =? "gpicview" --> doFloat
               , className =? "MPlayer" --> doFloat
               , resource =? "skype" --> doFloat
               -- , className =? "VirtualBox" --> doShift "η"
               -- , className =? "Xchat" --> doShift "ζ"
               , resource  =? "kdesktop"       --> doIgnore
               ]

-- Lookup Cheatsheet
--
-- myWorkspaces    = ["α", "β" ,"γ", "δ", "ε", "ζ", "η", "θ", "ι"]

------------------------------------------------------------------------
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
myLogHook h = do
  ewmhDesktopsLogHook
  dynamicLogWithPP $ revPP h
  updatePointer (Relative (1/20) (1/20))
  fadeInactiveLogHook fadeAmount
     where fadeAmount = 0.8

revPP :: Handle -> PP
revPP h = defaultPP  { ppCurrent = wrap "<fc=black,aquamarine3> " " </fc>"
                     , ppSep     = ""
                     , ppWsSep = ""
                     , ppVisible = wrap "<fc=black,DarkSlateGray4> " " </fc>"
                     , ppLayout = \x -> " <fc=#ee9a00,black>"
                                  ++ case x of
                                       "Mirror ResizableTall"   -> "MTiled"
                                       "ResizableTall"          -> "Tiled"
                                       "Tabbed Simplest" -> "Tabbed"
                                       "Magnifier Grid"        -> "MgGrid"
                                       _                        -> x
                                  ++ "</fc> "
                     , ppTitle = \x -> case length x of
                                         0 -> ""
                                         _ -> "<fc=DarkSlateGray3,black>" ++ shorten 30 x ++ "</fc>"
                     , ppHiddenNoWindows = wrap "<fc=#aaa,black> " " </fc>"
                     , ppHidden = wrap "<fc=#aaa,black> " " </fc>"
                     , ppOutput = hPutStrLn h
                     }

shorten :: Int -> String -> String
shorten n xs | length xs < n = xs
             | otherwise     = (take (n - length end) xs) ++ end
 where
    end = "…"

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitra-- ry action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do safeSpawnProg "rxvt"
                   safeSpawnProg "start-pulseaudio-x11"
                   -- spawn "gnome-sound-applet"
                   safeSpawnProg "xmobar"
                   safeSpawnProg "gnome-power-manager"
                   safeSpawnProg "nm"
                   safeSpawnProg "nm-applet"
                   safeSpawnProg "checkgmail"
                   spawn "dropbox start"
                   mapM_ spawn (configureSynaptics synapticsConfig)
                   safeSpawnProg "gnome-settings-daemon"
                   safeSpawnProg "gnome-screensaver"
                   safeSpawnProg "gnome-keyring-daemon"
                   -- spawn "xscrensaver"
                   -- safeSpawnProg "bluetooth-applet"
                   spawn "trayer --transparent true --alpha 0 --tint black --widthtype pixel --width 82 --edge top --distance 0 --align right --margin 0 --height 19 --heighttype pixel --SetDockType true --SetPartialStrut true --expand true"
                   return ()

synapticsConfig = [("CircularScrolling","1"),
                   ("CircScrollTrigger","2"),
                   ("VertEdgeScroll","1"),
                   ("HorizEdgeScroll","1"),
                   ("TapButton1","1"),
                   ("VertTwoFingerScroll","1"),
                   ("HorizTwoFingerScroll","1"),
                   ("TapButton2","3")
                  ]

------------------------------------------------------------------------
-- @rev
-- Syanptic Config
--
configureSynaptics cfg = foldr (\ (var,val) y -> ("sleep 1; synclient " ++ var ++ "=" ++ val) : y) [] cfg

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.

-- @rev from reb.
-- insertPath Function
--
insertPath :: String -> String -> IO ()
-- insertPath k v = getEnv k >>= (\ oldVal -> setEnv k $ v ++ ":" ++ oldVal)
insertPath k v = getEnv k >>= zopik
    where
      zopik (Just oldVal) = setEnv k (v ++ ":" ++ oldVal) True
      zopik Nothing = return ()

main = do insertPath "PATH" "/home/rev/.local/bin"
          setEnv "OOO_FORCE_DESKTOP" "Gnome" True
          setEnv "_JAVA_AWT_WM_NONREPARENTING" "1" True
          pipe <- spawnPipe "xmobar"
          xmonad $ withUrgencyHook NoUrgencyHook $ defaults pipe

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
-- defaults pipe
defaults pipe = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
      -- defaultGaps        = myDefaultGaps,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook <+> manageDocks <+> myManageHookOnes,
        logHook            = myLogHook pipe,
        startupHook        = myStartupHook
    }

------------------------------------------------------------------------
