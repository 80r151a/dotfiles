import XMonad
import XMonad.Hooks.DynamicLog        (ppSep, ppTitleSanitize, ppCurrent, ppHidden, ppHiddenNoWindows, ppUrgent, ppOrder, ppExtras)
import XMonad.Hooks.ManageHelpers     (isFullscreen, doFullFloat, isDialog, isInProperty, transience', composeOne, (-?>))
import XMonad.Hooks.StatusBar         (withEasySB, statusBarProp, defToggleStrutsKey)
import XMonad.Hooks.StatusBar.PP      (PP, xmobarStrip, xmobarColor, xmobarBorder, xmobarRaw, shorten, wrap)
import XMonad.Util.EZConfig           (additionalKeysP)
import XMonad.Util.Loggers            (logTitles)
import XMonad.Util.Ungrab             (unGrab)
import XMonad.Layout.Magnifier        (magnifiercz')
import XMonad.Layout.ThreeColumns     (ThreeCol( ThreeColMid))
import XMonad.Hooks.EwmhDesktops      (ewmhFullscreen, ewmh)
import XMonad.Layout.Fullscreen       (fullscreenFull)
import XMonad.Layout.Spacing          (smartSpacingWithEdge)
import qualified XMonad.Hooks.ManageDocks as Docks

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
    { modMask         = mod4Mask      -- Rebind Mod to the Super key
    , terminal        = "urxvt"       -- Default terminal
    , layoutHook      = myLayout      -- Use custom layouts
    , manageHook      = myManageHook  -- Match on certain windows
    , borderWidth     = 2
    }
  `additionalKeysP`
    [ ("M-S-z", spawn "i3lock" )
    , ("M-C-s", unGrab *> spawn "scrot -s ~/Pictures/screenshots/screenshot.png" )
    , ("M-f"  , spawn "firefox" )
    ]

myManageHook = composeAll [ transience', manageWindow, manageOverrides ]
  where manageWindow = composeOne
          [ isFullscreen -?> doFullFloat
          -- Dialogs (e.g. Ubuntu logout dialog)
          , isDialog -?> doFloat
          -- Splash screens
          , isSplash -?> doIgnore
          ]
        manageOverrides = composeOne
          [ className =? "Gimp" -?> doFloat
          ]
        isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

myLayout = Docks.avoidStruts $ withSpaces tiled ||| withSpaces Mirror tiled ||| fullscreenFull Full ||| withSpaces threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes
    withSpaces layout = smartSpacingWithEdge 3 $ layout


myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
