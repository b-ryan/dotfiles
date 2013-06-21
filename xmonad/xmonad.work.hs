-- required packages:
-- xmonad
-- xmonad-contrib
-- xmobar

-- https://wiki.archlinux.org/index.php/Xmonad#Using_xmobar_with_xmonad
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen%27s_Configuration#Configuring_xmonad_to_use_xmobar

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe,runProcessWithInput)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Layout
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Actions.SpawnOn(spawnOn)
import XMonad.Actions.GridSelect
import qualified XMonad.StackSet as W
import System.IO

yellow = xmobarColor "#F7F383" ""

xmobarPPOptions :: Handle -> PP
xmobarPPOptions handle = xmobarPP { ppOutput = hPutStrLn handle
                                  , ppTitle = xmobarColor "#7DF58F" "" . shorten 25
                                  , ppCurrent = yellow . (wrap  "**" "**")
                                  , ppVisible = yellow
                                  , ppHidden = xmobarColor "grey" ""
                                  , ppHiddenNoWindows = xmobarColor "#65BBF7" ""
                                  }

tallLayout = Tall 1 (3/100) (1/2)

tallNoStruts = avoidStruts $ tallLayout
tallMirrorNoStruts = avoidStruts $ Mirror tallLayout
maximized = avoidStruts $ renamed [Replace "Maximized"] Full
fullscreen = noBorders $ fullscreenFull Full

myLayoutHook = tallNoStruts
           ||| tallMirrorNoStruts
           ||| maximized
           ||| fullscreen

myWorkspaces = [ "1:chrome(work)"
               , "2:chrome(home)"
               , "3:test space"
               , "4:"
               , "5:"
               , "6:"
               , "7:"
               , "8:"
               , "9:vim"
               ]

pianobarCmd :: String -> String
pianobarCmd cmd = "pianobar-ctl '" ++ cmd ++ "'"

main = do
    dbproc <- spawnPipe "dropbox start"
    runProcessWithInput "xrandr" ["--auto", "--output", "VGA1", "--left-of", "HDMI1"] ""
    -- xmprocLeft <- spawnPipe "xmobar --screen=0"
    xmprocRight <- spawnPipe "xmobar --screen=1"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = myLayoutHook
        , normalBorderColor = "black"
        , focusedBorderColor = "#FAD4F1"

        -- BEGIN work-branch-specific config
        -- , logHook = (dynamicLogWithPP $ xmobarPPOptions xmprocLeft)
        --          >> (dynamicLogWithPP $ xmobarPPOptions xmprocRight)
        , logHook = (dynamicLogWithPP $ xmobarPPOptions xmprocRight)
        , workspaces = myWorkspaces
        -- END work-branch-specific config

        }
        `additionalKeysP`
        [ ("M-S-s", spawn "gksudo shutdown -P now")
        , ("M-g", goToSelected defaultGSConfig)

        -- BEGIN work branch shortcuts
        , ("M-s", spawn "gnome-screensaver-command -l")
        , ("M-v", spawn "gnome-terminal -x ssh -X rjmdash@fryan-vm")
        , ("M-S-v", spawn "gnome-terminal -x ssh -X rjmdash@fryan-vm gvim")
        , ("M-<KP_Enter>",    spawn $ pianobarCmd "start")
        , ("M-<KP_Delete>",   spawn $ pianobarCmd "q")
        , ("M-<KP_Insert>",   spawn $ pianobarCmd "p") -- keypad 0 = pause
        , ("M-<KP_Right>",    spawn $ pianobarCmd "n") -- keypad 6 = next
        , ("M-<KP_Up>",       spawn $ pianobarCmd "+") -- keypad 8 = thumbs up
        , ("M-<KP_Down>",     spawn $ pianobarCmd "-") -- keypad 2 = thumbs down
        , ("M-<KP_Add>",      spawn $ pianobarCmd ")") -- keypad + = increase volume
        , ("M-<KP_Subtract>", spawn $ pianobarCmd "(") -- keypad - = decrease volume
        -- END work branch shortcuts

        ]
