-- required packages:
-- xmonad
-- xmonad-contrib
-- xmobar
-- dmenu

-- https://wiki.archlinux.org/index.php/Xmonad#Using_xmobar_with_xmonad
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen%27s_Configuration#Configuring_xmonad_to_use_xmobar

-- other things to check out:
--  XMonad.Util.Scratchpad

import XMonad
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Layout
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP,removeKeysP)
import System.IO

yellow = xmobarColor "#F7F383" ""

xmobarPPOptions :: Handle -> PP
xmobarPPOptions handle = xmobarPP { ppOutput = hPutStrLn handle
                                  , ppTitle = xmobarColor "#7DF58F" "" . shorten 25
                                  , ppCurrent = yellow . (wrap  "." ".")
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

pianobarCmd :: String -> String
pianobarCmd cmd = "pianobar-ctl '" ++ cmd ++ "'"

main = do
    -- dbproc <- spawnPipe "dropbox start"
    xmproc <- spawnPipe "xmobar --screen=1"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> insertPosition Below Newer <+> manageHook defaultConfig
        , layoutHook = myLayoutHook
        , normalBorderColor = "black"
        , focusedBorderColor = "#FAD4F1"
        , logHook = (dynamicLogWithPP $ xmobarPPOptions xmproc)
        }
        `removeKeysP`
        [ "M-q"
        ]
        `additionalKeysP`
        [ ("M-s", spawn "dbus-send --system --print-reply --dest='org.freedesktop.UPower' /org/freedesktop/UPower org.freedesktop.UPower.Suspend")
        , ("M-v",             spawn "gnome-terminal -x ssh -X rjmdash@fryan-vm")
        , ("M-g",             bringSelected defaultGSConfig)
        --PIANOBAR
        , ("M-S-e", spawn $ pianobarCmd "q") -- quit
        , ("M-S-p", spawn $ pianobarCmd "p") -- pause
        , ("M-S-n", spawn $ pianobarCmd "n") -- next
        , ("M-S-u", spawn $ pianobarCmd "+") -- thumbs up
        , ("M-S-d", spawn $ pianobarCmd "-") -- thumbs down
        ]
