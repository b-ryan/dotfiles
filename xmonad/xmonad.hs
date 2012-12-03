-- required packages:
-- xmonad
-- xmonad-contrib
-- xmobar

-- https://wiki.archlinux.org/index.php/Xmonad#Using_xmobar_with_xmonad
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen%27s_Configuration#Configuring_xmonad_to_use_xmobar

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Layout
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import System.IO

yellow = xmobarColor "#F7F383" ""

xmobarPPOptions :: Handle -> PP
xmobarPPOptions handle = xmobarPP { ppOutput = hPutStrLn handle
                                  , ppTitle = xmobarColor "#7DF58F" "" . shorten 25
                                  , ppHidden = xmobarColor "grey" ""
                                  , ppHiddenNoWindows = xmobarColor "#65BBF7" ""
                                  }

layout = avoidStruts
    (   tallLayout
    ||| Mirror (tallLayout)
    ||| renamed [Replace "Maximized"] Full
    )
    ||| fullLayout
    where
        tallLayout = Tall 1 (3/100) (1/2)
        fullLayout = noBorders $ fullscreenFull Full

pianobarCmd :: String -> String
pianobarCmd cmd = "echo -n " ++ cmd ++ " > ~/.config/pianobar/ctl"

main = do
    dbproc <- spawnPipe "dropbox start"
    xmproc <- spawnPipe "xmobar --screen=0"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = layout
        , logHook = dynamicLogWithPP $ xmobarPPOptions xmproc
        , focusFollowsMouse = False
        }
        `additionalKeysP`
        [ ("M-S-s", spawn "gksudo shutdown -P now")
        , ("M-s", spawn "dbus-send --system --print-reply --dest='org.freedesktop.UPower' /org/freedesktop/UPower org.freedesktop.UPower.Suspend")
        , ("M-S-p", spawn $ pianobarCmd "p") -- pause
        , ("M-S-n", spawn $ pianobarCmd "n") -- next
        , ("M-S-u", spawn $ pianobarCmd "+") -- thumbs up
        , ("M-S-d", spawn $ pianobarCmd "-") -- thumbs down
        ]
