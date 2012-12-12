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
import XMonad.Actions.SpawnOn(spawnOn)
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

myStartupHook = do
    spawnOn "1" "chromium-browser"

pianobarCmd :: String -> String
pianobarCmd cmd = "pianobar-ctl '" ++ cmd ++ "'"

main = do
    dbproc <- spawnPipe "dropbox start"
    xmproc <- spawnPipe "xmobar --screen=0"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = layout
        , focusFollowsMouse = False
        , startupHook = myStartupHook

        -- BEGIN master-branch-specific config
        , logHook = dynamicLogWithPP $ xmobarPPOptions xmproc
        -- END master-branch-specific config

        }
        `additionalKeysP`
        [ ("M-S-s", spawn "gksudo shutdown -P now")

        -- BEGIN master-branch shortcuts
        , ("M-s", spawn "dbus-send --system --print-reply --dest='org.freedesktop.UPower' /org/freedesktop/UPower org.freedesktop.UPower.Suspend")
        , ("M-S-p", spawn $ pianobarCmd "p") -- pause
        , ("M-S-n", spawn $ pianobarCmd "n") -- next
        , ("M-S-u", spawn $ pianobarCmd "+") -- thumbs up
        , ("M-S-d", spawn $ pianobarCmd "-") -- thumbs down
        -- END master-branch shortcuts

        ]
