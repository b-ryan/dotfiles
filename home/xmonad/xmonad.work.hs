-- required packages:
-- xmonad
-- xmonad-contrib
-- xmobar
-- dmenu

-- https://wiki.archlinux.org/index.php/Xmonad#Using_xmobar_with_xmonad
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen%27s_Configuration#Configuring_xmonad_to_use_xmobar

import XMonad
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Layout
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Util.EZConfig(additionalKeysP,removeKeysP)
import XMonad.Util.Run(spawnPipe,runProcessWithInput)
import System.IO

import qualified XMonad.StackSet as W
import qualified XMonad.Util.NamedScratchpad as NS

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

scratchpads = [ NS.NS "keepassx" "keepassx" findKeepassX manageKeepassX
              , NS.NS "terminal" spawnTerm  findTerm     manageTerm
              ]
    where

        findKeepassX = title =? "passwords - KeePassX"
        manageKeepassX = NS.customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)

        spawnTerm = "gnome-terminal --title scratchterm -x ssh -X vm"
        findTerm = title =? "scratchterm"
        manageTerm = NS.customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)

main = do
    -- dbproc <- spawnPipe "dropbox start"
    runProcessWithInput "xrandr" ["--auto", "--output", "VGA1", "--left-of", "HDMI1"] ""
    xmprocRight <- spawnPipe "xmobar --screen=1"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> insertPosition Below Newer <+> manageHook defaultConfig
        , layoutHook = myLayoutHook
        , normalBorderColor = "black"
        , focusedBorderColor = "#FAD4F1"
        , logHook = (dynamicLogWithPP $ xmobarPPOptions xmprocRight)
        }
        `removeKeysP`
        [ "M-q"
        ]
        `additionalKeysP`
        [ ("M-s",             spawn "gnome-screensaver-command -l")
        , ("M-v",             spawn "gnome-terminal -x ssh -X vm")
        , ("M-g",             bringSelected defaultGSConfig)
        --PIANOBAR
        , ("M-<KP_Enter>",    spawn $ pianobarCmd "start")
        , ("M-<KP_Delete>",   spawn $ pianobarCmd "q")
        , ("M-<KP_Insert>",   spawn $ pianobarCmd "p") -- keypad 0 = pause
        , ("M-<KP_Right>",    spawn $ pianobarCmd "n") -- keypad 6 = next
        , ("M-<KP_Up>",       spawn $ pianobarCmd "+") -- keypad 8 = thumbs up
        , ("M-<KP_Down>",     spawn $ pianobarCmd "-") -- keypad 2 = thumbs down
        , ("M-<KP_Add>",      spawn $ pianobarCmd ")") -- keypad + = increase volume
        , ("M-<KP_Subtract>", spawn $ pianobarCmd "(") -- keypad - = decrease volume
        --
        , ("M-c", NS.namedScratchpadAction scratchpads "keepassx")
        , ("M-S-v", NS.namedScratchpadAction scratchpads "terminal")
        ]
