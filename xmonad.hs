import XMonad
import XMonad.Config.Gnome
import XMonad.Util.Run

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive

import System.IO

--http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Util-EZConfig.html
--http://thinkingeek.com/2011/11/21/simple-guide-configure-xmonad-dzen2-conky/

myXmonadBar = "dzen2 -x 1920 -y 0 -h 24 -w 1920 -ta l -fg #FFFFFF -bg #1B1D1E"

main = do
    dzenLeftBar <- spawnPipe myXmonadBar
    xmonad $ gnomeConfig
        {
            logHook = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd
        }

--Bar
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor "#ebac54" "#1B1D1E" . pad
      , ppVisible           =   dzenColor "white" "#1B1D1E" . pad
      , ppHidden            =   dzenColor "white" "#1B1D1E" . pad
      , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad
      , ppUrgent            =   dzenColor "#ff0000" "#1B1D1E" . pad
      , ppWsSep             =   " "
      , ppSep               =   "  |  "
      , ppTitle             =   (" " ++) . dzenColor "white" "#1B1D1E" . dzenEscape
      , ppOutput            =   hPutStrLn h
    }
