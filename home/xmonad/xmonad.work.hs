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
import Control.Monad (zipWithM_)
import System.IO

import qualified XMonad.StackSet as W
import qualified XMonad.Util.NamedScratchpad as NS

-- for the stuff
import qualified XMonad.StackSet as S
import Data.List (intercalate, sortBy)
import Data.Maybe (isJust, catMaybes)
import Codec.Binary.UTF8.String (encodeString)
import XMonad.Util.NamedWindows (getName)
import Data.Ord (comparing)
import XMonad.Hooks.UrgencyHook (readUrgents)

yellow = xmobarColor "#F7F383" ""
blue = xmobarColor "#65BBF7" ""
dark_blue = xmobarColor "#1187D9" ""
grey = xmobarColor "grey" ""

xmobarPPOptions :: Handle -> PP
xmobarPPOptions handle = xmobarPP { ppOutput = hPutStrLn handle
                                  , ppTitle = xmobarColor "#7DF58F" "" . shorten 25
                                  , ppCurrent = yellow . (wrap  "." ".")
                                  , ppVisible = dark_blue
                                  , ppHidden = grey
                                  , ppHiddenNoWindows = blue
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

    xmobarMiddle <- spawnPipe "xmobar --screen=1"
    xmobarRight <- spawnPipe "xmobar --screen=2"

    xmonad $ defaultConfig
        { manageHook = manageDocks <+> insertPosition Below Newer <+> manageHook defaultConfig
        , layoutHook = myLayoutHook
        , normalBorderColor = "black"
        , focusedBorderColor = "#FAD4F1"
        , logHook = myLogHook [ xmobarPPOptions xmobarMiddle
                              , xmobarPPOptions xmobarRight
                              ]
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

-- The following from:
-- from https://github.com/manzyuk/dotfiles/blob/master/.xmonad/xmonad.hs

myLogHook pps = do
  screens <- (sortBy (comparing S.screen) . S.screens) `fmap` gets windowset
  zipWithM_ dynamicLogWithPP' (tail screens) pps

-- Extract the focused window from the stack of windows on the given screen.
-- Return Just that window, or Nothing for an empty stack.
focusedWindow = maybe Nothing (return . S.focus) . S.stack . S.workspace

-- The functions dynamicLogWithPP', dynamicLogString', and pprWindowSet' below
-- are similar to their undashed versions, with the difference being that the
-- latter operate on the current screen, whereas the former take the screen to
-- operate on as the first argument.

dynamicLogWithPP' screen pp = dynamicLogString' screen pp >>= io . ppOutput pp

dynamicLogString' screen pp = do

  winset <- gets windowset
  urgents <- readUrgents
  sort' <- ppSort pp

  -- layout description
  let ld = description . S.layout . S.workspace $ screen

  -- workspace list
  let ws = pprWindowSet' screen sort' urgents pp winset

  -- window title
  wt <- maybe (return "") (fmap show . getName) $ focusedWindow screen

  -- run extra loggers, ignoring any that generate errors.
  extras <- mapM (`catchX` return Nothing) $ ppExtras pp

  return $ encodeString . sepBy (ppSep pp) . ppOrder pp $
             [ ws
             , ppLayout pp ld
             , ppTitle  pp wt
             ]
             ++ catMaybes extras


pprWindowSet' screen sort' urgents pp s = sepBy (ppWsSep pp) . map fmt . sort' $ S.workspaces s
    where this     = S.tag . S.workspace $ screen
          visibles = map (S.tag . S.workspace) (S.current s : S.visible s)

          fmt w = printer pp (S.tag w)
              where printer | S.tag w == this                                               = ppCurrent
                            | S.tag w `elem` visibles                                       = ppVisible
                            | any (\x -> maybe False (== S.tag w) (S.findTag x s)) urgents  = \ppC -> ppUrgent ppC . ppHidden ppC
                            | isJust (S.stack w)                                            = ppHidden
                            | otherwise                                                     = ppHiddenNoWindows


sepBy :: String -> [String] -> String
sepBy sep = intercalate sep . filter (not . null)
