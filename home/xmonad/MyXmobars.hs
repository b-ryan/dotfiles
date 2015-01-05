module MyXmobars (myLogHook) where
  import XMonad (gets, windowset, description, catchX, io)
  import System.IO (Handle, hPutStrLn)

  import qualified XMonad.StackSet as S
  import Data.List (intercalate, sortBy)
  import Data.Maybe (isJust, catMaybes)
  import Codec.Binary.UTF8.String (encodeString)
  import XMonad.Util.NamedWindows (getName)
  import Data.Ord (comparing)
  import XMonad.Hooks.UrgencyHook (readUrgents)
  import Control.Monad (zipWithM_)
  import XMonad.Util.Run (spawnPipe)

  import qualified XMonad.Hooks.DynamicLog as DL

  yellow = DL.xmobarColor "#F7F383" ""
  blue = DL.xmobarColor "#65BBF7" ""
  dark_blue = DL.xmobarColor "#1187D9" ""
  grey = DL.xmobarColor "grey" ""

  xmobarPPOptions :: Handle -> DL.PP
  xmobarPPOptions handle = DL.xmobarPP { DL.ppOutput = hPutStrLn handle
                                       , DL.ppTitle = DL.xmobarColor "#7DF58F" "" . DL.shorten 25
                                       , DL.ppCurrent = yellow . (DL.wrap  "." ".")
                                       , DL.ppVisible = dark_blue
                                       , DL.ppHidden = grey
                                       , DL.ppHiddenNoWindows = blue
                                       }

  -- The following from:
  -- from https://github.com/manzyuk/dotfiles/blob/master/.xmonad/xmonad.hs

  -- Extract the focused window from the stack of windows on the given screen.
  -- Return Just that window, or Nothing for an empty stack.
  focusedWindow = maybe Nothing (return . S.focus) . S.stack . S.workspace

  -- The functions dynamicLogWithPP', dynamicLogString', and pprWindowSet' below
  -- are similar to their undashed versions, with the difference being that the
  -- latter operate on the current screen, whereas the former take the screen to
  -- operate on as the first argument.

  dynamicLogWithPP' screen pp = dynamicLogString' screen pp >>= io . DL.ppOutput pp

  dynamicLogString' screen pp = do

    winset <- gets windowset
    urgents <- readUrgents
    sort' <- DL.ppSort pp

    -- layout description
    let ld = description . S.layout . S.workspace $ screen

    -- workspace list
    let ws = pprWindowSet' screen sort' urgents pp winset

    -- window title
    wt <- maybe (return "") (fmap show . getName) $ focusedWindow screen

    -- run extra loggers, ignoring any that generate errors.
    extras <- mapM (`catchX` return Nothing) $ DL.ppExtras pp

    return $ encodeString . sepBy (DL.ppSep pp) . DL.ppOrder pp $
               [ ws
               , DL.ppLayout pp ld
               , DL.ppTitle  pp wt
               ]
               ++ catMaybes extras


  pprWindowSet' screen sort' urgents pp s = sepBy (DL.ppWsSep pp) . map fmt . sort' $ S.workspaces s
      where this     = S.tag . S.workspace $ screen
            visibles = map (S.tag . S.workspace) (S.current s : S.visible s)

            fmt w = printer pp (S.tag w)
                where printer | S.tag w == this                                               = DL.ppCurrent
                              | S.tag w `elem` visibles                                       = DL.ppVisible
                              | any (\x -> maybe False (== S.tag w) (S.findTag x s)) urgents  = \ppC -> DL.ppUrgent ppC . DL.ppHidden ppC
                              | isJust (S.stack w)                                            = DL.ppHidden
                              | otherwise                                                     = DL.ppHiddenNoWindows


  sepBy :: String -> [String] -> String
  sepBy sep = intercalate sep . filter (not . null)

  myLogHook = do
    screens <- (sortBy (comparing S.screen) . S.screens) `fmap` gets windowset

    xmobarMiddle <- spawnPipe "xmobar --screen=1"
    xmobarRight <- spawnPipe "xmobar --screen=2"

    zipWithM_ dynamicLogWithPP' (tail screens) [ xmobarPPOptions xmobarMiddle
                                               , xmobarPPOptions xmobarRight
                                               ]
