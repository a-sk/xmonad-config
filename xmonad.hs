-- -*- haskell -*-
import qualified XMonad.StackSet as W
import Monad (liftM)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

-- lib
import Bar
import Hook
import Key
import Layout
import MyManageHook ( manageFullScreenHook )
import Scratchpad

myBorderWidth        = 1
myFocusedBorderColor = "black"
myModMask            = mod4Mask
myNormalBorderColor  = "black"
myNumlockMask        = mod2Mask
myTerminal           = "urxvtc -e tmux"
myWorkspaces         = ["web", "im", "work", "view"]

-- | Whether focus follows the mouse pointer.
myFocusFollowsMouse = False

-- Startup hook
myStartupHook = do
    spawn "/home/ask/.bin/autostart.sh"

-----------------------------------------------------------------------

main = do
  dzen    <- spawnPipe dzenStatusBar
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
      terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , handleEventHook    = ewmhDesktopsEventHook <+> manageFullScreenHook
    , keys               = keybinding
    , mouseBindings      = myMouseBindings
    , layoutHook         = layouts
    , manageHook         = manageHooks <+> (namedScratchpadManageHook scratchpads)
    , logHook            = ewmhDesktopsLogHook >> dynamicLogWithPP (logging dzen) >> updatePointer Nearest
    , startupHook        = myStartupHook
}
