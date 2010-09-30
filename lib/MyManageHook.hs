module MyManageHook 
    ( manageMenus
    , manageDialogs
    , manageFullScreenHook
    , myIsFullscreen
    ) where

import Data.Bits
import Data.List
import Data.Monoid (All (All))
import Foreign.C.Types (CLong)
import Monad
import qualified XMonad.StackSet as W
import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Util.WindowProperties

-- Float all menus
manageMenus = checkMenu --> doFloat
-- Float all dialogs
manageDialogs = checkDialog --> doFloat 

-- Check if window has named atom with given value
checkAtom name value = ask >>= \w -> liftX $ do
                a <- getAtom name
                val <- getAtom value
                mbr <- getProp w a
                case mbr of
                  Just [r] -> return $ elem (fromIntegral r) [val]
                  _ -> return False

-- Check if window is a menu (for Gimp tear-off menus, for example)
checkMenu = checkAtom "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_MENU"
-- Check if window is a dialog
checkDialog = checkAtom "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG"
checkDockAtom = checkAtom "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DOCK"

-- | Helper to read a property
getProp :: Window -> Atom -> X (Maybe [CLong])
getProp w a = withDisplay $ \dpy -> io $ getWindowProperty32 dpy a w


-- Hacks to help fullscreen apps work correctly

-- Helper functions to fullscreen the window
fullFloat, tileWin :: Window -> X ()
fullFloat w = windows $ W.float w r
    where r = W.RationalRect 0 0 1 1
tileWin w = windows $ W.sink w


-- force google-chrome F11 to work properly
manageFullScreenHook :: Event -> X All
manageFullScreenHook (ClientMessageEvent _ _ _ dpy win typ dat) = do
  state <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  isFull <- runQuery isFullscreen win

  -- Constants for the _NET_WM_STATE protocol
  let remove = 0
      add = 1
      toggle = 2

      -- The ATOM property type for changeProperty
      ptype = 4 

      action = head dat

  when (typ == state && (fromIntegral fullsc) `elem` tail dat) $ do
    when (action == add || (action == toggle && not isFull)) $ do
         io $ changeProperty32 dpy win state ptype propModeReplace [fromIntegral fullsc]
         fullFloat win
    when (head dat == remove || (action == toggle && isFull)) $ do
         io $ changeProperty32 dpy win state ptype propModeReplace []
         tileWin win

  -- It shouldn't be necessary for xmonad to do anything more with this event
  return $ All False

manageFullScreenHook _ = return $ All True

-- Force mplayer to go fullfloat
myIsFullscreen = do
  w <- ask
  fs <- isFullscreen
  if fs then return fs
        else liftX $ do
                   p <- getProp32s "_MOTIF_WM_HINTS" w
                   case p of
                     Just (flags:_:decorations:_) -> return ((flags .&. 2) /= 0 && decorations == 0)
                     Nothing -> return False
