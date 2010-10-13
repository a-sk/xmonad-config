module Hook
     ( manageHooks
     , manageFullScreenHook
     ) where

import qualified Data.Map        as M
import qualified XMonad.StackSet as W

import Data.Ratio ((%))
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.ManageHook
import XMonad.Util.WindowProperties

import MyManageHook

manageHooks = composeHook <+> manageDocks <+> manageDialogs <+> manageMenus

composeHook = composeOne $
    [ propertyToQuery onWeb  -?> moveTo "web"
    , propertyToQuery onIM   -?> moveTo "im"
    , propertyToQuery onView -?> moveTo "view"
    , propertyToQuery floats -?> doFloat
    , fullFloats             -?> doFullFloat
    , ignores                -?> doIgnore ]
    where
        moveTo     = doF . W.shift
        floatClass = anyClass [ "feh"]
        floats     = anyClass [ "Dialog", "Zenity", "Vlc" ]
        onWeb      = anyClass [ "Opera", "Minefield", "Namoroka", "Google-chrome", "Uzbl-core" ]
        onIM       = anyClass [ "psi", "Skype", "Qutim"]
        onView     = anyClass [ "Transmission", "Gpodder", "Transmission-gtk" ]
        ignores    = foldr1 (<||>) [ resource =? "stalonetray" ]
        fullFloats = foldr1 (<||>) [ propertyToQuery floatClass, isFullscreen, myIsFullscreen ]

anyClass :: [String] -> Property
anyClass = foldr Or (Const False) . map ClassName

