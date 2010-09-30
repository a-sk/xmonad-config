module PerLayoutBind 
       ( bindOnLayout
         ) where

import XMonad
import Data.List (find)
import XMonad.StackSet

-- Get the tag of the currently focused workspace.
currentLayout = description . layout . workspace . current

-- $usage
-- >   ,((0, xK_F2), bindOnLayout [("full", spawn "rxvt"), ("tall", spawn "xeyes"), ("", spawn "xmessage hello")])
--

-- | Uses supplied function to decide which action to run depending on current layout name.
chooseAction :: (String->X()) -> X()
chooseAction f = withWindowSet (f . currentLayout)

-- | If current layout is listed, run appropriate action (only the first match counts!)
-- If it isn't listed, then run default action (marked with empty string, \"\"), or do nothing if default isn't supplied.
bindOnLayout :: [(String, X())] -> X()
bindOnLayout bindings = chooseAction chooser where
    chooser ws = case find ((ws==).fst) bindings of
        Just (_, action) -> action
        Nothing -> case find ((""==).fst) bindings of
            Just (_, action) -> action
            Nothing -> return ()

