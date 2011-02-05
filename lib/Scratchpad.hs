module Scratchpad
     ( scratchpads
     , namedScratchpadAction
     ) where

import qualified XMonad.StackSet as W
import XMonad.ManageHook
import XMonad.Util.NamedScratchpad

scratchpads = [ 
                {-NS "terminal"    "urxvtc -name terminal -e tmux'" (resource =? "terminal") defaultRect-}
              NS "htop"     "urxvtc -name htop -e htop"     (title     =? "htop")     defaultRect
              , NS "ncmpcpp"  "urxvtc -name ncmpcpp -e tmux" (resource =? "ncmpcpp") defaultRect
              , NS "gtg"  "gtg_new_task" (title =? "(no title task)") smallerRect
              ] where
                fullRect    = customFloating $ W.RationalRect 0 0 1 1
                defaultRect = customFloating $ W.RationalRect (1/6) (1/6) 0.75 0.75
                smallerRect = customFloating $ W.RationalRect 0.27 0.25 0.45 0.4
