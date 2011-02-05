module Key
     ( keybinding
     , myMouseBindings
     ) where

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad
import Monad (liftM)
import XMonad.Util.EZConfig
import XMonad.Actions.FocusNth
import XMonad.Actions.UpdatePointer
import XMonad.Util.Run 
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
import XMonad.Layout.Gaps
import XMonad.Layout.LayoutCombinators
import XMonad.Actions.Promote
import XMonad.Prompt.Shell
import XMonad.Prompt

import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.DynamicWorkspaces
import XMonad hiding ((|||))
import XMonad.Layout.LayoutCombinators ((|||))
import XMonad.Actions.CycleSelectedLayouts
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Layout.Magnifier

import XMonad.Actions.CycleWindows

-- lib
import Scratchpad
import Layout
import PerLayoutBind

keybinding = \conf -> mkKeymap conf $
                [
                  ("M1-S-c",        kill)
                , ("M-<F1>",        runOrRaise "google-chrome" (className =? "Google-chrome"))
                , ("M-<F2>",        runOrRaise "opera" (className =? "Opera"))
                , ("M-<F12>",        runOrRaise "gvim" (className =? "Gvim"))
                , ("M-C-j",        nextNonEmptyWS )
                , ("M-C-k",        prevNonEmptyWS )
                , ("M-C-<Left>",   prevNonEmptyWS )
                , ("M-C-q",        restart "xmonad" True)
                , ("M-C-<Right>",  nextNonEmptyWS )
                , ("M-C-s", unsafeSpawn "sh -x $HOME/.bin/gtg.sh > /dev/null 2>&1 ")
                , ("M-f",   sendMessage $ JumpToLayout "full")
                , ("M-g",          sendMessage $ ToggleGaps)
                , ("M-a",        bindOnLayout  [("full", spawn "urxvt")])
                {-, ("M-a",        showCurrentWS)-}
                , ("M-h",          sendMessage Shrink)
                , ("M-j",          windows W.focusDown)
                , ("M-k",          windows W.focusUp)
                , ("M-l",          sendMessage Expand)
                , ("M-m",          windows W.focusMaster)
                , ("M-p",          shellPrompt defaultXPConfig)
                , ("M-q",            namedScratchpadAction scratchpads "htop")
                , ("M-r" , toggleWS  )
                , ("M-<Return>",   promote)
                , ("M-S-c",        kill)
                , ("M-,",          sendMessage (IncMasterN 1))
                , ("M-.",          sendMessage (IncMasterN (-1)))
                , ("M-S-j",        windows W.swapUp)
                , ("M-S-k",        windows W.swapDown)
                , ("M-s",          namedScratchpadAction scratchpads "gtg")
                , ("M-<Space>",    sendMessage NextLayout)
                , ("M1-<Space>",  bindOn [ ("im", sendMessage toogleMaximized), ("", cycleThroughLayouts ["tabs", "full"])])
                --, ("M1-<Space>",  withFocused (sendMessage . maximizeRestore))
                {-, ("M-S-q",        io (exitWith ExitSuccess))-}
                , ("M-S-<Return>", spawn $ XMonad.terminal conf)
                , ("M-t",   sendMessage $ JumpToLayout "tabs")
                , ("M-w",            namedScratchpadAction scratchpads "ncmpcpp")
                , ("M-x M-c",      kill)
                , ("M-x t",        withFocused $ windows . W.sink)
                , ("M-z",       cycleRecentWindows [xK_Super_L] xK_z xK_w)
                {-, ("M-z",       test) -}
                , ("<Print>", unsafeSpawn "import -w root png:$HOME/screenshot--`date +%F--%T`.png")]
                ++
                [ (m ++ i, windows $ f j)
                    | (i, j) <- zip (map show [1..9]) (XMonad.workspaces conf)
                    , (m, f) <- [("M-", W.view), ("M-S-", W.shift)]
                ]
                ++
                  [("M1-" ++ [k], focusNth i)
                | (k, i) <- zip ['1'..'9'] [0..]
                ]
    where
      nextNonEmptyWS  = moveTo Next (WSIs (liftM (not .) isVisible))
      prevNonEmptyWS  = moveTo Prev (WSIs (liftM (not .) isVisible))
      toogleMaximized = Toggle

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    {-mod-button2, Raise the window to the top of the stack-}
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    {-mod-button3, Set the window to floating mode and resize by dragging-}
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    {-you may also bind events to the mouse scroll wheel (button4 and button5)-}
    -- cycle focus
    , ((modMask, button4), (\_ -> windows W.focusUp))
    , ((modMask, button5), (\_ -> windows W.focusDown))
    {-cycle through workspaces-}
    , ((controlMask .|. modMask, button5), nextNonEmptyWS)
    , ((controlMask .|. modMask, button4), prevNonEmptyWS)
    ]
    where 
      nextNonEmptyWS = \_ -> moveTo Next (WSIs (liftM (not .) isVisible))
      prevNonEmptyWS = \_ -> moveTo Prev (WSIs (liftM (not .) isVisible))


-- helpers
isVisible :: X (WindowSpace -> Bool)
isVisible = do
  vs <- gets (map (W.tag . W.workspace) . W.visible . windowset)
  return (\w -> (W.tag w) `elem` vs)
 
