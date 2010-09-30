module Bar
     ( logging
     , dzenStatusBar
     ) where


import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedScratchpad(namedScratchpadFilterOutWorkspace)
import XMonad.Util.Run 

-- Statusbar with workspaces, layout and title
dzenStatusBar = "dzen2 -x 0  -y 0 -h 18 -ta l -fg '" ++ myDzenFGColor ++ 
              "' -bg '" ++ myDzenBGColor ++ "' -fn '" ++ myFont ++ "' -w 1024"


-- Colors, font and iconpath definitions:
myFont = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*%"
myIconDir = "/home/ask/.dzen"
myDzenFGColor = "#555555"
myDzenBGColor = "#212121"
myNormalFGColor = "#77e000"
myNormalBGColor = "#000000"
myFocusedFGColor = "#f0f0f0"
myFocusedBGColor = "#333333"
myUrgentFGColor = "white"
myUrgentBGColor = "#991133"
mySeperatorColor = "#555555"


-- Status bars and logging
logging h = defaultPP
    { ppCurrent     = dzenColor myNormalFGColor myDzenBGColor   . pad . ("^i(.dzen/corner.xbm)" ++) -- current workspace
    , ppVisible     = dzenColor "lightgreen" ""                 . pad                               -- visible workspaces on other screens
    , ppHidden      = dzenColor "white" ""                      . pad . ("^i(.dzen/corner.xbm)" ++) -- hidden workspaces with apps
    , ppHiddenNoWindows     = dzenColor "#444444"  ""           . pad                               -- empty workspaces
    , ppUrgent      = dzenColor myUrgentFGColor myUrgentBGColor . pad . ("^i(.dzen/corner.xbm)" ++) -- urgent workspaces
    , ppTitle       =  dzenColor myNormalFGColor ""              . pad . dzenEscape . shorten 50                  -- title of selected window
    , ppWsSep       = ""                                                                            -- workspace seperator
    , ppSep         = dzenEscape "|"                                                                -- workspace/layout/title seperator
    , ppSort            = fmap (.namedScratchpadFilterOutWorkspace)
                              $ ppSort defaultPP

    -- Layout icons
    , ppLayout      = dzenColor myNormalFGColor "" .
        (\ x -> case x of
                     "stack"                         -> pad "^i(/home/ask/.dzen/layout-tall-right.xbm)"
                     "Mirror Tall"                   -> pad "^i(/home/ask/.dzen/layout-mirror-bottom.xbm)"
                     "full"                          -> pad "^i(/home/ask/.dzen/layout-full.xbm)"
                     "tallC"                          -> pad "^i(/home/ask/.dzen/layout-withim-left1.xbm)"
                     "tabs"                          -> pad "^i(/home/ask/.dzen/tabs.xbm)"
                     _                               -> pad x
        )

    , ppOutput      = hPutStrLn h
    }


