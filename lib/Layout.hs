module Layout
     ( layouts 
     ) where


import Data.Ratio ( (%) )
import XMonad hiding ( (|||) )

import XMonad.Layout.Gaps
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Magnifier
import XMonad.Layout.Named
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spiral
import XMonad.Layout.StackTile
import XMonad.Layout.Tabbed
-- nessesary for tabs declaration
import XMonad.Layout.Decoration
import XMonad.Layout.Simplest ( Simplest(Simplest) )


layouts =  onWorkspace "web"  ( tabs' ||| stack' ||| full )
         $ onWorkspace "im"   ( magnifierOff ( im_tabs ||| im_stack ) )
         $ onWorkspace "work" ( tabs' ||| stack')
         $ stack' ||| tabs'
        where
            gap           = gaps [(U,18)]
            full          = named "full" $ Full
            tabs          :: ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest Window
            tabs          = tabbed shrinkText oxyTheme
            tabs'         = named "tabs" $ gap $ tabs
            spiral'       = named "spiralL" $ gap $ spiral (6/7)
            stack         = StackTile 1 (3/100) (1/2)
            stack'        = named "stack" $ gap $ stack
            im_stack      = named "im_stack" $ gap $ withIM (1%7) buddy_list (stack)
            im_tabs       = named "im_tabs" $ gap $ withIM (1%7)  buddy_list (tabs)
            buddy_list    = (is_skype `Or` Resource "main"  `Or` Role "contactlist")
            is_skype      = (Title "Skype™ 2.1 (Beta) для Linux") `Or` (Title "a.skurihin - Skype™ (Beta)")


-- Configuration for Tabbed
oxyTheme :: Theme
oxyTheme = defaultTheme { inactiveBorderColor = "#696969"
                        , activeBorderColor   = "white"
                        , activeColor         = "#111111"
                        , inactiveColor       = "#212121"
                        , inactiveTextColor   = "#696969"
                        , activeTextColor     = "white"
                        , fontName            = "xft:Terminus-9"
                        , decoHeight          = 18
                        , urgentColor         = "white"
                        , urgentTextColor     = "#77e000"
                        }
