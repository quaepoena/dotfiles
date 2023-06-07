import XMonad

main = xmonad defaultConfig
         { modMask = mod4Mask
         , terminal = "xfce4-terminal"
         -- , borderWidth = 0
         }
