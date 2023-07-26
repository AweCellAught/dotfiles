module Main where

import Control.Concurrent
import Xmobar

configTop = do
  defaultConfig
    { font = "xft:DejaVu Sans:size=12",
      additionalFonts = ["xft:B612:size=12", "xft:DejaVu Mono:style=bold:size=12", "xft:DejaVu Sans:size=13"],
      wmClass = "xmobar0",
      wmName = "xmobar0-T",
      bgColor = "#000000",
      fgColor = "#EFEFF1",
      alpha = 255,
      position = Top,
      border = BottomB,
      borderColor = "#1A1A1A",
      borderWidth = 1,
      textOffset = -1,
      iconOffset = -1,
      textOffsets = [],
      hideOnStart = False,
      lowerOnStart = True,
      persistent = False,
      allDesktops = True,
      overrideRedirect = True,
      pickBroadest = False,
      iconRoot = ".",
      sepChar = "%",
      alignSep = "}{",
      template =
        concat $
          ["<fn=3>%UnsafeStdinReader%</fn>}"]
            <> ["{<fc=#ffbf00>%date% — ╳</fc>"],
      commands =
        [ Run $ Date " %w • %Y/%m/%d • %H:%M (%S) [%Z] • %j" "date" 10,
          Run $ UnsafeStdinReader
        ]
    }

configBottom = do
  defaultConfig
    { font = "xft:B612 Mono:size=12",
      additionalFonts = [],
      wmClass = "xmobar0",
      wmName = "xmobar0-B",
      bgColor = "#000000",
      fgColor = "#CFCFCF",
      alpha = 255,
      position = Bottom,
      border = TopB,
      borderColor = "#1A1A1A",
      borderWidth = 1,
      textOffset = -1,
      iconOffset = -1,
      textOffsets = [],
      hideOnStart = False,
      lowerOnStart = True,
      persistent = False,
      allDesktops = True,
      overrideRedirect = True,
      pickBroadest = False,
      iconRoot = ".",
      sepChar = "%",
      alignSep = "}{",
      template =
        concat $
          ["}{%cpu% • %memory% • %swap% • %wlan0% <fc=#ffbf00>%uptime%</fc>"],
      commands =
        [ Run $ Network "wlan0" ["-L", "0", "-H", "32", "--normal", "#00FF00", "--high", "#FF0000"] 10,
          Run $ Uptime ["-t", "Up: <days>d <hours>h <minutes>m <seconds>s"] 10,
          Run $ Cpu ["-L", "3", "-H", "50", "--normal", "green", "--high", "red"] 10,
          Run $ Memory ["-t", "Mem: <usedratio>%"] 10,
          Run $ Swap [] 10
        ]
    }

main :: IO ()
main = do
  _ <- forkIO (xmobar configTop)
  xmobar configBottom
