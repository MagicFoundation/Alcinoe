@echo off

REM convert all SVGs in the current folder -> parent folder as 16/24/32 px PNGs
REM SVG are mostly taken from https://www.svgrepo.com

for %%F in (*.svg) do (
  "c:\Program Files\ImageMagick\magick.exe" -define svg:engine=msvg -background none "%%~fF" -alpha set -trim +repage -resize 16x16 -gravity center -background none -extent 16x16 "..\%%~nF16.png"
  "c:\Program Files\ImageMagick\magick.exe" -define svg:engine=msvg -background none "%%~fF" -alpha set -trim +repage -resize 24x24 -gravity center -background none -extent 24x24 "..\%%~nF24.png"
  "c:\Program Files\ImageMagick\magick.exe" -define svg:engine=msvg -background none "%%~fF" -alpha set -trim +repage -resize 32x32 -gravity center -background none -extent 32x32 "..\%%~nF32.png"
)
echo Done.
Pause