# Installing ImageMagick

## 1) Download ImageMagick

Download the Windows DLL builds from:  
https://imagemagick.org/script/download.php

Current version used by Alcinoe:
- ImageMagick-7.1.2-8-Q16-HDRI-x64-dll.exe
- ImageMagick-7.1.2-8-Q16-HDRI-x86-dll.exe

Install both (ideally inside a clean VM).  
During installation, make sure to **enable 
installation of the C++ headers** (the 
`include` folder).

---

## 2) Copy the DLL distributions

Copy the extracted content of:
- ImageMagick-7.1.2-8-Q16-HDRI-x86-dll.exe → `Alcinoe/References/ImageMagick/win32`
- ImageMagick-7.1.1-8-Q16-HDRI-x64-dll.exe → `Alcinoe/References/ImageMagick/win64`

---

## 3) Copy the `include` folder

(x86)/include/ → Alcinoe/References/ImageMagick/win32/include/
(x64)/include/ → Alcinoe/References/ImageMagick/win64/include/

---

## 4) Remove unnecessary files and folders

Delete these directories from both `win32` and `win64`:

images  
include  
lib  
Magick++_Demo  
uninstall  
www

Delete these files:

unins000.exe  
unins000.dat  
index.html  
magick.exe

---

## 5) Regenerate the Delphi wrapper

Clone:  
https://github.com/ImageMagick/ImageMagick.git

Then run:  
Alcinoe/Tools/ImageMagickWrapperGenerator/ImageMagickWrapperGenerator.exe

This automagically regenerates:  
Alcinoe.ImageMagick.pas

---

## Notes

If you use a non-HDRI build of ImageMagick, you 
must add the define MAGICKCORE_NO_HDRI_SUPPORT 
to your project. Similarly, if you use the Q8 
(non-Q16) build, you must add the define 
MAGICKCORE_QUANTUM_DEPTH_8.