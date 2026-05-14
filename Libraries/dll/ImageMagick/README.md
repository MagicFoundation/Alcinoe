# Installing ImageMagick

## 1) Download ImageMagick

Download the Windows DLL builds from:  
https://imagemagick.org/script/download.php

Current version used by Alcinoe:
- ImageMagick-7.1.2-22-Q16-HDRI-x64-dll.exe
- ImageMagick-7.1.2-22-Q16-HDRI-x86-dll.exe

Install both (ideally inside a clean VM).  
During installation, make sure to **enable 
installation of the C++ headers** (the 
`include` folder).

---

## 2) Copy the DLL distributions

Copy the content of:
- c:\Program Files (x86)\ImageMagick-7.1.2-Q16-HDRI\ → `Alcinoe/Libraries/dll/ImageMagick/win32`
- c:\Program Files\ImageMagick-7.1.2-Q16-HDRI\ → `Alcinoe/Libraries/dll/ImageMagick/win64`

---

## 4) Remove unnecessary files and folders

Delete these directories from both `win32` and `win64`:

images  
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

Copy the source code from:  
https://github.com/ImageMagick/ImageMagick/releases

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