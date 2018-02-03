echo off

rem compile UDExtract.dpr
dcc32 UDExtract.dpr -U..\..\jcl\source\windows;..\..\jcl\source\common -I..\..\jcl\source\include -N0. -E.

rem execute UDExtract.dpr no compression
UDExtract.exe UnicodeData.txt ..\..\jcl\source\common\JclUnicode.rc /c=SpecialCasing.txt /f=CaseFolding.txt /d=DerivedNormalizationProps.txt /p=PropList.txt

rem execute UDExtract.dpr zlib compression
UDExtract.exe UnicodeData.txt ..\..\jcl\source\common\JclUnicodeZLib.rc /z /c=SpecialCasing.txt /f=CaseFolding.txt /d=DerivedNormalizationProps.txt /p=PropList.txt

rem execute UDExtract.dpr bzip2 compression
UDExtract.exe UnicodeData.txt ..\..\jcl\source\common\JclUnicodeBzip2.rc /bz /c=SpecialCasing.txt /f=CaseFolding.txt /d=DerivedNormalizationProps.txt /p=PropList.txt

rem compiling JclUnicode.rc
cd ..\..\jcl\source\common
brcc32 JclUnicode.rc -foJclUnicode.res
brcc32 JclUnicodeZLib.rc -foJclUnicodeZLib.res
brcc32 JclUnicodeBZip2.rc -foJclUnicodeBZip2.res
cd ..\..\..\thirdparty\unicode_data_extractor
