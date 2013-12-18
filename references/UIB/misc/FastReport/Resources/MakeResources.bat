@echo off

echo Cleaning existing resources...
del frxUIB.res 2> nul
del frxUIB_FR4.dcr 2> nul
del frxUIBReg.dcr 2> nul

echo Building resources files...
brc32 frxUIB.rc -r -fofrxUIB.res
brc32 frxUIB_FR4.rc -r -fofrxUIB_FR4.res
brc32 frxUIBReg.rc -r -fofrxUIBReg.dcr

echo Copying resources to sources directory...
copy /Y frxUIB.res ..
copy /Y frxUIB_FR4.res ..
copy /Y frxUIBReg.dcr ..

del frxUIB.res
del frxUIB_FR4.res
del frxUIBReg.dcr

pause

echo Done.