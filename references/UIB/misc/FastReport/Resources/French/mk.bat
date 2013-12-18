set FRCC=..\..\..\..\Res\frcc.exe

%FRCC% frxrcUIB.frc

copy frxrcUIB.pas ..\..

del *.pas