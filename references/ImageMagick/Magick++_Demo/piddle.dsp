# Microsoft Developer Studio Project File - Name="piddle" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=piddle - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "piddle.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "piddle.mak" CFG="piddle - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "piddle - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ""
# PROP Intermediate_Dir "Release\piddle"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
LIB32=link.exe -lib
MTL=midl.exe
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD CPP /nologo /MD /W3 /GX /Zi /O2 /I "..\\" /I "..\include" /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "_VISUALC_" /D "NeedFunctionPrototypes" /D "_DLL" /D "_MAGICKMOD_" /FD /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /machine:IX86
# ADD LINK32 CORE_RL_magick_.lib CORE_RL_Magick++_.lib kernel32.lib user32.lib gdi32.lib odbc32.lib odbccp32.lib ole32.lib oleaut32.lib winmm.lib wsock32.lib advapi32.lib /nologo /subsystem:console /debug /machine:I386 /libpath:"..\lib\\" /libpath:"..\lib"
# SUBTRACT LINK32 /pdb:none
# Begin Target

# Name "piddle - Win32 Release"
# Begin Group "src"

# PROP Default_Filter ""
# Begin Source File

SOURCE="piddle.cpp"
# End Source File
# End Group
# Begin Group "include"

# PROP Default_Filter ""
# End Group
# End Target
# End Project
