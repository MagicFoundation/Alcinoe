BUILDING THE MONGODB C DRIVER LIBRARIES FROM SOURCE (Windows)
=============================================================

Official guide:
https://www.mongodb.com/docs/languages/c/c-driver/current/install-from-source/

1) Install Visual Studio
------------------------

- Download:
  https://visualstudio.microsoft.com/vs/community/
- During installation, select:
  "Desktop development with C++"
- Click Install.

2) Install CMake
----------------

- Download:
  https://cmake.org/download/
- Example installer:
  cmake-4.2.2-windows-x86_64.msi

3) Download a release archive
-----------------------------
- Releases:
  https://github.com/mongodb/mongo-c-driver/releases
- Download:
  "Source code (zip)"
- Extract to:
  C:\Dev\mongo-c-driver

4) Build libbson + libmongoc (update 2.2.1 with your downloaded version)
------------------------------------------------------------------------

Notes:
- Replace BUILD_VERSION="2.2.1" with the version you downloaded.
- RelWithDebInfo is a good default for producing usable binaries with symbols.

---- win64:
   
   cmake -S c:\Dev\mongo-c-driver -B c:\Dev\mongo-c-driver\_build_x64 -D CMAKE_BUILD_TYPE=RelWithDebInfo -D BUILD_VERSION="2.2.1" -D ENABLE_MONGOC=OFF -D ENABLE_UNINSTALL=OFF -A x64
   cmake --build c:\Dev\mongo-c-driver\_build_x64 --config RelWithDebInfo --parallel
   cmake --install "c:\Dev\mongo-c-driver\_build_x64" --prefix "c:\Dev\mongo-c-driver\_install_x64" --config RelWithDebInfo
   cmake -D ENABLE_MONGOC=ON c:\Dev\mongo-c-driver\_build_x64
   cmake --build c:\Dev\mongo-c-driver\_build_x64 --config RelWithDebInfo --parallel
   cmake --install "c:\Dev\mongo-c-driver\_build_x64" --prefix "c:\Dev\mongo-c-driver\_install_x64" --config RelWithDebInfo

---- win32:
   
   cmake -S c:\Dev\mongo-c-driver -B c:\Dev\mongo-c-driver\_build_win32 -D CMAKE_BUILD_TYPE=RelWithDebInfo -D BUILD_VERSION="2.2.1" -D ENABLE_MONGOC=OFF -D ENABLE_UNINSTALL=OFF -A Win32
   cmake --build c:\Dev\mongo-c-driver\_build_win32 --config RelWithDebInfo --parallel
   cmake --install "c:\Dev\mongo-c-driver\_build_win32" --prefix "c:\Dev\mongo-c-driver\_install_win32" --config RelWithDebInfo
   cmake -D ENABLE_MONGOC=ON c:\Dev\mongo-c-driver\_build_win32
   cmake --build c:\Dev\mongo-c-driver\_build_win32 --config RelWithDebInfo --parallel
   cmake --install "c:\Dev\mongo-c-driver\_build_win32" --prefix "c:\Dev\mongo-c-driver\_install_win32" --config RelWithDebInfo

5) Copy the produced files into Alcinoe
---------------------------------------

Copy (x64):
- From:
  C:\Dev\mongo-c-driver\_install_x64\bin\
  to:
  C:\Dev\MagicFoundation\Alcinoe\Libraries\dll\mongodb\win64\

- From:
  C:\Dev\mongo-c-driver\_install_x64\include\
  to:
  C:\Dev\MagicFoundation\Alcinoe\Libraries\dll\mongodb\win64\include\

Copy (Win32):
- From:
  C:\Dev\mongo-c-driver\_install_win32\bin\
  to:
  C:\Dev\MagicFoundation\Alcinoe\Libraries\dll\mongodb\win32\

- From:
  C:\Dev\mongo-c-driver\_install_win32\include\
  to:
  C:\Dev\MagicFoundation\Alcinoe\Libraries\dll\mongodb\win32\include\

6) Regenerate the Delphi wrapper
--------------------------------

Run:
  Alcinoe\Tools\CHeaderWrapperGenerator\CHeaderWrapperGenerator.exe

This regenerates:
  Alcinoe.MongoDB.Wrapper.pas