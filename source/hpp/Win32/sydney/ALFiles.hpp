// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ALFiles.pas' rev: 34.00 (Windows)

#ifndef AlfilesHPP
#define AlfilesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <ALCommon.hpp>

//-- user supplied -----------------------------------------------------------

namespace Alfiles
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool __fastcall AlEmptyDirectory(System::AnsiString Directory, bool SubDirectory, const System::AnsiString *IgnoreFiles, const int IgnoreFiles_High, const bool RemoveEmptySubDirectory = true, const System::AnsiString FileNameMask = "*", const System::TDateTime MinFileAge = 0.000000E+00)/* overload */;
extern DELPHI_PACKAGE bool __fastcall AlEmptyDirectory(const System::AnsiString Directory, bool SubDirectory, const bool RemoveEmptySubDirectory = true, const System::AnsiString FileNameMask = "*", const System::TDateTime MinFileAge = 0.000000E+00)/* overload */;
extern DELPHI_PACKAGE bool __fastcall AlCopyDirectory(System::AnsiString SrcDirectory, System::AnsiString DestDirectory, bool SubDirectory, const System::AnsiString FileNameMask = "*", const bool FailIfExists = true);
extern DELPHI_PACKAGE __int64 __fastcall AlGetFileSize(const System::AnsiString AFileName);
extern DELPHI_PACKAGE System::AnsiString __fastcall AlGetFileVersion(const System::AnsiString AFileName);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALGetModuleFileNameWithoutExtension(void);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALGetModuleName(void);
extern DELPHI_PACKAGE System::AnsiString __fastcall ALGetModulePath(void);
extern DELPHI_PACKAGE System::TDateTime __fastcall ALGetFileCreationDateTime(const System::AnsiString aFileName);
extern DELPHI_PACKAGE System::TDateTime __fastcall ALGetFileLastWriteDateTime(const System::AnsiString aFileName);
extern DELPHI_PACKAGE System::TDateTime __fastcall ALGetFileLastAccessDateTime(const System::AnsiString aFileName);
extern DELPHI_PACKAGE void __fastcall ALSetFileCreationDateTime(const System::AnsiString aFileName, const System::TDateTime aCreationDateTime);
extern DELPHI_PACKAGE void __fastcall ALSetFileLastWriteDateTime(const System::AnsiString aFileName, const System::TDateTime aLastWriteDateTime);
extern DELPHI_PACKAGE void __fastcall ALSetFileLastAccessDateTime(const System::AnsiString aFileName, const System::TDateTime aLastAccessDateTime);
extern DELPHI_PACKAGE bool __fastcall ALIsDirectoryEmpty(const System::AnsiString directory);
extern DELPHI_PACKAGE bool __fastcall ALFileExists(const System::AnsiString Path);
extern DELPHI_PACKAGE bool __fastcall ALDirectoryExists(const System::AnsiString Directory);
extern DELPHI_PACKAGE bool __fastcall ALCreateDir(const System::AnsiString Dir);
extern DELPHI_PACKAGE bool __fastcall ALRemoveDir(const System::AnsiString Dir);
extern DELPHI_PACKAGE bool __fastcall ALDeleteFile(const System::AnsiString FileName);
extern DELPHI_PACKAGE bool __fastcall ALRenameFile(const System::AnsiString OldName, const System::AnsiString NewName);
extern DELPHI_PACKAGE bool __fastcall AlEmptyDirectoryU(System::UnicodeString Directory, bool SubDirectory, const System::UnicodeString *IgnoreFiles, const int IgnoreFiles_High, const bool RemoveEmptySubDirectory = true, const System::UnicodeString FileNameMask = L"*", const System::TDateTime MinFileAge = 0.000000E+00)/* overload */;
extern DELPHI_PACKAGE bool __fastcall AlEmptyDirectoryU(const System::UnicodeString Directory, bool SubDirectory, const bool RemoveEmptySubDirectory = true, const System::UnicodeString FileNameMask = L"*", const System::TDateTime MinFileAge = 0.000000E+00)/* overload */;
extern DELPHI_PACKAGE __int64 __fastcall ALGetFileSizeU(const System::UnicodeString FileName);
}	/* namespace Alfiles */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ALFILES)
using namespace Alfiles;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AlfilesHPP
