; Script created by Andreas Hausladen (Andreas.Hausladen@gmx.de) for the JCL
;
; CONDITIONAL COMPILATION
;    Include_Binaries    Create an installer that can install a precompiled JCL
;    Include_Examples    Add the Examples directory to the installer (user can then select the component)
;    DEBUGGING           Development. Uses fast compression (script debugging)
;    Include_DelphiX     Include the binaries for Delphi X (X in 6..24)

#ifndef CmdLineBuild
#define JclRoot "..\Jcl"
#define JclLib "setupbuild\lib"
#define JclBpl "setupbuild\bpl"
#define JclHpp "setupbuild\hpp"
#define DEBUGGING
#endif

#define Include_SingleIDE
#define Include_Binaries
#define Include_Examples

#include "Settings.iss"

#define MyAppName "Jedi Code Library"
#define MyAppVerName "Jedi Code Library " + JclVersionStr
#define MyAppPublisher "JCL Team"
#define MyAppURL "http://jcl.sourceforge.net/"

;---------------------------------------------------
; Setup the preprocessor defines for the binary files
#ifdef Include_SingleIDE
#define JclLib6     JclLib
#define   JclBpl6   JclBpl
#define JclLib7     JclLib
#define   JclBpl7   JclBpl
#define JclLib9     JclLib
#define   JclBpl9   JclBpl
#define JclLib10    JclLib
#define   JclBpl10  JclBpl
#define   JclHpp10  JclHpp
#define JclLib11    JclLib
#define   JclBpl11  JclBpl
#define   JclHpp11  JclHpp
#define JclLib12    JclLib
#define   JclBpl12  JclBpl
#define   JclHpp12  JclHpp
#define JclLib14    JclLib
#define   JclBpl14  JclBpl
#define   JclHpp14  JclHpp
#define JclLib15    JclLib
#define   JclBpl15  JclBpl
#define   JclHpp15  JclHpp
#define JclLib16    JclLib
#define   JclBpl16  JclBpl
#define   JclHpp16  JclHpp
#define JclLib17    JclLib
#define   JclBpl17  JclBpl
#define   JclHpp17  JclHpp
#define JclLib18    JclLib
#define   JclBpl18  JclBpl
#define   JclHpp18  JclHpp
#define JclLib19    JclLib
#define   JclBpl19  JclBpl
#define   JclHpp19  JclHpp
#define JclLib20    JclLib
#define   JclBpl20  JclBpl
#define   JclHpp20  JclHpp
#define JclLib21    JclLib
#define   JclBpl21  JclBpl
#define   JclHpp21  JclHpp
#define JclLib22    JclLib
#define   JclBpl22  JclBpl
#define   JclHpp22  JclHpp
#define JclLib23    JclLib
#define   JclBpl23  JclBpl
#define   JclHpp23  JclHpp
#define JclLib24    JclLib
#define   JclBpl24  JclBpl
#define   JclHpp24  JclHpp
#define JclLib25    JclLib
#define   JclBpl25  JclBpl
#define   JclHpp25  JclHpp
#endif
;---------------------------------------------------

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppVersion={#JclVersionStr}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\DelphiComponents\JCL
DefaultGroupName=DelphiComponents\JEDI Code Library
DisableProgramGroupPage=no
;LicenseFile={#JclRoot}\help\MPL-1.1.html
OutputBaseFilename=JCLSetup
PrivilegesRequired=none
#ifdef DEBUGGING
Compression=zip/1
#else
Compression=lzma/ultra64
#endif
SolidCompression=yes
ShowLanguageDialog=auto
OptimizedChecks=yes


; for skin
#define MyWizardBottomImageFile "Skin\images\wizardbottom.bmp"
#define MyWizardButtonImageFile "Skin\images\button.bmp"
#define MyWizardImageFile "wizard.bmp"
#define MyWizardSmallImageFile "wizardsmall.bmp"
WizardImageFile=Skin\images\{#MyWizardImageFile}
WizardSmallImageFile=Skin\images\{#MyWizardSmallImageFile}
#include "Skin\isxskin.iss"

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "french"; MessagesFile: "compiler:Languages\French.isl"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"

#ifdef Include_Binaries
[Types]
Name: "full"; Description: "Full installation"
Name: "compact"; Description: "Source only installation"
Name: "custom"; Description: "Custom installation"; Flags: iscustom
Name: "prefered"; Description: "Prefered installation"
#endif

[Components]
#ifdef Include_Examples
Name: "Examples"; Description: "Example projects"; Types: full;
#endif

#ifdef Include_Binaries

#include "IdeComponents.iss"

[Components]
; Package selection
Name: "Experts"; Description: "Register IDE Experts"; Types: full prefered
Name: "Experts\JclDebugExpert"; Description: "Debug Expert"; Types: full prefered
Name: "Experts\JclFavoriteFoldersExpert"; Description: "Favorite Folder Expert"; Types: full prefered
Name: "Experts\JclProjectAnalysisExpert"; Description: "Project Analysis Expert"; Types: full prefered
Name: "Experts\JclRepositoryExpert"; Description: "Repository Expert"; Types: full prefered
Name: "Experts\JclSIMDViewExpert"; Description: "SIMD-View Expert"; Types: full prefered
Name: "Experts\JclThreadNameExpert"; Description: "Thread Name Expert"; Types: full prefered
Name: "Experts\JclUsesExpert"; Description: "Uses Expert"; Types: full prefered
Name: "Experts\JclVersionControlExpert"; Description: "Version Control Expert"

#endif

[Dirs]
Name: "{app}\bin"

[Files]
Source: {#JclRoot}\*.bat; DestDir: "{app}"; Flags: ignoreversion
Source: {#JclRoot}\*.sh; DestDir: "{app}"; Flags: ignoreversion
Source: {#JclRoot}\*.txt; DestDir: "{app}"; Flags: ignoreversion
Source: {#JclRoot}\*.proj; DestDir: "{app}"; Flags: ignoreversion
Source: {#JclRoot}\devtools\*; DestDir: "{app}\devtools"; Excludes: ".svn,__history"; Flags: ignoreversion sortfilesbyextension recursesubdirs
Source: {#JclRoot}\docs\*; DestDir: "{app}\docs"; Excludes: ".svn,__history"; Flags: ignoreversion sortfilesbyextension recursesubdirs
Source: {#JclRoot}\experts\*; DestDir: "{app}\experts"; Excludes: ".svn,__history,*.~*,*.txt"; Flags: ignoreversion sortfilesbyextension recursesubdirs
Source: {#JclRoot}\install\*; DestDir: "{app}\install"; Excludes: ".svn,__history,*.~*,ISS,dcc32.cfg,*.cmd,*.local,*.identcache"; Flags: ignoreversion recursesubdirs sortfilesbyextension
Source: {#JclRoot}\lib\*; DestDir: "{app}\lib"; Excludes: ".svn,__history,*.dcu,*.obj,*.dcp,*.lib,*.bpi,*.dfm,*.res,*.txt,*.a,*.o"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JclRoot}\packages\*; DestDir: "{app}\packages"; Excludes: ".svn,__history,*.drc,*.txt,*.identcache,*.local,*.~*,*.dcu,*.a,*.o"; Flags: ignoreversion sortfilesbyextension recursesubdirs
Source: {#JclRoot}\source\*; DestDir: "{app}\source"; Excludes: ".git,.svn,__history,*.~*,*.hpp,*.txt"; Flags: ignoreversion sortfilesbyextension recursesubdirs
#ifdef Include_Examples
; SolidBreak
Source: {#JclRoot}\examples\*; DestDir: "{app}\examples"; Excludes: ".svn,__history,*.dcu,*.obj,*.exe,*.map,*.bpl,*.dcp,*.~*,*.drc,*.local,*.a,*.o"; Components: "Examples"; Flags: ignoreversion recursesubdirs sortfilesbyextension solidbreak
#endif

#ifdef Include_Binaries
#ifdef Include_Delphi6
; SolidBreak;
Source: {#JclLib6}\*; DestDir: "{app}\lib\d6"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi6"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl6}\*; DestDir: "{code:GetDelphiBplDir|6}"; Components: "IDE\Delphi6"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
#endif
#ifdef Include_Delphi7
; SolidBreak;
Source: {#JclLib7}\*; DestDir: "{app}\lib\d7"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi7"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl7}\*; DestDir: "{code:GetDelphiBplDir|7}"; Components: "IDE\Delphi7"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
#endif
#ifdef Include_Delphi9
; SolidBreak;
Source: {#JclLib9}\*; DestDir: "{app}\lib\d9"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi9"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl9}\*; DestDir: "{code:GetDelphiBplDir|9}"; Components: "IDE\Delphi9"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
#endif
#ifdef Include_Delphi10
; SolidBreak;
Source: {#JclLib10}\*; DestDir: "{app}\lib\d10"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi10"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl10}\*; DestDir: "{code:GetDelphiBplDir|10}"; Components: "IDE\Delphi10"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JclHpp10}\*; DestDir: "{code:GetHPPDir|10}"; Components: "IDE\Delphi10"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi11
; SolidBreak;
Source: {#JclLib11}\*; DestDir: "{app}\lib\d11"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi11"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl11}\*; DestDir: "{code:GetDelphiBplDir|11}"; Components: "IDE\Delphi11"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JclHpp11}\*; DestDir: ""{app}\include\d11"; Components: "IDE\Delphi11"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi12
; SolidBreak;
Source: {#JclLib12}\*; DestDir: "{app}\lib\d12"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi12"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl12}\*; DestDir: "{code:GetDelphiBplDir|12}"; Components: "IDE\Delphi12"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JclHpp12}\*; DestDir: ""{app}\include\d12"; Components: "IDE\Delphi12"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi14
; SolidBreak;
Source: {#JclJcl14}\*; DestDir: "{app}\lib\d14"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi14"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl14}\*; DestDir: "{code:GetDelphiBplDir|14}"; Components: "IDE\Delphi14"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JclHpp14}\*; DestDir: ""{app}\include\d14"; Components: "IDE\Delphi14"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi15
; SolidBreak;
Source: {#JclLib15}\*; DestDir: "{app}\lib\d15"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi15"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl15}\*; DestDir: "{code:GetDelphiBplDir|15}"; Components: "IDE\Delphi15"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JclHpp15}\*; DestDir: "{app}\include\d15"; Components: "IDE\Delphi15"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi16
; SolidBreak;
Source: {#JclLib16}\*; DestDir: "{app}\lib\d16"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi16"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl16}\*; DestDir: "{code:GetDelphiBplDir|16}"; Components: "IDE\Delphi16"; Flags: ignoreversion sortfilesbyextension
Source: {#JclBpl16}\Win64\*; DestDir: "{code:GetDelphiBplDir|16}\Win64"; Components: "IDE\Delphi16"; Flags: ignoreversion sortfilesbyextension
Source: {#JclHpp16}\*; DestDir: "{app}\include\d16"; Components: "IDE\Delphi16"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi17
; SolidBreak;
Source: {#JclLib17}\*; DestDir: "{app}\lib\d17"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi17"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl17}\*; DestDir: "{code:GetDelphiBplDir|17}"; Components: "IDE\Delphi17"; Flags: ignoreversion sortfilesbyextension
Source: {#JclBpl17}\Win64\*; DestDir: "{code:GetDelphiBplDir|17}\Win64"; Components: "IDE\Delphi17"; Flags: ignoreversion sortfilesbyextension
Source: {#JclHpp17}\*; DestDir: "{app}\include\d17"; Components: "IDE\Delphi17"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi18
; SolidBreak;
Source: {#JclLib18}\*; DestDir: "{app}\lib\d18"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi18"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl18}\*; DestDir: "{code:GetDelphiBplDir|18}"; Components: "IDE\Delphi18"; Flags: ignoreversion sortfilesbyextension
Source: {#JclBpl18}\Win64\*; DestDir: "{code:GetDelphiBplDir|18}\Win64"; Components: "IDE\Delphi18"; Flags: ignoreversion sortfilesbyextension
Source: {#JclHpp18}\*; DestDir: "{app}\include\d18"; Components: "IDE\Delphi18"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi19
; SolidBreak;
Source: {#JclLib19}\*; DestDir: "{app}\lib\d19"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi19"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl19}\*; DestDir: "{code:GetDelphiBplDir|19}"; Components: "IDE\Delphi19"; Flags: ignoreversion sortfilesbyextension
Source: {#JclBpl19}\Win64\*; DestDir: "{code:GetDelphiBplDir|19}\Win64"; Components: "IDE\Delphi19"; Flags: ignoreversion sortfilesbyextension
Source: {#JclHpp19}\*; DestDir: "{app}\include\d19"; Components: "IDE\Delphi19"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi20
; SolidBreak;
Source: {#JclLib20}\*; DestDir: "{app}\lib\d20"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi20"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl20}\*; DestDir: "{code:GetDelphiBplDir|20}"; Components: "IDE\Delphi20"; Flags: ignoreversion sortfilesbyextension
Source: {#JclBpl20}\Win64\*; DestDir: "{code:GetDelphiBplDir|20}\Win64"; Components: "IDE\Delphi20"; Flags: ignoreversion sortfilesbyextension
Source: {#JclHpp20}\*; DestDir: "{app}\include\d20"; Components: "IDE\Delphi20"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi21
; SolidBreak;
Source: {#JclLib21}\*; DestDir: "{app}\lib\d21"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi21"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl21}\*; DestDir: "{code:GetDelphiBplDir|21}"; Components: "IDE\Delphi21"; Flags: ignoreversion sortfilesbyextension
Source: {#JclBpl21}\Win64\*; DestDir: "{code:GetDelphiBplDir|21}\Win64"; Components: "IDE\Delphi21"; Flags: ignoreversion sortfilesbyextension
Source: {#JclHpp21}\*; DestDir: "{app}\include\d21"; Components: "IDE\Delphi21"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi22
; SolidBreak;
Source: {#JclLib22}\*; DestDir: "{app}\lib\d22"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi22"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl22}\*; DestDir: "{code:GetDelphiBplDir|22}"; Components: "IDE\Delphi22"; Flags: ignoreversion sortfilesbyextension
Source: {#JclBpl22}\Win64\*; DestDir: "{code:GetDelphiBplDir|22}\Win64"; Components: "IDE\Delphi22"; Flags: ignoreversion sortfilesbyextension
Source: {#JclHpp22}\*; DestDir: "{app}\include\d22"; Components: "IDE\Delphi22"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi23
; SolidBreak;
Source: {#JclLib23}\*; DestDir: "{app}\lib\d23"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi23"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl23}\*; DestDir: "{code:GetDelphiBplDir|23}"; Components: "IDE\Delphi23"; Flags: ignoreversion sortfilesbyextension
Source: {#JclBpl23}\Win64\*; DestDir: "{code:GetDelphiBplDir|23}\Win64"; Components: "IDE\Delphi23"; Flags: ignoreversion sortfilesbyextension
Source: {#JclHpp23}\*; DestDir: "{app}\include\d23"; Components: "IDE\Delphi23"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi24
; SolidBreak;
Source: {#JclLib24}\*; DestDir: "{app}\lib\d24"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi24"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl24}\*; DestDir: "{code:GetDelphiBplDir|24}"; Components: "IDE\Delphi24"; Flags: ignoreversion sortfilesbyextension
Source: {#JclBpl24}\Win64\*; DestDir: "{code:GetDelphiBplDir|24}\Win64"; Components: "IDE\Delphi24"; Flags: ignoreversion sortfilesbyextension
Source: {#JclHpp24}\*; DestDir: "{app}\include\d24"; Components: "IDE\Delphi24"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi25
; SolidBreak;
Source: {#JclLib25}\*; DestDir: "{app}\lib\d25"; Excludes: ".svn,__history,*.txt,*.hpp"; Components: "IDE\Delphi25"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JclBpl25}\*; DestDir: "{code:GetDelphiBplDir|25}"; Components: "IDE\Delphi25"; Flags: ignoreversion sortfilesbyextension
Source: {#JclBpl25}\Win64\*; DestDir: "{code:GetDelphiBplDir|25}\Win64"; Components: "IDE\Delphi25"; Flags: ignoreversion sortfilesbyextension
Source: {#JclHpp25}\*; DestDir: "{app}\include\d25"; Components: "IDE\Delphi25"; Flags: ignoreversion sortfilesbyextension
#endif

#endif

; only source code => execute Jedi Installer
[Run]
Filename: {app}\install.bat; Description: "Execute JEDI Installer"; Flags: postinstall shellexec; Check: IsSourceInstall;

#ifdef Include_Binaries
[Registry]
#ifdef Include_Delphi6
; Delphi 6
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|6}; Components: "IDE\Delphi6"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d6; Components: "IDE\Delphi6"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi6"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi6"; Flags: uninsdeletevalue;
#endif
#ifdef Include_Delphi7
; Delphi 7
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|7}; Components: "IDE\Delphi7"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d7; Components: "IDE\Delphi7"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi7"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi7"; Flags: uninsdeletevalue;
#endif
#ifdef Include_Delphi9
; Delphi 2005
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|9}; Components: "IDE\Delphi9"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d9; Components: "IDE\Delphi9"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi9"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi9"; Flags: uninsdeletevalue;
#endif
#ifdef Include_Delphi10
; Delphi 2006
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|10}; Components: "IDE\Delphi10"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d10; Components: "IDE\Delphi10"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi10"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi10"; Flags: uninsdeletevalue;
#endif
#ifdef Include_Delphi11
; Delphi 2007
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|11}; Components: "IDE\Delphi11"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d11; Components: "IDE\Delphi11"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi11"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi11"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi11";
#endif
#ifdef Include_Delphi12
; Delphi 2009
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|12}; Components: "IDE\Delphi12"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d12; Components: "IDE\Delphi12"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi12"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi12"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi12";
#endif
#ifdef Include_Delphi14
; Delphi 2010
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|14}; Components: "IDE\Delphi14"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d14; Components: "IDE\Delphi14"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi14"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi14"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi14";
#endif
#ifdef Include_Delphi15
; Delphi XE
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|15}; Components: "IDE\Delphi15"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d15; Components: "IDE\Delphi15"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi15"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi15"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi15";
#endif
#ifdef Include_Delphi16
; Delphi XE2
Root: HKCU; Subkey: "{code:GetDelphiRegKey|16}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|16}; Components: "IDE\Delphi16"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|16}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d16; Components: "IDE\Delphi16"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|16}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi16"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|16}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi16"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|16}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi16";
#endif
#ifdef Include_Delphi17
; Delphi XE3
Root: HKCU; Subkey: "{code:GetDelphiRegKey|17}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|17}; Components: "IDE\Delphi17"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|17}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d17; Components: "IDE\Delphi17"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|17}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi17"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|17}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi17"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|17}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi17";
#endif
#ifdef Include_Delphi18
; Delphi XE4
Root: HKCU; Subkey: "{code:GetDelphiRegKey|18}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|18}; Components: "IDE\Delphi18"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|18}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d18; Components: "IDE\Delphi18"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|18}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi18"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|18}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi18"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|18}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi18";
#endif
#ifdef Include_Delphi19
; Delphi XE5
Root: HKCU; Subkey: "{code:GetDelphiRegKey|19}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|19}; Components: "IDE\Delphi19"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|19}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d19; Components: "IDE\Delphi19"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|19}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi19"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|19}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi19"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|19}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi19";
#endif
#ifdef Include_Delphi20
; Delphi XE6
Root: HKCU; Subkey: "{code:GetDelphiRegKey|20}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|20}; Components: "IDE\Delphi20"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|20}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d20; Components: "IDE\Delphi20"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|20}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi20"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|20}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi20"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|20}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi20";
#endif
#ifdef Include_Delphi21
; Delphi XE7
Root: HKCU; Subkey: "{code:GetDelphiRegKey|21}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|21}; Components: "IDE\Delphi21"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|21}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d21; Components: "IDE\Delphi21"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|21}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi21"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|21}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi21"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|21}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi21";
#endif
#ifdef Include_Delphi22
; Delphi XE8
Root: HKCU; Subkey: "{code:GetDelphiRegKey|22}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|22}; Components: "IDE\Delphi22"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|22}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d22; Components: "IDE\Delphi22"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|22}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi22"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|22}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi22"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|22}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi22";
#endif
#ifdef Include_Delphi23
; Delphi 10 Seattle
Root: HKCU; Subkey: "{code:GetDelphiRegKey|23}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|23}; Components: "IDE\Delphi23"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|23}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d23; Components: "IDE\Delphi23"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|23}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi23"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|23}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi23"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|23}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi23";
#endif
#ifdef Include_Delphi24
; Delphi 10.1 Berlin
Root: HKCU; Subkey: "{code:GetDelphiRegKey|24}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|24}; Components: "IDE\Delphi24"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|24}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d24; Components: "IDE\Delphi24"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|24}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi24"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|24}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi24"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|24}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi24";
#endif
#ifdef Include_Delphi25
; Delphi 10.2
Root: HKCU; Subkey: "{code:GetDelphiRegKey|25}\Jedi\JCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|25}; Components: "IDE\Delphi25"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|25}\Jedi\JCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d25; Components: "IDE\Delphi25"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|25}\Jedi\JCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi25"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|25}\Jedi\JCL"; ValueType: string; ValueName: "Version"; ValueData: {#JclVersionStr}; Components: "IDE\Delphi25"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|25}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi25";
#endif

#endif



[UninstallDelete]
Type: files; Name: "{app}\bin\JediInstaller.exe"
Type: files; Name: "{app}\bin\JCL-install.ini"
Type: files; Name: "{app}\bin\*.log"
Type: files; Name: "{app}\source\*.hpp"
Type: files; Name: "{app}\source\*.~*"
Type: files; Name: "{app}\source\common\*.hpp"
Type: files; Name: "{app}\source\vcl\*.hpp"
Type: files; Name: "{app}\source\windows\*.hpp"
Type: files; Name: "{app}\source\common\*.dcu"
Type: files; Name: "{app}\source\vcl\*.dcu"
Type: files; Name: "{app}\source\windows\*.dcu"
; lib\Delphi 6
Type: files; Name: "{app}\lib\d6\*"
Type: files; Name: "{app}\lib\d6\debug\*"
Type: files; Name: "{code:GetDelphiBplDir|6}\Jcl*.~bpl";
; lib\Delphi 7
Type: files; Name: "{app}\lib\d7\*"
Type: files; Name: "{app}\lib\d7\debug\*"
Type: files; Name: "{code:GetDelphiBplDir|7}\Jcl*.~bpl";
; lib\Delphi 2005
Type: files; Name: "{app}\lib\d9\*"
Type: files; Name: "{app}\lib\d9\debug\*"
Type: files; Name: "{code:GetDelphiBplDir|9}\Jcl*.~bpl";
; lib\Delphi/C++Builder 2006
Type: files; Name: "{app}\lib\d10\*"
Type: files; Name: "{app}\lib\d10\debug\*"
Type: files; Name: "{code:GetDelphiBplDir|10}\Jcl*.~bpl";
; lib\Delphi/C++Builder 2007
Type: files; Name: "{app}\lib\d11\*"
Type: files; Name: "{app}\lib\d11\debug\*"
Type: files; Name: "{app}\include\d11\*"
Type: files; Name: "{code:GetDelphiBplDir|11}\Jcl*.~bpl";
; lib\Delphi/C++Builder 2009
Type: files; Name: "{app}\lib\d12\*"
Type: files; Name: "{app}\lib\d12\debug\*"
Type: files; Name: "{app}\include\d12\*"
Type: files; Name: "{code:GetDelphiBplDir|12}\Jcl*.~bpl";
; lib\Delphi/C++Builder 2010
Type: files; Name: "{app}\lib\d14\*"
Type: files; Name: "{app}\lib\d14\debug\*"
Type: files; Name: "{app}\include\d14\*"
Type: files; Name: "{code:GetDelphiBplDir|14}\Jcl*.~bpl";
; lib\Delphi/C++Builder XE
Type: files; Name: "{app}\lib\d15\*"
Type: files; Name: "{app}\lib\d15\debug\*"
Type: files; Name: "{app}\include\d15\*"
Type: files; Name: "{code:GetDelphiBplDir|15}\Jcl*.~bpl";
; lib\Delphi/C++Builder XE2
Type: files; Name: "{app}\lib\d16\win32\*"
Type: files; Name: "{app}\lib\d16\win32\debug\*"
Type: files; Name: "{app}\lib\d16\win64\*"
Type: files; Name: "{app}\lib\d16\win64\debug\*"
Type: files; Name: "{app}\include\d16\*"
Type: files; Name: "{code:GetDelphiBplDir|16}\Jcl*.~bpl";
; lib\Delphi/C++Builder XE3
Type: files; Name: "{app}\lib\d17\win32\*"
Type: files; Name: "{app}\lib\d17\win32\debug\*"
Type: files; Name: "{app}\lib\d17\win64\*"
Type: files; Name: "{app}\lib\d17\win64\debug\*"
Type: files; Name: "{app}\include\d17\*"
Type: files; Name: "{code:GetDelphiBplDir|17}\Jcl*.~bpl";
; lib\Delphi/C++Builder XE4
Type: files; Name: "{app}\lib\d18\win32\*"
Type: files; Name: "{app}\lib\d18\win32\debug\*"
Type: files; Name: "{app}\lib\d18\win64\*"
Type: files; Name: "{app}\lib\d18\win64\debug\*"
Type: files; Name: "{app}\include\d18\*"
Type: files; Name: "{code:GetDelphiBplDir|18}\Jcl*.~bpl";
; lib\Delphi/C++Builder XE5
Type: files; Name: "{app}\lib\d19\win32\*"
Type: files; Name: "{app}\lib\d19\win32\debug\*"
Type: files; Name: "{app}\lib\d19\win64\*"
Type: files; Name: "{app}\lib\d19\win64\debug\*"
Type: files; Name: "{app}\include\d19\*"
Type: files; Name: "{code:GetDelphiBplDir|19}\Jcl*.~bpl";
; lib\Delphi/C++Builder XE6
Type: files; Name: "{app}\lib\d20\win32\*"
Type: files; Name: "{app}\lib\d20\win32\debug\*"
Type: files; Name: "{app}\lib\d20\win64\*"
Type: files; Name: "{app}\lib\d20\win64\debug\*"
Type: files; Name: "{app}\include\d20\*"
Type: files; Name: "{code:GetDelphiBplDir|20}\Jcl*.~bpl";
; lib\Delphi/C++Builder XE7
Type: files; Name: "{app}\lib\d21\win32\*"
Type: files; Name: "{app}\lib\d21\win32\debug\*"
Type: files; Name: "{app}\lib\d21\win64\*"
Type: files; Name: "{app}\lib\d21\win64\debug\*"
Type: files; Name: "{app}\include\d21\*"
Type: files; Name: "{code:GetDelphiBplDir|21}\Jcl*.~bpl";
; lib\Delphi/C++Builder XE8
Type: files; Name: "{app}\lib\d22\win32\*"
Type: files; Name: "{app}\lib\d22\win32\debug\*"
Type: files; Name: "{app}\lib\d22\win64\*"
Type: files; Name: "{app}\lib\d22\win64\debug\*"
Type: files; Name: "{app}\include\d22\*"
Type: files; Name: "{code:GetDelphiBplDir|22}\Jcl*.~bpl";
; lib\Delphi/C++Builder 10 Seattle
Type: files; Name: "{app}\lib\d23\win32\*"
Type: files; Name: "{app}\lib\d23\win32\debug\*"
Type: files; Name: "{app}\lib\d23\win64\*"
Type: files; Name: "{app}\lib\d23\win64\debug\*"
Type: files; Name: "{app}\include\d23\*"
Type: files; Name: "{code:GetDelphiBplDir|23}\Jcl*.~bpl";
; lib\Delphi/C++Builder 10.1 Berlin
Type: files; Name: "{app}\lib\d24\win32\*"
Type: files; Name: "{app}\lib\d24\win32\debug\*"
Type: files; Name: "{app}\lib\d24\win64\*"
Type: files; Name: "{app}\lib\d24\win64\debug\*"
Type: files; Name: "{app}\include\d24\*"
Type: files; Name: "{code:GetDelphiBplDir|24}\Jcl*.~bpl";
; lib\Delphi/C++Builder 10.2
Type: files; Name: "{app}\lib\d25\win32\*"
Type: files; Name: "{app}\lib\d25\win32\debug\*"
Type: files; Name: "{app}\lib\d25\win64\*"
Type: files; Name: "{app}\lib\d25\win64\debug\*"
Type: files; Name: "{app}\include\d25\*"
Type: files; Name: "{code:GetDelphiBplDir|25}\Jcl*.~bpl";

[Icons]
Name: "{group}\{cm:ProgramOnTheWeb,{#MyAppName}}"; Filename: "{#MyAppURL}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"

#include "ComponentInstallerScript.iss"

[Code]
// callbacks for the ComponentInstaller

procedure UserRegisterComponents(Components: TStrings);
begin
end;

procedure UserUnregisterComponents(Components: TStrings);
// uninstall all JCL experts, not only the one that the user had selected
// during the installation. The user could have started the JediInstaller
// or have added additional designtime packages by hand.
var
  IdeList: TStrings;
  IdeIndex: Integer;
  IdeKind: TIdeKind;
  Version: Integer;
begin
{  // Uninstall from all IDEs ?
  for Version := 6 to LastInstalledIDEVersionNumber do
    UninstallExpertsPrefixed(ikDelphi, Version, 'Jcl');
  for Version := 6 to 6 do
    UninstallExpertsPrefixed(ikBCB, Version, 'Jcl');}

  IdeList := TStringList.Create;
  try
    GetSelectedList(IdeList, 'IDE', Components);
    // unregister per IDE
    for IdeIndex := 0 to IdeList.Count - 1 do
    begin
      ExtractIdeInfo(IdeList[IdeIndex], IdeKind, Version);
      UninstallExpertsPrefixed(IdeKind, Version, 'Jcl');
    end;
  finally
    IdeList.Free;
  end;
end;

function MapExpert(IdeKind: TIdeKind; Version: Integer; const ExpertName: string): string;
begin
  if StartsText('Jcl', ExpertName) then
  begin
    if Version < 9 then
    begin
      case IdeKind of
        ikDelphi:
          Result := GetDelphiBplDir(IntToStr(Version)) + '\' + ExpertName + 'DLLd' + IntToStr(Version) + '0.dll';
        ikBCB:
          Result := GetBCBBplDir(IntToStr(Version)) + '\' + ExpertName + 'DLLc' + IntToStr(Version) + '0.dll';
      end;
    end
    else
      Result := GetDelphiBplDir(IntToStr(Version)) + '\' + ExpertName  + IntToStr(Version) + '0.bpl';
  end;
end;

function MapDesignPackage(IdeKind: TIdeKind; Version: Integer; const PackageName: string): string;
begin
  Result := '';
end;

procedure GetSearchPaths(IdeKind: TIdeKind; Version: Integer; var SearchPaths, DebugPaths, BrowsePaths, IncludePaths: string);
var
  LibDir, AppDir: string;
begin
  AppDir := ExpandConstant('{app}');
  SearchPaths := '';
  DebugPaths := '';
  BrowsePaths := '';
  IncludePaths := '';
  case IdeKind of
    ikDelphi:
	  if Version >= 16 then // XE2+
        LibDir := AppDir + '\lib\d' + IntToStr(Version) + '\$(Platform)' // is replaced by Win32/Win64 in the CompInstall.dll
	  else
        LibDir := AppDir + '\lib\d' + IntToStr(Version);
    ikBCB:
      LibDir := AppDir + '\lib\c' + IntToStr(Version);
  else
    Exit;
  end;
  
  SearchPaths := LibDir + ';' + AppDir + '\source\Include';
  DebugPaths := LibDir + '\debug';
  BrowsePaths := AppDir + '\source\common;' + AppDir + '\source\vcl;' + AppDir + '\source\windows';
  if Version >= 11 then
    IncludePaths := ExpandConstant('{app}') + '\include\d' + IntToStr(Version)
  else if Version = 10 then
    IncludePaths := GetHPPDir(IntToStr(Version));
end;

// events

function InitializeSetup(): Boolean;
begin
  Result := InitComponentInstaller;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssPostInstall  then
    RegisterComponents;
end;

function InitializeUninstall(): Boolean;
begin
  Result := InitComponentUninstaller;
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
  if CurUninstallStep = usUninstall then
    UnregisterComponents;
end;

// Skin

procedure CurPageChanged(CurPageID: Integer);
begin
  // update calls for skin
  UpdateButton(WizardForm.BackButton, bidBack);
  UpdateButton(WizardForm.NextButton, bidNext);
  UpdateButton(WizardForm.CancelButton, bidCancel);
end;

procedure InitializeWizard();
begin
  // initialize call for skin
  InitializeSkin;
end;

