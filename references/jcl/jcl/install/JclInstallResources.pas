{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) extension                                                        }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclInstall.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Robert Rossmair - crossplatform & BCB support, refactoring                                     }
{   Florent Ouchet (outchy) - New installer core, resource refactorings                            }
{   Jean-Fabien Connault (cycocrew)                                                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclInstallResources;

{$I jcl.inc}

interface

uses
  JclStrings;

resourcestring
  // Captions
  RsCaptionBPLPath    = '&BPL path:';
  RsCaptionDCPPath    = '&DCP path:';
  RsCaptionBPIPath    = 'BP&I path:';
  RsCaptionHPPPath    = '&HPP path:';

  // License
  RsCaptionLicense          = 'MPL 1.1 License';
  RsCaptionLicenseAgreement = 'I agree with the terms of the MPL 1.1 license';
  RsMissingLicenseAgreement = 'Please agree to the terms of the MPL 1.1 license first';

  // Products
  RsCaptionLibrary = 'JEDI Code Library';

  // Conditional features
  RsCaptionDef                 = 'Conditional defines';
  RsCaptionDefThreadSafe       = 'Enable thread safe code';
  RsCaptionDefDropObsoleteCode = 'Drop obsolete code';
  RsCaptionDefUnitVersioning   = 'Include Unit Versioning';
  // math options
  RsCaptionDefMath              = 'Math options';
  RsCaptionDefMathPrecSingle    = 'Single float precision';
  RsCaptionDefMathPrecDouble    = 'Double float precision';
  RsCaptionDefMathPrecExtended  = 'Extended float precision';
  RsCaptionDefMathExtremeValues = 'Support for infinite and NaN';
  // debug options
  RsCaptionDefDebug             = 'Debug and exception hooking options';
  RsCaptionDefHookDllExceptions = 'Hook exceptions in DLL';
  RsCaptionDefDebugNoBinary     = 'No debug source from JEDI debug informations';
  RsCaptionDefDebugNoTD32       = 'No debug source from TD32 debug symbols';
  RsCaptionDefDebugNoMap        = 'No debug source from Map files';
  RsCaptionDefDebugNoExports    = 'No debug source from function export table for libraries';
  RsCaptionDefDebugNoSymbols    = 'No debug source from Microsoft debug symbols';
  // Wrapper options
  RsCaptionDefWrappers          = 'Wrapper options';
  // PCRE options
  RsCaptionDefPCRE              = 'PCRE options';
  RsCaptionDefPCREStaticLink    = 'Static link to PCRE code';
  RsCaptionDefPCRELinkDLL       = 'Static bind to pcre.dll';
  RsCaptionDefPCRELinkOnRequest = 'Late bind to pcre.dll';
  RsCaptionDefPCRERTL           = 'Use RTL''s RegularExpressionAPI';
  RsCaptionDefPCRE8             = 'Enable 8-bit PCRE';
  RsCaptionDefPCRE16            = 'Enable 16-bit PCRE';
  RsCaptionDefPCREPrefer16      = 'Prefer 16-bit PCRE';
  // BZip2 options
  RsCaptionDefBZip2              = 'BZip2 options';
  RsCaptionDefBZip2StaticLink    = 'Static link to BZip2 code';
  RsCaptionDefBZip2LinkDLL       = 'Static bind to bzip2.dll';
  RsCaptionDefBZip2LinkOnRequest = 'Late bind to bzip2.dll';
  // ZLib options
  RsCaptionDefZLib              = 'ZLib options';
  RsCaptionDefZLibStaticLink    = 'Static link to ZLib code';
  RsCaptionDefZLibLinkDLL       = 'Static bind to zlib1.dll';
  RsCaptionDefZLibLinkOnRequest = 'Late bind to zlib1.dll';
  RsCaptionDefZLibRTL           = 'Use RTL''s ZLib';
  // Unicode options
  RsCaptionDefUnicode              = 'Unicode options';
  RsCaptionDefUnicodeRTLDatabase   = 'Prefer RTL database';
  RsCaptionDefUnicodeSilentFailure = 'Silent failure';
  RsCaptionDefUnicodeRawData       = 'Uncompressed Unicode data';
  RsCaptionDefUnicodeZLibData      = 'Compressed data using zlib';
  RsCaptionDefUnicodeBZip2Data     = 'Compressed data using bzip2';
  // Container options
  RsCaptionDefContainer           = 'Container options';
  RsCaptionDefContainerAnsiStr    = 'Alias AnsiString containers to String containers';
  RsCaptionDefContainerWideStr    = 'Alias WideString containers to String containers';
  RsCaptionDefContainerUnicodeStr = 'Alias UnicodeString containers to String containers (Delphi 2009 only)';
  RsCaptionDefContainerNoStr      = 'Do not alias anything';
  // 7Z options
  RsCaptionDef7z               = 'Sevenzip options';
  //RsCaptionDef7zStaticLink     = 'Static link to Sevenzip code (not supported yet)';
  RsCaptionDef7zLinkDLL        = 'Static bind to 7z.dll';
  RsCaptionDef7zLinkOnRequest  = 'Late bind to 7z.dll';

  // post compilation
  RsCaptionMapCreate  = 'Create MAP files';
  RsCaptionJdbgCreate = 'Create JEDI Debug Informations';
  RsCaptionJdbgInsert = 'Insert JEDI Debug Informations in the libraries';
  RsCaptionMapDelete  = 'Do not keep MAP files';

  // environment
  RsCaptionEnvironment     = 'Environment';
  RsCaptionEnvLibPath      = 'Add JCL to IDE Library Path';
  RsCaptionEnvBrowsingPath = 'Add JCL to IDE Browsing Path';
  RsCaptionEnvDebugDCUPath = 'Add JCL to Debug DCU Path';
  RsCaptionEnvIncludePath  = 'Add JCL to C++ Include Path';

  // make units
  RsCaptionMake          = 'Make library units';
  RsCaptionMakeRelease   = 'Release';
  RsCaptionMakeDebug     = 'Debug';
  RsCaptionCopyHppFiles  = 'Copy HPP files to %s';
  RsCaptionHppDirectory  = 'configured HPP directory';
  RsCaptionCheckHppFiles = 'Check HPP files';

  // packages
  RsCaptionPackages             = 'Packages';
  RsCaptionDualPackages         = 'Dual packages';
  RsCaptionCopyPackagesHppFiles = 'Output HPP files to %s';

  // exception dialogs
  RsCaptionExceptDlg       = 'Sample Exception Dialogs in the Object Repository';
  RsCaptionExceptDlgVCL    = 'VCL Exception Dialog';
  RsCaptionExceptDlgVCLSnd = 'VCL Exception Dialog with Send button';

  // experts
  RsCaptionExperts                = 'IDE experts';
  RsCaptionExpertsDsgnPackages    = 'Design packages';
  RsCaptionExpertsDLL             = 'DLL experts';
  RsCaptionExpertDebug            = 'Debug Extension';
  RsCaptionExpertAnalyzer         = 'Project Analyzer';
  RsCaptionExpertFavorite         = 'Favorite combobox in Open/Save dialogs';
  RsCaptionExpertRepository       = 'Exception dialog expert';
  RsCaptionExpertThreadNames      = 'Displaying thread names in Thread Status window';
  RsCaptionExpertUses             = 'Uses Wizard';
  RsCaptionExpertSimdView         = 'Debug window for XMM registers';
  RsCaptionExpertVersionControl   = 'Version control';
  RsCaptionExpertStackTraceViewer = 'Stack Trace Viewer';

  // help
  RsCaptionHelp          = 'Help files';
  RsCaptionHelpHlp       = 'Add help file to IDE help system';
  RsCaptionHelpChm       = 'Add HTML help to the Tools menu';
  RsCaptionHelpHxS       = 'Register help 2.0 files';
  RsCaptionHelpHxSPlugin = 'Plug help 2.0 files in the Embarcadero help system';

  // demos
  RsCaptionMakeDemos = 'Make demos';

// Hints
  // products
  RsHintLibrary = 'Select to install JCL for this target.';

  // conditional defines
  RsHintDef                 = 'Enable or disable specific features to be compiled';
  RsHintDefThreadSafe       = 'Conditionally some pieces of code to be thread safe, the ThreadSafe.txt file contains more informations about this feature';
  RsHintDefDropObsoleteCode = 'Do not compile deprecated code';
  RsHintDefUnitVersioning   = 'Includes JCL Unit Versioning informations into each JCL unit (see also JclUnitVersioning.pas)';
  // math options
  RsHintDefMath              = 'Math specific options (JclMath.pas)';
  RsHintDefMathPrecSingle    = 'type Float = Single';
  RsHintDefMathPrecDouble    = 'type Float = Double';
  RsHintDefMathPrecExtended  = 'type Float = Extended';
  RsHintDefMathExtremeValues = 'Exp en Power functions accept and return infinite and NaN';
  // Debug options
  RsHintDefDebug             = 'Debug and exception hooking specific options (JclDebug.pas and JclHookExcept.pas)';
  RsHintDefHookDllExceptions = 'Hook exceptions raised in DLL compiled with the JCL';
  RsHintDefDebugNoBinary     = 'Disable support for JDBG files';
  RsHintDefDebugNoMap        = 'Disable support for MAP files';
  RsHintDefDebugNoTD32       = 'Disable support for TD32 informations';
  RsHintDefDebugNoExports    = 'Disable support for export names of libraries';
  RsHintDefDebugNoSymbols    = 'Disable support for Microsoft debug symbols (PDB and DBG files)';
  // Wrapper options
  RsHintDefWrappers          = 'Configure linking options for wrappers to thirdparty libraries';
  // PCRE options
  RsHintDefPCRE              = 'PCRE specific options (pcre.pas and JclPCRE.pas)';
  RsHintDefPCREStaticLink    = 'Code from PCRE is linked into JCL binaries';
  RsHintDefPCRELinkDLL       = 'JCL binaries require pcre.dll to be present';
  RsHintDefPCRELinkOnRequest = 'JCL binaries require pcre.dll when calling PCRE functions';
  RsHintDefPCRERTL           = 'JCL relies on RTL''s RegularExpressionsAPI functions and declarations';
  RsHintDefPCRE8             = 'ANSI and UTF-8 is the historical version of PCRE library';
  RsHintDefPCRE16            = 'Unicode-enabled (UCS-2 and UTF-16) was introduced as of PCRE 8.30';
  RsHintDefPCREPrefer16      = 'Prefer Unicode-enabled PCRE when both versions are available';
  // BZip2 options
  RsHintDefBZip2              = 'BZip2 specific options (bzip2.pas)';
  RsHintDefBZip2StaticLink    = 'Code from BZip2 is linked into JCL binaries';
  RsHintDefBZip2LinkDLL       = 'JCL binaries require bzip2.dll to be present';
  RsHintDefBZip2LinkOnRequest = 'JCL binaries require bzip2.dll when calling BZip2 functions';
  // ZLib options
  RsHintDefZLib              = 'ZLib specific options (zlibh.pas)';
  RsHintDefZLibStaticLink    = 'Code from ZLib is linked into JCL binaries';
  RsHintDefZLibLinkDLL       = 'JCL binaries require zlib1.dll to be present';
  RsHintDefZLibLinkOnRequest = 'JCL binaries require zlib1.dll when calling ZLib functions';
  RsHintDefZLibRTL           = 'JCL relies on RTL''s ZLib functions and declarations';
  // Unicode options
  RsHintDefUnicode              = 'Unicode specific option (JclUnicode.pas)';
  RsHintDefUnicodeRTLDatabase   = 'Prefer RTL Character Database over JCL one, less accurate but reduce executable sizes';
  RsHintDefUnicodeSilentFailure = 'Insert a replacement character if sequence is corrupted rather than raising an exception';
  RsHintDefUnicodeRawData       = 'Link resource containing uncompressed Unicode data (bigger executable size)';
  RsHintDefUnicodeZLibData      = 'Link resource containing Unicode data compressed with ZLib';
  RsHintDefUnicodeBZip2Data     = 'Link resource containing Unicode data compressed with BZip2';
  // Container options
  RsHintDefContainer           = 'Container specific options';
  RsHintDefContainerAnsiStr    = 'Define TJclStr* containers as alias of TJclAnsiStr* containers';
  RsHintDefContainerWideStr    = 'Define TJclStr* containers as alias of TJclWideStr* containers';
  RsHintDefContainerUnicodeStr = 'Define TJClStr* containers as alias of TJclUnicodeStr* containers';
  RsHintDefContainerNoStr      = 'Do not define TJclStr* containers';
  // 7Z options
  RsHintDef7z               = 'Sevenzip specific options (sevenzip.pas)';
  //RsHintDef7zStaticLink     = 'Code from Sevenzip is linked into JCL binaries';
  RsHintDef7zLinkDLL        = 'JCL binaries require 7z.dll to be present';
  RsHintDef7zLinkOnRequest  = 'JCL binaries require 7z.dll when calling Sevenzip functions';

  // post compilation
  RsHintMapCreate  = 'Create detailed MAP files for each libraries';
  RsHintJdbgCreate = 'Create JEDI Debug Informations from the MAP files';
  RsHintJdbgInsert = 'Insert JEDI Debug Informations into the libraries (only the BPL has to be redistributed)';
  RsHintMapDelete  = 'The original MAP file is not kept once JEDI Debug Informations are generated';

  // environment
  RsHintEnvironment     = 'Set selected environment items';
  RsHintEnvLibPath      = 'Add JCL precompiled unit directories to library path';
  RsHintEnvBrowsingPath = 'Add JCL source directories to browsing path';
  RsHintEnvDebugDCUPath = 'This is a prerequisite for using the precompiled JCL debug units by means of the respective' + NativeLineBreak +
    'Project Options|Compiler switch. See "Make library units/Debug" option below.';
  RsHintEnvIncludePath  = 'Add JCL include path to C++ include path';

  // make units
  RsHintMake            = 'Generate .dcu files.' + NativeLineBreak + 'Recommended.';
  RsHintMakeRelease     = 'Make precompiled units for release, i.e. optimized, w/o debug information.';
  RsHintMakeDebug       = 'Make precompiled units for debugging, i.e.optimization off, debug information included.' + NativeLineBreak +
    'When installed, available through Project Options|Compiler|Use Debug DCUs.';
  RsHintCopyHppFiles    = 'Copy .hpp files into C++Builder''s include path.';
  RsHintCheckHppFiles   = 'Compile some C++ source files to verify JCL headers';

  // packages
  RsHintPackages             = 'Build and eventually install JCL runtime packages and optional IDE experts.';
  RsHintDualPackages         = 'The same package introduce code for Delphi Win32 and C++Builder Win32';
  RsHintCopyPackagesHppFiles = 'Output .hpp files into C++Builder''s include path instead of ' +
    'the JCL source paths.';

  // exception dialogs
  RsHintExceptDlg       = 'Add selected Exception dialogs to the Object Repository.';
  RsHintExceptDlgVCL    = 'Add VCL exception dialog to the Object Repository.';
  RsHintExceptDlgVCLSnd = 'Add VCL exception dialog with "Send Button" to the Object Repository.';

  // experts
  RsHintExperts                = 'Build and install selected IDE experts.';
  RsHintExpertsDsgnPackages    = 'Design packages containing JCL experts';
  RsHintExpertsDLL             = 'DLLs containing JCL experts';
  RsHintExpertDebug            = 'Install IDE expert which assists to insert JCL Debug information into executable files.';
  RsHintExpertAnalyzer         = 'Install IDE Project Analyzer.';
  RsHintExpertFavorite         = 'Install "Favorites" combobox in IDE Open/Save dialogs.';
  RsHintExpertRepository       = 'Repository expert to easily create exception dialogs';
  RsHintExpertThreadNames      = 'Display thread names in Thread Status window IDE extension.';
  RsHintExpertUses             = 'Install IDE Uses Wizard.';
  RsHintExpertSimdView         = 'Install a debug window of XMM registers (used by SSE instructions)';
  RsHintExpertVersionControl   = 'Integration of TortoiseCVS and TortoiseSVN in the IDE';
  RsHintExpertStackTraceViewer = 'Install an IDE expert which shows the JCL Debug stack trace information.';

  // help
  RsHintHelp          = 'Install JCL help files.';
  RsHintHelpHlp       = 'Customize Borland Open Help to include JCL help files.';
  RsHintHelpChm       = 'Compiled help files won''t be merged with the IDE help';
  RsHintHelpHxS       = 'Register Help 2.0 files';
  RsHintHelpHxSPlugin = 'Register Help 2.0 files as a plugin for the Borland.BDS* namespace';

  // demos
  RsHintMakeDemos = 'Make JCL demo applications';

// warning messages
  RsWarningPackageNodeNotSelected = 'The "Packages" node is not selected.' + sLineBreak +
    'Various libraries (including the JVCL) require JCL packages to be compiled' + sLineBreak +
    'Do you want to continue without compiling JCL packages?';
  RsWarningCreatePath = 'The path where %s files will be created doesn''t exist.' + sLineBreak +
    'Do you want the JCL installer to create it?';
  RsErrorCantCreatePath = 'The path %s cannot be created';
  RsWarningAddPathToEnvironment = 'The path where BPL are created must be present in the PATH' + sLineBreak +
    'environment variable, otherwise JCL packages won''t be found by the IDE.' + sLineBreak +
    'Do you want the JCL installer to add it?' + sLineBreak +
    'You will have to reboot your computer and/or to close your session to validate this change';
  RsHtmlHelp2Credentials = 'Registering HTML Help 2.0 files requires administrator privilege to be performed' + sLineBreak +
    'The RegHelper.exe utility will make this operation';
  RsKeepExpertSettings = 'Do you want to keep JCL expert settings?';
  RsHppCheckFailure = 'The JCL HPP files fail to compile, continue anyway?';

  RsExceptDlgVclName    = 'Exception Dialog';
  RsExceptDlgVclSndName = 'Exception Dialog with Send';

  RsExceptDlgDescription = 'JCL Application exception dialog';
  RsExceptDlgAuthor      = 'Project JEDI';
  RsExceptDlgPage        = 'Dialogs';

  RsLogInstalledPersonalities = 'Installed personalities :';
  RsLogSingleProfile = 'Single profile installation';
  RsLogMultipleProfile = 'Multiple profile installation';
  RsLogConditionalDefines = 'Saving conditional defines...';
  RsLogLoadTemplate = 'Loaded template for include file %s';
  RsLogSaveIncludeFile = 'Saved include file %s';
  RsLogAddLibrarySearchPath2 = 'Added "%s;%s" to library search path.';
  RsLogAddLibrarySearchPath1 = 'Added "%s" to library search path.';
  RsLogFailedAddLibrarySearchPath = 'Failed to add library search paths.';
  RsLogDelLibrarySearchPath2 = 'Removed "%s;%s" from library search path.';
  RsLogFailedDelLibrarySearchPath = 'Failed to remove library search path.';
  RsLogAddCppSearchPath2 = 'Added "%s;%s" to cpp search path.';
  RsLogFailedAddCppSearchPath = 'Failed to add cpp search paths.';
  RsLogDelCppSearchPath2 = 'Removed "%s;%s" from cpp search path.';
  RsLogFailedDelCppSearchPath = 'Failed to remove cpp search path.';
  RsLogAddLibraryBrowsingPath = 'Added "%s" to library browsing path.';
  RsLogFailedAddLibraryBrowsingPath = 'Failed to add library browsing path';
  RsLogDelLibraryBrowsingPath = 'Removed "%s" from library browsing path.';
  RsLogFailedDelLibraryBrowsingPath = 'Failed to remove library browsing path.';
  RsLogAddCppBrowsingPath = 'Added "%s" to cpp browsing path.';
  RsLogFailedAddCppBrowsingPath = 'Failed to add cpp browsing paths.';
  RsLogDelCppBrowsingPath = 'Removed "%s" from cpp browsing path.';
  RsLogFailedDelCppBrowsingPath = 'Failed to remove cpp browsing path.';
  RsLogAddDebugDCUPath = 'Added "%s" to Debug DCU Path.';
  RsLogFailedAddDebugDCUPath = 'Failed to add debug DCU path';
  RsLogDelDebugDCUPath = 'Removed "%s" from debug DCU Path.';
  RsLogFailedDelDebugDCUPath = 'Failed to remove debug DCU Path.';
  RsLogAddIncludePath = 'Added "%s" to C++ Include Path.';
  RsLogFailedAddIncludePath = 'Failed to add C++ Include path';
  RsLogIgnoreAddIncludePath = 'Not adding standard C++ Include Path "%s".';
  RsLogDelIncludePath = 'Removed "%s" from C++ Include Path.';
  RsLogFailedDelIncludePath = 'Failed to remove C++ Include Path.';
  RsLogIgnoreDelIncludePath = 'Not removing standard C++ Include Path "%s".';
  RsLogAddIdeTools = 'Added %s to %s IDE Tools';
  RsLogFailedAddIdeTools = 'Failed to add help file to IDE Tools';
  RsLogDelIdeTools = 'Removing %s from %s IDE Tools';
  RsLogFailedDelIdeTools = 'Failed to remove help file from IDE Tools';
  RsLogAddOpenHelp = 'Added %s to %s Open Help';
  RsLogFailedAddOpenHelp = 'Failed to add help file to Open Help';
  RsLogDelOpenHelp = 'Removing %s from %s Open Help';
  RsLogFailedDelOpenHelp = 'Failed to remove help file from Open Help';
  RsLogAddHelp2Files = 'Registering help 2.0 files...';
  RsLogDelHelp2Files = 'Unregistering help 2.0 files...';
  RsLogInstalling = 'Installing %s...';
  RsLogUninstalling = 'Removing %s...';
  RsLogInstallingJCL = 'Installing JCL for %s...';
  RsLogUninstallingJCL = 'Removing JCL for %s...';
  RsLogBuilding = 'Building %s...';
  RsLogRegistering = 'Registering %s...';
  RsLogFirstCompilationOk = 'First compilation ok.';
  RsLogFirstCompilationFailed = 'First compilation failed.';
  RsLogSearchingExpertEntryPoint = 'Analysing expert %s for entry point %s...';
  RsLogEntryPointNotFound = 'Entry point not found';
  RsLogEntryPointFound = 'Internal entry point found %s';
  RsLogRegHelperUnknownCommand = 'Fatal error: unknown reghelp command';
  RsLogRegHelperFailedCompile = 'Failed to compile RegHelper utility';
  RsLogRegHelperFailedExecute = 'Fatal error: failed to execute RegHelp utility';
  RsLogRegHelperError = 'RegHelper raised an error while executing RegHelp command: ';
  RsLogDone = '...done.';
  RsLogDefered = '...defered.';
  RsLogFailed = '...failed.';
  RsLogNoPersonalityExtension = 'No personality supports the extension %s';
  RsLogInvalidBplPath = 'Invalid BPL path "%s"';
  RsLogInvalidDcpPath = 'Invalid DCP path "%s"';
  RsLogInvalidHppPath = 'Invalid HPP path "%s"';
  RsLogLibDescriptor = '%s library %sunits for %s';

implementation

end.

