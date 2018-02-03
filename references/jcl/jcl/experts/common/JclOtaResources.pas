{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclOtaResources.pas.                                                        }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) of Petr Vones.                                  }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Florent Ouchet (outchy)                                                                        }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclOtaResources;

interface

{$I jcl.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase;

//=== JclOtaUtils.pas ========================================================
resourcestring
  RsENoOTAServices = 'Unable to get IDE Services';
  RsENoNTAServices = 'Unable to get IDE NTA Services';
  RsENoDebuggerServices = 'Unable to get IDE Debugger Services';
  RsENoEditorServices = 'Unable to get IDE Editor Services';
  RsENoOTAAboutServices = 'Unable to get IDE About Services';
  RsENoOTAModuleServices = 'Unable to get IDE Module Services';
  RsENoOTAWizardServices = 'Unable to get IDE Wizard Services';
  RsENoOTAPackageServices = 'Unable to get IDE Package Services';
  RsENoOTAPersonalityServices = 'Unable to get IDE Personality Services';
  RsENoOTAProjectManager = 'Unable to get project manager';
  RsENoOTAMessageServices = 'Unable to get IDE Message Services';
  RsENoOTAGalleryCategoryManager = 'Unable to get IDE Gallery Category Manager';
  RsENoModule = 'Unable to get Module';
  RsBadModuleHInstance = 'Unable to get module HInstance';
  RsENoRootDir = 'RootDir is empty';
  RsENoIDEMenu = 'Unable to get IDE menu';
  RsENoToolsMenu = 'Unable to get Tools menu';

  RsAboutDialogTitle = 'JEDI Code Library';
  RsAboutTitle = 'JEDI Code Library %d.%d.%d.%d';
  RsAboutDescription = 'JEDI Code Library http://jcl.delphi-jedi.org/' + NativeLineBreak +
                       'The JCL is a member of the JEDI Project http://www.delphi-jedi.org' + NativeLineBreak +
                       'Covered under the Mozilla Public License v1.1 (MPL 1.1)' + NativeLineBreak +
                       'License available at http://www.mozilla.org/MPL/MPL-1.1.html';
  RsAboutLicenceStatus = 'MPL 1.1';
  RsJCLOptions = 'JCL Options...';
  RsActionSheet = 'Common\Actions';
  RsUnitVersioningSheet = 'Common\Unit versioning';
  RsENoBitmapResources = 'Unable to load bitmap resource';
  RsENoEnvironmentOptions = 'Environment options are not available';
  RsELineTooLong = 'Line too long in project file';
  RsEUnterminatedComment = 'Unterminated comment in project file';
  RsBrowseToJCLRootDir = 'Browse to JCL root directory';
  RsENoNTAEnvironmentOptionsServices = 'Unable to get IDE Environment Options Services';

//=== JclExceptionForm.pas ===================================================
resourcestring
  RsReportFormCaption = 'Exception in an expert of the JCL';
  RsExceptionDetails = 'An exception was raised in an expert of the JCL.' + NativeLineBreak +
                       'The JCL development team expects quality and performance for the library.' +
                       'That''s why we highly encourage you to report this exception by quoting ' +
                       'your version of Delphi/BCB/BDS (including patch numbers), by explaining ' +
                       'steps to reproduce and by copying the call stack displayed in the box below.' + NativeLineBreak +
                       'There are several ways to report bugs in the JCL:' + NativeLineBreak +
                       ' - issue tracker (recommended),' + NativeLineBreak +
                       ' - jedi newsgroups,' + NativeLineBreak +
                       ' - mailing list.' + NativeLineBreak +
                       'Details and guidelines for these tools are available at:';
  { TODO : Should this link lead directly to the issue tracker at http://issuetracker.delphi-jedi.org/ ?}
  RsReportURL = 'http://jcl.delphi-jedi.org/page24.html';
  RsReportCaption = 'JCL - Feedback&&Support - Report a bug page';
  RsDetailsExceptionName = 'Exception class name: %s';
  RsDetailsExceptionMessage = 'Exception message: %s';
  RsErrorWhileFormatting = 'An exception was raised while formatting details for the report';
  RsReportClose = '&Close';

//=== JclOtaActionConfigureSheet.pas =========================================
resourcestring
  RsActions = '&Actions :';
  RsCaption = 'Caption';
  RsShortcut = 'Shortcut';
  RsRestore = '&Restore';

//=== JclOtaUnitVersioningSheet.pas ==========================================
resourcestring
  RsCopyToClipboard = '&Copy to clipboard';
  RsSaveAsText = '&Save as...';

//=== JclExpertConfigurationForm.pas =========================================
resourcestring
  RsConfigurationCaption = 'JCL Options';
  RsOk = '&Ok';
  RsCancel = '&Cancel';
  RsSelectPage = 'Select a page';
  RsHomePage = '&JCL Home page';
  RsHomePageURL = 'http://jcl.delphi-jedi.org/';

//=== JclOtaWizardForm.pas ===================================================
resourcestring
  RsNext = '&Next';
  RsPrevious = '&Previous';
  RsFinish = '&Finish';
  RsWizardProgression = 'Page %d of %d: %s';

//=== JclOtaExcDlgWizard.pas =================================================
resourcestring
  RsExceptionDialogConfigure = 'New exception dialog...';

//=== JclOtaExcDlgFileFrame.pas ==============================================
resourcestring
  RsExcDlgFileOptions = 'file options';
  RsLanguage = '&Language:';
  RsUnitFileName = '&Unit file name:';
  RsFormName = 'Form &name:';
  RsFormAncestor = 'Form &ancestor:';
  RsFileNameDialog = '&Save new file as...';

//=== JclOtaExcDlgFormFrame.pas ==============================================
resourcestring
  RsExcDlgFormOptions = 'form options';
  RsDialogWithMailButton = '&Button to send stack trace by mail';
  RsEMail = '&EMail:';
  RsSubject = '&Subject:';
  RsModalDialog = '&Modal dialog';
  RsSizeableDialog = 'S&izeable dialog';
  RsAutoScrollBars = '&Automatic scroll bars';

//=== JclOtaExcDlgSystemFrame.pas ============================================
resourcestring
  RsExcDlgSystemOptions = 'system options';
  RsDelayedStackTrace = '&Delayed stack traces (faster)';
  RsHookDll = '&Hook DLL';
  RsModuleList = '&Module list';
  RsUnitVersioning = '&Unit versioning';
  RsOSInfo = '&Operating system informations';
  RsActiveControls = '&List of active controls';
  RsCatchMainThread = '&Catch only exceptions of main thread';
  RsDisableIfDebuggerAttached = 'Disable if the debu&gger is attached';

//=== JclOtaExcDlgLogFrame.pas ===============================================
resourcestring
  RsExcDlgLogOptions = 'log options';
  RsLogFileName = '&Log file name:';
  RsLogTrace = '&Add crash data to log file';
  RsLogInWorkingDirectory = 'Autosave in &working directory';
  RsLogInApplicationDirectory = 'Autosave in a&pplication directory (not recommended)';
  RsLogInDesktopDirectory = 'Autosave in &desktop directory';
  RsLogSaveDialog = 'Add a save &button on dialog';

//=== JclOtaExcDlgTraceFrame.pas =============================================
resourcestring
  RsExcDlgTraceOptions = 'trace options';
  RsStackList = '&Stack list';
  RsRawData = '&Raw analysis of the stack';
  RsModuleName = '&Module name';
  //RsAddressOffset = 'Address offset';
  RsCodeDetails = '&Code details';
  RsVirtualAddress = '&Virtual address';
  RsModuleOffset = 'Module &offset';
  RsPreview = '&Preview:';

//=== JclOtaExcDlgThreadFrame.pas ============================================
resourcestring
  RsExcDlgThreadOptions = 'thread options';
  RsAllThreads = 'Traces for &all threads';
  RsAllRegisteredThreads = 'Traces for &registered threads';
  RsMainExceptionThreads = 'Traces for main a&nd exception threads';
  RsExceptionThread = 'Trace for &exception thread';
  RsMainThread = 'Trace for &main thread';

//=== JclOtaExcDlgIgnoreFrame.pas ============================================
resourcestring
  RsExcDlgIgnoreOptions = 'ignored exceptions';
  RsTraceAllExceptions = '&Trace all exceptions';
  RsTraceEAbort = 'Trace &EAbort and its descendants';
  RsIgnoredExceptions = '&Ancestor exception classes to ignore (one per line)';

//=== JclUsesDialog.pas ======================================================
resourcestring
  RsActionSkip = 'Skip';
  RsActionAdd = 'Add';
  RsActionMove = 'Move';
  RsSectionImpl = 'to implementation uses';
  RsSectionIntf = 'to interface uses';
  RsUndeclIdent = '[Error] %s(%d) Undeclared identifier: ''%s''';
  RsConfirmChanges = '%s: Confirm changes';

//=== ProjAnalyserImpl.pas ===================================================
resourcestring
  RsAnalyzeActionCaption = 'Analyze project %s';
  RsProjectNone = '[none]';
  RsCantFindMAPFile = 'Can''t find MAP "%s" for project "%s"';
  RsBuildingProject = 'Building project %s ...';
  RsAnalyseMenuItemNotInserted = 'Can''t insert the analyse menu item';

//=== ProjAnalyzerFrm.pas ====================================================
resourcestring
  RsCopy = 'Copy';
  RsSave = 'Save';
  RsDetails = 'Details';
  RsSummary = 'Summary';
  RsForms = 'Forms';
  RsShowPackages = 'Show Packages';
  RsTextLabels = 'Text labels';
  RsName = 'Name';
  RsSize = 'Size';
  RsGroup = 'Group';
  RsPackage = 'Package';
  RsFormCaption = 'Project Analyzer - %s';
  RsStatusText = 'Units: %d, Forms: %d, Code: %d, ICode: %d, Data: %d, Bss: %d, Resources: %d';
  RsCodeData = '(CODE+ICODE+DATA)';

//=== JclUsesWizard.pas ======================================================
resourcestring
  RsEErrorReadingBuffer = 'Error reading from edit buffer';
  RsUsesSheet = 'Uses wizard';

//=== JclOptionsFrame.pas ====================================================
resourcestring
  RsUsesConfigurationFile = '&Configuration file:';
  RsUsesActive = '&Active';
  RsUsesConfirm = '&Prompt to confirm changes';
  RsUsesOpenTitle = 'Select JEDI Uses wizard configuration file';
  RsUsesOpenFilters = 'Configuration files (*.ini)|*.ini|All files (*.*)|*.*';

//=== JclDebugIdeImpl.pas ====================================================
resourcestring
  RsENoProjectOptions = 'Project options are not available';
  RsENoProjectOptionsConfigurations = 'Project options configurations are not available';
  RsCantInsertToInstalledPackage = 'JCL Debug IDE Expert: Can not insert debug information to installed package' +
    NativeLineBreak + '%s' + NativeLineBreak + 'Would you like to disable the insertion of JCL Debug data ?';
  RsChangeMapFileOption = 'JCL Debug expert: the project "%s" must be configured to generate a detailled MAP file.' +
    NativeLineBreak + 'Do you want the expert to change this setting?';
  RsDisabledDebugExpert = 'JCL Debug expert is disabled';
  RsCompilationAborted = 'JCL Debug data cannot be inserted to installed package' + NativeLineBreak + 'Compilation aborted';
  RsDebugExpertCaption = 'JCL Debug expert';
  RsAlwaysDisabled = 'Always &disabled';
  RsProjectDisabled = 'D&isabled for this project';
  RsProjectEnabled = 'E&nabled for this project';
  RsAlwaysEnabled = 'Always &enabled';
  RsEExecutableNotFound = 'Executable file for project "%s" not found.' +
    'JCL debug data can''t be added to the binary.';
  RsEMapFileNotFound = 'Map file "%s" for project "%s" not found.' +
    'No conversions of debug information were made';
  RsConvertedMapToJdbg = 'Converted MAP file "%s" (%d bytes) to .jdbg (%d bytes)';
  RsInsertedJdbg = 'Converted MAP file "%s" (%d bytes) and inserted debug information (%d bytes) into the binary';
  RsDeletedMapFile = 'Deleted %s file "%s"';
  RsEFailedToDeleteMapFile = 'Failed to delete %s file "%s"';
  RsEMapConversion = 'Failed to convert MAP file "%s"';
  RsEMapInsertion = 'Failed to insert MAP file "%s"';
  RsENoActiveProject = 'No active project';
  RsENoProjectMenuItem = 'Project menu item not found';
  RsEInsertDataMenuItemNotInserted = 'Can''t insert the insert data menu item';
  RsENoBuildAction = 'Build action not found';
  RsENoBuildAllAction = 'Build All action not found';
  RsDebugConfigPageCaption = 'Debug info converter';
  RsEProjectPropertyFailed = 'Unable to save project properties, project file may be read-only';
  RsJclDebugMessagePrefix = 'JCL Debug Expert';

//=== JclDebugIdeConfigFrame.pas =============================================
resourcestring
  RsDefaultDisabled = 'D&isabled by default (can be enabled per project)';
  RsDefaultEnabled = 'E&nabled by default (can be disabled per project)';
  RsDebugGenerateJdbg = 'Generate .jdbg files';
  RsDebugInsertJdbg = 'Insert JDBG data into the binary';
  RsDeleteMapFile = 'Delete map files after conversion';
  RsEInvalidDebugExpertState = '%d is not a valid debug expert state';
  RsQuiet = 'Do not show dialogs or log entries';

//=== JclDebugIdeResult.pas ==================================================
resourcestring
  RsProject = 'Project';
  RsMapFileSize = 'MAP file size';
  RsJCLDebugSize = 'JCLDebug size';
  RsRatio = 'Ratio';
  RsExecutableFileName = 'Executable file name';
  RsLinkerBug = 'Linker bug';
  RsLineErrors = 'Line errors';

//=== JclSIMDCpuInfo.pas =====================================================
resourcestring
  RsCpuInfoTitle = 'Local CPU Informations';
  RsVendor = 'Vendor';
  RsFrequency = 'Frequency';
  RsEnabledFPU = 'Enabled FPU';
  RsEnabledSSE = 'Enabled SSE';
  RsEnabledAVX = 'Enabled AVX';
  RsSSE1 = 'SSE Version 1';
  RsSSE2 = 'SSE Version 2';
  RsSSE3 = 'SSE Version 3';
  RsSSE3Ext = 'SSE Version 3 Ext.';
  RsSSE41 = 'SSE Version 4.1';
  RsSSE42 = 'SSE Version 4.2';
  RsSSE4A = 'SSE Version 4 AMD Ext.';
  RsSSE5 = 'SSE Version 5';
  RsAVX = 'AVX';
  RsClose = 'Close';

//=== JclSIMDModifyForm.pas ==================================================
resourcestring
  RsDisplay = 'Display';
  RsFormat = 'Format';
  RsBinary = 'Binary';
  RsSignedDecimal = 'Signed Decimal';
  RsUnsignedDecimal = 'Unsigned Decimal';
  RsHexadecimal = 'Hexadecimal';
  RsKeepBlank = 'Keep blank for no change';
  RsSIMDModificationDescription = 'Tip: xmm0.byte0 will return the first byte of xmm0, ' +
    'valid fields are byteX, wordX, dwordX, qwordX, singleX, doubleX' + NativeLineBreak +
    'Valid registers are:' + NativeLineBreak +
    ' - mm0..mm7 (all processors)' + NativeLineBreak +
    ' - xmm0..xmm7 (32-bit processor with SSE) or xmm0..xmm15 (64-bit processor with SSE)' + NativeLineBreak +
    ' - ymm0..ymm7 (32-bit processor with AVX) or ymm0..ymm15 (64-bit processor with AVX)' + NativeLineBreak;

//=== JclSIMDView.pas ========================================================
resourcestring
  RsENoViewMenuItem = 'View menu item not found';
  RsENoDebugWindowsMenuItem = 'Debug windows menu item not found';

//=== JclSIMDUtils.pas =======================================================
resourcestring
  RsSIMD = 'SIMD';
  RsMMX = 'MMX';
  RsMMXExt = 'MMX Ext.';
  Rs3DNow = '3DNow!';
  Rs3DNowExt = '3DNow! Ext.';
  RsLong = '64-bit Core';

  RsNoSSE = 'SSE are not supported on this processor';
  RsNotSupportedFormat = '<Unsupported format>';
  RsNoPackedData = '<No packed data>';
  RsModifyMM = 'Modification of MM%d';
  RsModifyXMM1 = 'Modification of XMM%d';
  RsModifyXMM2 = 'Modification of XMM%.2d';
  RsModifyYMM1 = 'Modification of YMM%d';
  RsModifyYMM2 = 'Modification of YMM%.2d';

  RsVectorIE  = 'IE  ';
  RsVectorDE  = 'DE  ';
  RsVectorZE  = 'ZE  ';
  RsVectorOE  = 'OE  ';
  RsVectorUE  = 'UE  ';
  RsVectorPE  = 'PE  ';
  RsVectorDAZ = 'DAZ '; //  (Only in Intel P4, Intel Xeon and AMD)
  RsVectorIM  = 'IM  ';
  RsVectorDM  = 'DM  ';
  RsVectorZM  = 'ZM  ';
  RsVectorOM  = 'OM  ';
  RsVectorUM  = 'UM  ';
  RsVectorPM  = 'PM  ';
  RsVectorRC  = 'RC  ';
  RsVectorFZ  = 'FZ  ';

  RsVectorIEText  = 'Invalid-operation exception';
  RsVectorDEText  = 'Denormal-operand exception';
  RsVectorZEText  = 'Zero-divide exception';
  RsVectorOEText  = 'Overflow exception';
  RsVectorUEText  = 'Underflow exception';
  RsVectorPEText  = 'Precision exception';
  RsVectorDAZText = 'Denormal are zeros'; //  (Only in Intel P4, Intel Xeon and AMD)
  RsVectorIMText  = 'Invalid-operation mask';
  RsVectorDMText  = 'Denormal-operand mask';
  RsVectorZMText  = 'Zero-divide mask';
  RsVectorOMText  = 'Overflow mask';
  RsVectorUMText  = 'Underflow mask';
  RsVectorPMText  = 'Precision mask';
  RsVectorRCText  = 'Rounding control';
  RsVectorFZText  = 'Flush to zero';

  RsRoundToNearest = 'Round to nearest';
  RsRoundDown = 'Round down';
  RsRoundUp = 'Round up';
  RsRoundTowardZero = 'Round toward zero';

  RsEBadRegisterDisplay = 'Bad register display';

//=== JclSIMDViewForm.pas ====================================================
resourcestring
  RsStayOnTop = 'Stay on top';
  RsModify = 'Modify';
  RsComplementBit = 'Complement bit';
  RsEmptyMM = 'Empty MM register';
  RsEmptyAllMM = 'Empty all MM registers';
  RsViewYMM = 'View YMM registers';
  RsCPUInfo = 'CPU Informations...';

  RsECantUpdateThreadContext = 'Unable to update the thread context';

//=== JclOtaExcDlgRepository.pas =============================================
resourcestring
  RsRepositoryExcDlgPage = 'Exception dialog';

  RsRepositoryExcDlgDelphiName = 'JCL Exception dialog for Delphi';
  RsRepositoryExcDlgDelphiDescription = 'Create an exception dialog for your Delphi project';

  RsRepositoryExcDlgCBuilderName = 'JCL Exception dialog for C++Builder';
  RsRepositoryExcDlgCBuilderDescription = 'Create an exception dialog for your C++Builder';

//=== JclVersionControlImpl.pas ==============================================
resourcestring
  RsVersionCtrlMenuCaption = '&Version Control';
  RsSvnMenuItemNotInserted = 'Can''t insert the ''%s'' menu item';
  RsENoToolsMenuItem = 'Tools menu item not found';
  RsVersionControlSheet = 'Version control';
  RsActionCategory = 'JEDI Code Library';

//=== JclStackTraceViewerConfigFrame.pas =====================================
resourcestring
  RsExpandTreeView = 'Expand TreeView';
  RsModuleVersionAsRevision = 'Module FileVersion as Revision';

//=== JclStackTraceViewerExceptInfoFrame.pas =================================
resourcestring
  RsExceptionClassName = 'ClassName:';
  RsExceptionMessage = 'Message:';

//=== JclStackTraceViewerImpl.pas ============================================
resourcestring
  RsStackTraceViewerCaption = 'Stack Traces';
  RsStackTraceViewerOptionsPageName = 'Stack Trace Viewer';

//=== JclStackTraceViewerMainFrame.pas =======================================
resourcestring
  RsSTVFindFilesInProjectGroup = 'Find files in active project group';
  RsSTVFindFileInProjectGroup  = 'Find %s in active project group';
  RsSTVFindFilesInBrowsingPath = 'Find files in browsing path';

//=== JclStackTraceViewerMainFrame.pas =======================================
resourcestring
  RsJumpToCodeLine = 'Jump to code line';
  RsLoadStack = 'Load Stack';
  RsOptions = 'Options';
  RsUpdateLocalInfo = 'Update Local Info';

//=== JclStackTraceViewerModuleFrame.pas =====================================
resourcestring
  RsStartAddr = 'Start address';
  RsEndAddr = 'End address';
  RsSystemModule = 'System module';
  RsBinFileName = 'Bin file name';
  RsBinFileVersion = 'Bin file version';
  RsFileVersion = 'File version';
  RsFileDescription = 'File description';

//=== JclStackTraceViewerStackFrame.pas ======================================
resourcestring
  RsStackModuleName = 'Module name';
  RsSourceUnitName = 'Source unit name';
  RsProcedureName = 'Procedure name';
  RsSourceName = 'Source name';
  RsLineNumber = 'Line number';
  RsLineNumberOffsetFromProcedureStart = 'Line number offset from procedure start';
  RsRevision = 'Revision';
  RsProjectFile = 'Project file';
  RsTranslatedLineNumber = 'Translated line number';

//=== JclVersionCtrlCommonOptions.pas ========================================
resourcestring
  RsEInvalidMenuCaption = 'Menu caption cannot contain \, _ and numbers';
  RsDisableActions = '&Enable/disable actions';
  RsHideUnsupportedActions = '&Hide unsupported actions';
  RsSaveConfirmation = '&Save confirmation';
  RsActOnTopSandBox = '&Act on top sandbox';
  RsIcons = '&Icons:';
  RsNewItem = 'New item';
  RsNewSeparator = 'New &separator';
  RsNewSubMenu = 'New s&ub menu';
  RsNewAction = 'New &action';
  RsDeleteItem = '&Delete';
  RsRenameItem = '&Rename';
  RsMoveItemUp = 'Move &up';
  RsMoveItemDown = 'Move &down';
  RsMenuOrganization = 'Menu &organization:';
  RsNoIcon = 'No icon';
  RsJCLIcons = 'JCL icons';

//=== JclVersionCtrlImpl.pas =================================================
resourcestring
  RsEInvalidAction = 'Internal error: invalid action';

//=== JclOtaAddinOptions.pas =================================================
resourcestring
  RsProjectJEDIAddinOptionsCaptionPrefix = 'Project JEDI.JCL.';
  RsProjectJEDIAddinOptionsCaption = 'Project JEDI';
  RsProjectJEDIAddinOptionsTitle = 'Project JEDI Options';
  RsProjectJEDIJclAddinOptionsCaption = 'Project JEDI.JCL';
  RsProjectJEDIJclAddinOptionsTitle = 'JCL Options';
  RsProjectJEDIJclCommonAddinOptionsCaption = 'Project JEDI.JCL.Common';
  RsProjectJEDIJclCommonAddinOptionsTitle = 'JCL Common Options';

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
