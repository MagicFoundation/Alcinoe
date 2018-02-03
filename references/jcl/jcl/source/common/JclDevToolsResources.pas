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
{ The Original Code is JclResources.pas.                                                           }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel. All rights reserved.  }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Alexei Koudinov                                                                                }
{   Barry Kelly                                                                                    }
{   Flier Lu (flier)                                                                               }
{   Florent Ouchet (outchy)                                                                        }
{   Jean-Fabien Connault (cycocrew)                                                                }
{   Marcel Bestebroer                                                                              }
{   Marcel van Brakel                                                                              }
{   Matthias Thoma (mthoma)                                                                        }
{   Peter Friese                                                                                   }
{   Petr Vones (pvones)                                                                            }
{   Raymond Alexander (rayspostbox3)                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Scott Price (scottprice)                                                                       }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Unit which provides a central place for all resource strings used in the JCL developer tool      }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclDevToolsResources;

{$I jcl.inc}

interface

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

//=== JclIDEUtils ============================================================
resourcestring
  RsNeedUpdate          = 'You should install latest Update Pack #%d for %s';
  RsUpdatePackName      = 'Update Pack #%d';
  RsDelphiName          = 'Delphi';
  RsBCBName             = 'C++Builder';
  RsCSharpName          = 'C#Builder';
  RsBDSName             = 'Borland Developer Studio';
  RsRSName              = 'RAD Studio';
  {$IFDEF MSWINDOWS}
  RsClientServer        = 'Client/Server';
  RsStandard            = 'Standard';
  {$ENDIF MSWINDOWS}
  RsArchitect           = 'Architect';
  RsEnterprise          = 'Enterprise';
  RsPersonal            = 'Personal';
  RsProfessional        = 'Professional';

  RsMsBuildNotSupported = 'MSBuild is not supported by this IDE';

  RsPackageInstallationStarted    = 'Installing package %s';
  RsPackageInstallationFinished   = 'Installation of package finished';
  RsPackageUninstallationStarted  = 'Uninstalling package %s';
  RsPackageUninstallationFinished = 'Uninstallation of package finished';
  RsIdePackageInstallationStarted    = 'Installing ide package %s';
  RsIdePackageInstallationFinished   = 'Installation of ide package finished';
  RsIdePackageUninstallationStarted  = 'Uninstalling ide package %s';
  RsIdePackageUninstallationFinished = 'Uninstallation of ide package finished';
  RsExpertInstallationStarted     = 'Installing expert %s';
  RsExpertInstallationFinished    = 'Installation of expert finished';
  RsExpertUninstallationStarted   = 'Uninstalling expert %s';
  RsExpertUninstallationFinished  = 'Uninstallation of expert finished';

  RsCompilingPackage            = 'Compiling package %s';
  RsCompilingProject            = 'Compiling project %s';
  RsCompilationOk               = 'Compilation success';
  RsCompilationFailed           = 'Compilation failure';
  RsCreatingJdbg                = 'Creating JEDI Debug informations for %s';
  RsInsertingJdbg               = 'Inserting JEDI Debug informations in %s';
  RsJdbgInfo                    = 'Bug unit: %s; MAP size: %d; Debug size: %d';
  RsJdbgInfoOk                  = 'JDBG successfully generated';
  RsJdbgInfoFailed              = 'Cannot generate JDBG informations';
  RsDeletingFile                = 'Deleting file %s';
  RsFileDeletionOk              = 'File deletion success';
  RsFileDeletionFailed          = 'File deletion failure';
  RsRegisteringPackage          = 'Registering package %s';
  RsRegisteringIdePackage       = 'Registering ide package %s';
  RsRegisteringExpert           = 'Registering expert %s';
  RsRegistrationOk              = 'Registration ok';
  RsRegistrationFailed          = 'Registration failed';
  RsUnregisteringPackage        = 'Removing from registry package %s';
  RsUnregisteringIdePackage     = 'Removing from registry ide package %s';
  RsUnregisteringExpert         = 'Removing from registry expert %s';
  RsUnregistrationOk            = 'Unregistration ok';
  RsUnregistrationFailed        = 'Unregistration failed';
  RsCleaningPackageCache        = 'Cleaning package cache for %s';
  RsCleaningOk                  = 'Cleaning ok';
  RsCleaningFailed              = 'Cleaning failed';

  RsEUnknownPackageExtension    = '%s not a known package extension';
  RsEUnknownProjectExtension    = '%s not a known project extension';
  RsEUnknownIdePackageExtension = '%s not a known IDE package extension';
  RsEIndexOufOfRange            = 'Index out of range';
  RsECmdLineToolOutputInvalid   = '%s: Output invalid, when OutputCallback assigned.';
  RsENotABcbPackage             = '%s not a C++Builder package source file';
  RsENotADelphiProject          = '%s not a Delphi project source file';
  RsENotADelphiPackage          = '%s not a Delphi package source file';
  RsENotFound                   = '%s not found';
  RsECannotInstallRunOnly       = 'A run-only package cannot be installed';
  RsENotABcbProject             = '%s not a C++Builder project source file';
  RsENoSupportedPersonality     = 'No personalities supported';
  RsEDualPackageNotSupported    = 'This installation of %s doesn''t support dual packages';
  RsEWin64PlatformNotValid      = 'This installation cannot generate binaries for Win64';
  RsEOSXPlatformNotValid        = 'This installation cannot generate binaries for OSX';
  RsEPlatformNotValid           = 'This installation cannot generate binaries for an unknown platform';
  {$IFDEF MSWINDOWS}
  RsENoOpenHelp                 = 'open help not present in Borland Developer Studio';
  {$ENDIF MSWINDOWS}
  RsERsVars                     = 'Query of RsVars for %s %d reported the following error "%s"';

//=== JclMsBuild.pas =========================================================
resourcestring
  RsEEndOfString = 'Invalid condition: end of string in condition "%s"';
  RsEMissingParenthesis = 'Invalid condition: missing parenthesis in condition "%s"';
  RsEUnknownOperator = 'Invalid condition: unknown operator in condition "%s"';
  RsEReservedProperty = 'Attempt to override or to delete a reserved MsBuild property';
  RsENoProjectElem = 'Project element expected, got "%s"';
  RsEUnknownSchema = 'Unknown schema "%s"';
  RsEUnknownProperty = 'Unknown property "%s"';
  RsEUnknownElement = 'Unknown element "%s"';
  RsEMultipleProjectExtensions = 'Multiple project extensions';
  RsEMultipleOtherwise = 'Multiple otherwise';
  RsEConditionNotUnique = 'Condition is not unique';
  RsEMissingTargetName = 'Missing target name';
  RsEMissingTaskName = 'Missing task name';
  RsEMissingAssembly = 'Missing assembly';
  RsEMissingTaskParameter = 'Missing task parameter';
  RsEMissingOutputName = 'Missing output name';
  RsEMSBuildPath = 'Unable to locate MSBuild.exe';
  RsEFunctionProperty = 'Unable to evaluate function property "%s"';
  RsERegistryProperty = 'Unable to evaluate registry property root="%s" path="%s" name="%s"';
  RsELocateXmlElem = 'Unable to locate the XML element for MSBuild property "%s"';

//=== JclUsesUtils.pas =======================================================
resourcestring
  RsEDuplicateUnit = 'Duplicate unit ''%s''';
  RsEInvalidLibrary = 'Invalid library';
  RsEInvalidProgram = 'Invalid program';
  RsEInvalidUnit = 'Invalid unit';
  RsEInvalidUses = 'Invalid uses clause';

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
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
