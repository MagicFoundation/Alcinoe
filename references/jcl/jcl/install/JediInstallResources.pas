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
{ The Original Code is JediInstallIntf.pas.                                                        }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s): Robert Rossmair (crossplatform & BCB support)                                    }
{                 Florent Ouchet (new core for more than one target)                               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JediInstallResources;

{$I jcl.inc}

interface

uses
  JclStrings;

resourcestring
  RsCloseRADTool     = 'Please close all running instances of Delphi/C++Builder IDE before the installation.';
  RsConfirmInstall   = 'Are you sure to install all selected features?';
  RsConfirmUninstall = 'Do you really want to uninstall the JCL?';
  RsInstallSuccess   = 'Installation finished';
  RsInstallFailure   = 'Installation failed.'#10'Check compiler output for details.';
  RsUninstallSuccess = 'Uninstallation success';
  RsUninstallFailure = 'Uninstallation failed, see logs for details';
  RsNoInstall        = 'There is no Delphi/C++Builder installation on this machine. Installer will close.';
  RsUpdateNeeded     = 'You should install latest Update Pack #%d for %s.'#13#10 +
                       'Would you like to open Borland support web page?';
  RsHintTarget       = 'Installation target';
  RsSelectPath       = 'Select path';
  RsEnterValidPath   = '(Enter valid path)';
  RsInvalidOption    = 'Invalid option: %d';

  RsGUISelectComponents = 'Select components to install';
  RsGUIInstallationLog = 'Installation log';
  RsGUIAdvancedOptions = 'Advanced Options';
  RsGUICompiling = 'Compiling';
  RsGUIPreparing = 'Preparing...';
  RsGUILinking = 'Linking';
  RsGUIDone = 'Done';
  RsGUIThereAreErrors = 'There are errors.';
  RsGUIThereAreWarnings = 'There are warnings.';
  RsGUIThereAreHints = 'There are hints.';
  RsGUICompiled = 'compiled.';
  RsGUIProject = 'Project';
  RsGUICurrentLine = 'Current line';
  RsGUITotalLines = 'Total lines';
  RsGUIHints = 'Hints';
  RsGUIWarnings = 'Warnings';
  RsGUIErrors = 'Errors';
  RsGUIOk = 'OK';
  RsGUIJEDIInstaller = 'JEDI Installer';
  RsGUIProjectJEDIInstaller = 'Project JEDI Installer';
  RsGUIInstall = '&Install';
  RsGUIUninstall = '&Uninstall';
  RsGUIQuit = '&Quit';
  RsGUIProfiles = 'Select profiles in the list below. Note that only remote profiles logged on local computer and local profiles are available.' + NativeLineBreak +
                  'If a profile has not IDE settings, the JCL won''t be installed on it.';
  RsGUIInstallSelectedOnly = 'Install &selected only';


implementation

end.
