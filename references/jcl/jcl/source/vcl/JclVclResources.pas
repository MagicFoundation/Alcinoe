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
{ Unit which provides a central place for all resource strings used in the JCL VCL                 }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclVclResources;

{$I jcl.inc}

interface

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

//=== JclGraphics ============================================================
resourcestring
  RsAssertUnpairedEndUpdate   = 'Unpaired BeginUpdate EndUpdate';
  RsCreateCompatibleDc        = 'Could not create compatible DC';
  RsDestinationBitmapEmpty    = 'Destination bitmap is empty';
  RsDibHandleAllocation       = 'Could not allocate handle for DIB';
  RsMapSizeFmt                = 'Could not set size on class "%s"';
  RsSelectObjectInDc          = 'Could not select object in DC';
  RsSourceBitmapEmpty         = 'Source bitmap is empty';
  RsSourceBitmapInvalid       = 'Source bitmap is invalid';
  RsNoBitmapForRegion         = 'No bitmap for region';
  RsNoDeviceContextForWindow  = 'Cannot get device context of the window';
  RsInvalidRegion             = 'Invalid Region defined for RegionInfo';
  RsRegionDataOutOfBound      = 'Out of bound index on RegionData';
  RsRegionCouldNotCreated     = 'Region could not be created';
  RsInvalidHandleForRegion    = 'Invalid handle for region';
  RsInvalidRegionInfo         = 'Invalid RegionInfo';
  RsInvalidControlType        = '%s is not descended from TWinControl';
  RsInvalidFormOrComponent    = 'A %s with a nil reference has been passed to the method';

  RsBitmapExtension           = '.bmp';
  RsJpegExtension             = '.jpg';
  RsGifExtension              = '.gif';
  RsPngExtension              = '.png';

//=== JclGraphUtils ==========================================================
resourcestring
  RsBitsPerSampleNotSupported = '%d bits per sample not supported in color space conversion';

//=== JclOpenDialogFavorites.pas =============================================
resourcestring
  RsOpenDialogList         = ' --> Favorites';
  RsOpenDialogAdd          = ' --> Add to this list';
  RsOpenDialogDelete       = ' --> Delete from this list';
  RsOpenDialogVirtual      = ' --> Virtual directories cannot be added to the favorites';
  RsOpenDialogFavorites    = '&Favorites:';
  RsOpenDialogConfirmation = 'Confirmation';
  RsOpenDialogDelConfirm   = 'Are you sure to delete "%s" from favorite folders?';

//=== JclOpenDialogHooks.pas =================================================
resourcestring
  RsEOpenDialogHookExists = 'An open dialog hook is already installed (existing ClassName = %s)';

//=== JclPrint ===============================================================
resourcestring
  RsSpoolerDocName = 'My Document';

  RsInvalidPrinter        = 'Invalid printer';
  RsNAStartDocument       = 'Unable to "Start document"';
  RsNASendData            = 'Unable to send data to printer';
  RsNAStartPage           = 'Unable to "Start page"';
  RsNAEndPage             = 'Unable to "End page"';
  RsNAEndDocument         = 'Unable to "End document"';
  RsNATransmission        = 'Not all chars have been sent correctly to printer';
  RsDeviceMode            = 'Error retrieving DeviceMode';
  RsUpdatingPrinter       = 'Error updating printer driver';
  RsIndexOutOfRange       = 'Index out of range setting bin';
  RsRetrievingSource      = 'Error retrieving Bin Source Info';
  RsRetrievingPaperSource = 'Error retrieving Paper Source Info';
  RsIndexOutOfRangePaper  = 'Index out of range setting paper';

  // Paper Styles (PS)
  RsPSLetter      = 'Letter 8 1/2 x 11 in';
  RsPSLetterSmall = 'Letter Small 8 1/2 x 11 in';
  RsPSTabloid     = 'Tabloid 11 x 17 in';
  RsPSLedger      = 'Ledger 17 x 11 in';
  RsPSLegal       = 'Legal 8 1/2 x 14 in';
  RsPSStatement   = 'Statement 5 1/2 x 8 1/2 in';
  RsPSExecutive   = 'Executive 7 1/2 x 10 in';
  RsPSA3          = 'A3 297 x 420 mm';
  RsPSA4          = 'A4 210 x 297 mm';
  RsPSA4Small     = 'A4 Small 210 x 297 mm';
  RsPSA5          = 'A5 148 x 210 mm';
  RsPSB4          = 'B4 250 x 354';
  RsPSB5          = 'B5 182 x 257 mm';
  RsPSFolio       = 'Folio 8 1/2 x 13 in';
  RsPSQuarto      = 'Quarto 215 x 275 mm';
  RsPS10X14       = '10 x 14 in';
  RsPS11X17       = '11 x 17 in';
  RsPSNote        = 'Note 8 1/2 x 11 in';
  RsPSEnv9        = 'Envelope #9 3 7/8 x 8 7/8 in';
  RsPSEnv10       = 'Envelope #10 4 1/8 x 9 1/2 in';
  RsPSEnv11       = 'Envelope #11 4 1/2 x 10 3/8 in';
  RsPSEnv12       = 'Envelope #12 4 \276 x 11 in';
  RsPSEnv14       = 'Envelope #14 5 x 11 1/2 in';
  RsPSCSheet      = 'C size sheet';
  RsPSDSheet      = 'D size sheet';
  RsPSESheet      = 'E size sheet';
  RsPSUser        = 'User Defined Size';
  RsPSUnknown     = 'Unknown Paper Size';

//=== JclVersionControl ======================================================
resourcestring
  RsVersionCtrlAddCaption = '&Add';                                 // vcaAdd
  RsVersionCtrlAddSandboxCaption = 'Add ...';                       // vcaAddSandbox
  RsVersionCtrlBlameCaption = '&Blame';                             // vcaBlame
  RsVersionCtrlBranchCaption = 'Branc&h';                           // vcaBranch
  RsVersionCtrlBranchSandboxCaption = 'Branch ...';                 // vcaBranchSandbox
  RsVersionCtrlCheckOutSandboxCaption = 'C&heck out ...';           // vcaCreateSandbox
  RsVersionCtrlCommitCaption = 'Co&mmit';                           // vcaCommit
  RsVersionCtrlCommitSandboxCaption = 'Commit ...';                 // vcaCommitSandbox
  RsVersionCtrlContextMenuCaption = 'Co&ntext Menu (right-click)';  // vcaContextMenu
  RsVersionCtrlDiffCaption = '&Diff';                               // vcaDiff
  RsVersionCtrlExploreCaption = 'E&xplore';                         // vcaExplore
  RsVersionCtrlExploreSandboxCaption = 'E&xplore ...';              // vcaExploreSandbox
  RsVersionCtrlGraphCaption = 'Revision Gr&aph';                    // vcaGraph
  RsVersionCtrlLogCaption = '&Log';                                 // vcaLog
  RsVersionCtrlLogSandboxCaption = 'Log ...';                       // vcaLogSandbox
  RsVersionCtrlLockCaption = 'Loc&k';                               // vcaLock
  RsVersionCtrlLockSandboxCaption = 'Lock ...';                     // vcaLockSandbox
  RsVersionCtrlMergeCaption = '&Merge';                             // vcaMerge
  RsVersionCtrlMergeSandboxCaption = 'Merge ...';                   // vcaMergeSandbox
  RsVersionCtrlPropertiesCaption = 'Pr&operties';                   // vcaProperties
  RsVersionCtrlPropertiesSandboxCaption = 'Properties ...';         // vcaPropertiesSandbox
  RsVersionCtrlRenameCaption = '&Rename';                           // vcaRename
  RsVersionCtrlRenameSandboxCaption = '&Rename Sandbox';            // vcaRenameSandbox
  RsVersionCtrlRepoBrowserCaption = 'Repositor&y Browser';          // vcaRepoBrowser
  RsVersionCtrlRevertCaption = '&Revert';                           // vcaRevert
  RsVersionCtrlRevertSandboxCaption = 'Revert ...';                 // vcaRevertSandbox
  RsVersionCtrlStatusCaption = 'S&tatus';                           // vcaStatus
  RsVersionCtrlStatusSandboxCaption = 'Status ...';                 // vcaStatusSandbox
  RsVersionCtrlTagCaption = 'Ta&g';                                 // vcaTag
  RsVersionCtrlTagSandboxCaption = 'Tag ...';                       // vcaTagSandBox
  RsVersionCtrlUpdateCaption = 'U&pdate';                           // vcaUpdate
  RsVersionCtrlUpdateSandboxCaption = 'Update ...';                 // vcaUpdateSandbox
  RsVersionCtrlUpdateToCaption = 'Update &to ';                     // vcaUpdateTo
  RsVersionCtrlUpdateSandboxToCaption = 'Update to ...';            // vcaUpdateSandboxTo
  RsVersionCtrlUnlockCaption = '&Unlock';                           // vcaUnlock
  RsVersionCtrlUnlockSandboxCaption = 'Unlock ...';                 // vcaUnlockSandbox

//=== JclVersionCtrlCVSImpl ==================================================
resourcestring
  RsVersionCtrlCVSName = 'cvs';
  RsEEmptyFileName = 'Error: empty file name';
  RSENoTortoiseCVS = 'TortoiseCVS is not detected on the system';

//=== JclVersionCtrlSVNImpl ==================================================
resourcestring
  RsVersionCtrlSVNName = 'subversion';
  RSENoTortoiseSVN = 'TortoiseSVN is not detected on the system';

  //=== JclVersionCtrlGITImpl ==================================================
resourcestring
  RsVersionCtrlGITName = 'git';
  RSENoTortoiseGIT = 'TortoiseGIT is not detected on the system';

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\vcl';
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
