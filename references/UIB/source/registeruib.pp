{******************************************************************************}
{                        UNIFIED INTERBASE (UIB)                               }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is UIBReg.pas.                                           }
{                                                                              }
{ Register Components for Lazarus                                              }
{                                                                              }
{ Unit owner:    Olivier Guilbaud                                              }
{ Last modified: Jun 30, 2003                                                  }
{                                                                              }
{ History                                                                      }
{   dec 10 2003 : Add  TUIBDataSet and TUIBScript                          }                                            {                                                                              }
{******************************************************************************}
unit registeruib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, uib, uibdataset, LazarusPackageIntf;

procedure Register;

implementation

procedure RegisterUnitUIB;
begin
  RegisterComponents('UIB', [TUIBDatabase, TUIBTransaction, TUIBQuery,
    TUIBBackup, TUIBRestore, TUIBScript, TUIBEvents, TUIBRepair,
    TUIBSecurity, TUIBConfig, TUIBServerInfo]);
end;

procedure RegisterUnitDataSet;
begin
  RegisterComponents('UIB', [TUIBDataSet]);
end;

procedure Register;
begin
  RegisterUnit('UIB',@RegisterUnitUIB);
  RegisterUnit('UIBDataSet',@RegisterUnitDataSet);
end;

initialization
  {$i registeruib.lrs}

end.
