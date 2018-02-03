{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{ DUnit Test Unit                                                                                  }
{                                                                                                  }
{ Covers:      JclDebug                                                                            }
{ Last Update: $Date$                                }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{**************************************************************************************************}

unit TestJclDebug;

interface

uses
  Windows, SysUtils, Classes, TestFramework, JclDebug;

type
  TJclMapScannerTest = class(TTestCase)
  published
    procedure ScanCPP4096;
  end;

implementation

{$R TestJclDebug.res}

//==================================================================================================
// TJclDebugTest
//==================================================================================================

procedure TJclMapScannerTest.ScanCPP4096;
var
  MapFileName: string;
  RS: TResourceStream;
  FS: TFileStream;
  MapScanner: TJclMapScanner;
  ModuleStr, ProcStr: string;
begin
  MapFileName := ExtractFilePath(ParamStr(0)) + 'CPP4096.map';
  DeleteFile(MapFileName);
  try
    RS := TResourceStream.Create(HInstance, 'MapFileCPP4096', RT_RCDATA);
    try
      RS.SaveToFile(MapFileName);
    finally
      RS.Free;
    end;
    FS := TFileStream.Create(MapFileName, fmOpenRead);
    try
      Check(FS.Size = 4096, 'Map file size <> 4096');
    finally
      FS.Free;
    end;
    MapScanner := TJclMapScanner.Create(MapFileName);
    try
      ModuleStr := MapScanner.ModuleNameFromAddr($100);
      ProcStr := MapScanner.ProcNameFromAddr($100);
      Check((ModuleStr = 'FOO') or (ModuleStr = 'C:\TEST\FOO.OBJ'), '0x00000100: Module name mismatch');
      Check(ProcStr = 'test::testproc32', '0x00000100: Proc name mismatch');

      ModuleStr := MapScanner.ModuleNameFromAddr($900);
      ProcStr := MapScanner.ProcNameFromAddr($900);
      Check((ModuleStr = 'BAR') or (ModuleStr = 'C:\TEST\BAR.OBJ'), '0x00000900: Module name mismatch');
      Check(ProcStr = 'test::this_is_the_last_proc', '0x00000900: Proc name mismatch');
    finally
      MapScanner.Free;
    end;
  finally
    DeleteFile(MapFileName);
  end;
end;

initialization
  RegisterTest('JclDebug', TJclMapScannerTest.Suite);

end.
