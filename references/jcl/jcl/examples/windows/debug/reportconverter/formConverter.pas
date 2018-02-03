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
{ The Original Code is formConverter.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Elahn Ientile                                      }
{ Portions created by Elahn Ientile are Copyright (C) Elahn Ientile.                               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Converts a report send by TExceptionDialogMail where no Debug Info or Map file was present into  }
{ a report equivalent to one where Debug Info or Map file was present, i.e. inc. line numbers, etc }
{                                                                                                  }
{ Note: to generate a Map file on compile, add "-GD" to [project name].cfg                         }
{       the map file used must be the one generated when that copy of the program was compiled     }
{                                                                                                  }
{ Unit owner: Elahn Ientile                                                                        }
{ Last modified: $Date$                             }
{                                                                                                  }
{**************************************************************************************************}

unit formConverter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JclDebug;

type
  TfrmConverter = class(TForm)
    btnConvert: TButton;
    txtReportFile: TEdit;
    txtMapFile: TEdit;
    btnReportFile: TButton;
    btnMapFile: TButton;
    dlgOpen: TOpenDialog;
    procedure btnConvertClick(Sender: TObject);
    procedure btnReportFileClick(Sender: TObject);
    procedure btnMapFileClick(Sender: TObject);
  private
    FScanner: TJclMapScanner;
  public
    procedure ConvertFile(AMap, AInput, AOutput: TFileName);
    function FormatInfo(var Info: TJclLocationInfo;
      IncludeAddressOffset: Boolean = True;
      IncludeStartProcLineOffset: Boolean = True): string;
    function GetVALocationInfo(const VA: DWORD; var Info: TJclLocationInfo): Boolean;
  end;

var
  frmConverter: TfrmConverter;

implementation

{$R *.dfm}

procedure TfrmConverter.btnConvertClick(Sender: TObject);
var
  lStr, lExt: string;
begin
  if not FileExists(txtReportFile.Text) then
    ShowMessage('Report File does not exist.')
  else if not FileExists(txtMapFile.Text) then
    ShowMessage('Map File does not exist.')
  else
  begin
    lStr := txtReportFile.Text;
    lExt := ExtractFileExt(lStr);
    Insert('.converted', lStr, Length(lStr) + 1 - Length(lExt));
    ConvertFile(txtMapFile.Text, txtReportFile.Text, lStr);
  end;
end;

procedure TfrmConverter.ConvertFile(AMap, AInput, AOutput: TFileName);
var
  ls: TStringList;
  i: Integer;
  lInStackList: Boolean;
  s: string;
  VA: DWORD;
  Info: TJclLocationInfo;
begin
  FScanner := TJclMapScanner.Create(AMap);
  ls := TStringList.Create;
  try
    ls.LoadFromFile(AInput);
    if ls.Count = 0 then
      Exit;
    lInStackList := False;
    i := 0;
    while i < ls.Count do
    begin
      if (Copy(ls[i], 1, 3) = '---') then
        if lInStackList then
          Break
        else if Copy(ls[i+1], 1, 10) = 'Stack list' then
        begin
          lInStackList := True;
          Inc(i, 2);
        end;
      if lInStackList and (Copy(ls[i], 1, 1) = '(') then
      begin
        s := '$' + Copy(ls[i], 2, 8);
        VA := DWORD(StrToInt64(s));
        if GetVALocationInfo(VA, Info) then
          ls[i] := ls[i] + FormatInfo(Info);
      end;
      Inc(i);
    end;
    ls.SaveToFile(AOutput);
    ShowMessage('Successfully converted. Output filename:' + #13#10#13#10 + AOutput);
  finally
    ls.Free;
  end;
end;

function TfrmConverter.FormatInfo(var Info: TJclLocationInfo;
  IncludeAddressOffset, IncludeStartProcLineOffset: Boolean): string;
var
  StartProcInfo: TJclLocationInfo;
  OffsetStr, StartProcOffsetStr: string;
begin
  OffsetStr := '';
  with Info do
  begin
    if LineNumber > 0 then
    begin
      if IncludeStartProcLineOffset and GetVALocationInfo(DWORD(Cardinal(Info.Address) -
        Cardinal(Info.OffsetFromProcName)), StartProcInfo) and (StartProcInfo.LineNumber > 0) then
          StartProcOffsetStr := Format(' + %d', [LineNumber - StartProcInfo.LineNumber])
      else
        StartProcOffsetStr := '';
      if IncludeAddressOffset then
      begin
        if OffsetFromLineNumber >= 0 then
          OffsetStr := Format(' + $%x', [OffsetFromLineNumber])
        else
          OffsetStr := Format(' - $%x', [-OffsetFromLineNumber])
      end;
      Result := Format(' %s.%s (Line %u, "%s"%s)%s', [UnitName, ProcedureName, LineNumber,
        SourceName, StartProcOffsetStr, OffsetStr]);
    end
    else
    begin
      if IncludeAddressOffset then
        OffsetStr := Format(' + $%x', [OffsetFromProcName]);
      if UnitName <> '' then
        Result := Format(' %s.%s%s', [UnitName, ProcedureName, OffsetStr])
      else
        Result := Format(' %s%s', [ProcedureName, OffsetStr]);
    end;
  end;
end;

function TfrmConverter.GetVALocationInfo(const VA: DWORD; var Info: TJclLocationInfo): Boolean;
begin
  with FScanner do
  begin
    Info.UnitName := ModuleNameFromAddr(VA);
    Result := (Info.UnitName <> '');
    if Result then
    begin
      Info.Address := Pointer(VA);
      Info.ProcedureName := ProcNameFromAddr(VA, Info.OffsetFromProcName);
      Info.LineNumber := LineNumberFromAddr(VA, Info.OffsetFromLineNumber);
      Info.SourceName := SourceNameFromAddr(VA);
      Info.DebugInfo := nil;
    end;
  end;
end;

procedure TfrmConverter.btnReportFileClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    txtReportFile.Text := dlgOpen.FileName;
end;

procedure TfrmConverter.btnMapFileClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    txtMapFile.Text := dlgOpen.FileName;
end;

end.
