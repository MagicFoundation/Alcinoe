{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) - Delphi Tools                                                   }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is PeGenDef.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date$                                                      }
{                                                                                                  }
{**************************************************************************************************}

unit PeGenDef;

{$I JCL.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JclPeImage, ComCtrls, StdCtrls, Spin;

type
  TPeUnitGenFlags = set of (ufDecorated, ufDuplicate, ufVariable);

  TPeUnitGenerator = class(TJclPeImage)
  private
    FUnitGenFlags: array of TPeUnitGenFlags;
    function GetUnitGenFlags(Index: Integer): TPeUnitGenFlags;
  public
    procedure GenerateUnit(Strings: TStrings; const LibConst: string; WrapPos: Integer);
    procedure ScanExports;
    property UnitGenFlags[Index: Integer]: TPeUnitGenFlags read GetUnitGenFlags;
  end;

  TPeGenDefChild = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    FunctionsListView: TListView;
    UnitRichEdit: TRichEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    LibConstNameEdit: TEdit;
    WrapSpinEdit: TSpinEdit;
    WrapCheckBox: TCheckBox;
    SaveDialog: TSaveDialog;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FunctionsListViewData(Sender: TObject; Item: TListItem);
    procedure FunctionsListViewCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure PageControl1Change(Sender: TObject);
    procedure WrapCheckBoxClick(Sender: TObject);
  private
    FPeUnitGenerator: TPeUnitGenerator;
    procedure SetFileName(const Value: TFileName);
    function GetFileName: TFileName;
    procedure GenerateUnit;
  public
    function CanSave: Boolean;
    procedure SaveUnit;
    property FileName: TFileName read GetFileName write SetFileName;
  end;

var
  PeGenDefChild: TPeGenDefChild;

implementation

uses PeViewerMain, JclFileUtils, ToolsUtils, JclSysUtils;

{$R *.DFM}

const
  nfDecoratedName   = $01;
  nfAnsiUnicodePair = $02;

function PascalizeName(const Name: string): string;
  function CharIsValidLeadingChar(const C: Char): Boolean;
  begin
    case C of
      'A'..'Z',
      'a'..'z':
        Result := True;
    else
      Result := False;
    end;
  end;
  function CharIsStripLeadingChar(const C: Char): Boolean;
  begin
    Result := C = '_';
  end;
  function CharIsValid(const C: Char): Boolean;
  begin
    case C of
      'A'..'Z',
      'a'..'z',
      '0'..'9':
        Result := True;
    else
      Result := False;
    end;
  end;
const
  InvalidCharReplacement = '_';
  StopChar = '@';
var
  I: Integer;
  C: Char;
begin
  SetLength(Result, Length(Name));
  Result := '';
  for I := 1 to Length(Name) do
  begin
    C := Name[I];
    if I = 1 then
    begin
      if CharIsValidLeadingChar(C) then
        Result := Result + C
      else
      if not CharIsStripLeadingChar(C) then
        Break; // probably MS C++ or Borland name decoration
    end else
    begin
      if CharIsValid(C) then
        Result := Result + C
      else
      if C = StopChar then
        Break
      else
        Result := Result + InvalidCharReplacement;
    end;
  end;
  I := Length(Result);
  while I > 0 do
    if Result[I] = InvalidCharReplacement then
    begin
      Delete(Result, I, 1);
      Dec(I);
    end
    else
      Break;
end;

function PossiblyAnsiUnicodePair(const Name1, Name2: AnsiString): Boolean;
const
  AnsiUnicodeSuffixes = ['A', 'W'];
var
  L1, L2: Integer;
  Suffix1, Suffix2: AnsiChar;
begin
  Result := False;
  L1 := Length(Name1);
  L2 := Length(Name2);
  if (L1 = L2) and (L1 > 1) then
  begin
    Suffix1 := Name1[L1];
    Suffix2 := Name2[L2];
    Result := (Suffix1 in AnsiUnicodeSuffixes) and (Suffix2 in AnsiUnicodeSuffixes) and
      (Suffix1 <> Suffix2) and (Copy(Name1, 1, L1 - 1) = Copy(Name2, 1, L2 - 1));
  end;
end;

function IsDecoratedName(const Name: string): Boolean;
begin
  Result := (Length(Name) > 1) and (Name[1] = '?') and (Name[1] = '@');
end;


{ TPeUnitGenerator }

procedure TPeUnitGenerator.GenerateUnit(Strings: TStrings; const LibConst: string;
  WrapPos: Integer);
var
  I: Integer;
  S: string;
begin
  Strings.Add('implementation');
  Strings.Add('');
  Strings.Add('const');
  Strings.Add(Format('  %s = ''%s'';', [LibConst, ExtractFileName(FileName)]));
  Strings.Add('');
  for I := 0 to ExportList.Count - 1 do
    with ExportList[I] do
      if FUnitGenFlags[I] = [] then
      begin
        S := Format('function %s; external %s name ''%s'';', [PascalizeName(Name), LibConst, Name]);
        if WrapPos > 0 then
          S := WrapText(S, #13#10'  ', [' '], WrapPos);
        Strings.Add(S);
      end;
  Strings.Add('');
  Strings.Add('end.');
end;

function TPeUnitGenerator.GetUnitGenFlags(Index: Integer): TPeUnitGenFlags;
begin
  Result := FUnitGenFlags[Index];
end;

procedure TPeUnitGenerator.ScanExports;
var
  I: Integer;
  PascalName, LastName, FirstSectionName: string;
  LastAddress: DWORD;
  Flags: TPeUnitGenFlags;
begin
  SetLength(FUnitGenFlags, ExportList.Count);
  ExportList.SortList(esName);
  LastName := '';
  LastAddress := 0;
  FirstSectionName := ImageSectionNames[0]; // The first section is code section
  for I := 0 to ExportList.Count - 1 do
    with ExportList[I] do
    begin
      Flags := [];
      if SectionName <> FirstSectionName then
        Include(Flags, ufVariable)
      else
      if IsDecoratedName(Name) then
        Include(Flags, ufDecorated)
      else
      begin
        PascalName := PascalizeName(Name);
        if (LastAddress = Address) and (LastName = PascalName) then
          Include(Flags, ufDuplicate);
        LastName := PascalName;
        LastAddress := Address;
      end;
      FUnitGenFlags[I] := Flags;
    end;
end;

{ TPeGenDefChild }

procedure TPeGenDefChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Fix_ListViewBeforeClose(Self);
  Action := caFree;
end;

procedure TPeGenDefChild.FormCreate(Sender: TObject);
begin
  FPeUnitGenerator := TPeUnitGenerator.Create;
end;

procedure TPeGenDefChild.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPeUnitGenerator);
end;

function TPeGenDefChild.GetFileName: TFileName;
begin
  Result := FPeUnitGenerator.FileName;
end;

procedure TPeGenDefChild.SetFileName(const Value: TFileName);
begin
  Screen.Cursor := crHourGlass;
  try
    FPeUnitGenerator.FileName := Value;
    FPeUnitGenerator.ScanExports;
    LibConstNameEdit.Text := PathExtractFileNameNoExt(Value) + 'Lib';
    FunctionsListView.Items.Count := FPeUnitGenerator.ExportList.Count;
    FunctionsListView.Invalidate;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TPeGenDefChild.FunctionsListViewData(Sender: TObject; Item: TListItem);
var
  Flags: TPeUnitGenFlags;
begin
  Flags := FPeUnitGenerator.UnitGenFlags[Item.Index];
  with Item, FPeUnitGenerator.ExportList[Item.Index] do
  begin
    Caption := Name;
    SubItems.Add(PascalizeName(Name));
    SubItems.Add(AddressOrForwardStr);
    if ufDuplicate in Flags then
      ImageIndex := icoWarning
    else
    if Flags * [ufDecorated, ufVariable] = [] then
      ImageIndex := icoExports
    else
      ImageIndex := -1;
  end;
end;

procedure TPeGenDefChild.FunctionsListViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Flags: TPeUnitGenFlags;
begin
  Flags := FPeUnitGenerator.UnitGenFlags[Item.Index];
  if Flags * [ufDecorated, ufVariable] <> [] then
    Sender.Canvas.Font.Style := [fsStrikeOut];
end;

procedure TPeGenDefChild.GenerateUnit;
var
  SL: TStringList;
  WrapColumn: Integer;
begin
  Screen.Cursor := crHourGlass;
  SL := TStringList.Create;
  try
    if WrapCheckBox.Checked then
      WrapColumn := WrapSpinEdit.Value
    else
      WrapColumn := 0;
    FPeUnitGenerator.GenerateUnit(SL, LibConstNameEdit.Text, WrapColumn);
    UnitRichEdit.Text := SL.Text;
  finally
    SL.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TPeGenDefChild.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage = TabSheet1 then
    LibConstNameEdit.SetFocus
  else
  if PageControl1.ActivePage = TabSheet2 then
    GenerateUnit;
end;

procedure TPeGenDefChild.WrapCheckBoxClick(Sender: TObject);
begin
  WrapSpinEdit.Enabled := WrapCheckBox.Checked;
end;

function TPeGenDefChild.CanSave: Boolean;
begin
  Result := PageControl1.ActivePage = TabSheet2;
end;

procedure TPeGenDefChild.SaveUnit;
begin
  with SaveDialog do
  begin
    FileName := PathExtractFileNameNoExt(FPeUnitGenerator.FileName);
    if Execute then
      UnitRichEdit.Lines.SaveToFile(FileName);
  end;
end;

end.
