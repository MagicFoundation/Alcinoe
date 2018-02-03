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
{ The Original Code is ToolsUtils.pas.                                                            }
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

unit ToolsUtils;

{$I JCL.INC}

interface

uses
  Windows, Classes, SysUtils, ComCtrls, Math, ComObj, ActiveX, Controls, Forms,
  ImageHlp, JclFileUtils, JclStrings, JclSysInfo, JclRegistry, JclShell;

const
  PeViewerClassName = 'PeViewer.PeViewerControl';

function CreateOrGetOleObject(const ClassName: string): IDispatch;

function FmtStrToInt(S: string): Integer;

function GetImageBase(const FileName: TFileName): DWORD;

function IntToExtended(I: Integer): Extended;

function InfoTipVersionString(const FileName: TFileName): string;

function IsPeViewerRegistred: Boolean;

procedure LVColumnClick(Column: TListColumn);

procedure LVCompare(ListView: TListView; Item1, Item2: TListItem; var Compare: Integer);

procedure ListViewFocusFirstItem(ListView: TListView);

procedure ListViewSelectAll(ListView: TListView; Deselect: Boolean = False);

procedure ListViewToStrings(ListView: TListView; Strings: TStrings;
  SelectedOnly: Boolean = False; Headers: Boolean = True);

function MessBox(const Text: string; Flags: Word): Integer;

function MessBoxFmt(const Fmt: string; const Args: array of const; Flags: Word): Integer;

function SafeSubItemString(Item: TListItem; SubItemIndex: Integer): string;

procedure SendEmail;

procedure ShowToolsAboutBox;

function Win32HelpFileName: TFileName;

procedure Fix_ListViewBeforeClose(Form: TForm);

procedure D4FixCoolBarResizePaint(CoolBar: TObject);

implementation

uses
  {$IFDEF HAS_UNIT_CHARACTER}
  Character,
  {$ENDIF HAS_UNIT_CHARACTER}
  About, CommCtrl, JclPeImage, JclWin32;

resourcestring
  RsJCLLink = 'JEDI Code Library;http://delphi-jedi.org/Jedi:CODELIBJCL';
  RsEmailAddress = 'mailto:petr.v@mujmail.cz?subject=[Delphi Tools]';

function StrEmpty(const S: string): Boolean;
begin
  Result := Length(Trim(S)) = 0;
end;

function CreateOrGetOleObject(const ClassName: string): IDispatch;
var
  ClassID: TCLSID;
  Res: HResult;
  Unknown: IUnknown;
begin
  ClassID := ProgIDToClassID(ClassName);
  Res := GetActiveObject(ClassID, nil, Unknown);
  if Succeeded(Res) then
    OleCheck(Unknown.QueryInterface(IDispatch, Result))
  else
  begin
    if Res <> MK_E_UNAVAILABLE then OleError(Res);
    OleCheck(CoCreateInstance(ClassID, nil, CLSCTX_INPROC_SERVER or
      CLSCTX_LOCAL_SERVER, IDispatch, Result));
  end;
end;

function FmtStrToInt(S: string): Integer;
var
  I: Integer;
begin
  I := 1;
  while I <= Length(S) do
    if (not CharIsDigit(S[I])) and (S[I] <> '-') then
      Delete(S, I, 1)
    else
      Inc(I);
  Result := StrToIntDef(S, 0);
end;

function GetImageBase(const FileName: TFileName): DWORD;
var
  NtHeaders32: TImageNtHeaders32;
  NtHeaders64: TImageNtHeaders64;
  ImageStream: TMemoryStream;
  PETarget: TJclPeTarget;
begin
  ImageStream := TMemoryStream.Create;
  try
    ImageStream.LoadFromFile(FileName);
    PETarget := PeMapImgTarget(ImageStream.Memory);
  finally
    ImageStream.Free;
  end;
  if (PETarget = taWin32) and PeGetNtHeaders32(FileName, NtHeaders32) then
    Result := NtHeaders32.OptionalHeader.ImageBase
  else
  if (PETarget = taWin64) and PeGetNtHeaders64(FileName, NtHeaders64) then
    Result := NtHeaders64.OptionalHeader.ImageBase
  else
    Result := 0;
end;

function IntToExtended(I: Integer): Extended;
begin
  Result := I;
end;

function InfoTipVersionString(const FileName: TFileName): string;
begin
  Result := '';
  if VersionResourceAvailable(FileName) then
  try
    with TJclFileVersionInfo.Create(FileName) do
    try
      if not StrEmpty(FileVersion) then Result := FileVersion;
      if not StrEmpty(FileDescription) then
        Result := Format('%s'#13#10'%s', [Result, FileDescription])
    finally
      Free;
    end;
  except
  end;
end;

function IsPeViewerRegistred: Boolean;
begin
  Result := RegReadStringDef(HKEY_CLASSES_ROOT, PeViewerClassName, '', '') <> '';
end;

procedure LVColumnClick(Column: TListColumn);
var
  ColIndex: Integer;
  ListView: TListView;
begin
  ListView := TListColumns(Column.Collection).Owner as TListView;
  ColIndex := Column.Index;
  with ListView do
  begin
    if Tag and $FF = ColIndex then
      Tag := Tag xor $100
    else
      Tag := ColIndex;
    AlphaSort;
    if Selected <> nil then Selected.MakeVisible(False);
  end;
end;

procedure LVCompare(ListView: TListView; Item1, Item2: TListItem; var Compare: Integer);
var
  ColIndex: Integer;
begin
  with ListView do
  begin
    ColIndex := Tag and $FF - 1;
    if Columns[ColIndex + 1].Alignment = taLeftJustify then
    begin
      if ColIndex = -1 then
        Compare := AnsiCompareText(Item1.Caption, Item2.Caption)
      else
        Compare := AnsiCompareText(Item1.SubItems[ColIndex], Item2.SubItems[ColIndex]);
    end else
    begin
      if ColIndex = -1 then
        Compare := FmtStrToInt(Item1.Caption) - FmtStrToInt(Item2.Caption)
      else
        Compare := FmtStrToInt(Item1.SubItems[ColIndex]) - FmtStrToInt(Item2.SubItems[ColIndex]);
    end;
    if Tag and $100 <> 0 then Compare := -Compare;
  end;
end;

procedure ListViewFocusFirstItem(ListView: TListView);
begin
  with ListView do
    if Items.Count > 0 then
    begin
      ItemFocused := Items[0];
      ItemFocused.Selected := True;
      ItemFocused.MakeVisible(False);
    end;
end;

procedure ListViewSelectAll(ListView: TListView; Deselect: Boolean);
var
  I: Integer;
  H: THandle;
  Data: Integer;
  SaveOnSelectItem: TLVSelectItemEvent;
begin
  with ListView do if MultiSelect then
  begin
    Items.BeginUpdate;
    SaveOnSelectItem := OnSelectItem;
    Screen.Cursor := crHourGlass;
    try
      H := Handle;
      OnSelectItem := nil;
      if Deselect then Data := 0 else Data := LVIS_SELECTED;
      for I := 0 to Items.Count - 1 do
        ListView_SetItemState(H, I, Data, LVIS_SELECTED);
    finally
      OnSelectItem := SaveOnSelectItem;
      Items.EndUpdate;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure ListViewToStrings(ListView: TListView; Strings: TStrings;
  SelectedOnly: Boolean = False; Headers: Boolean = True);
var
  R, C: Integer;
  ColWidths: array of Word;
  S: String;

  procedure AddLine;
begin
  Strings.Add(TrimRight(S));
end;

  function MakeCellStr(const Text: String; Index: Integer): String;
begin
  with ListView.Columns[Index] do
    if Alignment = taLeftJustify then
      Result := StrPadRight(Text, ColWidths[Index] + 1)
    else
      Result := StrPadLeft(Text, ColWidths[Index]) + ' ';
end;

begin
  SetLength(S, 256);
  with ListView do
  begin
    SetLength(ColWidths, Columns.Count);
    if Headers then
      for C := 0 to Columns.Count - 1 do
        ColWidths[C] := Length(Trim(Columns[C].Caption));
    for R := 0 to Items.Count - 1 do
      if not SelectedOnly or Items[R].Selected then
      begin
        ColWidths[0] := Max(ColWidths[0], Length(Trim(Items[R].Caption)));
        for C := 0 to Items[R].SubItems.Count - 1 do
          ColWidths[C + 1] := Max(ColWidths[C + 1], Length(Trim(Items[R].SubItems[C])));
      end;
    Strings.BeginUpdate;
    try
      if Headers then
        with Columns do
        begin
          S := '';
          for C := 0 to Count - 1 do
            S := S + MakeCellStr(Items[C].Caption, C);
          AddLine;
          S := '';
          for C := 0 to Count - 1 do
            S := S + StringOfChar('-', ColWidths[C]) + ' ';
          AddLine;
        end;
      for R := 0 to Items.Count - 1 do
        if not SelectedOnly or Items[R].Selected then
        with Items[R] do
        begin
          S := MakeCellStr(Caption, 0);
          for C := 0 to Min(SubItems.Count, Columns.Count - 1) - 1 do
            S := S + MakeCellStr(SubItems[C], C + 1);
          AddLine;
        end;
    finally
      Strings.EndUpdate;
    end;
  end;
end;

function MessBox(const Text: string; Flags: Word): Integer;
begin
  with Application do Result := MessageBox(PChar(Text), PChar(Title), Flags);
end;

function MessBoxFmt(const Fmt: string; const Args: array of const; Flags: Word): Integer;
begin
  Result := MessBox(Format(Fmt, Args), Flags);
end;

function SafeSubItemString(Item: TListItem; SubItemIndex: Integer): string;
begin
  if Item.SubItems.Count > SubItemIndex then
    Result := Item.SubItems[SubItemIndex]
  else
    Result := ''
end;

procedure SendEmail;
begin
  ShellExecEx(RsEmailAddress);
end;

procedure ShowToolsAboutBox;
begin
  ShowAbout([RsJCLLink], 18);
end;

function Win32HelpFileName: TFileName;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE,
    'SOFTWARE\Borland\Borland Shared\MSHelp', 'RootDir', '') + '\Win32.hlp';
  if not FileExists(Result) then Result := '';
end;

procedure Fix_ListViewBeforeClose(Form: TForm);
var
  I: Integer;
begin
  with Form do
    for I := 0 to ComponentCount - 1 do
      if Components[I] is TListView then
        with TListView(Components[I]) do
          if OwnerData then Items.Count := 0;
end;

procedure D4FixCoolBarResizePaint(CoolBar: TObject);
{$IFDEF DELPHI4}
var
  R: TRect;
begin
  with CoolBar as TCoolBar do
  begin
    R := ClientRect;
    R.Left := R.Right - 8;
    InvalidateRect(Handle, @R, True);
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}

end.
