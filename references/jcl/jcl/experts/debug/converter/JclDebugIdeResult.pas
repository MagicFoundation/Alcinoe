{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) extension                                                        }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.0 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclDebugResult.pas.                                                         }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying help file JCL.chm.  }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclDebugIdeResult;

{$I jcl.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, ComCtrls, StdCtrls, ImgList,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclOtaUtils;

type
  TJclDebugResultForm = class(TForm)
    OkBtn: TButton;
    ResultListView: TListView;
    ImageList1: TImageList;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FSettings: TJclOtaSettings;
    procedure CopyReportToClipboard;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    property Settings: TJclOtaSettings read FSettings;
  public
    constructor Create(AOwner: TComponent; ASettings: TJclOTASettings); reintroduce;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\debug\converter';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

{$R *.dfm}

uses
  Clipbrd, Math,
  JclStrings,
  JclOtaConsts,
  JclOtaResources;

procedure ListViewToStrings(ListView: TListView; Strings: TStrings;
  SelectedOnly: Boolean = False; Headers: Boolean = True);
var
  R, C: Integer;
  ColWidths: array of Word;
  S: string;

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

//=== { TJclDebugResultForm } ================================================

procedure TJclDebugResultForm.CopyReportToClipboard;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    ListViewToStrings(ResultListView, SL);
    Clipboard.AsText := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure TJclDebugResultForm.FormResize(Sender: TObject);
begin
  OkBtn.Left := ClientWidth div 2 - OkBtn.Width div 2;
end;

constructor TJclDebugResultForm.Create(AOwner: TComponent; ASettings: TJclOTASettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
end;

procedure TJclDebugResultForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  // Fixing the Window Ghosting "bug"
  Params.Style := params.Style or WS_POPUP;
  if Assigned(Screen.ActiveForm) then
    Params.WndParent := Screen.ActiveForm.Handle
  else if Assigned (Application.MainForm) then
    Params.WndParent := Application.MainForm.Handle
  else
    Params.WndParent := Application.Handle;
end;

procedure TJclDebugResultForm.FormShow(Sender: TObject);
var
  Index: Integer;
begin
  SetBounds(Settings.LoadInteger(JclLeft, Left),
            Settings.LoadInteger(JclTop, Top),
            Settings.LoadInteger(JclWidth, Width),
            Settings.LoadInteger(JclHeight, Height));

  OkBtn.Caption := LoadResString(@RsOk);
  ResultListView.Columns.Items[0].Caption := LoadResString(@RsProject);
  ResultListView.Columns.Items[1].Caption := LoadResString(@RsMapFileSize);
  ResultListView.Columns.Items[2].Caption := LoadResString(@RsJCLDebugSize);
  ResultListView.Columns.Items[3].Caption := LoadResString(@RsRatio);
  ResultListView.Columns.Items[4].Caption := LoadResString(@RsExecutableFileName);
  ResultListView.Columns.Items[5].Caption := LoadResString(@RsLinkerBug);
  ResultListView.Columns.Items[6].Caption := LoadResString(@RsLineErrors);

  with ResultListView.Columns do
    for Index := 0 to Count - 1 do
      Items[Index].Width := Settings.LoadInteger(Format(ColumnRegName, [Index]), Items[Index].Width);
end;

procedure TJclDebugResultForm.FormDestroy(Sender: TObject);
var
  Index: Integer;
begin
  Settings.SaveInteger(JclLeft, Left);
  Settings.SaveInteger(JclTop, Top);
  Settings.SaveInteger(JclWidth, Width);
  Settings.SaveInteger(JclHeight, Height);

  with ResultListView.Columns do
    for Index := 0 to Count - 1 do
      Settings.SaveInteger(Format(ColumnRegName, [Index]), Items[Index].Width);
end;

procedure TJclDebugResultForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = Ord('C')) then
  begin
    CopyReportToClipboard;
    MessageBeep(MB_OK);
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
