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
{ The Original Code is D6MdiMsgFix.pas.                                                            }
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

unit FindDlg;

{$I JCL.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TFindTextForm = class(TForm)
    FindBtn: TButton;
    CancelBtn: TButton;
    ProgressBar1: TProgressBar;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    SearchTextEdit: TEdit;
    ColumnComboBox: TComboBox;
    Label2: TLabel;
    CaseCheckBox: TCheckBox;
    ExactCheckBox: TCheckBox;
    procedure FindBtnClick(Sender: TObject);
  private
    FListView: TListView;
    procedure SetListView(const Value: TListView);
  public
    function Find: Boolean;
    class function CanExecuteFind: Boolean;
    property ListView: TListView read FListView write SetListView;
  end;

function ShowFindDialog(AListView: TListView): Boolean;

var
  FindTextForm: TFindTextForm;

implementation

{$R *.DFM}

resourcestring
  RsAllColumns = '[all columns]';

function ShowFindDialog(AListView: TListView): Boolean;
begin
  with TFindTextForm.Create(Application) do
  try
    ListView := AListView;
    Result := ShowModal = mrOk;
  finally
    Free;
  end;
end;

{ TFindForm }

function TFindTextForm.Find: Boolean;
var
  R, C, FindColumn, ColCount, FoundRow: Integer;
  IgnoreCase, ExactMatch: Boolean;
  SearchText: string;


  function CompareColumnText(ColumnIndex: Integer): Boolean;
  var
    Text: string;
  begin
    with FListView.Items[R] do
      if ColumnIndex = 0 then
        Text := Caption
      else
        Text := SubItems[ColumnIndex - 1];
    if IgnoreCase then
      Text := AnsiUpperCase(Text);
    if ExactMatch then
      Result := (SearchText = Text)
    else
      Result := (Pos(SearchText, Text) > 0);
    if Result then
      FoundRow := R;
  end;

begin
  SearchTextEdit.Enabled := False;
  ColumnComboBox.Enabled := False;
  CaseCheckBox.Enabled := False;
  ExactCheckBox.Enabled := False;
  GroupBox1.Enabled := False;
  FindBtn.Enabled := False;
  CancelBtn.Enabled := False;
  Update;
  Result := False;
  with FListView do
  begin
    if ItemFocused = nil then
    begin
      ItemFocused := Items[0];
      ItemFocused.MakeVisible(False);
    end;
    ProgressBar1.Max := Items.Count;
    ProgressBar1.Min := ItemFocused.Index;
    ProgressBar1.Position := ItemFocused.Index;
    ProgressBar1.Visible := True;
    FindColumn := ColumnComboBox.ItemIndex - 1;
    ColCount := Columns.Count;
    FoundRow := -1;
    IgnoreCase := CaseCheckBox.Checked;
    ExactMatch := ExactCheckBox.Checked;
    if IgnoreCase then
      SearchText := AnsiUpperCase(SearchTextEdit.Text)
    else
      SearchText := SearchTextEdit.Text;
    for R := ItemFocused.Index + 1 to Items.Count - 1 do
    begin
      if FindColumn = -1 then
        for C := 0 to ColCount - 1 do
          CompareColumnText(C)
      else
        CompareColumnText(FindColumn);
      if R mod ProgressBar1.Step = 0 then
        ProgressBar1.StepIt;
      if FoundRow > -1 then
      begin
        Result := True;
        if Selected <> nil then
          Selected.Selected := False;
        ItemFocused := Items[FoundRow];
        Selected := ItemFocused;
        ItemFocused.MakeVisible(False);
        Break;
      end;
    end;
  end;
  SearchTextEdit.Enabled := True;
  ColumnComboBox.Enabled := True;
  CaseCheckBox.Enabled := True;
  ExactCheckBox.Enabled := True;
  GroupBox1.Enabled := True;
  ProgressBar1.Visible := False;
  FindBtn.Enabled := True;
  CancelBtn.Enabled := True;
  SearchTextEdit.SetFocus;
end;

procedure TFindTextForm.SetListView(const Value: TListView);
var
  I: Integer;
begin
  FListView := Value;
  ColumnComboBox.Items.BeginUpdate;
  ColumnComboBox.Items.Clear;
  ColumnComboBox.Items.Add(RsAllColumns);
  for I := 0 to FListView.Columns.Count - 1 do
    ColumnComboBox.Items.Add(FListView.Columns[I].Caption);
  ColumnComboBox.Items.EndUpdate;
  ColumnComboBox.ItemIndex := 0;
end;

procedure TFindTextForm.FindBtnClick(Sender: TObject);
begin
  Find;
end;

class function TFindTextForm.CanExecuteFind: Boolean;
var
  LV: TListView;
begin
  Result := (Screen.Activecontrol is TListView);
  if Result then
  begin
    LV := TListView(Screen.Activecontrol);
    Result := (LV.Items.Count > 0) and not LV.HideSelection;
  end;    
end;

end.
