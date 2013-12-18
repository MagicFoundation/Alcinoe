(********************************************************************************)
(*                        UNIFIED INTERBASE (UIB)                               *)
(*                                                                              *)
(* The contents of this file are subject to the Mozilla Public License Version  *)
(* 1.1 (the "License"); you may not use this file except in compliance with the *)
(* License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ *)
(*                                                                              *)
(* Software distributed under the License is distributed on an "AS IS" basis,   *)
(* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for *)
(* the specific language governing rights and limitations under the License.    *)
(*                                                                              *)
(* Unit owner : Henri Gourvest <hgourvest@progdigy.com>                         *)
(*                                                                              *)
(********************************************************************************)

unit uibtransactionedit;

{$I uib.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  uib, uiblib, StdCtrls, ExtCtrls;
{$ELSE}
  libc, SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  uib, uiblib, QStdCtrls, QExtCtrls;
{$ENDIF}

type
  TUIBTransactionEditForm = class(TForm)
    CommonBox: TComboBox;
    Label1: TLabel;
    OK: TButton;
    Cancel: TButton;
    OptionPanel: TPanel;
    ChkConsistency: TCheckBox;
    ChkConcurrency: TCheckBox;
    ChkShared: TCheckBox;
    ChkProtected: TCheckBox;
    ChkExclusive: TCheckBox;
    ChkWait: TCheckBox;
    ChkNowait: TCheckBox;
    ChkRead: TCheckBox;
    ChkWrite: TCheckBox;
    ChkLockRead: TCheckBox;
    ChkLockWrite: TCheckBox;
    ChkVerbTime: TCheckBox;
    ChkCommitTime: TCheckBox;
    ChkIgnoreLimbo: TCheckBox;
    ChkReadCommitted: TCheckBox;
    ChkAutoCommit: TCheckBox;
    ChkRecVersion: TCheckBox;
    ChkNoRecVersion: TCheckBox;
    ChkRestartRequests: TCheckBox;
    ChkNoAutoUndo: TCheckBox;
    LockReadTables: TEdit;
    LockWriteTable: TEdit;
    ChkLockTimeOut: TCheckBox;
    LockTimeoutValue: TEdit;
    procedure ChkOptionClick(Sender: TObject);
    procedure CommonBoxChange(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FOptions: TTransParams;
    FTransaction: TUIBTransaction;
    procedure SetTransaction(Value: TUIBTransaction);
    procedure SetOptions(Opt: TTransParams);
    procedure UpdateCommon;
  public
    { Public declarations }
    property Transaction: TUIBTransaction read FTransaction write SetTransaction;
  end;

var
  UIBTransactionEditForm: TUIBTransactionEditForm;

implementation

const
  TRDefault                 : TTransParams = [tpConcurrency,tpWait,tpWrite];
  TRSnapShot                : TTransParams = [tpConcurrency, tpNowait];
  TRReadCommitted           : TTransParams = [tpReadCommitted, tpRecVersion, tpNowait];
  TRReadOnlyTableStability  : TTransParams = [tpRead, tpConsistency];
  TRReadWriteTableStability : TTransParams = [tpWrite, tpConsistency];

{$IFDEF UNIX}
{$R *.xfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TUIBTransactionEditForm.ChkOptionClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
  case Checked of
    True  : Include(FOptions, TTransParam(Tag));
    False : Exclude(FOptions, TTransParam(Tag));
  end;
  if Sender = ChkLockRead then
    LockReadTables.Enabled := TCheckBox(Sender).Checked else
  if Sender = ChkLockWrite then
    LockWriteTable.Enabled := TCheckBox(Sender).Checked;

  UpdateCommon;
end;

procedure TUIBTransactionEditForm.SetOptions(Opt: TTransParams);
var i: Integer;
begin
  FOptions := Opt;
  with OptionPanel do
  for i := 0 to ControlCount - 1 do
    if (Controls[i] is TCheckBox) and TCheckBox(Controls[i]).Visible then
      TCheckBox(Controls[i]).Checked :=
        TTransParam(TCheckBox(Controls[i]).Tag) in FOptions;
  LockReadTables.Enabled := ChkLockRead.Checked;
  LockWriteTable.Enabled := ChkLockWrite.Checked;
end;

procedure TUIBTransactionEditForm.SetTransaction(Value: TUIBTransaction);
begin
  FTransaction := Value;
  SetOptions(FTransaction.Options);
  LockReadTables.Text := Transaction.LockRead;
  LockWriteTable.Text := Transaction.LockWrite;
{$IFDEF FB20_UP}
  LockTimeoutValue.Text := inttostr(Transaction.LockTimeout);
{$ENDIF}
  UpdateCommon;
end;

procedure TUIBTransactionEditForm.UpdateCommon;
begin
  if FOptions = TRDefault then CommonBox.ItemIndex := 0 else
  if FOptions = TRSnapShot then CommonBox.ItemIndex := 1 else
  if FOptions = TRReadCommitted then CommonBox.ItemIndex := 2 else
  if FOptions = TRReadOnlyTableStability then CommonBox.ItemIndex := 3 else
  if FOptions = TRReadWriteTableStability then CommonBox.ItemIndex := 4 else
  CommonBox.ItemIndex := 5;
end;

procedure TUIBTransactionEditForm.CommonBoxChange(Sender: TObject);
begin
  case CommonBox.ItemIndex of
    0: SetOptions(TRDefault);
    1: SetOptions(TRSnapShot);
    2: SetOptions(TRReadCommitted);
    3: SetOptions(TRReadOnlyTableStability);
    4: SetOptions(TRReadWriteTableStability);
  end;
end;

procedure TUIBTransactionEditForm.OKClick(Sender: TObject);
begin
  Transaction.Options := FOptions;
  Transaction.LockRead := LockReadTables.Text;
  Transaction.LockWrite := LockWriteTable.Text;
{$IFDEF FB20_UP}
  Transaction.LockTimeout := StrToInt(LockTimeoutValue.Text);
{$ENDIF}
end;

procedure TUIBTransactionEditForm.FormCreate(Sender: TObject);
begin
{$IFDEF FB20_UP}
  LockTimeoutValue.Visible := true;
  ChkLockTimeOut.Visible := true;
{$ENDIF}
end;

end.




