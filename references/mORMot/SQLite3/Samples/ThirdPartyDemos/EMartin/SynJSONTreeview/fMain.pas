unit fMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, ActnList, StdActns, ImgList, Menus,
  ComCtrls,
  SynJSONTreeView,
  SynCommons;

type
  TfrmJSONEditor = class(TForm)
    Panel1: TPanel;
    ActionList1: TActionList;
    ImageList1: TImageList;
    EditPaste1: TEditPaste;
    EditClear1: TEditDelete;
    BitBtn1: TBitBtn;
    btnClear: TBitBtn;
    ActionToggleVisibleChildrenCounts: TAction;
    ActionToggleVisibleByteSizes: TAction;
    PopupMenu1: TPopupMenu;
    VisibleChildrenCounts1: TMenuItem;
    VisibleByteSizes1: TMenuItem;
    pnlJSONTreeView: TPanel;
    btnSaveJSON: TBitBtn;
    EditSave1: TAction;
    dlgSave: TSaveDialog;
    actEditLoad1: TAction;
    btnLoad: TBitBtn;
    dlgOpenJSONFile: TOpenDialog;
    procedure ActionToggleVisibleChildrenCountsExecute(Sender: TObject);
    procedure ActionToggleVisibleByteSizesExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditPaste1Execute(Sender: TObject);
    procedure EditClear1Execute(Sender: TObject);
    procedure EditSave1Execute(Sender: TObject);
    procedure actEditLoad1Execute(Sender: TObject);
  private
    { Private declarations }
    JSONTreeView1: TSynJSONTreeView;
    procedure DoOnCustomInput(Sender: TObject; Node: TSynJSONTreeNode;
                              var Prompt: RawUTF8; var Value: Variant; var Handled: Boolean);
    procedure SetJSON(const aJSON: RawUTF8);
    procedure ToogleButtons;
  public
    { Public declarations }
  end;

var
  frmJSONEditor: TfrmJSONEditor;

implementation

uses
  Clipbrd,
  fLevel;

{$R *.dfm}

procedure TfrmJSONEditor.FormCreate(Sender: TObject);
begin
  JSONTreeView1 := TSynJSONTreeView.Create(pnlJSONTreeView);
  JSONTreeView1.PopupMenu := PopupMenu1; 
  JSONTreeView1.Align := alClient;
  JSONTreeView1.Parent := pnlJSONTreeView;
  JSONTreeView1.OnCustomInput := DoOnCustomInput;
  ActionToggleVisibleByteSizes.Checked := JSONTreeView1.VisibleByteSizes;
  ActionToggleVisibleChildrenCounts.Checked := JSONTreeView1.VisibleChildrenCounts;
end;

procedure TfrmJSONEditor.ActionToggleVisibleByteSizesExecute(Sender: TObject);
begin
  JSONTreeView1.VisibleByteSizes := not JSONTreeView1.VisibleByteSizes;
end;

procedure TfrmJSONEditor.ActionToggleVisibleChildrenCountsExecute(Sender: TObject);
begin
  JSONTreeView1.VisibleChildrenCounts := not JSONTreeView1.VisibleChildrenCounts;
end;

procedure TfrmJSONEditor.EditClear1Execute(Sender: TObject);
begin
  JSONTreeView1.ClearAll;
  ToogleButtons;
end;

procedure TfrmJSONEditor.EditPaste1Execute(Sender: TObject);
begin
  SetJSON(Clipboard.AsText);
  ToogleButtons;
end;

procedure TfrmJSONEditor.EditSave1Execute(Sender: TObject);
begin
  if dlgSave.Execute then
    JSONTreeView1.SaveToFile(dlgSave.FileName);
end;

procedure TfrmJSONEditor.actEditLoad1Execute(Sender: TObject);
begin
  if dlgOpenJSONFile.Execute then
  begin
    SetJSON(StringFromFile(dlgOpenJSONFile.FileName));
    ToogleButtons;
  end;
end;

procedure TfrmJSONEditor.SetJSON(const aJSON: RawUTF8);
begin
  JSONTreeView1.JsonText := aJSON;

  JSONTreeView1.Items.BeginUpdate;
  JSONTreeView1.LoadJSON;
  JSONTreeView1.Items.EndUpdate;
end;

procedure TfrmJSONEditor.ToogleButtons;
begin
  EditSave1.Enabled := (JSONTreeView1.Items.Count > 0);
  EditClear1.Enabled := (JSONTreeView1.Items.Count > 0);
end;

procedure TfrmJSONEditor.DoOnCustomInput(Sender: TObject;
  Node: TSynJSONTreeNode; var Prompt: RawUTF8; var Value: Variant; var Handled: Boolean);

  procedure ProcessLevel;
  begin
    if (TDocVariantData(Value).Kind <> dvArray) then
    begin
      MessageDlg('Level node is not an array', mtError, [mbOK], 0);
      Exit;
    end;

    with TFrmLevel.Create(nil) do
    begin
      Caption := Prompt;
      FillListBox(Value);
      ShowModal;
      if (ModalResult = mrOK) then
      begin
        TDocVariantData(Value).Clear;
        TDocVariantData(Value).InitJSON(ToJSON);
      end;
      Handled := True;
    end;
  end;

begin
  if IdemPChar(pointer(Prompt), 'LEVEL') then
    ProcessLevel;
end;

end.
