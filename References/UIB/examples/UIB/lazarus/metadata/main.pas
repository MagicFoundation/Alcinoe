unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, uibmetadata, StdCtrls, uib, uiblib, SynEdit,
  SynHighlighterSQL;

type

  { TMainForm }

  TMainForm = class(TForm)
    ImageList: TImageList;
    DataBase: TUIBDataBase;
    Memo: TSynEdit;
    mSave: TMenuItem;
    mLoad: TMenuItem;
    SaveDialog: TSaveDialog;
    SynSQLSyn1: TSynSQLSyn;
    Transaction: TUIBTransaction;
    MainMenu: TMainMenu;
    mOpen: TMenuItem;
    mFile: TMenuItem;
    OpenDialog: TOpenDialog;
    TreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mLoadClick(Sender: TObject);
    procedure mOpenClick(Sender: TObject);
    procedure mSaveClick(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowNodes(node: TMetaNode; from: TTreeNode);
  end; 

var
  MainForm: TMainForm;

implementation

var
  MetaData: TMetaDataBase;

{ TMainForm }

type
  TNodeInfo = record
    icon: Integer;
    color: TColor;
  end;

const
   NodeInfos: array[TMetaNodeType] of TNodeInfo = (
     (icon: 0;  color : clBlack),   // MetaNode,
     (icon: 1;  color : clBlack),   // MetaDatabase
     (icon: 7;  color : clRed),     // MetaException
     (icon: 6;  color : clMaroon),  // MetaGenerator
     (icon: 15; color : clBlack),   // MetaCheck
     (icon: 14; color : clBlack),   // MetaTrigger
     (icon: 8;  color : clBlue),    // MetaUDF
     (icon: 4;  color : clGreen),   // MetaView
     (icon: 5;  color : clTeal),    // MetaProcedure
     (icon: 9;  color : clBlack),   // MetaRole
     (icon: 3;  color : clGreen),   // MetaTable
     (icon: 10; color : clNavy),    // MetaBaseField
     (icon: 10; color : clNavy),    //   MetaUDFField
     (icon: 10; color : clNavy),    //   MetaField
     (icon: 10; color : clNavy),    //     MetaProcInField
     (icon: 10; color : clNavy),    //     MetaProcOutField
     (icon: 10; color : clNavy),    //     MetaTableField
     (icon: 2;  color : clOlive),   //       MetaDomain
     (icon: 0;  color : clBlack),   // MetaConstraint
     (icon: 12; color : clBlack),   //   MetaForeign
     (icon: 17;  color : clBlack),  //   MetaIndex
     (icon: 11; color : clBlack),   //   MetaPrimary
     (icon: 16; color : clBlack),   //   MetaUnique
     (icon: 9;  color : clBlack),   // MetaGrant
     (icon: 9;  color : clBlack),   //   MetaRoleGrant
     (icon: 9;  color : clBlack),   //   MetaTableGrant
     (icon: 9;  color : clBlack),   //   MetaFieldGrant
     (icon: 9;  color : clBlack),   //   MetaProcedureGrant
     (icon: 9;  color : clBlack),   // MetaGrantee
     (icon: 9;  color : clBlack),   //   MetaUserGrantee
     (icon: 9;  color : clBlack),   //   MetaRoleGrantee
     (icon: 9;  color : clBlack),   //   MetaProcedureGrantee
     (icon: 9;  color : clBlack),   //   MetaTriggerGrantee
     (icon: 9;  color : clBlack)    //   MetaViewGrantee
   );

procedure TMainForm.mOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    DataBase.Connected := false;
    TreeView.Items.Clear;
    DataBase.DatabaseName := OpenDialog.FileName;
    MetaData.Free;
    MetaData := TMetaDataBase.Create(nil, 0);
    MetaData.LoadFromDatabase(Transaction);
    Transaction.Commit;
    ShowNodes(MetaData, nil);
  end;
end;

procedure TMainForm.mSaveClick(Sender: TObject);
var FileStream: TFileStream;
begin
  if SaveDialog.Execute then
  begin
    FileStream := TFileStream.Create(SaveDialog.FileName, fmCreate);
    try
      MetaData.SaveToStream(FileStream);
    finally
      FileStream.Free;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DataBase.LibraryName := uiblib.GetClientLibrary;
  with Memo do
  begin
    Parent := Self;
    Align := alClient;
    ScrollBars := ssBoth;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  MetaData.Free;
end;

procedure TMainForm.mLoadClick(Sender: TObject);
var FileStream: TFileStream;
begin
  OpenDialog.FileName := SaveDialog.FileName;
  if OpenDialog.Execute then
  begin
    TreeView.Items.Clear;
    FileStream := TFileStream.Create(OpenDialog.FileName, fmOpenRead);
    try
      MetaData.Free;
      MetaData := TMetaDataBase.CreateFromStream(nil, -1, FileStream);
      ShowNodes(MetaData, nil);
    finally
      FileStream.Free;
    end;
  end;
end;

procedure TMainForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  If (node.Data <> nil) then
    memo.Lines.Text := TMetaNode(Node.Data).AsDDL else
    memo.Lines.Text := '';
end;

procedure TMainForm.ShowNodes(node: TMetaNode; from: TTreeNode);
var
  i, j: Integer;
  ClassNode: TTreeNode;
begin
  from := TreeView.Items.AddChild(from, node.Name);
  from.Data := Node;
  if (node.NodeType = MetaTableField) then
  with TMetaTableField(node) do
  begin
    if (FieldInfos >= [fiprimary, fiforeign]) then from.ImageIndex := 13 else
    if (FieldInfos >= [fiprimary]) then from.ImageIndex := NodeInfos[MetaPrimary].icon else
    if (FieldInfos >= [fiforeign]) then from.ImageIndex := NodeInfos[MetaForeign].icon else
      from.ImageIndex := NodeInfos[node.NodeType].icon;
  end else
    from.ImageIndex := NodeInfos[node.NodeType].icon;
  from.SelectedIndex := from.ImageIndex;
  for i := 0 to node.NodeCount - 1 do
    if node.Nodes[i].Childs.Count > 0 then
    begin
      ClassNode := TreeView.Items.AddChild(from, node.Nodes[i].ClassID.NodeClass + 's'
      + format(': %s(%d)', [node.Nodes[i].ClassID.ClassName, node.Nodes[i].Childs.Count]));
      ClassNode.ImageIndex := NodeInfos[node.Nodes[i].ClassID.NodeType].icon;
      ClassNode.SelectedIndex := ClassNode.ImageIndex;
      for j := 0 to node.Nodes[i].Childs.Count - 1 do
        ShowNodes(TMetaNode(node.Nodes[i].Childs[j]), ClassNode);
    end;
end;

initialization
  {$I main.lrs}

end.

