unit TreeStructureMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, ComCtrls,
  JclContainerIntf;

type
  TForm1 = class(TForm)
    GroupBoxOptions: TGroupBox;
    ActionListMain: TActionList;
    ActionAllowDuplicates: TAction;
    ActionIgnoreDuplicates: TAction;
    ActionAllowDefault: TAction;
    ActionRemoveSingle: TAction;
    ActionCaseSensitive: TAction;
    ActionGenerateRandom: TAction;
    ActionAddNew: TAction;
    ActionRemoveSelected: TAction;
    TreeViewResults: TTreeView;
    GroupBoxActions: TGroupBox;
    ButtonGenerateRandom: TButton;
    ButtonRemoveSelected: TButton;
    EditNewItem: TEdit;
    ButtonAddNew: TButton;
    ActionAddNewChild: TAction;
    ButtonAddNewChild: TButton;
    CheckBoxBinaryTree: TCheckBox;
    ActionBinaryTree: TAction;
    ActionGeneralPurposeTree: TAction;
    CheckBoxGeneralPurposeTree: TCheckBox;
    CheckBoxCaseSensitive: TCheckBox;
    CheckBoxAllowDefault: TCheckBox;
    CheckBoxAllowDuplicates: TCheckBox;
    CheckBoxIgnoreDuplicates: TCheckBox;
    CheckBoxRemoveSingle: TCheckBox;
    ActionPack: TAction;
    Button1: TButton;
    ActionTestTree: TAction;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ActionGeneralPurposeTreeExecute(Sender: TObject);
    procedure ActionAlwaysEnabled(Sender: TObject);
    procedure ActionBinaryTreeExecute(Sender: TObject);
    procedure ActionCaseSensitiveExecute(Sender: TObject);
    procedure ActionRemoveSingleExecute(Sender: TObject);
    procedure ActionAllowDefaultExecute(Sender: TObject);
    procedure ActionDuplicatesExecute(Sender: TObject);
    procedure ActionIgnoreDuplicatesUpdate(Sender: TObject);
    procedure ActionGenerateRandomExecute(Sender: TObject);
    procedure ActionPackExecute(Sender: TObject);
    procedure ActionAddNewExecute(Sender: TObject);
    procedure ActionAddNewChildUpdate(Sender: TObject);
    procedure ActionAddNewChildExecute(Sender: TObject);
    procedure ActionRemoveSelectedExecute(Sender: TObject);
    procedure ActionRemoveSelectedUpdate(Sender: TObject);
    procedure ActionTestTreeExecute(Sender: TObject);
  private
    FTree: IJclWideStrTree;
    function GetSelectedIterator: IJclWideStrTreeIterator;
    procedure PrintTree;
  public
  end;

var
  Form1: TForm1;

implementation

uses
  JclBinaryTrees,
  JclTrees;

{$R *.dfm}

procedure TForm1.ActionBinaryTreeExecute(Sender: TObject);
begin
  if ActionBinaryTree.Checked then
  begin
    ActionGeneralPurposeTree.Checked := False;

    FTree := TJclWideStrBinaryTree.Create(nil);

    ActionCaseSensitiveExecute(ActionCaseSensitive);
    ActionRemoveSingleExecute(ActionRemoveSingle);
    ActionAllowDefaultExecute(ActionAllowDefault);
    ActionDuplicatesExecute(nil);
    PrintTree;
  end;
end;

procedure TForm1.ActionCaseSensitiveExecute(Sender: TObject);
begin
  FTree.CaseSensitive := (Sender as TAction).Checked;
end;

procedure TForm1.ActionGeneralPurposeTreeExecute(Sender: TObject);
begin
  if ActionGeneralPurposeTree.Checked then
  begin
    ActionBinaryTree.Checked := False;

    FTree := TJclWideStrTree.Create;

    ActionCaseSensitiveExecute(ActionCaseSensitive);
    ActionRemoveSingleExecute(ActionRemoveSingle);
    ActionAllowDefaultExecute(ActionAllowDefault);
    ActionDuplicatesExecute(nil);
    PrintTree;
  end;
end;

procedure TForm1.ActionGenerateRandomExecute(Sender: TObject);
  var
    CurrentItem: Integer;
  function GenerateItem: WideString;
  begin
    if FTree.Duplicates = dupAccept then
      Result := Format('Item %.3d', [Random(10)])
    else
    begin
      Result := Format('Item %.3d', [CurrentItem]);
      Inc(CurrentItem);
    end;
  end;

  procedure GenerateRandomChild(const AIterator: IJclWideStrIterator; Count: Integer);
  begin
    while Count > 0 do
    begin
      (AIterator as IJclWideStrTreeIterator).AddChild(GenerateItem);
      Dec(Count);
    end;
  end;

  procedure GenerateRandom(Count: Integer);
  begin
    while Count > 0 do
    begin
      FTree.Add(GenerateItem);
      Dec(Count);
    end;
  end;
var
  Index1, Index2: Integer;
  Iterator0, Iterator1, Iterator2: IJclWideStrTreeIterator;
begin
  CurrentItem := 0;
  FTree.Clear;

  if ActionGeneralPurposeTree.Checked then
  begin
    // general purpose tree
    GenerateRandom(5);
    Iterator0 := FTree.Root;
    for Index1 := 0 to Iterator0.ChildrenCount - 1 do
    begin
      Iterator1 := (Iterator0 as IJclIntfCloneable).IntfClone as IJclWideStrTreeIterator;
      Iterator1.GetChild(Index1);
      GenerateRandomChild(Iterator1, 5);
      for Index2 := 0 to Iterator1.ChildrenCount - 1 do
      begin
        Iterator2 := (Iterator1 as IJclIntfCloneable).IntfClone as IJclWideStrTreeIterator;
        Iterator2.GetChild(Index2);
        GenerateRandomChild(Iterator2, 5);
      end;
    end;
  end
  else
  begin
    // binary tree
    GenerateRandom(100);
  end;
  PrintTree;
end;

procedure TForm1.ActionIgnoreDuplicatesUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not CheckBoxAllowDuplicates.Checked;
end;

procedure TForm1.ActionPackExecute(Sender: TObject);
begin
  (FTree as IJclPackable).Pack;
  PrintTree;
end;

procedure TForm1.ActionRemoveSelectedExecute(Sender: TObject);
begin
  GetSelectedIterator.Remove;
  PrintTree;
end;

procedure TForm1.ActionRemoveSelectedUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := TreeViewResults.Selected <> nil;
end;

procedure TForm1.ActionRemoveSingleExecute(Sender: TObject);
begin
  FTree.RemoveSingleElement := (Sender as TAction).Checked;
end;

procedure TForm1.ActionTestTreeExecute(Sender: TObject);
  procedure CheckNode(const AIterator: IJclWideStrTreeIterator);
  var
    Index: Integer;
    ChildIterator, ParentIterator: IJclWideStrTreeIterator;
  begin
    for Index := 0 to AIterator.ChildrenCount - 1 do
    begin
      ChildIterator := (AIterator as IJclIntfCloneable).IntfClone as IJclWideStrTreeIterator;
      ChildIterator.GetChild(Index);

      try
        ParentIterator := (ChildIterator as IJclIntfCloneable).IntfClone as IJclWideStrTreeIterator;
        ParentIterator.Parent;

        if not AIterator.IteratorEquals(ParentIterator) then
          ShowMessage('difference at parent of node ' + string(ChildIterator.GetString));
      except
        ShowMessage('error at parent of node ' + string(ChildIterator.GetString));
      end;

      CheckNode(ChildIterator);
    end;
  end;
var
  ARootIterator: IJclWideStrTreeIterator;
begin
  ARootIterator := FTree.Root;
  ARootIterator.Next; // unlock
  CheckNode(ARootIterator);
  ShowMessage('end of test');
end;

procedure TForm1.ActionAddNewChildExecute(Sender: TObject);
begin
  if GetSelectedIterator.AddChild(EditNewItem.Text) then
    ShowMessage('Success')
  else
    ShowMessage('Duplicate');
  PrintTree;
end;

procedure TForm1.ActionAddNewChildUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ActionGeneralPurposeTree.Checked and (TreeViewResults.Selected <> nil);
end;

procedure TForm1.ActionAddNewExecute(Sender: TObject);
begin
  if FTree.Add(EditNewItem.Text) then
    ShowMessage('Success')
  else
    ShowMessage('Duplicate');
  PrintTree;
end;

procedure TForm1.ActionAllowDefaultExecute(Sender: TObject);
begin
  FTree.AllowDefaultElements := (Sender as TAction).Checked;
end;

procedure TForm1.ActionDuplicatesExecute(Sender: TObject);
begin
  if ActionAllowDuplicates.Checked then
    FTree.Duplicates := dupAccept
  else
  if ActionIgnoreDuplicates.Checked then
    FTree.Duplicates := dupIgnore
  else
    FTree.Duplicates := dupError;
end;

procedure TForm1.ActionAlwaysEnabled(Sender: TObject);
begin
  (Sender as TAction).Enabled := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  ActionGeneralPurposeTreeExecute(ActionGeneralPurposeTree);
end;

function TForm1.GetSelectedIterator: IJclWideStrTreeIterator;
var
  Indexes: array of Integer;
  I: Integer;
  ANode: TTreeNode;
begin
  Result := nil;
  ANode := TreeViewResults.Selected;
  if ANode <> nil then
  begin
    while ANode.Parent <> nil do
    begin
      SetLength(Indexes, Length(Indexes) + 1);
      Indexes[High(Indexes)] := ANode.Index;
      ANode := ANode.Parent;
    end;
    Result := FTree.Root;
    for I := High(Indexes) downto Low(Indexes) do
      Result.GetChild(Indexes[I]);
    Result.Next;
  end;
end;

procedure TForm1.PrintTree;
  procedure ProcessNode(const AIterator: IJclWideStrTreeIterator; ANode: TTreeNode);
  var
    Index: Integer;
    ChildIterator: IJclWideStrTreeIterator;
    ChildNode: TTreeNode;
  begin
    ANode.Text := string(AIterator.GetString);
    for Index := 0 to AIterator.ChildrenCount - 1 do
    begin
      ChildIterator := (AIterator as IJclIntfCloneable).IntfClone as IJclWideStrTreeIterator;
      ChildIterator.GetChild(Index);
      ChildNode := TreeViewResults.Items.AddChild(ANode, '');
      ProcessNode(ChildIterator, ChildNode);
    end;
  end;
var
  ARootIterator: IJclWideStrTreeIterator;
  ARootNode: TTreeNode;
begin
  TreeViewResults.Items.Clear;
  if FTree.Size > 0 then
  begin
    ARootIterator := FTree.Root;
    ARootIterator.Next; // unlock
    ARootNode := TreeViewResults.Items.Add(nil, '');
    ProcessNode(ARootIterator, ARootNode);
    ARootNode.Expand(True);
    ARootNode.MakeVisible;
  end;
end;

end.
