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

unit uibsqledit;

{$I uib.inc}

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, uib, uibmetadata, uibsynedit, ExtCtrls, ImgList, SynEdit,
  ToolWin, Grids, StdCtrls, uiblib, SynCompletionProposal, SynEditTypes;
{$ELSE}
  QForms, QSynCompletionProposal, QImgList, QStdCtrls, QExtCtrls, QGrids,
  QControls, QComCtrls, QSynEdit, Classes, uiblib, uib, uibmetadata,
  uibsynedit, SysUtils, libc, QSynEditTypes, SynCompletionProposal;
{$ENDIF}
type
  TUIBSQLEditForm = class(TForm)
    ToolBar: TToolBar;
    RunBt: TToolButton;
    BtFetch: TToolButton;
    BtCommit: TToolButton;
    BtRollBack: TToolButton;
    ButtonImages: TImageList;
    CompletionProposal: TSynCompletionProposal;
    ToolButton1: TToolButton;
    btRefresh: TToolButton;
    PageControl: TPageControl;
    SQLTab: TTabSheet;
    Editor: TSynEdit;
    TabDDL: TTabSheet;
    Splitter: TSplitter;
    DDLEditor: TSynEdit;
    TreeView: TTreeView;
    ResultPage: TTabSheet;
    ResultGrid: TStringGrid;
    Splitter1: TSplitter;
    Log: TSynEdit;
    Panel2: TPanel;
    BtOK: TButton;
    BtCancel: TButton;
    ParamProposal: TSynCompletionProposal;
    ImageList: TImageList;
    procedure RunBtClick(Sender: TObject);
    procedure BtFetchClick(Sender: TObject);
    procedure BtCommitClick(Sender: TObject);
    procedure BtRollBackClick(Sender: TObject);
    procedure BtOKClick(Sender: TObject);
    procedure EditorKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure btRefreshClick(Sender: TObject);
    function Run: TUIBStatementType;
    procedure AddSQLTokens;
    procedure CompletionProposalExecute(Kind: SynCompletionType;
      Sender: TObject; var AString: String; var x, y: Integer;
      var CanExecute: Boolean);
    procedure ParamProposalExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: String; var x, y: Integer;
      var CanExecute: Boolean);
  private
    FStatement: TUIBStatement;
    HaveConnection: boolean;
    InsertList, ItemList: TStringList;
    Query: TUIBQuery;
    SQLHighLighter: TUIBSQLHighliter;
    procedure ShowNodes(node: TMetaNode; from: TTreeNode);
    procedure SetStatement(const Value: TUIBStatement);
    procedure CheckTransaction;
  public
    property Statement: TUIBStatement Read FStatement write SetStatement;
  end;

var
  UIBSQLEditForm: TUIBSQLEditForm;

implementation

uses
  uibkeywords;

{$IFDEF MSWINDOWS}
  {$R *.dfm}
{$ELSE}
  {$R *.xfm}
{$ENDIF}

{$IFDEF UNIX}
function GetTickCount: Cardinal;
var
  tv: timeval;
begin
  gettimeofday(tv, nil);
  Result := int64(tv.tv_sec) * 1000 + tv.tv_usec div 1000;
end;
{$ENDIF}

{ TUIBSQLEditForm }

procedure TUIBSQLEditForm.FormCreate(Sender: TObject);
{$IFNDEF UNIX}
  // Yukio Makino (makino@freejpn.com)
  procedure FontChange;
  var
    ChangeFont: TFont;
    i: integer;
  begin
    //The default font of its own country is called.
    ChangeFont := TFont.Create;
    try
      //In Japanese, since the size of a default font was small,
      // size was changed.
      ChangeFont.Size := 10;
      for i := 0 to ComponentCount-1 do
      begin
       if Components[i] is TSynEdit then
         TSynEdit(Components[i]).Font.Assign(ChangeFont) else
         if Components[i] is TStringGrid then
           TStringGrid(Components[i]).Font.Assign(ChangeFont);
      end;
    finally
      ChangeFont.Free;
    end;
  end;
{$ENDIF}
begin
  HaveConnection := True;
  InsertList := TStringList.Create;
  ItemList := TStringList.Create;
  Query := TUIBQuery.Create(nil);
  SQLHighLighter := TUIBSQLHighliter.Create(Self);
  Editor.Highlighter := SQLHighLighter;
  DDLEditor.Highlighter := SQLHighLighter;
  //Editor.Font.Size := 12;
  //Editor.Font.Height := 13;

{$IFNDEF UNIX}
  if SysLocale.PriLangID = LANG_JAPANESE then
    FontChange;
{$ENDIF}
end;

procedure TUIBSQLEditForm.SetStatement(const Value: TUIBStatement);
procedure DisableEditors;
begin
  HaveConnection := False;
  ToolBar.Enabled := False;
  TabDDL.TabVisible := False;
end;
begin
  FStatement := Value;
  Query.Transaction := FStatement.Transaction;
  Editor.Lines.Assign(FStatement.SQL);
  try
    if (Statement.Transaction <> nil) and (Statement.Transaction.DataBase <> nil) then
      try
        // Metadata
        SQLHighLighter.BeginUpdate;
        ShowNodes(TMetaNode(Statement.Transaction.DataBase.GetMetaData), nil);
        SQLHighLighter.EndUpdate;
      except
        DisableEditors;
      end else
        DisableEditors;
  finally
    AddSQLTokens;
  end;
end;

procedure TUIBSQLEditForm.ShowNodes(node: TMetaNode; from: TTreeNode);
var
  i, j: Integer;
  ClassNode: TTreeNode;
begin
  if not (Node is TMetaDatabase) then
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

    if node.NodeType in [MetaException, MetaGenerator, MetaUDF, MetaView,
      MetaProcedure, MetaRole, MetaTable, MetaDomain, MetaTrigger] then
    begin
      InsertList.Add(node.Name);
      if (node.NodeType = MetaDomain) then
      ItemList.Add(format('\image{%d} \color{$%x} %s \color{clBlack} \style{+B}%s\style{-B} %s',
        [NodeInfos[node.NodeType].Icon, NodeInfos[node.NodeType].color, node.NodeClass, Node.Name,
         TMetaDomain(node).ShortFieldType])) else
      ItemList.Add(format('\image{%d} \color{$%x} %s \color{clBlack} \style{+B}%s\style{-B}%s',
        [NodeInfos[node.NodeType].Icon, NodeInfos[node.NodeType].color, node.NodeClass,
         Node.Name, GetParams(node)]));

      SQLHighLighter.AddMetaKeyWord(node.Name, NodeInfos[node.NodeType].token);

    end;
  end;

  for i := 0 to node.NodeCount - 1 do
    if node.Nodes[i].Childs.Count > 0 then
    begin
      ClassNode := TreeView.Items.AddChild(from, node.Nodes[i].ClassID.NodeClass + 's');
      ClassNode.ImageIndex := NodeInfos[node.Nodes[i].ClassID.NodeType].icon;
      ClassNode.SelectedIndex := ClassNode.ImageIndex;
      for j := 0 to node.Nodes[i].Childs.Count - 1 do
        ShowNodes(TMetaNode(node.Nodes[i].Childs[j]), ClassNode);
    end;
end;

procedure TUIBSQLEditForm.RunBtClick(Sender: TObject);
begin
  Run;
  ResultPage.TabVisible := False;
  PageControl.ActivePage := SQLTab;
  CheckTransaction;
end;

procedure TUIBSQLEditForm.BtFetchClick(Sender: TObject);
var
  Fetchime: Cardinal;
  i: Integer;
  st: TUIBStatementType;
  procedure Display(n: Integer);
  var j: Integer;
  begin
    ResultGrid.Cells[0, n] := Inttostr(n);
    for j := 1 to Query.Fields.FieldCount do
    begin
      if (Query.Fields.FieldType[j-1] = uftBlob) then ResultGrid.Cells[j, n] := '[blob]' else
      if Query.Fields.IsNull[j-1] then ResultGrid.Cells[j, n] := 'null' else
      ResultGrid.Cells[j, n] := Query.Fields.AsString[j-1];
    end;
  end;
begin
  st := Run;
  if (not (st in [stSelect, stExecProcedure]))
    then exit;

  if (st = stSelect) then
  begin                     
    Fetchime := GetTickCount;
    Query.FetchAll;
    if (Query.Fields.RecordCount = 0) then Exit;
    Log.Lines.Add(format('Fetch: %d ms.', [GetTickCount - Fetchime]));
  end;

  if (st = stExecProcedure) and (Query.Fields.FieldCount = 0) then
    Exit;

  ResultGrid.ColCount := Query.Fields.FieldCount + 1;
  if (st = stExecProcedure) then
    ResultGrid.RowCount := 2 else
    ResultGrid.RowCount := Query.Fields.RecordCount + 1;

  for i := 1 to Query.Fields.FieldCount do
    ResultGrid.Cells[i, 0] := Query.Fields.AliasName[i-1];

  if (st = stExecProcedure) then
    Display(1) else
  for i := 1 to Query.Fields.RecordCount do
  begin
    Query.Fields.CurrentRecord := i - 1;
    Display(i);
  end;

  ResultPage.TabVisible := True;
  PageControl.ActivePage := ResultPage;
  CheckTransaction;
end;

procedure TUIBSQLEditForm.BtCommitClick(Sender: TObject);
begin
  Query.Transaction.Commit;
  CheckTransaction;
end;

procedure TUIBSQLEditForm.BtRollBackClick(Sender: TObject);
begin
  Query.Transaction.RollBack;
  CheckTransaction;
end;

procedure TUIBSQLEditForm.BtOKClick(Sender: TObject);
begin
  Statement.SQL.Assign(Editor.Lines);
end;

procedure TUIBSQLEditForm.CheckTransaction;
begin
  BtCommit.Enabled := Statement.Transaction.InTransaction;
  BtRollBack.Enabled := BtCommit.Enabled;
end;

procedure TUIBSQLEditForm.EditorKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = [ssShift]) and (Key = 120) then BtFetch.Click else
  if (Shift = []) and (Key = 120) then RunBt.Click;
end;

procedure TUIBSQLEditForm.FormDestroy(Sender: TObject);
begin
  InsertList.Free;
  ItemList.Free;
  Query.Free;
end;

procedure TUIBSQLEditForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  if (node.Data <> nil) then
    DDLEditor.Text := TMetaNode(node.Data).AsDDL else
    DDLEditor.Lines.Clear;
end;

procedure TUIBSQLEditForm.btRefreshClick(Sender: TObject);
begin
  TreeView.Items.Clear;
  ItemList.Clear;
  InsertList.Clear;
  DDLEditor.Lines.Clear;
  SQLHighLighter.BeginUpdate;
  ShowNodes(TMetaNode(Statement.Transaction.Database.GetMetadata(True)), nil);
  SQLHighLighter.EnUpdate;
  AddSQLTokens;
end;

function TUIBSQLEditForm.Run: TUIBStatementType;
var
  PrepTime, ExecTime: cardinal;
begin
  result := TUIBStatementType(-1);
  Log.Lines.Clear;
  try
    Query.SQL.Assign(Editor.Lines);
    Query.Transaction.StartTransaction;
    PrepTime := GetTickCount;
    query.Prepare;
    PrepTime := GetTickCount - PrepTime;
    ExecTime := GetTickCount;
    Query.Execute;
    ExecTime := GetTickCount - ExecTime;
  except
    on E: EUIBError do
      begin
        log.Highlighter := nil;
        log.Lines.Text := E.Message;
        CheckTransaction;
        exit;
      end;
    else
      raise;
  end;

  Log.Highlighter := SQLHighLighter;
  Result := Query.StatementType;
  if Result = stSelect then
    Log.Lines.Add(Query.Plan);
  Log.Lines.Add(format('Prepare: %d ms.', [PrepTime]));
  Log.Lines.Add(format('Execute: %d ms.', [ExecTime]));
end;

procedure TUIBSQLEditForm.AddSQLTokens;
var
  i: Integer;
  c: TCharacterSet;
begin
  for i := low(SQLToKens) to high(SQLToKens) do
  begin
    InsertList.Add(SQLToKens[i]);
    ItemList.Add(format(SQLITEMSTR, [SQLToKens[i]]));
  end;
  for c := low(TCharacterSet) to high(TCharacterSet) do
  begin
    InsertList.Add(CharacterSetStr[c]);
    ItemList.Add(format(CHARSETITEMSTR, [CharacterSetStr[c]]));
  end;
end;

procedure TUIBSQLEditForm.CompletionProposalExecute(
  Kind: SynCompletionType; Sender: TObject; var AString: String; var x,
  y: Integer; var CanExecute: Boolean);
  function FindTable: boolean;
  var
    table, s: string;
    start, stop: Integer;
    function GetMetaTable: boolean;
    var
      Meta: TMetaDataBase;
      i, j : Integer;
      icon: Integer;
      function ForeignStr(node: TMetaTableField): string;
      var i, j: Integer;
      begin
        for i := 0 to TMetaTable(Node.Parent).ForeignCount - 1 do
          for j := 0 to TMetaTable(Node.Parent).Foreign[i].FieldsCount - 1 do
          if TMetaTable(Node.Parent).Foreign[i].Fields[j] = Node then
            with TMetaTable(Node.Parent).Foreign[i] do
            begin
              result := format(' \image{11} \color{$%x}\style{+B}%s\style{-B}\color{clBlack}(%s)',
                [NodeInfos[TMetaTable.NodeType].color, ForTable.Name, ForFields[j].Name]);
              Exit;
            end;
      end;
    begin
      Result := False;
      Meta := TMetaDataBase(Statement.Transaction.DataBase.GetMetadata(false));

      CompletionProposal.ClearList;

      // TABLES
      for i := 0 to Meta.TablesCount - 1 do
        if CompareText(Meta.Tables[i].Name, table) = 0 then
        begin
          for j := 0 to Meta.Tables[i].FieldsCount - 1 do
            with Meta.Tables[i].Fields[j] do
            begin
              if (fiprimary in FieldInfos) then
              begin
                if (fiforeign in FieldInfos) then
                  icon := 13 else
                  icon := 11;
              end else
              if (fiforeign in FieldInfos) then
                icon := 12 else
                icon := NodeInfos[NodeType].Icon;

              if (icon in [12, 13]) then
              CompletionProposal.AddItem(format(FIELDSFORMAT,
               [Icon, Name, LowerCase(ShortFieldType)+
               ForeignStr(Meta.Tables[i].Fields[j])]), Name ) else
              CompletionProposal.AddItem(format(FIELDSFORMAT,
               [Icon, Name, LowerCase(ShortFieldType)]), Name);
            end;
          result := true;
          exit;
        end;

      // PROCEDURES
      for i := 0 to Meta.ProceduresCount - 1 do
        if CompareText(Meta.Procedures[i].Name, table) = 0 then
        begin
          for j := 0 to Meta.Procedures[i].OutputFieldsCount - 1 do
            with Meta.Procedures[i].OutputFields[j] do
             CompletionProposal.AddItem(format(FIELDSFORMAT,
               [NodeInfos[NodeType].Icon, Name, LowerCase(ShortFieldType)]), Name);
          result := true;
          exit;
        end;

      // VIEWS
      for i := 0 to Meta.ViewsCount - 1 do
        if CompareText(Meta.Views[i].Name, table) = 0 then
        begin
          for j := 0 to Meta.Views[i].FieldsCount - 1 do
            with Meta.Views[i].Fields[j] do
             CompletionProposal.AddItem(format(FIELDSFORMAT,
               [NodeInfos[NodeType].Icon, Name, LowerCase(ShortFieldType)]), Name);
          result := true;
          exit;
        end;

    end;
  var c: Integer;
  begin
    result := false;
    s := Editor.Text;
    if s = '' then exit;
    Stop := 0;
    for c := 0 to Editor.CaretY - 2 do
      stop := stop + Length(Editor.Lines[c]) + 2; // crlf
    Stop := Stop + Editor.CaretX - 1;
    if (Editor.CaretX > 1) then
      while (Stop > 0) do
      begin
        if (s[Stop] = '.') then
        begin
          Start := Stop;
          while (Start > 1) and (s[Start-1] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '$']) do
            dec(start);
          if (Start <> Stop) then
          begin
            // i've found a table name
            table := copy(s, start, stop - start);
            if GetMetaTable then
            begin
              Result := True;
              Exit;
            end;
            // table name not found in metadata, perhaps it is an alias ...
            if AliasToTable(s, table, table) and GetMetaTable then
            begin
              Result := True;
              Exit;
            end;
          end;
          exit;
        end else
        if (s[Stop] > #32) then
          dec(Stop) else
          Exit;
      end;
  end;
begin
  if not (HaveConnection and FindTable) then
  begin
    CompletionProposal.ItemList.Assign(ItemList);
    CompletionProposal.InsertList.Assign(InsertList);
  end;
end;

procedure TUIBSQLEditForm.ParamProposalExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: String; var x, y: Integer;
  var CanExecute: Boolean);
  function FindTable: boolean;
  var
    locline, lookup: String;
    TmpX, savepos, StartX,
    ParenCounter,
    TmpLocation: Integer;
    FoundMatch: Boolean;
    c: Integer;
    function GetMetaTable: boolean;
    var
      Meta: TMetaDataBase;
      i, j, k : Integer;
      proposal: string;
    begin
      Result := False;
      Meta := TMetaDataBase(Statement.Transaction.DataBase.GetMetadata(false));

      ParamProposal.ClearList;

      // PROCEDURES
      for i := 0 to Meta.ProceduresCount - 1 do
        if CompareText(Meta.Procedures[i].Name, lookup) = 0 then
        begin
          for j := 0 to Meta.Procedures[i].InputFieldsCount - 1 do
            with Meta.Procedures[i].InputFields[j] do
            begin
              if j = 0 then
                proposal := format('"%s: %s', [Name, LowerCase(ShortFieldType)]) else
                proposal := proposal + format(', "%s: %s', [Name, LowerCase(ShortFieldType)]);
              if j = Meta.Procedures[i].InputFieldsCount - 1 then
                proposal := proposal + '"' else
                proposal := proposal + ',"';
            end;

          ParamProposal.ItemList.Text := proposal;
          result := true;
          exit;
        end;

      // UDFS
      for i := 0 to Meta.UDFSCount - 1 do
        if CompareText(Meta.UDFS[i].Name, lookup) = 0 then
        begin
          if Meta.UDFS[i].Return = 0 then k := 1 else k := 0;
          for j := k to Meta.UDFS[i].FieldsCount - 1 do
            with Meta.UDFS[i].Fields[j] do
            begin
              if (j = k) then
                proposal := format('"%s', [LowerCase(ShortFieldType)]) else
                proposal := proposal + format(', "%s', [LowerCase(ShortFieldType)]);
              if j = Meta.UDFS[i].FieldsCount - 1 then
                proposal := proposal + '"' else
                proposal := proposal + ',"';
            end;
          ParamProposal.ItemList.Text := proposal;
          result := true;
          exit;
        end;

      // SQL
      for i := 0 to High(SQLComp) do
        if CompareText(SQLComp[i].Token, lookup) = 0 then
        begin
          ParamProposal.ItemList.Text := SQLComp[i].Comp;
          result := true;
          exit;
        end;

    end;
  begin
    result := false;
    locline := Editor.Text;
    if locline = '' then exit;
    TmpX := 0;
    for c := 0 to Editor.CaretY - 2 do
      TmpX := TmpX + Length(Editor.Lines[c]) + 2; // crlf
    TmpX := TmpX + Editor.CaretX - 1;

    FoundMatch := False;
    TmpLocation := 0;

    if (Editor.CaretX > 1) then
      while (TmpX > 0) and not(FoundMatch) do
      begin
        if LocLine[TmpX] = ',' then
        begin
          inc(TmpLocation);
          dec(TmpX);
        end else if LocLine[TmpX] = ')' then
        begin
          //We found a close, go till it's opening paren
          ParenCounter := 1;
          dec(TmpX);
          while (TmpX > 0) and (ParenCounter > 0) do
          begin
            if LocLine[TmpX] = ')' then inc(ParenCounter)
            else if LocLine[TmpX] = '(' then dec(ParenCounter);
            dec(TmpX);
          end;
          if TmpX > 0 then dec(TmpX);  //eat the open paren
        end else if locLine[TmpX] = '(' then
        begin
          //we have a valid open paren, lets see what the word before it is
          StartX := TmpX;
          while (TmpX > 0) and not(locLine[TmpX] in TSynValidStringChars) do
            Dec(TmpX);
          if TmpX > 0 then
          begin
            SavePos := TmpX;
            While (TmpX > 0) and (locLine[TmpX] in TSynValidStringChars) do
              dec(TmpX);
            inc(TmpX);
            lookup := Copy(LocLine, TmpX, SavePos - TmpX + 1);
            FoundMatch := GetMetaTable; // LookupList.IndexOf(Lookup) > -1;
            if not(FoundMatch) then
            begin
              TmpX := StartX;
              dec(TmpX);
            end;
          end;
        end else dec(TmpX);

      CanExecute := FoundMatch;
      if CanExecute then
         ParamProposal.Form.CurrentIndex := TmpLocation;
    end;
  end;
begin
  if HaveConnection then
    FindTable;
end;

end.



