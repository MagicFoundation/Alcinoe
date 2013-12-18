(* The contents of this file are subject to the Mozilla Public License
   Version 1.1 (the "License"); you may not use this file except in compliance
   with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
   Software distributed under the License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.
*)

unit main;

interface

uses
  Windows, SysUtils, Controls, Forms, ThreadAppLoader, SharedHook, AVL,
  VirtualTrees, SynEdit, SynEditHighlighter, SynHighlighterSQL, Menus,
  Classes, StdCtrls, ExtCtrls, MadCodeHook, ImgList, ComCtrls, ToolWin,
  ActnList, Buttons, Messages, TrayIcon, Dialogs;

type

  PHandleNode = ^THandleNode;
  THandleNode = record
    Handle: Integer;
    Node: PVirtualNode;
  end;

  TQueryNode = class
  private
    index: integer;
    DateTime: TDateTime;
    FirstTimeStamp: Int64;
    LastTimeStamp: Int64;
    isactive: boolean;
    sql: string;
    plan: string;
    preparetime: Int64;
    executetime: Int64;
    executecount: Integer;
    fetchtime: int64;
    //fetchcount: integer;
    queryparams: TStringList;
    affected: integer;
  public
    procedure DisplayQueryInfo(lst: TStrings);
    constructor Create; virtual;
    destructor Destroy; override;
  end;


  THandleList = class(TIndexedList)
  protected
    function doCompare(const item1, item2: Pointer): Integer; override;
    procedure doDelete(const item: Pointer); override;
  public
    procedure AddHandle(Handle: Integer; Node: PVirtualNode);
    function FindNode(handle: Integer): PVirtualNode;
    procedure RemoveHandle(handle: Integer);
    procedure RemoveNode(node: PVirtualNode);
  end;

  TMainForm = class(TForm)
    VirtualTree: TVirtualStringTree;
    Editor: TSynEdit;
    Panel1: TPanel;
    Button1: TButton;
    EditAppPath: TEdit;
    OpenDialog: TOpenDialog;
    SynSQLSyn1: TSynSQLSyn;
    Splitter1: TSplitter;
    MainMenu: TMainMenu;
    mFile: TMenuItem;
    mExit: TMenuItem;
    mOptions: TMenuItem;
    mStayontop: TMenuItem;
    mClear: TMenuItem;
    EditLibrary: TEdit;
    Button2: TButton;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ImageList: TImageList;
    ActionList: TActionList;
    acClear: TAction;
    acCopy: TAction;
    ToolButton2: TToolButton;
    Copy1: TMenuItem;
    PopupMenu: TPopupMenu;
    Clear1: TMenuItem;
    Copy2: TMenuItem;
    acSelectAll: TAction;
    Selectall1: TMenuItem;
    Selectall2: TMenuItem;
    acSaveToFile: TAction;
    Savetofile1: TMenuItem;
    SaveDialog: TSaveDialog;
    ToolButton3: TToolButton;
    View1: TMenuItem;
    acPause: TAction;
    ToolButton4: TToolButton;
    acRun: TAction;
    acDelete: TAction;
    ToolButton5: TToolButton;
    Deleteselected1: TMenuItem;
    ToolButton6: TToolButton;
    acStayOnTop: TAction;
    ToolButton7: TToolButton;
    SpeedButton1: TSpeedButton;
    Deleteselected2: TMenuItem;
    SysTrayMenu: TPopupMenu;
    Clear2: TMenuItem;
    Pause1: TMenuItem;
    Savetofile2: TMenuItem;
    acExit: TAction;
    Exit1: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure VirtualTreeFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VirtualTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure VirtualTreeFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure VirtualTreeChecking(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure VirtualTreeCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VirtualTreeHeaderClick(Sender: TVTHeader;
      Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure Button2Click(Sender: TObject);
    procedure VirtualTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure acClearExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acSelectAllExecute(Sender: TObject);
    procedure acSaveToFileExecute(Sender: TObject);
    procedure acPauseExecute(Sender: TObject);
    procedure acRunExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acStayOnTopExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
  private
    { Déclarations privées }
    FEventList: TThreadList;
    FSTList: THandleList;
    FFrequency: Int64;
    //FIconData: TNotifyIconData;
    procedure DisplayQueries(lst: TStrings; selected: boolean);
    procedure LoadParams;
    procedure SaveParams;
    procedure SysTrayDblClick(sender: TObject);
  protected
    procedure WndProc(var MsgRec: TMessage); override;
  public
    { Déclarations publiques }
    procedure AddMethodEvent(obj: TObject);
    procedure ReceiveEvent;
    procedure ProcessStreamEvent(Stream: TStreamMethod);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;
  NumberGenerator: Integer = 0;

implementation
uses uibase, uibLib, Clipbrd, inifiles, SynEditKeyCmds;

const
  CONFIGSECTION = 'CONFIG';

{$R *.dfm}

procedure AddLines(lst: TStrings; const Value: string);
var
  P, Start: PChar;
  S: string;
begin
  lst.BeginUpdate;
  try
    P := Pointer(Value);
    if P <> nil then
      while P^ <> #0 do
      begin
        Start := P;
        while not (P^ in [#0, #10, #13]) do Inc(P);
        SetString(S, Start, P - Start);
        lst.Add(S);
        if P^ = #13 then Inc(P);
        if P^ = #10 then Inc(P);
      end;
  finally
    lst.EndUpdate;
  end;
end;

procedure AddParams(lst: TStrings; const Value: string);
var
  P, Start: PChar;
  S: string;
  index: integer;
begin
  index := 0;
  lst.BeginUpdate;
  try
    P := Pointer(Value);
    if P <> nil then
      while P^ <> #0 do
      begin
        Start := P;
        while not (P^ in [#0, #10, #13]) do Inc(P);
        SetString(S, Start, P - Start);
        lst.Add(format('PARAM_%d = %s', [index, S]));
        inc(index);
        if P^ = #13 then Inc(P);
        if P^ = #10 then Inc(P);
      end;
  finally
    lst.EndUpdate;
  end;
end;

function StripLines(const Value: string): string;
var
  P, Start: PChar;
  S: string;
begin
  Result := '';
  P := Pointer(Value);
  if P <> nil then
    while P^ <> #0 do
    begin
      Start := P;
      while not (P^ in [#0, #10, #13]) do Inc(P);
      SetString(S, Start, P - Start);
      Result := result + S + ' ';
      if P^ = #13 then Inc(P);
      if P^ = #10 then Inc(P);
    end;
end;

procedure TMainForm.DisplayQueries(lst: TStrings; selected: boolean);
var
  node: PVirtualNode;
  qr: TQueryNode;
begin
  node := VirtualTree.RootNode.FirstChild;
  while node <> nil do
  begin
    if not selected or (vsSelected in Node.States) then
    begin
      qr := TQueryNode(VirtualTree.GetNodeData(node)^);
      qr.DisplayQueryInfo(lst);
    end;
    node := node.NextSibling;
  end;
end;

{ THandleList }

procedure THandleList.AddHandle(Handle: Integer; Node: PVirtualNode);
var
  Item: PHandleNode;
begin
  GetMem(Item, sizeof(item^));
  Item.Handle := Handle;
  Item.Node := Node;
  node.CheckType := ctCheckBox;
  node.CheckState := csCheckedNormal;
  Add(Item, true);
end;

function THandleList.doCompare(const item1, item2: Pointer): Integer;
begin
  Result := PHandleNode(item2).Handle - PHandleNode(item1).Handle;
end;

procedure THandleList.doDelete(const item: Pointer);
var
  node: PVirtualNode;
begin
  node := PHandleNode(item).Node;
  node.CheckState := csUncheckedNormal;
  MainForm.VirtualTree.InvalidateNode(node);
  FreeMem(item);
end;

function THandleList.FindNode(handle: Integer): PVirtualNode;
var
  p: PHandleNode;
begin
  p := Find(@handle);
  if (p <> nil) then
    Result := p.Node else
    Result := nil;
end;

procedure THandleList.RemoveHandle(handle: Integer);
begin
  Delete(@handle);
end;

procedure THandleList.RemoveNode(node: PVirtualNode);
var
  p: PHandleNode;
begin
  p := First;
  while p <> nil do
  begin
    if p.Node = node then
    begin
      Delete(p);
      Break;
    end;
    p := Next;
  end;
end;

{ TQueryNode }

constructor TQueryNode.Create;
begin
  DateTime := 0.0;
  FirstTimeStamp := 0;
  LastTimeStamp := 0;
  isactive := true;
  sql := '';
  plan := '';
  preparetime := 0;
  executetime := 0;
  executecount := 0;
  fetchtime := 0;
  //fetchcount := 0;
  queryparams := TStringList.Create;
  affected := 0;
end;

destructor TQueryNode.Destroy;
begin
  queryparams.Free;
  inherited;
end;

procedure TQueryNode.DisplayQueryInfo(lst: TStrings);
var i: integer;
begin
  lst.Add('/* SQL */');
  AddLines(lst, sql);
  lst.Add('');
  lst.Add('/* PLAN */');
  AddLines(lst, plan);
  lst.Add('');
  for i := 0 to queryparams.Count - 1 do
  begin
    lst.Add('/* EXECUTE PARAMETERS */');
    AddParams(lst, queryparams[i]);
    lst.Add('');
  end;
end;

{ TMainForm }

procedure TMainForm.AddMethodEvent(obj: TObject);
begin
  FEventList.Add(obj);
  TThread.Synchronize(nil, ReceiveEvent);
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  LoadParams;
  FEventList := TThreadList.Create;
  FSTList := THandleList.Create(true);
  QueryPerformanceFrequency(FFrequency);
  if ParamCount > 0 then
  begin
    EditAppPath.Text := ParamStr(1);
    if ParamCount > 1 then
      EditLibrary.Text := ParamStr(2);
  end;

  if EditAppPath.Text = '' then
    EditAppPath.Text := '< select your exe name here >';

  with TTrayIcon.Create(self) do
  begin
    Icon := Application.Icon;
    PopupMenu := SysTrayMenu;
    OnDblClick := SysTrayDblClick;
    ToolTip := Caption;
    Active := true;
  end;
end;

destructor TMainForm.Destroy;
var
  list: TList;
begin
  list := FEventList.LockList;
  try
    while list.Count > 0 do
    begin
      TObject(list[0]).Free;
      list.Delete(0);
    end;
  finally
    FEventList.UnlockList;
  end;
  FEventList.Free;
  FSTList.Free;
  SaveParams;
  inherited;
end;

procedure TMainForm.ReceiveEvent;
var
  list: TList;
  stream: TStreamMethod;
begin
  list := FEventList.LockList;
  try
    if list.Count > 0 then
    begin
      stream := list[0];
      list.Delete(0);
      ProcessStreamEvent(stream);
    end else
      Exit;
  finally
    FEventList.UnlockList;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
    EditAppPath.Text := OpenDialog.FileName;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  if OpenDialog.Execute then
    EditLibrary.Text := OpenDialog.FileName;
end;

procedure TMainForm.ProcessStreamEvent(Stream: TStreamMethod);
var
  header: THookedMethodHeader;
  str: string;
  qr: TQueryNode;
  node: PVirtualNode;
  procedure UpdateNode;
  begin
    with VirtualTree do
    begin
      if (Header.SortColumn <= 0) then
        ScrollIntoView(node, false);
      InvalidateNode(node);
    end;
  end;

begin
  try
    Stream.ReadHeader(header);
    case header.methodid of
      hm_isc_dsql_allocate_statement:
        if (not acPause.Checked) and (not IsStatusError(header.status)) then
        begin
          qr := TQueryNode.Create;
          qr.DateTime := header.timestamp;
          qr.FirstTimeStamp := header.cstart;
          qr.LastTimeStamp := header.cstop;
          FSTList.AddHandle(Stream.ReadInteger, VirtualTree.AddChild(nil, qr));
        end;
      hm_isc_dsql_free_statement:
        if Stream.ReadInteger = DSQL_drop then
           FSTList.RemoveHandle(Stream.ReadInteger);
      hm_isc_dsql_prepare:
        begin
          str := Stream.ReadString;
          if (not acPause.Checked) and (not IsStatusError(header.status)) then
          begin
            node := FSTList.FindNode(Stream.ReadInteger);
            if node <> nil then
            begin
              qr := TQueryNode(VirtualTree.GetNodeData(node)^);
              qr.LastTimeStamp := header.cstop;
              qr.sql := str;
              qr.plan := Stream.ReadString;
              qr.preparetime := qr.preparetime + (header.cstop - header.cstart);
              UpdateNode;
            end;
          end;
        end;
      hm_isc_dsql_execute2:
        if (not acPause.Checked) {and (not IsStatusError(header.status))} then
        begin
          node := FSTList.FindNode(Stream.ReadInteger);
          if node <> nil then
          begin
            qr := TQueryNode(VirtualTree.GetNodeData(node)^);
            qr.LastTimeStamp := header.cstop;
            qr.executetime := qr.executetime + (header.cstop - header.cstart);
            inc(qr.executecount);
            inc(qr.affected, Stream.ReadInteger);
            str := Stream.ReadString;
            if str <> '' then
              qr.queryparams.Add(str);
            UpdateNode;
          end;
        end;
      hm_isc_dsql_exec_immed2:
        if (not acPause.Checked) {and (not IsStatusError(header.status))} then
        begin
          qr := TQueryNode.Create;
          qr.DateTime := header.timestamp;
          qr.FirstTimeStamp := header.cstart;
          qr.LastTimeStamp := header.cstop;
          node := VirtualTree.AddChild(nil, qr);
          node.CheckType := ctCheckBox;
          node.CheckState := csUncheckedNormal;
          qr.sql := Stream.ReadString;
          qr.executetime := qr.executetime + (header.cstop - header.cstart);
          inc(qr.executecount);
          str := Stream.ReadString;
          if str <> '' then
            qr.queryparams.Add(str);
          UpdateNode;
        end;
      hm_isc_dsql_fetch:
        if (not acPause.Checked) and (not IsStatusError(header.status)) then
        begin
          node := FSTList.FindNode(Stream.ReadInteger);
          if node <> nil then
          begin
            qr := TQueryNode(VirtualTree.GetNodeData(node)^);
            qr.LastTimeStamp := header.cstop;
            qr.fetchtime := qr.fetchtime + (header.cstop - header.cstart);
            inc(qr.affected);
            UpdateNode;
          end;
        end;
    else
      editor.Lines.Add(MethodIdToString(header.methodid) + ' - ' + format('%10.2f',[((header.cstop - header.cstart ) / FFrequency) * 1000]));
    end;
  finally
    stream.Free;
  end;
  Application.ProcessMessages;
end;

procedure TMainForm.VirtualTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  TObject(Sender.GetNodeData(Node)^).Free;
end;

procedure TMainForm.VirtualTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  qr: TQueryNode;
begin
  qr := TQueryNode(Sender.GetNodeData(Node)^);
  case Column of
    0: CellText := TimeToStr(qr.DateTime);
    1: CellText := StripLines(qr.sql);
    2: CellText := Format('%d', [qr.executecount]);
    3: CellText := Format('%d', [qr.affected]);
    4: CellText := Format('%f', [(qr.preparetime / FFrequency) * 1000]);
    5: CellText := Format('%f', [(qr.executetime / FFrequency) * 1000]);
    6: CellText := Format('%f', [(qr.fetchtime / FFrequency) * 1000]);
    7: CellText := Format('%f', [((qr.LastTimeStamp - qr.FirstTimeStamp)/ FFrequency) * 1000]);
  end;
end;

procedure TMainForm.VirtualTreeFocusChanging(Sender: TBaseVirtualTree;
  OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
var
  qr: TQueryNode;
begin
  qr := TQueryNode(Sender.GetNodeData(NewNode)^);
  Editor.Clear;
  qr.DisplayQueryInfo(Editor.Lines);
end;

procedure TMainForm.VirtualTreeChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin
  Allowed := false;
end;

procedure TMainForm.VirtualTreeCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var id1, id2: TQueryNode;
begin
  id1 := TQueryNode(Sender.GetNodeData(Node1)^);
  id2 := TQueryNode(Sender.GetNodeData(Node2)^);
  case Column of
    0: result := id1.Index - id2.Index;
    1: result := AnsiCompareText(id1.sql, id2.sql);
    2: result := id1.executecount - id2.executecount;
    3: result := id1.affected - id2.affected;
    4: result := Integer(id1.preparetime - id2.preparetime);
    5: result := Integer(id1.executetime - id2.executetime);
    6: result := Integer(id1.fetchtime - id2.fetchtime);
    7: result := Integer((id1.LastTimeStamp - id1.FirstTimeStamp) - (id2.LastTimeStamp - id2.FirstTimeStamp));
  end;
end;

procedure TMainForm.VirtualTreeHeaderClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    with Sender, Treeview do
    begin
      if SortColumn > NoColumn then
        Columns[SortColumn].Options := Columns[SortColumn].Options + [coParentColor];

      if (SortColumn = NoColumn) or (SortColumn <> Column) then
      begin
        SortColumn := Column;

        if SortColumn in [0,1] then
          SortDirection := sdAscending else
          SortDirection := sdDescending;
      end
      else
        if SortDirection = sdAscending then
          SortDirection := sdDescending
        else
          SortDirection := sdAscending;

      Columns[SortColumn].Color := $F7F7F7;
      SortTree(SortColumn, SortDirection, true);

    end;
  end;
end;

procedure TMainForm.VirtualTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  inc(NumberGenerator);
  TQueryNode(Sender.GetNodeData(Node)^).index := NumberGenerator;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not acRun.Enabled then
  begin
    MessageDlg('Please close hooked application before', mtError, [mbOK], 0);
    CanClose := false;
  end;
  UninstallMadCHook;
end;

procedure TMainForm.acClearExecute(Sender: TObject);
var
  n1, n2: PVirtualNode;
begin
  VirtualTree.BeginUpdate;
  try
    n1 := VirtualTree.RootNode.LastChild;
    while n1 <> nil do
    begin
      n2 := n1.PrevSibling;
      if n1.CheckState = csUncheckedNormal then
        VirtualTree.DeleteNode(n1);
      n1 := n2;
    end;
  finally
    VirtualTree.EndUpdate;
  end;
end;

procedure TMainForm.acCopyExecute(Sender: TObject);
var
  lst: TStringList;
begin
  if ActiveControl = VirtualTree then
  begin
    lst := TStringList.Create;
    try
      DisplayQueries(lst, true);
      if (lst.Count > 0) then
        Clipboard.SetTextBuf(PChar(lst.Text));
    finally
      lst.Free;
    end;
  end else
    if ActiveControl = Editor then
      Editor.ExecuteCommand(ecCopy, #0, nil);
end;

procedure TMainForm.acSelectAllExecute(Sender: TObject);
begin
  if ActiveControl = Editor then
    Editor.SelectAll else
    if ActiveControl = VirtualTree then
      VirtualTree.SelectAll(true);
end;

procedure TMainForm.acSaveToFileExecute(Sender: TObject);
var
  lst: TStringList;
begin
  if SaveDialog.Execute then
  begin
    lst := TStringList.Create;
    try
      DisplayQueries(lst, false);
      lst.SaveToFile(SaveDialog.FileName);
    finally
      lst.Free;
    end;
  end;
end;

procedure TMainForm.acPauseExecute(Sender: TObject);
begin
//
end;

procedure TMainForm.LoadParams;
var
  inifile: TIniFile;
begin
  inifile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
  try
    EditAppPath.Text := inifile.ReadString(CONFIGSECTION, 'APPPATH', '');
    EditLibrary.Text := inifile.ReadString(CONFIGSECTION, 'LIBPATH', 'GDS32.DLL');
  finally
    inifile.Free;
  end;
end;

procedure TMainForm.SaveParams;
var
  inifile: TIniFile;
begin
  inifile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
  try
    inifile.WriteString(CONFIGSECTION, 'APPPATH', EditAppPath.Text);
    inifile.WriteString(CONFIGSECTION, 'LIBPATH', EditLibrary.Text);
  finally
    inifile.Free;
  end;
end;

procedure TMainForm.acRunExecute(Sender: TObject);
begin
  LoadProgram(EditAppPath.text, 'UIBHook.dll', EditLibrary.Text);
end;

procedure TMainForm.acDeleteExecute(Sender: TObject);
var
  n1, n2: PVirtualNode;
begin
  VirtualTree.BeginUpdate;
  try
    n1 := VirtualTree.RootNode.LastChild;
    while n1 <> nil do
    begin
      n2 := n1.PrevSibling;
      if VirtualTree.Selected[n1] then
      begin
        if n1.CheckState = csCheckedNormal then
          FSTList.RemoveNode(n1);
        VirtualTree.DeleteNode(n1);
      end;
      n1 := n2;
    end;
  finally
    VirtualTree.EndUpdate;
  end;
end;

procedure TMainForm.acStayOnTopExecute(Sender: TObject);
begin
  if acStayOnTop.Checked then
    FormStyle := fsStayOnTop else
    FormStyle := fsNormal;
end;

procedure TMainForm.acExitExecute(Sender: TObject);
begin
  Close
end;

procedure TMainForm.SysTrayDblClick(sender: TObject);
begin
  Visible := not Visible;
end;

procedure TMainForm.WndProc(var MsgRec: TMessage);
begin
  if (MsgRec.Msg = WM_SYSCOMMAND) and (MsgRec.WParam = SC_MINIMIZE) then
    Hide else
    inherited;

end;

end.



