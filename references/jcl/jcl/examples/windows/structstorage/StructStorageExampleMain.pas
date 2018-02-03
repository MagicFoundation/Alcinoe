{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ViewMain.PAS, released on 2002-12-29.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: $Date$

You may retrieve the latest version of this file at the Project JEDI's Code Library home page,
located at https://github.com/project-jedi/jcl

Description:

 Fairly complete demo program for the JclStructStorage unit.
 Note that the HexDump unit was taken from Borland's ResXplorer demo and has been
 slightly modified by me. It is still copyrighted by Borland, of course.

-----------------------------------------------------------------------------}

unit StructStorageExampleMain;

{$I jcl.inc}

interface

uses
  Windows, SysUtils, Classes, Messages, Forms, Menus, StdActns, StdCtrls, ComCtrls,
  ActnList, ImgList, Controls, Dialogs, ExtCtrls, Graphics, HexDump,
  JclStructStorage;

const
  WM_SHOWABOUT = WM_USER + 1;

type
  TfrmMain = class(TForm)
    mmMain: TMainMenu;
    OpenDialog: TOpenDialog;
    File1: TMenuItem;
    Open1: TMenuItem;
    Exit1: TMenuItem;
    tvDocInfo: TTreeView;
    StatusBar1: TStatusBar;
    il16: TImageList;
    Actions1: TMenuItem;
    N1: TMenuItem;
    Addfolder1: TMenuItem;
    Addfile1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Delete1: TMenuItem;
    alMain: TActionList;
    acOpen: TAction;
    acExit: TAction;
    acAddFolder: TAction;
    acAddFile: TAction;
    acDelete: TAction;
    acAbout: TAction;
    reDetails: TRichEdit;
    acEditData: TAction;
    acSaveData: TAction;
    Edit1: TMenuItem;
    Editstream1: TMenuItem;
    Savechanges1: TMenuItem;
    acCut: TEditCut;
    acCopy: TEditCopy;
    acPaste: TEditPaste;
    acUndo: TEditUndo;
    Undo1: TMenuItem;
    N4: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    N5: TMenuItem;
    acRename: TAction;
    Rename1: TMenuItem;
    popTreeView: TPopupMenu;
    AddFolder2: TMenuItem;
    AddFile2: TMenuItem;
    Rename2: TMenuItem;
    Delete2: TMenuItem;
    N7: TMenuItem;
    acRefresh: TAction;
    Splitter1: TSplitter;
    acProperties: TAction;
    Properties1: TMenuItem;
    acProper1: TMenuItem;
    N6: TMenuItem;
    acTransacted: TAction;
    ransacted1: TMenuItem;
    N9: TMenuItem;
    acNew: TAction;
    SaveDialog: TSaveDialog;
    New1: TMenuItem;
    N10: TMenuItem;
    Refresh1: TMenuItem;
    acSave: TAction;
    Save1: TMenuItem;
    N8: TMenuItem;
    N2: TMenuItem;
    acSaveAs: TAction;
    SaveAs1: TMenuItem;
    procedure tvDocInfoDeletion(Sender: TObject; Node: TTreeNode);
    procedure tvDocInfoCollapsed(Sender: TObject; Node: TTreeNode);
    procedure tvDocInfoExpanded(Sender: TObject; Node: TTreeNode);
    procedure FormCreate(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acAddFolderExecute(Sender: TObject);
    procedure acAddFileExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
    procedure alMainUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure acEditDataExecute(Sender: TObject);
    procedure acSaveDataExecute(Sender: TObject);
    procedure tvDocInfoChange(Sender: TObject; Node: TTreeNode);
    procedure tvDocInfoEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure tvDocInfoEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure acRenameExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acPropertiesExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acTransactedExecute(Sender: TObject);
    procedure tvDocInfoDblClick(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
  private
    { Private declarations }
    FFilename: string;
    FUpdating: boolean;
    HD: THexDump;
    FModified: boolean;
    procedure SortTree;
    // returns the folder in NOde.Data or nil if it isn't a folder
    function GetFolder(Node: TTreeNode): TJclStructStorageFolder;
    // returns the stream in Node.Data or nil if it isn't a stream
    function GetStream(Node: TTreeNode): TStream;
    // loads an exsisting or creates a new file with name AFilename
    procedure LoadFile(const AFilename: string; CreateNew: boolean);
    // add Storage as a subnode to ParentNode using the name AName
    procedure AddFolder(ParentNode: TTreeNode; AName: string; Storage: TJclStructStorageFolder);
    // add a stream in Storage with name AName as a subnode to ParentNode using the name
    procedure AddFile(ParentNode: TTreeNode; AName: string; Storage: TJclStructStorageFolder);
    // show the content of Stream
    procedure ViewDetails(Stream: TStream);
    // show the entire content of the laoded document
    procedure ViewDocument;
    // free the object in the Node.Data property
    // recurses the subnodes of Node
    procedure FreeData(const Node: TTreeNode);
    // adds a file stream to Node without creating a new node
    procedure UpdateFileData(Node: TTreeNode; const AName: string;
      Storage: TJclStructStorageFolder);
    // adds Storage to Node without creating a new node. Also adds new nodes for substorages
    // and substreams
    procedure UpdateFolderData(Node: TTreeNode; const AName: string; Storage: TJclStructStorageFolder);
    procedure WmShowAbout(var Msg: TMEssage); message WM_SHOWABOUT;
    function GetModified: boolean;
    procedure SetModified(const Value: boolean);
    procedure CheckModified;
    function GetReadOnly: boolean;
    procedure SetReadOnly(const Value: boolean);
  public
    { Public declarations }
    property Modified: boolean read GetModified write SetModified;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly;
  end;

var
  frmMain: TfrmMain;

implementation
uses
  ActiveX, ComObj, PropsFrm;

{$R *.DFM}

const
  cImageClosed = 0;
  cImageOpen = 1;
  cImageDoc = 2;
  cImageMod = 3;

function MinimizeName(const Filename: string; Canvas: TCanvas; MaxLen: Integer): string;
var
  R: TRect;
begin
  Result := Filename;
  if Result <> '' then
  begin
    UniqueString(Result);
    R := Rect(0, 0, MaxLen, Canvas.TextHeight('Wq'));
    if DrawText(Canvas.Handle, PChar(@Result[1]), Length(Result), R,
      DT_SINGLELINE or DT_MODIFYSTRING or DT_PATH_ELLIPSIS or DT_CALCRECT or DT_NOPREFIX) = 0 then
      Result := Filename;
  end;
end;

// returns true if Node.Data contains a TJclStructStorageFolder instance

function IsFolder(Node: TTreeNode): boolean;
begin
  Result := (Node <> nil) and (Node.Data <> nil) and (TObject(Node.Data) is TJclStructStorageFolder);
end;

// finds and returns the first sibling of ASibling (or ASibling itself) that has
// Text = AName. Returns nil if sucha  node couldn't be found

function FindSibling(ASibling: TTreeNode; AName: string): TTreeNode;
begin
  Result := ASibling;
  if Result = nil then Exit;
  // search backwards
  while (Result <> nil) do
  begin
    if AnsiSameText(Result.Text, AName) then
      Exit;
    Result := Result.getPrevSibling;
  end;
  Result := ASibling;
  // search forwards
  while (Result <> nil) do
  begin
    if AnsiSameText(Result.Text, AName) then
      Exit;
    Result := Result.getNextSibling;
  end;
  Result := nil;
end;

function YesNoDlg(const Caption, Msg: string): boolean;
begin
  Result := Windows.MessageBox(0, PChar(Msg), PChar(Caption), MB_YESNO or MB_ICONQUESTION or MB_TASKMODAL) = IDYES;
end;

procedure ErrorDlg(const Caption, Msg: string);
begin
  Windows.MessageBox(0, PChar(Msg), PChar(Caption), MB_OK or MB_ICONERROR or MB_TASKMODAL);
end;

procedure TfrmMain.LoadFile(const AFilename: string; CreateNew: boolean);
var
  Root: TJclStructStorageFolder;
  HR: HResult;
  AModes: TJclStructStorageAccessModes;
begin
  Screen.Cursor := crHourGlass;
  FUpdating := true;
  try
    if (AFilename <> '') and ((TJclStructStorageFolder.IsStructured(AFilename) = S_OK)or CreateNew) then
    begin
      FFilename := AFilename;
      tvDocInfo.Items.BeginUpdate;
      try
        tvDocInfo.Items.Clear;
        HD.Clear;
        if CreateNew then
          AModes := [smCreate]
        else if ReadOnly then
          AModes := [smOpenRead]
        else
          AModes := [smOpenRead, smOpenWrite];
        AModes := AModes + [smShareDenyRead, smShareDenyWrite];
        Root := TJclStructStorageFolder.Create(FFilename, AModes, CreateNew);
        AddFolder(nil, SRoot, Root);
      finally
        tvDocInfo.Items.EndUpdate;
      end;
    end
    else if YesNoDlg(SConfirmConversion, SConvertFilePrompt) then
    begin
      HR := TJclStructStorageFolder.Convert(AFilename);
      if Succeeded(HR) then
      begin
        ShowMessage(SConvertSuccess);
        LoadFile(AFilename, false);
      end
      else
        ErrorDlg(SError, Format(SConvertFailFmt, [SysErrorMessage(HR)]));
    end;
    if tvDocInfo.Items.Count > 0 then
    begin
      tvDocInfo.Items[0].Expand(false);
      tvDocInfo.Selected := tvDocInfo.Items[0];
      tvDocInfo.Selected.Focused := true;
    end;
    StatusBar1.Panels[0].Text := MinimizeName(FFilename, StatusBar1.Canvas,
      StatusBar1.Panels[0].Width - 4);
    SortTree;
  finally
    Screen.Cursor := crDefault;
    FUpdating := false;
    Modified := false;
  end;
end;

procedure TfrmMain.tvDocInfoDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Node.Data <> nil then
    TObject(Node.Data).Free;
  Node.Data := nil;
end;

function TfrmMain.GetStream(Node: TTreeNode): TStream;
begin
  if (Node <> nil) and (Node.Data <> nil) and (TObject(Node.Data) is TStream) then
  begin
    Result := TStream(Node.Data);
    Result.Seek(0, soFrombeginning);
  end
  else
    Result := nil;
end;

procedure TfrmMain.tvDocInfoCollapsed(Sender: TObject; Node: TTreeNode);
begin
  Node.ImageIndex := cImageClosed;
  Node.SelectedIndex := cImageClosed;
end;

procedure TfrmMain.tvDocInfoExpanded(Sender: TObject; Node: TTreeNode);
begin
  Node.ImageIndex := cImageOpen;
  Node.SelectedIndex := cImageOpen;
end;

procedure TfrmMain.ViewDetails(Stream: TStream);
var
  aSize: double;
begin
  if acEditData.Checked then acEditDataExecute(nil); // toggle into browse mode
  HD.LoadFromStream(Stream);
  if Stream <> nil then
  begin
    aSize := Stream.Size;
    StatusBar1.Panels[1].Text := Format(SBytesFloatFmt, [aSize]);
  end
  else
    StatusBar1.Panels[1].Text := '';
end;

procedure TfrmMain.ViewDocument;
var
  Filename: string;
  F: TFileStream;
begin
  Filename := TJclStructStorageFolder(tvDocInfo.Items.getFirstNode.Data).Name;
  if FileExists(Filename) then
  begin
    F := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
    try
      ViewDetails(F);
    finally
      F.Free;
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  HD := CreateHexDump(self);
  HD.Font := self.Font;
  //  HD.Font.Name := 'Courier New';
  HD.AddressColor := clMaroon;
  HD.AnsiCharColor := clNavy;
  Application.Title := Caption;
end;

function TfrmMain.GetFolder(Node: TTreeNode): TJclStructStorageFolder;
begin
  if (Node <> nil) and (Node.Data <> nil) and (TObject(Node.Data) is TJclStructStorageFolder) then
    Result := TJclStructStorageFolder(Node.Data)
  else
    Result := nil;
end;

procedure TfrmMain.CheckModified;
begin
  if Modified and YesNoDlg(SConfirm, SConfirmSaveChanges) then
    acSave.Execute;
end;

procedure TfrmMain.acOpenExecute(Sender: TObject);
begin
  // if in transacted mode, ask user to save any changes before loading a new file
  CheckModified;
  ReadOnly := false;
  if OpenDialog.Execute then
    LoadFile(OpenDialog.FileName, false);
end;

procedure TfrmMain.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.acAddFolderExecute(Sender: TObject);
var
  S: string;
  N: TTreeNode;
  SS, SS2: TJclStructStorageFolder;
begin
  if not IsFolder(tvDocInfo.Selected) then
    N := tvDocInfo.Selected.Parent
  else
    N := tvDocInfo.Selected;
  if (N = nil) then
    Exit;
  if InputQuery(SAddFolder, SFolderNameLabel, S) then
  begin
    if S = '' then
    begin
      ErrorDlg(SError, SErrNameEmpty);
      Exit;
    end;
    // since a duplicate name replaces the current folder/file, we have to check
    // explicitly for duplicates here so we don't add a duplicate node by mistake
    if (FindSibling(tvDocInfo.Selected.getFirstChild, S) <> nil) then
    begin
      ErrorDlg(SError, SErrNameDuplicate);
      Exit;
    end;

    SS := GetFolder(N);
    if not SS.Add(S, true) then
      OleError(SS.LastError)
    else if SS.GetFolder(S, SS2) then
    begin
      Modified := true;
      AddFolder(N, S, SS2);
    end;
  end;
  SortTree;
end;

procedure TfrmMain.acAddFileExecute(Sender: TObject);
var
  S: string;
  N: TTreeNode;
  SS: TJclStructStorageFolder;
begin
  if not IsFolder(tvDocInfo.Selected) then
    N := tvDocInfo.Selected.Parent
  else
    N := tvDocInfo.Selected;
  if (N = nil) then Exit;
  if InputQuery(SAddFile, SFileNameLabel, S) then
  begin
    if S = '' then
    begin
      ErrorDlg(SError, SErrNameEmpty);
      Exit;
    end;
    // since a duplicate name replaces the current folder/file, we have to check
    // explicitly for duplicates here so we don't add a duplicate node by mistake
    if (FindSibling(N.getFirstChild, S) <> nil) then
    begin
      ErrorDlg(SError, SErrNameDuplicate);
      Exit;
    end;
    SS := GetFolder(N);
    if not SS.Add(S, false) then
      OleError(SS.LastError)
    else
    begin
      AddFile(N, S, SS);
      Modified := true;
    end;
  end;
  SortTree;
end;

procedure TfrmMain.acDeleteExecute(Sender: TObject);
begin
  if YesNoDlg(SConfirm, SDeletePrompt) then
    if not TJclStructStorageFolder(tvDocInfo.Selected.Parent.Data).Delete(tvDocInfo.Selected.Text) then
      OleError(TJclStructStorageFolder(tvDocInfo.Selected.Parent.Data).LastError)
    else
    begin
      tvDocInfo.Selected.Delete;
      Modified := true;
    end;
end;

procedure TfrmMain.acAboutExecute(Sender: TObject);
var
  ParamsW: TMsgBoxParamsW;
  ParamsA: TMsgBoxParamsA;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    with ParamsW do
    begin
      cbSize := sizeof(TMsgBoxParamsW);
      hwndOwner := Handle;
      hInstance := SysInit.hInstance;
      lpszText := PWideChar(WideString(SAboutMsg));
      lpszCaption := PWideChar(WideString(SAboutCaption));
      dwStyle := MB_OK or MB_USERICON;
      lpszIcon := PWideChar(WideString('MAINICON'));
      dwContextHelpId := 0;
      lpfnMsgBoxCallback := nil;
      dwLanguageId := GetUserDefaultLangID;
      MessageBoxIndirectW(ParamsW);
    end
  end
  else
    with ParamsA do
    begin
      cbSize := sizeof(TMsgBoxParamsA);
      hwndOwner := Handle;
      hInstance := SysInit.hInstance;
      lpszText := PAnsiChar(AnsiString(SAboutMsg));
      lpszCaption := PAnsiChar(AnsiString(SAboutCaption));
      dwStyle := MB_OK or MB_USERICON;
      lpszIcon := PAnsiChar('MAINICON');
      dwContextHelpId := 0;
      lpfnMsgBoxCallback := nil;
      dwLanguageId := GetUserDefaultLangID;
      MessageBoxIndirectA(ParamsA);
    end;
end;

procedure TfrmMain.alMainUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  IsReadOnly: boolean;
begin
  IsReadOnly := ReadOnly;
  acTransacted.Enabled := not IsReadOnly;
  acSave.Enabled := not IsReadOnly and Modified;
  acSaveAs.Enabled := not IsReadOnly and (FFilename <> '');
  acDelete.Enabled := not IsReadOnly and
    (tvDocInfo.Selected <> nil) and (tvDocInfo.Selected.Parent <> nil);
  acAddFolder.Enabled := not IsReadOnly and
    (tvDocInfo.Selected <> nil) and not reDetails.Focused;
  acAddFile.Enabled := acAddFolder.Enabled;
  acEditData.Enabled := not ReadOnly and (GetStream(tvDocInfo.Selected) <> nil);
  acSaveData.Enabled := not IsReadOnly and acEditData.Enabled
    and acEditData.Checked and reDetails.Modified;
  acRename.Enabled := not IsReadOnly and (tvDocInfo.Selected <> nil)
    and (tvDocInfo.Selected.Parent <> nil);
  acProperties.Enabled := (tvDocInfo.Selected <> nil);
end;

{$IFDEF RTL230_UP}
function TreeSort(lParam1, lParam2, lParamSort: LPARAM): Integer; stdcall;
{$ELSE ~RTL230_UP}
function TreeSort(lParam1, lParam2, lParamSort: Longint): Integer; stdcall;
{$ENDIF ~RTL230_UP}
begin
  if IsFolder(TTreeNode(lParam1)) = IsFolder(TTreeNode(lParam2)) then
    Result := AnsiCompareText(TTreeNode(lParam1).Text, TTreeNode(lParam2).Text)
  else if IsFolder(TTreeNode(lParam1)) then
    Result := -1
  else if IsFolder(TTreeNode(lParam2)) then
    Result := 1
  else
    Result := 0;
end;

procedure TfrmMain.SortTree;
begin
  tvDocInfo.CustomSort(TreeSort, 0{$IFDEF COMPILER6_UP}, true{$ENDIF});
end;

function TfrmMain.GetModified: boolean;
begin
  // can never be modified when running in direct mode or as ReadOnly
  Result := FModified and not ReadOnly and (FFilename <> '') and
    acTransacted.Checked and (tvDocInfo.Items.Count > 0);
end;

procedure TfrmMain.SetModified(const Value: boolean);
begin
  FModified := Value;
end;

function TfrmMain.GetReadOnly: boolean;
begin
  Result := ofReadOnly in OpenDialog.Options;
end;

procedure TfrmMain.SetReadOnly(const Value: boolean);
begin
  if Value then
    OpenDialog.Options := OpenDialog.Options + [ofReadOnly]
  else
    OpenDialog.Options := OpenDialog.Options - [ofReadOnly];
end;

procedure TfrmMain.AddFile(ParentNode: TTreeNode; AName: string;
  Storage: TJclStructStorageFolder);
var
  Stream: TStream;
begin
  if ParentNode <> nil then
    with ParentNode do
    begin
      ImageIndex := Ord(Expanded);
      SelectedIndex := ImageIndex;
    end;
  if not Storage.GetFileStream(AName, Stream) then
    OleError(Storage.LastError)
  else
    with tvDocInfo.Items.AddChildObject(ParentNode, AName, Stream) do
    begin
      ImageIndex := cImageDoc;
      SelectedIndex := cImageDoc;
      if not FUpdating then
        MakeVisible;
    end;
end;

procedure TfrmMain.AddFolder(ParentNode: TTreeNode; AName: string;
  Storage: TJclStructStorageFolder);
var
  S: TStringlist;
  i: integer;
  N: TTreeNode;
  ST: TJclStructStorageFolder;
begin
  if ParentNode <> nil then
    with ParentNode do
    begin
      ImageIndex := Ord(Expanded);
      SelectedIndex := ImageIndex;
    end;
  N := tvDocInfo.Items.AddChildObject(ParentNode, AName, Storage);
  with N do
  begin
    ImageIndex := Ord(Expanded);
    SelectedIndex := ImageIndex;
    if not FUpdating then
      MakeVisible;
  end;

  S := TStringlist.Create;
  try
    // folders
    Storage.GetSubItems(S, true);
    for i := 0 to S.Count - 1 do
    begin
      if not Storage.GetFolder(S[i], ST) then
        OleError(Storage.LastError)
      else
        AddFolder(N, S[i], ST);
    end;
    S.Clear;
    // files
    Storage.GetSubItems(S, false);
    for i := 0 to S.Count - 1 do
      AddFile(N, S[i], Storage);
  finally
    S.Free;
  end;
end;

procedure TfrmMain.acEditDataExecute(Sender: TObject);
begin
  acEditData.Checked := not acEditData.Checked;
  if acEditData.Checked then
  begin
    reDetails.Visible := true;
    HD.Visible := false;
    reDetails.Lines.LoadFromStream(GetStream(tvDocInfo.Selected));
    reDetails.Modified := false;
    reDetails.SelStart := MaxInt;
    reDetails.SetFocus;
  end
  else
  begin
    HD.Visible := true;
    reDetails.Visible := false;
    tvDocInfoChange(Sender, tvDocInfo.Selected);
  end;
end;

procedure TfrmMain.acSaveDataExecute(Sender: TObject);
var
  S: TStream;
begin
  S := GetStream(tvDocInfo.Selected);
  if (S <> nil) and reDetails.Modified then
  begin
    S.Size := 0; // clear so we don't have old data at the end of the stream (if it's shorter now)
    reDetails.Lines.SaveToStream(S); // add new
    Modified := true;
    if (tvDocInfo.Selected <> nil) then
      with tvDocInfo.Selected do
      begin
        ImageIndex := cImageDoc + Ord(acTransacted.Checked);
        SelectedIndex := ImageIndex;
      end;
  end;
  acEditData.Execute; // toggle into browse mode
end;

procedure TfrmMain.tvDocInfoChange(Sender: TObject; Node: TTreeNode);
begin
  if Node = tvDocInfo.Items.getFirstNode then
    ViewDocument
  else
    ViewDetails(GetStream(Node));
end;

procedure TfrmMain.tvDocInfoEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := (Node <> nil) and (Node.Parent <> nil);
end;

procedure TfrmMain.FreeData(const Node: TTreeNode);
var
  N: TTreeNode;
begin
  TObject(Node.Data).Free;
  Node.Data := nil;
  N := Node.getFirstChild;
  while Assigned(N) do
  begin
    FreeData(N);
    N := N.GetNextSibling;
  end;
end;

procedure TfrmMain.acRenameExecute(Sender: TObject);
begin
  tvDocInfo.Selected.EditText;
end;

procedure TfrmMain.UpdateFolderData(Node: TTreeNode; const AName: string; Storage: TJclStructStorageFolder);
var
  SS: TJclStructStorageFolder;
  S: TStringlist;
  i: integer;
begin
  TObject(Node.Data).Free;
  Node.Data := nil;
  if Storage <> nil then
  begin
    Node.Data := Storage;
    Node.Text := AName;
  end
  else
    Exit;
  Node.DeleteChildren;
  S := TStringlist.Create;
  try
    // sub folders
    Storage.GetSubItems(S, true);
    for i := 0 to S.Count - 1 do
    begin
      if not Storage.GetFolder(S[i], SS) then
        OleError(Storage.LastError)
      else
        AddFolder(Node, S[i], SS);
    end;
    S.Clear;
    // sub files
    if not Storage.GetSubItems(S, false) then
      OleError(Storage.LastError)
    else
      for i := 0 to S.Count - 1 do
        AddFile(Node, S[i], Storage);
  finally
    S.Free;
  end;
end;

procedure TfrmMain.UpdateFileData(Node: TTreeNode; const AName: string; Storage: TJclStructStorageFolder);
var
  SS: TStream;
begin
  TObject(Node.Data).Free;
  Node.Data := nil;
  if Storage.GetFileStream(AName, SS) then
  begin
    Node.Data := SS;
    Node.Text := AName;
  end
  else
    OleError(Storage.LastError);
end;

procedure TfrmMain.tvDocInfoEdited(Sender: TObject; Node: TTreeNode;
  var S: string);
var
  SS, SS2: TJclStructStorageFolder;
  WasFolder: boolean;
begin
  // this is a bit convoluted since we can't rename a node that is open
  // so we have to destroy the Node.Data and recreate it again after the rename
  if (Node = nil) or (Node.Parent = nil) then
  begin
    ErrorDlg(SError, SErrNodeEdit);
    Node.EndEdit(true);
  end
  else
  begin
    SS := GetFolder(Node.Parent);
    WasFolder := IsFolder(Node);
    FreeData(Node); // release any storages / streams so we can rename
    if (SS = nil) or not SS.Rename(Node.Text, S) then
    begin
      if SS <> nil then
        OleError(SS.LastError)
      else
        ErrorDlg(SError, SErrNodeRename);
      S := Node.Text;
      Node.EndEdit(true);
    end
    else
    begin // update the node's (and subnodes') Data with new storages / streams
      if WasFolder then
      begin
        if not SS.GetFolder(S, SS2) then
          OleError(SS.LastError)
        else
          UpdateFolderData(Node, S, SS2);
      end
      else
        UpdateFileData(Node, S, SS);
    end;
    Modified := true;
  end;
  SortTree;
end;

procedure TfrmMain.acRefreshExecute(Sender: TObject);
begin
  SortTree;
end;

procedure TfrmMain.acPropertiesExecute(Sender: TObject);
var
  Stat: TStatStg;
  B: Boolean;
begin
  B := false;
  if IsFolder(tvDocInfo.Selected) then
    B := TJclStructStorageFolder(tvDocInfo.Selected.Data).GetStats(Stat, true)
  else if tvDocInfo.Selected <> nil then
    B := TJclStructStorageStream(tvDocInfo.Selected.Data).GetStats(Stat, true);
  if B then
  begin
    TfrmProps.ShowProperties(Stat);
    JclStructStorage.CoMallocFree(Stat.pwcsName);
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  PostMessage(Handle, WM_SHOWABOUT, 0, 0);
end;

procedure TfrmMain.WmShowAbout(var Msg: TMEssage);
begin
  acAbout.Execute;
end;

procedure TfrmMain.acTransactedExecute(Sender: TObject);
begin
  acTransacted.Checked := not acTransacted.Checked;
  if FileExists(FFilename) then
  begin
    CheckModified;
    LoadFile(FFilename, false);
  end;
end;

procedure TfrmMain.tvDocInfoDblClick(Sender: TObject);
begin
  if (tvDocInfo.Selected <> nil) and not tvDocInfo.Selected.HasChildren then
    acProperties.Execute;
end;

procedure TfrmMain.acNewExecute(Sender: TObject);
begin
  CheckModified;
  ReadOnly := false;
  if SaveDialog.Execute then
    LoadFile(SaveDialog.Filename, true);
end;

procedure TfrmMain.acSaveExecute(Sender: TObject);
var
  N: TTreeNode;
begin
  if Modified then
  begin
    // we must call Commit on *every* storage to save our changes (the fine print!)
    N := tvDocInfo.Items.getFirstNode;
    while Assigned(N) do
    begin
      if IsFolder(N) then
      begin
        TJclStructStorageFolder(N.Data).Commit;
        N.ImageIndex := cImageDoc;
        N.SelectedIndex := cImageDoc;
      end;
      N := N.GetNext;
    end;
  end;
  Modified := false;
end;

procedure TfrmMain.acSaveAsExecute(Sender: TObject);
var
  AFile: TJclStructStorageFolder;
begin
  // I know: I could just as well have done a standard FileCopy, but that's not any fun!
  if SaveDialog.Execute then
  begin
    AFile := TJclStructStorageFolder.Create(SaveDialog.Filename, [smCreate], true);
    try
      AFile.Assign(TJclStructStorageFolder(tvDocInfo.Items.GetFirstNode.Data));
    finally
      AFile.Free;
    end;
    LoadFile(SaveDialog.Filename, false);
  end;
end;

end.

