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
{ The Original Code is FileViewer.pas.                                                             }
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

unit FileViewer;

{$I JCL.INC}

{.$DEFINE UsePeImagesCache}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, Menus, JclPeImage;

type
  TFileViewerChild = class(TForm)
    DependencyTreeView: TTreeView;
    Splitter1: TSplitter;
    ListViewsPanel: TPanel;
    Splitter2: TSplitter;
    ImportListView: TListView;
    ExportListView: TListView;
    PopupMenu1: TPopupMenu;
    Copy1: TMenuItem;
    Save1: TMenuItem;
    N1: TMenuItem;
    Selectall1: TMenuItem;
    Win32helpkeyword1: TMenuItem;
    ModulesListView: TListView;
    Splitter3: TSplitter;
    DumpPEfile1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ExportListViewData(Sender: TObject; Item: TListItem);
    procedure ImportListViewData(Sender: TObject; Item: TListItem);
    procedure ExportListViewColumnClick(Sender: TObject;
      Column: TListColumn);
    procedure ImportListViewColumnClick(Sender: TObject;
      Column: TListColumn);
    procedure FormDestroy(Sender: TObject);
    procedure DependencyTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure DependencyTreeViewDeletion(Sender: TObject; Node: TTreeNode);
    procedure ModulesListViewInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: String);
    procedure ExportListViewDblClick(Sender: TObject);
    procedure ModulesListViewDblClick(Sender: TObject);
  private
    FAnyRootError: Boolean;
    FBasePath: string;
    FCurrentImportDirIndex: Integer;
    FFileName: TFileName;
    FModulesList: TStringList;
    FExportViewImage, FParentImportViewImage: TJclPeImage;
    FPeImagesCache: TJclPeImagesCache;
    function GetModuleName: string;
    procedure SetFileName(const Value: TFileName);
    procedure ExportListViewSort;
    function ModuleToFileName(const ModuleName: string): TFileName;
    procedure ImportListViewSort;
    procedure InitTree;
    function IsListViewActiveAndFocused( ListView: TListView): Boolean;
    procedure UpdateExportView(Node: TTreeNode);
    procedure UpdateModulesView;
    procedure UpdateParentImportView(Node: TTreeNode);
    class procedure UpdateSortData(Column: TListColumn);
    function GetSelectedFileName: TFileName;
  public
    function GetWin32Function: string;
    property FileName: TFileName read FFileName write SetFileName;
    property ModuleName: string read GetModuleName;
    property SelectedFileName: TFileName read GetSelectedFileName;
  end;

var
  FileViewerChild: TFileViewerChild;

implementation

uses
  DependViewMain, ToolsUtils,
  JclBase, JclSysInfo, JclStrings, JclFileUtils;

{$R *.DFM}

type
  TPeModuleState = (
    modNoErrors,             // Normal module with no errors.
    modFwdNoErrors,          // Forwarded module with no errors.
    modDupNoErrors,          // Duplicate module with no errors.
    modDupFwdNoErrors,       // Forwarded duplicate module with no errors.
    modExportMissing,        // Module with one or more missing export functions
    modFwdExportMissing,     // Forwarded module with one or more missing export functions
    modDupExportMissing,     // Duplicate module with one or more missing export functions
    modDupFwdExportMissing,  // Forwarded duplicate module with one or more missing export functions
    modMissing,              // Missing module.
    modFwdMissing,           // Missing forwarded module.
    modInvalid,              // Invalid module.
    modFwdInvalid,           // Invalid forwarded module.
    modRoot                  // Root node.
    );

  TPeModuleImageInfo = record
    ImageIndex, StateIndex: Integer;
  end;

  PPeModuleNodeData = ^TPeModuleNodeData;
  TPeModuleNodeData = record
    State: TPeModuleState;
    ImportDirectoryIndex: Integer;
  end;

const
  imgModule               =  0;
  imgDupModule            =  1;
  imgModExportMissing     =  2;
  imgDupExportMissing     =  3;
  imgMissingModule        =  4;
  imgInvalidModule        =  5;
  imgForwardFlag          =  6;
  imgRoot                 =  7;
  imgExport               =  8;
  imgFwdExport            =  9;
  imgImport               = 10;
  imgUnresolvedImport     = 11;
  imgSortAsceding         = 12;
  imgSortDesceding        =  3;

  ErrorModules = [modMissing, modFwdMissing, modInvalid, modFwdInvalid];
  MissingExportModules = [modExportMissing, modFwdExportMissing, modDupExportMissing,
    modDupFwdExportMissing, modMissing, modFwdMissing, modInvalid, modFwdInvalid];
  ForwardedModules = [modFwdNoErrors, modDupFwdNoErrors, modFwdExportMissing,
    modDupFwdExportMissing];

  ModuleImages: array[TPeModuleState] of TPeModuleImageInfo = (
    (ImageIndex: imgModule; StateIndex: -1),
    (ImageIndex: imgModule; StateIndex: imgForwardFlag),
    (ImageIndex: imgDupModule; StateIndex: -1),
    (ImageIndex: imgDupModule; StateIndex: imgForwardFlag),
    (ImageIndex: imgModExportMissing; StateIndex: -1),
    (ImageIndex: imgModExportMissing; StateIndex: imgForwardFlag),
    (ImageIndex: imgDupExportMissing; StateIndex: -1),
    (ImageIndex: imgDupExportMissing; StateIndex: imgForwardFlag),
    (ImageIndex: imgMissingModule; StateIndex: -1),
    (ImageIndex: imgMissingModule; StateIndex: imgForwardFlag),
    (ImageIndex: imgInvalidModule; StateIndex: -1),
    (ImageIndex: imgInvalidModule; StateIndex: imgForwardFlag),
    (ImageIndex: imgRoot; StateIndex: -1)
  );

{ TFileViewerChild }

procedure TFileViewerChild.FormCreate(Sender: TObject);
begin
  FModulesList := TStringList.Create;
  FModulesList.Sorted := True;
  FModulesList.Duplicates := dupIgnore;
  FExportViewImage := TJclPeImage.Create;
  FPeImagesCache := TJclPeImagesCache.Create;
{$IFNDEF UsePeImagesCache}
  FParentImportViewImage := TJclPeImage.Create;
{$ENDIF}
  FCurrentImportDirIndex := -1;
  ExportListView.Height := ListViewsPanel.ClientHeight div 2;
  ImportListView.Tag := $100;
  UpdateSortData(ImportListView.Columns[0]);
  ExportListView.Tag := $100;
  UpdateSortData(ExportListView.Columns[0]);
  ModulesListView.Columns[0].Width := ColumnTextWidth;
end;

procedure TFileViewerChild.FormDestroy(Sender: TObject);
begin
  FModulesList.Free;
  FExportViewImage.Free;
  FPeImagesCache.Free;
{$IFNDEF UsePeImagesCache}
  FParentImportViewImage.Free;
{$ENDIF}
end;

procedure TFileViewerChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Fix_ListViewBeforeClose(Self);
  Action := caFree;
end;

function TFileViewerChild.GetModuleName: string;
begin
  Result := ExtractFileName(FFileName);
end;

procedure TFileViewerChild.InitTree;
var
  RootNode: TTreeNode;

  procedure SetNodeState(Node: TTreeNode; State: TPeModuleState);
  var
    I: Integer;
  begin
    PPeModuleNodeData(Node.Data)^.State := State;
    Node.ImageIndex := ModuleImages[State].ImageIndex;
    Node.SelectedIndex := ModuleImages[State].ImageIndex;
    Node.StateIndex := ModuleImages[State].StateIndex;
    if State in (MissingExportModules + ErrorModules) then
    begin
      if Node.Parent = RootNode then FAnyRootError := True;
      I := FModulesList.IndexOf(Node.Text);
      Assert(I >= 0);
      FModulesList.Objects[I] := Pointer(State);
    end;
  end;

  function AddNode(Node: TTreeNode; const Text: string; State: TPeModuleState): TTreeNode;
  var
    Data: PPeModuleNodeData;
  begin
    Result := DependencyTreeView.Items.AddChild(Node, Text);
    New(Data);
    Result.Data := Data;
    SetNodeState(Result, State);
  end;

  procedure ScanModule(const ModuleName: string; Node: TTreeNode; Forwarded, ErrorsOnly: Boolean);
  var
    ExeImage: TJclPeImage;
    I, Found: Integer;
    S: string;
    TempNode: TTreeNode;
    AddedNodes: array of TTreeNode;
    AddedNodesCount: Integer;
  begin
    ExeImage := FPeImagesCache[ModuleToFilename(ModuleName)];
    case ExeImage.Status of
      stOk:
        if not ErrorsOnly then
        begin
          with ExeImage.ImportList do
          begin
            SetLength(AddedNodes, Count);
            AddedNodesCount := 0;
            CheckImports(FPeImagesCache);
            SortList(ilName);
            for I := 0 to Count - 1 do
            begin
              S := Items[I].Name;
              Found := FModulesList.IndexOf(S);
              if Found = -1 then
              begin
                Found := FModulesList.Add(S);
                FModulesList.Objects[Found] := Pointer(modNoErrors);
                if Items[I].TotalResolveCheck = icUnresolved then
                  TempNode := AddNode(Node, S, modExportMissing)
                else
                  TempNode := AddNode(Node, S, modNoErrors);
                AddedNodes[AddedNodesCount] := TempNode;
                Inc(AddedNodesCount);
              end else
              begin
                if Items[I].TotalResolveCheck = icUnresolved then
                  TempNode := AddNode(Node, S, modDupExportMissing)
                else
                  TempNode := AddNode(Node, S, modDupNoErrors);
                ScanModule(TempNode.Text, TempNode, False, True); // !
              end;
              PPeModuleNodeData(TempNode.Data)^.ImportDirectoryIndex := Items[I].ImportDirectoryIndex;
            end;
          end;
          for I := 0 to AddedNodesCount - 1 do
            ScanModule(AddedNodes[I].Text, AddedNodes[I], False, False);
          with ExeImage.ExportList do
          begin
            CheckForwards(FPeImagesCache);
            for I := 0 to ForwardedLibsList.Count - 1 do
            begin
              S := ForwardedLibsList[I];
              Found := FModulesList.IndexOf(S);
              if Found = -1 then
              begin
                Found := FModulesList.Add(S);
                FModulesList.Objects[Found] := Pointer(modNoErrors);
                if TJclPeResolveCheck(ForwardedLibsList.Objects[I]) = icUnresolved then
                  AddNode(Node, S, modFwdExportMissing)
                else
                  AddNode(Node, S, modFwdNoErrors);
              end else
              begin
                if TJclPeResolveCheck(ForwardedLibsList.Objects[I]) = icUnresolved then
                  TempNode := AddNode(Node, S, modDupFwdExportMissing)
                else
                  TempNode := AddNode(Node, S, modDupFwdNoErrors);
                ScanModule(TempNode.Text, TempNode, True, True); // !
              end;
            end;
          end;
        end;
      stNotFound:
        if Forwarded then SetNodeState(Node, modFwdMissing) else SetNodeState(Node, modMissing);
    else
       if Forwarded then SetNodeState(Node, modFwdInvalid) else SetNodeState(Node, modInvalid);
    end;
  end;

begin
  with DependencyTreeView do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      Screen.Cursor := crHourGlass;
      RootNode := AddNode(nil, ModuleName, modRoot);
      FModulesList.AddObject(ModuleName, Pointer(modRoot));
      ScanModule(FFileName, RootNode, False, False);
      RootNode.Expand(False);
      Selected := RootNode;
    finally
      Items.EndUpdate;
      Screen.Cursor := crDefault;
    end;
  end;
  UpdateModulesView;
{$IFNDEF UsePeImagesCache}
  FPeImagesCache.Clear;
{$ENDIF}
end;

procedure TFileViewerChild.SetFileName(const Value: TFileName);
begin
  FAnyRootError := False;
  FFileName := Value;
  FBasePath := ExtractFilePath(FFileName);
  Caption := ModuleName;
  InitTree;
end;

class procedure TFileViewerChild.UpdateSortData(Column: TListColumn);
var
  ListView: TListView;
  I: Integer;
begin
  ListView := TListView(TListColumns(Column.Collection).Owner);
  ListView.Columns.BeginUpdate;
  with ListView.Columns do
    for I := 0 to Count - 1 do
      Items[I].ImageIndex := -1;
  if ListView.Tag and $FF = Column.Index then
    ListView.Tag := ListView.Tag xor $100
  else
    ListView.Tag := Column.Index;
  if ListView.Tag and $100 = 0 then
    Column.ImageIndex := imgSortAsceding
  else
    Column.ImageIndex := imgSortDesceding;
  ListView.Columns.EndUpdate;
end;

function TFileViewerChild.IsListViewActiveAndFocused( ListView: TListView): Boolean;
begin
  Result := (ActiveControl = ListView) and (ListView.ItemFocused <> nil);
end;

function TFileViewerChild.GetWin32Function: String;
const
  BracketChars: array [0..1] of Char = ( '[', ']' );
begin
  Result := '';
  if IsListViewActiveAndFocused(ImportListView) then
    Result := ImportListView.ItemFocused.Caption
  else
  if IsListViewActiveAndFocused(ExportListView) then
    Result := ExportListView.ItemFocused.Caption
  else
    Result := '';
  if Pos('@', Result) > 0 then
    Result := ''
  else
    Result := StrRemoveChars(Result, BracketChars);
end;

procedure TFileViewerChild.ExportListViewData(Sender: TObject;
  Item: TListItem);
begin
  with Item, FExportViewImage.ExportList[Item.Index] do
  begin
    Caption := Name;
    SubItems.Add(Format('%d', [Ordinal]));
    SubItems.Add(Format('%d', [Hint]));
    SubItems.Add(AddressOrForwardStr);
    if IsForwarded then ImageIndex := imgFwdExport else ImageIndex := imgExport;
  end;
end;

procedure TFileViewerChild.ImportListViewData(Sender: TObject; Item: TListItem);
var
  ViewItem: TJclPeImportFuncItem;
begin
  if FCurrentImportDirIndex = -1 then
    ViewItem := FParentImportViewImage.ImportList.AllItems[Item.Index]
  else
    ViewItem := FParentImportViewImage.ImportList[FCurrentImportDirIndex][Item.Index];
  with Item, ViewItem do
  begin
    if IndirectImportName then
      Caption := Format('[%s]', [Name])
    else
      Caption := Name;
    if Ordinal <> 0 then
    begin
      SubItems.Add(Format('%d', [Ordinal]));
      SubItems.Add('');
    end else
    begin
      SubItems.Add('');
      SubItems.Add(Format('%d', [Hint]));
    end;  
    if FCurrentImportDirIndex = -1 then SubItems.Add(ImportLib.Name);
    case ResolveCheck of
      icUnresolved: ImageIndex := imgUnresolvedImport;
      icResolved, icNotChecked: ImageIndex := imgImport;
    end;
  end;
end;

procedure TFileViewerChild.ExportListViewColumnClick(Sender: TObject; Column: TListColumn);
begin
  UpdateSortData(Column);
  ExportListViewSort;
end;

procedure TFileViewerChild.ImportListViewColumnClick(Sender: TObject; Column: TListColumn);
begin
  UpdateSortData(Column);
  ImportListViewSort;
end;

procedure TFileViewerChild.UpdateExportView(Node: TTreeNode);
begin
  with ExportListView.Items do
  begin
    BeginUpdate;
    if PPeModuleNodeData(Node.Data)^.State in ErrorModules then
      FExportViewImage.FileName := ''
    else
      FExportViewImage.FileName := ModuleToFilename(Node.Text);
    Count := FExportViewImage.ExportList.Count;
    ExportListViewSort;
    EndUpdate;
  end;
end;

procedure TFileViewerChild.UpdateParentImportView(Node: TTreeNode);
var
  ParentFileName: TFileName;
  NodeState: TPeModuleState;

  procedure ShowModuleColumn(B: Boolean);
begin
  with ImportListView do
    if (B xor (Columns.Count <> 3)) then
    begin
      Columns.BeginUpdate;
      if B then Columns.Add.Caption := 'Module' else
      begin
        Columns[3].Free;
        if Tag and $FF = 3 then
        begin
          Tag := $100;
          UpdateSortData(Columns[0]);
          ImportListViewSort;
        end;
      end;
      Columns.EndUpdate;
    end;
end;

begin
  with ImportListView.Items do
  begin
    BeginUpdate;
    if Node.Parent = nil then
      ParentFileName := Node.Text
    else
      ParentFileName := Node.Parent.Text;
    ParentFileName := ModuleToFilename(ParentFileName);
    NodeState := PPeModuleNodeData(Node.Data)^.State;
{$IFDEF UsePeImagesCache}
    FParentImportViewImage := FPeImagesCache[ParentFileName];
    FParentImportViewImage.ImportList.SortList(ilIndex);
{$ELSE}
    FParentImportViewImage.FileName := ParentFileName;
{$ENDIF}
    if (NodeState in MissingExportModules + ErrorModules) or FAnyRootError then
      FParentImportViewImage.ImportList.CheckImports;
    FParentImportViewImage.TryGetNamesForOrdinalImports;
    if NodeState in ForwardedModules then
    begin
      ShowModuleColumn(False);
      FCurrentImportDirIndex := -1;
      FParentImportViewImage.ImportList.FilterModuleName := Node.Text;
      Count := FParentImportViewImage.ImportList.AllItemCount;
    end else
    if Node.Parent = nil then
    begin
      ShowModuleColumn(True);
      FCurrentImportDirIndex := -1;
      FParentImportViewImage.ImportList.FilterModuleName := '';
      Count := FParentImportViewImage.ImportList.AllItemCount;
    end else
    begin
      ShowModuleColumn(False);
      FCurrentImportDirIndex := PPeModuleNodeData(Node.Data)^.ImportDirectoryIndex;
      Count := FParentImportViewImage.ImportList[FCurrentImportDirIndex].Count;
    end;
    ImportListViewSort;
    EndUpdate;
  end;
end;

procedure TFileViewerChild.DependencyTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  UpdateExportView(Node);
  UpdateParentImportView(Node);
end;

procedure TFileViewerChild.DependencyTreeViewDeletion(Sender: TObject; Node: TTreeNode);
begin
  Dispose(Node.Data); // PPeModuleNodeData
end;

procedure TFileViewerChild.ImportListViewSort;
const
  MapIndexToSortType: array[0..3] of TJclPeImportSort = (isName, isOrdinal, isHint, isLibImport);
begin
  with ImportListView do
  begin
    if FCurrentImportDirIndex = -1 then
      FParentImportViewImage.ImportList.SortAllItemsList(MapIndexToSortType[Tag and $FF], Tag and $100 <> 0)
    else
      FParentImportViewImage.ImportList[FCurrentImportDirIndex].SortList(MapIndexToSortType[Tag and $FF], Tag and $100 <> 0);
    Invalidate;
  end;
end;

procedure TFileViewerChild.ExportListViewSort;
const
  MapIndexToSortType: array[0..3] of TJclPeExportSort =
    (esName, esOrdinal, esHint, esAddrOrFwd);
begin
  with ExportListView do
  begin
    FExportViewImage.ExportList.SortList(MapIndexToSortType[Tag and $FF], Tag and $100 <> 0);
    Invalidate;
  end;
end;

procedure TFileViewerChild.UpdateModulesView;
var
  I: Integer;
  ExeImage: TJclPeImage;
  VI: TJclFileVersionInfo;
begin
  with ModulesListView.Items do
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to FModulesList.Count - 1 do
        with Add, FModulesList do
        begin
          ExeImage := FPeImagesCache.Images[ModuleToFilename(Strings[I])];
          Caption := ExtractFileName(ExeImage.FileName);
          Data := Objects[I];
          if ExeImage.Status = stOk then
          begin
            VI := ExeImage.VersionInfo;
            with ExeImage.FileProperties, SubItems do
            begin
              Add(FormatDateTime('ddddd tt', LastWriteTime));
              Add(Format('%.0n', [IntToExtended(Size)]));
            end;
            with ExeImage, SubItems do
            begin
              Add(HeaderValues[JclPeHeader_Subsystem]);
              Add(HeaderValues[JclPeHeader_ImageBase]);
              if Assigned(VI) then Add(VI.FileVersion) else Add('');
              if Assigned(VI) then Add(VI.ProductVersion) else Add('');
              Add(HeaderValues[JclPeHeader_ImageVersion]);
              Add(HeaderValues[JclPeHeader_LinkerVersion]);
              Add(HeaderValues[JclPeHeader_OperatingSystemVersion]);
              Add(HeaderValues[JclPeHeader_SubsystemVersion]);
              if Assigned(VI) then Add(VI.FileDescription) else Add('');
            end;
          end;
          ImageIndex := ModuleImages[TPeModuleState(Objects[I])].ImageIndex; 
        end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TFileViewerChild.ModulesListViewInfoTip(Sender: TObject; Item: TListItem; var InfoTip: String);
begin
  with Item.SubItems do
    if Count > 10 then
      InfoTip := Strings[5] + #13#10 + Strings[10]
    else
      InfoTip := '';
end;

function TFileViewerChild.ModuleToFileName(const ModuleName: string): TFileName;
begin
  Result := TJclPeImage.ExpandBySearchPath(ModuleName, FBasePath);
end;

function TFileViewerChild.GetSelectedFileName: TFileName;
var
  S: string;
begin
  S := '';
  if ActiveControl = DependencyTreeView then
  begin
    with DependencyTreeView do
      if Selected <> nil then 
        if Selected.Level = 0 then S := FFileName else
          S := Selected.Text;
  end else
  if Activecontrol = ModulesListView then
    with ModulesListView do
      if Selected <> nil then
        S := Selected.Caption;
  Result := ModuleToFileName(S);
end;

procedure TFileViewerChild.ExportListViewDblClick(Sender: TObject);
begin
  MainForm.Win32Help1.Execute;
end;

procedure TFileViewerChild.ModulesListViewDblClick(Sender: TObject);
begin
  MainForm.DumpPe1.Execute;
end;

end.
