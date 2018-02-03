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
{ The Original Code is PeDump.pas.                                                                 }
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

unit PeDump;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JclPeImage, ComCtrls, ExtCtrls, Menus;

type
  TPeDumpViewCategory = (vcHeader, vcDirectory, vcSection, vcLoadConfig,
    vcImport, vcExport, vcResource, vcRelocation, vcDebug);

  TPeDumpChild = class(TForm)
    SectionTreeView: TTreeView;
    Splitter1: TSplitter;
    PageControl1: TPageControl;
    ItemsTab: TTabSheet;
    DirectoryTab: TTabSheet;
    ItemsListView: TListView;
    DirectoryListView: TListView;
    ImportTab: TTabSheet;
    ImportListView: TListView;
    ExportTab: TTabSheet;
    ExportListView: TListView;
    PopupMenu1: TPopupMenu;
    Copytoclipboard1: TMenuItem;
    Selectall1: TMenuItem;
    N1: TMenuItem;
    Openlibrary1: TMenuItem;
    FindinWin32APIhelp1: TMenuItem;
    ResourceTab: TTabSheet;
    ResourceListView: TListView;
    SectionTab: TTabSheet;
    SectionListView: TListView;
    ResourceDirTab: TTabSheet;
    ResourceDirListView: TListView;
    ExportStatusBar: TStatusBar;
    ImportStatusBar: TStatusBar;
    RelocTab: TTabSheet;
    RelocListView: TListView;
    RelocStatusBar: TStatusBar;
    DebugTab: TTabSheet;
    DebugListView: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ItemsListViewData(Sender: TObject; Item: TListItem);
    procedure SectionTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure DirectoryListViewData(Sender: TObject; Item: TListItem);
    procedure ImportListViewColumnClick(Sender: TObject;
      Column: TListColumn);
    procedure ImportListViewData(Sender: TObject; Item: TListItem);
    procedure FormDestroy(Sender: TObject);
    procedure ExportListViewData(Sender: TObject; Item: TListItem);
    procedure ExportListViewColumnClick(Sender: TObject;
      Column: TListColumn);
    procedure SectionTreeViewDblClick(Sender: TObject);
    procedure SectionListViewData(Sender: TObject; Item: TListItem);
    procedure ResourceListViewData(Sender: TObject; Item: TListItem);
    procedure ResourceDirListViewData(Sender: TObject; Item: TListItem);
    procedure ImportListViewDblClick(Sender: TObject);
    procedure DirectoryListViewCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure SectionTreeViewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure RelocListViewData(Sender: TObject; Item: TListItem);
    procedure DebugListViewData(Sender: TObject; Item: TListItem);
    procedure ItemsListViewInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: String);
  private
    FCurrentResourceDirectory: TJclPeResourceItem;
    FCurrentImportIndex: Integer;
    FCurrentRelocationIndex: Integer;
    FOriginalPageControlWndProc: TWndMethod;
    FPeImage: TJclPeImage;
    FGroupImports: Boolean;
    FUpdatingView: Boolean;
    FUnmangleNames: Boolean;
    function GetFileName: TFileName;
    function GetHasDirectory(const Directory: DWORD): Boolean;
    function GetNodeCategory(Node: TTreeNode): TPeDumpViewCategory;
    procedure ExportListViewSort;
    procedure ImportListViewSort;
    function IsListViewActiveAndFocused(ListView: TListView): Boolean;
    procedure PageControlWndProc(var Message: TMessage);
    procedure UpdateView;
    procedure UpdateImportView(Node: TTreeNode);
    procedure UpdateRelocationView(Node: TTreeNode);
    procedure UpdateResourceDir;
    procedure UpdateResourceView(Directory: TJclPeResourceItem);
    class procedure UpdateSortData(Column: TListColumn);
    procedure SetGroupImports(const Value: Boolean);
    procedure SetUnmangleNames(const Value: Boolean);
    function FunctionName(const Name: string): string;
    function HeadersRemark(HeaderItem: TJclPeHeader): string;
  public
    constructor CreateEx(AOwner: TComponent; APeImage: TJclPeImage);
    function ActiveLibName: string;
    function ActiveWin32Function: string;
    property FileName: TFileName read GetFileName;
    property HasDirectory[const Directory: DWORD]: Boolean read GetHasDirectory;
    property GroupImports: Boolean read FGroupImports write SetGroupImports;
    property PeImage: TJclPeImage read FPeImage;
    property UnmangleNames: Boolean read FUnmangleNames write SetUnmangleNames;
  end;

var
  PeDumpChild: TPeDumpChild;

implementation

{$R *.DFM}

uses
  CommCtrl, PeViewerMain, ToolsUtils, PeResource, JclStrings, JclWin32;

resourcestring
  RsHeader = 'Header';
  RsDirectory = 'Directory';
  RsSection = 'Sections';
  RsLoadConfig = 'Load config';
  RsImport = 'Imports';
  RsExport = 'Exports';
  RsRelocation = 'Relocations';
  RsResource = 'Resources';
  RsDebug = 'Debug';
  RsNumberOfNames = 'Names: %d';
  RsNumberOfFunctions = 'Functions: %d';
  RsLinkerProducer = 'Linker: %s';
  RsOrdinalBase = 'Ordinal base: %d';
  RsAddresses = 'Addresses: %d';

function GetCategoryName(Category: TPeDumpViewCategory): string;
begin
  case Category of
    vcHeader: Result := RsHeader;
    vcDirectory: Result := RsDirectory;
    vcSection: Result := RsSection;
    vcLoadConfig: Result := RsLoadConfig;
    vcImport: Result := RsImport;
    vcExport: Result := RsExport;
    vcResource: Result := RsResource;
    vcRelocation: Result := RsRelocation;
    vcDebug: Result := RsDebug;
  end;
end;

function ImageIndexFromImportKind(Kind: TJclPeImportKind): Integer;
begin
  case Kind of
    ikImport:
      Result := icoImports;
    ikDelayImport:
      Result := icoDelayImport;
    ikBoundImport:
      Result := icoBoundImport;
  else
    Result := 0;
  end;
end;

{ TPeDumpChild }

function TPeDumpChild.ActiveLibName: string;
begin
  with SectionTreeView do
    if (Selected <> nil) and (Selected.Level = 1) and
      (TPeDumpViewCategory(Selected.Parent.Data) = vcImport) then
      Result := FPeImage.ExpandBySearchPath(Selected.Text, ExtractFilePath(FileName))
    else
      Result := '';
end;

function TPeDumpChild.ActiveWin32Function: string;
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
    Result := StrRemoveChars(Result, ['[', ']']);  
end;

constructor TPeDumpChild.CreateEx(AOwner: TComponent; APeImage: TJclPeImage);
begin
  inherited Create(AOwner);
  FPeImage := APeImage;
  Caption := ExtractFileName(FileName);
  {$IFDEF COMPILER5_UP}
  ItemsListView.OnInfoTip := ItemsListViewInfoTip;
  {$ENDIF COMPILER5_UP}
end;

function TPeDumpChild.GetFileName: TFileName;
begin
  if FPeImage = nil then Result := '' else Result := FPeImage.FileName;
end;

function TPeDumpChild.GetHasDirectory(const Directory: DWORD): Boolean;
begin
  if FPeImage = nil then
    Result := False
  else
    Result := FPeImage.DirectoryExists[Directory];
end;

procedure TPeDumpChild.PageControlWndProc(var Message: TMessage);
begin
// remove PageControl's border
  FOriginalPageControlWndProc(Message);
  with Message do
    if (Msg = TCM_ADJUSTRECT) and (Message.WParam = 0) then
      InflateRect(PRect(LParam)^, 4, 4);
end;

procedure TPeDumpChild.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  with PageControl1 do
  begin
    for I := 0 to PageCount - 1 do Pages[I].TabVisible := False;
    FOriginalPageControlWndProc := WindowProc;
    WindowProc := PageControlWndProc;
    ActivePage := ItemsTab;
    Realign;
  end;

  ImportListView.Tag := $100;
  UpdateSortData(ImportListView.Columns[0]);
  ExportListView.Tag := $100;
  UpdateSortData(ExportListView.Columns[0]);

  UpdateView;
end;

procedure TPeDumpChild.FormClose(Sender: TObject; var Action: TCloseAction);
var
  F: TForm;
begin
  Fix_ListViewBeforeClose(Self);
  F := MainForm.FindPeResourceView(FPeImage);
  if F <> nil then F.Close;
  Action := caFree;
end;

procedure TPeDumpChild.UpdateView;

  procedure BuildImageTree;
  var
    Category: TPeDumpViewCategory;
    TempNode: TTreeNode;

    function AddCategoryNode(ImageIndex: Integer): TTreeNode;
    begin
      Result := SectionTreeView.Items.AddChildObject(nil, GetCategoryName(Category),
        Pointer(Category));
      Result.ImageIndex := ImageIndex;
      Result.SelectedIndex := ImageIndex;
    end;

  begin
    FPeImage.TryGetNamesForOrdinalImports;
    with SectionTreeView do
    begin
      Items.BeginUpdate;
      try
        Items.Clear;
        for Category := Low(Category) to High(Category) do
          case Category of
            vcHeader:
              AddCategoryNode(icoHeader);
            vcDirectory:
              AddCategoryNode(icoDirectory);
            vcSection:
              AddCategoryNode(icoSection);
            vcLoadConfig:
              if FPeImage.DirectoryExists[IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG] then
                AddCategoryNode(icoLoadConfig);
            vcImport:
              if FPeImage.DirectoryExists[IMAGE_DIRECTORY_ENTRY_IMPORT] then
              begin
                TempNode := AddCategoryNode(icoImports);
                TempNode.HasChildren := True;
              end;
            vcExport:
              if FPeImage.DirectoryExists[IMAGE_DIRECTORY_ENTRY_EXPORT] then
                AddCategoryNode(icoExports);
            vcRelocation:
              if FPeImage.DirectoryExists[IMAGE_DIRECTORY_ENTRY_BASERELOC] then
              begin
                TempNode := AddCategoryNode(icoRelocation);
                TempNode.HasChildren := True;
              end;
            vcResource:
              if FPeImage.DirectoryExists[IMAGE_DIRECTORY_ENTRY_RESOURCE] then
              begin
                TempNode := AddCategoryNode(icoResources);
                TempNode.HasChildren := True;
              end;
            vcDebug:
              if FPeImage.DirectoryExists[IMAGE_DIRECTORY_ENTRY_DEBUG] then
                AddCategoryNode(icoDebug);
          end;
        Selected := Items.GetFirstNode;
      finally
        Items.EndUpdate;
      end;
    end;
  end;

begin
  BuildImageTree;

  with DirectoryListView do
  begin
    Items.Count := IMAGE_NUMBEROF_DIRECTORY_ENTRIES;
    ItemFocused := Items[0];
  end;
  with SectionListView do
  begin
    Items.Count := FPeImage.ImageSectionCount;
    if Items.Count > 0 then ItemFocused := Items[0];
  end;
  ExportListView.Items.Count := FPeImage.ExportList.Count;
  UpdateResourceDir;
  with ExportStatusBar, FPeImage.ExportList do
  begin
    Panels[0].Text := Format(RsNumberOfNames, [Count]);
    Panels[1].Text := Format(RsNumberOfFunctions, [FunctionCount]);
    Panels[2].Text := Format(RsOrdinalBase, [Base]);
  end;
end;

procedure TPeDumpChild.ItemsListViewData(Sender: TObject; Item: TListItem);
begin
  with Item, FPeImage do
    case TListView(Sender).Tag of
      0: begin
           Caption := HeaderNames(TJclPeHeader(Index));
           SubItems.Add(HeaderValues[TJclPeHeader(Index)]);
         end;
      1: begin
           Caption := LoadConfigNames(TJclLoadConfig(Index));
           SubItems.Add(LoadConfigValues[TJclLoadConfig(Index)]);
         end;
   end;
end;

procedure TPeDumpChild.SectionTreeViewExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var
  I: Integer;
  TempNode: TTreeNode;
  ResItem: TJclPeResourceItem;
begin
  if Node.GetFirstChild = nil then with SectionTreeView do 
  begin
    Items.BeginUpdate;
    case GetNodeCategory(Node) of
      vcImport:
        if GroupImports then
        begin
          for I := 0 to FPeImage.ImportList.UniqueLibItemCount  - 1 do
            with Items.AddChild(Node, FPeImage.ImportList.UniqueLibNames[I]) do
            begin
              Data := Pointer(-1);
              ImageIndex := ImageIndexFromImportKind(FPeImage.ImportList.UniqueLibItems[I].ImportKind);
              SelectedIndex := ImageIndex;
            end;
        end else
        begin
//        FPeImage.ImportList.SortList(ilName);
          for I := 0 to FPeImage.ImportList.Count - 1 do
            with Items.AddChild(Node, FPeImage.ImportList[I].Name) do
            begin
              Data := Pointer(FPeImage.ImportList[I].ImportDirectoryIndex);
              ImageIndex := ImageIndexFromImportKind(FPeImage.ImportList[I].ImportKind);
              SelectedIndex := ImageIndex;
            end;
        end;
      vcResource:
        if Node.Level = 0 then
          for I := 0 to FPeImage.ResourceList.Count - 1 do
          begin
            ResItem := FPeImage.ResourceList[I];
            TempNode := Items.AddChildObject(Node, ResItem.ResourceTypeStr, ResItem);
            TempNode.ImageIndex := icoResources;
            TempNode.SelectedIndex := TempNode.ImageIndex;
            TempNode.HasChildren := True;
          end
        else
        begin
          ResItem := TJclPeResourceItem(Node.Data);
          for I := 0 to ResItem.List.Count - 1 do
            with Items.AddChildObject(Node, ResItem.List[I].Name, ResItem.List[I]) do
            begin
              ImageIndex := icoResources;
              SelectedIndex := ImageIndex;
            end;
        end;
      vcRelocation:
        for I := 0 to FPeImage.RelocationList.Count - 1 do
          with Items.AddChildObject(Node,
            Format('%.8x', [FPeImage.RelocationList[I].VirtualAddress]), Pointer(I)) do
          begin
            ImageIndex := icoRelocation;
            SelectedIndex := ImageIndex;
          end;
    end;
    Items.EndUpdate;
  end;
end;

procedure TPeDumpChild.SectionTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  if FUpdatingView then Exit;
  case GetNodeCategory(Node) of
    vcHeader:
      begin
        ItemsListView.Items.Count := Integer(High(TJclPeHeader)) + 1;
        ItemsListView.Tag := 0; // Header items
        ItemsListView.Invalidate;
        PageControl1.ActivePage := ItemsTab;
      end;
    vcDirectory: PageControl1.ActivePage := DirectoryTab;
    vcSection: PageControl1.ActivePage := SectionTab;
    vcLoadConfig:
      begin
        ItemsListView.Items.Count := Integer(High(TJclLoadConfig)) + 1;
        ItemsListView.Tag := 1; // Load config items
        ItemsListView.Invalidate;
        PageControl1.ActivePage := ItemsTab;
      end;
    vcImport:
      begin
        if Node.Level = 0 then UpdateImportView(nil) else UpdateImportView(Node);
        PageControl1.ActivePage := ImportTab;
      end;
    vcExport:
      PageControl1.ActivePage := ExportTab;
    vcRelocation:
      begin
        UpdateRelocationView(Node);
        PageControl1.ActivePage := RelocTab;
      end;
    vcResource:
      if Node.Level = 0 then
      begin
        UpdateResourceDir;
        PageControl1.ActivePage := ResourceDirTab;
      end else
      begin
        UpdateResourceView(TJclPeResourceItem(Node.Data));
        PageControl1.ActivePage := ResourceTab;
      end;
    vcDebug:
      begin
        DebugListView.Items.Count := FPeImage.DebugList.Count;
        PageControl1.ActivePage := DebugTab;
      end;
  end;
end;

procedure TPeDumpChild.DirectoryListViewData(Sender: TObject; Item: TListItem);
const
  DirectoryIcons: array[0..15] of Integer =
    (icoExports, icoImports, icoResources, -1, -1, icoRelocation, icoDebug,
     -1, -1, -1, icoLoadConfig, icoBoundImport, -1, icoDelayImport, -1, -1);
var
  Percent: Single;
begin
  if FPeImage.Target = taWin64 then
  begin
    with Item, FPeImage.OptionalHeader64 do
    begin
      Percent := DataDirectory[Index].Size * 100 / SizeOfImage;
      Caption := FPeImage.DirectoryNames(Index);
      Data := Pointer(DataDirectory[Index].Size);
      if Integer(Data) <> 0 then ImageIndex := DirectoryIcons[Index];
      SubItems.Add(Format('%.8x', [DataDirectory[Index].VirtualAddress]));
      SubItems.Add(Format('%.8x', [DataDirectory[Index].Size]));
      SubItems.Add(Format('%3.1f%%', [Percent]));
      SubItems.Add(FPeImage.ImageSectionNameFromRva[DataDirectory[Index].VirtualAddress]);
    end;
  end
  else
  begin
    with Item, FPeImage.OptionalHeader32 do
    begin
      Percent := DataDirectory[Index].Size * 100 / SizeOfImage;
      Caption := FPeImage.DirectoryNames(Index);
      Data := Pointer(DataDirectory[Index].Size);
      if Integer(Data) <> 0 then ImageIndex := DirectoryIcons[Index];
      SubItems.Add(Format('%.8x', [DataDirectory[Index].VirtualAddress]));
      SubItems.Add(Format('%.8x', [DataDirectory[Index].Size]));
      SubItems.Add(Format('%3.1f%%', [Percent]));
      SubItems.Add(FPeImage.ImageSectionNameFromRva[DataDirectory[Index].VirtualAddress]);
    end;
  end;
end;

class procedure TPeDumpChild.UpdateSortData(Column: TListColumn);
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
    Column.ImageIndex := icoSortDesc
  else
    Column.ImageIndex := icoSortAsc;
  ListView.Columns.EndUpdate;
end;

procedure TPeDumpChild.ImportListViewColumnClick(Sender: TObject; Column: TListColumn);
begin
  UpdateSortData(Column);
  ImportListViewSort;
end;

procedure TPeDumpChild.UpdateImportView(Node: TTreeNode);
const
  LinkerProducers: array[TJclPeLinkerProducer] of string =
    ('Borland', 'Microsoft');
begin
  FCurrentImportIndex := -1;
  if Node = nil then
  begin
    FPeImage.ImportList.FilterModuleName := '';
    ImportListView.Items.Count := FPeImage.ImportList.AllItemCount;
  end else
  if Integer(Node.Data) = -1 then
  begin
    FPeImage.ImportList.FilterModuleName := Node.Text;
    ImportListView.Items.Count := FPeImage.ImportList.AllItemCount;
  end else
  begin
    FCurrentImportIndex := Integer(Node.Data);
    ImportListView.Items.Count := FPeImage.ImportList[FCurrentImportIndex].Count;
  end;
  ImportListViewSort;
  ImportListView.Invalidate;
  with ImportStatusBar, FPeImage.ImportList do
  begin
    Panels[0].Text := Format(RsNumberOfFunctions, [ImportListView.Items.Count]);
    Panels[1].Text := Format(RsLinkerProducer, [LinkerProducers[LinkerProducer]]);
  end;
end;

procedure TPeDumpChild.ImportListViewData(Sender: TObject; Item: TListItem);
var
  ViewItem: TJclPeImportFuncItem;
begin
  if FCurrentImportIndex = -1 then
    ViewItem := FPeImage.ImportList.AllItems[Item.Index]
  else
    ViewItem := FPeImage.ImportList[FCurrentImportIndex][Item.Index];
  with Item, ViewItem do
  begin
    if IndirectImportName then
      Caption := Format('[%s]', [Name])
    else
      Caption := FunctionName(Name);
    if IsByOrdinal then
    begin
      SubItems.Add(Format('%d', [Ordinal]));
      SubItems.Add('');
    end else
    begin
      SubItems.Add('');
      SubItems.Add(Format('%d', [Hint]));
    end;
    SubItems.Add(ImportLib.Name);
    ImageIndex := ImageIndexFromImportKind(ImportLib.ImportKind);
  end;
end;

procedure TPeDumpChild.FormDestroy(Sender: TObject);
begin
  FPeImage.Free;
end;

procedure TPeDumpChild.ExportListViewData(Sender: TObject; Item: TListItem);
begin
  with Item, FPeImage.ExportList[Item.Index] do
  begin
    Caption := FunctionName(Name);
    SubItems.Add(Format('%d', [Ordinal]));
    SubItems.Add(Format('%d', [Hint]));
    SubItems.Add(Format('%.8x', [Address]));
    SubItems.Add(ForwardedName);
    SubItems.Add(SectionName);
    ImageIndex := 3;
  end;
end;

procedure TPeDumpChild.ExportListViewColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  UpdateSortData(Column);
  ExportListViewSort;
end;

function TPeDumpChild.IsListViewActiveAndFocused(ListView: TListView): Boolean;
begin
  Result := (ActiveControl = ListView) and (ListView.ItemFocused <> nil);
end;

procedure TPeDumpChild.SectionTreeViewDblClick(Sender: TObject);
begin
  MainForm.OpenLibrary1.Execute;
end;

procedure TPeDumpChild.SectionListViewData(Sender: TObject; Item: TListItem);
var
  Percent: Single;
begin
  with FPeImage, Item do
  begin
    Caption := ImageSectionNames[Item.Index];
    with ImageSectionHeaders[Item.Index] do
    begin
      if FPeImage.Target = taWin64 then
        Percent := SizeOfRawData * 100 / OptionalHeader64.SizeOfImage
      else
        Percent := SizeOfRawData * 100 / OptionalHeader32.SizeOfImage;
      SubItems.Add(Format('%.8x', [Misc.VirtualSize]));
      SubItems.Add(Format('%.8x', [VirtualAddress]));
      SubItems.Add(Format('%.8x', [SizeOfRawData]));
      SubItems.Add(Format('%.8x', [PointerToRawData]));
      SubItems.Add(Format('%.8x', [Characteristics]));
      SubItems.Add(ShortSectionInfo(Characteristics));
      SubItems.Add(Format('%3.1f%%', [Percent]));
    end;
  end;
end;

procedure TPeDumpChild.UpdateResourceView(Directory: TJclPeResourceItem);
begin
  ResourceListView.Items.Count := 0;
  FCurrentResourceDirectory := Directory;
  ResourceListView.Items.Count := Directory.List.Count;
  ResourceListView.Invalidate;
end;

procedure TPeDumpChild.ResourceListViewData(Sender: TObject; Item: TListItem);
var
  DirSize, I: Integer;
begin
  with Item, FCurrentResourceDirectory.List[Item.Index] do
  begin
    if IsDirectory then
    begin
      Caption := Name;
      if (List.Count = 1) and (StrToIntDef(List[0].Name, 0) = LANG_NEUTRAL) then
      begin // only neutral language
        DirSize := List[0].DataEntry^.Size;
        SubItems.Add(Format('(%x)', [List[0].DataEntry^.OffsetToData]));
      end else
      begin
        DirSize := 0;
        for I := 0 to List.Count - 1 do
          Inc(DirSize, List[I].DataEntry^.Size);
        SubItems.Add('');
      end;  
      SubItems.Add(Format('%x', [DirSize]));
      SubItems.Add(Format('%d', [List.Count]));
    end else
    begin 
      Caption := Format('%s (%s)', [ParentItem.Name, Name]);
      SubItems.Add(Format('%x', [DataEntry^.OffsetToData]));
      SubItems.Add(Format('%x', [DataEntry^.Size]));
      SubItems.Add(LangNameFromName(Name));
    end;
  end;
end;

procedure TPeDumpChild.UpdateResourceDir;
begin
  ResourceDirListView.Items.Count := FPeImage.ResourceList.Count;
  ResourceDirListView.Invalidate;
end;

procedure TPeDumpChild.ResourceDirListViewData(Sender: TObject; Item: TListItem);
begin
  with Item, FPeImage.ResourceList[Item.Index] do
  begin
    Caption := ResourceTypeStr;
    SubItems.Add(Format('%d', [List.Count]));
  end;
end;

procedure TPeDumpChild.UpdateRelocationView(Node: TTreeNode);
begin
  if Node.Level = 0 then
  begin
    FCurrentRelocationIndex := -1;
    RelocListView.Items.Count := FPeImage.RelocationList.AllItemCount;
  end else
  begin
    FCurrentRelocationIndex := Integer(Node.Data);
    RelocListView.Items.Count := FPeImage.RelocationList[FCurrentRelocationIndex].Count;
  end;
  RelocStatusBar.Panels[0].Text := Format(RsAddresses, [RelocListView.Items.Count]);
  RelocListView.Invalidate;
end;

procedure TPeDumpChild.RelocListViewData(Sender: TObject; Item: TListItem);
var
  ViewItem: TJclPeRelocation;

  function RelocationTypeStr(RelocType: Byte): string;
begin
  case RelocType of
    IMAGE_REL_BASED_ABSOLUTE: Result := 'ABSOLUTE';
    IMAGE_REL_BASED_HIGHLOW: Result := 'HIGHLOW';
  else
    Result := IntToStr(RelocType);
  end;
end;

begin
  if FCurrentRelocationIndex = -1 then
    ViewItem := FPeImage.RelocationList.AllItems[Item.Index]
  else
    ViewItem := FPeImage.RelocationList[FCurrentRelocationIndex][Item.Index];
  with Item, ViewItem do
  begin
    Caption := Format('%.8x', [VirtualAddress]);
    SubItems.Add(RelocationTypeStr(RelocType));
  end;
end;

procedure TPeDumpChild.DebugListViewData(Sender: TObject; Item: TListItem);
begin
  with Item, FPeImage.DebugList[Item.Index] do
  begin
    Caption := FPeImage.DebugTypeNames(_Type);
    SubItems.Add(Format('%.8x', [SizeOfData]));
    SubItems.Add(Format('%.8x', [AddressOfRawData]));
    SubItems.Add(Format('%.8x', [PointerToRawData]));
    SubItems.Add(Format('%d.%.2d', [MajorVersion, MinorVersion]));
  end;  
end;

procedure TPeDumpChild.ImportListViewDblClick(Sender: TObject);
begin
  MainForm.InvokeHelp1.Execute;
end;

procedure TPeDumpChild.DirectoryListViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Integer(Item.Data) = 0 then Sender.Canvas.Font.Color := clGrayText;
end;

procedure TPeDumpChild.SetGroupImports(const Value: Boolean);
var
  NodeIndex: Integer;
  TempNode: TTreeNode;
  WasExpanded: Boolean;
begin
  if FGroupImports <> Value then
  begin
    FGroupImports := Value;
    with SectionTreeView do
    begin
      Items.BeginUpdate;
      FUpdatingView := True;
      try
        if Assigned(Selected) then
        begin
          if Selected.Level > 0 then
          begin
            NodeIndex := Selected.Parent.Index;
            WasExpanded := True;
          end else
          begin
            NodeIndex := Selected.Index;
            WasExpanded := Selected.Expanded;
          end;
        end else
        begin
          NodeIndex := 0;
          WasExpanded := False;
        end;
        Self.UpdateView;
        TempNode := Items.GetFirstNode;
        while NodeIndex > 0 do
        begin
          TempNode := TempNode.GetNextSibling;
          Dec(NodeIndex);
        end;
        FUpdatingView := False;
        Selected := TempNode;
        if WasExpanded then Selected.Expand(False);
      finally
        Items.EndUpdate;
      end;
    end;
  end;
end;

procedure TPeDumpChild.ImportListViewSort;
const
  MapIndexToSortType: array[0..3] of TJclPeImportSort = (isName, isOrdinal, isHint, isLibImport);
begin
  with ImportListView do
  begin
    if FCurrentImportIndex = -1 then
      FPeImage.ImportList.SortAllItemsList(MapIndexToSortType[Tag and $FF], Tag and $100 <> 0)
    else
      FPeImage.ImportList[FCurrentImportIndex].SortList(MapIndexToSortType[Tag and $FF], Tag and $100 <> 0);
    Invalidate;
  end;
end;

procedure TPeDumpChild.ExportListViewSort;
const
  MapIndexToSortType: array[0..5] of TJclPeExportSort =
    (esName, esOrdinal, esHint, esAddress, esForwarded, esSection);
begin
  with ExportListView do
  begin
    FPeImage.ExportList.SortList(MapIndexToSortType[Tag and $FF], Tag and $100 <> 0);
    Invalidate;
  end;
end;

function TPeDumpChild.GetNodeCategory(Node: TTreeNode): TPeDumpViewCategory;
begin
  while Node.Parent <> nil do Node := Node.Parent;
  Result := TPeDumpViewCategory(Node.Data);
end;

procedure TPeDumpChild.SetUnmangleNames(const Value: Boolean);
begin
  if FUnmangleNames <> Value then
  begin
    FUnmangleNames := Value;
    ImportListView.Invalidate;
    ExportListView.Invalidate;
  end;
end;

function TPeDumpChild.FunctionName(const Name: string): string;
begin
  if FUnmangleNames then
    PeUnmangleName(Name, Result)
  else
    Result := Name;
end;

function TPeDumpChild.HeadersRemark(HeaderItem: TJclPeHeader): string;
const
  ImageCharacteristicValues: array [1..14] of packed record
      Value: Word;
      Name: PChar;
    end = (
    (Value: IMAGE_FILE_RELOCS_STRIPPED; Name: 'RELOCS_STRIPPED'),
    (Value: IMAGE_FILE_EXECUTABLE_IMAGE; Name: 'EXECUTABLE_IMAGE'),
    (Value: IMAGE_FILE_LINE_NUMS_STRIPPED; Name: 'LINE_NUMS_STRIPPED'),
    (Value: IMAGE_FILE_LOCAL_SYMS_STRIPPED; Name: 'LOCAL_SYMS_STRIPPED'),
    (Value: IMAGE_FILE_AGGRESIVE_WS_TRIM; Name: 'AGGRESIVE_WS_TRIM'),
    (Value: IMAGE_FILE_BYTES_REVERSED_LO; Name: 'BYTES_REVERSED_LO'),
    (Value: IMAGE_FILE_32BIT_MACHINE; Name: '32BIT_MACHINE'),
    (Value: IMAGE_FILE_DEBUG_STRIPPED; Name: 'DEBUG_STRIPPED'),
    (Value: IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP; Name: 'REMOVABLE_RUN_FROM_SWAP'),
    (Value: IMAGE_FILE_NET_RUN_FROM_SWAP; Name: 'NET_RUN_FROM_SWAP'),
    (Value: IMAGE_FILE_SYSTEM; Name: 'SYSTEM'),
    (Value: IMAGE_FILE_DLL; Name: 'DLL'),
    (Value: IMAGE_FILE_UP_SYSTEM_ONLY; Name: 'UP_SYSTEM_ONLY'),
    (Value: IMAGE_FILE_BYTES_REVERSED_HI; Name: 'BYTES_REVERSED_HI')
  );
var
  C: Word;
  I: Integer;
begin
  case HeaderItem of
    JclPeHeader_Characteristics:
      begin
        Result := '';
        C := FPeImage.LoadedImage.FileHeader.FileHeader.Characteristics;
        for I := Low(ImageCharacteristicValues) to High(ImageCharacteristicValues) do
          if C and ImageCharacteristicValues[I].Value <> 0 then
            Result := Result + #13#10 + ImageCharacteristicValues[I].Name;
        Delete(Result, 1, 2);
      end;
  else
    Result := '';
  end;
end;

procedure TPeDumpChild.ItemsListViewInfoTip(Sender: TObject;
  Item: TListItem; var InfoTip: String);
begin
  case TListView(Sender).Tag of
    0: InfoTip := HeadersRemark(TJclPeHeader(Item.Index));
  end;
end;

end.
