{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is ProjAnalyzerFrm.pas.                                                        }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclProjectAnalyzerFrm;

interface

{$I jcl.inc}

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs,
  ComCtrls, ActnList, Menus, ClipBrd, ImgList, ToolWin,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclDebug,
  JclOtaUtils;

type
  TUnitItem = record
    Name: string;
    Size: Integer;
    Group: string;
  end;

  TPackageUnitItem = record
    UnitName: string;
    PackageName: string;
  end;

  TProjectAnalyserView = (pavDetails, pavSummary, pavDfms);

  TProjectAnalyzerForm = class(TForm)
    UnitListView: TListView;
    ExplorerItemImages: TImageList;
    ToolBarMain: TToolBar;
    ActionListProjectAnalyser: TActionList;
    PopupMenuUnitView: TPopupMenu;
    ToolButtonDetails: TToolButton;
    ActionShowDetails: TAction;
    ActionShowSummary: TAction;
    MenuItemDetails: TMenuItem;
    MenuItemSummary: TMenuItem;
    ToolButtonSummary: TToolButton;
    ToolButtonSeparator1: TToolButton;
    ToolButtonCopy: TToolButton;
    ToolButtonSave: TToolButton;
    ActionCopy: TAction;
    ActionSave: TAction;
    PopupMenuToolbar: TPopupMenu;
    TextLabelsItem: TMenuItem;
    MenuItemSeparator: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemSave: TMenuItem;
    SaveDialogProjectAnalyser: TSaveDialog;
    StatusBarMain: TStatusBar;
    ActionShowDfms: TAction;
    ToolButtonDfms: TToolButton;
    MenuItemDfms: TMenuItem;
    ToolButtonSeparator2: TToolButton;
    ToolButtonShowPackages: TToolButton;
    ActionShowPackages: TAction;
    procedure ActionShowDfmsUpdate(Sender: TObject);
    procedure ActionShowSummaryUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure UnitListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure UnitListViewCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ActionShowDetailsExecute(Sender: TObject);
    procedure ActionShowSummaryExecute(Sender: TObject);
    procedure TextLabelsItemClick(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionShowDfmsExecute(Sender: TObject);
    procedure ActionShowDetailsUpdate(Sender: TObject);
    procedure ActionShowPackagesExecute(Sender: TObject);
    procedure ActionShowPackagesUpdate(Sender: TObject);
  private
    FCodeSize: Integer;
    FICodeSize: Integer;
    FDataSize: Integer;
    FBssSize: Integer;
    FPackageUnits: array of TPackageUnitItem;
    FUnits: array of TUnitItem;
    FDfms: array of TUnitItem;
    FUnitsSum: TStringList;
    FSettings: TJclOtaSettings;
    FShowPackages: Boolean;
    FView: TProjectAnalyserView;
    procedure OnMapSegmentEvent(Sender: TObject; const Address: TJclMapAddress;
      Length: Integer; const ClassName, UnitName: string);
    procedure SetStatusBarText(const Value: string);
    procedure ClearData;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent; ASettings: TJclOtaSettings); reintroduce;
    destructor Destroy; override;
    procedure ClearContent;
    function FindPackageForUnitName(const UnitName: string): string;
    procedure ShowDfms;
    procedure ShowDetails;
    procedure ShowSummary;
    procedure SetFileName(const FileName, MapFileName: TFileName; const ProjectName: string);
    property StatusBarText: string write SetStatusBarText;
    property Settings: TJclOtaSettings read FSettings;
    property View: TProjectAnalyserView read FView;
    property ShowPackages: Boolean read FShowPackages;
  end;

var
  ProjectAnalyzerForm: TProjectAnalyzerForm;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\projectanalyser';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

{$R *.dfm}

uses
  JclLogic, JclOtaResources, JclPeImage, JclStrings,
  JclOtaConsts;

procedure JvListViewSortClick(Column: TListColumn; AscendingSortImage: Integer;
  DescendingSortImage: Integer);
var
  ListView: TListView;
  I: Integer;
begin
  ListView := TListColumns(Column.Collection).Owner as TListView;
  ListView.Columns.BeginUpdate;
  try
    for I := 0 to ListView.Columns.Count - 1 do
        ListView.Columns.Items[I].ImageIndex := -1;
    if ListView.Tag and $FF = Column.Index then
      ListView.Tag := ListView.Tag xor $100
    else
      ListView.Tag := Column.Index;
    if ListView.Tag and $100 = 0 then
      Column.ImageIndex := AscendingSortImage
    else
      Column.ImageIndex := DescendingSortImage;
  finally
    ListView.Columns.EndUpdate;
  end;
end;

procedure JvListViewCompare(ListView: TListView; Item1, Item2: TListItem; var Compare: Integer);
var
  ColIndex: Integer;

  function FmtStrToInt(S: string): Integer;
  var
    I: Integer;
  begin
    I := 1;
    while I <= Length(S) do
      if not CharIsNumberChar(S[I]) then
        Delete(S, I, 1)
      else
        Inc(I);
    Result := StrToInt(S);
  end;

begin
  with ListView do
  begin
    ColIndex := Tag and $FF - 1;
    if Columns[ColIndex + 1].Alignment = taLeftJustify then
    begin
      if ColIndex = -1 then
        Compare := AnsiCompareText(Item1.Caption, Item2.Caption)
      else
        Compare := AnsiCompareText(Item1.SubItems[ColIndex], Item2.SubItems[ColIndex]);
    end
    else
    begin
      if ColIndex = -1 then
        Compare := FmtStrToInt(Item1.Caption) - FmtStrToInt(Item2.Caption)
      else
        Compare := FmtStrToInt(Item1.SubItems[ColIndex]) - FmtStrToInt(Item2.SubItems[ColIndex]);
    end;
    if (Tag and $100) <> 0 then
      Compare := -Compare;
  end;
end;

procedure JvListViewToStrings(ListView: TListView; Strings: TStrings;
  SelectedOnly: Boolean; Headers: Boolean);
var
  R, C: Integer;
  ColWidths: array of Word;
  S: string;

  procedure AddLine;
  begin
    Strings.Add(TrimRight(S));
  end;

  function MakeCellStr(const Text: string; Index: Integer): string;
  begin
    with ListView.Columns[Index] do
      if Alignment = taLeftJustify then
        Result := StrPadRight(Text, ColWidths[Index] + 1)
      else
        Result := StrPadLeft(Text, ColWidths[Index]) + ' ';
  end;

begin
  SetLength(S, 256);
  with ListView do
  begin
    SetLength(ColWidths, Columns.Count);
    if Headers then
      for C := 0 to Columns.Count - 1 do
        ColWidths[C] := Length(Trim(Columns[C].Caption));
    for R := 0 to Items.Count - 1 do
      if not SelectedOnly or Items[R].Selected then
      begin
        ColWidths[0] := Max(ColWidths[0], Length(Trim(Items[R].Caption)));
        for C := 0 to Items[R].SubItems.Count - 1 do
          ColWidths[C + 1] := Max(ColWidths[C + 1], Length(Trim(Items[R].SubItems[C])));
      end;
    Strings.BeginUpdate;
    try
      if Headers then
        with Columns do
        begin
          S := '';
          for C := 0 to Count - 1 do
            S := S + MakeCellStr(Items[C].Caption, C);
          AddLine;
          S := '';
          for C := 0 to Count - 1 do
            S := S + StringOfChar('-', ColWidths[C]) + ' ';
          AddLine;
        end;
      for R := 0 to Items.Count - 1 do
        if not SelectedOnly or Items[R].Selected then
        with Items[R] do
        begin
          S := MakeCellStr(Caption, 0);
          for C := 0 to Min(SubItems.Count, Columns.Count - 1) - 1 do
            S := S + MakeCellStr(SubItems[C], C + 1);
          AddLine;
        end;
    finally
      Strings.EndUpdate;
    end;
  end;
end;

function IntToExtended(I: Integer): Extended;
begin
  Result := I;
end;

//=== { TProjectAnalyzerForm } ===============================================

procedure TProjectAnalyzerForm.FormCreate(Sender: TObject);
var
  Index: Integer;
begin
  FUnitsSum := TStringList.Create;
  FUnitsSum.Sorted := True;
  FUnitsSum.Duplicates := dupIgnore;

  SetBounds(Settings.LoadInteger(JclLeft, Left),
            Settings.LoadInteger(JclTop, Top),
            Settings.LoadInteger(JclWidth, Width),
            Settings.LoadInteger(JclHeight, Height));

  ActionCopy.Caption := LoadResString(@RsCopy);
  ActionSave.Caption := LoadResString(@RsSave);
  ActionShowDetails.Caption := LoadResString(@RsDetails);
  ActionShowSummary.Caption := LoadResString(@RsSummary);
  ActionShowDfms.Caption := LoadResString(@RsForms);
  ActionShowPackages.Caption := LoadResString(@RsShowPackages);
  TextLabelsItem.Caption := LoadResString(@RsTextLabels);
  UnitListView.Columns[0].Caption := LoadResString(@RsName);
  UnitListView.Columns[1].Caption := LoadResString(@RsSize);
  UnitListView.Columns[2].Caption := LoadResString(@RsGroup);
  UnitListView.Columns[3].Caption := LoadResString(@RsPackage);

  FView := TProjectAnalyserView(Settings.LoadInteger(AnalyzerViewName, Integer(pavDetails)));
  FShowPackages := Settings.LoadBool(AnalyzerShowPackagesName, True);

  for Index := 0 to UnitListView.Columns.Count - 1 do
    UnitListView.Columns.Items[Index].Width := Settings.LoadInteger(Format(ColumnRegName, [Index]),
      UnitListView.Columns.Items[Index].Width);
end;

procedure TProjectAnalyzerForm.FormDestroy(Sender: TObject);
var
  Index: Integer;
begin
  Settings.SaveInteger(JclLeft, Left);
  Settings.SaveInteger(JclTop, Top);
  Settings.SaveInteger(JclWidth, Width);
  Settings.SaveInteger(JclHeight, Height);
  Settings.SaveInteger(AnalyzerViewName, Integer(FView));
  Settings.SaveBool(AnalyzerShowPackagesName, ShowPackages);
  for Index := 0 to UnitListView.Columns.Count - 1 do
    Settings.SaveInteger(Format(ColumnRegName, [Index]), UnitListView.Columns.Items[Index].Width);

  FreeAndNil(FUnitsSum);
end;

procedure TProjectAnalyzerForm.SetFileName(const FileName, MapFileName: TFileName; const ProjectName: string);
var
  MapParser: TJclMapParser;
  BorImage: TJclPeBorImage;
  PackagesList: TStringList;
  I, U, C, ResourcesSize: Integer;
  ShortPackageName: string;
begin
  ClearData;
  Caption := Format(LoadResString(@RsFormCaption), [ProjectName]);
  MapParser := TJclMapParser.Create(MapFileName);
  try
    MapParser.OnSegment := OnMapSegmentEvent;
    MapParser.Parse;
  finally
    MapParser.Free;
  end;
  BorImage := TJclPeBorImage.Create(True);
  PackagesList := TStringList.Create;
  try
    PeImportedLibraries(FileName, PackagesList, False, True);
    C := 0;
    for I := 0 to PackagesList.Count - 1 do
    begin
      BorImage.FileName := PackagesList[I];
      if BorImage.IsPackage then
      begin
        ShortPackageName := ExtractFileName(PackagesList[I]);
        with BorImage.PackageInfo do
          for U := 0 to ContainsCount - 1 do
          begin
            SetLength(FPackageUnits, C + 1);
            FPackageUnits[C].UnitName := ContainsNames[U];
            FPackageUnits[C].PackageName := ShortPackageName;
            Inc(C);
          end;
      end;
    end;
    BorImage.FileName := FileName;
    ResourcesSize := BorImage.Directories[IMAGE_DIRECTORY_ENTRY_RESOURCE].Size;
    with BorImage do
    begin
      SetLength(FDfms, FormCount);
      for I := 0 to FormCount - 1 do
      begin
        FDfms[I].Name := Forms[I].FormObjectName;
        FDfms[I].Size := Forms[I].ResItem.RawEntryDataSize;
      end;
    end;
  finally
    BorImage.Free;
    PackagesList.Free;
  end;
  StatusBarMain.Panels[0].Text := Format(LoadResString(@RsStatusText),
    [FUnitsSum.Count, Length(FDfms), FCodeSize, FICodeSize, FDataSize, FBssSize, ResourcesSize]);
  case View of
    pavDetails:
      ShowDetails;
    pavSummary:
      ShowSummary;
    else
      ShowDfms;
  end;
end;

procedure TProjectAnalyzerForm.ShowDetails;
var
  I: Integer;
  PackageName: string;
  AItem: TListItem;
begin
  FView := pavDetails;
  UnitListView.Items.BeginUpdate;
  try
    UnitListView.Items.Clear;
    for I := 0 to Length(FUnits) - 1 do
    begin
      PackageName := FindPackageForUnitName(FUnits[I].Name);
      if ShowPackages or (PackageName = '') then
      begin
        AItem := UnitListView.Items.Add;
        AItem.Caption := FUnits[I].Name;
        AItem.SubItems.Add(Format('%.0n', [IntToExtended(FUnits[I].Size)]));
        AItem.SubItems.Add(FUnits[I].Group);
        AItem.SubItems.Add(PackageName);
        case FUnits[I].Group[1] of
          'D':
            AItem.ImageIndex := 3;
          'B':
            AItem.ImageIndex := 4;
        else
          AItem.ImageIndex := 2;
        end;
      end;
    end;
    UnitListView.AlphaSort;
  finally
    UnitListView.Items.EndUpdate;
  end;
end;

procedure TProjectAnalyzerForm.ShowSummary;
var
  I: Integer;
  PackageName: string;
  AItem: TListItem;
begin
  FView := pavSummary;
  UnitListView.Items.BeginUpdate;
  try
    UnitListView.Items.Clear;
    for I := 0 to FUnitsSum.Count - 1 do
    begin
      PackageName := FindPackageForUnitName(FUnitsSum.Strings[I]);
      if ShowPackages or (PackageName = '') then
      begin
        AItem := UnitListView.Items.Add;
        AItem.Caption := FUnitsSum.Strings[I];
        AItem.SubItems.Add(Format('%.0n', [IntToExtended(Integer(FUnitsSum.Objects[I]))]));
        AItem.SubItems.Add(LoadResString(@RsCodeData));
        AItem.SubItems.Add(PackageName);
        AItem.ImageIndex := 2;
      end;
    end;
    UnitListView.AlphaSort;
  finally
    UnitListView.Items.EndUpdate;
  end;
end;

procedure TProjectAnalyzerForm.ShowDfms;
var
  I: Integer;
begin
  FView := pavDfms;
  with UnitListView do
  begin
    Items.BeginUpdate;
    Items.Clear;
    for I := 0 to Length(FDfms) - 1 do
      with Items.Add do
      begin
        Caption := FDfms[I].Name;
        SubItems.Add(Format('%.0n', [IntToExtended(FDfms[I].Size)]));
        SubItems.Add('');
        SubItems.Add('');
        ImageIndex := ActionShowDfms.ImageIndex;
      end;
    AlphaSort;
    Items.EndUpdate;
  end;
end;

procedure TProjectAnalyzerForm.OnMapSegmentEvent(Sender: TObject; const Address: TJclMapAddress;
  Length: Integer; const ClassName, UnitName: string);
var
  C: Integer;
  ClassName1: Char;
begin
  C := System.Length(FUnits);
  SetLength(FUnits, C + 1);
  if System.Length(ClassName) > 0 then
    ClassName1 := ClassName[1]
  else
    ClassName1 := #0;
  FUnits[C].Name := UnitName;
  FUnits[C].Size := Length;
  FUnits[C].Group := ClassName;
  case ClassName1 of
    'B':
      begin
        Inc(FBssSize, Length);
        Length := 0;
      end;
    'C':
      Inc(FCodeSize, Length);
    'D':
      Inc(FDataSize, Length);
    'I':
      Inc(FICodeSize, Length);
  end;
  C := FUnitsSum.IndexOf(UnitName);
  if C = -1 then
    FUnitsSum.AddObject(UnitName, Pointer(Length))
  else
    FUnitsSum.Objects[C] := Pointer(Integer(FUnitsSum.Objects[C]) + Length);
end;

procedure TProjectAnalyzerForm.UnitListViewColumnClick(Sender: TObject; Column: TListColumn);
begin
  JvListViewSortClick(Column, 0, 1);
  TListView(Sender).AlphaSort;
end;

procedure TProjectAnalyzerForm.UnitListViewCompare(Sender: TObject;
  Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  JvListViewCompare(TListView(Sender), Item1, Item2, Compare);
end;

procedure TProjectAnalyzerForm.ActionShowDetailsExecute(Sender: TObject);
begin
  ShowDetails;
end;

procedure TProjectAnalyzerForm.ActionShowDetailsUpdate(Sender: TObject);
var
  AAction: TAction;
begin
  AAction := Sender as TAction;

  AAction.Enabled := (Length(FUnits) > 0);
  AAction.Checked := View = pavDetails;
end;

procedure TProjectAnalyzerForm.ActionShowSummaryExecute(Sender: TObject);
begin
  ShowSummary;
end;

procedure TProjectAnalyzerForm.ActionShowSummaryUpdate(Sender: TObject);
var
  AAction: TAction;
begin
  AAction := Sender as TAction;

  AAction.Enabled := (Length(FUnits) > 0);
  AAction.Checked := View = pavSummary;
end;

procedure TProjectAnalyzerForm.ActionShowDfmsExecute(Sender: TObject);
begin
  ShowDfms;
end;

procedure TProjectAnalyzerForm.ActionShowDfmsUpdate(Sender: TObject);
var
  AAction: TAction;
begin
  AAction := Sender as TAction;

  AAction.Enabled := (Length(FUnits) > 0);
  AAction.Checked := View = pavDfms;
end;

procedure TProjectAnalyzerForm.ActionShowPackagesExecute(Sender: TObject);
begin
  FShowPackages := not FShowPackages;
  ActionShowPackages.Checked := not ActionShowPackages.Checked;
  case FView of
    pavDetails:
      ShowDetails;
    pavSummary:
      ShowSummary;
    pavDfms:
      ShowDfms;
  end;
end;

procedure TProjectAnalyzerForm.ActionShowPackagesUpdate(Sender: TObject);
var
  AAction: TAction;
begin
  AAction := Sender as TAction;

  AAction.Enabled := (Length(FUnits) > 0);
  AAction.Checked := ShowPackages;
end;

procedure TProjectAnalyzerForm.TextLabelsItemClick(Sender: TObject);
begin
  TextLabelsItem.Checked := not TextLabelsItem.Checked;
  ToolBarMain.ShowCaptions := TextLabelsItem.Checked;
  ToolBarMain.ButtonHeight := 0;
  ToolBarMain.ButtonWidth := 0;
end;

procedure TProjectAnalyzerForm.ActionCopyExecute(Sender: TObject);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    JvListViewToStrings(UnitListView, SL, False, True);
    SL.Add('');
    SL.Add(StatusBarMain.Panels[0].Text);
    Clipboard.AsText := SL.Text;
  finally
    SL.Free;
  end;
end;

constructor TProjectAnalyzerForm.Create(AOwner: TComponent;
  ASettings: TJclOtaSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
end;

procedure TProjectAnalyzerForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  // Fixing the Window Ghosting "bug"
  Params.Style := params.Style or WS_POPUP;
  if Assigned(Screen.ActiveForm) then
    Params.WndParent := Screen.ActiveForm.Handle
  else if Assigned (Application.MainForm) then
    Params.WndParent := Application.MainForm.Handle
  else
    Params.WndParent := Application.Handle;
end;

destructor TProjectAnalyzerForm.Destroy;
begin
  ProjectAnalyzerForm := nil;
  inherited Destroy;
end;

procedure TProjectAnalyzerForm.ActionSaveExecute(Sender: TObject);
var
  SL: TStringList;
begin
  with SaveDialogProjectAnalyser do
  begin
    FileName := '';
    if Execute then
    begin
      SL := TStringList.Create;
      try
        JvListViewToStrings(UnitListView, SL, False, True);
        SL.SaveToFile(FileName);
      finally
        SL.Free;
      end;
    end;
  end;
end;

function TProjectAnalyzerForm.FindPackageForUnitName(const UnitName: string): string;
var
  I: Integer;
begin
  Result := '';
  if UnitName <> 'SysInit' then
    for I := 0 to Length(FPackageUnits) - 1 do
      if FPackageUnits[I].UnitName = UnitName then
      begin
        Result := FPackageUnits[I].PackageName;
        Break;
      end;
end;

procedure TProjectAnalyzerForm.SetStatusBarText(const Value: string);
begin
  with StatusBarMain do
  begin
    Panels[0].Text := Value;
    Repaint;
  end;
end;

procedure TProjectAnalyzerForm.ClearContent;
begin
  ClearData;
  StatusBarText := '';
  UnitListView.Items.BeginUpdate;
  UnitListView.Items.Clear;
  UnitListView.Items.EndUpdate;
  Show;
  Repaint;
end;

procedure TProjectAnalyzerForm.ClearData;
begin
  FDfms := nil;
  FUnits := nil;
  FUnitsSum.Clear;
  FCodeSize := 0;
  FICodeSize := 0;
  FDataSize := 0;
  FBssSize := 0;
  FPackageUnits := nil;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
