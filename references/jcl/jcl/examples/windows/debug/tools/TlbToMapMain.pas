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
{ The Original Code is TlbToMapMain.pas.                                                           }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) of Petr Vones.                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit TlbToMapMain;

interface

{$I jcl.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ImgList, ActnList, Menus, ToolWin, StdCtrls, ExtCtrls;

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    MainMenu1: TMainMenu;
    ActionList1: TActionList;
    ImageList1: TImageList;
    StatusBar1: TStatusBar;
    Exit1: TAction;
    Open1: TAction;
    CreateMAP1: TAction;
    File1: TMenuItem;
    Open2: TMenuItem;
    N1: TMenuItem;
    Exit2: TMenuItem;
    OpenDialog1: TOpenDialog;
    MethodsListView: TListView;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    Run1: TMenuItem;
    Convert2: TMenuItem;
    CreateJDBG1: TAction;
    ToolButton4: TToolButton;
    CreateJDBGfile1: TMenuItem;
    VersionMemo: TMemo;
    Splitter1: TSplitter;
    procedure Exit1Execute(Sender: TObject);
    procedure Open1Execute(Sender: TObject);
    procedure CreateMAP1Execute(Sender: TObject);
    procedure CreateMAP1Update(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MethodsListViewData(Sender: TObject; Item: TListItem);
  private
    FFileName: TFileName;
    FMembersList: TStringList;
    procedure SetFileName(const Value: TFileName);
  public
    procedure OpenTypeLibrary(const FileName: TFileName);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses
  ComObj, ActiveX,
  JclBase, JclDebug, JclFileUtils, JclPeImage, JclSysInfo, JclSysUtils;

resourcestring
  RsReading = 'Reading type library ...';
  RsNoTypeLib = 'The file does not contain valid Type Library.';
  RsNoCoClass = 'Type library does not contain any CoClasses.';

// Reference:
// Improve Your Debugging by Generating Symbols from COM Type Libraries
// Matt Pietrek - Microsoft Systems Journal, March 1999
// http://msdn.microsoft.com/library/periodic/period99/comtype.htm

type
  TJclTypeLibScanner = class (TObject)
  private
    FMembersList: TStrings;
    FModuleFileName: TFileName;
    FTypeLib: ITypeLib;
    FValidFormat: Boolean;
  protected
    procedure Scan;
  public
    constructor Create(const FileName: TFileName);
    destructor Destroy; override;
    property MembersList: TStrings read FMembersList;
    property ModuleFileName: TFileName read FModuleFileName; 
    property ValidFormat: Boolean read FValidFormat;
  end;

{ TJclTypeLibScanner }

constructor TJclTypeLibScanner.Create(const FileName: TFileName);
begin
  FMembersList := TStringList.Create;
  FValidFormat := Succeeded(LoadTypeLib(PWideChar(WideString(FileName)), FTypeLib));
  if FValidFormat then
    Scan;
end;

destructor TJclTypeLibScanner.Destroy;
begin
  FreeAndNil(FMembersList);
  inherited;
end;

procedure TJclTypeLibScanner.Scan;
var
  TypeInfondex, FuncIndex: Integer;
  TypeInfo: ITypeInfo;
  TypeAttr: PTypeAttr;
  RefType: HRefType;

  function GetTypeInfoName(TI: ITypeInfo; MemID: TMemberID): string;
  var
    Name: WideString;
  begin
    if Succeeded(TI.GetDocumentation(MemID, @Name, nil, nil, nil)) then
      Result := Name
    else
      Result := '';  
  end;

  procedure EnumTypeInfoMembers(MemTypeInfo: ITypeInfo; MemTypeAttr: PTypeAttr;
    MemUnknown: IUnknown);
  var
    VTable: DWORD;
    InterfaceName, MemberName, Name: string;
    I: Integer;
    FuncDesc: PFuncDesc;
    Addr: DWORD;
  begin
    VTable := PDWORD(MemUnknown)^;
    if MemTypeAttr.cFuncs = 0 then
      Exit;
    InterfaceName := GetTypeInfoName(MemTypeInfo, -1);
    for I := 0 to MemTypeAttr.cFuncs - 1 do
    begin
      MemTypeInfo.GetFuncDesc(I, FuncDesc);
      MemberName := GetTypeInfoName(MemTypeInfo, FuncDesc.memid);
      Addr := PDWORD(Integer(VTable) + FuncDesc.oVft)^;
      if FModuleFileName = '' then
        FModuleFileName := GetModulePath(ModuleFromAddr(Pointer(Addr)));
      Dec(Addr, ModuleFromAddr(Pointer(Addr)));
      Name := InterfaceName + '.' + MemberName;
      case FuncDesc.invkind of
        INVOKE_PROPERTYGET:
          Name := Name + '_Get';
        INVOKE_PROPERTYPUT:
          Name := Name + '_Put';
        INVOKE_PROPERTYPUTREF:
          Name := Name + '_PutRef';
      end;
      MemTypeInfo.ReleaseFuncDesc(FuncDesc);
      FMembersList.AddObject(Name, Pointer(Addr));
    end;
  end;

  procedure ProcessReferencedTypeInfo;
  var
    RefTypeInfo: ITypeInfo;
    RefTypeAttr: PTypeAttr;
    Unknown: IUnknown;
    R: HRESULT;
  begin
    if Succeeded(TypeInfo.GetRefTypeInfo(RefType, RefTypeInfo)) and
      Succeeded(RefTypeInfo.GetTypeAttr(RefTypeAttr)) then
    begin
      R := CoCreateInstance(TypeAttr.guid, nil, CLSCTX_INPROC_SERVER or CLSCTX_INPROC_HANDLER,
        RefTypeAttr.guid, Unknown);
      if Succeeded(R) and (Unknown <> nil) then
        EnumTypeInfoMembers(RefTypeInfo, RefTypeAttr, Unknown);
      RefTypeInfo.ReleaseTypeAttr(RefTypeAttr);
    end;
  end;

begin
  for TypeInfondex := 0 to FTypeLib.GetTypeInfoCount - 1 do
  begin
    FTypeLib.GetTypeInfo(TypeInfondex, TypeInfo);
    if Succeeded(TypeInfo.GetTypeAttr(TypeAttr)) then
    begin
      if TypeAttr.typeKind = TKIND_COCLASS then
        for FuncIndex := 0 to TypeAttr.cImplTypes - 1 do
          if Succeeded(TypeInfo.GetRefTypeOfImplType(FuncIndex, RefType)) then
            ProcessReferencedTypeInfo;
      TypeInfo.ReleaseTypeAttr(TypeAttr);
    end;
  end;
  FTypeLib := nil;
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FMembersList := TStringList.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMembersList);
end;

procedure TMainForm.Exit1Execute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.Open1Execute(Sender: TObject);
begin
  with OpenDialog1 do
  begin
    FileName := '';
    if Execute then
      OpenTypeLibrary(FileName);
  end;
end;

function SortPublicsByValue(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := DWORD(List.Objects[Index1]) - DWORD(List.Objects[Index2]);
end;

procedure TMainForm.CreateMAP1Execute(Sender: TObject);
var
  MapList: TStringList;
  PeImage: TJclPeImage;
  LoAddress, HiAddress: DWORD;
  CodeSection: TImageSectionHeader;
  MapFileName: TFileName;

  procedure WriteList;
  var
    I: Integer;
  begin
    for I := 0 to FMembersList.Count - 1 do
      MapList.Add(Format(' 0001:%.8x       %s',
        [DWORD(FMembersList.Objects[I]) - CodeSection.VirtualAddress, FMembersList[I]]));
  end;

begin
  Screen.Cursor := crHourGlass;
  MapList := TStringList.Create;
  PeImage := TJclPeImage.Create;
  try
    PeImage.FileName := FFileName;
    CodeSection := PeImage.ImageSectionHeaders[0];
    FMembersList.CustomSort(SortPublicsByValue);
    LoAddress := DWORD(FMembersList.Objects[0]);
    HiAddress := DWORD(FMembersList.Objects[FMembersList.Count - 1]);
    FMembersList.Sort;
    Assert(LoAddress >= CodeSection.VirtualAddress);
    MapList.Add('');
    MapList.Add(' Start         Length     Name                   Class');
    MapList.Add(Format(' %.4x:%.8x %.8xH  %s                  CODE',
       [1, CodeSection.VirtualAddress, CodeSection.Misc.VirtualSize,
        PeImage.ImageSectionNames[0]]));
    MapList.Add('');
    MapList.Add('');
    MapList.Add('Detailed map of segments');
    MapList.Add('');
    MapList.Add(Format(' 0001:00000000 %.8xH C=CODE     S=.text    G=(none)   M=%s',
      [HiAddress, PathExtractFileNameNoExt(FFileName)]));
    MapList.Add('');
    MapList.Add('');
    MapList.Add('Address         Publics by Name');
    MapList.Add('');
    WriteList;
    MapList.Add('');
    MapList.Add('');
    FMembersList.CustomSort(SortPublicsByValue);
    MapList.Add('Address         Publics by Value');
    MapList.Add('');
    WriteList;
    FMembersList.Sort;
    MapFileName := ChangeFileExt(FFileName, '.map');
    MapList.SaveToFile(MapFileName);
    if TAction(Sender).Tag = 1 then
    begin
      ConvertMapFileToJdbgFile(MapFileName);
      DeleteFile(MapFileName);
    end;
  finally
    PeImage.Free;
    MapList.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.CreateMAP1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := MethodsListView.Items.Count > 0;
end;

procedure TMainForm.MethodsListViewData(Sender: TObject; Item: TListItem);
begin
  with Item do
  begin
    Caption := FMembersList[Index];
    SubItems.Add(Format('%p', [Pointer(FMembersList.Objects[Index])]));
    ImageIndex := 3;
  end;
end;

procedure TMainForm.OpenTypeLibrary(const FileName: TFileName);
var
  TypeLibScanner: TJclTypeLibScanner;
  ErrorMsg: string;
begin
  Screen.Cursor := crHourGlass;
  try
    FMembersList.Clear;
    MethodsListView.Items.Count := 0;
    MethodsListView.Repaint;
    StatusBar1.Panels[0].Text := RsReading;
    StatusBar1.Repaint;
    TypeLibScanner := TJclTypeLibScanner.Create(FileName);
    try
      if TypeLibScanner.ValidFormat and (TypeLibScanner.MembersList.Count > 0) then
      begin
        FMembersList.Assign(TypeLibScanner.MembersList);
        FMembersList.Sort;
        MethodsListView.Items.Count := FMembersList.Count;
        MethodsListView.Invalidate;
        SetFileName(TypeLibScanner.ModuleFileName);
      end
      else
      begin
        Screen.Cursor := crDefault;
        SetFileName('');
        if TypeLibScanner.ValidFormat then
          ErrorMsg := RsNoCoClass
        else
          ErrorMsg := RsNoTypeLib;
        with Application do
          MessageBox(PChar(ErrorMsg), PChar(Title), MB_ICONERROR or MB_OK);
      end;
    finally
      TypeLibScanner.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
  StatusBar1.Panels[0].Text := Value;
  StatusBar1.Repaint;
  VersionMemo.Lines.Clear;
  if VersionResourceAvailable(Value) then
    with TJclFileVersionInfo.Create(Value) do
    try
      VersionMemo.Lines.Assign(Items);
    finally
      Free;
    end;
  DisableAlign;
  VersionMemo.Visible := VersionMemo.Lines.Count > 0;
  Splitter1.Visible := VersionMemo.Visible;
  EnableAlign;
  VersionMemo.Repaint;
end;

end.
