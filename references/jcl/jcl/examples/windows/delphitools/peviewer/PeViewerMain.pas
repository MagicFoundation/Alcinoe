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
{ The Original Code is PeViewerMain.pas.                                                           }
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

unit PeViewerMain;

{$I JCL.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ActnList, ToolWin, ComCtrls, StdActns, ImgList, ShellAPI, JclPeImage;

const
  UM_CHECKPARAMSTR = WM_USER + $100;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    ActionList: TActionList;
    File1: TMenuItem;
    StatusBar1: TStatusBar;
    FileOpen1: TAction;
    Edit1: TMenuItem;
    Window1: TMenuItem;
    Exit1: TAction;
    Exit2: TMenuItem;
    InvokeHelp1: TAction;
    Copy1: TAction;
    Save1: TAction;
    Copytoclipboard1: TMenuItem;
    OpenFileDialog: TOpenDialog;
    WindowCascade1: TWindowCascade;
    WindowTileHorizontal1: TWindowTileHorizontal;
    WindowTileVertical1: TWindowTileVertical;
    Cascade1: TMenuItem;
    TileHorizontally1: TMenuItem;
    TileVertically1: TMenuItem;
    ToolbarImagesList: TImageList;
    Savetofile1: TMenuItem;
    Open1: TMenuItem;
    Help1: TMenuItem;
    About1: TAction;
    About2: TMenuItem;
    CoolBar1: TCoolBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton11: TToolButton;
    N3: TMenuItem;
    SaveDialog: TSaveDialog;
    OpenLibrary1: TAction;
    ToolButton2: TToolButton;
    SelectAll1: TAction;
    Selectall2: TMenuItem;
    IconImageList: TImageList;
    GroupImports1: TAction;
    View1: TMenuItem;
    Openlibrary2: TMenuItem;
    FindinWin32APIhelp1: TMenuItem;
    N1: TMenuItem;
    Search1: TAction;
    ToolButton6: TToolButton;
    N2: TMenuItem;
    Search2: TMenuItem;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    Groupimports2: TMenuItem;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ViewResources1: TAction;
    Viewresources2: TMenuItem;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ViewResDetails1: TAction;
    ViewResHex1: TAction;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    Viewdetails1: TMenuItem;
    Viewashex1: TMenuItem;
    SendMail1: TAction;
    Support1: TMenuItem;
    ShowUnitGen1: TAction;
    ToolButton18: TToolButton;
    Pascalunitgenerator1: TMenuItem;
    UnmangleNames1: TAction;
    ToolButton19: TToolButton;
    Unmanglenames2: TMenuItem;
    Find1: TAction;
    ToolButton20: TToolButton;
    N4: TMenuItem;
    Findtext1: TMenuItem;
    procedure Exit1Execute(Sender: TObject);
    procedure InvokeHelp1Update(Sender: TObject);
    procedure FileOpen1Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InvokeHelp1Execute(Sender: TObject);
    procedure Save1Execute(Sender: TObject);
    procedure OpenLibrary1Execute(Sender: TObject);
    procedure Copy1Update(Sender: TObject);
    procedure OpenLibrary1Update(Sender: TObject);
    procedure Copy1Execute(Sender: TObject);
    procedure SelectAll1Execute(Sender: TObject);
    procedure GroupImports1Update(Sender: TObject);
    procedure GroupImports1Execute(Sender: TObject);
    procedure Search1Execute(Sender: TObject);
    procedure ViewResources1Update(Sender: TObject);
    procedure ViewResources1Execute(Sender: TObject);
    procedure ViewResDetails1Update(Sender: TObject);
    procedure ViewResDetails1Execute(Sender: TObject);
    procedure ViewResHex1Update(Sender: TObject);
    procedure ViewResHex1Execute(Sender: TObject);
    procedure Save1Update(Sender: TObject);
    procedure About1Execute(Sender: TObject);
    procedure SendMail1Execute(Sender: TObject);
    procedure ShowUnitGen1Update(Sender: TObject);
    procedure ShowUnitGen1Execute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UnmangleNames1Update(Sender: TObject);
    procedure UnmangleNames1Execute(Sender: TObject);
    procedure SelectAll1Update(Sender: TObject);
    procedure Find1Update(Sender: TObject);
    procedure Find1Execute(Sender: TObject);
    procedure CoolBar1Resize(Sender: TObject);
  private
    FWin32Help: string;
    function ActiveListViewToStrings: TStrings;
    function IsWin32Help: Boolean;
    function IsPeDumpChildActive: Boolean;
    function IsPeResChildActive: Boolean;
    function IsSearchChildActive: Boolean;
    function IsGenDefChildActive: Boolean;
    procedure OnActiveFormChange(Sender: TObject);
    procedure UMCheckParamStr(var Message: TMessage); message UM_CHECKPARAMSTR;
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
  public
    function FindPeResourceView(APeImage: TJclPeImage): TForm;
    procedure InvokeWin32Help(const Name: string);
    procedure OpenFile(const FileName: TFileName; CheckIfOpen: Boolean);
  end;

var
  MainForm: TMainForm;

const
  icoHeader      =  0;
  icoDirectory   =  1;
  icoImports     =  2;
  icoExports     =  3;
  icoResources   =  4;
  icoSection     =  5;
  icoSortAsc     =  6;
  icoSortDesc    =  7;
  icoDelayImport =  8;
  icoBoundImport =  9;
  icoLoadConfig  = 10;
  icoRelocation  = 11;
  icoDebug       = 12;
  icoFolderShut  = 13;
  icoFolderOpen  = 14;
  icoResItem     = 15;
  icoWarning     = 16;

implementation

uses ActiveX, ClipBrd, ToolsUtils, JclFileUtils, JclSysUtils,
  About, PeDump, PeSearch, PeResView, PeGenDef, FindDlg;

{$R *.DFM}

{ TMainForm }

procedure TMainForm.Exit1Execute(Sender: TObject);
begin
  Close;
end;

function TMainForm.IsPeDumpChildActive: Boolean;
begin
  Result := ActiveMDIChild is TPeDumpChild;
end;

procedure TMainForm.InvokeHelp1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := IsWin32Help and IsPeDumpChildActive and
    (TPeDumpChild(ActiveMDIChild).ActiveWin32Function <> '');
end;

procedure TMainForm.FileOpen1Execute(Sender: TObject);
var
  I: Integer;
begin
  with OpenFileDialog do
  begin
    FileName := '';
    if Execute then
      for I := 0 to Files.Count - 1 do OpenFile(Files[I], False);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FWin32Help := Win32HelpFileName;
  Screen.OnActiveFormChange := OnActiveFormChange;
  DragAcceptFiles(Handle, True);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Screen.OnActiveFormChange := nil;
  WinHelp(Application.Handle, PChar(FWin32Help), HELP_QUIT, 0);
  DragAcceptFiles(Handle, False);
end;

procedure TMainForm.OnActiveFormChange(Sender: TObject);
begin
  if IsPeDumpChildActive then
  begin
//    GroupImports1.Checked := TPeDumpChild(ActiveMDIChild).GroupImports;
    StatusBar1.Panels[0].Text := TPeDumpChild(ActiveMDIChild).FileName;
  end else
  if IsPeResChildActive then
  begin
    StatusBar1.Panels[0].Text := TPeResViewChild(ActiveMDIChild).PeImage.FileName;
  end else
  if IsGenDefChildActive then
  begin
    StatusBar1.Panels[0].Text := TPeGenDefChild(ActiveMDIChild).FileName;
  end else
    StatusBar1.Panels[0].Text := '';
end;

procedure TMainForm.OpenFile(const FileName: TFileName; CheckIfOpen: Boolean);
var
  EI: TJclPeImage;
  I: Integer;
begin
  if CheckIfOpen then
  begin
    for I := 0 to MDIChildCount - 1 do
      if MDIChildren[I] is TPeDumpChild and (TPeDumpChild(MDIChildren[I]).FileName = FileName) then
      begin
        MDIChildren[I].BringToFront;
        Exit;
      end;
  end;
  Screen.Cursor := crHourGlass;
  EI := TJclPeImage.Create;
  try
    try
      EI.FileName := FileName;
      TPeDumpChild.CreateEx(Self, EI);
    except
      EI.Free;
      raise;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.InvokeHelp1Execute(Sender: TObject);
begin
  InvokeWin32Help(TPeDumpChild(ActiveMDIChild).ActiveWin32Function);
end;

procedure TMainForm.Save1Execute(Sender: TObject);
var
  SL: TStrings;
begin
  if IsPeResChildActive and TPeResViewChild(ActiveMDIChild).CanSaveResource then
    TPeResViewChild(ActiveMDIChild).SaveResource
  else
  if IsGenDefChildActive then
    TPeGenDefChild(ActiveMDIChild).SaveUnit
  else
  with SaveDialog do
  begin
    if IsPeDumpChildActive then
      FileName := ChangeFileExt(TPeDumpChild(ActiveMDIChild).FileName, '.txt')
    else
      FileName := '';
    if Execute then
    begin
      SL := ActiveListViewToStrings;
      try
        SL.SaveToFile(FileName);
      finally
        SL.Free;
      end;
    end;
  end;
end;

function TMainForm.IsWin32Help: Boolean;
begin
  Result := FWin32Help <> '';
end;

procedure TMainForm.InvokeWin32Help(const Name: string);
var
  S: string;
begin
  S := PeStripFunctionAW(Name);
  WinHelp(Application.Handle, PChar(FWin32Help), HELP_KEY, {$IFDEF RTL230_UP}NativeUInt(S){$ELSE}DWORD(S){$ENDIF});
end;

procedure TMainForm.OpenLibrary1Execute(Sender: TObject);
begin
  if IsPeDumpChildActive then
    OpenFile(TPeDumpChild(ActiveMDIChild).ActiveLibName, False)
  else
    OpenFile(TPeSearchChild(ActiveMDIChild).ActiveLibName, False);
end;

procedure TMainForm.Copy1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := (Screen.ActiveControl is TListView) or
   ((Screen.ActiveControl is TRichEdit) and ((Screen.ActiveControl as TRichEdit).SelLength > 0));
end;

procedure TMainForm.OpenLibrary1Update(Sender: TObject);
begin
  OpenLibrary1.Enabled :=
  (IsPeDumpChildActive and (TPeDumpChild(ActiveMDIChild).ActiveLibName <> '')) or
  (IsSearchChildActive and (TPeSearchChild(ActiveMDIChild).ActiveLibName <> ''));
end;

function TMainForm.ActiveListViewToStrings: TStrings;
begin
  Screen.Cursor := crHourGlass;
  try
    Result := TStringList.Create;
    try
      Result.Capacity := 256;
      ListViewToStrings(Screen.ActiveControl as TListView, Result, True);
    except
      FreeAndNil(Result);
      raise;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.Copy1Execute(Sender: TObject);
var
  SL: TStrings;
begin
  if Screen.ActiveControl is TRichEdit then
    (Screen.ActiveControl as TRichEdit).CopyToClipboard
  else
  if Screen.ActiveControl is TListView then
  begin
    SL := ActiveListViewToStrings;
    try
      Clipboard.AsText := SL.Text;
    finally
      SL.Free;
    end;
  end;
end;

procedure TMainForm.SelectAll1Execute(Sender: TObject);
begin
  if Screen.ActiveControl is TRichEdit then
    TRichEdit(Screen.ActiveControl).SelectAll
  else
  if Screen.ActiveControl is TListView then
    ListViewSelectAll(TListView(Screen.ActiveControl));
end;

procedure TMainForm.GroupImports1Update(Sender: TObject);
begin
  with TAction(Sender) do
  begin
    Enabled := IsPeDumpChildActive;
    if Enabled then
      Checked := TPeDumpChild(ActiveMDIChild).GroupImports
    else
      Checked := False;
  end;
end;

procedure TMainForm.GroupImports1Execute(Sender: TObject);
begin
  with TPeDumpChild(ActiveMDIChild) do
  begin
    GroupImports := not GroupImports;
    GroupImports1.Checked := GroupImports;
  end;
end;

procedure TMainForm.Search1Execute(Sender: TObject);
begin
  TPeSearchChild.Create(Self);
end;

function TMainForm.IsSearchChildActive: Boolean;
begin
  Result := ActiveMDIChild is TPeSearchChild;
end;

procedure TMainForm.ViewResources1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := IsPeDumpChildActive and
    TPeDumpChild(ActiveMDIChild).HasDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE];
end;

procedure TMainForm.ViewResources1Execute(Sender: TObject);
var
  F: TForm;
begin
  with ActiveMDIChild as TPeDumpChild do
  begin
    F := FindPeResourceView(PeImage);
    if F = nil then
      TPeResViewChild.CreateEx(Self, PeImage)
    else
      F.BringToFront;
  end;
end;

function TMainForm.FindPeResourceView(APeImage: TJclPeImage): TForm;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to MDIChildCount - 1 do
    if (MDIChildren[I] is TPeResViewChild) and (TPeResViewChild(MDIChildren[I]).PeImage = APeImage) then
    begin
      Result := MDIChildren[I];
      Break;
    end;
end;

function TMainForm.IsPeResChildActive: Boolean;
begin
  Result := ActiveMDIChild is TPeResViewChild;
end;

procedure TMainForm.ViewResDetails1Update(Sender: TObject);
begin
  with TAction(Sender) do
  begin
    Enabled := IsPeResChildActive;
    if Enabled then
      Checked := TPeResViewChild(ActiveMDIChild).ShowSpecialDirView
    else
      Checked := False;
  end;
end;

procedure TMainForm.ViewResDetails1Execute(Sender: TObject);
begin
  with ViewResDetails1 do
  begin
    Checked := not Checked;
    TPeResViewChild(ActiveMDIChild).ShowSpecialDirView := Checked;
  end;
end;

procedure TMainForm.ViewResHex1Update(Sender: TObject);
begin
  with TAction(Sender) do
  begin
    Enabled := IsPeResChildActive;
    if Enabled then
      Checked := TPeResViewChild(ActiveMDIChild).ShowAsHexView
    else
      Checked := False;
  end;
end;

procedure TMainForm.ViewResHex1Execute(Sender: TObject);
begin
  with ViewResHex1 do
  begin
    Checked := not Checked;
    TPeResViewChild(ActiveMDIChild).ShowAsHexView := Checked;
  end;
end;

procedure TMainForm.Save1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := (Screen.ActiveControl is TListView) or
    (IsPeResChildActive and TPeResViewChild(ActiveMDIChild).CanSaveResource) or
    (IsGenDefChildActive and TPeGenDefChild(ActiveMDIChild).CanSave);
end;

procedure TMainForm.About1Execute(Sender: TObject);
begin
  ShowToolsAboutBox;
end;

procedure TMainForm.SendMail1Execute(Sender: TObject);
begin
  SendEmail;
end;

procedure TMainForm.ShowUnitGen1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := IsPeDumpChildActive and
    TPeDumpChild(ActiveMDIChild).HasDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT];
end;

procedure TMainForm.ShowUnitGen1Execute(Sender: TObject);
var
  CurrFileName: TFileName;
begin
  CurrFileName := (ActiveMDIChild as TPeDumpChild).FileName;
  with TPeGenDefChild.Create(Self) do
    FileName := CurrFileName;
end;

function TMainForm.IsGenDefChildActive: Boolean;
begin
  Result := ActiveMDIChild is TPeGenDefChild;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  PostMessage(Handle, UM_CHECKPARAMSTR, 0, 0);
end;

procedure TMainForm.UMCheckParamStr(var Message: TMessage);
var
  I: Integer;
  FileName: TFileName;
begin
  for I := 1 to ParamCount do
  begin
    FileName := PathGetLongName(ParamStr(I));
    if (FileName <> '') and (FileName[1] <> '-') and (FileName[1] <> '/') then
      OpenFile(FileName, False);
  end;    
end;

procedure TMainForm.WMDropFiles(var Message: TWMDropFiles);
var
  FilesCount, I: Integer;
  FileName: array[0..MAX_PATH] of Char;
begin
  FilesCount := DragQueryFile(Message.Drop, MAXDWORD, nil, 0);
  for I := 0 to FilesCount - 1 do
  begin
    if (DragQueryFile(Message.Drop, I, @FileName, SizeOf(FileName)) > 0) and
      IsValidPeFile(FileName) then
        OpenFile(FileName, True);
  end;
  DragFinish(Message.Drop);
  Message.Result := 0;
  Application.BringToFront;
end;

procedure TMainForm.UnmangleNames1Update(Sender: TObject);
begin
  with TAction(Sender) do
  begin
    Enabled := IsPeDumpChildActive;
    if Enabled then
      Checked := TPeDumpChild(ActiveMDIChild).UnmangleNames
    else
      Checked := False;
  end;
end;

procedure TMainForm.UnmangleNames1Execute(Sender: TObject);
begin
  with TPeDumpChild(ActiveMDIChild) do
  begin
    UnmangleNames := not UnmangleNames;
    UnmangleNames1.Checked := UnmangleNames;
  end;
end;

procedure TMainForm.SelectAll1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := (Screen.ActiveControl is TListView) or
   (Screen.ActiveControl is TRichEdit);
end;

procedure TMainForm.Find1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := TFindTextForm.CanExecuteFind;
end;

procedure TMainForm.Find1Execute(Sender: TObject);
begin
  ShowFindDialog(Screen.ActiveControl as TListView);
end;

procedure TMainForm.CoolBar1Resize(Sender: TObject);
begin
  D4FixCoolBarResizePaint(Sender);
end;

initialization
  OleInitialize(nil);

finalization
  OleUninitialize;

end.
