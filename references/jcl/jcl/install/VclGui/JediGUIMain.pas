{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) extension                                                        }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JediInstallerMain.pas.                                                      }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Andreas Hausladen (ahuser)                                                                     }
{   Robert Rossmair (rrossmair) - crossplatform & BCB support, refactoring                         }
{   Florent Ouchet (outchy) - new installer core                                                   }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JediGUIMain;

{$I jcl.inc}
{$I crossplatform.inc}

interface

uses
  Windows, Messages, CommCtrl,
  SysUtils, Classes,
  Graphics, Forms, Controls, Dialogs, StdCtrls, ExtCtrls, Menus, Buttons, ComCtrls, ImgList,
  JclWin32, JclIDEUtils, JclContainerIntf, JediInstall;

const
  WM_AFTERSHOW = WM_USER + 10;

type
  TMainForm = class(TForm, IJediInstallGUI)
    InstallBtn: TBitBtn;
    UninstallBtn: TBitBtn;
    QuitBtn: TBitBtn;
    JediImage: TImage;
    TitlePanel: TPanel;
    Title: TLabel;
    ProductsPageControl: TPageControl;
    StatusBevel: TBevel;
    StatusLabel: TLabel;
    Bevel1: TBevel;
    ProgressBar: TProgressBar;
    ImageList: TImageList;
    InstallSelectedOnlyCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure QuitBtnClick(Sender: TObject);
    procedure InstallBtnClick(Sender: TObject);
    procedure UninstallBtnClick(Sender: TObject);
    procedure JediImageClick(Sender: TObject);
    procedure ProductsPageControlChange(Sender: TObject);
  protected
    FPages: IJclIntfList;
    FAutoAcceptDialogs: TDialogTypes;
    FAutoAcceptMPL: Boolean;
    FAutoCloseOnFailure: Boolean;
    FAutoCloseOnSuccess: Boolean;
    FAutoInstall: Boolean;
    FAutoUninstall: Boolean;
    FContinueOnTargetError: Boolean;
    FXMLResultFileName: string;
    FIncludeLogFilesInXML: Boolean;
    FDeletePreviousLogFiles: Boolean;
    FIgnoreRunningIDE: Boolean;
    FTaskBarList: ITaskbarList3;
    FInstallPageCount: Integer;
    procedure UpdateInstallSelectedOnlyCheckBoxVisibility;
    function GetSelectedInstallPage: IJediInstallPage;
    procedure HandleException(Sender: TObject; E: Exception);
    procedure SetFrameIcon(Sender: TObject; const FileName: string);
    procedure WMAfterShow(var Message: TMessage); Message WM_AFTERSHOW;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowFeatureHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    // IJediInstallGUI
    function Dialog(const Text: string; DialogType: TDialogType = dtInformation;
      Options: TDialogResponses = [drOK]): TDialogResponse;
    function CreateTextPage: IJediTextPage;
    function CreateInstallPage: IJediInstallPage;
    function CreateProfilesPage: IJediProfilesPage;
    function GetPageCount: Integer;
    function GetPage(Index: Integer): IJediPage;
    function GetStatus: string;
    procedure SetStatus(const Value: string);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function GetProgress: Integer;
    procedure SetProgress(Value: Integer);
    function GetAutoAcceptDialogs: TDialogTypes;
    procedure SetAutoAcceptDialogs(Value: TDialogTypes);
    function GetAutoAcceptMPL: Boolean;
    procedure SetAutoAcceptMPL(Value: Boolean);
    function GetAutoCloseOnFailure: Boolean;
    procedure SetAutoCloseOnFailure(Value: Boolean);
    function GetAutoCloseOnSuccess: Boolean;
    procedure SetAutoCloseOnSuccess(Value: Boolean);
    function GetAutoInstall: Boolean;
    procedure SetAutoInstall(Value: Boolean);
    function GetAutoUninstall: Boolean;
    procedure SetAutoUninstall(Value: Boolean);
    function GetContinueOnTargetError: Boolean;
    procedure SetContinueOnTargetError(Value: Boolean);
    function GetXMLResultFileName: string;
    procedure SetXMLResultFileName(const Value: string);
    function GetDeletePreviousLogFiles: Boolean;
    procedure SetDeletePreviousLogFiles(Value: Boolean);
    function GetIncludeLogFilesInXML: Boolean;
    procedure SetIncludeLogFilesInXML(Value: Boolean);
    function GetIgnoreRunningIDE: Boolean;
    procedure SetIgnoreRunningIDE(Value: Boolean);
    procedure Execute;
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  ActiveX, ComObj,
  FileCtrl,
  JclDebug, JclShell, JediGUIProfiles,
  JclBase, JclFileUtils, JclStrings, JclSysInfo, JclSysUtils, JclArrayLists,
  JediInstallResources,
  JediGUIText, JediGUIInstall;

const
  DelphiJediURL     = 'http://www.delphi-jedi.org/';

function CreateMainForm: IJediInstallGUI;
var
  MainForm: TMainForm;
begin
  {$IFDEF RTL185_UP}
  Application.MainFormOnTaskbar := True;
  {$ENDIF RTL185_UP}
  Application.Initialize;
  Application.Title := 'JEDI Installer';
  Application.CreateForm(TMainForm, MainForm);
  Result := MainForm;
end;

//=== { TMainForm } ==========================================================

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPages := TJclIntfArrayList.Create(5);
  Application.OnException := HandleException;
end;

destructor TMainForm.Destroy;
begin
  FPages := nil;
  inherited Destroy;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SetStatus('');

  Caption := LoadResString(@RsGUIJEDIInstaller);
  Title.Caption := LoadResString(@RsGUIProjectJEDIInstaller);
  InstallBtn.Caption := LoadResString(@RsGUIInstall);
  UninstallBtn.Caption := LoadResString(@RsGUIUninstall);
  QuitBtn.Caption := LoadResString(@RsGUIQuit);
  InstallSelectedOnlyCheckBox.Caption := LoadResString(@RsGUIInstallSelectedOnly);

  JediImage.Hint := DelphiJediURL;

  TitlePanel.DoubleBuffered := True;
  {$IFDEF COMPILER7_UP}
  TitlePanel.ParentBackground := False;
  {$ENDIF}
  Application.HintPause := 500;
  Application.OnShowHint := ShowFeatureHint;

  CoCreateInstance(CLSID_TaskbarList, nil, CLSCTX_INPROC_SERVER, ITaskBarList3, FTaskBarList);
  if Assigned(FTaskBarList) then
    FTaskBarList.HrInit;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  InstallCore.Close;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  PostMessage(Handle, WM_AFTERSHOW, 0, 0);
end;

procedure TMainForm.UpdateInstallSelectedOnlyCheckBoxVisibility;
begin
  InstallSelectedOnlyCheckBox.Visible := (FInstallPageCount > 1) and (GetSelectedInstallPage <> nil);
end;

procedure TMainForm.ShowFeatureHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
var
  ATabSheet: TTabSheet;
  ScreenPos: TPoint;
begin
  if HintStr = '' then
  begin
    ScreenPos := HintInfo.HintControl.ClientToScreen(HintInfo.CursorPos);
    ATabSheet := ProductsPageControl.ActivePage;
    HintStr := (FPages.GetObject(ATabSheet.PageIndex) as IJediPage).GetHintAtPos(ScreenPos.X, ScreenPos.Y);
    HintInfo.ReshowTimeout := 100;
  end;
  CanShow := HintStr <> '';
end;

procedure TMainForm.SetFrameIcon(Sender: TObject; const FileName: string);
var
  IconHandle: HICON;
  ModuleHandle: THandle;
  ATabSheet: TTabSheet;
begin
  ATabSheet := (Sender as TInstallFrame).Parent as TTabSheet;

  IconHandle := 0;

  if SameText(ExtractFileName(FileName), '.ico') then
    IconHandle := LoadImage(0, PChar(FileName), IMAGE_ICON, ImageList.Width, ImageList.Height,
      LR_LOADFROMFILE or LR_LOADTRANSPARENT)
  else
  begin
    ModuleHandle := LoadLibraryEx(PChar(FileName), 0, LOAD_LIBRARY_AS_DATAFILE or DONT_RESOLVE_DLL_REFERENCES);
    if ModuleHandle <> 0 then
    try
      IconHandle := LoadImage(ModuleHandle, 'MAINICON', IMAGE_ICON, ImageList.Width, ImageList.Height,
        LR_LOADTRANSPARENT);
    finally
      FreeLibrary(ModuleHandle);
    end;
  end;
  if IconHandle <> 0 then
  try
    ATabSheet.ImageIndex := ImageList_AddIcon(ImageList.Handle, IconHandle);
  finally
    DestroyIcon(IconHandle);
  end;
end;

procedure TMainForm.SetIncludeLogFilesInXML(Value: Boolean);
begin
  FIncludeLogFilesInXML := Value;
end;

procedure TMainForm.QuitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.InstallBtnClick(Sender: TObject);
var
  Success: Boolean;
  InstallPage: IJediInstallPage;
begin
  InstallPage := nil;
  if InstallSelectedOnlyCheckBox.Visible and InstallSelectedOnlyCheckBox.Checked then
    InstallPage := GetSelectedInstallPage;

  ProgressBar.Position := 0;
  ProgressBar.Visible := True;
  InstallSelectedOnlyCheckBox.Visible := False;
  Screen.Cursor := crHourGlass;
  try
    if Assigned(FTaskBarList) then
      FTaskBarList.SetProgressState(Self.Handle, TBPF_NORMAL);
    Success := InstallCore.Install(InstallPage);
    if (Success and FAutoCloseOnSuccess) or (not Success and FAutoCloseOnFailure) then
      Close;
  finally
    ProgressBar.Visible := False;
    UpdateInstallSelectedOnlyCheckBoxVisibility;
    Screen.Cursor := crDefault;
    if Assigned(FTaskBarList) then
      FTaskBarList.SetProgressState(Self.Handle, TBPF_NOPROGRESS);
  end;
  QuitBtn.SetFocus;
end;

procedure TMainForm.UninstallBtnClick(Sender: TObject);
var
  Success: Boolean;
  InstallPage: IJediInstallPage;
begin
  InstallPage := nil;
  if InstallSelectedOnlyCheckBox.Visible and InstallSelectedOnlyCheckBox.Checked then
    InstallPage := GetSelectedInstallPage;

  ProgressBar.Position := 0;
  ProgressBar.Visible := True;
  InstallSelectedOnlyCheckBox.Visible := False;
  Screen.Cursor := crHourGlass;
  try
    if Assigned(FTaskBarList) then
      FTaskBarList.SetProgressState(Self.Handle, TBPF_NORMAL);

    Success := InstallCore.Uninstall(InstallPage);
    if (Success and FAutoCloseOnSuccess) or (not Success and FAutoCloseOnFailure) then
      Close;
  finally
    ProgressBar.Visible := False;
    UpdateInstallSelectedOnlyCheckBoxVisibility;
    Screen.Cursor := crDefault;
    if Assigned(FTaskBarList) then
      FTaskBarList.SetProgressState(Self.Handle, TBPF_NOPROGRESS);
  end;
  QuitBtn.SetFocus;
end;

procedure TMainForm.WMAfterShow(var Message: TMessage);
begin
  if FAutoInstall then
  begin
    UninstallBtn.Visible := False;
    InstallBtnClick(InstallBtn)
  end
  else if FAutoUninstall then
  begin
    InstallBtn.Visible := False;
    UninstallBtnClick(UninstallBtn);
  end;
end;

procedure TMainForm.JediImageClick(Sender: TObject);
begin
  { TODO : implement for Unix }
  ShellExecEx(DelphiJediURL);
end;

procedure TMainForm.ProductsPageControlChange(Sender: TObject);
begin
  UpdateInstallSelectedOnlyCheckBoxVisibility;
end;

procedure TMainForm.HandleException(Sender: TObject; E: Exception);
begin
  if E is EJediInstallInitFailure then
  begin
    Dialog(E.Message, dtError);
    Application.ShowMainForm := False;
    Application.Terminate;
  end
  else
    Application.ShowException(E);
end;

function TMainForm.Dialog(const Text: string; DialogType: JediInstall.TDialogType = dtInformation;
  Options: TDialogResponses = [drOK]): TDialogResponse;
const
  DlgType: array[JediInstall.TDialogType] of TMsgDlgType = (mtWarning, mtError, mtInformation, mtConfirmation);
  DlgButton: array[TDialogResponse] of TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel);
  DlgResult: array[TDialogResponse] of Word = (mrYes, mrNo, mrOK, mrCancel);
var
  Buttons: TMsgDlgButtons;
  Res: Integer;
  OldCursor: TCursor;
  DialogResponse: TDialogResponse;
begin
  if DialogType in FAutoAcceptDialogs then
  begin
    for DialogResponse := Low(TDialogResponse) to High(TDialogResponse) do
      if DialogResponse in Options then
    begin
      Result := DialogResponse;
      Exit;
    end;
  end;
  OldCursor := Screen.Cursor;
  try
    if Assigned(FTaskBarList) then
    begin
      if DialogType = dtError then
        FTaskBarList.SetProgressState(Self.Handle, TBPF_ERROR)
      else
        FTaskBarList.SetProgressState(Self.Handle, TBPF_PAUSED);
    end;

    Screen.Cursor := crDefault;
    Buttons := [];
    for Result := Low(TDialogResponse) to High(TDialogResponse) do
      if Result in Options then
        Include(Buttons, DlgButton[Result]);
    Res := MessageDlg(Text, DlgType[DialogType], Buttons, 0);
    for Result := Low(TDialogResponse) to High(TDialogResponse) do
      if DlgResult[Result] = Res then
        Break;

    if Assigned(FTaskBarList) then
      FTaskBarList.SetProgressState(Self.Handle, TBPF_NORMAL)
  finally
    Screen.Cursor := OldCursor;
  end;
end;

function TMainForm.CreateTextPage: IJediTextPage;
var
  AReadmeFrame: TTextFrame;
  ATabSheet: TTabSheet;
begin
  ATabSheet := TTabSheet.Create(Self);
  ATabSheet.PageControl := ProductsPageControl;
  ATabSheet.ImageIndex := -1;

  AReadmeFrame := TTextFrame.Create(Self);
  AReadmeFrame.Parent := ATabSheet;
  AReadmeFrame.Align := alClient;
  AReadmeFrame.Name := '';

  Result := AReadmeFrame;
  FPages.Add(Result);
end;

function TMainForm.CreateInstallPage: IJediInstallPage;
var
  AInstallFrame: TInstallFrame;
  ATabSheet: TTabSheet;
begin
  Inc(FInstallPageCount);

  ATabSheet := TTabSheet.Create(Self);
  ATabSheet.PageControl := ProductsPageControl;
  ATabSheet.ImageIndex := -1;

  AInstallFrame := TInstallFrame.Create(Self, Self);
  AInstallFrame.Parent := ATabSheet;
  AInstallFrame.Align := alClient;
  AInstallFrame.TreeView.Images := ImageList;
  AInstallFrame.Name := '';
  AInstallFrame.OnSetIcon := SetFrameIcon;

  Result := AInstallFrame;
  FPages.Add(Result);
end;

function TMainForm.CreateProfilesPage: IJediProfilesPage;
var
  AProfilesFrame: TProfilesFrame;
  ATabSheet: TTabSheet;
begin
  ATabSheet := TTabSheet.Create(Self);
  ATabSheet.PageControl := ProductsPageControl;
  ATabSheet.ImageIndex := -1;

  AProfilesFrame := TProfilesFrame.Create(Self);
  AProfilesFrame.Parent := ATabSheet;
  AProfilesFrame.Align := alClient;
  AProfilesFrame.Name := '';

  Result := AProfilesFrame;
  FPages.Add(Result);
end;

function TMainForm.GetSelectedInstallPage: IJediInstallPage;
var
  Tab: TTabSheet;
begin
  Tab := ProductsPageControl.ActivePage;
  if (Tab <> nil) and (Tab.ControlCount > 0) and (Tab.Controls[0] is TInstallFrame) then
    Result := TInstallFrame(Tab.Controls[0])
  else
    Result := nil;
end;

function TMainForm.GetPageCount: Integer;
begin
  Result := FPages.Size;
end;

function TMainForm.GetPage(Index: Integer): IJediPage;
begin
  Result := FPages.GetObject(Index) as IJediPage;
end;

function TMainForm.GetStatus: string;
begin
  Result := StatusLabel.Caption;
end;

function TMainForm.GetXMLResultFileName: string;
begin
  Result := FXMLResultFileName;
end;

procedure TMainForm.SetStatus(const Value: string);
begin
  if Value = '' then
  begin
    StatusBevel.Visible := False;
    StatusLabel.Visible := False;
  end
  else
  begin
    StatusLabel.Caption := Value;
    StatusBevel.Visible := True;
    StatusLabel.Visible := True;
  end;
  Application.ProcessMessages;  //Update;
end;

procedure TMainForm.SetXMLResultFileName(const Value: string);
begin
  FXMLResultFileName := Value;
end;

function TMainForm.GetAutoAcceptDialogs: TDialogTypes;
begin
  Result := FAutoAcceptDialogs;
end;

function TMainForm.GetAutoAcceptMPL: Boolean;
begin
  Result := FAutoAcceptMPL;
end;

function TMainForm.GetAutoCloseOnFailure: Boolean;
begin
  Result := FAutoCloseOnFailure;
end;

function TMainForm.GetAutoCloseOnSuccess: Boolean;
begin
  Result := FAutoCloseOnSuccess;
end;

function TMainForm.GetAutoInstall: Boolean;
begin
  Result := FAutoInstall;
end;

function TMainForm.GetAutoUninstall: Boolean;
begin
  Result := FAutoUninstall;
end;

function TMainForm.GetCaption: string;
begin
  Result := Caption;
end;

function TMainForm.GetContinueOnTargetError: Boolean;
begin
  Result := FContinueOnTargetError;
end;

function TMainForm.GetDeletePreviousLogFiles: Boolean;
begin
  Result := FDeletePreviousLogFiles;
end;

function TMainForm.GetIncludeLogFilesInXML: Boolean;
begin
  Result := FIncludeLogFilesInXML;
end;

procedure TMainForm.SetAutoAcceptDialogs(Value: TDialogTypes);
begin
  FAutoAcceptDialogs := Value;
end;

procedure TMainForm.SetAutoAcceptMPL(Value: Boolean);
begin
  FAutoAcceptMPL := Value;
end;

procedure TMainForm.SetAutoCloseOnFailure(Value: Boolean);
begin
  FAutoCloseOnFailure := Value;
end;

procedure TMainForm.SetAutoCloseOnSuccess(Value: Boolean);
begin
  FAutoCloseOnSuccess := Value;
end;

procedure TMainForm.SetAutoInstall(Value: Boolean);
begin
  FAutoInstall := Value;
end;

procedure TMainForm.SetAutoUninstall(Value: Boolean);
begin
  FAutoUninstall := Value;
end;

procedure TMainForm.SetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TMainForm.SetContinueOnTargetError(Value: Boolean);
begin
  FContinueOnTargetError := Value;
end;

procedure TMainForm.SetDeletePreviousLogFiles(Value: Boolean);
begin
  FDeletePreviousLogFiles := Value;
end;

function TMainForm.GetProgress: Integer;
begin
  Result := ProgressBar.Position;
end;

procedure TMainForm.SetProgress(Value: Integer);
begin
  ProgressBar.Position := Value;

  if Assigned(FTaskBarList) then
  begin
    FTaskBarList.SetProgressState(Self.Handle, TBPF_NORMAL);
    FTaskBarList.SetProgressValue(Self.Handle, Value,100);
  end;
end;

procedure TMainForm.Execute;
begin
  Application.Run;
end;

function TMainForm.GetIgnoreRunningIDE: Boolean;
begin
  Result := FIgnoreRunningIDE;
end;

procedure TMainForm.SetIgnoreRunningIDE(Value: Boolean);
begin
  FIgnoreRunningIDE := Value;
end;

initialization

  InstallCore.InstallGUICreator := CreateMainForm;

end.
