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
{ The Original Code is OpenDlgFavAdapter.pas.                                                      }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) Petr Vones. All rights reserved.                }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Salvatore Besso                                                                                }
{   Florent Ouchet (move to JCL runtime)                                                           }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclOpenDialogHooks;

interface

{$I jcl.inc}
{$I windowsonly.inc}

uses
  {$IFDEF HAS_UNITSCOPE}
  Winapi.Windows, Winapi.Messages, Winapi.ShlObj, System.Classes, System.SysUtils,
  Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Dialogs, Vcl.Forms,
  {$ELSE ~HAS_UNITSCOPE}
  Windows, Messages, ShlObj, Classes, SysUtils, Controls, StdCtrls, ExtCtrls,
  Dialogs, Forms,
  {$ENDIF ~HAS_UNITSCOPE}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase, JclPeImage, JclWin32;

// old-style open dialogs are supported by all versions of Delphi
{$DEFINE OLDSTYLE}

// new-style file dialogs are supported by Delphi 2007 and newer
// it is disabled in D2007, because the D2006 RTL does not support it at all
{$IFDEF RTL200_UP}
{$DEFINE NEWSTYLE}
{$ENDIF RTL200_UP}

type
  EJclOpenDialogHookError = class(EJclError);

  {$IFDEF NEWSTYLE}
  TJclFileOpenDialogHook = class (TFileOpenDialog)
  strict protected
    function CreateFileDialog: IFileDialog; override;
  public
    class procedure InstallHook(out OldHandler: Pointer);
    class procedure UninstallHook(OldHandler: Pointer);
  end;

  TJclFileSaveDialogHook = class (TFileSaveDialog)
  strict protected
    function CreateFileDialog: IFileDialog; override;
  public
    class procedure InstallHook(out OldHandler: Pointer);
    class procedure UninstallHook(OldHandler: Pointer);
  end;
  {$ENDIF NEWSTYLE}

  TJclOpenDialogHook = class (TObject)
  {$IFDEF OLDSTYLE}
  private
    FDisableHelpButton: Boolean;
    FDisablePlacesBar: Boolean;
    FHooks: TJclPeMapImgHooks;
    FIsOpenPictureDialog: Boolean;
    FParentWndInstance: Pointer;
    FOldParentWndInstance: Pointer;
    FPictureDialogLastFolder: string;
    FWndInstance: Pointer;
    FOldWndInstance: Pointer;
    FOnClose: TNotifyEvent;
    FOnShow: TNotifyEvent;
    function GetCurrentFolder: string;
    function GetFileNameEditWnd: HWND;
    procedure SetCurrentFolder(const Value: string);
  protected
    FHandle: HWND;
    FParentWnd: HWND;
    procedure DialogAdjustControlPos; virtual;
    procedure DialogFolderChange; virtual;
    procedure DialogShow; virtual;
    procedure DialogClose; virtual;
    procedure DoClose;
    procedure DoShow;
    procedure ParentWndProc(var Message: TMessage); virtual;
    procedure WndProc(var Message: TMessage); virtual;
    property FileNameEditWnd: HWND read GetFileNameEditWnd;
  public
    property CurrentFolder: string read GetCurrentFolder write SetCurrentFolder;
    property DisableHelpButton: Boolean read FDisableHelpButton write FDisableHelpButton;
    property DisablePlacesBar: Boolean read FDisablePlacesBar write FDisablePlacesBar;
    property IsOpenPictureDialog: Boolean read FIsOpenPictureDialog;
    property PictureDialogLastFolder: string read FPictureDialogLastFolder write FPictureDialogLastFolder;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  {$ENDIF OLDSTYLE}
  {$IFDEF NEWSTYLE}
  private
    FOldFileOpenCreateFileDialog: Pointer;
    FOldFileSaveCreateFileDialog: Pointer;
  protected
    procedure FileDialogCreate(const AFileDialog: IFileDialog); virtual;
  {$ENDIF NEWSTYLE}
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure HookDialogs;
    procedure UnhookDialogs;
  end;

  TJclOpenDialogHookClass = class of TJclOpenDialogHook;

function InitializeOpenDialogHook(OpenDialogHookClass: TJclOpenDialogHookClass): TJclOpenDialogHook;
procedure FinalizeOpenDialogHook;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\vcl';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  Winapi.CommDlg, Winapi.Dlgs,
  {$ELSE ~HAS_UNITSCOPE}
  CommDlg, Dlgs,
  {$ENDIF ~HAS_UNITSCOPE}
  JclFileUtils, JclStrings, JclSysInfo, JclSysUtils,
  JclVclResources;

{$R JclOpenDialog.res}

var
  GlobalOpenDialogHook: TJclOpenDialogHook;

{$IFDEF OLDSTYLE}
const
  OpenDialogTemplateName        = 'JCLOPENDLGHOOK';
  OpenPictureDialogTemplateName = 'DLGTEMPLATE';

type
  TGetOpenFileName = function (var OpenFile: TOpenFilename): Bool; stdcall;

var
  OldGetOpenFileName: TGetOpenFileName;
  OldGetSaveFileName: TGetOpenFileName;
  OldExplorerHook: function(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): {$IFDEF RTL230_UP}UINT_PTR{$ELSE}UINT{$ENDIF RTL230_UP}; stdcall;

function NewExplorerHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM; LParam: LPARAM): {$IFDEF RTL230_UP}UINT_PTR{$ELSE}UINT{$ENDIF RTL230_UP}; stdcall;
begin
  Result := OldExplorerHook(Wnd, Msg, WParam, LParam);
  if (Msg = WM_INITDIALOG) and Assigned(GlobalOpenDialogHook) then
  begin
    GlobalOpenDialogHook.FHandle := Wnd;
    GlobalOpenDialogHook.FOldWndInstance := Pointer(SetWindowLongPtr(Wnd, GWLP_WNDPROC, LONG_PTR(GlobalOpenDialogHook.FWndInstance)));
    CallWindowProc(GlobalOpenDialogHook.FWndInstance, Wnd, Msg, WParam, LParam);
  end;
end;

procedure InitOpenFileStruct(var OpenFile: TOpenFilename);
var
  InitDir: string;
begin
  with OpenFile do
    if Flags and OFN_EXPLORER <> 0 then
    begin
      if Assigned(GlobalOpenDialogHook) then
        GlobalOpenDialogHook.FIsOpenPictureDialog := False;
      if Flags and OFN_ENABLETEMPLATE = 0 then
      begin
        OldExplorerHook := lpfnHook;
        lpfnHook := NewExplorerHook;
        lpTemplateName := OpenDialogTemplateName;
        hInstance := FindResourceHInstance(FindClassHInstance(GlobalOpenDialogHook.ClassType));
        Flags := Flags or OFN_ENABLETEMPLATE;
        if Assigned(GlobalOpenDialogHook) then
        begin
          if GlobalOpenDialogHook.DisableHelpButton then
            Flags := Flags and (not OFN_SHOWHELP);
          if GlobalOpenDialogHook.DisablePlacesBar and (lStructSize = SizeOf(TOpenFilename)) then
            FlagsEx := FlagsEx or OFN_EX_NOPLACESBAR;
        end;
      end
      else
      if (StrIComp(lpTemplateName, OpenPictureDialogTemplateName) = 0) and Assigned(GlobalOpenDialogHook) then
      begin
        GlobalOpenDialogHook.FIsOpenPictureDialog := True;
        OldExplorerHook := lpfnHook;
        lpfnHook := NewExplorerHook;
        InitDir := GlobalOpenDialogHook.PictureDialogLastFolder;
        if DirectoryExists(InitDir) then
          lpstrInitialDir := PChar(GlobalOpenDialogHook.PictureDialogLastFolder)
        else
          GlobalOpenDialogHook.PictureDialogLastFolder := '';
      end;
   end;
end;

function NewGetOpenFileName(var OpenFile: TOpenFilename): Bool; stdcall;
begin
  InitOpenFileStruct(OpenFile);
  Result := OldGetOpenFileName(OpenFile);
end;

function NewGetSaveFileName(var OpenFile: TOpenFilename): Bool; stdcall;
begin
  InitOpenFileStruct(OpenFile);
  Result := OldGetSaveFileName(OpenFile);
end;
{$ENDIF OLDSTYLE}

function InitializeOpenDialogHook(OpenDialogHookClass: TJclOpenDialogHookClass): TJclOpenDialogHook;
begin
  if Assigned(GlobalOpenDialogHook) then
  begin
    if GlobalOpenDialogHook.ClassType <> OpenDialogHookClass then
      raise EJclOpenDialogHookError.CreateResFmt(@RsEOpenDialogHookExists, [GlobalOpenDialogHook.ClassName]);
  end
  else
    GlobalOpenDialogHook := OpenDialogHookClass.Create;
  Result := GlobalOpenDialogHook;
end;

procedure FinalizeOpenDialogHook;
begin
  FreeAndNil(GlobalOpenDialogHook);
end;

//=== { TJclFileOpenDialogHook } =============================================

{$IFDEF NEWSTYLE}
function TJclFileOpenDialogHook.CreateFileDialog: IFileDialog;
begin
  Result := inherited CreateFileDialog;
  GlobalOpenDialogHook.FileDialogCreate(Result);
end;

class procedure TJclFileOpenDialogHook.InstallHook(out OldHandler: Pointer);

  function GetActualAddr(Proc: Pointer): Pointer;
  type
    {$IFDEF CPUX64}
    PAbsoluteIndirectJmp64 = ^TAbsoluteIndirectJmp64;
    TAbsoluteIndirectJmp64 = packed record
      OpCode: Word;   //$FF25(Jmp, FF /4)
      Rel: Integer;
    end;
    {$ELSE}
    PAbsoluteIndirectJmp32 = ^TAbsoluteIndirectJmp32;
    TAbsoluteIndirectJmp32 = packed record
      OpCode: Word;   //$FF25(Jmp, FF /4)
      Addr: ^Pointer;
    end;
    {$ENDIF CPUX64}
  begin
    Result := Proc;
    if Result <> nil then
    begin
      {$IFDEF CPUX64}
      if PAbsoluteIndirectJmp64(Result).OpCode = $25FF then
        Result := PPointer(PByte(@PAbsoluteIndirectJmp64(Result).OpCode) +
          SizeOf(TAbsoluteIndirectJmp64) + PAbsoluteIndirectJmp64(Result).Rel)^;
      {$ELSE}
      if (Win32Platform = VER_PLATFORM_WIN32_NT) then
        if PAbsoluteIndirectJmp32(Result).OpCode = $25FF then
          Result := PAbsoluteIndirectJmp32(Result).Addr^;
      {$ENDIF CPUX64}
    end;
  end;

var
  I: Integer;
  P: Pointer;
begin
  P := GetActualAddr(@TFileOpenDialog.CreateFileDialog);
  for I := 0 to GetVirtualMethodCount(TFileOpenDialog) - 1 do
  begin
    OldHandler := GetActualAddr(GetVirtualMethod(TFileOpenDialog, I));
    if OldHandler = P then
    begin
      SetVirtualMethod(TFileOpenDialog, I, @TJclFileOpenDialogHook.CreateFileDialog);
      Exit;
    end;
  end;
  OldHandler := nil;
end;

class procedure TJclFileOpenDialogHook.UninstallHook(OldHandler: Pointer);
var
  I: Integer;
begin
  for I := 0 to GetVirtualMethodCount(TFileOpenDialog) - 1 do
    if GetVirtualMethod(TFileOpenDialog, I) = @TJclFileOpenDialogHook.CreateFileDialog then
  begin
    SetVirtualMethod(TFileOpenDialog, I, OldHandler);
    Break;
  end;
end;
{$ENDIF NEWSTYLE}

//=== { TJclFileSaveDialogHook } =============================================

{$IFDEF NEWSTYLE}
function TJclFileSaveDialogHook.CreateFileDialog: IFileDialog;
begin
  Result := inherited CreateFileDialog;
  GlobalOpenDialogHook.FileDialogCreate(Result);
end;

class procedure TJclFileSaveDialogHook.InstallHook(out OldHandler: Pointer);
var
  I: Integer;
begin
  for I := 0 to GetVirtualMethodCount(TFileSaveDialog) - 1 do
  begin
    OldHandler := GetVirtualMethod(TFileSaveDialog, I);
    if OldHandler = @TFileSaveDialog.CreateFileDialog then
    begin
      SetVirtualMethod(TFileSaveDialog, I, @TJclFileSaveDialogHook.CreateFileDialog);
      Exit;
    end;
  end;
  OldHandler := nil;
end;

class procedure TJclFileSaveDialogHook.UninstallHook(OldHandler: Pointer);
var
  I: Integer;
begin
  for I := 0 to GetVirtualMethodCount(TFileSaveDialog) - 1 do
    if GetVirtualMethod(TFileSaveDialog, I) = @TJclFileSaveDialogHook.CreateFileDialog then
  begin
    SetVirtualMethod(TFileSaveDialog, I, OldHandler);
    Break;
  end;
end;
{$ENDIF NEWSTYLE}

//=== { TJclOpenDialogHook } =================================================

constructor TJclOpenDialogHook.Create;
begin
  inherited Create;
  {$IFDEF OLDSTYLE}
  FHooks := TJclPeMapImgHooks.Create;
  FParentWndInstance := MakeObjectInstance(ParentWndProc);
  FWndInstance := MakeObjectInstance(WndProc);
  {$ENDIF OLDSTYLE}
end;

destructor TJclOpenDialogHook.Destroy;
begin
  UnhookDialogs;
  {$IFDEF OLDSTYLE}
  FreeObjectInstance(FParentWndInstance);
  FreeObjectInstance(FWndInstance);
  FreeAndNil(FHooks);
  {$ENDIF OLDSTYLE}
  inherited Destroy;
end;

{$IFDEF OLDSTYLE}
procedure TJclOpenDialogHook.DialogAdjustControlPos;
begin
  // override to customize
end;

procedure TJclOpenDialogHook.DialogClose;
begin
  // override to customize
end;

procedure TJclOpenDialogHook.DialogFolderChange;
begin
  // override to customize
end;

procedure TJclOpenDialogHook.DialogShow;
begin
  // override to customize
  FParentWnd := GetParent(FHandle);
  if JclCheckWinVersion(5, 0) then // Win2k or newer
    FOldParentWndInstance := Pointer(SetWindowLongPtr(FParentWnd, GWLP_WNDPROC, LONG_PTR(FParentWndInstance)));
  DoShow;
end;

procedure TJclOpenDialogHook.DoClose;
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TJclOpenDialogHook.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;
{$ENDIF OLDSTYLE}

{$IFDEF NEWSTYLE}
procedure TJclOpenDialogHook.FileDialogCreate(const AFileDialog: IFileDialog);
begin
  // override to customize
end;
{$ENDIF NEWSTYLE}

{$IFDEF OLDSTYLE}
function TJclOpenDialogHook.GetCurrentFolder: string;
var
  Path: array [0..MAX_PATH] of Char;
begin
  SetString(Result, Path, SendMessage(FParentWnd, CDM_GETFOLDERPATH, Length(Path), LPARAM(@Path)));
  StrResetLength(Result);
end;

function TJclOpenDialogHook.GetFileNameEditWnd: HWND;
begin
  Result := GetDlgItem(FParentWnd, edt1);
  if Result = 0 then
    Result := GetDlgItem(FParentWnd, cmb13);
end;
{$ENDIF OLDSTYLE}

procedure TJclOpenDialogHook.HookDialogs;
{$IFDEF OLDSTYLE}
  procedure HookImportsForModule(ModuleBase: Pointer);
  const
    comdlg32 = 'comdlg32.dll';
  begin
    if ModuleBase <> nil then
    begin
      {$IFDEF UNICODE}
      FHooks.HookImport(ModuleBase, comdlg32, 'GetOpenFileNameW', @NewGetOpenFileName, @OldGetOpenFileName);
      FHooks.HookImport(ModuleBase, comdlg32, 'GetSaveFileNameW', @NewGetSaveFileName, @OldGetSaveFileName);
      {$ELSE}
      FHooks.HookImport(ModuleBase, comdlg32, 'GetOpenFileNameA', @NewGetOpenFileName, @OldGetOpenFileName);
      FHooks.HookImport(ModuleBase, comdlg32, 'GetSaveFileNameA', @NewGetSaveFileName, @OldGetSaveFileName);
      {$ENDIF UNICODE}
    end;
  end;
var
  Pe: TJclPeImage;
  I: Integer;
  HookedModule: LongWord;
{$ENDIF OLDSTYLE}
begin
  try
    {$IFDEF OLDSTYLE}
    { TODO : Hook all loaded modules }
    Pe := TJclPeImage.Create(True);
    try
      HookedModule := FindClassHInstance(ClassType);
      Pe.AttachLoadedModule(HookedModule);
      if Pe.StatusOK then
      begin
        HookImportsForModule(Pointer(HookedModule));
        for I := 0 to Pe.ImportList.UniqueLibItemCount - 1 do
          HookImportsForModule(Pointer(GetModuleHandle(PChar(Pe.ImportList.UniqueLibItems[I].FileName))));
      end;
    finally
      Pe.Free;
    end;
    {$ENDIF OLDSTYLE}
    {$IFDEF NEWSTYLE}
    TJclFileOpenDialogHook.InstallHook(FOldFileOpenCreateFileDialog);
    TJclFileSaveDialogHook.InstallHook(FOldFileSaveCreateFileDialog);
    {$ENDIF NEWSTYLE}
  except
    Application.HandleException(Self);
  end;
end;

{$IFDEF OLDSTYLE}
procedure TJclOpenDialogHook.ParentWndProc(var Message: TMessage);
begin
  with Message do
  begin
    Result := CallWindowProc(FOldParentWndInstance, FParentWnd, Msg, WParam, LParam);
    if Msg = WM_SIZE then
      DialogAdjustControlPos;
  end;
end;

procedure TJclOpenDialogHook.SetCurrentFolder(const Value: string);
var
  LastFocus: HWND;
  FileNameBuffer: string;
begin
  if (FParentWnd <> 0) and DirectoryExists(Value) then
  begin
    LastFocus := GetFocus;
    FileNameBuffer := GetWindowCaption(FileNameEditWnd);
    SendMessage(FParentWnd, CDM_SETCONTROLTEXT, edt1, LPARAM(PChar(Value)));
    SendMessage(GetDlgItem(FParentWnd, 1), BM_CLICK, 0, 0);
    SendMessage(FParentWnd, CDM_SETCONTROLTEXT, edt1, LPARAM(PChar(FileNameBuffer)));
    SetFocus(LastFocus);
  end;
end;
{$ENDIF OLDSTYLE}

procedure TJclOpenDialogHook.UnhookDialogs;
{$IFDEF OLDSTYLE}
var
  I: Integer;
{$ENDIF OLDSTYLE}
begin
  {$IFDEF OLDSTYLE}
  I := 0;
  while I < FHooks.Count do
    if not FHooks[I].Unhook then
      Inc(I);
  {$ENDIF OLDSTYLE}
  {$IFDEF NEWSTYLE}
  TJclFileOpenDialogHook.UninstallHook(FOldFileOpenCreateFileDialog);
  TJclFileSaveDialogHook.UninstallHook(FOldFileSaveCreateFileDialog);
  {$ENDIF NEWSTYLE}
end;

{$IFDEF OLDSTYLE}
procedure TJclOpenDialogHook.WndProc(var Message: TMessage);

  procedure Default;
  begin
    with Message do
      Result := CallWindowProc(FOldWndInstance, FHandle, Msg, WParam, LParam);
  end;

begin
  if FHandle <> 0 then
  begin
    case Message.Msg of
      WM_NOTIFY:
        begin
          case (POFNotify(Message.LParam)^.hdr.code) of
            CDN_INITDONE:
              begin
                DialogShow;
                DialogAdjustControlPos;
              end;
            CDN_FOLDERCHANGE:
              if not IsOpenPictureDialog then
                DialogFolderChange;
            CDN_FILEOK:
              if IsOpenPictureDialog then
                FPictureDialogLastFolder := CurrentFolder;
          end;
          Default;
        end;
      WM_NCDESTROY:
        begin
          Default;
          FHandle := 0;
        end;
      WM_DESTROY:
        begin
          DialogClose;
          DoClose;
        end;
    else
      Default;
    end;
  end;
end;
{$ENDIF OLDSTYLE}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
