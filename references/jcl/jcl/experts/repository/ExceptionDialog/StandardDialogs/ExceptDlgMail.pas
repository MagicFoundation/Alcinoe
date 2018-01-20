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
{ The Original Code is ExceptDlg.pas.                                                              }
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

unit ExceptDlgMail;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, AppEvnts,
  JclSysUtils, JclMapi, JclUnitVersioning, JclUnitVersioningProviders, JclDebug;


const
  UM_CREATEDETAILS = WM_USER + $100;

type
  TExceptionDialogMail = class(TForm)
    SendBtn: TButton;
    SaveBtn: TButton;
    TextMemo: TMemo;
    OkBtn: TButton;
    DetailsBtn: TButton;
    BevelDetails: TBevel;
    DetailsMemo: TMemo;
    procedure SendBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DetailsBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    private
    FDetailsVisible: Boolean;
    FThreadID: DWORD;
    FLastActiveControl: TWinControl;
    FNonDetailsHeight: Integer;
    FFullHeight: Integer;
    procedure SaveToLogFile(const FileName: TFileName);
    function GetReportAsText: string;
    procedure SetDetailsVisible(const Value: Boolean);
    procedure UMCreateDetails(var Message: TMessage); message UM_CREATEDETAILS;
  protected
    procedure AfterCreateDetails; dynamic;
    procedure BeforeCreateDetails; dynamic;
    procedure CreateDetails; dynamic;
    procedure CreateReport;
    function ReportMaxColumns: Integer; virtual;
    function ReportNewBlockDelimiterChar: Char; virtual;
    procedure NextDetailBlock;
    procedure UpdateTextMemoScrollbars;
  public
    procedure CopyReportToClipboard;
    class procedure ExceptionHandler(Sender: TObject; E: Exception);
    class procedure ExceptionThreadHandler(Thread: TJclDebugThread);
    class procedure ShowException(E: TObject; Thread: TJclDebugThread);
    property DetailsVisible: Boolean read FDetailsVisible
      write SetDetailsVisible;
    property ReportAsText: string read GetReportAsText;
  end;

  TExceptionDialogMailClass = class of TExceptionDialogMail;

var
  ExceptionDialogMailClass: TExceptionDialogMailClass = TExceptionDialogMail;

implementation

{$R *.dfm}

uses
  ClipBrd, Math,
  JclBase, JclFileUtils, JclHookExcept, JclPeImage, JclStrings, JclSysInfo, JclWin32;

resourcestring
  RsAppError = '%s - application error';
  RsExceptionClass = 'Exception class: %s';
  RsExceptionMessage = 'Exception message: %s';
  RsExceptionAddr = 'Exception address: %p';
  RsStackList = 'Stack list, generated %s';
  RsModulesList = 'List of loaded modules:';
  RsOSVersion = 'System   : %s %s, Version: %d.%d, Build: %x, "%s"';
  RsProcessor = 'Processor: %s, %s, %d MHz';
  RsMemory = 'Memory: %d; free %d';
  RsScreenRes = 'Display  : %dx%d pixels, %d bpp';
  RsActiveControl = 'Active Controls hierarchy:';
  RsThread = 'Thread: %s';
  RsMissingVersionInfo = '(no module version info)';
  RsExceptionStack = 'Exception stack';
  RsMainThreadID = 'Main thread ID = %d';
  RsExceptionThreadID = 'Exception thread ID = %d';
  RsMainThreadCallStack = 'Call stack for main thread';
  RsThreadCallStack = 'Call stack for thread %d %s "%s"';
  RsExceptionThreadCallStack = 'Call stack for exception thread %s';
  RsErrorMessage = 'There was an error during the execution of this program.' + NativeLineBreak +
                   'The application might become unstable and even useless.' + NativeLineBreak +
                   'It''s recommended that you save your work and close this application.' + NativeLineBreak + NativeLineBreak;
  RsDetailsIntro = 'Exception log with detailed tech info. Generated on %s.' + NativeLineBreak +
                   'You may send it to the application vendor, helping him to understand what had happened.' + NativeLineBreak +
                   ' Application title: %s' + NativeLineBreak +
                   ' Application file: %s';
  RsUnitVersioningIntro = 'Unit versioning information:';

var
  ExceptionDialogMail: TExceptionDialogMail;

//============================================================================
// Helper routines
//============================================================================

// SortModulesListByAddressCompare
// sorts module by address
function SortModulesListByAddressCompare(List: TStringList;
  Index1, Index2: Integer): Integer;
var
  Addr1, Addr2: TJclAddr;
begin
  Addr1 := TJclAddr(List.Objects[Index1]);
  Addr2 := TJclAddr(List.Objects[Index2]);
  if Addr1 > Addr2 then
    Result := 1
  else if Addr1 < Addr2 then
    Result := -1
  else
    Result := 0;
end;

//============================================================================
// TApplication.HandleException method code hooking for exceptions from DLLs
//============================================================================

// We need to catch the last line of TApplication.HandleException method:
// [...]
//   end else
//    SysUtils.ShowException(ExceptObject, ExceptAddr);
// end;

procedure HookShowException(ExceptObject: TObject; ExceptAddr: Pointer);
begin
  if JclValidateModuleAddress(ExceptAddr)
    and (ExceptObject.InstanceSize >= Exception.InstanceSize) then
    TExceptionDialogMail.ExceptionHandler(nil, Exception(ExceptObject))
  else
    SysUtils.ShowException(ExceptObject, ExceptAddr);
end;

//----------------------------------------------------------------------------

function HookTApplicationHandleException: Boolean;
const
  CallOffset      = $86;   // Until D2007
  CallOffsetDebug = $94;   // Until D2007
  CallOffsetWin32 = $7A;   // D2009 and newer
  CallOffsetWin64 = $95;   // DXE2 for Win64
type
  PCALLInstruction = ^TCALLInstruction;
  TCALLInstruction = packed record
    Call: Byte;
    Address: Integer;
  end;
var
  TApplicationHandleExceptionAddr, SysUtilsShowExceptionAddr: Pointer;
  CALLInstruction: TCALLInstruction;
  CallAddress: Pointer;
  WrittenBytes: Cardinal;

  function CheckAddressForOffset(Offset: Cardinal): Boolean;
  begin
    try
      CallAddress := Pointer(TJclAddr(TApplicationHandleExceptionAddr) + Offset);
      CALLInstruction.Call := $E8;
      Result := PCALLInstruction(CallAddress)^.Call = CALLInstruction.Call;
      if Result then
      begin
        if IsCompiledWithPackages then
          Result := PeMapImgResolvePackageThunk(Pointer(SizeInt(CallAddress) + Integer(PCALLInstruction(CallAddress)^.Address) + SizeOf(CALLInstruction))) = SysUtilsShowExceptionAddr
        else
          Result := PCALLInstruction(CallAddress)^.Address = SizeInt(SysUtilsShowExceptionAddr) - SizeInt(CallAddress) - SizeOf(CALLInstruction);
      end;
    except
      Result := False;
    end;
  end;

begin
  TApplicationHandleExceptionAddr := PeMapImgResolvePackageThunk(@TApplication.HandleException);
  SysUtilsShowExceptionAddr := PeMapImgResolvePackageThunk(@SysUtils.ShowException);
  if Assigned(TApplicationHandleExceptionAddr) and Assigned(SysUtilsShowExceptionAddr) then
  begin
    Result := CheckAddressForOffset(CallOffset) or CheckAddressForOffset(CallOffsetDebug) or
      CheckAddressForOffset(CallOffsetWin32) or CheckAddressForOffset(CallOffsetWin64);
    if Result then
    begin
      CALLInstruction.Address := SizeInt(@HookShowException) - SizeInt(CallAddress) - SizeOf(CALLInstruction);
      Result := WriteProtectedMemory(CallAddress, @CallInstruction, SizeOf(CallInstruction), WrittenBytes);
    end;
  end
  else
    Result := False;
end;

//============================================================================
// Exception dialog
//============================================================================

var
  ExceptionShowing: Boolean;

//=== { TExceptionDialogMail } ===============================================

procedure TExceptionDialogMail.AfterCreateDetails;
begin
  SendBtn.Enabled := True;
  SaveBtn.Enabled := True;
end;

//----------------------------------------------------------------------------

procedure TExceptionDialogMail.BeforeCreateDetails;
begin
  SendBtn.Enabled := False;
  SaveBtn.Enabled := False;
end;

//----------------------------------------------------------------------------

function TExceptionDialogMail.ReportMaxColumns: Integer;
begin
  Result := 78;
end;

//----------------------------------------------------------------------------

procedure TExceptionDialogMail.SendBtnClick(Sender: TObject);
begin
  with TJclEmail.Create do
  try
    ParentWnd := Application.Handle;
    Recipients.Add('name@domain.ext');
    Subject := 'email subject';
    Body := AnsiString(ReportAsText);
    SaveTaskWindows;
    try
      Send(True);
    finally
      RestoreTaskWindows;
    end;
  finally
    Free;
  end;
end;

//----------------------------------------------------------------------------

procedure TExceptionDialogMail.SaveBtnClick(Sender: TObject);
begin
  with TSaveDialog.Create(Self) do
  try
    DefaultExt := '.log';
    FileName := 'filename.log';
    Filter := 'Log Files (*.log)|*.log|All files (*.*)|*.*';
    Title := 'Save log as...';
    Options := [ofHideReadOnly,ofPathMustExist,ofNoReadOnlyReturn,ofEnableSizing,ofDontAddToRecent];
    if Execute then
      SaveToLogFile(FileName);
  finally
    Free;    
  end;
end;

//----------------------------------------------------------------------------

procedure TExceptionDialogMail.CopyReportToClipboard;
begin
  ClipBoard.AsText := ReportAsText;
end;

//----------------------------------------------------------------------------

procedure TExceptionDialogMail.CreateDetails;
begin
  Screen.Cursor := crHourGlass;
  DetailsMemo.Lines.BeginUpdate;
  try
    CreateReport;
    DetailsMemo.SelStart := 0;
    SendMessage(DetailsMemo.Handle, EM_SCROLLCARET, 0, 0);
    AfterCreateDetails;
  finally
    DetailsMemo.Lines.EndUpdate;
    OkBtn.Enabled := True;
    DetailsBtn.Enabled := True;
    OkBtn.SetFocus;
    Screen.Cursor := crDefault;
  end;
end;

//----------------------------------------------------------------------------

procedure TExceptionDialogMail.CreateReport;
var
  SL: TStringList;
  I: Integer;
  ModuleName: TFileName;
  NtHeaders32: PImageNtHeaders32;
  NtHeaders64: PImageNtHeaders64;
  ModuleBase: TJclAddr;
  ImageBaseStr: string;
  C: TWinControl;
  CpuInfo: TCpuInfo;
  ProcessorDetails: string;
  StackList: TJclStackInfoList;
  ThreadList: TJclDebugThreadList;
  AThreadID: DWORD;
  PETarget: TJclPeTarget;
  UnitVersioning: TUnitVersioning;
  UnitVersioningModule: TUnitVersioningModule;
  UnitVersion: TUnitVersion;
  ModuleIndex, UnitIndex: Integer;
begin
  DetailsMemo.Lines.Add(Format(LoadResString(PResStringRec(@RsMainThreadID)), [MainThreadID]));
  DetailsMemo.Lines.Add(Format(LoadResString(PResStringRec(@RsExceptionThreadID)), [MainThreadID]));
  NextDetailBlock;

  SL := TStringList.Create;
  try
    // Except stack list
    StackList := JclGetExceptStackList(FThreadID);
    if Assigned(StackList) then
    begin
      DetailsMemo.Lines.Add(RsExceptionStack);
      DetailsMemo.Lines.Add(Format(LoadResString(PResStringRec(@RsStackList)), [DateTimeToStr(StackList.TimeStamp)]));
      StackList.AddToStrings(DetailsMemo.Lines, True, True, True, True);
      NextDetailBlock;
    end;

    // Main thread
    StackList := JclCreateThreadStackTraceFromID(True, MainThreadID);
    if Assigned(StackList) then
    begin
      DetailsMemo.Lines.Add(LoadResString(PResStringRec(@RsMainThreadCallStack)));
      DetailsMemo.Lines.Add(Format(LoadResString(PResStringRec(@RsStackList)), [DateTimeToStr(StackList.TimeStamp)]));
      StackList.AddToStrings(DetailsMemo.Lines, True, True, True, True);
      NextDetailBlock;
    end;

    // All threads
    ThreadList := JclDebugThreadList;
    ThreadList.Lock.Enter; // avoid modifications
    try
      for I := 0 to ThreadList.ThreadIDCount - 1 do
      begin
        AThreadID := ThreadList.ThreadIDs[I];
        if (AThreadID <> FThreadID) then
        begin
          StackList := JclCreateThreadStackTrace(True, ThreadList.ThreadHandles[I]);
          if Assigned(StackList) then
          begin
            DetailsMemo.Lines.Add(Format(RsThreadCallStack, [AThreadID, ThreadList.ThreadInfos[AThreadID], ThreadList.ThreadNames[AThreadID]]));
            DetailsMemo.Lines.Add(Format(LoadResString(PResStringRec(@RsStackList)), [DateTimeToStr(StackList.TimeStamp)]));
            StackList.AddToStrings(DetailsMemo.Lines, True, True, True, True);
            NextDetailBlock;
          end;
        end;
      end;
    finally
      ThreadList.Lock.Leave;
    end;

    // System and OS information
    DetailsMemo.Lines.Add(Format(RsOSVersion, [GetWindowsVersionString, NtProductTypeString,
      Win32MajorVersion, Win32MinorVersion, Win32BuildNumber, Win32CSDVersion]));
    GetCpuInfo(CpuInfo);
    ProcessorDetails := Format(RsProcessor, [CpuInfo.Manufacturer, CpuInfo.CpuName,
      RoundFrequency(CpuInfo.FrequencyInfo.NormFreq)]);
    if not CpuInfo.IsFDIVOK then
      ProcessorDetails := ProcessorDetails + ' [FDIV Bug]';
    if CpuInfo.ExMMX then
      ProcessorDetails := ProcessorDetails + ' MMXex';
    if CpuInfo.MMX then
      ProcessorDetails := ProcessorDetails + ' MMX';
    if sse in CpuInfo.SSE then
      ProcessorDetails := ProcessorDetails + ' SSE';
    if sse2 in CpuInfo.SSE then
      ProcessorDetails := ProcessorDetails + ' SSE2';
    if sse3 in CpuInfo.SSE then
      ProcessorDetails := ProcessorDetails + ' SSE3';
    if ssse3 in CpuInfo.SSE then
      ProcessorDetails := ProcessorDetails + ' SSSE3';
    if sse41 in CpuInfo.SSE then
      ProcessorDetails := ProcessorDetails + ' SSE41';
    if sse42 in CpuInfo.SSE then
      ProcessorDetails := ProcessorDetails + ' SSE42';
    if sse4A in CpuInfo.SSE then
      ProcessorDetails := ProcessorDetails + ' SSE4A';
    if sse5 in CpuInfo.SSE then
      ProcessorDetails := ProcessorDetails + ' SSE5';
    if CpuInfo.Ex3DNow then
      ProcessorDetails := ProcessorDetails + ' 3DNow!ex';
    if CpuInfo._3DNow then
      ProcessorDetails := ProcessorDetails + ' 3DNow!';
    if CpuInfo.Is64Bits then
      ProcessorDetails := ProcessorDetails + ' 64 bits';
    if CpuInfo.DEPCapable then
      ProcessorDetails := ProcessorDetails + ' DEP';
    DetailsMemo.Lines.Add(ProcessorDetails);
    DetailsMemo.Lines.Add(Format(RsMemory, [GetTotalPhysicalMemory div 1024 div 1024,
      GetFreePhysicalMemory div 1024 div 1024]));
    DetailsMemo.Lines.Add(Format(RsScreenRes, [Screen.Width, Screen.Height, GetBPP]));
    NextDetailBlock;

    // Modules list
    if LoadedModulesList(SL, GetCurrentProcessId) then
    begin
      UnitVersioning := GetUnitVersioning;
      UnitVersioning.RegisterProvider(TJclDefaultUnitVersioningProvider);
      DetailsMemo.Lines.Add(RsModulesList);
      SL.CustomSort(SortModulesListByAddressCompare);
      for I := 0 to SL.Count - 1 do
      begin
        ModuleName := SL[I];
        ModuleBase := TJclAddr(SL.Objects[I]);
        DetailsMemo.Lines.Add(Format('[' + HexDigitFmt + '] %s', [ModuleBase, ModuleName]));
        PETarget := PeMapImgTarget(Pointer(ModuleBase));
        NtHeaders32 := nil;
        NtHeaders64 := nil;
        if PETarget = taWin32 then
          NtHeaders32 := PeMapImgNtHeaders32(Pointer(ModuleBase))
        else
        if PETarget = taWin64 then
          NtHeaders64 := PeMapImgNtHeaders64(Pointer(ModuleBase));
        if (NtHeaders32 <> nil) and (NtHeaders32^.OptionalHeader.ImageBase <> ModuleBase) then
          ImageBaseStr := Format('<' + HexDigitFmt32 + '> ', [NtHeaders32^.OptionalHeader.ImageBase])
        else
        if (NtHeaders64 <> nil) and (NtHeaders64^.OptionalHeader.ImageBase <> ModuleBase) then
          ImageBaseStr := Format('<' + HexDigitFmt64 + '> ', [NtHeaders64^.OptionalHeader.ImageBase])
        else
          ImageBaseStr := StrRepeat(' ', 11);
        if VersionResourceAvailable(ModuleName) then
          with TJclFileVersionInfo.Create(ModuleName) do
          try
            DetailsMemo.Lines.Add(ImageBaseStr + BinFileVersion + ' - ' + FileVersion);
            if FileDescription <> '' then
              DetailsMemo.Lines.Add(StrRepeat(' ', 11) + FileDescription);
          finally
            Free;
          end
        else
          DetailsMemo.Lines.Add(ImageBaseStr + RsMissingVersionInfo);
        for ModuleIndex := 0 to UnitVersioning.ModuleCount - 1 do
        begin
          UnitVersioningModule := UnitVersioning.Modules[ModuleIndex];
          if UnitVersioningModule.Instance = ModuleBase then
          begin
            if UnitVersioningModule.Count > 0 then
              DetailsMemo.Lines.Add(StrRepeat(' ', 11) + LoadResString(PResStringRec(@RsUnitVersioningIntro)));
            for UnitIndex := 0 to UnitVersioningModule.Count - 1 do
            begin
              UnitVersion := UnitVersioningModule.Items[UnitIndex];
              DetailsMemo.Lines.Add(Format('%s%s %s %s %s', [StrRepeat(' ', 13), UnitVersion.LogPath, UnitVersion.RCSfile, UnitVersion.Revision, UnitVersion.Date]));
            end;
          end;
        end;
      end;
      NextDetailBlock;
    end;

    // Active controls
    if (FLastActiveControl <> nil) then
    begin
      DetailsMemo.Lines.Add(RsActiveControl);
      C := FLastActiveControl;
      while C <> nil do
      begin
        DetailsMemo.Lines.Add(Format('%s "%s"', [C.ClassName, C.Name]));
        C := C.Parent;
      end;
      NextDetailBlock;
    end;
  finally
    SL.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.DetailsBtnClick(Sender: TObject);
begin
  DetailsVisible := not DetailsVisible;
end;

//--------------------------------------------------------------------------------------------------

class procedure TExceptionDialogMail.ExceptionHandler(Sender: TObject; E: Exception);
begin
  if Assigned(E) then
    if ExceptionShowing then
      Application.ShowException(E)
    else
    begin
      ExceptionShowing := True;
      try
        if IsIgnoredException(E.ClassType) then
          Application.ShowException(E)
        else
          ShowException(E, nil);
      finally
        ExceptionShowing := False;
      end;
    end;
end;

//--------------------------------------------------------------------------------------------------

class procedure TExceptionDialogMail.ExceptionThreadHandler(Thread: TJclDebugThread);
var
  E: Exception;
begin
  E := Exception(Thread.SyncException);
  if Assigned(E) then
    if ExceptionShowing then
      Application.ShowException(E)
    else
    begin
      ExceptionShowing := True;
      try
        if IsIgnoredException(E.ClassType) then
          Application.ShowException(E)
        else
          ShowException(E, Thread);
      finally
        ExceptionShowing := False;
      end;
    end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.FormCreate(Sender: TObject);
begin
  FFullHeight := ClientHeight;
  DetailsVisible := False;
  Caption := Format(RsAppError, [Application.Title]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.FormDestroy(Sender: TObject);
begin

end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = Ord('C')) and (ssCtrl in Shift) then
  begin
    CopyReportToClipboard;
    MessageBeep(MB_OK);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.FormPaint(Sender: TObject);
begin
  DrawIcon(Canvas.Handle, TextMemo.Left - GetSystemMetrics(SM_CXICON) - 15,
    TextMemo.Top, LoadIcon(0, IDI_ERROR));
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.FormResize(Sender: TObject);
begin
  UpdateTextMemoScrollbars;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.FormShow(Sender: TObject);
begin
  BeforeCreateDetails;
  MessageBeep(MB_ICONERROR);
  if (GetCurrentThreadId = MainThreadID) and (GetWindowThreadProcessId(Handle, nil) = MainThreadID) then
    PostMessage(Handle, UM_CREATEDETAILS, 0, 0)
  else
    CreateReport;
end;

//--------------------------------------------------------------------------------------------------

function TExceptionDialogMail.GetReportAsText: string;
begin
  Result := StrEnsureSuffix(NativeCrLf, TextMemo.Text) + NativeCrLf + DetailsMemo.Text;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.NextDetailBlock;
begin
  DetailsMemo.Lines.Add(StrRepeat(ReportNewBlockDelimiterChar, ReportMaxColumns));
end;

//--------------------------------------------------------------------------------------------------

function TExceptionDialogMail.ReportNewBlockDelimiterChar: Char;
begin
  Result := '-';
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.SaveToLogFile(const FileName: TFileName);
var
  SimpleLog: TJclSimpleLog;
begin
  SimpleLog := TJclSimpleLog.Create(FileName);
  try
    SimpleLog.WriteStamp(ReportMaxColumns);
    SimpleLog.Write(ReportAsText);
    SimpleLog.CloseLog;
  finally
    SimpleLog.Free;
  end;
end;
//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.SetDetailsVisible(const Value: Boolean);
const
  DirectionChars: array [0..1] of Char = ( '<', '>' );
var
  DetailsCaption: string;
begin
  FDetailsVisible := Value;
  DetailsCaption := Trim(StrRemoveChars(DetailsBtn.Caption, DirectionChars));
  if Value then
  begin
    Constraints.MinHeight := FNonDetailsHeight + 100;
    Constraints.MaxHeight := Screen.Height;
    DetailsCaption := '<< ' + DetailsCaption;
    ClientHeight := FFullHeight;
    DetailsMemo.Height := FFullHeight - DetailsMemo.Top - 3;
  end
  else
  begin
    FFullHeight := ClientHeight;
    DetailsCaption := DetailsCaption + ' >>';
    if FNonDetailsHeight = 0 then
    begin
      ClientHeight := BevelDetails.Top;
      FNonDetailsHeight := Height;
    end
    else
      Height := FNonDetailsHeight;
    Constraints.MinHeight := FNonDetailsHeight;
    Constraints.MaxHeight := FNonDetailsHeight
  end;
  DetailsBtn.Caption := DetailsCaption;
  DetailsMemo.Enabled := Value;
end;

//--------------------------------------------------------------------------------------------------

class procedure TExceptionDialogMail.ShowException(E: TObject; Thread: TJclDebugThread);
begin
  if ExceptionDialogMail = nil then
    ExceptionDialogMail := ExceptionDialogMailClass.Create(Application);
  try
    with ExceptionDialogMail do
    begin
      if Assigned(Thread) then
        FThreadID := Thread.ThreadID
      else
        FThreadID := MainThreadID;
      FLastActiveControl := Screen.ActiveControl;
      if E is Exception then
        TextMemo.Text := RsErrorMessage + AdjustLineBreaks(StrEnsureSuffix('.', Exception(E).Message))
      else
        TextMemo.Text := RsErrorMessage + AdjustLineBreaks(StrEnsureSuffix('.', E.ClassName));
      UpdateTextMemoScrollbars;
      NextDetailBlock;
      //Arioch: some header for possible saving to txt-file/e-mail/clipboard/NTEvent...
      DetailsMemo.Lines.Add(Format(RsDetailsIntro, [DateTimeToStr(Now), Application.Title, Application.ExeName]));
      NextDetailBlock;
      DetailsMemo.Lines.Add(Format(RsExceptionClass, [E.ClassName]));
      if E is Exception then
        DetailsMemo.Lines.Add(Format(RsExceptionMessage, [StrEnsureSuffix('.', Exception(E).Message)]));
      if Thread = nil then
        DetailsMemo.Lines.Add(Format(RsExceptionAddr, [ExceptAddr]))
      else
        DetailsMemo.Lines.Add(Format(RsThread, [Thread.ThreadInfo]));
      NextDetailBlock;
      ShowModal;
    end;
  finally
    FreeAndNil(ExceptionDialogMail);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.UMCreateDetails(var Message: TMessage);
begin
  Update;
  CreateDetails;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialogMail.UpdateTextMemoScrollbars;
begin
  Canvas.Font := TextMemo.Font;
  if TextMemo.Lines.Count * Canvas.TextHeight('Wg') > TextMemo.ClientHeight then
    TextMemo.ScrollBars := ssVertical
  else
    TextMemo.ScrollBars := ssNone;   
end;

//==================================================================================================
// Exception handler initialization code
//==================================================================================================

var
  AppEvents: TApplicationEvents = nil;

procedure InitializeHandler;
begin
  if AppEvents = nil then
  begin
    AppEvents := TApplicationEvents.Create(nil);
    AppEvents.OnException := TExceptionDialogMail.ExceptionHandler;
    JclStackTrackingOptions := JclStackTrackingOptions + [stRawMode];
    JclStackTrackingOptions := JclStackTrackingOptions + [stStaticModuleList];
    JclStackTrackingOptions := JclStackTrackingOptions + [stDelayedTrace];
    JclDebugThreadList.OnSyncException := TExceptionDialogMail.ExceptionThreadHandler;
    JclHookThreads;
    JclStartExceptionTracking;
    if HookTApplicationHandleException then
      JclTrackExceptionsFromLibraries;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure UnInitializeHandler;
begin
  if AppEvents <> nil then
  begin
    FreeAndNil(AppEvents);
    JclDebugThreadList.OnSyncException := nil;
    JclUnhookExceptions;
    JclStopExceptionTracking;
    JclUnhookThreads;
  end;
end;

//--------------------------------------------------------------------------------------------------

initialization
  InitializeHandler;

finalization
  UnInitializeHandler;

end.
