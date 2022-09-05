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
{ Last modified: $Date:: 2007-11-13 14:17:59 +0100 (mar., 13 nov. 2007)                          $ }
{ Revision:      $Rev:: 2213                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit exceptionform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, AppEvnts, JclSysUtils, JclDebug, JclAnsiStrings;

const
  UM_CREATEDETAILS = WM_USER + $100;

type
  TExceptionDialog = class(TForm)

    TextLabel: TMemo;
    OkBtn: TButton;
    DetailsBtn: TButton;
    BevelDetails: TBevel;
    DetailsMemo: TMemo;

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
    procedure UpdateTextLabelScrollbars;
  public
    procedure CopyReportToClipboard;
    class procedure ExceptionHandler(Sender: TObject; E: Exception);
    class procedure ExceptionThreadHandler(Thread: TJclDebugThread);
    class procedure ShowException(E: TObject; Thread: TJclDebugThread);
    property DetailsVisible: Boolean read FDetailsVisible
      write SetDetailsVisible;
    property ReportAsText: string read GetReportAsText;

  end;

  TExceptionDialogClass = class of TExceptionDialog;

var
  ExceptionDialogClass: TExceptionDialogClass = TExceptionDialog;

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
  RsMissingVersionInfo = '(no version info)';
  RsMainThreadCallStack = 'Call stack for main thread';
  RsThreadCallStack = 'Call stack for thread %s';

var
  ExceptionDialog: TExceptionDialog;

//============================================================================
// Helper routines
//============================================================================

// SortModulesListByAddressCompare
// sorts module by address
function SortModulesListByAddressCompare(List: TStringList;
  Index1, Index2: Integer): Integer;
var
  Addr1, Addr2: Cardinal;
begin
  Addr1 := Cardinal(List.Objects[Index1]);
  Addr2 := Cardinal(List.Objects[Index2]);
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
    TExceptionDialog.ExceptionHandler(nil, Exception(ExceptObject))
  else
    SysUtils.ShowException(ExceptObject, ExceptAddr);
end;

//----------------------------------------------------------------------------

function HookTApplicationHandleException: Boolean;
const
  CallOffset      = $86;
  CallOffsetDebug = $94;
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
      CallAddress := Pointer(Cardinal(TApplicationHandleExceptionAddr) + Offset);
      CALLInstruction.Call := $E8;
      Result := PCALLInstruction(CallAddress)^.Call = CALLInstruction.Call;
      if Result then
      begin
        if IsCompiledWithPackages then
          Result := PeMapImgResolvePackageThunk(Pointer(Integer(CallAddress) + Integer(PCALLInstruction(CallAddress)^.Address) + SizeOf(CALLInstruction))) = SysUtilsShowExceptionAddr
        else
          Result := PCALLInstruction(CallAddress)^.Address = Integer(SysUtilsShowExceptionAddr) - Integer(CallAddress) - SizeOf(CALLInstruction);
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
    Result := CheckAddressForOffset(CallOffset) or CheckAddressForOffset(CallOffsetDebug);
    if Result then
    begin
      CALLInstruction.Address := Integer(@HookShowException) - Integer(CallAddress) - SizeOf(CALLInstruction);
      Result := WriteProtectedMemory(CallAddress, @CallInstruction, SizeOf(CallInstruction), WrittenBytes);
    end;
  end
  else
    Result := False;
end;

//============================================================================
// Exception dialog with Send
//============================================================================

var
  ExceptionShowing: Boolean;

//=== { TExceptionDialog } ===============================================

procedure TExceptionDialog.AfterCreateDetails;
begin

end;

//----------------------------------------------------------------------------

procedure TExceptionDialog.BeforeCreateDetails;
begin

end;

//----------------------------------------------------------------------------

function TExceptionDialog.ReportMaxColumns: Integer;
begin
  Result := 78;
end;


//----------------------------------------------------------------------------

procedure TExceptionDialog.CopyReportToClipboard;
begin
  ClipBoard.AsText := ReportAsText;
end;

//----------------------------------------------------------------------------

procedure TExceptionDialog.CreateDetails;
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

procedure TExceptionDialog.CreateReport;
var
  SL: TStringList;
  I: Integer;
  ModuleName: TFileName;
  NtHeaders32: PImageNtHeaders32;
  NtHeaders64: PImageNtHeaders64;
  ModuleBase: Cardinal;
  ImageBaseStr: string;
  C: TWinControl;
  CpuInfo: TCpuInfo;
  ProcessorDetails: string;
  StackList: TJclStackInfoList;
  ThreadList: TJclDebugThreadList;
  AThreadID: DWORD; 
  PETarget: TJclPeTarget;
begin
  SL := TStringList.Create;
  try
    // Stack list
    StackList := JclGetExceptStackList(FThreadID);
    if Assigned(StackList) then
    begin
      DetailsMemo.Lines.Add(Format(RsStackList, [DateTimeToStr(StackList.TimeStamp)]));
      StackList.AddToStrings(DetailsMemo.Lines, True, False, True, False);
      NextDetailBlock;
    end;
    // Main thread
    if FThreadID <> MainThreadID then
    begin
      StackList := JclCreateThreadStackTraceFromID(False, MainThreadID);
      if Assigned(StackList) then
      begin
        DetailsMemo.Lines.Add(RsMainThreadCallStack);
        DetailsMemo.Lines.Add(Format(RsStackList, [DateTimeToStr(StackList.TimeStamp)]));
        StackList.AddToStrings(DetailsMemo.Lines, True, False, True, False);
        NextDetailBlock;
      end;
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
          StackList := JclCreateThreadStackTrace(False, ThreadList.ThreadHandles[I]);
          if Assigned(StackList) then
          begin
            DetailsMemo.Lines.Add(Format(RsThreadCallStack, [ThreadList.ThreadInfos[AThreadID]]));
            DetailsMemo.Lines.Add(Format(RsStackList, [DateTimeToStr(StackList.TimeStamp)]));
            StackList.AddToStrings(DetailsMemo.Lines, True, False, True, False);
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
    if sse4A in CpuInfo.SSE then
      ProcessorDetails := ProcessorDetails + ' SSE4A';
    if sse4B in CpuInfo.SSE then
      ProcessorDetails := ProcessorDetails + ' SSE4B';
    if sse5 in CpuInfo.SSE then
      ProcessorDetails := ProcessorDetails + ' SSE';
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
      DetailsMemo.Lines.Add(RsModulesList);
      SL.CustomSort(SortModulesListByAddressCompare);
      for I := 0 to SL.Count - 1 do
      begin
        ModuleName := SL[I];
        ModuleBase := Cardinal(SL.Objects[I]);
        DetailsMemo.Lines.Add(Format('[%.8x] %s', [ModuleBase, ModuleName]));
        PETarget := PeMapImgTarget(Pointer(ModuleBase));
        NtHeaders32 := nil;
        NtHeaders64 := nil;
        if PETarget = taWin32 then
          NtHeaders32 := PeMapImgNtHeaders32(Pointer(ModuleBase))
        else
        if PETarget = taWin64 then
          NtHeaders64 := PeMapImgNtHeaders64(Pointer(ModuleBase));
        if (NtHeaders32 <> nil) and (NtHeaders32^.OptionalHeader.ImageBase <> ModuleBase) then
          ImageBaseStr := Format('<%.8x> ', [NtHeaders32^.OptionalHeader.ImageBase])
        else
        if (NtHeaders64 <> nil) and (NtHeaders64^.OptionalHeader.ImageBase <> ModuleBase) then
          ImageBaseStr := Format('<%.8x> ', [NtHeaders64^.OptionalHeader.ImageBase])
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

procedure TExceptionDialog.DetailsBtnClick(Sender: TObject);
begin
  DetailsVisible := not DetailsVisible;
end;

//--------------------------------------------------------------------------------------------------

class procedure TExceptionDialog.ExceptionHandler(Sender: TObject; E: Exception);
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

class procedure TExceptionDialog.ExceptionThreadHandler(Thread: TJclDebugThread);
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

procedure TExceptionDialog.FormCreate(Sender: TObject);
begin

  FFullHeight := ClientHeight;
  DetailsVisible := False;
  Caption := Format(RsAppError, [Application.Title]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.FormDestroy(Sender: TObject);
begin

end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = Ord('C')) and (ssCtrl in Shift) then
  begin
    CopyReportToClipboard;
    MessageBeep(MB_OK);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.FormPaint(Sender: TObject);
begin
  DrawIcon(Canvas.Handle, TextLabel.Left - GetSystemMetrics(SM_CXICON) - 15,
    TextLabel.Top, LoadIcon(0, IDI_ERROR));
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.FormResize(Sender: TObject);
begin
  UpdateTextLabelScrollbars;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.FormShow(Sender: TObject);
begin
  BeforeCreateDetails;
  MessageBeep(MB_ICONERROR);
  if (GetCurrentThreadId = MainThreadID) and (GetWindowThreadProcessId(Handle, nil) = MainThreadID) then
    PostMessage(Handle, UM_CREATEDETAILS, 0, 0)
  else
    CreateReport;
end;

//--------------------------------------------------------------------------------------------------

function TExceptionDialog.GetReportAsText: string;
begin
  Result := StrEnsureSuffix(AnsiCrLf, TextLabel.Text) + AnsiCrLf + DetailsMemo.Text;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.NextDetailBlock;
begin
  DetailsMemo.Lines.Add(StrRepeat(ReportNewBlockDelimiterChar, ReportMaxColumns));
end;

//--------------------------------------------------------------------------------------------------

function TExceptionDialog.ReportNewBlockDelimiterChar: Char;
begin
  Result := '-';
end;


//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.SetDetailsVisible(const Value: Boolean);
var
  DetailsCaption: string;
begin
  FDetailsVisible := Value;
  DetailsCaption := Trim(StrRemoveChars(DetailsBtn.Caption, ['<', '>']));
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

class procedure TExceptionDialog.ShowException(E: TObject; Thread: TJclDebugThread);
begin
  if ExceptionDialog = nil then
    ExceptionDialog := ExceptionDialogClass.Create(Application);
  try
    with ExceptionDialog do
    begin
      if Assigned(Thread) then
        FThreadID := Thread.ThreadID
      else
        FThreadID := MainThreadID;
      FLastActiveControl := Screen.ActiveControl;
      if E is Exception then
        TextLabel.Text := AdjustLineBreaks(StrEnsureSuffix('.', Exception(E).Message))
      else
        TextLabel.Text := AdjustLineBreaks(StrEnsureSuffix('.', E.ClassName));
      UpdateTextLabelScrollbars;
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
    FreeAndNil(ExceptionDialog);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.UMCreateDetails(var Message: TMessage);
begin
  Update;
  CreateDetails;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.UpdateTextLabelScrollbars;
begin
  Canvas.Font := TextLabel.Font;
  if TextLabel.Lines.Count * Canvas.TextHeight('Wg') > TextLabel.ClientHeight then
    TextLabel.ScrollBars := ssVertical
  else
    TextLabel.ScrollBars := ssNone;   
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
    AppEvents.OnException := TExceptionDialog.ExceptionHandler;




    JclStackTrackingOptions := JclStackTrackingOptions + [stStaticModuleList];
    JclStackTrackingOptions := JclStackTrackingOptions + [stDelayedTrace];
    JclDebugThreadList.OnSyncException := TExceptionDialog.ExceptionThreadHandler;
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
  end;
end;

//--------------------------------------------------------------------------------------------------

initialization
  InitializeHandler;

finalization
  UnInitializeHandler;

end.
