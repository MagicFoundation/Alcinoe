unit Alcinoe.FMX.NativeView.Win;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported132}
  {$MESSAGE WARN 'Check if FMX.Presentation.Win.pas was not updated and adjust the IFDEF'}
{$ENDIF}

uses
  System.Types,
  System.Classes,
  System.Messaging,
  WinApi.Windows,
  Winapi.Messages,
  FMX.Types,
  FMX.Forms,
  FMX.Controls,
  FMX.Controls.Win,
  FMX.Zorder.Win,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Graphics;

type

  { TALWinNativeView }

  TALWinNativeView = class
  private
    class var FCreationControl: TALWinNativeView;
    class var FContainerHandle: HWnd;
    class function GetContainerHandle: HWnd; static;
    class procedure CreateContainerHandle;
    class procedure DestroyContainerHandle;
    class destructor DestroyClass;
  private
    FDefWndProc: Pointer;
    FHandle: HWND;
    FObjectInstance: Pointer;
    procedure WndProc(var Message: TMessage);
    function GetFormHandle: HWND;
  protected
    class property ContainerHandle: HWnd read GetContainerHandle;
    const NullHWnd = 0;
    procedure CreateHandle; virtual;
    procedure CreateParams(var Params: TCreateParams); virtual;
    procedure CreateSubClass(var Params: TCreateParams; ControlClassName: PChar);
    procedure DestroyHandle; virtual;
    property DefWndProc: Pointer read FDefWndProc;
  public
    procedure DefaultHandler(var Message); override;
    property Handle: HWND read FHandle;
  private
    FControl: TALControl;
    FVisible: Boolean;
    FCaptureDC: HDC;
    FCaptureBitmap: HBITMAP;
    FCaptureOldBitmap: HGDIOBJ;
    FCaptureBits: Pointer;
    FCaptureWidth: Integer;
    FCaptureHeight: Integer;
    procedure BeforeDestroyFormHandleListener(const Sender: TObject; const AMessage: System.Messaging.TMessage);
    procedure AfterCreateFormHandleListener(const Sender: TObject; const AMessage: System.Messaging.TMessage);
  protected
    function ExtractPoint(var Message: TWMMouse): TPointF; virtual;
  public
    function GetAbsoluteRect: TRectF;
    procedure SetEnabled(const value: Boolean); virtual; //procedure PMSetAbsoluteEnabled(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_ABSOLUTE_ENABLED;
    procedure SetVisible(const Value: Boolean); virtual; //procedure PMSetVisible(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_VISIBLE;
    procedure SetAlpha(const Value: Single); virtual;
    procedure BringToFront; virtual;
  protected
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var Message: TWMLButtonDown); message WM_RBUTTONDOWN;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMRButtonUp(var Message: TWMLButtonUp); message WM_RBUTTONUP;
    procedure WMLButtonDblClick(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMRButtonDblClick(var Message: TWMRButtonDblClk); message WM_RBUTTONDBLCLK;
    procedure WMMButtonDblClick(var Message: TWMMButtonDblClk); message WM_MBUTTONDBLCLK;
  public
    constructor Create; overload; virtual;
    constructor Create(const AControl: TALControl); overload; virtual;
    destructor Destroy; override;
    procedure SetFocus; virtual;
    procedure ResetFocus; virtual;
    procedure UpdateParent; virtual;
    procedure UpdateFrame; virtual;
    function CaptureScreenshot: TALDrawable; virtual;
    procedure Invalidate;
    property Control: TALControl read FControl;
    property Visible: Boolean read FVisible;
  end;
  TALWinNativeViewClass = class of TALWinNativeView;

implementation

uses
  System.SysUtils,
  System.UITypes,
  {$IF defined(ALSkiaCanvas)}
  System.Skia.API,
  {$ENDIF}
  Winapi.WinSpool,
  FMX.Platform.Win,
  FMX.Helpers.Win,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.NativeControl,
  Alcinoe.Common;

{***********************************************************************************************}
function InitWndProc(HWindow: HWND; Msg: UINT; WParam: WParam; LParam: LParam): LRESULT; stdcall;
{$IFDEF WIN64}
type
  TThunkProc = function(HWindow: HWND; Message: Longint; WParam: Winapi.Windows.WParam; LParam: Winapi.Windows.LParam):
    LRESULT; stdcall;
var
  WinControl: TALWinNativeView;
{$ENDIF}
begin
  TALWinNativeView.FCreationControl.FHandle := HWindow;
  SetWindowLongW(HWindow, GWL_WNDPROC, IntPtr(TALWinNativeView.FCreationControl.FObjectInstance));
  if (GetWindowLongW(HWindow, GWL_STYLE) and WS_CHILD <> 0) and (GetWindowLongW(HWindow, GWL_ID) = 0) then
    SetWindowLongW(HWindow, GWL_ID, HWindow);
{$IFDEF WIN32}
  asm
    PUSH    LParam
    PUSH    WParam
    PUSH    Msg
    PUSH    HWindow
    MOV     EAX,TALWinNativeView.FCreationControl
    MOV     TALWinNativeView.FCreationControl,0
    CALL    [EAX].TALWinNativeView.FObjectInstance
    MOV     Result,EAX
  end;
{$ENDIF}
{$IFDEF WIN64}
  WinControl := TALWinNativeView.FCreationControl;
  TALWinNativeView.FCreationControl := nil;
  Result := TThunkProc(WinControl.FObjectInstance)(HWindow, Msg, WParam, LParam);
{$ENDIF}
end;

{*********************************************}
class destructor TALWinNativeView.DestroyClass;
begin
  DestroyContainerHandle;
end;

{*****************************************************}
class procedure TALWinNativeView.CreateContainerHandle;
const
  FMContainerClass = 'ALFMContainer';
var
  WindowClass: TWndClass;
begin
  if FContainerHandle = NullHWnd then
  begin
    FillChar(WindowClass, SizeOf(WindowClass), 0);
    WindowClass.lpszClassName := FMContainerClass;
    WindowClass.lpfnWndProc := @DefWindowProc;
    WindowClass.hInstance := HInstance;
    if Winapi.Windows.RegisterClass(WindowClass) = 0 then
      RaiseLastOSError;

    FContainerHandle := CreateWindowEx(
                          WS_EX_TOOLWINDOW, WindowClass.lpszClassName, nil, WS_POPUP or WS_CAPTION or
                          WS_CLIPSIBLINGS or WS_SYSMENU or WS_MINIMIZEBOX, GetSystemMetrics(SM_CXSCREEN) div 2,
                          GetSystemMetrics(SM_CYSCREEN) div 2, 0, 0, 0, 0, HInstance, nil);
  end;
end;

{******************************************************}
class procedure TALWinNativeView.DestroyContainerHandle;
begin
  if FContainerHandle <> NullHWnd then
  begin
    DestroyWindow(FContainerHandle);
    FContainerHandle := 0;
  end;
end;

{*******************************************************}
class function TALWinNativeView.GetContainerHandle: HWnd;
begin
  if FContainerHandle = NullHWnd then
    CreateContainerHandle;
  Result := FContainerHandle;
end;

{**********************************}
constructor TALWinNativeView.Create;
begin
  inherited;
  FObjectInstance := MakeObjectInstance(WndProc);
  FVisible := True;
  FCaptureDC := 0;
  FCaptureBitmap := 0;
  FCaptureOldBitmap := 0;
  FCaptureBits := nil;
  FCaptureWidth := 0;
  FCaptureHeight := 0;
  TMessageManager.DefaultManager.SubscribeToMessage(TBeforeDestroyFormHandle, BeforeDestroyFormHandleListener);
  TMessageManager.DefaultManager.SubscribeToMessage(TAfterCreateFormHandle, AfterCreateFormHandleListener);
  CreateHandle;
end;

{**************************************************************}
constructor TALWinNativeView.Create(const AControl: TALControl);
begin
  FControl := AControl;
  Create;
  UpdateParent;
end;

{**********************************}
destructor TALWinNativeView.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TBeforeDestroyFormHandle, BeforeDestroyFormHandleListener);
  TMessageManager.DefaultManager.Unsubscribe(TAfterCreateFormHandle, AfterCreateFormHandleListener);
  DestroyHandle;
  FreeObjectInstance(FObjectInstance);
  if FCaptureBitmap <> 0 then begin
    SelectObject(FCaptureDC, FCaptureOldBitmap);
    DeleteObject(FCaptureBitmap);
  end;
  if FCaptureDC <> 0 then
    DeleteDC(FCaptureDC);
  inherited Destroy;
end;

{***************************************************************************************************************************}
procedure TALWinNativeView.BeforeDestroyFormHandleListener(const Sender: TObject; const AMessage: System.Messaging.TMessage);
begin
  if (AMessage is TBeforeDestroyFormHandle) and
     (TBeforeDestroyFormHandle(AMessage).Value = Control.Form) and
     (GetParent(Handle) = GetFormHandle) then begin
    {$IF defined(DEBUG)}
    ALLog(Classname + '.BeforeDestroyFormHandleListener');
    {$ENDIF}
    SetParent(Handle, NullHWnd);
  end;
end;

{*************************************************************************************************************************}
procedure TALWinNativeView.AfterCreateFormHandleListener(const Sender: TObject; const AMessage: System.Messaging.TMessage);
begin
  // This event is called only when the window's handle is recreated.
  if (AMessage is TAfterCreateFormHandle) and
     (TAfterCreateFormHandle(AMessage).Value = Control.Form) then begin
    {$IF defined(DEBUG)}
    ALLog(Classname + '.AfterCreateFormHandleListener');
    {$ENDIF}
    UpdateParent;
  end;
end;

{**************************************}
procedure TALWinNativeView.CreateHandle;
var
  Params: TCreateParams;
  ClassRegistered: Boolean;
  TempClass: TWndClass;
begin
  if FHandle = NullHWnd then
  begin
    CreateParams(Params);
    with Params do
    begin
      if (WndParent = NullHWnd) and (Style and WS_CHILD <> 0) then
        WndParent := ContainerHandle
      else
        if GetWindowLong(WndParent, GWL_EXSTYLE) or WS_EX_LAYERED <> 0 then
          WndParent := ContainerHandle;

      FDefWndProc := WindowClass.lpfnWndProc;
      ClassRegistered := GetClassInfo(WindowClass.hInstance, WinClassName, TempClass);
      if not ClassRegistered or (TempClass.lpfnWndProc <> @InitWndProc) then
      begin
        if ClassRegistered then
          Winapi.Windows.UnregisterClass(WinClassName, WindowClass.hInstance);
        WindowClass.lpfnWndProc := @InitWndProc;
        WindowClass.lpszClassName := WinClassName;
        if Winapi.Windows.RegisterClass(WindowClass) = 0 then
          RaiseLastOSError;
      end;
      FCreationControl := Self;
      FHandle := CreateWindowEx(
                   ExStyle, WinClassName, Caption, Style, X, Y, Width, Height, WndParent, 0,
                   WindowClass.hInstance, Param);
    end;
    if FHandle = NullHWnd then
      RaiseLastOSError;
    if (GetWindowLong(FHandle, GWL_STYLE) and WS_CHILD <> 0) and (GetWindowLong(FHandle, GWL_ID) = 0) then
      SetWindowLong(FHandle, GWL_ID, FHandle);
  end;
end;

{***************************************}
procedure TALWinNativeView.DestroyHandle;
begin
  if FHandle <> NullHWnd then
  try

    var LNativeChildControls := TALNativeControl(Control).GetNativeChildControls;
    for var I := Low(LNativeChildControls) to High(LNativeChildControls) do
      SetParent(LNativeChildControls[I].NativeView.Handle, NullHWnd);

    if not Winapi.Windows.DestroyWindow(FHandle) then
      RaiseLastOSError;

  finally
    FHandle := NullHWnd;
    FDefWndProc := nil;
  end;
end;

{$IFDEF WIN64}
var
  UserLibrary: THandle;
{$ENDIF  }

{*****************************************************************}
procedure TALWinNativeView.CreateParams(var Params: TCreateParams);
begin
  FillChar(Params, SizeOf(Params), 0);
  Params.Style := WS_CHILD or WS_CLIPSIBLINGS or WS_CLIPCHILDREN or WS_VISIBLE;

  Params.ExStyle := Params.ExStyle or WS_EX_CONTROLPARENT;
  Params.X := 0;
  Params.Y := 0;
  Params.Width := 0;
  Params.Height := 0;

  Params.WndParent := NullHWnd{ParentWindow};
  Params.WindowClass.Style := CS_VREDRAW + CS_HREDRAW + CS_DBLCLKS;
  Params.WindowClass.hCursor := LoadCursor(0, IDC_ARROW);
  Params.WindowClass.hbrBackground := 0;
  Params.WindowClass.hInstance := hInstance;
{$IFDEF WIN64}
  if UserLibrary = 0 then
    UserLibrary := LoadLibrary('User32.dll');
  Params.WindowClass.lpfnWndProc := GetProcAddress(UserLibrary, 'DefWindowProcW');
{$ELSE}
  Params.WindowClass.lpfnWndProc := @DefWindowProc;
{$ENDIF}
  StrPCopy(Params.WinClassName, ClassName);

  if FControl.TabStop then
    Params.Style := Params.Style or WS_TABSTOP;
end;

{********************************************************************************************}
procedure TALWinNativeView.CreateSubClass(var Params: TCreateParams; ControlClassName: PChar);
const
  CS_OFF = CS_OWNDC or CS_CLASSDC or CS_PARENTDC or CS_GLOBALCLASS;
  CS_ON = CS_VREDRAW or CS_HREDRAW;
var
  SaveInstance: HINST;
begin
  if ControlClassName <> nil then
    with Params do
    begin
      {We need to save the hInstance, because GetClassInfo changes it
       and the hInstance must be correct later when we check whether the
       class is already registered}

      SaveInstance := WindowClass.hInstance;
      if not GetClassInfo(HInstance, ControlClassName, WindowClass) and
        not GetClassInfo(MainInstance, ControlClassName, WindowClass) and
        not GetClassInfo(0, ControlClassName, WindowClass) then
        GetClassInfo(WindowClass.hInstance, ControlClassName, WindowClass);

      WindowClass.hInstance := SaveInstance;
      WindowClass.style := WindowClass.style and not CS_OFF or CS_ON;
    end;
end;

{********************************************}
function TALWinNativeView.GetFormHandle: HWND;
begin
  Result := FormToHWND(Control.Form);
end;

{*****************************************************}
procedure TALWinNativeView.DefaultHandler(var Message);
begin
  if FHandle <> NullHWnd then
    TMessage(Message).Result := CallWindowProc(
                                  FDefWndProc, FHandle, TMessage(Message).Msg, TMessage(Message).WParam,
                                  TMessage(Message).LParam)
  else
    inherited DefaultHandler(Message);
end;

{********************************************************}
procedure TALWinNativeView.WndProc(var Message: TMessage);
begin
  try
    Dispatch(Message);
  except
    Application.HandleException(Self);
  end;
end;

{************************************************}
function TALWinNativeView.GetAbsoluteRect: TRectF;
begin
  Result := TALNativeControl(Control).GetNativeViewAbsoluteRect;
end;

{**************************************}
procedure TALWinNativeView.UpdateParent;
begin

  {$IF defined(DEBUG)}
  ALLog(Classname + '.UpdateParent');
  {$ENDIF}

  var LParentHandle: HWND;
  if Control.Form = nil then LParentHandle := NullHWnd
  else LParentHandle := GetFormHandle;

  var LParentControl := Control.ParentControl;
  while LParentControl <> nil do begin
    if LParentControl is TALNativeControl then begin
      if not TALNativeControl(LParentControl).HasNativeView then exit;
      LParentHandle := TALNativeControl(LParentControl).NativeView.Handle;
      break;
    end;
    LParentControl := LParentControl.ParentControl;
  end;

  if GetParent(Handle) = LParentHandle then exit;
  Winapi.Windows.SetParent(Handle, LParentHandle);

  if LParentHandle <> NullHWnd then
    UpdateFrame;

end;

{*************************************}
procedure TALWinNativeView.UpdateFrame;
begin
  {$IF defined(DEBUG)}
  //ALLog(Classname + '.UpdateFrame');
  {$ENDIF}
  var LAbsoluteRect := GetAbsoluteRect;
  var LParentControl := Control.ParentControl;
  while LParentControl <> nil do begin
    if LParentControl is TALNativeControl then begin
      if not TALNativeControl(LParentControl).HasNativeView then exit;
      var LParentRect := TALNativeControl(LParentControl).GetNativeViewAbsoluteRect;
      LAbsoluteRect.Offset(-LParentRect.Left, -LParentRect.Top);
      break;
    end;
    LParentControl := LParentControl.ParentControl;
  end;

  var LWinBounds := TRectF.Create(
                      LAbsoluteRect.Left * ALGetScreenScale,
                      LAbsoluteRect.Top * ALGetScreenScale,
                      LAbsoluteRect.Right * ALGetScreenScale,
                      LAbsoluteRect.Bottom * ALGetScreenScale).Round;
  SetWindowPos(Handle, 0, LWinBounds.Left, LWinBounds.Top, LWinBounds.Width, LWinBounds.Height, SWP_NOZORDER or SWP_NOACTIVATE);
end;

{*******************************************************}
function TALWinNativeView.CaptureScreenshot: TALDrawable;
const
  PW_RENDERFULLCONTENT = $00000002; // Windows 8.1+
begin
  var LClientRect: TRect;
  if not GetClientRect(Handle, LClientRect) then exit(ALNullDrawable);
  var LWidth: Integer := LClientRect.Width;
  var LHeight: Integer := LClientRect.Height;
  if (LWidth <= 0) or (LHeight <= 0) then exit(ALNullDrawable);

  // Create the memory DC once.
  if FCaptureDC = 0 then begin
    FCaptureDC := CreateCompatibleDC(0{DC: HDC});
    if FCaptureDC = 0 then RaiseLastOSError;
  end;

  // Recreate the DIB section only when its dimensions change
  // (like FCaptureBitmap / FCaptureCanvas on Android).
  if (FCaptureBitmap = 0) or
     (FCaptureWidth <> LWidth) or
     (FCaptureHeight <> LHeight) then begin

    if FCaptureBitmap <> 0 then begin
      SelectObject(FCaptureDC, FCaptureOldBitmap);
      DeleteObject(FCaptureBitmap);
      FCaptureBitmap := 0;
      FCaptureBits := nil;
      FCaptureWidth := 0;
      FCaptureHeight := 0;
    end;

    var LBitmapInfo: TBitmapInfo;
    FillChar(LBitmapInfo, SizeOf(LBitmapInfo), 0);
    LBitmapInfo.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
    LBitmapInfo.bmiHeader.biWidth := LWidth;
    LBitmapInfo.bmiHeader.biHeight := -LHeight; // Top-down
    LBitmapInfo.bmiHeader.biPlanes := 1;
    LBitmapInfo.bmiHeader.biBitCount := 32;
    LBitmapInfo.bmiHeader.biCompression := BI_RGB;

    FCaptureBitmap := CreateDIBSection(
                        FCaptureDC, // DC: HDC;
                        LBitmapInfo, // const p2: TBitmapInfo;
                        DIB_RGB_COLORS, // p3: UINT;
                        FCaptureBits, // var p4: Pointer;
                        0, // p5: THandle;
                        0); // p6: DWORD
    if FCaptureBitmap = 0 then RaiseLastOSError;

    FCaptureOldBitmap := SelectObject(FCaptureDC, FCaptureBitmap);
    if (FCaptureOldBitmap = 0) or
       (FCaptureOldBitmap = HGDI_ERROR) then begin
      DeleteObject(FCaptureBitmap);
      FCaptureBitmap := 0;
      FCaptureBits := nil;
      RaiseLastOSError;
    end;
    FCaptureWidth := LWidth;
    FCaptureHeight := LHeight;
  end;

  // Render the window together with its child windows (the child native
  // views are child HWNDs of Handle). PW_RENDERFULLCONTENT is required to
  // capture DirectComposition content such as WebView2.
  if not PrintWindow(Handle, FCaptureDC, PW_RENDERFULLCONTENT) then
    exit(ALNullDrawable);
  GdiFlush;

  // GDI leaves the alpha channel undefined, and the native views are
  // opaque, so force alpha to 255 while copying the pixels.
  var LPixelCount: NativeInt := NativeInt(LWidth) * LHeight;

  {$IF defined(ALSkiaCanvas)}
  var LPixelData: Pointer;
  GetMem(LPixelData, LPixelCount * 4);
  try
    var LSrc := PCardinal(FCaptureBits);
    var LDst := PCardinal(LPixelData);
    for var I := 0 to LPixelCount - 1 do begin
      LDst^ := LSrc^ or $FF000000;
      inc(LSrc);
      inc(LDst);
    end;
    var LImageInfo := ALGetSkImageinfo(LWidth, LHeight);
    LImageInfo.color_type := sk_colortype_t.BGRA8888_SK_COLORTYPE;
    var LPixmap: sk_pixmap_t := ALSkCheckHandle(
                                  sk4d_pixmap_create(
                                    @LImageInfo,
                                    LPixelData,
                                    LWidth * 4));
    try
      Result := ALSkCheckHandle(
                  sk4d_image_make_from_raster(
                    LPixmap, // const pixmap: sk_pixmap_t;
                    @ALReleaseMemPixelBufferProc, // proc: sk_image_raster_release_proc;
                    LPixelData)); // proc_context: Pointer
    finally
      sk4d_refcnt_unref(LPixmap);
    end;
  except
    FreeMem(LPixelData);
    raise;
  end;
  {$ELSE}
  Result := FMX.Graphics.TBitmap.Create(LWidth, LHeight);
  try
    var LBitmapData: TBitmapData;
    if not Result.Map(TMapAccess.Write, LBitmapData) then
      raise Exception.Create('Cannot map bitmap');
    try
      if (LBitmapData.PixelFormat <> TPixelFormat.BGRA) or
         (LBitmapData.BytesPerPixel <> 4) then
        raise Exception.Create('Expected a 32-bit BGRA bitmap');
      for var Y := 0 to LHeight - 1 do begin
        var LSrc := PCardinal(NativeUInt(FCaptureBits) + NativeUInt(Y) * NativeUInt(LWidth * 4));
        var LDst := PCardinal(LBitmapData.GetScanline(Y));
        for var X := 0 to LWidth - 1 do begin
          LDst^ := LSrc^ or $FF000000;
          inc(LSrc);
          inc(LDst);
        end;
      end;
    finally
      Result.Unmap(LBitmapData);
    end;
  except
    ALFreeAndNil(Result);
    raise;
  end;
  {$ENDIF}
end;

{**************************************}
procedure TALWinNativeView.BringToFront;
begin
  SetWindowPos(Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
end;

{**********************************************************}
procedure TALWinNativeView.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  if not Visible or not Control.ParentedVisible then
    ShowWindow(Handle, SW_HIDE)
  else
    ShowWindow(Handle, SW_SHOW);
end;

{*******************************************************}
procedure TALWinNativeView.SetAlpha(const Value: Single);
begin
  // Not supported under Windows
end;

{**********************************************************}
procedure TALWinNativeView.SetEnabled(const value: Boolean);
begin
  EnableWindow(Handle, Value);
end;

{**********************************}
procedure TALWinNativeView.SetFocus;
begin
  Winapi.Windows.SetFocus(Handle);
end;

{************************************}
procedure TALWinNativeView.ResetFocus;
begin
  if GetFocus = Handle then begin
    if (Control.Form <> nil) then
      WinApi.Windows.SetFocus(GetFormHandle)
    else
      SendMessage(Handle, WM_KILLFOCUS, 0, 0);
  end;
end;

{**************************************************************}
procedure TALWinNativeView.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Control.SetFocus;
end;

{****************************************************************}
procedure TALWinNativeView.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Control.ResetFocus;
end;

{******************************************************************}
procedure TALWinNativeView.WMMouseWheel(var Message: TWMMouseWheel);
begin
  var LHandled: Boolean := False;
  try
    if Control.Form <> nil then
      Control.Form.Mousewheel(KeysToShiftState(Message.Keys), Message.WheelDelta, LHandled);
  finally
    If not LHandled then
      inherited;
  end;
end;

{************************************************************}
procedure TALWinNativeView.WMKeyDown(var Message: TWMKeyDown);
var
  KeyCode: Word;
  KeyChar: Char;
begin
  if Control.Form <> nil then
  begin
    KeyCode := Message.CharCode;
    KeyChar := Char(Message.CharCode);
    Control.Form.KeyDown(KeyCode, KeyChar, KeyDataToShiftState(Message.KeyData));
    if (KeyCode <> Message.CharCode) or (KeyChar <> Char(Message.CharCode)) then
      Message.Result := 0
    else
      inherited;
  end
  else
    inherited;
end;

{********************************************************}
procedure TALWinNativeView.WMKeyUp(var Message: TWMKeyUp);
var
  KeyCode: Word;
  KeyChar: Char;
begin
  if Control.Form <> nil then
  begin
    KeyCode := Message.CharCode;
    KeyChar := Char(Message.CharCode);
    Control.Form.KeyUp(KeyCode, KeyChar, KeyDataToShiftState(Message.KeyData));
    if (KeyCode <> Message.CharCode) or (KeyChar <> Char(Message.CharCode)) then
      Message.Result := 0
    else
      inherited;
  end
  else
    inherited;
end;

{********************************************************************}
procedure TALWinNativeView.WMLButtonDown(var Message: TWMLButtonDown);
begin
  try
    if Control.Form <> nil then
    begin
      var FormPoint := ExtractPoint(Message); // dp
      Control.Form.MouseMove([], FormPoint.X, FormPoint.Y); // Require for correct IsMouseOver handle
      Control.Form.MouseDown(TMouseButton.mbLeft, KeysToShiftState(Message.Keys), FormPoint.X, FormPoint.Y);
    end;
  finally
    inherited;
  end;
end;

{****************************************************************}
procedure TALWinNativeView.WMLButtonUp(var Message: TWMLButtonUp);
begin
  try
    if Control.Form <> nil then
    begin
      var FormPoint := ExtractPoint(Message); // dp
      Control.Form.MouseUp(TMouseButton.mbLeft, KeysToShiftState(Message.Keys), FormPoint.X, FormPoint.Y);
    end;
    // Don't combine these if statements because in MouseUp the form can unload presentation!
    if Control.Form <> nil then
      Control.Form.MouseLeave;
  finally
    inherited;
  end;
end;

{**************************************************************************}
procedure TALWinNativeView.WMMButtonDblClick(var Message: TWMMButtonDblClk);
begin
  try
    if Control.Form <> nil then
    begin
      var FormPoint := ExtractPoint(Message); // dp
      Control.Form.MouseDown(TMouseButton.mbMiddle, KeysToShiftState(Message.Keys) + [ssDouble], FormPoint.X, FormPoint.Y);
    end;
  finally
    inherited;
  end;
end;

{****************************************************************}
procedure TALWinNativeView.WMMouseMove(var Message: TWMMouseMove);
begin
  try
    if Control.Form <> nil then
    begin
      var FormPoint := ExtractPoint(Message); // dp
      Control.Form.MouseMove(KeysToShiftState(Message.Keys), FormPoint.X, FormPoint.Y);
    end;
  finally
    inherited;
  end;
end;

{**************************************************************************}
procedure TALWinNativeView.WMLButtonDblClick(var Message: TWMLButtonDblClk);
begin
  try
    if Control.Form <> nil then
    begin
      var FormPoint := ExtractPoint(Message); // dp
      Control.Form.MouseDown(TMouseButton.mbLeft, KeysToShiftState(Message.Keys) + [ssDouble], FormPoint.X, FormPoint.Y);
    end;
  finally
    inherited;
  end;
end;

{**************************************************************************}
procedure TALWinNativeView.WMRButtonDblClick(var Message: TWMRButtonDblClk);
begin
  try
    if Control.Form <> nil then
    begin
      var FormPoint := ExtractPoint(Message); // dp
      Control.Form.MouseDown(TMouseButton.mbRight, KeysToShiftState(Message.Keys) + [ssDouble], FormPoint.X, FormPoint.Y);
    end;
  finally
    inherited;
  end;
end;

{********************************************************************}
procedure TALWinNativeView.WMRButtonDown(var Message: TWMLButtonDown);
var
  ScreenPoint: TPointF;
begin
  try
    if Control.Form <> nil then
    begin
      var FormPoint := ExtractPoint(Message);
      if (Control <> nil) and (Control.PopupMenu <> nil) then
      begin
        ScreenPoint := Control.Form.ClientToScreen(FormPoint);
        IControl(Control).ShowContextMenu(ScreenPoint);
      end;
      Control.Form.MouseMove([], FormPoint.X, FormPoint.Y); // Require for correct IsMouseOver handle
      Control.Form.MouseDown(TMouseButton.mbRight, KeysToShiftState(Message.Keys), FormPoint.X, FormPoint.Y);
    end;
  finally
    if (Control = nil) or (Control.PopupMenu = nil) then
      inherited;
  end;
end;

{****************************************************************}
procedure TALWinNativeView.WMRButtonUp(var Message: TWMLButtonUp);
begin
  try
    if Control.Form <> nil then
    begin
      var FormPoint := ExtractPoint(Message); // dp
      Control.Form.MouseUp(TMouseButton.mbRight, KeysToShiftState(Message.Keys), FormPoint.X, FormPoint.Y);
    end;
    // Don't combine these if statements because in MouseUp the form can unload presentation!
    if Control.Form <> nil then
      Control.Form.MouseLeave;
  finally
    if (Control = nil) or (Control.PopupMenu = nil) then
      inherited;
  end;
end;

{*********************************************************************}
function TALWinNativeView.ExtractPoint(var Message: TWMMouse): TPointF;
var
  Point: TPoint;
begin
  Point := TPoint.Create(Message.XPos, Message.YPos); // px
  ClientToScreen(Handle, Point); // px
  if Control.Form = nil then
    Result := Point
  else
  begin
    ScreenToClient(GetFormHandle, Point); // px
    Result := TPointF.Create(Point.X / Control.Form.Handle.Scale, Point.Y / Control.Form.Handle.Scale); // dp
  end;
end;

{************************************}
procedure TALWinNativeView.Invalidate;
begin
  Winapi.Windows.InvalidateRect(Handle, NiL, TRUE); // Invalidate the entire client area
  UpdateWindow(Handle); // Force an immediate repaint
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.NativeView.Win','initialization');
  {$ENDIF}
  {$IFDEF WIN64}
  UserLibrary := 0;
  {$ENDIF}

end.