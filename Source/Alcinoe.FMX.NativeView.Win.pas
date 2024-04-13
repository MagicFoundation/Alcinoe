unit Alcinoe.FMX.NativeView.Win;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported120}
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
  FMX.Controls.Presentation,
  FMX.Controls.Win,
  FMX.Controls.Model,
  FMX.Graphics,
  FMX.Presentation.Messages,
  FMX.Zorder.Win,
  FMX.Helpers.Win;

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
    procedure SetParentWindow(const Value: HWND);
    function GetParentWindow: HWND;
    procedure WndProc(var Message: TMessage);
    function GetHandle: HWND;
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
    procedure RecreateWnd;
    function HandleAllocated: Boolean; inline;
    property Handle: HWND read GetHandle;
    property ParentWindow: HWND read GetParentWindow write SetParentWindow;
  private
    [Weak] FControl: TControl;
    [Weak] FForm: TCommonCustomForm;
    FSize: TSizeF; // FControlSize: TSizeF;
    function GetZOrderManager: TWinZOrderManager;
    procedure BeforeDestroyMessageListener(const Sender: TObject; const AMessage: System.Messaging.TMessage);
    procedure AfterCreateMessageListener(const Sender: TObject; const AMessage: System.Messaging.TMessage);
  protected
    procedure SetSize(const ASize: TSizeF); virtual;
    function ExtractPoint(var Message: TWMMouse): TPointF; virtual;
  protected
    { Messages from PresentationProxy }
    //procedure PMGetNativeObject(var AMessage: TDispatchMessageWithValue<IInterface>); message PM_GET_NATIVE_OBJECT;
    procedure RootChanged(const aRoot: IRoot); //procedure PMRootChanged(var AMessage: TDispatchMessageWithValue<IRoot>); message PM_ROOT_CHANGED;
    //procedure PMAncestorPresentationLoaded(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_ANCESTOR_PRESENTATION_LOADED;
    //procedure PMAncestorPresentationUnloading(var AMessage: TDispatchMessageWithValue<TControl>); message PM_ANCESTOR_PRESENTATION_UNLOADING;
    //procedure PMUnload(var AMessage: TDispatchMessage); message PM_UNLOAD;
    //procedure PMRefreshParent(var AMessage: TDispatchMessage); message PM_REFRESH_PARENT;
    procedure ChangeOrder; //procedure PMChangeOrder(var AMessage: TDispatchMessage); message PM_CHANGE_ORDER;
    //procedure PMAbsoluteChanged(var AMessage: TDispatchMessage); message PM_ABSOLUTE_CHANGED;
    //procedure PMSetSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_SET_SIZE;
    //procedure PMGetSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_GET_SIZE;
    procedure SetVisible(const Value: Boolean); //procedure PMSetVisible(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_VISIBLE;
    //procedure PMGetVisible(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_GET_VISIBLE;
    procedure AncestorVisibleChanged; //procedure PMAncesstorVisibleChanged(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_ANCESSTOR_VISIBLE_CHANGED;
    procedure SetAbsoluteEnabled(const value: Boolean); //procedure PMSetAbsoluteEnabled(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_ABSOLUTE_ENABLED;
    //procedure PMGetAbsoluteEnabled(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_GET_ABSOLUTE_ENABLED;
    //procedure PMDoExit(var AMessage: TDispatchMessage); message PM_DO_EXIT;
    //procedure PMDoEnter(var AMessage: TDispatchMessage); message PM_DO_ENTER;
    //procedure PMResetFocus(var AMessage: TDispatchMessage); message PM_RESET_FOCUS;
    function PointInObjectLocal(X: Single; Y: Single): Boolean;
  protected
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
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
  protected
    procedure UpdateFrame;
    procedure RefreshNativeParent; virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(const AControl: TControl); overload; virtual;
    destructor Destroy; override;
    function HasZOrderManager: Boolean;
    procedure SetFocus; virtual;
    procedure ResetFocus; virtual;
    property Form: TCommonCustomForm read FForm;
    property ZOrderManager: TWinZOrderManager read GetZOrderManager;
    procedure Invalidate;
  public
    property Control: TControl read FControl;
    property Size: TSizeF read FSize write SetSize;
  end;
  TALWinNativeViewClass = class of TALWinNativeView;

implementation

uses
  System.SysUtils,
  System.UITypes,
  FMX.Platform.Win,
  FMX.Consts;

{**********************************}
constructor TALWinNativeView.Create;
begin
  inherited;
  FObjectInstance := MakeObjectInstance(WndProc);
  TMessageManager.DefaultManager.SubscribeToMessage(TBeforeDestroyFormHandle, BeforeDestroyMessageListener);
  TMessageManager.DefaultManager.SubscribeToMessage(TAfterCreateFormHandle, AfterCreateMessageListener);
end;

{**********************************}
destructor TALWinNativeView.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TBeforeDestroyFormHandle, BeforeDestroyMessageListener);
  TMessageManager.DefaultManager.Unsubscribe(TAfterCreateFormHandle, AfterCreateMessageListener);
  DestroyHandle;
  FreeObjectInstance(FObjectInstance);
  inherited Destroy;
end;








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
procedure TALWinNativeView.DefaultHandler(var Message);
begin
  if (FHandle <> NullHWnd) and (TMessage(Message).Msg < PM_BASE) then
    TMessage(Message).Result := CallWindowProc(FDefWndProc, FHandle, TMessage(Message).Msg, TMessage(Message).WParam,
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

  Params.WndParent := ParentWindow;
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
      FHandle := CreateWindowEx(ExStyle, WinClassName, Caption, Style, X, Y, Width, Height, WndParent, 0,
        WindowClass.hInstance, Param);
    end;
    if FHandle = NullHWnd then
      RaiseLastOSError;
    if (GetWindowLong(FHandle, GWL_STYLE) and WS_CHILD <> 0) and (GetWindowLong(FHandle, GWL_ID) = 0) then
      SetWindowLong(FHandle, GWL_ID, FHandle);
  end;

  if HasZOrderManager then
    ZOrderManager.AddOrSetLink(Control, Handle, NullHWnd);
end;

{***************************************}
procedure TALWinNativeView.DestroyHandle;
begin
  if HasZOrderManager then
    ZOrderManager.RemoveLink(Control);

  if FHandle <> NullHWnd then
  try
    if not Winapi.Windows.DestroyWindow(FHandle) then
      RaiseLastOSError;
  finally
    FHandle := NullHWnd;
    FDefWndProc := nil;
  end;
end;

{*************************************}
procedure TALWinNativeView.RecreateWnd;
begin
  // to-do recreate all children
  if FHandle <> NullHWnd then
  begin
    DestroyHandle;
    CreateHandle;
  end;
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

    FContainerHandle := CreateWindowEx(WS_EX_TOOLWINDOW, WindowClass.lpszClassName, nil, WS_POPUP or WS_CAPTION or
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

{************************************************************}
procedure TALWinNativeView.SetParentWindow(const Value: HWND);
begin
  if HandleAllocated and (Value <> NullHWnd) then
    Winapi.Windows.SetParent(FHandle, Value)
  else
    Winapi.Windows.SetParent(ContainerHandle, Value);
end;

{*************************************************}
function TALWinNativeView.HandleAllocated: Boolean;
begin
  Result := FHandle <> NullHWnd;
end;

{*******************************************************}
class function TALWinNativeView.GetContainerHandle: HWnd;
begin
  if FContainerHandle = NullHWnd then
    CreateContainerHandle;
  Result := FContainerHandle;
end;

{****************************************}
function TALWinNativeView.GetHandle: HWND;
begin
  if (FHandle = NullHWnd) {and not (csDestroying in ComponentState)} then
    CreateHandle;
  Result := FHandle;
end;

{**********************************************}
function TALWinNativeView.GetParentWindow: HWND;
begin
  if HandleAllocated then
    Result := Winapi.Windows.GetParent(FHandle)
  else
    Exit(NullHWnd);

  if Result = ContainerHandle then
    Result := NullHWnd;
end;

{**********************************************************************************************************************}
procedure TALWinNativeView.AfterCreateMessageListener(const Sender: TObject; const AMessage: System.Messaging.TMessage);
begin
  if (AMessage is TAfterCreateFormHandle) and (TAfterCreateFormHandle(AMessage).Value = Form) then
    ZOrderManager.AddOrSetLink(Control, Handle, NullHWnd);
end;

{************************************************************************************************************************}
procedure TALWinNativeView.BeforeDestroyMessageListener(const Sender: TObject; const AMessage: System.Messaging.TMessage);
begin
  if (AMessage is TBeforeDestroyFormHandle) and (TBeforeDestroyFormHandle(AMessage).Value = Form) then
    ZOrderManager.RemoveLink(Control);
end;

{************************************************************}
constructor TALWinNativeView.Create(const AControl: TControl);
begin
  FControl := AControl;
  Create;
end;

{************************************************************}
function TALWinNativeView.GetZOrderManager: TWinZOrderManager;
begin
  if HasZOrderManager then
    Result := WindowHandleToPlatform(Form.Handle).ZOrderManager
  else
    Result := nil;
end;

{**************************************************}
function TALWinNativeView.HasZOrderManager: Boolean;
begin
  Result := (Form <> nil) and (Form.Handle <> nil);
end;

{*************************************}
procedure TALWinNativeView.UpdateFrame;
begin
  if HasZOrderManager then
    // Using UpdateBounds instead of UpdateOrderAndBounds to avoid losing focus
    // every time the edit control is moved.
    ZOrderManager.UpdateBounds(Control);
end;

{*********************************************}
procedure TALWinNativeView.RefreshNativeParent;
begin
  if HasZOrderManager then
    ZOrderManager.UpdateOrderAndBounds(Control);
end;

{************************************************}
procedure TALWinNativeView.AncestorVisibleChanged;
begin
  var LVisible := Control.Visible and Control.ParentedVisible;
  SetVisible(LVisible);
  if LVisible and HasZOrderManager then
    ZOrderManager.UpdateOrderAndBounds(Control);
end;

{*************************************}
procedure TALWinNativeView.ChangeOrder;
begin
  if HasZOrderManager then
    ZOrderManager.UpdateOrder(Control);
end;

{**************************************************************************}
function TALWinNativeView.PointInObjectLocal(X: Single; Y: Single): Boolean;
var HitTestPoint: TPointF;
begin
  HitTestPoint := TPointF.Create(x,y);
  Result := Control.LocalRect.Contains(HitTestPoint);
end;

{*********************************************************}
procedure TALWinNativeView.RootChanged(const aRoot: IRoot);
begin
  // Changing root for native control means changing ZOrderManager, because one form owns ZOrderManager.
  // So we need to remove itself from old one and add to new one.
  if HasZOrderManager then
    ZOrderManager.RemoveLink(Control);

  if aRoot is TCommonCustomForm then FForm := TCommonCustomForm(aRoot)
  else FForm := nil;

  if HasZOrderManager then
  begin
    ZOrderManager.AddOrSetLink(Control, Handle, NullHWnd);
    ZOrderManager.UpdateOrderAndBounds(Control);
  end;
end;

{******************************************************************}
procedure TALWinNativeView.SetAbsoluteEnabled(const value: Boolean);
begin
  EnableWindow(Handle, Value);
end;

{******************************************************}
procedure TALWinNativeView.SetSize(const ASize: TSizeF);
begin
  FSize := ASize;
  UpdateFrame;
end;

{**********************************************************}
procedure TALWinNativeView.SetVisible(const Value: Boolean);
begin
  if Value then
    ShowWindow(Handle, SW_SHOW)
  else
    ShowWindow(Handle, SW_HIDE);
end;

{************************************************************}
procedure TALWinNativeView.WMKeyDown(var Message: TWMKeyDown);
var
  KeyCode: Word;
  KeyChar: Char;
begin
  if Form <> nil then
  begin
    KeyCode := Message.CharCode;
    KeyChar := Char(Message.CharCode);
    Form.KeyDown(KeyCode, KeyChar, KeyDataToShiftState(Message.KeyData));
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
  if Form <> nil then
  begin
    KeyCode := Message.CharCode;
    KeyChar := Char(Message.CharCode);
    Form.KeyUp(KeyCode, KeyChar, KeyDataToShiftState(Message.KeyData));
    if (KeyCode <> Message.CharCode) or (KeyChar <> Char(Message.CharCode)) then
      Message.Result := 0
    else
      inherited;
  end
  else
    inherited;
end;

{****************************************************************}
procedure TALWinNativeView.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Control.ResetFocus;
end;

{*********************************************************************}
function TALWinNativeView.ExtractPoint(var Message: TWMMouse): TPointF;
var
  Point: TPoint;
begin
  Point := TPoint.Create(Message.XPos, Message.YPos); // px
  ClientToScreen(Handle, Point); // px
  if Form = nil then
    Result := Point
  else
  begin
    ScreenToClient(WindowHandleToPlatform(Form.Handle).Wnd, Point); // px
    Result := TPointF.Create(Point.X / Form.Handle.Scale, Point.Y / Form.Handle.Scale); // dp
  end;
end;

{********************************************************************}
procedure TALWinNativeView.WMLButtonDown(var Message: TWMLButtonDown);
begin
  try
    if Form <> nil then
    begin
      var FormPoint := ExtractPoint(Message); // dp
      Form.MouseMove([], FormPoint.X, FormPoint.Y); // Require for correct IsMouseOver handle
      Form.MouseDown(TMouseButton.mbLeft, KeysToShiftState(Message.Keys), FormPoint.X, FormPoint.Y);
    end;
  finally
    inherited;
  end;
end;

{****************************************************************}
procedure TALWinNativeView.WMLButtonUp(var Message: TWMLButtonUp);
begin
  try
    if Form <> nil then
    begin
      var FormPoint := ExtractPoint(Message); // dp
      Form.MouseUp(TMouseButton.mbLeft, KeysToShiftState(Message.Keys), FormPoint.X, FormPoint.Y);
    end;
    // Don't combine these if statements because in MouseUp the form can unload presentation!
    if Form <> nil then
      Form.MouseLeave;
  finally
    inherited;
  end;
end;

{**************************************************************************}
procedure TALWinNativeView.WMMButtonDblClick(var Message: TWMMButtonDblClk);
begin
  try
    if Form <> nil then
    begin
      var FormPoint := ExtractPoint(Message); // dp
      Form.MouseDown(TMouseButton.mbMiddle, KeysToShiftState(Message.Keys) + [ssDouble], FormPoint.X, FormPoint.Y);
    end;
  finally
    inherited;
  end;
end;

{****************************************************************}
procedure TALWinNativeView.WMMouseMove(var Message: TWMMouseMove);
begin
  try
    if Form <> nil then
    begin
      var FormPoint := ExtractPoint(Message); // dp
      Form.MouseMove(KeysToShiftState(Message.Keys), FormPoint.X, FormPoint.Y);
    end;
  finally
    inherited;
  end;
end;

{**************************************************************************}
procedure TALWinNativeView.WMLButtonDblClick(var Message: TWMLButtonDblClk);
begin
  try
    if Form <> nil then
    begin
      var FormPoint := ExtractPoint(Message); // dp
      Form.MouseDown(TMouseButton.mbLeft, KeysToShiftState(Message.Keys) + [ssDouble], FormPoint.X, FormPoint.Y);
    end;
  finally
    inherited;
  end;
end;

{**************************************************************************}
procedure TALWinNativeView.WMRButtonDblClick(var Message: TWMRButtonDblClk);
begin
  try
    if Form <> nil then
    begin
      var FormPoint := ExtractPoint(Message); // dp
      Form.MouseDown(TMouseButton.mbRight, KeysToShiftState(Message.Keys) + [ssDouble], FormPoint.X, FormPoint.Y);
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
    if Form <> nil then
    begin
      var FormPoint := ExtractPoint(Message);
      if (Control <> nil) and (Control.PopupMenu <> nil) then
      begin
        ScreenPoint := Form.ClientToScreen(FormPoint);
        IControl(Control).ShowContextMenu(ScreenPoint);
      end;
      Form.MouseMove([], FormPoint.X, FormPoint.Y); // Require for correct IsMouseOver handle
      Form.MouseDown(TMouseButton.mbRight, KeysToShiftState(Message.Keys), FormPoint.X, FormPoint.Y);
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
    if Form <> nil then
    begin
      var FormPoint := ExtractPoint(Message); // dp
      Form.MouseUp(TMouseButton.mbRight, KeysToShiftState(Message.Keys), FormPoint.X, FormPoint.Y);
    end;
    // Don't combine these if statements because in MouseUp the form can unload presentation!
    if Form <> nil then
      Form.MouseLeave;
  finally
    if (Control = nil) or (Control.PopupMenu = nil) then
      inherited;
  end;
end;

{**************************************************************}
procedure TALWinNativeView.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Control.SetFocus;
end;

{************************************}
procedure TALWinNativeView.ResetFocus;
begin
  if (Form <> nil) and Form.IsHandleAllocated then
    WinApi.Windows.SetFocus(FormToHWND(Form))
  else
    SendMessage(Handle, WM_KILLFOCUS, 0, 0);
end;

{**********************************}
procedure TALWinNativeView.SetFocus;
begin
  Winapi.Windows.SetFocus(Handle);
end;

{************************************}
procedure TALWinNativeView.Invalidate;
begin
  Winapi.Windows.InvalidateRect(Handle, NiL, TRUE); // Invalidate the entire client area
  UpdateWindow(Handle); // Force an immediate repaint
end;

end.
