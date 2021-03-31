{ *****************************************************************************}
{ 功能：FMX平台win托盘图标                                                      }
{ 名称：FMX.ZYJ.TrayIcon.pas                                                    }
{ 版本：1.1                                                                     }
{ 环境：Win8.1                                                                  }
{ 工具：Delphi XE3 AppMethod DelphiXE6                                          }
{ 日期：2014/3/12 20:27:56                                                      }
{ 作者：ying32                                                                  }
{ QQ  ：1444386932                                                              }
{ E-mail：1444386932@qq.com                                                     }
{ *****************************************************************************}

unit ALFmxTrayicon;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellApi,
  System.SysUtils,
  System.Classes,
  FMX.Forms,
  FMX.Types,
  FMX.Platform.Win;

type

  {**********************************}
  TBalloonFlags = (bfNone = NIIF_NONE,
                   bfInfo = NIIF_INFO,
                   bfWarning = NIIF_WARNING,
                   bfError = NIIF_ERROR);

  {*****************************}
  TALWinTrayIcon = class(TObject)
  public const
    WM_TRAYICON_MESSAGE = WM_USER + $128;
  private
    FIcon: string;
    FHandle: HWND;
    FData: TNotifyIconData;
    FHint: string;
    FVisible: Boolean;
    FOnLClick: TNotifyEvent;
    FOnRClick: TNotifyEvent;
    FOnDbLClick: TNotifyEvent;
    FOldWndProc : Pointer;
    FBalloonHint: string;
    FBalloonTitle: string;
    FBalloonFlags: TBalloonFlags;
    FBalloonTimeout: Integer;
    procedure SetTray(AId: Cardinal);
    procedure SetIcon(Value: string);
    procedure SetHint(Value: string);
    procedure SetVisible(Value: Boolean);
    procedure UpdateShow;
    procedure SetBalloonHint(const Value: string);
    procedure SetBalloonTitle(const Value: string);
    procedure SetBalloonTimeout(const Value: Integer);
    function GetBalloonTimeout: Integer;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    procedure ShowBalloonHint;
    procedure HideAppOnTaskbar;
    procedure ShowAppOnTaskbar;
    property Handle: HWND read FHandle;
    property Hint: string read FHint write SetHint;
    property Visible: Boolean read FVisible write SetVisible;
    property Icon: string read FIcon write SetIcon;
    property BalloonHint: string read FBalloonHint write SetBalloonHint;
    property BalloonTitle: string  read FBalloonTitle write SetBalloonTitle;
    property BalloonFlags: TBalloonFlags  read FBalloonFlags write FBalloonFlags;
    property BalloonTimeout: Integer  read GetBalloonTimeout write SetBalloonTimeout;
    property OnLClick: TNotifyEvent read FOnLClick write FOnLClick;
    property OnRClick: TNotifyEvent read FOnRClick write FOnRClick;
    property OnDbLClick: TNotifyEvent read FOnDbLClick write FOnDbLClick;
  end;

implementation

var
  uTrayIconClass : TALWinTrayIcon = nil;

{**********************************************************************************}
function NewWinProc(hWd: HWND; uMsg: UINT; wParam: wParam; lParam: lParam): LRESULT; stdcall;
begin
  case uMsg of
    TALWinTrayIcon.WM_TRAYICON_MESSAGE:
      begin
        case lParam of
          WM_LBUTTONDOWN:
            if Assigned(uTrayIconClass.FOnLClick) then uTrayIconClass.FOnLClick(uTrayIconClass);
          WM_RBUTTONDOWN:
            if Assigned(uTrayIconClass.FOnRClick) then uTrayIconClass.FOnRClick(uTrayIconClass);
          WM_LBUTTONDBLCLK:
            if Assigned(uTrayIconClass.FOnDbLClick) then uTrayIconClass.FOnDbLClick(uTrayIconClass);
        end;
        Exit(1);
      end;
  end;
  Result := CallWindowProc(uTrayIconClass.FOldWndProc, hWd, uMsg, wParam, lParam);
end;

{****************************************************}
constructor TALWinTrayIcon.Create(AOwner: TComponent);
begin
  inherited Create;
  FHandle := 0;
  if AOwner is TCustomForm then
  FHandle := FormToHWND(TCustomForm(AOwner));
  FBalloonHint := '';
  FBalloonTitle := '';
  FBalloonFlags := bfNone;
  FBalloonTimeout := 5000;

  FHint := '';
  FVisible := False;
  FIcon := 'MAINICON';
  FData.cbSize := Sizeof(FData);
  FData.Wnd := FHandle;
  FData.hIcon := LoadIcon(HInstance, PChar(FIcon));
  FData.uid := FData.Wnd;
  FData.uTimeout := FBalloonTimeout;
  FData.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
  FData.ucallbackmessage := WM_TRAYICON_MESSAGE;

  if uTrayIconClass = nil then begin
    uTrayIconClass := Self;
    if IsWindow(FHandle) then FOldWndProc := Pointer(SetWindowLong(FHandle, GWL_WNDPROC, Integer(@NewWinProc)));
  end;
end;

{********************************}
destructor TALWinTrayIcon.Destroy;
begin
  SetTray(NIM_DELETE);
  if (uTrayIconClass <> nil) then
  begin
    if IsWindow(FHandle) and (FOldWndProc <> nil) then
      SetWindowLong(FHandle, GWL_WNDPROC, Integer(FOldWndProc));
  end;
  inherited;
end;

{*************************************************}
function TALWinTrayIcon.GetBalloonTimeout: Integer;
begin
   Result := FData.uTimeout;
end;

{**************************************************}
procedure TALWinTrayIcon.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if FVisible then
      SetTray(NIM_ADD)
    else
      SetTray(NIM_DELETE);
  end;
end;

{***************************************}
procedure TALWinTrayIcon.ShowBalloonHint;
begin
  FData.uFlags := FData.uFlags or NIF_INFO;
  FData.dwInfoFlags := Cardinal(FBalloonFlags);
  UpdateShow;
end;

{****************************************}
procedure TALWinTrayIcon.HideAppOnTaskbar;
begin
  ShowWindow(ApplicationHWND, SW_HIDE);
end;

{****************************************}
procedure TALWinTrayIcon.ShowAppOnTaskbar;
begin
  ShowWindow(ApplicationHWND, SW_Show);
end;

{***********************************************************}
procedure TALWinTrayIcon.SetBalloonHint(const Value: string);
begin
  if CompareStr(FBalloonHint, Value) <> 0 then
  begin
    FBalloonHint := Value;
    StrPLCopy(FData.szInfo, FBalloonHint, Length(FData.szInfo) - 1);
    UpdateShow;
  end;
end;

{************************************************************}
procedure TALWinTrayIcon.SetBalloonTitle(const Value: string);
begin
  if CompareStr(FBalloonTitle, Value) <> 0 then
  begin
    FBalloonTitle := Value;
    StrPLCopy(FData.szInfoTitle, FBalloonTitle, Length(FData.szInfoTitle) - 1);
    UpdateShow;
  end;
end;

{***************************************************************}
procedure TALWinTrayIcon.SetBalloonTimeout(const Value: Integer);
begin
  FData.uTimeout := Value;
end;

{**********************************************}
procedure TALWinTrayIcon.SetHint(Value: string);
begin
  if CompareStr(FHint, Value) <> 0  then
  begin
    FHint := Value;
    StrPLCopy(FData.szTip, FHint, Length(FData.szTip) - 1);
    UpdateShow;
  end;
end;

{**********************************************}
procedure TALWinTrayIcon.SetIcon(Value: string);
begin
  if CompareStr(FIcon, Value) <> 0 then
  begin
    if CompareStr(FIcon, '') = 0 then
      FIcon := 'MAINICON';
    FIcon := Value;
    UpdateShow;
  end;
end;

{**********************************************}
procedure TALWinTrayIcon.SetTray(AId: Cardinal);
begin
    Shell_NotifyIcon(AId, @FData);
end;

{**********************************}
procedure TALWinTrayIcon.UpdateShow;
begin
  FData.HICON := LoadIcon(HInstance, PChar(FIcon));
  if FVisible then
    SetTray(NIM_MODIFY);
end;

end.
