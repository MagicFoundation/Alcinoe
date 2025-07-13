unit Alcinoe.FMX.NativeControl;

interface

{$I Alcinoe.inc}

uses
  System.Classes,
  FMX.Controls,
  {$IF defined(android)}
  Alcinoe.FMX.NativeView.Android,
  {$ELSEIF defined(IOS)}
  Alcinoe.FMX.NativeView.iOS,
  {$ELSEIF defined(ALMacOS)}
  Alcinoe.FMX.NativeView.Mac,
  {$ELSEIF defined(MSWINDOWS)}
  Alcinoe.FMX.NativeView.Win,
  {$ENDIF}
  Alcinoe.FMX.Common,
  Alcinoe.fmx.Controls;

type

  {*******************************************}
  TALNativeControl = class(TALControl(*, IControlTypeSupportable, IALNativeControl*))
  protected
    {$IF defined(android)}
    FNativeView: TALAndroidNativeView;
    Function CreateNativeView: TALAndroidNativeView; virtual; abstract;
    function GetNativeView: TALAndroidNativeView; virtual;
    {$ELSEIF defined(IOS)}
    FNativeView: TALIosNativeView;
    Function CreateNativeView: TALIosNativeView; virtual; abstract;
    function GetNativeView: TALIosNativeView; virtual;
    {$ELSEIF defined(ALMacOS)}
    FNativeView: TALMacNativeView;
    Function CreateNativeView: TALMacNativeView; virtual; abstract;
    function GetNativeView: TALMacNativeView; virtual;
    {$ELSEIF defined(MSWindows)}
    FNativeView: TALWinNativeView;
    Function CreateNativeView: TALWinNativeView; virtual; abstract;
    function GetNativeView: TALWinNativeView; virtual;
    {$ENDIF}
    procedure AncestorVisibleChanged(const Visible: Boolean); override;
    procedure AncestorParentChanged; override;
    procedure ParentChanged; override;
    procedure DoAbsoluteChanged; override;
    procedure DoRootChanged; override;
    procedure Resize; override;
    procedure VisibleChanged; override;
    procedure ChangeOrder; override;
    procedure DoEndUpdate; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RecalcOpacity; override;
    procedure RecalcEnabled; override;
    function HasNativeView: boolean; virtual;
    Procedure AddNativeView; virtual;
    Procedure RemoveNativeView; virtual;
    {$IF defined(android)}
    property NativeView: TALAndroidNativeView read GetNativeView;
    {$ELSEIF defined(IOS)}
    property NativeView: TALIosNativeView read GetNativeView;
    {$ELSEIF defined(ALMacOS)}
    property NativeView: TALMacNativeView read GetNativeView;
    {$ELSEIF defined(MSWindows)}
    property NativeView: TALWinNativeView read GetNativeView;
    {$ENDIF}
  end;

implementation

uses
  Alcinoe.Common;

{********************************************************}
constructor TALNativeControl.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  CanFocus := True;
  FNativeView := CreateNativeView;
end;

{************************************}
destructor TALNativeControl.Destroy;
begin
  ALFreeAndNil(FNativeView);
  inherited Destroy;
end;

{********************}
{$IF defined(android)}
function TALNativeControl.GetNativeView: TALAndroidNativeView;
begin
  Result := FNativeView;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function TALNativeControl.GetNativeView: TALIosNativeView;
begin
  Result := FNativeView;
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
function TALNativeControl.GetNativeView: TALMacNativeView;
begin
  Result := FNativeView;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
function TALNativeControl.GetNativeView: TALWinNativeView;
begin
  Result := FNativeView;
end;
{$ENDIF}

{*****************************************}
procedure TALNativeControl.DoRootChanged;
begin
  inherited;
  if csDestroying in ComponentState then exit;
  {$IF not defined(ALDPK)}
  if NativeView <> nil then
    NativeView.RootChanged(Root);
  {$ENDIF}
end;

{**********************************}
procedure TALNativeControl.Resize;
begin
  inherited;
  {$IF not defined(ALDPK)}
  if NativeView <> nil then
    NativeView.UpdateFrame;
  {$ENDIF}
end;

{*********************************************}
procedure TALNativeControl.DoAbsoluteChanged;
begin
  inherited;
  {$IF not defined(ALDPK)}
  if (not (csLoading in ComponentState)) and
     (NativeView <> nil) then
    NativeView.UpdateFrame;
  {$ENDIF}
end;

{******************************************}
procedure TALNativeControl.VisibleChanged;
begin
  inherited;
  {$IF not defined(ALDPK)}
  if NativeView <> nil then
    NativeView.SetVisible(Visible);
  {$ENDIF}
end;

{***************************************}
procedure TALNativeControl.ChangeOrder;
begin
  inherited;
  {$IF not defined(ALDPK)}
  if NativeView <> nil then
    NativeView.ChangeOrder;
  {$ENDIF}
end;

{*****************************************}
procedure TALNativeControl.RecalcOpacity;
begin
  inherited;
  {$IF not defined(ALDPK)}
  if NativeView <> nil then
    NativeView.setAlpha(AbsoluteOpacity);
  {$ENDIF}
end;

{*****************************************}
procedure TALNativeControl.RecalcEnabled;
begin
  inherited;
  {$IF not defined(ALDPK)}
  if NativeView <> nil then
    NativeView.SetEnabled(AbsoluteEnabled);
  {$ENDIF}
end;

{*************************************************}
function TALNativeControl.HasNativeView: boolean;
begin
  {$IF not defined(ALDPK)}
  Result := (NativeView <> nil) and (NativeView.Visible);
  {$ELSE}
  Result := false;
  {$ENDIF}
end;

{*****************************************}
Procedure TALNativeControl.AddNativeView;
begin
  {$IF not defined(ALDPK)}
  if NativeView = nil then exit;
  if NativeView.visible then exit;
  NativeView.SetVisible(true);
  if Parentcontrol.IsFocused then
    NativeView.SetFocus;
  {$ENDIF}
end;

{********************************************}
Procedure TALNativeControl.RemoveNativeView;
begin
  {$IF not defined(ALDPK)}
  if NativeView = nil then exit;
  if not NativeView.visible then exit;
  NativeView.ResetFocus;
  NativeView.SetVisible(False);
  {$ENDIF}
end;

{**************************************************************************}
procedure TALNativeControl.AncestorVisibleChanged(const Visible: Boolean);
begin
  inherited;
  {$IF not defined(ALDPK)}
  if NativeView <> nil then
    NativeView.AncestorVisibleChanged;
  {$ENDIF}
end;

{*************************************************}
procedure TALNativeControl.AncestorParentChanged;
begin
  inherited;
  if csDestroying in ComponentState then exit;
  {$IF not defined(ALDPK)}
  if NativeView <> nil then
    NativeView.UpdateFrame;
  {$ENDIF}
end;

{*****************************************}
procedure TALNativeControl.ParentChanged;
begin
  inherited;
  if csDestroying in ComponentState then exit;
  {$IF not defined(ALDPK)}
  if NativeView <> nil then
    NativeView.UpdateFrame;
  {$ENDIF}
end;

{***************************************}
procedure TALNativeControl.DoEndUpdate;
begin
  inherited;
  if csDestroying in ComponentState then exit;
  // Without this, in some case when we are doing beginupdate to the TEdit
  // (because in android for exemple we would like to not refresh the position of the control during calculation)
  // then when we do endupdate the control is not paint or lost somewhere
  {$IF not defined(ALDPK)}
  if NativeView <> nil then
    NativeView.UpdateFrame;
  {$ENDIF}
end;

end.
