unit Alcinoe.FMX.NativeControl;

interface

{$I Alcinoe.inc}

uses
  System.Classes,
  System.Types,
  system.UITypes,
  FMX.Controls,
  {$IF defined(android)}
  System.Messaging,
  Alcinoe.FMX.NativeView.Android,
  {$ELSEIF defined(IOS)}
  Alcinoe.FMX.NativeView.iOS,
  {$ELSEIF defined(ALMacOS)}
  Alcinoe.FMX.NativeView.Mac,
  {$ELSEIF defined(MSWINDOWS)}
  Alcinoe.FMX.NativeView.Win,
  {$ENDIF}
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.Common;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  IALNativeControl = interface
    ['{EB2063C4-CA1F-4415-97C3-4C161907F244}']
    {$IF defined(android)}
    function GetNativeView: TALAndroidNativeView;
    {$ELSEIF defined(IOS)}
    function GetNativeView: TALIosNativeView;
    {$ELSEIF defined(ALMacOS)}
    function GetNativeView: TALMacNativeView;
    {$ELSEIF defined(MSWindows)}
    function GetNativeView: TALWinNativeView;
    {$ENDIF}
  end;

  {***********************************************************************************}
  TALNativeControl = class(TALBaseRectangle, IControlTypeSupportable, IALNativeControl)
  public
    type
      // -----
      // TFill
      TFill = class(TALBrush)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
      // -------
      // TStroke
      TStroke = class(TALStrokeBrush)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
  private
    fDefStyleAttr: String;
    fDefStyleRes: String;
    FNativeViewMargins: TALBounds;
    FNativeViewScreenshot: TALDrawable;
    FFreezeNativeViewCount: Integer;
    {$IF defined(android)}
    function GetNativeView: TALAndroidNativeView;
    procedure ApplicationEventHandler(const Sender: TObject; const M : TMessage);
    {$ELSEIF defined(IOS)}
    function GetNativeView: TALIosNativeView;
    {$ELSEIF defined(ALMacOS)}
    function GetNativeView: TALMacNativeView;
    {$ELSEIF defined(MSWindows)}
    function GetNativeView: TALWinNativeView;
    {$ENDIF}
    function GetIsNativeViewFrozen: Boolean;
    procedure SetDefStyleAttr(const Value: String);
    procedure SetDefStyleRes(const Value: String);
    procedure SetNativeViewMargins(const Value: TALBounds);
    procedure NativeViewMarginsChanged(Sender: TObject);
    { IControlTypeSupportable }
    function GetControlType: TControlType;
    procedure SetControlType(const Value: TControlType);
  protected
    {$IF defined(android)}
    FNativeView: TALAndroidNativeView;
    Function CreateNativeView: TALAndroidNativeView; virtual; abstract;
    {$ELSEIF defined(IOS)}
    FNativeView: TALIosNativeView;
    Function CreateNativeView: TALIosNativeView; virtual; abstract;
    {$ELSEIF defined(ALMacOS)}
    FNativeView: TALMacNativeView;
    Function CreateNativeView: TALMacNativeView; virtual; abstract;
    {$ELSEIF defined(MSWindows)}
    FNativeView: TALWinNativeView;
    Function CreateNativeView: TALWinNativeView; virtual; abstract;
    {$ENDIF}
    procedure InitNativeView; virtual;
    procedure RecreateNativeView; Virtual;
    Procedure ShowNativeView; virtual;
    Procedure HideNativeView; virtual;
    function CreateFill: TALBrush; override;
    function CreateStroke: TALStrokeBrush; override;
    procedure AncestorVisibleChanged(const Visible: Boolean); override;
    procedure AncestorParentChanged; override;
    procedure ParentChanged; override;
    procedure DoAbsoluteChanged; override;
    procedure DoRootChanged; override;
    procedure DoResized; override;
    procedure VisibleChanged; override;
    procedure ChangeOrder; override;
    procedure DoRealign; override;
    procedure DoEndUpdate; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Loaded; override;
    procedure DoPaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Android only - the name of an attribute in the current theme that contains a reference to
    // a style resource that supplies defaults style values
    // Exemple of use: https://stackoverflow.com/questions/5051753/how-do-i-apply-a-style-programmatically
    // NOTE: !!IMPORTANT!! This properties must be defined the very first in the published section because
    // the stream system must load it the very first
    property DefStyleAttr: String read fDefStyleAttr write SetDefStyleAttr;
    // Android only - the name of a style resource that supplies default style values
    // NOTE: !!IMPORTANT!! This properties must be defined the very first in the published section because
    // the stream system must load it the very first
    property DefStyleRes: String read fDefStyleRes write SetDefStyleRes;
    procedure RecalcOpacity; override;
    procedure RecalcEnabled; override;
    /// <summary>
    ///   This method takes a screenshot of the underlying FNativeView, hides the actual native
    ///   control, and then draws the captured bitmap in its place. Use this to simulate z-order
    ///   (for example, to display a popup dialog above the control) without reordering or
    ///   disrupting the native view itself.
    /// </summary>
    function FreezeNativeView: boolean;
    Procedure UnFreezeNativeView; virtual;
    property IsNativeViewFrozen: Boolean read GetIsNativeViewFrozen;
    function IsNativeViewVisible: boolean; virtual;
    {$IF defined(android)}
    property NativeView: TALAndroidNativeView read GetNativeView;
    {$ELSEIF defined(IOS)}
    property NativeView: TALIosNativeView read GetNativeView;
    {$ELSEIF defined(ALMacOS)}
    property NativeView: TALMacNativeView read GetNativeView;
    {$ELSEIF defined(MSWindows)}
    property NativeView: TALWinNativeView read GetNativeView;
    {$ENDIF}
    property NativeViewMargins: TALBounds read FNativeViewMargins write SetNativeViewMargins;
    function GetNativeViewAbsoluteRect: TRectF; virtual;
    function GetNativeViewBoundsRect: TRectF;
    function GetNativeViewPosition: TPointF;
    function GetNativeViewWidth: Single;
    function GetNativeViewHeight: Single;
  end;

procedure ALFreezeNativeViews(var AFrozenNativeControls: TArray<TALNativeControl>);
procedure ALUnfreezeNativeViews(var AFrozenNativeControls: TArray<TALNativeControl>);

implementation

uses
  system.SysUtils,
  {$IF defined(android)}
  FMX.Platform,
  {$ENDIF}
  {$IF defined(ALSkiaCanvas)}
  System.Skia.API,
  {$ENDIF}
  FMX.Types,
  FMX.Types3D,
  FMX.Graphics,
  FMX.Forms,
  Alcinoe.FMX.Graphics,
  Alcinoe.Common;

{*********************************************************************************}
procedure ALFreezeNativeViews(var AFrozenNativeControls: TArray<TALNativeControl>);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _FreezeNativeViews(const AControl: TControl);
  begin
    for var I := 0 to AControl.ControlsCount - 1 do begin
      if (AControl.Controls[i] is TALNativeControl) and
         (TALNativeControl(AControl.Controls[i]).FreezeNativeView) then begin
        setlength(AFrozenNativeControls, Length(AFrozenNativeControls) + 1);
        AFrozenNativeControls[High(AFrozenNativeControls)] := TALNativeControl(AControl.Controls[i]);
      end
      else
        _FreezeNativeViews(AControl.Controls[i]);
    end;
  end;

begin
  Var LForm := Screen.ActiveForm;
  if LForm = nil then LForm := Application.MainForm;
  if LForm = nil then Raise Exception.Create('Error 0B1C5551-F59D-46FA-8E9B-A10AB6A65FDE');
  For var I := 0 to LForm.ChildrenCount - 1 do
    if LForm.Children[i] is TControl then
      _FreezeNativeViews(TControl(LForm.Children[i]));
end;

{***********************************************************************************}
procedure ALUnfreezeNativeViews(var AFrozenNativeControls: TArray<TALNativeControl>);
begin
  For var I := low(AFrozenNativeControls) to high(AFrozenNativeControls) do
    if AFrozenNativeControls[I] <> nil then
      AFrozenNativeControls[I].UnFreezeNativeView;
  setlength(AFrozenNativeControls, 0);
end;

{***********************************************************}
function TALNativeControl.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := Talphacolors.null;
end;

{*************************************************************}
function TALNativeControl.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := Talphacolors.null;
end;

{******************************************************}
constructor TALNativeControl.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  fDefStyleAttr := '';
  fDefStyleRes := '';
  FNativeViewMargins := TALBounds.Create;
  FNativeViewMargins.OnChanged := NativeViewMarginsChanged;
  FNativeViewScreenshot := ALNullDrawable;
  FFreezeNativeViewCount := 0;
  {$IF defined(android)}
  // In Android we must first know the value of DefStyleAttr/DefStyleRes
  // before to create the FNativeView. I use this way to know that the compoment
  // will load it's properties from the dfm
  if not IsOwnerLoading then begin
    FNativeView := CreateNativeView;
    InitNativeView;
  end
  else FNativeView := nil;
  {$ELSE}
  FNativeView := CreateNativeView;
  InitNativeView;
  {$ENDIF}
  {$IF defined(android)}
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventHandler);
  {$ENDIF}
end;

{**********************************}
destructor TALNativeControl.Destroy;
begin
  {$IF defined(android)}
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventHandler);
  {$ENDIF}
  ALFreeAndNil(FNativeView);
  ALFreeAndNil(FNativeViewMargins);
  ALFreeAndNilDrawable(FNativeViewScreenshot);
  inherited Destroy;
end;

{********************************}
procedure TALNativeControl.Loaded;
begin
  inherited;
  if FNativeView = nil then begin
    FNativeView := CreateNativeView;
    InitNativeView;
  end;
  if IsNativeViewVisible then
    // Because AncestorParentChanged is not called during loading,
    // we must call NativeView.SetVisible(true) in Loaded
    // to hide the NativeView in case a parent control is hidden.
    FNativeView.SetVisible(true);
end;

{********************}
{$IF defined(android)}
procedure TALNativeControl.ApplicationEventHandler(const Sender: TObject; const M: TMessage);
begin
  // Problem is that as we play with view, the WillBecomeInactive - BecameActive will be call everytime we toggle the
  // view under the MainActivity.getViewStack. so we can't use these event to know that the application resume from
  // background. most easy is to close the virtual keyboard when the application entere in background (EnteredBackground
  // event is call ONLY when application entered in the background so everything is fine
  if isfocused and
     (M is TApplicationEventMessage) and
     ((M as TApplicationEventMessage).Value.Event = TApplicationEvent.EnteredBackground) then resetfocus;
end;
{$ENDIF}

{*********************************************}
function TALNativeControl.CreateFill: TALBrush;
begin
  Result := TFill.Create;
end;

{*****************************************************}
function TALNativeControl.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
end;

{*******************************************************}
function TALNativeControl.GetIsNativeViewFrozen: Boolean;
begin
  Result := FFreezeNativeViewCount > 0;
end;

{**************************************************************}
procedure TALNativeControl.SetDefStyleAttr(const Value: String);
begin
  if Value <> fDefStyleAttr then begin
    fDefStyleAttr := Value;
    {$IFDEF ANDROID}
    if (not (csLoading in componentState)) and (FNativeView <> nil) then
      RecreateNativeView;
    {$ENDIF}
  end;
end;

{*************************************************************}
procedure TALNativeControl.SetDefStyleRes(const Value: String);
begin
  if Value <> fDefStyleRes then begin
    fDefStyleRes := Value;
    {$IFDEF ANDROID}
    if (not (csLoading in componentState)) and (FNativeView <> nil) then
      RecreateNativeView;
    {$ENDIF}
  end;
end;

{**********************************************************************}
procedure TALNativeControl.SetNativeViewMargins(const Value: TALBounds);
begin
  FNativeViewMargins.Assign(Value);
end;

{*******************************************************************}
procedure TALNativeControl.NativeViewMarginsChanged(Sender: TObject);
begin
  if FNativeView <> nil then
    FNativeView.UpdateFrame;
end;

{**********************************************************}
function TALNativeControl.GetNativeViewAbsoluteRect: TRectF;
begin
  Result := Padding.PaddingRect(AbsoluteRect);
  for var I := 0 to ControlsCount - 1 do begin
    var LControl := Controls[i];
    if not LControl.Visible then continue;
    {$IFNDEF ALCompilerVersionSupported130}
      {$MESSAGE WARN 'Check if FMX.Types.TAlignLayout was not updated and adjust the IFDEF'}
    {$ENDIF}
    case Lcontrol.Align of
      TAlignLayout.None,
      TAlignLayout.Center,
      TAlignLayout.Client,
      TAlignLayout.Contents,
      TAlignLayout.VertCenter,
      TAlignLayout.HorzCenter,
      TAlignLayout.Horizontal,
      TAlignLayout.Vertical,
      TAlignLayout.Scale,
      TAlignLayout.Fit,
      TAlignLayout.FitLeft,
      TAlignLayout.FitRight:;
      //--
      TAlignLayout.Top,
      TAlignLayout.TopLeft,
      TAlignLayout.TopCenter,
      TAlignLayout.TopRight,
      TAlignLayout.MostTop: Result.Top := Result.top + LControl.Height + LControl.Margins.top + LControl.Margins.bottom;
      //--
      TAlignLayout.Left,
      TAlignLayout.LeftTop,
      TAlignLayout.LeftCenter,
      TAlignLayout.LeftBottom,
      TAlignLayout.MostLeft: Result.Left := Result.Left + LControl.width + LControl.Margins.left + LControl.Margins.right;
      //--
      TAlignLayout.Right,
      TAlignLayout.RightTop,
      TAlignLayout.RightCenter,
      TAlignLayout.RightBottom,
      TAlignLayout.MostRight: Result.Right := Result.Right - LControl.width - LControl.Margins.left - LControl.Margins.right;
      //--
      TAlignLayout.Bottom,
      TAlignLayout.BottomLeft,
      TAlignLayout.BottomCenter,
      TAlignLayout.BottomRight,
      TAlignLayout.MostBottom: Result.Bottom := Result.Bottom - LControl.Height - LControl.Margins.top - LControl.Margins.bottom;
      //--
      else
        Raise Exception.Create('Error 0B27A512-250E-4D84-9EA5-C852AE80DAA0')
    end;
  end;
  Result := NativeViewMargins.PaddingRect(Result);
end;

{********************************************************}
function TALNativeControl.GetNativeViewBoundsRect: TRectF;
begin
  Result := AbsoluteToLocal(GetNativeViewAbsoluteRect);
end;

{*******************************************************}
function TALNativeControl.GetNativeViewPosition: TPointF;
begin
  Result := GetNativeViewBoundsRect.TopLeft;
end;

{***************************************************}
function TALNativeControl.GetNativeViewWidth: Single;
begin
  Result := GetNativeViewAbsoluteRect.Width;
end;

{****************************************************}
function TALNativeControl.GetNativeViewHeight: Single;
begin
  Result := GetNativeViewAbsoluteRect.Height;
end;

{*****************************************************}
function TALNativeControl.GetControlType: TControlType;
begin
  // We need ControlType because in function TFMXViewBase.canBecomeFirstResponder: Boolean;
  // we use it in IsNativeControl to determine if it's a native control or not
  Result := TControlType.Platform;
end;

{*******************************************************************}
procedure TALNativeControl.SetControlType(const Value: TControlType);
begin
  // The ControlType cannot be changed
end;

{********************}
{$IF defined(android)}
function TALNativeControl.GetNativeView: TALAndroidNativeView;
begin
  if FNativeView = nil then begin
    FNativeView := CreateNativeView;
    InitNativeView;
  end;
  Result := FNativeView;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function TALNativeControl.GetNativeView: TALIosNativeView;
begin
  if FNativeView = nil then begin
    FNativeView := CreateNativeView;
    InitNativeView;
  end;
  Result := FNativeView;
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
function TALNativeControl.GetNativeView: TALMacNativeView;
begin
  if FNativeView = nil then begin
    FNativeView := CreateNativeView;
    InitNativeView;
  end;
  Result := FNativeView;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
function TALNativeControl.GetNativeView: TALWinNativeView;
begin
  if FNativeView = nil then begin
    FNativeView := CreateNativeView;
    InitNativeView;
  end;
  Result := FNativeView;
end;
{$ENDIF}

{***************************************}
procedure TALNativeControl.DoRootChanged;
begin
  inherited;
  if csDestroying in ComponentState then exit;
  if FNativeView <> nil then
    FNativeView.RootChanged(Root);
end;

{***********************************}
procedure TALNativeControl.DoResized;
begin
  inherited;
  if FNativeView <> nil then
    FNativeView.UpdateFrame;
end;

{*******************************************}
procedure TALNativeControl.DoAbsoluteChanged;
begin
  inherited;
  if (not (csLoading in ComponentState)) and
     (FNativeView <> nil) then
    FNativeView.UpdateFrame;
end;

{****************************************}
procedure TALNativeControl.VisibleChanged;
begin
  inherited;
  if FNativeView <> nil then
    FNativeView.SetVisible(Visible);
end;

{*************************************}
procedure TALNativeControl.ChangeOrder;
begin
  inherited;
  if FNativeView <> nil then
    FNativeView.ChangeOrder;
end;

{***************************************}
procedure TALNativeControl.RecalcOpacity;
begin
  inherited;
  if FNativeView <> nil then
    FNativeView.setAlpha(AbsoluteOpacity);
end;

{***************************************}
procedure TALNativeControl.RecalcEnabled;
begin
  inherited;
  if FNativeView <> nil then
    FNativeView.SetEnabled(AbsoluteEnabled);
end;

{****************************************}
procedure TALNativeControl.InitNativeView;
begin
  // Virtual
end;

{********************************************}
procedure TALNativeControl.RecreateNativeView;
begin
  if fNativeView = nil then exit;
  ALFreeAndNil(fNativeView);
  FNativeView := CreateNativeView;
  InitNativeView;
end;

{*****************************************************}
function TALNativeControl.IsNativeViewVisible: boolean;
begin
  Result := (FNativeView <> nil) and (FNativeView.Visible);
end;

{****************************************}
Procedure TALNativeControl.ShowNativeView;
begin
  if FNativeView = nil then exit;
  if FNativeView.visible then exit;
  if IsNativeViewFrozen then exit;
  FNativeView.SetVisible(true);
  if IsFocused then
    FNativeView.SetFocus;
end;

{****************************************}
Procedure TALNativeControl.HideNativeView;
begin
  if FNativeView = nil then exit;
  if not FNativeView.visible then exit;
  ResetFocus;
  FNativeView.ResetFocus;
  FNativeView.SetVisible(False);
end;

{**************************************************}
function TALNativeControl.FreezeNativeView: boolean;
begin
  if IsNativeViewFrozen then begin
    inc(FFreezeNativeViewCount);
    Exit(true);
  end;
  if FNativeView = nil then exit(false);
  if not FNativeView.visible then exit(false);
  inc(FFreezeNativeViewCount);
  ALFreeAndNilDrawable(FNativeViewScreenshot);
  FNativeViewScreenshot := FNativeView.CaptureScreenshot;
  HideNativeView;
  Result := True;
end;

{********************************************}
Procedure TALNativeControl.UnFreezeNativeView;
begin
  if not IsNativeViewFrozen then exit;
  dec(FFreezeNativeViewCount);
  if FFreezeNativeViewCount = 0 then begin
    ALFreeAndNilDrawable(FNativeViewScreenshot);
    ShowNativeView;
  end;
end;

{************************************************************************}
procedure TALNativeControl.AncestorVisibleChanged(const Visible: Boolean);
begin
  inherited;
  if FNativeView <> nil then
    FNativeView.AncestorVisibleChanged;
end;

{***********************************************}
procedure TALNativeControl.AncestorParentChanged;
begin
  inherited;
  if csDestroying in ComponentState then exit;
  if FNativeView <> nil then
    FNativeView.UpdateFrame;
end;

{***************************************}
procedure TALNativeControl.ParentChanged;
begin
  inherited;
  if csDestroying in ComponentState then exit;
  if FNativeView <> nil then
    FNativeView.UpdateFrame;
end;

{***********************************}
procedure TALNativeControl.DoRealign;
begin
  inherited;
  if not FNeedAlign then
    Exit;
  if FNativeView <> nil then
    FNativeView.UpdateFrame;
end;

{*************************************}
procedure TALNativeControl.DoEndUpdate;
begin
  inherited;
  if csDestroying in ComponentState then exit;
  // Without this, in some case when we are doing beginupdate to the TEdit
  // (because in android for exemple we would like to not refresh the position of the control during calculation)
  // then when we do endupdate the control is not paint or lost somewhere
  if FNativeView <> nil then
    FNativeView.UpdateFrame;
end;

{*********************************}
procedure TALNativeControl.DoEnter;
begin
  {$IF defined(DEBUG)}
  //ALLog(classname+'.DoEnter', 'control.name: ' + Name);
  {$ENDIF}
  inherited DoEnter;
  if IsNativeViewVisible then
    FNativeView.SetFocus;
end;

{********************************}
procedure TALNativeControl.DoExit;
begin
  {$IF defined(DEBUG)}
  //ALLog(classname+'.DoExit', 'control.name: ' + Name);
  {$ENDIF}
  inherited DoExit;
  if IsNativeViewVisible then
    FNativeView.ResetFocus;
end;

{*********************************}
procedure TALNativeControl.DoPaint;
begin
  // DoPaint instead of paint to paint the FNativeViewScreenshot
  // After everything have been already painted (paint method already
  // called and PaintChildren too
  if IsNativeViewFrozen then begin
    var LRect := GetNativeViewBoundsRect;
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      FNativeViewScreenshot, // const ADrawable: TALDrawable;
      LRect.TopLeft, // const ATopLeft: TpointF;
      AbsoluteOpacity); // const AOpacity: Single);
  end;
end;

end.