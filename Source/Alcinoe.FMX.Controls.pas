unit Alcinoe.FMX.Controls;

interface

{$I Alcinoe.inc}

uses
  System.UITypes,
  system.Types,
  System.Classes,
  FMX.Forms,
  FMX.Controls,
  FMX.Types;

type

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if FMX.Types.TAlignLayout was not updated and adjust the IFDEF'}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2342 was implemented and adjust the IFDEF'}
  {$ENDIF}
  TALAlignLayout = (
    None,
    Top,
    Left,
    Right,
    Bottom,
    MostTop,
    MostBottom,
    MostLeft,
    MostRight,
    Client,
    //Contents,       // Removed from TAlignLayout
    Center,
    VertCenter,
    HorzCenter,
    Horizontal,
    Vertical,
    //Scale,          // Removed from TAlignLayout
    //Fit,            // Removed from TAlignLayout
    //FitLeft,        // Removed from TAlignLayout
    //FitRight,       // Removed from TAlignLayout
    TopCenter,        // Added from TAlignLayout - Works like TAlignLayout.Top, then centers the control horizontally
    TopLeft,          // Added from TAlignLayout - Works like TAlignLayout.Top, then aligns the control to the left.
    TopRight,         // Added from TAlignLayout - Works like TAlignLayout.Top, then aligns the control to the right.
    LeftCenter,       // Added from TAlignLayout - Works like TAlignLayout.Left, then centers the control vertically.
    LeftTop,          // Added from TAlignLayout - Works like TAlignLayout.Left, then aligns the control to the top.
    LeftBottom,       // Added from TAlignLayout - Works like TAlignLayout.Left, then aligns the control to the bottom.
    RightCenter,      // Added from TAlignLayout - Works like TAlignLayout.Right, then centers the control vertically.
    RightTop,         // Added from TAlignLayout - Works like TAlignLayout.Right, then aligns the control to the top.
    RightBottom,      // Added from TAlignLayout - Works like TAlignLayout.Right, then aligns the control to the bottom.
    BottomCenter,     // Added from TAlignLayout - Works like TAlignLayout.Bottom, then centers the control horizontally
    BottomLeft,       // Added from TAlignLayout - Works like TAlignLayout.Bottom, then aligns the control to the left.
    BottomRight,      // Added from TAlignLayout - Works like TAlignLayout.Bottom, then aligns the control to the right.
    MostTopCenter,    // Added from TAlignLayout - Works like TAlignLayout.MostTop, then centers the control horizontally
    MostTopLeft,      // Added from TAlignLayout - Works like TAlignLayout.MostTop, then aligns the control to the left.
    MostTopRight,     // Added from TAlignLayout - Works like TAlignLayout.MostTop, then aligns the control to the right.
    MostLeftCenter,   // Added from TAlignLayout - Works like TAlignLayout.MostLeft, then centers the control vertically.
    MostLeftTop,      // Added from TAlignLayout - Works like TAlignLayout.MostLeft, then aligns the control to the top.
    MostLeftBottom,   // Added from TAlignLayout - Works like TAlignLayout.MostLeft, then aligns the control to the bottom.
    MostRightCenter,  // Added from TAlignLayout - Works like TAlignLayout.MostRight, then centers the control vertically.
    MostRightTop,     // Added from TAlignLayout - Works like TAlignLayout.MostRight, then aligns the control to the top.
    MostRightBottom,  // Added from TAlignLayout - Works like TAlignLayout.MostRight, then aligns the control to the bottom.
    MostBottomCenter, // Added from TAlignLayout - Works like TAlignLayout.MostBottom, then centers the control horizontally
    MostBottomLeft,   // Added from TAlignLayout - Works like TAlignLayout.MostBottom, then aligns the control to the left.
    MostBottomRight); // Added from TAlignLayout - Works like TAlignLayout.MostBottom, then aligns the control to the right.

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl was not updated and adjust the IFDEF'}
  {$ENDIF}
  TALControl = class(TControl)
  private
    FForm: TCommonCustomForm; // 8 bytes
    FParentALControl: TALControl; // 8 bytes
    FFormerMarginsChangedHandler: TNotifyEvent; // 16 bytes
    FControlAbsolutePosAtMouseDown: TpointF; // 8 bytes
    FScale: Single; // 4 bytes
    FFocusOnMouseDown: Boolean; // 1 byte
    FFocusOnMouseUp: Boolean; // 1 byte
    FMouseDownAtLowVelocity: Boolean; // 1 byte
    FDisableDoubleClickHandling: Boolean; // 1 byte
    FIsPixelAlignmentEnabled: Boolean; // 1 byte
    FAlign: TALAlignLayout; // 1 byte
    FIsSetBoundsLocked: Boolean; // 1 byte
    function GetPivot: TPosition;
    procedure SetPivot(const Value: TPosition);
    function GetPressed: Boolean;
    procedure SetPressed(const AValue: Boolean);
    procedure DelayOnResize(Sender: TObject);
    procedure DelayOnResized(Sender: TObject);
    procedure MarginsChangedHandler(Sender: TObject);
    function IsScaledStored: Boolean;
  protected
    FTextUpdating: Boolean; // 1 byte
    FAutoSize: Boolean; // 1 byte
    FIsAdjustingSize: Boolean; // 1 byte
    FAdjustSizeOnEndUpdate: Boolean; // 1 byte
    function GetDoubleBuffered: boolean; virtual;
    procedure SetDoubleBuffered(const AValue: Boolean); virtual;
    procedure SetScale(const AValue: Single); virtual;
    property Scale: Single read FScale write SetScale stored IsScaledStored nodefault;
    property Pivot: TPosition read GetPivot write SetPivot;
    function GetAutoSize: Boolean; virtual;
    procedure SetAutoSize(const Value: Boolean); virtual;
    // Dynamically adjusts the dimensions to accommodate child controls,
    // considering their sizes, positions, margins, and alignments.
    property AutoSize: Boolean read GetAutoSize write SetAutoSize default False;
    function GetIsPixelAlignmentEnabled: Boolean; virtual;
    procedure SetIsPixelAlignmentEnabled(const AValue: Boolean); Virtual;
    property FocusOnMouseDown: Boolean read FFocusOnMouseDown write FFocusOnMouseDown;
    property FocusOnMouseUp: Boolean read FFocusOnMouseUp write FFocusOnMouseUp;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    function GetParentedVisible: Boolean; override;
    procedure DoMatrixChanged(Sender: TObject); override;
    procedure DoRootChanged; override;
    procedure IsMouseOverChanged; virtual;
    procedure IsFocusedChanged; virtual;
    procedure PressedChanged; virtual;
    Procedure MarginsChanged; Virtual;
    procedure PaddingChanged; override;
    procedure ParentChanged; override;
    procedure Loaded; override;
    function IsOwnerLoading: Boolean;
    function IsSizeStored: Boolean; override;
    function GetAlign: TALAlignLayout; Reintroduce;
    procedure SetAlign(const Value: TALAlignLayout); Reintroduce; virtual;
    procedure DoEndUpdate; override;
    procedure DoResized; override;
    procedure DoRealign; override;
    procedure AdjustSize; virtual;
    procedure BeginTextUpdate; virtual;
    procedure EndTextUpdate; virtual;
    procedure SetFixedSizeBounds(X, Y, AWidth, AHeight: Single); Virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EndUpdate; override;
    procedure SetNewScene(AScene: IScene); override;
    function IsVisibleWithinFormBounds: Boolean;
    property Form: TCommonCustomForm read FForm;
    property DisableDoubleClickHandling: Boolean read FDisableDoubleClickHandling write FDisableDoubleClickHandling;
    {$IFNDEF ALCompilerVersionSupported122}
      {$MESSAGE WARN 'Check if property FMX.Controls.TControl.Pressed still not fire a PressChanged event when it gets updated, and adjust the IFDEF'}
    {$ENDIF}
    property Pressed: Boolean read GetPressed write SetPressed;
    procedure AlignToPixel; virtual;
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
    function HasUnconstrainedAutosizeX: Boolean; virtual;
    function HasUnconstrainedAutosizeY: Boolean; virtual;
    procedure MakeBufDrawable; virtual;
    procedure ClearBufDrawable; virtual;
    property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered default False;
    property IsPixelAlignmentEnabled: Boolean read GetIsPixelAlignmentEnabled write SetIsPixelAlignmentEnabled;
    property Align: TALAlignLayout read FAlign write SetAlign default TALAlignLayout.None;
    property ParentALControl: TALControl read FParentALControl;
  end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if FMX.Controls.TContent was not updated and adjust the IFDEF'}
  {$ENDIF}
  TALContent = class(TALControl, IContent)
  private
    FParentAligning: Boolean;
  protected
    function GetTabStopController: ITabStopController; override;
    procedure DoRealign; override;
    procedure IContent.Changed = ContentChanged;
    procedure ContentChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function GetTabListClass: TTabListClass; override;
  published
    //property Action;
    property Align;
    //property Anchors;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClipChildren default False;
    //property ClipParent;
    property Cursor;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property Locked;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Size;
    //property TabOrder;
    //property TabStop;
    //property TouchTargetExpansion;
    property Visible;
    property Width;
    property OnCanFocus;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnEnter;
    property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

implementation

uses
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  Fmx.utils,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.ScrollEngine,
  Alcinoe.StringUtils,
  Alcinoe.Common;

{**}
Type
  _TControlAccessProtected = class(Tcontrol);
  _TStyledControlAccessProtected = class(TStyledControl);

{************************************************}
constructor TALControl.Create(AOwner: TComponent);
begin
  inherited;
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if MarginsChanged is not implemented in FMX.Controls.TControl and adjust the IFDEF'}
  {$ENDIF}
  FFormerMarginsChangedHandler := Margins.OnChange;
  Margins.OnChange := MarginsChangedHandler;
  Size.SetPlatformDefaultWithoutNotification(False);
  FForm := nil;
  FParentALControl := nil;
  FControlAbsolutePosAtMouseDown := TpointF.zero;
  FScale := 1;
  FFocusOnMouseDown := False;
  FFocusOnMouseUp := False;
  FMouseDownAtLowVelocity := True;
  // Double-clicks, or double-taps, are rarely used in mobile design due to
  // touch screen challenges and user experience considerations. Mobile devices
  // favor simpler, more intuitive gestures like swiping and pinching, which are
  // better suited to smaller screens and prevent confusion with similar
  // actions. Consequently, functionalities often tied to double-clicks on
  // desktops are handled by different gestures or interface elements in
  // mobile apps, leading to a more user-friendly experience.
  FDisableDoubleClickHandling := True;
  FIsPixelAlignmentEnabled := True;
  FAlign := TALAlignLayout.None;
  FIsSetBoundsLocked := False;
  FTextUpdating := False;
  FAutoSize := False;
  FIsAdjustingSize := False;
  FAdjustSizeOnEndUpdate := False;
end;

{****************************}
destructor TALControl.Destroy;
begin
  ClearBufDrawable;
  inherited;
end;

{*****************************}
procedure TALControl.EndUpdate;
begin
  if IsUpdating then
  begin
    if not FTextUpdating then begin
      BeginTextUpdate;
      Inherited;
      EndTextUpdate;
    end
    else
      Inherited;
  end;
end;

{*************************************************}
// Unfortunately, the way BeginUpdate/EndUpdate and
// Realign are implemented is not very efficient for TALText.
// When calling EndUpdate, it first propagates to the most
// deeply nested children in this hierarchy:
//   Control1
//     Control2
//       AlText1
// This means when Control1.EndUpdate is called,
// it executes in the following order:
//       AlText1.EndUpdate => AdjustSize and Realign
//     Control2.EndUpdate => Realign and potentially calls AlText1.AdjustSize again
//   Control1.EndUpdate => Realign and possibly triggers AlText1.AdjustSize once more
// This poses a problem since the BufDrawable will be
// recalculated multiple times.
// To mitigate this, we can use:
//   BeginTextUpdate;
//   EndUpdate;
//   EndTextUpdate;
procedure TALControl.BeginTextUpdate;
begin
  FTextUpdating := True;
  for var I := 0 to ControlsCount - 1 do
    if Controls[i] is TALControl then
      TALControl(Controls[i]).BeginTextUpdate;
end;

{*********************************}
procedure TALControl.EndTextUpdate;
begin
  FTextUpdating := False;
  for var I := 0 to ControlsCount - 1 do
    if Controls[i] is TALControl then
      TALControl(Controls[i]).EndTextUpdate;
end;

{**************************}
procedure TALControl.Loaded;
begin
  {$IF not DEFINED(ALDPK)}
  if IsPixelAlignmentEnabled then
    AlignToPixel;
  {$ENDIF}
  Inherited;
  AdjustSize;
end;

{******************************************}
function TALControl.IsOwnerLoading: Boolean;
begin
  result := (Owner <> nil) and
            (csloading in Owner.ComponentState);
end;

{****************************************}
function TALControl.IsSizeStored: Boolean;
begin
  var LDefaultSize := GetDefaultSize;
  result := (not SameValue(LDefaultSize.cx, Size.Size.cx, TEpsilon.Position)) or
            (not SameValue(LDefaultSize.cy, Size.Size.cy, TEpsilon.Position));
end;

{*******************************************}
function TALControl.GetAlign: TALAlignLayout;
begin
  Result := FAlign;
end;

{*********************************************************}
procedure TALControl.SetAlign(const Value: TALAlignLayout);
begin
  If FAlign <> Value then begin
    FAlign := Value;
    var LLegacyAlign: TAlignLayout;
    Case Value of
      TALAlignLayout.None: LLegacyAlign := TAlignLayout.None;
      TALAlignLayout.Top: LLegacyAlign := TAlignLayout.Top;
      TALAlignLayout.Left: LLegacyAlign := TAlignLayout.Left;
      TALAlignLayout.Right: LLegacyAlign := TAlignLayout.Right;
      TALAlignLayout.Bottom: LLegacyAlign := TAlignLayout.Bottom;
      TALAlignLayout.MostTop: LLegacyAlign := TAlignLayout.MostTop;
      TALAlignLayout.MostBottom: LLegacyAlign := TAlignLayout.MostBottom;
      TALAlignLayout.MostLeft: LLegacyAlign := TAlignLayout.MostLeft;
      TALAlignLayout.MostRight: LLegacyAlign := TAlignLayout.MostRight;
      TALAlignLayout.Client: LLegacyAlign := TAlignLayout.Client;
      //TALAlignLayout.Contents: LLegacyAlign := TAlignLayout.Contents;
      TALAlignLayout.Center: LLegacyAlign := TAlignLayout.Center;
      TALAlignLayout.VertCenter: LLegacyAlign := TAlignLayout.VertCenter;
      TALAlignLayout.HorzCenter: LLegacyAlign := TAlignLayout.HorzCenter;
      TALAlignLayout.Horizontal: LLegacyAlign := TAlignLayout.Horizontal;
      TALAlignLayout.Vertical: LLegacyAlign := TAlignLayout.Vertical;
      //TALAlignLayout.Scale: LLegacyAlign := TAlignLayout.Scale;
      //TALAlignLayout.Fit: LLegacyAlign := TAlignLayout.Fit;
      //TALAlignLayout.FitLeft: LLegacyAlign := TAlignLayout.FitLeft;
      //TALAlignLayout.FitRight: LLegacyAlign := TAlignLayout.FitRight;
      TALAlignLayout.TopCenter: LLegacyAlign := TAlignLayout.Top;
      TALAlignLayout.TopLeft: LLegacyAlign := TAlignLayout.Top;
      TALAlignLayout.TopRight: LLegacyAlign := TAlignLayout.Top;
      TALAlignLayout.LeftCenter: LLegacyAlign := TAlignLayout.Left;
      TALAlignLayout.LeftTop: LLegacyAlign := TAlignLayout.Left;
      TALAlignLayout.LeftBottom: LLegacyAlign := TAlignLayout.Left;
      TALAlignLayout.RightCenter: LLegacyAlign := TAlignLayout.Right;
      TALAlignLayout.RightTop: LLegacyAlign := TAlignLayout.Right;
      TALAlignLayout.RightBottom: LLegacyAlign := TAlignLayout.Right;
      TALAlignLayout.BottomCenter: LLegacyAlign := TAlignLayout.Bottom;
      TALAlignLayout.BottomLeft: LLegacyAlign := TAlignLayout.Bottom;
      TALAlignLayout.BottomRight: LLegacyAlign := TAlignLayout.Bottom;
      TALAlignLayout.MostTopCenter: LLegacyAlign := TAlignLayout.MostTop;
      TALAlignLayout.MostTopLeft: LLegacyAlign := TAlignLayout.MostTop;
      TALAlignLayout.MostTopRight: LLegacyAlign := TAlignLayout.MostTop;
      TALAlignLayout.MostLeftCenter: LLegacyAlign := TAlignLayout.MostLeft;
      TALAlignLayout.MostLeftTop: LLegacyAlign := TAlignLayout.MostLeft;
      TALAlignLayout.MostLeftBottom: LLegacyAlign := TAlignLayout.MostLeft;
      TALAlignLayout.MostRightCenter: LLegacyAlign := TAlignLayout.MostRight;
      TALAlignLayout.MostRightTop: LLegacyAlign := TAlignLayout.MostRight;
      TALAlignLayout.MostRightBottom: LLegacyAlign := TAlignLayout.MostRight;
      TALAlignLayout.MostBottomCenter: LLegacyAlign := TAlignLayout.MostBottom;
      TALAlignLayout.MostBottomLeft: LLegacyAlign := TAlignLayout.MostBottom;
      TALAlignLayout.MostBottomRight: LLegacyAlign := TAlignLayout.MostBottom;
      else Raise Exception.Create('Error D527A470-23AC-4E3C-BCC5-4C2DB578A691');
    end;
    Inherited SetAlign(LLegacyAlign);
  end;
end;

{*******************************}
procedure TALControl.DoEndUpdate;
begin
  inherited DoEndUpdate;
  if FAdjustSizeOnEndUpdate then
    AdjustSize;
end;

{*****************************}
procedure TALControl.DoResized;
begin
  {$IF defined(debug)}
  //ALLog(ClassName+'.DoResized', 'Name: ' + Name);
  {$ENDIF}
  inherited;
  AdjustSize;
end;

{*****************************}
procedure TALControl.DoRealign;
begin
  {$IF defined(debug)}
  //ALLog(ClassName+'.DoRealign', 'Name: ' + Name);
  {$ENDIF}

  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-15768 was implemented and adjust the IFDEF'}
  {$ENDIF}
  // I decided to remove this workaround because it doesn't
  // work well with TContent (e.g., ScrollBox).
  // The TContent size is always updated during realignment,
  // causing the process to be applied twice.
  //https://quality.embarcadero.com/browse/RSP-15768
  //var LOriginalSize: TPointF := Size.Size;
  inherited;
  //https://quality.embarcadero.com/browse/RSP-15768
  //if not LOriginalSize.EqualsTo(Size.Size) then DoRealign;

  // Unfortunately, FNeedAlign is set to true when any child control with
  // Align <> None is added. However, it does not take AutoSize into
  // consideration
  AdjustSize;
end;

{*********************************************************************}
procedure TALControl.SetFixedSizeBounds(X, Y, AWidth, AHeight: Single);
begin
  if TNonReentrantHelper.EnterSection(FIsSetBoundsLocked) then begin
    try

      {$IFNDEF ALCompilerVersionSupported122}
        {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2342 was implemented and adjust the IFDEF'}
      {$ENDIF}
      //TALAlignLayout.TopCenter
      //TALAlignLayout.TopLeft
      //TALAlignLayout.TopRight
      //TALAlignLayout.LeftCenter
      //TALAlignLayout.LeftTop
      //TALAlignLayout.LeftBottom
      //TALAlignLayout.RightCenter
      //TALAlignLayout.RightTop
      //TALAlignLayout.RightBottom
      //TALAlignLayout.BottomCenter
      //TALAlignLayout.BottomLeft
      //TALAlignLayout.BottomRight
      //TALAlignLayout.MostTopCenter
      //TALAlignLayout.MostTopLeft
      //TALAlignLayout.MostTopRight
      //TALAlignLayout.MostLeftCenter
      //TALAlignLayout.MostLeftTop
      //TALAlignLayout.MostLeftBottom
      //TALAlignLayout.MostRightCenter
      //TALAlignLayout.MostRightTop
      //TALAlignLayout.MostRightBottom
      //TALAlignLayout.MostBottomCenter
      //TALAlignLayout.MostBottomLeft
      //TALAlignLayout.MostBottomRight
      If (integer(FAlign) >= integer(TALAlignLayout.TopCenter)) and
         (integer(FAlign) <= integer(TALAlignLayout.MostBottomRight)) and
         (ParentControl <> nil) and                              // FDisableAlign = true mean that SetBounds was called by
         (_TControlAccessProtected(ParentControl).FDisableAlign) // AlignObjects procedure inside inherited DoRealign
      then begin
        case FAlign of
          TALAlignLayout.TopCenter,
          TALAlignLayout.BottomCenter,
          TALAlignLayout.MostTopCenter,
          TALAlignLayout.MostBottomCenter: begin
            X := X + ((AWidth - Width) / 2);
            AWidth := Width;
          end;
          TALAlignLayout.TopLeft,
          TALAlignLayout.BottomLeft,
          TALAlignLayout.MostTopLeft,
          TALAlignLayout.MostBottomLeft: begin
            AWidth := Width;
          end;
          TALAlignLayout.TopRight,
          TALAlignLayout.BottomRight,
          TALAlignLayout.MostTopRight,
          TALAlignLayout.MostBottomRight: begin
            X := X + (AWidth - Width);
            AWidth := Width;
          end;
          TALAlignLayout.LeftCenter,
          TALAlignLayout.RightCenter,
          TALAlignLayout.MostLeftCenter,
          TALAlignLayout.MostRightCenter: begin
            Y := Y + ((AHeight - Height) / 2);
            AHeight := Height;
          end;
          TALAlignLayout.LeftTop,
          TALAlignLayout.RightTop,
          TALAlignLayout.MostLeftTop,
          TALAlignLayout.MostRightTop: begin
            AHeight := Height;
          end;
          TALAlignLayout.LeftBottom,
          TALAlignLayout.RightBottom,
          TALAlignLayout.MostLeftBottom,
          TALAlignLayout.MostRightBottom: begin
            Y := Y + (AHeight - Height);
            AHeight := Height;
          end;
          else
            raise Exception.Create('Error 9431A388-3F2F-4F06-8296-210708F60C66');
        end;
      end;

      {$IF defined(debug)}
      //ALLog(ClassName+'.SetFixedSizeBounds', 'Name: ' + Name + ' | X : '+ALFloatToStrW(X, ALDefaultFormatSettingsW)+'('+ALFloatToStrW(Position.X, ALDefaultFormatSettingsW)+') | Y : '+ALFloatToStrW(Y, ALDefaultFormatSettingsW)+'('+ALFloatToStrW(Position.Y, ALDefaultFormatSettingsW)+') | AWidth : '+ALFloatToStrW(AWidth, ALDefaultFormatSettingsW)+'('+ALFloatToStrW(Width, ALDefaultFormatSettingsW)+') | AHeight : '+ALFloatToStrW(AHeight, ALDefaultFormatSettingsW)+'('+ALFloatToStrW(Height, ALDefaultFormatSettingsW)+')');
      {$ENDIF}

      inherited SetBounds(X, Y, AWidth, AHeight);

    finally
      TNonReentrantHelper.LeaveSection(FIsSetBoundsLocked)
    end;
  end
  else
    SetBounds(X, Y, AWidth, AHeight);
end;

{************************************************************}
procedure TALControl.SetBounds(X, Y, AWidth, AHeight: Single);
begin
  if FIsSetBoundsLocked then begin
    AWidth := Width;
    AHeight := Height;
  end;

  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2342 was implemented and adjust the IFDEF'}
  {$ENDIF}
  //TALAlignLayout.TopCenter
  //TALAlignLayout.TopLeft
  //TALAlignLayout.TopRight
  //TALAlignLayout.LeftCenter
  //TALAlignLayout.LeftTop
  //TALAlignLayout.LeftBottom
  //TALAlignLayout.RightCenter
  //TALAlignLayout.RightTop
  //TALAlignLayout.RightBottom
  //TALAlignLayout.BottomCenter
  //TALAlignLayout.BottomLeft
  //TALAlignLayout.BottomRight
  //TALAlignLayout.MostTopCenter
  //TALAlignLayout.MostTopLeft
  //TALAlignLayout.MostTopRight
  //TALAlignLayout.MostLeftCenter
  //TALAlignLayout.MostLeftTop
  //TALAlignLayout.MostLeftBottom
  //TALAlignLayout.MostRightCenter
  //TALAlignLayout.MostRightTop
  //TALAlignLayout.MostRightBottom
  //TALAlignLayout.MostBottomCenter
  //TALAlignLayout.MostBottomLeft
  //TALAlignLayout.MostBottomRight
  If (integer(FAlign) >= integer(TALAlignLayout.TopCenter)) and
     (integer(FAlign) <= integer(TALAlignLayout.MostBottomRight)) and
     (ParentControl <> nil) and                              // FDisableAlign = true mean that SetBounds was called by
     (_TControlAccessProtected(ParentControl).FDisableAlign) // AlignObjects procedure inside inherited DoRealign
  then begin
    case FAlign of
      TALAlignLayout.TopCenter,
      TALAlignLayout.BottomCenter,
      TALAlignLayout.MostTopCenter,
      TALAlignLayout.MostBottomCenter: begin
        X := X + ((AWidth - Width) / 2);
        AWidth := Width;
      end;
      TALAlignLayout.TopLeft,
      TALAlignLayout.BottomLeft,
      TALAlignLayout.MostTopLeft,
      TALAlignLayout.MostBottomLeft: begin
        AWidth := Width;
      end;
      TALAlignLayout.TopRight,
      TALAlignLayout.BottomRight,
      TALAlignLayout.MostTopRight,
      TALAlignLayout.MostBottomRight: begin
        X := X + (AWidth - Width);
        AWidth := Width;
      end;
      TALAlignLayout.LeftCenter,
      TALAlignLayout.RightCenter,
      TALAlignLayout.MostLeftCenter,
      TALAlignLayout.MostRightCenter: begin
        Y := Y + ((AHeight - Height) / 2);
        AHeight := Height;
      end;
      TALAlignLayout.LeftTop,
      TALAlignLayout.RightTop,
      TALAlignLayout.MostLeftTop,
      TALAlignLayout.MostRightTop: begin
        AHeight := Height;
      end;
      TALAlignLayout.LeftBottom,
      TALAlignLayout.RightBottom,
      TALAlignLayout.MostLeftBottom,
      TALAlignLayout.MostRightBottom: begin
        Y := Y + (AHeight - Height);
        AHeight := Height;
      end;
      else
        raise Exception.Create('Error 9431A388-3F2F-4F06-8296-210708F60C66');
    end;
  end;

  {$IF defined(debug)}
  //var LMoved := not (SameValue(X, Position.X, TEpsilon.Position) and SameValue(Y, Position.Y, TEpsilon.Position));
  //var LSizeChanged := not (SameValue(AWidth, Width, TEpsilon.Position) and SameValue(AHeight, Height, TEpsilon.Position));
  //if LMoved or LSizeChanged then
  //  ALLog(ClassName+'.SetBounds', 'Name: ' + Name + ' | X : '+ALFloatToStrW(X, ALDefaultFormatSettingsW)+'('+ALFloatToStrW(Position.X, ALDefaultFormatSettingsW)+') | Y : '+ALFloatToStrW(Y, ALDefaultFormatSettingsW)+'('+ALFloatToStrW(Position.Y, ALDefaultFormatSettingsW)+') | AWidth : '+ALFloatToStrW(AWidth, ALDefaultFormatSettingsW)+'('+ALFloatToStrW(Width, ALDefaultFormatSettingsW)+') | AHeight : '+ALFloatToStrW(AHeight, ALDefaultFormatSettingsW)+'('+ALFloatToStrW(Height, ALDefaultFormatSettingsW)+')');
  {$ENDIF}

  inherited;
end;

{******************************}
procedure TALControl.AdjustSize;
begin
  if (not (csLoading in ComponentState)) and // Loaded will call again AdjustSize
     (not (csDestroying in ComponentState)) and // If csDestroying do not do autosize
     (ControlsCount > 0) and // If there are no controls, do not perform autosizing
     (HasUnconstrainedAutosizeX or HasUnconstrainedAutosizeY) and // If AutoSize is false nothing to adjust
     (scene <> nil) and // SetNewScene will call again AdjustSize
     (TNonReentrantHelper.EnterSection(FIsAdjustingSize)) then begin // Non-reantrant
    try

      if IsUpdating then begin
        FAdjustSizeOnEndUpdate := True;
        Exit;
      end
      else
        FAdjustSizeOnEndUpdate := False;

      {$IF defined(debug)}
      //ALLog(ClassName+'.AdjustSize', 'Name: ' + Name + ' | HasUnconstrainedAutosize(X/Y) : '+ALBoolToStrW(HasUnconstrainedAutosizeX)+'/'+ALBoolToStrW(HasUnconstrainedAutosizeY));
      {$ENDIF}

      var LSize := TSizeF.Create(0,0);
      for var I := 0 to ControlsCount - 1 do begin
        var LChildControl := Controls[I];
        if (csDesigning in ComponentState) and (LChildControl.ClassName = 'TGrabHandle.TGrabHandleRectangle') then
          continue;

        var LALChildControl: TALControl;
        var LALChildControlAlign: TALAlignLayout;
        If (LChildControl is TALControl) then begin
          LALChildControl := TALControl(LChildControl);
          LALChildControlAlign := LALChildControl.Align
        end
        else begin
          LALChildControl := nil;
          LALChildControlAlign := TALAlignLayout(LChildControl.Align);
        end;

        case LALChildControlAlign of

          //--
          TALAlignLayout.None: begin
            // Adjusts AControl size to ensure it contains
            // the child control at its current position.
            LSize.Width := Max(LSize.Width, LChildControl.Position.X + LChildControl.width + LChildControl.Margins.right + padding.right);
            LSize.height := Max(LSize.height, LChildControl.Position.Y + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom);
          end;

          //--
          TALAlignLayout.Center: begin
            // Adjusts AControl size to ensure it contains the
            // child control without considering its current position.
            // !! Note: This may not work well if there is more than
            //    one child control. !!
            LSize.Width := Max(LSize.Width, LChildControl.Margins.left + padding.left + LChildControl.width + LChildControl.Margins.right + padding.right);
            LSize.height := Max(LSize.height, LChildControl.Margins.top + padding.top + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom);
          end;

          //--
          TALAlignLayout.Top,
          TALAlignLayout.MostTop,
          TALAlignLayout.Bottom,
          TALAlignLayout.MostBottom: begin
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeX then
              // If the child control has autosize enabled on the X-axis, adjusts
              // AControl width to ensure it contains the child control at its
              // current position. For example, TALText will never have
              // HasUnconstrainedAutosizeX set to true with TALAlignLayout.Top,
              // but TALLayout/TRectangle will have it set to true if their
              // autosize property is enabled.
              LSize.Width := Max(LSize.Width, LChildControl.Position.X + LChildControl.width + LChildControl.Margins.right + padding.right)
            else
              // Otherwise, do not adjust AControl width.
              LSize.Width := Max(LSize.Width, Width);
            // Adjusts AControl height to ensure it contains
            // the child control at its current position.
            LSize.height := Max(LSize.height, LChildControl.Position.Y + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom);
          end;

          //--
          TALAlignLayout.TopCenter,
          TALAlignLayout.TopLeft,
          TALAlignLayout.TopRight,
          TALAlignLayout.BottomCenter,
          TALAlignLayout.BottomLeft,
          TALAlignLayout.BottomRight,
          TALAlignLayout.MostTopCenter,
          TALAlignLayout.MostTopLeft,
          TALAlignLayout.MostTopRight,
          TALAlignLayout.MostBottomCenter,
          TALAlignLayout.MostBottomLeft,
          TALAlignLayout.MostBottomRight: begin
            // Adjusts AControl width to ensure it contains the
            // child control without considering its current position.
            // !! Note: This may not work well if there is another child control
            //    that is not aligned to the top or bottom. !!
            LSize.Width := Max(LSize.Width, LChildControl.Margins.left + padding.left + LChildControl.width + LChildControl.Margins.right + padding.right);
            // Adjusts AControl height to ensure it contains
            // the child control at its current position.
            LSize.height := Max(LSize.height, LChildControl.Position.Y + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom);
          end;

          //--
          TALAlignLayout.Left,
          TALAlignLayout.MostLeft,
          TALAlignLayout.Right,
          TALAlignLayout.MostRight: Begin
            // Adjusts AControl width to ensure it contains
            // the child control at its current position.
            LSize.Width := Max(LSize.Width, LChildControl.Position.X + LChildControl.width + LChildControl.Margins.right + padding.right);
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeY then
              // If the child control has autosize enabled on the X-axis, adjusts
              // AControl height to ensure it contains the child control at its
              // current position. For example, TALText will never have
              // HasUnconstrainedAutosizeX set to true with TALAlignLayout.Left,
              // but TALLayout/TRectangle will have it set to true if their
              // autosize property is enabled.
              LSize.height := Max(LSize.height, LChildControl.Position.Y + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom)
            else
              // Otherwise, do not adjust AControl height.
              LSize.height := Max(LSize.Height, Height);
          End;

          //--
          TALAlignLayout.LeftCenter,
          TALAlignLayout.LeftTop,
          TALAlignLayout.LeftBottom,
          TALAlignLayout.RightCenter,
          TALAlignLayout.RightTop,
          TALAlignLayout.RightBottom,
          TALAlignLayout.MostLeftCenter,
          TALAlignLayout.MostLeftTop,
          TALAlignLayout.MostLeftBottom,
          TALAlignLayout.MostRightCenter,
          TALAlignLayout.MostRightTop,
          TALAlignLayout.MostRightBottom: begin
            // Adjusts AControl width to ensure it contains
            // the child control at its current position.
            LSize.Width := Max(LSize.Width, LChildControl.Position.X + LChildControl.width + LChildControl.Margins.right + padding.right);
            // Adjusts AControl height to ensure it contains the
            // child control without considering its current position.
            // !! Note: This may not work well if there is another child control
            //    that is not aligned to the left or right. !!
            LSize.height := Max(LSize.height, LChildControl.Margins.top + padding.top + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom);
          end;

          //--
          //TALAlignLayout.Contents,
          //TALAlignLayout.Scale,
          //TALAlignLayout.Fit,
          //TALAlignLayout.FitLeft,
          //TALAlignLayout.FitRight,
          TALAlignLayout.Client: Begin
            if LALChildControl <> nil then begin
              if LALChildControl.HasUnconstrainedAutosizeX then LSize.Width := Max(LSize.Width, LChildControl.Position.X + LChildControl.width + LChildControl.Margins.right + padding.right)
              else LSize.Width := Max(LSize.Width, Width);
              if LALChildControl.HasUnconstrainedAutosizeY then LSize.height := Max(LSize.height, LChildControl.Position.Y + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom)
              else LSize.height := Max(LSize.Height, Height);
            end
            else begin
              LSize.Width := Max(LSize.Width, Width);
              LSize.height := Max(LSize.Height, Height);
            end;
          End;

          //--
          TALAlignLayout.Horizontal,
          TALAlignLayout.VertCenter: Begin
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeX then
              LSize.Width := Max(LSize.Width, LChildControl.Position.X + LChildControl.width + LChildControl.Margins.right + padding.right)
            else
              LSize.Width := Max(LSize.Width, Width);
          End;

          //--
          TALAlignLayout.Vertical,
          TALAlignLayout.HorzCenter: Begin
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeY then
              LSize.height := Max(LSize.height, LChildControl.Position.Y + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom)
            else
              LSize.height := Max(LSize.Height, Height);
          End;

          //--
          else
            raise Exception.Create('Error 431814A4-5A5F-462E-9491-88F1874210DC');

        end;
      end;

      if (not HasUnconstrainedAutosizeX) or (SameValue(LSize.Width, 0, Tepsilon.Position)) then
        LSize.Width := Width;
      if (not HasUnconstrainedAutosizeY) or (SameValue(LSize.Height, 0, Tepsilon.Position)) then
        LSize.Height := Height;
      SetFixedSizeBounds(Position.X, Position.Y, LSize.Width, LSize.Height);
    finally
      TNonReentrantHelper.LeaveSection(FIsAdjustingSize)
    end;
  end;
end;

{********************************}
procedure TALControl.AlignToPixel;
begin
  // Note: We do not align the position here. The position is aligned during
  // the painting process (e.g., via ALDrawDrawable).
  BeginUpdate;
  Try
    // OnResize and OnResized will be called in loaded
    var LOldOnResize := OnResize;
    var LOldOnResized := OnResized;
    if CSLoading in componentState then begin
      OnResize := DelayOnResize;
      OnResized := DelayOnResized;
    end;
    try
      Margins.Rect := ALAlignEdgesToPixelRound(Margins.Rect, ALGetScreenScale, TEpsilon.Position);
      Padding.Rect := ALAlignEdgesToPixelRound(Padding.Rect, ALGetScreenScale, TEpsilon.Position);
      case Align of
        TALAlignLayout.None,
        TALAlignLayout.Center,
        TALAlignLayout.TopCenter,
        TALAlignLayout.TopLeft,
        TALAlignLayout.TopRight,
        TALAlignLayout.LeftCenter,
        TALAlignLayout.LeftTop,
        TALAlignLayout.LeftBottom,
        TALAlignLayout.RightCenter,
        TALAlignLayout.RightTop,
        TALAlignLayout.RightBottom,
        TALAlignLayout.BottomCenter,
        TALAlignLayout.BottomLeft,
        TALAlignLayout.BottomRight,
        TALAlignLayout.MostTopCenter,
        TALAlignLayout.MostTopLeft,
        TALAlignLayout.MostTopRight,
        TALAlignLayout.MostLeftCenter,
        TALAlignLayout.MostLeftTop,
        TALAlignLayout.MostLeftBottom,
        TALAlignLayout.MostRightCenter,
        TALAlignLayout.MostRightTop,
        TALAlignLayout.MostRightBottom,
        TALAlignLayout.MostBottomCenter,
        TALAlignLayout.MostBottomLeft,
        TALAlignLayout.MostBottomRight:
          Size.Size := ALAlignDimensionToPixelRound(Size.Size, ALGetScreenScale, TEpsilon.Position);
        //--
        TALAlignLayout.Top,
        TALAlignLayout.MostTop,
        TALAlignLayout.Bottom,
        TALAlignLayout.MostBottom,
        TALAlignLayout.Horizontal,
        TALAlignLayout.VertCenter:
          Size.Height := ALAlignDimensionToPixelRound(Size.Height, ALGetScreenScale, TEpsilon.Position);
        //--
        TALAlignLayout.Left,
        TALAlignLayout.MostLeft,
        TALAlignLayout.Right,
        TALAlignLayout.MostRight,
        TALAlignLayout.Vertical,
        TALAlignLayout.HorzCenter:
          Size.Width := ALAlignDimensionToPixelRound(Size.Width, ALGetScreenScale, TEpsilon.Position);
        //--
        //TALAlignLayout.Contents,
        //TALAlignLayout.Scale,
        //TALAlignLayout.Fit,
        //TALAlignLayout.FitLeft,
        //TALAlignLayout.FitRight
        TALAlignLayout.Client:;
        //--
        else
          Raise Exception.Create('Error AC54DF90-F880-4BD5-8474-E62BD8D099FB')
      end;
    finally
      OnResize := LOldOnResize;
      OnResized := LOldOnResized;
    end;
  finally
    EndUpdate;
  end;
end;

{**************************************************}
procedure TALControl.DelayOnResize(Sender: TObject);
begin
  Include(TALControlAccessPrivate(Self).FDelayedEvents, TALControlAccessPrivate.TDelayedEvent.Resize);
end;

{***************************************************}
procedure TALControl.DelayOnResized(Sender: TObject);
begin
  Include(TALControlAccessPrivate(Self).FDelayedEvents, TALControlAccessPrivate.TDelayedEvent.Resized);
end;

{*********************************************}
function TALControl.GetDoubleBuffered: boolean;
begin
  result := False;
end;

{************************************************************}
procedure TALControl.SetDoubleBuffered(const AValue: Boolean);
begin
  // Not supported
end;

{**************************************************}
procedure TALControl.SetScale(const AValue: Single);
begin
  if not SameValue(FScale, AValue, TEpsilon.Scale) then begin
    ClearBufDrawable;
    FScale := AValue;
    DoMatrixChanged(nil);
    Repaint;
  end;
end;

{***************************************}
function TALControl.GetAutoSize: Boolean;
begin
  result := FAutoSize;
end;

{*****************************************************}
procedure TALControl.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustSize;
  end;
end;

{*****************************************************}
function TALControl.HasUnconstrainedAutosizeX: Boolean;
begin
  Result := GetAutoSize;
  if Result then begin
    result := not (Align in [TALAlignLayout.Client,
                             //TALAlignLayout.Contents,
                             TALAlignLayout.Top,
                             TALAlignLayout.Bottom,
                             TALAlignLayout.MostTop,
                             TALAlignLayout.MostBottom,
                             TALAlignLayout.Horizontal,
                             TALAlignLayout.VertCenter]);
    if (not result) and (ParentALControl <> nil) then
      Result := ParentALControl.HasUnconstrainedAutosizeX;
  end;
end;

{*****************************************************}
function TALControl.HasUnconstrainedAutosizeY: Boolean;
begin
  Result := GetAutoSize;
  if Result then begin
    result := not (Align in [TALAlignLayout.Client,
                             //TALAlignLayout.Contents,
                             TALAlignLayout.Left,
                             TALAlignLayout.Right,
                             TALAlignLayout.MostLeft,
                             TALAlignLayout.MostRight,
                             TALAlignLayout.Vertical,
                             TALAlignLayout.HorzCenter]);
    if (not result) and (ParentALControl <> nil) then
      Result := ParentALControl.HasUnconstrainedAutosizeY;
  end;
end;

{***********************************}
procedure TALControl.MakeBufDrawable;
begin
 // Virtual;
end;

{************************************}
procedure TALControl.ClearBufDrawable;
begin
 // Virtual;
end;

{******************************************************}
function TALControl.GetIsPixelAlignmentEnabled: Boolean;
begin
  Result := FIsPixelAlignmentEnabled;
end;

{*********************************************************************}
procedure TALControl.SetIsPixelAlignmentEnabled(const AValue: Boolean);
begin
  FIsPixelAlignmentEnabled := AValue;
end;

{*****************************************************}
function TALControl.IsVisibleWithinFormBounds: Boolean;
begin
  Result := GetParentedVisible;
  if not result then exit;
  if Form <> nil then
    Result := Form.ClientRect.IntersectsWith(LocalToAbsolute(LocalRect));
end;

{***********************************************}
procedure TALControl.SetNewScene(AScene: IScene);
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.Pressed is still changed only in SetNewScene/DoMouseLeave/MouseDown/MouseUp/MouseClick'}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.IsFocused is still changed only in SetNewScene/DoEnter/DoExit'}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.IsMouseOver is still changed only in SetNewScene/DoMouseEnter/DoMouseLeave'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  var LPrevIsFocused := IsFocused;
  var LPrevIsMouseOver := IsMouseOver;
  inherited;
  // At design time, when a new TEdit/TMemo is added to the form,
  // or a new TALBaseText control with AutoSize=true is added to the form,
  // the size will not adjust and will remain at its default (200x50).
  // Calling AdjustSize here will correct this.
  AdjustSize;
  {$IF defined(ANDROID) or defined(IOS)}
  FIsMouseOver := False;
  {$ENDIF}
  if LPrevPressed <> Pressed then PressedChanged;
  if LPrevIsFocused <> IsFocused then IsFocusedChanged;
  if LPrevIsMouseOver <> IsMouseOver then IsMouseOverChanged;
end;

{**************************************}
function TALControl.GetPivot: TPosition;
begin
  Result := Inherited RotationCenter;
end;

{****************************************************}
procedure TALControl.SetPivot(const Value: TPosition);
begin
  Inherited RotationCenter := Value;
end;

{**************************************}
function TALControl.GetPressed: Boolean;
begin
  result := inherited Pressed;
end;

{*****************************************************}
procedure TALControl.SetPressed(const AValue: Boolean);
begin
  if AValue <> GetPressed then begin
    inherited Pressed := AValue;
    pressedChanged;
  end;
end;

{***************************}
procedure TALControl.DoEnter;
begin
  var LPrevIsFocused := IsFocused;
  inherited;
  if LPrevIsFocused <> IsFocused then IsFocusedChanged;
end;

{**************************}
procedure TALControl.DoExit;
begin
  var LPrevIsFocused := IsFocused;
  inherited;
  if LPrevIsFocused <> IsFocused then IsFocusedChanged;
end;

{********************************}
procedure TALControl.DoMouseEnter;
begin
  var LPrevIsMouseOver := IsMouseOver;
  inherited;
  {$IF defined(ANDROID) or defined(IOS)}
  FIsMouseOver := False;
  {$ENDIF}
  if LPrevIsMouseOver <> IsMouseOver then IsMouseOverChanged;
end;

{********************************}
procedure TALControl.DoMouseLeave;
begin
  var LPrevIsMouseOver := IsMouseOver;
  inherited;
  {$IF defined(ANDROID) or defined(IOS)}
  FIsMouseOver := False;
  {$ENDIF}
  if not AutoCapture then
    Pressed := False;
  if LPrevIsMouseOver <> IsMouseOver then IsMouseOverChanged;
end;

{*************************************************************************************}
procedure TALControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  //--
  FControlAbsolutePosAtMouseDown := LocalToAbsolute(TPointF.Zero);
  FMouseDownAtLowVelocity := True;
  //--
  if FDisableDoubleClickHandling then Shift := Shift - [ssDouble];
  //--
  var LScrollableControl: IALScrollableControl;
  var LParentControl := ParentControl;
  while LParentControl <> nil do begin
    if Supports(LParentControl, IALScrollableControl, LScrollableControl) then begin
      if not LScrollableControl.GetScrollEngine.IsVelocityLow then begin
        FMouseDownAtLowVelocity := False;
        Break;
      end
      else LParentControl := LParentControl.ParentControl;
    end
    else LParentControl := LParentControl.ParentControl;
  end;
  //--
  if (not FFocusOnMouseDown) or (FFocusOnMouseUp) or (not FMouseDownAtLowVelocity) then begin
    Var LOldIsfocused := FIsfocused;
    FIsfocused := True;
    Try
      inherited;
    finally
      FIsfocused := LOldIsfocused;
    End;
  end
  else
    inherited;
  //--
  if LPrevPressed <> Pressed then PressedChanged;
end;

{***********************************************************************************}
procedure TALControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  inherited;
  if LPrevPressed <> Pressed then PressedChanged;
  var LControlAbsolutePos := LocalToAbsolute(TPointF.Zero);
  if (FFocusOnMouseUp) and
     (FMouseDownAtLowVelocity) and
     (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) <= TALScrollEngine.DefaultTouchSlop) and
     (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) <= TALScrollEngine.DefaultTouchSlop) and
     (not (csDesigning in ComponentState)) and
     (not FIsFocused) then
    SetFocus;
end;

{**************************************************************************************}
procedure TALControl.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  var LControlAbsolutePos := LocalToAbsolute(TPointF.Zero);
  if (not FMouseDownAtLowVelocity) or
     (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) > TALScrollEngine.DefaultTouchSlop) or
     (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) > TALScrollEngine.DefaultTouchSlop) then begin
    {$IF defined(debug)}
    if (not FMouseDownAtLowVelocity) then
      ALLog(Classname+'.MouseClick', 'Skipped | Mouse Down was not made at Low Velocity')
    else if (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) > TALScrollEngine.DefaultTouchSlop) then
      ALLog(Classname+'.MouseClick', 'Skipped | Control moved by '+ALFormatFloatW('0.##', abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x), ALDefaultFormatSettingsW) + ' horizontally')
    else if (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) > TALScrollEngine.DefaultTouchSlop) then
      ALLog(Classname+'.MouseClick', 'Skipped | Control moved by '+ALFormatFloatW('0.##', abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y), ALDefaultFormatSettingsW) + ' vertically')
    else
      raise Exception.Create('Error 79BF6F83-8725-476D-A283-507BE9CC671C');
    {$ENDIF}
    exit;
  end;
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  inherited;
  if LPrevPressed <> Pressed then PressedChanged;
end;

{*************************************}
// Optimized GetParentedVisible to work
// exclusively with ParentControl.
function TALControl.GetParentedVisible: Boolean;
begin
  {$IF defined(ALDPK)}
  result := Inherited GetParentedVisible;
  {$ELSE}
  var P: TControl := Self;
  Result := False;
  while P <> nil do begin
    if not p.Visible then Exit;
    P := P.ParentControl;
  end;
  // We do not care about FForm.visible like in
  // Inherited GetParentedVisible
  // Result := (FForm = nil) or (FForm.visible);
  Result := True;
  {$ENDIF}
end;

{****************************************************}
procedure TALControl.DoMatrixChanged(Sender: TObject);
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.DoMatrixChanged was not updated and adjust the IFDEF'}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2823 was not corrected and adjust the IFDEF'}
  {$ENDIF}
  if not FInPaintTo and not IsUpdating then
    Repaint;
  if SameValue(Scale, 1.0, TEpsilon.Scale) and SameValue(RotationAngle, 0.0, TEpsilon.Scale) then
  begin
    if (ParentControl <> nil) and not TALControlAccessPrivate(ParentControl).FSimpleTransform then
      TALControlAccessPrivate(Self).FSimpleTransform := False
    else
      TALControlAccessPrivate(Self).FSimpleTransform := True;
  end
  else
    TALControlAccessPrivate(Self).FSimpleTransform := False;

  if not TALControlAccessPrivate(Self).FSimpleTransform then
  begin
    if not SameValue(RotationAngle, 0.0, TEpsilon.Scale) then
    begin
      FLocalMatrix :=
        TMatrix.CreateTranslation(-Pivot.X * FSize.Width, -Pivot.Y * FSize.Height) *
        TMatrix.CreateScaling(Scale, Scale) *
        TMatrix.CreateRotation(DegToRad(RotationAngle)) *
        TMatrix.CreateTranslation(Pivot.X * FSize.Width + Position.X, Pivot.Y * FSize.Height + Position.Y);
    end
    else
    begin
      FLocalMatrix := TMatrix.Identity;
      FLocalMatrix.m31 := Position.X + ((1 - Scale) * Pivot.X * FSize.Width);
      FLocalMatrix.m32 := Position.Y + ((1 - Scale) * Pivot.Y * FSize.Height);
      FLocalMatrix.m11 := Scale;
      FLocalMatrix.m22 := Scale;
    end;
  end
  else
  begin
    FLocalMatrix := TMatrix.Identity;
    FLocalMatrix.m31 := Position.X;
    FLocalMatrix.m32 := Position.Y;
  end;

  RecalcAbsolute;
  RecalcUpdateRect;
  if HasDisablePaintEffect then
    UpdateEffects;
  if Visible then
    ParentContentChanged;

  if not GetAnchorMove then
  begin
    UpdateExplicitBounds;
    UpdateAnchorRules(True);
  end;
  if not FInPaintTo and not IsUpdating then
    Repaint;
end;

{*********************************}
procedure TALControl.DoRootChanged;
begin
  inherited;
  if Root is TCommonCustomForm then FForm := TCommonCustomForm(Root)
  else FForm := Nil;
end;

{**************************************}
procedure TALControl.IsMouseOverChanged;
begin
  // virtual
end;

{************************************}
procedure TALControl.IsFocusedChanged;
begin
  // virtual
end;

{**********************************}
procedure TALControl.PressedChanged;
begin
  // virtual
end;

{**********************************}
Procedure TALControl.MarginsChanged;
begin
  // virtual
end;

{**********************************}
procedure TALControl.PaddingChanged;
begin
  Inherited;
  AdjustSize;
end;

{*********************************}
procedure TALControl.ParentChanged;
begin
  inherited;
  // Note: The procedure TControl.PaintTo(const ACanvas: TCanvas; const ARect: TRectF; const AParent: TFmxObject = nil)
  // temporarily updates the ParentControl. Unfortunately, ParentControl is not a virtual property,
  // and TControl.UpdateParentProperties is strictly private. As a result, within TControl.PaintTo,
  // the value of FParentALControl will be incorrect.
  if (ParentControl <> nil) and (ParentControl is TALControl) then
    FParentALControl := TALControl(ParentControl)
  else
    FParentALControl := nil;
end;

{**********************************************************}
procedure TALControl.MarginsChangedHandler(Sender: TObject);
begin
  if Assigned(FFormerMarginsChangedHandler) then
    FFormerMarginsChangedHandler(Sender);
  MarginsChanged;
end;

{******************************************}
function TALControl.IsScaledStored: Boolean;
begin
  Result := not SameValue(FScale, 1, TEpsilon.Scale);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if FMX.Controls.TContentTabList was not updated and adjust the IFDEF'}
{$ENDIF}
type
  TALContentTabList = class(TTabList)
  protected
    function IsAddable(const TabStop: IControl): Boolean; override;
  end;

{*********************************************************************}
function TALContentTabList.IsAddable(const TabStop: IControl): Boolean;
var
  Content: IControl;
  Parent: TFmxObject;
begin
  Result := True;
  if Supports(Controller, IControl, Content) then
  begin
    Parent := Content.GetObject.Parent;
    if (Parent <> nil) and (Parent is TStyledControl) then
      Result := not TabStop.GetObject.Equals(_TStyledControlAccessProtected(Parent).ResourceLink);
  end;

  Result := Result and inherited IsAddable(TabStop);
end;

{************************************************}
constructor TALContent.Create(AOwner: TComponent);
begin
  inherited;
  SetAcceptsControls(False);
end;

{*****************************}
procedure TALContent.DoRealign;
var
  AlignRoot: IAlignRoot;
begin
  if (Parent <> nil) and not(csLoading in Parent.ComponentState) then
    inherited;
  if (Parent <> nil) and not FParentAligning and not(csLoading in ComponentState) then
  begin
    FParentAligning := True;
    if ParentControl <> nil then
      _TControlAccessProtected(ParentControl).Realign
    else
      if not(csLoading in ComponentState) and Supports(Parent, IAlignRoot, AlignRoot) then
        AlignRoot.Realign;
    FParentAligning := False;
  end;
end;

{*************************************************}
function TALContent.GetTabListClass: TTabListClass;
begin
  Result := TALContentTabList;
end;

{***********************************************************}
function TALContent.GetTabStopController: ITabStopController;
var
  Control: IControl;
begin
  if Supports(Parent, IControl, Control) then
    Result := Control.GetTabStopController
  else
    Result := nil;
end;

{**********************************}
procedure TALContent.ContentChanged;
begin
  {$IF defined(debug)}
  //ALLog(ClassName+'.ContentChanged', 'Name: ' + Name);
  {$ENDIF}
end;

end.
