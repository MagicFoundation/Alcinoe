unit Alcinoe.FMX.Controls;

interface

{$I Alcinoe.inc}

uses
  System.UITypes,
  system.Types,
  System.Classes,
  FMX.Forms,
  FMX.Controls,
  FMX.Types,
  FMX.Graphics;

type

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Types.TAlignLayout was not updated and adjust the IFDEF'}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2342 was implemented and adjust the IFDEF'}
  {$ENDIF}
  // ⚠ Important:
  // If the definition of FMX.Types.TAlignLayout changes in future Delphi versions,
  // review this enumeration and all casts like `TALAlignLayout(AlignLayout)`.
  // Any change in the order or number of values in TAlignLayout could cause
  // incorrect mappings or runtime errors.
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
    Contents,
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

  {*****************}
  TALAutoSizeMode = (
    None,
    {$IFNDEF ALCompilerVersionSupported123}
      {$MESSAGE WARN 'Delete this temp hack'}
    {$ENDIF}
    {$IF defined(ALBackwardCompatible)}
    False, // !! Ugly hack !!
    True, // !! Ugly hack !!
    {$ENDIF}
    Width,
    Height,
    Both);

  {*******************************************}
  TALClickSoundMode = (Default, Always, Never);

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl was not updated and adjust the IFDEF'}
  {$ENDIF}
  TALControl = class(TControl)
  private
    FForm: TCommonCustomForm; // 8 bytes
    FALParentControl: TALControl; // 8 bytes
    FFormerMarginsChangedHandler: TNotifyEvent; // 16 bytes
    FControlAbsolutePosAtMouseDown: TpointF; // 8 bytes
    FScale: TPosition; // 8 bytes | TPosition instead of TALPosition to avoid circular reference
    FFocusOnMouseDown: Boolean; // 1 byte
    FFocusOnMouseUp: Boolean; // 1 byte
    FMouseDownAtRest: Boolean; // 1 byte
    FDoubleClick: Boolean; // 1 byte
    FAutoAlignToPixel: Boolean; // 1 byte
    FAlign: TALAlignLayout; // 1 byte
    FIsSetBoundsLocked: Boolean; // 1 byte
    FBeforeDestructionExecuted: Boolean; // 1 byte
    procedure SetScale(const AValue: TPosition);
    function GetPivot: TPosition;
    procedure SetPivot(const Value: TPosition);
    function GetPressed: Boolean;
    procedure SetPressed(const AValue: Boolean);
    procedure DelayOnResize(Sender: TObject);
    procedure DelayOnResized(Sender: TObject);
    procedure MarginsChangedHandler(Sender: TObject);
    procedure ScaleChangedHandler(Sender: TObject);
  protected
    FClickSound: TALClickSoundMode; // 1 byte
    FAutoSize: TALAutoSizeMode; // 1 byte
    FIsAdjustingSize: Boolean; // 1 byte
    FAdjustSizeOnEndUpdate: Boolean; // 1 byte
    FPropagateMouseEvents: Boolean; // 1 byte — placed here (protected) because the private section has no room left for a 1-byte field
    property BeforeDestructionExecuted: Boolean read FBeforeDestructionExecuted;
    function GetDoubleBuffered: boolean; virtual;
    procedure SetDoubleBuffered(const AValue: Boolean); virtual;
    property Scale: TPosition read FScale write SetScale;
    property Pivot: TPosition read GetPivot write SetPivot;
    function GetAutoSize: TALAutoSizeMode; virtual;
    procedure SetAutoSize(const Value: TALAutoSizeMode); virtual;
    // Dynamically adjusts the dimensions to accommodate child controls,
    // considering their sizes, positions, margins, and alignments.
    property AutoSize: TALAutoSizeMode read GetAutoSize write SetAutoSize default TALAutoSizeMode.None;
    function GetAutoAlignToPixel: Boolean; virtual;
    procedure SetAutoAlignToPixel(const AValue: Boolean); Virtual;
    property FocusOnMouseDown: Boolean read FFocusOnMouseDown write FFocusOnMouseDown;
    property FocusOnMouseUp: Boolean read FFocusOnMouseUp write FFocusOnMouseUp;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoClickSound; virtual;
    procedure Click; override;
    {$IFNDEF ALCompilerVersionSupported123}
      {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-24397 have been implemented and adjust the IFDEF'}
    {$ENDIF}
    procedure ChildrenMouseDown(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure ChildrenMouseMove(const AObject: TControl; Shift: TShiftState; X, Y: Single); virtual;
    procedure ChildrenMouseUp(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure ChildrenMouseEnter(const AObject: TControl); virtual;
    procedure ChildrenMouseLeave(const AObject: TControl); virtual;
    function IsInMotion: Boolean; virtual;
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
    procedure ParentRealigning; virtual;
    /// <summary>
    ///   Return the largest size this control can have while still
    ///   being fully contained within its container (parent).
    /// </summary>
    function GetMaxContainedSize: TSizeF; Virtual;
    procedure AdjustSize; virtual;
    procedure SetFixedSizeBounds(X, Y, AWidth, AHeight: Single); Virtual;
    function GetAbsoluteDisplayedRect: TRectF; virtual;
    function FillTextFlags: TFillTextFlags; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure Assign(Source: TPersistent{TALControl}); override;
    procedure EndUpdate; override;
    procedure SetNewScene(AScene: IScene); override;
    function IsReadyToDisplay(const AStrict: Boolean = False): Boolean; virtual;
    function IsDisplayed: Boolean; virtual;
    property DisplayedRect: TRectF read GetAbsoluteDisplayedRect;
    property Form: TCommonCustomForm read FForm;
    {$IFNDEF ALCompilerVersionSupported123}
      {$MESSAGE WARN 'Check if property FMX.Controls.TControl.Pressed still not fire a PressChanged event when it gets updated, and adjust the IFDEF'}
    {$ENDIF}
    property Pressed: Boolean read GetPressed write SetPressed;
    procedure AlignToPixel; virtual;
    procedure ApplyColorScheme; virtual;
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
    function HasUnconstrainedAutosizeWidth: Boolean; virtual;
    function HasUnconstrainedAutosizeHeight: Boolean; virtual;
    procedure MakeBufDrawable; virtual;
    procedure ClearBufDrawable; virtual;
    property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered default False;
    /// <summary>
    ///   When AutoAlignToPixel is true, all dimensions used to build the buffered drawable
    ///   are aligned to the pixel grid. Additionally, after the object is loaded, all properties
    ///   related to the pixel grid (e.g., margins) are automatically aligned. Note that setting these
    ///   properties at runtime does not change their alignment; alignment is applied only via the
    ///   loading process.
    /// </summary>
    property AutoAlignToPixel: Boolean read GetAutoAlignToPixel write SetAutoAlignToPixel;
    property Align: TALAlignLayout read FAlign write SetAlign default TALAlignLayout.None;
    property ALParentControl: TALControl read FALParentControl;
    property ClickSound: TALClickSoundMode read FClickSound write FClickSound default TALClickSoundMode.Default;
    property PropagateMouseEvents: Boolean read FPropagateMouseEvents write FPropagateMouseEvents default True;
  end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Controls.TContent was not updated and adjust the IFDEF'}
  {$ENDIF}
  TALContent = class(TALControl, IContent)
  protected
    function GetTabStopController: ITabStopController; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
    procedure DoDeleteChildren; override;
    procedure DoRealign; override;
    procedure IContent.Changed = ContentChanged;
    procedure DoContentChanged; Virtual;
    procedure ContentChanged;
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
    property ClickSound;
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
    property OnDblClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

var
  ALGlobalClickSoundEnabled: Boolean;

implementation

uses
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  {$IF defined(ALDPK)}
  System.Rtti,
  {$ENDIF}
  {$IF defined(IOS)}
  FMX.Platform.iOS,
  {$ENDIF}
  Fmx.utils,
  Alcinoe.FMX.styles,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.ScrollEngine,
  Alcinoe.Localization,
  Alcinoe.StringUtils,
  Alcinoe.Common;

{**}
Type
  _TControlProtectedAccess = class(Tcontrol);
  _TCustomFormProtectedAccess = class(TCustomForm);
  _TStyledControlProtectedAccess = class(TStyledControl);

{************************************************}
constructor TALControl.Create(AOwner: TComponent);
begin
  inherited;
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if MarginsChanged is not implemented in FMX.Controls.TControl and adjust the IFDEF'}
  {$ENDIF}
  FFormerMarginsChangedHandler := Margins.OnChange;
  Margins.OnChange := MarginsChangedHandler;
  Size.SetPlatformDefaultWithoutNotification(False);
  FForm := nil;
  FALParentControl := nil;
  FControlAbsolutePosAtMouseDown := TpointF.zero;
  FScale := TPosition.Create(TPointF.Create(1, 1));
  FScale.OnChange := ScaleChangedHandler;
  FFocusOnMouseDown := False;
  FFocusOnMouseUp := False;
  FMouseDownAtRest := True;
  FDoubleClick := False;
  FAutoAlignToPixel := True;
  FAlign := TALAlignLayout.None;
  FIsSetBoundsLocked := False;
  FBeforeDestructionExecuted := False;
  FClickSound := TALClickSoundMode.Default;
  FAutoSize := TALAutoSizeMode.None;
  FIsAdjustingSize := False;
  FAdjustSizeOnEndUpdate := False;
  FPropagateMouseEvents := True;
end;

{****************************}
destructor TALControl.Destroy;
begin
  ClearBufDrawable;
  ALFreeAndNil(FScale);
  inherited;
end;

{*************************************}
procedure TALControl.BeforeDestruction;
begin
  if FBeforeDestructionExecuted then exit;
  FBeforeDestructionExecuted := True;
  for var I := 0 to ControlsCount - 1 do
    Controls[I].BeforeDestruction;
  inherited;
end;

{***********************************************************}
procedure TALControl.Assign(Source: TPersistent{TALControl});
begin
  BeginUpdate;
  Try
    if Source is TALControl then begin
      // --TALControl
      Align := TALControl(Source).Align;
      AutoAlignToPixel := TALControl(Source).AutoAlignToPixel;
      AutoSize := TALControl(Source).AutoSize;
      DoubleBuffered := TALControl(Source).DoubleBuffered;
      Pivot.Assign(TALControl(Source).Pivot);
      Scale.assign(TALControl(Source).Scale);
      // --TControl
      Anchors := TALControl(Source).Anchors;
      CanFocus := TALControl(Source).CanFocus;
      CanParentFocus := TALControl(Source).CanParentFocus;
      ClipChildren := TALControl(Source).ClipChildren;
      ClipParent := TALControl(Source).ClipParent;
      Cursor := TALControl(Source).Cursor;
      DisabledOpacity := TALControl(Source).DisabledOpacity;
      DragMode := TALControl(Source).DragMode;
      EnableDragHighlight := TALControl(Source).EnableDragHighlight;
      Enabled := TALControl(Source).Enabled;
      Hint := TALControl(Source).Hint;
      HitTest := TALControl(Source).HitTest;
      Locked := TALControl(Source).Locked;
      Margins.Assign(TALControl(Source).Margins);
      Opacity := TALControl(Source).Opacity;
      Padding.Assign(TALControl(Source).Padding);
      ParentShowHint := TALControl(Source).ParentShowHint;
      Position.Assign(TALControl(Source).Position);
      RotationAngle := TALControl(Source).RotationAngle;
      ShowHint := TALControl(Source).ShowHint;
      Size.Assign(TALControl(Source).Size);
      StyleName := TALControl(Source).StyleName;
      TabOrder := TALControl(Source).TabOrder;
      TabStop := TALControl(Source).TabStop;
      Tag := TALControl(Source).Tag;
      TagFloat := TALControl(Source).TagFloat;
      TagObject := TALControl(Source).TagObject;
      TagString := TALControl(Source).TagString;
      TouchTargetExpansion.Assign(TALControl(Source).TouchTargetExpansion);
      Visible := TALControl(Source).Visible;
      OnDragEnter := TALControl(Source).OnDragEnter;
      OnDragLeave := TALControl(Source).OnDragLeave;
      OnDragOver := TALControl(Source).OnDragOver;
      OnDragDrop := TALControl(Source).OnDragDrop;
      OnDragEnd := TALControl(Source).OnDragEnd;
      OnKeyDown := TALControl(Source).OnKeyDown;
      OnKeyUp := TALControl(Source).OnKeyUp;
      OnClick := TALControl(Source).OnClick;
      OnDblClick := TALControl(Source).OnDblClick;
      OnCanFocus := TALControl(Source).OnCanFocus;
      OnEnter := TALControl(Source).OnEnter;
      OnExit := TALControl(Source).OnExit;
      OnMouseDown := TALControl(Source).OnMouseDown;
      OnMouseMove := TALControl(Source).OnMouseMove;
      OnMouseUp := TALControl(Source).OnMouseUp;
      OnMouseWheel := TALControl(Source).OnMouseWheel;
      OnMouseEnter := TALControl(Source).OnMouseEnter;
      OnMouseLeave := TALControl(Source).OnMouseLeave;
      OnPainting := TALControl(Source).OnPainting;
      OnPaint := TALControl(Source).OnPaint;
      OnResize := TALControl(Source).OnResize;
      OnResized := TALControl(Source).OnResized;
      OnActivate := TALControl(Source).OnActivate;
      OnDeactivate := TALControl(Source).OnDeactivate;
    end
    else
      ALAssignError(Source{ASource}, Self{ADest});
  Finally
    EndUpdate;
  End;
end;

{***************************************************************}
// The current implementation of TControl's BeginUpdate/EndUpdate
// and Realign methods is inefficient—particularly for TALText,
// which must rebuild its internal buffer every time its size
// changes. When EndUpdate is called, the update propagates through
// the deepest nested children first. For example, consider the
// following hierarchy:
//
// Control1
//   └─ Control2
//         └─ AlText1
//
// When Control1.EndUpdate is called, the sequence of events is as
// follows:
//
//   * AlText1.EndUpdate is called first, triggering an AdjustSize
//     and a Realign.
//   * Next, Control2.EndUpdate executes, which calls Realign and
//     may trigger AlText1.AdjustSize again.
//   * Finally, Control1.EndUpdate runs, calling Realign and possibly
//     causing yet another AlText1.AdjustSize.
//
// This series of operations results in the BufDrawable being
// recalculated multiple times, leading to significant performance
// overhead.
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if FMX.Controls.TControl.EndUpdate was not updated and adjust the IFDEF'}
{$ENDIF}
procedure TALControl.EndUpdate;
begin
  if IsUpdating then
  begin
    Dec(FUpdating);
    if not IsUpdating then
    begin
      DoEndUpdate;
      //RefreshInheritedCursorForChildren;
    end;
    for var I := 0 to ControlsCount - 1 do
      Controls[I].EndUpdate;
  end;
end;

{**************************}
procedure TALControl.Loaded;
begin
  {$IF not defined(ALDPK)}
  if AutoAlignToPixel then
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
      TALAlignLayout.Contents: LLegacyAlign := TAlignLayout.Contents;
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

  {$IFNDEF ALCompilerVersionSupported123}
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

{************************************}
procedure TALControl.ParentRealigning;
begin
  // Virtual
end;

{*********************************************************************}
procedure TALControl.SetFixedSizeBounds(X, Y, AWidth, AHeight: Single);
begin
  if TNonReentrantHelper.EnterSection(FIsSetBoundsLocked) then begin
    try

      {$IF defined(debug)}
      //ALLog(ClassName+'.SetFixedSizeBounds', 'Name: ' + Name + ' | X : '+ALFloatToStrW(X)+'('+ALFloatToStrW(Position.X)+') | Y : '+ALFloatToStrW(Y)+'('+ALFloatToStrW(Position.Y)+') | AWidth : '+ALFloatToStrW(AWidth)+'('+ALFloatToStrW(Width)+') | AHeight : '+ALFloatToStrW(AHeight)+'('+ALFloatToStrW(Height)+')');
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
  // ParentControl.FDisableAlign = True means SetBounds was
  // called by the AlignObjects procedure inside ParentControl.DoRealign.
  // Fortunately, AlignObjects systematically calls SetBounds on all
  // its children, even when there’s nothing to update.
  var LParentRealigning := (((ParentControl <> nil) and (_TControlProtectedAccess(ParentControl).FDisableAlign)) or
                            ((ParentControl = nil) and (FForm <> nil) and (_TCustomFormProtectedAccess(FForm).FDisableAlign)));
  If LParentRealigning then
    ParentRealigning;

  {$IFNDEF ALCompilerVersionSupported123}
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
  If (LParentRealigning) and
     (integer(FAlign) >= integer(TALAlignLayout.TopCenter)) and
     (integer(FAlign) <= integer(TALAlignLayout.MostBottomRight)) then begin
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

  if FIsSetBoundsLocked then begin
    AWidth := Width;
    AHeight := Height;
  end;

  {$IF defined(debug)}
  //var LMoved := not (SameValue(X, Position.X, TEpsilon.Position) and SameValue(Y, Position.Y, TEpsilon.Position));
  //var LSizeChanged := not (SameValue(AWidth, Width, TEpsilon.Position) and SameValue(AHeight, Height, TEpsilon.Position));
  //if LMoved or LSizeChanged then
  //  ALLog(ClassName+'.SetBounds', 'Name: ' + Name + ' | X : '+ALFloatToStrW(X)+'('+ALFloatToStrW(Position.X)+') | Y : '+ALFloatToStrW(Y)+'('+ALFloatToStrW(Position.Y)+') | AWidth : '+ALFloatToStrW(AWidth)+'('+ALFloatToStrW(Width)+') | AHeight : '+ALFloatToStrW(AHeight)+'('+ALFloatToStrW(Height)+')');
  {$ENDIF}

  inherited;
end;

{**********************************************}
function TALControl.GetMaxContainedSize: TSizeF;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function CalculateAvailableClientSize(
             const AParentWidth: Single;
             const AParentHeight: Single;
             const AParentPadding: TRectF;
             const AChildControls: TArray<TControl>;
             const AChildControlsCount: Integer): TSizeF;
  begin

    if Align in [TALAlignLayout.None, TALAlignLayout.Center] then begin
      Result := TSizeF.Create(65535, 65535);
      Exit;
    end
    else if Align = TALAlignLayout.Contents then begin
      Result := TSizeF.Create(
                  AParentWidth - Margins.Left - Margins.Right,
                  AParentHeight - Margins.top - Margins.Bottom);
      Exit;
    end;

    Result := TSizeF.Create(
                AParentWidth - AParentPadding.Left - AParentPadding.Right,
                AParentHeight - AParentPadding.Top - AParentPadding.Bottom);

    var IsHorzAligned := Align in [TALAlignLayout.Top,
                                   TALAlignLayout.TopCenter,
                                   TALAlignLayout.TopLeft,
                                   TALAlignLayout.TopRight,
                                   TALAlignLayout.Bottom,
                                   TALAlignLayout.BottomCenter,
                                   TALAlignLayout.BottomLeft,
                                   TALAlignLayout.BottomRight,
                                   TALAlignLayout.Client,
                                   TALAlignLayout.VertCenter,
                                   TALAlignLayout.Horizontal];
    //var IsMostHorzAligned := Align in [TALAlignLayout.MostTop,
    //                                   TALAlignLayout.MostTopCenter,
    //                                   TALAlignLayout.MostTopLeft,
    //                                   TALAlignLayout.MostTopRight,
    //                                   TALAlignLayout.MostBottom,
    //                                   TALAlignLayout.MostBottomCenter,
    //                                   TALAlignLayout.MostBottomLeft,
    //                                   TALAlignLayout.MostBottomRight];
    var IsVertAligned := Align in [TALAlignLayout.Left,
                                   TALAlignLayout.LeftCenter,
                                   TALAlignLayout.LeftTop,
                                   TALAlignLayout.LeftBottom,
                                   TALAlignLayout.Right,
                                   TALAlignLayout.RightCenter,
                                   TALAlignLayout.RightTop,
                                   TALAlignLayout.RightBottom,
                                   TALAlignLayout.Client,
                                   TALAlignLayout.HorzCenter,
                                   TALAlignLayout.Vertical];
    var IsMostVertAligned := Align in [TALAlignLayout.MostLeft,
                                       TALAlignLayout.MostLeftCenter,
                                       TALAlignLayout.MostLeftTop,
                                       TALAlignLayout.MostLeftBottom,
                                       TALAlignLayout.MostRight,
                                       TALAlignLayout.MostRightCenter,
                                       TALAlignLayout.MostRightTop,
                                       TALAlignLayout.MostRightBottom];
    // These align values are not classified as horizontal
    // or vertical alignment:
    //   TALAlignLayout.None
    //   TALAlignLayout.Contents
    //   TALAlignLayout.Center

    for var I := 0 to AChildControlsCount - 1 do begin
      var LChildControl := AChildControls[I];
      if not LChildControl.Visible then Continue;
      if LChildControl = Self then Continue;
      {$IF defined(ALDPK)}
      // At design time, the Delphi IDE may add children such as
      // TGrabHandle.TGrabHandleRectangle
      if Supports(LChildControl, IDesignerControl) then
        Continue;
      {$ENDIF}

      var LChildControlAlign: TALAlignLayout;
      If (LChildControl is TALControl) then LChildControlAlign := TALControl(LChildControl).Align
      else begin
        {$If defined(debug)}
        if LChildControl.Align in [TAlignLayout.Scale,
                                   TAlignLayout.Fit,
                                   TAlignLayout.FitLeft,
                                   TAlignLayout.FitRight] then
          Raise Exception.Create('Align values Scale, Fit, FitLeft, and FitRight are not supported');
        {$ENDIF}
        LChildControlAlign := TALAlignLayout(LChildControl.Align);
      end;

      //
      //  #################            ######## #################            ##################            ##################
      //  #      TOP      #            #      # #      TOP      #            #    MOST TOP    #            #    MOST TOP    #
      //  #################            #      # #################            ##################            ##################
      //                               #      #
      //  ########                     # MOST #                              ########                      ########
      //  #      #                     # LEFT #                              #      #                      #      #
      //  # LEFT #                     #      #                              # LEFT #                      # MOST #
      //  #      #                     #      #                              #      #                      # LEFT #
      //  #      #                     #      #                              #      #                      #      #
      //  ########                     ########                              ########                      ########
      //

      case LChildControlAlign of

        //--
        TALAlignLayout.None,
        TALAlignLayout.Center,
        TALAlignLayout.Contents,
        TALAlignLayout.Client,
        TALAlignLayout.Horizontal,
        TALAlignLayout.VertCenter,
        TALAlignLayout.Vertical,
        TALAlignLayout.HorzCenter:;

        //--
        TALAlignLayout.Top,
        TALAlignLayout.TopCenter,
        TALAlignLayout.TopLeft,
        TALAlignLayout.TopRight,
        TALAlignLayout.MostTop,
        TALAlignLayout.MostTopCenter,
        TALAlignLayout.MostTopLeft,
        TALAlignLayout.MostTopRight,
        TALAlignLayout.Bottom,
        TALAlignLayout.BottomCenter,
        TALAlignLayout.BottomLeft,
        TALAlignLayout.BottomRight,
        TALAlignLayout.MostBottom,
        TALAlignLayout.MostBottomCenter,
        TALAlignLayout.MostBottomLeft,
        TALAlignLayout.MostBottomRight:
          Result.Height := Result.Height - LChildControl.Height - LChildControl.Margins.Top - LChildControl.Margins.Bottom;

        //--
        TALAlignLayout.Left,
        TALAlignLayout.LeftCenter,
        TALAlignLayout.LeftTop,
        TALAlignLayout.LeftBottom,
        TALAlignLayout.Right,
        TALAlignLayout.RightCenter,
        TALAlignLayout.RightTop,
        TALAlignLayout.RightBottom: begin
          if IsVertAligned or
             IsMostVertAligned then
            Result.Width := Result.Width - LChildControl.Width - LChildControl.Margins.Left - LChildControl.Margins.Right;
        end;

        //--
        TALAlignLayout.MostLeft,
        TALAlignLayout.MostLeftCenter,
        TALAlignLayout.MostLeftTop,
        TALAlignLayout.MostLeftBottom,
        TALAlignLayout.MostRight,
        TALAlignLayout.MostRightCenter,
        TALAlignLayout.MostRightTop,
        TALAlignLayout.MostRightBottom: begin
          if IsVertAligned or
             IsMostVertAligned or
             IsHorzAligned then
            Result.Width := Result.Width - LChildControl.Width - LChildControl.Margins.Left - LChildControl.Margins.Right;
        end;

        //--
        else
          raise Exception.Create('Error 6DF0FA18-83E4-4B1C-806C-D04A6ED29DB0');

      end;
    end;

    if not (Align in [TALAlignLayout.None,
                      TALAlignLayout.Center]) then begin
      Result.Width := Result.Width - Margins.Left - Margins.Right;
      Result.Height := Result.Height - Margins.Top - Margins.Bottom;
    end;
  end;

begin
  if ALParentcontrol <> nil then begin
    var LHasUnconstrainedAutosizeWidth := ALParentcontrol.HasUnconstrainedAutosizeWidth;
    var LHasUnconstrainedAutosizeHeight := ALParentcontrol.HasUnconstrainedAutosizeHeight;
    if (LHasUnconstrainedAutosizeWidth) and (LHasUnconstrainedAutosizeHeight) then
      Exit(TSizeF.Create(65535, 65535));

    Result := CalculateAvailableClientSize(
                Parentcontrol.Width, // const AWidth: Single;
                Parentcontrol.Height, // const AHeight: Single;
                Parentcontrol.Padding.Rect, // const APadding: TRectF;
                Parentcontrol.Controls.PList^, // const AChildControls: TArray<TControl>
                Parentcontrol.ControlsCount); // const AChildControlsCount: Integer

    if (LHasUnconstrainedAutosizeWidth) then Result.Width := 65535;
    if (LHasUnconstrainedAutosizeHeight) then Result.Height := 65535;

    var LParentControl := ParentControl;
    if LParentControl is TALContent then LParentControl := LParentControl.ParentControl;
    if LParentControl <> nil then begin
      var LScrollableControl: IALScrollableControl;
      if (Supports(LParentControl, IALScrollableControl, LScrollableControl)) then begin
        if ttVertical in LScrollableControl.GetScrollEngine.TouchTracking then Result.Height := 65535;
        if tthorizontal in LScrollableControl.GetScrollEngine.TouchTracking then Result.Width := 65535;
      end;
    end;
  end
  else if Parentcontrol <> nil then begin
    Result := CalculateAvailableClientSize(
                Parentcontrol.Width, // const AWidth: Single;
                Parentcontrol.Height, // const AHeight: Single;
                Parentcontrol.Padding.Rect, // const APadding: TRectF;
                Parentcontrol.Controls.PList^, // const AChildControls: TArray<TControl>
                Parentcontrol.ControlsCount); // const AChildControlsCount: Integer

    var LParentControl := ParentControl;
    if LParentControl is TALContent then LParentControl := LParentControl.ParentControl;
    if LParentControl <> nil then begin
      var LScrollableControl: IALScrollableControl;
      if (Supports(LParentControl, IALScrollableControl, LScrollableControl)) then begin
        if ttVertical in LScrollableControl.GetScrollEngine.TouchTracking then Result.Height := 65535;
        if tthorizontal in LScrollableControl.GetScrollEngine.TouchTracking then Result.Width := 65535;
      end;
    end;
  end
  else if FForm <> nil then begin
    var LChildControls: TArray<TControl>;
    Setlength(LChildControls, FForm.ChildrenCount);
    var LChildControlsCount := 0;
    For var I := 0 to FForm.ChildrenCount - 1 do begin
      var LObject := FForm.Children[i];
      if LObject is TControl then begin
        LChildControls[LChildControlsCount] := TControl(LObject);
        inc(LChildControlsCount);
      end;
    end;
    var LClientSize := _TCustomFormProtectedAccess(FForm).FWinService.GetClientSize(FForm);
    Result := CalculateAvailableClientSize(
                LClientSize.X , // const AWidth: Single;
                LClientSize.Y, // const AHeight: Single;
                FForm.Padding.Rect, // const APadding: TRectF;
                LChildControls, // const AChildControls: TArray<TControl>
                LChildControlsCount); // const AChildControlsCount: Integer
  end
  else
    Result := TSizeF.Create(Width, Height);
end;

{******************************}
procedure TALControl.AdjustSize;
begin
  var LHasUnconstrainedAutosizeWidth := HasUnconstrainedAutosizeWidth;
  var LHasUnconstrainedAutosizeHeight := HasUnconstrainedAutosizeHeight;
  if (not (csLoading in ComponentState)) and // Loaded will call again AdjustSize
     (not (csDestroying in ComponentState)) and // If csDestroying do not do autosize
     (ControlsCount > 0) and // If there are no controls, do not perform autosizing
     (LHasUnconstrainedAutosizeWidth or LHasUnconstrainedAutosizeHeight) and // If AutoSize is false nothing to adjust
     (TNonReentrantHelper.EnterSection(FIsAdjustingSize)) then begin // Non-reantrant
    try

      if IsUpdating then begin
        FAdjustSizeOnEndUpdate := True;
        Exit;
      end
      else
        FAdjustSizeOnEndUpdate := False;

      {$IF defined(debug)}
      //ALLog(ClassName+'.AdjustSize', 'Name: ' + Name + ' | HasUnconstrainedAutosize(X/Y) : '+ALBoolToStrW(LHasUnconstrainedAutosizeWidth)+'/'+ALBoolToStrW(LHasUnconstrainedAutosizeHeight));
      {$ENDIF}

      var LSize := TSizeF.Create(0,0);
      for var I := 0 to ControlsCount - 1 do begin
        var LChildControl := Controls[I];
        if not LChildControl.Visible then Continue;
        {$IF defined(ALDPK)}
        // At design time, the Delphi IDE may add children such as
        // TGrabHandle.TGrabHandleRectangle
        if (csDesigning in ComponentState) and Supports(LChildControl, IDesignerControl) then
          Continue;
        {$ENDIF}

        var LALChildControl: TALControl;
        var LChildControlAlign: TALAlignLayout;
        If (LChildControl is TALControl) then begin
          LALChildControl := TALControl(LChildControl);
          LChildControlAlign := LALChildControl.Align
        end
        else begin
          LALChildControl := nil;
          LChildControlAlign := TALAlignLayout(LChildControl.Align);
        end;

        case LChildControlAlign of

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
          TALAlignLayout.MostTop: begin
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeWidth then
              // If the child control has autosize enabled on the X-axis, adjusts
              // AControl width to ensure it contains the child control at its
              // current position. For example, TALText will never have
              // HasUnconstrainedAutosizeWidth set to true with TALAlignLayout.Top,
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
          TALAlignLayout.Bottom,
          TALAlignLayout.MostBottom: begin
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeWidth then
              // If the child control has autosize enabled on the X-axis, adjusts
              // AControl width to ensure it contains the child control at its
              // current position. For example, TALText will never have
              // HasUnconstrainedAutosizeWidth set to true with TALAlignLayout.Top,
              // but TALLayout/TRectangle will have it set to true if their
              // autosize property is enabled.
              LSize.Width := Max(LSize.Width, LChildControl.Position.X + LChildControl.width + LChildControl.Margins.right + padding.right)
            else
              // Otherwise, do not adjust AControl width.
              LSize.Width := Max(LSize.Width, Width);
            // Adjusts AControl height to ensure it contains
            // the child control at its current position.
            LSize.height := Max(LSize.height, Height - LChildControl.Position.Y + LChildControl.Margins.Top + padding.Top);
          end;

          //--
          TALAlignLayout.TopCenter,
          TALAlignLayout.TopLeft,
          TALAlignLayout.TopRight,
          TALAlignLayout.MostTopCenter,
          TALAlignLayout.MostTopLeft,
          TALAlignLayout.MostTopRight: begin
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
          TALAlignLayout.BottomCenter,
          TALAlignLayout.BottomLeft,
          TALAlignLayout.BottomRight,
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
            LSize.height := Max(LSize.height, Height - LChildControl.Position.Y + LChildControl.Margins.Top + padding.Top);
          end;

          //--
          TALAlignLayout.Left,
          TALAlignLayout.MostLeft: Begin
            // Adjusts AControl width to ensure it contains
            // the child control at its current position.
            LSize.Width := Max(LSize.Width, LChildControl.Position.X + LChildControl.width + LChildControl.Margins.right + padding.right);
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeHeight then
              // If the child control has autosize enabled on the X-axis, adjusts
              // AControl height to ensure it contains the child control at its
              // current position. For example, TALText will never have
              // HasUnconstrainedAutosizeWidth set to true with TALAlignLayout.Left,
              // but TALLayout/TRectangle will have it set to true if their
              // autosize property is enabled.
              LSize.height := Max(LSize.height, LChildControl.Position.Y + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom)
            else
              // Otherwise, do not adjust AControl height.
              LSize.height := Max(LSize.Height, Height);
          End;

          //--
          TALAlignLayout.Right,
          TALAlignLayout.MostRight: Begin
            // Adjusts AControl width to ensure it contains
            // the child control at its current position.
            LSize.Width := Max(LSize.Width, Width - LChildControl.Position.X + LChildControl.Margins.left + padding.left);
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeHeight then
              // If the child control has autosize enabled on the X-axis, adjusts
              // AControl height to ensure it contains the child control at its
              // current position. For example, TALText will never have
              // HasUnconstrainedAutosizeWidth set to true with TALAlignLayout.Left,
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
          TALAlignLayout.MostLeftCenter,
          TALAlignLayout.MostLeftTop,
          TALAlignLayout.MostLeftBottom: begin
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
          TALAlignLayout.RightCenter,
          TALAlignLayout.RightTop,
          TALAlignLayout.RightBottom,
          TALAlignLayout.MostRightCenter,
          TALAlignLayout.MostRightTop,
          TALAlignLayout.MostRightBottom: begin
            // Adjusts AControl width to ensure it contains
            // the child control at its current position.
            LSize.Width := Max(LSize.Width, Width - LChildControl.Position.X + LChildControl.Margins.left + padding.left);
            // Adjusts AControl height to ensure it contains the
            // child control without considering its current position.
            // !! Note: This may not work well if there is another child control
            //    that is not aligned to the left or right. !!
            LSize.height := Max(LSize.height, LChildControl.Margins.top + padding.top + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom);
          end;

          //--
          TALAlignLayout.Contents,
          //TALAlignLayout.Scale,
          //TALAlignLayout.Fit,
          //TALAlignLayout.FitLeft,
          //TALAlignLayout.FitRight,
          TALAlignLayout.Client: Begin
            if LALChildControl <> nil then begin
              if LALChildControl.HasUnconstrainedAutosizeWidth then LSize.Width := Max(LSize.Width, LChildControl.Position.X + LChildControl.width + LChildControl.Margins.right + padding.right)
              else LSize.Width := Max(LSize.Width, Width);
              if LALChildControl.HasUnconstrainedAutosizeHeight then LSize.height := Max(LSize.height, LChildControl.Position.Y + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom)
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
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeWidth then
              LSize.Width := Max(LSize.Width, LChildControl.Position.X + LChildControl.width + LChildControl.Margins.right + padding.right)
            else
              LSize.Width := Max(LSize.Width, Width);
          End;

          //--
          TALAlignLayout.Vertical,
          TALAlignLayout.HorzCenter: Begin
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeHeight then
              LSize.height := Max(LSize.height, LChildControl.Position.Y + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom)
            else
              LSize.height := Max(LSize.Height, Height);
          End;

          //--
          else
            raise Exception.Create('Error 431814A4-5A5F-462E-9491-88F1874210DC');

        end;
      end;

      if (not LHasUnconstrainedAutosizeWidth) or (SameValue(LSize.Width, 0, Tepsilon.Position)) then
        LSize.Width := Width;
      if (not LHasUnconstrainedAutosizeHeight) or (SameValue(LSize.Height, 0, Tepsilon.Position)) then
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
        TALAlignLayout.Contents,
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

{************************************}
procedure TALControl.ApplyColorScheme;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure ApplyColorSchemeRecursive(const AControl: TControl);
  begin
    for var I := 0 to AControl.ControlsCount - 1 do
      if AControl.Controls[i] is TALControl then TALControl(AControl.Controls[i]).ApplyColorScheme
      else ApplyColorSchemeRecursive(AControl.Controls[i]);
  end;

begin
  // ClearBufDrawable because when switching between dark and light mode,
  // the resource name remains the same, but the loaded resource changes
  // (e.g., xxx_dark instead of xxx in dark mode).
  ClearBufDrawable;
  ApplyColorSchemeRecursive(self);
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

{*****************************************************}
procedure TALControl.SetScale(const AValue: TPosition);
begin
  FScale.Assign(AValue);
end;

{***********************************************}
function TALControl.GetAutoSize: TALAutoSizeMode;
begin
  result := FAutoSize;
end;

{*************************************************************}
procedure TALControl.SetAutoSize(const Value: TALAutoSizeMode);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    {$IFNDEF ALCompilerVersionSupported123}
      {$MESSAGE WARN 'Delete this temp hack'}
    {$ENDIF}
    {$IF defined(ALBackwardCompatible)}
    if FAutoSize = TALAutoSizeMode.False then
      FAutoSize := TALAutoSizeMode.None
    else if FAutoSize = TALAutoSizeMode.True then
      FAutoSize := TALAutoSizeMode.Both;
    {$ENDIF}
    AdjustSize;
  end;
end;

{*********************************************************}
function TALControl.HasUnconstrainedAutosizeWidth: Boolean;
begin
  Result := GetAutoSize in [TALAutoSizeMode.Both, TALAutoSizeMode.Width];
  if Result then begin
    result := not (Align in [TALAlignLayout.Client,
                             TALAlignLayout.Contents,
                             TALAlignLayout.Top,
                             TALAlignLayout.Bottom,
                             TALAlignLayout.MostTop,
                             TALAlignLayout.MostBottom,
                             TALAlignLayout.Horizontal,
                             TALAlignLayout.VertCenter]);
    if (not result) and (ALParentControl <> nil) then
      Result := ALParentControl.HasUnconstrainedAutosizeWidth;
  end;
end;

{**********************************************************}
function TALControl.HasUnconstrainedAutosizeHeight: Boolean;
begin
  Result := GetAutoSize in [TALAutoSizeMode.Both, TALAutoSizeMode.Height];
  if Result then begin
    result := not (Align in [TALAlignLayout.Client,
                             TALAlignLayout.Contents,
                             TALAlignLayout.Left,
                             TALAlignLayout.Right,
                             TALAlignLayout.MostLeft,
                             TALAlignLayout.MostRight,
                             TALAlignLayout.Vertical,
                             TALAlignLayout.HorzCenter]);
    if (not result) and (ALParentControl <> nil) then
      Result := ALParentControl.HasUnconstrainedAutosizeHeight;
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

{***********************************************}
function TALControl.GetAutoAlignToPixel: Boolean;
begin
  Result := FAutoAlignToPixel;
end;

{**************************************************************}
procedure TALControl.SetAutoAlignToPixel(const AValue: Boolean);
begin
  FAutoAlignToPixel := AValue;
end;

{****************************************************************************}
function TALControl.IsReadyToDisplay(const AStrict: Boolean = False): Boolean;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function CheckAllChildrenAreReadyToDisplay(const AControl: TControl): boolean;
  begin
    Result := True;
    for var I := 0 to AControl.ControlsCount - 1 do begin
      if AControl.Controls[i] is TALControl then Result := TALControl(AControl.Controls[i]).IsReadyToDisplay(AStrict)
      else Result := CheckAllChildrenAreReadyToDisplay(AControl.Controls[i]);
      if not Result then exit;
    end;
  end;

begin
  MakeBufDrawable;
  Result := CheckAllChildrenAreReadyToDisplay(Self);
end;

{***************************************}
function TALControl.IsDisplayed: Boolean;
begin
  Result := not GetAbsoluteDisplayedRect.IsEmpty;
end;

{***************************************************}
function TALControl.GetAbsoluteDisplayedRect: TRectF;
begin
  if (not Visible) or (form = nil) then Exit(TRectF.Empty);
  var LAbsoluteIntersectionRect := AbsoluteRect;
  var LControlTmp := ParentControl;
  while LControlTmp <> nil do begin
    if not LControlTmp.Visible then Exit(TRectF.Empty);
    if LControlTmp.ClipChildren then begin
      var LAbsoluteClipRect := LControlTmp.LocalToAbsolute(LControlTmp.ClipRect);
      LAbsoluteIntersectionRect.Intersect(LAbsoluteClipRect);
      if LAbsoluteIntersectionRect.IsEmpty then
        Exit(TRectF.Empty);
    end;
    LControlTmp := LControlTmp.ParentControl;
  end;
  Result := TRectF.Intersect(Form.ClientRect, LAbsoluteIntersectionRect)
end;

{************************************************}
function TALControl.FillTextFlags: TFillTextFlags;
begin
  if (Root = nil) then result := ALGetFillTextFlags
  else result := inherited;
end;

{***********************************************}
procedure TALControl.SetNewScene(AScene: IScene);
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.Pressed is still changed only in SetNewScene/DoMouseLeave/MouseDown/MouseUp/MouseClick'}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.IsFocused is still changed only in SetNewScene/DoEnter/DoExit'}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.IsMouseOver is still changed only in SetNewScene/DoMouseEnter/DoMouseLeave'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  var LPrevIsFocused := IsFocused;
  var LPrevIsMouseOver := IsMouseOver;
  inherited;
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
  if (PropagateMouseEvents) and (ALParentControl <> nil) then
    ALParentControl.ChildrenMouseEnter(Self);
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
  if (PropagateMouseEvents) and (ALParentControl <> nil) then
    ALParentControl.ChildrenMouseLeave(Self);
end;

{*************************************************************************************}
procedure TALControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  //--
  FControlAbsolutePosAtMouseDown := LocalToAbsolute(TPointF.Zero);
  FMouseDownAtRest := not IsInMotion;
  //--
  FDoubleClick := ssDouble in Shift;
  if FDoubleClick then begin
    {$IF defined(IOS)}
    if FForm <> nil then
      TALFMXViewBaseAccessPrivate(WindowHandleToPlatform(FForm.Handle).Handle).FShouldIgnoreNextClick := False;
    {$ENDIF}
    Shift := Shift - [ssDouble];
  end;
  //--
  if (not FFocusOnMouseDown) or (FFocusOnMouseUp) or (not FMouseDownAtRest) then begin
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
  //--
  if (PropagateMouseEvents) and (ALParentControl <> nil) then
    ALParentControl.ChildrenMouseDown(Self, Button, Shift, X, Y);
end;

{***************************************************************}
procedure TALControl.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  Inherited;
  if (PropagateMouseEvents) and (ALParentControl <> nil) then
    ALParentControl.ChildrenMouseMove(Self, Shift, X, Y);
end;

{***********************************************************************************}
procedure TALControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  inherited;
  if LPrevPressed <> Pressed then PressedChanged;
  FDoubleClick := False;
  var LControlAbsolutePos := LocalToAbsolute(TPointF.Zero);
  if (FFocusOnMouseUp) and
     (FMouseDownAtRest) and
     (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) <= TALScrollEngine.DefaultTouchSlop) and
     (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) <= TALScrollEngine.DefaultTouchSlop) and
     (not (csDesigning in ComponentState)) and
     (not FIsFocused) then
    SetFocus;
  if (PropagateMouseEvents) and (ALParentControl <> nil) then
    ALParentControl.ChildrenMouseUp(Self, Button, Shift, X, Y);
end;

{**************************************************************************************}
procedure TALControl.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  var LControlAbsolutePos := LocalToAbsolute(TPointF.Zero);
  if (not FMouseDownAtRest) or
     (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) > TALScrollEngine.DefaultTouchSlop) or
     (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) > TALScrollEngine.DefaultTouchSlop) then begin
    {$IF defined(debug)}
    if (not FMouseDownAtRest) then
      ALLog(Classname+'.MouseClick', 'Skipped | Mouse Down was not made at Low Velocity')
    else if (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) > TALScrollEngine.DefaultTouchSlop) then
      ALLog(Classname+'.MouseClick', 'Skipped | Control moved by '+ALFormatFloatW('0.##', abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x)) + ' horizontally')
    else if (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) > TALScrollEngine.DefaultTouchSlop) then
      ALLog(Classname+'.MouseClick', 'Skipped | Control moved by '+ALFormatFloatW('0.##', abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y)) + ' vertically')
    else
      raise Exception.Create('Error 79BF6F83-8725-476D-A283-507BE9CC671C');
    {$ENDIF}
    exit;
  end;
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  inherited;
  if LPrevPressed <> Pressed then PressedChanged;
end;

{********************************}
procedure TALControl.DoClickSound;
begin
  if (ClickSound=TALClickSoundMode.Always) or
     ((assigned(OnClick)) and
      (ClickSound=TALClickSoundMode.Default) and
      (ALGlobalClickSoundEnabled)) then
    ALPlayClickSound;
end;

{*************************}
procedure TALControl.Click;
begin
  DoClickSound;
  inherited;
  if FDoubleClick then begin
    DblClick;
    FDoubleClick := False;
  end;
end;

{**************************************}
function TALControl.IsInMotion: Boolean;
begin
  If (ALParentControl <> nil) then begin
    var LScrollableControl: IALScrollableControl;
    if (Supports(ALParentControl, IALScrollableControl, LScrollableControl)) and
       (not LScrollableControl.GetScrollEngine.IsVelocityLow) then result := True
    else result := ALParentControl.IsInMotion;
  end
  else result := False;
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

{**********************************************************************************************************************}
procedure TALControl.ChildrenMouseDown(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if (PropagateMouseEvents) and (ALParentControl <> nil) then
    ALParentControl.ChildrenMouseDown(AObject, Button, Shift, X, Y);
end;

{************************************************************************************************}
procedure TALControl.ChildrenMouseMove(const AObject: TControl; Shift: TShiftState; X, Y: Single);
begin
  if (PropagateMouseEvents) and (ALParentControl <> nil) then
    ALParentControl.ChildrenMouseMove(AObject, Shift, X, Y);
end;

{********************************************************************************************************************}
procedure TALControl.ChildrenMouseUp(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if (PropagateMouseEvents) and (ALParentControl <> nil) then
    ALParentControl.ChildrenMouseUp(AObject, Button, Shift, X, Y);
end;

{***************************************************************}
procedure TALControl.ChildrenMouseEnter(const AObject: TControl);
begin
  if (PropagateMouseEvents) and (ALParentControl <> nil) then
    ALParentControl.ChildrenMouseEnter(AObject);
end;

{***************************************************************}
procedure TALControl.ChildrenMouseLeave(const AObject: TControl);
begin
  if (PropagateMouseEvents) and (ALParentControl <> nil) then
    ALParentControl.ChildrenMouseLeave(AObject);
end;

{****************************************************}
procedure TALControl.DoMatrixChanged(Sender: TObject);
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.DoMatrixChanged was not updated and adjust the IFDEF'}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2823 was not corrected and adjust the IFDEF'}
  {$ENDIF}
  if not FInPaintTo and not IsUpdating then
    Repaint;
  if SameValue(Scale.X, 1.0, TEpsilon.Scale) and SameValue(Scale.Y, 1.0, TEpsilon.Scale) and SameValue(RotationAngle, 0.0, TEpsilon.Scale) then
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
        TMatrix.CreateScaling(Scale.X, Scale.Y) *
        TMatrix.CreateRotation(DegToRad(RotationAngle)) *
        TMatrix.CreateTranslation(Pivot.X * FSize.Width + Position.X, Pivot.Y * FSize.Height + Position.Y);
    end
    else
    begin
      FLocalMatrix := TMatrix.Identity;
      FLocalMatrix.m31 := Position.X + ((1 - Scale.X) * Pivot.X * FSize.Width);
      FLocalMatrix.m32 := Position.Y + ((1 - Scale.Y) * Pivot.Y * FSize.Height);
      FLocalMatrix.m11 := Scale.X;
      FLocalMatrix.m22 := Scale.Y;
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
  else begin
    FForm := Nil;
    {$IF defined(ALDPK)}
    // At design time, the root is not a TCommonCustomForm,
    // but an opaque TFmxDesignSurface instance.
    // I happen to know (don’t ask how) that TFmxDesignSurface has
    // a private field named:
    //   FForm: FMX.Forms.TCommonCustomForm;
    if Root is TObject then begin
      var LObj := TObject(Root);
      var LContext: TRttiContext;
      var LType: TRttiType := LContext.GetType(LObj.ClassType);
      var LField: TRttiField := LType.GetField('FForm');
      if Assigned(LField) then
        FForm := TCommonCustomForm(LField.GetValue(LObj).AsObject);
    end;
    {$ENDIF}
  end;
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
  // the value of FALParentControl will be incorrect.
  if (ParentControl <> nil) and (ParentControl is TALControl) then
    FALParentControl := TALControl(ParentControl)
  else
    FALParentControl := nil;
end;

{**********************************************************}
procedure TALControl.MarginsChangedHandler(Sender: TObject);
begin
  if Assigned(FFormerMarginsChangedHandler) then
    FFormerMarginsChangedHandler(Sender);
  MarginsChanged;
end;

{********************************************************}
procedure TALControl.ScaleChangedHandler(Sender: TObject);
begin
  DoMatrixChanged(Sender);
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported123}
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
      Result := not TabStop.GetObject.Equals(_TStyledControlProtectedAccess(Parent).ResourceLink);
  end;

  Result := Result and inherited IsAddable(TabStop);
end;

{************************************************}
constructor TALContent.Create(AOwner: TComponent);
begin
  inherited;
  SetAcceptsControls(False);
end;

{**********************************************************}
procedure TALContent.DoAddObject(const AObject: TFmxObject);
begin
  inherited;
  ContentChanged;
end;

{*************************************************************}
procedure TALContent.DoRemoveObject(const AObject: TFmxObject);
begin
  inherited;
  ContentChanged;
end;

{************************************}
procedure TALContent.DoDeleteChildren;
begin
  inherited;
  ContentChanged;
end;

{*****************************}
procedure TALContent.DoRealign;
//var
//  AlignRoot: IAlignRoot;
begin
//  if (Parent <> nil) and not(csLoading in Parent.ComponentState) then
//    inherited;
//  if (Parent <> nil) and not FParentAligning and not(csLoading in ComponentState) then
//  begin
//    FParentAligning := True;
//    if ParentControl <> nil then
//      _TControlProtectedAccess(ParentControl).Realign
//    else
//      if not(csLoading in ComponentState) and Supports(Parent, IAlignRoot, AlignRoot) then
//        AlignRoot.Realign;
//    FParentAligning := False;
//  end;

  // There is nothing wrong with the previous implementation.
  // This code is taken from TContent. We must call ContentChanged every time we realign,
  // because if one of its child controls changes its position or size during realignment,
  // ContentChanged would normally be triggered. However, since ContentChanged is deactivated
  // (i.e., FDisableAlign is checked), DoContentChanged will not be called,
  // and thus the parent of the content will not be notified.

  inherited;
  ContentChanged;

end;

{************************************}
procedure TALContent.DoContentChanged;
begin
  // Virtual
end;

{**********************************}
procedure TALContent.ContentChanged;
begin

  // ContentChanged is called by the TControl.ParentContentChanged function,
  // which in turn is invoked by:
  //
  //  * TControl.DoMatrixChanged (i.e., when the position of a child control changes)
  //  * TControl.SetAlign (i.e., when the alignment property of a child control changes)
  //  * TControl.SetVisible (i.e., when the visibility of a child control changes)
  //  * TControl.InternalSizeChanged (i.e., when the size of a child control changes)
  //
  // Monitoring this event is important so that we can update the size of TALContent
  // to best fit its content—this is especially crucial for components such as TALScrollBox.

  {$IF defined(debug)}
  //ALLog(ClassName+'.ContentChanged', 'Name: ' + Name);
  {$ENDIF}

  // * We do not call DoContentChanged if IsUpdating because calling EndUpdate triggers a
  //   realignment (DoEndUpdate > Realign > DoRealign) that invokes ContentChanged again.
  // * Likewise, we ignore FDisableAlign since ContentChanged will be called again
  //   (in DoRealign) once the alignment process is complete.
  if (not IsUpdating) and (not FDisableAlign) and (not (csDestroying in ComponentState)) then
    DoContentChanged;

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

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.Controls','initialization');
  {$ENDIF}
  ALGlobalClickSoundEnabled := False;

end.