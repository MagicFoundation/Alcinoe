{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Controls3D;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.Types, System.UITypes, System.UIConsts,
  System.Generics.Defaults, System.Messaging, System.Math.Vectors, FMX.Types, FMX.Types3D, FMX.Graphics, FMX.Ani;

type
  TCamera = class;
  TLight = class;
  TControl3D = class;

  IViewport3D = interface
    ['{F819CBB6-B3CD-47ea-B4BA-6ED76E668CA9}']
    function GetObject: TFmxObject;
    function GetContext: TContext3D;
    function GetUsingDesignCamera: Boolean;
    function GetCamera: TCamera;
    function GetViewportScale: Single;
    function GetLightCount: Integer;
    function GetLight(Index: Integer): TLight;
    function GetCurrentCamera: TCamera;
    procedure SetCamera(const ACamera: TCamera);
    procedure AddLight(const ALight: TLight);
    procedure RemoveLight(const ALight: TLight);
    function ScreenToLocal(P: TPointF): TPointF;
    function LocalToScreen(P: TPointF): TPointF;
    procedure NeedRender;
    { access }
    property LightCount: Integer read GetLightCount;
    property Lights[Index: Integer]: TLight read GetLight;
    property CurrentCamera: TCamera read GetCurrentCamera;
    property Camera: TCamera read GetCamera write SetCamera;
    property Context: TContext3D read GetContext;
    property UsingDesignCamera: Boolean read GetUsingDesignCamera;
  end;

{ TControl3D }

  TMouseEvent3D = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single;
    RayPos, RayDir: TVector3D) of object;
  TMouseMoveEvent3D = procedure(Sender: TObject; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D) of object;

  TRenderEvent = procedure(Sender: TObject; Context: TContext3D) of object;

  TDragEnterEvent3D = procedure(Sender: TObject; const Data: TDragObject; const Point: TPoint3D) of object;
  TDragOverEvent3D = procedure(Sender: TObject; const Data: TDragObject; const Point: TPoint3D;
    var Operation: TDragOperation) of object;
  TDragDropEvent3D = procedure(Sender: TObject; const Data: TDragObject; const Point: TPoint3D) of object;

  TTapEvent3D = procedure(Sender: TObject; const Point: TPointF; const RayPos, RayDir: TVector3D) of object;

  TObjectAtPointData = record
    Distance: single;
    Projection: TProjection;
  end;

  TControl3D = class(TFmxObject, IControl, ITabStopController)
  private
    FVisible: Boolean;
    FRenderingList: TList<TControl3D>;
    FOnMouseUp: TMouseEvent3D;
    FOnMouseDown: TMouseEvent3D;
    FOnMouseMove: TMouseMoveEvent3D;
    FOnMouseWheel: TMouseWheelEvent;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnTap: TTapEvent3D;
    FMouseInObject: Boolean;
    FHitTest: Boolean;
    FAutoCapture: Boolean;
    FLocked: Boolean;
    FTempContext: TContext3D;
    FCanFocus: Boolean;
    FIsMouseOver: Boolean;
    FIsFocused: Boolean;
    FRotationCenter: TPosition3D;
    FOnKeyUp: TKeyEvent;
    FOnKeyDown: TKeyEvent;
    FOnRender: TRenderEvent;
    FTwoSide: Boolean;
    FDragMode: TDragMode;
    FDisableDragHighlight: Boolean;
    FOnDragEnter: TDragEnterEvent3D;
    FOnDragDrop: TDragDropEvent3D;
    FOnDragEnd: TNotifyEvent;
    FOnDragLeave: TNotifyEvent;
    FOnDragOver: TDragOverEvent3D;
    FIsDragOver: Boolean;
    FShowHint: Boolean;
    FHint: string;
    FPressed, FDoubleClick: Boolean;
    FCursor: TCursor;
    FInheritedCursor: TCursor;
    FVisibleContextMenu: Boolean;
    FTabList: TTabList;
    FAcceptsControls: boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FCanParentFocus: Boolean;
    FOnDeactivate: TNotifyEvent;
    FOnActivate: TNotifyEvent;
    FRecalcHasEffect: Boolean;
    FAbsoluteHasEffect: Boolean;
    FAbsoluteHasDisablePaintEffect: Boolean;
    FAbsoluteHasAfterPaintEffect: Boolean;
    FOpaque: Boolean;
    FTabStop: Boolean;
    function GetInvertAbsoluteMatrix: TMatrix3D;
    procedure SetHitTest(const Value: Boolean);
    function GetContext: TContext3D; inline;
    procedure SetLocked(const Value: Boolean);
    procedure SetTempContext(const Value: TContext3D);
    procedure SetOpacity(const Value: Single);
    function IsOpacityStored: Boolean;
    function GetEnabled: Boolean;
    function GetAbsolutePosition: TVector3D;
    function GetAbsoluteUp: TVector3D;
    function GetAbsoluteDirection: TVector3D;
    function GetAbsoluteLeft: TVector3D;
    function GetAbsoluteMatrix: TMatrix3D; virtual;
    function GetAbsoluteOpacity: Single; virtual;
    function GetPopupMenu: TCustomPopupMenu;
    function GetScreenBounds: TRectF;
    /// <summary>Returns the bounding box of the control in global space.</summary>
    function GetGlobalBounds: TBoundingBox;
    /// <summary>Returns the bounding box of the control in local space.</summary>
    function GetLocalBounds: TBoundingBox;
    procedure SetAbsolutePosition(const Value: TVector3D);
    procedure ReadQuaternion(Reader: TReader);
    procedure WriteQuaternion(Writer: TWriter);
    procedure SetZWrite(const Value: Boolean);
    procedure SetTabOrder(const Value: TTabOrder);
    function GetTabOrder: TTabOrder;
    function GetTabStop: Boolean;
    procedure SetTabStop(const TabStop: Boolean);
    function GetCursor: TCursor;
    procedure SetCursor(const Value: TCursor);
    procedure RefreshInheritedCursor;
    procedure RefreshInheritedCursorForChildren;
    function GetInheritedCursor: TCursor;
    function GetIsFocused: Boolean;
    function GetAbsoluteHasEffect: Boolean;
    procedure UpdateEffects;
    function GetAbsoluteHasAfterPaintEffect: Boolean;
    function GetAbsoluteHasDisablePaintEffect: Boolean;
    function GetAbsoluteEnabled: Boolean;
    procedure SetOpaque(const Value: Boolean);
    procedure ReadShowContextMenu(Reader: TReader);
    procedure ReadDesignVisible(Reader: TReader);
  protected
    FPosition: TPosition3D;
    FQuaternion: TQuaternion3D;
    FRotationAngle: TPosition3D;
    FSavedRotationAngle: TPoint3D;
    FScale: TPosition3D;
    FSkew: TPosition3D;
    FZWrite: Boolean;
    FProjection: TProjection;
    FViewport: IViewport3D;
    FHeight, FLastHeight: Single;
    FWidth, FLastWidth: Single;
    FDepth, FLastDepth: Single;
    FLocalMatrix: TMatrix3D;
    FAbsoluteMatrix: TMatrix3D;
    FInvAbsoluteMatrix: TMatrix3D;
    FRecalcAbsolute: Boolean;
    FCanResize, FCanRotate: Boolean;
    FBody: Integer;
    FDesignInteract: Boolean;
    FDesignLocked: Boolean;
    FOpacity, FAbsoluteOpacity: Single;
    FRecalcOpacity: Boolean;
    FUpdating: Integer;
    FDisableEffect: Boolean;
    FEffectTexture: TTexture;
    FEffectContext: TContext3D;
    FInRenderTo: Boolean;
    FUpdateEffects: Boolean;
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    function CheckHitTest(const AHitTest: Boolean): Boolean;
    { IFreeNotification }
    procedure FreeNotification(AObject: TObject); override;
    { props }
    procedure SetVisible(const Value: Boolean); virtual;
    procedure SetHeight(const Value: Single); virtual;
    procedure SetWidth(const Value: Single); virtual;
    procedure SetDepth(const Value: Single); virtual;
    procedure SetProjection(const Value: TProjection); virtual;
    procedure RecalcOpacity; virtual;
    procedure RecalcAbsolute; virtual;
    { events }
    procedure Capture;
    procedure ReleaseCapture;
    procedure MouseDown3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); virtual;
    procedure MouseMove3D(Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); virtual;
    procedure MouseUp3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); virtual;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); virtual;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); virtual;
    procedure KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); virtual;
    procedure DialogKey(var Key: Word; Shift: TShiftState); virtual;
    procedure AfterDialogKey(var Key: Word; Shift: TShiftState); virtual;
    procedure Click; virtual;
    procedure DblClick; virtual;
    procedure DragEnter(const Data: TDragObject; const Point: TPointF); virtual;
    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation); virtual;
    procedure DragDrop(const Data: TDragObject; const Point: TPointF); virtual;
    procedure DragLeave; virtual;
    procedure DragEnd; virtual;
    { ray casting }
    function DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean; virtual;
    { IControl }
    function GetCanFocus: boolean; virtual;
    function GetCanParentFocus: boolean; virtual;
    function EnterChildren(AObject: IControl): Boolean; virtual;
    function ExitChildren(AObject: IControl): Boolean; virtual;
    function ShowContextMenu(const ScreenPosition: TPointF): Boolean; virtual;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure DoEnter; virtual;
    procedure DoExit; virtual;
    procedure DoActivate; virtual;
    procedure DoDeactivate; virtual;
    function ObjectAtPoint(P: TPointF): IControl; virtual;
    function GetObject: TFmxObject;
    function GetVisible: Boolean;
    function GetDesignInteractive: Boolean;
    function AbsoluteToLocal(P: TPointF): TPointF;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseMove(Shift: TShiftState; X, Y: Single);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Tap(const Point: TPointF);
    function CheckForAllowFocus: Boolean;
    function ScreenToLocal(P: TPointF): TPointF;
    function LocalToScreen(P: TPointF): TPointF;
    procedure BeginAutoDrag;
    function GetDragMode: TDragMode;
    procedure SetDragMode(const ADragMode: TDragMode);
    function GetParent: TFmxObject;
    function GetLocked: Boolean;
    function GetHitTest: Boolean;
    function GetAcceptsControls: boolean;
    procedure SetAcceptsControls(const Value: boolean);
    function FindTarget(P: TPointF; const Data: TDragObject): IControl; virtual;
    function GetTabStopController: ITabStopController; virtual;
    /// <summary>Implementation of IControl.HasHint.</summary>
    function HasHint: Boolean; virtual;
    /// <summary>Implementation of IControl.GetHintString.</summary>
    function GetHintString: string; virtual;
    /// <summary>Implementation of IControl.GetHintObject.</summary>
    function GetHintObject: TObject; virtual;
    { rendering internal }
    procedure RecalcHasEffect;
    procedure RenderTo(const AContext: TContext3D; const Offset: TPointF);
    { rendering }
    procedure BeforeRender; virtual;
    procedure ApplyEffect; virtual;
    procedure Render; virtual;
    procedure RenderHelper; virtual;
    procedure RenderChildren; virtual;
    procedure DoRender;
    { children }
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
    procedure DoInsertObject(Index: Integer; const AObject: TFmxObject); override;
    { optimization }
    procedure RebuildRenderingList;
    function GetMaterialForSorting: TMaterial; virtual;
    { alignment }
    procedure Resize3D; virtual;
    { changes }
    procedure MatrixChanged(Sender: TObject); virtual;
    procedure RotationChanged(Sender: TObject); virtual;
    procedure RotateXChanged(Sender: TObject); virtual;
    procedure RotateYChanged(Sender: TObject); virtual;
    procedure RotateZChanged(Sender: TObject); virtual;
    procedure SetParent(const Value: TFmxObject); override;
    { props }
    property MouseInObject: Boolean read FMouseInObject write FMouseInObject;
    property Skew: TPosition3D read FSkew write FSkew;
    property TempContext: TContext3D read FTempContext write SetTempContext;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Calculate best fit size using AWidth and AHeight, create TBitmap object and render to this bitmap. }
    procedure PaintToBitmap(const ABitmap: TBitmap; AWidth, AHeight: Integer; ClearColor: TAlphaColor;
      AutoFit: Boolean = False; const AMultisample: TMultisample = TMultisample.None);
    { Create SuperSampled object's snapshot using tile-rendering and interpolation. Multisample can be 1..16 }
    procedure CreateHighMultisampleSnapshot(const ABitmap: TBitmap; AWidth, AHeight: Integer; ClearColor: TAlphaColor;
      Multisample: Integer);
    { Create tile-part of snaphot. Tiles can be merge. }
    procedure CreateTileSnapshot(const ABitmap: TBitmap; AWidth, AHeight, OffsetX, OffsetY: Integer;
      Scale: Single; ClearColor: TAlphaColor);
    procedure CopyRotationFrom(const AObject: TControl3D);
    function AbsoluteToLocal3D(const Point: TPoint3D): TPoint3D; virtual;
    function LocalToAbsolute3D(const Point: TPoint3D): TPoint3D; virtual;
    function AbsoluteToLocalVector(const Vector: TVector3D): TVector3D; virtual;
    function LocalToAbsoluteVector(const Vector: TVector3D): TVector3D; virtual;
    function AbsoluteToLocalDirection(const Vector: TVector3D): TVector3D;
    function LocalToAbsoluteDirection(const Vector: TVector3D): TVector3D;
    procedure SetSize(const AWidth, AHeight, ADepth: single);
    function RayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean; virtual;
    procedure ResetRotationAngle;
    procedure SetFocus;
    procedure Repaint;
    procedure Lock;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure RenderInternal;
    procedure SetNewViewport(AViewport: IViewport3D); virtual;
    { ITabStopController }
    function GetTabList: ITabList; virtual;
    function ShowInDesigner: Boolean;
    property AbsoluteMatrix: TMatrix3D read GetAbsoluteMatrix;
    property LocalMatrix: TMatrix3D read FLocalMatrix;
    property AbsolutePosition: TVector3D read GetAbsolutePosition write SetAbsolutePosition;
    property AbsoluteUp: TVector3D read GetAbsoluteUp;
    property AbsoluteDirection: TVector3D read GetAbsoluteDirection;
    property AbsoluteLeft: TVector3D read GetAbsoluteLeft;
    property AbsoluteOpacity: Single read GetAbsoluteOpacity;
    property InvertAbsoluteMatrix: TMatrix3D read GetInvertAbsoluteMatrix;
    property Context: TContext3D read GetContext;
    property DesignInteract: Boolean read FDesignInteract;
    property AutoCapture: Boolean read FAutoCapture write FAutoCapture default False;
    property RotationCenter: TPosition3D read FRotationCenter write FRotationCenter;
    property ScreenBounds: TRectF read GetScreenBounds;
    /// <summary>Bounding box of the control in global space.</summary>
    property GlobalBounds: TBoundingBox read GetGlobalBounds;
    /// <summary>Bounding box of the control in local space.</summary>
    property LocalBounds: TBoundingBox read GetLocalBounds;
    property CanFocus: Boolean read FCanFocus write FCanFocus default False;
    property CanParentFocus: Boolean read FCanParentFocus write FCanParentFocus default False;
    property TabOrder: TTabOrder read GetTabOrder write SetTabOrder;
    property DesignLocked: Boolean read FDesignLocked write FDesignLocked;
    property DisableDragHighlight: Boolean read FDisableDragHighlight write FDisableDragHighlight default False;
    property Hint: string read FHint write FHint;
    property ShowHint: Boolean read FShowHint write FShowHint default False;
    property IsDragOver: Boolean read FIsDragOver;
    property IsMouseOver: Boolean read FIsMouseOver;
    property IsFocused: Boolean read GetIsFocused;
    property IsVisible: Boolean read FVisible;
    property Cursor: TCursor read GetCursor write SetCursor default crDefault;
    property InheritedCursor: TCursor read GetInheritedCursor default crDefault;
    property DragMode: TDragMode read GetDragMode write SetDragMode default TDragMode.dmManual;
    property HasEffect: Boolean read GetAbsoluteHasEffect;
    property HasDisablePaintEffect: Boolean read GetAbsoluteHasDisablePaintEffect;
    property HasAfterPaintEffect: Boolean read GetAbsoluteHasAfterPaintEffect;
    property Opaque: Boolean read FOpaque write SetOpaque;
    property Position: TPosition3D read FPosition write FPosition;
    property Scale: TPosition3D read FScale write FScale;
    property RotationAngle: TPosition3D read FRotationAngle write FRotationAngle;
    property Locked: Boolean read FLocked write SetLocked default False;
    property Width: Single read FWidth write SetWidth;
    property Height: Single read FHeight write SetHeight;
    property Depth: Single read FDepth write SetDepth;
    property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
    property Projection: TProjection read FProjection write SetProjection default TProjection.Camera;
    property Viewport: IViewport3D read FViewport;
    property HitTest: Boolean read FHitTest write SetHitTest default True;
    property VisibleContextMenu: Boolean read FVisibleContextMenu write FVisibleContextMenu default True;
    property TwoSide: Boolean read FTwoSide write FTwoSide default False;
    property Visible: Boolean read FVisible write SetVisible default True;
    property ZWrite: Boolean read FZWrite write SetZWrite default True;
    property OnDragEnter: TDragEnterEvent3D read FOnDragEnter write FOnDragEnter;
    property OnDragLeave: TNotifyEvent read FOnDragLeave write FOnDragLeave;
    property OnDragOver: TDragOverEvent3D read FOnDragOver write FOnDragOver;
    property OnDragDrop: TDragDropEvent3D read FOnDragDrop write FOnDragDrop;
    property OnDragEnd: TNotifyEvent read FOnDragEnd write FOnDragEnd;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnMouseDown: TMouseEvent3D read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent3D read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent3D read FOnMouseUp write FOnMouseUp;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnRender: TRenderEvent read FOnRender write FOnRender;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnTap: TTapEvent3D read FOnTap write FOnTap;
  end;

{ TCamera }

  TCamera = class(TControl3D)
  private
    FTarget: TControl3D;
    FAngleOfView: Single;
    procedure SetTarget(const Value: TControl3D);
    function GetCameraMatrix: TMatrix3D;
    procedure SetAngleOfView(const Value: Single);
  protected
    procedure RecalcAbsolute; override;
    procedure Render; override;
    function DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property CameraMatrix: TMatrix3D read GetCameraMatrix;
  published
    property AngleOfView: Single read FAngleOfView write SetAngleOfView;
    property HitTest default False;
    property Target: TControl3D read FTarget write SetTarget;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Height;
    property Depth;
    property Opacity nodefault;
    property Projection;
    property VisibleContextMenu default True;
    property TwoSide default False;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
  end;

{ TLight }

  TLight = class(TControl3D)
  private
    FEnabled: Boolean;
    FLightType: TLightType;
    FSpotCutOff: Single;
    FSpotExponent: Single;
    FColor: TAlphaColor;
    procedure SetLightType(const Value: TLightType);
    procedure SetEnabled(const Value: Boolean);
    procedure SetSpotCutOff(const Value: Single);
    procedure SetSpotExponent(const Value: Single);
    procedure SetColor(const Value: TAlphaColor);
    procedure ReadDiffuse(Reader: TReader);
    procedure SkipColor(Reader: TReader);
    procedure SkipFloat(Reader: TReader);
    function GetLightDescription: TLightDescription;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Render; override;
    procedure SetHeight(const Value: Single); override;
    procedure SetWidth(const Value: Single); override;
    procedure SetDepth(const Value: Single); override;
    function DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetNewViewport(AViewport: IViewport3D); override;
    property LightDescription: TLightDescription read GetLightDescription;
  published
    property HitTest default False;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Color: TAlphaColor read FColor write SetColor;
    property LightType: TLightType read FLightType write SetLightType;
    property SpotCutOff: Single read FSpotCutOff write SetSpotCutOff;
    property SpotExponent: Single read FSpotExponent write SetSpotExponent;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Height;
    property Depth;
    property Opacity nodefault;
    property Projection;
    property VisibleContextMenu default True;
    property Visible default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
  end;

{ TProxyObject }

  TProxyObject = class(TControl3D)
  private
    FSourceObject: TControl3D;
    FDisableRendering: Boolean;
    procedure SetSourceObject(const Value: TControl3D);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Render; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property SourceObject: TControl3D read FSourceObject write SetSourceObject;
     property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Position;
    property Scale;
    property RotationAngle;
    property Locked default False;
    property Width;
    property Height;
    property Depth;
    property Opacity nodefault;
    property Projection;
    property HitTest default True;
    property VisibleContextMenu default True;
    property TwoSide default False;
    property Visible default True;
    property ZWrite default True;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnRender;
  end;

{ Used for correct Pick }

var
  GlobalDistance: Single = 0;
  GlobalProjection: TProjection;


type
  TRenderingCompare = class(TComparer<TControl3D>)
  public
    function Compare(const Left, Right: TControl3D): Integer; override;
  end;

implementation

uses
  System.Math,  System.TypInfo, System.RTLConsts, FMX.Consts, FMX.Platform, FMX.Materials, FMX.Forms, FMX.Effects,
  FMX.Utils;

type
  TOpenObject = class(TFmxObject);

{ TControl3D }

constructor TControl3D.Create(AOwner: TComponent);
begin
  inherited;
  FCursor := crDefault;
  FVisibleContextMenu := True;
  FCanResize := True;
  FCanRotate := True;
  FOpacity := 1;
  FZWrite := True;
  FLocalMatrix := TMatrix3D.Identity;
  FQuaternion := TQuaternion3D.Identity;
  FPosition := TPosition3D.Create(Point3D(0, 0, 0));
  FPosition.OnChange := MatrixChanged;
  FScale := TPosition3D.Create(Point3D(1, 1, 1));
  FScale.OnChange := MatrixChanged;
  FSkew := TPosition3D.Create(Point3D(0, 0, 0));
  FSkew.OnChange := MatrixChanged;
  FRotationAngle := TPosition3D.Create(Point3D(0, 0, 0));
  FRotationAngle.OnChange := RotationChanged;
  FRotationAngle.OnChangeX := RotateXChanged;
  FRotationAngle.OnChangeY := RotateYChanged;
  FRotationAngle.OnChangeZ := RotateZChanged;
  FSavedRotationAngle := Point3D(0, 0, 0);
  FRotationCenter := TPosition3D.Create(Point3D(0, 0, 0));
  FRotationCenter.OnChange := MatrixChanged;
  FWidth := 1;
  FLastWidth := FWidth;
  FHeight := 1;
  FLastHeight := FHeight;
  FDepth := 1;
  FLastDepth := FDepth;
  FVisible := True;
  FHitTest := True;
  FUpdateEffects := True;
  FRecalcAbsolute := True;
  FRecalcOpacity := True;
  FAcceptsControls := True;
  FRecalcHasEffect := True;
end;

function TControl3D.ShowInDesigner: Boolean;
begin
  Result := (csDesigning in ComponentState) and FVisible;
end;

destructor TControl3D.Destroy;
var
  Controller: ITabStopController;
begin
  Controller := GetTabStopController;
  if Controller <> nil then
    Controller.TabList.Remove(Self);
  FAbsoluteOpacity := 0;
  FVisible := False;
  FreeAndNil(FEffectContext);
  FreeAndNil(FEffectTexture);
  FreeAndNil(FRotationCenter);
  FreeAndNil(FRotationAngle);
  FreeAndNil(FScale);
  FreeAndNil(FSkew);
  FreeAndNil(FPosition);
  FreeAndNil(FRenderingList);
  inherited;
end;

procedure TControl3D.Loaded;
begin
  inherited;
  MatrixChanged(Self);
end;

procedure TControl3D.DoAddObject(const AObject: TFmxObject);
begin
  inherited;
  if AObject = nil then
    Exit;
  if AObject is TControl3D then
  begin
    TControl3D(AObject).FUpdating := FUpdating;
    TControl3D(AObject).SetNewViewport(FViewport);
    if TempContext <> nil then
      TControl3D(AObject).TempContext := TempContext;
    TControl3D(AObject).RecalcOpacity;
    TControl3D(AObject).RecalcAbsolute;
    RebuildRenderingList;
  end;
  if (AObject is TEffect) and (TEffect(AObject).Enabled) then
  begin
    RecalcHasEffect;
    if not (csLoading in ComponentState) then
      Repaint;
  end;
  RefreshInheritedCursorForChildren;
end;

procedure TControl3D.DoRemoveObject(const AObject: TFmxObject);
begin
  inherited;
  if (AObject is TControl3D) then
  begin
    TControl3D(AObject).FUpdating := 0;
    TControl3D(AObject).Repaint;
    TControl3D(AObject).SetNewViewport(nil);
    RebuildRenderingList;
  end;
  if AObject is TEffect then
    RecalcHasEffect;
  RefreshInheritedCursorForChildren;
end;

procedure TControl3D.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('ShowContextMenu', ReadShowContextMenu, nil, False);
  Filer.DefineProperty('Quanternion', ReadQuaternion, WriteQuaternion,
    (FQuaternion.ImagPart.X <> TQuaternion3D.Identity.ImagPart.X) or
    (FQuaternion.ImagPart.Y <> TQuaternion3D.Identity.ImagPart.Y) or
    (FQuaternion.ImagPart.Z <> TQuaternion3D.Identity.ImagPart.Z) or
    (FQuaternion.RealPart <> TQuaternion3D.Identity.RealPart));
  Filer.DefineProperty('DesignVisible', ReadDesignVisible, nil, False);
end;

procedure TControl3D.ReadShowContextMenu(Reader: TReader);
var
  B: Boolean;
begin
  B := Reader.ReadBoolean;
  VisibleContextMenu := B;
end;

procedure TControl3D.ReadDesignVisible(Reader: TReader);
begin
  Reader.ReadBoolean;
end;

procedure TControl3D.ReadQuaternion(Reader: TReader);
var
  S: string;
begin
  S := Reader.ReadString;
  try
    GetToken(S, ',()');
    FQuaternion.ImagPart.X := StrToFloat(GetToken(S, ',()'), USFormatSettings);
    FQuaternion.ImagPart.Y := StrToFloat(GetToken(S, ',()'), USFormatSettings);
    FQuaternion.ImagPart.Z := StrToFloat(GetToken(S, ',()'), USFormatSettings);
    FQuaternion.RealPart := StrToFloat(GetToken(S, ',()'), USFormatSettings);
  except
  end;
end;

procedure TControl3D.WriteQuaternion(Writer: TWriter);
var
  S: string;
begin
  S := '(' + FloatToStr(FQuaternion.ImagPart.X, USFormatSettings) + ',' + FloatToStr(FQuaternion.ImagPart.Y,
    USFormatSettings) + ',' + FloatToStr(FQuaternion.ImagPart.Z, USFormatSettings) + ',' +
    FloatToStr(FQuaternion.RealPart, USFormatSettings) + ')';
  Writer.WriteString(S);
end;

{ matrix }

procedure TControl3D.RotationChanged(Sender: TObject);
var
  a: Single;
  NeedChange: Boolean;
  NewValue: TPoint3D;
begin
  NeedChange := False;
  NewValue := RotationAngle.Point;
  a := DegNormalize(RotationAngle.X - FSavedRotationAngle.X);
  if a <> 0 then
  begin
    FQuaternion := FQuaternion * TQuaternion3D.Create(Point3D(1, 0, 0) { AbsoluteRight }, DegToRad(a));
    NeedChange := True;
    NewValue.X := DegNormalize(RotationAngle.X);
  end;
  a := DegNormalize(RotationAngle.Y - FSavedRotationAngle.Y);
  if a <> 0 then
  begin
    FQuaternion := FQuaternion * TQuaternion3D.Create(Point3D(0, 1, 0) { AbsoluteDirection }, DegToRad(a));
    NeedChange := True;
    NewValue.Y := DegNormalize(RotationAngle.Y);
  end;
  a := DegNormalize(RotationAngle.Z - FSavedRotationAngle.Z);
  if a <> 0 then
  begin
    FQuaternion := FQuaternion * TQuaternion3D.Create(Point3D(0, 0, 1) { AbsoluteUp }, DegToRad(a));
    NeedChange := True;
    NewValue.Z := DegNormalize(RotationAngle.Z);
  end;
  if NeedChange then
  begin
    FSavedRotationAngle := RotationAngle.Point;
    RotationAngle.SetPoint3DNoChange(NewValue);
    MatrixChanged(Sender);
  end;
end;

procedure TControl3D.RotateXChanged(Sender: TObject);
var
  a: Single;
begin
  a := DegNormalize(RotationAngle.X - FSavedRotationAngle.X);
  if a <> 0 then
  begin
    FQuaternion := FQuaternion * TQuaternion3D.Create(Point3D(1, 0, 0) { AbsoluteRight }, DegToRad(a));
    MatrixChanged(Sender);
    FSavedRotationAngle.X := RotationAngle.X;
    RotationAngle.SetPoint3DNoChange(Point3D(DegNormalize(RotationAngle.X), RotationAngle.Y, RotationAngle.Z));
  end;
end;

procedure TControl3D.RotateYChanged(Sender: TObject);
var
  a: Single;
begin
  a := DegNormalize(RotationAngle.Y - FSavedRotationAngle.Y);
  if a <> 0 then
  begin
    FQuaternion := FQuaternion * TQuaternion3D.Create(Point3D(0, 1, 0) { AbsoluteDirection }, DegToRad(a));
    MatrixChanged(Sender);
    FSavedRotationAngle.Y := RotationAngle.Y;
    RotationAngle.SetPoint3DNoChange(Point3D(RotationAngle.X, DegNormalize(RotationAngle.Y), RotationAngle.Z));
  end;
end;

procedure TControl3D.RotateZChanged(Sender: TObject);
var
  a: Single;
begin
  a := DegNormalize(RotationAngle.Z - FSavedRotationAngle.Z);
  if a <> 0 then
  begin
    FQuaternion := FQuaternion * TQuaternion3D.Create(Point3D(0, 0, 1) { AbsoluteUp }, DegToRad(a));
    MatrixChanged(Sender);
    FSavedRotationAngle.Z := RotationAngle.Z;
    RotationAngle.SetPoint3DNoChange(Point3D(RotationAngle.X, RotationAngle.Y, DegNormalize(RotationAngle.Z)));
  end;
end;

procedure TControl3D.ResetRotationAngle;
begin
  FQuaternion := TQuaternion3D.Identity;
  MatrixChanged(Self);
  RotationAngle.SetPoint3DNoChange(Point3D(0, 0, 0));
  FSavedRotationAngle := Point3D(0, 0, 0)
end;

procedure TControl3D.MatrixChanged(Sender: TObject);
var
  AuxiliarMatrix: TMatrix3D;
  RotMatrix: TMatrix3D;
begin
  FLocalMatrix := TMatrix3D.Identity;

  if not(SameValue(FScale.X, 1, TEpsilon.Scale) and SameValue(FScale.Y, 1, TEpsilon.Scale) and
    SameValue(FScale.Z, 1, TEpsilon.Scale)) then
    FLocalMatrix := FLocalMatrix * TMatrix3D.CreateScaling(FScale.Point);

  if not(SameValue(FRotationAngle.X, 0, TEpsilon.Vector) and SameValue(FRotationAngle.Y, 0, TEpsilon.Vector) and
    SameValue(FRotationAngle.Z, 0, TEpsilon.Vector)) then
  begin
    if not(SameValue(FRotationCenter.X, 0, TEpsilon.Position) and SameValue(FRotationCenter.Y, 0, TEpsilon.Position) and
      SameValue(FRotationCenter.Z, 0, TEpsilon.Position)) then
    begin
      AuxiliarMatrix := FQuaternion;
      RotMatrix := TMatrix3D.CreateTranslation(-FRotationCenter.Point) * AuxiliarMatrix *
        TMatrix3D.CreateTranslation(FRotationCenter.Point);
    end
    else
      RotMatrix := FQuaternion;
    FLocalMatrix := FLocalMatrix * RotMatrix;
  end
  else
  begin
    FSavedRotationAngle := TPoint3D.Zero;
    FQuaternion := TQuaternion3D.Identity;
  end;

  if not(SameValue(FPosition.X, 0, TEpsilon.Position) and SameValue(FPosition.Y, 0, TEpsilon.Position) and
    SameValue(FPosition.Z, 0, TEpsilon.Position)) then
  begin
    FLocalMatrix := FLocalMatrix * TMatrix3D.CreateTranslation(FPosition.Point);
  end;

  RecalcAbsolute;
  RebuildRenderingList;
  Repaint;
end;

procedure TControl3D.Resize3D;
begin
end;

function TControl3D.GetAbsoluteMatrix: TMatrix3D;
begin
  if FRecalcAbsolute then
  begin
    if FParent is TControl3D then
      FAbsoluteMatrix := FLocalMatrix * TControl3D(FParent).AbsoluteMatrix
    else
      FAbsoluteMatrix := FLocalMatrix;

    Result := FAbsoluteMatrix;
    FInvAbsoluteMatrix := FAbsoluteMatrix.Inverse;

    FRecalcAbsolute := False;
  end
  else
  begin
    Result := FAbsoluteMatrix;
  end;
end;

function TControl3D.GetInheritedCursor: TCursor;
begin
  Result := FInheritedCursor;
end;

function TControl3D.GetInvertAbsoluteMatrix: TMatrix3D;
begin
  AbsoluteMatrix; // require this call to force recalulation if need
  Result := FInvAbsoluteMatrix;
end;

function TControl3D.GetIsFocused: Boolean;
begin
  Result := FIsFocused;
end;

function TControl3D.GetDesignInteractive: Boolean;
begin
  Result := False;
end;

function TControl3D.GetAbsoluteDirection: TVector3D;
begin
  Result := AbsoluteMatrix.M[2];
end;

function TControl3D.GetAbsoluteHasAfterPaintEffect: Boolean;
begin
  if FRecalcHasEffect then
    HasEffect; // Force recalc
  Result := FAbsoluteHasAfterPaintEffect;
end;

function TControl3D.GetAbsoluteHasDisablePaintEffect: Boolean;
begin
  if FRecalcHasEffect then
    HasEffect; // Force recalc
  Result := FAbsoluteHasDisablePaintEffect;
end;

function TControl3D.GetAbsoluteHasEffect: Boolean;
var
  I: Integer;
begin
  if FRecalcHasEffect then
  begin
    FAbsoluteHasEffect := False;
    FAbsoluteHasDisablePaintEffect := False;
    FAbsoluteHasAfterPaintEffect := False;
    if FDisableEffect then
    begin
      Result := FAbsoluteHasEffect;
      Exit;
    end;
    if Children <> nil then
      for I := 0 to Children.Count - 1 do
      begin
        if (TFmxObject(Children[I]) is TEffect) and (TEffect(Children[I]).Enabled) then
        begin
          FAbsoluteHasEffect := True;
          if TEffectStyle.DisablePaint in TEffect(Children[I]).EffectStyle then
            FAbsoluteHasDisablePaintEffect := True;
          if TEffectStyle.AfterPaint in TEffect(Children[I]).EffectStyle then
            FAbsoluteHasAfterPaintEffect := True;
          Break;
        end;
      end;
    FRecalcHasEffect := False;
  end;
  Result := FAbsoluteHasEffect;
end;

function TControl3D.GetAbsoluteLeft: TVector3D;
begin
  Result := AbsoluteMatrix.M[0];
end;

function TControl3D.GetAbsoluteUp: TVector3D;
begin
  Result := AbsoluteMatrix.M[1];
end;

function TControl3D.GetAbsolutePosition: TVector3D;
begin
  Result := AbsoluteMatrix.M[3];
end;

function TControl3D.ScreenToLocal(P: TPointF): TPointF;
begin
  if FViewport <> nil then
    P := FViewport.ScreenToLocal(P);
  Result := P;
end;

function TControl3D.LocalToScreen(P: TPointF): TPointF;
begin
  Result := P;
  if FViewport <> nil then
    P := FViewport.LocalToScreen(P);
end;

procedure TControl3D.SetAbsolutePosition(const Value: TVector3D);
begin
  if Parent is TControl3D then
    Position.Vector := AbsoluteToLocal3D(TPoint3D(Value))
  else
    Position.Vector := Value;
end;

function TControl3D.GetLocked: Boolean;
begin
  Result := FLocked;
end;

function TControl3D.GetParent: TFmxObject;
begin
  Result := Parent;
end;

function TControl3D.GetPopupMenu: TCustomPopupMenu;
begin
  Result := nil;
end;

function TControl3D.GetScreenBounds: TRectF;
var
  Pts: array of TPoint3D;
  a, b: TPoint3D;
  i: Integer;
begin
  if Context = nil then
  begin
    Result := RectF(0, 0, 0, 0);
    Exit;
  end;
  SetLength(Pts, 8);
  Pts[0] := Point3D(Width / 2, Height / 2, Depth / 2);
  Pts[1] := Point3D(-Width / 2, Height / 2, Depth / 2);
  Pts[2] := Point3D(-Width / 2, -Height / 2, Depth / 2);
  Pts[3] := Point3D(-Width / 2, -Height / 2, -Depth / 2);
  Pts[4] := Point3D(Width / 2, -Height / 2, Depth / 2);
  Pts[5] := Point3D(Width / 2, Height / 2, -Depth / 2);
  Pts[6] := Point3D(Width / 2, -Height / 2, -Depth / 2);
  Pts[7] := Point3D(-Width / 2, Height / 2, -Depth / 2);
  for i := 0 to High(Pts) do
    Pts[i] := Context.WorldToScreen(Projection, LocalToAbsolute3D(Pts[i]));
  { normalize }
  a := Point3D($FFFF, $FFFF, $FFFF);
  b := Point3D(-$FFFF, -$FFFF, -$FFFF);
  for i := 0 to High(Pts) do
  begin
    If Pts[i].X < a.X then
      a.X := Pts[i].X;
    If Pts[i].Y < a.Y then
      a.Y := Pts[i].Y;
    If Pts[i].X > b.X then
      b.X := Pts[i].X;
    If Pts[i].Y > b.Y then
      b.Y := Pts[i].Y;
  end;
  Result := RectF(a.X, a.Y, b.X, b.Y);
end;

function TControl3D.GetGlobalBounds: TBoundingBox;
var
  Matrix: TMatrix3D;
  Points: array[0..7] of TPoint3D;
  HalfSize: TPoint3D;
  I: Integer;
begin
  HalfSize := TPoint3D.Create(Width * 0.5, Height * 0.5, Depth * 0.5);

  Points[0] := TPoint3D.Create(-HalfSize.X, -HalfSize.Y, -HalfSize.Z);
  Points[1] := TPoint3D.Create(-HalfSize.X, -HalfSize.Y, +HalfSize.Z);
  Points[2] := TPoint3D.Create(-HalfSize.X, +HalfSize.Y, -HalfSize.Z);
  Points[3] := TPoint3D.Create(-HalfSize.X, +HalfSize.Y, +HalfSize.Z);
  Points[4] := TPoint3D.Create(+HalfSize.X, -HalfSize.Y, -HalfSize.Z);
  Points[5] := TPoint3D.Create(+HalfSize.X, -HalfSize.Y, +HalfSize.Z);
  Points[6] := TPoint3D.Create(+HalfSize.X, +HalfSize.Y, -HalfSize.Z);
  Points[7] := TPoint3D.Create(+HalfSize.X, +HalfSize.Y, +HalfSize.Z);

  Matrix := AbsoluteMatrix;
  for I := 0 to 7 do
    Points[I] := Points[I] * Matrix;

  Result := TBoundingBox.Create(@Points[0], 8);
end;

function TControl3D.GetLocalBounds: TBoundingBox;
var
  HalfSize: TPoint3D;
begin
  HalfSize := TPoint3D.Create(Width * 0.5, Height * 0.5, Depth * 0.5);
  Result := TBoundingBox.Create(-HalfSize, HalfSize);
end;

procedure TControl3D.RecalcAbsolute;
var
  i: Integer;
  Child: TControl3D;
begin
  FRecalcAbsolute := True;
  if Children <> nil then
    for i := 0 to Children.Count - 1 do
    begin
        if not(Children[i] is TControl3D) then
        Continue;
        Child := TControl3D(Children[i]);
        TControl3D(Child).RecalcAbsolute;
    end;
end;

procedure TControl3D.UpdateEffects;
begin
  if HasEffect then
    FUpdateEffects := True;
  if Parent is TControl3D then
    TControl3D(Parent).UpdateEffects;
end;

procedure TControl3D.RecalcHasEffect;
begin
  FRecalcHasEffect := True;
  if Parent is TControl3D then
    TControl3D(Parent).RecalcHasEffect;
end;

function TControl3D.AbsoluteToLocalVector(const Vector: TVector3D): TVector3D;
begin
  Result := Vector * InvertAbsoluteMatrix;
end;

function TControl3D.LocalToAbsoluteVector(const Vector: TVector3D): TVector3D;
begin
  Result := Vector * AbsoluteMatrix;
end;

function TControl3D.AbsoluteToLocalDirection(const Vector: TVector3D): TVector3D;
begin
  Result := TVector3D.Create(TPoint3D(AbsoluteToLocalVector(TVector3D.Create(Vector.X, Vector.Y, Vector.Z, 0))).Normalize, 0);
end;

function TControl3D.LocalToAbsoluteDirection(const Vector: TVector3D): TVector3D;
begin
  Result := TVector3D.Create(TPoint3D(LocalToAbsoluteVector(TVector3D.Create(Vector.X, Vector.Y, Vector.Z, 0))).Normalize, 0);
end;

function TControl3D.AbsoluteToLocal(P: TPointF): TPointF;
begin
  Result := P;
end;

function TControl3D.AbsoluteToLocal3D(const Point: TPoint3D): TPoint3D;
begin
  Result := Point * InvertAbsoluteMatrix;
end;

function TControl3D.LocalToAbsolute3D(const Point: TPoint3D): TPoint3D;
begin
  Result := Point * AbsoluteMatrix;
end;

{ Opacity }

function TControl3D.GetAbsoluteOpacity: Single;
begin
  if FRecalcOpacity then
  begin
    if FParent is TControl3D then
      FAbsoluteOpacity := FOpacity * TControl3D(FParent).AbsoluteOpacity
    else
      FAbsoluteOpacity := FOpacity;

    Result := FAbsoluteOpacity;
    FRecalcOpacity := False;
  end
  else
  begin
    Result := FAbsoluteOpacity;
  end;
end;

procedure TControl3D.RecalcOpacity;
var
  i: Integer;
  Child: TControl3D;
begin
  FRecalcOpacity := True;
  if Children <> nil then
    for i := 0 to Children.Count - 1 do
    begin
      if not(Children[i] is TControl3D) then
        Continue;
      Child := TControl3D(Children[i]);
      TControl3D(Child).RecalcOpacity;
    end;
end;

procedure TControl3D.RefreshInheritedCursor;

  function GetParentInheritedCursor: TCursor;
  begin
    Result := crDefault;
    if Parent is TControl3D then
      Result := (Parent as TControl3D).InheritedCursor;
    if Parent is TCommonCustomForm then
      Result := (Parent as TCommonCustomForm).Cursor;
  end;

var
  CursorTmp: TCursor;
begin
  if (Cursor = crDefault) and (Parent <> nil) then
    CursorTmp := GetParentInheritedCursor
  else
    CursorTmp := Cursor;

  if FInheritedCursor <> CursorTmp then
  begin
    FInheritedCursor := CursorTmp;
    RefreshInheritedCursorForChildren;
  end;
end;

procedure TControl3D.RefreshInheritedCursorForChildren;
var
  Child: TFmxObject;
begin
  if Children <> nil then
    for Child in Children do
      if (Child is TControl3D) and ((Child as TControl3D).Cursor = crDefault) then
         (Child as TControl3D).RefreshInheritedCursor;
end;

procedure TControl3D.SetLocked(const Value: Boolean);
begin
  FLocked := Value;
end;

procedure TControl3D.SetNewViewport(AViewport: IViewport3D);
var
  i: Integer;
begin
  FViewport := AViewport;
  if (Children <> nil) and (Children.Count > 0) then
    for i := 0 to Children.Count - 1 do
      if TFmxObject(Children[i]) is TControl3D then
        TControl3D(Children[i]).SetNewViewport(FViewport);
end;

{ methods }

function TControl3D.RayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean;
begin
  Result := DoRayCastIntersect(RayPos, RayDir, Intersection);
end;

function TControl3D.DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean;
var
  INear, IFar: TPoint3D;
begin
  Result := RayCastCuboidIntersect(RayPos, RayDir, NullPoint3D, Width, Height, Depth, INear, IFar) > 0;
  if Result then
    Intersection := TPoint3D(LocalToAbsoluteVector(INear));
end;

function TControl3D.CheckForAllowFocus: Boolean;
begin
  Result := Visible and CanFocus;
end;

function TControl3D.CheckHitTest(const AHitTest: Boolean): Boolean;
begin
  Result := AHitTest;
  if csDesigning in ComponentState then
  begin
    if Visible then
      Result := True;
    if FLocked then
      Result := False;
    if FDesignLocked then
      Result := False;
  end;
end;

function TControl3D.ObjectAtPoint(P: TPointF): IControl;
var
  I: Integer;
  Obj: TFmxObject;
  NewObj: IControl;
  RayPos, RayDir: TVector3D;
  Intersect: TPoint3D;
begin
  Result := nil;

  for I := ChildrenCount - 1 downto 0 do
  begin
    Obj := Children[I];

    if IInterface(Obj).QueryInterface(IControl, NewObj) <> 0 then
      Continue;

    if not NewObj.GetVisible then
      Continue;

    NewObj := NewObj.ObjectAtPoint(P);
    if NewObj <> nil then
      Result := NewObj;
  end;

  if (Result = nil) and (Context <> nil) and Visible and (GlobalProjection = Projection) and CheckHitTest(HitTest) then
  begin
    if FViewport <> nil then
      P := FViewport.ScreenToLocal(P);

    Context.Pick(P.X, P.Y, FProjection, RayPos, RayDir);

    if RayCastIntersect(TPoint3D(AbsoluteToLocalVector(RayPos)), TPoint3D(AbsoluteToLocalDirection(RayDir)),
      Intersect) then
    begin
      if (Projection = TProjection.Screen) and ((Intersect - TPoint3D(RayPos)).Length < GlobalDistance) then
      begin
        GlobalDistance := (Intersect - TPoint3D(RayPos)).Length;
        Result := Self;
      end
      else if (Projection = TProjection.Camera) and (FViewport.CurrentCamera <> nil) and
        ((Intersect - TPoint3D(FViewport.CurrentCamera.AbsolutePosition)).Length < GlobalDistance) then
      begin
        GlobalDistance := (Intersect - TPoint3D(FViewport.CurrentCamera.AbsolutePosition)).Length;
        Result := Self;
      end;
    end;
  end;
end;

function TControl3D.FindTarget(P: TPointF; const Data: TDragObject): IControl;
var
  I: Integer;
  Obj: TFmxObject;
  NewObj: IControl;
  Intersect: TPoint3D;
  RayPos, RayDir: TVector3D;
  Operation: TDragOperation;
begin
  Result := nil;

  for I := ChildrenCount - 1 downto 0 do
  begin
    Obj := Children[I];

    if IInterface(Obj).QueryInterface(IControl, NewObj) <> 0 then
      Continue;

    if not NewObj.GetVisible  then
      Continue;

    NewObj := NewObj.FindTarget(P, Data);
    if NewObj <> nil then
      Result := NewObj;
  end;

  if (Result = nil) and (Context <> nil) then
  begin
    if FViewport <> nil then
      P := FViewport.ScreenToLocal(P);

    Context.Pick(P.X, P.Y, FProjection, RayPos, RayDir);

    if CheckHitTest(HitTest) and RayCastIntersect(TPoint3D(AbsoluteToLocalVector(RayPos)),
      TPoint3D(AbsoluteToLocalDirection(RayDir)), Intersect) and (GlobalProjection = Projection) then
    begin
      if (Projection = TProjection.Screen) and ((Intersect - TPoint3D(RayPos)).Length < GlobalDistance) then
      begin
        GlobalDistance := (Intersect - TPoint3D(RayPos)).Length;
        Operation := TDragOperation.None;
        DragOver(Data, P, Operation);
        if Operation <> TDragOperation.None then
          Result := Self;
      end;
      if (Projection = TProjection.Camera) and (FViewport.CurrentCamera <> nil) and ((Intersect -
        TPoint3D(FViewport.CurrentCamera.AbsolutePosition)).Length < GlobalDistance) then
      begin
        GlobalDistance := (Intersect - TPoint3D(FViewport.CurrentCamera.AbsolutePosition)).Length;
        Operation := TDragOperation.None;
        DragOver(Data, P, Operation);
        if Operation <> TDragOperation.None then
          Result := Self;
      end;
    end;
  end;
end;

procedure TControl3D.FreeNotification(AObject: TObject);
begin
  inherited;
end;

function TControl3D.GetObject: TFmxObject;
begin
  Result := Self;
end;

function TControl3D.GetContext: TContext3D;
begin
  if FTempContext <> nil then
    Result := FTempContext
  else
    if FViewport <> nil then
      Result := FViewport.GetContext
    else
      Result := nil;
end;

function TControl3D.GetCursor: TCursor;
begin
  Result := FCursor;
end;

function TControl3D.GetDragMode: TDragMode;
begin
  Result := FDragMode;
end;

function TControl3D.GetEnabled: Boolean;
begin
  Result := True;
end;

function TControl3D.GetAbsoluteEnabled: Boolean;
var
  ParentControl: IControl;
begin
  if Supports(Parent, IControl, ParentControl) then
    Result := ParentControl.Enabled
  else
    Result := True;
end;

function TControl3D.GetHintString: string;
begin
  Result := FHint;
end;

function TControl3D.GetHintObject: TObject;
begin
  Result := nil;
end;

function TControl3D.GetHitTest: Boolean;
begin
  Result := FHitTest;
end;

function TControl3D.GetAcceptsControls: boolean;
begin
  Result := FAcceptsControls;
end;

function TControl3D.GetMaterialForSorting: TMaterial;
begin
  Result := nil;
end;

procedure TControl3D.BeforeRender;
begin

end;

procedure TControl3D.BeginAutoDrag;
var
  S, B: TBitmap;
begin
  if Root <> nil then
  begin
    S := TBitmap.Create(0, 0);
    try
      B := TBitmap.Create(48, 48);
      try
        PaintToBitmap(S, 128, 128, 0, False, TMultisample.None);
        if B.Canvas.BeginScene then
        try
          B.Canvas.DrawBitmap(S, RectF(0, 0, S.Width, S.Height), RectF(0, 0, B.Width, B.Height), 0.7);
        finally
          B.Canvas.EndScene;
        end;
        Root.BeginInternalDrag(Self, B);
      finally
        B.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TControl3D.RenderInternal;
begin
  if not HasDisablePaintEffect or FInRenderTo then
  begin
    BeforeRender;
    if HasEffect and not HasAfterPaintEffect and not (FInRenderTo) then
      ApplyEffect;
    Context.SetMatrix(AbsoluteMatrix);
    if FZWrite then
    begin
      Context.SetContextState(TContextState.csZWriteOn);
      Context.SetContextState(TContextState.csZTestOn);
    end
    else
    begin
      Context.SetContextState(TContextState.csZWriteOff);
      Context.SetContextState(TContextState.csZTestOff);
    end;
    if Projection = TProjection.Camera then
      Context.SetContextState(TContextState.cs3DScene)
    else
      Context.SetContextState(TContextState.cs2DScene);
    if TwoSide then
      Context.SetContextState(TContextState.csAllFace)
    else
      Context.SetContextState(TContextState.csFrontFace);

    if Opaque then
      Context.SetContextState(TContextState.csAlphaBlendOff)
    else
      Context.SetContextState(TContextState.csAlphaBlendOn);
    Render;
    RenderHelper;
    RenderChildren;
    if HasAfterPaintEffect and not (FInRenderTo) then
      ApplyEffect;
  end;
  if HasDisablePaintEffect and not FInRenderTo then
    ApplyEffect;
  {$IFDEF MSWINDOWS}
  if not FInRenderTo and (Root <> nil) and (Root.GetObject is TCommonCustomForm) and
    (TCommonCustomForm(Root.GetObject).Designer <> nil) and
    TCommonCustomForm(Root.GetObject).Designer.IsSelected(Self) then
  begin
    Context.SetMatrix(AbsoluteMatrix);
    if FZWrite then
    begin
      Context.SetContextState(TContextState.csZWriteOn);
      Context.SetContextState(TContextState.csZTestOn);
    end
    else
    begin
      Context.SetContextState(TContextState.csZWriteOff);
      Context.SetContextState(TContextState.csZTestOff);
    end;
    TCommonCustomForm(Root.GetObject).Designer.DrawSelectionMarks(Self);
  end;
  {$ENDIF}
end;

procedure TControl3D.Render;
begin
end;

procedure TControl3D.RenderHelper;
begin
  DoRender;
  if not FDisableDragHighlight and (IsDragOver) then
  begin
    Context.SetMatrix(AbsoluteMatrix);
    Context.SetContextState(TContextState.csZWriteOn);
    Context.FillCube(NullPoint3D, TPoint3D.Create(Width + 0.01, Height + 0.01, Depth + 0.01), 0.4, $B2005ACC);
  end;
end;

procedure TControl3D.DoRender;
begin
  if Assigned(FOnRender) then
    FOnRender(Self, Context);
end;

procedure TControl3D.RebuildRenderingList;
var
  I: Integer;
  CompareFunc: TRenderingCompare;
begin
  if (Children <> nil) and (Children.Count > 0) then
  begin
    if (FUpdating = 0) then
    begin
      if FRenderingList = nil then
        FRenderingList := TList<TControl3D>.Create;
      FRenderingList.Clear;

      for I := 0 to Children.Count - 1 do
        if (Children[i] is TControl3D) then
          FRenderingList.Add(Children[I] as TControl3D);

      CompareFunc := TRenderingCompare.Create;
      try
        FRenderingList.Sort(CompareFunc);
      finally
        CompareFunc.Free;
      end;
    end;
  end else
    if FRenderingList <> nil then
      FRenderingList.Clear;
end;

procedure TControl3D.RenderChildren;
var
  I: Integer;
  Control: TControl3D;
begin
  if (FRenderingList <> nil) and (FRenderingList.Count > 0) then
  begin
    for I := 0 to FRenderingList.Count - 1 do
      if FRenderingList[i].Visible then
      begin
        Control := FRenderingList[i];
        Control.RenderInternal;
      end;
  end;
end;

procedure TControl3D.ApplyEffect;
var
  I: Integer;
  Effect: TFilterEffect;
  ControlRect, RoundRect, EffectRect: TRectF;
  Ver: TVertexBuffer;
  Ind: TIndexBuffer;
  Mat: TTextureMaterial;
begin
  if FViewport = nil then
    Exit;
  if FDisableEffect then
    Exit;
  if not HasEffect then
    Exit;
  ControlRect := ScreenBounds;
  if ControlRect.IsEmpty then Exit;

{  if not FUpdateEffects then
  begin
    if Assigned(FEffectTexture) then
    begin
      Canvas.SetMatrix(AbsoluteMatrix);
      for I := 0 to FChildren.Count - 1 do
        if (TFmxObject(FChildren[I]) is TEffect) and (TEffect(FChildren[I]).Enabled) then
        begin
          Effect := TEffect(FChildren[I]);
          EffectRect := Effect.GetRect(RectF(0, 0, Width, Height));
          Canvas.DrawBitmap(FEffectBitmap, RectF(0, 0, FEffectBitmap.Width, FEffectBitmap.Height), EffectRect, AbsoluteOpacity, RotationAngle = 0);
          Break;
        end;
    end;
  end
  else}
  begin
    for I := 0 to Children.Count - 1 do
      if (TFmxObject(Children[I]) is TFilterEffect) and (TFilterEffect(Children[I]).Enabled) then
      begin
        Effect := TFilterEffect(Children[I]);
        RoundRect := RectF(ControlRect.Truncate.Left, ControlRect.Truncate.Top, ControlRect.Round.Right, ControlRect.Round.Bottom);
        EffectRect := Effect.GetRect(RoundRect);

        if FEffectTexture = nil then
        begin
          FEffectTexture := TTexture.Create;
          FEffectTexture.Style := [TTextureStyle.RenderTarget];
          FEffectTexture.SetSize(EffectRect.Round.Width, EffectRect.Round.Height);
          FEffectContext := TContextManager.CreateFromTexture(FEffectTexture, Context.Multisample, True);
        end else if (FEffectTexture.Width <> EffectRect.Round.Width) or (FEffectTexture.Height <> EffectRect.Round.Height) then
        begin
          FreeAndNil(FEffectContext);
          FreeAndNil(FEffectTexture);
          FEffectTexture := TTexture.Create;
          FEffectTexture.Style := [TTextureStyle.RenderTarget];
          FEffectTexture.SetSize(EffectRect.Round.Width, EffectRect.Round.Height);
          FEffectContext := TContextManager.CreateFromTexture(FEffectTexture, Context.Multisample, True);
        end;

        // Render self
        if not (TEffectStyle.DisablePaintToBitmap in Effect.EffectStyle) then
        begin
          if FEffectContext.BeginScene then
          try
            FEffectContext.Clear([TClearTarget.Color, TClearTarget.Depth], 0, 1.0, 0);
            RenderTo(FEffectContext, Effect.GetOffset);
          finally
            FEffectContext.EndScene;
          end;
        end;
        // Apply effects
        Effect.ProcessTexture(FEffectTexture, FEffectContext);
        // Render EffectTexture
        EffectRect.Offset(Context.PixelToPixelPolygonOffset.X, Context.PixelToPixelPolygonOffset.Y);
        Ver := TVertexBuffer.Create([TVertexFormat.Vertex, TVertexFormat.TexCoord0], 4);
        Ver.Vertices[0] := Point3D(EffectRect.Left, EffectRect.Top, 0);
        Ver.TexCoord0[0] := PointF(0.0, 0.0);
        Ver.Vertices[1] := Point3D(EffectRect.Right, EffectRect.Top, 0);
        Ver.TexCoord0[1] := PointF(1.0, 0.0);
        Ver.Vertices[2] := Point3D(EffectRect.Right, EffectRect.Bottom, 0);
        Ver.TexCoord0[2] := PointF(1.0, 1.0);
        Ver.Vertices[3] := Point3D(EffectRect.Left, EffectRect.Bottom, 0);
        Ver.TexCoord0[3] := PointF(0.0, 1.0);
        Ind := TIndexBuffer.Create(6);
        Ind[0] := 0;
        Ind[1] := 1;
        Ind[2] := 3;
        Ind[3] := 3;
        Ind[4] := 1;
        Ind[5] := 2;
        Mat := TTextureMaterial.Create;
        Mat.Texture := FEffectTexture;
        Context.SetContextState(TContextState.cs2DScene);
        Context.SetMatrix(TMatrix3D.Identity);
        Context.DrawTriangles(Ver, Ind, Mat, 1);
        Mat.Free;
        Ind.Free;
        Ver.Free;
        Break;
      end;
    FUpdateEffects := False;
  end;
end;

procedure TControl3D.RenderTo(const AContext: TContext3D; const Offset: TPointF);
var
  S, T: TMatrix3D;
  R: TRectF;
begin
  R := ScreenBounds;
  if R.IsEmpty then Exit;

  if FViewport <> nil then
  begin
    if Projection = TProjection.Camera then
    begin
      S := TMatrix3D.Identity;
      S.m11 := FViewport.GetContext.Height / AContext.Height;
      S.m22 := S.m11;

      T := TMatrix3D.Identity;
      T.m41 := (Frac(R.Left) + ((R.Width - AContext.Width) / 2) + Offset.X + ((FViewport.GetContext.Width / 2) - ((R.Left + R.Right) / 2))) / AContext.Width * 2;
      T.m42 := -(Frac(R.Top) + ((R.Height - AContext.Height) / 2) + Offset.Y + ((FViewport.GetContext.Height / 2) - ((R.Top + R.Bottom) / 2))) / AContext.Height * 2;
      AContext.SetRenderToMatrix(S * T);
    end
    else
    begin
      S := TMatrix3D.Identity;
      S.m11 := FViewport.GetContext.Width / AContext.Width;
      S.m22 := FViewport.GetContext.Height / AContext.Height;

      T := TMatrix3D.Identity;
{      T.m41 := (Frac(R.Left) + ((R.Width - AContext.Width) / 2) + Offset.X + ((FViewport.GetContext.Width / 2) - ((R.Left + R.Right) / 2))) / AContext.Width * 2;
      T.m42 := -(Frac(R.Top) + ((R.Height - AContext.Height) / 2) + Offset.Y + ((FViewport.GetContext.Height / 2) - ((R.Top + R.Bottom) / 2))) / AContext.Height * 2;}
      T.m41 := -R.Left;
      T.m42 := -R.Top;

      AContext.SetRenderToMatrix((S * T) * FViewport.Context.CurrentScreenMatrix);
//      AContext.FRenderToMatrix := S * FViewport.GetContext.GetScreenMatrix;
//      AContext.FRenderToMatrix := FViewport.GetContext.GetScreenMatrix;
    end;
  end;

  TempContext := AContext;
  try
    AContext.SetStateFromContext(FViewport.Context);
    // render
    if AContext.BeginScene then
    try
      FInRenderTo := True;
      RenderInternal;
    finally
      AContext.SetRenderToMatrix(TMatrix3D.Identity);
      FInRenderTo := False;
      AContext.EndScene;
//      AContext.ClearLights;
    end;
  finally
    TempContext := nil;
  end;
end;

procedure TControl3D.PaintToBitmap(const ABitmap: TBitmap; AWidth, AHeight: Integer; ClearColor: TAlphaColor;
  AutoFit: Boolean = False; const AMultisample: TMultisample = TMultisample.None);
var
  B: TBitmap;
  FillR, R: TRectF;
  i, j: Integer;
  ratio: single;
  FitR: TRectF;
  S, t: TMatrix3D;
  Scale: Single;
  BitmapContext: TContext3D;
  Texture: TTexture;
  M: TBitmapData;
begin
  if AutoFit then
  begin
    { Render to bitmap }
    B := TBitmap.Create(0, 0);
    try
      PaintToBitmap(B, AWidth, AHeight, 0, False, TMultisample.None); // - render with alpha
      { calc best size }
      R := RectF(B.Width, B.Height, 0, 0);
      if B.Map(TMapAccess.Read, M) then
      try
        for i := 0 to B.Width - 1 do
          for j := 0 to B.Height - 1 do
          begin
            if PAlphaColorRecArray(M.Data)[i * (M.Pitch div 4) + j].A > 0 then
            begin
              if i < R.Left then
                R.Left := i;
              if j < R.Top then
                R.Top := j;
              if i > R.Right then
                R.Right := i;
              if j > R.Bottom then
                R.Bottom := j;
            end;
          end;
      finally
        B.Unmap(M);
      end;
      FillR := R;
      ratio := R.Fit(RectF(0, 0, AWidth, AHeight));
      if (ratio > 0) and (ratio < 1) then
      begin
        ABitmap.SetSize(AWidth, AHeight);
        // render again to better size
        PaintToBitmap(b, Round(AWidth / ratio), Round(AHeight / ratio), 0, False, TMultisample.None);
        { calc again }
        R := RectF(b.Width, b.Height, 0, 0);
        if B.Map(TMapAccess.Read, M) then
        try
          for i := 0 to b.Width - 1 do
            for j := 0 to b.Height - 1 do
            begin
              if PAlphaColorRecArray(M.Data)[i * (M.Pitch div 4) + j].A > 0 then
              begin
                if i < R.Left then
                  R.Left := i;
                if j < R.Top then
                  R.Top := j;
                if i > R.Right then
                  R.Right := i;
                if j > R.Bottom then
                  R.Bottom := j;
              end;
            end;
        finally
          B.Unmap(M);
        end;
        FillR := R;
        FillR.Fit(RectF(0, 0, AWidth, AHeight));
        ABitmap.Clear(CorrectColor(ClearColor));
        ABitmap.Canvas.DrawBitmap(b, RectF(R.Left, R.Top, R.Left + RectWidth(R), R.Top + RectHeight(FillR)),
          FillR, 1, True);
      end;
    finally
      B.Free;
    end;
  end
  else
  begin
    R := ScreenBounds;
    if IsRectEmpty(R) then
      Exit;
    FitR := R;

    Scale := 1 / FitR.Fit(RectF(0, 0, AWidth, AHeight));

    ABitmap.SetSize(Round(RectWidth(R) * Scale), Round(RectHeight(R) * Scale));

    Texture := TTexture.Create;
    try
      Texture.SetSize(ABitmap.Width, ABitmap.Height);
      Texture.Style := [TTextureStyle.RenderTarget];

      BitmapContext := TContextManager.CreateFromTexture(Texture, AMultisample, True);
      if FViewport <> nil then
      begin
        S := TMatrix3D.Identity;
        S.m11 := FViewport.GetContext.Height / RectHeight(R);
        S.m22 := S.m11;

        t := TMatrix3D.Identity;
        t.m41 := ( { OffsetX + } ((FViewport.GetContext.Width / 2) - ((R.Left + R.Right) / 2))) / RectWidth(R) * 2;
        t.m42 := -( { OffsetY + } ((FViewport.GetContext.Height / 2) - ((R.Top + R.Bottom) / 2))) / RectHeight(R) * 2;
        BitmapContext.SetRenderToMatrix(S * t);
      end;

      TempContext := BitmapContext;
      try
        Context.SetStateFromContext(FViewport.Context);
        // render
        if Context.BeginScene then
        try
          Context.Clear([TClearTarget.Color, TClearTarget.Depth], ClearColor, 1.0, 0);
          RenderInternal;
        finally
          Context.EndScene;
        end;
      finally
        TempContext := nil;
      end;
      BitmapContext.CopyToBitmap(ABitmap, Rect(0, 0, BitmapContext.Width, BitmapContext.Height));
      BitmapContext.Free;
    finally
      Texture.Free;
    end;
  end;
end;

procedure TControl3D.CreateTileSnapshot(const ABitmap: TBitmap; AWidth, AHeight, OffsetX, OffsetY: Integer;
  Scale: Single; ClearColor: TAlphaColor);
var
  FitR, R: TRectF;
  S, t: TMatrix3D;
  BitmapContext: TContext3D;
  Texture: TTexture;
begin
  R := ScreenBounds;
  if IsRectEmpty(R) then Exit;
  FitR := RectF(R.left * Scale, R.Top * Scale, R.Right * Scale, R.Bottom * Scale);

  RectCenter(FitR, RectF(0, 0, AWidth, AHeight));

  ABitmap.SetSize(AWidth, AHeight);
  Texture := TTexture.Create;
  try
    Texture.SetSize(ABitmap.Width, ABitmap.Height);
    Texture.Style := [TTextureStyle.RenderTarget];

    BitmapContext := TContextManager.CreateFromTexture(Texture, TMultisample.None, True);
    if (FViewport <> nil) and (FViewport.Context <> nil) then
    begin
      S := TMatrix3D.Identity;
      S.m11 := Min(FViewport.Context.Height / AHeight, (FViewport.Context.Width / AWidth)) * Scale;
      S.m22 := S.m11;

      T := TMatrix3D.Identity;
      T.m41 := (((-FitR.Left - offsetx) / Scale) + ((FViewport.Context.Width / 2) - ((R.Left + R.Right) / 2))) / AWidth * 2 * Scale;
      T.m42 := -(((-FitR.Top - offsety) / Scale) + ((FViewport.Context.Height / 2) - ((R.Top + R.Bottom) / 2))) / AHeight * 2 * Scale;

      TempContext := BitmapContext;
      try
        Context.SetRenderToMatrix(S * T);
        Context.SetStateFromContext(FViewport.Context);
        // render
        if Context.BeginScene then
        try
          Context.Clear([TClearTarget.Color, TClearTarget.Depth], ClearColor, 1.0, 0);
          RenderInternal;
        finally
          Context.EndScene;
        end;
      finally
        TempContext := nil;
      end;
    end;
    BitmapContext.CopyToBitmap(ABitmap, Rect(0, 0, BitmapContext.Width, BitmapContext.Height));
    BitmapContext.Free;
  finally
    Texture.Free;
  end;
end;

procedure TControl3D.CreateHighMultisampleSnapshot(const ABitmap: TBitmap; AWidth, AHeight: Integer; ClearColor: TAlphaColor;
  Multisample: Integer);
const
  TileSize = 512;
var
  I, J: Integer;
  Sample: TBitmap;
  Tile: TBitmap;
  R, FitR, TileR: TRectF;
  Factor: Single;
begin
  if Multisample < 1 then Multisample := 1;
  if Multisample > 16 then Multisample := 16;
  R := ScreenBounds;
  FitR := R;
  Factor := FitR.Fit(RectF(0, 0, AWidth, AHeight));
  if Factor < 1 then
  begin
    R := TRectF.Create(R.Left / Factor, R.Top / Factor, R.Right / Factor, R.Bottom / Factor);
    RectCenter(R, TRectF.Create(0, 0, AWidth, AHeight));
  end
  else
    R := FitR;

  Sample := TBitmap.Create(Round(R.Width * Multisample), Round(R.Height * Multisample));
  try
    Tile := TBitmap.Create(TileSize, TileSize);
    if Sample.Canvas.BeginScene then
    try
      for I := 0 to Sample.Width div TileSize do
        for J := 0 to Sample.Height div TileSize do
          begin
            CreateTileSnapshot(Tile, TileSize, TileSize, I * TileSize, J * TileSize, Multisample / Factor, ClearColor);
            TileR := RectF(0, 0, TileSize, TileSize);
            OffsetRect(TileR, I * TileSize, J * TileSize);
            Sample.Canvas.DrawBitmap(Tile, TRectF.Create(0, 0, TileSize, TileSize), TileR, 1, True);
            Sample.Canvas.Flush;
          end;
    finally
      Sample.Canvas.EndScene;
      Tile.Free;
    end;

    ABitmap.SetSize(AWidth, AHeight);
    if ABitmap.Canvas.BeginScene then
    try
      ABitmap.Canvas.DrawBitmap(Sample, TRectF.Create(0, 0, Sample.Width, Sample.Height), R, 1);
    finally
      ABitmap.Canvas.EndScene;
    end;
  finally
    Sample.Free;
  end;
end;

procedure TControl3D.Repaint;
begin
  if not Visible then
    Exit;
  if FViewport = nil then
    Exit;
  if csDestroying in ComponentState then
    Exit;
  if FUpdating > 0 then
    Exit;
  if HasEffect then
    UpdateEffects;
  FViewport.NeedRender;
end;

procedure TControl3D.Lock;
var
  i: Integer;
begin
  Locked := True;
  if Children <> nil then
    for i := 0 to Children.Count - 1 do
      if Children[i] is TControl3D then
        TControl3D(Children[i]).Lock;
end;

procedure TControl3D.DialogKey(var Key: Word; Shift: TShiftState);
begin
end;

procedure TControl3D.AfterDialogKey(var Key: Word; Shift: TShiftState);
begin
end;

procedure TControl3D.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, Key, KeyChar, Shift);
end;

procedure TControl3D.KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self, Key, KeyChar, Shift);
end;

procedure TControl3D.Capture;
begin
  if Root <> nil then
    Root.SetCaptured(Self);
end;

procedure TControl3D.ReleaseCapture;
begin
  if Root <> nil then
    Root.SetCaptured(nil);
end;

procedure TControl3D.DoMouseEnter;
begin
  FIsMouseOver := True;
  TAnimator.StartTriggerAnimation(Self, Self, 'IsMouseOver');
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TControl3D.DoMouseLeave;
begin
  FIsMouseOver := False;
  TAnimator.StartTriggerAnimation(Self, Self, 'IsMouseOver');
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TControl3D.DoEnter;
begin
  FIsFocused := True;
  TAnimator.StartTriggerAnimation(Self, Self, 'IsFocused');
end;

procedure TControl3D.DoExit;
begin
  FIsFocused := False;
  TAnimator.StartTriggerAnimation(Self, Self, 'IsFocused');
end;

procedure TControl3D.DoInsertObject(Index: Integer; const AObject: TFmxObject);
begin
  inherited;
  RefreshInheritedCursorForChildren;
end;

procedure TControl3D.DoActivate;
begin
  if Assigned(FOnActivate) then
    FOnActivate(self);
end;

procedure TControl3D.DoDeactivate;
begin
  if Assigned(FOnDeactivate) then
    FOnDeactivate(self);
end;

procedure TControl3D.SetFocus;
var
  LControl: IControl;
begin
  if Root <> nil then
  begin
    LControl := Root.NewFocusedControl(Self);
    if LControl <> nil then
      Root.SetFocused(LControl);
  end;
end;

{ Tab }
function TControl3D.GetTabList: ITabList;
begin
  if FTabList = nil then
    FTabList := TTabList.Create(Self);
  Result := FTabList;
end;

procedure TControl3D.SetTabOrder(const Value: TTabOrder);
var
  Controller: ITabStopController;
begin
  Controller := GetTabStopController;
  if Controller <> nil then
    Controller.TabList.Update(Self, Value);
end;

function TControl3D.GetTabOrder: TTabOrder;
var
  Controller: ITabStopController;
begin
  Controller := GetTabStopController;
  if Controller <> nil then
    Result := Controller.TabList.GetTabOrder(Self)
  else
    Result := -1;
end;

function TControl3D.GetTabStopController: ITabStopController;
var
  ParentObject: TFmxObject;
begin
  ParentObject := Parent;
  while (ParentObject <> nil) and (not Supports(ParentObject, ITabStopController, Result)) do
    ParentObject := ParentObject.Parent;
end;

procedure TControl3D.SetTabStop(const TabStop: Boolean);
begin
  FTabStop := TabStop;
end;

function TControl3D.GetTabStop: Boolean;
begin
  Result := FTabStop;
end;

function TControl3D.GetVisible: Boolean;
begin
  Result := Visible;
end;

function TControl3D.HasHint: Boolean;
begin
  Result := (FHint.Length > 0) and FShowHint;
end;

function TControl3D.ShowContextMenu(const ScreenPosition: TPointF): Boolean;
begin
  Result := False;
  { if FPopupMenu) then
    begin
    FPopupMenu.PopupComponent := Self;
    FPopupMenu.Popup(round(ScreenPosition.X), round(ScreenPosition.Y));
    end; }
end;

procedure TControl3D.CopyRotationFrom(const AObject: TControl3D);
begin
  FRotationAngle.SetPoint3DNoChange(AObject.RotationAngle.Point);
  FQuaternion := AObject.FQuaternion;
  MatrixChanged(Self);
end;

procedure TControl3D.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TControl3D.DblClick;
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

procedure TControl3D.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if FPressed and not (FDoubleClick) and (FIsMouseOver) then
  begin
    FPressed := False;
    Click;
  end
  else if FPressed and (FDoubleClick) and (FIsMouseOver) then
  begin
    FDoubleClick := False;
    FPressed := False;
    DblClick;
  end;
end;

procedure TControl3D.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  P: TPointF;
  RayDir, RayPos: TVector3D;
begin
  if (not (csDesigning in ComponentState)) and (not FIsFocused) then
    SetFocus;

  P := TPointF.Create(X, Y);
  Context.Pick(P.X, P.Y, FProjection, RayPos, RayDir);
  RayPos := TPoint3D(AbsoluteToLocalVector(RayPos));
  RayDir := AbsoluteToLocalDirection(RayDir);
  MouseDown3D(Button, Shift, P.X, P.Y, RayPos, RayDir);
end;

procedure TControl3D.MouseMove(Shift: TShiftState; X, Y: Single);
var
  P: TPointF;
  RayDir, RayPos: TVector3D;
begin
  P := TPointF.Create(X, Y);
  Context.Pick(P.X, P.Y, FProjection, RayPos, RayDir);
  RayPos := TPoint3D(AbsoluteToLocalVector(RayPos));
  RayDir := AbsoluteToLocalDirection(RayDir);
  MouseMove3D(Shift, P.X, P.Y, RayPos, RayDir);
end;

procedure TControl3D.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  P: TPointF;
  RayDir, RayPos: TVector3D;
begin
  if Context <> nil then
  begin
    P := TPointF.Create(X, Y);
    Context.Pick(P.X, P.Y, FProjection, RayPos, RayDir);
    RayPos := TPoint3D(AbsoluteToLocalVector(RayPos));
    RayDir := AbsoluteToLocalDirection(RayDir);
    MouseUp3D(Button, Shift, P.X, P.Y, RayPos, RayDir);
  end;
end;

procedure TControl3D.Tap(const Point: TPointF);
var
  RayDir, RayPos: TVector3D;
begin
  Context.Pick(Point.X, Point.Y, FProjection, RayPos, RayDir);
  RayPos := TPoint3D(AbsoluteToLocalVector(RayPos));
  RayDir := AbsoluteToLocalDirection(RayDir);

  if Assigned(OnTap) then
    OnTap(Self, Point, RayPos, RayDir);
end;

function TControl3D.GetCanFocus: boolean;
begin
  Result := CanFocus;
end;

function TControl3D.GetCanParentFocus: boolean;
begin
  Result := FCanParentFocus;
end;

procedure TControl3D.MouseDown3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single;
  RayPos, RayDir: TVector3D);
begin
  if (not (csDesigning in ComponentState)) and (not FIsFocused) then
    SetFocus;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y, RayPos, RayDir);
   {if (Button = TMouseButton.mbRight) and FShowContextMenu then
    begin
    VP := LocalToAbsolute(Point3D(X, Y, 0));
    P := Point(Trunc(VP.X), Trunc(VP.Y));
    P := Scene.ClientToScreen(P);
    ContextMenu(PointF(P.X, P.Y));
    Exit;
    end; }
  if FAutoCapture then
    Capture;
  if (ssDouble in Shift) then
  begin
    FPressed := True;
    FDoubleClick := True;
  end
  else if Button = TMouseButton.mbLeft then
  begin
    FPressed := True;
  end;
end;

procedure TControl3D.MouseMove3D(Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y, RayPos, RayDir);
end;

procedure TControl3D.MouseUp3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
begin
  if FAutoCapture then
    ReleaseCapture;
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y, RayPos, RayDir);
end;

procedure TControl3D.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, Shift, WheelDelta, Handled);
end;

procedure TControl3D.DragEnter(const Data: TDragObject; const Point: TPointF);
begin
  FIsDragOver := True;
  Repaint;
  TAnimator.StartTriggerAnimation(Self, Self, 'IsDragOver');
  if Assigned(OnDragEnter) then
    OnDragEnter(Self, Data, NullPoint3D);
end;

procedure TControl3D.DragLeave;
begin
  FIsDragOver := False;
  Repaint;
  TAnimator.StartTriggerAnimation(Self, Self, 'IsDragOver');
  if Assigned(OnDragLeave) then
    OnDragLeave(Self);
end;

procedure TControl3D.DragEnd;
begin
  // Call mouse up - for effects - inside control
  if DragMode = TDragMode.dmAutomatic then
    MouseUp3D(TMouseButton.mbLeft, [ssLeft], $FFFF, $FFFF, Vector3D($FFFF, $FFFF, 0), Vector3D(1, 0, 0));
  if Assigned(OnDragEnd) then
    OnDragEnd(Self);
end;

procedure TControl3D.DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  if Assigned(OnDragOver) then
    OnDragOver(Self, Data, NullPoint3D, Operation);
end;

procedure TControl3D.BeginUpdate;
var
  I: Integer;
  Control: IControl;
begin
  FUpdating := FUpdating + 1;
  if Children <> nil then
    for I := 0 to Children.Count - 1 do
      if Supports(Children[I], IControl, Control) then
        Control.BeginUpdate;
end;

procedure TControl3D.EndUpdate;
var
  I: Integer;
  Control: IControl;
begin
  FUpdating := FUpdating - 1;
  if Children <> nil then
    for I := 0 to ChildrenCount - 1 do
      if Supports(Children[I], IControl, Control) then
        Control.EndUpdate;
  if FUpdating = 0 then
    RebuildRenderingList;
end;

function TControl3D.EnterChildren(AObject: IControl): Boolean;
begin
  Result := False;
end;

function TControl3D.ExitChildren(AObject: IControl): Boolean;
begin
  Result := False;
end;

procedure TControl3D.DragDrop(const Data: TDragObject; const Point: TPointF);
begin
  FIsDragOver := False;
  Repaint;
  TAnimator.StartTriggerAnimation(Self, Self, 'IsDragOver');
  if Assigned(OnDragDrop) then
    OnDragDrop(Self, Data, NullPoint3D);
end;

{ properties }

procedure TControl3D.SetTempContext(const Value: TContext3D);
var
  i: Integer;
begin
  FTempContext := Value;
  if (Children <> nil) and (Children.Count > 0) then
    for i := 0 to Children.Count - 1 do
      if (Children[i] is TControl3D) then
        TControl3D(Children[i]).TempContext := Value;
end;

procedure TControl3D.SetHitTest(const Value: Boolean);
begin
  FHitTest := Value;
end;

procedure TControl3D.SetAcceptsControls(const Value: boolean);
begin
  FAcceptsControls := Value;
end;

procedure TControl3D.SetCursor(const Value: TCursor);
var
  CursorService: IFMXCursorService;
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    if FCursor <> crDefault then
      RefreshInheritedCursor
    else
    begin
      if Parent <> nil then
        RefreshInheritedCursor
      else
        FInheritedCursor := crDefault;
    end;

    if IsMouseOver and not (csLoading in ComponentState) and not (csDesigning in ComponentState) and
      TPlatformServices.Current.SupportsPlatformService(IFMXCursorService, CursorService) then
      CursorService.SetCursor(FInheritedCursor);
  end;
end;

procedure TControl3D.SetParent(const Value: TFmxObject);
begin
  inherited;
  RefreshInheritedCursor;
end;

procedure TControl3D.SetProjection(const Value: TProjection);
const
  Fit: single = 25;
var
  i: Integer;
begin
  if FProjection <> Value then
  begin
    FProjection := Value;
    if Children <> nil then
      for i := 0 to Children.Count - 1 do
        if (Children[i] is TControl3D) then
          TControl3D(Children[i]).Projection := Value;
    if not (csLoading in ComponentState) then
    begin
      if FProjection = TProjection.Screen then
      begin
        SetSize(Fit * Width, Fit * Height, Fit * Depth);
        if (FViewport <> nil) and (FViewport.Context <> nil) then
          Position.Point := Point3D(FViewport.Context.Width / 2, FViewport.Context.Height / 2, 0);
      end
      else
      begin
        SetSize(Width / Fit, Height / Fit, Depth / Fit);
        Position.Point := Point3D(0, 0, 0);
      end;
      Repaint;
    end;
  end;
end;

procedure TControl3D.SetSize(const AWidth, AHeight, ADepth: single);
begin
  if (FWidth <> AWidth) or (FHeight <> AHeight) or (FDepth <> ADepth) then
  begin
    FWidth := AWidth;
    FHeight := AHeight;
    FDepth := ADepth;
    Resize3D;
    if not(csLoading in ComponentState) then
      Repaint;
  end;
end;

procedure TControl3D.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    if FVisible then
      Repaint;
    FVisible := Value;
    if FVisible then
      Repaint;
    if FVisible then
      TAnimator.StartTriggerAnimation(Self, Self, 'IsVisible')
    else if FIsFocused and (Root <> nil) then
      Root.SetFocused(nil);
  end;
end;

procedure TControl3D.SetHeight(const Value: Single);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Resize3D;
    if (FHeight < 0) and (csDesigning in ComponentState) then
    begin
      FHeight := abs(FHeight);
      FScale.Y := -FScale.Y;
    end;
    if not (csLoading in ComponentState) then
      Repaint;
  end;
end;

procedure TControl3D.SetWidth(const Value: Single);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Resize3D;
    if (FWidth < 0) and (csDesigning in ComponentState) then
    begin
      FWidth := abs(FWidth);
      FScale.X := -FScale.X;
    end;
    if not(csLoading in ComponentState) then
      Repaint;
  end;
end;

procedure TControl3D.SetDepth(const Value: Single);
begin
  if FDepth <> Value then
  begin
    FDepth := Value;
    Resize3D;
    if (FDepth < 0) and (csDesigning in ComponentState) then
    begin
      FDepth := abs(FDepth);
      FScale.Z := -FScale.Z;
    end;
    if not (csLoading in ComponentState) then
      Repaint;
  end;
end;

function TControl3D.IsOpacityStored: Boolean;
begin
  Result := FOpacity <> 1;
end;

procedure TControl3D.SetZWrite(const Value: Boolean);
begin
  if FZWrite <> Value then
  begin
    FZWrite := Value;
    Repaint;
  end;
end;

procedure TControl3D.SetDragMode(const ADragMode: TDragMode);
begin
  FDragMode := ADragMode;
end;

procedure TControl3D.SetOpacity(const Value: Single);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    if FOpacity < 0 then
      FOpacity := 0;
    if FOpacity > 1 then
      FOpacity := 1;
    RecalcOpacity;
    Repaint;
  end;
end;

procedure TControl3D.SetOpaque(const Value: Boolean);
begin
  if FOpaque <> Value then
  begin
    FOpaque := Value;
    Repaint;
  end;
end;

{ TCamera }

constructor TCamera.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := False;
  FCanResize := False;
  FAngleOfView := 45;
  Position.Point := Point3D(0, 0, -5);
end;

destructor TCamera.Destroy;
begin
  inherited;
end;

procedure TCamera.RecalcAbsolute;
begin
  inherited;
end;

procedure TCamera.Render;
begin
  if Tag = $FFFE then
    Exit;
  if (csDesigning in ComponentState) then
  begin
    Context.FillCube(TPoint3D.Create(0, 0,   0), TPoint3D.Create(0.8, 0.8, 0.8), 1, $FF60A799);
    Context.FillCube(TPoint3D.Create(0, 0, 0.5), TPoint3D.Create(0.3, 0.3, 1.4), 1, $FF9C60A7);
    Context.DrawLine(TPoint3D.Create(0, 0,   0), TPoint3D.Create(0, 0, 1) * 1000, 1, $FF9C60A7);
  end;
end;

function TCamera.DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean;
begin
  Result := False;
  if (csDesigning in ComponentState) then
    Result := inherited;
end;

function TCamera.GetCameraMatrix: TMatrix3D;
begin
  if FTarget <> nil then
    Result := TMatrix3D.CreateLookAtDirRH(TPoint3D(AbsolutePosition), TPoint3D(AbsolutePosition) -
      TPoint3D(Target.AbsolutePosition), - TPoint3D(AbsoluteUp))
  else
    Result := TMatrix3D.CreateLookAtDirRH(TPoint3D(AbsolutePosition), - TPoint3D(AbsoluteDirection),
      - TPoint3D(AbsoluteUp));
end;

procedure TCamera.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FTarget) then
  begin
    FTarget := nil;
    MatrixChanged(Self);
  end;
end;

procedure TCamera.SetAngleOfView(const Value: Single);
begin
  if FAngleOfView <> Value then
  begin
    FAngleOfView := Value;

    if FAngleOfView < 1 then
      FAngleOfView := 1;
    if FAngleOfView > 179 then
      FAngleOfView := 179;

    MatrixChanged(Self);
  end;
end;

procedure TCamera.SetTarget(const Value: TControl3D);
begin
  if FTarget <> Value then
  begin
    FTarget := Value;
    MatrixChanged(Self);
  end;
end;

{ TLight }

constructor TLight.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := False;
  FEnabled := True;
  FCanResize := False;
  FSpotCutOff := 180;
  FSpotExponent := 0;
  FColor := claWhite;
end;

destructor TLight.Destroy;
begin
  inherited;
end;

procedure TLight.DefineProperties(Filer: TFiler);
begin
  inherited;
  // Only for backward compatibility with XE2
  Filer.DefineProperty('Diffuse', ReadDiffuse, nil, False);
  Filer.DefineProperty('Ambient', SkipColor, nil, False);
  Filer.DefineProperty('Specular', SkipColor, nil, False);
  Filer.DefineProperty('ConstantAttenuation', SkipFloat, nil, False);
  Filer.DefineProperty('LinearAttenuation', SkipFloat, nil, False);
  Filer.DefineProperty('QuadraticAttenuation', SkipFloat, nil, False);
end;

procedure TLight.ReadDiffuse(Reader: TReader);
var
  LColor: Integer;
begin
  IdentToAlphaColor(Reader.ReadIdent, LColor);
  {$R-}
  Color := TAlphaColor(LColor);
  {$R+}
end;

procedure TLight.SkipColor(Reader: TReader);
begin
  Reader.ReadIdent; // just skip
end;

procedure TLight.SkipFloat(Reader: TReader);
begin
  Reader.ReadFloat; // just skip
end;

procedure TLight.Render;
var
  i: Integer;
begin
  if (csDesigning in ComponentState) then
  begin
    if not Enabled then
    begin
      Context.FillCube(NullPoint3D, TPoint3D.Create(Width, Height, Depth) * 0.8, 1, TAlphaColors.Gray);
      Context.DrawCube(NullPoint3D, TPoint3D.Create(Width, Height, Depth) * 0.8, 1, TAlphaColors.Navy);
    end
    else
    begin
      Context.FillCube(NullPoint3D, TPoint3D.Create(Width, Height, Depth) * 0.8, 1, TAlphaColors.Yellow);
      Context.DrawCube(NullPoint3D, TPoint3D.Create(Width, Height, Depth) * 0.8, 1, TAlphaColors.Navy);
      case LightType of
        TLightType.Directional:
          begin
            Context.DrawLine(NullPoint3D, TPoint3D.Create(0, 0, 1) * (Width * 5), 1, TAlphaColors.Navy);
          end;
        TLightType.Point:
          begin
            for i := 1 to 18 do
              Context.DrawLine(NullPoint3D,
                TPoint3D.Create(cos(DegToRad(i * 20)), sin(DegToRad(i * 20)), 0) * (Width * 2), 1, TAlphaColors.Navy);
            for i := 1 to 18 do
              Context.DrawLine(NullPoint3D, TPoint3D.Create(cos(DegToRad(i * 20)), 0,
                sin(DegToRad(i * 20))) * (Width * 2), 1, TAlphaColors.Navy);
          end;
        TLightType.Spot:
          begin
            Context.DrawLine(NullPoint3D, TPoint3D.Create( 0.2, -0.2, 1).Normalize * (Width * 5), 1, TAlphaColors.Navy);
            Context.DrawLine(NullPoint3D, TPoint3D.Create( 0.2,  0.2, 1).Normalize * (Width * 5), 1, TAlphaColors.Navy);
            Context.DrawLine(NullPoint3D, TPoint3D.Create(-0.2,  0.2, 1).Normalize * (Width * 5), 1, TAlphaColors.Navy);
            Context.DrawLine(NullPoint3D, TPoint3D.Create(-0.2, -0.2, 1).Normalize * (Width * 5), 1, TAlphaColors.Navy);
          end;
      end;
    end;
  end;
end;

function TLight.DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean;
begin
  Result := False;
  if (csDesigning in ComponentState) then
    Result := inherited;
end;

function TLight.GetLightDescription: TLightDescription;
begin
  Result := TLightDescription.Create(Enabled, Color, LightType, SpotCutOff, SpotExponent, TPoint3D(AbsolutePosition),
    TPoint3D(AbsoluteDirection));
end;

procedure TLight.SetDepth(const Value: Single);
begin
  if Projection = TProjection.Camera then
    inherited SetDepth(1)
  else
    inherited SetDepth(25);
end;

procedure TLight.SetColor(const Value: TAlphaColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
  end;
end;

procedure TLight.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if FViewport <> nil then
      FViewport.NeedRender;
  end;
end;

procedure TLight.SetHeight(const Value: Single);
begin
  if Projection = TProjection.Camera then
    inherited SetHeight(1)
  else
    inherited SetHeight(25);
end;

procedure TLight.SetLightType(const Value: TLightType);
begin
  if FLightType <> Value then
  begin
  FLightType := Value;
    case FLightType of
      TLightType.Point:
        begin
          SpotCutOff := 180;
          SpotExponent := 0;
        end;
      TLightType.Spot:
        begin
          SpotCutOff := 60;
          SpotExponent := 4;
        end;
    end;
  end;
end;

procedure TLight.SetSpotCutOff(const Value: Single);
begin
  if FSpotCutOff <> Value then
  begin
    FSpotCutOff := Value;
  end;
end;

procedure TLight.SetSpotExponent(const Value: Single);
begin
  if FSpotExponent <> Value then
  begin
    FSpotExponent := Value;
  end;
end;

procedure TLight.SetNewViewport(AViewport: IViewport3D);
begin
  if FViewport <> nil then
    FViewport.RemoveLight(Self);
  inherited;
  if FViewport <> nil then
    FViewport.AddLight(Self);
end;

procedure TLight.SetWidth(const Value: Single);
begin
  if Projection = TProjection.Camera then
    inherited SetWidth(1)
  else
    inherited SetWidth(25);
end;

{ TRenderingCompare }

function TRenderingCompare.Compare(const Left, Right: TControl3D): Integer;
var
  LeftPosition, RightPosition: TPoint3D;
  LeftMaterial, RightMaterial: Pointer;
  LeftLength: Single;
  RightLenght: Single;
  IsLeftLengthNan: Boolean;
  IsRightLengthNan: Boolean;
begin
  LeftMaterial := Left.GetMaterialForSorting;
  RightMaterial := Right.GetMaterialForSorting;
  if LeftMaterial = RightMaterial then
  begin
    Result := 0;
    if Left.ZWrite and Right.ZWrite then
      if (Left.Opacity < 1) and (Right.Opacity >= 1) then
        Result := 1
      else if (Left.Opacity >= 1) and (Right.Opacity < 1) then
        Result := -1
      else if (Left.Context <> nil) and (Right.Context <> nil) then
        begin
          LeftPosition := TPoint3D(Left.AbsolutePosition) - TPoint3D(Left.Context.CurrentCameraMatrix.M[3]);
          RightPosition := TPoint3D(Right.AbsolutePosition) - TPoint3D(Left.Context.CurrentCameraMatrix.M[3]);
          LeftLength := LeftPosition.Length;
          RightLenght := RightPosition.Length;
          IsLeftLengthNan := IsNan(LeftLength);
          IsRightLengthNan := IsNan(RightLenght);
          // There are can be NaN values if controls have 0 scales, we need to processing it correctly.
          if IsLeftLengthNan and IsRightLengthNan then
            Result := 0
          else if not IsLeftLengthNan and IsRightLengthNan then
            Result := 1
          else if IsLeftLengthNan and not IsRightLengthNan then
            Result := -1
          else
            Result := Trunc(RightLenght - LeftLength);
        end;
  end
  else if NativeUInt(LeftMaterial) < NativeUInt(RightMaterial) then
    Result := -1
  else
    Result := 1;
end;

{ TProxyObject }

constructor TProxyObject.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TProxyObject.Destroy;
begin
  inherited;
end;

procedure TProxyObject.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSourceObject) then
    SourceObject := nil;
end;

procedure TProxyObject.Render;

  procedure RecalcChildAbsoluteMatrix;
  var
    I: Integer;
    LObject: TFmxObject;
  begin
    for I := 0 to FSourceObject.ChildrenCount - 1 do
    begin
      LObject := FSourceObject.Children[I];
      if (LObject <> Self) and (LObject is TControl3D) then
        TControl3D(LObject).RecalcAbsolute;
    end;
  end;

var
  LSaveMatrix: TMatrix3D;
  LSaveSize: TVector3D;
begin
  if (FDisableRendering = False) and (FSourceObject <> nil) then
  begin
    LSaveMatrix := FSourceObject.FAbsoluteMatrix;
    LSaveSize.X := FSourceObject.FWidth;
    LSaveSize.Y := FSourceObject.FHeight;
    LSaveSize.Z := FSourceObject.FDepth;
    FSourceObject.FAbsoluteMatrix := AbsoluteMatrix;
    RecalcChildAbsoluteMatrix;

    FSourceObject.FWidth := FWidth;
    FSourceObject.FHeight := FHeight;
    FSourceObject.FDepth := FDepth;
    FDisableRendering := True;
    try
      FSourceObject.RenderInternal;
    finally
      FDisableRendering := False;
      FSourceObject.FAbsoluteMatrix := LSaveMatrix;
      FSourceObject.FWidth := LSaveSize.X;
      FSourceObject.FHeight := LSaveSize.Y;
      FSourceObject.FDepth := LSaveSize.Z;
      RecalcChildAbsoluteMatrix;
    end;
  end;
end;

procedure TProxyObject.SetSourceObject(const Value: TControl3D);
begin
  if (FSourceObject <> Value) and (Value <> Self) then
  begin
    FSourceObject := Value;
    Repaint;
  end;
end;

initialization
  RegisterFmxClasses([TPosition3D, TControl3D, TCamera, TLight], [TPosition3D]);
finalization
end.
