{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Controls;

{$IF SizeOf(LongInt) = 8}
  {$DEFINE LONGINT64}
{$ENDIF}

interface

{$SCOPEDENUMS ON}

uses
  System.Actions, System.Types, System.Rtti, System.UITypes, System.Generics.Collections, System.Generics.Defaults,
  System.Classes, System.SysUtils, System.UIConsts, System.Math, System.Messaging, System.Math.Vectors,
  FMX.ActnList, FMX.Types, FMX.Styles, FMX.VirtualKeyboard, FMX.TextLayout, FMX.Graphics, FMX.Effects, FMX.Ani,
  FMX.AcceleratorKey;

type
  TControl = class;
  TStyledControl = class;
  TStyleBook = class;

  IStyleBookOwner = interface
    ['{BA1AE6C6-FCF7-43E2-92AA-2869FF203309}']
    function GetStyleBook: TStyleBook;
    procedure SetStyleBook(const Value: TStyleBook);
    property StyleBook: TStyleBook read GetStyleBook write SetStyleBook;
  end;

  IScene = interface(IStyleBookOwner)
    ['{16DB110E-DA7D-4e75-BC2D-999FA12E45F5}']
    procedure AddUpdateRect(R: TRectF);
    function GetUpdateRectsCount: Integer;
    function GetUpdateRect(const Index: Integer): TRectF;
    function GetObject: TFmxObject;
    function GetCanvas: TCanvas;
    function GetSceneScale: Single;
    function LocalToScreen(P: TPointF): TPointF;
    function ScreenToLocal(P: TPointF): TPointF;
    procedure ChangeScrollingState(const AControl: TControl; const Active: Boolean);
    /// <summary>Disable Scene's updating</summary>
    procedure DisableUpdating;
    /// <summary>Enable Scene's updating</summary>
    procedure EnableUpdating;
    property Canvas: TCanvas read GetCanvas;
  end;

  /// <summary>Exception raised when the process for Disabling Updating and
  /// Enabling Upating is not correct.</summary>
  EInvalidSceneUpdatingPairCall = class(Exception);

  { IDesignerControl: Control implementing this is part of the designer }
  IDesignerControl = interface
    ['{C57A701D-E4B5-4711-BFA4-716E2164A929}']
  end;

  /// <summary>Controls that can respond to hint-related events must implement
  /// this interface.</summary>
  IHintReceiver = interface
    ['{533671CF-86C5-489E-B32A-724AF8464DCE}']
    /// <summary>This method is called when a hint is triggered.</summary>
    procedure TriggerOnHint;
  end;

  /// <summary>A class needs to implement this interface in order to be able to
  /// register IHintReceiver instances.</summary>
  IHintRegistry = interface
    ['{8F3B3C46-450B-4A8C-800F-FD47538244C3}']
    /// <summary>Triggers the TriggerOnHint method of all the objects that are registered in this registry.</summary>
    procedure TriggerHints;
    /// <summary>Registers a new receiver.</summary>
    procedure RegisterHintReceiver(const AReceiver: IHintReceiver);
    /// <summary>Unregisters a receiver.</summary>
    procedure UnregisterHintReceiver(const AReceiver: IHintReceiver);
  end;

  /// <summary>The base class for an object that can manage a hint.</summary>
  THint = class
  public type
    THintClass = class of THint;
  private class var
    FClassRegistry: TArray<THintClass>;
  protected
    /// <summary>Field to store the hint.</summary>
    FHint: string;
    /// <summary>Field to store the status (enabled or not) of the hint.</summary>
    FEnabled: Boolean;
    /// <summary>Method that updates the state of enabled.</summary>
    procedure SetEnabled(const Value: Boolean); virtual;
  public
    /// <summary>Constructor. A constructor needs the native handle of the view that holds the hint. To give an example,
    /// in MS Windows is the HWND of the native window.</summary>
    constructor Create(const AHandle: TWindowHandle); virtual;
    /// <summary>Sets the full hint string.</summary>
    procedure SetHint(const AString: string); virtual;
    /// <summary>Gets the full hint string.</summary>
    function GetHint: string;
    /// <summary>The hint can follows the following pattern:  'A short Text| A Long text'. It means, the hint can hold
    /// two texts separated by the '|' character. This method returns the short text of the hint.</summary>
    function GetShortText: string;
    /// <summary>Returns the long text of the hint.</summary>
    function GetLongText: string;
    /// <summary>If the specific implementation supports it, this metods places the hint in the given position.</summary>
    procedure SetPosition(const X, Y: Single); virtual; abstract;
    /// <summary>Register a class to create hint instances. When a new THint instance is needed, the registered classes are invoked
    /// to create the needed instance.</summary>
    class procedure RegisterClass(const AClass: THintClass);
    /// <summary>Returns an instance created by the first available registered class. This method can return nil if there are no classes 
    /// registered or none of the registered classes can create a THint instance.</summary>
    class function CreateNewInstance(const AHandle: TWindowHandle): THint;
    /// <summary>Returns True if there are some THint class registered.</summary>
    class function ContainsRegistredHintClasses: Boolean;

    /// <summary>If this property is true, the hint can be displayed, if it is False, the hint won't be displayed.</summary>
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

{ TCustomControlAction }
  TCustomControlAction = class(TCustomAction)
  private
    [weak]FPopupMenu: TCustomPopupMenu;
    procedure SetPopupMenu(const Value: TCustomPopupMenu);
  protected
    procedure Notification(AComponent: TComponent;  Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    property PopupMenu: TCustomPopupMenu read FPopupMenu write SetPopupMenu;
  end;

{ TControlAction }

  TControlAction = class(TCustomControlAction)
  published
    property AutoCheck;
    property Text;
    property Checked;
    property Enabled;
    property GroupIndex;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property UnsupportedArchitectures;
    property UnsupportedPlatforms;
    property OnExecute;
    property OnHint;
    property OnUpdate;
    property PopupMenu;
  end;

{ TControlActionLink }
  /// <summary>Links an action to a client (generic control).</summary>
  TControlActionLink = class(FMX.ActnList.TActionLink)
  private
    function GetClient: TControl;
  protected
    procedure AssignClient(AClient: TObject); override;
    function IsEnabledLinked: Boolean; override;
    function IsHelpLinked: Boolean;  override;
    function IsHintLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    function IsPopupMenuLinked: boolean; virtual;
    /// <summary>This method is invoked to allow a link to customize a Hint that is going to be displayed.</summary>
    function DoShowHint(var HintStr: string): Boolean; virtual;
    /// <summary>This method sets the string of the hint.</summary>
    procedure SetHint(const Value: string); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHelpContext(Value: THelpContext); override;
    procedure SetHelpKeyword(const Value: string); override;
    procedure SetHelpType(Value: THelpType); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
    procedure SetPopupMenu(const Value: TCustomPopupMenu); virtual;
  public
    property Client: TControl read GetClient;
  end;

  TCaret = class (TCustomCaret)
  private const
    FMXFlasher = 'FMXFlasher';
  public
    class function FlasherName: string; override;
  published
    property Color;
    property Interval;
    property Width;
  end;

{ TControl }

  TEnumControlsResult = TEnumProcResult;

  TEnumControlsRef = reference to procedure(const AControl: TControl; var Done: boolean);

  TControlList = TList<TControl>;

  TOnPaintEvent = procedure(Sender: TObject; Canvas: TCanvas; const ARect: TRectF) of object;

  TPaintStage = (All, Background, Text);

  TPaintStageHelper = record helper for TPaintStage
  const
    psAll = TPaintStage.All deprecated 'Use TPaintStage.All';
    psBackground = TPaintStage.Background deprecated 'Use TPaintStage.Background';
    psText = TPaintStage.Text deprecated 'Use TPaintStage.Text';
  end;

  TControlType = (Styled, Platform);

  /// <summary>Helper for TControlType.</summary>
  TControlTypeHelper = record helper for TControlType
  public
    /// <summary>Returns string presentation of value of this type</summary>
    function ToString: string;
  end;

  TControl = class(TFmxObject, IControl, IContainerObject, IAlignRoot, IRotatedControl, IAlignableObject,
    IEffectContainer, IGestureControl, ITabStopController, ITriggerAnimation, ITriggerEffect)
  private type
    TDelayedEvent = (Resize, Resized);
  private const
    InitialControlsCapacity = 10;
  public const
    DefaultTouchTargetExpansion = 6;
    DefaultDisabledOpacity = 0.6;
    DesignBorderColor = $A0909090;
  private class var
    FPaintStage: TPaintStage;
  strict private
    FOnMouseUp: TMouseEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FHitTest: Boolean;
    FClipChildren: Boolean;
    FAutoCapture: Boolean;
    FPadding: TBounds;
    FMargins: TBounds;
    FTempCanvas: TCanvas;
    FRotationAngle: Single;
    FPosition: TPosition;
    FScale: TPosition;
    FSkew: TPosition;
    FRotationCenter: TPosition;
    FCanFocus: Boolean;
    FOnCanFocus: TCanFocusEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FClipParent: Boolean;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnPaint: TOnPaintEvent;
    FOnPainting: TOnPaintEvent;
    FCursor: TCursor;
    FInheritedCursor: TCursor;
    FDragMode: TDragMode;
    FEnableDragHighlight: Boolean;
    FOnDragEnter: TDragEnterEvent;
    FOnDragDrop: TDragDropEvent;
    FOnDragLeave: TNotifyEvent;
    FOnDragOver: TDragOverEvent;
    FOnDragEnd: TNotifyEvent;
    FIsDragOver: Boolean;
    FOnKeyDown: TKeyEvent;
    FOnKeyUp: TKeyEvent;
    FOnTap: TTapEvent;
    FHint: string;
    FActionHint: string;
    FShowHint: Boolean;
    FPopupMenu: TCustomPopupMenu;
    FRecalcEnabled, FEnabled, FAbsoluteEnabled: Boolean;
    FTabList: TTabList;
    FOnResize: TNotifyEvent;
    FOnResized: TNotifyEvent;
    FDisableEffect: Boolean;
    FAcceptsControls: Boolean;
    FControls: TControlList;
    FEnableExecuteAction: Boolean;
    FCanParentFocus: Boolean;
    FMinClipHeight: Single;
    FMinClipWidth: Single;
    FSmallSizeControl: Boolean;
    FTouchTargetExpansion: TBounds;
    FOnDeactivate: TNotifyEvent;
    FOnActivate: TNotifyEvent;
    FSimpleTransform: Boolean;
    FFixedSize: TSize;
    FEffects: TList<TEffect>;
    FDisabledOpacity: Single;
    [Weak] FParentControl: TControl;
    FParentContent: IContent;
    FUpdateRect: TRectF;
    FTabStop: Boolean;
    FDisableDisappear: Integer;
    FAnchorMove: Boolean;
    FApplyingEffect: Boolean;
    FExitingOrEntering: Boolean;
    FDelayedEvents: set of TDelayedEvent;
    procedure AddToEffectsList(const AEffect: TEffect);
    procedure RemoveFromEffectsList(const AEffect: TEffect);
    class var FEmptyControlList: TControlList;
    function GetInvertAbsoluteMatrix: TMatrix;
    procedure SetPosition(const Value: TPosition);
    procedure SetHitTest(const Value: Boolean);
    procedure SetClipChildren(const Value: Boolean);
    function GetCanvas: TCanvas; inline;
    procedure SetLocked(const Value: Boolean);
    procedure SetTempCanvas(const Value: TCanvas);
    procedure SetOpacity(const Value: Single);
    function IsOpacityStored: Boolean;
    procedure SetCursor(const Value: TCursor);
    procedure RefreshInheritedCursor;
    procedure RefreshInheritedCursorForChildren;
    function GetAbsoluteWidth: Single;
    function GetAbsoluteHeight: Single;
    function IsAnchorsStored: Boolean;
    function GetEnabled: Boolean;
    function GetCursor: TCursor;
    function GetInheritedCursor: TCursor;
    function GetAbsoluteHasEffect: Boolean;
    function GetAbsoluteHasDisablePaintEffect: Boolean;
    function GetAbsoluteHasAfterPaintEffect: Boolean;
    procedure PaddingChangedHandler(Sender: TObject); overload;
    procedure MarginsChanged(Sender: TObject);
    procedure MatrixChanged(Sender: TObject);
    procedure SizeChanged(Sender: TObject);
    function GetControlsCount: Integer;
    function OnClickStored: Boolean;
    function IsPopupMenuStored: Boolean;
    procedure RequestAlign;
    procedure SetMinClipHeight(const Value: Single);
    procedure SetMinClipWidth(const Value: Single);
    function UpdateSmallSizeControl: Boolean;
    class constructor Create;
    class destructor Destroy;
    procedure SetOnClick(const Value: TNotifyEvent);
    function GetIsFocused: Boolean;
    procedure SetPadding(const Value: TBounds);
    procedure SetMargins(const Value: TBounds);
    procedure SetTouchTargetExpansion(const Value: TBounds);
    procedure InternalSizeChanged;
    procedure ReadFixedWidth(Reader: TReader);
    procedure WriteFixedWidth(Writer: TWriter);
    procedure ReadFixedHeight(Reader: TReader);
    procedure WriteFixedHeight(Writer: TWriter);
    procedure ReadDesignVisible(Reader: TReader);
    procedure ReadHint(Reader: TReader);
    procedure ReadShowHint(Reader: TReader);
    function DisabledOpacityStored: Boolean;
    procedure SetDisabledOpacity(const Value: Single);
    function GetAxisAlignedRect: TRectF;
    { IRotatedControl }
    function GetRotationAngle: Single;
    function GetRotationCenter: TPosition;
    function GetScale: TPosition;
    procedure SetRotationAngle(const Value: Single);
    procedure SetRotationCenter(const Value: TPosition);
    procedure SetScale(const Value: TPosition);
    function GetTabOrder: TTabOrder;
    procedure SetTabOrder(const Value: TTabOrder);
    function GetTabStop: Boolean;
    procedure SetTabStop(const TabStop: Boolean);
    procedure SetDisableDisappear(const Value: Boolean);
    function GetDisableDisappear: Boolean;
    procedure UpdateParentProperties;
  private
    FInflated: Boolean;
    FOnApplyStyleLookup: TNotifyEvent;
    FAlign: TAlignLayout;
    FAnchors: TAnchors;
    FUpdateEffects: Boolean;
    FDisableFocusEffect: Boolean;
    FTouchManager: TTouchManager;
    FOnGesture: TGestureEvent;
    FVisible: Boolean;
    FPressed: Boolean;
    FPressedPosition: TPointF;
    FDoubleClick: Boolean;
    FParentShowHint: Boolean;
    function GetTouchManager: TTouchManager;
    procedure SetTouchManager(const Value: TTouchManager);
    function IsShowHintStored: Boolean;
    procedure SetParentShowHint(const Value: Boolean);
    procedure SetShowHint(const Value: Boolean);
    function GetAbsoluteClipRect: TRectF;
    function HintStored: Boolean;
  strict protected
    procedure RepaintJointArea(const DestControl: TObject);
  protected
    FScene: IScene;
    FLastHeight: Single;
    FLastWidth: Single;
    FSize: TControlSize;
    FLocalMatrix: TMatrix;
    FAbsoluteMatrix: TMatrix;
    FInvAbsoluteMatrix: TMatrix;
    FEffectBitmap: TBitmap;
    FLocked: Boolean;
    FOpacity, FAbsoluteOpacity: Single;
    FInPaintTo: Boolean;
    FInPaintToAbsMatrix, FInPaintToInvMatrix: TMatrix;
    FAbsoluteHasEffect: Boolean;
    FAbsoluteHasDisablePaintEffect: Boolean;
    FAbsoluteHasAfterPaintEffect: Boolean;
    FUpdating: Integer;
    FNeedAlign: Boolean;
    FDisablePaint: Boolean;
    FDisableAlign: Boolean;
    FRecalcOpacity: Boolean;
    FRecalcUpdateRect: Boolean;
    FRecalcAbsolute: Boolean;
    FRecalcHasEffect: Boolean;
    FHasClipParent: TControl;
    FRecalcHasClipParent: Boolean;
    FDesignInteractive: Boolean;
    FDesignSelectionMarks: Boolean;
    FIsMouseOver: Boolean;
    FIsFocused: Boolean;
    { added for aligment using a relation between align and anchors}
    FAnchorRules: TPointF;
    FAnchorOrigin: TPointF;
    FOriginalParentSize: TPointF;
    FLeft: Single;
    FTop: Single;
    FExplicitLeft: Single;
    FExplicitTop: Single;
    FExplicitWidth: Single;
    FExplicitHeight: Single;
    procedure DoAbsoluteChanged; virtual;
    function CheckHitTest(const AHitTest: Boolean): Boolean;
    procedure SetInPaintTo(Value: Boolean);
    procedure EndUpdateNoChanges;
    /// <summary>This method sets the hint string for this control.</summary>
    procedure SetHint(const AHint: string); virtual;
    { }
    procedure SetEnabled(const Value: Boolean); virtual;
    procedure Loaded; override;
    procedure Updated; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ParentChanged; override;
    procedure ChangeOrder; override;
    procedure ChangeChildren; override;
    procedure SetVisible(const Value: Boolean); virtual;
    function DoSetWidth(var Value: Single; NewValue: Single; var LastValue: Single): Boolean; virtual; deprecated 'Use DoSetSize';
    function DoSetHeight(var Value: Single; NewValue: Single; var LastValue: Single): Boolean; virtual; deprecated 'Use DoSetSize';
    function DoSetSize(const ASize: TControlSize; const NewPlatformDefault: Boolean; ANewWidth, ANewHeight: Single;
      var ALastWidth, ALastHeight: Single): Boolean; virtual;
    procedure HandleSizeChanged; virtual;
    { matrix }
    procedure DoMatrixChanged(Sender: TObject); virtual;
    procedure SetHeight(const Value: Single); virtual;
    procedure SetWidth(const Value: Single); virtual;
    procedure SetSize(const AValue: TControlSize); overload; virtual;
    procedure SetSize(const AWidth, AHeight: Single; const APlatformDefault: Boolean = False); overload; virtual;
    function GetAbsoluteRect: TRectF; virtual;
    function GetChildrenMatrix(var Matrix: TMatrix; var Simple: Boolean): Boolean; virtual;
    function GetAbsoluteScale: TPointF; virtual;
    function GetParentedRect: TRectF; virtual; deprecated 'Use GetBoundsRect';
    function GetClipRect: TRectF; virtual;
    function GetEffectsRect: TRectF; virtual;
    function GetAbsoluteEnabled: Boolean; virtual;
    function GetChildrenRect: TRectF; virtual;
    function GetLocalRect: TRectF; virtual;
    function GetBoundsRect: TRectF; virtual;
    procedure SetBoundsRect(const Value: TRectF); virtual;
    function IsHeightStored: Boolean; virtual; deprecated 'Use IsSizeStored';
    function IsWidthStored: Boolean; virtual; deprecated 'Use IsSizeStored';
    function IsPositionStored: Boolean; virtual;
    function IsSizeStored: Boolean; virtual;
    procedure SetPopupMenu(const Value: TCustomPopupMenu);
    { optimizations }
    function GetAbsoluteMatrix: TMatrix; virtual;
    function GetHasClipParent: TControl;
    function GetUpdateRect: TRectF;
    function DoGetUpdateRect: TRectF; virtual;
    { opacity }
    function GetAbsoluteOpacity: Single; virtual;
    { events }
    procedure BeginAutoDrag; virtual;
    procedure Capture;
    procedure ReleaseCapture;
    property EnableExecuteAction: boolean read FEnableExecuteAction write FEnableExecuteAction;
    procedure Click; virtual;
    procedure DblClick; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); virtual;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); virtual;
    procedure KeyUp(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); virtual;
    procedure DialogKey(var Key: Word; Shift: TShiftState); virtual;
    procedure AfterDialogKey(var Key: Word; Shift: TShiftState); virtual;
    function ShowContextMenu(const ScreenPosition: TPointF): Boolean; virtual;
    procedure DragEnter(const Data: TDragObject; const Point: TPointF); virtual;
    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation); virtual;
    procedure DragDrop(const Data: TDragObject; const Point: TPointF); virtual;
    procedure DragLeave; virtual;
    procedure DragEnd; virtual;
    function GetDefaultTouchTargetExpansion: TRectF; virtual;
    function GetCanFocus: Boolean; virtual;
    function GetCanParentFocus: Boolean; virtual;
    function EnterChildren(AObject: IControl): Boolean; virtual;
    function ExitChildren(AObject: IControl): Boolean; virtual;
    function GetParentedVisible: Boolean; virtual;
    { IEffectContainer }
    procedure NeedUpdateEffects;
    procedure BeforeEffectEnabledChanged(const Enabled: Boolean);
    procedure EffectEnabledChanged(const Enabled: Boolean);
    { IAlignRoot }
    procedure Realign;
    procedure ChildrenAlignChanged;
    { IAlignableObject }
    function GetAlign: TAlignLayout;
    procedure SetAlign(const Value: TAlignLayout); virtual;
    function GetAnchors: TAnchors;
    procedure SetAnchors(const Value: TAnchors); virtual;
    function GetMargins: TBounds;
    function GetPadding: TBounds;
    function GetWidth: Single; virtual;
    function GetHeight: Single; virtual;
    function GetLeft: Single; virtual;
    function GetTop: Single; virtual;
    function GetAllowAlign: Boolean;
    function GetAnchorRules : TPointF;
    function GetAnchorOrigin : TPointF;
    function GetOriginalParentSize : TPointF;
    function GetAnchorMove : Boolean;
    procedure SetAnchorMove(Value : Boolean);
    function GetAdjustSizeValue: TSizeF; virtual;
    function GetAdjustType: TAdjustType; virtual;
    { IContainerObject }
    function GetContainerWidth: Single;
    function GetContainerHeight: Single;
    { IControl }
    function IControl.GetObject = GetObject;
    function GetObject: TFmxObject;
    function GetParent: TFmxObject;
    function GetVisible: Boolean;
    function GetDesignInteractive: Boolean;
    function GetPopupMenu: TCustomPopupMenu;
    procedure DoEnter; virtual;
    procedure DoExit; virtual;
    procedure DoActivate; virtual;
    procedure DoDeactivate; virtual;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    function CheckForAllowFocus: Boolean;
    function ScreenToLocal(P: TPointF): TPointF; virtual;
    function LocalToScreen(P: TPointF): TPointF; virtual;
    function GetDragMode: TDragMode; virtual;
    procedure SetDragMode(const ADragMode: TDragMode); virtual;
    function GetLocked: Boolean;
    function GetHitTest: Boolean;
    function GetAcceptsControls: Boolean;
    procedure SetAcceptsControls(const Value: boolean);
    function FindTarget(P: TPointF; const Data: TDragObject): IControl; virtual;
    function ObjectAtPoint(P: TPointF): IControl; virtual;
    /// <summary>Implementation of IControl.HasHint. See IControl for details.</summary>
    function HasHint: Boolean; virtual;
    /// <summary>Implementation of IControl.GetHintString. See IControl for details.</summary>
    function GetHintString: string; virtual;
    /// <summary>Implementation of IControl.GetHintObject. See IControl for details.</summary>
    function GetHintObject: TObject; virtual;
    { IGestureControl }
    procedure BroadcastGesture(EventInfo: TGestureEventInfo);
    procedure CMGesture(var EventInfo: TGestureEventInfo); virtual;
    function TouchManager: TTouchManager;
    function GetFirstControlWithGesture(AGesture: TInteractiveGesture): TComponent; virtual;
    function GetFirstControlWithGestureEngine: TComponent;
    function GetListOfInteractiveGestures: TInteractiveGestures;
    procedure Tap(const Point:TPointF); virtual;
    { optimization }
    function GetFirstVisibleObjectIndex: Integer; virtual;
    function GetLastVisibleObjectIndex: Integer; virtual;
    function GetDefaultSize: TSizeF; virtual;
    { bi-di }
    function FillTextFlags: TFillTextFlags; virtual;
    { paint internal }
    procedure ApplyEffect;
    procedure PaintInternal;
    function SupportsPaintStage(const Stage: TPaintStage): Boolean; virtual;
    { paint }
    function CanRepaint: Boolean; virtual;
    procedure RepaintRect(const Rect: TRectF);
    procedure PaintChildren; virtual;
    procedure Painting; virtual;
    procedure Paint; virtual;
    procedure DoPaint; virtual;
    procedure AfterPaint; virtual;
    procedure DrawDesignBorder(const VertColor: TAlphaColor = DesignBorderColor;
      const HorzColor: TAlphaColor = DesignBorderColor);
    { align }
    procedure DoRealign; virtual;
    procedure DoBeginUpdate; virtual;
    procedure DoEndUpdate; virtual;
    { changes }
    procedure Move; virtual;
    procedure Resize; virtual;
    procedure DoResized; virtual;
    procedure Disappear; virtual;
    procedure Show; virtual;
    procedure Hide; virtual;
    procedure AncestorVisibleChanged(const Visible: Boolean); virtual;
    /// <summary>Notification about changed parent of ancestor</summary>
    procedure AncestorParentChanged; virtual;
    procedure ClipChildrenChanged; virtual;
    procedure HitTestChanged; virtual;
    /// <summary>Notification about changed padding</summary>
    procedure PaddingChanged; overload; virtual;
    property MinClipWidth: Single read FMinClipWidth write SetMinClipWidth;
    property MinClipHeight: Single read FMinClipHeight write SetMinClipHeight;
    property SmallSizeControl: Boolean read FSmallSizeControl;
    { children }
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoInsertObject(Index: Integer; const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
    procedure DoDeleteChildren; override;
    { props }
    class property PaintStage: TPaintStage read FPaintStage write FPaintStage;
    property TempCanvas: TCanvas read FTempCanvas write SetTempCanvas;
    { added for aligment using a relation between align and anchors}
    procedure SetLeft(const Value: Single);
    procedure SetTop(const Value: Single);
    procedure UpdateExplicitBounds;
    procedure UpdateAnchorRules(const Anchoring: Boolean = False);
    property Left: Single read FLeft write SetLeft;
    property Top: Single read FTop write SetTop;
    property ExplicitLeft: Single read FExplicitLeft;
    property ExplicitTop: Single read FExplicitTop;
    property ExplicitWidth: Single read FExplicitWidth;
    property ExplicitHeight: Single read FExplicitHeight;
    function GetActionLinkClass: TActionLinkClass; override;
    procedure ActionChange(Sender: TBasicAction; CheckDefaults: Boolean); override;
    function EnabledStored: Boolean; virtual;
    function VisibleStored: Boolean; virtual;
    /// <summary> This method is called after change field <b>FEnabled</b> in <b>SetEnabled</b> before all other
    /// actions</summary>
    procedure EnabledChanged; virtual;
    /// <summary> This method is called after change field <b>FVisible</b> in <b>SetVisible</b> before all other
    /// actions</summary>
    procedure VisibleChanged; virtual;
    /// <summary>Returns True if the control rect is empty.</summary>
    function IsControlRectEmpty: Boolean; virtual;
    function GetControls: TControlList;
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); virtual;
    function GetTabStopController: ITabStopController; virtual;
    function GetTabListClass: TTabListClass; virtual;
    property DoubleClick: Boolean read FDoubleClick;
    { IRotatedControl }
    property RotationAngle: Single read GetRotationAngle write SetRotationAngle;
    property RotationCenter: TPosition read GetRotationCenter write SetRotationCenter;
    property Scale: TPosition read GetScale write SetScale;
    property DisabledOpacity: Single read FDisabledOpacity write SetDisabledOpacity stored DisabledOpacityStored nodefault;
    property ParentContent: IContent read FParentContent;
    /// <summary>If the control has ShowHint to false, this property is used to see if a hint can be displayed.</summary>
    property ParentShowHint: Boolean read FParentShowHint write SetParentShowHint default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetNewScene(AScene: IScene); virtual;
    procedure SetBounds(X, Y, AWidth, AHeight: Single); virtual;
    function AbsoluteToLocal(const Point: TPointF): TPointF; virtual;
    function LocalToAbsolute(const Point: TPointF): TPointF; virtual;
    function AbsoluteToLocalVector(Vector: TVector): TVector; virtual;
    function LocalToAbsoluteVector(Vector: TVector): TVector; virtual;
    function PointInObject(X, Y: Single): Boolean; virtual;
    function PointInObjectLocal(X, Y: Single): Boolean; virtual;
    { drag and drop }
    function MakeScreenshot: TBitmap;
    { align }
    procedure BeginUpdate; virtual;
    function IsUpdating: Boolean; virtual;
    procedure EndUpdate; virtual;
    { optimizations }
    procedure RecalcAbsoluteNow;
    procedure RecalcUpdateRect; virtual;
    procedure RecalcOpacity; virtual;
    procedure RecalcAbsolute; virtual;
    procedure RecalcEnabled; virtual;
    procedure RecalcHasEffect; virtual;
    procedure RecalcHasClipParent; virtual;
    procedure PrepareForPaint; virtual;
    procedure RecalcSize; virtual;
    { effects }
    procedure UpdateEffects;
    { ITriggerEffect }
    procedure ApplyTriggerEffect(const AInstance: TFmxObject; const ATrigger: string); virtual;
    { ITriggerAnimation }
    procedure StartTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string); virtual;
    procedure StartTriggerAnimationWait(const AInstance: TFmxObject; const ATrigger: string); virtual;
    { }
    procedure SetFocus;
    procedure ResetFocus;
    procedure PaintTo(const ACanvas: TCanvas; const ARect: TRectF; const AParent: TFmxObject = nil);
    procedure Repaint;
    procedure InvalidateRect(ARect: TRectF);
    procedure Lock;
    property AbsoluteMatrix: TMatrix read GetAbsoluteMatrix;
    property AbsoluteOpacity: Single read GetAbsoluteOpacity;
    property AbsoluteWidth: Single read GetAbsoluteWidth;
    property AbsoluteHeight: Single read GetAbsoluteHeight;
    property AbsoluteScale: TPointF read GetAbsoluteScale;
    property AbsoluteEnabled: Boolean read GetAbsoluteEnabled;
    property AbsoluteRect: TRectF read GetAbsoluteRect;
    /// <summary> The absolute rectangle of control after clipping by all its parent controls </summary>
    property AbsoluteClipRect: TRectF read GetAbsoluteClipRect;
    property AxisAlignedRect: TRectF read GetAxisAlignedRect;
    /// <summary>Flag property indicates when control in applying effect state.</summary>
    property ApplyingEffect: Boolean read FApplyingEffect;
    property HasEffect: Boolean read GetAbsoluteHasEffect;
    property HasDisablePaintEffect: Boolean read GetAbsoluteHasDisablePaintEffect;
    property HasAfterPaintEffect: Boolean read GetAbsoluteHasAfterPaintEffect;
    property HasClipParent: TControl read GetHasClipParent;
    property ChildrenRect: TRectF read GetChildrenRect;
    property DefaultSize: TSizeF read GetDefaultSize;
    property FixedSize: TSize read FFixedSize write FFixedSize;
    property InvertAbsoluteMatrix: TMatrix read GetInvertAbsoluteMatrix;
    property InPaintTo: Boolean read FInPaintTo;
    property LocalRect: TRectF read GetLocalRect;
    property Pressed: Boolean read FPressed write FPressed;
    ///<summary>Point when MouseDown is called</summary>
    property PressedPosition: TPointF read FPressedPosition write FPressedPosition;
    property UpdateRect: TRectF read GetUpdateRect;
    property BoundsRect: TRectF read GetBoundsRect write SetBoundsRect;
    {$WARN SYMBOL_DEPRECATED OFF}
    property ParentedRect: TRectF read GetParentedRect;
    {$WARN SYMBOL_DEPRECATED DEFAULT}
    property ParentedVisible: Boolean read GetParentedVisible;
    property ClipRect: TRectF read GetClipRect;
    property Canvas: TCanvas read GetCanvas;
    property Controls: TControlList read GetControls;
    property ControlsCount: Integer read GetControlsCount;
    property ParentControl: TControl read FParentControl;
    property Scene: IScene read FScene;
    property AutoCapture: Boolean read FAutoCapture write FAutoCapture default False;
    property CanFocus: Boolean read FCanFocus write FCanFocus default False;
    property CanParentFocus: Boolean read FCanParentFocus write FCanParentFocus default False;
    property DisableFocusEffect: Boolean read FDisableFocusEffect write FDisableFocusEffect default False;
    property IsInflated: Boolean read FInflated;
    procedure EnumControls(const Proc: TFunc<TControl, TEnumControlsResult>); overload;
    function EnumControls(Proc: TEnumControlsRef; const VisibleOnly: Boolean = True): Boolean; overload;
      deprecated 'Use another version of EnumControls';
    { ITabStopController }
    function GetTabList: ITabList; virtual;
    function ShowInDesigner: Boolean; virtual;
    /// <summary>False if the control should be ignored in ObjectAtPoint, normally same as Visible.
    /// TFrame overrides it to allow itself to be painted in design time regardless of its Visible value.</summary>
    function ShouldTestMouseHits: Boolean; virtual;
    { triggers }
    property IsMouseOver: Boolean read FIsMouseOver;
    property IsDragOver: Boolean read FIsDragOver;
    property IsFocused: Boolean read GetIsFocused;
    property IsVisible: Boolean read FVisible;
    property Align: TAlignLayout read FAlign write SetAlign default TAlignLayout.None;
    property Anchors: TAnchors read FAnchors write SetAnchors stored IsAnchorsStored nodefault;
    property Cursor: TCursor read GetCursor write SetCursor default crDefault;
    property InheritedCursor: TCursor read GetInheritedCursor default crDefault;
    property DragMode: TDragMode read GetDragMode write SetDragMode default TDragMode.dmManual;
    property EnableDragHighlight: Boolean read FEnableDragHighlight write FEnableDragHighlight default True;
    property Enabled: Boolean read FEnabled write SetEnabled stored EnabledStored nodefault;
    property Position: TPosition read FPosition write SetPosition stored IsPositionStored;
    property Locked: Boolean read FLocked write SetLocked default False;
    property Width: Single read GetWidth write SetWidth stored False nodefault;
    property Height: Single read GetHeight write SetHeight stored False nodefault;
    property Size: TControlSize read FSize write SetSize stored IsSizeStored nodefault;
    property Padding: TBounds read GetPadding write SetPadding;
    property Margins: TBounds read GetMargins write SetMargins;
    property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
    property ClipChildren: Boolean read FClipChildren write SetClipChildren default False;
    property ClipParent: Boolean read FClipParent write FClipParent default False;
    property HitTest: Boolean read FHitTest write SetHitTest default True;
    property PopupMenu: TCustomPopupMenu read FPopupMenu write SetPopupMenu stored IsPopupMenuStored;
    property TabOrder: TTabOrder read GetTabOrder write SetTabOrder default -1;
    property Visible: Boolean read FVisible write SetVisible stored VisibleStored default True;
    property OnDragEnter: TDragEnterEvent read FOnDragEnter write FOnDragEnter;
    property OnDragLeave: TNotifyEvent read FOnDragLeave write FOnDragLeave;
    property OnDragOver: TDragOverEvent read FOnDragOver write FOnDragOver;
    property OnDragDrop: TDragDropEvent read FOnDragDrop write FOnDragDrop;
    property OnDragEnd: TNotifyEvent read FOnDragEnd write FOnDragEnd;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnClick: TNotifyEvent read FOnClick write SetOnClick stored OnClickStored;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnCanFocus: TCanFocusEvent read FOnCanFocus write FOnCanFocus;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnPainting: TOnPaintEvent read FOnPainting write FOnPainting;
    property OnPaint: TOnPaintEvent read FOnPaint write FOnPaint;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnResized: TNotifyEvent read FOnResized write FOnResized;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnApplyStyleLookup: TNotifyEvent read FOnApplyStyleLookup write FOnApplyStyleLookup;
    property TouchTargetExpansion: TBounds read FTouchTargetExpansion write SetTouchTargetExpansion;
    property TabStop: Boolean read GetTabStop write SetTabStop default True;
    property DisableDisappear: Boolean read GetDisableDisappear write SetDisableDisappear;
    /// <summary>If this property is true, the control will display its hint.</summary>
    property ShowHint: Boolean read FShowHint write SetShowHint stored IsShowHintStored;
    /// <summary>Hint string to display if the mouse hovers the control.</summary>
    property Hint: string read FHint write SetHint stored HintStored;
  published
    property Touch: TTouchManager read GetTouchManager write SetTouchManager;
    property OnGesture: TGestureEvent read FOnGesture write FOnGesture;
    property OnTap: TTapEvent read FOnTap write FOnTap;
  end;

  IDrawableObject = interface
  ['{C86EEAD8-69BF-4FDF-9FEE-A2F65E0EB3F0}']
    procedure DrawToCanvas(const Canvas: TCanvas; const ARect: TRectF; const AOpacity: Single = 1.0);
  end;

  ITintedObject = interface
  ['{42D829B7-6D86-41CC-86D5-F92C1FCAB060}']
    function GetCanBeTinted: Boolean;
    procedure SetTintColor(const ATintColor: TAlphaColor);
    property CanBeTinted: Boolean read GetCanBeTinted;
    property TintColor: TAlphaColor write SetTintColor;
  end;

  TOrientation = (Horizontal, Vertical);
  /// <summary> Determines the current state of the style
  /// <para> Unapplied - The style was successfully freed, or was not applied yet </para>
  /// <para> Freeing - At the moment the style is being freed
  /// See <see cref="FMX.Controls|TStyledControl.FreeStyle">FreeStyle</see> </para>
  /// <para> Applying - At the moment the style is being applied
  /// See <see cref="FMX.Controls|TStyledControl.ApplyStyle">ApplyStyle</see> </para>
  /// <para> Error - an exception was raised during applying or freeing the style </para>
  /// <para> Applied - The style was successfully applied </para>
  /// </summary>
  TStyleState = (Unapplied, Freeing, Applying, Error, Applied);

  TOrientationHelper = record helper for TOrientation
  const
    orHorizontal = TOrientation.Horizontal deprecated 'Use TOrientation.Horizontal';
    orVertical = TOrientation.Vertical deprecated 'Use TOrientation.Vertical';
  end;

{ TStyledControl }

  TStyledControl = class(TControl)
  public const
    StyleSuffix = 'style';
  strict private class var
    FLoadableStyle: TFmxObject;
  strict private
    FStylesData: TDictionary<string, TValue>;
    FScaleChangedId: Integer;
    FStyleChangedId: Integer;
    FResourceLink: TFmxObject;
    FAdjustType: TAdjustType;
    FAdjustSizeValue: TSizeF;
    FStyleLookup: string;
    FIsNeedStyleLookup: Boolean;
    FAutoTranslate: Boolean;
    FHelpType: THelpType;
    FHelpKeyword: string;
    FHelpContext: THelpContext;
    FStyleState: TStyleState;
    function GetStyleData(const Index: string): TValue;
    procedure SetStyleData(const Index: string; const Value: TValue);
    procedure SetStyleLookup(const Value: string);
    procedure ScaleChangedHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
    procedure StyleChangedHandler(const Sender: TObject; const Msg : TMessage);
  private
    procedure InternalFreeStyle;
  protected
    function SearchInto: Boolean; override;
    function GetBackIndex: Integer; override;
    function IsHelpContextStored: Boolean;
    procedure SetHelpContext(const Value: THelpContext);
    procedure SetHelpKeyword(const Value: string);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function DoSetSize(const ASize: TControlSize; const NewPlatformDefault: Boolean; ANewWidth, ANewHeight: Single;
       var ALastWidth, ALastHeight: Single): Boolean; override;
    procedure DoApplyStyleLookup; virtual;
    { Styles Data }
    procedure StyleDataChanged(const Index: string; const Value: TValue); virtual;
    function RequestStyleData(const Index: string): TValue; virtual;
    { control }
    procedure Painting; override;
    procedure ApplyStyle; virtual;
    procedure FreeStyle; virtual;
    /// <summary>Return Context for behavior manager.</summary>
    function GetStyleContext: TFmxObject; virtual;
    function GetDefaultStyleLookupName: string; virtual;
    /// <summary>Getter for ParentClassStyleLookupName property. Return default StyleLookup name for parent class.
    /// Used when style is loading.</summary>
    function GetParentClassStyleLookupName: string; virtual;
    procedure DoEnter; override;
    procedure Disappear; override;
    procedure AdjustSize; virtual;
    procedure AdjustFixedSize(const ReferenceControl: TControl); virtual;
    /// <summary>Select fixed size adjust type based on FixedSize in ResourceLink</summary>
    function ChooseAdjustType(const FixedSize: TSize): TAdjustType; virtual;
    procedure DoStyleChanged; virtual;
    procedure StyleLookupChanged; virtual;
    procedure RecycleResourceLink;
    procedure KillResourceLink;
    procedure DoDeleteChildren; override;
    /// <summary>Generate style lookup name based on AClassName string.</summary>
    function GenerateStyleName(const AClassName: string): string;
    function GetStyleObject: TFmxObject; overload; virtual;
    function GetStyleObject(const Clone: Boolean): TFmxObject; overload; virtual;
    procedure SetAdjustSizeValue(const Value: TSizeF); virtual;
    procedure SetAdjustType(const Value: TAdjustType); virtual;
    /// <summary>Gets style resource for this control as TFmxObject</summary>
    function GetResourceLink: TFmxObject; virtual;
    /// <summary>Gets style resource for this control as TControl</summary>
    function GetResourceControl: TControl;
    property IsNeedStyleLookup: Boolean read FIsNeedStyleLookup;
    property ResourceLink: TFmxObject read GetResourceLink;
    property ResourceControl: TControl read GetResourceControl;
    { IAlignableObject }
    function GetAdjustSizeValue: TSizeF; override;
    function GetAdjustType: TAdjustType; override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    procedure BeforeDestruction; override;
    destructor Destroy; override;
    property AdjustType: TAdjustType read GetAdjustType;
    property AdjustSizeValue: TSizeF read GetAdjustSizeValue;
    /// <summary> This property allows you to define the current state of style. It is changed when calling virtual
    /// methods <see cref="FMX.Controls|TStyledControl.FreeStyle">FreeStyle</see>,
    /// <see cref="FMX.Controls|TStyledControl.ApplyStyle">ApplyStyle</see>,
    /// <see cref="FMX.Controls|TStyledControl.DoApplyStyleLookup">DoApplyStyleLookup</see>
    /// </summary>
    property StyleState: TStyleState read FStyleState;
    procedure RecalcSize; override;
    function FindStyleResource(const AStyleLookup: string; const Clone: Boolean = False): TFmxObject; overload; override;
    /// <summary>Try find resource of specified type T by name <c>AStyleLookup</c>. If resource is not of type T or
    /// is not found, it returns false and doesn't change value of <c>AResource</c> param</summary>
    function FindStyleResource<T: TFmxObject>(const AStyleLookup: string; var AResource: T): Boolean; overload;
    /// <summary>Try find resource of specified type T by name <c>AStyleLookup</c> and makes a copy of original resource.
    /// If resource is not of type T, it returns nil</summary>
    function FindAndCloneStyleResource<T: TFmxObject>(const AStyleLookup: string; var AResource: T): Boolean;
    procedure SetNewScene(AScene: IScene); override;
    procedure ApplyStyleLookup; virtual;
    procedure NeedStyleLookup; virtual;
    procedure Inflate; virtual;
    procedure PrepareForPaint; override;
    procedure StartTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string); override;
    procedure StartTriggerAnimationWait(const AInstance: TFmxObject; const ATrigger: string); override;
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate;
    property DefaultStyleLookupName: string read GetDefaultStyleLookupName;
    /// <summary>Return default StyleLookup name for parent class</summary>
    property ParentClassStyleLookupName: string read GetParentClassStyleLookupName;
    property HelpType: THelpType read FHelpType write FHelpType default htContext;
    property HelpKeyword: string read FHelpKeyword write SetHelpKeyword stored IsHelpContextStored;
    property HelpContext: THelpContext read FHelpContext write SetHelpContext stored IsHelpContextStored default 0;
    property StylesData[const Index: string]: TValue read GetStyleData write SetStyleData;
    property StyleLookup: string read FStyleLookup write SetStyleLookup;
    /// <summary>Style that currently used to load style object</summary>
    class property LoadableStyle: TFmxObject read FLoadableStyle write FLoadableStyle;
    /// <summary>Lookup style object in scene StyleBook, active style and global pool.</summary>
    class function LookupStyleObject(const Instance: TFmxObject; const Context: TFmxObject; const Scene: IScene;
      const StyleLookup, DefaultStyleLookup, ParentClassStyleLookup: string; const Clone: Boolean;
      const UseGlobalPool: Boolean = True): TFmxObject;
  end;

  TStyleChangedMessage = class(TMessage<TStyleBook>)
  private
    FScene: IScene;
  public
    constructor Create(const StyleBook: TStyleBook; const Scene: IScene); overload;
    /// <summary>Scene where the style has been changed, nil if the change is global</summary>
    property Scene: IScene read FScene;
  end;

  TBeforeStyleChangingMessage = class(TMessage)
  end;

{ TStyleContainer }

  TStyleContainer = class(TControl, IBinaryStyleContainer)
  private
    FBinaryDict: TDictionary<string, TMemoryStream>;
    function CreateStyleResource(const AStyleLookup: string): TFmxObject;
    { IBinaryStyleContainer }
    procedure ClearContainer;
    procedure UnpackAllBinaries;
    procedure AddBinaryFromStream(const Name: string; const SourceStream: TStream; const Size: Int64);
    procedure AddObjectFromStream(const Name: string; const SourceStream: TStream; const Size: Int64);
    function LoadStyleResource(const AStream: TStream): TFmxObject;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindStyleResource(const AStyleLookup: string; const Clone: Boolean = False): TFmxObject; override;
  end;

{ TStyleBook }

  /// <summary>Represents an item in an instance of TStyleCollection that holds
  /// a style for a platform.</summary>
  TStyleCollectionItem = class(TCollectionItem)
  public const
    DefaultItem = 'Default';
  private
    [Weak] FStyleBook: TStyleBook;
    FBinary: TMemoryStream;
    FStyle: TFmxObject;
    FPlatform: string;
    FUnsupportedPlatform: Boolean;
    FNeedLoadFromBinary: Boolean;
    procedure SetPlatform(const Value: string);
    procedure SetResource(const Value: string);
    function GetResource: string;
    function GetStyle: TFmxObject;
    procedure ReadResources(Stream: TStream);
    procedure WriteResources(Stream: TStream);
    function StyleStored: Boolean;
    function GetIsEmpty: Boolean;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    /// <summary>Reload style from binary stream</summary>
    procedure LoadFromBinary;
    /// <summary>Save style to binary stream</summary>
    procedure SaveToBinary;
    /// <summary>Clear style and binary stream</summary>
    procedure Clear;
    /// <summary>Return true is style is empty</summary>
    property IsEmpty: Boolean read GetIsEmpty;
    /// <summary>Load style from stream</summary>
    procedure LoadFromStream(const Stream: TStream);
    /// <summary>Load style from file</summary>
    procedure LoadFromFile(const FileName: string);
    /// <summary>Save style to stream</summary>
    procedure SaveToStream(const Stream: TStream; const Format: TStyleFormat = TStyleFormat.Indexed);
    /// <summary>Link to owner StyleBook</summary>
    property StyleBook: TStyleBook read FStyleBook;
    /// <summary>Style that stored on this item</summary>
    property Style: TFmxObject read GetStyle;
    /// <summary>If style can not be load on current platform tihs property is True and Style is empty</summary>
    property UnsupportedPlatform: Boolean read FUnsupportedPlatform;
  published
    /// <summary>Name used to idenity style in collection</summary>
    property Platform: string read FPlatform write SetPlatform;
    /// <summary>Design-time only property used to show Style Designer</summary>
    property Resource: string read GetResource write SetResource stored False;
  end;

  /// <summary>Collection of  items that store styles for different
  /// platforms.</summary>
  TStyleCollection = class(TOwnedCollection)
  private
    [Weak] FStyleBook: TStyleBook;
    function GetItem(Index: Integer): TStyleCollectionItem;
    procedure SetItem(Index: Integer; const Value: TStyleCollectionItem);
  protected
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    constructor Create(AOwner: TPersistent);
    /// <summary>Create and add new item</summary>
    function Add: TStyleCollectionItem;
    /// <summary>Access property for style collection items</summary>
    property Items[Index: Integer]: TStyleCollectionItem read GetItem write SetItem; default;
  end;

  /// <summary>Record type that contains design-time information for the Form
  /// Designer.</summary>
  TStyleBookDesignInfo = record
    /// <summary>ClassName of selected control</summary>
    ClassName: string;
    /// <summary>If True that edit custom style mode is active</summary>
    CustomStyle: Boolean;
    /// <summary>Default StyleLookup for selected control</summary>
    DefaultStyleLookup: string;
    /// <summary>Name of selected control</summary>
    Name: string;
    /// <summary>StyleLookup of selected control</summary>
    StyleLookup: string;
    /// <summary>Selected control itself</summary>
    Control: TStyledControl;
    /// <summary>True if StyleBook just created</summary>
    JustCreated: Boolean;
  end;

  TStyleBook = class(TFmxObject)
  private
    FStyles: TStyleCollection;
    FStylesDic: TDictionary<string, TStyleCollectionItem>;
    FCurrentItemIndex: Integer;
    FFileName: string;
    FDesignInfo: TStyleBookDesignInfo;
    FUseStyleManager: Boolean;
    FBeforeStyleChangingId: Integer;
    FStyleChangedId: Integer;
    FResource: TStrings;
    procedure SetFileName(const Value: string);
    procedure SetUseStyleManager(const Value: Boolean);
    procedure SetStyles(const Value: TStyleCollection);
    function GetStyle: TFmxObject; overload;
    procedure SetCurrentItemIndex(const Value: Integer);
    procedure BeforeStyleChangingHandler(const Sender: TObject; const Msg: TMessage);
    procedure StyleChangedHandler(const Sender: TObject; const Msg: TMessage);
    function GetCurrentItem: TStyleCollectionItem;
    function GetUnsupportedPlatform: Boolean;
    procedure RebuildDictionary;
    procedure ResourceChanged(Sender: TObject);
    function StyleIndexByContext(const Context: TFmxObject): Integer;
    procedure CollectionChanged;
    procedure ReadStrings(Reader: TReader);
  protected
    /// <summary>Used when looking style in global pool</summary>
    function CustomFindStyleResource(const AStyleLookup: string; const Clone: Boolean): TFmxObject; virtual;
    /// <summary>Choose style depends on context</summary>
    procedure ChooseStyleIndex; virtual;
    /// <summary>Create empty item on demand</summary>
    procedure CreateDefaultItem; virtual;
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadResources(Stream: TStream);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>Integration property used with Style Designer</summary>
    property DesignInfo: TStyleBookDesignInfo read FDesignInfo write FDesignInfo;
    /// <summary>Clear style collection</summary>
    procedure Clear;
    /// <summary>Get Style by Context</summary>
    function GetStyle(const Context: TFmxObject): TFmxObject; overload;
    /// <summary>Load style from stream</summary>
    procedure LoadFromStream(const Stream: TStream);
    /// <summary>Load style from file</summary>
    procedure LoadFromFile(const AFileName: string);
    /// <summary>Current Style</summary>
    property Style: TFmxObject read GetStyle;
    /// <summary>Item index of current style in style's collection</summary>
    property CurrentItemIndex: Integer read FCurrentItemIndex write SetCurrentItemIndex;
    /// <summary>Current style in style's collection</summary>
    property CurrentItem: TStyleCollectionItem read GetCurrentItem;
    property Resource: TStrings read FResource;
    /// <summary>If style can not be load on current platform tihs property is True and Style is empty</summary>
    property UnsupportedPlatform: Boolean read GetUnsupportedPlatform;
  published
    /// <summary>Used to load style from file instead of storing style in form resource</summary>
    property FileName: string read FFileName write SetFileName;
    /// <summary>If UseStyleManager is True component use TStyleManager to replace default style for whole application</summary>
    property UseStyleManager: Boolean read FUseStyleManager write SetUseStyleManager default False;
    /// <summary>Collection of styles stored in StyleBook </summary>
    property Styles: TStyleCollection read FStyles write SetStyles;
  end;

  TStyleManagerHelper = class helper for TStyleManager
  public
    // Return active style for IScene
    class function ActiveStyleForScene(const Scene: IScene): TFmxObject; static;
    // Return Active Style Descriptor for selected object
    class function GetStyleDescriptionForControl(const AObject: TControl): TStyleDescription;
  end;


  TTextSettingsInfo = class (TPersistent)
  public type
    TBaseTextSettings = class (TTextSettings)
    private
      [Weak] FInfo: TTextSettingsInfo;
      [Weak] FControl: TControl;
    public
      constructor Create(const AOwner: TPersistent); override;
      property Info: TTextSettingsInfo read FInfo;
      property Control: TControl read FControl;
    end;
    TCustomTextSettings = class (TBaseTextSettings)
    public
      constructor Create(const AOwner: TPersistent); override;
      property WordWrap default True;
      property Trimming default TTextTrimming.None;
    end;
    TCustomTextSettingsClass = class of TCustomTextSettings;
    TTextPropLoader = class
    private
      FInstance: TPersistent;
      FFiler: TFiler;
      FITextSettings: ITextSettings;
      FTextSettings: TTextSettings;
    protected
      procedure ReadSet(const Instance: TPersistent; const Reader: TReader; const PropertyName: string);
      procedure ReadEnumeration(const Instance: TPersistent; const Reader: TReader; const PropertyName: string);
      procedure ReadFontFillColor(Reader: TReader);
      procedure ReadFontFamily(Reader: TReader);
      procedure ReadFontFillKind(Reader: TReader);
      procedure ReadFontStyle(Reader: TReader);
      procedure ReadFontSize(Reader: TReader);
      procedure ReadTextAlign(Reader: TReader);
      procedure ReadTrimming(Reader: TReader);
      procedure ReadVertTextAlign(Reader: TReader);
      procedure ReadWordWrap(Reader: TReader);
    public
      constructor Create(const AInstance: TComponent; const AFiler: TFiler);
      procedure ReadProperties; virtual;
      property Instance: TPersistent read FInstance;
      property Filer: TFiler read FFiler;
      property TextSettings: TTextSettings read FTextSettings;
    end;
  private
    FDefaultTextSettings: TTextSettings;
    FTextSettings: TTextSettings;
    FResultingTextSettings: TTextSettings;
    FOldTextSettings: TTextSettings;
    [Weak] FOwner: TPersistent;
    FDesign: Boolean;
    FStyledSettings: TStyledSettings;
    procedure SetDefaultTextSettings(const Value: TTextSettings);
    procedure SetStyledSettings(const Value: TStyledSettings);
    procedure SetTextSettings(const Value: TTextSettings);
    procedure OnDefaultChanged(Sender: TObject);
    procedure OnTextChanged(Sender: TObject);
    procedure OnCalculatedTextSettings(Sender: TObject);
  protected
    procedure RecalculateTextSettings; virtual;
    procedure DoDefaultChanged; virtual;
    procedure DoTextChanged; virtual;
    procedure DoCalculatedTextSettings; virtual;
    procedure DoStyledSettingsChanged; virtual;
  public
    constructor Create(AOwner: TPersistent; ATextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass); virtual;
    destructor Destroy; override;
    property Design: Boolean read FDesign write FDesign;
    property StyledSettings: TStyledSettings read FStyledSettings write SetStyledSettings;
    property DefaultTextSettings: TTextSettings read FDefaultTextSettings write SetDefaultTextSettings;
    property TextSettings: TTextSettings read FTextSettings write SetTextSettings;
    property ResultingTextSettings: TTextSettings read FResultingTextSettings;
    property Owner: TPersistent read FOwner;
  end;

{ TTextControl }

  TTextControl = class(TStyledControl, ITextSettings, ICaption, IAcceleratorKeyReceiver)
  private
    FTextSettingsInfo: TTextSettingsInfo;
    FTextObject: TControl;
    FITextSettings: ITextSettings;
    FObjectState: IObjectState;
    FText: string;
    FIsChanging: Boolean;
    FPrefixStyle: TPrefixStyle;
    FAcceleratorKey: Char;
    FAcceleratorKeyIndex: Integer;
    function GetFont: TFont;
    function GetText: string;
    function TextStored: Boolean;
    procedure SetFont(const Value: TFont);
    function GetTextAlign: TTextAlign;
    procedure SetTextAlign(const Value: TTextAlign);
    function GetVertTextAlign: TTextAlign;
    procedure SetVertTextAlign(const Value: TTextAlign);
    function GetWordWrap: Boolean;
    procedure SetWordWrap(const Value: Boolean);
    function GetFontColor: TAlphaColor;
    procedure SetFontColor(const Value: TAlphaColor);
    function GetTrimming: TTextTrimming;
    procedure SetTrimming(const Value: TTextTrimming);
    procedure SetPrefixStyle(const Value: TPrefixStyle);
    { ITextSettings }
    function GetDefaultTextSettings: TTextSettings;
    function GetTextSettings: TTextSettings;
    function GetStyledSettings: TStyledSettings;
    function GetResultingTextSettings: TTextSettings;
  protected
    /// <summary>A TTextControl can respond to an accelerator key, so TControl.DoRootChanging is overriden to register/unregister
    /// this control to/from a form when the control is added to a form, or when this control is moved from a form to another
    /// one.</summary>
    procedure DoRootChanging(const NewRoot: IRoot); override;
    /// <summary>This function is invoked to filter the text that is going to be displayed. This function doesn't modify the string
    /// stored by the control used as Text property.</summary>
    function DoFilterControlText(const AText: string): string; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure DoStyleChanged; override;
    procedure SetText(const Value: string); virtual;
    procedure SetTextInternal(const Value: string); virtual;
    procedure SetName(const Value: TComponentName); override;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    procedure ActionChange(Sender: TBasicAction; CheckDefaults: Boolean); override;
    procedure Loaded; override;
    function FindTextObject: TFmxObject; virtual;
    procedure UpdateTextObject(const TextControl: TControl; const Str: string);
    property TextObject: TControl read FTextObject;
    procedure DoTextChanged; virtual;
    procedure DoEndUpdate; override;
    function CalcTextObjectSize(const MaxWidth: Single; var Size: TSizeF): Boolean;
    { ITextSettings }
    procedure SetTextSettings(const Value: TTextSettings); virtual;
    procedure SetStyledSettings(const Value: TStyledSettings); virtual;
    procedure DoChanged; virtual;
    function StyledSettingsStored: Boolean; virtual;
    function GetTextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass; virtual;
    { IAcceleratorKeyReceiver }
    /// <summary>Implements IAcceleratorKeyReceiver.TriggerAcceleratorKey by setting focus to this control.</summary>
    procedure TriggerAcceleratorKey; virtual;
    /// <summary>Implements IAcceleratorKeyReceiver.CanTriggerAcceleratorKey by returning True if this control and all
    /// of its parent controls are visible.</summary>
    function CanTriggerAcceleratorKey: Boolean; virtual;
    /// <summary>Implements IAcceleratorKeyReceiver.GetAcceleratorChar by returning the value stored in FAcceleratorKey.</summary>
    function GetAcceleratorChar: Char;
    /// <summary>Implements IAcceleratorKeyReceiver.GetAcceleratorCharIndex by returning the value stored in
    /// FAcceleratorKeyIndex. This indicates the position within the text string of the accelerator character.</summary>
    function GetAcceleratorCharIndex: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function ToString: string; override;

    property Text: string read GetText write SetText stored TextStored;

    property DefaultTextSettings: TTextSettings read GetDefaultTextSettings;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property StyledSettings: TStyledSettings read GetStyledSettings write SetStyledSettings stored StyledSettingsStored nodefault;
    property ResultingTextSettings: TTextSettings read GetResultingTextSettings;

    procedure Change;
    property Font: TFont read GetFont write SetFont;
    property FontColor: TAlphaColor read GetFontColor write SetFontColor default TAlphaColorRec.Black;
    property VertTextAlign: TTextAlign read GetVertTextAlign write SetVertTextAlign default TTextAlign.Center;
    property TextAlign: TTextAlign read GetTextAlign write SetTextAlign default TTextAlign.Leading;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default False;
    property Trimming: TTextTrimming read GetTrimming write SetTrimming default TTextTrimming.None;
    /// <summary> Determine the way portraying a single character "&amp;" </summary>
    property PrefixStyle: TPrefixStyle read FPrefixStyle write SetPrefixStyle default TPrefixStyle.HidePrefix;
  end;

{ TContent }

  TContent = class(TControl, IContent)
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
    property Align;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled;
    property Locked default False;
    property Height;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Visible;
    property Width;
    property Size;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnCanFocus;
    property OnClick;
    property OnDblClick;

    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnResized;
  end;

{ TPopup }

  TPlacement = (Bottom, Top, Left, Right, Center, BottomCenter, TopCenter, LeftCenter, RightCenter, Absolute, Mouse, MouseCenter);

  TPlacementHelper = record helper for TPlacement
  const
    plBottom = TPlacement.Bottom deprecated 'Use TPlacement.Bottom';
    plTop = TPlacement.Top deprecated 'Use TPlacement.Top';
    plLeft = TPlacement.Left deprecated 'Use TPlacement.Left';
    plRight = TPlacement.Right deprecated 'Use TPlacement.Right';
    plCenter = TPlacement.Center deprecated 'Use TPlacement.Center';
    plBottomCenter = TPlacement.BottomCenter deprecated 'Use TPlacement.BottomCenter';
    plTopCenter = TPlacement.TopCenter deprecated 'Use TPlacement.TopCenter';
    plLeftCenter = TPlacement.LeftCenter deprecated 'Use TPlacement.LeftCenter';
    plRightCenter = TPlacement.RightCenter deprecated 'Use TPlacement.RightCenter';
    plAbsolute = TPlacement.Absolute deprecated 'Use TPlacement.Absolute';
    plMouse = TPlacement.Mouse deprecated 'Use TPlacement.Mouse';
    plMouseCenter = TPlacement.MouseCenter deprecated 'Use TPlacement.MouseCenter';
  end;

  TPopup = class(TStyledControl)
  private
    [Weak] FSaveParent: TFmxObject;
    FSaveScale: TPointF;
    FPopupForm: TFmxObject;
    FIsOpen: Boolean;
    FStaysOpen: Boolean;
    FPlacement: TPlacement;
    [Weak] FPlacementTarget: TControl;
    FPlacementRectangle: TBounds;
    FHorizontalOffset: Single;
    FVerticalOffset: Single;
    FDragWithParent: Boolean;
    FClosingAnimation: Boolean;
    FStyleBook: TStyleBook;
    FModalResult: TModalResult;
    FModal: Boolean;
    FOnClosePopup: TNotifyEvent;
    FOnPopup: TNotifyEvent;
    FBorderWidth: Single;
    FAniDuration: Single;
    FPopupFormSize: TSizeF;
    FPreferedDisplayIndex: Integer;
    FOnAniTimer: TNotifyEvent;
    procedure SetIsOpen(const Value: Boolean);
    procedure SetPlacementRectangle(const Value: TBounds);
    procedure SetModalResult(const Value: TModalResult);
    procedure SetPlacementTarget(const Value: TControl);
    procedure SetStyleBook(const Value: TStyleBook);
    procedure SetPlacement(const Value: TPlacement);
    procedure SetDragWithParent(const Value: Boolean);
    procedure SetBorderWidth(const Value: Single);
    procedure BeforeShowProc(Sender: TObject);
    procedure BeforeCloseProc(Sender: TObject);
    procedure CloseProc(Sender: TObject; var Action: TCloseAction);
    procedure SetAniDuration(const Value: Single);
    procedure ReadLeft(Reader: TReader);
    procedure ReadTop(Reader: TReader);
    procedure SetPopupFormSize(const Value: TSizeF);
    procedure UpdatePopupSize;
    procedure SetOnAniTimer(const Value: TNotifyEvent);
  protected
    procedure ApplyStyle; override;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DialogKey(var Key: Word; Shift: TShiftState); override;
    procedure DoClosePopup; virtual;
    procedure DoPopup; virtual;
    procedure ClosePopup; virtual;
    function CreatePopupForm: TFmxObject; virtual;
    property PopupForm: TFmxObject read FPopupForm;
    function VisibleStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Popup(const AShowModal: Boolean = False); virtual;
    function PopupModal: TModalResult; virtual;
    function HasPopupForm: Boolean;
    procedure BringToFront; override;
    property AniDuration: Single read FAniDuration write SetAniDuration;
    property BorderWidth : Single read FBorderWidth write SetBorderWidth;
    property ModalResult: TModalResult read FModalResult write SetModalResult;
    property IsOpen: Boolean read FIsOpen write SetIsOpen;
    property ClosingAnimation: Boolean read FClosingAnimation;
    property PopupFormSize: TSizeF read FPopupFormSize write SetPopupFormSize;
    property PreferedDisplayIndex: Integer read FPreferedDisplayIndex write FPreferedDisplayIndex;
    ///<summary> This event is called periodically (for the AniDuration time) after emergence and before the
    /// disappearance of the pop-up </summary>
    property OnAniTimer: TNotifyEvent read FOnAniTimer write SetOnAniTimer;
  published
    property Align;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property DragWithParent: Boolean read FDragWithParent write SetDragWithParent default False;
    property EnableDragHighlight default True;
    property Enabled;
    property Locked default False;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property HitTest default True;
    property HorizontalOffset: Single read FHorizontalOffset write FHorizontalOffset;
    property Padding;
    property Opacity;
    property Margins;
    property Placement: TPlacement read FPlacement write SetPlacement default TPlacement.Bottom;
    property PlacementRectangle: TBounds read FPlacementRectangle write SetPlacementRectangle;
    property PlacementTarget: TControl read FPlacementTarget write SetPlacementTarget;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property StyleBook: TStyleBook read FStyleBook write SetStyleBook;
    property StyleLookup;
    property TabOrder;
    property VerticalOffset: Single read FVerticalOffset write FVerticalOffset;
    property Visible stored VisibleStored;
    property Width;

    {events}
    property OnApplyStyleLookup;
    property OnClosePopup: TNotifyEvent read FOnClosePopup write FOnClosePopup;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnCanFocus;
    property OnClick;
    property OnDblClick;

    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnResized;
  end;

{ TPathAnimation }

  TPathAnimation = class(TAnimation)
  private
    FPath: TPathData;
    FPolygon: TPolygon;
    FObj: TControl;
    FStart: TPointF;
    FRotate: Boolean;
    FSpline: TSpline;
    procedure SetPath(const Value: TPathData);
    function EnabledStored: Boolean;
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
  published
    property AnimationType default TAnimationType.In;
    property AutoReverse default False;
    property Enabled stored EnabledStored;
    property Delay;
    property Duration nodefault;
    property Interpolation default TInterpolationType.Linear;
    property Inverse default False;
    property Loop default False;
    property OnProcess;
    property OnFinish;
    property Path: TPathData read FPath write SetPath;
    property Rotate: Boolean read FRotate write FRotate default False;
    property Trigger;
    property TriggerInverse;
  end;

  IInflatableContent<T: TStyledControl> = interface
    function GetInflatableItems: TList<T>;
    procedure NotifyInflated;
  end;

  TContentInflater<T: TStyledControl> = class(TInterfacedObject, IInterface)
  strict private
    FInflatable: IInflatableContent<T>;
    FBusy: Boolean;
    procedure ReceiveIdleMessage(const Sender : TObject; const M : System.Messaging.TMessage);
  public
    constructor Create(const Inflatable: IInflatableContent<T>);
    destructor Destroy; override;
    procedure Inflate(Total: Boolean);
  end;

  TControlsFilter<T: TFmxObject> = class(TEnumerableFilter<TControl,T>)
  end;

  ISearchResponder = interface
    ['{C73631F4-5AD7-48b9-92D2-CC808B911B5E}']
    procedure SetFilterPredicate(const Predicate: TPredicate<string>);
  end;

  IListBoxHeaderTrait = interface
    ['{C7BDF195-C1E2-48f9-9376-1382C60A6BCC}']
  end;

procedure CloseAllPopups;
function IsPopup(const Wnd: TFmxObject): Boolean;
function CanClosePopup(const Wnd: TFmxObject): Boolean;
procedure PopupBringToFront;
procedure ClosePopup(const AIndex: Integer); overload;
procedure ClosePopup(Wnd: TFmxObject); overload;

function GetPopup(const AIndex: Integer): TFmxObject;
function GetPopupCount: Integer;

procedure FreeControls;

type
  TPropertyApplyProc = reference to procedure(Instance: TObject; Prop: TRttiProperty);

function FindProperty(var O: TObject; Path: String; const Apply: TPropertyApplyProc): Boolean;

implementation

uses
  System.RTLConsts, System.TypInfo, System.Analytics, System.Character, System.IOUtils, FMX.Consts, FMX.Forms,
  FMX.Objects, FMX.Platform, FMX.BehaviorManager, FMX.Gestures, FMX.Utils, FMX.Menus;

type

  TOpenObject = class(TFmxObject);

  TOpenForm = class(TCustomForm);

{ TCustomControlAction }

constructor TCustomControlAction.Create(AOwner: TComponent);
begin
  inherited;
  DisableIfNoHandler := False;
end;

procedure TCustomControlAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FPopupMenu) and (Operation = opRemove) then
    PopupMenu := nil;
end;

procedure TCustomControlAction.SetPopupMenu(const Value: TCustomPopupMenu);
var
  I: Integer;
  OldPopup: TCustomPopupMenu;
begin
  if Value <> FPopupMenu then
  begin
    OldPopup := FPopupMenu;
    for I := 0 to ClientCount - 1 do
      if Clients[I] is TControlActionLink then
        TControlActionLink(Clients[I]).SetPopupMenu(Value);
    FPopupMenu := Value;
    if FPopupMenu <> nil then
      TComponent(FPopupMenu).FreeNotification(self);
    if OldPopup <> nil then
      TComponent(OldPopup).RemoveFreeNotification(self);
    Change;
  end;
end;

{ TControlActionLink }

procedure TControlActionLink.AssignClient(AClient: TObject);
begin
  if AClient = nil then
    raise EActionError.CreateFMT(SParamIsNil, ['AClient']);
  if not (AClient is TControl) then
    raise EActionError.CreateFmt(StrNoClientClass, [AClient.ClassName]);
  inherited;
end;

function TControlActionLink.GetClient: TControl;
begin
  Result := TControl(inherited Client);
end;

function TControlActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited and (Client.Enabled = TContainedAction(Action).Enabled);
end;

function TControlActionLink.IsHelpLinked: Boolean;
begin
  Result := (Client is TStyledControl) and inherited and
    (TStyledControl(Client).HelpContext = TContainedAction(Action).HelpContext) and
    (TStyledControl(Client).HelpKeyword = TContainedAction(Action).HelpKeyword) and
    (TStyledControl(Client).HelpType = TContainedAction(Action).HelpType);
end;

function TControlActionLink.IsOnExecuteLinked: Boolean;
begin
  if not Client.EnableExecuteAction then
    Result := False
  else
    Result := inherited and (TMethod(Client.OnClick) = TMethod(Action.OnExecute));
end;

function TControlActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited and (Client.Visible = TContainedAction(Action).Visible);
end;

procedure TControlActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then Client.Enabled := Value;
end;

procedure TControlActionLink.SetHelpContext(Value: THelpContext);
begin
  if IsHelpLinked then TStyledControl(Client).HelpContext := Value;
end;

procedure TControlActionLink.SetHelpKeyword(const Value: string);
begin
  if IsHelpLinked then TStyledControl(Client).HelpKeyword := Value;
end;

procedure TControlActionLink.SetHelpType(Value: THelpType);
begin
  if IsHelpLinked then TStyledControl(Client).HelpType := Value;
end;

procedure TControlActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then Client.OnClick := Value;
end;

procedure TControlActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then Client.Visible := Value;
end;

function TControlActionLink.IsPopupMenuLinked: boolean;
begin
  Result := (Client is TControl) and
            (Action is TCustomControlAction) and
            (TControl(Client).PopupMenu = TCustomControlAction(Action).PopupMenu);
end;

procedure TControlActionLink.SetPopupMenu(const Value: TCustomPopupMenu);
begin
  if IsPopupMenuLinked then
    Client.PopupMenu := Value;
end;

function TControlActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked and (Client.Hint = TContainedAction(Action).Hint);
end;

function TControlActionLink.DoShowHint(var HintStr: string): Boolean;
var
  LPlatformService: IFMXMenuService;
begin
  Result := False;
  if Action is TCustomAction then
  begin
    Result := TCustomAction(Action).DoHint(HintStr);
    if Result and Application.HintShortCuts and (TCustomAction(Action).ShortCut <> scNone) and
      TPlatformServices.Current.SupportsPlatformService(IFMXMenuService, LPlatformService) then
      HintStr := Format('%s (%s)', [HintStr, LPlatformService.ShortCutToText(TCustomAction(Action).ShortCut)]);
  end;
end;

procedure TControlActionLink.SetHint(const Value: string);
begin
  if IsHintLinked then
    TControl(Client).Hint := Value;
end;

{ TCaret }

class function TCaret.FlasherName: string;
begin
  Result := FMXFlasher;
end;

procedure ShowVirtualKeyboard(const Displayed: boolean; const Caret: TCustomCaret;
  var VirtualKeyboardState: TVirtualKeyboardStates);
var
  VKbSvc: IFMXVirtualKeyboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, VKbSvc) then
  begin
    if Displayed and (Caret <> nil) and not TCaret(Caret).ReadOnly then
    begin
      VirtualKeyboardState := VKbSvc.VirtualKeyboardState;
      if (TVirtualKeyboardState.AutoShow in VirtualKeyboardState) and
         (not (TVirtualKeyboardState.Error in VirtualKeyboardState)) then
      begin
        if VKbSvc.ShowVirtualKeyboard(Caret.Owner) then
          VirtualKeyboardState := VirtualKeyboardState + [TVirtualKeyboardState.Visible]
        else
          VirtualKeyboardState := VirtualKeyboardState - [TVirtualKeyboardState.Visible]
      end;
    end
    else
    begin
      VKbSvc.SetTransientState(VirtualKeyboardState * [TVirtualKeyboardState.Transient] <> []);
      VirtualKeyboardState := VKbSvc.VirtualKeyboardState;
      if (TVirtualKeyboardState.AutoShow in VirtualKeyboardState) and
         (not (TVirtualKeyboardState.Error in VirtualKeyboardState)) and
         (VKbSvc.HideVirtualKeyboard) then
      begin
        VirtualKeyboardState := VirtualKeyboardState - [TVirtualKeyboardState.Visible];
      end;
      VKbSvc.SetTransientState(False);
    end;
  end;
end;

type

{ TStyleCache }

  TStyleCache = class
  strict private
    class var FCurrent: TStyleCache;
    class function GetCurrent: TStyleCache; static;
    class function IsInitialized: Boolean; static;
  protected
    constructor Create;
  private
    FStyleList: TList<TControl>;
    FClearStyleCacheId: Integer;
    FEnabled: Boolean;
    FClearing: Boolean;
    procedure Clear;
    procedure ClearStyleCacheHandler(const Sender: TObject; const Msg : TMessage);
  public
    destructor Destroy; override;
    procedure FreeResource(const AResource: TControl);
    function FindResource(const StyleName: string): TControl;
    procedure Remove(const AResource: TControl);
    property Enabled: Boolean read FEnabled write FEnabled;
    class procedure Uninitialize;
    class property Current: TStyleCache read GetCurrent;
    class property Initialized: Boolean read IsInitialized;
  end;

constructor TStyleCache.Create;
begin
  inherited Create;
  FEnabled := True;
  FClearStyleCacheId := TMessageManager.DefaultManager.SubscribeToMessage(TBeforeStyleChangingMessage, ClearStyleCacheHandler);
end;

destructor TStyleCache.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TBeforeStyleChangingMessage, FClearStyleCacheId);
  Clear;
  FreeAndNil(FStyleList);
  inherited;
end;

procedure TStyleCache.Clear;
var
  I: Integer;
begin
  if FStyleList <> nil then
  begin
    FClearing := True;
    try
      for I := 0 to FStyleList.Count - 1 do
        FStyleList[I].DisposeOf;
      FStyleList.Clear;
    finally
      FClearing := False;
    end;
  end;
end;

function TStyleCache.FindResource(const StyleName: string): TControl;
var
  I: Integer;
begin
  Result := nil;
  if FStyleList <> nil then
    for I := 0 to FStyleList.Count - 1 do
      if SameText(FStyleList[I].TagString, StyleName) then
      begin
        Result := FStyleList[I];
        FStyleList.Delete(I);
        Break;
      end;
end;

procedure TStyleCache.FreeResource(const AResource: TControl);
begin
  if not FEnabled then
    Exit;
  AResource.FInPaintTo := True; // to disable call Repaint
  try
    AResource.Parent := nil;
  finally
    AResource.FInPaintTo := False;
  end;
  if FStyleList = nil then
    FStyleList := TList<TControl>.Create;
  FStyleList.Add(AResource);
end;

class function TStyleCache.GetCurrent: TStyleCache;
begin
  if FCurrent = nil then
    FCurrent := TStyleCache.Create;
  Result := FCurrent;
end;

class function TStyleCache.IsInitialized: Boolean;
begin
  Result := FCurrent <> nil;
end;

procedure TStyleCache.Remove(const AResource: TControl);
var
  I: Integer;
begin
  if (FStyleList <> nil) and not FClearing then
    for I := 0 to FStyleList.Count - 1 do
      if FStyleList[I] = AResource then
      begin
        FStyleList.Delete(I);
        Break;
      end;
end;

procedure TStyleCache.ClearStyleCacheHandler(const Sender: TObject; const Msg: TMessage);
begin
  Clear;
end;

class procedure TStyleCache.Uninitialize;
begin
  FreeAndNil(FCurrent);
end;

{ TControl }

constructor TControl.Create(AOwner: TComponent);
begin
  inherited;
  FSimpleTransform := True;
  FEnabled := True;
  FRecalcEnabled := True;
  FEnableDragHighlight := True;
  FOpacity := 1;
  FLocalMatrix := TMatrix.Identity;
  FPadding := TBounds.Create(TRectF.Empty);
  FPadding.OnChange := PaddingChangedHandler;
  FMargins := TBounds.Create(TRectF.Empty);
  FMargins.OnChange := MarginsChanged;
  FPosition := TPosition.Create(TPointF.Zero);
  FScale := TPosition.Create(TPointF.Create(1, 1));
  FSkew := TPosition.Create(TPointF.Zero);
  FRotationCenter := TPosition.Create(TPointF.Create(0.5, 0.5));
  FSize := TControlSize.Create(GetDefaultSize);
  FSize.OnChange := SizeChanged;
  FPosition.OnChange := MatrixChanged;
  FScale.OnChange := MatrixChanged;
  FSkew.OnChange := MatrixChanged;
  FRotationCenter.OnChange := MatrixChanged;
  FTouchTargetExpansion := TBounds.Create(GetDefaultTouchTargetExpansion);
  FAnchors := [TAnchorKind.akLeft, TAnchorKind.akTop];
  FLastWidth := FSize.Width;
  FLastHeight := FSize.Height;
  FVisible := True;
  FHitTest := True;
  FRecalcAbsolute := True;
  FRecalcOpacity := True;
  FUpdateEffects := True;
  FRecalcUpdateRect := True;
  FCanFocus := False;
  FAcceptsControls := True;
  FDesignSelectionMarks := True;
  FInheritedCursor := crDefault;
  FDisabledOpacity := DefaultDisabledOpacity;
  FTabStop := True;
  FParentShowHint := True;
end;

procedure TControl.DoDeleteChildren;
begin
  if FTabList <> nil then
    FTabList.Clear;
  inherited;
  FreeAndNil(FControls);
  if FEffects <> nil then
    FEffects.Clear;
end;

function TControl.ShowInDesigner: Boolean;
begin
  Result := (csDesigning in ComponentState) and FVisible;
end;

destructor TControl.Destroy;
var
  Controller: ITabStopController;
begin
  Controller := GetTabStopController;
  if Controller <> nil then
    Controller.TabList.Remove(Self);
  FreeAndNil(FTabList);
  FreeAndNil(FEffectBitmap);
  FreeAndNil(FControls);
  FreeAndNil(FEffects);
  FreeAndNil(FPadding);
  FreeAndNil(FMargins);
  FreeAndNil(FRotationCenter);
  FreeAndNil(FScale);
  FreeAndNil(FSkew);
  FreeAndNil(FPosition);
  FreeAndNil(FTouchTargetExpansion);
  FreeAndNil(FTouchManager);
  if TStyleCache.Initialized then
    TStyleCache.Current.Remove(Self);
  inherited;
  FreeAndNil(FSize);
end;

procedure TControl.Loaded;
begin
  inherited;
  if csDestroying in ComponentState then
    Exit;
  if TDelayedEvent.Resize in FDelayedEvents then
  begin
    Exclude(FDelayedEvents, TDelayedEvent.Resize);
    Resize;
  end;
  FLastWidth := FSize.Width;
  FLastHeight := FSize.Height;

  MatrixChanged(Self);

  UpdateExplicitBounds;
  UpdateAnchorRules(False);

  if TDelayedEvent.Resized in FDelayedEvents then
  begin
    Exclude(FDelayedEvents, TDelayedEvent.Resized);
    DoResized;
  end;

  if (Children <> nil) and (ChildrenCount > 0) then
    Realign;
  if (Root <> nil) and (Root.GetActiveControl <> nil) and (Root.GetActiveControl.GetObject = Self) then
    SetFocus;
end;

procedure TControl.Updated;
begin
  inherited;
  Disappear;
end;


procedure TControl.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('FixedWidth', ReadFixedWidth, WriteFixedWidth, FFixedSize.Width <> 0);
  Filer.DefineProperty('FixedHeight', ReadFixedHeight, WriteFixedHeight, FFixedSize.Height <> 0);
  Filer.DefineProperty('DesignVisible', ReadDesignVisible, nil, False);
  Filer.DefineProperty('ShowHint', ReadShowHint, nil, False);
  Filer.DefineProperty('Hint', ReadHint, nil, False);
end;

procedure TControl.ReadFixedWidth(Reader: TReader);
begin
  FFixedSize.Width := Reader.ReadInteger;
end;

procedure TControl.WriteFixedWidth(Writer: TWriter);
begin
  Writer.WriteInteger(FFixedSize.Width);
end;

procedure TControl.ReadDesignVisible(Reader: TReader);
begin
  Reader.ReadBoolean;
end;

procedure TControl.ReadFixedHeight(Reader: TReader);
begin
  FFixedSize.Height := Reader.ReadInteger;
end;

procedure TControl.WriteFixedHeight(Writer: TWriter);
begin
  Writer.WriteInteger(FFixedSize.Height);
end;

procedure TControl.ReadShowHint(Reader: TReader);
begin
  Reader.ReadBoolean;
end;

procedure TControl.ReadHint(Reader: TReader);
begin
  Reader.ReadString;
end;

procedure TControl.NeedUpdateEffects;
begin
  UpdateEffects;
  FRecalcUpdateRect := True;
  FRecalcHasEffect := True;
  Repaint;
end;

procedure TControl.BeforeEffectEnabledChanged(const Enabled: Boolean);
begin
  if not Enabled then
    Repaint;
end;

procedure TControl.EffectEnabledChanged(const Enabled: Boolean);
begin
  NeedUpdateEffects;
  RecalcHasEffect;
end;

procedure TControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited ;
  if (Operation = opRemove) and (AComponent = FPopupMenu) then
      FPopupMenu := nil;
end;

procedure TControl.RepaintJointArea(const DestControl: TObject);
var
  UpdateRect: TRectF;
begin
  if (not ClipChildren) and ((Canvas = nil) or (TCanvasStyle.SupportClipRects in Canvas.GetCanvasStyle)) then
  begin
    UpdateRect := GetUpdateRect;
    if DestControl is TControl then
      UpdateRect.Union(TControl(DestControl).GetUpdateRect);
    RepaintRect(UpdateRect);
  end
  else
    Repaint;
end;

procedure TControl.DoAddObject(const AObject: TFmxObject);
var
  AlignObject: IAlignableObject;
  TabStop: IControl;
  AsControl: TControl;
  NeedRepaint: Boolean;
  i: integer; // https://quality.embarcadero.com/browse/RSP-21013
begin
  DisableDisappear := True;
  try
    if AObject is TControl then
    begin
      AsControl := TControl(AObject);
      //https://quality.embarcadero.com/browse/RSP-21013
      //AsControl.FUpdating := FUpdating;
      for I := 1 to FUpdating do
        AsControl.beginUpdate;
    end
    else
      AsControl := nil;
    NeedRepaint := False;

    inherited DoAddObject(AObject);

    if (not (csReading in ComponentState)) and Supports(AObject, IControl, TabStop) then
      GetTabList.Add(TabStop);

    if Supports(AObject, IAlignableObject, AlignObject) and ((AlignObject.Align <> TAlignLayout.None) or
      (AlignObject.Anchors <> [TAnchorKind.akLeft, TAnchorKind.akTop])) then
      FNeedAlign := True;

    if AObject is TEffect then
    begin
      AddToEffectsList(TEffect(AObject));
      if TEffect(AObject).Enabled then
      begin
        RecalcHasEffect;
        if not (csLoading in ComponentState) then
        begin
          RecalcUpdateRect;
          NeedRepaint := True;
        end;
      end;
    end;
    if AsControl <> nil then
    begin
      if FControls = nil then
      begin
        FControls := TControlList.Create;
        FControls.Capacity := InitialControlsCapacity;
      end;
      AsControl.SetNewScene(FScene);
      if TempCanvas <> nil then
        AsControl.TempCanvas := TempCanvas;
      if FInPaintTo then
        AsControl.FInPaintTo := True;
      //https://quality.embarcadero.com/browse/RSP-21013
      //AsControl.FUpdating := FUpdating;
      if not FSimpleTransform then
        AsControl.FSimpleTransform := False;
      AsControl.RecalcEnabled;
      AsControl.RecalcOpacity;
      AsControl.RecalcAbsolute;
      AsControl.RecalcUpdateRect;
      AsControl.RecalcHasClipParent;
      RecalcHasEffect;
      FControls.Add(AsControl);
      if HasEffect then
        UpdateEffects;
      if AsControl.Align <> TAlignLayout.None then
        Realign;
      NeedRepaint := True;
    end;
    RefreshInheritedCursorForChildren;
    if NeedRepaint then
      RepaintJointArea(AObject);
  finally
    DisableDisappear := False;
  end;
end;

procedure TControl.DoRemoveObject(const AObject: TFmxObject);

  //https://quality.embarcadero.com/browse/RSP-21013
  //procedure ResetUpdatingState(const AObject: TFmxObject);
  //var
  //  I: Integer;
  //begin
  //  if AObject is TControl then
  //    TControl(AObject).FUpdating := 0;
  //  for I := 0 to AObject.ChildrenCount - 1 do
  //    ResetUpdatingState(AObject.Children[I]);
  //end;

var
  LParent: TFmxObject;
  NeedUpdate: Boolean;
  TabStop: IControl;
  AsControl: TControl;
  i: integer; // https://quality.embarcadero.com/browse/RSP-21013
begin
  DisableDisappear := True;
  try
    inherited;
    if (FTabList <> nil) and Supports(AObject, IControl, TabStop) then
      GetTabList.Remove(TabStop);
    NeedUpdate := False;
    if not (csDestroying in ComponentState) then
    begin
      LParent := Parent;
      while (LParent <> nil) and (not (LParent is TCommonCustomForm)) do
        LParent := LParent.Parent;
      NeedUpdate := LParent <> nil;
    end;
    if AObject is TControl then
      AsControl := TControl(AObject)
    else
      AsControl := nil;
    //https://quality.embarcadero.com/browse/RSP-21013
    //ResetUpdatingState(AObject);
    if AsControl <> nil then
    begin
      if FControls <> nil  then
        FControls.Remove(AsControl);
      //https://quality.embarcadero.com/browse/RSP-21013
      if (not (csDestroying in ComponentState)) and
         (not (csDestroying in AsControl.ComponentState)) then
        for I := 1 to FUpdating do
          AsControl.endUpdate;
      RepaintJointArea(AObject);
      AsControl.SetNewScene(nil);
      if NeedUpdate and (AsControl.Align <> TAlignLayout.None) then
        Realign;
    end;
    if AObject is TEffect then
    begin
      RemoveFromEffectsList(TEffect(AObject));
      if NeedUpdate then
      begin
        Repaint;
        RecalcHasEffect;
      end;
    end;
    if AsControl <> nil then
    begin
      if NeedUpdate then
        ChildrenAlignChanged;
      if AsControl.TempCanvas <> nil then
        AsControl.TempCanvas := nil;
      if AsControl.FInPaintTo then
        AsControl.FInPaintTo := False;
    end;
  finally
    DisableDisappear := False;
  end;
end;

{ matrix }

procedure TControl.MatrixChanged(Sender: TObject);
begin
  DoMatrixChanged(Sender);
end;

procedure TControl.RecalcUpdateRect;
var
  I: Integer;
begin
  FRecalcUpdateRect := True;
  if FControls <> nil then
    for I := GetFirstVisibleObjectIndex to GetLastVisibleObjectIndex - 1 do
      if I < FControls.Count then
        FControls[I].RecalcUpdateRect;
end;

function TControl.GetUpdateRect: TRectF;
begin
  if FRecalcUpdateRect then
  begin
    FRecalcUpdateRect := False;
    FUpdateRect := DoGetUpdateRect;
  end;
  Result := FUpdateRect;
end;

function TControl.GetAxisAlignedRect: TRectF;
begin
  Result := NormalizeRectF([LocalToAbsolute(TPointF.Zero), LocalToAbsolute(TPointF.Create(Width, 0)),
    LocalToAbsolute(TPointF.Create(Width, Height)), LocalToAbsolute(TPointF.Create(0, Height))]);
end;

function TControl.DoGetUpdateRect: TRectF;

  function SafeUpdateRect(const P: TControl): TRectF;
  begin
    try
      DisableDisappear := True;
      Result := P.UpdateRect;
    finally
      DisableDisappear := False;
    end;
  end;

var
  R, CanvasRect: TRectF;
  P: TControl;
  I: Integer;
begin
  FUpdating := FUpdating + 1;
  Result := AxisAlignedRect;
  if not (csLoading in ComponentState) then
  begin
    P := ParentControl;
    while P <> nil do
    begin
      if P.ClipChildren or P.SmallSizeControl then
        IntersectRect(Result, Result, SafeUpdateRect(P));
      P := P.ParentControl;
    end;
    if Canvas <> nil then
    begin
      CanvasRect := TRectF.Create(0, 0, Canvas.Width, Canvas.Height);
      if not FInPaintTo and IntersectRect(Result, CanvasRect) then
        IntersectRect(Result, Result, CanvasRect);
    end;
    if not Result.IsEmpty then
    begin
      { Focused }
      if CanFocus and IsFocused then
        InflateRect(Result, 5, 5);
      { Effects }
      if HasEffect and not (ClipChildren or SmallSizeControl) then
      begin
        R := GetEffectsRect;
        R := NormalizeRectF([LocalToAbsolute(R.TopLeft), LocalToAbsolute(PointF(R.Right, R.Top)),
          LocalToAbsolute(R.BottomRight), LocalToAbsolute(PointF(R.Left, R.Bottom))]);
        Result := UnionRect(Result, R);
      end;
      { Children }
      if not (ClipChildren or SmallSizeControl) and (ControlsCount > 0) then
      begin
        for I := GetLastVisibleObjectIndex - 1 downto GetFirstVisibleObjectIndex do
        begin
          if not FControls[I].Visible then
            Continue;
          R := FControls[I].UpdateRect;
          Result := UnionRect(Result, R);
        end;
      end;
    end
    else
      Disappear;
  end;
  FUpdating := FUpdating - 1;
end;

function TControl.GetVisible: Boolean;
begin
  Result := Visible;
end;

function TControl.GetWidth: Single;
begin
  if FSize.PlatformDefault then
    Result := FSize.DefaultValue.Width
  else
    Result := FSize.Width;
end;

function TControl.GetChildrenRect: TRectF;
var
  I: Integer;
  Control: TControl;
begin
  Result := AbsoluteRect;
  { children }
  if not (ClipChildren or SmallSizeControl) and (FControls <> nil) then
    for I := GetFirstVisibleObjectIndex to GetLastVisibleObjectIndex - 1 do
    begin
      Control := FControls[I];
      if Control.Visible then
        Result := UnionRect(Result, Control.GetChildrenRect);
    end
end;

function TControl.GetAbsoluteWidth: Single;
begin
  Result := LocalToAbsoluteVector(Vector(Width, Height)).X;
end;

function TControl.GetAlign: TAlignLayout;
begin
  Result := FAlign;
end;

function TControl.GetAllowAlign: Boolean;
begin
  Result := Visible or ((FAnchors * [TAnchorKind.akRight, TAnchorKind.akBottom] <> []) and (FAlign = TAlignLayout.None));
end;

function TControl.GetAnchors: TAnchors;
begin
  Result := FAnchors;
end;

function TControl.GetAnchorRules : TPointF;
begin
  Result := FAnchorRules;
end;

function TControl.GetAnchorOrigin : TPointF;
begin
  Result := FAnchorOrigin;
end;

function TControl.GetAnchorMove: Boolean;
begin
  Result := FAnchorMove;
end;

procedure TControl.SetAnchorMove(Value: Boolean);
begin
  FAnchorMove := Value;
end;

function TControl.GetOriginalParentSize : TPointF;
begin
  Result := FOriginalParentSize;
end;

function TControl.GetAbsoluteHeight: Single;
begin
  Result := LocalToAbsoluteVector(Vector(Width, Height)).Y;
end;

function TControl.GetAbsoluteScale: TPointF;
begin
  Result := AbsoluteMatrix.ExtractScale;
end;

function TControl.GetChildrenMatrix(var Matrix: TMatrix; var Simple: Boolean): Boolean;
begin
  Simple := True;
  Result := False;
end;

function TControl.GetAbsoluteMatrix: TMatrix;
var
  ChildrenMatrix, FinalLocalMatrix: TMatrix;
  SimpleChildrenTransform: Boolean;
  LSimpleTransform: Boolean;
begin
  if FRecalcAbsolute then
  begin
    LSimpleTransform := FSimpleTransform;
    if FParentControl <> nil then
    begin
      if FParentControl.GetChildrenMatrix(ChildrenMatrix, SimpleChildrenTransform) then
      begin
        FinalLocalMatrix := FLocalMatrix * ChildrenMatrix;
        FSimpleTransform := FSimpleTransform and SimpleChildrenTransform;
      end
      else
        FinalLocalMatrix := FLocalMatrix;

      if FParentControl.FSimpleTransform and FSimpleTransform then
      begin
        FAbsoluteMatrix := FParentControl.AbsoluteMatrix;
        FAbsoluteMatrix.m31 := FAbsoluteMatrix.m31 + FinalLocalMatrix.m31;
        FAbsoluteMatrix.m32 := FAbsoluteMatrix.m32 + FinalLocalMatrix.m32;
        FInvAbsoluteMatrix := FAbsoluteMatrix;
        FInvAbsoluteMatrix.m31 := -FInvAbsoluteMatrix.m31;
        FInvAbsoluteMatrix.m32 := -FInvAbsoluteMatrix.m32;
      end else begin
        if not FParentControl.FSimpleTransform then
          FSimpleTransform := False;
        FAbsoluteMatrix := FinalLocalMatrix * FParentControl.AbsoluteMatrix;
        FInvAbsoluteMatrix := FAbsoluteMatrix.Inverse;
      end;
    end
    else
    begin
      FAbsoluteMatrix := FLocalMatrix;
      FInvAbsoluteMatrix := FAbsoluteMatrix.Inverse;
    end;
    Result := FAbsoluteMatrix;
    FRecalcAbsolute := False;
    if LSimpleTransform <> FSimpleTransform then
      RecalcAbsolute;
  end
  else
    Result := FAbsoluteMatrix;
end;

function TControl.GetInvertAbsoluteMatrix: TMatrix;
begin
  AbsoluteMatrix; // Force recalaculation if need
  Result := FInvAbsoluteMatrix;
end;

procedure TControl.RecalcAbsoluteNow;
var
  I: Integer;
begin
  AbsoluteMatrix;
  // recalc
  if FControls <> nil then
    for I := 0 to FControls.Count - 1 do
      FControls[I].RecalcAbsoluteNow;
end;

procedure TControl.RecalcAbsolute;
var
  I: Integer;
begin
  if not FRecalcAbsolute then
  begin
    FRecalcAbsolute := True;
    DoAbsoluteChanged;
    if FControls <> nil then
      for I := 0 to FControls.Count - 1 do
      begin
        if not FSimpleTransform then
          FControls[I].FSimpleTransform := False;
        FControls[I].RecalcAbsolute;
      end;
  end;
end;

procedure TControl.ActionChange(Sender: TBasicAction; CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then
  begin
    if (not CheckDefaults) or (Hint = '') then
      Hint := TCustomAction(Sender).Hint;
    if not CheckDefaults then
    begin
      Visible := TCustomAction(Sender).Visible;
      Enabled := TCustomAction(Sender).Enabled;
    end;
    if (EnableExecuteAction) and ((not CheckDefaults) or (@OnClick = nil)) then
      OnClick := TCustomAction(Sender).OnExecute;
    if Sender is TCustomControlAction then
    begin
      if not CheckDefaults or (PopupMenu = nil) then
        PopupMenu := TCustomControlAction(Sender).PopupMenu;
    end;
  end;
  inherited;
end;

function TControl.AbsoluteToLocalVector(Vector: TVector): TVector;
begin
  Vector.W := 0;
  if FInPaintTo then
    Result := Vector * FInPaintToAbsMatrix
  else
    if FSimpleTransform then
      Result := Vector
    else
      Result := Vector * InvertAbsoluteMatrix;
end;

function TControl.LocalToAbsoluteVector(Vector: TVector): TVector;
begin
  Vector.W := 0;
  if FInPaintTo then
    Result := Vector * FInPaintToInvMatrix
  else
    if FSimpleTransform then
      Result := Vector
    else
      Result := Vector * AbsoluteMatrix;
end;

function TControl.AbsoluteToLocal(const Point: TPointF): TPointF;
begin
  if FInPaintTo then
    Result := Point * FInPaintToInvMatrix
  else
  begin
    if FSimpleTransform then
    begin
      Result.X := Point.X + InvertAbsoluteMatrix.m31;
      Result.Y := Point.Y + InvertAbsoluteMatrix.m32;
    end
    else
      Result := Point * InvertAbsoluteMatrix;
  end;
end;

function TControl.LocalToAbsolute(const Point: TPointF): TPointF;
begin
  if FInPaintTo then
    Result := Point * FInPaintToAbsMatrix
  else
  begin
    if FSimpleTransform then
    begin
      Result.X := Point.X + AbsoluteMatrix.m31;
      Result.Y := Point.Y + AbsoluteMatrix.m32;
    end
    else
      Result := Point * AbsoluteMatrix;
  end;
end;

{ Opacity }

function TControl.GetAbsoluteOpacity: Single;
begin
  if FRecalcOpacity then
  begin
    if FParentControl <> nil then
      FAbsoluteOpacity := FOpacity * FParentControl.AbsoluteOpacity
    else
      FAbsoluteOpacity := FOpacity;
    // Reduce the density of the inaccessible control
    if not Enabled then
      FAbsoluteOpacity := FAbsoluteOpacity * DisabledOpacity;
    FRecalcOpacity := False;
  end;
  Result := FAbsoluteOpacity;
end;

procedure TControl.RecalcOpacity;
var
  I: Integer;
begin
  if FRecalcOpacity then Exit;
  FRecalcOpacity := True;
  if FControls <> nil then
    for I := 0 to FControls.Count - 1 do
      FControls[I].RecalcOpacity;
end;

procedure TControl.RecalcSize;
begin
  if FSize.PlatformDefault then
    Size.DefaultValue := GetDefaultSize;
  SizeChanged(FSize);
end;

{ methods }

class constructor TControl.Create;
begin
  FEmptyControlList := TControlList.Create;
end;

class destructor TControl.Destroy;
begin
  FreeAndNil(FEmptyControlList);
end;

function TControl.PointInObject(X, Y: Single): Boolean;
var
  P: TPointF;
begin
  P := AbsoluteToLocal(PointF(X, Y));
  Exit(PointInObjectLocal(P.X, P.Y));
end;

function TControl.PointInObjectLocal(X, Y: Single): Boolean;
begin
  Result := (X >= (0 - TouchTargetExpansion.Left))
        and (X <= (Width + TouchTargetExpansion.Right))
        and (Y >= (0 - TouchTargetExpansion.Top))
        and (Y <= (Height + TouchTargetExpansion.Bottom));
end;

function TControl.ScreenToLocal(P: TPointF): TPointF;
begin
  if Scene <> nil then
    Result := AbsoluteToLocal(Scene.ScreenToLocal(P))
  else
    Result := AbsoluteToLocal(P);
end;

function TControl.LocalToScreen(P: TPointF): TPointF;
begin
  if Scene <> nil then
    Result := Scene.LocalToScreen(LocalToAbsolute(P))
  else
    Result := LocalToAbsolute(P);
end;

procedure TControl.ChangeChildren;
var
  I, C: Integer;
  Changes: Integer;
begin
  inherited;
  Changes := 0;
  if not(csLoading in ComponentState) and (FControls <> nil) then
  begin
    C := 0;
    for I := 0 to ChildrenCount - 1 do
      if (Children[I] is TControl) then
      begin
        if C = FControls.Count then
          FControls.Add(TControl(Children[I]))
        else
          if FControls[C] <> TControl(Children[I]) then
          begin
            Inc(Changes);
            FControls[C] := TControl(Children[I]);
          end;
        Inc(C);
      end;
    while C < Controls.Count do
    begin
      Inc(Changes);
      FControls.Delete(FControls.Count - 1);
    end;
    if Changes > 0 then
      Realign;
  end;
end;

procedure TControl.ChangeOrder;
var
  AlignRoot: IAlignRoot;
begin
  inherited;
  if not(csLoading in ComponentState) and Supports(Parent, IAlignRoot, AlignRoot) then
    AlignRoot.Realign;
end;

function TControl.CheckForAllowFocus: Boolean;
begin
  Result := ParentedVisible and CanFocus and AbsoluteEnabled
end;

function TControl.CheckHitTest(const AHitTest: Boolean): Boolean;
begin
  Result := AHitTest;
  if csDesigning in ComponentState then
  begin
    if Visible then
      Result := True;
    if FLocked then
      Result := False;
  end;
end;

procedure TControl.ChildrenAlignChanged;
var
  I: Integer;
  AlignObject: IAlignableObject;
begin
  FNeedAlign := False;
  if Children <> nil then
    for I := 0 to ChildrenCount - 1 do
      if Supports(Children[I], IAlignableObject, AlignObject) and
         ((AlignObject.Align <> TAlignLayout.None) or (AlignObject.Anchors <> [TAnchorKind.akLeft, TAnchorKind.akTop])) then
      begin
        FNeedAlign := True;
        Break;
      end;
end;

function TControl.FillTextFlags: TFillTextFlags;
begin
  if (Root <> nil) and (Root.BiDiMode = bdRightToLeft) then
  begin
    Result := [TFillTextFlag.RightToLeft]
  end
  else
    Result := [];
end;

function TControl.FindTarget(P: TPointF; const Data: TDragObject): IControl;
var
  I: Integer;
  NewObj: IControl;
  LP: TPointF;
begin
  Result := nil;
  if not Visible then
    Exit;
  if not AbsoluteEnabled and not(csDesigning in ComponentState) then
    Exit;
  LP := P;
  if FScene <> nil then
    LP := FScene.ScreenToLocal(LP);
  if (ClipChildren or SmallSizeControl) and not PointInObject(LP.X, LP.Y) then
    Exit;
  if Children <> nil then
    for I := ChildrenCount - 1 downto 0 do
    begin
      if not Supports(TFmxObject(Children[I]), IControl, NewObj) then
        Continue;
      if not NewObj.Visible then
        Continue;

      NewObj := NewObj.FindTarget(P, Data);
      if NewObj <> nil then
        Exit(NewObj);
    end;

  if PointInObject(LP.X, LP.Y) and CheckHitTest(HitTest) then
    Result := Self;
end;

function TControl.ShouldTestMouseHits: Boolean;
begin
  Result := Visible and (AbsoluteEnabled or (csDesigning in ComponentState));
end;

function TControl.ObjectAtPoint(P: TPointF): IControl;
var
  I: Integer;
  NewObj: IControl;
  Control: TControl;
  LP: TPointF;
begin
  if not ShouldTestMouseHits then
    Exit(nil);
  LP := P;
  if FScene <> nil then
    LP := FScene.ScreenToLocal(LP);
  if (ClipChildren or SmallSizeControl) and not PointInObject(LP.X, LP.Y) then
    Exit(nil);
  if ControlsCount > 0 then
    for I := GetLastVisibleObjectIndex - 1 downto GetFirstVisibleObjectIndex do
    begin
      Control := Controls[I];
      if not Control.GetVisible then
        Continue;

      NewObj := Control.ObjectAtPoint(P);
      if NewObj <> nil then
        Exit(NewObj);
    end;

  Result := nil;
  if PointInObject(LP.X, LP.Y) and CheckHitTest(HitTest) then
    Result := Self;
end;

function TControl.OnClickStored: Boolean;
begin
  Result := (not (ActionClient and (ActionLink <> nil) and (ActionLink.OnExecuteLinked) and
    (Action is TContainedAction)))
end;

function TControl.GetCanvas: TCanvas;
begin
  if FTempCanvas <> nil then
    Result := FTempCanvas
  else
    if FScene <> nil then
      Result := FScene.GetCanvas
    else
      Result := nil;
end;

procedure TControl.RefreshInheritedCursor;

  function GetParentInheritedCursor: TCursor;
  begin
    Result := crDefault;
    if ParentControl <> nil then
      Result := ParentControl.InheritedCursor;
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

procedure TControl.RefreshInheritedCursorForChildren;
var
  ChildControl: TControl;
begin
  if not IsUpdating and (Controls.Count > 0) then
    for ChildControl in Controls do
      if ChildControl.Cursor = crDefault then
         ChildControl.RefreshInheritedCursor;
end;

procedure TControl.SetInPaintTo(Value: Boolean);
var
  I: Integer;
begin
  FInPaintTo := Value;
  FInPaintToAbsMatrix := AbsoluteMatrix;
  FInPaintToInvMatrix := InvertAbsoluteMatrix;
  if FControls <> nil then
    for I := 0 to FControls.Count - 1 do
      FControls[I].SetInPaintTo(Value);
end;

function TControl.GetIsFocused: Boolean;
begin
  Result := FIsFocused;
end;

procedure TControl.UpdateParentProperties;
begin
  FParentControl := nil;
  FParentContent := nil;
  if FParent <> nil then
  begin
    if FParent is TControl then
      FParentControl := TControl(FParent);
    FParent.GetInterface(IContent, FParentContent);
  end;
end;

procedure TControl.ParentChanged;
begin
  inherited;
  UpdateParentProperties;
  RefreshInheritedCursor;
  if not (csLoading in ComponentState) then
    AncestorParentChanged;
end;

procedure TControl.PaintTo(const ACanvas: TCanvas; const ARect: TRectF; const AParent: TFmxObject = nil);
var
  SaveTempCanvas: TCanvas;
  SaveDisableAlign: Boolean;
  SavePos: TPointF;
  SaveScale: TPointF;
  SaveParent: TFmxObject;
  SaveRotate: Single;
  SaveInPaintTo: Boolean;
begin
  if IsControlRectEmpty then
    Exit;

  if Scene <> nil then
    Scene.DisableUpdating;
  SaveDisableAlign := FDisableAlign;
  SaveInPaintTo := FInPaintTo;
  FDisableAlign := True;
  try
    SetInPaintTo(True);
    SaveTempCanvas := TempCanvas;
    try
      TempCanvas := ACanvas;
      SavePos := Position.Point;
      SaveScale := Scale.Point;
      SaveParent := FParent;
      SaveRotate := RotationAngle;
      try
        FParent := AParent;
        UpdateParentProperties;

        FPosition.SetPointNoChange(TPointF.Create(ARect.Left, ARect.Top));
        FScale.SetPointNoChange(TPointF.Create(ARect.Width / Width, ARect.Height / Height));
        FRotationAngle := 0;
        MatrixChanged(Self);

        RecalcHasEffect;
        TempCanvas.SetMatrix(AbsoluteMatrix);
        PaintInternal;

        FRotationAngle := SaveRotate;
        FPosition.SetPointNoChange(SavePos);
        FScale.SetPointNoChange(SaveScale);
      finally
        FParent := SaveParent;
        UpdateParentProperties;
      end;
      MatrixChanged(Self);
      RecalcUpdateRect;
      RecalcAbsoluteNow;
      RecalcOpacity;
      RecalcEnabled;
      RecalcHasEffect;
    finally
      TempCanvas := SaveTempCanvas;
    end;
  finally
    SetInPaintTo(SaveInPaintTo);
    FDisableAlign := SaveDisableAlign;
    if Scene <> nil then
      Scene.EnableUpdating;
  end;
end;

procedure TControl.UpdateEffects;
begin
  if HasEffect then
    FUpdateEffects := True;
  if FParentControl <> nil then
    FParentControl.UpdateEffects;
end;

procedure TControl.UpdateExplicitBounds;
begin
  if not (csReading in ComponentState) then
  begin
    FExplicitLeft := FPosition.X;
    FExplicitTop := FPosition.Y;
    FExplicitWidth := FSize.Width;
    FExplicitHeight := FSize.Height;
  end;
end;

procedure TControl.ApplyEffect;
var
  R: TRectF;
  Effect: TEffect;
  EffectRect: TRectF;
  SceneScale: Single;
  S: TPointF;
  State: TCanvasSaveState;
begin
  if (Children = nil) or (FScene = nil) or FDisableEffect or not HasEffect then
    Exit;

  FApplyingEffect := True;
  try
    SceneScale := Scene.GetSceneScale;
    if not FUpdateEffects then
    begin
      if (FEffectBitmap <> nil) and (FEffects <> nil) and (FEffects.Count > 0) then
      begin
        Canvas.SetMatrix(AbsoluteMatrix);
        for Effect in FEffects do
          if Effect.Enabled then
          begin
            EffectRect := Effect.GetRect(TRectF.Create(0, 0, Width, Height));
            Canvas.DrawBitmap(FEffectBitmap, TRectF.Create(0, 0, FEffectBitmap.Width, FEffectBitmap.Height), EffectRect,
              AbsoluteOpacity, RotationAngle = 0);
            Break;
          end;
      end;
    end
    else
    begin
      if (FEffects <> nil) and (FEffects.Count > 0) then
        for Effect in FEffects do
          if Effect.Enabled then
          begin
            EffectRect := Effect.GetRect(TRectF.Create(0, 0, Width, Height));
            S := GetAbsoluteScale;
            MultiplyRect(EffectRect, S.X, S.Y);
            MultiplyRect(EffectRect, SceneScale, SceneScale);
            if FEffectBitmap = nil then
              FEffectBitmap := TBitmap.Create(Trunc(EffectRect.Width), Trunc(EffectRect.Height))
            else if (FEffectBitmap.Width <> Trunc(EffectRect.Width)) or (FEffectBitmap.Height <> Trunc(EffectRect.Height)) then
              FEffectBitmap.SetSize(Trunc(EffectRect.Width), Trunc(EffectRect.Height));
            FEffectBitmap.BitmapScale := SceneScale;
            { Paint Self }
            R := TRectF.Create(Effect.GetOffset.X, Effect.GetOffset.Y, (Effect.GetOffset.X + Width),
              (Effect.GetOffset.Y + Height));
            MultiplyRect(R, S.X, S.Y);
            if not (TEffectStyle.DisablePaintToBitmap in Effect.EffectStyle) then
            begin
              if FEffectBitmap.Canvas.BeginScene then
              try
                FEffectBitmap.Canvas.Clear(0);
                PaintTo(FEffectBitmap.Canvas, R);
              finally
                FEffectBitmap.Canvas.EndScene;
              end;
            end
            else
            begin
              FEffectBitmap.ClearRect(R, 0);
            end;
            { apply effects }
            State := Canvas.SaveState;
            try
              Effect.ProcessEffect(FEffectBitmap.Canvas, FEffectBitmap, S.X);
            finally
              Canvas.RestoreState(State);
            end;
            { draw effectBitmap }
            MultiplyRect(EffectRect, 1 / S.X, 1 / S.Y);
            MultiplyRect(EffectRect, 1 / SceneScale, 1 / SceneScale);
            Canvas.SetMatrix(AbsoluteMatrix);
            Canvas.DrawBitmap(FEffectBitmap, TRectF.Create(0, 0, FEffectBitmap.Width, FEffectBitmap.Height), EffectRect,
              AbsoluteOpacity, RotationAngle = 0);
            Break;
          end;
      FUpdateEffects := False;
    end;
  finally
    FApplyingEffect := False;
  end;
end;

procedure TControl.ApplyTriggerEffect(const AInstance: TFmxObject; const ATrigger: string);
begin
  TEffectAnimator.DefaultApplyTriggerEffect(Self, AInstance, ATrigger);
end;

procedure TControl.Painting;
begin
  if Assigned(FOnPainting) then
    FOnPainting(Self, Canvas, LocalRect);
end;

procedure TControl.DrawDesignBorder(const VertColor: TAlphaColor = DesignBorderColor;
  const HorzColor: TAlphaColor = DesignBorderColor);
const
  Dash: array [Boolean] of TStrokeDash = (TStrokeDash.Dot, TStrokeDash.Dash);
var
  R: TRectF;
  State: TCanvasSaveState;
begin
  if not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    State := Canvas.SaveState;
    try
      Canvas.Stroke.Kind := TBrushKind.Solid;
      Canvas.Stroke.Dash := Dash[Visible];
      Canvas.Stroke.Color := VertColor;
      Canvas.Stroke.Thickness := 1;
      if VertColor = HorzColor then
        Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity)
      else
      begin
        Canvas.DrawLine(R.TopLeft, TPointF.Create(R.Left, R.Bottom), AbsoluteOpacity);
        Canvas.DrawLine(TPointF.Create(R.Right, R.Top), R.BottomRight, AbsoluteOpacity);
        Canvas.Stroke.Color := HorzColor;
        Canvas.DrawLine(R.TopLeft, TPointF.Create(R.Right, R.Top), AbsoluteOpacity);
        Canvas.DrawLine(TPointF.Create(R.Left, R.Bottom), R.BottomRight, AbsoluteOpacity);
      end;
    finally
      Canvas.RestoreState(State);
    end;
  end;
end;

procedure TControl.PaddingChanged;
begin
end;

procedure TControl.Paint;
begin
end;

procedure TControl.PaintInternal;
const
  DragHighlightThickness = 3;
var
  SupportsStage: Boolean;
  ClipParentObject: TControl;

  procedure DoPaintInternal;
  var
    State: TCanvasSaveState;
  begin
    State := nil;
    if ClipParentObject <> nil then
    begin
      Canvas.SetMatrix(ClipParentObject.AbsoluteMatrix);
      State := Canvas.SaveState;
      Canvas.ExcludeClipRect(ClipParentObject.LocalRect);
    end;
    try
      Canvas.SetMatrix(AbsoluteMatrix);
      Painting;
      if SupportsStage then
      begin
        Canvas.SetMatrix(AbsoluteMatrix);
        Paint;
      end;
    finally
      if State <> nil then
        Canvas.RestoreState(State);
    end;
    Canvas.SetMatrix(AbsoluteMatrix);
    PaintChildren;
    Canvas.SetMatrix(AbsoluteMatrix);
    DoPaint;
    AfterPaint;
  end;

  procedure PaintAndClipChild;
  var
    State: TCanvasSaveState;
  begin
    if SupportsStage and HasEffect and not HasAfterPaintEffect and not ApplyingEffect then
      ApplyEffect;
    Canvas.SetMatrix(AbsoluteMatrix);
    if (ClipChildren or SmallSizeControl) then
    begin
      State := Canvas.SaveState;
      try
        Canvas.IntersectClipRect(ClipRect);
        DoPaintInternal;
      finally
        Canvas.RestoreState(State);
      end;
    end
    else
      DoPaintInternal;
    if SupportsStage and HasAfterPaintEffect and not ApplyingEffect then
    begin
      Canvas.SetMatrix(AbsoluteMatrix);
      ApplyEffect;
    end;
  end;

var
  R: TRectF;

begin
  if FDisablePaint or IsControlRectEmpty then
    Exit;
  SupportsStage := SupportsPaintStage(FPaintStage);
  if not HasDisablePaintEffect or FInPaintTo then
  begin
    ClipParentObject := HasClipParent;
    PaintAndClipChild;
  end;
  if SupportsStage and HasDisablePaintEffect and not ApplyingEffect then
  begin
    Canvas.SetMatrix(AbsoluteMatrix);
    ApplyEffect;
  end;

  if IsDragOver and EnableDragHighlight then
  begin
    Canvas.SetMatrix(AbsoluteMatrix);
    Canvas.Stroke.Kind := TBrushKind.Solid;
    Canvas.Stroke.Color := $B2005ACC;
    Canvas.Stroke.Cap := TStrokeCap.Flat;
    Canvas.Stroke.Join := TStrokeJoin.Miter;
    Canvas.Stroke.Dash := TStrokeDash.Solid;
    Canvas.Stroke.Thickness := DragHighlightThickness;
    R := LocalRect;
    InflateRect(R, -DragHighlightThickness / 2, -DragHighlightThickness / 2);
    Canvas.DrawRect(R, 1, 1, AllCorners, 1);
    Canvas.Stroke.Dash := TStrokeDash.Solid;
  end;
  {$IFDEF MSWINDOWS}
  if FDesignSelectionMarks and (Root <> nil) and (Root.GetObject is TCommonCustomForm) and
    (TCommonCustomForm(Root.GetObject).Designer <> nil) and
    TCommonCustomForm(Root.GetObject).Designer.IsSelected(Self) then
      TCommonCustomForm(Root.GetObject).Designer.DrawSelectionMarks(Self);
  {$ENDIF}
end;

procedure TControl.DoPaint;
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self, Canvas, LocalRect);
end;

procedure TControl.AfterPaint;
begin
end;

procedure TControl.AncestorParentChanged;
var
  I: Integer;
begin
  for I := 0 to Controls.Count - 1 do
    Controls[I].AncestorParentChanged;
end;

procedure TControl.AncestorVisibleChanged(const Visible: Boolean);
var
  I: Integer;
begin
  for I := 0 to Controls.Count - 1 do
    Controls[I].AncestorVisibleChanged(Visible);
end;

procedure TControl.PrepareForPaint;
var
  I, J: Integer;
  R: TRectF;
  AllowPaint: Boolean;
  Control: TControl;
begin
  if (FScene <> nil) and (ControlsCount > 0) then
    for I := GetFirstVisibleObjectIndex to GetLastVisibleObjectIndex - 1 do
      if FControls[I].Visible then
      begin
        Control := FControls[I];
        if Control.FScene = nil then
          Continue;
        if not Control.FInPaintTo and Control.UpdateRect.IsEmpty then
          Continue;
        if (ClipChildren or SmallSizeControl) and not IntersectRect(UpdateRect, Control.UpdateRect) then
          Continue;

        AllowPaint := False;
        if Control.FInPaintTo then
          AllowPaint := True;
        if not AllowPaint then
        begin
          R := UnionRect(Control.GetChildrenRect, Control.UpdateRect);
          for J := 0 to FScene.GetUpdateRectsCount - 1 do
            if IntersectRect(FScene.GetUpdateRect(J), R) then
            begin
              AllowPaint := True;
              Break;
            end;
        end;

        if AllowPaint then
          Control.PrepareForPaint;
      end;
end;

procedure TControl.PaintChildren;
var
  I, J: Integer;
  R: TRectF;
  AllowPaint: Boolean;
  Control: TControl;
begin
  if (FScene <> nil) and (ControlsCount > 0) then
    for I := GetFirstVisibleObjectIndex to GetLastVisibleObjectIndex - 1 do
      if FControls[I].Visible then
      begin
        Control := FControls[I];
        if Control.FScene = nil then
          Continue;
        if not Control.FInPaintTo and Control.UpdateRect.IsEmpty then
          Continue;
        if (ClipChildren or SmallSizeControl) and not IntersectRect(Self.UpdateRect, Control.UpdateRect) then
          Continue;


        AllowPaint := False;
        if Control.FInPaintTo then
          AllowPaint := True;
        if not AllowPaint then
        begin
          R := UnionRect(Control.GetChildrenRect, Control.UpdateRect);
          for J := 0 to FScene.GetUpdateRectsCount - 1 do
            if IntersectRect(FScene.GetUpdateRect(J), R) then
            begin
              AllowPaint := True;
              Break;
            end;
        end;

        if AllowPaint then
          Control.PaintInternal;
      end;
end;

function TControl.GetParentedVisible: Boolean;
var
  P: TFmxObject;
  Control: IControl;
begin
  P := Self;
  Result := False;
  while P <> nil do
  begin
    if Supports(P, IControl, Control) and (not Control.Visible) then
      Exit;
    P := P.Parent;
  end;
  Result := True;
end;

function TControl.GetPopupMenu: TCustomPopupMenu;
begin
  Result := FPopupMenu;
end;

function TControl.IsAnchorsStored: Boolean;
begin
  Result := Anchors <> AnchorAlign[Align];
end;

function TControl.IsControlRectEmpty: Boolean;
begin
  Result := (Width < 1) or (Height < 1);
end;

function TControl.CanRepaint: Boolean;
begin
  Result := (not IsUpdating) and (not FInPaintTo) and (FScene <> nil) and (not (csDestroying in ComponentState)) and
    Visible and ParentedVisible;
end;

procedure TControl.RepaintRect(const Rect: TRectF);
begin
  if CanRepaint and (not Rect.IsEmpty) then
  begin
    if HasDisablePaintEffect then
      UpdateEffects;
    if (Canvas <> nil) and (TCanvasStyle.SupportClipRects in Canvas.GetCanvasStyle) then
      FScene.AddUpdateRect(Rect)
    else
      FScene.AddUpdateRect(NullRect);
  end;
end;

procedure TControl.Repaint;
var
  R: TRectF;
begin
  if CanRepaint then
  begin
    if HasDisablePaintEffect then
      UpdateEffects;
    if Canvas <> nil then
    begin
      if TCanvasStyle.SupportClipRects in Canvas.GetCanvasStyle then
      begin
        R := UpdateRect;
        if not R.IsEmpty then
          FScene.AddUpdateRect(R);
      end
      else
        FScene.AddUpdateRect(TRectF.Create(0, 0, Canvas.Width, Canvas.Height));
    end;
  end;
end;

procedure TControl.InvalidateRect(ARect: TRectF);
var
  P: array of TPointF;
begin
  if (FScene <> nil) and Visible and ParentedVisible then
  begin
    SetLength(P,4);
    P[0] := LocalToAbsolute(ARect.TopLeft);
    P[1] := LocalToAbsolute(TPointF.Create(ARect.Right, ARect.Top));
    P[2] := LocalToAbsolute(TPointF.Create(ARect.Left, ARect.Bottom));
    P[3] := LocalToAbsolute(ARect.BottomRight);
    FScene.AddUpdateRect(NormalizeRectF(P));
  end;
end;

procedure TControl.Move;
begin
end;

procedure TControl.ResetFocus;
begin
  if (Root <> nil) then
  begin
    if FIsFocused then
      Root.SetFocused(nil)
    else
      EnumControls(function (Control: TControl): TEnumProcResult
      begin
        if not Control.Visible then
          Result := TEnumProcResult.Discard
        else if Control.IsFocused then
        begin
          Root.SetFocused(nil);
          Result := TEnumProcResult.Stop;
        end
        else
          Result := TEnumProcResult.Continue;
      end);
  end;
end;

procedure TControl.Resize;
begin
  if Assigned(FOnResize) then
    FOnResize(Self);
end;

procedure TControl.DoResized;
begin
  if Assigned(FOnResized) then
    FOnResized(Self);
end;

procedure TControl.Lock;
var
  I: Integer;
begin
  Locked := True;
  if FControls <> nil then
    for I := 0 to FControls.Count - 1 do
      FControls[I].Lock;
end;

function TControl.GetFirstControlWithGesture(AGesture: TInteractiveGesture): TComponent;
var
  LGObj: IGestureControl;
begin
  Result := nil;
  if AGesture in Touch.InteractiveGestures then
    Result := Self
  else
  if (Parent <> nil) and Supports(Parent, IGestureControl, LGObj) then
    Result := LGObj.GetFirstControlWithGesture(AGesture);
end;

function TControl.GetFirstControlWithGestureEngine: TComponent;
var
  LGObj: IGestureControl;
begin
  Result := nil;
  if Touch.GestureEngine <> nil then
    Result := Self
  else
  if (Parent <> nil) and Supports(Parent, IGestureControl, LGObj) then
    Result := LGObj.GetFirstControlWithGestureEngine;
end;

function TControl.GetFirstVisibleObjectIndex: Integer;
begin
  Result := 0;
end;

function TControl.GetLastVisibleObjectIndex: Integer;
begin
  if FControls <> nil then
    Result := FControls.Count
  else
    Result := 0;
end;

function TControl.GetLeft: Single;
begin
  Result := Position.X;
end;

function TControl.GetListOfInteractiveGestures: TInteractiveGestures;
var
  LGObj: IGestureControl;
begin
  Result := Touch.InteractiveGestures;
  if Result = [] then
    if (Parent <> nil) and Supports(Parent, IGestureControl, LGObj) then
      Result := LGObj.GetListOfInteractiveGestures;                 
end;

function TControl.GetLocked: Boolean;
begin
  Result := FLocked;
end;

function TControl.GetAbsoluteRect: TRectF;
begin
  Result := TRectF.Create(LocalToAbsolute(TPointF.Zero), LocalToAbsolute(TPointF.Create(Width, Height)));
end;

function TControl.GetAbsoluteClipRect: TRectF;
var
  R: TRectF;
  LControl: TControl;
  LContent: IContent;
begin
  Result := TRectF.Empty;
  R := AbsoluteRect;
  if (Root = nil) or not (Root.GetObject is TCommonCustomForm and IntersectRect(R, R,
    TCommonCustomForm(Root.GetObject).ClientRect)) then
  begin
    LControl := ParentControl;
    while LControl <> nil do
    begin
      if LControl.ClipChildren and not (LControl.GetInterface(IContent, LContent) or IntersectRect(R, R,
        LControl.AbsoluteRect)) then
        Exit;
      LControl := LControl.ParentControl;
    end;
    Result := R;
  end;
end;

function TControl.GetClipRect: TRectF;
begin
  Result := TRectF.Create(0, 0, Width, Height);
end;

function TControl.GetContainerHeight: Single;
begin
  Result := Height;
end;

function TControl.GetContainerWidth: Single;
begin
  Result := Width;
end;

function TControl.GetControls: TControlList;
begin
  if FControls <> nil then
    Result := FControls
  else
    Result := FEmptyControlList;
end;

function TControl.GetControlsCount: Integer;
begin
  if FControls <> nil then
    Result := FControls.Count
  else
    Result := 0;
end;

function TControl.GetCursor: TCursor;
begin
  Result := FCursor;
end;

function TControl.GetObject: TFmxObject;
begin
  Result := Self;
end;

function TControl.GetDragMode: TDragMode;
begin
  Result := FDragMode;
end;

function TControl.GetMargins: TBounds;
begin
  Result := FMargins;
end;

procedure TControl.SetMargins(const Value: TBounds);
begin
  FMargins.Assign(Value);
end;

function TControl.GetPadding: TBounds;
begin
  Result := FPadding;
end;

procedure TControl.SetPadding(const Value: TBounds);
begin
  FPadding.Assign(Value);
end;

procedure TControl.SetParentShowHint(const Value: Boolean);
begin
  if Value <> FParentShowHint then
  begin
    FParentShowHint := Value;
    if Value then
      FShowHint := False;
  end;
end;

function TControl.GetParent: TFmxObject;
begin
  Result := Parent;
end;

function TControl.GetParentedRect: TRectF;
begin
  Result := GetBoundsRect;
end;

function TControl.GetLocalRect: TRectF;
begin
  Result := TRectF.Create(0, 0, Width, Height);
end;

function TControl.GetBoundsRect: TRectF;
begin
  Result := TRectF.Create(Position.X, Position.Y, Position.X + Width, Position.Y + Height);
end;

procedure TControl.SetBoundsRect(const Value: TRectF);
begin
  SetBounds(Value.Left, Value.Top, Value.Width, Value.Height);
end;

{ }

procedure TControl.MarginsChanged(Sender: TObject);
var
  AlignRoot: IAlignRoot;
begin
  UpdateSmallSizeControl;
  if not(csLoading in ComponentState) and Supports(Parent, IAlignRoot, AlignRoot) then
    AlignRoot.Realign;
end;

procedure TControl.PaddingChangedHandler(Sender: TObject);
begin
  UpdateSmallSizeControl;
  PaddingChanged;
  Realign;
end;

procedure TControl.BroadcastGesture(EventInfo: TGestureEventInfo);
var
  LItem: TGestureCollectionItem;
  LAction: TCustomAction;
  LGObj: IGestureControl;
begin
  // Find control that will respond to the gesture
  LItem := nil;
  if Self.Touch.GestureManager <> nil then
    if EventInfo.GestureID <> sgiNoGesture then
      LItem := TGestureCollectionItem(Self.Touch.GestureManager.FindGesture(Self, EventInfo.GestureID));

  if LItem <> nil then
  begin
    // Execute the action or notify the control
    if not(csDesigning in Self.ComponentState) and (LItem <> nil) and
      (LItem.Action <> nil) and (LItem.ActionLink <> nil) then
    begin
      if LItem.ActionLink.Action is TCustomAction then
        LAction := TCustomAction(LItem.ActionLink.Action)
      else
        LAction := nil;
     if LAction <> nil then
      begin
        if not LAction.Supported then
          Exit;
        LAction.Target := Self;
      end;
      try
        if not LItem.ActionLink.Execute(Self) then
          ExecuteAction(LItem.ActionLink.Action);
      finally
        if LAction <> nil then
          LAction.Target := nil;
      end;
    end
    else
    begin
      EventInfo.Location := ScreenToLocal(EventInfo.Location);
      CMGesture(EventInfo);
    end;
  end
  else if (Parent <> nil) and Supports(Parent, IGestureControl, LGObj) then
    LGObj.BroadcastGesture(EventInfo);
end;

procedure TControl.BeginUpdate;
var
  I: Integer;
begin
  if FUpdating = 0 then
    DoBeginUpdate;
  Inc(FUpdating);
  if FControls <> nil then
    for I := 0 to FControls.Count - 1 do
      FControls[I].BeginUpdate;
end;

procedure TControl.EndUpdate;
var
  I: Integer;
begin
  if IsUpdating then
  begin
    if FControls <> nil then
      for I := 0 to FControls.Count - 1 do
        FControls[I].EndUpdate;
    Dec(FUpdating);
    if not IsUpdating then
    begin
      DoEndUpdate;
      RefreshInheritedCursorForChildren;
    end;
  end;
end;

procedure TControl.EndUpdateNoChanges;
var
  I: Integer;
begin
  if IsUpdating then
  begin
    if FControls <> nil then
      for I := 0 to FControls.Count - 1 do
        FControls[I].EndUpdate;
    Dec(FUpdating);
  end;
end;

procedure TControl.DoBeginUpdate;
begin
end;

procedure TControl.DoEndUpdate;
begin
  Realign;
end;

procedure TControl.DoRealign;
begin
  if not FNeedAlign then
    Exit;
  AlignObjects(Self, FPadding, FSize.Width, FSize.Height, FLastWidth, FLastHeight, FDisableAlign);
end;

procedure TControl.Realign;
begin
  if csDestroying in ComponentState then
    Exit;
  if FDisableAlign then
    Exit;
  if IsUpdating then
    Exit;
  if csLoading in ComponentState then
  begin
    FLastWidth := FSize.Width;
    FLastHeight := FSize.Height;
    Exit;
  end;
  if HasEffect and (not SameValue(FLastWidth, FSize.Width, TEpsilon.Position) or
    not SameValue(FLastHeight, FSize.Height, TEpsilon.Position)) then
    UpdateEffects;
  DoRealign;
end;

{ events }

procedure TControl.KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, Key, KeyChar, Shift);
end;

procedure TControl.KeyUp(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self, Key, KeyChar, Shift);
end;

procedure TControl.DialogKey(var Key: Word; Shift: TShiftState);
var
  I: Integer;
  LP: TPointF;
begin
  if IsFocused and (((Shift = [ssShift]) and (Key = vkF10)) or ((Shift * [ssAlt] = []) and (Key = vkApps))) then
  begin
    LP := LocalToAbsolute(TPointF.Create(Width / 2, Height / 2));
    LP := Scene.LocalToScreen(LP);
    if ShowContextMenu(LP) then
    begin
      Key := 0;
      Exit;
    end;
  end;
  if ActionClient and Enabled and
    (Action is TCustomAction) and
    (TCustomAction(Action).IsDialogKey(Key, Shift)) then
  begin
    Key := 0;
    TCustomAction(Action).ShortCutPressed := True;
    try
      Click;
    except
      TCustomAction(Action).ShortCutPressed := False;
      Raise;
    end;
  end
  else if FControls <> nil then
    for I := 0 to FControls.Count - 1 do
      if (FControls[I].Visible or Supports(FControls[I], IContent)) and FControls[I].Enabled then
      begin
        FControls[I].DialogKey(Key, Shift);
        if Key = 0 then
          Break;
      end;
end;

procedure TControl.AfterDialogKey(var Key: Word; Shift: TShiftState);
var
  I: Integer;
begin
  if FControls <> nil then
    for I := 0 to FControls.Count - 1 do
      if (FControls[I].Visible or Supports(FControls[I], IContent)) and FControls[I].Enabled then
      begin
        FControls[I].AfterDialogKey(Key, Shift);
        if Key = 0 then
          Break;
      end;
end;

procedure TControl.Disappear;
begin
end;

procedure TControl.Capture;
begin
  if Root <> nil then
    Root.Captured := Self;
end;

procedure TControl.ReleaseCapture;
begin
  if (Root <> nil) and (Root.Captured <> nil) and (Root.Captured.GetObject = Self) then
    Root.SetCaptured(nil);
end;

procedure TControl.DoMatrixChanged(Sender: TObject);
var
  TranslateMatrix, ScaleMatrix, RotMatrix: TMatrix;
  M1, M2: TMatrix;
begin
  if (not FInPaintTo) and not IsUpdating then
    Repaint;
  if SameValue(FScale.X, 1.0, TEpsilon.Scale) and SameValue(FScale.Y, 1.0, TEpsilon.Scale) and SameValue(FRotationAngle, 0.0, TEpsilon.Scale) then
  begin
    if (FParentControl <> nil) and not FParentControl.FSimpleTransform then
      FSimpleTransform := False
    else
      FSimpleTransform := True;
  end
  else
    FSimpleTransform := False;

  if not FSimpleTransform then
  begin
    if not SameValue(FRotationAngle, 0.0, TEpsilon.Scale) then
    begin
      // scale
      ScaleMatrix := TMatrix.Identity;
      ScaleMatrix.m11 := FScale.X;
      ScaleMatrix.m22 := FScale.Y;
      FLocalMatrix := ScaleMatrix;
      // rotation
      if FRotationAngle <> 0 then
      begin
        M1 := TMatrix.Identity;
        M1.m31 := -FRotationCenter.X * FSize.Width * FScale.X;
        M1.m32 := -FRotationCenter.Y * FSize.Height * FScale.Y;
        M2 := TMatrix.Identity;
        M2.m31 := FRotationCenter.X * FSize.Width * FScale.X;
        M2.m32 := FRotationCenter.Y * FSize.Height * FScale.Y;
        RotMatrix := M1 * (TMatrix.CreateRotation(DegToRad(FRotationAngle)) * M2);
        FLocalMatrix := FLocalMatrix * RotMatrix;
      end;
      // translate
      TranslateMatrix := TMatrix.Identity;
      TranslateMatrix.m31 := FPosition.X;
      TranslateMatrix.m32 := FPosition.Y;
      FLocalMatrix := FLocalMatrix * TranslateMatrix;
    end
    else
    begin
      FLocalMatrix := TMatrix.Identity;
      FLocalMatrix.m31 := FPosition.X;
      FLocalMatrix.m32 := FPosition.Y;
      FLocalMatrix.m11 := FScale.X;
      FLocalMatrix.m22 := FScale.Y;
    end;
  end
  else
  begin
    FLocalMatrix := TMatrix.Identity;
    FLocalMatrix.m31 := FPosition.X;
    FLocalMatrix.m32 := FPosition.Y;
  end;

  RecalcAbsolute;
  RecalcUpdateRect;
  if HasDisablePaintEffect then
    UpdateEffects;
  if Visible and (ParentContent <> nil) then
    ParentContent.Changed;
  if not FAnchorMove then
  begin
    UpdateExplicitBounds;
    UpdateAnchorRules(True);
  end;
  if (not FInPaintTo) and not IsUpdating then
    Repaint;
end;

procedure TControl.DoMouseEnter;
begin
  FIsMouseOver := True;
  TAnimator.StartTriggerAnimation(Self, Self, 'IsMouseOver');
  ApplyTriggerEffect(Self, 'IsMouseOver');
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TControl.DoMouseLeave;
begin
  FIsMouseOver := False;
  TAnimator.StartTriggerAnimation(Self, Self, 'IsMouseOver');
  ApplyTriggerEffect(Self, 'IsMouseOver');
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

function TControl.GetCanFocus: Boolean;
begin
  Result := FCanFocus and Enabled {and ParentedVisible};
  if Result and Assigned(OnCanFocus) then
    OnCanFocus(Self, Result);
end;

function TControl.GetCanParentFocus: Boolean;
begin
  Result := FCanParentFocus;
end;

function TControl.EnterChildren(AObject: IControl): Boolean;
begin
  Result := False;
end;

function TControl.EnumControls(Proc: TEnumControlsRef; const VisibleOnly: Boolean): Boolean;
var
  I: Integer;

  procedure EnumAllControls(const AParentControl: TControl; var Done: Boolean);
  var
    I: Integer;
  begin
    if VisibleOnly and not AParentControl.Visible then
      Exit;

    Proc(AParentControl, Done);
    if not Done then
      for I := 0 to AParentControl.Controls.Count - 1 do
      begin
         EnumAllControls(AParentControl.Controls[I], Done);
         if Done then
           Break;
      end;
  end;

begin
  Result := False;
  for I := 0 to Controls.Count - 1 do
  begin
    EnumAllControls(Controls[I], Result);
    if Result then
      Break;
  end;
end;

function TControl.ExitChildren(AObject: IControl): Boolean;
begin
  Result := False;
end;

procedure TControl.DoEnter;
begin
  if CanFocus and not FIsFocused and TNonReentrantHelper.EnterSection(FExitingOrEntering) then
  try
    if Application.TrackActivity then
      Application.AnalyticsManager.RecordActivity(TAppActivity.ControlFocused, Self);
    if Assigned(FOnEnter) then
      FOnEnter(Self);
    FIsFocused := True;
    FRecalcUpdateRect := True;
    Repaint;
    if not (DisableFocusEffect or GlobalDisableFocusEffect) then
    begin
      TAnimator.StartTriggerAnimation(Self, Self, 'IsFocused');
      ApplyTriggerEffect(Self, 'IsFocused');
    end;
  finally
    TNonReentrantHelper.LeaveSection(FExitingOrEntering);
  end;
end;

procedure TControl.SetNewScene(AScene: IScene);
var
  I: Integer;
begin
  FScene := AScene;
  if FControls <> nil then
    for I := 0 to FControls.Count - 1 do
      FControls[I].SetNewScene(FScene);
end;

procedure TControl.DoExit;
begin
  if FIsFocused and TNonReentrantHelper.EnterSection(FExitingOrEntering) then
  try
    FIsFocused := False;
    if CanFocus and Assigned(FOnExit) then
    try
      FOnExit(Self);
    except
      FIsFocused := True;
      raise;
    end;
    FRecalcUpdateRect := True;
    Repaint;
    if not (DisableFocusEffect or GlobalDisableFocusEffect) then
    begin
      TAnimator.StartTriggerAnimation(Self, Self, 'IsFocused');
      ApplyTriggerEffect(Self, 'IsFocused');
    end;
  finally
    TNonReentrantHelper.LeaveSection(FExitingOrEntering);
  end;
end;

procedure TControl.DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  // Override DoGesture to implement default behavior
  Handled := False;
end;

procedure TControl.DoInsertObject(Index: Integer; const AObject: TFmxObject);
begin
  inherited DoInsertObject(Index, AObject);
  RefreshInheritedCursorForChildren;
end;

procedure TControl.DoAbsoluteChanged;
begin

end;

procedure TControl.DoActivate;
begin
  DoEnter;
  if Assigned(FOnActivate) then
    FOnActivate(self);
end;

procedure TControl.DoDeactivate;
begin
  DoExit;
  if Assigned(FOnDeactivate) then
    FOnDeactivate(self);
end;

procedure TControl.SetFocus;
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

function TControl.ShowContextMenu(const ScreenPosition: TPointF): Boolean;
begin
  Result := FPopupMenu <> nil;
  if Result then
  begin
    FPopupMenu.PopupComponent := Self;
    FPopupMenu.Popup(Round(ScreenPosition.X), Round(ScreenPosition.Y));
  end;
end;

procedure TControl.HandleSizeChanged;
begin
  UpdateSmallSizeControl;
  if [csLoading, csReading] * ComponentState <> [csLoading, csReading] then
    Resize
  else
    Include(FDelayedEvents, TDelayedEvent.Resize);
  InternalSizeChanged;
  if not FAnchorMove then
  begin
    UpdateExplicitBounds;
    UpdateAnchorRules(True);
  end;
  if RotationAngle <> 0 then
    MatrixChanged(Self);
  if [csLoading, csReading] * ComponentState <> [csLoading, csReading] then
    DoResized
  else
    Include(FDelayedEvents, TDelayedEvent.Resized);
end;

procedure TControl.SizeChanged(Sender: TObject);
var
  SizeTmp: TSizeF;
  PlatformDefault: Boolean;
begin
  if Sender is TControlSize then
  begin
    PlatformDefault := TControlSize(Sender).PlatformDefault;
    if PlatformDefault then
      SizeTmp := GetDefaultSize
    else
      SizeTmp := Size.Size;
    SetSize(SizeTmp.Width, SizeTmp.Height, PlatformDefault);
    HandleSizeChanged;
  end;
end;

procedure TControl.Click;
var
  LAction: TCustomAction;
begin
  try
    if Assigned(FOnClick) and (not EnableExecuteAction or (ActionClient and (TMethod(FOnClick) <> TMethod(Action.OnExecute)))) then
      FOnClick(Self)
    else
      if not (csDesigning in ComponentState) and EnableExecuteAction and (ActionLink <> nil) then
      begin
        if ActionLink.Action is TCustomAction then
          LAction := TCustomAction(ActionLink.Action)
        else
          LAction := nil;
        if LAction <> nil then
        begin
          if not LAction.Supported then
            Exit;
          LAction.Target := Self;
        end;
        try
          if not ActionLink.Execute(Self) then
            ExecuteAction(ActionLink.Action);
        finally
          if LAction <> nil then
            LAction.Target := nil;
        end;
      end
      else
        if Assigned(FOnClick) then
          FOnClick(Self);
  finally
    if ActionClient and (Action is TCustomAction) then
      TCustomAction(Action).ShortCutPressed := False;
  end;
end;

procedure TControl.ClipChildrenChanged;
begin

end;

procedure TControl.CMGesture(var EventInfo: TGestureEventInfo);
var
  Handled: Boolean;
  LGObj: IGestureControl;
begin
  Handled := False;

  if Assigned(FOnGesture) then
    try
      FOnGesture(Self, EventInfo, Handled);
    except
      Application.HandleException(Self);
    end;

  if not Handled then
    try
      DoGesture(EventInfo, Handled);
    except
      Application.HandleException(Self);
    end;

  if not Handled and (FParent <> nil) and (EventInfo.GestureID <> sgiNoGesture) and Supports(Parent, IGestureControl, LGObj) then
    LGObj.CMGesture(EventInfo);                  
end;

procedure TControl.DblClick;
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

function TControl.MakeScreenshot: TBitmap;
var
  SceneScale: Single;
begin
  if Scene <> nil then
    SceneScale := Scene.GetSceneScale
  else
    SceneScale := 1;
  Result := TBitmap.Create(Round(Width * SceneScale), Round(Height * SceneScale));
  Result.BitmapScale := SceneScale;
  Result.Clear(0);
  if Result.Canvas.BeginScene then
  try
    PaintTo(Result.Canvas, TRectF.Create(0, 0, Result.Width / SceneScale, Result.Height / SceneScale));
  finally
    Result.Canvas.EndScene;
  end;
end;

procedure TControl.BeginAutoDrag;
const
  DraggingOpacity = 0.7;
var
  B, S: TBitmap;
  R: TRectF;
begin
  if Root <> nil then
  begin
    S := MakeScreenshot;
    try
      B := nil;
      try
        if (S.Width > 512) or (S.Height > 512) then
        begin
          R := TRectF.Create(0, 0, S.Width, S.Height);
          R.Fit(TRectF.Create(0, 0, 512, 512));
          B := TBitmap.Create(Round(R.Width), Round(R.Height));
          B.Clear(0);
          if B.Canvas.BeginScene then
          try
            B.Canvas.DrawBitmap(S, TRectF.Create(0, 0, S.Width, S.Height), TRectF.Create(0, 0, B.Width, B.Height),
              DraggingOpacity, True);
          finally
            B.Canvas.EndScene;
          end;
        end else
        begin
          B := TBitmap.Create(S.Width, S.Height);
          B.Clear(0);
          if B.Canvas.BeginScene then
          try
            B.Canvas.DrawBitmap(S, TRectF.Create(0, 0, B.Width, B.Height), TRectF.Create(0, 0, B.Width, B.Height),
              DraggingOpacity, True);
          finally
            B.Canvas.EndScene;
          end;
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

procedure TControl.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if AbsoluteEnabled and FPressed and not FDoubleClick and PointInObjectLocal(X, Y) then
  begin
    Click;
    FPressed := False;
    StartTriggerAnimation(Self, 'Pressed');
  end;
end;

procedure TControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if (not (csDesigning in ComponentState)) and (not FIsFocused) then
    SetFocus;

  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
  if FAutoCapture then
    Capture;
  if (ssDouble in Shift) then
  begin
    DblClick;
    FDoubleClick := True;
  end
  else if Button = TMouseButton.mbLeft then
  begin
    FPressed := True;
    FPressedPosition := TPointF.Create(X, Y);
    StartTriggerAnimation(Self, 'Pressed');
  end;
end;

procedure TControl.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  ReleaseCapture;

  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
  if FPressed then
  begin
    FPressed := False;
    StartTriggerAnimation(Self, 'Pressed');
  end;
  FDoubleClick := False;
end;

procedure TControl.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, Shift, WheelDelta, Handled)
end;

procedure TControl.DragEnter(const Data: TDragObject; const Point: TPointF);
var
  Operation: TDragOperation;
begin
  if Assigned(OnDragEnter) then
    OnDragEnter(Self, Data, Point);

  Operation := TDragOperation.None;
  DragOver(Data, Point, Operation);
  if Operation <> TDragOperation.None then
  begin
    FIsDragOver := True;
    Repaint;
    TAnimator.StartTriggerAnimation(Self, Self, 'IsDragOver');
    ApplyTriggerEffect(Self, 'IsDragOver');
  end;
end;

procedure TControl.DragLeave;
begin
  FIsDragOver := False;
  Repaint;
  TAnimator.StartTriggerAnimation(Self, Self, 'IsDragOver');
  ApplyTriggerEffect(Self, 'IsDragOver');
  if Assigned(OnDragLeave) then
    OnDragLeave(Self);
end;

procedure TControl.DragOver(const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  if Assigned(OnDragOver) then
    OnDragOver(Self, Data, Point, Operation);
end;

procedure TControl.DragDrop(const Data: TDragObject; const Point: TPointF);
begin
  FIsDragOver := False;
  Repaint;
  TAnimator.StartTriggerAnimation(Self, Self, 'IsDragOver');
  ApplyTriggerEffect(Self, 'IsDragOver');
  if Assigned(OnDragDrop) then
    OnDragDrop(Self, Data, Point);
end;

procedure TControl.DragEnd;
begin
  // Call mouse up - for effects - inside control
  if DragMode = TDragMode.dmAutomatic then
    MouseUp(TMouseButton.mbLeft, [ssLeft], $FFFF, $FFFF);
  if Assigned(OnDragEnd) then
    OnDragEnd(Self);
end;

{ controls }

procedure TControl.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  try
    FEnabled := Value;
    EnabledChanged;
  finally
    if not Value and (Root <> nil) then
    begin
      if FIsFocused then
        Root.SetFocused(nil)
      else
        ResetFocus;
    end;
    RecalcEnabled;
    RecalcOpacity;
    Repaint;
  end;
end;

procedure TControl.EnabledChanged;
begin

end;

function TControl.EnabledStored: Boolean;
begin
  if ActionClient then
    Result := True
  else
    Result := not Enabled;
end;

function TControl.GetInheritedCursor: TCursor;
begin
  Result := FInheritedCursor;
end;

function TControl.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(50, 50);
end;

function TControl.GetDefaultTouchTargetExpansion: TRectF;
begin
  Result := NullRect;
end;

function TControl.GetDesignInteractive: Boolean;
begin
  Result := FDesignInteractive;
end;

function TControl.GetAbsoluteEnabled: Boolean;
begin
  if FRecalcEnabled then
  begin
    if (FParentControl <> nil) and (not FParentControl.AbsoluteEnabled) then
      FAbsoluteEnabled := False
    else
      FAbsoluteEnabled := FEnabled;

    Result := FAbsoluteEnabled;
    FRecalcEnabled := False;

    if not Result and (Root <> nil) and (FScene <> nil) and IsFocused then
      Root.SetFocused(nil);
  end
  else
  begin
    Result := FAbsoluteEnabled;
  end;
end;

procedure TControl.RecalcEnabled;
var
  I: Integer;
begin
  if FRecalcEnabled then Exit;
  FRecalcEnabled := True;
  if FControls <> nil then
    for I := 0 to FControls.Count - 1 do
      FControls[I].RecalcEnabled;
end;

procedure TControl.SetTempCanvas(const Value: TCanvas);
var
  I: Integer;
begin
  FTempCanvas := Value;
  if FControls <> nil then
    for I := 0 to FControls.Count - 1 do
      FControls[I].TempCanvas := Value;
end;

procedure TControl.SetTop(const Value: Single);
begin
  FTop := Value;
  if csReading in ComponentState then
    FExplicitTop := FTop;
end;

procedure TControl.SetTouchManager(const Value: TTouchManager);
begin
  FTouchManager := Value;
end;

procedure TControl.SetTouchTargetExpansion(const Value: TBounds);
begin
  FTouchTargetExpansion.Assign(Value);
end;

procedure TControl.SetHint(const AHint: string);
begin
  FHint := AHint;
  FActionHint := string.Empty;
end;

procedure TControl.SetHitTest(const Value: Boolean);
begin
  FHitTest := Value;
  HitTestChanged;
end;

procedure TControl.SetAcceptsControls(const Value: boolean);
begin
  FAcceptsControls := Value;
end;

procedure TControl.SetClipChildren(const Value: Boolean);
begin
  if FClipChildren <> Value then
  begin
    FClipChildren := Value;
    ClipChildrenChanged;
    Repaint;
  end;
end;

procedure TControl.SetAlign(const Value: TAlignLayout);
var
  AlignRoot: IAlignRoot;
  OldAlign: TAlignLayout;
begin
  if FAlign <> Value then
  begin
    OldAlign:= FAlign;
    FAlign := Value;
    Anchors:= AnchorAlign[Value];
    if not(csLoading in ComponentState) and (not (csDesigning in ComponentState) or (Parent <> nil)) then
      if ((OldAlign in [TAlignLayout.Top, TAlignLayout.Bottom, TAlignLayout.MostTop, TAlignLayout.MostBottom]) =
        (Value in [TAlignLayout.Right, TAlignLayout.Left, TAlignLayout.MostRight, TAlignLayout.MostLeft]))
        and not (OldAlign in [TAlignLayout.None, TAlignLayout.Client, TAlignLayout.Contents])
        and not (Value in [TAlignLayout.None, TAlignLayout.Client, TAlignLayout.Contents])  then
        SetBounds(Left, Top, Width, Height)
      else if (OldAlign <> TAlignLayout.None) and (Value = TAlignLayout.None) then
        SetBounds(FExplicitLeft, FExplicitTop, FExplicitWidth, FExplicitHeight);
    if Visible and (ParentContent <> nil) then
      ParentContent.Changed
    else if (Align <> TAlignLayout.None) and Supports(Parent, IAlignRoot, AlignRoot) then
    begin
      AlignRoot.ChildrenAlignChanged;
      if not (csLoading in ComponentState) then
        AlignRoot.Realign;
    end;
  end;
end;

procedure TControl.SetAnchors(const Value: TAnchors);
var
  OldAnchors: TAnchors;
  AlignRoot: IAlignRoot;
begin
  if FAnchors <> Value then
  begin
    OldAnchors:= FAnchors;
    FAnchors := Value;
    if not (csLoading in ComponentState) then
      if (OldAnchors <> [TAnchorKind.akLeft, TAnchorKind.akTop]) and (FAnchors = [TAnchorKind.akLeft, TAnchorKind.akTop]) and
        ((FExplicitLeft <> Left) or (FExplicitTop <> Top) or (FExplicitWidth <> Width) or (FExplicitHeight <> Height)) then
        SetBounds(FExplicitLeft, FExplicitTop, FExplicitWidth, FExplicitHeight)
      else
        UpdateAnchorRules(True);
    if (Anchors <> [TAnchorKind.akLeft, TAnchorKind.akTop]) and Supports(Parent, IAlignRoot, AlignRoot) then
    begin
      AlignRoot.ChildrenAlignChanged;
      if not (csLoading in ComponentState) then
        AlignRoot.Realign;
    end;
  end;
end;

procedure TControl.EnumControls(const Proc: TFunc<TControl, TEnumControlsResult>);
  procedure EnumChildControls(const AParentControl: TControl; var ProcResult: TEnumControlsResult);
  var
    I: Integer;
    SaveDisableDisappear: Boolean;
    Control: TControl;
  begin
    for I := 0 to AParentControl.Controls.Count - 1 do
    begin
      Control := AParentControl.Controls[I];
      SaveDisableDisappear := Control.DisableDisappear;
      try
        Control.DisableDisappear := True;
        ProcResult := Proc(Control);
        if ProcResult = TEnumProcResult.Continue then
          EnumChildControls(Control, ProcResult);
      finally
        Control.DisableDisappear := SaveDisableDisappear;
      end;
      if ProcResult = TEnumProcResult.Stop then
        Break;
      if ProcResult = TEnumProcResult.Discard then
        ProcResult := TEnumProcResult.Continue;
    end;
  end;

var
  ProcResult: TEnumProcResult;
begin
  ProcResult := TEnumProcResult.Continue;
  EnumChildControls(Self, ProcResult);
end;

procedure TControl.SetVisible(const Value: Boolean);
var
  AlignRoot: IAlignRoot;
begin
  if FVisible <> Value then
  try
    if FVisible then
      Repaint;
    FVisible := Value;
    VisibleChanged;
  finally
    if FVisible then
      Show
    else
      Hide;
    // We notify all child controls, that parent changed visibility
    AncestorVisibleChanged(FVisible);
    if not (csLoading in ComponentState) and (Align <> TAlignLayout.None) then
    begin
      if FParentControl <> nil then
        FParentControl.Realign
      else
        if not(csLoading in ComponentState) and Supports(Parent, IAlignRoot, AlignRoot) then
          AlignRoot.Realign;
    end;
    if ParentContent <> nil then
      ParentContent.Changed;
    if FVisible then
    begin
      RecalcUpdateRect;
      Repaint;
      TAnimator.StartTriggerAnimation(Self, Self, 'IsVisible');
    end
    else
      ResetFocus;
  end;
end;

procedure TControl.VisibleChanged;
begin

end;

function TControl.VisibleStored: Boolean;
begin
  Result := True;
end;

procedure TControl.SetPopupMenu(const Value: TCustomPopupMenu);
begin
  if FPopupMenu <> Value then
  begin
    if FPopupMenu <> nil then
      FPopupMenu.RemoveFreeNotification(Self);
    FPopupMenu := Value;
    if FPopupMenu <> nil then
      FPopupMenu.FreeNotification(Self);
  end;
end;

procedure TControl.SetPosition(const Value: TPosition);
begin
  FPosition.Assign(Value);
end;

function TControl.GetRotationAngle: Single;
begin
  Result := FRotationAngle;
end;

procedure TControl.SetRotationAngle(const Value: Single);
begin
  if not SameValue(FRotationAngle, Value, TEpsilon.Scale) then
  begin
    FRotationAngle := Value;
    MatrixChanged(Self);
  end;
end;

function TControl.GetRotationCenter: TPosition;
begin
  Result := FRotationCenter;
end;

procedure TControl.SetRotationCenter(const Value: TPosition);
begin
  FRotationCenter.Assign(Value);
end;

function TControl.GetScale: TPosition;
begin
  Result := FScale;
end;

procedure TControl.SetScale(const Value: TPosition);
begin
  FScale.Assign(Value);
end;

procedure TControl.SetShowHint(const Value: Boolean);
begin
  if Value <> FShowHint then
  begin
    FShowHint := Value;
    if FShowHint then
      ParentShowHint := False;
  end;
end;

procedure TControl.SetSize(const AWidth, AHeight: Single; const APlatformDefault: Boolean);
begin
  if DoSetSize(FSize, APlatformDefault, AWidth, AHeight, FLastWidth, FLastHeight) then
    HandleSizeChanged;
end;

procedure TControl.SetSize(const AValue: TControlSize);
begin
  FSize.Assign(AValue);
end;

procedure TControl.Hide;
begin
end;

function TControl.HintStored: Boolean;
begin
  Result := (not Hint.IsEmpty and not ActionClient) or not (ActionClient and (ActionLink <> nil) and
    (ActionLink.HintLinked) and (Action is TContainedAction));
end;

procedure TControl.HitTestChanged;
begin
end;

procedure TControl.Show;
begin
end;

function TControl.SupportsPaintStage(const Stage: TPaintStage): Boolean;
begin
  Result := Stage <> TPaintStage.Text;
end;

procedure TControl.Tap(const Point:TPointF);
begin
  if Assigned(FOnTap) then
    FOnTap(Self, Point);
end;

function TControl.TouchManager: TTouchManager;
begin
  Result := GetTouchManager;
end;

function TControl.DoSetWidth(var Value: Single; NewValue: Single; var LastValue: Single): Boolean;
begin
  Result := DoSetSize(FSize, False, NewValue, FSize.Height, LastValue, FLastHeight);
end;

function TControl.DoSetHeight(var Value: Single; NewValue: Single; var LastValue: Single): Boolean;
begin
  Result := DoSetSize(FSize, False, FSize.Width, NewValue, FLastWidth, LastValue);
end;

function TControl.DoSetSize(const ASize: TControlSize; const NewPlatformDefault: Boolean;
  ANewWidth, ANewHeight: Single; var ALastWidth, ALastHeight: Single): Boolean;
var
  NewSize: TSizeF;
begin
  NewSize.Width := System.Math.Max(0, ANewWidth);
  NewSize.Height := System.Math.Max(0, ANewHeight);
  Result := not SameValue(NewSize.Width, ASize.Width, TEpsilon.Position) or
    not SameValue(NewSize.Height, ASize.Height, TEpsilon.Position);
  if Result then
    Repaint;
  ALastWidth := ASize.Width;
  ALastHeight := ASize.Height;
  ASize.SetSizeWithoutNotification(NewSize);
  ASize.SetPlatformDefaultWithoutNotification(NewPlatformDefault);
end;

procedure TControl.RequestAlign;
var
  AlignRoot: IAlignRoot;
begin
  if not (csLoading in ComponentState) and ((Align <> TAlignLayout.None) or (Anchors <> AnchorAlign[Align])) then
  begin
    if FParentControl <> nil then
      FParentControl.Realign
    else
      if not(csLoading in ComponentState) and Supports(Parent, IAlignRoot, AlignRoot) then
        AlignRoot.Realign;
  end;
end;

procedure TControl.SetBounds(X, Y, AWidth, AHeight: Single);
var
  SizeChanged: Boolean;
  Moved: Boolean;
  NeedRepaint: Boolean;
  ReductionSize: Boolean;
begin
  Moved := not (SameValue(X, Position.X, TEpsilon.Position) and SameValue(Y, Position.Y, TEpsilon.Position));
  SizeChanged := not (SameValue(AWidth, Width, TEpsilon.Position) and SameValue(AHeight, Height, TEpsilon.Position));
  if Moved or SizeChanged then
  begin
    ReductionSize := False;
    if SizeChanged then
    begin
      ReductionSize := (FSize.Height > AHeight) or (FSize.Width > AWidth);
      SizeChanged := DoSetSize(FSize, False, AWidth, AHeight, FLastWidth, FLastHeight);
    end;
    NeedRepaint := False;

    if Moved or (SizeChanged and (RotationAngle <> 0)) then
    begin
      if Moved or ReductionSize then
        Repaint;
      FPosition.SetPointNoChange(PointF(X, Y));
      FLeft := FPosition.X;
      FTop := FPosition.Y;
      Inc(FUpdating);
      try
        MatrixChanged(Self);
      finally
        Dec(FUpdating);
      end;
      NeedRepaint := True;
    end;

    if Moved or SizeChanged then
    begin
      if (csDesigning in ComponentState) then
        if (FParentControl <> nil) and not (csDestroying in FParentControl.ComponentState) then
          if (TRectF.Union(BoundsRect, FParentControl.LocalRect) <> FParentControl.LocalRect) then
            FParentControl.RecalcUpdateRect;
      UpdateExplicitBounds;
      UpdateAnchorRules;
      RequestAlign;
    end;

    if not (csLoading in ComponentState) and SizeChanged then
    begin
      UpdateSmallSizeControl;
      Resize;
      if FControls <> nil then
        Realign
      else if HasEffect then
        UpdateEffects;
    end;
    if not (csLoading in ComponentState) and not SizeChanged and Moved then
      Move;
    if not (csLoading in ComponentState) and (Moved or SizeChanged) then
    begin
      RecalcUpdateRect;
      NeedRepaint := True;
    end;
    if not (csLoading in ComponentState) and SizeChanged then
      DoResized;
    if NeedRepaint then
      Repaint;
  end;
end;

procedure TControl.InternalSizeChanged;
var
  AlignRoot: IAlignRoot;
begin
  if not(csLoading in ComponentState) then
  begin
    UpdateEffects;
    RecalcUpdateRect;
    if ParentContent <> nil then
      ParentContent.Changed;
    if (Align <> TAlignLayout.None) or (ParentContent <> nil) then
    begin
      if FParentControl <> nil then
        FParentControl.Realign
      else
        if Supports(Parent, IAlignRoot, AlignRoot) then
          AlignRoot.Realign;
    end;
    if Children <> nil then
      Realign
    else
      Repaint;
  end;
end;


procedure TControl.SetHeight(const Value: Single);
begin
  SetSize(FSize.Width, Value, False);
end;

procedure TControl.SetWidth(const Value: Single);
begin
  SetSize(Value, FSize.Height, False);
end;

function TControl.IsOpacityStored: Boolean;
begin
  Result := FOpacity <> 1;
end;

function TControl.IsPopupMenuStored: boolean;
begin
  Result := (not (ActionClient and
                  (ActionLink is TControlActionLink) and
                  (TControlActionLink(ActionLink).IsPopupMenuLinked)));
end;

function TControl.IsUpdating: Boolean;
begin
  Result := FUpdating > 0;
end;

function TControl.IsPositionStored: Boolean;
begin
  Result := not (FAlign in [TAlignLayout.Client, TAlignLayout.Contents, TAlignLayout.Center, TAlignLayout.Fit]);
end;

function TControl.IsHeightStored: Boolean;
begin
  Result := False;
end;

function TControl.IsWidthStored: Boolean;
begin
  Result := False;
end;

function TControl.IsShowHintStored: Boolean;
begin
  Result := not ParentShowHint;
end;

function TControl.IsSizeStored: Boolean;
begin
  Result := True;
end;

procedure TControl.SetOnClick(const Value: TNotifyEvent);
begin
  FOnClick := Value;
end;

procedure TControl.SetOpacity(const Value: Single);
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

procedure TControl.UpdateAnchorRules(const Anchoring: Boolean);
begin
  if not FAnchorMove and ([csLoading, csDestroying] * ComponentState = []) then
  begin
    RecalcAnchorRules(Parent, FAnchors, BoundsRect, FOriginalParentSize, FAnchorOrigin, FAnchorRules);
    RecalcControlOriginalParentSize(Parent, ComponentState, Anchoring, FOriginalParentSize);
  end;
end;

function TControl.DisabledOpacityStored: Boolean;
begin
  Result := not SameValue(FDisabledOpacity, DefaultDisabledOpacity, TEpsilon.Scale)
end;

procedure TControl.SetDisabledOpacity(const Value: Single);
begin
  if not SameValue(FDisabledOpacity, Value, TEpsilon.Scale) then
  begin
    FDisabledOpacity := Value;
    if not Enabled then
    begin
      RecalcOpacity;
      Repaint;
    end;
  end;
end;

procedure TControl.SetDragMode(const ADragMode: TDragMode);
begin
  FDragMode := ADragMode;
end;

procedure TControl.SetCursor(const Value: TCursor);
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

function TControl.GetTouchManager: TTouchManager;
begin
  if FTouchManager = nil then
    FTouchManager := TTouchManager.Create(Self);

  Result := FTouchManager;
end;

function TControl.GetTop: Single;
begin
  Result := Position.Y;
end;

procedure TControl.SetTabOrder(const Value: TTabOrder);
var
  Controller: ITabStopController;
begin
  Controller := GetTabStopController;
  if Controller <> nil then
    Controller.TabList.Update(Self, Value);
end;

procedure TControl.SetTabStop(const TabStop: Boolean);
begin
  FTabStop := TabStop;
end;

function TControl.GetTabStop: Boolean;
begin
  Result := FTabStop;
end;

function TControl.GetTabListClass: TTabListClass;
begin
  Result := TTabList;
end;

function TControl.GetTabList: ITabList;
begin
  if FTabList = nil then
    FTabList := GetTabListClass.Create(Self);
  Result := FTabList;
end;

function TControl.GetTabOrder: TTabOrder;
var
  Controller: ITabStopController;
begin
  Controller := GetTabStopController;
  if Controller <> nil then
    Result := Controller.TabList.GetTabOrder(Self)
  else
    Result := -1;
end;

function TControl.GetTabStopController: ITabStopController;
var
  ParentObject: TFmxObject;
begin
  ParentObject := Parent;
  while (ParentObject <> nil) and (not Supports(ParentObject, ITabStopController, Result)) do
    ParentObject := ParentObject.Parent;
end;

procedure TControl.AddToEffectsList(const AEffect: TEffect);
begin
  if FEffects = nil then
    FEffects := TList<TEffect>.Create;
  FEffects.Add(AEffect);
end;

procedure TControl.RemoveFromEffectsList(const AEffect: TEffect);
begin
  if FEffects <> nil then
    FEffects.Remove(AEffect);
end;

procedure TControl.SetLeft(const Value: Single);
begin
  FLeft := Value;
  if csReading in ComponentState then
    FExplicitLeft := FLeft;
end;

procedure TControl.SetLocked(const Value: Boolean);
begin
  FLocked := Value;
end;

procedure TControl.SetMinClipHeight(const Value: single);
begin
  if not SameValue(FMinClipHeight, Value, TEpsilon.Position) then
  begin
    FMinClipHeight := Value;
    UpdateSmallSizeControl;
  end;
end;

procedure TControl.SetMinClipWidth(const Value: single);
begin
  if not SameValue(FMinClipWidth, Value, TEpsilon.Position) then
  begin
    FMinClipWidth := Value;
    UpdateSmallSizeControl;
  end;
end;

function TControl.UpdateSmallSizeControl: Boolean;
var
  NeedClip: Boolean;
  R: TRectF;
begin
  R := LocalRect;
  R := Padding.PaddingRect(R);
  NeedClip := (FMinClipHeight > R.Height) or
              (FMinClipWidth > R.Width);
  Result := NeedClip <> FSmallSizeControl;
  if Result then
    FSmallSizeControl := NeedClip;
end;

function TControl.GetEffectsRect: TRectF;
var
  Effect: TEffect;
begin
  Result := LocalRect;
  if (FEffects <> nil) and (FEffects.Count > 0) then
    for Effect in FEffects do
      if Effect.Enabled then
        Result := UnionRect(Result, Effect.GetRect(LocalRect));
end;

function TControl.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TControl.GetHeight: Single;
begin
  if FSize.PlatformDefault then
    Result := FSize.DefaultValue.Height
  else
    Result := FSize.Height;
end;

function TControl.GetHintString: string;
begin
  if FActionHint.Length > 0 then
    Result := FActionHint
  else
    Result := FHint;
end;

function TControl.GetHintObject: TObject;
begin
  Result := nil;
end;

function TControl.HasHint: Boolean;
begin
  if FParentShowHint then
  begin
    if ParentControl <> nil then
      Result := ParentControl.FShowHint
    else
      if Parent is TCommonCustomForm then
        Result := TCommonCustomForm(Parent).ShowHint
      else
        Result := False;
  end
  else
    Result := FShowHint;

  if Result then
  begin
    if (ActionLink <> nil) and (ActionLink is TControlActionLink) then
    begin
      FActionHint := FHint;
      if TControlActionLink(ActionLink).DoShowHint(FActionHint) then
        Result := FActionHint.Length > 0
      else
      begin
        FActionHint := string.Empty;
        Result := FHint.Length > 0;
      end;
    end
    else
      Result := FHint.Length > 0;
  end;
end;

function TControl.GetHitTest: Boolean;
begin
  Result := FHitTest;
end;

function TControl.GetAcceptsControls: Boolean;
begin
  Result := FAcceptsControls;
end;

function TControl.GetActionLinkClass: TActionLinkClass;
begin
  Result := TControlActionLink;
end;

function TControl.GetAdjustSizeValue: TSizeF;
begin
  Result := TSizeF.Create(0, 0);
end;

function TControl.GetAdjustType: TAdjustType;
begin
  Result := TAdjustType.None;
end;

function TControl.GetAbsoluteHasAfterPaintEffect: Boolean;
begin
  if FRecalcHasEffect then
    HasEffect; // Force recalc
  Result := FAbsoluteHasAfterPaintEffect;
end;

function TControl.GetAbsoluteHasDisablePaintEffect: Boolean;
begin
  if FRecalcHasEffect then
    HasEffect; // Force recalc
  Result := FAbsoluteHasDisablePaintEffect;
end;

function TControl.GetAbsoluteHasEffect: Boolean;
var
  Effect: TEffect;
begin
  if FRecalcHasEffect then
  begin
    FAbsoluteHasEffect := False;
    FAbsoluteHasDisablePaintEffect := False;
    FAbsoluteHasAfterPaintEffect := False;
    if FDisableEffect or (FEffects = nil) or (FEffects.Count = 0) then
    begin
      Result := FAbsoluteHasEffect;
      Exit;
    end;

    for Effect in FEffects do
    begin
      if Effect.Enabled then
      begin
        FAbsoluteHasEffect := True;
        if TEffectStyle.DisablePaint in Effect.EffectStyle then
          FAbsoluteHasDisablePaintEffect := True;
        if TEffectStyle.AfterPaint in Effect.EffectStyle then
          FAbsoluteHasAfterPaintEffect := True;
        Break;
      end;
    end;
    FRecalcHasEffect := False;
  end;
  Result := FAbsoluteHasEffect;
end;

procedure TControl.RecalcHasClipParent;
begin
  FRecalcHasClipParent := True;
end;

function TControl.GetHasClipParent: TControl;
var
  I: Integer;
begin
  if FRecalcHasClipParent then
  begin
    FHasClipParent := nil;
    if FControls <> nil then
      for I := 0 to FControls.Count - 1 do
        if FControls[I].ClipParent then
        begin
          FHasClipParent := FControls[I];
          Break
        end;
    Result := FHasClipParent;
  end
  else
    Result := FHasClipParent;
end;

procedure TControl.RecalcHasEffect;
begin
  if FRecalcHasEffect then Exit;
  FRecalcHasEffect := True;
  if FParentControl <> nil then
    FParentControl.RecalcHasEffect;
end;

procedure TControl.StartTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string);
begin
  TAnimator.DefaultStartTriggerAnimation(Self, AInstance, ATrigger);
end;

procedure TControl.StartTriggerAnimationWait(const AInstance: TFmxObject; const ATrigger: string);
begin
  TAnimator.DefaultStartTriggerAnimationWait(Self, AInstance, ATrigger);
end;

function TControl.GetDisableDisappear: Boolean;
begin
  Result := FDisableDisappear > 0;
end;

procedure TControl.SetDisableDisappear(const Value: Boolean);
begin
  if Value then
    Inc(FDisableDisappear)
  else if FDisableDisappear > 0 then
    Dec(FDisableDisappear);
end;

{ TStyledControl }

constructor TStyledControl.Create(AOwner: TComponent);
begin
  inherited;
  FInflated := False;
  FHelpType := htContext;
  FHelpContext := 0;
  FIsNeedStyleLookup := True;
  FScaleChangedId := TMessageManager.DefaultManager.SubscribeToMessage(TScaleChangedMessage, ScaleChangedHandler);
  FStyleChangedId := TMessageManager.DefaultManager.SubscribeToMessage(TStyleChangedMessage, StyleChangedHandler);
end;

destructor TStyledControl.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TStyleChangedMessage, FStyleChangedId);
  TMessageManager.DefaultManager.Unsubscribe(TScaleChangedMessage, FScaleChangedId);
  FreeAndNil(FStylesData);
  inherited;
end;

procedure TStyledControl.BeforeDestruction;
begin
  inherited;
  if FResourceLink <> nil then
    InternalFreeStyle;
end;

procedure TStyledControl.Disappear;
begin
  inherited Disappear;
  if DisableDisappear then
    Exit;
  NeedStyleLookup;
end;

procedure TStyledControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (csDesigning in ComponentState) and not (csDestroying in ComponentState) and (Operation = opRemove) and
    (AComponent is TStyleBook) and not StyleLookup.IsEmpty then
    DoStyleChanged;
  if (Operation = opRemove) and (AComponent = Action) then
    Action := nil;
end;

procedure TStyledControl.SetNewScene(AScene: IScene);
var
  OldScene: IScene;
begin
  OldScene := FScene;
  inherited SetNewScene(AScene);
  if not (csDestroying in ComponentState) and (OldScene <> AScene) and (not IsUpdating)  then
    KillResourceLink;
end;

procedure TStyledControl.Painting;
begin
  inherited;
  ApplyStyleLookup;
end;

procedure TStyledControl.PrepareForPaint;
begin
  inherited;
  ApplyStyleLookup;
end;

procedure TStyledControl.RecalcSize;
begin
  ApplyStyleLookup;
  inherited;
end;

function TStyledControl.RequestStyleData(const Index: string): TValue;
var
  Obj: TObject;
  InstanceName, PropertyName: string;
  PropertyValue: TValue;
begin
  if Index.Contains('.') then
  begin
    PropertyName := Index;
    InstanceName := GetToken(PropertyName, '.');
  end
  else
    InstanceName := Index;

  Obj := FindStyleResource(InstanceName);
  if Obj <> nil then
  begin
    if not PropertyName.IsEmpty then
    begin
      if FindProperty(Obj, PropertyName,
        procedure (Instance: TObject; Prop: TRttiProperty)
        begin
          PropertyValue := Prop.GetValue(Instance);
        end) then
          Result := PropertyValue;
    end
    else
      Result := TFmxObject(Obj).Data
  end
  else
    Result := TValue.Empty;
end;

procedure TStyledControl.NeedStyleLookup;
begin
  FIsNeedStyleLookup := True;
  if FResourceLink <> nil then
  begin
    FResourceLink.Parent := nil;
    if (Scene <> nil) and (Scene.StyleBook <> nil) then
      KillResourceLink
    else
      RecycleResourceLink;
  end;
end;

function TStyledControl.SearchInto: Boolean;
begin
  Result := False;
end;

function TStyledControl.FindStyleResource(const AStyleLookup: string; const Clone: Boolean = False): TFmxObject;
begin
  if FResourceLink <> nil then
    Result := FResourceLink.FindStyleResource(AStyleLookup, Clone)
  else
    Result := nil;
end;

function TStyledControl.FindStyleResource<T>(const AStyleLookup: string; var AResource: T): Boolean;
var
  StyleObject: TFmxObject;
begin
  StyleObject := FindStyleResource(AStyleLookup, False);
  Result := StyleObject is T;
  if Result then
    AResource := T(StyleObject);
end;

function TStyledControl.FindAndCloneStyleResource<T>(const AStyleLookup: string; var AResource: T): Boolean;
var
  StyleObject: TFmxObject;
begin
  StyleObject := nil;
  if FindStyleResource(AStyleLookup, StyleObject) then
    AResource := T(FindStyleResource(AStyleLookup, True));
  Result := StyleObject <> nil;
end;

function TStyledControl.GetStyleObject: TFmxObject;
begin
  Result := GetStyleObject(True);
end;

class function TStyledControl.LookupStyleObject(const Instance: TFmxObject; const Context: TFmxObject; const Scene: IScene;
  const StyleLookup, DefaultStyleLookup, ParentClassStyleLookup: string; const Clone: Boolean;
  const UseGlobalPool: Boolean = True): TFmxObject;

  function LookupStyleInObject(ASource: TFmxObject; const AStyleName: string): TFmxObject;
  var
    SavedLoadableStyle: TFmxObject;
  begin
    Result := nil;
    if ASource <> nil then
    begin
      SavedLoadableStyle := FLoadableStyle;
      FLoadableStyle := ASource;
      try
        Result := ASource.FindStyleResource(AStyleName, Clone);
        if Result <> nil then
          Result.TagString := AStyleName.ToLowerInvariant;
      finally
        FLoadableStyle := SavedLoadableStyle;
      end;
    end;
  end;

  function LookupStyle(ASource: TFmxObject): TFmxObject;
  begin
    Result := nil;
    if not StyleLookup.IsEmpty then
      Result := LookupStyleInObject(ASource, StyleLookup);
    if (Result = nil) and not DefaultStyleLookup.IsEmpty then
      Result := LookupStyleInObject(ASource, DefaultStyleLookup);
    if (Result = nil) and not ParentClassStyleLookup.IsEmpty then
      Result := LookupStyleInObject(ASource, ParentClassStyleLookup);
  end;

var
  StyleBookStyle: TFmxObject;
  ActiveStyleForScene: TFmxObject;
begin
  Result := nil;

  StyleBookStyle := nil;
  if Scene <> nil then
  begin
    ActiveStyleForScene := TStyleManager.ActiveStyleForScene(Scene);
    if Scene.StyleBook <> nil then
      if csDesigning in Instance.ComponentState then
        StyleBookStyle := Scene.StyleBook.GetStyle(Context)
      else
        StyleBookStyle := Scene.StyleBook.Style;
  end
  else
    ActiveStyleForScene := nil;

  if not StyleLookup.IsEmpty then
  begin
    if StyleBookStyle <> nil then
      Result := LookupStyleInObject(StyleBookStyle, StyleLookup);

    if (Result = nil) and (ActiveStyleForScene <> nil) then
      Result := LookupStyleInObject(ActiveStyleForScene, StyleLookup);

    if (Result = nil) and UseGlobalPool then
    begin
      Result := FMX.Types.FindStyleResource(StyleLookup, Clone);
      if Result <> nil then
        Result.TagString := StyleLookup;
    end;
  end;

  if Result = nil then
  begin
    if StyleBookStyle <> nil then
      Result := LookupStyle(StyleBookStyle);
    if (Result = nil) and (ActiveStyleForScene <> nil) then
      Result := LookupStyle(ActiveStyleForScene);
  end;
end;

function TStyledControl.GetStyleObject(const Clone: Boolean): TFmxObject;
begin
  if not FStyleLookup.IsEmpty and SameText(FStyleLookup, FStyleName) then
    Exit(nil)
  else
    Result := LookupStyleObject(Self, GetStyleContext, Scene, FStyleLookup, GetDefaultStyleLookupName,
      GetParentClassStyleLookupName, Clone);
end;

procedure TStyledControl.DoApplyStyleLookup;
begin
  if Assigned(FOnApplyStyleLookup) then
    FOnApplyStyleLookup(Self);
end;

procedure TStyledControl.DoDeleteChildren;
begin
  if FResourceLink <> nil then
    InternalFreeStyle;
  inherited;
end;

procedure TStyledControl.ApplyStyleLookup;
  procedure InternalApplyStyle(const StyleObject: TFmxObject);
  var
    StyleControl: TControl;
  begin
    FStyleState := TStyleState.Applying;
    try
      if StyleObject is TControl then
      begin
        StyleControl := TControl(StyleObject);
        FAdjustSizeValue := TPointF.Create(StyleControl.Width, StyleControl.Height);
        StyleControl.Visible := True;
        StyleControl.FAlign := TAlignLayout.None;
        StyleControl.FAnchors := AnchorAlign[TAlignLayout.None];
        try
          StyleControl.BoundsRect := StyleControl.Margins.PaddingRect(LocalRect);
        finally
          StyleControl.FAlign := TAlignLayout.Contents;
          StyleControl.FAnchors := AnchorAlign[TAlignLayout.Contents];
        end;
        FResourceLink := StyleObject;
        StyleControl.SetAcceptsControls(False);
        InsertObject(0, StyleControl);
        { }
        StyleControl.Stored := False;
        StyleControl.Lock;
        ApplyStyle;
        FUpdateEffects := True;
        DoApplyStyleLookup;
      end
      else
      begin
        FResourceLink := StyleObject;
        InsertObject(0, StyleObject);
        { }
        StyleObject.Stored := False;
        ApplyStyle;
        DoApplyStyleLookup;
      end;
      FStyleState := TStyleState.Applied;
    except
      FStyleState := TStyleState.Error;
      raise;
    end;
  end;
var
  StyleObject: TFmxObject;
  SaveDisableAlign: Boolean;
begin
  if FIsNeedStyleLookup and (Scene <> nil) and not InPaintTo then
  begin
    FIsNeedStyleLookup := False;

    if not (csDesigning in ComponentState) and (Scene.StyleBook = nil) then
    begin
      if not FStyleLookup.IsEmpty then
        StyleObject := TStyleCache.Current.FindResource(FStyleLookup)
      else
        StyleObject := TStyleCache.Current.FindResource(GetDefaultStyleLookupName);
    end
    else
      StyleObject := nil;
    if StyleObject = nil then
      StyleObject := GetStyleObject;
    if StyleObject <> nil then
    begin
      SaveDisableAlign := FDisableAlign;
      try
        FDisableAlign := True;
        InternalApplyStyle(StyleObject);
      finally
        FDisableAlign := SaveDisableAlign;
        Realign;
      end;
    end;
  end;
end;

procedure TStyledControl.RecycleResourceLink;
var
  SaveAlign: Boolean;
  Link: TFmxObject;
begin
  Link := FResourceLink;
  if Link <> nil then
  begin
    SaveAlign := FDisableAlign;
    FDisableAlign := True;
    try
      InternalFreeStyle;
      if (Link is TControl) and TStyleCache.Initialized then
        TStyleCache.Current.FreeResource(TControl(Link));
    finally
      FDisableAlign := SaveAlign;
    end;
  end
  else
    InternalFreeStyle;
end;

procedure TStyledControl.KillResourceLink;
var
  SaveCacheEnabled: Boolean;
  SaveAlign: Boolean;
  Link: TFmxObject;
begin
  Link := FResourceLink;
  if Link <> nil then
  begin
    SaveCacheEnabled := TStyleCache.Current.Enabled;
    SaveAlign := FDisableAlign;
    TStyleCache.Current.Enabled := False;
    FDisableAlign := True;
    try
      InternalFreeStyle;
      Link.Parent := nil;
      {$IFDEF ANDROID} // RSP-17938
      TThread.CreateAnonymousThread(
        procedure
        begin
          TThread.CurrentThread.Queue(nil,
            procedure
            begin
              Link.DisposeOf;
            end);
        end).Start;
      {$ELSE}
      TThread.CurrentThread.ForceQueue(nil,
        procedure
        begin
          Link.DisposeOf;
        end);
      {$ENDIF}
    finally
      TStyleCache.Current.Enabled := SaveCacheEnabled;
      FDisableAlign := SaveAlign;
    end;
  end
  else
    InternalFreeStyle;
end;

procedure TStyledControl.DoStyleChanged;
begin
  FInflated := False;
  if csDestroying in ComponentState then
    Exit;
  KillResourceLink;
  if csLoading in ComponentState then
    Exit;
  Repaint;                             
end;

procedure TStyledControl.AdjustSize;
var
  PlatformDefault: Boolean;
begin
  if AdjustType <> TAdjustType.None then
  begin
    PlatformDefault := Size.PlatformDefault;
    try
      case AdjustType of
        TAdjustType.FixedSize: Size.Size := TSizeF.Create(AdjustSizeValue.Width, AdjustSizeValue.Height);
        TAdjustType.FixedWidth: Size.Width := AdjustSizeValue.Width;
        TAdjustType.FixedHeight: Size.Height := AdjustSizeValue.Height;
      end;
    finally
      Size.SetPlatformDefaultWithoutNotification(PlatformDefault);
    end;
  end;
end;

function TStyledControl.ChooseAdjustType(const FixedSize: TSize): TAdjustType;
begin
  if (FixedSize.Width = 0) and (FixedSize.Height <> 0) then
    Result := TAdjustType.FixedHeight
  else if (FixedSize.Width <> 0) and (FixedSize.Height = 0) then
    Result := TAdjustType.FixedWidth
  else
    Result := TAdjustType.FixedSize
end;

procedure TStyledControl.AdjustFixedSize(const ReferenceControl: TControl);
var
  Link: TControl;
  LAdjustType: TAdjustType;
  LAdjustSizeValue: TSizeF;
begin
  if ReferenceControl <> Self then
    Link := ReferenceControl
  else
    Link := ResourceControl;

  if (Link <> nil) and not Link.FixedSize.IsZero then
  begin
    LAdjustSizeValue := Link.FixedSize;
    SetAdjustSizeValue(LAdjustSizeValue);

    LAdjustType := ChooseAdjustType(Link.FixedSize);
    if LAdjustType <> TAdjustType.None then
    begin
      SetAdjustType(LAdjustType);
      AdjustSize;
    end;
  end
  else
    SetAdjustType(TAdjustType.None);
end;

procedure TStyledControl.ApplyStyle;
var
  NewT: string;
  Entry: TPair<string, TValue>;
begin
  if FIsFocused and CanFocus and not FDisableFocusEffect and not GlobalDisableFocusEffect then
  begin
    FRecalcUpdateRect := True;
    Repaint;
    StartTriggerAnimation(Self, 'IsFocused');
    ApplyTriggerEffect(Self, 'IsFocused');
  end;
  { translate }
  if FAutoTranslate and ShowHint and (Hint <> '') then
  begin
    NewT := Translate(Hint);
    // need for collection texts
    if not(csDesigning in ComponentState) then
      Hint := NewT;
  end;
  AdjustFixedSize(Self);
  { StylesData }
  if (FStylesData <> nil) and (FStylesData.Count > 0) then
  begin
    for Entry in FStylesData do
      StylesData[Entry.Key] := Entry.Value;
  end;
  if (Align <> TAlignLayout.None) and (AdjustType <> TAdjustType.None) and (Parent is TControl) then
    TControl(Parent).Realign;

  FInflated := True;
end;

procedure TStyledControl.InternalFreeStyle;
begin
  FStyleState := TStyleState.Freeing;
  try
    FIsNeedStyleLookup := True;
    FreeStyle;
    FStyleState := TStyleState.Unapplied;
  except
    FStyleState := TStyleState.Error;
    raise;
  end;
end;

procedure TStyledControl.FreeStyle;
var
  Entry: TPair<string, TValue>;
  ResControl: TControl;
begin
  { StylesData }
  if (FStylesData <> nil) and (FStylesData.Count > 0) then
    for Entry in FStylesData do
      FStylesData.AddOrSetValue(Entry.Key, StylesData[Entry.Key]);
  ResControl := ResourceControl;
  if ResControl <> nil then
    ResControl.RecalcEnabled;
  FResourceLink := nil;
end;

procedure TStyledControl.Inflate;
var
  StyleProto: TFmxObject;
begin
  if FInflated then Exit;
  StyleProto := GetStyleObject(False);
  if StyleProto is TControl then
    AdjustFixedSize(TControl(StyleProto))
  else
  begin
    DisableDisappear := True;
    try
      ApplyStyleLookup;
    finally
      DisableDisappear := False;
    end;
    Disappear;
  end;

  FInflated := True;
end;

function FindProperty(var O: TObject; Path: String; const Apply: TPropertyApplyProc): Boolean;
var
  Persistent: string;
  P: TRttiProperty;
  T: TRttiType;
  Instance: TObject;
begin
  Result := False;
  Instance := O;

  while Path.Contains('.') do
  begin
    Persistent := GetToken(Path, '.');
    T := SharedContext.GetType(Instance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(Persistent);
      if (P <> nil) and P.PropertyType.IsInstance then
        Instance := P.GetValue(Instance).AsObject
    end;
  end;
  if Instance <> nil then
  begin
    T := SharedContext.GetType(Instance.ClassInfo);
    P := T.GetProperty(Path);
    O := Instance;
    Result := P <> nil;
    Apply(O, P);
  end;
end;

function TStyledControl.GetStyleContext: TFmxObject;
begin
  Result := Self;
end;

function TStyledControl.GetStyleData(const Index: string): TValue;
begin
  Result := RequestStyleData(Index);
  if Result.IsEmpty and (FStylesData <> nil) then
    FStylesData.TryGetValue(Index, Result);
end;

procedure TStyledControl.SetStyleData(const Index: string; const Value: TValue);
begin
  if FStylesData = nil then
    FStylesData := TDictionary<string, TValue>.Create;
  FStylesData.AddOrSetValue(Index, Value);

  StyleDataChanged(Index, Value);
end;

function TStyledControl.GenerateStyleName(const AClassName: string): string;
const
  CustomClassPrefix = 'TCustom'; // Do not localize
begin
  Result := AClassName;
  if (Result.Length > 1) and Result.Contains(CustomClassPrefix) then
    Result := Result.Remove(0, CustomClassPrefix.Length);
  if (Result.Length > 1) and (Result.Chars[0].ToUpper = 'T') then
    Result := Result.Remove(0, 1);
  Result := Result + StyleSuffix;
end;

function TStyledControl.GetDefaultStyleLookupName: string;
begin
  Result := GenerateStyleName(ClassName);
end;

function TStyledControl.GetParentClassStyleLookupName: string;
begin
  Result := GenerateStyleName(ClassParent.ClassName);
end;

function TStyledControl.GetResourceControl: TControl;
var
  ResLink: TFmxObject;
begin
  Result := nil;
  ResLink := GetResourceLink;
  if ResLink is TControl then
    Result := TControl(ResLink);
end;

function TStyledControl.GetResourceLink: TFmxObject;
begin
  Result := FResourceLink;
end;

procedure TStyledControl.DoEnter;
begin
  ApplyStyleLookup;
  inherited;
end;

function TStyledControl.DoSetSize(const ASize: TControlSize; const NewPlatformDefault: Boolean;
  ANewWidth, ANewHeight: Single; var ALastWidth, ALastHeight: Single): Boolean;
begin
  if FAdjustType in [TAdjustType.FixedSize, TAdjustType.FixedWidth] then
    ANewWidth := FAdjustSizeValue.Width;
  if FAdjustType in [TAdjustType.FixedSize, TAdjustType.FixedHeight] then
    ANewHeight := FAdjustSizeValue.Height;
  Result := inherited;
end;

procedure TStyledControl.SetStyleLookup(const Value: string);
begin
  FStyleLookup := Value;
  StyleLookupChanged;
  NeedStyleLookup;
  if not (csLoading in ComponentState) and ((Owner = nil) or (not (csLoading in Owner.ComponentState)))
    and (FScene <> nil) and not IsUpdating then
    ApplyStyleLookup;
end;

procedure TStyledControl.StartTriggerAnimation(const AInstance: TFmxObject; const ATrigger: string);
begin
  DisableDisappear := True;
  try
    inherited;
  finally
    DisableDisappear := False;
  end;
end;

procedure TStyledControl.StartTriggerAnimationWait(const AInstance: TFmxObject; const ATrigger: string);
begin
  DisableDisappear := True;
  try
    inherited;
  finally
    DisableDisappear := False;
  end;
end;

procedure TStyledControl.StyleChangedHandler(const Sender: TObject; const Msg: TMessage);
begin
  if (TStyleChangedMessage(Msg).Scene <> nil) and (TStyleChangedMessage(Msg).Scene <> Scene) then
    Exit;
  if (TStyleChangedMessage(Msg).Value <> nil) and (Scene <> nil) and
    (Scene.StyleBook <> TStyleChangedMessage(Msg).Value) then
    Exit;
  DoStyleChanged;
end;

procedure TStyledControl.StyleDataChanged(const Index: string; const Value: TValue);
var
  Obj: TObject;
  InstanceName, PropertyName: string;
  PropertyValue: TValue;
begin
  PropertyName := Index;
  InstanceName := GetToken(PropertyName, '.');

  Obj := FindStyleResource(InstanceName);
  if Obj <> nil then
  begin
    if not PropertyName.IsEmpty then
    begin
      PropertyValue := Value;
      FindProperty(Obj, PropertyName,
        procedure (Instance: TObject; Prop: TRttiProperty)
        begin
          Prop.SetValue(Instance, PropertyValue);
        end);
    end else
      TFmxObject(Obj).Data := Value;
  end
end;

procedure TStyledControl.StyleLookupChanged;
begin
end;

function TStyledControl.GetAdjustSizeValue: TSizeF;
begin
  Result := FAdjustSizeValue;
end;

function TStyledControl.GetAdjustType: TAdjustType;
begin
  Result := FAdjustType;
end;

function TStyledControl.GetBackIndex: Integer;
begin
  Result := 1;
end;

function TStyledControl.IsHelpContextStored: Boolean;
begin
  // Result := (FActionLink = nil) or not FActionLink.IsHelpContextLinked;
  Result := (FHelpContext <> 0);
end;

procedure TStyledControl.ScaleChangedHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
begin
  DoStyleChanged;
end;

procedure TStyledControl.SetAdjustSizeValue(const Value: TSizeF);
begin
  FAdjustSizeValue := Value;
end;

procedure TStyledControl.SetAdjustType(const Value: TAdjustType);
begin
  FAdjustType := Value;
end;

procedure TStyledControl.SetHelpContext(const Value: THelpContext);
begin
  if not(csLoading in ComponentState) then
    FHelpType := htContext;
  FHelpContext := Value;
end;

procedure TStyledControl.SetHelpKeyword(const Value: string);
begin
  if not(csLoading in ComponentState) then
    FHelpType := htKeyword;
  FHelpKeyword := Value;
end;

{ TStyleContainer }

constructor TStyleContainer.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TStyleContainer.Destroy;
var
  StyleObject: TFmxObject;
begin
  ClearContainer;
  FreeAndNil(FBinaryDict);
  if Children <> nil then
    for StyleObject in Children do
      StyleObject.StyleName := '';
  inherited;
end;

procedure TStyleContainer.AddObjectFromStream(const Name: string; const SourceStream: TStream; const Size: Int64);
var
  Binary: TMemoryStream;
  StyleObject: TFmxObject;
begin
  Binary := TMemoryStream.Create;
  try
    Binary.CopyFrom(SourceStream, Size);
    StyleObject := LoadStyleResource(Binary);
    AddObject(StyleObject);
  finally
    Binary.Free;
  end;
end;

procedure TStyleContainer.AddBinaryFromStream(const Name: string; const SourceStream: TStream; const Size: Int64);
{$IFNDEF WIN32}
var
  Binary: TMemoryStream;
{$ENDIF}
begin
  {$IFDEF WIN32}
  AddObjectFromStream(Name, SourceStream, Size);
  {$ELSE}
  if TBitmapCodecManager.CodecExists(Name) or Name.Contains(StyleDescriptionName.ToLowerInvariant) then
    AddObjectFromStream(Name, SourceStream, Size)
  else
  begin
    if FBinaryDict = nil then
      FBinaryDict := TDictionary<string, TMemoryStream>.Create(300);

    if not FBinaryDict.ContainsKey(Name) then
    begin
      Binary := TMemoryStream.Create;
      Binary.CopyFrom(SourceStream, Size);

      FBinaryDict.Add(Name, Binary);
    end
    else
      SourceStream.Seek(Size, TSeekOrigin.soCurrent);
  end;
  {$ENDIF}
end;

procedure TStyleContainer.ClearContainer;
{$IFNDEF AUTOREFCOUNT}
var
  Stream: TStream;
{$ENDIF}
begin
  if FBinaryDict <> nil then
  begin
{$IFNDEF AUTOREFCOUNT}
    for Stream in FBinaryDict.Values do
      Stream.Free;
{$ENDIF}
    FBinaryDict.Clear;
  end;
end;

function TStyleContainer.LoadStyleResource(const AStream: TStream): TFmxObject;
var
  Reader: TReader;
  SavedStyle: TFmxObject;
begin
  AStream.Position := 0;

  BeginGlobalLoading;
  try
    Reader := TReader.Create(AStream, 4096);
    try
      SavedStyle := TStyledControl.LoadableStyle;
      try
        TStyledControl.LoadableStyle := Self;
        Result := TFmxObject(Reader.ReadRootComponent(nil));
      finally
        TStyledControl.LoadableStyle := SavedStyle;
      end;
    finally
      Reader.Free;
    end;
    NotifyGlobalLoading;
  finally
    EndGlobalLoading;
  end;
end;

function TStyleContainer.CreateStyleResource(const AStyleLookup: string): TFmxObject;
var
  BinaryStream: TMemoryStream;
begin
  Result := nil;
  if (FBinaryDict <> nil) and FBinaryDict.TryGetValue(AStyleLookup.ToLowerInvariant, BinaryStream) then
  begin
    Result := LoadStyleResource(BinaryStream);
    AddObject(Result);
    ChangeChildren;
  end;
end;

function TStyleContainer.FindStyleResource(const AStyleLookup: string; const Clone: Boolean = False): TFmxObject;
begin
  if Clone then
  begin
    Result := CreateStyleResource(AStyleLookup);
    if Result = nil then
      Result := inherited FindStyleResource(AStyleLookup, Clone);
  end
  else
  begin
    Result := inherited FindStyleResource(AStyleLookup, Clone);
    if Result = nil then
      Result := CreateStyleResource(AStyleLookup);
  end;
end;

procedure TStyleContainer.UnpackAllBinaries;
var
  Pair: TPair<string, TMemoryStream>;
begin
  if (FBinaryDict <> nil) then
  begin
    for Pair in FBinaryDict.ToArray do
      if inherited FindStyleResource(Pair.Key, False) = nil then
        AddObject(LoadStyleResource(Pair.Value));
    ChangeChildren;
  end;
end;

{ TStyleCollectionItem }

function CheckPlatformTargetAtDesignTime(const PlatformTarget: string): Boolean;
begin
  Result := True;
end;

function CheckPlatformTargetAtRunTime(const PlatformTarget: string): Boolean;
const
  AlternateTarget = '[IOSALTERNATE]';
begin
  Result := TStyleStreaming.DefaultIsSupportedPlatformTarget(PlatformTarget);
  {$IFDEF WIN32}
  if Result then
    Result := not PlatformTarget.ToUpper.Contains(AlternateTarget);
  {$ENDIF}
end;

constructor TStyleCollectionItem.Create(Collection: TCollection);
begin
  inherited;
  FStyleBook := TStyleCollection(Collection).FStyleBook;
  FBinary := TMemoryStream.Create;
  FStyle := TStyleContainer.Create(nil);
end;

destructor TStyleCollectionItem.Destroy;
begin
  FBinary.Free;
  FStyle.Free;
  inherited;
  if FStyleBook <> nil then
    FStyleBook.CollectionChanged;
end;

procedure TStyleCollectionItem.Assign(Source: TPersistent);
var
  SourceItem: TStyleCollectionItem;
begin
  if Source is TStyleCollectionItem then
  begin
    SourceItem := TStyleCollectionItem(Source);
    FPlatform := SourceItem.FPlatform;

    FStyle.DeleteChildren;
    FBinary.Clear;

    if SourceItem.FBinary.Size > 0 then
      FBinary.Write(SourceItem.FBinary.Memory^, SourceItem.FBinary.Size)
    else if SourceItem.FStyle.ChildrenCount > 0 then
      TStyleStreaming.SaveToStream(SourceItem.FStyle, FBinary);

    FNeedLoadFromBinary := True;

    if FStyleBook <> nil then
      FStyleBook.CollectionChanged;
  end
  else
    inherited;
end;

procedure TStyleCollectionItem.Clear;
begin
  FStyle.DeleteChildren;
  FBinary.Clear;
end;

procedure TStyleCollectionItem.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('ResourcesBin', ReadResources, WriteResources, StyleStored);
end;

procedure TStyleCollectionItem.ReadResources(Stream: TStream);
begin
  FBinary.Clear;
  FBinary.CopyFrom(Stream, Stream.Size);
  FNeedLoadFromBinary := True;
end;

procedure TStyleCollectionItem.WriteResources(Stream: TStream);
begin
  if FBinary.Size > 0 then
    Stream.Write(FBinary.Memory^, FBinary.Size)
  else
    TStyleStreaming.SaveToStream(FStyle, Stream, TStyleFormat.Indexed);
end;

procedure TStyleCollectionItem.LoadFromBinary;
begin
  if FBinary.Size > 0 then
  begin
    FBinary.Position := 0;
    LoadFromStream(FBinary);
  end
  else
    FStyle.DeleteChildren;
  FNeedLoadFromBinary := False;
end;

procedure TStyleCollectionItem.LoadFromFile(const FileName: string);
begin
  if (FStyleBook <> nil) and (csDesigning in FStyleBook.ComponentState) then
    TStyleStreaming.SetSupportedPlatformHook(CheckPlatformTargetAtDesignTime)
  else
    TStyleStreaming.SetSupportedPlatformHook(CheckPlatformTargetAtRunTime);
  try
    if TStyleStreaming.CanLoadFromFile(FileName) then
    begin
      FreeAndNil(FStyle);
      FStyle := TStyleStreaming.LoadFromFile(FileName);
      TStyleManager.RemoveStyleFromGlobalPool(FStyle);
      FUnsupportedPlatform := False;

      FBinary.Clear;
      if FStyle.ChildrenCount > 0 then
        TStyleStreaming.SaveToStream(FStyle, FBinary);
    end
    else
      FUnsupportedPlatform := True;
  finally
    TStyleStreaming.SetSupportedPlatformHook(nil);
  end;
end;

procedure TStyleCollectionItem.LoadFromStream(const Stream: TStream);
begin
  if (FStyleBook <> nil) and (csDesigning in FStyleBook.ComponentState) then
    TStyleStreaming.SetSupportedPlatformHook(CheckPlatformTargetAtDesignTime)
  else
    TStyleStreaming.SetSupportedPlatformHook(CheckPlatformTargetAtRunTime);
  try
    if TStyleStreaming.CanLoadFromStream(Stream) then
    begin
      FreeAndNil(FStyle);
      FStyle := TStyleStreaming.LoadFromStream(Stream);
      TStyleManager.RemoveStyleFromGlobalPool(FStyle);
      FUnsupportedPlatform := False;

      if Stream <> FBinary then
      begin
        FBinary.Clear;
        if FStyle.ChildrenCount > 0 then
          TStyleStreaming.SaveToStream(FStyle, FBinary);
      end;
    end
    else
      FUnsupportedPlatform := True;
  finally
    TStyleStreaming.SetSupportedPlatformHook(nil);
  end;
end;

procedure TStyleCollectionItem.SaveToBinary;
begin
  if not FNeedLoadFromBinary then
  begin
    FBinary.Clear;
    if FStyle.ChildrenCount > 0 then
      TStyleStreaming.SaveToStream(FStyle, FBinary);
  end;
end;

procedure TStyleCollectionItem.SaveToStream(const Stream: TStream; const Format: TStyleFormat = TStyleFormat.Indexed);
begin
  TStyleStreaming.SaveToStream(FStyle, Stream, Format);
end;

function TStyleCollectionItem.GetDisplayName: string;
begin
  if not Platform.IsEmpty then
    Result := Platform
  else
    Result := DefaultItem;
end;

function TStyleCollectionItem.GetIsEmpty: Boolean;
begin
  Result := not ((FStyle.ChildrenCount > 0) or (FNeedLoadFromBinary and (FBinary.Size > 0)));
end;

function TStyleCollectionItem.GetResource: string;
begin
  Result := '';
end;

function TStyleCollectionItem.GetStyle: TFmxObject;
begin
  if FNeedLoadFromBinary then
    LoadFromBinary;
  Result := FStyle;
end;

procedure TStyleCollectionItem.SetPlatform(const Value: string);
begin
  if FPlatform <> Value then
  begin
    FPlatform := Value;
    if FStyleBook <> nil then
      FStyleBook.CollectionChanged;
  end;
end;

procedure TStyleCollectionItem.SetResource(const Value: string);
begin
end;

function TStyleCollectionItem.StyleStored: Boolean;
begin
  Result := not IsEmpty;
end;

{ TStyleCollection }

function TStyleCollection.Add: TStyleCollectionItem;
begin
  Result := TStyleCollectionItem(inherited Add);
end;

constructor TStyleCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TStyleCollectionItem);
  FStyleBook := TStyleBook(AOwner);
end;

function TStyleCollection.GetItem(Index: Integer): TStyleCollectionItem;
begin
  Result := TStyleCollectionItem(inherited GetItem(Index));
end;

procedure TStyleCollection.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  inherited;
  if (Action in [cnExtracting, cnDeleting]) and (FStyleBook.FCurrentItemIndex = Item.Index) then
    FStyleBook.FCurrentItemIndex := 0;
end;

procedure TStyleCollection.SetItem(Index: Integer; const Value: TStyleCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TStyleBook }

constructor TStyleBook.Create(AOwner: TComponent);
begin
  inherited;
  FStyles := TStyleCollection.Create(Self);
  FStylesDic := TDictionary<string, TStyleCollectionItem>.Create;
  FResource := TStringList.Create;
  TStringList(FResource).OnChange := ResourceChanged;
  FCurrentItemIndex := 0;
  if csDesigning in ComponentState then
  begin
    FBeforeStyleChangingId := TMessageManager.DefaultManager.SubscribeToMessage(TBeforeStyleChangingMessage,
      BeforeStyleChangingHandler);
    FStyleChangedId := TMessageManager.DefaultManager.SubscribeToMessage(TStyleChangedMessage, StyleChangedHandler);
  end;
  AddCustomFindStyleResource(CustomFindStyleResource);
end;

procedure TStyleBook.CreateDefaultItem;
begin
  FStyles.Add;
end;

destructor TStyleBook.Destroy;
begin
  RemoveCustomFindStyleResource(CustomFindStyleResource);
  if csDesigning in ComponentState then
  begin
    TMessageManager.DefaultManager.Unsubscribe(TBeforeStyleChangingMessage, FBeforeStyleChangingId);
    TMessageManager.DefaultManager.Unsubscribe(TStyleChangedMessage, FStyleChangedId);
  end;
  FResource.Free;
  FStylesDic.Free;
  FStyles.Free;
  inherited;
end;

procedure TStyleBook.Loaded;
begin
  inherited;
  if not FFileName.IsEmpty then
    LoadFromFile(FFileName);
  ChooseStyleIndex;
  if not CurrentItem.IsEmpty then
  begin
    if (Style <> nil) and UseStyleManager and not (csDesigning in ComponentState) then
      TStyleManager.SetStyle(Style)
    else begin
      TMessageManager.DefaultManager.SendMessage(nil, TBeforeStyleChangingMessage.Create, True);
      TMessageManager.DefaultManager.SendMessage(nil, TStyleChangedMessage.Create(Self, nil), True);
    end;
  end;
end;

function TStyleBook.CustomFindStyleResource(const AStyleLookup: string; const Clone: Boolean): TFmxObject;
begin
  Result := nil;
  if TStyledControl.LoadableStyle = nil then
  begin
    ChooseStyleIndex;
    Result := Style.FindStyleResource(AStyleLookup, Clone);
  end;
end;

procedure TStyleBook.BeforeStyleChangingHandler(const Sender: TObject; const Msg: TMessage);
begin
  FCurrentItemIndex := 0;
end;

procedure TStyleBook.StyleChangedHandler(const Sender: TObject; const Msg: TMessage);
begin
  if TStyleChangedMessage(Msg).Value = nil then
    ChooseStyleIndex;
end;

procedure TStyleBook.RebuildDictionary;
var
  I: Integer;
begin
  if not (csDestroying in ComponentState) then
  begin
    FStylesDic.Clear;

    for I := 0 to Styles.Count - 1 do
      if not FStylesDic.ContainsKey(Styles[I].Platform) then
        FStylesDic.Add(Styles[I].Platform, Styles[I]);
  end;
end;

function TStyleBook.StyleIndexByContext(const Context: TFmxObject): Integer;
var
  Style: TFmxObject;
  Description: TStyleDescription;
  StyleBehavior: IInterface;
  DeviceBehavior: IInterface;
  OSPlatform: TOSPlatform;
  Item: TStyleCollectionItem;
  I: Integer;
begin
  if Styles.Count > 0 then
  begin
    DeviceBehavior := TBehaviorServices.Current.GetBehaviorService(IDeviceBehavior, Context);
    OSPlatform := (DeviceBehavior as IDeviceBehavior).GetOSPlatform(Context);
    for I := 0 to Styles.Count - 1 do
      if SameText(Styles[I].Platform, TStyleDescription.PlatformNames[OSPlatform]) then
        Exit(I);

    if UseStyleManager and not (csDesigning in ComponentState) then
      Exit(0)
    else
    begin
      StyleBehavior := TBehaviorServices.Current.GetBehaviorService(IStyleBehavior, Context);
      (StyleBehavior as IStyleBehavior).GetSystemStyle(Context, Style);
      Description := TStyleManager.FindStyleDescriptor(Style);
      if (Description <> nil) and FStylesDic.TryGetValue(Description.Title, Item) then
        Exit(Item.Index);
    end;
  end;
  Result := -1;
end;

procedure TStyleBook.ChooseStyleIndex;
var
  Idx: Integer;
begin
  Idx := StyleIndexByContext(Self);
  if Idx >= 0 then
    FCurrentItemIndex := Idx;
end;

procedure TStyleBook.Clear;
begin
  FStyles.Clear;
  FCurrentItemIndex := 0;
end;

procedure TStyleBook.CollectionChanged;
begin
  RebuildDictionary;
end;

procedure TStyleBook.ResourceChanged(Sender: TObject);
var
  Stream: TStream;
begin
  Clear;

  if (FResource.Count > 0) then
  begin
    Stream := TMemoryStream.Create;
    try
      FResource.SaveToStream(Stream);
      if Stream.Position > 0 then
      begin
        Stream.Position := 0;

        CurrentItem.LoadFromStream(Stream);

        if UseStyleManager and not (csDesigning in ComponentState) then
          TStyleManager.SetStyle(Style);
      end;
    finally
      Stream.Free;
    end;
  end
  else
end;

procedure TStyleBook.LoadFromStream(const Stream: TStream);
begin
  if not (csLoading in ComponentState) and TStyleStreaming.CanLoadFromStream(Stream) then
  begin
    Clear;
    CurrentItem.LoadFromStream(Stream);
  end;
end;

procedure TStyleBook.LoadFromFile(const AFileName: string);
var
  ResultFileName: string;
begin
  if not (csLoading in ComponentState) then
  begin
    FFileName := AFileName;

    ResultFileName := System.IOUtils.TPath.Combine(ExtractFilePath(ParamStr(0)), FFileName);
    if not FileExists(ResultFileName) and FileExists(FFileName) then
      ResultFileName := FFileName;

    if TStyleStreaming.CanLoadFromFile(ResultFileName) then
    begin
      Clear;
      CurrentItem.LoadFromFile(ResultFileName);
    end;
  end;
end;

procedure TStyleBook.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('ResourcesBin', ReadResources, nil, False);
  Filer.DefineProperty('HiResStyleBook', IgnoreIdentValue, nil, False);
  Filer.DefineProperty('Resource.Strings', ReadStrings, nil, False);
end;

procedure TStyleBook.ReadStrings(Reader: TReader);
var
  Str: TStrings;
  Stream: TMemoryStream;
begin
  Reader.ReadListBegin;
  try
    Str := TStringList.Create;
    try
      while not Reader.EndOfList do
        Str.Add(Reader.ReadString);

      Stream := TMemoryStream.Create;
      try
        Str.SaveToStream(Stream);
        Stream.Position := 0;
        ReadResources(Stream);
      finally
        Stream.Free;
      end;
    finally
      Str.Free;
    end;
  finally
    Reader.ReadListEnd;
  end;
end;

procedure TStyleBook.ReadResources(Stream: TStream);
var
  DefaultItem: TStyleCollectionItem;
begin
  DefaultItem := FStyles.Add;
  DefaultItem.LoadFromStream(Stream);
end;

function TStyleBook.GetCurrentItem: TStyleCollectionItem;
begin
  if Styles.Count = 0 then
    Result := Styles.Add
  else if (FCurrentItemIndex >= 0) and (FCurrentItemIndex < Styles.Count) then
    Result := Styles[FCurrentItemIndex]
  else
    Result := Styles[0];
end;

function TStyleBook.GetStyle(const Context: TFmxObject): TFmxObject;
var
  Idx: Integer;
begin
  Idx := StyleIndexByContext(Context);
  if Idx >= 0 then
    Result := Styles[Idx].Style
  else
    Result := Style;
end;

function TStyleBook.GetStyle: TFmxObject;
begin
  Result := CurrentItem.Style;
end;

function TStyleBook.GetUnsupportedPlatform: Boolean;
begin
  Result := CurrentItem.UnsupportedPlatform;
end;

procedure TStyleBook.SetFileName(const Value: string);
begin
  if FFileName <> Value then
  begin
    FFileName := Value;
    LoadFromFile(FFileName);
  end;
end;

procedure TStyleBook.SetCurrentItemIndex(const Value: Integer);
begin
  if FCurrentItemIndex <> Value then
  begin
    FCurrentItemIndex := Value;
    if FCurrentItemIndex < 0 then
      FCurrentItemIndex := 0;
    if FCurrentItemIndex >= Styles.Count then
      FCurrentItemIndex := Styles.Count - 1;
    Styles[FCurrentItemIndex].LoadFromBinary;
  end;
end;

procedure TStyleBook.SetStyles(const Value: TStyleCollection);
begin
  FStyles.Assign(Value);
end;

procedure TStyleBook.SetUseStyleManager(const Value: Boolean);
begin
  FUseStyleManager := Value;
end;

{ TStyleManagerHelper }

class function TStyleManagerHelper.ActiveStyleForScene(const Scene: IScene): TFmxObject;
var
  StyleBehavior: IInterface;
begin
  {$IFDEF MSWINDOWS}
  if (Scene <> nil) and (csDesigning in Scene.GetObject.ComponentState) and (Scene.GetObject is TCommonCustomForm) then
  begin
    StyleBehavior := TBehaviorServices.Current.GetBehaviorService(IStyleBehavior, Scene.GetObject);
    (StyleBehavior as IStyleBehavior).GetSystemStyle(Scene.GetObject, Result);
  end
  else if (Scene <> nil) and (csDesigning in Scene.GetObject.ComponentState) and (Scene.GetObject is TFmxObject) and
    (TFmxObject(Scene.GetObject).Root <> nil) and (TFmxObject(Scene.GetObject).Root.GetObject is TCommonCustomForm) then
  begin
    StyleBehavior := TBehaviorServices.Current.GetBehaviorService(IStyleBehavior, TFmxObject(Scene.GetObject).Root.GetObject);
    (StyleBehavior as IStyleBehavior).GetSystemStyle(TFmxObject(Scene.GetObject).Root.GetObject, Result);
  end else
  {$ENDIF}
  if Scene = nil then
  begin
    StyleBehavior := TBehaviorServices.Current.GetBehaviorService(IStyleBehavior, nil);
    (StyleBehavior as IStyleBehavior).GetSystemStyle(nil, Result);
  end
  else
  begin
    StyleBehavior := TBehaviorServices.Current.GetBehaviorService(IStyleBehavior, Scene.GetObject);
    (StyleBehavior as IStyleBehavior).GetSystemStyle(Scene.GetObject, Result);
  end;
end;

class function TStyleManagerHelper.GetStyleDescriptionForControl(const AObject: TControl): TStyleDescription;
var
  Resources: TFmxObject;
begin
  if AObject <> nil then
  begin
    if (AObject.Scene <> nil) and (AObject.Scene.StyleBook <> nil) then
      Resources := AObject.Scene.StyleBook.Style
    else
      Resources := ActiveStyleForScene(AObject.Scene);
    Result := FindStyleDescriptor(Resources);
    if Result = nil then
      Result := FindStyleDescriptor(ActiveStyle(AObject));
  end
  else
    Result := nil;
end;

{$REGION 'TTextSettingsInfo'}

{$REGION 'Helper classes'}

{ TTextSettingsInfo.TBaseTextSettings }

constructor TTextSettingsInfo.TBaseTextSettings.Create(const AOwner: TPersistent);
begin
  inherited;
  if AOwner is TTextSettingsInfo then
  begin
    FInfo := TTextSettingsInfo(AOwner);
    if FInfo.Owner is TControl then
      FControl := TControl(FInfo.Owner);
  end
  else
    raise EArgumentException.CreateFMT(SEUseHeirs, [TTextSettingsInfo.ClassName]);
end;

{ TTextSettingsInfo.TCustomTextSettings }

constructor TTextSettingsInfo.TCustomTextSettings.Create(const AOwner: TPersistent);
begin
  inherited;
  Trimming := TTextTrimming.None;
  WordWrap := True;
end;

{$ENDREGION}

type
  TOpenReader = class (TReader)

  end;

{ TTextSettingsInfo.TTextPropLoader }

constructor TTextSettingsInfo.TTextPropLoader.Create(const AInstance: TComponent; const AFiler: TFiler);
begin
  if (AInstance = nil) or (AFiler = nil) then
    raise EArgumentNilException.Create(SArgumentNil);
  inherited Create;
  FInstance := AInstance;
  FFiler := AFiler;
  if not FInstance.GetInterface(ITextSettings, FITextSettings) then
    raise EArgumentException.CreateFMT(SUnsupportedInterface, [FInstance.ClassName, 'ITextSettings']);
  FTextSettings := FITextSettings.TextSettings;
  if (FTextSettings = nil) then
    raise EArgumentNilException.Create(SArgumentNil);
end;

procedure TTextSettingsInfo.TTextPropLoader.ReadSet(const Instance: TPersistent; const Reader: TReader; const PropertyName: string);
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(Instance.ClassInfo, PropertyName);
  if (PropInfo <> nil) and (PropInfo.PropType <> nil) and (PropInfo.PropType^.Kind = tkSet) then
    SetOrdProp(Instance, PropInfo, TOpenReader(Reader).ReadSet(PropInfo.PropType^))
  else
    Reader.SkipValue;
end;

procedure TTextSettingsInfo.TTextPropLoader.ReadEnumeration(const Instance: TPersistent; const Reader: TReader; const PropertyName: string);
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(Instance.ClassInfo, PropertyName);
  if (PropInfo <> nil) and (PropInfo.PropType <> nil) and (PropInfo.PropType^.Kind = tkEnumeration) then
    SetEnumProp(Instance, PropertyName, Reader.ReadIdent)
  else
    Reader.SkipValue;
end;

procedure TTextSettingsInfo.TTextPropLoader.ReadFontFillColor(Reader: TReader);
var
  LFontColor: TAlphaColor;
begin
                                                                                          
{$IFDEF LONGINT64}
  IdentToAlphaColor(Reader.ReadIdent, Integer(LFontColor));
{$ELSE !LONGINT64}
  IdentToAlphaColor(Reader.ReadIdent, Longint(LFontColor));
{$ENDIF !LONGINT64}
  TextSettings.FontColor := LFontColor;
end;

procedure TTextSettingsInfo.TTextPropLoader.ReadFontFamily(Reader: TReader);
begin
  TextSettings.Font.Family := Reader.ReadString;
end;

procedure TTextSettingsInfo.TTextPropLoader.ReadFontFillKind(Reader: TReader);
begin
  Reader.ReadIdent;
end;

procedure TTextSettingsInfo.TTextPropLoader.ReadFontStyle(Reader: TReader);
begin
  ReadSet(TextSettings.Font, Reader, 'Style');
end;

procedure TTextSettingsInfo.TTextPropLoader.ReadFontSize(Reader: TReader);
begin
  TextSettings.Font.Size := Reader.ReadFloat;
end;

procedure TTextSettingsInfo.TTextPropLoader.ReadTextAlign(Reader: TReader);
begin
  ReadEnumeration(TextSettings, Reader, 'HorzAlign');
end;

procedure TTextSettingsInfo.TTextPropLoader.ReadVertTextAlign(Reader: TReader);
begin
  ReadEnumeration(TextSettings, Reader, 'VertAlign');
end;

procedure TTextSettingsInfo.TTextPropLoader.ReadWordWrap(Reader: TReader);
begin
  ReadEnumeration(TextSettings, Reader, 'WordWrap');
end;

procedure TTextSettingsInfo.TTextPropLoader.ReadTrimming(Reader: TReader);
begin
  ReadEnumeration(TextSettings, Reader, 'Trimming');
end;

procedure TTextSettingsInfo.TTextPropLoader.ReadProperties;
begin
  TextSettings.BeginUpdate;
  try
    Filer.DefineProperty('FontFill.Color', ReadFontFillColor, nil, False);
    Filer.DefineProperty('FontFill.Kind', ReadFontFillKind, nil, False);
    Filer.DefineProperty('Font.Family', ReadFontFamily, nil, False);
    Filer.DefineProperty('Font.Style', ReadFontStyle, nil, False);
    Filer.DefineProperty('Font.Size', ReadFontSize, nil, False);
    Filer.DefineProperty('FontColor', ReadFontFillColor, nil, False);
    Filer.DefineProperty('TextAlign', ReadTextAlign, nil, False);
    Filer.DefineProperty('HorzTextAlign', ReadTextAlign, nil, False);
    Filer.DefineProperty('VertTextAlign', ReadVertTextAlign, nil, False);
    Filer.DefineProperty('WordWrap', ReadWordWrap, nil, False);
    Filer.DefineProperty('Trimming', ReadTrimming, nil, False);
  finally
    TextSettings.EndUpdate;
  end;
end;

{ TTextSettingsInfo }

constructor TTextSettingsInfo.Create(AOwner: TPersistent; ATextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass);
var
  LClass: TTextSettingsInfo.TCustomTextSettingsClass;
begin
  if AOwner = nil then
    raise EArgumentNilException.Create(SArgumentNil);
  inherited Create;
  FOwner := AOwner;
  FStyledSettings := DefaultStyledSettings;
  if ATextSettingsClass = nil then
    LClass := TCustomTextSettings
  else
    LClass := ATextSettingsClass;

  FDefaultTextSettings := LClass.Create(Self);
  FDefaultTextSettings.OnChanged := OnDefaultChanged;
  FTextSettings := LClass.Create(Self);
  FTextSettings.OnChanged := OnTextChanged;
  FResultingTextSettings := LClass.Create(Self);
  FResultingTextSettings.OnChanged := OnCalculatedTextSettings;
  FOldTextSettings := LClass.Create(Self);
  FOldTextSettings.Assign(FTextSettings);
end;

destructor TTextSettingsInfo.Destroy;
begin
  FreeAndNil(FDefaultTextSettings);
  FreeAndNil(FTextSettings);
  FreeAndNil(FResultingTextSettings);
  FreeAndNil(FOldTextSettings);
  inherited;
end;

procedure TTextSettingsInfo.OnCalculatedTextSettings(Sender: TObject);
begin
  DoCalculatedTextSettings;
end;

procedure TTextSettingsInfo.OnDefaultChanged(Sender: TObject);
begin
  DoDefaultChanged;
end;

procedure TTextSettingsInfo.OnTextChanged(Sender: TObject);
begin
  DoTextChanged;
end;

procedure TTextSettingsInfo.SetDefaultTextSettings(const Value: TTextSettings);
begin
  FDefaultTextSettings.Assign(Value);
end;

procedure TTextSettingsInfo.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettings.Assign(Value);
end;

procedure TTextSettingsInfo.SetStyledSettings(const Value: TStyledSettings);
begin
  if FStyledSettings <> Value then
  begin
    FStyledSettings := Value;
    DoStyledSettingsChanged;
  end;
end;

procedure TTextSettingsInfo.DoStyledSettingsChanged;
begin
  RecalculateTextSettings;
end;

procedure TTextSettingsInfo.RecalculateTextSettings;
var
  TmpResultingTextSettings: TTextSettings;
begin
  if ResultingTextSettings <> nil then
  begin
    TmpResultingTextSettings := TTextSettingsClass(TextSettings.ClassType).Create(Self);
    try
      TmpResultingTextSettings.Assign(DefaultTextSettings);
      TmpResultingTextSettings.AssignNotStyled(TextSettings, StyledSettings);
      ResultingTextSettings.Assign(TmpResultingTextSettings);
    finally
      TmpResultingTextSettings.Free;
    end;
  end;
end;

procedure TTextSettingsInfo.DoDefaultChanged;
begin
  RecalculateTextSettings;
end;

procedure TTextSettingsInfo.DoTextChanged;
var
  LDesign: Boolean;
begin
  try
    LDesign := Design and (not (Owner is TComponent) or
      (TComponent(Owner).ComponentState * [csLoading, csDestroying, csReading] = []));
    if LDesign then
      TextSettings.UpdateStyledSettings(FOldTextSettings, DefaultTextSettings, FStyledSettings);
    RecalculateTextSettings;
  finally
    if FOldTextSettings <> nil then
      FOldTextSettings.Assign(FTextSettings);
  end;
end;

procedure TTextSettingsInfo.DoCalculatedTextSettings;
begin
end;

{$ENDREGION}

type
  TTextControlSettingsInfo = class (TTextSettingsInfo)
  private
    [Weak] FTextControl: TTextControl;
  protected
    procedure DoCalculatedTextSettings; override;
  public
    constructor Create(AOwner: TPersistent; ATextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass); override;
    property TextControl: TTextControl read FTextControl;
  end;

{ TTextControlSettingsInfo }

constructor TTextControlSettingsInfo.Create(AOwner: TPersistent; ATextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass);
begin
  inherited;
  if AOwner is TTextControl then
    FTextControl := TTextControl(AOwner)
  else
    raise EArgumentException.CreateFMT(SEUseHeirs, [TTextControl.ClassName]);
end;

procedure TTextControlSettingsInfo.DoCalculatedTextSettings;
begin
  inherited;
  FTextControl.DoChanged;
end;

{ TTextControl }
type
  TTextControlTextSettings = class (TTextSettingsInfo.TCustomTextSettings)
  published
    property Font;
    property FontColor;
  end;

constructor TTextControl.Create(AOwner: TComponent);
begin
  inherited;
  FIsChanging := True;
  FTextSettingsInfo := TTextControlSettingsInfo.Create(Self, GetTextSettingsClass);
  EnableExecuteAction := True;
  FPrefixStyle := TPrefixStyle.HidePrefix;
  FAcceleratorKey := #0;
  FAcceleratorKeyIndex := -1;
end;

destructor TTextControl.Destroy;
var
  AccelKeyService: IFMXAcceleratorKeyRegistryService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXAcceleratorKeyRegistryService, AccelKeyService) then
    AccelKeyService.UnregisterReceiver(Root, Self);
  FreeAndNil(FTextSettingsInfo);
  inherited;
end;

procedure TTextControl.AfterConstruction;
begin
  inherited;
  FIsChanging := False;
end;

procedure TTextControl.DefineProperties(Filer: TFiler);
var
  LTextPropLoader: TTextSettingsInfo.TTextPropLoader;
begin
  inherited;
  // Only for backward compatibility with old versions
  LTextPropLoader := TTextSettingsInfo.TTextPropLoader.Create(Self, Filer);
  try
    LTextPropLoader.ReadProperties;
  finally
    LTextPropLoader.Free;
  end;
end;

procedure TTextControl.SetName(const Value: TComponentName);
var
  ChangeText: Boolean;
begin
  ChangeText := not(csLoading in ComponentState) and (Name = Text) and
    ((Owner = nil) or not(Owner is TComponent) or not(csLoading in TComponent(Owner).ComponentState));
  inherited SetName(Value);
  if ChangeText then
    Text := Value;
end;

procedure TTextControl.ActionChange(Sender: TBasicAction; CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then
  begin
    if (not CheckDefaults) or (Text = '') or (Text = Name) then
     Text := TCustomAction(Sender).Text;
  end;
  inherited;
end;

function TTextControl.FindTextObject: TFmxObject;
begin
  Result := FindStyleResource('text');
end;

procedure TTextControl.ApplyStyle;
var
  S: TBrushObject;
  NewT : string;
  FontBehavior: IFontBehavior;
  AccelKeyService: IFMXAcceleratorKeyRegistryService;

  procedure SetupDefaultTextSetting(const AObject: TFmxObject;
                                      var AITextSettings: ITextSettings;
                                      var ATextObject: TControl;
                                    const ADefaultTextSettings: TTextSettings);
  var
    NewFamily: string;
    NewSize: Single;
  begin
    if (AObject <> nil) and AObject.GetInterface(IObjectState, FObjectState) then
      FObjectState.SaveState
    else
      FObjectState := nil;
    AITextSettings := nil;
    ATextObject := nil;
    if ADefaultTextSettings <> nil then
    begin
      if (AObject <> nil) and Supports(AObject, ITextSettings, AITextSettings) then
        ADefaultTextSettings.Assign(AITextSettings.TextSettings)
      else
        ADefaultTextSettings.Assign(nil);

      if FontBehavior <> nil then
      begin
        NewFamily := '';
        FontBehavior.GetDefaultFontFamily(Scene.GetObject, NewFamily);
        if NewFamily <> '' then
          ADefaultTextSettings.Font.Family := NewFamily;

        NewSize := 0;
        FontBehavior.GetDefaultFontSize(Scene.GetObject, NewSize);
        if not SameValue(NewSize, 0, TEpsilon.FontSize) then
          ADefaultTextSettings.Font.Size := NewSize;
      end;
    end;
    if (AObject is TControl) then
      ATextObject := TControl(AObject)
  end;

begin
  ResultingTextSettings.BeginUpdate;
  try
    FTextSettingsInfo.Design := False;
    { behavior }
    if Scene <> nil then
      TBehaviorServices.Current.SupportsBehaviorService(IFontBehavior, FontBehavior, Scene.GetObject);
    { from text }
    SetupDefaultTextSetting(FindTextObject, FITextSettings, FTextObject, FTextSettingsInfo.DefaultTextSettings);
    inherited;
    { from foreground }
    if FindStyleResource<TBrushObject>('foreground', S) then
    begin
      // use instead of the black, foreground color
      if (FTextSettingsInfo.DefaultTextSettings.FontColor = claBlack) or
         (FTextSettingsInfo.DefaultTextSettings.FontColor = claNull) then
        FTextSettingsInfo.DefaultTextSettings.FontColor := S.Brush.Color;
    end;
    ResultingTextSettings.Change;
  finally
    ResultingTextSettings.EndUpdate;
    FTextSettingsInfo.Design := csDesigning in ComponentState;
  end;
  if TPlatformServices.Current.SupportsPlatformService(IFMXAcceleratorKeyRegistryService, AccelKeyService) then
    AccelKeyService.RegisterReceiver(Root, Self);
  if AutoTranslate and (FText <> '') then
  begin
    NewT := Translate(Text); // need for collection texts
    if not(csDesigning in ComponentState) then
      Text := NewT;
  end;
end;

procedure TTextControl.FreeStyle;
var
  AccelKeyService: IFMXAcceleratorKeyRegistryService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXAcceleratorKeyRegistryService, AccelKeyService) then
    AccelKeyService.UnregisterReceiver(Root, Self);

  if FObjectState <> nil then
  begin
    FObjectState.RestoreState;
    FObjectState := nil;
  end
  else
    if FITextSettings <> nil then
      FITextSettings.TextSettings := FITextSettings.DefaultTextSettings;
  FITextSettings := nil;
  FTextObject := nil;
  inherited;
end;

procedure TTextControl.DoChanged;
var
  TextStr: string;
begin
  if FITextSettings <> nil then
    FITextSettings.TextSettings.BeginUpdate;
  try
    if FITextSettings <> nil then
      FITextSettings.TextSettings.Assign(ResultingTextSettings);
    if FPrefixStyle = TPrefixStyle.HidePrefix then
      TextStr := DelAmp(Text)
    else
      TextStr := Text;

    TextStr := DoFilterControlText(Text);

    if FTextObject <> nil then
      UpdateTextObject(FTextObject, TextStr)
    else
    begin
      if ResourceControl is TText then
        UpdateTextObject(ResourceControl, TextStr)
      else
      begin
        Repaint;
        UpdateEffects;
      end;
    end;
  finally
    if FITextSettings <> nil then
      FITextSettings.TextSettings.EndUpdate;
  end;
end;

procedure TTextControl.DoTextChanged;
begin

end;

function TTextControl.CanTriggerAcceleratorKey: Boolean;
begin
  Result := ParentedVisible;
end;

procedure TTextControl.DoEndUpdate;
begin
  inherited;
  if ([csLoading, csDestroying] * ComponentState = []) and
     (ResultingTextSettings.IsAdjustChanged or ResultingTextSettings.IsChanged) then
    Change;
end;

function TTextControl.DoFilterControlText(const AText: string): string;
begin
  Result := Text;
end;

procedure TTextControl.DoRootChanging(const NewRoot: IRoot);
var
  AccelKeyService: IFMXAcceleratorKeyRegistryService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXAcceleratorKeyRegistryService, AccelKeyService) then
    AccelKeyService.ChangeReceiverRoot(Self, Root, NewRoot);
  inherited;
end;

function TTextControl.CalcTextObjectSize(const MaxWidth: Single; var Size: TSizeF): Boolean;
const
  FakeText = 'P|y';

  function RoundToScale(const Value, Scale: Single): Single;
  begin
    if Scale > 0 then
      Result := Ceil(Value * Scale) / Scale
    else
      Result := Ceil(Value);
  end;

var
  Layout: TTextLayout;
  LScale: Single;
  LText: string;
  LMaxWidth: Single;

begin
  Result := False;
  if (Scene <> nil) and (TextObject <> nil) then
  begin
    LMaxWidth := MaxWidth - TextObject.Margins.Left - TextObject.Margins.Right;

    LScale := Scene.GetSceneScale;
    Layout := TTextLayoutManager.DefaultTextLayout.Create;
    try
      if TextObject is TText then
        LText := TText(TextObject).Text
      else
        LText := Text;
      Layout.BeginUpdate;
      if LText.IsEmpty then
        Layout.Text := FakeText
      else
        Layout.Text := LText;
      Layout.Font := ResultingTextSettings.Font;
      if WordWrap and (LMaxWidth > 1) then
        Layout.MaxSize := TPointF.Create(LMaxWidth, Layout.MaxSize.Y);
      Layout.WordWrap := WordWrap;
      Layout.Trimming := Trimming;
      Layout.VerticalAlign := TTextAlign.Leading;
      Layout.HorizontalAlign := TTextAlign.Leading;
      Layout.EndUpdate;
      if LText.IsEmpty then
        Size.Width := 0
      else
        Size.Width := RoundToScale(Layout.Width + Layout.TextRect.Left * 2 + ResultingTextSettings.Font.Size / 3, LScale);
      Size.Width := Size.Width + TextObject.Margins.Left + TextObject.Margins.Right;
      Size.Height := RoundToScale(Layout.Height, LScale) + TextObject.Margins.Top + TextObject.Margins.Bottom;
      Result := True;
    finally
      Layout.Free;
    end;
  end;
end;

procedure TTextControl.Change;
begin
  if not FIsChanging and ([csLoading, csDestroying] * ComponentState = []) then
  begin
    FIsChanging := True;
    try
      DoChanged;
      ResultingTextSettings.IsAdjustChanged := False;
      ResultingTextSettings.IsChanged := False;
    finally
      FIsChanging := False;
    end;
  end;
end;

procedure TTextControl.DoStyleChanged;
var
  NewT: string;
begin
  inherited;
  if AutoTranslate and (Text <> '') then
  begin
    NewT := Translate(Text); // need for collection texts
    if not(csDesigning in ComponentState) then
      Text := NewT;
  end;
end;

function TTextControl.TextStored: Boolean;
begin
  Result := ((Text <> '') and (not ActionClient)) or (not (ActionClient and (ActionLink <> nil) and
    (ActionLink.CaptionLinked) and (Action is TContainedAction)));
end;

procedure TTextControl.Loaded;
begin
  inherited;
  Change;
  FTextSettingsInfo.Design := csDesigning in ComponentState;
end;

function TTextControl.GetText: string;
begin
  Result := FText;
end;

procedure TTextControl.SetPrefixStyle(const Value: TPrefixStyle);
begin
  if FPrefixStyle <> Value then
  begin
    FPrefixStyle := Value;
    if FText.Contains('&') then
    begin
      ResultingTextSettings.IsAdjustChanged := True;
      if (FUpdating = 0) and ([csUpdating, csLoading, csDestroying] * ComponentState = []) then
      begin
        ApplyStyleLookup;
        Change;
      end;
    end;
  end;
end;

procedure TTextControl.SetText(const Value: string);
var
  AccelKeyService: IFMXAcceleratorKeyRegistryService;
begin
  if FText <> Value then
  begin
    FText := Value;
    if TPlatformServices.Current.SupportsPlatformService(IFMXAcceleratorKeyRegistryService, AccelKeyService) and
      (Root <> nil) then
      AccelKeyService.RegisterReceiver(Root, Self);

    ResultingTextSettings.IsAdjustChanged := True;
    if (FUpdating = 0) and ([csUpdating, csLoading, csDestroying] * ComponentState = []) then
    begin
      ApplyStyleLookup;
      Change;
      DoTextChanged;
    end;
  end;
end;

procedure TTextControl.SetTextInternal(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    ApplyStyleLookup;
    Change;
  end;
end;

procedure TTextControl.SetStyledSettings(const Value: TStyledSettings);
begin
  FTextSettingsInfo.StyledSettings := Value;
end;

function TTextControl.GetAcceleratorChar: Char;
var
  AccelKeyService: IFMXAcceleratorKeyRegistryService;
begin
  if (FAcceleratorKeyIndex < 0) and TPlatformServices.Current.SupportsPlatformService(IFMXAcceleratorKeyRegistryService,
    AccelKeyService) then
    AccelKeyService.ExtractAcceleratorKey(Text, FAcceleratorKey, FAcceleratorKeyIndex);

  Result := FAcceleratorKey;
end;

function TTextControl.GetAcceleratorCharIndex: Integer;
var
  AccelKeyService: IFMXAcceleratorKeyRegistryService;
begin
  if (FAcceleratorKeyIndex < 0) and TPlatformServices.Current.SupportsPlatformService(IFMXAcceleratorKeyRegistryService,
    AccelKeyService) then
    AccelKeyService.ExtractAcceleratorKey(Text, FAcceleratorKey, FAcceleratorKeyIndex);

  Result := FAcceleratorKeyIndex;
end;

function TTextControl.GetData: TValue;
begin
  Result := Text;
end;

procedure TTextControl.SetData(const Value: TValue);
begin
  if Value.IsEmpty then
    Text := ''
  else
    Text := Value.ToString;
end;

function TTextControl.GetFont: TFont;
begin
  Result := FTextSettingsInfo.TextSettings.Font;
end;

procedure TTextControl.SetFont(const Value: TFont);
begin
  FTextSettingsInfo.TextSettings.Font := Value;
end;

function TTextControl.GetFontColor: TAlphaColor;
begin
  Result := FTextSettingsInfo.TextSettings.FontColor;
end;

function TTextControl.GetResultingTextSettings: TTextSettings;
begin
  Result := FTextSettingsInfo.ResultingTextSettings;
end;

procedure TTextControl.SetFontColor(const Value: TAlphaColor);
begin
  FTextSettingsInfo.TextSettings.FontColor := Value;
end;

function TTextControl.GetTextAlign: TTextAlign;
begin
  Result := FTextSettingsInfo.TextSettings.HorzAlign;
end;

procedure TTextControl.SetTextAlign(const Value: TTextAlign);
begin
  FTextSettingsInfo.TextSettings.HorzAlign := Value;
end;

function TTextControl.GetVertTextAlign: TTextAlign;
begin
  Result := FTextSettingsInfo.TextSettings.VertAlign;
end;

procedure TTextControl.SetVertTextAlign(const Value: TTextAlign);
begin
  FTextSettingsInfo.TextSettings.VertAlign := Value;
end;

function TTextControl.GetWordWrap: Boolean;
begin
  Result := FTextSettingsInfo.TextSettings.WordWrap;
end;

procedure TTextControl.SetWordWrap(const Value: Boolean);
begin
  FTextSettingsInfo.TextSettings.WordWrap := Value;
end;

function TTextControl.StyledSettingsStored: Boolean;
begin
  Result := StyledSettings <> DefaultStyledSettings;
end;

function TTextControl.ToString: string;
begin
  Result := Format('%s ''%s''', [inherited ToString, FText]);
end;

procedure TTextControl.TriggerAcceleratorKey;
begin
  SetFocus;
end;

procedure TTextControl.UpdateTextObject(const TextControl: TControl; const Str: string);
var
  Caption: ICaption;
begin
  if TextControl <> nil then
  begin
    if TextControl is TText then
    begin
      TText(TextControl).Text := Str;
      TText(TextControl).Width := Min(TText(TextControl).Width, Width - TText(TextControl).Position.X - 5);
    end;
    if Supports(TextControl, ICaption, Caption) then
      Caption.Text := Str;
    TextControl.UpdateEffects;
    UpdateEffects;
    TextControl.Repaint;
  end;
end;

function TTextControl.GetDefaultTextSettings: TTextSettings;
begin
  Result := FTextSettingsInfo.DefaultTextSettings;
end;

function TTextControl.GetTextSettings: TTextSettings;
begin
  Result := FTextSettingsInfo.TextSettings;
end;

function TTextControl.GetTextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass;
begin
  Result := TTextControlTextSettings;
end;

procedure TTextControl.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettingsInfo.TextSettings.Assign(Value);
end;

function TTextControl.GetStyledSettings: TStyledSettings;
begin
  Result := FTextSettingsInfo.StyledSettings;
end;

function TTextControl.GetTrimming: TTextTrimming;
begin
  Result := FTextSettingsInfo.TextSettings.Trimming;
end;

procedure TTextControl.SetTrimming(const Value: TTextTrimming);
begin
  FTextSettingsInfo.TextSettings.Trimming := Value;
end;

{ TContent }

constructor TContent.Create(AOwner: TComponent);
begin
  inherited;
  SetAcceptsControls(False);
end;

procedure TContent.DoRealign;
var
  AlignRoot: IAlignRoot;
begin
  if (Parent <> nil) and not(csLoading in Parent.ComponentState) then
    inherited;
  if (Parent <> nil) and not FParentAligning and not(csLoading in ComponentState) then
  begin
    FParentAligning := True;
    if ParentControl <> nil then
      ParentControl.Realign
    else
      if not(csLoading in ComponentState) and Supports(Parent, IAlignRoot, AlignRoot) then
        AlignRoot.Realign;
    FParentAligning := False;
  end;
end;

type
  TContentTabList = class(TTabList)
  protected
    function IsAddable(const TabStop: IControl): Boolean; override;
  end;

{ TContentTabList }
function TContentTabList.IsAddable(const TabStop: IControl): Boolean;
var
  Content: IControl;
  Parent: TFmxObject;
begin
  Result := True;
  if Supports(Controller, IControl, Content) then
  begin
    Parent := Content.GetObject.Parent;
    if (Parent <> nil) and (Parent is TStyledControl) then
      Result := not TabStop.GetObject.Equals(TStyledControl(Parent).ResourceLink);
  end;

  Result := Result and inherited IsAddable(TabStop);
end;

function TContent.GetTabListClass: TTabListClass;
begin
  Result := TContentTabList;
end;

function TContent.GetTabStopController: ITabStopController;
var
  Control: IControl;
begin
  if Supports(Parent, IControl, Control) then
    Result := Control.GetTabStopController
  else
    Result := nil;
end;

procedure TContent.ContentChanged;
begin

end;

{$REGION 'TPopupList'}
type
  TPopupList = class (TComponent)
  private
    FPopupList: TList<Pointer>;
    FInClear: Boolean;
    function GetItem(const Index: Integer): TPopup;
    function GetCount: Integer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Item: TPopup);
    procedure Remove(Item: TPopup);
    procedure Delete(const Index: Integer);
    function IndexOf(Item: TPopup): Integer;
    property Count: Integer read GetCount;
    property Item[const Index: Integer]: TPopup read GetItem; default;
  end;

var
  PopupList: TPopupList;

{ TPopupList }

constructor TPopupList.Create(AOwner: TComponent);
begin
  inherited;
  FPopupList := TList<Pointer>.Create;
end;

procedure TPopupList.Delete(const Index: Integer);
var
  Instance: TPopup;
begin
  if FPopupList <> nil then
  begin
    if (0 <= Index) and (Index < Count) then
    begin
      Instance := TPopup(FPopupList[Index]);
      FPopupList.Delete(Index);
      if Instance <> nil then
        RemoveFreeNotification(Instance);
    end;
  end;
end;

destructor TPopupList.Destroy;
begin
  Clear;
  FreeAndNil(FPopupList);
  inherited;
end;

procedure TPopupList.Add(Item: TPopup);
begin
  if not (csDestroying in ComponentState) and (Item <> nil) and (FPopupList <> nil) then
  begin
    if FPopupList.IndexOf(Item) < 0 then
    begin
      TComponent(Item).FreeNotification(self);
      FPopupList.Add(Item);
    end;
  end;
end;

procedure TPopupList.Clear;
var
  I: Integer;
  Instance: TPopup;
begin
  if not FInClear then
  begin
    FInClear := True;
    try
      for I := Count - 1 downto 0 do
      begin
        Instance := TPopup(FPopupList[I]);
        FPopupList.Delete(I);
        if Instance.HasPopupForm then
          TCommonCustomForm(Instance.FPopupForm).Close;
        if Instance <> nil then
          RemoveFreeNotification(Instance);
      end;
    finally
      FInClear := False;
    end;
  end;
end;

function TPopupList.GetCount: Integer;
begin
  if FPopupList <> nil then
    Result := FPopupList.Count
  else
    Result := 0;
end;

function TPopupList.GetItem(const Index: Integer): TPopup;
begin
  if (Index >= 0) and (Index < Count) then
  begin
    Result := TPopup(FPopupList[Index]);
  end
  else
    raise EListError.CreateFMT(sArgumentOutOfRange_Index, [Index, Count]);
end;

function TPopupList.IndexOf(Item: TPopup): Integer;
begin
  Result := FPopupList.IndexOf(Item);
end;

procedure TPopupList.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited;
  if Operation = opRemove then
  begin
    if FPopupList <> nil then
    begin
      I := FPopupList.IndexOf(AComponent);
      if I >= 0 then
        FPopupList.Delete(I);
    end;
  end;
end;

procedure TPopupList.Remove(Item: TPopup);
var
  I: Integer;
  Instance: TPopup;
begin
  if FPopupList <> nil then
  begin
    I := FPopupList.IndexOf(Item);
    if I >= 0 then
    begin
      Instance := TPopup(FPopupList[I]);
      FPopupList.Delete(I);
      if Instance <> nil then
        RemoveFreeNotification(Instance);
    end;
  end;
end;
{$ENDREGION}

procedure ClosePopup(const AIndex: Integer);
begin
  if PopupList <> nil then
  begin
    PopupList[AIndex].IsOpen := False;
    PopupList.Delete(AIndex);
  end;
end;

procedure ClosePopup(Wnd: TFmxObject);
var
  Index: Integer;
begin
  if Wnd is TPopup then
  begin
    Index := PopupList.IndexOf(Wnd as TPopup);
    ClosePopup(Index);
  end;
end;

procedure CloseAllPopups;
var
  I: Integer;
begin
  { close other popups }
  if PopupList <> nil then
  begin
    I := PopupList.Count - 1;
    while I >= 0 do
    begin
      PopupList[I].IsOpen := False;
      I := Min(I - 1, PopupList.Count - 1);
    end;
    PopupList.Clear;
  end;
end;

function IsPopup(const Wnd: TFmxObject): boolean;
var I: integer;
begin
  Result := (PopupList <> nil) and (Wnd is TCommonCustomForm);
  if Result then
  begin
    Result := False;
    for I := 0 to PopupList.Count - 1 do
      if Wnd = PopupList[I].FPopupForm then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

function CanClosePopup(const Wnd: TFmxObject): boolean;
begin
  Result := IsPopup(Wnd) and
           (TCommonCustomForm(Wnd).FormStyle = TFormStyle.Popup) and
           (Wnd = PopupList[PopupList.Count - 1].FPopupForm);
end;

procedure PopupBringToFront;
begin
  if (PopupList <> nil) and (PopupList.Count > 0) then
    PopupList[PopupList.Count - 1].BringToFront;
end;

function GetPopup(const AIndex: Integer): TFmxObject;
begin
  if PopupList <> nil then
    Result := PopupList.Item[AIndex]
  else
    Result := nil;
end;

function GetPopupCount: Integer;
begin
  if PopupList <> nil then
    Result := PopupList.Count
  else
    Result := 0;
end;

type
  TPopupBounds = class (TBounds)
  private
    [Weak] FPopup: TPopup;
  protected
    procedure DoChange; override;
  end;

{ TPopupBounds }

procedure TPopupBounds.DoChange;
begin
  inherited;
  if (FPopup <> nil) and (FPopup.FPopupForm <> nil) then
    TCustomPopupForm(FPopup.FPopupForm).PlacementRectangle := self;
end;



{ TPopup }

constructor TPopup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPreferedDisplayIndex := -1;
  FPlacement := TPlacement.Bottom;
  FBorderWidth := 8;
  FPlacementRectangle := TPopupBounds.Create(TRectF.Empty);
  TPopupBounds(FPlacementRectangle).FPopup := Self;
  Visible := False;
  CanFocus := True;
  SetAcceptsControls(False);
end;

destructor TPopup.Destroy;
begin
  ClosePopup;
  if HasPopupForm then
  begin
    if Parent = FPopupForm then
      Parent := nil;
    FreeAndNil(FPopupForm);
  end;
  FreeAndNil(FPlacementRectangle);
  inherited;
end;

procedure TPopup.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Left', ReadLeft, nil, False);
  Filer.DefineProperty('Top', ReadTop, nil, False);
  inherited;
end;

procedure TPopup.ReadLeft(Reader: TReader);
var
  W: Single;
begin
  W := PlacementRectangle.Width;
  PlacementRectangle.Left := Reader.ReadFloat;
  PlacementRectangle.Right := PlacementRectangle.Left + W;
end;

procedure TPopup.ReadTop(Reader: TReader);
var
  H: Single;
begin
  H := PlacementRectangle.Height;
  PlacementRectangle.Top := Reader.ReadFloat;
  PlacementRectangle.Bottom := PlacementRectangle.Top + H;
end;

procedure TPopup.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

function TPopup.HasPopupForm: Boolean;
begin
  Result := FPopupForm <> nil;
end;

function TPopup.CreatePopupForm: TFmxObject;
var
  NewStyle: TStyleBook;
  NewForm: TCustomPopupForm;
begin
  NewForm := nil;
  try
    if (StyleBook = nil) and (Scene <> nil) then
      NewStyle := Scene.GetStyleBook
    else
      NewStyle := StyleBook;
    NewForm := TCustomPopupForm.Create(Self, NewStyle, PlacementTarget, False);
  except
    FreeAndNil(NewForm);
    Raise;
  end;
  Result := NewForm;
end;

procedure TPopup.Popup(const AShowModal: Boolean = False);
var
  StyledControl: TStyledControl;
  LScale: TPointF;
begin
  if FIsOpen then
    Exit;
  if HasPopupForm then
  begin
    TCustomPopupForm(FPopupForm).AniDuration := -1;
    TCustomPopupForm(FPopupForm).Close;
  end;

  FSaveScale := Scale.Point;
  FSaveParent := Parent;

  if PopupList = nil then
    PopupList := TPopupList.Create(nil);
  PopupList.Add(Self);


  FPopupForm := CreatePopupForm;
  if not (FPopupForm is TCustomPopupForm) then
  begin
    FreeAndNil(FPopupForm);
    raise EArgumentException.CreateFMT(SEUseHeirs, [TCustomPopupForm.ClassName]);
  end;
  try
    LScale := AbsoluteScale;
    TCustomForm(FPopupForm).BeginUpdate;
    try
      if AShowModal then
        TCustomPopupForm(FPopupForm).FormStyle := TFormStyle.Normal;
      if Owner is TFmxObject then
        FPopupForm.Parent := TFmxObject(Owner);
      TComponent(FPopupForm).FreeNotification(Self);
      TCustomPopupForm(FPopupForm).PreferedDisplayIndex := PreferedDisplayIndex;
      TCustomPopupForm(FPopupForm).OnAniTimer := OnAniTimer;
      TCustomPopupForm(FPopupForm).AniDuration := AniDuration;
      TCustomPopupForm(FPopupForm).Placement := Placement;
      TCustomPopupForm(FPopupForm).Offset := TPointF.Create(self.HorizontalOffset, self.VerticalOffset);
      TCustomPopupForm(FPopupForm).PlacementRectangle := Self.PlacementRectangle;
      TCustomPopupForm(FPopupForm).DragWithParent := DragWithParent;
      TCustomPopupForm(FPopupForm).StyleLookup := StyleLookup;
      UpdatePopupSize;
      TCustomPopupForm(FPopupForm).BeforeShow := BeforeShowProc;
      TCustomPopupForm(FPopupForm).BeforeClose := BeforeCloseProc;
      TCustomPopupForm(FPopupForm).OnClose := CloseProc;
      if (ControlsCount = 1) and (Controls[0].Align = TAlignLayout.Client) and (Controls[0] is TStyledControl) then
      begin
        StyledControl := TStyledControl(Controls[0]);
        if StyledControl.ResourceControl <> nil then
          TCustomPopupForm(FPopupForm).ContentPadding.Rect := StyledControl.ResourceControl.Padding.Rect;
      end;
      TCustomPopupForm(FPopupForm).Padding.Left := BorderWidth * LScale.X;
      TCustomPopupForm(FPopupForm).Padding.Top := BorderWidth * LScale.Y;
      TCustomPopupForm(FPopupForm).Padding.Right := BorderWidth * LScale.X;
      TCustomPopupForm(FPopupForm).Padding.Bottom := BorderWidth * LScale.Y;
      TCustomPopupForm(FPopupForm).ContentControl := Self;
    finally
      TCustomForm(FPopupForm).EndUpdate;
    end;
  except
    FreeAndNil(FPopupForm);
    Raise;
  end;
  { show }
  if AShowModal then
    FModalResult := TCustomPopupForm(FPopupForm).ShowModal
  else
    TCustomPopupForm(FPopupForm).Show;
end;

procedure TPopup.SetModalResult(const Value: TModalResult);
begin
  FModalResult := Value;
end;

function TPopup.PopupModal: TModalResult;
var
  LStaysOpen : Boolean;
begin
  Result := 0;
  if FIsOpen then
    Exit;
  if HasPopupForm then
  begin
    TCustomPopupForm(FPopupForm).AniDuration := -1;
    TCustomPopupForm(FPopupForm).Close;
  end;

  LStaysOpen := FStaysOpen;
  try
    FStaysOpen := True;
    Popup(True);
    Result := FModalResult;
    IsOpen := False;
  finally
    FStaysOpen := LStaysOpen;
  end;
end;

procedure TPopup.DialogKey(var Key: Word; Shift: TShiftState);
begin
  inherited DialogKey(Key, Shift);
  if (Key = vkEscape) and
     (HasPopupForm and (TCustomPopupForm(FPopupForm).FormStyle <> TFormStyle.Popup)) then
  begin
    IsOpen := False;
    Key := 0;
  end;
end;

procedure TPopup.ApplyStyle;
var
  Background: TControl;
begin
  inherited ApplyStyle;
  if FindStyleResource<TControl>('background', Background) then
    Padding.Rect := Background.Padding.Rect;
end;

procedure TPopup.ClosePopup;
begin
  if not HasPopupForm then
    Exit;
  if FModal and (FModalResult = 0) then
  begin
    ModalResult := mrCancel;
    Exit;
  end;
  { trigger }
  FIsOpen := False;
  { remove self }
  FPopupForm.RemoveObject(Self);
  { hide }
  Visible := False;
  TOpenForm(FPopupForm).SetStyleBookWithoutUpdate(nil);
  SetNewScene(nil);
  FClosingAnimation := False;
  { free }
  if not (csDestroying in ComponentState) then
  begin
    if HasPopupForm then
    begin
      DoClosePopup;
      FPopupForm := nil;
    end;
    Parent := FSaveParent;
    FSaveParent := nil;
    Scale.Point := FSaveScale;
  end;
end;

procedure TPopup.DoClosePopup;
begin
  if Assigned(FOnClosePopup) then
    FOnClosePopup(Self);
end;

procedure TPopup.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if (AComponent = FPopupForm) then
      FPopupForm := nil;
    if (AComponent = FPlacementTarget) then
    begin
      FPlacementTarget := nil;
      if HasPopupForm then
        TCustomPopupForm(FPopupForm).PlacementTarget := nil;
    end;
    if (AComponent = FStyleBook) then
    begin
      FStyleBook := nil;
      if HasPopupForm then
        TOpenForm(FPopupForm).SetStyleBookWithoutUpdate(nil);
    end;
    if (AComponent = FSaveParent) then
      FSaveParent := nil;
  end;
end;

procedure TPopup.SetIsOpen(const Value: Boolean);
begin
  if FIsOpen <> Value then
  begin
    if csDesigning in ComponentState then
    begin
      FIsOpen := False;
      Exit;
    end;
    if Value then
      Popup
    else
    begin
      if HasPopupForm then
        TCommonCustomForm(FPopupForm).Close
      else
        FIsOpen := Value;
    end;
  end;
end;

procedure TPopup.BeforeShowProc(Sender: TObject);
begin
  FIsOpen := True;
  ApplyTriggerEffect(Self, 'IsOpen');
  StartTriggerAnimation(Self, 'IsOpen');
  DoPopup;
end;

procedure TPopup.BringToFront;
begin
  if HasPopupForm then
    TCustomPopupForm(FPopupForm).BringToFront
  else
    inherited;
end;

procedure TPopup.UpdatePopupSize;
var
  LSize: TSizeF;
begin
  if FPopupForm <> nil then
  begin
    if (FPopupFormSize.cx > 0) then
      LSize.cx := FPopupFormSize.cx
    else
      LSize.cx := Width;
    if (FPopupFormSize.cy > 0) then
      LSize.cy := FPopupFormSize.cy
    else
      LSize.cy := Height;
    LSize := LSize * AbsoluteScale;
    if (FPopupForm is TCustomPopupForm) then
      TCustomPopupForm(FPopupForm).Size := LSize;
  end;
end;

function TPopup.VisibleStored: Boolean;
begin
  if ActionClient then
    Result := True
  else
    Result := False;
end;

procedure TPopup.BeforeCloseProc(Sender: TObject);
begin
  FClosingAnimation := True;
  FIsOpen := False;
  ApplyTriggerEffect(Self, 'IsOpen');
  StartTriggerAnimationWait(Self, 'IsOpen');
end;

procedure TPopup.CloseProc(Sender: TObject; var Action: TCloseAction);
begin
  ClosePopup;
end;

procedure TPopup.DoPopup;
begin
  if Assigned(FOnPopup) then
    FOnPopup(Self);
end;

procedure TPopup.SetPlacement(const Value: TPlacement);
begin
  if FPlacement <> Value then
  begin
    FPlacement := Value;
    if HasPopupForm then
      TCustomPopupForm(FPopupForm).Placement := FPlacement;
  end;
end;

procedure TPopup.SetPlacementRectangle(const Value: TBounds);
begin
  FPlacementRectangle.Assign(Value);
end;

procedure TPopup.SetPlacementTarget(const Value: TControl);
begin
  if FPlacementTarget <> Value then
  begin
    if FPlacementTarget <> nil then
      FPlacementTarget.RemoveFreeNotification(self);
    FPlacementTarget := Value;
    if HasPopupForm then
      TCustomPopupForm(FPopupForm).PlacementTarget := FPlacementTarget;
    if FPlacementTarget <> nil then
      FPlacementTarget.FreeNotification(self);
  end;
end;

procedure TPopup.SetPopupFormSize(const Value: TSizeF);
begin
  if not (FPopupFormSize = Value) then
  begin
    FPopupFormSize := Value;
    UpdatePopupSize;
  end;
end;

procedure TPopup.SetStyleBook(const Value: TStyleBook);
begin
  if FStyleBook <> Value then
  begin
    if FStyleBook <> nil then
      TComponent(FStyleBook).RemoveFreeNotification(self);
    FStyleBook := nil;
    if HasPopupForm then
      TOpenForm(FPopupForm).SetStyleBookWithoutUpdate(nil);
    FStyleBook := Value;
    if FStyleBook <> nil then
      TComponent(FStyleBook).FreeNotification(self);
    if HasPopupForm and (FStyleBook <> nil) then
      TOpenForm(FPopupForm).SetStyleBookWithoutUpdate(FStyleBook);
  end;
end;

procedure TPopup.SetAniDuration(const Value: Single);
begin
  if not SameValue(FAniDuration, Value) then
  begin
    FAniDuration := Value;
    if HasPopupForm and (not ClosingAnimation) then
      TCustomPopupForm(FPopupForm).AniDuration := FAniDuration;
  end;
end;

procedure TPopup.SetOnAniTimer(const Value: TNotifyEvent);
begin
  FOnAniTimer := Value;
  if HasPopupForm then
    TCustomPopupForm(FPopupForm).OnAniTimer := FOnAniTimer;
end;

procedure TPopup.SetBorderWidth(const Value: Single);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    if HasPopupForm then
    begin
      TCustomPopupForm(FPopupForm).BeginUpdate;
      try
        TCustomPopupForm(FPopupForm).Padding.Left := BorderWidth * Scale.X;
        TCustomPopupForm(FPopupForm).Padding.Top := BorderWidth * Scale.Y;
        TCustomPopupForm(FPopupForm).Padding.Right := BorderWidth * Scale.X;
        TCustomPopupForm(FPopupForm).Padding.Bottom := BorderWidth * Scale.Y;
      finally
        TCustomPopupForm(FPopupForm).EndUpdate;
      end;
    end;
  end;
end;

procedure TPopup.SetDragWithParent(const Value: Boolean);
begin
  if FDragWithParent <> Value then
  begin
    FDragWithParent := Value;
    if HasPopupForm then
      TCustomPopupForm(FPopupForm).DragWithParent := FDragWithParent;
  end;
end;

{ TPathAnimation }

constructor TPathAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FPath := TPathData.Create;
end;

destructor TPathAnimation.Destroy;
begin
  if FSpline <> nil then
    FreeAndNil(FSpline);
  FreeAndNil(FPath);
  inherited;
end;

function TPathAnimation.EnabledStored: Boolean;
begin
  if ActionClient then
    Result := True
  else
    Result := Enabled;
end;

procedure TPathAnimation.ProcessAnimation;

  function DeltaBetween(const Point1, Point2: TPointF): Single;
  begin
    if Point1.CrossProduct(Point2) < 0 then
      Result := ArcCos(Point1.AngleCosine(Point2))
    else
      Result := - ArcCos(Point1.AngleCosine(Point2));
  end;

var
  OldP, P1, Delta: TPointF;
begin
  if (Length(FPolygon) > 0) and (FObj <> nil) then
  begin
    OldP := FObj.Position.Point;
    FSpline.SplineXY(NormalizedTime * High(FPolygon), P1.X, P1.Y);
    FObj.Position.X := FStart.X + P1.X;
    FObj.Position.Y := FStart.Y + P1.Y;
    if FRotate and (NormalizedTime <> 0) and (NormalizedTime <> 1) and ((OldP.X <> FObj.Position.X) and
      (OldP.Y <> FObj.Position.Y)) then
    begin
      Delta := FObj.Position.Point - OldP;

      if Inverse then
        FObj.RotationAngle := 180 + RadToDeg(DeltaBetween(Delta, TPointF.Create(0, 1)))
      else
        FObj.RotationAngle := RadToDeg(DeltaBetween(Delta, TPointF.Create(0, 1)));
    end;
  end;
end;

procedure TPathAnimation.SetPath(const Value: TPathData);
begin
  FPath.Assign(Value);
end;

procedure TPathAnimation.Start;
var
  I: Integer;
begin
  if FSpline <> nil then
    FreeAndNil(FSpline);
  FPath.FlattenToPolygon(FPolygon);
  for I := Length(FPolygon) - 1 downto 0 do
    if (FPolygon[I].X = PolygonPointBreak.X) and (FPolygon[I].Y = PolygonPointBreak.Y) then
    begin
      if I < Length(FPolygon) - 1 then
        FPolygon[I] := FPolygon[I + 1];
      SetLength(FPolygon, Length(FPolygon) - 1);
    end;
  FSpline := TSpline.Create(FPolygon);
  if Parent is TControl then
    FObj := TControl(Parent)
  else
    FObj := nil;
  if FObj <> nil then
    FStart := FObj.Position.Point;
  inherited;
end;

{ TContentInflater }

constructor TContentInflater<T>.Create(const Inflatable: IInflatableContent<T>);
begin
  FInflatable := Inflatable;
  TMessageManager.DefaultManager.SubscribeToMessage(TIdleMessage, ReceiveIdleMessage);
end;

destructor TContentInflater<T>.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TIdleMessage, ReceiveIdleMessage);
  inherited;
end;

procedure TContentInflater<T>.Inflate(Total: Boolean);
var
  I: Integer;
  InflatedCount: Integer;
  Stop: TDateTime;
  Items: TList<T>;
begin
  if FBusy then
    Exit;
  Items := FInflatable.GetInflatableItems;
  if Items.Count = 0 then
    Exit;
  FBusy := True;
  try
    Stop := Now + EncodeTime(0, 0, 0, 50);
    InflatedCount := 0;
    for I := 0 to Items.Count - 1 do
    begin
      Items[I].Inflate;
      Inc(InflatedCount);
      if (not Total) and (Now > Stop) then
        Break;
    end;
    Items.DeleteRange(0, InflatedCount);
  finally
    FBusy := False;
  end;
  if Items.Count = 0 then
    FInflatable.NotifyInflated;
end;

procedure TContentInflater<T>.ReceiveIdleMessage(const Sender : TObject; const M : TMessage);
begin
  Inflate(False);
end;

procedure FreeControls;
begin
  TStyleCache.Uninitialize;
  FreeAndNil(PopupList);
end;

procedure RegisterAliases;
begin
  AddEnumElementAliases(TypeInfo(TPaintStage), ['psAll', 'psBackground', 'psText']);
  AddEnumElementAliases(TypeInfo(TOrientation), ['orHorizontal', 'orVertical']);
  AddEnumElementAliases(TypeInfo(TPlacement), ['plBottom', 'plTop', 'plLeft', 'plRight', 'plCenter', 'plBottomCenter', 'plTopCenter', 'plLeftCenter', 'plRightCenter', 'plAbsolute', 'plMouse', 'plMouseCenter']);
end;

procedure UnregisterAliases;
begin
  RemoveEnumElementAliases(TypeInfo(TPaintStage));
  RemoveEnumElementAliases(TypeInfo(TOrientation));
  RemoveEnumElementAliases(TypeInfo(TPlacement));
end;

{ TControlTypeHelper }

function TControlTypeHelper.ToString: string;
const
  Style = 'style'; // do not localize
  Native = 'native'; // do not localize
begin
  case Self of
    TControlType.Styled:
      Result := Style;
    TControlType.Platform:
      Result := Native;
  else
    Result := string.Empty;
  end;
end;

{ TStyleChangedMessage }

constructor TStyleChangedMessage.Create(const StyleBook: TStyleBook; const Scene: IScene);
begin
  inherited Create(StyleBook);
  FScene := Scene;
end;

{ THint }

class function THint.ContainsRegistredHintClasses: Boolean;
begin
  Result := Length(FClassRegistry) > 0;
end;

constructor THint.Create(const AHandle: TWindowHandle);
begin
  inherited Create;
end;

class function THint.CreateNewInstance(const AHandle: TWindowHandle): THint;
var
  I: Integer;
begin
  Result := nil;
  for I := Low(FClassRegistry) to High(FClassRegistry) do
  begin
    Result := FClassRegistry[I].Create(AHandle);
    if Result <> nil then
      Break;
  end;
end;

function THint.GetHint: string;
begin
  Result := FHint;
end;

function THint.GetLongText: string;
begin
  Result := GetLongHint(FHint);
end;

function THint.GetShortText: string;
begin
  Result := GetShortHint(FHint);
end;

class procedure THint.RegisterClass(const AClass: THintClass);
begin
  SetLength(FClassRegistry, Length(FClassRegistry) + 1);
  FClassRegistry[Length(FClassRegistry) - 1] := AClass;
end;

procedure THint.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure THint.SetHint(const AString: string);
begin
  FHint := AString;
end;

initialization
  RegisterAliases;
  RegisterShowVKProc(ShowVirtualKeyboard);
  StartClassGroup(TFmxObject);
  ActivateClassGroup(TFmxObject);
  //GroupDescendentsWith(TCustomGestureManager, TFmxObject);
  RegisterFmxClasses([TStyledControl, TContent, TPopup, TStyleContainer, TStyleBook, TPathAnimation], []);
  TStyleStreaming.SetDefaultContainerClass(TStyleContainer);
finalization
  UnregisterAliases;
end.
