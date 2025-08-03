unit Alcinoe.FMX.StdCtrls;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if FMX.StdCtrls.pas was not updated and adjust the IFDEF'}
{$ENDIF}

uses
  System.Classes,
  System.Types,
  {$IFDEF DEBUG}
  System.Diagnostics,
  {$ENDIF}
  System.UITypes,
  System.ImageList,
  System.Math,
  System.Rtti,
  System.Messaging,
  {$IF DEFINED(IOS) or DEFINED(ANDROID)}
  FMX.types3D,
  {$ENDIF}
  FMX.types,
  FMX.stdActns,
  FMX.Controls,
  FMX.Graphics,
  FMX.StdCtrls,
  FMX.actnlist,
  FMX.ImgList,
  Alcinoe.Common,
  Alcinoe.FMX.CacheEngines,
  Alcinoe.FMX.BreakText,
  Alcinoe.FMX.Ani,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.ScrollEngine,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.Objects;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALAniIndicator = class(TALControl)
  Public
    type
      // ---------
      // TMotionMode
      TMotionMode = (Frame, Rotate);
      // ----------
      // TAnimation
      TAnimation = class(TALFloatAnimation)
      private
        fOwner: TALAniIndicator;
        FEnabled: Boolean;
        procedure SetEnabled(const Value: Boolean);
        procedure repaint;
      protected
        procedure DoProcess; override;
        function GetDefaultDuration: Single; override;
        function GetDefaultLoop: Boolean; override;
        function GetDefaultStartValue: Single; override;
        function GetDefaultStopValue: Single; override;
      public
        constructor Create(const AOwner: TALAniIndicator); reintroduce; virtual;
        procedure Assign(Source: TPersistent); override;
        procedure Start; override;
      published
        property AutoReverse;
        property Delay;
        property Duration;
        property Enabled Read FEnabled write SetEnabled stored true default True;
        property Inverse;
        property Loop;
        property InterpolationType;
        property InterpolationMode;
        property InterpolationParams;
      end;
  private
    FAnimation: TAnimation; // 8 bytes
    FResourceName: String; // 8 bytes
    FTintColorKey: String; // 8 bytes
    FTintColor: TAlphaColor; // 4 bytes
    FFrameCount: Integer; // 4 bytes
    FRowCount: Integer; // 4 bytes
    FCacheIndex: Integer; // 4 bytes
    FCacheEngine: TALBufDrawableCacheEngine; // 8 bytes
    FMotionMode: TMotionMode; // 1 byte
    procedure SetTintColor(const Value: TAlphaColor);
    procedure setTintColorKey(const Value: String);
    procedure SetAnimation(const Value: TAnimation);
    procedure SetResourceName(const Value: String);
    procedure SetMotionMode(const Value: TMotionMode);
    procedure SetFrameCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    function IsTintColorStored: Boolean;
    function IsTintColorKeyStored: Boolean;
  protected
    FBufDrawable: TALDrawable;
    FBufDrawableRect: TRectF;
    procedure ApplyTintColorScheme; virtual;
    function GetCacheSubIndex: Integer; virtual;
    function GetDoubleBuffered: boolean; override;
    function GetDefaultSize: TSizeF; override;
    function GetDefaultTintColor: TAlphaColor; virtual;
    function GetDefaultTintColorKey: String; virtual;
    procedure Paint; override;
    procedure DoResized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure Assign(Source: TPersistent{TALControl}); override;
    procedure ApplyColorScheme; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    property DefaultTintColor: TAlphaColor read GetDefaultTintColor;
    property DefaultTintColorKey: String read GetDefaultTintColorKey;
    // CacheIndex and CacheEngine are primarily used in TALDynamicListBox to
    // prevent duplicate drawables across multiple identical controls.
    // CacheIndex specifies the slot in the cache engine where an existing
    // drawable can be retrieved.
    property CacheIndex: Integer read FCacheIndex write FCacheIndex;
    // CacheEngine is not owned by the current control.
    property CacheEngine: TALBufDrawableCacheEngine read FCacheEngine write FCacheEngine;
  published
    //property Action;
    property Align;
    property Animation: TAnimation read fAnimation write SetAnimation;
    property Anchors;
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClickSound;
    //property ClipChildren;
    //property ClipParent;
    property Cursor;
    //property DoubleBuffered;
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
    property ResourceName: String read FResourceName write SetResourceName;
    property MotionMode: TMotionMode read FMotionMode write SetMotionMode default TMotionMode.Rotate;
    property FrameCount: Integer read FFrameCount write SetFrameCount default 1;
    property RowCount: Integer read FRowCount write SetRowCount default 1;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Size;
    //property TabOrder;
    //property TabStop;
    property TintColor: TAlphaColor read FTintColor write SetTintColor stored IsTintColorStored;
    property TintColorKey: String read FTintColorKey write setTintColorKey Stored IsTintColorKeyStored;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    //property OnCanFocus;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnClick;
    property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBaseCheckBox = class(TALShape)
  public
    type
      // ---------------
      // TCheckMarkBrush
      TCheckMarkBrush = class(TALPersistentObserver)
      public
        Type
          TMargins = class(TALBounds)
          protected
            function GetDefaultValue: TRectF; override;
          end;
      private
        FColor: TAlphaColor;
        FColorKey: String;
        FResourceName: String;
        FWrapMode: TALImageWrapMode;
        FThickness: Single;
        FMargins: TALBounds;
        procedure SetColor(const Value: TAlphaColor);
        procedure SetColorKey(const Value: String);
        procedure SetResourceName(const Value: String);
        procedure SetWrapMode(const Value: TALImageWrapMode);
        procedure SetThickness(const Value: Single);
        procedure SetMargins(const Value: TALBounds);
        procedure MarginsChanged(Sender: TObject); virtual;
        function IsColorStored: Boolean;
        function IsColorKeyStored: Boolean;
        function IsResourceNameStored: Boolean;
        function IsWrapModeStored: Boolean;
        function IsThicknessStored: Boolean;
      protected
        function CreateMargins: TALBounds; virtual;
        function GetDefaultColor: TAlphaColor; virtual;
        function GetDefaultColorKey: String; virtual;
        function GetDefaultResourceName: String; virtual;
        function GetDefaultWrapMode: TALImageWrapMode; virtual;
        function GetDefaultThickness: Single; virtual;
      public
        constructor Create; override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; virtual;
        procedure ApplyColorScheme; virtual;
        procedure Interpolate(const ATo: TCheckMarkBrush; const ANormalizedTime: Single; const AReverse: Boolean); virtual;
        procedure InterpolateNoChanges(const ATo: TCheckMarkBrush; const ANormalizedTime: Single; const AReverse: Boolean);
        function HasCheckMark: boolean;
        property DefaultColor: TAlphaColor read GetDefaultColor;
        property DefaultColorKey: String read GetDefaultColorKey;
        property DefaultResourceName: String read GetDefaultResourceName;
        property DefaultWrapMode: TALImageWrapMode read GetDefaultWrapMode;
        property DefaultThickness: Single read GetDefaultThickness;
      published
        property Color: TAlphaColor read FColor write SetColor stored IsColorStored;
        property ColorKey: String read FColorKey write SetColorKey stored IsColorKeyStored;
        property ResourceName: String read FResourceName write SetResourceName stored IsResourceNameStored nodefault;
        property WrapMode: TALImageWrapMode read FWrapMode write SetWrapMode stored IsWrapModeStored;
        property Thickness: Single read FThickness write SetThickness stored IsThicknessStored nodefault;
        property Margins: TALBounds read FMargins write SetMargins;
      end;
      // ----------------------
      // TInheritCheckMarkBrush
      TInheritCheckMarkBrush = class(TCheckMarkBrush)
      private
        FParent: TCheckMarkBrush;
        FInherit: Boolean;
        fSuperseded: Boolean;
        procedure SetInherit(const AValue: Boolean);
      protected
        function CreateSavedState: TALPersistentObserver; override;
        procedure DoSupersede; virtual;
      public
        constructor Create(const AParent: TCheckMarkBrush); reintroduce; virtual;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure Supersede(Const ASaveState: Boolean = False); virtual;
        procedure SupersedeNoChanges(Const ASaveState: Boolean = False);
        property Superseded: Boolean read FSuperseded;
        property Parent: TCheckMarkBrush read FParent;
      published
        property Inherit: Boolean read FInherit write SetInherit Default True;
      end;
      // ---------------
      // TBaseStateStyle
      TBaseStateStyle = class(TALBaseStateStyle)
      public
        type
          TStateLayer = class(TALStateLayer)
          public
            Type
              TMargins = class(TALBounds)
              protected
                function GetDefaultValue: TRectF; override;
              end;
          protected
            function CreateMargins: TALBounds; override;
            function GetDefaultXRadius: Single; override;
            function GetDefaultYRadius: Single; override;
          end;
      private
        FCheckMark: TInheritCheckMarkBrush;
        function GetStateStyleParent: TBaseStateStyle;
        function GetControlParent: TALBaseCheckBox;
        procedure SetCheckMark(const AValue: TInheritCheckMarkBrush);
        procedure CheckMarkChanged(ASender: TObject);
      protected
        function CreateStateLayer: TALStateLayer; override;
        function CreateCheckMark(const AParent: TCheckMarkBrush): TInheritCheckMarkBrush; virtual;
        function GetInherit: Boolean; override;
        procedure DoSupersede; override;
      public
        constructor Create(const AParent: TObject); override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; override;
        procedure ApplyColorScheme; override;
        procedure Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single; const AReverse: Boolean); override;
        property StateStyleParent: TBaseStateStyle read GetStateStyleParent;
        property ControlParent: TALBaseCheckBox read GetControlParent;
      published
        property CheckMark: TInheritCheckMarkBrush read FCheckMark write SetCheckMark;
        property Fill;
        property Shadow;
        property Stroke;
      end;
      // ------------------
      // TDefaultStateStyle
      TDefaultStateStyle = class(TBaseStateStyle)
      protected
        function GetCacheSubIndex: Integer; override;
      end;
      // -------------------
      // TDisabledStateStyle
      TDisabledStateStyle = class(TBaseStateStyle)
      private
        FOpacity: Single;
        procedure SetOpacity(const Value: Single);
        function IsOpacityStored: Boolean;
      protected
        function GetInherit: Boolean; override;
        function GetCacheSubIndex: Integer; override;
      public
        constructor Create(const AParent: TObject); override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
      published
        property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
      end;
      // ------------------
      // THoveredStateStyle
      THoveredStateStyle = class(TBaseStateStyle)
      protected
        function GetCacheSubIndex: Integer; override;
      published
        property StateLayer;
        property Scale;
      end;
      // ------------------
      // TPressedStateStyle
      TPressedStateStyle = class(TBaseStateStyle)
      protected
        function GetCacheSubIndex: Integer; override;
      published
        property StateLayer;
        property Scale;
      end;
      // ------------------
      // TFocusedStateStyle
      TFocusedStateStyle = class(TBaseStateStyle)
      protected
        function GetCacheSubIndex: Integer; override;
      published
        property StateLayer;
        property Scale;
      end;
      // -----------------
      // TCheckStateStyles
      TCheckStateStyles = class(TALPersistentObserver)
      private
        FDefault: TDefaultStateStyle;
        FDisabled: TDisabledStateStyle;
        FHovered: THoveredStateStyle;
        FPressed: TPressedStateStyle;
        FFocused: TFocusedStateStyle;
        procedure SetDefault(const AValue: TDefaultStateStyle);
        procedure SetDisabled(const AValue: TDisabledStateStyle);
        procedure SetHovered(const AValue: THoveredStateStyle);
        procedure SetPressed(const AValue: TPressedStateStyle);
        procedure SetFocused(const AValue: TFocusedStateStyle);
        procedure DefaultChanged(ASender: TObject);
        procedure DisabledChanged(ASender: TObject);
        procedure HoveredChanged(ASender: TObject);
        procedure PressedChanged(ASender: TObject);
        procedure FocusedChanged(ASender: TObject);
      protected
        function CreateSavedState: TALPersistentObserver; override;
        function CreateDefaultStateStyle(const AParent: TObject): TDefaultStateStyle; virtual;
        function CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle; virtual;
        function CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle; virtual;
        function CreatePressedStateStyle(const AParent: TObject): TPressedStateStyle; virtual;
        function CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle; virtual;
      public
        constructor Create(const AParent: TALControl); reintroduce; virtual;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; virtual;
        procedure ApplyColorScheme; virtual;
        procedure ClearBufDrawable; virtual;
      published
        property &Default: TDefaultStateStyle read FDefault write SetDefault;
        property Disabled: TDisabledStateStyle read FDisabled write SetDisabled;
        property Hovered: THoveredStateStyle read FHovered write SetHovered;
        property Pressed: TPressedStateStyle read FPressed write SetPressed;
        property Focused: TFocusedStateStyle read FFocused write SetFocused;
      end;
      // ------------
      // TStateStyles
      TStateStyles = class(TALBaseStateStyles)
      public
        type
          // -----------
          // TTransition
          TTransition = class(TALBaseStateStyles.TTransition)
          published
            property FadeImage;
          end;
      private
        FChecked: TCheckStateStyles;
        FUnchecked: TCheckStateStyles;
        function GetParent: TALBaseCheckBox;
        function GetTransition: TStateStyles.TTransition;
        procedure SetTransition(const AValue: TStateStyles.TTransition);
        procedure SetChecked(const AValue: TCheckStateStyles);
        procedure SetUnchecked(const AValue: TCheckStateStyles);
        procedure CheckedChanged(ASender: TObject);
        procedure UncheckedChanged(ASender: TObject);
      protected
        function CreateTransition: TALBaseStateStyles.TTransition; override;
        function CreateCheckedStateStyles(const AParent: TALControl): TCheckStateStyles; virtual;
        function CreateUncheckedStateStyles(const AParent: TALControl): TCheckStateStyles; virtual;
      public
        constructor Create(const AParent: TALControl); override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; override;
        procedure ApplyColorScheme; override;
        procedure ClearBufDrawable; override;
        function GetCurrentRawStyle: TALBaseStateStyle; override;
        Property Parent: TALBaseCheckBox read GetParent;
      published
        property Checked: TCheckStateStyles read FChecked write SetChecked;
        property Unchecked: TCheckStateStyles read FUnchecked write SetUnchecked;
        property Transition: TStateStyles.TTransition read GetTransition write SetTransition;
      end;
  private
    FStateStyles: TStateStyles;
    FCheckMark: TCheckMarkBrush;
    FChecked: Boolean;
    FDoubleBuffered: boolean;
    FXRadius: Single;
    FYRadius: Single;
    FCacheIndex: Integer; // 4 bytes
    FCacheEngine: TALBufDrawableCacheEngine; // 8 bytes
    {$IF NOT DEFINED(ALSkiaCanvas)}
    FRenderTargetSurface: TALSurface; // 8 bytes
    FRenderTargetCanvas: TALCanvas; // 8 bytes
    fRenderTargetDrawable: TALDrawable; // 8 bytes
    {$ENDIF}
    FOnChange: TNotifyEvent;
    procedure SetCheckMark(const Value: TCheckMarkBrush);
    procedure SetStateStyles(const AValue: TStateStyles);
    function IsXRadiusStored: Boolean;
    function IsYRadiusStored: Boolean;
  protected
    function CreateCheckMark: TCheckMarkBrush; virtual;
    function CreateStateStyles: TStateStyles; virtual;
    function GetCacheSubIndex: Integer; virtual;
    function GetDoubleBuffered: boolean; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    function GetDefaultXRadius: Single; virtual;
    function GetDefaultYRadius: Single; virtual;
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;
    procedure CheckMarkChanged(Sender: TObject); virtual;
    procedure StateStylesChanged(Sender: TObject); virtual;
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); override;
    procedure IsMouseOverChanged; override;
    procedure IsFocusedChanged; override;
    procedure PressedChanged; override;
    function GetDefaultSize: TSizeF; override;
    function GetChecked: Boolean; virtual;
    procedure SetChecked(const Value: Boolean); virtual;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure DoClickSound; override;
    procedure Click; override;
    procedure DoChanged; virtual;
    procedure DoResized; override;
    procedure DrawCheckMark(
                const ACanvas: TALCanvas;
                const AScale: Single;
                const ADstRect: TrectF;
                const AOpacity: Single;
                const AChecked: Boolean;
                const ACheckMark: TCheckMarkBrush); virtual;
    Procedure CreateBufDrawable(
                var ABufDrawable: TALDrawable;
                out ABufDrawableRect: TRectF;
                const AScale: Single;
                const AFill: TALBrush;
                const AStateLayer: TALStateLayer;
                const AStroke: TALStrokeBrush;
                const ACheckMark: TCheckMarkBrush;
                const AShadow: TALShadow); virtual;
    {$IF NOT DEFINED(ALSkiaCanvas)}
    function GetRenderTargetRect(const ARect: TrectF): TRectF; virtual;
    procedure InitRenderTargets(var ARect: TrectF); virtual;
    procedure ClearRenderTargets; virtual;
    Property RenderTargetSurface: TALSurface read FRenderTargetSurface;
    Property RenderTargetCanvas: TALCanvas read FRenderTargetCanvas;
    Property RenderTargetDrawable: TALDrawable read fRenderTargetDrawable;
    {$ENDIF}
    procedure Paint; override;
    // CacheIndex and CacheEngine are primarily used in TALDynamicListBox to
    // prevent duplicate drawables across multiple identical controls.
    // CacheIndex specifies the slot in the cache engine where an existing
    // drawable can be retrieved.
    property CacheIndex: Integer read FCacheIndex write FCacheIndex;
    // CacheEngine is not owned by the current control.
    property CacheEngine: TALBufDrawableCacheEngine read FCacheEngine write FCacheEngine;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent{TALControl}); override;
    procedure AlignToPixel; override;
    procedure ApplyColorScheme; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    property CanFocus default True;
    property Cursor default crHandPoint;
    property Checked: Boolean read GetChecked write SetChecked default False;
    property CheckMark: TCheckMarkBrush read FCheckMark write SetCheckMark;
    property DoubleBuffered default true;
    property StateStyles: TStateStyles read FStateStyles write SetStateStyles;
    property XRadius: Single read FXRadius write SetXRadius stored IsXRadiusStored nodefault;
    property YRadius: Single read FYRadius write SetYRadius stored IsYRadiusStored nodefault;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property DefaultXRadius: Single read GetDefaultXRadius;
    property DefaultYRadius: Single read GetDefaultYRadius;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALCheckBox = class(TALBaseCheckBox)
  public
    type
      // ------------
      // TStateStyles
      TStateStyles = class(TALBaseCheckBox.TStateStyles)
      published
        property Transition;
      end;
  private
    function GetStateStyles: TStateStyles;
    procedure SetStateStyles(const AValue: TStateStyles);
  protected
    function CreateStateStyles: TALBaseCheckBox.TStateStyles; override;
  public
    property CacheEngine;
    property CacheIndex;
  published
    //property Action;
    property Align;
    property Anchors;
    //property AutoSize;
    property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property CheckMark;
    property Checked;
    property ClickSound;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DoubleBuffered;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Fill;
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
    property Shadow;
    property Size;
    property StateStyles: TStateStyles read GetStateStyles write SetStateStyles;
    property Stroke;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property XRadius;
    property YRadius;
    property OnCanFocus;
    property OnChange;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALRadioButton = class(TALCheckBox)
  public
    type
      // ---------------
      // TCheckMarkBrush
      TCheckMarkBrush = class(TALCheckBox.TCheckMarkBrush)
      public
        Type
          TMargins = class(TALCheckBox.TCheckMarkBrush.TMargins)
          protected
            function GetDefaultValue: TRectF; override;
          end;
      protected
        function CreateMargins: TALBounds; override;
      end;
      // ----------------------
      // TInheritCheckMarkBrush
      TInheritCheckMarkBrush = class(TALCheckBox.TInheritCheckMarkBrush)
      public
        Type
          TMargins = class(TALCheckBox.TInheritCheckMarkBrush.TMargins)
          protected
            function GetDefaultValue: TRectF; override;
          end;
      protected
        function CreateMargins: TALBounds; override;
      end;
      // ------------------
      // TDefaultStateStyle
      TDefaultStateStyle = class(TALCheckBox.TDefaultStateStyle)
      protected
        function CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush; override;
      end;
      // -------------------
      // TDisabledStateStyle
      TDisabledStateStyle = class(TALCheckBox.TDisabledStateStyle)
      protected
        function CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush; override;
      end;
      // ------------------
      // THoveredStateStyle
      THoveredStateStyle = class(TALCheckBox.THoveredStateStyle)
      protected
        function CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush; override;
      end;
      // ------------------
      // TPressedStateStyle
      TPressedStateStyle = class(TALCheckBox.TPressedStateStyle)
      protected
        function CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush; override;
      end;
      // ------------------
      // TFocusedStateStyle
      TFocusedStateStyle = class(TALCheckBox.TFocusedStateStyle)
      protected
        function CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush; override;
      end;
      // -----------------
      // TCheckStateStyles
      TCheckStateStyles = class(TALCheckBox.TCheckStateStyles)
      protected
        function CreateDefaultStateStyle(const AParent: TObject): TALBaseCheckBox.TDefaultStateStyle; override;
        function CreateDisabledStateStyle(const AParent: TObject): TALBaseCheckBox.TDisabledStateStyle; override;
        function CreateHoveredStateStyle(const AParent: TObject): TALBaseCheckBox.THoveredStateStyle; override;
        function CreatePressedStateStyle(const AParent: TObject): TALBaseCheckBox.TPressedStateStyle; override;
        function CreateFocusedStateStyle(const AParent: TObject): TALBaseCheckBox.TFocusedStateStyle; override;
      end;
      // ------------
      // TStateStyles
      TStateStyles = class(TALCheckBox.TStateStyles)
      protected
        function CreateCheckedStateStyles(const AParent: TALControl): TALBaseCheckBox.TCheckStateStyles; override;
        function CreateUncheckedStateStyles(const AParent: TALControl): TALBaseCheckBox.TCheckStateStyles; override;
      end;
  private
    FGroupName: string;
    fMandatory: boolean;
    function GetGroupName: string;
    procedure SetGroupName(const Value: string);
    function GroupNameStored: Boolean;
    procedure GroupMessageCall(const Sender : TObject; const M : TMessage);
  protected
    function CreateCheckMark: TALBaseCheckBox.TCheckMarkBrush; override;
    function CreateStateStyles: TALBaseCheckBox.TStateStyles; override;
    procedure SetChecked(const Value: Boolean); override;
    function GetDefaultXRadius: Single; override;
    function GetDefaultYRadius: Single; override;
    function GetDefaultSize: TSizeF; override;
    procedure DrawCheckMark(
                const ACanvas: TALCanvas;
                const AScale: Single;
                const ADstRect: TrectF;
                const AOpacity: Single;
                const AChecked: Boolean;
                const ACheckMark: TALBaseCheckBox.TCheckMarkBrush); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BeforeDestruction; override;
    procedure Assign(Source: TPersistent{TALControl}); override;
  published
    property GroupName: string read GetGroupName write SetGroupName stored GroupNameStored nodefault;
    property Mandatory: Boolean read fMandatory write fMandatory default false;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALSwitch = class(TALControl)
  public
    type
      // ------
      // TTrack
      TTrack = class(TALShape)
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
          // ---------------
          // TBaseStateStyle
          TBaseStateStyle = class(TALBaseStateStyle)
          public
            type
              TFill = class(TALInheritBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStroke = class(TALInheritStrokeBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStateLayer = class(TALStateLayer)
              protected
                function GetDefaultXRadius: Single; override;
                function GetDefaultYRadius: Single; override;
              end;
          protected
            function CreateFill(const AParent: TALBrush): TALInheritBrush; override;
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateStateLayer: TALStateLayer; override;
          published
            property Fill;
            property Shadow;
            property Stroke;
          end;
          // ------------------
          // TDefaultStateStyle
          TDefaultStateStyle = class(TBaseStateStyle)
          protected
            function GetCacheSubIndex: Integer; override;
          end;
          // -------------------
          // TDisabledStateStyle
          TDisabledStateStyle = class(TBaseStateStyle)
          private
            FOpacity: Single;
            procedure SetOpacity(const Value: Single);
            function IsOpacityStored: Boolean;
          protected
            function GetInherit: Boolean; override;
            function GetCacheSubIndex: Integer; override;
          public
            constructor Create(const AParent: TObject); override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
          published
            property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
          end;
          // ------------------
          // THoveredStateStyle
          THoveredStateStyle = class(TBaseStateStyle)
          protected
            function GetCacheSubIndex: Integer; override;
          published
            property StateLayer;
            // When the track is scaled, the thumb is no longer aligned with the track.
            // Therefore, currently, scaling of the track is disabled.
            //property Scale;
          end;
          // ------------------
          // TPressedStateStyle
          TPressedStateStyle = class(TBaseStateStyle)
          protected
            function GetCacheSubIndex: Integer; override;
          published
            property StateLayer;
            // When the track is scaled, the thumb is no longer aligned with the track.
            // Therefore, currently, scaling of the track is disabled.
            //property Scale;
          end;
          // ------------------
          // TFocusedStateStyle
          TFocusedStateStyle = class(TBaseStateStyle)
          protected
            function GetCacheSubIndex: Integer; override;
          published
            property StateLayer;
            // When the track is scaled, the thumb is no longer aligned with the track.
            // Therefore, currently, scaling of the track is disabled.
            //property Scale;
          end;
          // -----------------
          // TCheckStateStyles
          TCheckStateStyles = class(TALPersistentObserver)
          private
            FDefault: TDefaultStateStyle;
            FDisabled: TDisabledStateStyle;
            FHovered: THoveredStateStyle;
            FPressed: TPressedStateStyle;
            FFocused: TFocusedStateStyle;
            procedure SetDefault(const AValue: TDefaultStateStyle);
            procedure SetDisabled(const AValue: TDisabledStateStyle);
            procedure SetHovered(const AValue: THoveredStateStyle);
            procedure SetPressed(const AValue: TPressedStateStyle);
            procedure SetFocused(const AValue: TFocusedStateStyle);
            procedure DefaultChanged(ASender: TObject);
            procedure DisabledChanged(ASender: TObject);
            procedure HoveredChanged(ASender: TObject);
            procedure PressedChanged(ASender: TObject);
            procedure FocusedChanged(ASender: TObject);
          protected
            function CreateSavedState: TALPersistentObserver; override;
            function CreateDefaultStateStyle(const AParent: TObject): TDefaultStateStyle; virtual;
            function CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle; virtual;
            function CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle; virtual;
            function CreatePressedStateStyle(const AParent: TObject): TPressedStateStyle; virtual;
            function CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle; virtual;
          public
            constructor Create(const AParent: TALControl); reintroduce; virtual;
            destructor Destroy; override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure AlignToPixel; virtual;
            procedure ApplyColorScheme; virtual;
            procedure ClearBufDrawable; virtual;
          published
            property &Default: TDefaultStateStyle read FDefault write SetDefault;
            property Disabled: TDisabledStateStyle read FDisabled write SetDisabled;
            property Hovered: THoveredStateStyle read FHovered write SetHovered;
            property Pressed: TPressedStateStyle read FPressed write SetPressed;
            property Focused: TFocusedStateStyle read FFocused write SetFocused;
          end;
          // ------------
          // TStateStyles
          TStateStyles = class(TALBaseStateStyles)
          private
            FChecked: TCheckStateStyles;
            FUnchecked: TCheckStateStyles;
            function GetParent: TTrack;
            procedure SetChecked(const AValue: TCheckStateStyles);
            procedure SetUnchecked(const AValue: TCheckStateStyles);
            procedure CheckedChanged(ASender: TObject);
            procedure UncheckedChanged(ASender: TObject);
          protected
            function CreateCheckedStateStyles(const AParent: TALControl): TCheckStateStyles; virtual;
            function CreateUncheckedStateStyles(const AParent: TALControl): TCheckStateStyles; virtual;
          public
            constructor Create(const AParent: TALControl); override;
            destructor Destroy; override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure AlignToPixel; override;
            procedure ApplyColorScheme; override;
            procedure ClearBufDrawable; override;
            function GetCurrentRawStyle: TALBaseStateStyle; override;
            Property Parent: TTrack read GetParent;
          published
            property Checked: TCheckStateStyles read FChecked write SetChecked;
            property Unchecked: TCheckStateStyles read FUnchecked write SetUnchecked;
          end;
      private
        FStateStyles: TStateStyles;
        FChecked: Boolean;
        FDoubleBuffered: boolean;
        FXRadius: Single;
        FYRadius: Single;
        FCacheIndex: Integer; // 4 bytes
        FCacheEngine: TALBufDrawableCacheEngine; // 8 bytes
        {$IF NOT DEFINED(ALSkiaCanvas)}
        FRenderTargetSurface: TALSurface; // 8 bytes
        FRenderTargetCanvas: TALCanvas; // 8 bytes
        fRenderTargetDrawable: TALDrawable; // 8 bytes
        {$ENDIF}
        procedure SetStateStyles(const AValue: TStateStyles);
        function IsXRadiusStored: Boolean;
        function IsYRadiusStored: Boolean;
      protected
        function CreateFill: TALBrush; override;
        function CreateStroke: TALStrokeBrush; override;
        function CreateStateStyles: TStateStyles; virtual;
        function GetDefaultSize: TSizeF; override;
        function GetCacheSubIndex: Integer; virtual;
        function GetDoubleBuffered: boolean; override;
        procedure SetDoubleBuffered(const AValue: Boolean); override;
        function GetDefaultXRadius: Single; virtual;
        function GetDefaultYRadius: Single; virtual;
        procedure SetXRadius(const Value: Single); virtual;
        procedure SetYRadius(const Value: Single); virtual;
        procedure StateStylesChanged(Sender: TObject); virtual;
        procedure FillChanged(Sender: TObject); override;
        procedure StrokeChanged(Sender: TObject); override;
        procedure ShadowChanged(Sender: TObject); override;
        procedure IsMouseOverChanged; override;
        procedure IsFocusedChanged; override;
        procedure PressedChanged; override;
        function GetChecked: Boolean; virtual;
        procedure SetChecked(const Value: Boolean); virtual;
        procedure DoChanged; virtual;
        procedure DoResized; override;
        Procedure CreateBufDrawable(
                    var ABufDrawable: TALDrawable;
                    out ABufDrawableRect: TRectF;
                    const AScale: Single;
                    const AFill: TALBrush;
                    const AStateLayer: TALStateLayer;
                    const AStroke: TALStrokeBrush;
                    const AShadow: TALShadow); virtual;
        {$IF NOT DEFINED(ALSkiaCanvas)}
        function GetRenderTargetRect(const ARect: TrectF): TRectF; virtual;
        procedure InitRenderTargets(var ARect: TrectF); virtual;
        procedure ClearRenderTargets; virtual;
        Property RenderTargetSurface: TALSurface read FRenderTargetSurface;
        Property RenderTargetCanvas: TALCanvas read FRenderTargetCanvas;
        Property RenderTargetDrawable: TALDrawable read fRenderTargetDrawable;
        {$ENDIF}
        procedure Paint; override;
        property Checked: Boolean read GetChecked write SetChecked default False;
        property CacheIndex: Integer read FCacheIndex write FCacheIndex;
        property CacheEngine: TALBufDrawableCacheEngine read FCacheEngine write FCacheEngine;
      public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure AlignToPixel; override;
        procedure ApplyColorScheme; override;
        procedure MakeBufDrawable; override;
        procedure ClearBufDrawable; override;
        property DefaultXRadius: Single read GetDefaultXRadius;
        property DefaultYRadius: Single read GetDefaultYRadius;
        property DoubleBuffered default true;
        property Position stored false;
      published
        //property Action;
        //property Align;
        //property Anchors;
        //property AutoSize;
        //property CanFocus default False;
        //property CanParentFocus;
        //property DisableFocusEffect;
        property ClickSound;
        //property ClipChildren;
        //property ClipParent;
        //property Cursor;
        //property DoubleBuffered;
        //property DragMode;
        //property EnableDragHighlight;
        //property Enabled;
        property Fill;
        //property Height;
        //property Hint;
        //property ParentShowHint;
        //property ShowHint;
        //property HitTest default False;
        //property Locked default True;
        property Margins;
        property Opacity;
        property Padding;
        //property PopupMenu;
        //property Position;
        //property RotationAngle;
        //property RotationCenter;
        //property Pivot;
        //property Scale;
        property Shadow;
        //property Size;
        property StateStyles: TStateStyles read FStateStyles write SetStateStyles;
        property Stroke;
        //property TabOrder;
        //property TabStop;
        //property TouchTargetExpansion;
        //property Visible;
        //property Width;
        property XRadius: Single read FXRadius write SetXRadius stored IsXRadiusStored nodefault;
        property YRadius: Single read FYRadius write SetYRadius stored IsYRadiusStored nodefault;
        //property OnCanFocus;
        //property OnDragEnter;
        //property OnDragLeave;
        //property OnDragOver;
        //property OnDragDrop;
        //property OnDragEnd;
        //property OnEnter;
        //property OnExit;
        //property OnMouseEnter;
        //property OnMouseLeave;
        //property OnMouseDown;
        //property OnMouseUp;
        //property OnMouseMove;
        //property OnMouseWheel;
        //property OnClick;
        //property OnDblClick;
        //property OnKeyDown;
        //property OnKeyUp;
        property OnPainting;
        property OnPaint;
        //property OnResize;
        //property OnResized;
      end;
      // ------
      // TThumb
      TThumb = class(TALBaseCheckBox)
      public
        type
          // -------
          // TStroke
          TStroke = class(TALStrokeBrush)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          // --------------------
          // TCheckMarkBrush
          TCheckMarkBrush = class(TALBaseCheckBox.TCheckMarkBrush)
          public
            Type
              TMargins = class(TALBaseCheckBox.TCheckMarkBrush.TMargins)
              protected
                function GetDefaultValue: TRectF; override;
              end;
          protected
            function CreateMargins: TALBounds; override;
          end;
          // ----------------------
          // TInheritCheckMarkBrush
          TInheritCheckMarkBrush = class(TALBaseCheckBox.TInheritCheckMarkBrush)
          public
            Type
              TMargins = class(TALBaseCheckBox.TInheritCheckMarkBrush.TMargins)
              protected
                function GetDefaultValue: TRectF; override;
              end;
          protected
            function CreateMargins: TALBounds; override;
          end;
          // ------------------
          // TDefaultStateStyle
          TDefaultStateStyle = class(TALBaseCheckBox.TDefaultStateStyle)
          public
            type
              TStroke = class(TALInheritStrokeBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
          protected
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush; override;
          end;
          // -------------------
          // TDisabledStateStyle
          TDisabledStateStyle = class(TALBaseCheckBox.TDisabledStateStyle)
          public
            type
              TStroke = class(TALInheritStrokeBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
          protected
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush; override;
          end;
          // ------------------
          // THoveredStateStyle
          THoveredStateStyle = class(TALBaseCheckBox.THoveredStateStyle)
          public
            type
              TStroke = class(TALInheritStrokeBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
          protected
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush; override;
          end;
          // ------------------
          // TPressedStateStyle
          TPressedStateStyle = class(TALBaseCheckBox.TPressedStateStyle)
          public
            type
              TStroke = class(TALInheritStrokeBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
          protected
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush; override;
          end;
          // ------------------
          // TFocusedStateStyle
          TFocusedStateStyle = class(TALBaseCheckBox.TFocusedStateStyle)
          public
            type
              TStroke = class(TALInheritStrokeBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
          protected
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush; override;
          end;
          // -----------------
          // TCheckStateStyles
          TCheckStateStyles = class(TALBaseCheckBox.TCheckStateStyles)
          protected
            function CreateDefaultStateStyle(const AParent: TObject): TALBaseCheckBox.TDefaultStateStyle; override;
            function CreateDisabledStateStyle(const AParent: TObject): TALBaseCheckBox.TDisabledStateStyle; override;
            function CreateHoveredStateStyle(const AParent: TObject): TALBaseCheckBox.THoveredStateStyle; override;
            function CreatePressedStateStyle(const AParent: TObject): TALBaseCheckBox.TPressedStateStyle; override;
            function CreateFocusedStateStyle(const AParent: TObject): TALBaseCheckBox.TFocusedStateStyle; override;
          end;
          // ------------
          // TStateStyles
          TStateStyles = class(TALBaseCheckBox.TStateStyles)
          public
            type
              // -----------
              // TTransition
              TTransition = class(TALBaseStateStyles.TTransition)
              private
                FStartPositionX: Single;
              protected
                procedure DoProcess; override;
                procedure DoFinish; override;
              public
                procedure Start; override;
              end;
          protected
            function CreateTransition: TALBaseStateStyles.TTransition; override;
            function CreateCheckedStateStyles(const AParent: TALControl): TALBaseCheckBox.TCheckStateStyles; override;
            function CreateUncheckedStateStyles(const AParent: TALControl): TALBaseCheckBox.TCheckStateStyles; override;
          end;
      protected
        function CreateStroke: TALStrokeBrush; override;
        function CreateCheckMark: TALBaseCheckBox.TCheckMarkBrush; override;
        function CreateStateStyles: TALBaseCheckBox.TStateStyles; override;
        function GetDefaultXRadius: Single; override;
        function GetDefaultYRadius: Single; override;
        function GetDefaultSize: TSizeF; override;
        procedure Click; override;
      public
        constructor Create(AOwner: TComponent); override;
        property Position stored false;
      published
        //property Action;
        //property Align;
        //property Anchors;
        //property AutoSize;
        //property CanFocus default False;
        //property CanParentFocus;
        //property DisableFocusEffect;
        property CheckMark;
        //property Checked;
        property ClickSound;
        //property ClipChildren;
        //property ClipParent;
        //property Cursor;
        //property DoubleBuffered;
        //property DragMode;
        //property EnableDragHighlight;
        //property Enabled;
        property Fill;
        //property Height;
        //property Hint;
        //property ParentShowHint;
        //property ShowHint;
        //property HitTest default False;
        //property Locked default True;
        property Margins;
        property Opacity;
        property Padding;
        //property PopupMenu;
        //property Position;
        //property RotationAngle;
        //property RotationCenter;
        //property Pivot;
        //property Scale;
        property Shadow;
        property Size;
        property StateStyles;
        property Stroke;
        //property TabOrder;
        //property TabStop;
        //property TouchTargetExpansion;
        //property Visible;
        property Width;
        property XRadius;
        property YRadius;
        //property OnCanFocus;
        //property OnChange;
        //property OnDragEnter;
        //property OnDragLeave;
        //property OnDragOver;
        //property OnDragDrop;
        //property OnDragEnd;
        //property OnEnter;
        //property OnExit;
        //property OnMouseEnter;
        //property OnMouseLeave;
        //property OnMouseDown;
        //property OnMouseUp;
        //property OnMouseMove;
        //property OnMouseWheel;
        //property OnClick;
        //property OnDblClick;
        //property OnKeyDown;
        //property OnKeyUp;
        property OnPainting;
        property OnPaint;
        //property OnResize;
        //property OnResized;
      end;
      // -----------
      // TTransition
      TTransition = class(TPersistent)
      public
        type
          TInterpolationParams = class(TPersistent)
          private
            FOwner: TTransition;
            function GetBezierX1: Single;
            function GetBezierY1: Single;
            function GetBezierX2: Single;
            function GetBezierY2: Single;
            procedure SetBezierX1(const AValue: Single);
            procedure SetBezierY1(const AValue: Single);
            procedure SetBezierX2(const AValue: Single);
            procedure SetBezierY2(const AValue: Single);
            function GetOvershoot: Single;
            procedure SetOvershoot(const AValue: Single);
          public
            constructor Create(Const AOwner: TTransition); reintroduce; virtual;
          published
            property BezierX1: Single read GetBezierX1 write SetBezierX1;
            property BezierY1: Single read GetBezierY1 write SetBezierY1;
            property BezierX2: Single read GetBezierX2 write SetBezierX2;
            property BezierY2: Single read GetBezierY2 write SetBezierY2;
            property Overshoot: Single read GetOvershoot write SetOvershoot;
          end;
      private
        FOwner: TALSwitch;
        FInterpolationParams: TInterpolationParams;
        function GetDuration: Single;
        procedure SetDuration(const AValue: Single);
        function GetDelayClick: Boolean;
        procedure SetDelayClick(const AValue: Boolean);
        function GetInterpolationType: TALInterpolationType;
        procedure SetInterpolationType(const AValue: TALInterpolationType);
        function GetInterpolationMode: TALInterpolationMode;
        procedure SetInterpolationMode(const AValue: TALInterpolationMode);
        procedure SetInterpolationParams(const AValue: TInterpolationParams);
        Function IsDurationStored: Boolean;
        Function IsDelayClickStored: Boolean;
        Function IsInterpolationTypeStored: Boolean;
        Function IsInterpolationModeStored: Boolean;
      public
        constructor Create(Const AOwner: TALSwitch); reintroduce; virtual;
        destructor Destroy; override;
        procedure Start;
      published
        property Duration: Single read GetDuration write SetDuration stored IsDurationStored nodefault;
        property InterpolationType: TALInterpolationType read GetInterpolationType write SetInterpolationType stored IsInterpolationTypeStored;
        property InterpolationMode: TALInterpolationMode read GetInterpolationMode write SetInterpolationMode stored IsInterpolationModeStored;
        property InterpolationParams: TInterpolationParams read FInterpolationParams write SetInterpolationParams;
        property DelayClick: Boolean read GetDelayClick write SetDelayClick stored IsDelayClickStored;
      end;
  private
    FThumb: TThumb;
    FTrack: TTrack;
    FTransition: TTransition;
    FPressedThumbPos: TPointF;
    FOnChange: TNotifyEvent;
    fScrollCapturedByMe: boolean;
    procedure ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
    procedure SetTransition(const Value: TTransition);
    function GetCacheIndex: integer;
    procedure SetCacheIndex(const AValue: Integer);
    function GetCacheEngine: TALBufDrawableCacheEngine;
    procedure SetCacheEngine(const AValue: TALBufDrawableCacheEngine);
    function GetMinThumbPos: Single;
    function GetMaxThumbPos: Single;
    procedure AlignThumb;
  protected
    function CreateTrack: TTrack; virtual;
    function CreateThumb: TThumb; virtual;
    function GetDefaultSize: TSizeF; override;
    function GetDoubleBuffered: boolean; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    procedure IsMouseOverChanged; override;
    procedure IsFocusedChanged; override;
    procedure PressedChanged; override;
    procedure EnabledChanged; override;
    procedure DoChange;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    procedure DoClickSound; override;
    procedure Click; override;
    function GetChecked: boolean; virtual;
    procedure SetChecked(const Value: Boolean); virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure AfterConstruction; override;
    procedure AlignToPixel; override;
    procedure ApplyColorScheme; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    // CacheIndex and CacheEngine are primarily used in TALDynamicListBox to
    // prevent duplicate drawables across multiple identical controls.
    // CacheIndex specifies the slot in the cache engine where an existing
    // drawable can be retrieved.
    property CacheIndex: Integer read GetCacheIndex write SetCacheIndex;
    // CacheEngine is not owned by the current control.
    property CacheEngine: TALBufDrawableCacheEngine read GetCacheEngine write SetCacheEngine;
  published
    //property Action;
    property Align;
    property Anchors;
    //property AutoSize;
    property CanFocus default true;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property DoubleBuffered default true;
    property Checked: Boolean read GetChecked write SetChecked default false;
    property ClickSound;
    property ClipChildren;
    //property ClipParent;
    property Cursor default crHandPoint;
    //property DoubleBuffered;
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
    property TabOrder;
    property TabStop;
    property Thumb: TThumb read FThumb;
    property TouchTargetExpansion;
    property Track: TTrack read FTrack;
    property Transition: TTransition read FTransition write SetTransition;
    property Visible;
    property Width;
    property OnCanFocus;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALButton = class(TALBaseText)
  public
    type
      // -----
      // TFill
      TFill = class(TALBaseText.TFill)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
      // -------
      // TStroke
      TStroke = class(TALBaseText.TStroke)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
      // -------------
      // TTextSettings
      TTextSettings = class(TALBaseTextSettings)
      public
        Type
          TFont = Class(TALFont)
          protected
            function GetDefaultWeight: TFontWeight; override;
          End;
      protected
        function CreateFont: TALFont; override;
        function GetDefaultHorzAlign: TALTextHorzAlign; override;
      published
        property Font;
        property Decoration;
        property Ellipsis;
        property MaxLines;
        property IsHtml;
        property HorzAlign;
        property VertAlign;
        property LineHeightMultiplier;
        property LetterSpacing;
      end;
      // ---------------
      // TBaseStateStyle
      TBaseStateStyle = class(TALBaseStateStyle)
      public
        type
          TFill = class(TALInheritBrush)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          TStroke = class(TALInheritStrokeBrush)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          TTextSettings = class(TALInheritBaseTextSettings)
          public
            Type
              TFont = Class(TALFont)
              protected
                function GetDefaultWeight: TFontWeight; override;
              End;
          protected
            function CreateFont: TALFont; override;
          published
            property Font;
            property Decoration;
          end;
      private
        FText: String;
        FTextSettings: TBaseStateStyle.TTextSettings;
        FXRadius: Single;
        FYRadius: Single;
        function GetStateStyleParent: TBaseStateStyle;
        function GetControlParent: TALButton;
        procedure SetText(const Value: string);
        procedure SetTextSettings(const AValue: TBaseStateStyle.TTextSettings);
        procedure SetXRadius(const Value: Single); virtual;
        procedure SetYRadius(const Value: Single); virtual;
        procedure TextSettingsChanged(ASender: TObject);
        function IsTextStored: Boolean;
        function IsXRadiusStored: Boolean;
        function IsYRadiusStored: Boolean;
      protected
        function CreateFill(const AParent: TALBrush): TALInheritBrush; override;
        function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
        function CreateTextSettings(const AParent: TALBaseTextSettings): TBaseStateStyle.TTextSettings; virtual;
        function GetDefaultText: String; virtual;
        function GetDefaultXRadius: Single; virtual;
        function GetDefaultYRadius: Single; virtual;
        function GetInherit: Boolean; override;
        procedure DoSupersede; override;
      public
        constructor Create(const AParent: TObject); override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; override;
        procedure ApplyColorScheme; override;
        procedure Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single; const AReverse: Boolean); override;
        property StateStyleParent: TBaseStateStyle read GetStateStyleParent;
        property ControlParent: TALButton read GetControlParent;
        property DefaultText: String read GetDefaultText;
        property DefaultXRadius: Single read GetDefaultXRadius;
        property DefaultYRadius: Single read GetDefaultYRadius;
      published
        property Fill;
        property Shadow;
        property Stroke;
        property Text: string read FText write SetText stored IsTextStored nodefault;
        property TextSettings: TBaseStateStyle.TTextSettings read fTextSettings write SetTextSettings;
        property XRadius: Single read FXRadius write SetXRadius stored IsXRadiusStored nodefault;
        property YRadius: Single read FYRadius write SetYRadius stored IsYRadiusStored nodefault;
      end;
      // -------------------
      // TDisabledStateStyle
      TDisabledStateStyle = class(TBaseStateStyle)
      private
        FOpacity: Single;
        procedure SetOpacity(const Value: Single);
        function IsOpacityStored: Boolean;
      protected
        function GetInherit: Boolean; override;
        function GetCacheSubIndex: Integer; override;
      public
        constructor Create(const AParent: TObject); override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
      published
        property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
      end;
      // ------------------
      // THoveredStateStyle
      THoveredStateStyle = class(TBaseStateStyle)
      protected
        function GetCacheSubIndex: Integer; override;
      published
        property StateLayer;
        property Scale;
      end;
      // ------------------
      // TPressedStateStyle
      TPressedStateStyle = class(TBaseStateStyle)
      protected
        function GetCacheSubIndex: Integer; override;
      published
        property StateLayer;
        property Scale;
      end;
      // ------------------
      // TFocusedStateStyle
      TFocusedStateStyle = class(TBaseStateStyle)
      protected
        function GetCacheSubIndex: Integer; override;
      published
        property StateLayer;
        property Scale;
      end;
      // ------------
      // TStateStyles
      TStateStyles = class(TALBaseStateStyles)
      private
        FDisabled: TDisabledStateStyle;
        FHovered: THoveredStateStyle;
        FPressed: TPressedStateStyle;
        FFocused: TFocusedStateStyle;
        function GetParent: TALButton;
        procedure SetDisabled(const AValue: TDisabledStateStyle);
        procedure SetHovered(const AValue: THoveredStateStyle);
        procedure SetPressed(const AValue: TPressedStateStyle);
        procedure SetFocused(const AValue: TFocusedStateStyle);
        procedure DisabledChanged(ASender: TObject);
        procedure HoveredChanged(ASender: TObject);
        procedure PressedChanged(ASender: TObject);
        procedure FocusedChanged(ASender: TObject);
      protected
        function CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle; virtual;
        function CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle; virtual;
        function CreatePressedStateStyle(const AParent: TObject): TPressedStateStyle; virtual;
        function CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle; virtual;
      public
        constructor Create(const AParent: TALControl); override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; override;
        procedure ApplyColorScheme; override;
        procedure ClearBufDrawable; override;
        function GetCurrentRawStyle: TALBaseStateStyle; override;
        Property Parent: TALButton read GetParent;
      published
        property Disabled: TDisabledStateStyle read FDisabled write SetDisabled;
        property Hovered: THoveredStateStyle read FHovered write SetHovered;
        property Pressed: TPressedStateStyle read FPressed write SetPressed;
        property Focused: TFocusedStateStyle read FFocused write SetFocused;
        property Transition;
      end;
  private
    {$IF defined(ALDPK)}
    FPrevStateStyles: TStateStyles;
    {$ENDIF}
    FStateStyles: TStateStyles;
    function GetTextSettings: TTextSettings;
    procedure SetStateStyles(const AValue: TStateStyles);
  protected
    function CreateFill: TALBrush; override;
    function CreateStroke: TALStrokeBrush; override;
    function CreateTextSettings: TALBaseTextSettings; override;
    function CreateStateStyles: TStateStyles; virtual;
    procedure SetTextSettings(const Value: TTextSettings); reintroduce;
    procedure SetName(const Value: TComponentName); override;
    procedure TextSettingsChanged(Sender: TObject); override;
    procedure StateStylesChanged(Sender: TObject); virtual;
    procedure IsMouseOverChanged; override;
    procedure IsFocusedChanged; override;
    procedure PressedChanged; override;
    procedure Click; override;
    Procedure DrawMultilineTextAdjustRect(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; var ARect: TrectF; var ASurfaceSize: TSizeF); override;
    {$IF NOT DEFINED(ALSkiaCanvas)}
    function GetRenderTargetRect(const ARect: TrectF): TRectF; override;
    {$ENDIF}
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent{TALControl}); override;
    procedure AlignToPixel; override;
    procedure ApplyColorScheme; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    property CacheEngine;
    property CacheIndex;
  published
    //property Action;
    property Align;
    property Anchors;
    property AutoSize default TALAutoSizeMode.Both;
    property AutoTranslate;
    property CanFocus default true;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClickSound;
    property ClipChildren;
    //property ClipParent;
    property Corners;
    property Cursor default crHandPoint;
    property DoubleBuffered;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Fill;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest default True;
    property Locked;
    property Margins;
    property MaxWidth;
    property MaxHeight;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Shadow;
    property Sides;
    property Size;
    property StateStyles: TStateStyles read FStateStyles write SetStateStyles;
    property Stroke;
    property TabOrder;
    property TabStop;
    property Text;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property XRadius;
    property YRadius;
    //property OnCanFocus;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnClick;
    property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALToggleButton = class(TALBaseText)
  public
    type
      // -------------
      // TGroupMessage
      TGroupMessage = class(TMessage)
      private
        FGroupName: string;
      public
        constructor Create(const AGroupName: string);
        property GroupName: string read FGroupName;
      end;
      // -----
      // TFill
      TFill = class(TALBaseText.TFill)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
      // -------
      // TStroke
      TStroke = class(TALBaseText.TStroke)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
      // -------------
      // TTextSettings
      TTextSettings = class(TALBaseTextSettings)
      public
        Type
          TFont = Class(TALFont)
          protected
            function GetDefaultWeight: TFontWeight; override;
          End;
      protected
        function CreateFont: TALFont; override;
        function GetDefaultHorzAlign: TALTextHorzAlign; override;
      published
        property Font;
        property Decoration;
        property Ellipsis;
        property MaxLines;
        property IsHtml;
        property HorzAlign;
        property VertAlign;
        property LineHeightMultiplier;
        property LetterSpacing;
      end;
      // ---------------
      // TBaseStateStyle
      TBaseStateStyle = class(TALBaseStateStyle)
      public
        type
          TFill = class(TALInheritBrush)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          TStroke = class(TALInheritStrokeBrush)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          TTextSettings = class(TALInheritBaseTextSettings)
          public
            Type
              TFont = Class(TALFont)
              protected
                function GetDefaultWeight: TFontWeight; override;
              End;
          protected
            function CreateFont: TALFont; override;
          published
            property Font;
            property Decoration;
          end;
      private
        FText: String;
        FTextSettings: TBaseStateStyle.TTextSettings;
        FXRadius: Single;
        FYRadius: Single;
        function GetStateStyleParent: TBaseStateStyle;
        function GetControlParent: TALToggleButton;
        procedure SetText(const Value: string);
        procedure SetTextSettings(const AValue: TBaseStateStyle.TTextSettings);
        procedure SetXRadius(const Value: Single); virtual;
        procedure SetYRadius(const Value: Single); virtual;
        procedure TextSettingsChanged(ASender: TObject);
        function IsTextStored: Boolean;
        function IsXRadiusStored: Boolean;
        function IsYRadiusStored: Boolean;
      protected
        function CreateFill(const AParent: TALBrush): TALInheritBrush; override;
        function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
        function CreateTextSettings(const AParent: TALBaseTextSettings): TBaseStateStyle.TTextSettings; virtual;
        function GetDefaultText: String; virtual;
        function GetDefaultXRadius: Single; virtual;
        function GetDefaultYRadius: Single; virtual;
        function GetInherit: Boolean; override;
        procedure DoSupersede; override;
      public
        constructor Create(const AParent: TObject); override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; override;
        procedure ApplyColorScheme; override;
        procedure Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single; const AReverse: Boolean); override;
        property StateStyleParent: TBaseStateStyle read GetStateStyleParent;
        property ControlParent: TALToggleButton read GetControlParent;
        property DefaultText: String read GetDefaultText;
        property DefaultXRadius: Single read GetDefaultXRadius;
        property DefaultYRadius: Single read GetDefaultYRadius;
      published
        property Fill;
        property Shadow;
        property Stroke;
        property Text: string read FText write SetText stored IsTextStored nodefault;
        property TextSettings: TBaseStateStyle.TTextSettings read fTextSettings write SetTextSettings;
        property XRadius: Single read FXRadius write SetXRadius stored IsXRadiusStored nodefault;
        property YRadius: Single read FYRadius write SetYRadius stored IsYRadiusStored nodefault;
      end;
      // ------------------
      // TDefaultStateStyle
      TDefaultStateStyle = class(TBaseStateStyle)
      protected
        function GetCacheSubIndex: Integer; override;
      end;
      // -------------------
      // TDisabledStateStyle
      TDisabledStateStyle = class(TBaseStateStyle)
      private
        FOpacity: Single;
        procedure SetOpacity(const Value: Single);
        function IsOpacityStored: Boolean;
      protected
        function GetInherit: Boolean; override;
        function GetCacheSubIndex: Integer; override;
      public
        constructor Create(const AParent: TObject); override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
      published
        property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
      end;
      // ------------------
      // THoveredStateStyle
      THoveredStateStyle = class(TBaseStateStyle)
      protected
        function GetCacheSubIndex: Integer; override;
      published
        property StateLayer;
        property Scale;
      end;
      // ------------------
      // TPressedStateStyle
      TPressedStateStyle = class(TBaseStateStyle)
      protected
        function GetCacheSubIndex: Integer; override;
      published
        property StateLayer;
        property Scale;
      end;
      // ------------------
      // TFocusedStateStyle
      TFocusedStateStyle = class(TBaseStateStyle)
      protected
        function GetCacheSubIndex: Integer; override;
      published
        property StateLayer;
        property Scale;
      end;
      // -----------------
      // TCheckStateStyles
      TCheckStateStyles = class(TALPersistentObserver)
      private
        FDefault: TDefaultStateStyle;
        FDisabled: TDisabledStateStyle;
        FHovered: THoveredStateStyle;
        FPressed: TPressedStateStyle;
        FFocused: TFocusedStateStyle;
        procedure SetDefault(const AValue: TDefaultStateStyle);
        procedure SetDisabled(const AValue: TDisabledStateStyle);
        procedure SetHovered(const AValue: THoveredStateStyle);
        procedure SetPressed(const AValue: TPressedStateStyle);
        procedure SetFocused(const AValue: TFocusedStateStyle);
        procedure DefaultChanged(ASender: TObject);
        procedure DisabledChanged(ASender: TObject);
        procedure HoveredChanged(ASender: TObject);
        procedure PressedChanged(ASender: TObject);
        procedure FocusedChanged(ASender: TObject);
      protected
        function CreateSavedState: TALPersistentObserver; override;
        function CreateDefaultStateStyle(const AParent: TObject): TDefaultStateStyle; virtual;
        function CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle; virtual;
        function CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle; virtual;
        function CreatePressedStateStyle(const AParent: TObject): TPressedStateStyle; virtual;
        function CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle; virtual;
      public
        constructor Create(const AParent: TALControl); reintroduce; virtual;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; virtual;
        procedure ApplyColorScheme; virtual;
        procedure ClearBufDrawable; virtual;
      published
        property &Default: TDefaultStateStyle read FDefault write SetDefault;
        property Disabled: TDisabledStateStyle read FDisabled write SetDisabled;
        property Hovered: THoveredStateStyle read FHovered write SetHovered;
        property Pressed: TPressedStateStyle read FPressed write SetPressed;
        property Focused: TFocusedStateStyle read FFocused write SetFocused;
      end;
      // ------------
      // TStateStyles
      TStateStyles = class(TALBaseStateStyles)
      public
        type
          // -----------
          // TTransition
          TTransition = class(TALBaseStateStyles.TTransition)
          published
            property FadeImage;
          end;
      private
        FChecked: TCheckStateStyles;
        FUnchecked: TCheckStateStyles;
        function GetParent: TALToggleButton;
        function GetTransition: TStateStyles.TTransition;
        procedure SetTransition(const AValue: TStateStyles.TTransition);
        procedure SetChecked(const AValue: TCheckStateStyles);
        procedure SetUnchecked(const AValue: TCheckStateStyles);
        procedure CheckedChanged(ASender: TObject);
        procedure UncheckedChanged(ASender: TObject);
      protected
        function CreateTransition: TALBaseStateStyles.TTransition; override;
        function CreateCheckedStateStyles(const AParent: TALControl): TCheckStateStyles; virtual;
        function CreateUncheckedStateStyles(const AParent: TALControl): TCheckStateStyles; virtual;
      public
        constructor Create(const AParent: TALControl); override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; override;
        procedure ApplyColorScheme; override;
        procedure ClearBufDrawable; override;
        function GetCurrentRawStyle: TALBaseStateStyle; override;
        Property Parent: TALToggleButton read GetParent;
      published
        property Checked: TCheckStateStyles read FChecked write SetChecked;
        property Unchecked: TCheckStateStyles read FUnchecked write SetUnchecked;
        property Transition: TStateStyles.TTransition read GetTransition write SetTransition;
      end;
  private
    {$IF defined(ALDPK)}
    FPrevStateStyles: TStateStyles;
    {$ENDIF}
    FStateStyles: TStateStyles;
    FGroupName: string;
    fMandatory: boolean;
    FChecked: Boolean;
    FOnChange: TNotifyEvent;
    function GetTextSettings: TTextSettings;
    procedure SetStateStyles(const AValue: TStateStyles);
    function GetGroupName: string;
    procedure SetGroupName(const Value: string);
    function GroupNameStored: Boolean;
    procedure GroupMessageCall(const Sender : TObject; const M : TMessage);
  protected
    function CreateFill: TALBrush; override;
    function CreateStroke: TALStrokeBrush; override;
    function CreateTextSettings: TALBaseTextSettings; override;
    function CreateStateStyles: TStateStyles; virtual;
    procedure SetTextSettings(const Value: TTextSettings); reintroduce;
    procedure SetName(const Value: TComponentName); override;
    procedure TextSettingsChanged(Sender: TObject); override;
    procedure StateStylesChanged(Sender: TObject); virtual;
    procedure IsMouseOverChanged; override;
    procedure IsFocusedChanged; override;
    procedure PressedChanged; override;
    function GetChecked: Boolean; virtual;
    procedure SetChecked(const Value: Boolean); virtual;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure DoClickSound; override;
    procedure Click; override;
    procedure DoChanged; virtual;
    procedure AdjustSize; override;
    Procedure DrawMultilineTextAdjustRect(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; var ARect: TrectF; var ASurfaceSize: TSizeF); override;
    {$IF NOT DEFINED(ALSkiaCanvas)}
    function GetRenderTargetRect(const ARect: TrectF): TRectF; override;
    {$ENDIF}
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure Assign(Source: TPersistent{TALControl}); override;
    procedure AlignToPixel; override;
    procedure ApplyColorScheme; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    property CacheEngine;
    property CacheIndex;
  published
    //property Action;
    property Align;
    property Anchors;
    property AutoSize default TALAutoSizeMode.Both;
    property AutoTranslate;
    property CanFocus default true;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property Checked: Boolean read GetChecked write SetChecked default False;
    property ClickSound;
    property ClipChildren;
    //property ClipParent;
    property Corners;
    property Cursor default crHandPoint;
    property DoubleBuffered;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Fill;
    property GroupName: string read GetGroupName write SetGroupName stored GroupNameStored nodefault;
    property Mandatory: Boolean read fMandatory write fMandatory default false;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest default True;
    property Locked;
    property Margins;
    property MaxWidth;
    property MaxHeight;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Shadow;
    property Sides;
    property Size;
    property StateStyles: TStateStyles read FStateStyles write SetStateStyles;
    property Stroke;
    property TabOrder;
    property TabStop;
    property Text;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property XRadius;
    property YRadius;
    //property OnCanFocus;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCustomTrack = class(TALControl)
  public
    type
      // ------
      // TTrack
      TTrack = class(TALBaseRectangle)
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
          // -------------------
          // TStopIndicatorBrush
          TStopIndicatorBrush = class(TALPersistentObserver)
          private
            FColor: TAlphaColor;
            FColorKey: String;
            FResourceName: String;
            FWrapMode: TALImageWrapMode;
            FSize: Single;
            procedure SetColor(const Value: TAlphaColor);
            procedure SetColorKey(const Value: String);
            procedure SetResourceName(const Value: String);
            procedure SetWrapMode(const Value: TALImageWrapMode);
            procedure SetSize(const Value: Single);
            function IsColorStored: Boolean;
            function IsColorKeyStored: Boolean;
            function IsResourceNameStored: Boolean;
            function IsWrapModeStored: Boolean;
            function IsSizeStored: Boolean;
          protected
            function GetDefaultColor: TAlphaColor; virtual;
            function GetDefaultColorKey: String; virtual;
            function GetDefaultResourceName: String; virtual;
            function GetDefaultWrapMode: TALImageWrapMode; virtual;
            function GetDefaultSize: Single; virtual;
          public
            constructor Create; override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure AlignToPixel; virtual;
            procedure ApplyColorScheme; virtual;
            procedure Interpolate(const ATo: TStopIndicatorBrush; const ANormalizedTime: Single; const AReverse: Boolean); virtual;
            procedure InterpolateNoChanges(const ATo: TStopIndicatorBrush; const ANormalizedTime: Single; const AReverse: Boolean);
            function hasStopIndicator: Boolean;
            property DefaultColor: TAlphaColor read GetDefaultColor;
            property DefaultColorKey: String read GetDefaultColorKey;
            property DefaultResourceName: String read GetDefaultResourceName;
            property DefaultWrapMode: TALImageWrapMode read GetDefaultWrapMode;
            property DefaultSize: Single read GetDefaultSize;
          published
            property Color: TAlphaColor read FColor write SetColor stored IsColorStored;
            property ColorKey: String read FColorKey write SetColorKey stored IsColorKeyStored;
            property ResourceName: String read FResourceName write SetResourceName stored IsResourceNameStored nodefault;
            property WrapMode: TALImageWrapMode read FWrapMode write SetWrapMode stored IsWrapModeStored;
            property Size: Single read FSize write SetSize stored IsSizeStored nodefault;
          end;
          // --------------------------
          // TInheritStopIndicatorBrush
          TInheritStopIndicatorBrush = class(TStopIndicatorBrush)
          private
            FParent: TStopIndicatorBrush;
            FInherit: Boolean;
            fSuperseded: Boolean;
            procedure SetInherit(const AValue: Boolean);
          protected
            function CreateSavedState: TALPersistentObserver; override;
            procedure DoSupersede; virtual;
          public
            constructor Create(const AParent: TStopIndicatorBrush); reintroduce; virtual;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure Supersede(Const ASaveState: Boolean = False); virtual;
            procedure SupersedeNoChanges(Const ASaveState: Boolean = False);
            property Superseded: Boolean read FSuperseded;
            property Parent: TStopIndicatorBrush read FParent;
          published
            property Inherit: Boolean read FInherit write SetInherit Default True;
          end;
          // ---------------
          // TBaseStateStyle
          TBaseStateStyle = class(TALBaseStateStyle)
          public
            type
              TFill = class(TALInheritBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStroke = class(TALInheritStrokeBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStateLayer = class(TALStateLayer)
              protected
                function GetDefaultXRadius: Single; override;
                function GetDefaultYRadius: Single; override;
              end;
          private
            FStopIndicator: TInheritStopIndicatorBrush;
            function GetControlParent: TTrack;
            procedure SetStopIndicator(const AValue: TInheritStopIndicatorBrush);
            procedure StopIndicatorChanged(ASender: TObject);
          protected
            function CreateFill(const AParent: TALBrush): TALInheritBrush; override;
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateStateLayer: TALStateLayer; override;
            function CreateStopIndicator(const AParent: TStopIndicatorBrush): TInheritStopIndicatorBrush; virtual;
            procedure DoSupersede; override;
            function GetInherit: Boolean; override;
          public
            constructor Create(const AParent: TObject); override;
            destructor Destroy; override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure AlignToPixel; override;
            procedure ApplyColorScheme; override;
            procedure Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single; const AReverse: Boolean); override;
            property ControlParent: TTrack read GetControlParent;
          published
            property Fill;
            property StopIndicator: TInheritStopIndicatorBrush read FStopIndicator write SetStopIndicator;
            property Stroke;
          end;
          // -------------------
          // TDisabledStateStyle
          TDisabledStateStyle = class(TBaseStateStyle)
          private
            FOpacity: Single;
            procedure SetOpacity(const Value: Single);
            function IsOpacityStored: Boolean;
          protected
            function GetInherit: Boolean; override;
          public
            constructor Create(const AParent: TObject); override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
          published
            property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
          end;
          // ------------
          // TStateStyles
          TStateStyles = class(TALBaseStateStyles)
          private
            FDisabled: TDisabledStateStyle;
            function GetParent: TTrack;
            procedure SetDisabled(const AValue: TDisabledStateStyle);
            procedure DisabledChanged(ASender: TObject);
          protected
            function CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle; virtual;
          public
            constructor Create(const AParent: TALControl); override;
            destructor Destroy; override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure AlignToPixel; override;
            procedure ApplyColorScheme; override;
            procedure ClearBufDrawable; override;
            function GetCurrentRawStyle: TALBaseStateStyle; override;
            Property Parent: TTrack read GetParent;
          published
            property Disabled: TDisabledStateStyle read FDisabled write SetDisabled;
          end;
      private
        FStateStyles: TStateStyles;
        FCustomTrack: TALCustomTrack;
        FStopIndicator: TStopIndicatorBrush;
        procedure SetStateStyles(const AValue: TStateStyles);
        procedure SetStopIndicator(const Value: TStopIndicatorBrush);
      protected
        function CreateFill: TALBrush; override;
        function CreateStroke: TALStrokeBrush; override;
        function CreateStopIndicator: TStopIndicatorBrush; virtual;
        function CreateStateStyles: TStateStyles; virtual;
        procedure StateStylesChanged(Sender: TObject); virtual;
        procedure StopIndicatorChanged(Sender: TObject); virtual;
        procedure PaddingChanged; override;
        function HasCustomDraw: Boolean; override;
        Procedure CreateBufDrawable(
                    var ABufDrawable: TALDrawable;
                    out ABufDrawableRect: TRectF;
                    const AScale: Single;
                    const AFill: TALBrush;
                    const AStateLayer: TALStateLayer;
                    const AStateLayerContentColor: TAlphaColor;
                    const ADrawStateLayerOnTop: Boolean;
                    const AStroke: TALStrokeBrush;
                    const AShadow: TALShadow); overload; override;
        Procedure CreateBufDrawable(
                    var ABufDrawable: TALDrawable;
                    out ABufDrawableRect: TRectF;
                    const AScale: Single;
                    const AFill: TALBrush;
                    const AStateLayer: TALStateLayer;
                    const AStateLayerContentColor: TAlphaColor;
                    const ADrawStateLayerOnTop: Boolean;
                    const AStroke: TALStrokeBrush;
                    const AShadow: TALShadow;
                    const AStopIndicator: TStopIndicatorBrush); reintroduce; overload; virtual;
        function GetBufDrawableSrcRect: TRectF; virtual; abstract;
        procedure Paint; override;
      public
        constructor Create(const ACustomTrack: TALCustomTrack); reintroduce; virtual;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent{TALControl}); override;
        procedure AlignToPixel; override;
        procedure ApplyColorScheme; override;
        procedure MakeBufDrawable; override;
        procedure ClearBufDrawable; override;
        property HitTest default false;
        property Locked default True;
        property Position stored false;
      published
        //property Action;
        //property Align;
        //property Anchors;
        //property AutoSize;
        //property CanFocus;
        //property CanParentFocus;
        //property DisableFocusEffect;
        property ClickSound;
        //property ClipChildren;
        //property ClipParent;
        property Corners;
        //property Cursor;
        //property DoubleBuffered;
        //property DragMode;
        //property EnableDragHighlight;
        //property Enabled;
        property Fill;
        //property Height;
        //property Hint;
        //property ParentShowHint;
        //property ShowHint;
        //property HitTest;
        //property Locked;
        property Margins;
        property Opacity;
        property Padding;
        //property PopupMenu;
        //property Position;
        //property RotationAngle;
        //property RotationCenter;
        //property Pivot;
        //property Scale;
        //property Shadow;
        //property Sides;
        //property Size;
        property StateStyles: TStateStyles read FStateStyles write SetStateStyles;
        property StopIndicator: TStopIndicatorBrush read FStopIndicator write SetStopIndicator;
        property Stroke;
        //property TabOrder;
        //property TabStop;
        //property TouchTargetExpansion;
        //property Visible;
        //property Width;
        property XRadius;
        property YRadius;
        //property OnCanFocus;
        //property OnDragEnter;
        //property OnDragLeave;
        //property OnDragOver;
        //property OnDragDrop;
        //property OnDragEnd;
        //property OnEnter;
        //property OnExit;
        //property OnMouseEnter;
        //property OnMouseLeave;
        //property OnMouseDown;
        //property OnMouseUp;
        //property OnMouseMove;
        //property OnMouseWheel;
        //property OnClick;
        //property OnDblClick;
        //property OnKeyDown;
        //property OnKeyUp;
        property OnPainting;
        property OnPaint;
        //property OnResize;
        //property OnResized;
      end;
      // --------------
      // TInactiveTrack
      TInactiveTrack = class(TTrack)
      protected
        function GetBufDrawableSrcRect: TRectF; override;
      end;
      // ------------
      // TActiveTrack
      TActiveTrack = class(TTrack)
      public
        type
          // -----
          // TFill
          TFill = class(TTrack.TFill)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          // -------------------
          // TDisabledStateStyle
          TDisabledStateStyle = class(TTrack.TDisabledStateStyle)
          public
            type
              TFill = class(TTrack.TDisabledStateStyle.TFill)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
          protected
            function CreateFill(const AParent: TALBrush): TALInheritBrush; override;
          end;
          // ------------
          // TStateStyles
          TStateStyles = class(TTrack.TStateStyles)
          protected
            function CreateDisabledStateStyle(const AParent: TObject): TTrack.TDisabledStateStyle; override;
          end;
      protected
        function CreateFill: TALBrush; override;
        function CreateStateStyles: TTrack.TStateStyles; override;
        function GetBufDrawableSrcRect: TRectF; override;
      end;
      // ------
      // TThumb
      TThumb = class(TALBaseRectangle)
      public
        type
          // -------
          // TStroke
          TStroke = class(TALStrokeBrush)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          // ---------------
          // TBaseStateStyle
          TBaseStateStyle = class(TALBaseStateStyle)
          public
            type
              TStroke = class(TALInheritStrokeBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStateLayer = class(TALStateLayer)
              protected
                function GetDefaultXRadius: Single; override;
                function GetDefaultYRadius: Single; override;
              end;
          protected
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateStateLayer: TALStateLayer; override;
          published
            property Fill;
            property Shadow;
            property Stroke;
          end;
          // -------------------
          // TDisabledStateStyle
          TDisabledStateStyle = class(TBaseStateStyle)
          private
            FOpacity: Single;
            procedure SetOpacity(const Value: Single);
            function IsOpacityStored: Boolean;
          protected
            function GetInherit: Boolean; override;
          public
            constructor Create(const AParent: TObject); override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
          published
            property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
          end;
          // ------------------
          // THoveredStateStyle
          THoveredStateStyle = class(TBaseStateStyle)
          published
            property StateLayer;
            property Scale;
          end;
          // ------------------
          // TPressedStateStyle
          TPressedStateStyle = class(TBaseStateStyle)
          published
            property StateLayer;
            property Scale;
          end;
          // ------------------
          // TFocusedStateStyle
          TFocusedStateStyle = class(TBaseStateStyle)
          published
            property StateLayer;
            property Scale;
          end;
          // ------------
          // TStateStyles
          TStateStyles = class(TALBaseStateStyles)
          private
            FDisabled: TDisabledStateStyle;
            FHovered: THoveredStateStyle;
            FPressed: TPressedStateStyle;
            FFocused: TFocusedStateStyle;
            function GetParent: TThumb;
            procedure SetDisabled(const AValue: TDisabledStateStyle);
            procedure SetHovered(const AValue: THoveredStateStyle);
            procedure SetPressed(const AValue: TPressedStateStyle);
            procedure SetFocused(const AValue: TFocusedStateStyle);
            procedure DisabledChanged(ASender: TObject);
            procedure HoveredChanged(ASender: TObject);
            procedure PressedChanged(ASender: TObject);
            procedure FocusedChanged(ASender: TObject);
          protected
            function CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle; virtual;
            function CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle; virtual;
            function CreatePressedStateStyle(const AParent: TObject): TPressedStateStyle; virtual;
            function CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle; virtual;
          public
            constructor Create(const AParent: TALControl); override;
            destructor Destroy; override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure AlignToPixel; override;
            procedure ApplyColorScheme; override;
            procedure ClearBufDrawable; override;
            function GetCurrentRawStyle: TALBaseStateStyle; override;
            Property Parent: TThumb read GetParent;
          published
            property Disabled: TDisabledStateStyle read FDisabled write SetDisabled;
            property Hovered: THoveredStateStyle read FHovered write SetHovered;
            property Pressed: TPressedStateStyle read FPressed write SetPressed;
            property Focused: TFocusedStateStyle read FFocused write SetFocused;
            property Transition;
          end;
      private
        FStateStyles: TStateStyles;
        fValueRange: TValueRange;
        FCustomTrack: TALCustomTrack;
        fCustomTrackMouseDownPos: TPointF;
        fScrollCapturedByMe: boolean;
        procedure SetStateStyles(const AValue: TStateStyles);
        procedure ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
      protected
        function CreateStroke: TALStrokeBrush; override;
        function CreateStateStyles: TStateStyles; virtual;
        procedure DoBeginUpdate; override;
        procedure DoEndUpdate; override;
        function GetDefaultXRadius: Single; override;
        function GetDefaultYRadius: Single; override;
        procedure StateStylesChanged(Sender: TObject); virtual;
        procedure IsMouseOverChanged; override;
        procedure IsFocusedChanged; override;
        procedure PressedChanged; override;
        procedure ValueRangeChanged(Sender: TObject); Virtual;
        procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
        {$IF NOT DEFINED(ALSkiaCanvas)}
        function GetRenderTargetRect(const ARect: TrectF): TRectF; override;
        {$ENDIF}
        procedure Paint; override;
      public
        constructor Create(const ACustomTrack: TALCustomTrack); reintroduce; virtual;
        destructor Destroy; override;
        procedure BeforeDestruction; override;
        procedure Assign(Source: TPersistent{TALControl}); override;
        procedure AlignToPixel; override;
        procedure ApplyColorScheme; override;
        function GetValue: Double;
        procedure MakeBufDrawable; override;
        procedure ClearBufDrawable; override;
        procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
        procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
        procedure DoMouseLeave; override;
        property Locked default True;
        property Position stored false;
        property CanFocus default true;
      published
        //property Action;
        //property Align;
        //property Anchors;
        //property AutoSize;
        //property CanFocus default true;
        //property CanParentFocus;
        //property DisableFocusEffect;
        property ClickSound;
        //property ClipChildren;
        //property ClipParent;
        property Corners;
        property Cursor default crHandPoint;
        //property DoubleBuffered;
        //property DragMode;
        //property EnableDragHighlight;
        //property Enabled;
        property Fill;
        property Height;
        //property Hint;
        //property ParentShowHint;
        //property ShowHint;
        //property HitTest;
        //property Locked;
        property Margins;
        property Opacity;
        property Padding;
        //property PopupMenu;
        //property Position;
        //property RotationAngle;
        //property RotationCenter;
        //property Pivot;
        //property Scale;
        property Shadow;
        //property Sides;
        property Size;
        property StateStyles: TStateStyles read FStateStyles write SetStateStyles;
        property Stroke;
        //property TabOrder;
        //property TabStop;
        property TouchTargetExpansion;
        //property Visible;
        property Width;
        property XRadius;
        property YRadius;
        //property OnCanFocus;
        //property OnDragEnter;
        //property OnDragLeave;
        //property OnDragOver;
        //property OnDragDrop;
        //property OnDragEnd;
        //property OnEnter;
        //property OnExit;
        //property OnMouseEnter;
        //property OnMouseLeave;
        //property OnMouseDown;
        //property OnMouseUp;
        //property OnMouseMove;
        //property OnMouseWheel;
        //property OnClick;
        //property OnDblClick;
        //property OnKeyDown;
        //property OnKeyUp;
        property OnPainting;
        property OnPaint;
        //property OnResize;
        //property OnResized;
      end;
      // ---------------
      // TValueIndicator
      TValueIndicator = class(TALBaseText)
      public
        class var Format0: String;
      public
        type
          // -----
          // TFill
          TFill = class(TALBaseText.TFill)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          // -------------
          // TTextSettings
          TTextSettings = Class(TALTextSettings)
          public
            Type
              TFont = Class(TALFont)
              protected
                function GetDefaultWeight: TFontWeight; override;
                function GetDefaultColor: TAlphaColor; override;
              End;
          protected
            function CreateFont: TALFont; override;
            function GetDefaultHorzAlign: TALTextHorzAlign; override;
          End;
          // ----------
          // TAnimation
          TAnimation = (None, ScaleInOut, Opacity);
          // ------------------
          // TCustomFormatEvent
          TCustomFormatEvent = procedure(ASender: TObject; const AValue: Double; Var AText: String) of object;
      private
        FCustomTrack: TALCustomTrack;
        FFormat: String;
        FOnCustomFormat: TCustomFormatEvent;
        FFloatAnimation: TALFloatAnimation;
        FAnimation: TAnimation;
        FShowOnInteraction: Boolean;
        function GetTextSettings: TALTextSettings;
        procedure SetFormat(const Value: string);
        function IsFormatStored: Boolean;
        procedure AnimationProcess(Sender: TObject);
        procedure AnimationFinish(Sender: TObject);
        procedure AdjustPosition(const AThumb: TThumb);
      protected
        function CreateFill: TALBrush; override;
        function CreateTextSettings: TALBaseTextSettings; override;
        procedure SetTextSettings(const Value: TALTextSettings); reintroduce;
        function GetDefaultXRadius: Single; override;
        function GetDefaultYRadius: Single; override;
        function GetDefaultFormat: String; virtual;
      public
        constructor Create(const ACustomTrack: TALCustomTrack); reintroduce; virtual;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent{TALControl}); override;
        procedure Refresh(const AThumb: TThumb);
        property DefaultFormat: String read GetDefaultFormat;
        property Visible default false;
      published
        //property Action;
        //property Align;
        //property Anchors;
        property Animation: TAnimation read FAnimation write FAnimation default TAnimation.ScaleInOut;
        property AutoSize default TALAutoSizeMode.Both;
        //property AutoTranslate;
        //property CanFocus;
        //property CanParentFocus;
        //property DisableFocusEffect;
        property ClickSound;
        //property ClipChildren;
        //property ClipParent;
        property Corners;
        //property Cursor;
        //property DoubleBuffered;
        //property DragMode;
        //property EnableDragHighlight;
        //property Enabled;
        property Fill;
        property Format: string read FFormat write SetFormat stored IsFormatStored;
        property Height;
        //property Hint;
        //property ParentShowHint;
        //property ShowHint;
        //property HitTest;
        //property Locked;
        property Margins;
        //property MaxWidth;
        //property MaxHeight;
        property Opacity;
        property Padding;
        //property PopupMenu;
        //property Position;
        //property RotationAngle;
        //property RotationCenter;
        //property Pivot;
        //property Scale;
        property Shadow;
        property ShowOnInteraction: Boolean read FShowOnInteraction write FShowOnInteraction default false;
        property Sides;
        property Size;
        property Stroke;
        //property TabOrder;
        //property TabStop;
        //property Text;
        property TextSettings: TALTextSettings read GetTextSettings write SetTextSettings;
        //property TouchTargetExpansion;
        //property Visible default false;
        property Width;
        property XRadius;
        property YRadius;
        //property OnCanFocus;
        property OnCustomFormat: TCustomFormatEvent read FOnCustomFormat write FOnCustomFormat;
        //property OnDragEnter;
        //property OnDragLeave;
        //property OnDragOver;
        //property OnDragDrop;
        //property OnDragEnd;
        //property OnElementClick;
        //property OnElementDblClick;
        //property OnElementMouseDown;
        //property OnElementMouseMove;
        //property OnElementMouseUp;
        //property OnElementMouseEnter;
        //property OnElementMouseLeave;
        //property OnEnter;
        //property OnExit;
        //property OnMouseEnter;
        //property OnMouseLeave;
        //property OnMouseDown;
        //property OnMouseUp;
        //property OnMouseMove;
        //property OnMouseWheel;
        //property OnClick;
        //property OnDblClick;
        //property OnKeyDown;
        //property OnKeyUp;
        property OnPainting;
        property OnPaint;
        //property OnResize;
        //property OnResized;
      end;
  private
    FTabStop: Boolean;
    FIsAligningTracks: Boolean;
    function FrequencyStored: Boolean;
    function MaxStored: Boolean;
    function MinStored: Boolean;
    function ViewportSizeStored: Boolean;
  protected
    type
      TInactiveTrackClass = Class of TInactiveTrack;
      TActiveTrackClass = Class of TActiveTrack;
      TThumbClass = Class of TThumb;
      TValueIndicatorClass = Class of TValueIndicator;
  protected
    FInactiveTrack: TInactiveTrack;
    FActiveTrack: TActiveTrack;
    FThumb: TThumb;
    FValueIndicator: TValueIndicator;
    FOrientation: TOrientation;
    FOnChange: TNotifyEvent;
    function GetLeadingTrack: TTrack; virtual;
    function GetTrailingTrack: TTrack; virtual;
    function GetLeadingTrackStartPadding: Single;
    function GetTrailingTrackEndPadding: Single;
    /// <summary>
    ///   Return the precise size of the track where the center
    ///   of the thumb can be positioned
    /// </summary>
    function GetTrackSize(Const AIncludeTrackPadding: Boolean = False): Single; virtual;
    function GetDoubleBuffered: boolean; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    function _GetCanFocus: Boolean; virtual;
    procedure _SetCanFocus(const Value: Boolean); virtual;
    procedure _SetTabStop(const Value: Boolean); virtual;
    procedure SetViewportSize(const Value: Double); virtual;
    function GetViewportSize: Double; virtual;
    function GetFrequency: Double; virtual;
    procedure SetFrequency(const Value: Double); virtual;
    function GetMax: Double; virtual;
    procedure SetMax(const Value: Double); virtual;
    function GetMin: Double; virtual;
    procedure SetMin(const Value: Double); virtual;
    function GetValue: Double; virtual;
    procedure SetValue(Value: Double); virtual;
    function ValueStored: Boolean; virtual;
    procedure SetOrientation(const Value: TOrientation); virtual;
    /// <summary>
    ///   Return the center position of the thumb corresponding to AValue
    /// </summary>
    function GetThumbPos(const AValue: single): Single; virtual;
    procedure DoResized; override;
    procedure DoRealign; override;
    procedure Loaded; override;
    procedure DoChanged; virtual;
    procedure EnabledChanged; override;
    property Value: Double read GetValue write SetValue stored ValueStored nodefault;
    property InactiveTrack: TInactiveTrack read FInactiveTrack;
    property ActiveTrack: TActiveTrack read FActiveTrack;
    property Thumb: TThumb read FThumb;
    property ValueIndicator: TValueIndicator read FValueIndicator;
    function CreateInactiveTrack(const AInactiveTrackClass: TInactiveTrackClass = nil; Const AName: String = 'InactiveTrack'): TInactiveTrack; virtual;
    function CreateActiveTrack(const AActiveTrackClass: TActiveTrackClass = nil; Const AName: String = 'ActiveTrack'): TActiveTrack; virtual;
    function CreateThumb(const AThumbClass: TThumbClass = nil; Const AName: String = 'Thumb'): TThumb; virtual;
    function CreateValueIndicator(const AValueIndicatorClass: TValueIndicatorClass = nil; Const AName: String = 'ValueIndicator'): TValueIndicator; virtual;
    procedure AlignThumb; virtual;
    procedure AlignTracks; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    procedure Assign(Source: TPersistent{TALControl}); override;
    procedure AlignToPixel; override;
    procedure ApplyColorScheme; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    property DoubleBuffered default true;
    property CanFocus: Boolean read _GetCanFocus write _SetCanFocus default True;
    property TabStop: Boolean read FTabStop write _SetTabStop default True;
    property Min: Double read GetMin write SetMin stored MinStored nodefault;
    property Max: Double read GetMax write SetMax stored MaxStored nodefault;
    property Frequency: Double read GetFrequency write SetFrequency stored FrequencyStored nodefault;
    property ViewportSize: Double read GetViewportSize write SetViewportSize stored ViewportSizeStored nodefault;
    property Orientation: TOrientation read FOrientation write SetOrientation default TOrientation.Horizontal;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALTrackBar = class(TALCustomTrack)
  protected
    function GetDefaultSize: TSizeF; override;
  published
    //property Action;
    property ActiveTrack;
    property Align;
    property Anchors;
    //property AutoSize;
    property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property DoubleBuffered;
    property ClickSound;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
    //property DoubleBuffered;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Frequency;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property InactiveTrack;
    property Locked;
    property Margins;
    property Min;
    property Max;
    property Opacity;
    property Orientation;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Size;
    property TabOrder;
    property TabStop;
    property Thumb;
    property TouchTargetExpansion;
    property Value;
    property ValueIndicator;
    property Visible;
    property Width;
    property OnCanFocus;
    property OnChange;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALRangeTrackBar = class(TALCustomTrack)
  public
    Type
      // -----------------
      // TMinInactiveTrack
      TMinInactiveTrack = class(TALCustomTrack.TInactiveTrack)
      private
        function _GetOpacity: Single;
        procedure _SetOpacity(const AValue: Single);
        function _IsOpacityStored: boolean;
      protected
        procedure SetXRadius(const Value: Single); override;
        procedure SetYRadius(const Value: Single); override;
        procedure SetCorners(const Value: TCorners); override;
        procedure MarginsChanged; override;
        procedure PaddingChanged; override;
        procedure StopIndicatorChanged(Sender: TObject); override;
        procedure FillChanged(Sender: TObject); override;
        procedure StrokeChanged(Sender: TObject); override;
        function GetBufDrawableSrcRect: TRectF; override;
      published
        property Opacity: Single read _GetOpacity write _SetOpacity stored _IsOpacityStored nodefault;
      end;
      // -----------------
      // TMaxInactiveTrack
      TMaxInactiveTrack = class(TALCustomTrack.TInactiveTrack)
      protected
        function GetBufDrawableSrcRect: TRectF; override;
      end;
      // ------------
      // TActiveTrack
      TActiveTrack = class(TALCustomTrack.TActiveTrack)
      protected
        function GetBufDrawableSrcRect: TRectF; override;
      end;
      // ---------
      // TMinThumb
      TMinThumb = class(TALCustomTrack.TThumb)
      private
        FFormerTouchTargetExpansionChangedHandler: TNotifyEvent;
        function _GetOpacity: Single;
        procedure _SetOpacity(const AValue: Single);
        function _GetCursor: TCursor;
        procedure _SetCursor(const AValue: TCursor);
        function _IsOpacityStored: boolean;
      protected
        procedure SetXRadius(const Value: Single); override;
        procedure SetYRadius(const Value: Single); override;
        procedure SetCorners(const Value: TCorners); override;
        procedure MarginsChanged; override;
        procedure PaddingChanged; override;
        procedure FillChanged(Sender: TObject); override;
        procedure StrokeChanged(Sender: TObject); override;
        procedure ShadowChanged(Sender: TObject); override;
        procedure StateStylesChanged(Sender: TObject); override;
        procedure TouchTargetExpansionChanged(Sender: TObject); virtual;
        procedure DoResized; override;
     public
       constructor Create(const ACustomTrack: TALCustomTrack); override;
     published
        property Opacity: Single read _GetOpacity write _SetOpacity stored _IsOpacityStored nodefault;
        property Cursor: TCursor read _GetCursor write _SetCursor default crHandPoint;
      end;
      // ---------
      // TMaxThumb
      TMaxThumb = class(TALCustomTrack.TThumb)
      public
        constructor Create(const ACustomTrack: TALCustomTrack); override;
      end;
  protected
    FMaxInactiveTrack: TALCustomTrack.TInactiveTrack;
    FMaxThumb: TALCustomTrack.TThumb;
    function GetLeadingTrack: TALCustomTrack.TTrack; override;
    function GetTrailingTrack: TALCustomTrack.TTrack; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    procedure _SetCanFocus(const Value: Boolean); override;
    procedure _SetTabStop(const Value: Boolean); override;
    procedure SetViewportSize(const Value: Double); override;
    procedure SetFrequency(const Value: Double); override;
    procedure SetMax(const Value: Double); override;
    procedure SetMin(const Value: Double); override;
    function MaxValueStored: Boolean; virtual;
    function GetDefaultSize: TSizeF; override;
    procedure SetValue(Value: Double); override;
    function GetMaxValue: Double; virtual;
    procedure SetMaxValue(Value: Double); virtual;
    procedure SetOrientation(const Value: TOrientation); override;
    procedure EnabledChanged; override;
    procedure Loaded; override;
    function CreateInactiveTrack(const AInactiveTrackClass: TALCustomTrack.TInactiveTrackClass = nil; Const AName: String = 'InactiveTrack'): TALCustomTrack.TInactiveTrack; override;
    function CreateActiveTrack(const AActiveTrackClass: TALCustomTrack.TActiveTrackClass = nil; Const AName: String = 'ActiveTrack'): TALCustomTrack.TActiveTrack; override;
    function CreateThumb(const AThumbClass: TALCustomTrack.TThumbClass = nil; Const AName: String = 'Thumb'): TALCustomTrack.TThumb; override;
    procedure DoResized; override;
    procedure DoRealign; override;
    procedure AlignThumb; override;
    procedure AlignTracks; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AlignToPixel; override;
    procedure ApplyColorScheme; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
  published
    //property Action;
    property ActiveTrack;
    property Align;
    property Anchors;
    //property AutoSize;
    property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property DoubleBuffered;
    property ClickSound;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
    //property DoubleBuffered;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Frequency;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property InactiveTrack;
    property Locked;
    property Margins;
    property Min;
    property Max;
    property MinValue: Double read GetValue write SetValue stored ValueStored nodefault;
    property MaxValue: Double read GetMaxValue write SetMaxValue stored MaxValueStored nodefault;
    property Thumb;
    property Opacity;
    property Orientation;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Size;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Value;
    property ValueIndicator;
    property Visible;
    property Width;
    property OnCanFocus;
    property OnChange;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCustomScrollBar = class(TALCustomTrack)
  public
    type
      // ------
      // TThumb
      TThumb = class(TALCustomTrack.TThumb)
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
          TStroke = class(TALCustomTrack.TThumb.TStroke)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          // -------------------
          // TDisabledStateStyle
          TDisabledStateStyle = class(TALCustomTrack.TThumb.TDisabledStateStyle)
          public
            type
              TFill = class(TALInheritBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStroke = class(TALCustomTrack.TThumb.TDisabledStateStyle.TStroke)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStateLayer = class(TALCustomTrack.TThumb.TDisabledStateStyle.TStateLayer)
              protected
                function GetDefaultXRadius: Single; override;
                function GetDefaultYRadius: Single; override;
              end;
          protected
            function CreateFill(const AParent: TALBrush): TALInheritBrush; override;
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateStateLayer: TALStateLayer; override;
          end;
          // ------------------
          // THoveredStateStyle
          THoveredStateStyle = class(TALCustomTrack.TThumb.THoveredStateStyle)
          public
            type
              TFill = class(TALInheritBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStroke = class(TALCustomTrack.TThumb.THoveredStateStyle.TStroke)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStateLayer = class(TALCustomTrack.TThumb.THoveredStateStyle.TStateLayer)
              protected
                function GetDefaultXRadius: Single; override;
                function GetDefaultYRadius: Single; override;
              end;
          protected
            function CreateFill(const AParent: TALBrush): TALInheritBrush; override;
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateStateLayer: TALStateLayer; override;
          end;
          // ------------------
          // TPressedStateStyle
          TPressedStateStyle = class(TALCustomTrack.TThumb.TPressedStateStyle)
          public
            type
              TFill = class(TALInheritBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStroke = class(TALCustomTrack.TThumb.TPressedStateStyle.TStroke)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStateLayer = class(TALCustomTrack.TThumb.TPressedStateStyle.TStateLayer)
              protected
                function GetDefaultXRadius: Single; override;
                function GetDefaultYRadius: Single; override;
              end;
          protected
            function CreateFill(const AParent: TALBrush): TALInheritBrush; override;
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateStateLayer: TALStateLayer; override;
          end;
          // ------------------
          // TFocusedStateStyle
          TFocusedStateStyle = class(TALCustomTrack.TThumb.TFocusedStateStyle)
          public
            type
              TFill = class(TALInheritBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStroke = class(TALCustomTrack.TThumb.TFocusedStateStyle.TStroke)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStateLayer = class(TALCustomTrack.TThumb.TFocusedStateStyle.TStateLayer)
              protected
                function GetDefaultXRadius: Single; override;
                function GetDefaultYRadius: Single; override;
              end;
          protected
            function CreateFill(const AParent: TALBrush): TALInheritBrush; override;
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateStateLayer: TALStateLayer; override;
          end;
          // ------------
          // TStateStyles
          TStateStyles = class(TALCustomTrack.TThumb.TStateStyles)
          protected
            function CreateDisabledStateStyle(const AParent: TObject): TALCustomTrack.TThumb.TDisabledStateStyle; override;
            function CreateHoveredStateStyle(const AParent: TObject): TALCustomTrack.TThumb.THoveredStateStyle; override;
            function CreatePressedStateStyle(const AParent: TObject): TALCustomTrack.TThumb.TPressedStateStyle; override;
            function CreateFocusedStateStyle(const AParent: TObject): TALCustomTrack.TThumb.TFocusedStateStyle; override;
          end;
      protected
        function CreateFill: TALBrush; override;
        function CreateStroke: TALStrokeBrush; override;
        function CreateStateStyles: TALCustomTrack.TThumb.TStateStyles; override;
        function GetDefaultXRadius: Single; override;
        function GetDefaultYRadius: Single; override;
      end;
  protected
    function GetDefaultSize: TSizeF; override;
    procedure AlignThumb; override;
    function CreateInactiveTrack(const AInactiveTrackClass: TALCustomTrack.TInactiveTrackClass = nil; Const AName: String = 'InactiveTrack'): TALCustomTrack.TInactiveTrack; override;
    function CreateActiveTrack(const AActiveTrackClass: TALCustomTrack.TActiveTrackClass = nil; Const AName: String = 'ActiveTrack'): TALCustomTrack.TActiveTrack; override;
    function CreateValueIndicator(const AValueIndicatorClass: TALCustomTrack.TValueIndicatorClass = nil; Const AName: String = 'ValueIndicator'): TALCustomTrack.TValueIndicator; override;
    function CreateThumb(const AThumbClass: TALCustomTrack.TThumbClass = nil; Const AName: String = 'Thumb'): TALCustomTrack.TThumb; override;
  public
    constructor Create(AOwner: TComponent); override;
    property CanFocus default False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALScrollBar = class(TALCustomScrollBar)
  published
    //property Action;
    property Align;
    property Anchors;
    //property AutoSize;
    property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property DoubleBuffered;
    property ClickSound;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
    //property DoubleBuffered;
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
    property Min;
    property Max;
    property Opacity;
    property Orientation;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Size;
    property TabOrder;
    property TabStop;
    property Thumb;
    property TouchTargetExpansion;
    property Value;
    property ViewportSize;
    property Visible;
    property Width;
    property OnCanFocus;
    property OnChange;
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

procedure Register;

implementation

uses
  System.SysUtils,
  system.Math.Vectors,
  {$IF defined(ALSkiaEngine)}
  System.Skia.API,
  FMX.Skia.Canvas,
  {$ENDIF}
  {$IFDEF ALDPK}
  DesignIntf,
  {$ENDIF}
  {$IF DEFINED(ANDROID)}
  Androidapi.JNI.GraphicsContentViewText,
  {$ENDIF}
  {$IF DEFINED(IOS) or DEFINED(ANDROID)}
  FMX.Canvas.GPU,
  Alcinoe.FMX.Types3D,
  {$ENDIF}
  {$IF DEFINED(IOS)}
  iOSapi.CoreGraphics,
  {$ENDIF}
  {$IF DEFINED(ALMacOS)}
  Macapi.CoreGraphics,
  {$ENDIF}
  FMX.Platform,
  fmx.consts,
  fmx.utils,
  Alcinoe.Localization,
  Alcinoe.StringUtils,
  Alcinoe.FMX.Styles;

{**}
Type
  _TALBaseStateStyleProtectedAccess = class(TALBaseStateStyle);
  _TALControlProtectedAccess = class(TALControl);

{***************************************************************************}
constructor TALAniIndicator.TAnimation.Create(const AOwner: TALAniIndicator);
begin
  inherited Create;
  FOwner := AOwner;
  FEnabled := True;
end;

{*************************************************************}
procedure TALAniIndicator.TAnimation.Assign(Source: TPersistent);
begin
  if Source is TALAniIndicator.TAnimation then begin
    inherited Assign(Source);
    Enabled := TALAniIndicator.TAnimation(Source).Enabled;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{*****************************************}
procedure TALAniIndicator.TAnimation.Start;
begin
  if (Running) then
    Exit;
  FEnabled := True;
  inherited Start;
end;

{*********************************************}
procedure TALAniIndicator.TAnimation.DoProcess;
begin
  inherited;
  if Enabled then Repaint;
end;

{*******************************************}
procedure TALAniIndicator.TAnimation.repaint;
begin
  if Fowner.IsDisplayed then
    Fowner.Repaint
  else if Loop then
    Pause;
end;

{*************************************************************}
function TALAniIndicator.TAnimation.GetDefaultDuration: Single;
begin
  Result := 1.0;
end;

{**********************************************************}
function TALAniIndicator.TAnimation.GetDefaultLoop: Boolean;
begin
  Result := True;
end;

{***************************************************************}
function TALAniIndicator.TAnimation.GetDefaultStartValue: Single;
begin
  Result := 0.0;
end;

{**************************************************************}
function TALAniIndicator.TAnimation.GetDefaultStopValue: Single;
begin
  Result := 1.0;
end;

{********************************************************************}
procedure TALAniIndicator.TAnimation.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then begin
    FEnabled := Value;
    if not FEnabled then
      inherited Enabled := False;
  end;
end;

{*****************************************************}
constructor TALAniIndicator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnimation := TAnimation.Create(Self);
  FResourceName := '';
  FTintColor := DefaultTintColor;
  FTintColorKey := DefaultTintColorKey;
  FFrameCount := 1;
  FRowCount := 1;
  FCacheIndex := 0;
  FCacheEngine := nil;
  FMotionMode := TMotionMode.Rotate;
  FBufDrawable := ALNullDrawable;
  SetAcceptsControls(False);
end;

{*********************************}
destructor TALAniIndicator.Destroy;
begin
  ALFreeAndNil(FAnimation);
  inherited;
end;

{******************************************}
procedure TALAniIndicator.BeforeDestruction;
begin
  if BeforeDestructionExecuted then exit;
  // Necessary if the control is destroyed using
  // AlFreeAndNil with the delayed flag
  FAnimation.Enabled := False;
  inherited;
end;

{*****************************************************************}
procedure TALAniIndicator.Assign(Source: TPersistent{TALControl});
begin
  BeginUpdate;
  Try
    if Source is TALAniIndicator then begin
      Animation.Assign(TALAniIndicator(Source).Animation);
      ResourceName := TALAniIndicator(Source).ResourceName;
      TintColorKey := TALAniIndicator(Source).TintColorKey;
      TintColor := TALAniIndicator(Source).TintColor;
      FrameCount := TALAniIndicator(Source).FrameCount;
      RowCount := TALAniIndicator(Source).RowCount;
      CacheIndex := TALAniIndicator(Source).CacheIndex;
      CacheEngine := TALAniIndicator(Source).CacheEngine;
      MotionMode := TALAniIndicator(Source).MotionMode;
    end
    else
      ALAssignError(Source{ASource}, Self{ADest});
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{*********************************************}
procedure TALAniIndicator.ApplyTintColorScheme;
begin
  if FTintColorKey <> '' then begin
    var LTintColor := TALStyleManager.Instance.GetColor(FTintColorKey);
    if FTintColor <> LTintColor then begin
      FTintColor := LTintColor;
      ClearBufDrawable;
      Repaint;
    end;
  end;
end;

{*****************************************}
procedure TALAniIndicator.ApplyColorScheme;
begin
  beginUpdate;
  try
    inherited;
    ApplyTintColorScheme;
  finally
    EndUpdate;
  end;
end;

{**********************************************}
function TALAniIndicator.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(36, 36);
end;

{********************************************************}
function TALAniIndicator.GetDefaultTintColor: TAlphaColor;
begin
  Result := TAlphaColors.null;
end;

{******************************************************}
function TALAniIndicator.GetDefaulttintColorKey: String;
begin
  Result := '';
end;

{**********************************}
procedure TALAniIndicator.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{*****************************************}
procedure TALAniIndicator.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (not ALIsDrawableNull(FBufDrawable)) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(FBufDrawable);
end;

{****************************************}
procedure TALAniIndicator.MakeBufDrawable;
begin

  if //--- Do not create BufDrawable if the size is 0
     (Size.Size.IsZero) or
     //--- Do not create BufDrawable if fResourceName is empty
     (FResourceName = '')
  then begin
    ClearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(FBufDrawable)) then exit;

  if (CacheIndex > 0) and
     (CacheEngine <> nil) and
     (CacheEngine.HasEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex})) then Exit;

  {$IFDEF debug}
  ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Width: ' + ALFloatToStrW(Width)+ ' | Height: ' + ALFloatToStrW(Height));
  {$endif}

  FBufDrawableRect := LocalRect;
  {$IFDEF ALDPK}
  if ALGetResourceFilename(FResourceName) = '' then
    fBufDrawable := ALNullDrawable
  else
  try
  {$ENDIF}
    fBufDrawable := ALCreateDrawableFromResource(
                      FResourceName, // const AResourceName: String;
                      nil, // const AResourceStream: TStream;
                      '', // const AMaskResourceName: String;
                      1, // const AScale: Single;
                      Width * (fframeCount div fRowCount) * ALGetScreenScale,
                      Height * fRowCount * ALGetScreenScale, // const W, H: single;
                      TALImageWrapMode.Fit, // const AWrapMode: TALImageWrapMode;
                      TpointF.Create(-50,-50), // const ACropCenter: TpointF;
                      FTintColor, // const ATintColor: TalphaColor;
                      0, // const ABlurRadius: single;
                      0, // const AXRadius: Single;
                      0); // const AYRadius: Single);
  {$IFDEF ALDPK}
  except
    fBufDrawable := ALNullDrawable;
  end;
  {$ENDIF}

end;

{******************************}
procedure TALAniIndicator.Paint;
begin

  if FAnimation.Enabled then begin
    if not TALFloatAnimation(FAnimation).Enabled then
      TALFloatAnimation(FAnimation).Enabled := True
    else
      FAnimation.Resume;
  end;

  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    var R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
  end;

  var LDrawable: TALDrawable;
  var LDrawableRect: TRectF;
  if (CacheIndex <= 0) or
     (CacheEngine = nil) or
     (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
    MakeBufDrawable;
    if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(fBufDrawable)) then begin
      if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, fBufDrawable{ADrawable}, fBufDrawableRect{ARect}) then ALFreeAndNilDrawable(fBufDrawable)
      else fBufDrawable := ALNullDrawable;
      if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
        raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
    end
    else begin
      LDrawable := FBufDrawable;
      LDrawableRect := FBufDrawableRect;
    end;
  end;

  if FMotionMode = TMotionMode.Frame then begin

    var LFrameFlatIndex := Round(FAnimation.CurrentValue * FrameCount) mod FrameCount;
    var LTotalFramesPerRow := FrameCount div RowCount;
    var LFrameIndex: TSmallPoint;
    LFrameIndex.X := LFrameFlatIndex mod LTotalFramesPerRow;
    LFrameIndex.Y := LFrameFlatIndex div LTotalFramesPerRow;

    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      LDrawable, // const ADrawable: TALDrawable;
      TRectF.Create(
        TPointF.Create(
          LFrameIndex.x * Width * ALGetScreenScale,
          LFrameIndex.Y * Height * ALGetScreenScale),
        Width * ALGetScreenScale,
        Height * ALGetScreenScale), // const ASrcRect: TrectF; // IN REAL PIXEL !
      LDrawableRect, // const ADestRect: TrectF; // IN virtual pixels !
      AbsoluteOpacity); // const AOpacity: Single);

  end
  else begin

    var LSavedMatrix := Canvas.Matrix;
    var LMatrixRotationCenter: TpointF;
    LMatrixRotationCenter.X := (Width / 2) + Canvas.Matrix.m31;
    LMatrixRotationCenter.Y := (Height / 2) + Canvas.Matrix.m32;
    var LMatrix := Canvas.Matrix;
    LMatrix := LMatrix * TMatrix.CreateTranslation(-LMatrixRotationCenter.X,-LMatrixRotationCenter.Y);
    LMatrix := LMatrix * TMatrix.CreateRotation(DegToRad(Fanimation.CurrentValue * 360));
    LMatrix := LMatrix * TMatrix.CreateTranslation(LMatrixRotationCenter.X,LMatrixRotationCenter.Y);
    Canvas.SetMatrix(LMatrix);
    try

      ALDrawDrawable(
        Canvas, // const ACanvas: Tcanvas;
        LDrawable, // const ADrawable: TALDrawable;
        LDrawableRect.TopLeft, // const ADstTopLeft: TpointF;
        AbsoluteOpacity); // const AOpacity: Single);

    finally
      Canvas.SetMatrix(LSavedMatrix);
    end;

  end;

end;

{*************************************************}
function TALAniIndicator.GetCacheSubIndex: Integer;
begin
  Result := 0;
end;

{**************************************************}
function TALAniIndicator.GetDoubleBuffered: boolean;
begin
  result := True;
end;

{***************************************************************}
procedure TALAniIndicator.setTintColor(const Value: TAlphaColor);
begin
  if FTintColor <> Value then begin
    FTintColor := Value;
    FTintColorKey := '';
    ClearBufDrawable;
    Repaint;
  end;
end;

{*************************************************************}
procedure TALAniIndicator.setTintColorKey(const Value: String);
begin
  if FTintColorKey <> Value then begin
    FTintColorKey := Value;
    ApplyTintColorScheme;
  end;
end;

{**************************************************************}
procedure TALAniIndicator.SetAnimation(const Value: TAnimation);
begin
  FAnimation.Assign(Value);
end;

{*************************************************************}
procedure TALAniIndicator.setResourceName(const Value: String);
begin
  if FResourceName <> Value then begin
    ClearBufDrawable;
    FResourceName := Value;
    Repaint;
  end;
end;

{****************************************************************}
procedure TALAniIndicator.SetMotionMode(const Value: TMotionMode);
begin
  if FMotionMode <> Value then begin
    FMotionMode := Value;
    Repaint;
  end;
end;

{************************************************************}
procedure TALAniIndicator.SetFrameCount(const Value: Integer);
begin
  if FFrameCount <> Value then begin
    ClearBufDrawable;
    FFrameCount := Value;
    Repaint;
  end;
end;

{**********************************************************}
procedure TALAniIndicator.SetRowCount(const Value: Integer);
begin
  if FRowCount <> Value then begin
    ClearBufDrawable;
    FRowCount := Value;
    Repaint;
  end;
end;

{**************************************************}
function TALAniIndicator.IsTintColorStored: Boolean;
begin
  Result := FTintColor <> DefaultTintColor;
end;

{*****************************************************}
function TALAniIndicator.IsTintColorKeyStored: Boolean;
begin
  Result := FTintColorKey <> DefaultTintColorKey;
end;

{******************************************************************}
function TALCustomTrack.TThumb.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := $ffd5d5d5;
end;

{**********************************************************************************}
function TALCustomTrack.TThumb.TBaseStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := $ffd5d5d5;
end;

{***********************************************************************************}
function TALCustomTrack.TThumb.TBaseStateStyle.TStateLayer.GetDefaultXRadius: Single;
begin
  Result := -50;
end;

{***********************************************************************************}
function TALCustomTrack.TThumb.TBaseStateStyle.TStateLayer.GetDefaultYRadius: Single;
begin
  Result := -50;
end;

{****************************************************************************************************************}
function TALCustomTrack.TThumb.TBaseStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{*****************************************************************************}
function TALCustomTrack.TThumb.TBaseStateStyle.CreateStateLayer: TALStateLayer;
begin
  Result := TStateLayer.Create;
end;

{**************************************************************************}
function TALCustomTrack.TThumb.TDisabledStateStyle.IsOpacityStored: Boolean;
begin
  Result := not SameValue(FOpacity, TControl.DefaultDisabledOpacity, TEpsilon.Scale);
end;

{**********************************************************************************}
procedure TALCustomTrack.TThumb.TDisabledStateStyle.SetOpacity(const Value: Single);
begin
  if not SameValue(FOpacity, Value, TEpsilon.Scale) then begin
    FOpacity := Value;
    Change;
  end;
end;

{***********************************************************************************}
constructor TALCustomTrack.TThumb.TDisabledStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  FOpacity := TControl.DefaultDisabledOpacity;
end;

{******************************************************************************}
procedure TALCustomTrack.TThumb.TDisabledStateStyle.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TDisabledStateStyle then
      Opacity := TDisabledStateStyle(Source).Opacity
    else
      Opacity := TControl.DefaultDisabledOpacity;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{********************************************************}
procedure TALCustomTrack.TThumb.TDisabledStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Opacity := TControl.DefaultDisabledOpacity;
  finally
    EndUpdate;
  end;
end;

{*********************************************************************}
function TALCustomTrack.TThumb.TDisabledStateStyle.GetInherit: Boolean;
begin
  // Opacity is not part of the GetInherit function because it updates the
  // disabledOpacity of the base control immediately every time it changes.
  // Essentially, it acts merely as a link to the disabledOpacity of the base control.
  Result := inherited GetInherit;
end;

{*******************************************************************************}
constructor TALCustomTrack.TThumb.TStateStyles.Create(const AParent: TALControl);
begin
  inherited Create(AParent);
  //--
  FDisabled := CreateDisabledStateStyle(AParent);
  FDisabled.OnChanged := DisabledChanged;
  //--
  FHovered := CreateHoveredStateStyle(AParent);
  FHovered.OnChanged := HoveredChanged;
  //--
  FPressed := CreatePressedStateStyle(AParent);
  FPressed.OnChanged := PressedChanged;
  //--
  FFocused := CreateFocusedStateStyle(AParent);
  FFocused.OnChanged := FocusedChanged;
end;

{****************************************************}
destructor TALCustomTrack.TThumb.TStateStyles.Destroy;
begin
  ALFreeAndNil(FDisabled);
  ALFreeAndNil(FHovered);
  ALFreeAndNil(FPressed);
  ALFreeAndNil(FFocused);
  inherited Destroy;
end;

{****************************************************************************************************************}
function TALCustomTrack.TThumb.TStateStyles.CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle;
begin
  Result := TDisabledStateStyle.Create(AParent);
end;

{**************************************************************************************************************}
function TALCustomTrack.TThumb.TStateStyles.CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle;
begin
  Result := THoveredStateStyle.Create(AParent);
end;

{**************************************************************************************************************}
function TALCustomTrack.TThumb.TStateStyles.CreatePressedStateStyle(const AParent: TObject): TPressedStateStyle;
begin
  Result := TPressedStateStyle.Create(AParent);
end;

{**************************************************************************************************************}
function TALCustomTrack.TThumb.TStateStyles.CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle;
begin
  Result := TFocusedStateStyle.Create(AParent);
end;

{***********************************************************************}
procedure TALCustomTrack.TThumb.TStateStyles.Assign(Source: TPersistent);
begin
  if Source is TStateStyles then begin
    BeginUpdate;
    Try
      Disabled.Assign(TStateStyles(Source).Disabled);
      Hovered.Assign(TStateStyles(Source).Hovered);
      Pressed.Assign(TStateStyles(Source).Pressed);
      Focused.Assign(TStateStyles(Source).Focused);
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{*************************************************}
procedure TALCustomTrack.TThumb.TStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Disabled.reset;
    Hovered.reset;
    Pressed.reset;
    Focused.reset;
  finally
    EndUpdate;
  end;
end;

{********************************************************}
procedure TALCustomTrack.TThumb.TStateStyles.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    Disabled.AlignToPixel;
    Hovered.AlignToPixel;
    Pressed.AlignToPixel;
    Focused.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{************************************************************}
procedure TALCustomTrack.TThumb.TStateStyles.ApplyColorScheme;
begin
  BeginUpdate;
  Try
    inherited;
    Disabled.ApplyColorScheme;
    Hovered.ApplyColorScheme;
    Pressed.ApplyColorScheme;
    Focused.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{************************************************************}
procedure TALCustomTrack.TThumb.TStateStyles.ClearBufDrawable;
begin
  inherited;
  Disabled.ClearBufDrawable;
  Hovered.ClearBufDrawable;
  Pressed.ClearBufDrawable;
  Focused.ClearBufDrawable;
end;

{********************************************************************************}
function TALCustomTrack.TThumb.TStateStyles.GetCurrentRawStyle: TALBaseStateStyle;
begin
  if Not Parent.Enabled then Result := Disabled
  else if Parent.Pressed then Result := Pressed
  else if Parent.IsFocused then Result := Focused
  else if Parent.IsMouseOver then Result := Hovered
  else result := nil;
end;

{***************************************************************************}
function TALCustomTrack.TThumb.TStateStyles.GetParent: TALCustomTrack.TThumb;
begin
  Result := TALCustomTrack.TThumb(inherited Parent);
end;

{******************************************************************************************}
procedure TALCustomTrack.TThumb.TStateStyles.SetDisabled(const AValue: TDisabledStateStyle);
begin
  FDisabled.Assign(AValue);
end;

{****************************************************************************************}
procedure TALCustomTrack.TThumb.TStateStyles.SetHovered(const AValue: THoveredStateStyle);
begin
  FHovered.Assign(AValue);
end;

{****************************************************************************************}
procedure TALCustomTrack.TThumb.TStateStyles.SetPressed(const AValue: TPressedStateStyle);
begin
  FPressed.Assign(AValue);
end;

{****************************************************************************************}
procedure TALCustomTrack.TThumb.TStateStyles.SetFocused(const AValue: TFocusedStateStyle);
begin
  FFocused.Assign(AValue);
end;

{*****************************************************************************}
procedure TALCustomTrack.TThumb.TStateStyles.DisabledChanged(ASender: TObject);
begin
  Change;
end;

{****************************************************************************}
procedure TALCustomTrack.TThumb.TStateStyles.HoveredChanged(ASender: TObject);
begin
  Change;
end;

{****************************************************************************}
procedure TALCustomTrack.TThumb.TStateStyles.PressedChanged(ASender: TObject);
begin
  Change;
end;

{****************************************************************************}
procedure TALCustomTrack.TThumb.TStateStyles.FocusedChanged(ASender: TObject);
begin
  Change;
end;

{***************************************************************************}
constructor TALCustomTrack.TThumb.Create(const ACustomTrack: TALCustomTrack);
begin
  FStateStyles := nil;
  //--
  inherited create(ACustomTrack);
  FCustomTrack := ACustomTrack;
  //--
  CanFocus := ACustomTrack.CanFocus;
  TabStop := ACustomTrack.TabStop;
  cursor := crHandPoint;
  AutoCapture := True;
  Locked := True;
  //--
  FValueRange := TValueRange.create(self);
  {$IFDEF debug}
  if (FValueRange.Min <> 0) or
     (FValueRange.Max <> FMX.StdActns.DefaultMaxValue) or
     (FValueRange.Value <> 0) or
     (FValueRange.ViewportSize <> 0) or
     (FValueRange.Frequency <> 0) then
    Raise Exception.Create('Error 577E0A1F-9305-475A-AD94-AE60E257C8D2');
  {$ENDIF}
  FValueRange.onchanged := ValueRangeChanged;
  //--
  fCustomTrackMouseDownPos := TpointF.Zero;
  fScrollCapturedByMe := False;
  TMessageManager.DefaultManager.SubscribeToMessage(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
  //--
  FStateStyles := CreateStateStyles;
  FStateStyles.OnChanged := StateStylesChanged;
end;

{***************************************}
destructor TALCustomTrack.TThumb.Destroy;
begin
  ALFreeAndNil(FStateStyles);
  ALFreeAndNil(FValueRange);
  inherited;
end;

{************************************************}
procedure TALCustomTrack.TThumb.BeforeDestruction;
begin
  if BeforeDestructionExecuted then exit;
  // Unsubscribe from TALScrollCapturedMessage to stop receiving messages.
  // This must be done in BeforeDestruction rather than in Destroy,
  // because the control might be freed in the background via ALFreeAndNil(..., delayed),
  // and BeforeDestruction is guaranteed to execute on the main thread.
  TMessageManager.DefaultManager.Unsubscribe(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
  inherited;
end;

{**********************************************************************}
procedure TALCustomTrack.TThumb.Assign(Source: TPersistent{TALControl});
begin
  BeginUpdate;
  Try
    if Source is TThumb then begin
      StateStyles.Assign(TThumb(Source).StateStyles);
    end
    else
      ALAssignError(Source{ASource}, Self{ADest});
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{**********************************************************}
function TALCustomTrack.TThumb.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
end;

{*************************************************************}
function TALCustomTrack.TThumb.CreateStateStyles: TStateStyles;
begin
  Result := TStateStyles.Create(self);
end;

{********************************************}
procedure TALCustomTrack.TThumb.DoBeginUpdate;
begin
  fValueRange.BeginUpdate;
  inherited;
end;

{******************************************}
procedure TALCustomTrack.TThumb.DoEndUpdate;
begin
  fValueRange.EndUpdate;
  inherited;
end;

{*******************************************}
procedure TALCustomTrack.TThumb.AlignToPixel;
begin
  BeginUpdate;
  try
    inherited;
    StateStyles.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{***********************************************}
procedure TALCustomTrack.TThumb.ApplyColorScheme;
begin
  BeginUpdate;
  try
    inherited;
    StateStyles.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{**********************************************}
function TALCustomTrack.TThumb.GetValue: Double;
begin
  Result := FValueRange.Value;
end;

{*************************************************************************}
procedure TALCustomTrack.TThumb.SetStateStyles(const AValue: TStateStyles);
begin
  FStateStyles.Assign(AValue);
end;

{*******************************************************}
function TALCustomTrack.TThumb.GetDefaultXRadius: Single;
begin
  result := -50;
end;

{*******************************************************}
function TALCustomTrack.TThumb.GetDefaultYRadius: Single;
begin
  result := -50;
end;

{******************************************************************}
procedure TALCustomTrack.TThumb.StateStylesChanged(Sender: TObject);
begin
  ClearBufDrawable;
  DisabledOpacity := StateStyles.Disabled.opacity;
  Repaint;
end;

{*************************************************}
procedure TALCustomTrack.TThumb.IsMouseOverChanged;
begin
  inherited;
  StateStyles.Transition.start;
  if FcustomTrack.FValueIndicator <> nil then
    FcustomTrack.FValueIndicator.Refresh(Self);
  repaint;
end;

{***********************************************}
procedure TALCustomTrack.TThumb.IsFocusedChanged;
begin
  inherited;
  StateStyles.Transition.start;
  if FcustomTrack.FValueIndicator <> nil then
    FcustomTrack.FValueIndicator.Refresh(Self);
  repaint;
end;

{*********************************************}
procedure TALCustomTrack.TThumb.PressedChanged;
begin
  inherited;
  StateStyles.Transition.start;
  if FcustomTrack.FValueIndicator <> nil then
    FcustomTrack.FValueIndicator.Refresh(Self);
  repaint;
end;

{*****************************************************************}
procedure TALCustomTrack.TThumb.ValueRangeChanged(Sender: TObject);
begin
  FcustomTrack.Realign;
  if FcustomTrack.FValueIndicator <> nil then
    FcustomTrack.FValueIndicator.Refresh(Self);
  FcustomTrack.DoChanged;
end;

{*******************************************************************************************************}
procedure TALCustomTrack.TThumb.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
begin
  var LInc: Double := fValueRange.Frequency;
  if LInc = 0 then LInc := 1;
  inherited;
  var LValue: Double;
  case Key of
    vkHome: LValue := fValueRange.Min;
    vkEnd: LValue := fValueRange.Max;
    vkUp: LValue := fValueRange.Value - LInc;
    vkDown: LValue := fValueRange.Value + LInc;
    vkLeft: LValue := fValueRange.Value - LInc;
    vkRight: LValue := fValueRange.Value + LInc;
    else Exit;
  end;
  Key := 0;
  fValueRange.Value := LValue;
end;

{*****************************************************************************************************}
procedure TALCustomTrack.TThumb.ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
begin
  if (Sender = self) then exit;
  {$IFDEF DEBUG}
  //ALLog(
  //  'TALCustomTrack.TThumb.ScrollCapturedByOtherHandler',
  //  'Captured: ' + ALBoolToStrW(TALScrollCapturedMessage(M).Captured)+ ' | ' +
  //  'Pressed: ' + ALBoolToStrW(Pressed));
  {$ENDIF}
  if TALScrollCapturedMessage(M).Captured then begin
    {$IFDEF DEBUG}
    if fScrollCapturedByMe then
      raise Exception.Create('Error 40ED19CA-9F47-4A56-AC46-FA5D8D5429C0');
    {$ENDIF}
    Pressed := False;
  end;
end;

{************************************************************************************************}
procedure TALCustomTrack.TThumb.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Pressed then begin
    BringToFront;
    fCustomTrackMouseDownPos := FCustomTrack.AbsoluteToLocal(LocalToAbsolute(PressedPosition));
  end;
end;

{**************************************************************************}
procedure TALCustomTrack.TThumb.MouseMove(Shift: TShiftState; X, Y: Single);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _PosToValue(MinValue, MaxValue, ViewportSize: Double; TrackSize, Pos: Single): Double;
  begin
    Result := MinValue;
    if ViewportSize < 0 then ViewportSize := 0;
    var LValRel: Double := TrackSize;
    if LValRel > 0 then begin
      LValRel := Pos / LValRel;
      if LValRel < 0 then LValRel := 0;
      if LValRel > 1 then LValRel := 1;
      Result := MinValue + LValRel * (MaxValue - MinValue - ViewportSize);
    end;
  end;

begin
  {$IFDEF DEBUG}
  //ALLog(
  //  'TALCustomTrack.MouseMove',
  //  'Position:' + ALFormatFloatW('0.##', x) + ',' + ALFormatFloatW('0.##', y));
  {$ENDIF}
  if Pressed then begin

    if (not fScrollCapturedByMe) then begin
      var LCustomTrackMousePos := FCustomTrack.AbsoluteToLocal(LocalToAbsolute(TpointF.Create(X,Y)));
      If ((FCustomtrack.Orientation = TOrientation.Horizontal) and
          (abs(FCustomTrackMouseDownPos.x - LCustomTrackMousePos.x) > abs(FCustomTrackMouseDownPos.y - LCustomTrackMousePos.y)) and
          (abs(FCustomTrackMouseDownPos.x - LCustomTrackMousePos.x) > TALScrollEngine.DefaultTouchSlop))
         or
         ((FCustomtrack.Orientation = TOrientation.Vertical) and
          (abs(FCustomTrackMouseDownPos.y - LCustomTrackMousePos.y) > abs(FCustomTrackMouseDownPos.x - LCustomTrackMousePos.x)) and
          (abs(FCustomTrackMouseDownPos.y - LCustomTrackMousePos.y) > TALScrollEngine.DefaultTouchSlop)) then begin
        {$IFDEF DEBUG}
        //ALLog(
        //  'TALCustomTrack.MouseMove',
        //  'ScrollCapturedByMe');
        {$ENDIF}
        PressedPosition := PointF(X, Y);
        fCustomTrackMouseDownPos := LCustomTrackMousePos;
        fScrollCapturedByMe := true;
        TMessageManager.DefaultManager.SendMessage(self, TALScrollCapturedMessage.Create(true), True);
      end;
    end;

    if fScrollCapturedByMe then begin
      if FCustomTrack.Orientation = TOrientation.Horizontal then begin
        var P := FCustomTrack.AbsoluteToLocal(LocalToAbsolute(TPointF.Create(X - PressedPosition.X, 0)));
        FValueRange.Value := _PosToValue(
                               FCustomTrack.Min, // MinValue
                               FCustomTrack.Max, // MaxValue
                               FCustomTrack.ViewportSize, // ViewportSize
                               FCustomTrack.GetTrackSize, // TrackSize
                               P.X - FCustomTrack.padding.Left - FCustomTrack.GetLeadingTrackStartPadding); // Pos
      end
      else begin
        var P := FCustomTrack.AbsoluteToLocal(LocalToAbsolute(TPointF.Create(0, Y - PressedPosition.Y)));
        FValueRange.Value := _PosToValue(
                               FCustomTrack.Min, // MinValue
                               FCustomTrack.Max, // MaxValue
                               FCustomTrack.ViewportSize, // ViewportSize
                               FCustomTrack.GetTrackSize, // TrackSize
                               P.Y - FCustomTrack.padding.Top - FCustomTrack.GetLeadingTrackStartPadding); // Pos
      end;
    end;

  end;
  inherited;
end;

{**********************************************************************************************}
procedure TALCustomTrack.TThumb.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if FScrollCapturedByMe then
    FScrollCapturedByMe := False;
end;

{*******************************************}
procedure TALCustomTrack.TThumb.DoMouseLeave;
begin
  inherited;
  if FScrollCapturedByMe then
    FScrollCapturedByMe := False;
end;

{***********************************************}
procedure TALCustomTrack.TThumb.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (FStateStyles <> nil) and
     (not (csDestroying in ComponentState)) and
     (ALIsDrawableNull(FBufDrawable)) and // warn will be raise in inherited
     ((not ALIsDrawableNull(FStateStyles.Disabled.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Hovered.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Pressed.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Focused.FBufDrawable))) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  if FStateStyles <> nil then
    FStateStyles.ClearBufDrawable;
  inherited ClearBufDrawable;
end;

{**********************************************}
procedure TALCustomTrack.TThumb.MakeBufDrawable;
begin
  //--- Do not create BufDrawable if not DoubleBuffered
  if {$IF not DEFINED(ALDPK)}(not DoubleBuffered){$ELSE}False{$ENDIF} then begin
    ClearBufDrawable;
    exit;
  end;
  //--
  inherited MakeBufDrawable;
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if LStateStyle.Inherit then exit;
  if (not ALIsDrawableNull(LStateStyle.FBufDrawable)) then exit;
  LStateStyle.SupersedeNoChanges(true{ASaveState});
  try

    {$IFDEF debug}
    ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Style: ' + LStateStyle.ClassName + ' | Width: ' + ALFloatToStrW(Width)+ ' | Height: ' + ALFloatToStrW(Height));
    {$endif}

    // Create the BufDrawable
    CreateBufDrawable(
      LStateStyle.FBufDrawable, // var ABufDrawable: TALDrawable;
      LStateStyle.FBufDrawableRect, // var ABufDrawableRect: TRectF;
      ALGetScreenScale * LStateStyle.Scale, // const AScale: Single;
      LStateStyle.Fill, // const AFill: TALBrush;
      LStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
      TAlphaColors.null, // const AStateLayerContentColor: TAlphaColor;
      False, // const ADrawStateLayerOnTop: Boolean;
      LStateStyle.Stroke, // const AStroke: TALStrokeBrush;
      LStateStyle.Shadow); // const AShadow: TALShadow);

    // LStateStyle.FBufDrawableRect must include the LStateStyle.Scale
    LStateStyle.FBufDrawableRect.Top := LStateStyle.FBufDrawableRect.Top * LStateStyle.Scale;
    LStateStyle.FBufDrawableRect.right := LStateStyle.FBufDrawableRect.right * LStateStyle.Scale;
    LStateStyle.FBufDrawableRect.left := LStateStyle.FBufDrawableRect.left * LStateStyle.Scale;
    LStateStyle.FBufDrawableRect.bottom := LStateStyle.FBufDrawableRect.bottom * LStateStyle.Scale;

    // Since LStateStyle.FBufDrawableRect can have different dimensions than the main BufDrawableRect
    // (due to scale), we must center LStateStyle.FBufDrawableRect
    // within the main BufDrawableRect to ensure that all changes are visually centered.
    var LMainDrawableRect: TRectF;
    If AlIsDrawableNull(FBufDrawable) then LMainDrawableRect := LocalRect
    else LMainDrawableRect := FBufDrawableRect;
    LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
    var LCenteredRect := LStateStyle.FBufDrawableRect.CenterAt(LMainDrawableRect);
    LStateStyle.FBufDrawableRect.Offset(LCenteredRect.Left, LCenteredRect.top);

  finally
    LStateStyle.RestorestateNoChanges;
  end;
end;

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
function TALCustomTrack.TThumb.GetRenderTargetRect(const ARect: TrectF): TRectF;
begin
  if StateStyles.Transition.Running then begin
    Result := ARect;
    if StateStyles.Transition.FromStateStyle <> nil then begin
      var LFromSurfaceRect := ALGetShapeSurfaceRect(
                                ARect, // const ARect: TRectF;
                                AutoAlignToPixel, // const AAlignToPixel: Boolean;
                                _TALBaseStateStyleProtectedAccess(StateStyles.Transition.FromStateStyle).Fill, // const AFill: TALBrush;
                                nil, // const AFillResourceStream: TStream;
                                _TALBaseStateStyleProtectedAccess(StateStyles.Transition.FromStateStyle).StateLayer, // const AStateLayer: TALStateLayer;
                                _TALBaseStateStyleProtectedAccess(StateStyles.Transition.FromStateStyle).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LFromSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
    if StateStyles.Transition.ToStateStyle <> nil then begin
      var LToSurfaceRect := ALGetShapeSurfaceRect(
                              ARect, // const ARect: TRectF;
                              AutoAlignToPixel, // const AAlignToPixel: Boolean;
                              _TALBaseStateStyleProtectedAccess(StateStyles.Transition.ToStateStyle).Fill, // const AFill: TALBrush;
                              nil, // const AFillResourceStream: TStream;
                              _TALBaseStateStyleProtectedAccess(StateStyles.Transition.ToStateStyle).StateLayer, // const AStateLayer: TALStateLayer;
                              _TALBaseStateStyleProtectedAccess(StateStyles.Transition.ToStateStyle).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LToSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
  end
  else begin
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  AutoAlignToPixel, // const AAlignToPixel: Boolean;
                  LStateStyle.Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  LStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
                  LStateStyle.Shadow); // const AShadow: TALShadow): TRectF;
    end
    else begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  AutoAlignToPixel, // const AAlignToPixel: Boolean;
                  Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  nil, // const AStateLayer: TALStateLayer;
                  Shadow); // const AShadow: TALShadow): TRectF;
    end;
  end;
end;
{$ENDIF}

{************************************}
procedure TALCustomTrack.TThumb.Paint;
begin

  StateStyles.UpdateLastPaintedRawStyle;
  MakeBufDrawable;

  var LDrawable: TALDrawable;
  var LDrawableRect: TRectF;
  if StateStyles.Transition.Running then begin
    LDrawable := ALNullDrawable;
    LDrawableRect := TRectF.Empty;
  end
  else begin
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      LDrawable := LStateStyle.FBufDrawable;
      LDrawableRect := LStateStyle.FBufDrawableRect;
      if ALIsDrawableNull(LDrawable) then begin
        LDrawable := FBufDrawable;
        LDrawableRect := FBufDrawableRect;
      end;
    end
    else begin
      LDrawable := FBufDrawable;
      LDrawableRect := FBufDrawableRect;
    end;
  end;

  if ALIsDrawableNull(LDrawable) then begin

    var LCurrentAdjustedStateStyle := TBaseStateStyle(StateStyles.GetCurrentAdjustedStyle);
    if LCurrentAdjustedStateStyle = nil then begin
      inherited Paint;
      exit;
    end;

    {$IF DEFINED(ALSkiaCanvas)}

    // Using a matrix on the canvas results in smoother animations compared to using
    // Ascale with DrawMultilineText. This is because changes in scale affect the font size,
    // leading to rounding issues (I spent many hours looking for a way to avoid this).
    // If there is an animation, it appears jerky because the text position
    // shifts up or down with scale changes due to pixel alignment.
    var LCanvasSaveState: TCanvasSaveState := ALScaleAndCenterCanvas(
                                                Canvas, // Const ACanvas: TCanvas;
                                                AbsoluteRect, // Const AAbsoluteRect: TRectF;
                                                LCurrentAdjustedStateStyle.Scale, // Const AScale: Single;
                                                true); // Const ASaveState: Boolean);
    try

      TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
        .SetAlignToPixel(AutoAlignToPixel)
        .SetDstRect(LocalRect)
        .SetOpacity(AbsoluteOpacity)
        .SetFill(LCurrentAdjustedStateStyle.Fill)
        .SetStateLayer(LCurrentAdjustedStateStyle.StateLayer, TalphaColors.Null)
        .SetStroke(LCurrentAdjustedStateStyle.Stroke)
        .SetShadow(LCurrentAdjustedStateStyle.Shadow)
        .SetSides(Sides)
        .SetCorners(Corners)
        .SetXRadius(XRadius)
        .SetYRadius(YRadius)
        .Draw;

    finally
      if LCanvasSaveState <> nil then
        Canvas.RestoreState(LCanvasSaveState);
    end;

    {$ELSE}

    var LRect := LocalRect;
    InitRenderTargets(LRect);
    if ALCanvasBeginScene(RenderTargetCanvas) then
    try

      ALClearCanvas(RenderTargetCanvas, TAlphaColors.Null);

      TALDrawRectangleHelper.Create(RenderTargetCanvas)
        .SetScale(ALGetScreenScale)
        .SetAlignToPixel(AutoAlignToPixel)
        .SetDstRect(LRect)
        .SetOpacity(AbsoluteOpacity)
        .SetFill(LCurrentAdjustedStateStyle.Fill)
        .SetStateLayer(LCurrentAdjustedStateStyle.StateLayer, TalphaColors.Null)
        .SetStroke(LCurrentAdjustedStateStyle.Stroke)
        .SetShadow(LCurrentAdjustedStateStyle.Shadow)
        .SetSides(Sides)
        .SetCorners(Corners)
        .SetXRadius(XRadius)
        .SetYRadius(YRadius)
        .Draw;

    finally
      ALCanvasEndScene(RenderTargetCanvas)
    end;

    ALUpdateDrawableFromSurface(RenderTargetSurface, RenderTargetDrawable);

    // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
    // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
    LRect.Offset(-2*LRect.Left, -2*LRect.Top);

    // LRect must include the LScale
    LRect.Top := LRect.Top * LCurrentAdjustedStateStyle.Scale;
    LRect.right := LRect.right * LCurrentAdjustedStateStyle.Scale;
    LRect.left := LRect.left * LCurrentAdjustedStateStyle.Scale;
    LRect.bottom := LRect.bottom * LCurrentAdjustedStateStyle.Scale;

    // Since LStateStyle.FBufDrawableRect can have different dimensions than the main BufDrawableRect
    // (due to autosizing with different font sizes), we must center LStateStyle.FBufDrawableRect
    // within the main BufDrawableRect to ensure that all changes are visually centered.
    var LMainDrawableRect: TRectF;
    If AlIsDrawableNull(FBufDrawable) then LMainDrawableRect := LocalRect
    else LMainDrawableRect := FBufDrawableRect;
    LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
    var LCenteredRect := LRect.CenterAt(LMainDrawableRect);
    LRect.Offset(LCenteredRect.Left, LCenteredRect.top);

    // We cannot use the matrix because, if we do, ALAlignToPixelRound in ALDrawDrawable
    // will be ineffective since the matrix will no longer be a simple translation matrix.
    // In such a case, TCustomCanvasGpu(ACanvas).DrawTexture may produce border artifacts
    // if the texture is not perfectly pixel-aligned.
    var LDstRect := TRectF.Create(0, 0, ALGetDrawableWidth(RenderTargetDrawable), ALGetDrawableHeight(RenderTargetDrawable));
    LDstRect.Width := (LDstRect.Width / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
    LDstRect.height := (LDstRect.height / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
    LDstRect.SetLocation(
      LRect.Left,
      LRect.Top);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      RenderTargetDrawable, // const ADrawable: TALDrawable;
      LDstRect, // const ADstRect: TrectF; // IN Virtual pixels !
      AbsoluteOpacity); // const AOpacity: Single)

    {$ENDIF}

    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{*************************************************************************}
function TALCustomTrack.TValueIndicator.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphacolors.Black;
end;

{****************************************************************************************}
function TALCustomTrack.TValueIndicator.TTextSettings.TFont.GetDefaultWeight: TFontWeight;
begin
  Result := TFontWeight.medium;
end;

{***************************************************************************************}
function TALCustomTrack.TValueIndicator.TTextSettings.TFont.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.White;
end;

{************************************************************************}
function TALCustomTrack.TValueIndicator.TTextSettings.CreateFont: TALFont;
begin
  Result := TFont.Create;
end;

{******************************************************************************************}
function TALCustomTrack.TValueIndicator.TTextSettings.GetDefaultHorzAlign: TALTextHorzAlign;
begin
  Result := TALTextHorzAlign.center;
end;

{************************************************************************************}
constructor TALCustomTrack.TValueIndicator.Create(const ACustomTrack: TALCustomTrack);
begin
  inherited create(ACustomTrack);
  AutoSize := TALAutoSizeMode.Both;
  Visible := False;
  Pivot.Point := TPointF.Create(0.5,1);
  FCustomTrack := ACustomTrack;
  FFormat := DefaultFormat;
  FOnCustomFormat := nil;
  FAnimation := TAnimation.ScaleInOut;
  FShowOnInteraction := False;
  //--
  FFloatAnimation := TALFloatAnimation.Create;
  FFloatAnimation.StartValue := 0;
  FFloatAnimation.StopValue := 1;
  FFloatAnimation.Duration := 0.2;
  FFloatAnimation.InterpolationType := TALInterpolationType.cubic;
  FFloatAnimation.InterpolationMode := TALInterpolationMode.out;
  FFloatAnimation.OnProcess := AnimationProcess;
  FFloatAnimation.OnFinish := AnimationFinish;
  //--
  var LMarginsChange: TNotifyEvent := Margins.OnChange;
  Margins.OnChange := nil;
  Margins.DefaultValue := TRectF.create(6{Left}, 4{Top}, 6{Right}, 4{Bottom});
  Margins.Rect := Margins.DefaultValue;
  Margins.OnChange := LMarginsChange;
  //--
  var LPaddingChange: TNotifyEvent := Padding.OnChange;
  Padding.OnChange := nil;
  Padding.DefaultValue := TRectF.create(16{Left}, 12{Top}, 16{Right}, 12{Bottom});
  Padding.Rect := Padding.DefaultValue;
  padding.OnChange := LPaddingChange;
end;

{************************************************}
destructor TALCustomTrack.TValueIndicator.Destroy;
begin
  ALFreeAndNil(FFloatAnimation);
  inherited;
end;

{*******************************************************************************}
procedure TALCustomTrack.TValueIndicator.Assign(Source: TPersistent{TALControl});
begin
  BeginUpdate;
  Try
    if Source is TValueIndicator then begin
      Format := TValueIndicator(Source).Format;
      OnCustomFormat := TValueIndicator(Source).OnCustomFormat;
      Animation := TValueIndicator(Source).Animation;
      ShowOnInteraction := TValueIndicator(Source).ShowOnInteraction;
    end
    else
      ALAssignError(Source{ASource}, Self{ADest});
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{****************************************************************************}
procedure TALCustomTrack.TValueIndicator.AdjustPosition(const AThumb: TThumb);
begin
  if FCustomTrack.Orientation = TOrientation.Horizontal then begin
    Position.Point := TpointF.Create(
                        AThumb.Position.X - ((Width - AThumb.Width) / 2),
                        AThumb.Position.Y - Height - Margins.Bottom);
  end
  else begin
    Position.Point := TpointF.Create(
                        AThumb.Position.X + AThumb.Width + Margins.Left,
                        AThumb.Position.Y - ((Height - AThumb.Height) / 2));
  end;
end;

{*********************************************************************}
procedure TALCustomTrack.TValueIndicator.Refresh(const AThumb: TThumb);
begin
  if not FShowOnInteraction then exit;
  If AThumb.IsFocused or Athumb.IsMouseOver or AThumb.Pressed then begin
    if assigned(OnCustomFormat) then begin
      var LText: String;
      OnCustomFormat(Self, AThumb.GetValue, LText);
      Text := LText;
    end
    else
      Text := ALFormatFloatW(Format, AThumb.GetValue, FormatSettings);
    if FFloatAnimation.TagObject <> AThumb then begin
      FFloatAnimation.Enabled := False;
      visible := False;
    end;
    FFloatAnimation.TagObject := AThumb;
    if not visible then begin
      case FAnimation of
        TAnimation.None: begin
          AdjustPosition(AThumb);
          Visible := True;
          exit;
        end;
        TAnimation.ScaleInOut: begin
          AdjustPosition(AThumb);
          Opacity := 1;
          Scale.Point := TPointF.Create(0, 0);
        end;
        TAnimation.Opacity: begin
          AdjustPosition(AThumb);
          Opacity := 0;
          Scale.Point := TPointF.Create(1, 1);
        end
        else
          Raise Exception.Create('Error A2F4F658-97FC-4F92-AFA4-3BB8192003A8')
      end;
      Visible := True;
      FFloatAnimation.StopAtCurrent;
      FFloatAnimation.Inverse := False;
      FFloatAnimation.Start;
    end
    else if (FFloatAnimation.Running) and
            (FFloatAnimation.Inverse) then begin
      FFloatAnimation.Inverse := False;
    end
    else begin
      AdjustPosition(AThumb);
    end;
  end
  else begin
    if FFloatAnimation.TagObject <> AThumb then begin
      FFloatAnimation.Enabled := False;
      visible := False;
    end;
    FFloatAnimation.TagObject := AThumb;
    If Visible then begin
      if (not FFloatAnimation.Running) then begin
        FFloatAnimation.Inverse := true;
        FFloatAnimation.Start;
      end
      else if (not FFloatAnimation.Inverse) then begin
        FFloatAnimation.Inverse := True;
      end;
    end;
  end;
end;

{*************************************************************************}
procedure TALCustomTrack.TValueIndicator.AnimationProcess(Sender: TObject);
begin
  case FAnimation of
    TAnimation.ScaleInOut: Scale.Point := TPointF.Create(FFloatAnimation.CurrentValue, FFloatAnimation.CurrentValue);
    TAnimation.Opacity: Opacity := FFloatAnimation.CurrentValue
    else Raise Exception.Create('Error D6F17D76-E47E-4144-8FBA-5CAD3EBF84F3')
  end;
end;

{************************************************************************}
procedure TALCustomTrack.TValueIndicator.AnimationFinish(Sender: TObject);
begin
  FFloatAnimation.Enabled := False;
  case FAnimation of
    TAnimation.ScaleInOut: begin
      if SameValue(Scale.X, 0, TEpsilon.Scale) and SameValue(Scale.Y, 0, TEpsilon.Scale) then
        visible := False;
    end;
    TAnimation.Opacity: begin
      if SameValue(Opacity, 0, TEpsilon.Vector) then
        visible := False;
    end
    else
      Raise Exception.Create('Error D6F17D76-E47E-4144-8FBA-5CAD3EBF84F3')
  end;
end;

{***********************************************************}
function TALCustomTrack.TValueIndicator.CreateFill: TALBrush;
begin
  Result := TFill.Create;
end;

{******************************************************************************}
function TALCustomTrack.TValueIndicator.CreateTextSettings: TALBaseTextSettings;
begin
  Result := TTextSettings.Create;
end;

{***********************************************************************}
function TALCustomTrack.TValueIndicator.GetTextSettings: TALTextSettings;
begin
  Result := TALTextSettings(Inherited TextSettings);
end;

{*************************************************************************************}
procedure TALCustomTrack.TValueIndicator.SetTextSettings(const Value: TALTextSettings);
begin
  Inherited SetTextSettings(Value);
end;

{****************************************************************}
function TALCustomTrack.TValueIndicator.GetDefaultXRadius: Single;
begin
  Result := -50;
end;

{****************************************************************}
function TALCustomTrack.TValueIndicator.GetDefaultYRadius: Single;
begin
  Result := -50;
end;

{***************************************************************}
function TALCustomTrack.TValueIndicator.GetDefaultFormat: String;
begin
  Result := Format0;
end;

{**********************************************************************}
procedure TALCustomTrack.TValueIndicator.SetFormat(const Value: string);
begin
  if FFormat <> Value then
  begin
    ClearBufDrawable;
    FFormat := Value;
  end;
end;

{**************************************************************}
function TALCustomTrack.TValueIndicator.IsFormatStored: Boolean;
begin
  Result := FFormat <> DefaultFormat;
end;

{****************************************************************}
function TALCustomTrack.TTrack.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $ffc5c5c5;
end;

{******************************************************************}
function TALCustomTrack.TTrack.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{***********************************************************}
constructor TALCustomTrack.TTrack.TStopIndicatorBrush.Create;
begin
  inherited Create;
  FColor := DefaultColor;
  FColorKey := DefaultColorKey;
  FResourceName := DefaultResourceName;
  FWrapMode := DefaultWrapMode;
  FSize := DefaultSize;
end;

{******************************************************************************}
function TALCustomTrack.TTrack.TStopIndicatorBrush.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{****************************************************************************}
function TALCustomTrack.TTrack.TStopIndicatorBrush.GetDefaultColorKey: String;
begin
  Result := '';
end;

{********************************************************************************}
function TALCustomTrack.TTrack.TStopIndicatorBrush.GetDefaultResourceName: String;
begin
  Result := '';
end;

{**************************************************************************************}
function TALCustomTrack.TTrack.TStopIndicatorBrush.GetDefaultWrapMode: TALImageWrapMode;
begin
  Result := TALImageWrapMode.Fit;
end;

{************************************************************************}
function TALCustomTrack.TTrack.TStopIndicatorBrush.GetDefaultSize: Single;
begin
  Result := 4;
end;

{******************************************************************************}
procedure TALCustomTrack.TTrack.TStopIndicatorBrush.Assign(Source: TPersistent);
begin
  if Source is TStopIndicatorBrush then begin
    BeginUpdate;
    Try
      Color := TStopIndicatorBrush(Source).Color;
      ColorKey := TStopIndicatorBrush(Source).ColorKey;
      ResourceName := TStopIndicatorBrush(Source).ResourceName;
      WrapMode := TStopIndicatorBrush(Source).WrapMode;
      Size := TStopIndicatorBrush(Source).Size;
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{********************************************************}
procedure TALCustomTrack.TTrack.TStopIndicatorBrush.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Color := DefaultColor;
    ColorKey := DefaultColorKey;
    ResourceName := DefaultResourceName;
    WrapMode := DefaultWrapMode;
    Size := DefaultSize;
  finally
    EndUpdate;
  end;
end;

{***************************************************************}
procedure TALCustomTrack.TTrack.TStopIndicatorBrush.AlignToPixel;
begin
  BeginUpdate;
  try
    Size := ALAlignDimensionToPixelRound(Size, ALGetScreenScale, Tepsilon.Position);
  finally
    EndUpdate;
  end;
end;

{*******************************************************************}
procedure TALCustomTrack.TTrack.TStopIndicatorBrush.ApplyColorScheme;
begin
  if FColorKey <> '' then begin
    var LColor := TALStyleManager.Instance.GetColor(FColorKey);
    if FColor <> LColor then begin
      FColor := LColor;
      Change;
    end;
  end;
end;

{******************************************************************************************************************************************************}
procedure TALCustomTrack.TTrack.TStopIndicatorBrush.Interpolate(const ATo: TStopIndicatorBrush; const ANormalizedTime: Single; const AReverse: Boolean);
begin
  BeginUpdate;
  Try
    var LPrevColorKey := FColorKey;
    if ATo <> nil then begin
      Color := ALInterpolateColor(Color{Start}, ATo.Color{Stop}, ANormalizedTime);
      if not AReverse then ResourceName := ATo.ResourceName;
      if not AReverse then WrapMode := ATo.WrapMode;
      Size := InterpolateSingle(Size{Start}, ATo.Size{Stop}, ANormalizedTime);
    end
    else begin
      Color := ALInterpolateColor(Color{Start}, DefaultColor{Stop}, ANormalizedTime);
      if not AReverse then ResourceName := DefaultResourceName;
      if not AReverse then WrapMode := DefaultWrapMode;
      Size := InterpolateSingle(Size{Start}, DefaultSize{Stop}, ANormalizedTime);
    end;
    FColorKey := LPrevColorKey;
  finally
    EndUpdate;
  end;
end;

{***************************************************************************************************************************************************************}
procedure TALCustomTrack.TTrack.TStopIndicatorBrush.InterpolateNoChanges(const ATo: TStopIndicatorBrush; const ANormalizedTime: Single; const AReverse: Boolean);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime, AReverse);
  Finally
    EndUpdateNoChanges;
  end;
end;

{***************************************************************************}
function TALCustomTrack.TTrack.TStopIndicatorBrush.hasStopIndicator: Boolean;
begin
  Result := (CompareValue(FSize, 0, TEpsilon.Position) > 0) and
            ((Color <> TalphaColors.Null) or
             (ResourceName <> ''));
end;

{************************************************************************}
function TALCustomTrack.TTrack.TStopIndicatorBrush.IsColorStored: Boolean;
begin
  result := FColor <> DefaultColor;
end;

{***************************************************************************}
function TALCustomTrack.TTrack.TStopIndicatorBrush.IsColorKeyStored: Boolean;
begin
  result := FColorKey <> DefaultColorKey;
end;

{*******************************************************************************}
function TALCustomTrack.TTrack.TStopIndicatorBrush.IsResourceNameStored: Boolean;
begin
  result := FResourceName <> DefaultResourceName;
end;

{***************************************************************************}
function TALCustomTrack.TTrack.TStopIndicatorBrush.IsWrapModeStored: Boolean;
begin
  result := FWrapMode <> DefaultWrapMode;
end;

{***********************************************************************}
function TALCustomTrack.TTrack.TStopIndicatorBrush.IsSizeStored: Boolean;
begin
  result := not SameValue(fSize, DefaultSize, Tepsilon.Position);
end;

{*************************************************************************************}
procedure TALCustomTrack.TTrack.TStopIndicatorBrush.SetColor(const Value: TAlphaColor);
begin
  if fColor <> Value then begin
    fColor := Value;
    FColorKey := '';
    Change;
  end;
end;

{***********************************************************************************}
procedure TALCustomTrack.TTrack.TStopIndicatorBrush.SetColorKey(const Value: String);
begin
  if FColorKey <> Value then begin
    FColorKey := Value;
    ApplyColorScheme;
  end;
end;

{***************************************************************************************}
procedure TALCustomTrack.TTrack.TStopIndicatorBrush.SetResourceName(const Value: String);
begin
  if fResourceName <> Value then begin
    fResourceName := Value;
    Change;
  end;
end;

{*********************************************************************************************}
procedure TALCustomTrack.TTrack.TStopIndicatorBrush.SetWrapMode(const Value: TALImageWrapMode);
begin
  if fWrapMode <> Value then begin
    fWrapMode := Value;
    Change;
  end;
end;

{*******************************************************************************}
procedure TALCustomTrack.TTrack.TStopIndicatorBrush.SetSize(const Value: Single);
begin
  if not SameValue(FSize, Value, TEpsilon.Position) then begin
    FSize := Value;
    Change;
  end;
end;

{******************************************************************************************************}
constructor TALCustomTrack.TTrack.TInheritStopIndicatorBrush.Create(const AParent: TStopIndicatorBrush);
begin
  inherited create;
  FParent := AParent;
  FInherit := True;
  fSuperseded := False;
end;

{************************************************************************************************}
function TALCustomTrack.TTrack.TInheritStopIndicatorBrush.CreateSavedState: TALPersistentObserver;
type
  TInheritStopIndicatorBrushClass = class of TInheritStopIndicatorBrush;
begin
  result := TInheritStopIndicatorBrushClass(classtype).Create(nil{AParent});
end;

{*******************************************************************************************}
procedure TALCustomTrack.TTrack.TInheritStopIndicatorBrush.SetInherit(const AValue: Boolean);
begin
  If FInherit <> AValue then begin
    FInherit := AValue;
    Change;
  end;
end;

{*************************************************************************************}
procedure TALCustomTrack.TTrack.TInheritStopIndicatorBrush.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TInheritStopIndicatorBrush then begin
      Inherit := TInheritStopIndicatorBrush(Source).Inherit;
      fSuperseded := TInheritStopIndicatorBrush(Source).fSuperseded;
    end
    else begin
      Inherit := False;
      fSuperseded := False;
    end;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{***************************************************************}
procedure TALCustomTrack.TTrack.TInheritStopIndicatorBrush.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Inherit := True;
    fSuperseded := False;
  finally
    EndUpdate;
  end;
end;

{*********************************************************************}
procedure TALCustomTrack.TTrack.TInheritStopIndicatorBrush.DoSupersede;
begin
  Assign(FParent);
end;

{******************************************************************************************************}
procedure TALCustomTrack.TTrack.TInheritStopIndicatorBrush.Supersede(Const ASaveState: Boolean = False);
begin
  if ASaveState then SaveState;
  if (FSuperseded) or
     (not inherit) or
     (FParent = nil) then exit;
  BeginUpdate;
  try
    var LParentSuperseded := False;
    if FParent is TInheritStopIndicatorBrush then begin
      TInheritStopIndicatorBrush(FParent).SupersedeNoChanges(true{ASaveState});
      LParentSuperseded := True;
    end;
    try
      DoSupersede;
    finally
      if LParentSuperseded then
        TInheritStopIndicatorBrush(FParent).restoreStateNoChanges;
    end;
    Inherit := False;
    FSuperseded := True;
  finally
    EndUpdate;
  end;
end;

{***************************************************************************************************************}
procedure TALCustomTrack.TTrack.TInheritStopIndicatorBrush.SupersedeNoChanges(Const ASaveState: Boolean = False);
begin
  BeginUpdate;
  try
    Supersede(ASaveState);
  finally
    EndUpdateNoChanges;
  end;
end;

{********************************************************************************}
function TALCustomTrack.TTrack.TBaseStateStyle.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $ffc5c5c5;
end;

{**********************************************************************************}
function TALCustomTrack.TTrack.TBaseStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TalphaColors.Null;
end;

{***********************************************************************************}
function TALCustomTrack.TTrack.TBaseStateStyle.TStateLayer.GetDefaultXRadius: Single;
begin
  Result := -50;
end;

{***********************************************************************************}
function TALCustomTrack.TTrack.TBaseStateStyle.TStateLayer.GetDefaultYRadius: Single;
begin
  Result := -50;
end;

{*******************************************************************************}
constructor TALCustomTrack.TTrack.TBaseStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  if StateStyleParent <> nil then Raise Exception.create('Error 19ACDBF0-E33C-49D2-B199-5C232A0A71DB')
  else if ControlParent <> nil then FStopIndicator := CreateStopIndicator(ControlParent.StopIndicator)
  else FStopIndicator := CreateStopIndicator(nil);
  FStopIndicator.OnChanged := StopIndicatorChanged;
end;

{*******************************************************}
destructor TALCustomTrack.TTrack.TBaseStateStyle.Destroy;
begin
  ALFreeAndNil(FStopIndicator);
  inherited Destroy;
end;

{**************************************************************************************************}
function TALCustomTrack.TTrack.TBaseStateStyle.CreateFill(const AParent: TALBrush): TALInheritBrush;
begin
  Result := TFill.Create(AParent);
end;

{****************************************************************************************************************}
function TALCustomTrack.TTrack.TBaseStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{*****************************************************************************}
function TALCustomTrack.TTrack.TBaseStateStyle.CreateStateLayer: TALStateLayer;
begin
  Result := TStateLayer.Create;
end;

{*********************************************************************************************************************************}
function TALCustomTrack.TTrack.TBaseStateStyle.CreateStopIndicator(const AParent: TStopIndicatorBrush): TInheritStopIndicatorBrush;
begin
  Result := TInheritStopIndicatorBrush.Create(AParent);
end;

{**************************************************************************}
procedure TALCustomTrack.TTrack.TBaseStateStyle.Assign(Source: TPersistent);
begin
  if Source is TBaseStateStyle then begin
    BeginUpdate;
    Try
      StopIndicator.Assign(TBaseStateStyle(Source).StopIndicator);
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{****************************************************}
procedure TALCustomTrack.TTrack.TBaseStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    StopIndicator.Reset;
  finally
    EndUpdate;
  end;
end;

{***********************************************************}
procedure TALCustomTrack.TTrack.TBaseStateStyle.AlignToPixel;
begin
  BeginUpdate;
  try
    Inherited;
    StopIndicator.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{***************************************************************}
procedure TALCustomTrack.TTrack.TBaseStateStyle.ApplyColorScheme;
begin
  BeginUpdate;
  try
    Inherited;
    StopIndicator.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{************************************************************************************************************************************************}
procedure TALCustomTrack.TTrack.TBaseStateStyle.Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single; const AReverse: Boolean);
begin
  {$IF defined(debug)}
  if (ATo <> nil) and (not (ATo is TBaseStateStyle)) then
    Raise Exception.Create('Error 70FA71DE-6270-441D-AB33-6F987A011C09');
  {$ENDIF}
  BeginUpdate;
  Try
    inherited Interpolate(ATo, ANormalizedTime, AReverse);
    if ATo <> nil then StopIndicator.Interpolate(TBaseStateStyle(ATo).StopIndicator, ANormalizedTime, AReverse)
    {$IF defined(debug)}
    else if StateStyleParent <> nil then Raise Exception.Create('Error 9B674B61-66C2-4BB1-8A94-D6A58AEAF404')
    {$ENDIF}
    else if ControlParent <> nil then StopIndicator.Interpolate(ControlParent.StopIndicator, ANormalizedTime, AReverse)
    else StopIndicator.Interpolate(nil, ANormalizedTime, AReverse);
  Finally
    EndUpdate;
  End;
end;

{**********************************************************************}
function TALCustomTrack.TTrack.TBaseStateStyle.GetControlParent: TTrack;
begin
  {$IF defined(debug)}
  if (inherited ControlParent <> nil) and
     (not (inherited ControlParent is TTrack)) then
    raise Exception.Create('ControlParent must be of type TTrack');
  {$ENDIF}
  result := TTrack(inherited ControlParent);
end;

{**********************************************************}
procedure TALCustomTrack.TTrack.TBaseStateStyle.DoSupersede;
begin
  inherited;
  StopIndicator.Supersede;
end;

{*********************************************************************************************************}
procedure TALCustomTrack.TTrack.TBaseStateStyle.SetStopIndicator(const AValue: TInheritStopIndicatorBrush);
begin
  FStopIndicator.Assign(AValue);
end;

{*****************************************************************}
function TALCustomTrack.TTrack.TBaseStateStyle.GetInherit: Boolean;
begin
  Result := inherited GetInherit and
            StopIndicator.Inherit;
end;

{*************************************************************************************}
procedure TALCustomTrack.TTrack.TBaseStateStyle.StopIndicatorChanged(ASender: TObject);
begin
  Change;
end;

{**************************************************************************}
function TALCustomTrack.TTrack.TDisabledStateStyle.IsOpacityStored: Boolean;
begin
  Result := not SameValue(FOpacity, TControl.DefaultDisabledOpacity, TEpsilon.Scale);
end;

{**********************************************************************************}
procedure TALCustomTrack.TTrack.TDisabledStateStyle.SetOpacity(const Value: Single);
begin
  if not SameValue(FOpacity, Value, TEpsilon.Scale) then begin
    FOpacity := Value;
    Change;
  end;
end;

{***********************************************************************************}
constructor TALCustomTrack.TTrack.TDisabledStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  FOpacity := TControl.DefaultDisabledOpacity;
end;

{******************************************************************************}
procedure TALCustomTrack.TTrack.TDisabledStateStyle.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TDisabledStateStyle then
      Opacity := TDisabledStateStyle(Source).Opacity
    else
      Opacity := TControl.DefaultDisabledOpacity;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{********************************************************}
procedure TALCustomTrack.TTrack.TDisabledStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Opacity := TControl.DefaultDisabledOpacity;
  finally
    EndUpdate;
  end;
end;

{*********************************************************************}
function TALCustomTrack.TTrack.TDisabledStateStyle.GetInherit: Boolean;
begin
  // Opacity is not part of the GetInherit function because it updates the
  // disabledOpacity of the base control immediately every time it changes.
  // Essentially, it acts merely as a link to the disabledOpacity of the base control.
  Result := inherited GetInherit;
end;

{*******************************************************************************}
constructor TALCustomTrack.TTrack.TStateStyles.Create(const AParent: TALControl);
begin
  inherited Create(AParent);
  FDisabled := CreateDisabledStateStyle(AParent);
  FDisabled.OnChanged := DisabledChanged;
end;

{****************************************************}
destructor TALCustomTrack.TTrack.TStateStyles.Destroy;
begin
  ALFreeAndNil(FDisabled);
  inherited Destroy;
end;

{****************************************************************************************************************}
function TALCustomTrack.TTrack.TStateStyles.CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle;
begin
  Result := TDisabledStateStyle.Create(AParent);
end;

{***********************************************************************}
procedure TALCustomTrack.TTrack.TStateStyles.Assign(Source: TPersistent);
begin
  if Source is TStateStyles then begin
    BeginUpdate;
    Try
      Disabled.Assign(TStateStyles(Source).Disabled);
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{*************************************************}
procedure TALCustomTrack.TTrack.TStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Disabled.reset;
  finally
    EndUpdate;
  end;
end;

{********************************************************}
procedure TALCustomTrack.TTrack.TStateStyles.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    Disabled.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{************************************************************}
procedure TALCustomTrack.TTrack.TStateStyles.ApplyColorScheme;
begin
  BeginUpdate;
  Try
    inherited;
    Disabled.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{************************************************************}
procedure TALCustomTrack.TTrack.TStateStyles.ClearBufDrawable;
begin
  inherited;
  Disabled.ClearBufDrawable;
end;

{********************************************************************************}
function TALCustomTrack.TTrack.TStateStyles.GetCurrentRawStyle: TALBaseStateStyle;
begin
  if Not Parent.Enabled then Result := Disabled
  else result := nil;
end;

{***************************************************************************}
function TALCustomTrack.TTrack.TStateStyles.GetParent: TALCustomTrack.TTrack;
begin
  Result := TALCustomTrack.TTrack(inherited Parent);
end;

{******************************************************************************************}
procedure TALCustomTrack.TTrack.TStateStyles.SetDisabled(const AValue: TDisabledStateStyle);
begin
  FDisabled.Assign(AValue);
end;

{*****************************************************************************}
procedure TALCustomTrack.TTrack.TStateStyles.DisabledChanged(ASender: TObject);
begin
  Change;
end;

{***************************************************************************}
constructor TALCustomTrack.TTrack.Create(const ACustomTrack: TALCustomTrack);
begin
  FStateStyles := nil;
  //--
  inherited Create(ACustomTrack);
  FCustomTrack := ACustomTrack;
  FStopIndicator := CreateStopIndicator;
  FStopIndicator.OnChanged := StopIndicatorChanged;
  Locked := True;
  HitTest := False;
  //--
  FStateStyles := CreateStateStyles;
  FStateStyles.OnChanged := StateStylesChanged;
end;

{***************************************}
destructor TALCustomTrack.TTrack.Destroy;
begin
  ALFreeAndNil(FStateStyles);
  ALFreeAndNil(FStopIndicator);
  inherited;
end;

{**********************************************************************}
procedure TALCustomTrack.TTrack.Assign(Source: TPersistent{TALControl});
begin
  BeginUpdate;
  Try
    if Source is TTrack then begin
      StateStyles.Assign(TTrack(Source).StateStyles);
      StopIndicator.Assign(TTrack(Source).StopIndicator);
    end
    else
      ALAssignError(Source{ASource}, Self{ADest});
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{**************************************************}
function TALCustomTrack.TTrack.CreateFill: TALBrush;
begin
  Result := TFill.Create;
end;

{**********************************************************}
function TALCustomTrack.TTrack.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
end;

{**********************************************************************}
function TALCustomTrack.TTrack.CreateStopIndicator: TStopIndicatorBrush;
begin
  Result := TStopIndicatorBrush.Create;
end;

{*************************************************************}
function TALCustomTrack.TTrack.CreateStateStyles: TStateStyles;
begin
  Result := TStateStyles.Create(self);
end;

{*******************************************}
procedure TALCustomTrack.TTrack.AlignToPixel;
begin
  beginUpdate;
  try
    inherited;
    StateStyles.AlignToPixel;
    StopIndicator.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{***********************************************}
procedure TALCustomTrack.TTrack.ApplyColorScheme;
begin
  beginUpdate;
  try
    inherited;
    StateStyles.ApplyColorScheme;
    StopIndicator.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{****************************************************}
function TALCustomTrack.TTrack.HasCustomDraw: Boolean;
begin
  Result := StopIndicator.hasStopIndicator;
end;

{*************************************************************************}
procedure TALCustomTrack.TTrack.SetStateStyles(const AValue: TStateStyles);
begin
  FStateStyles.Assign(AValue);
end;

{*********************************************************************************}
procedure TALCustomTrack.TTrack.SetStopIndicator(const Value: TStopIndicatorBrush);
begin
  FStopIndicator.Assign(Value);
end;

{******************************************************************}
procedure TALCustomTrack.TTrack.StateStylesChanged(Sender: TObject);
begin
  ClearBufDrawable;
  DisabledOpacity := StateStyles.Disabled.opacity;
  Repaint;
end;

{********************************************************************}
procedure TALCustomTrack.TTrack.StopIndicatorChanged(Sender: TObject);
begin
  ClearBufDrawable;
  Repaint;
end;

{*********************************************}
procedure TALCustomTrack.TTrack.PaddingChanged;
begin
  inherited;
  ClearBufDrawable;
  Repaint;
end;

{***********************************************}
procedure TALCustomTrack.TTrack.ClearBufDrawable;
begin
  if FcustomTrack.FIsAligningTracks then exit;
  {$IFDEF debug}
  if (FStateStyles <> nil) and
     (not (csDestroying in ComponentState)) and
     (ALIsDrawableNull(FBufDrawable)) and // warn will be raise in inherited
     (not ALIsDrawableNull(FStateStyles.Disabled.FBufDrawable)) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  if FStateStyles <> nil then
    FStateStyles.ClearBufDrawable;
  inherited ClearBufDrawable;
end;

{**********************************************}
procedure TALCustomTrack.TTrack.MakeBufDrawable;
begin
  //--- Do not create BufDrawable if not DoubleBuffered
  if {$IF not DEFINED(ALDPK)}(not DoubleBuffered){$ELSE}False{$ENDIF} then begin
    ClearBufDrawable;
    exit;
  end;
  //--
  inherited MakeBufDrawable;
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if LStateStyle.Inherit then exit;
  if (not ALIsDrawableNull(LStateStyle.FBufDrawable)) then exit;
  LStateStyle.SupersedeNoChanges(true{ASaveState});
  try

    {$IFDEF debug}
    ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Style: ' + LStateStyle.ClassName + ' | Width: ' + ALFloatToStrW(Width)+ ' | Height: ' + ALFloatToStrW(Height));
    {$endif}

    // Create the BufDrawable
    CreateBufDrawable(
      LStateStyle.FBufDrawable, // var ABufDrawable: TALDrawable;
      LStateStyle.FBufDrawableRect, // var ABufDrawableRect: TRectF;
      ALGetScreenScale, // const AScale: Single;
      LStateStyle.Fill, // const AFill: TALBrush;
      LStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
      TAlphaColors.null, // const AStateLayerContentColor: TAlphaColor;
      False, // const ADrawStateLayerOnTop: Boolean;
      LStateStyle.Stroke, // const AStroke: TALStrokeBrush;
      LStateStyle.Shadow, // const AShadow: TALShadow
      LStateStyle.StopIndicator); // const AStopIndicator: TStopIndicator);

  finally
    LStateStyle.RestorestateNoChanges;
  end;
end;

{************************************************}
Procedure TALCustomTrack.TTrack.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            out ABufDrawableRect: TRectF;
            const AScale: Single;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStateLayerContentColor: TAlphaColor;
            const ADrawStateLayerOnTop: Boolean;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow);
begin
  CreateBufDrawable(
    ABufDrawable, // var ABufDrawable: TALDrawable;
    ABufDrawableRect, // out ABufDrawableRect: TRectF;
    AScale, // const AScale: Single;
    AFill, // const AFill: TALBrush;
    AStateLayer, // const AStateLayer: TALStateLayer;
    AStateLayerContentColor, // const AStateLayerContentColor: TAlphaColor;
    ADrawStateLayerOnTop, // const ADrawStateLayerOnTop: Boolean;
    AStroke, // const AStroke: TALStrokeBrush;
    AShadow, // const AShadow: TALShadow;
    StopIndicator); // const AStopIndicator: TStopIndicator);
end;

{************************************************}
Procedure TALCustomTrack.TTrack.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            out ABufDrawableRect: TRectF;
            const AScale: Single;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStateLayerContentColor: TAlphaColor;
            const ADrawStateLayerOnTop: Boolean;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow;
            const AStopIndicator: TStopIndicatorBrush);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  ABufDrawableRect := LocalRect;
  if FCustomTrack.Orientation = TOrientation.Horizontal then
    ABufDrawableRect.Width := FCustomTrack.GetTrackSize(true{AIncludeTrackPadding})
  else
    ABufDrawableRect.Height := FCustomTrack.GetTrackSize(true{AIncludeTrackPadding});
  var LSurfaceRect := ALGetShapeSurfaceRect(
                        ABufDrawableRect, // const ARect: TRectF;
                        AutoAlignToPixel, // const AAlignToPixel: Boolean;
                        AFill, // const AFill: TALBrush;
                        nil, // const AFillResourceStream: TStream;
                        AStateLayer, // const AStateLayer: TALStateLayer;
                        AShadow); // const AShadow: TALShadow): TRectF;
  ABufDrawableRect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);

  var LSurface: TALSurface;
  var LCanvas: TALCanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    AScale, // const AScale: Single;
    LSurfaceRect.Width, // const w: integer;
    LSurfaceRect.height);// const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      TALDrawRectangleHelper.Create(LCanvas)
        .SetScale(AScale)
        .SetAlignToPixel(AutoAlignToPixel)
        .SetDstRect(ABufDrawableRect)
        .SetFill(AFill)
        .SetStateLayer(AStateLayer, AStateLayerContentColor)
        .SetDrawStateLayerOnTop(ADrawStateLayerOnTop)
        .SetStroke(AStroke)
        .SetShadow(AShadow)
        .SetSides(Sides)
        .SetCorners(Corners)
        .SetXRadius(XRadius)
        .SetYRadius(YRadius)
        .Draw;

      If AStopIndicator.hasStopIndicator then begin
        var LStopIndicatorCount: integer;
        if FCustomTrack.Frequency > 0 then
          LStopIndicatorCount := Ceil((FCustomTrack.Max - FCustomTrack.Min) / FCustomTrack.Frequency) + 1
        else
          LStopIndicatorCount := 2;
        if FCustomTrack.Orientation = TOrientation.Horizontal then begin
          if LStopIndicatorCount * 2{1 indicator + 1 empty space} > ABufDrawableRect.Width / AStopIndicator.Size then
            LStopIndicatorCount := 2;
        end
        else begin
          if LStopIndicatorCount * 2{1 indicator + 1 empty space} > ABufDrawableRect.Height / AStopIndicator.Size then
            LStopIndicatorCount := 2;
        end;
        LStopIndicatorCount := system.Math.Max(LStopIndicatorCount, 2);
        For var i := 0 to LStopIndicatorCount -1 do begin
          var LDstRect := TrectF.Create(0,0,AStopIndicator.Size, AStopIndicator.Size);
          LDstRect := LDstRect.CenterAt(ABufDrawableRect);
          var LPos: Single;
          if (LStopIndicatorCount = 2) and (I = 1) then
            LPos := FCustomTrack.GetThumbPos(FCustomTrack.Max)
          else
            LPos := FCustomTrack.GetThumbPos(System.math.Min(FCustomTrack.Min + (i * FCustomTrack.Frequency), FCustomTrack.Max));
          LPos := LPos - FCustomTrack.Padding.Left - (AStopIndicator.Size / 2);
          if FCustomTrack.Orientation = TOrientation.Horizontal then
            LDstRect.SetLocation(LPos, LDstRect.Top)
          else
            LDstRect.SetLocation(LDstRect.Left, Lpos);
          TALDrawRectangleHelper.Create(LCanvas)
            .SetScale(AScale)
            .SetAlignToPixel(AutoAlignToPixel)
            .SetDstRect(TRectF.Create(0, 0, 1, 1).FitInto(LDstRect))
            .SetFillColor(AStopIndicator.Color)
            .SetFillResourceName(AStopIndicator.ResourceName)
            .SetFillWrapMode(AStopIndicator.WrapMode)
            .SetXRadius(-50)
            .SetYRadius(-50)
            .Draw;
        end;
      end;

    finally
      ALCanvasEndScene(LCanvas)
    end;

    ABufDrawable := ALCreateDrawableFromSurface(LSurface);
    // The Shadow or Statelayer are not included in the dimensions of the fBufDrawableRect rectangle.
    // However, the fBufDrawableRect rectangle is offset by the dimensions of the shadow/Statelayer.
    ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);

  finally
    ALFreeAndNilSurface(LSurface, LCanvas);
  end;

end;

{************************************}
procedure TALCustomTrack.TTrack.Paint;
begin

  StateStyles.UpdateLastPaintedRawStyle;
  MakeBufDrawable;

  var LDrawable: TALDrawable;
  var LDrawableRect: TRectF;
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle <> nil then begin
    LDrawable := LStateStyle.FBufDrawable;
    LDrawableRect := LStateStyle.FBufDrawableRect;
    if ALIsDrawableNull(LDrawable) then begin
      LDrawable := FBufDrawable;
      LDrawableRect := FBufDrawableRect;
    end;
  end
  else begin
    LDrawable := FBufDrawable;
    LDrawableRect := FBufDrawableRect;
  end;

  if ALIsDrawableNull(LDrawable) then begin

    var LCurrentAdjustedStateStyle := TBaseStateStyle(StateStyles.GetCurrentAdjustedStyle);
    if LCurrentAdjustedStateStyle = nil then begin
      inherited Paint;
      exit;
    end;

    {$IF DEFINED(ALSkiaCanvas)}

    TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
      .SetAlignToPixel(AutoAlignToPixel)
      .SetDstRect(LocalRect)
      .SetOpacity(AbsoluteOpacity)
      .SetFill(LCurrentAdjustedStateStyle.Fill)
      .SetStateLayer(LCurrentAdjustedStateStyle.StateLayer, TalphaColors.Null)
      .SetStroke(LCurrentAdjustedStateStyle.Stroke)
      .SetShadow(LCurrentAdjustedStateStyle.Shadow)
      .SetSides(Sides)
      .SetCorners(Corners)
      .SetXRadius(XRadius)
      .SetYRadius(YRadius)
      .Draw;

    {$ELSE}

    {$IF defined(DEBUG)}
    if not doublebuffered then begin
      ALLog('TALCustomTrack.TTrack.Paint', 'Controls that are not double-buffered only work when SKIA is enabled', TALLogType.ERROR);
      exit;
    end;
    {$ENDIF}

    {$ENDIF}

    exit;
  end;

  var LSrcRect := GetBufDrawableSrcRect;
  var LDstRect := LSrcRect;
  LDstRect.Width := LDstRect.Width / Canvas.Scale;
  LDstRect.height := LDstRect.height / Canvas.Scale;
  LDstRect.SetLocation(0,0);
  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LSrcRect, // const ASrcRect: TrectF; // IN REAL PIXEL !
    LDstRect, // const ADstRect: TrectF; // IN Virtual pixels !
    AbsoluteOpacity); // const AOpacity: Single);

end;

{*******************************************************************}
function TALCustomTrack.TInactiveTrack.GetBufDrawableSrcRect: TRectF;
begin
  if ALIsDrawableNull(FBufDrawable) then Exit(TRectF.Empty);
  Result := TRectF.Create(0, 0, ALGetDrawableWidth(FBufDrawable), ALGetDrawableHeight(FBufDrawable));
  if FCustomTrack.Orientation = TOrientation.Horizontal then begin
    Result.SetLocation(
      Result.width - (Width * Canvas.Scale) - 1{For AddPixelForAlignment in ALCreateSurface},
      Result.Top);
    Result.Width := (Width * Canvas.Scale) + 1{For AddPixelForAlignment in ALCreateSurface};
  end
  else begin
    Result.SetLocation(
      Result.Left,
      Result.Height - (Height * Canvas.Scale) - 1{For AddPixelForAlignment in ALCreateSurface});
    Result.Height := (Height * Canvas.Scale) + 1{For AddPixelForAlignment in ALCreateSurface};
  end;
  Result := ALAlignToPixelRound(Result, TMatrix.Identity, ALGetScreenScale{Scale}, TEpsilon.Position);
end;

{**********************************************************************}
function TALCustomTrack.TActiveTrack.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $ff167efc;
end;

{******************************************************************************************}
function TALCustomTrack.TActiveTrack.TDisabledStateStyle.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $ff167efc;
end;

{************************************************************************************************************}
function TALCustomTrack.TActiveTrack.TDisabledStateStyle.CreateFill(const AParent: TALBrush): TALInheritBrush;
begin
  Result := TFill.Create(AParent);
end;

{*****************************************************************************************************************************}
function TALCustomTrack.TActiveTrack.TStateStyles.CreateDisabledStateStyle(const AParent: TObject): TTrack.TDisabledStateStyle;
begin
  Result := TDisabledStateStyle.Create(AParent);
end;

{********************************************************}
function TALCustomTrack.TActiveTrack.CreateFill: TALBrush;
begin
  Result := TFill.Create;
end;

{**************************************************************************}
function TALCustomTrack.TActiveTrack.CreateStateStyles: TTrack.TStateStyles;
begin
  Result := TStateStyles.Create(self);
end;

{*****************************************************************}
function TALCustomTrack.TActiveTrack.GetBufDrawableSrcRect: TRectF;
begin
  if ALIsDrawableNull(FBufDrawable) then Exit(TRectF.Empty);
  Result := TRectF.Create(0, 0, ALGetDrawableWidth(FBufDrawable), ALGetDrawableHeight(FBufDrawable));
  if FCustomTrack.Orientation = TOrientation.Horizontal then
    Result.Width := Width * Canvas.Scale
  else
    Result.Height := Height * Canvas.Scale;
  Result := ALAlignToPixelRound(Result, TMatrix.Identity, ALGetScreenScale{Scale}, TEpsilon.Position);
end;

{****************************************************}
constructor TALCustomTrack.Create(AOwner: TComponent);
begin
  FThumb := nil;
  FInactiveTrack := nil;
  FActiveTrack := nil;
  FValueIndicator := nil;
  //--
  inherited;
  //--
  SetAcceptsControls(False);
  DisabledOpacity := 1;
  CanFocus := True;
  inherited TabStop := False;
  FTabStop := True;
  FIsAligningTracks := False;
  FOrientation := TOrientation.Horizontal;
  FOnChange := nil;
  //--
  FThumb := CreateThumb;
  FInactiveTrack := CreateInactiveTrack;
  FActiveTrack := CreateActiveTrack;
  FValueIndicator := CreateValueIndicator;
end;

{*****************************************}
procedure TALCustomTrack.AfterConstruction;
begin
  inherited;
  realign;
end;

{***************************************************************}
procedure TALCustomTrack.Assign(Source: TPersistent{TALControl});
begin
  BeginUpdate;
  Try
    if Source is TALCustomTrack then begin
      TabStop := TALCustomTrack(Source).TabStop;
      if InactiveTrack <> nil then InactiveTrack.assign(TALCustomTrack(Source).InactiveTrack);
      if ActiveTrack <> nil then ActiveTrack.assign(TALCustomTrack(Source).ActiveTrack);
      if Thumb <> nil then Thumb.assign(TALCustomTrack(Source).Thumb);
      if ValueIndicator <> nil then ValueIndicator.assign(TALCustomTrack(Source).ValueIndicator);
      Orientation := TALCustomTrack(Source).Orientation;
      OnChange := TALCustomTrack(Source).OnChange;
    end
    else
      ALAssignError(Source{ASource}, Self{ADest});
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{******************************}
procedure TALCustomTrack.Loaded;
begin
  if FThumb.FValueRange.IsChanged then
    FThumb.FValueRange.Changed(True);
  inherited;
end;

{*******************************************************************************************************************************************************}
function TALCustomTrack.CreateInactiveTrack(const AInactiveTrackClass: TInactiveTrackClass = nil; Const AName: String = 'InactiveTrack'): TInactiveTrack;
begin
  if AInactiveTrackClass = nil then Exit(CreateInactiveTrack(TInactiveTrack, AName));
  //--
  Result := AInactiveTrackClass.Create(self);
  Result.Parent := self;
  Result.Stored := False;
  Result.SetSubComponent(True);
  Result.Name := AName; // Useful at design time in the IDE
  //--
  var LHalfHeight := GetDefaultSize.Height / 2;
  var LMarginsChange := Result.Margins.OnChange;
  Result.Margins.OnChange := nil;
  Result.Margins.DefaultValue := TrectF.Create(0,LHalfHeight-1,0,LHalfHeight-1); // 2px height
  Result.Margins.Rect := Result.Margins.DefaultValue;
  Result.Margins.OnChange := LMarginsChange;
  //--
  if Orientation = TOrientation.Horizontal then Result.Align := TALAlignLayout.vertical
  else Result.Align := TALAlignLayout.horizontal;
end;

{*********************************************************************************************************************************************}
function TALCustomTrack.CreateActiveTrack(const AActiveTrackClass: TActiveTrackClass = nil; Const AName: String = 'ActiveTrack'): TActiveTrack;
begin
  if AActiveTrackClass = nil then Exit(CreateActiveTrack(TActiveTrack, AName));
  //--
  Result := AActiveTrackClass.Create(self);
  Result.Parent := self;
  Result.Stored := False;
  Result.SetSubComponent(True);
  Result.Name := AName; // Useful at design time in the IDE
  //--
  var LHalfHeight := GetDefaultSize.Height / 2;
  var LMarginsChange := Result.Margins.OnChange;
  Result.Margins.OnChange := nil;
  Result.Margins.DefaultValue := TrectF.Create(0,LHalfHeight-1,0,LHalfHeight-1); // 2px height
  Result.Margins.Rect := Result.Margins.DefaultValue;
  Result.Margins.OnChange := LMarginsChange;
  //--
  if Orientation = TOrientation.Horizontal then Result.Align := TALAlignLayout.vertical
  else Result.Align := TALAlignLayout.horizontal;
end;

{***************************************************************************************************************}
function TALCustomTrack.CreateThumb(const AThumbClass: TThumbClass = nil; Const AName: String = 'Thumb'): TThumb;
begin
  if AThumbClass = nil then Exit(CreateThumb(TThumb, AName));
  //--
  Result := AThumbClass.Create(self);
  Result.Parent := self;
  Result.Stored := False;
  Result.SetSubComponent(True);
  Result.Name := AName; // Useful at design time in the IDE
  Result.Width := GetDefaultSize.Height; // 32 px width
  //--
  if Orientation = TOrientation.Horizontal then Result.Align := TALAlignLayout.vertical
  else Result.Align := TALAlignLayout.horizontal;
end;

{************************************************************************************************************************************************************}
function TALCustomTrack.CreateValueIndicator(const AValueIndicatorClass: TValueIndicatorClass = nil; Const AName: String = 'ValueIndicator'): TValueIndicator;
begin
  if AValueIndicatorClass = nil then Exit(CreateValueIndicator(TValueIndicator, AName));
  //--
  Result := AValueIndicatorClass.Create(self);
  Result.Parent := self;
  Result.Stored := False;
  Result.SetSubComponent(True);
  Result.Name := AName; // Useful at design time in the IDE
end;

{************************************}
procedure TALCustomTrack.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    if FInactiveTrack <> nil then FInactiveTrack.AlignToPixel;
    if FActiveTrack <> nil then FActiveTrack.AlignToPixel;
    if FThumb <> nil then FThumb.AlignToPixel;
    if FValueIndicator <> nil then FValueIndicator.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{****************************************}
procedure TALCustomTrack.ApplyColorScheme;
begin
  //BeginUpdate;
  //Try
    inherited;
    //if FInactiveTrack <> nil then FInactiveTrack.ApplyColorScheme;
    //if FActiveTrack <> nil then FActiveTrack.ApplyColorScheme;
    //if FThumb <> nil then FThumb.ApplyColorScheme;
    //if FValueIndicator <> nil then FValueIndicator.ApplyColorScheme;
  //finally
    //EndUpdate;
  //end;
end;

{***************************************}
procedure TALCustomTrack.MakeBufDrawable;
begin
  if FInactiveTrack <> nil then FInactiveTrack.MakeBufDrawable;
  if FActiveTrack <> nil then FActiveTrack.MakeBufDrawable;
  if FThumb <> nil then FThumb.MakeBufDrawable;
  //if FValueIndicator <> nil then FValueIndicator.MakeBufDrawable;
end;

{****************************************}
procedure TALCustomTrack.ClearBufDrawable;
begin
  if FInactiveTrack <> nil then FInactiveTrack.ClearBufDrawable;
  if FActiveTrack <> nil then FActiveTrack.ClearBufDrawable;
  if FThumb <> nil then FThumb.ClearBufDrawable;
  if FValueIndicator <> nil then FValueIndicator.ClearBufDrawable;
end;

{*******************************************}
function TALCustomTrack.ValueStored: Boolean;
begin
  Result := not SameValue(Value, 0, Tepsilon.Vector);
end;

{**************************************************}
function TALCustomTrack.ViewportSizeStored: Boolean;
begin
  Result := not SameValue(ViewportSize, 0, Tepsilon.Vector);
end;

{***********************************************}
function TALCustomTrack.FrequencyStored: Boolean;
begin
  Result := not SameValue(Frequency, 0, Tepsilon.Vector);
end;

{*****************************************}
function TALCustomTrack.MaxStored: Boolean;
begin
  Result := not SameValue(Max, FMX.StdActns.DefaultMaxValue, Tepsilon.Vector);
end;

{*****************************************}
function TALCustomTrack.MinStored: Boolean;
begin
  Result := not SameValue(Min, 0, Tepsilon.Vector);
end;

{*************************************}
function TALCustomTrack.GetMax: Double;
begin
  Result := FThumb.FValueRange.Max;
end;

{***************************************************}
procedure TALCustomTrack.SetMax(const Value: Double);
begin
  if not SameValue(GetMax, Value) then begin
    if compareValue(Value, Min) < 0 then min := Value;
    FThumb.FValueRange.Max := Value;
    ClearBufDrawable;
  end;
end;

{*************************************}
function TALCustomTrack.GetMin: Double;
begin
  Result := FThumb.FValueRange.Min;
end;

{***************************************************}
procedure TALCustomTrack.SetMin(const Value: Double);
begin
  if not SameValue(GetMin, Value) then begin
    if compareValue(Value, Max) > 0 then max := Value;
    FThumb.FValueRange.Min := Value;
    ClearBufDrawable;
  end;
end;

{*******************************************}
function TALCustomTrack.GetFrequency: Double;
begin
  Result := FThumb.FValueRange.Frequency;
end;

{*********************************************************}
procedure TALCustomTrack.SetFrequency(const Value: Double);
begin
  if not SameValue(GetFrequency, Value) then begin
    FThumb.FValueRange.Frequency := Value;
    ClearBufDrawable;
  end;
end;

{***************************************}
function TALCustomTrack.GetValue: Double;
begin
  Result := FThumb.FValueRange.Value;
end;

{***********************************************}
procedure TALCustomTrack.SetValue(Value: Double);
begin
  if not SameValue(GetValue, Value) then
    FThumb.FValueRange.Value := Value;
end;

{**********************************************}
function TALCustomTrack.GetViewportSize: Double;
begin
  Result := FThumb.FValueRange.ViewportSize;
end;

{************************************************************}
procedure TALCustomTrack.SetViewportSize(const Value: Double);
begin
  if not SameValue(GetViewportSize, Value) then
    FThumb.FValueRange.ViewportSize := Value;
end;

{*************************************************}
function TALCustomTrack.GetDoubleBuffered: boolean;
begin
  result := FThumb.DoubleBuffered;
end;

{****************************************************************}
procedure TALCustomTrack.SetDoubleBuffered(const AValue: Boolean);
begin
  FThumb.DoubleBuffered := AValue;
  if FInactiveTrack <> nil then FInactiveTrack.DoubleBuffered := AValue;
  if FActiveTrack <> nil then FActiveTrack.DoubleBuffered := AValue;
  if FValueIndicator <> nil then FValueIndicator.DoubleBuffered := AValue;
end;

{********************************************}
function TALCustomTrack._GetCanFocus: Boolean;
begin
  Result := inherited CanFocus;
end;

{**********************************************************}
procedure TALCustomTrack._SetCanFocus(const Value: Boolean);
begin
  Inherited CanFocus := Value;
  if FThumb <> nil then FThumb.CanFocus := Value;
end;

{*********************************************************}
procedure TALCustomTrack._SetTabStop(const Value: Boolean);
begin
  FTabStop := Value;
  if FThumb <> nil then FThumb.TabStop := Value;
end;

{**********************************************}
function TALCustomTrack.GetLeadingTrack: TTrack;
begin
  Result := FActiveTrack;
end;

{***********************************************}
function TALCustomTrack.GetTrailingTrack: TTrack;
begin
  Result := FInactiveTrack;
end;

{**********************************************************}
function TALCustomTrack.GetLeadingTrackStartPadding: Single;
begin
  var LLeadingTrack := GetLeadingTrack;
  if LLeadingTrack <> nil then begin
    If Orientation = TOrientation.Horizontal then Result := LLeadingTrack.Padding.Left
    else Result := LLeadingTrack.Padding.Top;
  end
  else
    Result := 0;
end;

{*********************************************************}
function TALCustomTrack.GetTrailingTrackEndPadding: Single;
begin
  var LTrailingTrack := GetTrailingTrack;
  if LTrailingTrack <> nil then begin
    If Orientation = TOrientation.Horizontal then Result := LTrailingTrack.Padding.Right
    else Result := LTrailingTrack.Padding.bottom;
  end
  else
    Result := 0;
end;

{****************************************************************************************}
function TALCustomTrack.GetTrackSize(Const AIncludeTrackPadding: Boolean = False): Single;
begin
  if Orientation = TOrientation.Horizontal then begin
    result := Width - Padding.Left - Padding.Right;
    If FThumb <> nil then
      Result := Result - FThumb.Width{FThumb.Width/2 on the left + FThumb.Width/2 on the right};
  end
  else begin
    result := Height - Padding.Top - Padding.Bottom;
    If FThumb <> nil then
      Result := Result - FThumb.Height{FThumb.Height/2 on the left + FThumb.Height/2 on the right};
  end;
  If not AIncludeTrackPadding then
    Result := Result - GetLeadingTrackStartPadding - GetTrailingTrackEndPadding;
end;

{*********************************}
procedure TALCustomTrack.DoResized;
begin
  inherited;
  if FActiveTrack <> nil then
    FActiveTrack.ClearBufDrawable;
  if FInactiveTrack <> nil then
    FInactiveTrack.ClearBufDrawable;
end;

{*********************************}
procedure TALCustomTrack.DoRealign;
begin
  inherited;
  var LSaveDisableAlign := FDisableAlign;
  var LSaveIsAligningTracks := FIsAligningTracks;
  FIsAligningTracks := True;
  FDisableAlign := True;
  BeginUpdate;
  Try
    AlignThumb;
    AlignTracks;
  Finally
    EndUpdate;
    FDisableAlign := LSaveDisableAlign;
    FIsAligningTracks := LSaveIsAligningTracks;
  End;
end;

{****************************************************************}
function TALCustomTrack.GetThumbPos(const AValue: single): Single;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _ValueToPos(MinValue, MaxValue, ViewportSize: Double; TrackSize, Value: Single): Single;
  begin
    if ViewportSize < 0 then ViewportSize := 0;
    var LValRel: Double := MaxValue - MinValue - ViewportSize;
    if LValRel > 0 then begin
      LValRel := (Value - MinValue) / LValRel;
      Result := TrackSize * LValRel;
    end
    else Result := 0;
  end;

begin
  var LPos := _ValueToPos(
                Min, // MinValue
                Max, // MaxValue
                ViewportSize, // ViewportSize
                GetTrackSize, // TrackSize
                AValue); // Value
  If Orientation = TOrientation.Horizontal then Result := LPos + padding.Left
  else Result := LPos + padding.Top;
  Result := Result + GetLeadingTrackStartPadding;
  Result := ALAlignDimensionToPixelRound(Result, ALGetScreenScale, TEpsilon.Position);
end;

{**********************************}
procedure TALCustomTrack.AlignThumb;
begin
  if FThumb = nil then exit;
  var LThumbPos := GetThumbPos(Value);
  If Orientation = TOrientation.Horizontal then
    FThumb.Position.X := LThumbPos
  else
    FThumb.Position.Y := LThumbPos
end;

{***********************************}
procedure TALCustomTrack.AlignTracks;
begin
  if FThumb = nil then exit;
  if Orientation = TOrientation.Horizontal then begin
    if FInactiveTrack <> nil then begin
      FInactiveTrack.Position.X := FThumb.Position.X + Fthumb.Width + Fthumb.Margins.Right;
      FInactiveTrack.Width := Width - Padding.Right - FInactiveTrack.Position.X - (Fthumb.Width / 2);
    end;
    if FActiveTrack <> nil then begin
      FActiveTrack.Position.X := Padding.Left + (Fthumb.Width / 2);
      FActiveTrack.Width := FThumb.Position.X - Fthumb.Margins.Left - FActiveTrack.Position.X;
    end;
  end
  else begin
    if FInactiveTrack <> nil then begin
      FInactiveTrack.Position.Y := FThumb.Position.Y + Fthumb.Height + Fthumb.Margins.Bottom;
      FInactiveTrack.Height := Height - Padding.Bottom - FInactiveTrack.Position.Y - (Fthumb.Height / 2);
    end;
    if FActiveTrack <> nil then begin
      FActiveTrack.Position.Y := Padding.Top + (Fthumb.Height / 2);
      FActiveTrack.Height := FThumb.Position.Y - Fthumb.Margins.Top - FActiveTrack.Position.Y;
    end;
  end;
end;

{*********************************}
procedure TALCustomTrack.DoChanged;
begin
  if not (csLoading in ComponentState) and Assigned(FOnChange) then
    FOnChange(Self);
end;

{**************************************}
procedure TALCustomTrack.EnabledChanged;
begin
  inherited;
  if FInactiveTrack <> nil then FInactiveTrack.enabled := enabled;
  if FActiveTrack <> nil then FActiveTrack.enabled := enabled;
  if FThumb <> nil then FThumb.enabled := enabled;
  if FValueIndicator <> nil then FValueIndicator.enabled := enabled;
end;

{*****************************************************************}
procedure TALCustomTrack.SetOrientation(const Value: TOrientation);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function SwapTopBottomWithLeftRight(Const ARect: TrectF): TRectF;
  Begin
    Result.Left := ARect.Top;
    Result.Top := ARect.Left;
    Result.Right := ARect.Bottom;
    Result.Bottom := ARect.Right;
  End;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function SwapAlign(Const AAlign: TALAlignLayout): TALAlignLayout;
  Begin
    If AAlign = TALAlignLayout.Vertical then result := TALAlignLayout.Horizontal
    else If AAlign = TALAlignLayout.Horizontal then result := TALAlignLayout.Vertical
    else result := AAlign;
  End;

begin
  if FOrientation <> Value then begin
    FOrientation := Value;
    if not (csLoading in ComponentState) then begin
      BeginUpdate;
      Try
        SetBounds(Position.X, Position.Y, Height, Width);
        Margins.Rect := SwapTopBottomWithLeftRight(Margins.Rect);
        Padding.Rect := SwapTopBottomWithLeftRight(Padding.Rect);
        if FActiveTrack <> nil then begin
          FActiveTrack.Margins.Rect := SwapTopBottomWithLeftRight(FActiveTrack.Margins.Rect);
          FActiveTrack.Padding.Rect := SwapTopBottomWithLeftRight(FActiveTrack.Padding.Rect);
          FActiveTrack.Align := SwapAlign(FActiveTrack.Align);
        end;
        if FInactiveTrack <> nil then begin
          FInactiveTrack.Margins.Rect := SwapTopBottomWithLeftRight(FInactiveTrack.Margins.Rect);
          FInactiveTrack.Padding.Rect := SwapTopBottomWithLeftRight(FInactiveTrack.Padding.Rect);
          FInactiveTrack.Align := SwapAlign(FInactiveTrack.Align);
        end;
        if FThumb <> nil then begin
          FThumb.Margins.Rect := SwapTopBottomWithLeftRight(FThumb.Margins.Rect);
          FThumb.padding.Rect := SwapTopBottomWithLeftRight(FThumb.padding.Rect);
          FThumb.TouchTargetExpansion.Rect := SwapTopBottomWithLeftRight(FThumb.TouchTargetExpansion.Rect);
          var LThumbWidth := FThumb.Width;
          FThumb.Width := FThumb.Height;
          FThumb.Height := LThumbWidth;
          FThumb.Align := SwapAlign(FThumb.Align);
        end;
      Finally
        EndUpdate;
      End;
    end
    else begin
      if FActiveTrack <> nil then
        FActiveTrack.Align := SwapAlign(FActiveTrack.Align);
      if FInactiveTrack <> nil then
        FInactiveTrack.Align := SwapAlign(FInactiveTrack.Align);
      if FThumb <> nil then
        FThumb.Align := SwapAlign(FThumb.Align);
    end;
  end;
end;

{******************************************}
function TALTrackBar.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(150, 32);
end;

{***************************************************************************}
procedure TALRangeTrackBar.TMinInactiveTrack.SetXRadius(const Value: Single);
begin
  Inherited;
  var LMaxInactiveTrack := TALRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.SetXRadius(Value);
end;

{***************************************************************************}
procedure TALRangeTrackBar.TMinInactiveTrack.SetYRadius(const Value: Single);
begin
  Inherited;
  var LMaxInactiveTrack := TALRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.SetYRadius(Value);
end;

{*****************************************************************************}
procedure TALRangeTrackBar.TMinInactiveTrack.SetCorners(const Value: TCorners);
begin
  Inherited;
  var LMaxInactiveTrack := TALRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.SetCorners(Value);
end;

{**********************************************************}
procedure TALRangeTrackBar.TMinInactiveTrack.MarginsChanged;
begin
  Inherited;
  var LMaxInactiveTrack := TALRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.Margins.Rect := Margins.Rect;
end;

{**********************************************************}
procedure TALRangeTrackBar.TMinInactiveTrack.PaddingChanged;
begin
  Inherited;
  var LMaxInactiveTrack := TALRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.Padding.Rect := Padding.Rect;
end;

{*********************************************************************************}
procedure TALRangeTrackBar.TMinInactiveTrack.StopIndicatorChanged(Sender: TObject);
begin
  Inherited;
  var LMaxInactiveTrack := TALRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.StopIndicator.Assign(StopIndicator);
end;

{************************************************************************}
procedure TALRangeTrackBar.TMinInactiveTrack.FillChanged(Sender: TObject);
begin
  Inherited;
  var LMaxInactiveTrack := TALRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.Fill.Assign(Fill);
end;

{**************************************************************************}
procedure TALRangeTrackBar.TMinInactiveTrack.StrokeChanged(Sender: TObject);
begin
  Inherited;
  var LMaxInactiveTrack := TALRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.Stroke.Assign(Stroke);
end;

{**************************************************************}
function TALRangeTrackBar.TMinInactiveTrack._GetOpacity: Single;
begin
  Result := Inherited Opacity;
end;

{*****************************************************************************}
procedure TALRangeTrackBar.TMinInactiveTrack._SetOpacity(const AValue: Single);
begin
  Inherited Opacity := AValue;
  var LMaxInactiveTrack := TALRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.Opacity := AValue;
end;

{********************************************************************}
function TALRangeTrackBar.TMinInactiveTrack._IsOpacityStored: boolean;
begin
  Result := not SameValue(FOpacity, 1);
end;

{************************************************************************}
function TALRangeTrackBar.TMinInactiveTrack.GetBufDrawableSrcRect: TRectF;
begin
  if ALIsDrawableNull(FBufDrawable) then Exit(TRectF.Empty);
  Result := TRectF.Create(0, 0, ALGetDrawableWidth(FBufDrawable), ALGetDrawableHeight(FBufDrawable));
  if FCustomTrack.Orientation = TOrientation.Horizontal then
    Result.Width := Width * Canvas.Scale
  else
    Result.Height := Height * Canvas.Scale;
  Result := ALAlignToPixelRound(Result, TMatrix.Identity, ALGetScreenScale{Scale}, TEpsilon.Position);
end;

{************************************************************************}
function TALRangeTrackBar.TMaxInactiveTrack.GetBufDrawableSrcRect: TRectF;
begin
  if ALIsDrawableNull(FBufDrawable) then Exit(TRectF.Empty);
  Result := TRectF.Create(0, 0, ALGetDrawableWidth(FBufDrawable), ALGetDrawableHeight(FBufDrawable));
  if FCustomTrack.Orientation = TOrientation.Horizontal then begin
    Result.SetLocation(
      Result.width - (Width * Canvas.Scale) - 1{For AddPixelForAlignment in ALCreateSurface},
      Result.Top);
    Result.Width := (Width * Canvas.Scale) + 1{For AddPixelForAlignment in ALCreateSurface};
  end
  else begin
    Result.SetLocation(
      Result.Left,
      Result.Height - (Height * Canvas.Scale) - 1{For AddPixelForAlignment in ALCreateSurface});
    Result.Height := (Height * Canvas.Scale) + 1{For AddPixelForAlignment in ALCreateSurface};
  end;
  Result := ALAlignToPixelRound(Result, TMatrix.Identity, ALGetScreenScale{Scale}, TEpsilon.Position);
end;

{*******************************************************************}
function TALRangeTrackBar.TActiveTrack.GetBufDrawableSrcRect: TRectF;
begin
  if ALIsDrawableNull(FBufDrawable) then Exit(TRectF.Empty);
  Result := TRectF.Create(0, 0, ALGetDrawableWidth(FBufDrawable), ALGetDrawableHeight(FBufDrawable));
  if FCustomTrack.Orientation = TOrientation.Horizontal then begin
    Var LThumbWidth: Single;
    If FcustomTrack.FThumb <> nil then LThumbWidth := FcustomTrack.FThumb.Width
    else LThumbWidth := 0;
    Result.SetLocation(
      (Position.X - FcustomTrack.Padding.Left - (LThumbWidth / 2)) * Canvas.Scale,
      Result.Top);
    Result.Width := Width * Canvas.Scale;
  end
  else begin
    Var LThumbHeight: Single;
    If FcustomTrack.FThumb <> nil then LThumbHeight := FcustomTrack.FThumb.Height
    else LThumbHeight := 0;
    Result.SetLocation(
      Result.Left,
      (Position.Y - FcustomTrack.Padding.Top - (LThumbHeight / 2)) * Canvas.Scale);
    Result.Height := Height * Canvas.Scale;
  end;
  Result := ALAlignToPixelRound(Result, TMatrix.Identity, ALGetScreenScale{Scale}, TEpsilon.Position);
end;

{********************************************************************************}
constructor TALRangeTrackBar.TMinThumb.Create(const ACustomTrack: TALCustomTrack);
begin
  inherited;
  FFormerTouchTargetExpansionChangedHandler := TouchTargetExpansion.OnChange;
  TouchTargetExpansion.OnChange := TouchTargetExpansionChanged;
end;

{******************************************************}
function TALRangeTrackBar.TMinThumb._GetOpacity: Single;
begin
  Result := Inherited Opacity;
end;

{*********************************************************************}
procedure TALRangeTrackBar.TMinThumb._SetOpacity(const AValue: Single);
begin
  Inherited Opacity := AValue;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Opacity := AValue;
end;

{******************************************************}
function TALRangeTrackBar.TMinThumb._GetCursor: TCursor;
begin
  Result := Inherited Cursor;
end;

{*********************************************************************}
procedure TALRangeTrackBar.TMinThumb._SetCursor(const AValue: TCursor);
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Cursor := AValue;
end;

{************************************************************}
function TALRangeTrackBar.TMinThumb._IsOpacityStored: boolean;
begin
  Result := not SameValue(FOpacity, 1);
end;

{*******************************************************************}
procedure TALRangeTrackBar.TMinThumb.SetXRadius(const Value: Single);
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.XRadius := Value;
end;

{*******************************************************************}
procedure TALRangeTrackBar.TMinThumb.SetYRadius(const Value: Single);
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.YRadius := Value;
end;

{*********************************************************************}
procedure TALRangeTrackBar.TMinThumb.SetCorners(const Value: TCorners);
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Corners := Value;
end;

{**************************************************}
procedure TALRangeTrackBar.TMinThumb.MarginsChanged;
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Margins.rect := Margins.rect;
end;

{**************************************************}
procedure TALRangeTrackBar.TMinThumb.PaddingChanged;
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Padding.rect := Padding.rect;
end;

{****************************************************************}
procedure TALRangeTrackBar.TMinThumb.FillChanged(Sender: TObject);
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Fill.Assign(Fill);
end;

{******************************************************************}
procedure TALRangeTrackBar.TMinThumb.StrokeChanged(Sender: TObject);
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Stroke.Assign(Stroke);
end;

{******************************************************************}
procedure TALRangeTrackBar.TMinThumb.ShadowChanged(Sender: TObject);
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Shadow.Assign(Shadow);
end;

{***********************************************************************}
procedure TALRangeTrackBar.TMinThumb.StateStylesChanged(Sender: TObject);
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.StateStyles.Assign(StateStyles);
end;

{********************************************************************************}
procedure TALRangeTrackBar.TMinThumb.TouchTargetExpansionChanged(Sender: TObject);
begin
  if Assigned(FFormerTouchTargetExpansionChangedHandler) then
    FFormerTouchTargetExpansionChangedHandler(Sender);
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.TouchTargetExpansion.assign(TouchTargetExpansion);
end;

{*********************************************}
procedure TALRangeTrackBar.TMinThumb.DoResized;
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.size.size := size.size;
end;

{********************************************************************************}
constructor TALRangeTrackBar.TMaxThumb.Create(const ACustomTrack: TALCustomTrack);
begin
  inherited;
  var LValueRangeChanged := FValueRange.OnChanged;
  FValueRange.OnChanged := nil;
  FValueRange.Value := FValueRange.Max;
  FValueRange.OnChanged := LValueRangeChanged;
end;

{******************************************************}
constructor TALRangeTrackBar.Create(AOwner: TComponent);
begin
  //--
  FMaxInactiveTrack := nil;
  FMaxThumb := nil;
  //--
  inherited;
  //--
  FMaxInactiveTrack := CreateInactiveTrack(TMaxInactiveTrack, 'MaxInactiveTrack');
  FMaxThumb := CreateThumb(TMaxThumb, 'MaxThumb');
  //--
  FThumb.TabOrder := 0;
  FMaxThumb.TabOrder := 1;
end;

{********************************}
procedure TALRangeTrackBar.Loaded;
begin
  if FMaxThumb.FValueRange.IsChanged then
    FMaxThumb.FValueRange.Changed(True);
  inherited;
end;

{**************************************}
procedure TALRangeTrackBar.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    if FMaxInactiveTrack <> nil then FMaxInactiveTrack.AlignToPixel;
    if FMaxThumb <> nil then FMaxThumb.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{******************************************}
procedure TALRangeTrackBar.ApplyColorScheme;
begin
  //BeginUpdate;
  //Try
    inherited;
    //if FMaxInactiveTrack <> nil then FMaxInactiveTrack.ApplyColorScheme;
    //if FMaxThumb <> nil then FMaxThumb.ApplyColorScheme;
  //finally
    //EndUpdate;
  //end;
end;

{****************************************}
procedure TALRangeTrackBar.EnabledChanged;
begin
  inherited;
  if FMaxInactiveTrack <> nil then FMaxInactiveTrack.enabled := enabled;
  if FMaxThumb <> nil then FMaxThumb.enabled := enabled;
end;

{*****************************************}
procedure TALRangeTrackBar.MakeBufDrawable;
begin
  inherited;
  if FMaxInactiveTrack <> nil then FMaxInactiveTrack.MakeBufDrawable;
  if FMaxThumb <> nil then FMaxThumb.MakeBufDrawable;
end;

{******************************************}
procedure TALRangeTrackBar.ClearBufDrawable;
begin
  inherited;
  if FMaxInactiveTrack <> nil then FMaxInactiveTrack.ClearBufDrawable;
  if FMaxThumb <> nil then FMaxThumb.ClearBufDrawable;
end;

{***************************************************************}
function TALRangeTrackBar.GetLeadingTrack: TALCustomTrack.TTrack;
begin
  Result := FInactiveTrack;
end;

{****************************************************************}
function TALRangeTrackBar.GetTrailingTrack: TALCustomTrack.TTrack;
begin
  Result := FMaxInactiveTrack;
end;

{***********************************************}
function TALRangeTrackBar.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(200, 32);
end;

{************************************************}
function TALRangeTrackBar.MaxValueStored: Boolean;
begin
  Result := not SameValue(MaxValue, FMX.StdActns.DefaultMaxValue, Tepsilon.Vector);
end;

{*****************************************************}
procedure TALRangeTrackBar.SetMax(const Value: Double);
begin
  if not SameValue(GetMax, Value) then begin
    inherited;
    FMaxThumb.FValueRange.Max := Value;
  end;
end;

{*****************************************************}
procedure TALRangeTrackBar.SetMin(const Value: Double);
begin
  if not SameValue(GetMin, Value) then begin
    inherited;
    FMaxThumb.FValueRange.Min := Value;
  end;
end;

{***********************************************************}
procedure TALRangeTrackBar.SetFrequency(const Value: Double);
begin
  if not SameValue(GetFrequency, Value) then begin
    inherited;
    FMaxThumb.FValueRange.Frequency := Value;
  end;
end;

{*************************************************}
procedure TALRangeTrackBar.SetValue(Value: Double);
begin
  if not SameValue(GetValue, Value) then begin
    inherited;
    if (not fThumb.Pressed) and
       (GetValue > (max - Min) / 2) then fThumb.BringToFront;
  end;
end;

{********************************************}
function TALRangeTrackBar.GetMaxValue: Double;
begin
  Result := FMaxThumb.FValueRange.Value;
end;

{****************************************************}
procedure TALRangeTrackBar.SetMaxValue(Value: Double);
begin
  if not SameValue(GetMaxValue, Value) then begin
    FMaxThumb.FValueRange.Value := Value;
    if (not fMaxThumb.Pressed) and
       (GetMaxValue < (max - Min) / 2) then fMaxThumb.BringToFront;
  end;
end;

{**************************************************************}
procedure TALRangeTrackBar.SetViewportSize(const Value: Double);
begin
  if not SameValue(GetViewportSize, Value) then begin
    inherited;
    FMaxThumb.FValueRange.ViewportSize := Value;
  end;
end;

{******************************************************************}
procedure TALRangeTrackBar.SetDoubleBuffered(const AValue: Boolean);
begin
  Inherited;
  if FMaxThumb <> nil then FMaxThumb.DoubleBuffered := AValue;
end;

{************************************************************}
procedure TALRangeTrackBar._SetCanFocus(const Value: Boolean);
begin
  Inherited;
  if FMaxThumb <> nil then FMaxThumb.CanFocus := Value;
end;

{***********************************************************}
procedure TALRangeTrackBar._SetTabStop(const Value: Boolean);
begin
  Inherited;
  if FMaxThumb <> nil then FMaxThumb.TabStop := Value;
end;

{***************************************************************************************************************************************************************************************}
function TALRangeTrackBar.CreateInactiveTrack(const AInactiveTrackClass: TALCustomTrack.TInactiveTrackClass = nil; Const AName: String = 'InactiveTrack'): TALCustomTrack.TInactiveTrack;
begin
  if AInactiveTrackClass = nil then Exit(CreateInactiveTrack(TMinInactiveTrack, AName));
  result := Inherited;
end;

{*****************************************************************************************************************************************************************************}
function TALRangeTrackBar.CreateActiveTrack(const AActiveTrackClass: TALCustomTrack.TActiveTrackClass = nil; Const AName: String = 'ActiveTrack'): TALCustomTrack.TActiveTrack;
begin
  if AActiveTrackClass = nil then Exit(CreateActiveTrack(TActiveTrack, AName));
  result := Inherited;
end;

{***********************************************************************************************************************************************}
function TALRangeTrackBar.CreateThumb(const AThumbClass: TALCustomTrack.TThumbClass = nil; Const AName: String = 'Thumb'): TALCustomTrack.TThumb;
begin
  if AThumbClass = nil then Exit(CreateThumb(TMinThumb, AName));
  result := Inherited;
end;

{***********************************}
procedure TALRangeTrackBar.DoResized;
begin
  inherited;
  if FMaxInactiveTrack <> nil then
    FMaxInactiveTrack.ClearBufDrawable;
end;

{***********************************}
procedure TALRangeTrackBar.DoRealign;
begin
  // Realign is called by TALValueRangeTrack.DoChanged,
  // so we can check here if minValue <= maxValue.
  if (FThumb <> nil) and (FMaxThumb <> nil) and (minValue > MaxValue) then begin
    if fThumb.Pressed then MinValue := MaxValue
    else MaxValue := MinValue;
    exit; // no need to continue, this function will be called again
  end;
  inherited DoRealign;
end;

{************************************}
procedure TALRangeTrackBar.AlignThumb;
begin
  Inherited;
  if FMaxThumb = nil then exit;
  var LMaxThumbPos := GetThumbPos(MaxValue);
  If Orientation = TOrientation.Horizontal then
    FMaxThumb.Position.X := LMaxThumbPos
  else
    FMaxThumb.Position.Y := LMaxThumbPos
end;

{*************************************}
procedure TALRangeTrackBar.AlignTracks;
begin
  if (FThumb = nil) or (FMaxThumb = nil) then exit;
  if Orientation = TOrientation.Horizontal then begin
    if FInactiveTrack <> nil then begin
      FInactiveTrack.Position.X := Padding.Left + (Fthumb.Width / 2);
      FInactiveTrack.Width := FThumb.Position.X - Fthumb.Margins.Left - FInactiveTrack.Position.X;
    end;
    if FActiveTrack <> nil then begin
      FActiveTrack.Position.X := FThumb.Position.X + Fthumb.Width + Fthumb.Margins.Right;
      FActiveTrack.Width := FMaxThumb.Position.X - FMaxthumb.Margins.Left - FActiveTrack.Position.X;
    end;
    if FMaxInactiveTrack <> nil then begin
      FMaxInactiveTrack.Position.X := FMaxThumb.Position.X + FMaxthumb.Width + FMaxthumb.Margins.Right;
      FMaxInactiveTrack.Width := Width - Padding.Right - FMaxInactiveTrack.Position.X - (FMaxthumb.Width / 2);
    end;
  end
  else begin
    if FInactiveTrack <> nil then begin
      FInactiveTrack.Position.Y := Padding.Top + (Fthumb.Height / 2);
      FInactiveTrack.Height := FThumb.Position.Y - Fthumb.Margins.Top - FInactiveTrack.Position.Y;
    end;
    if FActiveTrack <> nil then begin
      FActiveTrack.Position.Y := FThumb.Position.Y + Fthumb.Height + Fthumb.Margins.Bottom;
      FActiveTrack.Height := FMaxThumb.Position.Y - FMaxthumb.Margins.Top - FActiveTrack.Position.Y;
    end;
    if FMaxInactiveTrack <> nil then begin
      FMaxInactiveTrack.Position.Y := FMaxThumb.Position.Y + FMaxthumb.Height + FMaxthumb.Margins.Bottom;
      FMaxInactiveTrack.Height := Height - Padding.Bottom - FMaxInactiveTrack.Position.Y - (FMaxthumb.Height / 2);
    end;
  end;
end;

{*******************************************************************}
procedure TALRangeTrackBar.SetOrientation(const Value: TOrientation);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function SwapTopBottomWithLeftRight(Const ARect: TrectF): TRectF;
  Begin
    Result.Left := ARect.Top;
    Result.Top := ARect.Left;
    Result.Right := ARect.Bottom;
    Result.Bottom := ARect.Right;
  End;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function SwapAlign(Const AAlign: TALAlignLayout): TALAlignLayout;
  Begin
    If AAlign = TALAlignLayout.Vertical then result := TALAlignLayout.Horizontal
    else If AAlign = TALAlignLayout.Horizontal then result := TALAlignLayout.Vertical
    else result := AAlign;
  End;

begin
  if FOrientation <> Value then begin
    if not (csLoading in ComponentState) then begin
      BeginUpdate;
      Try
        inherited;
        if FMaxInactiveTrack <> nil then begin
          FMaxInactiveTrack.Margins.Rect := SwapTopBottomWithLeftRight(FMaxInactiveTrack.Margins.Rect);
          FMaxInactiveTrack.Padding.Rect := SwapTopBottomWithLeftRight(FMaxInactiveTrack.Padding.Rect);
          FMaxInactiveTrack.Align := SwapAlign(FMaxInactiveTrack.Align);
        end;
        if FMaxThumb <> nil then begin
          FMaxThumb.Margins.Rect := SwapTopBottomWithLeftRight(FMaxThumb.Margins.Rect);
          FMaxThumb.padding.Rect := SwapTopBottomWithLeftRight(FMaxThumb.padding.Rect);
          FMaxThumb.TouchTargetExpansion.Rect := SwapTopBottomWithLeftRight(FMaxThumb.TouchTargetExpansion.Rect);
          var LMaxThumbWidth := FMaxThumb.Width;
          FMaxThumb.Width := FMaxThumb.Height;
          FMaxThumb.Height := LMaxThumbWidth;
          FMaxThumb.Align := SwapAlign(FMaxThumb.Align);
        end;
      Finally
        EndUpdate;
      End;
    end
    else begin
      inherited;
      if FMaxInactiveTrack <> nil then
        FMaxInactiveTrack.Align := SwapAlign(FMaxInactiveTrack.Align);
      if FMaxThumb <> nil then
        FMaxThumb.Align := SwapAlign(FMaxThumb.Align);
    end;
  end;
end;

{********************************************************************}
function TALCustomScrollBar.TThumb.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $47000000;
end;

{**********************************************************************}
function TALCustomScrollBar.TThumb.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := Talphacolors.Null;
end;

{****************************************************************************************}
function TALCustomScrollBar.TThumb.TDisabledStateStyle.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $47000000;
end;

{******************************************************************************************}
function TALCustomScrollBar.TThumb.TDisabledStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := Talphacolors.Null;
end;

{*******************************************************************************************}
function TALCustomScrollBar.TThumb.TDisabledStateStyle.TStateLayer.GetDefaultXRadius: Single;
begin
  Result := 0;
end;

{*******************************************************************************************}
function TALCustomScrollBar.TThumb.TDisabledStateStyle.TStateLayer.GetDefaultYRadius: Single;
begin
  Result := 0;
end;

{**********************************************************************************************************}
function TALCustomScrollBar.TThumb.TDisabledStateStyle.CreateFill(const AParent: TALBrush): TALInheritBrush;
begin
  Result := TFill.Create(AParent);
end;

{************************************************************************************************************************}
function TALCustomScrollBar.TThumb.TDisabledStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{*************************************************************************************}
function TALCustomScrollBar.TThumb.TDisabledStateStyle.CreateStateLayer: TALStateLayer;
begin
  Result := TStateLayer.Create;
end;

{***************************************************************************************}
function TALCustomScrollBar.TThumb.THoveredStateStyle.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $47000000;
end;

{*****************************************************************************************}
function TALCustomScrollBar.TThumb.THoveredStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := Talphacolors.Null;
end;

{******************************************************************************************}
function TALCustomScrollBar.TThumb.THoveredStateStyle.TStateLayer.GetDefaultXRadius: Single;
begin
  Result := 0;
end;

{******************************************************************************************}
function TALCustomScrollBar.TThumb.THoveredStateStyle.TStateLayer.GetDefaultYRadius: Single;
begin
  Result := 0;
end;

{*********************************************************************************************************}
function TALCustomScrollBar.TThumb.THoveredStateStyle.CreateFill(const AParent: TALBrush): TALInheritBrush;
begin
  Result := TFill.Create(AParent);
end;

{***********************************************************************************************************************}
function TALCustomScrollBar.TThumb.THoveredStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{************************************************************************************}
function TALCustomScrollBar.TThumb.THoveredStateStyle.CreateStateLayer: TALStateLayer;
begin
  Result := TStateLayer.Create;
end;

{***************************************************************************************}
function TALCustomScrollBar.TThumb.TPressedStateStyle.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $47000000;
end;

{*****************************************************************************************}
function TALCustomScrollBar.TThumb.TPressedStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := Talphacolors.Null;
end;

{******************************************************************************************}
function TALCustomScrollBar.TThumb.TPressedStateStyle.TStateLayer.GetDefaultXRadius: Single;
begin
  Result := 0;
end;

{******************************************************************************************}
function TALCustomScrollBar.TThumb.TPressedStateStyle.TStateLayer.GetDefaultYRadius: Single;
begin
  Result := 0;
end;

{*********************************************************************************************************}
function TALCustomScrollBar.TThumb.TPressedStateStyle.CreateFill(const AParent: TALBrush): TALInheritBrush;
begin
  Result := TFill.Create(AParent);
end;

{***********************************************************************************************************************}
function TALCustomScrollBar.TThumb.TPressedStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{************************************************************************************}
function TALCustomScrollBar.TThumb.TPressedStateStyle.CreateStateLayer: TALStateLayer;
begin
  Result := TStateLayer.Create;
end;

{***************************************************************************************}
function TALCustomScrollBar.TThumb.TFocusedStateStyle.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $47000000;
end;

{*****************************************************************************************}
function TALCustomScrollBar.TThumb.TFocusedStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := Talphacolors.Null;
end;

{******************************************************************************************}
function TALCustomScrollBar.TThumb.TFocusedStateStyle.TStateLayer.GetDefaultXRadius: Single;
begin
  Result := 0;
end;

{******************************************************************************************}
function TALCustomScrollBar.TThumb.TFocusedStateStyle.TStateLayer.GetDefaultYRadius: Single;
begin
  Result := 0;
end;

{*********************************************************************************************************}
function TALCustomScrollBar.TThumb.TFocusedStateStyle.CreateFill(const AParent: TALBrush): TALInheritBrush;
begin
  Result := TFill.Create(AParent);
end;

{***********************************************************************************************************************}
function TALCustomScrollBar.TThumb.TFocusedStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{************************************************************************************}
function TALCustomScrollBar.TThumb.TFocusedStateStyle.CreateStateLayer: TALStateLayer;
begin
  Result := TStateLayer.Create;
end;

{******************************************************************************************************************************************}
function TALCustomScrollBar.TThumb.TStateStyles.CreateDisabledStateStyle(const AParent: TObject): TALCustomTrack.TThumb.TDisabledStateStyle;
begin
  Result := TDisabledStateStyle.Create(AParent);
end;

{****************************************************************************************************************************************}
function TALCustomScrollBar.TThumb.TStateStyles.CreateHoveredStateStyle(const AParent: TObject): TALCustomTrack.TThumb.THoveredStateStyle;
begin
  Result := THoveredStateStyle.Create(AParent);
end;

{****************************************************************************************************************************************}
function TALCustomScrollBar.TThumb.TStateStyles.CreatePressedStateStyle(const AParent: TObject): TALCustomTrack.TThumb.TPressedStateStyle;
begin
  Result := TPressedStateStyle.Create(AParent);
end;

{****************************************************************************************************************************************}
function TALCustomScrollBar.TThumb.TStateStyles.CreateFocusedStateStyle(const AParent: TObject): TALCustomTrack.TThumb.TFocusedStateStyle;
begin
  Result := TFocusedStateStyle.Create(AParent);
end;

{******************************************************}
function TALCustomScrollBar.TThumb.CreateFill: TALBrush;
begin
  Result := TFill.Create;
end;

{**************************************************************}
function TALCustomScrollBar.TThumb.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
end;

{***************************************************************************************}
function TALCustomScrollBar.TThumb.CreateStateStyles: TALCustomTrack.TThumb.TStateStyles;
begin
  Result := TStateStyles.Create(self);
end;

{***********************************************************}
function TALCustomScrollBar.TThumb.GetDefaultXRadius: Single;
begin
  Result := 0;
end;

{***********************************************************}
function TALCustomScrollBar.TThumb.GetDefaultYRadius: Single;
begin
  Result := 0;
end;

{********************************************************}
constructor TALCustomScrollBar.Create(AOwner: TComponent);
begin
  inherited;
  CanFocus := False;
end;

{*************************************************}
function TALCustomScrollBar.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(150, 18);
end;

{**************************************}
procedure TALCustomScrollBar.AlignThumb;
begin
  if FThumb = nil then exit;
  if ViewportSize > 0 then begin
    If Orientation = TOrientation.Horizontal then begin
      FThumb.Width := ALAlignDimensionToPixelRound(
                        System.Math.Min(
                          System.Math.MaxValue(
                            [ViewportSize / (Max - Min) * Width,
                             Height / 2,
                             5{MinThumbSize}]),
                          Width),
                        ALGetScreenScale,
                        Tepsilon.Position);
    end
    else begin
      FThumb.Height := ALAlignDimensionToPixelRound(
                         System.Math.Min(
                           System.Math.MaxValue(
                             [ViewportSize / (Max - Min) * Height,
                              Width / 2,
                              5{MinThumbSize}]),
                           Height),
                         ALGetScreenScale,
                         Tepsilon.Position);
    end;
  end;
  inherited;
end;

{*************************************************************************************************************************************************}
function TALCustomScrollBar.CreateThumb(const AThumbClass: TALCustomTrack.TThumbClass = nil; Const AName: String = 'Thumb'): TALCustomTrack.TThumb;
begin
  if AThumbClass = nil then Exit(CreateThumb(TThumb, AName));
  result := Inherited;
end;

{*****************************************************************************************************************************************************************************************}
function TALCustomScrollBar.CreateInactiveTrack(const AInactiveTrackClass: TALCustomTrack.TInactiveTrackClass = nil; Const AName: String = 'InactiveTrack'): TALCustomTrack.TInactiveTrack;
begin
  Result := Nil;
end;

{*******************************************************************************************************************************************************************************}
function TALCustomScrollBar.CreateActiveTrack(const AActiveTrackClass: TALCustomTrack.TActiveTrackClass = nil; Const AName: String = 'ActiveTrack'): TALCustomTrack.TActiveTrack;
begin
  Result := Nil;
end;

{**********************************************************************************************************************************************************************************************}
function TALCustomScrollBar.CreateValueIndicator(const AValueIndicatorClass: TALCustomTrack.TValueIndicatorClass = nil; Const AName: String = 'ValueIndicator'): TALCustomTrack.TValueIndicator;
begin
  Result := Nil;
end;

{************************************************************************}
function TALBaseCheckBox.TCheckMarkBrush.TMargins.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(3,3,3,3);
end;

{*************************************************}
constructor TALBaseCheckBox.TCheckMarkBrush.Create;
begin
  inherited Create;
  //--
  FColor := DefaultColor;
  FColorKey := DefaultColorKey;
  FResourceName := DefaultResourceName;
  FWrapMode := DefaultWrapMode;
  FThickness := DefaultThickness;
  //--
  FMargins := CreateMargins;
  FMargins.OnChanged := MarginsChanged;
end;

{*************************************************}
destructor TALBaseCheckBox.TCheckMarkBrush.Destroy;
begin
  ALFreeAndNil(FMargins);
  inherited;
end;

{****************************************************************}
function TALBaseCheckBox.TCheckMarkBrush.CreateMargins: TALBounds;
begin
  Result := TMargins.Create;
end;

{********************************************************************}
function TALBaseCheckBox.TCheckMarkBrush.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Black;
end;

{******************************************************************}
function TALBaseCheckBox.TCheckMarkBrush.GetDefaultColorKey: String;
begin
  Result := '';
end;

{**********************************************************************}
function TALBaseCheckBox.TCheckMarkBrush.GetDefaultResourceName: String;
begin
  Result := '';
end;

{****************************************************************************}
function TALBaseCheckBox.TCheckMarkBrush.GetDefaultWrapMode: TALImageWrapMode;
begin
  Result := TALImageWrapMode.Fit;
end;

{*******************************************************************}
function TALBaseCheckBox.TCheckMarkBrush.GetDefaultThickness: Single;
begin
  Result := 2;
end;

{********************************************************************}
procedure TALBaseCheckBox.TCheckMarkBrush.Assign(Source: TPersistent);
begin
  if Source is TCheckMarkBrush then begin
    BeginUpdate;
    Try
      Color := TCheckMarkBrush(Source).Color;
      ColorKey := TCheckMarkBrush(Source).ColorKey;
      ResourceName := TCheckMarkBrush(Source).ResourceName;
      WrapMode := TCheckMarkBrush(Source).WrapMode;
      Thickness := TCheckMarkBrush(Source).Thickness;
      Margins.Assign(TCheckMarkBrush(Source).Margins);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{**********************************************}
procedure TALBaseCheckBox.TCheckMarkBrush.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Color := DefaultColor;
    ColorKey := DefaultColorKey;
    ResourceName := DefaultResourceName;
    WrapMode := DefaultWrapMode;
    Thickness := DefaultThickness;
    Margins.Rect := Margins.DefaultValue;
  finally
    EndUpdate;
  end;
end;

{*****************************************************}
procedure TALBaseCheckBox.TCheckMarkBrush.AlignToPixel;
begin
  BeginUpdate;
  Try
    Thickness := ALAlignDimensionToPixelRound(Thickness, ALGetScreenScale, Tepsilon.vector);
    Margins.Rect := ALAlignEdgesToPixelRound(Margins.Rect, ALGetScreenScale, TEpsilon.Position);
  finally
    EndUpdate;
  end;
end;

{*********************************************************}
procedure TALBaseCheckBox.TCheckMarkBrush.ApplyColorScheme;
begin
  if FColorKey <> '' then begin
    var LColor := TALStyleManager.Instance.GetColor(FColorKey);
    if FColor <> LColor then begin
      FColor := LColor;
      Change;
    end;
  end;
end;

{****************************************************************************************************************************************}
procedure TALBaseCheckBox.TCheckMarkBrush.Interpolate(const ATo: TCheckMarkBrush; const ANormalizedTime: Single; const AReverse: Boolean);
begin
  BeginUpdate;
  Try
    var LPrevColorKey := FColorKey;
    if ATo <> nil then begin
      Color := ALInterpolateColor(Color{Start}, ATo.Color{Stop}, ANormalizedTime);
      if not AReverse then ResourceName := ATo.ResourceName;
      if not AReverse then WrapMode := ATo.WrapMode;
      Thickness := InterpolateSingle(Thickness{Start}, ATo.Thickness{Stop}, ANormalizedTime);
      Margins.Left := InterpolateSingle(Margins.Left{Start}, ATo.Margins.Left{Stop}, ANormalizedTime);
      Margins.Right := InterpolateSingle(Margins.Right{Start}, ATo.Margins.Right{Stop}, ANormalizedTime);
      Margins.Top := InterpolateSingle(Margins.Top{Start}, ATo.Margins.Top{Stop}, ANormalizedTime);
      Margins.Bottom := InterpolateSingle(Margins.Bottom{Start}, ATo.Margins.Bottom{Stop}, ANormalizedTime);
    end
    else begin
      Color := ALInterpolateColor(Color{Start}, DefaultColor{Stop}, ANormalizedTime);
      if not AReverse then ResourceName := DefaultResourceName;
      if not AReverse then WrapMode := DefaultWrapMode;
      Thickness := InterpolateSingle(Thickness{Start}, DefaultThickness{Stop}, ANormalizedTime);
      Margins.Left := InterpolateSingle(Margins.Left{Start}, Margins.DefaultValue.Left{Stop}, ANormalizedTime);
      Margins.Right := InterpolateSingle(Margins.Right{Start}, Margins.DefaultValue.Right{Stop}, ANormalizedTime);
      Margins.Top := InterpolateSingle(Margins.Top{Start}, Margins.DefaultValue.Top{Stop}, ANormalizedTime);
      Margins.Bottom := InterpolateSingle(Margins.Bottom{Start}, Margins.DefaultValue.Bottom{Stop}, ANormalizedTime);
    end;
    FColorKey := LPrevColorKey;
  finally
    EndUpdate;
  end;
end;

{*************************************************************************************************************************************************}
procedure TALBaseCheckBox.TCheckMarkBrush.InterpolateNoChanges(const ATo: TCheckMarkBrush; const ANormalizedTime: Single; const AReverse: Boolean);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime, AReverse);
  Finally
    EndUpdateNoChanges;
  end;
end;

{*************************************************************}
function TALBaseCheckBox.TCheckMarkBrush.HasCheckMark: boolean;
begin
  result := ((Color <> TalphaColors.Null) and
             (CompareValue(FThickness, 0, TEpsilon.Vector) > 0)) or
            (ResourceName <> '');
end;

{**************************************************************}
function TALBaseCheckBox.TCheckMarkBrush.IsColorStored: Boolean;
begin
  result := FColor <> DefaultColor;
end;

{*****************************************************************}
function TALBaseCheckBox.TCheckMarkBrush.IsColorKeyStored: Boolean;
begin
  result := FColorKey <> DefaultColorKey;
end;

{*********************************************************************}
function TALBaseCheckBox.TCheckMarkBrush.IsResourceNameStored: Boolean;
begin
  result := FResourceName <> DefaultResourceName;
end;

{*****************************************************************}
function TALBaseCheckBox.TCheckMarkBrush.IsWrapModeStored: Boolean;
begin
  result := FWrapMode <> DefaultWrapMode;
end;

{******************************************************************}
function TALBaseCheckBox.TCheckMarkBrush.IsThicknessStored: Boolean;
begin
  result := not SameValue(FThickness, DefaultThickness, TEpsilon.Vector);
end;

{***************************************************************************}
procedure TALBaseCheckBox.TCheckMarkBrush.SetColor(const Value: TAlphaColor);
begin
  if fColor <> Value then begin
    fColor := Value;
    FColorKey := '';
    Change;
  end;
end;

{*************************************************************************}
procedure TALBaseCheckBox.TCheckMarkBrush.SetColorKey(const Value: String);
begin
  if FColorKey <> Value then begin
    FColorKey := Value;
    ApplyColorScheme;
  end;
end;

{*****************************************************************************}
procedure TALBaseCheckBox.TCheckMarkBrush.SetResourceName(const Value: String);
begin
  if fResourceName <> Value then begin
    fResourceName := Value;
    Change;
  end;
end;

{***********************************************************************************}
procedure TALBaseCheckBox.TCheckMarkBrush.SetWrapMode(const Value: TALImageWrapMode);
begin
  if fWrapMode <> Value then begin
    fWrapMode := Value;
    Change;
  end;
end;

{**************************************************************************}
procedure TALBaseCheckBox.TCheckMarkBrush.SetThickness(const Value: Single);
begin
  if not SameValue(Value, FThickness, TEpsilon.Vector) then begin
    fThickness := Value;
    Change;
  end;
end;

{***************************************************************************}
procedure TALBaseCheckBox.TCheckMarkBrush.SetMargins(const Value: TALBounds);
begin
  FMargins.Assign(Value);
end;

{************************************************************************}
procedure TALBaseCheckBox.TCheckMarkBrush.MarginsChanged(Sender: TObject);
begin
  change;
end;

{****************************************************************************************}
constructor TALBaseCheckBox.TInheritCheckMarkBrush.Create(const AParent: TCheckMarkBrush);
begin
  inherited create;
  FParent := AParent;
  FInherit := True;
  fSuperseded := False;
end;

{**************************************************************************************}
function TALBaseCheckBox.TInheritCheckMarkBrush.CreateSavedState: TALPersistentObserver;
type
  TInheritCheckMarkBrushClass = class of TInheritCheckMarkBrush;
begin
  result := TInheritCheckMarkBrushClass(classtype).Create(nil{AParent});
end;

{*********************************************************************************}
procedure TALBaseCheckBox.TInheritCheckMarkBrush.SetInherit(const AValue: Boolean);
begin
  If FInherit <> AValue then begin
    FInherit := AValue;
    Change;
  end;
end;

{***************************************************************************}
procedure TALBaseCheckBox.TInheritCheckMarkBrush.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TInheritCheckMarkBrush then begin
      Inherit := TInheritCheckMarkBrush(Source).Inherit;
      fSuperseded := TInheritCheckMarkBrush(Source).fSuperseded;
    end
    else begin
      Inherit := False;
      fSuperseded := False;
    end;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{*****************************************************}
procedure TALBaseCheckBox.TInheritCheckMarkBrush.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Inherit := True;
    fSuperseded := False;
  finally
    EndUpdate;
  end;
end;

{***********************************************************}
procedure TALBaseCheckBox.TInheritCheckMarkBrush.DoSupersede;
begin
  Assign(FParent);
end;

{********************************************************************************************}
procedure TALBaseCheckBox.TInheritCheckMarkBrush.Supersede(Const ASaveState: Boolean = False);
begin
  if ASaveState then SaveState;
  if (FSuperseded) or
     (not inherit) or
     (FParent = nil) then exit;
  BeginUpdate;
  try
    var LParentSuperseded := False;
    if FParent is TInheritCheckMarkBrush then begin
      TInheritCheckMarkBrush(FParent).SupersedeNoChanges(true{ASaveState});
      LParentSuperseded := True;
    end;
    try
      DoSupersede;
    finally
      if LParentSuperseded then
        TInheritCheckMarkBrush(FParent).restoreStateNoChanges;
    end;
    Inherit := False;
    FSuperseded := True;
  finally
    EndUpdate;
  end;
end;

{*****************************************************************************************************}
procedure TALBaseCheckBox.TInheritCheckMarkBrush.SupersedeNoChanges(Const ASaveState: Boolean = False);
begin
  BeginUpdate;
  try
    Supersede(ASaveState);
  finally
    EndUpdateNoChanges;
  end;
end;

{************************************************************************************}
function TALBaseCheckBox.TBaseStateStyle.TStateLayer.TMargins.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(-12,-12,-12,-12);
end;

{****************************************************************************}
function TALBaseCheckBox.TBaseStateStyle.TStateLayer.CreateMargins: TALBounds;
begin
  Result := TMargins.Create;
end;

{*****************************************************************************}
function TALBaseCheckBox.TBaseStateStyle.TStateLayer.GetDefaultXRadius: Single;
begin
  Result := -50;
end;

{*****************************************************************************}
function TALBaseCheckBox.TBaseStateStyle.TStateLayer.GetDefaultYRadius: Single;
begin
  Result := -50;
end;

{*************************************************************************}
constructor TALBaseCheckBox.TBaseStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  if StateStyleParent <> nil then FCheckMark := CreateCheckMark(StateStyleParent.CheckMark)
  else if ControlParent <> nil then FCheckMark := CreateCheckMark(ControlParent.CheckMark)
  else FCheckMark := CreateCheckMark(nil);
  FCheckMark.OnChanged := CheckMarkChanged;
end;

{*************************************************}
destructor TALBaseCheckBox.TBaseStateStyle.Destroy;
begin
  ALFreeAndNil(FCheckMark);
  inherited Destroy;
end;

{***********************************************************************}
function TALBaseCheckBox.TBaseStateStyle.CreateStateLayer: TALStateLayer;
begin
  Result := TStateLayer.Create;
end;

{***************************************************************************************************************}
function TALBaseCheckBox.TBaseStateStyle.CreateCheckMark(const AParent: TCheckMarkBrush): TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{********************************************************************}
procedure TALBaseCheckBox.TBaseStateStyle.Assign(Source: TPersistent);
begin
  if Source is TBaseStateStyle then begin
    BeginUpdate;
    Try
      CheckMark.Assign(TBaseStateStyle(Source).CheckMark);
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{**********************************************}
procedure TALBaseCheckBox.TBaseStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    CheckMark.Reset;
  finally
    EndUpdate;
  end;
end;

{*****************************************************}
procedure TALBaseCheckBox.TBaseStateStyle.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    CheckMark.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{*********************************************************}
procedure TALBaseCheckBox.TBaseStateStyle.ApplyColorScheme;
begin
  BeginUpdate;
  Try
    Inherited;
    CheckMark.ApplyColorScheme;
  finally
    EndUpdate;
  End;
end;

{******************************************************************************************************************************************}
procedure TALBaseCheckBox.TBaseStateStyle.Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single; const AReverse: Boolean);
begin
  {$IF defined(debug)}
  if (ATo <> nil) and (not (ATo is TBaseStateStyle)) then
    Raise Exception.Create('Error F3C72244-894F-4B67-AD86-F24DF5039927');
  {$ENDIF}
  BeginUpdate;
  Try
    inherited Interpolate(ATo, ANormalizedTime, AReverse);
    if ATo <> nil then CheckMark.Interpolate(TBaseStateStyle(ATo).CheckMark, ANormalizedTime, AReverse)
    else if StateStyleParent <> nil then begin
      StateStyleParent.SupersedeNoChanges(true{ASaveState});
      try
        CheckMark.Interpolate(StateStyleParent.CheckMark, ANormalizedTime, AReverse)
      finally
        StateStyleParent.RestoreStateNoChanges;
      end;
    end
    else if ControlParent <> nil then CheckMark.Interpolate(ControlParent.CheckMark, ANormalizedTime, AReverse)
    else CheckMark.Interpolate(nil, ANormalizedTime, AReverse);
  Finally
    EndUpdate;
  End;
end;

{****************************************************}
procedure TALBaseCheckBox.TBaseStateStyle.DoSupersede;
begin
  inherited;
  CheckMark.Supersede;
end;

{****************************************************************************}
function TALBaseCheckBox.TBaseStateStyle.GetStateStyleParent: TBaseStateStyle;
begin
  {$IF defined(debug)}
  if (inherited StateStyleParent <> nil) and
     (not (inherited StateStyleParent is TBaseStateStyle)) then
    raise Exception.Create('StateStyleParent must be of type TBaseStateStyle');
  {$ENDIF}
  result := TBaseStateStyle(inherited StateStyleParent);
end;

{*************************************************************************}
function TALBaseCheckBox.TBaseStateStyle.GetControlParent: TALBaseCheckBox;
begin
  {$IF defined(debug)}
  if (inherited ControlParent <> nil) and
     (not (inherited ControlParent is TALBaseCheckBox)) then
    raise Exception.Create('ControlParent must be of type TALBaseCheckBox');
  {$ENDIF}
  result := TALBaseCheckBox(inherited ControlParent);
end;

{*******************************************************************************************}
procedure TALBaseCheckBox.TBaseStateStyle.SetCheckMark(const AValue: TInheritCheckMarkBrush);
begin
  FCheckMark.Assign(AValue);
end;

{***********************************************************}
function TALBaseCheckBox.TBaseStateStyle.GetInherit: Boolean;
begin
  Result := inherited GetInherit and
            CheckMark.Inherit;
end;

{***************************************************************************}
procedure TALBaseCheckBox.TBaseStateStyle.CheckMarkChanged(ASender: TObject);
begin
  Change;
end;

{********************************************************************}
function TALBaseCheckBox.TDefaultStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 1;
end;

{********************************************************************}
function TALBaseCheckBox.TDisabledStateStyle.IsOpacityStored: Boolean;
begin
  Result := not SameValue(FOpacity, TControl.DefaultDisabledOpacity, TEpsilon.Scale);
end;

{****************************************************************************}
procedure TALBaseCheckBox.TDisabledStateStyle.SetOpacity(const Value: Single);
begin
  if not SameValue(FOpacity, Value, TEpsilon.Scale) then begin
    FOpacity := Value;
    Change;
  end;
end;

{*****************************************************************************}
constructor TALBaseCheckBox.TDisabledStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  FOpacity := TControl.DefaultDisabledOpacity;
end;

{************************************************************************}
procedure TALBaseCheckBox.TDisabledStateStyle.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TDisabledStateStyle then
      Opacity := TDisabledStateStyle(Source).Opacity
    else
      Opacity := TControl.DefaultDisabledOpacity;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{**************************************************}
procedure TALBaseCheckBox.TDisabledStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Opacity := TControl.DefaultDisabledOpacity;
  finally
    EndUpdate;
  end;
end;

{***************************************************************}
function TALBaseCheckBox.TDisabledStateStyle.GetInherit: Boolean;
begin
  // Opacity is not part of the GetInherit function because it updates the
  // disabledOpacity of the base control immediately every time it changes.
  // Essentially, it acts merely as a link to the disabledOpacity of the base control.
  Result := inherited GetInherit;
end;

{*********************************************************************}
function TALBaseCheckBox.TDisabledStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 2;
end;

{********************************************************************}
function TALBaseCheckBox.THoveredStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 3;
end;

{********************************************************************}
function TALBaseCheckBox.TPressedStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 4;
end;

{********************************************************************}
function TALBaseCheckBox.TFocusedStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 5;
end;

{******************************************************************************}
constructor TALBaseCheckBox.TCheckStateStyles.Create(const AParent: TALControl);
begin
  inherited Create;
  //--
  FDefault := CreateDefaultStateStyle(AParent);
  FDefault.OnChanged := DefaultChanged;
  //--
  FDisabled := CreateDisabledStateStyle(FDefault);
  FDisabled.OnChanged := DisabledChanged;
  //--
  FHovered := CreateHoveredStateStyle(FDefault);
  FHovered.OnChanged := HoveredChanged;
  //--
  FPressed := CreatePressedStateStyle(FDefault);
  FPressed.OnChanged := PressedChanged;
  //--
  FFocused := CreateFocusedStateStyle(FDefault);
  FFocused.OnChanged := FocusedChanged;
end;

{***************************************************}
destructor TALBaseCheckBox.TCheckStateStyles.Destroy;
begin
  ALFreeAndNil(FDefault);
  ALFreeAndNil(FDisabled);
  ALFreeAndNil(FHovered);
  ALFreeAndNil(FPressed);
  ALFreeAndNil(FFocused);
  inherited Destroy;
end;

{*********************************************************************************}
function TALBaseCheckBox.TCheckStateStyles.CreateSavedState: TALPersistentObserver;
type
  TCheckStateStylesClass = class of TCheckStateStyles;
begin
  result := TCheckStateStylesClass(classtype).Create(nil{AParent});
end;

{*************************************************************************************************************}
function TALBaseCheckBox.TCheckStateStyles.CreateDefaultStateStyle(const AParent: TObject): TDefaultStateStyle;
begin
  Result := TDefaultStateStyle.Create(AParent);
end;

{***************************************************************************************************************}
function TALBaseCheckBox.TCheckStateStyles.CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle;
begin
  Result := TDisabledStateStyle.Create(AParent);
end;

{*************************************************************************************************************}
function TALBaseCheckBox.TCheckStateStyles.CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle;
begin
  Result := THoveredStateStyle.Create(AParent);
end;

{*************************************************************************************************************}
function TALBaseCheckBox.TCheckStateStyles.CreatePressedStateStyle(const AParent: TObject): TPressedStateStyle;
begin
  Result := TPressedStateStyle.Create(AParent);
end;

{*************************************************************************************************************}
function TALBaseCheckBox.TCheckStateStyles.CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle;
begin
  Result := TFocusedStateStyle.Create(AParent);
end;

{**********************************************************************}
procedure TALBaseCheckBox.TCheckStateStyles.Assign(Source: TPersistent);
begin
  if Source is TCheckStateStyles then begin
    BeginUpdate;
    Try
      Default.Assign(TCheckStateStyles(Source).Default);
      Disabled.Assign(TCheckStateStyles(Source).Disabled);
      Hovered.Assign(TCheckStateStyles(Source).Hovered);
      Pressed.Assign(TCheckStateStyles(Source).Pressed);
      Focused.Assign(TCheckStateStyles(Source).Focused);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{************************************************}
procedure TALBaseCheckBox.TCheckStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Default.Reset;
    Disabled.Reset;
    Hovered.Reset;
    Pressed.Reset;
    Focused.Reset;
  finally
    EndUpdate;
  end;
end;

{*******************************************************}
procedure TALBaseCheckBox.TCheckStateStyles.AlignToPixel;
begin
  BeginUpdate;
  Try
    Default.AlignToPixel;
    Disabled.AlignToPixel;
    Hovered.AlignToPixel;
    Pressed.AlignToPixel;
    Focused.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{***********************************************************}
procedure TALBaseCheckBox.TCheckStateStyles.ApplyColorScheme;
begin
  BeginUpdate;
  Try
    Default.ApplyColorScheme;
    Disabled.ApplyColorScheme;
    Hovered.ApplyColorScheme;
    Pressed.ApplyColorScheme;
    Focused.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{***********************************************************}
procedure TALBaseCheckBox.TCheckStateStyles.ClearBufDrawable;
begin
  Default.ClearBufDrawable;
  Disabled.ClearBufDrawable;
  Hovered.ClearBufDrawable;
  Pressed.ClearBufDrawable;
  Focused.ClearBufDrawable;
end;

{***************************************************************************************}
procedure TALBaseCheckBox.TCheckStateStyles.SetDefault(const AValue: TDefaultStateStyle);
begin
  FDefault.Assign(AValue);
end;

{*****************************************************************************************}
procedure TALBaseCheckBox.TCheckStateStyles.SetDisabled(const AValue: TDisabledStateStyle);
begin
  FDisabled.Assign(AValue);
end;

{***************************************************************************************}
procedure TALBaseCheckBox.TCheckStateStyles.SetHovered(const AValue: THoveredStateStyle);
begin
  FHovered.Assign(AValue);
end;

{***************************************************************************************}
procedure TALBaseCheckBox.TCheckStateStyles.SetPressed(const AValue: TPressedStateStyle);
begin
  FPressed.Assign(AValue);
end;

{***************************************************************************************}
procedure TALBaseCheckBox.TCheckStateStyles.SetFocused(const AValue: TFocusedStateStyle);
begin
  FFocused.Assign(AValue);
end;

{***************************************************************************}
procedure TALBaseCheckBox.TCheckStateStyles.DefaultChanged(ASender: TObject);
begin
  Change;
end;

{****************************************************************************}
procedure TALBaseCheckBox.TCheckStateStyles.DisabledChanged(ASender: TObject);
begin
  Change;
end;

{***************************************************************************}
procedure TALBaseCheckBox.TCheckStateStyles.HoveredChanged(ASender: TObject);
begin
  Change;
end;

{***************************************************************************}
procedure TALBaseCheckBox.TCheckStateStyles.PressedChanged(ASender: TObject);
begin
  Change;
end;

{***************************************************************************}
procedure TALBaseCheckBox.TCheckStateStyles.FocusedChanged(ASender: TObject);
begin
  Change;
end;

{*************************************************************************}
constructor TALBaseCheckBox.TStateStyles.Create(const AParent: TALControl);
begin
  inherited Create(AParent);
  //--
  FChecked := CreateCheckedStateStyles(AParent);
  FChecked.OnChanged := CheckedChanged;
  //--
  FUnchecked := CreateUnCheckedStateStyles(AParent);
  FUnchecked.OnChanged := UncheckedChanged;
end;

{**********************************************}
destructor TALBaseCheckBox.TStateStyles.Destroy;
begin
  ALFreeAndNil(FChecked);
  ALFreeAndNil(FUnchecked);
  inherited Destroy;
end;

{*************************************************************************************}
function TALBaseCheckBox.TStateStyles.CreateTransition: TALBaseStateStyles.TTransition;
begin
  result := TTransition.Create(Self);
end;

{***********************************************************************************************************}
function TALBaseCheckBox.TStateStyles.CreateCheckedStateStyles(const AParent: TALControl): TCheckStateStyles;
begin
  Result := TCheckStateStyles.Create(AParent);
end;

{*************************************************************************************************************}
function TALBaseCheckBox.TStateStyles.CreateUncheckedStateStyles(const AParent: TALControl): TCheckStateStyles;
begin
  Result := TCheckStateStyles.Create(AParent);
end;

{*****************************************************************}
procedure TALBaseCheckBox.TStateStyles.Assign(Source: TPersistent);
begin
  if Source is TStateStyles then begin
    BeginUpdate;
    Try
      Checked.Assign(TStateStyles(Source).Checked);
      Unchecked.Assign(TStateStyles(Source).Unchecked);
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{*******************************************}
procedure TALBaseCheckBox.TStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Checked.reset;
    Unchecked.reset;
  finally
    EndUpdate;
  end;
end;

{**************************************************}
procedure TALBaseCheckBox.TStateStyles.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    Checked.AlignToPixel;
    Unchecked.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{******************************************************}
procedure TALBaseCheckBox.TStateStyles.ApplyColorScheme;
begin
  BeginUpdate;
  Try
    inherited;
    Checked.ApplyColorScheme;
    Unchecked.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{******************************************************}
procedure TALBaseCheckBox.TStateStyles.ClearBufDrawable;
begin
  inherited;
  Checked.ClearBufDrawable;
  Unchecked.ClearBufDrawable;
end;

{**************************************************************************}
function TALBaseCheckBox.TStateStyles.GetCurrentRawStyle: TALBaseStateStyle;
begin
  if Parent.Checked then begin
    if Not Parent.Enabled then Result := Checked.Disabled
    else if Parent.Pressed then Result := Checked.Pressed
    else if Parent.IsFocused then Result := Checked.Focused
    else if Parent.IsMouseOver then Result := Checked.Hovered
    else result := Checked.Default;
  end
  else begin
    if Not Parent.Enabled then Result := UnChecked.Disabled
    else if Parent.Pressed then Result := UnChecked.Pressed
    else if Parent.IsFocused then Result := UnChecked.Focused
    else if Parent.IsMouseOver then Result := UnChecked.Hovered
    else result := UnChecked.Default;
  end;
end;

{***************************************************************}
function TALBaseCheckBox.TStateStyles.GetParent: TALBaseCheckBox;
begin
  Result := TALBaseCheckBox(inherited Parent);
end;

{****************************************************************************}
function TALBaseCheckBox.TStateStyles.GetTransition: TStateStyles.TTransition;
begin
  Result := TStateStyles.TTransition(inherited Transition);
end;

{*******************************************************************************************}
procedure TALBaseCheckBox.TStateStyles.SetTransition(const AValue: TStateStyles.TTransition);
begin
  inherited Transition := AValue;
end;

{*********************************************************************************}
procedure TALBaseCheckBox.TStateStyles.SetChecked(const AValue: TCheckStateStyles);
begin
  FChecked.Assign(AValue);
end;

{***********************************************************************************}
procedure TALBaseCheckBox.TStateStyles.SetUnchecked(const AValue: TCheckStateStyles);
begin
  FUnchecked.Assign(AValue);
end;

{**********************************************************************}
procedure TALBaseCheckBox.TStateStyles.CheckedChanged(ASender: TObject);
begin
  Change;
end;

{************************************************************************}
procedure TALBaseCheckBox.TStateStyles.UncheckedChanged(ASender: TObject);
begin
  Change;
end;

{*****************************************************}
constructor TALBaseCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  //--
  SetAcceptsControls(False);
  CanFocus := True;
  Cursor := crHandPoint;
  //--
  FCheckMark := CreateCheckMark;
  FCheckMark.OnChanged := CheckMarkChanged;
  //--
  FChecked := False;
  FDoubleBuffered := true;
  FXRadius := DefaultXRadius;
  FYRadius := DefaultYRadius;
  FCacheIndex := 0;
  FCacheEngine := nil;
  {$IF NOT DEFINED(ALSkiaCanvas)}
  FRenderTargetSurface := ALNullSurface;
  FRenderTargetCanvas := ALNullCanvas;
  FRenderTargetDrawable := ALNullDrawable;
  {$ENDIF}
  FOnChange := nil;
  //--
  // Must be created at the end because it requires FCheckMark to
  // be already created.
  FStateStyles := CreateStateStyles;
  FStateStyles.OnChanged := StateStylesChanged;
end;

{*********************************}
destructor TALBaseCheckBox.Destroy;
begin
  ALFreeAndNil(FStateStyles);
  ALFreeAndNil(FCheckMark);
  {$IF NOT DEFINED(ALSkiaCanvas)}
  ClearRenderTargets;
  {$ENDIF}
  inherited;
end;

{****************************************************************}
procedure TALBaseCheckBox.Assign(Source: TPersistent{TALControl});
begin
  BeginUpdate;
  Try
    if Source is TALBaseCheckBox then begin
      StateStyles.Assign(TALBaseCheckBox(Source).StateStyles);
      CheckMark.Assign(TALBaseCheckBox(Source).CheckMark);
      Checked := TALBaseCheckBox(Source).Checked;
      XRadius := TALBaseCheckBox(Source).XRadius;
      YRadius := TALBaseCheckBox(Source).YRadius;
      CacheIndex := TALBaseCheckBox(Source).CacheIndex;
      CacheEngine := TALBaseCheckBox(Source).CacheEngine;
      OnChange := TALBaseCheckBox(Source).OnChange;
    end
    else
      ALAssignError(Source{ASource}, Self{ADest});
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{********************************************************}
function TALBaseCheckBox.CreateCheckMark: TCheckMarkBrush;
begin
  Result := TCheckMarkBrush.Create;
end;

{*******************************************************}
function TALBaseCheckBox.CreateStateStyles: TStateStyles;
begin
  Result := TStateStyles.Create(self);
end;

{*************************************}
procedure TALBaseCheckBox.AlignToPixel;
begin
  BeginUpdate;
  try
    inherited;
    StateStyles.AlignToPixel;
    CheckMark.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{*****************************************}
procedure TALBaseCheckBox.ApplyColorScheme;
begin
  BeginUpdate;
  Try
    inherited;
    StateStyles.ApplyColorScheme;
    CheckMark.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{*************************************************}
function TALBaseCheckBox.GetCacheSubIndex: Integer;
begin
  Result := 0;
end;

{**************************************************}
function TALBaseCheckBox.GetDoubleBuffered: boolean;
begin
  result := fDoubleBuffered;
end;

{*****************************************************************}
procedure TALBaseCheckBox.SetDoubleBuffered(const AValue: Boolean);
begin
  if AValue <> fDoubleBuffered then begin
    fDoubleBuffered := AValue;
    if not fDoubleBuffered then ClearBufDrawable
    {$IF NOT DEFINED(ALSkiaCanvas)}
    else ClearRenderTargets;
    {$ENDIF}
  end;
end;

{*******************************************}
function TALBaseCheckBox.GetChecked: Boolean;
begin
  Result := FChecked;
end;

{*********************************************************}
procedure TALBaseCheckBox.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then begin
    FChecked := Value;
    if FChecked then DisabledOpacity := StateStyles.Checked.Disabled.opacity
    else DisabledOpacity := StateStyles.Unchecked.Disabled.opacity;
    DoChanged;
  end;
end;

{*******************************************************************}
procedure TALBaseCheckBox.SetCheckMark(const Value: TCheckMarkBrush);
begin
  FCheckMark.Assign(Value);
end;

{*******************************************************************}
procedure TALBaseCheckBox.SetStateStyles(const AValue: TStateStyles);
begin
  FStateStyles.Assign(AValue);
end;

{************************************************}
function TALBaseCheckBox.IsXRadiusStored: Boolean;
begin
  Result := not SameValue(FXRadius, DefaultXRadius, TEpsilon.Vector);
end;

{************************************************}
function TALBaseCheckBox.IsYRadiusStored: Boolean;
begin
  Result := not SameValue(FYRadius, DefaultYRadius, TEpsilon.Vector);
end;

{*************************************************}
function TALBaseCheckBox.GetDefaultXRadius: Single;
begin
  Result := 0;
end;

{*************************************************}
function TALBaseCheckBox.GetDefaultYRadius: Single;
begin
  Result := 0;
end;

{********************************************************}
procedure TALBaseCheckBox.SetXRadius(const Value: Single);
begin
  if not SameValue(FXRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FXRadius := Value;
    Repaint;
  end;
end;

{********************************************************}
procedure TALBaseCheckBox.SetYRadius(const Value: Single);
begin
  if not SameValue(FYRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FYRadius := Value;
    Repaint;
  end;
end;

{**********************************************************}
procedure TALBaseCheckBox.CheckMarkChanged(Sender: TObject);
begin
  ClearBufDrawable;
  Repaint;
end;

{*****************************************************}
procedure TALBaseCheckBox.FillChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{*******************************************************}
procedure TALBaseCheckBox.StrokeChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{*******************************************************}
procedure TALBaseCheckBox.ShadowChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{************************************************************}
procedure TALBaseCheckBox.StateStylesChanged(Sender: TObject);
begin
  ClearBufDrawable;
  if Checked then DisabledOpacity := StateStyles.Checked.Disabled.opacity
  else DisabledOpacity := StateStyles.Unchecked.Disabled.opacity;
  Repaint;
end;

{*******************************************}
procedure TALBaseCheckBox.IsMouseOverChanged;
begin
  inherited;
  StateStyles.Transition.Start;
  repaint;
end;

{*****************************************}
procedure TALBaseCheckBox.IsFocusedChanged;
begin
  inherited;
  StateStyles.Transition.Start;
  repaint;
end;

{***************************************}
procedure TALBaseCheckBox.PressedChanged;
begin
  inherited;
  StateStyles.Transition.Start;
  repaint;
end;

{*************************************************************************************************}
procedure TALBaseCheckBox.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
begin
  inherited;
  if (KeyChar = ' ') then begin
    Click; // Emulate mouse click to perform Action.OnExecute
    KeyChar := #0;
  end;
end;

{*************************************}
procedure TALBaseCheckBox.DoClickSound;
begin
  if (ClickSound=TALClickSoundMode.Always) or
     ((ClickSound=TALClickSoundMode.Default) and ALGlobalClickSoundEnabled) then
    ALPlayClickSound;
end;

{******************************}
procedure TALBaseCheckBox.Click;
begin
  if StateStyles.Transition.Running and StateStyles.Transition.DelayClick then begin
    Checked := not Checked;
    StateStyles.Transition.ClickDelayed := True
  end
  else begin
    if not StateStyles.Transition.ClickDelayed then
      Checked := not Checked;
    inherited click;
  end;
end;

{**********************************************}
function TALBaseCheckBox.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(18, 18);
end;

{**********************************}
procedure TALBaseCheckBox.DoChanged;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  Repaint;
end;

{**********************************}
procedure TALBaseCheckBox.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{*****************************************}
procedure TALBaseCheckBox.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (FStateStyles <> nil) and
     (not (csDestroying in ComponentState)) and
     ((not ALIsDrawableNull(FStateStyles.Checked.Default.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Disabled.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Hovered.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Pressed.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Focused.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Default.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Disabled.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Hovered.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Pressed.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Focused.FBufDrawable))) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  if FStateStyles <> nil then
    FStateStyles.ClearBufDrawable;
end;

{****************************************}
procedure TALBaseCheckBox.MakeBufDrawable;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _DoMakeBufDrawable(const AStateStyle: TBaseStateStyle): boolean;
  begin
    if (not ALIsDrawableNull(AStateStyle.FBufDrawable)) then exit(False);
    AStateStyle.SupersedeNoChanges(true{ASaveState});
    try

      {$IFDEF debug}
      ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Style: ' + AStateStyle.ClassName + ' | Width: ' + ALFloatToStrW(Width)+ ' | Height: ' + ALFloatToStrW(Height));
      {$endif}

      CreateBufDrawable(
        AStateStyle.FBufDrawable, // var ABufDrawable: TALDrawable;
        AStateStyle.FBufDrawableRect, // var ABufDrawableRect: TRectF;
        ALGetScreenScale * AStateStyle.Scale, // const AScale: Single;
        AStateStyle.Fill, // const AFill: TALBrush;
        AStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
        AStateStyle.Stroke, // const AStroke: TALStrokeBrush;
        AStateStyle.CheckMark, // const ACheckMark: TCheckMarkBrush;
        AStateStyle.Shadow); // const AShadow: TALShadow);

      // LStateStyle.FBufDrawableRect must include the LScale
      AStateStyle.FBufDrawableRect.Top := AStateStyle.FBufDrawableRect.Top * AStateStyle.Scale;
      AStateStyle.FBufDrawableRect.right := AStateStyle.FBufDrawableRect.right * AStateStyle.Scale;
      AStateStyle.FBufDrawableRect.left := AStateStyle.FBufDrawableRect.left * AStateStyle.Scale;
      AStateStyle.FBufDrawableRect.bottom := AStateStyle.FBufDrawableRect.bottom * AStateStyle.Scale;

      // Since LStateStyle.FBufDrawableRect can have different dimensions than the main BufDrawableRect (LocalRect)
      // (due to scale), we must center LStateStyle.FBufDrawableRect within the main BufDrawableRect (LocalRect)
      // to ensure that all changes are visually centered.
      var LCenteredRect := AStateStyle.FBufDrawableRect.CenterAt(LocalRect);
      AStateStyle.FBufDrawableRect.Offset(LCenteredRect.Left, LCenteredRect.top);

    finally
      AStateStyle.RestorestateNoChanges;
    end;
    Result := True;
  end;

begin
  //--- Do not create BufDrawable if not DoubleBuffered
  if {$IF not DEFINED(ALDPK)}(not DoubleBuffered){$ELSE}False{$ENDIF} then begin
    ClearBufDrawable;
    exit;
  end;
  //--
  var LSubIndexOffset: Integer;
  var LDefaultStateStyle: TBaseStateStyle;
  if Checked then begin
    LSubIndexOffset := GetCacheSubIndex{+0};
    LDefaultStateStyle := StateStyles.Checked.Default;
  end
  else begin
    LSubIndexOffset := GetCacheSubIndex+5;
    LDefaultStateStyle := StateStyles.UnChecked.Default;
  end;
  //--
  if (CacheIndex = 0) or
     (CacheEngine = nil) or
     (not CacheEngine.HasEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex})) then
    _DoMakeBufDrawable(LDefaultStateStyle);
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if LStateStyle.Inherit then exit;
  if (CacheIndex > 0) and
     (CacheEngine <> nil) and
     (CacheEngine.HasEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex})) then Exit;
  _DoMakeBufDrawable(LStateStyle);
  // No need to center LStateStyle.FBufDrawableRect on the main BufDrawableRect
  // because BufDrawableRect always has the width and height of the localRect.
end;

{**************************************}
procedure TALBaseCheckBox.DrawCheckMark(
            const ACanvas: TALCanvas;
            const AScale: Single;
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AChecked: Boolean;
            const ACheckMark: TCheckMarkBrush);
begin

  var LCanvasMatrix: TMatrix;
  var LCanvasScale: Single;
  if AutoAlignToPixel then ALExtractMatrixFromCanvas(Acanvas, LCanvasMatrix, LCanvasScale)
  else begin
    LCanvasMatrix := TMatrix.Identity;
    LCanvasScale := 1;
  end;
  var LRect := ADstRect;
  LRect.Top := LRect.Top * AScale;
  LRect.right := LRect.right * AScale;
  LRect.left := LRect.left * AScale;
  LRect.bottom := LRect.bottom * AScale;
  if AutoAlignToPixel then
    LRect := ALAlignToPixelRound(LRect, LCanvasMatrix, LCanvasScale, TEpsilon.Position);
  var LScaledMarginsRect := ACheckMark.Margins.Rect;
  LScaledMarginsRect.Left := LScaledMarginsRect.Left * AScale;
  LScaledMarginsRect.right := LScaledMarginsRect.right * AScale;
  LScaledMarginsRect.top := LScaledMarginsRect.top * AScale;
  LScaledMarginsRect.bottom := LScaledMarginsRect.bottom * AScale;
  if AutoAlignToPixel then
    LScaledMarginsRect := ALAlignEdgesToPixelRound(LScaledMarginsRect, LCanvasScale, TEpsilon.Position);
  LRect.Top := LRect.Top + LScaledMarginsRect.top;
  LRect.right := LRect.right - LScaledMarginsRect.right;
  LRect.left := LRect.left + LScaledMarginsRect.left;
  LRect.bottom := LRect.bottom - LScaledMarginsRect.bottom;
  if LRect.IsEmpty then exit;

  // Without ResourceName
  if ACheckMark.ResourceName = '' then begin

    // Exit if no color or no stroke
    var LScaledCheckMarkThickness := ACheckMark.Thickness * AScale;
    if (ACheckMark.Color = TalphaColors.Null) or (CompareValue(LScaledCheckMarkThickness, 0, TEpsilon.position) <= 0) then
      exit;
    if AutoAlignToPixel then
      LScaledCheckMarkThickness := ALAlignDimensionToPixelRound(LScaledCheckMarkThickness, LCanvasScale, TEpsilon.Position);

    // exit if not checked
    if not Achecked then
      exit;

    // Try to find LPoint2.x so that LPoint1, LPoint2 and LPoint3 form
    // a right triangle
    Var LHalfScaledCheckMarkThickness := ((LScaledCheckMarkThickness / Sqrt(2)) / 2);
    Var LCheckMarkRect := TRectF.Create(0,0,342,270).FitInto(Lrect);
    var LPoint1 := TPointF.Create(LCheckMarkRect.left + LHalfScaledCheckMarkThickness, LCheckMarkRect.top+(LCheckMarkRect.height * 0.5));
    var LPoint2 := TPointF.Create(0, LCheckMarkRect.Bottom - (2*LHalfScaledCheckMarkThickness));
    var LPoint3 := TPointF.Create(LCheckMarkRect.right-LHalfScaledCheckMarkThickness, LCheckMarkRect.top+LHalfScaledCheckMarkThickness);
    // Coefficients for the quadratic equation ax^2 + bx + c = 0
    var a: Single := 1;
    var b: Single := -(LPoint1.X + LPoint3.X);
    var c: Single := LPoint1.X * LPoint3.X + LPoint1.Y * LPoint3.Y - LPoint1.Y * LPoint2.Y - LPoint2.Y * LPoint3.Y + Sqr(LPoint2.Y);
    // Calculate the discriminant
    var LDiscriminant: Single := b * b - 4 * a * c;
    // Check if there are real solutions
    if LDiscriminant < 0 then begin
      // No real solution so use place LPoint2.x
      // at 33% from the left border
      LPoint2.x := LCheckMarkRect.Left + (LCheckMarkRect.Width * 0.33);
    end
    else begin
      // 2 solutions:
      //     (-b - Sqrt(LDiscriminant)) / (2 * a);
      //     (-b + Sqrt(LDiscriminant)) / (2 * a);
      // Use only the first one
      LPoint2.x := (-b - Sqrt(LDiscriminant)) / (2 * a);
    end;

    {$REGION 'SKIA'}
    {$IF defined(ALSkiaEngine)}

    // Create LPaint
    var LPaint := ALSkCheckHandle(sk4d_paint_create);
    try

      // Requests, but does not require, that edge pixels draw opaque or with partial transparency.
      sk4d_paint_set_antialias(LPaint, true);
      // Sets whether the geometry is filled, stroked, or filled and stroked.
      sk4d_paint_set_dither(LPaint, true);

      // Stroke with solid color
      sk4d_paint_set_style(LPaint, sk_paintstyle_t.STROKE_SK_PAINTSTYLE);
      sk4d_paint_set_stroke_width(LPaint, LScaledCheckMarkThickness);
      sk4d_paint_set_color(LPaint, ACheckMark.Color);
      var LPathBuilder := ALSkCheckHandle(sk4d_pathbuilder_create);
      try
        sk4d_pathbuilder_move_to(LPathBuilder, @LPoint1);
        sk4d_pathbuilder_line_to(LPathBuilder, @LPoint2);
        sk4d_pathbuilder_line_to(LPathBuilder, @LPoint3);
        var LPath := sk4d_pathbuilder_detach(LPathBuilder);
        try
          sk4d_canvas_draw_Path(ACanvas, LPath, LPaint);
        finally
          sk4d_path_destroy(LPath);
        end;
      finally
        sk4d_pathbuilder_destroy(LPathBuilder);
      end;

    finally
      sk4d_paint_destroy(LPaint);
    end;

    {$ENDIF}
    {$ENDREGION}

    {$REGION 'ANDROID'}
    {$IF (defined(ANDROID)) and (not defined(ALSkiaEngine))}

    // Create LPaint
    var LPaint := TJPaint.JavaClass.init;
    LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
    LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
    LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

    // Stroke with solid color
    LPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
    LPaint.setStrokeWidth(LScaledCheckMarkThickness);
    LPaint.setColor(integer(ACheckMark.Color));
    var LPath := TJPath.Create;
    LPath.moveTo(LPoint1.x, LPoint1.y);
    LPath.LineTo(LPoint2.x, LPoint2.y);
    LPath.LineTo(LPoint3.x, LPoint3.y);
    aCanvas.drawPath(LPath,LPaint);
    LPath := nil;

    //free the paint
    LPaint := nil;

    {$ENDIF}
    {$ENDREGION}

    {$REGION 'APPLEOS'}
    {$IF (defined(ALAppleOS)) and (not defined(ALSkiaEngine))}

    var LAlphaColor := TAlphaColorCGFloat.Create(ACheckMark.Color);
    CGContextSetRGBStrokeColor(ACanvas, LAlphaColor.R, LAlphaColor.G, LAlphaColor.B, LAlphaColor.A);
    CGContextSetLineWidth(ACanvas, LScaledCheckMarkThickness);

    var LGridHeight := CGBitmapContextGetHeight(ACanvas);

    CGContextBeginPath(ACanvas);
    CGContextMoveToPoint(ACanvas, LPoint1.x, LGridHeight - LPoint1.y);
    CGContextAddLineToPoint(ACanvas, LPoint2.x, LGridHeight - LPoint2.y);
    CGContextAddLineToPoint(ACanvas, LPoint3.x, LGridHeight - LPoint3.y);
    CGContextStrokePath(ACanvas);


    {$ENDIF}
    {$ENDREGION}

    {$REGION 'MSWINDOWS'}
    {$IF (not defined(ANDROID)) and (not defined(ALAppleOS)) and (not defined(ALSkiaEngine))}

    var LSaveState := ACanvas.SaveState;
    try
      ACanvas.Stroke.Color := ACheckMark.Color;
      ACanvas.Stroke.Thickness := LScaledCheckMarkThickness;
      ACanvas.DrawLine(LPoint1, LPoint2, 1{AOpacity});
      ACanvas.DrawLine(LPoint2, LPoint3, 1{AOpacity});
    finally
      ACanvas.RestoreState(LSaveState)
    end;

    {$ENDIF}
    {$ENDREGION}

  end

  // With ResourceName
  else begin

    TALDrawRectangleHelper.Create(ACanvas)
      .SetAlignToPixel(AutoAlignToPixel)
      .SetDstRect(LRect)
      .SetOpacity(AOpacity)
      .SetFillColor(ACheckMark.Color)
      .SetFillResourceName(ACheckMark.ResourceName)
      .SetFillWrapMode(ACheckMark.WrapMode)
      .Draw;

  end;

end;

{******************************************}
Procedure TALBaseCheckBox.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            out ABufDrawableRect: TRectF;
            const AScale: Single;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStroke: TALStrokeBrush;
            const ACheckMark: TCheckMarkBrush;
            const AShadow: TALShadow);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  ABufDrawableRect := LocalRect;
  var LSurfaceRect := ALGetShapeSurfaceRect(
                        ABufDrawableRect, // const ARect: TRectF;
                        AutoAlignToPixel, // const AAlignToPixel: Boolean;
                        AFill, // const AFill: TALBrush;
                        nil, // const AFillResourceStream: TStream;
                        AStateLayer, // const AStateLayer: TALStateLayer;
                        AShadow); // const AShadow: TALShadow): TRectF;
  if ACheckMark.HasCheckMark then begin
    var LCheckMarkMarginsRect := ACheckMark.margins.Rect;
    if AutoAlignToPixel then LCheckMarkMarginsRect := ALAlignEdgesToPixelRound(LCheckMarkMarginsRect, ALGetScreenScale, TEpsilon.Position);
    var LCheckMarkRect := ABufDrawableRect;
    LCheckMarkRect.Inflate(-LCheckMarkMarginsRect.Left, -LCheckMarkMarginsRect.top, -LCheckMarkMarginsRect.right, -LCheckMarkMarginsRect.Bottom);
    LSurfaceRect := TRectF.Union(LCheckMarkRect, LSurfaceRect);
  end;
  ABufDrawableRect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);

  var LSurface: TALSurface;
  var LCanvas: TALCanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    AScale, // const AScale: Single;
    LSurfaceRect.Width, // const w: integer;
    LSurfaceRect.height);// const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      TALDrawRectangleHelper.Create(LCanvas)
        .SetScale(AScale)
        .SetAlignToPixel(AutoAlignToPixel)
        .SetDstRect(ABufDrawableRect)
        .SetFill(AFill)
        .SetStateLayer(AStateLayer, ACheckMark.Color)
        .SetDrawStateLayerOnTop(False)
        .SetStroke(AStroke)
        .SetShadow(AShadow)
        .SetSides(AllSides)
        .SetCorners(AllCorners)
        .SetXRadius(XRadius)
        .SetYRadius(YRadius)
        .Draw;

      DrawCheckMark(
        LCanvas, // const ACanvas: TALCanvas;
        AScale, // const AScale: Single;
        ABufDrawableRect, // const ADstRect: TrectF;
        1, // const AOpacity: Single;
        Checked, // const AChecked: Boolean
        ACheckMark); // const ACheckMark: TCheckMarkBrush;

    finally
      ALCanvasEndScene(LCanvas)
    end;

    ABufDrawable := ALCreateDrawableFromSurface(LSurface);
    // The Shadow or Statelayer are not included in the dimensions of the fBufDrawableRect rectangle.
    // However, the fBufDrawableRect rectangle is offset by the dimensions of the shadow/Statelayer.
    ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);

  finally
    ALFreeAndNilSurface(LSurface, LCanvas);
  end;

end;

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
function TALBaseCheckBox.GetRenderTargetRect(const ARect: TrectF): TRectF;
begin
  if StateStyles.Transition.Running then begin
    Result := ARect;
    if StateStyles.Transition.FromStateStyle <> nil then begin
      var LFromSurfaceRect := ALGetShapeSurfaceRect(
                                ARect, // const ARect: TRectF;
                                AutoAlignToPixel, // const AAlignToPixel: Boolean;
                                _TALBaseStateStyleProtectedAccess(StateStyles.Transition.FromStateStyle).Fill, // const AFill: TALBrush;
                                nil, // const AFillResourceStream: TStream;
                                _TALBaseStateStyleProtectedAccess(StateStyles.Transition.FromStateStyle).StateLayer, // const AStateLayer: TALStateLayer;
                                _TALBaseStateStyleProtectedAccess(StateStyles.Transition.FromStateStyle).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LFromSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
    if StateStyles.Transition.ToStateStyle <> nil then begin
      var LToSurfaceRect := ALGetShapeSurfaceRect(
                              ARect, // const ARect: TRectF;
                              AutoAlignToPixel, // const AAlignToPixel: Boolean;
                              _TALBaseStateStyleProtectedAccess(StateStyles.Transition.ToStateStyle).Fill, // const AFill: TALBrush;
                              nil, // const AFillResourceStream: TStream;
                              _TALBaseStateStyleProtectedAccess(StateStyles.Transition.ToStateStyle).StateLayer, // const AStateLayer: TALStateLayer;
                              _TALBaseStateStyleProtectedAccess(StateStyles.Transition.ToStateStyle).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LToSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
  end
  else begin
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  AutoAlignToPixel, // const AAlignToPixel: Boolean;
                  LStateStyle.Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  LStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
                  LStateStyle.Shadow); // const AShadow: TALShadow): TRectF;
    end
    else begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  AutoAlignToPixel, // const AAlignToPixel: Boolean;
                  Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  nil, // const AStateLayer: TALStateLayer;
                  Shadow); // const AShadow: TALShadow): TRectF;
    end;
  end;
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALBaseCheckBox.InitRenderTargets(var ARect: TrectF);
begin
  var LSurfaceRect := GetRenderTargetRect(ARect);
  ARect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);
  ALInitControlRenderTargets(
    LSurfaceRect, // Const ARect: TrectF;
    FRenderTargetSurface, // var ARenderTargetSurface: TALSurface;
    FRenderTargetCanvas, // var ARenderTargetCanvas: TALCanvas;
    FRenderTargetDrawable); // var ARenderTargetDrawable: TALDrawable):
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALBaseCheckBox.ClearRenderTargets;
begin
  ALFreeAndNilDrawable(FRenderTargetDrawable);
  ALFreeAndNilSurface(FRenderTargetSurface, FRenderTargetCanvas);
end;
{$ENDIF}

{******************************}
procedure TALBaseCheckBox.Paint;
begin

  StateStyles.UpdateLastPaintedRawStyle;

  var LDrawable: TALDrawable := ALNullDrawable;
  var LDrawableRect: TRectF := TRectF.Empty;
  if not StateStyles.Transition.Running then begin
    //--
    var LSubIndexOffset: Integer;
    var LDefaultStateStyle: TBaseStateStyle;
    if Checked then begin
      LSubIndexOffset := GetCacheSubIndex{+0};
      LDefaultStateStyle := StateStyles.Checked.Default;
    end
    else begin
      LSubIndexOffset := GetCacheSubIndex+5;
      LDefaultStateStyle := StateStyles.UnChecked.Default;
    end;
    //--
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      if (CacheIndex <= 0) or
         (CacheEngine = nil) or
         (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
        MakeBufDrawable;
        if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(LStateStyle.FBufDrawable)) then begin
          if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex}, LStateStyle.FBufDrawable{ADrawable}, LStateStyle.FBufDrawableRect{ARect}) then ALFreeAndNilDrawable(LStateStyle.FBufDrawable)
          else LStateStyle.FBufDrawable := ALNullDrawable;
          if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
            raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
        end
        else begin
          LDrawable := LStateStyle.FBufDrawable;
          LDrawableRect := LStateStyle.FBufDrawableRect;
        end;
      end;
    end;
    //--
    If ALIsDrawableNull(LDrawable) then begin
      if (CacheIndex <= 0) or
         (CacheEngine = nil) or
         (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
        if LStateStyle = nil then MakeBufDrawable;
        if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(LDefaultStateStyle.fBufDrawable)) then begin
          if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex}, LDefaultStateStyle.fBufDrawable{ADrawable}, LDefaultStateStyle.fBufDrawableRect{ARect}) then ALFreeAndNilDrawable(LDefaultStateStyle.fBufDrawable)
          else LDefaultStateStyle.fBufDrawable := ALNullDrawable;
          if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
            raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
        end
        else begin
          LDrawable := LDefaultStateStyle.FBufDrawable;
          LDrawableRect := LDefaultStateStyle.FBufDrawableRect;
        end;
      end;
    end;
    //--
  end;

  if ALIsDrawableNull(LDrawable) then begin

    var LCurrentAdjustedStateStyle := TBaseStateStyle(StateStyles.GetCurrentAdjustedStyle);
    if LCurrentAdjustedStateStyle = nil then begin
      inherited Paint;
      exit;
    end;

    {$IF DEFINED(ALSkiaCanvas)}

    var LCanvasSaveState: TCanvasSaveState := ALScaleAndCenterCanvas(
                                                Canvas, // Const ACanvas: TCanvas;
                                                AbsoluteRect, // Const AAbsoluteRect: TRectF;
                                                LCurrentAdjustedStateStyle.Scale, // Const AScale: Single;
                                                true); // Const ASaveState: Boolean);
    try

      var LRect := LocalRect;

      if compareValue(AbsoluteOpacity, 1, Tepsilon.Scale) < 0 then begin
        var LLayerRect := ALGetShapeSurfaceRect(
                            LRect, // const ARect: TrectF;
                            AutoAlignToPixel, // const AAlignToPixel: Boolean;
                            LCurrentAdjustedStateStyle.Fill.Color, // const AFillColor: TAlphaColor;
                            LCurrentAdjustedStateStyle.Fill.Gradient.Colors, // const AFillGradientColors: TArray<TAlphaColor>;
                            LCurrentAdjustedStateStyle.Fill.ResourceName, // const AFillResourceName: String;
                            nil, // const AFillResourceStream: TStream;
                            LCurrentAdjustedStateStyle.Fill.BackgroundMargins.Rect, // Const AFillBackgroundMarginsRect: TRectF;
                            LCurrentAdjustedStateStyle.Fill.ImageMargins.Rect, // Const AFillImageMarginsRect: TRectF;
                            LCurrentAdjustedStateStyle.StateLayer.Opacity, // const AStateLayerOpacity: Single;
                            LCurrentAdjustedStateStyle.StateLayer.Color, // const AStateLayerColor: TAlphaColor;
                            LCurrentAdjustedStateStyle.StateLayer.UseContentColor, // const AStateLayerUseContentColor: Boolean;
                            LCurrentAdjustedStateStyle.StateLayer.Margins.Rect, // Const AStateLayerMarginsRect: TRectF;
                            LCurrentAdjustedStateStyle.Shadow.Color, // const AShadowColor: TAlphaColor;
                            LCurrentAdjustedStateStyle.Shadow.Blur, // const AShadowBlur: Single;
                            LCurrentAdjustedStateStyle.Shadow.OffsetX, // const AShadowOffsetX: Single;
                            LCurrentAdjustedStateStyle.Shadow.OffsetY); // const AShadowOffsetY: Single);
        ALBeginTransparencyLayer(
          TSkCanvasCustom(Canvas).Canvas.Handle, // const aCanvas: TALCanvas;
          LLayerRect, // const ARect: TRectF;
          AbsoluteOpacity); // const AOpacity: Single);
      end;
      try

        TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
          .SetAlignToPixel(AutoAlignToPixel)
          .SetDstRect(LRect)
          .SetFill(LCurrentAdjustedStateStyle.Fill)
          .SetStateLayer(LCurrentAdjustedStateStyle.StateLayer, LCurrentAdjustedStateStyle.CheckMark.Color)
          .SetDrawStateLayerOnTop(False)
          .SetStroke(LCurrentAdjustedStateStyle.Stroke)
          .SetShadow(LCurrentAdjustedStateStyle.Shadow)
          .SetSides(AllSides)
          .SetCorners(AllCorners)
          .SetXRadius(XRadius)
          .SetYRadius(YRadius)
          .Draw;

        if (StateStyles.Transition.Running) and
           (StateStyles.Transition.FadeImage) and
           (StateStyles.Transition.FromStateStyle <> nil) and
           (TBaseStateStyle(StateStyles.Transition.FromStateStyle).CheckMark.ResourceName <> '') and
           (StateStyles.Transition.ToStateStyle <> nil) and
           (TBaseStateStyle(StateStyles.Transition.ToStateStyle).CheckMark.ResourceName <> '') and
           (TBaseStateStyle(StateStyles.Transition.FromStateStyle).CheckMark.ResourceName <> TBaseStateStyle(StateStyles.Transition.ToStateStyle).CheckMark.ResourceName) then begin

          LCurrentAdjustedStateStyle.BeginUpdate;
          try

            LCurrentAdjustedStateStyle.CheckMark.ResourceName := TBaseStateStyle(StateStyles.Transition.FromStateStyle).CheckMark.ResourceName;

            DrawCheckMark(
              TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
              1, // const AScale: Single;
              LRect, // const ADstRect: TrectF;
              1-StateStyles.Transition.CurrentValue, // const AOpacity: Single;
              // Check CheckMark.Color = TalphaColors.Null to enable an interpolation fade-out effect on the checkMark.
              // Without this, the checkMark disappears immediately.
              Checked or (TBaseStateStyle(StateStyles.GetCurrentRawStyle).CheckMark.Color = TalphaColors.Null), // const AChecked: Boolean
              LCurrentAdjustedStateStyle.CheckMark); // const ACheckMark: TCheckMarkBrush;

            LCurrentAdjustedStateStyle.CheckMark.ResourceName := TBaseStateStyle(StateStyles.Transition.ToStateStyle).CheckMark.ResourceName;

            DrawCheckMark(
              TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
              1, // const AScale: Single;
              LRect, // const ADstRect: TrectF;
              StateStyles.Transition.CurrentValue, // const AOpacity: Single;
              // Check CheckMark.Color = TalphaColors.Null to enable an interpolation fade-out effect on the checkMark.
              // Without this, the checkMark disappears immediately.
              Checked or (TBaseStateStyle(StateStyles.GetCurrentRawStyle).CheckMark.Color = TalphaColors.Null), // const AChecked: Boolean
              LCurrentAdjustedStateStyle.CheckMark); // const ACheckMark: TCheckMarkBrush;

          finally
            LCurrentAdjustedStateStyle.EndUpdateNoChanges;
          end;

        end
        else begin

          DrawCheckMark(
            TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
            1, // const AScale: Single;
            LRect, // const ADstRect: TrectF;
            1, // const AOpacity: Single;
            // Check CheckMark.Color = TalphaColors.Null to enable an interpolation fade-out effect on the checkMark.
            // Without this, the checkMark disappears immediately.
            Checked or (TBaseStateStyle(StateStyles.GetCurrentRawStyle).CheckMark.Color = TalphaColors.Null), // const AChecked: Boolean
            LCurrentAdjustedStateStyle.CheckMark); // const ACheckMark: TCheckMarkBrush;

        end;

      finally
        if compareValue(AbsoluteOpacity, 1, Tepsilon.Scale) < 0 then
          ALEndTransparencyLayer(TSkCanvasCustom(Canvas).Canvas.Handle);
      end;

    finally
      if LCanvasSaveState <> nil then
        Canvas.RestoreState(LCanvasSaveState);
    end;

    {$ELSE}

    var LRect := LocalRect;
    InitRenderTargets(LRect);
    if ALCanvasBeginScene(RenderTargetCanvas) then
    try

      ALClearCanvas(RenderTargetCanvas, TAlphaColors.Null);

      TALDrawRectangleHelper.Create(RenderTargetCanvas)
        .SetScale(ALGetScreenScale)
        .SetAlignToPixel(AutoAlignToPixel)
        .SetDstRect(LRect)
        .SetFill(LCurrentAdjustedStateStyle.Fill)
        .SetStateLayer(LCurrentAdjustedStateStyle.StateLayer, LCurrentAdjustedStateStyle.CheckMark.Color)
        .SetDrawStateLayerOnTop(False)
        .SetStroke(LCurrentAdjustedStateStyle.Stroke)
        .SetShadow(LCurrentAdjustedStateStyle.Shadow)
        .SetSides(AllSides)
        .SetCorners(AllCorners)
        .SetXRadius(XRadius)
        .SetYRadius(YRadius)
        .Draw;

      if (StateStyles.Transition.Running) and
         (StateStyles.Transition.FadeImage) and
         (StateStyles.Transition.FromStateStyle <> nil) and
         (TBaseStateStyle(StateStyles.Transition.FromStateStyle).CheckMark.ResourceName <> '') and
         (StateStyles.Transition.ToStateStyle <> nil) and
         (TBaseStateStyle(StateStyles.Transition.ToStateStyle).CheckMark.ResourceName <> '') and
         (TBaseStateStyle(StateStyles.Transition.FromStateStyle).CheckMark.ResourceName <> TBaseStateStyle(StateStyles.Transition.ToStateStyle).CheckMark.ResourceName) then begin

        LCurrentAdjustedStateStyle.BeginUpdate;
        try

          LCurrentAdjustedStateStyle.CheckMark.ResourceName := TBaseStateStyle(StateStyles.Transition.FromStateStyle).CheckMark.ResourceName;

          DrawCheckMark(
            RenderTargetCanvas, // const ACanvas: TALCanvas;
            ALGetScreenScale, // const AScale: Single;
            LRect, // const ADstRect: TrectF;
            1-StateStyles.Transition.CurrentValue, // const AOpacity: Single;
            // Check CheckMark.Color = TalphaColors.Null to enable an interpolation fade-out effect on the checkMark.
            // Without this, the checkMark disappears immediately.
            Checked or (TBaseStateStyle(StateStyles.GetCurrentRawStyle).CheckMark.Color = TalphaColors.Null), // const AChecked: Boolean
            LCurrentAdjustedStateStyle.CheckMark); // const ACheckMark: TCheckMarkBrush;

          LCurrentAdjustedStateStyle.CheckMark.ResourceName := TBaseStateStyle(StateStyles.Transition.ToStateStyle).CheckMark.ResourceName;

          DrawCheckMark(
            RenderTargetCanvas, // const ACanvas: TALCanvas;
            ALGetScreenScale, // const AScale: Single;
            LRect, // const ADstRect: TrectF;
            StateStyles.Transition.CurrentValue, // const AOpacity: Single;
            // Check CheckMark.Color = TalphaColors.Null to enable an interpolation fade-out effect on the checkMark.
            // Without this, the checkMark disappears immediately.
            Checked or (TBaseStateStyle(StateStyles.GetCurrentRawStyle).CheckMark.Color = TalphaColors.Null), // const AChecked: Boolean
            LCurrentAdjustedStateStyle.CheckMark); // const ACheckMark: TCheckMarkBrush;

        finally
          LCurrentAdjustedStateStyle.EndUpdateNoChanges;
        end;

      end
      else begin

        DrawCheckMark(
          RenderTargetCanvas, // const ACanvas: TALCanvas;
          ALGetScreenScale, // const AScale: Single;
          LRect, // const ADstRect: TrectF;
          1, // const AOpacity: Single;
          // Check CheckMark.Color = TalphaColors.Null to enable an interpolation fade-out effect on the checkMark.
          // Without this, the checkMark disappears immediately.
          Checked or (TBaseStateStyle(StateStyles.GetCurrentRawStyle).CheckMark.Color = TalphaColors.Null), // const AChecked: Boolean
          LCurrentAdjustedStateStyle.CheckMark); // const ACheckMark: TCheckMarkBrush;

      end;

    finally
      ALCanvasEndScene(RenderTargetCanvas)
    end;

    ALUpdateDrawableFromSurface(RenderTargetSurface, RenderTargetDrawable);

    // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
    // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
    LRect.Offset(-2*LRect.Left, -2*LRect.Top);

    // LRect must include the LScale
    LRect.Top := LRect.Top * LCurrentAdjustedStateStyle.Scale;
    LRect.right := LRect.right * LCurrentAdjustedStateStyle.Scale;
    LRect.left := LRect.left * LCurrentAdjustedStateStyle.Scale;
    LRect.bottom := LRect.bottom * LCurrentAdjustedStateStyle.Scale;

    // Since LStateStyle.FBufDrawableRect can have different dimensions than the main BufDrawableRect (LocalRect)
    // (due to scale), we must center LStateStyle.FBufDrawableRect within the main BufDrawableRect (LocalRect)
    // to ensure that all changes are visually centered.
    var LCenteredRect := LRect.CenterAt(LocalRect);
    LRect.Offset(LCenteredRect.Left, LCenteredRect.top);

    // We cannot use the matrix because, if we do, ALAlignToPixelRound in ALDrawDrawable
    // will be ineffective since the matrix will no longer be a simple translation matrix.
    // In such a case, TCustomCanvasGpu(ACanvas).DrawTexture may produce border artifacts
    // if the texture is not perfectly pixel-aligned.
    var LDstRect := TRectF.Create(0, 0, ALGetDrawableWidth(RenderTargetDrawable), ALGetDrawableHeight(RenderTargetDrawable));
    LDstRect.Width := (LDstRect.Width / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
    LDstRect.height := (LDstRect.height / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
    LDstRect.SetLocation(
      LRect.Left,
      LRect.Top);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      RenderTargetDrawable, // const ADrawable: TALDrawable;
      LDstRect, // const ADstRect: TrectF; // IN Virtual pixels !
      AbsoluteOpacity); // const AOpacity: Single)

    {$ENDIF}

    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{************************************************}
function TALCheckBox.GetStateStyles: TStateStyles;
begin
  Result := TStateStyles(inherited StateStyles);
end;

{***************************************************************}
procedure TALCheckBox.SetStateStyles(const AValue: TStateStyles);
begin
  inherited StateStyles := AValue;
end;

{*******************************************************************}
function TALCheckBox.CreateStateStyles: TALBaseCheckBox.TStateStyles;
begin
  Result := TStateStyles.Create(Self);
end;

{***********************************************************************}
function TALRadioButton.TCheckMarkBrush.TMargins.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(5,5,5,5);
end;

{***************************************************************}
function TALRadioButton.TCheckMarkBrush.CreateMargins: TALBounds;
begin
  Result := TMargins.Create;
end;

{******************************************************************************}
function TALRadioButton.TInheritCheckMarkBrush.TMargins.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(5,5,5,5);
end;

{**********************************************************************}
function TALRadioButton.TInheritCheckMarkBrush.CreateMargins: TALBounds;
begin
  Result := TMargins.Create;
end;

{*************************************************************************************************************************************************}
function TALRadioButton.TDefaultStateStyle.CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{**************************************************************************************************************************************************}
function TALRadioButton.TDisabledStateStyle.CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{*************************************************************************************************************************************************}
function TALRadioButton.THoveredStateStyle.CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{*************************************************************************************************************************************************}
function TALRadioButton.TPressedStateStyle.CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{*************************************************************************************************************************************************}
function TALRadioButton.TFocusedStateStyle.CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{****************************************************************************************************************************}
function TALRadioButton.TCheckStateStyles.CreateDefaultStateStyle(const AParent: TObject): TALBaseCheckBox.TDefaultStateStyle;
begin
  Result := TDefaultStateStyle.Create(AParent);
end;

{******************************************************************************************************************************}
function TALRadioButton.TCheckStateStyles.CreateDisabledStateStyle(const AParent: TObject): TALBaseCheckBox.TDisabledStateStyle;
begin
  Result := TDisabledStateStyle.Create(AParent);
end;

{****************************************************************************************************************************}
function TALRadioButton.TCheckStateStyles.CreateHoveredStateStyle(const AParent: TObject): TALBaseCheckBox.THoveredStateStyle;
begin
  Result := THoveredStateStyle.Create(AParent);
end;

{****************************************************************************************************************************}
function TALRadioButton.TCheckStateStyles.CreatePressedStateStyle(const AParent: TObject): TALBaseCheckBox.TPressedStateStyle;
begin
  Result := TPressedStateStyle.Create(AParent);
end;

{****************************************************************************************************************************}
function TALRadioButton.TCheckStateStyles.CreateFocusedStateStyle(const AParent: TObject): TALBaseCheckBox.TFocusedStateStyle;
begin
  Result := TFocusedStateStyle.Create(AParent);
end;

{**************************************************************************************************************************}
function TALRadioButton.TStateStyles.CreateCheckedStateStyles(const AParent: TALControl): TALBaseCheckBox.TCheckStateStyles;
begin
  Result := TCheckStateStyles.Create(AParent);
end;

{****************************************************************************************************************************}
function TALRadioButton.TStateStyles.CreateUncheckedStateStyles(const AParent: TALControl): TALBaseCheckBox.TCheckStateStyles;
begin
  Result := TCheckStateStyles.Create(AParent);
end;

{****************************************************}
constructor TALRadioButton.Create(AOwner: TComponent);
begin
  inherited;
  FGroupName := '';
  fMandatory := false;
  TMessageManager.DefaultManager.SubscribeToMessage(TRadioButtonGroupMessage, GroupMessageCall);
end;

{*****************************************}
procedure TALRadioButton.BeforeDestruction;
begin
  if BeforeDestructionExecuted then exit;
  // Unsubscribe from TRadioButtonGroupMessage to stop receiving messages.
  // This must be done in BeforeDestruction rather than in Destroy,
  // because the control might be freed in the background via ALFreeAndNil(..., delayed),
  // and BeforeDestruction is guaranteed to execute on the main thread.
  TMessageManager.DefaultManager.Unsubscribe(TRadioButtonGroupMessage, GroupMessageCall);
  inherited;
end;

{***************************************************************}
procedure TALRadioButton.Assign(Source: TPersistent{TALControl});
begin
  BeginUpdate;
  Try
    if Source is TALRadioButton then begin
      GroupName := TALRadioButton(Source).GroupName;
      Mandatory := TALRadioButton(Source).Mandatory;
    end
    else
      ALAssignError(Source{ASource}, Self{ADest});
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{***********************************************************************}
function TALRadioButton.CreateCheckMark: TALBaseCheckBox.TCheckMarkBrush;
begin
  Result := TCheckMarkBrush.Create;
end;

{**********************************************************************}
function TALRadioButton.CreateStateStyles: TALBaseCheckBox.TStateStyles;
begin
  Result := TStateStyles.Create(self);
end;

{********************************************************}
procedure TALRadioButton.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then begin
    if (csDesigning in ComponentState) and FChecked then inherited SetChecked(Value) // allows check/uncheck in design-mode
    else begin
      if (not value) and fMandatory then exit;
      inherited SetChecked(Value);
      if Value then begin
        var M := TRadioButtonGroupMessage.Create(GroupName);
        TMessageManager.DefaultManager.SendMessage(Self, M, True);
      end;
    end;
  end;
end;

{************************************************}
function TALRadioButton.GetDefaultXRadius: Single;
begin
  Result := -50;
end;

{************************************************}
function TALRadioButton.GetDefaultYRadius: Single;
begin
  Result := -50;
end;

{*********************************************}
function TALRadioButton.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(20, 20);
end;

{*******************************************}
function TALRadioButton.GetGroupName: string;
begin
  Result := FGroupName;
end;

{**********************************************************************************}
procedure TALRadioButton.GroupMessageCall(const Sender: TObject; const M: TMessage);
begin
  if SameText(TRadioButtonGroupMessage(M).GroupName, GroupName) and (Sender <> Self) and (Root <> nil) and
     (not (Sender is TControl) or ((Sender as TControl).Root = Root)) then begin
    var LOldMandatory := fMandatory;
    fMandatory := False;
    try
      Checked := False;
    finally
      fMandatory := LOldMandatory;
    end;
  end;
end;

{***********************************************}
function TALRadioButton.GroupNameStored: Boolean;
begin
  Result := FGroupName <> '';
end;

{*********************************************************}
procedure TALRadioButton.SetGroupName(const Value: string);
begin
  if FGroupName <> Value then
    FGroupName := Value;
end;

{*************************************}
procedure TALRadioButton.DrawCheckMark(
            const ACanvas: TALCanvas;
            const AScale: Single;
            const ADstRect: TrectF;
            const AOpacity: Single;
            const AChecked: Boolean;
            const ACheckMark: TALBaseCheckBox.TCheckMarkBrush);
begin

  var LCanvasMatrix: TMatrix;
  var LCanvasScale: Single;
  if AutoAlignToPixel then ALExtractMatrixFromCanvas(Acanvas, LCanvasMatrix, LCanvasScale)
  else begin
    LCanvasMatrix := TMatrix.Identity;
    LCanvasScale := 1;
  end;
  var LRect := ADstRect;
  LRect.Top := LRect.Top * AScale;
  LRect.right := LRect.right * AScale;
  LRect.left := LRect.left * AScale;
  LRect.bottom := LRect.bottom * AScale;
  if AutoAlignToPixel then
    LRect := ALAlignToPixelRound(LRect, LCanvasMatrix, LCanvasScale, TEpsilon.Position);
  var LScaledMarginsRect := ACheckMark.Margins.Rect;
  LScaledMarginsRect.Left := LScaledMarginsRect.Left * AScale;
  LScaledMarginsRect.right := LScaledMarginsRect.right * AScale;
  LScaledMarginsRect.top := LScaledMarginsRect.top * AScale;
  LScaledMarginsRect.bottom := LScaledMarginsRect.bottom * AScale;
  if AutoAlignToPixel then
    LScaledMarginsRect := ALAlignEdgesToPixelRound(LScaledMarginsRect, LCanvasScale, TEpsilon.Position);
  LRect.Top := LRect.Top + LScaledMarginsRect.top;
  LRect.right := LRect.right - LScaledMarginsRect.right;
  LRect.left := LRect.left + LScaledMarginsRect.left;
  LRect.bottom := LRect.bottom - LScaledMarginsRect.bottom;
  if LRect.IsEmpty then exit;

  // Without ResourceName
  if ACheckMark.ResourceName = '' then begin

    // exit if not checked
    if not AChecked then
      exit;

    TALDrawRectangleHelper.Create(ACanvas)
      .SetAlignToPixel(AutoAlignToPixel)
      .SetDstRect(TRectF.Create(0, 0, 1, 1).FitInto(LRect))
      .SetFillColor(ACheckMark.Color)
      .SetFillResourceName(ACheckMark.ResourceName)
      .SetFillWrapMode(ACheckMark.WrapMode)
      .SetXRadius(-50)
      .SetYRadius(-50)
      .Draw;

  end

  // With ResourceName
  else begin

    TALDrawRectangleHelper.Create(ACanvas)
      .SetAlignToPixel(AutoAlignToPixel)
      .SetDstRect(LRect)
      .SetOpacity(AOpacity)
      .SetFillColor(ACheckMark.Color)
      .SetFillResourceName(ACheckMark.ResourceName)
      .SetFillWrapMode(ACheckMark.WrapMode)
      .Draw;

  end;

end;

{***********************************************************}
function TALSwitch.TTrack.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $ffc5c5c5;
end;

{*************************************************************}
function TALSwitch.TTrack.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.null;
end;

{***************************************************************************}
function TALSwitch.TTrack.TBaseStateStyle.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $ffc5c5c5;
end;

{*****************************************************************************}
function TALSwitch.TTrack.TBaseStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.null;
end;

{******************************************************************************}
function TALSwitch.TTrack.TBaseStateStyle.TStateLayer.GetDefaultXRadius: Single;
begin
  Result := -50;
end;

{******************************************************************************}
function TALSwitch.TTrack.TBaseStateStyle.TStateLayer.GetDefaultYRadius: Single;
begin
  Result := -50;
end;

{*********************************************************************************************}
function TALSwitch.TTrack.TBaseStateStyle.CreateFill(const AParent: TALBrush): TALInheritBrush;
begin
  Result := TFill.Create(AParent);
end;

{***********************************************************************************************************}
function TALSwitch.TTrack.TBaseStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{************************************************************************}
function TALSwitch.TTrack.TBaseStateStyle.CreateStateLayer: TALStateLayer;
begin
  Result := TStateLayer.Create;
end;

{*********************************************************************}
function TALSwitch.TTrack.TDisabledStateStyle.IsOpacityStored: Boolean;
begin
  Result := not SameValue(FOpacity, TControl.DefaultDisabledOpacity, TEpsilon.Scale);
end;

{*****************************************************************************}
procedure TALSwitch.TTrack.TDisabledStateStyle.SetOpacity(const Value: Single);
begin
  if not SameValue(FOpacity, Value, TEpsilon.Scale) then begin
    FOpacity := Value;
    Change;
  end;
end;

{*********************************************************************}
function TALSwitch.TTrack.TDefaultStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 1;
end;

{******************************************************************************}
constructor TALSwitch.TTrack.TDisabledStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  FOpacity := TControl.DefaultDisabledOpacity;
end;

{*************************************************************************}
procedure TALSwitch.TTrack.TDisabledStateStyle.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TDisabledStateStyle then
      Opacity := TDisabledStateStyle(Source).Opacity
    else
      Opacity := TControl.DefaultDisabledOpacity;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{***************************************************}
procedure TALSwitch.TTrack.TDisabledStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Opacity := TControl.DefaultDisabledOpacity;
  finally
    EndUpdate;
  end;
end;

{****************************************************************}
function TALSwitch.TTrack.TDisabledStateStyle.GetInherit: Boolean;
begin
  // Opacity is not part of the GetInherit function because it updates the
  // disabledOpacity of the base control immediately every time it changes.
  // Essentially, it acts merely as a link to the disabledOpacity of the base control.
  Result := inherited GetInherit;
end;

{**********************************************************************}
function TALSwitch.TTrack.TDisabledStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 2;
end;

{*********************************************************************}
function TALSwitch.TTrack.THoveredStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 3;
end;

{*********************************************************************}
function TALSwitch.TTrack.TPressedStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 4;
end;

{*********************************************************************}
function TALSwitch.TTrack.TFocusedStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 5;
end;

{*******************************************************************************}
constructor TALSwitch.TTrack.TCheckStateStyles.Create(const AParent: TALControl);
begin
  inherited Create;
  //--
  FDefault := CreateDefaultStateStyle(AParent);
  FDefault.OnChanged := DefaultChanged;
  //--
  FDisabled := CreateDisabledStateStyle(FDefault);
  FDisabled.OnChanged := DisabledChanged;
  //--
  FHovered := CreateHoveredStateStyle(FDefault);
  FHovered.OnChanged := HoveredChanged;
  //--
  FPressed := CreatePressedStateStyle(FDefault);
  FPressed.OnChanged := PressedChanged;
  //--
  FFocused := CreateFocusedStateStyle(FDefault);
  FFocused.OnChanged := FocusedChanged;
end;

{****************************************************}
destructor TALSwitch.TTrack.TCheckStateStyles.Destroy;
begin
  ALFreeAndNil(FDefault);
  ALFreeAndNil(FDisabled);
  ALFreeAndNil(FHovered);
  ALFreeAndNil(FPressed);
  ALFreeAndNil(FFocused);
  inherited Destroy;
end;

{**********************************************************************************}
function TALSwitch.TTrack.TCheckStateStyles.CreateSavedState: TALPersistentObserver;
type
  TCheckStateStylesClass = class of TCheckStateStyles;
begin
  result := TCheckStateStylesClass(classtype).Create(nil{AParent});
end;

{**************************************************************************************************************}
function TALSwitch.TTrack.TCheckStateStyles.CreateDefaultStateStyle(const AParent: TObject): TDefaultStateStyle;
begin
  Result := TDefaultStateStyle.Create(AParent);
end;

{****************************************************************************************************************}
function TALSwitch.TTrack.TCheckStateStyles.CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle;
begin
  Result := TDisabledStateStyle.Create(AParent);
end;

{**************************************************************************************************************}
function TALSwitch.TTrack.TCheckStateStyles.CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle;
begin
  Result := THoveredStateStyle.Create(AParent);
end;

{**************************************************************************************************************}
function TALSwitch.TTrack.TCheckStateStyles.CreatePressedStateStyle(const AParent: TObject): TPressedStateStyle;
begin
  Result := TPressedStateStyle.Create(AParent);
end;

{**************************************************************************************************************}
function TALSwitch.TTrack.TCheckStateStyles.CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle;
begin
  Result := TFocusedStateStyle.Create(AParent);
end;

{***********************************************************************}
procedure TALSwitch.TTrack.TCheckStateStyles.Assign(Source: TPersistent);
begin
  if Source is TCheckStateStyles then begin
    BeginUpdate;
    Try
      Default.Assign(TCheckStateStyles(Source).Default);
      Disabled.Assign(TCheckStateStyles(Source).Disabled);
      Hovered.Assign(TCheckStateStyles(Source).Hovered);
      Pressed.Assign(TCheckStateStyles(Source).Pressed);
      Focused.Assign(TCheckStateStyles(Source).Focused);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{*************************************************}
procedure TALSwitch.TTrack.TCheckStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Default.Reset;
    Disabled.Reset;
    Hovered.Reset;
    Pressed.Reset;
    Focused.Reset;
  finally
    EndUpdate;
  end;
end;

{********************************************************}
procedure TALSwitch.TTrack.TCheckStateStyles.AlignToPixel;
begin
  BeginUpdate;
  Try
    Default.AlignToPixel;
    Disabled.AlignToPixel;
    Hovered.AlignToPixel;
    Pressed.AlignToPixel;
    Focused.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{************************************************************}
procedure TALSwitch.TTrack.TCheckStateStyles.ApplyColorScheme;
begin
  BeginUpdate;
  Try
    Default.ApplyColorScheme;
    Disabled.ApplyColorScheme;
    Hovered.ApplyColorScheme;
    Pressed.ApplyColorScheme;
    Focused.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{************************************************************}
procedure TALSwitch.TTrack.TCheckStateStyles.ClearBufDrawable;
begin
  Default.ClearBufDrawable;
  Disabled.ClearBufDrawable;
  Hovered.ClearBufDrawable;
  Pressed.ClearBufDrawable;
  Focused.ClearBufDrawable;
end;

{****************************************************************************************}
procedure TALSwitch.TTrack.TCheckStateStyles.SetDefault(const AValue: TDefaultStateStyle);
begin
  FDefault.Assign(AValue);
end;

{******************************************************************************************}
procedure TALSwitch.TTrack.TCheckStateStyles.SetDisabled(const AValue: TDisabledStateStyle);
begin
  FDisabled.Assign(AValue);
end;

{****************************************************************************************}
procedure TALSwitch.TTrack.TCheckStateStyles.SetHovered(const AValue: THoveredStateStyle);
begin
  FHovered.Assign(AValue);
end;

{****************************************************************************************}
procedure TALSwitch.TTrack.TCheckStateStyles.SetPressed(const AValue: TPressedStateStyle);
begin
  FPressed.Assign(AValue);
end;

{****************************************************************************************}
procedure TALSwitch.TTrack.TCheckStateStyles.SetFocused(const AValue: TFocusedStateStyle);
begin
  FFocused.Assign(AValue);
end;

{****************************************************************************}
procedure TALSwitch.TTrack.TCheckStateStyles.DefaultChanged(ASender: TObject);
begin
  Change;
end;

{*****************************************************************************}
procedure TALSwitch.TTrack.TCheckStateStyles.DisabledChanged(ASender: TObject);
begin
  Change;
end;

{****************************************************************************}
procedure TALSwitch.TTrack.TCheckStateStyles.HoveredChanged(ASender: TObject);
begin
  Change;
end;

{****************************************************************************}
procedure TALSwitch.TTrack.TCheckStateStyles.PressedChanged(ASender: TObject);
begin
  Change;
end;

{****************************************************************************}
procedure TALSwitch.TTrack.TCheckStateStyles.FocusedChanged(ASender: TObject);
begin
  Change;
end;

{**************************************************************************}
constructor TALSwitch.TTrack.TStateStyles.Create(const AParent: TALControl);
begin
  inherited Create(AParent);
  //--
  FChecked := CreateCheckedStateStyles(AParent);
  FChecked.OnChanged := CheckedChanged;
  //--
  FUnchecked := CreateUnCheckedStateStyles(AParent);
  FUnchecked.OnChanged := UncheckedChanged;
end;

{***********************************************}
destructor TALSwitch.TTrack.TStateStyles.Destroy;
begin
  ALFreeAndNil(FChecked);
  ALFreeAndNil(FUnchecked);
  inherited Destroy;
end;

{************************************************************************************************************}
function TALSwitch.TTrack.TStateStyles.CreateCheckedStateStyles(const AParent: TALControl): TCheckStateStyles;
begin
  Result := TCheckStateStyles.Create(AParent);
end;

{**************************************************************************************************************}
function TALSwitch.TTrack.TStateStyles.CreateUncheckedStateStyles(const AParent: TALControl): TCheckStateStyles;
begin
  Result := TCheckStateStyles.Create(AParent);
end;

{******************************************************************}
procedure TALSwitch.TTrack.TStateStyles.Assign(Source: TPersistent);
begin
  if Source is TStateStyles then begin
    BeginUpdate;
    Try
      Checked.Assign(TStateStyles(Source).Checked);
      Unchecked.Assign(TStateStyles(Source).Unchecked);
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{********************************************}
procedure TALSwitch.TTrack.TStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Checked.reset;
    Unchecked.reset;
  finally
    EndUpdate;
  end;
end;

{***************************************************}
procedure TALSwitch.TTrack.TStateStyles.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    Checked.AlignToPixel;
    Unchecked.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{*******************************************************}
procedure TALSwitch.TTrack.TStateStyles.ApplyColorScheme;
begin
  BeginUpdate;
  Try
    inherited;
    Checked.ApplyColorScheme;
    Unchecked.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{*******************************************************}
procedure TALSwitch.TTrack.TStateStyles.ClearBufDrawable;
begin
  inherited;
  Checked.ClearBufDrawable;
  Unchecked.ClearBufDrawable;
end;

{***************************************************************************}
function TALSwitch.TTrack.TStateStyles.GetCurrentRawStyle: TALBaseStateStyle;
begin
  if Parent.Checked then begin
    if Not Parent.Enabled then Result := Checked.Disabled
    else if Parent.Pressed then Result := Checked.Pressed
    else if Parent.IsFocused then Result := Checked.Focused
    else if Parent.IsMouseOver then Result := Checked.Hovered
    else result := Checked.Default;
  end
  else begin
    if Not Parent.Enabled then Result := UnChecked.Disabled
    else if Parent.Pressed then Result := UnChecked.Pressed
    else if Parent.IsFocused then Result := UnChecked.Focused
    else if Parent.IsMouseOver then Result := UnChecked.Hovered
    else result := UnChecked.Default;
  end;
end;

{*****************************************************************}
function TALSwitch.TTrack.TStateStyles.GetParent: TALSwitch.TTrack;
begin
  Result := TALSwitch.TTrack(inherited Parent);
end;

{**********************************************************************************}
procedure TALSwitch.TTrack.TStateStyles.SetChecked(const AValue: TCheckStateStyles);
begin
  FChecked.Assign(AValue);
end;

{************************************************************************************}
procedure TALSwitch.TTrack.TStateStyles.SetUnchecked(const AValue: TCheckStateStyles);
begin
  FUnchecked.Assign(AValue);
end;

{***********************************************************************}
procedure TALSwitch.TTrack.TStateStyles.CheckedChanged(ASender: TObject);
begin
  Change;
end;

{*************************************************************************}
procedure TALSwitch.TTrack.TStateStyles.UncheckedChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************}
constructor TALSwitch.TTrack.Create(AOwner: TComponent);
begin
  inherited;
  //--
  SetAcceptsControls(False);
  CanFocus := False;
  Locked := True;
  HitTest := False;
  //--
  FChecked := False;
  FDoubleBuffered := true;
  FXRadius := DefaultXRadius;
  FYRadius := DefaultYRadius;
  FCacheIndex := 0;
  FCacheEngine := nil;
  {$IF NOT DEFINED(ALSkiaCanvas)}
  FRenderTargetSurface := ALNullSurface;
  FRenderTargetCanvas := ALNullCanvas;
  FRenderTargetDrawable := ALNullDrawable;
  {$ENDIF}
  //--
  // Must be created at the end because it requires FCheckMark to
  // be already created.
  FStateStyles := CreateStateStyles;
  FStateStyles.OnChanged := StateStylesChanged;
end;

{**********************************}
destructor TALSwitch.TTrack.Destroy;
begin
  ALFreeAndNil(FStateStyles);
  {$IF NOT DEFINED(ALSkiaCanvas)}
  ClearRenderTargets;
  {$ENDIF}
  inherited;
end;

{*********************************************}
function TALSwitch.TTrack.CreateFill: TALBrush;
begin
  Result := TFill.Create;
end;

{*****************************************************}
function TALSwitch.TTrack.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
end;

{********************************************************}
function TALSwitch.TTrack.CreateStateStyles: TStateStyles;
begin
  Result := TStateStyles.Create(self);
end;

{**************************************}
procedure TALSwitch.TTrack.AlignToPixel;
begin
  BeginUpdate;
  try
    inherited;
    StateStyles.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{******************************************}
procedure TALSwitch.TTrack.ApplyColorScheme;
begin
  BeginUpdate;
  try
    inherited;
    StateStyles.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{***********************************************}
function TALSwitch.TTrack.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(52, 32);
end;

{**************************************************}
function TALSwitch.TTrack.GetCacheSubIndex: Integer;
begin
  // The Thumb uses 11 slots:
  // 0     - Unused
  // 1..5  - Checked state drawables
  // 6..10 - Unchecked state drawables
  Result := 11;
end;

{***************************************************}
function TALSwitch.TTrack.GetDoubleBuffered: boolean;
begin
  result := fDoubleBuffered;
end;

{******************************************************************}
procedure TALSwitch.TTrack.SetDoubleBuffered(const AValue: Boolean);
begin
  if AValue <> fDoubleBuffered then begin
    fDoubleBuffered := AValue;
    if not fDoubleBuffered then ClearBufDrawable
    {$IF NOT DEFINED(ALSkiaCanvas)}
    else ClearRenderTargets;
    {$ENDIF}
  end;
end;

{********************************************}
function TALSwitch.TTrack.GetChecked: Boolean;
begin
  Result := FChecked;
end;

{**********************************************************}
procedure TALSwitch.TTrack.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then begin
    FChecked := Value;
    if FChecked then DisabledOpacity := StateStyles.Checked.Disabled.opacity
    else DisabledOpacity := StateStyles.Unchecked.Disabled.opacity;
    DoChanged;
  end;
end;

{********************************************************************}
procedure TALSwitch.TTrack.SetStateStyles(const AValue: TStateStyles);
begin
  FStateStyles.Assign(AValue);
end;

{*************************************************}
function TALSwitch.TTrack.IsXRadiusStored: Boolean;
begin
  Result := not SameValue(FXRadius, DefaultXRadius, TEpsilon.Vector);
end;

{*************************************************}
function TALSwitch.TTrack.IsYRadiusStored: Boolean;
begin
  Result := not SameValue(FYRadius, DefaultYRadius, TEpsilon.Vector);
end;

{**************************************************}
function TALSwitch.TTrack.GetDefaultXRadius: Single;
begin
  Result := -50;
end;

{**************************************************}
function TALSwitch.TTrack.GetDefaultYRadius: Single;
begin
  Result := -50;
end;

{*********************************************************}
procedure TALSwitch.TTrack.SetXRadius(const Value: Single);
begin
  if not SameValue(FXRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FXRadius := Value;
    Repaint;
  end;
end;

{*********************************************************}
procedure TALSwitch.TTrack.SetYRadius(const Value: Single);
begin
  if not SameValue(FYRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FYRadius := Value;
    Repaint;
  end;
end;

{******************************************************}
procedure TALSwitch.TTrack.FillChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{********************************************************}
procedure TALSwitch.TTrack.StrokeChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{********************************************************}
procedure TALSwitch.TTrack.ShadowChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{*************************************************************}
procedure TALSwitch.TTrack.StateStylesChanged(Sender: TObject);
begin
  ClearBufDrawable;
  if Checked then DisabledOpacity := StateStyles.Checked.Disabled.opacity
  else DisabledOpacity := StateStyles.Unchecked.Disabled.opacity;
  Repaint;
end;

{********************************************}
procedure TALSwitch.TTrack.IsMouseOverChanged;
begin
  inherited;
  StateStyles.Transition.Start;
  repaint;
end;

{******************************************}
procedure TALSwitch.TTrack.IsFocusedChanged;
begin
  inherited;
  StateStyles.Transition.Start;
  repaint;
end;

{****************************************}
procedure TALSwitch.TTrack.PressedChanged;
begin
  inherited;
  StateStyles.Transition.Start;
  repaint;
end;

{***********************************}
procedure TALSwitch.TTrack.DoChanged;
begin
  Repaint;
end;

{***********************************}
procedure TALSwitch.TTrack.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{******************************************}
procedure TALSwitch.TTrack.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (FStateStyles <> nil) and
     (not (csDestroying in ComponentState)) and
     ((not ALIsDrawableNull(FStateStyles.Checked.Default.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Disabled.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Hovered.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Pressed.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Focused.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Default.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Disabled.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Hovered.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Pressed.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Focused.FBufDrawable))) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  if FStateStyles <> nil then
    FStateStyles.ClearBufDrawable;
end;

{*****************************************}
procedure TALSwitch.TTrack.MakeBufDrawable;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _DoMakeBufDrawable(const AStateStyle: TBaseStateStyle): boolean;
  begin
    if (not ALIsDrawableNull(AStateStyle.FBufDrawable)) then exit(False);
    AStateStyle.SupersedeNoChanges(true{ASaveState});
    try

      {$IFDEF debug}
      ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Style: ' + AStateStyle.ClassName + ' | Width: ' + ALFloatToStrW(Width)+ ' | Height: ' + ALFloatToStrW(Height));
      {$endif}

      CreateBufDrawable(
        AStateStyle.FBufDrawable, // var ABufDrawable: TALDrawable;
        AStateStyle.FBufDrawableRect, // var ABufDrawableRect: TRectF;
        ALGetScreenScale * AStateStyle.Scale, // const AScale: Single;
        AStateStyle.Fill, // const AFill: TALBrush;
        AStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
        AStateStyle.Stroke, // const AStroke: TALStrokeBrush;
        AStateStyle.Shadow); // const AShadow: TALShadow);

      // LStateStyle.FBufDrawableRect must include the LScale
      AStateStyle.FBufDrawableRect.Top := AStateStyle.FBufDrawableRect.Top * AStateStyle.Scale;
      AStateStyle.FBufDrawableRect.right := AStateStyle.FBufDrawableRect.right * AStateStyle.Scale;
      AStateStyle.FBufDrawableRect.left := AStateStyle.FBufDrawableRect.left * AStateStyle.Scale;
      AStateStyle.FBufDrawableRect.bottom := AStateStyle.FBufDrawableRect.bottom * AStateStyle.Scale;

      // Since LStateStyle.FBufDrawableRect can have different dimensions than the main BufDrawableRect (LocalRect)
      // (due to scale), we must center LStateStyle.FBufDrawableRect within the main BufDrawableRect (LocalRect)
      // to ensure that all changes are visually centered.
      var LCenteredRect := AStateStyle.FBufDrawableRect.CenterAt(LocalRect);
      AStateStyle.FBufDrawableRect.Offset(LCenteredRect.Left, LCenteredRect.top);

    finally
      AStateStyle.RestorestateNoChanges;
    end;
    Result := True;
  end;

begin
  //--- Do not create BufDrawable if not DoubleBuffered
  if {$IF not DEFINED(ALDPK)}(not DoubleBuffered){$ELSE}False{$ENDIF} then begin
    ClearBufDrawable;
    exit;
  end;
  //--
  var LSubIndexOffset: Integer;
  var LDefaultStateStyle: TBaseStateStyle;
  if Checked then begin
    LSubIndexOffset := GetCacheSubIndex{+0};
    LDefaultStateStyle := StateStyles.Checked.Default;
  end
  else begin
    LSubIndexOffset := GetCacheSubIndex+5;
    LDefaultStateStyle := StateStyles.UnChecked.Default;
  end;
  //--
  if (CacheIndex = 0) or
     (CacheEngine = nil) or
     (not CacheEngine.HasEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex})) then
    _DoMakeBufDrawable(LDefaultStateStyle);
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if LStateStyle.Inherit then exit;
  if (CacheIndex > 0) and
     (CacheEngine <> nil) and
     (CacheEngine.HasEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex})) then Exit;
  _DoMakeBufDrawable(LStateStyle);
  // No need to center LStateStyle.FBufDrawableRect on the main BufDrawableRect
  // because BufDrawableRect always has the width and height of the localRect.
end;

{*******************************************}
Procedure TALSwitch.TTrack.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            out ABufDrawableRect: TRectF;
            const AScale: Single;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  ABufDrawableRect := LocalRect;
  var LSurfaceRect := ALGetShapeSurfaceRect(
                        ABufDrawableRect, // const ARect: TRectF;
                        AutoAlignToPixel, // const AAlignToPixel: Boolean;
                        AFill, // const AFill: TALBrush;
                        nil, // const AFillResourceStream: TStream;
                        AStateLayer, // const AStateLayer: TALStateLayer;
                        AShadow); // const AShadow: TALShadow): TRectF;
  ABufDrawableRect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);

  var LSurface: TALSurface;
  var LCanvas: TALCanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    AScale, // const AScale: Single;
    LSurfaceRect.Width, // const w: integer;
    LSurfaceRect.height);// const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      TALDrawRectangleHelper.Create(LCanvas)
        .SetScale(AScale)
        .SetAlignToPixel(AutoAlignToPixel)
        .SetDstRect(ABufDrawableRect)
        .SetFill(AFill)
        .SetStateLayer(AStateLayer, TAlphaColors.Null)
        .SetDrawStateLayerOnTop(False)
        .SetStroke(AStroke)
        .SetShadow(AShadow)
        .SetSides(AllSides)
        .SetCorners(AllCorners)
        .SetXRadius(XRadius)
        .SetYRadius(YRadius)
        .Draw;

    finally
      ALCanvasEndScene(LCanvas)
    end;

    ABufDrawable := ALCreateDrawableFromSurface(LSurface);
    // The Shadow or Statelayer are not included in the dimensions of the fBufDrawableRect rectangle.
    // However, the fBufDrawableRect rectangle is offset by the dimensions of the shadow/Statelayer.
    ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);

  finally
    ALFreeAndNilSurface(LSurface, LCanvas);
  end;

end;

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
function TALSwitch.TTrack.GetRenderTargetRect(const ARect: TrectF): TRectF;
begin
  if StateStyles.Transition.Running then begin
    Result := ARect;
    if StateStyles.Transition.FromStateStyle <> nil then begin
      var LFromSurfaceRect := ALGetShapeSurfaceRect(
                                ARect, // const ARect: TRectF;
                                AutoAlignToPixel, // const AAlignToPixel: Boolean;
                                _TALBaseStateStyleProtectedAccess(StateStyles.Transition.FromStateStyle).Fill, // const AFill: TALBrush;
                                nil, // const AFillResourceStream: TStream;
                                _TALBaseStateStyleProtectedAccess(StateStyles.Transition.FromStateStyle).StateLayer, // const AStateLayer: TALStateLayer;
                                _TALBaseStateStyleProtectedAccess(StateStyles.Transition.FromStateStyle).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LFromSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
    if StateStyles.Transition.ToStateStyle <> nil then begin
      var LToSurfaceRect := ALGetShapeSurfaceRect(
                              ARect, // const ARect: TRectF;
                              AutoAlignToPixel, // const AAlignToPixel: Boolean;
                              _TALBaseStateStyleProtectedAccess(StateStyles.Transition.ToStateStyle).Fill, // const AFill: TALBrush;
                              nil, // const AFillResourceStream: TStream;
                              _TALBaseStateStyleProtectedAccess(StateStyles.Transition.ToStateStyle).StateLayer, // const AStateLayer: TALStateLayer;
                              _TALBaseStateStyleProtectedAccess(StateStyles.Transition.ToStateStyle).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LToSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
  end
  else begin
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  AutoAlignToPixel, // const AAlignToPixel: Boolean;
                  LStateStyle.Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  LStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
                  LStateStyle.Shadow); // const AShadow: TALShadow): TRectF;
    end
    else begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  AutoAlignToPixel, // const AAlignToPixel: Boolean;
                  Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  nil, // const AStateLayer: TALStateLayer;
                  Shadow); // const AShadow: TALShadow): TRectF;
    end;
  end;
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALSwitch.TTrack.InitRenderTargets(var ARect: TrectF);
begin
  var LSurfaceRect := GetRenderTargetRect(ARect);
  ARect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);
  ALInitControlRenderTargets(
    LSurfaceRect, // Const ARect: TrectF;
    FRenderTargetSurface, // var ARenderTargetSurface: TALSurface;
    FRenderTargetCanvas, // var ARenderTargetCanvas: TALCanvas;
    FRenderTargetDrawable); // var ARenderTargetDrawable: TALDrawable):
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALSwitch.TTrack.ClearRenderTargets;
begin
  ALFreeAndNilDrawable(FRenderTargetDrawable);
  ALFreeAndNilSurface(FRenderTargetSurface, FRenderTargetCanvas);
end;
{$ENDIF}

{*******************************}
procedure TALSwitch.TTrack.Paint;
begin

  StateStyles.UpdateLastPaintedRawStyle;

  var LDrawable: TALDrawable := ALNullDrawable;
  var LDrawableRect: TRectF := TRectF.Empty;
  if not StateStyles.Transition.Running then begin
    //--
    var LSubIndexOffset: Integer;
    var LDefaultStateStyle: TBaseStateStyle;
    if Checked then begin
      LSubIndexOffset := GetCacheSubIndex{+0};
      LDefaultStateStyle := StateStyles.Checked.Default;
    end
    else begin
      LSubIndexOffset := GetCacheSubIndex+5;
      LDefaultStateStyle := StateStyles.UnChecked.Default;
    end;
    //--
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      if (CacheIndex <= 0) or
         (CacheEngine = nil) or
         (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
        MakeBufDrawable;
        if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(LStateStyle.FBufDrawable)) then begin
          if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex}, LStateStyle.FBufDrawable{ADrawable}, LStateStyle.FBufDrawableRect{ARect}) then ALFreeAndNilDrawable(LStateStyle.FBufDrawable)
          else LStateStyle.FBufDrawable := ALNullDrawable;
          if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
            raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
        end
        else begin
          LDrawable := LStateStyle.FBufDrawable;
          LDrawableRect := LStateStyle.FBufDrawableRect;
        end;
      end;
    end;
    //--
    If ALIsDrawableNull(LDrawable) then begin
      if (CacheIndex <= 0) or
         (CacheEngine = nil) or
         (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
        if LStateStyle = nil then MakeBufDrawable;
        if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(LDefaultStateStyle.fBufDrawable)) then begin
          if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex}, LDefaultStateStyle.fBufDrawable{ADrawable}, LDefaultStateStyle.fBufDrawableRect{ARect}) then ALFreeAndNilDrawable(LDefaultStateStyle.fBufDrawable)
          else LDefaultStateStyle.fBufDrawable := ALNullDrawable;
          if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
            raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
        end
        else begin
          LDrawable := LDefaultStateStyle.FBufDrawable;
          LDrawableRect := LDefaultStateStyle.FBufDrawableRect;
        end;
      end;
    end;
    //--
  end;

  if ALIsDrawableNull(LDrawable) then begin

    var LCurrentAdjustedStateStyle := TBaseStateStyle(StateStyles.GetCurrentAdjustedStyle);
    if LCurrentAdjustedStateStyle = nil then begin
      inherited Paint;
      exit;
    end;

    {$IF DEFINED(ALSkiaCanvas)}

    var LCanvasSaveState: TCanvasSaveState := ALScaleAndCenterCanvas(
                                                Canvas, // Const ACanvas: TCanvas;
                                                AbsoluteRect, // Const AAbsoluteRect: TRectF;
                                                LCurrentAdjustedStateStyle.Scale, // Const AScale: Single;
                                                true); // Const ASaveState: Boolean);
    try

      var LRect := LocalRect;

      if compareValue(AbsoluteOpacity, 1, Tepsilon.Scale) < 0 then begin
        var LLayerRect := ALGetShapeSurfaceRect(
                            LRect, // const ARect: TrectF;
                            AutoAlignToPixel, // const AAlignToPixel: Boolean;
                            LCurrentAdjustedStateStyle.Fill.Color, // const AFillColor: TAlphaColor;
                            LCurrentAdjustedStateStyle.Fill.Gradient.Colors, // const AFillGradientColors: TArray<TAlphaColor>;
                            LCurrentAdjustedStateStyle.Fill.ResourceName, // const AFillResourceName: String;
                            nil, // const AFillResourceStream: TStream;
                            LCurrentAdjustedStateStyle.Fill.BackgroundMargins.Rect, // Const AFillBackgroundMarginsRect: TRectF;
                            LCurrentAdjustedStateStyle.Fill.ImageMargins.Rect, // Const AFillImageMarginsRect: TRectF;
                            LCurrentAdjustedStateStyle.StateLayer.Opacity, // const AStateLayerOpacity: Single;
                            LCurrentAdjustedStateStyle.StateLayer.Color, // const AStateLayerColor: TAlphaColor;
                            LCurrentAdjustedStateStyle.StateLayer.UseContentColor, // const AStateLayerUseContentColor: Boolean;
                            LCurrentAdjustedStateStyle.StateLayer.Margins.Rect, // Const AStateLayerMarginsRect: TRectF;
                            LCurrentAdjustedStateStyle.Shadow.Color, // const AShadowColor: TAlphaColor;
                            LCurrentAdjustedStateStyle.Shadow.Blur, // const AShadowBlur: Single;
                            LCurrentAdjustedStateStyle.Shadow.OffsetX, // const AShadowOffsetX: Single;
                            LCurrentAdjustedStateStyle.Shadow.OffsetY); // const AShadowOffsetY: Single);
        ALBeginTransparencyLayer(
          TSkCanvasCustom(Canvas).Canvas.Handle, // const aCanvas: TALCanvas;
          LLayerRect, // const ARect: TRectF;
          AbsoluteOpacity); // const AOpacity: Single);
      end;
      try

        TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
          .SetAlignToPixel(AutoAlignToPixel)
          .SetDstRect(LRect)
          .SetFill(LCurrentAdjustedStateStyle.Fill)
          .SetStateLayer(LCurrentAdjustedStateStyle.StateLayer, TAlphaColors.Null)
          .SetDrawStateLayerOnTop(False)
          .SetStroke(LCurrentAdjustedStateStyle.Stroke)
          .SetShadow(LCurrentAdjustedStateStyle.Shadow)
          .SetSides(AllSides)
          .SetCorners(AllCorners)
          .SetXRadius(XRadius)
          .SetYRadius(YRadius)
          .Draw;

      finally
        if compareValue(AbsoluteOpacity, 1, Tepsilon.Scale) < 0 then
          ALEndTransparencyLayer(TSkCanvasCustom(Canvas).Canvas.Handle);
      end;

    finally
      if LCanvasSaveState <> nil then
        Canvas.RestoreState(LCanvasSaveState);
    end;

    {$ELSE}

    var LRect := LocalRect;
    InitRenderTargets(LRect);
    if ALCanvasBeginScene(RenderTargetCanvas) then
    try

      ALClearCanvas(RenderTargetCanvas, TAlphaColors.Null);

      TALDrawRectangleHelper.Create(RenderTargetCanvas)
        .SetScale(ALGetScreenScale)
        .SetAlignToPixel(AutoAlignToPixel)
        .SetDstRect(LRect)
        .SetFill(LCurrentAdjustedStateStyle.Fill)
        .SetStateLayer(LCurrentAdjustedStateStyle.StateLayer, TAlphaColors.Null)
        .SetDrawStateLayerOnTop(False)
        .SetStroke(LCurrentAdjustedStateStyle.Stroke)
        .SetShadow(LCurrentAdjustedStateStyle.Shadow)
        .SetSides(AllSides)
        .SetCorners(AllCorners)
        .SetXRadius(XRadius)
        .SetYRadius(YRadius)
        .Draw;

    finally
      ALCanvasEndScene(RenderTargetCanvas)
    end;

    ALUpdateDrawableFromSurface(RenderTargetSurface, RenderTargetDrawable);

    // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
    // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
    LRect.Offset(-2*LRect.Left, -2*LRect.Top);

    // LRect must include the LScale
    LRect.Top := LRect.Top * LCurrentAdjustedStateStyle.Scale;
    LRect.right := LRect.right * LCurrentAdjustedStateStyle.Scale;
    LRect.left := LRect.left * LCurrentAdjustedStateStyle.Scale;
    LRect.bottom := LRect.bottom * LCurrentAdjustedStateStyle.Scale;

    // Since LStateStyle.FBufDrawableRect can have different dimensions than the main BufDrawableRect (LocalRect)
    // (due to scale), we must center LStateStyle.FBufDrawableRect within the main BufDrawableRect (LocalRect)
    // to ensure that all changes are visually centered.
    var LCenteredRect := LRect.CenterAt(LocalRect);
    LRect.Offset(LCenteredRect.Left, LCenteredRect.top);

    // We cannot use the matrix because, if we do, ALAlignToPixelRound in ALDrawDrawable
    // will be ineffective since the matrix will no longer be a simple translation matrix.
    // In such a case, TCustomCanvasGpu(ACanvas).DrawTexture may produce border artifacts
    // if the texture is not perfectly pixel-aligned.
    var LDstRect := TRectF.Create(0, 0, ALGetDrawableWidth(RenderTargetDrawable), ALGetDrawableHeight(RenderTargetDrawable));
    LDstRect.Width := (LDstRect.Width / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
    LDstRect.height := (LDstRect.height / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
    LDstRect.SetLocation(
      LRect.Left,
      LRect.Top);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      RenderTargetDrawable, // const ADrawable: TALDrawable;
      LDstRect, // const ADstRect: TrectF; // IN Virtual pixels !
      AbsoluteOpacity); // const AOpacity: Single)

    {$ENDIF}

    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{*************************************************************}
function TALSwitch.TThumb.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.null;
end;

{*************************************************************************}
function TALSwitch.TThumb.TCheckMarkBrush.TMargins.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(6,6,6,6);
end;

{*****************************************************************}
function TALSwitch.TThumb.TCheckMarkBrush.CreateMargins: TALBounds;
begin
  Result := TMargins.Create;
end;

{********************************************************************************}
function TALSwitch.TThumb.TInheritCheckMarkBrush.TMargins.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(6,6,6,6);
end;

{************************************************************************}
function TALSwitch.TThumb.TInheritCheckMarkBrush.CreateMargins: TALBounds;
begin
  Result := TMargins.Create;
end;

{********************************************************************************}
function TALSwitch.TThumb.TDefaultStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.null;
end;

{**************************************************************************************************************}
function TALSwitch.TThumb.TDefaultStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{***************************************************************************************************************************************************}
function TALSwitch.TThumb.TDefaultStateStyle.CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{*********************************************************************************}
function TALSwitch.TThumb.TDisabledStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.null;
end;

{***************************************************************************************************************}
function TALSwitch.TThumb.TDisabledStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{****************************************************************************************************************************************************}
function TALSwitch.TThumb.TDisabledStateStyle.CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{********************************************************************************}
function TALSwitch.TThumb.THoveredStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.null;
end;

{**************************************************************************************************************}
function TALSwitch.TThumb.THoveredStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{***************************************************************************************************************************************************}
function TALSwitch.TThumb.THoveredStateStyle.CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{********************************************************************************}
function TALSwitch.TThumb.TPressedStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.null;
end;

{**************************************************************************************************************}
function TALSwitch.TThumb.TPressedStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{***************************************************************************************************************************************************}
function TALSwitch.TThumb.TPressedStateStyle.CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{********************************************************************************}
function TALSwitch.TThumb.TFocusedStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.null;
end;

{**************************************************************************************************************}
function TALSwitch.TThumb.TFocusedStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{***************************************************************************************************************************************************}
function TALSwitch.TThumb.TFocusedStateStyle.CreateCheckMark(const AParent: TALBaseCheckBox.TCheckMarkBrush): TALBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{******************************************************************************************************************************}
function TALSwitch.TThumb.TCheckStateStyles.CreateDefaultStateStyle(const AParent: TObject): TALBaseCheckBox.TDefaultStateStyle;
begin
  Result := TDefaultStateStyle.Create(AParent);
end;

{********************************************************************************************************************************}
function TALSwitch.TThumb.TCheckStateStyles.CreateDisabledStateStyle(const AParent: TObject): TALBaseCheckBox.TDisabledStateStyle;
begin
  Result := TDisabledStateStyle.Create(AParent);
end;

{******************************************************************************************************************************}
function TALSwitch.TThumb.TCheckStateStyles.CreateHoveredStateStyle(const AParent: TObject): TALBaseCheckBox.THoveredStateStyle;
begin
  Result := THoveredStateStyle.Create(AParent);
end;

{******************************************************************************************************************************}
function TALSwitch.TThumb.TCheckStateStyles.CreatePressedStateStyle(const AParent: TObject): TALBaseCheckBox.TPressedStateStyle;
begin
  Result := TPressedStateStyle.Create(AParent);
end;

{******************************************************************************************************************************}
function TALSwitch.TThumb.TCheckStateStyles.CreateFocusedStateStyle(const AParent: TObject): TALBaseCheckBox.TFocusedStateStyle;
begin
  Result := TFocusedStateStyle.Create(AParent);
end;

{********************************************************}
procedure TALSwitch.TThumb.TStateStyles.TTransition.Start;
begin
  FStartPositionX := Owner{StateStyles}.Parent{Thumb}.Position.x;
  inherited;
  if not Running then
    TALSwitch(Owner{StateStyles}.Parent{Thumb}.ParentControl{Track}.ParentControl{Switch}).AlignThumb;
end;

{************************************************************}
procedure TALSwitch.TThumb.TStateStyles.TTransition.DoProcess;
begin
  if Enabled then begin
    var LThumb := Owner{StateStyles}.Parent{Thumb};
    var LSwitch := TALSwitch(LThumb.ParentControl{Track}.ParentControl{Switch});
    if (not LSwitch.Pressed) and (Lthumb.Align = TALAlignLayout.None) then begin
      var LStopPositionX: Single;
      If LSwitch.Checked then LStopPositionX := LSwitch.GetMaxThumbPos
      else LStopPositionX := LSwitch.GetMinThumbPos;
      LThumb.Position.x := FStartPositionX + (LStopPositionX - FStartPositionX) * CurrentValue;
    end;
  end;
  inherited;
end;

{***********************************************************}
procedure TALSwitch.TThumb.TStateStyles.TTransition.DoFinish;
begin
  if Enabled then begin
    TALSwitch(Owner{StateStyles}.Parent{Thumb}.ParentControl{Track}.ParentControl{Switch}).AlignThumb;
  end;
  inherited;
end;

{**************************************************************************************}
function TALSwitch.TThumb.TStateStyles.CreateTransition: TALBaseStateStyles.TTransition;
begin
  result := TTransition.Create(Self);
end;

{****************************************************************************************************************************}
function TALSwitch.TThumb.TStateStyles.CreateCheckedStateStyles(const AParent: TALControl): TALBaseCheckBox.TCheckStateStyles;
begin
  Result := TCheckStateStyles.Create(AParent);
end;

{******************************************************************************************************************************}
function TALSwitch.TThumb.TStateStyles.CreateUncheckedStateStyles(const AParent: TALControl): TALBaseCheckBox.TCheckStateStyles;
begin
  Result := TCheckStateStyles.Create(AParent);
end;

{******************************************************}
constructor TALSwitch.TThumb.Create(AOwner: TComponent);
begin
  inherited;
  //--
  SetAcceptsControls(False);
  CanFocus := False;
  Locked := True;
  HitTest := False;
  //--
  Margins.DefaultValue := TRectF.Create(4,4,4,4);
  Margins.Rect := Margins.DefaultValue;
end;

{*****************************************************}
function TALSwitch.TThumb.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
end;

{*************************************************************************}
function TALSwitch.TThumb.CreateCheckMark: TALBaseCheckBox.TCheckMarkBrush;
begin
  Result := TCheckMarkBrush.Create;
end;

{************************************************************************}
function TALSwitch.TThumb.CreateStateStyles: TALBaseCheckBox.TStateStyles;
begin
  result := TStateStyles.Create(Self);
end;

{**************************************************}
function TALSwitch.TThumb.GetDefaultXRadius: Single;
begin
  Result := -50;
end;

{**************************************************}
function TALSwitch.TThumb.GetDefaultYRadius: Single;
begin
  Result := -50;
end;

{***********************************************}
function TALSwitch.TThumb.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(24, 24);
end;

{*******************************}
procedure TALSwitch.TThumb.Click;
begin
  // Since TALSwitch.TThumb has HitTest set to false, this event
  // is triggered only at the end of the transition animation when
  // DelayClick is set to true.
  TALSwitch(ParentControl{Track}.ParentControl{Switch}).click;
end;

{***************************************************************************************}
constructor TALSwitch.TTransition.TInterpolationParams.Create(Const AOwner: TTransition);
begin
  inherited create;
  FOwner := AOwner;
end;

{**********************************************************************}
function TALSwitch.TTransition.TInterpolationParams.GetBezierX1: Single;
begin
  Result := FOwner{TTransition}.FOwner{TALSwitch}.Thumb.StateStyles.Transition.InterpolationParams.BezierX1;
end;

{**********************************************************************}
function TALSwitch.TTransition.TInterpolationParams.GetBezierY1: Single;
begin
  Result := FOwner{TTransition}.FOwner{TALSwitch}.Thumb.StateStyles.Transition.InterpolationParams.BezierY1;
end;

{**********************************************************************}
function TALSwitch.TTransition.TInterpolationParams.GetBezierX2: Single;
begin
  Result := FOwner{TTransition}.FOwner{TALSwitch}.Thumb.StateStyles.Transition.InterpolationParams.BezierX2;
end;

{**********************************************************************}
function TALSwitch.TTransition.TInterpolationParams.GetBezierY2: Single;
begin
  Result := FOwner{TTransition}.FOwner{TALSwitch}.Thumb.StateStyles.Transition.InterpolationParams.BezierY2;
end;

{*************************************************************************************}
procedure TALSwitch.TTransition.TInterpolationParams.SetBezierX1(const AValue: Single);
begin
  FOwner{TTransition}.FOwner{TALSwitch}.Thumb.StateStyles.Transition.InterpolationParams.BezierX1 := AValue;
  FOwner{TTransition}.FOwner{TALSwitch}.Track.StateStyles.Transition.InterpolationParams.BezierX1 := AValue;
end;

{*************************************************************************************}
procedure TALSwitch.TTransition.TInterpolationParams.SetBezierY1(const AValue: Single);
begin
  FOwner{TTransition}.FOwner{TALSwitch}.Thumb.StateStyles.Transition.InterpolationParams.BezierY1 := AValue;
  FOwner{TTransition}.FOwner{TALSwitch}.Track.StateStyles.Transition.InterpolationParams.BezierY1 := AValue;
end;

{*************************************************************************************}
procedure TALSwitch.TTransition.TInterpolationParams.SetBezierX2(const AValue: Single);
begin
  FOwner{TTransition}.FOwner{TALSwitch}.Thumb.StateStyles.Transition.InterpolationParams.BezierX2 := AValue;
  FOwner{TTransition}.FOwner{TALSwitch}.Track.StateStyles.Transition.InterpolationParams.BezierX2 := AValue;
end;

{*************************************************************************************}
procedure TALSwitch.TTransition.TInterpolationParams.SetBezierY2(const AValue: Single);
begin
  FOwner{TTransition}.FOwner{TALSwitch}.Thumb.StateStyles.Transition.InterpolationParams.BezierY2 := AValue;
  FOwner{TTransition}.FOwner{TALSwitch}.Track.StateStyles.Transition.InterpolationParams.BezierY2 := AValue;
end;

{***********************************************************************}
function TALSwitch.TTransition.TInterpolationParams.GetOvershoot: Single;
begin
  Result := FOwner{TTransition}.FOwner{TALSwitch}.Thumb.StateStyles.Transition.InterpolationParams.Overshoot;
end;

{**************************************************************************************}
procedure TALSwitch.TTransition.TInterpolationParams.SetOvershoot(const AValue: Single);
begin
  FOwner{TTransition}.FOwner{TALSwitch}.Thumb.StateStyles.Transition.InterpolationParams.Overshoot := AValue;
  FOwner{TTransition}.FOwner{TALSwitch}.Track.StateStyles.Transition.InterpolationParams.Overshoot := AValue;
end;

{****************************************************************}
constructor TALSwitch.TTransition.Create(Const AOwner: TALSwitch);
begin
  inherited create;
  FOwner := AOwner;
  FInterpolationParams := TInterpolationParams.Create(self);
end;

{***************************************}
destructor TALSwitch.TTransition.Destroy;
begin
  ALFreeAndNil(FInterpolationParams);
  inherited;
end;

{************************************}
procedure TALSwitch.TTransition.Start;
begin
  FOwner{TALSwitch}.Thumb.StateStyles.Transition.Start;
  FOwner{TALSwitch}.Track.StateStyles.Transition.Start;
end;

{*************************************************}
function TALSwitch.TTransition.GetDuration: Single;
begin
  Result := FOwner{TALSwitch}.Thumb.StateStyles.Transition.Duration;
end;

{****************************************************************}
procedure TALSwitch.TTransition.SetDuration(const AValue: Single);
begin
  FOwner{TALSwitch}.Thumb.StateStyles.Transition.Duration := AValue;
  FOwner{TALSwitch}.Track.StateStyles.Transition.Duration := AValue;
end;

{****************************************************}
function TALSwitch.TTransition.GetDelayClick: Boolean;
begin
  Result := FOwner{TALSwitch}.Thumb.StateStyles.Transition.DelayClick;
end;

{*******************************************************************}
procedure TALSwitch.TTransition.SetDelayClick(const AValue: Boolean);
begin
  FOwner{TALSwitch}.Thumb.StateStyles.Transition.DelayClick := AValue;
  FOwner{TALSwitch}.Track.StateStyles.Transition.DelayClick := AValue;
end;

{************************************************************************}
function TALSwitch.TTransition.GetInterpolationType: TALInterpolationType;
begin
  Result := FOwner{TALSwitch}.Thumb.StateStyles.Transition.InterpolationType;
end;

{***************************************************************************************}
procedure TALSwitch.TTransition.SetInterpolationType(const AValue: TALInterpolationType);
begin
  FOwner{TALSwitch}.Thumb.StateStyles.Transition.InterpolationType := AValue;
  FOwner{TALSwitch}.Track.StateStyles.Transition.InterpolationType := AValue;
end;

{************************************************************************}
function TALSwitch.TTransition.GetInterpolationMode: TALInterpolationMode;
begin
  Result := FOwner{TALSwitch}.Thumb.StateStyles.Transition.InterpolationMode;
end;

{***************************************************************************************}
procedure TALSwitch.TTransition.SetInterpolationMode(const AValue: TALInterpolationMode);
begin
  FOwner{TALSwitch}.Thumb.StateStyles.Transition.InterpolationMode := AValue;
  FOwner{TALSwitch}.Track.StateStyles.Transition.InterpolationMode := AValue;
end;

{*****************************************************************************************}
procedure TALSwitch.TTransition.SetInterpolationParams(const AValue: TInterpolationParams);
begin
  // No action required; Transition acts only as a proxy
end;

{*******************************************************}
Function TALSwitch.TTransition.IsDurationStored: Boolean;
begin
  result := not SameValue(Duration, FOwner{TALSwitch}.Thumb.StateStyles.Transition.DefaultDuration, TALAnimation.TimeEpsilon);
end;

{*********************************************************}
Function TALSwitch.TTransition.IsDelayClickStored: Boolean;
begin
  result := DelayClick <> FOwner{TALSwitch}.Thumb.StateStyles.Transition.DefaultDelayClick;
end;

{****************************************************************}
Function TALSwitch.TTransition.IsInterpolationTypeStored: Boolean;
begin
  result := InterpolationType <> FOwner{TALSwitch}.Thumb.StateStyles.Transition.DefaultInterpolationType;
end;

{****************************************************************}
Function TALSwitch.TTransition.IsInterpolationModeStored: Boolean;
begin
  result := InterpolationMode <> FOwner{TALSwitch}.Thumb.StateStyles.Transition.DefaultInterpolationMode;
end;

{***********************************************}
constructor TALSwitch.Create(AOwner: TComponent);
begin
  inherited;
  CanFocus := True;
  SetAcceptsControls(False);
  AutoCapture := True;
  Cursor := crHandPoint;
  DisabledOpacity := 1;
  //--
  FOnChange := nil;
  FPressedThumbPos := TpointF.create(0,0);
  //--
  fScrollCapturedByMe := False;
  TMessageManager.DefaultManager.SubscribeToMessage(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
  //--
  FTrack := CreateTrack;
  FTrack.Parent := self;
  FTrack.Stored := False;
  FTrack.SetSubComponent(True);
  FTrack.Name := 'Track'; // Useful at design time in the IDE
  FTrack.Align := TALAlignLayout.Client;
  //--
  // Use 'self' instead of 'FTrack' to ensure that
  // 'Fthumb.loaded' is called.
  FThumb := CreateThumb;
  FThumb.Parent := FTrack;
  FThumb.Stored := False;
  FThumb.SetSubComponent(True);
  FThumb.Name := 'Thumb'; // Useful at design time in the IDE
  //--
  FTransition := TTransition.Create(Self);
end;

{***************************}
destructor TALSwitch.Destroy;
begin
  ALFreeAndNil(FTransition);
  inherited;
end;

{************************************}
procedure TALSwitch.BeforeDestruction;
begin
  if BeforeDestructionExecuted then exit;
  // Unsubscribe from TALScrollCapturedMessage to stop receiving messages.
  // This must be done in BeforeDestruction rather than in Destroy,
  // because the control might be freed in the background via ALFreeAndNil(..., delayed),
  // and BeforeDestruction is guaranteed to execute on the main thread.
  TMessageManager.DefaultManager.Unsubscribe(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
  inherited;
end;

{************************************}
procedure TALSwitch.AfterConstruction;
begin
  inherited;
  if not IsOwnerLoading then
    AlignThumb;
end;

{*************************************}
function TALSwitch.CreateTrack: TTrack;
begin
  Result := TTrack.Create(self);
end;

{*************************************}
function TALSwitch.CreateThumb: TThumb;
begin
  Result := TThumb.Create(self);
end;

{*************************}
procedure TALSwitch.Loaded;
begin
  inherited;
  AlignThumb;
end;

{*******************************}
procedure TALSwitch.AlignToPixel;
begin
  BeginUpdate;
  try
    inherited;
    Thumb.AlignToPixel;
    Track.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{***********************************}
procedure TALSwitch.ApplyColorScheme;
begin
  //BeginUpdate;
  //try
    inherited;
    //Thumb.ApplyColorScheme;
    //Track.ApplyColorScheme;
  //finally
    //EndUpdate;
  //end;
end;

{**********************************}
procedure TALSwitch.MakeBufDrawable;
begin
  Track.MakeBufDrawable;
  Thumb.MakeBufDrawable;
end;

{***********************************}
procedure TALSwitch.ClearBufDrawable;
begin
  Track.ClearBufDrawable;
  Thumb.ClearBufDrawable;
end;

{****************************************}
function TALSwitch.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(52, 32);
end;

{********************************************}
function TALSwitch.GetDoubleBuffered: boolean;
begin
  result := Track.DoubleBuffered and Thumb.DoubleBuffered;
end;

{***********************************************************}
procedure TALSwitch.SetDoubleBuffered(const AValue: Boolean);
begin
  Track.DoubleBuffered := AValue;
  Thumb.DoubleBuffered := AValue;
end;

{*************************************}
procedure TALSwitch.IsMouseOverChanged;
begin
  inherited;
  Track.FIsMouseOver := IsMouseOver;
  Thumb.FIsMouseOver := IsMouseOver;
  Track.IsMouseOverChanged;
  Thumb.IsMouseOverChanged;
end;

{***********************************}
procedure TALSwitch.IsFocusedChanged;
begin
  inherited;
  Track.FIsFocused := IsFocused;
  Thumb.FIsFocused := IsFocused;
  Track.IsFocusedChanged;
  Thumb.IsFocusedChanged;
end;

{*********************************}
procedure TALSwitch.PressedChanged;
begin
  inherited;
  Track.Pressed := Pressed;
  Thumb.Pressed := Pressed;
end;

{*********************************}
procedure TALSwitch.EnabledChanged;
begin
  inherited;
  Track.enabled := enabled;
  Thumb.enabled := enabled;
end;

{***************************}
procedure TALSwitch.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{**********************************************************}
procedure TALSwitch.SetTransition(const Value: TTransition);
begin
  // No action required; Transition acts only as a proxy
end;

{************************************************************************************}
procedure TALSwitch.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Pressed then begin
    fThumb.Align := TALALignLayout.None;
    FPressedThumbPos := FThumb.Position.Point;
  end;
end;

{**************************************************************}
procedure TALSwitch.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  'TALSwitch.MouseMove',
  //  'Position:' + ALFormatFloatW('0.##', x) + ',' + ALFormatFloatW('0.##', y));
  {$ENDIF}
  if Pressed then begin

    if (not fScrollCapturedByMe) then begin
      If (abs(X - PressedPosition.X) > abs(Y - PressedPosition.Y)) and
         (abs(X - PressedPosition.X) > TALScrollEngine.DefaultTouchSlop) then begin
        {$IFDEF DEBUG}
        //ALLog(
        //  'TALSwitch.MouseMove',
        //  'ScrollCapturedByMe');
        {$ENDIF}
        PressedPosition := TpointF.Create(X,Y);
        fScrollCapturedByMe := true;
        TMessageManager.DefaultManager.SendMessage(self, TALScrollCapturedMessage.Create(true), True);
      end;
    end;

    if fScrollCapturedByMe then begin
      var LNewThumbPosX := FPressedThumbPos.x + (X - PressedPosition.X);
      LNewThumbPosX := min(LNewThumbPosX, GetMaxThumbPos);
      LNewThumbPosX := max(LNewThumbPosX, GetMinThumbPos);
      FThumb.Position.x :=LNewThumbPosX;
    end;

  end;
  inherited;
end;

{**********************************************************************************}
procedure TALSwitch.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if fScrollCapturedByMe then begin
    FScrollCapturedByMe := False;
    var LChecked: Boolean;
    if fThumb.Position.x + (fThumb.Width / 2) < Track.Width / 2 then LChecked := False
    else LChecked := True;
    if LChecked <> Checked then begin
      if (transition.DelayClick) and
         (compareValue(FTransition.Duration,0.0,TEpsilon.Scale) > 0) then
        Thumb.StateStyles.Transition.ClickDelayed := True;
      FTrack.Checked := LChecked;
      FThumb.Checked := LChecked;
      if not Thumb.StateStyles.Transition.ClickDelayed then
        DoChange;
    end;
    fThumb.Align := TALALignLayout.None;
    Transition.Start;
  end;
end;

{*******************************}
procedure TALSwitch.DoMouseLeave;
begin
  inherited;
  if fScrollCapturedByMe then begin
    FScrollCapturedByMe := False;
    var LChecked: Boolean;
    if fThumb.Position.x + (fThumb.Width / 2) < Track.Width / 2 then LChecked := False
    else LChecked := True;
    if LChecked <> Checked then begin
      if (transition.DelayClick) and
         (compareValue(FTransition.Duration,0.0,TEpsilon.Scale) > 0) then
        Thumb.StateStyles.Transition.ClickDelayed := True;
      FTrack.Checked := LChecked;
      FThumb.Checked := LChecked;
      if not Thumb.StateStyles.Transition.ClickDelayed then
        DoChange;
    end;
    fThumb.Align := TALALignLayout.None;
    Transition.Start;
  end;
end;

{*******************************}
procedure TALSwitch.DoClickSound;
begin
  if (ClickSound=TALClickSoundMode.Always) or
     ((ClickSound=TALClickSoundMode.Default) and ALGlobalClickSoundEnabled) then
    ALPlayClickSound;
end;

{************************}
procedure TALSwitch.Click;
begin
  // If fScrollCapturedByMe is true, the MouseUp event will handle the task.
  if fScrollCapturedByMe then Exit
  // If Pressed is true, it means this event is triggered by MouseDown/MouseUp.
  // In this case, if a delay is requested for the click, apply the delay.
  else if (Pressed) and
          (Transition.DelayClick) and
          (compareValue(FTransition.Duration,0.0,TEpsilon.Scale) > 0) then begin
    Thumb.StateStyles.Transition.ClickDelayed := True;
    var LChecked := not Checked;
    FTrack.Checked := LChecked;
    FThumb.Checked := LChecked;
    fThumb.Align := TALALignLayout.None;
    Transition.Start;
    exit;
  end
  // If Pressed is true, it means this event is triggered by MouseDown/MouseUp.
  else if Pressed then begin
    var LChecked := not Checked;
    FTrack.Checked := LChecked;
    FThumb.Checked := LChecked;
    fThumb.Align := TALALignLayout.None;
    DoChange;
    inherited;
    Transition.Start;
  end
  // if not Pressed, it means this event is triggered by event like TransitionAnimationFinish
  else begin
    DoChange;
    inherited;
    AlignThumb;
  end;
end;

{*****************************************************************************************}
procedure TALSwitch.ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
begin
  if (Sender = self) then exit;
  {$IFDEF DEBUG}
  //ALLog(
  //  'TALSwitch.ScrollCapturedByOtherHandler',
  //  'Captured: ' + ALBoolToStrW(TALScrollCapturedMessage(M).Captured)+ ' | ' +
  //  'Pressed: ' + ALBoolToStrW(Pressed));
  {$ENDIF}
  if TALScrollCapturedMessage(M).Captured then begin
    {$IFDEF DEBUG}
    if fScrollCapturedByMe then
      raise Exception.Create('Error 6C41BEC8-3AE9-4EC0-9D80-117ED5697397');
    {$ENDIF}
    Pressed := False;
  end;
end;

{****************************************}
function TALSwitch.GetCacheIndex: integer;
begin
  Result := FThumb.CacheIndex;
end;

{*******************************************************}
procedure TALSwitch.SetCacheIndex(const AValue: Integer);
begin
  FThumb.CacheIndex := AValue;
  FTrack.CacheIndex := AValue;
end;

{***********************************************************}
function TALSwitch.GetCacheEngine: TALBufDrawableCacheEngine;
begin
  Result := FThumb.CacheEngine;
end;

{**************************************************************************}
procedure TALSwitch.SetCacheEngine(const AValue: TALBufDrawableCacheEngine);
begin
  FThumb.CacheEngine := AValue;
  FTrack.CacheEngine := AValue;
end;

{****************************************}
function TALSwitch.GetMinThumbPos: Single;
begin
  result := Track.Padding.left + fThumb.Margins.left;
end;

{****************************************}
function TALSwitch.GetMaxThumbPos: Single;
begin
  result := Track.Width - fThumb.Width - Track.Padding.Right - fThumb.Margins.Right;
end;

{*****************************}
procedure TALSwitch.AlignThumb;
begin
  If CsLoading in ComponentState then Exit;
  if pressed or fScrollCapturedByMe then exit;
  If Checked then FThumb.Align := TALAlignLayout.right
  else FThumb.Align := TALAlignLayout.left;
end;

{*************************************}
function TALSwitch.GetChecked: boolean;
begin
  Result := FTrack.Checked and FThumb.Checked;
end;

{***************************************************}
procedure TALSwitch.SetChecked(const Value: Boolean);
begin
  if GetChecked <> Value then begin
    FTrack.Checked := Value;
    FThumb.Checked := Value;
    AlignThumb;
    DoChange;
  end;
end;

{****************************************************}
function TALButton.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $ffe1e1e1;
end;

{******************************************************}
function TALButton.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := $ffadadad;
end;

{*******************************************************************}
function TALButton.TTextSettings.TFont.GetDefaultWeight: TFontWeight;
begin
  Result := TFontWeight.medium;
end;

{***************************************************}
function TALButton.TTextSettings.CreateFont: TALFont;
begin
  Result := TFont.Create;
end;

{*********************************************************************}
function TALButton.TTextSettings.GetDefaultHorzAlign: TALTextHorzAlign;
begin
  Result := TALTextHorzAlign.center;
end;

{********************************************************************}
function TALButton.TBaseStateStyle.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $FFE1E1E1;
end;

{**********************************************************************}
function TALButton.TBaseStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := $FFADADAD;
end;

{***********************************************************************************}
function TALButton.TBaseStateStyle.TTextSettings.TFont.GetDefaultWeight: TFontWeight;
begin
  Result := TFontWeight.medium;
end;

{*******************************************************************}
function TALButton.TBaseStateStyle.TTextSettings.CreateFont: TALFont;
begin
  Result := TFont.Create;
end;

{*******************************************************************}
constructor TALButton.TBaseStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  FText := DefaultText;
  if StateStyleParent <> nil then FTextSettings := CreateTextSettings(StateStyleParent.TextSettings)
  else if ControlParent <> nil then FTextSettings := CreateTextSettings(ControlParent.TextSettings)
  else FTextSettings := CreateTextSettings(nil);
  FTextSettings.OnChanged := TextSettingsChanged;
  FXRadius := DefaultXRadius;
  FYRadius := DefaultYRadius;
end;

{*******************************************}
destructor TALButton.TBaseStateStyle.Destroy;
begin
  ALFreeAndNil(FTextSettings);
  inherited Destroy;
end;

{**************************************************************************************}
function TALButton.TBaseStateStyle.CreateFill(const AParent: TALBrush): TALInheritBrush;
begin
  Result := TFill.Create(AParent);
end;

{****************************************************************************************************}
function TALButton.TBaseStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{***********************************************************************************************************************}
function TALButton.TBaseStateStyle.CreateTextSettings(const AParent: TALBaseTextSettings): TBaseStateStyle.TTextSettings;
begin
  Result := TTextSettings.Create(AParent);
end;

{**************************************************************}
procedure TALButton.TBaseStateStyle.Assign(Source: TPersistent);
begin
  if Source is TBaseStateStyle then begin
    BeginUpdate;
    Try
      Text := TBaseStateStyle(Source).text;
      TextSettings.Assign(TBaseStateStyle(Source).TextSettings);
      XRadius := TBaseStateStyle(Source).XRadius;
      YRadius := TBaseStateStyle(Source).YRadius;
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{****************************************}
procedure TALButton.TBaseStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Text := DefaultText;
    TextSettings.reset;
    XRadius := DefaultXRadius;
    YRadius := DefaultYRadius;
  finally
    EndUpdate;
  end;
end;

{***********************************************}
procedure TALButton.TBaseStateStyle.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    TextSettings.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{***************************************************}
procedure TALButton.TBaseStateStyle.ApplyColorScheme;
begin
  BeginUpdate;
  Try
    inherited;
    TextSettings.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{************************************************************************************************************************************}
procedure TALButton.TBaseStateStyle.Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single; const AReverse: Boolean);
begin
  {$IF defined(debug)}
  if (ATo <> nil) and (not (ATo is TBaseStateStyle)) then
    Raise Exception.Create('Error F3C72244-894F-4B67-AD86-F24DF5039927');
  {$ENDIF}
  BeginUpdate;
  try
    Inherited Interpolate(ATo, ANormalizedTime, AReverse);
    if ATo <> nil then begin
      if not AReverse then Text := TBaseStateStyle(ATo).Text;
      TextSettings.Interpolate(TBaseStateStyle(ATo).TextSettings, ANormalizedTime, AReverse);
      XRadius := InterpolateSingle(XRadius{Start}, TBaseStateStyle(ATo).XRadius{Stop}, ANormalizedTime);
      YRadius := InterpolateSingle(YRadius{Start}, TBaseStateStyle(ATo).YRadius{Stop}, ANormalizedTime);
    end
    else if StateStyleParent <> nil then begin
      StateStyleParent.SupersedeNoChanges(true{ASaveState});
      try
        if not AReverse then Text := StateStyleParent.Text;
        TextSettings.Interpolate(StateStyleParent.TextSettings, ANormalizedTime, AReverse);
        XRadius := InterpolateSingle(XRadius{Start}, StateStyleParent.XRadius{Stop}, ANormalizedTime);
        YRadius := InterpolateSingle(YRadius{Start}, StateStyleParent.YRadius{Stop}, ANormalizedTime);
      finally
        StateStyleParent.RestoreStateNoChanges;
      end;
    end
    else if ControlParent <> nil then begin
      if not AReverse then Text := ControlParent.Text;
      TextSettings.Interpolate(ControlParent.TextSettings, ANormalizedTime, AReverse);
      XRadius := InterpolateSingle(XRadius{Start}, ControlParent.XRadius{Stop}, ANormalizedTime);
      YRadius := InterpolateSingle(YRadius{Start}, ControlParent.YRadius{Stop}, ANormalizedTime);
    end
    else begin
      if not AReverse then Text := DefaultText;
      TextSettings.Interpolate(nil, ANormalizedTime, AReverse);
      XRadius := InterpolateSingle(XRadius{Start}, ALIfThen(IsNaN(DefaultXRadius), 0, DefaultXRadius){Stop}, ANormalizedTime);
      YRadius := InterpolateSingle(YRadius{Start}, ALIfThen(IsNaN(DefaultYRadius), 0, DefaultYRadius){Stop}, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{**********************************************}
procedure TALButton.TBaseStateStyle.DoSupersede;
begin
  Inherited;
  if Text = '' then begin
    if StateStyleParent <> nil then Text := StateStyleParent.Text
    else Text := ControlParent.Text;
  end;
  if IsNaN(XRadius) then begin
    if StateStyleParent <> nil then XRadius := StateStyleParent.XRadius
    else XRadius := ControlParent.XRadius;
  end;
  if IsNaN(YRadius) then begin
    if StateStyleParent <> nil then YRadius := StateStyleParent.YRadius
    else YRadius := ControlParent.YRadius;
  end;
  TextSettings.SuperSede;
end;

{**********************************************************************}
function TALButton.TBaseStateStyle.GetStateStyleParent: TBaseStateStyle;
begin
  {$IF defined(debug)}
  if (inherited StateStyleParent <> nil) and
     (not (inherited StateStyleParent is TBaseStateStyle)) then
    raise Exception.Create('StateStyleParent must be of type TBaseStateStyle');
  {$ENDIF}
  Result := TBaseStateStyle(inherited StateStyleParent);
end;

{*************************************************************}
function TALButton.TBaseStateStyle.GetControlParent: TALButton;
begin
  {$IF defined(debug)}
  if (inherited ControlParent <> nil) and
     (not (inherited ControlParent is TALButton)) then
    raise Exception.Create('ControlParent must be of type TALButton');
  {$ENDIF}
  Result := TALButton(inherited ControlParent);
end;

{***************************************************************}
procedure TALButton.TBaseStateStyle.SetText(const Value: string);
begin
  if FText <> Value then begin
    FText := Value;
    Change;
  end;
end;

{***********************************************************************************************}
procedure TALButton.TBaseStateStyle.SetTextSettings(const AValue: TBaseStateStyle.TTextSettings);
begin
  FTextSettings.Assign(AValue);
end;

{******************************************************************}
procedure TALButton.TBaseStateStyle.SetXRadius(const Value: Single);
begin
  if IsNan(FXRadius) and IsNan(Value) then Exit;
  if not SameValue(FXRadius, Value, TEpsilon.Vector) then begin
    FXRadius := Value;
    Change;
  end;
end;

{******************************************************************}
procedure TALButton.TBaseStateStyle.SetYRadius(const Value: Single);
begin
  if IsNan(FYRadius) and IsNan(Value) then Exit;
  if not SameValue(FYRadius, Value, TEpsilon.Vector) then begin
    FYRadius := Value;
    Change;
  end;
end;

{********************************************************}
function TALButton.TBaseStateStyle.GetDefaultText: String;
begin
  Result := '';
end;

{***********************************************************}
function TALButton.TBaseStateStyle.GetDefaultXRadius: Single;
begin
  Result := NaN;
end;

{***********************************************************}
function TALButton.TBaseStateStyle.GetDefaultYRadius: Single;
begin
  Result := NaN;
end;

{*****************************************************}
function TALButton.TBaseStateStyle.GetInherit: Boolean;
begin
  Result := inherited GetInherit and
            IsNan(FXRadius) and
            IsNan(FYRadius) and
            Text.IsEmpty and
            TextSettings.Inherit;
end;

{************************************************************************}
procedure TALButton.TBaseStateStyle.TextSettingsChanged(ASender: TObject);
begin
  Change;
end;

{*******************************************************}
function TALButton.TBaseStateStyle.IsTextStored: Boolean;
begin
  Result := FText <> DefaultText;
end;

{**********************************************************}
function TALButton.TBaseStateStyle.IsXRadiusStored: Boolean;
begin
  if IsNan(FXRadius) and IsNan(DefaultXRadius) then Exit(False);
  Result := not SameValue(FXRadius, DefaultXRadius, TEpsilon.Vector);
end;

{**********************************************************}
function TALButton.TBaseStateStyle.IsYRadiusStored: Boolean;
begin
  if IsNan(FYRadius) and IsNan(DefaultYRadius) then Exit(False);
  Result := not SameValue(FYRadius, DefaultYRadius, TEpsilon.Vector);
end;

{**************************************************************}
function TALButton.TDisabledStateStyle.IsOpacityStored: Boolean;
begin
  Result := not SameValue(FOpacity, TControl.DefaultDisabledOpacity, TEpsilon.Scale);
end;

{**********************************************************************}
procedure TALButton.TDisabledStateStyle.SetOpacity(const Value: Single);
begin
  if not SameValue(FOpacity, Value, TEpsilon.Scale) then begin
    FOpacity := Value;
    Change;
  end;
end;

{***********************************************************************}
constructor TALButton.TDisabledStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  FOpacity := TControl.DefaultDisabledOpacity;
end;

{******************************************************************}
procedure TALButton.TDisabledStateStyle.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TDisabledStateStyle then
      Opacity := TDisabledStateStyle(Source).Opacity
    else
      Opacity := TControl.DefaultDisabledOpacity;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{********************************************}
procedure TALButton.TDisabledStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Opacity := TControl.DefaultDisabledOpacity;
  finally
    EndUpdate;
  end;
end;

{*********************************************************}
function TALButton.TDisabledStateStyle.GetInherit: Boolean;
begin
  // Opacity is not part of the GetInherit function because it updates the
  // disabledOpacity of the base control immediately every time it changes.
  // Essentially, it acts merely as a link to the disabledOpacity of the base control.
  Result := inherited GetInherit;
end;

{***************************************************************}
function TALButton.TDisabledStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 1;
end;

{**************************************************************}
function TALButton.THoveredStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 2;
end;

{**************************************************************}
function TALButton.TPressedStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 3;
end;

{**************************************************************}
function TALButton.TFocusedStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 4;
end;

{*******************************************************************}
constructor TALButton.TStateStyles.Create(const AParent: TALControl);
begin
  inherited Create(AParent);
  //--
  FDisabled := CreateDisabledStateStyle(AParent);
  FDisabled.OnChanged := DisabledChanged;
  //--
  FHovered := CreateHoveredStateStyle(AParent);
  FHovered.OnChanged := HoveredChanged;
  //--
  FPressed := CreatePressedStateStyle(AParent);
  FPressed.OnChanged := PressedChanged;
  //--
  FFocused := CreateFocusedStateStyle(AParent);
  FFocused.OnChanged := FocusedChanged;
end;

{****************************************}
destructor TALButton.TStateStyles.Destroy;
begin
  ALFreeAndNil(FDisabled);
  ALFreeAndNil(FHovered);
  ALFreeAndNil(FPressed);
  ALFreeAndNil(FFocused);
  inherited Destroy;
end;

{****************************************************************************************************}
function TALButton.TStateStyles.CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle;
begin
  Result := TDisabledStateStyle.Create(AParent);
end;

{**************************************************************************************************}
function TALButton.TStateStyles.CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle;
begin
  Result := THoveredStateStyle.Create(AParent);
end;

{**************************************************************************************************}
function TALButton.TStateStyles.CreatePressedStateStyle(const AParent: TObject): TPressedStateStyle;
begin
  Result := TPressedStateStyle.Create(AParent);
end;

{**************************************************************************************************}
function TALButton.TStateStyles.CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle;
begin
  Result := TFocusedStateStyle.Create(AParent);
end;

{***********************************************************}
procedure TALButton.TStateStyles.Assign(Source: TPersistent);
begin
  if Source is TStateStyles then begin
    BeginUpdate;
    Try
      Disabled.Assign(TStateStyles(Source).Disabled);
      Hovered.Assign(TStateStyles(Source).Hovered);
      Pressed.Assign(TStateStyles(Source).Pressed);
      Focused.Assign(TStateStyles(Source).Focused);
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{*************************************}
procedure TALButton.TStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Disabled.reset;
    Hovered.reset;
    Pressed.reset;
    Focused.reset;
  finally
    EndUpdate;
  end;
end;

{********************************************}
procedure TALButton.TStateStyles.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    Disabled.AlignToPixel;
    Hovered.AlignToPixel;
    Pressed.AlignToPixel;
    Focused.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{************************************************}
procedure TALButton.TStateStyles.ApplyColorScheme;
begin
  BeginUpdate;
  Try
    inherited;
    Disabled.ApplyColorScheme;
    Hovered.ApplyColorScheme;
    Pressed.ApplyColorScheme;
    Focused.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{************************************************}
procedure TALButton.TStateStyles.ClearBufDrawable;
begin
  inherited;
  Disabled.ClearBufDrawable;
  Hovered.ClearBufDrawable;
  Pressed.ClearBufDrawable;
  Focused.ClearBufDrawable;
end;

{********************************************************************}
function TALButton.TStateStyles.GetCurrentRawStyle: TALBaseStateStyle;
begin
  if Not Parent.Enabled then Result := Disabled
  else if Parent.Pressed then Result := Pressed
  else if Parent.IsFocused then Result := Focused
  else if Parent.IsMouseOver then Result := Hovered
  else result := nil;
end;

{***************************************************}
function TALButton.TStateStyles.GetParent: TALButton;
begin
  Result := TALButton(inherited Parent);
end;

{******************************************************************************}
procedure TALButton.TStateStyles.SetDisabled(const AValue: TDisabledStateStyle);
begin
  FDisabled.Assign(AValue);
end;

{****************************************************************************}
procedure TALButton.TStateStyles.SetHovered(const AValue: THoveredStateStyle);
begin
  FHovered.Assign(AValue);
end;

{****************************************************************************}
procedure TALButton.TStateStyles.SetPressed(const AValue: TPressedStateStyle);
begin
  FPressed.Assign(AValue);
end;

{****************************************************************************}
procedure TALButton.TStateStyles.SetFocused(const AValue: TFocusedStateStyle);
begin
  FFocused.Assign(AValue);
end;

{*****************************************************************}
procedure TALButton.TStateStyles.DisabledChanged(ASender: TObject);
begin
  Change;
end;

{****************************************************************}
procedure TALButton.TStateStyles.HoveredChanged(ASender: TObject);
begin
  Change;
end;

{****************************************************************}
procedure TALButton.TStateStyles.PressedChanged(ASender: TObject);
begin
  Change;
end;

{****************************************************************}
procedure TALButton.TStateStyles.FocusedChanged(ASender: TObject);
begin
  Change;
end;

{***********************************************}
constructor TALButton.Create(AOwner: TComponent);
begin
  {$IF defined(ALDPK)}
  FPrevStateStyles := nil;
  {$ENDIF}
  FStateStyles := nil;
  //--
  inherited Create(AOwner);
  //--
  CanFocus := True;
  HitTest := True;
  AutoSize := TALAutoSizeMode.Both;
  Cursor := crHandPoint;
  //--
  var LPaddingChange: TNotifyEvent := Padding.OnChange;
  Padding.OnChange := nil;
  Padding.DefaultValue := TRectF.create(12{Left}, 6{Top}, 12{Right}, 6{Bottom});
  Padding.Rect := Padding.DefaultValue;
  padding.OnChange := LPaddingChange;
  //--
  {$IF defined(ALDPK)}
  FPrevStateStyles := TStateStyles.Create(nil);
  {$ENDIF}
  //--
  FStateStyles := CreateStateStyles;
  FStateStyles.OnChanged := StateStylesChanged;
end;

{***************************}
destructor TALButton.Destroy;
begin
  {$IF defined(ALDPK)}
  ALFreeAndNil(FPrevStateStyles);
  {$ENDIF}
  ALFreeAndNil(FStateStyles);
  inherited Destroy;
end;

{**********************************************************}
procedure TALButton.Assign(Source: TPersistent{TALControl});
begin
  BeginUpdate;
  Try
    if Source is TALButton then begin
      StateStyles.Assign(TALButton(Source).StateStyles);
    end
    else
      ALAssignError(Source{ASource}, Self{ADest});
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{*******************************}
procedure TALButton.AlignToPixel;
begin
  BeginUpdate;
  try
    inherited;
    StateStyles.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{***********************************}
procedure TALButton.ApplyColorScheme;
begin
  BeginUpdate;
  try
    inherited;
    StateStyles.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{**************************************}
function TALButton.CreateFill: TALBrush;
begin
  Result := TFill.Create;
end;

{**********************************************}
function TALButton.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
end;

{*********************************************************}
function TALButton.CreateTextSettings: TALBaseTextSettings;
begin
  Result := TTextSettings.Create;
end;

{*************************************************}
function TALButton.CreateStateStyles: TStateStyles;
begin
  Result := TStateStyles.Create(self);
end;

{************************************************}
function TALButton.GetTextSettings: TTextSettings;
begin
  Result := TTextSettings(Inherited TextSettings);
end;

{**************************************************************}
procedure TALButton.SetTextSettings(const Value: TTextSettings);
begin
  Inherited SetTextSettings(Value);
end;

{*******************************************************}
procedure TALButton.SetName(const Value: TComponentName);
begin
  var LChangeText := not (csLoading in ComponentState) and (Name = Text) and
    ((Owner = nil) or not (csLoading in TComponent(Owner).ComponentState));
  inherited SetName(Value);
  if LChangeText then
    Text := Value;
end;

{*************************************************************}
procedure TALButton.SetStateStyles(const AValue: TStateStyles);
begin
  FStateStyles.Assign(AValue);
end;

{*******************************************************}
procedure TALButton.TextSettingsChanged(Sender: TObject);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin

    if (not (csLoading in ComponentState)) and
       (not AToStateStyle.TextSettings.inherit) then begin

      if APrevStateStyle.TextSettings.font.Family = AToStateStyle.TextSettings.font.Family then AToStateStyle.TextSettings.font.Family := TextSettings.font.Family;
      if SameValue(APrevStateStyle.TextSettings.font.Size, AToStateStyle.TextSettings.font.Size, TEpsilon.fontSize) then AToStateStyle.TextSettings.font.Size := TextSettings.font.Size;
      if APrevStateStyle.TextSettings.font.Weight = AToStateStyle.TextSettings.font.Weight then AToStateStyle.TextSettings.font.Weight := TextSettings.font.Weight;
      if APrevStateStyle.TextSettings.font.Slant = AToStateStyle.TextSettings.font.Slant then AToStateStyle.TextSettings.font.Slant := TextSettings.font.Slant;
      if APrevStateStyle.TextSettings.font.Stretch = AToStateStyle.TextSettings.font.Stretch then AToStateStyle.TextSettings.font.Stretch := TextSettings.font.Stretch;
      if (APrevStateStyle.TextSettings.font.Color = AToStateStyle.TextSettings.font.Color) and
         (APrevStateStyle.TextSettings.font.ColorKey = AToStateStyle.TextSettings.font.ColorKey) then begin
        AToStateStyle.TextSettings.font.Color := TextSettings.font.Color;
        AToStateStyle.TextSettings.font.ColorKey := TextSettings.font.ColorKey;
      end;

      if APrevStateStyle.TextSettings.Decoration.Kinds = AToStateStyle.TextSettings.Decoration.Kinds then AToStateStyle.TextSettings.Decoration.Kinds := TextSettings.Decoration.Kinds;
      if APrevStateStyle.TextSettings.Decoration.Style = AToStateStyle.TextSettings.Decoration.Style then AToStateStyle.TextSettings.Decoration.Style := TextSettings.Decoration.Style;
      if SameValue(APrevStateStyle.TextSettings.Decoration.ThicknessMultiplier, AToStateStyle.TextSettings.Decoration.ThicknessMultiplier, TEpsilon.Scale) then AToStateStyle.TextSettings.Decoration.ThicknessMultiplier := TextSettings.Decoration.ThicknessMultiplier;
      if (APrevStateStyle.TextSettings.Decoration.Color = AToStateStyle.TextSettings.Decoration.Color) and
         (APrevStateStyle.TextSettings.Decoration.ColorKey = AToStateStyle.TextSettings.Decoration.ColorKey) then begin
        AToStateStyle.TextSettings.Decoration.Color := TextSettings.Decoration.Color;
        AToStateStyle.TextSettings.Decoration.ColorKey := TextSettings.Decoration.ColorKey;
      end;

    end;

    APrevStateStyle.TextSettings.font.Family := TextSettings.font.Family;
    APrevStateStyle.TextSettings.font.Size := TextSettings.font.Size;
    APrevStateStyle.TextSettings.font.Weight := TextSettings.font.Weight;
    APrevStateStyle.TextSettings.font.Slant := TextSettings.font.Slant;
    APrevStateStyle.TextSettings.font.Stretch := TextSettings.font.Stretch;
    APrevStateStyle.TextSettings.font.Color := TextSettings.font.Color;
    APrevStateStyle.TextSettings.font.ColorKey := TextSettings.font.ColorKey;

    APrevStateStyle.TextSettings.Decoration.Kinds := TextSettings.Decoration.Kinds;
    APrevStateStyle.TextSettings.Decoration.Style := TextSettings.Decoration.Style;
    APrevStateStyle.TextSettings.Decoration.ThicknessMultiplier := TextSettings.Decoration.ThicknessMultiplier;
    APrevStateStyle.TextSettings.Decoration.Color := TextSettings.Decoration.Color;
    APrevStateStyle.TextSettings.Decoration.ColorKey := TextSettings.Decoration.ColorKey;

  end;
  {$ENDIF}

begin
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.Disabled, StateStyles.Disabled);
    _PropagateChanges(FPrevStateStyles.Hovered, StateStyles.Hovered);
    _PropagateChanges(FPrevStateStyles.Pressed, StateStyles.Pressed);
    _PropagateChanges(FPrevStateStyles.Focused, StateStyles.Focused);
  end;
  {$ENDIF}
  inherited;
end;

{******************************************************}
procedure TALButton.StateStylesChanged(Sender: TObject);
begin
  ClearBufDrawable;
  DisabledOpacity := StateStyles.Disabled.opacity;
  Repaint;
end;

{*************************************}
procedure TALButton.IsMouseOverChanged;
begin
  inherited;
  StateStyles.Transition.Start;
  repaint;
end;

{***********************************}
procedure TALButton.IsFocusedChanged;
begin
  inherited;
  StateStyles.Transition.Start;
  repaint;
end;

{*********************************}
procedure TALButton.PressedChanged;
begin
  inherited;
  StateStyles.Transition.Start;
  repaint;
end;

{************************}
procedure TALButton.Click;
begin
  if StateStyles.Transition.Running and StateStyles.Transition.DelayClick then
    StateStyles.Transition.ClickDelayed := True
  else
    inherited click;
end;

{***********************************}
procedure TALButton.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (FStateStyles <> nil) and
     (not (csDestroying in ComponentState)) and
     (ALIsDrawableNull(FBufDrawable)) and // warn will be raise in inherited
     ((not ALIsDrawableNull(FStateStyles.Disabled.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Hovered.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Pressed.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Focused.FBufDrawable))) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  if FStateStyles <> nil then
    FStateStyles.ClearBufDrawable;
  inherited ClearBufDrawable;
end;

{**********************************}
procedure TALButton.MakeBufDrawable;
begin
  //--- Do not create BufDrawable if not DoubleBuffered
  if {$IF not DEFINED(ALDPK)}(not DoubleBuffered){$ELSE}False{$ENDIF} then begin
    ClearBufDrawable;
    exit;
  end;
  //--
  inherited MakeBufDrawable;
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if LStateStyle.Inherit then exit;
  if (not ALIsDrawableNull(LStateStyle.FBufDrawable)) then exit;
  if (CacheIndex > 0) and
     (CacheEngine <> nil) and
     (CacheEngine.HasEntry(CacheIndex{AIndex}, GetCacheSubIndex+LStateStyle.CacheSubIndex{ASubIndex})) then Exit;
  LStateStyle.SupersedeNoChanges(true{ASaveState});
  try

    {$IFDEF debug}
    ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Style: ' + LStateStyle.ClassName + ' | Width: ' + ALFloatToStrW(Width)+ ' | Height: ' + ALFloatToStrW(Height));
    {$endif}

    // Create the BufDrawable
    var LTextBroken: Boolean;
    var LAllTextDrawn: Boolean;
    var LElements: TALTextElements;
    CreateBufDrawable(
      LStateStyle.FBufDrawable, // var ABufDrawable: TALDrawable;
      LStateStyle.FBufDrawableRect, // var ABufDrawableRect: TRectF;
      LTextBroken, // var ABufTextBroken: Boolean;
      LAllTextDrawn, // var ABufAllTextDrawn: Boolean;
      LElements, // var ABufElements: TALTextElements;
      ALGetScreenScale * LStateStyle.Scale, // const AScale: Single;
      LStateStyle.Text, // const AText: String;
      LStateStyle.TextSettings.Font, // const AFont: TALFont;
      LStateStyle.TextSettings.Decoration, // const ADecoration: TALTextDecoration;
      LStateStyle.TextSettings.Font, // const AEllipsisFont: TALFont;
      LStateStyle.TextSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
      LStateStyle.Fill, // const AFill: TALBrush;
      LStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
      LStateStyle.Stroke, // const AStroke: TALStrokeBrush;
      LStateStyle.Shadow, // const AShadow: TALShadow;
      LStateStyle.XRadius, // const AXRadius: Single;
      LStateStyle.YRadius); // const AYRadius: Single

    // LStateStyle.FBufDrawableRect must include the LScale
    LStateStyle.FBufDrawableRect.Top := LStateStyle.FBufDrawableRect.Top * LStateStyle.Scale;
    LStateStyle.FBufDrawableRect.right := LStateStyle.FBufDrawableRect.right * LStateStyle.Scale;
    LStateStyle.FBufDrawableRect.left := LStateStyle.FBufDrawableRect.left * LStateStyle.Scale;
    LStateStyle.FBufDrawableRect.bottom := LStateStyle.FBufDrawableRect.bottom * LStateStyle.Scale;

    // Since LStateStyle.FBufDrawableRect can have different dimensions than the main BufDrawableRect
    // (due to autosizing with different font sizes), we must center LStateStyle.FBufDrawableRect
    // within the main BufDrawableRect to ensure that all changes are visually centered.
    var LMainDrawableRect: TRectF;
    if (CacheIndex <= 0) or
       (CacheEngine = nil) or
       (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LMainDrawableRect{ARect})) then begin
      If AlIsDrawableNull(FBufDrawable) then LMainDrawableRect := LocalRect
      else LMainDrawableRect := FBufDrawableRect;
    end;
    LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
    var LCenteredRect := LStateStyle.FBufDrawableRect.CenterAt(LMainDrawableRect);
    LStateStyle.FBufDrawableRect.Offset(LCenteredRect.Left, LCenteredRect.top);

  finally
    LStateStyle.RestorestateNoChanges;
  end;
end;

{**************************************************************************************************************************************************************}
Procedure TALButton.DrawMultilineTextAdjustRect(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; var ARect: TrectF; var ASurfaceSize: TSizeF);
begin

  // If we are drawing directly on the form, center ARect in LocalRect. This is necessary if, for example,
  // the 'to' font size is smaller than the 'from' font size.
  {$IF defined(ALSkiaCanvas)}
  If (Canvas <> nil) and (TSkCanvasCustom(Canvas).Canvas <> nil) and (TSkCanvasCustom(Canvas).Canvas.Handle = ACanvas) then
    // ALAlignToPixelRound is used because when we call ALDrawDrawable,
    // we do LDstRect := AALAlignToPixelRound(LDstRect).
    // Therefore, when drawing directly on the canvas,
    // we must draw at the exact same position as when we call ALDrawDrawable.
    ARect := ALAlignToPixelRound(ARect.CenterAt(LocalRect), Canvas.Matrix, Canvas.Scale, TEpsilon.position)
  else
  {$ENDIF}

end;

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
function TALButton.GetRenderTargetRect(const ARect: TrectF): TRectF;
begin
  if StateStyles.Transition.Running then begin
    Result := ARect;
    if StateStyles.Transition.FromStateStyle <> nil then begin
      var LFromSurfaceRect := ALGetShapeSurfaceRect(
                                ARect, // const ARect: TRectF;
                                AutoAlignToPixel, // const AAlignToPixel: Boolean;
                                _TALBaseStateStyleProtectedAccess(StateStyles.Transition.FromStateStyle).Fill, // const AFill: TALBrush;
                                nil, // const AFillResourceStream: TStream;
                                _TALBaseStateStyleProtectedAccess(StateStyles.Transition.FromStateStyle).StateLayer, // const AStateLayer: TALStateLayer;
                                _TALBaseStateStyleProtectedAccess(StateStyles.Transition.FromStateStyle).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LFromSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
    if StateStyles.Transition.ToStateStyle <> nil then begin
      var LToSurfaceRect := ALGetShapeSurfaceRect(
                              ARect, // const ARect: TRectF;
                              AutoAlignToPixel, // const AAlignToPixel: Boolean;
                              _TALBaseStateStyleProtectedAccess(StateStyles.Transition.ToStateStyle).Fill, // const AFill: TALBrush;
                              nil, // const AFillResourceStream: TStream;
                              _TALBaseStateStyleProtectedAccess(StateStyles.Transition.ToStateStyle).StateLayer, // const AStateLayer: TALStateLayer;
                              _TALBaseStateStyleProtectedAccess(StateStyles.Transition.ToStateStyle).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LToSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
  end
  else begin
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  AutoAlignToPixel, // const AAlignToPixel: Boolean;
                  LStateStyle.Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  LStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
                  LStateStyle.Shadow); // const AShadow: TALShadow): TRectF;
    end
    else begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  AutoAlignToPixel, // const AAlignToPixel: Boolean;
                  Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  nil, // const AStateLayer: TALStateLayer;
                  Shadow); // const AShadow: TALShadow): TRectF;
    end;
  end;
end;
{$ENDIF}

{************************}
procedure TALButton.Paint;
begin

  StateStyles.UpdateLastPaintedRawStyle;

  var LDrawable: TALDrawable := ALNullDrawable;
  var LDrawableRect: TRectF := TRectF.Empty;
  if not StateStyles.Transition.Running then begin
    //--
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      if (CacheIndex <= 0) or
         (CacheEngine = nil) or
         (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex+LStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
        MakeBufDrawable;
        if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(LStateStyle.FBufDrawable)) then begin
          if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, GetCacheSubIndex+LStateStyle.CacheSubIndex{ASubIndex}, LStateStyle.FBufDrawable{ADrawable}, LStateStyle.FBufDrawableRect{ARect}) then ALFreeAndNilDrawable(LStateStyle.FBufDrawable)
          else LStateStyle.FBufDrawable := ALNullDrawable;
          if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex+LStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
            raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
        end
        else begin
          LDrawable := LStateStyle.FBufDrawable;
          LDrawableRect := LStateStyle.FBufDrawableRect;
        end;
      end;
    end;
    //--
    If ALIsDrawableNull(LDrawable) then begin
      if (CacheIndex <= 0) or
         (CacheEngine = nil) or
         (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
        if LStateStyle = nil then MakeBufDrawable;
        if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(fBufDrawable)) then begin
          if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, fBufDrawable{ADrawable}, fBufDrawableRect{ARect}) then ALFreeAndNilDrawable(fBufDrawable)
          else fBufDrawable := ALNullDrawable;
          if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
            raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
        end
        else begin
          LDrawable := FBufDrawable;
          LDrawableRect := FBufDrawableRect;
        end;
      end;
    end;
    //--
  end;

  if ALIsDrawableNull(LDrawable) then begin

    var LCurrentAdjustedStateStyle := TBaseStateStyle(StateStyles.GetCurrentAdjustedStyle);
    if LCurrentAdjustedStateStyle = nil then begin
      inherited Paint;
      exit;
    end;

    {$IF DEFINED(ALSkiaCanvas)}

    // Using a matrix on the canvas results in smoother animations compared to using
    // Ascale with DrawMultilineText. This is because changes in scale affect the font size,
    // leading to rounding issues (I spent many hours looking for a way to avoid this).
    // If there is an animation, it appears jerky because the text position
    // shifts up or down with scale changes due to pixel alignment.
    var LCanvasSaveState: TCanvasSaveState := ALScaleAndCenterCanvas(
                                                Canvas, // Const ACanvas: TCanvas;
                                                AbsoluteRect, // Const AAbsoluteRect: TRectF;
                                                LCurrentAdjustedStateStyle.Scale, // Const AScale: Single;
                                                true); // Const ASaveState: Boolean);
    try

      var LRect := LocalRect;
      var LTextBroken: Boolean;
      var LAllTextDrawn: Boolean;
      var LElements: TALTextElements;
      DrawMultilineText(
        TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
        LRect, // var ARect: TRectF;
        LTextBroken, // out ATextBroken: Boolean;
        LAllTextDrawn, // out AAllTextDrawn: Boolean;
        LElements, // out AElements: TALTextElements;
        1{Ascale},
        AbsoluteOpacity, // const AOpacity: Single;
        LCurrentAdjustedStateStyle.Text, // const AText: String;
        LCurrentAdjustedStateStyle.TextSettings.Font, // const AFont: TALFont;
        LCurrentAdjustedStateStyle.TextSettings.Decoration, // const ADecoration: TALTextDecoration;
        LCurrentAdjustedStateStyle.TextSettings.EllipsisSettings.font, // const AEllipsisFont: TALFont;
        LCurrentAdjustedStateStyle.TextSettings.EllipsisSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
        LCurrentAdjustedStateStyle.Fill, // const AFill: TALBrush;
        LCurrentAdjustedStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
        LCurrentAdjustedStateStyle.Stroke, // const AStroke: TALStrokeBrush;
        LCurrentAdjustedStateStyle.Shadow, // const AShadow: TALShadow);
        LCurrentAdjustedStateStyle.XRadius, // const AXRadius: Single;
        LCurrentAdjustedStateStyle.YRadius); // const AYRadius: Single

    finally
      if LCanvasSaveState <> nil then
        Canvas.RestoreState(LCanvasSaveState);
    end;

    {$ELSE}

    var LRect := LocalRect;
    InitRenderTargets(LRect);
    if ALCanvasBeginScene(RenderTargetCanvas) then
    try

      ALClearCanvas(RenderTargetCanvas, TAlphaColors.Null);

      var LTextBroken: Boolean;
      var LAllTextDrawn: Boolean;
      var LElements: TALTextElements;
      DrawMultilineText(
        RenderTargetCanvas, // const ACanvas: TALCanvas;
        LRect, // out ARect: TRectF;
        LTextBroken, // out ATextBroken: Boolean;
        LAllTextDrawn, // out AAllTextDrawn: Boolean;
        LElements, // out AElements: TALTextElements;
        ALGetScreenScale{Ascale},
        1, // const AOpacity: Single;
        LCurrentAdjustedStateStyle.Text, // const AText: String;
        LCurrentAdjustedStateStyle.TextSettings.Font, // const AFont: TALFont;
        LCurrentAdjustedStateStyle.TextSettings.Decoration, // const ADecoration: TALTextDecoration;
        LCurrentAdjustedStateStyle.TextSettings.EllipsisSettings.font, // const AEllipsisFont: TALFont;
        LCurrentAdjustedStateStyle.TextSettings.EllipsisSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
        LCurrentAdjustedStateStyle.Fill, // const AFill: TALBrush;
        LCurrentAdjustedStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
        LCurrentAdjustedStateStyle.Stroke, // const AStroke: TALStrokeBrush;
        LCurrentAdjustedStateStyle.Shadow, // const AShadow: TALShadow;
        LCurrentAdjustedStateStyle.XRadius, // const AXRadius: Single;
        LCurrentAdjustedStateStyle.YRadius); // const AYRadius: Single;

    finally
      ALCanvasEndScene(RenderTargetCanvas)
    end;

    ALUpdateDrawableFromSurface(RenderTargetSurface, RenderTargetDrawable);

    // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
    // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
    LRect.Offset(-2*LRect.Left, -2*LRect.Top);

    // LRect must include the LScale
    LRect.Top := LRect.Top * LCurrentAdjustedStateStyle.Scale;
    LRect.right := LRect.right * LCurrentAdjustedStateStyle.Scale;
    LRect.left := LRect.left * LCurrentAdjustedStateStyle.Scale;
    LRect.bottom := LRect.bottom * LCurrentAdjustedStateStyle.Scale;

    // Since LStateStyle.FBufDrawableRect can have different dimensions than the main BufDrawableRect
    // (due to autosizing with different font sizes), we must center LStateStyle.FBufDrawableRect
    // within the main BufDrawableRect to ensure that all changes are visually centered.
    var LMainDrawableRect: TRectF;
    if (CacheIndex <= 0) or
       (CacheEngine = nil) or
       (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LMainDrawableRect{ARect})) then begin
      If AlIsDrawableNull(FBufDrawable) then LMainDrawableRect := LocalRect
      else LMainDrawableRect := FBufDrawableRect;
    end;
    LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
    var LCenteredRect := LRect.CenterAt(LMainDrawableRect);
    LRect.Offset(LCenteredRect.Left, LCenteredRect.top);

    // We cannot use the matrix because, if we do, ALAlignToPixelRound in ALDrawDrawable
    // will be ineffective since the matrix will no longer be a simple translation matrix.
    // In such a case, TCustomCanvasGpu(ACanvas).DrawTexture may produce border artifacts
    // if the texture is not perfectly pixel-aligned.
    var LDstRect := TRectF.Create(0, 0, ALGetDrawableWidth(RenderTargetDrawable), ALGetDrawableHeight(RenderTargetDrawable));
    LDstRect.Width := (LDstRect.Width / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
    LDstRect.height := (LDstRect.height / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
    LDstRect.SetLocation(
      LRect.Left,
      LRect.Top);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      RenderTargetDrawable, // const ADrawable: TALDrawable;
      LDstRect, // const ADstRect: TrectF; // IN Virtual pixels !
      AbsoluteOpacity); // const AOpacity: Single)

    {$ENDIF}

    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{*************************************************************************}
constructor TALToggleButton.TGroupMessage.Create(const AGroupName: string);
begin
  inherited Create;
  FGroupName := AGroupName;
end;

{**********************************************************}
function TALToggleButton.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $ffe1e1e1;
end;

{************************************************************}
function TALToggleButton.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := $ffadadad;
end;

{*************************************************************************}
function TALToggleButton.TTextSettings.TFont.GetDefaultWeight: TFontWeight;
begin
  Result := TFontWeight.medium;
end;

{*********************************************************}
function TALToggleButton.TTextSettings.CreateFont: TALFont;
begin
  Result := TFont.Create;
end;

{***************************************************************************}
function TALToggleButton.TTextSettings.GetDefaultHorzAlign: TALTextHorzAlign;
begin
  Result := TALTextHorzAlign.center;
end;

{**************************************************************************}
function TALToggleButton.TBaseStateStyle.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $FFE1E1E1;
end;

{****************************************************************************}
function TALToggleButton.TBaseStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := $FFADADAD;
end;

{*****************************************************************************************}
function TALToggleButton.TBaseStateStyle.TTextSettings.TFont.GetDefaultWeight: TFontWeight;
begin
  Result := TFontWeight.medium;
end;

{*************************************************************************}
function TALToggleButton.TBaseStateStyle.TTextSettings.CreateFont: TALFont;
begin
  Result := TFont.Create;
end;

{*************************************************************************}
constructor TALToggleButton.TBaseStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  FText := DefaultText;
  if StateStyleParent <> nil then FTextSettings := CreateTextSettings(StateStyleParent.TextSettings)
  else if ControlParent <> nil then FTextSettings := CreateTextSettings(ControlParent.TextSettings)
  else FTextSettings := CreateTextSettings(nil);
  FTextSettings.OnChanged := TextSettingsChanged;
  FXRadius := DefaultXRadius;
  FYRadius := DefaultYRadius;
end;

{*************************************************}
destructor TALToggleButton.TBaseStateStyle.Destroy;
begin
  ALFreeAndNil(FTextSettings);
  inherited Destroy;
end;

{********************************************************************************************}
function TALToggleButton.TBaseStateStyle.CreateFill(const AParent: TALBrush): TALInheritBrush;
begin
  Result := TFill.Create(AParent);
end;

{**********************************************************************************************************}
function TALToggleButton.TBaseStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{*****************************************************************************************************************************}
function TALToggleButton.TBaseStateStyle.CreateTextSettings(const AParent: TALBaseTextSettings): TBaseStateStyle.TTextSettings;
begin
  Result := TTextSettings.Create(AParent);
end;

{********************************************************************}
procedure TALToggleButton.TBaseStateStyle.Assign(Source: TPersistent);
begin
  if Source is TBaseStateStyle then begin
    BeginUpdate;
    Try
      Text := TBaseStateStyle(Source).text;
      TextSettings.Assign(TBaseStateStyle(Source).TextSettings);
      XRadius := TBaseStateStyle(Source).XRadius;
      YRadius := TBaseStateStyle(Source).YRadius;
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{**********************************************}
procedure TALToggleButton.TBaseStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Text := DefaultText;
    TextSettings.reset;
    XRadius := DefaultXRadius;
    YRadius := DefaultYRadius;
  finally
    EndUpdate;
  end;
end;

{*****************************************************}
procedure TALToggleButton.TBaseStateStyle.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    TextSettings.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{*********************************************************}
procedure TALToggleButton.TBaseStateStyle.ApplyColorScheme;
begin
  BeginUpdate;
  Try
    inherited;
    TextSettings.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{******************************************************************************************************************************************}
procedure TALToggleButton.TBaseStateStyle.Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single; const AReverse: Boolean);
begin
  {$IF defined(debug)}
  if (ATo <> nil) and (not (ATo is TBaseStateStyle)) then
    Raise Exception.Create('Error F3C72244-894F-4B67-AD86-F24DF5039927');
  {$ENDIF}
  BeginUpdate;
  try
    Inherited Interpolate(ATo, ANormalizedTime, AReverse);
    if ATo <> nil then begin
      if not AReverse then Text := TBaseStateStyle(ATo).Text;
      TextSettings.Interpolate(TBaseStateStyle(ATo).TextSettings, ANormalizedTime, AReverse);
      XRadius := InterpolateSingle(XRadius{Start}, TBaseStateStyle(ATo).XRadius{Stop}, ANormalizedTime);
      YRadius := InterpolateSingle(YRadius{Start}, TBaseStateStyle(ATo).YRadius{Stop}, ANormalizedTime);
    end
    else if StateStyleParent <> nil then begin
      StateStyleParent.SupersedeNoChanges(true{ASaveState});
      try
        if not AReverse then Text := StateStyleParent.Text;
        TextSettings.Interpolate(StateStyleParent.TextSettings, ANormalizedTime, AReverse);
        XRadius := InterpolateSingle(XRadius{Start}, StateStyleParent.XRadius{Stop}, ANormalizedTime);
        YRadius := InterpolateSingle(YRadius{Start}, StateStyleParent.YRadius{Stop}, ANormalizedTime);
      finally
        StateStyleParent.RestoreStateNoChanges;
      end;
    end
    else if ControlParent <> nil then begin
      if not AReverse then Text := ControlParent.Text;
      TextSettings.Interpolate(ControlParent.TextSettings, ANormalizedTime, AReverse);
      XRadius := InterpolateSingle(XRadius{Start}, ControlParent.XRadius{Stop}, ANormalizedTime);
      YRadius := InterpolateSingle(YRadius{Start}, ControlParent.YRadius{Stop}, ANormalizedTime);
    end
    else begin
      if not AReverse then Text := DefaultText;
      TextSettings.Interpolate(nil, ANormalizedTime, AReverse);
      XRadius := InterpolateSingle(XRadius{Start}, ALIfThen(IsNaN(DefaultXRadius), 0, DefaultXRadius){Stop}, ANormalizedTime);
      YRadius := InterpolateSingle(YRadius{Start}, ALIfThen(IsNaN(DefaultYRadius), 0, DefaultYRadius){Stop}, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{****************************************************}
procedure TALToggleButton.TBaseStateStyle.DoSupersede;
begin
  Inherited;
  if Text = '' then begin
    if StateStyleParent <> nil then Text := StateStyleParent.Text
    else Text := ControlParent.Text;
  end;
  if IsNaN(XRadius) then begin
    if StateStyleParent <> nil then XRadius := StateStyleParent.XRadius
    else XRadius := ControlParent.XRadius;
  end;
  if IsNaN(YRadius) then begin
    if StateStyleParent <> nil then YRadius := StateStyleParent.YRadius
    else YRadius := ControlParent.YRadius;
  end;
  TextSettings.SuperSede;
end;

{****************************************************************************}
function TALToggleButton.TBaseStateStyle.GetStateStyleParent: TBaseStateStyle;
begin
  {$IF defined(debug)}
  if (inherited StateStyleParent <> nil) and
     (not (inherited StateStyleParent is TBaseStateStyle)) then
    raise Exception.Create('StateStyleParent must be of type TBaseStateStyle');
  {$ENDIF}
  result := TBaseStateStyle(inherited StateStyleParent);
end;

{*************************************************************************}
function TALToggleButton.TBaseStateStyle.GetControlParent: TALToggleButton;
begin
  {$IF defined(debug)}
  if (inherited ControlParent <> nil) and
     (not (inherited ControlParent is TALToggleButton)) then
    raise Exception.Create('ControlParent must be of type TALToggleButton');
  {$ENDIF}
  result := TALToggleButton(inherited ControlParent);
end;

{*********************************************************************}
procedure TALToggleButton.TBaseStateStyle.SetText(const Value: string);
begin
  if FText <> Value then begin
    FText := Value;
    Change;
  end;
end;

{*****************************************************************************************************}
procedure TALToggleButton.TBaseStateStyle.SetTextSettings(const AValue: TBaseStateStyle.TTextSettings);
begin
  FTextSettings.Assign(AValue);
end;

{************************************************************************}
procedure TALToggleButton.TBaseStateStyle.SetXRadius(const Value: Single);
begin
  if IsNan(FXRadius) and IsNan(Value) then Exit;
  if not SameValue(FXRadius, Value, TEpsilon.Vector) then begin
    FXRadius := Value;
    Change;
  end;
end;

{************************************************************************}
procedure TALToggleButton.TBaseStateStyle.SetYRadius(const Value: Single);
begin
  if IsNan(FYRadius) and IsNan(Value) then Exit;
  if not SameValue(FYRadius, Value, TEpsilon.Vector) then begin
    FYRadius := Value;
    Change;
  end;
end;

{**************************************************************}
function TALToggleButton.TBaseStateStyle.GetDefaultText: String;
begin
  Result := '';
end;

{*****************************************************************}
function TALToggleButton.TBaseStateStyle.GetDefaultXRadius: Single;
begin
  Result := NaN;
end;

{*****************************************************************}
function TALToggleButton.TBaseStateStyle.GetDefaultYRadius: Single;
begin
  Result := NaN;
end;

{***********************************************************}
function TALToggleButton.TBaseStateStyle.GetInherit: Boolean;
begin
  Result := inherited GetInherit and
            IsNan(FXRadius) and
            IsNan(FYRadius) and
            Text.IsEmpty and
            TextSettings.Inherit;
end;

{******************************************************************************}
procedure TALToggleButton.TBaseStateStyle.TextSettingsChanged(ASender: TObject);
begin
  Change;
end;

{*************************************************************}
function TALToggleButton.TBaseStateStyle.IsTextStored: Boolean;
begin
  Result := FText <> DefaultText;
end;

{****************************************************************}
function TALToggleButton.TBaseStateStyle.IsXRadiusStored: Boolean;
begin
  if IsNan(FXRadius) and IsNan(DefaultXRadius) then Exit(False);
  Result := not SameValue(FXRadius, DefaultXRadius, TEpsilon.Vector);
end;

{****************************************************************}
function TALToggleButton.TBaseStateStyle.IsYRadiusStored: Boolean;
begin
  if IsNan(FYRadius) and IsNan(DefaultYRadius) then Exit(False);
  Result := not SameValue(FYRadius, DefaultYRadius, TEpsilon.Vector);
end;

{********************************************************************}
function TALToggleButton.TDefaultStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 1;
end;

{********************************************************************}
function TALToggleButton.TDisabledStateStyle.IsOpacityStored: Boolean;
begin
  Result := not SameValue(FOpacity, TControl.DefaultDisabledOpacity, TEpsilon.Scale);
end;

{****************************************************************************}
procedure TALToggleButton.TDisabledStateStyle.SetOpacity(const Value: Single);
begin
  if not SameValue(FOpacity, Value, TEpsilon.Scale) then begin
    FOpacity := Value;
    Change;
  end;
end;

{*****************************************************************************}
constructor TALToggleButton.TDisabledStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  FOpacity := TControl.DefaultDisabledOpacity;
end;

{************************************************************************}
procedure TALToggleButton.TDisabledStateStyle.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TDisabledStateStyle then
      Opacity := TDisabledStateStyle(Source).Opacity
    else
      Opacity := TControl.DefaultDisabledOpacity;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{**************************************************}
procedure TALToggleButton.TDisabledStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Opacity := TControl.DefaultDisabledOpacity;
  finally
    EndUpdate;
  end;
end;

{***************************************************************}
function TALToggleButton.TDisabledStateStyle.GetInherit: Boolean;
begin
  // Opacity is not part of the GetInherit function because it updates the
  // disabledOpacity of the base control immediately every time it changes.
  // Essentially, it acts merely as a link to the disabledOpacity of the base control.
  Result := inherited GetInherit;
end;

{*********************************************************************}
function TALToggleButton.TDisabledStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 2;
end;

{********************************************************************}
function TALToggleButton.THoveredStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 3;
end;

{********************************************************************}
function TALToggleButton.TPressedStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 4;
end;

{********************************************************************}
function TALToggleButton.TFocusedStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 5;
end;

{******************************************************************************}
constructor TALToggleButton.TCheckStateStyles.Create(const AParent: TALControl);
begin
  inherited Create;
  //--
  FDefault := CreateDefaultStateStyle(AParent);
  FDefault.OnChanged := DefaultChanged;
  //--
  FDisabled := CreateDisabledStateStyle(FDefault);
  FDisabled.OnChanged := DisabledChanged;
  //--
  FHovered := CreateHoveredStateStyle(FDefault);
  FHovered.OnChanged := HoveredChanged;
  //--
  FPressed := CreatePressedStateStyle(FDefault);
  FPressed.OnChanged := PressedChanged;
  //--
  FFocused := CreateFocusedStateStyle(FDefault);
  FFocused.OnChanged := FocusedChanged;
end;

{***************************************************}
destructor TALToggleButton.TCheckStateStyles.Destroy;
begin
  ALFreeAndNil(FDefault);
  ALFreeAndNil(FDisabled);
  ALFreeAndNil(FHovered);
  ALFreeAndNil(FPressed);
  ALFreeAndNil(FFocused);
  inherited Destroy;
end;

{*********************************************************************************}
function TALToggleButton.TCheckStateStyles.CreateSavedState: TALPersistentObserver;
type
  TCheckStateStylesClass = class of TCheckStateStyles;
begin
  result := TCheckStateStylesClass(classtype).Create(nil{AParent});
end;

{*************************************************************************************************************}
function TALToggleButton.TCheckStateStyles.CreateDefaultStateStyle(const AParent: TObject): TDefaultStateStyle;
begin
  Result := TDefaultStateStyle.Create(AParent);
end;

{***************************************************************************************************************}
function TALToggleButton.TCheckStateStyles.CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle;
begin
  Result := TDisabledStateStyle.Create(AParent);
end;

{*************************************************************************************************************}
function TALToggleButton.TCheckStateStyles.CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle;
begin
  Result := THoveredStateStyle.Create(AParent);
end;

{*************************************************************************************************************}
function TALToggleButton.TCheckStateStyles.CreatePressedStateStyle(const AParent: TObject): TPressedStateStyle;
begin
  Result := TPressedStateStyle.Create(AParent);
end;

{*************************************************************************************************************}
function TALToggleButton.TCheckStateStyles.CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle;
begin
  Result := TFocusedStateStyle.Create(AParent);
end;

{**********************************************************************}
procedure TALToggleButton.TCheckStateStyles.Assign(Source: TPersistent);
begin
  if Source is TCheckStateStyles then begin
    BeginUpdate;
    Try
      Default.Assign(TCheckStateStyles(Source).Default);
      Disabled.Assign(TCheckStateStyles(Source).Disabled);
      Hovered.Assign(TCheckStateStyles(Source).Hovered);
      Pressed.Assign(TCheckStateStyles(Source).Pressed);
      Focused.Assign(TCheckStateStyles(Source).Focused);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{************************************************}
procedure TALToggleButton.TCheckStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Default.Reset;
    Disabled.Reset;
    Hovered.Reset;
    Pressed.Reset;
    Focused.Reset;
  finally
    EndUpdate;
  end;
end;

{*******************************************************}
procedure TALToggleButton.TCheckStateStyles.AlignToPixel;
begin
  BeginUpdate;
  Try
    Default.AlignToPixel;
    Disabled.AlignToPixel;
    Hovered.AlignToPixel;
    Pressed.AlignToPixel;
    Focused.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{***********************************************************}
procedure TALToggleButton.TCheckStateStyles.ApplyColorScheme;
begin
  BeginUpdate;
  Try
    Default.ApplyColorScheme;
    Disabled.ApplyColorScheme;
    Hovered.ApplyColorScheme;
    Pressed.ApplyColorScheme;
    Focused.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{***********************************************************}
procedure TALToggleButton.TCheckStateStyles.ClearBufDrawable;
begin
  Default.ClearBufDrawable;
  Disabled.ClearBufDrawable;
  Hovered.ClearBufDrawable;
  Pressed.ClearBufDrawable;
  Focused.ClearBufDrawable;
end;

{***************************************************************************************}
procedure TALToggleButton.TCheckStateStyles.SetDefault(const AValue: TDefaultStateStyle);
begin
  FDefault.Assign(AValue);
end;

{*****************************************************************************************}
procedure TALToggleButton.TCheckStateStyles.SetDisabled(const AValue: TDisabledStateStyle);
begin
  FDisabled.Assign(AValue);
end;

{***************************************************************************************}
procedure TALToggleButton.TCheckStateStyles.SetHovered(const AValue: THoveredStateStyle);
begin
  FHovered.Assign(AValue);
end;

{***************************************************************************************}
procedure TALToggleButton.TCheckStateStyles.SetPressed(const AValue: TPressedStateStyle);
begin
  FPressed.Assign(AValue);
end;

{***************************************************************************************}
procedure TALToggleButton.TCheckStateStyles.SetFocused(const AValue: TFocusedStateStyle);
begin
  FFocused.Assign(AValue);
end;

{***************************************************************************}
procedure TALToggleButton.TCheckStateStyles.DefaultChanged(ASender: TObject);
begin
  Change;
end;

{****************************************************************************}
procedure TALToggleButton.TCheckStateStyles.DisabledChanged(ASender: TObject);
begin
  Change;
end;

{***************************************************************************}
procedure TALToggleButton.TCheckStateStyles.HoveredChanged(ASender: TObject);
begin
  Change;
end;

{***************************************************************************}
procedure TALToggleButton.TCheckStateStyles.PressedChanged(ASender: TObject);
begin
  Change;
end;

{***************************************************************************}
procedure TALToggleButton.TCheckStateStyles.FocusedChanged(ASender: TObject);
begin
  Change;
end;

{*************************************************************************}
constructor TALToggleButton.TStateStyles.Create(const AParent: TALControl);
begin
  inherited Create(AParent);
  //--
  FChecked := CreateCheckedStateStyles(AParent);
  FChecked.OnChanged := CheckedChanged;
  //--
  FUnchecked := CreateUnCheckedStateStyles(AParent);
  FUnchecked.OnChanged := UncheckedChanged;
end;

{**********************************************}
destructor TALToggleButton.TStateStyles.Destroy;
begin
  ALFreeAndNil(FChecked);
  ALFreeAndNil(FUnchecked);
  inherited Destroy;
end;

{*************************************************************************************}
function TALToggleButton.TStateStyles.CreateTransition: TALBaseStateStyles.TTransition;
begin
  result := TTransition.Create(Self);
end;

{***********************************************************************************************************}
function TALToggleButton.TStateStyles.CreateCheckedStateStyles(const AParent: TALControl): TCheckStateStyles;
begin
  Result := TCheckStateStyles.Create(AParent);
end;

{*************************************************************************************************************}
function TALToggleButton.TStateStyles.CreateUncheckedStateStyles(const AParent: TALControl): TCheckStateStyles;
begin
  Result := TCheckStateStyles.Create(AParent);
end;

{*****************************************************************}
procedure TALToggleButton.TStateStyles.Assign(Source: TPersistent);
begin
  if Source is TStateStyles then begin
    BeginUpdate;
    Try
      Checked.Assign(TStateStyles(Source).Checked);
      Unchecked.Assign(TStateStyles(Source).Unchecked);
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{*******************************************}
procedure TALToggleButton.TStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Checked.reset;
    Unchecked.reset;
  finally
    EndUpdate;
  end;
end;

{**************************************************}
procedure TALToggleButton.TStateStyles.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    Checked.AlignToPixel;
    Unchecked.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{******************************************************}
procedure TALToggleButton.TStateStyles.ApplyColorScheme;
begin
  BeginUpdate;
  Try
    inherited;
    Checked.ApplyColorScheme;
    Unchecked.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{******************************************************}
procedure TALToggleButton.TStateStyles.ClearBufDrawable;
begin
  inherited;
  Checked.ClearBufDrawable;
  Unchecked.ClearBufDrawable;
end;

{**************************************************************************}
function TALToggleButton.TStateStyles.GetCurrentRawStyle: TALBaseStateStyle;
begin
  if Parent.Checked then begin
    if Not Parent.Enabled then Result := Checked.Disabled
    else if Parent.Pressed then Result := Checked.Pressed
    else if Parent.IsFocused then Result := Checked.Focused
    else if Parent.IsMouseOver then Result := Checked.Hovered
    else result := Checked.Default;
  end
  else begin
    if Not Parent.Enabled then Result := UnChecked.Disabled
    else if Parent.Pressed then Result := UnChecked.Pressed
    else if Parent.IsFocused then Result := UnChecked.Focused
    else if Parent.IsMouseOver then Result := UnChecked.Hovered
    else result := UnChecked.Default;
  end;
end;

{***************************************************************}
function TALToggleButton.TStateStyles.GetParent: TALToggleButton;
begin
  Result := TALToggleButton(inherited Parent);
end;

{****************************************************************************}
function TALToggleButton.TStateStyles.GetTransition: TStateStyles.TTransition;
begin
  Result := TStateStyles.TTransition(inherited Transition);
end;

{*******************************************************************************************}
procedure TALToggleButton.TStateStyles.SetTransition(const AValue: TStateStyles.TTransition);
begin
  inherited Transition := AValue;
end;

{*********************************************************************************}
procedure TALToggleButton.TStateStyles.SetChecked(const AValue: TCheckStateStyles);
begin
  FChecked.Assign(AValue);
end;

{***********************************************************************************}
procedure TALToggleButton.TStateStyles.SetUnchecked(const AValue: TCheckStateStyles);
begin
  FUnchecked.Assign(AValue);
end;

{**********************************************************************}
procedure TALToggleButton.TStateStyles.CheckedChanged(ASender: TObject);
begin
  Change;
end;

{************************************************************************}
procedure TALToggleButton.TStateStyles.UncheckedChanged(ASender: TObject);
begin
  Change;
end;

{*****************************************************}
constructor TALToggleButton.Create(AOwner: TComponent);
begin
  {$IF defined(ALDPK)}
  FPrevStateStyles := nil;
  {$ENDIF}
  FStateStyles := nil;
  //--
  inherited Create(AOwner);
  //--
  CanFocus := True;
  HitTest := True;
  AutoSize := TALAutoSizeMode.Both;
  Cursor := crHandPoint;
  //--
  FGroupName := '';
  fMandatory := false;
  TMessageManager.DefaultManager.SubscribeToMessage(TGroupMessage, GroupMessageCall);
  //--
  FChecked := False;
  FOnChange := nil;
  //--
  var LPaddingChange: TNotifyEvent := Padding.OnChange;
  Padding.OnChange := nil;
  Padding.DefaultValue := TRectF.create(12{Left}, 6{Top}, 12{Right}, 6{Bottom});
  Padding.Rect := Padding.DefaultValue;
  padding.OnChange := LPaddingChange;
  //--
  {$IF defined(ALDPK)}
  FPrevStateStyles := TStateStyles.Create(nil);
  {$ENDIF}
  //--
  FStateStyles := CreateStateStyles;
  FStateStyles.OnChanged := StateStylesChanged;
end;

{*********************************}
destructor TALToggleButton.Destroy;
begin
  {$IF defined(ALDPK)}
  ALFreeAndNil(FPrevStateStyles);
  {$ENDIF}
  ALFreeAndNil(FStateStyles);
  inherited Destroy;
end;

{******************************************}
procedure TALToggleButton.BeforeDestruction;
begin
  if BeforeDestructionExecuted then exit;
  // Unsubscribe from TGroupMessage to stop receiving messages.
  // This must be done in BeforeDestruction rather than in Destroy,
  // because the control might be freed in the background via ALFreeAndNil(..., delayed),
  // and BeforeDestruction is guaranteed to execute on the main thread.
  TMessageManager.DefaultManager.Unsubscribe(TGroupMessage, GroupMessageCall);
  inherited;
end;

{****************************************************************}
procedure TALToggleButton.Assign(Source: TPersistent{TALControl});
begin
  BeginUpdate;
  Try
    if Source is TALToggleButton then begin
      StateStyles.Assign(TALToggleButton(Source).StateStyles);
      GroupName := TALToggleButton(Source).GroupName;
      Mandatory := TALToggleButton(Source).Mandatory;
      Checked := TALToggleButton(Source).Checked;
      OnChange := TALToggleButton(Source).OnChange;
    end
    else
      ALAssignError(Source{ASource}, Self{ADest});
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{*************************************}
procedure TALToggleButton.AlignToPixel;
begin
  BeginUpdate;
  try
    inherited;
    StateStyles.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{*****************************************}
procedure TALToggleButton.ApplyColorScheme;
begin
  BeginUpdate;
  Try
    inherited;
    StateStyles.ApplyColorScheme;
  finally
    EndUpdate;
  end;
end;

{********************************************}
function TALToggleButton.CreateFill: TALBrush;
begin
  Result := TFill.Create;
end;

{****************************************************}
function TALToggleButton.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
end;

{***************************************************************}
function TALToggleButton.CreateTextSettings: TALBaseTextSettings;
begin
  Result := TTextSettings.Create;
end;

{*******************************************************}
function TALToggleButton.CreateStateStyles: TStateStyles;
begin
  Result := TStateStyles.Create(self);
end;

{*******************************************}
function TALToggleButton.GetChecked: Boolean;
begin
  Result := FChecked;
end;

{*********************************************************}
procedure TALToggleButton.SetChecked(const Value: Boolean);

  {~~~~~~~~~~~~~~~~~~~~~~}
  procedure _doSetChecked;
  begin
    FChecked := Value;
    if FChecked then DisabledOpacity := StateStyles.Checked.Disabled.opacity
    else DisabledOpacity := StateStyles.Unchecked.Disabled.opacity;
    AdjustSize;
    DoChanged;
  end;

begin
  if FChecked <> Value then begin
    if (csDesigning in ComponentState) and FChecked then _doSetChecked // allows check/uncheck in design-mode
    else begin
      if (not value) and fMandatory then exit;
      _doSetChecked;
      if Value and (GroupName <> '') then begin
        var M := TGroupMessage.Create(GroupName);
        TMessageManager.DefaultManager.SendMessage(Self, M, True);
      end;
    end;
  end;
end;

{******************************************************}
function TALToggleButton.GetTextSettings: TTextSettings;
begin
  Result := TTextSettings(Inherited TextSettings);
end;

{********************************************************************}
procedure TALToggleButton.SetTextSettings(const Value: TTextSettings);
begin
  Inherited SetTextSettings(Value);
end;

{*************************************************************}
procedure TALToggleButton.SetName(const Value: TComponentName);
begin
  var LChangeText := not (csLoading in ComponentState) and (Name = Text) and
    ((Owner = nil) or not (csLoading in TComponent(Owner).ComponentState));
  inherited SetName(Value);
  if LChangeText then
    Text := Value;
end;

{*******************************************************************}
procedure TALToggleButton.SetStateStyles(const AValue: TStateStyles);
begin
  FStateStyles.Assign(AValue);
end;

{********************************************}
function TALToggleButton.GetGroupName: string;
begin
  Result := FGroupName;
end;

{***********************************************************************************}
procedure TALToggleButton.GroupMessageCall(const Sender: TObject; const M: TMessage);
begin
  if SameText(TGroupMessage(M).GroupName, GroupName) and (Sender <> Self) and (Root <> nil) and
     (not (Sender is TControl) or ((Sender as TControl).Root = Root)) then begin
    var LOldMandatory := fMandatory;
    fMandatory := False;
    try
      Checked := False;
    finally
      fMandatory := LOldMandatory;
    end;
  end;
end;

{************************************************}
function TALToggleButton.GroupNameStored: Boolean;
begin
  Result := FGroupName <> '';
end;

{**********************************************************}
procedure TALToggleButton.SetGroupName(const Value: string);
begin
  if FGroupName <> Value then
    FGroupName := Value;
end;

{*************************************************************}
procedure TALToggleButton.TextSettingsChanged(Sender: TObject);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin

    if (not (csLoading in ComponentState)) and
       (not AToStateStyle.TextSettings.inherit) then begin

      if APrevStateStyle.TextSettings.font.Family = AToStateStyle.TextSettings.font.Family then AToStateStyle.TextSettings.font.Family := TextSettings.font.Family;
      if SameValue(APrevStateStyle.TextSettings.font.Size, AToStateStyle.TextSettings.font.Size, TEpsilon.fontSize) then AToStateStyle.TextSettings.font.Size := TextSettings.font.Size;
      if APrevStateStyle.TextSettings.font.Weight = AToStateStyle.TextSettings.font.Weight then AToStateStyle.TextSettings.font.Weight := TextSettings.font.Weight;
      if APrevStateStyle.TextSettings.font.Slant = AToStateStyle.TextSettings.font.Slant then AToStateStyle.TextSettings.font.Slant := TextSettings.font.Slant;
      if APrevStateStyle.TextSettings.font.Stretch = AToStateStyle.TextSettings.font.Stretch then AToStateStyle.TextSettings.font.Stretch := TextSettings.font.Stretch;
      if (APrevStateStyle.TextSettings.font.Color = AToStateStyle.TextSettings.font.Color) and
         (APrevStateStyle.TextSettings.font.ColorKey = AToStateStyle.TextSettings.font.ColorKey) then begin
        AToStateStyle.TextSettings.font.Color := TextSettings.font.Color;
        AToStateStyle.TextSettings.font.ColorKey := TextSettings.font.ColorKey;
      end;

      if APrevStateStyle.TextSettings.Decoration.Kinds = AToStateStyle.TextSettings.Decoration.Kinds then AToStateStyle.TextSettings.Decoration.Kinds := TextSettings.Decoration.Kinds;
      if APrevStateStyle.TextSettings.Decoration.Style = AToStateStyle.TextSettings.Decoration.Style then AToStateStyle.TextSettings.Decoration.Style := TextSettings.Decoration.Style;
      if SameValue(APrevStateStyle.TextSettings.Decoration.ThicknessMultiplier, AToStateStyle.TextSettings.Decoration.ThicknessMultiplier, TEpsilon.Scale) then AToStateStyle.TextSettings.Decoration.ThicknessMultiplier := TextSettings.Decoration.ThicknessMultiplier;
      if (APrevStateStyle.TextSettings.Decoration.Color = AToStateStyle.TextSettings.Decoration.Color) and
         (APrevStateStyle.TextSettings.Decoration.ColorKey = AToStateStyle.TextSettings.Decoration.ColorKey) then begin
        AToStateStyle.TextSettings.Decoration.Color := TextSettings.Decoration.Color;
        AToStateStyle.TextSettings.Decoration.ColorKey := TextSettings.Decoration.ColorKey;
      end;

    end;

    APrevStateStyle.TextSettings.font.Family := TextSettings.font.Family;
    APrevStateStyle.TextSettings.font.Size := TextSettings.font.Size;
    APrevStateStyle.TextSettings.font.Weight := TextSettings.font.Weight;
    APrevStateStyle.TextSettings.font.Slant := TextSettings.font.Slant;
    APrevStateStyle.TextSettings.font.Stretch := TextSettings.font.Stretch;
    APrevStateStyle.TextSettings.font.Color := TextSettings.font.Color;
    APrevStateStyle.TextSettings.font.ColorKey := TextSettings.font.ColorKey;

    APrevStateStyle.TextSettings.Decoration.Kinds := TextSettings.Decoration.Kinds;
    APrevStateStyle.TextSettings.Decoration.Style := TextSettings.Decoration.Style;
    APrevStateStyle.TextSettings.Decoration.ThicknessMultiplier := TextSettings.Decoration.ThicknessMultiplier;
    APrevStateStyle.TextSettings.Decoration.Color := TextSettings.Decoration.Color;
    APrevStateStyle.TextSettings.Decoration.ColorKey := TextSettings.Decoration.ColorKey;

  end;
  {$ENDIF}

begin
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.checked.Default, StateStyles.checked.Default);
    _PropagateChanges(FPrevStateStyles.checked.Disabled, StateStyles.checked.Disabled);
    _PropagateChanges(FPrevStateStyles.checked.Hovered, StateStyles.checked.Hovered);
    _PropagateChanges(FPrevStateStyles.checked.Pressed, StateStyles.checked.Pressed);
    _PropagateChanges(FPrevStateStyles.checked.Focused, StateStyles.checked.Focused);
    _PropagateChanges(FPrevStateStyles.Unchecked.Default, StateStyles.Unchecked.Default);
    _PropagateChanges(FPrevStateStyles.Unchecked.Disabled, StateStyles.Unchecked.Disabled);
    _PropagateChanges(FPrevStateStyles.Unchecked.Hovered, StateStyles.Unchecked.Hovered);
    _PropagateChanges(FPrevStateStyles.Unchecked.Pressed, StateStyles.Unchecked.Pressed);
    _PropagateChanges(FPrevStateStyles.Unchecked.Focused, StateStyles.Unchecked.Focused);
  end;
  {$ENDIF}
  inherited;
end;

{************************************************************}
procedure TALToggleButton.StateStylesChanged(Sender: TObject);
begin
  ClearBufDrawable;
  if Checked then DisabledOpacity := StateStyles.Checked.Disabled.opacity
  else DisabledOpacity := StateStyles.Unchecked.Disabled.opacity;
  Repaint;
end;

{*******************************************}
procedure TALToggleButton.IsMouseOverChanged;
begin
  inherited;
  StateStyles.Transition.Start;
  repaint;
end;

{*****************************************}
procedure TALToggleButton.IsFocusedChanged;
begin
  inherited;
  StateStyles.Transition.Start;
  repaint;
end;

{***************************************}
procedure TALToggleButton.PressedChanged;
begin
  inherited;
  StateStyles.Transition.Start;
  repaint;
end;

{*************************************************************************************************}
procedure TALToggleButton.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
begin
  inherited;
  if (KeyChar = ' ') then begin
    Click; // Emulate mouse click to perform Action.OnExecute
    KeyChar := #0;
  end;
end;

{*************************************}
procedure TALToggleButton.DoClickSound;
begin
  if (ClickSound=TALClickSoundMode.Always) or
     ((ClickSound=TALClickSoundMode.Default) and ALGlobalClickSoundEnabled) then
    ALPlayClickSound;
end;

{******************************}
procedure TALToggleButton.Click;
begin
  if StateStyles.Transition.Running and StateStyles.Transition.DelayClick then begin
    Checked := not Checked;
    StateStyles.Transition.ClickDelayed := True
  end
  else begin
    if not StateStyles.Transition.ClickDelayed then
      Checked := not Checked;
    inherited click;
  end;
end;

{**********************************}
procedure TALToggleButton.DoChanged;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  Repaint;
end;

{*****************************************}
procedure TALToggleButton.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (FStateStyles <> nil) and
     (not (csDestroying in ComponentState)) and
     (ALIsDrawableNull(FBufDrawable)) and // warn will be raise in inherited
     ((not ALIsDrawableNull(FStateStyles.Checked.Default.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Disabled.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Hovered.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Pressed.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Focused.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Default.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Disabled.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Hovered.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Pressed.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Focused.FBufDrawable))) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  if FStateStyles <> nil then
    FStateStyles.ClearBufDrawable;
  inherited ClearBufDrawable;
end;

{***********************************}
procedure TALToggleButton.AdjustSize;
begin
  if (not (csLoading in ComponentState)) and // loaded will call again AdjustSize
     (not (csDestroying in ComponentState)) and // if csDestroying do not do autosize
     (StateStyles <> nil) and // if StateStyles in nil nothing to adjust
     (HasUnconstrainedAutosizeWidth or HasUnconstrainedAutosizeHeight) and // if AutoSize is false nothing to adjust
     (TNonReentrantHelper.EnterSection(FIsAdjustingSize)) then begin // non-reantrant
    try

      if isupdating then begin
        FAdjustSizeOnEndUpdate := True;
        Exit;
      end
      else
        FAdjustSizeOnEndUpdate := False;

      var LSubIndexOffset: Integer;
      var LDefaultStateStyle: TBaseStateStyle;
      if Checked then begin
        LSubIndexOffset := GetCacheSubIndex{+0};
        LDefaultStateStyle := StateStyles.Checked.Default;
      end
      else begin
        LSubIndexOffset := GetCacheSubIndex+5;
        LDefaultStateStyle := StateStyles.UnChecked.Default;
      end;
      LDefaultStateStyle.SupersedeNoChanges(true{ASaveState});
      try

        // if Text is empty do not do autosize
        if LDefaultStateStyle.Text = '' then exit;

        {$IF defined(debug)}
        //ALLog(ClassName + '.AdjustSize', 'Name: ' + Name + ' | HasUnconstrainedAutosize(X/Y) : '+ALBoolToStrW(HasUnconstrainedAutosizeWidth)+'/'+ALBoolToStrW(HasUnconstrainedAutosizeHeight));
        {$ENDIF}

        var R: TrectF;
        If {$IF not DEFINED(ALDPK)}DoubleBuffered{$ELSE}True{$ENDIF} then begin
          if (CacheIndex <= 0) or
             (CacheEngine = nil) or
             (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex}, R{ARect})) then begin
            MakeBufDrawable;
            R := LDefaultStateStyle.FBufDrawableRect;
          end;
        end
        else begin
          {$IF not DEFINED(ALDPK)}
          var LTextBroken: Boolean;
          var LAllTextDrawn: Boolean;
          var LElements: TALTextElements;
          MeasureMultilineText(
            R, // out ARect: TRectF;
            LTextBroken, // out ATextBroken: Boolean;
            LAllTextDrawn, // out AAllTextDrawn: Boolean;
            LElements, // out AElements: TALTextElements;
            1, // const AScale: Single;
            LDefaultStateStyle.Text, // const AText: String;
            LDefaultStateStyle.TextSettings.Font, // const AFont: TALFont;
            LDefaultStateStyle.TextSettings.Decoration, // const ADecoration: TALTextDecoration;
            LDefaultStateStyle.TextSettings.EllipsisSettings.font, // const AEllipsisFont: TALFont;
            LDefaultStateStyle.TextSettings.EllipsisSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
            LDefaultStateStyle.Fill, // const AFill: TALBrush;
            nil, // const AStateLayer: TALStateLayer;
            LDefaultStateStyle.Stroke, // const AStroke: TALStrokeBrush;
            LDefaultStateStyle.Shadow, // const AShadow: TALShadow);
            LDefaultStateStyle.XRadius, // const AXRadius: Single;
            LDefaultStateStyle.YRadius); // const AYRadius: Single
          {$ENDIF}
        end;

        if not HasUnconstrainedAutosizeWidth then begin
          r.Left := 0;
          r.Width := Width;
        end;
        if not HasUnconstrainedAutosizeHeight then begin
          r.Top := 0;
          r.height := height;
        end;

        SetFixedSizeBounds(Position.X, Position.Y, R.Width, R.Height);

      finally
        LDefaultStateStyle.RestorestateNoChanges;
      end;

    finally
      TNonReentrantHelper.LeaveSection(FIsAdjustingSize)
    end;
  end;
end;

{****************************************}
procedure TALToggleButton.MakeBufDrawable;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _DoMakeBufDrawable(const AStateStyle: TBaseStateStyle): boolean;
  begin
    if (not ALIsDrawableNull(AStateStyle.FBufDrawable)) then exit(False);
    AStateStyle.SupersedeNoChanges(true{ASaveState});
    try

      {$IFDEF debug}
      ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Style: ' + AStateStyle.ClassName + ' | Width: ' + ALFloatToStrW(Width)+ ' | Height: ' + ALFloatToStrW(Height));
      {$endif}

      // Create the BufDrawable
      var LTextBroken: Boolean;
      var LAllTextDrawn: Boolean;
      var LElements: TALTextElements;
      CreateBufDrawable(
        AStateStyle.FBufDrawable, // var ABufDrawable: TALDrawable;
        AStateStyle.FBufDrawableRect, // var ABufDrawableRect: TRectF;
        LTextBroken, // var ABufTextBroken: Boolean;
        LAllTextDrawn, // var ABufAllTextDrawn: Boolean;
        LElements, // var ABufElements: TALTextElements;
        ALGetScreenScale * AStateStyle.Scale, // const AScale: Single;
        AStateStyle.Text, // const AText: String;
        AStateStyle.TextSettings.Font, // const AFont: TALFont;
        AStateStyle.TextSettings.Decoration, // const ADecoration: TALTextDecoration;
        AStateStyle.TextSettings.Font, // const AEllipsisFont: TALFont;
        AStateStyle.TextSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
        AStateStyle.Fill, // const AFill: TALBrush;
        AStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
        AStateStyle.Stroke, // const AStroke: TALStrokeBrush;
        AStateStyle.Shadow, // const AShadow: TALShadow);
        AStateStyle.XRadius, // const AXRadius: Single;
        AStateStyle.YRadius); // const AYRadius: Single

      // LStateStyle.FBufDrawableRect must include the LScale
      AStateStyle.FBufDrawableRect.Top := AStateStyle.FBufDrawableRect.Top * AStateStyle.Scale;
      AStateStyle.FBufDrawableRect.right := AStateStyle.FBufDrawableRect.right * AStateStyle.Scale;
      AStateStyle.FBufDrawableRect.left := AStateStyle.FBufDrawableRect.left * AStateStyle.Scale;
      AStateStyle.FBufDrawableRect.bottom := AStateStyle.FBufDrawableRect.bottom * AStateStyle.Scale;

      // Since LStateStyle.FBufDrawableRect can have different dimensions than the main BufDrawableRect
      // (due to autosizing with different font sizes), we must center LStateStyle.FBufDrawableRect
      // within the main BufDrawableRect to ensure that all changes are visually centered.
      If Checked then begin
        if AStateStyle <> StateStyles.Checked.Default then begin
          var LMainDrawableRect: TRectF;
          if (CacheIndex <= 0) or
             (CacheEngine = nil) or
             (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{+0}+StateStyles.Checked.Default.GetCacheSubIndex{ASubIndex}, LMainDrawableRect{ARect})) then begin
            If AlIsDrawableNull(StateStyles.Checked.Default.FBufDrawable) then LMainDrawableRect := LocalRect
            else LMainDrawableRect := StateStyles.Checked.Default.FBufDrawableRect;
          end;
          LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
          var LCenteredRect := AStateStyle.FBufDrawableRect.CenterAt(LMainDrawableRect);
          AStateStyle.FBufDrawableRect.Offset(LCenteredRect.Left, LCenteredRect.top);
        end;
      end
      else begin
        if AStateStyle <> StateStyles.Unchecked.Default then begin
          var LMainDrawableRect: TRectF;
          if (CacheIndex <= 0) or
             (CacheEngine = nil) or
             (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex+5+StateStyles.Unchecked.Default.GetCacheSubIndex{ASubIndex}, LMainDrawableRect{ARect})) then begin
            If AlIsDrawableNull(StateStyles.Unchecked.Default.FBufDrawable) then LMainDrawableRect := LocalRect
            else LMainDrawableRect := StateStyles.Unchecked.Default.FBufDrawableRect;
          end;
          LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
          var LCenteredRect := AStateStyle.FBufDrawableRect.CenterAt(LMainDrawableRect);
          AStateStyle.FBufDrawableRect.Offset(LCenteredRect.Left, LCenteredRect.top);
        end;
      end;

    finally
      AStateStyle.RestorestateNoChanges;
    end;
    Result := True;
  end;

begin
  //--- Do not create BufDrawable if not DoubleBuffered
  if {$IF not DEFINED(ALDPK)}(not DoubleBuffered){$ELSE}False{$ENDIF} then begin
    ClearBufDrawable;
    exit;
  end;
  //--
  var LSubIndexOffset: Integer;
  var LDefaultStateStyle: TBaseStateStyle;
  if Checked then begin
    LSubIndexOffset := GetCacheSubIndex{+0};
    LDefaultStateStyle := StateStyles.Checked.Default;
  end
  else begin
    LSubIndexOffset := GetCacheSubIndex+5;
    LDefaultStateStyle := StateStyles.UnChecked.Default;
  end;
  //--
  if (CacheIndex = 0) or
     (CacheEngine = nil) or
     (not CacheEngine.HasEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex})) then
    _DoMakeBufDrawable(LDefaultStateStyle);
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if LStateStyle.Inherit then exit;
  if (CacheIndex > 0) and
     (CacheEngine <> nil) and
     (CacheEngine.HasEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex})) then Exit;
  _DoMakeBufDrawable(LStateStyle);
  // No need to center LStateStyle.FBufDrawableRect on the main BufDrawableRect
  // because BufDrawableRect always has the width and height of the localRect.
end;

{********************************************************************************************************************************************************************}
Procedure TALToggleButton.DrawMultilineTextAdjustRect(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; var ARect: TrectF; var ASurfaceSize: TSizeF);
begin

  // If we are drawing directly on the form, center ARect in LocalRect. This is necessary if, for example,
  // the 'to' font size is smaller than the 'from' font size.
  {$IF defined(ALSkiaCanvas)}
  If (Canvas <> nil) and (TSkCanvasCustom(Canvas).Canvas <> nil) and (TSkCanvasCustom(Canvas).Canvas.Handle = ACanvas) then
    // ALAlignToPixelRound is used because when we call ALDrawDrawable,
    // we do LDstRect := AALAlignToPixelRound(LDstRect).
    // Therefore, when drawing directly on the canvas,
    // we must draw at the exact same position as when we call ALDrawDrawable.
    ARect := ALAlignToPixelRound(ARect.CenterAt(LocalRect), Canvas.Matrix, Canvas.Scale, TEpsilon.position)
  else
  {$ENDIF}

end;

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
function TALToggleButton.GetRenderTargetRect(const ARect: TrectF): TRectF;
begin
  if StateStyles.Transition.Running then begin
    Result := ARect;
    if StateStyles.Transition.FromStateStyle <> nil then begin
      var LFromSurfaceRect := ALGetShapeSurfaceRect(
                                ARect, // const ARect: TRectF;
                                AutoAlignToPixel, // const AAlignToPixel: Boolean;
                                _TALBaseStateStyleProtectedAccess(StateStyles.Transition.FromStateStyle).Fill, // const AFill: TALBrush;
                                nil, // const AFillResourceStream: TStream;
                                _TALBaseStateStyleProtectedAccess(StateStyles.Transition.FromStateStyle).StateLayer, // const AStateLayer: TALStateLayer;
                                _TALBaseStateStyleProtectedAccess(StateStyles.Transition.FromStateStyle).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LFromSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
    if StateStyles.Transition.ToStateStyle <> nil then begin
      var LToSurfaceRect := ALGetShapeSurfaceRect(
                              ARect, // const ARect: TRectF;
                              AutoAlignToPixel, // const AAlignToPixel: Boolean;
                              _TALBaseStateStyleProtectedAccess(StateStyles.Transition.ToStateStyle).Fill, // const AFill: TALBrush;
                              nil, // const AFillResourceStream: TStream;
                              _TALBaseStateStyleProtectedAccess(StateStyles.Transition.ToStateStyle).StateLayer, // const AStateLayer: TALStateLayer;
                              _TALBaseStateStyleProtectedAccess(StateStyles.Transition.ToStateStyle).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LToSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
  end
  else begin
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  AutoAlignToPixel, // const AAlignToPixel: Boolean;
                  LStateStyle.Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  LStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
                  LStateStyle.Shadow); // const AShadow: TALShadow): TRectF;
    end
    else begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  AutoAlignToPixel, // const AAlignToPixel: Boolean;
                  Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  nil, // const AStateLayer: TALStateLayer;
                  Shadow); // const AShadow: TALShadow): TRectF;
    end;
  end;
end;
{$ENDIF}

{******************************}
procedure TALToggleButton.Paint;
begin

  StateStyles.UpdateLastPaintedRawStyle;

  var LDrawable: TALDrawable := ALNullDrawable;
  var LDrawableRect: TRectF := TRectF.Empty;
  if not StateStyles.Transition.Running then begin
    //--
    var LSubIndexOffset: Integer;
    var LDefaultStateStyle: TBaseStateStyle;
    if Checked then begin
      LSubIndexOffset := GetCacheSubIndex{+0};
      LDefaultStateStyle := StateStyles.Checked.Default;
    end
    else begin
      LSubIndexOffset := GetCacheSubIndex+5;
      LDefaultStateStyle := StateStyles.UnChecked.Default;
    end;
    //--
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      if (CacheIndex <= 0) or
         (CacheEngine = nil) or
         (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
        MakeBufDrawable;
        if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(LStateStyle.FBufDrawable)) then begin
          if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex}, LStateStyle.FBufDrawable{ADrawable}, LStateStyle.FBufDrawableRect{ARect}) then ALFreeAndNilDrawable(LStateStyle.FBufDrawable)
          else LStateStyle.FBufDrawable := ALNullDrawable;
          if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
            raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
        end
        else begin
          LDrawable := LStateStyle.FBufDrawable;
          LDrawableRect := LStateStyle.FBufDrawableRect;
        end;
      end;
    end;
    //--
    If ALIsDrawableNull(LDrawable) then begin
      if (CacheIndex <= 0) or
         (CacheEngine = nil) or
         (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
        if LStateStyle = nil then MakeBufDrawable;
        if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(LDefaultStateStyle.fBufDrawable)) then begin
          if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex}, LDefaultStateStyle.fBufDrawable{ADrawable}, LDefaultStateStyle.fBufDrawableRect{ARect}) then ALFreeAndNilDrawable(LDefaultStateStyle.fBufDrawable)
          else LDefaultStateStyle.fBufDrawable := ALNullDrawable;
          if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
            raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
        end
        else begin
          LDrawable := LDefaultStateStyle.FBufDrawable;
          LDrawableRect := LDefaultStateStyle.FBufDrawableRect;
        end;
      end;
    end;
    //--
  end;

  if ALIsDrawableNull(LDrawable) then begin

    var LCurrentAdjustedStateStyle := TBaseStateStyle(StateStyles.GetCurrentAdjustedStyle);
    if LCurrentAdjustedStateStyle = nil then begin
      inherited Paint;
      exit;
    end;

    {$IF DEFINED(ALSkiaCanvas)}

    // Using a matrix on the canvas results in smoother animations compared to using
    // Ascale with DrawMultilineText. This is because changes in scale affect the font size,
    // leading to rounding issues (I spent many hours looking for a way to avoid this).
    // If there is an animation, it appears jerky because the text position
    // shifts up or down with scale changes due to pixel alignment.
    var LCanvasSaveState: TCanvasSaveState := ALScaleAndCenterCanvas(
                                                Canvas, // Const ACanvas: TCanvas;
                                                AbsoluteRect, // Const AAbsoluteRect: TRectF;
                                                LCurrentAdjustedStateStyle.Scale, // Const AScale: Single;
                                                true); // Const ASaveState: Boolean);
    try

      var LRect := LocalRect;
      var LTextBroken: Boolean;
      var LAllTextDrawn: Boolean;
      var LElements: TALTextElements;

      if (StateStyles.Transition.Running) and
         (StateStyles.Transition.FadeImage) and
         (LCurrentAdjustedStateStyle.Text = '') and
         (StateStyles.Transition.FromStateStyle <> nil) and
         (TBaseStateStyle(StateStyles.Transition.FromStateStyle).fill.ResourceName <> '') and
         (StateStyles.Transition.ToStateStyle <> nil) and
         (TBaseStateStyle(StateStyles.Transition.ToStateStyle).fill.ResourceName <> '') and
         (TBaseStateStyle(StateStyles.Transition.FromStateStyle).fill.ResourceName <> TBaseStateStyle(StateStyles.Transition.ToStateStyle).fill.ResourceName) then begin

        LCurrentAdjustedStateStyle.BeginUpdate;
        try

          LCurrentAdjustedStateStyle.Fill.ResourceName := TBaseStateStyle(StateStyles.Transition.FromStateStyle).fill.ResourceName;
          LCurrentAdjustedStateStyle.Fill.ImageTintColor := TBaseStateStyle(StateStyles.Transition.FromStateStyle).fill.ImageTintColor;

          DrawMultilineText(
            TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
            LRect, // var ARect: TRectF;
            LTextBroken, // out ATextBroken: Boolean;
            LAllTextDrawn, // out AAllTextDrawn: Boolean;
            LElements, // out AElements: TALTextElements;
            1{Ascale},
            1-StateStyles.Transition.CurrentValue, // const AOpacity: Single;
            '', // const AText: String;
            nil, // const AFont: TALFont;
            nil, // const ADecoration: TALTextDecoration;
            nil, // const AEllipsisFont: TALFont;
            nil, // const AEllipsisDecoration: TALTextDecoration;
            LCurrentAdjustedStateStyle.Fill, // const AFill: TALBrush;
            LCurrentAdjustedStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
            LCurrentAdjustedStateStyle.Stroke, // const AStroke: TALStrokeBrush;
            LCurrentAdjustedStateStyle.Shadow, // const AShadow: TALShadow);
            LCurrentAdjustedStateStyle.XRadius, // const AXRadius: Single;
            LCurrentAdjustedStateStyle.YRadius); // const AYRadius: Single

          LCurrentAdjustedStateStyle.Fill.ResourceName := TBaseStateStyle(StateStyles.Transition.ToStateStyle).fill.ResourceName;
          LCurrentAdjustedStateStyle.Fill.ImageTintColor := TBaseStateStyle(StateStyles.Transition.ToStateStyle).fill.ImageTintColor;

          DrawMultilineText(
            TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
            LRect, // var ARect: TRectF;
            LTextBroken, // out ATextBroken: Boolean;
            LAllTextDrawn, // out AAllTextDrawn: Boolean;
            LElements, // out AElements: TALTextElements;
            1{Ascale},
            StateStyles.Transition.CurrentValue, // const AOpacity: Single;
            '', // const AText: String;
            nil, // const AFont: TALFont;
            nil, // const ADecoration: TALTextDecoration;
            nil, // const AEllipsisFont: TALFont;
            nil, // const AEllipsisDecoration: TALTextDecoration;
            LCurrentAdjustedStateStyle.Fill, // const AFill: TALBrush;
            LCurrentAdjustedStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
            LCurrentAdjustedStateStyle.Stroke, // const AStroke: TALStrokeBrush;
            LCurrentAdjustedStateStyle.Shadow, // const AShadow: TALShadow);
            LCurrentAdjustedStateStyle.XRadius, // const AXRadius: Single;
            LCurrentAdjustedStateStyle.YRadius); // const AYRadius: Single

        finally
          LCurrentAdjustedStateStyle.EndUpdateNoChanges;
        end;

      end
      else begin

        DrawMultilineText(
          TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
          LRect, // var ARect: TRectF;
          LTextBroken, // out ATextBroken: Boolean;
          LAllTextDrawn, // out AAllTextDrawn: Boolean;
          LElements, // out AElements: TALTextElements;
          1{Ascale},
          AbsoluteOpacity, // const AOpacity: Single;
          LCurrentAdjustedStateStyle.Text, // const AText: String;
          LCurrentAdjustedStateStyle.TextSettings.Font, // const AFont: TALFont;
          LCurrentAdjustedStateStyle.TextSettings.Decoration, // const ADecoration: TALTextDecoration;
          LCurrentAdjustedStateStyle.TextSettings.EllipsisSettings.font, // const AEllipsisFont: TALFont;
          LCurrentAdjustedStateStyle.TextSettings.EllipsisSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
          LCurrentAdjustedStateStyle.Fill, // const AFill: TALBrush;
          LCurrentAdjustedStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
          LCurrentAdjustedStateStyle.Stroke, // const AStroke: TALStrokeBrush;
          LCurrentAdjustedStateStyle.Shadow, // const AShadow: TALShadow);
          LCurrentAdjustedStateStyle.XRadius, // const AXRadius: Single;
          LCurrentAdjustedStateStyle.YRadius); // const AYRadius: Single

      end;

    finally
      if LCanvasSaveState <> nil then
        Canvas.RestoreState(LCanvasSaveState);
    end;

    {$ELSE}

    var LRect := LocalRect;
    InitRenderTargets(LRect);
    if ALCanvasBeginScene(RenderTargetCanvas) then
    try

      ALClearCanvas(RenderTargetCanvas, TAlphaColors.Null);

      var LTextBroken: Boolean;
      var LAllTextDrawn: Boolean;
      var LElements: TALTextElements;

      if (StateStyles.Transition.Running) and
         (StateStyles.Transition.FadeImage) and
         (LCurrentAdjustedStateStyle.Text = '') and
         (StateStyles.Transition.FromStateStyle <> nil) and
         (TBaseStateStyle(StateStyles.Transition.FromStateStyle).fill.ResourceName <> '') and
         (StateStyles.Transition.ToStateStyle <> nil) and
         (TBaseStateStyle(StateStyles.Transition.ToStateStyle).fill.ResourceName <> '') and
         (TBaseStateStyle(StateStyles.Transition.FromStateStyle).fill.ResourceName <> TBaseStateStyle(StateStyles.Transition.ToStateStyle).fill.ResourceName) then begin

        LCurrentAdjustedStateStyle.BeginUpdate;
        try

          LCurrentAdjustedStateStyle.Fill.ResourceName := TBaseStateStyle(StateStyles.Transition.FromStateStyle).fill.ResourceName;
          LCurrentAdjustedStateStyle.Fill.ImageTintColor := TBaseStateStyle(StateStyles.Transition.FromStateStyle).fill.ImageTintColor;

          DrawMultilineText(
            RenderTargetCanvas, // const ACanvas: TALCanvas;
            LRect, // var ARect: TRectF;
            LTextBroken, // out ATextBroken: Boolean;
            LAllTextDrawn, // out AAllTextDrawn: Boolean;
            LElements, // out AElements: TALTextElements;
            ALGetScreenScale{Ascale},
            1-StateStyles.Transition.CurrentValue, // const AOpacity: Single;
            '', // const AText: String;
            nil, // const AFont: TALFont;
            nil, // const ADecoration: TALTextDecoration;
            nil, // const AEllipsisFont: TALFont;
            nil, // const AEllipsisDecoration: TALTextDecoration;
            LCurrentAdjustedStateStyle.Fill, // const AFill: TALBrush;
            LCurrentAdjustedStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
            LCurrentAdjustedStateStyle.Stroke, // const AStroke: TALStrokeBrush;
            LCurrentAdjustedStateStyle.Shadow, // const AShadow: TALShadow);
            LCurrentAdjustedStateStyle.XRadius, // const AXRadius: Single;
            LCurrentAdjustedStateStyle.YRadius); // const AYRadius: Single

          LCurrentAdjustedStateStyle.Fill.ResourceName := TBaseStateStyle(StateStyles.Transition.ToStateStyle).fill.ResourceName;
          LCurrentAdjustedStateStyle.Fill.ImageTintColor := TBaseStateStyle(StateStyles.Transition.ToStateStyle).fill.ImageTintColor;

          DrawMultilineText(
            RenderTargetCanvas, // const ACanvas: TALCanvas;
            LRect, // var ARect: TRectF;
            LTextBroken, // out ATextBroken: Boolean;
            LAllTextDrawn, // out AAllTextDrawn: Boolean;
            LElements, // out AElements: TALTextElements;
            ALGetScreenScale{Ascale},
            StateStyles.Transition.CurrentValue, // const AOpacity: Single;
            '', // const AText: String;
            nil, // const AFont: TALFont;
            nil, // const ADecoration: TALTextDecoration;
            nil, // const AEllipsisFont: TALFont;
            nil, // const AEllipsisDecoration: TALTextDecoration;
            LCurrentAdjustedStateStyle.Fill, // const AFill: TALBrush;
            LCurrentAdjustedStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
            LCurrentAdjustedStateStyle.Stroke, // const AStroke: TALStrokeBrush;
            LCurrentAdjustedStateStyle.Shadow, // const AShadow: TALShadow);
            LCurrentAdjustedStateStyle.XRadius, // const AXRadius: Single;
            LCurrentAdjustedStateStyle.YRadius); // const AYRadius: Single

        finally
          LCurrentAdjustedStateStyle.EndUpdateNoChanges;
        end;

      end
      else begin

        DrawMultilineText(
          RenderTargetCanvas, // const ACanvas: TALCanvas;
          LRect, // out ARect: TRectF;
          LTextBroken, // out ATextBroken: Boolean;
          LAllTextDrawn, // out AAllTextDrawn: Boolean;
          LElements, // out AElements: TALTextElements;
          ALGetScreenScale{Ascale},
          1, // const AOpacity: Single;
          LCurrentAdjustedStateStyle.Text, // const AText: String;
          LCurrentAdjustedStateStyle.TextSettings.Font, // const AFont: TALFont;
          LCurrentAdjustedStateStyle.TextSettings.Decoration, // const ADecoration: TALTextDecoration;
          LCurrentAdjustedStateStyle.TextSettings.EllipsisSettings.font, // const AEllipsisFont: TALFont;
          LCurrentAdjustedStateStyle.TextSettings.EllipsisSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
          LCurrentAdjustedStateStyle.Fill, // const AFill: TALBrush;
          LCurrentAdjustedStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
          LCurrentAdjustedStateStyle.Stroke, // const AStroke: TALStrokeBrush;
          LCurrentAdjustedStateStyle.Shadow, // const AShadow: TALShadow;
          LCurrentAdjustedStateStyle.XRadius, // const AXRadius: Single;
          LCurrentAdjustedStateStyle.YRadius); // const AYRadius: Single

      end;

    finally
      ALCanvasEndScene(RenderTargetCanvas)
    end;

    ALUpdateDrawableFromSurface(RenderTargetSurface, RenderTargetDrawable);

    // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
    // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
    LRect.Offset(-2*LRect.Left, -2*LRect.Top);

    // LRect must include the LScale
    LRect.Top := LRect.Top * LCurrentAdjustedStateStyle.Scale;
    LRect.right := LRect.right * LCurrentAdjustedStateStyle.Scale;
    LRect.left := LRect.left * LCurrentAdjustedStateStyle.Scale;
    LRect.bottom := LRect.bottom * LCurrentAdjustedStateStyle.Scale;

    // Since LStateStyle.FBufDrawableRect can have different dimensions than the main BufDrawableRect
    // (due to autosizing with different font sizes), we must center LStateStyle.FBufDrawableRect
    // within the main BufDrawableRect to ensure that all changes are visually centered.
    If Checked then begin
      var LMainDrawableRect: TRectF;
      if (CacheIndex <= 0) or
         (CacheEngine = nil) or
         (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{+0}+StateStyles.Checked.Default.GetCacheSubIndex{ASubIndex}, LMainDrawableRect{ARect})) then begin
        If AlIsDrawableNull(StateStyles.Checked.Default.FBufDrawable) then LMainDrawableRect := LocalRect
        else LMainDrawableRect := StateStyles.Checked.Default.FBufDrawableRect;
      end;
      LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
      var LCenteredRect := LRect.CenterAt(LMainDrawableRect);
      LRect.Offset(LCenteredRect.Left, LCenteredRect.top);
    end
    else begin
      var LMainDrawableRect: TRectF;
      if (CacheIndex <= 0) or
         (CacheEngine = nil) or
         (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex+5+StateStyles.Unchecked.Default.GetCacheSubIndex{ASubIndex}, LMainDrawableRect{ARect})) then begin
        If AlIsDrawableNull(StateStyles.Unchecked.Default.FBufDrawable) then LMainDrawableRect := LocalRect
        else LMainDrawableRect := StateStyles.Unchecked.Default.FBufDrawableRect;
      end;
      LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
      var LCenteredRect := LRect.CenterAt(LMainDrawableRect);
      LRect.Offset(LCenteredRect.Left, LCenteredRect.top);
    end;

    // We cannot use the matrix because, if we do, ALAlignToPixelRound in ALDrawDrawable
    // will be ineffective since the matrix will no longer be a simple translation matrix.
    // In such a case, TCustomCanvasGpu(ACanvas).DrawTexture may produce border artifacts
    // if the texture is not perfectly pixel-aligned.
    var LDstRect := TRectF.Create(0, 0, ALGetDrawableWidth(RenderTargetDrawable), ALGetDrawableHeight(RenderTargetDrawable));
    LDstRect.Width := (LDstRect.Width / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
    LDstRect.height := (LDstRect.height / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
    LDstRect.SetLocation(
      LRect.Left,
      LRect.Top);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      RenderTargetDrawable, // const ADrawable: TALDrawable;
      LDstRect, // const ADstRect: TrectF; // IN Virtual pixels !
      AbsoluteOpacity); // const AOpacity: Single)

    {$ENDIF}

    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{*****************}
procedure Register;
begin
  RegisterComponents(
    'Alcinoe',
    [TALAniIndicator, TALScrollBar, TALTrackBar,
     TALRangeTrackBar, TALCheckBox, TALRadioButton,
     TALSwitch, TALButton, TALToggleButton]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALAniIndicator, 'Size');
  UnlistPublishedProperty(TALAniIndicator, 'StyleName');
  UnlistPublishedProperty(TALAniIndicator, 'OnTap');
  //--
  UnlistPublishedProperty(TALCustomTrack.TThumb, 'Size');
  UnlistPublishedProperty(TALCustomTrack.TThumb, 'StyleName');
  UnlistPublishedProperty(TALCustomTrack.TThumb, 'OnTap');
  UnlistPublishedProperty(TALCustomTrack.TThumb, 'Tag');
  //--
  UnlistPublishedProperty(TALCustomTrack.TInactiveTrack, 'Size');
  UnlistPublishedProperty(TALCustomTrack.TInactiveTrack, 'StyleName');
  UnlistPublishedProperty(TALCustomTrack.TInactiveTrack, 'OnTap');
  UnlistPublishedProperty(TALCustomTrack.TInactiveTrack, 'Tag');
  UnlistPublishedProperty(TALCustomTrack.TInactiveTrack, 'Touch');
  UnlistPublishedProperty(TALCustomTrack.TInactiveTrack, 'OnGesture');
  //--
  UnlistPublishedProperty(TALCustomTrack.TActiveTrack, 'Size');
  UnlistPublishedProperty(TALCustomTrack.TActiveTrack, 'StyleName');
  UnlistPublishedProperty(TALCustomTrack.TActiveTrack, 'OnTap');
  UnlistPublishedProperty(TALCustomTrack.TActiveTrack, 'Tag');
  UnlistPublishedProperty(TALCustomTrack.TActiveTrack, 'Touch');
  UnlistPublishedProperty(TALCustomTrack.TActiveTrack, 'OnGesture');
  //--
  UnlistPublishedProperty(TALCustomTrack.TValueIndicator, 'Size');
  UnlistPublishedProperty(TALCustomTrack.TValueIndicator, 'StyleName');
  UnlistPublishedProperty(TALCustomTrack.TValueIndicator, 'OnTap');
  UnlistPublishedProperty(TALCustomTrack.TValueIndicator, 'Tag');
  UnlistPublishedProperty(TALCustomTrack.TValueIndicator, 'Touch');
  UnlistPublishedProperty(TALCustomTrack.TValueIndicator, 'OnGesture');
  //--
  UnlistPublishedProperty(TALScrollBar, 'Size');
  UnlistPublishedProperty(TALScrollBar, 'StyleName');
  UnlistPublishedProperty(TALScrollBar, 'OnTap');
  //--
  UnlistPublishedProperty(TALTrackBar, 'Size');
  UnlistPublishedProperty(TALTrackBar, 'StyleName');
  UnlistPublishedProperty(TALTrackBar, 'OnTap');
  //--
  UnlistPublishedProperty(TALRangeTrackBar, 'Size');
  UnlistPublishedProperty(TALRangeTrackBar, 'StyleName');
  UnlistPublishedProperty(TALRangeTrackBar, 'OnTap');
  //--
  UnlistPublishedProperty(TALCheckBox, 'Size');
  UnlistPublishedProperty(TALCheckBox, 'StyleName');
  UnlistPublishedProperty(TALCheckBox, 'OnTap');
  //--
  UnlistPublishedProperty(TALRadioButton, 'Size');
  UnlistPublishedProperty(TALRadioButton, 'StyleName');
  UnlistPublishedProperty(TALRadioButton, 'OnTap');
  //--
  UnlistPublishedProperty(TALSwitch, 'Size');
  UnlistPublishedProperty(TALSwitch, 'StyleName');
  UnlistPublishedProperty(TALSwitch, 'OnTap');
  //--
  UnlistPublishedProperty(TALButton, 'Size');
  UnlistPublishedProperty(TALButton, 'StyleName');
  UnlistPublishedProperty(TALButton, 'OnTap');
  //--
  UnlistPublishedProperty(TALToggleButton, 'Size');
  UnlistPublishedProperty(TALToggleButton, 'StyleName');
  UnlistPublishedProperty(TALToggleButton, 'OnTap');
  //--
  UnlistPublishedProperty(TALSwitch.TThumb, 'Size');
  UnlistPublishedProperty(TALSwitch.TThumb, 'StyleName');
  UnlistPublishedProperty(TALSwitch.TThumb, 'OnTap');
  UnlistPublishedProperty(TALSwitch.TThumb, 'Tag');
  UnlistPublishedProperty(TALSwitch.TThumb, 'Touch');
  UnlistPublishedProperty(TALSwitch.TThumb, 'OnGesture');
  //--
  UnlistPublishedProperty(TALSwitch.TTrack, 'Size');
  UnlistPublishedProperty(TALSwitch.TTrack, 'StyleName');
  UnlistPublishedProperty(TALSwitch.TTrack, 'OnTap');
  UnlistPublishedProperty(TALSwitch.TTrack, 'Tag');
  UnlistPublishedProperty(TALSwitch.TTrack, 'Touch');
  UnlistPublishedProperty(TALSwitch.TTrack, 'OnGesture');
  {$ENDIF}
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.StdCtrls','initialization');
  {$ENDIF}
  TALCustomTrack.TValueIndicator.Format0 := '0';
  RegisterFmxClasses(
    [TALAniIndicator, TALScrollBar, TALTrackBar,
     TALRangeTrackBar, TALCheckBox, TALRadioButton,
     TALSwitch, TALButton, TALToggleButton]);

end.
