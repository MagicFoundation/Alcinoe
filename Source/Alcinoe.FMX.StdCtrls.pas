unit Alcinoe.FMX.StdCtrls;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported122}
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
  TALAniIndicator = class(TALControl, IALDoubleBufferedControl)
  private
    fTimer: TTimer;
    finterval: integer;
    FFrameCount: Integer;
    FRowCount: Integer;
    fResourceName: String;
    fFrameIndex: TSmallPoint;
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    procedure setResourceName(const Value: String);
    procedure onTimer(sender: Tobject);
    function ResourceNameStored: Boolean;
  protected
    function GetDoubleBuffered: boolean;
    procedure SetDoubleBuffered(const AValue: Boolean);
    procedure Paint; override;
    property BufDrawable: TALDrawable read fBufDrawable;
    property BufDrawableRect: TRectF read fBufDrawableRect;
    function GetDefaultSize: TSizeF; override;
    procedure DoResized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MakeBufDrawable; virtual;
    procedure clearBufDrawable; virtual;
  published
    //property Action;
    property Align;
    property Anchors;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    //property ClipChildren;
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
    property ResourceName: String read fResourceName write setResourceName stored ResourceNameStored nodefault;
    property FrameCount: Integer read FFrameCount write FFrameCount default 20;
    property RowCount: Integer read FRowCount write FRowCount default 4;
    property Interval: integer read finterval write finterval default 50;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    //property TabOrder;
    //property TabStop;
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
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBaseCheckBox = class(TALShape, IALDoubleBufferedControl)
  public
    type
      // ---------------
      // TCheckMarkBrush
      TCheckMarkBrush = class(TALPersistentObserver)
      private
        FColor: TAlphaColor;
        FResourceName: String;
        FWrapMode: TALImageWrapMode;
        FThickness: Single;
        FMargins: TBounds;
        FDefaultColor: TAlphaColor;
        FDefaultResourceName: String;
        FDefaultWrapMode: TALImageWrapMode;
        FDefaultThickness: Single;
        procedure SetColor(const Value: TAlphaColor);
        procedure SetResourceName(const Value: String);
        procedure SetWrapMode(const Value: TALImageWrapMode);
        procedure SetThickness(const Value: Single);
        procedure SetMargins(const Value: TBounds);
        procedure MarginsChanged(Sender: TObject); virtual;
        function IsColorStored: Boolean;
        function IsResourceNameStored: Boolean;
        function IsWrapModeStored: Boolean;
        function IsThicknessStored: Boolean;
      protected
        function CreateSavedState: TALPersistentObserver; override;
      public
        constructor Create(const ADefaultColor: TAlphaColor); reintroduce; virtual;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; virtual;
        procedure Interpolate(const ATo: TCheckMarkBrush; const ANormalizedTime: Single); virtual;
        procedure InterpolateNoChanges(const ATo: TCheckMarkBrush; const ANormalizedTime: Single);
        function HasCheckMark: boolean;
        property DefaultColor: TAlphaColor read FDefaultColor write FDefaultColor;
        property DefaultResourceName: String read FDefaultResourceName write FDefaultResourceName;
        property DefaultWrapMode: TALImageWrapMode read FDefaultWrapMode write FDefaultWrapMode;
        property DefaultThickness: Single read FDefaultThickness write FDefaultThickness;
      published
        property Color: TAlphaColor read FColor write SetColor stored IsColorStored;
        property ResourceName: String read FResourceName write SetResourceName stored IsResourceNameStored nodefault;
        property WrapMode: TALImageWrapMode read FWrapMode write SetWrapMode stored IsWrapModeStored;
        property Thickness: Single read FThickness write SetThickness stored IsThicknessStored nodefault;
        property Margins: TBounds read FMargins write SetMargins;
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
        constructor Create(const AParent: TCheckMarkBrush; const ADefaultColor: TAlphaColor); reintroduce; virtual;
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
      private
        FCheckMark: TInheritCheckMarkBrush;
        function GetStateStyleParent: TBaseStateStyle;
        function GetControlParent: TALBaseCheckBox;
        procedure SetCheckMark(const AValue: TInheritCheckMarkBrush);
        procedure CheckMarkChanged(ASender: TObject);
      protected
        function GetInherit: Boolean; override;
        procedure DoSupersede; override;
      public
        constructor Create(const AParent: TObject); override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; override;
        procedure Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single); override;
        property StateStyleParent: TBaseStateStyle read GetStateStyleParent;
        property ControlParent: TALBaseCheckBox read GetControlParent;
      published
        property CheckMark: TInheritCheckMarkBrush read FCheckMark write SetCheckMark;
        property Fill;
        property Scale;
        property Shadow;
        property Stroke;
      end;
      // ------------------
      // TDefaultStateStyle
      TDefaultStateStyle = class(TBaseStateStyle);
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
      end;
      // ------------------
      // TPressedStateStyle
      TPressedStateStyle = class(TBaseStateStyle)
      published
        property StateLayer;
      end;
      // ------------------
      // TFocusedStateStyle
      TFocusedStateStyle = class(TBaseStateStyle)
      published
        property StateLayer;
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
      public
        constructor Create(const AParent: TALBaseCheckBox); reintroduce; virtual;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; virtual;
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
        function GetParent: TALBaseCheckBox;
        procedure SetChecked(const AValue: TCheckStateStyles);
        procedure SetUnchecked(const AValue: TCheckStateStyles);
        procedure CheckedChanged(ASender: TObject);
        procedure UncheckedChanged(ASender: TObject);
      protected
        function CreateSavedState: TALPersistentObserver; override;
      public
        constructor Create(const AParent: TALBaseCheckBox); reintroduce; virtual;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; override;
        procedure ClearBufDrawable; override;
        function GetCurrentRawStyle: TALBaseStateStyle; override;
        Property Parent: TALBaseCheckBox read GetParent;
      published
        property Checked: TCheckStateStyles read FChecked write SetChecked;
        property Unchecked: TCheckStateStyles read FUnchecked write SetUnchecked;
      end;
  private
    FStateStyles: TStateStyles;
    FCheckMark: TCheckMarkBrush;
    FChecked: Boolean;
    FDoubleBuffered: boolean;
    FXRadius: Single;
    FYRadius: Single;
    FOnChange: TNotifyEvent;
    FDefaultXRadius: Single;
    FDefaultYRadius: Single;
    procedure SetCheckMark(const Value: TCheckMarkBrush);
    procedure SetStateStyles(const AValue: TStateStyles);
    function IsXRadiusStored: Boolean;
    function IsYRadiusStored: Boolean;
  protected
    function CreateStateStyles: TStateStyles; virtual;
    function GetDoubleBuffered: boolean; virtual;
    procedure SetDoubleBuffered(const AValue: Boolean); virtual;
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
    procedure Click; override;
    procedure DoChanged; virtual;
    procedure DoResized; override;
    procedure DrawCheckMark(
                const ACanvas: TALCanvas;
                const AScale: Single;
                const ADstRect: TrectF;
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
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AlignToPixel; override;
    procedure MakeBufDrawable; virtual;
    procedure clearBufDrawable; virtual;
    property CanFocus default True;
    property Cursor default crHandPoint;
    property Checked: Boolean read GetChecked write SetChecked default False;
    property CheckMark: TCheckMarkBrush read FCheckMark write SetCheckMark;
    property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered default true;
    property StateStyles: TStateStyles read FStateStyles write SetStateStyles;
    property XRadius: Single read FXRadius write SetXRadius stored IsXRadiusStored nodefault;
    property YRadius: Single read FYRadius write SetYRadius stored IsYRadiusStored nodefault;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property DefaultXRadius: Single read FDefaultXRadius write FDefaultXRadius;
    property DefaultYRadius: Single read FDefaultYRadius write FDefaultYRadius;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALCheckBox = class(TALBaseCheckBox)
  public
    type
      // ------------
      // TStateStyles
      TCheckBoxStateStyles = class(TALBaseCheckBox.TStateStyles)
      published
        property Transition;
      end;
  private
    function GetStateStyles: TCheckBoxStateStyles;
    procedure SetStateStyles(const AValue: TCheckBoxStateStyles);
  protected
    function CreateStateStyles: TALBaseCheckBox.TStateStyles; override;
  published
    //property Action;
    property Align;
    property Anchors;
    property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property CheckMark;
    property Checked;
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
    property RotationCenter;
    property Scale;
    property Shadow;
    property Size;
    property StateStyles: TCheckBoxStateStyles read GetStateStyles write SetStateStyles;
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
    //property OnDblClick;
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
  private
    FGroupName: string;
    fMandatory: boolean;
    function GetGroupName: string;
    procedure SetGroupName(const Value: string);
    function GroupNameStored: Boolean;
    procedure GroupMessageCall(const Sender : TObject; const M : TMessage);
  protected
    procedure SetChecked(const Value: Boolean); override;
    function GetDefaultSize: TSizeF; override;
    procedure DrawCheckMark(
                const ACanvas: TALCanvas;
                const AScale: Single;
                const ADstRect: TrectF;
                const AChecked: Boolean;
                const ACheckMark: TALBaseCheckBox.TCheckMarkBrush); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property GroupName: string read GetGroupName write SetGroupName stored GroupNameStored nodefault;
    property Mandatory: Boolean read fMandatory write fMandatory default false;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALSwitch = class(TALControl, IALDoubleBufferedControl)
  public
    type
      // ------
      // TTrack
      TTrack = class(TALShape, IALDoubleBufferedControl)
      public
        type
          // ---------------
          // TBaseStateStyle
          TBaseStateStyle = class(TALBaseStateStyle)
          public
            constructor Create(const AParent: TObject); override;
          published
            property Fill;
            // When the track is scaled, the thumb is no longer aligned with the track.
            // Therefore, currently, scaling of the track is disabled.
            //property Scale;
            property Shadow;
            property Stroke;
          end;
          // ------------------
          // TDefaultStateStyle
          TDefaultStateStyle = class(TBaseStateStyle);
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
          end;
          // ------------------
          // TPressedStateStyle
          TPressedStateStyle = class(TBaseStateStyle)
          published
            property StateLayer;
          end;
          // ------------------
          // TFocusedStateStyle
          TFocusedStateStyle = class(TBaseStateStyle)
          published
            property StateLayer;
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
          public
            constructor Create(const AParent: TTrack); reintroduce; virtual;
            destructor Destroy; override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure AlignToPixel; virtual;
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
            function CreateSavedState: TALPersistentObserver; override;
          public
            constructor Create(const AParent: TTrack); reintroduce; virtual;
            destructor Destroy; override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure AlignToPixel; override;
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
        FDefaultXRadius: Single;
        FDefaultYRadius: Single;
        procedure SetStateStyles(const AValue: TStateStyles);
        function IsXRadiusStored: Boolean;
        function IsYRadiusStored: Boolean;
      protected
        function GetDefaultSize: TSizeF; override;
        function GetDoubleBuffered: boolean;
        procedure SetDoubleBuffered(const AValue: Boolean);
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
        procedure Paint; override;
        property Checked: Boolean read GetChecked write SetChecked default False;
        property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered default true;
      public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure AlignToPixel; override;
        procedure MakeBufDrawable; virtual;
        procedure clearBufDrawable; virtual;
        property DefaultXRadius: Single read FDefaultXRadius write FDefaultXRadius;
        property DefaultYRadius: Single read FDefaultYRadius write FDefaultYRadius;
        property Position stored false;
      published
        //property Action;
        //property Align;
        //property Anchors;
        //property CanFocus default False;
        //property CanParentFocus;
        //property DisableFocusEffect;
        //property ClipChildren;
        //property ClipParent;
        //property Cursor;
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
          // ------------
          // TStateStyles
          TThumbStateStyles = class(TALBaseCheckBox.TStateStyles)
          private
            FStartPositionX: Single;
          protected
            procedure StartTransition; override;
            procedure TransitionAnimationProcess(Sender: TObject); override;
            procedure TransitionAnimationFinish(Sender: TObject); override;
          end;
      protected
        function GetDefaultSize: TSizeF; override;
        function CreateStateStyles: TALBaseCheckBox.TStateStyles; override;
        procedure Click; override;
      public
        constructor Create(AOwner: TComponent); override;
        property Position stored false;
      published
        //property Action;
        //property Align;
        //property Anchors;
        //property CanFocus default False;
        //property CanParentFocus;
        //property DisableFocusEffect;
        property CheckMark;
        //property Checked;
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
  private
    FThumb: TThumb;
    FTrack: TTrack;
    FTransition: TALStateTransition;
    FPressedThumbPos: TPointF;
    FOnChange: TNotifyEvent;
    fScrollCapturedByMe: boolean;
    procedure ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
    procedure SetTransition(const Value: TALStateTransition);
    procedure TransitionChanged(ASender: TObject);
    function GetMinThumbPos: Single;
    function GetMaxThumbPos: Single;
    procedure AlignThumb;
  protected
    function GetDefaultSize: TSizeF; override;
    function GetDoubleBuffered: boolean;
    procedure SetDoubleBuffered(const AValue: Boolean);
    procedure StartTransition; virtual;
    procedure IsMouseOverChanged; override;
    procedure IsFocusedChanged; override;
    procedure PressedChanged; override;
    procedure EnabledChanged; override;
    procedure DoChange;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    procedure Click; override;
    function GetChecked: boolean; virtual;
    procedure SetChecked(const Value: Boolean); virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure AlignToPixel; override;
    procedure MakeBufDrawable; virtual;
    procedure clearBufDrawable; virtual;
  published
    //property Action;
    property Align;
    property Anchors;
    property CanFocus default true;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered default true;
    property Checked: Boolean read GetChecked write SetChecked default false;
    property ClipChildren;
    //property ClipParent;
    property Cursor default crHandPoint;
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
    property RotationCenter;
    property Scale;
    property Size;
    property TabOrder;
    property TabStop;
    property Thumb: TThumb read FThumb;
    property TouchTargetExpansion;
    property Track: TTrack read FTrack;
    property Transition: TALStateTransition read FTransition write SetTransition;
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
    //property OnDblClick;
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
      // -----------------------
      // TStateStyleTextSettings
      TStateStyleTextSettings = class(TALInheritBaseTextSettings)
      published
        property Font;
        property Decoration;
      end;
      // ---------------
      // TBaseStateStyle
      TBaseStateStyle = class(TALBaseStateStyle)
      private
        FText: String;
        FTextSettings: TStateStyleTextSettings;
        FDefaultText: String;
        FPriorSupersedeText: String;
        function GetStateStyleParent: TBaseStateStyle;
        function GetControlParent: TALButton;
        procedure SetText(const Value: string);
        procedure SetTextSettings(const AValue: TStateStyleTextSettings);
        procedure TextSettingsChanged(ASender: TObject);
        function IsTextStored: Boolean;
      protected
        function GetInherit: Boolean; override;
        procedure DoSupersede; override;
      public
        constructor Create(const AParent: TObject); override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; override;
        procedure Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single); override;
        property StateStyleParent: TBaseStateStyle read GetStateStyleParent;
        property ControlParent: TALButton read GetControlParent;
        property DefaultText: String read FDefaultText write FDefaultText;
      published
        property Fill;
        property Scale;
        property Shadow;
        property Stroke;
        property Text: string read FText write SetText stored IsTextStored nodefault;
        property TextSettings: TStateStyleTextSettings read fTextSettings write SetTextSettings;
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
      end;
      // ------------------
      // TPressedStateStyle
      TPressedStateStyle = class(TBaseStateStyle)
      published
        property StateLayer;
      end;
      // ------------------
      // TFocusedStateStyle
      TFocusedStateStyle = class(TBaseStateStyle)
      published
        property StateLayer;
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
        function CreateSavedState: TALPersistentObserver; override;
      public
        constructor Create(const AParent: TALButton); reintroduce; virtual;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; override;
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
      // -------------
      // TTextSettings
      TTextSettings = class(TALBaseTextSettings)
      published
        property Font;
        property Decoration;
        property Trimming;
        property MaxLines;
        property Ellipsis;
        property HorzAlign;
        property VertAlign;
        property LineHeightMultiplier;
        property LetterSpacing;
        property IsHtml;
      end;
  private
    {$IF defined(ALDPK)}
    FPrevStateStyles: TStateStyles;
    {$ENDIF}
    FStateStyles: TStateStyles;
    function GetTextSettings: TTextSettings;
    procedure SetStateStyles(const AValue: TStateStyles);
  protected
    function CreateTextSettings: TALBaseTextSettings; override;
    procedure SetTextSettings(const Value: TTextSettings); reintroduce;
    procedure SetName(const Value: TComponentName); override;
    procedure TextSettingsChanged(Sender: TObject); override;
    procedure SetXRadius(const Value: Single); override;
    procedure SetYRadius(const Value: Single); override;
    procedure StateStylesChanged(Sender: TObject); virtual;
    procedure IsMouseOverChanged; override;
    procedure IsFocusedChanged; override;
    procedure PressedChanged; override;
    procedure Click; override;
    Procedure DrawMultilineTextAdjustRect(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; var ARect: TrectF; var ASurfaceSize: TSizeF); override;
    procedure Paint; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AlignToPixel; override;
    procedure MakeBufDrawable; override;
    procedure clearBufDrawable; override;
  published
    //property Action;
    property Align;
    property Anchors;
    property AutoSize default True;
    property AutoTranslate;
    property CanFocus default true;
    //property CanParentFocus;
    //property DisableFocusEffect;
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
    property RotationCenter;
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
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCustomTrack = class(TALControl, IALDoubleBufferedControl)
  public
    type
      // ------
      // TTrack
      TTrack = class(TALBaseRectangle)
      public
        type
          // -------------------
          // TStopIndicatorBrush
          TStopIndicatorBrush = class(TALPersistentObserver)
          private
            FColor: TAlphaColor;
            FResourceName: String;
            FWrapMode: TALImageWrapMode;
            FSize: Single;
            FDefaultColor: TAlphaColor;
            FDefaultResourceName: String;
            FDefaultWrapMode: TALImageWrapMode;
            FDefaultSize: Single;
            procedure SetColor(const Value: TAlphaColor);
            procedure SetResourceName(const Value: String);
            procedure SetWrapMode(const Value: TALImageWrapMode);
            procedure SetSize(const Value: Single);
            function IsColorStored: Boolean;
            function IsResourceNameStored: Boolean;
            function IsWrapModeStored: Boolean;
            function IsSizeStored: Boolean;
          protected
            function CreateSavedState: TALPersistentObserver; override;
          public
            constructor Create(const ADefaultColor: TAlphaColor); reintroduce; virtual;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure AlignToPixel; virtual;
            procedure Interpolate(const ATo: TStopIndicatorBrush; const ANormalizedTime: Single); virtual;
            procedure InterpolateNoChanges(const ATo: TStopIndicatorBrush; const ANormalizedTime: Single);
            function hasStopIndicator: Boolean;
            property DefaultColor: TAlphaColor read FDefaultColor write FDefaultColor;
            property DefaultResourceName: String read FDefaultResourceName write FDefaultResourceName;
            property DefaultWrapMode: TALImageWrapMode read FDefaultWrapMode write FDefaultWrapMode;
            property DefaultSize: Single read FDefaultSize write FDefaultSize;
          published
            property Color: TAlphaColor read FColor write SetColor stored IsColorStored;
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
            constructor Create(const AParent: TStopIndicatorBrush; const ADefaultColor: TAlphaColor); reintroduce; virtual;
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
          private
            FStopIndicator: TInheritStopIndicatorBrush;
            function GetControlParent: TTrack;
            procedure SetStopIndicator(const AValue: TInheritStopIndicatorBrush);
            procedure StopIndicatorChanged(ASender: TObject);
          protected
            procedure DoSupersede; override;
            function GetInherit: Boolean; override;
          public
            constructor Create(const AParent: TObject); override;
            destructor Destroy; override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure AlignToPixel; override;
            procedure Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single); override;
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
            function CreateSavedState: TALPersistentObserver; override;
          public
            constructor Create(const AParent: TTrack); reintroduce; virtual;
            destructor Destroy; override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure AlignToPixel; override;
            procedure ClearBufDrawable; override;
            function GetCurrentRawStyle: TALBaseStateStyle; override;
            Property Parent: TTrack read GetParent;
          published
            property Disabled: TDisabledStateStyle read FDisabled write SetDisabled;
          end;
      private
        {$IF defined(ALDPK)}
        FPrevStateStyles: TStateStyles;
        {$ENDIF}
        FStateStyles: TStateStyles;
        FCustomTrack: TALCustomTrack;
        FStopIndicator: TStopIndicatorBrush;
        procedure SetStateStyles(const AValue: TStateStyles);
        procedure SetStopIndicator(const Value: TStopIndicatorBrush);
      protected
        procedure SetXRadius(const Value: Single); override;
        procedure SetYRadius(const Value: Single); override;
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
        procedure AlignToPixel; override;
        procedure MakeBufDrawable; override;
        procedure clearBufDrawable; override;
        property HitTest default false;
        property Locked default True;
        property Position stored false;
      published
        //property Action;
        //property Align;
        //property Anchors;
        //property CanFocus;
        //property CanParentFocus;
        //property DisableFocusEffect;
        //property ClipChildren;
        //property ClipParent;
        property Corners;
        //property Cursor;
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
      public
        constructor Create(const ACustomTrack: TALCustomTrack); override;
      end;
      // ------------
      // TActiveTrack
      TActiveTrack = class(TTrack)
      protected
        function GetBufDrawableSrcRect: TRectF; override;
      public
        constructor Create(const ACustomTrack: TALCustomTrack); override;
      end;
      // ------
      // TThumb
      TThumb = class(TALBaseRectangle)
      public
        type
          // ---------------
          // TBaseStateStyle
          TBaseStateStyle = class(TALBaseStateStyle)
          public
            constructor Create(const AParent: TObject); override;
          published
            property Fill;
            property Scale;
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
          end;
          // ------------------
          // TPressedStateStyle
          TPressedStateStyle = class(TBaseStateStyle)
          published
            property StateLayer;
          end;
          // ------------------
          // TFocusedStateStyle
          TFocusedStateStyle = class(TBaseStateStyle)
          published
            property StateLayer;
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
            function CreateSavedState: TALPersistentObserver; override;
          public
            constructor Create(const AParent: TThumb); reintroduce; virtual;
            destructor Destroy; override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure AlignToPixel; override;
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
        {$IF defined(ALDPK)}
        FPrevStateStyles: TStateStyles;
        {$ENDIF}
        FStateStyles: TStateStyles;
        fValueRange: TValueRange;
        FCustomTrack: TALCustomTrack;
        fCustomTrackMouseDownPos: TPointF;
        fScrollCapturedByMe: boolean;
        procedure SetStateStyles(const AValue: TStateStyles);
        procedure ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
      protected
        procedure DoBeginUpdate; override;
        procedure DoEndUpdate; override;
        procedure SetXRadius(const Value: Single); override;
        procedure SetYRadius(const Value: Single); override;
        procedure StateStylesChanged(Sender: TObject); virtual;
        procedure IsMouseOverChanged; override;
        procedure IsFocusedChanged; override;
        procedure PressedChanged; override;
        procedure ValueRangeChanged(Sender: TObject); Virtual;
        procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
        procedure Paint; override;
      public
        constructor Create(const ACustomTrack: TALCustomTrack); reintroduce; virtual;
        destructor Destroy; override;
        procedure AlignToPixel; override;
        function GetValue: Double;
        procedure MakeBufDrawable; override;
        procedure clearBufDrawable; override;
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
        //property CanFocus default true;
        //property CanParentFocus;
        //property DisableFocusEffect;
        //property ClipChildren;
        //property ClipParent;
        property Corners;
        property Cursor default crHandPoint;
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
        type
          // ----------
          // TAnimation
          TAnimation = (None, ScaleInOut, Opacity);
          // ----------
          // TAnimation
          TCustomFormatEvent = procedure(ASender: TObject; const AValue: Double; Var AText: String) of object;
      private
        FCustomTrack: TALCustomTrack;
        FFormat: String;
        FDefaultFormat: String;
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
        function CreateTextSettings: TALBaseTextSettings; override;
        procedure SetTextSettings(const Value: TALTextSettings); reintroduce;
      public
        constructor Create(const ACustomTrack: TALCustomTrack); reintroduce; virtual;
        destructor Destroy; override;
        procedure Refresh(const AThumb: TThumb);
        property DefaultFormat: String read FDefaultFormat write FDefaultFormat;
        property Visible default false;
      published
        //property Action;
        //property Align;
        //property Anchors;
        property Animation: TAnimation read FAnimation write FAnimation default TAnimation.ScaleInOut;
        property AutoSize default true;
        //property AutoTranslate;
        //property CanFocus;
        //property CanParentFocus;
        //property DisableFocusEffect;
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
    fDoubleBuffered: boolean;
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
    function GetDoubleBuffered: boolean; virtual;
    procedure SetDoubleBuffered(const AValue: Boolean); virtual;
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
    procedure MakeBufDrawable; virtual;
    procedure clearBufDrawable; virtual;
    property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered default true;
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
    property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property DoubleBuffered;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
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
    property RotationCenter;
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
    //property OnDblClick;
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
      // ----------------------
      // TRangeMinInactiveTrack
      TRangeMinInactiveTrack = class(TInactiveTrack)
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
      // ----------------------
      // TRangeMaxInactiveTrack
      TRangeMaxInactiveTrack = class(TInactiveTrack)
      protected
        function GetBufDrawableSrcRect: TRectF; override;
      end;
      // -----------------
      // TRangeActiveTrack
      TRangeActiveTrack = class(TActiveTrack)
      protected
        function GetBufDrawableSrcRect: TRectF; override;
      end;
      // --------------
      // TRangeMinThumb
      TRangeMinThumb = class(TThumb)
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
        procedure TouchTargetExpansionChanged(Sender: TObject);
        procedure DoResized; override;
     public
       constructor Create(const ACustomTrack: TALCustomTrack); override;
     published
        property Opacity: Single read _GetOpacity write _SetOpacity stored _IsOpacityStored nodefault;
        property Cursor: TCursor read _GetCursor write _SetCursor default crHandPoint;
      end;
      // --------------
      // TRangeMaxThumb
      TRangeMaxThumb = class(TThumb)
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
    procedure DoRealign; override;
    procedure AlignThumb; override;
    procedure AlignTracks; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure MakeBufDrawable; override;
    procedure clearBufDrawable; override;
  published
    //property Action;
    property ActiveTrack;
    property Align;
    property Anchors;
    property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property DoubleBuffered;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
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
    property RotationCenter;
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
    //property OnDblClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALScrollBar = class(TALCustomTrack)
  protected
    function GetDefaultSize: TSizeF; override;
    procedure AlignThumb; override;
    function CreateInactiveTrack(const AInactiveTrackClass: TALCustomTrack.TInactiveTrackClass = nil; Const AName: String = 'InactiveTrack'): TALCustomTrack.TInactiveTrack; override;
    function CreateActiveTrack(const AActiveTrackClass: TALCustomTrack.TActiveTrackClass = nil; Const AName: String = 'ActiveTrack'): TALCustomTrack.TActiveTrack; override;
    function CreateValueIndicator(const AValueIndicatorClass: TALCustomTrack.TValueIndicatorClass = nil; Const AName: String = 'ValueIndicator'): TALCustomTrack.TValueIndicator; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    //property Action;
    property Align;
    property Anchors;
    property CanFocus default False;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property DoubleBuffered;
    property ClipChildren;
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
    property Min;
    property Max;
    property Opacity;
    property Orientation;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
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
    //property OnDblClick;
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
  Alcinoe.StringUtils,
  Alcinoe.Common;

{*****************************************************}
constructor TALAniIndicator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  finterval := 50;
  FFrameCount := 20;
  FRowCount := 4;
  fResourceName := 'aniindicator_540x432';
  fFrameIndex := TSmallPoint.Create(0,0);
  fTimer := TTimer.Create(self);
  fTimer.Enabled := False;
  fTimer.Interval := finterval;
  fTimer.OnTimer := onTimer;
  fBufDrawable := ALNullDrawable;
  SetAcceptsControls(False);
end;

{*********************************}
destructor TALAniIndicator.Destroy;
begin
  ALFreeAndNil(fTimer);
  clearBufDrawable;
  inherited;
end;

{**********************************************}
function TALAniIndicator.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(36, 36);
end;

{**********************************}
procedure TALAniIndicator.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{*****************************************}
procedure TALAniIndicator.clearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
end;

{****************************************}
procedure TALAniIndicator.MakeBufDrawable;
begin

  if //--- Do not create BufDrawable if the size is 0
     (Size.Size.IsZero) or
     //--- Do not create BufDrawable if fResourceName is empty
     (fResourceName = '')
  then begin
    clearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(fBufDrawable)) then exit;

  fBufDrawableRect := LocalRect;
  {$IFDEF ALDPK}
  try
    var LFileName := ALGetResourceFilename(FResourceName);
    if LFileName <> '' then fBufDrawable := ALLoadFromFileAndFitIntoToDrawable(LFileName, Width * (fframeCount div fRowCount) * ALGetScreenScale, Height * fRowCount * ALGetScreenScale)
    else fBufDrawable := ALNullDrawable;
  except
    fBufDrawable := ALNullDrawable;
  end;
  {$ELSE}
  fBufDrawable := ALLoadFromResourceAndFitIntoToDrawable(fResourceName, Width * (fframeCount div fRowCount) * ALGetScreenScale, Height * fRowCount * ALGetScreenScale);
  {$ENDIF}

end;

{*************************************************}
procedure TALAniIndicator.onTimer(sender: Tobject);
begin
  if not IsVisibleWithinFormBounds then begin
    FTimer.Enabled := False;
    exit;
  end;
  inc(fFrameIndex.x);
  if fFrameIndex.x >= FFrameCount div FRowCount then begin
    fFrameIndex.x := 0;
    inc(fFrameIndex.Y);
    if fFrameIndex.Y >= FRowCount then fFrameIndex.Y := 0;
  end;
  repaint;
end;

{******************************}
procedure TALAniIndicator.Paint;
begin

  FTimer.Enabled := True;

  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    var R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
  end;

  MakeBufDrawable;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    fBufDrawable, // const ADrawable: TALDrawable;
    TRectF.Create(
      TPointF.Create(
        fFrameIndex.x * Width * ALGetScreenScale,
        fFrameIndex.Y * Height * ALGetScreenScale),
      Width * ALGetScreenScale,
      Height * ALGetScreenScale), // const ASrcRect: TrectF; // IN REAL PIXEL !
    fBufDrawableRect, // const ADestRect: TrectF; // IN virtual pixels !
    AbsoluteOpacity); // const AOpacity: Single);

end;

{**************************************************}
function TALAniIndicator.GetDoubleBuffered: boolean;
begin
  result := True;
end;

{*****************************************************************}
procedure TALAniIndicator.SetDoubleBuffered(const AValue: Boolean);
begin
  // Not yet supported
end;

{***************************************************}
function TALAniIndicator.ResourceNameStored: Boolean;
begin
  result := fResourceName <> 'aniindicator_540x432';
end;

{*************************************************************}
procedure TALAniIndicator.setResourceName(const Value: String);
begin
  if FResourceName <> Value then begin
    clearBufDrawable;
    FResourceName := Value;
    Repaint;
  end;
end;

{*******************************************************************************}
constructor TALCustomTrack.TThumb.TBaseStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  //--
  Fill.DefaultColor := TAlphaColors.White;
  Fill.Color := Fill.DefaultColor;
  //--
  Stroke.DefaultColor := $ffd5d5d5;
  Stroke.Color := Stroke.DefaultColor;
  //--
  StateLayer.DefaultXRadius := -50;
  StateLayer.DefaultYRadius := -50;
  StateLayer.XRadius := StateLayer.DefaultXRadius;
  StateLayer.YRadius := StateLayer.DefaultYRadius;
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

{******************************************************************************************}
constructor TALCustomTrack.TThumb.TStateStyles.Create(const AParent: TALCustomTrack.TThumb);
begin
  inherited Create(AParent);
  //--
  FDisabled := TDisabledStateStyle.Create(AParent);
  FDisabled.OnChanged := DisabledChanged;
  //--
  FHovered := THoveredStateStyle.Create(AParent);
  FHovered.OnChanged := HoveredChanged;
  //--
  FPressed := TPressedStateStyle.Create(AParent);
  FPressed.OnChanged := PressedChanged;
  //--
  FFocused := TFocusedStateStyle.Create(AParent);
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

{**********************************************************************************}
function TALCustomTrack.TThumb.TStateStyles.CreateSavedState: TALPersistentObserver;
type
  TThumbStateStylesClass = class of TStateStyles;
begin
  result := TThumbStateStylesClass(classtype).Create(nil{AParent});
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
  {$IF defined(ALDPK)}
  FPrevStateStyles := nil;
  {$ENDIF}
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
  DoubleBuffered := ACustomTrack.DoubleBuffered;
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
  var LFillChanged: TNotifyEvent := fill.OnChanged;
  fill.OnChanged := nil;
  Fill.DefaultColor := TAlphaColors.White;
  Fill.Color := Fill.DefaultColor;
  fill.OnChanged := LFillChanged;
  //--
  var LStrokeChanged: TNotifyEvent := stroke.OnChanged;
  stroke.OnChanged := Nil;
  Stroke.DefaultColor := $ffd5d5d5;
  Stroke.Color := Stroke.DefaultColor;
  stroke.OnChanged := LStrokeChanged;
  //--
  DefaultXRadius := -50;
  DefaultYRadius := -50;
  XRadius := DefaultXRadius;
  YRadius := DefaultYRadius;
  //--
  {$IF defined(ALDPK)}
  FPrevStateStyles := TStateStyles.Create(nil);
  {$ENDIF}
  //--
  FStateStyles := TStateStyles.Create(self);
  FStateStyles.OnChanged := StateStylesChanged;
end;

{***************************************}
destructor TALCustomTrack.TThumb.Destroy;
begin
  {$IF defined(ALDPK)}
  ALFreeAndNil(FPrevStateStyles);
  {$ENDIF}
  ALFreeAndNil(FStateStyles);
  TMessageManager.DefaultManager.Unsubscribe(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
  ALFreeAndNil(FValueRange);
  inherited;
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

{**************************************************************}
procedure TALCustomTrack.TThumb.SetXRadius(const Value: Single);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin
    if (not (csLoading in ComponentState)) and
       (not AToStateStyle.StateLayer.HasFill) then begin
      if (SameValue(APrevStateStyle.StateLayer.XRadius, AToStateStyle.StateLayer.XRadius, TEpsilon.Vector)) then AToStateStyle.StateLayer.XRadius := XRadius;
    end;
    APrevStateStyle.StateLayer.XRadius := XRadius;
  end;
  {$ENDIF}

begin
  inherited;
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.Disabled, StateStyles.Disabled);
    _PropagateChanges(FPrevStateStyles.Hovered, StateStyles.Hovered);
    _PropagateChanges(FPrevStateStyles.Pressed, StateStyles.Pressed);
    _PropagateChanges(FPrevStateStyles.Focused, StateStyles.Focused);
  end;
  {$ENDIF}
end;

{**************************************************************}
procedure TALCustomTrack.TThumb.SetYRadius(const Value: Single);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin
    if (not (csLoading in ComponentState)) and
       (not AToStateStyle.StateLayer.HasFill) then begin
      if (SameValue(APrevStateStyle.StateLayer.YRadius, AToStateStyle.StateLayer.YRadius, TEpsilon.Vector)) then AToStateStyle.StateLayer.YRadius := YRadius;
    end;
    APrevStateStyle.StateLayer.YRadius := YRadius;
  end;
  {$ENDIF}

begin
  inherited;
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.Disabled, StateStyles.Disabled);
    _PropagateChanges(FPrevStateStyles.Hovered, StateStyles.Hovered);
    _PropagateChanges(FPrevStateStyles.Pressed, StateStyles.Pressed);
    _PropagateChanges(FPrevStateStyles.Focused, StateStyles.Focused);
  end;
  {$ENDIF}
end;

{******************************************************************}
procedure TALCustomTrack.TThumb.StateStylesChanged(Sender: TObject);
begin
  clearBufDrawable;
  DisabledOpacity := StateStyles.Disabled.opacity;
  Repaint;
end;

{*************************************************}
procedure TALCustomTrack.TThumb.IsMouseOverChanged;
begin
  inherited;
  StateStyles.startTransition;
  if FcustomTrack.FValueIndicator <> nil then
    FcustomTrack.FValueIndicator.Refresh(Self);
  repaint;
end;

{***********************************************}
procedure TALCustomTrack.TThumb.IsFocusedChanged;
begin
  inherited;
  StateStyles.startTransition;
  if FcustomTrack.FValueIndicator <> nil then
    FcustomTrack.FValueIndicator.Refresh(Self);
  repaint;
end;

{*********************************************}
procedure TALCustomTrack.TThumb.PressedChanged;
begin
  inherited;
  StateStyles.startTransition;
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
  //  'Pressed: ' + ALBoolToStrW(Pressed),
  //  TalLogType.verbose);
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
  inherited;
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
procedure TALCustomTrack.TThumb.clearBufDrawable;
begin
  {$IFDEF debug}
  if (FStateStyles <> nil) and
     (not (csDestroying in ComponentState)) and
     (ALIsDrawableNull(BufDrawable)) and // warn will be raise in inherited
     ((not ALIsDrawableNull(FStateStyles.Disabled.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Hovered.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Pressed.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Focused.BufDrawable))) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  if FStateStyles <> nil then
    FStateStyles.ClearBufDrawable;
  inherited clearBufDrawable;
end;

{**********************************************}
procedure TALCustomTrack.TThumb.MakeBufDrawable;
begin
  //--- Do not create BufDrawable if not DoubleBuffered
  if {$IF not DEFINED(ALDPK)}(not DoubleBuffered){$ELSE}False{$ENDIF} then begin
    clearBufDrawable;
    exit;
  end;
  //--
  inherited MakeBufDrawable;
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if LStateStyle.Inherit then exit;
  if (not ALIsDrawableNull(LStateStyle.BufDrawable)) then exit;
  LStateStyle.SupersedeNoChanges(true{ASaveState});
  try

    // Create the BufDrawable
    CreateBufDrawable(
      LStateStyle.BufDrawable, // var ABufDrawable: TALDrawable;
      LStateStyle.BufDrawableRect, // var ABufDrawableRect: TRectF;
      ALGetScreenScale * LStateStyle.Scale, // const AScale: Single;
      LStateStyle.Fill, // const AFill: TALBrush;
      LStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
      TAlphaColors.null, // const AStateLayerContentColor: TAlphaColor;
      False, // const ADrawStateLayerOnTop: Boolean;
      LStateStyle.Stroke, // const AStroke: TALStrokeBrush;
      LStateStyle.Shadow); // const AShadow: TALShadow);

    // LStateStyle.BufDrawableRect must include the LStateStyle.Scale
    LStateStyle.BufDrawableRect.Top := LStateStyle.BufDrawableRect.Top * LStateStyle.Scale;
    LStateStyle.BufDrawableRect.right := LStateStyle.BufDrawableRect.right * LStateStyle.Scale;
    LStateStyle.BufDrawableRect.left := LStateStyle.BufDrawableRect.left * LStateStyle.Scale;
    LStateStyle.BufDrawableRect.bottom := LStateStyle.BufDrawableRect.bottom * LStateStyle.Scale;

    // Since LStateStyle.BufDrawableRect can have different dimensions than the main BufDrawableRect
    // (due to scale), we must center LStateStyle.BufDrawableRect
    // within the main BufDrawableRect to ensure that all changes are visually centered.
    var LMainDrawableRect := BufDrawableRect;
    LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
    var LCenteredRect := LStateStyle.BufDrawableRect.CenterAt(LMainDrawableRect);
    LStateStyle.BufDrawableRect.Offset(LCenteredRect.Left, LCenteredRect.top);

  finally
    LStateStyle.RestorestateNoChanges;
  end;
end;

{************************************}
procedure TALCustomTrack.TThumb.Paint;
begin

  StateStyles.UpdateLastPaintedRawStyle;
  MakeBufDrawable;

  var LDrawable: TALDrawable;
  var LDrawableRect: TRectF;
  if StateStyles.IsTransitionAnimationRunning then begin
    LDrawable := ALNullDrawable;
    LDrawableRect := TRectF.Empty;
  end
  else begin
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      LDrawable := LStateStyle.BufDrawable;
      LDrawableRect := LStateStyle.BufDrawableRect;
      if ALIsDrawableNull(LDrawable) then begin
        LDrawable := BufDrawable;
        LDrawableRect := BufDrawableRect;
      end;
    end
    else begin
      LDrawable := BufDrawable;
      LDrawableRect := BufDrawableRect;
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
                                                LCurrentAdjustedStateStyle.Scale, // Const AScaleX: Single;
                                                LCurrentAdjustedStateStyle.Scale, // Const AScaleY: Single;
                                                true); // Const ASaveState: Boolean);
    try

      ALDrawRectangle(
        TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
        1, // const AScale: Single;
        IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
        LocalRect, // const Rect: TrectF;
        AbsoluteOpacity, // const AOpacity: Single;
        LCurrentAdjustedStateStyle.Fill, // const Fill: TALBrush;
        LCurrentAdjustedStateStyle.StateLayer, // const StateLayer: TALStateLayer;
        TalphaColors.Null, // const AStateLayerContentColor: TAlphaColor;
        True, // const ADrawStateLayerOnTop: Boolean;
        LCurrentAdjustedStateStyle.Stroke, // const Stroke: TALStrokeBrush;
        LCurrentAdjustedStateStyle.Shadow, // const Shadow: TALShadow
        Sides, // const Sides: TSides;
        Corners, // const Corners: TCorners;
        XRadius, // const XRadius: Single = 0;
        YRadius); // const YRadius: Single = 0);

    finally
      if LCanvasSaveState <> nil then
        Canvas.RestoreState(LCanvasSaveState);
    end;

    {$ELSE}

    if StateStyles.IsTransitionAnimationRunning then begin

      var LRect := LocalRect;
      var LBufSurface: TALSurface;
      var LBufCanvas: TALCanvas;
      var LBufDrawable: TALDrawable;
      StateStyles.GetTransitionBufSurface(
        LRect, // var ARect: TrectF;
        ALGetScreenScale, // const AScale: Single;
        LBufSurface, // out ABufSurface: TALSurface;
        LBufCanvas, // out ABufCanvas: TALCanvas;
        LBufDrawable); // out ABufDrawable: TALDrawable);

      if ALCanvasBeginScene(LBufCanvas) then
      try

        ALClearCanvas(LBufCanvas, TAlphaColors.Null);

        ALDrawRectangle(
          LBufCanvas, // const ACanvas: TALCanvas;
          ALGetScreenScale, // const AScale: Single;
          IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
          LRect, // const Rect: TrectF;
          AbsoluteOpacity, // const AOpacity: Single;
          LCurrentAdjustedStateStyle.Fill, // const Fill: TALBrush;
          LCurrentAdjustedStateStyle.StateLayer, // const StateLayer: TALStateLayer;
          TalphaColors.Null, // const AStateLayerContentColor: TAlphaColor;
          True, // const ADrawStateLayerOnTop: Boolean;
          LCurrentAdjustedStateStyle.Stroke, // const Stroke: TALStrokeBrush;
          LCurrentAdjustedStateStyle.Shadow, // const Shadow: TALShadow
          Sides, // const Sides: TSides;
          Corners, // const Corners: TCorners;
          XRadius, // const XRadius: Single = 0;
          YRadius); // const YRadius: Single = 0);

      finally
        ALCanvasEndScene(LBufCanvas)
      end;

      ALUpdateDrawableFromSurface(LBufSurface, LBufDrawable);

      // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
      // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
      LRect.Offset(-2*LRect.Left, -2*LRect.Top);

      // LRect must include the LScale
      LRect.Top := LRect.Top * LCurrentAdjustedStateStyle.Scale;
      LRect.right := LRect.right * LCurrentAdjustedStateStyle.Scale;
      LRect.left := LRect.left * LCurrentAdjustedStateStyle.Scale;
      LRect.bottom := LRect.bottom * LCurrentAdjustedStateStyle.Scale;

      // Since LStateStyle.BufDrawableRect can have different dimensions than the main BufDrawableRect
      // (due to autosizing with different font sizes), we must center LStateStyle.BufDrawableRect
      // within the main BufDrawableRect to ensure that all changes are visually centered.
      var LMainDrawableRect := BufDrawableRect;
      LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
      var LCenteredRect := LRect.CenterAt(LMainDrawableRect);
      LRect.Offset(LCenteredRect.Left, LCenteredRect.top);

      // We cannot use the matrix because, if we do, ALAlignToPixelRound in ALDrawDrawable
      // will be ineffective since the matrix will no longer be a simple translation matrix.
      // In such a case, TCustomCanvasGpu(ACanvas).DrawTexture may produce border artifacts
      // if the texture is not perfectly pixel-aligned.
      var LDstRect := TRectF.Create(0, 0, ALGetDrawableWidth(LBufDrawable), ALGetDrawableHeight(LBufDrawable));
      LDstRect.Width := (LDstRect.Width / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
      LDstRect.height := (LDstRect.height / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
      LDstRect.SetLocation(
        LRect.Left,
        LRect.Top);
      ALDrawDrawable(
        Canvas, // const ACanvas: Tcanvas;
        LBufDrawable, // const ADrawable: TALDrawable;
        LDstRect, // const ADstRect: TrectF; // IN Virtual pixels !
        AbsoluteOpacity); // const AOpacity: Single)

    end

    {$IF defined(DEBUG)}
    else if not doublebuffered then begin
      ALLog('TALCustomTrack.TThumb.Paint', 'Controls that are not double-buffered only work when SKIA is enabled', TALLogType.ERROR);
      exit;
    end
    {$ENDIF};

    {$ENDIF}

    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{************************************************************************************}
constructor TALCustomTrack.TValueIndicator.Create(const ACustomTrack: TALCustomTrack);
begin
  inherited create(ACustomTrack);
  AutoSize := True;
  Visible := False;
  FCustomTrack := ACustomTrack;
  FDefaultFormat := '0';
  FFormat := FDefaultFormat;
  FOnCustomFormat := nil;
  FAnimation := TAnimation.ScaleInOut;
  FShowOnInteraction := False;
  //--
  FFloatAnimation := TALFloatAnimation.Create;
  FFloatAnimation.StartValue := 0;
  FFloatAnimation.StopValue := 1;
  FFloatAnimation.Duration := 0.2;
  FFloatAnimation.AnimationType := TanimationType.out;
  FFloatAnimation.Interpolation := TALInterpolationType.cubic;
  FFloatAnimation.OnProcess := AnimationProcess;
  FFloatAnimation.OnFinish := AnimationFinish;
  //--
  var LFillChanged: TNotifyEvent := fill.OnChanged;
  fill.OnChanged := nil;
  Fill.DefaultColor := TAlphacolors.Black;
  Fill.Color := Fill.DefaultColor;
  fill.OnChanged := LFillChanged;
  //--
  var LStrokeChanged: TNotifyEvent := stroke.OnChanged;
  stroke.OnChanged := Nil;
  Stroke.DefaultColor := TAlphaColors.Null;
  Stroke.Color := Stroke.DefaultColor;
  stroke.OnChanged := LStrokeChanged;
  //--
  var LTextSettingsChanged: TNotifyEvent := TextSettings.OnChanged;
  TextSettings.OnChanged := nil;
  TextSettings.Font.DefaultColor := TAlphaColors.White;
  TextSettings.Font.Color := TextSettings.Font.DefaultColor;
  TextSettings.Font.DefaultWeight := TFontWeight.medium;
  TextSettings.Font.Weight := TextSettings.Font.DefaultWeight;
  TextSettings.DefaultHorzAlign := TALTextHorzAlign.center;
  TextSettings.HorzAlign := TextSettings.DefaultHorzAlign;
  TextSettings.OnChanged := LTextSettingsChanged;
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
  //--
  DefaultXRadius := -50;
  DefaultYRadius := -50;
  XRadius := DefaultXRadius;
  YRadius := DefaultYRadius;
end;

{************************************************}
destructor TALCustomTrack.TValueIndicator.Destroy;
begin
  ALFreeAndNil(FFloatAnimation);
  inherited;
end;

{****************************************************************************}
procedure TALCustomTrack.TValueIndicator.AdjustPosition(const AThumb: TThumb);
begin
  if FCustomTrack.Orientation = TOrientation.Horizontal then begin
    Position.Point := TpointF.Create(
                        AThumb.Position.X - (((Width * Scale.X) - AThumb.Width) / 2),
                        AThumb.Position.Y - (Height * Scale.Y) - Margins.Bottom);
  end
  else begin
    Position.Point := TpointF.Create(
                        AThumb.Position.X + AThumb.Width + Margins.Left,
                        AThumb.Position.Y - (((Height * Scale.Y) - AThumb.Height) / 2));
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
      Text := ALFormatFloatW(Format, AThumb.GetValue);
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
          Scale.Point := TPointF.Create(0,0);
        end;
        TAnimation.Opacity: begin
          AdjustPosition(AThumb);
          Opacity := 0;
          Scale.Point := TPointF.Create(1,1);
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
    TAnimation.ScaleInOut: begin
      var LThumb := TTHumb(FFloatAnimation.TagObject);
      {$IFDEF DEBUG}
      if LThumb = nil then
        Raise Exception.Create('Error 5B1652DB-6068-48EE-862F-ADDBC256A86D');
      {$ENDIF}
      Scale.Point := TPointF.Create(FFloatAnimation.CurrentValue,FFloatAnimation.CurrentValue);
      AdjustPosition(LThumb)
    end;
    TAnimation.Opacity: begin
      Opacity := FFloatAnimation.CurrentValue
    end
    else
      Raise Exception.Create('Error D6F17D76-E47E-4144-8FBA-5CAD3EBF84F3')
  end;
end;

{************************************************************************}
procedure TALCustomTrack.TValueIndicator.AnimationFinish(Sender: TObject);
begin
  FFloatAnimation.Enabled := False;
  case FAnimation of
    TAnimation.ScaleInOut: begin
      if SameValue(Scale.Point.x, 0, TEpsilon.Vector) or
         SameValue(Scale.Point.y, 0, TEpsilon.Vector) then visible := False;
    end;
    TAnimation.Opacity: begin
      if SameValue(Opacity, 0, TEpsilon.Vector) then visible := False;
    end
    else
      Raise Exception.Create('Error D6F17D76-E47E-4144-8FBA-5CAD3EBF84F3')
  end;
end;

{******************************************************************************}
function TALCustomTrack.TValueIndicator.CreateTextSettings: TALBaseTextSettings;
begin
  Result := TALTextSettings.Create;
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

{**********************************************************************}
procedure TALCustomTrack.TValueIndicator.SetFormat(const Value: string);
begin
  if FFormat <> Value then
  begin
    clearBufDrawable;
    FFormat := Value;
  end;
end;

{**************************************************************}
function TALCustomTrack.TValueIndicator.IsFormatStored: Boolean;
begin
  Result := FFormat <> FDefaultFormat;
end;

{*********************************************************************************************}
constructor TALCustomTrack.TTrack.TStopIndicatorBrush.Create(const ADefaultColor: TAlphaColor);
begin
  inherited Create;
  //--
  FDefaultColor := ADefaultColor;
  FDefaultResourceName := '';
  FDefaultWrapMode := TALImageWrapMode.Fit;
  FDefaultSize := 4;
  //--
  FColor := FDefaultColor;
  FResourceName := FDefaultResourceName;
  FWrapMode := FDefaultWrapMode;
  FSize := FDefaultSize;
end;

{*****************************************************************************************}
function TALCustomTrack.TTrack.TStopIndicatorBrush.CreateSavedState: TALPersistentObserver;
type
  TStopIndicatorBrushClass = class of TStopIndicatorBrush;
begin
  result := TStopIndicatorBrushClass(classtype).Create(DefaultColor);
end;

{******************************************************************************}
procedure TALCustomTrack.TTrack.TStopIndicatorBrush.Assign(Source: TPersistent);
begin
  if Source is TStopIndicatorBrush then begin
    BeginUpdate;
    Try
      Color := TStopIndicatorBrush(Source).Color;
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

{*****************************************************************************************************************************}
procedure TALCustomTrack.TTrack.TStopIndicatorBrush.Interpolate(const ATo: TStopIndicatorBrush; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    if ATo <> nil then begin
      Color := ALInterpolateColor(Color{Start}, ATo.Color{Stop}, ANormalizedTime);
      ResourceName := ATo.ResourceName;
      WrapMode := ATo.WrapMode;
      Size := InterpolateSingle(Size{Start}, ATo.Size{Stop}, ANormalizedTime);
    end
    else begin
      Color := ALInterpolateColor(Color{Start}, DefaultColor{Stop}, ANormalizedTime);
      ResourceName := DefaultResourceName;
      WrapMode := DefaultWrapMode;
      Size := InterpolateSingle(Size{Start}, DefaultSize{Stop}, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{**************************************************************************************************************************************}
procedure TALCustomTrack.TTrack.TStopIndicatorBrush.InterpolateNoChanges(const ATo: TStopIndicatorBrush; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
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
  result := FColor <> FDefaultColor;
end;

{*******************************************************************************}
function TALCustomTrack.TTrack.TStopIndicatorBrush.IsResourceNameStored: Boolean;
begin
  result := FResourceName <> FDefaultResourceName;
end;

{***************************************************************************}
function TALCustomTrack.TTrack.TStopIndicatorBrush.IsWrapModeStored: Boolean;
begin
  result := FWrapMode <> FDefaultWrapMode;
end;

{***********************************************************************}
function TALCustomTrack.TTrack.TStopIndicatorBrush.IsSizeStored: Boolean;
begin
  result := not SameValue(fSize, FDefaultSize, Tepsilon.Position);
end;

{*************************************************************************************}
procedure TALCustomTrack.TTrack.TStopIndicatorBrush.SetColor(const Value: TAlphaColor);
begin
  if fColor <> Value then begin
    fColor := Value;
    Change;
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

{****************************************************************************************************************************************}
constructor TALCustomTrack.TTrack.TInheritStopIndicatorBrush.Create(const AParent: TStopIndicatorBrush; const ADefaultColor: TAlphaColor);
begin
  inherited create(ADefaultColor);
  FParent := AParent;
  FInherit := True;
  fSuperseded := False;
end;

{************************************************************************************************}
function TALCustomTrack.TTrack.TInheritStopIndicatorBrush.CreateSavedState: TALPersistentObserver;
type
  TInheritStopIndicatorBrushClass = class of TInheritStopIndicatorBrush;
begin
  result := TInheritStopIndicatorBrushClass(classtype).Create(nil{AParent}, DefaultColor);
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

{*******************************************************************************}
constructor TALCustomTrack.TTrack.TBaseStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  //--
  if StateStyleParent <> nil then Raise Exception.create('Error 19ACDBF0-E33C-49D2-B199-5C232A0A71DB')
  else if ControlParent <> nil then FStopIndicator := TInheritStopIndicatorBrush.Create(ControlParent.StopIndicator, TAlphaColors.null)
  else FStopIndicator := TInheritStopIndicatorBrush.Create(nil, TAlphaColors.null);
  FStopIndicator.OnChanged := StopIndicatorChanged;
  //--
  Stroke.DefaultColor := TalphaColors.Null;
  Stroke.Color := Stroke.DefaultColor;
  //--
  StateLayer.DefaultXRadius := -50;
  StateLayer.DefaultYRadius := -50;
  StateLayer.XRadius := StateLayer.DefaultXRadius;
  StateLayer.YRadius := StateLayer.DefaultYRadius;
end;

{*******************************************************}
destructor TALCustomTrack.TTrack.TBaseStateStyle.Destroy;
begin
  ALFreeAndNil(FStopIndicator);
  inherited Destroy;
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

{***********************************************************************************************************************}
procedure TALCustomTrack.TTrack.TBaseStateStyle.Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single);
begin
  {$IF defined(debug)}
  if (ATo <> nil) and (not (ATo is TBaseStateStyle)) then
    Raise Exception.Create('Error 70FA71DE-6270-441D-AB33-6F987A011C09');
  {$ENDIF}
  BeginUpdate;
  Try
    inherited Interpolate(ATo, ANormalizedTime);
    if ATo <> nil then StopIndicator.Interpolate(TBaseStateStyle(ATo).StopIndicator, ANormalizedTime)
    {$IF defined(debug)}
    else if StateStyleParent <> nil then Raise Exception.Create('Error 9B674B61-66C2-4BB1-8A94-D6A58AEAF404')
    {$ENDIF}
    else if ControlParent <> nil then StopIndicator.Interpolate(ControlParent.StopIndicator, ANormalizedTime)
    else StopIndicator.Interpolate(nil, ANormalizedTime);
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

{******************************************************************************************}
constructor TALCustomTrack.TTrack.TStateStyles.Create(const AParent: TALCustomTrack.TTrack);
begin
  inherited Create(AParent);
  //--
  FDisabled := TDisabledStateStyle.Create(AParent);
  FDisabled.OnChanged := DisabledChanged;
end;

{****************************************************}
destructor TALCustomTrack.TTrack.TStateStyles.Destroy;
begin
  ALFreeAndNil(FDisabled);
  inherited Destroy;
end;

{**********************************************************************************}
function TALCustomTrack.TTrack.TStateStyles.CreateSavedState: TALPersistentObserver;
type
  TTrackStateStylesClass = class of TStateStyles;
begin
  result := TTrackStateStylesClass(classtype).Create(nil{AParent});
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
  {$IF defined(ALDPK)}
  FPrevStateStyles := nil;
  {$ENDIF}
  FStateStyles := nil;
  //--
  inherited Create(ACustomTrack);
  FCustomTrack := ACustomTrack;
  FStopIndicator := TStopIndicatorBrush.Create(TAlphaColors.Null);
  FStopIndicator.OnChanged := StopIndicatorChanged;
  Locked := True;
  HitTest := False;
  DoubleBuffered := ACustomTrack.DoubleBuffered;
  //--
  {$IF defined(ALDPK)}
  FPrevStateStyles := TStateStyles.Create(nil);
  {$ENDIF}
  //--
  FStateStyles := TStateStyles.Create(self);
  FStateStyles.OnChanged := StateStylesChanged;
end;

{***************************************}
destructor TALCustomTrack.TTrack.Destroy;
begin
  {$IF defined(ALDPK)}
  ALFreeAndNil(FPrevStateStyles);
  {$ENDIF}
  ALFreeAndNil(FStateStyles);
  ALFreeAndNil(FStopIndicator);
  inherited;
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

{**************************************************************}
procedure TALCustomTrack.TTrack.SetXRadius(const Value: Single);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin
    if (not (csLoading in ComponentState)) and
       (not AToStateStyle.StateLayer.HasFill) then begin
      if (SameValue(APrevStateStyle.StateLayer.XRadius, AToStateStyle.StateLayer.XRadius, TEpsilon.Vector)) then AToStateStyle.StateLayer.XRadius := XRadius;
    end;
    APrevStateStyle.StateLayer.XRadius := XRadius;
  end;
  {$ENDIF}

begin
  inherited;
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.Disabled, StateStyles.Disabled);
  end;
  {$ENDIF}
end;

{**************************************************************}
procedure TALCustomTrack.TTrack.SetYRadius(const Value: Single);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin
    if (not (csLoading in ComponentState)) and
       (not AToStateStyle.StateLayer.HasFill) then begin
      if (SameValue(APrevStateStyle.StateLayer.YRadius, AToStateStyle.StateLayer.YRadius, TEpsilon.Vector)) then AToStateStyle.StateLayer.YRadius := YRadius;
    end;
    APrevStateStyle.StateLayer.YRadius := YRadius;
  end;
  {$ENDIF}

begin
  inherited;
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.Disabled, StateStyles.Disabled);
  end;
  {$ENDIF}
end;

{******************************************************************}
procedure TALCustomTrack.TTrack.StateStylesChanged(Sender: TObject);
begin
  clearBufDrawable;
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
procedure TALCustomTrack.TTrack.clearBufDrawable;
begin
  if FcustomTrack.FIsAligningTracks then exit;
  {$IFDEF debug}
  if (FStateStyles <> nil) and
     (not (csDestroying in ComponentState)) and
     (ALIsDrawableNull(BufDrawable)) and // warn will be raise in inherited
     (not ALIsDrawableNull(FStateStyles.Disabled.BufDrawable)) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  if FStateStyles <> nil then
    FStateStyles.ClearBufDrawable;
  inherited clearBufDrawable;
end;

{**********************************************}
procedure TALCustomTrack.TTrack.MakeBufDrawable;
begin
  //--- Do not create BufDrawable if not DoubleBuffered
  if {$IF not DEFINED(ALDPK)}(not DoubleBuffered){$ELSE}False{$ENDIF} then begin
    clearBufDrawable;
    exit;
  end;
  //--
  inherited MakeBufDrawable;
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if LStateStyle.Inherit then exit;
  if (not ALIsDrawableNull(LStateStyle.BufDrawable)) then exit;
  LStateStyle.SupersedeNoChanges(true{ASaveState});
  try

    // Create the BufDrawable
    CreateBufDrawable(
      LStateStyle.BufDrawable, // var ABufDrawable: TALDrawable;
      LStateStyle.BufDrawableRect, // var ABufDrawableRect: TRectF;
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
                        AFill, // const AFill: TALBrush;
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

      ALDrawRectangle(
        LCanvas, // const ACanvas: TALCanvas;
        AScale, // const AScale: Single;
        IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
        ABufDrawableRect, // const Rect: TrectF;
        1, // const AOpacity: Single;
        AFill, // const Fill: TALBrush;
        AStateLayer, // const StateLayer: TALStateLayer;
        AStateLayerContentColor, // const AStateLayerContentColor: TAlphaColor;
        ADrawStateLayerOnTop, // const ADrawStateLayerOnTop: Boolean;
        AStroke, // const Stroke: TALStrokeBrush;
        AShadow, // const Shadow: TALShadow
        Sides, // const Sides: TSides;
        Corners, // const Corners: TCorners;
        XRadius, // const XRadius: Single = 0;
        YRadius); // const YRadius: Single = 0);

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
        ALDrawCircle(
          LCanvas, // const ACanvas: TALCanvas;
          AScale, // const AScale: Single;
          IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
          LDstRect, // const ADstRect: TrectF;
          1, // const AOpacity: Single;
          AStopIndicator.Color, // const AFillColor: TAlphaColor;
          TGradientStyle.Linear, // const AFillGradientStyle: TGradientStyle;
          [], // const AFillGradientColors: TArray<TAlphaColor>;
          [], // const AFillGradientOffsets: TArray<Single>;
          TPointF.Zero, // const AFillGradientStartPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
          TPointF.Zero, // const AFillGradientEndPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
          AStopIndicator.ResourceName, // const AFillResourceName: String;
          AStopIndicator.WrapMode, // Const AFillWrapMode: TALImageWrapMode;
          TRectF.Empty, // Const AFillBackgroundMarginsRect: TRectF;
          TRectF.Empty, // Const AFillImageMarginsRect: TRectF;
          0, // const AStateLayerOpacity: Single;
          TAlphaColors.Null, // const AStateLayerColor: TAlphaColor;
          TRectF.Empty, // Const AStateLayerMarginsRect: TRectF;
          0, // const AStateLayerXRadius: Single;
          0, // const AStateLayerYRadius: Single;
          true, // const ADrawStateLayerOnTop: Boolean;
          TalphaColors.Null, // const AStrokeColor: TalphaColor;
          0, // const AStrokeThickness: Single;
          TalphaColors.Null, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, then the Canvas must have enough space to draw the shadow (approximately ShadowBlur on each side of the circle)
          0, // const AShadowBlur: Single;
          0, //const AShadowOffsetX: Single;
          0); // const AShadowOffsetY: Single);
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
    LDrawable := LStateStyle.BufDrawable;
    LDrawableRect := LStateStyle.BufDrawableRect;
    if ALIsDrawableNull(LDrawable) then begin
      LDrawable := BufDrawable;
      LDrawableRect := BufDrawableRect;
    end;
  end
  else begin
    LDrawable := BufDrawable;
    LDrawableRect := BufDrawableRect;
  end;

  if ALIsDrawableNull(LDrawable) then begin

    var LCurrentAdjustedStateStyle := TBaseStateStyle(StateStyles.GetCurrentAdjustedStyle);
    if LCurrentAdjustedStateStyle = nil then begin
      inherited Paint;
      exit;
    end;

    {$IF DEFINED(ALSkiaCanvas)}

    ALDrawRectangle(
      TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
      1, // const AScale: Single;
      IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
      LocalRect, // const Rect: TrectF;
      AbsoluteOpacity, // const AOpacity: Single;
      LCurrentAdjustedStateStyle.Fill, // const Fill: TALBrush;
      LCurrentAdjustedStateStyle.StateLayer, // const StateLayer: TALStateLayer;
      TalphaColors.Null, // const AStateLayerContentColor: TAlphaColor;
      True, // const ADrawStateLayerOnTop: Boolean;
      LCurrentAdjustedStateStyle.Stroke, // const Stroke: TALStrokeBrush;
      LCurrentAdjustedStateStyle.Shadow, // const Shadow: TALShadow
      Sides, // const Sides: TSides;
      Corners, // const Corners: TCorners;
      XRadius, // const XRadius: Single = 0;
      YRadius); // const YRadius: Single = 0);

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

{***********************************************************************************}
constructor TALCustomTrack.TInactiveTrack.Create(const ACustomTrack: TALCustomTrack);
begin
  inherited Create(ACustomTrack);
  //--
  var LFillChanged: TNotifyEvent := fill.OnChanged;
  fill.OnChanged := nil;
  Fill.DefaultColor := $ffc5c5c5;
  Fill.Color := Fill.DefaultColor;
  fill.OnChanged := LFillChanged;
  //--
  var LStrokeChanged: TNotifyEvent := stroke.OnChanged;
  stroke.OnChanged := Nil;
  Stroke.DefaultColor := TAlphaColors.Null;
  Stroke.Color := Stroke.DefaultColor;
  stroke.OnChanged := LStrokeChanged;
end;

{*******************************************************************}
function TALCustomTrack.TInactiveTrack.GetBufDrawableSrcRect: TRectF;
begin
  if ALIsDrawableNull(BufDrawable) then Exit(TRectF.Empty);
  Result := TRectF.Create(0, 0, ALGetDrawableWidth(BufDrawable), ALGetDrawableHeight(BufDrawable));
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

{*********************************************************************************}
constructor TALCustomTrack.TActiveTrack.Create(const ACustomTrack: TALCustomTrack);
begin
  inherited Create(ACustomTrack);
  //--
  var LFillChanged: TNotifyEvent := fill.OnChanged;
  fill.OnChanged := nil;
  Fill.DefaultColor := $ff167efc;
  Fill.Color := Fill.DefaultColor;
  fill.OnChanged := LFillChanged;
  //--
  var LStrokeChanged: TNotifyEvent := stroke.OnChanged;
  stroke.OnChanged := Nil;
  Stroke.DefaultColor := TAlphaColors.Null;
  Stroke.Color := Stroke.DefaultColor;
  stroke.OnChanged := LStrokeChanged;
end;

{*****************************************************************}
function TALCustomTrack.TActiveTrack.GetBufDrawableSrcRect: TRectF;
begin
  if ALIsDrawableNull(BufDrawable) then Exit(TRectF.Empty);
  Result := TRectF.Create(0, 0, ALGetDrawableWidth(BufDrawable), ALGetDrawableHeight(BufDrawable));
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
  fDoubleBuffered := True;
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
  if Orientation = TOrientation.Horizontal then Result.Align := TalignLayout.vertical
  else Result.Align := TalignLayout.horizontal;
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
  if Orientation = TOrientation.Horizontal then Result.Align := TalignLayout.vertical
  else Result.Align := TalignLayout.horizontal;
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
  if Orientation = TOrientation.Horizontal then Result.Align := TalignLayout.vertical
  else Result.Align := TalignLayout.horizontal;
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

{***************************************}
procedure TALCustomTrack.MakeBufDrawable;
begin
  if FInactiveTrack <> nil then FInactiveTrack.MakeBufDrawable;
  if FActiveTrack <> nil then FActiveTrack.MakeBufDrawable;
  if FThumb <> nil then FThumb.MakeBufDrawable;
end;

{****************************************}
procedure TALCustomTrack.clearBufDrawable;
begin
  if FInactiveTrack <> nil then FInactiveTrack.clearBufDrawable;
  if FActiveTrack <> nil then FActiveTrack.clearBufDrawable;
  if FThumb <> nil then FThumb.clearBufDrawable;
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
  if compareValue(Value, Min) < 0 then min := Value;
  FThumb.FValueRange.Max := Value;
  ClearBufDrawable;
end;

{*************************************}
function TALCustomTrack.GetMin: Double;
begin
  Result := FThumb.FValueRange.Min;
end;

{***************************************************}
procedure TALCustomTrack.SetMin(const Value: Double);
begin
  if compareValue(Value, Max) > 0 then max := Value;
  FThumb.FValueRange.Min := Value;
  ClearBufDrawable;
end;

{*******************************************}
function TALCustomTrack.GetFrequency: Double;
begin
  Result := FThumb.FValueRange.Frequency;
end;

{*********************************************************}
procedure TALCustomTrack.SetFrequency(const Value: Double);
begin
  FThumb.FValueRange.Frequency := Value;
  ClearBufDrawable;
end;

{***************************************}
function TALCustomTrack.GetValue: Double;
begin
  Result := FThumb.FValueRange.Value;
end;

{***********************************************}
procedure TALCustomTrack.SetValue(Value: Double);
begin
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
  FThumb.FValueRange.ViewportSize := Value;
end;

{*************************************************}
function TALCustomTrack.GetDoubleBuffered: boolean;
begin
  result := fDoubleBuffered;
end;

{****************************************************************}
procedure TALCustomTrack.SetDoubleBuffered(const AValue: Boolean);
begin
  fDoubleBuffered := AValue;
  if FInactiveTrack <> nil then FInactiveTrack.DoubleBuffered := AValue;
  if FActiveTrack <> nil then FActiveTrack.DoubleBuffered := AValue;
  if FThumb <> nil then FThumb.DoubleBuffered := AValue;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function SwapAlign(Const AAlign: TalignLayout): TalignLayout;
  Begin
    If AAlign = TalignLayout.Vertical then result := TalignLayout.Horizontal
    else If AAlign = TalignLayout.Horizontal then result := TalignLayout.Vertical
    else result := AAlign;
  End;

begin
  if FOrientation <> Value then begin
    FOrientation := Value;
    if not (csLoading in ComponentState) then begin
      BeginUpdate;
      Try
        SetBounds(Position.X, Position.Y, Size.Height, Size.Width);
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

{********************************************************************************}
procedure TALRangeTrackBar.TRangeMinInactiveTrack.SetXRadius(const Value: Single);
begin
  Inherited;
  var LMaxInactiveTrack := TALRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.SetXRadius(Value);
end;

{********************************************************************************}
procedure TALRangeTrackBar.TRangeMinInactiveTrack.SetYRadius(const Value: Single);
begin
  Inherited;
  var LMaxInactiveTrack := TALRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.SetYRadius(Value);
end;

{**********************************************************************************}
procedure TALRangeTrackBar.TRangeMinInactiveTrack.SetCorners(const Value: TCorners);
begin
  Inherited;
  var LMaxInactiveTrack := TALRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.SetCorners(Value);
end;

{***************************************************************}
procedure TALRangeTrackBar.TRangeMinInactiveTrack.MarginsChanged;
begin
  Inherited;
  var LMaxInactiveTrack := TALRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.Margins.Rect := Margins.Rect;
end;

{***************************************************************}
procedure TALRangeTrackBar.TRangeMinInactiveTrack.PaddingChanged;
begin
  Inherited;
  var LMaxInactiveTrack := TALRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.Padding.Rect := Padding.Rect;
end;

{**************************************************************************************}
procedure TALRangeTrackBar.TRangeMinInactiveTrack.StopIndicatorChanged(Sender: TObject);
begin
  Inherited;
  var LMaxInactiveTrack := TALRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.StopIndicator.Assign(StopIndicator);
end;

{*****************************************************************************}
procedure TALRangeTrackBar.TRangeMinInactiveTrack.FillChanged(Sender: TObject);
begin
  Inherited;
  var LMaxInactiveTrack := TALRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.Fill.Assign(Fill);
end;

{*******************************************************************************}
procedure TALRangeTrackBar.TRangeMinInactiveTrack.StrokeChanged(Sender: TObject);
begin
  Inherited;
  var LMaxInactiveTrack := TALRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.Stroke.Assign(Stroke);
end;

{*******************************************************************}
function TALRangeTrackBar.TRangeMinInactiveTrack._GetOpacity: Single;
begin
  Result := Inherited Opacity;
end;

{**********************************************************************************}
procedure TALRangeTrackBar.TRangeMinInactiveTrack._SetOpacity(const AValue: Single);
begin
  Inherited Opacity := AValue;
  var LMaxInactiveTrack := TALRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.Opacity := AValue;
end;

{*************************************************************************}
function TALRangeTrackBar.TRangeMinInactiveTrack._IsOpacityStored: boolean;
begin
  Result := not SameValue(FOpacity, 1);
end;

{*****************************************************************************}
function TALRangeTrackBar.TRangeMinInactiveTrack.GetBufDrawableSrcRect: TRectF;
begin
  if ALIsDrawableNull(BufDrawable) then Exit(TRectF.Empty);
  Result := TRectF.Create(0, 0, ALGetDrawableWidth(BufDrawable), ALGetDrawableHeight(BufDrawable));
  if FCustomTrack.Orientation = TOrientation.Horizontal then
    Result.Width := Width * Canvas.Scale
  else
    Result.Height := Height * Canvas.Scale;
  Result := ALAlignToPixelRound(Result, TMatrix.Identity, ALGetScreenScale{Scale}, TEpsilon.Position);
end;

{*****************************************************************************}
function TALRangeTrackBar.TRangeMaxInactiveTrack.GetBufDrawableSrcRect: TRectF;
begin
  if ALIsDrawableNull(BufDrawable) then Exit(TRectF.Empty);
  Result := TRectF.Create(0, 0, ALGetDrawableWidth(BufDrawable), ALGetDrawableHeight(BufDrawable));
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

{************************************************************************}
function TALRangeTrackBar.TRangeActiveTrack.GetBufDrawableSrcRect: TRectF;
begin
  if ALIsDrawableNull(BufDrawable) then Exit(TRectF.Empty);
  Result := TRectF.Create(0, 0, ALGetDrawableWidth(BufDrawable), ALGetDrawableHeight(BufDrawable));
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

{*************************************************************************************}
constructor TALRangeTrackBar.TRangeMinThumb.Create(const ACustomTrack: TALCustomTrack);
begin
  inherited;
  FFormerTouchTargetExpansionChangedHandler := TouchTargetExpansion.OnChange;
  TouchTargetExpansion.OnChange := TouchTargetExpansionChanged;
end;

{***********************************************************}
function TALRangeTrackBar.TRangeMinThumb._GetOpacity: Single;
begin
  Result := Inherited Opacity;
end;

{**************************************************************************}
procedure TALRangeTrackBar.TRangeMinThumb._SetOpacity(const AValue: Single);
begin
  Inherited Opacity := AValue;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Opacity := AValue;
end;

{***********************************************************}
function TALRangeTrackBar.TRangeMinThumb._GetCursor: TCursor;
begin
  Result := Inherited Cursor;
end;

{**************************************************************************}
procedure TALRangeTrackBar.TRangeMinThumb._SetCursor(const AValue: TCursor);
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Cursor := AValue;
end;

{*****************************************************************}
function TALRangeTrackBar.TRangeMinThumb._IsOpacityStored: boolean;
begin
  Result := not SameValue(FOpacity, 1);
end;

{************************************************************************}
procedure TALRangeTrackBar.TRangeMinThumb.SetXRadius(const Value: Single);
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.XRadius := Value;
end;

{************************************************************************}
procedure TALRangeTrackBar.TRangeMinThumb.SetYRadius(const Value: Single);
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.YRadius := Value;
end;

{**************************************************************************}
procedure TALRangeTrackBar.TRangeMinThumb.SetCorners(const Value: TCorners);
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Corners := Value;
end;

{*******************************************************}
procedure TALRangeTrackBar.TRangeMinThumb.MarginsChanged;
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Margins.rect := Margins.rect;
end;

{*******************************************************}
procedure TALRangeTrackBar.TRangeMinThumb.PaddingChanged;
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Padding.rect := Padding.rect;
end;

{*********************************************************************}
procedure TALRangeTrackBar.TRangeMinThumb.FillChanged(Sender: TObject);
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Fill.Assign(Fill);
end;

{***********************************************************************}
procedure TALRangeTrackBar.TRangeMinThumb.StrokeChanged(Sender: TObject);
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Stroke.Assign(Stroke);
end;

{***********************************************************************}
procedure TALRangeTrackBar.TRangeMinThumb.ShadowChanged(Sender: TObject);
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Shadow.Assign(Shadow);
end;

{****************************************************************************}
procedure TALRangeTrackBar.TRangeMinThumb.StateStylesChanged(Sender: TObject);
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.StateStyles.Assign(StateStyles);
end;

{*************************************************************************************}
procedure TALRangeTrackBar.TRangeMinThumb.TouchTargetExpansionChanged(Sender: TObject);
begin
  Inherited;
  if Assigned(FFormerTouchTargetExpansionChangedHandler) then
    FFormerTouchTargetExpansionChangedHandler(Sender);
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.TouchTargetExpansion.assign(TouchTargetExpansion);
end;

{**************************************************}
procedure TALRangeTrackBar.TRangeMinThumb.DoResized;
begin
  Inherited;
  var LMaxThumb := TALRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.size.size := size.size;
end;

{*************************************************************************************}
constructor TALRangeTrackBar.TRangeMaxThumb.Create(const ACustomTrack: TALCustomTrack);
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
  FMaxInactiveTrack := CreateInactiveTrack(TRangeMaxInactiveTrack, 'MaxInactiveTrack');
  FMaxThumb := CreateThumb(TRangeMaxThumb, 'MaxThumb');
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
procedure TALRangeTrackBar.clearBufDrawable;
begin
  inherited;
  if FMaxInactiveTrack <> nil then FMaxInactiveTrack.clearBufDrawable;
  if FMaxThumb <> nil then FMaxThumb.clearBufDrawable;
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
  inherited;
  FMaxThumb.FValueRange.Max := Value;
end;

{*****************************************************}
procedure TALRangeTrackBar.SetMin(const Value: Double);
begin
  inherited;
  FMaxThumb.FValueRange.Min := Value;
end;

{***********************************************************}
procedure TALRangeTrackBar.SetFrequency(const Value: Double);
begin
  inherited;
  FMaxThumb.FValueRange.Frequency := Value;
end;

{*************************************************}
procedure TALRangeTrackBar.SetValue(Value: Double);
begin
  inherited;
  if (not fThumb.Pressed) and
     (GetValue > (max - Min) / 2) then fThumb.BringToFront;
end;

{********************************************}
function TALRangeTrackBar.GetMaxValue: Double;
begin
  Result := FMaxThumb.FValueRange.Value;
end;

{****************************************************}
procedure TALRangeTrackBar.SetMaxValue(Value: Double);
begin
  FMaxThumb.FValueRange.Value := Value;
  if (not fMaxThumb.Pressed) and
     (GetMaxValue < (max - Min) / 2) then fMaxThumb.BringToFront;
end;

{**************************************************************}
procedure TALRangeTrackBar.SetViewportSize(const Value: Double);
begin
  inherited;
  FMaxThumb.FValueRange.ViewportSize := Value;
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
  if AInactiveTrackClass = nil then Exit(CreateInactiveTrack(TRangeMinInactiveTrack, AName));
  result := Inherited;
end;

{*****************************************************************************************************************************************************************************}
function TALRangeTrackBar.CreateActiveTrack(const AActiveTrackClass: TALCustomTrack.TActiveTrackClass = nil; Const AName: String = 'ActiveTrack'): TALCustomTrack.TActiveTrack;
begin
  if AActiveTrackClass = nil then Exit(CreateActiveTrack(TRangeActiveTrack, AName));
  result := Inherited;
end;

{***********************************************************************************************************************************************}
function TALRangeTrackBar.CreateThumb(const AThumbClass: TALCustomTrack.TThumbClass = nil; Const AName: String = 'Thumb'): TALCustomTrack.TThumb;
begin
  if AThumbClass = nil then Exit(CreateThumb(TRangeMinThumb, AName));
  result := Inherited;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function SwapAlign(Const AAlign: TalignLayout): TalignLayout;
  Begin
    If AAlign = TalignLayout.Vertical then result := TalignLayout.Horizontal
    else If AAlign = TalignLayout.Horizontal then result := TalignLayout.Vertical
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

{**************************************************}
constructor TALScrollBar.Create(AOwner: TComponent);
begin
  inherited;
  CanFocus := False;
  //--
  var LFillChanged: TNotifyEvent := FThumb.fill.OnChanged;
  FThumb.fill.OnChanged := nil;
  FThumb.Fill.DefaultColor := $47000000;
  FThumb.Fill.Color := FThumb.Fill.DefaultColor;
  FThumb.fill.OnChanged := LFillChanged;
  //--
  var LStrokeChanged: TNotifyEvent := FThumb.stroke.OnChanged;
  FThumb.stroke.OnChanged := Nil;
  FThumb.Stroke.DefaultColor := Talphacolors.Null;
  FThumb.Stroke.Color := FThumb.Stroke.DefaultColor;
  FThumb.stroke.OnChanged := LStrokeChanged;
  //--
  FThumb.DefaultXRadius := 0;
  FThumb.DefaultYRadius := 0;
  FThumb.XRadius := FThumb.DefaultXRadius;
  FThumb.YRadius := FThumb.DefaultYRadius;
  FThumb.StateStyles.Disabled.StateLayer.DefaultXRadius := FThumb.DefaultXRadius;
  FThumb.StateStyles.Disabled.StateLayer.DefaultYRadius := FThumb.DefaultYRadius;
  FThumb.StateStyles.Hovered.StateLayer.DefaultXRadius := FThumb.DefaultXRadius;
  FThumb.StateStyles.Hovered.StateLayer.DefaultYRadius := FThumb.DefaultYRadius;
  FThumb.StateStyles.Pressed.StateLayer.DefaultXRadius := FThumb.DefaultXRadius;
  FThumb.StateStyles.Pressed.StateLayer.DefaultYRadius := FThumb.DefaultYRadius;
  FThumb.StateStyles.Focused.StateLayer.DefaultXRadius := FThumb.DefaultXRadius;
  FThumb.StateStyles.Focused.StateLayer.DefaultYRadius := FThumb.DefaultYRadius;
  FThumb.StateStyles.Disabled.StateLayer.XRadius := FThumb.DefaultXRadius;
  FThumb.StateStyles.Disabled.StateLayer.YRadius := FThumb.DefaultYRadius;
  FThumb.StateStyles.Hovered.StateLayer.XRadius := FThumb.DefaultXRadius;
  FThumb.StateStyles.Hovered.StateLayer.YRadius := FThumb.DefaultYRadius;
  FThumb.StateStyles.Pressed.StateLayer.XRadius := FThumb.DefaultXRadius;
  FThumb.StateStyles.Pressed.StateLayer.YRadius := FThumb.DefaultYRadius;
  FThumb.StateStyles.Focused.StateLayer.XRadius := FThumb.DefaultXRadius;
  FThumb.StateStyles.Focused.StateLayer.YRadius := FThumb.DefaultYRadius;
end;

{*******************************************}
function TALScrollBar.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(150, 18);
end;

{********************************}
procedure TALScrollBar.AlignThumb;
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

{***********************************************************************************************************************************************************************************}
function TALScrollBar.CreateInactiveTrack(const AInactiveTrackClass: TALCustomTrack.TInactiveTrackClass = nil; Const AName: String = 'InactiveTrack'): TALCustomTrack.TInactiveTrack;
begin
  Result := Nil;
end;

{*************************************************************************************************************************************************************************}
function TALScrollBar.CreateActiveTrack(const AActiveTrackClass: TALCustomTrack.TActiveTrackClass = nil; Const AName: String = 'ActiveTrack'): TALCustomTrack.TActiveTrack;
begin
  Result := Nil;
end;

{****************************************************************************************************************************************************************************************}
function TALScrollBar.CreateValueIndicator(const AValueIndicatorClass: TALCustomTrack.TValueIndicatorClass = nil; Const AName: String = 'ValueIndicator'): TALCustomTrack.TValueIndicator;
begin
  Result := Nil;
end;

{***********************************************************************************}
constructor TALBaseCheckBox.TCheckMarkBrush.Create(const ADefaultColor: TAlphaColor);
begin
  inherited Create;
  //--
  FDefaultColor := ADefaultColor;
  FDefaultResourceName := '';
  FDefaultWrapMode := TALImageWrapMode.Fit;
  FDefaultThickness := 2;
  //--
  FColor := FDefaultColor;
  FResourceName := FDefaultResourceName;
  FWrapMode := FDefaultWrapMode;
  FThickness := FDefaultThickness;
  //--
  FMargins := TBounds.Create(TRectF.Create(3,3,3,3));
  FMargins.OnChange := MarginsChanged;
end;

{*************************************************}
destructor TALBaseCheckBox.TCheckMarkBrush.Destroy;
begin
  ALFreeAndNil(FMargins);
  inherited;
end;

{*******************************************************************************}
function TALBaseCheckBox.TCheckMarkBrush.CreateSavedState: TALPersistentObserver;
type
  TCheckMarkBrushClass = class of TCheckMarkBrush;
begin
  result := TCheckMarkBrushClass(classtype).Create(DefaultColor);
end;

{********************************************************************}
procedure TALBaseCheckBox.TCheckMarkBrush.Assign(Source: TPersistent);
begin
  if Source is TCheckMarkBrush then begin
    BeginUpdate;
    Try
      Color := TCheckMarkBrush(Source).Color;
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

{***************************************************************************************************************}
procedure TALBaseCheckBox.TCheckMarkBrush.Interpolate(const ATo: TCheckMarkBrush; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    if ATo <> nil then begin
      Color := ALInterpolateColor(Color{Start}, ATo.Color{Stop}, ANormalizedTime);
      ResourceName := ATo.ResourceName;
      WrapMode := ATo.WrapMode;
      Thickness := InterpolateSingle(Thickness{Start}, ATo.Thickness{Stop}, ANormalizedTime);
      Margins.Left := InterpolateSingle(Margins.Left{Start}, ATo.Margins.Left{Stop}, ANormalizedTime);
      Margins.Right := InterpolateSingle(Margins.Right{Start}, ATo.Margins.Right{Stop}, ANormalizedTime);
      Margins.Top := InterpolateSingle(Margins.Top{Start}, ATo.Margins.Top{Stop}, ANormalizedTime);
      Margins.Bottom := InterpolateSingle(Margins.Bottom{Start}, ATo.Margins.Bottom{Stop}, ANormalizedTime);
    end
    else begin
      Color := ALInterpolateColor(Color{Start}, DefaultColor{Stop}, ANormalizedTime);
      ResourceName := DefaultResourceName;
      WrapMode := DefaultWrapMode;
      Thickness := InterpolateSingle(Thickness{Start}, DefaultThickness{Stop}, ANormalizedTime);
      Margins.Left := InterpolateSingle(Margins.Left{Start}, Margins.DefaultValue.Left{Stop}, ANormalizedTime);
      Margins.Right := InterpolateSingle(Margins.Right{Start}, Margins.DefaultValue.Right{Stop}, ANormalizedTime);
      Margins.Top := InterpolateSingle(Margins.Top{Start}, Margins.DefaultValue.Top{Stop}, ANormalizedTime);
      Margins.Bottom := InterpolateSingle(Margins.Bottom{Start}, Margins.DefaultValue.Bottom{Stop}, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{************************************************************************************************************************}
procedure TALBaseCheckBox.TCheckMarkBrush.InterpolateNoChanges(const ATo: TCheckMarkBrush; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
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
  result := FColor <> FDefaultColor;
end;

{*********************************************************************}
function TALBaseCheckBox.TCheckMarkBrush.IsResourceNameStored: Boolean;
begin
  result := FResourceName <> FDefaultResourceName;
end;

{*****************************************************************}
function TALBaseCheckBox.TCheckMarkBrush.IsWrapModeStored: Boolean;
begin
  result := FWrapMode <> FDefaultWrapMode;
end;

{******************************************************************}
function TALBaseCheckBox.TCheckMarkBrush.IsThicknessStored: Boolean;
begin
  result := not SameValue(FThickness, FDefaultThickness, TEpsilon.Vector);
end;

{***************************************************************************}
procedure TALBaseCheckBox.TCheckMarkBrush.SetColor(const Value: TAlphaColor);
begin
  if fColor <> Value then begin
    fColor := Value;
    Change;
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

{*************************************************************************}
procedure TALBaseCheckBox.TCheckMarkBrush.SetMargins(const Value: TBounds);
begin
  FMargins.Assign(Value);
end;

{************************************************************************}
procedure TALBaseCheckBox.TCheckMarkBrush.MarginsChanged(Sender: TObject);
begin
  change;
end;

{**************************************************************************************************************************}
constructor TALBaseCheckBox.TInheritCheckMarkBrush.Create(const AParent: TCheckMarkBrush; const ADefaultColor: TAlphaColor);
begin
  inherited create(ADefaultColor);
  FParent := AParent;
  FInherit := True;
  fSuperseded := False;
end;

{**************************************************************************************}
function TALBaseCheckBox.TInheritCheckMarkBrush.CreateSavedState: TALPersistentObserver;
type
  TInheritCheckMarkBrushClass = class of TInheritCheckMarkBrush;
begin
  result := TInheritCheckMarkBrushClass(classtype).Create(nil{AParent}, DefaultColor);
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

{*************************************************************************}
constructor TALBaseCheckBox.TBaseStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  //--
  if StateStyleParent <> nil then FCheckMark := TInheritCheckMarkBrush.Create(StateStyleParent.CheckMark, TAlphaColors.Black{ADefaultColor})
  else if ControlParent <> nil then FCheckMark := TInheritCheckMarkBrush.Create(ControlParent.CheckMark, TAlphaColors.Black{ADefaultColor})
  else FCheckMark := TInheritCheckMarkBrush.Create(nil, TAlphaColors.Black{ADefaultColor});
  FCheckMark.OnChanged := CheckMarkChanged;
  //--
  StateLayer.Margins.DefaultValue := TRectF.Create(-12,-12,-12,-12);
  StateLayer.Margins.Rect := StateLayer.Margins.DefaultValue;
  //--
  StateLayer.DefaultXRadius := -50;
  StateLayer.DefaultYRadius := -50;
  StateLayer.XRadius := StateLayer.DefaultXRadius;
  StateLayer.YRadius := StateLayer.DefaultYRadius;
end;

{*************************************************}
destructor TALBaseCheckBox.TBaseStateStyle.Destroy;
begin
  ALFreeAndNil(FCheckMark);
  inherited Destroy;
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

{*****************************************************************************************************************}
procedure TALBaseCheckBox.TBaseStateStyle.Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single);
begin
  {$IF defined(debug)}
  if (ATo <> nil) and (not (ATo is TBaseStateStyle)) then
    Raise Exception.Create('Error F3C72244-894F-4B67-AD86-F24DF5039927');
  {$ENDIF}
  BeginUpdate;
  Try
    inherited Interpolate(ATo, ANormalizedTime);
    if ATo <> nil then CheckMark.Interpolate(TBaseStateStyle(ATo).CheckMark, ANormalizedTime)
    else if StateStyleParent <> nil then begin
      StateStyleParent.SupersedeNoChanges(true{ASaveState});
      try
        CheckMark.Interpolate(StateStyleParent.CheckMark, ANormalizedTime)
      finally
        StateStyleParent.RestoreStateNoChanges;
      end;
    end
    else if ControlParent <> nil then CheckMark.Interpolate(ControlParent.CheckMark, ANormalizedTime)
    else CheckMark.Interpolate(nil, ANormalizedTime);
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

{***********************************************************************************}
constructor TALBaseCheckBox.TCheckStateStyles.Create(const AParent: TALBaseCheckBox);
begin
  inherited Create;
  //--
  FDefault := TDefaultStateStyle.Create(AParent);
  FDefault.OnChanged := DefaultChanged;
  //--
  FDisabled := TDisabledStateStyle.Create(FDefault);
  FDisabled.OnChanged := DisabledChanged;
  //--
  FHovered := THoveredStateStyle.Create(FDefault);
  FHovered.OnChanged := HoveredChanged;
  //--
  FPressed := TPressedStateStyle.Create(FDefault);
  FPressed.OnChanged := PressedChanged;
  //--
  FFocused := TFocusedStateStyle.Create(FDefault);
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

{******************************************************************************}
constructor TALBaseCheckBox.TStateStyles.Create(const AParent: TALBaseCheckBox);
begin
  inherited Create(AParent);
  //--
  FChecked := TCheckStateStyles.Create(AParent);
  FChecked.OnChanged := CheckedChanged;
  //--
  FUnchecked := TCheckStateStyles.Create(AParent);
  FUnchecked.OnChanged := UncheckedChanged;
end;

{**********************************************}
destructor TALBaseCheckBox.TStateStyles.Destroy;
begin
  ALFreeAndNil(FChecked);
  ALFreeAndNil(FUnchecked);
  inherited Destroy;
end;

{****************************************************************************}
function TALBaseCheckBox.TStateStyles.CreateSavedState: TALPersistentObserver;
type
  TStateStylesClass = class of TStateStyles;
begin
  result := TStateStylesClass(classtype).Create(nil{AParent});
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
  var LFillChanged: TNotifyEvent := fill.OnChanged;
  fill.OnChanged := nil;
  Fill.DefaultColor := TAlphaColors.White;
  Fill.Color := Fill.DefaultColor;
  fill.OnChanged := LFillChanged;
  //--
  var LStrokeChanged: TNotifyEvent := stroke.OnChanged;
  stroke.OnChanged := Nil;
  Stroke.DefaultColor := TAlphaColors.Black;
  Stroke.Color := Stroke.DefaultColor;
  stroke.OnChanged := LStrokeChanged;
  //--
  FCheckMark := TCheckMarkBrush.Create(TAlphaColors.Black);
  FCheckMark.OnChanged := CheckMarkChanged;
  //--
  FChecked := False;
  FDoubleBuffered := true;
  FDefaultXRadius := 0;
  FDefaultYRadius := 0;
  FXRadius := FDefaultXRadius;
  FYRadius := FDefaultYRadius;
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
  inherited;
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

{*******************************************************}
function TALBaseCheckBox.CreateStateStyles: TStateStyles;
begin
  Result := TStateStyles.Create(self);
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
    if not fDoubleBuffered then clearBufDrawable;
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
  Result := not SameValue(FXRadius, FDefaultXRadius, TEpsilon.Vector);
end;

{************************************************}
function TALBaseCheckBox.IsYRadiusStored: Boolean;
begin
  Result := not SameValue(FYRadius, FDefaultYRadius, TEpsilon.Vector);
end;

{********************************************************}
procedure TALBaseCheckBox.SetXRadius(const Value: Single);
var
  NewValue: Single;
begin
  if csDesigning in ComponentState then NewValue := Max(-50, Min(Value, Min(Width / 2, Height / 2)))
  else NewValue := Value;
  if not SameValue(FXRadius, NewValue, TEpsilon.Vector) then begin
    clearBufDrawable;
    FXRadius := NewValue;
    Repaint;
  end;
end;

{********************************************************}
procedure TALBaseCheckBox.SetYRadius(const Value: Single);
var
  NewValue: Single;
begin
  if csDesigning in ComponentState then NewValue := Max(-50, Min(Value, Min(Width / 2, Height / 2)))
  else NewValue := Value;
  if not SameValue(FYRadius, NewValue, TEpsilon.Vector) then begin
    clearBufDrawable;
    FYRadius := NewValue;
    Repaint;
  end;
end;

{**********************************************************}
procedure TALBaseCheckBox.CheckMarkChanged(Sender: TObject);
begin
  clearBufDrawable;
  Repaint;
end;

{*****************************************************}
procedure TALBaseCheckBox.FillChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{*******************************************************}
procedure TALBaseCheckBox.StrokeChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{*******************************************************}
procedure TALBaseCheckBox.ShadowChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{************************************************************}
procedure TALBaseCheckBox.StateStylesChanged(Sender: TObject);
begin
  clearBufDrawable;
  if Checked then DisabledOpacity := StateStyles.Checked.Disabled.opacity
  else DisabledOpacity := StateStyles.Unchecked.Disabled.opacity;
  Repaint;
end;

{*******************************************}
procedure TALBaseCheckBox.IsMouseOverChanged;
begin
  inherited;
  StateStyles.startTransition;
  repaint;
end;

{*****************************************}
procedure TALBaseCheckBox.IsFocusedChanged;
begin
  inherited;
  StateStyles.startTransition;
  repaint;
end;

{***************************************}
procedure TALBaseCheckBox.PressedChanged;
begin
  inherited;
  StateStyles.startTransition;
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

{******************************}
procedure TALBaseCheckBox.Click;
begin
  Checked := not Checked;
  inherited;
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
procedure TALBaseCheckBox.clearBufDrawable;
begin
  {$IFDEF debug}
  if (FStateStyles <> nil) and
     (not (csDestroying in ComponentState)) and
     ((not ALIsDrawableNull(FStateStyles.Checked.Default.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Disabled.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Hovered.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Pressed.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Focused.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Default.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Disabled.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Hovered.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Pressed.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Focused.BufDrawable))) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  if FStateStyles <> nil then
    FStateStyles.ClearBufDrawable;
end;

{****************************************}
procedure TALBaseCheckBox.MakeBufDrawable;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _DoMakeBufDrawable(const AStateStyle: TBaseStateStyle): boolean;
  begin
    if (not ALIsDrawableNull(AStateStyle.BufDrawable)) then exit(False);
    AStateStyle.SupersedeNoChanges(true{ASaveState});
    try
      CreateBufDrawable(
        AStateStyle.BufDrawable, // var ABufDrawable: TALDrawable;
        AStateStyle.BufDrawableRect, // var ABufDrawableRect: TRectF;
        ALGetScreenScale * AStateStyle.Scale, // const AScale: Single;
        AStateStyle.Fill, // const AFill: TALBrush;
        AStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
        AStateStyle.Stroke, // const AStroke: TALStrokeBrush;
        AStateStyle.CheckMark, // const ACheckMark: TCheckMarkBrush;
        AStateStyle.Shadow); // const AShadow: TALShadow);

      // LStateStyle.BufDrawableRect must include the LScale
      AStateStyle.BufDrawableRect.Top := AStateStyle.BufDrawableRect.Top * AStateStyle.Scale;
      AStateStyle.BufDrawableRect.right := AStateStyle.BufDrawableRect.right * AStateStyle.Scale;
      AStateStyle.BufDrawableRect.left := AStateStyle.BufDrawableRect.left * AStateStyle.Scale;
      AStateStyle.BufDrawableRect.bottom := AStateStyle.BufDrawableRect.bottom * AStateStyle.Scale;

      // Since LStateStyle.BufDrawableRect can have different dimensions than the main BufDrawableRect (LocalRect)
      // (due to scale), we must center LStateStyle.BufDrawableRect within the main BufDrawableRect (LocalRect)
      // to ensure that all changes are visually centered.
      var LCenteredRect := AStateStyle.BufDrawableRect.CenterAt(LocalRect);
      AStateStyle.BufDrawableRect.Offset(LCenteredRect.Left, LCenteredRect.top);

    finally
      AStateStyle.RestorestateNoChanges;
    end;
    Result := True;
  end;

begin
  //--- Do not create BufDrawable if not DoubleBuffered
  if {$IF not DEFINED(ALDPK)}(not DoubleBuffered){$ELSE}False{$ENDIF} then begin
    clearBufDrawable;
    exit;
  end;
  //--
  if Checked then _DoMakeBufDrawable(StateStyles.Checked.Default)
  else _DoMakeBufDrawable(StateStyles.UnChecked.Default);
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if LStateStyle.Inherit then exit;
  _DoMakeBufDrawable(LStateStyle);
  // No need to center LStateStyle.BufDrawableRect on the main BufDrawableRect
  // because BufDrawableRect always has the width and height of the localRect.
end;

{**************************************}
procedure TALBaseCheckBox.DrawCheckMark(
            const ACanvas: TALCanvas;
            const AScale: Single;
            const ADstRect: TrectF;
            const AChecked: Boolean;
            const ACheckMark: TCheckMarkBrush);
begin

  var LCanvasMatrix: TMatrix;
  var LCanvasScale: Single;
  if IsPixelAlignmentEnabled then ALExtractMatrixFromCanvas(Acanvas, LCanvasMatrix, LCanvasScale)
  else begin
    LCanvasMatrix := TMatrix.Identity;
    LCanvasScale := 1;
  end;
  var LRect := ADstRect;
  LRect.Top := LRect.Top * AScale;
  LRect.right := LRect.right * AScale;
  LRect.left := LRect.left * AScale;
  LRect.bottom := LRect.bottom * AScale;
  if IsPixelAlignmentEnabled then
    LRect := ALAlignToPixelRound(LRect, LCanvasMatrix, LCanvasScale, TEpsilon.Position);
  var LScaledMarginsRect := ACheckMark.Margins.Rect;
  LScaledMarginsRect.Left := LScaledMarginsRect.Left * AScale;
  LScaledMarginsRect.right := LScaledMarginsRect.right * AScale;
  LScaledMarginsRect.top := LScaledMarginsRect.top * AScale;
  LScaledMarginsRect.bottom := LScaledMarginsRect.bottom * AScale;
  if IsPixelAlignmentEnabled then
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
    if IsPixelAlignmentEnabled then
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

    ALDrawRectangle(
      ACanvas, // const ACanvas: TALCanvas;
      1, // const AScale: Single;
      IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
      LRect, // const ADstRect: TrectF;
      1, // const AOpacity: Single;
      ACheckMark.Color, // const AFillColor: TAlphaColor;
      TGradientStyle.Linear, // const AFillGradientStyle: TGradientStyle;
      [], // const AFillGradientColors: TArray<TAlphaColor>;
      [], // const AFillGradientOffsets: TArray<Single>;
      TPointF.Zero, // const AFillGradientStartPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
      TPointF.Zero, // const AFillGradientEndPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
      ACheckMark.ResourceName, // const AFillResourceName: String;
      ACheckMark.WrapMode, // Const AFillWrapMode: TALImageWrapMode;
      TRectF.Empty, // Const AFillBackgroundMarginsRect: TRectF;
      TRectF.Empty, // Const AFillImageMarginsRect: TRectF;
      0, // const AStateLayerOpacity: Single;
      TAlphaColors.Null, // const AStateLayerColor: TAlphaColor;
      TRectF.Empty, // Const AStateLayerMarginsRect: TRectF;
      0, // const AStateLayerXRadius: Single;
      0, // const AStateLayerYRadius: Single;
      False, // const ADrawStateLayerOnTop: Boolean;
      TAlphaColors.Null, // const AStrokeColor: TalphaColor;
      0, // const AStrokeThickness: Single;
      TAlphaColors.Null, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, the Canvas should have adequate space to accommodate the shadow. You can use the ALGetShadowWidth function to estimate the required width.
      0, // const AShadowBlur: Single;
      0, // const AShadowOffsetX: Single;
      0, // const AShadowOffsetY: Single;
      AllSides, // const ASides: TSides;
      AllCorners, // const ACorners: TCorners;
      0, // const AXRadius: Single;
      0); // const AYRadius: Single)

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
                        AFill, // const AFill: TALBrush;
                        AStateLayer, // const AStateLayer: TALStateLayer;
                        AShadow); // const AShadow: TALShadow): TRectF;
  if ACheckMark.HasCheckMark then begin
    var LCheckMarkRect := ABufDrawableRect;
    LCheckMarkRect.Inflate(-ACheckMark.margins.Left, -ACheckMark.margins.top, -ACheckMark.margins.right, -ACheckMark.margins.Bottom);
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

      ALDrawRectangle(
        LCanvas, // const ACanvas: TALCanvas;
        AScale, // const AScale: Single;
        IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
        ABufDrawableRect, // const Rect: TrectF;
        1, // const AOpacity: Single;
        AFill, // const Fill: TALBrush;
        AStateLayer, // const StateLayer: TALStateLayer;
        ACheckMark.Color, // const AStateLayerContentColor: TAlphaColor;
        False, // const ADrawStateLayerOnTop: Boolean;
        AStroke, // const Stroke: TALStrokeBrush;
        AShadow, // const Shadow: TALShadow
        AllSides, // const Sides: TSides;
        AllCorners, // const Corners: TCorners;
        XRadius, // const XRadius: Single = 0;
        YRadius); // const YRadius: Single = 0);

      DrawCheckMark(
        LCanvas, // const ACanvas: TALCanvas;
        AScale, // const AScale: Single;
        ABufDrawableRect, // const ADstRect: TrectF;
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

{******************************}
procedure TALBaseCheckBox.Paint;
begin

  StateStyles.UpdateLastPaintedRawStyle;
  MakeBufDrawable;

  var LDrawable: TALDrawable;
  var LDrawableRect: TRectF;
  if StateStyles.IsTransitionAnimationRunning then begin
    LDrawable := ALNullDrawable;
    LDrawableRect := TRectF.Empty;
  end
  else begin
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      LDrawable := LStateStyle.BufDrawable;
      LDrawableRect := LStateStyle.BufDrawableRect;
      if ALIsDrawableNull(LDrawable) then begin
        if checked then begin
          LDrawable := StateStyles.Checked.default.BufDrawable;
          LDrawableRect := StateStyles.Checked.default.BufDrawableRect;
        end
        else begin
          LDrawable := StateStyles.UnChecked.default.BufDrawable;
          LDrawableRect := StateStyles.UnChecked.default.BufDrawableRect;
        end;
      end;
    end
    else
      raise Exception.Create('Error #EA9B4064-F1D2-4E04-82FE-99FD3ED8B1F3');
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
                                                LCurrentAdjustedStateStyle.Scale, // Const AScaleX: Single;
                                                LCurrentAdjustedStateStyle.Scale, // Const AScaleY: Single;
                                                true); // Const ASaveState: Boolean);
    try

      var LRect := LocalRect;

      if compareValue(AbsoluteOpacity, 1, Tepsilon.Scale) < 0 then begin
        var LLayerRect := ALGetShapeSurfaceRect(
                            LRect, // const ARect: TrectF;
                            LCurrentAdjustedStateStyle.Fill.Color, // const AFillColor: TAlphaColor;
                            LCurrentAdjustedStateStyle.Fill.Gradient.Colors, // const AFillGradientColors: TArray<TAlphaColor>;
                            LCurrentAdjustedStateStyle.Fill.ResourceName, // const AFillResourceName: String;
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

        ALDrawRectangle(
          TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
          1, // const AScale: Single;
          IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
          LRect, // const Rect: TrectF;
          1, // const AOpacity: Single;
          LCurrentAdjustedStateStyle.Fill, // const Fill: TALBrush;
          LCurrentAdjustedStateStyle.StateLayer, // const StateLayer: TALStateLayer;
          LCurrentAdjustedStateStyle.CheckMark.Color, // const AStateLayerContentColor: TAlphaColor;
          False, // const ADrawStateLayerOnTop: Boolean;
          LCurrentAdjustedStateStyle.Stroke, // const Stroke: TALStrokeBrush;
          LCurrentAdjustedStateStyle.Shadow, // const Shadow: TALShadow
          AllSides, // const Sides: TSides;
          AllCorners, // const Corners: TCorners;
          XRadius, // const XRadius: Single = 0;
          YRadius); // const YRadius: Single = 0);

        DrawCheckMark(
          TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
          1, // const AScale: Single;
          LRect, // const ADstRect: TrectF;
          // Check CheckMark.Color = TalphaColors.Null to enable an interpolation fade-out effect on the checkMark.
          // Without this, the checkMark disappears immediately.
          Checked or (TBaseStateStyle(StateStyles.GetCurrentRawStyle).CheckMark.Color = TalphaColors.Null), // const AChecked: Boolean
          LCurrentAdjustedStateStyle.CheckMark); // const ACheckMark: TCheckMarkBrush;

      finally
        if compareValue(AbsoluteOpacity, 1, Tepsilon.Scale) < 0 then
          ALEndTransparencyLayer(TSkCanvasCustom(Canvas).Canvas.Handle);
      end;

    finally
      if LCanvasSaveState <> nil then
        Canvas.RestoreState(LCanvasSaveState);
    end;

    {$ELSE}

    if StateStyles.IsTransitionAnimationRunning then begin

      var LRect := LocalRect;
      var LBufSurface: TALSurface;
      var LBufCanvas: TALCanvas;
      var LBufDrawable: TALDrawable;
      StateStyles.GetTransitionBufSurface(
        LRect, // var ARect: TrectF;
        ALGetScreenScale, // const AScale: Single;
        LBufSurface, // out ABufSurface: TALSurface;
        LBufCanvas, // out ABufCanvas: TALCanvas;
        LBufDrawable); // out ABufDrawable: TALDrawable);

      if ALCanvasBeginScene(LBufCanvas) then
      try

        ALClearCanvas(LBufCanvas, TAlphaColors.Null);

        ALDrawRectangle(
          LBufCanvas, // const ACanvas: TALCanvas;
          ALGetScreenScale, // const AScale: Single;
          IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
          LRect, // const Rect: TrectF;
          1, // const AOpacity: Single;
          LCurrentAdjustedStateStyle.Fill, // const Fill: TALBrush;
          LCurrentAdjustedStateStyle.StateLayer, // const StateLayer: TALStateLayer;
          LCurrentAdjustedStateStyle.CheckMark.Color, // const AStateLayerContentColor: TAlphaColor;
          False, // const ADrawStateLayerOnTop: Boolean;
          LCurrentAdjustedStateStyle.Stroke, // const Stroke: TALStrokeBrush;
          LCurrentAdjustedStateStyle.Shadow, // const Shadow: TALShadow
          AllSides, // const Sides: TSides;
          AllCorners, // const Corners: TCorners;
          XRadius, // const XRadius: Single = 0;
          YRadius); // const YRadius: Single = 0);

        DrawCheckMark(
          LBufCanvas, // const ACanvas: TALCanvas;
          ALGetScreenScale, // const AScale: Single;
          LRect, // const ADstRect: TrectF;
          // Check CheckMark.Color = TalphaColors.Null to enable an interpolation fade-out effect on the checkMark.
          // Without this, the checkMark disappears immediately.
          Checked or (TBaseStateStyle(StateStyles.GetCurrentRawStyle).CheckMark.Color = TalphaColors.Null), // const AChecked: Boolean
          LCurrentAdjustedStateStyle.CheckMark); // const ACheckMark: TCheckMarkBrush;

      finally
        ALCanvasEndScene(LBufCanvas)
      end;

      ALUpdateDrawableFromSurface(LBufSurface, LBufDrawable);

      // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
      // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
      LRect.Offset(-2*LRect.Left, -2*LRect.Top);

      // LRect must include the LScale
      LRect.Top := LRect.Top * LCurrentAdjustedStateStyle.Scale;
      LRect.right := LRect.right * LCurrentAdjustedStateStyle.Scale;
      LRect.left := LRect.left * LCurrentAdjustedStateStyle.Scale;
      LRect.bottom := LRect.bottom * LCurrentAdjustedStateStyle.Scale;

      // Since LStateStyle.BufDrawableRect can have different dimensions than the main BufDrawableRect (LocalRect)
      // (due to scale), we must center LStateStyle.BufDrawableRect within the main BufDrawableRect (LocalRect)
      // to ensure that all changes are visually centered.
      var LCenteredRect := LRect.CenterAt(LocalRect);
      LRect.Offset(LCenteredRect.Left, LCenteredRect.top);

      // We cannot use the matrix because, if we do, ALAlignToPixelRound in ALDrawDrawable
      // will be ineffective since the matrix will no longer be a simple translation matrix.
      // In such a case, TCustomCanvasGpu(ACanvas).DrawTexture may produce border artifacts
      // if the texture is not perfectly pixel-aligned.
      var LDstRect := TRectF.Create(0, 0, ALGetDrawableWidth(LBufDrawable), ALGetDrawableHeight(LBufDrawable));
      LDstRect.Width := (LDstRect.Width / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
      LDstRect.height := (LDstRect.height / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
      LDstRect.SetLocation(
        LRect.Left,
        LRect.Top);
      ALDrawDrawable(
        Canvas, // const ACanvas: Tcanvas;
        LBufDrawable, // const ADrawable: TALDrawable;
        LDstRect, // const ADstRect: TrectF; // IN Virtual pixels !
        AbsoluteOpacity); // const AOpacity: Single)

    end

    {$IF defined(DEBUG)}
    else if not doublebuffered then begin
      ALLog('TALBaseCheckBox.Paint', 'Controls that are not double-buffered only work when SKIA is enabled', TALLogType.ERROR);
      exit;
    end
    {$ENDIF};

    {$ENDIF}

    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{********************************************************}
function TALCheckBox.GetStateStyles: TCheckBoxStateStyles;
begin
  Result := TCheckBoxStateStyles(inherited StateStyles);
end;

{***********************************************************************}
procedure TALCheckBox.SetStateStyles(const AValue: TCheckBoxStateStyles);
begin
  inherited StateStyles := AValue;
end;

{*******************************************************************}
function TALCheckBox.CreateStateStyles: TALBaseCheckBox.TStateStyles;
begin
  Result := TCheckBoxStateStyles.Create(Self);
end;

{****************************************************}
constructor TALRadioButton.Create(AOwner: TComponent);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateCheckMark(const aCheckMark: TCheckMarkBrush);
  begin
    var LCheckMarkChanged: TNotifyEvent := ACheckMark.OnChanged;
    ACheckMark.OnChanged := nil;
    ACheckMark.Margins.DefaultValue := TRectF.Create(5,5,5,5);
    ACheckMark.Margins.Rect := CheckMark.Margins.DefaultValue;
    ACheckMark.OnChanged := LCheckMarkChanged;
  end;

begin
  inherited;
  FGroupName := '';
  fMandatory := false;
  DefaultXRadius := -50;
  DefaultYRadius := -50;
  FXRadius := DefaultXRadius;
  FYRadius := DefaultYRadius;
  _UpdateCheckMark(CheckMark);
  _UpdateCheckMark(StateStyles.Checked.Default.CheckMark);
  _UpdateCheckMark(StateStyles.Checked.Disabled.CheckMark);
  _UpdateCheckMark(StateStyles.Checked.Hovered.CheckMark);
  _UpdateCheckMark(StateStyles.Checked.Pressed.CheckMark);
  _UpdateCheckMark(StateStyles.Checked.Focused.CheckMark);
  _UpdateCheckMark(StateStyles.UnChecked.Default.CheckMark);
  _UpdateCheckMark(StateStyles.UnChecked.Disabled.CheckMark);
  _UpdateCheckMark(StateStyles.UnChecked.Hovered.CheckMark);
  _UpdateCheckMark(StateStyles.UnChecked.Pressed.CheckMark);
  _UpdateCheckMark(StateStyles.UnChecked.Focused.CheckMark);
  TMessageManager.DefaultManager.SubscribeToMessage(TRadioButtonGroupMessage, GroupMessageCall);
end;

{********************************}
destructor TALRadioButton.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TRadioButtonGroupMessage, GroupMessageCall);
  inherited;
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
  if SameText(TRadioButtonGroupMessage(M).GroupName, GroupName) and (Sender <> Self) and (Scene <> nil) and
     (not (Sender is TControl) or ((Sender as TControl).Scene = Scene)) then begin
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
            const AChecked: Boolean;
            const ACheckMark: TALBaseCheckBox.TCheckMarkBrush);
begin

  var LRect := ADstRect;
  LRect.Top := LRect.Top * AScale;
  LRect.right := LRect.right * AScale;
  LRect.left := LRect.left * AScale;
  LRect.bottom := LRect.bottom * AScale;
  var LScaledMarginsRect := ACheckMark.Margins.Rect;
  LScaledMarginsRect.Left := LScaledMarginsRect.Left * AScale;
  LScaledMarginsRect.right := LScaledMarginsRect.right * AScale;
  LScaledMarginsRect.top := LScaledMarginsRect.top * AScale;
  LScaledMarginsRect.bottom := LScaledMarginsRect.bottom * AScale;
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

    ALDrawCircle(
      ACanvas, // const ACanvas: TALCanvas;
      1, // const AScale: Single;
      IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
      LRect, // const ADstRect: TrectF;
      1, // const AOpacity: Single;
      ACheckMark.Color, // const AFillColor: TAlphaColor;
      TGradientStyle.Linear, // const AFillGradientStyle: TGradientStyle;
      [], // const AFillGradientColors: TArray<TAlphaColor>;
      [], // const AFillGradientOffsets: TArray<Single>;
      TPointF.Zero, // const AFillGradientStartPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
      TPointF.Zero, // const AFillGradientEndPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
      ACheckMark.ResourceName, // const AFillResourceName: String;
      ACheckMark.WrapMode, // Const AFillWrapMode: TALImageWrapMode;
      TRectF.Empty, // Const AFillBackgroundMarginsRect: TRectF;
      TRectF.Empty, // Const AFillImageMarginsRect: TRectF;
      0, // const AStateLayerOpacity: Single;
      TAlphaColors.Null, // const AStateLayerColor: TAlphaColor;
      TRectF.Empty, // Const AStateLayerMarginsRect: TRectF;
      0, // const AStateLayerXRadius: Single;
      0, // const AStateLayerYRadius: Single;
      False, // const ADrawStateLayerOnTop: Boolean;
      TAlphaColors.Null, // const AStrokeColor: TalphaColor;
      0, // const AStrokeThickness: Single;
      TAlphaColors.Null, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, the Canvas should have adequate space to accommodate the shadow. You can use the ALGetShadowWidth function to estimate the required width.
      0, // const AShadowBlur: Single;
      0, // const AShadowOffsetX: Single;
      0); // const AShadowOffsetY: Single;

  end

  // With ResourceName
  else begin

    ALDrawRectangle(
      ACanvas, // const ACanvas: TALCanvas;
      1, // const AScale: Single;
      IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
      LRect, // const ADstRect: TrectF;
      1, // const AOpacity: Single;
      ACheckMark.Color, // const AFillColor: TAlphaColor;
      TGradientStyle.Linear, // const AFillGradientStyle: TGradientStyle;
      [], // const AFillGradientColors: TArray<TAlphaColor>;
      [], // const AFillGradientOffsets: TArray<Single>;
      TPointF.Zero, // const AFillGradientStartPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
      TPointF.Zero, // const AFillGradientEndPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
      ACheckMark.ResourceName, // const AFillResourceName: String;
      ACheckMark.WrapMode, // Const AFillWrapMode: TALImageWrapMode;
      TRectF.Empty, // Const AFillBackgroundMarginsRect: TRectF;
      TRectF.Empty, // Const AFillImageMarginsRect: TRectF;
      0, // const AStateLayerOpacity: Single;
      TAlphaColors.Null, // const AStateLayerColor: TAlphaColor;
      TRectF.Empty, // Const AStateLayerMarginsRect: TRectF;
      0, // const AStateLayerXRadius: Single;
      0, // const AStateLayerYRadius: Single;
      False, // const ADrawStateLayerOnTop: Boolean;
      TAlphaColors.Null, // const AStrokeColor: TalphaColor;
      0, // const AStrokeThickness: Single;
      TAlphaColors.Null, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, the Canvas should have adequate space to accommodate the shadow. You can use the ALGetShadowWidth function to estimate the required width.
      0, // const AShadowBlur: Single;
      0, // const AShadowOffsetX: Single;
      0, // const AShadowOffsetY: Single;
      AllSides, // const ASides: TSides;
      AllCorners, // const ACorners: TCorners;
      0, // const AXRadius: Single;
      0); // const AYRadius: Single)

  end;

end;

{**************************************************************************}
constructor TALSwitch.TTrack.TBaseStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  //--
  StateLayer.DefaultXRadius := -50;
  StateLayer.DefaultYRadius := -50;
  StateLayer.XRadius := StateLayer.DefaultXRadius;
  StateLayer.YRadius := StateLayer.DefaultYRadius;
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

{*************************************************************************************}
constructor TALSwitch.TTrack.TCheckStateStyles.Create(const AParent: TALSwitch.TTrack);
begin
  inherited Create;
  //--
  FDefault := TDefaultStateStyle.Create(AParent);
  FDefault.OnChanged := DefaultChanged;
  //--
  FDisabled := TDisabledStateStyle.Create(FDefault);
  FDisabled.OnChanged := DisabledChanged;
  //--
  FHovered := THoveredStateStyle.Create(FDefault);
  FHovered.OnChanged := HoveredChanged;
  //--
  FPressed := TPressedStateStyle.Create(FDefault);
  FPressed.OnChanged := PressedChanged;
  //--
  FFocused := TFocusedStateStyle.Create(FDefault);
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

{********************************************************************************}
constructor TALSwitch.TTrack.TStateStyles.Create(const AParent: TALSwitch.TTrack);
begin
  inherited Create(AParent);
  //--
  FChecked := TCheckStateStyles.Create(AParent);
  FChecked.OnChanged := CheckedChanged;
  //--
  FUnchecked := TCheckStateStyles.Create(AParent);
  FUnchecked.OnChanged := UncheckedChanged;
end;

{***********************************************}
destructor TALSwitch.TTrack.TStateStyles.Destroy;
begin
  ALFreeAndNil(FChecked);
  ALFreeAndNil(FUnchecked);
  inherited Destroy;
end;

{*****************************************************************************}
function TALSwitch.TTrack.TStateStyles.CreateSavedState: TALPersistentObserver;
type
  TStateStylesClass = class of TStateStyles;
begin
  result := TStateStylesClass(classtype).Create(nil{AParent});
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
  var LFillChanged: TNotifyEvent := fill.OnChanged;
  fill.OnChanged := nil;
  Fill.DefaultColor := $ffc5c5c5;
  Fill.Color := Fill.DefaultColor;
  fill.OnChanged := LFillChanged;
  //--
  var LStrokeChanged: TNotifyEvent := stroke.OnChanged;
  stroke.OnChanged := Nil;
  Stroke.DefaultColor := TAlphaColors.null;
  Stroke.Color := Stroke.DefaultColor;
  stroke.OnChanged := LStrokeChanged;
  //--
  FChecked := False;
  FDoubleBuffered := true;
  FDefaultXRadius := -50;
  FDefaultYRadius := -50;
  FXRadius := FDefaultXRadius;
  FYRadius := FDefaultYRadius;
  //--
  // Must be created at the end because it requires FCheckMark to
  // be already created.
  FStateStyles := TStateStyles.Create(self);
  FStateStyles.OnChanged := StateStylesChanged;
end;

{**********************************}
destructor TALSwitch.TTrack.Destroy;
begin
  ALFreeAndNil(FStateStyles);
  inherited;
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

{***********************************************}
function TALSwitch.TTrack.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(52, 32);
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
    if not fDoubleBuffered then clearBufDrawable;
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
  Result := not SameValue(FXRadius, FDefaultXRadius, TEpsilon.Vector);
end;

{*************************************************}
function TALSwitch.TTrack.IsYRadiusStored: Boolean;
begin
  Result := not SameValue(FYRadius, FDefaultYRadius, TEpsilon.Vector);
end;

{*********************************************************}
procedure TALSwitch.TTrack.SetXRadius(const Value: Single);
var
  NewValue: Single;
begin
  if csDesigning in ComponentState then NewValue := Max(-50, Min(Value, Min(Width / 2, Height / 2)))
  else NewValue := Value;
  if not SameValue(FXRadius, NewValue, TEpsilon.Vector) then begin
    clearBufDrawable;
    FXRadius := NewValue;
    Repaint;
  end;
end;

{*********************************************************}
procedure TALSwitch.TTrack.SetYRadius(const Value: Single);
var
  NewValue: Single;
begin
  if csDesigning in ComponentState then NewValue := Max(-50, Min(Value, Min(Width / 2, Height / 2)))
  else NewValue := Value;
  if not SameValue(FYRadius, NewValue, TEpsilon.Vector) then begin
    clearBufDrawable;
    FYRadius := NewValue;
    Repaint;
  end;
end;

{******************************************************}
procedure TALSwitch.TTrack.FillChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{********************************************************}
procedure TALSwitch.TTrack.StrokeChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{********************************************************}
procedure TALSwitch.TTrack.ShadowChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{*************************************************************}
procedure TALSwitch.TTrack.StateStylesChanged(Sender: TObject);
begin
  clearBufDrawable;
  if Checked then DisabledOpacity := StateStyles.Checked.Disabled.opacity
  else DisabledOpacity := StateStyles.Unchecked.Disabled.opacity;
  Repaint;
end;

{********************************************}
procedure TALSwitch.TTrack.IsMouseOverChanged;
begin
  inherited;
  StateStyles.startTransition;
  repaint;
end;

{******************************************}
procedure TALSwitch.TTrack.IsFocusedChanged;
begin
  inherited;
  StateStyles.startTransition;
  repaint;
end;

{****************************************}
procedure TALSwitch.TTrack.PressedChanged;
begin
  inherited;
  StateStyles.startTransition;
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
procedure TALSwitch.TTrack.clearBufDrawable;
begin
  {$IFDEF debug}
  if (FStateStyles <> nil) and
     (not (csDestroying in ComponentState)) and
     ((not ALIsDrawableNull(FStateStyles.Checked.Default.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Disabled.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Hovered.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Pressed.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Focused.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Default.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Disabled.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Hovered.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Pressed.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Focused.BufDrawable))) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  if FStateStyles <> nil then
    FStateStyles.ClearBufDrawable;
end;

{*****************************************}
procedure TALSwitch.TTrack.MakeBufDrawable;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _DoMakeBufDrawable(const AStateStyle: TBaseStateStyle): boolean;
  begin
    if (not ALIsDrawableNull(AStateStyle.BufDrawable)) then exit(False);
    AStateStyle.SupersedeNoChanges(true{ASaveState});
    try
      CreateBufDrawable(
        AStateStyle.BufDrawable, // var ABufDrawable: TALDrawable;
        AStateStyle.BufDrawableRect, // var ABufDrawableRect: TRectF;
        ALGetScreenScale * AStateStyle.Scale, // const AScale: Single;
        AStateStyle.Fill, // const AFill: TALBrush;
        AStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
        AStateStyle.Stroke, // const AStroke: TALStrokeBrush;
        AStateStyle.Shadow); // const AShadow: TALShadow);

      // LStateStyle.BufDrawableRect must include the LScale
      AStateStyle.BufDrawableRect.Top := AStateStyle.BufDrawableRect.Top * AStateStyle.Scale;
      AStateStyle.BufDrawableRect.right := AStateStyle.BufDrawableRect.right * AStateStyle.Scale;
      AStateStyle.BufDrawableRect.left := AStateStyle.BufDrawableRect.left * AStateStyle.Scale;
      AStateStyle.BufDrawableRect.bottom := AStateStyle.BufDrawableRect.bottom * AStateStyle.Scale;

      // Since LStateStyle.BufDrawableRect can have different dimensions than the main BufDrawableRect (LocalRect)
      // (due to scale), we must center LStateStyle.BufDrawableRect within the main BufDrawableRect (LocalRect)
      // to ensure that all changes are visually centered.
      var LCenteredRect := AStateStyle.BufDrawableRect.CenterAt(LocalRect);
      AStateStyle.BufDrawableRect.Offset(LCenteredRect.Left, LCenteredRect.top);

    finally
      AStateStyle.RestorestateNoChanges;
    end;
    Result := True;
  end;

begin
  //--- Do not create BufDrawable if not DoubleBuffered
  if {$IF not DEFINED(ALDPK)}(not DoubleBuffered){$ELSE}False{$ENDIF} then begin
    clearBufDrawable;
    exit;
  end;
  //--
  if Checked then _DoMakeBufDrawable(StateStyles.Checked.Default)
  else _DoMakeBufDrawable(StateStyles.UnChecked.Default);
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if LStateStyle.Inherit then exit;
  _DoMakeBufDrawable(LStateStyle);
  // No need to center LStateStyle.BufDrawableRect on the main BufDrawableRect
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
                        AFill, // const AFill: TALBrush;
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

      ALDrawRectangle(
        LCanvas, // const ACanvas: TALCanvas;
        AScale, // const AScale: Single;
        IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
        ABufDrawableRect, // const Rect: TrectF;
        1, // const AOpacity: Single;
        AFill, // const Fill: TALBrush;
        AStateLayer, // const StateLayer: TALStateLayer;
        TAlphaColors.Null, // const AStateLayerContentColor: TAlphaColor;
        False, // const ADrawStateLayerOnTop: Boolean;
        AStroke, // const Stroke: TALStrokeBrush;
        AShadow, // const Shadow: TALShadow
        AllSides, // const Sides: TSides;
        AllCorners, // const Corners: TCorners;
        XRadius, // const XRadius: Single = 0;
        YRadius); // const YRadius: Single = 0);

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

{*******************************}
procedure TALSwitch.TTrack.Paint;
begin

  StateStyles.UpdateLastPaintedRawStyle;
  MakeBufDrawable;

  var LDrawable: TALDrawable;
  var LDrawableRect: TRectF;
  if StateStyles.IsTransitionAnimationRunning then begin
    LDrawable := ALNullDrawable;
    LDrawableRect := TRectF.Empty;
  end
  else begin
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      LDrawable := LStateStyle.BufDrawable;
      LDrawableRect := LStateStyle.BufDrawableRect;
      if ALIsDrawableNull(LDrawable) then begin
        if checked then begin
          LDrawable := StateStyles.Checked.default.BufDrawable;
          LDrawableRect := StateStyles.Checked.default.BufDrawableRect;
        end
        else begin
          LDrawable := StateStyles.UnChecked.default.BufDrawable;
          LDrawableRect := StateStyles.UnChecked.default.BufDrawableRect;
        end;
      end;
    end
    else
      raise Exception.Create('Error #EA9B4064-F1D2-4E04-82FE-99FD3ED8B1F3');
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
                                                LCurrentAdjustedStateStyle.Scale, // Const AScaleX: Single;
                                                LCurrentAdjustedStateStyle.Scale, // Const AScaleY: Single;
                                                true); // Const ASaveState: Boolean);
    try

      var LRect := LocalRect;

      if compareValue(AbsoluteOpacity, 1, Tepsilon.Scale) < 0 then begin
        var LLayerRect := ALGetShapeSurfaceRect(
                            LRect, // const ARect: TrectF;
                            LCurrentAdjustedStateStyle.Fill.Color, // const AFillColor: TAlphaColor;
                            LCurrentAdjustedStateStyle.Fill.Gradient.Colors, // const AFillGradientColors: TArray<TAlphaColor>;
                            LCurrentAdjustedStateStyle.Fill.ResourceName, // const AFillResourceName: String;
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

        ALDrawRectangle(
          TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
          1, // const AScale: Single;
          IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
          LRect, // const Rect: TrectF;
          1, // const AOpacity: Single;
          LCurrentAdjustedStateStyle.Fill, // const Fill: TALBrush;
          LCurrentAdjustedStateStyle.StateLayer, // const StateLayer: TALStateLayer;
          TAlphaColors.Null, // const AStateLayerContentColor: TAlphaColor;
          False, // const ADrawStateLayerOnTop: Boolean;
          LCurrentAdjustedStateStyle.Stroke, // const Stroke: TALStrokeBrush;
          LCurrentAdjustedStateStyle.Shadow, // const Shadow: TALShadow
          AllSides, // const Sides: TSides;
          AllCorners, // const Corners: TCorners;
          XRadius, // const XRadius: Single = 0;
          YRadius); // const YRadius: Single = 0);

      finally
        if compareValue(AbsoluteOpacity, 1, Tepsilon.Scale) < 0 then
          ALEndTransparencyLayer(TSkCanvasCustom(Canvas).Canvas.Handle);
      end;

    finally
      if LCanvasSaveState <> nil then
        Canvas.RestoreState(LCanvasSaveState);
    end;

    {$ELSE}

    if StateStyles.IsTransitionAnimationRunning then begin

      var LRect := LocalRect;
      var LBufSurface: TALSurface;
      var LBufCanvas: TALCanvas;
      var LBufDrawable: TALDrawable;
      StateStyles.GetTransitionBufSurface(
        LRect, // var ARect: TrectF;
        ALGetScreenScale, // const AScale: Single;
        LBufSurface, // out ABufSurface: TALSurface;
        LBufCanvas, // out ABufCanvas: TALCanvas;
        LBufDrawable); // out ABufDrawable: TALDrawable);

      if ALCanvasBeginScene(LBufCanvas) then
      try

        ALClearCanvas(LBufCanvas, TAlphaColors.Null);

        ALDrawRectangle(
          LBufCanvas, // const ACanvas: TALCanvas;
          ALGetScreenScale, // const AScale: Single;
          IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
          LRect, // const Rect: TrectF;
          1, // const AOpacity: Single;
          LCurrentAdjustedStateStyle.Fill, // const Fill: TALBrush;
          LCurrentAdjustedStateStyle.StateLayer, // const StateLayer: TALStateLayer;
          TAlphaColors.Null, // const AStateLayerContentColor: TAlphaColor;
          False, // const ADrawStateLayerOnTop: Boolean;
          LCurrentAdjustedStateStyle.Stroke, // const Stroke: TALStrokeBrush;
          LCurrentAdjustedStateStyle.Shadow, // const Shadow: TALShadow
          AllSides, // const Sides: TSides;
          AllCorners, // const Corners: TCorners;
          XRadius, // const XRadius: Single = 0;
          YRadius); // const YRadius: Single = 0);

      finally
        ALCanvasEndScene(LBufCanvas)
      end;

      ALUpdateDrawableFromSurface(LBufSurface, LBufDrawable);

      // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
      // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
      LRect.Offset(-2*LRect.Left, -2*LRect.Top);

      // LRect must include the LScale
      LRect.Top := LRect.Top * LCurrentAdjustedStateStyle.Scale;
      LRect.right := LRect.right * LCurrentAdjustedStateStyle.Scale;
      LRect.left := LRect.left * LCurrentAdjustedStateStyle.Scale;
      LRect.bottom := LRect.bottom * LCurrentAdjustedStateStyle.Scale;

      // Since LStateStyle.BufDrawableRect can have different dimensions than the main BufDrawableRect (LocalRect)
      // (due to scale), we must center LStateStyle.BufDrawableRect within the main BufDrawableRect (LocalRect)
      // to ensure that all changes are visually centered.
      var LCenteredRect := LRect.CenterAt(LocalRect);
      LRect.Offset(LCenteredRect.Left, LCenteredRect.top);

      // We cannot use the matrix because, if we do, ALAlignToPixelRound in ALDrawDrawable
      // will be ineffective since the matrix will no longer be a simple translation matrix.
      // In such a case, TCustomCanvasGpu(ACanvas).DrawTexture may produce border artifacts
      // if the texture is not perfectly pixel-aligned.
      var LDstRect := TRectF.Create(0, 0, ALGetDrawableWidth(LBufDrawable), ALGetDrawableHeight(LBufDrawable));
      LDstRect.Width := (LDstRect.Width / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
      LDstRect.height := (LDstRect.height / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
      LDstRect.SetLocation(
        LRect.Left,
        LRect.Top);
      ALDrawDrawable(
        Canvas, // const ACanvas: Tcanvas;
        LBufDrawable, // const ADrawable: TALDrawable;
        LDstRect, // const ADstRect: TrectF; // IN Virtual pixels !
        AbsoluteOpacity); // const AOpacity: Single)

    end

    {$IF defined(DEBUG)}
    else if not doublebuffered then begin
      ALLog('TALSwitch.TTrack.Paint', 'Controls that are not double-buffered only work when SKIA is enabled', TALLogType.ERROR);
      exit;
    end
    {$ENDIF};

    {$ENDIF}

    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{***********************************************************}
procedure TALSwitch.TThumb.TThumbStateStyles.StartTransition;
begin
  FStartPositionX := Parent{Thumb}.Position.x;
  inherited;
  if not IsTransitionAnimationRunning then
    TALSwitch(Parent{Thumb}.ParentControl{Track}.ParentControl{Switch}).AlignThumb;
end;

{***************************************************************************************}
procedure TALSwitch.TThumb.TThumbStateStyles.TransitionAnimationProcess(Sender: TObject);
begin
  var LThumb := Parent;
  var LSwitch := TALSwitch(LThumb.ParentControl{Track}.ParentControl{Switch});
  if (not LSwitch.Pressed) and (Lthumb.Align = TAlignLayout.None) then begin
    var LFloatAnimation := TALFloatAnimation(Sender);
    var LStopPositionX: Single;
    If LSwitch.Checked then LStopPositionX := LSwitch.GetMaxThumbPos
    else LStopPositionX := LSwitch.GetMinThumbPos;
    LThumb.Position.x := FStartPositionX + (LStopPositionX - FStartPositionX) * LFloatAnimation.CurrentValue;
  end;
  inherited;
end;

{**************************************************************************************}
procedure TALSwitch.TThumb.TThumbStateStyles.TransitionAnimationFinish(Sender: TObject);
begin
  TALSwitch(Parent{Thumb}.ParentControl{Track}.ParentControl{Switch}).AlignThumb;
  inherited;
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
  FDefaultXRadius := -50;
  FDefaultYRadius := -50;
  FXRadius := FDefaultXRadius;
  FYRadius := FDefaultYRadius;
  //--
  var LStrokeChanged: TNotifyEvent := stroke.OnChanged;
  stroke.OnChanged := Nil;
  Stroke.DefaultColor := TAlphaColors.null;
  Stroke.Color := Stroke.DefaultColor;
  stroke.OnChanged := LStrokeChanged;
  //--
  var LCheckMarkChanged: TNotifyEvent := CheckMark.OnChanged;
  CheckMark.OnChanged := Nil;
  CheckMark.Margins.DefaultValue := TRectF.Create(6,6,6,6);
  CheckMark.Margins.Rect := CheckMark.Margins.DefaultValue;
  CheckMark.OnChanged := LCheckMarkChanged;
  //--
  Margins.DefaultValue := TRectF.Create(4,4,4,4);
  Margins.Rect := Margins.DefaultValue;
end;

{***********************************************}
function TALSwitch.TThumb.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(24, 24);
end;

{************************************************************************}
function TALSwitch.TThumb.CreateStateStyles: TALBaseCheckBox.TStateStyles;
begin
  result := TThumbStateStyles.Create(Self);
end;

{*******************************}
procedure TALSwitch.TThumb.Click;
begin
  // Since TALSwitch.TThumb has HitTest set to false, this event
  // is triggered only at the end of the transition animation when
  // DelayClick is set to true.
  TALSwitch(ParentControl{Track}.ParentControl{Switch}).click;
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
  FTransition := TALStateTransition.Create(0.16{ADefaultDuration});
  FTransition.OnChanged := TransitionChanged;
  //--
  FTrack := TTrack.Create(self);
  FTrack.Parent := self;
  FTrack.Stored := False;
  FTrack.SetSubComponent(True);
  FTrack.Name := 'Track'; // Useful at design time in the IDE
  FTrack.Align := TalignLayout.Client;
  //--
  // Use 'self' instead of 'FTrack' to ensure that
  // 'Fthumb.loaded' is called.
  FThumb := TThumb.Create(self);
  FThumb.Parent := FTrack;
  FThumb.Stored := False;
  FThumb.SetSubComponent(True);
  FThumb.Name := 'Thumb'; // Useful at design time in the IDE
end;

{***************************}
destructor TALSwitch.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
  ALFreeAndNil(FTransition);
  inherited;
end;

{************************************}
procedure TALSwitch.AfterConstruction;
begin
  inherited;
  if not IsOwnerLoading then
    AlignThumb;
end;

{*************************}
procedure TALSwitch.Loaded;
begin
  inherited;
  Thumb.StateStyles.Transition.Assign(Transition);
  Track.StateStyles.Transition.Assign(Transition);
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

{**********************************}
procedure TALSwitch.MakeBufDrawable;
begin
  Track.MakeBufDrawable;
  Thumb.MakeBufDrawable;
end;

{***********************************}
procedure TALSwitch.clearBufDrawable;
begin
  Track.clearBufDrawable;
  Thumb.clearBufDrawable;
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

{*****************************************************************}
procedure TALSwitch.SetTransition(const Value: TALStateTransition);
begin
  FTransition.Assign(Value);
end;

{******************************************************}
procedure TALSwitch.TransitionChanged(ASender: TObject);
begin
  if csLoading in ComponentState then exit;
  Thumb.StateStyles.Transition.Assign(Transition);
  Track.StateStyles.Transition.Assign(Transition);
end;

{**********************************}
procedure TALSwitch.StartTransition;
begin
  Thumb.StateStyles.StartTransition;
  Track.StateStyles.StartTransition;
end;

{************************************************************************************}
procedure TALSwitch.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Pressed then begin
    fThumb.Align := TALignLayout.None;
    FPressedThumbPos := FThumb.Position.Point;
  end;
end;

{**************************************************************}
procedure TALSwitch.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Pressed then begin

    if (not fScrollCapturedByMe) then begin
      If (abs(X - PressedPosition.X) > abs(Y - PressedPosition.Y)) and
         (abs(X - PressedPosition.X) > TALScrollEngine.DefaultTouchSlop) then begin
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
        Thumb.StateStyles.TransitionClickDelayed := True;
      FTrack.Checked := LChecked;
      FThumb.Checked := LChecked;
      if not Thumb.StateStyles.TransitionClickDelayed then
        DoChange;
    end;
    fThumb.Align := TALignLayout.None;
    StartTransition;
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
        Thumb.StateStyles.TransitionClickDelayed := True;
      FTrack.Checked := LChecked;
      FThumb.Checked := LChecked;
      if not Thumb.StateStyles.TransitionClickDelayed then
        DoChange;
    end;
    fThumb.Align := TALignLayout.None;
    StartTransition;
  end;
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
    Thumb.StateStyles.TransitionClickDelayed := True;
    var LChecked := not Checked;
    FTrack.Checked := LChecked;
    FThumb.Checked := LChecked;
    fThumb.Align := TALignLayout.None;
    StartTransition;
    exit;
  end
  // If Pressed is true, it means this event is triggered by MouseDown/MouseUp.
  else if Pressed then begin
    var LChecked := not Checked;
    FTrack.Checked := LChecked;
    FThumb.Checked := LChecked;
    fThumb.Align := TALignLayout.None;
    DoChange;
    inherited;
    StartTransition;
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
  //  'Pressed: ' + ALBoolToStrW(Pressed),
  //  TalLogType.verbose);
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
  If Checked then FThumb.Align := TALignLayout.right
  else FThumb.Align := TALignLayout.left;
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

{*******************************************************************}
constructor TALButton.TBaseStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  FDefaultText := '';
  FText := FDefaultText;
  //--
  if StateStyleParent <> nil then FTextSettings := TStateStyleTextSettings.Create(StateStyleParent.TextSettings)
  else if ControlParent <> nil then FTextSettings := TStateStyleTextSettings.Create(ControlParent.TextSettings)
  else FTextSettings := TStateStyleTextSettings.Create(nil);
  FTextSettings.OnChanged := TextSettingsChanged;
  //--
  Fill.DefaultColor := $FFE1E1E1;
  Fill.Color := Fill.DefaultColor;
  //--
  Stroke.DefaultColor := $FFADADAD;
  Stroke.Color := Stroke.DefaultColor;
  //--
  //FPriorSupersedeText
end;

{*******************************************}
destructor TALButton.TBaseStateStyle.Destroy;
begin
  ALFreeAndNil(FTextSettings);
  inherited Destroy;
end;

{**************************************************************}
procedure TALButton.TBaseStateStyle.Assign(Source: TPersistent);
begin
  if Source is TBaseStateStyle then begin
    BeginUpdate;
    Try
      Text := TBaseStateStyle(Source).text;
      TextSettings.Assign(TBaseStateStyle(Source).TextSettings);
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

{***********************************************************************************************************}
procedure TALButton.TBaseStateStyle.Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single);
begin
  {$IF defined(debug)}
  if (ATo <> nil) and (not (ATo is TBaseStateStyle)) then
    Raise Exception.Create('Error F3C72244-894F-4B67-AD86-F24DF5039927');
  {$ENDIF}
  BeginUpdate;
  try
    Inherited Interpolate(ATo, ANormalizedTime);
    if ATo <> nil then begin
      Text := TBaseStateStyle(ATo).Text;
      TextSettings.Interpolate(TBaseStateStyle(ATo).TextSettings, ANormalizedTime);
    end
    else if StateStyleParent <> nil then begin
      StateStyleParent.SupersedeNoChanges(true{ASaveState});
      try
        Text := StateStyleParent.Text;
        TextSettings.Interpolate(StateStyleParent.TextSettings, ANormalizedTime);
      finally
        StateStyleParent.RestoreStateNoChanges;
      end;
    end
    else if ControlParent <> nil then begin
      Text := ControlParent.Text;
      TextSettings.Interpolate(ControlParent.TextSettings, ANormalizedTime);
    end
    else begin
      Text := DefaultText;
      TextSettings.Interpolate(nil, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{**********************************************}
procedure TALButton.TBaseStateStyle.DoSupersede;
begin
  Inherited;
  //--
  FPriorSupersedeText := Text;
  //--
  if Text = '' then begin
    if StateStyleParent <> nil then Text := StateStyleParent.Text
    else Text := ControlParent.Text;
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

{*****************************************************************************************}
procedure TALButton.TBaseStateStyle.SetTextSettings(const AValue: TStateStyleTextSettings);
begin
  FTextSettings.Assign(AValue);
end;

{*****************************************************}
function TALButton.TBaseStateStyle.GetInherit: Boolean;
begin
  Result := inherited GetInherit and
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
  Result := FText <> FDefaultText;
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

{******************************************************************}
constructor TALButton.TStateStyles.Create(const AParent: TALButton);
begin
  inherited Create(AParent);
  //--
  FDisabled := TDisabledStateStyle.Create(AParent);
  FDisabled.OnChanged := DisabledChanged;
  //--
  FHovered := THoveredStateStyle.Create(AParent);
  FHovered.OnChanged := HoveredChanged;
  //--
  FPressed := TPressedStateStyle.Create(AParent);
  FPressed.OnChanged := PressedChanged;
  //--
  FFocused := TFocusedStateStyle.Create(AParent);
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

{**********************************************************************}
function TALButton.TStateStyles.CreateSavedState: TALPersistentObserver;
type
  TALButtonStateStylesClass = class of TStateStyles;
begin
  result := TALButtonStateStylesClass(classtype).Create(nil{AParent});
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
  AutoSize := True;
  Cursor := crHandPoint;
  //--
  var LFillChanged: TNotifyEvent := fill.OnChanged;
  fill.OnChanged := nil;
  Fill.DefaultColor := $ffe1e1e1;
  Fill.Color := Fill.DefaultColor;
  fill.OnChanged := LFillChanged;
  //--
  var LStrokeChanged: TNotifyEvent := stroke.OnChanged;
  stroke.OnChanged := Nil;
  Stroke.DefaultColor := $ffadadad;
  Stroke.Color := Stroke.DefaultColor;
  stroke.OnChanged := LStrokeChanged;
  //--
  var LTextSettingsChanged: TNotifyEvent := TextSettings.OnChanged;
  TextSettings.OnChanged := nil;
  TextSettings.Font.DefaultWeight := TFontWeight.medium;
  TextSettings.Font.Weight := TextSettings.Font.DefaultWeight;
  TextSettings.DefaultHorzAlign := TALTextHorzAlign.center;
  TextSettings.HorzAlign := TextSettings.DefaultHorzAlign;
  TextSettings.OnChanged := LTextSettingsChanged;
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
  FStateStyles := TStateStyles.Create(self);
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

{*************************}
procedure TALButton.Loaded;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _ConvertFontFamily(const AStateStyle: TBaseStateStyle);
  begin
    if (AStateStyle.TextSettings.Font.AutoConvert) and
       (AStateStyle.TextSettings.Font.Family <> '') and
       (not (csDesigning in ComponentState)) then
      AStateStyle.TextSettings.Font.Family := ALConvertFontFamily(AStateStyle.TextSettings.Font.Family);
  end;

begin
  _ConvertFontFamily(StateStyles.Disabled);
  _ConvertFontFamily(StateStyles.Hovered);
  _ConvertFontFamily(StateStyles.Pressed);
  _ConvertFontFamily(StateStyles.Focused);
  inherited Loaded;
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

{*********************************************************}
function TALButton.CreateTextSettings: TALBaseTextSettings;
begin
  Result := TTextSettings.Create;
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
      if APrevStateStyle.TextSettings.font.Color = AToStateStyle.TextSettings.font.Color then AToStateStyle.TextSettings.font.Color := TextSettings.font.Color;
      if APrevStateStyle.TextSettings.font.AutoConvert = AToStateStyle.TextSettings.font.AutoConvert then AToStateStyle.TextSettings.font.AutoConvert := TextSettings.font.AutoConvert;

      if APrevStateStyle.TextSettings.Decoration.Kinds = AToStateStyle.TextSettings.Decoration.Kinds then AToStateStyle.TextSettings.Decoration.Kinds := TextSettings.Decoration.Kinds;
      if APrevStateStyle.TextSettings.Decoration.Style = AToStateStyle.TextSettings.Decoration.Style then AToStateStyle.TextSettings.Decoration.Style := TextSettings.Decoration.Style;
      if SameValue(APrevStateStyle.TextSettings.Decoration.ThicknessMultiplier, AToStateStyle.TextSettings.Decoration.ThicknessMultiplier, TEpsilon.Scale) then AToStateStyle.TextSettings.Decoration.ThicknessMultiplier := TextSettings.Decoration.ThicknessMultiplier;
      if APrevStateStyle.TextSettings.Decoration.Color = AToStateStyle.TextSettings.Decoration.Color then AToStateStyle.TextSettings.Decoration.Color := TextSettings.Decoration.Color;

    end;

    APrevStateStyle.TextSettings.font.Family := TextSettings.font.Family;
    APrevStateStyle.TextSettings.font.Size := TextSettings.font.Size;
    APrevStateStyle.TextSettings.font.Weight := TextSettings.font.Weight;
    APrevStateStyle.TextSettings.font.Slant := TextSettings.font.Slant;
    APrevStateStyle.TextSettings.font.Stretch := TextSettings.font.Stretch;
    APrevStateStyle.TextSettings.font.Color := TextSettings.font.Color;
    APrevStateStyle.TextSettings.font.AutoConvert := TextSettings.font.AutoConvert;

    APrevStateStyle.TextSettings.Decoration.Kinds := TextSettings.Decoration.Kinds;
    APrevStateStyle.TextSettings.Decoration.Style := TextSettings.Decoration.Style;
    APrevStateStyle.TextSettings.Decoration.ThicknessMultiplier := TextSettings.Decoration.ThicknessMultiplier;
    APrevStateStyle.TextSettings.Decoration.Color := TextSettings.Decoration.Color;

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

{**************************************************}
procedure TALButton.SetXRadius(const Value: Single);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin
    if (not (csLoading in ComponentState)) and
       (not AToStateStyle.StateLayer.HasFill) then begin
      if (SameValue(APrevStateStyle.StateLayer.XRadius, AToStateStyle.StateLayer.XRadius, TEpsilon.Vector)) then AToStateStyle.StateLayer.XRadius := XRadius;
    end;
    APrevStateStyle.StateLayer.XRadius := XRadius;
  end;
  {$ENDIF}

begin
  inherited;
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.Disabled, StateStyles.Disabled);
    _PropagateChanges(FPrevStateStyles.Hovered, StateStyles.Hovered);
    _PropagateChanges(FPrevStateStyles.Pressed, StateStyles.Pressed);
    _PropagateChanges(FPrevStateStyles.Focused, StateStyles.Focused);
  end;
  {$ENDIF}
end;

{**************************************************}
procedure TALButton.SetYRadius(const Value: Single);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin
    if (not (csLoading in ComponentState)) and
       (not AToStateStyle.StateLayer.HasFill) then begin
      if (SameValue(APrevStateStyle.StateLayer.YRadius, AToStateStyle.StateLayer.YRadius, TEpsilon.Vector)) then AToStateStyle.StateLayer.YRadius := YRadius;
    end;
    APrevStateStyle.StateLayer.YRadius := YRadius;
  end;
  {$ENDIF}

begin
  inherited;
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.Disabled, StateStyles.Disabled);
    _PropagateChanges(FPrevStateStyles.Hovered, StateStyles.Hovered);
    _PropagateChanges(FPrevStateStyles.Pressed, StateStyles.Pressed);
    _PropagateChanges(FPrevStateStyles.Focused, StateStyles.Focused);
  end;
  {$ENDIF}
end;

{******************************************************}
procedure TALButton.StateStylesChanged(Sender: TObject);
begin
  clearBufDrawable;
  DisabledOpacity := StateStyles.Disabled.opacity;
  Repaint;
end;

{*************************************}
procedure TALButton.IsMouseOverChanged;
begin
  inherited;
  StateStyles.startTransition;
  repaint;
end;

{***********************************}
procedure TALButton.IsFocusedChanged;
begin
  inherited;
  StateStyles.startTransition;
  repaint;
end;

{*********************************}
procedure TALButton.PressedChanged;
begin
  inherited;
  StateStyles.startTransition;
  repaint;
end;

{************************}
procedure TALButton.Click;
begin
  if StateStyles.IsTransitionAnimationRunning and StateStyles.Transition.DelayClick then
    StateStyles.TransitionClickDelayed := True
  else
    inherited click;
end;

{***********************************}
procedure TALButton.clearBufDrawable;
begin
  {$IFDEF debug}
  if (FStateStyles <> nil) and
     (not (csDestroying in ComponentState)) and
     (ALIsDrawableNull(BufDrawable)) and // warn will be raise in inherited
     ((not ALIsDrawableNull(FStateStyles.Disabled.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Hovered.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Pressed.BufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Focused.BufDrawable))) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  if FStateStyles <> nil then
    FStateStyles.ClearBufDrawable;
  inherited clearBufDrawable;
end;

{**********************************}
procedure TALButton.MakeBufDrawable;
begin
  //--- Do not create BufDrawable if not DoubleBuffered
  if {$IF not DEFINED(ALDPK)}(not DoubleBuffered){$ELSE}False{$ENDIF} then begin
    clearBufDrawable;
    exit;
  end;
  //--
  inherited MakeBufDrawable;
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if LStateStyle.Inherit then exit;
  if (not ALIsDrawableNull(LStateStyle.BufDrawable)) then exit;
  LStateStyle.SupersedeNoChanges(true{ASaveState});
  try

    // Create the BufDrawable
    var LTextBroken: Boolean;
    var LAllTextDrawn: Boolean;
    var LElements: TALTextElements;
    CreateBufDrawable(
      LStateStyle.BufDrawable, // var ABufDrawable: TALDrawable;
      LStateStyle.BufDrawableRect, // var ABufDrawableRect: TRectF;
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
      LStateStyle.Shadow); // const AShadow: TALShadow);

    // LStateStyle.BufDrawableRect must include the LScale
    LStateStyle.BufDrawableRect.Top := LStateStyle.BufDrawableRect.Top * LStateStyle.Scale;
    LStateStyle.BufDrawableRect.right := LStateStyle.BufDrawableRect.right * LStateStyle.Scale;
    LStateStyle.BufDrawableRect.left := LStateStyle.BufDrawableRect.left * LStateStyle.Scale;
    LStateStyle.BufDrawableRect.bottom := LStateStyle.BufDrawableRect.bottom * LStateStyle.Scale;

    // Since LStateStyle.BufDrawableRect can have different dimensions than the main BufDrawableRect
    // (due to autosizing with different font sizes), we must center LStateStyle.BufDrawableRect
    // within the main BufDrawableRect to ensure that all changes are visually centered.
    var LMainDrawableRect := BufDrawableRect;
    LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
    var LCenteredRect := LStateStyle.BufDrawableRect.CenterAt(LMainDrawableRect);
    LStateStyle.BufDrawableRect.Offset(LCenteredRect.Left, LCenteredRect.top);

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

{************************}
procedure TALButton.Paint;
begin

  StateStyles.UpdateLastPaintedRawStyle;
  MakeBufDrawable;

  var LDrawable: TALDrawable;
  var LDrawableRect: TRectF;
  if StateStyles.IsTransitionAnimationRunning then begin
    LDrawable := ALNullDrawable;
    LDrawableRect := TRectF.Empty;
  end
  else begin
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      LDrawable := LStateStyle.BufDrawable;
      LDrawableRect := LStateStyle.BufDrawableRect;
      if ALIsDrawableNull(LDrawable) then begin
        LDrawable := BufDrawable;
        LDrawableRect := BufDrawableRect;
      end;
    end
    else begin
      LDrawable := BufDrawable;
      LDrawableRect := BufDrawableRect;
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
                                                LCurrentAdjustedStateStyle.Scale, // Const AScaleX: Single;
                                                LCurrentAdjustedStateStyle.Scale, // Const AScaleY: Single;
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
        LCurrentAdjustedStateStyle.Shadow); // const AShadow: TALShadow);

    finally
      if LCanvasSaveState <> nil then
        Canvas.RestoreState(LCanvasSaveState);
    end;

    {$ELSE}

    if StateStyles.IsTransitionAnimationRunning then begin

      var LRect := LocalRect;
      var LBufSurface: TALSurface;
      var LBufCanvas: TALCanvas;
      var LBufDrawable: TALDrawable;
      StateStyles.GetTransitionBufSurface(
        LRect, // var ARect: TrectF;
        ALGetScreenScale, // const AScale: Single;
        LBufSurface, // out ABufSurface: TALSurface;
        LBufCanvas, // out ABufCanvas: TALCanvas;
        LBufDrawable); // out ABufDrawable: TALDrawable);

      if ALCanvasBeginScene(LBufCanvas) then
      try

        ALClearCanvas(LBufCanvas, TAlphaColors.Null);

        var LTextBroken: Boolean;
        var LAllTextDrawn: Boolean;
        var LElements: TALTextElements;
        DrawMultilineText(
          LBufCanvas, // const ACanvas: TALCanvas;
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
          LCurrentAdjustedStateStyle.Shadow); // const AShadow: TALShadow);

      finally
        ALCanvasEndScene(LBufCanvas)
      end;

      ALUpdateDrawableFromSurface(LBufSurface, LBufDrawable);

      // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
      // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
      LRect.Offset(-2*LRect.Left, -2*LRect.Top);

      // LRect must include the LScale
      LRect.Top := LRect.Top * LCurrentAdjustedStateStyle.Scale;
      LRect.right := LRect.right * LCurrentAdjustedStateStyle.Scale;
      LRect.left := LRect.left * LCurrentAdjustedStateStyle.Scale;
      LRect.bottom := LRect.bottom * LCurrentAdjustedStateStyle.Scale;

      // Since LStateStyle.BufDrawableRect can have different dimensions than the main BufDrawableRect
      // (due to autosizing with different font sizes), we must center LStateStyle.BufDrawableRect
      // within the main BufDrawableRect to ensure that all changes are visually centered.
      var LMainDrawableRect := BufDrawableRect;
      LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
      var LCenteredRect := LRect.CenterAt(LMainDrawableRect);
      LRect.Offset(LCenteredRect.Left, LCenteredRect.top);

      // We cannot use the matrix because, if we do, ALAlignToPixelRound in ALDrawDrawable
      // will be ineffective since the matrix will no longer be a simple translation matrix.
      // In such a case, TCustomCanvasGpu(ACanvas).DrawTexture may produce border artifacts
      // if the texture is not perfectly pixel-aligned.
      var LDstRect := TRectF.Create(0, 0, ALGetDrawableWidth(LBufDrawable), ALGetDrawableHeight(LBufDrawable));
      LDstRect.Width := (LDstRect.Width / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
      LDstRect.height := (LDstRect.height / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
      LDstRect.SetLocation(
        LRect.Left,
        LRect.Top);
      ALDrawDrawable(
        Canvas, // const ACanvas: Tcanvas;
        LBufDrawable, // const ADrawable: TALDrawable;
        LDstRect, // const ADstRect: TrectF; // IN Virtual pixels !
        AbsoluteOpacity); // const AOpacity: Single)

    end

    {$IF defined(DEBUG)}
    else if not doublebuffered then begin
      ALLog('TALButton.Paint', 'Controls that are not double-buffered only work when SKIA is enabled', TALLogType.ERROR);
      exit;
    end
    {$ENDIF};

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
     TALSwitch, TALButton]);
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
  RegisterFmxClasses(
    [TALAniIndicator, TALScrollBar, TALTrackBar,
     TALRangeTrackBar, TALCheckBox, TALRadioButton,
     TALSwitch, TALButton]);

end.
