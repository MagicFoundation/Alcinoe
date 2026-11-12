unit Alcinoe.FMX.MapLibre;

interface

{$I Alcinoe.inc}

uses
  System.Classes,
  System.UITypes,
  System.Types,
  FMX.Controls,
  {$IF defined(ANDROID)}
  System.Messaging,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Alcinoe.AndroidApi.MapLibre,
  Alcinoe.FMX.NativeView.Android,
  {$ENDIF}
  {$IF defined(IOS)}
  System.TypInfo,
  Macapi.ObjectiveC,
  iOSapi.UIKit,
  Alcinoe.iOSApi.MapLibre,
  Alcinoe.FMX.NativeView.Ios,
  {$ENDIF}
  {$IF defined(ALMacOS)}
  Alcinoe.FMX.WebBrowser,
  Alcinoe.fmx.NativeView.Mac,
  {$ENDIF}
  {$IF defined(MSWINDOWS)}
  Alcinoe.FMX.NativeView.Win,
  Alcinoe.FMX.WebBrowser,
  {$ENDIF}
  Alcinoe.Common,
  Alcinoe.StringList,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.NativeControl;

type

  {********************}
  {$IF defined(android)}
  TALAndroidMapLibreView = class;
  {$endif}
  {$IF defined(ios)}
  TALIosMapLibreView = class;
  {$endif}
  {$IF defined(ALMacOS)}
  TALMacMapLibreView = class;
  {$endif}
  {$IF defined(MSWINDOWS)}
  TALWinMapLibreView = class;
  {$endif}

  {*************************}
  [ComponentPlatforms($FFFF)]
  TALMapLibre = class({$IF defined(MSWINDOWS) or defined(ALMacOS)}TALBaseWebBrowser{$ELSE}TALNativeControl{$ENDIF}, IControlTypeSupportable, IALNativeControl)
  public
    type
      // -----------------
      // TSymbolClickEvent
      TSymbolClickEvent = procedure(Sender: TObject; const ALayerID: String; const ASymbolID: String) of object;
      // --------------
      // TMapClickEvent
      TMapClickEvent = procedure(Sender: TObject; const ALatitude: Double; Const ALongitude: Double) of object;
      // ---------------
      // TCameraPosition
      TCameraPosition = record
        bearing: Double;
        padding: TALRectD;
        roll: Double;
        target: TALLatLng;
        tilt: Double;
        zoom: Double;
        constructor Create(
                      const ABearing: Double;
                      const APadding: TALRectD;
                      const ARoll: Double;
                      const ATarget: TALLatLng;
                      const ATilt: Double;
                      const AZoom: Double);
      end;
      // -------------------
      // TSymbolLayerOptions
      PSymbolLayerOptions = ^TSymbolLayerOptions;
      TSymbolLayerOptions = Record
      private
        FSymbolPlacement: String;
        FSymbolSpacing: Single;
        FSymbolAvoidEdges: Boolean;
        FSymbolZOrder: String;
        FIconAllowOverlap: Boolean;
        FIconIgnorePlacement: Boolean;
        FIconOptional: Boolean;
        FIconRotationAlignment: String;
        FIconPitchAlignment: String;
        FIconPadding: Single;
        FIconKeepUpright: Boolean;
        FIconTextFit: String;
        FIconTextFitPadding: TRectF;
        FIconTranslate: TPointF;
        FIconTranslateAnchor: String;
        FTextAllowOverlap: Boolean;
        FTextIgnorePlacement: Boolean;
        FTextOptional: Boolean;
        FTextFont: TArray<String>;
        FTextRotationAlignment: String;
        FTextPitchAlignment: String;
        FTextPadding: Single;
        FTextKeepUpright: Boolean;
        FTextMaxAngle: Single;
        FTextLineHeight: Single;
        FTextWritingMode: TArray<String>;
        FTextVariableAnchor: TArray<String>;
        FTextTranslate: TPointF;
        FTextTranslateAnchor: String;
        FVisibility: String;
      public
        class operator Initialize (out Dest: TSymbolLayerOptions);
        class function Create: TSymbolLayerOptions; static;
        function SetSymbolPlacement(const AValue: String): PSymbolLayerOptions;
        function SetSymbolSpacing(const AValue: Single): PSymbolLayerOptions;
        function SetSymbolAvoidEdges(const AValue: Boolean): PSymbolLayerOptions;
        function SetSymbolZOrder(const AValue: String): PSymbolLayerOptions;
        function SetIconAllowOverlap(const AValue: Boolean): PSymbolLayerOptions; // → Can MY icon appear even if another symbol is already there?
        function SetIconIgnorePlacement(const AValue: Boolean): PSymbolLayerOptions; // → Does MY icon prevent other symbols from appearing?
        function SetIconOptional(const AValue: Boolean): PSymbolLayerOptions; // → Can MY text appear without my icon?
        function SetIconRotationAlignment(const AValue: String): PSymbolLayerOptions;
        function SetIconPitchAlignment(const AValue: String): PSymbolLayerOptions;
        function SetIconPadding(const AValue: Single): PSymbolLayerOptions;
        function SetIconKeepUpright(const AValue: Boolean): PSymbolLayerOptions;
        function SetIconTextFit(const AValue: String): PSymbolLayerOptions;
        function SetIconTextFitPadding(const AValue: TRectF): PSymbolLayerOptions;
        function SetIconTranslate(const AValue: TPointF): PSymbolLayerOptions;
        function SetIconTranslateAnchor(const AValue: String): PSymbolLayerOptions;
        function SetTextAllowOverlap(const AValue: Boolean): PSymbolLayerOptions; // → Can MY text appear even if another symbol is already there?
        function SetTextIgnorePlacement(const AValue: Boolean): PSymbolLayerOptions; // → Does MY text prevent other symbols from appearing?
        function SetTextOptional(const AValue: Boolean): PSymbolLayerOptions; // → Can MY icon appear without my text?
        function SetTextFont(const AValue: TArray<String>): PSymbolLayerOptions;
        function SetTextRotationAlignment(const AValue: String): PSymbolLayerOptions;
        function SetTextPitchAlignment(const AValue: String): PSymbolLayerOptions;
        function SetTextPadding(const AValue: Single): PSymbolLayerOptions;
        function SetTextKeepUpright(const AValue: Boolean): PSymbolLayerOptions;
        function SetTextMaxAngle(const AValue: Single): PSymbolLayerOptions;
        function SetTextLineHeight(const AValue: Single): PSymbolLayerOptions;
        function SetTextWritingMode(const AValue: TArray<String>): PSymbolLayerOptions;
        function SetTextVariableAnchor(const AValue: TArray<String>): PSymbolLayerOptions;
        function SetTextTranslate(const AValue: TPointF): PSymbolLayerOptions;
        function SetTextTranslateAnchor(const AValue: String): PSymbolLayerOptions;
        function SetVisibility(const AValue: String): PSymbolLayerOptions;
      end;
      // -----------------
      // TLineLayerOptions
      PLineLayerOptions = ^TLineLayerOptions;
      TLineLayerOptions = record
      private
        FLineCap: String;
        FLineJoin: String;
        FLineMiterLimit: Single;
        FLineRoundLimit: Single;
        FLineDasharray: TArray<Single>;
        FLineTranslate: TPointF;
        FLineTranslateAnchor: String;
        FVisibility: String;
      public
        class operator Initialize(out Dest: TLineLayerOptions);
        class function Create: TLineLayerOptions; static;
        function SetLineCap(const AValue: String): PLineLayerOptions;
        function SetLineJoin(const AValue: String): PLineLayerOptions;
        function SetLineMiterLimit(const AValue: Single): PLineLayerOptions;
        function SetLineRoundLimit(const AValue: Single): PLineLayerOptions;
        function SetLineDasharray(const AValue: TArray<Single>): PLineLayerOptions;
        function SetLineTranslate(const AValue: TPointF): PLineLayerOptions;
        function SetLineTranslateAnchor(const AValue: String): PLineLayerOptions;
        function SetVisibility(const AValue: String): PLineLayerOptions;
      end;
      // -------------------
      // TCircleLayerOptions
      PCircleLayerOptions = ^TCircleLayerOptions;
      TCircleLayerOptions = record
      private
        FCirclePitchAlignment: String;
        FCirclePitchScale: String;
        FCircleTranslate: TPointF;
        FCircleTranslateAnchor: String;
        FVisibility: String;
      public
        class operator Initialize(out Dest: TCircleLayerOptions);
        class function Create: TCircleLayerOptions; static;
        function SetCirclePitchAlignment(const AValue: String): PCircleLayerOptions;
        function SetCirclePitchScale(const AValue: String): PCircleLayerOptions;
        function SetCircleTranslate(const AValue: TPointF): PCircleLayerOptions;
        function SetCircleTranslateAnchor(const AValue: String): PCircleLayerOptions;
        function SetVisibility(const AValue: String): PCircleLayerOptions;
      end;
      // -----------------
      // TFillLayerOptions
      PFillLayerOptions = ^TFillLayerOptions;
      TFillLayerOptions = record
      private
        FFillAntialias: Boolean;
        FFillTranslate: TPointF;
        FFillTranslateAnchor: String;
        FFillPattern: String;
        FVisibility: String;
      public
        class operator Initialize(out Dest: TFillLayerOptions);
        class function Create: TFillLayerOptions; static;
        function SetFillAntialias(const AValue: Boolean): PFillLayerOptions;
        function SetFillTranslate(const AValue: TPointF): PFillLayerOptions;
        function SetFillTranslateAnchor(const AValue: String): PFillLayerOptions;
        function SetFillPattern(const AValue: String): PFillLayerOptions;
        function SetVisibility(const AValue: String): PFillLayerOptions;
      end;
      // --------------------------
      // TFillExtrusionLayerOptions
      PFillExtrusionLayerOptions = ^TFillExtrusionLayerOptions;
      TFillExtrusionLayerOptions = record
      private
        FFillExtrusionOpacity: Single;
        FFillExtrusionVerticalGradient: Boolean;
        FFillExtrusionTranslate: TPointF;
        FFillExtrusionTranslateAnchor: String;
        FVisibility: String;
      public
        class operator Initialize(out Dest: TFillExtrusionLayerOptions);
        class function Create: TFillExtrusionLayerOptions; static;
        function SetFillExtrusionOpacity(const AValue: Single): PFillExtrusionLayerOptions;
        function SetFillExtrusionVerticalGradient(const AValue: Boolean): PFillExtrusionLayerOptions;
        function SetFillExtrusionTranslate(const AValue: TPointF): PFillExtrusionLayerOptions;
        function SetFillExtrusionTranslateAnchor(const AValue: String): PFillExtrusionLayerOptions;
        function SetVisibility(const AValue: String): PFillExtrusionLayerOptions;
      end;
      // --------------------
      // THeatmapLayerOptions
      PHeatmapLayerOptions = ^THeatmapLayerOptions;
      THeatmapLayerOptions = record
      private
        FHeatmapRadius: Single;
        FHeatmapIntensity: Single;
        FHeatmapOpacity: Single;
        FVisibility: String;
      public
        class operator Initialize(out Dest: THeatmapLayerOptions);
        class function Create: THeatmapLayerOptions; static;
        function SetHeatmapRadius(const AValue: Single): PHeatmapLayerOptions;
        function SetHeatmapIntensity(const AValue: Single): PHeatmapLayerOptions;
        function SetHeatmapOpacity(const AValue: Single): PHeatmapLayerOptions;
        function SetVisibility(const AValue: String): PHeatmapLayerOptions;
      end;
      // -------------------
      // TRasterLayerOptions
      PRasterLayerOptions = ^TRasterLayerOptions;
      TRasterLayerOptions = record
      private
        FRasterOpacity: Single;
        FRasterHueRotate: Single;
        FRasterBrightnessMin: Single;
        FRasterBrightnessMax: Single;
        FRasterSaturation: Single;
        FRasterContrast: Single;
        FRasterResampling: String;
        FRasterFadeDuration: Single;
        FVisibility: String;
      public
        class operator Initialize(out Dest: TRasterLayerOptions);
        class function Create: TRasterLayerOptions; static;
        function SetRasterOpacity(const AValue: Single): PRasterLayerOptions;
        function SetRasterHueRotate(const AValue: Single): PRasterLayerOptions;
        function SetRasterBrightnessMin(const AValue: Single): PRasterLayerOptions;
        function SetRasterBrightnessMax(const AValue: Single): PRasterLayerOptions;
        function SetRasterSaturation(const AValue: Single): PRasterLayerOptions;
        function SetRasterContrast(const AValue: Single): PRasterLayerOptions;
        function SetRasterResampling(const AValue: String): PRasterLayerOptions;
        function SetRasterFadeDuration(const AValue: Single): PRasterLayerOptions;
        function SetVisibility(const AValue: String): PRasterLayerOptions;
      end;
      // ----------------
      // TOrnamentGravity
      TOrnamentGravity = (TopLeft, TopRight, BottomLeft, BottomRight);
      // --------
      // TCompass
      TCompass = class(TALPersistentObserver)
      public
        Type
          TMargins = class(TALBounds)
          protected
            function GetDefaultValue: TRectF; override;
          end;
      private
        FEnabled: Boolean; // 1 byte
        FFadeFacingNorth: Boolean; // 1 byte
        FGravity: TOrnamentGravity; // 1 byte
        FResourceName: String; // 8 bytes
        FWidth: Single; // 4 bytes
        FHeight: Single; // 4 bytes
        FMargins: TALBounds; // 8 bytes
        procedure SetEnabled(const AValue: Boolean);
        procedure SetFadeFacingNorth(const AValue: Boolean);
        procedure SetGravity(const AValue: TOrnamentGravity);
        procedure SetResourceName(const AValue: String);
        procedure SetWidth(const AValue: Single);
        procedure SetHeight(const AValue: Single);
        procedure SetMargins(const AValue: TALBounds);
        procedure MarginsChanged(Sender: TObject);
        function IsEnabledStored: Boolean;
        function IsFadeFacingNorthStored: Boolean;
        function IsGravityStored: Boolean;
        function IsResourceNameStored: Boolean;
        function IsWidthStored: Boolean;
        function IsHeightStored: Boolean;
      protected
        function CreateMargins: TALBounds; virtual;
        function GetDefaultEnabled: Boolean; virtual;
        function GetDefaultFadeFacingNorth: Boolean; virtual;
        function GetDefaultGravity: TOrnamentGravity; virtual;
        function GetDefaultResourceName: String; virtual;
        function GetDefaultWidth: Single; virtual;
        function GetDefaultHeight: Single; virtual;
      public
        constructor Create; override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        property DefaultEnabled: Boolean read GetDefaultEnabled;
        property DefaultFadeFacingNorth: Boolean read GetDefaultFadeFacingNorth;
        property DefaultGravity: TOrnamentGravity read GetDefaultGravity;
        property DefaultResourceName: String read GetDefaultResourceName;
        property DefaultWidth: Single read GetDefaultWidth;
        property DefaultHeight: Single read GetDefaultHeight;
      published
        property Enabled: Boolean read FEnabled write SetEnabled stored IsEnabledStored;
        property FadeFacingNorth: Boolean read FFadeFacingNorth write SetFadeFacingNorth stored IsFadeFacingNorthStored;
        property Gravity: TOrnamentGravity read FGravity write SetGravity stored IsGravityStored;
        property ResourceName: String read FResourceName write SetResourceName stored IsResourceNameStored;
        property Width: Single read FWidth write SetWidth stored IsWidthStored nodefault;
        property Height: Single read FHeight write SetHeight stored IsHeightStored nodefault;
        property Margins: TALBounds read FMargins write SetMargins;
      end;
  private
    FStyleJSON: String;
    FStyleUrl: String;
    FCompass: TCompass;
    FOnMapReady: TNotifyEvent;
    FOnStyleLoaded: TNotifyEvent;
    FOnCameraWillChange: TNotifyEvent;
    FOnCameraIsChanging: TNotifyEvent;
    FOnCameraDidChange: TNotifyEvent;
    FOnSymbolClick: TSymbolClickEvent;
    FOnMapClick: TMapClickEvent;
    {$IF defined(android)}
    function GetNativeView: TALAndroidMapLibreView;
    {$ELSEIF defined(IOS)}
    function GetNativeView: TALIosMapLibreView;
    {$ELSEIF defined(ALMacOS)}
    function GetNativeView: TALMacMapLibreView;
    {$ELSEIF defined(MSWindows)}
    function GetNativeView: TALWinMapLibreView;
    {$ENDIF}
    procedure SetStyleURL(const AValue: string);
    procedure SetStyleJSON(const AValue: string);
    procedure SetCompass(const AValue: TCompass);
    procedure CompassChanged(Sender: TObject);
  protected
    {$IF defined(android)}
    Function CreateNativeView: TALAndroidNativeView; override;
    {$ELSEIF defined(IOS)}
    Function CreateNativeView: TALIosNativeView; override;
    {$ELSEIF defined(ALMacOS)}
    Function CreateNativeView: TALMacNativeView; override;
    {$ELSEIF defined(MSWindows)}
    Function CreateNativeView: TALWinNativeView; override;
    {$ENDIF}
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IF defined(android)}
    property NativeView: TALAndroidMapLibreView read GetNativeView;
    {$ELSEIF defined(IOS)}
    property NativeView: TALIosMapLibreView read GetNativeView;
    {$ELSEIF defined(ALMacOS)}
    property NativeView: TALMacMapLibreView read GetNativeView;
    {$ELSEIF defined(MSWindows)}
    property NativeView: TALWinMapLibreView read GetNativeView;
    {$ENDIF}
    procedure SetCenterCoordinate(const ALatitude, ALongitude: Double; const AZoomLevel: double);
    procedure SetImage(
                const AStream: TStream;
                const AScale: Single;
                const W, H: single;
                const AName: String); overload;
    procedure SetImage(
                const AResourceName: String;
                const AScale: Single;
                const W, H: single;
                const AName: String); overload;
    procedure RemoveImage(const AName: String);
    procedure SetSymbolLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PSymbolLayerOptions = nil);
    procedure SetLineLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PLineLayerOptions = nil);
    procedure SetCircleLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PCircleLayerOptions = nil);
    procedure SetFillLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillLayerOptions = nil);
    procedure SetFillExtrusionLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillExtrusionLayerOptions = nil);
    procedure SetHeatmapLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PHeatmapLayerOptions = nil);
    procedure SetRasterLayer(const AId: String; const ATileURL: String; const AIsTileJSON: Boolean = False; const ATileSize: Integer = 256; const AOptions: TALMapLibre.PRasterLayerOptions = nil);
    procedure RemoveLayer(const AId: String);
    function GetViewportBounds: TALRectD;
    function GetCameraPosition: TCameraPosition;
    {$IF defined(android)}
    procedure ShowLoadingSpinner(const AView: JView); overload;
    {$ELSEIF defined(IOS)}
    procedure ShowLoadingSpinner(const AView: UIView); overload;
    {$ELSEIF defined(ALMacOS)}
    procedure ShowLoadingSpinner(const AHTML: String); overload;
    {$ELSEIF defined(MSWindows)}
    procedure ShowLoadingSpinner(const AHTML: String); overload;
    {$ENDIF}
    procedure ShowLoadingSpinner(const AColor: TAlphaColor = Talphacolors.null; const AWidth: Single = 0; const AHeight: Single = 0); overload;
    procedure HideLoadingSpinner;
  published
    //property Action;
    property Align;
    property Anchors;
    //property AutoSize;
    property CanFocus default True;
    //property CanParentFocus;
    //property ClickSound;
    //property ClipChildren;
    //property ClipParent;
    property Compass: TCompass read FCompass write SetCompass;
    //property Cursor;
    //property DisableFocusEffect;
    //property DoubleBuffered;
    //property DragMode;
    //property EnableDragHighlight;
    //property Enabled;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    //property HitTest;
    //property Locked;
    property Margins;
    //property Opacity;
    //property Padding;
    //property PopupMenu;
    property Position;
    //property ReadOnly;
    //property RotationAngle;
    //property RotationCenter;
    //property Pivot;
    //property Scale;
    property Size;
    property StyleJSON: string read FStyleJSON write SetStyleJSON;
    property StyleURL: string read FStyleURL write SetStyleURL;
    //property TabOrder;
    //property TabStop;
    //property TouchTargetExpansion;
    property Visible;
    property Width;
    //property OnCanFocus;
    //property OnChange;
    //property OnDragEnter;
    //property OnDragLeave;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMapReady: TNotifyEvent read FOnMapReady write FOnMapReady;
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
    //property OnPainting;
    //property OnPaint;
    //property OnResize;
    property OnResized;
    property OnStyleLoaded: TNotifyEvent read FOnStyleLoaded write FOnStyleLoaded;
    property OnCameraWillChange: TNotifyEvent read FOnCameraWillChange write FOnCameraWillChange;
    property OnCameraIsChanging: TNotifyEvent read FOnCameraIsChanging write FOnCameraIsChanging;
    property OnCameraDidChange: TNotifyEvent read FOnCameraDidChange write FOnCameraDidChange;
    property OnSymbolClick: TSymbolClickEvent read FOnSymbolClick write FOnSymbolClick;
    property OnMapClick: TMapClickEvent read FOnMapClick write FOnMapClick;
  end;

{$REGION ' ANDROID'}
{$IF defined(android)}

  {**************************************************}
  TALAndroidMapLibreView = class(TALAndroidNativeView)
  private
    type
      // -------------------
      // TOnMapReadyCallback
      TOnMapReadyCallback = class(TJavaLocal, JOnMapReadyCallback)
      private
        FMapLibreView: TALAndroidMapLibreView;
      public
        constructor Create(const AMapLibreView: TALAndroidMapLibreView);
        procedure onMapReady(maplibreMap: JMapLibreMap); cdecl;
      end;
      // --------------------------------
      // TOnDidFinishLoadingStyleListener
      TOnDidFinishLoadingStyleListener = class(TJavaLocal, JMapView_OnDidFinishLoadingStyleListener)
      private
        FMapLibreView: TALAndroidMapLibreView;
      public
        constructor Create(const AMapLibreView: TALAndroidMapLibreView);
        procedure onDidFinishLoadingStyle; cdecl;
      end;
      // ---------------------------
      // TOnCameraMoveStartedListener
      TOnCameraMoveStartedListener = class(TJavaLocal, JMapLibreMap_OnCameraMoveStartedListener)
      private
        FMapLibreView: TALAndroidMapLibreView;
      public
        constructor Create(const AMapLibreView: TALAndroidMapLibreView);
        procedure onCameraMoveStarted(reason: Integer); cdecl;
      end;
      // ---------------------------
      // TOnCameraMoveListener
      TOnCameraMoveListener = class(TJavaLocal, JMapLibreMap_OnCameraMoveListener)
      private
        FMapLibreView: TALAndroidMapLibreView;
      public
        constructor Create(const AMapLibreView: TALAndroidMapLibreView);
        procedure onCameraMove; cdecl;
      end;
      // --------------------------
      // TOnCameraIdleListener
      TOnCameraIdleListener = class(TJavaLocal, JMapLibreMap_OnCameraIdleListener)
      private
        FMapLibreView: TALAndroidMapLibreView;
      public
        constructor Create(const AMapLibreView: TALAndroidMapLibreView);
        procedure onCameraIdle; cdecl;
      end;
      // -------------------
      // TOnMapClickListener
      TOnMapClickListener = class(TJavaLocal, JMapLibreMap_OnMapClickListener)
      private
        FMapLibreView: TALAndroidMapLibreView;
      public
        constructor Create(const AMapLibreView: TALAndroidMapLibreView);
        function onMapClick(point: JLatLng): Boolean; cdecl;
      end;
  private
    FMaplibreMap: JMapLibreMap;
    FLoadingSpinnerView: JView;
    FSymbolLayerIds: TALStringListW;
    FOnDidFinishLoadingStyleListener: TOnDidFinishLoadingStyleListener;
    FOnCameraMoveStartedListener: TOnCameraMoveStartedListener;
    FOnCameraMoveListener: TOnCameraMoveListener;
    FOnCameraIdleListener: TOnCameraIdleListener;
    FOnMapClickListener: TOnMapClickListener;
    FOnMapReadyCallback: TOnMapReadyCallback;
    function GetView: JMapView;
    function GetControl: TALMapLibre;
    procedure ApplicationEventHandler(const Sender: TObject; const M: TMessage);
  protected
    function CreateView: JView; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property View: JMapView read GetView;
    property Control: TALMapLibre read GetControl;
    procedure Draw(const ACanvas: JCanvas); override;
    procedure SetVisible(const Value: Boolean); override;
    procedure ShowLoadingSpinner(const AView: JView); overload;
    procedure ShowLoadingSpinner(const AColor: TAlphaColor = Talphacolors.null; const AWidth: Single = 0; const AHeight: Single = 0); overload;
    procedure HideLoadingSpinner;
    procedure SetCenterCoordinate(const ALatitude, ALongitude: Double; const AZoomLevel: double);
    procedure SetImage(
                const AResourceName: String;
                const AStream: TStream;
                const AScale: Single;
                const W, H: single;
                const AName: String);
    procedure RemoveImage(const AName: String);
    procedure SetSymbolLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PSymbolLayerOptions = nil);
    procedure SetLineLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PLineLayerOptions = nil);
    procedure SetCircleLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PCircleLayerOptions = nil);
    procedure SetFillLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillLayerOptions = nil);
    procedure SetFillExtrusionLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillExtrusionLayerOptions = nil);
    procedure SetHeatmapLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PHeatmapLayerOptions = nil);
    procedure SetRasterLayer(const AId: String; const ATileURL: String; const AIsTileJSON: Boolean = False; const ATileSize: Integer = 256; const AOptions: TALMapLibre.PRasterLayerOptions = nil);
    procedure RemoveLayer(const AId: String);
    function GetViewportBounds: TALRectD;
    function GetCameraPosition: TALMapLibre.TCameraPosition;
    procedure ApplyStyle;
    procedure ApplyCompass;
  end;

{$endif}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(ios)}

  {****************************************}
  IALIosMapLibreView = interface(MLNMapView)
    ['{296C1058-9A89-4D71-A5B6-9F0EF1681435}']
  end;

  {******************************************}
  TALIosMapLibreView = class(TALIosNativeView)
  private
    type
      // -----------
      // ITapHandler
      IMapViewTapHandler = interface(IObjectiveC)
        ['{B2608976-1C44-4573-AD58-8E441F9EC7FD}']
        procedure mapViewTap(sender: UITapGestureRecognizer); cdecl;
      end;
      // -------------------
      // TNavigationDelegate
      TMapViewDelegate = class(TOCLocal, MLNMapViewDelegate, IMapViewTapHandler)
      private
        FMapLibreView: TALIosMapLibreView;
      public
        constructor Create(const AMapLibreView: TALIosMapLibreView);
        procedure mapViewTap(sender: UITapGestureRecognizer); cdecl;
        [MethodName('mapView:regionWillChangeWithReason:animated:')]
        procedure mapViewRegionWillChangeWithReasonAnimated(mapView: MLNMapView; regionWillChangeWithReason: MLNCameraChangeReason; animated: Boolean); cdecl;
        [MethodName('mapView:regionIsChangingWithReason:')]
        procedure mapViewRegionIsChangingWithReason(mapView: MLNMapView; regionIsChangingWithReason: MLNCameraChangeReason); cdecl;
        [MethodName('mapView:regionDidChangeWithReason:animated:')]
        procedure mapViewRegionDidChangeWithReasonAnimated(mapView: MLNMapView; regionDidChangeWithReason: MLNCameraChangeReason; animated: Boolean); cdecl;
        [MethodName('mapView:didFinishLoadingStyle:')]
        procedure mapViewDidFinishLoadingStyle(mapView: MLNMapView; didFinishLoadingStyle: MLNStyle); cdecl;
      end;
  private
    FThreadQueueCleaner: TALThreadQueueCleaner;
    FMapViewDelegate: TMapViewDelegate;
    FMapViewTapGestureRecognizer: UITapGestureRecognizer;
    FLoadingSpinnerView: UIView;
    FSymbolLayerIds: TALStringListW;
    function GetView: MLNMapView;
    function GetControl: TALMapLibre;
  protected
    procedure InitView; override;
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property View: MLNMapView read GetView;
    procedure ShowLoadingSpinner(const AView: UIView); overload;
    procedure ShowLoadingSpinner(const AColor: TAlphaColor = Talphacolors.null; const AWidth: Single = 0; const AHeight: Single = 0); overload;
    procedure HideLoadingSpinner;
    property Control: TALMapLibre read GetControl;
    procedure SetCenterCoordinate(const ALatitude, ALongitude: Double; const AZoomLevel: double);
    procedure SetImage(
                const AResourceName: String;
                const AStream: TStream;
                const AScale: Single;
                const W, H: single;
                const AName: String);
    procedure RemoveImage(const AName: String);
    procedure SetSymbolLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PSymbolLayerOptions = nil);
    procedure SetLineLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PLineLayerOptions = nil);
    procedure SetCircleLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PCircleLayerOptions = nil);
    procedure SetFillLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillLayerOptions = nil);
    procedure SetFillExtrusionLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillExtrusionLayerOptions = nil);
    procedure SetHeatmapLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PHeatmapLayerOptions = nil);
    procedure SetRasterLayer(const AId: String; const ATileURL: String; const AIsTileJSON: Boolean = False; const ATileSize: Integer = 256; const AOptions: TALMapLibre.PRasterLayerOptions = nil);
    procedure RemoveLayer(const AId: String);
    function GetViewportBounds: TALRectD;
    function GetCameraPosition: TALMapLibre.TCameraPosition;
    procedure ApplyStyle;
    procedure ApplyCompass;
  end;

{$endif}
{$ENDREGION}

{$REGION ' MacOS'}
{$IF defined(ALMacOS)}

  {**********************************************}
  TALMacMapLibreView = class(TALMacWebBrowserView)
  private
    function GetControl: TALMapLibre;
  public
    property Control: TALMapLibre read GetControl;
    procedure ShowLoadingSpinner(const AHTML: String); overload;
    procedure ShowLoadingSpinner(const AColor: TAlphaColor = Talphacolors.null; const AWidth: Single = 0; const AHeight: Single = 0); overload;
    procedure HideLoadingSpinner;
    procedure SetCenterCoordinate(const ALatitude, ALongitude: Double; const AZoomLevel: double);
    procedure SetImage(
                const AResourceName: String;
                const AStream: TStream;
                const AScale: Single;
                const W, H: single;
                const AName: String);
    procedure RemoveImage(const AName: String);
    procedure SetSymbolLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PSymbolLayerOptions = nil);
    procedure SetLineLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PLineLayerOptions = nil);
    procedure SetCircleLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PCircleLayerOptions = nil);
    procedure SetFillLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillLayerOptions = nil);
    procedure SetFillExtrusionLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillExtrusionLayerOptions = nil);
    procedure SetHeatmapLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PHeatmapLayerOptions = nil);
    procedure SetRasterLayer(const AId: String; const ATileURL: String; const AIsTileJSON: Boolean = False; const ATileSize: Integer = 256; const AOptions: TALMapLibre.PRasterLayerOptions = nil);
    procedure RemoveLayer(const AId: String);
    function GetViewportBounds: TALRectD;
    function GetCameraPosition: TALMapLibre.TCameraPosition;
    procedure ApplyStyle;
    procedure ApplyCompass;
  end;

{$endif}
{$ENDREGION}

{$REGION ' MSWINDOWS'}
{$IF defined(MSWINDOWS)}

  {**********************************************}
  TALWinMapLibreView = class(TALWinWebBrowserView)
  private
    FThreadQueueCleaner: TALThreadQueueCleaner;
    FMapReady: Boolean;
    FApplyStyleWhenMapReady: Boolean;
    function GetControl: TALMapLibre;
    procedure SetGeoJsonLayer(
                const AId: String;
                const AType: String;
                const AGeoJSON: String;
                const ALayout: String;
                const APaint: String;
                const AIsSymbolLayer: Boolean);
  protected
    function GetHtml: String;
    procedure DoWebMessage(const AMessage: string); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Control: TALMapLibre read GetControl;
    procedure ShowLoadingSpinner(const AHTML: String); overload;
    procedure ShowLoadingSpinner(const AColor: TAlphaColor = Talphacolors.null; const AWidth: Single = 0; const AHeight: Single = 0); overload;
    procedure HideLoadingSpinner;
    procedure SetCenterCoordinate(const ALatitude, ALongitude: Double; const AZoomLevel: double);
    procedure SetImage(
                const AResourceName: String;
                const AStream: TStream;
                const AScale: Single;
                const W, H: single;
                const AName: String);
    procedure RemoveImage(const AName: String);
    procedure SetSymbolLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PSymbolLayerOptions = nil);
    procedure SetLineLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PLineLayerOptions = nil);
    procedure SetCircleLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PCircleLayerOptions = nil);
    procedure SetFillLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillLayerOptions = nil);
    procedure SetFillExtrusionLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillExtrusionLayerOptions = nil);
    procedure SetHeatmapLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PHeatmapLayerOptions = nil);
    procedure SetRasterLayer(const AId: String; const ATileURL: String; const AIsTileJSON: Boolean = False; const ATileSize: Integer = 256; const AOptions: TALMapLibre.PRasterLayerOptions = nil);
    procedure RemoveLayer(const AId: String);
    function GetViewportBounds: TALRectD;
    function GetCameraPosition: TALMapLibre.TCameraPosition;
    procedure ApplyStyle;
    procedure ApplyCompass;
  end;

{$endif}
{$ENDREGION}

procedure Register;

implementation

uses
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  System.Generics.Collections,
  {$IF defined(ANDROID)}
  System.Rtti,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Widget,
  Androidapi.Jni,
  Androidapi.Helpers,
  Alcinoe.AndroidApi.Widget,
  FMX.Platform,
  {$ENDIF}
  {$IF defined(IOS)}
  iOSapi.CoreGraphics,
  iOSApi.Foundation,
  iOSapi.CoreLocation,
  Macapi.ObjCRuntime,
  Macapi.Helpers,
  FMX.Helpers.iOS,
  Alcinoe.iOSApi.UIKit,
  {$ENDIF}
  {$IF defined(MSWindows)}
  WinApi.Windows,
  System.Skia.API,
  Fmx.Forms,
  FMX.Graphics,
  FMX.Platform.Win,
  Alcinoe.HTML,
  Alcinoe.Localization,
  {$ENDIF}
  FMX.Types,
  {$IFDEF ALDPK}
  DesignIntf,
  {$ENDIF}
  Alcinoe.FMX.Graphics,
  Alcinoe.JSONDoc,
  Alcinoe.StringUtils;

type
  TALLayerDataKey = (
    // Symbol layer
    IconImage,
    IconColor,
    IconOpacity,
    IconSize,
    IconAnchor,
    IconOffset,
    IconRotate,
    IconHaloColor,
    IconHaloWidth,
    IconHaloBlur,
    Text,
    TextColor,
    TextOpacity,
    TextSize,
    TextLetterSpacing,
    TextJustify,
    TextAnchor,
    TextOffset,
    TextMaxWidth,
    TextHaloColor,
    TextHaloWidth,
    TextHaloBlur,
    TextRotate,
    TextRadialOffset,
    TextTransform,
    SymbolSortKey,
    // Line layer
    LineColor,
    LineOpacity,
    LineWidth,
    LineBlur,
    LineGapWidth,
    LineOffset,
    LineSortKey,
    // Circle layer
    CircleColor,
    CircleOpacity,
    CircleRadius,
    CircleBlur,
    CircleStrokeColor,
    CircleStrokeOpacity,
    CircleStrokeWidth,
    CircleSortKey,
    // Fill layer
    FillColor,
    FillOpacity,
    FillOutlineColor,
    FillSortKey,
    // FillExtrusion layer
    FillExtrusionColor,
    FillExtrusionHeight,
    FillExtrusionBase,
    // Heatmap layer
    HeatmapWeight);
  TALLayerDataKeys = set of TALLayerDataKey;

const
  ALLayerDataKeyNames: array[TALLayerDataKey] of String = (
    // Symbol layer
    '_icon_image',
    '_icon_color',
    '_icon_opacity',
    '_icon_size',
    '_icon_anchor',
    '_icon_offset',
    '_icon_rotate',
    '_icon_halo_color',
    '_icon_halo_width',
    '_icon_halo_blur',
    '_text',
    '_text_color',
    '_text_opacity',
    '_text_size',
    '_text_letter_spacing',
    '_text_justify',
    '_text_anchor',
    '_text_offset',
    '_text_max_width',
    '_text_halo_color',
    '_text_halo_width',
    '_text_halo_blur',
    '_text_rotate',
    '_text_radial_offset',
    '_text_transform',
    '_symbol_sort_key',
    // Line layer
    '_line_color',
    '_line_opacity',
    '_line_width',
    '_line_blur',
    '_line_gap_width',
    '_line_offset',
    '_line_sort_key',
    // Circle layer
    '_circle_color',
    '_circle_opacity',
    '_circle_radius',
    '_circle_blur',
    '_circle_stroke_color',
    '_circle_stroke_opacity',
    '_circle_stroke_width',
    '_circle_sort_key',
    // Fill layer
    '_fill_color',
    '_fill_opacity',
    '_fill_outline_color',
    '_fill_sort_key',
    // FillExtrusion layer
    '_fill_extrusion_color',
    '_fill_extrusion_height',
    '_fill_extrusion_base',
    // Heatmap layer
    '_heatmap_weight');

  ALSymbolLayerDataKeys = [TALLayerDataKey.IconImage..TALLayerDataKey.SymbolSortKey];
  ALLineLayerDataKeys = [TALLayerDataKey.LineColor..TALLayerDataKey.LineSortKey];
  ALCircleLayerDataKeys = [TALLayerDataKey.CircleColor..TALLayerDataKey.CircleSortKey];
  ALFillLayerDataKeys = [TALLayerDataKey.FillColor..TALLayerDataKey.FillSortKey];
  ALFillExtrusionLayerDataKeys = [TALLayerDataKey.FillExtrusionColor..TALLayerDataKey.FillExtrusionBase];
  ALHeatmapLayerDataKeys = [TALLayerDataKey.HeatmapWeight..TALLayerDataKey.HeatmapWeight];

{****************************************************************************}
// Returns the data-driven keys (_icon_image, _line_color, _circle_color, ...)
// present in the "properties" of the first feature of the GeoJSON, restricted
// to AAllowedKeys. Only those keys must be bound to the layer: binding a key
// that is absent from the data is not harmless in MapLibre (the property is
// considered "set").
function ALGetLayerDataKeys(const AGeoJSON: String; const AAllowedKeys: TALLayerDataKeys): TALLayerDataKeys;
begin
  var LKeys: TALLayerDataKeys := [];
  var LDictionary := TDictionary<String, TALLayerDataKey>.Create;
  try

    for var LKey in AAllowedKeys do
      LDictionary.Add('features[0].properties.' + ALLayerDataKeyNames[LKey], LKey);

    TALJsonDocumentW.ParseJSONString(
      AGeoJSON,
      // OnParseText
      procedure(Sender: TObject; const Path: String; const Name: String; const Args: array of const; NodeSubType: TALJSONNodeSubType; var Abort: Boolean)
      begin
        if (Name = '') or (Name[Low(Name)] <> '_') then Exit;
        var LKey: TALLayerDataKey;
        if LDictionary.TryGetValue(Path, LKey) then Include(LKeys, LKey);
      end,
      // OnParseStartObject
      procedure(Sender: TObject; const Path: String; const Name: String; var Abort: Boolean)
      begin
        if (Name = '') or (Name[Low(Name)] <> '_') then Exit;
        var LKey: TALLayerDataKey;
        if LDictionary.TryGetValue(Path, LKey) then Include(LKeys, LKey);
      end,
      // OnParseEndObject
      procedure(Sender: TObject; const Path: String; const Name: String; var Abort: Boolean)
      begin
        if Path = 'features[0]' then Abort := True;
      end,
      // OnParseStartArray
      procedure(Sender: TObject; const Path: String; const Name: String; var Abort: Boolean)
      begin
        if (Name = '') or (Name[Low(Name)] <> '_') then Exit;
        var LKey: TALLayerDataKey;
        if LDictionary.TryGetValue(Path, LKey) then Include(LKeys, LKey);
      end,
      // OnParseEndArray
      nil);

    Result := LKeys;

  finally
    LDictionary.Free;
  end;
end;

{*********************************************}
constructor TALMapLibre.TCameraPosition.Create(
              const ABearing: Double;
              const APadding: TALRectD;
              const ARoll: Double;
              const ATarget: TALLatLng;
              const ATilt: Double;
              const AZoom: Double);
begin
  bearing := ABearing;
  padding := APadding;
  roll := ARoll;
  target := ATarget;
  tilt := ATilt;
  zoom := AZoom;
end;

{****************************************************************************************************}
class operator TALMapLibre.TSymbolLayerOptions.Initialize (out Dest: TALMapLibre.TSymbolLayerOptions);
begin
  // Because of :
  // https://stackoverflow.com/questions/79318689/why-does-copyrecord-get-called-in-some-cases-with-tmyrec-but-not-others
end;

{*************************************************************************}
class function TALMapLibre.TSymbolLayerOptions.Create: TSymbolLayerOptions;
begin
  Result.FSymbolPlacement := 'point';
  Result.FSymbolSpacing := 250;
  Result.FSymbolAvoidEdges := False;
  Result.FSymbolZOrder := 'auto';
  Result.FIconAllowOverlap := False;
  Result.FIconIgnorePlacement := False;
  Result.FIconOptional := False;
  Result.FIconRotationAlignment := 'auto';
  Result.FIconPitchAlignment := 'auto';
  Result.FIconPadding := 2;
  Result.FIconKeepUpright := False;
  Result.FIconTextFit := 'none';
  Result.FIconTextFitPadding := TRectF.Empty;
  Result.FIconTranslate := TPointF.Zero;
  Result.FIconTranslateAnchor := 'map';
  Result.FTextAllowOverlap := False;
  Result.FTextIgnorePlacement := False;
  Result.FTextOptional := False;
  Result.FTextFont := nil;
  Result.FTextRotationAlignment := 'auto';
  Result.FTextPitchAlignment := 'auto';
  Result.FTextPadding := 2;
  Result.FTextKeepUpright := True;
  Result.FTextMaxAngle := 45;
  Result.FTextLineHeight := 1.2;
  Result.FTextWritingMode := nil;
  Result.FTextVariableAnchor := nil;
  Result.FTextTranslate := TPointF.Zero;
  Result.FTextTranslateAnchor := 'map';
  Result.FVisibility := 'visible';
end;

{*****************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetSymbolPlacement(const AValue: String): PSymbolLayerOptions;
begin
  FSymbolPlacement := AValue;
  Result := @Self;
end;

{***************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetSymbolSpacing(const AValue: Single): PSymbolLayerOptions;
begin
  FSymbolSpacing := AValue;
  Result := @Self;
end;

{*******************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetSymbolAvoidEdges(const AValue: Boolean): PSymbolLayerOptions;
begin
  FSymbolAvoidEdges := AValue;
  Result := @Self;
end;

{**************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetSymbolZOrder(const AValue: String): PSymbolLayerOptions;
begin
  FSymbolZOrder := AValue;
  Result := @Self;
end;

{*******************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetIconAllowOverlap(const AValue: Boolean): PSymbolLayerOptions;
begin
  FIconAllowOverlap := AValue;
  Result := @Self;
end;

{**********************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetIconIgnorePlacement(const AValue: Boolean): PSymbolLayerOptions;
begin
  FIconIgnorePlacement := AValue;
  Result := @Self;
end;

{***************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetIconOptional(const AValue: Boolean): PSymbolLayerOptions;
begin
  FIconOptional := AValue;
  Result := @Self;
end;

{***********************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetIconRotationAlignment(const AValue: String): PSymbolLayerOptions;
begin
  FIconRotationAlignment := AValue;
  Result := @Self;
end;

{********************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetIconPitchAlignment(const AValue: String): PSymbolLayerOptions;
begin
  FIconPitchAlignment := AValue;
  Result := @Self;
end;

{*************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetIconPadding(const AValue: Single): PSymbolLayerOptions;
begin
  FIconPadding := AValue;
  Result := @Self;
end;

{******************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetIconKeepUpright(const AValue: Boolean): PSymbolLayerOptions;
begin
  FIconKeepUpright := AValue;
  Result := @Self;
end;

{*************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetIconTextFit(const AValue: String): PSymbolLayerOptions;
begin
  FIconTextFit := AValue;
  Result := @Self;
end;

{********************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetIconTextFitPadding(const AValue: TRectF): PSymbolLayerOptions;
begin
  FIconTextFitPadding := AValue;
  Result := @Self;
end;

{****************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetIconTranslate(const AValue: TPointF): PSymbolLayerOptions;
begin
  FIconTranslate := AValue;
  Result := @Self;
end;

{*********************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetIconTranslateAnchor(const AValue: String): PSymbolLayerOptions;
begin
  FIconTranslateAnchor := AValue;
  Result := @Self;
end;

{*******************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetTextAllowOverlap(const AValue: Boolean): PSymbolLayerOptions;
begin
  FTextAllowOverlap := AValue;
  Result := @Self;
end;

{**********************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetTextIgnorePlacement(const AValue: Boolean): PSymbolLayerOptions;
begin
  FTextIgnorePlacement := AValue;
  Result := @Self;
end;

{***************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetTextOptional(const AValue: Boolean): PSymbolLayerOptions;
begin
  FTextOptional := AValue;
  Result := @Self;
end;

{******************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetTextFont(const AValue: TArray<String>): PSymbolLayerOptions;
begin
  FTextFont := AValue;
  Result := @Self;
end;

{***********************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetTextRotationAlignment(const AValue: String): PSymbolLayerOptions;
begin
  FTextRotationAlignment := AValue;
  Result := @Self;
end;

{********************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetTextPitchAlignment(const AValue: String): PSymbolLayerOptions;
begin
  FTextPitchAlignment := AValue;
  Result := @Self;
end;

{*************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetTextPadding(const AValue: Single): PSymbolLayerOptions;
begin
  FTextPadding := AValue;
  Result := @Self;
end;

{******************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetTextKeepUpright(const AValue: Boolean): PSymbolLayerOptions;
begin
  FTextKeepUpright := AValue;
  Result := @Self;
end;

{**************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetTextMaxAngle(const AValue: Single): PSymbolLayerOptions;
begin
  FTextMaxAngle := AValue;
  Result := @Self;
end;

{****************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetTextLineHeight(const AValue: Single): PSymbolLayerOptions;
begin
  FTextLineHeight := AValue;
  Result := @Self;
end;

{*************************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetTextWritingMode(const AValue: TArray<String>): PSymbolLayerOptions;
begin
  FTextWritingMode := AValue;
  Result := @Self;
end;

{****************************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetTextVariableAnchor(const AValue: TArray<String>): PSymbolLayerOptions;
begin
  FTextVariableAnchor := AValue;
  Result := @Self;
end;

{****************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetTextTranslate(const AValue: TPointF): PSymbolLayerOptions;
begin
  FTextTranslate := AValue;
  Result := @Self;
end;

{*********************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetTextTranslateAnchor(const AValue: String): PSymbolLayerOptions;
begin
  FTextTranslateAnchor := AValue;
  Result := @Self;
end;

{************************************************************************************************}
function TALMapLibre.TSymbolLayerOptions.SetVisibility(const AValue: String): PSymbolLayerOptions;
begin
  FVisibility := AValue;
  Result := @Self;
end;

{***********************************************************************************************}
class operator TALMapLibre.TLineLayerOptions.Initialize(out Dest: TALMapLibre.TLineLayerOptions);
begin
  // Because of :
  // https://stackoverflow.com/questions/79318689/why-does-copyrecord-get-called-in-some-cases-with-tmyrec-but-not-others
end;

{*********************************************************************************}
class function TALMapLibre.TLineLayerOptions.Create: TALMapLibre.TLineLayerOptions;
begin
  Result.FLineCap := 'butt';
  Result.FLineJoin := 'miter';
  Result.FLineMiterLimit := 2;
  Result.FLineRoundLimit := 1.05;
  Result.FLineDasharray := nil;
  Result.FLineTranslate := TPointF.Zero;
  Result.FLineTranslateAnchor := 'map';
  Result.FVisibility := 'visible';
end;

{*****************************************************************************************}
function TALMapLibre.TLineLayerOptions.SetLineCap(const AValue: String): PLineLayerOptions;
begin
  FLineCap := AValue;
  Result := @Self;
end;

{******************************************************************************************}
function TALMapLibre.TLineLayerOptions.SetLineJoin(const AValue: String): PLineLayerOptions;
begin
  FLineJoin := AValue;
  Result := @Self;
end;

{************************************************************************************************}
function TALMapLibre.TLineLayerOptions.SetLineMiterLimit(const AValue: Single): PLineLayerOptions;
begin
  FLineMiterLimit := AValue;
  Result := @Self;
end;

{************************************************************************************************}
function TALMapLibre.TLineLayerOptions.SetLineRoundLimit(const AValue: Single): PLineLayerOptions;
begin
  FLineRoundLimit := AValue;
  Result := @Self;
end;

{*******************************************************************************************************}
function TALMapLibre.TLineLayerOptions.SetLineDasharray(const AValue: TArray<Single>): PLineLayerOptions;
begin
  FLineDasharray := AValue;
  Result := @Self;
end;

{************************************************************************************************}
function TALMapLibre.TLineLayerOptions.SetLineTranslate(const AValue: TPointF): PLineLayerOptions;
begin
  FLineTranslate := AValue;
  Result := @Self;
end;

{*****************************************************************************************************}
function TALMapLibre.TLineLayerOptions.SetLineTranslateAnchor(const AValue: String): PLineLayerOptions;
begin
  FLineTranslateAnchor := AValue;
  Result := @Self;
end;

{********************************************************************************************}
function TALMapLibre.TLineLayerOptions.SetVisibility(const AValue: String): PLineLayerOptions;
begin
  FVisibility := AValue;
  Result := @Self;
end;

{***************************************************************************************************}
class operator TALMapLibre.TCircleLayerOptions.Initialize(out Dest: TALMapLibre.TCircleLayerOptions);
begin
  // Because of :
  // https://stackoverflow.com/questions/79318689/why-does-copyrecord-get-called-in-some-cases-with-tmyrec-but-not-others
end;

{*************************************************************************************}
class function TALMapLibre.TCircleLayerOptions.Create: TALMapLibre.TCircleLayerOptions;
begin
  Result.FCirclePitchAlignment := 'viewport';
  Result.FCirclePitchScale := 'map';
  Result.FCircleTranslate := TPointF.Zero;
  Result.FCircleTranslateAnchor := 'map';
  Result.FVisibility := 'visible';
end;

{**********************************************************************************************************}
function TALMapLibre.TCircleLayerOptions.SetCirclePitchAlignment(const AValue: String): PCircleLayerOptions;
begin
  FCirclePitchAlignment := AValue;
  Result := @Self;
end;

{******************************************************************************************************}
function TALMapLibre.TCircleLayerOptions.SetCirclePitchScale(const AValue: String): PCircleLayerOptions;
begin
  FCirclePitchScale := AValue;
  Result := @Self;
end;

{******************************************************************************************************}
function TALMapLibre.TCircleLayerOptions.SetCircleTranslate(const AValue: TPointF): PCircleLayerOptions;
begin
  FCircleTranslate := AValue;
  Result := @Self;
end;

{***********************************************************************************************************}
function TALMapLibre.TCircleLayerOptions.SetCircleTranslateAnchor(const AValue: String): PCircleLayerOptions;
begin
  FCircleTranslateAnchor := AValue;
  Result := @Self;
end;

{************************************************************************************************}
function TALMapLibre.TCircleLayerOptions.SetVisibility(const AValue: String): PCircleLayerOptions;
begin
  FVisibility := AValue;
  Result := @Self;
end;

{***********************************************************************************************}
class operator TALMapLibre.TFillLayerOptions.Initialize(out Dest: TALMapLibre.TFillLayerOptions);
begin
  // Because of :
  // https://stackoverflow.com/questions/79318689/why-does-copyrecord-get-called-in-some-cases-with-tmyrec-but-not-others
end;

{*********************************************************************************}
class function TALMapLibre.TFillLayerOptions.Create: TALMapLibre.TFillLayerOptions;
begin
  Result.FFillAntialias := True;
  Result.FFillTranslate := TPointF.Zero;
  Result.FFillTranslateAnchor := 'map';
  Result.FFillPattern := '';
  Result.FVisibility := 'visible';
end;

{************************************************************************************************}
function TALMapLibre.TFillLayerOptions.SetFillAntialias(const AValue: Boolean): PFillLayerOptions;
begin
  FFillAntialias := AValue;
  Result := @Self;
end;

{************************************************************************************************}
function TALMapLibre.TFillLayerOptions.SetFillTranslate(const AValue: TPointF): PFillLayerOptions;
begin
  FFillTranslate := AValue;
  Result := @Self;
end;

{*****************************************************************************************************}
function TALMapLibre.TFillLayerOptions.SetFillTranslateAnchor(const AValue: String): PFillLayerOptions;
begin
  FFillTranslateAnchor := AValue;
  Result := @Self;
end;

{*********************************************************************************************}
function TALMapLibre.TFillLayerOptions.SetFillPattern(const AValue: String): PFillLayerOptions;
begin
  FFillPattern := AValue;
  Result := @Self;
end;

{********************************************************************************************}
function TALMapLibre.TFillLayerOptions.SetVisibility(const AValue: String): PFillLayerOptions;
begin
  FVisibility := AValue;
  Result := @Self;
end;

{*****************************************************************************************************************}
class operator TALMapLibre.TFillExtrusionLayerOptions.Initialize(out Dest: TALMapLibre.TFillExtrusionLayerOptions);
begin
  // Because of :
  // https://stackoverflow.com/questions/79318689/why-does-copyrecord-get-called-in-some-cases-with-tmyrec-but-not-others
end;

{***************************************************************************************************}
class function TALMapLibre.TFillExtrusionLayerOptions.Create: TALMapLibre.TFillExtrusionLayerOptions;
begin
  Result.FFillExtrusionOpacity := 1;
  Result.FFillExtrusionVerticalGradient := True;
  Result.FFillExtrusionTranslate := TPointF.Zero;
  Result.FFillExtrusionTranslateAnchor := 'map';
  Result.FVisibility := 'visible';
end;

{************************************************************************************************************************}
function TALMapLibre.TFillExtrusionLayerOptions.SetFillExtrusionOpacity(const AValue: Single): PFillExtrusionLayerOptions;
begin
  FFillExtrusionOpacity := AValue;
  Result := @Self;
end;

{**********************************************************************************************************************************}
function TALMapLibre.TFillExtrusionLayerOptions.SetFillExtrusionVerticalGradient(const AValue: Boolean): PFillExtrusionLayerOptions;
begin
  FFillExtrusionVerticalGradient := AValue;
  Result := @Self;
end;

{***************************************************************************************************************************}
function TALMapLibre.TFillExtrusionLayerOptions.SetFillExtrusionTranslate(const AValue: TPointF): PFillExtrusionLayerOptions;
begin
  FFillExtrusionTranslate := AValue;
  Result := @Self;
end;

{********************************************************************************************************************************}
function TALMapLibre.TFillExtrusionLayerOptions.SetFillExtrusionTranslateAnchor(const AValue: String): PFillExtrusionLayerOptions;
begin
  FFillExtrusionTranslateAnchor := AValue;
  Result := @Self;
end;

{**************************************************************************************************************}
function TALMapLibre.TFillExtrusionLayerOptions.SetVisibility(const AValue: String): PFillExtrusionLayerOptions;
begin
  FVisibility := AValue;
  Result := @Self;
end;

{*****************************************************************************************************}
class operator TALMapLibre.THeatmapLayerOptions.Initialize(out Dest: TALMapLibre.THeatmapLayerOptions);
begin
  // Because of :
  // https://stackoverflow.com/questions/79318689/why-does-copyrecord-get-called-in-some-cases-with-tmyrec-but-not-others
end;

{***************************************************************************************}
class function TALMapLibre.THeatmapLayerOptions.Create: TALMapLibre.THeatmapLayerOptions;
begin
  Result.FHeatmapRadius := 30;
  Result.FHeatmapIntensity := 1;
  Result.FHeatmapOpacity := 1;
  Result.FVisibility := 'visible';
end;

{*****************************************************************************************************}
function TALMapLibre.THeatmapLayerOptions.SetHeatmapRadius(const AValue: Single): PHeatmapLayerOptions;
begin
  FHeatmapRadius := AValue;
  Result := @Self;
end;

{********************************************************************************************************}
function TALMapLibre.THeatmapLayerOptions.SetHeatmapIntensity(const AValue: Single): PHeatmapLayerOptions;
begin
  FHeatmapIntensity := AValue;
  Result := @Self;
end;

{******************************************************************************************************}
function TALMapLibre.THeatmapLayerOptions.SetHeatmapOpacity(const AValue: Single): PHeatmapLayerOptions;
begin
  FHeatmapOpacity := AValue;
  Result := @Self;
end;

{**************************************************************************************************}
function TALMapLibre.THeatmapLayerOptions.SetVisibility(const AValue: String): PHeatmapLayerOptions;
begin
  FVisibility := AValue;
  Result := @Self;
end;

{***************************************************************************************************}
class operator TALMapLibre.TRasterLayerOptions.Initialize(out Dest: TALMapLibre.TRasterLayerOptions);
begin
  // Because of :
  // https://stackoverflow.com/questions/79318689/why-does-copyrecord-get-called-in-some-cases-with-tmyrec-but-not-others
end;

{*************************************************************************************}
class function TALMapLibre.TRasterLayerOptions.Create: TALMapLibre.TRasterLayerOptions;
begin
  Result.FRasterOpacity := 1;
  Result.FRasterHueRotate := 0;
  Result.FRasterBrightnessMin := 0;
  Result.FRasterBrightnessMax := 1;
  Result.FRasterSaturation := 0;
  Result.FRasterContrast := 0;
  Result.FRasterResampling := 'linear';
  Result.FRasterFadeDuration := 300;
  Result.FVisibility := 'visible';
end;

{***************************************************************************************************}
function TALMapLibre.TRasterLayerOptions.SetRasterOpacity(const AValue: Single): PRasterLayerOptions;
begin
  FRasterOpacity := AValue;
  Result := @Self;
end;

{*****************************************************************************************************}
function TALMapLibre.TRasterLayerOptions.SetRasterHueRotate(const AValue: Single): PRasterLayerOptions;
begin
  FRasterHueRotate := AValue;
  Result := @Self;
end;

{*********************************************************************************************************}
function TALMapLibre.TRasterLayerOptions.SetRasterBrightnessMin(const AValue: Single): PRasterLayerOptions;
begin
  FRasterBrightnessMin := AValue;
  Result := @Self;
end;

{*********************************************************************************************************}
function TALMapLibre.TRasterLayerOptions.SetRasterBrightnessMax(const AValue: Single): PRasterLayerOptions;
begin
  FRasterBrightnessMax := AValue;
  Result := @Self;
end;

{******************************************************************************************************}
function TALMapLibre.TRasterLayerOptions.SetRasterSaturation(const AValue: Single): PRasterLayerOptions;
begin
  FRasterSaturation := AValue;
  Result := @Self;
end;

{****************************************************************************************************}
function TALMapLibre.TRasterLayerOptions.SetRasterContrast(const AValue: Single): PRasterLayerOptions;
begin
  FRasterContrast := AValue;
  Result := @Self;
end;

{******************************************************************************************************}
function TALMapLibre.TRasterLayerOptions.SetRasterResampling(const AValue: String): PRasterLayerOptions;
begin
  FRasterResampling := AValue;
  Result := @Self;
end;

{********************************************************************************************************}
function TALMapLibre.TRasterLayerOptions.SetRasterFadeDuration(const AValue: Single): PRasterLayerOptions;
begin
  FRasterFadeDuration := AValue;
  Result := @Self;
end;

{************************************************************************************************}
function TALMapLibre.TRasterLayerOptions.SetVisibility(const AValue: String): PRasterLayerOptions;
begin
  FVisibility := AValue;
  Result := @Self;
end;

{*************************************************************}
function TALMapLibre.TCompass.TMargins.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(8{Left}, 8{Top}, 8{Right}, 8{Bottom});
end;

{**************************************}
constructor TALMapLibre.TCompass.Create;
begin
  inherited Create;
  FEnabled := DefaultEnabled;
  FFadeFacingNorth := DefaultFadeFacingNorth;
  FGravity := DefaultGravity;
  FResourceName := DefaultResourceName;
  FWidth := DefaultWidth;
  FHeight := DefaultHeight;
  FMargins := CreateMargins;
  FMargins.OnChanged := MarginsChanged;
end;

{**************************************}
destructor TALMapLibre.TCompass.Destroy;
begin
  ALFreeAndNil(FMargins);
  inherited;
end;

{*****************************************************}
function TALMapLibre.TCompass.CreateMargins: TALBounds;
begin
  Result := TMargins.Create;
end;

{*********************************************************}
procedure TALMapLibre.TCompass.Assign(Source: TPersistent);
begin
  if Source is TCompass then begin
    BeginUpdate;
    Try
      Enabled := TCompass(Source).Enabled;
      FadeFacingNorth := TCompass(Source).FadeFacingNorth;
      Gravity := TCompass(Source).Gravity;
      ResourceName := TCompass(Source).ResourceName;
      Width := TCompass(Source).Width;
      Height := TCompass(Source).Height;
      Margins.Assign(TCompass(Source).Margins);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{***********************************}
procedure TALMapLibre.TCompass.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Enabled := DefaultEnabled;
    FadeFacingNorth := DefaultFadeFacingNorth;
    Gravity := DefaultGravity;
    ResourceName := DefaultResourceName;
    Width := DefaultWidth;
    Height := DefaultHeight;
    Margins.Rect := Margins.DefaultValue;
  finally
    EndUpdate;
  end;
end;

{*****************************************************}
function TALMapLibre.TCompass.IsEnabledStored: Boolean;
begin
  Result := FEnabled <> DefaultEnabled;
end;

{*************************************************************}
function TALMapLibre.TCompass.IsFadeFacingNorthStored: Boolean;
begin
  Result := FFadeFacingNorth <> DefaultFadeFacingNorth;
end;

{*****************************************************}
function TALMapLibre.TCompass.IsGravityStored: Boolean;
begin
  Result := FGravity <> DefaultGravity;
end;

{**********************************************************}
function TALMapLibre.TCompass.IsResourceNameStored: Boolean;
begin
  Result := FResourceName <> DefaultResourceName;
end;

{***************************************************}
function TALMapLibre.TCompass.IsWidthStored: Boolean;
begin
  Result := not SameValue(FWidth, DefaultWidth, TEpsilon.Position);
end;

{****************************************************}
function TALMapLibre.TCompass.IsHeightStored: Boolean;
begin
  Result := not SameValue(FHeight, DefaultHeight, TEpsilon.Position);
end;

{*******************************************************}
function TALMapLibre.TCompass.GetDefaultEnabled: Boolean;
begin
  Result := True;
end;

{***************************************************************}
function TALMapLibre.TCompass.GetDefaultFadeFacingNorth: Boolean;
begin
  Result := True;
end;

{****************************************************************}
function TALMapLibre.TCompass.GetDefaultGravity: TOrnamentGravity;
begin
  Result := TOrnamentGravity.TopRight;
end;

{***********************************************************}
function TALMapLibre.TCompass.GetDefaultResourceName: String;
begin
  Result := '';
end;

{****************************************************}
function TALMapLibre.TCompass.GetDefaultWidth: Single;
begin
  Result := 40;
end;

{*****************************************************}
function TALMapLibre.TCompass.GetDefaultHeight: Single;
begin
  Result := 40;
end;

{***************************************************************}
procedure TALMapLibre.TCompass.SetEnabled(const AValue: Boolean);
begin
  if FEnabled <> AValue then begin
    FEnabled := AValue;
    Change;
  end;
end;

{***********************************************************************}
procedure TALMapLibre.TCompass.SetFadeFacingNorth(const AValue: Boolean);
begin
  if FFadeFacingNorth <> AValue then begin
    FFadeFacingNorth := AValue;
    Change;
  end;
end;

{************************************************************************}
procedure TALMapLibre.TCompass.SetGravity(const AValue: TOrnamentGravity);
begin
  if FGravity <> AValue then begin
    FGravity := AValue;
    Change;
  end;
end;

{*******************************************************************}
procedure TALMapLibre.TCompass.SetResourceName(const AValue: String);
begin
  if FResourceName <> AValue then begin
    FResourceName := AValue;
    Change;
  end;
end;

{************************************************************}
procedure TALMapLibre.TCompass.SetWidth(const AValue: Single);
begin
  if not SameValue(FWidth, AValue, TEpsilon.Position) then begin
    FWidth := AValue;
    Change;
  end;
end;

{*************************************************************}
procedure TALMapLibre.TCompass.SetHeight(const AValue: Single);
begin
  if not SameValue(FHeight, AValue, TEpsilon.Position) then begin
    FHeight := AValue;
    Change;
  end;
end;

{*****************************************************************}
procedure TALMapLibre.TCompass.SetMargins(const AValue: TALBounds);
begin
  FMargins.Assign(AValue);
end;

{*************************************************************}
procedure TALMapLibre.TCompass.MarginsChanged(Sender: TObject);
begin
  Change;
end;

{*************************************************}
constructor TALMapLibre.Create(AOwner: TComponent);
begin
  FCompass := TCompass.Create;
  FCompass.OnChanged := CompassChanged;
  FOnMapReady := nil;
  FOnStyleLoaded := nil;
  FOnCameraWillChange := nil;
  FOnCameraIsChanging := nil;
  FOnCameraDidChange := nil;
  FOnSymbolClick := nil;
  FOnMapClick := nil;
  FStyleJSON := '';
  FStyleUrl := '';
  inherited; // This can call CreateNativeView
end;

{*****************************}
destructor TALMapLibre.Destroy;
begin
  inherited;
  ALFreeAndNil(FCompass);
end;

{********************}
{$IF defined(android)}
Function TALMapLibre.CreateNativeView: TALAndroidNativeView;
begin
  result := TALAndroidMapLibreView.create(self);
end;
{$ENDIF}

{********************}
{$IF defined(android)}
function TALMapLibre.GetNativeView: TALAndroidMapLibreView;
begin
  result := TALAndroidMapLibreView(inherited NativeView);
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
Function TALMapLibre.CreateNativeView: TALIosNativeView;
begin
  result := TALIosMapLibreView.create(self);
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function TALMapLibre.GetNativeView: TALIosMapLibreView;
begin
  result := TALIosMapLibreView(inherited NativeView);
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
Function TALMapLibre.CreateNativeView: TALMacNativeView;
begin
  result := TALMacMapLibreView.create(self);
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
function TALMapLibre.GetNativeView: TALMacMapLibreView;
begin
  result := TALMacMapLibreView(inherited NativeView);
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
Function TALMapLibre.CreateNativeView: TALWinNativeView;
begin
  {$IF defined(ALDPK)}
  Result := nil;
  {$ELSE}
  result := TALWinMapLibreView.create(self);
  {$ENDIF}
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
function TALMapLibre.GetNativeView: TALWinMapLibreView;
begin
  result := TALWinMapLibreView(inherited NativeView);
end;
{$ENDIF}

{***************************}
procedure TALMapLibre.Loaded;
begin
  inherited;
  if NativeView <> nil then begin
    NativeView.ApplyStyle;
    NativeView.ApplyCompass;
  end;
end;

{******************************************************}
procedure TALMapLibre.SetStyleURL(const AValue: string);
begin
  if (FStyleURL = AValue) and (FStyleJSON = '') then Exit;
  FStyleURL := AValue;
  FStyleJSON := '';
  if HasNativeView then
    NativeView.ApplyStyle;
end;

{*******************************************************}
procedure TALMapLibre.SetStyleJSON(const AValue: string);
begin
  if (FStyleJSON = AValue) and (FStyleURL = '') then Exit;
  FStyleJSON := AValue;
  FStyleURL := '';
  if HasNativeView then
    NativeView.ApplyStyle;
end;

{*******************************************************}
procedure TALMapLibre.SetCompass(const AValue: TCompass);
begin
  FCompass.Assign(AValue);
end;

{****************************************************}
procedure TALMapLibre.CompassChanged(Sender: TObject);
begin
  if HasNativeView then
    NativeView.ApplyCompass;
end;

{*******************************************************************************************************}
procedure TALMapLibre.SetCenterCoordinate(const ALatitude, ALongitude: Double; const AZoomLevel: double);
begin
  if NativeView <> nil then
    NativeView.SetCenterCoordinate(ALatitude, ALongitude, AZoomLevel);
end;

{*****************************}
procedure TALMapLibre.SetImage(
            const AStream: TStream;
            const AScale: Single;
            const W, H: single;
            const AName: String);
begin
  if NativeView <> nil then
    NativeView.SetImage(''{AResourceName}, AStream{AStream}, AScale, W, H, AName);
end;

{*****************************}
procedure TALMapLibre.SetImage(
            const AResourceName: String;
            const AScale: Single;
            const W, H: single;
            const AName: String);
begin
  if NativeView <> nil then
    NativeView.SetImage(AResourceName{AResourceName}, nil{AStream}, AScale, W, H, AName);
end;

{*****************************************************}
procedure TALMapLibre.RemoveImage(const AName: String);
begin
  if NativeView <> nil then
    NativeView.RemoveImage(AName);
end;

{*************************************************************************************************************************************}
procedure TALMapLibre.SetSymbolLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PSymbolLayerOptions = nil);
begin
  if NativeView <> nil then
    NativeView.SetSymbolLayer(AId, AGeoJSON, AOptions);
end;

{*********************************************************************************************************************************}
procedure TALMapLibre.SetLineLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PLineLayerOptions = nil);
begin
  if NativeView <> nil then
    NativeView.SetLineLayer(AId, AGeoJSON, AOptions);
end;

{*************************************************************************************************************************************}
procedure TALMapLibre.SetCircleLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PCircleLayerOptions = nil);
begin
  if NativeView <> nil then
    NativeView.SetCircleLayer(AId, AGeoJSON, AOptions);
end;

{*********************************************************************************************************************************}
procedure TALMapLibre.SetFillLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillLayerOptions = nil);
begin
  if NativeView <> nil then
    NativeView.SetFillLayer(AId, AGeoJSON, AOptions);
end;

{***************************************************************************************************************************************************}
procedure TALMapLibre.SetFillExtrusionLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillExtrusionLayerOptions = nil);
begin
  if NativeView <> nil then
    NativeView.SetFillExtrusionLayer(AId, AGeoJSON, AOptions);
end;

{***************************************************************************************************************************************}
procedure TALMapLibre.SetHeatmapLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PHeatmapLayerOptions = nil);
begin
  if NativeView <> nil then
    NativeView.SetHeatmapLayer(AId, AGeoJSON, AOptions);
end;

{*********************************************************************************************************************************************************************************************************}
procedure TALMapLibre.SetRasterLayer(const AId: String; const ATileURL: String; const AIsTileJSON: Boolean = False; const ATileSize: Integer = 256; const AOptions: TALMapLibre.PRasterLayerOptions = nil);
begin
  if NativeView <> nil then
    NativeView.SetRasterLayer(AId, ATileURL, AIsTileJSON, ATileSize, AOptions);
end;

{***************************************************}
procedure TALMapLibre.RemoveLayer(const AId: String);
begin
  if NativeView <> nil then
    NativeView.RemoveLayer(AId);
end;

{***********************************************}
function TALMapLibre.GetViewportBounds: TALRectD;
begin
  if NativeView <> nil then
    result := NativeView.GetViewportBounds
  else
    Result := TAlRectD.Empty;
end;

{******************************************************}
function TALMapLibre.GetCameraPosition: TCameraPosition;
begin
  if NativeView <> nil then
    result := NativeView.GetCameraPosition
  else begin
    Result := TCameraPosition.Create(
                0, // const ABearing: Double;
                TALRectD.Empty, // const APadding: TALRectD;
                0, // const ARoll: Double;
                TALLatLng.Create(ALNullLatLng, ALNullLatLng), // const ATarget: TALLatLng;
                0, // const ATilt: Double;
                0); // const AZoom: Double);;
  end;
end;

{********************}
{$IF defined(android)}
procedure TALMapLibre.ShowLoadingSpinner(const AView: JView);
begin
  if NativeView <> nil then
    NativeView.ShowLoadingSpinner(AView);
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
procedure TALMapLibre.ShowLoadingSpinner(const AView: UIView);
begin
  if NativeView <> nil then
    NativeView.ShowLoadingSpinner(AView);
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
procedure TALMapLibre.ShowLoadingSpinner(const AHTML: String);
begin
  if NativeView <> nil then
    NativeView.ShowLoadingSpinner(AHTML);
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
procedure TALMapLibre.ShowLoadingSpinner(const AHTML: String);
begin
  if NativeView <> nil then
    NativeView.ShowLoadingSpinner(AHTML);
end;
{$ENDIF}

{*******************************************************************************************************************************************}
procedure TALMapLibre.ShowLoadingSpinner(const AColor: TAlphaColor = Talphacolors.null; const AWidth: Single = 0; const AHeight: Single = 0);
begin
  if NativeView <> nil then
    NativeView.ShowLoadingSpinner(AColor, AWidth, AHeight);
end;

{***************************************}
procedure TALMapLibre.HideLoadingSpinner;
begin
  if NativeView <> nil then
    NativeView.HideLoadingSpinner;
end;

{$REGION ' ANDROID'}
{$IF defined(android)}

{*********************************************************************************************************}
constructor TALAndroidMapLibreView.TOnMapReadyCallback.Create(const AMapLibreView: TALAndroidMapLibreView);
begin
  inherited Create;
  FMapLibreView := AMapLibreView;
end;

{*****************************************************************************************}
procedure TALAndroidMapLibreView.TOnMapReadyCallback.onMapReady(maplibreMap: JMapLibreMap);
begin

  {$IF defined(DEBUG)}
  ALLog('TALAndroidMapLibreView.TOnMapReadyCallback.onMapReady');
  {$ENDIF}

  Try

    if FMapLibreView <> nil then begin
      FMapLibreView.FOnMapReadyCallback := nil;
      FMapLibreView.FMaplibreMap := maplibreMap;
      FMapLibreView.FMaplibreMap.addOnCameraMoveStartedListener(FMapLibreView.FOnCameraMoveStartedListener);
      FMapLibreView.FMaplibreMap.addOnCameraMoveListener(FMapLibreView.FOnCameraMoveListener);
      FMapLibreView.FMaplibreMap.addOnCameraIdleListener(FMapLibreView.FOnCameraIdleListener);
      FMapLibreView.FMaplibreMap.addOnMapClickListener(FMapLibreView.FOnMapClickListener);
      FMapLibreView.ApplyStyle;
      FMapLibreView.ApplyCompass;
      if Assigned(FMapLibreView.Control.FOnMapReady) then
        FMapLibreView.Control.FOnMapReady(FMapLibreView.Control);
    end;

  finally
    var LOnMapReadyCallback := Self;
    TThread.ForceQueue(nil,
      procedure
      begin
        ALFreeAndNil(LOnMapReadyCallback);
      end);
  end;

end;

{**********************************************************************************************************************}
constructor TALAndroidMapLibreView.TOnDidFinishLoadingStyleListener.Create(const AMapLibreView: TALAndroidMapLibreView);
begin
  inherited Create;
  FMapLibreView := AMapLibreView;
end;

{****************************************************************************************}
procedure TALAndroidMapLibreView.TOnDidFinishLoadingStyleListener.onDidFinishLoadingStyle;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidMapLibreView.TOnDidFinishLoadingStyleListener.onDidFinishLoadingStyle');
  {$ENDIF}
  FMapLibreView.FSymbolLayerIds.Clear;
  if Assigned(FMapLibreView.Control.FOnStyleLoaded) then
    FMapLibreView.Control.FOnStyleLoaded(FMapLibreView.Control);
end;

{******************************************************************************************************************}
constructor TALAndroidMapLibreView.TOnCameraMoveStartedListener.Create(const AMapLibreView: TALAndroidMapLibreView);
begin
  inherited Create;
  FMapLibreView := AMapLibreView;
end;

{********************************************************************************************************}
procedure TALAndroidMapLibreView.TOnCameraMoveStartedListener.onCameraMoveStarted(reason: Integer); cdecl;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidMapLibreView.TOnCameraMoveStartedListener.onCameraMoveStarted');
  {$ENDIF}
  if Assigned(FMapLibreView.Control.FOnCameraWillChange) then
    FMapLibreView.Control.FOnCameraWillChange(FMapLibreView.Control);
end;

{***********************************************************************************************************}
constructor TALAndroidMapLibreView.TOnCameraMoveListener.Create(const AMapLibreView: TALAndroidMapLibreView);
begin
  inherited Create;
  FMapLibreView := AMapLibreView;
end;

{******************************************************************}
procedure TALAndroidMapLibreView.TOnCameraMoveListener.onCameraMove;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidMapLibreView.TOnCameraMoveListener.onCameraMove');
  {$ENDIF}
  if Assigned(FMapLibreView.Control.FOnCameraIsChanging) then
    FMapLibreView.Control.FOnCameraIsChanging(FMapLibreView.Control);
end;

{***********************************************************************************************************}
constructor TALAndroidMapLibreView.TOnCameraIdleListener.Create(const AMapLibreView: TALAndroidMapLibreView);
begin
  inherited Create;
  FMapLibreView := AMapLibreView;
end;

{******************************************************************}
procedure TALAndroidMapLibreView.TOnCameraIdleListener.onCameraIdle;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidMapLibreView.TOnCameraIdleListener.onCameraIdle');
  {$ENDIF}
  if Assigned(FMapLibreView.Control.FOnCameraDidChange) then
    FMapLibreView.Control.FOnCameraDidChange(FMapLibreView.Control);
end;

{*********************************************************************************************************}
constructor TALAndroidMapLibreView.TOnMapClickListener.Create(const AMapLibreView: TALAndroidMapLibreView);
begin
  inherited Create;
  FMapLibreView := AMapLibreView;
end;

{**************************************************************************************}
function TALAndroidMapLibreView.TOnMapClickListener.onMapClick(point: JLatLng): Boolean;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidMapLibreView.TOnMapClickListener.onMapClick');
  {$ENDIF}

  // Return: True if this click should be consumed and not passed further to
  // other listeners registered afterwards, false otherwise.
  Result := True;

  if Assigned(FMapLibreView.Control.FOnMapClick) then
    FMapLibreView.Control.FOnMapClick(
      FMapLibreView.Control,
      point.getLatitude,
      point.getLongitude);

  if not Assigned(FMapLibreView.Control.FOnSymbolClick) then Exit;
  if FMapLibreView.FMaplibreMap = nil then Exit;
  var LStyle := FMapLibreView.FMaplibreMap.getStyle;
  if LStyle = nil then Exit;

  var LPoint := FMapLibreView.FMaplibreMap.getProjection.toScreenLocation(point);
  var LLayers := TJavaObjectArray<JString>.Create(1);
  Try

    for var LIndex := FMapLibreView.FSymbolLayerIds.Count - 1 downto 0 do begin
      var LLayerIDStr := FMapLibreView.FSymbolLayerIds[LIndex];
      var LLayerIDJStr := StringToJString(LLayerIDStr);
      if LStyle.getLayer(LLayerIDJStr) = nil then Continue;

      LLayers.Items[0] := LLayerIDJStr;
      var LFeatures := FMapLibreView.FMaplibreMap.queryRenderedFeatures(LPoint, LLayers);
      if (LFeatures = nil) or (LFeatures.size = 0) then Continue;

      for var I := 0 to LFeatures.size - 1 do begin
        var LFeature := TJFeature.Wrap(TAndroidHelper.JObjectToID(LFeatures.get(I)));
        if (LFeature = nil) or (not LFeature.hasProperty(StringToJString('id'))) then Continue;
        var LSymbolID := LFeature.getStringProperty(StringToJString('id'));
        if LSymbolID = nil then Continue;

        FMapLibreView.Control.FOnSymbolClick(
          FMapLibreView.Control, // Sender: TObject;
          LLayerIDStr, // const ALayerID: String;
          JStringToString(LSymbolID)); // const ASymbolID: String

        Exit;
      end;
    end;

  finally
    AlFreeAndNil(LLayers);
  end;
end;

{****************************************}
constructor TALAndroidMapLibreView.Create;
begin
  inherited; // This will call CreateView

  FMaplibreMap := nil;
  FLoadingSpinnerView := nil;

  FSymbolLayerIds := TALStringListW.Create;

  FOnDidFinishLoadingStyleListener := TOnDidFinishLoadingStyleListener.Create(Self);
  View.addOnDidFinishLoadingStyleListener(FOnDidFinishLoadingStyleListener);

  FOnCameraMoveStartedListener := TOnCameraMoveStartedListener.Create(Self);
  FOnCameraMoveListener := TOnCameraMoveListener.Create(Self);
  FOnCameraIdleListener := TOnCameraIdleListener.Create(Self);
  FOnMapClickListener := TOnMapClickListener.Create(Self);

  FOnMapReadyCallback := TOnMapReadyCallback.Create(Self);
  view.getMapAsync(FOnMapReadyCallback);

  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventHandler);
end;

{****************************************}
destructor TALAndroidMapLibreView.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventHandler);

  HideLoadingSpinner;

  if FMaplibreMap <> nil then begin
    FMaplibreMap.removeOnCameraMoveStartedListener(FOnCameraMoveStartedListener);
    FMaplibreMap.removeOnCameraMoveListener(FOnCameraMoveListener);
    FMaplibreMap.removeOnCameraIdleListener(FOnCameraIdleListener);
    FMaplibreMap.removeOnMapClickListener(FOnMapClickListener);
    FMaplibreMap := nil;
  end;

  if FOnMapReadyCallback <> nil then
    FOnMapReadyCallback.FMapLibreView := nil;

  View.removeOnDidFinishLoadingStyleListener(FOnDidFinishLoadingStyleListener);
  ALFreeAndNil(FOnDidFinishLoadingStyleListener);
  ALFreeAndNil(FOnCameraMoveStartedListener);
  ALFreeAndNil(FOnCameraMoveListener);
  ALFreeAndNil(FOnCameraIdleListener);
  ALFreeAndNil(FOnMapClickListener);

  View.onPause();
  View.onStop();
  View.onDestroy();

  AlFreeAndNil(FSymbolLayerIds);

  inherited;
end;

{************************************************}
function TALAndroidMapLibreView.CreateView: JView;
begin

  {$IF defined(DEBUG)}
  ALLog('TALAndroidMapLibreView.CreateView', 'Visible: '+ ALBoolToStrW(Visible, 'True', 'False'));
  {$ENDIF}

  // Initialize MapLibre SDK
  TJMapLibre.JavaClass.getInstance(TAndroidHelper.Context);

  // Create MapView
  var LOptions: JMapLibreMapOptions;
  LOptions := TJMapLibreMapOptions.JavaClass.init;
  // Texture mode is required for FreezeNativeView to work correctly.
  LOptions.textureMode(True);
  var LMapView := TJMapView.JavaClass.init(TAndroidHelper.Context, LOptions);
  LMapView.onCreate(nil);
  if Visible then begin
    if ALApplicationState in [TApplicationEvent.WillBecomeInactive,
                              TApplicationEvent.WillBecomeForeground,
                              TApplicationEvent.BecameActive] then LMapView.onStart;
    // Currently onResume does nothing, but retained for compatibility
    // with potential future API changes.
    if ALApplicationState = TApplicationEvent.BecameActive then LMapView.onResume;
  end;
  Result := LMapView;

end;

{************************************************}
function TALAndroidMapLibreView.GetView: JMapView;
begin
  Result := inherited GetView<JMapView>;
end;

{******************************************************}
function TALAndroidMapLibreView.GetControl: TALMapLibre;
begin
  Result := TALMapLibre(inherited Control)
end;

{*************************************************************************************************}
procedure TALAndroidMapLibreView.ApplicationEventHandler(const Sender: TObject; const M: TMessage);
begin
  if M is TApplicationEventMessage then begin
    var LValue := (M as TApplicationEventMessage).value;
    {$IF defined(DEBUG)}
    ALLog('TALAndroidMapLibreView.ApplicationEventHandler', 'Event: '+ TRttiEnumerationType.GetName(LValue.Event));
    {$ENDIF}
    case LValue.Event of
      //TApplicationEvent.FinishedLaunching: if Visible then View.onStart;
      TApplicationEvent.BecameActive: if Visible then View.onResume;
      TApplicationEvent.WillBecomeInactive: if Visible then View.onPause;
      TApplicationEvent.EnteredBackground: if Visible then View.onStop;
      TApplicationEvent.WillBecomeForeground: if Visible then View.onStart;
      //TApplicationEvent.WillTerminate: View.onDestroy;
      TApplicationEvent.LowMemory: View.onLowMemory;
      //TApplicationEvent.TimeChange:;
      //TApplicationEvent.OpenURL:;
    end;
  end;
end;

{************************************************************}
procedure TALAndroidMapLibreView.Draw(const ACanvas: JCanvas);
begin

  var LRenderView := TJMapView.Wrap(View).getRenderView;
  if (LRenderView = nil) or
     (not TJNIResolver.IsInstanceOf(LRenderView, TJTextureView.GetClsID)) then begin
    inherited;
    exit;
  end;

  var LBitmap := TJTextureView.Wrap(LRenderView).getBitmap;
  if LBitmap <> nil then begin
    try

      // Draw the MapLibre TextureView
      ACanvas.drawBitmap(
        LBitmap, // bitmap: JBitmap;
        View.getX + LRenderView.getX, // left: Single;
        View.getY + LRenderView.getY, // top: Single;
        nil); // paint: JPaint

      // Draw overlays inside MapView
      var LMapView := TJViewGroup.Wrap(View);
      for var I := 0 to LMapView.getChildCount - 1 do begin
        var LChild := LMapView.getChildAt(I);
        if LChild.equals(LRenderView) then continue;
        if LChild.getVisibility <> TJView.JavaClass.VISIBLE then continue;
        ACanvas.save;
        try
          ACanvas.translate(
            View.getX + LChild.getX,
            View.getY + LChild.getY);
          LChild.draw(ACanvas);
        finally
          ACanvas.restore;
        end;
      end;

    finally
      LBitmap.recycle;
    end;
  end;

  // Draw native child controls
  DrawNativeChildViews(ACanvas);

end;

{****************************************************************}
procedure TALAndroidMapLibreView.SetVisible(const Value: Boolean);
begin
  if Visible <> Value then begin
    {$IF defined(DEBUG)}
    ALLog('TALAndroidMapLibreView.SetVisible', 'Value: '+ ALBoolToStrW(Value, 'True', 'False'));
    {$ENDIF}
    if not Value then begin
      // Currently onPause does nothing, but retained for compatibility
      // with potential future API changes.
      View.onPause;
      View.onStop;
    end
    else begin
      if ALApplicationState in [TApplicationEvent.WillBecomeInactive,
                                TApplicationEvent.WillBecomeForeground,
                                TApplicationEvent.BecameActive] then View.onStart;
      // Currently onResume does nothing, but retained for compatibility
      // with potential future API changes.
      if ALApplicationState = TApplicationEvent.BecameActive then View.onResume;
    end;
  end;
  inherited;
end;

{******************************************************************************************************************}
procedure TALAndroidMapLibreView.SetCenterCoordinate(const ALatitude, ALongitude: Double; const AZoomLevel: double);
begin
  If FMaplibreMap = nil then raise Exception.Create('MapLibre is not initialized');
  FMaplibreMap.moveCamera(
    TJCameraUpdateFactory.JavaClass.newLatLngZoom(
      TJLatLng.JavaClass.init(ALatitude, ALongitude), // latLng: JLatLng;
      AZoomLevel)); // zoom: Double
end;

{****************************************}
procedure TALAndroidMapLibreView.SetImage(
            const AResourceName: String;
            const AStream: TStream;
            const AScale: Single;
            const W, H: single;
            const AName: String);
begin
  If FMaplibreMap = nil then raise Exception.Create('MapLibre is not initialized');
  var LStyle := FMaplibreMap.getStyle;
  if LStyle = nil then raise Exception.Create('MapLibre style is not loaded');
  var LBitmap := ALCreateJBitmapFromResource(
                   AResourceName, // const AResourceName: String;
                   AStream, // const AResourceStream: TStream;
                   '', // const AMaskResourceName: String;
                   AScale, // const AScale: Single;
                   TRectF.Empty, // const ASrcRect: TRectF; // In source image coordinates (real pixels). Use TRectF.Empty to use the full image
                   W, H, // const W, H: single;
                   False, // const AApplyMetadataOrientation: Boolean;
                   TALImageWrapMode.fit, // const AWrapMode: TALImageWrapMode;
                   TpointF.Create(0.5,0.5), // const ACropCenter: TpointF;
                   TalphaColors.null, // const ATintColor: TalphaColor;
                   0, // const ABlurRadius: single;
                   0, // const AXRadius: Single;
                   0); // const AYRadius: Single): CGImageRef;
  try
    LStyle.addImage(StringToJString(AName), LBitmap);
  finally
    LBitmap.recycle;
  end;
end;

{****************************************************************}
procedure TALAndroidMapLibreView.RemoveImage(const AName: String);
begin
  If FMaplibreMap = nil then raise Exception.Create('MapLibre is not initialized');
  var LStyle := FMaplibreMap.getStyle;
  if LStyle = nil then raise Exception.Create('MapLibre style is not loaded');
  LStyle.removeImage(StringToJString(AName));
end;

{************************************************************************************************************************************************}
procedure TALAndroidMapLibreView.SetSymbolLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PSymbolLayerOptions = nil);
begin
  If FMaplibreMap = nil then raise Exception.Create('MapLibre is not initialized');
  var LStyle := FMaplibreMap.getStyle;
  if LStyle = nil then raise Exception.Create('MapLibre style is not loaded');

  var LSource := LStyle.getSource(StringToJString(AId));
  if LSource = nil then begin
    LSource := TJGeoJsonSource.JavaClass.init(
                 StringToJString(AId), // id
                 StringToJString(AGeoJSON)); // geoJson
    LStyle.addSource(LSource);
  end
  else begin
    var LGeoJsonSource := TJGeoJsonSource.Wrap(TAndroidHelper.JObjectToID(LSource));
    LGeoJsonSource.setGeoJson(StringToJString(AGeoJSON));
  end;

  if LStyle.getLayer(StringToJString(AId)) = nil then begin
    var LLayer := TJSymbolLayer.JavaClass.init(
                    StringToJString(AId), // layerId
                    StringToJString(AId)); // sourceId

    var LOptions: TALMapLibre.TSymbolLayerOptions;
    if AOptions <> nil then LOptions := AOptions^
    else LOptions := TALMapLibre.TSymbolLayerOptions.Create;

    var LDataKeys := ALGetLayerDataKeys(AGeoJSON, ALSymbolLayerDataKeys);

    var LCount := 27;
    for var LDataKey in LDataKeys do inc(LCount);
    if Length(LOptions.FTextFont) > 0 then Inc(LCount);
    if Length(LOptions.FTextWritingMode) > 0 then inc(LCount);
    if Length(LOptions.FTextVariableAnchor) > 0 then inc(LCount);
    var LProperties := TJavaObjectArray<JPropertyValue>.Create(LCount);
    Try

      var LIdx: Integer := -1;
      for var LDataKey in LDataKeys do begin
        var LExpr := TJExpression.JavaClass.get(StringToJString(ALLayerDataKeyNames[LDataKey]));
        var LProperty: JPropertyValue;
        case LDataKey of
          TALLayerDataKey.IconImage:         LProperty := TJPropertyFactory.JavaClass.iconImage(LExpr);
          TALLayerDataKey.IconColor:         LProperty := TJPropertyFactory.JavaClass.iconColor(LExpr);
          TALLayerDataKey.IconOpacity:       LProperty := TJPropertyFactory.JavaClass.iconOpacity(LExpr);
          TALLayerDataKey.IconSize:          LProperty := TJPropertyFactory.JavaClass.iconSize(LExpr);
          TALLayerDataKey.IconAnchor:        LProperty := TJPropertyFactory.JavaClass.iconAnchor(LExpr);
          TALLayerDataKey.IconOffset:        LProperty := TJPropertyFactory.JavaClass.iconOffset(LExpr);
          TALLayerDataKey.IconRotate:        LProperty := TJPropertyFactory.JavaClass.iconRotate(LExpr);
          TALLayerDataKey.IconHaloColor:     LProperty := TJPropertyFactory.JavaClass.iconHaloColor(LExpr);
          TALLayerDataKey.IconHaloWidth:     LProperty := TJPropertyFactory.JavaClass.iconHaloWidth(LExpr);
          TALLayerDataKey.IconHaloBlur:      LProperty := TJPropertyFactory.JavaClass.iconHaloBlur(LExpr);
          TALLayerDataKey.Text:              LProperty := TJPropertyFactory.JavaClass.textField(LExpr);
          TALLayerDataKey.TextColor:         LProperty := TJPropertyFactory.JavaClass.textColor(LExpr);
          TALLayerDataKey.TextOpacity:       LProperty := TJPropertyFactory.JavaClass.textOpacity(LExpr);
          TALLayerDataKey.TextSize:          LProperty := TJPropertyFactory.JavaClass.textSize(LExpr);
          TALLayerDataKey.TextLetterSpacing: LProperty := TJPropertyFactory.JavaClass.textLetterSpacing(LExpr);
          TALLayerDataKey.TextJustify:       LProperty := TJPropertyFactory.JavaClass.textJustify(LExpr);
          TALLayerDataKey.TextAnchor:        LProperty := TJPropertyFactory.JavaClass.textAnchor(LExpr);
          TALLayerDataKey.TextOffset:        LProperty := TJPropertyFactory.JavaClass.textOffset(LExpr);
          TALLayerDataKey.TextMaxWidth:      LProperty := TJPropertyFactory.JavaClass.textMaxWidth(LExpr);
          TALLayerDataKey.TextHaloColor:     LProperty := TJPropertyFactory.JavaClass.textHaloColor(LExpr);
          TALLayerDataKey.TextHaloWidth:     LProperty := TJPropertyFactory.JavaClass.textHaloWidth(LExpr);
          TALLayerDataKey.TextHaloBlur:      LProperty := TJPropertyFactory.JavaClass.textHaloBlur(LExpr);
          TALLayerDataKey.TextRotate:        LProperty := TJPropertyFactory.JavaClass.textRotate(LExpr);
          TALLayerDataKey.TextRadialOffset:  LProperty := TJPropertyFactory.JavaClass.textRadialOffset(LExpr);
          TALLayerDataKey.TextTransform:     LProperty := TJPropertyFactory.JavaClass.textTransform(LExpr);
          TALLayerDataKey.SymbolSortKey:     LProperty := TJPropertyFactory.JavaClass.symbolSortKey(LExpr);
          else raise Exception.Create('Error 0E4F8C21-6B3A-4D7E-9A15-2C8B7F3E6D90');
        end;
        LProperties.Items[ALInc(LIdx)] := LProperty;
      end;

      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.SymbolPlacement(StringToJString(LOptions.FSymbolPlacement));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.SymbolSpacing(TJFloat.JavaClass.valueOf(LOptions.FSymbolSpacing));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.SymbolAvoidEdges(TJBoolean.JavaClass.valueOf(LOptions.FSymbolAvoidEdges));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.SymbolZOrder(StringToJString(LOptions.FSymbolZOrder));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.IconAllowOverlap(TJBoolean.JavaClass.valueOf(LOptions.FIconAllowOverlap));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.IconIgnorePlacement(TJBoolean.JavaClass.valueOf(LOptions.FIconIgnorePlacement));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.IconOptional(TJBoolean.JavaClass.valueOf(LOptions.FIconOptional));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.IconRotationAlignment(StringToJString(LOptions.FIconRotationAlignment));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.IconPitchAlignment(StringToJString(LOptions.FIconPitchAlignment));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.IconPadding(TJFloat.JavaClass.valueOf(LOptions.FIconPadding));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.IconKeepUpright(TJBoolean.JavaClass.valueOf(LOptions.FIconKeepUpright));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.IconTextFit(StringToJString(LOptions.FIconTextFit));
      //--
      var LIconTextFitPadding := TJavaObjectArray<JFloat>.Create(4);
      Try
        LIconTextFitPadding.Items[0] := TJFloat.JavaClass.valueOf(LOptions.FIconTextFitPadding.Top);
        LIconTextFitPadding.Items[1] := TJFloat.JavaClass.valueOf(LOptions.FIconTextFitPadding.Right);
        LIconTextFitPadding.Items[2] := TJFloat.JavaClass.valueOf(LOptions.FIconTextFitPadding.Bottom);
        LIconTextFitPadding.Items[3] := TJFloat.JavaClass.valueOf(LOptions.FIconTextFitPadding.Left);
        LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.IconTextFitPadding(LIconTextFitPadding);
      finally
        AlFreeAndNil(LIconTextFitPadding);
      end;
      //--
      var LIconTranslate := TJavaObjectArray<JFloat>.Create(2);
      Try
        LIconTranslate.Items[0] := TJFloat.JavaClass.valueOf(LOptions.FIconTranslate.X);
        LIconTranslate.Items[1] := TJFloat.JavaClass.valueOf(LOptions.FIconTranslate.Y);
        LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.IconTranslate(LIconTranslate);
      finally
        AlFreeAndNil(LIconTranslate);
      end;
      //--
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.IconTranslateAnchor(StringToJString(LOptions.FIconTranslateAnchor));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.TextAllowOverlap(TJBoolean.JavaClass.valueOf(LOptions.FTextAllowOverlap));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.TextIgnorePlacement(TJBoolean.JavaClass.valueOf(LOptions.FTextIgnorePlacement));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.TextOptional(TJBoolean.JavaClass.valueOf(LOptions.FTextOptional));
      //--
      if Length(LOptions.FTextFont) > 0 then begin
        var LTextFont := TJavaObjectArray<JString>.Create(Length(LOptions.FTextFont));
        Try
          for var I := 0 to High(LOptions.FTextFont) do
            LTextFont.Items[I] := StringToJString(LOptions.FTextFont[I]);
          LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.TextFont(LTextFont);
        finally
          AlFreeAndNil(LTextFont);
        end;
      end;
      //--
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.TextRotationAlignment(StringToJString(LOptions.FTextRotationAlignment));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.TextPitchAlignment(StringToJString(LOptions.FTextPitchAlignment));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.TextPadding(TJFloat.JavaClass.valueOf(LOptions.FTextPadding));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.TextKeepUpright(TJBoolean.JavaClass.valueOf(LOptions.FTextKeepUpright));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.TextMaxAngle(TJFloat.JavaClass.valueOf(LOptions.FTextMaxAngle));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.TextLineHeight(TJFloat.JavaClass.valueOf(LOptions.FTextLineHeight));
      //--
      if Length(LOptions.FTextWritingMode) > 0 then begin
        var LTextWritingMode := TJavaObjectArray<JString>.Create(Length(LOptions.FTextWritingMode));
        Try
          for var I := 0 to High(LOptions.FTextWritingMode) do
            LTextWritingMode.Items[I] := StringToJString(LOptions.FTextWritingMode[I]);
          LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.TextWritingMode(LTextWritingMode);
        finally
          AlFreeAndNil(LTextWritingMode);
        end;
      end;
      //--
      if Length(LOptions.FTextVariableAnchor) > 0 then begin
        var LTextVariableAnchor := TJavaObjectArray<JString>.Create(Length(LOptions.FTextVariableAnchor));
        Try
          for var I := 0 to High(LOptions.FTextVariableAnchor) do
            LTextVariableAnchor.Items[I] := StringToJString(LOptions.FTextVariableAnchor[I]);
          LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.TextVariableAnchor(LTextVariableAnchor);
        finally
          AlFreeAndNil(LTextVariableAnchor);
        end;
      end;
      //--
      var LTextTranslate := TJavaObjectArray<JFloat>.Create(2);
      Try
        LTextTranslate.Items[0] := TJFloat.JavaClass.valueOf(LOptions.FTextTranslate.X);
        LTextTranslate.Items[1] := TJFloat.JavaClass.valueOf(LOptions.FTextTranslate.Y);
        LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.TextTranslate(LTextTranslate);
      finally
        AlFreeAndNil(LTextTranslate);
      end;
      //--
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.TextTranslateAnchor(StringToJString(LOptions.FTextTranslateAnchor));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.Visibility(StringToJString(LOptions.FVisibility));

      Assert(LIdx = LCount - 1);
      LLayer.setProperties(LProperties);
      LStyle.addLayer(LLayer);

    finally
      AlFreeAndNil(LProperties);
    end;
  end;

  if not FSymbolLayerIds.Contains(AId) then
    FSymbolLayerIds.Add(AId);
end;

{********************************************************************************************************************************************}
procedure TALAndroidMapLibreView.SetLineLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PLineLayerOptions = nil);
begin
  If FMaplibreMap = nil then raise Exception.Create('MapLibre is not initialized');
  var LStyle := FMaplibreMap.getStyle;
  if LStyle = nil then raise Exception.Create('MapLibre style is not loaded');

  var LSource := LStyle.getSource(StringToJString(AId));
  if LSource = nil then begin
    LSource := TJGeoJsonSource.JavaClass.init(
                 StringToJString(AId), // id
                 StringToJString(AGeoJSON)); // geoJson
    LStyle.addSource(LSource);
  end
  else begin
    var LGeoJsonSource := TJGeoJsonSource.Wrap(TAndroidHelper.JObjectToID(LSource));
    LGeoJsonSource.setGeoJson(StringToJString(AGeoJSON));
  end;

  if LStyle.getLayer(StringToJString(AId)) = nil then begin
    var LLayer := TJLineLayer.JavaClass.init(
                    StringToJString(AId), // layerId
                    StringToJString(AId)); // sourceId

    var LOptions: TALMapLibre.TLineLayerOptions;
    if AOptions <> nil then LOptions := AOptions^
    else LOptions := TALMapLibre.TLineLayerOptions.Create;

    var LDataKeys := ALGetLayerDataKeys(AGeoJSON, ALLineLayerDataKeys);

    var LCount := 7;
    for var LDataKey in LDataKeys do inc(LCount);
    if Length(LOptions.FLineDasharray) > 0 then Inc(LCount);
    var LProperties := TJavaObjectArray<JPropertyValue>.Create(LCount);
    Try

      var LIdx: Integer := -1;
      for var LDataKey in LDataKeys do begin
        var LExpr := TJExpression.JavaClass.get(StringToJString(ALLayerDataKeyNames[LDataKey]));
        case LDataKey of
          TALLayerDataKey.LineColor:    LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.LineColor(LExpr);
          TALLayerDataKey.LineOpacity:  LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.LineOpacity(LExpr);
          TALLayerDataKey.LineWidth:    LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.LineWidth(LExpr);
          TALLayerDataKey.LineBlur:     LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.LineBlur(LExpr);
          TALLayerDataKey.LineGapWidth: LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.LineGapWidth(LExpr);
          TALLayerDataKey.LineOffset:   LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.LineOffset(LExpr);
          TALLayerDataKey.LineSortKey:  LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.LineSortKey(LExpr);
          else raise Exception.Create('Error B29EBDC5-4F40-38D9-E14A-D6AB3A268D6A');
        end;
      end;

      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.LineCap(StringToJString(LOptions.FLineCap));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.LineJoin(StringToJString(LOptions.FLineJoin));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.LineMiterLimit(TJFloat.JavaClass.valueOf(LOptions.FLineMiterLimit));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.LineRoundLimit(TJFloat.JavaClass.valueOf(LOptions.FLineRoundLimit));
      //--
      if Length(LOptions.FLineDasharray) > 0 then begin
        var LLineDasharray := TJavaObjectArray<JFloat>.Create(Length(LOptions.FLineDasharray));
        Try
          for var I := 0 to High(LOptions.FLineDasharray) do
            LLineDasharray.Items[I] := TJFloat.JavaClass.valueOf(LOptions.FLineDasharray[I]);
          LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.LineDasharray(LLineDasharray);
        finally
          AlFreeAndNil(LLineDasharray);
        end;
      end;
      //--
      var LLineTranslate := TJavaObjectArray<JFloat>.Create(2);
      Try
        LLineTranslate.Items[0] := TJFloat.JavaClass.valueOf(LOptions.FLineTranslate.X);
        LLineTranslate.Items[1] := TJFloat.JavaClass.valueOf(LOptions.FLineTranslate.Y);
        LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.LineTranslate(LLineTranslate);
      finally
        AlFreeAndNil(LLineTranslate);
      end;
      //--
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.LineTranslateAnchor(StringToJString(LOptions.FLineTranslateAnchor));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.Visibility(StringToJString(LOptions.FVisibility));

      Assert(LIdx = LCount - 1);
      LLayer.setProperties(LProperties);
      LStyle.addLayer(LLayer);

    finally
      AlFreeAndNil(LProperties);
    end;
  end;
end;

{************************************************************************************************************************************************}
procedure TALAndroidMapLibreView.SetCircleLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PCircleLayerOptions = nil);
begin
  If FMaplibreMap = nil then raise Exception.Create('MapLibre is not initialized');
  var LStyle := FMaplibreMap.getStyle;
  if LStyle = nil then raise Exception.Create('MapLibre style is not loaded');

  var LSource := LStyle.getSource(StringToJString(AId));
  if LSource = nil then begin
    LSource := TJGeoJsonSource.JavaClass.init(
                 StringToJString(AId), // id
                 StringToJString(AGeoJSON)); // geoJson
    LStyle.addSource(LSource);
  end
  else begin
    var LGeoJsonSource := TJGeoJsonSource.Wrap(TAndroidHelper.JObjectToID(LSource));
    LGeoJsonSource.setGeoJson(StringToJString(AGeoJSON));
  end;

  if LStyle.getLayer(StringToJString(AId)) = nil then begin
    var LLayer := TJCircleLayer.JavaClass.init(
                    StringToJString(AId), // layerId
                    StringToJString(AId)); // sourceId

    var LOptions: TALMapLibre.TCircleLayerOptions;
    if AOptions <> nil then LOptions := AOptions^
    else LOptions := TALMapLibre.TCircleLayerOptions.Create;

    var LDataKeys := ALGetLayerDataKeys(AGeoJSON, ALCircleLayerDataKeys);

    var LCount := 5;
    for var LDataKey in LDataKeys do inc(LCount);
    var LProperties := TJavaObjectArray<JPropertyValue>.Create(LCount);
    Try

      var LIdx: Integer := -1;
      for var LDataKey in LDataKeys do begin
        var LExpr := TJExpression.JavaClass.get(StringToJString(ALLayerDataKeyNames[LDataKey]));
        case LDataKey of
          TALLayerDataKey.CircleColor:         LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.CircleColor(LExpr);
          TALLayerDataKey.CircleOpacity:       LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.CircleOpacity(LExpr);
          TALLayerDataKey.CircleRadius:        LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.CircleRadius(LExpr);
          TALLayerDataKey.CircleBlur:          LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.CircleBlur(LExpr);
          TALLayerDataKey.CircleStrokeColor:   LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.CircleStrokeColor(LExpr);
          TALLayerDataKey.CircleStrokeOpacity: LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.CircleStrokeOpacity(LExpr);
          TALLayerDataKey.CircleStrokeWidth:   LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.CircleStrokeWidth(LExpr);
          TALLayerDataKey.CircleSortKey:       LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.CircleSortKey(LExpr);
          else raise Exception.Create('Error 30160E74-A7ED-532A-E085-3BD592D353D4');
        end;
      end;

      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.CirclePitchAlignment(StringToJString(LOptions.FCirclePitchAlignment));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.CirclePitchScale(StringToJString(LOptions.FCirclePitchScale));
      //--
      var LCircleTranslate := TJavaObjectArray<JFloat>.Create(2);
      Try
        LCircleTranslate.Items[0] := TJFloat.JavaClass.valueOf(LOptions.FCircleTranslate.X);
        LCircleTranslate.Items[1] := TJFloat.JavaClass.valueOf(LOptions.FCircleTranslate.Y);
        LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.CircleTranslate(LCircleTranslate);
      finally
        AlFreeAndNil(LCircleTranslate);
      end;
      //--
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.CircleTranslateAnchor(StringToJString(LOptions.FCircleTranslateAnchor));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.Visibility(StringToJString(LOptions.FVisibility));

      Assert(LIdx = LCount - 1);
      LLayer.setProperties(LProperties);
      LStyle.addLayer(LLayer);

    finally
      AlFreeAndNil(LProperties);
    end;
  end;
end;

{********************************************************************************************************************************************}
procedure TALAndroidMapLibreView.SetFillLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillLayerOptions = nil);
begin
  If FMaplibreMap = nil then raise Exception.Create('MapLibre is not initialized');
  var LStyle := FMaplibreMap.getStyle;
  if LStyle = nil then raise Exception.Create('MapLibre style is not loaded');

  var LSource := LStyle.getSource(StringToJString(AId));
  if LSource = nil then begin
    LSource := TJGeoJsonSource.JavaClass.init(
                 StringToJString(AId), // id
                 StringToJString(AGeoJSON)); // geoJson
    LStyle.addSource(LSource);
  end
  else begin
    var LGeoJsonSource := TJGeoJsonSource.Wrap(TAndroidHelper.JObjectToID(LSource));
    LGeoJsonSource.setGeoJson(StringToJString(AGeoJSON));
  end;

  if LStyle.getLayer(StringToJString(AId)) = nil then begin
    var LLayer := TJFillLayer.JavaClass.init(
                    StringToJString(AId), // layerId
                    StringToJString(AId)); // sourceId

    var LOptions: TALMapLibre.TFillLayerOptions;
    if AOptions <> nil then LOptions := AOptions^
    else LOptions := TALMapLibre.TFillLayerOptions.Create;

    var LDataKeys := ALGetLayerDataKeys(AGeoJSON, ALFillLayerDataKeys);

    var LCount := 4;
    for var LDataKey in LDataKeys do inc(LCount);
    if not LOptions.FFillPattern.IsEmpty then Inc(LCount);
    var LProperties := TJavaObjectArray<JPropertyValue>.Create(LCount);
    Try

      var LIdx: Integer := -1;
      for var LDataKey in LDataKeys do begin
        var LExpr := TJExpression.JavaClass.get(StringToJString(ALLayerDataKeyNames[LDataKey]));
        case LDataKey of
          TALLayerDataKey.FillColor:        LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.FillColor(LExpr);
          TALLayerDataKey.FillOpacity:      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.FillOpacity(LExpr);
          TALLayerDataKey.FillOutlineColor: LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.FillOutlineColor(LExpr);
          TALLayerDataKey.FillSortKey:      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.FillSortKey(LExpr);
          else raise Exception.Create('Error 21E01EB0-73EA-EE3E-EB10-7058768B1D77');
        end;
      end;

      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.FillAntialias(TJBoolean.JavaClass.valueOf(LOptions.FFillAntialias));
      //--
      var LFillTranslate := TJavaObjectArray<JFloat>.Create(2);
      Try
        LFillTranslate.Items[0] := TJFloat.JavaClass.valueOf(LOptions.FFillTranslate.X);
        LFillTranslate.Items[1] := TJFloat.JavaClass.valueOf(LOptions.FFillTranslate.Y);
        LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.FillTranslate(LFillTranslate);
      finally
        AlFreeAndNil(LFillTranslate);
      end;
      //--
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.FillTranslateAnchor(StringToJString(LOptions.FFillTranslateAnchor));
      //--
      if not LOptions.FFillPattern.IsEmpty then
        LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.FillPattern(StringToJString(LOptions.FFillPattern));
      //--
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.Visibility(StringToJString(LOptions.FVisibility));

      Assert(LIdx = LCount - 1);
      LLayer.setProperties(LProperties);
      LStyle.addLayer(LLayer);

    finally
      AlFreeAndNil(LProperties);
    end;
  end;
end;

{**************************************************************************************************************************************************************}
procedure TALAndroidMapLibreView.SetFillExtrusionLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillExtrusionLayerOptions = nil);
begin
  If FMaplibreMap = nil then raise Exception.Create('MapLibre is not initialized');
  var LStyle := FMaplibreMap.getStyle;
  if LStyle = nil then raise Exception.Create('MapLibre style is not loaded');

  var LSource := LStyle.getSource(StringToJString(AId));
  if LSource = nil then begin
    LSource := TJGeoJsonSource.JavaClass.init(
                 StringToJString(AId), // id
                 StringToJString(AGeoJSON)); // geoJson
    LStyle.addSource(LSource);
  end
  else begin
    var LGeoJsonSource := TJGeoJsonSource.Wrap(TAndroidHelper.JObjectToID(LSource));
    LGeoJsonSource.setGeoJson(StringToJString(AGeoJSON));
  end;

  if LStyle.getLayer(StringToJString(AId)) = nil then begin
    var LLayer := TJFillExtrusionLayer.JavaClass.init(
                    StringToJString(AId), // layerId
                    StringToJString(AId)); // sourceId

    var LOptions: TALMapLibre.TFillExtrusionLayerOptions;
    if AOptions <> nil then LOptions := AOptions^
    else LOptions := TALMapLibre.TFillExtrusionLayerOptions.Create;

    var LDataKeys := ALGetLayerDataKeys(AGeoJSON, ALFillExtrusionLayerDataKeys);

    var LCount := 5;
    for var LDataKey in LDataKeys do inc(LCount);
    var LProperties := TJavaObjectArray<JPropertyValue>.Create(LCount);
    Try

      var LIdx: Integer := -1;
      for var LDataKey in LDataKeys do begin
        var LExpr := TJExpression.JavaClass.get(StringToJString(ALLayerDataKeyNames[LDataKey]));
        case LDataKey of
          TALLayerDataKey.FillExtrusionColor:  LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.FillExtrusionColor(LExpr);
          TALLayerDataKey.FillExtrusionHeight: LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.FillExtrusionHeight(LExpr);
          TALLayerDataKey.FillExtrusionBase:   LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.FillExtrusionBase(LExpr);
          else raise Exception.Create('Error BB2EF362-D997-C9C7-7553-ACDC0FFFFFC8');
        end;
      end;

      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.FillExtrusionOpacity(TJFloat.JavaClass.valueOf(LOptions.FFillExtrusionOpacity));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.FillExtrusionVerticalGradient(TJBoolean.JavaClass.valueOf(LOptions.FFillExtrusionVerticalGradient));
      //--
      var LFillExtrusionTranslate := TJavaObjectArray<JFloat>.Create(2);
      Try
        LFillExtrusionTranslate.Items[0] := TJFloat.JavaClass.valueOf(LOptions.FFillExtrusionTranslate.X);
        LFillExtrusionTranslate.Items[1] := TJFloat.JavaClass.valueOf(LOptions.FFillExtrusionTranslate.Y);
        LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.FillExtrusionTranslate(LFillExtrusionTranslate);
      finally
        AlFreeAndNil(LFillExtrusionTranslate);
      end;
      //--
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.FillExtrusionTranslateAnchor(StringToJString(LOptions.FFillExtrusionTranslateAnchor));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.Visibility(StringToJString(LOptions.FVisibility));

      Assert(LIdx = LCount - 1);
      LLayer.setProperties(LProperties);
      LStyle.addLayer(LLayer);

    finally
      AlFreeAndNil(LProperties);
    end;
  end;
end;

{**************************************************************************************************************************************************}
procedure TALAndroidMapLibreView.SetHeatmapLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PHeatmapLayerOptions = nil);
begin
  If FMaplibreMap = nil then raise Exception.Create('MapLibre is not initialized');
  var LStyle := FMaplibreMap.getStyle;
  if LStyle = nil then raise Exception.Create('MapLibre style is not loaded');

  var LSource := LStyle.getSource(StringToJString(AId));
  if LSource = nil then begin
    LSource := TJGeoJsonSource.JavaClass.init(
                 StringToJString(AId), // id
                 StringToJString(AGeoJSON)); // geoJson
    LStyle.addSource(LSource);
  end
  else begin
    var LGeoJsonSource := TJGeoJsonSource.Wrap(TAndroidHelper.JObjectToID(LSource));
    LGeoJsonSource.setGeoJson(StringToJString(AGeoJSON));
  end;

  if LStyle.getLayer(StringToJString(AId)) = nil then begin
    var LLayer := TJHeatmapLayer.JavaClass.init(
                    StringToJString(AId), // layerId
                    StringToJString(AId)); // sourceId

    var LOptions: TALMapLibre.THeatmapLayerOptions;
    if AOptions <> nil then LOptions := AOptions^
    else LOptions := TALMapLibre.THeatmapLayerOptions.Create;

    var LDataKeys := ALGetLayerDataKeys(AGeoJSON, ALHeatmapLayerDataKeys);

    var LCount := 4;
    for var LDataKey in LDataKeys do inc(LCount);
    var LProperties := TJavaObjectArray<JPropertyValue>.Create(LCount);
    Try

      var LIdx: Integer := -1;
      for var LDataKey in LDataKeys do begin
        var LExpr := TJExpression.JavaClass.get(StringToJString(ALLayerDataKeyNames[LDataKey]));
        case LDataKey of
          TALLayerDataKey.HeatmapWeight: LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.HeatmapWeight(LExpr);
          else raise Exception.Create('Error 768F13BE-0861-5D3F-CC54-1410542ECEB8');
        end;
      end;

      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.HeatmapRadius(TJFloat.JavaClass.valueOf(LOptions.FHeatmapRadius));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.HeatmapIntensity(TJFloat.JavaClass.valueOf(LOptions.FHeatmapIntensity));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.HeatmapOpacity(TJFloat.JavaClass.valueOf(LOptions.FHeatmapOpacity));
      LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.Visibility(StringToJString(LOptions.FVisibility));

      Assert(LIdx = LCount - 1);
      LLayer.setProperties(LProperties);
      LStyle.addLayer(LLayer);

    finally
      AlFreeAndNil(LProperties);
    end;
  end;
end;

{********************************************************************************************************************************************************************************************************************}
procedure TALAndroidMapLibreView.SetRasterLayer(const AId: String; const ATileURL: String; const AIsTileJSON: Boolean = False; const ATileSize: Integer = 256; const AOptions: TALMapLibre.PRasterLayerOptions = nil);
begin
  If FMaplibreMap = nil then raise Exception.Create('MapLibre is not initialized');
  var LStyle := FMaplibreMap.getStyle;
  if LStyle = nil then raise Exception.Create('MapLibre style is not loaded');

  if LStyle.getLayer(StringToJString(AId)) <> nil then
    if not LStyle.removeLayer(StringToJString(AId)) then
      raise Exception.Create('Cannot remove MapLibre raster layer: ' + AId);

  if LStyle.getSource(StringToJString(AId)) <> nil then
    if not LStyle.removeSource(StringToJString(AId)) then
      raise Exception.Create('Cannot remove MapLibre raster source: ' + AId);

  var LOptions: TALMapLibre.TRasterLayerOptions;
  if AOptions <> nil then LOptions := AOptions^
  else LOptions := TALMapLibre.TRasterLayerOptions.Create;

  var LSource: JRasterSource;
  if AIsTileJSON then begin
    LSource := TJRasterSource.JavaClass.init(
                 StringToJString(AId), // id
                 StringToJString(ATileURL), // uri
                 ATileSize); // tileSize
  end
  else begin
    var LTiles := TJavaObjectArray<JString>.Create(1);
    Try
      LTiles.Items[0] := StringToJString(ATileURL);
      var LTileSet := TJTileSet.JavaClass.init(
                        StringToJString('2.2.0'), // tilejson
                        LTiles); // tiles
      LSource := TJRasterSource.JavaClass.init(
                   StringToJString(AId), // id
                   LTileSet, // tileSet
                   ATileSize); // tileSize
    finally
      AlFreeAndNil(LTiles);
    end;
  end;

  LStyle.addSource(LSource);

  var LLayer := TJRasterLayer.JavaClass.init(
                  StringToJString(AId), // layerId
                  StringToJString(AId)); // sourceId

  var LCount := 9;
  var LProperties := TJavaObjectArray<JPropertyValue>.Create(LCount);
  Try

    var LIdx: Integer := -1;
    LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.RasterOpacity(TJFloat.JavaClass.valueOf(LOptions.FRasterOpacity));
    LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.RasterHueRotate(TJFloat.JavaClass.valueOf(LOptions.FRasterHueRotate));
    LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.RasterBrightnessMin(TJFloat.JavaClass.valueOf(LOptions.FRasterBrightnessMin));
    LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.RasterBrightnessMax(TJFloat.JavaClass.valueOf(LOptions.FRasterBrightnessMax));
    LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.RasterSaturation(TJFloat.JavaClass.valueOf(LOptions.FRasterSaturation));
    LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.RasterContrast(TJFloat.JavaClass.valueOf(LOptions.FRasterContrast));
    LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.RasterResampling(StringToJString(LOptions.FRasterResampling));
    LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.RasterFadeDuration(TJFloat.JavaClass.valueOf(LOptions.FRasterFadeDuration));

    LProperties.Items[ALInc(LIdx)] := TJPropertyFactory.JavaClass.Visibility(StringToJString(LOptions.FVisibility));

    Assert(LIdx = LCount - 1);
    LLayer.setProperties(LProperties);
    LStyle.addLayer(LLayer);

  finally
    AlFreeAndNil(LProperties);
  end;
end;

{**************************************************************}
procedure TALAndroidMapLibreView.RemoveLayer(const AId: String);
begin
  var AIndex := FSymbolLayerIds.IndexOf(AId);
  If AIndex >= 0 then FSymbolLayerIds.delete(AIndex);
  if FMaplibreMap = nil then Exit;
  var LStyle := FMaplibreMap.getStyle;
  if LStyle = nil then Exit;
  var LIDJStr := StringToJString(AId);
  if LStyle.getLayer(LIDJStr) <> nil then LStyle.removeLayer(LIDJStr);
  if LStyle.getSource(LIDJStr) <> nil then LStyle.removeSource(LIDJStr);
end;

{**********************************************************}
function TALAndroidMapLibreView.GetViewportBounds: TALRectD;
begin
  if FMaplibreMap = nil then raise Exception.Create('MapLibre is not initialized');
  var LBounds := FMaplibreMap.getProjection.getVisibleRegion.latLngBounds;
  Result := TALRectD.Create(
              LBounds.getLonWest,
              LBounds.getLatSouth,
              LBounds.getLonEast,
              LBounds.getLatNorth);
end;

{*****************************************************************************}
function TALAndroidMapLibreView.GetCameraPosition: TALMapLibre.TCameraPosition;
begin
  if FMaplibreMap = nil then raise Exception.Create('MapLibre is not initialized');
  var LCameraPosition := FMaplibreMap.getCameraPosition;
  Result.bearing := LCameraPosition.bearing;
  var LPadding := LCameraPosition.padding;
  Try
    if LPadding <> nil then
      Result.padding := TALRectD.Create(
                          LPadding.Items[0] / ALGetScreenScale, // Left
                          LPadding.Items[1] / ALGetScreenScale, // Top
                          LPadding.Items[2] / ALGetScreenScale, // Right
                          LPadding.Items[3] / ALGetScreenScale) // Bottom
    else
      Result.padding := TALRectD.Empty;
  finally
    ALFreeAndNil(LPadding);
  end;
  Result.roll := LCameraPosition.roll;
  var LTarget := LCameraPosition.target;
  if LTarget <> nil then
    Result.target := TALLatLng.Create(LTarget.getLatitude, LTarget.getLongitude)
  else
    Result.target := TALLatLng.Create(ALNullLatLng, ALNullLatLng);
  Result.tilt := LCameraPosition.tilt;
  Result.zoom := LCameraPosition.zoom;
end;

{******************************************}
procedure TALAndroidMapLibreView.ApplyStyle;
begin
  if FMaplibreMap = nil then exit;
  {$IF defined(DEBUG)}
  ALLog('TALAndroidMapLibreView.ApplyStyle');
  {$ENDIF}
  var LBuilder := TJStyle_Builder.JavaClass.init;
  if Control.StyleJSON <> '' then LBuilder := LBuilder.fromJson(StringToJString(Control.StyleJSON))
  else if Control.StyleURL <> '' then LBuilder := LBuilder.fromUri(StringToJString(Control.StyleURL))
  else Exit;
  FMaplibreMap.setStyle(LBuilder);
end;

{********************************************}
procedure TALAndroidMapLibreView.ApplyCompass;
begin
  if FMaplibreMap = nil then exit;
  {$IF defined(DEBUG)}
  ALLog('TALAndroidMapLibreView.ApplyCompass');
  {$ENDIF}
  var LCompass := Control.Compass;
  var LUiSettings := FMaplibreMap.getUiSettings;
  LUiSettings.setCompassEnabled(LCompass.Enabled);
  LUiSettings.setCompassFadeFacingNorth(LCompass.FadeFacingNorth);
  var LGravity: Integer;
  case LCompass.Gravity of
    TALMapLibre.TOrnamentGravity.TopLeft: LGravity := TJGravity.JavaClass.TOP or TJGravity.JavaClass.LEFT;
    TALMapLibre.TOrnamentGravity.TopRight: LGravity := TJGravity.JavaClass.TOP or TJGravity.JavaClass.RIGHT;
    TALMapLibre.TOrnamentGravity.BottomLeft: LGravity := TJGravity.JavaClass.BOTTOM or TJGravity.JavaClass.LEFT;
    TALMapLibre.TOrnamentGravity.BottomRight: LGravity := TJGravity.JavaClass.BOTTOM or TJGravity.JavaClass.RIGHT;
    else raise Exception.Create('Error 3E6B1C0A-8F4D-4A27-9B5E-1D7C2F9A6E43');
  end;
  LUiSettings.setCompassGravity(LGravity);
  LUiSettings.setCompassMargins(
    Round(LCompass.Margins.Left * ALGetScreenScale), // left: Integer;
    Round(LCompass.Margins.Top * ALGetScreenScale), // top: Integer;
    Round(LCompass.Margins.Right * ALGetScreenScale), // right: Integer;
    Round(LCompass.Margins.Bottom * ALGetScreenScale)); // bottom: Integer
  if LCompass.ResourceName <> '' then begin
    var LBitmap := ALCreateJBitmapFromResource(
                     LCompass.ResourceName, // const AResourceName: String;
                     nil, // const AResourceStream: TStream;
                     '', // const AMaskResourceName: String;
                     ALGetScreenScale, // const AScale: Single;
                     TRectF.Empty, // const ASrcRect: TRectF; // In source image coordinates (real pixels). Use TRectF.Empty to use the full image
                     LCompass.Width, LCompass.Height, // const W, H: single;
                     False, // const AApplyMetadataOrientation: Boolean;
                     TALImageWrapMode.fit, // const AWrapMode: TALImageWrapMode;
                     TpointF.Create(0.5,0.5), // const ACropCenter: TpointF;
                     TalphaColors.null, // const ATintColor: TalphaColor;
                     0, // const ABlurRadius: single;
                     0, // const AXRadius: Single;
                     0); // const AYRadius: Single): JBitmap;
    LBitmap.setDensity(TJBitmap.JavaClass.DENSITY_NONE);
    LUiSettings.setCompassImage(TJBitmapDrawable.JavaClass.init(LBitmap));
  end;
end;

{**********************************************************************}
procedure TALAndroidMapLibreView.ShowLoadingSpinner(const AView: JView);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DisableTouchHandling(const AChildView: JView);
  begin
    // A clickable view consumes touch events (even when disabled), so make
    // sure no view in the spinner hierarchy can intercept the map gestures.
    // Touch events not consumed by the spinner are dispatched to the MapView.
    AChildView.setClickable(False);
    AChildView.setLongClickable(False);
    AChildView.setFocusable(False);
    if TJNIResolver.IsInstanceOf(AChildView, TJViewGroup.GetClsID) then begin
      var LViewGroup := TJViewGroup.Wrap(AChildView);
      for var I := 0 to LViewGroup.getChildCount - 1 do
        _DisableTouchHandling(LViewGroup.getChildAt(I));
    end;
  end;

begin
  if TJNIResolver.IsSameObject(AView, FLoadingSpinnerView) then exit;
  HideLoadingSpinner;

  var LParent: JViewParent := AView.getParent;
  if LParent <> nil then TJViewGroup.Wrap(LParent).removeView(AView);

  _DisableTouchHandling(AView);

  // Keep any explicit size supplied by the caller, otherwise wrap the content.
  var LWidth := TJViewGroup_LayoutParams.JavaClass.WRAP_CONTENT;
  var LHeight := TJViewGroup_LayoutParams.JavaClass.WRAP_CONTENT;
  var LCurrentLayoutParams := AView.getLayoutParams;
  if LCurrentLayoutParams <> nil then begin
    LWidth := LCurrentLayoutParams.width;
    LHeight := LCurrentLayoutParams.height;
  end;

  // MapView is a FrameLayout. Gravity.CENTER keeps the spinner centered when
  // the MapView is resized, and adding it as the last child draws it above
  // the map surface and the MapLibre ornaments.
  var LLayoutParams := TJFrameLayout_LayoutParams.JavaClass.init(
                         LWidth, // width: Integer;
                         LHeight, // height: Integer;
                         TJGravity.JavaClass.CENTER); // gravity: Integer
  View.addView(AView, LLayoutParams);
  FLoadingSpinnerView := AView;
end;

{******************************************************************************************************************************************************}
procedure TALAndroidMapLibreView.ShowLoadingSpinner(const AColor: TAlphaColor = Talphacolors.null; const AWidth: Single = 0; const AHeight: Single = 0);
begin
  var LProgressBar := TJProgressBar.JavaClass.init(TAndroidHelper.Context);
  LProgressBar.setIndeterminate(True);
  if AColor <> Talphacolors.null then begin
    var LDrawable := LProgressBar.getIndeterminateDrawable.mutate;
    LDrawable.setColorFilter(
      TJPorterDuffColorFilter.JavaClass.init(
        Integer(AColor), // color: Integer;
        TJPorterDuff_Mode.JavaClass.SRC_IN)); // mode: JPorterDuff_Mode
  end;
  if (CompareValue(AWidth, 0, TEpsilon.position) > 0) and
     (CompareValue(AHeight, 0, TEpsilon.position) > 0) then begin
    LProgressBar.setLayoutParams(
      TJViewGroup_LayoutParams.JavaClass.init(
        Round(AWidth * ALGetScreenScale), // width: Integer;
        Round(AHeight * ALGetScreenScale))); // height: Integer
  end;
  ShowLoadingSpinner(LProgressBar);
end;

{**************************************************}
procedure TALAndroidMapLibreView.HideLoadingSpinner;
begin
  if FLoadingSpinnerView = nil then exit;
  View.removeView(FLoadingSpinnerView);
  FLoadingSpinnerView := nil;
end;

{$endif}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(ios)}

{**********************************************************************************************}
constructor TALIosMapLibreView.TMapViewDelegate.Create(const AMapLibreView: TALIosMapLibreView);
begin
  inherited Create;
  FMapLibreView := AMapLibreView;
end;

{***************************************************************************************}
procedure TALIosMapLibreView.TMapViewDelegate.mapViewTap(sender: UITapGestureRecognizer);
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosMapLibreView.TMapViewDelegate.mapViewTap');
  {$ENDIF}

  if sender.state <> UIGestureRecognizerStateRecognized then Exit;

  var LView := FMapLibreView.View;
  var LPoint := sender.locationInView(LView);

  if Assigned(FMapLibreView.Control.FOnMapClick) then begin
    var LCoordinate := LView.convertPoint(LPoint, LView);
    FMapLibreView.Control.FOnMapClick(
      FMapLibreView.Control,
      LCoordinate.latitude,
      LCoordinate.longitude);
  end;

  if not Assigned(FMapLibreView.Control.FOnSymbolClick) then Exit;
  if LView.style = nil then Exit;

  for var LIndex := FMapLibreView.FSymbolLayerIds.Count - 1 downto 0 do begin
    var LLayerIDStr := FMapLibreView.FSymbolLayerIds[LIndex];
    var LLayerIDNSStr := StrToNSStr(LLayerIDStr);
    if LView.style.layerWithIdentifier(LLayerIDNSStr) = nil then Continue;

    var LLayers := TNSSet.Wrap(TNSSet.OCClass.setWithObject(NSObjectToID(LLayerIDNSStr)));
    var LFeatures := LView.visibleFeaturesAtPoint(LPoint, LLayers);
    if (LFeatures = nil) or (LFeatures.count = 0) then Continue;

    for var I := 0 to LFeatures.count - 1 do begin
      var LFeature := TMLNPointFeature.Wrap(LFeatures.objectAtIndex(I));
      if LFeature = nil then Continue;
      var LSymbolID := LFeature.attributeForKey(StrToNSStr('id'));
      if LSymbolID = nil then Continue;

      FMapLibreView.Control.FOnSymbolClick(
        FMapLibreView.Control, // Sender: TObject;
        LLayerIDStr, // const ALayerID: String;
        NSStrToStr(TNSString.Wrap(LSymbolID))); // const ASymbolID: String

      Exit;
    end;
  end;
end;

{*********************************************************************************************************************************************************************************}
procedure TALIosMapLibreView.TMapViewDelegate.mapViewRegionWillChangeWithReasonAnimated(mapView: MLNMapView; regionWillChangeWithReason: MLNCameraChangeReason; animated: Boolean);
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosMapLibreView.TMapViewDelegate.mapViewRegionWillChangeWithReasonAnimated');
  {$ENDIF}
  if Assigned(FMapLibreView.Control.FOnCameraWillChange) then
    FMapLibreView.Control.FOnCameraWillChange(FMapLibreView.Control);
end;

{******************************************************************************************************************************************************}
procedure TALIosMapLibreView.TMapViewDelegate.mapViewRegionIsChangingWithReason(mapView: MLNMapView; regionIsChangingWithReason: MLNCameraChangeReason);
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosMapLibreView.TMapViewDelegate.mapViewRegionIsChangingWithReason');
  {$ENDIF}
  if Assigned(FMapLibreView.Control.FOnCameraIsChanging) then
    FMapLibreView.Control.FOnCameraIsChanging(FMapLibreView.Control);
end;

{*******************************************************************************************************************************************************************************}
procedure TALIosMapLibreView.TMapViewDelegate.mapViewRegionDidChangeWithReasonAnimated(mapView: MLNMapView; regionDidChangeWithReason: MLNCameraChangeReason; animated: Boolean);
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosMapLibreView.TMapViewDelegate.mapViewRegionDidChangeWithReasonAnimated');
  {$ENDIF}
  if Assigned(FMapLibreView.Control.FOnCameraDidChange) then
    FMapLibreView.Control.FOnCameraDidChange(FMapLibreView.Control);
end;

{*******************************************************************************************************************************}
procedure TALIosMapLibreView.TMapViewDelegate.mapViewDidFinishLoadingStyle(mapView: MLNMapView; didFinishLoadingStyle: MLNStyle);
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosMapLibreView.TMapViewDelegate.mapViewDidFinishLoadingStyle');
  {$ENDIF}
  FMapLibreView.FSymbolLayerIds.Clear;
  if Assigned(FMapLibreView.Control.FOnStyleLoaded) then
    FMapLibreView.Control.FOnStyleLoaded(FMapLibreView.Control);
end;

{************************************}
constructor TALIosMapLibreView.Create;
begin
  FThreadQueueCleaner := TALThreadQueueCleaner.Create;

  inherited; // This will call InitView

  FMapViewDelegate := TMapViewDelegate.Create(Self);
  View.setDelegate(FMapViewDelegate);

  FMapViewTapGestureRecognizer := TUITapGestureRecognizer.Wrap(
                                    TUITapGestureRecognizer.Alloc.initWithTarget(
                                      FMapViewDelegate.GetObjectID,
                                      sel_getUid('mapViewTap:')));
  FMapViewTapGestureRecognizer.setNumberOfTapsRequired(1);
  FMapViewTapGestureRecognizer.setCancelsTouchesInView(False);
  View.addGestureRecognizer(FMapViewTapGestureRecognizer);

  FLoadingSpinnerView := nil;

  FSymbolLayerIds := TALStringListW.Create;

  ApplyCompass;
end;

{************************************}
destructor TALIosMapLibreView.Destroy;
begin
  HideLoadingSpinner;

  ALFreeAndNil(FThreadQueueCleaner);

  View.removeGestureRecognizer(FMapViewTapGestureRecognizer);
  FMapViewTapGestureRecognizer.release;
  FMapViewTapGestureRecognizer := nil;

  View.setDelegate(nil);
  ALFreeAndNil(FMapViewDelegate);

  ALFreeAndNil(FSymbolLayerIds);

  inherited Destroy;
end;

{************************************}
procedure TALIosMapLibreView.InitView;
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosMapLibreView.InitView', 'Visible: '+ ALBoolToStrW(Visible, 'True', 'False'));
  {$ENDIF}
  var V: Pointer;
  if Control.StyleJSON <> '' then V := NSObjectToID(MLNMapView(Super).initWithFrame(CGRectFromRect(Control.GetNativeViewAbsoluteRect), StrToNSStr(Control.StyleJSON)))
  else if Control.StyleURL <> '' then V := NSObjectToID(MLNMapView(Super).initWithFrame(CGRectFromRect(Control.GetNativeViewAbsoluteRect), StrToNSUrl(Control.StyleURL)))
  else V := NSObjectToID(MLNMapView(Super).initWithFrame(CGRectFromRect(Control.GetNativeViewAbsoluteRect), StrToNSStr('{"version":8,"sources":{},"layers":[]}')));
  if GetObjectID <> V then
    UpdateObjectID(V);
  // Defer OnMapReady to the next main-thread cycle to allow event handlers
  // to be assigned after the control is created programmatically.
  TThread.ForceQueue(
    FThreadQueueCleaner,
    procedure
    begin
       {$IF defined(DEBUG)}
      ALLog('TALIosMapLibreView.onMapReady', 'Visible: '+ ALBoolToStrW(Visible, 'True', 'False'));
      {$ENDIF}
      if Assigned(Control.FOnMapReady) then
        Control.FOnMapReady(Control);
    end);
end;

{********************************************************}
function TALIosMapLibreView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALIosMapLibreView);
end;

{**********************************************}
function TALIosMapLibreView.GetView: MLNMapView;
begin
  Result := inherited GetView<MLNMapView>;
end;

{**************************************************}
function TALIosMapLibreView.GetControl: TALMapLibre;
begin
  Result := TALMapLibre(inherited Control);
end;

{**************************************************************************************************************}
procedure TALIosMapLibreView.SetCenterCoordinate(const ALatitude, ALongitude: Double; const AZoomLevel: double);
begin
  var LCoordinate: CLLocationCoordinate2D;
  LCoordinate.latitude := ALatitude;
  LCoordinate.longitude := ALongitude;
  view.setCenterCoordinate(
    LCoordinate, // coordinate: CLLocationCoordinate2D;
    AZoomLevel, // zoomLevel: Double;
    False); // animated: Boolean
end;

{************************************}
procedure TALIosMapLibreView.SetImage(
            const AResourceName: String;
            const AStream: TStream;
            const AScale: Single;
            const W, H: single;
            const AName: String);
begin
  var LStyle := View.style;
  if LStyle = nil then raise Exception.Create('MapLibre style is not loaded');
  var LCGImageRef := ALCreateCGImageRefFromResource(
                       AResourceName, // const AResourceName: String;
                       AStream, // const AResourceStream: TStream;
                       '', // const AMaskResourceName: String;
                       AScale, // const AScale: Single;
                       TRectF.Empty, // const ASrcRect: TRectF; // In source image coordinates (real pixels). Use TRectF.Empty to use the full image
                       W, H, // const W, H: single;
                       False, // const AApplyMetadataOrientation: Boolean;
                       TALImageWrapMode.fit, // const AWrapMode: TALImageWrapMode;
                       TpointF.Create(0.5,0.5), // const ACropCenter: TpointF;
                       TalphaColors.null, // const ATintColor: TalphaColor;
                       0, // const ABlurRadius: single;
                       0, // const AXRadius: Single;
                       0); // const AYRadius: Single): CGImageRef;
  try
    var LUIImage := TUIImage.OCClass.imageWithCGImage(LCGImageRef, AScale, UIImageOrientationUp);
    LStyle.setImage(LUIImage, StrToNSStr(AName));
  finally
    CGImageRelease(LCGImageRef);
  end;
end;

{************************************************************}
procedure TALIosMapLibreView.RemoveImage(const AName: String);
begin
  var LStyle := View.style;
  if LStyle = nil then raise Exception.Create('MapLibre style is not loaded');
  LStyle.removeImageForName(StrToNSStr(AName));
end;

{********************************************************************************************************************************************}
procedure TALIosMapLibreView.SetSymbolLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PSymbolLayerOptions = nil);
begin
  var LStyle := View.style;
  if LStyle = nil then raise Exception.Create('MapLibre style is not loaded');

  var LData := StrToNSStr(AGeoJSON).dataUsingEncoding(NSUTF8StringEncoding);
  var LError: Pointer := nil;
  var LShape := TMLNShape.OCClass.shapeWithData(LData{data}, NSUTF8StringEncoding{encoding}, @LError{error});
  if LShape = nil then begin
    if LError <> nil then raise Exception.Create(NSStrToStr(TNSError.Wrap(LError).localizedDescription))
    else raise Exception.Create('Failed to parse GeoJSON');
  end;

  var LReleaseSource: Boolean := False;
  var LSource := LStyle.sourceWithIdentifier(StrToNSStr(AId));
  try
    if LSource = nil then begin
      LSource := TMLNShapeSource.Alloc.initWithIdentifier(StrToNSStr(AId){identifier}, LShape{shape}, nil{options});
      LReleaseSource := True;
      LStyle.addSource(LSource);
    end
    else
      TMLNShapeSource.Wrap(LSource).setShape(LShape);

    if LStyle.layerWithIdentifier(StrToNSStr(AId)) = nil then begin

      var LLayer := TMLNSymbolStyleLayer.Alloc.initWithIdentifier(StrToNSStr(AId){identifier}, LSource{source});
      Try

        var LOptions: TALMapLibre.TSymbolLayerOptions;
        if AOptions <> nil then LOptions := AOptions^
        else LOptions := TALMapLibre.TSymbolLayerOptions.Create;

        var LDataKeys := ALGetLayerDataKeys(AGeoJSON, ALSymbolLayerDataKeys);
        for var LDataKey in LDataKeys do begin
          var LExpr := TNSExpression.OCClass.expressionForKeyPath(StrToNSStr(ALLayerDataKeyNames[LDataKey]));
          case LDataKey of
            TALLayerDataKey.IconImage:         LLayer.setIconImageName(LExpr);
            TALLayerDataKey.IconColor:         LLayer.setIconColor(LExpr);
            TALLayerDataKey.IconOpacity:       LLayer.setIconOpacity(LExpr);
            TALLayerDataKey.IconSize:          LLayer.setIconScale(LExpr);
            TALLayerDataKey.IconAnchor:        LLayer.setIconAnchor(LExpr);
            TALLayerDataKey.IconOffset:        LLayer.setIconOffset(LExpr);
            TALLayerDataKey.IconRotate:        LLayer.setIconRotation(LExpr);
            TALLayerDataKey.IconHaloColor:     LLayer.setIconHaloColor(LExpr);
            TALLayerDataKey.IconHaloWidth:     LLayer.setIconHaloWidth(LExpr);
            TALLayerDataKey.IconHaloBlur:      LLayer.setIconHaloBlur(LExpr);
            TALLayerDataKey.Text:              LLayer.setText(LExpr);
            TALLayerDataKey.TextColor:         LLayer.setTextColor(LExpr);
            TALLayerDataKey.TextOpacity:       LLayer.setTextOpacity(LExpr);
            TALLayerDataKey.TextSize:          LLayer.setTextFontSize(LExpr);
            TALLayerDataKey.TextLetterSpacing: LLayer.setTextLetterSpacing(LExpr);
            TALLayerDataKey.TextJustify:       LLayer.setTextJustification(LExpr);
            TALLayerDataKey.TextAnchor:        LLayer.setTextAnchor(LExpr);
            TALLayerDataKey.TextOffset:        LLayer.setTextOffset(LExpr);
            TALLayerDataKey.TextMaxWidth:      LLayer.setMaximumTextWidth(LExpr);
            TALLayerDataKey.TextHaloColor:     LLayer.setTextHaloColor(LExpr);
            TALLayerDataKey.TextHaloWidth:     LLayer.setTextHaloWidth(LExpr);
            TALLayerDataKey.TextHaloBlur:      LLayer.setTextHaloBlur(LExpr);
            TALLayerDataKey.TextRotate:        LLayer.setTextRotation(LExpr);
            TALLayerDataKey.TextRadialOffset:  LLayer.setTextRadialOffset(LExpr);
            TALLayerDataKey.TextTransform:     LLayer.setTextTransform(LExpr);
            TALLayerDataKey.SymbolSortKey:     LLayer.setSymbolSortKey(LExpr);
            else raise Exception.Create('Error 7A2D9E64-3F1B-4C85-B0E7-5D6A1C9F2B48');
          end;
        end;

        LLayer.setSymbolPlacement(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(StrToNSStr(LOptions.FSymbolPlacement))));
        LLayer.setSymbolSpacing(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithDouble(LOptions.FSymbolSpacing))));
        LLayer.setSymbolAvoidsEdges(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithBool(LOptions.FSymbolAvoidEdges))));
        LLayer.setSymbolZOrder(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(StrToNSStr(LOptions.FSymbolZOrder))));
        LLayer.setIconAllowsOverlap(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithBool(LOptions.FIconAllowOverlap))));
        LLayer.setIconIgnoresPlacement(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithBool(LOptions.FIconIgnorePlacement))));
        LLayer.setIconOptional(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithBool(LOptions.FIconOptional))));
        LLayer.setIconRotationAlignment(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(StrToNSStr(LOptions.FIconRotationAlignment))));
        LLayer.setIconPitchAlignment(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(StrToNSStr(LOptions.FIconPitchAlignment))));
        LLayer.setIconPadding(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithDouble(LOptions.FIconPadding))));
        LLayer.setKeepsIconUpright(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithBool(LOptions.FIconKeepUpright))));
        LLayer.setIconTextFit(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(StrToNSStr(LOptions.FIconTextFit))));
        //--
        var LIconTextFitPadding: UIEdgeInsets;
        LIconTextFitPadding.top := LOptions.FIconTextFitPadding.Top;
        LIconTextFitPadding.left := LOptions.FIconTextFitPadding.Left;
        LIconTextFitPadding.bottom := LOptions.FIconTextFitPadding.Bottom;
        LIconTextFitPadding.right := LOptions.FIconTextFitPadding.Right;
        LLayer.setIconTextFitPadding(TNSExpression.OCClass.expressionForConstantValue(iOSapi.UIKit.TNSValue.OCClass.valueWithUIEdgeInsets(LIconTextFitPadding)));
        //--
        var LIconTranslate: CGVector;
        LIconTranslate.dx := LOptions.FIconTranslate.X;
        LIconTranslate.dy := LOptions.FIconTranslate.Y;
        LLayer.setIconTranslation(TNSExpression.OCClass.expressionForConstantValue(Alcinoe.iOSApi.UIKit.TNSValue.OCClass.valueWithCGVector(LIconTranslate)));
        //--
        LLayer.setIconTranslationAnchor(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(StrToNSStr(LOptions.FIconTranslateAnchor))));
        LLayer.setTextAllowsOverlap(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithBool(LOptions.FTextAllowOverlap))));
        LLayer.setTextIgnoresPlacement(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithBool(LOptions.FTextIgnorePlacement))));
        LLayer.setTextOptional(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithBool(LOptions.FTextOptional))));
        //--
        if Length(LOptions.FTextFont) > 0 then begin
          var LTextFont := TNSMutableArray.wrap(TNSMutableArray.OCClass.arrayWithCapacity(Length(LOptions.FTextFont)));
          for var LValue in LOptions.FTextFont do LTextFont.addObject(NSObjectToID(StrToNSStr(LValue)));
          LLayer.setTextFontNames(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(LTextFont)));
        end;
        //--
        LLayer.setTextRotationAlignment(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(StrToNSStr(LOptions.FTextRotationAlignment))));
        LLayer.setTextPitchAlignment(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(StrToNSStr(LOptions.FTextPitchAlignment))));
        LLayer.setTextPadding(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithDouble(LOptions.FTextPadding))));
        LLayer.setKeepsTextUpright(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithBool(LOptions.FTextKeepUpright))));
        LLayer.setMaximumTextAngle(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithDouble(LOptions.FTextMaxAngle))));
        LLayer.setTextLineHeight(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithDouble(LOptions.FTextLineHeight))));
        //--
        if Length(LOptions.FTextWritingMode) > 0 then begin
          var LTextWritingMode := TNSMutableArray.wrap(TNSMutableArray.OCClass.arrayWithCapacity(Length(LOptions.FTextWritingMode)));
          for var LValue in LOptions.FTextWritingMode do LTextWritingMode.addObject(NSObjectToID(StrToNSStr(LValue)));
          LLayer.setTextWritingModes(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(LTextWritingMode)));
        end;
        //--
        if Length(LOptions.FTextVariableAnchor) > 0 then begin
          var LTextVariableAnchor := TNSMutableArray.wrap(TNSMutableArray.OCClass.arrayWithCapacity(Length(LOptions.FTextVariableAnchor)));
          for var LValue in LOptions.FTextVariableAnchor do LTextVariableAnchor.addObject(NSObjectToID(StrToNSStr(LValue)));
          LLayer.setTextVariableAnchor(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(LTextVariableAnchor)));
        end;
        //--
        var LTextTranslate: CGVector;
        LTextTranslate.dx := LOptions.FTextTranslate.X;
        LTextTranslate.dy := LOptions.FTextTranslate.Y;
        LLayer.setTextTranslation(TNSExpression.OCClass.expressionForConstantValue(Alcinoe.iOSapi.UIKit.TNSValue.OCClass.valueWithCGVector(LTextTranslate)));
        //--
        LLayer.setTextTranslationAnchor(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(StrToNSStr(LOptions.FTextTranslateAnchor))));
        LLayer.setVisible(LOptions.FVisibility <> 'none');

        LStyle.addLayer(LLayer);

      finally
        LLayer.release;
      end;
    end;
  finally
    if LReleaseSource then LSource.release;
  end;

  if not FSymbolLayerIds.Contains(AId) then
    FSymbolLayerIds.Add(AId);
end;

{****************************************************************************************************************************************}
procedure TALIosMapLibreView.SetLineLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PLineLayerOptions = nil);
begin
  var LStyle := View.style;
  if LStyle = nil then raise Exception.Create('MapLibre style is not loaded');

  var LData := StrToNSStr(AGeoJSON).dataUsingEncoding(NSUTF8StringEncoding);
  var LError: Pointer := nil;
  var LShape := TMLNShape.OCClass.shapeWithData(LData{data}, NSUTF8StringEncoding{encoding}, @LError{error});
  if LShape = nil then begin
    if LError <> nil then raise Exception.Create(NSStrToStr(TNSError.Wrap(LError).localizedDescription))
    else raise Exception.Create('Failed to parse GeoJSON');
  end;

  var LReleaseSource: Boolean := False;
  var LSource := LStyle.sourceWithIdentifier(StrToNSStr(AId));
  Try
    if LSource = nil then begin
      LSource := TMLNShapeSource.Alloc.initWithIdentifier(StrToNSStr(AId){identifier}, LShape{shape}, nil{options});
      LReleaseSource := True;
      LStyle.addSource(LSource);
    end
    else
      TMLNShapeSource.Wrap(LSource).setShape(LShape);

    if LStyle.layerWithIdentifier(StrToNSStr(AId)) = nil then begin

      var LLayer := TMLNLineStyleLayer.Alloc.initWithIdentifier(StrToNSStr(AId){identifier}, LSource{source});
      Try

        var LOptions: TALMapLibre.TLineLayerOptions;
        if AOptions <> nil then LOptions := AOptions^
        else LOptions := TALMapLibre.TLineLayerOptions.Create;

        var LDataKeys := ALGetLayerDataKeys(AGeoJSON, ALLineLayerDataKeys);
        for var LDataKey in LDataKeys do begin
          var LExpr := TNSExpression.OCClass.expressionForKeyPath(StrToNSStr(ALLayerDataKeyNames[LDataKey]));
          case LDataKey of
            TALLayerDataKey.LineColor:    LLayer.setLineColor(LExpr);
            TALLayerDataKey.LineOpacity:  LLayer.setLineOpacity(LExpr);
            TALLayerDataKey.LineWidth:    LLayer.setLineWidth(LExpr);
            TALLayerDataKey.LineBlur:     LLayer.setLineBlur(LExpr);
            TALLayerDataKey.LineGapWidth: LLayer.setLineGapWidth(LExpr);
            TALLayerDataKey.LineOffset:   LLayer.setLineOffset(LExpr);
            TALLayerDataKey.LineSortKey:  LLayer.setLineSortKey(LExpr);
            else raise Exception.Create('Error B4888A17-17FD-897A-1269-6EEE6966B786');
          end;
        end;

        LLayer.setLineCap(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(StrToNSStr(LOptions.FLineCap))));
        LLayer.setLineJoin(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(StrToNSStr(LOptions.FLineJoin))));
        LLayer.setLineMiterLimit(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithDouble(LOptions.FLineMiterLimit))));
        LLayer.setLineRoundLimit(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithDouble(LOptions.FLineRoundLimit))));
        if Length(LOptions.FLineDasharray) > 0 then begin
          var LLineDasharray := TNSMutableArray.wrap(TNSMutableArray.OCClass.arrayWithCapacity(Length(LOptions.FLineDasharray)));
          for var LValue in LOptions.FLineDasharray do LLineDasharray.addObject(NSObjectToID(TNSNumber.OCClass.numberWithDouble(LValue)));
          LLayer.setLineDashPattern(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(LLineDasharray)));
        end;
        //--
        var LLineTranslate: CGVector;
        LLineTranslate.dx := LOptions.FLineTranslate.X;
        LLineTranslate.dy := LOptions.FLineTranslate.Y;
        LLayer.setLineTranslation(TNSExpression.OCClass.expressionForConstantValue(Alcinoe.iOSapi.UIKit.TNSValue.OCClass.valueWithCGVector(LLineTranslate)));
        //--
        LLayer.setLineTranslationAnchor(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(StrToNSStr(LOptions.FLineTranslateAnchor))));
        LLayer.setVisible(LOptions.FVisibility <> 'none');

        LStyle.addLayer(LLayer);

      finally
        LLayer.release;
      end;
    end;
  finally
    if LReleaseSource then LSource.release;
  end;
end;

{********************************************************************************************************************************************}
procedure TALIosMapLibreView.SetCircleLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PCircleLayerOptions = nil);
begin
  var LStyle := View.style;
  if LStyle = nil then raise Exception.Create('MapLibre style is not loaded');

  var LData := StrToNSStr(AGeoJSON).dataUsingEncoding(NSUTF8StringEncoding);
  var LError: Pointer := nil;
  var LShape := TMLNShape.OCClass.shapeWithData(LData{data}, NSUTF8StringEncoding{encoding}, @LError{error});
  if LShape = nil then begin
    if LError <> nil then raise Exception.Create(NSStrToStr(TNSError.Wrap(LError).localizedDescription))
    else raise Exception.Create('Failed to parse GeoJSON');
  end;

  var LReleaseSource: Boolean := False;
  var LSource := LStyle.sourceWithIdentifier(StrToNSStr(AId));
  Try
    if LSource = nil then begin
      LSource := TMLNShapeSource.Alloc.initWithIdentifier(StrToNSStr(AId){identifier}, LShape{shape}, nil{options});
      LReleaseSource := True;
      LStyle.addSource(LSource);
    end
    else
      TMLNShapeSource.Wrap(LSource).setShape(LShape);

    if LStyle.layerWithIdentifier(StrToNSStr(AId)) = nil then begin

      var LLayer := TMLNCircleStyleLayer.Alloc.initWithIdentifier(StrToNSStr(AId){identifier}, LSource{source});
      Try

        var LOptions: TALMapLibre.TCircleLayerOptions;
        if AOptions <> nil then LOptions := AOptions^
        else LOptions := TALMapLibre.TCircleLayerOptions.Create;

        var LDataKeys := ALGetLayerDataKeys(AGeoJSON, ALCircleLayerDataKeys);
        for var LDataKey in LDataKeys do begin
          var LExpr := TNSExpression.OCClass.expressionForKeyPath(StrToNSStr(ALLayerDataKeyNames[LDataKey]));
          case LDataKey of
            TALLayerDataKey.CircleColor:         LLayer.setCircleColor(LExpr);
            TALLayerDataKey.CircleOpacity:       LLayer.setCircleOpacity(LExpr);
            TALLayerDataKey.CircleRadius:        LLayer.setCircleRadius(LExpr);
            TALLayerDataKey.CircleBlur:          LLayer.setCircleBlur(LExpr);
            TALLayerDataKey.CircleStrokeColor:   LLayer.setCircleStrokeColor(LExpr);
            TALLayerDataKey.CircleStrokeOpacity: LLayer.setCircleStrokeOpacity(LExpr);
            TALLayerDataKey.CircleStrokeWidth:   LLayer.setCircleStrokeWidth(LExpr);
            TALLayerDataKey.CircleSortKey:       LLayer.setCircleSortKey(LExpr);
            else raise Exception.Create('Error C23C4508-710A-B064-7F68-8A2E7C608FDD');
          end;
        end;

        LLayer.setCirclePitchAlignment(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(StrToNSStr(LOptions.FCirclePitchAlignment))));
        LLayer.setCircleScaleAlignment(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(StrToNSStr(LOptions.FCirclePitchScale))));
        //--
        var LCircleTranslate: CGVector;
        LCircleTranslate.dx := LOptions.FCircleTranslate.X;
        LCircleTranslate.dy := LOptions.FCircleTranslate.Y;
        LLayer.setCircleTranslation(TNSExpression.OCClass.expressionForConstantValue(Alcinoe.iOSapi.UIKit.TNSValue.OCClass.valueWithCGVector(LCircleTranslate)));
        //--
        LLayer.setCircleTranslationAnchor(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(StrToNSStr(LOptions.FCircleTranslateAnchor))));
        LLayer.setVisible(LOptions.FVisibility <> 'none');

        LStyle.addLayer(LLayer);

      finally
        LLayer.release;
      end;
    end;
  finally
    if LReleaseSource then LSource.release;
  end;
end;

{****************************************************************************************************************************************}
procedure TALIosMapLibreView.SetFillLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillLayerOptions = nil);
begin
  var LStyle := View.style;
  if LStyle = nil then raise Exception.Create('MapLibre style is not loaded');

  var LData := StrToNSStr(AGeoJSON).dataUsingEncoding(NSUTF8StringEncoding);
  var LError: Pointer := nil;
  var LShape := TMLNShape.OCClass.shapeWithData(LData{data}, NSUTF8StringEncoding{encoding}, @LError{error});
  if LShape = nil then begin
    if LError <> nil then raise Exception.Create(NSStrToStr(TNSError.Wrap(LError).localizedDescription))
    else raise Exception.Create('Failed to parse GeoJSON');
  end;

  var LReleaseSource: Boolean := False;
  var LSource := LStyle.sourceWithIdentifier(StrToNSStr(AId));
  Try
    if LSource = nil then begin
      LSource := TMLNShapeSource.Alloc.initWithIdentifier(StrToNSStr(AId){identifier}, LShape{shape}, nil{options});
      LReleaseSource := True;
      LStyle.addSource(LSource);
    end
    else
      TMLNShapeSource.Wrap(LSource).setShape(LShape);

    if LStyle.layerWithIdentifier(StrToNSStr(AId)) = nil then begin

      var LLayer := TMLNFillStyleLayer.Alloc.initWithIdentifier(StrToNSStr(AId){identifier}, LSource{source});
      Try

        var LOptions: TALMapLibre.TFillLayerOptions;
        if AOptions <> nil then LOptions := AOptions^
        else LOptions := TALMapLibre.TFillLayerOptions.Create;

        var LDataKeys := ALGetLayerDataKeys(AGeoJSON, ALFillLayerDataKeys);
        for var LDataKey in LDataKeys do begin
          var LExpr := TNSExpression.OCClass.expressionForKeyPath(StrToNSStr(ALLayerDataKeyNames[LDataKey]));
          case LDataKey of
            TALLayerDataKey.FillColor:        LLayer.setFillColor(LExpr);
            TALLayerDataKey.FillOpacity:      LLayer.setFillOpacity(LExpr);
            TALLayerDataKey.FillOutlineColor: LLayer.setFillOutlineColor(LExpr);
            TALLayerDataKey.FillSortKey:      LLayer.setFillSortKey(LExpr);
            else raise Exception.Create('Error 628F9E4D-438B-AB65-F5F3-E3CF3EBBDF22');
          end;
        end;

        LLayer.setFillAntialiased(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithBool(LOptions.FFillAntialias))));
        //--
        var LFillTranslate: CGVector;
        LFillTranslate.dx := LOptions.FFillTranslate.X;
        LFillTranslate.dy := LOptions.FFillTranslate.Y;
        LLayer.setFillTranslation(TNSExpression.OCClass.expressionForConstantValue(Alcinoe.iOSapi.UIKit.TNSValue.OCClass.valueWithCGVector(LFillTranslate)));
        //--
        LLayer.setFillTranslationAnchor(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(StrToNSStr(LOptions.FFillTranslateAnchor))));
        if not LOptions.FFillPattern.IsEmpty then
          LLayer.setFillPattern(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(StrToNSStr(LOptions.FFillPattern))));
        LLayer.setVisible(LOptions.FVisibility <> 'none');

        LStyle.addLayer(LLayer);

      finally
        LLayer.release;
      end;
    end;
  finally
    if LReleaseSource then LSource.release;
  end;
end;

{**********************************************************************************************************************************************************}
procedure TALIosMapLibreView.SetFillExtrusionLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillExtrusionLayerOptions = nil);
begin
  var LStyle := View.style;
  if LStyle = nil then raise Exception.Create('MapLibre style is not loaded');

  var LData := StrToNSStr(AGeoJSON).dataUsingEncoding(NSUTF8StringEncoding);
  var LError: Pointer := nil;
  var LShape := TMLNShape.OCClass.shapeWithData(LData{data}, NSUTF8StringEncoding{encoding}, @LError{error});
  if LShape = nil then begin
    if LError <> nil then raise Exception.Create(NSStrToStr(TNSError.Wrap(LError).localizedDescription))
    else raise Exception.Create('Failed to parse GeoJSON');
  end;

  var LReleaseSource: Boolean := False;
  var LSource := LStyle.sourceWithIdentifier(StrToNSStr(AId));
  Try
    if LSource = nil then begin
      LSource := TMLNShapeSource.Alloc.initWithIdentifier(StrToNSStr(AId){identifier}, LShape{shape}, nil{options});
      LReleaseSource := True;
      LStyle.addSource(LSource);
    end
    else
      TMLNShapeSource.Wrap(LSource).setShape(LShape);

    if LStyle.layerWithIdentifier(StrToNSStr(AId)) = nil then begin

      var LLayer := TMLNFillExtrusionStyleLayer.Alloc.initWithIdentifier(StrToNSStr(AId){identifier}, LSource{source});
      Try

        var LOptions: TALMapLibre.TFillExtrusionLayerOptions;
        if AOptions <> nil then LOptions := AOptions^
        else LOptions := TALMapLibre.TFillExtrusionLayerOptions.Create;

        var LDataKeys := ALGetLayerDataKeys(AGeoJSON, ALFillExtrusionLayerDataKeys);
        for var LDataKey in LDataKeys do begin
          var LExpr := TNSExpression.OCClass.expressionForKeyPath(StrToNSStr(ALLayerDataKeyNames[LDataKey]));
          case LDataKey of
            TALLayerDataKey.FillExtrusionColor:  LLayer.setFillExtrusionColor(LExpr);
            TALLayerDataKey.FillExtrusionHeight: LLayer.setFillExtrusionHeight(LExpr);
            TALLayerDataKey.FillExtrusionBase:   LLayer.setFillExtrusionBase(LExpr);
            else raise Exception.Create('Error F78361DF-5AC0-C47C-1970-3F0BDF2A383A');
          end;
        end;

        LLayer.setFillExtrusionOpacity(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithDouble(LOptions.FFillExtrusionOpacity))));
        LLayer.setFillExtrusionHasVerticalGradient(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithBool(LOptions.FFillExtrusionVerticalGradient))));
        //--
        var LFillExtrusionTranslate: CGVector;
        LFillExtrusionTranslate.dx := LOptions.FFillExtrusionTranslate.X;
        LFillExtrusionTranslate.dy := LOptions.FFillExtrusionTranslate.Y;
        LLayer.setFillExtrusionTranslation(TNSExpression.OCClass.expressionForConstantValue(Alcinoe.iOSapi.UIKit.TNSValue.OCClass.valueWithCGVector(LFillExtrusionTranslate)));
        //--
        LLayer.setFillExtrusionTranslationAnchor(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(StrToNSStr(LOptions.FFillExtrusionTranslateAnchor))));
        LLayer.setVisible(LOptions.FVisibility <> 'none');

        LStyle.addLayer(LLayer);

      finally
        LLayer.release;
      end;
    end;
  finally
    if LReleaseSource then LSource.release;
  end;
end;

{**********************************************************************************************************************************************}
procedure TALIosMapLibreView.SetHeatmapLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PHeatmapLayerOptions = nil);
begin
  var LStyle := View.style;
  if LStyle = nil then raise Exception.Create('MapLibre style is not loaded');

  var LData := StrToNSStr(AGeoJSON).dataUsingEncoding(NSUTF8StringEncoding);
  var LError: Pointer := nil;
  var LShape := TMLNShape.OCClass.shapeWithData(LData{data}, NSUTF8StringEncoding{encoding}, @LError{error});
  if LShape = nil then begin
    if LError <> nil then raise Exception.Create(NSStrToStr(TNSError.Wrap(LError).localizedDescription))
    else raise Exception.Create('Failed to parse GeoJSON');
  end;

  var LReleaseSource: Boolean := False;
  var LSource := LStyle.sourceWithIdentifier(StrToNSStr(AId));
  Try
    if LSource = nil then begin
      LSource := TMLNShapeSource.Alloc.initWithIdentifier(StrToNSStr(AId){identifier}, LShape{shape}, nil{options});
      LReleaseSource := True;
      LStyle.addSource(LSource);
    end
    else
      TMLNShapeSource.Wrap(LSource).setShape(LShape);

    if LStyle.layerWithIdentifier(StrToNSStr(AId)) = nil then begin

      var LLayer := TMLNHeatmapStyleLayer.Alloc.initWithIdentifier(StrToNSStr(AId){identifier}, LSource{source});
      Try

        var LOptions: TALMapLibre.THeatmapLayerOptions;
        if AOptions <> nil then LOptions := AOptions^
        else LOptions := TALMapLibre.THeatmapLayerOptions.Create;

        var LDataKeys := ALGetLayerDataKeys(AGeoJSON, ALHeatmapLayerDataKeys);
        for var LDataKey in LDataKeys do begin
          var LExpr := TNSExpression.OCClass.expressionForKeyPath(StrToNSStr(ALLayerDataKeyNames[LDataKey]));
          case LDataKey of
            TALLayerDataKey.HeatmapWeight: LLayer.setHeatmapWeight(LExpr);
            else raise Exception.Create('Error 3D349B2D-4704-BE9C-82D6-6D0749D98833');
          end;
        end;

        LLayer.setHeatmapRadius(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithDouble(LOptions.FHeatmapRadius))));
        LLayer.setHeatmapIntensity(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithDouble(LOptions.FHeatmapIntensity))));
        LLayer.setHeatmapOpacity(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithDouble(LOptions.FHeatmapOpacity))));
        LLayer.setVisible(LOptions.FVisibility <> 'none');

        LStyle.addLayer(LLayer);

      finally
        LLayer.release;
      end;
    end;
  finally
    if LReleaseSource then LSource.release;
  end;
end;

{****************************************************************************************************************************************************************************************************************}
procedure TALIosMapLibreView.SetRasterLayer(const AId: String; const ATileURL: String; const AIsTileJSON: Boolean = False; const ATileSize: Integer = 256; const AOptions: TALMapLibre.PRasterLayerOptions = nil);
begin
  var LStyle := View.style;
  if LStyle = nil then raise Exception.Create('MapLibre style is not loaded');

  var LID := StrToNSStr(AId);
  var LOldLayer := LStyle.layerWithIdentifier(LID);
  if LOldLayer <> nil then LStyle.removeLayer(LOldLayer);
  var LOldSource := LStyle.sourceWithIdentifier(LID);
  if LOldSource <> nil then LStyle.removeSource(LOldSource);

  var LOptions: TALMapLibre.TRasterLayerOptions;
  if AOptions <> nil then LOptions := AOptions^
  else LOptions := TALMapLibre.TRasterLayerOptions.Create;

  var LSource: MLNRasterTileSource;
  if AIsTileJSON then LSource := TMLNRasterTileSource.Alloc.initWithIdentifier(LID, TNSURL.Wrap(TNSURL.OCClass.URLWithString(StrToNSStr(ATileURL))), ATileSize)
  else begin
    var LTemplates := TNSMutableArray.wrap(TNSMutableArray.OCClass.arrayWithCapacity(1));
    LTemplates.addObject(NSObjectToID(StrToNSStr(ATileURL)));
    var LSourceOptions := TNSMutableDictionary.Create;
    Try
      LSourceOptions.setObject(
        NSObjectToID(TNSNumber.OCClass.numberWithInt(ATileSize)),
        NSObjectToID(StrToNSStr('MLNTileSourceOptionTileSize')));
      LSource := TMLNRasterTileSource.Alloc.initWithIdentifier(LID, LTemplates, LSourceOptions);
    finally
      LSourceOptions.release;
    end;
  end;
  Try
    LStyle.addSource(LSource);

    var LLayer := TMLNRasterStyleLayer.Alloc.initWithIdentifier(LID, LSource);
    Try

      LLayer.setRasterOpacity(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithDouble(LOptions.FRasterOpacity))));
      LLayer.setRasterHueRotation(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithDouble(LOptions.FRasterHueRotate))));
      LLayer.setMinimumRasterBrightness(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithDouble(LOptions.FRasterBrightnessMin))));
      LLayer.setMaximumRasterBrightness(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithDouble(LOptions.FRasterBrightnessMax))));
      LLayer.setRasterSaturation(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithDouble(LOptions.FRasterSaturation))));
      LLayer.setRasterContrast(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithDouble(LOptions.FRasterContrast))));
      LLayer.setRasterResamplingMode(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(StrToNSStr(LOptions.FRasterResampling))));
      LLayer.setRasterFadeDuration(TNSExpression.OCClass.expressionForConstantValue(NSObjectToID(TNSNumber.OCClass.numberWithDouble(LOptions.FRasterFadeDuration))));
      LLayer.setVisible(LOptions.FVisibility <> 'none');

      LStyle.addLayer(LLayer);

    finally
      LLayer.release;
    end;
  finally
    LSource.release;
  end;
end;

{**********************************************************}
procedure TALIosMapLibreView.RemoveLayer(const AId: String);
begin
  var AIndex := FSymbolLayerIds.IndexOf(AId);
  If AIndex >= 0 then FSymbolLayerIds.delete(AIndex);
  var LStyle := View.style;
  if LStyle = nil then Exit;
  var LIDNSStr:= StrToNSStr(AId);
  var LLayer := LStyle.layerWithIdentifier(LIDNSStr);
  if LLayer <> nil then LStyle.removeLayer(LLayer);
  var LSource := LStyle.sourceWithIdentifier(LIDNSStr);
  if LSource <> nil then LStyle.removeSource(LSource);
end;

{******************************************************}
function TALIosMapLibreView.GetViewportBounds: TALRectD;
begin
  var LBounds := View.visibleCoordinateBounds;
  Result := TALRectD.Create(
              LBounds.sw.longitude,
              LBounds.sw.latitude,
              LBounds.ne.longitude,
              LBounds.ne.latitude);
end;

{*************************************************************************}
function TALIosMapLibreView.GetCameraPosition: TALMapLibre.TCameraPosition;
begin
  var LCamera := View.camera;
  if LCamera = nil then raise Exception.Create('MapLibre camera is not initialized');
  Result.bearing := LCamera.heading;
  var LPadding := View.cameraEdgeInsets;
  Result.padding := TALRectD.Create(
                      LPadding.left,
                      LPadding.top,
                      LPadding.right,
                      LPadding.bottom);
  Result.roll := LCamera.roll;
  var LTarget := LCamera.centerCoordinate;
  Result.target := TALLatLng.Create(
                     LTarget.latitude,
                     LTarget.longitude);
  Result.tilt := LCamera.pitch;
  Result.zoom := View.zoomLevel;
end;

{**************************************}
procedure TALIosMapLibreView.ApplyStyle;
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosMapLibreView.ApplyStyle');
  {$ENDIF}
  if Control.StyleJSON <> '' then view.SetStyleJSON(StrToNSStr(Control.StyleJSON))
  else if Control.StyleURL <> '' then view.SetStyleUrl(StrToNSUrl(Control.StyleURL));
end;

{****************************************}
procedure TALIosMapLibreView.ApplyCompass;
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosMapLibreView.ApplyCompass');
  {$ENDIF}
  var LCompass := Control.Compass;
  var LCompassView := View.compassView;
  View.setShowsCompassView(LCompass.Enabled);
  if not LCompass.Enabled then LCompassView.setCompassVisibility(MLNOrnamentVisibilityHidden)
  else if LCompass.FadeFacingNorth then LCompassView.setCompassVisibility(MLNOrnamentVisibilityAdaptive)
  else LCompassView.setCompassVisibility(MLNOrnamentVisibilityVisible);
  // On iOS the margins are the offset from the corner where the
  // compass is positioned, so only two of the four margins apply.
  var LMargins: CGPoint;
  case LCompass.Gravity of
    TALMapLibre.TOrnamentGravity.TopLeft: begin
      View.setCompassViewPosition(MLNOrnamentPositionTopLeft);
      LMargins.x := LCompass.Margins.Left;
      LMargins.y := LCompass.Margins.Top;
    end;
    TALMapLibre.TOrnamentGravity.TopRight: begin
      View.setCompassViewPosition(MLNOrnamentPositionTopRight);
      LMargins.x := LCompass.Margins.Right;
      LMargins.y := LCompass.Margins.Top;
    end;
    TALMapLibre.TOrnamentGravity.BottomLeft: begin
      View.setCompassViewPosition(MLNOrnamentPositionBottomLeft);
      LMargins.x := LCompass.Margins.Left;
      LMargins.y := LCompass.Margins.Bottom;
    end;
    TALMapLibre.TOrnamentGravity.BottomRight: begin
      View.setCompassViewPosition(MLNOrnamentPositionBottomRight);
      LMargins.x := LCompass.Margins.Right;
      LMargins.y := LCompass.Margins.Bottom;
    end;
    else raise Exception.Create('Error 9A4C2E7B-5D1F-4B83-A6E0-2F8B7C3D1A95');
  end;
  View.setCompassViewMargins(LMargins);
  if LCompass.ResourceName <> '' then begin
    var LScreenScale := ALGetScreenScale;
    var LCGImageRef := ALCreateCGImageRefFromResource(
                         LCompass.ResourceName, // const AResourceName: String;
                         nil, // const AResourceStream: TStream;
                         '', // const AMaskResourceName: String;
                         LScreenScale, // const AScale: Single;
                         TRectF.Empty, // const ASrcRect: TRectF; // In source image coordinates (real pixels). Use TRectF.Empty to use the full image
                         LCompass.Width, LCompass.Height, // const W, H: single;
                         True, // const AApplyMetadataOrientation: Boolean;
                         TALImageWrapMode.fit, // const AWrapMode: TALImageWrapMode;
                         TpointF.Create(0.5,0.5), // const ACropCenter: TpointF;
                         TalphaColors.null, // const ATintColor: TalphaColor;
                         0, // const ABlurRadius: single;
                         0, // const AXRadius: Single;
                         0); // const AYRadius: Single): CGImageRef;
    try
      LCompassView.setImage(TUIImage.OCClass.imageWithCGImage(LCGImageRef, LScreenScale, UIImageOrientationUp));
    finally
      CGImageRelease(LCGImageRef);
    end;
    // MLNMapView sizes the compass from its bounds the first time it
    // installs the compass constraints, then keeps the width/height
    // constraints it created. Update both so the new image size is used.
    LCompassView.sizeToFit;
    var LImageSize := LCompassView.image.size;
    var LConstraints := LCompassView.constraints;
    for var I := 0 to LConstraints.count - 1 do begin
      var LConstraint := TNSLayoutConstraint.Wrap(LConstraints.objectAtIndex(I));
      if LConstraint.secondItem <> nil then continue;
      if LConstraint.firstAttribute = NSLayoutAttributeWidth then LConstraint.setConstant(LImageSize.width)
      else if LConstraint.firstAttribute = NSLayoutAttributeHeight then LConstraint.setConstant(LImageSize.height);
    end;
  end;
end;

{*******************************************************************}
procedure TALIosMapLibreView.ShowLoadingSpinner(const AView: UIView);
begin
  if SameObjects(AView, FLoadingSpinnerView) then Exit;
  HideLoadingSpinner;
  AView.retain;
  AView.removeFromSuperview;

  if (AView.bounds.size.width <= 0) or (AView.bounds.size.height <= 0) then
    AView.sizeToFit;

  AView.setUserInteractionEnabled(False);

  View.addSubview(AView);
  FLoadingSpinnerView := AView;
  var LBounds := View.bounds;
  var LCenter: CGPoint;
  LCenter.x := LBounds.origin.x + (LBounds.size.width / 2);
  LCenter.y := LBounds.origin.y + (LBounds.size.height / 2);
  FLoadingSpinnerView.setCenter(LCenter);
  FLoadingSpinnerView.setAutoresizingMask(
    UIViewAutoresizingFlexibleLeftMargin or
    UIViewAutoresizingFlexibleRightMargin or
    UIViewAutoresizingFlexibleTopMargin or
    UIViewAutoresizingFlexibleBottomMargin);
end;

{**************************************************************************************************************************************************}
procedure TALIosMapLibreView.ShowLoadingSpinner(const AColor: TAlphaColor = Talphacolors.null; const AWidth: Single = 0; const AHeight: Single = 0);
begin
  var LActivityIndicatorView := TUIActivityIndicatorView.Wrap(
                                  TUIActivityIndicatorView.Alloc.initWithActivityIndicatorStyle(
                                    UIActivityIndicatorViewStyleLarge));
  try
    if AColor <> TAlphaColors.Null then
      LActivityIndicatorView.setColor(AlphaColorToUIColor(AColor));
    LActivityIndicatorView.sizeToFit;
    if (CompareValue(AWidth, 0, TEpsilon.position) > 0) and
       (CompareValue(AHeight, 0, TEpsilon.position) > 0) then begin
      // UIActivityIndicatorView always draws at its intrinsic size, so
      // scale it to the requested size. The transform does not change
      // the bounds or the center, so the spinner stays centered.
      var LBounds := LActivityIndicatorView.bounds;
      if (LBounds.size.width > 0) and (LBounds.size.height > 0) then
        LActivityIndicatorView.setTransform(
          CGAffineTransformMakeScale(
            AWidth / LBounds.size.width, // sx: CGFloat;
            AHeight / LBounds.size.height)); // sy: CGFloat
    end;
    ShowLoadingSpinner(LActivityIndicatorView);
    LActivityIndicatorView.startAnimating;
  finally
    LActivityIndicatorView.release;
  end;
end;

{**********************************************}
procedure TALIosMapLibreView.HideLoadingSpinner;
begin
  if FLoadingSpinnerView = nil then exit;
  FLoadingSpinnerView.removeFromSuperview;
  FLoadingSpinnerView.release;
  FLoadingSpinnerView := nil;
end;

{$endif}
{$ENDREGION}

{$REGION ' ALMACOS'}
{$IF defined(ALMACOS)}

{**************************************************}
function TALMacMapLibreView.GetControl: TALMapLibre;
begin
  Result := TALMapLibre(inherited Control);
end;

{**************************************************************************************************************}
procedure TALMacMapLibreView.SetCenterCoordinate(const ALatitude, ALongitude: Double; const AZoomLevel: double);
begin
  // Dummy
end;

{************************************}
procedure TALMacMapLibreView.SetImage(
            const AResourceName: String;
            const AStream: TStream;
            const AScale: Single;
            const W, H: single;
            const   AName: String);
begin
  // Dummy
end;

{************************************************************}
procedure TALMacMapLibreView.RemoveImage(const AName: String);
begin
  // Dummy
end;

{********************************************************************************************************************************************}
procedure TALMacMapLibreView.SetSymbolLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PSymbolLayerOptions = nil);
begin
  // Dummy
end;

{****************************************************************************************************************************************}
procedure TALMacMapLibreView.SetLineLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PLineLayerOptions = nil);
begin
  // Dummy
end;

{********************************************************************************************************************************************}
procedure TALMacMapLibreView.SetCircleLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PCircleLayerOptions = nil);
begin
  // Dummy
end;

{****************************************************************************************************************************************}
procedure TALMacMapLibreView.SetFillLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillLayerOptions = nil);
begin
  // Dummy
end;

{**********************************************************************************************************************************************************}
procedure TALMacMapLibreView.SetFillExtrusionLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillExtrusionLayerOptions = nil);
begin
  // Dummy
end;

{**********************************************************************************************************************************************}
procedure TALMacMapLibreView.SetHeatmapLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PHeatmapLayerOptions = nil);
begin
  // Dummy
end;

{****************************************************************************************************************************************************************************************************************}
procedure TALMacMapLibreView.SetRasterLayer(const AId: String; const ATileURL: String; const AIsTileJSON: Boolean = False; const ATileSize: Integer = 256; const AOptions: TALMapLibre.PRasterLayerOptions = nil);
begin
  // Dummy
end;

{**********************************************************}
procedure TALMacMapLibreView.RemoveLayer(const AId: String);
begin
  // Dummy
end;

{******************************************************}
function TALMacMapLibreView.GetViewportBounds: TALRectD;
begin
  Result := TALRectD.Empty;
end;

{*************************************************************************}
function TALMacMapLibreView.GetCameraPosition: TALMapLibre.TCameraPosition;
begin
  Result := TALMapLibre.TCameraPosition.Create(
              0, // const ABearing: Double;
              TALRectD.Empty, // APadding: TALRectD;
              0, // const ARoll: Double;
              TALLatLng.Create(ALNullLatLng, ALNullLatLng), // const ATarget: TALLatLng;
              0, // const ATilt: Double;
              0); // const AZoom: Double
end;

{**************************************}
procedure TALMacMapLibreView.ApplyStyle;
begin
  // Dummy
end;

{****************************************}
procedure TALMacMapLibreView.ApplyCompass;
begin
  // Dummy
end;

{*******************************************************************}
procedure TALMacMapLibreView.ShowLoadingSpinner(const AHTML: String);
begin
  // Dummy
end;

{**************************************************************************************************************************************************}
procedure TALMacMapLibreView.ShowLoadingSpinner(const AColor: TAlphaColor = Talphacolors.null; const AWidth: Single = 0; const AHeight: Single = 0);
begin
  // Dummy
end;

{**********************************************}
procedure TALMacMapLibreView.HideLoadingSpinner;
begin
  // Dummy
end;

{$endif}
{$ENDREGION}

{$REGION ' MSWINDOWS'}
{$IF defined(MSWINDOWS)}

{************************************}
constructor TALWinMapLibreView.Create;
begin
  inherited;
  FThreadQueueCleaner := TALThreadQueueCleaner.Create;
  FMapReady := False;
  FApplyStyleWhenMapReady := False;
  LoadData(
    '', // ABaseUrl
    GetHtml, // AData
    'text/html', // AMimeType
    'UTF-8'); // AEncoding
end;

{************************************}
destructor TALWinMapLibreView.Destroy;
begin
  ALFreeAndNil(FThreadQueueCleaner);
  inherited;
end;

{**************************************************}
function TALWinMapLibreView.GetControl: TALMapLibre;
begin
  Result := TALMapLibre(inherited Control);
end;

{****************************************************************}
procedure TALWinMapLibreView.DoWebMessage(const AMessage: string);
begin
  // WebView2 callbacks are not reentrant: a new callback (such as the
  // ExecuteScript completion handler) cannot be invoked while another
  // WebView2 callback is still executing.
  //
  // GetViewportBounds synchronously waits for ExecuteScript to complete
  // by calling Application.ProcessMessages. However, if called directly
  // from an event triggered by DoWebMessage, the completion callback
  // cannot execute until DoWebMessage returns, causing an infinite wait.
  //
  // ForceQueue defers event handling until after DoWebMessage has returned,
  // allowing Application.ProcessMessages to dispatch the ExecuteScript
  // completion callback and GetViewportBounds to finish.
  TThread.ForceQueue(
    FThreadQueueCleaner,
    procedure
    begin
      {$IF defined(DEBUG)}
      ALLog('TALWinMapLibreView.DoWebMessage', AMessage);
      {$ENDIF}
      if (AMessage <> '') and (AMessage[1] = '{') then begin

        var LJSON := TALJsonDocumentW.CreateFromJSONString(AMessage);
        try

          var LType := LJSON.GetChildNode('type').Text;

          if (LType = 'map-ready') then begin
            FMapReady := True;
            if FApplyStyleWhenMapReady then begin
              FApplyStyleWhenMapReady := False;
              ApplyStyle;
            end;
            ApplyCompass;
            if Assigned(Control.FOnMapReady) then
              Control.FOnMapReady(Control);
          end
          else if LType = 'style-loaded' then begin
            if Assigned(Control.FOnStyleLoaded) then
              Control.FOnStyleLoaded(Control);
          end
          else if LType = 'camera-will-change' then begin
            if Assigned(Control.FOnCameraWillChange) then
              Control.FOnCameraWillChange(Control);
          end
          else if LType = 'camera-is-changing' then begin
            if Assigned(Control.FOnCameraIsChanging) then
              Control.FOnCameraIsChanging(Control);
          end
          else if LType = 'camera-did-change' then begin
            if Assigned(Control.FOnCameraDidChange) then
              Control.FOnCameraDidChange(Control);
          end
          else if LType = 'map-click' then begin
            if Assigned(Control.FOnMapClick) then
              Control.FOnMapClick(
                Control,
                LJSON.GetChildNode('latitude').Float,
                LJSON.GetChildNode('longitude').Float);
          end
          else if LType = 'symbol-click' then begin
            if Assigned(Control.FOnSymbolClick) then
              Control.FOnSymbolClick(
                Control,
                LJSON.GetChildNode('layerId').Text,
                LJSON.GetChildNode('id').Text);
          end;

        finally
          ALFreeAndNil(LJSON);
        end;

      end;

    end);
  inherited;
end;

{******************************************}
function TALWinMapLibreView.GetHtml: String;
begin
  var LStyle: string;
  if Control.StyleJSON <> '' then LStyle := ',style: ' + Control.StyleJSON
  else if Control.StyleURL <> '' then LStyle := ',style: "' + ALJAvascriptEncode(Control.StyleURL) + '"'
  else LStyle := '';
  result := '''
            <!DOCTYPE html>
            <html lang="en">
            <head>
              <meta charset="UTF-8">
              <meta name="viewport" content="width=device-width, initial-scale=1.0">
              <link href="https://unpkg.com/maplibre-gl@5/dist/maplibre-gl.css" rel="stylesheet">
              <script src="https://unpkg.com/maplibre-gl@5/dist/maplibre-gl.js"></script>
              <style>
                html, body {
                  margin: 0;
                  padding: 0;
                  width: 100%;
                  height: 100%;
                  overflow: hidden;
                }
                #map {
                  position: absolute;
                  inset: 0;
                }
                #loadingSpinner {
                  position: absolute;
                  inset: 0;
                  display: flex;
                  align-items: center;
                  justify-content: center;
                  z-index: 10;
                  pointer-events: none;
                }
                #loadingSpinner * {
                  pointer-events: none !important;
                }
              </style>
            </head>
            <body>
              <div id="map"></div>
              <script>
                window.map = new maplibregl.Map({
                  container: 'map'
            ''' +
                LStyle +
            '''
                });

                window.map.on('style.load', function() {
                  window.symbolLayerIds = [];
                  window.chrome.webview.postMessage(JSON.stringify({type: 'style-loaded'}));
                });

                window.map.on('movestart', function() {
                  window.chrome.webview.postMessage(JSON.stringify({type: 'camera-will-change'}));
                });

                window.map.on('move', function() {
                  window.chrome.webview.postMessage(JSON.stringify({type: 'camera-is-changing'}));
                });

                window.map.on('moveend', function() {
                  window.chrome.webview.postMessage(JSON.stringify({type: 'camera-did-change'}));
                });

                window.symbolLayerIds = [];

                window.map.on('click', function(e) {

                  // Notify Delphi about the map click.
                  window.chrome.webview.postMessage(JSON.stringify({
                    type: 'map-click', latitude: e.lngLat.lat, longitude: e.lngLat.lng
                  }));

                  // Query only symbol layers registered through SetSymbolLayer.
                  const ids = window.symbolLayerIds || [];
                  for (let i = ids.length - 1; i >= 0; i--) {
                    const layerId = ids[i];
                    if (!window.map.getLayer(layerId)) continue;

                    const features = window.map.queryRenderedFeatures(
                      e.point,
                      { layers: [layerId] }
                    );

                    const feature = features.find(f =>
                      f.properties?.id !== undefined && f.properties?.id !== null
                    );
                    if (!feature) continue;

                    window.chrome.webview.postMessage(JSON.stringify({
                      type: 'symbol-click', layerId: layerId, id: String(feature.properties.id)
                    }));
                    return;
                  }

                });

                window.chrome.webview.postMessage(JSON.stringify({type: 'map-ready'}));

              </script>
            </body>
            </html>
            ''';
end;

{**************************************************************************************************************}
procedure TALWinMapLibreView.SetCenterCoordinate(const ALatitude, ALongitude: Double; const AZoomLevel: double);
begin
  if not FMapReady then raise Exception.Create('MapLibre is not initialized');
  EvaluateJavaScript(
    'window.map.jumpTo({' +
      'center: [' + ALFloatToStrW(ALongitude) + ',' + ALFloatToStrW(ALatitude) + '],' +
      'zoom: ' + ALFloatToStrW(AZoomLevel) +
    '});');
end;

{************************************}
procedure TALWinMapLibreView.SetImage(
            const AResourceName: String;
            const AStream: TStream;
            const AScale: Single;
            const W, H: single;
            const   AName: String);
begin
  if not FMapReady then raise Exception.Create('MapLibre is not initialized');
  var LBitmap := ALCreateBitmapFromResource(
                   AResourceName, // const AResourceName: String;
                   AStream, // const AResourceStream: TStream;
                   '', // const AMaskResourceName: String;
                   AScale, // const AScale: Single;
                   TRectF.Empty, // const ASrcRect: TRectF; // In source image coordinates (real pixels). Use TRectF.Empty to use the full image
                   W, H, // const W, H: single;
                   False, // const AApplyMetadataOrientation: Boolean;
                   TALImageWrapMode.fit, // const AWrapMode: TALImageWrapMode;
                   TpointF.Create(0.5,0.5), // const ACropCenter: TpointF;
                   TalphaColors.null, // const ATintColor: TalphaColor;
                   0, // const ABlurRadius: single;
                   0, // const AXRadius: Single;
                   0); // const AYRadius: Single): CGImageRef;
  try

    var LBase64 := ALBitmapToBase64(LBitmap, 'PNG', 100);

    EvaluateJavaScript(
      '''
      (function() {
       const name = "
      '''
      + ALJavaScriptEncode(AName) +
      '''
      ";
       const image = new Image();

       image.onload = function() {
         const map = window.map;

         if (!map) return;

         if (map.hasImage(name))
           map.removeImage(name);

         map.addImage(name, image, {
           pixelRatio:
      '''
      + ALFloatToStrW(AScale)+
      '''
         });
       };
       image.src = "data:image/png;base64,
      ''' +
      LBase64 +
      '''
      ";
      })();
      '''
      );

  finally
    ALFreeAndNilBitmap(LBitmap);
  end;
end;

{************************************************************}
procedure TALWinMapLibreView.RemoveImage(const AName: String);
begin
  if not FMapReady then raise Exception.Create('MapLibre is not initialized');
  EvaluateJavaScript('if (window.map.hasImage("'+ ALJavaScriptEncode(AName) + '")) {window.map.removeImage("' + ALJavaScriptEncode(AName) + '");}');
end;

{************************************************************************}
// Adds (or updates the data of) a GeoJSON source and adds the layer if it
// does not exist yet. Like on Android/iOS, ALayout and APaint (and so the
// options and the data keys) are only applied when the layer is created.
procedure TALWinMapLibreView.SetGeoJsonLayer(
            const AId: String;
            const AType: String;
            const AGeoJSON: String;
            const ALayout: String;
            const APaint: String;
            const AIsSymbolLayer: Boolean);
begin
  if not FMapReady then raise Exception.Create('MapLibre is not initialized');
  var LScript :=
    '(function() {' +
      'const map = window.map;' +
      'const id = ' + ALJsonEncodeTextW(AId) + ';' +
      'const data = JSON.parse(' + ALJsonEncodeTextW(AGeoJSON) + ');' +
      'const source = map.getSource(id);' +
      'if (!source) map.addSource(id, {type: "geojson", data: data});' +
      'else source.setData(data);' +
      'if (!map.getLayer(id)) {' +
        'map.addLayer({' +
          'id: id,' +
          'type: ' + ALJsonEncodeTextW(AType) + ',' +
          'source: id,' +
          'layout: {' + ALayout + '},' +
          'paint: {' + APaint + '}' +
        '});' +
      '}';
  if AIsSymbolLayer then
    LScript := LScript +
      'if (!window.symbolLayerIds.includes(id)) window.symbolLayerIds.push(id);';
  LScript := LScript + '})();';
  EvaluateJavaScript(LScript);
end;

{********************************************************************************************************************************************}
procedure TALWinMapLibreView.SetSymbolLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PSymbolLayerOptions = nil);
begin
  var LOptions: TALMapLibre.TSymbolLayerOptions;
  if AOptions <> nil then LOptions := AOptions^
  else LOptions := TALMapLibre.TSymbolLayerOptions.Create;

  var LLayout := '';
  var LPaint := '';
  for var LDataKey in ALGetLayerDataKeys(AGeoJSON, ALSymbolLayerDataKeys) do begin
    var LName := ALLayerDataKeyNames[LDataKey].Substring(1).Replace('_', '-');
    if LDataKey = TALLayerDataKey.Text then LName := 'text-field';
    var LExpr := '["get",' + ALJsonEncodeTextW(ALLayerDataKeyNames[LDataKey]) + ']';
    if LDataKey in [TALLayerDataKey.IconImage, TALLayerDataKey.IconSize, TALLayerDataKey.IconAnchor, TALLayerDataKey.IconOffset, TALLayerDataKey.IconRotate, TALLayerDataKey.Text, TALLayerDataKey.TextSize, TALLayerDataKey.TextLetterSpacing, TALLayerDataKey.TextJustify, TALLayerDataKey.TextAnchor, TALLayerDataKey.TextOffset, TALLayerDataKey.TextMaxWidth, TALLayerDataKey.TextRotate, TALLayerDataKey.TextRadialOffset, TALLayerDataKey.TextTransform, TALLayerDataKey.SymbolSortKey] then
      LLayout := LLayout + '"' + LName + '":' + LExpr + ','
    else
      LPaint := LPaint + '"' + LName + '":' + LExpr + ',';
  end;
  LLayout := LLayout + '"symbol-placement":' + ALJsonEncodeTextW(LOptions.FSymbolPlacement) + ',';
  LLayout := LLayout + '"symbol-spacing":' + ALJsonEncodeFloatW(LOptions.FSymbolSpacing, TALJSONInterchangeFormat.icfStandardJson) + ',';
  LLayout := LLayout + '"symbol-avoid-edges":' + ALJsonEncodeBooleanW(LOptions.FSymbolAvoidEdges) + ',';
  LLayout := LLayout + '"symbol-z-order":' + ALJsonEncodeTextW(LOptions.FSymbolZOrder) + ',';
  LLayout := LLayout + '"icon-allow-overlap":' + ALJsonEncodeBooleanW(LOptions.FIconAllowOverlap) + ',';
  LLayout := LLayout + '"icon-ignore-placement":' + ALJsonEncodeBooleanW(LOptions.FIconIgnorePlacement) + ',';
  LLayout := LLayout + '"icon-optional":' + ALJsonEncodeBooleanW(LOptions.FIconOptional) + ',';
  LLayout := LLayout + '"icon-rotation-alignment":' + ALJsonEncodeTextW(LOptions.FIconRotationAlignment) + ',';
  LLayout := LLayout + '"icon-pitch-alignment":' + ALJsonEncodeTextW(LOptions.FIconPitchAlignment) + ',';
  LLayout := LLayout + '"icon-padding":' + ALJsonEncodeFloatW(LOptions.FIconPadding, TALJSONInterchangeFormat.icfStandardJson) + ',';
  LLayout := LLayout + '"icon-keep-upright":' + ALJsonEncodeBooleanW(LOptions.FIconKeepUpright) + ',';
  LLayout := LLayout + '"icon-text-fit":' + ALJsonEncodeTextW(LOptions.FIconTextFit) + ',';
  LLayout := LLayout + '"icon-text-fit-padding":' + '[' + ALJsonEncodeFloatW(LOptions.FIconTextFitPadding.Top, TALJSONInterchangeFormat.icfStandardJson) + ',' + ALJsonEncodeFloatW(LOptions.FIconTextFitPadding.Right, TALJSONInterchangeFormat.icfStandardJson) + ',' + ALJsonEncodeFloatW(LOptions.FIconTextFitPadding.Bottom, TALJSONInterchangeFormat.icfStandardJson) + ',' + ALJsonEncodeFloatW(LOptions.FIconTextFitPadding.Left, TALJSONInterchangeFormat.icfStandardJson) + '],';
  LLayout := LLayout + '"text-allow-overlap":' + ALJsonEncodeBooleanW(LOptions.FTextAllowOverlap) + ',';
  LLayout := LLayout + '"text-ignore-placement":' + ALJsonEncodeBooleanW(LOptions.FTextIgnorePlacement) + ',';
  LLayout := LLayout + '"text-optional":' + ALJsonEncodeBooleanW(LOptions.FTextOptional) + ',';
  if Length(LOptions.FTextFont) > 0 then
    begin
      LLayout := LLayout + '"text-font":[';
      for var LValue in LOptions.FTextFont do
        LLayout := LLayout + ALJsonEncodeTextW(LValue) + ',';
      Delete(LLayout, Length(LLayout), 1);
      LLayout := LLayout + '],';
    end;
  LLayout := LLayout + '"text-rotation-alignment":' + ALJsonEncodeTextW(LOptions.FTextRotationAlignment) + ',';
  LLayout := LLayout + '"text-pitch-alignment":' + ALJsonEncodeTextW(LOptions.FTextPitchAlignment) + ',';
  LLayout := LLayout + '"text-padding":' + ALJsonEncodeFloatW(LOptions.FTextPadding, TALJSONInterchangeFormat.icfStandardJson) + ',';
  LLayout := LLayout + '"text-keep-upright":' + ALJsonEncodeBooleanW(LOptions.FTextKeepUpright) + ',';
  LLayout := LLayout + '"text-max-angle":' + ALJsonEncodeFloatW(LOptions.FTextMaxAngle, TALJSONInterchangeFormat.icfStandardJson) + ',';
  LLayout := LLayout + '"text-line-height":' + ALJsonEncodeFloatW(LOptions.FTextLineHeight, TALJSONInterchangeFormat.icfStandardJson) + ',';
  if Length(LOptions.FTextWritingMode) > 0 then
    begin
      LLayout := LLayout + '"text-writing-mode":[';
      for var LValue in LOptions.FTextWritingMode do
        LLayout := LLayout + ALJsonEncodeTextW(LValue) + ',';
      Delete(LLayout, Length(LLayout), 1);
      LLayout := LLayout + '],';
    end;
  if Length(LOptions.FTextVariableAnchor) > 0 then
    begin
      LLayout := LLayout + '"text-variable-anchor":[';
      for var LValue in LOptions.FTextVariableAnchor do
        LLayout := LLayout + ALJsonEncodeTextW(LValue) + ',';
      Delete(LLayout, Length(LLayout), 1);
      LLayout := LLayout + '],';
    end;
  LLayout := LLayout + '"visibility":' + ALJsonEncodeTextW(LOptions.FVisibility) + ',';
  //--
  LPaint := LPaint + '"icon-translate":' + '[' + ALJsonEncodeFloatW(LOptions.FIconTranslate.X, TALJSONInterchangeFormat.icfStandardJson) + ',' + ALJsonEncodeFloatW(LOptions.FIconTranslate.Y, TALJSONInterchangeFormat.icfStandardJson) + '],';
  LPaint := LPaint + '"icon-translate-anchor":' + ALJsonEncodeTextW(LOptions.FIconTranslateAnchor) + ',';
  LPaint := LPaint + '"text-translate":' + '[' + ALJsonEncodeFloatW(LOptions.FTextTranslate.X, TALJSONInterchangeFormat.icfStandardJson) + ',' + ALJsonEncodeFloatW(LOptions.FTextTranslate.Y, TALJSONInterchangeFormat.icfStandardJson) + '],';
  LPaint := LPaint + '"text-translate-anchor":' + ALJsonEncodeTextW(LOptions.FTextTranslateAnchor) + ',';

  if LLayout <> '' then Delete(LLayout, Length(LLayout), 1);
  if LPaint <> '' then Delete(LPaint, Length(LPaint), 1);

  SetGeoJsonLayer(AId, 'symbol', AGeoJSON, LLayout, LPaint, True{AIsSymbolLayer});
end;

{****************************************************************************************************************************************}
procedure TALWinMapLibreView.SetLineLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PLineLayerOptions = nil);
begin
  var LOptions: TALMapLibre.TLineLayerOptions;
  if AOptions <> nil then LOptions := AOptions^
  else LOptions := TALMapLibre.TLineLayerOptions.Create;

  var LLayout := '';
  var LPaint := '';
  for var LDataKey in ALGetLayerDataKeys(AGeoJSON, ALLineLayerDataKeys) do begin
    var LName := ALLayerDataKeyNames[LDataKey].Substring(1).Replace('_', '-');
    var LExpr := '["get",' + ALJsonEncodeTextW(ALLayerDataKeyNames[LDataKey]) + ']';
    if LDataKey in [TALLayerDataKey.LineSortKey] then
      LLayout := LLayout + '"' + LName + '":' + LExpr + ','
    else
      LPaint := LPaint + '"' + LName + '":' + LExpr + ',';
  end;
  LLayout := LLayout + '"line-cap":' + ALJsonEncodeTextW(LOptions.FLineCap) + ',';
  LLayout := LLayout + '"line-join":' + ALJsonEncodeTextW(LOptions.FLineJoin) + ',';
  LLayout := LLayout + '"line-miter-limit":' + ALJsonEncodeFloatW(LOptions.FLineMiterLimit, TALJSONInterchangeFormat.icfStandardJson) + ',';
  LLayout := LLayout + '"line-round-limit":' + ALJsonEncodeFloatW(LOptions.FLineRoundLimit, TALJSONInterchangeFormat.icfStandardJson) + ',';
  LLayout := LLayout + '"visibility":' + ALJsonEncodeTextW(LOptions.FVisibility) + ',';
  //--
  if Length(LOptions.FLineDasharray) > 0 then
    begin
      LPaint := LPaint + '"line-dasharray":[';
      for var LValue in LOptions.FLineDasharray do
        LPaint := LPaint + ALJsonEncodeFloatW(LValue, TALJSONInterchangeFormat.icfStandardJson) + ',';
      Delete(LPaint, Length(LPaint), 1);
      LPaint := LPaint + '],';
    end;
  LPaint := LPaint + '"line-translate":' + '[' + ALJsonEncodeFloatW(LOptions.FLineTranslate.X, TALJSONInterchangeFormat.icfStandardJson) + ',' + ALJsonEncodeFloatW(LOptions.FLineTranslate.Y, TALJSONInterchangeFormat.icfStandardJson) + '],';
  LPaint := LPaint + '"line-translate-anchor":' + ALJsonEncodeTextW(LOptions.FLineTranslateAnchor) + ',';

  if LLayout <> '' then Delete(LLayout, Length(LLayout), 1);
  if LPaint <> '' then Delete(LPaint, Length(LPaint), 1);

  SetGeoJsonLayer(AId, 'line', AGeoJSON, LLayout, LPaint, False{AIsSymbolLayer});
end;

{********************************************************************************************************************************************}
procedure TALWinMapLibreView.SetCircleLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PCircleLayerOptions = nil);
begin
  var LOptions: TALMapLibre.TCircleLayerOptions;
  if AOptions <> nil then LOptions := AOptions^
  else LOptions := TALMapLibre.TCircleLayerOptions.Create;

  var LLayout := '';
  var LPaint := '';
  for var LDataKey in ALGetLayerDataKeys(AGeoJSON, ALCircleLayerDataKeys) do begin
    var LName := ALLayerDataKeyNames[LDataKey].Substring(1).Replace('_', '-');
    var LExpr := '["get",' + ALJsonEncodeTextW(ALLayerDataKeyNames[LDataKey]) + ']';
    if LDataKey in [TALLayerDataKey.CircleSortKey] then
      LLayout := LLayout + '"' + LName + '":' + LExpr + ','
    else
      LPaint := LPaint + '"' + LName + '":' + LExpr + ',';
  end;
  LLayout := LLayout + '"visibility":' + ALJsonEncodeTextW(LOptions.FVisibility) + ',';
  //--
  LPaint := LPaint + '"circle-pitch-alignment":' + ALJsonEncodeTextW(LOptions.FCirclePitchAlignment) + ',';
  LPaint := LPaint + '"circle-pitch-scale":' + ALJsonEncodeTextW(LOptions.FCirclePitchScale) + ',';
  LPaint := LPaint + '"circle-translate":' + '[' + ALJsonEncodeFloatW(LOptions.FCircleTranslate.X, TALJSONInterchangeFormat.icfStandardJson) + ',' + ALJsonEncodeFloatW(LOptions.FCircleTranslate.Y, TALJSONInterchangeFormat.icfStandardJson) + '],';
  LPaint := LPaint + '"circle-translate-anchor":' + ALJsonEncodeTextW(LOptions.FCircleTranslateAnchor) + ',';

  if LLayout <> '' then Delete(LLayout, Length(LLayout), 1);
  if LPaint <> '' then Delete(LPaint, Length(LPaint), 1);

  SetGeoJsonLayer(AId, 'circle', AGeoJSON, LLayout, LPaint, False{AIsSymbolLayer});
end;

{****************************************************************************************************************************************}
procedure TALWinMapLibreView.SetFillLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillLayerOptions = nil);
begin
  var LOptions: TALMapLibre.TFillLayerOptions;
  if AOptions <> nil then LOptions := AOptions^
  else LOptions := TALMapLibre.TFillLayerOptions.Create;

  var LLayout := '';
  var LPaint := '';
  for var LDataKey in ALGetLayerDataKeys(AGeoJSON, ALFillLayerDataKeys) do begin
    var LName := ALLayerDataKeyNames[LDataKey].Substring(1).Replace('_', '-');
    var LExpr := '["get",' + ALJsonEncodeTextW(ALLayerDataKeyNames[LDataKey]) + ']';
    if LDataKey in [TALLayerDataKey.FillSortKey] then
      LLayout := LLayout + '"' + LName + '":' + LExpr + ','
    else
      LPaint := LPaint + '"' + LName + '":' + LExpr + ',';
  end;
  LLayout := LLayout + '"visibility":' + ALJsonEncodeTextW(LOptions.FVisibility) + ',';
  //--
  LPaint := LPaint + '"fill-antialias":' + ALJsonEncodeBooleanW(LOptions.FFillAntialias) + ',';
  LPaint := LPaint + '"fill-translate":' + '[' + ALJsonEncodeFloatW(LOptions.FFillTranslate.X, TALJSONInterchangeFormat.icfStandardJson) + ',' + ALJsonEncodeFloatW(LOptions.FFillTranslate.Y, TALJSONInterchangeFormat.icfStandardJson) + '],';
  LPaint := LPaint + '"fill-translate-anchor":' + ALJsonEncodeTextW(LOptions.FFillTranslateAnchor) + ',';
  if not LOptions.FFillPattern.IsEmpty then
    LPaint := LPaint + '"fill-pattern":' + ALJsonEncodeTextW(LOptions.FFillPattern) + ',';

  if LLayout <> '' then Delete(LLayout, Length(LLayout), 1);
  if LPaint <> '' then Delete(LPaint, Length(LPaint), 1);

  SetGeoJsonLayer(AId, 'fill', AGeoJSON, LLayout, LPaint, False{AIsSymbolLayer});
end;

{**********************************************************************************************************************************************************}
procedure TALWinMapLibreView.SetFillExtrusionLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PFillExtrusionLayerOptions = nil);
begin
  var LOptions: TALMapLibre.TFillExtrusionLayerOptions;
  if AOptions <> nil then LOptions := AOptions^
  else LOptions := TALMapLibre.TFillExtrusionLayerOptions.Create;

  var LLayout := '';
  var LPaint := '';
  for var LDataKey in ALGetLayerDataKeys(AGeoJSON, ALFillExtrusionLayerDataKeys) do begin
    var LName := ALLayerDataKeyNames[LDataKey].Substring(1).Replace('_', '-');
    var LExpr := '["get",' + ALJsonEncodeTextW(ALLayerDataKeyNames[LDataKey]) + ']';
    LPaint := LPaint + '"' + LName + '":' + LExpr + ',';
  end;
  LLayout := LLayout + '"visibility":' + ALJsonEncodeTextW(LOptions.FVisibility) + ',';
  //--
  LPaint := LPaint + '"fill-extrusion-opacity":' + ALJsonEncodeFloatW(LOptions.FFillExtrusionOpacity, TALJSONInterchangeFormat.icfStandardJson) + ',';
  LPaint := LPaint + '"fill-extrusion-vertical-gradient":' + ALJsonEncodeBooleanW(LOptions.FFillExtrusionVerticalGradient) + ',';
  LPaint := LPaint + '"fill-extrusion-translate":' + '[' + ALJsonEncodeFloatW(LOptions.FFillExtrusionTranslate.X, TALJSONInterchangeFormat.icfStandardJson) + ',' + ALJsonEncodeFloatW(LOptions.FFillExtrusionTranslate.Y, TALJSONInterchangeFormat.icfStandardJson) + '],';
  LPaint := LPaint + '"fill-extrusion-translate-anchor":' + ALJsonEncodeTextW(LOptions.FFillExtrusionTranslateAnchor) + ',';

  if LLayout <> '' then Delete(LLayout, Length(LLayout), 1);
  if LPaint <> '' then Delete(LPaint, Length(LPaint), 1);

  SetGeoJsonLayer(AId, 'fill-extrusion', AGeoJSON, LLayout, LPaint, False{AIsSymbolLayer});
end;

{**********************************************************************************************************************************************}
procedure TALWinMapLibreView.SetHeatmapLayer(const AId: String; const AGeoJSON: String; const AOptions: TALMapLibre.PHeatmapLayerOptions = nil);
begin
  var LOptions: TALMapLibre.THeatmapLayerOptions;
  if AOptions <> nil then LOptions := AOptions^
  else LOptions := TALMapLibre.THeatmapLayerOptions.Create;

  var LLayout := '';
  var LPaint := '';
  for var LDataKey in ALGetLayerDataKeys(AGeoJSON, ALHeatmapLayerDataKeys) do begin
    var LName := ALLayerDataKeyNames[LDataKey].Substring(1).Replace('_', '-');
    var LExpr := '["get",' + ALJsonEncodeTextW(ALLayerDataKeyNames[LDataKey]) + ']';
    LPaint := LPaint + '"' + LName + '":' + LExpr + ',';
  end;
  LLayout := LLayout + '"visibility":' + ALJsonEncodeTextW(LOptions.FVisibility) + ',';
  //--
  LPaint := LPaint + '"heatmap-radius":' + ALJsonEncodeFloatW(LOptions.FHeatmapRadius, TALJSONInterchangeFormat.icfStandardJson) + ',';
  LPaint := LPaint + '"heatmap-intensity":' + ALJsonEncodeFloatW(LOptions.FHeatmapIntensity, TALJSONInterchangeFormat.icfStandardJson) + ',';
  LPaint := LPaint + '"heatmap-opacity":' + ALJsonEncodeFloatW(LOptions.FHeatmapOpacity, TALJSONInterchangeFormat.icfStandardJson) + ',';

  if LLayout <> '' then Delete(LLayout, Length(LLayout), 1);
  if LPaint <> '' then Delete(LPaint, Length(LPaint), 1);

  SetGeoJsonLayer(AId, 'heatmap', AGeoJSON, LLayout, LPaint, False{AIsSymbolLayer});
end;

{****************************************************************************************************************************************************************************************************************}
procedure TALWinMapLibreView.SetRasterLayer(const AId: String; const ATileURL: String; const AIsTileJSON: Boolean = False; const ATileSize: Integer = 256; const AOptions: TALMapLibre.PRasterLayerOptions = nil);
begin
  if not FMapReady then raise Exception.Create('MapLibre is not initialized');

  var LOptions: TALMapLibre.TRasterLayerOptions;
  if AOptions <> nil then LOptions := AOptions^
  else LOptions := TALMapLibre.TRasterLayerOptions.Create;

  var LSource: String;
  if AIsTileJSON then LSource := 'url: ' + ALJsonEncodeTextW(ATileURL)
  else LSource := 'tiles: [' + ALJsonEncodeTextW(ATileURL) + ']';

  var LLayout := '';
  LLayout := LLayout + '"visibility":' + ALJsonEncodeTextW(LOptions.FVisibility) + ',';

  var LPaint := '';
  LPaint := LPaint + '"raster-opacity":' + ALJsonEncodeFloatW(LOptions.FRasterOpacity, TALJSONInterchangeFormat.icfStandardJson) + ',';
  LPaint := LPaint + '"raster-hue-rotate":' + ALJsonEncodeFloatW(LOptions.FRasterHueRotate, TALJSONInterchangeFormat.icfStandardJson) + ',';
  LPaint := LPaint + '"raster-brightness-min":' + ALJsonEncodeFloatW(LOptions.FRasterBrightnessMin, TALJSONInterchangeFormat.icfStandardJson) + ',';
  LPaint := LPaint + '"raster-brightness-max":' + ALJsonEncodeFloatW(LOptions.FRasterBrightnessMax, TALJSONInterchangeFormat.icfStandardJson) + ',';
  LPaint := LPaint + '"raster-saturation":' + ALJsonEncodeFloatW(LOptions.FRasterSaturation, TALJSONInterchangeFormat.icfStandardJson) + ',';
  LPaint := LPaint + '"raster-contrast":' + ALJsonEncodeFloatW(LOptions.FRasterContrast, TALJSONInterchangeFormat.icfStandardJson) + ',';
  LPaint := LPaint + '"raster-resampling":' + ALJsonEncodeTextW(LOptions.FRasterResampling) + ',';
  LPaint := LPaint + '"raster-fade-duration":' + ALJsonEncodeFloatW(LOptions.FRasterFadeDuration, TALJSONInterchangeFormat.icfStandardJson) + ',';

  if LLayout <> '' then Delete(LLayout, Length(LLayout), 1);
  if LPaint <> '' then Delete(LPaint, Length(LPaint), 1);

  // Like on Android/iOS, the layer and the source are always recreated
  EvaluateJavaScript(
    '(function() {' +
      'const map = window.map;' +
      'const id = ' + ALJsonEncodeTextW(AId) + ';' +
      'if (map.getLayer(id)) map.removeLayer(id);' +
      'if (map.getSource(id)) map.removeSource(id);' +
      'map.addSource(id, {type: "raster", ' + LSource + ', tileSize: ' + ALIntToStrW(ATileSize) + '});' +
      'map.addLayer({' +
        'id: id,' +
        'type: "raster",' +
        'source: id,' +
        'layout: {' + LLayout + '},' +
        'paint: {' + LPaint + '}' +
      '});' +
    '})();');
end;

{**********************************************************}
procedure TALWinMapLibreView.RemoveLayer(const AId: String);
begin
  if not FMapReady then Exit;
  EvaluateJavaScript(
    '''
    (function() {
      const map = window.map;
      const id = "
    ''' + ALJavaScriptEncode(AId) + '''
    ";
      if (map.getLayer(id)) map.removeLayer(id);
      if (map.getSource(id)) map.removeSource(id);
      window.symbolLayerIds = (window.symbolLayerIds || []).filter(x => x !== id);
    })();
    ''');
end;

{******************************************************}
function TALWinMapLibreView.GetViewportBounds: TALRectD;
begin
  if not FMapReady then raise Exception.Create('MapLibre is not initialized');

  var LCompleted := False;
  var LResult := '';
  EvaluateJavaScript(
    'window.map.getBounds().toArray()',
    procedure(const AResult: String)
    begin
      LResult := AResult; // [[2.3483376190212084,48.85103720424459],[2.3560623809837864,48.86216217765707]]
      LCompleted := True;
    end);

  Var LForm := Screen.ActiveForm;
  if LForm = nil then LForm := Application.MainForm;
  if LForm <> nil then Winapi.Windows.EnableWindow(WindowHandleToPlatform(LForm.Handle).Wnd, False);
  try
    while not LCompleted do
      Application.ProcessMessages;
  finally
    if LForm <> nil then Winapi.Windows.EnableWindow(WindowHandleToPlatform(LForm.Handle).Wnd, True);
  end;

  if (LResult = '') or (LResult = 'null') then raise Exception.Create('Invalid JavaScript result');
  var LJSON := TALJsonDocumentW.CreateFromJSONString(LResult);
  try
    Result := TALRectD.Create(
                LJSON.ChildNodes[0].ChildNodes[0].Float, // West
                LJSON.ChildNodes[0].ChildNodes[1].Float, // South
                LJSON.ChildNodes[1].ChildNodes[0].Float, // East
                LJSON.ChildNodes[1].ChildNodes[1].Float); // North
  finally
    ALFreeAndNil(LJSON);
  end;
end;

{*************************************************************************}
function TALWinMapLibreView.GetCameraPosition: TALMapLibre.TCameraPosition;
begin
  if not FMapReady then raise Exception.Create('MapLibre is not initialized');

  var LCompleted := False;
  var LResult := '';
  EvaluateJavaScript(
        '''
        (function() {
          const map = window.map;
          const center = map.getCenter();
          const padding = map.getPadding();
          return {
            bearing: map.getBearing(),
            padding: [
              padding.left,
              padding.top,
              padding.right,
              padding.bottom
            ],
            roll: map.getRoll(),
            target: [center.lat, center.lng],
            tilt: map.getPitch(),
            zoom: map.getZoom()
          };
        })()
        ''',
        procedure(const AResult: String)
        begin
          LResult := AResult;
          LCompleted := True;
        end);

      var LForm := Screen.ActiveForm;
      if LForm = nil then LForm := Application.MainForm;
      if LForm <> nil then Winapi.Windows.EnableWindow(WindowHandleToPlatform(LForm.Handle).Wnd, False);
      try
        while not LCompleted do
          Application.ProcessMessages;
      finally
        if LForm <> nil then Winapi.Windows.EnableWindow(WindowHandleToPlatform(LForm.Handle).Wnd, True);
      end;

      if (LResult = '') or (LResult = 'null') then raise Exception.Create('Invalid JavaScript result');
      var LJSON := TALJsonDocumentW.CreateFromJSONString(LResult);
      try
        Result.bearing := LJSON.GetChildNode('bearing').Float;
        var LPadding := LJSON.GetChildNode('padding');
        Result.padding := TALRectD.Create(
                            LPadding.ChildNodes[0].Float, // Left
                            LPadding.ChildNodes[1].Float, // Top
                            LPadding.ChildNodes[2].Float, // Right
                            LPadding.ChildNodes[3].Float); // Bottom
        Result.roll := LJSON.GetChildNode('roll').Float;
        var LTarget := LJSON.GetChildNode('target');
        Result.target := TALLatLng.Create(
                           LTarget.ChildNodes[0].Float, // Latitude
                           LTarget.ChildNodes[1].Float); // Longitude
        Result.tilt := LJSON.GetChildNode('tilt').Float;
        Result.zoom := LJSON.GetChildNode('zoom').Float;
      finally
        ALFreeAndNil(LJSON);
      end;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure TALWinMapLibreView.ApplyStyle;
    begin
      if FMapReady then begin
        var LStyle: string;
        if Control.StyleJSON <> '' then LStyle := Control.StyleJSON
        else if Control.StyleURL <> '' then LStyle := '"' + ALJAvascriptEncode(Control.StyleURL) + '"'
        else Exit;
        EvaluateJavaScript('window.map.setStyle('+Lstyle+');');
      end
      else if not IsWebView2Ready then begin
        LoadData(
          '', // ABaseUrl
          GetHtml, // AData
          'text/html', // AMimeType
          'UTF-8'); // AEncoding
      end
      else
        FApplyStyleWhenMapReady := True;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure TALWinMapLibreView.ApplyCompass;
    begin
      // Called again on 'map-ready'
      if not FMapReady then Exit;
      {$IF defined(DEBUG)}
      ALLog('TALWinMapLibreView.ApplyCompass');
      {$ENDIF}
      var LCompass := Control.Compass;

      var LPosition: String;
      case LCompass.Gravity of
        TALMapLibre.TOrnamentGravity.TopLeft: LPosition := 'top-left';
        TALMapLibre.TOrnamentGravity.TopRight: LPosition := 'top-right';
        TALMapLibre.TOrnamentGravity.BottomLeft: LPosition := 'bottom-left';
        TALMapLibre.TOrnamentGravity.BottomRight: LPosition := 'bottom-right';
        else raise Exception.Create('Error 5C8E1A3F-7B2D-4E96-A0F4-3D9B6E1C7A52');
      end;

      var LImage := '';
      if LCompass.Enabled and (LCompass.ResourceName <> '') then begin
        var LBitmap := ALCreateBitmapFromResource(
                         LCompass.ResourceName, // const AResourceName: String;
                         nil, // const AResourceStream: TStream;
                         '', // const AMaskResourceName: String;
                         ALGetScreenScale, // const AScale: Single;
                         TRectF.Empty, // const ASrcRect: TRectF; // In source image coordinates (real pixels). Use TRectF.Empty to use the full image
                         LCompass.Width, LCompass.Height, // const W, H: single;
                         True, // const AApplyMetadataOrientation: Boolean;
                         TALImageWrapMode.fit, // const AWrapMode: TALImageWrapMode;
                         TpointF.Create(0.5,0.5), // const ACropCenter: TpointF;
                         TalphaColors.null, // const ATintColor: TalphaColor;
                         0, // const ABlurRadius: single;
                         0, // const AXRadius: Single;
                         0); // const AYRadius: Single): TALBitmap;
        try
          LImage := ALBitmapToBase64(LBitmap, 'PNG', 100);
        finally
          ALFreeAndNilBitmap(LBitmap);
        end;
      end;

      // The compass is the compass button of a maplibregl.NavigationControl,
      // wrapped in a custom control to be able to style its container.
      EvaluateJavaScript(
        '(function() {' +
          'const map = window.map;' +
          'if (window.compassControl) {' +
            'map.removeControl(window.compassControl);' +
            'window.compassControl = null;' +
          '}' +
          'if (!' + ALJsonEncodeBooleanW(LCompass.Enabled) + ') return;' +
          'const fadeFacingNorth = ' + ALJsonEncodeBooleanW(LCompass.FadeFacingNorth) + ';' +
          'const image = ' + ALJsonEncodeTextW(LImage) + ';' +
          'const width = ' + ALJsonEncodeFloatW(LCompass.Width, TALJSONInterchangeFormat.icfStandardJson) + ';' +
          'const height = ' + ALJsonEncodeFloatW(LCompass.Height, TALJSONInterchangeFormat.icfStandardJson) + ';' +
          'const margin = "' + ALJsonEncodeFloatW(LCompass.Margins.Top, TALJSONInterchangeFormat.icfStandardJson) + 'px ' +
                               ALJsonEncodeFloatW(LCompass.Margins.Right, TALJSONInterchangeFormat.icfStandardJson) + 'px ' +
                               ALJsonEncodeFloatW(LCompass.Margins.Bottom, TALJSONInterchangeFormat.icfStandardJson) + 'px ' +
                               ALJsonEncodeFloatW(LCompass.Margins.Left, TALJSONInterchangeFormat.icfStandardJson) + 'px";' +
          'const nav = new maplibregl.NavigationControl({showCompass: true, showZoom: false, visualizePitch: false});' +
          'window.compassControl = {' +
            'onAdd: function(m) {' +
              'const el = nav.onAdd(m);' +
              'el.style.margin = margin;' +
              'if (image !== "") {' +
                'el.style.background = "transparent";' +
                'el.style.boxShadow = "none";' +
                'const button = el.querySelector(".maplibregl-ctrl-compass");' +
                'if (button) {' +
                  'button.style.width = width + "px";' +
                  'button.style.height = height + "px";' +
                  'button.style.background = "transparent";' +
                '}' +
                'const icon = el.querySelector(".maplibregl-ctrl-icon");' +
                'if (icon) {' +
                  'icon.style.backgroundImage = "url(data:image/png;base64," + image + ")";' +
                  'icon.style.backgroundSize = "contain";' +
                  'icon.style.backgroundRepeat = "no-repeat";' +
                  'icon.style.backgroundPosition = "center";' +
                '}' +
              '}' +
              'if (fadeFacingNorth) {' +
                'el.style.transition = "opacity 0.3s";' +
                'this._update = function() {' +
                  'const north = Math.abs(m.getBearing()) < 0.01;' +
                  'el.style.opacity = north ? "0" : "1";' +
                  'el.style.pointerEvents = north ? "none" : "auto";' +
                '};' +
                'm.on("rotate", this._update);' +
                'this._update();' +
              '}' +
              'return el;' +
            '},' +
            'onRemove: function(m) {' +
              'if (this._update) m.off("rotate", this._update);' +
              'nav.onRemove();' +
            '}' +
          '};' +
          'map.addControl(window.compassControl, "' + LPosition + '");' +
        '})();');
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure TALWinMapLibreView.ShowLoadingSpinner(const AHTML: String);
    begin
      if not FMapReady then raise Exception.Create('MapLibre is not initialized');
      EvaluateJavaScript(
        '''
        (function() {
          let container = document.getElementById('loadingSpinner');
          if (container) container.remove();
          container = document.createElement('div');
          container.id = 'loadingSpinner';
          container.innerHTML = "
        '''
        + ALJavaScriptEncode(AHTML) +
        '''
        ";
          document.body.appendChild(container);
          // Scripts inserted with innerHTML are never executed: recreate them
          // so that JavaScript-driven animations work as well.
          container.querySelectorAll('script').forEach(function(oldScript) {
            const newScript = document.createElement('script');
            for (const attr of oldScript.attributes)
              newScript.setAttribute(attr.name, attr.value);
            newScript.textContent = oldScript.textContent;
            oldScript.replaceWith(newScript);
          });
        })();
    ''');
end;

{**************************************************************************************************************************************************}
procedure TALWinMapLibreView.ShowLoadingSpinner(const AColor: TAlphaColor = Talphacolors.null; const AWidth: Single = 0; const AHeight: Single = 0);
begin
  var LColor := AColor;
  if LColor = TAlphaColors.Null then LColor := TAlphaColors.Black;
  var LColorRec := TAlphaColorRec(LColor);
  var LRGB := ALIntToStrW(LColorRec.R) + ',' + ALIntToStrW(LColorRec.G) + ',' + ALIntToStrW(LColorRec.B);
  var LAlpha: Single := LColorRec.A / 255;

  var LWidth := AWidth;
  if CompareValue(LWidth, 0) <= 0 then LWidth := 48;
  var LHeight := AHeight;
  if CompareValue(LHeight, 0) <= 0 then LHeight := 48;
  var LBorderWidth: Single;
  if LWidth < LHeight then LBorderWidth := LWidth / 10
  else LBorderWidth := LHeight / 10;
  if LBorderWidth < 2 then LBorderWidth := 2;
  ShowLoadingSpinner(
    '<div style="' +
      'box-sizing: border-box;' +
      'width: ' + ALFloatToStrW(LWidth) + 'px;' +
      'height: ' + ALFloatToStrW(LHeight) + 'px;' +
      'border-radius: 50%;' +
      'border: ' + ALFloatToStrW(LBorderWidth) + 'px solid rgba(' + LRGB + ',' + ALFloatToStrW(LAlpha * 0.2) + ');' +
      'border-top-color: rgba(' + LRGB + ',' + ALFloatToStrW(LAlpha) + ');' +
      'animation: loadingSpinnerRotate 0.8s linear infinite;' +
    '"></div>' +
    '<style>@keyframes loadingSpinnerRotate { to { transform: rotate(360deg); } }</style>');
end;

{**********************************************}
procedure TALWinMapLibreView.HideLoadingSpinner;
begin
  if FMapReady then
    EvaluateJavaScript(
      '''
      (function() {
        const container = document.getElementById('loadingSpinner');
        if (container) container.remove();
      })();
      ''');
end;

{$endif}
{$ENDREGION}

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALMapLibre]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALMapLibre, 'Size');
  UnlistPublishedProperty(TALMapLibre, 'StyleName');
  UnlistPublishedProperty(TALMapLibre, 'OnTap');
  {$ENDIF}
end;


initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.MapLibre','initialization');
  {$ENDIF}
  RegisterFmxClasses([TALMapLibre]);

end.