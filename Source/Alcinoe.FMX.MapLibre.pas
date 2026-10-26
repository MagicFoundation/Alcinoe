unit Alcinoe.FMX.MapLibre;

interface

{$I Alcinoe.inc}

uses
  System.Classes,
  System.UITypes,
  FMX.Controls,
  {$IF defined(ANDROID)}
  System.Messaging,
  Androidapi.Helpers,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Alcinoe.AndroidApi.Widget,
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
  {$ENDIF}
  {$IF defined(MSWINDOWS)}
  Alcinoe.FMX.NativeView.Win,
  Alcinoe.FMX.WebBrowser,
  {$ENDIF}
  Alcinoe.Common,
  Alcinoe.FMX.NativeControl;

type

  {******************}
  TALMapLibre = class;

  {********************************}
  TALMapLibreCameraPosition = record
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
    procedure SetMarkers(const AGeoJSON: String; const AIconAllowsOverlap: Boolean);
    function GetViewportBounds: TALRectD;
    function GetCameraPosition: TALMapLibreCameraPosition;
    procedure ApplyStyle;
  end;

{$endif}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(ios)}

  {****************************************}
  IALIosMapLibreView = interface(MLNMapView)
    ['{296C1058-9A89-4D71-A5B6-9F0EF1681435}']
  end;

  {********************************************}
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
    FMapViewDelegate: TMapViewDelegate;
    FMapViewTapGestureRecognizer: UITapGestureRecognizer;
    FLoadingSpinnerView: UIView;
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
    procedure SetMarkers(const AGeoJSON: String; const AIconAllowsOverlap: Boolean);
    function GetViewportBounds: TALRectD;
    function GetCameraPosition: TALMapLibreCameraPosition;
    procedure ApplyStyle;
  end;

{$endif}
{$ENDREGION}

{$REGION ' MSWINDOWS'}
{$IF defined(MSWINDOWS)}

  {**********************************************}
  TALWinMapLibreView = class(TALWinWebBrowserView)
  private
    FMapReady: Boolean;
    FApplyStyleWhenMapReady: Boolean;
    function GetControl: TALMapLibre;
  protected
    function GetHtml: String;
    procedure DoWebMessage(const AMessage: string); override;
  public
    constructor Create; override;
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
    procedure SetMarkers(const AGeoJSON: String; const AIconAllowsOverlap: Boolean);
    function GetViewportBounds: TALRectD;
    function GetCameraPosition: TALMapLibreCameraPosition;
    procedure ApplyStyle;
  end;

{$endif}
{$ENDREGION}

  {*************************}
  [ComponentPlatforms($FFFF)]
  TALMapLibre = class({$IF defined(MSWINDOWS) or defined(ALMacOS)}TALWebBrowser{$ELSE}TALNativeControl{$ENDIF}, IControlTypeSupportable, IALNativeControl)
  public
    type
      TMarkerClickEvent = procedure(Sender: TObject; const AMarkerID: String) of object;
      TMapClickEvent = procedure(Sender: TObject; const ALatitude: Double; Const ALongitude: Double) of object;
  private
    FStyleJSON: String;
    FStyleUrl: String;
    FOnMapReady: TNotifyEvent;
    FOnStyleLoaded: TNotifyEvent;
    FOnCameraWillChange: TNotifyEvent;
    FOnCameraIsChanging: TNotifyEvent;
    FOnCameraDidChange: TNotifyEvent;
    FOnMarkerClick: TMarkerClickEvent;
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
  public
    constructor Create(AOwner: TComponent); override;
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
    procedure SetMarkers(const AGeoJSON: String; const AIconAllowsOverlap: Boolean = True);
    function GetViewportBounds: TALRectD;
    function GetCameraPosition: TALMapLibreCameraPosition;
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
    property OnMarkerClick: TMarkerClickEvent read FOnMarkerClick write FOnMarkerClick;
    property OnMapClick: TMapClickEvent read FOnMapClick write FOnMapClick;
  end;

procedure Register;

implementation

uses
  System.SysUtils,
  System.types,
  System.Math,
  System.Math.Vectors,
  {$IF defined(ANDROID)}
  System.Rtti,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Widget,
  Androidapi.Jni,
  FMX.Platform,
  FMX.Forms,
  Alcinoe.fmx.Common,
  {$ENDIF}
  {$IF defined(IOS)}
  iOSapi.CoreGraphics,
  iOSApi.Foundation,
  iOSapi.CoreLocation,
  Macapi.ObjCRuntime,
  Macapi.Helpers,
  FMX.Helpers.iOS,
  {$ENDIF}
  {$IF defined(MSWindows)}
  WinApi.Windows,
  System.Skia.API,
  Fmx.Forms,
  FMX.Platform.Win,
  Alcinoe.HTML,
  Alcinoe.JSONDoc,
  {$ENDIF}
  FMX.Types,
  Alcinoe.Localization,
  Alcinoe.FMX.Graphics,
  Alcinoe.StringUtils;

{*********************************************}
constructor TALMapLibreCameraPosition.Create(
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

  if FMapLibreView <> nil then begin
    FMapLibreView.FOnMapReadyCallback := nil;
    FMapLibreView.FMaplibreMap := maplibreMap;
    FMapLibreView.FMaplibreMap.addOnCameraMoveStartedListener(FMapLibreView.FOnCameraMoveStartedListener);
    FMapLibreView.FMaplibreMap.addOnCameraMoveListener(FMapLibreView.FOnCameraMoveListener);
    FMapLibreView.FMaplibreMap.addOnCameraIdleListener(FMapLibreView.FOnCameraIdleListener);
    FMapLibreView.FMaplibreMap.addOnMapClickListener(FMapLibreView.FOnMapClickListener);
    FmapLibreView.ApplyStyle;
    if Assigned(FMapLibreView.Control.FOnMapReady) then
      FMapLibreView.Control.FOnMapReady(FMapLibreView.Control);
  end;

  var LOnMapReadyCallback := Self;
  TThread.ForceQueue(nil,
    procedure
    begin
      ALFreeAndNil(LOnMapReadyCallback);
    end);

end;

{*********************************************************************************************************}
constructor TALAndroidMapLibreView.TOnDidFinishLoadingStyleListener.Create(const AMapLibreView: TALAndroidMapLibreView);
begin
  inherited Create;
  FMapLibreView := AMapLibreView;
end;

{*****************************************************************************************}
procedure TALAndroidMapLibreView.TOnDidFinishLoadingStyleListener.onDidFinishLoadingStyle;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidMapLibreView.TOnDidFinishLoadingStyleListener.onDidFinishLoadingStyle');
  {$ENDIF}
  if Assigned(FMapLibreView.Control.FOnStyleLoaded) then
    FMapLibreView.Control.FOnStyleLoaded(FMapLibreView.Control);
end;

{*********************************************************************************************************}
constructor TALAndroidMapLibreView.TOnCameraMoveStartedListener.Create(const AMapLibreView: TALAndroidMapLibreView);
begin
  inherited Create;
  FMapLibreView := AMapLibreView;
end;

{*****************************************************************************************}
procedure TALAndroidMapLibreView.TOnCameraMoveStartedListener.onCameraMoveStarted(reason: Integer); cdecl;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidMapLibreView.TOnCameraMoveStartedListener.onCameraMoveStarted');
  {$ENDIF}
  if Assigned(FMapLibreView.Control.FOnCameraWillChange) then
    FMapLibreView.Control.FOnCameraWillChange(FMapLibreView.Control);
end;

{*********************************************************************************************************}
constructor TALAndroidMapLibreView.TOnCameraMoveListener.Create(const AMapLibreView: TALAndroidMapLibreView);
begin
  inherited Create;
  FMapLibreView := AMapLibreView;
end;

{*****************************************************************************************}
procedure TALAndroidMapLibreView.TOnCameraMoveListener.onCameraMove;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidMapLibreView.TOnCameraMoveListener.onCameraMove');
  {$ENDIF}
  if Assigned(FMapLibreView.Control.FOnCameraIsChanging) then
    FMapLibreView.Control.FOnCameraIsChanging(FMapLibreView.Control);
end;

{*********************************************************************************************************}
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

{*************************************************************************************}
function TALAndroidMapLibreView.TOnMapClickListener.onMapClick(point: JLatLng): Boolean;
begin
  {$IF defined(DEBUG)}
  ALLog('TALAndroidMapLibreView.TOnMapClickListener.onMapClick');
  {$ENDIF}

  // Return: True if this click should be consumed and not passed further to
  // other listeners registered afterwards, false otherwise.
  Result := True;

  if Assigned(FMapLibreView.Control.FOnMapClick) then
    FMapLibreView.Control.FOnMapClick(FMapLibreView.Control, point.getLatitude, point.getLongitude);

  if not Assigned(FMapLibreView.Control.FOnMarkerClick) then Exit;

  if FMapLibreView.FMaplibreMap = nil then Exit;
  var LPoint := FMapLibreView.FMaplibreMap.getProjection.toScreenLocation(point);

  var LLayers := TJavaObjectArray<JString>.Create(1);
  LLayers.Items[0] := StringToJString('ALMarkerLayer');
  var LFeatures := FMapLibreView.FMaplibreMap.queryRenderedFeatures(LPoint, LLayers);
  if (LFeatures = nil) or (LFeatures.size = 0) then Exit;

  var LFeature := TJFeature.Wrap(TAndroidHelper.JObjectToID(LFeatures.get(0)));
  if (LFeature = nil) or (not LFeature.hasProperty(StringToJString('id'))) then Exit;
  var LMarkerID := LFeature.getStringProperty(StringToJString('id'));

  FMapLibreView.Control.FOnMarkerClick(
    FMapLibreView.Control,
    JStringToString(LMarkerID));
end;

{****************************************}
constructor TALAndroidMapLibreView.Create;
begin
  inherited; // This will call CreateView

  FMaplibreMap := nil;
  FLoadingSpinnerView := nil;

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

{**************************************}
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

  if FOnMapReadyCallback <> nil then begin
    FOnMapReadyCallback.FMapLibreView := nil;
    ALFreeAndNil(FOnMapReadyCallback);
  end;

  View.removeOnDidFinishLoadingStyleListener(FOnDidFinishLoadingStyleListener);
  ALFreeAndNil(FOnDidFinishLoadingStyleListener);
  ALFreeAndNil(FOnCameraMoveStartedListener);
  ALFreeAndNil(FOnCameraMoveListener);
  ALFreeAndNil(FOnCameraIdleListener);
  ALFreeAndNil(FOnMapClickListener);

  View.onPause();
  View.onStop();
  View.onDestroy();

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
  If FMaplibreMap = nil then raise Exception.Create('Cannot set the map center coordinate: MapLibre is not initialized');
  FMaplibreMap.moveCamera(
    TJCameraUpdateFactory.JavaClass.newLatLngZoom(
      TJLatLng.JavaClass.init(ALatitude, ALongitude), // latLng: JLatLng;
      AZoomLevel)); // zoom: Double
end;

{*******************************************}
procedure TALAndroidMapLibreView.SetImage(
            const AResourceName: String;
            const AStream: TStream;
            const AScale: Single;
            const W, H: single;
            const AName: String);
begin
  var LStyle := FMaplibreMap.getStyle;
  if LStyle = nil then raise Exception.Create('Cannot register map image: MapLibre style is not loaded');
  var LBitmap := ALCreateJBitmapFromResource(
                   AResourceName, // const AResourceName: String;
                   AStream, // const AResourceStream: TStream;
                   '', // const AMaskResourceName: String;
                   AScale, // const AScale: Single;
                   TRectF.Empty, // const ASrcRect: TRectF; // In source image coordinates (real pixels). Use TRectF.Empty to use the full image
                   W, H, // const W, H: single;
                   True, // const AApplyMetadataOrientation: Boolean;
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
  var LStyle := FMaplibreMap.getStyle;
  if LStyle = nil then raise Exception.Create('Cannot unregister map image: MapLibre style is not loaded');
  LStyle.removeImage(StringToJString(AName));
end;

{*******************************************}
procedure TALAndroidMapLibreView.SetMarkers(const AGeoJSON: String; const AIconAllowsOverlap: Boolean);
begin
  var LStyle := FMaplibreMap.getStyle;
  if LStyle = nil then raise Exception.Create('Cannot set map markers: MapLibre style is not loaded');

  var LSource := LStyle.getSource(StringToJString('ALMarkerSource'));
  if LSource = nil then begin
    LSource := TJGeoJsonSource.JavaClass.init(
                 StringToJString('ALMarkerSource'), // id
                 StringToJString(AGeoJSON)); // geoJson
    LStyle.addSource(LSource);
  end
  else begin
    var LGeoJsonSource := TJGeoJsonSource.Wrap(TAndroidHelper.JObjectToID(LSource));
    LGeoJsonSource.setGeoJson(StringToJString(AGeoJSON));
  end;

  if LStyle.getLayer(StringToJString('ALMarkerLayer')) = nil then begin
    var LLayer := TJSymbolLayer.JavaClass.init(
                    StringToJString('ALMarkerLayer'), // layerId
                    StringToJString('ALMarkerSource')); // sourceId
    var LProperties := TJavaObjectArray<JPropertyValue>.Create(5);
    //--
    LProperties.Items[0] := TJPropertyFactory.JavaClass.iconImage(
                              TJExpression.JavaClass.get(
                                StringToJString('_icon_image')));
    //--
    LProperties.Items[1] := TJPropertyFactory.JavaClass.iconAnchor(
                              TJExpression.JavaClass.raw(
                                StringToJString('["coalesce",["get","_icon_anchor"],"bottom"]')));
    //--
    LProperties.Items[2] := TJPropertyFactory.JavaClass.iconSize(
      TJExpression.JavaClass.raw(
        StringToJString('["coalesce",["get","_icon_size"],1.0]')));
    //--
    LProperties.Items[3] := TJPropertyFactory.JavaClass.symbolSortKey(
      TJExpression.JavaClass.raw(
        StringToJString('["coalesce",["get","_icon_size"],1.0]')));
    //--
    LProperties.Items[4] := TJPropertyFactory.JavaClass.iconAllowOverlap(
                              TJBoolean.JavaClass.valueOf(AIconAllowsOverlap));
    //--
    LLayer.setProperties(LProperties);
    LStyle.addLayer(LLayer);
  end;
end;

{**********************************************************}
function TALAndroidMapLibreView.GetViewportBounds: TALRectD;
begin
  if FMaplibreMap = nil then raise Exception.Create('Cannot get visible bounds: MapLibre is not initialized');
  var LBounds := FMaplibreMap.getProjection.getVisibleRegion.latLngBounds;
  Result := TALRectD.Create(
              LBounds.getLonWest,
              LBounds.getLatSouth,
              LBounds.getLonEast,
              LBounds.getLatNorth);
end;

{***************************************************************************}
function TALAndroidMapLibreView.GetCameraPosition: TALMapLibreCameraPosition;
begin
  if FMaplibreMap = nil then raise Exception.Create('Cannot get camera position: MapLibre is not initialized');
  var LCameraPosition := FMaplibreMap.getCameraPosition;
  Result.bearing := LCameraPosition.bearing;
  var LPadding := LCameraPosition.padding;
  if LPadding <> nil then
    Result.padding := TALRectD.Create(
                        LPadding.Items[0] / ALGetScreenScale, // Left
                        LPadding.Items[1] / ALGetScreenScale, // Top
                        LPadding.Items[2] / ALGetScreenScale, // Right
                        LPadding.Items[3] / ALGetScreenScale) // Bottom
  else
    Result.padding := TALRectD.Empty;
  Result.roll := LCameraPosition.roll;
  var LTarget := LCameraPosition.target;
  if LTarget <> nil then
    Result.target := TALLatLng.Create(LTarget.getLatitude, LTarget.getLongitude)
  else
    Result.target := TALLatLng.Create(ALNullLatLng, ALNullLatLng);
  Result.tilt := LCameraPosition.tilt;
  Result.zoom := LCameraPosition.zoom;
end;

{*******************************************}
procedure TALAndroidMapLibreView.ApplyStyle;
begin
  if FMaplibreMap <> nil then begin
    {$IF defined(DEBUG)}
    ALLog('TALAndroidMapLibreView.ApplyStyle');
    {$ENDIF}
    var LBuilder := TJStyle_Builder.JavaClass.init;
    if Control.StyleJSON <> '' then LBuilder := LBuilder.fromJson(StringToJString(Control.StyleJSON))
    else if Control.StyleURL <> '' then LBuilder := LBuilder.fromUri(StringToJString(Control.StyleURL))
    else Exit;
    FMaplibreMap.setStyle(LBuilder);
  end;
end;

{***************************************************************************}
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

{**************************************************************************************************************}
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
  if (CompareValue(AWidth, 0, TEpsilon.position) > 0) or
     (CompareValue(AWidth, 0, TEpsilon.position) > 0) then begin
    LProgressBar.setLayoutParams(
      TJViewGroup_LayoutParams.JavaClass.init(
        Round(AWidth * ALGetScreenScale), // width: Integer;
        Round(AHeight * ALGetScreenScale))); // height: Integer
  end;
  ShowLoadingSpinner(LProgressBar);
end;

{*********************************************************}
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

{************************************}
constructor TALIosMapLibreView.TMapViewDelegate.Create(const AMapLibreView: TALIosMapLibreView);
begin
  inherited Create;
  FMapLibreView := AMapLibreView;
end;

{*************************************************************************}
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

  if not Assigned(FMapLibreView.Control.FOnMarkerClick) then Exit;
  if LView.style = nil then Exit;

  var LLayers := TNSSet.Wrap(TNSSet.OCClass.setWithObject(NSObjectToID(StrToNSStr('ALMarkerLayer'))));
  var LFeatures := LView.visibleFeaturesAtPoint(LPoint, LLayers);
  if (LFeatures = nil) or (LFeatures.count = 0) then Exit;

  var LFeature := TMLNPointFeature.Wrap(LFeatures.objectAtIndex(0));
  if LFeature = nil then Exit;

  var LMarkerID := LFeature.attributeForKey(StrToNSStr('id'));
  if LMarkerID = nil then Exit;

  FMapLibreView.Control.FOnMarkerClick(
    FMapLibreView.Control,
    NSStrToStr(TNSString.Wrap(LMarkerID)));
end;

{************************************}
procedure TALIosMapLibreView.TMapViewDelegate.mapViewRegionWillChangeWithReasonAnimated(mapView: MLNMapView; regionWillChangeWithReason: MLNCameraChangeReason; animated: Boolean);
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosMapLibreView.TMapViewDelegate.mapViewRegionWillChangeWithReasonAnimated');
  {$ENDIF}
  if Assigned(FMapLibreView.Control.FOnCameraWillChange) then
    FMapLibreView.Control.FOnCameraWillChange(FMapLibreView.Control);
end;

{************************************}
procedure TALIosMapLibreView.TMapViewDelegate.mapViewRegionIsChangingWithReason(mapView: MLNMapView; regionIsChangingWithReason: MLNCameraChangeReason);
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosMapLibreView.TMapViewDelegate.mapViewRegionIsChangingWithReason');
  {$ENDIF}
  if Assigned(FMapLibreView.Control.FOnCameraIsChanging) then
    FMapLibreView.Control.FOnCameraIsChanging(FMapLibreView.Control);
end;

{************************************}
procedure TALIosMapLibreView.TMapViewDelegate.mapViewRegionDidChangeWithReasonAnimated(mapView: MLNMapView; regionDidChangeWithReason: MLNCameraChangeReason; animated: Boolean);
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosMapLibreView.TMapViewDelegate.mapViewRegionDidChangeWithReasonAnimated');
  {$ENDIF}
  if Assigned(FMapLibreView.Control.FOnCameraDidChange) then
    FMapLibreView.Control.FOnCameraDidChange(FMapLibreView.Control);
end;

{************************************}
procedure TALIosMapLibreView.TMapViewDelegate.mapViewDidFinishLoadingStyle(mapView: MLNMapView; didFinishLoadingStyle: MLNStyle);
begin
  {$IF defined(DEBUG)}
  ALLog('TALIosMapLibreView.TMapViewDelegate.mapViewDidFinishLoadingStyle');
  {$ENDIF}
  if Assigned(FMapLibreView.Control.FOnStyleLoaded) then
    FMapLibreView.Control.FOnStyleLoaded(FMapLibreView.Control);
end;

{**************************************}
constructor TALIosMapLibreView.Create;
begin
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
end;

{**************************************}
destructor TALIosMapLibreView.Destroy;
begin
  HideLoadingSpinner;

  View.removeGestureRecognizer(FMapViewTapGestureRecognizer);
  FMapViewTapGestureRecognizer.release;
  FMapViewTapGestureRecognizer := nil;

  View.setDelegate(nil);
  ALFreeAndNil(FMapViewDelegate);

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
  TThread.ForceQueue(nil,
    procedure
    begin
       {$IF defined(DEBUG)}
      ALLog('TALIosMapLibreView.onMapReady', 'Visible: '+ ALBoolToStrW(Visible, 'True', 'False'));
      {$ENDIF}
      if Assigned(Control.FOnMapReady) then
        Control.FOnMapReady(Control);
    end);
end;

{**********************************************************}
function TALIosMapLibreView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALIosMapLibreView);
end;

{***********************************************}
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

{*****************************************}
procedure TALIosMapLibreView.SetImage(
            const AResourceName: String;
            const AStream: TStream;
            const AScale: Single;
            const W, H: single;
            const AName: String);
begin
  var LStyle := View.style;
  if LStyle = nil then raise Exception.Create('Cannot register map image: MapLibre style is not loaded');
  var LCGImageRef := ALCreateCGImageRefFromResource(
                       AResourceName, // const AResourceName: String;
                       AStream, // const AResourceStream: TStream;
                       '', // const AMaskResourceName: String;
                       AScale, // const AScale: Single;
                       TRectF.Empty, // const ASrcRect: TRectF; // In source image coordinates (real pixels). Use TRectF.Empty to use the full image
                       W, H, // const W, H: single;
                       True, // const AApplyMetadataOrientation: Boolean;
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
  if LStyle = nil then raise Exception.Create('Cannot unregister map image: MapLibre style is not loaded');
  LStyle.removeImageForName(StrToNSStr(AName));
end;

{**************************************************************}
procedure TALIosMapLibreView.SetMarkers(const AGeoJSON: String; const AIconAllowsOverlap: Boolean);
begin
  var LStyle := View.style;
  if LStyle = nil then raise Exception.Create('Cannot set map markers: MapLibre style is not loaded');

  var LData := StrToNSStr(AGeoJSON).dataUsingEncoding(NSUTF8StringEncoding);
  var LError: Pointer := nil;
  var LShape := TMLNShape.OCClass.shapeWithData(
                  LData, // data: NSData;
                  NSUTF8StringEncoding, // encoding: NSStringEncoding;
                  @LError); // error: NSError
  if LShape = nil then begin
    var LErrorMessage: String;
    if LError <> nil then LErrorMessage := NSStrToStr(TNSError.Wrap(LError).localizedDescription)
    else LErrorMessage := 'Cannot set map markers: failed to parse GeoJSON';
    raise Exception.Create(LErrorMessage);
  end;

  var LSource := LStyle.sourceWithIdentifier(StrToNSStr('ALMarkerSource'));
  if LSource = nil then begin
    LSource := TMLNShapeSource.Wrap(
                 TMLNShapeSource.Alloc.initWithIdentifier(
                   StrToNSStr('ALMarkerSource'), // identifier: NSString;
                   LShape, // shape: MLNShape;
                   nil)); // options: NSDictionary;
    LStyle.addSource(LSource);
  end
  else
    TMLNShapeSource.wrap(LSource).setShape(LShape);

  if LStyle.layerWithIdentifier(StrToNSStr('ALMarkerLayer')) = nil then begin
    var LLayer := TMLNSymbolStyleLayer.Wrap(
                    TMLNSymbolStyleLayer.Alloc.initWithIdentifier(
                      StrToNSStr('ALMarkerLayer'), // identifier: NSString;
                      LSource)); // source: MLNSource;
    LLayer.setIconImageName(
      TNSExpression.OCClass.expressionForKeyPath(
        StrToNSStr('_icon_image')));
    //--
    LLayer.setIconAnchor(
      TNSExpression.OCClass.expressionWithFormat(
        StrToNSStr('mgl_coalesce({_icon_anchor, "bottom"})')));
    //--
    LLayer.setIconScale(
      TNSExpression.OCClass.expressionWithFormat(
        StrToNSStr('mgl_coalesce({_icon_size, 1.0})')));
    //--
    LLayer.setSymbolSortKey(
      TNSExpression.OCClass.expressionWithFormat(
        StrToNSStr('mgl_coalesce({_icon_size, 1.0})')));
    //--
    LLayer.setIconAllowsOverlap(
      TNSExpression.OCClass.expressionForConstantValue(
        NSObjectToID(TNSNumber.OCClass.numberWithBool(AIconAllowsOverlap))));
    //--
    LStyle.addLayer(LLayer);
  end;
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

{***********************************************************************}
function TALIosMapLibreView.GetCameraPosition: TALMapLibreCameraPosition;
begin
  var LCamera := View.camera;
  if LCamera = nil then raise Exception.Create('Cannot get camera position: MapLibre camera is not initialized');
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

{***********************************************************************}
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

{**********************************************************************************************************}
procedure TALIosMapLibreView.ShowLoadingSpinner(const AColor: TAlphaColor = Talphacolors.null; const AWidth: Single = 0; const AHeight: Single = 0);
begin
  var LActivityIndicatorView := TUIActivityIndicatorView.Wrap(
                                  TUIActivityIndicatorView.Alloc.initWithActivityIndicatorStyle(
                                    UIActivityIndicatorViewStyleLarge));
  try
    if AColor <> TAlphaColors.Null then
      LActivityIndicatorView.setColor(AlphaColorToUIColor(AColor));
    LActivityIndicatorView.sizeToFit;
    if (CompareValue(AWidth, 0, TEpsilon.position) > 0) or
       (CompareValue(AWidth, 0, TEpsilon.position) > 0) then begin
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

{******************************************************}
procedure TALIosMapLibreView.HideLoadingSpinner;
begin
  if FLoadingSpinnerView = nil then exit;
  FLoadingSpinnerView.removeFromSuperview;
  FLoadingSpinnerView.release;
  FLoadingSpinnerView := nil;
end;

{$endif}
{$ENDREGION}

{$REGION ' MSWINDOWS'}
{$IF defined(MSWINDOWS)}

{************************************}
constructor TALWinMapLibreView.Create;
begin
  inherited;
  FMapReady := False;
  FApplyStyleWhenMapReady := False;
  LoadData(
    '', // ABaseUrl
    GetHtml, // AData
    'text/html', // AMimeType
    'UTF-8'); // AEncoding
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
  TThread.ForceQueue(nil,
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
          else if LType = 'marker-click' then begin
            if Assigned(Control.FOnMarkerClick) then
              Control.FOnMarkerClick(
                Control,
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
                  window.chrome.webview.postMessage({type: 'style-loaded'});
                });

                window.map.on('movestart', function() {
                  window.chrome.webview.postMessage({type: 'camera-will-change'});
                });

                window.map.on('move', function() {
                  window.chrome.webview.postMessage({type: 'camera-is-changing'});
                });

                window.map.on('moveend', function() {
                  window.chrome.webview.postMessage({type: 'camera-did-change'});
                });

                window.map.on('click', function(e) {

                  // Notify Delphi about the map click.
                  window.chrome.webview.postMessage({
                    type: 'map-click',
                    latitude: e.lngLat.lat,
                    longitude: e.lngLat.lng
                  });

                  // Check if the click hit one of our markers.
                  if (!window.map.getLayer('ALMarkerLayer')) return;

                  const features = window.map.queryRenderedFeatures(
                    e.point,
                    { layers: ['ALMarkerLayer'] }
                  );

                  if (features.length === 0) return;

                  const markerID = features[0].properties?.id;
                  if (markerID === undefined || markerID === null) return;

                  // Notify Delphi about the marker click.
                  window.chrome.webview.postMessage({
                    type: 'marker-click',
                    id: String(markerID)
                  });

                });

                window.chrome.webview.postMessage({type: 'map-ready'});

              </script>
            </body>
            </html>
            ''';
end;

{**************************************************************************************************************}
procedure TALWinMapLibreView.SetCenterCoordinate(const ALatitude, ALongitude: Double; const AZoomLevel: double);
begin
  if not FMapReady then raise Exception.Create('Cannot set the map center coordinate: MapLibre is not initialized');
  ExecuteJavaScript(
    'window.map.jumpTo({' +
      'center: [' + ALFloatToStrW(ALongitude) + ',' + ALFloatToStrW(ALatitude) + '],' +
      'zoom: ' + ALFloatToStrW(AZoomLevel) +
    '});');
end;

{**************************************}
procedure TALWinMapLibreView.SetImage(
            const AResourceName: String;
            const AStream: TStream;
            const AScale: Single;
            const W, H: single;
            const   AName: String);
begin
  if not FMapReady then raise Exception.Create('Cannot register map image: MapLibre is not initialized');
  var LBitmap := ALCreateBitmapFromResource(
                   AResourceName, // const AResourceName: String;
                   AStream, // const AResourceStream: TStream;
                   '', // const AMaskResourceName: String;
                   AScale, // const AScale: Single;
                   TRectF.Empty, // const ASrcRect: TRectF; // In source image coordinates (real pixels). Use TRectF.Empty to use the full image
                   W, H, // const W, H: single;
                   True, // const AApplyMetadataOrientation: Boolean;
                   TALImageWrapMode.fit, // const AWrapMode: TALImageWrapMode;
                   TpointF.Create(0.5,0.5), // const ACropCenter: TpointF;
                   TalphaColors.null, // const ATintColor: TalphaColor;
                   0, // const ABlurRadius: single;
                   0, // const AXRadius: Single;
                   0); // const AYRadius: Single): CGImageRef;
  try

    var LBase64: String;

    {$IF defined(ALSkiaEngine)}
    var LOutputStream := TMemoryStream.Create;
    try
      var LSkStream := sk4d_wstreamadapter_create(LOutputStream);
      if LSkStream = 0 then raise Exception.Create('Cannot create Skia write stream');
      try
        if not sk4d_image_encode_to_stream(
                 LBitmap,
                 LSkStream,
                 PNG_SK_ENCODEDIMAGEFORMAT,
                 100) then raise Exception.Create('Cannot encode Skia image to PNG');
      finally
        sk4d_wstreamadapter_destroy(LSkStream);
      end;
      LBase64 := ALBase64EncodeBytesW(LOutputStream.Memory, LOutputStream.Size);
    finally
      ALFreeAndNil(LOutputStream);
    end;
    {$ELSE}
    var LOutputStream := TMemoryStream.Create;
    try
      LBitmap.SaveToStream(LOutputStream);
      LBase64 := ALBase64EncodeBytesW(LOutputStream.Memory, LOutputStream.Size);
    finally
      ALFreeAndNil(LOutputStream);
    end;
    {$ENDIF}

    ExecuteJavaScript(
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

{**************************************}
procedure TALWinMapLibreView.RemoveImage(const AName: String);
begin
  if not FMapReady then raise Exception.Create('Cannot unregister map image: MapLibre is not initialized');
  ExecuteJavaScript('if (window.map.hasImage("'+ ALJavaScriptEncode(AName) + '")) {window.map.removeImage("' + ALJavaScriptEncode(AName) + '");}');
end;

{**************************************}
procedure TALWinMapLibreView.SetMarkers(const AGeoJSON: String; const AIconAllowsOverlap: Boolean);
begin
  if not FMapReady then raise Exception.Create('Cannot set map markers: MapLibre is not initialized');
  ExecuteJavaScript(
    '''
    (function() {
      const map = window.map;
      const data = JSON.parse("
    '''
    + ALJavaScriptEncode(AGeoJSON) +
    '''
    ");

      if (!map.getSource('ALMarkerSource')) {
        map.addSource('ALMarkerSource', {
          type: 'geojson',
          data: data
        });
      }
      else {
        map.getSource('ALMarkerSource').setData(data);
      }

      if (!map.getLayer('ALMarkerLayer')) {
        map.addLayer({
          id: 'ALMarkerLayer',
          type: 'symbol',
          source: 'ALMarkerSource',
          layout: {
            'icon-image': ['get', '_icon_image'],
            'icon-anchor': ['coalesce', ['get', '_icon_anchor'], 'bottom'],
            'icon-size': ['coalesce', ['get', '_icon_size'], 1.0],
            'symbol-sort-key': ['coalesce', ['get', '_icon_size'], 1.0],
            'icon-allow-overlap':
    '''
    + ALBoolToStrW(AIconAllowsOverlap, 'true', 'false') +
    '''
          }
        });
      }
      else {
        map.setLayoutProperty(
          'ALMarkerLayer',
          'icon-allow-overlap',
    '''
    + ALBoolToStrW(AIconAllowsOverlap, 'true', 'false') +
    '''
        );
      }
    })();
    ''');
end;

{******************************************************}
function TALWinMapLibreView.GetViewportBounds: TALRectD;
begin
  if not FMapReady then raise Exception.Create('Cannot get viewport bounds: MapLibre is not initialized');

  var LCompleted := False;
  var LResult := '';
  ExecuteJavaScript(
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

  if (LResult = '') or (LResult = 'null') then raise Exception.Create('Cannot get viewport bounds: invalid JavaScript result');
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

{***********************************************************************}
function TALWinMapLibreView.GetCameraPosition: TALMapLibreCameraPosition;
begin
  if not FMapReady then raise Exception.Create('Cannot get camera position: MapLibre is not initialized');

  var LCompleted := False;
  var LResult := '';
  ExecuteJavaScript(
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

  if (LResult = '') or (LResult = 'null') then raise Exception.Create('Cannot get camera position: invalid JavaScript result');
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

{**************************************}
procedure TALWinMapLibreView.ApplyStyle;
begin
  if FMapReady then begin
    var LStyle: string;
    if Control.StyleJSON <> '' then LStyle := Control.StyleJSON
    else if Control.StyleURL <> '' then LStyle := '"' + ALJAvascriptEncode(Control.StyleURL) + '"'
    else Exit;
    ExecuteJavaScript('window.map.setStyle('+Lstyle+');');
  end
  else if not IsContentReady then begin
    LoadData(
      '', // ABaseUrl
      GetHtml, // AData
      'text/html', // AMimeType
      'UTF-8'); // AEncoding
  end
  else
    FApplyStyleWhenMapReady := True;
end;

{***********************************************************************}
procedure TALWinMapLibreView.ShowLoadingSpinner(const AHTML: String);
begin
  if not FMapReady then raise Exception.Create('Cannot show loading spinner: MapLibre is not initialized');
  ExecuteJavaScript(
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

{******************************************************}
procedure TALWinMapLibreView.HideLoadingSpinner;
begin
  if FMapReady then
    ExecuteJavaScript(
      '''
      (function() {
        const container = document.getElementById('loadingSpinner');
        if (container) container.remove();
      })();
      ''');
end;

{$endif}
{$ENDREGION}

{*************************************************}
constructor TALMapLibre.Create(AOwner: TComponent);
begin
  inherited;
  FOnMapReady := nil;
  FOnStyleLoaded := nil;
  FOnCameraWillChange := nil;
  FOnCameraIsChanging := nil;
  FOnCameraDidChange := nil;
  FOnMarkerClick := nil;
  FOnMapClick := nil;
  FStyleJSON := '';
  FStyleUrl := '';
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

{******************************************************}
procedure TALMapLibre.SetStyleURL(const AValue: string);
begin
  if (FStyleURL = AValue) and (FStyleJSON = '') then Exit;
  FStyleURL := AValue;
  FStyleJSON := '';
  if NativeView <> nil then
    NativeView.ApplyStyle;
end;

{*******************************************************}
procedure TALMapLibre.SetStyleJSON(const AValue: string);
begin
  if (FStyleJSON = AValue) and (FStyleURL = '') then Exit;
  FStyleJSON := AValue;
  FStyleURL := '';
  if NativeView <> nil then
    NativeView.ApplyStyle;
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

{*******************************************************}
procedure TALMapLibre.SetMarkers(const AGeoJSON: String; const AIconAllowsOverlap: Boolean = True);
begin
  if NativeView <> nil then
    NativeView.SetMarkers(AGeoJSON, AIconAllowsOverlap);
end;

{*******************************************************}
function TALMapLibre.GetViewportBounds: TALRectD;
begin
  if NativeView <> nil then
    result := NativeView.GetViewportBounds
  else
    Result := TAlRectD.Empty;
end;

{*******************************************************}
function TALMapLibre.GetCameraPosition: TALMapLibreCameraPosition;
begin
  if NativeView <> nil then
    result := NativeView.GetCameraPosition
  else begin
    Result := TALMapLibreCameraPosition.Create(
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

{*************************************************************************************************************************}
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
