//
// Made from MapLibre Native 13.5.3
//
unit Alcinoe.AndroidApi.MapLibre;

interface

{$I Alcinoe.inc}

uses
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Util,
  Androidapi.JNI.Os,
  Androidapi.JNI.Java.Net,
  Androidapi.JNI.Location,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Widget,
  Androidapi.JNIBridge;

type

  {**********************}
  JUiSettings = interface;
  JFeature = interface;
  JCameraPosition = interface;
  JVisibleRegion = interface;
  JProjection = interface;
  JExpression = interface;
  JPropertyValue = interface;
  JPropertyFactory = interface;
  JLayer = interface;
  JSymbolLayer = interface;
  JLineLayer = interface;
  JCircleLayer = interface;
  JFillLayer = interface;
  JFillExtrusionLayer = interface;
  JHeatmapLayer = interface;
  JColorReliefLayer = interface;
  JHillshadeLayer = interface;
  JRasterLayer = interface;
  JSource = interface;
  JGeoJsonSource = interface;
  JRasterSource = interface;
  JTileSet = interface;
  JLatLng = interface;
  JLatLngBounds = interface;
  JLatLngBounds_Builder = interface;
  JStyle = interface;
  JStyle_Builder = interface;
  JCameraUpdate = interface;
  JCameraUpdateFactory = interface;
  JMapLibre = interface;
  JMapLibreMap = interface;
  JMapLibreMap_OnCameraMoveStartedListener = interface;
  JMapLibreMap_OnCameraMoveListener = interface;
  JMapLibreMap_OnCameraMoveCanceledListener = interface;
  JMapLibreMap_OnCameraIdleListener = interface;
  JMapLibreMap_OnMapClickListener = interface;
  JMapLibreMapOptions = interface;
  JMapView = interface;
  JMapView_OnCameraWillChangeListener = interface;
  JMapView_OnCameraIsChangingListener = interface;
  JMapView_OnCameraDidChangeListener = interface;
  JMapView_OnDidFinishLoadingStyleListener = interface;
  JOnMapReadyCallback = interface;

  {*********************************************}
  JUiSettingsClass = interface(JObjectClass)
    //['{A4D03696-AE65-4D1E-8694-16BC0E40014F}']
  end;
  [JavaSignature('org/maplibre/android/maps/UiSettings')]
  JUiSettings = interface(JObject)
    //['{BC3F66EE-5565-4DC9-83C5-8581AF2CC59D}']
    //function areAllGesturesEnabled: Boolean; cdecl;
    //function getAttributionDialogManager: JAttributionDialogManager; cdecl;
    //function getAttributionGravity: Integer; cdecl;
    //function getAttributionMarginBottom: Integer; cdecl;
    //function getAttributionMarginLeft: Integer; cdecl;
    //function getAttributionMarginRight: Integer; cdecl;
    //function getAttributionMarginTop: Integer; cdecl;
    function getCompassGravity: Integer; cdecl;
    function getCompassImage: JDrawable; cdecl;
    function getCompassMarginBottom: Integer; cdecl;
    function getCompassMarginLeft: Integer; cdecl;
    function getCompassMarginRight: Integer; cdecl;
    function getCompassMarginTop: Integer; cdecl;
    //function getFlingAnimationBaseTime: Int64; cdecl;
    //function getFlingThreshold: Int64; cdecl;
    //function getFocalPoint: JPointF; cdecl;
    //function getHeight: Single; cdecl;
    //function getLogoGravity: Integer; cdecl;
    //function getLogoMarginBottom: Integer; cdecl;
    //function getLogoMarginLeft: Integer; cdecl;
    //function getLogoMarginRight: Integer; cdecl;
    //function getLogoMarginTop: Integer; cdecl;
    //function getWidth: Single; cdecl;
    //function getZoomRate: Single; cdecl;
    //procedure invalidate; cdecl;
    //function isAttributionEnabled: Boolean; cdecl;
    function isCompassEnabled: Boolean; cdecl;
    function isCompassFadeWhenFacingNorth: Boolean; cdecl;
    //function isDeselectMarkersOnTap: Boolean; cdecl;
    //function isDisableRotateWhenScaling: Boolean; cdecl;
    //function isDoubleTapGesturesEnabled: Boolean; cdecl;
    //function isFlingVelocityAnimationEnabled: Boolean; cdecl;
    //function isHorizontalScrollGesturesEnabled: Boolean; cdecl;
    //function isIncreaseRotateThresholdWhenScaling: Boolean; cdecl;//Deprecated
    //function isIncreaseScaleThresholdWhenRotating: Boolean; cdecl;
    //function isLogoEnabled: Boolean; cdecl;
    //function isQuickZoomGesturesEnabled: Boolean; cdecl;
    //function isRotateGesturesEnabled: Boolean; cdecl;
    //function isRotateVelocityAnimationEnabled: Boolean; cdecl;
    //function isScaleVelocityAnimationEnabled: Boolean; cdecl;
    //function isScrollGesturesEnabled: Boolean; cdecl;
    //function isTiltGesturesEnabled: Boolean; cdecl;
    //function isZoomGesturesEnabled: Boolean; cdecl;
    //procedure setAllGesturesEnabled(b: Boolean); cdecl;
    //procedure setAllVelocityAnimationsEnabled(b: Boolean); cdecl;
    //procedure setAttributionDialogManager(attributionDialogManager: JAttributionDialogManager); cdecl;
    //procedure setAttributionEnabled(b: Boolean); cdecl;
    //procedure setAttributionGravity(i: Integer); cdecl;
    //procedure setAttributionMargins(i: Integer; i1: Integer; i2: Integer; i3: Integer); cdecl;
    //procedure setAttributionTintColor(i: Integer); cdecl;
    procedure setCompassEnabled(compassEnabled: Boolean); cdecl;
    procedure setCompassFadeFacingNorth(b: Boolean); cdecl;
    procedure setCompassGravity(compassFadeFacingNorth: Integer); cdecl;
    procedure setCompassImage(compass: JDrawable); cdecl;
    procedure setCompassMargins(left: Integer; top: Integer; right: Integer; bottom: Integer); cdecl;
    //procedure setDeselectMarkersOnTap(b: Boolean); cdecl;
    //procedure setDisableRotateWhenScaling(b: Boolean); cdecl;
    //procedure setDoubleTapGesturesEnabled(b: Boolean); cdecl;
    //procedure setFlingAnimationBaseTime(l: Int64); cdecl;
    //procedure setFlingThreshold(l: Int64); cdecl;
    //procedure setFlingVelocityAnimationEnabled(b: Boolean); cdecl;
    //procedure setFocalPoint(pointF: JPointF); cdecl;
    //procedure setHorizontalScrollGesturesEnabled(b: Boolean); cdecl;
    //procedure setIncreaseRotateThresholdWhenScaling(b: Boolean); cdecl;//Deprecated
    //procedure setIncreaseScaleThresholdWhenRotating(b: Boolean); cdecl;
    //procedure setLogoEnabled(b: Boolean); cdecl;
    //procedure setLogoGravity(i: Integer); cdecl;
    //procedure setLogoMargins(i: Integer; i1: Integer; i2: Integer; i3: Integer); cdecl;
    //procedure setQuickZoomGesturesEnabled(b: Boolean); cdecl;
    //procedure setRotateGesturesEnabled(b: Boolean); cdecl;
    //procedure setRotateVelocityAnimationEnabled(b: Boolean); cdecl;
    //procedure setScaleVelocityAnimationEnabled(b: Boolean); cdecl;
    //procedure setScrollGesturesEnabled(b: Boolean); cdecl;
    //procedure setTiltGesturesEnabled(b: Boolean); cdecl;
    //procedure setZoomGesturesEnabled(b: Boolean); cdecl;
    //procedure setZoomRate(f: Single); cdecl;
  end;
  TJUiSettings = class(TJavaGenericImport<JUiSettingsClass, JUiSettings>) end;

  {*********************************************}
  JFeatureClass = interface(JObjectClass)
    ['{2B523FF0-23CD-4E88-8E57-08B9AB011EC0}']
    //{class} function fromGeometry(geometry: JGeometry): JFeature; cdecl; overload;
    //{class} function fromGeometry(geometry: JGeometry; boundingBox: JBoundingBox): JFeature; cdecl; overload;
    //{class} function fromGeometry(geometry: JGeometry; jsonObject: JObject): JFeature; cdecl; overload;
    //{class} function fromGeometry(geometry: JGeometry; jsonObject: JObject; boundingBox: JBoundingBox): JFeature; cdecl; overload;
    //{class} function fromGeometry(geometry: JGeometry; jsonObject: JObject; string_: JString): JFeature; cdecl; overload;
    //{class} function fromGeometry(geometry: JGeometry; jsonObject: JObject; string_: JString; boundingBox: JBoundingBox): JFeature; cdecl; overload;
    //{class} function fromJson(string_: JString): JFeature; cdecl;
    //{class} function typeAdapter(gson: JObject): JObject; cdecl;
  end;
  [JavaSignature('org/maplibre/geojson/Feature')]
  JFeature = interface(JObject)
    ['{CF71A529-1E39-4BC8-B100-AB1F088B2FDF}']
    //procedure addBooleanProperty(string_: JString; boolean: JBoolean); cdecl;
    //procedure addCharacterProperty(string_: JString; character: JCharacter); cdecl;
    //procedure addNumberProperty(string_: JString; number: JNumber); cdecl;
    //procedure addProperty(string_: JString; jsonElement: JObject); cdecl;
    //procedure addStringProperty(string_: JString; string_1: JString); cdecl;
    //function bbox: JBoundingBox; cdecl;
    //function equals(object_: JObject): Boolean; cdecl;
    //function geometry: JGeometry; cdecl;
    //function getBooleanProperty(key: JString): JBoolean; cdecl;
    //function getCharacterProperty(key: JString): JCharacter; cdecl;//Deprecated
    //function getNumberProperty(key: JString): JNumber; cdecl;
    //function getProperty(string_: JString): JObject; cdecl;
    function getStringProperty(key: JString): JString; cdecl;
    //function hasNonNullValueForProperty(string_: JString): Boolean; cdecl;
    function hasProperty(key: JString): Boolean; cdecl;
    //function hashCode: Integer; cdecl;
    //function id: JString; cdecl;
    //function properties: JObject; cdecl;
    //function removeProperty(string_: JString): JObject; cdecl;
    //function toJson: JString; cdecl;
    //function toString: JString; cdecl;
    //function &type: JString; cdecl;
  end;
  TJFeature = class(TJavaGenericImport<JFeatureClass, JFeature>) end;

  {*********************************************}
  JCameraPositionClass = interface(JObjectClass)
    ['{0D75D097-29D1-440D-9CE5-C075691863EF}']
    //{class} function _GetCREATOR: JParcelable_Creator; cdecl;
    //{class} function _GetCompanion: JCameraPosition_Companion; cdecl;
    //{class} function _GetDEFAULT: JCameraPosition; cdecl;
    //{class} function init(latLng: JLatLng; d: Double; d1: Double; d2: Double; d3: Double; d4: Double; d5: Double; d6: TJavaArray<Double>): JCameraPosition; cdecl; overload;
    //{class} function init(latLng: JLatLng; d: Double; d1: Double; d2: Double; d3: TJavaArray<Double>): JCameraPosition; cdecl; overload;
    //{class} function init(latLng: JLatLng; d: Double; d1: Double; d2: Double): JCameraPosition; cdecl; overload;
    //{class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    //{class} property Companion: JCameraPosition_Companion read _GetCompanion;
    //{class} property DEFAULT: JCameraPosition read _GetDEFAULT;
  end;
  [JavaSignature('org/maplibre/android/camera/CameraPosition')]
  JCameraPosition = interface(JObject)
    ['{9AFFB7A6-3340-45FE-BEB5-E24C240CA3DE}']
    function _Getbearing: Double; cdecl;
    //function _GetcenterAltitude: Double; cdecl;
    //function _Getfov: Double; cdecl;
    function _Getpadding: TJavaArray<Double>; cdecl;
    function _Getroll: Double; cdecl;
    function _Gettarget: JLatLng; cdecl;
    function _Gettilt: Double; cdecl;
    function _Getzoom: Double; cdecl;
    //function describeContents: Integer; cdecl;
    //function equals(other: JObject): Boolean; cdecl;
    //function hashCode: Integer; cdecl;
    //function toString: JString; cdecl;
    //procedure writeToParcel(&out: JParcel; flags: Integer); cdecl;
    property bearing: Double read _Getbearing;
    //property centerAltitude: Double read _GetcenterAltitude;
    //property fov: Double read _Getfov;
    property padding: TJavaArray<Double> read _Getpadding;
    property roll: Double read _Getroll;
    property target: JLatLng read _Gettarget;
    property tilt: Double read _Gettilt;
    property zoom: Double read _Getzoom;
  end;
  TJCameraPosition = class(TJavaGenericImport<JCameraPositionClass, JCameraPosition>) end;

  {*********************************************}
  JVisibleRegionClass = interface(JObjectClass)
    ['{9541B713-566A-4757-9717-BC32D755EC3A}']
    //{class} function _GetCREATOR: JParcelable_Creator; cdecl;
    //{class} function _GetCompanion: JVisibleRegion_Companion; cdecl;
    //{class} function init(latLng: JLatLng; latLng1: JLatLng; latLng2: JLatLng; latLng3: JLatLng; latLngBounds: JLatLngBounds): JVisibleRegion; cdecl;
    //{class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    //{class} property Companion: JVisibleRegion_Companion read _GetCompanion;
  end;
  [JavaSignature('org/maplibre/android/geometry/VisibleRegion')]
  JVisibleRegion = interface(JObject)
    ['{45B88660-F48E-4158-A20B-459F7A22E01B}']
    //function _GetfarLeft: JLatLng; cdecl;
    //function _GetfarRight: JLatLng; cdecl;
    function _GetlatLngBounds: JLatLngBounds; cdecl;
    //function _GetnearLeft: JLatLng; cdecl;
    //function _GetnearRight: JLatLng; cdecl;
    //function describeContents: Integer; cdecl;
    //function equals(other: JObject): Boolean; cdecl;
    //function hashCode: Integer; cdecl;
    //function toString: JString; cdecl;
    //procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
    //property farLeft: JLatLng read _GetfarLeft;
    //property farRight: JLatLng read _GetfarRight;
    property latLngBounds: JLatLngBounds read _GetlatLngBounds;
    //property nearLeft: JLatLng read _GetnearLeft;
    //property nearRight: JLatLng read _GetnearRight;
  end;
  TJVisibleRegion = class(TJavaGenericImport<JVisibleRegionClass, JVisibleRegion>) end;

  {*********************************************}
  JProjectionClass = interface(JObjectClass)
    ['{3AED82D0-D2E4-4C87-A0B3-73CF875AEEC8}']
  end;
  [JavaSignature('org/maplibre/android/maps/Projection')]
  JProjection = interface(JObject)
    ['{0C748462-CB44-48BC-8218-A138618F0F58}']
    //function calculateZoom(minScale: Single): Double; cdecl;
    //function fromScreenLocation(point: JPointF): JLatLng; cdecl;
    //procedure fromScreenLocations(input: TJavaArray<Double>; output: TJavaArray<Double>); cdecl;
    //function getLatLngForProjectedMeters(projectedMeters: JProjectedMeters): JLatLng; cdecl;
    //function getMetersPerPixelAtLatitude(latitude: Double): Double; cdecl;
    //function getProjectedMetersForLatLng(latLng: JLatLng): JProjectedMeters; cdecl;
    //procedure getVisibleCoordinateBounds(bounds: TJavaArray<Double>); cdecl;
    function getVisibleRegion: JVisibleRegion; cdecl; overload;
    function getVisibleRegion(ignorePadding: Boolean): JVisibleRegion; cdecl; overload;
    //procedure invalidateContentPadding; cdecl; Deprecated
    function toScreenLocation(location: JLatLng): JPointF; cdecl;
    //procedure toScreenLocations(input: TJavaArray<Double>; output: TJavaArray<Double>); cdecl;
  end;
  TJProjection = class(TJavaGenericImport<JProjectionClass, JProjection>) end;

  {****************************************}
  JExpressionClass = interface(JObjectClass)
    ['{6CA759F4-C9D7-4C49-899E-2F29CC690F20}']
    //{class} function init(string_: JString; expression: TJavaObjectArray<JExpression>): JExpression; cdecl;
    //{class} function abs(expression: JExpression): JExpression; cdecl; overload;
    //{class} function abs(number: JNumber): JExpression; cdecl; overload;
    //{class} function accumulated: JExpression; cdecl;
    //{class} function acos(expression: JExpression): JExpression; cdecl; overload;
    //{class} function acos(number: JNumber): JExpression; cdecl; overload;
    //{class} function all(expression: TJavaObjectArray<JExpression>): JExpression; cdecl;
    //{class} function any(expression: TJavaObjectArray<JExpression>): JExpression; cdecl;
    //{class} function &array(expression: JExpression): JExpression; cdecl;
    //{class} function asin(expression: JExpression): JExpression; cdecl; overload;
    //{class} function asin(number: JNumber): JExpression; cdecl; overload;
    //{class} function at(expression: JExpression; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function at(number: JNumber; expression: JExpression): JExpression; cdecl; overload;
    //{class} function atan(expression: JExpression): JExpression; cdecl; overload;
    //{class} function atan(number: JNumber): JExpression; cdecl; overload;
    //{class} function bool(expression: TJavaObjectArray<JExpression>): JExpression; cdecl;
    //{class} function ceil(expression: JExpression): JExpression; cdecl; overload;
    //{class} function ceil(number: JNumber): JExpression; cdecl; overload;
    //{class} function coalesce(expression: TJavaObjectArray<JExpression>): JExpression; cdecl;
    //{class} function collator(b: Boolean; b1: Boolean; locale: JLocale): JExpression; cdecl; overload;
    //{class} function collator(b: Boolean; b1: Boolean): JExpression; cdecl; overload;
    //{class} function collator(expression: JExpression; expression1: JExpression; expression2: JExpression): JExpression; cdecl; overload;
    //{class} function collator(expression: JExpression; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function color(i: Integer): JExpression; cdecl;
    //{class} function concat(expression: TJavaObjectArray<JExpression>): JExpression; cdecl; overload;
    //{class} function concat(string_: TJavaObjectArray<JString>): JExpression; cdecl; overload;
    //{class} function cos(expression: JExpression): JExpression; cdecl; overload;
    //{class} function cos(number: JNumber): JExpression; cdecl; overload;
    //{class} function cubicBezier(expression: JExpression; expression1: JExpression; expression2: JExpression; expression3: JExpression): JExpression_Interpolator; cdecl; overload;
    //{class} function cubicBezier(number: JNumber; number1: JNumber; number2: JNumber; number3: JNumber): JExpression_Interpolator; cdecl; overload;
    //{class} function distance(geoJson: JObject): JExpression; cdecl;
    //{class} function division(expression: JExpression; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function division(number: JNumber; number1: JNumber): JExpression; cdecl; overload;
    //{class} function downcase(expression: JExpression): JExpression; cdecl; overload;
    //{class} function downcase(string_: JString): JExpression; cdecl; overload;
    //{class} function e: JExpression; cdecl;
    //{class} function eq(expression: JExpression; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function eq(expression: JExpression; expression1: JExpression; expression2: JExpression): JExpression; cdecl; overload;
    //{class} function eq(expression: JExpression; b: Boolean): JExpression; cdecl; overload;
    //{class} function eq(expression: JExpression; string_: JString): JExpression; cdecl; overload;
    //{class} function eq(expression: JExpression; string_: JString; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function eq(expression: JExpression; number: JNumber): JExpression; cdecl; overload;
    //{class} function exponential(number: JNumber): JExpression_Interpolator; cdecl; overload;
    //{class} function exponential(expression: JExpression): JExpression_Interpolator; cdecl; overload;
    //{class} function featureState(string_: JString): JExpression; cdecl;
    //{class} function floor(expression: JExpression): JExpression; cdecl; overload;
    //{class} function floor(number: JNumber): JExpression; cdecl; overload;
    //{class} function format(formatEntry: TJavaObjectArray<JExpression_FormatEntry>): JExpression; cdecl;
    //{class} function formatEntry(expression: JExpression; formatOption: TJavaObjectArray<JExpression_FormatOption>): JExpression_FormatEntry; cdecl; overload;
    //{class} function formatEntry(expression: JExpression): JExpression_FormatEntry; cdecl; overload;
    //{class} function formatEntry(string_: JString; formatOption: TJavaObjectArray<JExpression_FormatOption>): JExpression_FormatEntry; cdecl; overload;
    //{class} function formatEntry(string_: JString): JExpression_FormatEntry; cdecl; overload;
    //{class} function geometryType: JExpression; cdecl;
    //{class} function &get(expression: JExpression): JExpression; cdecl; overload;
    {class} function &get(input: JString): JExpression; cdecl; overload;
    //{class} function &get(expression: JExpression; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function &get(string_: JString; expression: JExpression): JExpression; cdecl; overload;
    //{class} function gt(expression: JExpression; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function gt(expression: JExpression; expression1: JExpression; expression2: JExpression): JExpression; cdecl; overload;
    //{class} function gt(expression: JExpression; number: JNumber): JExpression; cdecl; overload;
    //{class} function gt(expression: JExpression; string_: JString): JExpression; cdecl; overload;
    //{class} function gt(expression: JExpression; string_: JString; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function gte(expression: JExpression; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function gte(expression: JExpression; expression1: JExpression; expression2: JExpression): JExpression; cdecl; overload;
    //{class} function gte(expression: JExpression; number: JNumber): JExpression; cdecl; overload;
    //{class} function gte(expression: JExpression; string_: JString): JExpression; cdecl; overload;
    //{class} function gte(expression: JExpression; string_: JString; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function has(expression: JExpression): JExpression; cdecl; overload;
    //{class} function has(string_: JString): JExpression; cdecl; overload;
    //{class} function has(expression: JExpression; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function has(string_: JString; expression: JExpression): JExpression; cdecl; overload;
    //{class} function heatmapDensity: JExpression; cdecl;
    //{class} function id: JExpression; cdecl;
    //{class} function image(expression: JExpression): JExpression; cdecl;
    //{class} function &in(expression: JExpression; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function &in(number: JNumber; expression: JExpression): JExpression; cdecl; overload;
    //{class} function &in(string_: JString; expression: JExpression): JExpression; cdecl; overload;
    //{class} function indexOf(expression: JExpression; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function indexOf(expression: JExpression; expression1: JExpression; expression2: JExpression): JExpression; cdecl; overload;
    //{class} function interpolate(interpolator: JExpression_Interpolator; expression: JExpression; expression1: TJavaObjectArray<JExpression>): JExpression; cdecl; overload;
    //{class} function interpolate(interpolator: JExpression_Interpolator; expression: JExpression; stop: TJavaObjectArray<JExpression_Stop>): JExpression; cdecl; overload;
    //{class} function isSupportedScript(expression: JExpression): JExpression; cdecl; overload;
    //{class} function isSupportedScript(string_: JString): JExpression; cdecl; overload;
    //{class} function length(expression: JExpression): JExpression; cdecl; overload;
    //{class} function length(string_: JString): JExpression; cdecl; overload;
    //{class} function let(expression: TJavaObjectArray<JExpression>): JExpression; cdecl;
    //{class} function lineProgress: JExpression; cdecl;
    //{class} function linear: JExpression_Interpolator; cdecl;
    //{class} function literal(number: JNumber): JExpression; cdecl; overload;
    //{class} function literal(string_: JString): JExpression; cdecl; overload;
    //{class} function literal(b: Boolean): JExpression; cdecl; overload;
    //{class} function literal(object_: JObject): JExpression; cdecl; overload;
    //{class} function literal(object_: TJavaObjectArray<JObject>): JExpression; cdecl; overload;
    //{class} function ln(expression: JExpression): JExpression; cdecl; overload;
    //{class} function ln(number: JNumber): JExpression; cdecl; overload;
    //{class} function ln2: JExpression; cdecl;
    //{class} function log10(expression: JExpression): JExpression; cdecl; overload;
    //{class} function log10(number: JNumber): JExpression; cdecl; overload;
    //{class} function log2(expression: JExpression): JExpression; cdecl; overload;
    //{class} function log2(number: JNumber): JExpression; cdecl; overload;
    //{class} function lt(expression: JExpression; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function lt(expression: JExpression; expression1: JExpression; expression2: JExpression): JExpression; cdecl; overload;
    //{class} function lt(expression: JExpression; number: JNumber): JExpression; cdecl; overload;
    //{class} function lt(expression: JExpression; string_: JString): JExpression; cdecl; overload;
    //{class} function lt(expression: JExpression; string_: JString; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function lte(expression: JExpression; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function lte(expression: JExpression; expression1: JExpression; expression2: JExpression): JExpression; cdecl; overload;
    //{class} function lte(expression: JExpression; number: JNumber): JExpression; cdecl; overload;
    //{class} function lte(expression: JExpression; string_: JString): JExpression; cdecl; overload;
    //{class} function lte(expression: JExpression; string_: JString; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function match(expression: TJavaObjectArray<JExpression>): JExpression; cdecl; overload;
    //{class} function match(expression: JExpression; expression1: JExpression; stop: TJavaObjectArray<JExpression_Stop>): JExpression; cdecl; overload;
    //{class} function max(expression: TJavaObjectArray<JExpression>): JExpression; cdecl; overload;
    //{class} function max(number: TJavaObjectArray<JNumber>): JExpression; cdecl; overload;
    //{class} function min(expression: TJavaObjectArray<JExpression>): JExpression; cdecl; overload;
    //{class} function min(number: TJavaObjectArray<JNumber>): JExpression; cdecl; overload;
    //{class} function &mod(expression: JExpression; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function &mod(number: JNumber; number1: JNumber): JExpression; cdecl; overload;
    //{class} function neq(expression: JExpression; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function neq(expression: JExpression; expression1: JExpression; expression2: JExpression): JExpression; cdecl; overload;
    //{class} function neq(expression: JExpression; b: Boolean): JExpression; cdecl; overload;
    //{class} function neq(expression: JExpression; string_: JString): JExpression; cdecl; overload;
    //{class} function neq(expression: JExpression; string_: JString; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function neq(expression: JExpression; number: JNumber): JExpression; cdecl; overload;
    //{class} function &not(expression: JExpression): JExpression; cdecl; overload;
    //{class} function &not(b: Boolean): JExpression; cdecl; overload;
    //{class} function number(expression: TJavaObjectArray<JExpression>): JExpression; cdecl;
    //{class} function numberFormat(expression: JExpression; numberFormatOption: TJavaObjectArray<JExpression_NumberFormatOption>): JExpression; cdecl; overload;
    //{class} function numberFormat(number: JNumber; numberFormatOption: TJavaObjectArray<JExpression_NumberFormatOption>): JExpression; cdecl; overload;
    //{class} function &object(expression: JExpression): JExpression; cdecl;
    //{class} function pi: JExpression; cdecl;
    //{class} function pow(expression: JExpression; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function pow(number: JNumber; number1: JNumber): JExpression; cdecl; overload;
    //{class} function product(expression: TJavaObjectArray<JExpression>): JExpression; cdecl; overload;
    //{class} function product(number: TJavaObjectArray<JNumber>): JExpression; cdecl; overload;
    //{class} function properties: JExpression; cdecl;
    {class} function raw(rawExpression: JString): JExpression; cdecl;
    //{class} function resolvedLocale(expression: JExpression): JExpression; cdecl;
    //{class} function rgb(expression: JExpression; expression1: JExpression; expression2: JExpression): JExpression; cdecl; overload;
    //{class} function rgb(number: JNumber; number1: JNumber; number2: JNumber): JExpression; cdecl; overload;
    //{class} function rgba(expression: JExpression; expression1: JExpression; expression2: JExpression; expression3: JExpression): JExpression; cdecl; overload;
    //{class} function rgba(number: JNumber; number1: JNumber; number2: JNumber; number3: JNumber): JExpression; cdecl; overload;
    //{class} function round(expression: JExpression): JExpression; cdecl; overload;
    //{class} function round(number: JNumber): JExpression; cdecl; overload;
    //{class} function sin(expression: JExpression): JExpression; cdecl; overload;
    //{class} function sin(number: JNumber): JExpression; cdecl; overload;
    //{class} function slice(expression: JExpression; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function slice(expression: JExpression; expression1: JExpression; expression2: JExpression): JExpression; cdecl; overload;
    //{class} function sqrt(expression: JExpression): JExpression; cdecl; overload;
    //{class} function sqrt(number: JNumber): JExpression; cdecl; overload;
    //{class} function step(number: JNumber; expression: JExpression; expression1: TJavaObjectArray<JExpression>): JExpression; cdecl; overload;
    //{class} function step(expression: JExpression; expression1: JExpression; expression2: TJavaObjectArray<JExpression>): JExpression; cdecl; overload;
    //{class} function step(number: JNumber; expression: JExpression; stop: TJavaObjectArray<JExpression_Stop>): JExpression; cdecl; overload;
    //{class} function step(expression: JExpression; expression1: JExpression; stop: TJavaObjectArray<JExpression_Stop>): JExpression; cdecl; overload;
    //{class} function step(number: JNumber; number1: JNumber; expression: TJavaObjectArray<JExpression>): JExpression; cdecl; overload;
    //{class} function step(expression: JExpression; number: JNumber; expression1: TJavaObjectArray<JExpression>): JExpression; cdecl; overload;
    //{class} function step(number: JNumber; number1: JNumber; stop: TJavaObjectArray<JExpression_Stop>): JExpression; cdecl; overload;
    //{class} function step(expression: JExpression; number: JNumber; stop: TJavaObjectArray<JExpression_Stop>): JExpression; cdecl; overload;
    //{class} function stop(object_: JObject; object_1: JObject): JExpression_Stop; cdecl;
    //{class} function &string(expression: TJavaObjectArray<JExpression>): JExpression; cdecl;
    //{class} function subtract(expression: JExpression): JExpression; cdecl; overload;
    //{class} function subtract(number: JNumber): JExpression; cdecl; overload;
    //{class} function subtract(expression: JExpression; expression1: JExpression): JExpression; cdecl; overload;
    //{class} function subtract(number: JNumber; number1: JNumber): JExpression; cdecl; overload;
    //{class} function sum(expression: TJavaObjectArray<JExpression>): JExpression; cdecl; overload;
    //{class} function sum(number: TJavaObjectArray<JNumber>): JExpression; cdecl; overload;
    //{class} function switchCase(expression: TJavaObjectArray<JExpression>): JExpression; cdecl;
    //{class} function tan(expression: JExpression): JExpression; cdecl; overload;
    //{class} function tan(number: JNumber): JExpression; cdecl; overload;
    //{class} function toBool(expression: JExpression): JExpression; cdecl;
    //{class} function toColor(expression: JExpression): JExpression; cdecl;
    //{class} function toNumber(expression: JExpression): JExpression; cdecl;
    //{class} function toPadding(expression: JExpression): JExpression; cdecl;
    //{class} function toRgba(expression: JExpression): JExpression; cdecl;
    //{class} function toString(expression: JExpression): JExpression; cdecl; overload;
    //{class} function typeOf(expression: JExpression): JExpression; cdecl;
    //{class} function upcase(expression: JExpression): JExpression; cdecl; overload;
    //{class} function upcase(string_: JString): JExpression; cdecl; overload;
    //{class} function var(expression: JExpression): JExpression; cdecl; overload;
    //{class} function var(string_: JString): JExpression; cdecl; overload;
    //{class} function within(polygon: JObject): JExpression; cdecl;
    //{class} function zoom: JExpression; cdecl;
  end;
  [JavaSignature('org/maplibre/android/style/expressions/Expression')]
  JExpression = interface(JObject)
    ['{2DE24266-C5FA-402E-8307-18C718E9FEBE}']
    //function equals(object_: JObject): Boolean; cdecl;
    //function hashCode: Integer; cdecl;
    //function toArray: TJavaObjectArray<JObject>; cdecl;
    //function toString: JString; cdecl; overload;
  end;
  TJExpression = class(TJavaGenericImport<JExpressionClass, JExpression>) end;

  {*********************************************}
  JPropertyValueClass = interface(JObjectClass)
    ['{AC424892-9691-4E67-9AB3-9CB116F6CAB9}']
    //{class} function init(string_: JString; object_: JObject): JPropertyValue; cdecl;
  end;
  [JavaSignature('org/maplibre/android/style/layers/PropertyValue')]
  JPropertyValue = interface(JObject)
    ['{4AB50FE4-234E-4848-93B9-31F9C1FE9B02}']
    //function _Getname: JString; cdecl;
    //function _Getvalue: JObject; cdecl;
    //function equals(object_: JObject): Boolean; cdecl;
    //function getColorInt: JInteger; cdecl;
    //function getExpression: JExpression; cdecl;
    //function getValue: JObject; cdecl;
    //function hashCode: Integer; cdecl;
    //function isExpression: Boolean; cdecl;
    //function isNull: Boolean; cdecl;
    //function isValue: Boolean; cdecl;
    //function toString: JString; cdecl;
    //property name: JString read _Getname;
    //property value: JObject read _Getvalue;
  end;
  TJPropertyValue = class(TJavaGenericImport<JPropertyValueClass, JPropertyValue>) end;

  {*********************************************}
  JPropertyFactoryClass = interface(JObjectClass)
    ['{608444A7-923B-4062-965B-F1787D1DC6CC}']
    //{class} function init: JPropertyFactory; cdecl;
    {class} function backgroundColor(value: Integer): JPropertyValue; cdecl; overload;
    {class} function backgroundColor(value: JString): JPropertyValue; cdecl; overload;
    {class} function backgroundColor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function backgroundOpacity(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function backgroundOpacity(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function backgroundPattern(value: JString): JPropertyValue; cdecl; overload;
    {class} function backgroundPattern(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function circleBlur(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function circleBlur(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function circleColor(value: Integer): JPropertyValue; cdecl; overload;
    {class} function circleColor(value: JString): JPropertyValue; cdecl; overload;
    {class} function circleColor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function circleOpacity(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function circleOpacity(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function circlePitchAlignment(value: JString): JPropertyValue; cdecl; overload;
    {class} function circlePitchAlignment(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function circlePitchScale(value: JString): JPropertyValue; cdecl; overload;
    {class} function circlePitchScale(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function circleRadius(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function circleRadius(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function circleSortKey(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function circleSortKey(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function circleStrokeColor(value: Integer): JPropertyValue; cdecl; overload;
    {class} function circleStrokeColor(value: JString): JPropertyValue; cdecl; overload;
    {class} function circleStrokeColor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function circleStrokeOpacity(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function circleStrokeOpacity(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function circleStrokeWidth(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function circleStrokeWidth(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function circleTranslate(value: TJavaObjectArray<JFloat>): JPropertyValue; cdecl; overload;
    {class} function circleTranslate(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function circleTranslateAnchor(value: JString): JPropertyValue; cdecl; overload;
    {class} function circleTranslateAnchor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function colorReliefColor(value: Integer): JPropertyValue; cdecl; overload;
    {class} function colorReliefColor(value: JString): JPropertyValue; cdecl; overload;
    {class} function colorReliefColor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function colorReliefOpacity(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function colorReliefOpacity(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function fillAntialias(value: JBoolean): JPropertyValue; cdecl; overload;
    {class} function fillAntialias(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function fillColor(value: Integer): JPropertyValue; cdecl; overload;
    {class} function fillColor(value: JString): JPropertyValue; cdecl; overload;
    {class} function fillColor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function fillExtrusionBase(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function fillExtrusionBase(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function fillExtrusionColor(value: Integer): JPropertyValue; cdecl; overload;
    {class} function fillExtrusionColor(value: JString): JPropertyValue; cdecl; overload;
    {class} function fillExtrusionColor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function fillExtrusionHeight(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function fillExtrusionHeight(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function fillExtrusionOpacity(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function fillExtrusionOpacity(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function fillExtrusionPattern(value: JString): JPropertyValue; cdecl; overload;
    {class} function fillExtrusionPattern(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function fillExtrusionRoundedCornerDistance(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function fillExtrusionRoundedCornerDistance(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function fillExtrusionTranslate(value: TJavaObjectArray<JFloat>): JPropertyValue; cdecl; overload;
    {class} function fillExtrusionTranslate(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function fillExtrusionTranslateAnchor(value: JString): JPropertyValue; cdecl; overload;
    {class} function fillExtrusionTranslateAnchor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function fillExtrusionVerticalGradient(value: JBoolean): JPropertyValue; cdecl; overload;
    {class} function fillExtrusionVerticalGradient(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function fillOpacity(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function fillOpacity(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function fillOutlineColor(value: Integer): JPropertyValue; cdecl; overload;
    {class} function fillOutlineColor(value: JString): JPropertyValue; cdecl; overload;
    {class} function fillOutlineColor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function fillPattern(value: JString): JPropertyValue; cdecl; overload;
    {class} function fillPattern(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function fillSortKey(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function fillSortKey(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function fillTranslate(value: TJavaObjectArray<JFloat>): JPropertyValue; cdecl; overload;
    {class} function fillTranslate(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function fillTranslateAnchor(value: JString): JPropertyValue; cdecl; overload;
    {class} function fillTranslateAnchor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function heatmapColor(value: Integer): JPropertyValue; cdecl; overload;
    {class} function heatmapColor(value: JString): JPropertyValue; cdecl; overload;
    {class} function heatmapColor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function heatmapIntensity(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function heatmapIntensity(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function heatmapOpacity(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function heatmapOpacity(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function heatmapRadius(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function heatmapRadius(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function heatmapWeight(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function heatmapWeight(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function hillshadeAccentColor(value: Integer): JPropertyValue; cdecl; overload;
    {class} function hillshadeAccentColor(value: JString): JPropertyValue; cdecl; overload;
    {class} function hillshadeAccentColor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function hillshadeExaggeration(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function hillshadeExaggeration(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function hillshadeHighlightColor(value: TJavaObjectArray<JString>): JPropertyValue; cdecl; overload;
    {class} function hillshadeHighlightColor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function hillshadeIlluminationAltitude(value: TJavaObjectArray<JFloat>): JPropertyValue; cdecl; overload;
    {class} function hillshadeIlluminationAltitude(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function hillshadeIlluminationAnchor(value: JString): JPropertyValue; cdecl; overload;
    {class} function hillshadeIlluminationAnchor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function hillshadeIlluminationDirection(value: TJavaObjectArray<JFloat>): JPropertyValue; cdecl; overload;
    {class} function hillshadeIlluminationDirection(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function hillshadeMethod(value: JString): JPropertyValue; cdecl; overload;
    {class} function hillshadeMethod(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function hillshadeShadowColor(value: TJavaObjectArray<JString>): JPropertyValue; cdecl; overload;
    {class} function hillshadeShadowColor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconAllowOverlap(value: JBoolean): JPropertyValue; cdecl; overload;
    {class} function iconAllowOverlap(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconAnchor(value: JString): JPropertyValue; cdecl; overload;
    {class} function iconAnchor(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconColor(value: Integer): JPropertyValue; cdecl; overload;
    {class} function iconColor(value: JString): JPropertyValue; cdecl; overload;
    {class} function iconColor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconHaloBlur(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function iconHaloBlur(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconHaloColor(value: Integer): JPropertyValue; cdecl; overload;
    {class} function iconHaloColor(value: JString): JPropertyValue; cdecl; overload;
    {class} function iconHaloColor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconHaloWidth(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function iconHaloWidth(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconIgnorePlacement(value: JBoolean): JPropertyValue; cdecl; overload;
    {class} function iconIgnorePlacement(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconImage(value: JString): JPropertyValue; cdecl; overload;
    {class} function iconImage(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconKeepUpright(value: JBoolean): JPropertyValue; cdecl; overload;
    {class} function iconKeepUpright(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconOffset(value: TJavaObjectArray<JFloat>): JPropertyValue; cdecl; overload;
    {class} function iconOffset(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconOpacity(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function iconOpacity(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconOptional(value: JBoolean): JPropertyValue; cdecl; overload;
    {class} function iconOptional(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconPadding(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function iconPadding(value: TJavaObjectArray<JFloat>): JPropertyValue; cdecl; overload;
    {class} function iconPadding(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconPitchAlignment(value: JString): JPropertyValue; cdecl; overload;
    {class} function iconPitchAlignment(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconRotate(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function iconRotate(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconRotationAlignment(value: JString): JPropertyValue; cdecl; overload;
    {class} function iconRotationAlignment(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconSize(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function iconSize(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconTextFit(value: JString): JPropertyValue; cdecl; overload;
    {class} function iconTextFit(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconTextFitPadding(value: TJavaObjectArray<JFloat>): JPropertyValue; cdecl; overload;
    {class} function iconTextFitPadding(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconTranslate(value: TJavaObjectArray<JFloat>): JPropertyValue; cdecl; overload;
    {class} function iconTranslate(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function iconTranslateAnchor(value: JString): JPropertyValue; cdecl; overload;
    {class} function iconTranslateAnchor(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function lineBlur(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function lineBlur(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function lineCap(value: JString): JPropertyValue; cdecl; overload;
    {class} function lineCap(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function lineColor(value: Integer): JPropertyValue; cdecl; overload;
    {class} function lineColor(value: JString): JPropertyValue; cdecl; overload;
    {class} function lineColor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function lineDasharray(value: TJavaObjectArray<JFloat>): JPropertyValue; cdecl; overload;
    {class} function lineDasharray(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function lineGapWidth(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function lineGapWidth(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function lineGradient(value: Integer): JPropertyValue; cdecl; overload;
    {class} function lineGradient(value: JString): JPropertyValue; cdecl; overload;
    {class} function lineGradient(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function lineJoin(value: JString): JPropertyValue; cdecl; overload;
    {class} function lineJoin(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function lineMiterLimit(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function lineMiterLimit(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function lineOffset(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function lineOffset(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function lineOpacity(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function lineOpacity(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function linePattern(value: JString): JPropertyValue; cdecl; overload;
    {class} function linePattern(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function lineRoundLimit(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function lineRoundLimit(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function lineSortKey(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function lineSortKey(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function lineTranslate(value: TJavaObjectArray<JFloat>): JPropertyValue; cdecl; overload;
    {class} function lineTranslate(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function lineTranslateAnchor(value: JString): JPropertyValue; cdecl; overload;
    {class} function lineTranslateAnchor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function lineWidth(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function lineWidth(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function rasterBrightnessMax(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function rasterBrightnessMax(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function rasterBrightnessMin(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function rasterBrightnessMin(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function rasterContrast(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function rasterContrast(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function rasterFadeDuration(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function rasterFadeDuration(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function rasterHueRotate(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function rasterHueRotate(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function rasterOpacity(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function rasterOpacity(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function rasterResampling(value: JString): JPropertyValue; cdecl; overload;
    {class} function rasterResampling(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function rasterSaturation(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function rasterSaturation(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function symbolAvoidEdges(value: JBoolean): JPropertyValue; cdecl; overload;
    {class} function symbolAvoidEdges(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function symbolPlacement(value: JString): JPropertyValue; cdecl; overload;
    {class} function symbolPlacement(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function symbolScreenSpace(value: JBoolean): JPropertyValue; cdecl; overload;
    {class} function symbolScreenSpace(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function symbolSortKey(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function symbolSortKey(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function symbolSpacing(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function symbolSpacing(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function symbolZOrder(value: JString): JPropertyValue; cdecl; overload;
    {class} function symbolZOrder(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textAllowOverlap(value: JBoolean): JPropertyValue; cdecl; overload;
    {class} function textAllowOverlap(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textAnchor(value: JString): JPropertyValue; cdecl; overload;
    {class} function textAnchor(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textColor(value: Integer): JPropertyValue; cdecl; overload;
    {class} function textColor(value: JString): JPropertyValue; cdecl; overload;
    {class} function textColor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function textField(value: JString): JPropertyValue; cdecl; overload;
    //{class} function textField(value: JFormatted): JPropertyValue; cdecl; overload;
    {class} function textField(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textFont(value: TJavaObjectArray<JString>): JPropertyValue; cdecl; overload;
    {class} function textFont(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textHaloBlur(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function textHaloBlur(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function textHaloColor(value: Integer): JPropertyValue; cdecl; overload;
    {class} function textHaloColor(value: JString): JPropertyValue; cdecl; overload;
    {class} function textHaloColor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function textHaloWidth(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function textHaloWidth(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function textIgnorePlacement(value: JBoolean): JPropertyValue; cdecl; overload;
    {class} function textIgnorePlacement(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textJustify(value: JString): JPropertyValue; cdecl; overload;
    {class} function textJustify(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textKeepUpright(value: JBoolean): JPropertyValue; cdecl; overload;
    {class} function textKeepUpright(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textLetterSpacing(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function textLetterSpacing(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textLineHeight(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function textLineHeight(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textMaxAngle(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function textMaxAngle(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textMaxWidth(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function textMaxWidth(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textOffset(value: TJavaObjectArray<JFloat>): JPropertyValue; cdecl; overload;
    {class} function textOffset(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textOpacity(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function textOpacity(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textOptional(value: JBoolean): JPropertyValue; cdecl; overload;
    {class} function textOptional(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textPadding(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function textPadding(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textPitchAlignment(value: JString): JPropertyValue; cdecl; overload;
    {class} function textPitchAlignment(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textRadialOffset(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function textRadialOffset(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textRotate(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function textRotate(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textRotationAlignment(value: JString): JPropertyValue; cdecl; overload;
    {class} function textRotationAlignment(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textSize(value: JFloat): JPropertyValue; cdecl; overload;
    {class} function textSize(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textTransform(value: JString): JPropertyValue; cdecl; overload;
    {class} function textTransform(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textTranslate(value: TJavaObjectArray<JFloat>): JPropertyValue; cdecl; overload;
    {class} function textTranslate(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textTranslateAnchor(value: JString): JPropertyValue; cdecl; overload;
    {class} function textTranslateAnchor(expression: JExpression): JPropertyValue; cdecl; overload;
    {class} function textVariableAnchor(value: TJavaObjectArray<JString>): JPropertyValue; cdecl; overload;
    {class} function textVariableAnchor(value: JExpression): JPropertyValue; cdecl; overload;
    //{class} function textVariableAnchorOffset(value: TJavaObjectArray<JObject>): JPropertyValue; cdecl; overload;
    //{class} function textVariableAnchorOffset(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function textWritingMode(value: TJavaObjectArray<JString>): JPropertyValue; cdecl; overload;
    {class} function textWritingMode(value: JExpression): JPropertyValue; cdecl; overload;
    {class} function visibility(value: JString): JPropertyValue; cdecl;
  end;
  [JavaSignature('org/maplibre/android/style/layers/PropertyFactory')]
  JPropertyFactory = interface(JObject)
    ['{8EB35DE4-4BBD-40FD-8F42-F5A9EFD14FFE}']
  end;
  TJPropertyFactory = class(TJavaGenericImport<JPropertyFactoryClass, JPropertyFactory>) end;

  {***********************************}
  JLayerClass = interface(JObjectClass)
    ['{C982F318-76DF-4816-B05E-23E8984450A1}']
    //{class} function init: JLayer; cdecl;
  end;
  [JavaSignature('org/maplibre/android/style/layers/Layer')]
  JLayer = interface(JObject)
    ['{A0518CEC-1139-445B-9441-73197C4E850F}']
    //function getId: JString; cdecl;
    //function getMaxZoom: Single; cdecl;
    //function getMinZoom: Single; cdecl;
    //function getNativePtr: Int64; cdecl;
    //function getVisibility: JPropertyValue; cdecl;
    //function isDetached: Boolean; cdecl;
    //procedure setDetached; cdecl;
    //procedure setMaxZoom(f: Single); cdecl;
    //procedure setMinZoom(f: Single); cdecl;
    procedure setProperties(properties: TJavaObjectArray<JPropertyValue>); cdecl;
  end;
  TJLayer = class(TJavaGenericImport<JLayerClass, JLayer>) end;

  {****************************************}
  JSymbolLayerClass = interface(JLayerClass)
    ['{03F54BF9-D9F1-4928-A97A-64A301BA1356}']
    {class} function init(layerId: JString; sourceId: JString): JSymbolLayer; cdecl;
  end;
  [JavaSignature('org/maplibre/android/style/layers/SymbolLayer')]
  JSymbolLayer = interface(JLayer)
    ['{A3EDBD96-65C7-4A68-B461-97B56C73DC8D}']
    //function getFilter: JExpression; cdecl;
    //function getIconAllowOverlap: JPropertyValue; cdecl;
    //function getIconAnchor: JPropertyValue; cdecl;
    //function getIconColor: JPropertyValue; cdecl;
    //function getIconColorAsInt: Integer; cdecl;
    //function getIconColorTransition: JTransitionOptions; cdecl;
    //function getIconHaloBlur: JPropertyValue; cdecl;
    //function getIconHaloBlurTransition: JTransitionOptions; cdecl;
    //function getIconHaloColor: JPropertyValue; cdecl;
    //function getIconHaloColorAsInt: Integer; cdecl;
    //function getIconHaloColorTransition: JTransitionOptions; cdecl;
    //function getIconHaloWidth: JPropertyValue; cdecl;
    //function getIconHaloWidthTransition: JTransitionOptions; cdecl;
    //function getIconIgnorePlacement: JPropertyValue; cdecl;
    //function getIconImage: JPropertyValue; cdecl;
    //function getIconKeepUpright: JPropertyValue; cdecl;
    //function getIconOffset: JPropertyValue; cdecl;
    //function getIconOpacity: JPropertyValue; cdecl;
    //function getIconOpacityTransition: JTransitionOptions; cdecl;
    //function getIconOptional: JPropertyValue; cdecl;
    //function getIconPadding: JPropertyValue; cdecl;
    //function getIconPitchAlignment: JPropertyValue; cdecl;
    //function getIconRotate: JPropertyValue; cdecl;
    //function getIconRotationAlignment: JPropertyValue; cdecl;
    //function getIconSize: JPropertyValue; cdecl;
    //function getIconTextFit: JPropertyValue; cdecl;
    //function getIconTextFitPadding: JPropertyValue; cdecl;
    //function getIconTranslate: JPropertyValue; cdecl;
    //function getIconTranslateAnchor: JPropertyValue; cdecl;
    //function getIconTranslateTransition: JTransitionOptions; cdecl;
    //function getSourceId: JString; cdecl;
    //function getSourceLayer: JString; cdecl;
    //function getSymbolAvoidEdges: JPropertyValue; cdecl;
    //function getSymbolPlacement: JPropertyValue; cdecl;
    //function getSymbolScreenSpace: JPropertyValue; cdecl;
    //function getSymbolSortKey: JPropertyValue; cdecl;
    //function getSymbolSpacing: JPropertyValue; cdecl;
    //function getSymbolZOrder: JPropertyValue; cdecl;
    //function getTextAllowOverlap: JPropertyValue; cdecl;
    //function getTextAnchor: JPropertyValue; cdecl;
    //function getTextColor: JPropertyValue; cdecl;
    //function getTextColorAsInt: Integer; cdecl;
    //function getTextColorTransition: JTransitionOptions; cdecl;
    //function getTextField: JPropertyValue; cdecl;
    //function getTextFont: JPropertyValue; cdecl;
    //function getTextHaloBlur: JPropertyValue; cdecl;
    //function getTextHaloBlurTransition: JTransitionOptions; cdecl;
    //function getTextHaloColor: JPropertyValue; cdecl;
    //function getTextHaloColorAsInt: Integer; cdecl;
    //function getTextHaloColorTransition: JTransitionOptions; cdecl;
    //function getTextHaloWidth: JPropertyValue; cdecl;
    //function getTextHaloWidthTransition: JTransitionOptions; cdecl;
    //function getTextIgnorePlacement: JPropertyValue; cdecl;
    //function getTextJustify: JPropertyValue; cdecl;
    //function getTextKeepUpright: JPropertyValue; cdecl;
    //function getTextLetterSpacing: JPropertyValue; cdecl;
    //function getTextLineHeight: JPropertyValue; cdecl;
    //function getTextMaxAngle: JPropertyValue; cdecl;
    //function getTextMaxWidth: JPropertyValue; cdecl;
    //function getTextOffset: JPropertyValue; cdecl;
    //function getTextOpacity: JPropertyValue; cdecl;
    //function getTextOpacityTransition: JTransitionOptions; cdecl;
    //function getTextOptional: JPropertyValue; cdecl;
    //function getTextPadding: JPropertyValue; cdecl;
    //function getTextPitchAlignment: JPropertyValue; cdecl;
    //function getTextRadialOffset: JPropertyValue; cdecl;
    //function getTextRotate: JPropertyValue; cdecl;
    //function getTextRotationAlignment: JPropertyValue; cdecl;
    //function getTextSize: JPropertyValue; cdecl;
    //function getTextTransform: JPropertyValue; cdecl;
    //function getTextTranslate: JPropertyValue; cdecl;
    //function getTextTranslateAnchor: JPropertyValue; cdecl;
    //function getTextTranslateTransition: JTransitionOptions; cdecl;
    //function getTextVariableAnchor: JPropertyValue; cdecl;
    //function getTextVariableAnchorOffset: JPropertyValue; cdecl;
    //function getTextWritingMode: JPropertyValue; cdecl;
    //procedure setFilter(expression: JExpression); cdecl;
    //procedure setIconColorTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setIconHaloBlurTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setIconHaloColorTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setIconHaloWidthTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setIconOpacityTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setIconTranslateTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setSourceLayer(string_: JString); cdecl;
    //procedure setTextColorTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setTextHaloBlurTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setTextHaloColorTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setTextHaloWidthTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setTextOpacityTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setTextTranslateTransition(transitionOptions: JTransitionOptions); cdecl;
    //function withFilter(expression: JExpression): JSymbolLayer; cdecl;
    //function withProperties(propertyValue: TJavaObjectArray<JPropertyValue>): JSymbolLayer; cdecl;
    //function withSourceLayer(string_: JString): JSymbolLayer; cdecl;
  end;
  TJSymbolLayer = class(TJavaGenericImport<JSymbolLayerClass, JSymbolLayer>) end;

  {**************************************}
  JLineLayerClass = interface(JLayerClass)
    ['{D38CAF21-B119-47FC-95EF-9F3B034CFD2F}']
    {class} function init(layerId: JString; sourceId: JString): JLineLayer; cdecl;
  end;
  [JavaSignature('org/maplibre/android/style/layers/LineLayer')]
  JLineLayer = interface(JLayer)
    ['{22EF9E5A-9D06-4FE0-A81A-26A7DC3DBFFB}']
    //function getFilter: JExpression; cdecl;
    //function getLineBlur: JPropertyValue; cdecl;
    //function getLineBlurTransition: JTransitionOptions; cdecl;
    //function getLineCap: JPropertyValue; cdecl;
    //function getLineColor: JPropertyValue; cdecl;
    //function getLineColorAsInt: Integer; cdecl;
    //function getLineColorTransition: JTransitionOptions; cdecl;
    //function getLineDasharray: JPropertyValue; cdecl;
    //function getLineDasharrayTransition: JTransitionOptions; cdecl;
    //function getLineGapWidth: JPropertyValue; cdecl;
    //function getLineGapWidthTransition: JTransitionOptions; cdecl;
    //function getLineGradient: JPropertyValue; cdecl;
    //function getLineGradientAsInt: Integer; cdecl;
    //function getLineJoin: JPropertyValue; cdecl;
    //function getLineMiterLimit: JPropertyValue; cdecl;
    //function getLineOffset: JPropertyValue; cdecl;
    //function getLineOffsetTransition: JTransitionOptions; cdecl;
    //function getLineOpacity: JPropertyValue; cdecl;
    //function getLineOpacityTransition: JTransitionOptions; cdecl;
    //function getLinePattern: JPropertyValue; cdecl;
    //function getLinePatternTransition: JTransitionOptions; cdecl;
    //function getLineRoundLimit: JPropertyValue; cdecl;
    //function getLineSortKey: JPropertyValue; cdecl;
    //function getLineTranslate: JPropertyValue; cdecl;
    //function getLineTranslateAnchor: JPropertyValue; cdecl;
    //function getLineTranslateTransition: JTransitionOptions; cdecl;
    //function getLineWidth: JPropertyValue; cdecl;
    //function getLineWidthTransition: JTransitionOptions; cdecl;
    //function getSourceId: JString; cdecl;
    //function getSourceLayer: JString; cdecl;
    //procedure setFilter(expression: JExpression); cdecl;
    //procedure setLineBlurTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setLineColorTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setLineDasharrayTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setLineGapWidthTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setLineOffsetTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setLineOpacityTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setLinePatternTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setLineTranslateTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setLineWidthTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setSourceLayer(string_: JString); cdecl;
    //function withFilter(expression: JExpression): JLineLayer; cdecl;
    //function withProperties(propertyValue: TJavaObjectArray<JPropertyValue>): JLineLayer; cdecl;
    //function withSourceLayer(string_: JString): JLineLayer; cdecl;
  end;
  TJLineLayer = class(TJavaGenericImport<JLineLayerClass, JLineLayer>) end;

  {****************************************}
  JCircleLayerClass = interface(JLayerClass)
    ['{FBC962CF-1161-4023-8950-B5FE3FE0A7FB}']
    {class} function init(layerId: JString; sourceId: JString): JCircleLayer; cdecl;
  end;
  [JavaSignature('org/maplibre/android/style/layers/CircleLayer')]
  JCircleLayer = interface(JLayer)
    ['{0185645D-07CC-49A2-84E8-553B5A3EE072}']
    //function getCircleBlur: JPropertyValue; cdecl;
    //function getCircleBlurTransition: JTransitionOptions; cdecl;
    //function getCircleColor: JPropertyValue; cdecl;
    //function getCircleColorAsInt: Integer; cdecl;
    //function getCircleColorTransition: JTransitionOptions; cdecl;
    //function getCircleOpacity: JPropertyValue; cdecl;
    //function getCircleOpacityTransition: JTransitionOptions; cdecl;
    //function getCirclePitchAlignment: JPropertyValue; cdecl;
    //function getCirclePitchScale: JPropertyValue; cdecl;
    //function getCircleRadius: JPropertyValue; cdecl;
    //function getCircleRadiusTransition: JTransitionOptions; cdecl;
    //function getCircleSortKey: JPropertyValue; cdecl;
    //function getCircleStrokeColor: JPropertyValue; cdecl;
    //function getCircleStrokeColorAsInt: Integer; cdecl;
    //function getCircleStrokeColorTransition: JTransitionOptions; cdecl;
    //function getCircleStrokeOpacity: JPropertyValue; cdecl;
    //function getCircleStrokeOpacityTransition: JTransitionOptions; cdecl;
    //function getCircleStrokeWidth: JPropertyValue; cdecl;
    //function getCircleStrokeWidthTransition: JTransitionOptions; cdecl;
    //function getCircleTranslate: JPropertyValue; cdecl;
    //function getCircleTranslateAnchor: JPropertyValue; cdecl;
    //function getCircleTranslateTransition: JTransitionOptions; cdecl;
    //function getFilter: JExpression; cdecl;
    //function getSourceId: JString; cdecl;
    //function getSourceLayer: JString; cdecl;
    //procedure setCircleBlurTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setCircleColorTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setCircleOpacityTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setCircleRadiusTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setCircleStrokeColorTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setCircleStrokeOpacityTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setCircleStrokeWidthTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setCircleTranslateTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setFilter(expression: JExpression); cdecl;
    //procedure setSourceLayer(string_: JString); cdecl;
    //function withFilter(expression: JExpression): JCircleLayer; cdecl;
    //function withProperties(propertyValue: TJavaObjectArray<JPropertyValue>): JCircleLayer; cdecl;
    //function withSourceLayer(string_: JString): JCircleLayer; cdecl;
  end;
  TJCircleLayer = class(TJavaGenericImport<JCircleLayerClass, JCircleLayer>) end;

  {**************************************}
  JFillLayerClass = interface(JLayerClass)
    ['{06091FE0-8320-42E6-84C9-E31766DA3AA6}']
    {class} function init(layerId: JString; sourceId: JString): JFillLayer; cdecl;
  end;
  [JavaSignature('org/maplibre/android/style/layers/FillLayer')]
  JFillLayer = interface(JLayer)
    ['{ED890C3F-5905-4EF2-A3B7-2CD20BB65EB0}']
    //function getFillAntialias: JPropertyValue; cdecl;
    //function getFillColor: JPropertyValue; cdecl;
    //function getFillColorAsInt: Integer; cdecl;
    //function getFillColorTransition: JTransitionOptions; cdecl;
    //function getFillOpacity: JPropertyValue; cdecl;
    //function getFillOpacityTransition: JTransitionOptions; cdecl;
    //function getFillOutlineColor: JPropertyValue; cdecl;
    //function getFillOutlineColorAsInt: Integer; cdecl;
    //function getFillOutlineColorTransition: JTransitionOptions; cdecl;
    //function getFillPattern: JPropertyValue; cdecl;
    //function getFillPatternTransition: JTransitionOptions; cdecl;
    //function getFillSortKey: JPropertyValue; cdecl;
    //function getFillTranslate: JPropertyValue; cdecl;
    //function getFillTranslateAnchor: JPropertyValue; cdecl;
    //function getFillTranslateTransition: JTransitionOptions; cdecl;
    //function getFilter: JExpression; cdecl;
    //function getSourceId: JString; cdecl;
    //function getSourceLayer: JString; cdecl;
    //procedure setFillColorTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setFillOpacityTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setFillOutlineColorTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setFillPatternTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setFillTranslateTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setFilter(expression: JExpression); cdecl;
    //procedure setSourceLayer(string_: JString); cdecl;
    //function withFilter(expression: JExpression): JFillLayer; cdecl;
    //function withProperties(propertyValue: TJavaObjectArray<JPropertyValue>): JFillLayer; cdecl;
    //function withSourceLayer(string_: JString): JFillLayer; cdecl;
  end;
  TJFillLayer = class(TJavaGenericImport<JFillLayerClass, JFillLayer>) end;

  {***********************************************}
  JFillExtrusionLayerClass = interface(JLayerClass)
    ['{7E9DF104-C1BF-447A-BA55-17A1D10ED76C}']
    {class} function init(layerId: JString; sourceId: JString): JFillExtrusionLayer; cdecl;
  end;
  [JavaSignature('org/maplibre/android/style/layers/FillExtrusionLayer')]
  JFillExtrusionLayer = interface(JLayer)
    ['{24DA06AE-47BD-47B6-BFC6-93D1B7A554FD}']
    //function getFillExtrusionBase: JPropertyValue; cdecl;
    //function getFillExtrusionBaseTransition: JTransitionOptions; cdecl;
    //function getFillExtrusionColor: JPropertyValue; cdecl;
    //function getFillExtrusionColorAsInt: Integer; cdecl;
    //function getFillExtrusionColorTransition: JTransitionOptions; cdecl;
    //function getFillExtrusionHeight: JPropertyValue; cdecl;
    //function getFillExtrusionHeightTransition: JTransitionOptions; cdecl;
    //function getFillExtrusionOpacity: JPropertyValue; cdecl;
    //function getFillExtrusionOpacityTransition: JTransitionOptions; cdecl;
    //function getFillExtrusionPattern: JPropertyValue; cdecl;
    //function getFillExtrusionPatternTransition: JTransitionOptions; cdecl;
    //function getFillExtrusionRoundedCornerDistance: JPropertyValue; cdecl;
    //function getFillExtrusionTranslate: JPropertyValue; cdecl;
    //function getFillExtrusionTranslateAnchor: JPropertyValue; cdecl;
    //function getFillExtrusionTranslateTransition: JTransitionOptions; cdecl;
    //function getFillExtrusionVerticalGradient: JPropertyValue; cdecl;
    //function getFilter: JExpression; cdecl;
    //function getSourceId: JString; cdecl;
    //function getSourceLayer: JString; cdecl;
    //procedure setFillExtrusionBaseTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setFillExtrusionColorTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setFillExtrusionHeightTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setFillExtrusionOpacityTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setFillExtrusionPatternTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setFillExtrusionTranslateTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setFilter(expression: JExpression); cdecl;
    //procedure setSourceLayer(string_: JString); cdecl;
    //function withFilter(expression: JExpression): JFillExtrusionLayer; cdecl;
    //function withProperties(propertyValue: TJavaObjectArray<JPropertyValue>): JFillExtrusionLayer; cdecl;
    //function withSourceLayer(string_: JString): JFillExtrusionLayer; cdecl;
  end;
  TJFillExtrusionLayer = class(TJavaGenericImport<JFillExtrusionLayerClass, JFillExtrusionLayer>) end;

  {*****************************************}
  JHeatmapLayerClass = interface(JLayerClass)
    ['{E62A05EA-BAF0-43AC-848B-F3C34604F51F}']
    {class} function init(layerId: JString; sourceId: JString): JHeatmapLayer; cdecl;
  end;
  [JavaSignature('org/maplibre/android/style/layers/HeatmapLayer')]
  JHeatmapLayer = interface(JLayer)
    ['{6909BD8E-EBBE-40D6-B906-E98FA8035631}']
    //function getFilter: JExpression; cdecl;
    //function getHeatmapColor: JPropertyValue; cdecl;
    //function getHeatmapColorAsInt: Integer; cdecl;
    //function getHeatmapIntensity: JPropertyValue; cdecl;
    //function getHeatmapIntensityTransition: JTransitionOptions; cdecl;
    //function getHeatmapOpacity: JPropertyValue; cdecl;
    //function getHeatmapOpacityTransition: JTransitionOptions; cdecl;
    //function getHeatmapRadius: JPropertyValue; cdecl;
    //function getHeatmapRadiusTransition: JTransitionOptions; cdecl;
    //function getHeatmapWeight: JPropertyValue; cdecl;
    //function getSourceId: JString; cdecl;
    //function getSourceLayer: JString; cdecl;
    //procedure setFilter(expression: JExpression); cdecl;
    //procedure setHeatmapIntensityTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setHeatmapOpacityTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setHeatmapRadiusTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setSourceLayer(string_: JString); cdecl;
    //function withFilter(expression: JExpression): JHeatmapLayer; cdecl;
    //function withProperties(propertyValue: TJavaObjectArray<JPropertyValue>): JHeatmapLayer; cdecl;
    //function withSourceLayer(string_: JString): JHeatmapLayer; cdecl;
  end;
  TJHeatmapLayer = class(TJavaGenericImport<JHeatmapLayerClass, JHeatmapLayer>) end;

  {*********************************************}
  JColorReliefLayerClass = interface(JLayerClass)
    ['{9AC13837-56AF-4940-A72A-D44C43DE3294}']
    {class} function init(layerId: JString; sourceId: JString): JColorReliefLayer; cdecl;
  end;
  [JavaSignature('org/maplibre/android/style/layers/ColorReliefLayer')]
  JColorReliefLayer = interface(JLayer)
    ['{76948547-770E-4335-98E6-BF1CD576AE6D}']
    //function getColorReliefColor: JPropertyValue; cdecl;
    //function getColorReliefColorAsInt: Integer; cdecl;
    //function getColorReliefOpacity: JPropertyValue; cdecl;
    //function getColorReliefOpacityTransition: JTransitionOptions; cdecl;
    //function getFilter: JExpression; cdecl;
    //function getSourceId: JString; cdecl;
    //function getSourceLayer: JString; cdecl;
    //procedure setColorReliefOpacityTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setFilter(expression: JExpression); cdecl;
    //procedure setSourceLayer(string_: JString); cdecl;
    //function withFilter(expression: JExpression): JColorReliefLayer; cdecl;
    //function withProperties(propertyValue: TJavaObjectArray<JPropertyValue>): JColorReliefLayer; cdecl;
    //function withSourceLayer(string_: JString): JColorReliefLayer; cdecl;
  end;
  TJColorReliefLayer = class(TJavaGenericImport<JColorReliefLayerClass, JColorReliefLayer>) end;

  {*******************************************}
  JHillshadeLayerClass = interface(JLayerClass)
    ['{7347701B-B997-4F66-BD35-6C0D98EC8147}']
    {class} function init(layerId: JString; sourceId: JString): JHillshadeLayer; cdecl;
  end;
  [JavaSignature('org/maplibre/android/style/layers/HillshadeLayer')]
  JHillshadeLayer = interface(JLayer)
    ['{CD1C7626-6062-46D2-944E-1B2899506B8B}']
    //function getHillshadeAccentColor: JPropertyValue; cdecl;
    //function getHillshadeAccentColorAsInt: Integer; cdecl;
    //function getHillshadeAccentColorTransition: JTransitionOptions; cdecl;
    //function getHillshadeExaggeration: JPropertyValue; cdecl;
    //function getHillshadeExaggerationTransition: JTransitionOptions; cdecl;
    //function getHillshadeHighlightColor: JPropertyValue; cdecl;
    //function getHillshadeHighlightColorTransition: JTransitionOptions; cdecl;
    //function getHillshadeIlluminationAltitude: JPropertyValue; cdecl;
    //function getHillshadeIlluminationAnchor: JPropertyValue; cdecl;
    //function getHillshadeIlluminationDirection: JPropertyValue; cdecl;
    //function getHillshadeMethod: JPropertyValue; cdecl;
    //function getHillshadeShadowColor: JPropertyValue; cdecl;
    //function getHillshadeShadowColorTransition: JTransitionOptions; cdecl;
    //function getSourceId: JString; cdecl;
    //procedure setHillshadeAccentColorTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setHillshadeExaggerationTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setHillshadeHighlightColorTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setHillshadeShadowColorTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setSourceLayer(string_: JString); cdecl;
    //function withProperties(propertyValue: TJavaObjectArray<JPropertyValue>): JHillshadeLayer; cdecl;
    //function withSourceLayer(string_: JString): JHillshadeLayer; cdecl;
  end;
  TJHillshadeLayer = class(TJavaGenericImport<JHillshadeLayerClass, JHillshadeLayer>) end;

  {****************************************}
  JRasterLayerClass = interface(JLayerClass)
    ['{5CE6D498-5D21-4681-81E3-17009C3E8A4B}']
    {class} function init(layerId: JString; sourceId: JString): JRasterLayer; cdecl;
  end;
  [JavaSignature('org/maplibre/android/style/layers/RasterLayer')]
  JRasterLayer = interface(JLayer)
    ['{FB838FB6-C0D8-40FD-8B07-AED185246FC3}']
    //function getRasterBrightnessMax: JPropertyValue; cdecl;
    //function getRasterBrightnessMaxTransition: JTransitionOptions; cdecl;
    //function getRasterBrightnessMin: JPropertyValue; cdecl;
    //function getRasterBrightnessMinTransition: JTransitionOptions; cdecl;
    //function getRasterContrast: JPropertyValue; cdecl;
    //function getRasterContrastTransition: JTransitionOptions; cdecl;
    //function getRasterFadeDuration: JPropertyValue; cdecl;
    //function getRasterHueRotate: JPropertyValue; cdecl;
    //function getRasterHueRotateTransition: JTransitionOptions; cdecl;
    //function getRasterOpacity: JPropertyValue; cdecl;
    //function getRasterOpacityTransition: JTransitionOptions; cdecl;
    //function getRasterResampling: JPropertyValue; cdecl;
    //function getRasterSaturation: JPropertyValue; cdecl;
    //function getRasterSaturationTransition: JTransitionOptions; cdecl;
    //function getSourceId: JString; cdecl;
    //procedure setRasterBrightnessMaxTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setRasterBrightnessMinTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setRasterContrastTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setRasterHueRotateTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setRasterOpacityTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setRasterSaturationTransition(transitionOptions: JTransitionOptions); cdecl;
    //procedure setSourceLayer(string_: JString); cdecl;
    //function withProperties(propertyValue: TJavaObjectArray<JPropertyValue>): JRasterLayer; cdecl;
    //function withSourceLayer(string_: JString): JRasterLayer; cdecl;
  end;
  TJRasterLayer = class(TJavaGenericImport<JRasterLayerClass, JRasterLayer>) end;

  {************************************}
  JSourceClass = interface(JObjectClass)
    ['{9D2D1C95-60A5-434E-A887-FAB17ADDD5E7}']
    //{class} function init: JSource; cdecl;
  end;
  [JavaSignature('org/maplibre/android/style/sources/Source')]
  JSource = interface(JObject)
    ['{E44265BF-C8C4-495A-9CB5-528F48045CD7}']
    //function getAttribution: JString; cdecl;
    //function getId: JString; cdecl;
    //function getMaxOverscaleFactorForParentTiles: JInteger; cdecl;
    //function getMinimumTileUpdateInterval: JLong; cdecl;
    //function getNativePtr: Int64; cdecl;
    //function getPrefetchZoomDelta: JInteger; cdecl;
    //function isVolatile: JBoolean; cdecl;
    //procedure setDetached; cdecl;
    //procedure setMaxOverscaleFactorForParentTiles(integer_: JInteger); cdecl;
    //procedure setMinimumTileUpdateInterval(long: JLong); cdecl;
    //procedure setPrefetchZoomDelta(integer_: JInteger); cdecl;
    //procedure setVolatile(boolean: JBoolean); cdecl;
  end;
  TJSource = class(TJavaGenericImport<JSourceClass, JSource>) end;

  {***************************************************}
  JGeoJsonSourceClass = interface(JSourceClass)
    ['{E2347622-AE08-41AF-8607-FDFB3E0BC522}']
    //{class} function init(l: Int64): JGeoJsonSource; cdecl; overload;
    //{class} function init(id: JString): JGeoJsonSource; cdecl; overload;
    //{class} function init(string_: JString; geoJsonOptions: JGeoJsonOptions): JGeoJsonSource; cdecl; overload;
    {class} function init(id: JString; geoJson: JString): JGeoJsonSource; cdecl; overload;
    //{class} function init(string_: JString; string_1: JString; geoJsonOptions: JGeoJsonOptions): JGeoJsonSource; cdecl; overload;
    //{class} function init(string_: JString; uRL: JURL): JGeoJsonSource; cdecl; overload;
    //{class} function init(string_: JString; uRL: JURL; geoJsonOptions: JGeoJsonOptions): JGeoJsonSource; cdecl; overload;
    //{class} function init(string_: JString; uRI: JURI): JGeoJsonSource; cdecl; overload;
    //{class} function init(string_: JString; uRI: JURI; geoJsonOptions: JGeoJsonOptions): JGeoJsonSource; cdecl; overload;
    //{class} function init(string_: JString; featureCollection: JObject): JGeoJsonSource; cdecl; overload;
    //{class} function init(string_: JString; featureCollection: JObject; geoJsonOptions: JGeoJsonOptions): JGeoJsonSource; cdecl; overload;
    //{class} function init(string_: JString; feature: JObject): JGeoJsonSource; cdecl; overload;
    //{class} function init(string_: JString; feature: JObject; geoJsonOptions: JGeoJsonOptions): JGeoJsonSource; cdecl; overload;
    //{class} function init(string_: JString; geometry: JObject): JGeoJsonSource; cdecl; overload;
    //{class} function init(string_: JString; geometry: JObject; geoJsonOptions: JGeoJsonOptions): JGeoJsonSource; cdecl; overload;
  end;
  [JavaSignature('org/maplibre/android/style/sources/GeoJsonSource')]
  JGeoJsonSource = interface(JSource)
    ['{8C7119BD-BAF9-44E3-A2B9-690D8C643DAF}']
    //function getClusterChildren(feature: JObject): JObject; cdecl;
    //function getClusterExpansionZoom(feature: JObject): Integer; cdecl;
    //function getClusterLeaves(feature: JObject; l: Int64; l1: Int64): JObject; cdecl;
    //function getFeatureState(string_: JString): JObject; cdecl;
    //function getUri: JString; cdecl;
    //function getUrl: JString; cdecl;
    //function querySourceFeatures(expression: JExpression): JList; cdecl;
    //function removeFeatureState(string_: JString; string_1: JString): Boolean; cdecl; overload;
    //function removeFeatureState(string_: JString): Boolean; cdecl; overload;
    //function resetFeatureStates: Boolean; cdecl;
    //function setFeatureState(string_: JString; jsonObject: JObject): Boolean; cdecl;
    //procedure setGeoJson(feature: JObject); cdecl; overload;
    //procedure setGeoJson(geometry: JObject); cdecl; overload;
    //procedure setGeoJson(featureCollection: JObject); cdecl; overload;
    procedure setGeoJson(json: JString); cdecl; overload;
    //procedure setOverrideSynchronousUpdate(b: Boolean); cdecl;
    //procedure setUri(uRI: JURI); cdecl; overload;
    //procedure setUri(string_: JString); cdecl; overload;
    //procedure setUrl(uRL: JURL); cdecl; overload;
    //procedure setUrl(string_: JString); cdecl; overload;
  end;
  TJGeoJsonSource = class(TJavaGenericImport<JGeoJsonSourceClass, JGeoJsonSource>) end;

  {******************************************}
  JRasterSourceClass = interface(JSourceClass)
    ['{24F6B1F6-EEF7-4882-BE01-3C78AC25B128}']
    //{class} function _GetCompanion: JRasterSource_Companion; cdecl;
    //{class} function _GetDEFAULT_TILE_SIZE: Integer; cdecl;
    //{class} function init(l: Int64): JRasterSource; cdecl; overload;
    {class} function init(id: JString; url: JURL): JRasterSource; cdecl; overload;
    {class} function init(id: JString; uri: JURI): JRasterSource; cdecl; overload;
    {class} function init(id: JString; uri: JString): JRasterSource; cdecl; overload;
    {class} function init(id: JString; uri: JString; tileSize: Integer): JRasterSource; cdecl; overload;
    {class} function init(id: JString; tileSet: JTileSet): JRasterSource; cdecl; overload;
    {class} function init(id: JString; tileSet: JTileSet; tileSize: Integer): JRasterSource; cdecl; overload;
    //{class} property Companion: JRasterSource_Companion read _GetCompanion;
    //{class} property DEFAULT_TILE_SIZE: Integer read _GetDEFAULT_TILE_SIZE;
  end;
  [JavaSignature('org/maplibre/android/style/sources/RasterSource')]
  JRasterSource = interface(JSource)
    ['{56B9F10D-B789-49A0-9F5D-8EAA7A71B444}']
    //function getUri: JString; cdecl;
    //function getUrl: JString; cdecl;
  end;
  TJRasterSource = class(TJavaGenericImport<JRasterSourceClass, JRasterSource>) end;

  {*************************************}
  JTileSetClass = interface(JObjectClass)
    ['{CB746429-E55B-456B-9C23-83AC4780D18D}']
    {class} function init(tilejson: JString; tiles: TJavaObjectArray<JString>): JTileSet; cdecl;
  end;
  [JavaSignature('org/maplibre/android/style/sources/TileSet')]
  JTileSet = interface(JObject)
    ['{36629FF1-E2DF-41C1-9BCF-86736490B288}']
    //function _GetmaxZoom: JFloat; cdecl;
    //procedure _SetmaxZoom(Value: JFloat); cdecl;
    //function _GetminZoom: JFloat; cdecl;
    //procedure _SetminZoom(Value: JFloat); cdecl;
    //function getAttribution: JString; cdecl;
    //function getBounds: TJavaObjectArray<JFloat>; cdecl;
    //function getCenter: TJavaObjectArray<JFloat>; cdecl;
    //function getData: TJavaObjectArray<JString>; cdecl;
    //function getDescription: JString; cdecl;
    //function getEncoding: JString; cdecl;
    //function getGrids: TJavaObjectArray<JString>; cdecl;
    //function getLegend: JString; cdecl;
    //function getMaxZoom: Single; cdecl;
    //function getMinZoom: Single; cdecl;
    //function getName: JString; cdecl;
    //function getScheme: JString; cdecl;
    //function getTemplate: JString; cdecl;
    //function getTilejson: JString; cdecl;
    //function getTiles: TJavaObjectArray<JString>; cdecl;
    //function getVersion: JString; cdecl;
    //procedure setAttribution(string_: JString); cdecl;
    //procedure setBounds(f: TJavaArray<Single>); cdecl; overload;
    //procedure setBounds(f: Single; f1: Single; f2: Single; f3: Single); cdecl; overload;
    //procedure setBounds(float: TJavaObjectArray<JFloat>); cdecl; overload;
    //procedure setBounds(latLngBounds: Jgeometry_LatLngBounds); cdecl; overload;
    //procedure setCenter(f: TJavaArray<Single>); cdecl; overload;
    //procedure setCenter(latLng: Jgeometry_LatLng); cdecl; overload;
    //procedure setData(string_: TJavaObjectArray<JString>); cdecl;
    //procedure setDescription(string_: JString); cdecl;
    //procedure setEncoding(string_: JString); cdecl;
    //procedure setGrids(string_: TJavaObjectArray<JString>); cdecl;
    //procedure setLegend(string_: JString); cdecl;
    procedure setMaxZoom(maxZoom: Single); cdecl;
    procedure setMinZoom(minZoom: Single); cdecl;
    //procedure setName(string_: JString); cdecl;
    //procedure setScheme(string_: JString); cdecl;
    //procedure setTemplate(string_: JString); cdecl;
    //procedure setVersion(string_: JString); cdecl;
    //function toValueObject: JMap; cdecl;
    //property maxZoom: JFloat read _GetmaxZoom write _SetmaxZoom;
    //property minZoom: JFloat read _GetminZoom write _SetminZoom;
  end;
  TJTileSet = class(TJavaGenericImport<JTileSetClass, JTileSet>) end;

  {*********************************************}
  JLatLngClass = interface(JObjectClass)
    ['{3910F996-BAE6-4131-B531-76A83F648AE5}']
    //{class} function _GetCREATOR: JParcelable_Creator; cdecl;
    //{class} function _GetCompanion: JLatLng_Companion; cdecl;
    //{class} function init: JLatLng; cdecl; overload;
    {class} function init(latitude: Double; longitude: Double): JLatLng; cdecl; overload;
    {class} function init(latitude: Double; longitude: Double; altitude: Double): JLatLng; cdecl; overload;
    //{class} function init(location: JLocation): JLatLng; cdecl; overload;
    //{class} function init(latLng: JLatLng): JLatLng; cdecl; overload;
    //{class} function init(&in: JParcel): JLatLng; cdecl; overload;
    //{class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    //{class} property Companion: JLatLng_Companion read _GetCompanion;
  end;
  [JavaSignature('org/maplibre/android/geometry/LatLng')]
  JLatLng = interface(JObject)
    ['{23299645-C593-401A-BB92-38C75978C3DC}']
    //function describeContents: Integer; cdecl;
    //function distanceTo(other: JLatLng): Double; cdecl;
    //function equals(other: JObject): Boolean; cdecl;
    function getAltitude: Double; cdecl;
    function getLatitude: Double; cdecl;
    function getLongitude: Double; cdecl;
    //function hashCode: Integer; cdecl;
    procedure setAltitude(altitude: Double); cdecl;
    procedure setLatitude(latitude: Double); cdecl;
    procedure setLongitude(longitude: Double); cdecl;
    //function toString: JString; cdecl;
    //function wrap: JLatLng; cdecl;
    //procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
  end;
  TJLatLng = class(TJavaGenericImport<JLatLngClass, JLatLng>) end;

  {******************************************}
  JLatLngBoundsClass = interface(JObjectClass)
    ['{6C9BE42E-8364-4573-A311-7F6262ADBCCE}']
    //{class} function _GetCREATOR: JParcelable_Creator; cdecl;
    //{class} function _GetCompanion: JLatLngBounds_Companion; cdecl;
    //{class} function init(d: Double; d1: Double; d2: Double; d3: Double): JLatLngBounds; cdecl;
    //{class} function from(latNorth: Double; lonEast: Double; latSouth: Double; lonWest: Double): JLatLngBounds; cdecl; overload;
    //{class} function from(z: Integer; x: Integer; y: Integer): JLatLngBounds; cdecl; overload;
    //{class} function world: JLatLngBounds; cdecl;
    //{class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    //{class} property Companion: JLatLngBounds_Companion read _GetCompanion;
  end;
  [JavaSignature('org/maplibre/android/geometry/LatLngBounds')]
  JLatLngBounds = interface(JObject)
    ['{CBA2A96B-14D5-4AC5-A5D5-F3730BA2C01F}']
    //function _GetlatitudeNorth: Double; cdecl;
    //function _GetlatitudeSouth: Double; cdecl;
    //function _GetlongitudeEast: Double; cdecl;
    //function _GetlongitudeWest: Double; cdecl;
    //function &contains(latLng: JLatLng): Boolean; cdecl; overload;
    //function &contains(other: JLatLngBounds): Boolean; cdecl; overload;
    //function describeContents: Integer; cdecl;
    //function equals(other: JObject): Boolean; cdecl;
    //function getCenter: JLatLng; cdecl;
    function getLatNorth: Double; cdecl;
    function getLatSouth: Double; cdecl;
    //function getLatitudeSpan: Double; cdecl;
    function getLonEast: Double; cdecl;
    function getLonWest: Double; cdecl;
    //function getLongitudeSpan: Double; cdecl;
    //function getNorthEast: JLatLng; cdecl;
    //function getNorthWest: JLatLng; cdecl;
    //function getSouthEast: JLatLng; cdecl;
    //function getSouthWest: JLatLng; cdecl;
    //function getSpan: JLatLngSpan; cdecl;
    //function hashCode: Integer; cdecl;
    //function include(latLng: JLatLng): JLatLngBounds; cdecl;
    //function intersect(box: JLatLngBounds): JLatLngBounds; cdecl; overload;
    //function intersect(northLat: Double; eastLon: Double; southLat: Double; westLon: Double): JLatLngBounds; cdecl; overload;
    //function isEmptySpan: Boolean; cdecl;
    //function toLatLngs: TJavaObjectArray<JLatLng>; cdecl;
    //function toString: JString; cdecl;
    //function union(bounds: JLatLngBounds): JLatLngBounds; cdecl; overload;
    //function union(northLat: Double; eastLon: Double; southLat: Double; westLon: Double): JLatLngBounds; cdecl; overload;
    //procedure writeToParcel(out: JParcel; flags: Integer); cdecl;
    //property latitudeNorth: Double read _GetlatitudeNorth;
    //property latitudeSouth: Double read _GetlatitudeSouth;
    //property longitudeEast: Double read _GetlongitudeEast;
    //property longitudeWest: Double read _GetlongitudeWest;
  end;
  TJLatLngBounds = class(TJavaGenericImport<JLatLngBoundsClass, JLatLngBounds>) end;

  {**************************************************}
  JLatLngBounds_BuilderClass = interface(JObjectClass)
    ['{C2655C69-3E0E-44AA-A377-E150B939B54B}']
    //{class} function init: JLatLngBounds_Builder; cdecl;
  end;
  [JavaSignature('org/maplibre/android/geometry/LatLngBounds$Builder')]
  JLatLngBounds_Builder = interface(JObject)
    ['{A0AFADC8-EA81-4DDD-B293-48F87568021F}']
    //function build: JLatLngBounds; cdecl;
    //function include(latLng: JLatLng): JLatLngBounds_Builder; cdecl;
    //function includes(latLngs: JList): JLatLngBounds_Builder; cdecl;
  end;
  TJLatLngBounds_Builder = class(TJavaGenericImport<JLatLngBounds_BuilderClass, JLatLngBounds_Builder>) end;

  {***********************************}
  JStyleClass = interface(JObjectClass)
    ['{8C259A00-17CC-482D-95EE-AE388B837C48}']
    //{class} function getPredefinedStyle(string_: JString): JString; cdecl;
    //{class} function getPredefinedStyles: TJavaObjectArray<JDefaultStyle>; cdecl;
    //{class} function toImage(imageWrapper: JBuilder_ImageWrapper): Jmaps_Image; cdecl;
  end;
  [JavaSignature('org/maplibre/android/maps/Style')]
  JStyle = interface(JObject)
    ['{1801D420-5CAD-474B-8F5A-50990E2EC531}']
    procedure addImage(name: JString; image: JBitmap); cdecl; overload;
    //procedure addImage(string_: JString; bitmap: JBitmap; list: JList; list1: JList; imageContent: JImageContent); cdecl; overload;
    //procedure addImage(string_: JString; drawable: JDrawable); cdecl; overload;
    //procedure addImage(string_: JString; drawable: JDrawable; list: JList; list1: JList; imageContent: JImageContent); cdecl; overload;
    //procedure addImage(string_: JString; bitmap: JBitmap; b: Boolean); cdecl; overload;
    //procedure addImage(string_: JString; bitmap: JBitmap; b: Boolean; list: JList; list1: JList; imageContent: JImageContent); cdecl; overload;
    //procedure addImageAsync(name: JString; image: JBitmap); cdecl; overload;
    //procedure addImageAsync(string_: JString; bitmap: JBitmap; list: JList; list1: JList; imageContent: JImageContent); cdecl; overload;
    //procedure addImageAsync(string_: JString; drawable: JDrawable); cdecl; overload;
    //procedure addImageAsync(string_: JString; drawable: JDrawable; list: JList; list1: JList; imageContent: JImageContent); cdecl; overload;
    //procedure addImageAsync(string_: JString; bitmap: JBitmap; b: Boolean); cdecl; overload;
    //procedure addImageAsync(string_: JString; bitmap: JBitmap; b: Boolean; list: JList; list1: JList; imageContent: JImageContent); cdecl; overload;
    //procedure addImages(hashMap: JHashMap); cdecl; overload;
    //procedure addImages(hashMap: JHashMap; list: JList; list1: JList; imageContent: JImageContent); cdecl; overload;
    //procedure addImages(hashMap: JHashMap; b: Boolean); cdecl; overload;
    //procedure addImages(hashMap: JHashMap; b: Boolean; list: JList; list1: JList; imageContent: JImageContent); cdecl; overload;
    //procedure addImagesAsync(hashMap: JHashMap); cdecl; overload;
    //procedure addImagesAsync(hashMap: JHashMap; list: JList; list1: JList; imageContent: JImageContent); cdecl; overload;
    //procedure addImagesAsync(hashMap: JHashMap; b: Boolean); cdecl; overload;
    //procedure addImagesAsync(hashMap: JHashMap; b: Boolean; list: JList; list1: JList; imageContent: JImageContent); cdecl; overload;
    procedure addLayer(layer: JLayer); cdecl;
    //procedure addLayerAbove(layer: JLayer; string_: JString); cdecl;
    //procedure addLayerAt(layer: JLayer; i: Integer); cdecl;
    //procedure addLayerBelow(layer: JLayer; string_: JString); cdecl;
    procedure addSource(source: JSource); cdecl;
    //function getImage(string_: JString): JBitmap; cdecl;
    //function getJson: JString; cdecl;
    function getLayer(id: JString): JLayer; cdecl;
    //function getLayerAs(string_: JString): JLayer; cdecl;
    //function getLayers: JList; cdecl;
    //function getLight: Jlight_Light; cdecl;
    function getSource(id: JString): JSource; cdecl;
    //function getSourceAs(string_: JString): JSource; cdecl;
    //function getSources: JList; cdecl;
    //function getTransition: JTransitionOptions; cdecl;
    //function getUri: JString; cdecl;
    //function getUrl: JString; cdecl; // Deprecated
    //function isFullyLoaded: Boolean; cdecl;
    procedure removeImage(name: JString); cdecl;
    function removeLayer(layerId: JString): Boolean; cdecl; overload;
    function removeLayer(layer: JLayer): Boolean; cdecl; overload;
    //function removeLayerAt(i: Integer): Boolean; cdecl;
    function removeSource(sourceId: JString): Boolean; cdecl; overload;
    function removeSource(source: JSource): Boolean; cdecl; overload;
    //procedure setTransition(transitionOptions: JTransitionOptions); cdecl;
  end;
  TJStyle = class(TJavaGenericImport<JStyleClass, JStyle>) end;

  {*******************************************}
  JStyle_BuilderClass = interface(JObjectClass)
    ['{0F8E896F-B254-4B9A-8AB3-B9477CF223A6}']
    {class} function init: JStyle_Builder; cdecl;
  end;
  [JavaSignature('org/maplibre/android/maps/Style$Builder')]
  JStyle_Builder = interface(JObject)
    ['{57037845-EF83-437A-9255-06FB918973E3}']
    function fromJson(styleJson: JString): JStyle_Builder; cdecl;
    function fromUri(uri: JString): JStyle_Builder; cdecl;
    //function fromUrl(string_: JString): JStyle_Builder; cdecl; // Deprecated
    //function getImages: JList; cdecl;
    //function getJson: JString; cdecl;
    //function getLayers: JList; cdecl;
    //function getSources: JList; cdecl;
    //function getUri: JString; cdecl;
    //function withBitmapImages(pair: TJavaObjectArray<JPair>): JStyle_Builder; cdecl; overload;
    //function withBitmapImages(b: Boolean; pair: TJavaObjectArray<JPair>): JStyle_Builder; cdecl; overload;
    //function withDrawableImages(pair: TJavaObjectArray<JPair>): JStyle_Builder; cdecl; overload;
    //function withDrawableImages(b: Boolean; pair: TJavaObjectArray<JPair>): JStyle_Builder; cdecl; overload;
    //function withImage(string_: JString; drawable: JDrawable): JStyle_Builder; cdecl; overload;
    //function withImage(string_: JString; drawable: JDrawable; list: JList; list1: JList; imageContent: JImageContent): JStyle_Builder; cdecl; overload;
    //function withImage(string_: JString; bitmap: JBitmap): JStyle_Builder; cdecl; overload;
    //function withImage(string_: JString; bitmap: JBitmap; list: JList; list1: JList; imageContent: JImageContent): JStyle_Builder; cdecl; overload;
    //function withImage(string_: JString; drawable: JDrawable; b: Boolean): JStyle_Builder; cdecl; overload;
    //function withImage(string_: JString; drawable: JDrawable; b: Boolean; list: JList; list1: JList; imageContent: JImageContent): JStyle_Builder; cdecl; overload;
    //function withImage(string_: JString; bitmap: JBitmap; b: Boolean): JStyle_Builder; cdecl; overload;
    //function withImage(string_: JString; bitmap: JBitmap; b: Boolean; list: JList; list1: JList; imageContent: JImageContent): JStyle_Builder; cdecl; overload;
    //function withLayer(layer: JLayer): JStyle_Builder; cdecl;
    //function withLayerAbove(layer: JLayer; string_: JString): JStyle_Builder; cdecl;
    //function withLayerAt(layer: JLayer; i: Integer): JStyle_Builder; cdecl;
    //function withLayerBelow(layer: JLayer; string_: JString): JStyle_Builder; cdecl;
    //function withLayers(layer: TJavaObjectArray<JLayer>): JStyle_Builder; cdecl;
    //function withSource(source: JSource): JStyle_Builder; cdecl;
    //function withSources(source: TJavaObjectArray<JSource>): JStyle_Builder; cdecl;
    //function withTransition(transitionOptions: JTransitionOptions): JStyle_Builder; cdecl;
  end;
  TJStyle_Builder = class(TJavaGenericImport<JStyle_BuilderClass, JStyle_Builder>) end;

  {***********************************************}
  JCameraUpdateClass = interface(IJavaClass)
    ['{D6779DC3-A32B-4086-84E5-5A1D309EDD8D}']
  end;
  [JavaSignature('org/maplibre/android/camera/CameraUpdate')]
  JCameraUpdate = interface(IJavaInstance)
    ['{EB3FE761-93A7-47F8-B7C0-224B6E297BCD}']
    //function getCameraPosition(mapLibreMap: JMapLibreMap): JCameraPosition; cdecl;
  end;
  TJCameraUpdate = class(TJavaGenericImport<JCameraUpdateClass, JCameraUpdate>) end;

  {*************************************************}
  JCameraUpdateFactoryClass = interface(JObjectClass)
    ['{CD974031-5949-4A36-A8C8-C11A50789E0E}']
    //{class} function _GetINSTANCE: JCameraUpdateFactory; cdecl;
    //{class} function bearingTo(d: Double): JCameraUpdate; cdecl;
    //{class} function newCameraPosition(cameraPosition: JCameraPosition): JCameraUpdate; cdecl;
    //{class} function newLatLng(latLng: JLatLng): JCameraUpdate; cdecl;
    //{class} function newLatLngBounds(bounds: JLatLngBounds; padding: Integer): JCameraUpdate; cdecl; overload;
    //{class} function newLatLngBounds(bounds: JLatLngBounds; bearing: Double; tilt: Double; padding: Integer): JCameraUpdate; cdecl; overload;
    //{class} function newLatLngBounds(bounds: JLatLngBounds; paddingLeft: Integer; paddingTop: Integer; paddingRight: Integer; paddingBottom: Integer): JCameraUpdate; cdecl; overload;
    //{class} function newLatLngBounds(bounds: JLatLngBounds; bearing: Double; tilt: Double; paddingLeft: Integer; paddingTop: Integer; paddingRight: Integer; paddingBottom: Integer): JCameraUpdate; cdecl; overload;
    //{class} function newLatLngPadding(latLng: JLatLng; left: Double; top: Double; right: Double; bottom: Double): JCameraUpdate; cdecl;
    {class} function newLatLngZoom(latLng: JLatLng; zoom: Double): JCameraUpdate; cdecl;
    //{class} function paddingTo(d: TJavaArray<Double>): JCameraUpdate; cdecl; overload;
    //{class} function paddingTo(d: Double; d1: Double; d2: Double; d3: Double): JCameraUpdate; cdecl; overload;
    //{class} function tiltTo(d: Double): JCameraUpdate; cdecl;
    //{class} function zoomBy(d: Double; point: JPoint): JCameraUpdate; cdecl; overload;
    //{class} function zoomBy(d: Double): JCameraUpdate; cdecl; overload;
    //{class} function zoomIn: JCameraUpdate; cdecl;
    //{class} function zoomOut: JCameraUpdate; cdecl;
    //{class} function zoomTo(d: Double): JCameraUpdate; cdecl;
    //{class} property INSTANCE: JCameraUpdateFactory read _GetINSTANCE;
  end;
  [JavaSignature('org/maplibre/android/camera/CameraUpdateFactory')]
  JCameraUpdateFactory = interface(JObject)
    ['{6B64E2C5-F621-42DF-8BCE-3184B8921D7A}']
  end;
  TJCameraUpdateFactory = class(TJavaGenericImport<JCameraUpdateFactoryClass, JCameraUpdateFactory>) end;

  {**************************************}
  JMapLibreClass = interface(JObjectClass)
    ['{BDC41523-9E89-4ED8-A8D3-30E36FBECC5C}']
    //{class} function getApiKey: JString; cdecl;
    //{class} function getApplicationContext: JContext; cdecl;
    {class} function getInstance(context: JContext): JMapLibre; cdecl; overload;
    //{class} function getInstance(context: JContext; string_: JString; wellKnownTileServer: JWellKnownTileServer): JMapLibre; cdecl; overload;
    //{class} function getInstance(context: JContext; string_: JString; wellKnownTileServer: JWellKnownTileServer; type_: JRenderingEngine_Type): JMapLibre; cdecl; overload;
    //{class} function getModuleProvider: JModuleProvider; cdecl;
    //{class} function getPredefinedStyle(string_: JString): JDefaultStyle; cdecl;
    //{class} function getPredefinedStyles: TJavaObjectArray<JDefaultStyle>; cdecl;
    //{class} function getTileServerOptions: JTileServerOptions; cdecl;
    //{class} function hasInstance: Boolean; cdecl;
    //{class} function isConnected: JBoolean; cdecl;
    //{class} procedure setApiKey(string_: JString); cdecl;
    //{class} procedure setConnected(boolean: JBoolean); cdecl;
    //{class} procedure setModuleProvider(moduleProvider: JModuleProvider); cdecl;
    //{class} procedure throwIfApiKeyInvalid(string_: JString); cdecl;
  end;
  [JavaSignature('org/maplibre/android/MapLibre')]
  JMapLibre = interface(JObject)
    ['{A06FBC10-CC2D-4DA1-BA54-870BE09A0CBE}']
  end;
  TJMapLibre = class(TJavaGenericImport<JMapLibreClass, JMapLibre>) end;

  {*****************************************}
  JMapLibreMapClass = interface(JObjectClass)
    ['{96ACC57E-D5D0-4D63-BCC6-7685C1C2760B}']
  end;
  [JavaSignature('org/maplibre/android/maps/MapLibreMap')]
  JMapLibreMap = interface(JObject)
    ['{DD80198D-014D-47A8-8AEE-B45FAB031B79}']
    //function addMarker(markerOptions: Jannotations_MarkerOptions): Jannotations_Marker; cdecl; overload; // Deprecated
    //function addMarker(baseMarkerOptions: JBaseMarkerOptions): Jannotations_Marker; cdecl; overload; // Deprecated
    //function addMarkers(list: JList): JList; cdecl; // Deprecated
    procedure addOnCameraIdleListener(listener: JMapLibreMap_OnCameraIdleListener); cdecl;
    procedure addOnCameraMoveCancelListener(listener: JMapLibreMap_OnCameraMoveCanceledListener); cdecl;
    procedure addOnCameraMoveListener(listener: JMapLibreMap_OnCameraMoveListener); cdecl;
    procedure addOnCameraMoveStartedListener(listener: JMapLibreMap_OnCameraMoveStartedListener); cdecl;
    //procedure addOnFlingListener(onFlingListener: JMapLibreMap_OnFlingListener); cdecl;
    procedure addOnMapClickListener(listener: JMapLibreMap_OnMapClickListener); cdecl;
    //procedure addOnMapLongClickListener(onMapLongClickListener: JMapLibreMap_OnMapLongClickListener); cdecl;
    //procedure addOnMoveListener(onMoveListener: JMapLibreMap_OnMoveListener); cdecl;
    //procedure addOnRotateListener(onRotateListener: JMapLibreMap_OnRotateListener); cdecl;
    //procedure addOnScaleListener(onScaleListener: JMapLibreMap_OnScaleListener); cdecl;
    //procedure addOnShoveListener(onShoveListener: JMapLibreMap_OnShoveListener); cdecl;
    //function addPolygon(polygonOptions: Jannotations_PolygonOptions): Jannotations_Polygon; cdecl; // Deprecated
    //function addPolygons(list: JList): JList; cdecl; // Deprecated
    //function addPolyline(polylineOptions: Jannotations_PolylineOptions): Jannotations_Polyline; cdecl; // Deprecated
    //function addPolylines(list: JList): JList; cdecl; // Deprecated
    //procedure animateCamera(cameraUpdate: JCameraUpdate); cdecl; overload;
    //procedure animateCamera(cameraUpdate: JCameraUpdate; cancelableCallback: JMapLibreMap_CancelableCallback); cdecl; overload;
    //procedure animateCamera(cameraUpdate: JCameraUpdate; i: Integer); cdecl; overload;
    //procedure animateCamera(cameraUpdate: JCameraUpdate; i: Integer; cancelableCallback: JMapLibreMap_CancelableCallback); cdecl; overload;
    //procedure cancelAllVelocityAnimations; cdecl;
    //procedure cancelTransitions; cdecl;
    //procedure clear; cdecl; // Deprecated
    //procedure clearActionJournalLog; cdecl;
    //procedure cycleDebugOptions; cdecl; // Deprecated
    //procedure deselectMarker(marker: Jannotations_Marker); cdecl; // Deprecated
    //procedure deselectMarkers; cdecl; // Deprecated
    //procedure easeCamera(cameraUpdate: JCameraUpdate); cdecl; overload;
    //procedure easeCamera(cameraUpdate: JCameraUpdate; cancelableCallback: JMapLibreMap_CancelableCallback); cdecl; overload;
    //procedure easeCamera(cameraUpdate: JCameraUpdate; i: Integer); cdecl; overload;
    //procedure easeCamera(cameraUpdate: JCameraUpdate; i: Integer; cancelableCallback: JMapLibreMap_CancelableCallback); cdecl; overload;
    //procedure easeCamera(cameraUpdate: JCameraUpdate; i: Integer; b: Boolean); cdecl; overload;
    //procedure easeCamera(cameraUpdate: JCameraUpdate; i: Integer; b: Boolean; cancelableCallback: JMapLibreMap_CancelableCallback); cdecl; overload;
    //procedure enableRenderingStatsView(b: Boolean); cdecl;
    //function getActionJournalLog: TJavaObjectArray<JString>; cdecl;
    //function getActionJournalLogFiles: TJavaObjectArray<JString>; cdecl;
    //function getAnnotation(l: Int64): Jannotations_Annotation; cdecl; // Deprecated
    //function getAnnotations: JList; cdecl; // Deprecated
    //function getCameraForGeometry(geometry: JObject): JCameraPosition; cdecl; overload;
    //function getCameraForGeometry(geometry: JObject; i: TJavaArray<Integer>): JCameraPosition; cdecl; overload;
    //function getCameraForGeometry(geometry: JObject; d: Double; d1: Double): JCameraPosition; cdecl; overload;
    //function getCameraForGeometry(geometry: JObject; i: TJavaArray<Integer>; d: Double; d1: Double): JCameraPosition; cdecl; overload;
    //function getCameraForLatLngBounds(latLngBounds: JLatLngBounds): JCameraPosition; cdecl; overload;
    //function getCameraForLatLngBounds(latLngBounds: JLatLngBounds; i: TJavaArray<Integer>): JCameraPosition; cdecl; overload;
    //function getCameraForLatLngBounds(latLngBounds: JLatLngBounds; d: Double; d1: Double): JCameraPosition; cdecl; overload;
    //function getCameraForLatLngBounds(latLngBounds: JLatLngBounds; i: TJavaArray<Integer>; d: Double; d1: Double): JCameraPosition; cdecl; overload;
    function getCameraPosition: JCameraPosition; cdecl;
    //function getGesturesManager: JObject; cdecl;
    //function getHeight: Single; cdecl;
    //function getInfoWindowAdapter: JMapLibreMap_InfoWindowAdapter; cdecl; // Deprecated
    //function getLocationComponent: JLocationComponent; cdecl;
    //function getMarkers: JList; cdecl; // Deprecated
    //function getMaxPitch: Double; cdecl;
    //function getMaxZoomLevel: Double; cdecl;
    //function getMinPitch: Double; cdecl;
    //function getMinZoomLevel: Double; cdecl;
    //function getOnInfoWindowClickListener: JMapLibreMap_OnInfoWindowClickListener; cdecl;
    //function getOnInfoWindowCloseListener: JMapLibreMap_OnInfoWindowCloseListener; cdecl;
    //function getOnInfoWindowLongClickListener: JMapLibreMap_OnInfoWindowLongClickListener; cdecl;
    //function getPadding: TJavaArray<Integer>; cdecl; // Deprecated
    //function getPolygons: JList; cdecl; // Deprecated
    //function getPolylines: JList; cdecl; // Deprecated
    //function getPrefetchZoomDelta: Integer; cdecl;
    //function getPrefetchesTiles: Boolean; cdecl; // Deprecated
    function getProjection: JProjection; cdecl;
    //function getSelectedMarkers: JList; cdecl; // Deprecated
    //procedure getStyle(onStyleLoaded: JStyle_OnStyleLoaded); cdecl; overload;
    function getStyle: JStyle; cdecl; overload;
    //function getTileCacheEnabled: Boolean; cdecl;
    //function getTileLodMinRadius: Double; cdecl;
    //function getTileLodPitchThreshold: Double; cdecl;
    //function getTileLodScale: Double; cdecl;
    //function getTileLodZoomShift: Double; cdecl;
    function getUiSettings: JUiSettings; cdecl;
    //function getWidth: Single; cdecl;
    //function getZoom: Double; cdecl;
    //function isAllowConcurrentMultipleOpenInfoWindows: Boolean; cdecl; // Deprecated
    //function isDebugActive: Boolean; cdecl;
    //function isRenderingStatsViewEnabled: Boolean; cdecl;
    procedure moveCamera(update: JCameraUpdate); cdecl; overload;
    //procedure moveCamera(cameraUpdate: JCameraUpdate; cancelableCallback: JMapLibreMap_CancelableCallback); cdecl; overload;
    function queryRenderedFeatures(coordinates: JPointF; layerIds: TJavaObjectArray<JString>): JList; cdecl; overload;
    function queryRenderedFeatures(coordinates: JPointF; filter: JExpression; layerIds: TJavaObjectArray<JString>): JList; cdecl; overload;
    function queryRenderedFeatures(coordinates: JRectF; layerIds: TJavaObjectArray<JString>): JList; cdecl; overload;
    function queryRenderedFeatures(coordinates: JRectF; filter: JExpression; layerIds: TJavaObjectArray<JString>): JList; cdecl; overload;
    //procedure removeAnnotation(annotation: Jannotations_Annotation); cdecl; overload; // Deprecated
    //procedure removeAnnotation(l: Int64); cdecl; overload; // Deprecated
    //procedure removeAnnotations(list: JList); cdecl; overload; // Deprecated
    //procedure removeAnnotations; cdecl; overload; // Deprecated
    //procedure removeMarker(marker: Jannotations_Marker); cdecl; // Deprecated
    procedure removeOnCameraIdleListener(listener: JMapLibreMap_OnCameraIdleListener); cdecl;
    procedure removeOnCameraMoveCancelListener(listener: JMapLibreMap_OnCameraMoveCanceledListener); cdecl;
    procedure removeOnCameraMoveListener(listener: JMapLibreMap_OnCameraMoveListener); cdecl;
    procedure removeOnCameraMoveStartedListener(listener: JMapLibreMap_OnCameraMoveStartedListener); cdecl;
    //procedure removeOnFlingListener(onFlingListener: JMapLibreMap_OnFlingListener); cdecl;
    procedure removeOnMapClickListener(listener: JMapLibreMap_OnMapClickListener); cdecl;
    //procedure removeOnMapLongClickListener(onMapLongClickListener: JMapLibreMap_OnMapLongClickListener); cdecl;
    //procedure removeOnMoveListener(onMoveListener: JMapLibreMap_OnMoveListener); cdecl;
    //procedure removeOnRotateListener(onRotateListener: JMapLibreMap_OnRotateListener); cdecl;
    //procedure removeOnScaleListener(onScaleListener: JMapLibreMap_OnScaleListener); cdecl;
    //procedure removeOnShoveListener(onShoveListener: JMapLibreMap_OnShoveListener); cdecl;
    //procedure removePolygon(polygon: Jannotations_Polygon); cdecl; // Deprecated
    //procedure removePolyline(polyline: Jannotations_Polyline); cdecl; // Deprecated
    //procedure resetNorth; cdecl;
    //procedure scrollBy(f: Single; f1: Single); cdecl; overload;
    //procedure scrollBy(f: Single; f1: Single; l: Int64); cdecl; overload;
    //procedure selectMarker(marker: Jannotations_Marker); cdecl; // Deprecated
    //procedure setAllowConcurrentMultipleOpenInfoWindows(b: Boolean); cdecl; // Deprecated
    //procedure setCameraPosition(cameraPosition: JCameraPosition); cdecl;
    //procedure setDebugActive(b: Boolean); cdecl;
    //procedure setFocalBearing(d: Double; f: Single; f1: Single; l: Int64); cdecl;
    //procedure setFrustumOffset(rectF: JRectF); cdecl;
    //procedure setGesturesManager(androidGesturesManager: JObject; b: Boolean; b1: Boolean); cdecl;
    //procedure setInfoWindowAdapter(infoWindowAdapter: JMapLibreMap_InfoWindowAdapter); cdecl; // Deprecated
    //procedure setLatLngBoundsForCameraTarget(latLngBounds: JLatLngBounds); cdecl;
    //procedure setMaxPitchPreference(d: Double); cdecl;
    //procedure setMaxZoomPreference(d: Double); cdecl;
    //procedure setMinPitchPreference(d: Double); cdecl;
    //procedure setMinZoomPreference(d: Double); cdecl;
    //procedure setOfflineRegionDefinition(offlineRegionDefinition: JOfflineRegionDefinition); cdecl; overload;
    //procedure setOfflineRegionDefinition(offlineRegionDefinition: JOfflineRegionDefinition; onStyleLoaded: JStyle_OnStyleLoaded); cdecl; overload;
    //procedure setOnFpsChangedListener(onFpsChangedListener: JMapLibreMap_OnFpsChangedListener); cdecl;
    //procedure setOnInfoWindowClickListener(onInfoWindowClickListener: JMapLibreMap_OnInfoWindowClickListener); cdecl;
    //procedure setOnInfoWindowCloseListener(onInfoWindowCloseListener: JMapLibreMap_OnInfoWindowCloseListener); cdecl;
    //procedure setOnInfoWindowLongClickListener(onInfoWindowLongClickListener: JMapLibreMap_OnInfoWindowLongClickListener); cdecl;
    //procedure setOnMarkerClickListener(onMarkerClickListener: JMapLibreMap_OnMarkerClickListener); cdecl; // Deprecated
    //procedure setOnPolygonClickListener(onPolygonClickListener: JMapLibreMap_OnPolygonClickListener); cdecl; // Deprecated
    //procedure setOnPolylineClickListener(onPolylineClickListener: JMapLibreMap_OnPolylineClickListener); cdecl; // Deprecated
    //procedure setPadding(i: Integer; i1: Integer; i2: Integer; i3: Integer); cdecl; // Deprecated
    //procedure setPrefetchZoomDelta(i: Integer); cdecl;
    //procedure setPrefetchesTiles(b: Boolean); cdecl; // Deprecated
    //procedure setStyle(style: JString); cdecl; overload;
    //procedure setStyle(string_: JString; onStyleLoaded: JStyle_OnStyleLoaded); cdecl; overload;
    procedure setStyle(builder: JStyle_Builder); cdecl; overload;
    //procedure setStyle(builder: JStyle_Builder; onStyleLoaded: JStyle_OnStyleLoaded); cdecl; overload;
    //procedure setSwapBehaviorFlush(b: Boolean); cdecl;
    //procedure setTileCacheEnabled(b: Boolean); cdecl;
    //procedure setTileLodMinRadius(d: Double); cdecl;
    //procedure setTileLodPitchThreshold(d: Double); cdecl;
    //procedure setTileLodScale(d: Double); cdecl;
    //procedure setTileLodZoomShift(d: Double); cdecl;
    //procedure setZoom(d: Double; pointF: JPointF; l: Int64); cdecl;
    //procedure snapshot(snapshotReadyCallback: JMapLibreMap_SnapshotReadyCallback); cdecl;
    //procedure triggerRepaint; cdecl;
    //procedure updateMarker(marker: Jannotations_Marker); cdecl; // Deprecated
    //procedure updatePolygon(polygon: Jannotations_Polygon); cdecl; // Deprecated
    //procedure updatePolyline(polyline: Jannotations_Polyline); cdecl; // Deprecated
  end;
  TJMapLibreMap = class(TJavaGenericImport<JMapLibreMapClass, JMapLibreMap>) end;

  {************************************************}
  JMapLibreMap_OnCameraMoveStartedListenerClass = interface(IJavaClass)
    ['{98C9E64B-2A67-40EF-8011-3152FD3F94E5}']
    {class} function _GetREASON_API_ANIMATION: Integer; cdecl;
    {class} function _GetREASON_API_GESTURE: Integer; cdecl;
    {class} function _GetREASON_DEVELOPER_ANIMATION: Integer; cdecl;
    {class} property REASON_API_ANIMATION: Integer read _GetREASON_API_ANIMATION;
    {class} property REASON_API_GESTURE: Integer read _GetREASON_API_GESTURE;
    {class} property REASON_DEVELOPER_ANIMATION: Integer read _GetREASON_DEVELOPER_ANIMATION;
  end;
  [JavaSignature('org/maplibre/android/maps/MapLibreMap$OnCameraMoveStartedListener')]
  JMapLibreMap_OnCameraMoveStartedListener = interface(IJavaInstance)
    ['{96580AEC-E844-4850-8A18-3B695A8EA0D3}']
    procedure onCameraMoveStarted(reason: Integer); cdecl;
  end;
  TJMapLibreMap_OnCameraMoveStartedListener = class(TJavaGenericImport<JMapLibreMap_OnCameraMoveStartedListenerClass, JMapLibreMap_OnCameraMoveStartedListener>) end;

  {************************************************}
  JMapLibreMap_OnCameraMoveListenerClass = interface(IJavaClass)
    ['{7D559CE6-45FD-497B-88E1-437EB737F7A7}']
  end;
  [JavaSignature('org/maplibre/android/maps/MapLibreMap$OnCameraMoveListener')]
  JMapLibreMap_OnCameraMoveListener = interface(IJavaInstance)
    ['{E52D280B-7468-462C-A9F2-731E5FFF0A2C}']
    procedure onCameraMove; cdecl;
  end;
  TJMapLibreMap_OnCameraMoveListener = class(TJavaGenericImport<JMapLibreMap_OnCameraMoveListenerClass, JMapLibreMap_OnCameraMoveListener>) end;

  {************************************************}
  JMapLibreMap_OnCameraMoveCanceledListenerClass = interface(IJavaClass)
    ['{8CD1152E-7A81-41C9-9A24-7968BCAF2449}']
  end;
  [JavaSignature('org/maplibre/android/maps/MapLibreMap$OnCameraMoveCanceledListener')]
  JMapLibreMap_OnCameraMoveCanceledListener = interface(IJavaInstance)
    ['{F72E13B0-F7D9-4573-9E77-891C0209CEC7}']
    procedure onCameraMoveCanceled; cdecl;
  end;
  TJMapLibreMap_OnCameraMoveCanceledListener = class(TJavaGenericImport<JMapLibreMap_OnCameraMoveCanceledListenerClass, JMapLibreMap_OnCameraMoveCanceledListener>) end;

  {************************************************}
  JMapLibreMap_OnCameraIdleListenerClass = interface(IJavaClass)
    ['{CAAAF97C-FC79-4CF1-84E6-EB1FF8ED8A56}']
  end;
  [JavaSignature('org/maplibre/android/maps/MapLibreMap$OnCameraIdleListener')]
  JMapLibreMap_OnCameraIdleListener = interface(IJavaInstance)
    ['{49A83216-ED36-42C5-BEFD-5B55AC555BA0}']
    procedure onCameraIdle; cdecl;
  end;
  TJMapLibreMap_OnCameraIdleListener = class(TJavaGenericImport<JMapLibreMap_OnCameraIdleListenerClass, JMapLibreMap_OnCameraIdleListener>) end;

  {************************************************}
  JMapLibreMap_OnMapClickListenerClass = interface(IJavaClass)
    ['{EC24BC8F-5B3B-4553-B669-BD79DF18A845}']
  end;
  [JavaSignature('org/maplibre/android/maps/MapLibreMap$OnMapClickListener')]
  JMapLibreMap_OnMapClickListener = interface(IJavaInstance)
    ['{84D0E98F-8012-4628-9E6D-D58CB80062D4}']
    function onMapClick(point: JLatLng): Boolean; cdecl;
  end;
  TJMapLibreMap_OnMapClickListener = class(TJavaGenericImport<JMapLibreMap_OnMapClickListenerClass, JMapLibreMap_OnMapClickListener>) end;


  {************************************************}
  JMapLibreMapOptionsClass = interface(JObjectClass)
    ['{13C1C3BB-2658-4FDA-B464-76E3A7EACD08}']
    //{class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init: JMapLibreMapOptions; cdecl;
    //{class} function createFromAttributes(context: JContext): JMapLibreMapOptions; cdecl; overload;
    //{class} function createFromAttributes(context: JContext; attributeSet: JAttributeSet): JMapLibreMapOptions; cdecl; overload;
    //{class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;
  [JavaSignature('org/maplibre/android/maps/MapLibreMapOptions')]
  JMapLibreMapOptions = interface(JObject)
    ['{DEB79839-1682-4DDF-9912-8052230CC599}']
    //function actionJournalEnabled(b: Boolean): JMapLibreMapOptions; cdecl;
    //function actionJournalLogFileCount(l: Int64): JMapLibreMapOptions; cdecl;
    //function actionJournalLogFileSize(l: Int64): JMapLibreMapOptions; cdecl;
    //function actionJournalPath(string_: JString): JMapLibreMapOptions; cdecl;
    //function actionJournalRenderingReportInterval(i: Integer): JMapLibreMapOptions; cdecl;
    //function apiBaseUri(string_: JString): JMapLibreMapOptions; cdecl;
    //function apiBaseUrl(string_: JString): JMapLibreMapOptions; cdecl; // Deprecated
    //function asyncRendererCleanup(b: Boolean): JMapLibreMapOptions; cdecl;
    //function attributionEnabled(b: Boolean): JMapLibreMapOptions; cdecl;
    //function attributionGravity(i: Integer): JMapLibreMapOptions; cdecl;
    //function attributionMargins(i: TJavaArray<Integer>): JMapLibreMapOptions; cdecl;
    //function attributionTintColor(i: Integer): JMapLibreMapOptions; cdecl;
    //function camera(cameraPosition: JCameraPosition): JMapLibreMapOptions; cdecl;
    //function compassEnabled(b: Boolean): JMapLibreMapOptions; cdecl;
    //function compassFadesWhenFacingNorth(b: Boolean): JMapLibreMapOptions; cdecl;
    //function compassGravity(i: Integer): JMapLibreMapOptions; cdecl;
    //function compassImage(drawable: JDrawable): JMapLibreMapOptions; cdecl;
    //function compassMargins(i: TJavaArray<Integer>): JMapLibreMapOptions; cdecl;
    //function crossSourceCollisions(b: Boolean): JMapLibreMapOptions; cdecl;
    //function debugActive(b: Boolean): JMapLibreMapOptions; cdecl;
    //function describeContents: Integer; cdecl;
    //function doubleTapGesturesEnabled(b: Boolean): JMapLibreMapOptions; cdecl;
    //function equals(object_: JObject): Boolean; cdecl;
    //function fastPFOREnabled(b: Boolean): JMapLibreMapOptions; cdecl;
    //function foregroundLoadColor(i: Integer): JMapLibreMapOptions; cdecl;
    //function getActionJournalEnabled: Boolean; cdecl;
    //function getActionJournalLogFileCount: Int64; cdecl;
    //function getActionJournalLogFileSize: Int64; cdecl;
    //function getActionJournalPath: JString; cdecl;
    //function getActionJournalRenderingReportInterval: Integer; cdecl;
    //function getApiBaseUri: JString; cdecl;
    //function getApiBaseUrl: JString; cdecl; // Deprecated
    //function getAsyncRendererCleanup: Boolean; cdecl;
    //function getAttributionEnabled: Boolean; cdecl;
    //function getAttributionGravity: Integer; cdecl;
    //function getAttributionMargins: TJavaArray<Integer>; cdecl;
    //function getAttributionTintColor: Integer; cdecl;
    //function getCamera: JCameraPosition; cdecl;
    //function getCompassEnabled: Boolean; cdecl;
    //function getCompassFadeFacingNorth: Boolean; cdecl;
    //function getCompassGravity: Integer; cdecl;
    //function getCompassImage: JDrawable; cdecl;
    //function getCompassMargins: TJavaArray<Integer>; cdecl;
    //function getCrossSourceCollisions: Boolean; cdecl;
    //function getDebugActive: Boolean; cdecl;
    //function getDoubleTapGesturesEnabled: Boolean; cdecl;
    //function getFastPFOREnabled: Boolean; cdecl;
    //function getForegroundLoadColor: Integer; cdecl;
    //function getHorizontalScrollGesturesEnabled: Boolean; cdecl;
    //function getLocalIdeographFontFamily: JString; cdecl;
    //function getLogoEnabled: Boolean; cdecl;
    //function getLogoGravity: Integer; cdecl;
    //function getLogoMargins: TJavaArray<Integer>; cdecl;
    //function getMaxPitchPreference: Double; cdecl;
    //function getMaxZoomPreference: Double; cdecl;
    //function getMinPitchPreference: Double; cdecl;
    //function getMinZoomPreference: Double; cdecl;
    //function getPixelRatio: Single; cdecl;
    //function getPrefetchZoomDelta: Integer; cdecl;
    //function getPrefetchesTiles: Boolean; cdecl; // Deprecated
    //function getQuickZoomGesturesEnabled: Boolean; cdecl;
    //function getRenderSurfaceOnTop: Boolean; cdecl;
    //function getRotateGesturesEnabled: Boolean; cdecl;
    //function getScrollGesturesEnabled: Boolean; cdecl;
    //function getTextureMode: Boolean; cdecl;
    //function getTiltGesturesEnabled: Boolean; cdecl;
    //function getTranslucentTextureSurface: Boolean; cdecl;
    //function getZoomGesturesEnabled: Boolean; cdecl;
    //function hashCode: Integer; cdecl;
    //function horizontalScrollGesturesEnabled(b: Boolean): JMapLibreMapOptions; cdecl;
    //function isLocalIdeographFontFamilyEnabled: Boolean; cdecl;
    //function localIdeographFontFamily(string_: JString): JMapLibreMapOptions; cdecl; overload;
    //function localIdeographFontFamily(string_: TJavaObjectArray<JString>): JMapLibreMapOptions; cdecl; overload;
    //function localIdeographFontFamilyEnabled(b: Boolean): JMapLibreMapOptions; cdecl;
    //function logoEnabled(b: Boolean): JMapLibreMapOptions; cdecl;
    //function logoGravity(i: Integer): JMapLibreMapOptions; cdecl;
    //function logoMargins(i: TJavaArray<Integer>): JMapLibreMapOptions; cdecl;
    //function maxPitchPreference(d: Double): JMapLibreMapOptions; cdecl;
    //function maxZoomPreference(d: Double): JMapLibreMapOptions; cdecl;
    //function minPitchPreference(d: Double): JMapLibreMapOptions; cdecl;
    //function minZoomPreference(d: Double): JMapLibreMapOptions; cdecl;
    //function pixelRatio(f: Single): JMapLibreMapOptions; cdecl;
    //function quickZoomGesturesEnabled(b: Boolean): JMapLibreMapOptions; cdecl;
    //procedure renderSurfaceOnTop(b: Boolean); cdecl;
    //function rotateGesturesEnabled(b: Boolean): JMapLibreMapOptions; cdecl;
    //function scrollGesturesEnabled(b: Boolean): JMapLibreMapOptions; cdecl;
    //function setPrefetchZoomDelta(i: Integer): JMapLibreMapOptions; cdecl;
    //function setPrefetchesTiles(b: Boolean): JMapLibreMapOptions; cdecl; // Deprecated
    function textureMode(textureMode: Boolean): JMapLibreMapOptions; cdecl;
    //function tiltGesturesEnabled(b: Boolean): JMapLibreMapOptions; cdecl;
    //function translucentTextureSurface(b: Boolean): JMapLibreMapOptions; cdecl;
    //procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
    //function zoomGesturesEnabled(b: Boolean): JMapLibreMapOptions; cdecl;
  end;
  TJMapLibreMapOptions = class(TJavaGenericImport<JMapLibreMapOptionsClass, JMapLibreMapOptions>) end;

  {******************************************}
  JMapViewClass = interface(JFrameLayoutClass)
    ['{7EDE8419-80A2-48C2-BE95-A07FD33DFCA6}']
    //{class} function init(context: JContext): JMapView; cdecl; overload;
    //{class} function init(context: JContext; attributeSet: JAttributeSet): JMapView; cdecl; overload;
    //{class} function init(context: JContext; attributeSet: JAttributeSet; i: Integer): JMapView; cdecl; overload;
    {class} function init(context: JContext; mapLibreMapOptions: JMapLibreMapOptions): JMapView; cdecl; overload;
    //{class} procedure setMapStrictModeEnabled(b: Boolean); cdecl;
  end;
  [JavaSignature('org/maplibre/android/maps/MapView')]
  JMapView = interface(JFrameLayout)
    ['{CAC13368-7DA9-4940-9C09-E00CA6A86B19}']
    procedure addOnCameraDidChangeListener(listener: JMapView_OnCameraDidChangeListener); cdecl;
    procedure addOnCameraIsChangingListener(listener: JMapView_OnCameraIsChangingListener); cdecl;
    procedure addOnCameraWillChangeListener(listener: JMapView_OnCameraWillChangeListener); cdecl;
    //procedure addOnCanRemoveUnusedStyleImageListener(onCanRemoveUnusedStyleImageListener: JMapView_OnCanRemoveUnusedStyleImageListener); cdecl;
    //procedure addOnDidBecomeIdleListener(onDidBecomeIdleListener: JMapView_OnDidBecomeIdleListener); cdecl;
    //procedure addOnDidFailLoadingMapListener(onDidFailLoadingMapListener: JMapView_OnDidFailLoadingMapListener); cdecl;
    //procedure addOnDidFinishLoadingMapListener(onDidFinishLoadingMapListener: JMapView_OnDidFinishLoadingMapListener); cdecl;
    procedure addOnDidFinishLoadingStyleListener(listener: JMapView_OnDidFinishLoadingStyleListener); cdecl;
    //procedure addOnDidFinishRenderingFrameListener(onDidFinishRenderingFrameListener: JMapView_OnDidFinishRenderingFrameListener); cdecl; overload;
    //procedure addOnDidFinishRenderingFrameListener(onDidFinishRenderingFrameWithStatsListener: JMapView_OnDidFinishRenderingFrameWithStatsListener); cdecl; overload;
    //procedure addOnDidFinishRenderingMapListener(onDidFinishRenderingMapListener: JMapView_OnDidFinishRenderingMapListener); cdecl;
    //procedure addOnGlyphsErrorListener(onGlyphsErrorListener: JMapView_OnGlyphsErrorListener); cdecl;
    //procedure addOnGlyphsLoadedListener(onGlyphsLoadedListener: JMapView_OnGlyphsLoadedListener); cdecl;
    //procedure addOnGlyphsRequestedListener(onGlyphsRequestedListener: JMapView_OnGlyphsRequestedListener); cdecl;
    //procedure addOnPostCompileShaderListener(onPostCompileShaderListener: JMapView_OnPostCompileShaderListener); cdecl;
    //procedure addOnPreCompileShaderListener(onPreCompileShaderListener: JMapView_OnPreCompileShaderListener); cdecl;
    //procedure addOnRenderErrorListener(onRenderErrorListener: JMapView_OnRenderErrorListener); cdecl;
    //procedure addOnShaderCompileFailedListener(onShaderCompileFailedListener: JMapView_OnShaderCompileFailedListener); cdecl;
    //procedure addOnSourceChangedListener(onSourceChangedListener: JMapView_OnSourceChangedListener); cdecl;
    //procedure addOnSpriteErrorListener(onSpriteErrorListener: JMapView_OnSpriteErrorListener); cdecl;
    //procedure addOnSpriteLoadedListener(onSpriteLoadedListener: JMapView_OnSpriteLoadedListener); cdecl;
    //procedure addOnSpriteRequestedListener(onSpriteRequestedListener: JMapView_OnSpriteRequestedListener); cdecl;
    //procedure addOnStyleImageMissingListener(onStyleImageMissingListener: JMapView_OnStyleImageMissingListener); cdecl;
    //procedure addOnSymbolErrorListener(onSymbolErrorListener: JMapView_OnSymbolErrorListener); cdecl;
    //procedure addOnTileActionListener(onTileActionListener: JMapView_OnTileActionListener); cdecl;
    //procedure addOnWillStartLoadingMapListener(onWillStartLoadingMapListener: JMapView_OnWillStartLoadingMapListener); cdecl;
    //procedure addOnWillStartRenderingFrameListener(onWillStartRenderingFrameListener: JMapView_OnWillStartRenderingFrameListener); cdecl;
    //procedure addOnWillStartRenderingMapListener(onWillStartRenderingMapListener: JMapView_OnWillStartRenderingMapListener); cdecl;
    procedure getMapAsync(callback: JOnMapReadyCallback); cdecl;
    //function getPixelRatio: Single; cdecl;
    //function getRenderView: JView; cdecl;
    //function getRenderingRefreshMode: JMapRenderer_RenderingRefreshMode; cdecl;
    //function getViewContent: JBitmap; cdecl;
    //function isDestroyed: Boolean; cdecl;
    procedure onCreate(bundle: JBundle); cdecl;
    procedure onDestroy; cdecl;
    //function onGenericMotionEvent(motionEvent: JMotionEvent): Boolean; cdecl;
    //function onKeyDown(i: Integer; keyEvent: JKeyEvent): Boolean; cdecl;
    //function onKeyLongPress(i: Integer; keyEvent: JKeyEvent): Boolean; cdecl;
    //function onKeyUp(i: Integer; keyEvent: JKeyEvent): Boolean; cdecl;
    procedure onLowMemory; cdecl;
    procedure onPause; cdecl;
    procedure onResume; cdecl;
    //procedure onSaveInstanceState(bundle: JBundle); cdecl;
    procedure onStart; cdecl;
    procedure onStop; cdecl;
    //function onTouchEvent(motionEvent: JMotionEvent): Boolean; cdecl;
    //function onTrackballEvent(motionEvent: JMotionEvent): Boolean; cdecl;
    //procedure queueEvent(runnable: JRunnable); cdecl;
    procedure removeOnCameraDidChangeListener(listener: JMapView_OnCameraDidChangeListener); cdecl;
    procedure removeOnCameraIsChangingListener(listener: JMapView_OnCameraIsChangingListener); cdecl;
    procedure removeOnCameraWillChangeListener(listener: JMapView_OnCameraWillChangeListener); cdecl;
    //procedure removeOnCanRemoveUnusedStyleImageListener(onCanRemoveUnusedStyleImageListener: JMapView_OnCanRemoveUnusedStyleImageListener); cdecl;
    //procedure removeOnDidBecomeIdleListener(onDidBecomeIdleListener: JMapView_OnDidBecomeIdleListener); cdecl;
    //procedure removeOnDidFailLoadingMapListener(onDidFailLoadingMapListener: JMapView_OnDidFailLoadingMapListener); cdecl;
    //procedure removeOnDidFinishLoadingMapListener(onDidFinishLoadingMapListener: JMapView_OnDidFinishLoadingMapListener); cdecl;
    procedure removeOnDidFinishLoadingStyleListener(listener: JMapView_OnDidFinishLoadingStyleListener); cdecl;
    //procedure removeOnDidFinishRenderingFrameListener(onDidFinishRenderingFrameListener: JMapView_OnDidFinishRenderingFrameListener); cdecl; overload;
    //procedure removeOnDidFinishRenderingFrameListener(onDidFinishRenderingFrameWithStatsListener: JMapView_OnDidFinishRenderingFrameWithStatsListener); cdecl; overload;
    //procedure removeOnDidFinishRenderingMapListener(onDidFinishRenderingMapListener: JMapView_OnDidFinishRenderingMapListener); cdecl;
    //procedure removeOnGlyphsErrorListener(onGlyphsErrorListener: JMapView_OnGlyphsErrorListener); cdecl;
    //procedure removeOnGlyphsLoadedListener(onGlyphsLoadedListener: JMapView_OnGlyphsLoadedListener); cdecl;
    //procedure removeOnGlyphsRequestedListener(onGlyphsRequestedListener: JMapView_OnGlyphsRequestedListener); cdecl;
    //procedure removeOnPostCompileShaderListener(onPostCompileShaderListener: JMapView_OnPostCompileShaderListener); cdecl;
    //procedure removeOnPreCompileShaderListener(onPreCompileShaderListener: JMapView_OnPreCompileShaderListener); cdecl;
    //procedure removeOnRenderErrorListener(onRenderErrorListener: JMapView_OnRenderErrorListener); cdecl;
    //procedure removeOnShaderCompileFailedListener(onShaderCompileFailedListener: JMapView_OnShaderCompileFailedListener); cdecl;
    //procedure removeOnSourceChangedListener(onSourceChangedListener: JMapView_OnSourceChangedListener); cdecl;
    //procedure removeOnSpriteErrorListener(onSpriteErrorListener: JMapView_OnSpriteErrorListener); cdecl;
    //procedure removeOnSpriteLoadedListener(onSpriteLoadedListener: JMapView_OnSpriteLoadedListener); cdecl;
    //procedure removeOnSpriteRequestedListener(onSpriteRequestedListener: JMapView_OnSpriteRequestedListener); cdecl;
    //procedure removeOnStyleImageMissingListener(onStyleImageMissingListener: JMapView_OnStyleImageMissingListener); cdecl;
    //procedure removeOnSymbolErrorListener(onSymbolErrorListener: JMapView_OnSymbolErrorListener); cdecl;
    //procedure removeOnTileActionListener(onTileActionListener: JMapView_OnTileActionListener); cdecl;
    //procedure removeOnWillStartLoadingMapListener(onWillStartLoadingMapListener: JMapView_OnWillStartLoadingMapListener); cdecl;
    //procedure removeOnWillStartRenderingFrameListener(onWillStartRenderingFrameListener: JMapView_OnWillStartRenderingFrameListener); cdecl;
    //procedure removeOnWillStartRenderingMapListener(onWillStartRenderingMapListener: JMapView_OnWillStartRenderingMapListener); cdecl;
    //procedure setMaximumFps(i: Integer); cdecl;
    //procedure setRenderingRefreshMode(renderingRefreshMode: JMapRenderer_RenderingRefreshMode); cdecl;
  end;
  TJMapView = class(TJavaGenericImport<JMapViewClass, JMapView>) end;

  {**************************************************************}
  JMapView_OnCameraWillChangeListenerClass = interface(IJavaClass)
    ['{1F1D0296-3A71-4FE4-9A58-220BF0FA9A7A}']
  end;
  [JavaSignature('org/maplibre/android/maps/MapView$OnCameraWillChangeListener')]
  JMapView_OnCameraWillChangeListener = interface(IJavaInstance)
    ['{9CECC423-FBD4-4105-9BA9-CC413283F0A7}']
    procedure onCameraWillChange(animated: Boolean); cdecl;
  end;
  TJMapView_OnCameraWillChangeListener = class(TJavaGenericImport<JMapView_OnCameraWillChangeListenerClass, JMapView_OnCameraWillChangeListener>) end;

  {**************************************************************}
  JMapView_OnCameraIsChangingListenerClass = interface(IJavaClass)
    ['{7B76BAF1-5B5C-44BE-86F1-DA866CC01AF2}']
  end;
  [JavaSignature('org/maplibre/android/maps/MapView$OnCameraIsChangingListener')]
  JMapView_OnCameraIsChangingListener = interface(IJavaInstance)
    ['{D1A13E77-AB57-43B1-B19E-2B2205113484}']
    procedure onCameraIsChanging; cdecl;
  end;
  TJMapView_OnCameraIsChangingListener = class(TJavaGenericImport<JMapView_OnCameraIsChangingListenerClass, JMapView_OnCameraIsChangingListener>) end;

  {*************************************************************}
  JMapView_OnCameraDidChangeListenerClass = interface(IJavaClass)
    ['{82A51802-D6A0-458E-9458-F9114C856961}']
  end;
  [JavaSignature('org/maplibre/android/maps/MapView$OnCameraDidChangeListener')]
  JMapView_OnCameraDidChangeListener = interface(IJavaInstance)
    ['{3AE428A9-C84B-4C8E-AC12-29B67885464E}']
    procedure onCameraDidChange(animated: Boolean); cdecl;
  end;
  TJMapView_OnCameraDidChangeListener = class(TJavaGenericImport<JMapView_OnCameraDidChangeListenerClass, JMapView_OnCameraDidChangeListener>) end;

  {*******************************************************************}
  JMapView_OnDidFinishLoadingStyleListenerClass = interface(IJavaClass)
    ['{DB2F1988-9349-4D6C-8676-EC87E3E6A4EE}']
  end;
  [JavaSignature('org/maplibre/android/maps/MapView$OnDidFinishLoadingStyleListener')]
  JMapView_OnDidFinishLoadingStyleListener = interface(IJavaInstance)
    ['{B333D18A-9710-4488-BC5F-B7DD7B2F7BCF}']
    procedure onDidFinishLoadingStyle; cdecl;
  end;
  TJMapView_OnDidFinishLoadingStyleListener = class(TJavaGenericImport<JMapView_OnDidFinishLoadingStyleListenerClass, JMapView_OnDidFinishLoadingStyleListener>) end;

  {***************************************************}
  JOnMapReadyCallbackClass = interface(IJavaClass)
    ['{474C8C24-F79B-4B5B-86D4-E1A52F2A6FD2}']
  end;
  [JavaSignature('org/maplibre/android/maps/OnMapReadyCallback')]
  JOnMapReadyCallback = interface(IJavaInstance)
    ['{EBBE7A9F-DF67-4D48-B73E-6B0ACBB466B9}']
    procedure onMapReady(maplibreMap: JMapLibreMap); cdecl;
  end;
  TJOnMapReadyCallback = class(TJavaGenericImport<JOnMapReadyCallbackClass, JOnMapReadyCallback>) end;

implementation

uses
  Alcinoe.Common;

{**********************}
procedure RegisterTypes;
begin
(*
  TRegTypes.RegisterType('Alcinoe.AndroidApi.MapLibre.JMapLibre', TypeInfo(Alcinoe.AndroidApi.MapLibre.JMapLibre));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.MapLibre.JMapLibreMap', TypeInfo(Alcinoe.AndroidApi.MapLibre.JMapLibreMap));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.MapLibre.JMapLibreMapOptions', TypeInfo(Alcinoe.AndroidApi.MapLibre.JMapLibreMapOptions));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.MapLibre.JMapView', TypeInfo(Alcinoe.AndroidApi.MapLibre.JMapView));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.MapLibre.JOnMapReadyCallback', TypeInfo(Alcinoe.AndroidApi.MapLibre.JOnMapReadyCallback));
*)
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.AndroidApi.MapLibre','initialization');
  {$ENDIF}
  RegisterTypes;

end.
