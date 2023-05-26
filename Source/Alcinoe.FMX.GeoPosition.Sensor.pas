(*******************************************************************************
The TALGeoPositionSensor component is a Delphi component that grants access to
location services on iOS and Android devices. It enables the retrieval of the
device's current location, and can provide location updates as the device's
location changes. It supports a range of location providers, including GPS,
cellular network triangulation, and Wi-Fi positioning.

Aside from granting access to location services, TALGeoPositionSensor also
automates the process of acquiring the user's permission to use the location
sensor on both iOS and Android devices. The component can handle situations
where the user has previously refused access to their location. The
TALGeoPositionSensor component provides a comprehensive solution for developers
seeking to integrate location-based functionality into their apps without
having to worry about low-level implementation details.

Setup (ANDROID)
---------------

1) On android you need to include the library
   * com.google.android.gms:play-services-base:18.2.0
   * com.google.android.gms:play-services-location:21.0.1
   in the project. You can do this with the help of AndroidMerger. You can see
   an exemple in <Alcinoe>\Demos\ALGeoPositionSensor\_source\Android\MergeLibraries.bat

2) In the AndroidManifest.template.xml file, you need to add the following
   permissions based on your requirements:
   * <uses-permission android:name="android.permission.ACCESS_COARSE_LOCATION"/>
   * <uses-permission android:name="android.permission.ACCESS_FINE_LOCATION"/>
   * <uses-permission android:name="android.permission.ACCESS_BACKGROUND_LOCATION" />


Setup (IOS)
-----------

1) NSLocationDefaultAccuracyReduced
   Set the key value to true to prompt the user for reduced accuracy by
   default; set it to false to prompt for full location accuracy. If you don't
   include that key in your Info.plist, that's equivalent to setting it to false.
     <!-- Info.plist -->
     <key>NSLocationDefaultAccuracyReduced</key>
     <true/>

2) NSLocationAlwaysAndWhenInUseUsageDescription
   Use this key if your iOS app accesses location information while running in
   the background. If your app only needs location information when in the
   foreground, use NSLocationWhenInUseUsageDescription instead.
     <!-- Info.plist -->
     <key>NSLocationAlwaysUsageDescription</key>
     <string>The reason for accessing the location information of the user</string>

3) NSLocationAlwaysUsageDescription (Deprecated in ios 11)
   Use this key if your iOS app accesses location information in the
   background, and you deploy to a target earlier than iOS 11. In that case,
   add both this key and NSLocationAlwaysAndWhenInUseUsageDescription to
   your app’s Info.plist file with the same message.
     <!-- Info.plist -->
     <key>NSLocationAlwaysUsageDescription</key>
     <string>The reason for accessing the location information of the user</string>

4) NSLocationWhenInUseUsageDescription
   Use this key if your iOS app accesses location information only when running
   in the foreground.
     <!-- Info.plist -->
     <key>NSLocationWhenInUseUsageDescription</key>
     <string>The reason for accessing the location information of the user</string>

5) UIBackgroundModes
   Apps that receive location updates when running in the background must
   include the UIBackgroundModes key (with the location value) in their
   app’s Info.plist file.
     <key>UIBackgroundModes</key>
     <array>
       <string>location</string>
     </array>

*******************************************************************************)
unit Alcinoe.FMX.GeoPosition.Sensor;

interface

{$I Alcinoe.inc}

uses
  system.Classes,
  system.Messaging,
  System.SysUtils,
  {$IF defined(android)}
  AndroidApi.jni,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.Provider,
  Androidapi.JNI.Location,
  Alcinoe.AndroidApi.Google,
  {$ENDIF}
  {$IF defined(IOS)}
  Macapi.ObjectiveC,
  iOSapi.CoreLocation,
  iOSapi.Foundation,
  {$ENDIF}
  FMX.Platform;

type

  {***********************************}
  TALGeoPositionSensor = class(Tobject)

    {$REGION ' ANDROID'}
    {$IF defined(android)}
    private
      type
        TAndroidLocationListener = class(TJavaLocal, Androidapi.JNI.Location.JLocationListener)
        private
          FGeoPositionSensor: TALGeoPositionSensor;
        public
          constructor Create(AGeoPositionSensor: TALGeoPositionSensor);
          procedure onFlushComplete(requestCode: Integer); cdecl;
          procedure onLocationChanged(location: JLocation); overload; cdecl;
          procedure onLocationChanged(locations: JList); overload; cdecl;
          procedure onProviderDisabled(provider: JString); cdecl;
          procedure onProviderEnabled(provider: JString); cdecl;
          procedure onStatusChanged(provider: JString; status: Integer; extras: JBundle); cdecl;
        end;
        //--
        TGMSLocationListener = class(TJavaLocal, Alcinoe.AndroidApi.Google.JLocationListener)
        private
          FGeoPositionSensor: TALGeoPositionSensor;
        public
          constructor Create(AGeoPositionSensor: TALGeoPositionSensor);
          procedure onLocationChanged(location: JLocation); cdecl;
        end;
    private
      const
        RequestPermissionsCode: Integer = 32769;
    private
      FLocationManager: JLocationManager;
      FAndroidLocationListener: TAndroidLocationListener;
      FFusedLocationProviderClient: JFusedLocationProviderClient;
      FGMSLocationListener: TGMSLocationListener;
      FPermissionsRequestResultHandlerEnabled: Boolean;
      procedure PermissionsRequestResultHandler(const Sender: TObject; const M: TMessage);
    {$ENDIF}
    {$ENDREGION}

    {$REGION ' IOS'}
    {$IF defined(IOS)}
    private
      type
        TLocationManagerDelegate = class(TOCLocal, CLLocationManagerDelegate)
        private
          FGeoPositionSensor: TALGeoPositionSensor;
        public
          constructor Create(AGeoPositionSensor: TALGeoPositionSensor);
          procedure locationManager(manager: CLLocationManager; didFailWithError: NSError); overload; cdecl;
          procedure locationManager(manager: CLLocationManager; didUpdateHeading: CLHeading); overload; cdecl;
          procedure locationManager(manager: CLLocationManager; didUpdateToLocation: CLLocation; fromLocation: CLLocation); overload; cdecl;
          procedure locationManager(manager: CLLocationManager; monitoringDidFailForRegion: CLRegion; withError: NSError); overload; cdecl;
          procedure locationManager(manager: CLLocationManager; didChangeAuthorizationStatus: CLAuthorizationStatus); overload; cdecl;
          procedure locationManagerDidChangeAuthorization(manager: CLLocationManager); cdecl;
          [MethodName('locationManager:didUpdateLocations:')]
          procedure locationManagerDidUpdateLocations(manager: CLLocationManager; locations: NSArray); cdecl;
          function locationManagerShouldDisplayHeadingCalibration(manager: CLLocationManager): Boolean; cdecl;
          [MethodName('locationManager:didDetermineState:forRegion:')]
          procedure locationManagerDidDetermineStateForRegion(manager: CLLocationManager; state: CLRegionState; region: CLRegion); cdecl;
          [MethodName('locationManager:didRangeBeacons:satisfyingConstraint:')]
          procedure locationManagerDidRangeBeaconsSatisfyingConstraint(manager: CLLocationManager; didRangeBeacons: NSArray; satisfyingConstraint: CLBeaconIdentityConstraint); cdecl;
          [MethodName('locationManager:didFailRangingBeaconsForConstraint:error:')]
          procedure locationManagerDidFailRangingBeaconsForConstraintError(manager: CLLocationManager; didFailRangingBeaconsForConstraint: CLBeaconIdentityConstraint; error: NSError); cdecl;
          [MethodName('locationManager:didRangeBeacons:inRegion:')]
          procedure locationManagerDidRangeBeaconsInRegion(manager: CLLocationManager; beacons: NSArray; region: CLBeaconRegion); cdecl;
          [MethodName('locationManager:rangingBeaconsDidFailForRegion:withError:')]
          procedure locationManagerRangingBeaconsDidFailForRegionWithError(manager: CLLocationManager; region: CLBeaconRegion; error: NSError); cdecl;
          [MethodName('locationManager:didEnterRegion:')]
          procedure locationManagerDidEnterRegion(manager: CLLocationManager; region: CLRegion); cdecl;
          [MethodName('locationManager:didExitRegion:')]
          procedure locationManagerDidExitRegion(manager: CLLocationManager; region: CLRegion); cdecl;
          [MethodName('locationManager:didStartMonitoringForRegion:')]
          procedure locationManagerDidStartMonitoringForRegion(manager: CLLocationManager; region: CLRegion); cdecl;
          procedure locationManagerDidPauseLocationUpdates(manager: CLLocationManager); cdecl;
          procedure locationManagerDidResumeLocationUpdates(manager: CLLocationManager); cdecl;
          [MethodName('locationManager:didFinishDeferredUpdatesWithError:')]
          procedure locationManagerDidFinishDeferredUpdatesWithError(manager: CLLocationManager; error: NSError); cdecl;
          [MethodName('locationManager:didVisit:')]
          procedure locationManagerDidVisit(manager: CLLocationManager; visit: CLVisit); cdecl;
        end;
    private
      fLocationManager: CLLocationManager;
      fLocationManagerDelegate: TLocationManagerDelegate;
      flocationManagerDidChangeAuthorizationStatusEnabled: Boolean;
    {$ENDIF}
    {$ENDREGION}

    public
      type
        TGeoPositionUpdateEvent = procedure(
                                    const Sender: TObject;
                                    const ALatitude: Double;
                                    const ALongitude: Double;
                                    const AAltitude: Double;
                                    const AAccuracy: Double;
                                    Const ADateTime: TdateTime) of object;
        TShowRequestPermissionRationaleEvent = Procedure(
                                                 const Sender: TObject;
                                                 const AToActivateGPS: Boolean;
                                                 const AToRequestCoarseGeoPositionPermission: Boolean;
                                                 const AToRequestPreciseGeoPositionPermission: Boolean;
                                                 const AToRequestAlwaysAuthorization: Boolean;
                                                 const AIsForced: Boolean; // when true it's mean that the user denied the previous permission request with checking "Never ask again option"
                                                 const ACanRequestPermissionProc: TProc; // the procedure to launch when the user response positivelly to the rationale
                                                 const ACanNotRequestPermissionProc: TProc) of object; // the procedure to launch when the user response negativelly to the rationale
    private
      fOnGeoPositionUpdate: TGeoPositionUpdateEvent;
      fOnAuthorizationStatus: TNotifyEvent;
      fOnShowRequestPermissionRationale: TShowRequestPermissionRationaleEvent;
      FRequestPermissionRationaleShowed: Boolean;
      //--
      FIsActivatingGpsAndGrantingGeoPositionAccess: Boolean;
      //--
      FWaitingGpsActivationResult: boolean;
      FWaitingGrantCoarseGeoPositionAccessResult: Boolean;
      FWaitingGrantPreciseGeoPositionAccessResult: Boolean;
      FWaitingAlwaysAuthorizationResult: Boolean;
      //--
      FGeoPositionUpdatesDelayed: Boolean;
      FGeoPositionUpdatesActive: Boolean;
      //--
      FRequestCoarseGeoPosition: Boolean;
      FRequestPreciseGeoPosition: Boolean;
      FRequestAlwaysAuthorization: Boolean;
      FRequestMinDistance: Integer;
      //--
      FApplicationEventHandlerEnabled: Boolean;
      //--
      procedure ApplicationEventHandler(const Sender: TObject; const M: TMessage);
      procedure DoActivateGpsAndGrantGeoPositionAccess(Const AForceShowRequestPermissionRationale: Boolean = False);
      procedure DoActivateGpsAndGrantGeoPositionAccessResult;
      procedure DoStartGeoPositionUpdates;
      procedure DoShowRequestPermissionRationale(
                  const AToActivateGPS: Boolean;
                  const AToRequestCoarseGeoPositionPermission: Boolean;
                  const AToRequestPreciseGeoPositionPermission: Boolean;
                  const AToRequestAlwaysAuthorization: Boolean;
                  const AIsForced: Boolean; // when true it's mean that the user denied the previous permission request with checking "Never ask again option"
                  const ARequestPermissionProc: TProc);
      function ShouldShowRequestPermissionRationale(
                 const ACoarseGeoPosition: Boolean;
                 const APreciseGeoPosition: Boolean;
                 const AAlwaysAuthorization: Boolean): Boolean;
      procedure RequestPermissions(
                  const ACoarseGeoPosition: Boolean;
                  const APreciseGeoPosition: Boolean;
                  const AAlwaysAuthorization: Boolean);
      function GetIsListeningGeoPositionUpdates: boolean;
    public
      constructor Create(Const AUseGooglePlayServicesIfAvailable: Boolean = True); virtual;
      destructor Destroy; override;
      function  IsGpsEnabled: Boolean; // If your device GPS is on
      procedure  GetPermissionsGranted(
                   out ARestricted: boolean; // This app is not authorized to use location services. The user cannot change this app’s status, possibly due to active restrictions such as parental controls being in place.
                   out ACoarseGeoPosition: Boolean; // If you have granted the app access to your coarse geoposition
                   out APreciseGeoPosition: boolean; // If you have granted the app access to your precise geoposition
                   out AAuthorizedAlways: Boolean); // This app is authorized to start location services at any time
      function  IsGeoPositionAccessGranted: Boolean; // If you have granted the app access to your geoposition
      function  IsGpsEnabledAndGeoPositionAccessGranted: Boolean; overload; // If your device GPS is on and if your have granted the app access to your geoposition
      function  IsGpsEnabledAndGeoPositionAccessGranted(out ARestricted: boolean): Boolean; overload; // If your device GPS is on and if your have granted the app access to your geoposition
      function  IsRestricted: Boolean; // This app is not authorized to use location services. The user cannot change this app’s status, possibly due to active restrictions such as parental controls being in place.
      procedure ActivateGpsAndGrantGeoPositionAccess(
                  const ACoarseGeoPosition: boolean = True;  // when ACoarseGeoPosition = true and APreciseGeoPosition = true
                  const APreciseGeoPosition: boolean = True; // then user can choose either ACoarseGeoPosition or APreciseGeoPosition
                  const AAlwaysAuthorization: boolean = False); virtual;
      procedure StartGeoPositionUpdates(
                  const aMinDistance: Integer; // minimum distance between geoposition updates in meters
                  const ACoarseGeoPosition: boolean = true;  // when ACoarseGeoPosition = true and APreciseGeoPosition = true
                  const APreciseGeoPosition: boolean = true; // then user can choose either ACoarseGeoPosition or APreciseGeoPosition
                  const AAlwaysAuthorization: boolean = False); virtual;
      procedure StopGeoPositionUpdates; virtual;
      property OnGeoPositionUpdate: TGeoPositionUpdateEvent read fOnGeoPositionUpdate write fOnGeoPositionUpdate;
      property OnAuthorizationStatus: TNotifyEvent read fOnAuthorizationStatus write fOnAuthorizationStatus;
      property OnShowRequestPermissionRationale: TShowRequestPermissionRationaleEvent read fOnShowRequestPermissionRationale write fOnShowRequestPermissionRationale;
      property IsListeningGeoPositionUpdates: Boolean read GetIsListeningGeoPositionUpdates; // Set to true in StartGeoPositionUpdates and set to false only in StopGeoPositionUpdates
      property IsActivatingGpsAndGrantingGeoPositionAccess: Boolean read FIsActivatingGpsAndGrantingGeoPositionAccess; // set to true in ActivateGpsAndGrantGeoPositionAccess and set to false in OnAuthorizationStatus
    end;

implementation

uses
  {$IF defined(DEBUG)}
  System.Rtti,
  {$ENDIF}
  System.math,
  System.Math.Vectors,
  {$IF defined(android)}
  Androidapi.Helpers,
  AndroidApi.Jni.App,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.PlayServices,
  Androidapi.JNI.Net,
  FMX.Platform.Android,
  Alcinoe.AndroidApi.AndroidX,
  {$ENDIF}
  {$IF defined(IOS)}
  Macapi.Helpers,
  iOSapi.UIKit,
  FMX.Helpers.iOS,
  {$ENDIF}
  Alcinoe.Cipher,
  Alcinoe.stringutils,
  ALcinoe.common;

{***********************************************************************************************}
constructor TALGeoPositionSensor.Create(Const AUseGooglePlayServicesIfAvailable: Boolean = True);
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  if (AUseGooglePlayServicesIfAvailable) and
     (TJGooglePlayServicesUtil
        .JavaClass
        .isGooglePlayServicesAvailable(TAndroidHelper.Activity) = TJConnectionResult.JavaClass.SUCCESS) then begin
    FLocationManager := nil;
    FAndroidLocationListener := nil;
    FFusedLocationProviderClient := TJLocationServices.JavaClass.getFusedLocationProviderClient(TAndroidHelper.Context);
    FGMSLocationListener := TGMSLocationListener.Create(Self);
  end
  else begin
    FLocationManager := TJLocationManager.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.LOCATION_SERVICE));
    FAndroidLocationListener := TAndroidLocationListener.Create(Self);
    FFusedLocationProviderClient := nil;
    FGMSLocationListener := nil;
  end;
  FPermissionsRequestResultHandlerEnabled := False;
  TMessageManager.DefaultManager.SubscribeToMessage(TPermissionsRequestResultMessage, PermissionsRequestResultHandler);
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  flocationManagerDidChangeAuthorizationStatusEnabled := False;
  fLocationManager := TCLLocationManager.Create;
  fLocationManagerDelegate := TLocationManagerDelegate.Create(self);
  fLocationManager.setDelegate(fLocationManagerDelegate.GetObjectID);
  {$ENDIF}
  {$ENDREGION}

  fOnGeoPositionUpdate := nil;
  fOnAuthorizationStatus := nil;
  fOnShowRequestPermissionRationale := nil;
  FRequestPermissionRationaleShowed := False;
  //--
  FIsActivatingGpsAndGrantingGeoPositionAccess := False;
  //--
  FWaitingGpsActivationResult := False;
  FWaitingGrantCoarseGeoPositionAccessResult := False;
  FWaitingGrantPreciseGeoPositionAccessResult := False;
  FWaitingAlwaysAuthorizationResult := False;
  //--
  FGeoPositionUpdatesDelayed := False;
  FGeoPositionUpdatesActive := False;
  //--
  FRequestCoarseGeoPosition := False;
  FRequestPreciseGeoPosition := False;
  FRequestAlwaysAuthorization := False;
  FRequestMinDistance := 0;
  //--
  FApplicationEventHandlerEnabled := False;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventHandler);

end;

{**************************************}
destructor TALGeoPositionSensor.Destroy;
begin

  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventHandler);

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  TMessageManager.DefaultManager.Unsubscribe(TPermissionsRequestResultMessage, PermissionsRequestResultHandler);
  //--
  if (FLocationManager <> nil) and (FAndroidLocationListener <> nil) then FLocationManager.removeUpdates(FAndroidLocationListener);
  ALFreeAndNil(FAndroidLocationListener);
  FLocationManager := nil;
  //--
  if (FFusedLocationProviderClient <> nil) and (FGMSLocationListener <> nil) then FFusedLocationProviderClient.removeLocationUpdates(FGMSLocationListener);
  ALFreeAndNil(FGMSLocationListener);
  FFusedLocationProviderClient := nil;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  if fLocationManager <> nil then begin
    fLocationManager.setDelegate(nil);
    fLocationManager.release;
  end;
  alFreeAndNil(fLocationManagerDelegate);
  {$ENDIF}
  {$ENDREGION}

  inherited Destroy;

end;

{**************************************************}
function TALGeoPositionSensor.IsGpsEnabled: Boolean;
begin

  {$REGION ' ANDROID'}
  {$IF defined(ANDROID)}
  if TJBuild_VERSION.JavaClass.SDK_INT >= 28 {android P} then begin
    var LLocationManager := TJLocationManager.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.LOCATION_SERVICE));
    result := LLocationManager.isLocationEnabled();
  end
  else begin
    result := TJSettings_Secure.javaclass.getInt(
                TAndroidHelper.Context.getContentResolver(),
                TJSettings_Secure.javaclass.LOCATION_MODE,
                TJSettings_Secure.javaclass.LOCATION_MODE_HIGH_ACCURACY) <> TJSettings_Secure.javaclass.LOCATION_MODE_OFF;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  // Returns a Boolean value indicating whether location services are enabled on the device.
  // The user can enable or disable location services from the Settings app by toggling
  // the Location Services switch in General.
  // You should check the return value of this method before starting location updates to
  // determine whether the user has location services enabled for the current device.
  // Location services prompts users the first time they attempt to use location-related
  // information in an app but does not prompt for subsequent attempts. If the user denies
  // the use of location services and you attempt to start location updates anyway,
  // the location manager reports an error to its delegate.
  result := TCLLocationManager.OCClass.locationServicesEnabled;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' MSWINDOWS / ALMacOS'}
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  result := false;
  {$ENDIF}
  {$ENDREGION}

  {$IFDEF DEBUG}
  ALLog('TALGeoPositionSensor.IsGpsEnabled', 'Result: ' + ALBoolToStrW(result), TalLogType.verbose);
  {$ENDIF}

end;

{****************************************************}
procedure  TALGeoPositionSensor.GetPermissionsGranted(
             out ARestricted: boolean; // This app is not authorized to use location services. The user cannot change this app’s status, possibly due to active restrictions such as parental controls being in place.
             out ACoarseGeoPosition: Boolean; // If you have granted the app access to your coarse geoposition
             out APreciseGeoPosition: boolean; // If you have granted the app access to your precise geoposition
             out AAuthorizedAlways: Boolean); // This app is authorized to start location services at any time
begin

  {$REGION ' ANDROID'}
  {$IF defined(ANDROID)}

  //http://stackoverflow.com/questions/33407250/checkselfpermission-method-is-not-working-in-targetsdkversion-22
  //If your application is targeting an API level before 23 (Android M) then both: ContextCompat#checkSelfPermission
  //and Context#checkSelfPermission doesn't work and always returns 0 (PERMISSION_GRANTED).

  ARestricted := False;
  //----
  if (TJBuild_VERSION.JavaClass.SDK_INT >= 23 {marshmallow}) then begin
    ACoarseGeoPosition := MainActivity.checkSelfPermission(StringToJString('android.permission.ACCESS_COARSE_LOCATION')) = TJPackageManager.JavaClass.PERMISSION_GRANTED;
    APreciseGeoPosition := MainActivity.checkSelfPermission(StringToJString('android.permission.ACCESS_FINE_LOCATION')) = TJPackageManager.JavaClass.PERMISSION_GRANTED;
  end
  else begin
    ACoarseGeoPosition := True;
    APreciseGeoPosition := True;
  end;
  //----
  if (TJBuild_VERSION.JavaClass.SDK_INT >= 29 {Android 10}) then
    AAuthorizedAlways := MainActivity.checkSelfPermission(StringToJString('android.permission.ACCESS_BACKGROUND_LOCATION')) = TJPackageManager.JavaClass.PERMISSION_GRANTED
  else
    AAuthorizedAlways := true;

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

  //init output vars
  ARestricted := False;
  ACoarseGeoPosition := False;
  APreciseGeoPosition := false;
  AAuthorizedAlways := False;

  // The authorization status of a given app is managed by the system and determined by several
  // factors. Apps must be explicitly authorized to use location services by the user and
  // location services must themselves currently be enabled for the system. A request for user
  // authorization is displayed automatically when your app first attempts to use location services.
  Var LAuthorizationStatus: Integer;
  if TOSVersion.Check(14, 0) then LAuthorizationStatus := fLocationManager.authorizationStatus
  else LAuthorizationStatus := TCLLocationManager.OCClass.authorizationStatus;
  case LAuthorizationStatus of

    // The user has not yet made a choice regarding whether this app can use location services.
    kCLAuthorizationStatusNotDetermined:;

    // The user explicitly denied the use of location services for this app or location services are currently disabled in Settings.
    kCLAuthorizationStatusDenied:;

    // This app is not authorized to use location services. The user cannot change this app’s status, possibly due to active restrictions such as parental controls being in place.
    kCLAuthorizationStatusRestricted: ARestricted := True;

    // This app is authorized to start most location services while running in the foreground. This authorization does not allow you to use APIs that could launch your app in response to an event, such as region monitoring and the significant location change services.
    kCLAuthorizationStatusAuthorizedWhenInUse: begin
      ACoarseGeoPosition := True;
      if TOSVersion.Check(14, 0) then
        APreciseGeoPosition := fLocationManager.accuracyAuthorization = CLAccuracyAuthorizationFullAccuracy
      else
        APreciseGeoPosition := true;
    end;

    // This app is authorized to start location services at any time. This authorization allows you to use all location services, including those for monitoring regions and significant location changes.
    kCLAuthorizationStatusauthorizedAlways: begin
      ACoarseGeoPosition := True;
      if TOSVersion.Check(14, 0) then
        APreciseGeoPosition := fLocationManager.accuracyAuthorization = CLAccuracyAuthorizationFullAccuracy
      else
        APreciseGeoPosition := true;
      AAuthorizedAlways := True;
    end;

    // avoids compiler warnings
    else
      raise Exception.Create('Error 98FF6C0D-9690-4E50-A256-21A0783A0E0A');

  end;

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' MSWINDOWS / ALMacOS'}
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}

  //if someone know how to grab location from windows could be good
  //to update this function
  ARestricted := True;
  ACoarseGeoPosition := false;
  APreciseGeoPosition := false;
  AAuthorizedAlways := False;

  {$ENDIF}
  {$ENDREGION}

  if APreciseGeoPosition then ACoarseGeoPosition := true;

  {$IFDEF DEBUG}
  ALLog(
    'TALGeoPositionSensor.GetPermissionsGranted',
    'Restricted: ' + ALBoolToStrW(ARestricted) + ' | '+
    'CoarseGeoPosition: ' + ALBoolToStrW(ACoarseGeoPosition) + ' | '+
    'PreciseGeoPosition: ' + ALBoolToStrW(APreciseGeoPosition) + ' | '+
    'AuthorizedAlways: ' + ALBoolToStrW(AAuthorizedAlways),
    TalLogType.verbose);
  {$ENDIF}

end;

{****************************************************************}
function TALGeoPositionSensor.IsGeoPositionAccessGranted: Boolean;
begin
  var LRestricted: boolean;
  var LCoarseGeoPosition: Boolean;
  var LPreciseGeoPosition: boolean;
  var LAuthorizedAlways: Boolean;
  GetPermissionsGranted(
    LRestricted,
    LCoarseGeoPosition,
    LPreciseGeoPosition,
    LAuthorizedAlways);
  result := LCoarseGeoPosition or LPreciseGeoPosition;
end;

{*****************************************************************************}
function TALGeoPositionSensor.IsGpsEnabledAndGeoPositionAccessGranted: Boolean;
begin
  result := IsGpsEnabled and IsGeoPositionAccessGranted;
end;

{********************************************************************************************************}
function  TALGeoPositionSensor.IsGpsEnabledAndGeoPositionAccessGranted(out ARestricted: boolean): Boolean;
begin
  var LCoarseGeoPosition: Boolean;
  var LPreciseGeoPosition: boolean;
  var LAuthorizedAlways: Boolean;
  GetPermissionsGranted(
    ARestricted,
    LCoarseGeoPosition,
    LPreciseGeoPosition,
    LAuthorizedAlways);
  result := IsGpsEnabled and (LCoarseGeoPosition or LPreciseGeoPosition);
end;

{***************************************************}
function  TALGeoPositionSensor.IsRestricted: Boolean;
begin
  var LCoarseGeoPosition: Boolean;
  var LPreciseGeoPosition: boolean;
  var LAuthorizedAlways: Boolean;
  GetPermissionsGranted(
    Result, // out ARestricted: boolean
    LCoarseGeoPosition,
    LPreciseGeoPosition,
    LAuthorizedAlways);
end;

{*****************************************************************}
function TALGeoPositionSensor.ShouldShowRequestPermissionRationale(
           const ACoarseGeoPosition: Boolean;
           const APreciseGeoPosition: Boolean;
           const AAlwaysAuthorization: Boolean): Boolean;
begin

  {$REGION ' ANDROID'}
  {$IF defined(ANDROID)}
  Result := False;
  if (TJBuild_VERSION.JavaClass.SDK_INT >= 23 {marshmallow}) then begin
    if ACoarseGeoPosition then Result := Result or (MainActivity.shouldShowRequestPermissionRationale(StringToJString('android.permission.ACCESS_COARSE_LOCATION')));
    if APreciseGeoPosition then Result := Result or (MainActivity.shouldShowRequestPermissionRationale(StringToJString('android.permission.ACCESS_FINE_LOCATION')));
  end;
  if (TJBuild_VERSION.JavaClass.SDK_INT >= 29 {Android 10}) then begin
    if aAlwaysAuthorization then Result := Result or (MainActivity.shouldShowRequestPermissionRationale(StringToJString('android.permission.ACCESS_BACKGROUND_LOCATION')));
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  Var LAuthorizationStatus: Integer;
  if TOSVersion.Check(14, 0) then LAuthorizationStatus := fLocationManager.authorizationStatus
  else LAuthorizationStatus := TCLLocationManager.OCClass.authorizationStatus;
  case LAuthorizationStatus of
    kCLAuthorizationStatusNotDetermined: result := false;
    //--
    kCLAuthorizationStatusDenied: result := ACoarseGeoPosition or APreciseGeoPosition or AAlwaysAuthorization;
    //--
    kCLAuthorizationStatusRestricted: result := ACoarseGeoPosition or APreciseGeoPosition or AAlwaysAuthorization;
    //--
    kCLAuthorizationStatusAuthorizedWhenInUse: begin
      var LPreciseGeoPositionGranted: Boolean;
      if TOSVersion.Check(14, 0) then
        LPreciseGeoPositionGranted := fLocationManager.accuracyAuthorization = CLAccuracyAuthorizationFullAccuracy
      else
        LPreciseGeoPositionGranted := true;
      result := AAlwaysAuthorization or (APreciseGeoPosition and (not LPreciseGeoPositionGranted));
    end;
    //--
    kCLAuthorizationStatusauthorizedAlways: begin
      var LPreciseGeoPositionGranted: Boolean;
      if TOSVersion.Check(14, 0) then
        LPreciseGeoPositionGranted := fLocationManager.accuracyAuthorization = CLAccuracyAuthorizationFullAccuracy
      else
        LPreciseGeoPositionGranted := true;
      result := (APreciseGeoPosition and (not LPreciseGeoPositionGranted));
    end;
    //--
    Else
      raise Exception.Create('Error 2E8AF369-E662-432A-B958-BC0E91BE86EC');
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' MSWINDOWS / ALMacOS'}
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  result := False;
  {$ENDIF}
  {$ENDREGION}

  {$IFDEF DEBUG}
  ALLog(
    'TALGeoPositionSensor.ShouldShowRequestPermissionRationale',
    'Result: ' + ALBoolToStrW(Result) + ' | '+
    'CoarseGeoPosition: ' + ALBoolToStrW(ACoarseGeoPosition) + ' | '+
    'PreciseGeoPosition: ' + ALBoolToStrW(APreciseGeoPosition) + ' | '+
    'AlwaysAuthorization: ' + ALBoolToStrW(AAlwaysAuthorization),
    TalLogType.verbose);
  {$ENDIF}

end;

{**************************************************************}
procedure TALGeoPositionSensor.DoShowRequestPermissionRationale(
            const AToActivateGPS: Boolean;
            const AToRequestCoarseGeoPositionPermission: Boolean;
            const AToRequestPreciseGeoPositionPermission: Boolean;
            const AToRequestAlwaysAuthorization: Boolean;
            const AIsForced: Boolean; // when true it's mean that the user denied the previous permission request with checking "Never ask again option"
            const ARequestPermissionProc: TProc);
begin
  if assigned(OnShowRequestPermissionRationale) then begin
    FRequestPermissionRationaleShowed := True;
    OnShowRequestPermissionRationale(
      Self, // const Sender: TObject;
      AToActivateGPS, // const AToActivateGPS: Boolean;
      AToRequestCoarseGeoPositionPermission, // const AToRequestCoarseGeoPositionPermission: Boolean;
      AToRequestPreciseGeoPositionPermission, // const AToRequestPreciseGeoPositionPermission: Boolean;
      AToRequestAlwaysAuthorization, // const AToRequestAlwaysAuthorization: Boolean;
      AIsForced, // const AIsForced: Boolean;
      // the procedure to launch when the user response positivelly to the rationale
      procedure
      begin
        FWaitingGpsActivationResult := AToActivateGPS;
        FWaitingGrantCoarseGeoPositionAccessResult := AToRequestCoarseGeoPositionPermission;
        FWaitingGrantPreciseGeoPositionAccessResult := AToRequestPreciseGeoPositionPermission;
        FWaitingAlwaysAuthorizationResult := AToRequestAlwaysAuthorization;
        ARequestPermissionProc;
      end,
      // the procedure to launch when the user response negativelly to the rationale
      procedure
      begin
        DoActivateGpsAndGrantGeoPositionAccessResult;
      end);
  end
  else
    DoActivateGpsAndGrantGeoPositionAccessResult;
end;

{************************************************}
procedure TALGeoPositionSensor.requestPermissions(
            const ACoarseGeoPosition: Boolean;
            const APreciseGeoPosition: Boolean;
            const AAlwaysAuthorization: Boolean);
begin

  FApplicationEventHandlerEnabled := False;
  //--
  FWaitingGpsActivationResult := False;
  FWaitingGrantCoarseGeoPositionAccessResult := False;
  FWaitingGrantPreciseGeoPositionAccessResult := False;
  FWaitingAlwaysAuthorizationResult := False;

  {$REGION ' ANDROID'}
  {$IF defined(ANDROID)}
  FPermissionsRequestResultHandlerEnabled := True;
  var LPermissionsCount: integer := 0;
  if ACoarseGeoPosition then inc(LPermissionsCount);
  if APreciseGeoPosition then inc(LPermissionsCount);
  if AAlwaysAuthorization then inc(LPermissionsCount);
  if LPermissionsCount = 0 then
    raise Exception.Create('Error 15E524DB-8DA1-440D-8571-3AB304DC8ABC');
  var LPermissions := TJavaObjectArray<JString>.create(LPermissionsCount);
  try
    var LIdx: integer := 0;
    if ACoarseGeoPosition then begin
      FWaitingGrantCoarseGeoPositionAccessResult := True;
      LPermissions.Items[LIdx] := StringToJString('android.permission.ACCESS_COARSE_LOCATION');
      inc(LIdx);
    end;
    if APreciseGeoPosition then begin
      FWaitingGrantPreciseGeoPositionAccessResult := True;
      LPermissions.Items[LIdx] := StringToJString('android.permission.ACCESS_FINE_LOCATION');
      inc(LIdx);
    end;
    if AAlwaysAuthorization then begin
      FWaitingAlwaysAuthorizationResult := True;
      LPermissions.Items[LIdx] := StringToJString('android.permission.ACCESS_BACKGROUND_LOCATION');
      //inc(LIdx);
    end;
    MainActivity.requestPermissions(LPermissions, RequestPermissionsCode);
  finally
    ALFreeAndNil(LPermissions);
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  flocationManagerDidChangeAuthorizationStatusEnabled := ACoarseGeoPosition or APreciseGeoPosition or AAlwaysAuthorization;
  //--
  if ACoarseGeoPosition then FWaitingGrantCoarseGeoPositionAccessResult := True;
  if APreciseGeoPosition then FWaitingGrantPreciseGeoPositionAccessResult := True;
  if AAlwaysAuthorization then FWaitingAlwaysAuthorizationResult := True;
  //--
  if ACoarseGeoPosition then fLocationManager.requestWhenInUseAuthorization
  else if APreciseGeoPosition then fLocationManager.requestWhenInUseAuthorization
  else if AAlwaysAuthorization then fLocationManager.requestAlwaysAuthorization;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' MSWINDOWS / ALMacOS'}
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  //nothing to do here
  {$ENDIF}
  {$ENDREGION}

end;

{**********************************************************************}
function TALGeoPositionSensor.GetIsListeningGeoPositionUpdates: boolean;
begin
  result := FGeoPositionUpdatesDelayed or FGeoPositionUpdatesActive;
end;

{*********************************************************************************************************************************}
procedure TALGeoPositionSensor.DoActivateGpsAndGrantGeoPositionAccess(Const AForceShowRequestPermissionRationale: Boolean = False);
begin

  //security check FRequestCoarseGeoPosition and/or FRequestPreciseGeoPosition are mandatories
  if (not FRequestCoarseGeoPosition) and (not FRequestPreciseGeoPosition) then
    raise Exception.Create('You must say if you need coarse or precise geoposition permissions');

  //if everything is already enabled then nothing to do so exit
  var LGpsEnabled := IsGpsEnabled;
  var LRestricted: boolean;
  var LCoarseGeoPositionGranted: Boolean;
  var LPreciseGeoPositionGranted: boolean;
  var LAuthorizedAlways: Boolean;
  GetPermissionsGranted(
    LRestricted,
    LCoarseGeoPositionGranted,
    LPreciseGeoPositionGranted,
    LAuthorizedAlways);
  if (LGpsEnabled) and // Gps is enabled
     (LCoarseGeoPositionGranted or LPreciseGeoPositionGranted) and // Coarse or Precise GeoPosition Granted
     (FRequestCoarseGeoPosition or ((FRequestPreciseGeoPosition) and LPreciseGeoPositionGranted)) and // we request CoarseGeoPosition or (PreciseGeoPosition and we have PreciseGeoPositionGranted)
     ((not FRequestAlwaysAuthorization) or LAuthorizedAlways) then begin // we do not request AlwaysAuthorization or we have AuthorizedAlwaysGranted
    DoActivateGpsAndGrantGeoPositionAccessResult;
    exit;
  end;

  //if LRestricted we can not do anything so exit
  if LRestricted then begin
    DoActivateGpsAndGrantGeoPositionAccessResult;
    exit;
  end;

  //we must request geoposition permissions and
  //AlwaysAuthorization separatly
  var LRequestCoarseGeoPosition: Boolean;
  var LRequestPreciseGeoPosition: Boolean;
  var LRequestAlwaysAuthorization: Boolean;
  if (not LGpsEnabled) then begin
    LRequestCoarseGeoPosition := False;
    LRequestPreciseGeoPosition := False;
    LRequestAlwaysAuthorization := False;
  end
  else if ((not LCoarseGeoPositionGranted) and (not LPreciseGeoPositionGranted)) or
          ((not FRequestCoarseGeoPosition) and ((not FRequestPreciseGeoPosition) or (not LPreciseGeoPositionGranted))) then begin
    LRequestCoarseGeoPosition := FRequestCoarseGeoPosition;
    LRequestPreciseGeoPosition := FRequestPreciseGeoPosition;
    LRequestAlwaysAuthorization := False;
  end
  else if (FRequestAlwaysAuthorization and (not LAuthorizedAlways)) then begin
    LRequestCoarseGeoPosition := False;
    LRequestPreciseGeoPosition := False;
    LRequestAlwaysAuthorization := True;
  end
  else
    raise Exception.Create('Error 38D5CB1F-F403-4BA4-A904-5D02A4FE31E6');

  //init members
  {$IF defined(ANDROID)}
  FPermissionsRequestResultHandlerEnabled := False;
  {$ENDIF}
  {$IF defined(IOS)}
  flocationManagerDidChangeAuthorizationStatusEnabled := False;
  {$ENDIF}
  FApplicationEventHandlerEnabled := False;
  FRequestPermissionRationaleShowed := False;
  FWaitingGpsActivationResult := False;
  FWaitingGrantCoarseGeoPositionAccessResult := False;
  FWaitingGrantPreciseGeoPositionAccessResult := False;
  FWaitingAlwaysAuthorizationResult := False;

  //If not IsGpsEnabled
  if (not IsGpsEnabled) then begin
    DoShowRequestPermissionRationale(
      True, // const AToActivateGPS: Boolean;
      False, // const AToRequestCoarseGeoPositionPermission: Boolean;
      False, // const AToRequestPreciseGeoPositionPermission: Boolean;
      False, // const AToRequestAlwaysAuthorization: Boolean;
      True, // const AIsForced: Boolean;
      procedure
      begin

        {$REGION ' ANDROID'}
        {$IF defined(ANDROID)}
        FApplicationEventHandlerEnabled:=true;
        var LIntent := TJIntent.JavaClass.init(TJSettings.JavaClass.ACTION_LOCATION_SOURCE_SETTINGS);
        TAndroidHelper.Context.startActivity(LIntent);
        {$ENDIF}
        {$ENDREGION}

        {$REGION ' IOS'}
        {$IF defined(IOS)}
        FApplicationEventHandlerEnabled:=true;
        SharedApplication.openUrl(TNSUrl.Wrap(TNSUrl.OCClass.URLWithString((UIApplicationOpenSettingsURLString))));
        {$ENDIF}
        {$ENDREGION}

      end); // const ARequestPermissionProc: TProc
   end

  //if IsGpsEnabled
  else begin

    //Android location permissions was made by true monkey!! their is no way to know
    //if a user dissmiss the permission dialog with "Never ask again". I try everything
    //and it's end up no way to know :( their is many situations, for exemple if
    //user click on the shadow of the permission popup, then the popup close with denied
    //in PermissionsRequestResultHandler but it's not mean that the user denied it
    //but from shouldShowRequestPermissionRationale it's will look like he denied the
    //access with "never ask again" because if we call again shouldShowRequestPermissionRationale
    //it's will return false like if the user denied the access with "never ask again".

    //shouldShowRequestPermissionRationale return on android:
    // * true –  if the permission is already requested before but was denied WITHOUT checking "Never ask again"
    // * false – If the permission is requested first time or if the last time the user launch the app he select "only this time" option in the permission dialog
    //           If the permission is disabled by some device policy or the permission is already requested but the user denied it WITH checking "Never ask again" option in the permission dialog
    var LShouldShowRequestPermissionRationale := AForceShowRequestPermissionRationale or
                                                 ShouldShowRequestPermissionRationale(
                                                   LRequestCoarseGeoPosition, // const ACoarseGeoPosition: Boolean;
                                                   LRequestPreciseGeoPosition, // const APreciseGeoPosition: Boolean;
                                                   LRequestAlwaysAuthorization); // const AAlwaysAuthorization: Boolean

    //we do not need to show a Rationale
    if (not LShouldShowRequestPermissionRationale) then begin
      requestPermissions(
        LRequestCoarseGeoPosition,
        LRequestPreciseGeoPosition,
        LRequestAlwaysAuthorization);
    end

    //we must show a Rationale
    else begin
      DoShowRequestPermissionRationale(
        False, // const AToActivateGPS: Boolean;
        LRequestCoarseGeoPosition, // const AToRequestCoarseGeoPositionPermission: Boolean;
        LRequestPreciseGeoPosition, // const AToRequestPreciseGeoPositionPermission: Boolean;
        LRequestAlwaysAuthorization, // const AToRequestAlwaysAuthorization: Boolean;
        {$IF defined(IOS)}LShouldShowRequestPermissionRationale{$ELSE}AForceShowRequestPermissionRationale{$ENDIF}, // const AIsForced: Boolean;
        procedure
        begin
          if {$IF defined(IOS)}LShouldShowRequestPermissionRationale{$ELSE}AForceShowRequestPermissionRationale{$ENDIF} then begin

            {$REGION ' ANDROID'}
            {$IF defined(ANDROID)}
            FApplicationEventHandlerEnabled:=true;
            var LIntent := TJIntent.JavaClass.init(TJSettings.JavaClass.ACTION_APPLICATION_DETAILS_SETTINGS);
            var LUri := TJnet_Uri.JavaClass.fromParts(StringToJString('package'), TAndroidHelper.Context.getPackageName(), nil);
            LIntent.setData(LUri);
            TAndroidHelper.Context.startActivity(LIntent);
            {$ENDIF}
            {$ENDREGION}

            {$REGION ' IOS'}
            {$IF defined(IOS)}
            FApplicationEventHandlerEnabled:=true;
            SharedApplication.openUrl(TNSUrl.Wrap(TNSUrl.OCClass.URLWithString((UIApplicationOpenSettingsURLString))));
            {$ENDIF}
            {$ENDREGION}

          end
          else begin

            requestPermissions(
              LRequestCoarseGeoPosition, // const ACoarseGeoPosition: Boolean;
              LRequestPreciseGeoPosition,
              LRequestAlwaysAuthorization);

          end;
        end) // const ARequestPermissionProc: TProc
    end;

  end;

end;

{**************************************************************************}
procedure TALGeoPositionSensor.DoActivateGpsAndGrantGeoPositionAccessResult;
begin
  {$IF defined(ANDROID)}
  FPermissionsRequestResultHandlerEnabled := False;
  {$ENDIF}
  {$IF defined(IOS)}
  flocationManagerDidChangeAuthorizationStatusEnabled := False;
  {$ENDIF}
  FApplicationEventHandlerEnabled := False;
  //--
  FIsActivatingGpsAndGrantingGeoPositionAccess := False;
  //--
  FWaitingGpsActivationResult := False;
  FWaitingGrantCoarseGeoPositionAccessResult := False;
  FWaitingGrantPreciseGeoPositionAccessResult := False;
  FWaitingAlwaysAuthorizationResult := False;
  //--
  if assigned(fOnAuthorizationStatus) then
    fOnAuthorizationStatus(self);
  //--
  if FGeoPositionUpdatesDelayed and IsGpsEnabledAndGeoPositionAccessGranted then
    DoStartGeoPositionUpdates;
end;

{******************************************************************}
procedure TALGeoPositionSensor.ActivateGpsAndGrantGeoPositionAccess(
            const ACoarseGeoPosition: boolean = True;
            const APreciseGeoPosition: boolean = True;
            const AAlwaysAuthorization: boolean = False);
begin
  if FIsActivatingGpsAndGrantingGeoPositionAccess then
    raise Exception.Create('ActivateGpsAndGrantGeoPositionAccess is already running');
  FIsActivatingGpsAndGrantingGeoPositionAccess := True;
  FRequestCoarseGeoPosition := ACoarseGeoPosition;
  FRequestPreciseGeoPosition := APreciseGeoPosition;
  FRequestAlwaysAuthorization := AAlwaysAuthorization;
  DoActivateGpsAndGrantGeoPositionAccess;
end;

{*******************************************************}
procedure TALGeoPositionSensor.DoStartGeoPositionUpdates;
begin

  FGeoPositionUpdatesDelayed := False;
  FGeoPositionUpdatesActive := True;

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  if FLocationManager <> nil then begin
    if (TJBuild_VERSION.JavaClass.SDK_INT >= 31 {Android 12}) and
       (FLocationManager.isProviderEnabled(TJLocationManager.javaclass.FUSED_PROVIDER)) then begin
      FLocationManager.requestLocationUpdates(
        TJLocationManager.javaclass.FUSED_PROVIDER, // provider: JString; - the listener to receive location updates This value cannot be null.
        0, // minTime: Int64; - minimum time interval between location updates in milliseconds
        FRequestMinDistance, // minDistance: Single; - minimum distance between location updates in meters
        FAndroidLocationListener); // listener: JLocationListener - the listener to receive location updates This value cannot be null.
    end
    else begin
      if FLocationManager.isProviderEnabled(TJLocationManager.javaclass.GPS_PROVIDER) then
        FLocationManager.requestLocationUpdates(
          TJLocationManager.javaclass.GPS_PROVIDER, // provider: JString; - the listener to receive location updates This value cannot be null.
          0, // minTime: Int64; - minimum time interval between location updates in milliseconds
          FRequestMinDistance, // minDistance: Single; - minimum distance between location updates in meters
          FAndroidLocationListener); // listener: JLocationListener - the listener to receive location updates This value cannot be null.
      if FLocationManager.isProviderEnabled(TJLocationManager.javaclass.NETWORK_PROVIDER) then
        FLocationManager.requestLocationUpdates(
          TJLocationManager.javaclass.NETWORK_PROVIDER, // provider: JString; - the listener to receive location updates This value cannot be null.
          0, // minTime: Int64; - minimum time interval between location updates in milliseconds
          FrequestMinDistance, // minDistance: Single; - minimum distance between location updates in meters
          FAndroidLocationListener); // listener: JLocationListener - the listener to receive location updates This value cannot be null.
    end
  end
  else if FFusedLocationProviderClient <> nil then begin
    Var LPriority: integer;
    if FRequestPreciseGeoPosition then LPriority := TJPriority.JavaClass.PRIORITY_HIGH_ACCURACY
    else if FRequestCoarseGeoPosition then LPriority := TJPriority.JavaClass.PRIORITY_BALANCED_POWER_ACCURACY
    else raise Exception.Create('Error D2AA5FE1-50E9-4AE3-9474-0DD6C8E4F5B0');
    //--
    var LLocationRequest := TJLocationRequest_Builder.JavaClass.init(0{intervalMillis})
                              //.setDurationMillis(durationMillis: Int64) - Sets the duration of this request. - The default value is Long.MAX_VALUE.
                              //.setGranularity(granularity: Integer) - Sets the Granularity of locations returned for this request. - The default value is Granularity.GRANULARITY_PERMISSION_LEVEL.
                              //.setIntervalMillis(intervalMillis: Int64) - Sets the desired interval of location updates.
                              //.setMaxUpdateAgeMillis(maxUpdateAgeMillis: Int64) - Sets the maximum age of an initial historical location delivered for this request. - The default value is IMPLICIT_MAX_UPDATE_AGE.
                              //.setMaxUpdateDelayMillis(maxUpdateDelayMillis: Int64) - Sets the longest a location update may be delayed. - The default value is 0.
                              //.setMaxUpdates(maxUpdates: Integer) - Sets the maximum number of updates delivered to this request. - The default value is Integer.MAX_VALUE.
                              .setMinUpdateDistanceMeters(FRequestMinDistance) // - Sets the maximum number of updates delivered to this request. - The default value is 0.
                              //.setMinUpdateIntervalMillis(minUpdateIntervalMillis: Int64) - Sets the fastest allowed interval of location updates.  - The default value is IMPLICIT_MIN_UPDATE_INTERVAL
                              .setPriority(LPriority) // - Sets the Priority of the location request. - The default value is Priority.PRIORITY_BALANCED_POWER_ACCURACY.
                              //.setWaitForAccurateLocation(waitForAccurateLocation: Boolean) - If set to true and this request is Priority.PRIORITY_HIGH_ACCURACY, this will delay delivery of initial low accuracy locations for a small amount of time in case a high accuracy location can be delivered instead. - The default value is true.
                              .build();
    //--
    FFusedLocationProviderClient.requestLocationUpdates(
      LLocationRequest, // request: JLocationRequest;
      FGMSLocationListener, // listener: JLocationListener;
      TJLooper.JavaClass.getMainLooper); // looper: JLooper
  end
  else
    raise Exception.Create('Error 78DC6A41-1341-497A-AFED-6CC3EAD1C03F');
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  //The minimum distance in meters the device must move
  //horizontally before an update event is generated.
  FlocationManager.setDistanceFilter(FRequestMinDistance);
  //kCLLocationAccuracyBestForNavigation
  //kCLLocationAccuracyBest
  //kCLLocationAccuracyNearestTenMeters
  //kCLLocationAccuracyHundredMeters
  //kCLLocationAccuracyKilometer
  //kCLLocationAccuracyThreeKilometers
  //kCLLocationAccuracyReduced
  Var LdesiredAccuracy: CLLocationAccuracy;
  if FRequestPreciseGeoPosition then LdesiredAccuracy := kCLLocationAccuracyBest
  else if FRequestCoarseGeoPosition then begin
    if TOSVersion.Check(14, 0) then LdesiredAccuracy := kCLLocationAccuracyReduced
    else LdesiredAccuracy := kCLLocationAccuracyThreeKilometers;
  end
  else raise Exception.Create('Error BD7DBCEB-5400-4FA4-8742-C3E3BC102601');
  FlocationManager.setdesiredAccuracy(LdesiredAccuracy);
  if FRequestAlwaysAuthorization then FlocationManager.setAllowsBackgroundLocationUpdates(True)
  else FlocationManager.setAllowsBackgroundLocationUpdates(false);
  FlocationManager.setPausesLocationUpdatesAutomatically(false);
  FlocationManager.startUpdatingLocation;
  {$ENDIF}
  {$ENDREGION}

end;

{*****************************************************}
procedure TALGeoPositionSensor.StartGeoPositionUpdates(
            const aMinDistance: Integer; // minimum distance between geoposition updates in meters
            const ACoarseGeoPosition: boolean = true;  // when ACoarseGeoPosition = true and APreciseGeoPosition = true
            const APreciseGeoPosition: boolean = true; // then user can choose either ACoarseGeoPosition or APreciseGeoPosition
            const AAlwaysAuthorization: boolean = False);
begin
  if FIsActivatingGpsAndGrantingGeoPositionAccess then
    raise Exception.Create('StartGeoPositionUpdates cannot be called while ActivateGpsAndGrantGeoPositionAccess is already running');
  //--
  StopGeoPositionUpdates;
  //--
  FRequestMinDistance := aMinDistance;
  //--
  FGeoPositionUpdatesDelayed := True;
  ActivateGpsAndGrantGeoPositionAccess(
    ACoarseGeoPosition,
    APreciseGeoPosition,
    AAlwaysAuthorization);
end;

{****************************************************}
procedure TALGeoPositionSensor.StopGeoPositionUpdates;
begin

  if FGeoPositionUpdatesActive then begin

    {$REGION ' ANDROID'}
    {$IF defined(ANDROID)}
    if FLocationManager <> nil then FLocationManager.removeUpdates(FAndroidLocationListener)
    else if FFusedLocationProviderClient <> nil then FFusedLocationProviderClient.removeLocationUpdates(FGMSLocationListener)
    else raise Exception.Create('Error E0008647-0D10-484F-B9EE-18D2FF831035');
    {$ENDIF}
    {$ENDREGION}

    {$REGION ' IOS'}
    {$IF defined(IOS)}
    FlocationManager.stopUpdatingLocation;
    {$ENDIF}
    {$ENDREGION}

  end;
  FGeoPositionUpdatesActive := False;
  FGeoPositionUpdatesDelayed := False;

end;

{***********************************************************************************************}
procedure TALGeoPositionSensor.ApplicationEventHandler(const Sender: TObject; const M: TMessage);
begin
  if not FApplicationEventHandlerEnabled then exit;
  if (M is TApplicationEventMessage) then begin

    var LMsg := TApplicationEventMessage(M);
    if (LMsg.Value.Event <> TApplicationEvent.BecameActive) then exit;

    {$IFDEF DEBUG}
    allog(
      'TALGeoPositionSensor.ApplicationEventHandler',
      'Event: '+TRttiEnumerationType.GetName(LMsg.Value.Event),
      TalLogType.verbose);
    {$ENDIF}

    //retrieve the Permissions Granted
    var LRestricted: boolean;
    var LCoarseGeoPositionGranted: Boolean;
    var LPreciseGeoPositionGranted: boolean;
    var LAuthorizedAlways: Boolean;
    GetPermissionsGranted(
      LRestricted,
      LCoarseGeoPositionGranted,
      LPreciseGeoPositionGranted,
      LAuthorizedAlways);

    //FWaitingGpsActivationResult
    If FWaitingGpsActivationResult then begin
      if ISGpsEnabled then DoActivateGpsAndGrantGeoPositionAccess
      else DoActivateGpsAndGrantGeoPositionAccessResult;
    end

    //FWaitingGrantCoarseGeoPositionAccessResult
    else if FWaitingGrantCoarseGeoPositionAccessResult then begin
      if LCoarseGeoPositionGranted or LPreciseGeoPositionGranted then DoActivateGpsAndGrantGeoPositionAccess
      else DoActivateGpsAndGrantGeoPositionAccessResult;
    end

    //FWaitingGrantPreciseGeoPositionAccessResult
    else if FWaitingGrantPreciseGeoPositionAccessResult then begin
      if LPreciseGeoPositionGranted then DoActivateGpsAndGrantGeoPositionAccess
      else DoActivateGpsAndGrantGeoPositionAccessResult;
    end

    //FWaitingAlwaysAuthorizationResult
    else if FWaitingAlwaysAuthorizationResult then begin
      if LAuthorizedAlways then DoActivateGpsAndGrantGeoPositionAccess
      else DoActivateGpsAndGrantGeoPositionAccessResult;
    end

    //Misc error
    else
      raise Exception.Create('Error 44770999-D42E-4035-B6EC-A90D91F03A26');

  end;
end;

{$REGION ' ANDROID'}
{$IF defined(android)}

{*********************************************************************************************************}
constructor TALGeoPositionSensor.TAndroidLocationListener.Create(AGeoPositionSensor: TALGeoPositionSensor);
begin
  inherited Create;
  FGeoPositionSensor := AGeoPositionSensor;
end;

{********************************************************************************************}
procedure TALGeoPositionSensor.TAndroidLocationListener.onFlushComplete(requestCode: Integer);
begin
  {$IFDEF DEBUG}
  allog('TALGeoPositionSensor.TAndroidLocationListener.onFlushComplete', 'requestCode: '+ALInttostrW(requestCode), TalLogType.verbose);
  {$ENDIF}
end;

{*********************************************************************************************}
procedure TALGeoPositionSensor.TAndroidLocationListener.onLocationChanged(location: JLocation);
begin
  if location = nil then exit;
  {$IFDEF DEBUG}
  allog('TALGeoPositionSensor.TAndroidLocationListener.onLocationChanged', JstringToString(location.tostring), TalLogType.verbose);
  {$ENDIF}
  if assigned(FGeoPositionSensor.OnGeoPositionUpdate) then begin
    FGeoPositionSensor.OnGeoPositionUpdate(
      Self, // const Sender: TObject;
      location.getLatitude, // const ALatitude: Double;
      location.getLongitude, // const ALongitude: Double;
      location.getAltitude, // const AAltitude: Double;
      location.getAccuracy, // const AAccuracy: Double;
      ALUnixMsToDateTime(location.getTime)); // Const ADateTime: TdateTime
  end;
end;

{******************************************************************************************}
procedure TALGeoPositionSensor.TAndroidLocationListener.onLocationChanged(locations: JList);
begin
  for var I := 0 to locations.size - 1 do
    onLocationChanged(TJLocation.Wrap(locations.get(I)));
end;

{********************************************************************************************}
procedure TALGeoPositionSensor.TAndroidLocationListener.onProviderDisabled(provider: JString);
begin
  {$IFDEF DEBUG}
  allog('TALGeoPositionSensor.TAndroidLocationListener.onProviderDisabled', 'provider: '+JstringToString(provider), TalLogType.verbose);
  {$ENDIF}
end;

{*******************************************************************************************}
procedure TALGeoPositionSensor.TAndroidLocationListener.onProviderEnabled(provider: JString);
begin
  {$IFDEF DEBUG}
  allog('TALGeoPositionSensor.TAndroidLocationListener.onProviderEnabled', 'provider: '+JstringToString(provider), TalLogType.verbose);
  {$ENDIF}
end;

{***************************************************************************************************************************}
procedure TALGeoPositionSensor.TAndroidLocationListener.onStatusChanged(provider: JString; status: Integer; extras: JBundle);
begin
  {$IFDEF DEBUG}
  allog('TALGeoPositionSensor.TAndroidLocationListener.onStatusChanged', 'provider: '+JstringToString(provider) + ' | status: ' + ALInttostrW(status), TalLogType.verbose);
  {$ENDIF}
end;

{*****************************************************************************************************}
constructor TALGeoPositionSensor.TGMSLocationListener.Create(AGeoPositionSensor: TALGeoPositionSensor);
begin
  inherited Create;
  FGeoPositionSensor := AGeoPositionSensor;
end;

{*****************************************************************************************}
procedure TALGeoPositionSensor.TGMSLocationListener.onLocationChanged(location: JLocation);
begin
  if location = nil then exit;
  {$IFDEF DEBUG}
  allog('TALGeoPositionSensor.TGMSLocationListener.onLocationChanged', JstringToString(location.tostring), TalLogType.verbose);
  {$ENDIF}
  if assigned(FGeoPositionSensor.OnGeoPositionUpdate) then begin
    FGeoPositionSensor.OnGeoPositionUpdate(
      Self, // const Sender: TObject;
      location.getLatitude, // const ALatitude: Double;
      location.getLongitude, // const ALongitude: Double;
      location.getAltitude, // const AAltitude: Double;
      location.getAccuracy, // const AAccuracy: Double;
      ALUnixMsToDateTime(location.getTime)); // Const ADateTime: TdateTime
  end;
end;

{*******************************************************************************************************}
procedure TALGeoPositionSensor.PermissionsRequestResultHandler(const Sender: TObject; const M: TMessage);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _CheckIfNeedForceShowRequestPermissionRationale;
  begin
    //We detect that the user previously denied the permission with
    //"never ask again" if before PermissionsRequestResultHandler
    //ShouldShowRequestPermissionRationale return false and after
    //ShouldShowRequestPermissionRationale return false also. It's really
    //a stupid way but no other way in Android. The problem is that
    //if the user denied the permission the very first time with "never
    //ask again" then we will immediatly show him another rationale
    if (not FRequestPermissionRationaleShowed) and
       (not ShouldShowRequestPermissionRationale(
              FWaitingGrantCoarseGeoPositionAccessResult, // const ACoarseGeoPosition: Boolean;
              FWaitingGrantPreciseGeoPositionAccessResult, // const APreciseGeoPosition: Boolean;
              FWaitingAlwaysAuthorizationResult)) then // const AAlwaysAuthorization: Boolean)) then
      DoActivateGpsAndGrantGeoPositionAccess(true{AForceShowRequestPermissionRationale})
    else
      DoActivateGpsAndGrantGeoPositionAccessResult;
  end;

begin
  if not FPermissionsRequestResultHandlerEnabled then exit;
  if (M is TPermissionsRequestResultMessage) then begin

    var LMsg := TPermissionsRequestResultMessage(M);
    if LMsg.Value.RequestCode <> RequestPermissionsCode then exit;

    {$IFDEF DEBUG}
    var LPermissions: Tarray<String>;
    var LPermissionsStr: String := '';
    setlength(LPermissions, LMsg.Value.Permissions.Length);
    for var I := low(LPermissions) to High(LPermissions) do begin
      LPermissions[I] := JStringToString(LMsg.Value.Permissions[I]);
      LPermissionsStr := LPermissionsStr + alifThenW(I > low(LPermissions), ';') + LPermissions[I];
    end;
    //--
    var LGrantResults: Tarray<Integer>;
    var LGrantResultsStr: String := '';
    setlength(LGrantResults, LMsg.Value.GrantResults.Length);
    for var I := low(LGrantResults) to High(LGrantResults) do begin
      LGrantResults[I] := LMsg.Value.GrantResults[I];
      LGrantResultsStr := LGrantResultsStr + alifThenW(I > low(LGrantResults), ';') + AlIntToStrW(LGrantResults[I]);
    end;
    //--
    allog(
      'TALGeoPositionSensor.PermissionsRequestResultHandler',
      'Permissions: ' + LPermissionsStr  + ' | '+
      'GrantResults: ' + LGrantResultsStr,
      TalLogType.verbose);
    {$ENDIF}

    //retrieve the Permissions Granted
    var LRestricted: boolean;
    var LCoarseGeoPositionGranted: Boolean;
    var LPreciseGeoPositionGranted: boolean;
    var LAuthorizedAlways: Boolean;
    GetPermissionsGranted(
      LRestricted,
      LCoarseGeoPositionGranted,
      LPreciseGeoPositionGranted,
      LAuthorizedAlways);

    //FWaitingGpsActivationResult
    if FWaitingGpsActivationResult then
      raise Exception.Create('Error AC7DFE47-8B4D-467C-AD78-F9EE73B19038')

    //FWaitingGrantCoarseGeoPositionAccessResult
    else if FWaitingGrantCoarseGeoPositionAccessResult then begin
      if LCoarseGeoPositionGranted or LPreciseGeoPositionGranted then DoActivateGpsAndGrantGeoPositionAccess
      else _CheckIfNeedForceShowRequestPermissionRationale;
    end

    //FWaitingGrantPreciseGeoPositionAccessResult
    else if FWaitingGrantPreciseGeoPositionAccessResult then begin
      if LPreciseGeoPositionGranted then DoActivateGpsAndGrantGeoPositionAccess
      else _CheckIfNeedForceShowRequestPermissionRationale;
    end

    //FWaitingAlwaysAuthorizationResult
    else if FWaitingAlwaysAuthorizationResult then begin
      if LAuthorizedAlways then DoActivateGpsAndGrantGeoPositionAccess
      else _CheckIfNeedForceShowRequestPermissionRationale;
    end

    //Misc error
    else
      raise Exception.Create('Error 96BD8595-B451-4037-A23D-80E80FB3625A');

  end;
end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}

{*********************************************************************************************************}
constructor TALGeoPositionSensor.TLocationManagerDelegate.Create(AGeoPositionSensor: TALGeoPositionSensor);
begin
  inherited Create;
  FGeoPositionSensor := AGeoPositionSensor;
end;

{*****************************************************************************************************************************}
procedure TALGeoPositionSensor.TLocationManagerDelegate.locationManager(manager: CLLocationManager; didFailWithError: NSError);
begin

  //Tells the delegate that the location manager was unable to retrieve a
  //location value.
  //
  //If you do not implement this method, Core Location throws an exception when
  //attempting to use location services.
  //
  //The location manager calls this method when it encounters an error trying
  //to get the location or heading data. If the location service is unable to
  //retrieve a location right away, it reports a kCLErrorLocationUnknown error
  //and keeps trying. In such a situation, you can simply ignore the error and
  //wait for a new event. If a heading could not be determined because of
  //strong interference from nearby magnetic fields, this method returns
  //kCLErrorHeadingFailure.
  //
  //If the user denies your app's use of the location service, this method
  //reports a kCLErrorDenied error. Upon receiving such an error, you should
  //stop the location service.

  {$IFDEF DEBUG}
  // kCLErrorLocationUnknown = 0;
  // kCLErrorDenied = 1;
  if (didFailWithError <> nil) then
    allog(
      'TALGeoPositionSensor.TLocationManagerDelegate.locationManager:didFailWithError',
      NSStrToStr(didFailWithError.localizedDescription) + ' | ' +
      'ErrorCode: ' + ALIntToStrW(didFailWithError.code),
      TalLogType.error)
  else
    allog('TALGeoPositionSensor.TLocationManagerDelegate.locationManager:didFailWithError', TalLogType.verbose);
  {$ENDIF}

end;

{*******************************************************************************************************************************}
procedure TALGeoPositionSensor.TLocationManagerDelegate.locationManager(manager: CLLocationManager; didUpdateHeading: CLHeading);
begin

  //Tells the delegate that the location manager received updated
  //heading information.
  //
  //Implementation of this method is optional but expected if you start heading
  //updates using the startUpdatingHeading method.
  //
  //The location manager object calls this method after you initially start the
  //heading service. Subsequent events are delivered when the previously
  //reported value changes by more than the value specified in the
  //headingFilter property of the location manager object.

  {$IFDEF DEBUG}
  allog('TALGeoPositionSensor.TLocationManagerDelegate.locationManager:didUpdateHeading', TalLogType.VERBOSE);
  {$ENDIF}
end;

{*************************************************************************************************************************************************************}
procedure TALGeoPositionSensor.TLocationManagerDelegate.locationManager(manager: CLLocationManager; didUpdateToLocation: CLLocation; fromLocation: CLLocation);
begin

  //Tells the delegate that a new location value is available.
  //
  //iOS 2.0–6.0 Deprecated

  {$IFDEF DEBUG}
  allog('TALGeoPositionSensor.TLocationManagerDelegate.locationManager:didUpdateToLocation:fromLocation', TalLogType.VERBOSE);
  {$ENDIF}

end;

{************************************************************************************************************************************************************}
procedure TALGeoPositionSensor.TLocationManagerDelegate.locationManager(manager: CLLocationManager; monitoringDidFailForRegion: CLRegion; withError: NSError);
begin

  //Tells the delegate that a region monitoring error occurred.
  //
  //If an error occurs while trying to monitor a given region, the
  //location manager sends this message to its delegate. Region monitoring
  //might fail because the region itself cannot be monitored or because there
  //was a more general failure in configuring the region monitoring service.
  //
  //Although implementation of this method is optional, it is recommended that
  //you implement it if you use region monitoring in your application.

  {$IFDEF DEBUG}
  if (withError <> nil) then
    allog(
      'TALGeoPositionSensor.TLocationManagerDelegate.locationManager:monitoringDidFailForRegion:withError',
      NSStrToStr(withError.localizedDescription) + ' | ' +
      'ErrorCode: ' + ALIntToStrW(withError.code),
      TalLogType.error)
  else
    allog('TALGeoPositionSensor.TLocationManagerDelegate.locationManager:monitoringDidFailForRegion:withError', TalLogType.verbose);
  {$ENDIF}

end;

{*******************************************************************************************************************************************************}
procedure TALGeoPositionSensor.TLocationManagerDelegate.locationManager(manager: CLLocationManager; didChangeAuthorizationStatus: CLAuthorizationStatus);
begin

  //Tells the delegate its authorization status when the app creates the
  //location manager and when the authorization status changes.
  //
  //iOS 4.2–14.0 Deprecated

  locationManagerDidChangeAuthorization(FGeoPositionSensor.fLocationManager);

end;

{************************************************************************************************************************}
procedure TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidChangeAuthorization(manager: CLLocationManager);
begin

  //Tells the delegate when the app creates the location manager and when the
  //authorization status changes.
  //
  //The system calls this method when the app creates the related object’s
  //CLLocationManager instance, and when the app’s authorization status changes.
  //The status informs the app whether it can access the user’s location.
  //
  //Use this delegate method to manage your app’s state changes in response to
  //its ability to use location information. For example, you may wish to
  //enable or disable your app’s location-related features, as appropriate. To
  //determine the app’s current authorization, read the new value of the
  //authorizationStatus and accuracyAuthorization properties of the location
  //manager.
  //
  //Important
  //Core Location always calls locationManagerDidChangeAuthorization: when the
  //user’s action results in an authorization status change, and when your app
  //creates an instance of CLLocationManager, whether your app runs in the
  //foreground or in the background.
  //
  //If the user’s choice doesn’t change the authorization status after you call
  //the requestWhenInUseAuthorization or requestAlwaysAuthorization method, the
  //location manager doesn’t report the current authorization status to this
  //method—the location manager only reports changes. For example, the location
  //manager calls this method when the status changes from
  //kCLAuthorizationStatusNotDetermined to kCLAuthorizationStatusAuthorizedWhenInUse.
  //
  //Events that Cause Authorization Status Changes
  //An app's authorization status changes in response to users’ actions. Users
  //can change permission for apps to use location information at any time.
  //The user can:
  //
  //  Change an app’s location authorization in Settings > Privacy > Location
  //  Services, or in Settings > (the app) > Location Services.
  //
  //  Turn location services on or off globally in Settings > Privacy >
  //  Location Services.
  //
  //  Choose Reset Location & Privacy in Settings > General > Reset.
  //
  //A user's response to location manager prompts can also change authorization
  //status. For instance, users may change the authorization status by
  //responding to the prompts initiated by calls to
  //requestWhenInUseAuthorization or requestAlwaysAuthorization methods. For
  //apps with Always authorization, users may change the authorization status
  //to When In Use when responding to the location usage reminder alert.
  //
  //When an app has temporary authorization, the authorization changes when the
  //user ceases to use the app.

  {$IFDEF DEBUG}
  Var LAuthorizationStatus: Integer;
  if TOSVersion.Check(14, 0) then LAuthorizationStatus := manager.authorizationStatus
  else LAuthorizationStatus := TCLLocationManager.OCClass.authorizationStatus;
  var LAuthorizationStatusStr: String;
  case LAuthorizationStatus of
    kCLAuthorizationStatusNotDetermined:       LAuthorizationStatusStr := 'kCLAuthorizationStatusNotDetermined';
    kCLAuthorizationStatusDenied:              LAuthorizationStatusStr := 'kCLAuthorizationStatusDenied';
    kCLAuthorizationStatusRestricted:          LAuthorizationStatusStr := 'kCLAuthorizationStatusRestricted';
    kCLAuthorizationStatusAuthorizedWhenInUse: LAuthorizationStatusStr := 'kCLAuthorizationStatusAuthorizedWhenInUse';
    kCLAuthorizationStatusauthorizedAlways:    LAuthorizationStatusStr := 'kCLAuthorizationStatusauthorizedAlways';
    else                                       LAuthorizationStatusStr := '???';
  end;
  allog('TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidChangeAuthorization', LAuthorizationStatusStr, TalLogType.VERBOSE);
  {$ENDIF}

  //exit if not flocationManagerDidChangeAuthorizationStatusEnabled
  if not FGeoPositionSensor.flocationManagerDidChangeAuthorizationStatusEnabled then exit;

  //retrieve the Permissions Granted
  var LRestricted: boolean;
  var LCoarseGeoPositionGranted: Boolean;
  var LPreciseGeoPositionGranted: boolean;
  var LAuthorizedAlways: Boolean;
  FGeoPositionSensor.GetPermissionsGranted(
    LRestricted,
    LCoarseGeoPositionGranted,
    LPreciseGeoPositionGranted,
    LAuthorizedAlways);

  //FWaitingGpsActivationResult
  if FGeoPositionSensor.FWaitingGpsActivationResult then
    raise Exception.Create('Error 507E3A3B-8949-42DF-BD53-03078B4C5454')

  //FWaitingGrantCoarseGeoPositionAccessResult
  else if FGeoPositionSensor.FWaitingGrantCoarseGeoPositionAccessResult then begin
    if LCoarseGeoPositionGranted or LPreciseGeoPositionGranted then FGeoPositionSensor.DoActivateGpsAndGrantGeoPositionAccess
    else FGeoPositionSensor.DoActivateGpsAndGrantGeoPositionAccessResult;
  end

  //FWaitingGrantPreciseGeoPositionAccessResult
  else if FGeoPositionSensor.FWaitingGrantPreciseGeoPositionAccessResult then begin
    if LPreciseGeoPositionGranted then FGeoPositionSensor.DoActivateGpsAndGrantGeoPositionAccess
    else FGeoPositionSensor.DoActivateGpsAndGrantGeoPositionAccessResult;
  end

  //FWaitingAlwaysAuthorizationResult
  else if FGeoPositionSensor.FWaitingAlwaysAuthorizationResult then begin
    if LAuthorizedAlways then FGeoPositionSensor.DoActivateGpsAndGrantGeoPositionAccess
    else FGeoPositionSensor.DoActivateGpsAndGrantGeoPositionAccessResult;
  end

  //Misc error
  else
    raise Exception.Create('Error 8AED11FC-A68E-447A-B598-E5AE11EC476C');

end;

{****************************************************************************************************************************************}
procedure TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidUpdateLocations(manager: CLLocationManager; locations: NSArray);
begin

  //Tells the delegate that new location data is available.

  for var I := 0 to locations.count - 1 do begin
    var LLocation := TCLLocation.Wrap(locations.objectAtIndex(I));
    {$IFDEF DEBUG}
    ALLog(
      'TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidUpdateLocations',
      'Date: ' + ALDateTimeToStrW(ALNSDateToUTCDateTime(LLocation.timestamp), ALDefaultFormatSettingsW) + ' | ' +
      'Latitude: ' + ALFormatFloatW('#.#####', LLocation.coordinate.latitude, ALDefaultFormatSettingsW) + ' | ' +
      'Longitude: ' + ALFormatFloatW('#.#####', LLocation.coordinate.longitude, ALDefaultFormatSettingsW) + ' | ' +
      'Accuracy: ' + ALFormatFloatW('#.##', LLocation.horizontalAccuracy, ALDefaultFormatSettingsW) + 'm',
      TalLogType.VERBOSE);
    {$ENDIF}
    //The location’s latitude and longitude identify the center of the
    //circle, and this value indicates the radius of that circle.
    //A negative value indicates that the location’s latitude and longitude
    //are invalid. i don't think this will ever happen but cost nothing to
    //add a trap here
    if (compareValue(LLocation.horizontalAccuracy, 0, Tepsilon.Vector) >= 0) and
       (assigned(FGeoPositionSensor.OnGeoPositionUpdate)) then begin
      FGeoPositionSensor.OnGeoPositionUpdate(
        Self, // const Sender: TObject;
        Llocation.coordinate.latitude, // const ALatitude: Double;
        Llocation.coordinate.longitude, // const ALongitude: Double;
        Llocation.Altitude, // const AAltitude: Double;
        Llocation.horizontalAccuracy, // const AAccuracy: Double;
        ALNSDateToUTCDateTime(LLocation.timestamp)); // Const ADateTime: TdateTime
    end;
  end;

end;

{*****************************************************************************************************************************************}
function TALGeoPositionSensor.TLocationManagerDelegate.locationManagerShouldDisplayHeadingCalibration(manager: CLLocationManager): Boolean;
begin

  //Asks the delegate whether the heading calibration alert should be displayed.
  //
  //Core Location may call this method in an effort to calibrate the onboard
  //hardware used to determine heading values. Typically, Core Location calls
  //this method at the following times:
  //
  //  The first time heading updates are ever requested
  //
  //  When Core Location observes a significant change in magnitude or
  //  inclination of the observed magnetic field

  //If you return YES from this method, Core Location displays the heading
  //calibration alert on top of the current window immediately. The calibration
  //alert prompts the user to move the device in a particular pattern so that
  //Core Location can distinguish between the Earth’s magnetic field and any
  //local magnetic fields. The alert remains visible until calibration is
  //complete or until you explicitly dismiss it by calling the
  //dismissHeadingCalibrationDisplay method. In the latter case, you can use
  //this method to set up a timer and dismiss the interface after a specified
  //amount of time has elapsed.
  //
  //Note
  //The calibration process is able to filter out only those magnetic fields
  //that move with the device. To calibrate a device that is near other sources
  //of magnetic interference, the user must either move the device away from
  //the source or move the source in conjunction with the device during the
  //calibration process.
  //
  //If you return NO from this method or do not provide an implementation for
  //it in your delegate, Core Location does not display the heading calibration
  //alert. Even if the alert is not displayed, calibration can still occur
  //naturally when any interfering magnetic fields move away from the device.
  //However, if the device is unable to calibrate itself for any reason, the
  //value in the headingAccuracy property of any subsequent events will reflect
  //the uncalibrated readings.

  {$IFDEF DEBUG}
  allog('TALGeoPositionSensor.TLocationManagerDelegate.locationManagerShouldDisplayHeadingCalibration', TalLogType.VERBOSE);
  {$ENDIF}

  Result := False;

end;

{********************************************************************************************************************************************************************}
procedure TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidDetermineStateForRegion(manager: CLLocationManager; state: CLRegionState; region: CLRegion);
begin

  //Tells the delegate about the state of the specified region.
  //
  //The location manager calls this method whenever there is a boundary
  //transition for a region. It calls this method in addition to calling the
  //locationManager:didEnterRegion: and locationManager:didExitRegion: methods.
  //The location manager also calls this method in response to a call to its
  //requestStateForRegion: method, which runs asynchronously.

  {$IFDEF DEBUG}
  allog('TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidDetermineStateForRegion', TalLogType.VERBOSE);
  {$ENDIF}

end;

{*****************************************************************************************************************************************************************************************************************}
procedure TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidRangeBeaconsSatisfyingConstraint(manager: CLLocationManager; didRangeBeacons: NSArray; satisfyingConstraint: CLBeaconIdentityConstraint);
begin

  //Tells the delegate that the location manager detected at least one beacon
  //that satisfies the provided constraint.

  {$IFDEF DEBUG}
  allog('TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidRangeBeaconsSatisfyingConstraint', TalLogType.VERBOSE);
  {$ENDIF}

end;

{*************************************************************************************************************************************************************************************************************************}
procedure TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidFailRangingBeaconsForConstraintError(manager: CLLocationManager; didFailRangingBeaconsForConstraint: CLBeaconIdentityConstraint; error: NSError);
begin

  //Tells the delegate that the location manager couldn’t detect any beacons
  //that satisfy the provided constraint.

  {$IFDEF DEBUG}
  allog('TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidFailRangingBeaconsForConstraintError', TalLogType.VERBOSE);
  {$ENDIF}

end;

{*******************************************************************************************************************************************************************}
procedure TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidRangeBeaconsInRegion(manager: CLLocationManager; beacons: NSArray; region: CLBeaconRegion);
begin

  //iOS 7.0–13.0 Deprecated
  //
  //Tells the delegate that one or more beacons are in range.
  //
  //The location manager calls this method when a new set of beacons
  //becomes available in the specified region or when a beacon goes out
  //of range. The location manager also calls this method when the range
  //of a beacon changes; for example, when a beacon gets closer.

  {$IFDEF DEBUG}
  allog('TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidRangeBeaconsInRegion', TalLogType.VERBOSE);
  {$ENDIF}

end;

{*********************************************************************************************************************************************************************************}
procedure TALGeoPositionSensor.TLocationManagerDelegate.locationManagerRangingBeaconsDidFailForRegionWithError(manager: CLLocationManager; region: CLBeaconRegion; error: NSError);
begin

  //iOS 7.0–13.0 Deprecated
  //
  //Tells the delegate that an error occurred while gathering ranging
  //information for a set of beacons.
  //
  //Errors occur most often when registering a beacon region failed. If
  //the region object itself is invalid or if it contains invalid data, the
  //location manager calls this method to report the problem.

  {$IFDEF DEBUG}
  if (error <> nil) then
    allog(
      'TALGeoPositionSensor.TLocationManagerDelegate.locationManagerRangingBeaconsDidFailForRegionWithError',
      NSStrToStr(error.localizedDescription) + ' | ' +
      'ErrorCode: ' + ALIntToStrW(error.code),
      TalLogType.error)
  else
    allog('TALGeoPositionSensor.TLocationManagerDelegate.locationManagerRangingBeaconsDidFailForRegionWithError', TalLogType.verbose);
  {$ENDIF}

end;

{**********************************************************************************************************************************}
procedure TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidEnterRegion(manager: CLLocationManager; region: CLRegion);
begin

  //Tells the delegate that location updates were paused.
  //
  //Because regions are a shared application resource, every active location
  //manager object delivers this message to its associated delegate. It
  //doesn’t matter which location manager actually registered the specified
  //region. If multiple location managers share a delegate object, that
  //delegate receives the message multiple times.
  //
  //The region object provided may not be the same one that was registered.
  //As a result, you should never perform pointer-level comparisons to
  //determine equality. Instead, use the region’s identifier string to
  //determine if your delegate should respond.

  {$IFDEF DEBUG}
  allog('TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidEnterRegion', TalLogType.VERBOSE);
  {$ENDIF}

end;

{*********************************************************************************************************************************}
procedure TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidExitRegion(manager: CLLocationManager; region: CLRegion);
begin

  //Tells the delegate that the user left the specified region.
  //
  //Because regions are a shared application resource, every active
  //location manager object delivers this message to its associated delegate.
  //It doesn't matter which location manager actually registered the
  //specified region. If multiple location managers share a delegate object,
  //that delegate receives the message multiple times.
  //
  //The region object provided may not be the same one that was registered.
  //As a result, you should never perform pointer-level comparisons to
  //determine equality. Instead, use the region’s identifier string to
  //determine if your delegate should respond.


  {$IFDEF DEBUG}
  allog('TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidExitRegion', TalLogType.VERBOSE);
  {$ENDIF}

end;

{***********************************************************************************************************************************************}
procedure TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidStartMonitoringForRegion(manager: CLLocationManager; region: CLRegion);
begin

  //Tells the delegate that a new region is being monitored.

  {$IFDEF DEBUG}
  allog('TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidStartMonitoringForRegion', TalLogType.VERBOSE);
  {$ENDIF}

end;

{*************************************************************************************************************************}
procedure TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidPauseLocationUpdates(manager: CLLocationManager);
begin

  //Tells the delegate that location updates were paused.
  //
  //When the location manager detects that the device’s location is not
  //changing, it can pause the delivery of updates in order to shut down the
  //appropriate hardware and save power. When it does this, it calls this
  //method to let your app know that this has happened.
  //
  //After a pause occurs, it is your responsibility to restart location
  //services again at an appropriate time. You might use your implementation
  //of this method to start region monitoring at the user's current
  //location or enable the visits location service to determine when the
  //user starts moving again. Another alternative is to restart location
  //services immediately with a reduced accuracy (which can save power)
  //and then return to a greater accuracy only after the user starts
  //moving again.

  {$IFDEF DEBUG}
  allog('TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidPauseLocationUpdates', TalLogType.VERBOSE)
  {$ENDIF}

end;

{**************************************************************************************************************************}
procedure TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidResumeLocationUpdates(manager: CLLocationManager);
begin

  //Tells the delegate that the delivery of location updates has resumed.
  //
  //When you restart location services after an automatic pause, Core
  //Location calls this method to notify your app that services have resumed.
  //You are responsible for restarting location services in your app.
  //Core Location does not resume updates automatically after it pauses them.
  //For tips on how to restart location services when a pause occurs, see
  //the discussion of the locationManagerDidPauseLocationUpdates: method.
  //
  //NOTE: this is false :
  //You are responsible for restarting location services in your app. Core
  //Location does not resume updates automatically after it pauses them.
  //i see that when the app is in background, then
  //locationManagerDidPauseLocationUpdates is called and then if later you
  //move the app to the foreground then locationManagerDidResumeLocationUpdates
  //will be called automatiquelly !

  {$IFDEF DEBUG}
  allog('TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidResumeLocationUpdates', TalLogType.VERBOSE)
  {$ENDIF}

end;

{***************************************************************************************************************************************************}
procedure TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidFinishDeferredUpdatesWithError(manager: CLLocationManager; error: NSError);
begin

  //Tells the delegate that updates will no longer be deferred.
  //
  //The location manager object calls this method to let you know that it has
  //stopped deferring the delivery of location events. The manager may call
  //this method for any number of reasons. For example, it calls it when you
  //stop location updates altogether, when you ask the location manager to
  //disallow deferred updates, or when a condition for deferring updates (such
  //as exceeding a timeout or distance parameter) is met.

  {$IFDEF DEBUG}
  if (error <> nil) then
    allog(
      'TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidFinishDeferredUpdatesWithError',
      NSStrToStr(error.localizedDescription) + ' | ' +
      'ErrorCode: ' + ALIntToStrW(error.code),
      TalLogType.error)
  else
    allog('TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidFinishDeferredUpdatesWithError', TalLogType.verbose);
  {$ENDIF}

end;

{**************************************************************************************************************************}
procedure TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidVisit(manager: CLLocationManager; visit: CLVisit);
begin

  //Tells the delegate that a new visit-related event was received.
  //
  //The location manager calls this method whenever it has new visit event
  //to report to your app.

  {$IFDEF DEBUG}
  allog('TALGeoPositionSensor.TLocationManagerDelegate.locationManagerDidVisit', TalLogType.VERBOSE);
  {$ENDIF}

end;

{$ENDIF}
{$ENDREGION}

end.
