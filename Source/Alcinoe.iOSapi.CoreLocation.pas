unit Alcinoe.iOSapi.CoreLocation;

interface

{$I Alcinoe.inc}

uses
  Macapi.ObjectiveC,
  iOSapi.CoreLocation,
  iOSapi.Foundation;

{$M+}

type

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-18621 has been resolved. If resolved, remove the definitions below.'}
  {$ENDIF}
  CLBeaconIdentityConstraint = interface; // https://quality.embarcadero.com/browse/RSP-18621
  CLVisit = interface; // https://quality.embarcadero.com/browse/RSP-18621

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-18621 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  CLLocationManagerDelegate = interface(IObjectiveC)
    ['{69DC1DAB-5A4D-44F9-8291-F00DAB2E7511}']
    procedure locationManager(manager: CLLocationManager; didFailWithError: NSError); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; didUpdateHeading: CLHeading); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; didUpdateToLocation: CLLocation; fromLocation: CLLocation); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; monitoringDidFailForRegion: CLRegion; withError: NSError); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; didChangeAuthorizationStatus: CLAuthorizationStatus); overload; cdecl;
    procedure locationManagerDidChangeAuthorization(manager: CLLocationManager); cdecl; // https://quality.embarcadero.com/browse/RSP-18621
    [MethodName('locationManager:didUpdateLocations:')]
    procedure locationManagerDidUpdateLocations(manager: CLLocationManager; locations: NSArray); cdecl;
    function locationManagerShouldDisplayHeadingCalibration(manager: CLLocationManager): Boolean; cdecl;
    [MethodName('locationManager:didDetermineState:forRegion:')]
    procedure locationManagerDidDetermineStateForRegion(manager: CLLocationManager; state: CLRegionState; region: CLRegion); cdecl;
    [MethodName('locationManager:didRangeBeacons:satisfyingConstraint:')]  // https://quality.embarcadero.com/browse/RSP-18621
    procedure locationManagerDidRangeBeaconsSatisfyingConstraint(manager: CLLocationManager; didRangeBeacons: NSArray; satisfyingConstraint: CLBeaconIdentityConstraint); cdecl;  // https://quality.embarcadero.com/browse/RSP-18621
    [MethodName('locationManager:didFailRangingBeaconsForConstraint:error:')]  // https://quality.embarcadero.com/browse/RSP-18621
    procedure locationManagerDidFailRangingBeaconsForConstraintError(manager: CLLocationManager; didFailRangingBeaconsForConstraint: CLBeaconIdentityConstraint; error: NSError); cdecl;  // https://quality.embarcadero.com/browse/RSP-18621
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
    [MethodName('locationManager:didVisit:')]                                             // https://quality.embarcadero.com/browse/RSP-18621
    procedure locationManagerDidVisit(manager: CLLocationManager; visit: CLVisit); cdecl; // https://quality.embarcadero.com/browse/RSP-18621
  end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-18621 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  CLBeaconIdentityConstraintClass = interface(NSObjectClass)
    ['{B81513FE-6EDE-4BDF-8AFB-F467B106962A}']
  end;
  CLBeaconIdentityConstraint = interface(NSObject)
    ['{41B39E7D-4037-431E-B6FA-FC97B81978F4}']
    function UUID: NSUUID; cdecl;
    function major: NSNumber; cdecl;
    function minor: NSNumber; cdecl;
    [MethodName('initWithUUID:')]
    function initWithUUID(uuid: NSUUID): Pointer{instancetype}; cdecl; overload;
    [MethodName('initWithUUID:major:')]
    function initWithUUID(uuid: NSUUID; major: CLBeaconMajorValue): Pointer{instancetype}; cdecl; overload;
    [MethodName('initWithUUID:major:minor:')]
    function initWithUUID(uuid: NSUUID; major: CLBeaconMajorValue; minor: CLBeaconMinorValue): Pointer{instancetype}; cdecl; overload;
  end;
  TCLBeaconIdentityConstraint = class(TOCGenericImport<CLBeaconIdentityConstraintClass, CLBeaconIdentityConstraint>)  end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-18621 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  CLVisitClass = interface(NSObjectClass)
    ['{01330775-4296-4A0D-8A1F-256E93DA4559}']
  end;
  CLVisit = interface(NSObject)
    ['{3B54ADD0-A799-4DC0-98F1-7D07F64DBE24}']
    function arrivalDate: NSDate; cdecl;
    function departureDate: NSDate; cdecl;
    function coordinate: CLLocationCoordinate2D; cdecl;
    function horizontalAccuracy: CLLocationAccuracy; cdecl;
  end;
  TCLVisit = class(TOCGenericImport<CLVisitClass, CLVisit>)  end;

implementation

end.
