{**********************************************************}
{                                                          }
{           CodeGear Delphi Runtime Library                }
{                                                          }
{ Delphi-Objective-C Bridge                                }
{ Interfaces for Cocoa framework CoreLocation              }
{                                                          }
{ Copyright (c) 2008-2010, Apple Inc. All rights reserved. }
{                                                          }
{ Translator: Embarcadero Technologies, Inc.               }
{   Copyright(c) 2012-2017 Embarcadero Technologies, Inc.  }
{              All rights reserved                         }
{                                                          }
{**********************************************************}

unit iOSapi.CoreLocation;

interface

uses
  Macapi.ObjectiveC, Macapi.ObjCRuntime, iOSapi.CocoaTypes, iOSapi.Foundation;

const
  CLDeviceOrientationFaceDown = 6;
  CLDeviceOrientationFaceUp = 5;
  CLDeviceOrientationLandscapeLeft = 3;
  CLDeviceOrientationLandscapeRight = 4;
  CLDeviceOrientationPortrait = 1;
  CLDeviceOrientationPortraitUpsideDown = 2;
  CLDeviceOrientationUnknown = 0;
  kCLAuthorizationStatusAuthorized = 3;
  kCLAuthorizationStatusDenied = 2;
  kCLAuthorizationStatusNotDetermined = 0;
  kCLAuthorizationStatusRestricted = 1;
  kCLAuthorizationStatusAuthorizedAlways  = kCLAuthorizationStatusAuthorized ;
  kCLAuthorizationStatusAuthorizedWhenInUse = 4; 
  kCLErrorDenied = 1;
  kCLErrorGeocodeCanceled = 10;
  kCLErrorGeocodeFoundNoResult = 8;
  kCLErrorGeocodeFoundPartialResult = 9;
  kCLErrorHeadingFailure = 3;
  kCLErrorLocationUnknown = 0;
  kCLErrorNetwork = 2;
  kCLErrorRegionMonitoringDenied = 4;
  kCLErrorRegionMonitoringFailure = 5;
  kCLErrorRegionMonitoringResponseDelayed = 7;
  kCLErrorRegionMonitoringSetupDelayed = 6;
  kCLLocationAccuracyThreeKilometers: Double = 3000;
  kCLLocationAccuracyKilometer: Double = 1000;
  kCLLocationAccuracyHundredMeters: Double = 100;
  kCLLocationAccuracyNearestTenMeters: Double = 10;
  kCLLocationAccuracyBest: Double = -1;
  kCLLocationAccuracyBestForNavigation: Double = -1;
  CLRegionStateUnknown = 0;
  CLRegionStateInside = 1;
  CLRegionStateOutside = 2;
  CLProximityUnknown = 0;
  CLProximityImmediate = 1;
  CLProximityNear = 2;
  CLProximityFar = 3;
  kCLErrorDeferredFailed = 11;
  kCLErrorDeferredNotUpdatingLocation = 12;
  kCLErrorDeferredAccuracyTooLow = 13;
  kCLErrorDeferredDistanceFiltered = 14;
  kCLErrorDeferredCanceled = 15;
  kCLErrorRangingUnavailable = 16;
  kCLErrorRangingFailure = 17;
  CLActivityTypeOther = 1;
  CLActivityTypeAutomotiveNavigation = 2;
  CLActivityTypeFitness = 3;
  CLActivityTypeOtherNavigation = 4;


// ===== External functions =====

const
  libCoreLocation = '/System/Library/Frameworks/CoreLocation.framework/CoreLocation';

// ===== Typedefs and structs =====
type
{$M+}
  CLAuthorizationStatus = NSUInteger;
  CLDeviceOrientation = NSUInteger;
  CLHeadingComponentValue = Double;
  CLLocationAccuracy = Double;
  CLLocationDegrees = Double;
  CLLocationDirection = Double;
  CLLocationDistance = Double;
  CLLocationSpeed = Double;

  CLLocationCoordinate2D = record
    latitude: CLLocationDegrees;
    longitude: CLLocationDegrees;
  end;
  PCLLocationCoordinate2D = ^CLLocationCoordinate2D;
  CLBeaconMajorValue = UInt16;
  CLBeaconMinorValue = UInt16;
  CLRegionState = NSInteger;
  CLProximity = NSInteger;
  CLError = NSInteger;
  CLGeocodeCompletionHandler = procedure(param1: NSArray; param2: NSError) of object;
  CLActivityType = NSInteger;

type
{$M+}
  TCLGeocodeCompletionHandler = procedure (const placemark: NSArray;const error: NSError) of object;
// ===== Forward declarations =====

  CLLocationManagerDelegate = interface;

  CLPlacemark = interface;
  CLRegion = interface;
  CLVisit = interface; // https://quality.embarcadero.com/browse/RSP-18621
  CLLocationManager = interface;
  CLGeocoder = interface;
  CLHeading = interface;
  CLLocation = interface;
  CLBeaconRegion = interface;

// ===== Protocol declarations =====

  CLLocationManagerDelegate = interface(IObjectiveC)
    ['{A1481CE0-8EE2-497A-9132-5309F0DCCA90}']
    procedure locationManager(manager: CLLocationManager; didFailWithError: NSError); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; didUpdateHeading: CLHeading); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; didUpdateToLocation: CLLocation; fromLocation: CLLocation); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; monitoringDidFailForRegion: CLRegion; withError: NSError); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; didChangeAuthorizationStatus: CLAuthorizationStatus); overload; cdecl;
    [MethodName('locationManager:didUpdateLocations:')]
    procedure locationManagerDidUpdateLocations(manager: CLLocationManager; locations: NSArray); cdecl;
    function locationManagerShouldDisplayHeadingCalibration(manager: CLLocationManager): Boolean; cdecl;
    [MethodName('locationManager:didDetermineState:forRegion:')]
    procedure locationManagerDidDetermineStateForRegion(manager: CLLocationManager; state: CLRegionState; region: CLRegion); cdecl;
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

// ===== Interface declarations =====

  CLPlacemarkClass = interface(NSObjectClass)
    ['{A0539851-6F72-4A85-A6DF-5ACDAD4DC09E}']
  end;
  CLPlacemark = interface(NSObject)
    ['{A33A0164-05D6-49DE-A8DB-DC984C948779}']
    function ISOcountryCode: NSString; cdecl;
    function addressDictionary: NSDictionary; cdecl;
    function administrativeArea: NSString; cdecl;
    function areasOfInterest: NSArray; cdecl;
    function country: NSString; cdecl;
    function initWithPlacemark(placemark: CLPlacemark): Pointer; cdecl;
    function inlandWater: NSString; cdecl;
    function locality: NSString; cdecl;
    function location: CLLocation; cdecl;
    function name: NSString; cdecl;
    function ocean: NSString; cdecl;
    function postalCode: NSString; cdecl;
    function region: CLRegion; cdecl;
    function subAdministrativeArea: NSString; cdecl;
    function subLocality: NSString; cdecl;
    function subThoroughfare: NSString; cdecl;
    function thoroughfare: NSString; cdecl;
  end;
  TCLPlacemark = class(TOCGenericImport<CLPlacemarkClass, CLPlacemark>)  end;

  CLRegionClass = interface(NSObjectClass)
    ['{92664E5E-D93F-4FD5-9A70-7EF649C7DF7E}']
  end;
  CLRegion = interface(NSObject)
    ['{DC764906-7A0D-429C-A4B4-C939254AC7C2}']
    function center: CLLocationCoordinate2D; cdecl;
    function containsCoordinate(coordinate: CLLocationCoordinate2D): Boolean; cdecl;
    function identifier: NSString; cdecl;
    function initCircularRegionWithCenter(center: CLLocationCoordinate2D; radius: CLLocationDistance; identifier: NSString): Pointer; cdecl;
    function radius: CLLocationDistance; cdecl;
    procedure setNotifyOnEntry(notifyOnEntry: Boolean); cdecl;
    function notifyOnEntry : Boolean; cdecl;
    procedure setNotifyOnExit(notifyOnExit: Boolean); cdecl;
    function notifyOnExit : Boolean; cdecl;
  end;
  TCLRegion = class(TOCGenericImport<CLRegionClass, CLRegion>)  end;

  // https://quality.embarcadero.com/browse/RSP-18621
  CLVisitClass = interface(NSObjectClass)
    ['{8526B69B-5AD8-4B4B-91DC-6C06474BE925}']
  end;
  CLVisit = interface(NSObject)
    ['{BB121072-B081-4C90-BB85-6FCC24DF8223}']
    function arrivalDate: NSDate; cdecl;
    function departureDate: NSDate; cdecl;
    function coordinate: CLLocationCoordinate2D; cdecl;
    function horizontalAccuracy: CLLocationAccuracy; cdecl;
  end;
  TCLVisit = class(TOCGenericImport<CLVisitClass, CLVisit>)  end;

  CLLocationManagerClass = interface(NSObjectClass)
    ['{4422C248-523F-4237-8966-2A322F58A77C}']
    {class} function authorizationStatus: CLAuthorizationStatus; cdecl;
    {class} function deferredLocationUpdatesAvailable : Boolean; cdecl;
    {class} function headingAvailable: Boolean; cdecl; overload;
    {class} function isRangingAvailable: Boolean; cdecl;
    {class} function isMonitoringAvailableForClass(regionClass: Pointer) : Boolean; cdecl;
    {class} function locationServicesEnabled: Boolean; cdecl; overload;
    {class} function regionMonitoringAvailable: Boolean; cdecl;
    {class} function regionMonitoringEnabled: Boolean; cdecl;
    {class} function significantLocationChangeMonitoringAvailable: Boolean; cdecl;
  end;

  CLLocationManager = interface(NSObject)
    ['{48C6C581-DA14-4444-A65D-896A0EF5E828}']
    function activityType : CLActivityType; cdecl;
    procedure allowDeferredLocationUpdatesUntilTraveled(distance: CLLocationDistance; timeout: NSTimeInterval); cdecl;
    function delegate: Pointer; cdecl;
    function desiredAccuracy: CLLocationAccuracy; cdecl;
    procedure disallowDeferredLocationUpdates; cdecl;
    procedure dismissHeadingCalibrationDisplay; cdecl;
    function distanceFilter: CLLocationDistance; cdecl;
    function heading: CLHeading; cdecl;
    function headingAvailable: Boolean; cdecl;
    function headingFilter: CLLocationDegrees; cdecl;
    function headingOrientation: CLDeviceOrientation; cdecl;
    function location: CLLocation; cdecl;
    function locationServicesEnabled: Boolean; cdecl;
    function maximumRegionMonitoringDistance: CLLocationDistance; cdecl;
    function monitoredRegions: NSSet; cdecl;
    function pausesLocationUpdatesAutomatically : Boolean; cdecl;
    function purpose: NSString; cdecl;
    function rangedRegions: NSSet; cdecl;
    function allowsBackgroundLocationUpdates: Boolean; cdecl;
    procedure requestStateForRegion(region: CLRegion); cdecl;
    procedure requestAlwaysAuthorization; cdecl;
    procedure requestWhenInUseAuthorization; cdecl;
    procedure requestLocation; cdecl;
    procedure setActivityType(activityType: CLActivityType); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDesiredAccuracy(desiredAccuracy: CLLocationAccuracy); cdecl;
    procedure setDistanceFilter(distanceFilter: CLLocationDistance); cdecl;
    procedure setHeadingFilter(headingFilter: CLLocationDegrees); cdecl;
    procedure setHeadingOrientation(headingOrientation: CLDeviceOrientation); cdecl;
    procedure setPausesLocationUpdatesAutomatically(pausesLocationUpdatesAutomatically: Boolean); cdecl;
    procedure setPurpose(purpose: NSString); cdecl;
    procedure setAllowsBackgroundLocationUpdates(AllowsBackgroundLocationUpdates: Boolean); cdecl;
    procedure startMonitoringForRegion(region: CLRegion); cdecl; overload;
    procedure startMonitoringForRegion(region: CLRegion; desiredAccuracy: CLLocationAccuracy); cdecl; overload;
    procedure startMonitoringSignificantLocationChanges; cdecl;
    procedure startRangingBeaconsInRegion(region: CLBeaconRegion); cdecl;
    procedure startUpdatingHeading; cdecl;
    procedure startUpdatingLocation; cdecl;
    procedure stopMonitoringForRegion(region: CLRegion); cdecl;
    procedure stopMonitoringSignificantLocationChanges; cdecl;
    procedure stopRangingBeaconsInRegion(region: CLBeaconRegion); cdecl;
    procedure stopUpdatingHeading; cdecl;
    procedure stopUpdatingLocation; cdecl;
    procedure startMonitoringVisits; cdecl; // https://quality.embarcadero.com/browse/RSP-18621
    procedure stopMonitoringVisits; cdecl; // https://quality.embarcadero.com/browse/RSP-18621
  end;
  TCLLocationManager = class(TOCGenericImport<CLLocationManagerClass, CLLocationManager>)  end;

  CLGeocoderClass = interface(NSObjectClass)
    ['{995D14FC-7FFA-4523-947D-B7F94E2BBB5E}']
  end;
  CLGeocoder = interface(NSObject)
    ['{34147362-3DFB-4489-AD97-59BDF0AEE825}']
    procedure cancelGeocode; cdecl;
    function isGeocoding: Boolean; cdecl;
    procedure geocodeAddressDictionary(addressDictionary: NSDictionary; completionHandler: TCLGeocodeCompletionHandler); cdecl;
    procedure geocodeAddressString(addressString: NSString; completionHandler: TCLGeocodeCompletionHandler); cdecl; overload;
    procedure geocodeAddressString(addressString: NSString; region: CLRegion; completionHandler: TCLGeocodeCompletionHandler); cdecl; overload;
    procedure reverseGeocodeLocation(location: CLLocation; completionHandler: TCLGeocodeCompletionHandler); cdecl;
    [MethodName('geocodeAddressString:completionHandler:')]
    procedure geocodeAddressStringCompletionHandler(addressString: NSString; completionHandler: CLGeocodeCompletionHandler); cdecl;
    [MethodName('geocodeAddressString:inRegion:completionHandler:')]
    procedure geocodeAddressStringInRegionCompletionHandler(addressString: NSString; region: CLRegion; completionHandler: CLGeocodeCompletionHandler); cdecl;
  end;
  TCLGeocoder = class(TOCGenericImport<CLGeocoderClass, CLGeocoder>)  end;

  CLHeadingClass = interface(NSObjectClass)
    ['{E160A766-8EB9-4D39-B830-94EF1119CA89}']
  end;
  CLHeading = interface(NSObject)
    ['{F1C59601-D23E-4D1E-B13B-D1F5AFD9ACE9}']
    function description: NSString; cdecl;
    function headingAccuracy: CLLocationDirection; cdecl;
    function magneticHeading: CLLocationDirection; cdecl;
    function timestamp: NSDate; cdecl;
    function trueHeading: CLLocationDirection; cdecl;
    function x: CLHeadingComponentValue; cdecl;
    function y: CLHeadingComponentValue; cdecl;
    function z: CLHeadingComponentValue; cdecl;
  end;
  TCLHeading = class(TOCGenericImport<CLHeadingClass, CLHeading>)  end;

  CLLocationClass = interface(NSObjectClass)
    ['{70F80709-7F9D-4427-A243-32B28792018C}']
  end;
  CLLocation = interface(NSObject)
    ['{3285343B-6FDA-4144-87ED-5CF72565212E}']
    function altitude: CLLocationDistance; cdecl;
    function coordinate: CLLocationCoordinate2D; cdecl;
    function course: CLLocationDirection; cdecl;
    function description: NSString; cdecl;
    function distanceFromLocation(location: CLLocation): CLLocationDistance; cdecl;
    function getDistanceFrom(location: CLLocation): CLLocationDistance; cdecl;
    function horizontalAccuracy: CLLocationAccuracy; cdecl;
    function initWithCoordinate(coordinate: CLLocationCoordinate2D; altitude: CLLocationDistance; horizontalAccuracy: CLLocationAccuracy; verticalAccuracy: CLLocationAccuracy; course: CLLocationDirection; speed: CLLocationSpeed; timestamp: NSDate): Pointer; cdecl; overload;
    function initWithCoordinate(coordinate: CLLocationCoordinate2D; altitude: CLLocationDistance; horizontalAccuracy: CLLocationAccuracy; verticalAccuracy: CLLocationAccuracy; timestamp: NSDate): Pointer; cdecl; overload;
    function initWithLatitude(latitude: CLLocationDegrees; longitude: CLLocationDegrees): Pointer; cdecl;
    function speed: CLLocationSpeed; cdecl;
    function timestamp: NSDate; cdecl;
    function verticalAccuracy: CLLocationAccuracy; cdecl;
  end;
  TCLLocation = class(TOCGenericImport<CLLocationClass, CLLocation>)  end;

  CLBeaconRegionClass = interface(CLRegionClass)
    ['{E4DFC71D-F5BE-4C90-AAAF-09952242D3A5}']
  end;

  CLBeaconRegion = interface(CLRegion)
    ['{56116AA2-6986-4808-BDAA-DD04F6A96F8C}']
    [MethodName('initWithProximityUUID:identifier:')]
    function initWithProximityUUIDIdentifier(proximityUUID: NSUUID; identifier: NSString): Pointer; cdecl;
    [MethodName('initWithProximityUUID:major:identifier:')]
    function initWithProximityUUIDMajorIdentifier(proximityUUID: NSUUID; major: CLBeaconMajorValue; identifier: NSString): Pointer; cdecl;
    [MethodName('initWithProximityUUID:major:minor:identifier:')]
    function initWithProximityUUIDMajorMinorIdentifier(proximityUUID: NSUUID; major: CLBeaconMajorValue; minor: CLBeaconMinorValue; identifier: NSString) : Pointer; cdecl;
    function peripheralDataWithMeasuredPower(measuredPower: NSNumber): NSMutableDictionary; cdecl;
    function proximityUUID: NSUUID; cdecl;
    function major: NSNumber; cdecl;
    function minor: NSNumber; cdecl;
    procedure setNotifyEntryStateOnDisplay(notifyEntryStateOnDisplay: Boolean); cdecl;
    function notifyEntryStateOnDisplay: Boolean; cdecl;
  end;
  TCLBeaconRegion = class(TOCGenericImport<CLBeaconRegionClass, CLBeaconRegion>)  end;

  CLBeaconClass = interface(NSObjectClass)
    ['{FC079E8A-099B-48DB-ACA5-57CDDECE5F2A}']
  end;

  CLBeacon = interface(NSObject)
    ['{86AD6390-D5BA-40C1-9043-B826B64F6F83}']
    function proximityUUID: NSUUID; cdecl;
    function major: NSNumber; cdecl;
    function minor: NSNumber; cdecl;
    function proximity: CLProximity; cdecl;
    function accuracy: CLLocationAccuracy; cdecl;
    function rssi: NSInteger; cdecl;
  end;
  TCLBeacon = class(TOCGenericImport<CLBeaconClass, CLBeacon>)  end;

  CLCircularRegionClass = interface(CLRegionClass)
    ['{B2E71730-FB37-4DB4-9D49-8A004BB6C62C}']
  end;

  CLCircularRegion = interface(CLRegion)
    ['{FF4DCF91-376B-41BB-B60A-880BEBB5B4EE}']
    function initWithCenter(center: CLLocationCoordinate2D; radius: CLLocationDistance; identifier: pointer): Pointer; cdecl; // https://quality.embarcadero.com/browse/RSP-15717
    function center: CLLocationCoordinate2D; cdecl;
    function radius: CLLocationDistance; cdecl;
    function containsCoordinate(coordinate: CLLocationCoordinate2D): Boolean; cdecl;
  end;
  TCLCircularRegion = class(TOCGenericImport<CLCircularRegionClass, CLCircularRegion>)  end;

function CLLocationCoordinate2DIsValid(coord: CLLocationCoordinate2D): Boolean; cdecl; external libCoreLocation name _PU + 'CLLocationCoordinate2DIsValid';
function CLLocationCoordinate2DMake(latitude: CLLocationDegrees; longitude: CLLocationDegrees): CLLocationCoordinate2D; cdecl; external libCoreLocation name _PU + 'CLLocationCoordinate2DMake';

implementation

{$IF defined(IOS) and NOT defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  CLModule: THandle;
{$ENDIF IOS}

function kCLErrorUserInfoAlternateRegionKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreLocation, 'kCLErrorUserInfoAlternateRegionKey');
end;

function kCLErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libCoreLocation, 'kCLErrorDomain');
end;

{$IF defined(IOS) and NOT defined(CPUARM)}
initialization
  CLModule := dlopen(MarshaledAString(libCoreLocation), RTLD_LAZY);

finalization
  dlclose(CLModule);
{$ENDIF IOS}

end.
