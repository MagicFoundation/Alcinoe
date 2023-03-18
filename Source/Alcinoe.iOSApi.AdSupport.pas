unit Alcinoe.iOSApi.AdSupport;

interface

uses
  Macapi.ObjectiveC,
  iOSapi.Foundation;

{$M+}

type

  {******************************}
  ASIdentifierManager = interface;

  {*****************************************}
  //@interface ASIdentifierManager : NSObject
  ASIdentifierManagerClass = interface(NSObjectClass)
    ['{B646F491-264C-4DBD-8DE6-8C2CC09322E4}']

    //+ (ASIdentifierManager * _Nonnull)sharedManager;
    {class} function sharedManager : ASIdentifierManager; cdecl;

  end;
  ASIdentifierManager = interface(NSObject)
    ['{FCE2CAED-3712-485A-8CDA-FEDE193E0E2C}']

    //- (void)clearAdvertisingIdentifier API_UNAVAILABLE(ios, tvos);
    procedure clearAdvertisingIdentifier; cdecl;

    //@property (nonnull, nonatomic, readonly) NSUUID *advertisingIdentifier;
    function advertisingIdentifier : NSUUID; cdecl;

    //@property (nonatomic, readonly, getter=isAdvertisingTrackingEnabled) BOOL advertisingTrackingEnabled;
    function isAdvertisingTrackingEnabled : Boolean; cdecl;

  end;
  TASIdentifierManager = class(TOCGenericImport<ASIdentifierManagerClass, ASIdentifierManager>) end;
  PASIdentifierManager = Pointer;

const
   libAdSupport = '/System/Library/Frameworks/AdSupport.framework/AdSupport';

implementation

{****************************************************************************************}
procedure StubProc1; cdecl; external libAdSupport name 'OBJC_CLASS_$_ASIdentifierManager';

end.
