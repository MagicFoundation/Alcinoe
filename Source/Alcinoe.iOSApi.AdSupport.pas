unit Alcinoe.iOSApi.AdSupport;

interface

{$I Alcinoe.inc}

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
    {class} function sharedManager : ASIdentifierManager; cdecl;
  end;
  ASIdentifierManager = interface(NSObject)
    ['{FCE2CAED-3712-485A-8CDA-FEDE193E0E2C}']
    procedure clearAdvertisingIdentifier; cdecl;
    function advertisingIdentifier : NSUUID; cdecl;
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