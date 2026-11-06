unit Alcinoe.Macapi.Foundation;

interface

{$I Alcinoe.inc}

uses
  Macapi.ObjectiveC,
  Macapi.CocoaTypes,
  Macapi.Foundation;

{$M+}

Type

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported132}
    {$MESSAGE WARN 'Check if NSJSONReadingOptions/NSJSONWritingOptions are not defined in Macapi.Foundation.pas. If yes, remove the class definition below.'}
  {$ENDIF}
  NSJSONReadingOptions = NSInteger;
  NSJSONWritingOptions = NSInteger;

const

  {***************************************}
  NSJSONReadingMutableContainers = 1 shl 0;
  NSJSONReadingMutableLeaves = 1 shl 1;
  NSJSONReadingFragmentsAllowed = 1 shl 2;
  NSJSONReadingJSON5Allowed = 1 shl 3;
  NSJSONReadingTopLevelDictionaryAssumed = 1 shl 4;
  NSJSONReadingAllowFragments = NSJSONReadingFragmentsAllowed;
  NSJSONWritingPrettyPrinted = 1 shl 0;
  NSJSONWritingSortedKeys = 1 shl 1;
  NSJSONWritingFragmentsAllowed = 1 shl 2;
  NSJSONWritingWithoutEscapingSlashes = 1 shl 3;

Type

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported132}
    {$MESSAGE WARN 'Check if NSJSONSerialization is not defined in Macapi.Foundation.pas. If yes, remove the class definition below.'}
  {$ENDIF}
  NSJSONSerializationClass = interface(NSObjectClass)
    ['{3AF4BF13-1BA3-42A5-B301-7ACBDC8A6B0D}']
    {class} function dataWithJSONObject(obj: Pointer; options: NSJSONWritingOptions; error: PPointer): NSData; cdecl;
    {class} function isValidJSONObject(obj: Pointer): Boolean; cdecl;
    {class} function JSONObjectWithData(data: NSData; options: NSJSONReadingOptions; error: PPointer): Pointer; cdecl;
    {class} function JSONObjectWithStream(stream: NSInputStream; options: NSJSONReadingOptions; error: PPointer): Pointer; cdecl;
    {class} function writeJSONObject(obj: Pointer; toStream: NSOutputStream; options: NSJSONWritingOptions; error: PPointer): NSInteger; cdecl;
  end;
  NSJSONSerialization = interface(NSObject)
    ['{28BD69AA-C5B7-40C9-A8AC-88B9CAEE72C7}']
  end;
  TNSJSONSerialization = class(TOCGenericImport<NSJSONSerializationClass, NSJSONSerialization>) end;

implementation

end.