unit Alcinoe.Macapi.Foundation;

interface

{$I Alcinoe.inc}

uses
  Macapi.ObjectiveC,
  Macapi.Foundation;

{$M+}

type

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1935 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  ALNSAttributedStringClass = interface(NSAttributedStringClass)
    ['{D0F9606E-782D-48C5-84F4-3D2629867AEC}']
  end;
  ALNSAttributedString = interface(NSAttributedString)
    ['{482108A1-EE62-4B48-9BDA-036583F5C17D}']
    function &string : NSString; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1935
  end;
  TALNSAttributedString = class(TOCGenericImport<ALNSAttributedStringClass, ALNSAttributedString>)  end;

implementation

end.
