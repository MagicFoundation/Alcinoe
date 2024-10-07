unit Alcinoe.iOSapi.Foundation;

interface

{$I Alcinoe.inc}

uses
  iOSapi.Foundation,
  Macapi.ObjectiveC;

{$M+}

type

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1940 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  ALNSAttributedStringClass = interface(NSAttributedStringClass)
    ['{08662114-6FAA-409B-8967-36CF7412CDE8}']
  end;
  ALNSAttributedString = interface(NSAttributedString)
    ['{FBA1AC4C-FA36-4A85-86DA-7811EBADC0A1}']
    function &string : NSString; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1940
  end;
  TALNSAttributedString = class(TOCGenericImport<ALNSAttributedStringClass, ALNSAttributedString>)  end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-28096 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  NSPersonNameComponentsClass = interface(NSObjectClass)
    ['{EC3A2FD0-96BA-4D9D-B8B9-1B958065A4B1}']
  end;
  NSPersonNameComponents = interface(NSObject)
    ['{0CAA6C3F-96B7-4D89-BC2C-95427D20603B}']
    procedure setNamePrefix(namePrefix: NSString); cdecl;
    function namePrefix : NSString; cdecl;
    procedure setGivenName(givenName: NSString); cdecl;
    function givenName : NSString; cdecl;
    procedure setMiddleName(middleName: NSString); cdecl;
    function middleName : NSString; cdecl;
    procedure setFamilyName(familyName: NSString); cdecl;
    function familyName : NSString; cdecl;
    procedure setNameSuffix(nameSuffix: NSString); cdecl;
    function nameSuffix : NSString; cdecl;
    procedure setNickname(nickname: NSString); cdecl;
    function nickname : NSString; cdecl;
    procedure setPhoneticRepresentation(phoneticRepresentation: NSPersonNameComponents); cdecl;
    function phoneticRepresentation : NSPersonNameComponents; cdecl;
  end;

implementation

end.
