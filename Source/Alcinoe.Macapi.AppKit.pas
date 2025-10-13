unit Alcinoe.Macapi.AppKit;

interface

{$I Alcinoe.inc}

uses
  Macapi.ObjectiveC,
  Macapi.CocoaTypes,
  Macapi.Foundation,
  Macapi.AppKit;

{$M+}

Type

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported130}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-4353 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  ALNSTextFieldClass = interface(NSTextFieldClass)
    ['{B4708AC0-B11C-47AA-B15A-C9BD161255CC}']
  end;
  ALNSTextField = interface(NSTextField)
    ['{17BF10A7-13BC-484B-87F6-9B71727A032D}']
    function placeholderString: NSString; cdecl;
    function placeholderAttributedString: NSAttributedString; cdecl;
    procedure setPlaceholderAttributedString(placeholderAttributedString: NSAttributedString); cdecl;
  end;
  TALNSTextField = class(TOCGenericImport<ALNSTextFieldClass, ALNSTextField>)  end;

implementation

end.