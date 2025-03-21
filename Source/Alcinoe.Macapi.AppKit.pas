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
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  NSControlTextEditingDelegate = interface(IObjectiveC)
    ['{54716EE1-A8FC-4452-9A2F-8831DBE453F9}']
    procedure controlTextDidBeginEditing(obj: NSNotification); cdecl;
    procedure controlTextDidEndEditing(obj: NSNotification); cdecl;
    procedure controlTextDidChange(obj: NSNotification); cdecl;
    [MethodName('control:textShouldBeginEditing:')]
    function controlTextShouldBeginEditing(control: NSControl; textShouldBeginEditing: NSText): Boolean; cdecl;
    [MethodName('control:textShouldEndEditing:')]
    function controlTextShouldEndEditing(control: NSControl; textShouldEndEditing: NSText): Boolean; cdecl;
    [MethodName('control:textView:doCommandBySelector:')]
    function controlTextViewDoCommandBySelector(control: NSControl; textView: NSTextView; doCommandBySelector: SEL): Boolean; cdecl;
  end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  NSTextDelegate = interface(IObjectiveC)
    ['{C86B4A40-41A6-420D-AA5F-5DA4833B4698}']
    function textShouldBeginEditing(textObject: NSText): Boolean; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933
    function textShouldEndEditing(textObject: NSText): Boolean; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933
    procedure textDidBeginEditing(notification: NSNotification); cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933
    procedure textDidEndEditing(notification: NSNotification); cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933
    procedure textDidChange(notification: NSNotification); cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933
  end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  //NSTextFieldDelegate = interface(NSControlTextEditingDelegate) // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933
  //  ['{334420BA-E3C2-4654-B5BD-BBECC0FB879D}']
  //end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  NSTextViewDelegate = interface(NSTextDelegate) // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933
    ['{FC194BA9-B397-4431-8506-3840CE4CAF09}']
    [MethodName('textView:shouldChangeTextInRange:replacementString:')] // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933
    function textViewShouldChangeTextInRangeReplacementString(textView: NSTextView; shouldChangeTextInRange: NSRange; replacementString: NSString): boolean; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933
  end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  ALNSTextClass = interface(NSTextClass)
    ['{92A499D1-AA6C-4FBF-81C4-7627AC66BEB1}']
  end;
  ALNSText = interface(NSText)
    ['{43755DC0-462B-45EB-BA06-1DE8FFE48A3E}']
    function &string : NSString; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933
  end;
  TALNSText = class(TOCGenericImport<ALNSTextClass, ALNSText>)  end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  ALNSControlClass = interface(NSControlClass)
    ['{E95BA47F-25A7-47EE-831E-C5A7BC9F3416}']
  end;
  ALNSControl = interface(NSControl)
    ['{A17C8CD8-6DA7-4F3D-86D8-8F2CA265AF85}']
    procedure setLineBreakMode(lineBreakMode: NSLineBreakMode); cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933
    function lineBreakMode : NSLineBreakMode; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933
  end;
  TALNSControl = class(TOCGenericImport<ALNSControlClass, ALNSControl>)  end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  ALNSTextViewClass = interface(NSTextViewClass)
    ['{FC405A98-623C-43C2-8DD9-7557ADEEE4DC}']
  end;
  ALNSTextView = interface(NSTextView)
    ['{05816560-6877-4EDE-ADA9-EA0C35A269DF}']
    procedure setDelegate(anObject: Pointer); cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933
  end;
  TALNSTextView = class(TOCGenericImport<ALNSTextViewClass, ALNSTextView>)  end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  ALNSTextFieldClass = interface(NSTextFieldClass)
    ['{B4708AC0-B11C-47AA-B15A-C9BD161255CC}']
  end;
  ALNSTextField = interface(NSTextField)
    ['{17BF10A7-13BC-484B-87F6-9B71727A032D}']
    function delegate: Pointer; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933
    procedure setDelegate(delegate: pointer); cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933
    function placeholderString: NSString; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933
    function placeholderAttributedString: NSAttributedString; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933
    procedure setPlaceholderAttributedString(placeholderAttributedString: NSAttributedString); cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1933
  end;
  TALNSTextField = class(TOCGenericImport<ALNSTextFieldClass, ALNSTextField>)  end;

implementation

end.
