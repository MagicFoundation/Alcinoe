unit Alcinoe.iOSapi.UIKit;

interface

{$I Alcinoe.inc}

uses
  iOSapi.Foundation,
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.UIKit;

{$M+}

type

  {********************************************}
  ALUITextViewClass = interface(UITextViewClass)
    ['{C448C47F-14A8-4A97-A39F-41481FF68E7C}']
  end;
  ALUITextView = interface(UITextView)
    ['{1B6B1339-95D5-466E-9214-C57C2AE58B4D}']
    {$IFNDEF ALCompilerVersionSupported123}
      {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-860 has been resolved. If resolved, remove the class definition below.'}
    {$ENDIF}
    function textContainer: NSTextContainer; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-860
    procedure setTypingAttributes(typingAttributes: NSDictionary); cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-860
    function typingAttributes: NSDictionary; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-860
    procedure setSelectable(selectable: Boolean); cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-860
    function isSelectable: Boolean; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-860
    {$IFNDEF ALCompilerVersionSupported123}
      {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1941 has been resolved. If resolved, remove the class definition below.'}
    {$ENDIF}
    function layoutManager : NSLayoutManager; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1941
  end;
  TALUITextView = class(TOCGenericImport<ALUITextViewClass, ALUITextView>)  end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1941 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  ALUITextFieldClass = interface(UITextFieldClass)
    ['{E7506B76-2E9E-41C0-8257-F3FF286CCDA9}']
  end;
  ALUITextField = interface(UITextField)
    ['{00D7B422-8D57-428D-BB00-864046D7D298}']
    function attributedPlaceholder: NSAttributedString; cdecl;
  end;
  TUITextField = class(TOCGenericImport<ALUITextFieldClass, ALUITextField>)  end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2993 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  ALUIEventClass = interface(UIEventClass)
    ['{75AEFA97-79E0-4308-A7F9-D08B24F69EE3}']
  end;
  ALUIEvent = interface(UIEvent)
    ['{14730E60-1488-4CA1-953A-01E62BAD1A49}']
    function coalescedTouchesForTouch(touch: UITouch): NSArray; cdecl;
  end;
  TALUIEvent = class(TOCGenericImport<ALUIEventClass, ALUIEvent>)  end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-43458 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  ALUIScreenClass = interface(UIScreenClass)
    ['{9FF3822D-E17B-4F79-B097-26BB1B7182C1}']
  end;
  ALUIScreen = interface(UIScreen)
    ['{A9F1AF20-25FE-4437-9E1B-746A04A94DE2}']
    function maximumFramesPerSecond: NSInteger; cdecl;
  end;
  TALUIScreen = class(TOCGenericImport<ALUIScreenClass, ALUIScreen>)  end;

implementation

end.
