unit Alcinoe.iOSapi.UIKit;

interface

{$I Alcinoe.inc}

uses
  iOSapi.CocoaTypes,
  iOSapi.Foundation,
  Macapi.ObjectiveC,
  iOSapi.UIKit;

{$M+}

//const

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-44208 has been resolved. If resolved, remove the definitions below.'}
  {$ENDIF}
  //UIDisplayGamutUnspecified = -1; // https://quality.embarcadero.com/browse/RSP-44208
  //{$EXTERNALSYM UIDisplayGamutUnspecified}
  //UIDisplayGamutSRGB = 0; // https://quality.embarcadero.com/browse/RSP-44208
  //{$EXTERNALSYM UIDisplayGamutSRGB}
  //UIDisplayGamutP3 = 1; // https://quality.embarcadero.com/browse/RSP-44208
  //{$EXTERNALSYM UIDisplayGamutP3}

type

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-44208 has been resolved. If resolved, remove the definitions below.'}
  {$ENDIF}
  //UIDisplayGamut = NSInteger; // https://quality.embarcadero.com/browse/RSP-44208

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-44208 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  //ALUITraitCollectionClass = interface(UITraitCollectionClass)
  //  ['{865B9D11-3D3B-4E69-8B44-65C21AB34C35}']
  //end;
  //ALUITraitCollection = interface(UITraitCollection)
  //  ['{4786862F-88CB-4CCA-BE86-EC8273D04960}']
  //  function displayGamut: UIDisplayGamut; cdecl; // https://quality.embarcadero.com/browse/RSP-44208
  //end;
  //TALUITraitCollection = class(TOCGenericImport<ALUITraitCollectionClass, ALUITraitCollection>) end;

  {********************************************}
  ALUITextViewClass = interface(UITextViewClass)
    ['{C448C47F-14A8-4A97-A39F-41481FF68E7C}']
  end;
  ALUITextView = interface(UITextView)
    ['{1B6B1339-95D5-466E-9214-C57C2AE58B4D}']
    {$IFNDEF ALCompilerVersionSupported122}
      {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-860 has been resolved. If resolved, remove the class definition below.'}
    {$ENDIF}
    function textContainer: NSTextContainer; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-860
    procedure setTypingAttributes(typingAttributes: NSDictionary); cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-860
    function typingAttributes: NSDictionary; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-860
    procedure setSelectable(selectable: Boolean); cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-860
    function isSelectable: Boolean; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-860
    {$IFNDEF ALCompilerVersionSupported122}
      {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1941 has been resolved. If resolved, remove the class definition below.'}
    {$ENDIF}
    function layoutManager : NSLayoutManager; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1941
  end;
  TALUITextView = class(TOCGenericImport<ALUITextViewClass, ALUITextView>)  end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1941 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  ALUITextFieldClass = interface(UITextFieldClass)
    ['{E7506B76-2E9E-41C0-8257-F3FF286CCDA9}']
  end;
  ALUITextField = interface(UITextField)
    ['{00D7B422-8D57-428D-BB00-864046D7D298}']
    function attributedPlaceholder: NSAttributedString; cdecl; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1941
  end;
  TUITextField = class(TOCGenericImport<ALUITextFieldClass, ALUITextField>)  end;

implementation

end.
