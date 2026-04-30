unit Alcinoe.Androidapi.Window;

interface

{$I Alcinoe.inc}

uses
  Androidapi.JNIBridge;

type

  {*********************************}
  JOnBackInvokedCallback = interface;
  JOnBackInvokedDispatcher = interface;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported131}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-5362 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  JOnBackInvokedCallbackClass = interface(IJavaClass)
    ['{0D2D4826-836C-4D83-AEF9-5DD6F48686E5}']
  end;
  [JavaSignature('android/window/OnBackInvokedCallback')]
  JOnBackInvokedCallback = interface(IJavaInstance)
    ['{E456F171-47B3-476F-86C1-C3FEB52D5002}']
    procedure onBackInvoked; cdecl;
  end;
  TJOnBackInvokedCallback = class(TJavaGenericImport<JOnBackInvokedCallbackClass, JOnBackInvokedCallback>) end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported131}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-5362 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  JOnBackInvokedDispatcherClass = interface(IJavaClass)
    ['{1428BEE0-DA87-4179-BA57-2D34399594EE}']
    {class} function _GetPRIORITY_DEFAULT: Integer; cdecl;
    {class} function _GetPRIORITY_OVERLAY: Integer; cdecl;
    {class} property PRIORITY_DEFAULT: Integer read _GetPRIORITY_DEFAULT;
    {class} property PRIORITY_OVERLAY: Integer read _GetPRIORITY_OVERLAY;
  end;
  [JavaSignature('android/window/OnBackInvokedDispatcher')]
  JOnBackInvokedDispatcher = interface(IJavaInstance)
    ['{45E4D5E8-47AB-4435-A0B1-42576E211434}']
    procedure registerOnBackInvokedCallback(priority: Integer; callback: JOnBackInvokedCallback); cdecl;
    procedure unregisterOnBackInvokedCallback(callback: JOnBackInvokedCallback); cdecl;
  end;
  TJOnBackInvokedDispatcher = class(TJavaGenericImport<JOnBackInvokedDispatcherClass, JOnBackInvokedDispatcher>) end;

implementation

uses
  Alcinoe.Common;

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.Androidapi.Window.JOnBackInvokedCallbackClass', TypeInfo(Alcinoe.AndroidApi.Window.JOnBackInvokedCallbackClass));
  TRegTypes.RegisterType('Alcinoe.Androidapi.Window.JOnBackInvokedDispatcher', TypeInfo(Alcinoe.AndroidApi.Window.JOnBackInvokedDispatcher));
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.Androidapi.Window','initialization');
  {$ENDIF}
  RegisterTypes;

end.