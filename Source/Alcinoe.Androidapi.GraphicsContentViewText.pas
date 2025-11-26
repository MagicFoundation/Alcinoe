unit Alcinoe.Androidapi.GraphicsContentViewText;

interface

{$I Alcinoe.inc}

uses
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Hardware,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge;

type

  {***************************************}
  JALBroadcastReceiverListener = interface;
  JALBroadcastReceiver = interface;
  JSoundEffectConstants = interface;
  JALBitmap = interface;

  {*******************************************************}
  JALBroadcastReceiverListenerClass = interface(IJavaClass)
    ['{C4B00A4E-9785-45DD-B05A-8553501C51F5}']
  end;
  [JavaSignature('io/magicfoundation/alcinoe/broadcastreceiver/ALBroadcastReceiverListener')]
  JALBroadcastReceiverListener = interface(IJavaInstance)
    ['{4461603A-5503-4B0A-A8B0-227AD9481BBD}']
    procedure onReceive(context: JContext; intent: JIntent); cdecl;
  end;
  TJALBroadcastReceiverListener = class(TJavaGenericImport<JALBroadcastReceiverListenerClass, JALBroadcastReceiverListener>) end;

  {************************************************************}
  JALBroadcastReceiverClass = interface(JBroadcastReceiverClass)
    ['{AECB6BD7-3500-4627-A217-F9CBFBAB68D5}']
    {class} procedure setListener(listener: JALBroadcastReceiverListener); cdecl;
    {class} procedure deliverPendingBroadcasts(context: JContext); cdecl;
  end;
  [JavaSignature('io/magicfoundation/alcinoe/broadcastreceiver/ALBroadcastReceiver')]
  JALBroadcastReceiver = interface(JBroadcastReceiver)
    ['{BD2257E5-250E-4A74-9E6D-C2CA7A6536DC}']
  end;
  TJALBroadcastReceiver = class(TJavaGenericImport<JALBroadcastReceiverClass, JALBroadcastReceiver>) end;

  {**************************************************}
  JSoundEffectConstantsClass = interface(JObjectClass)
    ['{B19777B1-340A-4288-BC9A-4D74CF831E25}']
    {class} function _GetCLICK: Integer; cdecl;
    {class} function _GetNAVIGATION_LEFT: Integer; cdecl;
    {class} function _GetNAVIGATION_UP: Integer; cdecl;
    {class} function _GetNAVIGATION_RIGHT: Integer; cdecl;
    {class} function _GetNAVIGATION_DOWN: Integer; cdecl;
    {class} function _GetNAVIGATION_REPEAT_LEFT: Integer; cdecl;
    {class} function _GetNAVIGATION_REPEAT_UP: Integer; cdecl;
    {class} function _GetNAVIGATION_REPEAT_RIGHT: Integer; cdecl;
    {class} function _GetNAVIGATION_REPEAT_DOWN: Integer; cdecl;
    {class} property CLICK: Integer read _GetCLICK;
    {class} property NAVIGATION_LEFT: Integer read _GetNAVIGATION_LEFT;
    {class} property NAVIGATION_UP: Integer read _GetNAVIGATION_UP;
    {class} property NAVIGATION_RIGHT: Integer read _GetNAVIGATION_RIGHT;
    {class} property NAVIGATION_DOWN: Integer read _GetNAVIGATION_DOWN;
    {class} property NAVIGATION_REPEAT_LEFT: Integer read _GetNAVIGATION_REPEAT_LEFT;
    {class} property NAVIGATION_REPEAT_UP: Integer read _GetNAVIGATION_REPEAT_UP;
    {class} property NAVIGATION_REPEAT_RIGHT: Integer read _GetNAVIGATION_REPEAT_RIGHT;
    {class} property NAVIGATION_REPEAT_DOWN: Integer read _GetNAVIGATION_REPEAT_DOWN;
  end;
  [JavaSignature('android/view/SoundEffectConstants')]
  JSoundEffectConstants = interface(JObject)
    ['{584AACAC-7FE3-49D4-A748-069F28B515CB}']
  end;
  TJSoundEffectConstants = class(TJavaGenericImport<JSoundEffectConstantsClass, JSoundEffectConstants>) end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported130}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-44100 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  JALBitmapClass = interface(JBitmapClass)
    ['{3CB7580B-2DE1-4F79-A5EC-18BAD23884E4}']
    {class} function wrapHardwareBuffer(hardwareBuffer: JHardwareBuffer; colorSpace: JColorSpace): JBitmap; cdecl; //https://quality.embarcadero.com/browse/RSP-44100
  end;
  [JavaSignature('android/graphics/Bitmap')]
  JALBitmap = interface(JBitmap)
    ['{FF2C1C70-F5E1-44AC-A8C6-4326BAA4440B}']
    function getHardwareBuffer: JHardwareBuffer; cdecl;
  end;
  TJALBitmap = class(TJavaGenericImport<JALBitmapClass, JALBitmap>) end; //https://quality.embarcadero.com/browse/RSP-44100

implementation

uses
  Alcinoe.Common;

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.GraphicsContentViewText.JALBroadcastReceiverListener', TypeInfo(Alcinoe.AndroidApi.GraphicsContentViewText.JALBroadcastReceiverListener));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.GraphicsContentViewText.JALBroadcastReceiver', TypeInfo(Alcinoe.AndroidApi.GraphicsContentViewText.JALBroadcastReceiver));
  TRegTypes.RegisterType('Alcinoe.Androidapi.GraphicsContentViewText.JSoundEffectConstants', TypeInfo(Alcinoe.Androidapi.GraphicsContentViewText.JSoundEffectConstants));
  TRegTypes.RegisterType('Alcinoe.Androidapi.GraphicsContentViewText.JALBitmap', TypeInfo(Alcinoe.Androidapi.GraphicsContentViewText.JALBitmap));
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.Androidapi.GraphicsContentViewText','initialization');
  {$ENDIF}
  RegisterTypes;

end.