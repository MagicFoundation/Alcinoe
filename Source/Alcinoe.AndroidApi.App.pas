unit Alcinoe.AndroidApi.App;

interface

{$I Alcinoe.inc}

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.Media,
  Androidapi.JNI.App,
  Androidapi.JNI.Net,
  Androidapi.JNI.JavaTypes;

type

  {*************************}
  JUiModeManager = interface;
  JALNotification = interface;
  JALNotificationChannel = interface;

  {*******************************************}
  JUiModeManagerClass = interface(JObjectClass)
    ['{FD71340D-4C9D-4078-A322-218A2DAAEF69}']
    {class} function _GetMODE_NIGHT_AUTO: Integer; cdecl;
    {class} function _GetMODE_NIGHT_CUSTOM: Integer; cdecl;
    {class} function _GetMODE_NIGHT_NO: Integer; cdecl;
    {class} function _GetMODE_NIGHT_YES: Integer; cdecl;
    {class} property MODE_NIGHT_AUTO: Integer read _GetMODE_NIGHT_AUTO;
    {class} property MODE_NIGHT_CUSTOM: Integer read _GetMODE_NIGHT_CUSTOM;
    {class} property MODE_NIGHT_NO: Integer read _GetMODE_NIGHT_NO;
    {class} property MODE_NIGHT_YES: Integer read _GetMODE_NIGHT_YES;
  end;
  [JavaSignature('android/app/UiModeManager')]
  JUiModeManager = interface(JObject)
    ['{40B9632D-FC87-41D9-866A-4D0E60F9BEDF}']
    function getNightMode: Integer; cdecl;
    procedure setApplicationNightMode(mode: integer); cdecl;
  end;
  TJUiModeManager = class(TJavaGenericImport<JUiModeManagerClass, JUiModeManager>) end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-21296 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  JALNotificationClass = interface(JNotificationClass)
    ['{D0C93C68-19D5-4CB6-997B-19EC96149450}']
    {class} function _GetAUDIO_ATTRIBUTES_DEFAULT: JAudioAttributes; cdecl; // https://quality.embarcadero.com/browse/RSP-39511
    {class} property AUDIO_ATTRIBUTES_DEFAULT: JAudioAttributes read _GetAUDIO_ATTRIBUTES_DEFAULT; // https://quality.embarcadero.com/browse/RSP-39511
  end;
  [JavaSignature('android/app/Notification')]
  JALNotification = interface(JNotification)
    ['{593CF14D-A225-437E-B9D6-43D3BDAB57AA}']
  end;
  TJALNotification = class(TJavaGenericImport<JALNotificationClass, JALNotification>) end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-21296 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  JALNotificationChannelClass = interface(JNotificationChannelClass)
    ['{118B9745-6C2E-466E-B0FC-CF86F896259B}']
  end;
  [JavaSignature('android/app/NotificationChannel')]
  JALNotificationChannel = interface(JNotificationChannel)
    ['{5338DB86-8DE1-41ED-9455-1234D9274A5C}']
    procedure setSound(sound: Jnet_Uri; audioAttributes: JAudioAttributes); cdecl; // https://quality.embarcadero.com/browse/RSP-39511
  end;
  TJALNotificationChannel = class(TJavaGenericImport<JALNotificationChannelClass, JALNotificationChannel>) end;

implementation

uses
  Alcinoe.Common;

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.App.JUiModeManager', TypeInfo(Alcinoe.AndroidApi.App.JUiModeManager));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.App.JALNotification', TypeInfo(Alcinoe.AndroidApi.App.JALNotification));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.App.JALNotificationChannel', TypeInfo(Alcinoe.AndroidApi.App.JALNotificationChannel));
end;

initialization
  {$IF defined(UiModeManager)}
  ALLog('Alcinoe.AndroidApi.App','initialization');
  {$ENDIF}
  RegisterTypes;

end.