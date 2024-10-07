unit Alcinoe.Androidapi.JNI.App;

interface

{$I Alcinoe.inc}

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.Media,
  Androidapi.JNI.App,
  Androidapi.JNI.Net;

type

  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-39511 has been resolved. If resolved, remove the class definition below.'}
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

  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-39511 has been resolved. If resolved, remove the class definition below.'}
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

end.
