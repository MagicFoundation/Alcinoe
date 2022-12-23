unit ALFmxFirebaseCommon;

interface

implementation

uses
  system.Messaging,
  system.Classes,
  system.SysUtils,
  Fmx.Platform,
  {$IF defined(ios)}
  ALIosFirebaseCommonApi,
  {$ENDIF}
  ALCommon,
  ALString;


{*********************************************************************************************}
procedure ALFmxFirebaseCommonApplicationEventHandler(const Sender: TObject; const M: TMessage);
begin

  {$REGION ' IOS'}
  {$IF defined(ios)}
  if (M is TApplicationEventMessage) then begin
    if ((M as TApplicationEventMessage).Value.Event = TApplicationEvent.FinishedLaunching) then begin

      // Initialize Firebase in your app
      // Configure a FIRApp shared instance, typically in your application's
      // application:didFinishLaunchingWithOptions: method:
      // This is the normal flow:
      // 1) Unit initialization
      // 2) didFinishLaunchingWithOptions
      // 3) main form create
      // 4) BecameActive event received
      {$IF defined(debug)}
      allog(
        'ALFmxFirebaseCommon._ApplicationEventHandler',
        'FinishedLaunching',
        TalLogType.VERBOSE);
      {$ENDIF}
      TFIRApp.OCClass.configure;

    end
  end;
  {$ENDIF}
  {$ENDREGION}

end;

initialization
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ALFmxFirebaseCommonApplicationEventHandler);

finalization
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ALFmxFirebaseCommonApplicationEventHandler);

end.
