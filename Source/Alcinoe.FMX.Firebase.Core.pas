unit Alcinoe.FMX.Firebase.Core;

interface

{$I Alcinoe.inc}

{$IF defined(ios)}
procedure ALFIRAppConfigure;
{$ENDIF}

implementation

uses
  system.Messaging,
  system.Classes,
  system.SysUtils,
  Fmx.Platform,
  {$IF defined(ios)}
  Alcinoe.iOSApi.FirebaseCore,
  {$ENDIF}
  Alcinoe.Common,
  Alcinoe.StringUtils;

{****************}
{$IF defined(ios)}
var
  ALIsFIRAppConfigured: Boolean;
{$ENDIF}

{****************}
{$IF defined(ios)}
procedure ALFIRAppConfigure;
begin
  if not ALIsFIRAppConfigured then TFIRApp.OCClass.configure;
  ALIsFIRAppConfigured := True;
end;
{$ENDIF}

{*******************************************************************************************}
procedure ALFmxFirebaseCoreApplicationEventHandler(const Sender: TObject; const M: TMessage);
begin

  {$REGION ' IOS'}
  {$IF defined(ios)}
  if (M is TApplicationEventMessage) and
     (TApplicationEventMessage(M).Value.Event = TApplicationEvent.FinishedLaunching) then begin

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
      'ALFmxFirebaseCoreApplicationEventHandler',
      'FinishedLaunching');
    {$ENDIF}
    ALFIRAppConfigure;

  end;
  {$ENDIF}
  {$ENDREGION}

end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.Firebase.Core','initialization');
  {$ENDIF}
  {$IF defined(ios)}
  ALIsFIRAppConfigured := False;
  {$ENDIF}
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ALFmxFirebaseCoreApplicationEventHandler);

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.Firebase.Core','finalization');
  {$ENDIF}
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ALFmxFirebaseCoreApplicationEventHandler);

end.