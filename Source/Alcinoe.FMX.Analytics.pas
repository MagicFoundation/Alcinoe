unit Alcinoe.FMX.Analytics;

{$I Alcinoe.inc}

//
//  Analytics providers are disabled by default.
//  To enable a provider, add the corresponding conditional symbol in:
//    Project > Options > Delphi Compiler > Conditional defines
//
//  Available symbols:
//    ALAnalyticsFirebase: Enables analytics through Firebase Analytics.
//    ALAnalyticsFacebook: Enables analytics through Facebook App Events.
//
//  Make sure the symbols are added to every required build configuration
//  and target platform, such as Debug/Release and Android/iOS.
//

{.$DEFINE ALAnalyticsFirebase}
{.$DEFINE ALAnalyticsFacebook}

interface

uses
  {$IF defined(android)}
    {$IF defined(ALAnalyticsFirebase)}
    Alcinoe.AndroidApi.Firebase.Analytics,
    {$ENDIF}
    {$IF defined(ALAnalyticsFacebook)}
    Alcinoe.AndroidApi.Facebook,
    {$ENDIF}
  {$ENDIF}
  {$IF defined(ios)}
    {$IF defined(ALAnalyticsFacebook)}
    System.Messaging,
    System.Generics.Collections,
    {$ENDIF}
  {$ENDIF}
  System.Net.URLClient;

type

  {***************************}
  TALAnalytics = class(Tobject)
  private
    class function CreateInstance: TALAnalytics;
    class function GetInstance: TALAnalytics; static;
  protected
    class var FInstance: TALAnalytics;
  public
    type
      TCreateInstanceFunc = function: TALAnalytics;
    class var CreateInstanceFunc: TCreateInstanceFunc;
    class property Instance: TALAnalytics read GetInstance;
    class function HasInstance: Boolean; inline;
  private

    {$REGION 'android'}
    {$IF defined(android)}
      {$IF defined(ALAnalyticsFirebase)}
      fFirebaseAnalytics: JFirebaseAnalytics;
      {$ENDIF}
      {$IF defined(ALAnalyticsFacebook)}
      fFacebookAppEventsLogger: JAppEventsLogger;
      {$ENDIF}
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'IOS'}
    {$IF defined(IOS)}
      {$IF defined(ALAnalyticsFacebook)}
      private
        type
          TPendingOperationKind = (TrackEvent, TrackEventWithValues, SetUserID, ClearUserID);
          TPendingOperation = record
            Kind: TPendingOperationKind;
            EventName: String;
            EventValues: TNameValueArray;
            UserID: Int64;
          end;
      private
        FPendingOperations: TQueue<TPendingOperation>;
        FFacebookAppActivated: Boolean;
        procedure ApplicationEventHandler(const Sender: TObject; const M: TMessage);
      {$ENDIF}
    {$ENDIF}
    {$ENDREGION}

  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure trackEvent(const AEventName: String); overload; virtual;
    procedure trackEvent(const AEventName: String; const AEventValues: TNameValueArray); overload; virtual;
    procedure TrackScreenView(const AScreenName: String; const AScreenClass: String); virtual;
    procedure SetUserID(const AUserID: Int64); virtual;
    procedure ClearUserID; virtual;
    procedure SetUserProperty(const APropertyName: String; const APropertyValue: String); virtual;
    procedure ClearUserProperty(const APropertyName: String); virtual;
  end;

implementation

uses
  System.SysUtils,
  {$IF defined(android)}
    Androidapi.JNI.App,
    Androidapi.JNI.JavaTypes,
    Androidapi.JNI.Os,
    Androidapi.Helpers,
  {$ENDIF}
  {$IF defined(ios)}
    Macapi.Helpers,
    Macapi.ObjectiveC,
    iOSapi.Foundation,
    // https://firebase.google.com/support/faq#analytics-adsupport-framework
    // Some Analytics features, such as audiences and campaign attribution,
    // and some user properties, such as age and interests, require the AdSupport
    // framework to be enabled. Without this framework, Analytics cannot collect
    // information needed for these features to function properly.
    Alcinoe.iOSApi.AdSupport, // UsesCleaner:keep
    Alcinoe.FMX.Firebase.Core,
    {$IF defined(ALAnalyticsFirebase)}
    Alcinoe.iOSApi.FirebaseAnalytics,
    {$ENDIF}
    {$IF defined(ALAnalyticsFacebook)}
    FMX.Platform,
    Alcinoe.iOSApi.FacebookCoreKit,
    {$ENDIF}
  {$ENDIF}
  {$IF defined(ALAnalyticsFacebook)}
  Alcinoe.FMX.Facebook.Core,
  {$ENDIF}
  Alcinoe.StringUtils,
  Alcinoe.Common;

{******************************}
constructor TALAnalytics.Create;
begin

  inherited;

  {$REGION 'android'}
  {$IF defined(android)}

    {$IF defined(ALAnalyticsFirebase)}
    fFirebaseAnalytics := TJFirebaseAnalytics.JavaClass.getInstance(TAndroidHelper.Context.getApplicationContext);
    {$ENDIF}

    {$IF defined(ALAnalyticsFacebook)}
    ALInitFacebook;
    TJAppEventsLogger.JavaClass.activateApp(TAndroidHelper.Activity.getApplication);
    fFacebookAppEventsLogger := TJAppEventsLogger.JavaClass.newLogger(TAndroidHelper.Context.getApplicationContext);
    {$ENDIF}

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'ios'}
  {$IF defined(ios)}

    {$IF defined(ALAnalyticsFirebase)}
    ALFIRAppConfigure;
    {$ENDIF}

    {$IF defined(ALAnalyticsFacebook)}
    if not ALInitFacebookSDKAtStartup then ALInitFacebook;
    FPendingOperations := TQueue<TPendingOperation>.Create;
    FFacebookAppActivated := False;
    TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventHandler);
    {$ENDIF}

  {$ENDIF}
  {$ENDREGION}

end;

{******************************}
destructor TALAnalytics.Destroy;
begin

  {$REGION 'ios'}
  {$IF defined(ios)}

    {$IF defined(ALAnalyticsFacebook)}
    TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventHandler);
    ALFreeAndNil(FPendingOperations);
    {$ENDIF}

  {$ENDIF}
  {$ENDREGION}

  inherited;

end;

{*******************************************************}
class function TALAnalytics.CreateInstance: TALAnalytics;
begin
  result := TALAnalytics.Create;
end;

{*************}
//[MultiThread]
class function TALAnalytics.GetInstance: TALAnalytics;
begin
  if FInstance = nil then begin
    var LInstance := CreateInstanceFunc;
    if AtomicCmpExchange(Pointer(FInstance), Pointer(LInstance), nil) <> nil then ALFreeAndNil(LInstance)
  end;
  Result := FInstance;
end;

{*************}
//[MultiThread]
class function TALAnalytics.HasInstance: Boolean;
begin
  result := FInstance <> nil;
end;

{*************************************************}
{$IF defined(IOS) and defined(ALAnalyticsFacebook)}
procedure TALAnalytics.ApplicationEventHandler(const Sender: TObject; const M: TMessage);
begin
  if (M is TApplicationEventMessage) and
     (TApplicationEventMessage(M).value.Event = TApplicationEvent.BecameActive) then begin
    {$IFDEF DEBUG}
    ALLog(
      'TALAnalytics.ApplicationEventHandler',
      'Event: BecameActive');
    {$ENDIF}
    ALInitFacebook;
    TFBSDKAppEvents.OCClass.shared.activateApp;
    if not FFacebookAppActivated then begin
      TMonitor.Enter(FPendingOperations);
      try
        while FPendingOperations.Count > 0 do begin
          var LPendingOperation := FPendingOperations.Dequeue;
          case LPendingOperation.Kind of
            TPendingOperationKind.TrackEvent: TFBSDKAppEvents.OCClass.shared.logEvent(StrToNsStr(LPendingOperation.EventName));
            TPendingOperationKind.TrackEventWithValues: begin
              var LParameters := TNSMutableDictionary.Create;
              try
                for var I := Low(LPendingOperation.EventValues) to High(LPendingOperation.EventValues) do
                  LParameters.setObject(StringToID(LPendingOperation.EventValues[i].value), StringToID(LPendingOperation.EventValues[i].name));
                TFBSDKAppEvents.OCClass.shared.logEvent(StrToNsStr(LPendingOperation.EventName), LParameters);
              finally
                LParameters.release;
              end;
            end;
            TPendingOperationKind.SetUserID: TFBSDKAppEvents.OCClass.shared.setUserID(strToNSStr(ALIntToStrW(LPendingOperation.UserID)));
            TPendingOperationKind.ClearUserID: TFBSDKAppEvents.OCClass.shared.setUserID(nil);
            else Raise Exception.Create('Error 85D28DB9-3C84-411F-95C3-67D04F856A4B')
          end;
        end;
        FFacebookAppActivated := True;
      finally
        TMonitor.Exit(FPendingOperations);
      end;
    end;
  end;
end;
{$ENDIF}

{**********************************************************}
procedure TALAnalytics.trackEvent(const AEventName: String);
begin

  {$IFDEF DEBUG}
  ALLog(Classname+'.trackEvent', 'EventName: ' + AEventName);
  if length(AEventName) > 40 then raise Exception.CreateFmt('Invalid analytics event name "%s": maximum length is 40 characters, but it is %d characters long.', [AEventName, Length(AEventName)]);
  {$ENDIF}

  {$REGION 'android'}
  {$IF defined(android)}

    {$IF defined(ALAnalyticsFirebase)}
    fFirebaseAnalytics.logEvent(StringToJString(aEventName), nil);
    {$ENDIF}

    {$IF defined(ALAnalyticsFacebook)}
    fFacebookAppEventsLogger.logEvent(StringToJString(AEventName));
    {$ENDIF}

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'ios'}
  {$IF defined(ios)}

    {$IF defined(ALAnalyticsFirebase)}
    TFIRAnalytics.OCClass.logEventWithName(StrToNsStr(aEventName), nil);
    {$ENDIF}

    {$IF defined(ALAnalyticsFacebook)}
    if not FFacebookAppActivated then begin
      TMonitor.Enter(FPendingOperations);
      try
        if FFacebookAppActivated then TFBSDKAppEvents.OCClass.shared.logEvent(StrToNsStr(AEventName))
        else begin
          var LPendingOperation: TPendingOperation;
          LPendingOperation.Kind := TPendingOperationKind.TrackEvent;
          LPendingOperation.EventName := AEventName;
          LPendingOperation.EventValues := nil;
          LPendingOperation.UserID := 0;
          FPendingOperations.Enqueue(LPendingOperation);
        end;
      finally
        TMonitor.Exit(FPendingOperations);
      end;
    end
    else TFBSDKAppEvents.OCClass.shared.logEvent(StrToNsStr(AEventName));
    {$ENDIF}

  {$ENDIF}
  {$ENDREGION}

end;

{***********************************************************************************************}
procedure TALAnalytics.trackEvent(const AEventName: String; const AEventValues: TNameValueArray);
begin

  if length(AEventValues) = 0 then begin
    trackEvent(AEventName);
    exit;
  end;

  {$IFDEF DEBUG}
  var LEventValues: String := '';
  For var I := Low(AEventValues) to High(AEventValues) do
    LEventValues := LEventValues + ALIfThenW(LEventValues <> '', ',') + AEventValues[I].Name + '=' + AEventValues[I].Value;
  ALLog(Classname+'.trackEvent', 'EventName: ' + AEventName + ' | EventValues: ['+ LEventValues + ']');
  if length(AEventName) > 40 then raise Exception.CreateFmt('Invalid analytics event name "%s": maximum length is 40 characters, but it is %d characters long.', [AEventName, Length(AEventName)]);
  {$ENDIF}

  {$REGION 'android'}
  {$IF defined(android)}

    var LBundle := TJBundle.JavaClass.init;
    for var I := Low(AEventValues) to High(AEventValues) do
      LBundle.putString(
        StringToJString(AEventValues[i].name),
        StringToJString(AEventValues[i].value));

    {$IF defined(ALAnalyticsFirebase)}
    fFirebaseAnalytics.logEvent(StringToJString(AEventName), Lbundle);
    {$ENDIF}

    {$IF defined(ALAnalyticsFacebook)}
    fFacebookAppEventsLogger.logEvent(StringToJString(AEventName), LBundle);
    {$ENDIF}

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'ios'}
  {$IF defined(ios)}

    var LParameters := TNSMutableDictionary.Create;
    try
      for var I := Low(AEventValues) to High(AEventValues) do
        LParameters.setObject(StringToID(AEventValues[i].value), StringToID(AEventValues[i].name));

      {$IF defined(ALAnalyticsFirebase)}
      TFIRAnalytics.OCClass.logEventWithName(StrToNsStr(AEventName), LParameters);
      {$ENDIF}

      {$IF defined(ALAnalyticsFacebook)}
      if not FFacebookAppActivated then begin
        TMonitor.Enter(FPendingOperations);
        try
          if FFacebookAppActivated then TFBSDKAppEvents.OCClass.shared.logEvent(StrToNsStr(AEventName), LParameters)
          else begin
            var LPendingOperation: TPendingOperation;
            LPendingOperation.Kind := TPendingOperationKind.TrackEventWithValues;
            LPendingOperation.EventName := AEventName;
            LPendingOperation.EventValues := AEventValues;
            LPendingOperation.UserID := 0;
            FPendingOperations.Enqueue(LPendingOperation);
          end;
        finally
          TMonitor.Exit(FPendingOperations);
        end;
      end
      else TFBSDKAppEvents.OCClass.shared.logEvent(StrToNsStr(AEventName), LParameters);
      {$ENDIF}

    finally
      LParameters.release;
    end;

  {$ENDIF}
  {$ENDREGION}

end;

{********************************************************************************************}
procedure TALAnalytics.TrackScreenView(const AScreenName: String; const AScreenClass: String);
begin
  // https://firebase.google.com/docs/analytics/screenviews
  // https://firebase.google.com/docs/reference/kotlin/com/google/firebase/analytics/FirebaseAnalytics.Param
  trackEvent(
    'screen_view',
    [TNameValuePair.Create('screen_name', AScreenName),
     TNameValuePair.Create('screen_class', AScreenClass)]);
end;

{*****************************************************}
procedure TALAnalytics.SetUserID(const AUserID: Int64);
begin

  {$IFDEF DEBUG}
  ALLog(Classname+'.SetUserID', 'UserID: ' + ALInttoStrW(AUserID));
  {$ENDIF}

  if AUserID = 0 then begin
    ClearUserID;
    Exit;
  end;

  {$REGION 'android'}
  {$IF defined(android)}

    {$IF defined(ALAnalyticsFirebase)}
    fFirebaseAnalytics.setUserID(StringToJstring(ALIntToStrW(AUserID)));
    {$ENDIF}

    {$IF defined(ALAnalyticsFacebook)}
    TJAppEventsLogger.JavaClass.setUserID(StringToJstring(ALIntToStrW(AUserID)));
    {$ENDIF}

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'ios'}
  {$IF defined(ios)}

    {$IF defined(ALAnalyticsFirebase)}
    TFIRAnalytics.OCClass.setUserID(strToNSStr(ALIntToStrW(AUserID)));
    {$ENDIF}

    {$IF defined(ALAnalyticsFacebook)}
    if not FFacebookAppActivated then begin
      TMonitor.Enter(FPendingOperations);
      try
        if FFacebookAppActivated then TFBSDKAppEvents.OCClass.shared.setUserID(strToNSStr(ALIntToStrW(AUserID)))
        else begin
          var LPendingOperation: TPendingOperation;
          LPendingOperation.Kind := TPendingOperationKind.SetUserID;
          LPendingOperation.EventName := '';
          LPendingOperation.EventValues := nil;
          LPendingOperation.UserID := AUserID;
          FPendingOperations.Enqueue(LPendingOperation);
        end;
      finally
        TMonitor.Exit(FPendingOperations);
      end;
    end
    else TFBSDKAppEvents.OCClass.shared.setUserID(strToNSStr(ALIntToStrW(AUserID)));
    {$ENDIF}

  {$ENDIF}
  {$ENDREGION}

end;

{*********************************}
procedure TALAnalytics.ClearUserID;
begin

  {$IFDEF DEBUG}
  ALLog(Classname+'.ClearUserID');
  {$ENDIF}

  {$REGION 'android'}
  {$IF defined(android)}

    {$IF defined(ALAnalyticsFirebase)}
    fFirebaseAnalytics.setUserID(nil);
    {$ENDIF}

    {$IF defined(ALAnalyticsFacebook)}
    TJAppEventsLogger.JavaClass.ClearUserID;
    {$ENDIF}

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'ios'}
  {$IF defined(ios)}

    {$IF defined(ALAnalyticsFirebase)}
    TFIRAnalytics.OCClass.setUserID(nil);
    {$ENDIF}

    {$IF defined(ALAnalyticsFacebook)}
    if not FFacebookAppActivated then begin
      TMonitor.Enter(FPendingOperations);
      try
        if FFacebookAppActivated then TFBSDKAppEvents.OCClass.shared.setUserID(nil)
        else begin
          var LPendingOperation: TPendingOperation;
          LPendingOperation.Kind := TPendingOperationKind.ClearUserID;
          LPendingOperation.EventName := '';
          LPendingOperation.EventValues := nil;
          LPendingOperation.UserID := 0;
          FPendingOperations.Enqueue(LPendingOperation);
        end;
      finally
        TMonitor.Exit(FPendingOperations);
      end;
    end
    else TFBSDKAppEvents.OCClass.shared.setUserID(nil);
    {$ENDIF}

  {$ENDIF}
  {$ENDREGION}

end;

{************************************************************************************************}
procedure TALAnalytics.SetUserProperty(const APropertyName: String; const APropertyValue: String);
begin

  {$IFDEF DEBUG}
  ALLog(Classname+'.SetUserProperty', 'PropertyName: ' + APropertyName + ' | PropertyValue: ' + APropertyValue);
  {$ENDIF}

  {$REGION 'android'}
  {$IF defined(android)}

    {$IF defined(ALAnalyticsFirebase)}
    fFirebaseAnalytics.setUserProperty(StringToJString(APropertyName), StringToJString(APropertyValue));
    {$ENDIF}

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'ios'}
  {$IF defined(ios)}

    {$IF defined(ALAnalyticsFirebase)}
    TFIRAnalytics.OCClass.setUserPropertyString(StrToNSStr(APropertyValue), StrToNSStr(APropertyName));
    {$ENDIF}

  {$ENDIF}
  {$ENDREGION}

end;

{********************************************************************}
procedure TALAnalytics.ClearUserProperty(const APropertyName: String);
begin

  {$IFDEF DEBUG}
  ALLog(Classname+'.ClearUserProperty', 'PropertyName: ' + APropertyName);
  {$ENDIF}

  {$REGION 'android'}
  {$IF defined(android)}

    {$IF defined(ALAnalyticsFirebase)}
    fFirebaseAnalytics.setUserProperty(StringToJString(APropertyName), nil);
    {$ENDIF}

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'ios'}
  {$IF defined(ios)}

    {$IF defined(ALAnalyticsFirebase)}
    TFIRAnalytics.OCClass.setUserPropertyString(nil, StrToNSStr(APropertyName));
    {$ENDIF}

  {$ENDIF}
  {$ENDREGION}

end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.Analytics','initialization');
  {$ENDIF}
  TALAnalytics.FInstance := nil;
  TALAnalytics.CreateInstanceFunc := @TALAnalytics.CreateInstance;

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.Analytics','finalization');
  {$ENDIF}
  ALFreeAndNil(TALAnalytics.FInstance);

end.