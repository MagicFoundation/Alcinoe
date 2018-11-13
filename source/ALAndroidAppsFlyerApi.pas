unit ALAndroidAppsFlyerApi;

interface

uses Androidapi.JNIBridge,
     Androidapi.JNI.App,
     Androidapi.JNI.GraphicsContentViewText,
     Androidapi.JNI.JavaTypes;

type

  {***************************************}
  JAppsFlyerConversionListener = interface;
  JALAppsFlyerLib = interface;

  {*******************************************************}
  JAppsFlyerConversionListenerClass = interface(IJavaClass)
    ['{E7C1E146-2E55-478F-9218-C1DAA89DA964}']
  end;

  {**********************************************************}
  [JavaSignature('com/appsflyer/AppsFlyerConversionListener')]
  JAppsFlyerConversionListener = interface(IJavaInstance)
    ['{ACE202A5-AD33-4D2D-876B-296874928AA4}']
    procedure onAppOpenAttribution(conversionData: JMap); cdecl;
    procedure onAttributionFailure(errorMessage: JString); cdecl;
    procedure onInstallConversionDataLoaded(conversionData: JMap); cdecl;
    procedure onInstallConversionFailure(errorMessage: JString); cdecl;
  end;
  TJAppsFlyerConversionListener = class(TJavaGenericImport<JAppsFlyerConversionListenerClass, JAppsFlyerConversionListener>) end;

  {*******************************}
  //this class because of this bug:
  //https://stackoverflow.com/questions/53141813/delphi-android-java-class-init-function
  JALAppsFlyerLibClass = interface(JObjectClass)
    ['{48A22162-044E-48B1-BBA0-C8D02162A522}']
    {class} procedure initialize(key: JString; conversionListener: JAppsFlyerConversionListener; context: JContext); cdecl;
    {class} procedure sendDeepLinkData(activity: JActivity); cdecl;
    {class} procedure startTracking(application: JApplication); cdecl;
    {class} procedure trackEvent(context: JContext; eventName: JString; eventValues: JHashMap); cdecl; overload;
    {class} procedure trackEvent(context: JContext; eventName: JString); cdecl; overload;
    {class} procedure unregisterConversionListener; cdecl;
    {class} procedure setAndroidIdData(androidIdData: JString); cdecl;
    {class} procedure enableUninstallTracking(senderId: JString); cdecl;
    {class} procedure updateServerUninstallToken(context: JContext; refreshedToken: JString); cdecl;
    {class} procedure setCustomerUserId(id: JString); cdecl;
  end;

  {**************************************************}
  [JavaSignature('com/alcinoe/appsflyer/ALAppsFlyer')]
  JALAppsFlyerLib = interface(JObject)
    ['{6145C2CE-433B-4F69-B996-614EA0C015A5}']
  end;
  TJALAppsFlyerLib = class(TJavaGenericImport<JALAppsFlyerLibClass, JALAppsFlyerLib>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidAppsFlyerApi.JAppsFlyerConversionListener', TypeInfo(ALAndroidAppsFlyerApi.JAppsFlyerConversionListener));
  TRegTypes.RegisterType('ALAndroidAppsFlyerApi.JALAppsFlyerLib', TypeInfo(ALAndroidAppsFlyerApi.JALAppsFlyerLib));
end;

initialization
  RegisterTypes;

end.

