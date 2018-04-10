unit ALAndroidGooglePlayServicesApi;

interface

uses Androidapi.JNI.GraphicsContentViewText,
     Androidapi.JNIBridge,
     Androidapi.JNI.JavaTypes,
     Androidapi.JNI.Location;

type

  {**************************************}
  JALLocationServicesListener = interface;
  JALLocationServices = interface;

  {******************************************************}
  JALLocationServicesListenerClass = interface(IJavaClass)
    ['{0C115FE4-04ED-47EE-948A-EC67D83CD9A3}']
  end;

  {****************************************************************}
  [JavaSignature('com/alcinoe/location/ALLocationServicesListener')]
  JALLocationServicesListener = interface(IJavaInstance)
    ['{613CEAD9-104B-4F6A-B105-200F34DDB5DD}']
    procedure onLocationChanged(location: JLocation); cdecl;
  end;
  TJALLocationServicesListener = class(TJavaGenericImport<JALLocationServicesListenerClass, JALLocationServicesListener>) end;

  {************************************************}
  JALLocationServicesClass = interface(JObjectClass)
    ['{253118F7-7CF0-4EBB-8F94-3FD6C560B708}']
    {class} function init(context: JContext): JALLocationServices; cdecl;
  end;

  {********************************************************}
  [JavaSignature('com/alcinoe/location/ALLocationServices')]
  JALLocationServices = interface(JObject)
    ['{F0547773-B7C9-4AE4-8838-99CB941CA0CB}']
    procedure setListener(listener: JALLocationServicesListener); cdecl;
    procedure startLocationUpdates(startWithLastKnownLocation: boolean;
                                   interval: Int64;
                                   fastestInterval: Int64;
                                   maxWaitTime: Int64;
                                   priority: integer;
                                   smallestDisplacement: Single); cdecl;
    procedure stopLocationUpdates; cdecl;
  end;
  TJALLocationServices = class(TJavaGenericImport<JALLocationServicesClass, JALLocationServices>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidGooglePlayServicesApi.JALLocationServicesListener', TypeInfo(ALAndroidGooglePlayServicesApi.JALLocationServicesListener));
  TRegTypes.RegisterType('ALAndroidGooglePlayServicesApi.JALLocationServices', TypeInfo(ALAndroidGooglePlayServicesApi.JALLocationServices));
end;

initialization
  RegisterTypes;

end.
