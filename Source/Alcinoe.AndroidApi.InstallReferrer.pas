unit Alcinoe.AndroidApi.InstallReferrer;

interface

uses
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes;

type

  {*************************************}
  JALInstallReferrerListener = interface;
  JALInstallReferrer = interface;

  {*****************************************************}
  JALInstallReferrerListenerClass = interface(IJavaClass)
    ['{1BDEF5E8-0076-4FDE-965A-602B3D130724}']
  end;

  {**********************************************************************}
  [JavaSignature('io/magicfoundation/alcinoe/installreferrer/ALInstallReferrerListener')]
  JALInstallReferrerListener = interface(IJavaInstance)
    ['{AA8674DA-EE8D-4324-97FB-4810EBDDD30B}']
    procedure onGetInstallReferrerSuccess(referrer: JString; clickTimestampSeconds: int64; installBeginTimestampSeconds: int64); cdecl;
    procedure onGetInstallReferrerError(responseCode: integer); cdecl;
  end;
  TJALInstallReferrerListener = class(TJavaGenericImport<JALInstallReferrerListenerClass, JALInstallReferrerListener>) end;

  {***********************************************}
  JALInstallReferrerClass = interface(JObjectClass)
    ['{0EB19261-0388-4FED-A8D9-9F581E214FE6}']
    {class} function init(context: JContext): JALInstallReferrer; cdecl;
  end;

  {**************************************************************}
  [JavaSignature('io/magicfoundation/alcinoe/installreferrer/ALInstallReferrer')]
  JALInstallReferrer = interface(JObject)
    ['{8E2550A4-C4B9-425F-A09A-5F397F1542AA}']
    procedure setListener(listener: JALInstallReferrerListener); cdecl;
    procedure getInstallReferrer; cdecl;
  end;
  TJALInstallReferrer = class(TJavaGenericImport<JALInstallReferrerClass, JALInstallReferrer>) end;

implementation

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Common.JALInstallReferrerListener', TypeInfo(Alcinoe.AndroidApi.InstallReferrer.JALInstallReferrerListener));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Common.JALInstallReferrer', TypeInfo(Alcinoe.AndroidApi.InstallReferrer.JALInstallReferrer));
end;

initialization
  RegisterTypes;

end.
