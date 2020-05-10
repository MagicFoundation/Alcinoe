
  JALFirebaseDynamicLinksListener = interface;
  JALFirebaseDynamicLinks = interface;

  {**********************************************************}
  JALFirebaseDynamicLinksListenerClass = interface(IJavaClass)
    ['{6BD08D6D-4A89-47CC-B042-BB9DF9699975}']
  end;

  {*********************************************************************************}
  [JavaSignature('com/alcinoe/firebase/dynamiclinks/ALFirebaseDynamicLinksListener')]
  JALFirebaseDynamicLinksListener = interface(IJavaInstance)
    ['{15A97628-F9B2-4358-B668-C858D4141067}']
    procedure onGetDynamicLinkSuccess(deepLink: JString; invitationId: JString); cdecl;
    procedure onGetDynamicLinkError(errorCode: integer; errorMsg: JString); cdecl;
  end;
  TJALFirebaseDynamicLinksListener = class(TJavaGenericImport<JALFirebaseDynamicLinksListenerClass, JALFirebaseDynamicLinksListener>) end;

  {****************************************************}
  JALFirebaseDynamicLinksClass = interface(JObjectClass)
    ['{7CC5AFFB-5D9C-43CD-9E04-7BD1AA7C77DA}']
    {class} function _GetERROR_TASK_FAILED: Integer; cdecl;
    {class} function _GetERROR_NO_PENDING_LINK: Integer; cdecl;
    {class} function init(activity: JActivity): JALFirebaseDynamicLinks; cdecl;
    {class} property ERROR_TASK_FAILED: Integer read _GetERROR_TASK_FAILED;
    {class} property ERROR_NO_PENDING_LINK: Integer read _GetERROR_NO_PENDING_LINK;
  end;

  {*************************************************************************}
  [JavaSignature('com/alcinoe/firebase/dynamiclinks/ALFirebaseDynamicLinks')]
  JALFirebaseDynamicLinks = interface(JObject)
    ['{A42F3176-EB18-4C47-9156-EA55FC4224C7}']
    procedure getDynamicLink(); cdecl;
    procedure setListener(listener: JALFirebaseDynamicLinksListener); cdecl;
  end;
  TJALFirebaseDynamicLinks = class(TJavaGenericImport<JALFirebaseDynamicLinksClass, JALFirebaseDynamicLinks>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JALFirebaseDynamicLinksListener', TypeInfo(ALAndroidFirebaseApi.JALFirebaseDynamicLinksListener));
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JALFirebaseDynamicLinks', TypeInfo(ALAndroidFirebaseApi.JALFirebaseDynamicLinks));
end;
