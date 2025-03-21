//
// Made from com.vk:android-sdk-core:3.5.1
//
unit Alcinoe.AndroidApi.VKontakte;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported123}
  //Please run <Alcinoe>\Tools\NativeBridgeFileGenerator\NativeBridgeFileGeneratorAndroid.bat
  //with the library identifiers com.vk:android-sdk-core:xx.xx.xx where xx.xx.xx
  //is the last version of the VKontakte (You can find this version at
  //https://github.com/VKCOM/vk-android-sdk/releases) and gave also the path to
  //<Alcinoe>\Source\Alcinoe.AndroidApi.VKontakte.pas to build the compare source file. Then make a diff
  //compare between the new generated Alcinoe.AndroidApi.VKontakte.pas and this one to see if the api
  //signature is still the same
  {$MESSAGE WARN 'Check if the api signature of the last version of VKontakte sdk (android) is still the same'}
{$ENDIF}

uses
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.App,
  Androidapi.JNI.Os;

type

  {******************}
  JUserId = interface;
  JVKAuthException = interface;
  JVKScope = interface;
  JVKAccessToken = interface;
  JVKAuthCallback = interface;
  JVK = interface;

  {****************************************}
  JUserIdClass = interface(JParcelableClass)
    ['{3F4AC4F7-0225-4A77-8EC7-7957BBAEB98E}']
  end;
  [JavaSignature('com/vk/dto/common/id/UserId')]
  JUserId = interface(JParcelable)
    ['{CCE90A13-2538-4413-BD73-B27E5BAA4628}']
    function toString: JString; cdecl;
  end;
  TJUserId = class(TJavaGenericImport<JUserIdClass, JUserId>) end;

  {************************************************}
  JVKAuthExceptionClass = interface(JExceptionClass)
    ['{D3099883-E9CD-4B17-BF67-931280B3C2D3}']
  end;
  [JavaSignature('com/vk/api/sdk/exceptions/VKAuthException')]
  JVKAuthException = interface(JException)
    ['{356CB57F-3E2F-4F84-9767-CC7DA9E4CCB0}']
    function getAuthError: JString; cdecl;
    function isCanceled: Boolean; cdecl;
  end;
  TJVKAuthException = class(TJavaGenericImport<JVKAuthExceptionClass, JVKAuthException>) end;

  {***********************************}
  JVKScopeClass = interface(JEnumClass)
    ['{6D299E22-9E66-4DD9-8556-17C7830134F6}']
    {class} function _GetADS: JVKScope; cdecl;
    {class} function _GetAUDIO: JVKScope; cdecl;
    {class} function _GetDOCS: JVKScope; cdecl;
    {class} function _GetEMAIL: JVKScope; cdecl;
    {class} function _GetFRIENDS: JVKScope; cdecl;
    {class} function _GetGROUPS: JVKScope; cdecl;
    {class} function _GetMARKET: JVKScope; cdecl;
    {class} function _GetMESSAGES: JVKScope; cdecl;
    {class} function _GetNOTES: JVKScope; cdecl;
    {class} function _GetNOTIFICATIONS: JVKScope; cdecl;
    {class} function _GetNOTIFY: JVKScope; cdecl;
    {class} function _GetOFFLINE: JVKScope; cdecl;
    {class} function _GetPAGES: JVKScope; cdecl;
    {class} function _GetPHONE: JVKScope; cdecl;
    {class} function _GetPHOTOS: JVKScope; cdecl;
    {class} function _GetSTATS: JVKScope; cdecl;
    {class} function _GetSTATUS: JVKScope; cdecl;
    {class} function _GetSTORIES: JVKScope; cdecl;
    {class} function _GetVIDEO: JVKScope; cdecl;
    {class} function _GetWALL: JVKScope; cdecl;
    {class} function valueOf(name: JString): JVKScope; cdecl;
    {class} function values: TJavaObjectArray<JVKScope>; cdecl;
    {class} property ADS: JVKScope read _GetADS;
    {class} property AUDIO: JVKScope read _GetAUDIO;
    {class} property DOCS: JVKScope read _GetDOCS;
    {class} property EMAIL: JVKScope read _GetEMAIL;
    {class} property FRIENDS: JVKScope read _GetFRIENDS;
    {class} property GROUPS: JVKScope read _GetGROUPS;
    {class} property MARKET: JVKScope read _GetMARKET;
    {class} property MESSAGES: JVKScope read _GetMESSAGES;
    {class} property NOTES: JVKScope read _GetNOTES;
    {class} property NOTIFICATIONS: JVKScope read _GetNOTIFICATIONS;
    {class} property NOTIFY: JVKScope read _GetNOTIFY;
    {class} property OFFLINE: JVKScope read _GetOFFLINE;
    {class} property PAGES: JVKScope read _GetPAGES;
    {class} property PHONE: JVKScope read _GetPHONE;
    {class} property PHOTOS: JVKScope read _GetPHOTOS;
    {class} property STATS: JVKScope read _GetSTATS;
    {class} property STATUS: JVKScope read _GetSTATUS;
    {class} property STORIES: JVKScope read _GetSTORIES;
    {class} property VIDEO: JVKScope read _GetVIDEO;
    {class} property WALL: JVKScope read _GetWALL;
  end;
  [JavaSignature('com/vk/api/sdk/auth/VKScope')]
  JVKScope = interface(JEnum)
    ['{2596E59E-D2E5-457F-B3EE-CA7ADD5FD1C2}']
  end;
  TJVKScope = class(TJavaGenericImport<JVKScopeClass, JVKScope>) end;

  {*******************************************}
  JVKAccessTokenClass = interface(JObjectClass)
    ['{4AA5D139-70EF-4BED-AFCE-6AD11B109F57}']
  end;
  [JavaSignature('com/vk/api/sdk/auth/VKAccessToken')]
  JVKAccessToken = interface(JObject)
    ['{B7E0A898-D133-4A4B-A74E-F192D64E28DA}']
    function getAccessToken: JString; cdecl;
    function getCreated: Int64; cdecl;
    function getEmail: JString; cdecl;
    function getPhone: JString; cdecl;
    function getPhoneAccessKey: JString; cdecl;
    function getSecret: JString; cdecl;
    function getUserId: JUserId; cdecl;
    function isValid: Boolean; cdecl;
  end;
  TJVKAccessToken = class(TJavaGenericImport<JVKAccessTokenClass, JVKAccessToken>) end;

  {******************************************}
  JVKAuthCallbackClass = interface(IJavaClass)
    ['{85991DF5-2CD9-46BD-B3BC-41373EA0850F}']
  end;
  [JavaSignature('com/vk/api/sdk/auth/VKAuthCallback')]
  JVKAuthCallback = interface(IJavaInstance)
    ['{682834EF-3C5A-40DA-AD7D-30DE4015C2B5}']
    procedure onLogin(token: JVKAccessToken); cdecl;
    procedure onLoginFailed(authException: JVKAuthException); cdecl;
  end;
  TJVKAuthCallback = class(TJavaGenericImport<JVKAuthCallbackClass, JVKAuthCallback>) end;

  {********************************}
  JVKClass = interface(JObjectClass)
    ['{0450DCD7-E946-43D6-801E-9DAC255B6052}']
    {class} function getUserId: JUserId; cdecl;
    {class} procedure initialize(context: JContext); cdecl;
    {class} procedure login(activity: JActivity); cdecl; overload;
    {class} procedure login(activity: JActivity; scopes: JCollection); cdecl; overload;
    {class} procedure logout; cdecl;
    {class} function onActivityResult(requestCode: Integer; resultCode: Integer; data: JIntent; callback: JVKAuthCallback): Boolean; cdecl;
  end;
  [JavaSignature('com/vk/api/sdk/VK')]
  JVK = interface(JObject)
    ['{CBA5C3FA-9F3B-467C-A3D5-0F21093F2CFB}']
  end;
  TJVK = class(TJavaGenericImport<JVKClass, JVK>) end;

implementation

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.VKontakte.JUserId', TypeInfo(Alcinoe.AndroidApi.VKontakte.JUserId));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.VKontakte.JVKAuthException', TypeInfo(Alcinoe.AndroidApi.VKontakte.JVKAuthException));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.VKontakte.JVKScope', TypeInfo(Alcinoe.AndroidApi.VKontakte.JVKScope));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.VKontakte.JVKAccessToken', TypeInfo(Alcinoe.AndroidApi.VKontakte.JVKAccessToken));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.VKontakte.JVKAuthCallback', TypeInfo(Alcinoe.AndroidApi.VKontakte.JVKAuthCallback));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.VKontakte.JVK', TypeInfo(Alcinoe.AndroidApi.VKontakte.JVK));
end;

initialization
  RegisterTypes;

end.
