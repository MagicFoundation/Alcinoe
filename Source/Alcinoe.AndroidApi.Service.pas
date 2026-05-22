unit Alcinoe.AndroidApi.Service;

interface

{$I Alcinoe.inc}

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.App,
  Androidapi.JNI.Os;

type

  {*********************************}
  JStatusBarNotification = interface;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported131}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-5425 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  JStatusBarNotificationClass = interface(JObjectClass)
    ['{0E26E5AC-C8D6-413E-B13A-6456FA31BB30}']
  end;
  [JavaSignature('android/service/notification/StatusBarNotification')]
  JStatusBarNotification = interface(JObject)
    ['{6A78CF31-7C5C-4D0A-B8C0-FF73B10DDBC6}']
    function getPackageName: JString; cdecl;
    function getId: Integer; cdecl;
    function getTag: JString; cdecl;
    function getUid: Integer; cdecl;
    function getOpPkg: JString; cdecl;
    function getNotification: JNotification; cdecl;
    function getUser: JUserHandle; cdecl;
    function getPostTime: Int64; cdecl;
    function getKey: JString; cdecl;
    function getGroupKey: JString; cdecl;
    function getOverrideGroupKey: JString; cdecl;
    function isGroup: Boolean; cdecl;
    function isAppGroup: Boolean; cdecl;
    function isOngoing: Boolean; cdecl;
    function isClearable: Boolean; cdecl;
    function getUserId: Integer; cdecl;
  end;
  TJStatusBarNotification = class(TJavaGenericImport<JStatusBarNotificationClass, JStatusBarNotification>) end;

implementation

uses
  Alcinoe.Common;

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Service.JStatusBarNotification', TypeInfo(Alcinoe.AndroidApi.Service.JStatusBarNotification));
end;

initialization
  {$IF defined(UiModeManager)}
  ALLog('Alcinoe.AndroidApi.Service','initialization');
  {$ENDIF}
  RegisterTypes;

end.