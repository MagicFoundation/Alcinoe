unit Alcinoe.AndroidApi.App;

interface

{$I Alcinoe.inc}

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes;

type

  {*************************}
  JUiModeManager = interface;

  {*******************************************}
  JUiModeManagerClass = interface(JObjectClass)
    ['{FD71340D-4C9D-4078-A322-218A2DAAEF69}']
    {class} function _GetMODE_NIGHT_AUTO: Integer; cdecl;
    {class} function _GetMODE_NIGHT_CUSTOM: Integer; cdecl;
    {class} function _GetMODE_NIGHT_NO: Integer; cdecl;
    {class} function _GetMODE_NIGHT_YES: Integer; cdecl;
    {class} property MODE_NIGHT_AUTO: Integer read _GetMODE_NIGHT_AUTO;
    {class} property MODE_NIGHT_CUSTOM: Integer read _GetMODE_NIGHT_CUSTOM;
    {class} property MODE_NIGHT_NO: Integer read _GetMODE_NIGHT_NO;
    {class} property MODE_NIGHT_YES: Integer read _GetMODE_NIGHT_YES;
  end;
  [JavaSignature('android/app/UiModeManager')]
  JUiModeManager = interface(JObject)
    ['{40B9632D-FC87-41D9-866A-4D0E60F9BEDF}']
    function getNightMode: Integer; cdecl;
    procedure setApplicationNightMode(mode: integer); cdecl;
  end;
  TJUiModeManager = class(TJavaGenericImport<JUiModeManagerClass, JUiModeManager>) end;

implementation

uses
  Alcinoe.Common;

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.App.JUiModeManager', TypeInfo(Alcinoe.AndroidApi.App.JUiModeManager));
end;

initialization
  {$IF defined(UiModeManager)}
  ALLog('Alcinoe.AndroidApi.App','initialization');
  {$ENDIF}
  RegisterTypes;

end.
