//
// Made from androidx.appcompat:appcompat:1.7.0
//
unit Alcinoe.AndroidApi.AppCompat;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported123}
  //Please run <Alcinoe>\Tools\NativeBridgeFileGenerator\NativeBridgeFileGeneratorAndroid.bat
  //with the library identifiers androidx.appcompat:appcompat:xx.xx.xx where xx.xx.xx
  //is the last version of the AppCompat (You can find this version at
  //https://maven.google.com/web/index.html#androidx.appcompat:appcompat) and gave also the path to
  //<Alcinoe>\Source\Alcinoe.AndroidApi.AppCompat.pas to build the compare source file. Then make a diff
  //compare between the new generated Alcinoe.AndroidApi.AppCompat.pas and this one to see if the api
  //signature is still the same
  {$MESSAGE WARN 'Check if the api signature of the last version of AppCompat sdk (android) is still the same'}
{$ENDIF}

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes;

type

  {*****************************}
  JAppCompatDelegate = interface;

  {***********************************************}
  JAppCompatDelegateClass = interface(JObjectClass)
    ['{E7A839F3-3705-4759-A5F9-6BA4B1B92B80}']
    {class} function _GetMODE_NIGHT_AUTO: Integer; cdecl;
    {class} function _GetMODE_NIGHT_AUTO_BATTERY: Integer; cdecl;
    {class} function _GetMODE_NIGHT_AUTO_TIME: Integer; cdecl;
    {class} function _GetMODE_NIGHT_FOLLOW_SYSTEM: Integer; cdecl;
    {class} function _GetMODE_NIGHT_NO: Integer; cdecl;
    {class} function _GetMODE_NIGHT_UNSPECIFIED: Integer; cdecl;
    {class} function _GetMODE_NIGHT_YES: Integer; cdecl;
    {class} function getDefaultNightMode: Integer; cdecl;
    {class} procedure setDefaultNightMode(mode: Integer); cdecl;
    {class} property MODE_NIGHT_AUTO: Integer read _GetMODE_NIGHT_AUTO;
    {class} property MODE_NIGHT_AUTO_BATTERY: Integer read _GetMODE_NIGHT_AUTO_BATTERY;
    {class} property MODE_NIGHT_AUTO_TIME: Integer read _GetMODE_NIGHT_AUTO_TIME;
    {class} property MODE_NIGHT_FOLLOW_SYSTEM: Integer read _GetMODE_NIGHT_FOLLOW_SYSTEM;
    {class} property MODE_NIGHT_NO: Integer read _GetMODE_NIGHT_NO;
    {class} property MODE_NIGHT_UNSPECIFIED: Integer read _GetMODE_NIGHT_UNSPECIFIED;
    {class} property MODE_NIGHT_YES: Integer read _GetMODE_NIGHT_YES;
  end;
  [JavaSignature('androidx/appcompat/app/AppCompatDelegate')]
  JAppCompatDelegate = interface(JObject)
    ['{E9AC68DA-6195-40DB-96EA-F654B72ACE8A}']
  end;
  TJAppCompatDelegate = class(TJavaGenericImport<JAppCompatDelegateClass, JAppCompatDelegate>) end;

implementation

uses
  Alcinoe.Common;

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AppCompat.JAppCompatDelegate', TypeInfo(Alcinoe.AndroidApi.AppCompat.JAppCompatDelegate));
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.AndroidApi.AppCompat','initialization');
  {$ENDIF}
  RegisterTypes;

end.
