unit Alcinoe.FMX.UserPreferences;

interface

{$I Alcinoe.inc}

{$IF defined(ANDROID)}
uses
  Androidapi.JNI.GraphicsContentViewText;
{$ENDIF}
{$IF defined(ALMacOS)}
uses
  MacApi.Foundation;
{$ENDIF}
{$IF defined(IOS)}
uses
  iOSApi.Foundation;
{$ENDIF}
{$IF defined(MSWindows) and (not defined(ALDPK))}
uses
  Alcinoe.IniFiles;
{$ENDIF}

Type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALUserPreferences = class(Tobject)
  private
    class function CreateInstance: TALUserPreferences;
    class function GetInstance: TALUserPreferences; static;
  protected
    class var FInstance: TALUserPreferences;
  public
    type
      TCreateInstanceFunc = function: TALUserPreferences;
    class var CreateInstanceFunc: TCreateInstanceFunc;
    class property Instance: TALUserPreferences read GetInstance;
    class function HasInstance: Boolean; inline;
  private
    {$IF defined(ANDROID)}
    FSharedPreferences: JSharedPreferences;
    {$ENDIF}
    {$IF defined(ALAppleOS)}
    FUserDefaults: NSUserDefaults;
    {$ENDIF}
    {$IF defined(MSWindows) and (not defined(ALDPK))}
    FSection: String;
    FIniFile: TALIniFileW;
    {$ENDIF}
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetBoolean(Const AKey: String; const AValue: Boolean);
    procedure SetInt32(Const AKey: String; const AValue: Int32);
    procedure SetInt64(Const AKey: String; const AValue: Int64);
    procedure SetString(Const AKey: String; const AValue: String);
    function getBoolean(Const AKey: String; const ADefValue: Boolean): Boolean;
    function getInt32(Const AKey: String; const ADefValue: Int32): Int32;
    function getInt64(Const AKey: String; const ADefValue: Int64): Int64;
    function getString(Const AKey: String; const ADefValue: String): String;
    {$IF defined(MSWindows) and (not defined(ALDPK))}
    property Section: String read FSection write FSection;
    {$ENDIF}
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  {$IF defined(ANDROID)}
  Androidapi.Helpers,
  Androidapi.JNI.App,
  Androidapi.JNI.JavaTypes,
  {$ENDIF}
  {$IF defined(ALAppleOS)}
  Macapi.Helpers,
  {$ENDIF}
  Alcinoe.Files,
  Alcinoe.Common;

{***********************************}
constructor TALUserPreferences.Create;
begin
  {$IF defined(Android)}
  FSharedPreferences := TAndroidHelper.Activity.getPreferences(TJContext.JavaClass.MODE_PRIVATE);
  {$ENDIF}
  {$IF defined(ALAppleOS)}
  FUserDefaults := TNSUserDefaults.Wrap(TNSUserDefaults.OCClass.standardUserDefaults);
  {$ENDIF}
  {$IF defined(MSWindows) and (not defined(ALDPK))}
  FSection := 'Settings';
  var LHomePath := TPath.Combine(TPath.GetHomePath, ALGetModuleFileNameW(true{AWithoutExtension}));
  if not TDirectory.Exists(LHomePath) then
    TDirectory.CreateDirectory(LHomePath);
  FIniFile := TALIniFileW.Create(TPath.Combine(LHomePath, 'UserPreferences.ini'));
  {$ENDIF}
  inherited;
end;

{***********************************}
destructor TALUserPreferences.Destroy;
begin
  {$IF defined(Android)}
  FSharedPreferences := nil;
  {$ENDIF}
  {$IF defined(ALAppleOS)}
  // You do not need to call release on FUserDefaults when using:
  // FUserDefaults := TNSUserDefaults.Wrap(TNSUserDefaults.OCClass.standardUserDefaults);
  // Because standardUserDefaults returns a singleton object managed by the Cocoa
  // runtime. You are not the owner, so you must not release it.
  //FUserDefaults.release;
  FUserDefaults := nil;
  {$ENDIF}
  {$IF defined(MSWindows) and (not defined(ALDPK))}
  ALFreeAndNil(FIniFile);
  {$ENDIF}
  inherited;
end;

{*************}
//[MultiThread]
class function TALUserPreferences.CreateInstance: TALUserPreferences;
begin
  result := TALUserPreferences.Create;
end;

{*************}
//[MultiThread]
class function TALUserPreferences.GetInstance: TALUserPreferences;
begin
  if FInstance = nil then begin
    var LInstance := CreateInstanceFunc;
    if AtomicCmpExchange(Pointer(FInstance), Pointer(LInstance), nil) <> nil then ALFreeAndNil(LInstance);
  end;
  Result := FInstance;
end;

{*************}
//[MultiThread]
class function TALUserPreferences.HasInstance: Boolean;
begin
  result := FInstance <> nil;
end;

{*************}
procedure TALUserPreferences.SetBoolean(Const AKey: String; const AValue: Boolean);
begin
  {$IF defined(Android)}
  var LEditor := FSharedPreferences.edit;
  LEditor.putBoolean(StringToJString(AKey), AValue);
  LEditor.commit;
  {$ENDIF}
  {$IF defined(ALAppleOS)}
  FUserDefaults.setBool(AValue, StrToNSStr(AKey));
  {$ENDIF}
  {$IF defined(MSWindows) and (not defined(ALDPK))}
  FIniFile.WriteBool(FSection{Section}, AKey{Ident}, AValue);
  {$ENDIF}
end;

{*************}
procedure TALUserPreferences.SetInt32(Const AKey: String; const AValue: Int32);
begin
  {$IF defined(Android)}
  var LEditor := FSharedPreferences.edit;
  LEditor.putint(StringToJString(AKey), AValue);
  LEditor.commit;
  {$ENDIF}
  {$IF defined(ALAppleOS)}
  FUserDefaults.setInteger(AValue, StrToNSStr(AKey));
  {$ENDIF}
  {$IF defined(MSWindows) and (not defined(ALDPK))}
  FIniFile.WriteInteger(FSection{Section}, AKey{Ident}, AValue);
  {$ENDIF}
end;

{*************}
procedure TALUserPreferences.SetInt64(Const AKey: String; const AValue: Int64);
begin
  {$IF defined(Android)}
  var LEditor := FSharedPreferences.edit;
  LEditor.putLong(StringToJString(AKey), AValue);
  LEditor.commit;
  {$ENDIF}
  {$IF defined(ALAppleOS)}
  FUserDefaults.setInteger(AValue, StrToNSStr(AKey));
  {$ENDIF}
  {$IF defined(MSWindows) and (not defined(ALDPK))}
  FIniFile.WriteInt64(FSection{Section}, AKey{Ident}, AValue);
  {$ENDIF}
end;

{*************}
procedure TALUserPreferences.SetString(Const AKey: String; const AValue: String);
begin
  {$IF defined(Android)}
  var LEditor := FSharedPreferences.edit;
  LEditor.putString(StringToJString(AKey), StringToJString(AValue));
  LEditor.commit;
  {$ENDIF}
  {$IF defined(ALAppleOS)}
  FUserDefaults.setObject(StringToID(AValue), StrToNSStr(AKey));
  {$ENDIF}
  {$IF defined(MSWindows) and (not defined(ALDPK))}
  FIniFile.WriteString(FSection{Section}, AKey{Ident}, AValue);
  {$ENDIF}
end;

{*************}
function TALUserPreferences.getBoolean(Const AKey: String; const ADefValue: Boolean): Boolean;
begin
  {$IF defined(Android)}
  Result := FSharedPreferences.GetBoolean(StringToJString(AKey), ADefValue);
  {$ELSEIF defined(ALAppleOS)}
  if (ADefValue = False) or (FUserDefaults.dictionaryRepresentation.objectForKey(StrToNSStr(AKey)) <> nil) then
    Result := FUserDefaults.boolForKey(StrToNSStr(AKey))
  else
    Result := ADefValue;
  {$ELSEIF defined(MSWindows) and (not defined(ALDPK))}
  Result := FIniFile.ReadBool(FSection{Section}, AKey{Ident}, ADefValue{Default});
  {$ELSE}
  Result := ADefValue;
  {$ENDIF}
end;

{*************}
function TALUserPreferences.getInt32(Const AKey: String; const ADefValue: Int32): Int32;
begin
  {$IF defined(Android)}
  Result := FSharedPreferences.getInt(StringToJString(AKey), ADefValue);
  {$ELSEIF defined(ALAppleOS)}
  if (ADefValue = 0) or (FUserDefaults.dictionaryRepresentation.objectForKey(StrToNSStr(AKey)) <> nil) then
    Result := FUserDefaults.integerForKey(StrToNSStr(AKey))
  else
    Result := ADefValue;
  {$ELSEIF defined(MSWindows) and (not defined(ALDPK))}
  Result := FIniFile.ReadInteger(FSection{Section}, AKey{Ident}, ADefValue{Default});
  {$ELSE}
  Result := ADefValue;
  {$ENDIF}
end;

{*************}
function TALUserPreferences.getInt64(Const AKey: String; const ADefValue: Int64): Int64;
begin
  {$IF defined(Android)}
  Result := FSharedPreferences.getLong(StringToJString(AKey), ADefValue);
  {$ELSEIF defined(ALAppleOS)}
  if (ADefValue = 0) or (FUserDefaults.dictionaryRepresentation.objectForKey(StrToNSStr(AKey)) <> nil) then
    Result := FUserDefaults.integerForKey(StrToNSStr(AKey))
  else
    Result := ADefValue;
  {$ELSEIF defined(MSWindows) and (not defined(ALDPK))}
  Result := FIniFile.ReadInt64(FSection{Section}, AKey{Ident}, ADefValue{Default});
  {$ELSE}
  Result := ADefValue;
  {$ENDIF}
end;

{*************}
function TALUserPreferences.getString(Const AKey: String; const ADefValue: String): String;
begin
  {$IF defined(Android)}
  Result := JStringToString(FSharedPreferences.getString(StringToJString(AKey), StringToJString(ADefValue)));
  {$ELSEIF defined(ALAppleOS)}
  if (ADefValue = '') or (FUserDefaults.dictionaryRepresentation.objectForKey(StrToNSStr(AKey)) <> nil) then
    Result := NSStrToStr(FUserDefaults.stringForKey(StrToNSStr(AKey)))
  else
    Result := ADefValue;
  {$ELSEIF defined(MSWindows) and (not defined(ALDPK))}
  Result := FIniFile.ReadString(FSection{Section}, AKey{Ident}, ADefValue{Default});
  {$ELSE}
  Result := ADefValue;
  {$ENDIF}
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.UserPreferences','initialization');
  {$ENDIF}
  TALUserPreferences.FInstance := nil;
  TALUserPreferences.CreateInstanceFunc := @TALUserPreferences.CreateInstance;

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.UserPreferences','finalization');
  {$ENDIF}
  ALFreeAndNil(TALUserPreferences.FInstance);

end.
