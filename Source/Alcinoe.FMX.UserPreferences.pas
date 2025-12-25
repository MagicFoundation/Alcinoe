unit Alcinoe.FMX.UserPreferences;

interface

{$I Alcinoe.inc}

uses
  {$IF defined(ANDROID)}
  Androidapi.JNI.Java.Security,
  Androidapi.JNI.GraphicsContentViewText,
  {$ENDIF}
  {$IF defined(ALMacOS)}
  MacApi.Foundation,
  {$ENDIF}
  {$IF defined(IOS)}
  iOSApi.Foundation,
  {$ENDIF}
  {$IF defined(MSWindows) and (not defined(ALDPK))}
  Alcinoe.IniFiles,
  {$ENDIF}
  system.SysUtils;

Type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
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
    FAESKey: JKey;
    function GetOrCreateAesKey: JKey;
    function AesGcmEncryptToBytes(const APlain: TBytes): TBytes;
    function AesGcmDecryptFromBytes(const AEncrypted: TBytes): TBytes;
    {$ENDIF}
    {$IF defined(ALAppleOS)}
    FUserDefaults: NSUserDefaults;
    function KeychainReadBytes(const AKey: string; out ABytes: TBytes): Boolean;
    procedure KeychainWriteBytes(const AKey: string; const ABytes: TBytes);
    {$ENDIF}
    {$IF defined(MSWindows) and (not defined(ALDPK))}
    FSection: String;
    FIniFile: TALIniFileW;
    {$ENDIF}
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function getBoolean(Const AKey: String; const ADefValue: Boolean; const ASecure: Boolean = False): Boolean;
    procedure SetBoolean(Const AKey: String; const AValue: Boolean; const ASecure: Boolean = False);
    function getInt32(Const AKey: String; const ADefValue: Int32; const ASecure: Boolean = False): Int32;
    procedure SetInt32(Const AKey: String; const AValue: Int32; const ASecure: Boolean = False);
    function getInt64(Const AKey: String; const ADefValue: Int64; const ASecure: Boolean = False): Int64;
    procedure SetInt64(Const AKey: String; const AValue: Int64; const ASecure: Boolean = False);
    function getSingle(Const AKey: String; const ADefValue: Single; const ASecure: Boolean = False): Single;
    procedure SetSingle(Const AKey: String; const AValue: Single; const ASecure: Boolean = False);
    function getDouble(Const AKey: String; const ADefValue: Double; const ASecure: Boolean = False): Double;
    procedure SetDouble(Const AKey: String; const AValue: Double; const ASecure: Boolean = False);
    function getDateTime(Const AKey: String; const ADefValue: TDateTime; const ASecure: Boolean = False): TDateTime;
    procedure SetDateTime(Const AKey: String; const AValue: TDateTime; const ASecure: Boolean = False);
    function getString(Const AKey: String; const ADefValue: String; const ASecure: Boolean = False): String;
    procedure SetString(Const AKey: String; const AValue: String; const ASecure: Boolean = False);
    {$IF defined(MSWindows) and (not defined(ALDPK))}
    property Section: String read FSection write FSection;
    {$ENDIF}
  end;

implementation

uses
  System.IOUtils,
  {$IF defined(ANDROID)}
  Androidapi.Helpers,
  AndroidApi.JNI,
  Androidapi.JNIBridge,
  Androidapi.JNI.App,
  Androidapi.JNI.JavaTypes,
  Alcinoe.AndroidApi.Crypto,
  Alcinoe.AndroidApi.Security,
  Alcinoe.AndroidApi.JavaTypes,
  {$ENDIF}
  {$IF defined(IOS)}
  Macapi.CoreFoundation,
  Macapi.Helpers,
  Macapi.ObjectiveC,
  iOSapi.Security,
  {$ENDIF}
  {$IF defined(ALMacOS)}
  Macapi.CoreFoundation,
  Macapi.Helpers,
  Macapi.ObjectiveC,
  Macapi.Security,
  {$ENDIF}
  Alcinoe.Files,
  Alcinoe.StringUtils,
  Alcinoe.Common;

{************************************}
constructor TALUserPreferences.Create;
begin
  {$IF defined(Android)}
  FSharedPreferences := TAndroidHelper.Activity.getSharedPreferences(
                          StringToJString(JStringToString(TAndroidHelper.Activity.getPackageName) + '.user_preferences'),
                          TJContext.JavaClass.MODE_PRIVATE);
  FAESKey := nil;
  {$ENDIF}
  {$IF defined(ALAppleOS)}
  {$IFNDEF ALCompilerVersionSupported130}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-4389 was corrected and adjust the IFDEF'}
  {$ENDIF}
  FUserDefaults := {$IF defined(ALMacOS)}TNSUserDefaults.Wrap({$ENDIF}TNSUserDefaults.OCClass.standardUserDefaults{$IF defined(ALMacOS)}){$ENDIF};
  FUserDefaults.retain;
  {$ENDIF}
  {$IF defined(MSWindows) and (not defined(ALDPK))}
  FSection := 'Settings';
  FIniFile := TALIniFileW.Create(TPath.Combine(ALGetAppDataPathW, 'UserPreferences.ini'));
  {$ENDIF}
  inherited;
end;

{************************************}
destructor TALUserPreferences.Destroy;
begin
  {$IF defined(Android)}
  FSharedPreferences := nil;
  FAESKey := nil;
  {$ENDIF}
  {$IF defined(ALAppleOS)}
  FUserDefaults.release;
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

{********************}
{$IF defined(Android)}
function TALUserPreferences.GetOrCreateAesKey: JKey;
begin
  var LAlias := StringToJString('alcinoe.userpreferences.encryptionkey');
  Var LKeyStore := TJKeyStore.JavaClass.getInstance(StringToJString('AndroidKeyStore'));
  LKeyStore.load(nil{JKeyStore_LoadStoreParameter});

  // If it already exists, fetch it as a generic Key
  if LKeyStore.containsAlias(LAlias) then
    Exit(LKeyStore.getKey(LAlias, nil{password}));

  // Otherwise create it
  var LKeyGenerator := TJKeyGenerator.JavaClass.getInstance(
                         StringToJString('AES'), // algorithm: JString
                         StringToJString('AndroidKeyStore')); // provider: JString
  var LBuilder := TJKeyGenParameterSpec_Builder.JavaClass.init(
                    LAlias, // keystoreAlias: JString
                    TJKeyProperties.JavaClass.PURPOSE_ENCRYPT or
                    TJKeyProperties.JavaClass.PURPOSE_DECRYPT); // purposes: Integer
  var LBlockModesArray := TJavaObjectArray<JString>.Create(1);
  var LEncryptionPaddingsArray := TJavaObjectArray<JString>.Create(1);
  try
    LBlockModesArray.Items[0] := TJKeyProperties.JavaClass.BLOCK_MODE_GCM;
    LBuilder.setBlockModes(LBlockModesArray);
    LEncryptionPaddingsArray.Items[0] := TJKeyProperties.JavaClass.ENCRYPTION_PADDING_NONE;
    LBuilder.setEncryptionPaddings(LEncryptionPaddingsArray);
    LBuilder.setRandomizedEncryptionRequired(True);
    LBuilder.setKeySize(256);
    var LKeyGenParameterSpec := LBuilder.build;
    LKeyGenerator.init(LKeyGenParameterSpec);
    LKeyGenerator.generateKey;
  finally
    ALFreeAndNil(LBlockModesArray);
    ALFreeAndNil(LEncryptionPaddingsArray);
  end;
  Result := LKeyStore.getKey(LAlias, nil{password});
end;
{$ENDIF}

{********************}
{$IF defined(Android)}
function TALUserPreferences.AesGcmEncryptToBytes(const APlain: TBytes): TBytes;
begin
  if FAESKey = nil then begin
    FAESKey := GetOrCreateAesKey;
    if FAESKey = nil then raise Exception.Create('AES key is nil');
  end;
  var LCipher := TJCipher.JavaClass.getInstance(StringToJString('AES/GCM/NoPadding'{transformation}));
  TJALCipher.Wrap(LCipher).init(TJCipher.JavaClass.ENCRYPT_MODE{opmode}, FAESKey{key});
  var LIV := LCipher.getIV;
  try
    var LIV_LEN := LIV.Length;
    if LIV_LEN <> 12 {GCM standard IV length} then raise Exception.Create('Invalid IV length');
    var LPlainArr := TJavaArray<Byte>.Create(Length(APlain));
    try
      if Length(APlain) > 0 then ALMove(APlain[0], LPlainArr.Data^, Length(APlain));
      var LCt := LCipher.doFinal(LPlainArr); // ciphertext+tag
      try
        if Lct.Length < 16 {GCM default tag length (128 bits)} then raise Exception.Create('Invalid ciphertext length: tag missing');
        SetLength(Result, LIV_LEN + LCt.Length);
        var LOfs: Integer := 0;
        ALMove(LIV.Data^, Result[LOfs], LIV_LEN);
        Inc(LOfs, LIV_LEN);
        ALMove(LCt.Data^, Result[LOfs], LCt.Length);
      finally
        AlFreeAndNil(LCt);
      end;
    finally
      AlFreeAndNil(LPlainArr);
    end;
  finally
    AlFreeAndNil(LIV);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(Android)}
function TALUserPreferences.AesGcmDecryptFromBytes(const AEncrypted: TBytes): TBytes;
begin
  if FAESKey = nil then begin
    FAESKey := GetOrCreateAesKey;
    if FAESKey = nil then raise Exception.Create('AES key is nil');
  end;
  const LIV_LEN = 12; // GCM standard IV length
  const LTAG_LEN = 16; // GCM standard tag length
  if Length(AEncrypted) < LIV_LEN + LTAG_LEN then Exit(nil); // malformed or empty
  var LIVArr := TJavaArray<Byte>.Create(LIV_LEN);
  Try
    ALMove(AEncrypted[0], LIVArr.Data^, LIV_LEN);
    var LCtLen := Length(AEncrypted) - LIV_LEN;
    var LCtArr := TJavaArray<Byte>.Create(LCtLen);
    try
      ALMove(AEncrypted[LIV_LEN], LCtArr.Data^, LCtLen);
      var LCipher := TJCipher.JavaClass.getInstance(StringToJString('AES/GCM/NoPadding'));
      var LGcmSpec := TJGCMParameterSpec.JavaClass.init(128{tLen}, LIVArr{src});
      TJALCipher.Wrap(LCipher).init(TJCipher.JavaClass.DECRYPT_MODE{opmode}, FAESKey{key}, LGcmSpec{params});
      var LPlainArr := LCipher.doFinal(LCtArr);
      try
        Result := TJavaArrayToTBytes(LPlainArr);
      finally
        AlFreeAndNil(LPlainArr);
      end;
    finally
      AlFreeandNil(LCtArr);
    end;
  finally
    AlFreeandNil(LIVArr);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function TALUserPreferences.KeychainReadBytes(const AKey: string; out ABytes: TBytes): Boolean;
begin
  Result := True;
  var LQuery := TNSMutableDictionary.Create;
  try
    {$IF defined(ALMacOS)}
    {$IFNDEF ALCompilerVersionSupported130}
      {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-4404 was corrected and adjust the IFDEF'}
    {$ENDIF}
    LQuery.setObject(NSStringToID(kSecClassGenericPassword), NSStringToID(kSecClass));
    LQuery.setObject(StringToID('alcinoe.userpreferences'), NSStringToID(kSecAttrService));
    LQuery.setObject(StringToID(AKey), NSStringToID(kSecAttrAccount));
    LQuery.setObject(kCFBooleanTrue, NSStringToID(kSecReturnData));
    LQuery.setObject(NSStringToID(kSecMatchLimitOne), NSStringToID(kSecMatchLimit));
    {$ELSE}
    LQuery.setObject(kSecClassGenericPassword, kSecClass);
    LQuery.setObject(StringToID('alcinoe.userpreferences'), kSecAttrService);
    LQuery.setObject(StringToID(AKey), kSecAttrAccount);
    LQuery.setObject(kCFBooleanTrue, kSecReturnData);
    LQuery.setObject(kSecMatchLimitOne, kSecMatchLimit);
    {$ENDIF}
    var CFRef: CFTypeRef := nil;
    var Status := SecItemCopyMatching(NSObjectToID(LQuery), @CFRef);
    if (Status = errSecSuccess) and
       (CFRef <> nil) and
       (CFGetTypeID(CFRef) = CFDataGetTypeID) then begin
      var Data := TNSData.Wrap(CFRef);
      SetLength(ABytes, Data.length);
      if Data.length > 0 then Move(Data.bytes^, ABytes[0], Data.length);
    end
    else
      Result := False;
  finally
    LQuery.release;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
procedure TALUserPreferences.KeychainWriteBytes(const AKey: string; const ABytes: TBytes);
begin
  var LDataObj: NSData;
  if Length(ABytes) = 0 then LDataObj := TNSData.Wrap(TNSData.OCClass.data) // empty NSData
  else LDataObj := TNSData.Wrap(TNSData.OCClass.dataWithBytes(@ABytes[0], Length(ABytes)));
  // https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/MemoryMgmt/Articles/mmRules.html
  // No release required for LDataObj because it wasn’t created via a method whose name starts with “alloc”, “new”, “copy”, or “mutableCopy”.
  var LQuery := TNSMutableDictionary.Create;
  Try
    {$IF defined(ALMacOS)}
    {$IFNDEF ALCompilerVersionSupported130}
      {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-4404 was corrected and adjust the IFDEF'}
    {$ENDIF}
    LQuery.setObject(NSStringToID(kSecClassGenericPassword), NSStringToID(kSecClass));
    LQuery.setObject(StringToID('alcinoe.userpreferences'), NSStringToID(kSecAttrService));
    LQuery.setObject(StringToID(AKey), NSStringToID(kSecAttrAccount));
    {$ELSE}
    LQuery.setObject(kSecClassGenericPassword, kSecClass);
    LQuery.setObject(StringToID('alcinoe.userpreferences'), kSecAttrService);
    LQuery.setObject(StringToID(AKey), kSecAttrAccount);
    {$ENDIF}

    var LAttrs := TNSMutableDictionary.Create;
    Try
      {$IF defined(ALMacOS)}
      {$IFNDEF ALCompilerVersionSupported130}
        {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-4404 was corrected and adjust the IFDEF'}
      {$ENDIF}
      LAttrs.setObject(NSObjectToID(LDataObj), NSStringToID(kSecValueData));
      LAttrs.setObject(NSStringToID(kSecAttrAccessibleAfterFirstUnlock), NSStringToID(kSecAttrAccessible));
      {$ELSE}
      LAttrs.setObject(NSObjectToID(LDataObj), kSecValueData);
      LAttrs.setObject(kSecAttrAccessibleAfterFirstUnlock, kSecAttrAccessible);
      {$ENDIF}

      var LStatus := SecItemUpdate(NSObjectToID(LQuery), NSObjectToID(LAttrs));
      if LStatus = errSecItemNotFound then begin
        LAttrs.addEntriesFromDictionary(LQuery);
        LStatus := SecItemAdd(NSObjectToID(LAttrs), nil);
      end;

      if LStatus <> errSecSuccess then
        raise Exception.CreateFmt('Keychain save failed (status %d)', [LStatus]);
    finally
      LAttrs.release;
    end;
  finally
    LQuery.release;
  end;
end;
{$ENDIF}

{****************************************************************************************************************************}
function TALUserPreferences.getBoolean(Const AKey: String; const ADefValue: Boolean; const ASecure: Boolean = False): Boolean;
begin
  {$IF defined(Android)}
  if ASecure and TOSVersion.Check(6, 0) {API level >= 23 (Android M)} then begin
    var S := JStringToString(FSharedPreferences.getString(StringToJString(AKey){key}, StringToJString(''){defValue}));
    if S = '' then Result := ADefValue
    else begin
      var LDecrypted := AesGcmDecryptFromBytes(ALBase64DecodeBytes(S));
      if Length(LDecrypted) = 1 then Result := LDecrypted[0] <> 0
      else Result := ADefValue;
    end;
  end
  else
    Result := FSharedPreferences.GetBoolean(StringToJString(AKey), ADefValue);
  {$ELSEIF defined(ALAppleOS)}
  if ASecure then begin
    var LBytes: TBytes;
    if KeychainReadBytes(AKey, LBytes) and (Length(LBytes) = 1) then Result := LBytes[0] <> 0
    else Result := ADefValue;
  end
  else begin
    if (ADefValue = False) or (FUserDefaults.dictionaryRepresentation.objectForKey(StringToID(AKey)) <> nil) then
      Result := FUserDefaults.boolForKey(StrToNSStr(AKey))
    else
      Result := ADefValue;
  end;
  {$ELSEIF defined(MSWindows) and (not defined(ALDPK))}
  Result := FIniFile.ReadBool(FSection{Section}, AKey{Ident}, ADefValue{Default});
  {$ELSE}
  Result := ADefValue;
  {$ENDIF}

  {$IF defined(DEBUG)}
  ALLog('TALUserPreferences.getBoolean', 'ASecure: ' + ALBoolTostrW(ASecure)+ ' | AKey: ' + AKey + ' | Value: ' + ALBooltostrW(Result));
  {$ENDIF}
end;

{*****************************************************************************************************************}
procedure TALUserPreferences.SetBoolean(Const AKey: String; const AValue: Boolean; const ASecure: Boolean = False);
begin
  {$IF defined(DEBUG)}
  ALLog('TALUserPreferences.SetBoolean', 'ASecure: ' + ALBoolTostrW(ASecure)+ ' | AKey: ' + AKey + ' | AValue: ' + ALBooltostrW(AValue));
  {$ENDIF}

  {$IF defined(Android)}
  if ASecure and TOSVersion.Check(6, 0) {API level >= 23 (Android M)} then begin
    var LBytes: TBytes;
    SetLength(LBytes, 1);
    LBytes[0] := Byte(AValue);
    var LEditor := FSharedPreferences.edit;
    LEditor.putString(StringToJString(AKey), StringToJString(ALBase64EncodeBytesW(AesGcmEncryptToBytes(LBytes))));
    LEditor.commit;
  end
  else begin
    var LEditor := FSharedPreferences.edit;
    LEditor.putBoolean(StringToJString(AKey), AValue);
    LEditor.commit;
  end;
  {$ELSEIF defined(ALAppleOS)}
  if ASecure then begin
    var LBytes: TBytes;
    SetLength(LBytes, 1);
    LBytes[0] := Byte(AValue);
    KeychainWriteBytes(AKey, LBytes);
  end
  else
    FUserDefaults.setBool(AValue, StrToNSStr(AKey));
  {$ELSEIF defined(MSWindows) and (not defined(ALDPK))}
  FIniFile.WriteBool(FSection{Section}, AKey{Ident}, AValue);
  {$ENDIF}
end;

{**********************************************************************************************************************}
function TALUserPreferences.getInt32(Const AKey: String; const ADefValue: Int32; const ASecure: Boolean = False): Int32;
begin
  {$IF defined(Android)}
  if ASecure and TOSVersion.Check(6, 0) {API level >= 23 (Android M)} then begin
    var S := JStringToString(FSharedPreferences.getString(StringToJString(AKey){key}, StringToJString(''){defValue}));
    if S = '' then Result := ADefValue
    else begin
      var LDecrypted := AesGcmDecryptFromBytes(ALBase64DecodeBytes(S));
      if Length(LDecrypted) = SizeOf(Int32) then Result := PInteger(@LDecrypted[0])^
      else Result := ADefValue;
    end;
  end
  else
    Result := FSharedPreferences.getInt(StringToJString(AKey), ADefValue);
  {$ELSEIF defined(ALAppleOS)}
  if ASecure then begin
    var LBytes: TBytes;
    if KeychainReadBytes(AKey, LBytes) and (Length(LBytes) = SizeOf(Int32)) then Result := PInteger(@LBytes[0])^
    else Result := ADefValue;
  end
  else begin
    if (ADefValue = 0) or (FUserDefaults.dictionaryRepresentation.objectForKey(StringToID(AKey)) <> nil) then
      Result := FUserDefaults.integerForKey(StrToNSStr(AKey))
    else
      Result := ADefValue;
  end;
  {$ELSEIF defined(MSWindows) and (not defined(ALDPK))}
  Result := FIniFile.ReadInteger(FSection{Section}, AKey{Ident}, ADefValue{Default});
  {$ELSE}
  Result := ADefValue;
  {$ENDIF}

  {$IF defined(DEBUG)}
  ALLog('TALUserPreferences.getInt32', 'ASecure: ' + ALBoolTostrW(ASecure)+ ' | AKey: ' + AKey + ' | Value: ' + ALInttostrW(Result));
  {$ENDIF}
end;

{*************************************************************************************************************}
procedure TALUserPreferences.SetInt32(Const AKey: String; const AValue: Int32; const ASecure: Boolean = False);
begin
  {$IF defined(DEBUG)}
  ALLog('TALUserPreferences.SetInt32', 'ASecure: ' + ALBoolTostrW(ASecure)+ ' | AKey: ' + AKey + ' | AValue: ' + ALInttostrW(AValue));
  {$ENDIF}

  {$IF defined(Android)}
  if ASecure and TOSVersion.Check(6, 0) {API level >= 23 (Android M)} then begin
    var LBytes: TBytes;
    SetLength(LBytes, SizeOf(Int32));
    PInteger(@LBytes[0])^ := AValue;
    var LEditor := FSharedPreferences.edit;
    LEditor.putString(StringToJString(AKey), StringToJString(ALBase64EncodeBytesW(AesGcmEncryptToBytes(LBytes))));
    LEditor.commit;
  end
  else begin
    var LEditor := FSharedPreferences.edit;
    LEditor.putInt(StringToJString(AKey), AValue);
    LEditor.commit;
  end;
  {$ELSEIF defined(ALAppleOS)}
  if ASecure then begin
    var LBytes: TBytes;
    SetLength(LBytes, SizeOf(Int32));
    PInteger(@LBytes[0])^ := AValue;
    KeychainWriteBytes(AKey, LBytes);
  end
  else
    FUserDefaults.setInteger(AValue, StrToNSStr(AKey));
  {$ELSEIF defined(MSWindows) and (not defined(ALDPK))}
  FIniFile.WriteInteger(FSection{Section}, AKey{Ident}, AValue);
  {$ENDIF}
end;

{**********************************************************************************************************************}
function TALUserPreferences.getInt64(Const AKey: String; const ADefValue: Int64; const ASecure: Boolean = False): Int64;
begin
  {$IF defined(Android)}
  if ASecure and TOSVersion.Check(6, 0) {API level >= 23 (Android M)} then begin
    var S := JStringToString(FSharedPreferences.getString(StringToJString(AKey){key}, StringToJString(''){defValue}));
    if S = '' then Result := ADefValue
    else begin
      var LDecrypted := AesGcmDecryptFromBytes(ALBase64DecodeBytes(S));
      if Length(LDecrypted) = SizeOf(Int64) then Result := PInt64(@LDecrypted[0])^
      else Result := ADefValue;
    end;
  end
  else
    Result := FSharedPreferences.getLong(StringToJString(AKey), ADefValue);
  {$ELSEIF defined(ALAppleOS)}
  if ASecure then begin
    var LBytes: TBytes;
    if KeychainReadBytes(AKey, LBytes) and (Length(LBytes) = SizeOf(Int64)) then Result := PInt64(@LBytes[0])^
    else Result := ADefValue;
  end
  else begin
    if (ADefValue = 0) or (FUserDefaults.dictionaryRepresentation.objectForKey(StringToID(AKey)) <> nil) then
      Result := FUserDefaults.integerForKey(StrToNSStr(AKey))
    else
      Result := ADefValue;
  end;
  {$ELSEIF defined(MSWindows) and (not defined(ALDPK))}
  Result := FIniFile.ReadInt64(FSection{Section}, AKey{Ident}, ADefValue{Default});
  {$ELSE}
  Result := ADefValue;
  {$ENDIF}

  {$IF defined(DEBUG)}
  ALLog('TALUserPreferences.getInt64', 'ASecure: ' + ALBoolTostrW(ASecure)+ ' | AKey: ' + AKey + ' | Value: ' + ALInttostrW(Result));
  {$ENDIF}
end;

{*************************************************************************************************************}
procedure TALUserPreferences.SetInt64(Const AKey: String; const AValue: Int64; const ASecure: Boolean = False);
begin
  {$IF defined(DEBUG)}
  ALLog('TALUserPreferences.SetInt64', 'ASecure: ' + ALBoolTostrW(ASecure)+ ' | AKey: ' + AKey + ' | AValue: ' + ALInttostrW(AValue));
  {$ENDIF}

  {$IF defined(Android)}
  if ASecure and TOSVersion.Check(6, 0) {API level >= 23 (Android M)} then begin
    var LBytes: TBytes;
    SetLength(LBytes, SizeOf(Int64));
    PInt64(@LBytes[0])^ := AValue;
    var LEditor := FSharedPreferences.edit;
    LEditor.putString(StringToJString(AKey), StringToJString(ALBase64EncodeBytesW(AesGcmEncryptToBytes(LBytes))));
    LEditor.commit;
  end
  else begin
    var LEditor := FSharedPreferences.edit;
    LEditor.putLong(StringToJString(AKey), AValue);
    LEditor.commit;
  end;
  {$ELSEIF defined(ALAppleOS)}
  if ASecure then begin
    var LBytes: TBytes;
    SetLength(LBytes, SizeOf(Int64));
    PInt64(@LBytes[0])^ := AValue;
    KeychainWriteBytes(AKey, LBytes);
  end
  else
    FUserDefaults.setInteger(AValue, StrToNSStr(AKey));
  {$ELSEIF defined(MSWindows) and (not defined(ALDPK))}
  FIniFile.WriteInt64(FSection{Section}, AKey{Ident}, AValue);
  {$ENDIF}
end;

{*************************************************************************************************************************}
function TALUserPreferences.getSingle(Const AKey: String; const ADefValue: Single; const ASecure: Boolean = False): Single;
begin
  PInteger(@result)^ := getInt32(AKey, PInteger(@ADefValue)^, ASecure);
end;

{***************************************************************************************************************}
procedure TALUserPreferences.SetSingle(Const AKey: String; const AValue: Single; const ASecure: Boolean = False);
begin
  SetInt32(AKey, PInteger(@AValue)^, ASecure);
end;

{*************************************************************************************************************************}
function TALUserPreferences.getDouble(Const AKey: String; const ADefValue: Double; const ASecure: Boolean = False): Double;
begin
  PInt64(@result)^ := getInt64(AKey, PInt64(@ADefValue)^, ASecure);
end;

{***************************************************************************************************************}
procedure TALUserPreferences.SetDouble(Const AKey: String; const AValue: Double; const ASecure: Boolean = False);
begin
  SetInt64(AKey, PInt64(@AValue)^, ASecure);
end;

{*********************************************************************************************************************************}
function TALUserPreferences.getDateTime(Const AKey: String; const ADefValue: TDateTime; const ASecure: Boolean = False): TDateTime;
begin
  Result := ALUnixMsToDateTime(getInt64(AKey, ALDateTimeToUnixMs(ADefValue), ASecure));
end;

{********************************************************************************************************************}
procedure TALUserPreferences.SetDateTime(Const AKey: String; const AValue: TDateTime; const ASecure: Boolean = False);
begin
  SetInt64(AKey, ALDateTimeToUnixMs(AValue), ASecure);
end;

{*************************************************************************************************************************}
function TALUserPreferences.getString(Const AKey: String; const ADefValue: String; const ASecure: Boolean = False): String;
begin
  {$IF defined(Android)}
  if ASecure and TOSVersion.Check(6, 0) {API level >= 23 (Android M)} then begin
    Result := JStringToString(FSharedPreferences.getString(StringToJString(AKey){key}, StringToJString(''){defValue}));
    if Result = '' then Result := ADefValue
    else Result := TEncoding.UTF8.GetString(AesGcmDecryptFromBytes(ALBase64DecodeBytes(Result)));
  end
  else
    Result := JStringToString(FSharedPreferences.getString(StringToJString(AKey){key}, StringToJString(ADefValue){defValue}));
  {$ELSEIF defined(ALAppleOS)}
  if ASecure then begin
    var LBytes: TBytes;
    if KeychainReadBytes(AKey, LBytes) then Result := TEncoding.UTF8.GetString(LBytes)
    else Result := ADefValue;
  end
  else begin
    if (ADefValue = '') or (FUserDefaults.dictionaryRepresentation.objectForKey(StringToID(AKey)) <> nil) then
      Result := NSStrToStr(FUserDefaults.stringForKey(StrToNSStr(AKey)))
    else
      Result := ADefValue;
  end;
  {$ELSEIF defined(MSWindows) and (not defined(ALDPK))}
  Result := FIniFile.ReadString(FSection{Section}, AKey{Ident}, ADefValue{Default});
  {$ELSE}
  Result := ADefValue;
  {$ENDIF}

  {$IF defined(DEBUG)}
  ALLog('TALUserPreferences.getString', 'ASecure: ' + ALBoolTostrW(ASecure)+ ' | AKey: ' + AKey + ' | Value: ' + Result);
  {$ENDIF}
end;

{***************************************************************************************************************}
procedure TALUserPreferences.SetString(Const AKey: String; const AValue: String; const ASecure: Boolean = False);
begin
  {$IF defined(DEBUG)}
  ALLog('TALUserPreferences.SetString', 'ASecure: ' + ALBoolTostrW(ASecure)+ ' | AKey: ' + AKey + ' | AValue: ' + AValue);
  {$ENDIF}

  {$IF defined(Android)}
  var LValue: String;
  if ASecure and TOSVersion.Check(6, 0) {API level >= 23 (Android M)} then LValue := ALBase64EncodeBytesW(AesGcmEncryptToBytes(TEncoding.UTF8.GetBytes(AValue)))
  else LValue := AValue;
  var LEditor := FSharedPreferences.edit;
  LEditor.putString(StringToJString(AKey), StringToJString(LValue));
  LEditor.commit;
  {$ELSEIF defined(ALAppleOS)}
  if ASecure then
    KeychainWriteBytes(AKey, TEncoding.UTF8.GetBytes(AValue))
  else
    FUserDefaults.setObject(StringToID(AValue), StrToNSStr(AKey));
  {$ELSEIF defined(MSWindows) and (not defined(ALDPK))}
  FIniFile.WriteString(FSection{Section}, AKey{Ident}, AValue);
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