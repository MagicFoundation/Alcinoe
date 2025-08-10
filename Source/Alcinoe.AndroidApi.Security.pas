unit Alcinoe.AndroidApi.Security;

interface

{$I Alcinoe.inc}

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.jni.Java.Security;

type

  {*******************************}
  JKeyGenParameterSpec = interface;
  JKeyGenParameterSpec_Builder = interface;
  JKeyProperties = interface;

  {*****************************************************************}
  JKeyGenParameterSpecClass = interface(JAlgorithmParameterSpecClass)
    ['{7AC16209-CCEF-4A73-AC35-BA154718DF1A}']
  end;
  [JavaSignature('android/security/keystore/KeyGenParameterSpec')]
  JKeyGenParameterSpec = interface(JAlgorithmParameterSpec)
    ['{B5B0C3B4-C53A-4C44-B841-68131AD1FDB6}']
  end;
  TJKeyGenParameterSpec = class(TJavaGenericImport<JKeyGenParameterSpecClass, JKeyGenParameterSpec>) end;

  {*********************************************************}
  JKeyGenParameterSpec_BuilderClass = interface(JObjectClass)
    ['{58064C89-567B-4485-BBC0-846447271450}']
    {class} function init(keystoreAlias: JString; purposes: Integer): JKeyGenParameterSpec_Builder; cdecl;
  end;
  [JavaSignature('android/security/keystore/KeyGenParameterSpec$Builder')]
  JKeyGenParameterSpec_Builder = interface(JObject)
    ['{9AD5E8E6-58C9-4CA6-A93F-EC94B17D5C31}']
    function setBlockModes(blockModes: TJavaObjectArray<JString>): JKeyGenParameterSpec_Builder; cdecl;
    function setEncryptionPaddings(paddings: TJavaObjectArray<JString>): JKeyGenParameterSpec_Builder; cdecl;
    function setRandomizedEncryptionRequired(required: Boolean): JKeyGenParameterSpec_Builder; cdecl;
    function setKeySize(keySize: integer): JKeyGenParameterSpec_Builder; cdecl;
    function build: JKeyGenParameterSpec; cdecl;
  end;
  TJKeyGenParameterSpec_Builder = class(TJavaGenericImport<JKeyGenParameterSpec_BuilderClass, JKeyGenParameterSpec_Builder>) end;

  {*******************************************}
  JKeyPropertiesClass = interface(JObjectClass)
    ['{79950AF5-E74F-4655-B1C9-5079D07D3F57}']
    {class} function _GetBLOCK_MODE_GCM: JString; cdecl;
    {class} function _GetENCRYPTION_PADDING_NONE: JString; cdecl;
    {class} function _GetPURPOSE_ENCRYPT: Integer; cdecl;
    {class} function _GetPURPOSE_DECRYPT: Integer; cdecl;
    {class} property BLOCK_MODE_GCM: JString read _GetBLOCK_MODE_GCM;
    {class} property ENCRYPTION_PADDING_NONE: JString read _GetENCRYPTION_PADDING_NONE;
    {class} property PURPOSE_ENCRYPT: Integer read _GetPURPOSE_ENCRYPT;
    {class} property PURPOSE_DECRYPT: Integer read _GetPURPOSE_DECRYPT;
  end;
  [JavaSignature('android/security/keystore/KeyProperties')]
  JKeyProperties = interface(JObject)
    ['{8DA44B30-D28F-4DB8-A820-BFE99D4F0229}']
  end;
  TJKeyProperties = class(TJavaGenericImport<JKeyPropertiesClass, JKeyProperties>) end;

implementation

uses
  Alcinoe.Common;

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Security.JKeyGenParameterSpec', TypeInfo(Alcinoe.AndroidApi.Security.JKeyGenParameterSpec));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Security.JKeyGenParameterSpec_Builder', TypeInfo(Alcinoe.AndroidApi.Security.JKeyGenParameterSpec_Builder));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Security.JKeyProperties', TypeInfo(Alcinoe.AndroidApi.Security.JKeyProperties));
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.AndroidApi.Security','initialization');
  {$ENDIF}
  RegisterTypes;

end.
