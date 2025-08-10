unit Alcinoe.AndroidApi.Crypto;

interface

{$I Alcinoe.inc}

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Java.Security;

type

  {****************************}
  JGCMParameterSpec = interface;
  JSecretKey = interface;
  JKeyGenerator = interface;
  JALCipher = interface;

  {**************************************************************}
  JGCMParameterSpecClass = interface(JAlgorithmParameterSpecClass)
    ['{2AD30FD4-566F-4DC2-9D2F-E086E01F6FFC}']
    {class} function init(tLen: integer; src: TJavaArray<Byte>): JGCMParameterSpec; cdecl;
  end;
  [JavaSignature('javax/crypto/spec/GCMParameterSpec')]
  JGCMParameterSpec = interface(JAlgorithmParameterSpec)
    ['{F498AE5A-0086-4B68-BF27-CE2B061A743B}']
  end;
  TJGCMParameterSpec = class(TJavaGenericImport<JGCMParameterSpecClass, JGCMParameterSpec>) end;

  {************************************}
  JSecretKeyClass = interface(JKeyClass)
    ['{8D9DCE5D-BA44-4C56-8B6C-9AAA7F439156}']
  end;
  [JavaSignature('javax/crypto/SecretKey')]
  JSecretKey = interface(JKey)
    ['{51C30A5D-47E8-47B8-BF5C-EACF35E548E6}']
  end;
  TJSecretKey = class(TJavaGenericImport<JSecretKeyClass, JSecretKey>) end;

  {******************************************}
  JKeyGeneratorClass = interface(JObjectClass)
    ['{8666A931-03C6-4E81-858D-DC962079A3F8}']
    {class} function getInstance(algorithm: JString): JKeyGenerator; cdecl; overload;
    {class} function getInstance(algorithm: JString; provider: JString): JKeyGenerator; cdecl; overload;
  end;
  [JavaSignature('javax/crypto/KeyGenerator')]
  JKeyGenerator = interface(JObject)
    ['{7E22026B-B28F-45F8-A11A-443EB87BFA31}']
    //procedure init(random: JSecureRandom); cdecl; overload;
    procedure init(params: JAlgorithmParameterSpec); cdecl; overload;
    //procedure init(params: JAlgorithmParameterSpec; random: JSecureRandom); cdecl; overload;
    procedure init(keysize: integer); cdecl; overload;
    //procedure init(keysize: integer; random: JSecureRandom); cdecl; overload;
    function generateKey: JSecretKey; cdecl;
  end;
  TJKeyGenerator = class(TJavaGenericImport<JKeyGeneratorClass, JKeyGenerator>) end;

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-3921 has been resolved. If resolved, remove the class definition below.'}
  {$ENDIF}
  JALCipherClass = interface(JCipherClass)
    ['{453AB42D-7678-4E6C-ACF7-5C3D111A122F}']
  end;
  [JavaSignature('javax/crypto/Cipher')]
  JALCipher = interface(JCipher)
    ['{46327FEB-AF73-4AAA-9B3F-29C25DD15143}']
    procedure init(opmode: Integer; key: JKey); cdecl; overload;
    procedure init(opmode: Integer; key: JKey; params: JAlgorithmParameterSpec); cdecl; overload;
  end;
  TJALCipher = class(TJavaGenericImport<JALCipherClass, JALCipher>) end;

implementation

uses
  Alcinoe.Common;

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Crypto.JGCMParameterSpec', TypeInfo(Alcinoe.AndroidApi.Crypto.JGCMParameterSpec));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Crypto.JSecretKey', TypeInfo(Alcinoe.AndroidApi.Crypto.JSecretKey));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Crypto.JKeyGenerator', TypeInfo(Alcinoe.AndroidApi.Crypto.JKeyGenerator));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Crypto.JALCipher', TypeInfo(Alcinoe.AndroidApi.Crypto.JALCipher));
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.AndroidApi.Crypto','initialization');
  {$ENDIF}
  RegisterTypes;

end.
