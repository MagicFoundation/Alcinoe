unit Alcinoe.Cipher;

interface

{$I Alcinoe.inc}

uses
  system.sysutils,
  {$IF not defined(MSWINDOWS)}
  system.types,
  {$ENDIF}
  system.hash,
  Alcinoe.Common;

/////////////////
////// MD5 //////
/////////////////

type
  TALMD5Digest  = array [0..15] of Byte;         { 128 bits - MD5 }

function ALHashMD5AsBytes(const AData: TBytes): TBytes; inline; overload;
function ALHashMD5AsBytes(const AData: AnsiString): TBytes; inline; overload;
function ALHashMD5AsBytes(const AData: String; const AEncoding: TEncoding): TBytes; inline; overload;
function ALHashMD5AsDigest(const AData: TBytes): TALMD5Digest; inline; overload;
function ALHashMD5AsDigest(const AData: AnsiString): TALMD5Digest; inline; overload;
function ALHashMD5AsDigest(const AData: String; const AEncoding: TEncoding): TALMD5Digest; inline; overload;
function ALHashMD5AsStringA(const AData: TBytes; const AHexEncode: boolean = true): AnsiString; inline; overload;
function ALHashMD5AsStringA(const AData: AnsiString; const AHexEncode: boolean = true): AnsiString; inline; overload;
function ALHashMD5AsStringW(const AData: TBytes): String; inline; overload;
function ALHashMD5AsStringW(const AData: String; const AEncoding: TEncoding): String; inline; overload; // result will be hexencoded


//////////////////
////// SHA1 //////
//////////////////

type
  TALSHA1Digest = array [0..19] of Byte;         { 160 bits - SHA-1 }

function ALHashSHA1AsBytes(const AData: TBytes): TBytes; inline; overload;
function ALHashSHA1AsBytes(const AData: AnsiString): TBytes; inline; overload;
function ALHashSHA1AsBytes(const AData: String; const AEncoding: TEncoding): TBytes; inline; overload;
function ALHashSHA1AsDigest(const AData: TBytes): TALSHA1Digest; inline; overload;
function ALHashSHA1AsDigest(const AData: AnsiString): TALSHA1Digest; inline; overload;
function ALHashSHA1AsDigest(const AData: String; const AEncoding: TEncoding): TALSHA1Digest; inline; overload;
function ALHashSHA1AsStringA(const AData: TBytes; const AHexEncode: boolean = true): AnsiString; inline; overload;
function ALHashSHA1AsStringA(const AData: AnsiString; const AHexEncode: boolean = true): AnsiString; inline; overload;
function ALHashSHA1AsStringW(const AData: TBytes): String; inline; overload;
function ALHashSHA1AsStringW(const AData: String; const AEncoding: TEncoding): String; inline; overload; // result will be hexencoded


//////////////////
////// SHA2 //////
//////////////////

function ALHashSHA2AsBytes(const AData: TBytes; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256): TBytes; inline; overload;
function ALHashSHA2AsBytes(const AData: AnsiString; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256): TBytes; inline; overload;
function ALHashSHA2AsBytes(const AData: String; const AEncoding: TEncoding; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256): TBytes; inline; overload;
function ALHashSHA2AsStringA(const AData: TBytes; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256; const AHexEncode: boolean = true): AnsiString; inline; overload;
function ALHashSHA2AsStringA(const AData: AnsiString; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256; const AHexEncode: boolean = true): AnsiString; inline; overload;
function ALHashSHA2AsStringW(const AData: TBytes; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256): String; inline; overload;
function ALHashSHA2AsStringW(const AData: String; const AEncoding: TEncoding; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256): String; inline; overload; // result will be hexencoded


/////////////////////////////
////// HMAC algorithms //////
/////////////////////////////

function ALCalcHMACMD5AsBytes(const AData: TBytes; const AKey: TBytes): TBytes; inline; overload;
function ALCalcHMACMD5AsBytes(const AData: AnsiString; const AKey: TBytes): TBytes; inline; overload;
function ALCalcHMACMD5AsBytes(const AData, AKey: AnsiString): TBytes; inline; overload;

function ALCalcHMACMD5AsStringA(const AData: TBytes; const AKey: TBytes; const AHexEncode: boolean = true): AnsiString; inline; overload;
function ALCalcHMACMD5AsStringA(const AData: AnsiString; const AKey: TBytes; const AHexEncode: boolean = true): AnsiString; inline; overload;
function ALCalcHMACMD5AsStringA(const AData, AKey: AnsiString; const AHexEncode: boolean = true): AnsiString; inline; overload;

function ALCalcHMACSHA1AsBytes(const AData: TBytes; const AKey: TBytes): TBytes; inline; overload;
function ALCalcHMACSHA1AsBytes(const AData: AnsiString; const AKey: TBytes): TBytes; inline; overload;
function ALCalcHMACSHA1AsBytes(const AData, AKey: AnsiString): TBytes; inline; overload;

function ALCalcHMACSHA1AsStringA(const AData: TBytes; const AKey: TBytes; const AHexEncode: boolean = true): AnsiString; inline; overload;
function ALCalcHMACSHA1AsStringA(const AData: AnsiString; const AKey: TBytes; const AHexEncode: boolean = true): AnsiString; inline; overload;
function ALCalcHMACSHA1AsStringA(const AData, AKey: AnsiString; const AHexEncode: boolean = true): AnsiString; inline; overload;

function ALCalcHMACSHA2AsBytes(const AData: TBytes; const AKey: TBytes; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256): TBytes; inline; overload;
function ALCalcHMACSHA2AsBytes(const AData: AnsiString; const AKey: TBytes; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256): TBytes; inline; overload;
function ALCalcHMACSHA2AsBytes(const AData, AKey: AnsiString; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256): TBytes; inline; overload;

function ALCalcHMACSHA2AsStringA(const AData: TBytes; const AKey: TBytes; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256; const AHexEncode: boolean = true): AnsiString; inline; overload;
function ALCalcHMACSHA2AsStringA(const AData: AnsiString; const AKey: TBytes; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256; const AHexEncode: boolean = true): AnsiString; inline; overload;
function ALCalcHMACSHA2AsStringA(const AData, AKey: AnsiString; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256; const AHexEncode: boolean = true): AnsiString; inline; overload;


//////////////////////////
////// Random bytes //////
//////////////////////////

{$IFDEF MSWINDOWS}
procedure ALRandomBytes(const ADest; const ALen: Cardinal); overload;
function ALRandomBytes(const ALen: Cardinal): TBytes; overload;
function ALRandomByteStr(const ALen: Cardinal): AnsiString;
{$ENDIF}
var ALRandom32: function(const ARange: Cardinal): cardinal;
var ALRandom64: function(const ARange: UInt64): UInt64;


///////////////////
////// Fnv1a //////
///////////////////

function ALFnv1aInt32(const AStr: AnsiString): int32; overload; inline;
function ALFnv1aInt32(const AStr: String; const AEncoding: TEncoding): int32; overload; inline;
function ALFnv1aInt64(const AStr: AnsiString): Int64; overload; inline;
function ALFnv1aInt64(const AStr: String; const AEncoding: TEncoding): Int64; overload; inline;


///////////////////////
////// Signature //////
///////////////////////

{$IF defined(MSWINDOWS)}
function ALVerifyRS256Signature(
           const AData: TBytes;
           const ASignature: TBytes;
           const APubKeyModulus: TBytes;
           const APubKeyExponent: TBytes): boolean;
function ALRS256Sign(
           const AData: TBytes;
           const APemPrivateKey: AnsiString): TBytes;
{$ENDIF}


////////////////////
////// OAuth2 //////
////////////////////

{$IF defined(MSWINDOWS)}
function ALGenerateGoogleOAuth2AccessToken(
           const AServiceAccountEmail: AnsiString;
           const AScope: AnsiString;
           const APrivateKey: AnsiString): AnsiString;
{$ENDIF}


//////////////////////////////////////
////// Windows Credential Vault //////
//////////////////////////////////////

{$IF defined(MSWINDOWS)}
function ALTryGetGenericWinCredential(
           const ATargetName: AnsiString;
           out AUserName: AnsiString;
           out APassword: AnsiString): Boolean;
procedure ALGetGenericWinCredential(
            const ATargetName: AnsiString;
            out AUserName: AnsiString;
            out APassword: AnsiString);
{$ENDIF}

implementation

uses
  {$IF defined(MSWINDOWS)}
  WinApi.windows,
  WinApi.WinCred,
  System.DateUtils,
  System.AnsiStrings,
  System.Generics.Collections,
  System.SyncObjs,
  System.Math,
  Alcinoe.WinApi.Windows,
  Alcinoe.WinApi.WinCrypt,
  Alcinoe.WinApi.BCrypt,
  Alcinoe.HTTP.Client.WinHTTP,
  Alcinoe.JSONDoc,
  Alcinoe.HTML,
  {$ENDIF}
  System.Classes,
  Alcinoe.StringList,
  Alcinoe.StringUtils;


/////////////////
////// MD5 //////
/////////////////

{*****************************************************}
function ALHashMD5AsBytes(const AData: TBytes): TBytes;
begin
  var LMD5 := THashMD5.Create;
  LMD5.Update(AData);
  Result := LMD5.HashAsBytes;
end;

{*********************************************************}
function ALHashMD5AsBytes(const AData: AnsiString): TBytes;
begin
  var LMD5 := THashMD5.Create;
  if AData <> '' then LMD5.Update(pointer(AData)^, length(AData));
  Result := LMD5.HashAsBytes;
end;

{*********************************************************************************}
function ALHashMD5AsBytes(const AData: String; const AEncoding: TEncoding): TBytes;
begin
  var LMD5 := THashMD5.Create;
  LMD5.Update(AEncoding.GetBytes(AData));
  Result := LMD5.HashAsBytes;
end;

{************************************************************}
function ALHashMD5AsDigest(const AData: TBytes): TALMD5Digest;
begin
  var LBytes := ALHashMD5AsBytes(AData);
  ALMove(PByte(LBytes)^, Result[0], length(LBytes));
end;

{****************************************************************}
function ALHashMD5AsDigest(const AData: AnsiString): TALMD5Digest;
begin
  var LBytes := ALHashMD5AsBytes(AData);
  ALMove(PByte(LBytes)^, Result[0], length(LBytes));
end;

{****************************************************************************************}
function ALHashMD5AsDigest(const AData: String; const AEncoding: TEncoding): TALMD5Digest;
begin
  var LBytes := ALHashMD5AsBytes(AData, AEncoding);
  ALMove(PByte(LBytes)^, Result[0], length(LBytes));
end;

{*********************************************************************************************}
function ALHashMD5AsStringA(const AData: TBytes; const AHexEncode: boolean = true): AnsiString;
begin
  var LBytes := ALHashMD5AsBytes(AData);
  if AHexEncode then result := ALBinToHexA(LBytes, True{ALowerCase})
  else Result := ALBytesToString(LBytes);
end;

{*************************************************************************************************}
function ALHashMD5AsStringA(const AData: AnsiString; const AHexEncode: boolean = true): AnsiString;
begin
  var LBytes := ALHashMD5AsBytes(AData);
  if AHexEncode then result := ALBinToHexA(LBytes, True{ALowerCase})
  else Result := ALBytesToString(LBytes);
end;

{*******************************************************}
function ALHashMD5AsStringW(const AData: TBytes): String;
begin
  var LBytes := ALHashMD5AsBytes(AData);
  Result := ALBinToHexW(LBytes, True{ALowerCase});
end;

{***********************************************************************************}
function ALHashMD5AsStringW(const AData: String; const AEncoding: TEncoding): String;
begin
  var LBytes := ALHashMD5AsBytes(AData, AEncoding);
  Result := ALBinToHexW(LBytes, True{ALowerCase});
end;


//////////////////
////// SHA1 //////
//////////////////

{******************************************************}
function ALHashSHA1AsBytes(const AData: TBytes): TBytes;
begin
  var LSHA1 := THashSHA1.Create;
  LSHA1.Update(AData);
  Result := LSHA1.HashAsBytes;
end;

{**********************************************************}
function ALHashSHA1AsBytes(const AData: AnsiString): TBytes;
begin
  var LSHA1 := THashSHA1.Create;
  if AData <> '' then LSHA1.Update(pointer(AData)^, length(AData));
  Result := LSHA1.HashAsBytes;
end;

{**********************************************************************************}
function ALHashSHA1AsBytes(const AData: String; const AEncoding: TEncoding): TBytes;
begin
  var LSHA1 := THashSHA1.Create;
  LSHA1.Update(AEncoding.GetBytes(AData));
  Result := LSHA1.HashAsBytes;
end;

{**************************************************************}
function ALHashSHA1AsDigest(const AData: TBytes): TALSHA1Digest;
begin
  var LBytes := ALHashSHA1AsBytes(AData);
  ALMove(PByte(LBytes)^, Result[0], length(LBytes));
end;

{******************************************************************}
function ALHashSHA1AsDigest(const AData: AnsiString): TALSHA1Digest;
begin
  var LBytes := ALHashSHA1AsBytes(AData);
  ALMove(PByte(LBytes)^, Result[0], length(LBytes));
end;

{******************************************************************************************}
function ALHashSHA1AsDigest(const AData: String; const AEncoding: TEncoding): TALSHA1Digest;
begin
  var LBytes := ALHashSHA1AsBytes(AData, AEncoding);
  ALMove(PByte(LBytes)^, Result[0], length(LBytes));
end;

{**********************************************************************************************}
function ALHashSHA1AsStringA(const AData: TBytes; const AHexEncode: boolean = true): AnsiString;
begin
  var LBytes := ALHashSHA1AsBytes(AData);
  if AHexEncode then result := ALBinToHexA(LBytes, True{ALowerCase})
  else Result := ALBytesToString(LBytes);
end;

{**************************************************************************************************}
function ALHashSHA1AsStringA(const AData: AnsiString; const AHexEncode: boolean = true): AnsiString;
begin
  var LBytes := ALHashSHA1AsBytes(AData);
  if AHexEncode then result := ALBinToHexA(LBytes, True{ALowerCase})
  else Result := ALBytesToString(LBytes);
end;

{********************************************************}
function ALHashSHA1AsStringW(const AData: TBytes): String;
begin
  var LBytes := ALHashSHA1AsBytes(AData);
  Result := ALBinToHexW(LBytes, True{ALowerCase});
end;

{************************************************************************************}
function ALHashSHA1AsStringW(const AData: String; const AEncoding: TEncoding): String;
begin
  var LBytes := ALHashSHA1AsBytes(AData, AEncoding);
  Result := ALBinToHexW(LBytes, True{ALowerCase});
end;


//////////////////
////// SHA2 //////
//////////////////

{**********************************************************************************************************************************}
function ALHashSHA2AsBytes(const AData: TBytes; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256): TBytes;
begin
  var LSHA2 := THashSHA2.Create(AHashVersion);
  LSHA2.Update(AData);
  Result := LSHA2.HashAsBytes;
end;

{**************************************************************************************************************************************}
function ALHashSHA2AsBytes(const AData: AnsiString; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256): TBytes;
begin
  var LSHA2 := THashSHA2.Create(AHashVersion);
  if AData <> '' then LSHA2.Update(pointer(AData)^, length(AData));
  Result := LSHA2.HashAsBytes;
end;

{**************************************************************************************************************************************************************}
function ALHashSHA2AsBytes(const AData: String; const AEncoding: TEncoding; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256): TBytes;
begin
  var LSHA2 := THashSHA2.Create(AHashVersion);
  LSHA2.Update(AEncoding.GetBytes(AData));
  Result := LSHA2.HashAsBytes;
end;

{**************************************************************************************************************************************************************************}
function ALHashSHA2AsStringA(const AData: TBytes; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256; const AHexEncode: boolean = true): AnsiString;
begin
  var LBytes := ALHashSHA2AsBytes(AData, AHashVersion);
  if AHexEncode then result := ALBinToHexA(LBytes, True{ALowerCase})
  else Result := ALBytesToString(LBytes);
end;

{******************************************************************************************************************************************************************************}
function ALHashSHA2AsStringA(const AData: AnsiString; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256; const AHexEncode: boolean = true): AnsiString;
begin
  var LBytes := ALHashSHA2AsBytes(AData, AHashVersion);
  if AHexEncode then result := ALBinToHexA(LBytes, True{ALowerCase})
  else Result := ALBytesToString(LBytes);
end;

{************************************************************************************************************************************}
function ALHashSHA2AsStringW(const AData: TBytes; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256): String;
begin
  var LBytes := ALHashSHA2AsBytes(AData, AHashVersion);
  Result := ALBinToHexW(LBytes, True{ALowerCase});
end;

{****************************************************************************************************************************************************************}
function ALHashSHA2AsStringW(const AData: String; const AEncoding: TEncoding; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256): String;
begin
  var LBytes := ALHashSHA2AsBytes(AData, AEncoding, AHashVersion);
  Result := ALBinToHexW(LBytes, True{ALowerCase});
end;


/////////////////////////////
////// HMAC algorithms //////
/////////////////////////////

{*****************************************************************************}
function ALCalcHMACMD5AsBytes(const AData: TBytes; const AKey: TBytes): TBytes;
begin
  Result := THashMD5.GetHMACAsBytes(AData, AKey);
end;

{*********************************************************************************}
function ALCalcHMACMD5AsBytes(const AData: AnsiString; const AKey: TBytes): TBytes;
begin
  Result := THashMD5.GetHMACAsBytes(ALStringToBytes(AData), AKey);
end;

{*******************************************************************}
function ALCalcHMACMD5AsBytes(const AData, AKey: AnsiString): TBytes;
begin
  Result := THashMD5.GetHMACAsBytes(ALStringToBytes(AData), ALStringToBytes(AKey));
end;

{*********************************************************************************************************************}
function ALCalcHMACMD5AsStringA(const AData: TBytes; const AKey: TBytes; const AHexEncode: boolean = true): AnsiString;
begin
  var LMac := ALCalcHMACMD5AsBytes(AData, AKey);
  if AHexEncode then Result := ALBinToHexA(LMac, True{ALowerCase})
  else Result := ALBytesToString(LMac);
end;

{*************************************************************************************************************************}
function ALCalcHMACMD5AsStringA(const AData: AnsiString; const AKey: TBytes; const AHexEncode: boolean = true): AnsiString;
begin
  var LMac := ALCalcHMACMD5AsBytes(AData, AKey);
  if AHexEncode then Result := ALBinToHexA(LMac, True{ALowerCase})
  else Result := ALBytesToString(LMac);
end;

{***********************************************************************************************************}
function ALCalcHMACMD5AsStringA(const AData, AKey: AnsiString; const AHexEncode: boolean = true): AnsiString;
begin
  var LMac := ALCalcHMACMD5AsBytes(AData, AKey);
  if AHexEncode then Result := ALBinToHexA(LMac, True{ALowerCase})
  else Result := ALBytesToString(LMac);
end;

{******************************************************************************}
function ALCalcHMACSHA1AsBytes(const AData: TBytes; const AKey: TBytes): TBytes;
begin
  Result := THashSHA1.GetHMACAsBytes(AData, AKey);
end;

{**********************************************************************************}
function ALCalcHMACSHA1AsBytes(const AData: AnsiString; const AKey: TBytes): TBytes;
begin
  Result := THashSHA1.GetHMACAsBytes(ALStringToBytes(AData), AKey);
end;

{********************************************************************}
function ALCalcHMACSHA1AsBytes(const AData, AKey: AnsiString): TBytes;
begin
  Result := THashSHA1.GetHMACAsBytes(ALStringToBytes(AData), ALStringToBytes(AKey));
end;

{**********************************************************************************************************************}
function ALCalcHMACSHA1AsStringA(const AData: TBytes; const AKey: TBytes; const AHexEncode: boolean = true): AnsiString;
begin
  var LMac := ALCalcHMACSHA1AsBytes(AData, AKey);
  if AHexEncode then Result := ALBinToHexA(LMac, True{ALowerCase})
  else Result := ALBytesToString(LMac);
end;

{**************************************************************************************************************************}
function ALCalcHMACSHA1AsStringA(const AData: AnsiString; const AKey: TBytes; const AHexEncode: boolean = true): AnsiString;
begin
  var LMac := ALCalcHMACSHA1AsBytes(AData, AKey);
  if AHexEncode then Result := ALBinToHexA(LMac, True{ALowerCase})
  else Result := ALBytesToString(LMac);
end;

{************************************************************************************************************}
function ALCalcHMACSHA1AsStringA(const AData, AKey: AnsiString; const AHexEncode: boolean = true): AnsiString;
begin
  var LMac := ALCalcHMACSHA1AsBytes(AData, AKey);
  if AHexEncode then Result := ALBinToHexA(LMac, True{ALowerCase})
  else Result := ALBytesToString(LMac);
end;

{**********************************************************************************************************************************************************}
function ALCalcHMACSHA2AsBytes(const AData: TBytes; const AKey: TBytes; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256): TBytes;
begin
  Result := THashSHA2.GetHMACAsBytes(AData, AKey, AHashVersion);
end;

{**************************************************************************************************************************************************************}
function ALCalcHMACSHA2AsBytes(const AData: AnsiString; const AKey: TBytes; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256): TBytes;
begin
  Result := THashSHA2.GetHMACAsBytes(ALStringToBytes(AData), AKey, AHashVersion);
end;

{************************************************************************************************************************************************}
function ALCalcHMACSHA2AsBytes(const AData, AKey: AnsiString; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256): TBytes;
begin
  Result := THashSHA2.GetHMACAsBytes(ALStringToBytes(AData), ALStringToBytes(AKey), AHashVersion);
end;

{**************************************************************************************************************************************************************************************************}
function ALCalcHMACSHA2AsStringA(const AData: TBytes; const AKey: TBytes; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256; const AHexEncode: boolean = true): AnsiString;
begin
  var LMac := ALCalcHMACSHA2AsBytes(AData, AKey, AHashVersion);
  if AHexEncode then Result := ALBinToHexA(LMac, True{ALowerCase})
  else Result := ALBytesToString(LMac);
end;

{******************************************************************************************************************************************************************************************************}
function ALCalcHMACSHA2AsStringA(const AData: AnsiString; const AKey: TBytes; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256; const AHexEncode: boolean = true): AnsiString;
begin
  var LMac := ALCalcHMACSHA2AsBytes(AData, AKey, AHashVersion);
  if AHexEncode then Result := ALBinToHexA(LMac, True{ALowerCase})
  else Result := ALBytesToString(LMac);
end;

{****************************************************************************************************************************************************************************************}
function ALCalcHMACSHA2AsStringA(const AData, AKey: AnsiString; const AHashVersion: THashSHA2.TSHA2Version = THashSHA2.TSHA2Version.SHA256; const AHexEncode: boolean = true): AnsiString;
begin
  var LMac := ALCalcHMACSHA2AsBytes(AData, AKey, AHashVersion);
  if AHexEncode then Result := ALBinToHexA(LMac, True{ALowerCase})
  else Result := ALBytesToString(LMac);
end;


//////////////////////////
////// Random bytes //////
//////////////////////////

{$IFDEF MSWINDOWS}

{*********************************************************}
procedure ALRandomBytes(const ADest; const ALen: Cardinal);
begin
  if ALen = 0 then exit;
  var hProv: HCRYPTPROV;
  if (not CryptAcquireContextA(
            @hProv,
            nil,
            nil,
            PROV_RSA_FULL,
            CRYPT_VERIFYCONTEXT or CRYPT_SILENT)) then raiselastOsError;
  try
    if not CryptGenRandom(hProv,ALen,@ADest) then raiselastOsError;
  finally
    if not CryptReleaseContext(hProv,0) then raiseLastOsError;
  end;
end;

{***************************************************}
function ALRandomBytes(const ALen: Cardinal): TBytes;
begin
  if ALen = 0 then begin
    SetLength(Result,0);
    exit;
  end;
  var hProv: HCRYPTPROV;
  if (not CryptAcquireContextA(
            @hProv,
            nil,
            nil,
            PROV_RSA_FULL,
            CRYPT_VERIFYCONTEXT or CRYPT_SILENT)) then raiselastOsError;
  try
    SetLength(Result,ALen);
    if not CryptGenRandom(hProv,ALen,@Result[0]) then raiselastOsError;
  finally
    if not CryptReleaseContext(hProv,0) then raiseLastOsError;
  end;
end;

{*********************************************************}
function ALRandomByteStr(const ALen: Cardinal): AnsiString;
begin
  if ALen = 0 then exit('');
  var hProv: HCRYPTPROV;
  if (not CryptAcquireContextA(
            @hProv,
            nil,
            nil,
            PROV_RSA_FULL,
            CRYPT_VERIFYCONTEXT or CRYPT_SILENT)) then raiselastOsError;
  try
    SetLength(Result,ALen);
    if not CryptGenRandom(hProv,ALen,@Result[low(result)]) then raiselastOsError;
  finally
    if not CryptReleaseContext(hProv,0) then raiseLastOsError;
  end;
end;

{$ENDIF MSWINDOWS}

{$IFDEF ALCPUXASM}

{*******************************************************************************************************************}
// https://software.intel.com/en-us/articles/intel-digital-random-number-generator-drng-software-implementation-guide
function _TryRdRand32(out Value: Cardinal): Boolean;
asm
{$if defined(CPUX64)}
  .noframe
  // rdrand eax
  db   $0f
  db   $c7
  db   $f0
  jnc  @fail
  mov  [rcx],eax
{$elseif defined(CPUX86)}
  // rdrand ecx
  db   $0f
  db   $c7
  db   $f1
  jnc  @fail
  mov  [eax],ecx
{$else}
  {$Message Fatal 'TryRdRand32 not implemented for this platform'}
{$endif}
  mov  eax,1
  ret
@fail:
  xor  eax,eax
end;

{*******************************************************************************************************************}
// https://software.intel.com/en-us/articles/intel-digital-random-number-generator-drng-software-implementation-guide
{$if defined(CPUX64)}
function _TryRdRand64(out Value: UInt64): Boolean;
asm
  .noframe
  // rdrand rax
  db   $48  // REX.W = 1
  db   $0f
  db   $c7
  db   $f0
  jnc  @fail
  mov  [rcx],rax
  mov  eax,1
  ret
@fail:
  xor  eax,eax
end;
{$endif}

{***********************************************************}
function ALRandom32_RdRand(const ARange: Cardinal): cardinal;
begin
  //https://software.intel.com/content/www/us/en/develop/articles/intel-digital-random-number-generator-drng-software-implementation-guide.html
  //It is recommended that applications attempt 10 retries in a tight
  //loop in the unlikely event that the RDRAND instruction does not return a
  //random number. This number is based on a binomial probability argument: given
  //the design margins of the DRNG, the odds of ten failures in a row are
  //astronomically small and would in fact be an indication of a larger CPU issue.
  for var I := 1 to 10 do
    if _TryRdRand32(result) then
      exit(Result mod ARange);
  raise Exception.Create('RDRand failed!');
end;

{*******************************************************}
function ALRandom64_RdRand(const ARange: UInt64): UInt64;
begin
  //https://software.intel.com/content/www/us/en/develop/articles/intel-digital-random-number-generator-drng-software-implementation-guide.html
  //It is recommended that applications attempt 10 retries in a tight
  //loop in the unlikely event that the RDRAND instruction does not return a
  //random number. This number is based on a binomial probability argument: given
  //the design margins of the DRNG, the odds of ten failures in a row are
  //astronomically small and would in fact be an indication of a larger CPU issue.
  {$if defined(CPUX64)}
  for var I := 1 to 10 do
    if _TryRdRand64(result) then
      exit(Result mod ARange);
  {$ELSE}
  Result := (UInt64(ALRandom32_RdRand(ALMAXUInt)) shl 32) or ((UInt64(ALRandom32_RdRand(ALMAXUInt)) shl 32) shr 32);
  exit(result mod ARange);
  {$ENDIF}
  raise Exception.Create('RDRand failed!');
end;

{$ENDIF ALCPUXASM}

{************************************************************}
function ALRandom32_Default(const ARange: Cardinal): cardinal;
begin
  {$IFDEF MSWINDOWS}
  var LBytes := ALRandomBytes(sizeOf(result));
  ALMove(LBytes[0],result,sizeOf(result));
  result := result mod ARange;
  {$ELSE}
  result := cardinal(Random(integer(ARange)));
  {$ENDIF}
end;

{********************************************************}
function ALRandom64_Default(const ARange: UInt64): UInt64;
begin
  {$IFDEF MSWINDOWS}
  var LBytes := ALRandomBytes(sizeOf(result));
  ALMove(LBytes[0],result,sizeOf(result));
  result := result mod ARange;
  {$ELSE}
  Result := (UInt64(Random(ALMAXInt)) shl 32) or ((UInt64(Random(ALMAXInt)) shl 32) shr 32);
  result := result mod ARange;
  {$ENDIF}
end;


///////////////////
////// Fnv1a //////
///////////////////

{***********************}
{$Q-} {Overflow Checking}
//http://programmers.stackexchange.com/questions/49550/which-hashing-algorithm-is-best-for-uniqueness-and-speed
//https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
//http://www.isthe.com/chongo/tech/comp/fnv/index.html#FNV-param
function ALFnv1aInt64(const AStr: AnsiString): Int64;
begin
   Result := Int64(14695981039346656037);
   for var i := low(AStr) to high(AStr) do
      Result := (Result xor Ord(AStr[i])) * 1099511628211;
end;
{$IF defined(ALOverflowCheckingON)}
  {$Q+} {Overflow Checking}
{$ENDIF}

{***********************}
{$Q-} {Overflow Checking}
function ALFnv1aInt64(const AStr: String; const AEncoding: TEncoding): Int64;
begin
  var LBytes := AEncoding.GetBytes(AStr);
  Result := Int64(14695981039346656037);
  for var i := low(LBytes) to high(LBytes) do
    Result := (Result xor LBytes[i]) * 10995116282118;
end;
{$IF defined(ALOverflowCheckingON)}
  {$Q+} {Overflow Checking}
{$ENDIF}

{***********************}
{$Q-} {Overflow Checking}
function ALFnv1aInt32(const AStr: AnsiString): int32;
begin
   Result := int32(2166136261);
   for var i := low(AStr) to high(AStr) do
      Result := (Result xor Ord(AStr[i])) * 16777619;
end;
{$IF defined(ALOverflowCheckingON)}
  {$Q+} {Overflow Checking}
{$ENDIF}

{***********************}
{$Q-} {Overflow Checking}
function ALFnv1aInt32(const AStr: String; const AEncoding: TEncoding): int32;
begin
  var LBytes := AEncoding.GetBytes(AStr);
  Result := int32(2166136261);
  for var i := low(LBytes) to high(LBytes) do
    Result := (Result xor LBytes[i]) * 16777619;
end;
{$IF defined(ALOverflowCheckingON)}
  {$Q+} {Overflow Checking}
{$ENDIF}


///////////////////////
////// Signature //////
///////////////////////

{**********************}
{$IF defined(MSWINDOWS)}
function ALVerifyRS256Signature(
           const AData: TBytes;
           const ASignature: TBytes;
           const APubKeyModulus: TBytes;
           const APubKeyExponent: TBytes): boolean;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure BigEndianToLittleEndian(var AArr: TBytes);
  begin
    var J: integer := high(AArr);
    var i: integer := low(AArr);
    while i < J do begin
      var B: Byte := AArr[i];
      AArr[i] := AArr[j];
      AArr[j] := B;
      Dec(j);
      inc(i);
    end;
  end;

begin

  if Length(AData) = 0 then raise Exception.Create('Cannot verify RS256 signature: data is empty.');
  if Length(ASignature) = 0 then raise Exception.Create('Cannot verify RS256 signature: signature is empty.');
  if Length(APubKeyModulus) = 0 then raise Exception.Create('Cannot verify RS256 signature: RSA public modulus is empty.');
  if Length(APubKeyExponent) = 0 then raise Exception.Create('Cannot verify RS256 signature: RSA public exponent is empty.');

  var cbModulus: DWORD := Length(APubKeyModulus);
  var cbExponent: DWORD := Length(APubKeyExponent);

  if Length(ASignature) <> Integer(cbModulus) then
    Exit(False);

  var pModulus := Copy(APubKeyModulus);
  BigEndianToLittleEndian(pModulus);

  var pExponent := Copy(APubKeyExponent);
  BigEndianToLittleEndian(pExponent);

  if cbExponent > SizeOf(DWORD) then
    raise Exception.Create('Cannot verify RS256 signature: RSA public exponent is larger than 32 bits.');

  var dwExponent: DWORD := 0;
  ALMove(pExponent[0], dwExponent, cbExponent);

  var hProv: HCRYPTPROV;
  ALCheckWinApiBoolean(
    'CryptAcquireContextA',
    CryptAcquireContextA(
      @hProv, // phProv: PHCRYPTPROV;
      nil, // pszContainer: PAnsiChar;
      nil, // pszProvider: PAnsiChar;
      PROV_RSA_AES, // dwProvType: DWORD;
      CRYPT_VERIFYCONTEXT)); // dwFlags: DWORD
  try

    // create the pKeyBlob
    // The data format is: PUBLICKEYSTRUC + RSAPUBKEY + key
    var cbKeyBlob: DWord := sizeof(PUBLICKEYSTRUC) + sizeof(RSAPUBKEY) + cbModulus;
    var pKeyBlob: Tbytes;
    setlength(pKeyBlob, cbKeyBlob);

    // Fill in the PUBLICKEYSTRUC
    var pPublicKey: PUBLICKEYSTRUC;
    pPublicKey.bType := PUBLICKEYBLOB;
    pPublicKey.bVersion := CUR_BLOB_VERSION;  // Always use this value.
    pPublicKey.reserved := 0;                 // Must be zero.
    pPublicKey.aiKeyAlg := CALG_RSA_SIGN;     // RSA signature verification.
    ALMove(pPublicKey,pKeyBlob[0],sizeof(PUBLICKEYSTRUC));

    // Fill in the RSAPUBKEY
    var pRsaPubKey: RSAPUBKEY;
    pRsaPubKey.magic := BCRYPT_RSAPUBLIC_MAGIC;            // Public key.
    pRsaPubKey.bitlen := cbModulus * 8;  // Number of bits in the modulus.
    pRsaPubKey.pubexp := dwExponent;     // Exponent.
    ALMove(pRsaPubKey,pKeyBlob[sizeof(PUBLICKEYSTRUC)],sizeof(RSAPUBKEY));

    // Fill in the modulus
    ALMove(pModulus[0],pKeyBlob[sizeof(PUBLICKEYSTRUC)+sizeof(RSAPUBKEY)],cbModulus);

    // Now import the key.
    var hRSAKey: HCRYPTKEY;
    ALCheckWinApiBoolean(
      'CryptImportKey',
      CryptImportKey(
        hProv, // hProv: HCRYPTPROV;
        @pKeyBlob[0], // const pbData: PBYTE;
        cbKeyBlob, // dwDataLen: DWORD;
        0, // hPubKey: HCRYPTKEY;
        0, // dwFlags: DWORD;
        @hRSAKey)); // phKey: PHCRYPTKEY
    try

      //initiates the hashing of a stream of data.
      var hHash: HCRYPTHASH;
      ALCheckWinApiBoolean(
        'CryptCreateHash',
        CryptCreateHash(
          hProv, // hProv: HCRYPTPROV;
          CALG_SHA_256, // Algid: ALG_ID;
          0, // hKey: HCRYPTKEY;
          0, // dwFlags: DWORD;
          @hHash));
      try

        //adds data to a specified hash object.
        ALCheckWinApiBoolean(
          'CryptHashData',
          CryptHashData(
            hHash, // hHash: HCRYPTHASH;
            pbyte(AData), // const pbData: PBYTE;
            length(AData), // dwDataLen: DWORD;
            0)); // dwFlags: DWORD

        //verifies the signature
        var pSignature := copy(ASignature);
        BigEndianToLittleEndian(pSignature);
        if not CryptVerifySignatureA(
                 hHash, // hHash: HCRYPTHASH;
                 @pSignature[0], // const pbSignature: PBYTE;
                 length(pSignature), // dwSigLen: DWORD;
                 hRSAKey, // hPubKey: HCRYPTKEY;
                 nil, // const sDescription: LPCSTR;
                 0) then begin // dwFlags: DWORD)
          if HRESULT(GetLastError) = NTE_BAD_SIGNATURE then exit(False)
          else raiseLastOsError;
        end;

        //everything is ok
        Result := True;

      finally
        ALCheckWinApiBoolean(
          'CryptDestroyHash',
          CryptDestroyHash(hHash));
      end;

    finally
      ALCheckWinApiBoolean(
        'CryptDestroyKey',
        CryptDestroyKey(hRSAKey));
    end;

  finally
    ALCheckWinApiBoolean(
      'CryptReleaseContext',
      CryptReleaseContext(hProv,0));
  end;

end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function ALRS256Sign(
           const AData: TBytes;
           const APemPrivateKey: AnsiString): TBytes;

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  function TryExtractPemDer(
             const APem: AnsiString;
             const ABeginMarker: AnsiString;
             const AEndMarker: AnsiString;
             out ADer: TBytes): Boolean;
  begin
    var P1 := ALPosA(ABeginMarker, APem);
    if P1 <= 0 then Exit(False);
    Inc(P1, Length(ABeginMarker));
    var P2 := ALPosA(AEndMarker, APem, P1);
    if P2 <= 0 then Exit(False);
    Result := True;
    ADer := ALBase64DecodeBytesMIME(ALCopyStr(APem, P1, P2 - P1));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function DecodePkcs8ToPkcs1Der(const APkcs8Der: TBytes): TBytes;
  begin
    if Length(APkcs8Der) = 0 then raise Exception.Create('PKCS#8 key is empty');

    var LPrivateKeyInfo: PCRYPT_PRIVATE_KEY_INFO := nil;
    var LSize: DWORD := 0;
    ALCheckWinApiBoolean(
      'CryptDecodeObjectEx',
      CryptDecodeObjectEx(
        X509_ASN_ENCODING or PKCS_7_ASN_ENCODING, // dwCertEncodingType: DWORD;
        PKCS_PRIVATE_KEY_INFO, // lpszStructType: LPCSTR;
        @APkcs8Der[0], // pbEncoded: PBYTE;
        Length(APkcs8Der), // cbEncoded: DWORD;
        CRYPT_DECODE_ALLOC_FLAG, // dwFlags: DWORD;
        nil, // pDecodePara: PCRYPT_DECODE_PARA;
        @LPrivateKeyInfo, // pvStructInfo: Pvoid;
        @LSize)); // pcbStructInfo: PDWORD
    if LPrivateKeyInfo = nil then raise Exception.Create('Failed to decode PKCS#8 private key');
    try

      if not ALSameTextA(LPrivateKeyInfo.Algorithm.pszObjId, szOID_RSA_RSA) then
        raise Exception.CreateFmt('Unsupported private key algorithm OID: %s', [AnsiString(LPrivateKeyInfo.Algorithm.pszObjId)]);

      if (LPrivateKeyInfo.PrivateKey.cbData = 0) or
         (LPrivateKeyInfo.PrivateKey.pbData = nil) then
        raise Exception.Create('PKCS#8 private key does not contain an RSA private key');

      SetLength(Result, LPrivateKeyInfo.PrivateKey.cbData);
      ALMove(LPrivateKeyInfo.PrivateKey.pbData^, Result[0], LPrivateKeyInfo.PrivateKey.cbData);

    finally
      ALCheckWinApiBoolean(
        'LocalFree',
        LocalFree(HLOCAL(LPrivateKeyInfo)) = 0);
    end;
  end;

begin

  if APemPrivateKey = '' then raise Exception.Create('Private key is empty');
  if length(AData) = 0 then raise Exception.Create('Data to sign is empty');

  var LPKCS1Der: TBytes;
  var LKeyDer: TBytes;

  // PKCS#8
  if TryExtractPemDer(
       APemPrivateKey,
       '-----BEGIN PRIVATE KEY-----',
       '-----END PRIVATE KEY-----',
       LKeyDer) then LPKCS1Der := DecodePkcs8ToPkcs1Der(LKeyDer)

  // PKCS#1
  else if not TryExtractPemDer(
                APemPrivateKey,
                '-----BEGIN RSA PRIVATE KEY-----',
                '-----END RSA PRIVATE KEY-----',
                LPKCS1Der) then
    raise Exception.Create('Unsupported PEM format. Expected BEGIN PRIVATE KEY or BEGIN RSA PRIVATE KEY');

  if Length(LPKCS1Der) = 0 then
    raise Exception.Create('Decoded RSA private key is empty');

  // PKCS#1 DER -> CryptoAPI PRIVATEKEYBLOB
  var LKeyBlob: TBytes;
  var LKeyBlobSize: DWORD := 0;
  ALCheckWinApiBoolean(
    'CryptDecodeObjectEx',
    CryptDecodeObjectEx(
      X509_ASN_ENCODING or PKCS_7_ASN_ENCODING, // dwCertEncodingType: DWORD;
      PKCS_RSA_PRIVATE_KEY, // lpszStructType: LPCSTR;
      @LPKCS1Der[0], // pbEncoded: PBYTE;
      Length(LPKCS1Der), // cbEncoded: DWORD;
      0, // dwFlags: DWORD;
      nil, // pDecodePara: PCRYPT_DECODE_PARA;
      nil, // pvStructInfo: Pvoid;
      @LKeyBlobSize)); // pcbStructInfo: PDWORD
  if LKeyBlobSize = 0 then raise Exception.Create('Failed to decode RSA private key blob size');

  SetLength(LKeyBlob, LKeyBlobSize);
  ALCheckWinApiBoolean(
    'CryptDecodeObjectEx',
    CryptDecodeObjectEx(
      X509_ASN_ENCODING or PKCS_7_ASN_ENCODING, // dwCertEncodingType: DWORD;
      PKCS_RSA_PRIVATE_KEY, // lpszStructType: LPCSTR;
      @LPKCS1Der[0], // pbEncoded: PBYTE;
      Length(LPKCS1Der), // cbEncoded: DWORD;
      0, // dwFlags: DWORD;
      nil, // pDecodePara: PCRYPT_DECODE_PARA;
      @LKeyBlob[0], // pvStructInfo: Pvoid;
      @LKeyBlobSize)); // pcbStructInfo: PDWORD
  if Length(LKeyBlob) <> Integer(LKeyBlobSize) then
    raise Exception.CreateFmt('Decoded RSA private key blob size mismatch (expected %d bytes, got %d bytes)', [LKeyBlobSize, Length(LKeyBlob)]);

  var LAlgorithm: BCRYPT_ALG_HANDLE := nil;
  ALCheckWinApiNTStatus(
    'BCryptOpenAlgorithmProvider',
    BCryptOpenAlgorithmProvider(
      @LAlgorithm, // phAlgorithm: PBCRYPT_ALG_HANDLE;
      BCRYPT_RSA_ALGORITHM, // pszAlgId: LPCWSTR;
      nil, // pszImplementation: LPCWSTR;
      0)); // dwFlags: ULONG
  try

    var LKeyHandle: BCRYPT_KEY_HANDLE := nil;
    ALCheckWinApiNTStatus(
      'BCryptImportKeyPair',
      BCryptImportKeyPair(
        LAlgorithm, // hAlgorithm: BCRYPT_ALG_HANDLE;
        nil, // hImportKey: BCRYPT_KEY_HANDLE;
        LEGACY_RSAPRIVATE_BLOB, // pszBlobType: LPCWSTR;
        @LKeyHandle, // phKey: PBCRYPT_KEY_HANDLE;
        @LKeyBlob[0], // pbInput: PUCHAR;
        Length(LKeyBlob), // cbInput: ULONG;
        0)); // dwFlags: ULONG
    try

      var LHashData := ALHashSHA2AsBytes(AData, THashSHA2.TSHA2Version.SHA256);
      var LPaddingInfo: BCRYPT_PKCS1_PADDING_INFO;
      LPaddingInfo.pszAlgId := BCRYPT_SHA256_ALGORITHM;

      var LSignatureSize: DWORD := 0;
      ALCheckWinApiNTStatus(
        'BCryptSignHash',
        BCryptSignHash(
          LKeyHandle, // hKey: BCRYPT_KEY_HANDLE;
          @LPaddingInfo, // pPaddingInfo: PVOID;
          PUCHAR(LHashData), // pbInput: PUCHAR;
          Length(LHashData), // cbInput: ULONG;
          nil, // pbOutput: PUCHAR;
          0, // cbOutput: ULONG;
          @LSignatureSize, // pcbResult: PULONG;
          BCRYPT_PAD_PKCS1)); // dwFlags: ULONG
      if LSignatureSize = 0 then raise Exception.Create('BCryptSignHash returned an empty signature');

      SetLength(Result, LSignatureSize);
      ALCheckWinApiNTStatus(
        'BCryptSignHash',
        BCryptSignHash(
          LKeyHandle, // hKey: BCRYPT_KEY_HANDLE;
          @LPaddingInfo, // pPaddingInfo: PVOID;
          PUCHAR(LHashData), // pbInput: PUCHAR;
          Length(LHashData), // cbInput: ULONG;
          PUCHAR(Result), // pbOutput: PUCHAR;
          Length(Result), // cbOutput: ULONG;
          @LSignatureSize, // pcbResult: PULONG;
          BCRYPT_PAD_PKCS1)); // dwFlags: ULONG
      if Length(Result) <> Integer(LSignatureSize) then
        raise Exception.CreateFmt('BCryptSignHash returned inconsistent signature size (expected %d bytes, got %d bytes)', [Length(Result), LSignatureSize]);

    finally
      ALCheckWinApiNTStatus(
        'BCryptDestroyKey',
        BCryptDestroyKey(LKeyHandle));
    end;

  finally
    ALCheckWinApiNTStatus(
      'BCryptCloseAlgorithmProvider',
      BCryptCloseAlgorithmProvider(LAlgorithm, 0));
  end;

end;
{$ENDIF}


////////////////////
////// OAuth2 //////
////////////////////

{**********************}
{$IF defined(MSWINDOWS)}
var
  ALGoogleOAuth2Lock: TLightweightMREW;
  ALGoogleOAuth2AccessTokens: TDictionary<AnsiString, TPair<AnsiString, TDateTime>>;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function ALGenerateGoogleOAuth2AccessToken(
           const AServiceAccountEmail: AnsiString;
           const AScope: AnsiString;
           const APrivateKey: AnsiString): AnsiString;
begin

  // Build the cache key from the service account email and the requested scope.
  // If a valid token already exists for this pair, we reuse it.
  var LKey := AServiceAccountEmail + #30{record separator} + AScope;

  // First try under a read lock to avoid unnecessary blocking.
  ALGoogleOAuth2Lock.BeginRead;
  try
    var LPair: TPair<AnsiString, TDateTime>;
    if ALGoogleOAuth2AccessTokens.TryGetValue(LKey, LPair) and (LPair.Value > ALUTCNow) then begin
      result := LPair.Key;
      Exit;
    end;
  finally
    ALGoogleOAuth2Lock.EndRead;
  end;

  // No valid cached token was found.
  // Switch to write lock so we can safely generate and store a new one.
  ALGoogleOAuth2Lock.BeginWrite;
  try

    // Double-check after acquiring the write lock, because another thread
    // may have generated and stored the token while we were waiting.
    var LPair: TPair<AnsiString, TDateTime>;
    if ALGoogleOAuth2AccessTokens.TryGetValue(LKey, LPair) and (LPair.Value > ALUTCNow) then begin
      result := LPair.Key;
      Exit;
    end;

    //
    // Using OAuth 2.0 for Server to Server Applications
    // https://developers.google.com/identity/protocols/oauth2/service-account#httprest
    //

    // JWT header:
    // {"alg":"RS256","typ":"JWT"}
    // already encoded as Base64URL.
    var LJWT := AnsiString('eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9');

    // Build the JWT claim set.
    // - iss   : service account email
    // - scope : requested Google API scope(s)
    // - aud   : token endpoint
    // - exp   : expiration time, max 1 hour after iat
    // - iat   : issued-at time
    var LNow := ALUtcNow;
    var LJWTclaim := '{'+
                       '"iss":"'+ALJavascriptEncode(AServiceAccountEmail)+'",'+ // The email address of the service account
                       '"scope":"'+ALJavascriptEncode(AScope)+'",'+ // A space-delimited list of the permissions that the application requests.
                       '"aud":"https://oauth2.googleapis.com/token",'+ // A descriptor of the intended target of the assertion. When making an access token request this value is always https://oauth2.googleapis.com/token.
                       '"exp":'+ALIntToStrA(DateTimeToUnix(IncHour(LNow,1)))+','+ // The expiration time of the assertion, specified as seconds since 00:00:00 UTC, January 1, 1970. This value has a maximum of 1 hour after the issued time.
                       '"iat":'+ALIntToStrA(DateTimeToUnix(LNow))+ // The time the assertion was issued, specified as seconds since 00:00:00 UTC, January 1, 1970.
                     '}';

    // Append the Base64URL-encoded claim set to the JWT header.
    LJWT := LJWT + '.' + ALURLBase64EncodeString(LJWTclaim);

    // Sign "<header>.<claimset>" using RS256 with the private key,
    // then append the Base64URL-encoded signature.
    LJWT := LJWT + '.' + ALURLBase64EncodeBytesA(ALRS256Sign(ALStringToBytes(LJWT),APrivateKey));

    // Prepare the HTTP client and request objects.
    var LWinHttpClient := ALCreateWinHttpClient;
    var LRequestFields := TALNVStringListA.Create;
    var LJsonDoc := TALJSONDocumentA.Create;
    try

      // Build the application/x-www-form-urlencoded POST body expected by Google.
      LRequestFields.AddNameValue('grant_type', 'urn:ietf:params:oauth:grant-type:jwt-bearer');
      LRequestFields.AddNameValue('assertion', LJWT);

      // Request the OAuth2 access token from Google.
      var LHTTPClientResponse := LWinHttpClient.PostFormUrlEncoded('https://oauth2.googleapis.com/token', LRequestFields);
      Try

        // Google should return HTTP 200 on success.
        if LHTTPClientResponse.StatusCode <> 200 then
          raise Exception.CreateFmt('Google OAuth2 token request failed (HTTP %d)', [LHTTPClientResponse.StatusCode]);

        // Expected successful response example:
        // {
        //   "access_token": "...",
        //   "scope": "...",
        //   "token_type": "Bearer",
        //   "expires_in": 3600
        // }
        LJsonDoc.LoadFromJSONString(LHTTPClientResponse.BodyString);

        // Extract the access token from the JSON response.
        Result := LJsonDoc.GetChildValueText('access_token', '');
        if result = '' then raise Exception.Create('Google OAuth2 token response does not contain an access token');

        // Cache the token with an expiration time shorter than the real one.
        // Here we keep only half of the official lifetime as a safety margin.
        LPair.Key := result;
        var LExpireID: Int32 := LJsonDoc.GetChildValueInt32('expires_in', 0);
        LPair.Value := max(
                         IncMinute(IncSecond(ALUtcNow, LExpireID), -15),
                         IncSecond(ALUtcNow, LExpireID div 2));
        ALGoogleOAuth2AccessTokens.AddOrSetValue(LKey, LPair);

      finally
        ALFreeAndNil(LHTTPClientResponse);
      end;

    finally
      alfreeAndNil(LJsonDoc);
      alFreeAndNil(LRequestFields);
      alFreeAndNil(LWinHttpClient);
    end;

  finally
    ALGoogleOAuth2Lock.EndWrite;
  end;

end;
{$ENDIF}


//////////////////////////////////////
////// Windows Credential Vault //////
//////////////////////////////////////

{**********************}
{$IF defined(MSWINDOWS)}
function ALTryGetGenericWinCredential(
           const ATargetName: AnsiString;
           out AUserName: AnsiString;
           out APassword: AnsiString): Boolean;
begin
  var LCred: PCREDENTIALA := nil;
  Result := CredReadA(
              PAnsiChar(ATargetName), // TargetName: LPCSTR
              CRED_TYPE_GENERIC, // &Type: DWORD;
              0, // Flags: DWORD;
              LCred); // out Credential: PCREDENTIALA
  if not Result then exit;
  try
    AUsername := LCred.UserName;
    if (LCred.CredentialBlob <> nil) and
       (LCred.CredentialBlobSize > 0) then begin
      SetString(
        APassword,
        PAnsiChar(LCred.CredentialBlob),
        LCred.CredentialBlobSize);
    end
    else
      APassword := '';
  finally
    if LCred <> nil then CredFree(LCred);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
procedure ALGetGenericWinCredential(
            const ATargetName: AnsiString;
            out AUserName: AnsiString;
            out APassword: AnsiString);
begin
  ALCheckWinApiBoolean(
    'CredReadW',
    ALTryGetGenericWinCredential(
      ATargetName, // const ATargetName: AnsiString;
      AUserName, // out AUserName: AnsiString;
      APassword)); // out APassword: AnsiString) then
end;
{$ENDIF}


initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.Cipher','initialization');
  {$ENDIF}

  randomize;
  ALRandom32 := ALRandom32_Default;
  ALRandom64 := ALRandom64_Default;

  {$IFDEF ALCPUXASM}

  //https://en.wikipedia.org/wiki/CPUID#EAX.3D1:_Processor_Info_and_Feature_Bits
  //The processor info and feature flags are manufacturer specific but usually the
  //Intel values are used by other manufacturers for the sake of compatibility.
  //Bit 00 = $00000001
  //Bit 01 = $00000002
  //Bit 02 = $00000004
  //Bit 03 = $00000008
  //Bit 04 = $00000010
  //Bit 05 = $00000020
  //Bit 06 = $00000040
  //Bit 07 = $00000080
  //Bit 08 = $00000100
  //Bit 09 = $00000200
  //Bit 10 = $00000400
  //Bit 11 = $00000800
  //Bit 12 = $00001000
  //Bit 13 = $00002000
  //Bit 14 = $00004000
  //Bit 15 = $00008000
  //Bit 16 = $00010000
  //Bit 17 = $00020000
  //Bit 18 = $00040000
  //Bit 19 = $00080000
  //Bit 20 = $00100000
  //Bit 21 = $00200000
  //Bit 22 = $00400000
  //Bit 23 = $00800000
  //Bit 24 = $01000000
  //Bit 25 = $02000000
  //Bit 26 = $04000000
  //Bit 27 = $08000000
  //Bit 28 = $10000000
  //Bit 29 = $20000000
  //Bit 30 = $40000000
  //Bit 31 = $80000000
  {$WARN SYMBOL_PLATFORM OFF}
  if (CPUIDTable[1].ECX and $40000000{Bit 30}) <> 0 then begin
    ALRandom32 := ALRandom32_RdRand;
    ALRandom64 := ALRandom64_RdRand;
    try
      if (ALRandom32_RdRand(ALMaxUint) = ALRandom32_RdRand(ALMaxUint)) or             // most probably a RDRAND bug,
         (ALRandom64_RdRand(ALMaxUint64) = ALRandom64_RdRand(ALMaxUint64)) then begin // e.g. on AMD Rizen 3000
        ALRandom32 := ALRandom32_Default;
        ALRandom64 := ALRandom64_Default;
      end;
    except
      // may trigger an illegal instruction exception on some Ivy Bridge
      ALRandom32 := ALRandom32_Default;
      ALRandom64 := ALRandom64_Default;
    end;
  end;
  {$WARN SYMBOL_PLATFORM ON}

  {$ENDIF ALCPUXASM}

  {$IF defined(MSWINDOWS)}
  //ALGoogleOAuth2Lock := ?? There is no TLightweightMREW.Create; initialization is done through the TLightweightMREW.Initialize class operator instead
  ALGoogleOAuth2AccessTokens := TDictionary<AnsiString, TPair<AnsiString, TDateTime>>.Create;
  {$ENDIF}

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.Cipher','finalization');
  {$ENDIF}
  {$IF defined(MSWINDOWS)}
  AlFreeAndNil(ALGoogleOAuth2AccessTokens);
  {$ENDIF}

end.