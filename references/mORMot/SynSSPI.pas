/// low level access to Windows SSPI/SChannel API for the Win32/Win64 platform
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynSSPI;
{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Chaa.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):
   Arnaud Bouchez

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

}

{$I Synopse.inc} // define HASINLINE and other compatibility switches

interface

{$ifdef MSWINDOWS} // compiles as void unit for non-Windows - allow Lazarus package

uses
  Windows,
  SysUtils;


(* ================= Low-Level SSPI / SChannel API types and functions ====== *)

type
  {$ifdef CPU64}
  LONG_PTR = Int64;
  {$else}
  LONG_PTR = integer;
  {$endif}

  {$ifdef HASCODEPAGE}
  TSSPIBuffer = RawByteString;
  {$else}
  TSSPIBuffer = AnsiString;
  {$endif}

  /// SSPI context handle
  TSecHandle = record
    dwLower: LONG_PTR;
    dwUpper: LONG_PTR;
  end;
  PSecHandle = ^TSecHandle;

  /// SSPI context
  TSecContext = record
    ID: Int64;
    CredHandle: TSecHandle;
    CtxHandle: TSecHandle;
    CreatedTick64: Int64;
  end;
  PSecContext = ^TSecContext;

  /// dynamic array of SSPI contexts
  // - used to hold information between calls to ServerSSPIAuth
  TSecContextDynArray = array of TSecContext;

  /// defines a SSPI buffer
  {$ifdef USERECORDWITHMETHODS}TSecBuffer = record
    {$else}TSecBuffer = object{$endif}
  public
    cbBuffer: Cardinal;
    BufferType: Cardinal;
    pvBuffer: Pointer;
    procedure Init(aType: cardinal; aData: pointer; aSize: cardinal);
  end;
  PSecBuffer = ^TSecBuffer;

  /// describes a SSPI buffer
  {$ifdef USERECORDWITHMETHODS}TSecBufferDesc = record
    {$else}TSecBufferDesc = object{$endif}
  public
    ulVersion: Cardinal;
    cBuffers: Cardinal;
    pBuffers: PSecBuffer;
    procedure Init(aVersion: cardinal; aBuffers: PSecBuffer; aBuffersCount: cardinal);
  end;
  PSecBufferDesc = ^TSecBufferDesc;

  /// store the name associated with the context
  SecPkgContext_NamesW = record
    sUserName: PWideChar;
  end;
  
  /// store information about a SSPI package
  TSecPkgInfoW = record
    fCapabilities: Cardinal;
    wVersion: Word;
    wRPCID: Word;
    cbMaxToken: Cardinal;
    Name: PWideChar;
    Comment: PWideChar;
  end;
  /// pointer to information about a SSPI package
  PSecPkgInfoW = ^TSecPkgInfoW;

  /// store negotation information about a SSPI package
  TSecPkgContext_NegotiationInfo = record
    PackageInfo: PSecPkgInfoW;
    NegotiationState: Cardinal;
  end;

  /// store various working buffer sizes of a SSPI command
  TSecPkgContext_Sizes = record
    cbMaxToken: Cardinal;
    cbMaxSignature: Cardinal;
    cbBlockSize: Cardinal;
    cbSecurityTrailer: Cardinal;
  end;

  /// store various working buffer sizes of a SSPI stream
  TSecPkgContext_StreamSizes = record
    cbHeader: Cardinal;
    cbTrailer: Cardinal;
    cbMaximumMessage: Cardinal;
    cBuffers: Cardinal;
    cbBlockSize: Cardinal;
  end;

  /// information about SSPI supported algorithm
  TSecPkgCred_SupportedAlgs = record
    cSupportedAlgs: Cardinal;
    palgSupportedAlgs: Pointer;
  end;
  /// pointer to SSPI supported algorithm
  PSecPkgCred_SupportedAlgs = ^TSecPkgCred_SupportedAlgs;

  /// information about SSPI Authority Identify
  TSecWinntAuthIdentityW = record
    User: PWideChar;
    UserLength: Cardinal;
    Domain: PWideChar;
    DomainLength: Cardinal;
    Password: PWideChar;
    PasswordLength: Cardinal;
    Flags: Cardinal
  end;
  /// pointer to SSPI Authority Identify
  PSecWinntAuthIdentityW = ^TSecWinntAuthIdentityW;

const
  SECBUFFER_VERSION = 0;
  SECBUFFER_DATA = 1;
  SECBUFFER_TOKEN = 2;
  SECBUFFER_PADDING = 9;
  SECBUFFER_STREAM = 10;
  SECPKG_CRED_INBOUND  = $00000001;
  SECPKG_CRED_OUTBOUND = $00000002;
  SECPKG_ATTR_SIZES = 0;
  SECPKG_ATTR_NAMES = 1;
  SECPKG_ATTR_STREAM_SIZES = 4;
  SECPKG_ATTR_NEGOTIATION_INFO = 12;
  SECURITY_NETWORK_DREP = 0;
  SECURITY_NATIVE_DREP = $10;
  ISC_REQ_MUTUAL_AUTH = $00000002;
  ISC_REQ_CONFIDENTIALITY = $00000010;
  ISC_REQ_ALLOCATE_MEMORY = $00000100;
  ASC_REQ_CONFIDENTIALITY = $00000010;
  ASC_REQ_ALLOCATE_MEMORY = $00000100;
  SEC_I_CONTINUE_NEEDED = $00090312;
  SEC_I_COMPLETE_NEEDED = $00090313;
  SEC_I_COMPLETE_AND_CONTINUE = $00090314;
  SEC_WINNT_AUTH_IDENTITY_UNICODE = $02;

function QuerySecurityPackageInfoW(pszPackageName: PWideChar;
  var ppPackageInfo: PSecPkgInfoW): Integer; stdcall;

function AcquireCredentialsHandleW(pszPrincipal, pszPackage: PWideChar;
  fCredentialUse: Cardinal; pvLogonId: Pointer; pAuthData: PSecWinntAuthIdentityW;
  pGetKeyFn: Pointer; pvGetKeyArgument: Pointer; phCredential: PSecHandle;
  var ptsExpiry: LARGE_INTEGER): Integer; stdcall;

function InitializeSecurityContextW(phCredential: PSecHandle; phContext: PSecHandle;
  pszTargetName: PWideChar; fContextReq, Reserved1, TargetDataRep: Cardinal;
  pInput: PSecBufferDesc; Reserved2: Cardinal; phNewContext: PSecHandle;
  pOutput: PSecBufferDesc; var pfContextAttr: Cardinal; var ptsExpiry: LARGE_INTEGER): Integer; stdcall;

function AcceptSecurityContext(phCredential: PSecHandle; phContext: PSecHandle;
  pInput: PSecBufferDesc; fContextReq, TargetDataRep: Cardinal;
  phNewContext: PSecHandle; pOutput: PSecBufferDesc; var pfContextAttr: Cardinal;
  var ptsExpiry: LARGE_INTEGER): Integer; stdcall;

function CompleteAuthToken(phContext: PSecHandle; pToken: PSecBufferDesc): Integer; stdcall;

function QueryContextAttributesW(phContext: PSecHandle; ulAttribute: Cardinal;
  pBuffer: Pointer): Integer; stdcall;

function QuerySecurityContextToken(phContext: PSecHandle; var Token: THandle): Integer; stdcall;

function EncryptMessage(phContext: PSecHandle; fQOP: Cardinal;
  pToken: PSecBufferDesc; MessageSeqNo: Cardinal): Integer; stdcall;

function DecryptMessage(phContext: PSecHandle; pToken: PSecBufferDesc;
  MessageSeqNo: Cardinal; var fQOP: Cardinal): Integer; stdcall;

function FreeContextBuffer(pvContextBuffer: Pointer): Integer; stdcall;

function DeleteSecurityContext(phContext: PSecHandle): Integer; stdcall;

function FreeCredentialsHandle(phCredential: PSecHandle): Integer; stdcall;


type
  HCRYPTPROV = pointer;
  HCERTSTORE = Pointer;
  PCCERT_CONTEXT = pointer;
  ALG_ID = pointer;
  _HMAPPER = pointer;

  /// SChannel credential information
  TSChannel_Cred = record
    dwVersion: Cardinal;
    cCreds: Cardinal;
    paCred: PCCERT_CONTEXT;
    hRootStore: HCERTSTORE;
    cMappers: Cardinal;
    aphMappers: _HMAPPER;
    cSupportedAlgs: Cardinal;
    palgSupportedAlgs: ALG_ID;
    grbitEnabledProtocols: Cardinal;
    dwMinimumCipherStrength: Cardinal;
    dwMaximumCipherStrength: Cardinal;
    dwSessionLifespan: Cardinal;
    dwFlags: Cardinal;
    dwCredFormat: Cardinal;
  end;
  /// pointer to SChannel credential information
  PSChannel_Cred = ^TSChannel_Cred;


const
  UNISP_NAME = 'Microsoft Unified Security Protocol Provider';

  SP_PROT_TLS1_0_SERVER             = $00000040;
  SP_PROT_TLS1_0_CLIENT             = $00000080;
  SP_PROT_TLS1_0                    = SP_PROT_TLS1_0_SERVER + SP_PROT_TLS1_0_CLIENT;

  SP_PROT_TLS1_1_SERVER             = $00000100;
  SP_PROT_TLS1_1_CLIENT             = $00000200;
  SP_PROT_TLS1_1                    = SP_PROT_TLS1_1_SERVER + SP_PROT_TLS1_1_CLIENT;

  // TLS 1.2 should be the preferred safe default
  SP_PROT_TLS1_2_SERVER             = $00000400;
  SP_PROT_TLS1_2_CLIENT             = $00000800;
  SP_PROT_TLS1_2                    = SP_PROT_TLS1_2_SERVER + SP_PROT_TLS1_2_CLIENT;

  SP_PROT_TLS1_X_SERVER             = SP_PROT_TLS1_0_SERVER + SP_PROT_TLS1_1_SERVER + SP_PROT_TLS1_2_SERVER;
  SP_PROT_TLS1_X_CLIENT             = SP_PROT_TLS1_0_CLIENT + SP_PROT_TLS1_1_CLIENT + SP_PROT_TLS1_2_CLIENT;
  SP_PROT_TLS1_X                    = SP_PROT_TLS1_X_SERVER + SP_PROT_TLS1_X_CLIENT;


function CertOpenStore(lpszStoreProvider: PAnsiChar; dwEncodingType: DWORD;
  hCryptProv: HCRYPTPROV; dwFlags: DWORD; pvPara: Pointer): HCERTSTORE; stdcall;

function CertOpenSystemStoreW(hProv: HCRYPTPROV; szSubsystemProtocol: PWideChar): HCERTSTORE; stdcall;

function CertCloseStore(hCertStore: HCERTSTORE; dwFlags: DWORD): BOOL; stdcall;

function CertFindCertificateInStore(hCertStore: HCERTSTORE;
  dwCertEncodingType, dwFindFlags, dwFindType: DWORD; pvFindPara: Pointer;
  pPrevCertContext: PCCERT_CONTEXT): PCCERT_CONTEXT; stdcall;



(* ========================= High-Level SSPI / SChannel API wrappers  ======= *)

/// Sets aSecHandle fields to empty state for a given connection ID
procedure InvalidateSecContext(var aSecContext: TSecContext; aConnectionID: Int64);

/// Free aSecContext on client or server side
procedure FreeSecContext(var aSecContext: TSecContext);

/// Encrypts a message
// - aSecContext must be set e.g. from previous success call to ServerSSPIAuth
// or ClientSSPIAuth
// - aPlain contains data that must be encrypted
// - returns encrypted message
function SecEncrypt(var aSecContext: TSecContext; const aPlain: TSSPIBuffer): TSSPIBuffer;

/// Decrypts a message
// - aSecContext must be set e.g. from previous success call to ServerSSPIAuth
// or ClientSSPIAuth
// - aEncrypted contains data that must be decrypted
// - returns decrypted message
function SecDecrypt(var aSecContext: TSecContext; const aEncrypted: TSSPIBuffer): TSSPIBuffer;


type
  /// exception class raised durint SSPI/SChannel process
  ESynSSPI = class(Exception)
  public
    constructor CreateLastOSError(const aContext: TSecContext);
  end;

  /// the supported TLS modes
  // - unsafe deprecated modes (e.g. SSL) are not defined at all
  TSynSSPIMode = (tls10, tls11, tls12);
  /// set of supported TLS modes
  TSynSSPIModes = set of TSynSSPIMode;

  /// used for low-level logging
  TSynSSPILog = procedure(const Fmt: TSSPIBuffer; const Args: array of const) of object;

  /// abstract parent class for SSPI / SChannel process
  TSynSSPIAbstract = class
  protected
    fNewConversation: boolean;
    fTLS: TSynSSPIModes;
    fContext: TSecContext;
    fStreamSizes: TSecPkgContext_StreamSizes;
    procedure DeleteContext;
    procedure EnsureStreamSizes;
  public
    /// initialize the process
    constructor Create(aConnectionID: Int64); virtual;
    /// read-only access to the associated connection ID, as provided to Create
    property ConnectionID: Int64 read fContext.ID;
    /// the TLS modes supported by this instance
    // - only TLS 1.2 is suppported by default, for security reasons
    property TLS: TSynSSPIModes read fTLS write fTLS;
  end;

  TSynSSPIClient = class(TSynSSPIAbstract)
  protected
  public
  end;


implementation


(* ========================= Low-Level SSPI / SChannel API types ====== *)

const
  secur32 = 'secur32.dll';

function QuerySecurityPackageInfoW; external secur32;
function AcquireCredentialsHandleW; external secur32;
function InitializeSecurityContextW; external secur32;
function AcceptSecurityContext; external secur32;
function CompleteAuthToken; external secur32;
function QueryContextAttributesW; external secur32;
function QuerySecurityContextToken; external secur32;
function EncryptMessage; external secur32;
function DecryptMessage; external secur32;
function FreeContextBuffer; external secur32;
function DeleteSecurityContext; external secur32;
function FreeCredentialsHandle; external secur32;

const
  crypt32 = 'crypt32.dll';

function CertOpenStore; external crypt32;
function CertOpenSystemStoreW; external crypt32;
function CertCloseStore; external crypt32;
function CertFindCertificateInStore; external crypt32;


{ TSecBuffer }

procedure TSecBuffer.Init(aType: cardinal; aData: pointer;
  aSize: cardinal);
begin
  BufferType := aType;
  pvBuffer := aData;
  cbBuffer := aSize;
end;


{ TSecBufferDesc }

procedure TSecBufferDesc.Init(aVersion: cardinal; aBuffers: PSecBuffer;
  aBuffersCount: cardinal);
begin
  ulVersion := aVersion;
  pBuffers := aBuffers;
  cBuffers := aBuffersCount;
end;



(* ========================= High-Level SSPI / SChannel API wrappers  === *)

procedure InvalidateSecContext(var aSecContext: TSecContext; aConnectionID: Int64);
begin
  aSecContext.ID := aConnectionID;
  aSecContext.CredHandle.dwLower := -1;
  aSecContext.CredHandle.dwUpper := -1;
  aSecContext.CtxHandle.dwLower := -1;
  aSecContext.CtxHandle.dwUpper := -1;
  aSecContext.CreatedTick64 := 0;
end;

procedure FreeSecurityContext(var handle: TSecHandle);
begin
  if (handle.dwLower <> -1) or (handle.dwUpper <> -1) then begin
    DeleteSecurityContext(@handle);
    handle.dwLower := -1;
    handle.dwUpper := -1;
  end;
end;

procedure FreeCredentialsContext(var handle: TSecHandle);
begin
  if (handle.dwLower <> -1) or (handle.dwUpper <> -1) then begin
    FreeCredentialsHandle(@handle);
    handle.dwLower := -1;
    handle.dwUpper := -1;
  end;
end;

procedure FreeSecContext(var aSecContext: TSecContext);
begin
  FreeSecurityContext(aSecContext.CtxHandle);
  FreeCredentialsContext(aSecContext.CredHandle);
end;

function SecEncrypt(var aSecContext: TSecContext; const aPlain: TSSPIBuffer): TSSPIBuffer;
var Sizes: TSecPkgContext_Sizes;
    SrcLen, EncLen: Cardinal;
    Token: array [0..127] of Byte; // Usually 60 bytes
    Padding: array [0..63] of Byte; // Usually 1 byte
    InBuf: array[0..2] of TSecBuffer;
    InDesc: TSecBufferDesc;
    EncBuffer: TSSPIBuffer;
    Status: Integer;
    BufPtr: PByte;
begin
  // Sizes.cbSecurityTrailer is size of the trailer (signature + padding) block
  if QueryContextAttributesW(@aSecContext.CtxHandle, SECPKG_ATTR_SIZES, @Sizes) <> 0 then
    raise ESynSSPI.CreateLastOSError(aSecContext);

  // Encrypted data buffer structure:
  //
  // SSPI/Kerberos Interoperability with GSSAPI
  // https://msdn.microsoft.com/library/windows/desktop/aa380496.aspx
  //
  // GSS-API wrapper for Microsoft's Kerberos SSPI in Windows 2000
  // http://www.kerberos.org/software/samples/gsskrb5/gsskrb5/krb5/krb5msg.c
  //
  //   cbSecurityTrailer bytes   SrcLen bytes     cbBlockSize bytes or less
  //   (60 bytes)                                 (0 bytes, not used)
  // +-------------------------+----------------+--------------------------+
  // | Trailer                 | Data           | Padding                  |
  // +-------------------------+----------------+--------------------------+

  Assert(Sizes.cbSecurityTrailer <= High(Token)+1);
  InBuf[0].Init(SECBUFFER_TOKEN, @Token[0], Sizes.cbSecurityTrailer);

  // Encoding done in-place, so we copy the data
  SrcLen := Length(aPlain);
  SetString(EncBuffer, PAnsiChar(Pointer(aPlain)), SrcLen);
  InBuf[1].Init(SECBUFFER_DATA, Pointer(EncBuffer), SrcLen);

  Assert(Sizes.cbBlockSize <= High(Padding)+1);
  InBuf[2].Init(SECBUFFER_PADDING, @Padding[0], Sizes.cbBlockSize);

  InDesc.Init(SECBUFFER_VERSION, @InBuf, 3);

  Status := EncryptMessage(@aSecContext.CtxHandle, 0, @InDesc, 0);
  if Status < 0 then
    raise ESynSSPI.CreateLastOSError(aSecContext);

  EncLen := InBuf[0].cbBuffer + InBuf[1].cbBuffer + InBuf[2].cbBuffer;
  SetLength(Result, EncLen);
  BufPtr := PByte(Result);
  Move(PByte(InBuf[0].pvBuffer)^, BufPtr^, InBuf[0].cbBuffer);
  Inc(BufPtr, InBuf[0].cbBuffer);
  Move(PByte(InBuf[1].pvBuffer)^, BufPtr^, InBuf[1].cbBuffer);
  Inc(BufPtr, InBuf[1].cbBuffer);
  Move(PByte(InBuf[2].pvBuffer)^, BufPtr^, InBuf[2].cbBuffer);
end;

function SecDecrypt(var aSecContext: TSecContext; const aEncrypted: TSSPIBuffer): TSSPIBuffer;
var EncLen, SigLen: Cardinal;
    BufPtr: PByte;
    InBuf: array [0..1] of TSecBuffer;
    InDesc: TSecBufferDesc;
    Status: Integer;
    QOP: Cardinal;
begin
  EncLen := Length(aEncrypted);
  BufPtr := PByte(aEncrypted);
  if EncLen < SizeOf(Cardinal) then  begin
    SetLastError(ERROR_INVALID_PARAMETER);
    raise ESynSSPI.CreateLastOSError(aSecContext);
  end;

  // Hack for compatibility with previous versions.
  // Should be removed in future.
  // Old version buffer format - first 4 bytes is Trailer length, skip it.
  // 16 bytes for NTLM and 60 bytes for Kerberos
  SigLen := PCardinal(BufPtr)^;
  if (SigLen = 16) or (SigLen = 60) then
  begin
    Inc(BufPtr, SizeOf(Cardinal));
    Dec(EncLen, SizeOf(Cardinal));
  end;

  InBuf[0].Init(SECBUFFER_STREAM, BufPtr, EncLen);
  InBuf[1].Init(SECBUFFER_DATA, nil, 0);
  InDesc.Init(SECBUFFER_VERSION, @InBuf, 2);

  Status := DecryptMessage(@aSecContext.CtxHandle, @InDesc, 0, QOP);
  if Status < 0 then
    raise ESynSSPI.CreateLastOSError(aSecContext);

  SetString(Result, PAnsiChar(InBuf[1].pvBuffer), InBuf[1].cbBuffer);
  FreeContextBuffer(InBuf[1].pvBuffer);
end;


{ ESynSSPI }

constructor ESynSSPI.CreateLastOSError(const aContext: TSecContext);
var error: integer;
begin
  error := GetLastError;
  CreateFmt('API Error %d [%s] for ConnectionID=%d',
    [error,SysErrorMessage(error),aContext.ID]);
end;


{ TSynSSPIAbstract }

constructor TSynSSPIAbstract.Create(aConnectionID: Int64);
begin
  inherited Create;
  fNewConversation := true;
  InvalidateSecContext(fContext, aConnectionID);
  fTLS := [tls12];
end;

procedure TSynSSPIAbstract.EnsureStreamSizes;
begin
  if fStreamSizes.cbHeader=0 then
    if QueryContextAttributesW(@fContext.CtxHandle,SECPKG_ATTR_STREAM_SIZES,@fStreamSizes) <> 0 then
      raise ESynSSPI.CreateLastOSError(fContext);
end;

procedure TSynSSPIAbstract.DeleteContext;
begin
  FreeSecurityContext(fContext.CtxHandle);
  FreeCredentialsContext(fContext.CredHandle);
end;

{$else}

implementation // compiles as void unit for non-Windows

{$endif MSWINDOWS}

end.
