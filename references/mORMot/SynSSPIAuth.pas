/// low level access to Windows Authentication for the Win32/Win64 platform
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynSSPIAuth;
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

  This unit has been contributed by Chaa.
  See https://synopse.info/forum/viewtopic.php?pid=5619#p5619
}

{$I Synopse.inc} // define HASINLINE and other compatibility switches

interface

{$ifdef MSWINDOWS} // compiles as void unit for non-Windows - allow Lazarus package

uses
  Windows,
  SysUtils,
  SynCommons,
  SynSSPI;

/// Client-side authentication procedure
// - aSecContext holds information between function calls
// - aInData contains data received from server
// - aSecKerberosSPN is the optional SPN domain name, e.g.
// 'mymormotservice/myserver.mydomain.tld'
// - aOutData contains data that must be sent to server
// - if function returns True, client must send aOutData to server
// and call function again width data, returned from servsr
function ClientSSPIAuth(var aSecContext: TSecContext;
    const aInData: RawByteString; const aSecKerberosSPN: RawUTF8;
    out aOutData: RawByteString): Boolean;

/// Client-side authentication procedure with clear text password.
//  This function must be used when application need to use different
//  user credentials (not credentials of logged in user)
// - aSecContext holds information between function calls
// - aInData contains data received from server
// - aUserName is the domain and user name, in form of
// 'DomainName\UserName'
// - aPassword is the user clear text password
// - aOutData contains data that must be sent to server
// - if function returns True, client must send aOutData to server
// and call function again width data, returned from server
function ClientSSPIAuthWithPassword(var aSecContext: TSecContext;
    const aInData: RawByteString; const aUserName: RawUTF8;
    const aPassword: RawUTF8; out aOutData: RawByteString): Boolean;

/// Server-side authentication procedure
// - aSecContext holds information between function calls
// - aInData contains data recieved from client
// - aOutData contains data that must be sent to client
// - if function returns True, server must send aOutData to client
// and call function again width data, returned from client
function ServerSSPIAuth(var aSecContext: TSecContext;
    const aInData: RawByteString; out aOutData: RawByteString): Boolean;

/// Server-side function that returns authenticated user name
// - aSecContext must be received from previous successful call to ServerSSPIAuth
// - aUserName contains authenticated user name
procedure ServerSSPIAuthUser(var aSecContext: TSecContext; out aUserName: RawUTF8);

/// Returns name of the security package that has been used with the negotiation process
// - aSecContext must be received from previos success call to ServerSSPIAuth
// or ClientSSPIAuth
function SecPackageName(var aSecContext: TSecContext): RawUTF8;

/// Force using aSecKerberosSPN for server identification.
// - aSecKerberosSPN is the Service Principal Name,
// registered in domain, e.g.
// 'mymormotservice/myserver.mydomain.tld@MYDOMAIN.TLD'
procedure ClientForceSPN(const aSecKerberosSPN: RawUTF8);

/// Force NTLM authentication instead of Negotiate for browser authenticaton.
// Use case: SPNs not configured properly in domain
// - see for details https://synopse.info/forum/viewtopic.php?id=931&p=3
procedure ServerForceNTLM(IsNTLM: boolean);

const
  /// SSPI package names. Client always use Negotiate.
  // Server detect Negotiate or NTLM requests and use appropriate package
  SECPKGNAMENTLM = 'NTLM';
  SECPKGNAMENEGOTIATE = 'Negotiate';

var
  /// HTTP header to be set for SSPI authentication 'WWW-Authenticate: NTLM' or 'WWW-Authenticate: Negotiate';
  SECPKGNAMEHTTPWWWAUTHENTICATE: RawUTF8;
  /// HTTP header pattern received for SSPI authentication 'AUTHORIZATION: NTLM ' or 'AUTHORIZATION: NEGOTIATE '
  SECPKGNAMEHTTPAUTHORIZATION: PAnsiChar;


implementation

var
  ForceSecKerberosSPN: WideString;

function ClientSSPIAuthWorker(var aSecContext: TSecContext;
    const aInData: RawByteString; pszTargetName: PWideChar;
    pAuthData: PSecWinntAuthIdentityW;
    out aOutData: RawByteString): Boolean;
var InBuf: TSecBuffer;
    InDesc: TSecBufferDesc;
    InDescPtr: PSecBufferDesc;
    SecPkgInfo: PSecPkgInfoW;
    Expiry: LARGE_INTEGER;
    LInCtxPtr: PSecHandle;
    OutBuf: TSecBuffer;
    OutDesc: TSecBufferDesc;
    CtxReqAttr: Cardinal;
    CtxAttr: Cardinal;
    Status: Integer;
begin
  InBuf.BufferType := SECBUFFER_TOKEN;
  InBuf.cbBuffer := Length(aInData);
  InBuf.pvBuffer := PByte(aInData);

  if (aSecContext.CredHandle.dwLower = -1) and (aSecContext.CredHandle.dwUpper = -1) then begin
    aSecContext.CreatedTick64 := GetTickCount64();
    if QuerySecurityPackageInfoW(SECPKGNAMENEGOTIATE, SecPkgInfo) <> 0 then
      raise ESynSSPI.CreateLastOSError(aSecContext);
    try
      if AcquireCredentialsHandleW(nil, SecPkgInfo^.Name, SECPKG_CRED_OUTBOUND, nil, pAuthData, nil, nil, @aSecContext.CredHandle, Expiry) <> 0 then
        raise ESynSSPI.CreateLastOSError(aSecContext);
    finally
      FreeContextBuffer(SecPkgInfo);
    end;
    InDescPtr := nil;
    LInCtxPtr := nil;
  end
  else begin
    InDesc.ulVersion := SECBUFFER_VERSION;
    InDesc.cBuffers := 1;
    InDesc.pBuffers := @InBuf;
    InDescPtr := @InDesc;
    LInCtxPtr := @aSecContext.CtxHandle;
  end;

  CtxReqAttr := ISC_REQ_ALLOCATE_MEMORY or ASC_REQ_CONFIDENTIALITY;
  if pszTargetName <> nil then
    CtxReqAttr := CtxReqAttr or ISC_REQ_MUTUAL_AUTH;

  OutBuf.BufferType := SECBUFFER_TOKEN;
  OutBuf.cbBuffer := 0;
  OutBuf.pvBuffer := nil;
  OutDesc.ulVersion := SECBUFFER_VERSION;
  OutDesc.cBuffers := 1;
  OutDesc.pBuffers := @OutBuf;

  Status := InitializeSecurityContextW(@aSecContext.CredHandle, LInCtxPtr,
    pszTargetName, CtxReqAttr,
    0, SECURITY_NATIVE_DREP, InDescPtr, 0, @aSecContext.CtxHandle, @OutDesc, CtxAttr, Expiry);

  Result := (Status = SEC_I_CONTINUE_NEEDED) or (Status = SEC_I_COMPLETE_AND_CONTINUE);

  if (Status = SEC_I_COMPLETE_NEEDED) or (Status = SEC_I_COMPLETE_AND_CONTINUE) then
    Status := CompleteAuthToken(@aSecContext.CtxHandle, @OutDesc);
  if Status < 0 then
    raise ESynSSPI.CreateLastOSError(aSecContext);

  SetString(aOutData, PAnsiChar(OutBuf.pvBuffer), OutBuf.cbBuffer);
  FreeContextBuffer(OutBuf.pvBuffer);
end;

function ClientSSPIAuth(var aSecContext: TSecContext;
    const aInData: RawByteString; const aSecKerberosSPN: RawUTF8;
    out aOutData: RawByteString): Boolean;
var TargetName: PWideChar;
begin
  if aSecKerberosSPN <> '' then
    TargetName := PWideChar(UTF8ToSynUnicode(aSecKerberosSPN)) else begin
      if ForceSecKerberosSPN <> '' then
        TargetName := PWideChar(ForceSecKerberosSPN) else
        TargetName := nil;
    end;
  Result := ClientSSPIAuthWorker(aSecContext, aInData, TargetName, nil, aOutData);
end;

function ClientSSPIAuthWithPassword(var aSecContext: TSecContext;
    const aInData: RawByteString; const aUserName: RawUTF8;
    const aPassword: RawUTF8; out aOutData: RawByteString): Boolean;
var UserPos: Integer;
    Domain, User, Password: SynUnicode;
    AuthIdentity: TSecWinntAuthIdentityW;
    TargetName: PWideChar;
begin
  UserPos := PosExChar('\', aUserName);
  if UserPos=0 then begin
    Domain := '';
    User := UTF8ToSynUnicode(aUserName);
  end else begin
    Domain := UTF8ToSynUnicode(Copy(aUserName, 1, UserPos-1));
    User := UTF8ToSynUnicode(Copy(aUserName, UserPos+1, MaxInt));
  end;
  Password := UTF8ToSynUnicode(aPassword);

  AuthIdentity.Domain := PWideChar(Domain);
  AuthIdentity.DomainLength := Length(Domain);
  AuthIdentity.User := PWideChar(User);
  AuthIdentity.UserLength := Length(User);
  AuthIdentity.Password := PWideChar(Password);
  AuthIdentity.PasswordLength := Length(Password);
  AuthIdentity.Flags := SEC_WINNT_AUTH_IDENTITY_UNICODE;

  if ForceSecKerberosSPN <> '' then
    TargetName := PWideChar(ForceSecKerberosSPN) else
    TargetName := nil;

  Result := ClientSSPIAuthWorker(aSecContext, aInData, TargetName, @AuthIdentity, aOutData);
end;

function ServerSSPIAuth(var aSecContext: TSecContext;
  const aInData: RawByteString; out aOutData: RawByteString): Boolean;
var InBuf: TSecBuffer;
    InDesc: TSecBufferDesc;
    SecPkgInfo: PSecPkgInfoW;
    Expiry: LARGE_INTEGER;
    LInCtxPtr: PSecHandle;
    OutBuf: TSecBuffer;
    OutDesc: TSecBufferDesc;
    CtxAttr: Cardinal;
    Status: Integer;
begin
  InBuf.BufferType := SECBUFFER_TOKEN;
  InBuf.cbBuffer := Length(aInData);
  InBuf.pvBuffer := PByte(aInData);
  InDesc.ulVersion := SECBUFFER_VERSION;
  InDesc.cBuffers := 1;
  InDesc.pBuffers := @InBuf;

  if (aSecContext.CredHandle.dwLower = -1) and (aSecContext.CredHandle.dwUpper = -1) then begin
    aSecContext.CreatedTick64 := GetTickCount64();
    if IdemPChar(Pointer(aInData), 'NTLMSSP') then begin
      if QuerySecurityPackageInfoW(SECPKGNAMENTLM, SecPkgInfo) <> 0 then
        raise ESynSSPI.CreateLastOSError(aSecContext);
    end else begin
      if QuerySecurityPackageInfoW(SECPKGNAMENEGOTIATE, SecPkgInfo) <> 0 then
        raise ESynSSPI.CreateLastOSError(aSecContext);
    end;
    try
      if AcquireCredentialsHandleW(nil, SecPkgInfo^.Name, SECPKG_CRED_INBOUND, nil, nil, nil, nil, @aSecContext.CredHandle, Expiry) <> 0 then
        raise ESynSSPI.CreateLastOSError(aSecContext);
    finally
      FreeContextBuffer(SecPkgInfo);
    end;
    LInCtxPtr := nil;
  end else
    LInCtxPtr := @aSecContext.CtxHandle;

  OutBuf.BufferType := SECBUFFER_TOKEN;
  OutBuf.cbBuffer := 0;
  OutBuf.pvBuffer := nil;
  OutDesc.ulVersion := SECBUFFER_VERSION;
  OutDesc.cBuffers := 1;
  OutDesc.pBuffers := @OutBuf;

  Status := AcceptSecurityContext(@aSecContext.CredHandle, LInCtxPtr, @InDesc,
      ASC_REQ_ALLOCATE_MEMORY or ASC_REQ_CONFIDENTIALITY,
      SECURITY_NATIVE_DREP, @aSecContext.CtxHandle, @OutDesc, CtxAttr, Expiry);

  Result := (Status = SEC_I_CONTINUE_NEEDED) or (Status = SEC_I_COMPLETE_AND_CONTINUE);

  if (Status = SEC_I_COMPLETE_NEEDED) or (Status = SEC_I_COMPLETE_AND_CONTINUE) then
    Status := CompleteAuthToken(@aSecContext.CtxHandle, @OutDesc);
  if Status < 0 then
      raise ESynSSPI.CreateLastOSError(aSecContext);

  SetString(aOutData, PAnsiChar(OutBuf.pvBuffer), OutBuf.cbBuffer);
  FreeContextBuffer(OutBuf.pvBuffer);
end;

procedure ServerSSPIAuthUser(var aSecContext: TSecContext; out aUserName: RawUTF8);
var Names: SecPkgContext_NamesW;
begin
  if QueryContextAttributesW(@aSecContext.CtxHandle, SECPKG_ATTR_NAMES, @Names) <> 0 then
    raise ESynSSPI.CreateLastOSError(aSecContext);
  aUserName := RawUnicodeToUtf8(Names.sUserName, StrLenW(Names.sUserName));
  FreeContextBuffer(Names.sUserName);
end;

function SecPackageName(var aSecContext: TSecContext): RawUTF8;
var NegotiationInfo: TSecPkgContext_NegotiationInfo;
begin
  if QueryContextAttributesW(@aSecContext.CtxHandle, SECPKG_ATTR_NEGOTIATION_INFO, @NegotiationInfo) <> 0 then
    raise ESynSSPI.CreateLastOSError(aSecContext);
  Result := RawUnicodeToUtf8(NegotiationInfo.PackageInfo^.Name, StrLenW(NegotiationInfo.PackageInfo^.Name));
  FreeContextBuffer(NegotiationInfo.PackageInfo);
end;

procedure ClientForceSPN(const aSecKerberosSPN: RawUTF8);
begin
  ForceSecKerberosSPN := UTF8ToSynUnicode(aSecKerberosSPN);
end;

procedure ServerForceNTLM(IsNTLM: boolean);
begin
  if IsNTLM then begin
    SECPKGNAMEHTTPWWWAUTHENTICATE := 'WWW-Authenticate: NTLM';
    SECPKGNAMEHTTPAUTHORIZATION := 'AUTHORIZATION: NTLM ';
  end else begin
    SECPKGNAMEHTTPWWWAUTHENTICATE := 'WWW-Authenticate: Negotiate';
    SECPKGNAMEHTTPAUTHORIZATION := 'AUTHORIZATION: NEGOTIATE ';
  end;
end;

initialization
  ServerForceNTLM(False);

{$else}

implementation

{$endif MSWINDOWS} // compiles as void unit for non-Windows - allow Lazarus package

end.
