/// Low-level access to GSSAPI Authentication for Linux 32/64-bit platform
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynGSSAPIAuth;
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

uses
  SysUtils,
  SynCommons,
  SynGSSAPI;

/// Client-side authentication procedure
// - aSecContext holds information between function calls
// - aInData contains data received from server
// - aSecKerberosSPN is the Service Principal Name,
// registered in domain, e.g.
// 'mymormotservice/myserver.mydomain.tld@MYDOMAIN.TLD'
// - aOutData contains data that must be sent to server
// - if function returns True, client must send aOutData to server
// and call function again with data, returned from server
function ClientSSPIAuth(var aSecContext: TSecContext;
    const aInData: RawByteString; const aSecKerberosSPN: RawUTF8;
    out aOutData: RawByteString): Boolean;

/// Client-side authentication procedure with clear text password.
//  This function must be used when application need to use different
//  user credentials (not credentials of logged in user)
// - aSecContext holds information between function calls
// - aInData contains data received from server
// - aUserName is the domain and user name, in form of
// 'username@MYDOMAIN.TLD'. Note that domain name requires to be in upper case.
// - aPassword is the user clear text password
// - aOutData contains data that must be sent to server
// - if function returns True, client must send aOutData to server
// and call function again with data, returned from server
// - you must use ClientForceSPN to specify server SPN before call
function ClientSSPIAuthWithPassword(var aSecContext: TSecContext;
    const aInData: RawByteString; const aUserName: RawUTF8;
    const aPassword: RawUTF8; out aOutData: RawByteString): Boolean;

/// Server-side authentication procedure
// - aSecContext holds information between function calls
// - aInData contains data received from client
// - aOutData contains data that must be sent to client
// - if function returns True, server must send aOutData to client
// and call function again with data, returned from client
function ServerSSPIAuth(var aSecContext: TSecContext;
    const aInData: RawByteString; out aOutData: RawByteString): Boolean;

/// Server-side function that returns authenticated user name
// - aSecContext must be received from previous successful call to ServerSSPIAuth
// - aUserName contains authenticated user name
procedure ServerSSPIAuthUser(var aSecContext: TSecContext; out aUserName: RawUTF8);

/// Returns name of the security package that has been used with the negotiation process
// - aSecContext must be received from previous success call to ServerSSPIAuth
// or ClientSSPIAuth
function SecPackageName(var aSecContext: TSecContext): RawUTF8;

/// Force using aSecKerberosSPN for server identification
// - aSecKerberosSPN is the Service Principal Name, registered in domain, e.g.
// 'mymormotservice/myserver.mydomain.tld@MYDOMAIN.TLD'
procedure ClientForceSPN(const aSecKerberosSPN: RawUTF8);

/// Force loading server credentials from specified keytab file
// - by default, clients may authenticate to any service principal
// in the default keytab (/etc/krb5.keytab or the value of the KRB5_KTNAME
// environment variable)
procedure ServerForceKeytab(const aKeytab: RawUTF8);

const
  /// HTTP header to be set for authentication
  SECPKGNAMEHTTPWWWAUTHENTICATE = 'WWW-Authenticate: Negotiate';
  /// HTTP header pattern received for authentication
  SECPKGNAMEHTTPAUTHORIZATION = 'AUTHORIZATION: NEGOTIATE ';

var
  /// Dictionary for converting fully qualified domain names to NT4-style NetBIOS names
  // - to use same value for TSQLAuthUser.LogonName on all platforms user name
  // changed from 'username@MYDOMAIN.TLD' to 'MYDOMAIN\username'
  // - when converting fully qualified domain name to NT4-style NetBIOS name
  // ServerDomainMap first checked. If domain name not found, then it's truncated on first dot,
  // e.g. 'user1@CORP.ABC.COM' changed to 'CORP\user1'
  // - you can change domain name conversion by registering names at server startup, e.g.
  // ServerDomainMap.Add('CORP.ABC.COM', 'ABCCORP') change conversion for previuos
  // example to 'ABCCORP\user1'
  // - use only if automatic conversion (truncate on first dot) do it wrong
  ServerDomainMap: TSynNameValue;



implementation

var
  ForceSecKerberosSPN: RawUTF8;

function ClientSSPIAuthWorker(var aSecContext: TSecContext;
    const aInData: RawByteString; const aSecKerberosSPN: RawUTF8;
    out aOutData: RawByteString): Boolean;
var TargetName: gss_name_t;
    MajStatus, MinStatus: Cardinal;
    InBuf: gss_buffer_desc;
    OutBuf: gss_buffer_desc;
    CtxReqAttr: Cardinal;
    CtxAttr: Cardinal;
begin
  TargetName := nil;
  if aSecKerberosSPN <> '' then begin
    InBuf.length := Length(aSecKerberosSPN);
    InBuf.value := Pointer(aSecKerberosSPN);
    MajStatus := gss_import_name(MinStatus, @InBuf, GSS_KRB5_NT_PRINCIPAL_NAME, TargetName);
    GSSCheck(MajStatus, MinStatus, 'Failed to import server SPN');
  end;

  try
    CtxReqAttr := GSS_C_INTEG_FLAG or GSS_C_CONF_FLAG;
    if TargetName <> nil then
      CtxReqAttr := CtxReqAttr or GSS_C_MUTUAL_FLAG;

    InBuf.length := Length(aInData);
    InBuf.value := Pointer(aInData);

    OutBuf.length := 0;
    OutBuf.value := nil;

    MajStatus := gss_init_sec_context(MinStatus, aSecContext.CredHandle,
      aSecContext.CtxHandle, TargetName, GSS_C_MECH_SPNEGO,
      CtxReqAttr, GSS_C_INDEFINITE, nil, @InBuf, nil, @OutBuf, @CtxAttr, nil);
    GSSCheck(MajStatus, MinStatus, 'Failed to initialize security context');

    Result := (MajStatus and GSS_S_CONTINUE_NEEDED) <> 0;

    SetString(aOutData, PAnsiChar(OutBuf.value), OutBuf.length);
    gss_release_buffer(MinStatus, OutBuf);
  finally
    if TargetName <> nil then
      gss_release_name(MinStatus, TargetName);
  end;
end;

function ClientSSPIAuth(var aSecContext: TSecContext;
    const aInData: RawByteString; const aSecKerberosSPN: RawUTF8;
    out aOutData: RawByteString): Boolean;
var MajStatus, MinStatus: Cardinal;
    SecKerberosSPN: RawUTF8;
begin
  RequireGSSAPI;
  if aSecContext.CredHandle = nil then begin
    aSecContext.CreatedTick64 := GetTickCount64();
    MajStatus := gss_acquire_cred(MinStatus, nil, GSS_C_INDEFINITE, nil,
      GSS_C_INITIATE, aSecContext.CredHandle, nil, nil);
    GSSCheck(MajStatus, MinStatus, 'Failed to acquire credentials for current user');
  end;
  if aSecKerberosSPN<>'' then
    SecKerberosSPN := aSecKerberosSPN else
    SecKerberosSPN := ForceSecKerberosSPN;
  Result := ClientSSPIAuthWorker(aSecContext, aInData, SecKerberosSPN, aOutData);
end;

function ClientSSPIAuthWithPassword(var aSecContext: TSecContext;
    const aInData: RawByteString; const aUserName: RawUTF8;
    const aPassword: RawUTF8; out aOutData: RawByteString): Boolean;
var MajStatus, MinStatus: Cardinal;
    InBuf: gss_buffer_desc;
    UserName: gss_name_t;
begin
  RequireGSSAPI;
  if aSecContext.CredHandle = nil then begin
    aSecContext.CreatedTick64 := GetTickCount64();
    InBuf.length := Length(aUserName);
    InBuf.value := Pointer(aUserName);
    MajStatus := gss_import_name(MinStatus, @InBuf, GSS_KRB5_NT_PRINCIPAL_NAME, UserName);
    GSSCheck(MajStatus, MinStatus, 'Failed to import UserName');
    InBuf.length := Length(aPassword);
    InBuf.value := Pointer(aPassword);
    MajStatus := gss_acquire_cred_with_password(MinStatus, UserName, @InBuf,
      GSS_C_INDEFINITE, nil, GSS_C_INITIATE, aSecContext.CredHandle, nil, nil);
    GSSCheck(MajStatus, MinStatus, 'Failed to acquire credentials for specified user');
  end;
  Result := ClientSSPIAuthWorker(aSecContext, aInData, ForceSecKerberosSPN, aOutData);
end;

function ServerSSPIAuth(var aSecContext: TSecContext;
  const aInData: RawByteString; out aOutData: RawByteString): Boolean;
var MajStatus, MinStatus: Cardinal;
    InBuf: gss_buffer_desc;
    OutBuf: gss_buffer_desc;
    CtxAttr: Cardinal;
begin
  RequireGSSAPI;

  if aSecContext.CredHandle = nil then begin
    aSecContext.CreatedTick64 := GetTickCount64();
    if IdemPChar(Pointer(aInData), 'NTLMSSP') then
      raise ESynGSSAPI.CreateFmt('NTLM authentication not supported by GSSAPI library',[]);
    MajStatus := gss_acquire_cred(MinStatus, nil, GSS_C_INDEFINITE, nil,
      GSS_C_ACCEPT, aSecContext.CredHandle, nil, nil);
    GSSCheck(MajStatus, MinStatus, 'Failed to aquire credentials for service');
  end;

  InBuf.length := Length(aInData);
  InBuf.value := PByte(aInData);

  OutBuf.length := 0;
  OutBuf.value := nil;

  MajStatus := gss_accept_sec_context(MinStatus, aSecContext.CtxHandle,
    aSecContext.CredHandle, @InBuf, nil, nil, nil, @OutBuf, @CtxAttr, nil, nil);
  GSSCheck(MajStatus, MinStatus, 'Failed to accept client credentials');

  Result := (MajStatus and GSS_S_CONTINUE_NEEDED) <> 0;

  SetString(aOutData, PAnsiChar(OutBuf.value), OutBuf.length);
  gss_release_buffer(MinStatus, OutBuf);
end;

procedure ConvertUserName(P: PUTF8Char; Len: PtrUInt; out aUserName: RawUTF8);
var DomainStart, DomainEnd: PUTF8Char;
    Index: Integer;
    Domain: RawUTF8;
begin
  // Assume GSSAPI buffer is null-terminated
  Assert(P[Len] = #0);
  // Change user name from 'username@MYDOMAIN.TLD' to 'MYDOMAIN\username'
  DomainStart := PosChar(P, '@');
  if DomainStart<>nil then begin
    DomainStart^ := #0;
    Inc(DomainStart);
    Index := ServerDomainMap.Find(DomainStart);
    if Index<0 then begin
      DomainEnd := PosChar(DomainStart, '.');
      if DomainEnd<>nil then
        DomainEnd^ := #0;
      Domain := DomainStart;
    end else
      Domain := ServerDomainMap.List[Index].Value;
    aUserName := FormatUTF8('%\%', [Domain, P]);
  end else
    // Unknown user name format, leave as is
    SetString(aUserName, P, Len);
end;

procedure ServerSSPIAuthUser(var aSecContext: TSecContext; out aUserName: RawUTF8);
var MajStatus, MinStatus: Cardinal;
    SrcName: gss_name_t;
    OutBuf: gss_buffer_desc;
    NameType: gss_OID;
begin
  RequireGSSAPI;
  MajStatus := gss_inquire_context(MinStatus, aSecContext.CtxHandle,
    @SrcName, nil, nil, nil, nil, nil, nil);
  GSSCheck(MajStatus, MinStatus, 'Failed to inquire security context information (src_name)');
  try
    OutBuf.length := 0;
    OutBuf.value := nil;
    MajStatus := gss_display_name(MinStatus, SrcName, @OutBuf, @NameType);
    GSSCheck(MajStatus, MinStatus, 'Failed to obtain name for authenticated user');
    if gss_compare_oid(NameType, GSS_KRB5_NT_PRINCIPAL_NAME) then begin
      ConvertUserName(PUTF8Char(OutBuf.value), OutBuf.length, aUserName);
    end;
    gss_release_buffer(MinStatus, OutBuf);
  finally
    gss_release_name(MinStatus, SrcName);
  end;
end;

function SecPackageName(var aSecContext: TSecContext): RawUTF8;
var MajStatus, MinStatus: Cardinal;
    MechType: gss_OID;
    OutBuf: gss_buffer_desc;
begin
  RequireGSSAPI;
  MajStatus := gss_inquire_context(MinStatus, aSecContext.CtxHandle,
    nil, nil, nil, @MechType, nil, nil, nil);
  GSSCheck(MajStatus, MinStatus, 'Failed to inquire security context information (mech_type)');
  OutBuf.length := 0;
  OutBuf.value := nil;
  MajStatus := gss_inquire_saslname_for_mech(MinStatus, MechType, nil, @OutBuf, nil);
  GSSCheck(MajStatus, MinStatus, 'Failed to obtain name for mech');
  SetString(Result, PAnsiChar(OutBuf.value), OutBuf.length);
  gss_release_buffer(MinStatus, OutBuf);
end;

procedure ClientForceSPN(const aSecKerberosSPN: RawUTF8);
begin
  ForceSecKerberosSPN := aSecKerberosSPN;
end;

procedure ServerForceKeytab(const aKeytab: RawUTF8);
begin
  if Assigned(krb5_gss_register_acceptor_identity) then begin
    krb5_gss_register_acceptor_identity(Pointer(aKeytab));
  end;
end;

initialization
  ServerDomainMap.Init({casesensitive=}false);
end.
