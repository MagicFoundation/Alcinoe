/// Low-level access to GSSAPI for Linux 32/64-bit platform.
// Both MIT and Heimdal implementation libraries are supported
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynGSSAPI;
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

  The Initial Developer of the Original Code is pavelmash/ssoftpro.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):
   Arnaud Bouchez
   ssoftpro
   Chaa

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
  Classes;

type
  gss_name_t = Pointer;
  gss_name_t_ptr = ^gss_name_t;
  gss_cred_id_t = Pointer;
  gss_ctx_id_t = Pointer;

  gss_OID_desc = record
    length: PtrUInt;
    elements: Pointer;
  end;
  gss_OID = ^gss_OID_desc;
  gss_OID_ptr = ^gss_OID;
  gss_OID_array = array [0..0] of gss_OID_desc;
  gss_OID_descs = ^gss_OID_array;

  gss_OID_set_desc = record
    count: PtrUInt;
    elements: gss_OID_descs;
  end;
  gss_OID_set = ^gss_OID_set_desc;
  gss_OID_set_ptr = ^gss_OID_set;

  gss_buffer_desc = record
    length: PtrUInt;
    value: Pointer;
  end;
  gss_buffer_t = ^gss_buffer_desc;

const
  GSS_C_NO_NAME = nil;

  GSS_C_GSS_CODE  = 1;
  GSS_C_MECH_CODE = 2;

  GSS_C_INDEFINITE = $FFFFFFFF;

  GSS_C_BOTH      = 0;
  GSS_C_INITIATE  = 1;
  GSS_C_ACCEPT    = 2;

  GSS_C_MUTUAL_FLAG = 2;
  GSS_C_CONF_FLAG = 16;
  GSS_C_INTEG_FLAG = 32;

  GSS_C_CALLING_ERROR_OFFSET = 24;
  GSS_C_ROUTINE_ERROR_OFFSET = 16;
  GSS_C_SUPPLEMENTARY_OFFSET =  0;
  GSS_C_CALLING_ERROR_MASK = $ff;
  GSS_C_ROUTINE_ERROR_MASK = $ff;
  GSS_C_SUPPLEMENTARY_MASK = $ffff;

  GSS_S_CONTINUE_NEEDED = 1 shl (GSS_C_SUPPLEMENTARY_OFFSET + 0);
  GSS_S_DUPLICATE_TOKEN = 1 shl (GSS_C_SUPPLEMENTARY_OFFSET + 1);
  GSS_S_OLD_TOKEN       = 1 shl (GSS_C_SUPPLEMENTARY_OFFSET + 2);
  GSS_S_UNSEQ_TOKEN     = 1 shl (GSS_C_SUPPLEMENTARY_OFFSET + 3);
  GSS_S_GAP_TOKEN       = 1 shl (GSS_C_SUPPLEMENTARY_OFFSET + 4);

  gss_mech_spnego: array [0..5] of Byte = (43, 6, 1, 5, 5, 2);
  gss_mech_spnego_desc: gss_OID_desc = (length: Length(gss_mech_spnego); elements: @gss_mech_spnego);
  GSS_C_MECH_SPNEGO: gss_OID = @gss_mech_spnego_desc;

  gss_nt_krb5_name: array [0..9] of Byte = (42, 134, 72, 134, 247, 18, 1, 2, 2, 1);
  gss_nt_krb5_name_desc: gss_OID_desc = (length: Length(gss_nt_krb5_name); elements: @gss_nt_krb5_name);
  GSS_KRB5_NT_PRINCIPAL_NAME: gss_OID = @gss_nt_krb5_name_desc;

  gss_nt_user_name: array [0..9] of Byte = (42, 134, 72, 134, 247, 18, 1, 2, 1, 1);
  gss_nt_user_name_desc: gss_OID_desc = (length: Length(gss_nt_user_name); elements: @gss_nt_user_name);
  GSS_C_NT_USER_NAME: gss_OID = @gss_nt_user_name_desc;

var
  gss_import_name: function (
    out minor_status: Cardinal;
    input_name_buffer: gss_buffer_t;
    input_name_type: gss_OID;
    out output_name: gss_name_t): Cardinal; cdecl;

  gss_display_name: function (
    out minor_status: Cardinal;
    input_name: gss_name_t;
    output_name_buffer: gss_buffer_t;
    output_name_type: gss_OID_ptr): Cardinal; cdecl;

  gss_release_name: function (
    out minor_status: Cardinal;
    var name: gss_name_t): Cardinal; cdecl;

  gss_acquire_cred: function (
    out minor_status: Cardinal;
    desired_name: gss_name_t;
    time_req: Cardinal;
    desired_mechs: gss_OID_set;
    cred_usage: Integer;
    out output_cred_handle: gss_cred_id_t;
    actual_mechs: gss_OID_set_ptr;
    time_rec: PCardinal): Cardinal; cdecl;

  gss_acquire_cred_with_password: function (
    out minor_status: Cardinal;
    desired_name: gss_name_t;
    password: gss_buffer_t;
    time_req: Cardinal;
    desired_mechs: gss_OID_set;
    cred_usage: Integer;
    out output_cred_handle: gss_cred_id_t;
    actual_mechs: gss_OID_set_ptr;
    time_rec: PCardinal): Cardinal; cdecl;

  gss_release_cred: function (
    out minor_status: Cardinal;
    var cred_handle: gss_cred_id_t): Cardinal; cdecl;

  gss_init_sec_context: function (
    out minor_status: Cardinal;
    initiator_cred_handle: gss_cred_id_t;
    var context_handle: gss_ctx_id_t;
    target_name: gss_name_t;
    mech_type: gss_OID;
    req_flags: Cardinal;
    time_req: Cardinal;
    input_chan_bindings: Pointer;
    input_token: gss_buffer_t;
    actual_mech_type: gss_OID_ptr;
    output_token: gss_buffer_t;
    ret_flags: PCardinal;
    time_rec: PCardinal): Cardinal; cdecl;

  gss_accept_sec_context: function (
    out minor_status: Cardinal;
    var context_handle: Pointer;
    acceptor_cred_handle: Pointer;
    input_token_buffer: gss_buffer_t;
    input_chan_bindings: Pointer;
    src_name: gss_name_t;
    mech_type: gss_OID_ptr;
    output_token: gss_buffer_t;
    ret_flags: PCardinal;
    time_rec: PCardinal;
    delegated_cred_handle: PPointer): Cardinal; cdecl;

  gss_inquire_context: function (
    out minor_status: Cardinal;
    context_handle: gss_ctx_id_t;
    src_name: gss_name_t_ptr;
    targ_name: gss_name_t_ptr;
    lifetime_rec: PCardinal;
    mech_type: gss_OID_ptr;
    ctx_flags: PCardinal;
    locally_initiated: PInteger;
    open: PInteger): Cardinal; cdecl;

  gss_delete_sec_context: function (
    out minor_status: Cardinal;
    var gss_context: gss_ctx_id_t;
    buffer: gss_buffer_t): Cardinal; cdecl;

  gss_inquire_saslname_for_mech: function (
    out minor_status: Cardinal;
    desired_mech: gss_OID;
    sasl_mech_name: gss_buffer_t;
    mech_name: gss_buffer_t;
    mech_description: gss_buffer_t): Cardinal; cdecl;

  gss_release_buffer: function (
    out minor_status: Cardinal;
    var buffer: gss_buffer_desc): Cardinal; cdecl;

  gss_wrap: function (
    out minor_status: Cardinal;
    context_handle: gss_ctx_id_t;
    conf_req_flag: Integer;
    qop_req: Cardinal;
    input_message_buffer: gss_buffer_t;
    conf_state: PInteger;
    output_message_buffer: gss_buffer_t): Cardinal; cdecl;

  gss_unwrap: function (
    out minor_status: Cardinal;
    context_handle: gss_ctx_id_t;
    input_message_buffer: gss_buffer_t;
    output_message_buffer: gss_buffer_t;
    conf_state: PInteger;
    qop_state: PCardinal): Cardinal; cdecl;

  gss_indicate_mechs: function (
    out minor_status: Cardinal;
    out mech_set: gss_OID_set): Cardinal; cdecl;

  gss_release_oid_set: function (
    out minor_status: Cardinal;
    out mech_set: gss_OID_set): Cardinal; cdecl;

  gss_display_status: function (
    out minor_status: Cardinal;
    status: Cardinal;
    status_type: Integer;
    mech_type: gss_OID;
    out message_context: Cardinal;
    out status_string: gss_buffer_desc): Cardinal; cdecl;

  krb5_gss_register_acceptor_identity: function (
    path: PAnsiChar): Cardinal; cdecl;

function gss_compare_oid(oid1, oid2: gss_OID): Boolean;

var
  /// library name of the MIT implementation of GSSAPI
  GSSLib_MIT: string = 'libgssapi_krb5.so.2';
  /// library name of the Heimdal implementation of GSSAPI
  GSSLib_Heimdal: string = 'libgssapi.so.3';

// High-level wrappers

type
  /// Exception raised during gssapi library process
  ESynGSSAPI = class(Exception)
  private
    FMajorStatus: Cardinal;
    FMinorStatus: Cardinal;
  public
    /// initialize an gssapi library exception
    constructor Create(AMajorStatus, AMinorStatus: Cardinal; const APrefix: String);
    /// associated GSS_C_GSS_CODE state value
    property MajorStatus: Cardinal read FMajorStatus;
    /// associated GSS_C_MECH_CODE state value
    property MinorStatus: Cardinal read FMinorStatus;
  end;

  {$ifdef HASCODEPAGE}
  TGSSAPIBuffer = RawByteString;
  {$else}
  TGSSAPIBuffer = AnsiString;
  {$endif}

  /// Auth context
  TSecContext = record
    ID: Int64;
    CredHandle: Pointer;
    CtxHandle: Pointer;
    CreatedTick64: Int64;
  end;
  PSecContext = ^TSecContext;

  /// dynamic array of Auth contexts
  // - used to hold information between calls to ServerSSPIAuth
  TSecContextDynArray = array of TSecContext;

/// Sets aSecHandle fields to empty state for a given connection ID
procedure InvalidateSecContext(var aSecContext: TSecContext; aConnectionID: Int64);

/// Free aSecContext on client or server side
procedure FreeSecContext(var aSecContext: TSecContext);

/// Encrypts a message
// - aSecContext must be set e.g. from previous success call to ServerSSPIAuth
// or ClientSSPIAuth
// - aPlain contains data that must be encrypted
// - returns encrypted message
function SecEncrypt(var aSecContext: TSecContext; const aPlain: TGSSAPIBuffer): TGSSAPIBuffer;

/// Decrypts a message
// - aSecContext must be set e.g. from previous success call to ServerSSPIAuth
// or ClientSSPIAuth
// - aEncrypted contains data that must be decrypted
// - returns decrypted message
function SecDecrypt(var aSecContext: TSecContext; const aEncrypted: TGSSAPIBuffer): TGSSAPIBuffer;

/// Checks the return value of GSSAPI call and raises ESynGSSAPI exception
// when it indicates failure
procedure GSSCheck(AMajorStatus, AMinorStatus: Cardinal; const APrefix: String = '');

/// Lists supported security mechanisms in form
// sasl:name:description
// - not all mechanisms provide human readable name and description
procedure GSSEnlistMechsSupported(MechList: TStringList);

/// Dynamically load GSSAPI library
// - in multithreaded server application you must call LoadGSSAPI
//   at startup to avoid race condition (if you do not use mORMot.pas)
procedure LoadGSSAPI;

/// Call this function to check whether GSSAPI library loaded or not
function GSSAPILoaded: Boolean;

/// Call this function to check whether GSSAPI library loaded
// and raise exception if not.
procedure RequireGSSAPI;

implementation

var
  GSSAPILibrary: {$ifdef FPC}TLibHandle{$else}HMODULE{$endif};

/// The macros that test status codes for error conditions. Note that the
// GSS_ERROR() macro has changed slightly from the V1 GSSAPI so that it now
// evaluates its argument only once.

function GSS_CALLING_ERROR(x: Cardinal): Cardinal; inline;
begin
  Result := x and (GSS_C_CALLING_ERROR_MASK shl GSS_C_CALLING_ERROR_OFFSET);
end;

function GSS_ROUTINE_ERROR(x: Cardinal): Cardinal; inline;
begin
  Result := x and (GSS_C_ROUTINE_ERROR_MASK shl GSS_C_ROUTINE_ERROR_OFFSET);
end;

function GSS_SUPPLEMENTARY_INFO(x: Cardinal): Cardinal; inline;
begin
  Result := x and (GSS_C_SUPPLEMENTARY_MASK shl GSS_C_SUPPLEMENTARY_OFFSET);
end;

function GSS_ERROR(x: Cardinal): Cardinal; inline;
begin
  Result := x and
    ((GSS_C_CALLING_ERROR_MASK shl GSS_C_CALLING_ERROR_OFFSET) or
     (GSS_C_ROUTINE_ERROR_MASK shl GSS_C_ROUTINE_ERROR_OFFSET));
end;

procedure GSSCheck(AMajorStatus, AMinorStatus: Cardinal; const APrefix: String = '');
begin
  if GSS_ERROR(AMajorStatus) <> 0 then
    raise ESynGSSAPI.Create(AMajorStatus, AMinorStatus, APrefix);
end;

procedure LoadGSSAPI;
var
  LibHandle: {$ifdef FPC}TLibHandle{$else}HMODULE{$endif};
  UseHeimdal: Boolean;
begin
  if GSSAPILibrary=0 then begin
    LibHandle := SafeLoadLibrary(GSSLib_MIT);
    UseHeimdal := LibHandle=0;
    if UseHeimdal then
      LibHandle := SafeLoadLibrary(GSSLib_Heimdal);
    if LibHandle<>0 then begin
      gss_import_name := GetProcAddress(LibHandle, 'gss_import_name');
      gss_display_name := GetProcAddress(LibHandle, 'gss_display_name');
      gss_release_name := GetProcAddress(LibHandle, 'gss_release_name');
      gss_acquire_cred := GetProcAddress(LibHandle, 'gss_acquire_cred');
      gss_acquire_cred_with_password := GetProcAddress(LibHandle, 'gss_acquire_cred_with_password');
      gss_release_cred := GetProcAddress(LibHandle, 'gss_release_cred');
      gss_init_sec_context := GetProcAddress(LibHandle, 'gss_init_sec_context');
      gss_accept_sec_context := GetProcAddress(LibHandle, 'gss_accept_sec_context');
      gss_inquire_context := GetProcAddress(LibHandle, 'gss_inquire_context');
      gss_delete_sec_context := GetProcAddress(LibHandle, 'gss_delete_sec_context');
      gss_inquire_saslname_for_mech := GetProcAddress(LibHandle, 'gss_inquire_saslname_for_mech');
      gss_release_buffer := GetProcAddress(LibHandle, 'gss_release_buffer');
      gss_wrap := GetProcAddress(LibHandle, 'gss_wrap');
      gss_unwrap := GetProcAddress(LibHandle, 'gss_unwrap');
      gss_indicate_mechs := GetProcAddress(LibHandle, 'gss_indicate_mechs');
      gss_release_oid_set := GetProcAddress(LibHandle, 'gss_release_oid_set');
      gss_display_status := GetProcAddress(LibHandle, 'gss_display_status');
      krb5_gss_register_acceptor_identity := GetProcAddress(LibHandle, 'krb5_gss_register_acceptor_identity');
      if not Assigned(krb5_gss_register_acceptor_identity) then
        krb5_gss_register_acceptor_identity := GetProcAddress(LibHandle, 'gsskrb5_register_acceptor_identity');
      // At least it should work in server
      if Assigned(gss_acquire_cred) and Assigned(gss_accept_sec_context)
        and Assigned(gss_release_buffer) and Assigned(gss_inquire_context)
        and Assigned(gss_display_name) and Assigned(gss_release_name) then
      begin
        GSSAPILibrary := LibHandle
      end else
        FreeLibrary(LibHandle);
    end;
  end;
end;

function GSSAPILoaded: Boolean;
begin
  Result := GSSAPILibrary<>0;
end;

procedure RequireGSSAPI;
begin
  if GSSAPILibrary=0 then
    raise ENotSupportedException.Create(
      'No GSSAPI library found - please install ' +
      'either MIT or Heimdal GSSAPI implementation');
end;

function gss_compare_oid(oid1, oid2: gss_OID): Boolean;
begin
  if (oid1<>nil) and (oid2<>nil) then begin
    Result := (oid1^.length = oid2^.length) and
              CompareMem(oid1^.elements, oid2^.elements, oid1^.length);
  end
  else
    Result := False;
end;

procedure GSSEnlistMechsSupported(MechList: TStringList);
var
  i, MajSt, MinSt: Cardinal;
  Mechs: gss_OID_set;
  Buf_sasl, Buf_name, Buf_desc: gss_buffer_desc;
  Sasl, Name, Desc: String;
begin
  RequireGSSAPI;
  if MechList <> nil then begin
    MajSt := gss_indicate_mechs(MinSt, Mechs);
    for i := 0 to Pred(Mechs^.count) do begin
      MajSt := gss_inquire_saslname_for_mech(MinSt, @Mechs^.elements[i], @Buf_sasl, @Buf_name, @Buf_desc);
      SetString(Sasl, Buf_sasl.value, Buf_sasl.length);
      SetString(Name, Buf_name.value, Buf_name.length);
      SetString(Desc, Buf_desc.value, Buf_desc.length);
      MechList.Add(Format('%s:%s:%s', [Sasl, Name, Desc]));
      gss_release_buffer(MinSt, Buf_sasl);
      gss_release_buffer(MinSt, Buf_name);
      gss_release_buffer(MinSt, Buf_desc);
    end;
    MajSt := gss_release_oid_set(MinSt, Mechs);
  end;
end;

{ ESynGSSAPI }

constructor ESynGSSAPI.Create(AMajorStatus, AMinorStatus: Cardinal; const APrefix: String);

  procedure GetDisplayStatus(var Msg: String; AErrorStatus: Cardinal; StatusType: Integer);
  var
    Str: String;
    MsgCtx: Cardinal;
    MsgBuf: gss_buffer_desc;
    MajSt, MinSt: Cardinal;
  begin
    MsgCtx := 0;
    repeat
      MajSt := gss_display_status(
        MinSt, AErrorStatus, StatusType, nil, MsgCtx, MsgBuf);
      SetString(Str, MsgBuf.value, MsgBuf.length);
      gss_release_buffer(MinSt, MsgBuf);
      if Msg <> '' then
        Msg := Msg + ': ' + Str
      else
        Msg := Str;
    until (GSS_ERROR(MajSt) <> 0) or (MsgCtx = 0);
  end;

var
  Msg: String;
begin
  Msg := APrefix;
  GetDisplayStatus(Msg, AMajorStatus, GSS_C_GSS_CODE);
  if AMinorStatus <> 0 then
    GetDisplayStatus(Msg, AMinorStatus, GSS_C_MECH_CODE);
  inherited Create(Msg);
  FMajorStatus := AMajorStatus;
  FMinorStatus := AMinorStatus;
end;

procedure InvalidateSecContext(var aSecContext: TSecContext; aConnectionID: Int64);
begin
  aSecContext.ID := aConnectionID;
  aSecContext.CredHandle := nil;
  aSecContext.CtxHandle := nil;
  aSecContext.CreatedTick64 := 0;
end;

procedure FreeSecContext(var aSecContext: TSecContext);
var MinStatus: Cardinal;
begin
  if aSecContext.CtxHandle <> nil then
    gss_delete_sec_context(MinStatus, aSecContext.CtxHandle, nil);
  if aSecContext.CredHandle <> nil then
    gss_release_cred(MinStatus, aSecContext.CredHandle);
end;

function SecEncrypt(var aSecContext: TSecContext; const aPlain: TGSSAPIBuffer): TGSSAPIBuffer;
var MajStatus, MinStatus: Cardinal;
    InBuf: gss_buffer_desc;
    OutBuf: gss_buffer_desc;
begin
  InBuf.length := Length(aPlain);
  InBuf.value := Pointer(aPlain);

  MajStatus := gss_wrap(MinStatus, aSecContext.CtxHandle, 1, 0, @InBuf, nil, @OutBuf);
  GSSCheck(MajStatus, MinStatus, 'Failed to encrypt message');

  SetString(Result, PAnsiChar(OutBuf.value), OutBuf.length);
  gss_release_buffer(MinStatus, OutBuf);
end;

function SecDecrypt(var aSecContext: TSecContext; const aEncrypted: TGSSAPIBuffer): TGSSAPIBuffer;
var MajStatus, MinStatus: Cardinal;
    InBuf: gss_buffer_desc;
    OutBuf: gss_buffer_desc;
begin
  InBuf.length := Length(aEncrypted);
  InBuf.value := Pointer(aEncrypted);

  MajStatus := gss_unwrap(MinStatus, aSecContext.CtxHandle, @InBuf, @OutBuf, nil, nil);
  GSSCheck(MajStatus, MinStatus, 'Failed to decrypt message');

  SetString(Result, PAnsiChar(OutBuf.value), OutBuf.length);
  gss_release_buffer(MinStatus, OutBuf);
end;

finalization
  if GSSAPILibrary<>0 then begin
    FreeLibrary(GSSAPILibrary);
    GSSAPILibrary := 0;
  end;
end.
