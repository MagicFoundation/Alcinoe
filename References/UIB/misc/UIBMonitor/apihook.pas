(* The contents of this file are subject to the Mozilla Public License
   Version 1.1 (the "License"); you may not use this file except in compliance
   with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
   Software distributed under the License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.
*)

unit apihook;
{$I uib.inc}
{$ALIGN ON}
{$MINENUMSIZE 4}
{.$define INTERCEPTALL}

interface
uses madCodeHook;

implementation
uses Windows, uibase, SysUtils, SharedHook, uibLib;

type
  TUIBHookKedLibrary = class(TUIBLibrary)
  protected
    function Loaded: Boolean; override;
    function Unload: Boolean; override;
    function Load(const lib: string = GDS32DLL): Boolean; override;
  end;

  PXSQLVAR_V1 = ^TXSQLVAR_V1;
  XSQLVAR_V1 = record
    sqltype: Smallint; // datatype of field
    sqlscale: Smallint; // scale factor
    sqlsubtype: Smallint; // datatype subtype - BLOBs & Text types only
    sqllen: Smallint; // length of data area
    sqldata: PChar; // address of data
    sqlind: PSmallint; // address of indicator variable
    sqlname_length: Smallint; // length of sqlname field
    sqlname: array [0..31] of Char; // name of field, name length + space for NULL
    relname_length: Smallint; // length of relation name
    relname: array [0..31] of Char; // field's relation name + space for NULL
    ownname_length: Smallint; // length of owner name
    ownname: array [0..31] of Char; // relation's owner name + space for  NULL
    aliasname_length: Smallint; // length of alias name
    aliasname: array [0..31] of Char; // relation's alias name + space for NULL
  end;
  TXSQLVAR_V1 = XSQLVAR_V1;

  PXSQLDA_V1 = ^TXSQLDA_V1;
  XSQLDA_V1 = record
    version: Smallint; // version of this XSQLDA
    sqldaid: array [0..7] of Char; // XSQLDA name field          ->  RESERVED
    sqldabc: ISCLong; // length in bytes of SQLDA   ->  RESERVED
    sqln: Smallint; // number of fields allocated
    sqld: Smallint; // actual number of fields
    sqlvar: array [0..0] of TXSQLVAR_V1; // first field address
  end;
  TXSQLDA_V1 = XSQLDA_V1;

  PXSQLVAR_V2 = ^TXSQLVAR_V2;
  XSQLVAR_V2 = record
    sqltype: Smallint; // datatype of field
    sqlscale: Smallint; // scale factor
    sqlprecision: Smallint; // precision : Reserved for future
    sqlsubtype: Smallint; // datatype subtype
    sqllen: Smallint; // length of data area
    sqldata: PChar; // address of data
    sqlind: PSmallint; // address of indicator variable
    sqlname_length: Smallint; // length of sqlname field
    sqlname: array [0..67] of Char; // name of field, name length + space  for NULL
    relname_length: Smallint; // length of relation name
    relname: array [0..67] of Char; // field's relation name + space for NULL
    ownname_length: Smallint; // length of owner name
    ownname: array [0..67] of Char; // relation's owner name + space for  NULL
    aliasname_length: Smallint; // length of alias name
    aliasname: array [0..67] of Char; // relation's alias name + space for NULL
  end;
  TXSQLVAR_V2 = XSQLVAR_V2;

  PXSQLDA_V2 = ^TXSQLDA_V2;
  XSQLDA_V2 = record
    version: Smallint; // version of this XSQLDA
    sqldaid: array [0..7] of Char; // XSQLDA name field          ->  RESERVED
    sqldabc: ISCLong; // length in bytes of SQLDA   ->  RESERVED
    sqln: Smallint; // number of fields allocated
    sqld: Smallint; // actual number of fields
    sqlvar: array [0..0] of TXSQLVAR_V2; // first field address
  end;
  TXSQLDA_V2 = XSQLDA_V2;

var
  HookedLib: TUIBHookKedLibrary;
  UIBHOOKIPC: string;
  IndexGenerator: Integer = 0;

function ParamsToString_V1(sqlda: PXSQLDA_V1): string;
var
  sqlvar: PXSQLVAR_V1;
  i: integer;
  ASQLCode: SmallInt;
  function DecodeString(const Code: Smallint): String;
  begin
    with sqlvar^ do
    case Code of
      SQL_TEXT    : SetString(Result, sqldata, sqllen);
      SQL_VARYING : SetString(Result, PVary(sqldata).vary_string, PVary(sqldata).vary_length);
    end;
    Result := '''' + Result + '''';
  end;
begin
  Result := '';
  for i := 0 to sqlda.sqld - 1 do
  begin
    sqlvar := @sqlda.sqlvar[i];
    with sqlvar^ do
    if (sqlind <> nil) and (sqlind^ = -1) then Result := Result + 'NULL'#13 else
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : Result := Result + FloatToStr(PSmallInt(sqldata)^ / ScaleDivisor[sqlscale]) + #13;
          SQL_LONG   : Result := Result + FloatToStr(PInteger(sqldata)^  / ScaleDivisor[sqlscale]) + #13;
          SQL_INT64,
          SQL_QUAD   : Result := Result + FloatToStr(PInt64(sqldata)^    / ScaleDivisor[sqlscale]) + #13;
          SQL_DOUBLE : Result := Result + FloatToStr(PDouble(sqldata)^) + #13;
        else
          Result := Result + '<DATA ERROR>'#13;
        end;
      end else
        case ASQLCode of
          SQL_VARYING   : Result := Result + DecodeString(SQL_VARYING) + #13;
          SQL_TEXT      : Result := Result + DecodeString(SQL_TEXT) + #13;
          SQL_TIMESTAMP : Result := Result + DateTimeToStr(DecodeTimeStamp(PISCTimeStamp(sqldata))) + #13;
          SQL_TYPE_DATE : Result := Result + DateToStr(PInteger(sqldata)^ - DateOffset) + #13;
          SQL_TYPE_TIME : Result := Result + TimeToStr(PCardinal(sqldata)^ / TimeCoeff) + #13;
          SQL_DOUBLE    : Result := Result + FloatToStr(PDouble(sqldata)^) + #13;
          SQL_LONG      : Result := Result + IntToStr(PInteger(sqldata)^) + #13;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Result + FloatToStr(PSingle(sqldata)^) + #13;
{$IFDEF IB7_UP}
          SQL_BOOLEAN   : Result := Result + BoolToStr(PSmallint(sqldata)^ = 1) + #13;
{$ENDIF}
          SQL_SHORT     : Result := Result + IntToStr(PSmallint(sqldata)^) + #13;
          SQL_INT64     : Result := Result + IntToStr(PInt64(sqldata)^) + #13;
        else
          Result := Result + '<DATA ERROR>'#13;
        end;
    end;
  end;
end;

function ParamsToString_V2(sqlda: PXSQLDA_V2): string;
var
  sqlvar: PXSQLVAR_V2;
  i: integer;
  ASQLCode: SmallInt;
  function DecodeString(const Code: Smallint): String;
  begin
    with sqlvar^ do
    case Code of
      SQL_TEXT    : SetString(Result, sqldata, sqllen);
      SQL_VARYING : SetString(Result, PVary(sqldata).vary_string, PVary(sqldata).vary_length);
    end;
    Result := '''' + Result + '''';
  end;
begin
  Result := '';
  for i := 0 to sqlda.sqld - 1 do
  begin
    sqlvar := @sqlda.sqlvar[i];
    with sqlvar^ do
    if (sqlind <> nil) and (sqlind^ = -1) then Result := Result + 'NULL'#13 else
    begin
      ASQLCode := (sqltype and not(1));
      // Is Numeric ?
      if (sqlscale < 0)  then
      begin
        case ASQLCode of
          SQL_SHORT  : Result := Result + FloatToStr(PSmallInt(sqldata)^ / ScaleDivisor[sqlscale]) + #13;
          SQL_LONG   : Result := Result + FloatToStr(PInteger(sqldata)^  / ScaleDivisor[sqlscale]) + #13;
          SQL_INT64,
          SQL_QUAD   : Result := Result + FloatToStr(PInt64(sqldata)^    / ScaleDivisor[sqlscale]) + #13;
          SQL_DOUBLE : Result := Result + FloatToStr(PDouble(sqldata)^) + #13;
        else
          Result := Result + '<DATA ERROR>'#13;
        end;
      end else
        case ASQLCode of
          SQL_VARYING   : Result := Result + DecodeString(SQL_VARYING) + #13;
          SQL_TEXT      : Result := Result + DecodeString(SQL_TEXT) + #13;
          SQL_TIMESTAMP : Result := Result + DateTimeToStr(DecodeTimeStamp(PISCTimeStamp(sqldata))) + #13;
          SQL_TYPE_DATE : Result := Result + DateToStr(PInteger(sqldata)^ - DateOffset) + #13;
          SQL_TYPE_TIME : Result := Result + TimeToStr(PCardinal(sqldata)^ / TimeCoeff) + #13;
          SQL_DOUBLE    : Result := Result + FloatToStr(PDouble(sqldata)^) + #13;
          SQL_LONG      : Result := Result + IntToStr(PInteger(sqldata)^) + #13;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Result + FloatToStr(PSingle(sqldata)^) + #13;
{$IFDEF IB7_UP}
          SQL_BOOLEAN   : Result := Result + BoolToStr(PSmallint(sqldata)^ = 1) + #13;
{$ENDIF}
          SQL_SHORT     : Result := Result + IntToStr(PSmallint(sqldata)^) + #13;
          SQL_INT64     : Result := Result + IntToStr(PInt64(sqldata)^) + #13;
        else
          Result := Result + '<DATA ERROR>'#13;
        end;
    end;
  end;
end;

//  next_isc_decode_date: procedure(date: PISCQuad; times: PPointer); {$IFDEF UNIX}cdecl;{$ELSE}stdcall;{$ENDIF}
//  next_isc_decode_sql_date: procedure(date: PISCDate; times_arg: PPointer); {$IFDEF UNIX}cdecl;{$ELSE}stdcall;{$ENDIF}
//  next_isc_decode_sql_time: procedure(sql_time: PISCTime; times_arg: PPointer); {$IFDEF UNIX}cdecl;{$ELSE}stdcall;{$ENDIF}
//  next_isc_decode_timestamp: procedure(date: PISCTimeStamp; times_arg:
//    PPointer); {$IFDEF UNIX}cdecl;{$ELSE}stdcall;{$ENDIF}
//next_isc_vax_integer: function(ptr: PChar; Length: Smallint): ISCLong; {$IFDEF UNIX}cdecl;{$ELSE}stdcall;{$ENDIF}


  //------------------------------------------------------------------------------

function hook_BLOB_close(Stream: PBStream): Integer; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}
  Result := HookedLib.BLOB_close(Stream);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_BLOB_close;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}
end;

{$IFDEF INTERBASEORFIREBIRD}

function hook_BLOB_display(blob_id: PISCQuad; database: IscDbHandle; transaction:
  IscTrHandle;
  field_name: PChar): Integer; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.BLOB_display(blob_id, database, transaction,
    field_name);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_BLOB_display;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;
{$ENDIF INTERBASEORFIREBIRD}

function hook_BLOB_dump(blob_id: PISCQuad; database: IscDbHandle; transaction:
  IscTrHandle;
  file_name: PChar): Integer; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.BLOB_dump(blob_id, database, transaction, file_name);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_BLOB_dump;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_BLOB_edit(blob_id: PISCQuad; database: IscDbHandle; transaction:
  IscTrHandle;
  field_name: PChar): Integer; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.BLOB_edit(blob_id, database, transaction, field_name);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_BLOB_edit;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_BLOB_get(stream: PBStream): Integer; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.BLOB_get(stream);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_BLOB_get;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_BLOB_load(blob_id: PISCQuad; database: IscDbHandle; transaction:
  IscTrHandle;
  file_name: PChar): Integer; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.BLOB_load(blob_id, database, transaction, file_name);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_BLOB_load;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_BLOB_open(blob: IscBlobHandle; buffer: PChar; Length: Integer):
  PBStream; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.BLOB_open(blob, buffer, Length);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_BLOB_open;
  header.threadid := GetCurrentThreadId;
  header.status := 0;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_BLOB_put(x: Char; stream: PBStream): Integer; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.BLOB_put(x, stream);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_BLOB_put;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_BLOB_text_dump(blob_id: PISCQuad; database: IscDbHandle; transaction:
  IscTrHandle;
  file_name: PChar): Integer; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.BLOB_text_dump(blob_id, database, transaction, file_name);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_BLOB_text_dump;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_BLOB_text_load(blob_id: PISCQuad; database: IscDbHandle; transaction:
  IscTrHandle;
  file_name: PChar): Integer; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.BLOB_text_load(blob_id, database, transaction, file_name);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_BLOB_text_load;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_Bopen(blob_id: PISCQuad; database: IscDbHandle; transaction:
  IscTrHandle;
  mode: PChar): PBStream; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.Bopen(blob_id, database, transaction, mode);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_Bopen;
  header.threadid := GetCurrentThreadId;
  header.status := 0;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_add_user(status: PISCStatus; user_data: PUserSecData): Integer; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_add_user(status, user_data);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_add_user;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_array_gen_sdl(status: PISCStatus; desc: PISCArrayDesc;
  sdl_buffer_length: PSmallInt;
  sdl_buffer: PChar; sdl_length: PSmallInt): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_array_gen_sdl(status, desc, sdl_buffer_length, sdl_buffer,
    sdl_length);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_array_gen_sdl;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_array_get_slice(status: PISCStatus; db_handle: PIscDbHandle;
  trans_handle: PIscTrHandle; array_id: PISCQuad; desc: PISCArrayDesc; array_:
  PPointer;
  slice_length: PISCLong): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_array_get_slice(status, db_handle, trans_handle, array_id,
    desc, array_, slice_length);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_array_get_slice;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_array_lookup_bounds(status: PISCStatus; db_handle: PIscDbHandle;
  trans_handle: PIscTrHandle; relation_name, field_name: PChar;
  desc: PISCArrayDesc): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_array_lookup_bounds(status, db_handle, trans_handle,
    relation_name, field_name, desc);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_array_lookup_bounds;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_array_lookup_desc(status: PISCStatus; db_handle: PIscDbHandle;
  trans_handle: PIscTrHandle; relation_name, field_name: PChar;
  desc: PISCArrayDesc): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_array_lookup_desc(status, db_handle, trans_handle,
    relation_name, field_name, desc);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_array_lookup_desc;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_array_put_slice(status: PISCStatus; db_handle: PIscDbHandle;
  trans_handle: PIscTrHandle; array_id: PISCQuad; desc: PISCArrayDesc; array_:
  PPointer;
  slice_length: PISCLong): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_array_put_slice(status, db_handle, trans_handle, array_id,
    desc, array_, slice_length);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_array_put_slice;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_array_set_desc(status: PISCStatus; relation_name, field_name: PChar;
  sql_dtype, sql_length, dimensions: PSmallint; desc: PISCArrayDesc): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_array_set_desc(status, relation_name, field_name,
    sql_dtype, sql_length, dimensions, desc);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_array_set_desc;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_attach_database(user_status: PISCStatus; file_length: Smallint;
  file_name: PChar; handle: PIscDbHandle; dpb_length: Smallint; dpb: PChar):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_attach_database(user_status, file_length,
    file_name, handle, dpb_length, dpb);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_attach_database;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

procedure hook_isc_blob_default_desc(desc: PISCBlobDesc; relation_name, field_name:
  PChar); {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  HookedLib.isc_blob_default_desc(desc, relation_name, field_name);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_blob_default_desc;
  header.threadid := GetCurrentThreadId;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_blob_gen_bpb(status: PISCStatus; to_desc, from_desc: PISCBlobDesc;
  bpb_buffer_length: Word; bpb_buffer: PChar; bpb_length: PWord): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_blob_gen_bpb(status, to_desc, from_desc,
    bpb_buffer_length, bpb_buffer, bpb_length);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_blob_gen_bpb;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_blob_info(user_status: PISCStatus; blob_handle: PIscBlobHandle;
  item_length: Smallint; items: PChar; buffer_length: Smallint; buffer: PChar):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_blob_info(user_status, blob_handle,
    item_length, items, buffer_length, buffer);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_blob_info;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_blob_lookup_desc(status: PISCStatus; db_handle: PIscDbHandle;
  trans_handle: PIscTrHandle; relation_name, field_name: PChar; desc:
  PISCBlobDesc;
  global: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_blob_lookup_desc(status, db_handle,
    trans_handle, relation_name, field_name, desc,
    global);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_blob_lookup_desc;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_blob_set_desc(status: PISCStatus; relation_name, field_name: PChar;
  subtype, charset, segment_size: Smallint; desc: PISCBlobDesc): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_blob_set_desc(status, relation_name, field_name,
    subtype, charset, segment_size, desc);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_blob_set_desc;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_cancel_blob(user_status: PISCStatus; blob_handle: PIscBlobHandle):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_cancel_blob(user_status, blob_handle);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_cancel_blob;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_cancel_events(user_status: PISCStatus; handle: PIscDbHandle;
  id: PISCLong): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_cancel_events(user_status, handle,
    id);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_cancel_events;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_close(user_status: PISCStatus; name: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_close(user_status, name);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_close;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_close_blob(user_status: PISCStatus; blob_handle: PIscBlobHandle):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_close_blob(user_status, blob_handle);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_close_blob;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_commit_retaining(user_status: PISCStatus; tra_handle:
  PIscTrHandle): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_commit_retaining(user_status, tra_handle);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_commit_retaining;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_commit_transaction(user_status: PISCStatus;
  tra_handle: PIscTrHandle): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_commit_transaction(user_status, tra_handle);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_commit_transaction;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_compile_request(user_status: PISCStatus; db_handle: PIscDbHandle;
  req_handle: PIscReqHandle; blr_length: Smallint;
  blr: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_compile_request(user_status, db_handle,
    req_handle, blr_length, blr);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_compile_request;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_compile_request2(user_status: PISCStatus; db_handle: PIscDbHandle;
  req_handle: PIscReqHandle; blr_length: Smallint;
  blr: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_compile_request2(user_status, db_handle,
    req_handle, blr_length, blr);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_compile_request2;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_create_blob(user_status: PISCStatus; db_handle: PIscDbHandle;
  tra_handle: PIscTrHandle; blob_handle: PIscBlobHandle;
  blob_id: PISCQuad): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_create_blob(user_status, db_handle,
    tra_handle, blob_handle, blob_id);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_create_blob;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_create_blob2(user_status: PISCStatus; db_handle: PIscDbHandle;
  tra_handle: PIscTrHandle; blob_handle: PIscBlobHandle; blob_id: PISCQuad;
  bpb_length: Smallint; bpb: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_create_blob2(user_status, db_handle,
    tra_handle, blob_handle, blob_id,
    bpb_length, bpb);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_create_blob2;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_create_database(user_status: PISCStatus; file_length: Smallint;
  file_name: PChar; handle: PIscDbHandle; dpb_length: Smallint; dpb: PChar;
  db_type: Smallint): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_create_database(user_status, file_length,
    file_name, handle, dpb_length, dpb,
    db_type);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_create_database;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_database_info(user_status: PISCStatus; handle: PIscDbHandle;
  item_length: Smallint; items: PChar; buffer_length: Smallint;
  buffer: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_database_info(user_status, handle,
    item_length, items, buffer_length,
    buffer);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_database_info;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_ddl(user_status: PISCStatus; db_handle: PIscDbHandle; tra_handle:
  PIscTrHandle;
  Length: Smallint; ddl: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_ddl(user_status, db_handle, tra_handle,
    Length, ddl);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_ddl;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_declare(user_status: PISCStatus; statement, cursor: PChar):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_declare(user_status, statement, cursor);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_declare;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_delete_user(status: PISCStatus; user_data: PUserSecData): Integer; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_delete_user(status, user_data);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_delete_user;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_describe(user_status: PISCStatus; name: PChar; sqlda: PXSQLDA):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_describe(user_status, name, sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_describe;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_describe_bind(user_status: PISCStatus; name: PChar; sqlda:
  PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_describe_bind(user_status, name, sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_describe_bind;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_detach_database(user_status: PISCStatus; handle: PIscDbHandle):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_detach_database(user_status, handle);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_detach_database;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_drop_database(user_status: PISCStatus; handle: PIscDbHandle):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_drop_database(user_status, handle);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_drop_database;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_dsql_alloc_statement2(user_status: PISCStatus; db_handle: PIscDbHandle;
  stmt_handle: PIscStmtHandle): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_dsql_alloc_statement2(user_status, db_handle, stmt_handle);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_alloc_statement2;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}
end;

function hook_isc_dsql_allocate_statement(user_status: PISCStatus; db_handle: PIscDbHandle;
  stmt_handle: PIscStmtHandle): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
begin
  QueryPerformanceCounter(header.cstart);
  Result := HookedLib.isc_dsql_allocate_statement(user_status, db_handle, stmt_handle);
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_allocate_statement;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    if not IsStatusError(Result) then
      strm.WriteHandle(stmt_handle);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
end;

function hook_isc_dsql_describe(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
  dialect: Word; sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_dsql_describe(user_status, stmt_handle,
    dialect, sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_describe;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_dsql_describe_bind(user_status: PISCStatus; stmt_handle:
  PIscStmtHandle;
  dialect: Word; sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_dsql_describe_bind(user_status, stmt_handle,
    dialect, sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_describe_bind;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}
end;

function hook_isc_dsql_exec_immed2(user_status: PISCStatus; db_handle: PIscDbHandle;
  tra_handle: PIscTrHandle; Length: Word; string_: PChar; dialect: Word;
  in_sqlda, out_sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
begin
  QueryPerformanceCounter(header.cstart);
  Result := HookedLib.isc_dsql_exec_immed2(user_status, db_handle,
    tra_handle, Length, string_, dialect, in_sqlda, out_sqlda);
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_exec_immed2;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    strm.WriteString(string_);
    if in_sqlda = nil then
      strm.WriteInteger(0) else
      case in_sqlda.version of
        1: strm.WriteString(ParamsToString_V1(PXSQLDA_V1(in_sqlda)));
        2: strm.WriteString(ParamsToString_V2(PXSQLDA_V2(in_sqlda)));
      end;
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
end;

function hook_isc_dsql_execute(user_status: PISCStatus; tra_handle: PIscTrHandle;
  stmt_handle: PIscStmtHandle; dialect: Word; sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_dsql_execute(user_status, tra_handle,
    stmt_handle, dialect, sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_execute;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_dsql_execute_immediate(user_status: PISCStatus; db_handle:
  PIscDbHandle; tra_handle: PIscTrHandle; Length: Word; string_: PChar; dialect: Word;
  sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_dsql_execute_immediate(user_status, db_handle,
    tra_handle, Length, string_, dialect, sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_execute_immediate;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_dsql_execute2(user_status: PISCStatus; tra_handle: PIscTrHandle;
  stmt_handle: PIscStmtHandle; dialect: Word; in_sqlda, out_sqlda: PXSQLDA):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
begin
  QueryPerformanceCounter(header.cstart);
  Result := HookedLib.isc_dsql_execute2(user_status, tra_handle,
    stmt_handle, dialect, in_sqlda, out_sqlda);
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_execute2;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    strm.WriteHandle(stmt_handle);
    strm.WriteInteger(HookedLib.DSQLInfoRowsAffected(stmt_handle^, HookedLib.DSQLInfoStatementType(stmt_handle^)));
    if in_sqlda = nil then
      strm.WriteInteger(0) else
      case in_sqlda.version of
        1: strm.WriteString(ParamsToString_V1(PXSQLDA_V1(in_sqlda)));
        2: strm.WriteString(ParamsToString_V2(PXSQLDA_V2(in_sqlda)));
      end;
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
end;

function hook_isc_dsql_fetch(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
  dialect: Word; sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
begin
  QueryPerformanceCounter(header.cstart);
  Result := HookedLib.isc_dsql_fetch(user_status, stmt_handle, dialect, sqlda);
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_fetch;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    strm.WriteHandle(stmt_handle);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
end;

function hook_isc_dsql_finish(db_handle: PIscDbHandle): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_dsql_finish(db_handle);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_finish;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_dsql_free_statement(user_status: PISCStatus; stmt_handle:
  PIscStmtHandle;
  option: Word): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
  h: Integer;
begin
  if stmt_handle <> nil then
    h := PInteger(stmt_handle)^ else
    h := -1;
  QueryPerformanceCounter(header.cstart);
  Result := HookedLib.isc_dsql_free_statement(user_status, stmt_handle, option);
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_free_statement;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    strm.WriteInteger(option);
    strm.WriteInteger(h);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
end;

function hook_isc_dsql_insert(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
  dialect: Word; sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_dsql_insert(user_status, stmt_handle,
    dialect, sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_insert;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_dsql_prepare(user_status: PISCStatus; tra_handle: PIscTrHandle;
  stmt_handle: PIscStmtHandle; Length: Word; string_: PChar; dialect: Word;
  sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
begin
  QueryPerformanceCounter(header.cstart);
  Result := HookedLib.isc_dsql_prepare(user_status, tra_handle, stmt_handle, Length, string_, dialect, sqlda);
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_prepare;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    if Length <> 0 then
      strm.WriteItem(string_, Length) else
      strm.WriteString(string_);
    if not IsStatusError(Result) then
    begin
      strm.WriteHandle(stmt_handle);
      strm.WriteString(HookedLib.DSQLInfoPlan(stmt_handle^));
    end;
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
end;

function hook_isc_dsql_release(user_status: PISCStatus; name: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_dsql_release(user_status, name);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_release;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_dsql_set_cursor_name(user_status: PISCStatus; stmt_handle:
  PIscStmtHandle;
  cursor: PChar; type_: Word): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_dsql_set_cursor_name(user_status, stmt_handle,
    cursor, type_);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_set_cursor_name;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_dsql_sql_info(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
  item_length: Smallint; items: PChar; buffer_length: Smallint; buffer: PChar):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_dsql_sql_info(user_status, stmt_handle,
    item_length, items, buffer_length, buffer);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_sql_info;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_embed_dsql_close(user_status: PISCStatus; name: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_embed_dsql_close(user_status, name);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_embed_dsql_close;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_embed_dsql_declare(user_status: PISCStatus; stmt_name, cursor:
  PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_embed_dsql_declare(user_status, stmt_name, cursor);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_embed_dsql_declare;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_embed_dsql_describe(user_status: PISCStatus; stmt_name: PChar;
  dialect: Word; sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_embed_dsql_describe(user_status, stmt_name,
    dialect, sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_embed_dsql_describe;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_embed_dsql_describe_bind(user_status: PISCStatus; stmt_name: PChar;
  dialect: Word; sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_embed_dsql_describe_bind(user_status, stmt_name,
    dialect, sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_embed_dsql_describe_bind;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_embed_dsql_execute(user_status: PISCStatus; trans_handle:
  PIscTrHandle;
  stmt_name: PChar; dialect: Word; sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_embed_dsql_execute(user_status, trans_handle,
    stmt_name, dialect, sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_embed_dsql_execute;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_embed_dsql_execute_immed(user_status: PISCStatus; db_handle:
  PIscDbHandle;
  trans_handle: PIscTrHandle; Length: Word; string_: PChar; dialect: Word;
  sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_embed_dsql_execute_immed(user_status, db_handle,
    trans_handle, Length, string_, dialect,
    sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_embed_dsql_execute_immed;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_embed_dsql_execute2(user_status: PISCStatus; trans_handle:
  PIscTrHandle;
  stmt_name: PChar; dialect: Word; in_sqlda, out_sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_embed_dsql_execute2(user_status, trans_handle,
    stmt_name, dialect, in_sqlda, out_sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_embed_dsql_execute2;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_embed_dsql_fetch(user_status: PISCStatus; cursor_name: PChar;
  dialect: Word; sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_embed_dsql_fetch(user_status, cursor_name,
    dialect, sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_embed_dsql_fetch;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_embed_dsql_insert(user_status: PISCStatus; cursor_name: PChar;
  dialect: Word; sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_embed_dsql_insert(user_status, cursor_name,
    dialect, sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_embed_dsql_insert;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_embed_dsql_open(user_status: PISCStatus; trans_handle:
  PIscTrHandle;
  cursor_name: PChar; dialect: Word; sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_embed_dsql_open(user_status, trans_handle,
    cursor_name, dialect, sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_embed_dsql_open;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_embed_dsql_open2(user_status: PISCStatus; trans_handle:
  PIscTrHandle;
  cursor_name: PChar; dialect: Word; in_sqlda, out_sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_embed_dsql_open2(user_status, trans_handle,
    cursor_name, dialect, in_sqlda, out_sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_embed_dsql_open2;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}
end;

function hook_isc_embed_dsql_prepare(user_status: PISCStatus; db_handle:
  PIscDbHandle;
  trans_handle: PIscTrHandle; stmt_name: PChar; Length: Word; string_: PChar;
  dialect: Word; sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_embed_dsql_prepare(user_status, db_handle,
    trans_handle, stmt_name, Length, string_,
    dialect, sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_embed_dsql_prepare;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_embed_dsql_release(user_status: PISCStatus; stmt_name: PChar):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_embed_dsql_release(user_status, stmt_name);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_embed_dsql_release;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_event_block(event_buffer, result_buffer: PPChar; count: Smallint;
  v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15: PChar):
  ISCLong; cdecl;
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_event_block(event_buffer, result_buffer, count,
    v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_event_block;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}
end;

procedure hook_isc_event_counts(ser_status: PISCStatus; buffer_length: Smallint;
  event_buffer, result_buffer: PChar); {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  HookedLib.isc_event_counts(ser_status, buffer_length,
    event_buffer, result_buffer);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_event_counts;
  header.threadid := GetCurrentThreadId;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_execute(user_status: PISCStatus; trans_handle: PIscTrHandle;
  name: PChar; sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_execute(user_status, trans_handle,
    name, sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_execute;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_execute_immediate(user_status: PISCStatus; db_handle: PIscDbHandle;
  trans_handle: PIscTrHandle; Length: PSmallint; string_: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_execute_immediate(user_status, db_handle,
    trans_handle, Length, string_);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_execute_immediate;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

procedure hook_isc_expand_dpb(dpb: PPChar; dpb_size: PSmallint; name_buffer: PPChar);
  cdecl;
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  HookedLib.isc_expand_dpb(dpb, dpb_size, name_buffer);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_expand_dpb;
  header.threadid := GetCurrentThreadId;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}
end;

function hook_isc_fetch(user_status: PISCStatus; name: PChar; sqlda: PXSQLDA):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_fetch(user_status, name, sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_fetch;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_free(blk: PChar): ISCLong; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_free(blk);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_free;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_ftof(string_: PChar; length1: Word; field: PChar; length2: Word):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_ftof(string_, length1, field, length2);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_ftof;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_get_segment(user_status: PISCStatus; blob_handle: PIscBlobHandle;
  Length: PWord;
  buffer_length: Word; buffer: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_get_segment(user_status, blob_handle, Length,
    buffer_length, buffer);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_get_segment;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}
end;

function hook_isc_get_slice(user_status: PISCStatus; db_handle: PIscDbHandle;
  tra_handle: PIscTrHandle;
  array_id: PISCQuad; sdl_length: Smallint; sdl: PChar; param_length: Smallint;
  param: PISCLong;
  slice_length: ISCLong; slice: PPointer; return_length: PISCLong): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_get_slice(user_status, db_handle, tra_handle,
    array_id, sdl_length, sdl, param_length, param,
    slice_length, slice, return_length);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_get_slice;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_interprete(buffer: PChar; status_vector: PPISCStatus): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_interprete(buffer, status_vector);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_interprete;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

{$IFDEF FB20_UP}

function hook_fb_interpret(buffer: PChar; v: Integer; status_vector: PPISCStatus):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.fb_interpret(buffer, v, status_vector);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_fb_interpret;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;
{$ENDIF}

function hook_isc_modify_dpb(dpb: PPChar; dpb_length: PSmallint; type_: Word;
  str: PChar; str_len: Smallint): Integer; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_modify_dpb(dpb, dpb_length, type_,
    str, str_len);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_modify_dpb;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_modify_user(status: PISCStatus; user_data: PUserSecData): Integer; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_modify_user(status, user_data);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_modify_user;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_open(user_status: PISCStatus; trans_handle: PIscTrHandle; name:
  PChar;
  sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_open(user_status, trans_handle, name,
    sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_open;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}
end;

function hook_isc_open_blob(user_status: PISCStatus; db_handle: PIscDbHandle;
  tra_handle: PIscTrHandle; blob_handle: PIscBlobHandle; blob_id: PISCQuad):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_open_blob(user_status, db_handle,
    tra_handle, blob_handle, blob_id);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_open_blob;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_open_blob2(user_status: PISCStatus; db_handle: PIscDbHandle;
  tra_handle: PIscTrHandle; blob_handle: PIscBlobHandle; blob_id: PISCQuad;
  bpb_length: Word;
  bpb: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_open_blob2(user_status, db_handle,
    tra_handle, blob_handle, blob_id, bpb_length,
    bpb);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_open_blob2;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_portable_integer(ptr: PChar; Length: Smallint): ISCInt64; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_portable_integer(ptr, Length);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_portable_integer;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}
end;

function hook_isc_prepare(user_status: PISCStatus; db_handle: PIscDbHandle;
  trans_handle: PIscTrHandle; name: PChar; Length: PSmallint; string_: PChar;
  sqlda: PXSQLDA): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_prepare(user_status, db_handle,
    trans_handle, name, Length, string_,
    sqlda);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_prepare;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_prepare_transaction(user_status: PISCStatus; tra_handle:
  PIscTrHandle): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_prepare_transaction(user_status, tra_handle);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_prepare_transaction;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_prepare_transaction2(user_status: PISCStatus; tra_handle:
  PIscTrHandle;
  msg_length: ISCUShort; msg: PISCUChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_prepare_transaction2(user_status, tra_handle,
    msg_length, msg);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_prepare_transaction2;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_print_blr(blr: PChar; callback: IscCallback; callback_argument:
  PPointer;
  language: Smallint): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_print_blr(blr, callback, callback_argument,
    language);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_print_blr;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

procedure hook_isc_print_sqlerror(sqlcode: ISCShort; status_vector: PISCStatus); {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  HookedLib.isc_print_sqlerror(sqlcode, status_vector);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_print_sqlerror;
  header.threadid := GetCurrentThreadId;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}
end;

function hook_isc_print_status(status_vector: PISCStatus): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_print_status(status_vector);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_print_status;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_put_segment(user_status: PISCStatus; blob_handle: PIscBlobHandle;
  buffer_length: Word; buffer: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_put_segment(user_status, blob_handle,
    buffer_length, buffer);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_put_segment;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_put_slice(user_status: PISCStatus; db_handle: PIscDbHandle;
  tra_handle: PIscTrHandle;
  array_id: PISCQuad; sdl_length: Smallint; sdl: PChar; param_length: Smallint;
  param: PISCLong;
  slice_length: ISCLong; slice: PPointer): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_put_slice(user_status, db_handle, tra_handle,
    array_id, sdl_length, sdl, param_length, param,
    slice_length, slice);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_put_slice;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

procedure hook_isc_qtoq(quad1, quad2: PISCQuad); {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  HookedLib.isc_qtoq(quad1, quad2);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_qtoq;
  header.threadid := GetCurrentThreadId;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_que_events(user_status: PISCStatus; handle: PIscDbHandle; id:
  PISCLong;
  Length: ISCUShort; events: PChar; ast: IscCallback; arg: PPointer): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_que_events(user_status, handle, id,
    Length, events, ast, arg);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_que_events;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_receive(user_status: PISCStatus; req_handle: PIscReqHandle;
  msg_type,
  msg_length: Smallint; msg: PPointer; level: Smallint): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_receive(user_status, req_handle, msg_type,
    msg_length, msg, level);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_receive;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_reconnect_transaction(user_status: PISCStatus; db_handle:
  PIscDbHandle;
  tra_handle: PIscTrHandle; Length: Smallint; id: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_reconnect_transaction(user_status, db_handle,
    tra_handle, Length, id);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_reconnect_transaction;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_release_request(user_status: PISCStatus; req_handle:
  PIscReqHandle): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_release_request(user_status, req_handle);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_release_request;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_request_info(user_status: PISCStatus; req_handle: PIscReqHandle;
  level,
  item_length: Smallint; items: PChar; buffer_length: Smallint; buffer: PChar):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_request_info(user_status, req_handle, level,
    item_length, items, buffer_length, buffer);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_request_info;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_rollback_retaining(status_vector: PISCStatus; trans_handle:
  PIscTrHandle): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_rollback_retaining(status_vector, trans_handle);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_rollback_retaining;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_rollback_transaction(user_status: PISCStatus; tra_handle:
  PIscTrHandle): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_rollback_transaction(user_status, tra_handle);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_rollback_transaction;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_seek_blob(user_status: PISCStatus; blob_handle: PIscBlobHandle;
  mode: Smallint;
  offset: ISCLong; Result_: PISCLong): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_seek_blob(user_status, blob_handle, mode,
    offset, Result_);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_seek_blob;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_send(user_status: PISCStatus; req_handle: PIscReqHandle; msg_type,
  msg_length: Smallint; msg: PPointer; level: Smallint): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_send(user_status, req_handle, msg_type,
    msg_length, msg, level);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_send;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_service_attach(status_vector: PISCStatus; service_length: Word;
  service_name: PChar; handle: PIscSvcHandle; spb_length: Word; spb: PChar):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_service_attach(status_vector, service_length,
    service_name, handle, spb_length, spb);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_service_attach;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_service_detach(status_vector: PISCStatus; handle: PIscSvcHandle):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_service_detach(status_vector, handle);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_service_detach;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_service_query(status_vector: PISCStatus; svc_handle: PIscSvcHandle;
  reserved: PIscResvHandle; send_spb_length: Word; send_spb: PChar;
  request_spb_length: Word;
  request_spb: PChar; buffer_length: Word; buffer: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_service_query(status_vector, svc_handle,
    reserved, send_spb_length, send_spb, request_spb_length,
    request_spb, buffer_length, buffer);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_service_query;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_service_start(status_vector: PISCStatus; svc_handle: PIscSvcHandle;
  reserved: PIscResvHandle; spb_length: Word; spb: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_service_start(status_vector, svc_handle,
    reserved, spb_length, spb);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_service_start;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}
end;

{$IFDEF INTERBASEORFIREBIRD}

procedure hook_isc_set_debug(flag: Integer); {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  HookedLib.isc_set_debug(flag);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_set_debug;
  header.threadid := GetCurrentThreadId;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;
{$ENDIF INTERBASEORFIREBIRD}

procedure hook_isc_sql_interprete(SQLCODE: Smallint; buffer: PChar; buffer_length:
  Smallint); {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  HookedLib.isc_sql_interprete(SQLCODE, buffer, buffer_length);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_sql_interprete;
  header.threadid := GetCurrentThreadId;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_sqlcode(user_status: PISCStatus): ISCLong; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_sqlcode(user_status);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_sqlcode;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_start_and_send(user_status: PISCStatus; req_handle: PIscReqHandle;
  tra_handle: PIscTrHandle; msg_type, msg_length: Smallint; msg: PPointer;
  level: Smallint): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_start_and_send(user_status, req_handle,
    tra_handle, msg_type, msg_length, msg,
    level);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_start_and_send;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_start_multiple(user_status: PISCStatus; tra_handle: PIscTrHandle;
  count: Smallint; vector: PISCTEB): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_start_multiple(user_status, tra_handle,
    count, vector);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_start_multiple;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    strm.WriteHandle(vector.Handle); // db handle
    if not IsStatusError(Result) then
      strm.WriteHandle(tra_handle); // tr handle
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_start_request(user_status: PISCStatus; req_handle: PIscReqHandle;
  tra_handle: PIscTrHandle; level: Smallint): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_start_request(user_status, req_handle,
    tra_handle, level);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_start_request;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_start_transaction(user_status: PISCStatus; tra_handle: PIscTrHandle; count: Smallint;
  db_handle: PIscDbHandle; tpb_length: ISCUShort; tpb_ad: PChar): ISCStatus;
  cdecl;
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_start_transaction(user_status, tra_handle, count, db_handle, tpb_length, tpb_ad);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_start_transaction;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_transact_request(user_status: PISCStatus; db_handle: PIscDbHandle;
  tra_handle: PIscTrHandle; blr_length: Word; blr: PChar; in_msg_length: Word;
  in_msg: PChar;
  out_msg_length: Word; out_msg: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_transact_request(user_status, db_handle,
    tra_handle, blr_length, blr, in_msg_length, in_msg,
    out_msg_length, out_msg);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_transact_request;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_transaction_info(user_status: PISCStatus; tra_handle: PIscTrHandle;
  item_length: Smallint;
  items: PChar; buffer_length: Smallint; buffer: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_transaction_info(user_status, tra_handle, item_length,
    items, buffer_length, buffer);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_transaction_info;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_unwind_request(user_status: PISCStatus; req_handle: PIscTrHandle;
  level: Smallint): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_unwind_request(user_status, req_handle,
    level);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_unwind_request;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_version(db_handle: PIscDbHandle; callback: IscCallback;
  callback_argument: PPointer): Integer; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_version(db_handle, callback,
    callback_argument);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_version;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

procedure hook_isc_vtof(string1, string2: PChar; Length: Word); {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  HookedLib.isc_vtof(string1, string2, Length);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_vtof;
  header.threadid := GetCurrentThreadId;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

procedure hook_isc_vtov(string1, string2: PChar; Length: Smallint); {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  HookedLib.isc_vtov(string1, string2, Length);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_vtov;
  header.threadid := GetCurrentThreadId;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_wait_for_event(user_status: PISCStatus; handle: PIscDbHandle;
  Length: Smallint; events, buffer: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_wait_for_event(user_status, handle,
    Length, events, buffer);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_wait_for_event;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

{$IFDEF FB15_UP}

function hook_isc_reset_fpe(fpe_status: Word): ISCLong; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_reset_fpe(fpe_status);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_reset_fpe;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;
{$ENDIF FB15_UP}

{$IFDEF IB7_UP}

function hook_isc_array_gen_sdl2(status: PISCStatus; desc: PISCArrayDescV2;
  sdl_buffer_length: PSmallInt; sdl_buffer: PChar; sdl_length: PSmallInt):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_array_gen_sdl2(status, desc,
    sdl_buffer_length, sdl_buffer, sdl_length);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_array_gen_sdl2;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_array_get_slice2(status: PISCStatus; db_handle: PIscDbHandle;
  trans_handle: PIscTrHandle; array_id: PISCQuad; desc: PISCArrayDescV2; array_:
  PPointer;
  slice_length: PISCLong): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_array_get_slice2(status, db_handle,
    trans_handle, array_id, desc, array_,
    slice_length);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_array_get_slice2;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_array_lookup_bounds2(status: PISCStatus; db_handle: PIscDbHandle;
  trans_handle: PIscTrHandle; relation_name, field_name: PChar; desc:
  PISCArrayDescV2): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_array_lookup_bounds2(status, db_handle,
    trans_handle, relation_name, field_name, desc);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_array_lookup_bounds2;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_array_lookup_desc2(status: PISCStatus; db_handle: PIscDbHandle;
  trans_handle: PIscTrHandle; relation_name, field_name: PChar;
  desc: PISCArrayDescV2): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_array_lookup_desc2(status, db_handle,
    trans_handle, relation_name, field_name,
    desc);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_array_lookup_desc2;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_array_put_slice2(status: PISCStatus; db_handle: PIscDbHandle;
  trans_handle: PIscTrHandle; array_id: PISCQuad; desc: PISCArrayDescV2; array_:
  PPointer;
  slice_length: PISCLong): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_array_put_slice2(status, db_handle,
    trans_handle, array_id, desc, array_,
    slice_length);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_array_put_slice2;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_array_set_desc2(status: PISCStatus; relation_name, field_name:
  PChar;
  sql_dtype, sql_length, dimensions: PSmallint; desc: PISCArrayDescV2):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_array_set_desc2(status, relation_name, field_name,
    sql_dtype, sql_length, dimensions, desc);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_array_set_desc2;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

procedure hook_isc_blob_default_desc2(desc: PISCBlobDescV2; relation_name,
  field_name: PChar); {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  HookedLib.isc_blob_default_desc2(desc, relation_name,
    field_name);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_blob_default_desc2;
  header.threadid := GetCurrentThreadId;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_blob_gen_bpb2(status: PISCStatus; to_desc, from_desc:
  PISCBlobDescV2;
  bpb_buffer_length: Word; bpb_buffer: PChar; bpb_length: PWord): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_blob_gen_bpb2(status, to_desc, from_desc,
    bpb_buffer_length, bpb_buffer, bpb_length);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_blob_gen_bpb2;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_blob_lookup_desc2(status: PISCStatus; db_handle: PIscDbHandle;
  trans_handle: PIscTrHandle; relation_name, field_name: PChar; desc:
  PISCBlobDescV2;
  global: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_blob_lookup_desc2(status, db_handle, trans_handle, relation_name, field_name, desc, global);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_blob_lookup_desc2;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_blob_set_desc2(status: PISCStatus; relation_name, field_name:
  PChar; subtype, charset,
  segment_size: Smallint; desc: PISCBlobDescV2): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_blob_set_desc2(status, relation_name, field_name, subtype, charset, segment_size, desc);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_blob_set_desc2;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;
{$ENDIF IB7_UP}

{$IFDEF IB7ORFB15}

procedure hook_isc_get_client_version(version: PChar); {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  HookedLib.isc_get_client_version(version);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_get_client_version;
  header.threadid := GetCurrentThreadId;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_get_client_major_version: Integer; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_get_client_major_version;
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_get_client_major_version:;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_get_client_minor_version: Integer; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_get_client_minor_version;
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_get_client_minor_version:;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;
{$ENDIF IB7ORFB15}

{$IFDEF IB71_UP}

function hook_isc_release_savepoint(status: PISCStatus; TrHandle: PIscTrHandle;
  name: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_release_savepoint(status, TrHandle, name);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_release_savepoint;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_rollback_savepoint(status: PISCStatus; TrHandle: PIscTrHandle;
  name: PChar; Option: Word): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_rollback_savepoint(status, TrHandle, name, Option);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_rollback_savepoint;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_start_savepoint(status: PISCStatus; TrHandle: PIscTrHandle;
  name: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_start_savepoint(status, TrHandle, name);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_start_savepoint;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;
{$ENDIF IB71_UP}

function hook_isc_dsql_exec_immed3_m(user_status: PISCStatus; db_handle:
  PIscDbHandle;
  tra_handle: PIscTrHandle; Length: Word; string_: PChar; dialect,
  in_blr_length: Word;
  in_blr: PChar; in_msg_type, in_msg_length: Word; in_msg: PChar;
  out_blr_length: Word;
  out_blr: PChar; out_msg_type, out_msg_length: Word; out_msg: PChar):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_dsql_exec_immed3_m(user_status, db_handle,
    tra_handle, Length, string_, dialect, in_blr_length,
    in_blr, in_msg_type, in_msg_length, in_msg, out_blr_length,
    out_blr, out_msg_type, out_msg_length, out_msg);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_exec_immed3_m;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_dsql_execute_immediate_m(user_status: PISCStatus; db_handle: PIscDbHandle;
  tra_handle: PIscTrHandle; Length: Word; string_: PChar; dialect, blr_length: Word;
  blr: PChar; msg_type, msg_length: Word; msg: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_dsql_execute_immediate_m(user_status, db_handle, tra_handle,
    Length, string_, dialect, blr_length, blr, msg_type, msg_length, msg);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_execute_immediate_m;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_dsql_execute_m(user_status: PISCStatus; tra_handle: PIscTrHandle;
  stmt_handle: PIscStmtHandle; blr_length: Word; blr: PChar; msg_type,
  msg_length: Word;
  msg: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_dsql_execute_m(user_status, tra_handle,
    stmt_handle, blr_length, blr, msg_type, msg_length, msg);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_execute_m;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_dsql_execute2_m(user_status: PISCStatus; tra_handle: PIscTrHandle;
  stmt_handle: PIscStmtHandle; in_blr_length: Word; in_blr: PChar; in_msg_type,
  in_msg_length: Word; in_msg: PChar; out_blr_length: Word; out_blr: PChar;
  out_msg_type, out_msg_length: Word; out_msg: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_dsql_execute2_m(user_status, tra_handle,
    stmt_handle, in_blr_length, in_blr, in_msg_type,
    in_msg_length, in_msg, out_blr_length, out_blr,
    out_msg_type, out_msg_length, out_msg);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_execute2_m;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_dsql_fetch_m(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
  blr_length: Word; blr: PChar; msg_type, msg_length: Word; msg: PChar):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_dsql_fetch_m(user_status, stmt_handle,
    blr_length, blr, msg_type, msg_length, msg);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_fetch_m;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_dsql_insert_m(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
  blr_length: Word; blr: PChar; msg_type, msg_length: Word; msg: PChar):
  ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_dsql_insert_m(user_status, stmt_handle,
    blr_length, blr, msg_type, msg_length, msg);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_insert_m;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

function hook_isc_dsql_prepare_m(user_status: PISCStatus; tra_handle: PIscTrHandle;
  stmt_handle: PIscStmtHandle; Length: Word; string_: PChar; dialect,
  item_length: Word; items: PChar; buffer_length: Word; buffer: PChar): ISCStatus; {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
  {$IFDEF INTERCEPTALL}
  var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  Result := HookedLib.isc_dsql_prepare_m(user_status, tra_handle,
    stmt_handle, Length, string_, dialect, item_length,
    items, buffer_length, buffer);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_dsql_prepare_m;
  header.threadid := GetCurrentThreadId;
  header.status := Result;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

procedure hook_isc_encode_date(times: PPointer; date: PISCQuad); {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}  
  HookedLib.isc_encode_date(times, date);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_encode_date;
  header.threadid := GetCurrentThreadId;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}  
end;

procedure hook_isc_encode_sql_date(times_arg: PPointer; date: PISCDate); {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}
  HookedLib.isc_encode_sql_date(times_arg, date);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_encode_sql_date;
  header.threadid := GetCurrentThreadId;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}
end;

procedure hook_isc_encode_sql_time(times_arg: PPointer; isc_time: PISCTime); {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}
  HookedLib.isc_encode_sql_time(times_arg, isc_time);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_encode_sql_time;
  header.threadid := GetCurrentThreadId;
  strm := TStreamMethod.Create;
  try
    strm.WriteHeader(header);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}
end;

procedure hook_isc_encode_timestamp(times_arg: PPointer; date: PISCTimeStamp); {$IFDEF UNIX} cdecl;{$ELSE} stdcall;{$ENDIF}
{$IFDEF INTERCEPTALL}
var
  strm: TStreamMethod;
  header: THookedMethodHeader;
{$ENDIF}
begin
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstart);
{$ENDIF}
  HookedLib.isc_encode_timestamp(times_arg, date);
{$IFDEF INTERCEPTALL}
  QueryPerformanceCounter(header.cstop);
  header.index := InterlockedIncrement(IndexGenerator);
  header.timestamp := Now;
  header.methodid := hm_isc_encode_timestamp;
  header.index := InterlockedIncrement(IndexGenerator);
  header.threadid := GetCurrentThreadId;
  strm := TStreamMethod.Create;
  try
    header.index := InterlockedIncrement(IndexGenerator);
    strm.WriteHeader(header);
    header.index := InterlockedIncrement(IndexGenerator);
    SendIpcMessage(PChar(UIBHOOKIPC), strm.Memory, strm.Size);
  finally
    strm.Free;
  end;
{$ENDIF}
end;


{ TUIBHookKedLibrary }

function TUIBHookKedLibrary.Load(const lib: string): Boolean;
const
  HOOKFLAG = 0;
begin
  CollectHooks;
  HookApi(PChar(lib), 'BLOB_close', @hook_BLOB_close, @BLOB_close, HOOKFLAG);
{$IFDEF INTERBASEORFIREBIRD}
  HookApi(PChar(lib), 'BLOB_display', @hook_BLOB_display, @BLOB_display, HOOKFLAG);
{$ENDIF INTERBASEORFIREBIRD}
  HookApi(PChar(lib), 'BLOB_dump', @hook_BLOB_dump, @BLOB_dump, HOOKFLAG);
  HookApi(PChar(lib), 'BLOB_edit', @hook_BLOB_edit, @BLOB_edit, HOOKFLAG);
  HookApi(PChar(lib), 'BLOB_get', @hook_BLOB_get, @BLOB_get, HOOKFLAG);
  HookApi(PChar(lib), 'BLOB_load', @hook_BLOB_load, @BLOB_load, HOOKFLAG);
  HookApi(PChar(lib), 'BLOB_open', @hook_BLOB_open, @BLOB_open, HOOKFLAG);
  HookApi(PChar(lib), 'BLOB_put', @hook_BLOB_put, @BLOB_put, HOOKFLAG);
  HookApi(PChar(lib), 'BLOB_text_dump', @hook_BLOB_text_dump, @BLOB_text_dump, HOOKFLAG);
  HookApi(PChar(lib), 'BLOB_text_load', @hook_BLOB_text_load, @BLOB_text_load, HOOKFLAG);
  HookApi(PChar(lib), 'Bopen', @hook_Bopen, @Bopen, HOOKFLAG);
  HookApi(PChar(lib), 'isc_add_user', @hook_isc_add_user, @isc_add_user, HOOKFLAG);
  HookApi(PChar(lib), 'isc_array_gen_sdl', @hook_isc_array_gen_sdl, @isc_array_gen_sdl, HOOKFLAG);
  HookApi(PChar(lib), 'isc_array_get_slice', @hook_isc_array_get_slice, @isc_array_get_slice, HOOKFLAG);
  HookApi(PChar(lib), 'isc_array_lookup_bounds', @hook_isc_array_lookup_bounds, @isc_array_lookup_bounds, HOOKFLAG);
  HookApi(PChar(lib), 'isc_array_lookup_desc', @hook_isc_array_lookup_desc, @isc_array_lookup_desc, HOOKFLAG);
  HookApi(PChar(lib), 'isc_array_put_slice', @hook_isc_array_put_slice, @isc_array_put_slice, HOOKFLAG);
  HookApi(PChar(lib), 'isc_array_set_desc', @hook_isc_array_set_desc, @isc_array_set_desc, HOOKFLAG);
  HookApi(PChar(lib), 'isc_attach_database', @hook_isc_attach_database, @isc_attach_database, HOOKFLAG);
  HookApi(PChar(lib), 'isc_blob_default_desc', @hook_isc_blob_default_desc, @isc_blob_default_desc, HOOKFLAG);
  HookApi(PChar(lib), 'isc_blob_gen_bpb', @hook_isc_blob_gen_bpb, @isc_blob_gen_bpb, HOOKFLAG);
  HookApi(PChar(lib), 'isc_blob_info', @hook_isc_blob_info, @isc_blob_info, HOOKFLAG);
  HookApi(PChar(lib), 'isc_blob_lookup_desc', @hook_isc_blob_lookup_desc, @isc_blob_lookup_desc, HOOKFLAG);
  HookApi(PChar(lib), 'isc_blob_set_desc', @hook_isc_blob_set_desc, @isc_blob_set_desc, HOOKFLAG);
  HookApi(PChar(lib), 'isc_cancel_blob', @hook_isc_cancel_blob, @isc_cancel_blob, HOOKFLAG);
  HookApi(PChar(lib), 'isc_cancel_events', @hook_isc_cancel_events, @isc_cancel_events, HOOKFLAG);
  HookApi(PChar(lib), 'isc_close', @hook_isc_close, @isc_close, HOOKFLAG);
  HookApi(PChar(lib), 'isc_close_blob', @hook_isc_close_blob, @isc_close_blob, HOOKFLAG);
  HookApi(PChar(lib), 'isc_commit_retaining', @hook_isc_commit_retaining, @isc_commit_retaining, HOOKFLAG);
  HookApi(PChar(lib), 'isc_commit_transaction', @hook_isc_commit_transaction, @isc_commit_transaction, HOOKFLAG);
  HookApi(PChar(lib), 'isc_compile_request', @hook_isc_compile_request, @isc_compile_request, HOOKFLAG);
  HookApi(PChar(lib), 'isc_compile_request2', @hook_isc_compile_request2, @isc_compile_request2, HOOKFLAG);
  HookApi(PChar(lib), 'isc_create_blob', @hook_isc_create_blob, @isc_create_blob, HOOKFLAG);
  HookApi(PChar(lib), 'isc_create_blob2', @hook_isc_create_blob2, @isc_create_blob2, HOOKFLAG);
  HookApi(PChar(lib), 'isc_create_database', @hook_isc_create_database, @isc_create_database, HOOKFLAG);
  HookApi(PChar(lib), 'isc_database_info', @hook_isc_database_info, @isc_database_info, HOOKFLAG);
  HookApi(PChar(lib), 'isc_ddl', @hook_isc_ddl, @isc_ddl, HOOKFLAG);
  HookApi(PChar(lib), 'isc_declare', @hook_isc_declare, @isc_declare, HOOKFLAG);
  //HookApi(PChar(lib), 'isc_decode_date', @hook_isc_decode_date, @isc_decode_date, HOOKFLAG);
  //HookApi(PChar(lib), 'isc_decode_sql_date', @hook_isc_decode_sql_date,
  //  @isc_decode_sql_date, HOOKFLAG);
  //HookApi(PChar(lib), 'isc_decode_sql_time', @hook_isc_decode_sql_time,
  //  @isc_decode_sql_time, HOOKFLAG);
  //HookApi(PChar(lib), 'isc_decode_timestamp', @hook_isc_decode_timestamp,
  //  @isc_decode_timestamp, HOOKFLAG);
  HookApi(PChar(lib), 'isc_delete_user', @hook_isc_delete_user, @isc_delete_user, HOOKFLAG);
  HookApi(PChar(lib), 'isc_describe', @hook_isc_describe, @isc_describe, HOOKFLAG);
  HookApi(PChar(lib), 'isc_describe_bind', @hook_isc_describe_bind, @isc_describe_bind, HOOKFLAG);
  HookApi(PChar(lib), 'isc_detach_database', @hook_isc_detach_database, @isc_detach_database, HOOKFLAG);
  HookApi(PChar(lib), 'isc_drop_database', @hook_isc_drop_database, @isc_drop_database, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_alloc_statement2', @hook_isc_dsql_alloc_statement2, @isc_dsql_alloc_statement2, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_allocate_statement', @hook_isc_dsql_allocate_statement, @isc_dsql_allocate_statement, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_describe', @hook_isc_dsql_describe, @isc_dsql_describe, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_describe_bind', @hook_isc_dsql_describe_bind, @isc_dsql_describe_bind, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_exec_immed2', @hook_isc_dsql_exec_immed2, @isc_dsql_exec_immed2, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_exec_immed3_m', @hook_isc_dsql_exec_immed3_m, @isc_dsql_exec_immed3_m, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_execute', @hook_isc_dsql_execute, @isc_dsql_execute, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_execute_immediate', @hook_isc_dsql_execute_immediate, @isc_dsql_execute_immediate, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_execute_immediate_m', @hook_isc_dsql_execute_immediate_m, @isc_dsql_execute_immediate_m, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_execute_m', @hook_isc_dsql_execute_m, @isc_dsql_execute_m, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_execute2', @hook_isc_dsql_execute2, @isc_dsql_execute2, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_execute2_m', @hook_isc_dsql_execute2_m, @isc_dsql_execute2_m, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_fetch', @hook_isc_dsql_fetch, @isc_dsql_fetch, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_fetch_m', @hook_isc_dsql_fetch_m, @isc_dsql_fetch_m, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_finish', @hook_isc_dsql_finish, @isc_dsql_finish, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_free_statement', @hook_isc_dsql_free_statement, @isc_dsql_free_statement, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_insert', @hook_isc_dsql_insert, @isc_dsql_insert, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_insert_m', @hook_isc_dsql_insert_m, @isc_dsql_insert_m, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_prepare', @hook_isc_dsql_prepare, @isc_dsql_prepare, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_prepare_m', @hook_isc_dsql_prepare_m, @isc_dsql_prepare_m, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_release', @hook_isc_dsql_release, @isc_dsql_release, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_set_cursor_name', @hook_isc_dsql_set_cursor_name, @isc_dsql_set_cursor_name, HOOKFLAG);
  HookApi(PChar(lib), 'isc_dsql_sql_info', @hook_isc_dsql_sql_info, @isc_dsql_sql_info, HOOKFLAG);
  HookApi(PChar(lib), 'isc_embed_dsql_close', @hook_isc_embed_dsql_close, @isc_embed_dsql_close, HOOKFLAG);
  HookApi(PChar(lib), 'isc_embed_dsql_declare', @hook_isc_embed_dsql_declare, @isc_embed_dsql_declare, HOOKFLAG);
  HookApi(PChar(lib), 'isc_embed_dsql_describe', @hook_isc_embed_dsql_describe, @isc_embed_dsql_describe, HOOKFLAG);
  HookApi(PChar(lib), 'isc_embed_dsql_describe_bind', @hook_isc_embed_dsql_describe_bind, @isc_embed_dsql_describe_bind, HOOKFLAG);
  HookApi(PChar(lib), 'isc_embed_dsql_execute', @hook_isc_embed_dsql_execute, @isc_embed_dsql_execute, HOOKFLAG);
  HookApi(PChar(lib), 'isc_embed_dsql_execute_immed', @hook_isc_embed_dsql_execute_immed, @isc_embed_dsql_execute_immed, HOOKFLAG);
  HookApi(PChar(lib), 'isc_embed_dsql_execute2', @hook_isc_embed_dsql_execute2, @isc_embed_dsql_execute2, HOOKFLAG);
  HookApi(PChar(lib), 'isc_embed_dsql_fetch', @hook_isc_embed_dsql_fetch, @isc_embed_dsql_fetch, HOOKFLAG);
  HookApi(PChar(lib), 'isc_embed_dsql_insert', @hook_isc_embed_dsql_insert, @isc_embed_dsql_insert, HOOKFLAG);
  HookApi(PChar(lib), 'isc_embed_dsql_open', @hook_isc_embed_dsql_open, @isc_embed_dsql_open, HOOKFLAG);
  HookApi(PChar(lib), 'isc_embed_dsql_open2', @hook_isc_embed_dsql_open2, @isc_embed_dsql_open2, HOOKFLAG);
  HookApi(PChar(lib), 'isc_embed_dsql_prepare', @hook_isc_embed_dsql_prepare, @isc_embed_dsql_prepare, HOOKFLAG);
  HookApi(PChar(lib), 'isc_embed_dsql_release', @hook_isc_embed_dsql_release, @isc_embed_dsql_release, HOOKFLAG);
//  HookApi(PChar(lib), 'isc_encode_date', @hook_isc_encode_date, @isc_encode_date, HOOKFLAG);
//  HookApi(PChar(lib), 'isc_encode_sql_date', @hook_isc_encode_sql_date, @isc_encode_sql_date, HOOKFLAG);
//  HookApi(PChar(lib), 'isc_encode_sql_time', @hook_isc_encode_sql_time, @isc_encode_sql_time, HOOKFLAG);
//  HookApi(PChar(lib), 'isc_encode_timestamp', @hook_isc_encode_timestamp, @isc_encode_timestamp, HOOKFLAG);
  HookApi(PChar(lib), 'isc_event_block', @hook_isc_event_block, @isc_event_block, HOOKFLAG);
  HookApi(PChar(lib), 'isc_event_counts', @hook_isc_event_counts, @isc_event_counts, HOOKFLAG);
  HookApi(PChar(lib), 'isc_execute', @hook_isc_execute, @isc_execute, HOOKFLAG);
  HookApi(PChar(lib), 'isc_execute_immediate', @hook_isc_execute_immediate, @isc_execute_immediate, HOOKFLAG);
  HookApi(PChar(lib), 'isc_expand_dpb', @hook_isc_expand_dpb, @isc_expand_dpb, HOOKFLAG);
  HookApi(PChar(lib), 'isc_fetch', @hook_isc_fetch, @isc_fetch, HOOKFLAG);
  HookApi(PChar(lib), 'isc_free', @hook_isc_free, @isc_free, HOOKFLAG);
  HookApi(PChar(lib), 'isc_ftof', @hook_isc_ftof, @isc_ftof, HOOKFLAG);
  HookApi(PChar(lib), 'isc_get_segment', @hook_isc_get_segment, @isc_get_segment, HOOKFLAG);
  HookApi(PChar(lib), 'isc_get_slice', @hook_isc_get_slice, @isc_get_slice, HOOKFLAG);
  HookApi(PChar(lib), 'isc_interprete', @hook_isc_interprete, @isc_interprete, HOOKFLAG);
{$IFDEF FB20_UP}
  HookApi(PChar(lib), 'fb_interpret', @hook_fb_interpret, @fb_interpret, HOOKFLAG);
{$ENDIF}
  HookApi(PChar(lib), 'isc_modify_dpb', @hook_isc_modify_dpb, @isc_modify_dpb, HOOKFLAG);
  HookApi(PChar(lib), 'isc_modify_user', @hook_isc_modify_user, @isc_modify_user, HOOKFLAG);
  HookApi(PChar(lib), 'isc_open', @hook_isc_open, @isc_open, HOOKFLAG);
  HookApi(PChar(lib), 'isc_open_blob', @hook_isc_open_blob, @isc_open_blob, HOOKFLAG);
  HookApi(PChar(lib), 'isc_open_blob2', @hook_isc_open_blob2, @isc_open_blob2, HOOKFLAG);
  HookApi(PChar(lib), 'isc_portable_integer', @hook_isc_portable_integer, @isc_portable_integer, HOOKFLAG);
  HookApi(PChar(lib), 'isc_prepare', @hook_isc_prepare, @isc_prepare, HOOKFLAG);
  HookApi(PChar(lib), 'isc_prepare_transaction', @hook_isc_prepare_transaction, @isc_prepare_transaction, HOOKFLAG);
  HookApi(PChar(lib), 'isc_prepare_transaction2', @hook_isc_prepare_transaction2, @isc_prepare_transaction2, HOOKFLAG);
  HookApi(PChar(lib), 'isc_print_blr', @hook_isc_print_blr, @isc_print_blr, HOOKFLAG);
  HookApi(PChar(lib), 'isc_print_sqlerror', @hook_isc_print_sqlerror, @isc_print_sqlerror, HOOKFLAG);
  HookApi(PChar(lib), 'isc_print_status', @hook_isc_print_status, @isc_print_status, HOOKFLAG);
  HookApi(PChar(lib), 'isc_put_segment', @hook_isc_put_segment, @isc_put_segment, HOOKFLAG);
  HookApi(PChar(lib), 'isc_put_slice', @hook_isc_put_slice, @isc_put_slice, HOOKFLAG);
  HookApi(PChar(lib), 'isc_qtoq', @hook_isc_qtoq, @isc_qtoq, HOOKFLAG);
  HookApi(PChar(lib), 'isc_que_events', @hook_isc_que_events, @isc_que_events, HOOKFLAG);
  HookApi(PChar(lib), 'isc_receive', @hook_isc_receive, @isc_receive, HOOKFLAG);
  HookApi(PChar(lib), 'isc_reconnect_transaction', @hook_isc_reconnect_transaction, @isc_reconnect_transaction, HOOKFLAG);
  HookApi(PChar(lib), 'isc_release_request', @hook_isc_release_request, @isc_release_request, HOOKFLAG);
  HookApi(PChar(lib), 'isc_request_info', @hook_isc_request_info, @isc_request_info, HOOKFLAG);
  HookApi(PChar(lib), 'isc_rollback_retaining', @hook_isc_rollback_retaining, @isc_rollback_retaining, HOOKFLAG);
  HookApi(PChar(lib), 'isc_rollback_transaction', @hook_isc_rollback_transaction, @isc_rollback_transaction, HOOKFLAG);
  HookApi(PChar(lib), 'isc_seek_blob', @hook_isc_seek_blob, @isc_seek_blob, HOOKFLAG);
  HookApi(PChar(lib), 'isc_send', @hook_isc_send, @isc_send, HOOKFLAG);
  HookApi(PChar(lib), 'isc_service_attach', @hook_isc_service_attach, @isc_service_attach, HOOKFLAG);
  HookApi(PChar(lib), 'isc_service_detach', @hook_isc_service_detach, @isc_service_detach, HOOKFLAG);
  HookApi(PChar(lib), 'isc_service_query', @hook_isc_service_query, @isc_service_query, HOOKFLAG);
  HookApi(PChar(lib), 'isc_service_start', @hook_isc_service_start, @isc_service_start, HOOKFLAG);
{$IFDEF INTERBASEORFIREBIRD}
  HookApi(PChar(lib), 'isc_set_debug', @hook_isc_set_debug, @isc_set_debug, HOOKFLAG);
{$ENDIF INTERBASEORFIREBIRD}
  HookApi(PChar(lib), 'isc_sql_interprete', @hook_isc_sql_interprete, @isc_sql_interprete, HOOKFLAG);
  HookApi(PChar(lib), 'isc_sqlcode', @hook_isc_sqlcode, @isc_sqlcode, HOOKFLAG);
  HookApi(PChar(lib), 'isc_start_and_send', @hook_isc_start_and_send, @isc_start_and_send, HOOKFLAG);
  HookApi(PChar(lib), 'isc_start_multiple', @hook_isc_start_multiple, @isc_start_multiple, HOOKFLAG);
  HookApi(PChar(lib), 'isc_start_request', @hook_isc_start_request, @isc_start_request, HOOKFLAG);
  HookApi(PChar(lib), 'isc_start_transaction', @hook_isc_start_transaction, @isc_start_transaction, HOOKFLAG);
  HookApi(PChar(lib), 'isc_transact_request', @hook_isc_transact_request, @isc_transact_request, HOOKFLAG);
  HookApi(PChar(lib), 'isc_transaction_info', @hook_isc_transaction_info, @isc_transaction_info, HOOKFLAG);
  HookApi(PChar(lib), 'isc_unwind_request', @hook_isc_unwind_request, @isc_unwind_request, HOOKFLAG);
  //HookApi(PChar(lib), 'isc_vax_integer', @hook_isc_vax_integer, @isc_vax_integer, HOOKFLAG);
  HookApi(PChar(lib), 'isc_version', @hook_isc_version, @isc_version, HOOKFLAG);
  HookApi(PChar(lib), 'isc_vtof', @hook_isc_vtof, @isc_vtof, HOOKFLAG);
  HookApi(PChar(lib), 'isc_vtov', @hook_isc_vtov, @isc_vtov, HOOKFLAG);
  HookApi(PChar(lib), 'isc_wait_for_event', @hook_isc_wait_for_event, @isc_wait_for_event, HOOKFLAG);
{$IFDEF FB15_UP}
  HookApi(PChar(lib), 'isc_reset_fpe', @hook_isc_reset_fpe, @isc_reset_fpe, HOOKFLAG);
{$ENDIF FB15_UP}
{$IFDEF IB7_UP}
  HookApi(PChar(lib), 'isc_array_gen_sdl2', @hook_isc_array_gen_sdl2, @isc_array_gen_sdl2, HOOKFLAG);
  HookApi(PChar(lib), 'isc_array_get_slice2', @hook_isc_array_get_slice2, @isc_array_get_slice2, HOOKFLAG);
  HookApi(PChar(lib), 'isc_array_lookup_bounds2', @hook_isc_array_lookup_bounds2, @isc_array_lookup_bounds2, HOOKFLAG);
  HookApi(PChar(lib), 'isc_array_lookup_desc2', @hook_isc_array_lookup_desc2, @isc_array_lookup_desc2, HOOKFLAG);
  HookApi(PChar(lib), 'isc_array_put_slice2', @hook_isc_array_put_slice2, @isc_array_put_slice2, HOOKFLAG);
  HookApi(PChar(lib), 'isc_array_set_desc2', @hook_isc_array_set_desc2, @isc_array_set_desc2, HOOKFLAG);
  HookApi(PChar(lib), 'isc_blob_default_desc2', @hook_isc_blob_default_desc2, @isc_blob_default_desc2, HOOKFLAG);
  HookApi(PChar(lib), 'isc_blob_gen_bpb2', @hook_isc_blob_gen_bpb2, @isc_blob_gen_bpb2, HOOKFLAG);
  HookApi(PChar(lib), 'isc_blob_lookup_desc2', @hook_isc_blob_lookup_desc2, @isc_blob_lookup_desc2, HOOKFLAG);
  HookApi(PChar(lib), 'isc_blob_set_desc2', @hook_isc_blob_set_desc2, @isc_blob_set_desc2, HOOKFLAG);
{$ENDIF IB7_UP}
{$IFDEF IB7ORFB15}
  HookApi(PChar(lib), 'isc_get_client_version', @hook_isc_get_client_version, @isc_get_client_version, HOOKFLAG);
  HookApi(PChar(lib), 'isc_get_client_major_version', @hook_isc_get_client_major_version, @isc_get_client_major_version, HOOKFLAG);
  HookApi(PChar(lib), 'isc_get_client_minor_version', @hook_isc_get_client_minor_version, @isc_get_client_minor_version, HOOKFLAG);
{$ENDIF IB7ORFB15}
{$IFDEF IB71_UP}
  HookApi(PChar(lib), 'isc_release_savepoint', @hook_isc_release_savepoint, @isc_release_savepoint, HOOKFLAG);
  HookApi(PChar(lib), 'isc_rollback_savepoint', @hook_isc_rollback_savepoint, @isc_rollback_savepoint, HOOKFLAG);
  HookApi(PChar(lib), 'isc_start_savepoint', @hook_isc_start_savepoint, @isc_start_savepoint, HOOKFLAG);
{$ENDIF IB71_UP}
  FlushHooks;
  Result := true;
end;

function TUIBHookKedLibrary.Loaded: Boolean;
begin
  Result := true;
end;

function TUIBHookKedLibrary.Unload: Boolean;
begin
  UnhookAPI(@BLOB_close);
{$IFDEF INTERBASEORFIREBIRD}
  UnHookApi(@BLOB_display);
{$ENDIF INTERBASEORFIREBIRD}
  UnHookApi(@BLOB_dump);
  UnHookApi(@BLOB_edit);
  UnHookApi(@BLOB_get);
  UnHookApi(@BLOB_load);
  UnHookApi(@BLOB_open);
  UnHookApi(@BLOB_put);
  UnHookApi(@BLOB_text_dump);
  UnHookApi(@BLOB_text_load);
  UnHookApi(@Bopen);
  UnHookApi(@isc_add_user);
  UnHookApi(@isc_array_gen_sdl);
  UnHookApi(@isc_array_get_slice);
  UnHookApi(@isc_array_lookup_bounds);
  UnHookApi(@isc_array_lookup_desc);
  UnHookApi(@isc_array_put_slice);
  UnHookApi(@isc_array_set_desc);
  UnHookApi(@isc_attach_database);
  UnHookApi(@isc_blob_default_desc);
  UnHookApi(@isc_blob_gen_bpb);
  UnHookApi(@isc_blob_info);
  UnHookApi(@isc_blob_lookup_desc);
  UnHookApi(@isc_blob_set_desc);
  UnHookApi(@isc_cancel_blob);
  UnHookApi(@isc_cancel_events);
  UnHookApi(@isc_close);
  UnHookApi(@isc_close_blob);
  UnHookApi(@isc_commit_retaining);
  UnHookApi(@isc_commit_transaction);
  UnHookApi(@isc_compile_request);
  UnHookApi(@isc_compile_request2);
  UnHookApi(@isc_create_blob);
  UnHookApi(@isc_create_blob2);
  UnHookApi(@isc_create_database);
  UnHookApi(@isc_database_info);
  UnHookApi(@isc_ddl);
  UnHookApi(@isc_declare);
  UnHookApi(@isc_delete_user);
  UnHookApi(@isc_describe);
  UnHookApi(@isc_describe_bind);
  UnHookApi(@isc_detach_database);
  UnHookApi(@isc_drop_database);
  UnHookApi(@isc_dsql_alloc_statement2);
  UnHookApi(@isc_dsql_allocate_statement);
  UnHookApi(@isc_dsql_describe);
  UnHookApi(@isc_dsql_describe_bind);
  UnHookApi(@isc_dsql_exec_immed2);
  UnHookApi(@isc_dsql_exec_immed3_m);
  UnHookApi(@isc_dsql_execute);
  UnHookApi(@isc_dsql_execute_immediate);
  UnHookApi(@isc_dsql_execute_immediate_m);
  UnHookApi(@isc_dsql_execute_m);
  UnHookApi(@isc_dsql_execute2);
  UnHookApi(@isc_dsql_execute2_m);
  UnHookApi(@isc_dsql_fetch);
  UnHookApi(@isc_dsql_fetch_m);
  UnHookApi(@isc_dsql_finish);
  UnHookApi(@isc_dsql_free_statement);
  UnHookApi(@isc_dsql_insert);
  UnHookApi(@isc_dsql_insert_m);
  UnHookApi(@isc_dsql_prepare);
  UnHookApi(@isc_dsql_prepare_m);
  UnHookApi(@isc_dsql_release);
  UnHookApi(@isc_dsql_set_cursor_name);
  UnHookApi(@isc_dsql_sql_info);
  UnHookApi(@isc_embed_dsql_close);
  UnHookApi(@isc_embed_dsql_declare);
  UnHookApi(@isc_embed_dsql_describe);
  UnHookApi(@isc_embed_dsql_describe_bind);
  UnHookApi(@isc_embed_dsql_execute);
  UnHookApi(@isc_embed_dsql_execute_immed);
  UnHookApi(@isc_embed_dsql_execute2);
  UnHookApi(@isc_embed_dsql_fetch);
  UnHookApi(@isc_embed_dsql_insert);
  UnHookApi(@isc_embed_dsql_open);
  UnHookApi(@isc_embed_dsql_open2);
  UnHookApi(@isc_embed_dsql_prepare);
  UnHookApi(@isc_embed_dsql_release);
  UnHookApi(@isc_event_block);
  UnHookApi(@isc_event_counts);
  UnHookApi(@isc_execute);
  UnHookApi(@isc_execute_immediate);
  UnHookApi(@isc_expand_dpb);
  UnHookApi(@isc_fetch);
  UnHookApi(@isc_free);
  UnHookApi(@isc_ftof);
  UnHookApi(@isc_get_segment);
  UnHookApi(@isc_get_slice);
  UnHookApi(@isc_interprete);
{$IFDEF FB20_UP}
  UnHookApi(@fb_interpret);
{$ENDIF}
  UnHookApi(@isc_modify_dpb);
  UnHookApi(@isc_modify_user);
  UnHookApi(@isc_open);
  UnHookApi(@isc_open_blob);
  UnHookApi(@isc_open_blob2);
  UnHookApi(@isc_portable_integer);
  UnHookApi(@isc_prepare);
  UnHookApi(@isc_prepare_transaction);
  UnHookApi(@isc_prepare_transaction2);
  UnHookApi(@isc_print_blr);
  UnHookApi(@isc_print_sqlerror);
  UnHookApi(@isc_print_status);
  UnHookApi(@isc_put_segment);
  UnHookApi(@isc_put_slice);
  UnHookApi(@isc_qtoq);
  UnHookApi(@isc_que_events);
  UnHookApi(@isc_receive);
  UnHookApi(@isc_reconnect_transaction);
  UnHookApi(@isc_release_request);
  UnHookApi(@isc_request_info);
  UnHookApi(@isc_rollback_retaining);
  UnHookApi(@isc_rollback_transaction);
  UnHookApi(@isc_seek_blob);
  UnHookApi(@isc_send);
  UnHookApi(@isc_service_attach);
  UnHookApi(@isc_service_detach);
  UnHookApi(@isc_service_query);
  UnHookApi(@isc_service_start);
{$IFDEF INTERBASEORFIREBIRD}
  UnHookApi(@isc_set_debug);
{$ENDIF INTERBASEORFIREBIRD}
  UnHookApi(@isc_sql_interprete);
  UnHookApi(@isc_sqlcode);
  UnHookApi(@isc_start_and_send);
  UnHookApi(@isc_start_multiple);
  UnHookApi(@isc_start_request);
  UnHookApi(@isc_start_transaction);
  UnHookApi(@isc_transact_request);
  UnHookApi(@isc_transaction_info);
  UnHookApi(@isc_unwind_request);
  UnHookApi(@isc_version);
  UnHookApi(@isc_vtof);
  UnHookApi(@isc_vtov);
  UnHookApi(@isc_wait_for_event);
{$IFDEF FB15_UP}
  UnHookApi(@isc_reset_fpe);
{$ENDIF FB15_UP}
{$IFDEF IB7_UP}
  UnHookApi(@isc_array_gen_sdl2);
  UnHookApi(@isc_array_get_slice2);
  UnHookApi(@isc_array_lookup_bounds2);
  UnHookApi(@isc_array_lookup_desc2);
  UnHookApi(@isc_array_put_slice2);
  UnHookApi(@isc_array_set_desc2);
  UnHookApi(@isc_blob_default_desc2);
  UnHookApi(@isc_blob_gen_bpb2);
  UnHookApi(@isc_blob_lookup_desc2);
  UnHookApi(@isc_blob_set_desc2);
{$ENDIF IB7_UP}
{$IFDEF IB7ORFB15}
  UnHookApi(@isc_get_client_version);
  UnHookApi(@isc_get_client_major_version);
  UnHookApi(@isc_get_client_minor_version);
{$ENDIF IB7ORFB15}
{$IFDEF IB71_UP}
  UnHookApi(@isc_release_savepoint);
  UnHookApi(@isc_rollback_savepoint);
  UnHookApi(@isc_start_savepoint);
{$ENDIF IB71_UP}
  Result := true;
end;

initialization
  UIBHOOKIPC := GetEnvironmentVariable('UIBHOOKIPC');
  HookedLib := TUIBHookKedLibrary.Create;
  HookedLib.Load(GetEnvironmentVariable('UIBHOOKLIB'));
finalization
  HookedLib.Free;
  UninstallMadCHook;

end.


