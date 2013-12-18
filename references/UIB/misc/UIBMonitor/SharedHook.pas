(* The contents of this file are subject to the Mozilla Public License
   Version 1.1 (the "License"); you may not use this file except in compliance
   with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
   Software distributed under the License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.
*)

unit SharedHook;
{$I uib.inc}
{$ALIGN ON}
{$MINENUMSIZE 4}

interface
uses Classes, uibase, SysUtils;

type
  THookedMethod = (
    hm_BLOB_close,
{$IFDEF INTERBASEORFIREBIRD}
    hm_BLOB_display,
{$ENDIF INTERBASEORFIREBIRD}
    hm_BLOB_dump,
    hm_BLOB_edit,
    hm_BLOB_get,
    hm_BLOB_load,
    hm_BLOB_open,
    hm_BLOB_put,
    hm_BLOB_text_dump,
    hm_BLOB_text_load,
    hm_Bopen,
    hm_isc_add_user,
    hm_isc_array_gen_sdl,
    hm_isc_array_get_slice,
    hm_isc_array_lookup_bounds,
    hm_isc_array_lookup_desc,
    hm_isc_array_put_slice,
    hm_isc_array_set_desc,
    hm_isc_attach_database,
    hm_isc_blob_default_desc,
    hm_isc_blob_gen_bpb,
    hm_isc_blob_info,
    hm_isc_blob_lookup_desc,
    hm_isc_blob_set_desc,
    hm_isc_cancel_blob,
    hm_isc_cancel_events,
    hm_isc_close,
    hm_isc_close_blob,
    hm_isc_commit_retaining,
    hm_isc_commit_transaction,
    hm_isc_compile_request,
    hm_isc_compile_request2,
    hm_isc_create_blob,
    hm_isc_create_blob2,
    hm_isc_create_database,
    hm_isc_database_info,
    hm_isc_ddl,
    hm_isc_declare,
    hm_isc_delete_user,
    hm_isc_describe,
    hm_isc_describe_bind,
    hm_isc_detach_database,
    hm_isc_drop_database,
    hm_isc_dsql_allocate_statement,
    hm_isc_dsql_alloc_statement2,
    hm_isc_dsql_describe,
    hm_isc_dsql_describe_bind,
    hm_isc_dsql_exec_immed2,
    hm_isc_dsql_execute,
    hm_isc_dsql_execute_immediate,
    hm_isc_dsql_execute2,
    hm_isc_dsql_fetch,
    hm_isc_dsql_finish,
    hm_isc_dsql_free_statement,
    hm_isc_dsql_insert,
    hm_isc_dsql_prepare,
    hm_isc_dsql_release,
    hm_isc_dsql_set_cursor_name,
    hm_isc_dsql_sql_info,
    hm_isc_embed_dsql_close,
    hm_isc_embed_dsql_declare,
    hm_isc_embed_dsql_describe,
    hm_isc_embed_dsql_describe_bind,
    hm_isc_embed_dsql_execute,
    hm_isc_embed_dsql_execute_immed,
    hm_isc_embed_dsql_execute2,
    hm_isc_embed_dsql_fetch,
    hm_isc_embed_dsql_insert,
    hm_isc_embed_dsql_open,
    hm_isc_embed_dsql_open2,
    hm_isc_embed_dsql_prepare,
    hm_isc_embed_dsql_release,
    hm_isc_encode_date,
    hm_isc_encode_sql_date,
    hm_isc_encode_sql_time,
    hm_isc_encode_timestamp,
    hm_isc_event_block,
    hm_isc_event_counts,
    hm_isc_execute,
    hm_isc_execute_immediate,
    hm_isc_expand_dpb,
    hm_isc_fetch,
    hm_isc_free,
    hm_isc_ftof,
    hm_isc_get_segment,
    hm_isc_get_slice,
    hm_isc_interprete,
{$IFDEF FB20_UP}
    hm_fb_interpret,
{$ENDIF}
    hm_isc_modify_dpb,
    hm_isc_modify_user,
    hm_isc_open,
    hm_isc_open_blob,
    hm_isc_open_blob2,
    hm_isc_portable_integer,
    hm_isc_prepare,
    hm_isc_prepare_transaction,
    hm_isc_prepare_transaction2,
    hm_isc_print_blr,
    hm_isc_print_sqlerror,
    hm_isc_print_status,
    hm_isc_put_segment,
    hm_isc_put_slice,
    hm_isc_qtoq,
    hm_isc_que_events,
    hm_isc_receive,
    hm_isc_reconnect_transaction,
    hm_isc_release_request,
    hm_isc_request_info,
    hm_isc_rollback_retaining,
    hm_isc_rollback_transaction,
    hm_isc_seek_blob,
    hm_isc_send,
    hm_isc_service_attach,
    hm_isc_service_detach,
    hm_isc_service_query,
    hm_isc_service_start,
{$IFDEF INTERBASEORFIREBIRD}
    hm_isc_set_debug,
{$ENDIF INTERBASEORFIREBIRD}
    hm_isc_sql_interprete,
    hm_isc_sqlcode,
    hm_isc_start_and_send,
    hm_isc_start_multiple,
    hm_isc_start_request,
    hm_isc_start_transaction,
    hm_isc_transact_request,
    hm_isc_transaction_info,
    hm_isc_unwind_request,
    hm_isc_version,
    hm_isc_vtof,
    hm_isc_vtov,
    hm_isc_wait_for_event
{$IFDEF FB15_UP}
    ,hm_isc_reset_fpe
{$ENDIF FB15_UP}
{$IFDEF IB7_UP}
    ,hm_isc_array_gen_sdl2
    ,hm_isc_array_get_slice2
    ,hm_isc_array_lookup_bounds2
    ,hm_isc_array_lookup_desc2
    ,hm_isc_array_put_slice2
    ,hm_isc_array_set_desc2
    ,hm_isc_blob_default_desc2
    ,hm_isc_blob_gen_bpb2
    ,hm_isc_blob_lookup_desc2
    ,hm_isc_blob_set_desc2
{$ENDIF IB7_UP}
{$IFDEF IB7ORFB15}
    ,hm_isc_get_client_version
    ,hm_isc_get_client_major_version
    ,hm_isc_get_client_minor_version
{$ENDIF IB7ORFB15}
{$IFDEF IB71_UP}
    ,hm_isc_release_savepoint
    ,hm_isc_rollback_savepoint
    ,hm_isc_start_savepoint
{$ENDIF IB71_UP}
    ,hm_isc_dsql_exec_immed3_m
    ,hm_isc_dsql_execute_immediate_m
    ,hm_isc_dsql_execute_m
    ,hm_isc_dsql_execute2_m
    ,hm_isc_dsql_fetch_m
    ,hm_isc_dsql_insert_m
    ,hm_isc_dsql_prepare_m
  );

  THookedMethodHeader = object
    index: Integer;
    timestamp: TDateTime;
    methodid: THookedMethod;
    threadid: Cardinal;
    cstart, cstop: int64;
    status: ISCStatus;
  end;

  function MethodIdToString(id: THookedMethod): string;
  function IsStatusError(status: ISCStatus): boolean;

type
  TStreamMethod = class(TMemoryStream)
  public
    procedure WriteHeader(var header: THookedMethodHeader);
    procedure ReadHeader(var header: THookedMethodHeader);
    procedure WriteItem(item: pointer; size: Integer);
    procedure WriteString(str: PChar); overload;
    procedure WriteString(const str: string); overload;
    function ReadString: string;
    function ReadInteger: Integer;
    procedure WriteHandle(handle: Pointer);
    procedure WriteInteger(Value: Integer);
  end;

implementation

function MethodIdToString(id: THookedMethod): string;
begin
  case id of
    hm_BLOB_close: Result := 'BLOB_close';
{$IFDEF INTERBASEORFIREBIRD}
    hm_BLOB_display: Result := 'BLOB_display';
{$ENDIF INTERBASEORFIREBIRD}
    hm_BLOB_dump: Result := 'BLOB_dump';
    hm_BLOB_edit: Result := 'BLOB_edit';
    hm_BLOB_get: Result := 'BLOB_get';
    hm_BLOB_load: Result := 'BLOB_load';
    hm_BLOB_open: Result := 'BLOB_open';
    hm_BLOB_put: Result := 'BLOB_put';
    hm_BLOB_text_dump: Result := 'BLOB_text_dump';
    hm_BLOB_text_load: Result := 'BLOB_text_load';
    hm_Bopen: Result := 'Bopen';
    hm_isc_add_user: Result := 'isc_add_user';
    hm_isc_array_gen_sdl: Result := 'isc_array_gen_sdl';
    hm_isc_array_get_slice: Result := 'isc_array_get_slice';
    hm_isc_array_lookup_bounds: Result := 'isc_array_lookup_bounds';
    hm_isc_array_lookup_desc: Result := 'isc_array_lookup_desc';
    hm_isc_array_put_slice: Result := 'isc_array_put_slice';
    hm_isc_array_set_desc: Result := 'isc_array_set_desc';
    hm_isc_attach_database: Result := 'isc_attach_database';
    hm_isc_blob_default_desc: Result := 'isc_blob_default_desc';
    hm_isc_blob_gen_bpb: Result := 'isc_blob_gen_bpb';
    hm_isc_blob_info: Result := 'isc_blob_info';
    hm_isc_blob_lookup_desc: Result := 'isc_blob_lookup_desc';
    hm_isc_blob_set_desc: Result := 'isc_blob_set_desc';
    hm_isc_cancel_blob: Result := 'isc_cancel_blob';
    hm_isc_cancel_events: Result := 'isc_cancel_events';
    hm_isc_close: Result := 'isc_close';
    hm_isc_close_blob: Result := 'isc_close_blob';
    hm_isc_commit_retaining: Result := 'isc_commit_retaining';
    hm_isc_commit_transaction: Result := 'isc_commit_transaction';
    hm_isc_compile_request: Result := 'isc_compile_request';
    hm_isc_compile_request2: Result := 'isc_compile_request2';
    hm_isc_create_blob: Result := 'isc_create_blob';
    hm_isc_create_blob2: Result := 'isc_create_blob2';
    hm_isc_create_database: Result := 'isc_create_database';
    hm_isc_database_info: Result := 'isc_database_info';
    hm_isc_ddl: Result := 'isc_ddl';
    hm_isc_declare: Result := 'isc_declare';
    hm_isc_delete_user: Result := 'isc_delete_user';
    hm_isc_describe: Result := 'isc_describe';
    hm_isc_describe_bind: Result := 'isc_describe_bind';
    hm_isc_detach_database: Result := 'isc_detach_database';
    hm_isc_drop_database: Result := 'isc_drop_database';
    hm_isc_dsql_allocate_statement: Result := 'isc_dsql_allocate_statement';
    hm_isc_dsql_alloc_statement2: Result := 'isc_dsql_alloc_statement2';
    hm_isc_dsql_describe: Result := 'isc_dsql_describe';
    hm_isc_dsql_describe_bind: Result := 'isc_dsql_describe_bind';
    hm_isc_dsql_exec_immed2: Result := 'isc_dsql_exec_immed2';
    hm_isc_dsql_execute: Result := 'isc_dsql_execute';
    hm_isc_dsql_execute_immediate: Result := 'isc_dsql_execute_immediate';
    hm_isc_dsql_execute2: Result := 'isc_dsql_execute2';
    hm_isc_dsql_fetch: Result := 'isc_dsql_fetch';
    hm_isc_dsql_finish: Result := 'isc_dsql_finish';
    hm_isc_dsql_free_statement: Result := 'isc_dsql_free_statement';
    hm_isc_dsql_insert: Result := 'isc_dsql_insert';
    hm_isc_dsql_prepare: Result := 'isc_dsql_prepare';
    hm_isc_dsql_release: Result := 'isc_dsql_release';
    hm_isc_dsql_set_cursor_name: Result := 'isc_dsql_set_cursor_name';
    hm_isc_dsql_sql_info: Result := 'isc_dsql_sql_info';
    hm_isc_embed_dsql_close: Result := 'isc_embed_dsql_close';
    hm_isc_embed_dsql_declare: Result := 'isc_embed_dsql_declare';
    hm_isc_embed_dsql_describe: Result := 'isc_embed_dsql_describe';
    hm_isc_embed_dsql_describe_bind: Result := 'isc_embed_dsql_describe_bind';
    hm_isc_embed_dsql_execute: Result := 'isc_embed_dsql_execute';
    hm_isc_embed_dsql_execute_immed: Result := 'isc_embed_dsql_execute_immed';
    hm_isc_embed_dsql_execute2: Result := 'isc_embed_dsql_execute2';
    hm_isc_embed_dsql_fetch: Result := 'isc_embed_dsql_fetch';
    hm_isc_embed_dsql_insert: Result := 'isc_embed_dsql_insert';
    hm_isc_embed_dsql_open: Result := 'isc_embed_dsql_open';
    hm_isc_embed_dsql_open2: Result := 'isc_embed_dsql_open2';
    hm_isc_embed_dsql_prepare: Result := 'isc_embed_dsql_prepare';
    hm_isc_embed_dsql_release: Result := 'isc_embed_dsql_release';
    hm_isc_encode_date: Result := 'isc_encode_date';
    hm_isc_encode_sql_date: Result := 'isc_encode_sql_date';
    hm_isc_encode_sql_time: Result := 'isc_encode_sql_time';
    hm_isc_encode_timestamp: Result := 'isc_encode_timestamp';
    hm_isc_event_block: Result := 'isc_event_block';
    hm_isc_event_counts: Result := 'isc_event_counts';
    hm_isc_execute: Result := 'isc_execute';
    hm_isc_execute_immediate: Result := 'isc_execute_immediate';
    hm_isc_expand_dpb: Result := 'isc_expand_dpb';
    hm_isc_fetch: Result := 'isc_fetch';
    hm_isc_free: Result := 'isc_free';
    hm_isc_ftof: Result := 'isc_ftof';
    hm_isc_get_segment: Result := 'isc_get_segment';
    hm_isc_get_slice: Result := 'isc_get_slice';
    hm_isc_interprete: Result := 'isc_interprete';
{$IFDEF FB20_UP}
    hm_fb_interpret: Result := 'fb_interpret';
{$ENDIF}
    hm_isc_modify_dpb: Result := 'isc_modify_dpb';
    hm_isc_modify_user: Result := 'isc_modify_user';
    hm_isc_open: Result := 'isc_open';
    hm_isc_open_blob: Result := 'isc_open_blob';
    hm_isc_open_blob2: Result := 'isc_open_blob2';
    hm_isc_portable_integer: Result := 'isc_portable_integer';
    hm_isc_prepare: Result := 'isc_prepare';
    hm_isc_prepare_transaction: Result := 'isc_prepare_transaction';
    hm_isc_prepare_transaction2: Result := 'isc_prepare_transaction2';
    hm_isc_print_blr: Result := 'isc_print_blr';
    hm_isc_print_sqlerror: Result := 'isc_print_sqlerror';
    hm_isc_print_status: Result := 'isc_print_status';
    hm_isc_put_segment: Result := 'isc_put_segment';
    hm_isc_put_slice: Result := 'isc_put_slice';
    hm_isc_qtoq: Result := 'isc_qtoq';
    hm_isc_que_events: Result := 'isc_que_events';
    hm_isc_receive: Result := 'isc_receive';
    hm_isc_reconnect_transaction: Result := 'isc_reconnect_transaction';
    hm_isc_release_request: Result := 'isc_release_request';
    hm_isc_request_info: Result := 'isc_request_info';
    hm_isc_rollback_retaining: Result := 'isc_rollback_retaining';
    hm_isc_rollback_transaction: Result := 'isc_rollback_transaction';
    hm_isc_seek_blob: Result := 'isc_seek_blob';
    hm_isc_send: Result := 'isc_send';
    hm_isc_service_attach: Result := 'isc_service_attach';
    hm_isc_service_detach: Result := 'isc_service_detach';
    hm_isc_service_query: Result := 'isc_service_query';
    hm_isc_service_start: Result := 'isc_service_start';
{$IFDEF INTERBASEORFIREBIRD}
    hm_isc_set_debug: Result := 'isc_set_debug';
{$ENDIF INTERBASEORFIREBIRD}
    hm_isc_sql_interprete: Result := 'isc_sql_interprete';
    hm_isc_sqlcode: Result := 'isc_sqlcode';
    hm_isc_start_and_send: Result := 'isc_start_and_send';
    hm_isc_start_multiple: Result := 'isc_start_multiple';
    hm_isc_start_request: Result := 'isc_start_request';
    hm_isc_start_transaction: Result := 'isc_start_transaction';
    hm_isc_transact_request: Result := 'isc_transact_request';
    hm_isc_transaction_info: Result := 'isc_transaction_info';
    hm_isc_unwind_request: Result := 'isc_unwind_request';
    hm_isc_version: Result := 'isc_version';
    hm_isc_vtof: Result := 'isc_vtof';
    hm_isc_vtov: Result := 'isc_vtov';
    hm_isc_wait_for_event: Result := 'isc_wait_for_event';
{$IFDEF FB15_UP}
    hm_isc_reset_fpe: Result := 'isc_reset_fpe';
{$ENDIF FB15_UP}
{$IFDEF IB7_UP}
    hm_isc_array_gen_sdl2: Result := 'isc_array_gen_sdl2';
    hm_isc_array_get_slice2: Result := 'isc_array_get_slice2';
    hm_isc_array_lookup_bounds2: Result := 'isc_array_lookup_bounds2';
    hm_isc_array_lookup_desc2: Result := 'isc_array_lookup_desc2';
    hm_isc_array_put_slice2: Result := 'isc_array_put_slice2';
    hm_isc_array_set_desc2: Result := 'isc_array_set_desc2';
    hm_isc_blob_default_desc2: Result := 'isc_blob_default_desc2';
    hm_isc_blob_gen_bpb2: Result := 'isc_blob_gen_bpb2';
    hm_isc_blob_lookup_desc2: Result := 'isc_blob_lookup_desc2';
    hm_isc_blob_set_desc2: Result := 'isc_blob_set_desc2';
{$ENDIF IB7_UP}
{$IFDEF IB7ORFB15}
    hm_isc_get_client_version: Result := 'isc_get_client_version';
    hm_isc_get_client_major_version: Result := 'isc_get_client_major_version';
    hm_isc_get_client_minor_version: Result := 'isc_get_client_minor_version';
{$ENDIF IB7ORFB15}
{$IFDEF IB71_UP}
    hm_isc_release_savepoint: Result := 'isc_release_savepoint';
    hm_isc_rollback_savepoint: Result := 'isc_rollback_savepoint';
    hm_isc_start_savepoint: Result := 'isc_start_savepoint';
{$ENDIF IB71_UP}
    hm_isc_dsql_exec_immed3_m: Result := 'isc_dsql_exec_immed3_m';
    hm_isc_dsql_execute_immediate_m: Result := 'isc_dsql_execute_immediate_m';
    hm_isc_dsql_execute_m: Result := 'isc_dsql_execute_m';
    hm_isc_dsql_execute2_m: Result := 'isc_dsql_execute2_m';
    hm_isc_dsql_fetch_m: Result := 'isc_dsql_fetch_m';
    hm_isc_dsql_insert_m: Result := 'isc_dsql_insert_m';
    hm_isc_dsql_prepare_m: Result := 'isc_dsql_prepare_m';
  else
    Result := 'Unknow method';
  end;
end;

function IsStatusError(status: ISCStatus): boolean;
begin
  Result := (status <> 0) and ((status and $F0000000) shr 30 = 0);
end;

{ TStreamMethod }

procedure TStreamMethod.ReadHeader(var header: THookedMethodHeader);
begin
  Read(header, SizeOf(header));
end;

function TStreamMethod.ReadInteger: Integer;
begin
  Read(Result, SizeOf(Result));
end;

function TStreamMethod.ReadString: string;
var size: integer;
begin
  Read(size, sizeof(size));
  SetLength(Result, size);
  read(PChar(Result)^, size);
end;

procedure TStreamMethod.WriteHandle(handle: Pointer);
begin
  Write(handle^, SizeOf(Pointer));
end;

procedure TStreamMethod.WriteHeader(var header: THookedMethodHeader);
begin
  Write(header, SizeOf(header));
end;

procedure TStreamMethod.WriteInteger(Value: Integer);
begin
  Write(Value, SizeOf(Value));
end;

procedure TStreamMethod.WriteItem(item: pointer; size: Integer);
begin
  Write(size, sizeof(size));
  Write(item^, size);
end;

procedure TStreamMethod.WriteString(str: PChar);
var len: Integer;
begin
  len := StrLen(str);
  Write(len, sizeof(len));
  write(str^, len);
end;

procedure TStreamMethod.WriteString(const str: string);
var len: Integer;
begin
  len := Length(str);
  Write(len, sizeof(len));
  write(PChar(str)^, len);
end;

end.
