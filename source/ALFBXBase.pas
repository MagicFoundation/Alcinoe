{*******************************************************************************
Author(s):
Henri Gourvest <hgourvest@progdigy.com>
Olivier Guilbaud <oguilb@free.fr>
Volkan Ceylan <volkance@hotmail.com>
The Original Code is the UIB code (version 2.1)

The Initial Developer of the Original Code is
Henri Gourvest <hgourvest@progdigy.com>. Portions
created by the Initial Developer are Copyright (C)
by the Initial Developer. All Rights Reserved.

Description:
ALFBX (Alcinoe FireBird Express) does for the Firebird
API what Delphi does for the WINDOWS API! Create high
performance client/server applications based on FireBird
without the BDE or ODBC.

Link :
https://uib.svn.sourceforge.net/svnroot/uib (current code is from the trunk rev 391)
http://www.progdigy.com/modules.php?name=UIB
*******************************************************************************}

unit ALFBXBase;

{$IFNDEF CPU64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

interface

uses winapi.Windows;

(* Basic data types *)
type

  {$IFDEF CPUX64}
    PtrInt = IntPtr;
  {$ELSE}
    PtrInt = type Longint;
  {$ENDIF}

  UCHAR =  AnsiChar;
  USHORT =  Word;
  ULONG =  Cardinal;
  SCHAR =  AnsiChar;
  SSHORT =  Smallint;
  SLONG =  Integer;

  SQUAD = record
    high: SLONG;
    low: ULONG;
  end;

  PGDSQuad = ^TGDSQuad;
  GDS_QUAD = record
    gds_quad_high: SLONG;
    gds_quad_low: ULONG;
  end;
  TGDSQuad = GDS_QUAD;

{FB25_UP}
  FB_SHUTDOWN_CALLBACK = function(const reason, mask: Integer; arg: Pointer): Integer; cdecl;
{FB25_UP}

  // *************************************************
  // TMN: some misc data types from all over the place
  //**************************************************

  // Originally vary_length = SShort but if you need the correct length you
  // need to use isc_portable_integer that convert it to word so i simply convert
  // it directly to Word, Very strange.

  PVary = ^TVary;
  vary = record
    vary_length: USHORT;
    vary_string: array [0..0] of AnsiChar;
  end;
  TVary = vary;

  {FB15_UP}
  PLString = ^TLString;
  lstring = record
    lstr_length: ULONG;
    lstr_allocated: ULONG;
    lstr_address: ^UCHAR;
  end;
  TLString = lstring;
  {FB15_UP}

  TEXT =  AnsiChar; (* To be expunged over time *)
  STEXT =  AnsiChar; (* Signed text - very rare *)
  UTEXT =  AnsiChar; (* Unsigned text - common *)
  SBYTE =  Byte; (* Signed byte - rare usage *)
  STATUS =  Longint;
  IPTR =  Longint;
  U_IPTR =  Cardinal;
  RCRD_OFFSET =  Cardinal;
  FLD_LENGTH =  Word;

//typedef void (*FPTR_VOID) ();
//typedef void (*FPTR_VOID_PTR) (void *);
//typedef int (*FPTR_INT) ();
//typedef int (*FPTR_INT_VOID_PTR) (void *);

  TALFBXVersion_API = (FB102, FB103, FB15, FB20, FB21, FB25);
const
(* Number of elements in an arry *)

{FB15_UP}
{ TODO -oHG : TRANSLATE }
//#define FB_NELEM(x) ((int)(sizeof(x) / sizeof(x[0])))
//#define FB_ALIGN(n,b) ((n+b-1)&~(b-1))
{FB15_UP}

{FB15}
//  FB_API_VER = 15;
{FB15}

{FB20}
//  FB_API_VER = 20;
{FB20}

{FB25}
//  FB_API_VER = 25;
{FB25}

  GDS_TRUE = 1;
  GDS_FALSE = 0;

type
  (* We can remove these three #defines if we change gdsold.h and gen/codes.h *)
  GDS_LONG = SLONG;
  GDS_ULONG = ULONG;

  GDS_STATUS = Longint;

const
  ISC_TRUE = 1;
  ISC_FALSE = 0;

type
  ISC_LONG =  Integer;
  ISCLong = ISC_LONG;
  PISCLong = ^ISCLong;

  ISC_ULONG =  Cardinal;
  ISCULong = ISC_ULONG;
  PISCULong = ^ISCULong;

  ISC_SHORT =  Smallint;
  ISCShort = ISC_SHORT;
  PISCShort = ^ISCShort;

  ISC_USHORT =  Word;
  ISCUShort = ISC_USHORT;
  PISCUShort = ^ISCUShort;

  ISC_STATUS =  PtrInt;
  ISCStatus = ISC_STATUS;
  PISCStatus = ^ISCStatus;
  PPISCStatus = ^PISCStatus;

  ISC_UCHAR =  AnsiChar;
  ISCUChar = ISC_UCHAR;
  PISCUChar = ^ISCUChar;

const
  ISC_STATUS_LENGTH = 20;
type
  ISC_STATUS_ARRAY = array[0..ISC_STATUS_LENGTH - 1] of ISC_STATUS;

{FB25_UP}
const
  FB_SQLSTATE_SIZE  = 6;
type
  FB_SQLSTATE_STRING = array[0..FB_SQLSTATE_SIZE-1] of AnsiChar;
{FB25_UP}

const
  DSQL_close = 1;
  DSQL_drop = 2;

{FB25_UP}
  DSQL_unprepare = 4;
{FB25_UP}

  METADATALENGTH = 32;

(******************************************************************
 * Define type, export and other stuff based on c/c++ and Windows *
 ******************************************************************)

type
  ISC_INT64 =  Int64;
  ISCInt64 = ISC_INT64;

  ISC_UINT64 =  Int64;
  ISCUInt64 = ISC_UINT64;

(*******************************************************************
 * 64 bit Integers                                                 *
 *******************************************************************)

(*******************************************************************
 * Time & Date Support                                             *
 *******************************************************************)

  ISC_DATE =  Longint;
  ISCDate = ISC_DATE;
  PISCDate = ^ISCDate;

  ISC_TIME =  Cardinal;
  ISCTime = ISC_TIME;
  PISCTime = ^ISCTime;

  PISCTimeStamp = ^TISCTimeStamp;
  ISC_TIMESTAMP = record
    timestamp_date: ISC_DATE;
    timestamp_time: ISC_TIME;
  end;
  TISCTimeStamp = ISC_TIMESTAMP;

const
  ISC_TIME_SECONDS_PRECISION = 10000;
  ISC_TIME_SECONDS_PRECISION_SCALE = -4;

(*******************************************************************
 * Blob id structure                                               *
 *******************************************************************)

type
  ISC_QUAD = GDS_QUAD;
  TISCQuad =  ISC_QUAD;
  PISCQuad = ^TISCQuad;

  PISCArrayBound = ^TISCArrayBound;
  ISC_ARRAY_BOUND = record
    array_bound_lower: Smallint;
    array_bound_upper: Smallint;
  end;
  TISCArrayBound = ISC_ARRAY_BOUND;

type
  PISCArrayDesc = ^TISCArrayDesc;
  ISC_ARRAY_DESC = record
    array_desc_dtype: byte;
    array_desc_scale: byte;
    array_desc_length: Word;
    array_desc_field_name: array [0..METADATALENGTH - 1] of AnsiChar;
    array_desc_relation_name: array [0..METADATALENGTH - 1] of AnsiChar;
    array_desc_dimensions: Smallint;
    array_desc_flags: Smallint;
    array_desc_bounds: array [0..15] of TISCArrayBound;
  end;
  TISCArrayDesc =  ISC_ARRAY_DESC;

  PISCBlobDesc = ^TISCBlobDesc;
  ISC_BLOB_DESC = record
    blob_desc_subtype: Smallint;
    blob_desc_charset: Smallint;
    blob_desc_segment_size: Smallint;
    blob_desc_field_name: array [0..METADATALENGTH - 1] of AnsiChar;
    blob_desc_relation_name: array [0..METADATALENGTH - 1] of AnsiChar;
  end;
  TISCBlobDesc =  ISC_BLOB_DESC;

(***************************
 * Blob control structure  *
 ***************************)

  PISCBlobCtl = ^TISCBlobCtl;
  TISCStatusFn = function(Action: Word; Control: PIscBlobCtl): ISCStatus; cdecl;
  ISC_BLOB_CTL = record
    ctl_source: TISCStatusFn;          // Source filter
    ctl_source_handle: PISCBlobCtl;    // Argument to pass to source filter
    ctl_to_sub_type: Smallint;         // Target type
    ctl_from_sub_type: Smallint;       // Source type
    ctl_buffer_length: Word;           // Length of buffer
    ctl_segment_length: Word;          // Length of current segment
    ctl_bpb_length: Word;              // Length of blob parameter  block
    ctl_bpb: PAnsiChar;                // Address of blob parameter block
    ctl_buffer: PAnsiChar;             // Address of segment buffer
    ctl_max_segment: ISCLong;          // Length of longest segment
    ctl_number_segments: ISCLong;      // Total number of segments
    ctl_total_length: ISCLong;         // Total length of blob
    ctl_status: PISCStatus;            // Address of status vector
    ctl_data: array [0..7] of Longint; // Application specific data
  end;
  TISCBlobCtl = ISC_BLOB_CTL;

(***************************)
(* Blob stream definitions *)
(***************************)

  PBStream = ^TBStream;
  BSTREAM = record
    bstr_blob: PPointer;    // Blob handle
    bstr_buffer: PAnsiChar; // Address of buffer
    bstr_ptr: PAnsiChar;    // Next character
    bstr_length: Smallint;  // Length of buffer
    bstr_cnt: Smallint;     // Characters in buffer
    bstr_mode: AnsiChar;    // (mode) ? OUTPUT : INPUT
  end;
  TBStream = BSTREAM;

(********************************************************************
 * CVC: Public blob interface definition held in val.h.             *
 * For some unknown reason, it was only documented in langRef       *
 * and being the structure passed by the engine to UDFs it never    *
 * made its way into this public definitions file.                  *
 * Being its original name "blob", I renamed it blobcallback here.  *
 * I did the full definition with the proper parameters instead of  *
 * the weak C declaration with any number and type of parameters.   *
 * Since the first parameter -BLB- is unknown outside the engine,   *
 * it's more accurate to use void* than int* as the blob pointer    *
 ********************************************************************)

(* Blob passing structure *)

const
  (* This enum applies to parameter "mode" in blob_lseek *)
  blb_seek_relative = 1;
  blb_seek_from_tail = 2;

{FB20_UP}
  (* This enum applies to the value returned by blob_get_segment *)
  blb_got_fragment = -1;
  blb_got_eof = 0;
  blb_got_full_segment = 1;
{FB20_UP}

type

  TBlobGetSegmentFn = function(hnd: Pointer; buffer: PAnsiChar; buf_size: ISCUShort;
    var result_len: ISCUShort): Smallint; cdecl;

  TBlobPutSegmentFn = procedure(hnd: PPointer; buffer: PAnsiChar;
    buf_size: ISCUShort); cdecl;

  TBlobLSeekFn = function(hnd: PPointer; mode: ISCUShort; offset: ISCLong): ISCLong; cdecl;


  PBlobCallBack = ^TBlobCallBack;
  BLOBCALLBACK = record
    blob_get_segment: TBlobGetSegmentFn;
    blob_handle: PPointer;
    blob_number_segments: ISCLong;
    blob_max_segment: ISCLong;
    blob_total_length: ISCLong;
    blob_put_segment: TBlobPutSegmentFn;
    blob_lseek: TBlobLSeekFn;
  end;
  TBlobCallBack = BLOBCALLBACK;

(********************************************************************
 * CVC: Public descriptor interface held in dsc.h.                  *
 * We need it documented to be able to recognize NULL in UDFs.      *
 * Being its original name "dsc", I renamed it paramdsc here.       *
 * Notice that I adjust to the original definition: contrary to     *
 * other cases, the typedef is the same struct not the pointer.     *
 * I included the enumeration of dsc_dtype possible values.         *
 * Ultimately, dsc.h should be part of the public interface.        *
 ********************************************************************)

(* This is the famous internal descriptor that UDFs can use, too. *)

  PParamDsc = ^TParamDsc;
  PARAMDSC = record
    dsc_dtype: byte;
    dsc_scale: Shortint;
    dsc_length: ISCUShort;
    dsc_sub_type: Smallint;
    dsc_flags: ISCUShort;
    dsc_address: PAnsiChar;
  end;
  TParamDsc = PARAMDSC;

(* This is a helper struct to work with varchars. *)

  PParamVary = ^TParamVary;
  PARAMVARY = record
    vary_length: ISCUShort;
    vary_string: array [0..0] of AnsiChar;
  end;
  TParamVary = PARAMVARY;

(* values for dsc_flags *)
(* Note: DSC_null is only reliably set for local variables
   (blr_variable) *)
const
  DSC_null = 1;
  DSC_no_subtype = 2; (* dsc has no sub type specified *)
  DSC_nullable = 4; (* not stored. instead, is derived
                           from metadata primarily to flag
                           SQLDA (in DSQL) *)

(* Note that dtype_null actually means that we do not yet know the
   dtype for this descriptor.  A nice cleanup item would be to globally
   change it to dtype_unknown.  --chrisj 1999-02-17 *)
{FB20_UP}
  dtype_unknown = 0;
{FB20_UP}
{FB20_LOWER}
  dtype_null = 0;
{FB20_LOWER}
  dtype_text = 1;
  dtype_cstring = 2;
  dtype_varying = 3;

  dtype_packed = 6;
  dtype_byte = 7;
  dtype_short = 8;
  dtype_long = 9;
  dtype_quad = 10;
  dtype_real = 11;
  dtype_double = 12;
  dtype_d_float = 13;
  dtype_sql_date = 14;
  dtype_sql_time = 15;
  dtype_timestamp = 16;
  dtype_blob = 17;
  dtype_array = 18;
  dtype_int64 = 19;
{FB25_UP}
  dtype_dbkey = 20;
  DTYPE_TYPE_MAX_FB25 = 21;
{FB25_UP}
  DTYPE_TYPE_MAX_FB102 = 20;

(***************************
 * Dynamic SQL definitions *
 ***************************)

(******************************
 * Declare the extended SQLDA *
 ******************************)

type
  PXSQLVar = ^TXSQLVar;
  XSQLVAR = record
    sqltype: Smallint; // datatype of field
    sqlscale: Smallint; // scale factor
    sqlsubtype: Smallint; // datatype subtype - BLOBs & Text types only
    sqllen: Smallint; // length of data area
    sqldata: PAnsiChar; // address of data
    sqlind: PSmallint; // address of indicator variable
    sqlname_length: Smallint; // length of sqlname field
    sqlname: array [0..METADATALENGTH - 1] of AnsiChar; // name of field, name length + space for NULL
    relname_length: Smallint; // length of relation name
    relname: array [0..METADATALENGTH - 1] of AnsiChar; // field's relation name + space for NULL
    ownname_length: Smallint; // length of owner name
    ownname: array [0..METADATALENGTH - 1] of AnsiChar; // relation's owner name + space for  NULL
    aliasname_length: Smallint; // length of alias name
    aliasname: array [0..METADATALENGTH - 1] of AnsiChar; // relation's alias name + space for NULL
  end;

  TXSQLVar =  XSQLVAR;

  PXSQLDA = ^TXSQLDA;
  XSQLDA = record
    version: Smallint; // version of this XSQLDA
    sqldaid: array [0..7] of AnsiChar; // XSQLDA name field          ->  RESERVED
    sqldabc: ISCLong; // length in bytes of SQLDA   ->  RESERVED
    sqln: Smallint; // number of fields allocated
    sqld: Smallint; // actual number of fields
    sqlvar: array [0..0] of TXSQLVar; // first field address
  end;
  TXSQLDA = XSQLDA;

function XSQLDA_LENGTH(n: Integer): Integer;

const
  SQLDA_VERSION1 = 1;

  SQLDA_CURRENT_VERSION = SQLDA_VERSION1;
  {.$EXTERNALSYM SQLDA_CURRENT_VERSION}

  SQL_DIALECT_V5 = 1; (* meaning is same as DIALECT_xsqlda. *)
  SQL_DIALECT_V6_TRANSITION = 2; (* flagging anything that is delimited
                                    by double quotes as an error and
                                    flagging keyword DATE as an error *)
  SQL_DIALECT_V6 = 3; (* supports SQL delimited identifier,
                         SQLDATE/DATE, TIME, TIMESTAMP,
                         CURRENT_DATE, CURRENT_TIME,
                         CURRENT_TIMESTAMP, and 64-bit exact
                         numeric type *)
  SQL_DIALECT_CURRENT = SQL_DIALECT_V6; (* latest IB DIALECT *)

(********************************
 * InterBase Handle Definitions *
 ********************************)
type
  isc_att_handle = PPointer;
  IscAttHandle =  isc_att_handle;
  PIscAttHandle = ^IscAttHandle;

  isc_blob_handle = PPointer;
  IscBlobHandle =  isc_blob_handle;
  PIscBlobHandle = ^IscBlobHandle;

  isc_db_handle = PPointer;
  IscDbHandle =  isc_db_handle;
  PIscDbHandle = ^IscDbHandle;

  isc_form_handle = PPointer;
  IscFormHandle =  isc_form_handle;
  PIscFormHandle = ^IscFormHandle;

  isc_req_handle = PPointer;
  IscReqHandle =  isc_req_handle;
  PIscReqHandle = ^IscReqHandle;

  isc_stmt_handle = PPointer;
  IscStmtHandle =  isc_stmt_handle;
  PIscStmtHandle = ^IscStmtHandle;

  isc_svc_handle = PPointer;
  IscSvcHandle =  isc_svc_handle;
  PIscSvcHandle = ^IscSvcHandle;

  isc_tr_handle = PPointer;
  IscTrHandle =  isc_tr_handle;
  PIscTrHandle = ^IscTrHandle;

  isc_win_handle = PPointer;
  IscWinHandle =  isc_win_handle;
  PIscWinHandle = ^IscWinHandle;

//  isc_callback = procedure;
//  IscCallback = isc_callback;

  ISC_PRINT_CALLBACK = procedure(p: Pointer; v: ISC_SHORT; const c: PAnsiChar); cdecl;
  ISC_VERSION_CALLBACK = procedure(p: Pointer; const c: PAnsiChar); cdecl;
  ISC_EVENT_CALLBACK = procedure(user_data: Pointer; length: ISC_USHORT; const updated: PAnsiChar); cdecl;

  isc_resv_handle = ISC_LONG;
  IscResvHandle = isc_resv_handle;
  PIscResvHandle = ^IscResvHandle;

(***************************
 * OSRI database functions *
 ***************************)

type
  // Parameter for transaction on multiple Database, see
  PISCTEB = ^TISCTEB;
  TISCTEB = record
    Handle: PIscDbHandle;
    Len: Integer;
    Address: PAnsiChar;
  end;

(*************************************
 * Security Functions and structures *
 *************************************)
const
  sec_uid_spec = $01;
  sec_gid_spec = $02;
  sec_server_spec = $04;
  sec_password_spec = $08;
  sec_group_name_spec = $10;
  sec_first_name_spec = $20;
  sec_middle_name_spec = $40;
  sec_last_name_spec = $80;
  sec_dba_user_name_spec = $100;
  sec_dba_password_spec = $200;

  sec_protocol_tcpip = 1;
  sec_protocol_netbeui = 2;

  sec_protocol_local = 4;

type
  PUserSecData = ^TUserSecData;
  USER_SEC_DATA = record
    sec_flags: Smallint; // which fields are specified
    uid: Integer; // the user's id
    gid: Integer; // the user's group id
    protocol: Integer; // protocol to use for connection
    server: PAnsiChar; // server to administer
    user_name: PAnsiChar; // the user's name
    password: PAnsiChar; // the user's password
    group_name: PAnsiChar; // the group name
    first_name: PAnsiChar; // the user's first name
    middle_name: PAnsiChar; // the user's middle name
    last_name: PAnsiChar; // the user's last name
    dba_user_name: PAnsiChar; // the dba user name
    dba_password: PAnsiChar; // the dba password
  end;
  TUserSecData = USER_SEC_DATA;

(*****************************************
 * Service manager functions             *
 *****************************************)

procedure ADD_SPB_LENGTH(var p: PAnsiChar; length: Integer);
procedure ADD_SPB_NUMERIC(var p: PAnsiChar; data: Integer);

(***************************************************
 * Actions to pass to the blob filter (ctl_source) *
 ***************************************************)
const
  isc_blob_filter_open = 0;
  isc_blob_filter_get_segment = 1;
  isc_blob_filter_close = 2;
  isc_blob_filter_create = 3;
  isc_blob_filter_put_segment = 4;
  isc_blob_filter_alloc = 5;
  isc_blob_filter_free = 6;
  isc_blob_filter_seek = 7;

(*******************
 * Blr definitions *
 *******************)

  blr_text = 14;
  blr_text2 = 15; // added in 3.2 JPN
  blr_short = 7;
  blr_long = 8;
  blr_quad = 9;
  blr_float = 10;
  blr_double = 27;
  blr_d_float = 11;
  blr_timestamp = 35;
  blr_varying = 37;
  blr_varying2 = 38; // added in 3.2 JPN
  blr_blob = 261;
  blr_cstring = 40;
  blr_cstring2 = 41; // added in 3.2 JPN
  blr_blob_id = 45; // added from gds.h
  blr_sql_date = 12;
  blr_sql_time = 13;
  blr_int64 = 16;
{FB20_UP}
  blr_blob2 = 17;
{FB20_UP}

{FB21_UP}
  blr_domain_name = 18;
  blr_domain_name2 = 19;
  blr_not_nullable = 20;
{FB25_UP}
  blr_column_name  = 21;
  blr_column_name2 = 22;
{FB25_UP}

  blr_domain_type_of = 0;
  blr_domain_full = 1;
{FB21_UP}

  (* Historical alias for pre V6 applications *)
  blr_date = blr_timestamp;

  blr_inner = 0;
  blr_left = 1;
  blr_right = 2;
  blr_full = 3;

  blr_gds_code = 0;
  blr_sql_code = 1;
  blr_exception = 2;
  blr_trigger_code = 3;
  blr_default_code = 4;
  blr_raise = 5;
  blr_exception_msg = 6;

  blr_version4 = 4;
  blr_version5 = 5;
  blr_eoc = 76;
  blr_end = 255; // note: defined as -1 in gds.h

  blr_assignment = 1;
  blr_begin = 2;
  blr_dcl_variable = 3; // added from gds.h
  blr_message = 4;
  blr_erase = 5;
  blr_fetch = 6;
  blr_for = 7;
  blr_if = 8;
  blr_loop = 9;
  blr_modify = 10;
  blr_handler = 11;
  blr_receive = 12;
  blr_select = 13;
  blr_send = 14;
  blr_store = 15;
  blr_label = 17;
  blr_leave = 18;
  blr_store2 = 19;
  blr_post = 20;
  blr_literal = 21;
  blr_dbkey = 22;
  blr_field = 23;
  blr_fid = 24;
  blr_parameter = 25;
  blr_variable = 26;
  blr_average = 27;
  blr_count = 28;
  blr_maximum = 29;
  blr_minimum = 30;
  blr_total = 31;

  (* count 2
  define blr_count2  32
  *)
  blr_add = 34;
  blr_subtract = 35;
  blr_multiply = 36;
  blr_divide = 37;
  blr_negate = 38;
  blr_concatenate = 39;
  blr_substring = 40;
  blr_parameter2 = 41;
  blr_from = 42;
  blr_via = 43;
  blr_parameter2_old = 44; // Confusion
  blr_user_name = 44; // added from gds.h
  blr_null = 45;

{FB20_UP}
  blr_equiv	= 46;
{FB20_UP}
  blr_eql = 47;
  blr_neq = 48;
  blr_gtr = 49;
  blr_geq = 50;
  blr_lss = 51;
  blr_leq = 52;
  blr_containing = 53;
  blr_matching = 54;
  blr_starting = 55;
  blr_between = 56;
  blr_or = 57;
  blr_and = 58;
  blr_not = 59;
  blr_any = 60;
  blr_missing = 61;
  blr_unique = 62;
  blr_like = 63;
{FB20_LOWER}
  blr_stream = 65; // added from gds.h
  blr_set_index = 66; // added from gds.h
{FB20_LOWER}
  blr_rse = 67;
  blr_first = 68;
  blr_project = 69;
  blr_sort = 70;
  blr_boolean = 71;
  blr_ascending = 72;
  blr_descending = 73;
  blr_relation = 74;
  blr_rid = 75;
  blr_union = 76;
  blr_map = 77;
  blr_group_by = 78;
  blr_aggregate = 79;
  blr_join_type = 80;

  blr_agg_count = 83;
  blr_agg_max = 84;
  blr_agg_min = 85;
  blr_agg_total = 86;
  blr_agg_average = 87;
  blr_parameter3 = 88; // same as Rdb definition
  blr_run_max = 89;
  blr_run_min = 90;
  blr_run_total = 91;
  blr_run_average = 92;
  blr_agg_count2 = 93;
  blr_agg_count_distinct = 94;
  blr_agg_total_distinct = 95;
  blr_agg_average_distinct = 96;

  blr_function = 100;
  blr_gen_id = 101;
  blr_prot_mask = 102;
  blr_upcase = 103;
  blr_lock_state = 104;
  blr_value_if = 105;
  blr_matching2 = 106;
  blr_index = 107;
  blr_ansi_like = 108;
{FB20_LOWER}
  blr_bookmark = 109;
  blr_crack = 110;
  blr_force_crack = 111;
{FB20_LOWER}
  blr_seek = 112;
{FB20_LOWER}
  blr_find = 113;
{FB20_LOWER}

  (* these indicate directions for blr_seek and blr_find *)

  blr_continue = 0;
  blr_forward = 1;
  blr_backward = 2;
  blr_bof_forward = 3;
  blr_eof_backward = 4;
{FB20_LOWER}
  blr_lock_relation = 114;
  blr_lock_record = 115;
  blr_set_bookmark = 116;
  blr_get_bookmark = 117;
{FB20_LOWER}
  blr_run_count = 118; // changed from 88 to avoid conflict with blr_parameter3
  blr_rs_stream = 119;
  blr_exec_proc = 120;
{FB20_LOWER}
  blr_begin_range = 121;
  blr_end_range = 122;
  blr_delete_range = 123;
{FB20_LOWER}
  blr_procedure = 124;
  blr_pid = 125;
  blr_exec_pid = 126;
  blr_singular = 127;
  blr_abort = 128;
  blr_block = 129;
  blr_error_handler = 130;

  blr_cast = 131;
{FB20_LOWER}
  blr_release_lock = 132;
  blr_release_locks = 133;
{FB20_LOWER}
  blr_start_savepoint = 134;
  blr_end_savepoint = 135;
{FB20_LOWER}
  blr_find_dbkey = 136;
  blr_range_relation = 137;
  blr_delete_ranges = 138;
{FB20_LOWER}

  blr_plan = 139; // access plan items
  blr_merge = 140;
  blr_join = 141;
  blr_sequential = 142;
  blr_navigational = 143;
  blr_indices = 144;
  blr_retrieve = 145;

  blr_relation2 = 146;
  blr_rid2 = 147;
{FB20_LOWER}
  blr_reset_stream = 148;
  blr_release_bookmark = 149;
{FB20_LOWER}

  blr_set_generator = 150;

  blr_ansi_any = 151; // required for NULL handling
  blr_exists = 152; // required for NULL handling
{FB20_LOWER}
  blr_cardinality = 153;
{FB20_LOWER}

  blr_record_version = 154; // get tid of record
  blr_stall = 155; // fake server stall

{FB20_LOWER}
  blr_seek_no_warn = 156;
  blr_find_dbkey_version = 157; // find dbkey with record version
{FB20_LOWER}
  blr_ansi_all = 158; // required for NULL handling

  blr_extract = 159;

  (* sub parameters for blr_extract *)

  blr_extract_year = 0;
  blr_extract_month = 1;
  blr_extract_day = 2;
  blr_extract_hour = 3;
  blr_extract_minute = 4;
  blr_extract_second = 5;
  blr_extract_weekday = 6;
  blr_extract_yearday = 7;
{FB21_UP}
  blr_extract_millisecond = 8;
  blr_extract_week = 9;
{FB21_UP}


  blr_current_date = 160;
  blr_current_timestamp = 161;
  blr_current_time = 162;

{FB15_UP}
  (* This codes reuse BLR code space *)
  blr_post_arg = 163;
  blr_exec_into	= 164;
  blr_user_savepoint = 165;
{FB15_UP}

{FB20_UP}
  blr_dcl_cursor = 166;
  blr_cursor_stmt	=	167;
  blr_current_timestamp2 = 168;
  blr_current_time2 = 169;
{FB20_UP}

{FB21_UP}
  blr_agg_list = 170;
  blr_agg_list_distinct = 171;
  blr_modify2 = 172;
{FB21_UP}

  (* FB1 specific BLR *)
  blr_current_role = 174;
  blr_skip = 175;

{FB15_UP}
  (* FB 1.5 specific BLR *)
  blr_exec_sql = 176;
  blr_internal_info = 177;
  blr_nullsfirst = 178;
  blr_writelock = 179;
  blr_nullslast = 180;
{FB15_UP}

{FB20_UP}
  blr_lowcase = 181;
  blr_strlen = 182;

  blr_strlen_bit = 0;
  blr_strlen_char = 1;
  blr_strlen_octet = 2;

  blr_trim = 183;

  blr_trim_both = 0;
  blr_trim_leading = 1;
  blr_trim_trailing = 2;

  blr_trim_spaces = 0;
  blr_trim_characters = 1;
{FB20_UP}

  (* These codes are actions for user-defined savepoints *)
{FB15_UP}
  blr_savepoint_set = 0;
  blr_savepoint_release = 1;
  blr_savepoint_undo = 2;
  blr_savepoint_release_single = 3;
{FB15_UP}

{FB20_UP}
  blr_cursor_open	 = 0;
  blr_cursor_close = 1;
  blr_cursor_fetch = 2;
{FB20_UP}

{FB21_UP}
  blr_init_variable = 184;
  blr_recurse = 185;
  blr_sys_function = 186;
{FB21_UP}

{FB25_UP}
  blr_auto_trans   = 187;
  blr_similar      = 188;
  blr_exec_stmt    = 189;

  blr_exec_stmt_inputs      = 1;
  blr_exec_stmt_outputs     = 2;
  blr_exec_stmt_sql         = 3;
  blr_exec_stmt_proc_block  = 4;
  blr_exec_stmt_data_src    = 5;
  blr_exec_stmt_user        = 6;
  blr_exec_stmt_pwd         = 7;
  blr_exec_stmt_tran        = 8;
  blr_exec_stmt_tran_clone  = 9;
  blr_exec_stmt_privs       = 10;
  blr_exec_stmt_in_params   = 11;
  blr_exec_stmt_in_params2  = 12;
  blr_exec_stmt_out_params  = 13;
  blr_exec_stmt_role        = 14;

  blr_stmt_expr             = 190;
  blr_derived_expr          = 191;
{FB25_UP}

(**********************************
 * Database parameter block stuff *
 **********************************)

  isc_dpb_version1 = 1;
  isc_dpb_cdd_pathname = 1;
  isc_dpb_allocation = 2;
  isc_dpb_journal = 3;
  isc_dpb_page_size = 4;
  isc_dpb_num_buffers = 5;
  isc_dpb_buffer_length = 6;
  isc_dpb_debug = 7;
  isc_dpb_garbage_collect = 8;
  isc_dpb_verify = 9;
  isc_dpb_sweep = 10;
  isc_dpb_enable_journal = 11;
  isc_dpb_disable_journal = 12;
  isc_dpb_dbkey_scope = 13;
  isc_dpb_number_of_users = 14;
  isc_dpb_trace = 15;
  isc_dpb_no_garbage_collect = 16;
  isc_dpb_damaged = 17;
  isc_dpb_license = 18;
  isc_dpb_sys_user_name = 19;
  isc_dpb_encrypt_key = 20;
  isc_dpb_activate_shadow = 21;
  isc_dpb_sweep_interval = 22;
  isc_dpb_delete_shadow = 23;
  isc_dpb_force_write = 24;
  isc_dpb_begin_log = 25;
  isc_dpb_quit_log = 26;
  isc_dpb_no_reserve = 27;
  isc_dpb_user_name = 28;
  isc_dpb_password = 29;
  isc_dpb_password_enc = 30;
  isc_dpb_sys_user_name_enc = 31;
  isc_dpb_interp = 32;
  isc_dpb_online_dump = 33;
  isc_dpb_old_file_size = 34;
  isc_dpb_old_num_files = 35;
  isc_dpb_old_file = 36;
  isc_dpb_old_start_page = 37;
  isc_dpb_old_start_seqno = 38;
  isc_dpb_old_start_file = 39;
  isc_dpb_drop_walfile = 40;
  isc_dpb_old_dump_id = 41;
  isc_dpb_wal_backup_dir = 42;
  isc_dpb_wal_chkptlen = 43;
  isc_dpb_wal_numbufs = 44;
  isc_dpb_wal_bufsize = 45;
  isc_dpb_wal_grp_cmt_wait = 46;
  isc_dpb_lc_messages = 47;
  isc_dpb_lc_ctype = 48;
  isc_dpb_cache_manager = 49;
  isc_dpb_shutdown = 50;
  isc_dpb_online = 51;
  isc_dpb_shutdown_delay = 52;
  isc_dpb_reserved = 53;
  isc_dpb_overwrite = 54;
  isc_dpb_sec_attach = 55;
  isc_dpb_disable_wal = 56;
  isc_dpb_connect_timeout = 57;
  isc_dpb_dummy_packet_interval = 58;
  isc_dpb_gbak_attach = 59;
  isc_dpb_sql_role_name = 60;
  isc_dpb_set_page_buffers = 61;
  isc_dpb_working_directory = 62;
  isc_dpb_sql_dialect = 63;
  isc_dpb_set_db_readonly = 64;
  isc_dpb_set_db_sql_dialect = 65;
  isc_dpb_gfix_attach = 66;
  isc_dpb_gstat_attach = 67;

{FB25_UP}
  fb_shut_confirmation        =  1;
  fb_shut_preproviders        =  2;
  fb_shut_postproviders       =  4;
  fb_shut_finish              =  8;

  fb_shutrsn_svc_stopped      = -1;
  fb_shutrsn_no_connection    = -2;
  fb_shutrsn_app_stopped      = -3;
  fb_shutrsn_device_removed   = -4;
  fb_shutrsn_signal           = -5;
  fb_shutrsn_services         = -6;
  fb_shutrsn_exit_called      = -7;

  fb_cancel_disable           =  1;
  fb_cancel_enable            =  2;
  fb_cancel_raise             =  3;
  fb_cancel_abort             =  4;
{FB25_UP}

{FB103_UP}
  isc_dpb_set_db_charset = 68;
{FB103_UP}

{FB20_UP}
  isc_dpb_gsec_attach = 69;
  isc_dpb_address_path = 70;
{FB20_UP}

{FB21_UP}
  isc_dpb_process_id = 71;
  isc_dpb_no_db_triggers = 72;
  isc_dpb_trusted_auth = 73;
  isc_dpb_process_name = 74;
{FB21_UP}

{FB25_UP}
  isc_dpb_trusted_role = 75;
  isc_dpb_org_filename = 76;
  isc_dpb_utf8_filename = 77;
  isc_dpb_ext_call_depth = 78;
{FB25_UP}

{FB25}
  isc_dpb_Max_Value = 78;
{FB25}

{FB20_UP}
(**************************************************)
(* clumplet tags used inside isc_dpb_address_path *)
(**************************************************)

(* Format of this clumplet is the following:

 <address-path-clumplet> ::=
	isc_dpb_address_path <byte-clumplet-length> <address-stack>

 <address-stack> ::=
	<address-descriptor> |
	<address-stack> <address-descriptor>

 <address-descriptor> ::=
	isc_dpb_address <byte-clumplet-length> <address-elements>

 <address-elements> ::=
	<address-element> |
	<address-elements> <address-element>

 <address-element> ::=
	isc_dpb_addr_protocol <byte-clumplet-length> <protocol-string> |
	isc_dpb_addr_endpoint <byte-clumplet-length> <remote-endpoint-string>

 <protocol-string> ::=
	"TCPv4" |
	"TCPv6" |
	"XNET" |
	"WNET" |
	....

 <remote-endpoint-string> ::=
	<IPv4-address> | // such as "172.20.1.1"
	<IPv6-address> | // such as "2001:0:13FF:09FF::1"
	<xnet-process-id> | // such as "17864"
	...
*)
const

  isc_dpb_address = 1;

  isc_dpb_addr_protocol = 1;
  isc_dpb_addr_endpoint = 2;
{FB20_UP}

  (*********************************
   * isc_dpb_verify specific flags *
   *********************************)
const
  isc_dpb_pages = 1;
  isc_dpb_records = 2;
  isc_dpb_indices = 4;
  isc_dpb_transactions = 8;
  isc_dpb_no_update = 16;
  isc_dpb_repair = 32;
  isc_dpb_ignore = 64;

  (***********************************
   * isc_dpb_shutdown specific flags *
   ***********************************)

  isc_dpb_shut_cache        = $1;
  isc_dpb_shut_attachment   = $2;
  isc_dpb_shut_transaction  = $4;
  isc_dpb_shut_force        = $8;
{FB20_UP}
  isc_dpb_shut_mode_mask    = $70;

  isc_dpb_shut_default      = $00;
  isc_dpb_shut_normal       = $10;
  isc_dpb_shut_multi        = $20;
  isc_dpb_shut_single       = $30;
  isc_dpb_shut_full         = $40;
{FB20_UP}

  (**************************************
   * Bit assignments in RDB$SYSTEM_FLAG *
   **************************************)

  RDB_system = 1;
  RDB_id_assigned = 2;

  (*************************************
   * Transaction parameter block stuff *
   *************************************)
  isc_tpb_version1 = #1;
  isc_tpb_version3 = #3;
  isc_tpb_consistency = #1;
  isc_tpb_concurrency = #2;
{FB_21_LOWER}
  isc_tpb_shared = #3;
  isc_tpb_protected = #4;
  isc_tpb_exclusive = #5;
{FB_21_LOWER}
  isc_tpb_wait = #6;
  isc_tpb_nowait = #7;
  isc_tpb_read = #8;
  isc_tpb_write = #9;
  isc_tpb_lock_read = #10;
  isc_tpb_lock_write = #11;
  isc_tpb_verb_time = #12;
  isc_tpb_commit_time = #13;
  isc_tpb_ignore_limbo = #14;
  isc_tpb_read_committed = #15;
  isc_tpb_autocommit = #16;
  isc_tpb_rec_version = #17;
  isc_tpb_no_rec_version = #18;
  isc_tpb_restart_requests = #19;
  isc_tpb_no_auto_undo = #20;
{FB20_UP}
  isc_tpb_lock_timeout = #21;
{FB20_UP}

  (************************
   * Blob Parameter Block *
   ************************)

  isc_bpb_Max_Value = 6;

  isc_bpb_version1 = #1;
  isc_bpb_source_type = #1;
  isc_bpb_target_type = #2;
  isc_bpb_type = #3;
  isc_bpb_source_interp = #4;
  isc_bpb_target_interp = #5;
  isc_bpb_filter_parameter = #6;
{FB21_UP}
  isc_bpb_storage = #7;
{FB21_UP}

  isc_bpb_type_segmented = #0;
  isc_bpb_type_stream = #1;

{FB21_UP}
  isc_bpb_storage_main = #0;
  isc_bpb_storage_temp = #2;
{FB21_UP}

  (*********************************
   * Service parameter block stuff *
   *********************************)

  isc_spb_version1 = AnsiChar(#1);
  isc_spb_current_version = AnsiChar(#2);
  isc_spb_version = isc_spb_current_version;
  isc_spb_user_name = AnsiChar(isc_dpb_user_name);
  isc_spb_sys_user_name = AnsiChar(isc_dpb_sys_user_name);
  isc_spb_sys_user_name_enc = AnsiChar(isc_dpb_sys_user_name_enc);
  isc_spb_password = AnsiChar(isc_dpb_password);
  isc_spb_password_enc = AnsiChar(isc_dpb_password_enc);
  isc_spb_command_line = AnsiChar(#105);
  isc_spb_dbname = AnsiChar(#106);
  isc_spb_verbose = AnsiChar(#107);
  isc_spb_options = AnsiChar(#108);
{FB20_UP}
  isc_spb_address_path = AnsiChar(#109);
{FB20_UP}

{FB21_UP}
  isc_spb_process_id = AnsiChar(#110);
  isc_spb_trusted_auth = AnsiChar(#111);
  isc_spb_process_name = AnsiChar(#112);
{FB21_UP}

{FB25_UP}
   isc_spb_trusted_role = AnsiChar(#113);
{FB25_UP}

  isc_spb_connect_timeout = AnsiChar(isc_dpb_connect_timeout);
  isc_spb_dummy_packet_interval = AnsiChar(isc_dpb_dummy_packet_interval);
  isc_spb_sql_role_name = AnsiChar(isc_dpb_sql_role_name);

  (*********************************
   * Information call declarations *
   *********************************)

  (****************************
   * Common, structural codes *
   ****************************)

  isc_info_end = 1;
  isc_info_truncated = 2;
  isc_info_error = 3;
  isc_info_data_not_ready = 4;
{FB21_UP}
  isc_info_length = 126;
{FB21_UP}
  isc_info_flag_end = 127;

  (******************************
   * Database information items *
   ******************************)

  isc_info_db_id = 4;
  isc_info_reads = 5;
  isc_info_writes = 6;
  isc_info_fetches = 7;
  isc_info_marks = 8;

  isc_info_implementation = 11;
  isc_info_isc_version = 12;
  isc_info_base_level = 13;

  isc_info_page_size = 14;
  isc_info_num_buffers = 15;
  isc_info_limbo = 16;
  isc_info_current_memory = 17;
  isc_info_max_memory = 18;
  isc_info_window_turns = 19;
  isc_info_license = 20;

  isc_info_allocation = 21;
  isc_info_attachment_id = 22;
  isc_info_read_seq_count = 23;
  isc_info_read_idx_count = 24;
  isc_info_insert_count = 25;
  isc_info_update_count = 26;
  isc_info_delete_count = 27;
  isc_info_backout_count = 28;
  isc_info_purge_count = 29;
  isc_info_expunge_count = 30;

  isc_info_sweep_interval = 31;
  isc_info_ods_version = 32;
  isc_info_ods_minor_version = 33;
  isc_info_no_reserve = 34;
  isc_info_logfile = 35;
  isc_info_cur_logfile_name = 36;
  isc_info_cur_log_part_offset = 37;
  isc_info_num_wal_buffers = 38;
  isc_info_wal_buffer_size = 39;
  isc_info_wal_ckpt_length = 40;

  isc_info_wal_cur_ckpt_interval = 41;
  isc_info_wal_prv_ckpt_fname = 42;
  isc_info_wal_prv_ckpt_poffset = 43;
  isc_info_wal_recv_ckpt_fname = 44;
  isc_info_wal_recv_ckpt_poffset = 45;
  isc_info_wal_grpc_wait_usecs = 47;
  isc_info_wal_num_io = 48;
  isc_info_wal_avg_io_size = 49;
  isc_info_wal_num_commits = 50;

  isc_info_wal_avg_grpc_size = 51;
  isc_info_forced_writes = 52;
  isc_info_user_names = 53;
  isc_info_page_errors = 54;
  isc_info_record_errors = 55;
  isc_info_bpage_errors = 56;
  isc_info_dpage_errors = 57;
  isc_info_ipage_errors = 58;
  isc_info_ppage_errors = 59;
  isc_info_tpage_errors = 60;

  isc_info_set_page_buffers = 61;
  isc_info_db_sql_dialect = 62;
  isc_info_db_read_only = 63;
  isc_info_db_size_in_pages = 64;

  frb_info_att_charset = 101;
  isc_info_db_class = 102;
  isc_info_firebird_version = 103;
  isc_info_oldest_transaction = 104;
  isc_info_oldest_active = 105;
  isc_info_oldest_snapshot = 106;
  isc_info_next_transaction = 107;
  isc_info_db_provider = 108;
  isc_info_active_transactions = 109;
{FB20_UP}
  isc_info_active_tran_count = 110;
  isc_info_creation_date = 111;
{FB20_UP}
{FB21_UP}
  isc_info_db_file_size = 112;
{FB21_UP}
{FB25_UP}
  fb_info_page_contents = 113;
{FB25_UP}

  isc_info_version = isc_info_isc_version;

  (**************************************
   * Database information return values *
   **************************************)

  isc_info_db_impl_rdb_vms = 1;
  isc_info_db_impl_rdb_eln = 2;
  isc_info_db_impl_rdb_eln_dev = 3;
  isc_info_db_impl_rdb_vms_y = 4;
  isc_info_db_impl_rdb_eln_y = 5;
  isc_info_db_impl_jri = 6;
  isc_info_db_impl_jsv = 7;

  isc_info_db_impl_isc_apl_68K = 25;
  isc_info_db_impl_isc_vax_ultr = 26;
  isc_info_db_impl_isc_vms = 27;
  isc_info_db_impl_isc_sun_68k = 28;
  isc_info_db_impl_isc_os2 = 29;
  isc_info_db_impl_isc_sun4 = 30;

  isc_info_db_impl_isc_hp_ux = 31;
  isc_info_db_impl_isc_sun_386i = 32;
  isc_info_db_impl_isc_vms_orcl = 33;
  isc_info_db_impl_isc_mac_aux = 34;
  isc_info_db_impl_isc_rt_aix = 35;
  isc_info_db_impl_isc_mips_ult = 36;
  isc_info_db_impl_isc_xenix = 37;
  isc_info_db_impl_isc_dg = 38;
  isc_info_db_impl_isc_hp_mpexl = 39;
  isc_info_db_impl_isc_hp_ux68K = 40;

  isc_info_db_impl_isc_sgi = 41;
  isc_info_db_impl_isc_sco_unix = 42;
  isc_info_db_impl_isc_cray = 43;
  isc_info_db_impl_isc_imp = 44;
  isc_info_db_impl_isc_delta = 45;
  isc_info_db_impl_isc_next = 46;
  isc_info_db_impl_isc_dos = 47;

  isc_info_db_impl_m88K = 48;
  isc_info_db_impl_unixware = 49;
  isc_info_db_impl_isc_winnt_x86 = 50;

  isc_info_db_impl_isc_epson = 51;
  isc_info_db_impl_alpha_osf = 52;
  isc_info_db_impl_alpha_vms = 53;
  isc_info_db_impl_netware_386 = 54;
  isc_info_db_impl_win_only = 55;
  isc_info_db_impl_ncr_3000 = 56;
  isc_info_db_impl_winnt_ppc = 57;
  isc_info_db_impl_dg_x86 = 58;
  isc_info_db_impl_sco_ev = 59;
  isc_info_db_impl_i386 = 60;

  isc_info_db_impl_freebsd = 61;
  isc_info_db_impl_netbsd = 62;
  isc_info_db_impl_darwin_ppc = 63;

  isc_info_db_impl_sinixz = 64;

{FB15_UP}
  isc_info_db_impl_linux_sparc = 65;
  isc_info_db_impl_linux_amd64 = 66; // FB151
{FB15_UP}
{FB20_UP}
  isc_info_db_impl_freebsd_amd64 = 67;
  isc_info_db_impl_winnt_amd64 = 68;
  isc_info_db_impl_linux_ppc = 69;
  isc_info_db_impl_darwin_x86 = 70;
{FB20_UP}
{FB21_UP}
  isc_info_db_impl_linux_mipsel = 71;
  isc_info_db_impl_linux_mips = 72;
  isc_info_db_impl_darwin_x64 = 73;
{FB21_UP}
{FB25_UP}
  isc_info_db_impl_sun_amd64 = 74;

  isc_info_db_impl_linux_arm = 75;
  isc_info_db_impl_linux_ia64 = 76;

  isc_info_db_impl_darwin_ppc64 = 77;
  isc_info_db_impl_linux_s390x = 78;
  isc_info_db_impl_linux_s390 = 79;

  isc_info_db_impl_linux_sh = 80;
  isc_info_db_impl_linux_sheb = 81;
{FB25_UP}

  isc_info_db_impl_isc_a = isc_info_db_impl_isc_apl_68K;
  isc_info_db_impl_isc_u = isc_info_db_impl_isc_vax_ultr;
  isc_info_db_impl_isc_v = isc_info_db_impl_isc_vms;
  isc_info_db_impl_isc_s = isc_info_db_impl_isc_sun_68k;

type
  info_db_class = (
    isc_info_db_class_INVALID_0,
    isc_info_db_class_access,
    isc_info_db_class_y_valve,
    isc_info_db_class_rem_int,
    isc_info_db_class_rem_srvr,
    isc_info_db_class_INVALID_5,
    isc_info_db_class_INVALID_6,
    isc_info_db_class_pipe_int,
    isc_info_db_class_pipe_srvr,
    isc_info_db_class_sam_int,
    isc_info_db_class_sam_srvr,
    isc_info_db_class_gateway,
    isc_info_db_class_cache,
    isc_info_db_class_classic_access,
    isc_info_db_class_server_access,
    isc_info_db_class_last_value (* Leave this LAST! *)
    );

  info_db_provider = (
    isc_info_db_code_INVALID_0,
    isc_info_db_code_rdb_eln,
    isc_info_db_code_rdb_vms,
    isc_info_db_code_interbase,
    isc_info_db_code_firebird,
    isc_info_db_code_last_value (* Leave this LAST! *)
    );

(*****************************
 * Request information items *
 *****************************)
const
  isc_info_number_messages = 4;
  isc_info_max_message = 5;
  isc_info_max_send = 6;
  isc_info_max_receive = 7;
  isc_info_state = 8;
  isc_info_message_number = 9;
  isc_info_message_size = 10;
  isc_info_request_cost = 11;
  isc_info_access_path = 12;
  isc_info_req_select_count = 13;
  isc_info_req_insert_count = 14;
  isc_info_req_update_count = 15;
  isc_info_req_delete_count = 16;

  (*********************
   * Access path items *
   *********************)

  isc_info_rsb_end = 0;
  isc_info_rsb_begin = 1;
  isc_info_rsb_type = 2;
  isc_info_rsb_relation = 3;
  isc_info_rsb_plan = 4;

  (*************
   * Rsb types *
   *************)

  isc_info_rsb_unknown = 1;
  isc_info_rsb_indexed = 2;
  isc_info_rsb_navigate = 3;
  isc_info_rsb_sequential = 4;
  isc_info_rsb_cross = 5;
  isc_info_rsb_sort = 6;
  isc_info_rsb_first = 7;
  isc_info_rsb_boolean = 8;
  isc_info_rsb_union = 9;
  isc_info_rsb_aggregate = 10;
  isc_info_rsb_merge = 11;
  isc_info_rsb_ext_sequential = 12;
  isc_info_rsb_ext_indexed = 13;
  isc_info_rsb_ext_dbkey = 14;
  isc_info_rsb_left_cross = 15;
  isc_info_rsb_select = 16;
  isc_info_rsb_sql_join = 17;
  isc_info_rsb_simulate = 18;
  isc_info_rsb_sim_cross = 19;
  isc_info_rsb_once = 20;
  isc_info_rsb_procedure = 21;
{FB20_UP}
  isc_info_rsb_skip = 22;
{FB20_UP}
{FB21_UP}
  isc_info_rsb_virt_sequential = 23;
  isc_info_rsb_recursive = 24;
{FB21_UP}

  (**********************
   * Bitmap expressions *
   **********************)

  isc_info_rsb_and = 1;
  isc_info_rsb_or = 2;
  isc_info_rsb_dbkey = 3;
  isc_info_rsb_index = 4;

  isc_info_req_active = 2;
  isc_info_req_inactive = 3;
  isc_info_req_send = 4;
  isc_info_req_receive = 5;
  isc_info_req_select = 6;
  isc_info_req_sql_stall = 7;

  (**************************
   * Blob information items *
   **************************)

  isc_info_blob_num_segments = #4;
  isc_info_blob_max_segment = #5;
  isc_info_blob_total_length = #6;
  isc_info_blob_type = #7;

  (*********************************
   * Transaction information items *
   *********************************)

  isc_info_tra_id = 4;
{FB20_UP}
  isc_info_tra_oldest_interesting = 5;
  isc_info_tra_oldest_snapshot    = 6;
  isc_info_tra_oldest_active      = 7;
  isc_info_tra_isolation          = 8;
  isc_info_tra_access             = 9;
  isc_info_tra_lock_timeout       = 10;

  isc_info_tra_consistency        = 1;
  isc_info_tra_concurrency        = 2;
  isc_info_tra_read_committed     = 3;

  isc_info_tra_no_rec_version     = 0;
  isc_info_tra_rec_version        = 1;

  isc_info_tra_readonly           = 0;
  isc_info_tra_readwrite          = 1;
{FB20_UP}
  (*****************************
   * Service action items      *
   *****************************)

  isc_action_svc_backup = #1; // Starts database backup process on the server
  isc_action_svc_restore = #2; // Starts database restore process on the server
  isc_action_svc_repair = #3; // Starts database repair process on the server
  isc_action_svc_add_user = #4; // Adds a new user to the security database
  isc_action_svc_delete_user = #5; // Deletes a user record from the security database
  isc_action_svc_modify_user = #6; // Modifies a user record in the security database
  isc_action_svc_display_user = #7; // Displays a user record from the security database
  isc_action_svc_properties = #8; // Sets database properties
  isc_action_svc_add_license = #9; // Adds a license to the license file
  isc_action_svc_remove_license = #10; // Removes a license from the license file
  isc_action_svc_db_stats = #11; // Retrieves database statistics
  isc_action_svc_get_ib_log = #12; // Retrieves the InterBase log file from the server
{FB20_UP}
  isc_action_svc_get_fb_log = #12;	// Retrieves the Firebird log file from the server
{FB20_UP}
{FB25_UP}
  isc_action_svc_nbak             = #20;
  isc_action_svc_nrest            = #21;
  isc_action_svc_trace_start      = #22;
  isc_action_svc_trace_stop       = #23;
  isc_action_svc_trace_suspend    = #24;
  isc_action_svc_trace_resume     = #25;
  isc_action_svc_trace_list       = #26;
  isc_action_svc_set_mapping      = #27;
  isc_action_svc_drop_mapping     = #28;
  isc_action_svc_display_user_adm = #29;
  isc_action_svc_last             = #30;
{FB25_UP}

  (*****************************
   * Service information items *
   *****************************)

  // Retrieves the number of attachments and databases
  isc_info_svc_svr_db_info = #50;
  // Retrieves all license keys and IDs from the license file
  isc_info_svc_get_license = #51;
  // Retrieves a bitmask representing licensed options on the server
  isc_info_svc_get_license_mask = #52;
  // Retrieves the parameters and values for IB_CONFIG
  isc_info_svc_get_config = #53;
  // Retrieves the version of the services manager
  isc_info_svc_version = #54;
  // Retrieves the version of the InterBase server
  isc_info_svc_server_version = #55;
  // Retrieves the implementation of the InterBase server
  isc_info_svc_implementation = #56;
  // Retrieves a bitmask representing the server's capabilities
  isc_info_svc_capabilities = #57;
  // Retrieves the path to the security database in use by the server
  isc_info_svc_user_dbpath = #58;
  // Retrieves the setting of $INTERBASE
  isc_info_svc_get_env = #59;
  // Retrieves the setting of $INTERBASE_LCK
  isc_info_svc_get_env_lock = #60;
  // Retrieves the setting of $INTERBASE_MSG
  isc_info_svc_get_env_msg = #61;
  // Retrieves 1 line of service output per call
  isc_info_svc_line = #62;
  // Retrieves as much of the server output as will fit in the supplied buffer
  isc_info_svc_to_eof = #63;
  // Sets / signifies a timeout value for reading service information
  isc_info_svc_timeout = #64;
  // Retrieves the number of users licensed for accessing the server
  isc_info_svc_get_licensed_users = #65;
  // Retrieve the limbo transactions
  isc_info_svc_limbo_trans = #66;
  // Checks to see if a service is running on an attachment
  isc_info_svc_running = #67;
  // Returns the user information from isc_action_svc_display_users
  isc_info_svc_get_users = #68;

  (******************************************************
   * Parameters for isc_action_{add|delete|modify)_user *
   ******************************************************)

  isc_spb_sec_userid = #5;
  isc_spb_sec_groupid = #6;
  isc_spb_sec_username = #7;
  isc_spb_sec_password = #8;
  isc_spb_sec_groupname = #9;
  isc_spb_sec_firstname = #10;
  isc_spb_sec_middlename = #11;
  isc_spb_sec_lastname = #12;
{FB25_UP}
  isc_spb_sec_admin = #13;
{FB25_UP}

  (*******************************************************
   * Parameters for isc_action_svc_(add|remove)_license, *
   * isc_info_svc_get_license                            *
   *******************************************************)

  isc_spb_lic_key = #5;
  isc_spb_lic_id = #6;
  isc_spb_lic_desc = #7;

  (*****************************************
   * Parameters for isc_action_svc_backup  *
   *****************************************)

  isc_spb_bkp_file = #5;
  isc_spb_bkp_factor = #6;
  isc_spb_bkp_length = #7;

  //flags
  isc_spb_bkp_ignore_checksums = $01;
  isc_spb_bkp_ignore_limbo = $02;
  isc_spb_bkp_metadata_only = $04;
  isc_spb_bkp_no_garbage_collect = $08;
  isc_spb_bkp_old_descriptions = $10;
  isc_spb_bkp_non_transportable = $20;
  isc_spb_bkp_convert = $40;
  isc_spb_bkp_expand = $80;
{FB25_UP}
  isc_spb_bkp_no_triggers = $8000;
{FB25_UP}

  (********************************************
   * Parameters for isc_action_svc_properties *
   ********************************************)

  isc_spb_prp_page_buffers = #5;
  isc_spb_prp_sweep_interval = #6;
  isc_spb_prp_shutdown_db = #7;
  isc_spb_prp_deny_new_attachments = #9;
  isc_spb_prp_deny_new_transactions = #10;
  isc_spb_prp_reserve_space = #11;
  isc_spb_prp_write_mode = #12;
  isc_spb_prp_access_mode = #13;
  isc_spb_prp_set_sql_dialect = #14;
  isc_spb_prp_activate = $0100;
  isc_spb_prp_db_online = $0200;
{FB25_UP}
  isc_spb_prp_force_shutdown              = #41;
  isc_spb_prp_attachments_shutdown        = #42;
  isc_spb_prp_transactions_shutdown       = #43;
  isc_spb_prp_shutdown_mode               = #44;
  isc_spb_prp_online_mode                 = #45;

  isc_spb_prp_sm_normal          = 0;
  isc_spb_prp_sm_multi           = 1;
  isc_spb_prp_sm_single          = 2;
  isc_spb_prp_sm_full            = 3;
{FB25_UP}

  (********************************************
   * Parameters for isc_spb_prp_reserve_space *
   ********************************************)

  isc_spb_prp_res_use_full = #35;
  isc_spb_prp_res = #36;

  (******************************************
   * Parameters for isc_spb_prp_write_mode  *
   ******************************************)

  isc_spb_prp_wm_async = #37;
  isc_spb_prp_wm_sync = #38;

  (******************************************
   * Parameters for isc_spb_prp_access_mode *
   ******************************************)

  isc_spb_prp_am_readonly = #39;
  isc_spb_prp_am_readwrite = #40;

  (*****************************************
   * Parameters for isc_action_svc_repair  *
   *****************************************)

  isc_spb_rpr_commit_trans = 15;
  isc_spb_rpr_rollback_trans = 34;
  isc_spb_rpr_recover_two_phase = 17;
  isc_spb_tra_id = 18;
  isc_spb_single_tra_id = 19;
  isc_spb_multi_tra_id = 20;
  isc_spb_tra_state = 21;
  isc_spb_tra_state_limbo = 22;
  isc_spb_tra_state_commit = 23;
  isc_spb_tra_state_rollback = 24;
  isc_spb_tra_state_unknown = 25;
  isc_spb_tra_host_site = 26;
  isc_spb_tra_remote_site = 27;
  isc_spb_tra_db_path = 28;
  isc_spb_tra_advise = 29;
  isc_spb_tra_advise_commit = 30;
  isc_spb_tra_advise_rollback = 31;
  isc_spb_tra_advise_unknown = 33;

  isc_spb_rpr_validate_db = $01;
  isc_spb_rpr_sweep_db = $02;
  isc_spb_rpr_mend_db = $04;
  isc_spb_rpr_list_limbo_trans = $08;
  isc_spb_rpr_check_db = $10;
  isc_spb_rpr_ignore_checksum = $20;
  isc_spb_rpr_kill_shadows = $40;
  isc_spb_rpr_full = $80;

  (*****************************************
   * Parameters for isc_action_svc_restore *
   *****************************************)

  isc_spb_res_buffers = #9;
  isc_spb_res_page_size = #10;
  isc_spb_res_length = #11;
  isc_spb_res_access_mode = #12;
{FB25_UP}
  isc_spb_res_fix_fss_data = #13;
  isc_spb_res_fix_fss_metadata = #14;
{FB25_UP}
  isc_spb_res_deactivate_idx = $0100;
  isc_spb_res_no_shadow = $0200;
  isc_spb_res_no_validity = $0400;
  isc_spb_res_one_at_a_time = $0800;
  isc_spb_res_replace = $1000;
  isc_spb_res_create = $2000;
  isc_spb_res_use_all_space = $4000;

  (******************************************
   * Parameters for isc_spb_res_access_mode *
   ******************************************)

  isc_spb_res_am_readonly = isc_spb_prp_am_readonly;
  isc_spb_res_am_readwrite = isc_spb_prp_am_readwrite;

  (*******************************************
   * Parameters for isc_info_svc_svr_db_info *
   *******************************************)

  isc_spb_num_att = 5;
  isc_spb_num_db = 6;

  (*****************************************
   * Parameters for isc_info_svc_db_stats  *
   *****************************************)

  isc_spb_sts_data_pages = $01;
  isc_spb_sts_db_log = $02;
  isc_spb_sts_hdr_pages = $04;

  isc_spb_sts_idx_pages = $08;
  isc_spb_sts_sys_relations = $10;

{FB15_UP}
  isc_spb_sts_record_versions = $20;
  isc_spb_sts_table = $40;
{FB15_UP}

{FB20_UP}
  isc_spb_sts_nocreation		= $80;
{FB20_UP}

{FB25_UP}
  isc_spb_nbk_level       = 5;
  isc_spb_nbk_file        = 6;
  isc_spb_nbk_direct      = 7;
  isc_spb_nbk_no_triggers = $01;

  isc_spb_trc_id          = 1;
  isc_spb_trc_name        = 2;
  isc_spb_trc_cfg         = 3;
{FB25_UP}

  (*************************
   * SQL information items *
   *************************)

  isc_info_sql_select = 4;
  isc_info_sql_bind = 5;
  isc_info_sql_num_variables = 6;
  isc_info_sql_describe_vars = 7;
  isc_info_sql_describe_end = 8;
  isc_info_sql_sqlda_seq = 9;
  isc_info_sql_message_seq = 10;
  isc_info_sql_type = 11;
  isc_info_sql_sub_type = 12;
  isc_info_sql_scale = 13;
  isc_info_sql_length = 14;
  isc_info_sql_null_ind = 15;
  isc_info_sql_field = 16;
  isc_info_sql_relation = 17;
  isc_info_sql_owner = 18;
  isc_info_sql_alias = 19;
  isc_info_sql_sqlda_start = 20;
  isc_info_sql_stmt_type = 21;
  isc_info_sql_get_plan = 22;
  isc_info_sql_records = 23;
  isc_info_sql_batch_fetch = 24;

{FB20_UP}
  isc_info_sql_relation_alias = 25;
{FB20_UP}

  (*********************************
   * SQL information return values *
   *********************************)

  isc_info_sql_stmt_select = 1;
  isc_info_sql_stmt_insert = 2;
  isc_info_sql_stmt_update = 3;
  isc_info_sql_stmt_delete = 4;
  isc_info_sql_stmt_ddl = 5;
  isc_info_sql_stmt_get_segment = 6;
  isc_info_sql_stmt_put_segment = 7;
  isc_info_sql_stmt_exec_procedure = 8;
  isc_info_sql_stmt_start_trans = 9;
  isc_info_sql_stmt_commit = 10;
  isc_info_sql_stmt_rollback = 11;
  isc_info_sql_stmt_select_for_upd = 12;
  isc_info_sql_stmt_set_generator = 13;
{FB15_UP}
  isc_info_sql_stmt_savepoint = 14;
{FB15_UP}

  (***********************************
   * Server configuration key values *
   ***********************************)

  (**********************************************
   * Dynamic Data Definition Language operators *
   **********************************************)

  (******************
   * Version number *
   ******************)

  isc_dyn_version_1 = 1;
  isc_dyn_eoc = 255;

  (******************************
   * Operations (may be nested) *
   ******************************)

  isc_dyn_begin = 2;
  isc_dyn_end = 3;
  isc_dyn_if = 4;
  isc_dyn_def_database = 5;
  isc_dyn_def_global_fld = 6;
  isc_dyn_def_local_fld = 7;
  isc_dyn_def_idx = 8;
  isc_dyn_def_rel = 9;
  isc_dyn_def_sql_fld = 10;
  isc_dyn_def_view = 12;
  isc_dyn_def_trigger = 15;
  isc_dyn_def_security_class = 120;
  isc_dyn_def_dimension = 140;
  isc_dyn_def_generator = 24;
  isc_dyn_def_function = 25;
  isc_dyn_def_filter = 26;
  isc_dyn_def_function_arg = 27;
  isc_dyn_def_shadow = 34;
  isc_dyn_def_trigger_msg = 17;
  isc_dyn_def_file = 36;
  isc_dyn_mod_database = 39;
  isc_dyn_mod_rel = 11;
  isc_dyn_mod_global_fld = 13;
  isc_dyn_mod_idx = 102;
  isc_dyn_mod_local_fld = 14;
  isc_dyn_mod_sql_fld = 216;
  isc_dyn_mod_view = 16;
  isc_dyn_mod_security_class = 122;
  isc_dyn_mod_trigger = 113;
  isc_dyn_mod_trigger_msg = 28;
  isc_dyn_delete_database = 18;
  isc_dyn_delete_rel = 19;
  isc_dyn_delete_global_fld = 20;
  isc_dyn_delete_local_fld = 21;
  isc_dyn_delete_idx = 22;
  isc_dyn_delete_security_class = 123;
  isc_dyn_delete_dimensions = 143;
  isc_dyn_delete_trigger = 23;
  isc_dyn_delete_trigger_msg = 29;
  isc_dyn_delete_filter = 32;
  isc_dyn_delete_function = 33;

(**********************************************)
(* Generators again                           *)
(**********************************************)

  // FB20_UP + IB71_UP
  isc_dyn_delete_generator = 217;

{FB20_UP}
// New for comments in objects.
  isc_dyn_mod_function = 224;
  isc_dyn_mod_filter = 225;
  isc_dyn_mod_generator = 226;
  isc_dyn_mod_sql_role = 227;
  isc_dyn_mod_charset = 228;
  isc_dyn_mod_collation = 229;
  isc_dyn_mod_prc_parameter = 230;

(***********************)
(* collation values    *)
(***********************)
  isc_dyn_def_collation	= 231;
  isc_dyn_coll_for_charset = 232;
  isc_dyn_coll_from =	233;
  isc_dyn_coll_attribute = 234;
  isc_dyn_coll_specific_attributes_charset = 235;
  isc_dyn_coll_specific_attributes = 236;
  isc_dyn_del_collation = 237;
{FB20_UP}
{FB21_UP}
  isc_dyn_coll_from_external = 239;
{FB21_UP}
{FB25_UP}
  isc_dyn_mapping        = 243;
  isc_dyn_map_role       = 1;
  isc_dyn_unmap_role     = 2;
  isc_dyn_map_user       = 3;
  isc_dyn_unmap_user     = 4;
  isc_dyn_automap_role   = 5;
  isc_dyn_autounmap_role = 6;

  isc_dyn_user           = 244;
  isc_dyn_user_add       = 1;
  isc_dyn_user_mod       = 2;
  isc_dyn_user_del       = 3;
  isc_dyn_user_passwd    = 4;
  isc_dyn_user_first     = 5;
  isc_dyn_user_middle    = 6;
  isc_dyn_user_last      = 7;
  isc_dyn_user_admin     = 8;
  isc_user_end           = 0;
{FB25_UP}

  isc_dyn_delete_shadow = 35;
  isc_dyn_grant = 30;
  isc_dyn_revoke = 31;
{FB25_UP}
  isc_dyn_revoke_all = 246;
{FB25_UP}
  isc_dyn_def_primary_key = 37;
  isc_dyn_def_foreign_key = 38;
  isc_dyn_def_unique = 40;
  isc_dyn_def_procedure = 164;
  isc_dyn_delete_procedure = 165;
  isc_dyn_def_parameter = 135;
  isc_dyn_delete_parameter = 136;
  isc_dyn_mod_procedure = 175;
{FB20_LOWER}
  // deprecated
  isc_dyn_def_log_file = 176;
  isc_dyn_def_cache_file = 180;
{FB20_LOWER}
  isc_dyn_def_exception = 181;
  isc_dyn_mod_exception = 182;
  isc_dyn_del_exception = 183;
{FB20_LOWER}
  // deprecated
  isc_dyn_drop_log = 194;
  isc_dyn_drop_cache = 195;
  isc_dyn_def_default_log = 202;
{FB20_LOWER}
{FB20_UP}
  isc_dyn_def_difference = 220;
  isc_dyn_drop_difference = 221;
  isc_dyn_begin_backup = 222;
  isc_dyn_end_backup = 223;
{FB20_UP}

{FB21_UP}
  isc_dyn_debug_info = 240;
{FB21_UP}

  (***********************
   * View specific stuff *
   ***********************)

  isc_dyn_view_blr = 43;
  isc_dyn_view_source = 44;
  isc_dyn_view_relation = 45;
  isc_dyn_view_context = 46;
  isc_dyn_view_context_name = 47;

  (**********************
   * Generic attributes *
   **********************)

  isc_dyn_rel_name = 50;
  isc_dyn_fld_name = 51;
  isc_dyn_new_fld_name = 215;
  isc_dyn_idx_name = 52;
  isc_dyn_description = 53;
  isc_dyn_security_class = 54;
  isc_dyn_system_flag = 55;
  isc_dyn_update_flag = 56;
  isc_dyn_prc_name = 166;
  isc_dyn_prm_name = 137;
  isc_dyn_sql_object = 196;
  isc_dyn_fld_character_set_name = 174;

  (********************************
   * Relation specific attributes *
   ********************************)

  isc_dyn_rel_dbkey_length = 61;
  isc_dyn_rel_store_trig = 62;
  isc_dyn_rel_modify_trig = 63;
  isc_dyn_rel_erase_trig = 64;
  isc_dyn_rel_store_trig_source = 65;
  isc_dyn_rel_modify_trig_source = 66;
  isc_dyn_rel_erase_trig_source = 67;
  isc_dyn_rel_ext_file = 68;
  isc_dyn_rel_sql_protection = 69;
  isc_dyn_rel_constraint = 162;
  isc_dyn_delete_rel_constraint = 163;

  (************************************
   * Global field specific attributes *
   ************************************)
{FB21_UP}
  isc_dyn_rel_temporary = 238;
  isc_dyn_rel_temp_global_preserve = 1;
  isc_dyn_rel_temp_global_delete = 2;
{FB21_UP}

  isc_dyn_fld_type = 70;
  isc_dyn_fld_length = 71;
  isc_dyn_fld_scale = 72;
  isc_dyn_fld_sub_type = 73;
  isc_dyn_fld_segment_length = 74;
  isc_dyn_fld_query_header = 75;
  isc_dyn_fld_edit_string = 76;
  isc_dyn_fld_validation_blr = 77;
  isc_dyn_fld_validation_source = 78;
  isc_dyn_fld_computed_blr = 79;
  isc_dyn_fld_computed_source = 80;
  isc_dyn_fld_missing_value = 81;
  isc_dyn_fld_default_value = 82;
  isc_dyn_fld_query_name = 83;
  isc_dyn_fld_dimensions = 84;
  isc_dyn_fld_not_null = 85;
  isc_dyn_fld_precision = 86;
  isc_dyn_fld_char_length = 172;
  isc_dyn_fld_collation = 173;
  isc_dyn_fld_default_source = 193;
  isc_dyn_del_default = 197;
  isc_dyn_del_validation = 198;
  isc_dyn_single_validation = 199;
  isc_dyn_fld_character_set = 203;
{FB25_UP}
  isc_dyn_del_computed = 242;
{FB25_UP}

  (***********************************
   * Local field specific attributes *
   ***********************************)

  isc_dyn_fld_source = 90;
  isc_dyn_fld_base_fld = 91;
  isc_dyn_fld_position = 92;
  isc_dyn_fld_update_flag = 93;

  (*****************************
   * Index specific attributes *
   *****************************)

  isc_dyn_idx_unique = 100;
  isc_dyn_idx_inactive = 101;
  isc_dyn_idx_type = 103;
  isc_dyn_idx_foreign_key = 104;
  isc_dyn_idx_ref_column = 105;
  isc_dyn_idx_statistic = 204;

  (*******************************
   * Trigger specific attributes *
   *******************************)

  isc_dyn_trg_type = 110;
  isc_dyn_trg_blr = 111;
  isc_dyn_trg_source = 112;
  isc_dyn_trg_name = 114;
  isc_dyn_trg_sequence = 115;
  isc_dyn_trg_inactive = 116;
  isc_dyn_trg_msg_number = 117;
  isc_dyn_trg_msg = 118;

  (**************************************
   * Security Class specific attributes *
   **************************************)

  isc_dyn_scl_acl = 121;
  isc_dyn_grant_user = 130;
  isc_dyn_grant_proc = 186;
  isc_dyn_grant_trig = 187;
  isc_dyn_grant_view = 188;
  isc_dyn_grant_options = 132;
  isc_dyn_grant_user_group = 205;
  isc_dyn_grant_role = 218;
  isc_dyn_grant_user_explicit = 219;
{FB25_UP}
  isc_dyn_grant_grantor = 245;
{FB25_UP}

  (**********************************
   * Dimension specific information *
   **********************************)

  isc_dyn_dim_lower = 141;
  isc_dyn_dim_upper = 142;

  (****************************
   * File specific attributes *
   ****************************)

  isc_dyn_file_name = 125;
  isc_dyn_file_start = 126;
  isc_dyn_file_length = 127;
  isc_dyn_shadow_number = 128;
  isc_dyn_shadow_man_auto = 129;
  isc_dyn_shadow_conditional = 130;

  (********************************
   * Log file specific attributes *
   ********************************)
{FB20_LOWER}
  // deprecated
  isc_dyn_log_file_sequence = 177;
  isc_dyn_log_file_partitions = 178;
  isc_dyn_log_file_serial = 179;
  isc_dyn_log_file_overflow = 200;
  isc_dyn_log_file_raw = 201;
{FB20_LOWER}
  (***************************
   * Log specific attributes *
   ***************************)

{FB20_LOWER}
  // deprecated
  isc_dyn_log_group_commit_wait = 189;
  isc_dyn_log_buffer_size = 190;
  isc_dyn_log_check_point_length = 191;
  isc_dyn_log_num_of_buffers = 192;
{FB20_LOWER}

  (********************************
   * Function specific attributes *
   ********************************)

  isc_dyn_function_name = 145;
  isc_dyn_function_type = 146;
  isc_dyn_func_module_name = 147;
  isc_dyn_func_entry_point = 148;
  isc_dyn_func_return_argument = 149;
  isc_dyn_func_arg_position = 150;
  isc_dyn_func_mechanism = 151;
  isc_dyn_filter_in_subtype = 152;
  isc_dyn_filter_out_subtype = 153;

  isc_dyn_description2 = 154;
  isc_dyn_fld_computed_source2 = 155;
  isc_dyn_fld_edit_string2 = 156;
  isc_dyn_fld_query_header2 = 157;
  isc_dyn_fld_validation_source2 = 158;
  isc_dyn_trg_msg2 = 159;
  isc_dyn_trg_source2 = 160;
  isc_dyn_view_source2 = 161;
  isc_dyn_xcp_msg2 = 184;

  (*********************************
   * Generator specific attributes *
   *********************************)

  isc_dyn_generator_name = 95;
  isc_dyn_generator_id = 96;

  (*********************************
   * Procedure specific attributes *
   *********************************)

  isc_dyn_prc_inputs = 167;
  isc_dyn_prc_outputs = 168;
  isc_dyn_prc_source = 169;
  isc_dyn_prc_blr = 170;
  isc_dyn_prc_source2 = 171;

  (*********************************
   * Parameter specific attributes *
   *********************************)
{FB21_UP}
  isc_dyn_prc_type = 239;

  isc_dyn_prc_t_selectable = 1;
  isc_dyn_prc_t_executable = 2;
{FB21_UP}

  isc_dyn_prm_number = 138;
  isc_dyn_prm_type = 139;

  (********************************
   * Relation specific attributes *
   ********************************)
{FB21_UP}
  isc_dyn_prm_mechanism = 241;
{FB21_UP}

  isc_dyn_xcp_msg = 185;

  (**********************************************
   * Cascading referential integrity values     *
   **********************************************)

  isc_dyn_foreign_key_update = 205;
  isc_dyn_foreign_key_delete = 206;
  isc_dyn_foreign_key_cascade = 207;
  isc_dyn_foreign_key_default = 208;
  isc_dyn_foreign_key_null = 209;
  isc_dyn_foreign_key_none = 210;

  (***********************
   * SQL role values     *
   ***********************)

  isc_dyn_def_sql_role = 211;
  isc_dyn_sql_role_name = 212;
  isc_dyn_grant_admin_options = 213;
  isc_dyn_del_sql_role = 214;
  (* 215 & 216 are used some lines above. *)

  (**********************************************
   * Generators again                           *
   **********************************************)
{FB15_UP}
  gds_dyn_delete_generator = 217;
{FB15_UP}

  (****************************
   * Last $dyn value assigned *
   ****************************)

{FB25}
 isc_dyn_last_dyn_value_FB25 = 247;
{FB25}

{FB21}
 isc_dyn_last_dyn_value_FB21 = 242;
{FB21}

{FB20}
 isc_dyn_last_dyn_value_FB20 = 227;
{FB20}

{FB15}
 isc_dyn_last_dyn_value_FB15 = 219;
{FB15}

{FB103}
 isc_dyn_last_dyn_value_FB103 = 219;
{FB103}

{FB102}
 isc_dyn_last_dyn_value_FB102 = 219;
{FB102}

  (******************************************
   * Array slice description language (SDL) *
   ******************************************)

  isc_sdl_version1 = 1;
  isc_sdl_eoc = 255;
  isc_sdl_relation = 2;
  isc_sdl_rid = 3;
  isc_sdl_field = 4;
  isc_sdl_fid = 5;
  isc_sdl_struct = 6;
  isc_sdl_variable = 7;
  isc_sdl_scalar = 8;
  isc_sdl_tiny_integer = 9;
  isc_sdl_short_integer = 10;
  isc_sdl_long_integer = 11;
  isc_sdl_literal = 12;
  isc_sdl_add = 13;
  isc_sdl_subtract = 14;
  isc_sdl_multiply = 15;
  isc_sdl_divide = 16;
  isc_sdl_negate = 17;
  isc_sdl_eql = 18;
  isc_sdl_neq = 19;
  isc_sdl_gtr = 20;
  isc_sdl_geq = 21;
  isc_sdl_lss = 22;
  isc_sdl_leq = 23;
  isc_sdl_and = 24;
  isc_sdl_or = 25;
  isc_sdl_not = 26;
  isc_sdl_while = 27;
  isc_sdl_assignment = 28;
  isc_sdl_label = 29;
  isc_sdl_leave = 30;
  isc_sdl_begin = 31;
  isc_sdl_end = 32;
  isc_sdl_do3 = 33;
  isc_sdl_do2 = 34;
  isc_sdl_do1 = 35;
  isc_sdl_element = 36;

  (********************************************
   * International text interpretation values *
   ********************************************)

  isc_interp_eng_ascii = 0;
  isc_interp_jpn_sjis = 5;
  isc_interp_jpn_euc = 6;

  (*******************
   * SQL definitions *
   *******************)

  SQL_TEXT = 452; // Array of char
  SQL_VARYING = 448;
  SQL_SHORT = 500;
  SQL_LONG = 496;
  SQL_FLOAT = 482;
  SQL_DOUBLE = 480;
  SQL_D_FLOAT = 530;
  SQL_TIMESTAMP = 510;
  SQL_BLOB = 520;
  SQL_ARRAY = 540;
  SQL_QUAD = 550;
  SQL_TYPE_TIME = 560;
  SQL_TYPE_DATE = 570;
  SQL_INT64 = 580;

{FB25_UP}
  SQL_NULL = 32766;
{FB25_UP}

  (* Historical alias for pre V6 applications *)
  SQL_DATE = SQL_TIMESTAMP;

  (*****************
   * Blob Subtypes *
   *****************)

  (* types less than zero are reserved for customer use *)

  isc_blob_untyped = 0;

  (* internal subtypes *)

  isc_blob_text                     = 1;
  isc_blob_blr                      = 2;
  isc_blob_acl                      = 3;
  isc_blob_ranges                   = 4;
  isc_blob_summary                  = 5;
  isc_blob_format                   = 6;
  isc_blob_tra                      = 7;
  isc_blob_extfile                  = 8;
{FB21_UP}
  isc_blob_debug_info               = 9;
  isc_blob_max_predefined_subtype   = 10;
{FB21_UP}

  (* the range 20-30 is reserved for dBASE and Paradox types *)

  isc_blob_formatted_memo = 20;
  isc_blob_paradox_ole = 21;
  isc_blob_graphic = 22;
  isc_blob_dbase_ole = 23;
  isc_blob_typed_binary = 24;

{FB21_UP}
  fb_dbg_version = 1;
  fb_dbg_end = 255;
  fb_dbg_map_src2blr = 2;
  fb_dbg_map_varname = 3;
  fb_dbg_map_argument = 4;

  fb_dbg_arg_input = 0;
  fb_dbg_arg_output = 1;
{FB21_UP}

(*******************************************************************************
 *    LINK LIBRARY                                                             *
 *******************************************************************************)

const
  GDS32DLL = 'fbclient.dll';

type
  TALFBXBaseLibrary = class(TObject)
  private
    FGDS32Lib: THandle;
    FVersion_API: TALFBXVersion_API;
  protected
    BLOB_close: function(Stream: PBStream): Integer;
      stdcall;
    BLOB_display: function(blob_id: PISCQuad; database: IscDbHandle; transaction: IscTrHandle;
      field_name: PAnsiChar): Integer;
      stdcall;
    BLOB_dump: function(blob_id: PISCQuad; database: IscDbHandle; transaction: IscTrHandle;
      file_name: PAnsiChar): Integer;
      stdcall;
    BLOB_edit: function(blob_id: PISCQuad; database: IscDbHandle; transaction: IscTrHandle;
      field_name: PAnsiChar): Integer;
      stdcall;
    BLOB_get: function(Stream: PBStream): Integer;
      stdcall;
    BLOB_load: function(blob_id: PISCQuad; database: IscDbHandle; transaction: IscTrHandle;
      file_name: PAnsiChar): Integer;
      stdcall;
    BLOB_open: function(blob: IscBlobHandle; buffer: PAnsiChar; length: Integer): PBStream;
      stdcall;
    BLOB_put: function(x: AnsiChar; Stream: PBStream): Integer;
      stdcall;
    BLOB_text_dump: function(blob_id: PISCQuad; database: IscDbHandle; transaction: IscTrHandle;
      file_name: PAnsiChar): Integer;
      stdcall;
    BLOB_text_load: function(blob_id: PISCQuad; database: IscDbHandle; transaction: IscTrHandle;
      file_name: PAnsiChar): Integer;
      stdcall;
    Bopen: function(blob_id: PISCQuad; database: IscDbHandle; transaction: IscTrHandle;
      mode: PAnsiChar): PBStream;
      stdcall;
    isc_add_user: function(status: PISCStatus; user_data: PUserSecData): Integer;
      stdcall;
    isc_array_gen_sdl: function(status: PISCStatus; desc: PISCArrayDesc; sdl_buffer_length: PSmallInt;
      sdl_buffer: PAnsiChar; sdl_length: PSmallInt): ISCStatus;
      stdcall;
    isc_array_get_slice: function(status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; array_id: PISCQuad; desc: PISCArrayDesc; array_: PPointer;
      slice_length: PISCLong): ISCStatus;
      stdcall;
    isc_array_lookup_bounds: function(status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; relation_name, field_name: PAnsiChar;
      desc: PISCArrayDesc): ISCStatus;
      stdcall;
    isc_array_lookup_desc: function(status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; relation_name, field_name: PAnsiChar;
      desc: PISCArrayDesc): ISCStatus;
      stdcall;
    isc_array_put_slice: function(status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; array_id: PISCQuad; desc: PISCArrayDesc; array_: PPointer;
      slice_length: PISCLong): ISCStatus;
      stdcall;
    isc_array_set_desc: function(status: PISCStatus; relation_name, field_name: PAnsiChar;
      sql_dtype, sql_length, dimensions: PSmallint; desc: PISCArrayDesc): ISCStatus;
      stdcall;
    isc_attach_database: function(user_status: PISCStatus; file_length: Smallint;
      file_name: PAnsiChar; handle: PIscDbHandle; dpb_length: Smallint; dpb: PAnsiChar): ISCStatus;
      stdcall;
    isc_blob_default_desc: procedure(desc: PISCBlobDesc; relation_name, field_name: PAnsiChar);
      stdcall;
    isc_blob_gen_bpb: function(status: PISCStatus; to_desc, from_desc: PISCBlobDesc;
      bpb_buffer_length: Word; bpb_buffer: PAnsiChar; bpb_length: PWord): ISCStatus;
      stdcall;
    isc_blob_info: function(user_status: PISCStatus; blob_handle: PIscBlobHandle;
      item_length: Smallint; items: PAnsiChar; buffer_length: Smallint; buffer: PAnsiChar): ISCStatus;
      stdcall;
    isc_blob_lookup_desc: function(status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; relation_name, field_name: PAnsiChar; desc: PISCBlobDesc;
      global: PAnsiChar): ISCStatus;
      stdcall;
    isc_blob_set_desc: function(status: PISCStatus; relation_name, field_name: PAnsiChar;
      subtype, charset, segment_size: Smallint; desc: PISCBlobDesc): ISCStatus;
      stdcall;
    isc_cancel_blob: function(user_status: PISCStatus; blob_handle: PIscBlobHandle): ISCStatus;
      stdcall;
    isc_cancel_events: function(user_status: PISCStatus; handle: PIscDbHandle;
      id: PISCLong): ISCStatus;
      stdcall;
    isc_close: function(user_status: PISCStatus; name: PAnsiChar): ISCStatus;
      stdcall;
    isc_close_blob: function(user_status: PISCStatus;
      blob_handle: PIscBlobHandle): ISCStatus;
      stdcall;
    isc_commit_retaining: function(user_status: PISCStatus;
      tra_handle: PIscTrHandle): ISCStatus;
      stdcall;
    isc_commit_transaction: function(user_status: PISCStatus;
      tra_handle: PIscTrHandle): ISCStatus;
      stdcall;
    isc_compile_request: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      req_handle: PIscReqHandle; blr_length: Smallint;
      blr: PAnsiChar): ISCStatus;
      stdcall;
    isc_compile_request2: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      req_handle: PIscReqHandle; blr_length: Smallint;
      blr: PAnsiChar): ISCStatus;
      stdcall;
    isc_create_blob: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; blob_handle: PIscBlobHandle;
      blob_id: PISCQuad): ISCStatus;
      stdcall;
    isc_create_blob2: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; blob_handle: PIscBlobHandle; blob_id: PISCQuad;
      bpb_length: Smallint; bpb: PAnsiChar): ISCStatus;
      stdcall;
    isc_create_database: function(user_status: PISCStatus; file_length: Smallint;
      file_name: PAnsiChar; handle: PIscDbHandle; dpb_length: Smallint; dpb: PAnsiChar;
      db_type: Smallint): ISCStatus;
      stdcall;
    isc_database_info: function(user_status: PISCStatus; handle: PIscDbHandle;
      item_length: Smallint; items: PAnsiChar; buffer_length: Smallint;
      buffer: PAnsiChar): ISCStatus;
      stdcall;
    isc_ddl: function(user_status: PISCStatus; db_handle: PIscDbHandle; tra_handle: PIscTrHandle;
      length: Smallint; ddl: PAnsiChar): ISCStatus;
      stdcall;
    isc_declare: function(user_status: PISCStatus; statement,
      cursor: PAnsiChar): ISCStatus;
      stdcall;
    isc_decode_date: procedure(date: PISCQuad; times: PPointer);
      stdcall;
    isc_decode_sql_date: procedure(date: PISCDate; times_arg: PPointer);
      stdcall;
    isc_decode_sql_time: procedure(sql_time: PISCTime; times_arg: PPointer);
      stdcall;
    isc_decode_timestamp: procedure(date: PISCTimeStamp; times_arg: PPointer);
      stdcall;
    isc_delete_user: function(status: PISCStatus; user_data: PUserSecData): Integer;
      stdcall;
    isc_describe: function(user_status: PISCStatus; name: PAnsiChar; sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_describe_bind: function(user_status: PISCStatus; name: PAnsiChar; sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_detach_database: function(user_status: PISCStatus; handle: PIscDbHandle): ISCStatus;
      stdcall;
    isc_drop_database: function(user_status: PISCStatus; handle: PIscDbHandle): ISCStatus;
      stdcall;
    isc_dsql_alloc_statement2: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      stmt_handle: PIscStmtHandle): ISCStatus;
      stdcall;
    isc_dsql_allocate_statement: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      stmt_handle: PIscStmtHandle): ISCStatus;
      stdcall;
    isc_dsql_describe: function(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
      dialect: Word; sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_dsql_describe_bind: function(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
      dialect: Word; sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_dsql_exec_immed2: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; length: Word; string_: PAnsiChar; dialect: Word; in_sqlda,
      out_sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_dsql_exec_immed3_m: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; Length: Word; string_: PAnsiChar; dialect, in_blr_length: Word;
      in_blr: PAnsiChar; in_msg_type, in_msg_length: Word; in_msg: PAnsiChar; out_blr_length: Word;
      out_blr: PAnsiChar; out_msg_type, out_msg_length: Word; out_msg: PAnsiChar): ISCStatus;
      stdcall;
    isc_dsql_execute: function(user_status: PISCStatus; tra_handle: PIscTrHandle;
      stmt_handle: PIscStmtHandle; dialect: Word; sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_dsql_execute_immediate: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; length: Word; string_: PAnsiChar; dialect: Word;
      sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_dsql_execute_immediate_m: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; length: Word; string_: PAnsiChar; dialect, blr_length: Word;
      blr: PAnsiChar; msg_type, msg_length: Word; msg: PAnsiChar): ISCStatus;
      stdcall;
    isc_dsql_execute_m: function(user_status: PISCStatus; tra_handle: PIscTrHandle;
      stmt_handle: PIscStmtHandle; blr_length: Word; blr: PAnsiChar; msg_type, msg_length: Word;
      msg: PAnsiChar): ISCStatus;
      stdcall;
    isc_dsql_execute2: function(user_status: PISCStatus; tra_handle: PIscTrHandle;
      stmt_handle: PIscStmtHandle; dialect: Word; in_sqlda, out_sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_dsql_execute2_m: function(user_status: PISCStatus; tra_handle: PIscTrHandle;
      stmt_handle: PIscStmtHandle; in_blr_length: Word; in_blr: PAnsiChar; in_msg_type,
      in_msg_length: Word; in_msg: PAnsiChar; out_blr_length: Word; out_blr: PAnsiChar;
      out_msg_type, out_msg_length: Word; out_msg: PAnsiChar): ISCStatus;
      stdcall;
    isc_dsql_fetch: function(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
      dialect: Word; sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_dsql_fetch_m: function(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
      blr_length: Word; blr: PAnsiChar; msg_type, msg_length: Word; msg: PAnsiChar): ISCStatus;
      stdcall;
    isc_dsql_finish: function(db_handle: PIscDbHandle): ISCStatus;
      stdcall;
    isc_dsql_free_statement: function(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
      option: Word): ISCStatus;
      stdcall;
    isc_dsql_insert: function(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
      dialect: Word; sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_dsql_insert_m: function(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
      blr_length: Word; blr: PAnsiChar; msg_type, msg_length: Word; msg: PAnsiChar): ISCStatus;
      stdcall;
    isc_dsql_prepare: function(user_status: PISCStatus; tra_handle: PIscTrHandle;
      stmt_handle: PIscStmtHandle; length: Word; string_: PAnsiChar; dialect: Word;
      sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_dsql_prepare_m: function(user_status: PISCStatus; tra_handle: PIscTrHandle;
      stmt_handle: PIscStmtHandle; length: Word; string_: PAnsiChar; dialect, item_length: Word;
      items: PAnsiChar; buffer_length: Word; buffer: PAnsiChar): ISCStatus;
      stdcall;
    isc_dsql_release: function(user_status: PISCStatus; name: PAnsiChar): ISCStatus;
      stdcall;
    isc_dsql_set_cursor_name: function(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
      cursor: PAnsiChar; type_: Word): ISCStatus;
      stdcall;
    isc_dsql_sql_info: function(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
      item_length: Smallint; items: PAnsiChar; buffer_length: Smallint; buffer: PAnsiChar): ISCStatus;
      stdcall;
    isc_embed_dsql_close: function(user_status: PISCStatus; name: PAnsiChar): ISCStatus;
      stdcall;
    isc_embed_dsql_declare: function(user_status: PISCStatus; stmt_name, cursor: PAnsiChar): ISCStatus;
      stdcall;
    isc_embed_dsql_describe: function(user_status: PISCStatus; stmt_name: PAnsiChar;
      dialect: Word; sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_embed_dsql_describe_bind: function(user_status: PISCStatus; stmt_name: PAnsiChar;
      dialect: Word; sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_embed_dsql_execute: function(user_status: PISCStatus; trans_handle: PIscTrHandle;
      stmt_name: PAnsiChar; dialect: Word; sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_embed_dsql_execute_immed: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; length: Word; string_: PAnsiChar; dialect: Word;
      sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_embed_dsql_execute2: function(user_status: PISCStatus; trans_handle: PIscTrHandle;
      stmt_name: PAnsiChar; dialect: Word; in_sqlda, out_sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_embed_dsql_fetch: function(user_status: PISCStatus; cursor_name: PAnsiChar;
      dialect: Word; sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_embed_dsql_insert: function(user_status: PISCStatus; cursor_name: PAnsiChar;
      dialect: Word; sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_embed_dsql_open: function(user_status: PISCStatus; trans_handle: PIscTrHandle;
      cursor_name: PAnsiChar; dialect: Word; sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_embed_dsql_open2: function(user_status: PISCStatus; trans_handle: PIscTrHandle;
      cursor_name: PAnsiChar; dialect: Word; in_sqlda, out_sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_embed_dsql_prepare: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; stmt_name: PAnsiChar; length: Word; string_: PAnsiChar;
      dialect: Word; sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_embed_dsql_release: function(user_status: PISCStatus; stmt_name: PAnsiChar): ISCStatus;
      stdcall;
    isc_encode_date: procedure(times: PPointer; date: PISCQuad);
      stdcall;
    isc_encode_sql_date: procedure(times_arg: PPointer; date: PISCDate);
      stdcall;
    isc_encode_sql_time: procedure(times_arg: PPointer; isc_time: PISCTime);
      stdcall;
    isc_encode_timestamp: procedure(times_arg: PPointer; date: PISCTimeStamp);
      stdcall;
    isc_event_block: function(event_buffer, result_buffer: PPAnsiChar; count: Word;
      v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15: PAnsiChar): ISCLong; cdecl;
{FB21_UP}
    isc_event_block_a: function(event_buffer, result_buffer: PPAnsiChar; count: Word; name_buffer: PPAnsiChar): Word;
      stdcall;
{FB21_UP}
    isc_event_counts: procedure(ser_status: PISCStatus; buffer_length: Smallint;
      event_buffer, result_buffer: PAnsiChar);
      stdcall;
    isc_execute: function(user_status: PISCStatus; trans_handle: PIscTrHandle;
      name: PAnsiChar; sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_execute_immediate: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; length: PSmallint; string_: PAnsiChar): ISCStatus;
      stdcall;
    isc_expand_dpb: procedure(dpb: PPAnsiChar; dpb_size: PSmallint; name_buffer: PPAnsiChar); cdecl;
    isc_fetch: function(user_status: PISCStatus; name: PAnsiChar; sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_free: function(blk: PAnsiChar): ISCLong;
      stdcall;
    isc_ftof: function(string_: PAnsiChar; length1: Word; field: PAnsiChar; length2: Word): ISCStatus;
      stdcall;
    isc_get_segment: function(user_status: PISCStatus; blob_handle: PIscBlobHandle; length: PWord;
      buffer_length: Word; buffer: PAnsiChar): ISCStatus;
      stdcall;
    isc_get_slice: function(user_status: PISCStatus; db_handle: PIscDbHandle; tra_handle: PIscTrHandle;
      array_id: PISCQuad; sdl_length: Smallint; sdl: PAnsiChar; param_length: Smallint; param: PISCLong;
      slice_length: ISCLong; slice: PPointer; return_length: PISCLong): ISCStatus;
      stdcall;
    isc_interprete: function(buffer: PAnsiChar; status_vector: PPISCStatus): ISCStatus;
      stdcall;
{FB20_UP}
    fb_interpret: function(buffer: PAnsiChar; v: integer; status_vector: PPISCStatus): ISCStatus;
      stdcall;
{FB20_UP}
    isc_modify_dpb: function(dpb: PPAnsiChar; dpb_length: PSmallint; type_: Word;
      str: PAnsiChar; str_len: Smallint): Integer;
      stdcall;
    isc_modify_user: function(status: PISCStatus; user_data: PUserSecData): Integer;
      stdcall;
    isc_open: function(user_status: PISCStatus; trans_handle: PIscTrHandle; name: PAnsiChar;
      sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_open_blob: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; blob_handle: PIscBlobHandle; blob_id: PISCQuad): ISCStatus;
      stdcall;
    isc_open_blob2: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; blob_handle: PIscBlobHandle; blob_id: PISCQuad; bpb_length: Word;
      bpb: PAnsiChar): ISCStatus;
      stdcall;
    isc_portable_integer: function(ptr: PAnsiChar; length: Smallint): ISCInt64;
      stdcall;
    isc_prepare: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; name: PAnsiChar; length: PSmallint; string_: PAnsiChar;
      sqlda: PXSQLDA): ISCStatus;
      stdcall;
    isc_prepare_transaction: function(user_status: PISCStatus; tra_handle: PIscTrHandle): ISCStatus;
      stdcall;
    isc_prepare_transaction2: function(user_status: PISCStatus; tra_handle: PIscTrHandle;
      msg_length: ISCUShort; msg: PISCUChar): ISCStatus;
      stdcall;
    isc_print_blr: function(blr: PAnsiChar; callback: ISC_PRINT_CALLBACK; callback_argument: PPointer;
      language: Smallint): ISCStatus;
      stdcall;
{FB25_UP}
    fb_print_blr: function(const blr: PAnsiChar; blr_length: ISC_ULONG; routine: ISC_PRINT_CALLBACK;
			user_arg: Pointer; language: Smallint): Integer;
      stdcall;
{FB25_UP}
    isc_print_sqlerror: procedure(sqlcode: ISCShort; status_vector: PISCStatus);
      stdcall;
    isc_print_status: function(status_vector: PISCStatus): ISCStatus;
      stdcall;
    isc_put_segment: function(user_status: PISCStatus; blob_handle: PIscBlobHandle;
      buffer_length: Word; buffer: PAnsiChar): ISCStatus;
      stdcall;
    isc_put_slice: function(user_status: PISCStatus; db_handle: PIscDbHandle; tra_handle: PIscTrHandle;
      array_id: PISCQuad; sdl_length: Smallint; sdl: PAnsiChar; param_length: Smallint; param: PISCLong;
      slice_length: ISCLong; slice: PPointer): ISCStatus;
      stdcall;
    isc_qtoq: procedure(quad1, quad2: PISCQuad);
      stdcall;
    isc_que_events: function(user_status: PISCStatus; handle: PIscDbHandle; id: PISCLong;
      length: ISCUShort; events: PAnsiChar; ast: ISC_EVENT_CALLBACK; arg: PPointer): ISCStatus;
      stdcall;
    isc_receive: function(user_status: PISCStatus; req_handle: PIscReqHandle; msg_type,
      msg_length: Smallint; msg: PPointer; level: Smallint): ISCStatus;
      stdcall;
    isc_reconnect_transaction: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; length: Smallint; id: PAnsiChar): ISCStatus;
      stdcall;
    isc_release_request: function(user_status: PISCStatus; req_handle: PIscReqHandle): ISCStatus;
      stdcall;
    isc_request_info: function(user_status: PISCStatus; req_handle: PIscReqHandle; level,
      item_length: Smallint; items: PAnsiChar; buffer_length: Smallint; buffer: PAnsiChar): ISCStatus;
      stdcall;
    isc_rollback_retaining: function(status_vector: PISCStatus; trans_handle: PIscTrHandle): ISCStatus;
      stdcall;
    isc_rollback_transaction: function(user_status: PISCStatus; tra_handle: PIscTrHandle): ISCStatus;
      stdcall;
    isc_seek_blob: function(user_status: PISCStatus; blob_handle: PIscBlobHandle; mode: Smallint;
      offset: ISCLong; Result_: PISCLong): ISCStatus;
      stdcall;
    isc_send: function(user_status: PISCStatus; req_handle: PIscReqHandle; msg_type,
      msg_length: Smallint; msg: PPointer; level: Smallint): ISCStatus;
      stdcall;
    isc_service_attach: function(status_vector: PISCStatus; service_length: Word;
      service_name: PAnsiChar; handle: PIscSvcHandle; spb_length: Word; spb: PAnsiChar): ISCStatus;
      stdcall;
    isc_service_detach: function(status_vector: PISCStatus; handle: PIscSvcHandle): ISCStatus;
      stdcall;
    isc_service_query: function(status_vector: PISCStatus; svc_handle: PIscSvcHandle;
      reserved: PIscResvHandle; send_spb_length: Word; send_spb: PAnsiChar; request_spb_length: Word;
      request_spb: PAnsiChar; buffer_length: Word; buffer: PAnsiChar): ISCStatus;
      stdcall;
    isc_service_start: function(status_vector: PISCStatus; svc_handle: PIscSvcHandle;
      reserved: PIscResvHandle; spb_length: Word; spb: PAnsiChar): ISCStatus;
      stdcall;
{FB25_UP}
    fb_shutdown: function(timeout: Cardinal; const reason: Integer): Integer;
      stdcall;
    fb_shutdown_callback: function(user_status: PISCStatus; callBack: FB_SHUTDOWN_CALLBACK;
      const mask: Integer; arg: Pointer): ISC_STATUS;
      stdcall;
    fb_cancel_operation: function(user_status: PISCStatus; handle: PIscDbHandle; option: ISC_USHORT): ISC_STATUS;
      stdcall;
{FB25_UP}
    isc_set_debug: procedure(flag: Integer);
      stdcall;
    isc_sql_interprete: procedure(SQLCODE: Smallint; buffer: PAnsiChar; buffer_length: Smallint);
      stdcall;
    isc_sqlcode: function(user_status: PISCStatus): ISCLong;
      stdcall;
{FB25_UP}
    fb_sqlstate: procedure(buffer: PAnsiChar; const user_status: PISCStatus);
      stdcall;
{FB25_UP}
    isc_start_and_send: function(user_status: PISCStatus; req_handle: PIscReqHandle;
      tra_handle: PIscTrHandle; msg_type, msg_length: Smallint; msg: PPointer;
      level: Smallint): ISCStatus;
      stdcall;
    isc_start_multiple: function(user_status: PISCStatus; tra_handle: PIscTrHandle;
      count: Smallint; vector: PISCTEB): ISCStatus;
      stdcall;
    isc_start_request: function(user_status: PISCStatus; req_handle: PIscReqHandle;
      tra_handle: PIscTrHandle; level: Smallint): ISCStatus;
      stdcall;
    isc_start_transaction: function(user_status: PISCStatus; tra_handle: PIscTrHandle; count: Smallint;
      db_handle: PIscDbHandle; tpb_length: ISCUShort; tpb_ad: PAnsiChar): ISCStatus; cdecl;
{FB25_UP}
    fb_disconnect_transaction: function(user_status: PISCStatus; tra_handle: PIscTrHandle): ISC_STATUS;
      stdcall;
{FB25_UP}
    isc_transact_request: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; blr_length: Word; blr: PAnsiChar; in_msg_length: Word; in_msg: PAnsiChar;
      out_msg_length: Word; out_msg: PAnsiChar): ISCStatus;
      stdcall;
    isc_transaction_info: function(user_status: PISCStatus; tra_handle: PIscTrHandle; item_length: Smallint;
      items: PAnsiChar; buffer_length: Smallint; buffer: PAnsiChar): ISCStatus;
      stdcall;
    isc_unwind_request: function(user_status: PISCStatus; req_handle: PIscTrHandle;
      level: Smallint): ISCStatus;
      stdcall;
    isc_vax_integer: function(ptr: PAnsiChar; length: Smallint): ISCLong;
      stdcall;
    isc_version: function(db_handle: PIscDbHandle; callback: ISC_VERSION_CALLBACK;
      callback_argument: PPointer): Integer;
      stdcall;
    isc_vtof: procedure(string1, string2: PAnsiChar; length: Word);
      stdcall;
    isc_vtov: procedure(string1, string2: PAnsiChar; length: Smallint);
      stdcall;
    isc_wait_for_event: function(user_status: PISCStatus; handle: PIscDbHandle;
      length: Smallint; events, buffer: PAnsiChar): ISCStatus;
      stdcall;
    {FB15_UP}
    isc_reset_fpe: function(fpe_status: Word): ISCLong; stdcall;
    {FB15_UP}
    {FB15}
    isc_get_client_version: procedure(version: PAnsiChar);
      stdcall;
    isc_get_client_major_version: function: Integer;
      stdcall;
    isc_get_client_minor_version: function: Integer;
      stdcall;
    {FB15}
  public
    constructor Create(ApiVer: TALFBXVersion_Api); virtual;
    destructor Destroy; override;
    Function Version_Api_Is_FB15_Up: Boolean;
    Function Version_Api_Is_FB20_Up: Boolean;
    Function Version_Api_Is_FB21_Up: Boolean;
    Function Version_Api_Is_FB25_Up: Boolean;
    function getb(p: PBStream): AnsiChar;
    function putb(x: AnsiChar; p: PBStream): Integer;
    function putbx(x: AnsiChar; p: PBStream): Integer;
    function Loaded: Boolean; virtual;
    function Unload: Boolean; virtual;
    function Load(const lib: AnsiString = GDS32DLL): Boolean; virtual;
  end;

implementation

uses System.SysUtils,
     ALFBXconst;

(*******************************************************************************
 *    MACROS                                                                   *
 *******************************************************************************)

function XSQLDA_LENGTH(n: Integer): Integer;
begin
  Result := SizeOf(TXSQLDA) + ((n - 1) * SizeOf(TXSQLVAR));
end;

procedure ADD_SPB_LENGTH(var p: PAnsiChar; length: Integer);
begin
  p^ := AnsiChar(length);
  Inc(p);
  p^ := AnsiChar(length shr 8);
  Inc(p);
end;

procedure ADD_SPB_NUMERIC(var p: PAnsiChar; data: Integer);
begin
  p^ := AnsiChar(data);
  Inc(p);
  p^ := AnsiChar(data shr 8);
  Inc(p);
  p^ := AnsiChar(data shr 16);
  Inc(p);
  p^ := AnsiChar(data shr 24);
  Inc(p);
end;

{ TUIBLibrary }

Function TALFBXBaseLibrary.Version_Api_Is_FB15_Up: Boolean;
Begin
  Result := FVersion_Api in [FB15, FB20, FB21, FB25];
end;

Function TALFBXBaseLibrary.Version_Api_Is_FB20_Up: Boolean;
Begin
  Result := FVersion_Api in [FB20, FB21, FB25];
end;

Function TALFBXBaseLibrary.Version_Api_Is_FB21_Up: Boolean;
Begin
  Result := FVersion_Api in [FB21, FB25];
end;

Function TALFBXBaseLibrary.Version_Api_Is_FB25_Up: Boolean;
Begin
  Result := FVersion_Api in [FB25];
end;

constructor TALFBXBaseLibrary.Create(ApiVer: TALFBXVersion_Api);
begin
  FGDS32Lib := 0;
  FVersion_API := ApiVer;
end;

function TALFBXBaseLibrary.getb(p: PBStream): AnsiChar;
begin
  Dec(p^.bstr_cnt);
  if p^.bstr_cnt >= 0 then
  begin
    Result := AnsiChar(Integer(p^.bstr_ptr^) and $FF);
    Inc(p^.bstr_ptr);
  end
  else
    Result := AnsiChar(BLOB_get(p));
end;

function TALFBXBaseLibrary.putb(x: AnsiChar; p: PBStream): Integer;
begin
  Dec(p^.bstr_cnt);
  if (x = #10) or (p^.bstr_cnt = 0) then
    Result := BLOB_put(x, p)
  else
  begin
    p^.bstr_ptr^ := AnsiChar(x);
    Inc(p^.bstr_ptr);
    Result := Cardinal(x);
  end;
end;

function TALFBXBaseLibrary.putbx(x: AnsiChar; p: PBStream): Integer;
begin
  Dec(p^.bstr_cnt);
  if p^.bstr_cnt = 0 then
    Result := BLOB_put(x, p)
  else
  begin
    p^.bstr_ptr^ := AnsiChar(x);
    Inc(p^.bstr_ptr);
    Result := Cardinal(x);
  end;
end;

function TALFBXBaseLibrary.Loaded: Boolean;
begin
  Result := FGDS32Lib <> 0;
end;

function TALFBXBaseLibrary.Unload: Boolean;
begin
    Result := True;
    if Loaded then
    begin
      Result := Boolean(FreeLibrary(FGDS32Lib));
      FGDS32Lib := 0;
      BLOB_close := nil;
      BLOB_display := nil;
      BLOB_dump := nil;
      BLOB_edit := nil;
      BLOB_get := nil;
      BLOB_load := nil;
      BLOB_open := nil;
      BLOB_put := nil;
      BLOB_text_dump := nil;
      BLOB_text_load := nil;
      Bopen := nil;
      isc_add_user := nil;
      isc_array_gen_sdl := nil;
      isc_array_get_slice := nil;
      isc_array_lookup_bounds := nil;
      isc_array_lookup_desc := nil;
      isc_array_put_slice := nil;
      isc_array_set_desc := nil;
      isc_attach_database := nil;
      isc_blob_default_desc := nil;
      isc_blob_gen_bpb := nil;
      isc_blob_info := nil;
      isc_blob_lookup_desc := nil;
      isc_blob_set_desc := nil;
      isc_cancel_blob := nil;
      isc_cancel_events := nil;
      isc_close := nil;
      isc_close_blob := nil;
      isc_commit_retaining := nil;
      isc_commit_transaction := nil;
      isc_compile_request := nil;
      isc_compile_request2 := nil;
      isc_create_blob := nil;
      isc_create_blob2 := nil;
      isc_create_database := nil;
      isc_database_info := nil;
      isc_ddl := nil;
      isc_declare := nil;
      isc_decode_date := nil;
      isc_decode_sql_date := nil;
      isc_decode_sql_time := nil;
      isc_decode_timestamp := nil;
      isc_delete_user := nil;
      isc_describe := nil;
      isc_describe_bind := nil;
      isc_detach_database := nil;
      isc_drop_database := nil;
      isc_dsql_alloc_statement2 := nil;
      isc_dsql_allocate_statement := nil;
      isc_dsql_describe := nil;
      isc_dsql_describe_bind := nil;
      isc_dsql_exec_immed2 := nil;
      isc_dsql_exec_immed3_m := nil;
      isc_dsql_execute := nil;
      isc_dsql_execute_immediate := nil;
      isc_dsql_execute_immediate_m := nil;
      isc_dsql_execute_m := nil;
      isc_dsql_execute2 := nil;
      isc_dsql_execute2_m := nil;
      isc_dsql_fetch := nil;
      isc_dsql_fetch_m := nil;
      isc_dsql_finish := nil;
      isc_dsql_free_statement := nil;
      isc_dsql_insert := nil;
      isc_dsql_insert_m := nil;
      isc_dsql_prepare := nil;
      isc_dsql_prepare_m := nil;
      isc_dsql_release := nil;
      isc_dsql_set_cursor_name := nil;
      isc_dsql_sql_info := nil;
      isc_embed_dsql_close := nil;
      isc_embed_dsql_declare := nil;
      isc_embed_dsql_describe := nil;
      isc_embed_dsql_describe_bind := nil;
      isc_embed_dsql_execute := nil;
      isc_embed_dsql_execute_immed := nil;
      isc_embed_dsql_execute2 := nil;
      isc_embed_dsql_fetch := nil;
      isc_embed_dsql_insert := nil;
      isc_embed_dsql_open := nil;
      isc_embed_dsql_open2 := nil;
      isc_embed_dsql_prepare := nil;
      isc_embed_dsql_release := nil;
      isc_encode_date := nil;
      isc_encode_sql_date := nil;
      isc_encode_sql_time := nil;
      isc_encode_timestamp := nil;
      isc_event_block := nil;
      {FB21_UP}
      isc_event_block_a := nil;
      {FB21_UP}
      isc_event_counts := nil;
      isc_execute := nil;
      isc_execute_immediate := nil;
      isc_expand_dpb := nil;
      isc_fetch := nil;
      isc_free := nil;
      isc_ftof := nil;
      isc_get_segment := nil;
      isc_get_slice := nil;
      isc_interprete := nil;
      isc_modify_dpb := nil;
      isc_modify_user := nil;
      isc_open := nil;
      isc_open_blob := nil;
      isc_open_blob2 := nil;
      isc_portable_integer := nil;
      isc_prepare := nil;
      isc_prepare_transaction := nil;
      isc_prepare_transaction2 := nil;
      isc_print_blr := nil;
      isc_print_sqlerror := nil;
      isc_print_status := nil;
      isc_put_segment := nil;
      isc_put_slice := nil;
      isc_qtoq := nil;
      isc_que_events := nil;
      isc_receive := nil;
      isc_reconnect_transaction := nil;
      isc_release_request := nil;
      isc_request_info := nil;
      isc_rollback_retaining := nil;
      isc_rollback_transaction := nil;
      isc_seek_blob := nil;
      isc_send := nil;
      isc_service_attach := nil;
      isc_service_detach := nil;
      isc_service_query := nil;
      isc_service_start := nil;
      isc_set_debug := nil;
      isc_sql_interprete := nil;
      isc_sqlcode := nil;
      isc_start_and_send := nil;
      isc_start_multiple := nil;
      isc_start_request := nil;
      isc_start_transaction := nil;
      isc_transact_request := nil;
      isc_transaction_info := nil;
      isc_unwind_request := nil;
      isc_vax_integer := nil;
      isc_version := nil;
      isc_vtof := nil;
      isc_vtov := nil;
      isc_wait_for_event := nil;
    {FB15_UP}
      isc_reset_fpe := nil;
      isc_get_client_version := nil;
      isc_get_client_major_version := nil;
      isc_get_client_minor_version := nil;
    {FB20_UP}
      fb_interpret := nil;
    {FB20_UP}
    {FB25_UP}
      fb_print_blr := nil;
      fb_shutdown := nil;
      fb_shutdown_callback := nil;
      fb_cancel_operation := nil;
      fb_sqlstate := nil;
      fb_disconnect_transaction := nil;
    {FB25_UP}
    end;
end;

function TALFBXBaseLibrary.Load(const lib: AnsiString = GDS32DLL): Boolean;

begin
    Result := Loaded;
    if not Result then
    begin
      FGDS32Lib := LoadLibraryA(PAnsiChar(lib));
      if Loaded then
      begin
        BLOB_close := GetProcAddress(FGDS32Lib, 'BLOB_close');
        BLOB_display := GetProcAddress(FGDS32Lib, 'BLOB_display');
        BLOB_dump := GetProcAddress(FGDS32Lib, 'BLOB_dump');
        BLOB_edit := GetProcAddress(FGDS32Lib, 'BLOB_edit');
        BLOB_get := GetProcAddress(FGDS32Lib, 'BLOB_get');
        BLOB_load := GetProcAddress(FGDS32Lib, 'BLOB_load');
        BLOB_open := GetProcAddress(FGDS32Lib, 'BLOB_open');
        BLOB_put := GetProcAddress(FGDS32Lib, 'BLOB_put');
        BLOB_text_dump := GetProcAddress(FGDS32Lib, 'BLOB_text_dump');
        BLOB_text_load := GetProcAddress(FGDS32Lib, 'BLOB_text_load');
        Bopen := GetProcAddress(FGDS32Lib, 'Bopen');
        isc_add_user := GetProcAddress(FGDS32Lib, 'isc_add_user');
        isc_array_gen_sdl := GetProcAddress(FGDS32Lib, 'isc_array_gen_sdl');
        isc_array_get_slice := GetProcAddress(FGDS32Lib, 'isc_array_get_slice');
        isc_array_lookup_bounds := GetProcAddress(FGDS32Lib, 'isc_array_lookup_bounds');
        isc_array_lookup_desc := GetProcAddress(FGDS32Lib, 'isc_array_lookup_desc');
        isc_array_put_slice := GetProcAddress(FGDS32Lib, 'isc_array_put_slice');
        isc_array_set_desc := GetProcAddress(FGDS32Lib, 'isc_array_set_desc');
        isc_attach_database := GetProcAddress(FGDS32Lib, 'isc_attach_database');
        isc_blob_default_desc := GetProcAddress(FGDS32Lib, 'isc_blob_default_desc');
        isc_blob_gen_bpb := GetProcAddress(FGDS32Lib, 'isc_blob_gen_bpb');
        isc_blob_info := GetProcAddress(FGDS32Lib, 'isc_blob_info');
        isc_blob_lookup_desc := GetProcAddress(FGDS32Lib, 'isc_blob_lookup_desc');
        isc_blob_set_desc := GetProcAddress(FGDS32Lib, 'isc_blob_set_desc');
        isc_cancel_blob := GetProcAddress(FGDS32Lib, 'isc_cancel_blob');
        isc_cancel_events := GetProcAddress(FGDS32Lib, 'isc_cancel_events');
        isc_close := GetProcAddress(FGDS32Lib, 'isc_close');
        isc_close_blob := GetProcAddress(FGDS32Lib, 'isc_close_blob');
        isc_commit_retaining := GetProcAddress(FGDS32Lib, 'isc_commit_retaining');
        isc_commit_transaction := GetProcAddress(FGDS32Lib, 'isc_commit_transaction');
        isc_compile_request := GetProcAddress(FGDS32Lib, 'isc_compile_request');
        isc_compile_request2 := GetProcAddress(FGDS32Lib, 'isc_compile_request2');
        isc_create_blob := GetProcAddress(FGDS32Lib, 'isc_create_blob');
        isc_create_blob2 := GetProcAddress(FGDS32Lib, 'isc_create_blob2');
        isc_create_database := GetProcAddress(FGDS32Lib, 'isc_create_database');
        isc_database_info := GetProcAddress(FGDS32Lib, 'isc_database_info');
        isc_ddl := GetProcAddress(FGDS32Lib, 'isc_ddl');
        isc_declare := GetProcAddress(FGDS32Lib, 'isc_declare');
        isc_decode_date := GetProcAddress(FGDS32Lib, 'isc_decode_date');
        isc_decode_sql_date := GetProcAddress(FGDS32Lib, 'isc_decode_sql_date');
        isc_decode_sql_time := GetProcAddress(FGDS32Lib, 'isc_decode_sql_time');
        isc_decode_timestamp := GetProcAddress(FGDS32Lib, 'isc_decode_timestamp');
        isc_delete_user := GetProcAddress(FGDS32Lib, 'isc_delete_user');
        isc_describe := GetProcAddress(FGDS32Lib, 'isc_describe');
        isc_describe_bind := GetProcAddress(FGDS32Lib, 'isc_describe_bind');
        isc_detach_database := GetProcAddress(FGDS32Lib, 'isc_detach_database');
        isc_drop_database := GetProcAddress(FGDS32Lib, 'isc_drop_database');
        isc_dsql_alloc_statement2 := GetProcAddress(FGDS32Lib, 'isc_dsql_alloc_statement2');
        isc_dsql_allocate_statement := GetProcAddress(FGDS32Lib, 'isc_dsql_allocate_statement');
        isc_dsql_describe := GetProcAddress(FGDS32Lib, 'isc_dsql_describe');
        isc_dsql_describe_bind := GetProcAddress(FGDS32Lib, 'isc_dsql_describe_bind');
        isc_dsql_exec_immed2 := GetProcAddress(FGDS32Lib, 'isc_dsql_exec_immed2');
        isc_dsql_exec_immed3_m := GetProcAddress(FGDS32Lib, 'isc_dsql_exec_immed3_m');
        isc_dsql_execute := GetProcAddress(FGDS32Lib, 'isc_dsql_execute');
        isc_dsql_execute_immediate := GetProcAddress(FGDS32Lib, 'isc_dsql_execute_immediate');
        isc_dsql_execute_immediate_m := GetProcAddress(FGDS32Lib, 'isc_dsql_execute_immediate_m');
        isc_dsql_execute_m := GetProcAddress(FGDS32Lib, 'isc_dsql_execute_m');
        isc_dsql_execute2 := GetProcAddress(FGDS32Lib, 'isc_dsql_execute2');
        isc_dsql_execute2_m := GetProcAddress(FGDS32Lib, 'isc_dsql_execute2_m');
        isc_dsql_fetch := GetProcAddress(FGDS32Lib, 'isc_dsql_fetch');
        isc_dsql_fetch_m := GetProcAddress(FGDS32Lib, 'isc_dsql_fetch_m');
        isc_dsql_finish := GetProcAddress(FGDS32Lib, 'isc_dsql_finish');
        isc_dsql_free_statement := GetProcAddress(FGDS32Lib, 'isc_dsql_free_statement');
        isc_dsql_insert := GetProcAddress(FGDS32Lib, 'isc_dsql_insert');
        isc_dsql_insert_m := GetProcAddress(FGDS32Lib, 'isc_dsql_insert_m');
        isc_dsql_prepare := GetProcAddress(FGDS32Lib, 'isc_dsql_prepare');
        isc_dsql_prepare_m := GetProcAddress(FGDS32Lib, 'isc_dsql_prepare_m');
        isc_dsql_release := GetProcAddress(FGDS32Lib, 'isc_dsql_release');
        isc_dsql_set_cursor_name := GetProcAddress(FGDS32Lib, 'isc_dsql_set_cursor_name');
        isc_dsql_sql_info := GetProcAddress(FGDS32Lib, 'isc_dsql_sql_info');
        isc_embed_dsql_close := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_close');
        isc_embed_dsql_declare := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_declare');
        isc_embed_dsql_describe := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_describe');
        isc_embed_dsql_describe_bind := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_describe_bind');
        isc_embed_dsql_execute := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_execute');
        isc_embed_dsql_execute_immed := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_execute_immed');
        isc_embed_dsql_execute2 := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_execute2');
        isc_embed_dsql_fetch := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_fetch');
        isc_embed_dsql_insert := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_insert');
        isc_embed_dsql_open := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_open');
        isc_embed_dsql_open2 := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_open2');
        isc_embed_dsql_prepare := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_prepare');
        isc_embed_dsql_release := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_release');
        isc_encode_date := GetProcAddress(FGDS32Lib, 'isc_encode_date');
        isc_encode_sql_date := GetProcAddress(FGDS32Lib, 'isc_encode_sql_date');
        isc_encode_sql_time := GetProcAddress(FGDS32Lib, 'isc_encode_sql_time');
        isc_encode_timestamp := GetProcAddress(FGDS32Lib, 'isc_encode_timestamp');
        isc_event_block := GetProcAddress(FGDS32Lib, 'isc_event_block');
        {FB21_UP}
        if Version_api_IS_FB21_UP then begin
          isc_event_block_a := GetProcAddress(FGDS32Lib, 'isc_event_block_a');
        end
        else begin
          isc_event_block_a := nil;
        end;
        {FB21_UP}
        isc_event_counts := GetProcAddress(FGDS32Lib, 'isc_event_counts');
        isc_execute := GetProcAddress(FGDS32Lib, 'isc_execute');
        isc_execute_immediate := GetProcAddress(FGDS32Lib, 'isc_execute_immediate');
        isc_expand_dpb := GetProcAddress(FGDS32Lib, 'isc_expand_dpb');
        isc_fetch := GetProcAddress(FGDS32Lib, 'isc_fetch');
        isc_free := GetProcAddress(FGDS32Lib, 'isc_free');
        isc_ftof := GetProcAddress(FGDS32Lib, 'isc_ftof');
        isc_get_segment := GetProcAddress(FGDS32Lib, 'isc_get_segment');
        isc_get_slice := GetProcAddress(FGDS32Lib, 'isc_get_slice');
        isc_interprete := GetProcAddress(FGDS32Lib, 'isc_interprete');
        isc_modify_dpb := GetProcAddress(FGDS32Lib, 'isc_modify_dpb');
        isc_modify_user := GetProcAddress(FGDS32Lib, 'isc_modify_user');
        isc_open := GetProcAddress(FGDS32Lib, 'isc_open');
        isc_open_blob := GetProcAddress(FGDS32Lib, 'isc_open_blob');
        isc_open_blob2 := GetProcAddress(FGDS32Lib, 'isc_open_blob2');
        isc_portable_integer := GetProcAddress(FGDS32Lib, 'isc_portable_integer');
        isc_prepare := GetProcAddress(FGDS32Lib, 'isc_prepare');
        isc_prepare_transaction := GetProcAddress(FGDS32Lib, 'isc_prepare_transaction');
        isc_prepare_transaction2 := GetProcAddress(FGDS32Lib, 'isc_prepare_transaction2');
        isc_print_blr := GetProcAddress(FGDS32Lib, 'isc_print_blr');
        isc_print_sqlerror := GetProcAddress(FGDS32Lib, 'isc_print_sqlerror');
        isc_print_status := GetProcAddress(FGDS32Lib, 'isc_print_status');
        isc_put_segment := GetProcAddress(FGDS32Lib, 'isc_put_segment');
        isc_put_slice := GetProcAddress(FGDS32Lib, 'isc_put_slice');
        isc_qtoq := GetProcAddress(FGDS32Lib, 'isc_qtoq');
        isc_que_events := GetProcAddress(FGDS32Lib, 'isc_que_events');
        isc_receive := GetProcAddress(FGDS32Lib, 'isc_receive');
        isc_reconnect_transaction := GetProcAddress(FGDS32Lib, 'isc_reconnect_transaction');
        isc_release_request := GetProcAddress(FGDS32Lib, 'isc_release_request');
        isc_request_info := GetProcAddress(FGDS32Lib, 'isc_request_info');
        isc_rollback_retaining := GetProcAddress(FGDS32Lib, 'isc_rollback_retaining');
        isc_rollback_transaction := GetProcAddress(FGDS32Lib, 'isc_rollback_transaction');
        isc_seek_blob := GetProcAddress(FGDS32Lib, 'isc_seek_blob');
        isc_send := GetProcAddress(FGDS32Lib, 'isc_send');
        isc_service_attach := GetProcAddress(FGDS32Lib, 'isc_service_attach');
        isc_service_detach := GetProcAddress(FGDS32Lib, 'isc_service_detach');
        isc_service_query := GetProcAddress(FGDS32Lib, 'isc_service_query');
        isc_service_start := GetProcAddress(FGDS32Lib, 'isc_service_start');
        isc_set_debug := GetProcAddress(FGDS32Lib, 'isc_set_debug');
        isc_sql_interprete := GetProcAddress(FGDS32Lib, 'isc_sql_interprete');
        isc_sqlcode := GetProcAddress(FGDS32Lib, 'isc_sqlcode');
        isc_start_and_send := GetProcAddress(FGDS32Lib, 'isc_start_and_send');
        isc_start_multiple := GetProcAddress(FGDS32Lib, 'isc_start_multiple');
        isc_start_request := GetProcAddress(FGDS32Lib, 'isc_start_request');
        isc_start_transaction := GetProcAddress(FGDS32Lib, 'isc_start_transaction');
        isc_transact_request := GetProcAddress(FGDS32Lib, 'isc_transact_request');
        isc_transaction_info := GetProcAddress(FGDS32Lib, 'isc_transaction_info');
        isc_unwind_request := GetProcAddress(FGDS32Lib, 'isc_unwind_request');
        isc_vax_integer := GetProcAddress(FGDS32Lib, 'isc_vax_integer');
        isc_version := GetProcAddress(FGDS32Lib, 'isc_version');
        isc_vtof := GetProcAddress(FGDS32Lib, 'isc_vtof');
        isc_vtov := GetProcAddress(FGDS32Lib, 'isc_vtov');
        isc_wait_for_event := GetProcAddress(FGDS32Lib, 'isc_wait_for_event');

      {FB15_UP}
        if Version_api_IS_FB15_UP then begin
          isc_reset_fpe := GetProcAddress(FGDS32Lib, 'isc_reset_fpe');
          isc_get_client_version := GetProcAddress(FGDS32Lib, 'isc_get_client_version');
          isc_get_client_major_version := GetProcAddress(FGDS32Lib, 'isc_get_client_major_version');
          isc_get_client_minor_version := GetProcAddress(FGDS32Lib, 'isc_get_client_minor_version');
        end
        else begin
          isc_reset_fpe := nil;
          isc_get_client_version := nil;
          isc_get_client_major_version := nil;
          isc_get_client_minor_version := nil;
        end;
      {FB15_UP}
      {FB20_UP}
        if Version_api_IS_FB20_UP then begin
          fb_interpret := GetProcAddress(FGDS32Lib, 'fb_interpret');
        end
        else begin
          fb_interpret := nil;
        end;
      {FB20_UP}

      {FB25_UP}
        if Version_api_IS_FB25_UP then begin
          fb_print_blr := GetProcAddress(FGDS32Lib, 'fb_print_blr');
          fb_shutdown := GetProcAddress(FGDS32Lib, 'fb_shutdown');
          fb_shutdown_callback := GetProcAddress(FGDS32Lib, 'fb_shutdown_callback');
          fb_cancel_operation := GetProcAddress(FGDS32Lib, 'fb_cancel_operation');
          fb_sqlstate := GetProcAddress(FGDS32Lib, 'fb_sqlstate');
          fb_disconnect_transaction := GetProcAddress(FGDS32Lib, 'fb_disconnect_transaction');
        end
        else begin
          fb_print_blr := nil;
          fb_shutdown := nil;
          fb_shutdown_callback := nil;
          fb_cancel_operation := nil;
          fb_sqlstate := nil;
          fb_disconnect_transaction := nil;
        end;
      {FB25_UP}


        Result := Assigned(BLOB_close) and Assigned(BLOB_dump) and
          Assigned(BLOB_edit) and Assigned(BLOB_get) and Assigned(BLOB_load) and
          Assigned(BLOB_open) and Assigned(BLOB_put) and Assigned(BLOB_text_dump) and
          Assigned(BLOB_text_load) and Assigned(Bopen) and Assigned(isc_add_user) and
          Assigned(isc_array_gen_sdl) and Assigned(isc_array_get_slice) and
          Assigned(isc_array_lookup_bounds) and Assigned(isc_array_lookup_desc) and
          Assigned(isc_array_put_slice) and Assigned(isc_array_set_desc) and
          Assigned(isc_attach_database) and Assigned(isc_blob_default_desc) and
          Assigned(isc_blob_gen_bpb) and Assigned(isc_blob_info) and
          Assigned(isc_blob_lookup_desc) and Assigned(isc_blob_set_desc) and
          Assigned(isc_cancel_blob) and Assigned(isc_cancel_events) and Assigned(isc_close) and
          Assigned(isc_close_blob) and Assigned(isc_commit_retaining) and
          Assigned(isc_commit_transaction) and Assigned(isc_compile_request) and
          Assigned(isc_compile_request2) and Assigned(isc_create_blob) and
          Assigned(isc_create_blob2) and Assigned(isc_create_database) and
          Assigned(isc_database_info) and Assigned(isc_ddl) and Assigned(isc_declare) and
          Assigned(isc_decode_date) and Assigned(isc_decode_sql_date) and
          Assigned(isc_decode_sql_time) and Assigned(isc_decode_timestamp) and
          Assigned(isc_delete_user) and Assigned(isc_describe) and Assigned(isc_describe_bind) and
          Assigned(isc_detach_database) and Assigned(isc_drop_database) and
          Assigned(isc_dsql_alloc_statement2) and Assigned(isc_dsql_allocate_statement) and
          Assigned(isc_dsql_describe) and Assigned(isc_dsql_describe_bind) and
          Assigned(isc_dsql_exec_immed2) and Assigned(isc_dsql_exec_immed3_m) and
          Assigned(isc_dsql_execute) and Assigned(isc_dsql_execute_immediate) and
          Assigned(isc_dsql_execute_immediate_m) and Assigned(isc_dsql_execute_m) and
          Assigned(isc_dsql_execute2) and Assigned(isc_dsql_execute2_m) and
          Assigned(isc_dsql_fetch) and Assigned(isc_dsql_fetch_m) and
          Assigned(isc_dsql_finish) and Assigned(isc_dsql_free_statement) and
          Assigned(isc_dsql_insert) and Assigned(isc_dsql_insert_m) and
          Assigned(isc_dsql_prepare) and Assigned(isc_dsql_prepare_m) and
          Assigned(isc_dsql_release) and Assigned(isc_dsql_set_cursor_name) and
          Assigned(isc_dsql_sql_info) and Assigned(isc_embed_dsql_close) and
          Assigned(isc_embed_dsql_declare) and Assigned(isc_embed_dsql_describe) and
          Assigned(isc_embed_dsql_describe_bind) and Assigned(isc_embed_dsql_execute) and
          Assigned(isc_embed_dsql_execute_immed) and Assigned(isc_embed_dsql_execute2) and
          Assigned(isc_embed_dsql_fetch) and Assigned(isc_embed_dsql_insert) and
          Assigned(isc_embed_dsql_open) and Assigned(isc_embed_dsql_open2) and
          Assigned(isc_embed_dsql_prepare) and Assigned(isc_embed_dsql_release) and
          Assigned(isc_encode_date) and Assigned(isc_encode_sql_date) and
          Assigned(isc_encode_sql_time) and Assigned(isc_encode_timestamp) and
          Assigned(isc_event_block)
          {FB21_UP}
          and ((not Version_api_IS_FB21_UP) or Assigned(isc_event_block_a))
          {FB21_UP}
          and Assigned(isc_event_counts) and Assigned(isc_execute) and
          Assigned(isc_execute_immediate) and Assigned(isc_expand_dpb) and
          Assigned(isc_free) and Assigned(isc_ftof) and Assigned(isc_get_segment) and
          Assigned(isc_fetch) and Assigned(isc_get_slice) and Assigned(isc_interprete) and
          Assigned(isc_modify_dpb) and Assigned(isc_modify_user) and Assigned(isc_open) and
          Assigned(isc_open_blob) and Assigned(isc_open_blob2) and Assigned(isc_portable_integer) and
          Assigned(isc_prepare) and Assigned(isc_prepare_transaction) and
          Assigned(isc_prepare_transaction2) and Assigned(isc_print_blr) and
          Assigned(isc_print_sqlerror) and Assigned(isc_print_status) and Assigned(isc_put_segment) and
          Assigned(isc_put_slice) and Assigned(isc_qtoq) and Assigned(isc_que_events) and
          Assigned(isc_receive) and Assigned(isc_reconnect_transaction) and
          Assigned(isc_release_request) and Assigned(isc_request_info) and
          Assigned(isc_rollback_retaining) and Assigned(isc_rollback_transaction) and
          Assigned(isc_seek_blob) and Assigned(isc_send) and Assigned(isc_service_attach) and
          Assigned(isc_service_detach) and Assigned(isc_service_query) and
          Assigned(isc_service_start) and Assigned(isc_sql_interprete) and
          Assigned(isc_sqlcode) and Assigned(isc_start_and_send) and Assigned(isc_start_multiple) and
          Assigned(isc_start_request) and Assigned(isc_start_transaction) and
          Assigned(isc_transact_request) and Assigned(isc_transaction_info) and
          Assigned(isc_unwind_request) and Assigned(isc_vax_integer) and
          Assigned(isc_version) and Assigned(isc_vtof) and Assigned(isc_vtov) and
          Assigned(isc_wait_for_event)
          and Assigned(isc_set_debug) and Assigned(BLOB_display)
        {FB15_UP}
        and (
             (not Version_api_IS_FB15_UP) or
             (
              Assigned(isc_reset_fpe)
              and Assigned(isc_get_client_version) and Assigned(isc_get_client_major_version)
              and Assigned(isc_get_client_minor_version)
             )
            )
        {FB15_UP}
        {FB20_UP}
        and (
             (not Version_api_IS_FB20_UP) or
             assigned(fb_interpret)
            )
        {FB20_UP}
        {FB25_UP}
        and (
             (not Version_api_IS_FB25_UP) or
             (
              assigned(fb_print_blr)
              and assigned(fb_shutdown)
              and assigned(fb_shutdown_callback)
              and assigned(fb_cancel_operation)
              and assigned(fb_sqlstate)
              and assigned(fb_disconnect_transaction)
             )
            )
        {FB25_UP}
        ;

        if not Result then
        begin
          Unload;
          raise Exception.Create(cALFBX_INVALIDEIBVERSION);
        end;
      end
      else
        raise Exception.CreateFmt(cALFBX_CANTLOADLIB, [lib]);
    end;
end;

destructor TALFBXBaseLibrary.Destroy;
begin
  Unload;
  inherited Destroy;
end;

end.
