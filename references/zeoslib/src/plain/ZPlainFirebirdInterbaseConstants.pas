{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase and Firebird Common constants         }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZPlainFirebirdInterbaseConstants;

interface

{$I ZPlain.inc}

uses 
   ZCompatibility; 

const
  IBLocalBufferLength = 512;
  IBBigLocalBufferLength = IBLocalBufferLength * 2;
  IBHugeLocalBufferLength = IBBigLocalBufferLength * 20;

  ISC_NULL = -1;
  ISC_NOTNULL = 0;

  ISC_TRUE                      = 1;
  ISC_FALSE                     = 0;
  DSQL_CLOSE                    = 1;
  DSQL_DROP                     = 2;
  DSQL_UNPREPARE                = 4;


  SQLDA_VERSION1                = 1;
  SQLDA_VERSION2                = 2;
  SQL_DIALECT_V5                = 1;
  SQL_DIALECT_V6                = 3;
  SQL_DIALECT_CURRENT = SQL_DIALECT_V6; (* latest IB DIALECT *)

  { SQL definitions }
  SQL_VARYING                    = 448;
  SQL_TEXT                       = 452;
  SQL_DOUBLE                     = 480;
  SQL_FLOAT                      = 482;
  SQL_LONG                       = 496;
  SQL_SHORT                      = 500;
  SQL_TIMESTAMP                  = 510;
  SQL_BLOB                       = 520;
  SQL_D_FLOAT                    = 530;
  SQL_ARRAY                      = 540;
  SQL_QUAD                       = 550;
  SQL_TYPE_TIME                  = 560;
  SQL_TYPE_DATE                  = 570;
  SQL_INT64                      = 580;
  SQL_BOOLEAN                    = 590;
  SQL_DATE                       = SQL_TIMESTAMP;

  { SQL types definitions from RDB$FIELDS}
  RDB_VARCHAR = 37;
  RDB_VARCHAR2 = 38;
  RDB_CSTRING = 40;
  RDB_CSTRING2 = 41;
  RDB_CHAR = 14;
  RDB_CHAR2 = 15;
  RDB_D_FLOAT = 11;
  RDB_DOUBLE = 27;
  RDB_FLOAT = 10;
  RDB_INT64 = 16;
  RDB_QUAD = 9;
  RDB_BLOB_ID = 45;
  RDB_INTEGER = 8;
  RDB_SMALLINT = 7;
  RDB_DATE = 12;
  RDB_TIME = 13;
  RDB_TIMESTAMP = 35;
  RDB_BOOLEAN = 17;
  RDB_BLOB = 261;
  { SQL subtypes definitions from RDB$FIELDS}
  CS_NONE = 0;
  CS_BINARY = 1;
  CS_ASCII = 2;
  CS_UNICODE_FSS = 3;
  CS_METADATA = CS_UNICODE_FSS;

  RDB_BLOB_NONE = 0;
  RDB_BLOB_TEXT = 1;
  RDB_BLOB_BLR = 2;
  RDB_BLOB_ACL = 3;
  RDB_BLOB_RESERVED = 4;
  RDB_BLOB_ENCODED = 5;
  RDB_BLOB_DESCRIPTION = 6;
  RDB_NUMBERS_NONE = 0;
  RDB_NUMBERS_NUMERIC = 1;
  RDB_NUMBERS_DECIMAL = 2;

  { Blob Subtypes }
  { types less than zero are reserved for customer use }
  isc_blob_untyped               = 0;

  { internal subtypes }
  isc_blob_text                  = 1;
  isc_blob_blr                   = 2;
  isc_blob_acl                   = 3;
  isc_blob_ranges                = 4;
  isc_blob_summary               = 5;
  isc_blob_format                = 6;
  isc_blob_tra                   = 7;
  isc_blob_extfile               = 8;

  { the range 20-30 is reserved for dBASE and Paradox types }
  isc_blob_formatted_memo        = 20;
  isc_blob_paradox_ole           = 21;
  isc_blob_graphic               = 22;
  isc_blob_dbase_ole             = 23;
  isc_blob_typed_binary          = 24;

  {* Blob information items *}
  isc_info_blob_num_segments = 4;
  isc_info_blob_max_segment = 5;
  isc_info_blob_total_length = 6;
  isc_info_blob_type = 7;

  {* error codes *}
  isc_segment                    =  335544366;
  isc_segstr_eof                 =  335544367;
  isc_lock_conflict              =  335544345;

  { Database parameter block stuff }
  isc_dpb_version1               = 1;
  isc_dpb_cdd_pathname           = 1;
  isc_dpb_allocation             = 2;
  isc_dpb_journal                = 3;
  isc_dpb_page_size              = 4;
  isc_dpb_num_buffers            = 5;
  isc_dpb_buffer_length          = 6;
  isc_dpb_debug                  = 7;
  isc_dpb_garbage_collect        = 8;
  isc_dpb_verify                 = 9;
  isc_dpb_sweep                  = 10;
  isc_dpb_enable_journal         = 11;
  isc_dpb_disable_journal        = 12;
  isc_dpb_dbkey_scope            = 13;
  isc_dpb_number_of_users        = 14;
  isc_dpb_trace                  = 15;
  isc_dpb_no_garbage_collect     = 16;
  isc_dpb_damaged                = 17;
  isc_dpb_license                = 18;
  isc_dpb_sys_user_name          = 19;
  isc_dpb_encrypt_key            = 20;
  isc_dpb_activate_shadow        = 21;
  isc_dpb_sweep_interval         = 22;
  isc_dpb_delete_shadow          = 23;
  isc_dpb_force_write            = 24;
  isc_dpb_begin_log              = 25;
  isc_dpb_quit_log               = 26;
  isc_dpb_no_reserve             = 27;
  isc_dpb_user_name              = 28;
  isc_dpb_password               = 29;
  isc_dpb_password_enc           = 30;
  isc_dpb_sys_user_name_enc      = 31;
  isc_dpb_interp                 = 32;
  isc_dpb_online_dump            = 33;
  isc_dpb_old_file_size          = 34;
  isc_dpb_old_num_files          = 35;
  isc_dpb_old_file               = 36;
  isc_dpb_old_start_page         = 37;
  isc_dpb_old_start_seqno        = 38;
  isc_dpb_old_start_file         = 39;
  isc_dpb_drop_walfile           = 40;
  isc_dpb_old_dump_id            = 41;
  isc_dpb_wal_backup_dir         = 42;
  isc_dpb_wal_chkptlen           = 43;
  isc_dpb_wal_numbufs            = 44;
  isc_dpb_wal_bufsize            = 45;
  isc_dpb_wal_grp_cmt_wait       = 46;
  isc_dpb_lc_messages            = 47;
  isc_dpb_lc_ctype               = 48;
  isc_dpb_cache_manager          = 49;
  isc_dpb_shutdown               = 50;
  isc_dpb_online                 = 51;
  isc_dpb_shutdown_delay         = 52;
  isc_dpb_reserved               = 53;
  isc_dpb_overwrite              = 54;
  isc_dpb_sec_attach             = 55;
  isc_dpb_disable_wal            = 56;
  isc_dpb_connect_timeout        = 57;
  isc_dpb_dummy_packet_interval  = 58;
  isc_dpb_gbak_attach            = 59;
  isc_dpb_sql_role_name          = 60;
  isc_dpb_set_page_buffers       = 61;
  isc_dpb_working_directory      = 62;
  isc_dpb_SQL_dialect            = 63;
  isc_dpb_set_db_readonly        = 64;
  isc_dpb_set_db_SQL_dialect     = 65;
  isc_dpb_gfix_attach            = 66;
  isc_dpb_gstat_attach           = 67;
  isc_dpb_last_dpb_constant      = isc_dpb_gstat_attach;

  { isc_dpb_verify specific flags }
  isc_dpb_pages                  = 1;
  isc_dpb_records                = 2;
  isc_dpb_indices                = 4;
  isc_dpb_transactions           = 8;
  isc_dpb_no_update              = 16;
  isc_dpb_repair                 = 32;
  isc_dpb_ignore                 = 64;

  { isc_dpb_shutdown specific flags }
  isc_dpb_shut_cache             = 1;
  isc_dpb_shut_attachment        = 2;
  isc_dpb_shut_transaction       = 4;
  isc_dpb_shut_force             = 8;

  { Transaction parameter block stuff }
  isc_tpb_version1               = 1;
  isc_tpb_version3               = 3;
  isc_tpb_consistency            = 1;
  isc_tpb_concurrency            = 2;
  isc_tpb_shared                 = 3;
  isc_tpb_protected              = 4;
  isc_tpb_exclusive              = 5;
  isc_tpb_wait                   = 6;
  isc_tpb_nowait                 = 7;
  isc_tpb_read                   = 8;
  isc_tpb_write                  = 9;
  isc_tpb_lock_read              = 10;
  isc_tpb_lock_write             = 11;
  isc_tpb_verb_time              = 12;
  isc_tpb_commit_time            = 13;
  isc_tpb_ignore_limbo           = 14;
  isc_tpb_read_committed         = 15;
  isc_tpb_autocommit             = 16;
  isc_tpb_rec_version            = 17;
  isc_tpb_no_rec_version         = 18;
  isc_tpb_restart_requests       = 19;
  isc_tpb_no_auto_undo           = 20;
  isc_tpb_last_tpb_constant      = isc_tpb_no_auto_undo;

  { Blob Parameter Block }
  isc_bpb_version1               = 1;
  isc_bpb_source_type            = 1;
  isc_bpb_target_type            = 2;
  isc_bpb_type                   = 3;
  isc_bpb_source_interp          = 4;
  isc_bpb_target_interp          = 5;
  isc_bpb_filter_parameter       = 6;

  { SQL information items }
  isc_info_sql_select            = 4;
  isc_info_sql_bind              = 5;
  isc_info_sql_num_variables     = 6;
  isc_info_sql_describe_vars     = 7;
  isc_info_sql_describe_end      = 8;
  isc_info_sql_sqlda_seq         = 9;
  isc_info_sql_message_seq       = 10;
  isc_info_sql_type              = 11;
  isc_info_sql_sub_type          = 12;
  isc_info_sql_scale             = 13;
  isc_info_sql_length            = 14;
  isc_info_sql_null_ind          = 15;
  isc_info_sql_field             = 16;
  isc_info_sql_relation          = 17;
  isc_info_sql_owner             = 18;
  isc_info_sql_alias             = 19;
  isc_info_sql_sqlda_start       = 20;
  isc_info_sql_stmt_type         = 21;
  isc_info_sql_get_plan          = 22;
  isc_info_sql_records           = 23;
  isc_info_sql_batch_fetch       = 24;

  { SQL information return values }
  isc_info_sql_stmt_select         = 1;
  isc_info_sql_stmt_insert         = 2;
  isc_info_sql_stmt_update         = 3;
  isc_info_sql_stmt_delete         = 4;
  isc_info_sql_stmt_ddl            = 5;
  isc_info_sql_stmt_get_segment    = 6;
  isc_info_sql_stmt_put_segment    = 7;
  isc_info_sql_stmt_exec_procedure = 8;
  isc_info_sql_stmt_start_trans    = 9;
  isc_info_sql_stmt_commit         = 10;
  isc_info_sql_stmt_rollback       = 11;
  isc_info_sql_stmt_select_for_upd = 12;
  isc_info_sql_stmt_set_generator  = 13;

  isc_bpb_type_segmented           = 0;
  isc_bpb_type_stream              = 1;

  {************** Information call declarations **************}

  { Common, structural codes }
  isc_info_end                     = 1;
  isc_info_truncated               = 2;
  isc_info_error                   = 3;
  isc_info_data_not_ready          = 4;
  isc_info_flag_end                = 127;

  { Request information items }
  isc_info_number_messages         = 4;
  isc_info_max_message             = 5;
  isc_info_max_send                = 6;
  isc_info_max_receive             = 7;
  isc_info_state                   = 8;
  isc_info_message_number          = 9;
  isc_info_message_size            = 10;
  isc_info_request_cost            = 11;
  isc_info_access_path             = 12;
  isc_info_req_select_count        = 13;
  isc_info_req_insert_count        = 14;
  isc_info_req_update_count        = 15;
  isc_info_req_delete_count        = 16;

  { Database information items }
  isc_info_db_id                 =          4;
  isc_info_reads                 =          5;
  isc_info_writes                =          6;
  isc_info_fetches               =          7;
  isc_info_marks                 =          8;
  isc_info_implementation        =         11;
  isc_info_version               =         12;
  isc_info_base_level            =         13;
  isc_info_page_size             =         14;
  isc_info_num_buffers           =         15;
  isc_info_limbo                 =         16;
  isc_info_current_memory        =         17;
  isc_info_max_memory            =         18;
  isc_info_window_turns          =         19;
  isc_info_license               =         20;
  isc_info_allocation            =         21;
  isc_info_attachment_id         =         22;
  isc_info_read_seq_count        =         23;
  isc_info_read_idx_count        =         24;
  isc_info_insert_count          =         25;
  isc_info_update_count          =         26;
  isc_info_delete_count          =         27;
  isc_info_backout_count         =         28;
  isc_info_purge_count           =         29;
  isc_info_expunge_count         =         30;
  isc_info_sweep_interval        =         31;
  isc_info_ods_version           =         32;
  isc_info_ods_minor_version     =         33;
  isc_info_no_reserve            =         34;
  isc_info_logfile               =         35;
  isc_info_cur_logfile_name      =         36;
  isc_info_cur_log_part_offset   =         37;
  isc_info_num_wal_buffers       =         38;
  isc_info_wal_buffer_size       =         39;
  isc_info_wal_ckpt_length       =         40;
  isc_info_wal_cur_ckpt_interval =         41;
  isc_info_wal_prv_ckpt_fname    =         42;
  isc_info_wal_prv_ckpt_poffset  =         43;
  isc_info_wal_recv_ckpt_fname   =         44;
  isc_info_wal_recv_ckpt_poffset =         45;
  isc_info_wal_grpc_wait_usecs   =         47;
  isc_info_wal_num_io            =         48;
  isc_info_wal_avg_io_size       =         49;
  isc_info_wal_num_commits       =         50;
  isc_info_wal_avg_grpc_size     =         51;
  isc_info_forced_writes         =         52;
  isc_info_user_names            =         53;
  isc_info_page_errors           =         54;
  isc_info_record_errors         =         55;
  isc_info_bpage_errors          =         56;
  isc_info_dpage_errors          =         57;
  isc_info_ipage_errors          =         58;
  isc_info_ppage_errors          =         59;
  isc_info_tpage_errors          =         60;
  isc_info_set_page_buffers      =         61;
  isc_info_db_SQL_dialect        =         62;
  isc_info_db_read_only          =         63;
  isc_info_db_size_in_pages      =         64;

  frb_info_att_charset           = 101;
  isc_info_db_class              = 102;
  isc_info_firebird_version      = 103;
  isc_info_oldest_transaction    = 104;
  isc_info_oldest_active         = 105;
  isc_info_oldest_snapshot       = 106;
  isc_info_next_transaction      = 107;
  isc_info_db_provider           = 108;
  isc_info_active_transactions   = 109;
  isc_info_active_tran_count     = 110;
  isc_info_creation_date         = 111;
  isc_info_db_file_size          = 112;

type
  ULong                = Cardinal;
  UChar                = AnsiChar;
  Short                = SmallInt;

  ISC_LONG             = LongInt;
  UISC_LONG            = ULong;
  ISC_INT64            = Int64;
  ISC_STATUS           = LongInt;
  UISC_STATUS          = ULong;
  PISC_LONG            = ^ISC_LONG;
  PUISC_LONG           = ^UISC_LONG;
  PISC_STATUS          = ^ISC_STATUS;
  PPISC_STATUS         = ^PISC_STATUS;
  PUISC_STATUS         = ^UISC_STATUS;
  PShort               = ^Short;
  PPAnsiChar               = ^PAnsiChar;
  UShort               = Word;
  PVoid                = Pointer;

  { C Date/Time Structure }
  TCTimeStructure = record
    tm_sec:        Integer;   { Seconds }
    tm_min:        Integer;   { Minutes }
    tm_hour:       Integer;   { Hour (0--23) }
    tm_mday:       Integer;   { Day of month (1--31) }
    tm_mon:        Integer;   { Month (0--11) }
    tm_year:       Integer;   { Year (calendar year minus 1900) }
    tm_wday:       Integer;   { Weekday (0--6) Sunday = 0) }
    tm_yday:       Integer;   { Day of year (0--365) }
    tm_isdst:      Integer;   { 0 if daylight savings time is not in effect) }
  end;
  PCTimeStructure = ^TCTimeStructure;
  TM = TCTimeStructure;
  PTM = ^TM;

  TISC_VARYING = record
    strlen:       Short;
    str:          array[0..0] of AnsiChar;
  end;
  PISC_VARYING = ^TISC_VARYING;

  { InterBase Handle Definitions }
  TISC_BLOB_HANDLE              = PVoid;
  PISC_BLOB_HANDLE              = ^TISC_BLOB_HANDLE;
  TISC_DB_HANDLE                = PVoid;
  PISC_DB_HANDLE                = ^TISC_DB_HANDLE;
  TISC_STMT_HANDLE              = PVoid;
  PISC_STMT_HANDLE              = ^TISC_STMT_HANDLE;
  TISC_TR_HANDLE                = PVoid;
  PISC_TR_HANDLE                = ^TISC_TR_HANDLE;
  TISC_CALLBACK                 = procedure;

  { Time & Date Support }
  ISC_DATE = LongInt;
  PISC_DATE = ^ISC_DATE;
  ISC_TIME = ULong;
  PISC_TIME = ^ISC_TIME;

  TISC_TIMESTAMP = record
    timestamp_date: ISC_DATE; 
    timestamp_time: ISC_TIME;
  end;
  PISC_TIMESTAMP = ^TISC_TIMESTAMP;

  { Blob id structure }
  TGDS_QUAD = record
    gds_quad_high:  ISC_LONG;
    gds_quad_low:   UISC_LONG;
  end;
  PGDS_QUAD            = ^TGDS_QUAD;

  TISC_QUAD            = TGDS_QUAD;
  PISC_QUAD            = ^TISC_QUAD;

  TISC_ARRAY_BOUND = record
    array_bound_lower:  Short;
    array_bound_upper:  Short;
  end;
  PISC_ARRAY_BOUND = ^TISC_ARRAY_BOUND;

  TISC_ARRAY_DESC = record
    array_desc_dtype:   UChar;
    array_desc_scale:   AnsiChar;
    array_desc_length:  Short;
    array_desc_field_name: array[0..31] of AnsiChar;
    array_desc_relation_name: array[0..31] of AnsiChar;
    array_desc_dimensions: Short;
    array_desc_flags: Short;
    array_desc_bounds: array[0..15] of TISC_ARRAY_BOUND;
  end;
  PISC_ARRAY_DESC = ^TISC_ARRAY_DESC;

  TISC_BLOB_DESC = record
    blob_desc_subtype:          Short;
    blob_desc_charset:          Short;
    blob_desc_segment_size:     Short;
    blob_desc_field_name:       array[0..31] of UChar;
    blob_desc_relation_name:    array[0..31] of UChar;
  end;
  PISC_BLOB_DESC = ^TISC_BLOB_DESC;

  { Declare the extended SQLDA }
  TXSQLVAR = record
    sqltype:            Short;     { datatype of field }
    sqlscale:           Short;     { scale factor }
    sqlsubtype:         Short;     { datatype subtype - BLOBs }
			           { & text types only }
    sqllen:             Short;     { length of data area }
    sqldata:            PAnsiChar;     { address of data }
    sqlind:             PSmallInt;  { address of indicator } 
                                   { variable }
    sqlname_length:     Short;     { length of sqlname field }
    { name of field, name length + space for NULL }
    sqlname:            array[0..31] of AnsiChar;
    relname_length:     Short;     { length of relation name }
    { field's relation name + space for NULL }
    relname:            array[0..31] of AnsiChar;
    ownname_length:     Short;     { length of owner name }
    { relation's owner name + space for NULL }
    ownname:            array[0..31] of AnsiChar;
    aliasname_length:   Short;     { length of alias name }
    { relation's alias name + space for NULL }
    aliasname:          array[0..31] of AnsiChar;
  end;
  PXSQLVAR = ^TXSQLVAR;

  TXSQLDA = record
    version:            Short;     { version of this XSQLDA }
    { XSQLDA name field }
    sqldaid:            array[0..7] of AnsiChar;
    sqldabc:            ISC_LONG;  { length in bytes of SQLDA }
    sqln:               Short;     { number of fields allocated }
    sqld:               Short;     { actual number of fields }
    { first field address }
    sqlvar:             array[0..0] of TXSQLVAR;
  end;
  PXSQLDA = ^TXSQLDA;

 {****************************************************}
 { This record type is for passing arguments to       }
 { isc_start_transaction (See docs)                   }
 {****************************************************}
  TISC_START_TRANS = record
    db_handle:          PISC_DB_HANDLE;
    tpb_length:         Word;
    tpb_address:        PAnsiChar;
  end;

 {****************************************************}
 { This record type is for passing arguments to       }
 { isc_start_multiple (see docs)                      }
 {****************************************************}
  TISC_TEB = record
    db_handle:          PISC_DB_HANDLE;
    tpb_length:         LongInt;
    tpb_address:        PAnsiChar;
  end;
  PISC_TEB = ^TISC_TEB;
  TISC_TEB_ARRAY = array[0..0] of TISC_TEB;
  PISC_TEB_ARRAY = ^TISC_TEB_ARRAY;

  { Interbase status array }
  PARRAY_ISC_STATUS = ^TARRAY_ISC_STATUS;
  TARRAY_ISC_STATUS = array[0..20] of ISC_STATUS;

{ ************** Plain API Function types definition ************* }

  { General database routines }

  Tisc_attach_database = function(status_vector: PISC_STATUS;
    db_name_length: Short; db_name: PAnsiChar; db_handle: PISC_DB_HANDLE;
    parm_buffer_length: Short; parm_buffer: PAnsiChar): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_detach_database = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_drop_database = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_database_info = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short;
    item_list_buffer: PAnsiChar; result_buffer_length: Short;
    result_buffer: PAnsiChar): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Array processing routines }
  Tisc_array_gen_sdl = function(status_vector: PISC_STATUS;
    isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort;
    isc_arg4: PAnsiChar; isc_arg5: PShort): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_get_slice = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
    array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
    dest_array: PVoid; slice_length: ISC_LONG): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_lookup_bounds = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
    table_name, column_name: PAnsiChar;
    descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_lookup_desc = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
    table_name, column_name: PAnsiChar;
    descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_set_desc = function(status_vector: PISC_STATUS;
    table_name: PAnsiChar; column_name: PAnsiChar;
    sql_dtype, sql_length, sql_dimensions: PShort;
    descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_put_slice = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
    array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
    source_array: PVoid; slice_length: PISC_LONG): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_free = function(isc_arg1: PAnsiChar): ISC_LONG;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_sqlcode = function(status_vector: PISC_STATUS): ISC_LONG;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_sql_interprete = procedure(sqlcode: Short; buffer: PAnsiChar;
    buffer_length: Short); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_interprete = function(buffer: PAnsiChar; status_vector: PPISC_STATUS):
    ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tfb_interpret = function(buffer: PAnsiChar;  bufsize: integer; status_vector: PPISC_STATUS):
    ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Transaction support routines }

  Tisc_start_transaction = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
    db_handle: PISC_DB_HANDLE; tpb_length: Word; tpb_address: PAnsiChar):
    ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_start_multiple = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
    teb_vector_address: PISC_TEB): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_rollback_transaction = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_rollback_retaining = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_commit_retaining = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_commit_transaction = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_transaction_info = function(status_vector: PISC_STATUS;
    tr_handle: PISC_TR_HANDLE; item_list_buffer_length: Short;
    item_list_buffer: PAnsiChar; result_buffer_length: Short;
    result_buffer: PAnsiChar): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Dynamic SQL routines }

  Tisc_dsql_allocate_statement = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_alloc_statement2 = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_describe = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_describe_bind = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_execute = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
    xsqlda: PXSQLDA): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_execute2 = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
    in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_execute_immediate = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: Word;
    statement: PAnsiChar; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_fetch = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_free_statement = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; options: Word): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_prepare = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
    length: Word; statement: PAnsiChar; dialect: Word; xsqlda: PXSQLDA):
    ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_set_cursor_name = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; cursor_name: PAnsiChar; _type: Word): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_sql_info = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; item_length: Short; items: PAnsiChar;
    buffer_length: Short; buffer: PAnsiChar): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Blob processing routines }

  Tisc_open_blob2 = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
    blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
    bpb_buffer: PAnsiChar): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_create_blob2 = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
    blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
    bpb_address: PAnsiChar): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_blob_info = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short;
    item_list_buffer: PAnsiChar; result_buffer_length: Short; result_buffer: PAnsiChar):
    ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_close_blob = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_cancel_blob = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_get_segment = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE; actual_seg_length: PWord;
    seg_buffer_length: Word; seg_buffer: PAnsiChar): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_put_segment = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE; seg_buffer_len: Word; seg_buffer: PAnsiChar):
    ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Event processing routines }

  Tisc_event_block = function(event_buffer: PPAnsiChar; result_buffer: PPAnsiChar;
    id_count: Word; event_list: array of PAnsiChar): ISC_LONG;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_event_counts = procedure(status_vector: PISC_STATUS;
    buffer_length: Short; event_buffer: PAnsiChar; result_buffer: PAnsiChar);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_cancel_events = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_que_events = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short;
    event_buffer: PAnsiChar; event_function: TISC_CALLBACK;
    event_function_arg: PVoid): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Types convertion routines }

  Tisc_decode_date = procedure(ib_date: PISC_QUAD; tm_date: PCTimeStructure);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_encode_date = procedure(tm_date: PCTimeStructure; ib_date: PISC_QUAD);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Interbase Version 6 routines }
  Tisc_decode_sql_date = procedure(ib_date: PISC_DATE;
    tm_date: PCTimeStructure); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_decode_sql_time = procedure(ib_time: PISC_TIME;
    tm_date: PCTimeStructure); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_decode_timestamp = procedure(ib_timestamp: PISC_TIMESTAMP;
    tm_date: PCTimeStructure); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_encode_sql_date = procedure(tm_date: PCTimeStructure;
    ib_date: PISC_DATE); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_encode_sql_time = procedure(tm_date: PCTimeStructure;
    ib_time: PISC_TIME); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_encode_timestamp = procedure(tm_date: PCTimeStructure;
    ib_timestamp: PISC_TIMESTAMP);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_vax_integer = function(buffer: PAnsiChar; length: Short): ISC_LONG;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

{ ************** Collection of Plain API Function types definition ************* }
TZFirebird_API = record
  { General database routines }
  isc_attach_database:  Tisc_attach_database;
  isc_detach_database:  Tisc_detach_database;
  isc_drop_database:    Tisc_drop_database;
  isc_database_info:    Tisc_database_info;
  isc_free:             Tisc_free;
  isc_sqlcode:          Tisc_sqlcode;
  isc_sql_interprete:   Tisc_sql_interprete;
  isc_interprete:       Tisc_interprete;
  fb_interpret:         Tfb_interpret;

  { Transaction support routines }
  isc_start_transaction: Tisc_start_transaction;
  isc_start_multiple:   Tisc_start_multiple;
  isc_rollback_transaction: Tisc_rollback_transaction;
  isc_rollback_retaining: Tisc_rollback_retaining;
  isc_commit_transaction: Tisc_commit_transaction;
  isc_commit_retaining: Tisc_commit_retaining;
  isc_transaction_info: Tisc_transaction_info;

  { Dynamic SQL routines }
  isc_dsql_allocate_statement: Tisc_dsql_allocate_statement;
  isc_dsql_alloc_statement2: Tisc_dsql_alloc_statement2;
  isc_dsql_describe:    Tisc_dsql_describe;
  isc_dsql_describe_bind: Tisc_dsql_describe_bind;
  isc_dsql_execute:     Tisc_dsql_execute;
  isc_dsql_execute2:    Tisc_dsql_execute2;
  isc_dsql_execute_immediate: Tisc_dsql_execute_immediate;
  isc_dsql_fetch:       Tisc_dsql_fetch;
  isc_dsql_free_statement: Tisc_dsql_free_statement;
  isc_dsql_prepare:     Tisc_dsql_prepare;
  isc_dsql_set_cursor_name: Tisc_dsql_set_cursor_name;
  isc_dsql_sql_info:    Tisc_dsql_sql_info;

  { Array processing routines }
  isc_array_gen_sdl:    Tisc_array_gen_sdl;
  isc_array_get_slice:  Tisc_array_get_slice;
  isc_array_lookup_bounds: Tisc_array_lookup_bounds;
  isc_array_lookup_desc: Tisc_array_lookup_desc;
  isc_array_set_desc:   Tisc_array_set_desc;
  isc_array_put_slice:  Tisc_array_put_slice;

  { Blob processing routines }
  isc_open_blob2:       Tisc_open_blob2;
  isc_create_blob2:     Tisc_create_blob2;
  isc_blob_info:        Tisc_blob_info;
  isc_close_blob:       Tisc_close_blob;
  isc_cancel_blob:      Tisc_cancel_blob;
  isc_get_segment:      Tisc_get_segment;
  isc_put_segment:      Tisc_put_segment;

  { Event processing routines }
  isc_que_events:       Tisc_que_events;
  isc_event_counts:     Tisc_event_counts;
  isc_event_block:      Tisc_event_block;
  isc_cancel_events:    Tisc_cancel_events;

  { Types convertion routines }
  isc_encode_date:      Tisc_encode_date;
  isc_decode_date:      Tisc_decode_date;
  isc_vax_integer:      Tisc_vax_integer;

  isc_encode_sql_date:  Tisc_encode_sql_date;
  isc_decode_sql_date:  Tisc_decode_sql_date;

  isc_encode_sql_time:  Tisc_encode_sql_time;
  isc_decode_sql_time:  Tisc_decode_sql_time;

  isc_encode_timestamp: Tisc_encode_timestamp;
  isc_decode_timestamp: Tisc_decode_timestamp;
end;

implementation

end.
