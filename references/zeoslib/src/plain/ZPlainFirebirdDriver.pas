{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Native Plain Drivers for Firebird             }
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

unit ZPlainFirebirdDriver;

interface

{$I ZPlain.inc}

{$IFDEF UNIX}
{$IFDEF INTERBASE_CRYPT}
{$DEFINE ENABLE_INTERBASE_CRYPT}
{$ENDIF}
{$ENDIF}

uses ZClasses, ZCompatibility, ZPlainDriver, ZPlainLoader, ZPlainFirebirdInterbaseConstants;

const

  WINDOWSIB6_DLL_LOCATION   = 'gds32.dll';
  LINUXIB6_DLL_LOCATION   = 'libgds.so';

  WINDOWS_DLL_LOCATION   = 'gds32.dll';
  LINUX_DLL_LOCATION   = 'libgds32.so';
  LINUX_IB_CRYPT_LOCATION = 'libcrypt.so';

  WINDOWS2_DLL_LOCATION   = 'fbclient.dll';
  WINDOWS2_DLL_LOCATION_EMBEDDED = 'fbclientd.dll';

  LINUX2_DLL_LOCATION   = 'libfbclient.so';
  LINUX2_DLL_LOCATION_EMBEDDED = 'libfbembed.so';
  LINUX2_IB_CRYPT_LOCATION = 'libcrypt.so';

  WINDOWS15_DLL_LOCATION   = 'fbclient15.dll';
  WINDOWS15_DLL_LOCATION_EMBEDDED = 'fbclientd15.dll';
  LINUX15_DLL_LOCATION   = 'libfbclient.so.15';
  LINUX15_IB_CRYPT_LOCATION = 'libcrypt.so.15';
  LINUX15_DLL_LOCATION_EMBEDDED = 'libfbembed.so.15';

  WINDOWS20_DLL_LOCATION   = 'fbclient20.dll';
  WINDOWS20_DLL_LOCATION_EMBEDDED = 'fbclientd20.dll';
  LINUX20_DLL_LOCATION   = 'libfbclient.so.20';
  LINUX20_DLL_LOCATION_EMBEDDED = 'libfbembed.so.20';
  LINUX20_IB_CRYPT_LOCATION = 'libcrypt.so.20';

  WINDOWS21_DLL_LOCATION   = 'fbclient21.dll';
  WINDOWS21_DLL_LOCATION_EMBEDDED = 'fbclientd21.dll';
  LINUX21_DLL_LOCATION   = 'libfbclient.so.21';
  LINUX21_DLL_LOCATION_EMBEDDED = 'libfbembed.so.21';
  LINUX21_IB_CRYPT_LOCATION = 'libcrypt.so.21';

  WINDOWS25_DLL_LOCATION   = 'fbclient25.dll';
  WINDOWS25_DLL_LOCATION_EMBEDDED = 'fbclientd25.dll';
  LINUX25_DLL_LOCATION   = 'libfbclient.so.25';
  LINUX25_DLL_LOCATION_EMBEDDED = 'libfbembed.so.25';
  LINUX25_IB_CRYPT_LOCATION = 'libcrypt.so.25';

type

  {** Represents a generic interface to Interbase native API. }
  IZInterbasePlainDriver = interface (IZPlainDriver)
    ['{AE2C4379-4E47-4752-BC01-D405ACC337F5}']
    function GetFirebirdAPI: TZFirebird_API;
    function isc_attach_database (status_vector: PISC_STATUS;
      db_name_length: Short; db_name: PAnsiChar; db_handle: PISC_DB_HANDLE;
      parm_buffer_length: Short; parm_buffer: PAnsiChar): ISC_STATUS;
    function isc_detach_database(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE): ISC_STATUS;
    function isc_drop_database(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE): ISC_STATUS;
    function isc_database_info(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short;
      item_list_buffer: PAnsiChar; result_buffer_length: Short;
      result_buffer: PAnsiChar): ISC_STATUS;
    function isc_array_gen_sdl(status_vector: PISC_STATUS;
      isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort;
      isc_arg4: PAnsiChar; isc_arg5: PShort): ISC_STATUS;
    function isc_array_get_slice(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
      dest_array: PVoid; slice_length: ISC_LONG): ISC_STATUS;
    function isc_array_lookup_bounds(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      table_name, column_name: PAnsiChar;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_lookup_desc(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      table_name, column_name: PAnsiChar;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_set_desc(status_vector: PISC_STATUS;
      table_name: PAnsiChar; column_name: PAnsiChar;
      sql_dtype, sql_length, sql_dimensions: PShort;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_put_slice(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
      source_array: PVoid; slice_length: PISC_LONG): ISC_STATUS;
    function isc_free(isc_arg1: PAnsiChar): ISC_LONG;
    function isc_sqlcode(status_vector: PISC_STATUS): ISC_LONG;
    procedure isc_sql_interprete(sqlcode: Short; buffer: PAnsiChar;
      buffer_length: Short);
    function isc_interprete(buffer: PAnsiChar; status_vector: PPISC_STATUS): ISC_STATUS;
    function isc_start_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
      db_handle: PISC_DB_HANDLE; tpb_length: Word; tpb_address: PAnsiChar): ISC_STATUS;
    function isc_start_multiple(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
      teb_vector_address: PISC_TEB): ISC_STATUS;
    function isc_rollback_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_rollback_retaining(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_commit_retaining(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_commit_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_dsql_allocate_statement(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    function isc_dsql_alloc_statement2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    function isc_dsql_describe(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_describe_bind(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
      xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute2(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
      in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute_immediate(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: Word;
      statement: PAnsiChar; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_fetch(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_free_statement(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; options: Word): ISC_STATUS;
    function isc_dsql_prepare(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
      length: Word; statement: PAnsiChar; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_set_cursor_name(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; cursor_name: PAnsiChar; _type: Word): ISC_STATUS;
    function isc_dsql_sql_info(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; item_length: Short; items: PAnsiChar;
      buffer_length: Short; buffer: PAnsiChar): ISC_STATUS;
    function isc_open_blob2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
      blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
      bpb_buffer: PAnsiChar): ISC_STATUS;
    function isc_create_blob2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
      blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
      bpb_address: PAnsiChar): ISC_STATUS;
    function isc_blob_info(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short;
      item_list_buffer: PAnsiChar; result_buffer_length: Short; result_buffer: PAnsiChar): PISC_STATUS;
    function isc_close_blob(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    function isc_cancel_blob(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    function isc_get_segment(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; actual_seg_length: PWord;
      seg_buffer_length: Word; seg_buffer: PAnsiChar): ISC_STATUS;
    function isc_put_segment(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; seg_buffer_len: Word; seg_buffer: PAnsiChar): ISC_STATUS;
    function isc_event_block(event_buffer: PPAnsiChar; result_buffer: PPAnsiChar;
      id_count: Word; event_list: array of PAnsiChar): ISC_LONG;
    procedure isc_event_counts(status_vector: PISC_STATUS;
      buffer_length: Short; event_buffer: PAnsiChar; result_buffer: PAnsiChar);
    function isc_cancel_events(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS;
    function isc_que_events(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short;
      event_buffer: PAnsiChar; event_function: TISC_CALLBACK;
      event_function_arg: PVoid): ISC_STATUS;
    procedure isc_decode_date(ib_date: PISC_QUAD; tm_date: PCTimeStructure);
    procedure isc_encode_date(tm_date: PCTimeStructure; ib_date: PISC_QUAD);
    function isc_vax_integer(buffer: PAnsiChar; length: Short): ISC_LONG;

    procedure isc_decode_sql_date(ib_date: PISC_DATE; tm_date: PCTimeStructure);
    procedure isc_decode_sql_time(ib_time: PISC_TIME; tm_date: PCTimeStructure);
    procedure isc_decode_timestamp(ib_timestamp: PISC_TIMESTAMP;
      tm_date: PCTimeStructure);
    procedure isc_encode_sql_date(tm_date: PCTimeStructure;
      ib_date: PISC_DATE);
    procedure isc_encode_sql_time(tm_date: PCTimeStructure;
      ib_time: PISC_TIME);
    procedure isc_encode_timestamp(tm_date: PCTimeStructure;
      ib_timestamp: PISC_TIMESTAMP);
  end;

  {** Implements a base driver for Firebird}

  { TZFirebirdBaseDriver }

  TZFirebirdBaseDriver = class (TZAbstractPlainDriver, IZPlainDriver,
    IZInterbasePlainDriver)
    FIREBIRD_API : TZFIREBIRD_API;
  protected
    {$IFDEF ENABLE_INTERBASE_CRYPT}
    FPreLoader : TZNativeLibraryLoader;
    procedure Initialize; virtual;
    {$ENDIF}
    procedure LoadApi; override;
  public
    constructor Create;
    {$IFDEF ENABLE_INTERBASE_CRYPT}
    destructor Destroy; override;
    {$ENDIF}

    function GetFirebirdAPI: TZFirebird_API;
    function isc_attach_database (status_vector: PISC_STATUS;
      db_name_length: Short; db_name: PAnsiChar; db_handle: PISC_DB_HANDLE;
      parm_buffer_length: Short; parm_buffer: PAnsiChar): ISC_STATUS;
    function isc_detach_database(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE): ISC_STATUS;
    function isc_drop_database(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE): ISC_STATUS;
    function isc_database_info(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short;
      item_list_buffer: PAnsiChar; result_buffer_length: Short;
      result_buffer: PAnsiChar): ISC_STATUS;
    function isc_transaction_info(status_vector: PISC_STATUS;
      tr_handle: PISC_TR_HANDLE; item_list_buffer_length: Short;
      item_list_buffer: PAnsiChar; result_buffer_length: Short;
      result_buffer: PAnsiChar): ISC_STATUS;
    function isc_array_gen_sdl(status_vector: PISC_STATUS;
      isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort;
      isc_arg4: PAnsiChar; isc_arg5: PShort): ISC_STATUS;
    function isc_array_get_slice(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
      dest_array: PVoid; slice_length: ISC_LONG): ISC_STATUS;
    function isc_array_lookup_bounds(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      table_name, column_name: PAnsiChar;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_lookup_desc(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      table_name, column_name: PAnsiChar;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_set_desc(status_vector: PISC_STATUS;
      table_name: PAnsiChar; column_name: PAnsiChar;
      sql_dtype, sql_length, sql_dimensions: PShort;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_put_slice(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
      source_array: PVoid; slice_length: PISC_LONG): ISC_STATUS;
    function isc_free(isc_arg1: PAnsiChar): ISC_LONG;
    function isc_sqlcode(status_vector: PISC_STATUS): ISC_LONG;
    procedure isc_sql_interprete(sqlcode: Short; buffer: PAnsiChar;
      buffer_length: Short);
    function isc_interprete(buffer: PAnsiChar; status_vector: PPISC_STATUS): ISC_STATUS; virtual;
    function isc_start_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
      db_handle: PISC_DB_HANDLE; tpb_length: Word; tpb_address: PAnsiChar): ISC_STATUS;
    function isc_start_multiple(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
      teb_vector_address: PISC_TEB): ISC_STATUS;
    function isc_rollback_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_rollback_retaining(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_commit_retaining(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_commit_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_dsql_allocate_statement(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    function isc_dsql_alloc_statement2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    function isc_dsql_describe(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_describe_bind(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
      xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute2(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
      in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute_immediate(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: Word;
      statement: PAnsiChar; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_fetch(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_free_statement(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; options: Word): ISC_STATUS;
    function isc_dsql_prepare(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
      length: Word; statement: PAnsiChar; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_set_cursor_name(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; cursor_name: PAnsiChar; _type: Word): ISC_STATUS;
    function isc_dsql_sql_info(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; item_length: Short; items: PAnsiChar;
      buffer_length: Short; buffer: PAnsiChar): ISC_STATUS;
    function isc_open_blob2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
      blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
      bpb_buffer: PAnsiChar): ISC_STATUS;
    function isc_create_blob2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
      blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
      bpb_address: PAnsiChar): ISC_STATUS;
    function isc_blob_info(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short;
      item_list_buffer: PAnsiChar; result_buffer_length: Short; result_buffer: PAnsiChar): PISC_STATUS;
    function isc_close_blob(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    function isc_cancel_blob(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    function isc_get_segment(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; actual_seg_length: PWord;
      seg_buffer_length: Word; seg_buffer: PAnsiChar): ISC_STATUS;
    function isc_put_segment(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; seg_buffer_len: Word; seg_buffer: PAnsiChar): ISC_STATUS;
    function isc_event_block(event_buffer: PPAnsiChar; result_buffer: PPAnsiChar;
      id_count: Word; event_list: array of PAnsiChar): ISC_LONG;
    procedure isc_event_counts(status_vector: PISC_STATUS;
      buffer_length: Short; event_buffer: PAnsiChar; result_buffer: PAnsiChar);
    function isc_cancel_events(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS;
    function isc_que_events(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short;
      event_buffer: PAnsiChar; event_function: TISC_CALLBACK;
      event_function_arg: PVoid): ISC_STATUS;
    procedure isc_decode_date(ib_date: PISC_QUAD; tm_date: PCTimeStructure);
    procedure isc_encode_date(tm_date: PCTimeStructure; ib_date: PISC_QUAD);
    procedure isc_decode_sql_date(ib_date: PISC_DATE; tm_date: PCTimeStructure);
    procedure isc_decode_sql_time(ib_time: PISC_TIME; tm_date: PCTimeStructure);
    procedure isc_decode_timestamp(ib_timestamp: PISC_TIMESTAMP;
      tm_date: PCTimeStructure);
    procedure isc_encode_sql_date(tm_date: PCTimeStructure;
      ib_date: PISC_DATE);
    procedure isc_encode_sql_time(tm_date: PCTimeStructure;
      ib_time: PISC_TIME);
    procedure isc_encode_timestamp(tm_date: PCTimeStructure;
      ib_timestamp: PISC_TIMESTAMP);
    function isc_vax_integer(buffer: PAnsiChar; length: Short): ISC_LONG;
  end;

  {** Implements a native driver for Interbase6}
  TZInterbase6PlainDriver = class (TZFirebirdBaseDriver)
  public
    constructor Create;

    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  {** Implements a native driver for Firebird 1.0}
  TZFirebird10PlainDriver = class (TZFirebirdBaseDriver)
  public
    constructor Create;

    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  {** Implements a native driver for Firebird 1.5}
  TZFirebird15PlainDriver = class (TZFirebirdBaseDriver)
  public
    constructor Create;

    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  TZFirebirdD15PlainDriver = class (TZFirebird15PlainDriver)
  public
    constructor Create;

    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;


  {** Implements a native driver for Firebird 2.0}
  TZFirebird20PlainDriver = class (TZFirebirdBaseDriver)
  public
    constructor Create;

    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  TZFirebirdD20PlainDriver = class (TZFirebird20PlainDriver)
  public
    constructor Create;

    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  {** Represents class to Interbase 6+ native API. }

  { TZFirebird21PlainDriver }

  TZFirebird21PlainDriver = class (TZFirebirdBaseDriver)
  protected
    procedure LoadApi; override;
  public
    constructor Create;

    function GetProtocol: string; override;
    function GetDescription: string; override;

    function isc_interprete(buffer: PAnsiChar; status_vector: PPISC_STATUS): ISC_STATUS; override;
  end;

  TZFirebirdD21PlainDriver = class (TZFirebird21PlainDriver)
  public
    constructor Create;

    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  { TZFirebird25PlainDriver }

  TZFirebird25PlainDriver = class (TZFirebirdBaseDriver)
  protected
    procedure LoadApi; override;
  public
    constructor Create;

    function GetProtocol: string; override;
    function GetDescription: string; override;

    function isc_interprete(buffer: PAnsiChar; status_vector: PPISC_STATUS): ISC_STATUS; override;
  end;

  { TZFirebirdD25PlainDriver }

  TZFirebirdD25PlainDriver = class (TZFirebird25PlainDriver)
  public
    constructor Create;

    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;


  function XSQLDA_LENGTH(Value: LongInt): LongInt;

implementation

uses SysUtils;

function XSQLDA_LENGTH(Value: LongInt): LongInt;
begin
  Result := SizeOf(TXSQLDA) + ((Value - 1) * SizeOf(TXSQLVAR));
end;

{ IZFirebirdPlainDriver }

{$IFDEF ENABLE_INTERBASE_CRYPT}
procedure TZFirebirdBaseDriver.Initialize;
begin
  If Assigned(FPreLoader) and not FPreLoader.Loaded then
    FPreLoader.LoadNativeLibrary;
  inherited Initialize;
end;
{$ENDIF}

procedure TZFirebirdBaseDriver.LoadApi;
begin
  inherited LoadApi;
  with Loader do
  begin
    @FIREBIRD_API.isc_sqlcode         := GetAddress('isc_sqlcode');
    @FIREBIRD_API.isc_sql_interprete  := GetAddress('isc_sql_interprete');
    @FIREBIRD_API.isc_interprete      := GetAddress('isc_interprete');
    @FIREBIRD_API.isc_vax_integer     := GetAddress('isc_vax_integer');

    @FIREBIRD_API.isc_array_gen_sdl   := GetAddress( 'isc_array_gen_sdl');
    @FIREBIRD_API.isc_array_get_slice := GetAddress( 'isc_array_get_slice');
    @FIREBIRD_API.isc_array_lookup_bounds := GetAddress( 'isc_array_lookup_bounds');
    @FIREBIRD_API.isc_array_lookup_desc := GetAddress( 'isc_array_lookup_desc');
    @FIREBIRD_API.isc_array_set_desc  := GetAddress( 'isc_array_set_desc');
    @FIREBIRD_API.isc_array_put_slice := GetAddress( 'isc_array_put_slice');

    @FIREBIRD_API.isc_blob_info       := GetAddress('isc_blob_info');
    @FIREBIRD_API.isc_open_blob2      := GetAddress('isc_open_blob2');
    @FIREBIRD_API.isc_close_blob      := GetAddress('isc_close_blob');
    @FIREBIRD_API.isc_cancel_blob     := GetAddress('isc_cancel_blob');
    @FIREBIRD_API.isc_get_segment     := GetAddress('isc_get_segment');
    @FIREBIRD_API.isc_put_segment     := GetAddress('isc_put_segment');
    @FIREBIRD_API.isc_create_blob2    := GetAddress('isc_create_blob2');
    @FIREBIRD_API.isc_decode_date     := GetAddress('isc_decode_date');
    @FIREBIRD_API.isc_encode_date     := GetAddress('isc_encode_date');
    @FIREBIRD_API.isc_dsql_free_statement := GetAddress('isc_dsql_free_statement');
    @FIREBIRD_API.isc_dsql_execute2   := GetAddress('isc_dsql_execute2');
    @FIREBIRD_API.isc_dsql_execute    := GetAddress('isc_dsql_execute');
    @FIREBIRD_API.isc_dsql_set_cursor_name := GetAddress('isc_dsql_set_cursor_name');
    @FIREBIRD_API.isc_dsql_fetch      := GetAddress('isc_dsql_fetch');
    @FIREBIRD_API.isc_dsql_sql_info   := GetAddress('isc_dsql_sql_info');
    @FIREBIRD_API.isc_dsql_allocate_statement := GetAddress('isc_dsql_allocate_statement');
    @FIREBIRD_API.isc_dsql_alloc_statement2 := GetAddress('isc_dsql_alloc_statement2');
    @FIREBIRD_API.isc_dsql_prepare    := GetAddress('isc_dsql_prepare');
    @FIREBIRD_API.isc_dsql_describe_bind := GetAddress('isc_dsql_describe_bind');
    @FIREBIRD_API.isc_dsql_describe   := GetAddress('isc_dsql_describe');
    @FIREBIRD_API.isc_dsql_execute_immediate := GetAddress('isc_dsql_execute_immediate');
    @FIREBIRD_API.isc_drop_database   := GetAddress('isc_drop_database');
    @FIREBIRD_API.isc_detach_database := GetAddress('isc_detach_database');
    @FIREBIRD_API.isc_attach_database := GetAddress('isc_attach_database');
    @FIREBIRD_API.isc_database_info   := GetAddress('isc_database_info');
    @FIREBIRD_API.isc_transaction_info   := GetAddress('isc_transaction_info');
    @FIREBIRD_API.isc_start_multiple  := GetAddress('isc_start_multiple');
    @FIREBIRD_API.isc_start_transaction := GetAddress('isc_start_transaction');
    @FIREBIRD_API.isc_commit_transaction := GetAddress('isc_commit_transaction');

    @FIREBIRD_API.isc_commit_retaining := GetAddress('isc_commit_retaining');
    @FIREBIRD_API.isc_rollback_transaction := GetAddress('isc_rollback_transaction');
    @FIREBIRD_API.isc_cancel_events   := GetAddress('isc_cancel_events');
    @FIREBIRD_API.isc_que_events      := GetAddress('isc_que_events');
    @FIREBIRD_API.isc_event_counts    := GetAddress('isc_event_counts');
    @FIREBIRD_API.isc_event_block     := GetAddress('isc_event_block');
    @FIREBIRD_API.isc_free            := GetAddress('isc_free');

    @FIREBIRD_API.isc_rollback_retaining := GetAddress( 'isc_rollback_retaining');
    @FIREBIRD_API.isc_decode_sql_date := GetAddress('isc_decode_sql_date');
    @FIREBIRD_API.isc_decode_sql_time := GetAddress('isc_decode_sql_time');
    @FIREBIRD_API.isc_decode_timestamp := GetAddress('isc_decode_timestamp');
    @FIREBIRD_API.isc_encode_sql_date := GetAddress('isc_encode_sql_date');
    @FIREBIRD_API.isc_encode_sql_time := GetAddress('isc_encode_sql_time');
    @FIREBIRD_API.isc_encode_timestamp := GetAddress('isc_encode_timestamp');
  end;
end;

constructor TZFirebirdBaseDriver.Create;
begin
   inherited create;
    FLoader := TZNativeLibraryLoader.Create([]);
    {$IFDEF ENABLE_INTERBASE_CRYPT}
    FPreLoader := TZNativeLibraryLoader.Create([LINUX_IB_CRYPT_LOCATION]);
    {$ENDIF}
end;

{$IFDEF ENABLE_INTERBASE_CRYPT}
destructor TZFirebirdBaseDriver.Destroy;
begin
  FPreLoader.Free;
  inherited Destroy;
end;
{$ENDIF}

function TZFirebirdBaseDriver.GetFirebirdAPI: TZFirebird_API;
begin
  result := FIREBIRD_API;
end;

function TZFirebirdBaseDriver.isc_array_gen_sdl(status_vector: PISC_STATUS;
  isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort;
  isc_arg4: PAnsiChar; isc_arg5: PShort): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_array_gen_sdl(status_vector, isc_array_desc,
    isc_arg3, isc_arg4, isc_arg5);
end;

function TZFirebirdBaseDriver.isc_array_get_slice(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD;
  descriptor: PISC_ARRAY_DESC; dest_array: PVoid;
  slice_length: ISC_LONG): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_array_get_slice(status_vector, db_handle,
    trans_handle, array_id, descriptor, dest_array, slice_length);
end;

function TZFirebirdBaseDriver.isc_array_lookup_bounds(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  trans_handle: PISC_TR_HANDLE; table_name, column_name: PAnsiChar;
  descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_array_lookup_bounds(status_vector, db_handle,
    trans_handle, table_name, column_name, descriptor);
end;

function TZFirebirdBaseDriver.isc_array_lookup_desc(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  trans_handle: PISC_TR_HANDLE; table_name, column_name: PAnsiChar;
  descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_array_lookup_desc(status_vector, db_handle,
    trans_handle, table_name, column_name, descriptor);
end;

function TZFirebirdBaseDriver.isc_array_put_slice(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD;
  descriptor: PISC_ARRAY_DESC; source_array: PVoid;
  slice_length: PISC_LONG): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_array_put_slice(status_vector, db_handle,
    trans_handle, array_id, descriptor, source_array, slice_length);
end;

function TZFirebirdBaseDriver.isc_array_set_desc(status_vector: PISC_STATUS;
  table_name: PAnsiChar; column_name: PAnsiChar; sql_dtype, sql_length,
  sql_dimensions: PShort; descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_array_set_desc(status_vector, table_name,
    column_name, sql_dtype, sql_length, sql_dimensions, descriptor);
end;

function TZFirebirdBaseDriver.isc_attach_database(status_vector: PISC_STATUS;
  db_name_length: Short; db_name: PAnsiChar; db_handle: PISC_DB_HANDLE;
  parm_buffer_length: Short; parm_buffer: PAnsiChar): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_attach_database(status_vector, db_name_length,
    db_name, db_handle, parm_buffer_length, parm_buffer);
end;

function TZFirebirdBaseDriver.isc_blob_info(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short;
  item_list_buffer: PAnsiChar; result_buffer_length: Short;
  result_buffer: PAnsiChar): PISC_STATUS;
begin
  Result := PISC_STATUS(FIREBIRD_API.isc_blob_info(status_vector, blob_handle,
    item_list_buffer_length, item_list_buffer, result_buffer_length,
    result_buffer));
end;

function TZFirebirdBaseDriver.isc_cancel_blob(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_cancel_blob(status_vector, blob_handle);
end;

function TZFirebirdBaseDriver.isc_cancel_events(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_cancel_events(status_vector, db_handle,
    event_id);
end;

function TZFirebirdBaseDriver.isc_close_blob(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_close_blob(status_vector, blob_handle);
end;

function TZFirebirdBaseDriver.isc_commit_retaining(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_commit_retaining(status_vector, tran_handle);
end;

function TZFirebirdBaseDriver.isc_commit_transaction(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_commit_transaction(status_vector, tran_handle);
end;

function TZFirebirdBaseDriver.isc_create_blob2(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
  blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
  bpb_address: PAnsiChar): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_create_blob2(status_vector, db_handle,
    tran_handle, blob_handle, blob_id, bpb_length, bpb_address);
end;

function TZFirebirdBaseDriver.isc_database_info(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short;
  item_list_buffer: PAnsiChar; result_buffer_length: Short;
  result_buffer: PAnsiChar): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_database_info(status_vector, db_handle,
    item_list_buffer_length, item_list_buffer, result_buffer_length,
    result_buffer);
end;

function TZFirebirdBaseDriver.isc_transaction_info(status_vector: PISC_STATUS;
  tr_handle: PISC_TR_HANDLE; item_list_buffer_length: Short;
  item_list_buffer: PAnsiChar; result_buffer_length: Short;
  result_buffer: PAnsiChar): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_transaction_info(status_vector, tr_handle,
    item_list_buffer_length, item_list_buffer, result_buffer_length,
    result_buffer);
end;

procedure TZFirebirdBaseDriver.isc_decode_date(ib_date: PISC_QUAD;
  tm_date: PCTimeStructure);
begin
  FIREBIRD_API.isc_decode_date(ib_date, tm_date);
end;

procedure TZFirebirdBaseDriver.isc_decode_sql_date(ib_date: PISC_DATE;
  tm_date: PCTimeStructure);
begin
  FIREBIRD_API.isc_decode_sql_date(ib_date, tm_date);
end;

procedure TZFirebirdBaseDriver.isc_decode_sql_time(ib_time: PISC_TIME;
  tm_date: PCTimeStructure);
begin
  FIREBIRD_API.isc_decode_sql_time(ib_time, tm_date);
end;

procedure TZFirebirdBaseDriver.isc_decode_timestamp(
  ib_timestamp: PISC_TIMESTAMP; tm_date: PCTimeStructure);
begin
  FIREBIRD_API.isc_decode_timestamp(ib_timestamp, tm_date);
end;

function TZFirebirdBaseDriver.isc_detach_database(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_detach_database(status_vector, db_handle);
end;

function TZFirebirdBaseDriver.isc_drop_database(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_drop_database(status_vector, db_handle);
end;

function TZFirebirdBaseDriver.isc_dsql_alloc_statement2(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_dsql_alloc_statement2(status_vector,
    db_handle, stmt_handle);
end;

function TZFirebirdBaseDriver.isc_dsql_allocate_statement(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_dsql_allocate_statement(status_vector,
    db_handle, stmt_handle);
end;

function TZFirebirdBaseDriver.isc_dsql_describe(status_vector: PISC_STATUS;
  stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_dsql_describe(status_vector, stmt_handle,
    dialect, xsqlda);
end;

function TZFirebirdBaseDriver.isc_dsql_describe_bind(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
  xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_dsql_describe_bind(status_vector, stmt_handle,
    dialect, xsqlda);
end;

function TZFirebirdBaseDriver.isc_dsql_execute(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
  xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_dsql_execute(status_vector, tran_handle,
    stmt_handle, dialect, xsqlda);
end;

function TZFirebirdBaseDriver.isc_dsql_execute_immediate(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  tran_handle: PISC_TR_HANDLE; length: Word; statement: PAnsiChar;
  dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_dsql_execute_immediate(status_vector,
    db_handle, tran_handle, length, statement, dialect, xsqlda);
end;

function TZFirebirdBaseDriver.isc_dsql_execute2(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
  dialect: Word; in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_dsql_execute2(status_vector, tran_handle,
    stmt_handle, dialect, in_xsqlda, out_xsqlda);
end;

function TZFirebirdBaseDriver.isc_dsql_fetch(status_vector: PISC_STATUS;
  stmt_handle: PISC_STMT_HANDLE; dialect: Word;
  xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_dsql_fetch(status_vector, stmt_handle, dialect,
    xsqlda);
end;

function TZFirebirdBaseDriver.isc_dsql_free_statement(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  options: Word): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_dsql_free_statement(status_vector, stmt_handle,
    options);
end;

function TZFirebirdBaseDriver.isc_dsql_prepare(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  stmt_handle: PISC_STMT_HANDLE; length: Word; statement: PAnsiChar;
  dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_dsql_prepare(status_vector, tran_handle,
    stmt_handle, length, statement, dialect, xsqlda);
end;

function TZFirebirdBaseDriver.isc_dsql_set_cursor_name(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  cursor_name: PAnsiChar; _type: Word): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_dsql_set_cursor_name(status_vector,
    stmt_handle, cursor_name, _type);
end;

function TZFirebirdBaseDriver.isc_dsql_sql_info(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  item_length: Short; items: PAnsiChar; buffer_length: Short;
  buffer: PAnsiChar): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_dsql_sql_info(status_vector, stmt_handle,
    item_length, items, buffer_length, buffer);
end;

procedure TZFirebirdBaseDriver.isc_encode_date(tm_date: PCTimeStructure;
  ib_date: PISC_QUAD);
begin
  FIREBIRD_API.isc_encode_date(tm_date, ib_date);
end;

procedure TZFirebirdBaseDriver.isc_encode_sql_date(
  tm_date: PCTimeStructure; ib_date: PISC_DATE);
begin
  FIREBIRD_API.isc_encode_sql_date(tm_date, ib_date);
end;

procedure TZFirebirdBaseDriver.isc_encode_sql_time(
  tm_date: PCTimeStructure; ib_time: PISC_TIME);
begin
  FIREBIRD_API.isc_encode_sql_time(tm_date, ib_time);
end;

procedure TZFirebirdBaseDriver.isc_encode_timestamp(
  tm_date: PCTimeStructure; ib_timestamp: PISC_TIMESTAMP);
begin
  FIREBIRD_API.isc_encode_timestamp(tm_date, ib_timestamp);
end;

function TZFirebirdBaseDriver.isc_event_block(event_buffer: PPAnsiChar;
  result_buffer: PPAnsiChar; id_count: Word; event_list: array of PAnsiChar
  ): ISC_LONG;
begin
  Result := FIREBIRD_API.isc_event_block(event_buffer, result_buffer,
    id_count, event_list);
end;

procedure TZFirebirdBaseDriver.isc_event_counts(
  status_vector: PISC_STATUS; buffer_length: Short; event_buffer,
  result_buffer: PAnsiChar);
begin
  FIREBIRD_API.isc_event_counts(status_vector, buffer_length,
    event_buffer, result_buffer);
end;

function TZFirebirdBaseDriver.isc_free(isc_arg1: PAnsiChar): ISC_LONG;
begin
  Result := FIREBIRD_API.isc_free(isc_arg1);
end;

function TZFirebirdBaseDriver.isc_get_segment(
  status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE;
  actual_seg_length: PWord; seg_buffer_length: Word;
  seg_buffer: PAnsiChar): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_get_segment(status_vector, blob_handle,
    actual_seg_length, seg_buffer_length, seg_buffer);
end;

function TZFirebirdBaseDriver.isc_interprete(buffer: PAnsiChar;
  status_vector: PPISC_STATUS): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_interprete(buffer, status_vector);
end;

function TZFirebirdBaseDriver.isc_open_blob2(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
  blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
  bpb_buffer: PAnsiChar): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_open_blob2(status_vector, db_handle,
    tran_handle, blob_handle, blob_id, bpb_length, bpb_buffer);
end;

function TZFirebirdBaseDriver.isc_put_segment(
  status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE;
  seg_buffer_len: Word; seg_buffer: PAnsiChar): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_put_segment(status_vector, blob_handle,
    seg_buffer_len, seg_buffer);
end;

function TZFirebirdBaseDriver.isc_que_events(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short;
  event_buffer: PAnsiChar; event_function: TISC_CALLBACK;
  event_function_arg: PVoid): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_que_events(status_vector, db_handle,
    event_id, length, event_buffer, event_function, event_function_arg)
end;

function TZFirebirdBaseDriver.isc_rollback_retaining(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_rollback_retaining(status_vector, tran_handle);
end;

procedure TZFirebirdBaseDriver.isc_sql_interprete(sqlcode: Short;
  buffer: PAnsiChar; buffer_length: Short);
begin
  FIREBIRD_API.isc_sql_interprete(sqlcode, buffer, buffer_length);
end;

function TZFirebirdBaseDriver.isc_sqlcode(
  status_vector: PISC_STATUS): ISC_LONG;
begin
  Result := FIREBIRD_API.isc_sqlcode(status_vector);
end;

function TZFirebirdBaseDriver.isc_rollback_transaction(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_rollback_transaction(status_vector, tran_handle);
end;

function TZFirebirdBaseDriver.isc_start_multiple(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  db_handle_count: Short; teb_vector_address: PISC_TEB): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_start_multiple(status_vector, tran_handle,
    db_handle_count, teb_vector_address);
end;

function TZFirebirdBaseDriver.isc_start_transaction(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  db_handle_count: Short; db_handle: PISC_DB_HANDLE; tpb_length: Word;
  tpb_address: PAnsiChar): ISC_STATUS;
begin
  Result := FIREBIRD_API.isc_start_transaction(status_vector, tran_handle,
    db_handle_count, db_handle, tpb_length, tpb_address);
end;

function TZFirebirdBaseDriver.isc_vax_integer(buffer: PAnsiChar;
  length: Short): ISC_LONG;
begin
  Result := FIREBIRD_API.isc_vax_integer(buffer, length);
end;


constructor TZInterbase6PlainDriver.Create;
begin
   inherited create;
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWSIB6_DLL_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUXIB6_DLL_LOCATION);
    {$IFDEF ENABLE_INTERBASE_CRYPT}
    FPreLoader.AddLocation(LINUX_IB_CRYPT_LOCATION);
    {$ENDIF}
  {$ENDIF}
end;

function TZInterbase6PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Interbase 6';
end;

function TZInterbase6PlainDriver.GetProtocol: string;
begin
  Result := 'interbase-6';
end;

constructor TZFirebird10PlainDriver.Create;
begin
   inherited create;
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION);
    {$IFDEF ENABLE_INTERBASE_CRYPT}
    FPreLoader.AddLocation(LINUX_IB_CRYPT_LOCATION);
    {$ENDIF}
  {$ENDIF}
end;

function TZFirebird10PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Firebird 1.0';
end;

function TZFirebird10PlainDriver.GetProtocol: string;
begin
  Result := 'firebird-1.0';
end;

{ IZFirebird15PlainDriver }

constructor TZFirebird15PlainDriver.Create;
begin
   inherited create;
  {$IFNDEF UNIX}
    {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
      FLoader.AddLocation(WINDOWS2_DLL_LOCATION);
    {$ENDIF}
    FLoader.AddLocation(WINDOWS15_DLL_LOCATION);
  {$ELSE}
    {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
      FLoader.AddLocation(LINUX2_DLL_LOCATION);
    {$ENDIF}
    FLoader.AddLocation(LINUX15_DLL_LOCATION);
    {$IFDEF ENABLE_INTERBASE_CRYPT}
      {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
        FPreLoader.AddLocation(LINUX2_IB_CRYPT_LOCATION);
      {$ENDIF}
      FPreLoader.AddLocation(LINUX15_IB_CRYPT_LOCATION);
    {$ENDIF}
  {$ENDIF}
end;

function TZFirebird15PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Firebird 1.5';
end;

function TZFirebird15PlainDriver.GetProtocol: string;
begin
  Result := 'firebird-1.5';
end;

{ IZFirebird15PlainDriver }

constructor TZFirebirdD15PlainDriver.Create;
begin
   inherited create;
  {$IFNDEF UNIX}
    {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
      FLoader.AddLocation(WINDOWS2_DLL_LOCATION_EMBEDDED);
    {$ENDIF}
    FLoader.AddLocation(WINDOWS15_DLL_LOCATION_EMBEDDED);
  {$ELSE}
    {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
      FLoader.AddLocation(LINUX2_DLL_LOCATION_EMBEDDED);
    {$ENDIF}
    FLoader.AddLocation(LINUX15_DLL_LOCATION_EMBEDDED);
    {$ENDIF}
end;

function TZFirebirdD15PlainDriver.GetProtocol: string;
begin
  Result := 'firebirdd-1.5';
end;

function TZFirebirdD15PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Firebird Embedded 1.5';
end;

{ IZFirebird20PlainDriver }

constructor TZFirebird20PlainDriver.Create;
begin
   inherited create;
  {$IFNDEF UNIX}
    {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
      FLoader.AddLocation(WINDOWS2_DLL_LOCATION);
    {$ENDIF}
    FLoader.AddLocation(WINDOWS20_DLL_LOCATION);
  {$ELSE}
    {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
      FLoader.AddLocation(LINUX2_DLL_LOCATION);
    {$ENDIF}
    FLoader.AddLocation(LINUX20_DLL_LOCATION);
    {$IFDEF ENABLE_INTERBASE_CRYPT}
      {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
        FPreLoader.AddLocation(LINUX2_IB_CRYPT_LOCATION);
      {$ENDIF}
      FPreLoader.AddLocation(LINUX20_IB_CRYPT_LOCATION);
    {$ENDIF}
  {$ENDIF}
end;

function TZFirebird20PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Firebird 2.0';
end;

function TZFirebird20PlainDriver.GetProtocol: string;
begin
  Result := 'firebird-2.0';
end;

{ IZFirebirdD20PlainDriver }


constructor TZFirebirdD20PlainDriver.Create;
begin
   inherited create;
  {$IFNDEF UNIX}
    {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
      FLoader.AddLocation(WINDOWS2_DLL_LOCATION_EMBEDDED);
    {$ENDIF}
    FLoader.AddLocation(WINDOWS20_DLL_LOCATION_EMBEDDED);
  {$ELSE}
    {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
      FLoader.AddLocation(LINUX2_DLL_LOCATION_EMBEDDED);
    {$ENDIF}
    FLoader.AddLocation(LINUX20_DLL_LOCATION_EMBEDDED);
    {$ENDIF}
end;

function TZFirebirdD20PlainDriver.GetProtocol: string;
begin
  Result := 'firebirdd-2.0';
end;

function TZFirebirdD20PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Firebird Embedded 2.0';
end;

{ IZFirebird21PlainDriver }

procedure TZFirebird21PlainDriver.LoadApi;
begin
  inherited LoadApi;

  with Loader do
  begin
  @FIREBIRD_API.fb_interpret        := GetAddress('fb_interpret');
  end;
end;

constructor TZFirebird21PlainDriver.Create;
begin
   inherited create;
  {$IFNDEF UNIX}
    {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
      FLoader.AddLocation(WINDOWS2_DLL_LOCATION);
    {$ENDIF}
    FLoader.AddLocation(WINDOWS21_DLL_LOCATION);
  {$ELSE}
    {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
      FLoader.AddLocation(LINUX2_DLL_LOCATION);
    {$ENDIF}
    FLoader.AddLocation(LINUX21_DLL_LOCATION);
    {$IFDEF ENABLE_INTERBASE_CRYPT}
      {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
        FPreLoader.AddLocation(LINUX2_IB_CRYPT_LOCATION);
      {$ENDIF}
      FPreLoader.AddLocation(LINUX21_IB_CRYPT_LOCATION);
    {$ENDIF}
  {$ENDIF}
end;


function TZFirebird21PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Firebird 2.1';
end;

function TZFirebird21PlainDriver.GetProtocol: string;
begin
  Result := 'firebird-2.1';
end;

function TZFirebird21PlainDriver.isc_interprete(buffer: PAnsiChar;
  status_vector: PPISC_STATUS): ISC_STATUS;
var
   bufsize : integer;
begin
  bufsize := 1024;
  Result := FIREBIRD_API.fb_interpret(buffer, bufsize, status_vector);
end;

{ IZFirebirdD21PlainDriver }

constructor TZFirebirdD21PlainDriver.Create;
begin
   inherited create;
  {$IFNDEF UNIX}
    {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
      FLoader.AddLocation(WINDOWS2_DLL_LOCATION_EMBEDDED);
    {$ENDIF}
    FLoader.AddLocation(WINDOWS21_DLL_LOCATION_EMBEDDED);
  {$ELSE}
    {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
      FLoader.AddLocation(LINUX2_DLL_LOCATION_EMBEDDED);
    {$ENDIF}
    FLoader.AddLocation(LINUX21_DLL_LOCATION_EMBEDDED);
    {$ENDIF}
end;

function TZFirebirdD21PlainDriver.GetProtocol: string;
begin
  Result := 'firebirdd-2.1';
end;

function TZFirebirdD21PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Firebird Embedded 2.1';
end;

{ TZFirebird25PlainDriver }

procedure TZFirebird25PlainDriver.LoadApi;
begin
  inherited LoadApi;

  with Loader do
  begin
    @FIREBIRD_API.fb_interpret        := GetAddress('fb_interpret');
  end;
end;

constructor TZFirebird25PlainDriver.Create;
begin
  inherited create;
  {$IFNDEF UNIX}
    {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
      FLoader.AddLocation(WINDOWS2_DLL_LOCATION);
    {$ENDIF}
    FLoader.AddLocation(WINDOWS25_DLL_LOCATION);
  {$ELSE}
    {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
      FLoader.AddLocation(LINUX2_DLL_LOCATION);
    {$ENDIF}
    FLoader.AddLocation(LINUX25_DLL_LOCATION);
    {$IFDEF ENABLE_INTERBASE_CRYPT}
      {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
        FPreLoader.AddLocation(LINUX2_IB_CRYPT_LOCATION);
      {$ENDIF}
      FPreLoader.AddLocation(LINUX25_IB_CRYPT_LOCATION);
    {$ENDIF}
  {$ENDIF}
end;

function TZFirebird25PlainDriver.GetProtocol: string;
begin
  Result := 'firebird-2.5';
end;

function TZFirebird25PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Firebird 2.5';
end;

function TZFirebird25PlainDriver.isc_interprete(buffer: PAnsiChar;
  status_vector: PPISC_STATUS): ISC_STATUS;
begin
  Result:=inherited isc_interprete(buffer, status_vector);
end;

{ TZFirebirdD25PlainDriver }

constructor TZFirebirdD25PlainDriver.Create;
begin
   inherited create;
  {$IFNDEF UNIX}
    {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
      FLoader.AddLocation(WINDOWS2_DLL_LOCATION_EMBEDDED);
    {$ENDIF}
    FLoader.AddLocation(WINDOWS25_DLL_LOCATION_EMBEDDED);
  {$ELSE}
    {$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
      FLoader.AddLocation(LINUX2_DLL_LOCATION_EMBEDDED);
    {$ENDIF}
    FLoader.AddLocation(LINUX25_DLL_LOCATION_EMBEDDED);
    {$ENDIF}
end;

function TZFirebirdD25PlainDriver.GetProtocol: string;
begin
  Result := 'firebirdd-2.5';
end;

function TZFirebirdD25PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Firebird Embedded 2.5';
end;

end.
