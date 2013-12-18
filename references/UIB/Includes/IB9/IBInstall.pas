{******************************************************}
{                                                      }
{  ibinstall.pas                                       }
{                                                      }
{  Description:                                        }
{	      InterBase Install API definitions        }
{                                                      }
{  Copyright (c) InterBase Software Corporation, 1999  }
{                                                      }
{******************************************************}

unit IBInstall;

interface

uses
  Windows;

type
  OPTIONS_HANDLE = Integer;
  POPTIONS_HANDLE = ^OPTIONS_HANDLE;
  MSG_NO = Longint;
  OPT = Longint;
  TEXT = PChar;
  FP_ERROR = function(msg: MSG_NO; data: Pointer; error_msg: TEXT): Integer; stdcall;
  FP_STATUS = function(status: integer; data: Pointer; const status_msg: TEXT):
                       Integer; stdcall;

const
  IB_INSTALL_DLL = 'ibinstall.dll';

  { These are the values the FP_ERROR routine can return. }
  isc_install_fp_retry    = -1;
  isc_install_fp_continue =  0;
  isc_install_fp_abort    =  1;

   { isc_install_get_info info_types }
   isc_install_info_destination    =  1;
   isc_install_info_opspace        =  2;
   isc_install_info_opname         =  3;
   isc_install_info_opdescription  =  4;


  ISC_INSTALL_MAX_MESSAGE_LEN   = 300;
  ISC_INSTALL_MAX_MESSAGES      = 200;
  ISC_INSTALL_MAX_PATH          = MAX_PATH;

  { Basic Components used to install InterBase }
  INTERBASE                     = 1000;
  IB_SERVER                     = 1001;
  IB_CLIENT                     = 1002;

  IB_CMD_TOOLS                  = 1003;
  IB_CMD_TOOLS_DB_MGMT          = 1004;
  IB_CMD_TOOLS_USR_MGMT         = 1005;
  IB_CMD_TOOLS_DB_QUERY         = 1006;

  IB_GUI_TOOLS                  = 1007;
  IB_DOC                        = 1011;

  IB_EXAMPLES                   = 1012;
  IB_EXAMPLE_API                = 1013;
  IB_EXAMPLE_DB                 = 1014;
  IB_DEV                        = 1015;
  IB_REPLICATION                = 1016;
  IB_REPL_MANAGER               = 1017;
  IB_REPL_SERVER                = 1018;

  IB_CONNECTIVITY               = 1101;
  IB_ODBC_CLIENT                = 1102;
  IB_JDBC                       = 1110;
  IB_JDBC_CLIENT                = 1103;
  IB_JDBC_SERVER                = 1105;

  { Error and warning codes }
  isc_install_optlist_empty           = -1;
  isc_install_actlist_empty           = -2;
  isc_install_fp_copy_delayed         = -3;
  isc_install_fp_delete_delayed       = -4;
  isc_install_option_not_found        = -5;
  isc_install_msg_version             = -6;
  isc_install_cant_load_msg           = -7;
  isc_install_invalid_msg             = -8;
  isc_install_invalid_tbl             = -9;
  isc_install_cant_create_msg         = -10;
  isc_install_handle_not_allocated    = -11;
  isc_install_odbc_comp_notfound      = -12;
  isc_install_cant_delete             = -13;
  isc_install_cant_rmdir              = -14;
  isc_install_key_nonempty            = -15;

  isc_install_success                 = 0;

  { File and directory related errors }
  isc_install_path_not_valid          = 1;
  isc_install_path_not_exists         = 2;
  isc_install_cant_write              = 3;
  isc_install_type_unknown            = 4;
  isc_install_cant_move_file          = 5;
  isc_install_device_not_valid        = 6;
  isc_install_data_truncated          = 7;
  isc_install_cant_get_temp           = 8;
  isc_install_no_file                 = 9;
  isc_install_cant_load_lib           = 10;
  isc_install_cant_lookup_lib         = 11;
  isc_install_file_exists             = 12;
  isc_install_cant_open_log           = 13;
  isc_install_write_error             = 14;
  isc_install_read_error              = 15;
  isc_install_invalid_log             = 16;
  isc_install_cant_read               = 17;
  isc_install_no_diskspace            = 18;
  isc_install_cant_create_dir         = 19;
  isc_install_msg_syntax              = 20;
  isc_install_fp_delete_error         = 21;
  isc_install_fp_rename_error         = 22;
  isc_install_fp_copy_error           = 23;

  { Precheck related errors }
  isc_install_system_not_supported    = 24;
  isc_install_server_running          = 25;
  isc_install_classic_found           = 26;
  isc_install_no_privileges           = 27;
  isc_install_cant_get_free_space     = 28;
  isc_install_guardian_running        = 29;
  isc_install_invalid_option          = 30;
  isc_install_invalid_handle          = 31;
  isc_install_message_not_found       = 32;

  { TCP/IP services related }
  isc_install_no_stack                = 33;
  isc_install_cant_add_service        = 34;
  isc_install_invalid_port            = 35;
  isc_install_invalid_service         = 36;
  isc_install_no_proto                = 37;
  isc_install_no_services_entry       = 38;
  isc_install_sock_error              = 39;
  isc_install_conversion_error        = 40;


  { Operations errors }
  isc_install_cant_copy               = 41;
  isc_install_no_mem                  = 42;
  isc_install_queue_failed            = 43;
  isc_install_invalid_param           = 44;
  isc_install_fp_error_exception      = 45;
  isc_install_fp_status_exception     = 46;
  isc_install_user_aborted            = 47;

  { Registry related errors }
  isc_install_key_exists              = 48;
  isc_install_cant_create_key         = 49;
  isc_install_cant_set_value          = 50;
  isc_install_cant_open_key           = 51;
  isc_install_cant_delete_key         = 52;
  isc_install_cant_query_key          = 53;
  isc_install_cant_delete_value       = 54;

  { OS services related errors }
  isc_install_service_existed         = 55;
  isc_install_cant_create_service     = 56;
  isc_install_cant_open_service       = 57;
  isc_install_cant_query_service      = 58;
  isc_install_service_running         = 59;
  isc_install_cant_delete_service     = 60;
  isc_install_cant_open_manager       = 61;
  isc_install_system_error            = 62;
  isc_install_com_regfail             = 63;
  isc_install_dcom_required           = 64;

  { ODBC installation errors }
  isc_install_odbc_general            = 65;
  isc_install_core_version            = 66;
  isc_install_drv_version             = 67;
  isc_install_tran_version            = 68;


function isc_install_clear_options(pHandle: POPTIONS_HANDLE):MSG_NO; stdcall;
                                   external IB_INSTALL_DLL;

function isc_install_execute(Handle: OPTIONS_HANDLE; src_dir, dest_dir: TEXT;
                             status_func: FP_STATUS; status_data: Pointer;
                             error_func: FP_ERROR; error_data: Pointer;
                             uninstal_file_name: TEXT):MSG_NO; stdcall;
                             external IB_INSTALL_DLL;

function isc_install_get_info(info_type :integer; option :OPT; info_buffer : Pointer;
                              buf_len : Cardinal): MSG_NO; stdcall; external IB_INSTALL_DLL;

function isc_install_get_message(Handle: OPTIONS_HANDLE; message_no: MSG_NO;
                                 message_txt: Pointer; message_len: Cardinal):
                                 MSG_NO; stdcall; external IB_INSTALL_DLL;

function isc_install_load_external_text(msg_file_name: TEXT):MSG_NO; stdcall;
                                        external IB_INSTALL_DLL;

function isc_install_precheck(Handle: OPTIONS_HANDLE; src_dir, dest_dir: TEXT):
                              MSG_NO; stdcall; external IB_INSTALL_DLL;

function isc_install_set_option(pHandle: POPTIONS_HANDLE; option: OPT):MSG_NO;
                                stdcall; external IB_INSTALL_DLL;

function isc_uninstall_execute(uninstall_file_name: TEXT; status_func: FP_STATUS;
                               status_data: pointer; error_func: FP_ERROR; error_data: pointer):
                               MSG_NO; stdcall; external IB_INSTALL_DLL;

function isc_uninstall_precheck(uninstall_file_name: TEXT):MSG_NO; stdcall;
                                external IB_INSTALL_DLL;

function isc_install_unset_option(pHandle: POPTIONS_HANDLE; option: OPT):MSG_NO;
                                  stdcall; external IB_INSTALL_DLL;


implementation

end.
