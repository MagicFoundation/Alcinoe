/***********************************************
*
*   MODULE: IBINSTALL.H
*
*   DESCRIPTION: INSTALL OSRI entry points
*
*   Copyright (c) InterBase Software Corporation, 1998, 1999, 2000
*
*****************************************************************/

#ifndef _IBINSTALL_H_
#define _IBINSTALL_H_

/******************************************************************/
/* Define type, export and other stuff based on c/c++ and Windows */
/******************************************************************/

#ifndef ISC_EXPORT

#if (defined(_MSC_VER) && defined(WIN32)) || \
    (defined(__BORLANDC__) && (defined(__WIN32__) || defined(__OS2__)))
#define  ISC_FAR
#define  ISC_EXPORT         __stdcall
#define  ISC_EXPORT_VARARG  __cdecl
#else                   /* Not Windows/NT */
#if (defined(__IBMC__) && defined(__OS2__))
#define  ISC_FAR
#define  ISC_EXPORT         System
#define  ISC_EXPORT_VARARG  ISC_EXPORT
#else                   /* not IBM C Set++ for OS/2 */
#if ( defined( _Windows) || defined( _WINDOWS))
#define  ISC_FAR        __far
#define  ISC_EXPORT     ISC_FAR __cdecl __loadds __export
#define  ISC_EXPORT_VARARG  ISC_EXPORT
#else                   /* Not Windows/NT, OS/2 or Windows */
#define  ISC_FAR
#define  ISC_EXPORT
#define  ISC_EXPORT_VARARG
#endif                  /* Windows and Not Windows/NT or OS/2 */
#endif                  /* IBM C Set++ for OS/2 */
#endif                  /* Windows/NT */

#endif /* ISC_EXPORT */

typedef void*           OPTIONS_HANDLE;

typedef long            MSG_NO;

typedef unsigned long   OPT;

#ifdef TEXT
#undef TEXT
#endif

typedef char    TEXT;


/**********************************************************************************************
 * Name: FP_ERROR
 *
 * Description: This is a user defined callback function to be invoked by
 *              Install API functions
 *
 ************************************************************************************************/

typedef int  (ISC_EXPORT *FP_ERROR)(MSG_NO, void*, const TEXT*);

/* *********************************************************
 * These are the values the FP_ERROR routine can return.
 * FP_STATUS also must return one of these except fp_retry
 ***********************************************************/

#define isc_install_fp_retry    -1
#define isc_install_fp_continue  0
#define isc_install_fp_abort     1


/* *********************************************************
 * Info types for isc_install_get_info
 * 
 ***********************************************************/

#define  isc_install_info_destination   1
#define  isc_install_info_opspace       2
#define  isc_install_info_opname        3
#define  isc_install_info_opdescription 4


#ifdef WIN32
#ifndef ISC_INSTALL_MAX_PATH
#define ISC_INSTALL_MAX_PATH        MAX_PATH
#endif
#endif

/***********************************************************
 * Name: I S C _ I N S T A L L _ M A X _ M E S S A G E _ L E N
 *
 * Description: This is a maximum error message length
 ***********************************************************/

#define ISC_INSTALL_MAX_MESSAGE_LEN 300

/***********************************************************
 * Name: I S C _ I N S T A L L _ M A X _ M E S S A G E S
 *
 * Description: This is a maximum anumber of messages
 ***********************************************************/

#define ISC_INSTALL_MAX_MESSAGES    200

/**********************************************************************************************
 * Name: FP_STATUS
 *
 * Description: This is a user defined callback function to be invoked by
 *              Install API functions
 *
 ************************************************************************************************/

typedef int (ISC_EXPORT *FP_STATUS)(int, void*, const TEXT*);

/*
 *  Basic Components used to install InterBase
 */

#define INTERBASE               1000
#define IB_SERVER               1001
#define IB_CLIENT               1002

#define IB_CMD_TOOLS            1003
#define IB_CMD_TOOLS_DB_MGMT    1004
#define IB_CMD_TOOLS_USR_MGMT   1005
#define IB_CMD_TOOLS_DB_QUERY   1006

#define IB_GUI_TOOLS            1007
#define IB_DOC                  1011

#define IB_EXAMPLES             1012
#define IB_EXAMPLE_API          1013
#define IB_EXAMPLE_DB           1014
#define IB_DEV                  1015

#define IB_REPLICATION          1016
#define IB_REPL_MANAGER         1017
#define IB_REPL_SERVER          1018


#define IB_CONNECTIVITY         1101
#define IB_ODBC_CLIENT          1102
#define IB_OLEDB_CLIENT         1104

#define IB_JDBC                 1110
#define IB_JDBC_CLIENT          1103
#define IB_JDBC_SERVER          1105

/*
 *  Error codes.<M-F4>
 */

/* Warnings */

#define isc_install_optlist_empty            -1
#define isc_install_actlist_empty            -2
#define isc_install_fp_copy_delayed         -3
#define isc_install_fp_delete_delayed       -4
#define isc_install_option_not_found        -5
#define isc_install_msg_version             -6
#define isc_install_cant_load_msg           -7
#define isc_install_invalid_msg             -8
#define isc_install_invalid_tbl             -9
#define isc_install_cant_create_msg         -10
#define isc_install_handle_not_allocated    -11
#define isc_install_odbc_comp_notfound      -12
#define isc_install_cant_delete             -13
#define isc_install_cant_rmdir              -14
#define isc_install_key_nonempty            -15

#define isc_install_success                  0

/* File and directory related errors */

#define isc_install_path_not_valid           1
#define isc_install_path_not_exists          2
#define isc_install_cant_write               3
#define isc_install_type_unknown             4
#define isc_install_cant_move_file           5
#define isc_install_device_not_valid         6
#define isc_install_data_truncated           7
#define isc_install_cant_get_temp            8
#define isc_install_no_file                  9
#define isc_install_cant_load_lib            10
#define isc_install_cant_lookup_lib          11
#define isc_install_file_exists              12
#define isc_install_cant_open_log            13
#define isc_install_write_error              14
#define isc_install_read_error               15
#define isc_install_invalid_log              16
#define isc_install_cant_read                17
#define isc_install_no_diskspace             18
#define isc_install_cant_create_dir          19
#define isc_install_msg_syntax               20
#define isc_install_fp_delete_error          21
#define isc_install_fp_rename_error          22
#define isc_install_fp_copy_error            23

/* Precheck related errors */

#define isc_install_system_not_supported     24
#define isc_install_server_running           25
#define isc_install_classic_found            26
#define isc_install_no_privileges            27
#define isc_install_cant_get_free_space      28
#define isc_install_guardian_running         29
#define isc_install_invalid_option           30
#define isc_install_invalid_handle           31
#define isc_install_message_not_found        32

/* TCP/IP services related */

#define isc_install_no_stack                 33
#define isc_install_cant_add_service         34
#define isc_install_invalid_port             35
#define isc_install_invalid_service          36
#define isc_install_no_proto                 37
#define isc_install_no_services_entry        38
#define isc_install_sock_error               39
#define isc_install_conversion_error         40


/* Operations errors */

#define isc_install_cant_copy                 41
#define isc_install_no_mem                    42
#define isc_install_queue_failed              43
#define isc_install_invalid_param             44
#define isc_install_fp_error_exception        45
#define isc_install_fp_status_exception       46
#define isc_install_user_aborted              47

/* Registry related errors */

#define isc_install_key_exists                48
#define isc_install_cant_create_key           49
#define isc_install_cant_set_value            50
#define isc_install_cant_open_key             51
#define isc_install_cant_delete_key           52
#define isc_install_cant_query_key            53
#define isc_install_cant_delete_value         54

/* OS services related errors */

#define isc_install_service_existed           55
#define isc_install_cant_create_service       56
#define isc_install_cant_open_service         57
#define isc_install_cant_query_service        58
#define isc_install_service_running           59
#define isc_install_cant_delete_service       60
#define isc_install_cant_open_manager         61
#define isc_install_system_error              62
#define isc_install_com_regfail               63
#define isc_install_dcom_required             64

/* ODBC installation errors */

#define isc_install_odbc_general              65
#define isc_install_core_version              66
#define isc_install_drv_version               67
#define isc_install_tran_version              68


/* InterBase Install API entry points */

#ifdef __cplusplus
extern "C" {
#endif
MSG_NO  ISC_EXPORT isc_install_clear_options(OPTIONS_HANDLE*);

MSG_NO  ISC_EXPORT isc_install_execute(OPTIONS_HANDLE,
                                       TEXT*,
                                       TEXT*,
                                       FP_STATUS,
                                       void*,
                                       FP_ERROR,
                                       void*,
                                       TEXT*);

MSG_NO  ISC_EXPORT isc_install_get_info(int, OPT, void*, unsigned);

MSG_NO  ISC_EXPORT isc_install_get_message(OPTIONS_HANDLE,
                                           MSG_NO,
                                           void*,
                                           unsigned);

MSG_NO ISC_EXPORT isc_install_load_external_text(TEXT*);

MSG_NO  ISC_EXPORT isc_install_precheck(OPTIONS_HANDLE,
                                        TEXT*,
                                        TEXT*);

MSG_NO  ISC_EXPORT  isc_install_set_option(OPTIONS_HANDLE*,
                                           OPT);

MSG_NO  ISC_EXPORT isc_uninstall_execute(TEXT*,
                                         FP_STATUS,
                                         void*,
                                         FP_ERROR,
                                         void*);

MSG_NO  ISC_EXPORT isc_uninstall_precheck(TEXT*);

MSG_NO  ISC_EXPORT isc_install_unset_option(OPTIONS_HANDLE*,
                                            OPT);
#ifdef __cplusplus
}
#endif

#endif /* _IBINSTALL_H_ */
