{*******************************************************************************
Author(s):
Henri Gourvest <hgourvest@progdigy.com>
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
https://uib.svn.sourceforge.net/svnroot/uib (current code is from the trunk rev 364)
http://www.progdigy.com/modules.php?name=UIB
*******************************************************************************}

unit ALFBXError;

interface

//***********************/
//*   ISC Error Codes   */
//***********************/

const
  isc_facility = 20;
  isc_base     = 335544320;
  isc_factor   = 1;

  isc_arg_end          = 0;  (* end of argument list *)
  isc_arg_gds          = 1;  (* generic DSRI status value *)
  isc_arg_string       = 2;  (* string argument *)
  isc_arg_cstring      = 3;  (* count & string argument *)
  isc_arg_number       = 4;  (* numeric argument (long) *)
  isc_arg_interpreted  = 5;  (* interpreted status code (string) *)
  isc_arg_vms          = 6;  (* VAX/VMS status code (long) *)
  isc_arg_unix         = 7;  (* UNIX error code *)
  isc_arg_domain       = 8;  (* Apollo/Domain error code *)
  isc_arg_dos          = 9;  (* MSDOS/OS2 error code *)
  isc_arg_mpexl        = 10; (* HP MPE/XL error code *)
  isc_arg_mpexl_ipc    = 11; (* HP MPE/XL IPC error code *)
  isc_arg_next_mach    = 15; (* NeXT/Mach error code *)
  isc_arg_netware      = 16; (* NetWare error code *)
  isc_arg_win32        = 17; (* Win32 error code *)
  isc_arg_warning      = 18; (* warning argument *)
{FB25_UP}
  isc_arg_sql_state    = 19; // SQLSTATE
{FB25_UP}

  isc_arith_except                      = 335544321;
  isc_bad_dbkey                         = 335544322;
  isc_bad_db_format                     = 335544323;
  isc_bad_db_handle                     = 335544324;
  isc_bad_dpb_content                   = 335544325;
  isc_bad_dpb_form                      = 335544326;
  isc_bad_req_handle                    = 335544327;
  isc_bad_segstr_handle                 = 335544328;
  isc_bad_segstr_id                     = 335544329;
  isc_bad_tpb_content                   = 335544330;
  isc_bad_tpb_form                      = 335544331;
  isc_bad_trans_handle                  = 335544332;
  isc_bug_check                         = 335544333;
  isc_convert_error                     = 335544334;
  isc_db_corrupt                        = 335544335;
  isc_deadlock                          = 335544336;
  isc_excess_trans                      = 335544337;
  isc_from_no_match                     = 335544338;
  isc_infinap                           = 335544339;
  isc_infona                            = 335544340;
  isc_infunk                            = 335544341;
  isc_integ_fail                        = 335544342;
  isc_invalid_blr                       = 335544343;
  isc_io_error                          = 335544344;
  isc_lock_conflict                     = 335544345;
  isc_metadata_corrupt                  = 335544346;
  isc_not_valid                         = 335544347;
  isc_no_cur_rec                        = 335544348;
  isc_no_dup                            = 335544349;
  isc_no_finish                         = 335544350;
  isc_no_meta_update                    = 335544351;
  isc_no_priv                           = 335544352;
  isc_no_recon                          = 335544353;
  isc_no_record                         = 335544354;
  isc_no_segstr_close                   = 335544355;
  isc_obsolete_metadata                 = 335544356;
  isc_open_trans                        = 335544357;
  isc_port_len                          = 335544358;
  isc_read_only_field                   = 335544359;
  isc_read_only_rel                     = 335544360;
  isc_read_only_trans                   = 335544361;
  isc_read_only_view                    = 335544362;
  isc_req_no_trans                      = 335544363;
  isc_req_sync                          = 335544364;
  isc_req_wrong_db                      = 335544365;
  isc_segment                           = 335544366;
  isc_segstr_eof                        = 335544367;
  isc_segstr_no_op                      = 335544368;
  isc_segstr_no_read                    = 335544369;
  isc_segstr_no_trans                   = 335544370;
  isc_segstr_no_write                   = 335544371;
  isc_segstr_wrong_db                   = 335544372;
  isc_sys_request                       = 335544373;
  isc_stream_eof                        = 335544374;
  isc_unavailable                       = 335544375;
  isc_unres_rel                         = 335544376;
  isc_uns_ext                           = 335544377;
  isc_wish_list                         = 335544378;
  isc_wrong_ods                         = 335544379;
  isc_wronumarg                         = 335544380;
  isc_imp_exc                           = 335544381;
  isc_random                            = 335544382;
  isc_fatal_conflict                    = 335544383;
  isc_badblk                            = 335544384;
  isc_invpoolcl                         = 335544385;
  isc_nopoolids                         = 335544386;
  isc_relbadblk                         = 335544387;
  isc_blktoobig                         = 335544388;
  isc_bufexh                            = 335544389;
  isc_syntaxerr                         = 335544390;
  isc_bufinuse                          = 335544391;
  isc_bdbincon                          = 335544392;
  isc_reqinuse                          = 335544393;
  isc_badodsver                         = 335544394;
  isc_relnotdef                         = 335544395;
  isc_fldnotdef                         = 335544396;
  isc_dirtypage                         = 335544397;
  isc_waifortra                         = 335544398;
  isc_doubleloc                         = 335544399;
  isc_nodnotfnd                         = 335544400;
  isc_dupnodfnd                         = 335544401;
  isc_locnotmar                         = 335544402;
  isc_badpagtyp                         = 335544403;
  isc_corrupt                           = 335544404;
  isc_badpage                           = 335544405;
  isc_badindex                          = 335544406;
  isc_dbbnotzer                         = 335544407;
  isc_tranotzer                         = 335544408;
  isc_trareqmis                         = 335544409;
  isc_badhndcnt                         = 335544410;
  isc_wrotpbver                         = 335544411;
  isc_wroblrver                         = 335544412;
  isc_wrodpbver                         = 335544413;
  isc_blobnotsup                        = 335544414;
  isc_badrelation                       = 335544415;
  isc_nodetach                          = 335544416;
  isc_notremote                         = 335544417;
  isc_trainlim                          = 335544418;
  isc_notinlim                          = 335544419;
  isc_traoutsta                         = 335544420;
  isc_connect_reject                    = 335544421;
  isc_dbfile                            = 335544422;
  isc_orphan                            = 335544423;
  isc_no_lock_mgr                       = 335544424;
  isc_ctxinuse                          = 335544425;
  isc_ctxnotdef                         = 335544426;
  isc_datnotsup                         = 335544427;
  isc_badmsgnum                         = 335544428;
  isc_badparnum                         = 335544429;
  isc_virmemexh                         = 335544430;
  isc_blocking_signal                   = 335544431;
  isc_lockmanerr                        = 335544432;
  isc_journerr                          = 335544433;
  isc_keytoobig                         = 335544434;
  isc_nullsegkey                        = 335544435;
  isc_sqlerr                            = 335544436;
  isc_wrodynver                         = 335544437;
  isc_funnotdef                         = 335544438;
  isc_funmismat                         = 335544439;
  isc_bad_msg_vec                       = 335544440;
  isc_bad_detach                        = 335544441;
  isc_noargacc_read                     = 335544442;
  isc_noargacc_write                    = 335544443;
  isc_read_only                         = 335544444;
  isc_ext_err                           = 335544445;
  isc_non_updatable                     = 335544446;
  isc_no_rollback                       = 335544447;
  isc_bad_sec_info                      = 335544448;
  isc_invalid_sec_info                  = 335544449;
  isc_misc_interpreted                  = 335544450;
  isc_update_conflict                   = 335544451;
  isc_unlicensed                        = 335544452;
  isc_obj_in_use                        = 335544453;
  isc_nofilter                          = 335544454;
  isc_shadow_accessed                   = 335544455;
  isc_invalid_sdl                       = 335544456;
  isc_out_of_bounds                     = 335544457;
  isc_invalid_dimension                 = 335544458;
  isc_rec_in_limbo                      = 335544459;
  isc_shadow_missing                    = 335544460;
  isc_cant_validate                     = 335544461;
  isc_cant_start_journal                = 335544462;
  isc_gennotdef                         = 335544463;
  isc_cant_start_logging                = 335544464;
  isc_bad_segstr_type                   = 335544465;
  isc_foreign_key                       = 335544466;
  isc_high_minor                        = 335544467;
  isc_tra_state                         = 335544468;
  isc_trans_invalid                     = 335544469;
  isc_buf_invalid                       = 335544470;
  isc_indexnotdefined                   = 335544471;
  isc_login                             = 335544472;
  isc_invalid_bookmark                  = 335544473;
  isc_bad_lock_level                    = 335544474;
  isc_relation_lock                     = 335544475;
  isc_record_lock                       = 335544476;
  isc_max_idx                           = 335544477;
  isc_jrn_enable                        = 335544478;
  isc_old_failure                       = 335544479;
  isc_old_in_progress                   = 335544480;
  isc_old_no_space                      = 335544481;
  isc_no_wal_no_jrn                     = 335544482;
  isc_num_old_files                     = 335544483;
  isc_wal_file_open                     = 335544484;
  isc_bad_stmt_handle                   = 335544485;
  isc_wal_failure                       = 335544486;
  isc_walw_err                          = 335544487;
  isc_logh_small                        = 335544488;
  isc_logh_inv_version                  = 335544489;
  isc_logh_open_flag                    = 335544490;
  isc_logh_open_flag2                   = 335544491;
  isc_logh_diff_dbname                  = 335544492;
  isc_logf_unexpected_eof               = 335544493;
  isc_logr_incomplete                   = 335544494;
  isc_logr_header_small                 = 335544495;
  isc_logb_small                        = 335544496;
  isc_wal_illegal_attach                = 335544497;
  isc_wal_invalid_wpb                   = 335544498;
  isc_wal_err_rollover                  = 335544499;
  isc_no_wal                            = 335544500;
  isc_drop_wal                          = 335544501;
  isc_stream_not_defined                = 335544502;
  isc_wal_subsys_error                  = 335544503;
  isc_wal_subsys_corrupt                = 335544504;
  isc_no_archive                        = 335544505;
  isc_shutinprog                        = 335544506;
  isc_range_in_use                      = 335544507;
  isc_range_not_found                   = 335544508;
  isc_charset_not_found                 = 335544509;
  isc_lock_timeout                      = 335544510;
  isc_prcnotdef                         = 335544511;
  isc_prcmismat                         = 335544512;
  isc_wal_bugcheck                      = 335544513;
  isc_wal_cant_expand                   = 335544514;
  isc_codnotdef                         = 335544515;
  isc_xcpnotdef                         = 335544516;
  isc_except                            = 335544517;
  isc_cache_restart                     = 335544518;
  isc_bad_lock_handle                   = 335544519;
  isc_jrn_present                       = 335544520;
  isc_wal_err_rollover2                 = 335544521;
  isc_wal_err_logwrite                  = 335544522;
  isc_wal_err_jrn_comm                  = 335544523;
  isc_wal_err_expansion                 = 335544524;
  isc_wal_err_setup                     = 335544525;
  isc_wal_err_ww_sync                   = 335544526;
  isc_wal_err_ww_start                  = 335544527;
  isc_shutdown                          = 335544528;
  isc_existing_priv_mod                 = 335544529;
  isc_primary_key_ref                   = 335544530;
  isc_primary_key_notnull               = 335544531;
  isc_ref_cnstrnt_notfound              = 335544532;
  isc_foreign_key_notfound              = 335544533;
  isc_ref_cnstrnt_update                = 335544534;
  isc_check_cnstrnt_update              = 335544535;
  isc_check_cnstrnt_del                 = 335544536;
  isc_integ_index_seg_del               = 335544537;
  isc_integ_index_seg_mod               = 335544538;
  isc_integ_index_del                   = 335544539;
  isc_integ_index_mod                   = 335544540;
  isc_check_trig_del                    = 335544541;
  isc_check_trig_update                 = 335544542;
  isc_cnstrnt_fld_del                   = 335544543;
  isc_cnstrnt_fld_rename                = 335544544;
  isc_rel_cnstrnt_update                = 335544545;
  isc_constaint_on_view                 = 335544546;
  isc_invld_cnstrnt_type                = 335544547;
  isc_primary_key_exists                = 335544548;
  isc_systrig_update                    = 335544549;
  isc_not_rel_owner                     = 335544550;
  isc_grant_obj_notfound                = 335544551;
  isc_grant_fld_notfound                = 335544552;
  isc_grant_nopriv                      = 335544553;
  isc_nonsql_security_rel               = 335544554;
  isc_nonsql_security_fld               = 335544555;
  isc_wal_cache_err                     = 335544556;
  isc_shutfail                          = 335544557;
  isc_check_constraint                  = 335544558;
  isc_bad_svc_handle                    = 335544559;
  isc_shutwarn                          = 335544560;
  isc_wrospbver                         = 335544561;
  isc_bad_spb_form                      = 335544562;
  isc_svcnotdef                         = 335544563;
  isc_no_jrn                            = 335544564;
  isc_transliteration_failed            = 335544565;
  isc_start_cm_for_wal                  = 335544566;
  isc_wal_ovflow_log_required           = 335544567;
  isc_text_subtype                      = 335544568;
  isc_dsql_error                        = 335544569;
  isc_dsql_command_err                  = 335544570;
  isc_dsql_constant_err                 = 335544571;
  isc_dsql_cursor_err                   = 335544572;
  isc_dsql_datatype_err                 = 335544573;
  isc_dsql_decl_err                     = 335544574;
  isc_dsql_cursor_update_err            = 335544575;
  isc_dsql_cursor_open_err              = 335544576;
  isc_dsql_cursor_close_err             = 335544577;
  isc_dsql_field_err                    = 335544578;
  isc_dsql_internal_err                 = 335544579;
  isc_dsql_relation_err                 = 335544580;
  isc_dsql_procedure_err                = 335544581;
  isc_dsql_request_err                  = 335544582;
  isc_dsql_sqlda_err                    = 335544583;
  isc_dsql_var_count_err                = 335544584;
  isc_dsql_stmt_handle                  = 335544585;
  isc_dsql_function_err                 = 335544586;
  isc_dsql_blob_err                     = 335544587;
  isc_collation_not_found               = 335544588;
  isc_collation_not_for_charset         = 335544589;
  isc_dsql_dup_option                   = 335544590;
  isc_dsql_tran_err                     = 335544591;
  isc_dsql_invalid_array                = 335544592;
  isc_dsql_max_arr_dim_exceeded         = 335544593;
  isc_dsql_arr_range_error              = 335544594;
  isc_dsql_trigger_err                  = 335544595;
  isc_dsql_subselect_err                = 335544596;
  isc_dsql_crdb_prepare_err             = 335544597;
  isc_specify_field_err                 = 335544598;
  isc_num_field_err                     = 335544599;
  isc_col_name_err                      = 335544600;
  isc_where_err                         = 335544601;
  isc_table_view_err                    = 335544602;
  isc_distinct_err                      = 335544603;
  isc_key_field_count_err               = 335544604;
  isc_subquery_err                      = 335544605;
  isc_expression_eval_err               = 335544606;
  isc_node_err                          = 335544607;
  isc_command_end_err                   = 335544608;
  isc_index_name                        = 335544609;
  isc_exception_name                    = 335544610;
  isc_field_name                        = 335544611;
  isc_token_err                         = 335544612;
  isc_union_err                         = 335544613;
  isc_dsql_construct_err                = 335544614;
  isc_field_aggregate_err               = 335544615;
  isc_field_ref_err                     = 335544616;
  isc_order_by_err                      = 335544617;
  isc_return_mode_err                   = 335544618;
  isc_extern_func_err                   = 335544619;
  isc_alias_conflict_err                = 335544620;
  isc_procedure_conflict_error          = 335544621;
  isc_relation_conflict_err             = 335544622;
  isc_dsql_domain_err                   = 335544623;
  isc_idx_seg_err                       = 335544624;
  isc_node_name_err                     = 335544625;
  isc_table_name                        = 335544626;
  isc_proc_name                         = 335544627;
  isc_idx_create_err                    = 335544628;
  isc_wal_shadow_err                    = 335544629;
  isc_dependency                        = 335544630;
  isc_idx_key_err                       = 335544631;
  isc_dsql_file_length_err              = 335544632;
  isc_dsql_shadow_number_err            = 335544633;
  isc_dsql_token_unk_err                = 335544634;
  isc_dsql_no_relation_alias            = 335544635;
  isc_indexname                         = 335544636;
  isc_no_stream_plan                    = 335544637;
  isc_stream_twice                      = 335544638;
  isc_stream_not_found                  = 335544639;
  isc_collation_requires_text           = 335544640;
  isc_dsql_domain_not_found             = 335544641;
  isc_index_unused                      = 335544642;
  isc_dsql_self_join                    = 335544643;
  isc_stream_bof                        = 335544644;
  isc_stream_crack                      = 335544645;
  isc_db_or_file_exists                 = 335544646;
  isc_invalid_operator                  = 335544647;
  isc_conn_lost                         = 335544648;
  isc_bad_checksum                      = 335544649;
  isc_page_type_err                     = 335544650;
  isc_ext_readonly_err                  = 335544651;
  isc_sing_select_err                   = 335544652;
  isc_psw_attach                        = 335544653;
  isc_psw_start_trans                   = 335544654;
  isc_invalid_direction                 = 335544655;
  isc_dsql_var_conflict                 = 335544656;
  isc_dsql_no_blob_array                = 335544657;
  isc_dsql_base_table                   = 335544658;
  isc_duplicate_base_table              = 335544659;
  isc_view_alias                        = 335544660;
  isc_index_root_page_full              = 335544661;
  isc_dsql_blob_type_unknown            = 335544662;
  isc_req_max_clones_exceeded           = 335544663;
  isc_dsql_duplicate_spec               = 335544664;
  isc_unique_key_violation              = 335544665;
  isc_srvr_version_too_old              = 335544666;
  isc_drdb_completed_with_errs          = 335544667;
  isc_dsql_procedure_use_err            = 335544668;
  isc_dsql_count_mismatch               = 335544669;
  isc_blob_idx_err                      = 335544670;
  isc_array_idx_err                     = 335544671;
  isc_key_field_err                     = 335544672;
  isc_no_delete                         = 335544673;
  isc_del_last_field                    = 335544674;
  isc_sort_err                          = 335544675;
  isc_sort_mem_err                      = 335544676;
  isc_version_err                       = 335544677;
  isc_inval_key_posn                    = 335544678;
  isc_no_segments_err                   = 335544679;
  isc_crrp_data_err                     = 335544680;
  isc_rec_size_err                      = 335544681;
  isc_dsql_field_ref                    = 335544682;
  isc_req_depth_exceeded                = 335544683;
  isc_no_field_access                   = 335544684;
  isc_no_dbkey                          = 335544685;
  isc_jrn_format_err                    = 335544686;
  isc_jrn_file_full                     = 335544687;
  isc_dsql_open_cursor_request          = 335544688;
  isc_ib_error                          = 335544689;
  isc_cache_redef                       = 335544690;
  isc_cache_too_small                   = 335544691;
  isc_log_redef                         = 335544692;
  isc_log_too_small                     = 335544693;
  isc_partition_too_small               = 335544694;
  isc_partition_not_supp                = 335544695;
  isc_log_length_spec                   = 335544696;
  isc_precision_err                     = 335544697;
  isc_scale_nogt                        = 335544698;
  isc_expec_short                       = 335544699;
  isc_expec_long                        = 335544700;
  isc_expec_ushort                      = 335544701;
{FB25_UP}
  isc_escape_invalid                    = 335544702;
{FB25_UP}
  isc_like_escape_invalid               = 335544702;
  isc_svcnoexe                          = 335544703;
  isc_net_lookup_err                    = 335544704;
  isc_service_unknown                   = 335544705;
  isc_host_unknown                      = 335544706;
  isc_grant_nopriv_on_base              = 335544707;
  isc_dyn_fld_ambiguous                 = 335544708;
  isc_dsql_agg_ref_err                  = 335544709;
  isc_complex_view                      = 335544710;
  isc_unprepared_stmt                   = 335544711;
  isc_expec_positive                    = 335544712;
  isc_dsql_sqlda_value_err              = 335544713;
  isc_invalid_array_id                  = 335544714;
  isc_extfile_uns_op                    = 335544715;
  isc_svc_in_use                        = 335544716;
  isc_err_stack_limit                   = 335544717;
  isc_invalid_key                       = 335544718;
  isc_net_init_error                    = 335544719;
  isc_loadlib_failure                   = 335544720;
  isc_network_error                     = 335544721;
  isc_net_connect_err                   = 335544722;
  isc_net_connect_listen_err            = 335544723;
  isc_net_event_connect_err             = 335544724;
  isc_net_event_listen_err              = 335544725;
  isc_net_read_err                      = 335544726;
  isc_net_write_err                     = 335544727;
  isc_integ_index_deactivate            = 335544728;
  isc_integ_deactivate_primary          = 335544729;
  isc_cse_not_supported                 = 335544730;
  isc_tra_must_sweep                    = 335544731;
  isc_unsupported_network_drive         = 335544732;
  isc_io_create_err                     = 335544733;
  isc_io_open_err                       = 335544734;
  isc_io_close_err                      = 335544735;
  isc_io_read_err                       = 335544736;
  isc_io_write_err                      = 335544737;
  isc_io_delete_err                     = 335544738;
  isc_io_access_err                     = 335544739;
  isc_udf_exception                     = 335544740;
  isc_lost_db_connection                = 335544741;
  isc_no_write_user_priv                = 335544742;
  isc_token_too_long                    = 335544743;
  isc_max_att_exceeded                  = 335544744;
  isc_login_same_as_role_name           = 335544745;
  isc_reftable_requires_pk              = 335544746;
  isc_usrname_too_long                  = 335544747;
  isc_password_too_long                 = 335544748;
  isc_usrname_required                  = 335544749;
  isc_password_required                 = 335544750;
  isc_bad_protocol                      = 335544751;
  isc_dup_usrname_found                 = 335544752;
  isc_usrname_not_found                 = 335544753;
  isc_error_adding_sec_record           = 335544754;
  isc_error_modifying_sec_record        = 335544755;
  isc_error_deleting_sec_record         = 335544756;
  isc_error_updating_sec_db             = 335544757;
  isc_sort_rec_size_err                 = 335544758;
  isc_bad_default_value                 = 335544759;
  isc_invalid_clause                    = 335544760;
  isc_too_many_handles                  = 335544761;
  isc_optimizer_blk_exc                 = 335544762;
  isc_invalid_string_constant           = 335544763;
  isc_transitional_date                 = 335544764;
  isc_read_only_database                = 335544765;
  isc_must_be_dialect_2_and_up          = 335544766;
  isc_blob_filter_exception             = 335544767;
  isc_exception_access_violation        = 335544768;
  isc_exception_datatype_missalignment  = 335544769;
  isc_exception_array_bounds_exceeded   = 335544770;
  isc_exception_float_denormal_operand  = 335544771;
  isc_exception_float_divide_by_zero    = 335544772;
  isc_exception_float_inexact_result    = 335544773;
  isc_exception_float_invalid_operand   = 335544774;
  isc_exception_float_overflow          = 335544775;
  isc_exception_float_stack_check       = 335544776;
  isc_exception_float_underflow         = 335544777;
  isc_exception_integer_divide_by_zero  = 335544778;
  isc_exception_integer_overflow        = 335544779;
  isc_exception_unknown                 = 335544780;
  isc_exception_stack_overflow          = 335544781;
  isc_exception_sigsegv                 = 335544782;
  isc_exception_sigill                  = 335544783;
  isc_exception_sigbus                  = 335544784;
  isc_exception_sigfpe                  = 335544785;
  isc_ext_file_delete                   = 335544786;
  isc_ext_file_modify                   = 335544787;
  isc_adm_task_denied                   = 335544788;
  isc_extract_input_mismatch            = 335544789;
  isc_insufficient_svc_privileges       = 335544790;
  isc_file_in_use                       = 335544791;
  isc_service_att_err                   = 335544792;
  isc_ddl_not_allowed_by_db_sql_dial    = 335544793;
  isc_cancelled                         = 335544794;
  isc_unexp_spb_form                    = 335544795;
  isc_sql_dialect_datatype_unsupport    = 335544796;
  isc_svcnouser                         = 335544797;
  isc_depend_on_uncommitted_rel         = 335544798;
  isc_svc_name_missing                  = 335544799;
  isc_too_many_contexts                 = 335544800;
  isc_datype_notsup                     = 335544801;
  isc_dialect_reset_warning             = 335544802;
  isc_dialect_not_changed               = 335544803;
  isc_database_create_failed            = 335544804;
  isc_inv_dialect_specified             = 335544805;
  isc_valid_db_dialects                 = 335544806;
  isc_sqlwarn                           = 335544807;
  isc_dtype_renamed                     = 335544808;
  isc_extern_func_dir_error             = 335544809;
  isc_date_range_exceeded               = 335544810;
  isc_inv_client_dialect_specified      = 335544811;
  isc_valid_client_dialects             = 335544812;
  isc_optimizer_between_err             = 335544813;
  isc_service_not_supported             = 335544814;

  isc_generator_name                    = 335544815;
  isc_udf_name                          = 335544816;
  isc_bad_limit_param                   = 335544817;
  isc_bad_skip_param                    = 335544818;
  isc_io_32bit_exceeded_err             = 335544819;

{FB15_UP}
  isc_invalid_savepoint                 = 335544820;
  isc_dsql_column_pos_err               = 335544821;
  isc_dsql_agg_where_err                = 335544822;
  isc_dsql_agg_group_err                = 335544823;
  isc_dsql_agg_column_err               = 335544824;
  isc_dsql_agg_having_err               = 335544825;
  isc_dsql_agg_nested_err               = 335544826;
  isc_exec_sql_invalid_arg              = 335544827;
  isc_exec_sql_invalid_req              = 335544828;
  isc_exec_sql_invalid_var              = 335544829;
  isc_exec_sql_max_call_exceeded        = 335544830;
  isc_conf_access_denied                = 335544831;
{FB15_UP}

{FB20_UP}
  isc_wrong_backup_state               = 335544832;
  isc_wal_backup_err                   = 335544833;
  isc_cursor_not_open                  = 335544834;
  isc_bad_shutdown_mode                = 335544835;
  isc_concat_overflow                  = 335544836;
  isc_bad_substring_offset             = 335544837;
  isc_foreign_key_target_doesnt_exist  = 335544838;
  isc_foreign_key_references_present   = 335544839;
  isc_no_update                        = 335544840;
  isc_cursor_already_open              = 335544841;
  isc_stack_trace                      = 335544842;
  isc_ctx_var_not_found                = 335544843;
  isc_ctx_namespace_invalid            = 335544844;
  isc_ctx_too_big                      = 335544845;
  isc_ctx_bad_argument                 = 335544846;
  isc_identifier_too_long              = 335544847;
  isc_except2                          = 335544848;
  isc_malformed_string                 = 335544849;
  isc_prc_out_param_mismatch           = 335544850;
  isc_command_end_err2                 = 335544851;
  isc_partner_idx_incompat_type        = 335544852;
  isc_bad_substring_length             = 335544853;
  isc_charset_not_installed            = 335544854;
  isc_collation_not_installed          = 335544855;
  isc_att_shutdown                     = 335544856;
  isc_blobtoobig                       = 335544857;
  isc_must_have_phys_field             = 335544858;
  isc_invalid_time_precision           = 335544859;
  isc_blob_convert_error               = 335544860;
  isc_array_convert_error              = 335544861;
  isc_record_lock_not_supp             = 335544862;
  isc_partner_idx_not_found            = 335544863;
  isc_tra_num_exc                      = 335544864;
  isc_field_disappeared                = 335544865;
{FB20_UP}

{FB21_UP}
  isc_met_wrong_gtt_scope              = 335544866;
  isc_subtype_for_internal_use         = 335544867;
  isc_illegal_prc_type                 = 335544868;
  isc_invalid_sort_datatype            = 335544869;
  isc_collation_name                   = 335544870;
  isc_domain_name                      = 335544871;
  isc_domnotdef                        = 335544872;
  isc_array_max_dimensions             = 335544873;
  isc_max_db_per_trans_allowed         = 335544874;
  isc_bad_debug_format                 = 335544875;
  isc_bad_proc_BLR                     = 335544876;
  isc_key_too_big                      = 335544877;
  isc_concurrent_transaction           = 335544878;
  isc_not_valid_for_var                = 335544879;
  isc_not_valid_for                    = 335544880;
  isc_need_difference                  = 335544881;
  isc_long_login                       = 335544882;
  isc_fldnotdef2                       = 335544883;
{FB21_UP}

{FB25_UP}
  isc_invalid_similar_pattern          = 335544884;
  isc_bad_teb_form                     = 335544885;
  isc_tpb_multiple_txn_isolation       = 335544886;
  isc_tpb_reserv_before_table          = 335544887;
  isc_tpb_multiple_spec                = 335544888;
  isc_tpb_option_without_rc            = 335544889;
  isc_tpb_conflicting_options          = 335544890;
  isc_tpb_reserv_missing_tlen          = 335544891;
  isc_tpb_reserv_long_tlen             = 335544892;
  isc_tpb_reserv_missing_tname         = 335544893;
  isc_tpb_reserv_corrup_tlen           = 335544894;
  isc_tpb_reserv_null_tlen             = 335544895;
  isc_tpb_reserv_relnotfound           = 335544896;
  isc_tpb_reserv_baserelnotfound       = 335544897;
  isc_tpb_missing_len                  = 335544898;
  isc_tpb_missing_value                = 335544899;
  isc_tpb_corrupt_len                  = 335544900;
  isc_tpb_null_len                     = 335544901;
  isc_tpb_overflow_len                 = 335544902;
  isc_tpb_invalid_value                = 335544903;
  isc_tpb_reserv_stronger_wng          = 335544904;
  isc_tpb_reserv_stronger              = 335544905;
  isc_tpb_reserv_max_recursion         = 335544906;
  isc_tpb_reserv_virtualtbl            = 335544907;
  isc_tpb_reserv_systbl                = 335544908;
  isc_tpb_reserv_temptbl               = 335544909;
  isc_tpb_readtxn_after_writelock      = 335544910;
  isc_tpb_writelock_after_readtxn      = 335544911;
  isc_time_range_exceeded              = 335544912;
  isc_datetime_range_exceeded          = 335544913;
  isc_string_truncation                = 335544914;
  isc_blob_truncation                  = 335544915;
  isc_numeric_out_of_range             = 335544916;
  isc_shutdown_timeout                 = 335544917;
  isc_att_handle_busy                  = 335544918;
  isc_bad_udf_freeit                   = 335544919;
  isc_eds_provider_not_found           = 335544920;
  isc_eds_connection                   = 335544921;
  isc_eds_preprocess                   = 335544922;
  isc_eds_stmt_expected                = 335544923;
  isc_eds_prm_name_expected            = 335544924;
  isc_eds_unclosed_comment             = 335544925;
  isc_eds_statement                    = 335544926;
  isc_eds_input_prm_mismatch           = 335544927;
  isc_eds_output_prm_mismatch          = 335544928;
  isc_eds_input_prm_not_set            = 335544929;
  isc_too_big_blr                      = 335544930;
  isc_montabexh                        = 335544931;
  isc_modnotfound                      = 335544932;
  isc_nothing_to_cancel                = 335544933;
  isc_ibutil_not_loaded                = 335544934;
  isc_circular_computed                = 335544935;
  isc_psw_db_error                     = 335544936;
  isc_invalid_type_datetime_op         = 335544937;
  isc_onlycan_add_timetodate           = 335544938;
  isc_onlycan_add_datetotime           = 335544939;
  isc_onlycansub_tstampfromtstamp      = 335544940;
  isc_onlyoneop_mustbe_tstamp          = 335544941;
  isc_invalid_extractpart_time         = 335544942;
  isc_invalid_extractpart_date         = 335544943;
  isc_invalidarg_extract               = 335544944;
  isc_sysf_argmustbe_exact             = 335544945;
  isc_sysf_argmustbe_exact_or_fp       = 335544946;
  isc_sysf_argviolates_uuidtype        = 335544947;
  isc_sysf_argviolates_uuidlen         = 335544948;
  isc_sysf_argviolates_uuidfmt         = 335544949;
  isc_sysf_argviolates_guidigits       = 335544950;
  isc_sysf_invalid_addpart_time        = 335544951;
  isc_sysf_invalid_add_datetime        = 335544952;
  isc_sysf_invalid_addpart_dtime       = 335544953;
  isc_sysf_invalid_add_dtime_rc        = 335544954;
  isc_sysf_invalid_diff_dtime          = 335544955;
  isc_sysf_invalid_timediff            = 335544956;
  isc_sysf_invalid_tstamptimediff      = 335544957;
  isc_sysf_invalid_datetimediff        = 335544958;
  isc_sysf_invalid_diffpart            = 335544959;
  isc_sysf_argmustbe_positive          = 335544960;
  isc_sysf_basemustbe_positive         = 335544961;
  isc_sysf_argnmustbe_nonneg           = 335544962;
  isc_sysf_argnmustbe_positive         = 335544963;
  isc_sysf_invalid_zeropowneg          = 335544964;
  isc_sysf_invalid_negpowfp            = 335544965;
  isc_sysf_invalid_scale               = 335544966;
  isc_sysf_argmustbe_nonneg            = 335544967;
  isc_sysf_binuuid_mustbe_str          = 335544968;
  isc_sysf_binuuid_wrongsize           = 335544969;
  isc_missing_required_spb             = 335544970;
  isc_net_server_shutdown              = 335544971;
  isc_bad_conn_str                     = 335544972;
  isc_bad_epb_form                     = 335544973;
  isc_no_threads                       = 335544974;
  isc_net_event_connect_timeout        = 335544975;
  isc_sysf_argmustbe_nonzero           = 335544976;
  isc_sysf_argmustbe_range_inc1_1      = 335544977;
  isc_sysf_argmustbe_gteq_one          = 335544978;
  isc_sysf_argmustbe_range_exc1_1      = 335544979;
  isc_internal_rejected_params         = 335544980;
  isc_sysf_fp_overflow                 = 335544981;
  isc_udf_fp_overflow                  = 335544982;
  isc_udf_fp_nan                       = 335544983;
  isc_instance_conflict                = 335544984;
  isc_out_of_temp_space                = 335544985;
  isc_eds_expl_tran_ctrl               = 335544986;
  isc_no_trusted_spb                   = 335544987;
  isc_async_active                     = 335545017;
{FB25_UP}

  isc_gfix_db_name                      = 335740929;
  isc_gfix_invalid_sw                   = 335740930;
  isc_gfix_incmp_sw                     = 335740932;
  isc_gfix_replay_req                   = 335740933;
  isc_gfix_pgbuf_req                    = 335740934;
  isc_gfix_val_req                      = 335740935;
  isc_gfix_pval_req                     = 335740936;
  isc_gfix_trn_req                      = 335740937;
  isc_gfix_full_req                     = 335740940;
  isc_gfix_usrname_req                  = 335740941;
  isc_gfix_pass_req                     = 335740942;
  isc_gfix_subs_name                    = 335740943;
  isc_gfix_wal_req                      = 335740944;
  isc_gfix_sec_req                      = 335740945;
  isc_gfix_nval_req                     = 335740946;
  isc_gfix_type_shut                    = 335740947;
  isc_gfix_retry                        = 335740948;
  isc_gfix_retry_db                     = 335740951;
  isc_gfix_exceed_max                   = 335740991;
  isc_gfix_corrupt_pool                 = 335740992;
  isc_gfix_mem_exhausted                = 335740993;
  isc_gfix_bad_pool                     = 335740994;
  isc_gfix_trn_not_valid                = 335740995;
  isc_gfix_unexp_eoi                    = 335741012;
  isc_gfix_recon_fail                   = 335741018;
  isc_gfix_trn_unknown                  = 335741036;
  isc_gfix_mode_req                     = 335741038;
  isc_gfix_opt_SQL_dialect              = 335741039;
{FB20_UP}
  isc_gfix_pzval_req                    = 335741042;
{FB20_UP}
  isc_dsql_dbkey_from_non_table         = 336003074;
  isc_dsql_transitional_numeric         = 336003075;
  isc_dsql_dialect_warning_expr         = 336003076;
  isc_sql_db_dialect_dtype_unsupport    = 336003077;
  isc_isc_sql_dialect_conflict_num      = 336003079;
  isc_dsql_warning_number_ambiguous     = 336003080;
  isc_dsql_warning_number_ambiguous1    = 336003081;
  isc_dsql_warn_precision_ambiguous     = 336003082;
  isc_dsql_warn_precision_ambiguous1    = 336003083;
  isc_dsql_warn_precision_ambiguous2    = 336003084;

  isc_dsql_ambiguous_field_name         = 336003085;
  isc_dsql_udf_return_pos_err           = 336003086;

{FB15_UP}
  isc_dsql_invalid_label                = 336003087;
  isc_dsql_datatypes_not_comparable     = 336003088;
{FB15_UP}

{FB20_UP}
  isc_dsql_cursor_invalid              = 336003089;
  isc_dsql_cursor_redefined            = 336003090;
  isc_dsql_cursor_not_found            = 336003091;
  isc_dsql_cursor_exists               = 336003092;
  isc_dsql_cursor_rel_ambiguous        = 336003093;
  isc_dsql_cursor_rel_not_found        = 336003094;
  isc_dsql_cursor_not_open             = 336003095;
  isc_dsql_type_not_supp_ext_tab       = 336003096;
{FB20_UP}

{FB21_UP}
  isc_dsql_feature_not_supported_ods   = 336003097;
  isc_primary_key_required             = 336003098;
  isc_upd_ins_doesnt_match_pk          = 336003099;
  isc_upd_ins_doesnt_match_matching    = 336003100;
  isc_upd_ins_with_complex_view        = 336003101;
  isc_dsql_incompatible_trigger_type   = 336003102;
  isc_dsql_db_trigger_type_cant_change = 336003103;
{FB21_UP}

{FB25_UP}
  isc_dyn_dup_table                    = 336068740;
  isc_dyn_column_does_not_exist        = 336068784;
{FB25_UP}

  isc_dyn_role_does_not_exist           = 336068796;
  isc_dyn_no_grant_admin_opt            = 336068797;
  isc_dyn_user_not_role_member          = 336068798;
  isc_dyn_delete_role_failed            = 336068799;
  isc_dyn_grant_role_to_user            = 336068800;
  isc_dyn_inv_sql_role_name             = 336068801;
  isc_dyn_dup_sql_role                  = 336068802;
  isc_dyn_kywd_spec_for_role            = 336068803;
  isc_dyn_roles_not_supported           = 336068804;
  isc_dyn_domain_name_exists            = 336068812;
  isc_dyn_field_name_exists             = 336068813;
  isc_dyn_dependency_exists             = 336068814;
  isc_dyn_dtype_invalid                 = 336068815;
  isc_dyn_char_fld_too_small            = 336068816;
  isc_dyn_invalid_dtype_conversion      = 336068817;
  isc_dyn_dtype_conv_invalid            = 336068818;

  isc_dyn_zero_len_id                   = 336068820;

{FB21_UP}
  isc_max_coll_per_charset             = 336068829;
  isc_invalid_coll_attr                = 336068830;
  isc_dyn_wrong_gtt_scope              = 336068840;
  isc_dyn_scale_too_big                = 336068852;
  isc_dyn_precision_too_small          = 336068853;
  isc_dyn_miss_priv_warning            = 336068855;
  isc_dyn_ods_not_supp_feature         = 336068856;
{FB21_UP}

{FB25_UP}
  isc_dyn_cannot_addrem_computed       = 336068857;
  isc_dyn_no_empty_pw                  = 336068858;
  isc_dyn_dup_index                    = 336068859;
{FB25_UP}

  isc_gbak_unknown_switch               = 336330753;
  isc_gbak_page_size_missing            = 336330754;
  isc_gbak_page_size_toobig             = 336330755;
  isc_gbak_redir_ouput_missing          = 336330756;
  isc_gbak_switches_conflict            = 336330757;
  isc_gbak_unknown_device               = 336330758;
  isc_gbak_no_protection                = 336330759;
  isc_gbak_page_size_not_allowed        = 336330760;
  isc_gbak_multi_source_dest            = 336330761;
  isc_gbak_filename_missing             = 336330762;
  isc_gbak_dup_inout_names              = 336330763;
  isc_gbak_inv_page_size                = 336330764;
  isc_gbak_db_specified                 = 336330765;
  isc_gbak_db_exists                    = 336330766;
  isc_gbak_unk_device                   = 336330767;
  isc_gbak_blob_info_failed             = 336330772;
  isc_gbak_unk_blob_item                = 336330773;
  isc_gbak_get_seg_failed               = 336330774;
  isc_gbak_close_blob_failed            = 336330775;
  isc_gbak_open_blob_failed             = 336330776;
  isc_gbak_put_blr_gen_id_failed        = 336330777;
  isc_gbak_unk_type                     = 336330778;
  isc_gbak_comp_req_failed              = 336330779;
  isc_gbak_start_req_failed             = 336330780;
  isc_gbak_rec_failed                   = 336330781;
  isc_gbak_rel_req_failed               = 336330782;
  isc_gbak_db_info_failed               = 336330783;
  isc_gbak_no_db_desc                   = 336330784;
  isc_gbak_db_create_failed             = 336330785;
  isc_gbak_decomp_len_error             = 336330786;
  isc_gbak_tbl_missing                  = 336330787;
  isc_gbak_blob_col_missing             = 336330788;
  isc_gbak_create_blob_failed           = 336330789;
  isc_gbak_put_seg_failed               = 336330790;
  isc_gbak_rec_len_exp                  = 336330791;
  isc_gbak_inv_rec_len                  = 336330792;
  isc_gbak_exp_data_type                = 336330793;
  isc_gbak_gen_id_failed                = 336330794;
  isc_gbak_unk_rec_type                 = 336330795;
  isc_gbak_inv_bkup_ver                 = 336330796;
  isc_gbak_missing_bkup_desc            = 336330797;
  isc_gbak_string_trunc                 = 336330798;
  isc_gbak_cant_rest_record             = 336330799;
  isc_gbak_send_failed                  = 336330800;
  isc_gbak_no_tbl_name                  = 336330801;
  isc_gbak_unexp_eof                    = 336330802;
  isc_gbak_db_format_too_old            = 336330803;
  isc_gbak_inv_array_dim                = 336330804;
  isc_gbak_xdr_len_expected             = 336330807;
  isc_gbak_open_bkup_error              = 336330817;
  isc_gbak_open_error                   = 336330818;
  isc_gbak_missing_block_fac            = 336330934;
  isc_gbak_inv_block_fac                = 336330935;
  isc_gbak_block_fac_specified          = 336330936;
  isc_gbak_missing_username             = 336330940;
  isc_gbak_missing_password             = 336330941;
  isc_gbak_missing_skipped_bytes        = 336330952;
  isc_gbak_inv_skipped_bytes            = 336330953;
  isc_gbak_err_restore_charset          = 336330965;
  isc_gbak_err_restore_collation        = 336330967;
  isc_gbak_read_error                   = 336330972;
  isc_gbak_write_error                  = 336330973;
  isc_gbak_db_in_use                    = 336330985;
  isc_gbak_sysmemex                     = 336330990;
  isc_gbak_restore_role_failed          = 336331002;
  isc_gbak_role_op_missing              = 336331005;
  isc_gbak_page_buffers_missing         = 336331010;
  isc_gbak_page_buffers_wrong_param     = 336331011;
  isc_gbak_page_buffers_restore         = 336331012;
  isc_gbak_inv_size                     = 336331014;
  isc_gbak_file_outof_sequence          = 336331015;
  isc_gbak_join_file_missing            = 336331016;
  isc_gbak_stdin_not_supptd             = 336331017;
  isc_gbak_stdout_not_supptd            = 336331018;
  isc_gbak_bkup_corrupt                 = 336331019;
  isc_gbak_unk_db_file_spec             = 336331020;
  isc_gbak_hdr_write_failed             = 336331021;
  isc_gbak_disk_space_ex                = 336331022;
  isc_gbak_size_lt_min                  = 336331023;
  isc_gbak_svc_name_missing             = 336331025;
  isc_gbak_not_ownr                     = 336331026;
  isc_gbak_mode_req                     = 336331031;

  isc_gbak_just_data                    = 336331033;
  isc_gbak_data_only                    = 336331034;

{FB25_UP}
  isc_gbak_invalid_metadata            = 336331093;
  isc_gbak_invalid_data                = 336331094;
{FB25_UP}

{FB20_UP}
  isc_dsql_too_old_ods                 = 336397205;
  isc_dsql_table_not_found             = 336397206;
  isc_dsql_view_not_found              = 336397207;
  isc_dsql_line_col_error              = 336397208;
  isc_dsql_unknown_pos                 = 336397209;
  isc_dsql_no_dup_name                 = 336397210;
  isc_dsql_too_many_values             = 336397211;
{FB20_UP}

{FB21_UP}
  isc_dsql_no_array_computed           = 336397212;
  isc_dsql_implicit_domain_name        = 336397213;
  isc_dsql_only_can_subscript_array    = 336397214;
  isc_dsql_max_sort_items              = 336397215;
  isc_dsql_max_group_items             = 336397216;
  isc_dsql_conflicting_sort_field      = 336397217;
  isc_dsql_derived_table_more_columns  = 336397218;
  isc_dsql_derived_table_less_columns  = 336397219;
  isc_dsql_derived_field_unnamed       = 336397220;
  isc_dsql_derived_field_dup_name      = 336397221;
  isc_dsql_derived_alias_select        = 336397222;
  isc_dsql_derived_alias_field         = 336397223;
  isc_dsql_auto_field_bad_pos          = 336397224;
  isc_dsql_cte_wrong_reference         = 336397225;
  isc_dsql_cte_cycle                   = 336397226;
  isc_dsql_cte_outer_join              = 336397227;
  isc_dsql_cte_mult_references         = 336397228;
  isc_dsql_cte_not_a_union             = 336397229;
  isc_dsql_cte_nonrecurs_after_recurs  = 336397230;
  isc_dsql_cte_wrong_clause            = 336397231;
  isc_dsql_cte_union_all               = 336397232;
  isc_dsql_cte_miss_nonrecursive       = 336397233;
  isc_dsql_cte_nested_with             = 336397234;
  isc_dsql_col_more_than_once_using    = 336397235;
  isc_dsql_unsupp_feature_dialect      = 336397236;
{FB21_UP}

{FB25_UP}
  isc_dsql_cte_not_used                = 336397237;
  isc_dsql_col_more_than_once_view     = 336397238;
  isc_dsql_unsupported_in_auto_trans   = 336397239;
  isc_dsql_eval_unknode                = 336397240;
  isc_dsql_agg_wrongarg                = 336397241;
  isc_dsql_agg2_wrongarg               = 336397242;
  isc_dsql_nodateortime_pm_string      = 336397243;
  isc_dsql_invalid_datetime_subtract   = 336397244;
  isc_dsql_invalid_dateortime_add      = 336397245;
  isc_dsql_invalid_type_minus_date     = 336397246;
  isc_dsql_nostring_addsub_dial3       = 336397247;
  isc_dsql_invalid_type_addsub_dial3   = 336397248;
  isc_dsql_invalid_type_multip_dial1   = 336397249;
  isc_dsql_nostring_multip_dial3       = 336397250;
  isc_dsql_invalid_type_multip_dial3   = 336397251;
  isc_dsql_mustuse_numeric_div_dial1   = 336397252;
  isc_dsql_nostring_div_dial3          = 336397253;
  isc_dsql_invalid_type_div_dial3      = 336397254;
  isc_dsql_nostring_neg_dial3          = 336397255;
  isc_dsql_invalid_type_neg            = 336397256;
  isc_dsql_max_distinct_items          = 336397257;
{FB25_UP}

  isc_gsec_cant_open_db                 = 336723983;
  isc_gsec_switches_error               = 336723984;
  isc_gsec_no_op_spec                   = 336723985;
  isc_gsec_no_usr_name                  = 336723986;
  isc_gsec_err_add                      = 336723987;
  isc_gsec_err_modify                   = 336723988;
  isc_gsec_err_find_mod                 = 336723989;
  isc_gsec_err_rec_not_found            = 336723990;
  isc_gsec_err_delete                   = 336723991;
  isc_gsec_err_find_del                 = 336723992;
  isc_gsec_err_find_disp                = 336723996;
  isc_gsec_inv_param                    = 336723997;
  isc_gsec_op_specified                 = 336723998;
  isc_gsec_pw_specified                 = 336723999;
  isc_gsec_uid_specified                = 336724000;
  isc_gsec_gid_specified                = 336724001;
  isc_gsec_proj_specified               = 336724002;
  isc_gsec_org_specified                = 336724003;
  isc_gsec_fname_specified              = 336724004;
  isc_gsec_mname_specified              = 336724005;
  isc_gsec_lname_specified              = 336724006;
  isc_gsec_inv_switch                   = 336724008;
  isc_gsec_amb_switch                   = 336724009;
  isc_gsec_no_op_specified              = 336724010;
  isc_gsec_params_not_allowed           = 336724011;
  isc_gsec_incompat_switch              = 336724012;
  isc_gsec_inv_username                 = 336724044;
  isc_gsec_inv_pw_length                = 336724045;
  isc_gsec_db_specified                 = 336724046;
  isc_gsec_db_admin_specified           = 336724047;
  isc_gsec_db_admin_pw_specified        = 336724048;
  isc_gsec_sql_role_specified           = 336724049;
  isc_license_no_file                   = 336789504;
  isc_license_op_specified              = 336789523;
  isc_license_op_missing                = 336789524;
  isc_license_inv_switch                = 336789525;
  isc_license_inv_switch_combo          = 336789526;
  isc_license_inv_op_combo              = 336789527;
  isc_license_amb_switch                = 336789528;
  isc_license_inv_parameter             = 336789529;
  isc_license_param_specified           = 336789530;
  isc_license_param_req                 = 336789531;
  isc_license_syntx_error               = 336789532;
  isc_license_dup_id                    = 336789534;
  isc_license_inv_id_key                = 336789535;
  isc_license_err_remove                = 336789536;
  isc_license_err_update                = 336789537;
  isc_license_err_convert               = 336789538;
  isc_license_err_unk                   = 336789539;
  isc_license_svc_err_add               = 336789540;
  isc_license_svc_err_remove            = 336789541;
  isc_license_eval_exists               = 336789563;

  isc_gstat_unknown_switch              = 336920577;
  isc_gstat_retry                       = 336920578;
  isc_gstat_wrong_ods                   = 336920579;
  isc_gstat_unexpected_eof              = 336920580;
  isc_gstat_open_err                    = 336920605;
  isc_gstat_read_err                    = 336920606;
  isc_gstat_sysmemex                    = 336920607;

{FB21_UP}
  isc_fbsvcmgr_bad_am                  = 336986113;
  isc_fbsvcmgr_bad_wm                  = 336986114;
  isc_fbsvcmgr_bad_rs                  = 336986115;
  isc_fbsvcmgr_info_err                = 336986116;
  isc_fbsvcmgr_query_err               = 336986117;
  isc_fbsvcmgr_switch_unknown          = 336986118;
{FB21_UP}

{FB25_UP}
  isc_fbsvcmgr_bad_sm                  = 336986159;
  isc_fbsvcmgr_fp_open                 = 336986160;
  isc_fbsvcmgr_fp_read                 = 336986161;
  isc_fbsvcmgr_fp_empty                = 336986162;
  isc_fbsvcmgr_bad_arg                 = 336986164;
  isc_utl_trusted_switch               = 337051649;
{FB25_UP}

{FB102}
  isc_err_max_FB102                           = 699;
{FB102}

{FB103}
  isc_err_max_FB103                           = 699;
{FB103}

{FB15}
  isc_err_max_FB15                           = 713;
{FB15}

{FB20}
  isc_err_max_FB20                           = 763;
{FB20}

{FB21}
  isc_err_max_FB21                           = 826;
{FB20}

{FB25}
  isc_err_max_FB25                           = 964;
{FB25}

implementation

end.

