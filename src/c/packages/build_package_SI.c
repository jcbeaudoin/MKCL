/* -*- mode: c  -*- */
/*
    Copyright (c) 2022, Jean-Claude Beaudoin.

    This program is under GNU LGPL, you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../../Copyright' for full details.
*/


#include "build_package.h"


#include "SI_package.h"


struct mkcl_function_declaration {
  struct mkcl_base_string symbol_name;
  struct mkcl_base_string function_object_denotator;
};

struct mkcl_function_declaration mkcl_si_declare_lisp_functions[] = {
  /* {SYMBOL_NAME(""), FUN_DENOT("")}, */
  {SYMBOL_NAME("UNBOUND"), FUN_DENOT("&mk_si_unbound_cfunobj")},
  {SYMBOL_NAME("*MAKE-CONSTANT"), FUN_DENOT("&mk_si_Xmake_constant_cfunobj")},
  {SYMBOL_NAME("*MAKE-SPECIAL"), FUN_DENOT("&mk_si_Xmake_special_cfunobj")},
  {SYMBOL_NAME("ALLOCATE-RAW-INSTANCE"), FUN_DENOT("&mk_si_allocate_raw_instance_cfunobj")},
  {SYMBOL_NAME("ASET"), FUN_DENOT("&mk_si_aset_cfunobj")},
  {SYMBOL_NAME("BASE-STRING-P"), FUN_DENOT("&mk_si_base_string_p_cfunobj")},
  {SYMBOL_NAME("BC-DISASSEMBLE"), FUN_DENOT("&mk_si_bc_disassemble_cfunobj")},
  {SYMBOL_NAME("BC-SPLIT"), FUN_DENOT("&mk_si_bc_split_cfunobj")},
  {SYMBOL_NAME("BDS-TOP"), FUN_DENOT("&mk_si_bds_top_cfunobj")},
  {SYMBOL_NAME("BDS-VAL"), FUN_DENOT("&mk_si_bds_val_cfunobj")},
  {SYMBOL_NAME("BDS-VAR"), FUN_DENOT("&mk_si_bds_var_cfunobj")},
  {SYMBOL_NAME("BIT-ARRAY-OP"), FUN_DENOT("&mk_si_bit_array_op_cfunobj")},
  {SYMBOL_NAME("CHAR-SET"), FUN_DENOT("&mk_si_char_set_cfunobj")},
  {SYMBOL_NAME("CLEAR-COMPILER-PROPERTIES"), FUN_DENOT("&mk_si_clear_compiler_properties_cfunobj")},
  {SYMBOL_NAME("COERCE-TO-BASE-STRING"), FUN_DENOT("&mk_si_coerce_to_base_string_cfunobj")},
  {SYMBOL_NAME("COERCE-TO-CHARACTER-STRING"), FUN_DENOT("&mk_si_coerce_to_character_string_cfunobj")},
  {SYMBOL_NAME("COERCE-TO-FILENAME"), FUN_DENOT("&mk_si_coerce_to_filename_cfunobj")},
  {SYMBOL_NAME("COERCE-TO-FUNCTION"), FUN_DENOT("&mk_si_coerce_to_function_cfunobj")},
  {SYMBOL_NAME("COERCE-TO-PACKAGE"), FUN_DENOT("&mk_si_coerce_to_package_cfunobj")},
  {SYMBOL_NAME("COPY-TO-SIMPLE-BASE-STRING"), FUN_DENOT("&mk_si_copy_to_simple_base_string_cfunobj")},
  {SYMBOL_NAME("COMPILED-FUNCTION-BLOCK"), FUN_DENOT("&mk_si_compiled_function_block_cfunobj")},
  {SYMBOL_NAME("COMPILED-FUNCTION-NAME"), FUN_DENOT("&mk_si_compiled_function_name_cfunobj")},
  {SYMBOL_NAME("COPY-STREAM"), FUN_DENOT("&mk_si_copy_stream_cfunobj")},
  {SYMBOL_NAME("DO-READ-SEQUENCE"), FUN_DENOT("&mk_si_do_read_sequence_cfunobj")},
  {SYMBOL_NAME("DO-WRITE-SEQUENCE"), FUN_DENOT("&mk_si_do_write_sequence_cfunobj")},
  {SYMBOL_NAME("ELT-SET"), FUN_DENOT("&mk_si_elt_set_cfunobj")},
  {SYMBOL_NAME("EVAL-IN-ENV"), FUN_DENOT("&mk_si_eval_in_env_cfunobj")},
  {SYMBOL_NAME("FILE-COLUMN"), FUN_DENOT("&mk_si_file_column_cfunobj")},
  {SYMBOL_NAME("FILE-KIND"), FUN_DENOT("&mk_si_file_kind_cfunobj")},
  {SYMBOL_NAME("FILL-POINTER-SET"), FUN_DENOT("&mk_si_fill_pointer_set_cfunobj")},
  {SYMBOL_NAME("FRS-BDS"), FUN_DENOT("&mk_si_frs_bds_cfunobj")},
  {SYMBOL_NAME("FRS-IHS"), FUN_DENOT("&mk_si_frs_ihs_cfunobj")},
  {SYMBOL_NAME("FRS-TAG"), FUN_DENOT("&mk_si_frs_tag_cfunobj")},
  {SYMBOL_NAME("FRS-TOP"), FUN_DENOT("&mk_si_frs_top_cfunobj")},
  {SYMBOL_NAME("FSET"), FUN_DENOT("&mk_si_fset_cfunobj")},
  {SYMBOL_NAME("FUNCTION-BLOCK-NAME"), FUN_DENOT("&mk_si_function_block_name_cfunobj")},
  {SYMBOL_NAME("GET-SYS-LIBRARY-PATHNAME"), FUN_DENOT("&mk_si_get_SYS_library_pathname_cfunobj")},
  {SYMBOL_NAME("GET-SYSPROP"), FUN_DENOT("&mk_si_get_sysprop_cfunobj")},
  {SYMBOL_NAME("HASH-SET"), FUN_DENOT("&mk_si_hash_set_cfunobj")},
  {SYMBOL_NAME("HASH-TABLE-ITERATOR"), FUN_DENOT("&mk_si_hash_table_iterator_cfunobj")},
  {SYMBOL_NAME("IHS-BDS-MARKER"), FUN_DENOT("&mk_si_ihs_bds_marker_cfunobj")},
  {SYMBOL_NAME("IHS-ENV"), FUN_DENOT("&mk_si_ihs_env_cfunobj")},
  {SYMBOL_NAME("IHS-FUN"), FUN_DENOT("&mk_si_ihs_fun_cfunobj")},
  {SYMBOL_NAME("IHS-NEXT"), FUN_DENOT("&mk_si_ihs_next_cfunobj")},
  {SYMBOL_NAME("IHS-PREV"), FUN_DENOT("&mk_si_ihs_prev_cfunobj")},
  {SYMBOL_NAME("IHS-TOP"), FUN_DENOT("&mk_si_ihs_top_cfunobj")},
  {SYMBOL_NAME("LOAD-SOURCE"), FUN_DENOT("&mk_si_load_source_cfunobj")},
  {SYMBOL_NAME("MAKE-LAMBDA"), FUN_DENOT("&mk_si_make_lambda_cfunobj")},
  {SYMBOL_NAME("MAKE-PURE-ARRAY"), FUN_DENOT("&mk_si_make_pure_array_cfunobj")},
  {SYMBOL_NAME("MAKE-STRING-OUTPUT-STREAM-FROM-STRING"), FUN_DENOT("&mk_si_make_string_output_stream_from_string_cfunobj")},
  {SYMBOL_NAME("MAKE-STRUCTURE"), FUN_DENOT("&mk_si_make_structure_cfunobj")},
  {SYMBOL_NAME("MAKE-VECTOR"), FUN_DENOT("&mk_si_make_vector_cfunobj")},
  {SYMBOL_NAME("MANGLE-NAME"), FUN_DENOT("&mk_si_mangle_name_cfunobj")},
  {SYMBOL_NAME("MEMBER1"), FUN_DENOT("&mk_si_member1_cfunobj")},
  {SYMBOL_NAME("MEMQ"), FUN_DENOT("&mk_si_memq_cfunobj")},
  {SYMBOL_NAME("PACKAGE-HASH-TABLES"), FUN_DENOT("&mk_si_package_hash_tables_cfunobj")},
  {SYMBOL_NAME("PATHNAME-TRANSLATIONS"), FUN_DENOT("&mk_si_pathname_translations_cfunobj")},
  {SYMBOL_NAME("ALL-LOGICAL-PATHNAME-TRANSLATIONS"), FUN_DENOT("&mk_si_all_logical_pathname_translations_cfunobj")},
  {SYMBOL_NAME("POINTER"), FUN_DENOT("&mk_si_pointer_cfunobj")},
  {SYMBOL_NAME("PROCESS-DECLARATIONS"), FUN_DENOT("&mk_si_process_declarations_cfunobj")},
  {SYMBOL_NAME("PROCESS-LAMBDA-LIST"), FUN_DENOT("&mk_si_process_lambda_list_cfunobj")},
  {SYMBOL_NAME("PUT-F"), FUN_DENOT("&mk_si_put_f_cfunobj")},
  {SYMBOL_NAME("PUT-PROPERTIES"), FUN_DENOT("&mk_si_put_properties_cfunobj")},
  {SYMBOL_NAME("PUT-SYSPROP"), FUN_DENOT("&mk_si_put_sysprop_cfunobj")},
  {SYMBOL_NAME("PUTPROP"), FUN_DENOT("&mk_si_putprop_cfunobj")},
  {SYMBOL_NAME("READTABLE-CASE-SET"), FUN_DENOT("&mk_si_readtable_case_set_cfunobj")},
  {SYMBOL_NAME("REM-F"), FUN_DENOT("&mk_si_rem_f_cfunobj")},
  {SYMBOL_NAME("REM-SYSPROP"), FUN_DENOT("&mk_si_rem_sysprop_cfunobj")},
  {SYMBOL_NAME("REPLACE-ARRAY"), FUN_DENOT("&mk_si_replace_array_cfunobj")},
  {SYMBOL_NAME("ROW-MAJOR-ASET"), FUN_DENOT("&mk_si_row_major_aset_cfunobj")},
  {SYMBOL_NAME("SAFE-EVAL"), FUN_DENOT("MKCL_IN_LISP(&mk_si_safe_eval_cfunobj)")},
  {SYMBOL_NAME("SCH-FRS-BASE"), FUN_DENOT("&mk_si_sch_frs_base_cfunobj")},
  {SYMBOL_NAME("SCHAR-SET"), FUN_DENOT("&mk_si_schar_set_cfunobj")},
  {SYMBOL_NAME("SELECT-PACKAGE"), FUN_DENOT("&mk_si_select_package_cfunobj")},
  {SYMBOL_NAME("SET-SYMBOL-PLIST"), FUN_DENOT("&mk_si_set_symbol_plist_cfunobj")},
  {SYMBOL_NAME("SPECIALP"), FUN_DENOT("&mk_si_specialp_cfunobj")},
  {SYMBOL_NAME("STANDARD-READTABLE"), FUN_DENOT("&mk_si_standard_readtable_cfunobj")},
  {SYMBOL_NAME("CONCATENATE-BASE-STRINGS"), FUN_DENOT("&mk_si_concatenate_base_strings_cfunobj")},
  {SYMBOL_NAME("FAST-READ-FROM-BASE-STRING"), FUN_DENOT("&mk_si_fast_read_from_base_string_cfunobj")},
  {SYMBOL_NAME("STRUCTURE-NAME"), FUN_DENOT("&mk_si_structure_name_cfunobj")},
  {SYMBOL_NAME("STRUCTURE-LENGTH"), FUN_DENOT("&mk_si_structure_length_cfunobj")},
  {SYMBOL_NAME("STRUCTURE-REF"), FUN_DENOT("&mk_si_structure_ref_cfunobj")},
  {SYMBOL_NAME("STRUCTURE-SET"), FUN_DENOT("&mk_si_structure_set_cfunobj")},
  {SYMBOL_NAME("STRUCTURE-SUBTYPE-P"), FUN_DENOT("&mk_si_structure_subtype_p_cfunobj")},
  {SYMBOL_NAME("STRUCTUREP"), FUN_DENOT("&mk_si_structurep_cfunobj")},
  {SYMBOL_NAME("SVSET"), FUN_DENOT("&mk_si_svset_cfunobj")},
  {SYMBOL_NAME("VALID-FUNCTION-NAME-P"), FUN_DENOT("&mk_si_valid_function_name_p_cfunobj")},
  {SYMBOL_NAME("WRITE-OBJECT"), FUN_DENOT("&mk_si_write_object_cfunobj")},
  {SYMBOL_NAME("WRITE-UGLY-OBJECT"), FUN_DENOT("&mk_si_write_ugly_object_cfunobj")},
  {SYMBOL_NAME("COPY-INSTANCE"), FUN_DENOT("&mk_si_copy_instance_cfunobj")},
  {SYMBOL_NAME("GENERIC-FUNCTION-P"), FUN_DENOT("&mk_si_generic_function_p_cfunobj")},
  {SYMBOL_NAME("INSTANCE-REF-SAFE"), FUN_DENOT("&mk_si_instance_ref_safe_cfunobj")},
  {SYMBOL_NAME("INSTANCE-REF"), FUN_DENOT("&mk_si_instance_ref_cfunobj")},
  {SYMBOL_NAME("INSTANCE-SET"), FUN_DENOT("&mk_si_instance_set_cfunobj")},
  {SYMBOL_NAME("INSTANCE-SIG"), FUN_DENOT("&mk_si_instance_sig_cfunobj")},
  {SYMBOL_NAME("INSTANCE-SIG-SET"), FUN_DENOT("&mk_si_instance_sig_set_cfunobj")},
  {SYMBOL_NAME("INSTANCE-SIG-SET2"), FUN_DENOT("&mk_si_instance_sig_set2_cfunobj")},
  {SYMBOL_NAME("INSTANCE-CLASS"), FUN_DENOT("&mk_si_instance_class_cfunobj")},
  {SYMBOL_NAME("INSTANCE-CLASS-SET"), FUN_DENOT("&mk_si_instance_class_set_cfunobj")},
  {SYMBOL_NAME("INSTANCEP"), FUN_DENOT("&mk_si_instancep_cfunobj")},
  {SYMBOL_NAME("INSTANCE-LENGTH"), FUN_DENOT("&mk_si_instance_length_cfunobj")},
  {SYMBOL_NAME("SL-BOUNDP"), FUN_DENOT("&mk_si_sl_boundp_cfunobj")},
  {SYMBOL_NAME("SL-MAKUNBOUND"), FUN_DENOT("&mk_si_sl_makunbound_cfunobj")},
  {SYMBOL_NAME("SUBCLASSP"), FUN_DENOT("MKCL_IN_LISP(&mk_si_subclassp_cfunobj)")},
  {SYMBOL_NAME("OF-CLASS-P"), FUN_DENOT("MKCL_IN_LISP(&mk_si_of_class_p_cfunobj)")},
  {SYMBOL_NAME("ALLOCATE-FOREIGN-DATA"), FUN_DENOT("&mk_si_allocate_foreign_data_cfunobj")},
  {SYMBOL_NAME("FIND-FOREIGN-SYMBOL"), FUN_DENOT("&mk_si_find_foreign_symbol_cfunobj")},
  {SYMBOL_NAME("FOREIGN-ADDRESS"), FUN_DENOT("&mk_si_foreign_address_cfunobj")},
  {SYMBOL_NAME("FOREIGN-INDEXED"), FUN_DENOT("&mk_si_foreign_indexed_cfunobj")},
  {SYMBOL_NAME("FOREIGN-RECAST"), FUN_DENOT("&mk_si_foreign_recast_cfunobj")},
  {SYMBOL_NAME("FOREIGN-REF"), FUN_DENOT("&mk_si_foreign_ref_cfunobj")},
  {SYMBOL_NAME("FOREIGN-REF-ELT"), FUN_DENOT("&mk_si_foreign_ref_elt_cfunobj")},
  {SYMBOL_NAME("FOREIGN-SET"), FUN_DENOT("&mk_si_foreign_set_cfunobj")},
  {SYMBOL_NAME("FOREIGN-SET-ELT"), FUN_DENOT("&mk_si_foreign_set_elt_cfunobj")},
  {SYMBOL_NAME("FOREIGN-TAG"), FUN_DENOT("&mk_si_foreign_tag_cfunobj")},
  {SYMBOL_NAME("FREE-FOREIGN-DATA"), FUN_DENOT("&mk_si_free_foreign_data_cfunobj")},
  {SYMBOL_NAME("MAKE-FOREIGN-DATA-FROM-ARRAY"), FUN_DENOT("&mk_si_make_foreign_data_from_array_cfunobj")},
  {SYMBOL_NAME("LOAD-FOREIGN-MODULE"), FUN_DENOT("&mk_si_load_foreign_module_cfunobj")},
  {SYMBOL_NAME("UNLOAD-FOREIGN-MODULE"), FUN_DENOT("&mk_si_unload_foreign_module_cfunobj")},
  {SYMBOL_NAME("NULL-POINTER-P"), FUN_DENOT("&mk_si_null_pointer_p_cfunobj")},
  {SYMBOL_NAME("SIZE-OF-FOREIGN-ELT-TYPE"), FUN_DENOT("&mk_si_size_of_foreign_elt_type_cfunobj")},
  {SYMBOL_NAME("GC"), FUN_DENOT("&mk_si_gc_cfunobj")},
  {SYMBOL_NAME("GC-DUMP"), FUN_DENOT("&mk_si_gc_dump_cfunobj")},
  {SYMBOL_NAME("DO-DEFTYPE"), FUN_DENOT("MKCL_IN_LISP(&mk_si_do_deftype_cfunobj)")},
  {SYMBOL_NAME("LOAD-BINARY"), FUN_DENOT("&mk_si_load_binary_cfunobj")},
  {SYMBOL_NAME("CALL-CFUN"), FUN_DENOT("&mk_si_call_cfun_cfunobj")},
  {SYMBOL_NAME("MAKE-DYNAMIC-CALLBACK"), FUN_DENOT("&mk_si_make_dynamic_callback_cfunobj")},
  {SYMBOL_NAME("DO-SIGSEGV"), FUN_DENOT("&mk_si_do_sigsegv_cfunobj")},
  {SYMBOL_NAME("OBJNULL"), FUN_DENOT("&mk_si_objnull_cfunobj")},
  {SYMBOL_NAME("DISPLAY-SIGNAL-DISPOSITIONS"), FUN_DENOT("&mk_si_display_signal_dispositions_cfunobj")},
  {SYMBOL_NAME("SET-BUFFERING-MODE"), FUN_DENOT("&mk_si_set_buffering_mode_cfunobj")},
  {SYMBOL_NAME("GET-FINALIZER"), FUN_DENOT("&mk_si_get_finalizer_cfunobj")},
  {SYMBOL_NAME("SET-FINALIZER"), FUN_DENOT("&mk_si_set_finalizer_cfunobj")},
  {SYMBOL_NAME("FIND-RELATIVE-PACKAGE"), FUN_DENOT("MKCL_IN_LISP(&mk_si_find_relative_package_cfunobj)")},
  {SYMBOL_NAME("PACKAGE-PARENT"), FUN_DENOT("MKCL_IN_LISP(&mk_si_package_parent_cfunobj)")},
  {SYMBOL_NAME("PACKAGE-CHILDREN"), FUN_DENOT("MKCL_IN_LISP(&mk_si_package_children_cfunobj)")}, /* proclaimed in sysfun.lsp */
  {SYMBOL_NAME("GC-STATS"), FUN_DENOT("&mk_si_gc_stats_cfunobj")},
  {SYMBOL_NAME("CLEAR-GFUN-CACHE"), FUN_DENOT("&mk_si_clear_gfun_cache_cfunobj")},
  {SYMBOL_NAME("APPLY-FROM-TEMP-STACK-FRAME"), FUN_DENOT("&mk_si_apply_from_temp_stack_frame_cfunobj")},
  {SYMBOL_NAME("LOG1P"), FUN_DENOT("&mk_si_log1p_cfunobj")},
  {SYMBOL_NAME("COMPILED-FUNCTION-FILE"), FUN_DENOT("&mk_si_compiled_function_file_cfunobj")},
  {SYMBOL_NAME("HASH-EQL"), FUN_DENOT("&mk_si_hash_eql_cfunobj")},
  {SYMBOL_NAME("HASH-EQUAL"), FUN_DENOT("&mk_si_hash_equal_cfunobj")},
  {SYMBOL_NAME("HASH-EQUALP"), FUN_DENOT("&mk_si_hash_equalp_cfunobj")},
  {SYMBOL_NAME("FILL-ARRAY-WITH-ELT"), FUN_DENOT("&mk_si_fill_array_with_elt_cfunobj")},
  {SYMBOL_NAME("FLOAT-NAN-P"), FUN_DENOT("&mk_si_float_nan_p_cfunobj")},
  {SYMBOL_NAME("FLOAT-INFINITY-P"), FUN_DENOT("&mk_si_float_infinity_p_cfunobj")},
  {SYMBOL_NAME("READ-OBJECT-OR-IGNORE"), FUN_DENOT("&mk_si_read_object_or_ignore_cfunobj")},
  {SYMBOL_NAME("UNBOUND-VALUE-P"), FUN_DENOT("&mk_si_unbound_value_p_cfunobj")},
  {SYMBOL_NAME("PACKAGES-IN-WAITING"), FUN_DENOT("&mk_si_packages_in_waiting_cfunobj")},
  {SYMBOL_NAME("HASH-TABLES-STATISTICS"), FUN_DENOT("&mk_si_hash_tables_statistics_cfunobj")},
  {SYMBOL_NAME("MEM-STATS"), FUN_DENOT("&mk_si_mem_stats_cfunobj")},
  {SYMBOL_NAME("CLOSUREP"), FUN_DENOT("&mk_si_closurep_cfunobj")},
  {SYMBOL_NAME("CLOSURE-ENV"), FUN_DENOT("&mk_si_closure_env_cfunobj")},
  {SYMBOL_NAME("CLOSURE-PRODUCER"), FUN_DENOT("&mk_si_closure_producer_cfunobj")},
  {SYMBOL_NAME("SET-COMPILED-FUNCTION-NAME"), FUN_DENOT("&mk_si_set_compiled_function_name_cfunobj")},
  {SYMBOL_NAME("COMPILED-FUNCTION-OWNER"), FUN_DENOT("&mk_si_compiled_function_owner_cfunobj")},
  {SYMBOL_NAME("SET-COMPILED-FUNCTION-OWNER"), FUN_DENOT("&mk_si_set_compiled_function_owner_cfunobj")},
  {SYMBOL_NAME("SELF-TRUENAME"), FUN_DENOT("&mk_si_self_truename_cfunobj")},
  {SYMBOL_NAME("GC-OFF"), FUN_DENOT("&mk_si_gc_off_cfunobj")},
  {SYMBOL_NAME("GC-ON"), FUN_DENOT("&mk_si_gc_on_cfunobj")},
  {SYMBOL_NAME("TOP-APPLY"), FUN_DENOT("MKCL_IN_LISP(&mk_si_top_apply_cfunobj)")},
  {SYMBOL_NAME("CONVERT-CMP-LEXICAL-INFO"), FUN_DENOT("&mk_si_convert_cmp_lexical_info_cfunobj")},
  {SYMBOL_NAME("INSTALL-SIGSEGV-MONITOR"), FUN_DENOT("&mk_si_install_sigsegv_monitor_cfunobj")},
  {SYMBOL_NAME("LIST-LIBRARIES"), FUN_DENOT("&mk_si_list_libraries_cfunobj")},
  {SYMBOL_NAME("CLOSURE-DEPTH"), FUN_DENOT("&mk_si_closure_depth_cfunobj")},
  {SYMBOL_NAME("CLOSURE-LEVEL"), FUN_DENOT("&mk_si_closure_level_cfunobj")},
  {SYMBOL_NAME("CLOSURE-LEVEL-SIZE"), FUN_DENOT("&mk_si_closure_level_size_cfunobj")},
  {SYMBOL_NAME("CLOSURE-LEVEL-VAR"), FUN_DENOT("&mk_si_closure_level_var_cfunobj")},
  {SYMBOL_NAME("CLOSURE-LEVEL-SET-VAR"), FUN_DENOT("&mk_si_closure_level_set_var_cfunobj")},
  {SYMBOL_NAME("CLOSURE-LEVEL-OUTER-LEVEL"), FUN_DENOT("&mk_si_closure_level_outer_level_cfunobj")},
  {SYMBOL_NAME("SYSTEM-PROPERTIES"), FUN_DENOT("&mk_si_system_properties_cfunobj")},
  {SYMBOL_NAME("BYTECODEP"), FUN_DENOT("&mk_si_bytecodep_cfunobj")},
  {SYMBOL_NAME("SET-CLASS-PROPER-NAME"), FUN_DENOT("&mk_si_set_class_proper_name_cfunobj")},
  {SYMBOL_NAME("CLONE-CLOSURE"), FUN_DENOT("&mk_si_clone_closure_cfunobj")},
  {SYMBOL_NAME("UPDATE-FUNCTION-REFERENCES"), FUN_DENOT("&mk_si_update_function_references_cfunobj")},
  {SYMBOL_NAME("GET-FUN-REF-SYM"), FUN_DENOT("&mk_si_get_fun_ref_sym_cfunobj")},
  {SYMBOL_NAME("DISABLE-INTERRUPTS"), FUN_DENOT("&mk_si_disable_interrupts_cfunobj")},
  {SYMBOL_NAME("ENABLE-INTERRUPTS"), FUN_DENOT("&mk_si_enable_interrupts_cfunobj")},
  {SYMBOL_NAME("GDB"), FUN_DENOT("&mk_si_gdb_cfunobj")},
  {SYMBOL_NAME("TRIM-DYNAMIC-CONS-STACK"), FUN_DENOT("&mk_si_trim_dynamic_cons_stack_cfunobj")},
  {SYMBOL_NAME("DYN-CONS"), FUN_DENOT("&mk_si_dyn_cons_cfunobj")},
  {SYMBOL_NAME("DISABLE-FPE"), FUN_DENOT("&mk_si_disable_fpe_cfunobj")},
  {SYMBOL_NAME("ENABLE-FPE"), FUN_DENOT("&mk_si_enable_fpe_cfunobj")},
  {SYMBOL_NAME("ALL-ENABLED-FPE"), FUN_DENOT("&mk_si_all_enabled_fpe_cfunobj")},
  {SYMBOL_NAME("FPE-ENABLED-P"), FUN_DENOT("&mk_si_fpe_enabled_p_cfunobj")},
  {SYMBOL_NAME("ALL-RAISED-FPE"), FUN_DENOT("&mk_si_all_raised_fpe_cfunobj")},
  {SYMBOL_NAME("FPE-RAISED-P"), FUN_DENOT("&mk_si_fpe_raised_p_cfunobj")},
  {SYMBOL_NAME("RAISE-FPE"), FUN_DENOT("&mk_si_raise_fpe_cfunobj")},
  {SYMBOL_NAME("CLEAR-FPE"), FUN_DENOT("&mk_si_clear_fpe_cfunobj")},
  {SYMBOL_NAME("CLEAR-ALL-FPE"), FUN_DENOT("&mk_si_clear_all_fpe_cfunobj")},
  {SYMBOL_NAME("INITIAL-FLOATING-POINT-EXCEPTION-SET"), FUN_DENOT("&mk_si_initial_floating_point_exception_set_cfunobj")},
  {SYMBOL_NAME("CONCATENATE-STRINGS"), FUN_DENOT("&mk_si_concatenate_strings_cfunobj")},
  {SYMBOL_NAME("SIMPLE-BASE-STRING-P"), FUN_DENOT("&mk_si_simple_base_string_p_cfunobj")},
  {SYMBOL_NAME("STREAM-EXTERNAL-FORMAT-SET"), FUN_DENOT("&mk_si_stream_external_format_set_cfunobj")},
  {SYMBOL_NAME("GET-BUFFERING-MODE"), FUN_DENOT("&mk_si_get_buffering_mode_cfunobj")},
  {SYMBOL_NAME("UTF-8"), FUN_DENOT("&mk_si_utf_8_cfunobj")},
  {SYMBOL_NAME("UTF-8-P"), FUN_DENOT("&mk_si_utf_8_p_cfunobj")},
  {SYMBOL_NAME("UTF-8-LENGTH"), FUN_DENOT("&mk_si_utf_8_length_cfunobj")},
  {SYMBOL_NAME("UTF-8-AS-IS"), FUN_DENOT("&mk_si_utf_8_as_is_cfunobj")},
  {SYMBOL_NAME("UTF-8-CHAR"), FUN_DENOT("&mk_si_utf_8_char_cfunobj")},
  {SYMBOL_NAME("UTF-8+"), FUN_DENOT("&mk_si_utf_8Plus_cfunobj")},
  {SYMBOL_NAME("UTF-8="), FUN_DENOT("&mk_si_utf_8E_cfunobj")},
  {SYMBOL_NAME("UTF-8-PUSH-EXTEND"), FUN_DENOT("&mk_si_utf_8_push_extend_cfunobj")},
  {SYMBOL_NAME("UTF-8-LAST"), FUN_DENOT("&mk_si_utf_8_last_cfunobj")},
  {SYMBOL_NAME("UTF-16"), FUN_DENOT("&mk_si_utf_16_cfunobj")},
  {SYMBOL_NAME("UTF-16-P"), FUN_DENOT("&mk_si_utf_16_p_cfunobj")},
  {SYMBOL_NAME("UTF-16-LENGTH"), FUN_DENOT("&mk_si_utf_16_length_cfunobj")},
  {SYMBOL_NAME("UTF-16-AS-IS"), FUN_DENOT("&mk_si_utf_16_as_is_cfunobj")},
  {SYMBOL_NAME("UTF-16-CHAR"), FUN_DENOT("&mk_si_utf_16_char_cfunobj")},
  {SYMBOL_NAME("UTF-16+"), FUN_DENOT("&mk_si_utf_16Plus_cfunobj")},
  {SYMBOL_NAME("UTF-16="), FUN_DENOT("&mk_si_utf_16E_cfunobj")},
  {SYMBOL_NAME("UTF-16-PUSH-EXTEND"), FUN_DENOT("&mk_si_utf_16_push_extend_cfunobj")},
  {SYMBOL_NAME("UTF-16-LAST"), FUN_DENOT("&mk_si_utf_16_last_cfunobj")},
  {SYMBOL_NAME("SAMPLE-ALLOCATION-STATISTICS"), FUN_DENOT("&mk_si_sample_allocation_statistics_cfunobj")},
  {SYMBOL_NAME("RESET-ALLOCATION-STATISTICS"), FUN_DENOT("&mk_si_reset_allocation_statistics_cfunobj")},
  {SYMBOL_NAME("ROOM-REPORT"), FUN_DENOT("&mk_si_room_report_cfunobj")},
  {SYMBOL_NAME("MANGLE-FUNCTION-NAME"), FUN_DENOT("&mk_si_mangle_function_name_cfunobj")},
  {SYMBOL_NAME("MANGLE-STRING"), FUN_DENOT("&mk_si_mangle_string_cfunobj")},
  {SYMBOL_NAME("MANGLE-SYMBOL"), FUN_DENOT("&mk_si_mangle_symbol_cfunobj")},
  {SYMBOL_NAME("CLOSE-PACKAGE"), FUN_DENOT("&mk_si_close_package_cfunobj")},
  {SYMBOL_NAME("REOPEN-PACKAGE"), FUN_DENOT("&mk_si_reopen_package_cfunobj")},
  {SYMBOL_NAME("PACKAGE-CLOSED-P"), FUN_DENOT("&mk_si_package_closed_p_cfunobj")},
  {SYMBOL_NAME("SHUTDOWN-IN-PROGRESS-P"), FUN_DENOT("&mk_si_shutdown_in_progress_p_cfunobj")},
  {SYMBOL_NAME("REGISTER-SHUTDOWN-THREAD"), FUN_DENOT("&mk_si_register_shutdown_thread_cfunobj")},
  {SYMBOL_NAME("REGISTER-SHUTDOWN-WATCHDOG-THREAD"), FUN_DENOT("& mk_si_register_shutdown_watchdog_thread_cfunobj")},
  {SYMBOL_NAME("SHUTDOWN-WATCHDOG-THREAD"), FUN_DENOT("&mk_si_shutdown_watchdog_thread_cfunobj")},
  {SYMBOL_NAME("SHUTDOWN-MKCL"), FUN_DENOT("&mk_si_shutdown_mkcl_cfunobj")},
  {SYMBOL_NAME("SETUP-FOR-GDB"), FUN_DENOT("&mk_si_setup_for_gdb_cfunobj")},
  {SYMBOL_NAME("INTERRUPT-STATUS"), FUN_DENOT("&mk_si_interrupt_status_cfunobj")},
  {SYMBOL_NAME("COPY-TO-SIMPLE-STRING"), FUN_DENOT("&mk_si_copy_to_simple_string_cfunobj")},
  {SYMBOL_NAME("SCRUB-VALUES"), FUN_DENOT("&mk_si_scrub_values_cfunobj")},
  {SYMBOL_NAME("MAKE-FOREIGN-NULL-POINTER"), FUN_DENOT("&mk_si_make_foreign_null_pointer_cfunobj")},
  {SYMBOL_NAME("FOREIGNP"), FUN_DENOT("&mk_si_foreignp_cfunobj")},
  {SYMBOL_NAME("LIBC-ERROR-STRING"), FUN_DENOT("&mk_si_libc_error_string_cfunobj")},
  {SYMBOL_NAME("ERRNO-STRING"), FUN_DENOT("&mk_si_errno_string_cfunobj")},
  {SYMBOL_NAME("GET-LOCAL-TIME-ZONE"), FUN_DENOT("&mk_si_get_local_time_zone_cfunobj")},
  {SYMBOL_NAME("UNAME"), FUN_DENOT("&mk_si_uname_cfunobj")},
  {SYMBOL_NAME("NON-INTERACTIVE-THREAD-DEBUGGER-TRAP"), FUN_DENOT("&mk_si_non_interactive_thread_debugger_trap_cfunobj")},
  {SYMBOL_NAME("MKCL-VERSION"), FUN_DENOT("MKCL_IN_LISP(&mk_si_mkcl_version_cfunobj)")},
  {SYMBOL_NAME("MKCL-MAJOR-VERSION"), FUN_DENOT("MKCL_IN_LISP(&mk_si_mkcl_major_version_cfunobj)")},
  {SYMBOL_NAME("MKCL-MINOR-VERSION"), FUN_DENOT("MKCL_IN_LISP(&mk_si_mkcl_minor_version_cfunobj)")},
  {SYMBOL_NAME("MKCL-PATCH-LEVEL"), FUN_DENOT("MKCL_IN_LISP(&mk_si_mkcl_patch_level_cfunobj)")},
  {SYMBOL_NAME("SET-BINDING-STACK-LIMIT"), FUN_DENOT("&mk_si_set_binding_stack_limit_cfunobj")},
  {SYMBOL_NAME("GET-BINDING-STACK-LIMIT"), FUN_DENOT("&mk_si_get_binding_stack_limit_cfunobj")},
  {SYMBOL_NAME("SET-FRAME-STACK-LIMIT"), FUN_DENOT("&mk_si_set_frame_stack_limit_cfunobj")},
  {SYMBOL_NAME("GET-FRAME-STACK-LIMIT"), FUN_DENOT("&mk_si_get_frame_stack_limit_cfunobj")},
  {SYMBOL_NAME("SET-LISP-TEMP-STACK-LIMIT"), FUN_DENOT("&mk_si_set_lisp_temp_stack_limit_cfunobj")},
  {SYMBOL_NAME("GET-LISP-TEMP-STACK-LIMIT"), FUN_DENOT("&mk_si_get_lisp_temp_stack_limit_cfunobj")},
  {SYMBOL_NAME("SET-HEAP-SIZE-LIMIT"), FUN_DENOT("&mk_si_set_heap_size_limit_cfunobj")},
  {SYMBOL_NAME("GET-HEAP-SIZE-LIMIT"), FUN_DENOT("&mk_si_get_heap_size_limit_cfunobj")},
  {SYMBOL_NAME("GET-CALL-STACK-LIMIT"), FUN_DENOT("&mk_si_get_call_stack_limit_cfunobj")},
  {SYMBOL_NAME("TRIM-FFI-ARGUMENTS-STAGING-AREA"), FUN_DENOT("&mk_si_trim_ffi_arguments_staging_area_cfunobj")},
  {SYMBOL_NAME("RELEASE-FFI-AREA"), FUN_DENOT("&mk_si_release_ffi_area_cfunobj")},
  {SYMBOL_NAME("LIST-ALL-CHILDREN"), FUN_DENOT("&mk_si_list_all_children_cfunobj")},
  {SYMBOL_NAME("TRACE-SPECIALS"), FUN_DENOT("&mk_si_trace_specials_cfunobj")},
  {SYMBOL_NAME("UNTRACE-SPECIALS"), FUN_DENOT("&mk_si_untrace_specials_cfunobj")},
  {SYMBOL_NAME("SIGNUM-TO-SIGNAL-NAME"), FUN_DENOT("&mk_si_signum_to_signal_name_cfunobj")},
  {SYMBOL_NAME("OBJNULL-VALUE-P"), FUN_DENOT("&mk_si_objnull_value_p_cfunobj")},
  {SYMBOL_NAME("SHUTDOWN-MKCL-THREADS"), FUN_DENOT("MKCL_IN_LISP(&mk_si_shutdown_mkcl_threads_cfunobj)")},
  {SYMBOL_NAME("ANSI-CLOSE"), FUN_DENOT("&mk_si_ansi_close_cfunobj")},
  {SYMBOL_NAME("ANSI-STREAMP"), FUN_DENOT("&mk_si_ansi_streamp_cfunobj")},
  {SYMBOL_NAME("ANSI-INPUT-STREAM-P"), FUN_DENOT("&mk_si_ansi_input_stream_p_cfunobj")},
  {SYMBOL_NAME("ANSI-OUTPUT-STREAM-P"), FUN_DENOT("&mk_si_ansi_output_stream_p_cfunobj")},
  {SYMBOL_NAME("ANSI-OPEN-STREAM-P"), FUN_DENOT("&mk_si_ansi_open_stream_p_cfunobj")},
  {SYMBOL_NAME("ANSI-STREAM-ELEMENT-TYPE"), FUN_DENOT("&mk_si_ansi_stream_element_type_cfunobj")},
};

struct mkcl_function_declaration mkcl_si_declare_macros[] = {
};

struct mkcl_function_declaration mkcl_si_declare_special_operators[] = {
};

struct mkcl_variable_declaration {
  struct mkcl_base_string symbol_name;
  struct mkcl_base_string value_denotator;
};

struct mkcl_variable_declaration mkcl_si_declare_specials[] = {
  /* {SYMBOL_NAME(""), VAL_DENOT("mk_cl_Cnil")}, */
  {SYMBOL_NAME("*BACKQ-LEVEL*"), VAL_DENOT("MKCL_MAKE_FIXNUM(0)")},
  {SYMBOL_NAME("*CBLOCK*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*CIRCLE-COUNTER*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*CIRCLE-STACK*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*CLASS-NAME-HASH-TABLE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*COMPILER-CONSTANTS*"), VAL_DENOT("MKCL_OBJNULL")}, /* Added back for use by #'compile. JCB */
  {SYMBOL_NAME("*GC-MESSAGE*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*GC-VERBOSE*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*IGNORE-EOF-ON-TERMINAL-IO*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*INDENT-FORMATTED-OUTPUT*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*INIT-FUNCTION-PREFIX*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*KEEP-DEFINITIONS*"), VAL_DENOT("mk_cl_Ct")},
  {SYMBOL_NAME("*LOAD-HOOKS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOAD-SEARCH-LIST*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*PRINT-PACKAGE*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*PRINT-STRUCTURE*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*SHARP-EQ-CONTEXT*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*STEP-LEVEL*"), VAL_DENOT("MKCL_MAKE_FIXNUM(0)")},
  {SYMBOL_NAME("*STEP-ACTION*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("+READING-FASL-FILE+"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("WRITE-OBJECT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("WRITE-UGLY-OBJECT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*SOURCE-LOCATION*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*REGISTER-WITH-PDE-HOOK*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*EXIT-HOOKS*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*ALIEN-DECLARATIONS*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*CODE-WALKER*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*ACTION-ON-UNDEFINED-VARIABLE*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*RELATIVE-PACKAGE-NAMES*"), VAL_DENOT("RELATIVE_PACKAGES_P")},
  {SYMBOL_NAME("*CURRENT-FORM*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*BYTECODE-COMPILER*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*DYNAMIC-CONS-STACK*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*DEFAULT-FLOATING-POINT-EXCEPTION-SET*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*READ-FLOAT-EXACTLY*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*PRINT-FLOAT-EXACTLY*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*DEFAULT-EXTERNAL-FORMAT*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*OS-STRING-FORMAT*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("+SHUTDOWN-GATE+"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DRIBBLE-CLOSER*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*RESTART-CLUSTERS*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*CONDITION-RESTARTS*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*HANDLER-CLUSTERS*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*EXTENDED-CHARACTER-NAMES*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*SHARP-LABELS*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*PENDING-SHARP-LABELS*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*WARN-ON-FORWARD-REFERENCE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DL*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*KEY-CHECK*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*ARG-CHECK*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DOCUMENTATION-POOL*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*KEEP-DOCUMENTATION*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*SAFETY*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*SPEED*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*SPACE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DEBUG*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*COMPILATION-SPEED*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*COMPILATION-UNIT-ENVIRONMENT*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*COMPILER-FLOATING-POINT-EXCLUSION-SET*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*SUBTYPEP-CACHE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*UPGRADED-ARRAY-ELEMENT-TYPE-CACHE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*SAVE-TYPES-DATABASE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*HIGHEST-TYPE-TAG*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*MEMBER-TYPES*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*INTERVALS-MASK*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*ELEMENTARY-TYPES*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DEFSTRUCT-AS-DEFCLASS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*APROPOS-PRINT-LEVEL*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*APROPOS-PRINT-LENGTH*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*TRACE-LEVEL*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*TRACE-LIST*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*TRACE-MAX-INDENT*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*INSIDE-TRACE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*STEP-FORM*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*STEP-TAG*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*STEP-FUNCTIONS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*STRICT-ANSI-LOOP-MACRO*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-PROLOGUE-BEFORE-ALL*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-GENTEMP*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-REAL-DATA-TYPE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-MINIMAX-TYPE-INFINITIES-ALIST*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-UNIVERSE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-DESTRUCTURING-HOOKS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-DESETQ-TEMPORARY*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-SOURCE-CODE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-ORIGINAL-SOURCE-CODE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-SOURCE-CONTEXT*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-NAMES*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-MACRO-ENVIRONMENT*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-NAMED-VARIABLES*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-VARIABLES*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-DECLARATIONS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-DESETQ-CROCKS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-WRAPPERS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-BIND-STACK*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-NODECLARE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-ITERATION-VARIABLES*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-PROLOGUE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-BEFORE-LOOP*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-BODY*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-AFTER-BODY*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-EMITTED-BODY*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-EPILOGUE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-AFTER-EPILOGUE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-FINAL-VALUE-CULPRIT*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-INSIDE-CONDITIONAL*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-WHEN-IT-VARIABLE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-NEVER-STEPPED-VARIABLE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-COLLECTION-CRUFT*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-DUPLICATE-CODE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-ITERATION-FLAG-VARIABLE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*SPECIAL-CODE-SIZES*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*ESTIMATE-CODE-SIZE-PUNT*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*IGNORES*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-FOR-IN-BUG-DO-STEP-BEFORE-BODY*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOOP-ANSI-UNIVERSE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DIGITS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*FORMAT-DIRECTIVE-EXPANDERS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*FORMAT-DIRECTIVE-INTERPRETERS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DEFAULT-FORMAT-ERROR-CONTROL-STRING*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DEFAULT-FORMAT-ERROR-OFFSET*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*OUTPUT-LAYOUT-MODE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*UP-UP-AND-OUT-ALLOWED*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOGICAL-BLOCK-POPPER*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*EXPANDER-NEXT-ARG-MACRO*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*ONLY-SIMPLE-ARGS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*ORIG-ARGS-AVAILABLE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*SIMPLE-ARGS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*OUTSIDE-ARGS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*ILLEGAL-INSIDE-JUSTIFICATION*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LISP-INIT-FILE-LIST*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*HELP-MESSAGE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*BREAK-ENABLE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DISPLAY-BANNER*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*INITIAL-PPRINT-DISPATCH*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DEFAULT-UNIVERSAL-ERROR-HANDLER-MAXIMUM-DEPTH*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*UNIVERSAL-ERROR-HANDLER-STACK*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*UNIVERSAL-ERROR-HANDLER-LEVEL*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DEFAULT-REPLACEMENT-CHARACTER*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DEFAULT-REPLACEMENT-CHARACTER-ALIST*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*REQUIRING*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*INSPECT-LEVEL*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*INSPECT-HISTORY*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*INSPECT-MODE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*OLD-PRINT-LEVEL*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*OLD-PRINT-LENGTH*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*QUIT-TAGS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*QUIT-TAG*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*BREAK-LEVEL*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*BREAK-ENV*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*IHS-BASE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*IHS-TOP*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*IHS-CURRENT*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*FRS-BASE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*FRS-TOP*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*TPL-CONTINUABLE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*TPL-PROMPT-HOOK*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LAST-ERROR*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*BREAK-MESSAGE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*BREAK-READTABLE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*TPL-LEVEL*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*BREAK-HIDDEN-FUNCTIONS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*BREAK-HIDDEN-PACKAGES*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("TPL-COMMANDS"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LISP-INITIALIZED*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*ALLOW-RECURSIVE-DEBUG*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DEBUG-STATUS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*CONSOLE-LOCK*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*CONSOLE-AVAILABLE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*CONSOLE-OWNER*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DEBUG-TPL-COMMANDS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*TPL-LAST-LOAD*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*TPL-LAST-COMPILE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DEFAULT-DEBUGGER-MAXIMUM-DEPTH*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DEBUGGER-WAITING-LIST-LOCK*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DEBUGGER-WAITING-LIST*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DEBUGGER-LOCK*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DEBUGGEE-ELECT*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DEBUGGEE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*TPL-EVALHOOK*"), VAL_DENOT("MKCL_OBJNULL")},
};

struct mkcl_variable_declaration mkcl_si_declare_constants[] = {
  {SYMBOL_NAME("UNBOUND"), VAL_DENOT("MKCL_UNBOUND")},
  {SYMBOL_NAME("C-ARGUMENTS-LIMIT"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_C_ARGUMENTS_LIMIT)")},
  {SYMBOL_NAME("C-CHAR-BIT"), VAL_DENOT("MKCL_MAKE_FIXNUM(CHAR_BIT)")},
  {SYMBOL_NAME("C-CHAR-MAX"), VAL_DENOT("MKCL_MAKE_FIXNUM(CHAR_MAX)")},
  {SYMBOL_NAME("C-CHAR-MIN"), VAL_DENOT("MKCL_MAKE_FIXNUM(CHAR_MIN)")},
  {SYMBOL_NAME("C-INT-MAX"), VAL_DENOT("MKCL_OBJNULL")}, /* See main.d */
  {SYMBOL_NAME("C-INT-MIN"), VAL_DENOT("MKCL_OBJNULL")}, /* See main.d */
  {SYMBOL_NAME("C-SHORT-MAX"), VAL_DENOT("MKCL_MAKE_FIXNUM(SHRT_MAX)")},
  {SYMBOL_NAME("C-SHORT-MIN"), VAL_DENOT("MKCL_MAKE_FIXNUM(SHRT_MIN)")},
  {SYMBOL_NAME("C-LONG-MAX"), VAL_DENOT("MKCL_OBJNULL")}, /* See main.d */
  {SYMBOL_NAME("C-LONG-MIN"), VAL_DENOT("MKCL_OBJNULL")}, /* See main.d */
  {SYMBOL_NAME("C-LONG-LONG-MAX"), VAL_DENOT("MKCL_OBJNULL")}, /* See main.d */
  {SYMBOL_NAME("C-LONG-LONG-MIN"), VAL_DENOT("MKCL_OBJNULL")}, /* See main.d */
  {SYMBOL_NAME("C-UCHAR-MAX"), VAL_DENOT("MKCL_MAKE_FIXNUM(UCHAR_MAX)")},
  {SYMBOL_NAME("C-UINT-MAX"), VAL_DENOT("MKCL_OBJNULL")}, /* See main.d */
  {SYMBOL_NAME("C-USHORT-MAX"), VAL_DENOT("MKCL_MAKE_FIXNUM(USHRT_MAX)")},
  {SYMBOL_NAME("C-ULONG-MAX"), VAL_DENOT("MKCL_OBJNULL")}, /* See main.d */
  {SYMBOL_NAME("C-ULONG-LONG-MAX"), VAL_DENOT("MKCL_OBJNULL")}, /* See main.d */
  {SYMBOL_NAME("CL-WORD-BITS"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_WORD_BITS)")},
  {SYMBOL_NAME("+UNICODE-DATABASE+"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("+MKCL-VERSION-NUMBER+"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_VERSION_NUMBER)")},
  {SYMBOL_NAME("+MKCL-FASL-VERSION+"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_FASL_VERSION)")},
  {SYMBOL_NAME("NAMED-SIGNAL-LIMIT"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_BASE_SIGMAX)")},
  {SYMBOL_NAME("SIGNAL-LIMIT"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_SIGMAX)")},
};

struct exposed_symbol {
  struct mkcl_base_string symbol_name;
  char * exposition;
};

static struct exposed_symbol const exposed_symbols[] = {
  /*  { SYMBOL_NAME("SOMETHING"), "MK_SI_something" }, */
  {SYMBOL_NAME("#!"), "MK_SI_HASH_BANG"},
  {SYMBOL_NAME("."), "MK_SI_DOT"},
  {SYMBOL_NAME("ALL-ENABLED-FPE"), "MK_SI_all_enabled_fpe"},
  {SYMBOL_NAME("ALL-LOGICAL-PATHNAME-TRANSLATIONS"), "MK_SI_all_logical_pathname_translations"},
  {SYMBOL_NAME("ALL-RAISED-FPE"), "MK_SI_all_raised_fpe"},
  {SYMBOL_NAME("ALLOCATE-FOREIGN-DATA"), "MK_SI_allocate_foreign_data"},
  {SYMBOL_NAME("ALLOCATE-RAW-INSTANCE"), "MK_SI_allocate_raw_instance"},
  {SYMBOL_NAME("ANSI-CLOSE"), "MK_SI_ansi_close"},
  {SYMBOL_NAME("ANSI-INPUT-STREAM-P"), "MK_SI_ansi_input_stream_p"},
  {SYMBOL_NAME("ANSI-OPEN-STREAM-P"), "MK_SI_ansi_open_stream_p"},
  {SYMBOL_NAME("ANSI-OUTPUT-STREAM-P"), "MK_SI_ansi_output_stream_p"},
  {SYMBOL_NAME("ANSI-STREAM-ELEMENT-TYPE"), "MK_SI_ansi_stream_element_type"},
  {SYMBOL_NAME("ANSI-STREAMP"), "MK_SI_ansi_streamp"},
  {SYMBOL_NAME("APPLY-FROM-TEMP-STACK-FRAME"), "MK_SI_apply_from_temp_stack_frame"},
  {SYMBOL_NAME("ASET"), "MK_SI_aset"},
  {SYMBOL_NAME("BASE-STRING-P"), "MK_SI_base_string_p"},
  {SYMBOL_NAME("BC-DISASSEMBLE"), "MK_SI_bc_disassemble"},
  {SYMBOL_NAME("BC-SPLIT"), "MK_SI_bc_split"},
  {SYMBOL_NAME("BDS-TOP"), "MK_SI_bds_top"},
  {SYMBOL_NAME("BDS-VAL"), "MK_SI_bds_val"},
  {SYMBOL_NAME("BDS-VAR"), "MK_SI_bds_var"},
  {SYMBOL_NAME("BINDING-STACK"), "MK_SI_binding_stack"},
  {SYMBOL_NAME("BIT-ARRAY-OP"), "MK_SI_bit_array_op"},
  {SYMBOL_NAME("BYTECODE"), "MK_SI_bytecode"},
  {SYMBOL_NAME("BYTECODE-CLOSURE"), "MK_SI_bytecode_closure"},
  {SYMBOL_NAME("BYTECODEP"), "MK_SI_bytecodep"},
  {SYMBOL_NAME("CALL-CFUN"), "MK_SI_call_cfun"},
  {SYMBOL_NAME("CALL-STACK"), "MK_SI_call_stack"},
  {SYMBOL_NAME("CHAR-SET"), "MK_SI_char_set"},
  {SYMBOL_NAME("C-INT-MAX"), "MK_SI_c_int_max"},
  {SYMBOL_NAME("C-INT-MIN"), "MK_SI_c_int_min"},
  {SYMBOL_NAME("CLEAR-ALL-FPE"), "MK_SI_clear_all_fpe"},
  {SYMBOL_NAME("CLEAR-COMPILER-PROPERTIES"), "MK_SI_clear_compiler_properties"},
  {SYMBOL_NAME("CLEAR-FPE"), "MK_SI_clear_fpe"},
  {SYMBOL_NAME("CLEAR-GFUN-CACHE"), "MK_SI_clear_gfun_cache"},
  {SYMBOL_NAME("CLONE-CLOSURE"), "MK_SI_clone_closure"},
  {SYMBOL_NAME("C-LONG-LONG-MAX"), "MK_SI_c_long_long_max"},
  {SYMBOL_NAME("C-LONG-LONG-MIN"), "MK_SI_c_long_long_min"},
  {SYMBOL_NAME("C-LONG-MAX"), "MK_SI_c_long_max"},
  {SYMBOL_NAME("C-LONG-MIN"), "MK_SI_c_long_min"},
  {SYMBOL_NAME("CLOSED-STREAM-ERROR"), "MK_SI_closed_stream_error"},
  {SYMBOL_NAME("CLOSE-PACKAGE"), "MK_SI_close_package"},
  {SYMBOL_NAME("CLOSURE-ENV"), "MK_SI_closure_env"},
  {SYMBOL_NAME("CLOSURE-DEPTH"), "MK_SI_closure_depth"},
  {SYMBOL_NAME("CLOSURE-LEVEL"), "MK_SI_closure_level"},
  {SYMBOL_NAME("CLOSURE-LEVEL-OUTER-LEVEL"), "MK_SI_closure_level_outer_level"},
  {SYMBOL_NAME("CLOSURE-LEVEL-SET-VAR"), "MK_SI_closure_level_set_var"},
  {SYMBOL_NAME("CLOSURE-LEVEL-SIZE"), "MK_SI_closure_level_size"},
  {SYMBOL_NAME("CLOSURE-LEVEL-VAR"), "MK_SI_closure_level_var"},
  {SYMBOL_NAME("CLOSUREP"), "MK_SI_closurep"},
  {SYMBOL_NAME("CLOSURE-PRODUCER"), "MK_SI_closure_producer"},
  {SYMBOL_NAME("CMP-ENV-REGISTER-MACROLET"), "MK_SI_cmp_env_register_macrolet"},
  {SYMBOL_NAME("CODE-BLOCK"), "MK_SI_code_block"},
  {SYMBOL_NAME("COERCE-TO-BASE-STRING"), "MK_SI_coerce_to_base_string"},
  {SYMBOL_NAME("COERCE-TO-CHARACTER-STRING"), "MK_SI_coerce_to_character_string"},
  {SYMBOL_NAME("COERCE-TO-FILENAME"), "MK_SI_coerce_to_filename"},
  {SYMBOL_NAME("COERCE-TO-FUNCTION"), "MK_SI_coerce_to_function"},
  {SYMBOL_NAME("COERCE-TO-PACKAGE"), "MK_SI_coerce_to_package"},
  {SYMBOL_NAME("COMPILED-CLOSURE"), "MK_SI_compiled_closure"},
  {SYMBOL_NAME("COMPILED-CLOSURE-DISPLAY"), "MK_SI_compiled_closure_display"},
  {SYMBOL_NAME("COMPILED-CLOSURE-LEVEL"), "MK_SI_compiled_closure_level"},
  {SYMBOL_NAME("COMPILED-DEBUG-INFORMATION"), "MK_SI_compiled_debug_information"},
  {SYMBOL_NAME("COMPILED-FUNCTION-BLOCK"), "MK_SI_compiled_function_block"},
  {SYMBOL_NAME("COMPILED-FUNCTION-FILE"), "MK_SI_compiled_function_file"},
  {SYMBOL_NAME("COMPILED-FUNCTION-NAME"), "MK_SI_compiled_function_name"},
  {SYMBOL_NAME("COMPILED-FUNCTION-OWNER"), "MK_SI_compiled_function_owner"},
  {SYMBOL_NAME("CONCATENATE-BASE-STRINGS"), "MK_SI_concatenate_base_strings"},
  {SYMBOL_NAME("CONCATENATE-STRINGS"), "MK_SI_concatenate_strings"},
  {SYMBOL_NAME("CONVERT-CMP-LEXICAL-INFO"), "MK_SI_convert_cmp_lexical_info"},
  {SYMBOL_NAME("COPY-INSTANCE"), "MK_SI_copy_instance"},
  {SYMBOL_NAME("COPY-STREAM"), "MK_SI_copy_stream"},
  {SYMBOL_NAME("COPY-TO-SIMPLE-BASE-STRING"), "MK_SI_copy_to_simple_base_string"},
  {SYMBOL_NAME("COPY-TO-SIMPLE-STRING"), "MK_SI_copy_to_simple_string"},
  {SYMBOL_NAME("C-UINT-MAX"), "MK_SI_c_uint_max"},
  {SYMBOL_NAME("C-ULONG-LONG-MAX"), "MK_SI_c_ulong_long_max"},
  {SYMBOL_NAME("C-ULONG-MAX"), "MK_SI_c_ulong_max"},
  {SYMBOL_NAME("DISABLE-FPE"), "MK_SI_disable_fpe"},
  {SYMBOL_NAME("DISABLE-INTERRUPTS"), "MK_SI_disable_interrupts"},
  {SYMBOL_NAME("DISPLAY-SIGNAL-DISPOSITIONS"), "MK_SI_display_signal_dispositions"},
  {SYMBOL_NAME("DO-DEFTYPE"), "MK_SI_do_deftype"},
  {SYMBOL_NAME("DO-READ-SEQUENCE"), "MK_SI_do_read_sequence"},
  {SYMBOL_NAME("DO-SIGSEGV"), "MK_SI_do_sigsegv"},
  {SYMBOL_NAME("DO-WRITE-SEQUENCE"), "MK_SI_do_write_sequence"},
  {SYMBOL_NAME("DYN-CONS"), "MK_SI_dyn_cons"},
  {SYMBOL_NAME("ELT-SET"), "MK_SI_elt_set"},
  {SYMBOL_NAME("ENABLE-FPE"), "MK_SI_enable_fpe"},
  {SYMBOL_NAME("ENABLE-INTERRUPTS"), "MK_SI_enable_interrupts"},
  {SYMBOL_NAME("EVAL-IN-ENV"), "MK_SI_eval_in_env"},
  {SYMBOL_NAME("ERRNO-STRING"), "MK_SI_errno_string"},
  {SYMBOL_NAME("FAST-READ-FROM-BASE-STRING"), "MK_SI_fast_read_from_base_string"},
  {SYMBOL_NAME("FILE-COLUMN"), "MK_SI_file_column"},
  {SYMBOL_NAME("FILE-KIND"), "MK_SI_file_kind"},
  {SYMBOL_NAME("FILL-ARRAY-WITH-ELT"), "MK_SI_fill_array_with_elt"},
  {SYMBOL_NAME("FILL-POINTER-SET"), "MK_SI_fill_pointer_set"},
  {SYMBOL_NAME("FIND-FOREIGN-SYMBOL"), "MK_SI_find_foreign_symbol"},
  {SYMBOL_NAME("FIND-RELATIVE-PACKAGE"), "MK_SI_find_relative_package"},
  {SYMBOL_NAME("FLOAT-INFINITY-P"), "MK_SI_float_infinity_p"},
  {SYMBOL_NAME("FLOAT-NAN-P"), "MK_SI_float_nan_p"},
  {SYMBOL_NAME("FOREIGN"), "MK_SI_foreign"},
  {SYMBOL_NAME("FOREIGN-ADDRESS"), "MK_SI_foreign_address"},
  {SYMBOL_NAME("FOREIGN-INDEXED"), "MK_SI_foreign_indexed"},
  {SYMBOL_NAME("FOREIGNP"), "MK_SI_foreignp"},
  {SYMBOL_NAME("FOREIGN-RECAST"), "MK_SI_foreign_recast"},
  {SYMBOL_NAME("FOREIGN-REF"), "MK_SI_foreign_ref"},
  {SYMBOL_NAME("FOREIGN-REF-ELT"), "MK_SI_foreign_ref_elt"},
  {SYMBOL_NAME("FOREIGN-SET"), "MK_SI_foreign_set"},
  {SYMBOL_NAME("FOREIGN-SET-ELT"), "MK_SI_foreign_set_elt"},
  {SYMBOL_NAME("FOREIGN-TAG"), "MK_SI_foreign_tag"},
  {SYMBOL_NAME("FORMAT-ERROR"), "MK_SI_format_error"},
  {SYMBOL_NAME("FORMATTER-AUX"), "MK_SI_formatter_aux"},
  {SYMBOL_NAME("FPE-ENABLED-P"), "MK_SI_fpe_enabled_p"},
  {SYMBOL_NAME("FPE-RAISED-P"), "MK_SI_fpe_raised_p"},
  {SYMBOL_NAME("FRAME-STACK"), "MK_SI_frame_stack"},
  {SYMBOL_NAME("FREE-FOREIGN-DATA"), "MK_SI_free_foreign_data"},
  {SYMBOL_NAME("FRS-BDS"), "MK_SI_frs_bds"},
  {SYMBOL_NAME("FRS-IHS"), "MK_SI_frs_ihs"},
  {SYMBOL_NAME("FRS-TAG"), "MK_SI_frs_tag"},
  {SYMBOL_NAME("FRS-TOP"), "MK_SI_frs_top"},
  {SYMBOL_NAME("FSET"), "MK_SI_fset"},
  {SYMBOL_NAME("FUNCTION-BLOCK-NAME"), "MK_SI_function_block_name"},
  {SYMBOL_NAME("GC"), "MK_SI_gc"},
  {SYMBOL_NAME("GC-DUMP"), "MK_SI_gc_dump"},
  {SYMBOL_NAME("GC-OFF"), "MK_SI_gc_off"},
  {SYMBOL_NAME("GC-ON"), "MK_SI_gc_on"},
  {SYMBOL_NAME("GC-STATS"), "MK_SI_gc_stats"},
  {SYMBOL_NAME("GDB"), "MK_SI_gdb"},
  {SYMBOL_NAME("GENERATE-FORWARD-FUN-REF-HANDLER"), "MK_SI_generate_forward_fun_ref_handler"},
  {SYMBOL_NAME("GENERIC-FUNCTION-P"), "MK_SI_generic_function_p"},
  {SYMBOL_NAME("GET-BINDING-STACK-LIMIT"), "MK_SI_get_binding_stack_limit"},
  {SYMBOL_NAME("GET-BUFFERING-MODE"), "MK_SI_get_buffering_mode"},
  {SYMBOL_NAME("GET-CALL-STACK-LIMIT"), "MK_SI_get_call_stack_limit"},
  {SYMBOL_NAME("GET-FINALIZER"), "MK_SI_get_finalizer"},
  {SYMBOL_NAME("GET-FUN-REF-SYM"), "MK_SI_get_fun_ref_sym"},
  {SYMBOL_NAME("GET-FRAME-STACK-LIMIT"), "MK_SI_get_frame_stack_limit"},
  {SYMBOL_NAME("GET-HEAP-SIZE-LIMIT"), "MK_SI_get_heap_size_limit"},
  {SYMBOL_NAME("GET-LISP-TEMP-STACK-LIMIT"), "MK_SI_get_lisp_temp_stack_limit"},
  {SYMBOL_NAME("GET-LOCAL-TIME-ZONE"), "MK_SI_get_local_time_zone"},
  {SYMBOL_NAME("GET-SYS-LIBRARY-PATHNAME"), "MK_SI_get_SYS_library_pathname"},
  {SYMBOL_NAME("GET-SYSPROP"), "MK_SI_get_sysprop"},
  {SYMBOL_NAME("HASH-EQL"), "MK_SI_hash_eql"},
  {SYMBOL_NAME("HASH-EQUAL"), "MK_SI_hash_equal"},
  {SYMBOL_NAME("HASH-EQUALP"), "MK_SI_hash_equalp"},
  {SYMBOL_NAME("HASH-SET"), "MK_SI_hash_set"},
  {SYMBOL_NAME("HASH-TABLE-ITERATOR"), "MK_SI_hash_table_iterator"},
  {SYMBOL_NAME("HASH-TABLES-STATISTICS"), "MK_SI_hash_tables_statistics"},
  {SYMBOL_NAME("IHS-BDS-MARKER"), "MK_SI_ihs_bds_marker"},
  {SYMBOL_NAME("IHS-ENV"), "MK_SI_ihs_env"},
  {SYMBOL_NAME("IHS-FUN"), "MK_SI_ihs_fun"},
  {SYMBOL_NAME("IHS-NEXT"), "MK_SI_ihs_next"},
  {SYMBOL_NAME("IHS-PREV"), "MK_SI_ihs_prev"},
  {SYMBOL_NAME("IHS-TOP"), "MK_SI_ihs_top"},
  {SYMBOL_NAME("INITIAL-FLOATING-POINT-EXCEPTION-SET"), "MK_SI_initial_floating_point_exception_set"},
  {SYMBOL_NAME("INSTALL-SIGSEGV-MONITOR"), "MK_SI_install_sigsegv_monitor"},
  {SYMBOL_NAME("INSTANCE"), "MK_SI_instance"},
  {SYMBOL_NAME("INSTANCE-CLASS"), "MK_SI_instance_class"},
  {SYMBOL_NAME("INSTANCE-CLASS-SET"), "MK_SI_instance_class_set"},
  {SYMBOL_NAME("INSTANCE-LENGTH"), "MK_SI_instance_length"},
  {SYMBOL_NAME("INSTANCEP"), "MK_SI_instancep"},
  {SYMBOL_NAME("INSTANCE-SIG"), "MK_SI_instance_sig"},
  {SYMBOL_NAME("INSTANCE-SIG-SET"), "MK_SI_instance_sig_set"},
  {SYMBOL_NAME("INSTANCE-REF"), "MK_SI_instance_ref"},
  {SYMBOL_NAME("INSTANCE-REF-SAFE"), "MK_SI_instance_ref_safe"},
  {SYMBOL_NAME("INSTANCE-SET"), "MK_SI_instance_set"},
  {SYMBOL_NAME("INSTANCE-SIG-SET"), "MK_SI_instance_sig_set"},
  {SYMBOL_NAME("INTERRUPT-STATUS"), "MK_SI_interrupt_status"},
  {SYMBOL_NAME("LAMBDA-BLOCK"), "MK_SI_lambda_block"},
  {SYMBOL_NAME("LIBC-ERROR-STRING"), "MK_SI_libc_error_string"},
  {SYMBOL_NAME("LISP-TEMP-STACK"), "MK_SI_lisp_temp_stack"},
  {SYMBOL_NAME("LIST-ALL-CHILDREN"), "MK_SI_list_all_children"},
  {SYMBOL_NAME("LIST-LIBRARIES"), "MK_SI_list_libraries"},
  {SYMBOL_NAME("LOAD-BINARY"), "MK_SI_load_binary"},
  {SYMBOL_NAME("LOAD-FOREIGN-MODULE"), "MK_SI_load_foreign_module"},
  {SYMBOL_NAME("LOAD-SOURCE"), "MK_SI_load_source"},
  {SYMBOL_NAME("LOG1P"), "MK_SI_log1p"},
  {SYMBOL_NAME("MACRO"), "MK_SI_macro"},
  {SYMBOL_NAME("MAKE-PURE-ARRAY"), "MK_SI_make_pure_array"},
  {SYMBOL_NAME("MAKE-VECTOR"), "MK_SI_make_vector"},
  {SYMBOL_NAME("MAKE-DYNAMIC-CALLBACK"), "MK_SI_make_dynamic_callback"},
  {SYMBOL_NAME("MAKE-ENCODING"), "MK_SI_make_encoding"},
  {SYMBOL_NAME("MAKE-FOREIGN-DATA-FROM-ARRAY"), "MK_SI_make_foreign_data_from_array"},
  {SYMBOL_NAME("MAKE-FOREIGN-NULL-POINTER"), "MK_SI_make_foreign_null_pointer"},
  {SYMBOL_NAME("MAKE-LAMBDA"), "MK_SI_make_lambda"},
  {SYMBOL_NAME("MAKE-STRING-OUTPUT-STREAM-FROM-STRING"), "MK_SI_make_string_output_stream_from_string"},
  {SYMBOL_NAME("MAKE-STRUCTURE"), "MK_SI_make_structure"},
  {SYMBOL_NAME("MANGLE-FUNCTION-NAME"), "MK_SI_mangle_function_name"},
  {SYMBOL_NAME("MANGLE-NAME"), "MK_SI_mangle_name"},
  {SYMBOL_NAME("MANGLE-STRING"), "MK_SI_mangle_string"},
  {SYMBOL_NAME("MANGLE-SYMBOL"), "MK_SI_mangle_symbol"},
  {SYMBOL_NAME("MAYBE-QUOTE"), "MK_SI_maybe_quote"},
  {SYMBOL_NAME("MEMBER1"), "MK_SI_member1"},
  {SYMBOL_NAME("MEMQ"), "MK_SI_memq"},
  {SYMBOL_NAME("MEM-STATS"), "MK_SI_mem_stats"},
  {SYMBOL_NAME("MKCL-VERSION"), "MK_SI_mkcl_version"},
  {SYMBOL_NAME("MKCL-MAJOR-VERSION"), "MK_SI_mkcl_major_version"},
  {SYMBOL_NAME("MKCL-MINOR-VERSION"), "MK_SI_mkcl_minor_version"},
  {SYMBOL_NAME("MKCL-PATCH-LEVEL"), "MK_SI_mkcl_patch_level"},
  {SYMBOL_NAME("NON-INTERACTIVE-THREAD-DEBUGGER-TRAP"), "MK_SI_non_interactive_thread_debugger_trap"},
  {SYMBOL_NAME("NULL-POINTER-P"), "MK_SI_null_pointer_p"},
  {SYMBOL_NAME("OF-CLASS-P"), "MK_SI_of_class_p"},
  {SYMBOL_NAME("OS-FILE-ERROR"), "MK_SI_OS_file_error"},
  {SYMBOL_NAME("OS-STREAM-ERROR"), "MK_SI_OS_stream_error"},
  {SYMBOL_NAME("OBJNULL"), "MK_SI_objnull"},
  {SYMBOL_NAME("OBJNULL-VALUE-P"), "MK_SI_objnull_value_p"},
  {SYMBOL_NAME("OUTPUT-FLOAT"), "MK_SI_output_float"},
  {SYMBOL_NAME("OUTPUT-FLOAT-INFINITY"), "MK_SI_output_float_infinity"},
  {SYMBOL_NAME("OUTPUT-FLOAT-NAN"), "MK_SI_output_float_nan"},
  {SYMBOL_NAME("PACKAGE-CHILDREN"), "MK_SI_package_children"},
  {SYMBOL_NAME("PACKAGE-CLOSED-P"), "MK_SI_package_closed_p"},
  {SYMBOL_NAME("PACKAGE-PARENT"), "MK_SI_package_parent"},
  {SYMBOL_NAME("PACKAGE-HASH-TABLES"), "MK_SI_package_hash_tables"},
  {SYMBOL_NAME("PACKAGES-IN-WAITING"), "MK_SI_packages_in_waiting"},
  {SYMBOL_NAME("PATHNAME-TRANSLATIONS"), "MK_SI_pathname_translations"},
  {SYMBOL_NAME("POINTER"), "MK_SI_pointer"},
  {SYMBOL_NAME("PROCESS-DECLARATIONS"), "MK_SI_process_declarations"},
  {SYMBOL_NAME("PROCESS-LAMBDA-LIST"), "MK_SI_process_lambda_list"},
  {SYMBOL_NAME("PROPERTY-LIST"), "MK_SI_property_list"},
  {SYMBOL_NAME("PUT-F"), "MK_SI_put_f"},
  {SYMBOL_NAME("PUT-PROPERTIES"), "MK_SI_put_properties"},
  {SYMBOL_NAME("PUT-SYSPROP"), "MK_SI_put_sysprop"},
  {SYMBOL_NAME("PUTPROP"), "MK_SI_putprop"},
  {SYMBOL_NAME("QUASIQUOTE"), "MK_SI_quasiquote"},
  {SYMBOL_NAME("RAISE-FPE"), "MK_SI_raise_fpe"},
  {SYMBOL_NAME("READ-OBJECT-OR-IGNORE"), "MK_SI_read_object_or_ignore"},
  {SYMBOL_NAME("READTABLE-CASE-SET"), "MK_SI_readtable_case_set"},
  {SYMBOL_NAME("RELEASE-FFI-AREA"), "MK_SI_release_ffi_area"},
  {SYMBOL_NAME("REGISTER-SHUTDOWN-THREAD"), "MK_SI_register_shutdown_thread"},
  {SYMBOL_NAME("REGISTER-SHUTDOWN-WATCHDOG-THREAD"), "MK_SI_register_shutdown_watchdog_thread"},
  {SYMBOL_NAME("REM-F"), "MK_SI_rem_f"},
  {SYMBOL_NAME("REM-SYSPROP"), "MK_SI_rem_sysprop"},
  {SYMBOL_NAME("REOPEN-PACKAGE"), "MK_SI_reopen_package"},
  {SYMBOL_NAME("REPLACE-ARRAY"), "MK_SI_replace_array"},
  {SYMBOL_NAME("RESET-ALLOCATION-STATISTICS"), "MK_SI_reset_allocation_statistics"},
  {SYMBOL_NAME("ROOM-REPORT"), "MK_SI_room_report"},
  {SYMBOL_NAME("ROW-MAJOR-ASET"), "MK_SI_row_major_aset"},
  {SYMBOL_NAME("SAFE-EVAL"), "MK_SI_safe_eval"},
  {SYMBOL_NAME("SAMPLE-ALLOCATION-STATISTICS"), "MK_SI_sample_allocation_statistics"},
  {SYMBOL_NAME("SCHAR-SET"), "MK_SI_schar_set"},
  {SYMBOL_NAME("SCH-FRS-BASE"), "MK_SI_sch_frs_base"},
  {SYMBOL_NAME("SCRUB-VALUES"), "MK_SI_scrub_values"},
  {SYMBOL_NAME("SELECT-PACKAGE"), "MK_SI_select_package"},
  {SYMBOL_NAME("SELF-TRUENAME"), "MK_SI_self_truename"},
  {SYMBOL_NAME("SET-BINDING-STACK-LIMIT"), "MK_SI_set_binding_stack_limit"},
  {SYMBOL_NAME("SET-BUFFERING-MODE"), "MK_SI_set_buffering_mode"},
  {SYMBOL_NAME("SET-CLASS-PROPER-NAME"), "MK_SI_set_class_proper_name"},
  {SYMBOL_NAME("SET-COMPILED-FUNCTION-NAME"), "MK_SI_set_compiled_function_name"},
  {SYMBOL_NAME("SET-COMPILED-FUNCTION-OWNER"), "MK_SI_set_compiled_function_owner"},
  {SYMBOL_NAME("SET-FINALIZER"), "MK_SI_set_finalizer"},
  {SYMBOL_NAME("SETF-LAMBDA"), "MK_SI_setf_lambda"},
  {SYMBOL_NAME("SETF-METHOD"), "MK_SI_setf_method"},
  {SYMBOL_NAME("SET-FRAME-STACK-LIMIT"), "MK_SI_set_frame_stack_limit"},
  {SYMBOL_NAME("SETF-SYMBOL"), "MK_SI_setf_symbol"},
  {SYMBOL_NAME("SETF-UPDATE"), "MK_SI_setf_update"},
  {SYMBOL_NAME("SET-HEAP-SIZE-LIMIT"), "MK_SI_set_heap_size_limit"},
  {SYMBOL_NAME("SET-LISP-TEMP-STACK-LIMIT"), "MK_SI_set_lisp_temp_stack_limit"},
  {SYMBOL_NAME("SET-SYMBOL-PLIST"), "MK_SI_set_symbol_plist"},
  {SYMBOL_NAME("SETUP-FOR-GDB"), "MK_SI_setup_for_gdb"},
  {SYMBOL_NAME("SHARP-A-READER"), "MK_SI_sharp_a_reader"},
  {SYMBOL_NAME("SHARP-S-READER"), "MK_SI_sharp_s_reader"},
  {SYMBOL_NAME("SHUTDOWN-IN-PROGRESS-P"), "MK_SI_shutdown_in_progress_p"},
  {SYMBOL_NAME("SHUTDOWN-MKCL"), "MK_SI_shutdown_mkcl"},
  {SYMBOL_NAME("SHUTDOWN-MKCL-THREADS"), "MK_SI_shutdown_mkcl_threads"},
  {SYMBOL_NAME("SHUTDOWN-WATCHDOG-THREAD"), "MK_SI_shutdown_watchdog_thread"},
  {SYMBOL_NAME("SIGHUP-HANDLER"), "MK_SI_sighup_handler"},
  {SYMBOL_NAME("SIGINT-HANDLER"), "MK_SI_sigint_handler"},
  {SYMBOL_NAME("SIGNUM-TO-SIGNAL-NAME"), "MK_SI_signum_to_signal_name"},
  {SYMBOL_NAME("SIGTERM-HANDLER"), "MK_SI_sigterm_handler"},
  {SYMBOL_NAME("SIMPLE-BASE-STRING-P"), "MK_SI_simple_base_string_p"},
  {SYMBOL_NAME("SIMPLE-CONTROL-ERROR"), "MK_SI_simple_control_error"},
  {SYMBOL_NAME("SIMPLE-PACKAGE-ERROR"), "MK_SI_simple_package_error"},
  {SYMBOL_NAME("SIMPLE-PROGRAM-ERROR"), "MK_SI_simple_program_error"},
  {SYMBOL_NAME("SIMPLE-READER-ERROR"), "MK_SI_simple_reader_error"},
  {SYMBOL_NAME("SIZE-OF-FOREIGN-ELT-TYPE"), "MK_SI_size_of_foreign_elt_type"},
  {SYMBOL_NAME("SL-BOUNDP"), "MK_SI_sl_boundp"},
  {SYMBOL_NAME("SL-MAKUNBOUND"), "MK_SI_sl_makunbound"},
  {SYMBOL_NAME("SPECIALP"), "MK_SI_specialp"},
  {SYMBOL_NAME("STANDARD-READTABLE"), "MK_SI_standard_readtable"},
  {SYMBOL_NAME("STEPPER"), "MK_SI_stepper"},
  {SYMBOL_NAME("STREAM-EXTERNAL-FORMAT-SET"), "MK_SI_stream_external_format_set"},
  {SYMBOL_NAME("STRUCTURE-INCLUDE"), "MK_SI_structure_include"},
  {SYMBOL_NAME("STRUCTURE-LENGTH"), "MK_SI_structure_length"},
  {SYMBOL_NAME("STRUCTURE-NAME"), "MK_SI_structure_name"},
  {SYMBOL_NAME("STRUCTUREP"), "MK_SI_structurep"},
  {SYMBOL_NAME("STRUCTURE-PRINT-FUNCTION"), "MK_SI_structure_print_function"},
  {SYMBOL_NAME("STRUCTURE-REF"), "MK_SI_structure_ref"},
  {SYMBOL_NAME("STRUCTURE-SET"), "MK_SI_structure_set"},
  {SYMBOL_NAME("STRUCTURE-SUBTYPE-P"), "MK_SI_structure_subtype_p"},
  {SYMBOL_NAME("SUBCLASSP"), "MK_SI_subclassp"},
  {SYMBOL_NAME("SVSET"), "MK_SI_svset"},
  {SYMBOL_NAME("SYMBOL-MACRO"), "MK_SI_symbol_macro"},
  {SYMBOL_NAME("SYSTEM-PROPERTIES"), "MK_SI_system_properties"},
  {SYMBOL_NAME("TEMP-STACK-FRAME"), "MK_SI_temp_stack_frame"},
  {SYMBOL_NAME("TERMINAL-SIGNAL-HANDLER"), "MK_SI_terminal_signal_handler"},
  {SYMBOL_NAME("TOP-APPLY"), "MK_SI_top_apply"},
  {SYMBOL_NAME("TRACE-SPECIALS"), "MK_SI_trace_specials"},
  {SYMBOL_NAME("TRIM-FFI-ARGUMENTS-STAGING-AREA"), "MK_SI_trim_ffi_arguments_staging_area"},
  {SYMBOL_NAME("TRIM-DYNAMIC-CONS-STACK"), "MK_SI_trim_dynamic_cons_stack"},
  {SYMBOL_NAME("UNAME"), "MK_SI_uname"},
  {SYMBOL_NAME("UNBOUND-VALUE-P"), "MK_SI_unbound_value_p"},
  {SYMBOL_NAME("UNIVERSAL-ERROR-HANDLER"), "MK_SI_universal_error_handler"},
  {SYMBOL_NAME("UNLOAD-FOREIGN-MODULE"), "MK_SI_unload_foreign_module"},
  {SYMBOL_NAME("UNQUOTE"), "MK_SI_unquote"},
  {SYMBOL_NAME("UNQUOTE-NSPLICE"), "MK_SI_unquote_nsplice"},
  {SYMBOL_NAME("UNQUOTE-SPLICE"), "MK_SI_unquote_splice"},
  {SYMBOL_NAME("UNTIL"), "MK_SI_until"},
  {SYMBOL_NAME("UNTRACE-SPECIALS"), "MK_SI_untrace_specials"},
  {SYMBOL_NAME("UPDATE-FUNCTION-REFERENCES"), "MK_SI_update_function_references"},
  {SYMBOL_NAME("UTF-16"), "MK_SI_utf_16"},
  {SYMBOL_NAME("UTF-16+"), "MK_SI_utf_16P"},
  {SYMBOL_NAME("UTF-16="), "MK_SI_utf_16E"},
  {SYMBOL_NAME("UTF-16-AS-IS"), "MK_SI_utf_16_as_is"},
  {SYMBOL_NAME("UTF-16-CHAR"), "MK_SI_utf_16_char"},
  {SYMBOL_NAME("UTF-16-LAST"), "MK_SI_utf_16_last"},
  {SYMBOL_NAME("UTF-16-LENGTH"), "MK_SI_utf_16_length"},
  {SYMBOL_NAME("UTF-16+"), "MK_SI_utf_16Plus"},
  {SYMBOL_NAME("UTF-16-P"), "MK_SI_utf_16_p"},
  {SYMBOL_NAME("UTF-16-PUSH-EXTEND"), "MK_SI_utf_16_push_extend"},
  {SYMBOL_NAME("UTF-8"), "MK_SI_utf_8"},
  {SYMBOL_NAME("UTF-8+"), "MK_SI_utf_8P"},
  {SYMBOL_NAME("UTF-8="), "MK_SI_utf_8E"},
  {SYMBOL_NAME("UTF-8-AS-IS"), "MK_SI_utf_8_as_is"},
  {SYMBOL_NAME("UTF-8-CHAR"), "MK_SI_utf_8_char"},
  {SYMBOL_NAME("UTF-8-LAST"), "MK_SI_utf_8_last"},
  {SYMBOL_NAME("UTF-8-LENGTH"), "MK_SI_utf_8_length"},
  {SYMBOL_NAME("UTF-8+"), "MK_SI_utf_8Plus"},
  {SYMBOL_NAME("UTF-8-P"), "MK_SI_utf_8_p"},
  {SYMBOL_NAME("UTF-8-PUSH-EXTEND"), "MK_SI_utf_8_push_extend"},
  {SYMBOL_NAME("VALID-FUNCTION-NAME-P"), "MK_SI_valid_function_name_p"},
  {SYMBOL_NAME("WHILE"), "MK_SI_while"},
  {SYMBOL_NAME("WRITE-OBJECT"), "MK_SI_write_object"},
  {SYMBOL_NAME("WRITE-UGLY-OBJECT"), "MK_SI_write_ugly_object"},
  {SYMBOL_NAME("WRONG-TYPE-ARGUMENT"), "MK_SI_wrong_type_argument"},
  {SYMBOL_NAME("*MAKE-CONSTANT"), "MK_SI_Xmake_constant"},
  {SYMBOL_NAME("*MAKE-SPECIAL"), "MK_SI_Xmake_special"},
  {SYMBOL_NAME("*ACTION-ON-UNDEFINED-VARIABLE*"), "MK_SI_DYNVAR_action_on_undefined_variable"},
  {SYMBOL_NAME("*BACKQ-LEVEL*"), "MK_SI_DYNVAR_backq_level"},
  {SYMBOL_NAME("*CBLOCK*"), "MK_SI_DYNVAR_cblock"},
  {SYMBOL_NAME("*CIRCLE-COUNTER*"), "MK_SI_DYNVAR_circle_counter"},
  {SYMBOL_NAME("*CIRCLE-STACK*"), "MK_SI_DYNVAR_circle_stack"},
  {SYMBOL_NAME("*CLASS-NAME-HASH-TABLE*"), "MK_SI_DYNVAR_class_name_hash_table"},
  {SYMBOL_NAME("*CODE-WALKER*"), "MK_SI_DYNVAR_code_walker"},
  {SYMBOL_NAME("*CONDITION-RESTARTS*"), "MK_SI_DYNVAR_condition_restarts"},
  {SYMBOL_NAME("*CURRENT-FORM*"), "MK_SI_DYNVAR_current_form"},
  {SYMBOL_NAME("*DEFAULT-EXTERNAL-FORMAT*"), "MK_SI_DYNVAR_default_external_format"},
  {SYMBOL_NAME("*DEFAULT-FLOATING-POINT-EXCEPTION-SET*"), "MK_SI_DYNVAR_default_floating_point_exception_set"},
  {SYMBOL_NAME("*DRIBBLE-CLOSER*"), "MK_SI_DYNVAR_dribble_closer"},
  {SYMBOL_NAME("*DYNAMIC-CONS-STACK*"), "MK_SI_DYNVAR_dynamic_cons_stack"},
  {SYMBOL_NAME("*EXTENDED-CHARACTER-NAMES*"), "MK_SI_DYNVAR_extended_character_names"},
  {SYMBOL_NAME("*GC-VERBOSE*"), "MK_SI_DYNVAR_gc_verbose"},
  {SYMBOL_NAME("*HANDLER-CLUSTERS*"), "MK_SI_DYNVAR_handler_clusters"},
  {SYMBOL_NAME("*INIT-FUNCTION-PREFIX*"), "MK_SI_DYNVAR_init_function_prefix"},
  {SYMBOL_NAME("*KEEP-DEFINITIONS*"), "MK_SI_DYNVAR_keep_definitions"},
  {SYMBOL_NAME("*LOAD-HOOKS*"), "MK_SI_DYNVAR_load_hooks"},
  {SYMBOL_NAME("*LOAD-SEARCH-LIST*"), "MK_SI_DYNVAR_load_search_list"},
  {SYMBOL_NAME("*OS-STRING-FORMAT*"), "MK_SI_DYNVAR_os_string_format"},
  {SYMBOL_NAME("*PENDING-SHARP-LABELS*"), "MK_SI_DYNVAR_pending_sharp_labels"},
  {SYMBOL_NAME("*PRINT-FLOAT-EXACTLY*"), "MK_SI_DYNVAR_print_float_exactly"},
  {SYMBOL_NAME("*PRINT-PACKAGE*"), "MK_SI_DYNVAR_print_package"},
  {SYMBOL_NAME("*READ-FLOAT-EXACTLY*"), "MK_SI_DYNVAR_read_float_exactly"},
  {SYMBOL_NAME("*RELATIVE-PACKAGE-NAMES*"), "MK_SI_DYNVAR_relative_package_names"},
  {SYMBOL_NAME("*RESTART-CLUSTERS*"), "MK_SI_DYNVAR_restart_clusters"},
  {SYMBOL_NAME("*SHARP-LABELS*"), "MK_SI_DYNVAR_sharp_labels"},
  {SYMBOL_NAME("*SOURCE-LOCATION*"), "MK_SI_DYNVAR_source_location"},
  {SYMBOL_NAME("*STEP-ACTION*"), "MK_SI_DYNVAR_step_action"},
  {SYMBOL_NAME("*STEP-LEVEL*"), "MK_SI_DYNVAR_step_level"},
  {SYMBOL_NAME("+READING-FASL-FILE+"), "MK_SI_CONSTANT_reading_fasl_file"},
  {SYMBOL_NAME("+SHUTDOWN-GATE+"), "MK_SI_CONSTANT_shutdown_gate"},
  {SYMBOL_NAME("UNBOUND"), "MK_SI_CONSTANT_unbound"},
};



const struct mkcl_symbol blank_symbol = BLANK_SYMBOL_INITIALIZER;


void init_si_package(void)
{
  long i;

  for (i = 0; i < MKCL_NB_ELEMS(mkcl_si_external_symbol_names); i++)
    {
      struct mkcl_symbol * sym = &mkcl_si_external_symbols[i];
      const struct mkcl_base_string * name = &mkcl_si_external_symbol_names[i];
      const mkcl_hash_value hashed_name = hash_base_string(name->self, name->fillp, 0);

      *sym = blank_symbol;

      sym->name = (mkcl_object) name;
      sym->hashed_name = hashed_name;
      sym->hpack = (mkcl_object) &mkcl_package_si;

      struct mkcl_hashtable_entry * entry = &(external_entries[i]);
      entry->hashed_key = hashed_name;
      entry->value = (mkcl_object) sym;
      entry->key = sym->name;
      entry->next = NULL;
      
      add_entry_to_hash(hashed_name, &external_ht, entry);
    }
  
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_si_internal_symbol_names); i++)
    {
      struct mkcl_symbol * sym = &mkcl_si_internal_symbols[i];
      const struct mkcl_base_string * name = &mkcl_si_internal_symbol_names[i];
      const mkcl_hash_value hashed_name = hash_base_string(name->self, name->fillp, 0);

      *sym = blank_symbol;

      sym->name = (mkcl_object) name;
      sym->hashed_name = hashed_name;
      sym->hpack = (mkcl_object) &mkcl_package_si;

      struct mkcl_hashtable_entry * entry = &(internal_entries[i]);
      entry->hashed_key = hashed_name;
      entry->value = (mkcl_object) sym;
      entry->key = sym->name;
      entry->next = NULL;

      add_entry_to_hash(hashed_name, &internal_ht, entry);
    }

  for (i = 0; i < MKCL_NB_ELEMS(mkcl_si_declare_specials); i++)
    {
      int intern_flag;
      struct mkcl_base_string * const name = &mkcl_si_declare_specials[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_si, NULL);

      if (sym)
	{
	  sym->value = (mkcl_object) &mkcl_si_declare_specials[i].value_denotator;
	  sym->stype = mkcl_stp_special;
	}
      else
	{
	  printf("special variable not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_si_declare_constants); i++)
    {
      int intern_flag;
      struct mkcl_base_string * const name = &mkcl_si_declare_constants[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_si, NULL);

      if (sym)
	{
	  sym->value = (mkcl_object) &mkcl_si_declare_constants[i].value_denotator;
	  sym->stype = mkcl_stp_constant;
	}
      else
	{
	  printf("special variable not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_si_declare_lisp_functions); i++)
    {
      int intern_flag;
      struct mkcl_base_string * name = &mkcl_si_declare_lisp_functions[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_si, NULL);

      if (sym)
	{
	  sym->gfdef = (mkcl_object) &mkcl_si_declare_lisp_functions[i].function_object_denotator;
	}
      else
	{
	  printf("function not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_si_declare_macros); i++)
    {
      int intern_flag;
      struct mkcl_base_string * name = &mkcl_si_declare_macros[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_si, NULL);

      if (sym)
	{
	  sym->stype |= mkcl_stp_macro;
	  sym->gfdef = (mkcl_object) &mkcl_si_declare_macros[i].function_object_denotator;
	}
      else
	{
	  printf("function not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_si_declare_special_operators); i++)
    {
      int intern_flag;
      struct mkcl_base_string * name = &mkcl_si_declare_special_operators[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_si, NULL);

      if (sym)
	{
	  sym->stype |= mkcl_stp_special_form;
	  sym->gfdef = (mkcl_object) &mkcl_si_declare_special_operators[i].function_object_denotator;
	}
      else
	{
	  printf("function not found %s\n", name->self);
	  exit(1);
	}
    }
}



mkcl_index si_internal_symbol_index(struct mkcl_symbol * sym)
{
  return (sym - mkcl_si_internal_symbols);
}

mkcl_index si_external_symbol_index(struct mkcl_symbol * sym)
{
  return (sym - mkcl_si_external_symbols);
}

mkcl_index si_internal_entry_index(struct mkcl_hashtable_entry * entry)
{
  return (entry - internal_entries);
}

mkcl_index si_external_entry_index(struct mkcl_hashtable_entry * entry)
{
  return (entry - external_entries);
}



void print_si_internal_symbol_C_names(void)
{
  mkcl_index i;

  printf("\nstatic const struct mkcl_base_string si_internal_C_names[] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      printf("SYMBOL_NAME(\"mkcl_si_internal_symbols[%lu]\"),\n", i);
    }
  printf("};\n");
}

void print_si_external_symbol_C_names(void)
{
  mkcl_index i;

  printf("\nstatic const struct mkcl_base_string si_external_C_names[] = {\n");
  for (i = 0; i < external_count; i++)
    {
      printf("SYMBOL_NAME(\"mkcl_si_external_symbols[%lu]\"),\n", i);
    }
  printf("};\n");
}

const char * symbol_type(struct mkcl_symbol * sym)
{
  switch (sym->stype)
    {
    case mkcl_stp_ordinary: return "mkcl_stp_ordinary";
    case mkcl_stp_constant: return "mkcl_stp_constant";
    case mkcl_stp_special: return "mkcl_stp_special";
    case mkcl_stp_macro: return "mkcl_stp_macro";
    case mkcl_stp_special_form: return "mkcl_stp_special_form";
    default: return "";
    }
}

void print_si_internal_symbol_initializers(void)
{
  mkcl_index i;

  printf("\nstruct mkcl_symbol mkcl_si_internal_symbols[] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      struct mkcl_symbol * sym = &mkcl_si_internal_symbols[i];
      
      printf("{ mkcl_t_symbol, 0, %s, 0, ", symbol_type(sym));  /* MKCL_HEADER(stype) */
      if (sym->value == MKCL_OBJNULL) /* value */
	printf("MKCL_OBJNULL, ");
      else
	printf("%s, ", sym->value->base_string.self);
      if (sym->gfdef == mk_cl_Cnil) /* gfdef */
	printf("mk_cl_Cnil, ");
      else
	printf("(mkcl_object) %s, ", sym->gfdef->base_string.self);
      printf("mk_cl_Cnil, "); /* plist */
      printf("(mkcl_object) &mkcl_si_internal_symbol_names[%lu], ", i); /* name */
      printf("(mkcl_object) &mkcl_package_si, "); /* hpack */
      printf("mk_cl_Cnil, "); /* properly_named_class */
      printf("mk_cl_Cnil, "); /* sys_plist */
      printf("MKCL_NOT_A_SPECIAL_INDEX, "); /* special_index */
      printf("%luUL, ", sym->hashed_name); /* hashed_name */
      printf("(mkcl_object) &si_internal_C_names[%lu], ", i); /* C_name */
      printf("NULL, "); /* _C_name */
      printf("NULL, "); /* _name */
      
      printf("}, /* %lu %s */\n", i, sym->name->base_string.self);
    }
  printf("};\n");
}

void print_si_external_symbol_initializers(void)
{
  mkcl_index i;

  printf("\nstruct mkcl_symbol mkcl_si_external_symbols[] = {\n");
  for (i = 0; i < external_count; i++)
    {
      struct mkcl_symbol * sym = &mkcl_si_external_symbols[i];
      
      printf("{ mkcl_t_symbol, 0, %s, 0, ", symbol_type(sym));  /* MKCL_HEADER(stype) */
      if (sym->value == MKCL_OBJNULL) /* value */
	printf("MKCL_OBJNULL, ");
      else
	printf("%s, ", sym->value->base_string.self);
      if (sym->gfdef == mk_cl_Cnil) /* gfdef */
	printf("mk_cl_Cnil, ");
      else
	printf("(mkcl_object) %s, ", sym->gfdef->base_string.self);
      printf("mk_cl_Cnil, "); /* plist */
      printf("(mkcl_object) &mkcl_si_external_symbol_names[%lu], ", i); /* name */
      printf("(mkcl_object) &mkcl_package_si, "); /* hpack */
      printf("mk_cl_Cnil, "); /* properly_named_class */
      printf("mk_cl_Cnil, "); /* sys_plist */
      printf("MKCL_NOT_A_SPECIAL_INDEX, "); /* special_index */
      printf("%luUL, ", sym->hashed_name); /* hashed_name */
      printf("(mkcl_object) &si_external_C_names[%lu], ", i); /* C_name */
      printf("NULL, "); /* _C_name */
      printf("NULL, "); /* _name */
      
      printf("}, /* %lu %s */\n", i, sym->name->base_string.self);
    }
  printf("};\n");
}


void print_si_internal_hashtable_entry_initializers(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry internal_entries[internal_count] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      struct mkcl_hashtable_entry * entry = &internal_entries[i];
      

      printf("{ ");
      if (entry->next)
	printf("&internal_entries[%lu], ", si_internal_entry_index(entry->next));
      else
	printf("NULL, ");
      if (mkcl_Null(entry->value))
	{
	  printf("mk_cl_Cnil, ");
	  printf("0, ");
	  printf("mk_cl_Cnil, ");
	}
      else
	{
	  mkcl_index sym_index = si_internal_symbol_index((struct mkcl_symbol *) entry->value);

	  printf("(mkcl_object) &mkcl_si_internal_symbol_names[%lu], ", sym_index);
	  printf("%luUL, ", entry->hashed_key);
	  printf("(mkcl_object) &mkcl_si_internal_symbols[%lu], ", sym_index);
	}
      printf("}, \n");
    }
  printf("};\n");
}

void print_si_internal_hashtable_vector_initializer(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry * internal_vector[internal_size] = {\n");
  for (i = 0; i < internal_size; i++)
    {
      struct mkcl_hashtable_entry * entry = internal_vector[i];

      if (entry)
	printf("&internal_entries[%lu], \n", si_internal_entry_index(entry));
      else
	printf("NULL, \n");

    }
  printf("};\n");
}

void print_si_external_hashtable_entry_initializers(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry external_entries[external_count] = {\n");
  for (i = 0; i < external_count; i++)
    {
      struct mkcl_hashtable_entry * entry = &external_entries[i];
      

      printf("{ ");
      if (entry->next)
	printf("&external_entries[%lu], ", si_external_entry_index(entry->next));
      else
	printf("NULL, ");
      if (mkcl_Null(entry->value))
	{
	  printf("mk_cl_Cnil, ");
	  printf("0, ");
	  printf("mk_cl_Cnil, ");
	}
      else
	{
	  mkcl_index sym_index = si_external_symbol_index((struct mkcl_symbol *) entry->value);

	  printf("(mkcl_object) &mkcl_si_external_symbol_names[%lu], ", sym_index);
	  printf("%luUL, ", entry->hashed_key);
	  printf("(mkcl_object) &mkcl_si_external_symbols[%lu], ", sym_index);
	}
      printf("}, \n");
    }
  printf("};\n");
}

void print_si_external_hashtable_vector_initializer(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry * external_vector[external_size] = {\n");
  for (i = 0; i < external_size; i++)
    {
      struct mkcl_hashtable_entry * entry = external_vector[i];

      if (entry)
	printf("&external_entries[%lu], \n", si_external_entry_index(entry));
      else
	printf("NULL, \n");

    }
  printf("};\n");
}

void print_copyright(void)
{
  printf("/* -*- mode: c  -*- */\n");
  printf("/*\n");
  printf("       Copyright (c) 2022, Jean-Claude Beaudoin.\n");
  printf("\n");
  printf("       This program is under GNU LGPL, you can redistribute it and/or\n");
  printf("       modify it under the terms of the GNU Lesser General Public\n");
  printf("       License as published by the Free Software Foundation; either\n");
  printf("       version 3 of the License, or (at your option) any later version.\n");
  printf("\n");
  printf("       See file '../../../Copyright' for full details.\n");
  printf("*/\n");
}

void print_si_package(void)
{
  print_copyright();
  print_si_internal_symbol_C_names();
  print_si_internal_symbol_initializers();
  print_si_external_symbol_C_names();
  print_si_external_symbol_initializers();
  print_si_internal_hashtable_entry_initializers();
  print_si_internal_hashtable_vector_initializer();
  print_si_external_hashtable_entry_initializers();
  print_si_external_hashtable_vector_initializer();
}

void expose_si_symbol(const struct exposed_symbol * exposed_symbol)
{
  bool internalp;
  struct mkcl_symbol * sym = find_symbol(&exposed_symbol->symbol_name, &mkcl_package_si, &internalp);

  if ( sym == NULL ) { printf("Exposed symbol not found: %s\n", exposed_symbol->symbol_name.self); exit(1); }

  printf("#define %s ", exposed_symbol->exposition);
  if (internalp)
    printf("&mkcl_si_internal_symbols[%lu]\n", si_internal_symbol_index(sym));
  else
    printf("&mkcl_si_external_symbols[%lu]\n", si_external_symbol_index(sym)); 
}

void expose_si_package(void)
{
  mkcl_index i;

  print_copyright();
  for (i = 0; i < MKCL_NB_ELEMS(exposed_symbols); i++)
    {
      expose_si_symbol(&exposed_symbols[i]);
    }
}

int main(int argc, char * argv[])
{
  char * program_name = basename(strdup(argv[0]));
  init_si_package();
  

  if ( strcmp(program_name, "build_package_SI") == 0 )
    print_si_package();
  else if ( strcmp(program_name, "expose_package_SI") == 0 )
    expose_si_package();

  return 0;
}

