/* -*- mode: c -*- */
/*
    main.c --
*/
/*
    Copyright (c) 2022, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

/******************************** GLOBALS *****************************/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>


#ifndef MKCL_MIN

struct mkcl_cfun mk_cl_adjust_array_cfunobj = MKCL_CFUN_VA(mk_cl_adjust_array, (mkcl_object) &MK_CL_adjust_array);
struct mkcl_cfun mk_cl_apropos_cfunobj = MKCL_CFUN_VA(mk_cl_apropos, (mkcl_object) &MK_CL_apropos);
struct mkcl_cfun mk_cl_apropos_list_cfunobj = MKCL_CFUN_VA(mk_cl_apropos_list, (mkcl_object) &MK_CL_apropos_list);
struct mkcl_cfun mk_cl_array_in_bounds_p_cfunobj = MKCL_CFUN_VA(mk_cl_array_in_bounds_p, (mkcl_object) &MK_CL_array_in_bounds_p);
struct mkcl_cfun mk_cl_assoc_if_cfunobj = MKCL_CFUN_VA(mk_cl_assoc_if, (mkcl_object) &MK_CL_assoc_if);
struct mkcl_cfun mk_cl_assoc_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_assoc_if_not, (mkcl_object) &MK_CL_assoc_if_not);
struct mkcl_cfun mk_cl_bit_cfunobj = MKCL_CFUN_VA(mk_cl_bit, (mkcl_object) &MK_CL_bit);
struct mkcl_cfun mk_cl_bit_and_cfunobj = MKCL_CFUN_VA(mk_cl_bit_and, (mkcl_object) &MK_CL_bit_and);
struct mkcl_cfun mk_cl_bit_andc1_cfunobj = MKCL_CFUN_VA(mk_cl_bit_andc1, (mkcl_object) &MK_CL_bit_andc1);
struct mkcl_cfun mk_cl_bit_andc2_cfunobj = MKCL_CFUN_VA(mk_cl_bit_andc2, (mkcl_object) &MK_CL_bit_andc2);
struct mkcl_cfun mk_cl_bit_eqv_cfunobj = MKCL_CFUN_VA(mk_cl_bit_eqv, (mkcl_object) &MK_CL_bit_eqv);
struct mkcl_cfun mk_cl_bit_ior_cfunobj = MKCL_CFUN_VA(mk_cl_bit_ior, (mkcl_object) &MK_CL_bit_ior);
struct mkcl_cfun mk_cl_bit_nand_cfunobj = MKCL_CFUN_VA(mk_cl_bit_nand, (mkcl_object) &MK_CL_bit_nand);
struct mkcl_cfun mk_cl_bit_nor_cfunobj = MKCL_CFUN_VA(mk_cl_bit_nor, (mkcl_object) &MK_CL_bit_nor);
struct mkcl_cfun mk_cl_bit_not_cfunobj = MKCL_CFUN_VA(mk_cl_bit_not, (mkcl_object) &MK_CL_bit_not);
struct mkcl_cfun mk_cl_bit_orc1_cfunobj = MKCL_CFUN_VA(mk_cl_bit_orc1, (mkcl_object) &MK_CL_bit_orc1);
struct mkcl_cfun mk_cl_bit_orc2_cfunobj = MKCL_CFUN_VA(mk_cl_bit_orc2, (mkcl_object) &MK_CL_bit_orc2);
struct mkcl_cfun mk_cl_bit_xor_cfunobj = MKCL_CFUN_VA(mk_cl_bit_xor, (mkcl_object) &MK_CL_bit_xor);
struct mkcl_cfun mk_cl_cerror_cfunobj = MKCL_CFUN_VA(mk_cl_cerror, (mkcl_object) &MK_CL_cerror);
struct mkcl_cfun mk_cl_concatenate_cfunobj = MKCL_CFUN_VA(mk_cl_concatenate, (mkcl_object) &MK_CL_concatenate);
struct mkcl_cfun mk_cl_continue_cfunobj = MKCL_CFUN_VA(mk_cl_continue, (mkcl_object) &MK_CL_continue);
struct mkcl_cfun mk_cl_copy_pprint_dispatch_cfunobj = MKCL_CFUN_VA(mk_cl_copy_pprint_dispatch, (mkcl_object) &MK_CL_copy_pprint_dispatch);
struct mkcl_cfun mk_cl_count_cfunobj = MKCL_CFUN_VA(mk_cl_count, (mkcl_object) &MK_CL_count);
struct mkcl_cfun mk_cl_count_if_cfunobj = MKCL_CFUN_VA(mk_cl_count_if, (mkcl_object) &MK_CL_count_if);
struct mkcl_cfun mk_cl_count_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_count_if_not, (mkcl_object) &MK_CL_count_if_not);
struct mkcl_cfun mk_cl_decode_universal_time_cfunobj = MKCL_CFUN_VA(mk_cl_decode_universal_time, (mkcl_object) &MK_CL_decode_universal_time);
struct mkcl_cfun mk_cl_delete_cfunobj = MKCL_CFUN_VA(mk_cl_delete, (mkcl_object) &MK_CL_delete);
struct mkcl_cfun mk_cl_delete_duplicates_cfunobj = MKCL_CFUN_VA(mk_cl_delete_duplicates, (mkcl_object) &MK_CL_delete_duplicates);
struct mkcl_cfun mk_cl_delete_if_cfunobj = MKCL_CFUN_VA(mk_cl_delete_if, (mkcl_object) &MK_CL_delete_if);
struct mkcl_cfun mk_cl_delete_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_delete_if_not, (mkcl_object) &MK_CL_delete_if_not);
struct mkcl_cfun mk_cl_encode_universal_time_cfunobj = MKCL_CFUN_VA(mk_cl_encode_universal_time, (mkcl_object) &MK_CL_encode_universal_time);
struct mkcl_cfun mk_cl_ensure_directories_exist_cfunobj = MKCL_CFUN_VA(mk_cl_ensure_directories_exist, (mkcl_object) &MK_CL_ensure_directories_exist);
struct mkcl_cfun mk_cl_every_cfunobj = MKCL_CFUN_VA(mk_cl_every, (mkcl_object) &MK_CL_every);
struct mkcl_cfun mk_cl_fceiling_cfunobj = MKCL_CFUN_VA(mk_cl_fceiling, (mkcl_object) &MK_CL_fceiling);
struct mkcl_cfun mk_cl_ffloor_cfunobj = MKCL_CFUN_VA(mk_cl_ffloor, (mkcl_object) &MK_CL_ffloor);
struct mkcl_cfun mk_cl_fill_cfunobj = MKCL_CFUN_VA(mk_cl_fill, (mkcl_object) &MK_CL_fill);
struct mkcl_cfun mk_cl_find_cfunobj = MKCL_CFUN_VA(mk_cl_find, (mkcl_object) &MK_CL_find);
struct mkcl_cfun mk_cl_find_if_cfunobj = MKCL_CFUN_VA(mk_cl_find_if, (mkcl_object) &MK_CL_find_if);
struct mkcl_cfun mk_cl_find_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_find_if_not, (mkcl_object) &MK_CL_find_if_not);
struct mkcl_cfun mk_cl_fround_cfunobj = MKCL_CFUN_VA(mk_cl_fround, (mkcl_object) &MK_CL_fround);
struct mkcl_cfun mk_cl_ftruncate_cfunobj = MKCL_CFUN_VA(mk_cl_ftruncate, (mkcl_object) &MK_CL_ftruncate);
struct mkcl_cfun mk_cl_intersection_cfunobj = MKCL_CFUN_VA(mk_cl_intersection, (mkcl_object) &MK_CL_intersection);
struct mkcl_cfun mk_cl_make_array_cfunobj = MKCL_CFUN_VA(mk_cl_make_array, (mkcl_object) &MK_CL_make_array);
struct mkcl_cfun mk_cl_make_sequence_cfunobj = MKCL_CFUN_VA(mk_cl_make_sequence, (mkcl_object) &MK_CL_make_sequence);
struct mkcl_cfun mk_cl_map_cfunobj = MKCL_CFUN_VA(mk_cl_map, (mkcl_object) &MK_CL_map);
struct mkcl_cfun mk_cl_map_into_cfunobj = MKCL_CFUN_VA(mk_cl_map_into, (mkcl_object) &MK_CL_map_into);
struct mkcl_cfun mk_cl_member_if_cfunobj = MKCL_CFUN_VA(mk_cl_member_if, (mkcl_object) &MK_CL_member_if);
struct mkcl_cfun mk_cl_member_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_member_if_not, (mkcl_object) &MK_CL_member_if_not);
struct mkcl_cfun mk_cl_merge_cfunobj = MKCL_CFUN_VA(mk_cl_merge, (mkcl_object) &MK_CL_merge);
struct mkcl_cfun mk_cl_mismatch_cfunobj = MKCL_CFUN_VA(mk_cl_mismatch, (mkcl_object) &MK_CL_mismatch);
struct mkcl_cfun mk_cl_nintersection_cfunobj = MKCL_CFUN_VA(mk_cl_nintersection, (mkcl_object) &MK_CL_nintersection);
struct mkcl_cfun mk_cl_notany_cfunobj = MKCL_CFUN_VA(mk_cl_notany, (mkcl_object) &MK_CL_notany);
struct mkcl_cfun mk_cl_notevery_cfunobj = MKCL_CFUN_VA(mk_cl_notevery, (mkcl_object) &MK_CL_notevery);
struct mkcl_cfun mk_cl_nset_difference_cfunobj = MKCL_CFUN_VA(mk_cl_nset_difference, (mkcl_object) &MK_CL_nset_difference);
struct mkcl_cfun mk_cl_nset_exclusive_or_cfunobj = MKCL_CFUN_VA(mk_cl_nset_exclusive_or, (mkcl_object) &MK_CL_nset_exclusive_or);
struct mkcl_cfun mk_cl_nsubst_if_cfunobj = MKCL_CFUN_VA(mk_cl_nsubst_if, (mkcl_object) &MK_CL_nsubst_if);
struct mkcl_cfun mk_cl_nsubst_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_nsubst_if_not, (mkcl_object) &MK_CL_nsubst_if_not);
struct mkcl_cfun mk_cl_nsubstitute_cfunobj = MKCL_CFUN_VA(mk_cl_nsubstitute, (mkcl_object) &MK_CL_nsubstitute);
struct mkcl_cfun mk_cl_nsubstitute_if_cfunobj = MKCL_CFUN_VA(mk_cl_nsubstitute_if, (mkcl_object) &MK_CL_nsubstitute_if);
struct mkcl_cfun mk_cl_nsubstitute_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_nsubstitute_if_not, (mkcl_object) &MK_CL_nsubstitute_if_not);
struct mkcl_cfun mk_cl_nunion_cfunobj = MKCL_CFUN_VA(mk_cl_nunion, (mkcl_object) &MK_CL_nunion);
struct mkcl_cfun mk_cl_position_cfunobj = MKCL_CFUN_VA(mk_cl_position, (mkcl_object) &MK_CL_position);
struct mkcl_cfun mk_cl_position_if_cfunobj = MKCL_CFUN_VA(mk_cl_position_if, (mkcl_object) &MK_CL_position_if);
struct mkcl_cfun mk_cl_position_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_position_if_not, (mkcl_object) &MK_CL_position_if_not);
struct mkcl_cfun mk_cl_pprint_dispatch_cfunobj = MKCL_CFUN_VA(mk_cl_pprint_dispatch, (mkcl_object) &MK_CL_pprint_dispatch);
struct mkcl_cfun mk_cl_pprint_fill_cfunobj = MKCL_CFUN_VA(mk_cl_pprint_fill, (mkcl_object) &MK_CL_pprint_fill);
struct mkcl_cfun mk_cl_pprint_indent_cfunobj = MKCL_CFUN_VA(mk_cl_pprint_indent, (mkcl_object) &MK_CL_pprint_indent);
struct mkcl_cfun mk_cl_pprint_linear_cfunobj = MKCL_CFUN_VA(mk_cl_pprint_linear, (mkcl_object) &MK_CL_pprint_linear);
struct mkcl_cfun mk_cl_pprint_newline_cfunobj = MKCL_CFUN_VA(mk_cl_pprint_newline, (mkcl_object) &MK_CL_pprint_newline);
struct mkcl_cfun mk_cl_pprint_tab_cfunobj = MKCL_CFUN_VA(mk_cl_pprint_tab, (mkcl_object) &MK_CL_pprint_tab);
struct mkcl_cfun mk_cl_pprint_tabular_cfunobj = MKCL_CFUN_VA(mk_cl_pprint_tabular, (mkcl_object) &MK_CL_pprint_tabular);
struct mkcl_cfun mk_cl_rassoc_if_cfunobj = MKCL_CFUN_VA(mk_cl_rassoc_if, (mkcl_object) &MK_CL_rassoc_if);
struct mkcl_cfun mk_cl_rassoc_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_rassoc_if_not, (mkcl_object) &MK_CL_rassoc_if_not);
struct mkcl_cfun mk_cl_read_from_string_cfunobj = MKCL_CFUN_VA(mk_cl_read_from_string, (mkcl_object) &MK_CL_read_from_string);
struct mkcl_cfun mk_cl_reduce_cfunobj = MKCL_CFUN_VA(mk_cl_reduce, (mkcl_object) &MK_CL_reduce);
struct mkcl_cfun mk_cl_remove_cfunobj = MKCL_CFUN_VA(mk_cl_remove, (mkcl_object) &MK_CL_remove);
struct mkcl_cfun mk_cl_remove_duplicates_cfunobj = MKCL_CFUN_VA(mk_cl_remove_duplicates, (mkcl_object) &MK_CL_remove_duplicates);
struct mkcl_cfun mk_cl_remove_if_cfunobj = MKCL_CFUN_VA(mk_cl_remove_if, (mkcl_object) &MK_CL_remove_if);
struct mkcl_cfun mk_cl_remove_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_remove_if_not, (mkcl_object) &MK_CL_remove_if_not);
struct mkcl_cfun mk_cl_replace_cfunobj = MKCL_CFUN_VA(mk_cl_replace, (mkcl_object) &MK_CL_replace);
struct mkcl_cfun mk_cl_require_cfunobj = MKCL_CFUN_VA(mk_cl_require, (mkcl_object) &MK_CL_require);
struct mkcl_cfun mk_cl_sbit_cfunobj = MKCL_CFUN_VA(mk_cl_sbit, (mkcl_object) &MK_CL_sbit);
struct mkcl_cfun mk_cl_search_cfunobj = MKCL_CFUN_VA(mk_cl_search, (mkcl_object) &MK_CL_search);
struct mkcl_cfun mk_cl_set_difference_cfunobj = MKCL_CFUN_VA(mk_cl_set_difference, (mkcl_object) &MK_CL_set_difference);
struct mkcl_cfun mk_cl_set_exclusive_or_cfunobj = MKCL_CFUN_VA(mk_cl_set_exclusive_or, (mkcl_object) &MK_CL_set_exclusive_or);
struct mkcl_cfun mk_cl_set_pprint_dispatch_cfunobj = MKCL_CFUN_VA(mk_cl_set_pprint_dispatch, (mkcl_object) &MK_CL_set_pprint_dispatch);
struct mkcl_cfun mk_cl_signum_cfunobj = MKCL_CFUN1(mk_cl_signum, (mkcl_object) &MK_CL_signum);
struct mkcl_cfun mk_cl_some_cfunobj = MKCL_CFUN_VA(mk_cl_some, (mkcl_object) &MK_CL_some);
struct mkcl_cfun mk_cl_sort_cfunobj = MKCL_CFUN_VA(mk_cl_sort, (mkcl_object) &MK_CL_sort);
struct mkcl_cfun mk_cl_stable_sort_cfunobj = MKCL_CFUN_VA(mk_cl_stable_sort, (mkcl_object) &MK_CL_stable_sort);
struct mkcl_cfun mk_cl_subsetp_cfunobj = MKCL_CFUN_VA(mk_cl_subsetp, (mkcl_object) &MK_CL_subsetp);
struct mkcl_cfun mk_cl_subst_if_cfunobj = MKCL_CFUN_VA(mk_cl_subst_if, (mkcl_object) &MK_CL_subst_if);
struct mkcl_cfun mk_cl_subst_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_subst_if_not, (mkcl_object) &MK_CL_subst_if_not);
struct mkcl_cfun mk_cl_substitute_cfunobj = MKCL_CFUN_VA(mk_cl_substitute, (mkcl_object) &MK_CL_substitute);
struct mkcl_cfun mk_cl_substitute_if_cfunobj = MKCL_CFUN_VA(mk_cl_substitute_if, (mkcl_object) &MK_CL_substitute_if);
struct mkcl_cfun mk_cl_substitute_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_substitute_if_not, (mkcl_object) &MK_CL_substitute_if_not);
struct mkcl_cfun mk_cl_subtypep_cfunobj = MKCL_CFUN_VA(mk_cl_subtypep, (mkcl_object) &MK_CL_subtypep);
struct mkcl_cfun mk_cl_typep_cfunobj = MKCL_CFUN_VA(mk_cl_typep, (mkcl_object) &MK_CL_typep);
struct mkcl_cfun mk_cl_union_cfunobj = MKCL_CFUN_VA(mk_cl_union, (mkcl_object) &MK_CL_union);
struct mkcl_cfun mk_cl_upgraded_array_element_type_cfunobj = MKCL_CFUN_VA(mk_cl_upgraded_array_element_type, (mkcl_object) &MK_CL_upgraded_array_element_type);
struct mkcl_cfun mk_cl_upgraded_complex_part_type_cfunobj = MKCL_CFUN_VA(mk_cl_upgraded_complex_part_type, (mkcl_object) &MK_CL_upgraded_complex_part_type);
struct mkcl_cfun mk_cl_vector_cfunobj = MKCL_CFUN_VA(mk_cl_vector, (mkcl_object) &MK_CL_vector);
struct mkcl_cfun mk_cl_vector_push_extend_cfunobj = MKCL_CFUN_VA(mk_cl_vector_push_extend, (mkcl_object) &MK_CL_vector_push_extend);
struct mkcl_cfun mk_cl_write_to_string_cfunobj = MKCL_CFUN_VA(mk_cl_write_to_string, (mkcl_object) &MK_CL_write_to_string);
struct mkcl_cfun mk_cl_y_or_n_p_cfunobj = MKCL_CFUN_VA(mk_cl_y_or_n_p, (mkcl_object) &MK_CL_y_or_n_p);
struct mkcl_cfun mk_cl_yes_or_no_p_cfunobj = MKCL_CFUN_VA(mk_cl_yes_or_no_p, (mkcl_object) &MK_CL_yes_or_no_p);
struct mkcl_cfun mk_cl_invalid_method_error_cfunobj = MKCL_CFUN_VA(mk_cl_invalid_method_error, (mkcl_object) &MK_CL_invalid_method_error);
struct mkcl_cfun mk_cl_method_combination_error_cfunobj = MKCL_CFUN_VA(mk_cl_method_combination_error, (mkcl_object) &MK_CL_method_combination_error);
struct mkcl_cfun mk_si_package_children_cfunobj = MKCL_CFUN_VA(mk_si_package_children, (mkcl_object) &MK_SI_package_children);
struct mkcl_cfun mk_mkcl_prin1_to_base_string_cfunobj = MKCL_CFUN_VA(mk_mkcl_prin1_to_base_string, (mkcl_object) &MK_MKCL_prin1_to_base_string);
struct mkcl_cfun mk_mkcl_princ_to_base_string_cfunobj = MKCL_CFUN_VA(mk_mkcl_princ_to_base_string, (mkcl_object) &MK_MKCL_princ_to_base_string);
struct mkcl_cfun mk_mkcl_write_to_base_string_cfunobj = MKCL_CFUN_VA(mk_mkcl_write_to_base_string, (mkcl_object) &MK_MKCL_write_to_base_string);
struct mkcl_cfun mk_mkcl_run_program_cfunobj = MKCL_CFUN_VA(mk_mkcl_run_program, (mkcl_object) &MK_MKCL_run_program);

struct mkcl_cfun mk_cl_acos_cfunobj = MKCL_CFUN1(mk_cl_acos, (mkcl_object) &MK_CL_acos);
struct mkcl_cfun mk_cl_acosh_cfunobj = MKCL_CFUN1(mk_cl_acosh, (mkcl_object) &MK_CL_acosh);
struct mkcl_cfun mk_cl_array_dimensions_cfunobj = MKCL_CFUN1(mk_cl_array_dimensions, (mkcl_object) &MK_CL_array_dimensions);
struct mkcl_cfun mk_cl_asin_cfunobj = MKCL_CFUN1(mk_cl_asin, (mkcl_object) &MK_CL_asin);
struct mkcl_cfun mk_cl_asinh_cfunobj = MKCL_CFUN1(mk_cl_asinh, (mkcl_object) &MK_CL_asinh);
struct mkcl_cfun mk_cl_atanh_cfunobj = MKCL_CFUN1(mk_cl_atanh, (mkcl_object) &MK_CL_atanh);
struct mkcl_cfun mk_cl_byte_cfunobj = MKCL_CFUN2(mk_cl_byte, (mkcl_object) &MK_CL_byte);
struct mkcl_cfun mk_cl_byte_position_cfunobj = MKCL_CFUN1(mk_cl_byte_position, (mkcl_object) &MK_CL_byte_position);
struct mkcl_cfun mk_cl_byte_size_cfunobj = MKCL_CFUN1(mk_cl_byte_size, (mkcl_object) &MK_CL_byte_size);
struct mkcl_cfun mk_cl_cis_cfunobj = MKCL_CFUN1(mk_cl_cis, (mkcl_object) &MK_CL_cis);
struct mkcl_cfun mk_cl_coerce_cfunobj = MKCL_CFUN2(mk_cl_coerce, (mkcl_object) &MK_CL_coerce);
struct mkcl_cfun mk_cl_complement_cfunobj = MKCL_CFUN1(mk_cl_complement, (mkcl_object) &MK_CL_complement);
struct mkcl_cfun mk_cl_constantly_cfunobj = MKCL_CFUN1(mk_cl_constantly, (mkcl_object) &MK_CL_constantly);
struct mkcl_cfun mk_cl_deposit_field_cfunobj = MKCL_CFUN3(mk_cl_deposit_field, (mkcl_object) &MK_CL_deposit_field);
struct mkcl_cfun mk_cl_dpb_cfunobj = MKCL_CFUN3(mk_cl_dpb, (mkcl_object) &MK_CL_dpb);
struct mkcl_cfun mk_cl_find_all_symbols_cfunobj = MKCL_CFUN1(mk_cl_find_all_symbols, (mkcl_object) &MK_CL_find_all_symbols);
struct mkcl_cfun mk_cl_get_decoded_time_cfunobj = MKCL_CFUN0(mk_cl_get_decoded_time, (mkcl_object) &MK_CL_get_decoded_time);
struct mkcl_cfun mk_cl_isqrt_cfunobj = MKCL_CFUN1(mk_cl_isqrt, (mkcl_object) &MK_CL_isqrt);
struct mkcl_cfun mk_cl_ldb_cfunobj = MKCL_CFUN2(mk_cl_ldb, (mkcl_object) &MK_CL_ldb);
struct mkcl_cfun mk_cl_ldb_test_cfunobj = MKCL_CFUN2(mk_cl_ldb_test, (mkcl_object) &MK_CL_ldb_test);
struct mkcl_cfun mk_cl_load_logical_pathname_translations_cfunobj = MKCL_CFUN1(mk_cl_load_logical_pathname_translations, (mkcl_object) &MK_CL_load_logical_pathname_translations);
struct mkcl_cfun mk_cl_logical_pathname_translations_cfunobj = MKCL_CFUN1(mk_cl_logical_pathname_translations, (mkcl_object) &MK_CL_logical_pathname_translations);
struct mkcl_cfun mk_cl_logtest_cfunobj = MKCL_CFUN2(mk_cl_logtest, (mkcl_object) &MK_CL_logtest);
struct mkcl_cfun mk_cl_mask_field_cfunobj = MKCL_CFUN2(mk_cl_mask_field, (mkcl_object) &MK_CL_mask_field);
struct mkcl_cfun mk_cl_phase_cfunobj = MKCL_CFUN1(mk_cl_phase, (mkcl_object) &MK_CL_phase);
struct mkcl_cfun mk_cl_prin1_to_string_cfunobj = MKCL_CFUN1(mk_cl_prin1_to_string, (mkcl_object) &MK_CL_prin1_to_string);
struct mkcl_cfun mk_cl_princ_to_string_cfunobj = MKCL_CFUN1(mk_cl_princ_to_string, (mkcl_object) &MK_CL_princ_to_string);
struct mkcl_cfun mk_cl_provide_cfunobj = MKCL_CFUN1(mk_cl_provide, (mkcl_object) &MK_CL_provide);
struct mkcl_cfun mk_cl_vector_pop_cfunobj = MKCL_CFUN1(mk_cl_vector_pop, (mkcl_object) &MK_CL_vector_pop);
struct mkcl_cfun mk_cl_vector_push_cfunobj = MKCL_CFUN2(mk_cl_vector_push, (mkcl_object) &MK_CL_vector_push);
struct mkcl_cfun mk_si_safe_eval_cfunobj = MKCL_CFUN3(mk_si_safe_eval, (mkcl_object) &MK_SI_safe_eval);
struct mkcl_cfun mk_si_subclassp_cfunobj = MKCL_CFUN2(mk_si_subclassp, (mkcl_object) &MK_SI_subclassp);
struct mkcl_cfun mk_si_of_class_p_cfunobj = MKCL_CFUN2(mk_si_of_class_p, (mkcl_object) &MK_SI_of_class_p);
struct mkcl_cfun mk_si_do_deftype_cfunobj = MKCL_CFUN3(mk_si_do_deftype, (mkcl_object) &MK_SI_do_deftype);
struct mkcl_cfun mk_si_find_relative_package_cfunobj = MKCL_CFUN1(mk_si_find_relative_package, (mkcl_object) &MK_SI_find_relative_package);
struct mkcl_cfun mk_si_package_parent_cfunobj = MKCL_CFUN1(mk_si_package_parent, (mkcl_object) &MK_SI_package_parent);
struct mkcl_cfun mk_si_top_apply_cfunobj = MKCL_CFUN2(mk_si_top_apply, (mkcl_object) &MK_SI_top_apply);
struct mkcl_cfun mk_si_mkcl_version_cfunobj = MKCL_CFUN0(mk_si_mkcl_version, (mkcl_object) &MK_SI_mkcl_version);
struct mkcl_cfun mk_si_mkcl_major_version_cfunobj = MKCL_CFUN0(mk_si_mkcl_major_version, (mkcl_object) &MK_SI_mkcl_major_version);
struct mkcl_cfun mk_si_mkcl_minor_version_cfunobj = MKCL_CFUN0(mk_si_mkcl_minor_version, (mkcl_object) &MK_SI_mkcl_minor_version);
struct mkcl_cfun mk_si_mkcl_patch_level_cfunobj = MKCL_CFUN0(mk_si_mkcl_patch_level, (mkcl_object) &MK_SI_mkcl_patch_level);
struct mkcl_cfun mk_si_shutdown_mkcl_threads_cfunobj = MKCL_CFUN4(mk_si_shutdown_mkcl_threads, (mkcl_object) &MK_SI_shutdown_mkcl_threads);

#endif /* def MKCL_FINAL */



void mkcl_init_all_packages(MKCL)
{

  /*
   * 1) Initialize symbols and packages
   */
  mkcl_core.packages = mk_cl_Cnil;
  mkcl_core.packages_to_be_created = mk_cl_Cnil;

  {
    static struct mkcl_base_string pack_nick1_obj = MKCL_BASE_STRING_INIT("CL");
    static struct mkcl_base_string pack_nick2_obj = MKCL_BASE_STRING_INIT("LISP");
    static struct mkcl_cons nicknames[] = {
      MKCL_CONS_INIT(&pack_nick1_obj, &nicknames[1]),
      MKCL_CONS_INIT(&pack_nick2_obj, mk_cl_Cnil),

    };

    mkcl_core.lisp_package = mkcl_setup_package_cl();
    mkcl_core.lisp_package->pack.nicknames = (mkcl_object) nicknames;
    mkcl_core.packages = MKCL_CONS(env, mkcl_core.lisp_package, mkcl_core.packages);
  }
  
  {
    static struct mkcl_base_string pack_nick1_obj = MKCL_BASE_STRING_INIT("MKCL-EXTENSIONS");
    static struct mkcl_base_string pack_nick2_obj = MKCL_BASE_STRING_INIT("MK-EXT");
    static struct mkcl_cons nicknames[] = {
      MKCL_CONS_INIT(&pack_nick1_obj, &nicknames[1]),
      MKCL_CONS_INIT(&pack_nick2_obj, mk_cl_Cnil),
    };
    static struct mkcl_cons use_list[] = {
      MKCL_CONS_INIT(mk_cl_Cnil, mk_cl_Cnil),
    };

    use_list[0].car = mkcl_core.lisp_package;

    mkcl_core.mkcl_ext_package = mkcl_setup_package_mkcl();
    mkcl_core.mkcl_ext_package->pack.uses = (mkcl_object) use_list;
    mkcl_core.mkcl_ext_package->pack.nicknames = (mkcl_object) nicknames;
    mkcl_core.packages = MKCL_CONS(env, mkcl_core.mkcl_ext_package, mkcl_core.packages);
  }

  {
    static struct mkcl_base_string pack_nick1_obj = MKCL_BASE_STRING_INIT("CL-USER");
    static struct mkcl_base_string pack_nick2_obj = MKCL_BASE_STRING_INIT("USER");
    static struct mkcl_cons nicknames[] = {
      MKCL_CONS_INIT(&pack_nick1_obj, &nicknames[1]),
      MKCL_CONS_INIT(&pack_nick2_obj, mk_cl_Cnil),
    };
    static struct mkcl_cons use_list[] = {
      MKCL_CONS_INIT(mk_cl_Cnil, &use_list[1]),
      MKCL_CONS_INIT(mk_cl_Cnil, mk_cl_Cnil),
    };

    use_list[0].car = mkcl_core.lisp_package;
    use_list[1].car = mkcl_core.mkcl_ext_package;
    mkcl_core.user_package = mkcl_setup_package_user();
    mkcl_core.user_package->pack.uses = (mkcl_object) use_list;
    mkcl_core.user_package->pack.nicknames = (mkcl_object) nicknames;
    mkcl_core.packages = MKCL_CONS(env, mkcl_core.user_package, mkcl_core.packages);
  }
  
  {
    mkcl_core.keyword_package = mkcl_setup_package_keyword();
    mkcl_core.packages = MKCL_CONS(env, mkcl_core.keyword_package, mkcl_core.packages);
  }
    
  {
    static struct mkcl_base_string pack_nick1_obj = MKCL_BASE_STRING_INIT("SYSTEM");
    static struct mkcl_base_string pack_nick2_obj = MKCL_BASE_STRING_INIT("SYS");
    static struct mkcl_cons nicknames[] = {
      MKCL_CONS_INIT(&pack_nick1_obj, &nicknames[1]),
      MKCL_CONS_INIT(&pack_nick2_obj, mk_cl_Cnil),
    };
    static struct mkcl_cons use_list[] = {
      MKCL_CONS_INIT(mk_cl_Cnil, &use_list[1]),
      MKCL_CONS_INIT(mk_cl_Cnil, mk_cl_Cnil),
    };

    use_list[0].car = mkcl_core.lisp_package;
    use_list[1].car = mkcl_core.mkcl_ext_package;
    mkcl_core.system_package = mkcl_setup_package_si();
    mkcl_core.system_package->pack.uses = (mkcl_object) use_list;
    mkcl_core.system_package->pack.nicknames = (mkcl_object) nicknames;
    mkcl_core.packages = MKCL_CONS(env, mkcl_core.system_package, mkcl_core.packages);
  }
  
  {
    static struct mkcl_cons use_list[] = {
      MKCL_CONS_INIT(mk_cl_Cnil, mk_cl_Cnil),
    };

    use_list[0].car = mkcl_core.lisp_package;
    mkcl_core.clos_package = mkcl_setup_package_clos();
    mkcl_core.clos_package->pack.uses = (mkcl_object) use_list;
    mkcl_core.packages = MKCL_CONS(env, mkcl_core.clos_package, mkcl_core.packages);
  }
  
  {
    static struct mkcl_base_string pack_nick1_obj = MKCL_BASE_STRING_INIT("UFFI");
    static struct mkcl_cons nicknames[] = {
      MKCL_CONS_INIT(&pack_nick1_obj, mk_cl_Cnil),
    };
    static struct mkcl_cons use_list[] = {
      MKCL_CONS_INIT(mk_cl_Cnil, mk_cl_Cnil),
    };

    use_list[0].car = mkcl_core.lisp_package;
    mkcl_core.ffi_package = mkcl_setup_package_ffi();
    mkcl_core.ffi_package->pack.uses = (mkcl_object) use_list;
    mkcl_core.ffi_package->pack.nicknames = (mkcl_object) nicknames;
    mkcl_core.packages = MKCL_CONS(env, mkcl_core.ffi_package, mkcl_core.packages);
  }
  
  {
    static struct mkcl_base_string pack_nick1_obj = MKCL_BASE_STRING_INIT("MULTI-THREADING");
    static struct mkcl_base_string pack_nick2_obj = MKCL_BASE_STRING_INIT("MP");
    static struct mkcl_base_string pack_nick3_obj = MKCL_BASE_STRING_INIT("MULTIPROCESSING");
    static struct mkcl_cons nicknames[] = {
      MKCL_CONS_INIT(&pack_nick1_obj, &nicknames[1]),
      MKCL_CONS_INIT(&pack_nick2_obj, &nicknames[2]),
      MKCL_CONS_INIT(&pack_nick3_obj, mk_cl_Cnil),
    };
    static struct mkcl_cons use_list[] = {
      MKCL_CONS_INIT(mk_cl_Cnil, mk_cl_Cnil),
    };

    use_list[0].car = mkcl_core.lisp_package;
    mkcl_core.mt_package = mkcl_setup_package_mt();
    mkcl_core.mt_package->pack.uses = (mkcl_object) use_list;
    mkcl_core.mt_package->pack.nicknames = (mkcl_object) nicknames;
    mkcl_core.packages = MKCL_CONS(env, mkcl_core.mt_package, mkcl_core.packages);
  }
  
  {
    static struct mkcl_cons use_list[] = {
      MKCL_CONS_INIT(mk_cl_Cnil, mk_cl_Cnil),
    };

    use_list[0].car = mkcl_core.lisp_package;
    mkcl_core.gray_package = mkcl_setup_package_gray();
    mkcl_core.gray_package->pack.uses = (mkcl_object) use_list;
    mkcl_core.packages = MKCL_CONS(env, mkcl_core.gray_package, mkcl_core.packages);
  }

  /* Patch used-by lists on relevant packages. */
  {
      static struct mkcl_cons used_by_list[] = {
      MKCL_CONS_INIT(mk_cl_Cnil, &used_by_list[1]),
      MKCL_CONS_INIT(mk_cl_Cnil, mk_cl_Cnil),
    };

    used_by_list[0].car = mkcl_core.system_package;
    used_by_list[1].car = mkcl_core.user_package;
    mkcl_core.mkcl_ext_package->pack.usedby = (mkcl_object) used_by_list;
  }

  {
    static struct mkcl_cons used_by_list[] = {
      MKCL_CONS_INIT(mk_cl_Cnil, &used_by_list[1]),
      MKCL_CONS_INIT(mk_cl_Cnil, &used_by_list[2]),
      MKCL_CONS_INIT(mk_cl_Cnil, &used_by_list[3]),
      MKCL_CONS_INIT(mk_cl_Cnil, &used_by_list[4]),
      MKCL_CONS_INIT(mk_cl_Cnil, &used_by_list[5]),
      MKCL_CONS_INIT(mk_cl_Cnil, &used_by_list[6]),
      MKCL_CONS_INIT(mk_cl_Cnil, mk_cl_Cnil),
    };

    used_by_list[0].car = mkcl_core.clos_package;
    used_by_list[1].car = mkcl_core.ffi_package;
    used_by_list[2].car = mkcl_core.gray_package;
    used_by_list[3].car = mkcl_core.mt_package;
    used_by_list[4].car = mkcl_core.system_package;
    used_by_list[5].car = mkcl_core.user_package;
    used_by_list[6].car = mkcl_core.mkcl_ext_package;
    mkcl_core.lisp_package->pack.usedby = (mkcl_object) used_by_list;
  }

}

