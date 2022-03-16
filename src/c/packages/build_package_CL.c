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


#include "CL_package.h"


struct mkcl_function_declaration {
  struct mkcl_base_string symbol_name;
  struct mkcl_base_string function_object_denotator;
};

struct mkcl_function_declaration mkcl_cl_declare_lisp_functions[] = {
  /*  {SYMBOL_NAME(""), FUN_DENOT("&_cfunobj")}, */
  {SYMBOL_NAME("+"), FUN_DENOT("&mk_cl_P_cfunobj")},
  {SYMBOL_NAME("-"), FUN_DENOT("&mk_cl_M_cfunobj")},
  {SYMBOL_NAME("*"), FUN_DENOT("&mk_cl_X_cfunobj")},
  {SYMBOL_NAME("/"), FUN_DENOT("&mk_cl_N_cfunobj")},
  {SYMBOL_NAME("/="), FUN_DENOT("&mk_cl_NE_cfunobj")},
  {SYMBOL_NAME("1+"), FUN_DENOT("&mk_cl_1P_cfunobj")},
  {SYMBOL_NAME("1-"), FUN_DENOT("&mk_cl_1M_cfunobj")},
  {SYMBOL_NAME("<"), FUN_DENOT("&mk_cl_L_cfunobj")},
  {SYMBOL_NAME("<="), FUN_DENOT("&mk_cl_LE_cfunobj")},
  {SYMBOL_NAME("="), FUN_DENOT("&mk_cl_E_cfunobj")},
  {SYMBOL_NAME(">"), FUN_DENOT("&mk_cl_G_cfunobj")},
  {SYMBOL_NAME(">="), FUN_DENOT("&mk_cl_GE_cfunobj")},
  {SYMBOL_NAME("ABS"), FUN_DENOT("&mk_cl_abs_cfunobj")},
  {SYMBOL_NAME("ACONS"), FUN_DENOT("&mk_cl_acons_cfunobj")},
  {SYMBOL_NAME("ACOS"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_acos_cfunobj)")},
  {SYMBOL_NAME("ACOSH"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_acosh_cfunobj)")},
  {SYMBOL_NAME("ADJOIN"), FUN_DENOT("&mk_cl_adjoin_cfunobj")},
  {SYMBOL_NAME("ADJUST-ARRAY"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_adjust_array_cfunobj)")},
  {SYMBOL_NAME("ADJUSTABLE-ARRAY-P"), FUN_DENOT("&mk_cl_adjustable_array_p_cfunobj")},
  {SYMBOL_NAME("ALPHA-CHAR-P"), FUN_DENOT("&mk_cl_alpha_char_p_cfunobj")},
  {SYMBOL_NAME("ALPHANUMERICP"), FUN_DENOT("&mk_cl_alphanumericp_cfunobj")},
  {SYMBOL_NAME("APPEND"), FUN_DENOT("&mk_cl_append_cfunobj")},
  {SYMBOL_NAME("APPLY"), FUN_DENOT("&mk_cl_apply_cfunobj")},
  {SYMBOL_NAME("APROPOS"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_apropos_cfunobj)")},
  {SYMBOL_NAME("APROPOS-LIST"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_apropos_list_cfunobj)")},
  {SYMBOL_NAME("AREF"), FUN_DENOT("&mk_cl_aref_cfunobj")},
  {SYMBOL_NAME("ARRAY-DIMENSION"), FUN_DENOT("&mk_cl_array_dimension_cfunobj")},
  {SYMBOL_NAME("ARRAY-DIMENSIONS"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_array_dimensions_cfunobj)")},
  {SYMBOL_NAME("ARRAY-DISPLACEMENT"), FUN_DENOT("&mk_cl_array_displacement_cfunobj")},
  {SYMBOL_NAME("ARRAY-ELEMENT-TYPE"), FUN_DENOT("&mk_cl_array_element_type_cfunobj")},
  {SYMBOL_NAME("ARRAY-HAS-FILL-POINTER-P"), FUN_DENOT("&mk_cl_array_has_fill_pointer_p_cfunobj")},
  {SYMBOL_NAME("ARRAY-IN-BOUNDS-P"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_array_in_bounds_p_cfunobj)")},
  {SYMBOL_NAME("ARRAY-RANK"), FUN_DENOT("&mk_cl_array_rank_cfunobj")},
  {SYMBOL_NAME("ARRAY-ROW-MAJOR-INDEX"), FUN_DENOT("&mk_cl_array_row_major_index_cfunobj")},
  {SYMBOL_NAME("ARRAY-TOTAL-SIZE"), FUN_DENOT("&mk_cl_array_total_size_cfunobj")},
  {SYMBOL_NAME("ARRAYP"), FUN_DENOT("&mk_cl_arrayp_cfunobj")},
  {SYMBOL_NAME("ASH"), FUN_DENOT("&mk_cl_ash_cfunobj")},
  {SYMBOL_NAME("ASIN"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_asin_cfunobj)")},
  {SYMBOL_NAME("ASINH"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_asinh_cfunobj)")},
  {SYMBOL_NAME("ASSOC"), FUN_DENOT("&mk_cl_assoc_cfunobj")},
  {SYMBOL_NAME("ASSOC-IF"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_assoc_if_cfunobj)")},
  {SYMBOL_NAME("ASSOC-IF-NOT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_assoc_if_not_cfunobj)")},
  {SYMBOL_NAME("ATAN"), FUN_DENOT("&mk_cl_atan_cfunobj")},
  {SYMBOL_NAME("ATANH"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_atanh_cfunobj)")},
  {SYMBOL_NAME("ATOM"), FUN_DENOT("&mk_cl_atom_cfunobj")},
  {SYMBOL_NAME("BIT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_bit_cfunobj)")},
  {SYMBOL_NAME("BIT-AND"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_bit_and_cfunobj)")},
  {SYMBOL_NAME("BIT-ANDC1"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_bit_andc1_cfunobj)")},
  {SYMBOL_NAME("BIT-ANDC2"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_bit_andc2_cfunobj)")},
  {SYMBOL_NAME("BIT-EQV"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_bit_eqv_cfunobj)")},
  {SYMBOL_NAME("BIT-IOR"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_bit_ior_cfunobj)")},
  {SYMBOL_NAME("BIT-NAND"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_bit_nand_cfunobj)")},
  {SYMBOL_NAME("BIT-NOR"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_bit_nor_cfunobj)")},
  {SYMBOL_NAME("BIT-NOT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_bit_not_cfunobj)")},
  {SYMBOL_NAME("BIT-ORC1"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_bit_orc1_cfunobj)")},
  {SYMBOL_NAME("BIT-ORC2"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_bit_orc2_cfunobj)")},
  {SYMBOL_NAME("BIT-VECTOR-P"), FUN_DENOT("&mk_cl_bit_vector_p_cfunobj")},
  {SYMBOL_NAME("BIT-XOR"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_bit_xor_cfunobj)")},
  {SYMBOL_NAME("BOOLE"), FUN_DENOT("&mk_cl_boole_cfunobj")},
  {SYMBOL_NAME("BOTH-CASE-P"), FUN_DENOT("&mk_cl_both_case_p_cfunobj")},
  {SYMBOL_NAME("BOUNDP"), FUN_DENOT("&mk_cl_boundp_cfunobj")},
  {SYMBOL_NAME("BROADCAST-STREAM-STREAMS"), FUN_DENOT("&mk_cl_broadcast_stream_streams_cfunobj")},
  {SYMBOL_NAME("BUTLAST"), FUN_DENOT("&mk_cl_butlast_cfunobj")},
  {SYMBOL_NAME("BYTE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_byte_cfunobj)")},
  {SYMBOL_NAME("BYTE-POSITION"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_byte_position_cfunobj)")},
  {SYMBOL_NAME("BYTE-SIZE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_byte_size_cfunobj)")},
  {SYMBOL_NAME("CAAAAR"), FUN_DENOT("&mk_cl_caaaar_cfunobj")},
  {SYMBOL_NAME("CAAADR"), FUN_DENOT("&mk_cl_caaadr_cfunobj")},
  {SYMBOL_NAME("CAAAR"), FUN_DENOT("&mk_cl_caaar_cfunobj")},
  {SYMBOL_NAME("CAADAR"), FUN_DENOT("&mk_cl_caadar_cfunobj")},
  {SYMBOL_NAME("CAADDR"), FUN_DENOT("&mk_cl_caaddr_cfunobj")},
  {SYMBOL_NAME("CAADR"), FUN_DENOT("&mk_cl_caadr_cfunobj")},
  {SYMBOL_NAME("CAAR"), FUN_DENOT("&mk_cl_caar_cfunobj")},
  {SYMBOL_NAME("CADAAR"), FUN_DENOT("&mk_cl_cadaar_cfunobj")},
  {SYMBOL_NAME("CADADR"), FUN_DENOT("&mk_cl_cadadr_cfunobj")},
  {SYMBOL_NAME("CADAR"), FUN_DENOT("&mk_cl_cadar_cfunobj")},
  {SYMBOL_NAME("CADDAR"), FUN_DENOT("&mk_cl_caddar_cfunobj")},
  {SYMBOL_NAME("CADDDR"), FUN_DENOT("&mk_cl_cadddr_cfunobj")},
  {SYMBOL_NAME("CADDR"), FUN_DENOT("&mk_cl_caddr_cfunobj")},
  {SYMBOL_NAME("CADR"), FUN_DENOT("&mk_cl_cadr_cfunobj")},
  {SYMBOL_NAME("CAR"), FUN_DENOT("&mk_cl_car_cfunobj")},
  {SYMBOL_NAME("CDAAAR"), FUN_DENOT("&mk_cl_cdaaar_cfunobj")},
  {SYMBOL_NAME("CDAADR"), FUN_DENOT("&mk_cl_cdaadr_cfunobj")},
  {SYMBOL_NAME("CDAAR"), FUN_DENOT("&mk_cl_cdaar_cfunobj")},
  {SYMBOL_NAME("CDADAR"), FUN_DENOT("&mk_cl_cdadar_cfunobj")},
  {SYMBOL_NAME("CDADDR"), FUN_DENOT("&mk_cl_cdaddr_cfunobj")},
  {SYMBOL_NAME("CDADR"), FUN_DENOT("&mk_cl_cdadr_cfunobj")},
  {SYMBOL_NAME("CDAR"), FUN_DENOT("&mk_cl_cdar_cfunobj")},
  {SYMBOL_NAME("CDDAAR"), FUN_DENOT("&mk_cl_cddaar_cfunobj")},
  {SYMBOL_NAME("CDDADR"), FUN_DENOT("&mk_cl_cddadr_cfunobj")},
  {SYMBOL_NAME("CDDAR"), FUN_DENOT("&mk_cl_cddar_cfunobj")},
  {SYMBOL_NAME("CDDDAR"), FUN_DENOT("&mk_cl_cdddar_cfunobj")},
  {SYMBOL_NAME("CDDDDR"), FUN_DENOT("&mk_cl_cddddr_cfunobj")},
  {SYMBOL_NAME("CDDDR"), FUN_DENOT("&mk_cl_cdddr_cfunobj")},
  {SYMBOL_NAME("CDDR"), FUN_DENOT("&mk_cl_cddr_cfunobj")},
  {SYMBOL_NAME("CDR"), FUN_DENOT("&mk_cl_cdr_cfunobj")},
  {SYMBOL_NAME("CEILING"), FUN_DENOT("&mk_cl_ceiling_cfunobj")},
  {SYMBOL_NAME("CERROR"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_cerror_cfunobj)")},
  {SYMBOL_NAME("CHAR"), FUN_DENOT("&mk_cl_char_cfunobj")},
  {SYMBOL_NAME("CHAR-CODE"), FUN_DENOT("&mk_cl_char_code_cfunobj")},
  {SYMBOL_NAME("CHAR-DOWNCASE"), FUN_DENOT("&mk_cl_char_downcase_cfunobj")},
  {SYMBOL_NAME("CHAR-EQUAL"), FUN_DENOT("&mk_cl_char_equal_cfunobj")},
  {SYMBOL_NAME("CHAR-GREATERP"), FUN_DENOT("&mk_cl_char_greaterp_cfunobj")},
  {SYMBOL_NAME("CHAR-INT"), FUN_DENOT("&mk_cl_char_int_cfunobj")},
  {SYMBOL_NAME("CHAR-LESSP"), FUN_DENOT("&mk_cl_char_lessp_cfunobj")},
  {SYMBOL_NAME("CHAR-NAME"), FUN_DENOT("&mk_cl_char_name_cfunobj")},
  {SYMBOL_NAME("CHAR-NOT-EQUAL"), FUN_DENOT("&mk_cl_char_not_equal_cfunobj")},
  {SYMBOL_NAME("CHAR-NOT-GREATERP"), FUN_DENOT("&mk_cl_char_not_greaterp_cfunobj")},
  {SYMBOL_NAME("CHAR-NOT-LESSP"), FUN_DENOT("&mk_cl_char_not_lessp_cfunobj")},
  {SYMBOL_NAME("CHAR-UPCASE"), FUN_DENOT("&mk_cl_char_upcase_cfunobj")},
  {SYMBOL_NAME("CHAR/="), FUN_DENOT("&mk_cl_charNE_cfunobj")},
  {SYMBOL_NAME("CHAR<"), FUN_DENOT("&mk_cl_charL_cfunobj")},
  {SYMBOL_NAME("CHAR<="), FUN_DENOT("&mk_cl_charLE_cfunobj")},
  {SYMBOL_NAME("CHAR="), FUN_DENOT("&mk_cl_charE_cfunobj")},
  {SYMBOL_NAME("CHAR>"), FUN_DENOT("&mk_cl_charG_cfunobj")},
  {SYMBOL_NAME("CHAR>="), FUN_DENOT("&mk_cl_charGE_cfunobj")},
  {SYMBOL_NAME("CHARACTER"), FUN_DENOT("&mk_cl_character_cfunobj")},
  {SYMBOL_NAME("CHARACTERP"), FUN_DENOT("&mk_cl_characterp_cfunobj")},
  {SYMBOL_NAME("CIS"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_cis_cfunobj)")},
  {SYMBOL_NAME("CLEAR-INPUT"), FUN_DENOT("&mk_cl_clear_input_cfunobj")},
  {SYMBOL_NAME("CLEAR-OUTPUT"), FUN_DENOT("&mk_cl_clear_output_cfunobj")},
  {SYMBOL_NAME("CLOSE"), FUN_DENOT("&mk_cl_close_cfunobj")},
  {SYMBOL_NAME("CLRHASH"), FUN_DENOT("&mk_cl_clrhash_cfunobj")},
  {SYMBOL_NAME("CODE-CHAR"), FUN_DENOT("&mk_cl_code_char_cfunobj")},
  {SYMBOL_NAME("COERCE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_coerce_cfunobj)")},
  {SYMBOL_NAME("COMPILED-FUNCTION-P"), FUN_DENOT("&mk_cl_compiled_function_p_cfunobj")},
  {SYMBOL_NAME("COMPLEMENT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_complement_cfunobj)")},
  {SYMBOL_NAME("COMPLEX"), FUN_DENOT("&mk_cl_complex_cfunobj")},
  {SYMBOL_NAME("COMPLEXP"), FUN_DENOT("&mk_cl_complexp_cfunobj")},
  {SYMBOL_NAME("CONCATENATE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_concatenate_cfunobj)")},
  {SYMBOL_NAME("CONCATENATED-STREAM-STREAMS"), FUN_DENOT("&mk_cl_concatenated_stream_streams_cfunobj")},
  {SYMBOL_NAME("CONJUGATE"), FUN_DENOT("&mk_cl_conjugate_cfunobj")},
  {SYMBOL_NAME("CONTINUE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_continue_cfunobj)")},
  {SYMBOL_NAME("CONS"), FUN_DENOT("&mk_cl_cons_cfunobj")},
  {SYMBOL_NAME("CONSP"), FUN_DENOT("&mk_cl_consp_cfunobj")},
  {SYMBOL_NAME("CONSTANTLY"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_constantly_cfunobj)")},
  {SYMBOL_NAME("CONSTANTP"), FUN_DENOT("&mk_cl_constantp_cfunobj")},
  {SYMBOL_NAME("COPY-ALIST"), FUN_DENOT("&mk_cl_copy_alist_cfunobj")},
  {SYMBOL_NAME("COPY-LIST"), FUN_DENOT("&mk_cl_copy_list_cfunobj")},
  {SYMBOL_NAME("COPY-PPRINT-DISPATCH"), FUN_DENOT("MKCL_NAME_PPRINT(&mk_cl_copy_pprint_dispatch_cfunobj)")},
  {SYMBOL_NAME("COPY-READTABLE"), FUN_DENOT("&mk_cl_copy_readtable_cfunobj")},
  {SYMBOL_NAME("COPY-SEQ"), FUN_DENOT("&mk_cl_copy_seq_cfunobj")},
  {SYMBOL_NAME("COPY-STRUCTURE"), FUN_DENOT("&mk_cl_copy_structure_cfunobj")},
  {SYMBOL_NAME("COPY-SYMBOL"), FUN_DENOT("&mk_cl_copy_symbol_cfunobj")},
  {SYMBOL_NAME("COPY-TREE"), FUN_DENOT("&mk_cl_copy_tree_cfunobj")},
  {SYMBOL_NAME("COS"), FUN_DENOT("&mk_cl_cos_cfunobj")},
  {SYMBOL_NAME("COSH"), FUN_DENOT("&mk_cl_cosh_cfunobj")},
  {SYMBOL_NAME("COUNT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_count_cfunobj)")},
  {SYMBOL_NAME("COUNT-IF"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_count_if_cfunobj)")},
  {SYMBOL_NAME("COUNT-IF-NOT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_count_if_not_cfunobj)")},
  {SYMBOL_NAME("DECODE-FLOAT"), FUN_DENOT("&mk_cl_decode_float_cfunobj")},
  {SYMBOL_NAME("DECODE-UNIVERSAL-TIME"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_decode_universal_time_cfunobj)")},
  {SYMBOL_NAME("DELETE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_delete_cfunobj)")},
  {SYMBOL_NAME("DELETE-DUPLICATES"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_delete_duplicates_cfunobj)")},
  {SYMBOL_NAME("DELETE-FILE"), FUN_DENOT("&mk_cl_delete_file_cfunobj")},
  {SYMBOL_NAME("DELETE-IF"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_delete_if_cfunobj)")},
  {SYMBOL_NAME("DELETE-IF-NOT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_delete_if_not_cfunobj)")},
  {SYMBOL_NAME("DELETE-PACKAGE"), FUN_DENOT("&mk_cl_delete_package_cfunobj")},
  {SYMBOL_NAME("DENOMINATOR"), FUN_DENOT("&mk_cl_denominator_cfunobj")},
  {SYMBOL_NAME("DEPOSIT-FIELD"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_deposit_field_cfunobj)")},
  {SYMBOL_NAME("DIGIT-CHAR"), FUN_DENOT("&mk_cl_digit_char_cfunobj")},
  {SYMBOL_NAME("DIGIT-CHAR-P"), FUN_DENOT("&mk_cl_digit_char_p_cfunobj")},
  {SYMBOL_NAME("DIRECTORY"), FUN_DENOT("&mk_cl_directory_cfunobj")},
  {SYMBOL_NAME("DIRECTORY-NAMESTRING"), FUN_DENOT("&mk_cl_directory_namestring_cfunobj")},
  {SYMBOL_NAME("DPB"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_dpb_cfunobj)")},
  {SYMBOL_NAME("ECHO-STREAM-INPUT-STREAM"), FUN_DENOT("&mk_cl_echo_stream_input_stream_cfunobj")},
  {SYMBOL_NAME("ECHO-STREAM-OUTPUT-STREAM"), FUN_DENOT("&mk_cl_echo_stream_output_stream_cfunobj")},
  {SYMBOL_NAME("EIGHTH"), FUN_DENOT("&mk_cl_eighth_cfunobj")},
  {SYMBOL_NAME("ELT"), FUN_DENOT("&mk_cl_elt_cfunobj")},
  {SYMBOL_NAME("ENCODE-UNIVERSAL-TIME"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_encode_universal_time_cfunobj)")},
  {SYMBOL_NAME("ENDP"), FUN_DENOT("&mk_cl_endp_cfunobj")},
  {SYMBOL_NAME("ENOUGH-NAMESTRING"), FUN_DENOT("&mk_cl_enough_namestring_cfunobj")},
  {SYMBOL_NAME("ENSURE-DIRECTORIES-EXIST"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_ensure_directories_exist_cfunobj)")},
  {SYMBOL_NAME("EQ"), FUN_DENOT("&mk_cl_eq_cfunobj")},
  {SYMBOL_NAME("EQL"), FUN_DENOT("&mk_cl_eql_cfunobj")},
  {SYMBOL_NAME("EQUAL"), FUN_DENOT("&mk_cl_equal_cfunobj")},
  {SYMBOL_NAME("EQUALP"), FUN_DENOT("&mk_cl_equalp_cfunobj")},
  {SYMBOL_NAME("ERROR"), FUN_DENOT("&mk_cl_error_cfunobj")},
  {SYMBOL_NAME("EVAL"), FUN_DENOT("&mk_cl_eval_cfunobj")},
  {SYMBOL_NAME("EVENP"), FUN_DENOT("&mk_cl_evenp_cfunobj")},
  {SYMBOL_NAME("EVERY"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_every_cfunobj)")},
  {SYMBOL_NAME("EXP"), FUN_DENOT("&mk_cl_exp_cfunobj")},
  {SYMBOL_NAME("EXPORT"), FUN_DENOT("&mk_cl_export_cfunobj")},
  {SYMBOL_NAME("EXPT"), FUN_DENOT("&mk_cl_expt_cfunobj")},
  {SYMBOL_NAME("FBOUNDP"), FUN_DENOT("&mk_cl_fboundp_cfunobj")},
  {SYMBOL_NAME("FCEILING"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_fceiling_cfunobj)")},
  {SYMBOL_NAME("FDEFINITION"), FUN_DENOT("&mk_cl_fdefinition_cfunobj")},
  {SYMBOL_NAME("FFLOOR"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_ffloor_cfunobj)")},
  {SYMBOL_NAME("FIFTH"), FUN_DENOT("&mk_cl_fifth_cfunobj")},
  {SYMBOL_NAME("FILE-AUTHOR"), FUN_DENOT("&mk_cl_file_author_cfunobj")},
  {SYMBOL_NAME("FILE-LENGTH"), FUN_DENOT("&mk_cl_file_length_cfunobj")},
  {SYMBOL_NAME("FILE-NAMESTRING"), FUN_DENOT("&mk_cl_file_namestring_cfunobj")},
  {SYMBOL_NAME("FILE-POSITION"), FUN_DENOT("&mk_cl_file_position_cfunobj")},
  {SYMBOL_NAME("FILE-STRING-LENGTH"), FUN_DENOT("&mk_cl_file_string_length_cfunobj")},
  {SYMBOL_NAME("FILE-WRITE-DATE"), FUN_DENOT("&mk_cl_file_write_date_cfunobj")},
  {SYMBOL_NAME("FILL"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_fill_cfunobj)")},
  {SYMBOL_NAME("FILL-POINTER"), FUN_DENOT("&mk_cl_fill_pointer_cfunobj")},
  {SYMBOL_NAME("FIND"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_find_cfunobj)")},
  {SYMBOL_NAME("FIND-ALL-SYMBOLS"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_find_all_symbols_cfunobj)")},
  {SYMBOL_NAME("FIND-IF"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_find_if_cfunobj)")},
  {SYMBOL_NAME("FIND-IF-NOT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_find_if_not_cfunobj)")},
  {SYMBOL_NAME("FIND-PACKAGE"), FUN_DENOT("&mk_cl_find_package_cfunobj")},
  {SYMBOL_NAME("FIND-SYMBOL"), FUN_DENOT("&mk_cl_find_symbol_cfunobj")},
  {SYMBOL_NAME("FINISH-OUTPUT"), FUN_DENOT("&mk_cl_finish_output_cfunobj")},
  {SYMBOL_NAME("FIRST"), FUN_DENOT("&mk_cl_first_cfunobj")},
  {SYMBOL_NAME("FLOAT"), FUN_DENOT("&mk_cl_float_cfunobj")},
  {SYMBOL_NAME("FLOAT-DIGITS"), FUN_DENOT("&mk_cl_float_digits_cfunobj")},
  {SYMBOL_NAME("FLOAT-PRECISION"), FUN_DENOT("&mk_cl_float_precision_cfunobj")},
  {SYMBOL_NAME("FLOAT-RADIX"), FUN_DENOT("&mk_cl_float_radix_cfunobj")},
  {SYMBOL_NAME("FLOAT-SIGN"), FUN_DENOT("&mk_cl_float_sign_cfunobj")},
  {SYMBOL_NAME("FLOATP"), FUN_DENOT("&mk_cl_floatp_cfunobj")},
  {SYMBOL_NAME("FLOOR"), FUN_DENOT("&mk_cl_floor_cfunobj")},
  {SYMBOL_NAME("FMAKUNBOUND"), FUN_DENOT("&mk_cl_fmakunbound_cfunobj")},
  {SYMBOL_NAME("FORCE-OUTPUT"), FUN_DENOT("&mk_cl_force_output_cfunobj")},
  {SYMBOL_NAME("FORMAT"), FUN_DENOT("&mk_cl_format_cfunobj")},
  {SYMBOL_NAME("FOURTH"), FUN_DENOT("&mk_cl_fourth_cfunobj")},
  {SYMBOL_NAME("FRESH-LINE"), FUN_DENOT("&mk_cl_fresh_line_cfunobj")},
  {SYMBOL_NAME("FROUND"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_fround_cfunobj)")},
  {SYMBOL_NAME("FTRUNCATE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_ftruncate_cfunobj)")},
  {SYMBOL_NAME("FUNCALL"), FUN_DENOT("&mk_cl_funcall_cfunobj")},
  {SYMBOL_NAME("FUNCTION-LAMBDA-EXPRESSION"), FUN_DENOT("&mk_cl_function_lambda_expression_cfunobj")},
  {SYMBOL_NAME("FUNCTIONP"), FUN_DENOT("&mk_cl_functionp_cfunobj")},
  {SYMBOL_NAME("GCD"), FUN_DENOT("&mk_cl_gcd_cfunobj")},
  {SYMBOL_NAME("GENSYM"), FUN_DENOT("&mk_cl_gensym_cfunobj")},
  {SYMBOL_NAME("GENTEMP"), FUN_DENOT("&mk_cl_gentemp_cfunobj")},
  {SYMBOL_NAME("GET"), FUN_DENOT("&mk_cl_get_cfunobj")},
  {SYMBOL_NAME("GET-DECODED-TIME"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_get_decoded_time_cfunobj)")},
  {SYMBOL_NAME("GET-DISPATCH-MACRO-CHARACTER"), FUN_DENOT("&mk_cl_get_dispatch_macro_character_cfunobj")},
  {SYMBOL_NAME("GET-INTERNAL-REAL-TIME"), FUN_DENOT("&mk_cl_get_internal_real_time_cfunobj")},
  {SYMBOL_NAME("GET-INTERNAL-RUN-TIME"), FUN_DENOT("&mk_cl_get_internal_run_time_cfunobj")},
  {SYMBOL_NAME("GET-MACRO-CHARACTER"), FUN_DENOT("&mk_cl_get_macro_character_cfunobj")},
  {SYMBOL_NAME("GET-OUTPUT-STREAM-STRING"), FUN_DENOT("&mk_cl_get_output_stream_string_cfunobj")},
  {SYMBOL_NAME("GET-PROPERTIES"), FUN_DENOT("&mk_cl_get_properties_cfunobj")},
  {SYMBOL_NAME("GET-UNIVERSAL-TIME"), FUN_DENOT("&mk_cl_get_universal_time_cfunobj")},
  {SYMBOL_NAME("GETF"), FUN_DENOT("&mk_cl_getf_cfunobj")},
  {SYMBOL_NAME("GETHASH"), FUN_DENOT("&mk_cl_gethash_cfunobj")},
  {SYMBOL_NAME("GRAPHIC-CHAR-P"), FUN_DENOT("&mk_cl_graphic_char_p_cfunobj")},
  {SYMBOL_NAME("HASH-TABLE-COUNT"), FUN_DENOT("&mk_cl_hash_table_count_cfunobj")},
  {SYMBOL_NAME("HASH-TABLE-P"), FUN_DENOT("&mk_cl_hash_table_p_cfunobj")},
  {SYMBOL_NAME("HASH-TABLE-REHASH-SIZE"), FUN_DENOT("&mk_cl_hash_table_rehash_size_cfunobj")},
  {SYMBOL_NAME("HASH-TABLE-REHASH-THRESHOLD"), FUN_DENOT("&mk_cl_hash_table_rehash_threshold_cfunobj")},
  {SYMBOL_NAME("HASH-TABLE-SIZE"), FUN_DENOT("&mk_cl_hash_table_size_cfunobj")},
  {SYMBOL_NAME("HASH-TABLE-TEST"), FUN_DENOT("&mk_cl_hash_table_test_cfunobj")},
  {SYMBOL_NAME("HOST-NAMESTRING"), FUN_DENOT("&mk_cl_host_namestring_cfunobj")},
  {SYMBOL_NAME("IDENTITY"), FUN_DENOT("&mk_cl_identity_cfunobj")},
  {SYMBOL_NAME("IMAGPART"), FUN_DENOT("&mk_cl_imagpart_cfunobj")},
  {SYMBOL_NAME("IMPORT"), FUN_DENOT("&mk_cl_import_cfunobj")},
  {SYMBOL_NAME("INPUT-STREAM-P"), FUN_DENOT("&mk_cl_input_stream_p_cfunobj")},
  {SYMBOL_NAME("INTEGER-DECODE-FLOAT"), FUN_DENOT("&mk_cl_integer_decode_float_cfunobj")},
  {SYMBOL_NAME("INTEGER-LENGTH"), FUN_DENOT("&mk_cl_integer_length_cfunobj")},
  {SYMBOL_NAME("INTEGERP"), FUN_DENOT("&mk_cl_integerp_cfunobj")},
  {SYMBOL_NAME("INTERACTIVE-STREAM-P"), FUN_DENOT("&mk_cl_interactive_stream_p_cfunobj")},
  {SYMBOL_NAME("INTERN"), FUN_DENOT("&mk_cl_intern_cfunobj")},
  {SYMBOL_NAME("INTERSECTION"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_intersection_cfunobj)")},
  {SYMBOL_NAME("ISQRT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_isqrt_cfunobj)")},
  {SYMBOL_NAME("KEYWORDP"), FUN_DENOT("&mk_cl_keywordp_cfunobj")},
  {SYMBOL_NAME("LAST"), FUN_DENOT("&mk_cl_last_cfunobj")},
  {SYMBOL_NAME("LCM"), FUN_DENOT("&mk_cl_lcm_cfunobj")},
  {SYMBOL_NAME("LDB"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_ldb_cfunobj)")},
  {SYMBOL_NAME("LDB-TEST"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_ldb_test_cfunobj)")},
  {SYMBOL_NAME("LDIFF"), FUN_DENOT("&mk_cl_ldiff_cfunobj")},
  {SYMBOL_NAME("LENGTH"), FUN_DENOT("&mk_cl_length_cfunobj")},
  {SYMBOL_NAME("LIST"), FUN_DENOT("&mk_cl_list_cfunobj")},
  {SYMBOL_NAME("LIST*"), FUN_DENOT("&mk_cl_listX_cfunobj")},
  {SYMBOL_NAME("LIST-ALL-PACKAGES"), FUN_DENOT("&mk_cl_list_all_packages_cfunobj")},
  {SYMBOL_NAME("LIST-LENGTH"), FUN_DENOT("&mk_cl_list_length_cfunobj")},
  {SYMBOL_NAME("LISTEN"), FUN_DENOT("&mk_cl_listen_cfunobj")},
  {SYMBOL_NAME("LISTP"), FUN_DENOT("&mk_cl_listp_cfunobj")},
  {SYMBOL_NAME("LOAD"), FUN_DENOT("&mk_cl_load_cfunobj")},
  {SYMBOL_NAME("LOAD-LOGICAL-PATHNAME-TRANSLATIONS"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_load_logical_pathname_translations_cfunobj)")},
  {SYMBOL_NAME("LOG"), FUN_DENOT("&mk_cl_log_cfunobj")},
  {SYMBOL_NAME("LOGAND"), FUN_DENOT("&mk_cl_logand_cfunobj")},
  {SYMBOL_NAME("LOGANDC1"), FUN_DENOT("&mk_cl_logandc1_cfunobj")},
  {SYMBOL_NAME("LOGANDC2"), FUN_DENOT("&mk_cl_logandc2_cfunobj")},
  {SYMBOL_NAME("LOGBITP"), FUN_DENOT("&mk_cl_logbitp_cfunobj")},
  {SYMBOL_NAME("LOGCOUNT"), FUN_DENOT("&mk_cl_logcount_cfunobj")},
  {SYMBOL_NAME("LOGEQV"), FUN_DENOT("&mk_cl_logeqv_cfunobj")},
  {SYMBOL_NAME("LOGICAL-PATHNAME"), FUN_DENOT("&mk_cl_logical_pathname_cfunobj")},
  {SYMBOL_NAME("LOGICAL-PATHNAME-TRANSLATIONS"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_logical_pathname_translations_cfunobj)")},
  {SYMBOL_NAME("LOGIOR"), FUN_DENOT("&mk_cl_logior_cfunobj")},
  {SYMBOL_NAME("LOGNAND"), FUN_DENOT("&mk_cl_lognand_cfunobj")},
  {SYMBOL_NAME("LOGNOR"), FUN_DENOT("&mk_cl_lognor_cfunobj")},
  {SYMBOL_NAME("LOGNOT"), FUN_DENOT("&mk_cl_lognot_cfunobj")},
  {SYMBOL_NAME("LOGORC1"), FUN_DENOT("&mk_cl_logorc1_cfunobj")},
  {SYMBOL_NAME("LOGORC2"), FUN_DENOT("&mk_cl_logorc2_cfunobj")},
  {SYMBOL_NAME("LOGTEST"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_logtest_cfunobj)")},
  {SYMBOL_NAME("LOGXOR"), FUN_DENOT("&mk_cl_logxor_cfunobj")},
  {SYMBOL_NAME("LOWER-CASE-P"), FUN_DENOT("&mk_cl_lower_case_p_cfunobj")},
  {SYMBOL_NAME("MACRO-FUNCTION"), FUN_DENOT("&mk_cl_macro_function_cfunobj")},
  {SYMBOL_NAME("MACROEXPAND"), FUN_DENOT("&mk_cl_macroexpand_cfunobj")},
  {SYMBOL_NAME("MACROEXPAND-1"), FUN_DENOT("&mk_cl_macroexpand_1_cfunobj")},
  {SYMBOL_NAME("MAKE-ARRAY"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_make_array_cfunobj)")},
  {SYMBOL_NAME("MAKE-BROADCAST-STREAM"), FUN_DENOT("&mk_cl_make_broadcast_stream_cfunobj")},
  {SYMBOL_NAME("MAKE-CONCATENATED-STREAM"), FUN_DENOT("&mk_cl_make_concatenated_stream_cfunobj")},
  {SYMBOL_NAME("MAKE-DISPATCH-MACRO-CHARACTER"), FUN_DENOT("&mk_cl_make_dispatch_macro_character_cfunobj")},
  {SYMBOL_NAME("MAKE-ECHO-STREAM"), FUN_DENOT("&mk_cl_make_echo_stream_cfunobj")},
  {SYMBOL_NAME("MAKE-HASH-TABLE"), FUN_DENOT("&mk_cl_make_hash_table_cfunobj")},
  {SYMBOL_NAME("MAKE-LIST"), FUN_DENOT("&mk_cl_make_list_cfunobj")},
  {SYMBOL_NAME("MAKE-PACKAGE"), FUN_DENOT("&mk_cl_make_package_cfunobj")},
  {SYMBOL_NAME("MAKE-PATHNAME"), FUN_DENOT("&mk_cl_make_pathname_cfunobj")},
  {SYMBOL_NAME("MAKE-RANDOM-STATE"), FUN_DENOT("&mk_cl_make_random_state_cfunobj")},
  {SYMBOL_NAME("MAKE-SEQUENCE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_make_sequence_cfunobj)")},
  {SYMBOL_NAME("MAKE-STRING"), FUN_DENOT("&mk_cl_make_string_cfunobj")},
  {SYMBOL_NAME("MAKE-STRING-INPUT-STREAM"), FUN_DENOT("&mk_cl_make_string_input_stream_cfunobj")},
  {SYMBOL_NAME("MAKE-STRING-OUTPUT-STREAM"), FUN_DENOT("&mk_cl_make_string_output_stream_cfunobj")},
  {SYMBOL_NAME("MAKE-SYMBOL"), FUN_DENOT("&mk_cl_make_symbol_cfunobj")},
  {SYMBOL_NAME("MAKE-SYNONYM-STREAM"), FUN_DENOT("&mk_cl_make_synonym_stream_cfunobj")},
  {SYMBOL_NAME("MAKE-TWO-WAY-STREAM"), FUN_DENOT("&mk_cl_make_two_way_stream_cfunobj")},
  {SYMBOL_NAME("MAKUNBOUND"), FUN_DENOT("&mk_cl_makunbound_cfunobj")},
  {SYMBOL_NAME("MAP"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_map_cfunobj)")},
  {SYMBOL_NAME("MAP-INTO"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_map_into_cfunobj)")},
  {SYMBOL_NAME("MAPC"), FUN_DENOT("&mk_cl_mapc_cfunobj")},
  {SYMBOL_NAME("MAPCAN"), FUN_DENOT("&mk_cl_mapcan_cfunobj")},
  {SYMBOL_NAME("MAPCAR"), FUN_DENOT("&mk_cl_mapcar_cfunobj")},
  {SYMBOL_NAME("MAPCON"), FUN_DENOT("&mk_cl_mapcon_cfunobj")},
  {SYMBOL_NAME("MAPHASH"), FUN_DENOT("&mk_cl_maphash_cfunobj")},
  {SYMBOL_NAME("MAPL"), FUN_DENOT("&mk_cl_mapl_cfunobj")},
  {SYMBOL_NAME("MAPLIST"), FUN_DENOT("&mk_cl_maplist_cfunobj")},
  {SYMBOL_NAME("MASK-FIELD"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_mask_field_cfunobj)")},
  {SYMBOL_NAME("MAX"), FUN_DENOT("&mk_cl_max_cfunobj")},
  {SYMBOL_NAME("MEMBER"), FUN_DENOT("&mk_cl_member_cfunobj")},
  {SYMBOL_NAME("MEMBER-IF"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_member_if_cfunobj)")},
  {SYMBOL_NAME("MEMBER-IF-NOT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_member_if_not_cfunobj)")},
  {SYMBOL_NAME("MERGE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_merge_cfunobj)")},
  {SYMBOL_NAME("MERGE-PATHNAMES"), FUN_DENOT("&mk_cl_merge_pathnames_cfunobj")},
  {SYMBOL_NAME("MIN"), FUN_DENOT("&mk_cl_min_cfunobj")},
  {SYMBOL_NAME("MINUSP"), FUN_DENOT("&mk_cl_minusp_cfunobj")},
  {SYMBOL_NAME("MISMATCH"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_mismatch_cfunobj)")},
  {SYMBOL_NAME("MOD"), FUN_DENOT("&mk_cl_mod_cfunobj")},
  {SYMBOL_NAME("NAME-CHAR"), FUN_DENOT("&mk_cl_name_char_cfunobj")},
  {SYMBOL_NAME("NAMESTRING"), FUN_DENOT("&mk_cl_namestring_cfunobj")},
  {SYMBOL_NAME("NBUTLAST"), FUN_DENOT("&mk_cl_nbutlast_cfunobj")},
  {SYMBOL_NAME("NCONC"), FUN_DENOT("&mk_cl_nconc_cfunobj")},
  {SYMBOL_NAME("NINTERSECTION"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_nintersection_cfunobj)")},
  {SYMBOL_NAME("NINTH"), FUN_DENOT("&mk_cl_ninth_cfunobj")},
  {SYMBOL_NAME("NOT"), FUN_DENOT("&mk_cl_not_cfunobj")},
  {SYMBOL_NAME("NOTANY"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_notany_cfunobj)")},
  {SYMBOL_NAME("NOTEVERY"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_notevery_cfunobj)")},
  {SYMBOL_NAME("NRECONC"), FUN_DENOT("&mk_cl_nreconc_cfunobj")},
  {SYMBOL_NAME("NREVERSE"), FUN_DENOT("&mk_cl_nreverse_cfunobj")},
  {SYMBOL_NAME("NSET-DIFFERENCE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_nset_difference_cfunobj)")},
  {SYMBOL_NAME("NSET-EXCLUSIVE-OR"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_nset_exclusive_or_cfunobj)")},
  {SYMBOL_NAME("NSTRING-CAPITALIZE"), FUN_DENOT("&mk_cl_nstring_capitalize_cfunobj")},
  {SYMBOL_NAME("NSTRING-DOWNCASE"), FUN_DENOT("&mk_cl_nstring_downcase_cfunobj")},
  {SYMBOL_NAME("NSTRING-UPCASE"), FUN_DENOT("&mk_cl_nstring_upcase_cfunobj")},
  {SYMBOL_NAME("NSUBLIS"), FUN_DENOT("&mk_cl_nsublis_cfunobj")},
  {SYMBOL_NAME("NSUBST"), FUN_DENOT("&mk_cl_nsubst_cfunobj")},
  {SYMBOL_NAME("NSUBST-IF"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_nsubst_if_cfunobj)")},
  {SYMBOL_NAME("NSUBST-IF-NOT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_nsubst_if_not_cfunobj)")},
  {SYMBOL_NAME("NSUBSTITUTE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_nsubstitute_cfunobj)")},
  {SYMBOL_NAME("NSUBSTITUTE-IF"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_nsubstitute_if_cfunobj)")},
  {SYMBOL_NAME("NSUBSTITUTE-IF-NOT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_nsubstitute_if_not_cfunobj)")},
  {SYMBOL_NAME("NTH"), FUN_DENOT("&mk_cl_nth_cfunobj")},
  {SYMBOL_NAME("NTHCDR"), FUN_DENOT("&mk_cl_nthcdr_cfunobj")},
  {SYMBOL_NAME("NULL"), FUN_DENOT("&mk_cl_null_cfunobj")},
  {SYMBOL_NAME("NUMBERP"), FUN_DENOT("&mk_cl_numberp_cfunobj")},
  {SYMBOL_NAME("NUMERATOR"), FUN_DENOT("&mk_cl_numerator_cfunobj")},
  {SYMBOL_NAME("NUNION"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_nunion_cfunobj)")},
  {SYMBOL_NAME("ODDP"), FUN_DENOT("&mk_cl_oddp_cfunobj")},
  {SYMBOL_NAME("OPEN"), FUN_DENOT("&mk_cl_open_cfunobj")},
  {SYMBOL_NAME("OPEN-STREAM-P"), FUN_DENOT("&mk_cl_open_stream_p_cfunobj")},
  {SYMBOL_NAME("OUTPUT-STREAM-P"), FUN_DENOT("&mk_cl_output_stream_p_cfunobj")},
  {SYMBOL_NAME("PACKAGE-NAME"), FUN_DENOT("&mk_cl_package_name_cfunobj")},
  {SYMBOL_NAME("PACKAGE-NICKNAMES"), FUN_DENOT("&mk_cl_package_nicknames_cfunobj")},
  {SYMBOL_NAME("PACKAGE-SHADOWING-SYMBOLS"), FUN_DENOT("&mk_cl_package_shadowing_symbols_cfunobj")},
  {SYMBOL_NAME("PACKAGE-USE-LIST"), FUN_DENOT("&mk_cl_package_use_list_cfunobj")},
  {SYMBOL_NAME("PACKAGE-USED-BY-LIST"), FUN_DENOT("&mk_cl_package_used_by_list_cfunobj")},
  {SYMBOL_NAME("PACKAGEP"), FUN_DENOT("&mk_cl_packagep_cfunobj")},
  {SYMBOL_NAME("PAIRLIS"), FUN_DENOT("&mk_cl_pairlis_cfunobj")},
  {SYMBOL_NAME("PARSE-INTEGER"), FUN_DENOT("&mk_cl_parse_integer_cfunobj")},
  {SYMBOL_NAME("PARSE-NAMESTRING"), FUN_DENOT("&mk_cl_parse_namestring_cfunobj")},
  {SYMBOL_NAME("PATHNAME"), FUN_DENOT("&mk_cl_pathname_cfunobj")},
  {SYMBOL_NAME("PATHNAME-DEVICE"), FUN_DENOT("&mk_cl_pathname_device_cfunobj")},
  {SYMBOL_NAME("PATHNAME-DIRECTORY"), FUN_DENOT("&mk_cl_pathname_directory_cfunobj")},
  {SYMBOL_NAME("PATHNAME-HOST"), FUN_DENOT("&mk_cl_pathname_host_cfunobj")},
  {SYMBOL_NAME("PATHNAME-MATCH-P"), FUN_DENOT("&mk_cl_pathname_match_p_cfunobj")},
  {SYMBOL_NAME("PATHNAME-NAME"), FUN_DENOT("&mk_cl_pathname_name_cfunobj")},
  {SYMBOL_NAME("PATHNAME-TYPE"), FUN_DENOT("&mk_cl_pathname_type_cfunobj")},
  {SYMBOL_NAME("PATHNAME-VERSION"), FUN_DENOT("&mk_cl_pathname_version_cfunobj")},
  {SYMBOL_NAME("PATHNAMEP"), FUN_DENOT("&mk_cl_pathnamep_cfunobj")},
  {SYMBOL_NAME("PEEK-CHAR"), FUN_DENOT("&mk_cl_peek_char_cfunobj")},
  {SYMBOL_NAME("PHASE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_phase_cfunobj)")},
  {SYMBOL_NAME("PLUSP"), FUN_DENOT("&mk_cl_plusp_cfunobj")},
  {SYMBOL_NAME("POSITION"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_position_cfunobj)")},
  {SYMBOL_NAME("POSITION-IF"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_position_if_cfunobj)")},
  {SYMBOL_NAME("POSITION-IF-NOT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_position_if_not_cfunobj)")},
  {SYMBOL_NAME("PPRINT"), FUN_DENOT("&mk_cl_pprint_cfunobj")},
  {SYMBOL_NAME("PPRINT-DISPATCH"), FUN_DENOT("MKCL_NAME_PPRINT(&mk_cl_pprint_dispatch_cfunobj)")},
  {SYMBOL_NAME("PPRINT-FILL"), FUN_DENOT("MKCL_NAME_PPRINT(&mk_cl_pprint_fill_cfunobj)")},
  {SYMBOL_NAME("PPRINT-INDENT"), FUN_DENOT("MKCL_NAME_PPRINT(&mk_cl_pprint_indent_cfunobj)")},
  {SYMBOL_NAME("PPRINT-LINEAR"), FUN_DENOT("MKCL_NAME_PPRINT(&mk_cl_pprint_linear_cfunobj)")},
  {SYMBOL_NAME("PPRINT-NEWLINE"), FUN_DENOT("MKCL_NAME_PPRINT(&mk_cl_pprint_newline_cfunobj)")},
  {SYMBOL_NAME("PPRINT-TAB"), FUN_DENOT("MKCL_NAME_PPRINT(&mk_cl_pprint_tab_cfunobj)")},
  {SYMBOL_NAME("PPRINT-TABULAR"), FUN_DENOT("MKCL_NAME_PPRINT(&mk_cl_pprint_tabular_cfunobj)")},
  {SYMBOL_NAME("PRIN1"), FUN_DENOT("&mk_cl_prin1_cfunobj")},
  {SYMBOL_NAME("PRIN1-TO-STRING"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_prin1_to_string_cfunobj)")},
  {SYMBOL_NAME("PRINC"), FUN_DENOT("&mk_cl_princ_cfunobj")},
  {SYMBOL_NAME("PRINC-TO-STRING"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_princ_to_string_cfunobj)")},
  {SYMBOL_NAME("PRINT"), FUN_DENOT("&mk_cl_print_cfunobj")},
  {SYMBOL_NAME("PROBE-FILE"), FUN_DENOT("&mk_cl_probe_file_cfunobj")},
  {SYMBOL_NAME("PROVIDE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_provide_cfunobj)")},
  {SYMBOL_NAME("RANDOM"), FUN_DENOT("&mk_cl_random_cfunobj")},
  {SYMBOL_NAME("RANDOM-STATE-P"), FUN_DENOT("&mk_cl_random_state_p_cfunobj")},
  {SYMBOL_NAME("RASSOC"), FUN_DENOT("&mk_cl_rassoc_cfunobj")},
  {SYMBOL_NAME("RASSOC-IF"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_rassoc_if_cfunobj)")},
  {SYMBOL_NAME("RASSOC-IF-NOT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_rassoc_if_not_cfunobj)")},
  {SYMBOL_NAME("RATIONAL"), FUN_DENOT("&mk_cl_rational_cfunobj")},
  {SYMBOL_NAME("RATIONALIZE"), FUN_DENOT("&mk_cl_rationalize_cfunobj")},
  {SYMBOL_NAME("RATIONALP"), FUN_DENOT("&mk_cl_rationalp_cfunobj")},
  {SYMBOL_NAME("READ"), FUN_DENOT("&mk_cl_read_cfunobj")},
  {SYMBOL_NAME("READ-BYTE"), FUN_DENOT("&mk_cl_read_byte_cfunobj")},
  {SYMBOL_NAME("READ-CHAR"), FUN_DENOT("&mk_cl_read_char_cfunobj")},
  {SYMBOL_NAME("READ-CHAR-NO-HANG"), FUN_DENOT("&mk_cl_read_char_no_hang_cfunobj")},
  {SYMBOL_NAME("READ-DELIMITED-LIST"), FUN_DENOT("&mk_cl_read_delimited_list_cfunobj")},
  {SYMBOL_NAME("READ-FROM-STRING"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_read_from_string_cfunobj)")},
  {SYMBOL_NAME("READ-LINE"), FUN_DENOT("&mk_cl_read_line_cfunobj")},
  {SYMBOL_NAME("READ-PRESERVING-WHITESPACE"), FUN_DENOT("&mk_cl_read_preserving_whitespace_cfunobj")},
  {SYMBOL_NAME("READ-SEQUENCE"), FUN_DENOT("&mk_cl_read_sequence_cfunobj")},
  {SYMBOL_NAME("READTABLE-CASE"), FUN_DENOT("&mk_cl_readtable_case_cfunobj")},
  {SYMBOL_NAME("READTABLEP"), FUN_DENOT("&mk_cl_readtablep_cfunobj")},
  {SYMBOL_NAME("REALP"), FUN_DENOT("&mk_cl_realp_cfunobj")},
  {SYMBOL_NAME("REALPART"), FUN_DENOT("&mk_cl_realpart_cfunobj")},
  {SYMBOL_NAME("REDUCE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_reduce_cfunobj)")},
  {SYMBOL_NAME("REM"), FUN_DENOT("&mk_cl_rem_cfunobj")},
  {SYMBOL_NAME("REMHASH"), FUN_DENOT("&mk_cl_remhash_cfunobj")},
  {SYMBOL_NAME("REMOVE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_remove_cfunobj)")},
  {SYMBOL_NAME("REMOVE-DUPLICATES"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_remove_duplicates_cfunobj)")},
  {SYMBOL_NAME("REMOVE-IF"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_remove_if_cfunobj)")},
  {SYMBOL_NAME("REMOVE-IF-NOT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_remove_if_not_cfunobj)")},
  {SYMBOL_NAME("REMPROP"), FUN_DENOT("&mk_cl_remprop_cfunobj")},
  {SYMBOL_NAME("RENAME-FILE"), FUN_DENOT("&mk_cl_rename_file_cfunobj")},
  {SYMBOL_NAME("RENAME-PACKAGE"), FUN_DENOT("&mk_cl_rename_package_cfunobj")},
  {SYMBOL_NAME("REPLACE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_replace_cfunobj)")},
  {SYMBOL_NAME("REQUIRE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_require_cfunobj)")},
  {SYMBOL_NAME("REST"), FUN_DENOT("&mk_cl_rest_cfunobj")},
  {SYMBOL_NAME("REVAPPEND"), FUN_DENOT("&mk_cl_revappend_cfunobj")},
  {SYMBOL_NAME("REVERSE"), FUN_DENOT("&mk_cl_reverse_cfunobj")},
  {SYMBOL_NAME("ROUND"), FUN_DENOT("&mk_cl_round_cfunobj")},
  {SYMBOL_NAME("ROW-MAJOR-AREF"), FUN_DENOT("&mk_cl_row_major_aref_cfunobj")},
  {SYMBOL_NAME("RPLACA"), FUN_DENOT("&mk_cl_rplaca_cfunobj")},
  {SYMBOL_NAME("RPLACD"), FUN_DENOT("&mk_cl_rplacd_cfunobj")},
  {SYMBOL_NAME("SBIT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_sbit_cfunobj)")},
  {SYMBOL_NAME("SCALE-FLOAT"), FUN_DENOT("&mk_cl_scale_float_cfunobj")},
  {SYMBOL_NAME("SCHAR"), FUN_DENOT("&mk_cl_schar_cfunobj")},
  {SYMBOL_NAME("SEARCH"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_search_cfunobj)")},
  {SYMBOL_NAME("SECOND"), FUN_DENOT("&mk_cl_second_cfunobj")},
  {SYMBOL_NAME("SET"), FUN_DENOT("&mk_cl_set_cfunobj")},
  {SYMBOL_NAME("SET-DIFFERENCE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_set_difference_cfunobj)")},
  {SYMBOL_NAME("SET-DISPATCH-MACRO-CHARACTER"), FUN_DENOT("&mk_cl_set_dispatch_macro_character_cfunobj")},
  {SYMBOL_NAME("SET-EXCLUSIVE-OR"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_set_exclusive_or_cfunobj)")},
  {SYMBOL_NAME("SET-MACRO-CHARACTER"), FUN_DENOT("&mk_cl_set_macro_character_cfunobj")},
  {SYMBOL_NAME("SET-PPRINT-DISPATCH"), FUN_DENOT("MKCL_NAME_PPRINT(&mk_cl_set_pprint_dispatch_cfunobj)")},
  {SYMBOL_NAME("SET-SYNTAX-FROM-CHAR"), FUN_DENOT("&mk_cl_set_syntax_from_char_cfunobj")},
  {SYMBOL_NAME("SEVENTH"), FUN_DENOT("&mk_cl_seventh_cfunobj")},
  {SYMBOL_NAME("SHADOW"), FUN_DENOT("&mk_cl_shadow_cfunobj")},
  {SYMBOL_NAME("SHADOWING-IMPORT"), FUN_DENOT("&mk_cl_shadowing_import_cfunobj")},
  {SYMBOL_NAME("SIGNUM"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_signum_cfunobj)")},
  {SYMBOL_NAME("SIMPLE-BIT-VECTOR-P"), FUN_DENOT("&mk_cl_simple_bit_vector_p_cfunobj")},
  {SYMBOL_NAME("SIMPLE-STRING-P"), FUN_DENOT("&mk_cl_simple_string_p_cfunobj")},
  {SYMBOL_NAME("SIMPLE-VECTOR-P"), FUN_DENOT("&mk_cl_simple_vector_p_cfunobj")},
  {SYMBOL_NAME("SIN"), FUN_DENOT("&mk_cl_sin_cfunobj")},
  {SYMBOL_NAME("SINH"), FUN_DENOT("&mk_cl_sinh_cfunobj")},
  {SYMBOL_NAME("SIXTH"), FUN_DENOT("&mk_cl_sixth_cfunobj")},
  {SYMBOL_NAME("SLEEP"), FUN_DENOT("&mk_cl_sleep_cfunobj")},
  {SYMBOL_NAME("SOME"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_some_cfunobj)")},
  {SYMBOL_NAME("SORT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_sort_cfunobj)")},
  {SYMBOL_NAME("SPECIAL-OPERATOR-P"), FUN_DENOT("&mk_cl_special_operator_p_cfunobj")},
  {SYMBOL_NAME("SQRT"), FUN_DENOT("&mk_cl_sqrt_cfunobj")},
  {SYMBOL_NAME("STABLE-SORT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_stable_sort_cfunobj)")},
  {SYMBOL_NAME("STANDARD-CHAR-P"), FUN_DENOT("&mk_cl_standard_char_p_cfunobj")},
  {SYMBOL_NAME("STREAM-ELEMENT-TYPE"), FUN_DENOT("&mk_cl_stream_element_type_cfunobj")},
  {SYMBOL_NAME("STREAM-EXTERNAL-FORMAT"), FUN_DENOT("&mk_cl_stream_external_format_cfunobj")},
  {SYMBOL_NAME("STREAMP"), FUN_DENOT("&mk_cl_streamp_cfunobj")},
  {SYMBOL_NAME("STRING"), FUN_DENOT("&mk_cl_string_cfunobj")},
  {SYMBOL_NAME("STRING-DOWNCASE"), FUN_DENOT("&mk_cl_string_downcase_cfunobj")},
  {SYMBOL_NAME("STRING-CAPITALIZE"), FUN_DENOT("&mk_cl_string_capitalize_cfunobj")},
  {SYMBOL_NAME("STRING-EQUAL"), FUN_DENOT("&mk_cl_string_equal_cfunobj")},
  {SYMBOL_NAME("STRING-GREATERP"), FUN_DENOT("&mk_cl_string_greaterp_cfunobj")},
  {SYMBOL_NAME("STRING-LEFT-TRIM"), FUN_DENOT("&mk_cl_string_left_trim_cfunobj")},
  {SYMBOL_NAME("STRING-LESSP"), FUN_DENOT("&mk_cl_string_lessp_cfunobj")},
  {SYMBOL_NAME("STRING-NOT-EQUAL"), FUN_DENOT("&mk_cl_string_not_equal_cfunobj")},
  {SYMBOL_NAME("STRING-NOT-GREATERP"), FUN_DENOT("&mk_cl_string_not_greaterp_cfunobj")},
  {SYMBOL_NAME("STRING-NOT-LESSP"), FUN_DENOT("&mk_cl_string_not_lessp_cfunobj")},
  {SYMBOL_NAME("STRING-RIGHT-TRIM"), FUN_DENOT("&mk_cl_string_right_trim_cfunobj")},
  {SYMBOL_NAME("STRING-TRIM"), FUN_DENOT("&mk_cl_string_trim_cfunobj")},
  {SYMBOL_NAME("STRING-UPCASE"), FUN_DENOT("&mk_cl_string_upcase_cfunobj")},
  {SYMBOL_NAME("STRING/="), FUN_DENOT("&mk_cl_stringNE_cfunobj")},
  {SYMBOL_NAME("STRING<"), FUN_DENOT("&mk_cl_stringL_cfunobj")},
  {SYMBOL_NAME("STRING<="), FUN_DENOT("&mk_cl_stringLE_cfunobj")},
  {SYMBOL_NAME("STRING="), FUN_DENOT("&mk_cl_stringE_cfunobj")},
  {SYMBOL_NAME("STRING>"), FUN_DENOT("&mk_cl_stringG_cfunobj")},
  {SYMBOL_NAME("STRING>="), FUN_DENOT("&mk_cl_stringGE_cfunobj")},
  {SYMBOL_NAME("STRINGP"), FUN_DENOT("&mk_cl_stringp_cfunobj")},
  {SYMBOL_NAME("SUBLIS"), FUN_DENOT("&mk_cl_sublis_cfunobj")},
  {SYMBOL_NAME("SUBSEQ"), FUN_DENOT("&mk_cl_subseq_cfunobj")},
  {SYMBOL_NAME("SUBSETP"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_subsetp_cfunobj)")},
  {SYMBOL_NAME("SUBST"), FUN_DENOT("&mk_cl_subst_cfunobj")},
  {SYMBOL_NAME("SUBST-IF"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_subst_if_cfunobj)")},
  {SYMBOL_NAME("SUBST-IF-NOT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_subst_if_not_cfunobj)")},
  {SYMBOL_NAME("SUBSTITUTE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_substitute_cfunobj)")},
  {SYMBOL_NAME("SUBSTITUTE-IF"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_substitute_if_cfunobj)")},
  {SYMBOL_NAME("SUBSTITUTE-IF-NOT"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_substitute_if_not_cfunobj)")},
  {SYMBOL_NAME("SUBTYPEP"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_subtypep_cfunobj)")},
  {SYMBOL_NAME("SVREF"), FUN_DENOT("&mk_cl_svref_cfunobj")},
  {SYMBOL_NAME("SXHASH"), FUN_DENOT("&mk_cl_sxhash_cfunobj")},
  {SYMBOL_NAME("SYMBOL-FUNCTION"), FUN_DENOT("&mk_cl_symbol_function_cfunobj")},
  {SYMBOL_NAME("SYMBOL-NAME"), FUN_DENOT("&mk_cl_symbol_name_cfunobj")},
  {SYMBOL_NAME("SYMBOL-PACKAGE"), FUN_DENOT("&mk_cl_symbol_package_cfunobj")},
  {SYMBOL_NAME("SYMBOL-PLIST"), FUN_DENOT("&mk_cl_symbol_plist_cfunobj")},
  {SYMBOL_NAME("SYMBOL-VALUE"), FUN_DENOT("&mk_cl_symbol_value_cfunobj")},
  {SYMBOL_NAME("SYMBOLP"), FUN_DENOT("&mk_cl_symbolp_cfunobj")},
  {SYMBOL_NAME("SYNONYM-STREAM-SYMBOL"), FUN_DENOT("&mk_cl_synonym_stream_symbol_cfunobj")},
  {SYMBOL_NAME("TAILP"), FUN_DENOT("&mk_cl_tailp_cfunobj")},
  {SYMBOL_NAME("TAN"), FUN_DENOT("&mk_cl_tan_cfunobj")},
  {SYMBOL_NAME("TANH"), FUN_DENOT("&mk_cl_tanh_cfunobj")},
  {SYMBOL_NAME("TENTH"), FUN_DENOT("&mk_cl_tenth_cfunobj")},
  {SYMBOL_NAME("TERPRI"), FUN_DENOT("&mk_cl_terpri_cfunobj")},
  {SYMBOL_NAME("THIRD"), FUN_DENOT("&mk_cl_third_cfunobj")},
  {SYMBOL_NAME("TRANSLATE-LOGICAL-PATHNAME"), FUN_DENOT("&mk_cl_translate_logical_pathname_cfunobj")},
  {SYMBOL_NAME("TRANSLATE-PATHNAME"), FUN_DENOT("&mk_cl_translate_pathname_cfunobj")},
  {SYMBOL_NAME("TREE-EQUAL"), FUN_DENOT("&mk_cl_tree_equal_cfunobj")},
  {SYMBOL_NAME("TRUENAME"), FUN_DENOT("&mk_cl_truename_cfunobj")},
  {SYMBOL_NAME("TRUNCATE"), FUN_DENOT("&mk_cl_truncate_cfunobj")},
  {SYMBOL_NAME("TWO-WAY-STREAM-INPUT-STREAM"), FUN_DENOT("&mk_cl_two_way_stream_input_stream_cfunobj")},
  {SYMBOL_NAME("TWO-WAY-STREAM-OUTPUT-STREAM"), FUN_DENOT("&mk_cl_two_way_stream_output_stream_cfunobj")},
  {SYMBOL_NAME("TYPE-OF"), FUN_DENOT("&mk_cl_type_of_cfunobj")},
  {SYMBOL_NAME("TYPEP"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_typep_cfunobj)")},
  {SYMBOL_NAME("UNEXPORT"), FUN_DENOT("&mk_cl_unexport_cfunobj")},
  {SYMBOL_NAME("UNINTERN"), FUN_DENOT("&mk_cl_unintern_cfunobj")},
  {SYMBOL_NAME("UNION"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_union_cfunobj)")},
  {SYMBOL_NAME("UNREAD-CHAR"), FUN_DENOT("&mk_cl_unread_char_cfunobj")},
  {SYMBOL_NAME("UNUSE-PACKAGE"), FUN_DENOT("&mk_cl_unuse_package_cfunobj")},
  {SYMBOL_NAME("UPGRADED-ARRAY-ELEMENT-TYPE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_upgraded_array_element_type_cfunobj)")},
  {SYMBOL_NAME("UPGRADED-COMPLEX-PART-TYPE"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_upgraded_complex_part_type_cfunobj)")},
  {SYMBOL_NAME("UPPER-CASE-P"), FUN_DENOT("&mk_cl_upper_case_p_cfunobj")},
  {SYMBOL_NAME("USE-PACKAGE"), FUN_DENOT("&mk_cl_use_package_cfunobj")},
  {SYMBOL_NAME("USER-HOMEDIR-PATHNAME"), FUN_DENOT("&mk_cl_user_homedir_pathname_cfunobj")},
  {SYMBOL_NAME("VALUES"), FUN_DENOT("&mk_cl_values_cfunobj")},
  {SYMBOL_NAME("VALUES-LIST"), FUN_DENOT("&mk_cl_values_list_cfunobj")},
  {SYMBOL_NAME("VECTOR"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_vector_cfunobj)")},
  {SYMBOL_NAME("VECTOR-POP"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_vector_pop_cfunobj)")},
  {SYMBOL_NAME("VECTOR-PUSH"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_vector_push_cfunobj)")},
  {SYMBOL_NAME("VECTOR-PUSH-EXTEND"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_vector_push_extend_cfunobj)")},
  {SYMBOL_NAME("VECTORP"), FUN_DENOT("&mk_cl_vectorp_cfunobj")},
  {SYMBOL_NAME("WILD-PATHNAME-P"), FUN_DENOT("&mk_cl_wild_pathname_p_cfunobj")},
  {SYMBOL_NAME("WRITE"), FUN_DENOT("&mk_cl_write_cfunobj")},
  {SYMBOL_NAME("WRITE-BYTE"), FUN_DENOT("&mk_cl_write_byte_cfunobj")},
  {SYMBOL_NAME("WRITE-CHAR"), FUN_DENOT("&mk_cl_write_char_cfunobj")},
  {SYMBOL_NAME("WRITE-LINE"), FUN_DENOT("&mk_cl_write_line_cfunobj")},
  {SYMBOL_NAME("WRITE-SEQUENCE"), FUN_DENOT("&mk_cl_write_sequence_cfunobj")},
  {SYMBOL_NAME("WRITE-STRING"), FUN_DENOT("&mk_cl_write_string_cfunobj")},
  {SYMBOL_NAME("WRITE-TO-STRING"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_write_to_string_cfunobj)")},
  {SYMBOL_NAME("Y-OR-N-P"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_y_or_n_p_cfunobj)")},
  {SYMBOL_NAME("YES-OR-NO-P"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_yes_or_no_p_cfunobj)")},
  {SYMBOL_NAME("ZEROP"), FUN_DENOT("&mk_cl_zerop_cfunobj")},
  {SYMBOL_NAME("CLASS-OF"), FUN_DENOT("&mk_cl_class_of_cfunobj")},
  {SYMBOL_NAME("FIND-CLASS"), FUN_DENOT("&mk_cl_find_class_cfunobj")},
  {SYMBOL_NAME("INVALID-METHOD-ERROR"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_invalid_method_error_cfunobj)")},
  {SYMBOL_NAME("METHOD-COMBINATION-ERROR"), FUN_DENOT("MKCL_IN_LISP(&mk_cl_method_combination_error_cfunobj)")},
};

struct mkcl_function_declaration mkcl_cl_declare_macros[] = {
};

struct mkcl_function_declaration mkcl_cl_declare_special_operators[] = {
  {SYMBOL_NAME("BLOCK"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("CATCH"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("EVAL-WHEN"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("FLET"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("FUNCTION"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("GO"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("IF"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("LABELS"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("LET"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("LET*"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("LOAD-TIME-VALUE"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("LOCALLY"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("MACROLET"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("MULTIPLE-VALUE-CALL"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("MULTIPLE-VALUE-PROG1"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("PROGN"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("PROGV"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("QUOTE"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("RETURN-FROM"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("SETQ"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("SYMBOL-MACROLET"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("TAGBODY"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("THE"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("THROW"), FUN_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("UNWIND-PROTECT"), FUN_DENOT("mk_cl_Cnil")},
  /* {SYMBOL_NAME("LAMBDA"), FUN_DENOT("mk_cl_Cnil")}, */
  /* {SYMBOL_NAME("CASE"), FUN_DENOT("mk_cl_Cnil")}, */
  /* {SYMBOL_NAME("COND"), FUN_DENOT("mk_cl_Cnil")}, */
  /* {SYMBOL_NAME("DO"), FUN_DENOT("mk_cl_Cnil")}, */
  /* {SYMBOL_NAME("DO*"), FUN_DENOT("mk_cl_Cnil")}, */
  /* {SYMBOL_NAME("DOLIST"), FUN_DENOT("mk_cl_Cnil")}, */
  /* {SYMBOL_NAME("DOTIMES"), FUN_DENOT("mk_cl_Cnil")}, */
  /* {SYMBOL_NAME("MULTIPLE-VALUE-BIND"), FUN_DENOT("mk_cl_Cnil")}, */
  /* {SYMBOL_NAME("MULTIPLE-VALUE-LIST"), FUN_DENOT("mk_cl_Cnil")}, */
  /* {SYMBOL_NAME("MULTIPLE-VALUE-SETQ"), FUN_DENOT("mk_cl_Cnil")}, */
  /* {SYMBOL_NAME("NTH-VALUE"), FUN_DENOT("mk_cl_Cnil")}, */
  /* {SYMBOL_NAME("PROG"), FUN_DENOT("mk_cl_Cnil")}, */
  /* {SYMBOL_NAME("PROG*"), FUN_DENOT("mk_cl_Cnil")}, */
  /* {SYMBOL_NAME("PROG1"), FUN_DENOT("mk_cl_Cnil")}, */
  /* {SYMBOL_NAME("PROG2"), FUN_DENOT("mk_cl_Cnil")}, */
  /* {SYMBOL_NAME("PSETQ"), FUN_DENOT("mk_cl_Cnil")}, */
  /* {SYMBOL_NAME("RETURN"), FUN_DENOT("mk_cl_Cnil")}, */
  /* {SYMBOL_NAME("UNLESS"), FUN_DENOT("mk_cl_Cnil")}, */
  /* {SYMBOL_NAME("WHEN"), FUN_DENOT("mk_cl_Cnil")}, */
};

struct mkcl_variable_declaration {
  struct mkcl_base_string symbol_name;
  struct mkcl_base_string value_denotator;
};

struct mkcl_variable_declaration mkcl_cl_declare_specials[] = {
  /*  {SYMBOL_NAME("*BUILTIN-CLASSES*"), VAL_DENOT("mk_cl_Cnil")}, */
  {SYMBOL_NAME("+"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("++"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("+++"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("-"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("**"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("***"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("/"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("//"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("///"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*BREAK-ON-SIGNALS*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*COMPILE-FILE-PATHNAME*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*COMPILE-FILE-TRUENAME*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*COMPILE-PRINT*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*COMPILE-VERBOSE*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*DEBUG-IO*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*DEBUGGER-HOOK*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*DEFAULT-PATHNAME-DEFAULTS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*ERROR-OUTPUT*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*FEATURES*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*GENSYM-COUNTER*"), VAL_DENOT("MKCL_MAKE_FIXNUM(0)")},
  {SYMBOL_NAME("*LOAD-PATHNAME*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*LOAD-PRINT*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*LOAD-TRUENAME*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*LOAD-VERBOSE*"), VAL_DENOT("mk_cl_Ct")},
  {SYMBOL_NAME("*MACROEXPAND-HOOK*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*MODULES*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*PACKAGE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*PRINT-ARRAY*"), VAL_DENOT("mk_cl_Ct")},
  {SYMBOL_NAME("*PRINT-BASE*"), VAL_DENOT("MKCL_MAKE_FIXNUM(10)")},
  {SYMBOL_NAME("*PRINT-CASE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*PRINT-CIRCLE*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*PRINT-ESCAPE*"), VAL_DENOT("mk_cl_Ct")},
  {SYMBOL_NAME("*PRINT-GENSYM*"), VAL_DENOT("mk_cl_Ct")},
  {SYMBOL_NAME("*PRINT-LENGTH*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*PRINT-LEVEL*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*PRINT-LINES*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*PRINT-MISER-WIDTH*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*PRINT-PPRINT-DISPATCH*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*PRINT-PRETTY*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*PRINT-RADIX*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*PRINT-READABLY*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*PRINT-RIGHT-MARGIN*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*QUERY-IO*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*RANDOM-STATE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*READ-BASE*"), VAL_DENOT("MKCL_MAKE_FIXNUM(10)")},
  {SYMBOL_NAME("*READ-DEFAULT-FLOAT-FORMAT*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*READ-EVAL*"), VAL_DENOT("mk_cl_Ct")},
  {SYMBOL_NAME("*READ-SUPPRESS*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*READTABLE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*STANDARD-INPUT*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*STANDARD-OUTPUT*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*TERMINAL-IO*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*TRACE-OUTPUT*"), VAL_DENOT("MKCL_OBJNULL")},
};

struct mkcl_variable_declaration mkcl_cl_declare_constants[] = {
  {SYMBOL_NAME("NIL"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("T"), VAL_DENOT("mk_cl_Ct")},
  {SYMBOL_NAME("ARRAY-DIMENSION-LIMIT"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_ADIMLIM)")},
  {SYMBOL_NAME("ARRAY-RANK-LIMIT"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_ARANKLIM)")},
  {SYMBOL_NAME("ARRAY-TOTAL-SIZE-LIMIT"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_ATOTLIM)")},
  {SYMBOL_NAME("BOOLE-1"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_BOOL1)")},
  {SYMBOL_NAME("BOOLE-2"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_BOOL2)")},
  {SYMBOL_NAME("BOOLE-AND"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_BOOLAND)")},
  {SYMBOL_NAME("BOOLE-ANDC1"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_BOOLANDC1)")},
  {SYMBOL_NAME("BOOLE-ANDC2"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_BOOLANDC2)")},
  {SYMBOL_NAME("BOOLE-C1"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_BOOLC1)")},
  {SYMBOL_NAME("BOOLE-C2"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_BOOLC2)")},
  {SYMBOL_NAME("BOOLE-CLR"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_BOOLCLR)")},
  {SYMBOL_NAME("BOOLE-EQV"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_BOOLEQV)")},
  {SYMBOL_NAME("BOOLE-IOR"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_BOOLIOR)")},
  {SYMBOL_NAME("BOOLE-NAND"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_BOOLNAND)")},
  {SYMBOL_NAME("BOOLE-NOR"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_BOOLNOR)")},
  {SYMBOL_NAME("BOOLE-ORC1"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_BOOLORC1)")},
  {SYMBOL_NAME("BOOLE-ORC2"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_BOOLORC2)")},
  {SYMBOL_NAME("BOOLE-SET"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_BOOLSET)")},
  {SYMBOL_NAME("BOOLE-XOR"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_BOOLXOR)")},
  {SYMBOL_NAME("CALL-ARGUMENTS-LIMIT"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_CALL_ARGUMENTS_LIMIT)")},
  {SYMBOL_NAME("CHAR-CODE-LIMIT"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_CHAR_CODE_LIMIT)")},
  {SYMBOL_NAME("DOUBLE-FLOAT-EPSILON"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("DOUBLE-FLOAT-NEGATIVE-EPSILON"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("INTERNAL-TIME-UNITS-PER-SECOND"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LAMBDA-LIST-KEYWORDS"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LAMBDA-PARAMETERS-LIMIT"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_LAMBDA_PARAMETERS_LIMIT)")},
  {SYMBOL_NAME("LEAST-NEGATIVE-DOUBLE-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LEAST-NEGATIVE-LONG-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LEAST-NEGATIVE-SHORT-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LEAST-NEGATIVE-SINGLE-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LEAST-POSITIVE-DOUBLE-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LEAST-POSITIVE-LONG-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LEAST-POSITIVE-NORMALIZED-LONG-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LEAST-POSITIVE-SHORT-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LEAST-POSITIVE-SINGLE-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LONG-FLOAT-EPSILON"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LONG-FLOAT-NEGATIVE-EPSILON"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("MOST-NEGATIVE-DOUBLE-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("MOST-NEGATIVE-FIXNUM"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_MOST_NEGATIVE_FIXNUM)")},
  {SYMBOL_NAME("MOST-NEGATIVE-LONG-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("MOST-NEGATIVE-SHORT-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("MOST-NEGATIVE-SINGLE-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("MOST-POSITIVE-DOUBLE-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("MOST-POSITIVE-FIXNUM"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_MOST_POSITIVE_FIXNUM)")},
  {SYMBOL_NAME("MOST-POSITIVE-LONG-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("MOST-POSITIVE-SHORT-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("MOST-POSITIVE-SINGLE-FLOAT"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("MULTIPLE-VALUES-LIMIT"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_MULTIPLE_VALUES_LIMIT)")},
  {SYMBOL_NAME("PI"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("SHORT-FLOAT-EPSILON"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("SHORT-FLOAT-NEGATIVE-EPSILON"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("SINGLE-FLOAT-EPSILON"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("SINGLE-FLOAT-NEGATIVE-EPSILON"), VAL_DENOT("MKCL_OBJNULL")},
};

struct exposed_symbol {
  struct mkcl_base_string symbol_name;
  char * exposition;
};

static struct exposed_symbol const exposed_symbols[] = {
  /*  { SYMBOL_NAME("SOMETHING"), "MK_CL_something" }, */
  { SYMBOL_NAME("="), "MK_CL_E" },
  { SYMBOL_NAME("<"), "MK_CL_L" },
  { SYMBOL_NAME("<="), "MK_CL_LE" },
  { SYMBOL_NAME(">"), "MK_CL_G" },
  { SYMBOL_NAME(">="), "MK_CL_GE" },
  { SYMBOL_NAME("-"), "MK_CL_M" },
  { SYMBOL_NAME("/"), "MK_CL_N" },
  { SYMBOL_NAME("/="), "MK_CL_NE" },
  { SYMBOL_NAME("+"), "MK_CL_P" },
  { SYMBOL_NAME("*"), "MK_CL_X" },
  { SYMBOL_NAME("ABS"), "MK_CL_abs" },
  { SYMBOL_NAME("ACONS"), "MK_CL_acons" },
  { SYMBOL_NAME("ACOS"), "MK_CL_acos" },
  { SYMBOL_NAME("ACOSH"), "MK_CL_acosh" },
  { SYMBOL_NAME("ADJOIN"), "MK_CL_adjoin" },
  { SYMBOL_NAME("ADJUSTABLE-ARRAY-P"), "MK_CL_adjustable_array_p" },
  { SYMBOL_NAME("ADJUST-ARRAY"), "MK_CL_adjust_array" },
  { SYMBOL_NAME("ALPHA-CHAR-P"), "MK_CL_alpha_char_p" },
  { SYMBOL_NAME("ALPHANUMERICP"), "MK_CL_alphanumericp" },
  { SYMBOL_NAME("AND"), "MK_CL_and" },
  { SYMBOL_NAME("APPEND"), "MK_CL_append" },
  { SYMBOL_NAME("APPLY"), "MK_CL_apply" },
  { SYMBOL_NAME("APROPOS"), "MK_CL_apropos" },
  { SYMBOL_NAME("APROPOS-LIST"), "MK_CL_apropos_list" },
  { SYMBOL_NAME("AREF"), "MK_CL_aref" },
  { SYMBOL_NAME("ARITHMETIC-ERROR"), "MK_CL_arithmetic_error" },
  { SYMBOL_NAME("ARRAY"), "MK_CL_array" },
  { SYMBOL_NAME("ARRAY-DIMENSION"), "MK_CL_array_dimension" },
  { SYMBOL_NAME("ARRAY-DIMENSIONS"), "MK_CL_array_dimensions" },
  { SYMBOL_NAME("ARRAY-DISPLACEMENT"), "MK_CL_array_displacement" },
  { SYMBOL_NAME("ARRAY-ELEMENT-TYPE"), "MK_CL_array_element_type" },
  { SYMBOL_NAME("ARRAY-HAS-FILL-POINTER-P"), "MK_CL_array_has_fill_pointer_p" },
  { SYMBOL_NAME("ARRAY-IN-BOUNDS-P"), "MK_CL_array_in_bounds_p" },
  { SYMBOL_NAME("ARRAYP"), "MK_CL_arrayp" },
  { SYMBOL_NAME("ARRAY-RANK"), "MK_CL_array_rank" },
  { SYMBOL_NAME("ARRAY-ROW-MAJOR-INDEX"), "MK_CL_array_row_major_index" },
  { SYMBOL_NAME("ARRAY-TOTAL-SIZE"), "MK_CL_array_total_size" },
  { SYMBOL_NAME("ASH"), "MK_CL_ash" },
  { SYMBOL_NAME("ASIN"), "MK_CL_asin" },
  { SYMBOL_NAME("ASINH"), "MK_CL_asinh" },
  { SYMBOL_NAME("ASSOC"), "MK_CL_assoc" },
  { SYMBOL_NAME("ASSOC-IF"), "MK_CL_assoc_if" },
  { SYMBOL_NAME("ASSOC-IF-NOT"), "MK_CL_assoc_if_not" },
  { SYMBOL_NAME("ATAN"), "MK_CL_atan" },
  { SYMBOL_NAME("ATANH"), "MK_CL_atanh" },
  { SYMBOL_NAME("ATOM"), "MK_CL_atom" },
  { SYMBOL_NAME("BASE-CHAR"), "MK_CL_base_char" },
  { SYMBOL_NAME("BASE-STRING"), "MK_CL_base_string" },
  { SYMBOL_NAME("BIGNUM"), "MK_CL_bignum" },
  { SYMBOL_NAME("BIT"), "MK_CL_bit" },
  { SYMBOL_NAME("BIT-AND"), "MK_CL_bit_and" },
  { SYMBOL_NAME("BIT-ANDC1"), "MK_CL_bit_andc1" },
  { SYMBOL_NAME("BIT-ANDC2"), "MK_CL_bit_andc2" },
  { SYMBOL_NAME("BIT-EQV"), "MK_CL_bit_eqv" },
  { SYMBOL_NAME("BIT-IOR"), "MK_CL_bit_ior" },
  { SYMBOL_NAME("BIT-NAND"), "MK_CL_bit_nand" },
  { SYMBOL_NAME("BIT-NOR"), "MK_CL_bit_nor" },
  { SYMBOL_NAME("BIT-NOT"), "MK_CL_bit_not" },
  { SYMBOL_NAME("BIT-ORC1"), "MK_CL_bit_orc1" },
  { SYMBOL_NAME("BIT-ORC2"), "MK_CL_bit_orc2" },
  { SYMBOL_NAME("BIT-VECTOR"), "MK_CL_bit_vector" },
  { SYMBOL_NAME("BIT-VECTOR-P"), "MK_CL_bit_vector_p" },
  { SYMBOL_NAME("BIT-XOR"), "MK_CL_bit_xor" },
  { SYMBOL_NAME("BLOCK"), "MK_CL_block" },
  { SYMBOL_NAME("BOOLE"), "MK_CL_boole" },
  { SYMBOL_NAME("BOOLEAN"), "MK_CL_boolean" },
  { SYMBOL_NAME("BOTH-CASE-P"), "MK_CL_both_case_p" },
  { SYMBOL_NAME("BOUNDP"), "MK_CL_boundp" },
  { SYMBOL_NAME("BROADCAST-STREAM"), "MK_CL_broadcast_stream" },
  { SYMBOL_NAME("BROADCAST-STREAM-STREAMS"), "MK_CL_broadcast_stream_streams" },
  { SYMBOL_NAME("BUTLAST"), "MK_CL_butlast" },
  { SYMBOL_NAME("BYTE"), "MK_CL_byte" },
  { SYMBOL_NAME("BYTE-POSITION"), "MK_CL_byte_position" },
  { SYMBOL_NAME("BYTE-SIZE"), "MK_CL_byte_size" },
  { SYMBOL_NAME("CAR"), "MK_CL_car" },
  { SYMBOL_NAME("CAAR"), "MK_CL_caar" },
  { SYMBOL_NAME("CADR"), "MK_CL_cadr" },
  { SYMBOL_NAME("CAAAR"), "MK_CL_caaar" },
  { SYMBOL_NAME("CAADR"), "MK_CL_caadr" },
  { SYMBOL_NAME("CADAR"), "MK_CL_cadar" },
  { SYMBOL_NAME("CADDR"), "MK_CL_caddr" },
  { SYMBOL_NAME("CAAAAR"), "MK_CL_caaaar" },
  { SYMBOL_NAME("CAAADR"), "MK_CL_caaadr" },
  { SYMBOL_NAME("CAADAR"), "MK_CL_caadar" },
  { SYMBOL_NAME("CAADDR"), "MK_CL_caaddr" },
  { SYMBOL_NAME("CADAAR"), "MK_CL_cadaar" },
  { SYMBOL_NAME("CADADR"), "MK_CL_cadadr" },
  { SYMBOL_NAME("CADDAR"), "MK_CL_caddar" },
  { SYMBOL_NAME("CADDDR"), "MK_CL_cadddr" },
  { SYMBOL_NAME("CASE"), "MK_CL_case" },
  { SYMBOL_NAME("CATCH"), "MK_CL_catch" },
  { SYMBOL_NAME("CDR"), "MK_CL_cdr" },
  { SYMBOL_NAME("CDAR"), "MK_CL_cdar" },
  { SYMBOL_NAME("CDDR"), "MK_CL_cddr" },
  { SYMBOL_NAME("CDAAR"), "MK_CL_cdaar" },
  { SYMBOL_NAME("CDADR"), "MK_CL_cdadr" },
  { SYMBOL_NAME("CDDAR"), "MK_CL_cddar" },
  { SYMBOL_NAME("CDDDR"), "MK_CL_cdddr" },
  { SYMBOL_NAME("CDAAAR"), "MK_CL_cdaaar" },
  { SYMBOL_NAME("CDAADR"), "MK_CL_cdaadr" },
  { SYMBOL_NAME("CDADAR"), "MK_CL_cdadar" },
  { SYMBOL_NAME("CDADDR"), "MK_CL_cdaddr" },
  { SYMBOL_NAME("CDDAAR"), "MK_CL_cddaar" },
  { SYMBOL_NAME("CDDADR"), "MK_CL_cddadr" },
  { SYMBOL_NAME("CDDDAR"), "MK_CL_cdddar" },
  { SYMBOL_NAME("CDDDDR"), "MK_CL_cddddr" },
  { SYMBOL_NAME("CEILING"), "MK_CL_ceiling" },
  { SYMBOL_NAME("CERROR"), "MK_CL_cerror" },
  { SYMBOL_NAME("CHAR"), "MK_CL_char" },
  { SYMBOL_NAME("CHAR-CODE"), "MK_CL_char_code" },
  { SYMBOL_NAME("CHAR-DOWNCASE"), "MK_CL_char_downcase" },
  { SYMBOL_NAME("CHAR="), "MK_CL_charE" },
  { SYMBOL_NAME("CHAR>"), "MK_CL_charG" },
  { SYMBOL_NAME("CHAR>="), "MK_CL_charGE" },
  { SYMBOL_NAME("CHAR<"), "MK_CL_charL" },
  { SYMBOL_NAME("CHAR<="), "MK_CL_charLE" },
  { SYMBOL_NAME("CHAR/="), "MK_CL_charNE" },
  { SYMBOL_NAME("CHARACTER"), "MK_CL_character" },
  { SYMBOL_NAME("CHARACTERP"), "MK_CL_characterp" },
  { SYMBOL_NAME("CHAR-EQUAL"), "MK_CL_char_equal" },
  { SYMBOL_NAME("CHAR-GREATERP"), "MK_CL_char_greaterp" },
  { SYMBOL_NAME("CHAR-INT"), "MK_CL_char_int" },
  { SYMBOL_NAME("CHAR-LESSP"), "MK_CL_char_lessp" },
  { SYMBOL_NAME("CHAR-NAME"), "MK_CL_char_name" },
  { SYMBOL_NAME("CHAR-NOT-EQUAL"), "MK_CL_char_not_equal" },
  { SYMBOL_NAME("CHAR-NOT-GREATERP"), "MK_CL_char_not_greaterp" },
  { SYMBOL_NAME("CHAR-NOT-LESSP"), "MK_CL_char_not_lessp" },
  { SYMBOL_NAME("CHAR-UPCASE"), "MK_CL_char_upcase" },
  { SYMBOL_NAME("CIS"), "MK_CL_cis" },
  { SYMBOL_NAME("CLASS-OF"), "MK_CL_class_of" },
  { SYMBOL_NAME("CLEAR-INPUT"), "MK_CL_clear_input" },
  { SYMBOL_NAME("CLEAR-OUTPUT"), "MK_CL_clear_output" },
  { SYMBOL_NAME("CLOSE"), "MK_CL_close" },
  { SYMBOL_NAME("CLRHASH"), "MK_CL_clrhash" },
  { SYMBOL_NAME("CODE-CHAR"), "MK_CL_code_char" },
  { SYMBOL_NAME("COERCE"), "MK_CL_coerce" },
  { SYMBOL_NAME("COMPILE"), "MK_CL_compile" },
  { SYMBOL_NAME("COMPILED-FUNCTION"), "MK_CL_compiled_function" },
  { SYMBOL_NAME("COMPILED-FUNCTION-P"), "MK_CL_compiled_function_p" },
  { SYMBOL_NAME("COMPLEMENT"), "MK_CL_complement" },
  { SYMBOL_NAME("COMPLEX"), "MK_CL_complex" },
  { SYMBOL_NAME("COMPLEXP"), "MK_CL_complexp" },
  { SYMBOL_NAME("CONCATENATE"), "MK_CL_concatenate" },
  { SYMBOL_NAME("CONCATENATED-STREAM"), "MK_CL_concatenated_stream" },
  { SYMBOL_NAME("CONCATENATED-STREAM-STREAMS"), "MK_CL_concatenated_stream_streams" },
  { SYMBOL_NAME("COND"), "MK_CL_cond" },
  { SYMBOL_NAME("CONJUGATE"), "MK_CL_conjugate" },
  { SYMBOL_NAME("CONS"), "MK_CL_cons" },
  { SYMBOL_NAME("CONSP"), "MK_CL_consp" },
  { SYMBOL_NAME("CONSTANTP"), "MK_CL_constantp" },
  { SYMBOL_NAME("CONSTANTLY"), "MK_CL_constantly" },
  { SYMBOL_NAME("CONTINUE"), "MK_CL_continue" },
  { SYMBOL_NAME("COPY-ALIST"), "MK_CL_copy_alist" },
  { SYMBOL_NAME("COPY-LIST"), "MK_CL_copy_list" },
  { SYMBOL_NAME("COPY-PPRINT-DISPATCH"), "MK_CL_copy_pprint_dispatch" },
  { SYMBOL_NAME("COPY-READTABLE"), "MK_CL_copy_readtable" },
  { SYMBOL_NAME("COPY-SEQ"), "MK_CL_copy_seq" },
  { SYMBOL_NAME("COPY-STRUCTURE"), "MK_CL_copy_structure" },
  { SYMBOL_NAME("COPY-SYMBOL"), "MK_CL_copy_symbol" },
  { SYMBOL_NAME("COPY-TREE"), "MK_CL_copy_tree" },
  { SYMBOL_NAME("COS"), "MK_CL_cos" },
  { SYMBOL_NAME("COSH"), "MK_CL_cosh" },
  { SYMBOL_NAME("COUNT"), "MK_CL_count" },
  { SYMBOL_NAME("COUNT-IF"), "MK_CL_count_if" },
  { SYMBOL_NAME("COUNT-IF-NOT"), "MK_CL_count_if_not" },
  { SYMBOL_NAME("DECLARE"), "MK_CL_declare" },
  { SYMBOL_NAME("DECODE-FLOAT"), "MK_CL_decode_float" },
  { SYMBOL_NAME("DECODE-UNIVERSAL-TIME"), "MK_CL_decode_universal_time" },
  { SYMBOL_NAME("DELETE"), "MK_CL_delete" },
  { SYMBOL_NAME("DELETE-DUPLICATES"), "MK_CL_delete_duplicates" },
  { SYMBOL_NAME("DELETE-FILE"), "MK_CL_delete_file" },
  { SYMBOL_NAME("DELETE-IF"), "MK_CL_delete_if" },
  { SYMBOL_NAME("DELETE-IF-NOT"), "MK_CL_delete_if_not" },
  { SYMBOL_NAME("DELETE-PACKAGE"), "MK_CL_delete_package" },
  { SYMBOL_NAME("DENOMINATOR"), "MK_CL_denominator" },
  { SYMBOL_NAME("DEPOSIT-FIELD"), "MK_CL_deposit_field" },
  { SYMBOL_NAME("DESTRUCTURING-BIND"), "MK_CL_destructuring_bind" },
  { SYMBOL_NAME("DIGIT-CHAR"), "MK_CL_digit_char" },
  { SYMBOL_NAME("DIGIT-CHAR-P"), "MK_CL_digit_char_p" },
  { SYMBOL_NAME("DIRECTORY"), "MK_CL_directory" },
  { SYMBOL_NAME("DIRECTORY-NAMESTRING"), "MK_CL_directory_namestring" },
  { SYMBOL_NAME("DIVISION-BY-ZERO"), "MK_CL_division_by_zero" },
  { SYMBOL_NAME("DOUBLE-FLOAT"), "MK_CL_double_float" },
  { SYMBOL_NAME("DPB"), "MK_CL_dpb" },
  { SYMBOL_NAME("ECHO-STREAM"), "MK_CL_echo_stream" },
  { SYMBOL_NAME("ECHO-STREAM-INPUT-STREAM"), "MK_CL_echo_stream_input_stream" },
  { SYMBOL_NAME("ECHO-STREAM-OUTPUT-STREAM"), "MK_CL_echo_stream_output_stream" },
  { SYMBOL_NAME("EIGHTH"), "MK_CL_eighth" },
  { SYMBOL_NAME("ELT"), "MK_CL_elt" },
  { SYMBOL_NAME("ENCODE-UNIVERSAL-TIME"), "MK_CL_encode_universal_time" },
  { SYMBOL_NAME("END-OF-FILE"), "MK_CL_end_of_file" },
  { SYMBOL_NAME("ENDP"), "MK_CL_endp" },
  { SYMBOL_NAME("ENOUGH-NAMESTRING"), "MK_CL_enough_namestring" },
  { SYMBOL_NAME("ENSURE-DIRECTORIES-EXIST"), "MK_CL_ensure_directories_exist" },
  { SYMBOL_NAME("EQ"), "MK_CL_eq" },
  { SYMBOL_NAME("EQL"), "MK_CL_eql" },
  { SYMBOL_NAME("EQUAL"), "MK_CL_equal" },
  { SYMBOL_NAME("EQUALP"), "MK_CL_equalp" },
  { SYMBOL_NAME("ERROR"), "MK_CL_error" },
  { SYMBOL_NAME("EVAL"), "MK_CL_eval" },
  { SYMBOL_NAME("EVAL-WHEN"), "MK_CL_eval_when" },
  { SYMBOL_NAME("EVENP"), "MK_CL_evenp" },
  { SYMBOL_NAME("EVERY"), "MK_CL_every" },
  { SYMBOL_NAME("EXP"), "MK_CL_exp" },
  { SYMBOL_NAME("EXPORT"), "MK_CL_export" },
  { SYMBOL_NAME("EXPT"), "MK_CL_expt" },
  { SYMBOL_NAME("FBOUNDP"), "MK_CL_fboundp" },
  { SYMBOL_NAME("FDEFINITION"), "MK_CL_fdefinition" },
  { SYMBOL_NAME("FCEILING"), "MK_CL_fceiling" },
  { SYMBOL_NAME("FFLOOR"), "MK_CL_ffloor" },
  { SYMBOL_NAME("FIFTH"), "MK_CL_fifth" },
  { SYMBOL_NAME("FILE-AUTHOR"), "MK_CL_file_author" },
  { SYMBOL_NAME("FILE-ERROR"), "MK_CL_file_error" },
  { SYMBOL_NAME("FILE-LENGTH"), "MK_CL_file_length" },
  { SYMBOL_NAME("FILE-NAMESTRING"), "MK_CL_file_namestring" },
  { SYMBOL_NAME("FILE-POSITION"), "MK_CL_file_position" },
  { SYMBOL_NAME("FILE-STREAM"), "MK_CL_file_stream" },
  { SYMBOL_NAME("FILE-STRING-LENGTH"), "MK_CL_file_string_length" },
  { SYMBOL_NAME("FILE-WRITE-DATE"), "MK_CL_file_write_date" },
  { SYMBOL_NAME("FILL"), "MK_CL_fill" },
  { SYMBOL_NAME("FILL-POINTER"), "MK_CL_fill_pointer" },
  { SYMBOL_NAME("FIND"), "MK_CL_find" },
  { SYMBOL_NAME("FIND-ALL-SYMBOLS"), "MK_CL_find_all_symbols" },
  { SYMBOL_NAME("FIND-CLASS"), "MK_CL_find_class" },
  { SYMBOL_NAME("FIND-IF"), "MK_CL_find_if" },
  { SYMBOL_NAME("FIND-IF-NOT"), "MK_CL_find_if_not" },
  { SYMBOL_NAME("FIND-PACKAGE"), "MK_CL_find_package" },
  { SYMBOL_NAME("FIND-SYMBOL"), "MK_CL_find_symbol" },
  { SYMBOL_NAME("FINISH-OUTPUT"), "MK_CL_finish_output" },
  { SYMBOL_NAME("FIRST"), "MK_CL_first" },
  { SYMBOL_NAME("FIXNUM"), "MK_CL_fixnum" },
  { SYMBOL_NAME("FLET"), "MK_CL_flet" },
  { SYMBOL_NAME("FLOAT"), "MK_CL_float" },
  { SYMBOL_NAME("FLOATP"), "MK_CL_floatp" },
  { SYMBOL_NAME("FLOAT-DIGITS"), "MK_CL_float_digits" },
  { SYMBOL_NAME("FLOATING-POINT-INEXACT"), "MK_CL_floating_point_inexact" },
  { SYMBOL_NAME("FLOATING-POINT-INVALID-OPERATION"), "MK_CL_floating_point_invalid_operation" },
  { SYMBOL_NAME("FLOATING-POINT-OVERFLOW"), "MK_CL_floating_point_overflow" },
  { SYMBOL_NAME("FLOATING-POINT-UNDERFLOW"), "MK_CL_floating_point_underflow" },
  { SYMBOL_NAME("FLOAT-PRECISION"), "MK_CL_float_precision" },
  { SYMBOL_NAME("FLOAT-RADIX"), "MK_CL_float_radix" },
  { SYMBOL_NAME("FLOAT-SIGN"), "MK_CL_float_sign" },
  { SYMBOL_NAME("FLOOR"), "MK_CL_floor" },
  { SYMBOL_NAME("FMAKUNBOUND"), "MK_CL_fmakunbound" },
  { SYMBOL_NAME("FORCE-OUTPUT"), "MK_CL_force_output" },
  { SYMBOL_NAME("FORMAT"), "MK_CL_format" },
  { SYMBOL_NAME("FOURTH"), "MK_CL_fourth" },
  { SYMBOL_NAME("FRESH-LINE"), "MK_CL_fresh_line" },
  { SYMBOL_NAME("FROUND"), "MK_CL_fround" },
  { SYMBOL_NAME("FTRUNCATE"), "MK_CL_ftruncate" },
  { SYMBOL_NAME("FUNCALL"), "MK_CL_funcall" },
  { SYMBOL_NAME("FUNCTION"), "MK_CL_function" },
  { SYMBOL_NAME("FUNCTION-LAMBDA-EXPRESSION"), "MK_CL_function_lambda_expression" },
  { SYMBOL_NAME("FUNCTIONP"), "MK_CL_functionp" },
  { SYMBOL_NAME("GCD"), "MK_CL_gcd" },
  { SYMBOL_NAME("GENSYM"), "MK_CL_gensym" },
  { SYMBOL_NAME("GENTEMP"), "MK_CL_gentemp" },
  { SYMBOL_NAME("GET"), "MK_CL_get" },
  { SYMBOL_NAME("GET-DECODED-TIME"), "MK_CL_get_decoded_time" },
  { SYMBOL_NAME("GET-DISPATCH-MACRO-CHARACTER"), "MK_CL_get_dispatch_macro_character" },
  { SYMBOL_NAME("GET-INTERNAL-REAL-TIME"), "MK_CL_get_internal_real_time" },
  { SYMBOL_NAME("GET-INTERNAL-RUN-TIME"), "MK_CL_get_internal_run_time" },
  { SYMBOL_NAME("GETF"), "MK_CL_getf" },
  { SYMBOL_NAME("GETHASH"), "MK_CL_gethash" },
  { SYMBOL_NAME("GET-MACRO-CHARACTER"), "MK_CL_get_macro_character" },
  { SYMBOL_NAME("GET-OUTPUT-STREAM-STRING"), "MK_CL_get_output_stream_string" },
  { SYMBOL_NAME("GET-PROPERTIES"), "MK_CL_get_properties" },
  { SYMBOL_NAME("GET-UNIVERSAL-TIME"), "MK_CL_get_universal_time" },
  { SYMBOL_NAME("GRAPHIC-CHAR-P"), "MK_CL_graphic_char_p" },
  { SYMBOL_NAME("GO"), "MK_CL_go" },
  { SYMBOL_NAME("HASH-TABLE"), "MK_CL_hash_table" },
  { SYMBOL_NAME("HASH-TABLE-COUNT"), "MK_CL_hash_table_count" },
  { SYMBOL_NAME("HASH-TABLE-P"), "MK_CL_hash_table_p" },
  { SYMBOL_NAME("HASH-TABLE-REHASH-SIZE"), "MK_CL_hash_table_rehash_size" },
  { SYMBOL_NAME("HASH-TABLE-REHASH-THRESHOLD"), "MK_CL_hash_table_rehash_threshold" },
  { SYMBOL_NAME("HASH-TABLE-SIZE"), "MK_CL_hash_table_size" },
  { SYMBOL_NAME("HASH-TABLE-TEST"), "MK_CL_hash_table_test" },
  { SYMBOL_NAME("HOST-NAMESTRING"), "MK_CL_host_namestring" },
  { SYMBOL_NAME("IDENTITY"), "MK_CL_identity" },
  { SYMBOL_NAME("IF"), "MK_CL_if" },
  { SYMBOL_NAME("IMAGPART"), "MK_CL_imagpart" },
  { SYMBOL_NAME("IMPORT"), "MK_CL_import" },
  { SYMBOL_NAME("INPUT-STREAM-P"), "MK_CL_input_stream_p" },
  { SYMBOL_NAME("INTEGER"), "MK_CL_integer" },
  { SYMBOL_NAME("INTEGER-DECODE-FLOAT"), "MK_CL_integer_decode_float" },
  { SYMBOL_NAME("INTEGER-LENGTH"), "MK_CL_integer_length" },
  { SYMBOL_NAME("INTEGERP"), "MK_CL_integerp" },
  { SYMBOL_NAME("INTERACTIVE-STREAM-P"), "MK_CL_interactive_stream_p" },
  { SYMBOL_NAME("INTERN"), "MK_CL_intern" },
  { SYMBOL_NAME("INTERNAL-TIME-UNITS-PER-SECOND"), "MK_CL_internal_time_units_per_second" },
  { SYMBOL_NAME("INTERSECTION"), "MK_CL_intersection" },
  { SYMBOL_NAME("INVALID-METHOD-ERROR"), "MK_CL_invalid_method_error" },
  { SYMBOL_NAME("ISQRT"), "MK_CL_isqrt" },
  { SYMBOL_NAME("KEYWORD"), "MK_CL_keyword" },
  { SYMBOL_NAME("KEYWORDP"), "MK_CL_keywordp" },
  { SYMBOL_NAME("LABELS"), "MK_CL_labels" },
  { SYMBOL_NAME("LAMBDA"), "MK_CL_lambda" },
  { SYMBOL_NAME("LAMBDA-LIST-KEYWORDS"), "MK_CL_LAMBDA_LIST_KEYWORDS" },
  { SYMBOL_NAME("LAST"), "MK_CL_last" },
  { SYMBOL_NAME("LCM"), "MK_CL_lcm" },
  { SYMBOL_NAME("LDB"), "MK_CL_ldb" },
  { SYMBOL_NAME("LDB-TEST"), "MK_CL_ldb_test" },
  { SYMBOL_NAME("LDIFF"), "MK_CL_ldiff" },
  { SYMBOL_NAME("LEAST-NEGATIVE-DOUBLE-FLOAT"), "MK_CL_LEAST_NEGATIVE_DOUBLE_FLOAT" },
  { SYMBOL_NAME("LEAST-NEGATIVE-LONG-FLOAT"), "MK_CL_LEAST_NEGATIVE_LONG_FLOAT" },
  { SYMBOL_NAME("LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT"), "MK_CL_LEAST_NEGATIVE_NORMALIZED_DOUBLE_FLOAT" },
  { SYMBOL_NAME("LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT"), "MK_CL_LEAST_NEGATIVE_NORMALIZED_LONG_FLOAT" },
  { SYMBOL_NAME("LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT"), "MK_CL_LEAST_NEGATIVE_NORMALIZED_SHORT_FLOAT" },
  { SYMBOL_NAME("LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT"), "MK_CL_LEAST_NEGATIVE_NORMALIZED_SINGLE_FLOAT" },
  { SYMBOL_NAME("LEAST-NEGATIVE-SHORT-FLOAT"), "MK_CL_LEAST_NEGATIVE_SHORT_FLOAT" },
  { SYMBOL_NAME("LEAST-NEGATIVE-SINGLE-FLOAT"), "MK_CL_LEAST_NEGATIVE_SINGLE_FLOAT" },
  { SYMBOL_NAME("LEAST-POSITIVE-DOUBLE-FLOAT"), "MK_CL_LEAST_POSITIVE_DOUBLE_FLOAT" },
  { SYMBOL_NAME("LEAST-POSITIVE-LONG-FLOAT"), "MK_CL_LEAST_POSITIVE_LONG_FLOAT" },
  { SYMBOL_NAME("LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT"), "MK_CL_LEAST_POSITIVE_NORMALIZED_DOUBLE_FLOAT" },
  { SYMBOL_NAME("LEAST-POSITIVE-NORMALIZED-LONG-FLOAT"), "MK_CL_LEAST_POSITIVE_NORMALIZED_LONG_FLOAT" },
  { SYMBOL_NAME("LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT"), "MK_CL_LEAST_POSITIVE_NORMALIZED_SHORT_FLOAT" },
  { SYMBOL_NAME("LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT"), "MK_CL_LEAST_POSITIVE_NORMALIZED_SINGLE_FLOAT" },
  { SYMBOL_NAME("LEAST-POSITIVE-SHORT-FLOAT"), "MK_CL_LEAST_POSITIVE_SHORT_FLOAT" },
  { SYMBOL_NAME("LEAST-POSITIVE-SINGLE-FLOAT"), "MK_CL_LEAST_POSITIVE_SINGLE_FLOAT" },
  { SYMBOL_NAME("LENGTH"), "MK_CL_length" },
  { SYMBOL_NAME("LET"), "MK_CL_let" },
  { SYMBOL_NAME("LET*"), "MK_CL_letX" },
  { SYMBOL_NAME("LIST"), "MK_CL_list" },
  { SYMBOL_NAME("LIST*"), "MK_CL_listX" },
  { SYMBOL_NAME("LISTEN"), "MK_CL_listen" },
  { SYMBOL_NAME("LIST-ALL-PACKAGES"), "MK_CL_list_all_packages" },
  { SYMBOL_NAME("LIST-LENGTH"), "MK_CL_list_length" },
  { SYMBOL_NAME("LISTP"), "MK_CL_listp" },
  { SYMBOL_NAME("LOAD"), "MK_CL_load" },
  { SYMBOL_NAME("LOAD-LOGICAL-PATHNAME-TRANSLATIONS"), "MK_CL_load_logical_pathname_translations" },
  { SYMBOL_NAME("LOAD-TIME-VALUE"), "MK_CL_load_time_value" },
  { SYMBOL_NAME("LOCALLY"), "MK_CL_locally" },
  { SYMBOL_NAME("LOG"), "MK_CL_log" },
  { SYMBOL_NAME("LOGAND"), "MK_CL_logand" },
  { SYMBOL_NAME("LOGANDC1"), "MK_CL_logandc1" },
  { SYMBOL_NAME("LOGANDC2"), "MK_CL_logandc2" },
  { SYMBOL_NAME("LOGBITP"), "MK_CL_logbitp" },
  { SYMBOL_NAME("LOGCOUNT"), "MK_CL_logcount" },
  { SYMBOL_NAME("LOGEQV"), "MK_CL_logeqv" },
  { SYMBOL_NAME("LOGICAL-PATHNAME"), "MK_CL_logical_pathname" },
  { SYMBOL_NAME("LOGICAL-PATHNAME-TRANSLATIONS"), "MK_CL_logical_pathname_translations" },
  { SYMBOL_NAME("LOGIOR"), "MK_CL_logior" },
  { SYMBOL_NAME("LOGNAND"), "MK_CL_lognand" },
  { SYMBOL_NAME("LOGNOR"), "MK_CL_lognor" },
  { SYMBOL_NAME("LOGNOT"), "MK_CL_lognot" },
  { SYMBOL_NAME("LOGORC1"), "MK_CL_logorc1" },
  { SYMBOL_NAME("LOGORC2"), "MK_CL_logorc2" },
  { SYMBOL_NAME("LOGTEST"), "MK_CL_logtest" },
  { SYMBOL_NAME("LOGXOR"), "MK_CL_logxor" },
  { SYMBOL_NAME("LONG-FLOAT"), "MK_CL_long_float" },
  { SYMBOL_NAME("LOWER-CASE-P"), "MK_CL_lower_case_p" },
  { SYMBOL_NAME("MACROEXPAND"), "MK_CL_macroexpand" },
  { SYMBOL_NAME("MACROEXPAND-1"), "MK_CL_macroexpand_1" },
  { SYMBOL_NAME("MACRO-FUNCTION"), "MK_CL_macro_function" },
  { SYMBOL_NAME("MACROLET"), "MK_CL_macrolet" },
  { SYMBOL_NAME("MAKE-ARRAY"), "MK_CL_make_array" },
  { SYMBOL_NAME("MAKE-BROADCAST-STREAM"), "MK_CL_make_broadcast_stream" },
  { SYMBOL_NAME("MAKE-CONCATENATED-STREAM"), "MK_CL_make_concatenated_stream" },
  { SYMBOL_NAME("MAKE-DISPATCH-MACRO-CHARACTER"), "MK_CL_make_dispatch_macro_character" },
  { SYMBOL_NAME("MAKE-ECHO-STREAM"), "MK_CL_make_echo_stream" },
  { SYMBOL_NAME("MAKE-HASH-TABLE"), "MK_CL_make_hash_table" },
  { SYMBOL_NAME("MAKE-LIST"), "MK_CL_make_list" },
  { SYMBOL_NAME("MAKE-PACKAGE"), "MK_CL_make_package" },
  { SYMBOL_NAME("MAKE-PATHNAME"), "MK_CL_make_pathname" },
  { SYMBOL_NAME("MAKE-RANDOM-STATE"), "MK_CL_make_random_state" },
  { SYMBOL_NAME("MAKE-SEQUENCE"), "MK_CL_make_sequence" },
  { SYMBOL_NAME("MAKE-STRING"), "MK_CL_make_string" },
  { SYMBOL_NAME("MAKE-STRING-INPUT-STREAM"), "MK_CL_make_string_input_stream" },
  { SYMBOL_NAME("MAKE-STRING-OUTPUT-STREAM"), "MK_CL_make_string_output_stream" },
  { SYMBOL_NAME("MAKE-SYMBOL"), "MK_CL_make_symbol" },
  { SYMBOL_NAME("MAKE-SYNONYM-STREAM"), "MK_CL_make_synonym_stream" },
  { SYMBOL_NAME("MAKE-TWO-WAY-STREAM"), "MK_CL_make_two_way_stream" },
  { SYMBOL_NAME("MAKUNBOUND"), "MK_CL_makunbound" },
  { SYMBOL_NAME("MAP"), "MK_CL_map" },
  { SYMBOL_NAME("MAPC"), "MK_CL_mapc" },
  { SYMBOL_NAME("MAPCAN"), "MK_CL_mapcan" },
  { SYMBOL_NAME("MAPCAR"), "MK_CL_mapcar" },
  { SYMBOL_NAME("MAPCON"), "MK_CL_mapcon" },
  { SYMBOL_NAME("MAPHASH"), "MK_CL_maphash" },
  { SYMBOL_NAME("MAP-INTO"), "MK_CL_map_into" },
  { SYMBOL_NAME("MAPL"), "MK_CL_mapl" },
  { SYMBOL_NAME("MAPLIST"), "MK_CL_maplist" },
  { SYMBOL_NAME("MASK-FIELD"), "MK_CL_mask_field" },
  { SYMBOL_NAME("MAX"), "MK_CL_max" },
  { SYMBOL_NAME("MEMBER"), "MK_CL_member" },
  { SYMBOL_NAME("MEMBER-IF"), "MK_CL_member_if" },
  { SYMBOL_NAME("MEMBER-IF-NOT"), "MK_CL_member_if_not" },
  { SYMBOL_NAME("MERGE"), "MK_CL_merge" },
  { SYMBOL_NAME("MERGE-PATHNAMES"), "MK_CL_merge_pathnames" },
  { SYMBOL_NAME("METHOD-COMBINATION-ERROR"), "MK_CL_method_combination_error" },
  { SYMBOL_NAME("MIN"), "MK_CL_min" },
  { SYMBOL_NAME("MINUSP"), "MK_CL_minusp" },
  { SYMBOL_NAME("MISMATCH"), "MK_CL_mismatch" },
  { SYMBOL_NAME("MOD"), "MK_CL_mod" },
  { SYMBOL_NAME("MOST-NEGATIVE-DOUBLE-FLOAT"), "MK_CL_MOST_NEGATIVE_DOUBLE_FLOAT" },
  { SYMBOL_NAME("MOST-NEGATIVE-LONG-FLOAT"), "MK_CL_MOST_NEGATIVE_LONG_FLOAT" },
  { SYMBOL_NAME("MOST-NEGATIVE-SHORT-FLOAT"), "MK_CL_MOST_NEGATIVE_SHORT_FLOAT" },
  { SYMBOL_NAME("MOST-NEGATIVE-SINGLE-FLOAT"), "MK_CL_MOST_NEGATIVE_SINGLE_FLOAT" },
  { SYMBOL_NAME("MOST-POSITIVE-DOUBLE-FLOAT"), "MK_CL_MOST_POSITIVE_DOUBLE_FLOAT" },
  { SYMBOL_NAME("MOST-POSITIVE-LONG-FLOAT"), "MK_CL_MOST_POSITIVE_LONG_FLOAT" },
  { SYMBOL_NAME("MOST-POSITIVE-SHORT-FLOAT"), "MK_CL_MOST_POSITIVE_SHORT_FLOAT" },
  { SYMBOL_NAME("MOST-POSITIVE-SINGLE-FLOAT"), "MK_CL_MOST_POSITIVE_SINGLE_FLOAT" },
  { SYMBOL_NAME("MULTIPLE-VALUE-BIND"), "MK_CL_multiple_value_bind" },
  { SYMBOL_NAME("MULTIPLE-VALUE-CALL"), "MK_CL_multiple_value_call" },
  { SYMBOL_NAME("MULTIPLE-VALUE-PROG1"), "MK_CL_multiple_value_prog1" },
  { SYMBOL_NAME("MULTIPLE-VALUE-SETQ"), "MK_CL_multiple_value_setq" },
  { SYMBOL_NAME("NAME-CHAR"), "MK_CL_name_char" },
  { SYMBOL_NAME("NAMESTRING"), "MK_CL_namestring" },
  { SYMBOL_NAME("NBUTLAST"), "MK_CL_nbutlast" },
  { SYMBOL_NAME("NCONC"), "MK_CL_nconc" },
  { SYMBOL_NAME("NINTERSECTION"), "MK_CL_nintersection" },
  { SYMBOL_NAME("NINTH"), "MK_CL_ninth" },
  { SYMBOL_NAME("NOT"), "MK_CL_not" },
  { SYMBOL_NAME("NOTANY"), "MK_CL_notany" },
  { SYMBOL_NAME("NOTEVERY"), "MK_CL_notevery" },
  { SYMBOL_NAME("NRECONC"), "MK_CL_nreconc" },
  { SYMBOL_NAME("NREVERSE"), "MK_CL_nreverse" },
  { SYMBOL_NAME("NSET-DIFFERENCE"), "MK_CL_nset_difference" },
  { SYMBOL_NAME("NSET-EXCLUSIVE-OR"), "MK_CL_nset_exclusive_or" },
  { SYMBOL_NAME("NSTRING-CAPITALIZE"), "MK_CL_nstring_capitalize" },
  { SYMBOL_NAME("NSTRING-DOWNCASE"), "MK_CL_nstring_downcase" },
  { SYMBOL_NAME("NSTRING-UPCASE"), "MK_CL_nstring_upcase" },
  { SYMBOL_NAME("NSUBLIS"), "MK_CL_nsublis" },
  { SYMBOL_NAME("NSUBST"), "MK_CL_nsubst" },
  { SYMBOL_NAME("NSUBST-IF"), "MK_CL_nsubst_if" },
  { SYMBOL_NAME("NSUBST-IF-NOT"), "MK_CL_nsubst_if_not" },
  { SYMBOL_NAME("NSUBSTITUTE"), "MK_CL_nsubstitute" },
  { SYMBOL_NAME("NSUBSTITUTE-IF"), "MK_CL_nsubstitute_if" },
  { SYMBOL_NAME("NSUBSTITUTE-IF-NOT"), "MK_CL_nsubstitute_if_not" },
  { SYMBOL_NAME("NTH"), "MK_CL_nth" },
  { SYMBOL_NAME("NTH-VALUE"), "MK_CL_nth_value" },
  { SYMBOL_NAME("NTHCDR"), "MK_CL_nthcdr" },
  { SYMBOL_NAME("NULL"), "MK_CL_null" },
  { SYMBOL_NAME("NUMBER"), "MK_CL_number" },
  { SYMBOL_NAME("NUMBERP"), "MK_CL_numberp" },
  { SYMBOL_NAME("NUMERATOR"), "MK_CL_numerator" },
  { SYMBOL_NAME("NUNION"), "MK_CL_nunion" },
  { SYMBOL_NAME("ODDP"), "MK_CL_oddp" },
  { SYMBOL_NAME("OPEN"), "MK_CL_open" },
  { SYMBOL_NAME("OPEN-STREAM-P"), "MK_CL_open_stream_p" },
  { SYMBOL_NAME("OR"), "MK_CL_or" },
  { SYMBOL_NAME("OTHERWISE"), "MK_CL_otherwise" },
  { SYMBOL_NAME("OUTPUT-STREAM-P"), "MK_CL_output_stream_p" },
  { SYMBOL_NAME("PACKAGE"), "MK_CL_package" },
  { SYMBOL_NAME("PACKAGE-NAME"), "MK_CL_package_name" },
  { SYMBOL_NAME("PACKAGE-NICKNAMES"), "MK_CL_package_nicknames" },
  { SYMBOL_NAME("PACKAGE-SHADOWING-SYMBOLS"), "MK_CL_package_shadowing_symbols" },
  { SYMBOL_NAME("PACKAGE-USE-LIST"), "MK_CL_package_use_list" },
  { SYMBOL_NAME("PACKAGE-USED-BY-LIST"), "MK_CL_package_used_by_list" },
  { SYMBOL_NAME("PACKAGEP"), "MK_CL_packagep" },
  { SYMBOL_NAME("PAIRLIS"), "MK_CL_pairlis" },
  { SYMBOL_NAME("PARSE-INTEGER"), "MK_CL_parse_integer" },
  { SYMBOL_NAME("PARSE-NAMESTRING"), "MK_CL_parse_namestring" },
  { SYMBOL_NAME("PATHNAME"), "MK_CL_pathname" },
  { SYMBOL_NAME("PATHNAME-DEVICE"), "MK_CL_pathname_device" },
  { SYMBOL_NAME("PATHNAME-DIRECTORY"), "MK_CL_pathname_directory" },
  { SYMBOL_NAME("PATHNAME-HOST"), "MK_CL_pathname_host" },
  { SYMBOL_NAME("PATHNAME-MATCH-P"), "MK_CL_pathname_match_p" },
  { SYMBOL_NAME("PATHNAME-NAME"), "MK_CL_pathname_name" },
  { SYMBOL_NAME("PATHNAMEP"), "MK_CL_pathnamep" },
  { SYMBOL_NAME("PATHNAME-TYPE"), "MK_CL_pathname_type" },
  { SYMBOL_NAME("PATHNAME-VERSION"), "MK_CL_pathname_version" },
  { SYMBOL_NAME("PEEK-CHAR"), "MK_CL_peek_char" },
  { SYMBOL_NAME("PHASE"), "MK_CL_phase" },
  { SYMBOL_NAME("PI"), "MK_CL_pi" },
  { SYMBOL_NAME("PLUSP"), "MK_CL_plusp" },
  { SYMBOL_NAME("POSITION"), "MK_CL_position" },
  { SYMBOL_NAME("POSITION-IF"), "MK_CL_position_if" },
  { SYMBOL_NAME("POSITION-IF-NOT"), "MK_CL_position_if_not" },
  { SYMBOL_NAME("PPRINT"), "MK_CL_pprint" },
  { SYMBOL_NAME("PPRINT-DISPATCH"), "MK_CL_pprint_dispatch" },
  { SYMBOL_NAME("PPRINT-FILL"), "MK_CL_pprint_fill" },
  { SYMBOL_NAME("PPRINT-INDENT"), "MK_CL_pprint_indent" },
  { SYMBOL_NAME("PPRINT-LINEAR"), "MK_CL_pprint_linear" },
  { SYMBOL_NAME("PPRINT-NEWLINE"), "MK_CL_pprint_newline" },
  { SYMBOL_NAME("PPRINT-TAB"), "MK_CL_pprint_tab" },
  { SYMBOL_NAME("PPRINT-TABULAR"), "MK_CL_pprint_tabular" },
  { SYMBOL_NAME("PRIN1"), "MK_CL_prin1" },
  { SYMBOL_NAME("PRIN1-TO-STRING"), "MK_CL_prin1_to_string" },
  { SYMBOL_NAME("PRINC"), "MK_CL_princ" },
  { SYMBOL_NAME("PRINC-TO-STRING"), "MK_CL_princ_to_string" },
  { SYMBOL_NAME("PRINT"), "MK_CL_print" },
  { SYMBOL_NAME("PRINT-NOT-READABLE"), "MK_CL_print_not_readable" },
  { SYMBOL_NAME("PRINT-OBJECT"), "MK_CL_print_object" },
  { SYMBOL_NAME("PROBE-FILE"), "MK_CL_probe_file" },
  { SYMBOL_NAME("PROG1"), "MK_CL_prog1" },
  { SYMBOL_NAME("PROGN"), "MK_CL_progn" },
  { SYMBOL_NAME("PROGV"), "MK_CL_progv" },
  { SYMBOL_NAME("PROVIDE"), "MK_CL_provide" },
  { SYMBOL_NAME("PSETF"), "MK_CL_psetf" },
  { SYMBOL_NAME("PSETQ"), "MK_CL_psetq" },
  { SYMBOL_NAME("QUOTE"), "MK_CL_quote" },
  { SYMBOL_NAME("RANDOM"), "MK_CL_random" },
  { SYMBOL_NAME("RANDOM-STATE"), "MK_CL_random_state" },
  { SYMBOL_NAME("RANDOM-STATE-P"), "MK_CL_random_state_p" },
  { SYMBOL_NAME("RASSOC"), "MK_CL_rassoc" },
  { SYMBOL_NAME("RASSOC-IF"), "MK_CL_rassoc_if" },
  { SYMBOL_NAME("RASSOC-IF-NOT"), "MK_CL_rassoc_if_not" },
  { SYMBOL_NAME("RATIO"), "MK_CL_ratio" },
  { SYMBOL_NAME("RATIONAL"), "MK_CL_rational" },
  { SYMBOL_NAME("RATIONALIZE"), "MK_CL_rationalize" },
  { SYMBOL_NAME("RATIONALP"), "MK_CL_rationalp" },
  { SYMBOL_NAME("READ"), "MK_CL_read" },
  { SYMBOL_NAME("READ-BYTE"), "MK_CL_read_byte" },
  { SYMBOL_NAME("READ-CHAR"), "MK_CL_read_char" },
  { SYMBOL_NAME("READ-CHAR-NO-HANG"), "MK_CL_read_char_no_hang" },
  { SYMBOL_NAME("READ-DELIMITED-LIST"), "MK_CL_read_delimited_list" },
  { SYMBOL_NAME("READ-FROM-STRING"), "MK_CL_read_from_string" },
  { SYMBOL_NAME("READ-LINE"), "MK_CL_read_line" },
  { SYMBOL_NAME("READ-PRESERVING-WHITESPACE"), "MK_CL_read_preserving_whitespace" },
  { SYMBOL_NAME("READ-SEQUENCE"), "MK_CL_read_sequence" },
  { SYMBOL_NAME("READTABLE"), "MK_CL_readtable" },
  { SYMBOL_NAME("READTABLEP"), "MK_CL_readtablep" },
  { SYMBOL_NAME("READTABLE-CASE"), "MK_CL_readtable_case" },
  { SYMBOL_NAME("REAL"), "MK_CL_real" },
  { SYMBOL_NAME("REALP"), "MK_CL_realp" },
  { SYMBOL_NAME("REALPART"), "MK_CL_realpart" },
  { SYMBOL_NAME("REDUCE"), "MK_CL_reduce" },
  { SYMBOL_NAME("REM"), "MK_CL_rem" },
  { SYMBOL_NAME("REMHASH"), "MK_CL_remhash" },
  { SYMBOL_NAME("REMPROP"), "MK_CL_remprop" },
  { SYMBOL_NAME("REMOVE"), "MK_CL_remove" },
  { SYMBOL_NAME("REMOVE-IF"), "MK_CL_remove_if" },
  { SYMBOL_NAME("REMOVE-IF-NOT"), "MK_CL_remove_if_not" },
  { SYMBOL_NAME("REMOVE-DUPLICATES"), "MK_CL_remove_duplicates" },
  { SYMBOL_NAME("RENAME-FILE"), "MK_CL_rename_file" },
  { SYMBOL_NAME("RENAME-PACKAGE"), "MK_CL_rename_package" },
  { SYMBOL_NAME("REPLACE"), "MK_CL_replace" },
  { SYMBOL_NAME("REQUIRE"), "MK_CL_require" },
  { SYMBOL_NAME("REST"), "MK_CL_rest" },
  { SYMBOL_NAME("RETURN"), "MK_CL_return" },
  { SYMBOL_NAME("RETURN-FROM"), "MK_CL_return_from" },
  { SYMBOL_NAME("REVAPPEND"), "MK_CL_revappend" },
  { SYMBOL_NAME("REVERSE"), "MK_CL_reverse" },
  { SYMBOL_NAME("ROUND"), "MK_CL_round" },
  { SYMBOL_NAME("ROW-MAJOR-AREF"), "MK_CL_row_major_aref" },
  { SYMBOL_NAME("RPLACA"), "MK_CL_rplaca" },
  { SYMBOL_NAME("RPLACD"), "MK_CL_rplacd" },
  { SYMBOL_NAME("SATISFIES"), "MK_CL_satisfies" },
  { SYMBOL_NAME("SBIT"), "MK_CL_sbit" },
  { SYMBOL_NAME("SCALE-FLOAT"), "MK_CL_scale_float" },
  { SYMBOL_NAME("SCHAR"), "MK_CL_schar" },
  { SYMBOL_NAME("SEARCH"), "MK_CL_search" },
  { SYMBOL_NAME("SECOND"), "MK_CL_second" },
  { SYMBOL_NAME("SEQUENCE"), "MK_CL_sequence" },
  { SYMBOL_NAME("SET"), "MK_CL_set" },
  { SYMBOL_NAME("SET-DIFFERENCE"), "MK_CL_set_difference" },
  { SYMBOL_NAME("SET-DISPATCH-MACRO-CHARACTER"), "MK_CL_set_dispatch_macro_character" },
  { SYMBOL_NAME("SET-EXCLUSIVE-OR"), "MK_CL_set_exclusive_or" },
  { SYMBOL_NAME("SET-PPRINT-DISPATCH"), "MK_CL_set_pprint_dispatch" },
  { SYMBOL_NAME("SETF"), "MK_CL_setf" },
  { SYMBOL_NAME("SET-MACRO-CHARACTER"), "MK_CL_set_macro_character" },
  { SYMBOL_NAME("SETQ"), "MK_CL_setq" },
  { SYMBOL_NAME("SET-SYNTAX-FROM-CHAR"), "MK_CL_set_syntax_from_char" },
  { SYMBOL_NAME("SEVENTH"), "MK_CL_seventh" },
  { SYMBOL_NAME("SHADOW"), "MK_CL_shadow" },
  { SYMBOL_NAME("SHADOWING-IMPORT"), "MK_CL_shadowing_import" },
  { SYMBOL_NAME("SHORT-FLOAT"), "MK_CL_short_float" },
  { SYMBOL_NAME("SIGNED-BYTE"), "MK_CL_signed_byte" },
  { SYMBOL_NAME("SIGNUM"), "MK_CL_signum" },
  { SYMBOL_NAME("SIMPLE-ARRAY"), "MK_CL_simple_array" },
  { SYMBOL_NAME("SIMPLE-BASE-STRING"), "MK_CL_simple_base_string" },
  { SYMBOL_NAME("SIMPLE-BIT-VECTOR"), "MK_CL_simple_bit_vector" },
  { SYMBOL_NAME("SIMPLE-BIT-VECTOR-P"), "MK_CL_simple_bit_vector_p" },
  { SYMBOL_NAME("SIMPLE-STRING"), "MK_CL_simple_string" },
  { SYMBOL_NAME("SIMPLE-STRING-P"), "MK_CL_simple_string_p" },
  { SYMBOL_NAME("SIMPLE-TYPE-ERROR"), "MK_CL_simple_type_error" },
  { SYMBOL_NAME("SIMPLE-VECTOR"), "MK_CL_simple_vector" },
  { SYMBOL_NAME("SIMPLE-VECTOR-P"), "MK_CL_simple_vector_p" },
  { SYMBOL_NAME("SIN"), "MK_CL_sin" },
  { SYMBOL_NAME("SINGLE-FLOAT"), "MK_CL_single_float" },
  { SYMBOL_NAME("SINH"), "MK_CL_sinh" },
  { SYMBOL_NAME("SIXTH"), "MK_CL_sixth" },
  { SYMBOL_NAME("SLEEP"), "MK_CL_sleep" },
  { SYMBOL_NAME("SLOT-VALUE"), "MK_CL_slot_value" },
  { SYMBOL_NAME("SOME"), "MK_CL_some" },
  { SYMBOL_NAME("SORT"), "MK_CL_sort" },
  { SYMBOL_NAME("SPECIAL"), "MK_CL_special" },
  { SYMBOL_NAME("SPECIAL-OPERATOR-P"), "MK_CL_special_operator_p" },
  { SYMBOL_NAME("SQRT"), "MK_CL_sqrt" },
  { SYMBOL_NAME("STABLE-SORT"), "MK_CL_stable_sort" },
  { SYMBOL_NAME("STANDARD-CHAR"), "MK_CL_standard_char" },
  { SYMBOL_NAME("STANDARD-CHAR-P"), "MK_CL_standard_char_p" },
  { SYMBOL_NAME("STANDARD-OBJECT"), "MK_CL_standard_object" },
  { SYMBOL_NAME("STREAM"), "MK_CL_stream" },
  { SYMBOL_NAME("STREAMP"), "MK_CL_streamp" },
  { SYMBOL_NAME("STREAM-ELEMENT-TYPE"), "MK_CL_stream_element_type" },
  { SYMBOL_NAME("STREAM-EXTERNAL-FORMAT"), "MK_CL_stream_external_format" },
  { SYMBOL_NAME("STRING"), "MK_CL_string" },
  { SYMBOL_NAME("STRINGP"), "MK_CL_stringp" },
  { SYMBOL_NAME("STRING/="), "MK_CL_stringNE" },
  { SYMBOL_NAME("STRING<"), "MK_CL_stringL" },
  { SYMBOL_NAME("STRING<="), "MK_CL_stringLE" },
  { SYMBOL_NAME("STRING="), "MK_CL_stringE" },
  { SYMBOL_NAME("STRING>"), "MK_CL_stringG" },
  { SYMBOL_NAME("STRING>="), "MK_CL_stringGE" },
  { SYMBOL_NAME("STRING-CAPITALIZE"), "MK_CL_string_capitalize" },
  { SYMBOL_NAME("STRING-DOWNCASE"), "MK_CL_string_downcase" },
  { SYMBOL_NAME("STRING-EQUAL"), "MK_CL_string_equal" },
  { SYMBOL_NAME("STRING-GREATERP"), "MK_CL_string_greaterp" },
  { SYMBOL_NAME("STRING-LEFT-TRIM"), "MK_CL_string_left_trim" },
  { SYMBOL_NAME("STRING-LESSP"), "MK_CL_string_lessp" },
  { SYMBOL_NAME("STRING-NOT-EQUAL"), "MK_CL_string_not_equal" },
  { SYMBOL_NAME("STRING-NOT-GREATERP"), "MK_CL_string_not_greaterp" },
  { SYMBOL_NAME("STRING-NOT-LESSP"), "MK_CL_string_not_lessp" },
  { SYMBOL_NAME("STRING-RIGHT-TRIM"), "MK_CL_string_right_trim" },
  { SYMBOL_NAME("STRING-TRIM"), "MK_CL_string_trim" },
  { SYMBOL_NAME("STRING-STREAM"), "MK_CL_string_stream" },
  { SYMBOL_NAME("STRING-UPCASE"), "MK_CL_string_upcase" },
  { SYMBOL_NAME("STRUCTURE"), "MK_CL_structure" },
  { SYMBOL_NAME("STRUCTURE-OBJECT"), "MK_CL_structure_object" },
  { SYMBOL_NAME("SUBLIS"), "MK_CL_sublis" },
  { SYMBOL_NAME("SUBSEQ"), "MK_CL_subseq" },
  { SYMBOL_NAME("SUBSETP"), "MK_CL_subsetp" },
  { SYMBOL_NAME("SUBST"), "MK_CL_subst" },
  { SYMBOL_NAME("SUBST-IF"), "MK_CL_subst_if" },
  { SYMBOL_NAME("SUBST-IF-NOT"), "MK_CL_subst_if_not" },
  { SYMBOL_NAME("SUBSTITUTE"), "MK_CL_substitute" },
  { SYMBOL_NAME("SUBSTITUTE-IF"), "MK_CL_substitute_if" },
  { SYMBOL_NAME("SUBSTITUTE-IF-NOT"), "MK_CL_substitute_if_not" },
  { SYMBOL_NAME("SUBTYPEP"), "MK_CL_subtypep" },
  { SYMBOL_NAME("SVREF"), "MK_CL_svref" },
  { SYMBOL_NAME("SXHASH"), "MK_CL_sxhash" },
  { SYMBOL_NAME("SYMBOL"), "MK_CL_symbol" },
  { SYMBOL_NAME("SYMBOL-FUNCTION"), "MK_CL_symbol_function" },
  { SYMBOL_NAME("SYMBOL-MACROLET"), "MK_CL_symbol_macrolet" },
  { SYMBOL_NAME("SYMBOL-NAME"), "MK_CL_symbol_name" },
  { SYMBOL_NAME("SYMBOL-PACKAGE"), "MK_CL_symbol_package" },
  { SYMBOL_NAME("SYMBOL-PLIST"), "MK_CL_symbol_plist" },
  { SYMBOL_NAME("SYMBOL-VALUE"), "MK_CL_symbol_value" },
  { SYMBOL_NAME("SYMBOLP"), "MK_CL_symbolp" },
  { SYMBOL_NAME("SYNONYM-STREAM"), "MK_CL_synonym_stream" },
  { SYMBOL_NAME("SYNONYM-STREAM-SYMBOL"), "MK_CL_synonym_stream_symbol" },
  { SYMBOL_NAME("TAGBODY"), "MK_CL_tagbody" },
  { SYMBOL_NAME("TAILP"), "MK_CL_tailp" },
  { SYMBOL_NAME("TAN"), "MK_CL_tan" },
  { SYMBOL_NAME("TANH"), "MK_CL_tanh" },
  { SYMBOL_NAME("TENTH"), "MK_CL_tenth" },
  { SYMBOL_NAME("TERPRI"), "MK_CL_terpri" },
  { SYMBOL_NAME("THIRD"), "MK_CL_third" },
  { SYMBOL_NAME("THROW"), "MK_CL_throw" },
  { SYMBOL_NAME("TRANSLATE-PATHNAME"), "MK_CL_translate_pathname" },
  { SYMBOL_NAME("TRANSLATE-LOGICAL-PATHNAME"), "MK_CL_translate_logical_pathname" },
  { SYMBOL_NAME("TREE-EQUAL"), "MK_CL_tree_equal" },
  { SYMBOL_NAME("TRUENAME"), "MK_CL_truename" },
  { SYMBOL_NAME("TRUNCATE"), "MK_CL_truncate" },
  { SYMBOL_NAME("TWO-WAY-STREAM"), "MK_CL_two_way_stream" },
  { SYMBOL_NAME("TWO-WAY-STREAM-INPUT-STREAM"), "MK_CL_two_way_stream_input_stream" },
  { SYMBOL_NAME("TWO-WAY-STREAM-OUTPUT-STREAM"), "MK_CL_two_way_stream_output_stream" },
  { SYMBOL_NAME("TYPE-ERROR"), "MK_CL_type_error" },
  { SYMBOL_NAME("TYPE-OF"), "MK_CL_type_of" },
  { SYMBOL_NAME("TYPEP"), "MK_CL_typep" },
  { SYMBOL_NAME("UNBOUND-SLOT"), "MK_CL_unbound_slot" },
  { SYMBOL_NAME("UNBOUND-VARIABLE"), "MK_CL_unbound_variable" },
  { SYMBOL_NAME("UNDEFINED-FUNCTION"), "MK_CL_undefined_function" },
  { SYMBOL_NAME("UNEXPORT"), "MK_CL_unexport" },
  { SYMBOL_NAME("UNINTERN"), "MK_CL_unintern" },
  { SYMBOL_NAME("UNION"), "MK_CL_union" },
  { SYMBOL_NAME("UNLESS"), "MK_CL_unless" },
  { SYMBOL_NAME("UNREAD-CHAR"), "MK_CL_unread_char" },
  { SYMBOL_NAME("UNSIGNED-BYTE"), "MK_CL_unsigned_byte" },
  { SYMBOL_NAME("UNUSE-PACKAGE"), "MK_CL_unuse_package" },
  { SYMBOL_NAME("UNWIND-PROTECT"), "MK_CL_unwind_protect" },
  { SYMBOL_NAME("UPGRADED-ARRAY-ELEMENT-TYPE"), "MK_CL_upgraded_array_element_type" },
  { SYMBOL_NAME("UPGRADED-COMPLEX-PART-TYPE"), "MK_CL_upgraded_complex_part_type" },
  { SYMBOL_NAME("UPPER-CASE-P"), "MK_CL_upper_case_p" },
  { SYMBOL_NAME("USE-PACKAGE"), "MK_CL_use_package" },
  { SYMBOL_NAME("USER-HOMEDIR-PATHNAME"), "MK_CL_user_homedir_pathname" },
  { SYMBOL_NAME("VALUES"), "MK_CL_values" },
  { SYMBOL_NAME("VALUES-LIST"), "MK_CL_values_list" },
  { SYMBOL_NAME("VECTOR"), "MK_CL_vector" },
  { SYMBOL_NAME("VECTORP"), "MK_CL_vectorp" },
  { SYMBOL_NAME("VECTOR-POP"), "MK_CL_vector_pop" },
  { SYMBOL_NAME("VECTOR-PUSH"), "MK_CL_vector_push" },
  { SYMBOL_NAME("VECTOR-PUSH-EXTEND"), "MK_CL_vector_push_extend" },
  { SYMBOL_NAME("WHEN"), "MK_CL_when" },
  { SYMBOL_NAME("WILD-PATHNAME-P"), "MK_CL_wild_pathname_p" },
  { SYMBOL_NAME("WRITE"), "MK_CL_write" },
  { SYMBOL_NAME("WRITE-BYTE"), "MK_CL_write_byte" },
  { SYMBOL_NAME("WRITE-CHAR"), "MK_CL_write_char" },
  { SYMBOL_NAME("WRITE-LINE"), "MK_CL_write_line" },
  { SYMBOL_NAME("WRITE-SEQUENCE"), "MK_CL_write_sequence" },
  { SYMBOL_NAME("WRITE-STRING"), "MK_CL_write_string" },
  { SYMBOL_NAME("WRITE-TO-STRING"), "MK_CL_write_to_string" },
  { SYMBOL_NAME("Y-OR-N-P"), "MK_CL_y_or_n_p" },
  { SYMBOL_NAME("YES-OR-NO-P"), "MK_CL_yes_or_no_p" },
  { SYMBOL_NAME("ZEROP"), "MK_CL_zerop" },
  { SYMBOL_NAME("&ALLOW-OTHER-KEYS"), "MK_CL_LKEY_allow_other_keys" },
  { SYMBOL_NAME("&AUX"), "MK_CL_LKEY_aux" },
  { SYMBOL_NAME("&BODY"), "MK_CL_LKEY_body" },
  { SYMBOL_NAME("&ENVIRONMENT"), "MK_CL_LKEY_environment" },
  { SYMBOL_NAME("&KEY"), "MK_CL_LKEY_key" },
  { SYMBOL_NAME("&OPTIONAL"), "MK_CL_LKEY_optional" },
  { SYMBOL_NAME("&REST"), "MK_CL_LKEY_rest" },
  { SYMBOL_NAME("&WHOLE"), "MK_CL_LKEY_whole" },
  { SYMBOL_NAME("*BREAK-ON-SIGNALS*"), "MK_CL_DYNVAR_break_on_signals" },
  { SYMBOL_NAME("*COMPILE-PRINT*"), "MK_CL_DYNVAR_compile_print" },
  { SYMBOL_NAME("*COMPILE-VERBOSE*"), "MK_CL_DYNVAR_compile_verbose" },
  { SYMBOL_NAME("*DEBUGGER-HOOK*"), "MK_CL_DYNVAR_debugger_hook" },
  { SYMBOL_NAME("*DEBUG-IO*"), "MK_CL_DYNVAR_debug_io" },
  { SYMBOL_NAME("*DEFAULT-PATHNAME-DEFAULTS*"), "MK_CL_DYNVAR_default_pathname_defaults" },
  { SYMBOL_NAME("*ERROR-OUTPUT*"), "MK_CL_DYNVAR_error_output" },
  { SYMBOL_NAME("*FEATURES*"), "MK_CL_DYNVAR_features" },
  { SYMBOL_NAME("*GENSYM-COUNTER*"), "MK_CL_DYNVAR_gensym_counter" },
  { SYMBOL_NAME("*LOAD-PATHNAME*"), "MK_CL_DYNVAR_load_pathname" },
  { SYMBOL_NAME("*LOAD-PRINT*"), "MK_CL_DYNVAR_load_print" },
  { SYMBOL_NAME("*LOAD-TRUENAME*"), "MK_CL_DYNVAR_load_truename" },
  { SYMBOL_NAME("*LOAD-VERBOSE*"), "MK_CL_DYNVAR_load_verbose" },
  { SYMBOL_NAME("*MACROEXPAND-HOOK*"), "MK_CL_DYNVAR_macroexpand_hook" },
  { SYMBOL_NAME("*PACKAGE*"), "MK_CL_DYNVAR_package" },
  { SYMBOL_NAME("*PRINT-ARRAY*"), "MK_CL_DYNVAR_print_array" },
  { SYMBOL_NAME("*PRINT-BASE*"), "MK_CL_DYNVAR_print_base" },
  { SYMBOL_NAME("*PRINT-CASE*"), "MK_CL_DYNVAR_print_case" },
  { SYMBOL_NAME("*PRINT-CIRCLE*"), "MK_CL_DYNVAR_print_circle" },
  { SYMBOL_NAME("*PRINT-ESCAPE*"), "MK_CL_DYNVAR_print_escape" },
  { SYMBOL_NAME("*PRINT-GENSYM*"), "MK_CL_DYNVAR_print_gensym" },
  { SYMBOL_NAME("*PRINT-LENGTH*"), "MK_CL_DYNVAR_print_length" },
  { SYMBOL_NAME("*PRINT-LEVEL*"), "MK_CL_DYNVAR_print_level" },
  { SYMBOL_NAME("*PRINT-LINES*"), "MK_CL_DYNVAR_print_lines" },
  { SYMBOL_NAME("*PRINT-MISER-WIDTH*"), "MK_CL_DYNVAR_print_miser_width" },
  { SYMBOL_NAME("*PRINT-PPRINT-DISPATCH*"), "MK_CL_DYNVAR_print_pprint_dispatch" },
  { SYMBOL_NAME("*PRINT-PRETTY*"), "MK_CL_DYNVAR_print_pretty" },
  { SYMBOL_NAME("*PRINT-RADIX*"), "MK_CL_DYNVAR_print_radix" },
  { SYMBOL_NAME("*PRINT-READABLY*"), "MK_CL_DYNVAR_print_readably" },
  { SYMBOL_NAME("*PRINT-RIGHT-MARGIN*"), "MK_CL_DYNVAR_print_right_margin" },
  { SYMBOL_NAME("*QUERY-IO*"), "MK_CL_DYNVAR_query_io" },
  { SYMBOL_NAME("*RANDOM-STATE*"), "MK_CL_DYNVAR_random_state" },
  { SYMBOL_NAME("*READ-BASE*"), "MK_CL_DYNVAR_read_base" },
  { SYMBOL_NAME("*READ-DEFAULT-FLOAT-FORMAT*"), "MK_CL_DYNVAR_read_default_float_format" },
  { SYMBOL_NAME("*READ-EVAL*"), "MK_CL_DYNVAR_read_eval" },
  { SYMBOL_NAME("*READ-SUPPRESS*"), "MK_CL_DYNVAR_read_suppress" },
  { SYMBOL_NAME("*READTABLE*"), "MK_CL_DYNVAR_readtable" },
  { SYMBOL_NAME("*STANDARD-INPUT*"), "MK_CL_DYNVAR_standard_input" },
  { SYMBOL_NAME("*STANDARD-OUTPUT*"), "MK_CL_DYNVAR_standard_output" },
  { SYMBOL_NAME("*TERMINAL-IO*"), "MK_CL_DYNVAR_terminal_io" },
  { SYMBOL_NAME("*TRACE-OUTPUT*"), "MK_CL_DYNVAR_trace_output" },
  { SYMBOL_NAME("1+"), "MK_CL_1P" },
  { SYMBOL_NAME("1-"), "MK_CL_1M" },
};



const struct mkcl_symbol blank_symbol = BLANK_SYMBOL_INITIALIZER;


void init_cl_package(void)
{
  long i;

  {
    struct mkcl_symbol * sym = &mkcl_cl_external_symbols[0];
    const struct mkcl_base_string * name = &mkcl_cl_external_symbol_names[0];
    const mkcl_hash_value hashed_name = hash_base_string(name->self, name->fillp, 0);

    *sym = blank_symbol;

    sym->name = (mkcl_object) name;
    sym->hashed_name = hashed_name;
    sym->hpack = (mkcl_object) &mkcl_package_cl;
    sym->value = (mkcl_object) &mkcl_cl_declare_constants[0].value_denotator;
    sym->stype = mkcl_stp_constant;

    struct mkcl_hashtable_entry * entry = &(external_entries[0]);
    entry->hashed_key = hashed_name;
    entry->value = mk_cl_Cnil;
    entry->key = sym->name;
    entry->next = NULL;

    add_entry_to_hash(hashed_name, &external_ht, entry);
  }

  for (i = 1; i < MKCL_NB_ELEMS(mkcl_cl_external_symbol_names); i++) /* we skip NIL since we initialized it just above. */
    {
      struct mkcl_symbol * sym = &mkcl_cl_external_symbols[i];
      const struct mkcl_base_string * name = &mkcl_cl_external_symbol_names[i];
      const mkcl_hash_value hashed_name = hash_base_string(name->self, name->fillp, 0);

      *sym = blank_symbol;

      sym->name = (mkcl_object) name;
      sym->hashed_name = hashed_name;
      sym->hpack = (mkcl_object) &mkcl_package_cl;

      struct mkcl_hashtable_entry * entry = &(external_entries[i]);
      entry->hashed_key = hashed_name;
      entry->value = (mkcl_object) sym;
      entry->key = sym->name;
      entry->next = NULL;
      
      add_entry_to_hash(hashed_name, &external_ht, entry);
    }
  
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_cl_internal_symbol_names); i++)
    {
      struct mkcl_symbol * sym = &mkcl_cl_internal_symbols[i];
      const struct mkcl_base_string * name = &mkcl_cl_internal_symbol_names[i];
      const mkcl_hash_value hashed_name = hash_base_string(name->self, name->fillp, 0);

      *sym = blank_symbol;

      sym->name = (mkcl_object) name;
      sym->hashed_name = hashed_name;
      sym->hpack = (mkcl_object) &mkcl_package_cl;

      struct mkcl_hashtable_entry * entry = &(internal_entries[i]);
      entry->hashed_key = hashed_name;
      entry->value = (mkcl_object) sym;
      entry->key = sym->name;
      entry->next = NULL;

      add_entry_to_hash(hashed_name, &internal_ht, entry);
    }

  for (i = 0; i < MKCL_NB_ELEMS(mkcl_cl_declare_specials); i++)
    {
      int intern_flag;
      struct mkcl_base_string * const name = &mkcl_cl_declare_specials[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_cl, NULL);

      if (sym)
	{
	  sym->value = (mkcl_object) &mkcl_cl_declare_specials[i].value_denotator;
	  sym->stype = mkcl_stp_special;
	}
      else
	{
	  printf("special variable not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 1; i < MKCL_NB_ELEMS(mkcl_cl_declare_constants); i++) /* we skip NIL since we initialized it just above. */
    {
      int intern_flag;
      struct mkcl_base_string * const name = &mkcl_cl_declare_constants[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_cl, NULL);

      if (sym)
	{
	  sym->value = (mkcl_object) &mkcl_cl_declare_constants[i].value_denotator;
	  sym->stype = mkcl_stp_constant;
	}
      else
	{
	  printf("constant variable not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_cl_declare_lisp_functions); i++)
    {
      int intern_flag;
      struct mkcl_base_string * name = &mkcl_cl_declare_lisp_functions[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_cl, NULL);

      if (sym)
	{
	  sym->gfdef = (mkcl_object) &mkcl_cl_declare_lisp_functions[i].function_object_denotator;
	}
      else
	{
	  printf("function not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_cl_declare_macros); i++)
    {
      int intern_flag;
      struct mkcl_base_string * name = &mkcl_cl_declare_macros[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_cl, NULL);

      if (sym)
	{
	  sym->stype |= mkcl_stp_macro;
	  sym->gfdef = (mkcl_object) &mkcl_cl_declare_macros[i].function_object_denotator;
	}
      else
	{
	  printf("function not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_cl_declare_special_operators); i++)
    {
      int intern_flag;
      struct mkcl_base_string * name = &mkcl_cl_declare_special_operators[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_cl, NULL);

      if (sym)
	{
	  sym->stype |= mkcl_stp_special_form;
	  sym->gfdef = (mkcl_object) &mkcl_cl_declare_special_operators[i].function_object_denotator;
	}
      else
	{
	  printf("function not found %s\n", name->self);
	  exit(1);
	}
    }
}



mkcl_index cl_internal_symbol_index(struct mkcl_symbol * sym)
{
  return (sym - mkcl_cl_internal_symbols);
}

mkcl_index cl_external_symbol_index(struct mkcl_symbol * sym)
{
  return (sym - mkcl_cl_external_symbols);
}

mkcl_index cl_internal_entry_index(struct mkcl_hashtable_entry * entry)
{
  return (entry - internal_entries);
}

mkcl_index cl_external_entry_index(struct mkcl_hashtable_entry * entry)
{
  return (entry - external_entries);
}



void print_cl_internal_symbol_C_names(void)
{
  mkcl_index i;

  printf("\nstatic const struct mkcl_base_string cl_internal_C_names[] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      printf("SYMBOL_NAME(\"mkcl_cl_internal_symbols[%lu]\"),\n", i);
    }
  printf("};\n");
}

void print_cl_external_symbol_C_names(void)
{
  mkcl_index i;

  printf("\nstatic const struct mkcl_base_string cl_external_C_names[] = {\n");
  for (i = 0; i < external_count; i++)
    {
      printf("SYMBOL_NAME(\"mkcl_cl_external_symbols[%lu]\"),\n", i);
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

void print_cl_internal_symbol_initializers(void)
{
  mkcl_index i;

  printf("\nstruct mkcl_symbol mkcl_cl_internal_symbols[] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      struct mkcl_symbol * sym = &mkcl_cl_internal_symbols[i];
      
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
      printf("(mkcl_object) &mkcl_cl_internal_symbol_names[%lu], ", i); /* name */
      printf("(mkcl_object) &mkcl_package_cl, "); /* hpack */
      printf("mk_cl_Cnil, "); /* properly_named_class */
      printf("mk_cl_Cnil, "); /* sys_plist */
      printf("MKCL_NOT_A_SPECIAL_INDEX, "); /* special_index */
      printf("%luUL, ", sym->hashed_name); /* hashed_name */
      printf("(mkcl_object) &cl_internal_C_names[%lu], ", i); /* C_name */
      printf("NULL, "); /* _C_name */
      printf("NULL, "); /* _name */
      
      printf("}, /* %lu %s */\n", i, sym->name->base_string.self);
    }
  printf("};\n");
}

void print_cl_external_symbol_initializers(void)
{
  mkcl_index i;

  printf("\nstruct mkcl_symbol mkcl_cl_external_symbols[] = {\n");
  for (i = 0; i < external_count; i++)
    {
      struct mkcl_symbol * sym = &mkcl_cl_external_symbols[i];
      
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
      printf("(mkcl_object) &mkcl_cl_external_symbol_names[%lu], ", i); /* name */
      printf("(mkcl_object) &mkcl_package_cl, "); /* hpack */
      printf("mk_cl_Cnil, "); /* properly_named_class */
      printf("mk_cl_Cnil, "); /* sys_plist */
      printf("MKCL_NOT_A_SPECIAL_INDEX, "); /* special_index */
      printf("%luUL, ", sym->hashed_name); /* hashed_name */
      printf("(mkcl_object) &cl_external_C_names[%lu], ", i); /* C_name */
      printf("NULL, "); /* _C_name */
      printf("NULL, "); /* _name */
      
      printf("}, /* %lu %s */\n", i, sym->name->base_string.self);
    }
  printf("};\n");
}


void print_cl_internal_hashtable_entry_initializers(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry internal_entries[internal_count] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      struct mkcl_hashtable_entry * entry = &internal_entries[i];
      

      printf("{ ");
      if (entry->next)
	printf("&internal_entries[%lu], ", cl_internal_entry_index(entry->next));
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
	  mkcl_index sym_index = cl_internal_symbol_index((struct mkcl_symbol *) entry->value);

	  printf("(mkcl_object) &mkcl_cl_internal_symbol_names[%lu], ", sym_index);
	  printf("%luUL, ", entry->hashed_key);
	  printf("(mkcl_object) &mkcl_cl_internal_symbols[%lu], ", sym_index);
	}
      printf("}, \n");
    }
  printf("};\n");
}

void print_cl_internal_hashtable_vector_initializer(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry * internal_vector[internal_size] = {\n");
  for (i = 0; i < internal_size; i++)
    {
      struct mkcl_hashtable_entry * entry = internal_vector[i];

      if (entry)
	printf("&internal_entries[%lu], \n", cl_internal_entry_index(entry));
      else
	printf("NULL, \n");

    }
  printf("};\n");
}

void print_cl_external_hashtable_entry_initializers(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry external_entries[external_count] = {\n");
  for (i = 0; i < external_count; i++)
    {
      struct mkcl_hashtable_entry * entry = &external_entries[i];
      

      printf("{ ");
      if (entry->next)
	printf("&external_entries[%lu], ", cl_external_entry_index(entry->next));
      else
	printf("NULL, ");
      if (mkcl_Null(entry->value))
	{
	  if (i == 0) /* this is the special case of NIL. */
	    {
	      printf("mk_cl_Cnil, ");
	      printf("%luUL, ", entry->hashed_key);
	      printf("(mkcl_object) &mkcl_cl_external_symbols[0], ");
	    }
	  else
	    {
	      printf("mk_cl_Cnil, ");
	      printf("0, ");
	      printf("mk_cl_Cnil, ");
	    }
	}
      else
	{
	  mkcl_index sym_index = cl_external_symbol_index((struct mkcl_symbol *) entry->value);

	  printf("(mkcl_object) &mkcl_cl_external_symbol_names[%lu], ", sym_index);
	  printf("%luUL, ", entry->hashed_key);
	  printf("(mkcl_object) &mkcl_cl_external_symbols[%lu], ", sym_index);
	}
      printf("}, \n");
    }
  printf("};\n");
}

void print_cl_external_hashtable_vector_initializer(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry * external_vector[external_size] = {\n");
  for (i = 0; i < external_size; i++)
    {
      struct mkcl_hashtable_entry * entry = external_vector[i];

      if (entry)
	printf("&external_entries[%lu], \n", cl_external_entry_index(entry));
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

void print_cl_package(void)
{
  print_copyright();
  print_cl_internal_symbol_C_names();
  print_cl_internal_symbol_initializers();
  print_cl_external_symbol_C_names();
  print_cl_external_symbol_initializers();
  print_cl_internal_hashtable_entry_initializers();
  print_cl_internal_hashtable_vector_initializer();
  print_cl_external_hashtable_entry_initializers();
  print_cl_external_hashtable_vector_initializer();
}

void expose_cl_symbol(const struct exposed_symbol * exposed_symbol)
{
  bool internalp;
  struct mkcl_symbol * sym = find_symbol(&exposed_symbol->symbol_name, &mkcl_package_cl, &internalp);

  if ( sym == NULL ) { printf("Exposed symbol not found: %s\n", exposed_symbol->symbol_name.self); exit(1); }

  printf("#define %s ", exposed_symbol->exposition);
  if (internalp)
    printf("&mkcl_cl_internal_symbols[%lu]\n", cl_internal_symbol_index(sym));
  else
    printf("&mkcl_cl_external_symbols[%lu]\n", cl_external_symbol_index(sym)); 
}

void expose_cl_package(void)
{
  mkcl_index i;

  print_copyright();
  for (i = 0; i < MKCL_NB_ELEMS(exposed_symbols); i++)
    {
      expose_cl_symbol(&exposed_symbols[i]);
    }
}

int main(int argc, char * argv[])
{
  char * program_name = basename(strdup(argv[0]));
  init_cl_package();
  

  if ( strcmp(program_name, "build_package_CL") == 0 )
    print_cl_package();
  else if ( strcmp(program_name, "expose_package_CL") == 0 )
    expose_cl_package();

  return 0;
}

