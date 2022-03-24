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


#include "MKCL_package.h"


struct mkcl_function_declaration {
  struct mkcl_base_string symbol_name;
  struct mkcl_base_string function_object_denotator;
};

struct mkcl_function_declaration mkcl_mkcl_declare_lisp_functions[] = {
  {SYMBOL_NAME("ARGC"), FUN_DENOT("&mk_mkcl_argc_cfunobj")}, 
  {SYMBOL_NAME("ARGV"), FUN_DENOT("&mk_mkcl_argv_cfunobj")},
  {SYMBOL_NAME("CHDIR"), FUN_DENOT("&mk_mkcl_chdir_cfunobj")},
  {SYMBOL_NAME("FIXNUMP"), FUN_DENOT("&mk_mkcl_fixnump_cfunobj")},
  {SYMBOL_NAME("GETENV"), FUN_DENOT("&mk_mkcl_getenv_cfunobj")},
  {SYMBOL_NAME("GETCWD"), FUN_DENOT("&mk_mkcl_getcwd_cfunobj")},
  {SYMBOL_NAME("GETPID"), FUN_DENOT("&mk_mkcl_getpid_cfunobj")},
  {SYMBOL_NAME("GETTID"), FUN_DENOT("&mk_mkcl_gettid_cfunobj")},
  {SYMBOL_NAME("LOGICAL-PATHNAME-P"), FUN_DENOT("&mk_mkcl_logical_pathname_p_cfunobj")},
  {SYMBOL_NAME("MKDIR"), FUN_DENOT("&mk_mkcl_mkdir_cfunobj")},
  {SYMBOL_NAME("MKSTEMP"), FUN_DENOT("&mk_mkcl_mkstemp_cfunobj")},
  {SYMBOL_NAME("RMDIR"), FUN_DENOT("&mk_mkcl_rmdir_cfunobj")},
  {SYMBOL_NAME("MAKE-PIPE"), FUN_DENOT("&mk_mkcl_make_pipe_cfunobj")},
  {SYMBOL_NAME("RUN-COMMAND"), FUN_DENOT("&mk_mkcl_run_command_cfunobj")},
  {SYMBOL_NAME("RUN-PROGRAM-1"), FUN_DENOT("&mk_mkcl_run_program_1_cfunobj")},
  {SYMBOL_NAME("SETENV"), FUN_DENOT("&mk_mkcl_setenv_cfunobj")},
  {SYMBOL_NAME("SYSTEM"), FUN_DENOT("&mk_mkcl_system_cfunobj")},
  {SYMBOL_NAME("COPY-FILE"), FUN_DENOT("&mk_mkcl_copy_file_cfunobj")},
  {SYMBOL_NAME("GETUID"), FUN_DENOT("&mk_mkcl_getuid_cfunobj")},
  {SYMBOL_NAME("JOIN-PROCESS"), FUN_DENOT("&mk_mkcl_join_process_cfunobj")},
  {SYMBOL_NAME("TERMINATE-PROCESS"), FUN_DENOT("&mk_mkcl_terminate_process_cfunobj")},
  {SYMBOL_NAME("PROCESS-P"), FUN_DENOT("&mk_mkcl_process_p_cfunobj")},
  {SYMBOL_NAME("PROCESS-INPUT"), FUN_DENOT("&mk_mkcl_process_input_cfunobj")},
  {SYMBOL_NAME("PROCESS-OUTPUT"), FUN_DENOT("&mk_mkcl_process_output_cfunobj")},
  {SYMBOL_NAME("PROCESS-ERROR"), FUN_DENOT("&mk_mkcl_process_error_cfunobj")},
  {SYMBOL_NAME("PROCESS-PLIST"), FUN_DENOT("&mk_mkcl_process_plist_cfunobj")},
  {SYMBOL_NAME("PROCESS-TO-WORKER"), FUN_DENOT("&mk_mkcl_process_to_worker_cfunobj")},
  {SYMBOL_NAME("PROCESS-FROM-WORKER"), FUN_DENOT("&mk_mkcl_process_from_worker_cfunobj")},
  {SYMBOL_NAME("PROCESS-ERROR-FROM-WORKER"), FUN_DENOT("&mk_mkcl_process_error_from_worker_cfunobj")},
  {SYMBOL_NAME("SET-PROCESS-PLIST"), FUN_DENOT("&mk_mkcl_set_process_plist_cfunobj")},
  {SYMBOL_NAME("SET-PROCESS-TO-WORKER"), FUN_DENOT("&mk_mkcl_set_process_to_worker_cfunobj")},
  {SYMBOL_NAME("SET-PROCESS-FROM-WORKER"), FUN_DENOT("&mk_mkcl_set_process_from_worker_cfunobj")},
  {SYMBOL_NAME("SET-PROCESS-ERROR-FROM-WORKER"), FUN_DENOT("&mk_mkcl_set_process_error_from_worker_cfunobj")},
  {SYMBOL_NAME("PROCESS-STATUS"), FUN_DENOT("&mk_mkcl_process_status_cfunobj")},
  {SYMBOL_NAME("PROCESS-EXIT-CODE"), FUN_DENOT("&mk_mkcl_process_exit_code_cfunobj")},
  {SYMBOL_NAME("PROCESS-COMMAND"), FUN_DENOT("&mk_mkcl_process_command_cfunobj")},
  {SYMBOL_NAME("PROCESS-ARGV"), FUN_DENOT("&mk_mkcl_process_argv_cfunobj")},
  {SYMBOL_NAME("PROCESS-ID"), FUN_DENOT("&mk_mkcl_process_id_cfunobj")},
  {SYMBOL_NAME("PROCESS-DETACHED-P"), FUN_DENOT("&mk_mkcl_process_detached_p_cfunobj")},
  {SYMBOL_NAME("DETACH-PROCESS"), FUN_DENOT("&mk_mkcl_detach_process_cfunobj")},
  {SYMBOL_NAME("PROBE-FILE-P"), FUN_DENOT("&mk_mkcl_probe_file_p_cfunobj")},
  {SYMBOL_NAME("STREAM-FILENAME"), FUN_DENOT("&mk_mkcl_stream_filename_cfunobj")},
  {SYMBOL_NAME("PRIN1-TO-BASE-STRING"), FUN_DENOT("MKCL_IN_LISP(&mk_mkcl_prin1_to_base_string_cfunobj)")}, /* proclaimed in sysfun.lsp */
  {SYMBOL_NAME("PRINC-TO-BASE-STRING"), FUN_DENOT("MKCL_IN_LISP(&mk_mkcl_princ_to_base_string_cfunobj)")}, /* proclaimed in sysfun.lsp */
  {SYMBOL_NAME("WRITE-TO-BASE-STRING"), FUN_DENOT("MKCL_IN_LISP(&mk_mkcl_write_to_base_string_cfunobj)")}, /* proclaimed in sysfun.lsp */
  {SYMBOL_NAME("BASE-CHAR-P"), FUN_DENOT("&mk_mkcl_base_char_p_cfunobj")},
  {SYMBOL_NAME("PATHNAME-COMPLETE-P"), FUN_DENOT("&mk_mkcl_pathname_complete_p_cfunobj")},
  {SYMBOL_NAME("MELD-PATHNAMES"), FUN_DENOT("&mk_mkcl_meld_pathnames_cfunobj")},
  {SYMBOL_NAME("OCTETS"), FUN_DENOT("&mk_mkcl_octets_cfunobj")},
  {SYMBOL_NAME("DOUBLE-OCTETS"), FUN_DENOT("&mk_mkcl_double_octets_cfunobj")},
  {SYMBOL_NAME("RUN-PROGRAM"), FUN_DENOT("MKCL_IN_LISP(&mk_mkcl_run_program_cfunobj)")}, /* proclaimed in sysfun.lsp */
};

struct mkcl_function_declaration mkcl_mkcl_declare_macros[] = {
};

struct mkcl_function_declaration mkcl_mkcl_declare_special_operators[] = {
  {SYMBOL_NAME("COMPILER-LET"), FUN_DENOT("mk_cl_Cnil")},
};

struct mkcl_variable_declaration {
  struct mkcl_base_string symbol_name;
  struct mkcl_base_string value_denotator;
};

struct mkcl_variable_declaration mkcl_mkcl_declare_specials[] = {
  {SYMBOL_NAME("*MODULE-PROVIDER-FUNCTIONS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("!"), VAL_DENOT("mk_cl_Cnil")}, /* used by inspect and the debugger. */
  {SYMBOL_NAME("*CURRENT-WORKING-DIRECTORY*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*ALL-CURRENT-WORKING-DIRECTORIES*"), VAL_DENOT("mk_cl_Cnil")},
};

struct mkcl_variable_declaration mkcl_mkcl_declare_constants[] = {
  {SYMBOL_NAME("SHORT-FLOAT-POSITIVE-INFINITY"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("SINGLE-FLOAT-POSITIVE-INFINITY"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("DOUBLE-FLOAT-POSITIVE-INFINITY"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LONG-FLOAT-POSITIVE-INFINITY"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("SHORT-FLOAT-NEGATIVE-INFINITY"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("SINGLE-FLOAT-NEGATIVE-INFINITY"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("DOUBLE-FLOAT-NEGATIVE-INFINITY"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("LONG-FLOAT-NEGATIVE-INFINITY"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("BASE-CHAR-CODE-LIMIT"), VAL_DENOT("MKCL_MAKE_FIXNUM(MKCL_BASE_CHAR_CODE_LIMIT)")},
};

struct exposed_symbol {
  struct mkcl_base_string symbol_name;
  char * exposition;
};

static struct exposed_symbol const exposed_symbols[] = {
  /*  { SYMBOL_NAME("SOMETHING"), "MK_MKCL_something" }, */
  { SYMBOL_NAME("ARGC"), "MK_MKCL_argc"},
  { SYMBOL_NAME("ARGV"), "MK_MKCL_argv"},
  { SYMBOL_NAME("BAD-FASL-FILE"), "MK_MKCL_bad_fasl_file"},
  { SYMBOL_NAME("BASE-CHAR-P"), "MK_MKCL_base_char_p"},
  { SYMBOL_NAME("CHDIR"), "MK_MKCL_chdir"},
  { SYMBOL_NAME("CL-INDEX"), "MK_MKCL_cl_index"},
  { SYMBOL_NAME("CL-WORD"), "MK_MKCL_cl_word"},
  { SYMBOL_NAME("COMPILER-LET"), "MK_MKCL_compiler_let"},
  { SYMBOL_NAME("COPY-FILE"), "MK_MKCL_copy_file"},
  { SYMBOL_NAME("DETACH-PROCESS"), "MK_MKCL_detach_process"},
  { SYMBOL_NAME("DOUBLE-FLOAT-NEGATIVE-INFINITY"), "MK_MKCL_double_float_negative_infinity"},
  { SYMBOL_NAME("DOUBLE-FLOAT-POSITIVE-INFINITY"), "MK_MKCL_double_float_positive_infinity"},
  { SYMBOL_NAME("DOUBLE-OCTETS"), "MK_MKCL_double_octets"},
  { SYMBOL_NAME("FIXNUMP"), "MK_MKCL_fixnump"},
  { SYMBOL_NAME("GETCWD"), "MK_MKCL_getcwd"},
  { SYMBOL_NAME("GETENV"), "MK_MKCL_getenv"},
  { SYMBOL_NAME("GETPID"), "MK_MKCL_getpid"},
  { SYMBOL_NAME("GETTID"), "MK_MKCL_gettid"},
  { SYMBOL_NAME("GETUID"), "MK_MKCL_getuid"},
  { SYMBOL_NAME("INTEGER16"), "MK_MKCL_integer16"},
  { SYMBOL_NAME("INTEGER32"), "MK_MKCL_integer32"},
  { SYMBOL_NAME("INTEGER64"), "MK_MKCL_integer64"},
  { SYMBOL_NAME("INTEGER8"), "MK_MKCL_integer8"},
  { SYMBOL_NAME("INVALID-SLOT"), "MK_MKCL_invalid_slot"},
  { SYMBOL_NAME("JOIN-PROCESS"), "MK_MKCL_join_process"},
  { SYMBOL_NAME("LOGICAL-PATHNAME-P"), "MK_MKCL_logical_pathname_p"},
  { SYMBOL_NAME("LONG-FLOAT-NEGATIVE-INFINITY"), "MK_MKCL_long_float_negative_infinity"},
  { SYMBOL_NAME("LONG-FLOAT-POSITIVE-INFINITY"), "MK_MKCL_long_float_positive_infinity"},
  { SYMBOL_NAME("MAKE-PIPE"), "MK_MKCL_make_pipe"},
  { SYMBOL_NAME("MELD-PATHNAMES"), "MK_MKCL_meld_pathnames"},
  { SYMBOL_NAME("MKDIR"), "MK_MKCL_mkdir"},
  { SYMBOL_NAME("MKSTEMP"), "MK_MKCL_mkstemp"},
  { SYMBOL_NAME("NATURAL16"), "MK_MKCL_natural16"},
  { SYMBOL_NAME("NATURAL32"), "MK_MKCL_natural32"},
  { SYMBOL_NAME("NATURAL64"), "MK_MKCL_natural64"},
  { SYMBOL_NAME("NATURAL8"), "MK_MKCL_natural8"},
  { SYMBOL_NAME("OCTETS"), "MK_MKCL_octets"},
  { SYMBOL_NAME("PATHNAME-COMPLETE-P"), "MK_MKCL_pathname_complete_p"},
  { SYMBOL_NAME("PRIN1-TO-BASE-STRING"), "MK_MKCL_prin1_to_base_string"},
  { SYMBOL_NAME("PRINC-TO-BASE-STRING"), "MK_MKCL_princ_to_base_string"},
  { SYMBOL_NAME("PROBE-FILE-P"), "MK_MKCL_probe_file_p"},
  { SYMBOL_NAME("PROCESS"), "MK_MKCL_process"},
  { SYMBOL_NAME("PROCESS-ARGV"), "MK_MKCL_process_argv"},
  { SYMBOL_NAME("PROCESS-COMMAND"), "MK_MKCL_process_command"},
  { SYMBOL_NAME("PROCESS-DETACHED-P"), "MK_MKCL_process_detached_p"},
  { SYMBOL_NAME("PROCESS-ERROR"), "MK_MKCL_process_error"},
  { SYMBOL_NAME("PROCESS-ERROR-FROM-WORKER"), "MK_MKCL_process_error_from_worker"},
  { SYMBOL_NAME("PROCESS-EXIT-CODE"), "MK_MKCL_process_exit_code"},
  { SYMBOL_NAME("PROCESS-FROM-WORKER"), "MK_MKCL_process_from_worker"},
  { SYMBOL_NAME("PROCESS-ID"), "MK_MKCL_process_id"},
  { SYMBOL_NAME("PROCESS-INPUT"), "MK_MKCL_process_input"},
  { SYMBOL_NAME("PROCESS-OUTPUT"), "MK_MKCL_process_output"},
  { SYMBOL_NAME("PROCESS-P"), "MK_MKCL_process_p"},
  { SYMBOL_NAME("PROCESS-PLIST"), "MK_MKCL_process_plist"},
  { SYMBOL_NAME("PROCESS-STATUS"), "MK_MKCL_process_status"},
  { SYMBOL_NAME("PROCESS-TO-WORKER"), "MK_MKCL_process_to_worker"},
  { SYMBOL_NAME("RMDIR"), "MK_MKCL_rmdir"},
  { SYMBOL_NAME("RUN-COMMAND"), "MK_MKCL_run_command"},
  { SYMBOL_NAME("RUN-PROGRAM"), "MK_MKCL_run_program"},
  { SYMBOL_NAME("RUN-PROGRAM-1"), "MK_MKCL_run_program_1"},
  { SYMBOL_NAME("SEGMENTATION-VIOLATION"), "MK_MKCL_segmentation_violation"},
  { SYMBOL_NAME("SETENV"), "MK_MKCL_setenv"},
  { SYMBOL_NAME("SET-PROCESS-ERROR-FROM-WORKER"), "MK_MKCL_set_process_error_from_worker"},
  { SYMBOL_NAME("SET-PROCESS-FROM-WORKER"), "MK_MKCL_set_process_from_worker"},
  { SYMBOL_NAME("SET-PROCESS-PLIST"), "MK_MKCL_set_process_plist"},
  { SYMBOL_NAME("SET-PROCESS-TO-WORKER"), "MK_MKCL_set_process_to_worker"},
  { SYMBOL_NAME("SINGLE-FLOAT-NEGATIVE-INFINITY"), "MK_MKCL_single_float_negative_infinity"},
  { SYMBOL_NAME("SINGLE-FLOAT-POSITIVE-INFINITY"), "MK_MKCL_single_float_positive_infinity"},
  { SYMBOL_NAME("STACK-OVERFLOW"), "MK_MKCL_stack_overflow"},
  { SYMBOL_NAME("STORAGE-EXHAUSTED"), "MK_MKCL_storage_exhausted"},
  { SYMBOL_NAME("STREAM-DECODING-ERROR"), "MK_MKCL_stream_decoding_error"},
  { SYMBOL_NAME("STREAM-ENCODING-ERROR"), "MK_MKCL_stream_encoding_error"},
  { SYMBOL_NAME("STREAM-FILENAME"), "MK_MKCL_stream_filename"},
  { SYMBOL_NAME("SYSTEM"), "MK_MKCL_system"},
  { SYMBOL_NAME("TERMINATE-PROCESS"), "MK_MKCL_terminate_process"},
  { SYMBOL_NAME("WRITE-TO-BASE-STRING"), "MK_MKCL_write_to_base_string"},
  { SYMBOL_NAME("*ALL-CURRENT-WORKING-DIRECTORIES*"), "MK_MKCL_DYNVAR_all_current_working_directories"},
  { SYMBOL_NAME("*CURRENT-WORKING-DIRECTORY*"), "MK_MKCL_DYNVAR_current_working_directory"},
};



const struct mkcl_symbol blank_symbol = BLANK_SYMBOL_INITIALIZER;


void init_mkcl_package(void)
{
  long i;

  for (i = 0; i < MKCL_NB_ELEMS(mkcl_mkcl_external_symbol_names); i++)
    {
      struct mkcl_symbol * sym = &mkcl_mkcl_external_symbols[i];
      const struct mkcl_base_string * name = &mkcl_mkcl_external_symbol_names[i];
      const mkcl_hash_value hashed_name = hash_base_string(name->self, name->fillp, 0);

      *sym = blank_symbol;

      sym->name = (mkcl_object) name;
      sym->hashed_name = hashed_name;
      sym->hpack = (mkcl_object) &mkcl_package_mkcl;

      struct mkcl_hashtable_entry * entry = &(external_entries[i]);
      entry->hashed_key = hashed_name;
      entry->value = (mkcl_object) sym;
      entry->key = sym->name;
      entry->next = NULL;
      
      add_entry_to_hash(hashed_name, &external_ht, entry);
    }
  
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_mkcl_internal_symbol_names); i++)
    {
      struct mkcl_symbol * sym = &mkcl_mkcl_internal_symbols[i];
      const struct mkcl_base_string * name = &mkcl_mkcl_internal_symbol_names[i];
      const mkcl_hash_value hashed_name = hash_base_string(name->self, name->fillp, 0);

      *sym = blank_symbol;

      sym->name = (mkcl_object) name;
      sym->hashed_name = hashed_name;
      sym->hpack = (mkcl_object) &mkcl_package_mkcl;

      struct mkcl_hashtable_entry * entry = &(internal_entries[i]);
      entry->hashed_key = hashed_name;
      entry->value = (mkcl_object) sym;
      entry->key = sym->name;
      entry->next = NULL;

      add_entry_to_hash(hashed_name, &internal_ht, entry);
    }

  for (i = 0; i < MKCL_NB_ELEMS(mkcl_mkcl_declare_specials); i++)
    {
      int intern_flag;
      struct mkcl_base_string * const name = &mkcl_mkcl_declare_specials[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_mkcl, NULL);

      if (sym)
	{
	  sym->value = (mkcl_object) &mkcl_mkcl_declare_specials[i].value_denotator;
	  sym->stype = mkcl_stp_special;
	}
      else
	{
	  printf("special variable not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_mkcl_declare_constants); i++)
    {
      int intern_flag;
      struct mkcl_base_string * const name = &mkcl_mkcl_declare_constants[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_mkcl, NULL);

      if (sym)
	{
	  sym->value = (mkcl_object) &mkcl_mkcl_declare_constants[i].value_denotator;
	  sym->stype = mkcl_stp_constant;
	}
      else
	{
	  printf("special variable not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_mkcl_declare_lisp_functions); i++)
    {
      int intern_flag;
      struct mkcl_base_string * name = &mkcl_mkcl_declare_lisp_functions[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_mkcl, NULL);

      if (sym)
	{
	  sym->gfdef = (mkcl_object) &mkcl_mkcl_declare_lisp_functions[i].function_object_denotator;
	}
      else
	{
	  printf("function not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_mkcl_declare_macros); i++)
    {
      int intern_flag;
      struct mkcl_base_string * name = &mkcl_mkcl_declare_macros[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_mkcl, NULL);

      if (sym)
	{
	  sym->stype |= mkcl_stp_macro;
	  sym->gfdef = (mkcl_object) &mkcl_mkcl_declare_macros[i].function_object_denotator;
	}
      else
	{
	  printf("function not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_mkcl_declare_special_operators); i++)
    {
      int intern_flag;
      struct mkcl_base_string * name = &mkcl_mkcl_declare_special_operators[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_mkcl, NULL);

      if (sym)
	{
	  sym->stype |= mkcl_stp_special_form;
	  sym->gfdef = (mkcl_object) &mkcl_mkcl_declare_special_operators[i].function_object_denotator;
	}
      else
	{
	  printf("function not found %s\n", name->self);
	  exit(1);
	}
    }
}



mkcl_index mkcl_internal_symbol_index(struct mkcl_symbol * sym)
{
  return (sym - mkcl_mkcl_internal_symbols);
}

mkcl_index mkcl_external_symbol_index(struct mkcl_symbol * sym)
{
  return (sym - mkcl_mkcl_external_symbols);
}

mkcl_index mkcl_internal_entry_index(struct mkcl_hashtable_entry * entry)
{
  return (entry - internal_entries);
}

mkcl_index mkcl_external_entry_index(struct mkcl_hashtable_entry * entry)
{
  return (entry - external_entries);
}



void print_mkcl_internal_symbol_C_names(void)
{
  mkcl_index i;

  printf("\nstatic const struct mkcl_base_string mkcl_internal_C_names[] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      printf("SYMBOL_NAME(\"mkcl_mkcl_internal_symbols[%lu]\"),\n", i);
    }
  printf("};\n");
}

void print_mkcl_external_symbol_C_names(void)
{
  mkcl_index i;

  printf("\nstatic const struct mkcl_base_string mkcl_external_C_names[] = {\n");
  for (i = 0; i < external_count; i++)
    {
      printf("SYMBOL_NAME(\"mkcl_mkcl_external_symbols[%lu]\"),\n", i);
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

void print_mkcl_internal_symbol_initializers(void)
{
  mkcl_index i;

  printf("\nstruct mkcl_symbol mkcl_mkcl_internal_symbols[] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      struct mkcl_symbol * sym = &mkcl_mkcl_internal_symbols[i];
      
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
      printf("(mkcl_object) &mkcl_mkcl_internal_symbol_names[%lu], ", i); /* name */
      printf("(mkcl_object) &mkcl_package_mkcl, "); /* hpack */
      printf("mk_cl_Cnil, "); /* properly_named_class */
      printf("mk_cl_Cnil, "); /* sys_plist */
      printf("MKCL_NOT_A_SPECIAL_INDEX, "); /* special_index */
      printf("%luUL, ", sym->hashed_name); /* hashed_name */
      printf("(mkcl_object) &mkcl_internal_C_names[%lu], ", i); /* C_name */
      printf("NULL, "); /* _C_name */
      printf("NULL, "); /* _name */
      
      printf("}, /* %lu %s */\n", i, sym->name->base_string.self);
    }
  printf("};\n");
}

void print_mkcl_external_symbol_initializers(void)
{
  mkcl_index i;

  printf("\nstruct mkcl_symbol mkcl_mkcl_external_symbols[] = {\n");
  for (i = 0; i < external_count; i++)
    {
      struct mkcl_symbol * sym = &mkcl_mkcl_external_symbols[i];
      
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
      printf("(mkcl_object) &mkcl_mkcl_external_symbol_names[%lu], ", i); /* name */
      printf("(mkcl_object) &mkcl_package_mkcl, "); /* hpack */
      printf("mk_cl_Cnil, "); /* properly_named_class */
      printf("mk_cl_Cnil, "); /* sys_plist */
      printf("MKCL_NOT_A_SPECIAL_INDEX, "); /* special_index */
      printf("%luUL, ", sym->hashed_name); /* hashed_name */
      printf("(mkcl_object) &mkcl_external_C_names[%lu], ", i); /* C_name */
      printf("NULL, "); /* _C_name */
      printf("NULL, "); /* _name */
      
      printf("}, /* %lu %s */\n", i, sym->name->base_string.self);
    }
  printf("};\n");
}


void print_mkcl_internal_hashtable_entry_initializers(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry internal_entries[internal_count] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      struct mkcl_hashtable_entry * entry = &internal_entries[i];
      

      printf("{ ");
      if (entry->next)
	printf("&internal_entries[%lu], ", mkcl_internal_entry_index(entry->next));
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
	  mkcl_index sym_index = mkcl_internal_symbol_index((struct mkcl_symbol *) entry->value);

	  printf("(mkcl_object) &mkcl_mkcl_internal_symbol_names[%lu], ", sym_index);
	  printf("%luUL, ", entry->hashed_key);
	  printf("(mkcl_object) &mkcl_mkcl_internal_symbols[%lu], ", sym_index);
	}
      printf("}, \n");
    }
  printf("};\n");
}

void print_mkcl_internal_hashtable_vector_initializer(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry * internal_vector[internal_size] = {\n");
  for (i = 0; i < internal_size; i++)
    {
      struct mkcl_hashtable_entry * entry = internal_vector[i];

      if (entry)
	printf("&internal_entries[%lu], \n", mkcl_internal_entry_index(entry));
      else
	printf("NULL, \n");

    }
  printf("};\n");
}

void print_mkcl_external_hashtable_entry_initializers(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry external_entries[external_count] = {\n");
  for (i = 0; i < external_count; i++)
    {
      struct mkcl_hashtable_entry * entry = &external_entries[i];
      

      printf("{ ");
      if (entry->next)
	printf("&external_entries[%lu], ", mkcl_external_entry_index(entry->next));
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
	  mkcl_index sym_index = mkcl_external_symbol_index((struct mkcl_symbol *) entry->value);

	  printf("(mkcl_object) &mkcl_mkcl_external_symbol_names[%lu], ", sym_index);
	  printf("%luUL, ", entry->hashed_key);
	  printf("(mkcl_object) &mkcl_mkcl_external_symbols[%lu], ", sym_index);
	}
      printf("}, \n");
    }
  printf("};\n");
}

void print_mkcl_external_hashtable_vector_initializer(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry * external_vector[external_size] = {\n");
  for (i = 0; i < external_size; i++)
    {
      struct mkcl_hashtable_entry * entry = external_vector[i];

      if (entry)
	printf("&external_entries[%lu], \n", mkcl_external_entry_index(entry));
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


void print_mkcl_package(void)
{
  print_copyright();
  print_mkcl_internal_symbol_C_names();
  print_mkcl_internal_symbol_initializers();
  print_mkcl_external_symbol_C_names();
  print_mkcl_external_symbol_initializers();
  print_mkcl_internal_hashtable_entry_initializers();
  print_mkcl_internal_hashtable_vector_initializer();
  print_mkcl_external_hashtable_entry_initializers();
  print_mkcl_external_hashtable_vector_initializer();
}

void expose_mkcl_symbol(const struct exposed_symbol * exposed_symbol)
{
  bool internalp;
  struct mkcl_symbol * sym = find_symbol(&exposed_symbol->symbol_name, &mkcl_package_mkcl, &internalp);

  if ( sym == NULL ) { printf("Exposed symbol not found: %s\n", exposed_symbol->symbol_name.self); exit(1); }

  printf("#define %s ", exposed_symbol->exposition);
  if (internalp)
    printf("mkcl_mkcl_internal_symbols[%lu]\n", mkcl_internal_symbol_index(sym));
  else
    printf("mkcl_mkcl_external_symbols[%lu]\n", mkcl_external_symbol_index(sym)); 
}

void expose_mkcl_package(void)
{
  mkcl_index i;

  print_copyright();
  for (i = 0; i < MKCL_NB_ELEMS(exposed_symbols); i++)
    {
      expose_mkcl_symbol(&exposed_symbols[i]);
    }
}

int main(int argc, char * argv[])
{
  char * program_name = basename(strdup(argv[0]));
  init_mkcl_package();
  

  if ( strcmp(program_name, "build_package_MKCL") == 0 )
    print_mkcl_package();
  else if ( strcmp(program_name, "expose_package_MKCL") == 0 )
    expose_mkcl_package();

  return 0;
}

