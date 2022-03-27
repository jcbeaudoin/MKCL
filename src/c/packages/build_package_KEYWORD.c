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


#include "KEYWORD_package.h"


struct exposed_symbol {
  struct mkcl_base_string symbol_name;
  char * exposition;
};

static struct exposed_symbol const exposed_symbols[] = {
  /*  { SYMBOL_NAME("SOMETHING"), "MK_KEYWORD_something" }, */
  { SYMBOL_NAME("ABORT"), "MK_KEY_abort" },
  { SYMBOL_NAME("ABORTED"), "MK_KEY_aborted" },
  { SYMBOL_NAME("ABSOLUTE"), "MK_KEY_absolute" },
  { SYMBOL_NAME("ACTIVE"), "MK_KEY_active" },
  { SYMBOL_NAME("ADDRESS"), "MK_KEY_address" },
  { SYMBOL_NAME("ALL-DRIVES"), "MK_KEY_all_drives" },
  { SYMBOL_NAME("ALLOW-OTHER-KEYS"), "MK_KEY_allow_other_keys" },
  { SYMBOL_NAME("ANSI-CL"), "MK_KEY_ansi_cl" },
  { SYMBOL_NAME("APPEND"), "MK_KEY_append" },
  { SYMBOL_NAME("ARRAY"), "MK_KEY_array" },
  { SYMBOL_NAME("ASCII"), "MK_KEY_ascii" },
  { SYMBOL_NAME("BACK"), "MK_KEY_back" },
  { SYMBOL_NAME("BASE"), "MK_KEY_base" },
  { SYMBOL_NAME("BIG-ENDIAN"), "MK_KEY_big_endian" },
  { SYMBOL_NAME("BINDING-STACK-INITIAL-SIZE"), "MK_KEY_binding_stack_initial_size" },
  { SYMBOL_NAME("BINDING-STACK-SIZE-LIMIT"), "MK_KEY_binding_stack_size_limit" },
  { SYMBOL_NAME("BLOCK"), "MK_KEY_block" },
  { SYMBOL_NAME("BYTE"), "MK_KEY_byte" },
  { SYMBOL_NAME("CALLBACK"), "MK_KEY_callback" },
  { SYMBOL_NAME("CALL-STACK-SIZE"), "MK_KEY_call_stack_size" },
  { SYMBOL_NAME("CANCELED"), "MK_KEY_canceled" },
  { SYMBOL_NAME("CAPITALIZE"), "MK_KEY_capitalize" },
  { SYMBOL_NAME("CASE"), "MK_KEY_case" },
  { SYMBOL_NAME("CDECL"), "MK_KEY_cdecl" },
  { SYMBOL_NAME("CHANGE-DEFAULT-PATHNAME-DEFAULTS"), "MK_KEY_change_default_pathname_defaults" },
  { SYMBOL_NAME("CHAR"), "MK_KEY_char" },
  { SYMBOL_NAME("CIRCLE"), "MK_KEY_circle" },
  { SYMBOL_NAME("COMMON"), "MK_KEY_common" },
  { SYMBOL_NAME("COMMON-LISP"), "MK_KEY_common_lisp" },
  { SYMBOL_NAME("COMPILE-TOPLEVEL"), "MK_KEY_compile_toplevel" },
  { SYMBOL_NAME("CONTROL-STRING"), "MK_KEY_control_string" },
  { SYMBOL_NAME("CORRUPTED"), "MK_KEY_corrupted" },
  { SYMBOL_NAME("CR"), "MK_KEY_cr" },
  { SYMBOL_NAME("CREATE"), "MK_KEY_create" },
  { SYMBOL_NAME("CRLF"), "MK_KEY_crlf" },
  { SYMBOL_NAME("CSTRING"), "MK_KEY_cstring" },
  { SYMBOL_NAME("DATUM"), "MK_KEY_datum" },
  { SYMBOL_NAME("DEAD"), "MK_KEY_dead" },
  { SYMBOL_NAME("DECLARE"), "MK_KEY_declare" },
  { SYMBOL_NAME("DEFAULT"), "MK_KEY_default" },
  { SYMBOL_NAME("DEFAULTS"), "MK_KEY_defaults" },
  { SYMBOL_NAME("DETACHED"), "MK_KEY_detached" },
  { SYMBOL_NAME("DEVICE"), "MK_KEY_device" },
  { SYMBOL_NAME("DIRECTION"), "MK_KEY_direction" },
  { SYMBOL_NAME("DIRECTORY"), "MK_KEY_directory" },
  { SYMBOL_NAME("DISABLED"), "MK_KEY_disabled" },
  { SYMBOL_NAME("DONE"), "MK_KEY_done" },
  { SYMBOL_NAME("DOUBLE"), "MK_KEY_double" },
  { SYMBOL_NAME("DOUBLE-COMPLEX"), "MK_KEY_double_complex" },
  { SYMBOL_NAME("DOUBLE-IMAGINARY"), "MK_KEY_double_imaginary" },
  { SYMBOL_NAME("DOWNCASE"), "MK_KEY_downcase" },
  { SYMBOL_NAME("ELEMENT-TYPE"), "MK_KEY_element_type" },
  { SYMBOL_NAME("ENABLED"), "MK_KEY_enabled" },
  { SYMBOL_NAME("ENCODING"), "MK_KEY_encoding" },
  { SYMBOL_NAME("END"), "MK_KEY_end" },
  { SYMBOL_NAME("END1"), "MK_KEY_end1" },
  { SYMBOL_NAME("END2"), "MK_KEY_end2" },
  { SYMBOL_NAME("ENVIRONMENT"), "MK_KEY_environment" },
  { SYMBOL_NAME("EOF"), "MK_KEY_eof" },
  { SYMBOL_NAME("ERROR"), "MK_KEY_error" },
  { SYMBOL_NAME("ESCAPE"), "MK_KEY_escape" },
  { SYMBOL_NAME("EXECUTE"), "MK_KEY_execute" },
  { SYMBOL_NAME("EXITED"), "MK_KEY_exited" },
  { SYMBOL_NAME("EXPECTED-TYPE"), "MK_KEY_expected_type" },
  { SYMBOL_NAME("EXTERNAL"), "MK_KEY_external" },
  { SYMBOL_NAME("EXTERNAL-SIZE"), "MK_KEY_external_size" },
  { SYMBOL_NAME("EXTERNAL-FORMAT"), "MK_KEY_external_format" },
  { SYMBOL_NAME("FAST"), "MK_KEY_fast" },
  { SYMBOL_NAME("FILE"), "MK_KEY_file" },
  { SYMBOL_NAME("FLOAT"), "MK_KEY_float" },
  { SYMBOL_NAME("FLOAT-COMPLEX"), "MK_KEY_float_complex" },
  { SYMBOL_NAME("FLOAT-IMAGINARY"), "MK_KEY_float_imaginary" },
  { SYMBOL_NAME("FOLLOW-SYMLINKS"), "MK_KEY_follow_symlinks" },
  { SYMBOL_NAME("FORCE"), "MK_KEY_force" },
  { SYMBOL_NAME("FOREIGN"), "MK_KEY_foreign" },
  { SYMBOL_NAME("FORMAT"), "MK_KEY_format" },
  { SYMBOL_NAME("FORMAT-ARGUMENTS"), "MK_KEY_format_arguments" },
  { SYMBOL_NAME("FORMAT-CONTROL"), "MK_KEY_format_control" },
  { SYMBOL_NAME("FRAME-STACK-INITIAL-SIZE"), "MK_KEY_frame_stack_initial_size" },
  { SYMBOL_NAME("FRAME-STACK-SIZE-LIMIT"), "MK_KEY_frame_stack_size_limit" },
  { SYMBOL_NAME("FULL"), "MK_KEY_full" },
  { SYMBOL_NAME("FULLY-BUFFERED"), "MK_KEY_fully_buffered" },
  { SYMBOL_NAME("FUNCTION"), "MK_KEY_function" },
  { SYMBOL_NAME("GC-ABORT"), "MK_KEY_gc_abort" },
  { SYMBOL_NAME("GC-EXIT"), "MK_KEY_gc_exit" },
  { SYMBOL_NAME("GENSYM"), "MK_KEY_gensym" },
  { SYMBOL_NAME("HOST"), "MK_KEY_host" },
  { SYMBOL_NAME("IEEE-FLOATING-POINT"), "MK_KEY_ieee_floating_point" },
  { SYMBOL_NAME("IF-DOES-NOT-EXIST"), "MK_KEY_if_does_not_exist" },
  { SYMBOL_NAME("IF-EXISTS"), "MK_KEY_if_exists" },
  { SYMBOL_NAME("IMPORTED"), "MK_KEY_imported" },
  { SYMBOL_NAME("IMPORTED-AND-GC-REGISTERED"), "MK_KEY_imported_and_gc_registered" },
  { SYMBOL_NAME("INHERITED"), "MK_KEY_inherited" },
  { SYMBOL_NAME("INITIAL-BINDINGS"), "MK_KEY_initial_bindings" },
  { SYMBOL_NAME("INITIAL-CONTENTS"), "MK_KEY_initial_contents" },
  { SYMBOL_NAME("INITIAL-ELEMENT"), "MK_KEY_initial_element" },
  { SYMBOL_NAME("INITIALIZED"), "MK_KEY_initialized" },
  { SYMBOL_NAME("INPUT"), "MK_KEY_input" },
  { SYMBOL_NAME("INSTANCE"), "MK_KEY_instance" },
  { SYMBOL_NAME("INT"), "MK_KEY_int" },
  { SYMBOL_NAME("INTERNAL"), "MK_KEY_internal" },
  { SYMBOL_NAME("INTERNAL-SIZE"), "MK_KEY_internal_size" },
  { SYMBOL_NAME("INTERRUPTED"), "MK_KEY_interrupted" },
  { SYMBOL_NAME("INVALID"), "MK_KEY_invalid" },
  { SYMBOL_NAME("INVALID-VALUE"), "MK_KEY_invalid_value" },
  { SYMBOL_NAME("INVERT"), "MK_KEY_invert" },
  { SYMBOL_NAME("IO"), "MK_KEY_io" },
  { SYMBOL_NAME("ISO-8859-1"), "MK_KEY_iso_8859_1" },
  { SYMBOL_NAME("JUNK-ALLOWED"), "MK_KEY_junk_allowed" },
  { SYMBOL_NAME("KEY"), "MK_KEY_key" },
  { SYMBOL_NAME("LATIN-1"), "MK_KEY_latin_1" },
  { SYMBOL_NAME("LENGTH"), "MK_KEY_length" },
  { SYMBOL_NAME("LEVEL"), "MK_KEY_level" },
  { SYMBOL_NAME("LF"), "MK_KEY_lf" },
  { SYMBOL_NAME("LINE"), "MK_KEY_line" },
  { SYMBOL_NAME("LINE-BUFFERED"), "MK_KEY_line_buffered" },
  { SYMBOL_NAME("LINENO"), "MK_KEY_lineno" },
  { SYMBOL_NAME("LINES"), "MK_KEY_lines" },
  { SYMBOL_NAME("LINK"), "MK_KEY_link" },
  { SYMBOL_NAME("LINUX"), "MK_KEY_linux" },
  { SYMBOL_NAME("LISP-TEMP-STACK-INITIAL-SIZE"), "MK_KEY_lisp_temp_stack_initial_size" },
  { SYMBOL_NAME("LISP-TEMP-STACK-SIZE-LIMIT"), "MK_KEY_lisp_temp_stack_size_limit" },
  { SYMBOL_NAME("LITTLE-ENDIAN"), "MK_KEY_little_endian" },
  { SYMBOL_NAME("LOAD-TOPLEVEL"), "MK_KEY_load_toplevel" },
  { SYMBOL_NAME("LOCAL"), "MK_KEY_local" },
  { SYMBOL_NAME("LONG"), "MK_KEY_long" },
  { SYMBOL_NAME("LONG-DOUBLE"), "MK_KEY_long_double" },
  { SYMBOL_NAME("LONG-DOUBLE-COMPLEX"), "MK_KEY_long_double_complex" },
  { SYMBOL_NAME("LONG-DOUBLE-IMAGINARY"), "MK_KEY_long_double_imaginary" },
  { SYMBOL_NAME("LONG-LONG"), "MK_KEY_long_long" },
  { SYMBOL_NAME("MISER-WIDTH"), "MK_KEY_miser_width" },
  { SYMBOL_NAME("MKCL"), "MK_KEY_mkcl" },
  { SYMBOL_NAME("MKCL-COMPILED"), "MK_KEY_mkcl_compiled" },
  { SYMBOL_NAME("NAME"), "MK_KEY_name" },
  { SYMBOL_NAME("NEWEST"), "MK_KEY_newest" },
  { SYMBOL_NAME("NEW-VERSION"), "MK_KEY_new_version" },
  { SYMBOL_NAME("NICKNAMES"), "MK_KEY_nicknames" },
  { SYMBOL_NAME("NONE"), "MK_KEY_none" },
  { SYMBOL_NAME("OBJECT"), "MK_KEY_object" },
  { SYMBOL_NAME("OFFSET"), "MK_KEY_offset" },
  { SYMBOL_NAME("OPERANDS"), "MK_KEY_operands" },
  { SYMBOL_NAME("OPERATION"), "MK_KEY_operation" },
  { SYMBOL_NAME("OUTPUT"), "MK_KEY_output" },
  { SYMBOL_NAME("OVERWRITE"), "MK_KEY_overwrite" },
  { SYMBOL_NAME("PACKAGE"), "MK_KEY_package" },
  { SYMBOL_NAME("PATHNAME"), "MK_KEY_pathname" },
  { SYMBOL_NAME("POINTER-VOID"), "MK_KEY_pointer_void" },
  { SYMBOL_NAME("PPRINT-DISPATCH"), "MK_KEY_pprint_dispatch" },
  { SYMBOL_NAME("PRESERVE"), "MK_KEY_preserve" },
  { SYMBOL_NAME("PRETTY"), "MK_KEY_pretty" },
  { SYMBOL_NAME("PRINT"), "MK_KEY_print" },
  { SYMBOL_NAME("PROBE"), "MK_KEY_probe" },
  { SYMBOL_NAME("RADIX"), "MK_KEY_radix" },
  { SYMBOL_NAME("READ"), "MK_KEY_read" },
  { SYMBOL_NAME("READABLY"), "MK_KEY_readably" },
  { SYMBOL_NAME("REAL-NAME"), "MK_KEY_real_name" },
  { SYMBOL_NAME("REASON"), "MK_KEY_reason" },
  { SYMBOL_NAME("RECURSIVE"), "MK_KEY_recursive" },
  { SYMBOL_NAME("REHASH-SIZE"), "MK_KEY_rehash_size" },
  { SYMBOL_NAME("REHASH-THRESHOLD"), "MK_KEY_rehash_threshold" },
  { SYMBOL_NAME("RELATIVE"), "MK_KEY_relative" },
  { SYMBOL_NAME("RELATIVE-PACKAGE-NAMES"), "MK_KEY_relative_package_names" },
  { SYMBOL_NAME("RENAME"), "MK_KEY_rename" },
  { SYMBOL_NAME("RENAME-AND-DELETE"), "MK_KEY_rename_and_delete" },
  { SYMBOL_NAME("RIGHT-MARGIN"), "MK_KEY_right_margin" },
  { SYMBOL_NAME("RUNNING"), "MK_KEY_running" },
  { SYMBOL_NAME("SEARCH"), "MK_KEY_search" },
  { SYMBOL_NAME("SEARCH-LIST"), "MK_KEY_search_list" },
  { SYMBOL_NAME("SET"), "MK_KEY_set" },
  { SYMBOL_NAME("SHORT"), "MK_KEY_short" },
  { SYMBOL_NAME("SIGALTSTACK-SIZE"), "MK_KEY_sigaltstack_size" },
  { SYMBOL_NAME("SIGNAL-ERROR"), "MK_KEY_signal_error" },
  { SYMBOL_NAME("SIZE"), "MK_KEY_size" },
  { SYMBOL_NAME("SPECIAL"), "MK_KEY_special" },
  { SYMBOL_NAME("STALE"), "MK_KEY_stale" },
  { SYMBOL_NAME("START"), "MK_KEY_start" },
  { SYMBOL_NAME("START1"), "MK_KEY_start1" },
  { SYMBOL_NAME("START2"), "MK_KEY_start2" },
  { SYMBOL_NAME("STDCALL"), "MK_KEY_stdcall" },
  { SYMBOL_NAME("STDIO-STREAM"), "MK_KEY_stdio_stream" },
  { SYMBOL_NAME("STOPPED"), "MK_KEY_stopped" },
  { SYMBOL_NAME("STREAM"), "MK_KEY_stream" },
  { SYMBOL_NAME("SUPERSEDE"), "MK_KEY_supersede" },
  { SYMBOL_NAME("SUSPENDED"), "MK_KEY_suspended" },
  { SYMBOL_NAME("TAG"), "MK_KEY_tag" },
  { SYMBOL_NAME("TERMINATED"), "MK_KEY_terminated" },
  { SYMBOL_NAME("TEST"), "MK_KEY_test" },
  { SYMBOL_NAME("TEST-NOT"), "MK_KEY_test_not" },
  { SYMBOL_NAME("THREAD"), "MK_KEY_thread" },
  { SYMBOL_NAME("TYPE"), "MK_KEY_type" },
  { SYMBOL_NAME("UNICODE"), "MK_KEY_unicode" },
  { SYMBOL_NAME("UNKNOWN"), "MK_KEY_unknown" },
  { SYMBOL_NAME("UNIX"), "MK_KEY_unix" },
  { SYMBOL_NAME("UNLIMITED"), "MK_KEY_unlimited" },
  { SYMBOL_NAME("UNSIGNED-BYTE"), "MK_KEY_unsigned_byte" },
  { SYMBOL_NAME("UNSIGNED-CHAR"), "MK_KEY_unsigned_char" },
  { SYMBOL_NAME("UNSIGNED-INT"), "MK_KEY_unsigned_int" },
  { SYMBOL_NAME("UNSIGNED-LONG"), "MK_KEY_unsigned_long" },
  { SYMBOL_NAME("UNSIGNED-LONG-LONG"), "MK_KEY_unsigned_long_long" },
  { SYMBOL_NAME("UNSIGNED-SHORT"), "MK_KEY_unsigned_short" },
  { SYMBOL_NAME("UNSPECIFIC"), "MK_KEY_unspecific" },
  { SYMBOL_NAME("UP"), "MK_KEY_up" },
  { SYMBOL_NAME("UPCASE"), "MK_KEY_upcase" },
  { SYMBOL_NAME("US-ASCII"), "MK_KEY_us_ascii" },
  { SYMBOL_NAME("USE"), "MK_KEY_use" },
  { SYMBOL_NAME("UTF-16"), "MK_KEY_utf_16" },
  { SYMBOL_NAME("UTF-16BE"), "MK_KEY_utf_16be" },
  { SYMBOL_NAME("UTF-16LE"), "MK_KEY_utf_16le" },
  { SYMBOL_NAME("UTF-32"), "MK_KEY_utf_32" },
  { SYMBOL_NAME("UTF-32BE"), "MK_KEY_utf_32be" },
  { SYMBOL_NAME("UTF-32LE"), "MK_KEY_utf_32le" },
  { SYMBOL_NAME("UTF-8"), "MK_KEY_utf_8" },
  { SYMBOL_NAME("VERBOSE"), "MK_KEY_verbose" },
  { SYMBOL_NAME("VERSION"), "MK_KEY_version" },
  { SYMBOL_NAME("VOID"), "MK_KEY_void" },
  { SYMBOL_NAME("WAIT"), "MK_KEY_wait" },
  { SYMBOL_NAME("WILD"), "MK_KEY_wild" },
  { SYMBOL_NAME("WILD-INFERIORS"), "MK_KEY_wild_inferiors" },
  { SYMBOL_NAME("WRITE"), "MK_KEY_write" },
  { SYMBOL_NAME("X86"), "MK_KEY_x86" },
  { SYMBOL_NAME("X86-64"), "MK_KEY_x86_64" },
  { SYMBOL_NAME("MINGW32"), "MK_KEY_mingw32" },
  { SYMBOL_NAME("MINGW64"), "MK_KEY_mingw64" },
  { SYMBOL_NAME("MSVC"), "MK_KEY_msvc" },
  { SYMBOL_NAME("WINDOWS"), "MK_KEY_windows" },
  { SYMBOL_NAME("ANDROID"), "MK_KEY_android" },
  { SYMBOL_NAME("FREEBSD"), "MK_KEY_freebsd" },
  { SYMBOL_NAME("ARM"), "MK_KEY_arm" },
  { SYMBOL_NAME("AARCH64"), "MK_KEY_aarch64" },
};



const struct mkcl_symbol blank_symbol = BLANK_SYMBOL_INITIALIZER;


void init_keyword_package(void)
{
  long i;

  for (i = 0; i < MKCL_NB_ELEMS(mkcl_keyword_external_symbol_names); i++)
    {
      struct mkcl_symbol * sym = &mkcl_keyword_external_symbols[i];
      const struct mkcl_base_string * name = &mkcl_keyword_external_symbol_names[i];
      const mkcl_hash_value hashed_name = hash_base_string(name->self, name->fillp, 0);

      *sym = blank_symbol;

      sym->name = (mkcl_object) name;
      sym->hashed_name = hashed_name;
      sym->hpack = (mkcl_object) &mkcl_package_keyword;
      sym->stype = mkcl_stp_constant;

      struct mkcl_hashtable_entry * entry = &(external_entries[i]);
      entry->hashed_key = hashed_name;
      entry->value = (mkcl_object) sym;
      entry->key = sym->name;
      entry->next = NULL;
      
      add_entry_to_hash(hashed_name, &external_ht, entry);
    }
}



mkcl_index keyword_external_symbol_index(struct mkcl_symbol * sym)
{
  return (sym - mkcl_keyword_external_symbols);
}

mkcl_index keyword_external_entry_index(struct mkcl_hashtable_entry * entry)
{
  return (entry - external_entries);
}



void print_keyword_external_symbol_C_names(void)
{
  mkcl_index i;

  printf("\nstatic const struct mkcl_base_string keyword_external_C_names[] = {\n");
  for (i = 0; i < external_count; i++)
    {
      printf("SYMBOL_NAME(\"mkcl_keyword_external_symbols[%lu]\"),\n", i);
    }
  printf("};\n");
}


void print_keyword_external_symbol_initializers(void)
{
  mkcl_index i;

  printf("\nstruct mkcl_symbol mkcl_keyword_external_symbols[] = {\n");
  for (i = 0; i < external_count; i++)
    {
      struct mkcl_symbol * sym = &mkcl_keyword_external_symbols[i];
      
      printf("{ mkcl_t_symbol, 0, mkcl_stp_constant, 0, ");  /* MKCL_HEADER(stype) */
      printf("(mkcl_object) &mkcl_keyword_external_symbols[%lu],  ", i); /* value */
      printf("mk_cl_Cnil, "); /* gfdef */
      printf("mk_cl_Cnil, "); /* plist */
      printf("(mkcl_object) &mkcl_keyword_external_symbol_names[%lu], ", i); /* name */
      printf("(mkcl_object) &mkcl_package_keyword, "); /* hpack */
      printf("mk_cl_Cnil, "); /* properly_named_class */
      printf("mk_cl_Cnil, "); /* sys_plist */
      printf("MKCL_NOT_A_SPECIAL_INDEX, "); /* special_index */
      printf("%luUL, ", sym->hashed_name); /* hashed_name */
      printf("(mkcl_object) &keyword_external_C_names[%lu], ", i); /* C_name */
      printf("NULL, "); /* _C_name */
      printf("NULL, "); /* _name */
      
      printf("}, /* %lu %s */\n", i, sym->name->base_string.self);
    }
  printf("};\n");
}



void print_keyword_external_hashtable_entry_initializers(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry external_entries[external_count] = {\n");
  for (i = 0; i < external_count; i++)
    {
      struct mkcl_hashtable_entry * entry = &external_entries[i];
      

      printf("{ ");
      if (entry->next)
	printf("&external_entries[%lu], ", keyword_external_entry_index(entry->next));
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
	  mkcl_index sym_index = keyword_external_symbol_index((struct mkcl_symbol *) entry->value);

	  printf("(mkcl_object) &mkcl_keyword_external_symbol_names[%lu], ", sym_index);
	  printf("%luUL, ", entry->hashed_key);
	  printf("(mkcl_object) &mkcl_keyword_external_symbols[%lu], ", sym_index);
	}
      printf("}, \n");
    }
  printf("};\n");
}

void print_keyword_external_hashtable_vector_initializer(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry * external_vector[external_size] = {\n");
  for (i = 0; i < external_size; i++)
    {
      struct mkcl_hashtable_entry * entry = external_vector[i];

      if (entry)
	printf("&external_entries[%lu], \n", keyword_external_entry_index(entry));
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


void print_keyword_package(void)
{
  print_copyright();
  print_keyword_external_symbol_C_names();
  print_keyword_external_symbol_initializers();
  print_keyword_external_hashtable_entry_initializers();
  print_keyword_external_hashtable_vector_initializer();
}

void expose_keyword_symbol(const struct exposed_symbol * exposed_symbol)
{
  bool internalp;
  struct mkcl_symbol * sym = find_symbol(&exposed_symbol->symbol_name, &mkcl_package_keyword, &internalp);

  if ( sym == NULL ) { printf("Exposed symbol not found: %s\n", exposed_symbol->symbol_name.self); exit(1); }

  printf("#define %s ", exposed_symbol->exposition);
  printf("mkcl_keyword_external_symbols[%lu]\n", keyword_external_symbol_index(sym)); 
}

void expose_keyword_package(void)
{
  mkcl_index i;

  print_copyright();
  for (i = 0; i < MKCL_NB_ELEMS(exposed_symbols); i++)
    {
      expose_keyword_symbol(&exposed_symbols[i]);
    }
}

int main(int argc, char * argv[])
{
  char * program_name = basename(strdup(argv[0]));
  
  init_keyword_package();

  if ( strcmp(program_name, "build_package_KEYWORD" PROGRAM_SUFFIX) == 0 )
    print_keyword_package();
  else if ( strcmp(program_name, "expose_package_KEYWORD" PROGRAM_SUFFIX) == 0 )
    expose_keyword_package();
  else
    { printf("\nDon't know this program name: %s\n", program_name); exit(2); }


  return 0;
}

