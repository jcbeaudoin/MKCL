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


#include "GRAY_package.h"


struct mkcl_function_declaration {
  struct mkcl_base_string symbol_name;
  struct mkcl_base_string function_object_denotator;
};

struct mkcl_function_declaration mkcl_gray_declare_lisp_functions[] = {
};

struct mkcl_variable_declaration {
  struct mkcl_base_string symbol_name;
  struct mkcl_base_string value_denotator;
};

struct mkcl_variable_declaration mkcl_gray_declare_specials[] = {
};

struct mkcl_variable_declaration mkcl_gray_declare_constants[] = {
};

struct exposed_symbol {
  struct mkcl_base_string symbol_name;
  char * exposition;
};

static struct exposed_symbol const exposed_symbols[] = {
  /*  { SYMBOL_NAME("SOMETHING"), "MK_GRAY_something" }, */
  {SYMBOL_NAME("STREAM-CLEAR-INPUT"), "MK_GRAY_stream_clear_input"},
  {SYMBOL_NAME("STREAM-CLEAR-OUTPUT"), "MK_GRAY_stream_clear_output"},
  {SYMBOL_NAME("STREAM-FILE-POSITION"), "MK_GRAY_stream_file_position"},
  {SYMBOL_NAME("STREAM-FINISH-OUTPUT"), "MK_GRAY_stream_finish_output"},
  {SYMBOL_NAME("STREAM-FORCE-OUTPUT"), "MK_GRAY_stream_force_output"},
  {SYMBOL_NAME("STREAM-FRESH-LINE"), "MK_GRAY_stream_fresh_line"},
  {SYMBOL_NAME("STREAM-INTERACTIVE-P"), "MK_GRAY_stream_interactive_p"},
  {SYMBOL_NAME("STREAM-LINE-COLUMN"), "MK_GRAY_stream_line_column"},
  {SYMBOL_NAME("STREAM-LISTEN"), "MK_GRAY_stream_listen"},
  {SYMBOL_NAME("STREAM-PEEK-CHAR"), "MK_GRAY_stream_peek_char"},
  {SYMBOL_NAME("STREAM-READ-BYTE"), "MK_GRAY_stream_read_byte"},
  {SYMBOL_NAME("STREAM-READ-CHAR"), "MK_GRAY_stream_read_char"},
  {SYMBOL_NAME("STREAM-READ-CHAR-NO-HANG"), "MK_GRAY_stream_read_char_no_hang"},
  {SYMBOL_NAME("STREAM-READ-LINE"), "MK_GRAY_stream_read_line"},
  {SYMBOL_NAME("STREAM-READ-SEQUENCE"), "MK_GRAY_stream_read_sequence"},
  {SYMBOL_NAME("STREAM-TERPRI"), "MK_GRAY_stream_terpri"},
  {SYMBOL_NAME("STREAM-UNREAD-CHAR"), "MK_GRAY_stream_unread_char"},
  {SYMBOL_NAME("STREAM-WRITE-BYTE"), "MK_GRAY_stream_write_byte"},
  {SYMBOL_NAME("STREAM-WRITE-CHAR"), "MK_GRAY_stream_write_char"},
  {SYMBOL_NAME("STREAM-WRITE-SEQUENCE"), "MK_GRAY_stream_write_sequence"},
  {SYMBOL_NAME("STREAM-WRITE-STRING"), "MK_GRAY_stream_write_string"},
};



const struct mkcl_symbol blank_symbol = BLANK_SYMBOL_INITIALIZER;


void init_gray_package(void)
{
  long i;

  for (i = 0; i < MKCL_NB_ELEMS(mkcl_gray_external_symbol_names); i++)
    {
      struct mkcl_symbol * sym = &mkcl_gray_external_symbols[i];
      const struct mkcl_base_string * name = &mkcl_gray_external_symbol_names[i];
      const mkcl_hash_value hashed_name = hash_base_string(name->self, name->fillp, 0);

      *sym = blank_symbol;

      sym->name = (mkcl_object) name;
      sym->hashed_name = hashed_name;
      sym->hpack = (mkcl_object) &mkcl_package_gray;

      struct mkcl_hashtable_entry * entry = &(external_entries[i]);
      entry->hashed_key = hashed_name;
      entry->value = (mkcl_object) sym;
      entry->key = sym->name;
      entry->next = NULL;
      
      add_entry_to_hash(hashed_name, &external_ht, entry);
    }
  
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_gray_internal_symbol_names); i++)
    {
      struct mkcl_symbol * sym = &mkcl_gray_internal_symbols[i];
      const struct mkcl_base_string * name = &mkcl_gray_internal_symbol_names[i];
      const mkcl_hash_value hashed_name = hash_base_string(name->self, name->fillp, 0);

      *sym = blank_symbol;

      sym->name = (mkcl_object) name;
      sym->hashed_name = hashed_name;
      sym->hpack = (mkcl_object) &mkcl_package_gray;

      struct mkcl_hashtable_entry * entry = &(internal_entries[i]);
      entry->hashed_key = hashed_name;
      entry->value = (mkcl_object) sym;
      entry->key = sym->name;
      entry->next = NULL;

      add_entry_to_hash(hashed_name, &internal_ht, entry);
    }

  for (i = 0; i < MKCL_NB_ELEMS(mkcl_gray_declare_specials); i++)
    {
      int intern_flag;
      struct mkcl_base_string * const name = &mkcl_gray_declare_specials[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_gray, NULL);

      if (sym)
	{
	  sym->value = (mkcl_object) &mkcl_gray_declare_specials[i].value_denotator;
	  sym->stype = mkcl_stp_special;
	}
      else
	{
	  printf("special variable not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_gray_declare_constants); i++)
    {
      int intern_flag;
      struct mkcl_base_string * const name = &mkcl_gray_declare_constants[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_gray, NULL);

      if (sym)
	{
	  sym->value = (mkcl_object) &mkcl_gray_declare_constants[i].value_denotator;
	  sym->stype = mkcl_stp_constant;
	}
      else
	{
	  printf("special variable not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_gray_declare_lisp_functions); i++)
    {
      int intern_flag;
      struct mkcl_base_string * name = &mkcl_gray_declare_lisp_functions[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_gray, NULL);

      if (sym)
	{
	  sym->gfdef = (mkcl_object) &mkcl_gray_declare_lisp_functions[i].function_object_denotator;
	}
      else
	{
	  printf("function not found %s\n", name->self);
	  exit(1);
	}
    }
}



mkcl_index gray_internal_symbol_index(struct mkcl_symbol * sym)
{
  return (sym - mkcl_gray_internal_symbols);
}

mkcl_index gray_external_symbol_index(struct mkcl_symbol * sym)
{
  return (sym - mkcl_gray_external_symbols);
}

mkcl_index gray_internal_entry_index(struct mkcl_hashtable_entry * entry)
{
  return (entry - internal_entries);
}

mkcl_index gray_external_entry_index(struct mkcl_hashtable_entry * entry)
{
  return (entry - external_entries);
}



void print_gray_internal_symbol_C_names(void)
{
  mkcl_index i;

  printf("\nstatic const struct mkcl_base_string gray_internal_C_names[] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      printf("SYMBOL_NAME(\"mkcl_gray_internal_symbols[%lu]\"),\n", i);
    }
  printf("};\n");
}

void print_gray_external_symbol_C_names(void)
{
  mkcl_index i;

  printf("\nstatic const struct mkcl_base_string gray_external_C_names[] = {\n");
  for (i = 0; i < external_count; i++)
    {
      printf("SYMBOL_NAME(\"mkcl_gray_external_symbols[%lu]\"),\n", i);
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

void print_gray_internal_symbol_initializers(void)
{
  mkcl_index i;

  printf("\nstruct mkcl_symbol mkcl_gray_internal_symbols[] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      struct mkcl_symbol * sym = &mkcl_gray_internal_symbols[i];
      
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
      printf("(mkcl_object) &mkcl_gray_internal_symbol_names[%lu], ", i); /* name */
      printf("(mkcl_object) &mkcl_package_gray, "); /* hpack */
      printf("mk_cl_Cnil, "); /* properly_named_class */
      printf("mk_cl_Cnil, "); /* sys_plist */
      printf("MKCL_NOT_A_SPECIAL_INDEX, "); /* special_index */
      printf("%luUL, ", sym->hashed_name); /* hashed_name */
      printf("(mkcl_object) &gray_internal_C_names[%lu], ", i); /* C_name */
      printf("NULL, "); /* _C_name */
      printf("NULL, "); /* _name */
      
      printf("}, /* %lu %s */\n", i, sym->name->base_string.self);
    }
  printf("};\n");
}

void print_gray_external_symbol_initializers(void)
{
  mkcl_index i;

  printf("\nstruct mkcl_symbol mkcl_gray_external_symbols[] = {\n");
  for (i = 0; i < external_count; i++)
    {
      struct mkcl_symbol * sym = &mkcl_gray_external_symbols[i];
      
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
      printf("(mkcl_object) &mkcl_gray_external_symbol_names[%lu], ", i); /* name */
      printf("(mkcl_object) &mkcl_package_gray, "); /* hpack */
      printf("mk_cl_Cnil, "); /* properly_named_class */
      printf("mk_cl_Cnil, "); /* sys_plist */
      printf("MKCL_NOT_A_SPECIAL_INDEX, "); /* special_index */
      printf("%luUL, ", sym->hashed_name); /* hashed_name */
      printf("(mkcl_object) &gray_external_C_names[%lu], ", i); /* C_name */
      printf("NULL, "); /* _C_name */
      printf("NULL, "); /* _name */
      
      printf("}, /* %lu %s */\n", i, sym->name->base_string.self);
    }
  printf("};\n");
}


void print_gray_internal_hashtable_entry_initializers(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry internal_entries[internal_count] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      struct mkcl_hashtable_entry * entry = &internal_entries[i];
      

      printf("{ ");
      if (entry->next)
	printf("&internal_entries[%lu], ", gray_internal_entry_index(entry->next));
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
	  mkcl_index sym_index = gray_internal_symbol_index((struct mkcl_symbol *) entry->value);

	  printf("(mkcl_object) &mkcl_gray_internal_symbol_names[%lu], ", sym_index);
	  printf("%luUL, ", entry->hashed_key);
	  printf("(mkcl_object) &mkcl_gray_internal_symbols[%lu], ", sym_index);
	}
      printf("}, \n");
    }
  printf("};\n");
}

void print_gray_internal_hashtable_vector_initializer(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry * internal_vector[internal_size] = {\n");
  for (i = 0; i < internal_size; i++)
    {
      struct mkcl_hashtable_entry * entry = internal_vector[i];

      if (entry)
	printf("&internal_entries[%lu], \n", gray_internal_entry_index(entry));
      else
	printf("NULL, \n");

    }
  printf("};\n");
}

void print_gray_external_hashtable_entry_initializers(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry external_entries[external_count] = {\n");
  for (i = 0; i < external_count; i++)
    {
      struct mkcl_hashtable_entry * entry = &external_entries[i];
      

      printf("{ ");
      if (entry->next)
	printf("&external_entries[%lu], ", gray_external_entry_index(entry->next));
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
	  mkcl_index sym_index = gray_external_symbol_index((struct mkcl_symbol *) entry->value);

	  printf("(mkcl_object) &mkcl_gray_external_symbol_names[%lu], ", sym_index);
	  printf("%luUL, ", entry->hashed_key);
	  printf("(mkcl_object) &mkcl_gray_external_symbols[%lu], ", sym_index);
	}
      printf("}, \n");
    }
  printf("};\n");
}

void print_gray_external_hashtable_vector_initializer(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry * external_vector[external_size] = {\n");
  for (i = 0; i < external_size; i++)
    {
      struct mkcl_hashtable_entry * entry = external_vector[i];

      if (entry)
	printf("&external_entries[%lu], \n", gray_external_entry_index(entry));
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

void print_gray_package(void)
{
  print_copyright();
  print_gray_internal_symbol_C_names();
  print_gray_internal_symbol_initializers();
  print_gray_external_symbol_C_names();
  print_gray_external_symbol_initializers();
  print_gray_internal_hashtable_entry_initializers();
  print_gray_internal_hashtable_vector_initializer();
  print_gray_external_hashtable_entry_initializers();
  print_gray_external_hashtable_vector_initializer();
}

void expose_gray_symbol(const struct exposed_symbol * exposed_symbol)
{
  bool internalp;
  struct mkcl_symbol * sym = find_symbol(&exposed_symbol->symbol_name, &mkcl_package_gray, &internalp);

  if ( sym == NULL ) { printf("Exposed symbol not found: %s\n", exposed_symbol->symbol_name.self); exit(1); }
  
  printf("#define %s ", exposed_symbol->exposition);
  if (internalp)
    printf("mkcl_gray_internal_symbols[%lu]\n", gray_internal_symbol_index(sym));
  else
    printf("mkcl_gray_external_symbols[%lu]\n", gray_external_symbol_index(sym)); 
}

void expose_gray_package(void)
{
  mkcl_index i;

  print_copyright();
  for (i = 0; i < MKCL_NB_ELEMS(exposed_symbols); i++)
    {
      expose_gray_symbol(&exposed_symbols[i]);
    }
}

int main(int argc, char * argv[])
{
  char * program_name = basename(strdup(argv[0]));
  init_gray_package();
  

  if ( strcmp(program_name, "build_package_GRAY" PROGRAM_SUFFIX) == 0 )
    print_gray_package();
  else if ( strcmp(program_name, "expose_package_GRAY" PROGRAM_SUFFIX) == 0 )
    expose_gray_package();
  else
    { printf("\nDon't know this program name: %s\n", program_name); exit(2); }


  return 0;
}

