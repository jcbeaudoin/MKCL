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

#include "FFI_package.h"


struct mkcl_function_declaration {
  struct mkcl_base_string symbol_name;
  struct mkcl_base_string function_object_denotator;
};

struct mkcl_function_declaration mkcl_ffi_declare_lisp_functions[] = {

};

struct mkcl_special_declaration {
  struct mkcl_base_string symbol_name;
  struct mkcl_base_string value_denotator;
};

struct mkcl_special_declaration mkcl_ffi_declare_specials[] = {
  {SYMBOL_NAME("+NULL-CSTRING-POINTER+"), FUN_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*FFI-TYPES*"), FUN_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*REFERENCED-LIBRARIES*"), FUN_DENOT("MKCL_OBJNULL")},

};


const struct mkcl_symbol blank_symbol = BLANK_SYMBOL_INITIALIZER;



void init_ffi_package(void)
{
  long i;

  for (i = 0; i < MKCL_NB_ELEMS(mkcl_ffi_external_symbol_names); i++)
    {
      struct mkcl_symbol * sym = &mkcl_ffi_external_symbols[i];
      const struct mkcl_base_string * name = &mkcl_ffi_external_symbol_names[i];
      const mkcl_hash_value hashed_name = hash_base_string(name->self, name->fillp, 0);

      *sym = blank_symbol;

      sym->name = (mkcl_object) name;
      sym->hashed_name = hashed_name;
      sym->hpack = (mkcl_object) &mkcl_package_ffi;

      struct mkcl_hashtable_entry * entry = &(external_entries[i]);
      entry->hashed_key = hashed_name;
      entry->value = (mkcl_object) sym;
      entry->key = sym->name;
      entry->next = NULL;
      
      add_entry_to_hash(hashed_name, &external_ht, entry);
    }
  
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_ffi_internal_symbol_names); i++)
    {
      struct mkcl_symbol * sym = &mkcl_ffi_internal_symbols[i];
      const struct mkcl_base_string * name = &mkcl_ffi_internal_symbol_names[i];
      const mkcl_hash_value hashed_name = hash_base_string(name->self, name->fillp, 0);

      *sym = blank_symbol;

      sym->name = (mkcl_object) name;
      sym->hashed_name = hashed_name;
      sym->hpack = (mkcl_object) &mkcl_package_ffi;

      struct mkcl_hashtable_entry * entry = &(internal_entries[i]);
      entry->hashed_key = hashed_name;
      entry->value = (mkcl_object) sym;
      entry->key = sym->name;
      entry->next = NULL;

      add_entry_to_hash(hashed_name, &internal_ht, entry);
    }

  for (i = 0; i < MKCL_NB_ELEMS(mkcl_ffi_declare_specials); i++)
    {
      int intern_flag;
      struct mkcl_base_string * const name = &mkcl_ffi_declare_specials[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_ffi, NULL);

      if (sym)
	{
	  sym->value = (mkcl_object) &mkcl_ffi_declare_specials[i].value_denotator;
	  sym->stype = mkcl_stp_special;
	}
      else
	{
	  printf("special variable not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_ffi_declare_lisp_functions); i++)
    {
      int intern_flag;
      struct mkcl_base_string * name = &mkcl_ffi_declare_lisp_functions[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_ffi, NULL);

      if (sym)
	{
	  sym->gfdef = (mkcl_object) &mkcl_ffi_declare_lisp_functions[i].function_object_denotator;
	}
      else
	{
	  printf("function not found %s\n", name->self);
	  exit(1);
	}
    }
}



mkcl_index ffi_internal_symbol_index(struct mkcl_symbol * sym)
{
  return (sym - mkcl_ffi_internal_symbols);
}

mkcl_index ffi_external_symbol_index(struct mkcl_symbol * sym)
{
  return (sym - mkcl_ffi_external_symbols);
}

mkcl_index ffi_internal_entry_index(struct mkcl_hashtable_entry * entry)
{
  return (entry - internal_entries);
}

mkcl_index ffi_external_entry_index(struct mkcl_hashtable_entry * entry)
{
  return (entry - external_entries);
}



void print_ffi_internal_symbol_C_names(void)
{
  mkcl_index i;

  printf("\nstatic const struct mkcl_base_string ffi_internal_C_names[] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      printf("SYMBOL_NAME(\"mkcl_ffi_internal_symbols[%lu]\"),\n", i);
    }
  printf("};\n");
}

void print_ffi_external_symbol_C_names(void)
{
  mkcl_index i;

  printf("\nstatic const struct mkcl_base_string ffi_external_C_names[] = {\n");
  for (i = 0; i < external_count; i++)
    {
      printf("SYMBOL_NAME(\"mkcl_ffi_external_symbols[%lu]\"),\n", i);
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
    case mkcl_stp_special_operator: return "mkcl_stp_special_operator";
    default: return "";
    }
}

void print_ffi_internal_symbol_initializers(void)
{
  mkcl_index i;

  printf("\nstruct mkcl_symbol mkcl_ffi_internal_symbols[] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      struct mkcl_symbol * sym = &mkcl_ffi_internal_symbols[i];
      
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
      printf("(mkcl_object) &mkcl_ffi_internal_symbol_names[%lu], ", i); /* name */
      printf("(mkcl_object) &mkcl_package_ffi, "); /* hpack */
      printf("mk_cl_Cnil, "); /* properly_named_class */
      printf("mk_cl_Cnil, "); /* sys_plist */
      printf("MKCL_NOT_A_SPECIAL_INDEX, "); /* special_index */
      printf(MKCL_HASH_VALUE_FORMAT ", ", sym->hashed_name); /* hashed_name */
      printf("(mkcl_object) &ffi_internal_C_names[%lu], ", i); /* C_name */
      printf("NULL, "); /* _C_name */
      printf("NULL, "); /* _name */
      
      printf("}, /* %lu %s */\n", i, sym->name->base_string.self);
    }
  printf("};\n");
}

void print_ffi_external_symbol_initializers(void)
{
  mkcl_index i;

  printf("\nstruct mkcl_symbol mkcl_ffi_external_symbols[] = {\n");
  for (i = 0; i < external_count; i++)
    {
      struct mkcl_symbol * sym = &mkcl_ffi_external_symbols[i];
      
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
      printf("(mkcl_object) &mkcl_ffi_external_symbol_names[%lu], ", i); /* name */
      printf("(mkcl_object) &mkcl_package_ffi, "); /* hpack */
      printf("mk_cl_Cnil, "); /* properly_named_class */
      printf("mk_cl_Cnil, "); /* sys_plist */
      printf("MKCL_NOT_A_SPECIAL_INDEX, "); /* special_index */
      printf(MKCL_HASH_VALUE_FORMAT ", ", sym->hashed_name); /* hashed_name */
      printf("(mkcl_object) &ffi_external_C_names[%lu], ", i); /* C_name */
      printf("NULL, "); /* _C_name */
      printf("NULL, "); /* _name */
      
      printf("}, /* %lu %s */\n", i, sym->name->base_string.self);
    }
  printf("};\n");
}


void print_ffi_internal_hashtable_entry_initializers(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry internal_entries[internal_count] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      struct mkcl_hashtable_entry * entry = &internal_entries[i];
      

      printf("{ ");
      if (entry->next)
	printf("&internal_entries[%lu], ", ffi_internal_entry_index(entry->next));
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
	  mkcl_index sym_index = ffi_internal_symbol_index((struct mkcl_symbol *) entry->value);

	  printf("(mkcl_object) &mkcl_ffi_internal_symbol_names[%lu], ", sym_index);
	  printf(MKCL_HASH_VALUE_FORMAT ", ", entry->hashed_key);
	  printf("(mkcl_object) &mkcl_ffi_internal_symbols[%lu], ", sym_index);
	}
      printf("}, \n");
    }
  printf("};\n");
}

void print_ffi_internal_hashtable_vector_initializer(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry * internal_vector[internal_size] = {\n");
  for (i = 0; i < internal_size; i++)
    {
      struct mkcl_hashtable_entry * entry = internal_vector[i];

      if (entry)
	printf("&internal_entries[%lu], \n", ffi_internal_entry_index(entry));
      else
	printf("NULL, \n");

    }
  printf("};\n");
}

void print_ffi_external_hashtable_entry_initializers(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry external_entries[external_count] = {\n");
  for (i = 0; i < external_count; i++)
    {
      struct mkcl_hashtable_entry * entry = &external_entries[i];
      

      printf("{ ");
      if (entry->next)
	printf("&external_entries[%lu], ", ffi_external_entry_index(entry->next));
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
	  mkcl_index sym_index = ffi_external_symbol_index((struct mkcl_symbol *) entry->value);

	  printf("(mkcl_object) &mkcl_ffi_external_symbol_names[%lu], ", sym_index);
	  printf(MKCL_HASH_VALUE_FORMAT ", ", entry->hashed_key);
	  printf("(mkcl_object) &mkcl_ffi_external_symbols[%lu], ", sym_index);
	}
      printf("}, \n");
    }
  printf("};\n");
}

void print_ffi_external_hashtable_vector_initializer(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry * external_vector[external_size] = {\n");
  for (i = 0; i < external_size; i++)
    {
      struct mkcl_hashtable_entry * entry = external_vector[i];

      if (entry)
	printf("&external_entries[%lu], \n", ffi_external_entry_index(entry));
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

void print_ffi_package(void)
{
  print_copyright();
  print_ffi_internal_symbol_C_names();
  print_ffi_internal_symbol_initializers();
  print_ffi_external_symbol_C_names();
  print_ffi_external_symbol_initializers();
  print_ffi_internal_hashtable_entry_initializers();
  print_ffi_internal_hashtable_vector_initializer();
  print_ffi_external_hashtable_entry_initializers();
  print_ffi_external_hashtable_vector_initializer();
}

static struct mkcl_base_string const extra_exposed_symbols[] = {
  /*  SYMBOL_NAME("SOMETHING"), */
};

void expose_ffi_symbol(const struct mkcl_base_string * sym_name)
{
  bool internalp;
  struct mkcl_symbol * sym = find_symbol(sym_name, &mkcl_package_ffi, &internalp);

  printf("#define MK_FFI_%s ", sym_name->self);
  if (internalp)
    printf("mkcl_ffi_internal_symbols[%lu]\n", ffi_internal_symbol_index(sym));
  else
    printf("mkcl_ffi_external_symbols[%lu]\n", ffi_external_symbol_index(sym)); 
}

void expose_ffi_package(void)
{
  mkcl_index i;

  print_copyright();
  for (i = 0; i < MKCL_NB_ELEMS(extra_exposed_symbols); i++)
    {
      expose_ffi_symbol(&extra_exposed_symbols[i]);
    }
}


int main(int argc, char * argv[])
{
  char * program_name = basename(strdup(argv[0]));
  init_ffi_package();

#if 0
  if (argc != 2) { printf("\n%s usage: %s <output filename>\n", program_name, program_name); exit(1); }
  
  printf("\nAbout to redirect to: %s\n", argv[1]);
  FILE * new_stdout = freopen(argv[1], "w", stdout);

  if (new_stdout == NULL) { perror("Output redirection failed!"); exit(2); }
#endif
  
  if ( strcmp(program_name, "build_package_FFI" PROGRAM_SUFFIX) == 0 )
    print_ffi_package();
  else if ( strcmp(program_name, "expose_package_FFI" PROGRAM_SUFFIX) == 0 )
    expose_ffi_package();
  else
    { printf("\nDon't know this program name: %s\n", program_name); exit(2); }

  return 0;
}

