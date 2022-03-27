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


#include "CLOS_package.h"


struct mkcl_function_declaration {
  struct mkcl_base_string symbol_name;
  struct mkcl_base_string function_object_denotator;
};

struct mkcl_function_declaration mkcl_clos_declare_lisp_functions[] = {
  {SYMBOL_NAME("FUNCALLABLE-STANDARD-INSTANCE-ACCESS"), FUN_DENOT("&mk_clos_funcallable_standard_instance_access_cfunobj")},
  {SYMBOL_NAME("SET-FUNCALLABLE-INSTANCE-FUNCTION"), FUN_DENOT("&mk_clos_set_funcallable_instance_function_cfunobj")},
};

struct mkcl_function_declaration mkcl_clos_declare_macros[] = {
};

struct mkcl_function_declaration mkcl_clos_declare_special_operators[] = {
};

struct mkcl_variable_declaration {
  struct mkcl_base_string symbol_name;
  struct mkcl_base_string value_denotator;
};

struct mkcl_variable_declaration mkcl_clos_declare_specials[] = {
  {SYMBOL_NAME("*BUILTIN-CLASSES*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*REDEFINE-CLASS-IN-PLACE*"), VAL_DENOT("mk_cl_Cnil")},
  {SYMBOL_NAME("*WARN-ON-REDEFINITION*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("+INSIDE-MAKE-INSTANCE+"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("+METADATA-LOCK+"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*EARLY-METHODS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*NEXT-METHODS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME(".COMBINED-METHOD-ARGS."), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*METHOD-COMBINATIONS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("+CACHED-SHARED-INITIALIZE-EMFUN+"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("+CACHED-CLASS-SLOTS+"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("+CACHED-CLASS-SIZE+"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("+CACHED-CLASS+"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("+INSTANCE-TO-INITIALIZE+"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*OPTIMIZE-SLOT-ACCESS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*TRACE-FINALIZE-INHERITANCE*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*MIGRATE-METHODS-ON-CLASS-REDEFINITION*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*MIGRATE-SUBCLASSES-ON-CLASS-REDEFINITION*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*WARN-ON-FORWARD-REFERENCED-CLASS*"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("*LOAD-FORM-CACHE*"), VAL_DENOT("MKCL_OBJNULL")},
};

struct mkcl_variable_declaration mkcl_clos_declare_constants[] = {
};

struct exposed_symbol {
  struct mkcl_base_string symbol_name;
  char * exposition;
};

static struct exposed_symbol const exposed_symbols[] = {
  /*  { SYMBOL_NAME("SOMETHING"), "MK_CLOS_something" }, */
  {SYMBOL_NAME("COMPUTE-EFFECTIVE-METHOD-FOR-CACHE"), "MK_CLOS_compute_effective_method_for_cache"},
  {SYMBOL_NAME("FUNCALLABLE-STANDARD-INSTANCE-ACCESS"), "MK_CLOS_funcallable_standard_instance_access"},
  {SYMBOL_NAME("SET-FUNCALLABLE-INSTANCE-FUNCTION"), "MK_CLOS_set_funcallable_instance_function"},
  {SYMBOL_NAME("*BUILTIN-CLASSES*"), "MK_CLOS_DYNVAR_builtin_classes"},
  {SYMBOL_NAME("*REDEFINE-CLASS-IN-PLACE*"), "MK_CLOS_DYNVAR_redefine_class_in_place"},
};



const struct mkcl_symbol blank_symbol = BLANK_SYMBOL_INITIALIZER;


void init_clos_package(void)
{
  long i;

  for (i = 0; i < MKCL_NB_ELEMS(mkcl_clos_external_symbol_names); i++)
    {
      struct mkcl_symbol * sym = &mkcl_clos_external_symbols[i];
      const struct mkcl_base_string * name = &mkcl_clos_external_symbol_names[i];
      const mkcl_hash_value hashed_name = hash_base_string(name->self, name->fillp, 0);

      *sym = blank_symbol;

      sym->name = (mkcl_object) name;
      sym->hashed_name = hashed_name;
      sym->hpack = (mkcl_object) &mkcl_package_clos;

      struct mkcl_hashtable_entry * entry = &(external_entries[i]);
      entry->hashed_key = hashed_name;
      entry->value = (mkcl_object) sym;
      entry->key = sym->name;
      entry->next = NULL;
      
      add_entry_to_hash(hashed_name, &external_ht, entry);
    }
  
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_clos_internal_symbol_names); i++)
    {
      struct mkcl_symbol * sym = &mkcl_clos_internal_symbols[i];
      const struct mkcl_base_string * name = &mkcl_clos_internal_symbol_names[i];
      const mkcl_hash_value hashed_name = hash_base_string(name->self, name->fillp, 0);

      *sym = blank_symbol;

      sym->name = (mkcl_object) name;
      sym->hashed_name = hashed_name;
      sym->hpack = (mkcl_object) &mkcl_package_clos;

      struct mkcl_hashtable_entry * entry = &(internal_entries[i]);
      entry->hashed_key = hashed_name;
      entry->value = (mkcl_object) sym;
      entry->key = sym->name;
      entry->next = NULL;

      add_entry_to_hash(hashed_name, &internal_ht, entry);
    }

  for (i = 0; i < MKCL_NB_ELEMS(mkcl_clos_declare_specials); i++)
    {
      int intern_flag;
      struct mkcl_base_string * const name = &mkcl_clos_declare_specials[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_clos, NULL);

      if (sym)
	{
	  sym->value = (mkcl_object) &mkcl_clos_declare_specials[i].value_denotator;
	  sym->stype = mkcl_stp_special;
	}
      else
	{
	  printf("special variable not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_clos_declare_constants); i++)
    {
      int intern_flag;
      struct mkcl_base_string * const name = &mkcl_clos_declare_constants[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_clos, NULL);

      if (sym)
	{
	  sym->value = (mkcl_object) &mkcl_clos_declare_constants[i].value_denotator;
	  sym->stype = mkcl_stp_constant;
	}
      else
	{
	  printf("special variable not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_clos_declare_lisp_functions); i++)
    {
      int intern_flag;
      struct mkcl_base_string * name = &mkcl_clos_declare_lisp_functions[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_clos, NULL);

      if (sym)
	{
	  sym->gfdef = (mkcl_object) &mkcl_clos_declare_lisp_functions[i].function_object_denotator;
	}
      else
	{
	  printf("function not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_clos_declare_macros); i++)
    {
      int intern_flag;
      struct mkcl_base_string * name = &mkcl_clos_declare_macros[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_clos, NULL);

      if (sym)
	{
	  sym->stype |= mkcl_stp_macro;
	  sym->gfdef = (mkcl_object) &mkcl_clos_declare_macros[i].function_object_denotator;
	}
      else
	{
	  printf("function not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_clos_declare_special_operators); i++)
    {
      int intern_flag;
      struct mkcl_base_string * name = &mkcl_clos_declare_special_operators[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_clos, NULL);

      if (sym)
	{
	  sym->stype |= mkcl_stp_special_form;
	  sym->gfdef = (mkcl_object) &mkcl_clos_declare_special_operators[i].function_object_denotator;
	}
      else
	{
	  printf("function not found %s\n", name->self);
	  exit(1);
	}
    }
}



mkcl_index clos_internal_symbol_index(struct mkcl_symbol * sym)
{
  return (sym - mkcl_clos_internal_symbols);
}

mkcl_index clos_external_symbol_index(struct mkcl_symbol * sym)
{
  return (sym - mkcl_clos_external_symbols);
}

mkcl_index clos_internal_entry_index(struct mkcl_hashtable_entry * entry)
{
  return (entry - internal_entries);
}

mkcl_index clos_external_entry_index(struct mkcl_hashtable_entry * entry)
{
  return (entry - external_entries);
}



void print_clos_internal_symbol_C_names(void)
{
  mkcl_index i;

  printf("\nstatic const struct mkcl_base_string clos_internal_C_names[] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      printf("SYMBOL_NAME(\"mkcl_clos_internal_symbols[%lu]\"),\n", i);
    }
  printf("};\n");
}

void print_clos_external_symbol_C_names(void)
{
  mkcl_index i;

  printf("\nstatic const struct mkcl_base_string clos_external_C_names[] = {\n");
  for (i = 0; i < external_count; i++)
    {
      printf("SYMBOL_NAME(\"mkcl_clos_external_symbols[%lu]\"),\n", i);
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

void print_clos_internal_symbol_initializers(void)
{
  mkcl_index i;

  printf("\nstruct mkcl_symbol mkcl_clos_internal_symbols[] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      struct mkcl_symbol * sym = &mkcl_clos_internal_symbols[i];
      
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
      printf("(mkcl_object) &mkcl_clos_internal_symbol_names[%lu], ", i); /* name */
      printf("(mkcl_object) &mkcl_package_clos, "); /* hpack */
      printf("mk_cl_Cnil, "); /* properly_named_class */
      printf("mk_cl_Cnil, "); /* sys_plist */
      printf("MKCL_NOT_A_SPECIAL_INDEX, "); /* special_index */
      printf("%luUL, ", sym->hashed_name); /* hashed_name */
      printf("(mkcl_object) &clos_internal_C_names[%lu], ", i); /* C_name */
      printf("NULL, "); /* _C_name */
      printf("NULL, "); /* _name */
      
      printf("}, /* %lu %s */\n", i, sym->name->base_string.self);
    }
  printf("};\n");
}

void print_clos_external_symbol_initializers(void)
{
  mkcl_index i;

  printf("\nstruct mkcl_symbol mkcl_clos_external_symbols[] = {\n");
  for (i = 0; i < external_count; i++)
    {
      struct mkcl_symbol * sym = &mkcl_clos_external_symbols[i];
      
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
      printf("(mkcl_object) &mkcl_clos_external_symbol_names[%lu], ", i); /* name */
      printf("(mkcl_object) &mkcl_package_clos, "); /* hpack */
      printf("mk_cl_Cnil, "); /* properly_named_class */
      printf("mk_cl_Cnil, "); /* sys_plist */
      printf("MKCL_NOT_A_SPECIAL_INDEX, "); /* special_index */
      printf("%luUL, ", sym->hashed_name); /* hashed_name */
      printf("(mkcl_object) &clos_external_C_names[%lu], ", i); /* C_name */
      printf("NULL, "); /* _C_name */
      printf("NULL, "); /* _name */
      
      printf("}, /* %lu %s */\n", i, sym->name->base_string.self);
    }
  printf("};\n");
}


void print_clos_internal_hashtable_entry_initializers(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry internal_entries[internal_count] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      struct mkcl_hashtable_entry * entry = &internal_entries[i];
      

      printf("{ ");
      if (entry->next)
	printf("&internal_entries[%lu], ", clos_internal_entry_index(entry->next));
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
	  mkcl_index sym_index = clos_internal_symbol_index((struct mkcl_symbol *) entry->value);

	  printf("(mkcl_object) &mkcl_clos_internal_symbol_names[%lu], ", sym_index);
	  printf("%luUL, ", entry->hashed_key);
	  printf("(mkcl_object) &mkcl_clos_internal_symbols[%lu], ", sym_index);
	}
      printf("}, \n");
    }
  printf("};\n");
}

void print_clos_internal_hashtable_vector_initializer(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry * internal_vector[internal_size] = {\n");
  for (i = 0; i < internal_size; i++)
    {
      struct mkcl_hashtable_entry * entry = internal_vector[i];

      if (entry)
	printf("&internal_entries[%lu], \n", clos_internal_entry_index(entry));
      else
	printf("NULL, \n");

    }
  printf("};\n");
}

void print_clos_external_hashtable_entry_initializers(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry external_entries[external_count] = {\n");
  for (i = 0; i < external_count; i++)
    {
      struct mkcl_hashtable_entry * entry = &external_entries[i];
      

      printf("{ ");
      if (entry->next)
	printf("&external_entries[%lu], ", clos_external_entry_index(entry->next));
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
	  mkcl_index sym_index = clos_external_symbol_index((struct mkcl_symbol *) entry->value);

	  printf("(mkcl_object) &mkcl_clos_external_symbol_names[%lu], ", sym_index);
	  printf("%luUL, ", entry->hashed_key);
	  printf("(mkcl_object) &mkcl_clos_external_symbols[%lu], ", sym_index);
	}
      printf("}, \n");
    }
  printf("};\n");
}

void print_clos_external_hashtable_vector_initializer(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry * external_vector[external_size] = {\n");
  for (i = 0; i < external_size; i++)
    {
      struct mkcl_hashtable_entry * entry = external_vector[i];

      if (entry)
	printf("&external_entries[%lu], \n", clos_external_entry_index(entry));
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

void print_clos_package(void)
{
  print_copyright();
  print_clos_internal_symbol_C_names();
  print_clos_internal_symbol_initializers();
  print_clos_external_symbol_C_names();
  print_clos_external_symbol_initializers();
  print_clos_internal_hashtable_entry_initializers();
  print_clos_internal_hashtable_vector_initializer();
  print_clos_external_hashtable_entry_initializers();
  print_clos_external_hashtable_vector_initializer();
}

void expose_clos_symbol(const struct exposed_symbol * exposed_symbol)
{
  bool internalp;
  struct mkcl_symbol * sym = find_symbol(&exposed_symbol->symbol_name, &mkcl_package_clos, &internalp);

  if ( sym == NULL ) { printf("Exposed symbol not found: %s\n", exposed_symbol->symbol_name.self); exit(1); }

  printf("#define %s ", exposed_symbol->exposition);
  if (internalp)
    printf("mkcl_clos_internal_symbols[%lu]\n", clos_internal_symbol_index(sym));
  else
    printf("mkcl_clos_external_symbols[%lu]\n", clos_external_symbol_index(sym)); 
}

void expose_clos_package(void)
{
  mkcl_index i;

  print_copyright();
  for (i = 0; i < MKCL_NB_ELEMS(exposed_symbols); i++)
    {
      expose_clos_symbol(&exposed_symbols[i]);
    }
}

int main(int argc, char * argv[])
{
  char * program_name = basename(strdup(argv[0]));
  init_clos_package();
  

  if ( strcmp(program_name, "build_package_CLOS" PROGRAM_SUFFIX) == 0 )
    print_clos_package();
  else if ( strcmp(program_name, "expose_package_CLOS" PROGRAM_SUFFIX) == 0 )
    expose_clos_package();
  else
    { printf("\nDon't know this program name: %s\n", program_name); exit(2); }


  return 0;
}

