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


#include "MT_package.h"


struct mkcl_function_declaration {
  struct mkcl_base_string symbol_name;
  struct mkcl_base_string function_object_denotator;
};

struct mkcl_function_declaration mkcl_mt_declare_lisp_functions[] = {
  {SYMBOL_NAME("ALL-THREADS"), FUN_DENOT("&mk_mt_all_threads_cfunobj")},
  {SYMBOL_NAME("EXIT-THREAD"), FUN_DENOT("&mk_mt_exit_thread_cfunobj")},
  {SYMBOL_NAME("TERMINATE-THREAD"), FUN_DENOT("&mk_mt_terminate_thread_cfunobj")},
  {SYMBOL_NAME("MAKE-THREAD"), FUN_DENOT("&mk_mt_make_thread_cfunobj")},
  {SYMBOL_NAME("THREAD-ACTIVE-P"), FUN_DENOT("&mk_mt_thread_active_p_cfunobj")},
  {SYMBOL_NAME("THREAD-ENABLE"), FUN_DENOT("&mk_mt_thread_enable_cfunobj")},
  {SYMBOL_NAME("THREAD-YIELD"), FUN_DENOT("&mk_mt_thread_yield_cfunobj")},
  {SYMBOL_NAME("THREAD-KILL"), FUN_DENOT("&mk_mt_thread_kill_cfunobj")},
  {SYMBOL_NAME("THREAD-DETACH"), FUN_DENOT("&mk_mt_thread_detach_cfunobj")},
  {SYMBOL_NAME("THREAD-JOIN"), FUN_DENOT("&mk_mt_thread_join_cfunobj")},
  {SYMBOL_NAME("THREAD-NAME"), FUN_DENOT("&mk_mt_thread_name_cfunobj")},
  {SYMBOL_NAME("THREAD-PRESET"), FUN_DENOT("&mk_mt_thread_preset_cfunobj")},
  {SYMBOL_NAME("SHOW-SIGMASK"), FUN_DENOT("&mk_mt_show_sigmask_cfunobj")},
  {SYMBOL_NAME("RESET-SIGMASK"), FUN_DENOT("&mk_mt_reset_sigmask_cfunobj")},
  {SYMBOL_NAME("BLOCK-SIGNALS"), FUN_DENOT("&mk_mt_block_signals_cfunobj")},
  {SYMBOL_NAME("UNBLOCK-SIGNALS"), FUN_DENOT("&mk_mt_unblock_signals_cfunobj")},
  {SYMBOL_NAME("THREAD-RUN-FUNCTION"), FUN_DENOT("&mk_mt_thread_run_function_cfunobj")},
  {SYMBOL_NAME("MAKE-LOCK"), FUN_DENOT("&mk_mt_make_lock_cfunobj")},
  {SYMBOL_NAME("RECURSIVE-LOCK-P"), FUN_DENOT("&mk_mt_recursive_lock_p_cfunobj")},
  {SYMBOL_NAME("LOCK-NAME"), FUN_DENOT("&mk_mt_lock_name_cfunobj")},
  {SYMBOL_NAME("LOCK-HOLDER"), FUN_DENOT("&mk_mt_lock_holder_cfunobj")},
  {SYMBOL_NAME("GET-LOCK"), FUN_DENOT("&mk_mt_get_lock_cfunobj")},
  {SYMBOL_NAME("GIVEUP-LOCK"), FUN_DENOT("&mk_mt_giveup_lock_cfunobj")},
  {SYMBOL_NAME("MAKE-RWLOCK"), FUN_DENOT("&mk_mt_make_rwlock_cfunobj")},
  {SYMBOL_NAME("GIVEUP-RWLOCK"), FUN_DENOT("&mk_mt_giveup_rwlock_cfunobj")},
  {SYMBOL_NAME("GET-READ-RWLOCK"), FUN_DENOT("&mk_mt_get_read_rwlock_cfunobj")},
  {SYMBOL_NAME("GET-WRITE-RWLOCK"), FUN_DENOT("&mk_mt_get_write_rwlock_cfunobj")},
  {SYMBOL_NAME("MAKE-SEMAPHORE"), FUN_DENOT("&mk_mt_make_semaphore_cfunobj")},
  {SYMBOL_NAME("SEMAPHORE-COUNT"), FUN_DENOT("&mk_mt_semaphore_count_cfunobj")},
  {SYMBOL_NAME("SEMAPHORE-SIGNAL"), FUN_DENOT("&mk_mt_semaphore_signal_cfunobj")},
  {SYMBOL_NAME("SEMAPHORE-WAIT"), FUN_DENOT("&mk_mt_semaphore_wait_cfunobj")},
  {SYMBOL_NAME("MAKE-CONDITION-VARIABLE"), FUN_DENOT("&mk_mt_make_condition_variable_cfunobj")},
  {SYMBOL_NAME("CONDITION-WAIT"), FUN_DENOT("&mk_mt_condition_wait_cfunobj")},
  {SYMBOL_NAME("CONDITION-SIGNAL"), FUN_DENOT("&mk_mt_condition_signal_cfunobj")},
  {SYMBOL_NAME("CONDITION-BROADCAST"), FUN_DENOT("&mk_mt_condition_broadcast_cfunobj")},
  {SYMBOL_NAME("INTERRUPT-THREAD"), FUN_DENOT("&mk_mt_interrupt_thread_cfunobj")},
  {SYMBOL_NAME("JOIN-THREAD"), FUN_DENOT("&mk_mt_thread_join_cfunobj")},
  {SYMBOL_NAME("DETACH-THREAD"), FUN_DENOT("&mk_mt_thread_detach_cfunobj")},
  {SYMBOL_NAME("THREAD-PLIST"), FUN_DENOT("&mk_mt_thread_plist_cfunobj")},
  {SYMBOL_NAME("SET-THREAD-PLIST"), FUN_DENOT("&mk_mt_set_thread_plist_cfunobj")},
  {SYMBOL_NAME("ABANDON-THREAD"), FUN_DENOT("&mk_mt_abandon_thread_cfunobj")},
  {SYMBOL_NAME("CURRENT-THREAD"), FUN_DENOT("&mk_mt_current_thread_cfunobj")},
  {SYMBOL_NAME("CANCEL-THREAD"), FUN_DENOT("&mk_mt_cancel_thread_cfunobj")},
  {SYMBOL_NAME("TRY-TO-WAKE-UP-THREAD"), FUN_DENOT("&mk_mt_try_to_wake_up_thread_cfunobj")},
  {SYMBOL_NAME("REQUEST-THREAD-SHUTDOWN"), FUN_DENOT("&mk_mt_request_thread_shutdown_cfunobj")},
  {SYMBOL_NAME("THREAD-SHUTDOWN-REQUESTED-P"), FUN_DENOT("&mk_mt_thread_shutdown_requested_p_cfunobj")},
  {SYMBOL_NAME("THREAD-STATUS"), FUN_DENOT("&mk_mt_thread_status_cfunobj")},
  {SYMBOL_NAME("ABORT-THREAD"), FUN_DENOT("&mk_mt_abort_thread_cfunobj")},
};

struct mkcl_variable_declaration {
  struct mkcl_base_string symbol_name;
  struct mkcl_base_string value_denotator;
};

struct mkcl_variable_declaration mkcl_mt_declare_specials[] = {
  {SYMBOL_NAME("*THREAD*"), VAL_DENOT("MKCL_OBJNULL")},
};

struct mkcl_variable_declaration mkcl_mt_declare_constants[] = {
  {SYMBOL_NAME("+LOAD-COMPILE-LOCK+"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("WITH-LOCK"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("WITHOUT-LOCK"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("WITHOUT-INTERRUPTS"), VAL_DENOT("MKCL_OBJNULL")},
  {SYMBOL_NAME("+FORWARD-REFERENCE-LOCK+"), VAL_DENOT("MKCL_OBJNULL")},
};

struct exposed_symbol {
  struct mkcl_base_string symbol_name;
  char * exposition;
};

static struct exposed_symbol const exposed_symbols[] = {
  /*  { SYMBOL_NAME("SOMETHING"), "MK_MT_something" }, */
  { SYMBOL_NAME("ALL-THREADS"), "MK_MT_all_threads" },
  { SYMBOL_NAME("EXIT-THREAD"), "MK_MT_exit_thread" },
  { SYMBOL_NAME("TERMINATE-THREAD"), "MK_MT_terminate_thread" },
  { SYMBOL_NAME("MAKE-THREAD"), "MK_MT_make_thread" },
  { SYMBOL_NAME("THREAD-ACTIVE-P"), "MK_MT_thread_active_p" },
  { SYMBOL_NAME("THREAD-ENABLE"), "MK_MT_thread_enable" },
  { SYMBOL_NAME("THREAD-YIELD"), "MK_MT_thread_yield" },
  { SYMBOL_NAME("THREAD-KILL"), "MK_MT_thread_kill" },
  { SYMBOL_NAME("THREAD-DETACH"), "MK_MT_thread_detach" },
  { SYMBOL_NAME("THREAD-JOIN"), "MK_MT_thread_join" },
  { SYMBOL_NAME("THREAD-NAME"), "MK_MT_thread_name" },
  { SYMBOL_NAME("THREAD-PRESET"), "MK_MT_thread_preset" },
  { SYMBOL_NAME("SHOW-SIGMASK"), "MK_MT_show_sigmask" },
  { SYMBOL_NAME("RESET-SIGMASK"), "MK_MT_reset_sigmask" },
  { SYMBOL_NAME("BLOCK-SIGNALS"), "MK_MT_block_signals" },
  { SYMBOL_NAME("UNBLOCK-SIGNALS"), "MK_MT_unblock_signals" },
  { SYMBOL_NAME("THREAD-RUN-FUNCTION"), "MK_MT_thread_run_function" },
  { SYMBOL_NAME("MAKE-LOCK"), "MK_MT_make_lock" },
  { SYMBOL_NAME("RECURSIVE-LOCK-P"), "MK_MT_recursive_lock_p" },
  { SYMBOL_NAME("LOCK-NAME"), "MK_MT_lock_name" },
  { SYMBOL_NAME("LOCK-HOLDER"), "MK_MT_lock_holder" },
  { SYMBOL_NAME("GET-LOCK"), "MK_MT_get_lock" },
  { SYMBOL_NAME("GIVEUP-LOCK"), "MK_MT_giveup_lock" },
  { SYMBOL_NAME("MAKE-RWLOCK"), "MK_MT_make_rwlock" },
  { SYMBOL_NAME("GIVEUP-RWLOCK"), "MK_MT_giveup_rwlock" },
  { SYMBOL_NAME("GET-READ-RWLOCK"), "MK_MT_get_read_rwlock" },
  { SYMBOL_NAME("GET-WRITE-RWLOCK"), "MK_MT_get_write_rwlock" },
  { SYMBOL_NAME("MAKE-SEMAPHORE"), "MK_MT_make_semaphore" },
  { SYMBOL_NAME("SEMAPHORE-COUNT"), "MK_MT_semaphore_count" },
  { SYMBOL_NAME("SEMAPHORE-SIGNAL"), "MK_MT_semaphore_signal" },
  { SYMBOL_NAME("SEMAPHORE-WAIT"), "MK_MT_semaphore_wait" },
  { SYMBOL_NAME("MAKE-CONDITION-VARIABLE"), "MK_MT_make_condition_variable" },
  { SYMBOL_NAME("CONDITION-WAIT"), "MK_MT_condition_wait" },
  { SYMBOL_NAME("CONDITION-SIGNAL"), "MK_MT_condition_signal" },
  { SYMBOL_NAME("CONDITION-BROADCAST"), "MK_MT_condition_broadcast" },
  { SYMBOL_NAME("INTERRUPT-THREAD"), "MK_MT_interrupt_thread" },
  { SYMBOL_NAME("JOIN-THREAD"), "MK_MT_join_thread" },
  { SYMBOL_NAME("DETACH-THREAD"), "MK_MT_detach_thread" },
  { SYMBOL_NAME("THREAD-PLIST"), "MK_MT_thread_plist" },
  { SYMBOL_NAME("SET-THREAD-PLIST"), "MK_MT_set_thread_plist" },
  { SYMBOL_NAME("ABANDON-THREAD"), "MK_MT_abandon_thread" },
  { SYMBOL_NAME("CURRENT-THREAD"), "MK_MT_current_thread" },
  { SYMBOL_NAME("CANCEL-THREAD"), "MK_MT_cancel_thread" },
  { SYMBOL_NAME("TRY-TO-WAKE-UP-THREAD"), "MK_MT_try_to_wake_up_thread" },
  { SYMBOL_NAME("REQUEST-THREAD-SHUTDOWN"), "MK_MT_request_thread_shutdown" },
  { SYMBOL_NAME("THREAD-SHUTDOWN-REQUESTED-P"), "MK_MT_thread_shutdown_requested_p" },
  { SYMBOL_NAME("THREAD-STATUS"), "MK_MT_thread_status" },
  { SYMBOL_NAME("ABORT-THREAD"), "MK_MT_abort_thread" },
  { SYMBOL_NAME("INTERRUPT-REFUSED"), "MK_MT_interrupt_refused" },
  { SYMBOL_NAME("INVALID-THREAD"), "MK_MT_invalid_thread" },
  { SYMBOL_NAME("THREAD"), "MK_MT_thread" },
  { SYMBOL_NAME("LOCK"), "MK_MT_lock" },
  { SYMBOL_NAME("RWLOCK"), "MK_MT_rwlock" },
  { SYMBOL_NAME("SEMAPHORE"), "MK_MT_semaphore" },
  { SYMBOL_NAME("CONDITION-VARIABLE"), "MK_MT_condition_variable" },
  { SYMBOL_NAME("THREAD-SLEEPING"), "MK_MT_thread_sleeping" },
  { SYMBOL_NAME("*THREAD*"), "MK_MT_DYNVAR_thread" },
  { SYMBOL_NAME("+FORWARD-REFERENCE-LOCK+"), "MK_MT_CONSTANT_forward_reference_lock" },
  { SYMBOL_NAME("+LOAD-COMPILE-LOCK+"), "MK_MT_CONSTANT_load_compile_lock" },
};



const struct mkcl_symbol blank_symbol = BLANK_SYMBOL_INITIALIZER;


void init_mt_package(void)
{
  long i;

  for (i = 0; i < MKCL_NB_ELEMS(mkcl_mt_external_symbol_names); i++)
    {
      struct mkcl_symbol * sym = &mkcl_mt_external_symbols[i];
      const struct mkcl_base_string * name = &mkcl_mt_external_symbol_names[i];
      const mkcl_hash_value hashed_name = hash_base_string(name->self, name->fillp, 0);

      *sym = blank_symbol;

      sym->name = (mkcl_object) name;
      sym->hashed_name = hashed_name;
      sym->hpack = (mkcl_object) &mkcl_package_mt;

#if 0
      sym->C_name = C_name_for_symbol("mkcl_mt_external_symbols", i);
      sym->_C_name = sym->C_name->base_string.self;
#endif

      struct mkcl_hashtable_entry * entry = &(external_entries[i]);
      entry->hashed_key = hashed_name;
      entry->value = (mkcl_object) sym;
      entry->key = sym->name;
      entry->next = NULL;
      
      add_entry_to_hash(hashed_name, &external_ht, entry);
    }
  
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_mt_internal_symbol_names); i++)
    {
      struct mkcl_symbol * sym = &mkcl_mt_internal_symbols[i];
      const struct mkcl_base_string * name = &mkcl_mt_internal_symbol_names[i];
      const mkcl_hash_value hashed_name = hash_base_string(name->self, name->fillp, 0);

      *sym = blank_symbol;

      sym->name = (mkcl_object) name;
      sym->hashed_name = hashed_name;
      sym->hpack = (mkcl_object) &mkcl_package_mt;

      struct mkcl_hashtable_entry * entry = &(internal_entries[i]);
      entry->hashed_key = hashed_name;
      entry->value = (mkcl_object) sym;
      entry->key = sym->name;
      entry->next = NULL;

      add_entry_to_hash(hashed_name, &internal_ht, entry);
    }

  for (i = 0; i < MKCL_NB_ELEMS(mkcl_mt_declare_specials); i++)
    {
      int intern_flag;
      struct mkcl_base_string * const name = &mkcl_mt_declare_specials[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_mt, NULL);

      if (sym)
	{
	  sym->value = (mkcl_object) &mkcl_mt_declare_specials[i].value_denotator;
	  sym->stype = mkcl_stp_special;
	}
      else
	{
	  printf("special variable not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_mt_declare_constants); i++)
    {
      int intern_flag;
      struct mkcl_base_string * const name = &mkcl_mt_declare_constants[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_mt, NULL);

      if (sym)
	{
	  sym->value = (mkcl_object) &mkcl_mt_declare_constants[i].value_denotator;
	  sym->stype = mkcl_stp_constant;
	}
      else
	{
	  printf("special variable not found %s\n", name->self);
	  exit(1);
	}
    }
  for (i = 0; i < MKCL_NB_ELEMS(mkcl_mt_declare_lisp_functions); i++)
    {
      int intern_flag;
      struct mkcl_base_string * name = &mkcl_mt_declare_lisp_functions[i].symbol_name;

      struct mkcl_symbol * sym = find_symbol(name, &mkcl_package_mt, NULL);

      if (sym)
	{
	  sym->gfdef = (mkcl_object) &mkcl_mt_declare_lisp_functions[i].function_object_denotator;
	}
      else
	{
	  printf("function not found %s\n", name->self);
	  exit(1);
	}
    }
}



mkcl_index mt_internal_symbol_index(struct mkcl_symbol * sym)
{
  return (sym - mkcl_mt_internal_symbols);
}

mkcl_index mt_external_symbol_index(struct mkcl_symbol * sym)
{
  return (sym - mkcl_mt_external_symbols);
}

mkcl_index mt_internal_entry_index(struct mkcl_hashtable_entry * entry)
{
  return (entry - internal_entries);
}

mkcl_index mt_external_entry_index(struct mkcl_hashtable_entry * entry)
{
  return (entry - external_entries);
}



void print_mt_internal_symbol_C_names(void)
{
  mkcl_index i;

  printf("\nstatic const struct mkcl_base_string mt_internal_C_names[] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      printf("SYMBOL_NAME(\"mkcl_mt_internal_symbols[%lu]\"),\n", i);
    }
  printf("};\n");
}

void print_mt_external_symbol_C_names(void)
{
  mkcl_index i;

  printf("\nstatic const struct mkcl_base_string mt_external_C_names[] = {\n");
  for (i = 0; i < external_count; i++)
    {
      printf("SYMBOL_NAME(\"mkcl_mt_external_symbols[%lu]\"),\n", i);
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

void print_mt_internal_symbol_initializers(void)
{
  mkcl_index i;

  printf("\nstruct mkcl_symbol mkcl_mt_internal_symbols[] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      struct mkcl_symbol * sym = &mkcl_mt_internal_symbols[i];
      
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
      printf("(mkcl_object) &mkcl_mt_internal_symbol_names[%lu], ", i); /* name */
      printf("(mkcl_object) &mkcl_package_mt, "); /* hpack */
      printf("mk_cl_Cnil, "); /* properly_named_class */
      printf("mk_cl_Cnil, "); /* sys_plist */
      printf("MKCL_NOT_A_SPECIAL_INDEX, "); /* special_index */
      printf(MKCL_HASH_VALUE_FORMAT ", ", sym->hashed_name); /* hashed_name */
      printf("(mkcl_object) &mt_internal_C_names[%lu], ", i); /* C_name */
      printf("NULL, "); /* _C_name */
      printf("NULL, "); /* _name */
      
      printf("}, /* %lu %s */\n", i, sym->name->base_string.self);
    }
  printf("};\n");
}

void print_mt_external_symbol_initializers(void)
{
  mkcl_index i;

  printf("\nstruct mkcl_symbol mkcl_mt_external_symbols[] = {\n");
  for (i = 0; i < external_count; i++)
    {
      struct mkcl_symbol * sym = &mkcl_mt_external_symbols[i];
      
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
      printf("(mkcl_object) &mkcl_mt_external_symbol_names[%lu], ", i); /* name */
      printf("(mkcl_object) &mkcl_package_mt, "); /* hpack */
      printf("mk_cl_Cnil, "); /* properly_named_class */
      printf("mk_cl_Cnil, "); /* sys_plist */
      printf("MKCL_NOT_A_SPECIAL_INDEX, "); /* special_index */
      printf(MKCL_HASH_VALUE_FORMAT ", ", sym->hashed_name); /* hashed_name */
      printf("(mkcl_object) &mt_external_C_names[%lu], ", i); /* C_name */
      printf("NULL, "); /* _C_name */
      printf("NULL, "); /* _name */
      
      printf("}, /* %lu %s */\n", i, sym->name->base_string.self);
    }
  printf("};\n");
}


void print_mt_internal_hashtable_entry_initializers(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry internal_entries[internal_count] = {\n");
  for (i = 0; i < internal_count; i++)
    {
      struct mkcl_hashtable_entry * entry = &internal_entries[i];
      

      printf("{ ");
      if (entry->next)
	printf("&internal_entries[%lu], ", mt_internal_entry_index(entry->next));
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
	  mkcl_index sym_index = mt_internal_symbol_index((struct mkcl_symbol *) entry->value);

	  printf("(mkcl_object) &mkcl_mt_internal_symbol_names[%lu], ", sym_index);
	  printf(MKCL_HASH_VALUE_FORMAT ", ", entry->hashed_key);
	  printf("(mkcl_object) &mkcl_mt_internal_symbols[%lu], ", sym_index);
	}
      printf("}, \n");
    }
  printf("};\n");
}

void print_mt_internal_hashtable_vector_initializer(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry * internal_vector[internal_size] = {\n");
  for (i = 0; i < internal_size; i++)
    {
      struct mkcl_hashtable_entry * entry = internal_vector[i];

      if (entry)
	printf("&internal_entries[%lu], \n", mt_internal_entry_index(entry));
      else
	printf("NULL, \n");

    }
  printf("};\n");
}

void print_mt_external_hashtable_entry_initializers(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry external_entries[external_count] = {\n");
  for (i = 0; i < external_count; i++)
    {
      struct mkcl_hashtable_entry * entry = &external_entries[i];
      

      printf("{ ");
      if (entry->next)
	printf("&external_entries[%lu], ", mt_external_entry_index(entry->next));
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
	  mkcl_index sym_index = mt_external_symbol_index((struct mkcl_symbol *) entry->value);

	  printf("(mkcl_object) &mkcl_mt_external_symbol_names[%lu], ", sym_index);
	  printf(MKCL_HASH_VALUE_FORMAT ", ", entry->hashed_key);
	  printf("(mkcl_object) &mkcl_mt_external_symbols[%lu], ", sym_index);
	}
      printf("}, \n");
    }
  printf("};\n");
}

void print_mt_external_hashtable_vector_initializer(void)
{
  mkcl_index i;

  printf("\nstatic struct mkcl_hashtable_entry * external_vector[external_size] = {\n");
  for (i = 0; i < external_size; i++)
    {
      struct mkcl_hashtable_entry * entry = external_vector[i];

      if (entry)
	printf("&external_entries[%lu], \n", mt_external_entry_index(entry));
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


void print_mt_package(void)
{
  print_copyright();
  print_mt_internal_symbol_C_names();
  print_mt_internal_symbol_initializers();
  print_mt_external_symbol_C_names();
  print_mt_external_symbol_initializers();
  print_mt_internal_hashtable_entry_initializers();
  print_mt_internal_hashtable_vector_initializer();
  print_mt_external_hashtable_entry_initializers();
  print_mt_external_hashtable_vector_initializer();
}

void expose_mt_symbol(const struct exposed_symbol * exposed_symbol)
{
  bool internalp;
  struct mkcl_symbol * sym = find_symbol(&exposed_symbol->symbol_name, &mkcl_package_mt, &internalp);

  printf("#define %s ", exposed_symbol->exposition);
  if (internalp)
    printf("mkcl_mt_internal_symbols[%lu]\n", mt_internal_symbol_index(sym));
  else
    printf("mkcl_mt_external_symbols[%lu]\n", mt_external_symbol_index(sym)); 
}

void expose_mt_package(void)
{
  mkcl_index i;

  print_copyright();
  for (i = 0; i < MKCL_NB_ELEMS(exposed_symbols); i++)
    {
      expose_mt_symbol(&exposed_symbols[i]);
    }
}

int main(int argc, char * argv[])
{
  char * program_name = basename(strdup(argv[0]));
  init_mt_package();
  

  if ( strcmp(program_name, "build_package_MT" PROGRAM_SUFFIX) == 0 )
    print_mt_package();
  else if ( strcmp(program_name, "expose_package_MT" PROGRAM_SUFFIX) == 0 )
    expose_mt_package();
  else
    { printf("\nDon't know this program name: %s\n", program_name); exit(2); }


  return 0;
}

