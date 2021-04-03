/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2017, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file './Copyright' for full details.
*/

#ifndef MKCL_EXTERNAL_H
#define MKCL_EXTERNAL_H

#define MKCL_STARTUP_SPECIALS_SIZE 256

#ifdef __cplusplus
extern "C" {
#endif

  /*
   * Per-thread data.
   */

  struct mkcl_env_struct {
    /* Flag for disabling interrupts while we call C library functions. */
    volatile int disable_interrupts;
    volatile mkcl_object sleeping_on;

    mkcl_index nvalues;

    /* Call stack management. */
    char * cs_limit; /* The line where an overflow is declared. */
    char * cs_org; /* The base of the memory region requested to be used as call stack for this thread. */
    mkcl_index cs_size; /* The allocated size of the call stack of this thread. */
    mkcl_index cs_overflow_size; /* The size of the call stack overflow area. */
    bool cs_overflowing;

    /* Array where values are returned by functions. */
    mkcl_object values[MKCL_MULTIPLE_VALUES_LIMIT];

    /* Environment for calling closures, CLOS generic functions, etc */
    mkcl_object function; /* Can be trusted only in the context of a call to a closure (or CLOS ?).
			     Otherwise, can be filled with leftover garbage! JCB */


    /* The three secondary stacks in MKCL. */

    /*
     * The lisp temporaries stack, which is used mainly for passing in the arguments during
     * function invocation (only if arguments count is above MKCL_C_ARGUMENTS_LIMIT),
     * and also by the bytecode compiler and by the
     * reader when they are building some data structure.
     */
    mkcl_index temp_stack_size;
    mkcl_index temp_stack_size_limit;
    mkcl_object *temp_stack;
    mkcl_object *temp_stack_top;
    mkcl_object *temp_stack_upper_bound;

    mkcl_index temp_stack_overflow_size;
    bool temp_stack_overflowing;

    /*
     * The BinDing Stack stores the bindings of special variables.
     */
    mkcl_index bds_size;
    mkcl_index bds_size_limit;
    struct mkcl_bds_bd *bds_org;
    struct mkcl_bds_bd *bds_top;
    struct mkcl_bds_bd *bds_upper_bound;

    mkcl_index bds_overflow_size;
    bool bds_overflowing;

    mkcl_object * specials;
    mkcl_index specials_size;

    /*
     * The FRames Stack (FRS) is a list of frames or jump points, and it
     * is used by different high-level constructs (BLOCK, TAGBODY, CATCH...)
     * to set return points.
     */
    mkcl_index frs_size;
    mkcl_index frs_size_limit;
    struct mkcl_frame *frs_org;
    struct mkcl_frame *frs_top;
    struct mkcl_frame *frs_upper_bound;

    mkcl_index frs_overflow_size;
    bool frs_overflowing;

    struct mkcl_frame *nlj_fr; /* The Non-Local Jump-to frame. */
    mkcl_index go_label_index;

    /*
     * The Invocation History Stack (IHS) keeps a list of the names of the
     * functions that are invoked, together with their lexical
     * environments.
     */
    struct mkcl_ihs_frame *ihs_top;

    /* Private variables used by different parts of MKCL: */
    /* ... the reader ... */
    mkcl_object string_pool;

    /* ... the compiler ... */
    struct mkcl_compiler_env *c_env;

    /* ... the formatter ... */
    mkcl_object fmt_aux_stream;

    /* ... the pretty printer ... */
    bool print_pretty;
    short *queue;
    short *indent_stack;
    int qh, qt, qc, isp, iisp;

    /* ... arithmetics ... */
    /* Note: if you change the size of these registers, change also
       MKCL_BIGNUM_REGISTER_SIZE in config.h */
    mkcl_object big_register[3];

    mkcl_object own_thread;

    /* The following is a hash table for caching invocations of
       generic functions. In a multithreaded environment we must
       queue operations in which the hash is cleared from updated
       generic functions. */
    volatile mkcl_object method_hash_clear_list; /* across thread communication! JCB */
    mkcl_object method_hash;
    mkcl_object method_spec_vector;
    mkcl_word method_generation;

    /* foreign function interface */
    struct mkcl_fficall * fficall;

    /* Alternative stack for processing signals */ /* Not used! Incompatible with Boehm's GC. JCB */
    void *altstack;
    mkcl_index altstack_size;

    /* Floating point interrupts which are trapped */
    int fpe_control_bits;

    /* to support MKCL_DEBUG_INTERRUPT_MASK */
    char * volatile interrupt_disabler_file;
    volatile size_t interrupt_disabler_lineno;

    volatile double fp_drone;
    struct mkcl_alloc_stats * alloc;

    /* Re-initialization parameters */
    char * cs_org_request;
    mkcl_index cs_size_request;
  };


  typedef struct mkcl_env_struct * mkcl_env;

#define MKCL register const mkcl_env env
#define MKCL_ENV() mkcl_thread_env()

  extern MKCL_API const mkcl_env mkcl_thread_env(void);


  /*
   * process global data.
   */

  struct mkcl_core_struct {
    mkcl_object packages;
    mkcl_object lisp_package;
    mkcl_object user_package;
    mkcl_object keyword_package;
    mkcl_object system_package;
    mkcl_object mkcl_ext_package;
    mkcl_object clos_package;
    mkcl_object gray_package;
    mkcl_object mt_package;
    mkcl_object packages_to_be_created;

    mkcl_object pathname_translations;
    mkcl_object SYS_library_pathname;

    mkcl_object terminal_io;
    mkcl_object null_stream;
    mkcl_object standard_input;
    mkcl_object standard_output;
    mkcl_object error_output;
    mkcl_object standard_readtable;
    mkcl_object dispatch_reader;        /* a constant included here for GC purposes? JCB */
    mkcl_object default_dispatch_macro; /* unused? JCB */

    mkcl_object base_char_names;
    mkcl_object empty_base_string;
    mkcl_object empty_string;
    mkcl_object dot_string;
    mkcl_object dot_dot_string;
    mkcl_object localhost_string;

    mkcl_object plus_half;
    mkcl_object minus_half;
    mkcl_object imag_unit;
    mkcl_object minus_imag_unit;
    mkcl_object imag_two;
    mkcl_object singlefloat_zero;
    mkcl_object doublefloat_zero;
    mkcl_object singlefloat_minus_zero;
    mkcl_object doublefloat_minus_zero;
    mkcl_object longfloat_zero;
    mkcl_object longfloat_minus_zero;

    mkcl_object gensym_prefix;
    mkcl_object gentemp_prefix;
    mkcl_object gentemp_counter;

    mkcl_object Jan1st1970UT;

    mkcl_object system_properties;

    mkcl_index top_special_index;  /* should this really be public? JCB */
#if MKCL_WINDOWS
    CRITICAL_SECTION special_index_lock;  /* should this really be public? JCB */
#else
    pthread_mutex_t special_index_lock;  /* should this really be public? JCB */
#endif

    mkcl_object threads;
    mkcl_object initial_thread;
    mkcl_object shutdown_watchdog_thread;
    mkcl_object shutdown_watchdog_will_clean_up;
    mkcl_object shutdown_thread;
    mkcl_object shutdown_gate;
    mkcl_object imported_thread_pool;

#if MKCL_WINDOWS
    CRITICAL_SECTION thread_list_lock;  /* should this really be public? JCB */
    CRITICAL_SECTION package_list_lock;  /* should this really be public? JCB */
#else
    pthread_mutex_t thread_list_lock;  /* should this really be public? JCB */
    pthread_mutex_t package_list_lock;  /* should this really be public? JCB */
#endif

    mkcl_object libraries; /* protected by the Load-Compile lock. */
    mkcl_object to_be_finalized;

    char *safety_region; /* protected by the Out-Of-Memory lock. */
    mkcl_index max_heap_size; /* protected by the Out-Of-Memory lock. */

    mkcl_object bytes_consed;
    mkcl_object gc_pinned;
    mkcl_object gc_counter;
    long gc_fast_counter;
    bool gc_stats;

    long path_max;
    long name_max;
    long arg_max;
    long pagesize;

    mkcl_object self;
    mkcl_object self_truename;

    mkcl_object empty_default_pathname_defaults;
    mkcl_object default_default_external_format;
    volatile mkcl_object children;
    volatile mkcl_object detached_children;

#ifdef HASHTABLE_STATS /* JCB */
    mkcl_object hashtables[mkcl_htt_package + 1];
#endif
  };

  extern MKCL_API struct mkcl_core_struct mkcl_core;

  /* alloc.c / alloc_2.c */

  extern MKCL_API void * mkcl_alloc_pages(MKCL, mkcl_index nb_pages);
  extern MKCL_API void * mkcl_alloc_callback_block(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_cdisplay(MKCL, mkcl_index nb_levels);
  extern MKCL_API mkcl_object mkcl_alloc_clevel_block(MKCL, mkcl_object producer, const union mkcl_lispunion * const outer, const mkcl_index nb_vars);
  extern MKCL_API mkcl_object mkcl_alloc_raw_instance(MKCL, mkcl_index nb_slots);
  extern MKCL_API mkcl_object mkcl_alloc_raw_structure(MKCL, mkcl_object type,  mkcl_index nb_slots);
  extern MKCL_API mkcl_object mkcl_alloc_raw_base_string(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_string(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_symbol(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_bytecode(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_bclosure(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_cfun(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_cclosure(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_vector(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_bitvector(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_array(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_bignum(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_bignum_with_limbs(MKCL, int nb_limbs);
  extern MKCL_API mkcl_object mkcl_alloc_raw_ratio(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_singlefloat(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_doublefloat(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_longfloat(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_complex(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_hashtable(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_codeblock(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_random(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_package(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_pathname(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_readtable(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_thread(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_lock(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_rwlock(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_semaphore(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_condition_variable(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_foreign(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_stream(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_raw_process(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_utf_8(MKCL, mkcl_index length);
  extern MKCL_API mkcl_object mkcl_alloc_raw_utf_8(MKCL);
  extern MKCL_API mkcl_object mkcl_alloc_utf_16(MKCL, mkcl_index length);
  extern MKCL_API mkcl_object mkcl_alloc_raw_utf_16(MKCL);

  extern MKCL_API mkcl_object mkcl_cons(MKCL, mkcl_object a, mkcl_object d);
  extern MKCL_API mkcl_object mkcl_list1(MKCL, mkcl_object a);

  extern MKCL_API mkcl_object mk_si_scrub_values(MKCL);
  extern MKCL_API mkcl_object mk_si_gc(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_si_gc_dump(MKCL);
  extern MKCL_API mkcl_object mk_si_gc_off(MKCL);
  extern MKCL_API mkcl_object mk_si_gc_on(MKCL);
  extern MKCL_API mkcl_object mk_si_gc_stats(MKCL, mkcl_object enable);
  extern MKCL_API mkcl_object mk_si_mem_stats(MKCL);
  extern MKCL_API void *_mkcl_boot_alloc_unprotected(mkcl_index n);
  extern MKCL_API void *mkcl_alloc(MKCL, mkcl_index n);
  extern MKCL_API void *mkcl_alloc_atomic(MKCL, mkcl_index n);
  extern MKCL_API void *mkcl_alloc_uncollectable(MKCL, size_t size);
  extern MKCL_API void mkcl_free_uncollectable(MKCL, void *);
  extern MKCL_API void mkcl_dealloc(MKCL, void *);
#define mkcl_alloc_align(e,s,d) mkcl_alloc(e,s)
#define mkcl_alloc_atomic_align(e,s,d) mkcl_alloc_atomic(e,s)

  extern MKCL_API void * mkcl_foreign_malloc(MKCL, size_t size);
  extern MKCL_API void mkcl_foreign_free(MKCL, void *);

  extern MKCL_API size_t mkcl_GC_get_total_bytes(void);

  extern MKCL_API mkcl_object mkcl_alloc_pin_bag(MKCL);
  extern MKCL_API mkcl_object mkcl_pin(MKCL, mkcl_object bag, mkcl_object obj);
  extern MKCL_API mkcl_object mkcl_unpin(MKCL, mkcl_object pin);

  extern MKCL_API mkcl_object mk_si_sample_allocation_statistics(MKCL);
  extern MKCL_API mkcl_object mk_si_reset_allocation_statistics(MKCL);
  extern MKCL_API mkcl_object mk_si_room_report(MKCL, mkcl_object label);


  /* all_symbols */

  extern MKCL_API mkcl_object mk_si_mangle_string(MKCL, mkcl_object string);
  extern MKCL_API mkcl_object mk_si_mangle_symbol(MKCL, mkcl_object symbol);
  extern MKCL_API mkcl_object mk_si_mangle_name(MKCL, mkcl_object symbol);
  extern MKCL_API mkcl_object mk_si_mangle_function_name(MKCL, mkcl_object symbol);
  typedef union {
    struct {
      const char *name;
      int type;
      void *fun;
      short narg;
      mkcl_object value;
    } init;
    struct mkcl_symbol data;
  } mkcl_symbol_initializer;
  extern MKCL_API mkcl_symbol_initializer mkcl_root_symbols[];
  extern MKCL_API const mkcl_index mkcl_root_symbols_count;

#define MKCL_SYM(name,code) ((mkcl_object) (mkcl_root_symbols+(code)))

  /* apply.c */

  extern MKCL_API mkcl_object mkcl_APPLY_fixed(MKCL, mkcl_narg n, mkcl_object (*f)(), mkcl_object *x);
  extern MKCL_API mkcl_object mkcl_APPLY(MKCL, mkcl_narg n, mkcl_object fun, mkcl_object *x);

  /* array.c */

  extern MKCL_API mkcl_object mkcl_out_of_bounds_error(MKCL, mkcl_object fun, const char *place, mkcl_object value, mkcl_index min, mkcl_index max);
  extern MKCL_API mkcl_index mkcl_ensure_valid_array_index(MKCL, mkcl_object x, mkcl_index index);
  extern MKCL_API mkcl_object mkcl_ensure_valid_array_index_type(MKCL, mkcl_object x, mkcl_object index);
  extern MKCL_API mkcl_index mkcl_array_row_major_index_2_t(MKCL, mkcl_object a, mkcl_object i, mkcl_object j);
  extern MKCL_API mkcl_index mkcl_array_row_major_index_3_t(MKCL, mkcl_object a, mkcl_object i, mkcl_object j, mkcl_object k);
  extern MKCL_API mkcl_object mk_cl_array_row_major_index(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_row_major_aref(MKCL, mkcl_object x, mkcl_object i);
  extern MKCL_API mkcl_object mk_si_row_major_aset(MKCL, mkcl_object x, mkcl_object i, mkcl_object v);
  extern MKCL_API mkcl_object mk_si_make_vector(MKCL, mkcl_object etype, mkcl_object dim, mkcl_object adj, mkcl_object fillp, mkcl_object displ, mkcl_object disploff);
  extern MKCL_API mkcl_object mkcl_alloc_simple_vector(MKCL, mkcl_index l, mkcl_elttype aet);
  extern MKCL_API mkcl_object mk_cl_array_element_type(MKCL, mkcl_object a);
  extern MKCL_API mkcl_object mk_cl_array_rank(MKCL, mkcl_object a);
  extern MKCL_API mkcl_object mk_cl_array_dimension(MKCL, mkcl_object a, mkcl_object index);
  extern MKCL_API mkcl_object mk_cl_array_total_size(MKCL, mkcl_object a);
  extern MKCL_API mkcl_object mk_cl_adjustable_array_p(MKCL, mkcl_object a);
  extern MKCL_API mkcl_object mk_cl_array_displacement(MKCL, mkcl_object a);
  extern MKCL_API mkcl_object mk_cl_svref(MKCL, mkcl_object x, mkcl_object index);
  extern MKCL_API mkcl_object mk_si_svset(MKCL, mkcl_object x, mkcl_object index, mkcl_object v);
  extern MKCL_API mkcl_object mk_cl_array_has_fill_pointer_p(MKCL, mkcl_object a);
  extern MKCL_API mkcl_object mk_cl_fill_pointer(MKCL, mkcl_object a);
  extern MKCL_API mkcl_object mk_si_fill_pointer_set(MKCL, mkcl_object a, mkcl_object fp);
  extern MKCL_API mkcl_object mk_si_replace_array(MKCL, mkcl_object old_obj, mkcl_object new_obj);
  extern MKCL_API mkcl_object mk_cl_aref(MKCL, mkcl_narg narg, mkcl_object x, ...);
  extern MKCL_API mkcl_object mk_si_aset(MKCL, mkcl_narg narg, mkcl_object v, mkcl_object x, ...);
  extern MKCL_API mkcl_object mk_si_make_pure_array(MKCL, mkcl_object etype, mkcl_object dims, mkcl_object adj, mkcl_object fillp, mkcl_object displ, mkcl_object disploff);
  extern MKCL_API mkcl_object mk_si_fill_array_with_elt(MKCL, mkcl_object array, mkcl_object elt, mkcl_object start, mkcl_object end);

  extern MKCL_API mkcl_index mkcl_to_array_index(MKCL, mkcl_object n);
  extern MKCL_API mkcl_object mkcl_aref(MKCL, mkcl_object x, mkcl_object index);
  extern MKCL_API mkcl_object mkcl_vref(MKCL, mkcl_object v, mkcl_object index);
  extern MKCL_API mkcl_object mkcl_aset(MKCL, mkcl_object x, mkcl_object index, mkcl_object value);
  extern MKCL_API mkcl_object mkcl_vset(MKCL, mkcl_object v, mkcl_object index, mkcl_object val);
  extern MKCL_API mkcl_object mkcl_bvref_index(MKCL, mkcl_object x, mkcl_index index);
  extern MKCL_API mkcl_object mkcl_bvref(MKCL, mkcl_object x, mkcl_object index);
  extern MKCL_API mkcl_object mkcl_bvset_index(MKCL, mkcl_object x, mkcl_index index, mkcl_word value);
  extern MKCL_API mkcl_object mkcl_bvset(MKCL, mkcl_object x, mkcl_object index, mkcl_object value);
  extern MKCL_API void mkcl_array_allocself(MKCL, mkcl_object x);
  extern MKCL_API mkcl_elttype mkcl_array_elttype(MKCL, mkcl_object x);
  extern MKCL_API mkcl_elttype mkcl_symbol_to_elttype(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_elttype_to_symbol(MKCL, mkcl_elttype aet);
  extern MKCL_API void mkcl_copy_subarray(MKCL, mkcl_object dest, mkcl_index i0, mkcl_object orig, mkcl_index i1, mkcl_index l);
  extern MKCL_API void mkcl_reverse_subarray(MKCL, mkcl_object dest, mkcl_index i0, mkcl_index i1);


  /* assignment.c */

  extern MKCL_API mkcl_object mk_cl_set(MKCL, mkcl_object var, mkcl_object val);
  extern MKCL_API mkcl_object mk_cl_makunbound(MKCL, mkcl_object sym);
  extern MKCL_API mkcl_object mk_cl_fmakunbound(MKCL, mkcl_object sym);
  extern MKCL_API mkcl_object mk_si_fset(MKCL, mkcl_narg narg, mkcl_object fun, mkcl_object def, ...);
  extern MKCL_API mkcl_object mk_si_get_sysprop(MKCL, mkcl_object sym, mkcl_object prop);
  extern MKCL_API mkcl_object mk_si_put_sysprop(MKCL, mkcl_object sym, mkcl_object prop, mkcl_object value);
  extern MKCL_API mkcl_object mk_si_rem_sysprop(MKCL, mkcl_object sym, mkcl_object prop);
  extern MKCL_API mkcl_object mk_si_system_properties(MKCL);

  extern MKCL_API void mkcl_clear_compiler_properties(MKCL, mkcl_object sym);

  /* big.c */

#define _mkcl_big_register0()	env->big_register[0]
#define _mkcl_big_register1()	env->big_register[1]
#define _mkcl_big_register2()	env->big_register[2]
  extern MKCL_API mkcl_object _mkcl_big_register_copy(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object _mkcl_big_register_normalize(MKCL, mkcl_object x);
  extern MKCL_API void _mkcl_big_register_free(MKCL, mkcl_object x);


  /* cfun.c */

  extern MKCL_API mkcl_object mk_si_compiled_function_name(MKCL, mkcl_object fun);
  extern MKCL_API mkcl_object mk_si_set_compiled_function_name(MKCL, mkcl_object fun, mkcl_object name);
  extern MKCL_API mkcl_object mk_si_compiled_function_block(MKCL, mkcl_object fun);
  extern MKCL_API mkcl_object mk_cl_function_lambda_expression(MKCL, mkcl_object fun);
  extern MKCL_API mkcl_object mk_si_compiled_function_file(MKCL, mkcl_object fun);

  extern MKCL_API mkcl_object mkcl_make_cfun(MKCL, mkcl_objectfn_fixed c_function, mkcl_object name, mkcl_object block, int narg, mkcl_object * anchor);
  extern MKCL_API mkcl_object mkcl_make_cfun_va(MKCL, mkcl_objectfn c_function, mkcl_object name, mkcl_object block, mkcl_object * anchor);
  extern MKCL_API void mkcl_build_named_cfun_fun_ref_syms(MKCL, mkcl_object fun, mkcl_object * VV, mkcl_object * fun_ref_sym_locs, mkcl_index nb_fun_refs);
  extern MKCL_API mkcl_object mkcl_fix_lambda_fun_refs(MKCL, mkcl_object * VV, mkcl_object * fun_ref_syms_locs, mkcl_index nb_fun_refs, mkcl_object fun);
  extern MKCL_API mkcl_object mkcl_fix_lex_local_fun_refs(MKCL, mkcl_object producer, mkcl_object fun);

  extern MKCL_API mkcl_object mkcl_debug_make_cfun(MKCL, mkcl_objectfn_fixed c_function, mkcl_object name, mkcl_object cblock, int narg, mkcl_object * anchor, char * source, int position);
  extern MKCL_API mkcl_object mkcl_debug_make_cfun_va(MKCL, mkcl_objectfn c_function, mkcl_object name, mkcl_object cblock, mkcl_object * anchor, char * source, int position);
  extern MKCL_API mkcl_object mkcl_build_cdisplay(MKCL, mkcl_object producer, mkcl_object cenv, mkcl_index depth);

  extern MKCL_API mkcl_object mkcl_make_cclosure(MKCL, mkcl_object producer, mkcl_objectfn_fixed c_function, int narg, mkcl_index depth, mkcl_object syms_cenv, mkcl_object cenv, mkcl_object block, int position);
  extern MKCL_API mkcl_object mkcl_make_cclosure_va(MKCL, mkcl_object producer, mkcl_objectfn c_function, mkcl_index depth, mkcl_object syms_cenv, mkcl_object cenv, mkcl_object block, int position);

  extern MKCL_API mkcl_object mk_si_clone_closure(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_si_closure_depth(MKCL, mkcl_object clo);
  extern MKCL_API mkcl_object mk_si_closure_level(MKCL, mkcl_object clo, mkcl_object i);
  extern MKCL_API mkcl_object mk_si_closure_level_size(MKCL, mkcl_object level);
  extern MKCL_API mkcl_object mk_si_closure_level_var(MKCL, mkcl_object level, mkcl_object i);
  extern MKCL_API mkcl_object mk_si_closure_level_set_var(MKCL, mkcl_object level, mkcl_object i, mkcl_object val);
  extern MKCL_API mkcl_object mk_si_closure_level_outer_level(MKCL, mkcl_object level);

  extern MKCL_API void mkcl_def_c_function(MKCL, mkcl_object sym, mkcl_objectfn_fixed c_function, int narg);
  extern MKCL_API void mkcl_def_c_macro(MKCL, mkcl_object sym, mkcl_objectfn_fixed c_function, int narg);
  extern MKCL_API void mkcl_def_c_macro_va(MKCL, mkcl_object sym, mkcl_objectfn c_function);
  extern MKCL_API void mkcl_def_c_function_va(MKCL, mkcl_object sym, mkcl_objectfn c_function);
  extern MKCL_API void mkcl_set_function_source_file_info(MKCL, mkcl_object fun, mkcl_object source, mkcl_object position);
  extern MKCL_API void mkcl_cmp_defmacro(MKCL, mkcl_object data);
  extern MKCL_API void mkcl_cmp_defun(MKCL, mkcl_object data);

  extern MKCL_API mkcl_object mk_si_closurep(MKCL, mkcl_object fun);
  extern MKCL_API mkcl_object mk_si_closure_env(MKCL, mkcl_object fun);
  extern MKCL_API mkcl_object mk_si_closure_producer(MKCL, mkcl_object fun);
  extern MKCL_API mkcl_object mk_si_compiled_function_owner(MKCL, mkcl_object fun);
  extern MKCL_API mkcl_object mk_si_set_compiled_function_owner(MKCL, mkcl_object fun, mkcl_object owner);

  extern MKCL_API mkcl_object * mkcl_build_fun_ref_syms_from_locs(MKCL, mkcl_object * VV, mkcl_object * locs, mkcl_index size);
  extern MKCL_API mkcl_object * mkcl_build_fun_refs_from_syms(MKCL, mkcl_object fun_or_cblock, mkcl_object * syms, mkcl_index size);

  extern MKCL_API mkcl_object mk_si_patch_fun_ref(MKCL, mkcl_object fun, mkcl_object index, mkcl_object fun_ref);
  extern MKCL_API mkcl_object mk_si_get_fun_ref_sym(MKCL, mkcl_object fun, mkcl_object index);
  extern MKCL_API mkcl_object mkcl_fun_ref_fdefinition(MKCL, const mkcl_object * const fun_refs, mkcl_index i);
  extern MKCL_API int mkcl_fun_refs_trap(MKCL, mkcl_object fun, const mkcl_object * const fun_refs, mkcl_index i); /* debug JCB */
  extern MKCL_API mkcl_object mk_si_update_function_references(MKCL, mkcl_object fun);

  /* character.c */

  extern MKCL_API mkcl_object mk_cl_digit_char_p(MKCL, mkcl_narg narg, mkcl_object c, ...);
  extern MKCL_API mkcl_object mk_cl_charE(MKCL, mkcl_narg narg, mkcl_object c, ...);
  extern MKCL_API mkcl_object mk_cl_charNE(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_charL(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_charG(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_charLE(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_charGE(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_char_equal(MKCL, mkcl_narg narg, mkcl_object c, ...);
  extern MKCL_API mkcl_object mk_cl_char_not_equal(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_char_lessp(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_char_greaterp(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_char_not_greaterp(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_char_not_lessp(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_digit_char(MKCL, mkcl_narg narg, mkcl_object w, ...);

  extern MKCL_API mkcl_object mk_cl_alpha_char_p(MKCL, mkcl_object c);
  extern MKCL_API mkcl_object mk_cl_alphanumericp(MKCL, mkcl_object c);
  extern MKCL_API mkcl_object mk_cl_both_case_p(MKCL, mkcl_object c);
  extern MKCL_API mkcl_object mk_cl_char_code(MKCL, mkcl_object c);
  extern MKCL_API mkcl_object mk_cl_char_downcase(MKCL, mkcl_object c);
  extern MKCL_API mkcl_object mk_cl_char_int(MKCL, mkcl_object c);
  extern MKCL_API mkcl_object mk_cl_char_name(MKCL, mkcl_object c);
  extern MKCL_API mkcl_object mk_cl_char_upcase(MKCL, mkcl_object c);
  extern MKCL_API mkcl_object mk_cl_character(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_code_char(MKCL, mkcl_object c);
  extern MKCL_API mkcl_object mk_cl_graphic_char_p(MKCL, mkcl_object c);
  extern MKCL_API mkcl_object mk_cl_lower_case_p(MKCL, mkcl_object c);
  extern MKCL_API mkcl_object mk_cl_name_char(MKCL, mkcl_object s);
  extern MKCL_API mkcl_object mk_cl_standard_char_p(MKCL, mkcl_object c);
  extern MKCL_API mkcl_object mk_cl_upper_case_p(MKCL, mkcl_object c);

  enum mkcl_string_case { mkcl_lowercase_string = -1, mkcl_mixedcase_string = 0, mkcl_uppercase_string = 1 };
  extern MKCL_API enum mkcl_string_case mkcl_string_case(const mkcl_object s);

  static inline bool mkcl_graphic_char_p(mkcl_character code) { return code > 159 || ((31 < code) && (code < 127)); } /* SBCL compatible */


#include <mkcl/mkcl-unicode.h>

  static inline const struct mkcl_unichar_info * mkcl_unicode_character_information(mkcl_character code)
  {
    if (code >= MKCL_CHAR_CODE_LIMIT)
      return NULL;
    else
      {
	const mkcl_uint8_t page_index = _mkcl_unichar_info_pages[code >> 8];
	return &(_mkcl_unichar_info[page_index][code & 0xFF]);
      }
  }

  static inline enum mkcl_ucd_general_category  mkcl_unicode_character_general_category(mkcl_character code)
  {
    const struct mkcl_unichar_info * char_info = mkcl_unicode_character_information(code);
    if (char_info)
      return _mkcl_unichar_properties_signatures[char_info->properties_signature_index].general_category;
    else
      return (enum mkcl_ucd_general_category) -1;
  }

  static inline int mkcl_ucd_decimal_digit(mkcl_character code)
  {
    const struct mkcl_unichar_info * char_info = mkcl_unicode_character_information(code);
    if (char_info)
      return _mkcl_unichar_properties_signatures[char_info->properties_signature_index].decimal_digit;
    else
      return -1;
  }

  static inline bool mkcl_alpha_char_p(mkcl_character code)
  {
    const enum mkcl_ucd_general_category gc = mkcl_unicode_character_general_category(code);
    return mkcl_ucd_Uppercase_Letter <= gc && gc <= mkcl_ucd_Other_Letter;
  }

  static inline bool mkcl_upper_case_p(mkcl_character code)
  {
    const struct mkcl_unichar_info * char_info = mkcl_unicode_character_information(code);
    return (char_info && char_info->properties_signature_index == 0);
  }
  static inline bool mkcl_lower_case_p(mkcl_character code)
  {
    const struct mkcl_unichar_info * char_info = mkcl_unicode_character_information(code);
    return (char_info && char_info->properties_signature_index == 1);
  }
  static inline bool mkcl_both_case_p(mkcl_character code)
  {
    const struct mkcl_unichar_info * char_info = mkcl_unicode_character_information(code);
    return (char_info && char_info->properties_signature_index < 2);
  }
  
  static inline bool mkcl_alphanumericp(mkcl_character code)
  {
    const enum mkcl_ucd_general_category gc = mkcl_unicode_character_general_category(code);
    return ((mkcl_ucd_Uppercase_Letter <= gc && gc <= mkcl_ucd_Other_Letter)
	    || (mkcl_ucd_Decimal_Number <= gc && gc <= mkcl_ucd_Other_Number));
  }

  static inline mkcl_character mkcl_char_upcase(mkcl_character code)
  {
    const struct mkcl_unichar_info * char_info = mkcl_unicode_character_information(code);
    if (char_info && char_info->properties_signature_index == 1)
      return char_info->transform;
    else 
      return code;
  }

  static inline mkcl_character mkcl_char_downcase(mkcl_character code)
  {
    const struct mkcl_unichar_info * char_info = mkcl_unicode_character_information(code);
    if (char_info && char_info->properties_signature_index == 0)
      return char_info->transform;
    else 
      return code;
  }


  extern MKCL_API void mkcl_FEtype_error_character(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_base_char(MKCL, mkcl_object x) mkcl_noreturn;

  static inline mkcl_character mkcl_char_code(MKCL, mkcl_object c)
  {
    if (mkcl_likely(MKCL_CHARACTERP(c)))
      return MKCL_CHAR_CODE(c);
    else
      mkcl_FEtype_error_character(env, c);
  }
  
  static inline mkcl_base_char mkcl_base_char_code(MKCL, mkcl_object c)
  {
    mkcl_index code;
    
    if (mkcl_likely(MKCL_CHARACTERP(c) && MKCL_BASE_CHAR_CODE_P(code = MKCL_CHAR_CODE(c))))
      return code;
    else
      mkcl_FEtype_error_base_char(env, c);
  }
  
  static inline bool mkcl_base_char_p(mkcl_character c) { return MKCL_BASE_CHAR_CODE_P(c); }
  static inline bool mkcl_standard_char_p(mkcl_character code) { return ((' ' <= code) && (code < '\177')) || (code == '\n'); }


  extern MKCL_API int mkcl_base_string_case(mkcl_object s);
  extern MKCL_API int mkcl_digitp(mkcl_character i, int r);
  extern MKCL_API bool mkcl_char_eq(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API int mkcl_char_cmp(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API bool mkcl_char_equal(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API int mkcl_char_compare(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API short mkcl_digit_char(mkcl_word w, mkcl_word r);

  /* clos.c */

  extern MKCL_API mkcl_object mk_cl_find_class(MKCL, mkcl_narg narg, mkcl_object name, ...);
  extern MKCL_API mkcl_object mk_cl_class_of(MKCL, mkcl_object x);

  /* cmpaux.c */

  extern MKCL_API mkcl_object mk_si_specialp(MKCL, mkcl_object sym);

  extern MKCL_API mkcl_word mkcl_ifloor(MKCL, mkcl_word x, mkcl_word y);
  extern MKCL_API mkcl_word mkcl_imod(MKCL, mkcl_word x, mkcl_word y);
  extern MKCL_API char mkcl_to_char(MKCL, mkcl_object x);
  extern MKCL_API mkcl_word mkcl_number_to_word(MKCL, mkcl_object x);
  extern MKCL_API mkcl_index mkcl_to_unsigned_integer(MKCL, mkcl_object x);
  extern MKCL_API float mkcl_to_float(MKCL, mkcl_object x);
  extern MKCL_API void mkcl_throw(MKCL, mkcl_object tag) mkcl_noreturn;
  extern MKCL_API void mkcl_return_from(MKCL, mkcl_object block_id, mkcl_object block_name) mkcl_noreturn;
  extern MKCL_API void mkcl_go(MKCL, mkcl_object tag_id, mkcl_index label_index) mkcl_noreturn;
  extern MKCL_API void mkcl_parse_key(MKCL, mkcl_va_list args,
                                      const mkcl_word nkey, const mkcl_object * const keys,
                                      mkcl_object * const vars, mkcl_object * rest,
                                      const bool allow_other_keys, const bool dynamic);
  extern MKCL_API mkcl_object mkcl_grab_rest_args(MKCL, mkcl_va_list args, bool dynamic);
  extern MKCL_API mkcl_object mk_si_convert_cmp_lexical_info(MKCL, mkcl_object cmp_env);

  /* compiler.c */

  extern MKCL_API mkcl_object mk_si_macrolet_function(MKCL, mkcl_object form, mkcl_object cenv);
  extern MKCL_API mkcl_object mk_si_process_lambda_list(MKCL, mkcl_object lambda_list, mkcl_object context);
  extern MKCL_API mkcl_object mk_si_process_lambda(MKCL, mkcl_object lambda);
  extern MKCL_API mkcl_object mk_si_make_lambda(MKCL, mkcl_object name, mkcl_object body);
  extern MKCL_API mkcl_object mk_si_function_block_name(MKCL, mkcl_object name);
  extern MKCL_API mkcl_object mk_si_valid_function_name_p(MKCL, mkcl_object name);
  extern MKCL_API mkcl_object mk_si_process_declarations(MKCL, mkcl_narg narg, mkcl_object body, ...);

  extern MKCL_API mkcl_object mk_si_eval_in_env(MKCL, mkcl_narg narg, mkcl_object form, ...);

  /* interpreter.c */

  extern MKCL_API mkcl_object mk_si_interpreter_stack(MKCL, mkcl_narg narg);
  extern MKCL_API mkcl_object mkcl_temp_stack_frame_open(MKCL, mkcl_object f, mkcl_index size);
  extern MKCL_API void mkcl_temp_stack_frame_push(MKCL, mkcl_object f, mkcl_object o);
  extern MKCL_API void mkcl_temp_stack_frame_push_values(MKCL, mkcl_object f);
  extern MKCL_API mkcl_object mkcl_temp_stack_frame_pop_values(MKCL, mkcl_object f);
  extern MKCL_API void mkcl_temp_stack_frame_close(MKCL, mkcl_object f);
#define mk_si_apply_from_temp_stack_frame mkcl_apply_from_temp_stack_frame

  extern MKCL_API void mkcl_FEtemp_stack_underflow(MKCL);
  extern MKCL_API void mkcl_FEtemp_stack_advance(MKCL);
  extern MKCL_API mkcl_index mkcl_stack_push_values(MKCL);
  extern MKCL_API void mkcl_stack_pop_values(MKCL, mkcl_index n);
  extern MKCL_API mkcl_object mkcl_interpret(MKCL, mkcl_object frame, mkcl_object lenv, mkcl_object bytecode);
  extern MKCL_API mkcl_object _mkcl_bytecode_dispatch(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object _mkcl_bclosure_dispatch(MKCL, mkcl_narg narg, ...);

  /* disassembler.c */

  extern MKCL_API mkcl_object mk_si_bc_disassemble(MKCL, mkcl_object v);
  extern MKCL_API mkcl_object mk_si_bc_split(MKCL, mkcl_object v);

  /* error.c */

  extern MKCL_API mkcl_object mk_cl_error(MKCL, mkcl_narg narg, mkcl_object eformat, ...) mkcl_noreturn;
  extern MKCL_API mkcl_object mk_cl_cerror(MKCL, mkcl_narg narg, mkcl_object cformat, mkcl_object eformat, ...);

  extern MKCL_API void mkcl_internal_error(MKCL, const char * const s, const char * const file, const int lineno) mkcl_noreturn;
  extern MKCL_API void mkcl_internal_C_error(MKCL, const char * const s, const char * const file, const int lineno) mkcl_noreturn;
#define mkcl_C_lose(e, m) mkcl_internal_C_error(e, m, __FILE__, __LINE__)
#define mkcl_lose(e, m) mkcl_internal_error(e, m, __FILE__, __LINE__)

  extern MKCL_API mkcl_object mkcl_libc_error_string(MKCL, mkcl_word errno_value);
  extern MKCL_API mkcl_object mk_si_libc_error_string(MKCL, mkcl_object errno_val);
  extern MKCL_API mkcl_object mk_si_errno_string(MKCL);

  extern MKCL_API void mkcl_FEprogram_error(MKCL, const char *s, int narg, ...) mkcl_noreturn;
  extern MKCL_API void mkcl_FEcontrol_error(MKCL, const char *s, int narg, ...) mkcl_noreturn;
  extern MKCL_API void mkcl_FEreader_error(MKCL, const char *s, mkcl_object stream, int narg, ...) mkcl_noreturn;
#define mkcl_FEparse_error mkcl_FEreader_error
  extern MKCL_API void mkcl_FEerror(MKCL, const char *s, int narg, ...) mkcl_noreturn;
  extern MKCL_API void mkcl_FEcannot_open(MKCL, mkcl_object fn) mkcl_noreturn;
  extern MKCL_API void mkcl_FEend_of_file(MKCL, mkcl_object strm) mkcl_noreturn;
  extern MKCL_API void mkcl_FEclosed_stream(MKCL, mkcl_object strm) mkcl_noreturn;
  extern MKCL_API void mkcl_FEwrong_type_argument(MKCL, mkcl_object type, mkcl_object value) mkcl_noreturn;
  extern MKCL_API void mkcl_FEnot_fixnum_type(MKCL, mkcl_object value) mkcl_noreturn;
  extern MKCL_API void mkcl_FEnot_codeblock_type(MKCL, mkcl_object value) mkcl_noreturn;
  extern MKCL_API void mkcl_FEwrong_num_arguments(MKCL, mkcl_object fun, mkcl_narg min, mkcl_narg max, mkcl_narg narg) mkcl_noreturn;
  extern MKCL_API void mkcl_FEwrong_num_arguments_anonym(MKCL, mkcl_narg min, mkcl_narg max, mkcl_narg narg) mkcl_noreturn;
  extern MKCL_API void mkcl_FEunbound_variable(MKCL, mkcl_object sym) mkcl_noreturn;
  extern MKCL_API void mkcl_FEinvalid_macro_call(MKCL, mkcl_object obj) mkcl_noreturn;
  extern MKCL_API void mkcl_FEinvalid_variable(MKCL, const char *s, mkcl_object obj) mkcl_noreturn;
  extern MKCL_API void mkcl_FEassignment_to_constant(MKCL, mkcl_object v) mkcl_noreturn;
  extern MKCL_API void mkcl_FEillegal_variable_name(MKCL, mkcl_object) mkcl_noreturn;
  extern MKCL_API void mkcl_FEundefined_function(MKCL, mkcl_object fname) mkcl_noreturn;
  extern MKCL_API void mkcl_FEinvalid_function(MKCL, mkcl_object obj) mkcl_noreturn;
  extern MKCL_API void mkcl_FEinvalid_function_name(MKCL, mkcl_object obj) mkcl_noreturn;
  extern MKCL_API mkcl_object mkcl_CEerror(MKCL, mkcl_object c, const char *err_str, mkcl_narg narg, ...);
  extern MKCL_API void mkcl_FEillegal_index(MKCL, mkcl_object x, mkcl_object i) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_symbol(MKCL, mkcl_object obj) mkcl_noreturn;
  extern MKCL_API void mkcl_FElibc_error(MKCL, const char *msg, int narg, ...) mkcl_noreturn;
  extern MKCL_API void mkcl_FElibc_file_error(MKCL, mkcl_object pathname, const char *msg, int narg, ...) mkcl_noreturn;
  extern MKCL_API void mkcl_FElibc_stream_error(MKCL, mkcl_object stream, const char *msg, int narg, ...) mkcl_noreturn;
#if MKCL_WINDOWS
  extern MKCL_API void mkcl_FEwin32_error(MKCL, const char *msg, int narg, ...) mkcl_noreturn;
  extern MKCL_API void mkcl_FEwin32_file_error(MKCL, mkcl_object pathname, const char *msg, int narg, ...) mkcl_noreturn;
  extern MKCL_API void mkcl_FEwin32_stream_error(MKCL, mkcl_object stream, const char *msg, int narg, ...) mkcl_noreturn;
#endif

  /* eval.c */

  extern MKCL_API mkcl_object mk_cl_funcall(MKCL, mkcl_narg narg, mkcl_object fun, ...);
  extern MKCL_API mkcl_object mk_cl_apply(MKCL, mkcl_narg narg, mkcl_object fun, mkcl_object arg, ...);
  extern MKCL_API mkcl_object mk_si_top_apply(MKCL, mkcl_object fun, mkcl_object args);
  extern MKCL_API mkcl_object mk_si_safe_eval(MKCL, mkcl_object form, mkcl_object lex_env, mkcl_object value);
  extern MKCL_API mkcl_object mk_si_unlink_symbol(MKCL, mkcl_object s);
  extern MKCL_API mkcl_object mk_cl_eval(MKCL, mkcl_object form);
  extern MKCL_API mkcl_object mk_cl_constantp(MKCL, mkcl_narg narg, mkcl_object arg, ...);

  extern MKCL_API mkcl_object mk_cl_apply_from_stack(MKCL, mkcl_index narg, mkcl_object fun);
  extern MKCL_API mkcl_object mkcl_apply_from_temp_stack_frame(MKCL, mkcl_object f, mkcl_object o);
  extern MKCL_API mkcl_object _mkcl_link_call(MKCL, mkcl_object sym, mkcl_objectfn *pLK, mkcl_object cblock, int narg, mkcl_va_list args);

  static inline mkcl_object mkcl_validate_function(MKCL, mkcl_object fun)
  {
    for (;;)
      if (mkcl_unlikely(fun == mk_cl_Cnil))
	mkcl_FEundefined_function(env, fun);
      else
	{
	  mkcl_type obj_type = fun->d.t;
	  
	  if (mkcl_likely( mkcl_t_cfun == (obj_type & (((~0UL) << 5) + 3)) )) {
	    env->function = fun;
	    return fun;
	  } else if (mkcl_unlikely( obj_type == mkcl_t_symbol )) {
	    if (fun->symbol.stype & mkcl_stp_macro)
	      mkcl_FEundefined_function(env, fun);
	    fun = MKCL_SYM_FUN(fun);
	  } else
	    mkcl_FEinvalid_function(env, fun);
	}
  }

  static inline mkcl_object mkcl_validate_sym_fun(MKCL, mkcl_object sym)
  {
    mkcl_object fun = sym->symbol.gfdef;

    if (fun == mk_cl_Cnil)
      mkcl_FEundefined_function(env, sym);
    else if ( mkcl_t_cfun == ((fun->d.t) & (((~0UL) << 5) + 3)) ) {
      env->function = fun;
      return fun;
    } else
      mkcl_FEinvalid_function(env, fun);
  }

#define mkcl_funcall0(e, fun) (mkcl_validate_function(e, fun)->cfun.f._[0](e))
#define mkcl_funcall1(e, fun, a0) (mkcl_validate_function(e, fun)->cfun.f._[1](e, a0))
#define mkcl_funcall2(e, fun, a0, a1) (mkcl_validate_function(e, fun)->cfun.f._[2](e, a0, a1))
#define mkcl_funcall3(e, fun, a0, a1, a2) (mkcl_validate_function(e, fun)->cfun.f._[3](e, a0, a1, a2))
#define mkcl_funcall4(e, fun, a0, a1, a2, a3) (mkcl_validate_function(e, fun)->cfun.f._[4](e, a0, a1, a2, a3))



  /* ffi.c */

  extern MKCL_API mkcl_object mk_si_foreignp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_allocate_foreign_data(MKCL, mkcl_object tag, mkcl_object size);
  extern MKCL_API mkcl_object mk_si_make_foreign_null_pointer(MKCL);
  extern MKCL_API mkcl_object mk_si_foreign_address(MKCL, mkcl_object f);
  extern MKCL_API mkcl_object mk_si_foreign_indexed(MKCL, mkcl_object f, mkcl_object ndx, mkcl_object size, mkcl_object tag);
  extern MKCL_API mkcl_object mk_si_foreign_ref(MKCL, mkcl_object f, mkcl_object ndx, mkcl_object size, mkcl_object tag);
  extern MKCL_API mkcl_object mk_si_foreign_ref_elt(MKCL, mkcl_object f, mkcl_object ndx, mkcl_object tag);
  extern MKCL_API mkcl_object mk_si_foreign_set(MKCL, mkcl_object f, mkcl_object ndx, mkcl_object value);
  extern MKCL_API mkcl_object mk_si_foreign_set_elt(MKCL, mkcl_object f, mkcl_object ndx, mkcl_object tag, mkcl_object value);
  extern MKCL_API mkcl_object mk_si_foreign_tag(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_foreign_recast(MKCL, mkcl_object f, mkcl_object size, mkcl_object tag);
  extern MKCL_API mkcl_object mk_si_free_foreign_data(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_make_foreign_data_from_array(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_null_pointer_p(MKCL, mkcl_object f);
  extern MKCL_API mkcl_object mk_si_size_of_foreign_elt_type(MKCL, mkcl_object tag);
  extern MKCL_API mkcl_object mk_si_load_foreign_module(MKCL, mkcl_object module);
  extern MKCL_API mkcl_object mk_si_unload_foreign_module(MKCL, mkcl_object module);
  extern MKCL_API mkcl_object mk_si_find_foreign_symbol(MKCL, mkcl_object var, mkcl_object module, mkcl_object type, mkcl_object size);
  extern MKCL_API mkcl_object mk_si_call_cfun(MKCL, mkcl_narg, mkcl_object fun, mkcl_object return_type, mkcl_object arg_types, mkcl_object args, ...);
  extern MKCL_API mkcl_object mk_si_make_dynamic_callback(MKCL, mkcl_narg, mkcl_object fun, mkcl_object sym, mkcl_object return_type, mkcl_object arg_types, ...);
  extern MKCL_API mkcl_object mk_si_trim_ffi_arguments_staging_area(MKCL);
  extern MKCL_API mkcl_object mk_si_release_ffi_area(MKCL);
  extern MKCL_API mkcl_object mk_si_pointer(MKCL, mkcl_object x);

  extern MKCL_API mkcl_object mkcl_make_foreign(MKCL, mkcl_object type_tag, mkcl_index data_size, void * foreign_data_pointer);
  extern MKCL_API mkcl_object mkcl_allocate_foreign_data(MKCL, mkcl_object tag, mkcl_index size);
  extern MKCL_API void * mkcl_foreign_raw_pointer(MKCL, mkcl_object f);
  extern MKCL_API char * mkcl_base_string_raw_pointer(MKCL, mkcl_object f);
  extern MKCL_API mkcl_object mkcl_null_terminated_base_string(MKCL, mkcl_object s);
  extern MKCL_API mkcl_object mkcl_foreign_ref_elt(MKCL, void *p, enum mkcl_ffi_tag type);
  extern MKCL_API void mkcl_foreign_set_elt(MKCL, void *p, enum mkcl_ffi_tag type, mkcl_object value);

  static inline bool mkcl_foreignp(MKCL, mkcl_object x)
  {
    mkcl_type t = mkcl_type_of(x);
    return (t == mkcl_t_foreign);
  }

  /* file.c */

#define MKCL_LISTEN_NO_CHAR	0
#define MKCL_LISTEN_AVAILABLE	1
#define MKCL_LISTEN_EOF		-1
#define MKCL_LISTEN_ERROR       -3

  extern MKCL_API mkcl_object mk_cl_make_synonym_stream(MKCL, mkcl_object sym);
  extern MKCL_API mkcl_object mk_cl_synonym_stream_symbol(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mk_cl_make_two_way_stream(MKCL, mkcl_object strm1, mkcl_object strm2);
  extern MKCL_API mkcl_object mk_cl_two_way_stream_input_stream(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mk_cl_two_way_stream_output_stream(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mk_cl_make_echo_stream(MKCL, mkcl_object strm1, mkcl_object strm2);
  extern MKCL_API mkcl_object mk_cl_echo_stream_input_stream(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mk_cl_echo_stream_output_stream(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mk_cl_make_string_output_stream(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_get_output_stream_string(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mk_cl_streamp(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mk_cl_input_stream_p(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mk_cl_output_stream_p(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mk_cl_stream_element_type(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mk_cl_stream_external_format(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mk_si_stream_external_format_set(MKCL, mkcl_object stream, mkcl_object format);
  extern MKCL_API mkcl_object mk_cl_file_length(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mk_si_get_string_input_stream_index(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mk_si_make_string_output_stream_from_string(MKCL, mkcl_object strng, mkcl_object external_format);
  extern MKCL_API mkcl_object mk_si_copy_stream(MKCL, mkcl_object in, mkcl_object out);
  extern MKCL_API mkcl_object mk_cl_open_stream_p(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mk_cl_make_broadcast_stream(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_broadcast_stream_streams(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mk_cl_make_concatenated_stream(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_concatenated_stream_streams(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mk_cl_make_string_input_stream(MKCL, mkcl_narg narg, mkcl_object strng, ...);
  extern MKCL_API mkcl_object mk_cl_close(MKCL, mkcl_narg narg, mkcl_object strm, ...);
  extern MKCL_API mkcl_object mk_cl_open(MKCL, mkcl_narg narg, mkcl_object filename, ...);
  extern MKCL_API mkcl_object mk_cl_file_position(MKCL, mkcl_narg narg, mkcl_object file_stream, ...);
  extern MKCL_API mkcl_object mk_cl_file_string_length(MKCL, mkcl_object stream, mkcl_object string);
  extern MKCL_API mkcl_object mk_si_do_write_sequence(MKCL, mkcl_object string, mkcl_object stream, mkcl_object start, mkcl_object end);
  extern MKCL_API mkcl_object mk_si_do_read_sequence(MKCL, mkcl_object string, mkcl_object stream, mkcl_object start, mkcl_object end);
  extern MKCL_API mkcl_object mk_si_file_column(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mk_cl_interactive_stream_p(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mk_si_set_buffering_mode(MKCL, mkcl_object strm, mkcl_object mode);
  extern MKCL_API mkcl_object mk_si_get_buffering_mode(MKCL, mkcl_object strm);

  extern MKCL_API bool mkcl_input_stream_p(MKCL, mkcl_object strm);
  extern MKCL_API bool mkcl_output_stream_p(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mkcl_stream_element_type(MKCL, mkcl_object strm);
  extern MKCL_API bool mkcl_interactive_stream_p(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mkcl_open_stream(MKCL, mkcl_object fn, enum mkcl_smmode smm, mkcl_object if_exists, mkcl_object if_does_not_exist, mkcl_object element_type, mkcl_object external_format);
  extern MKCL_API mkcl_object mkcl_make_string_input_stream(MKCL, mkcl_object strng, mkcl_index istart, mkcl_index iend, mkcl_object external_format);
  extern MKCL_API mkcl_object mkcl_make_string_output_stream(MKCL, mkcl_index line_length, bool extended, mkcl_object external_format);
  extern MKCL_API mkcl_object mkcl_read_byte(MKCL, mkcl_object strm);
  extern MKCL_API void mkcl_write_byte(MKCL, mkcl_object byte, mkcl_object strm);
  extern MKCL_API mkcl_character mkcl_read_char_noeof(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_character mkcl_read_char(MKCL, mkcl_object strm);
  extern MKCL_API void mkcl_unread_char(MKCL, mkcl_character c, mkcl_object strm);
  extern MKCL_API mkcl_character mkcl_peek_char(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_character mkcl_write_char(MKCL, mkcl_character c, mkcl_object strm);
  extern MKCL_API void mkcl_write_cstr(MKCL, const char *s, mkcl_object strm);
  extern MKCL_API void mkcl_force_output(MKCL, mkcl_object strm);
  extern MKCL_API void mkcl_finish_output(MKCL, mkcl_object strm);
  extern MKCL_API void mkcl_clear_input(MKCL, mkcl_object strm);
  extern MKCL_API void mkcl_clear_output(MKCL, mkcl_object strm);
  extern MKCL_API int mkcl_listen_stream(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mkcl_file_position(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mkcl_file_position_set(MKCL, mkcl_object strm, mkcl_object disp);
  extern MKCL_API mkcl_object mkcl_file_length(MKCL, mkcl_object strm);
  extern MKCL_API int mkcl_file_column(MKCL, mkcl_object strm);
  extern MKCL_API mkcl_object mkcl_make_stream_from_fd(MKCL, mkcl_object fname, mkcl_index fd, enum mkcl_smmode smm, mkcl_object element_type, mkcl_object external_format);
  extern MKCL_API int mkcl_stream_to_handle(MKCL, mkcl_object s, bool output);

  /* finalize.c (a.k.a. alloc_2.d) */

  extern MKCL_API mkcl_object mk_si_get_finalizer(MKCL, mkcl_object o);
  extern MKCL_API mkcl_object mk_si_set_finalizer(MKCL, mkcl_object o, mkcl_object finalizer);
  extern MKCL_API mkcl_object mk_si_set_heap_size_limit(MKCL, mkcl_object size_limit);
  extern MKCL_API mkcl_object mk_si_get_heap_size_limit(MKCL);
  

  /* format.c */

  extern MKCL_API mkcl_object mk_cl_format(MKCL, mkcl_narg narg, mkcl_object stream, mkcl_object string, ...);

  /* gbc.c */

#define MK_GC_enabled() (!MK_GC_dont_gc)



  /* gfun.c */

  extern MKCL_API void _mkcl_set_method_hash_size(MKCL, mkcl_index size);
  extern MKCL_API mkcl_object mk_si_clear_gfun_cache(MKCL, mkcl_object what);
  extern MKCL_API mkcl_object mk_clos_set_funcallable_instance_function(MKCL, mkcl_object x, mkcl_object function_or_t);
  extern MKCL_API mkcl_object mk_si_generic_function_p(MKCL, mkcl_object instance);

  extern MKCL_API mkcl_object _mkcl_standard_dispatch(MKCL, mkcl_object frame, mkcl_object fun);


  /* hash.c */

  extern MKCL_API mkcl_object mk_cl__make_hash_table(MKCL, mkcl_object test, mkcl_object size, mkcl_object rehash_size, mkcl_object rehash_threshold);
  extern MKCL_API mkcl_object mk_cl_hash_table_p(MKCL, mkcl_object ht);
  extern MKCL_API mkcl_object mk_si_hash_set(MKCL, mkcl_object key, mkcl_object ht, mkcl_object val);
  extern MKCL_API mkcl_object mk_cl_remhash(MKCL, mkcl_object key, mkcl_object ht);
  extern MKCL_API mkcl_object mk_cl_clrhash(MKCL, mkcl_object ht);
  extern MKCL_API mkcl_object mk_cl_hash_table_count(MKCL, mkcl_object ht);
  extern MKCL_API mkcl_object mk_cl_sxhash(MKCL, mkcl_object key);
  extern MKCL_API mkcl_object mk_cl_maphash(MKCL, mkcl_object fun, mkcl_object ht);
  extern MKCL_API mkcl_object mk_cl_hash_table_rehash_size(MKCL, mkcl_object ht);
  extern MKCL_API mkcl_object mk_cl_hash_table_rehash_threshold(MKCL, mkcl_object ht);
  extern MKCL_API mkcl_object mk_cl_hash_table_size(MKCL, mkcl_object ht);
  extern MKCL_API mkcl_object mk_cl_hash_table_test(MKCL, mkcl_object ht);
  extern MKCL_API mkcl_object mk_si_hash_table_iterator(MKCL, mkcl_object ht);
  extern MKCL_API mkcl_object mk_cl_make_hash_table(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_gethash(MKCL, mkcl_narg narg, mkcl_object key, mkcl_object ht, ...);
  extern MKCL_API mkcl_object mk_si_copy_hash_table(MKCL, mkcl_object orig);
  extern MKCL_API mkcl_object mk_si_hash_eql(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_si_hash_equal(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_si_hash_equalp(MKCL, mkcl_narg narg, ...);

  extern MKCL_API void mkcl_sethash(MKCL, mkcl_object key, mkcl_object hashtable, mkcl_object value);
  extern MKCL_API mkcl_object mkcl_gethash_safe(MKCL, mkcl_object key, mkcl_object hash, mkcl_object def);
  extern MKCL_API bool mkcl_remhash(MKCL, mkcl_object key, mkcl_object hash);

  static inline struct mkcl_hashtable_entry *
  mkcl_search_hash(MKCL, mkcl_object key, mkcl_object hashtable)
  {
    return(hashtable->hash.search_fun(env, key, hashtable));
  }

  extern MKCL_API mkcl_object mk_si_hash_tables_statistics(MKCL);

  /* instance.c */

  extern MKCL_API mkcl_object mk_si_allocate_raw_instance(MKCL, mkcl_object orig, mkcl_object clas, mkcl_object size);
  extern MKCL_API mkcl_object mk_si_instance_class(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_instance_class_set(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_si_instance_length(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_instance_ref(MKCL, mkcl_object x, mkcl_object index);
  extern MKCL_API mkcl_object mk_si_instance_ref_safe(MKCL, mkcl_object x, mkcl_object index);
  extern MKCL_API mkcl_object mk_si_instance_set(MKCL, mkcl_object x, mkcl_object index, mkcl_object value);
  extern MKCL_API mkcl_object mk_si_instancep(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_unbound(MKCL);
  extern MKCL_API mkcl_object mk_si_sl_boundp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_sl_makunbound(MKCL, mkcl_object x, mkcl_object index);
  extern MKCL_API mkcl_object mk_si_instance_sig(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_instance_sig_set(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_instance_sig_set2(MKCL, mkcl_object x, mkcl_object sig);

  extern MKCL_API mkcl_object mkcl_allocate_instance(MKCL, mkcl_object clas, mkcl_index size);
  extern MKCL_API mkcl_object mk_si_copy_instance(MKCL, mkcl_object x);

  extern MKCL_API mkcl_object mk_si_set_class_proper_name(MKCL, mkcl_object sym, mkcl_object class);

  extern MKCL_API mkcl_object mkcl_slot_value(MKCL, mkcl_object x, const char *slot);
  extern MKCL_API mkcl_object mkcl_slot_value_set(MKCL, mkcl_object x, const char *slot, mkcl_object y);


  /* list.c */

  extern MKCL_API void mkcl_FEtype_error_list(MKCL, mkcl_object x) mkcl_noreturn;
  static inline mkcl_object mk_cl_car(MKCL, mkcl_object x)
  {
    if (mkcl_Null(x))
      return ((env->nvalues = 1),(env->values[0] = mk_cl_Cnil));
    else if (mkcl_likely(MKCL_F_CONSP(x)))
      return ((env->nvalues = 1),(env->values[0] = MKCL_CONS_CAR(x)));
    else
      mkcl_FEtype_error_list(env, x);
  }

  static inline mkcl_object mk_cl_cdr(MKCL, mkcl_object x)
  {
    if (mkcl_Null(x))
      return ((env->nvalues = 1),(env->values[0] = mk_cl_Cnil));
    else if (mkcl_likely(MKCL_F_CONSP(x)))
      return ((env->nvalues = 1),(env->values[0] = MKCL_CONS_CDR(x)));
    else
      mkcl_FEtype_error_list(env, x);
  }

  extern MKCL_API mkcl_object mk_cl_caar(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cadr(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cdar(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cddr(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_caaar(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_caadr(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cadar(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_caddr(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cdaar(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cdadr(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cddar(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cdddr(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_caaaar(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_caaadr(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_caadar(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_caaddr(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cadaar(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cadadr(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_caddar(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cadddr(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cdaaar(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cdaadr(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cdadar(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cdaddr(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cddaar(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cddadr(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cdddar(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cddddr(MKCL, mkcl_object x);
#define mk_cl_rest mk_cl_cdr
#define mk_cl_first mk_cl_car
#define mk_cl_second mk_cl_cadr
#define mk_cl_third mk_cl_caddr
#define mk_cl_fourth mk_cl_cadddr

  extern MKCL_API mkcl_object mk_cl_fifth(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_sixth(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_seventh(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_eighth(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_ninth(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_tenth(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_endp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_list_length(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_nth(MKCL, mkcl_object n, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_nthcdr(MKCL, mkcl_object n, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_copy_list(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_copy_alist(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_copy_tree(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_revappend(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_ldiff(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_rplaca(MKCL, mkcl_object x, mkcl_object v);
  extern MKCL_API mkcl_object mk_cl_rplacd(MKCL, mkcl_object x, mkcl_object v);
  extern MKCL_API mkcl_object mk_cl_tailp(MKCL, mkcl_object y, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_memq(MKCL, mkcl_object x, mkcl_object l);
  extern MKCL_API mkcl_object mk_cl_nreconc(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_cons(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_acons(MKCL, mkcl_object x, mkcl_object y, mkcl_object z);
  extern MKCL_API mkcl_object mk_cl_list(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_listX(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_append(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_tree_equal(MKCL, mkcl_narg narg, mkcl_object x, mkcl_object y, ...);
  extern MKCL_API mkcl_object mk_cl_last(MKCL, mkcl_narg narg, mkcl_object x, ...);
  extern MKCL_API mkcl_object mk_cl_make_list(MKCL, mkcl_narg narg, mkcl_object size, ...);
  extern MKCL_API mkcl_object mk_cl_nconc(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_butlast(MKCL, mkcl_narg narg, mkcl_object lis, ...);
  extern MKCL_API mkcl_object mk_cl_nbutlast(MKCL, mkcl_narg narg, mkcl_object lis, ...);
  extern MKCL_API mkcl_object mk_cl_subst(MKCL, mkcl_narg narg, mkcl_object new_obj, mkcl_object old_obj, mkcl_object tree, ...);
  extern MKCL_API mkcl_object mk_cl_nsubst(MKCL, mkcl_narg narg, mkcl_object new_obj, mkcl_object old_obj, mkcl_object tree, ...);
  extern MKCL_API mkcl_object mk_cl_sublis(MKCL, mkcl_narg narg, mkcl_object alist, mkcl_object tree, ...);
  extern MKCL_API mkcl_object mk_cl_nsublis(MKCL, mkcl_narg narg, mkcl_object alist, mkcl_object tree, ...);
  extern MKCL_API mkcl_object mk_cl_member(MKCL, mkcl_narg narg, mkcl_object item, mkcl_object list, ...);
  extern MKCL_API mkcl_object mk_si_member1(MKCL, mkcl_object item, mkcl_object list, mkcl_object test, mkcl_object test_not, mkcl_object key);
  extern MKCL_API mkcl_object mk_cl_adjoin(MKCL, mkcl_narg narg, mkcl_object item, mkcl_object list, ...);
  extern MKCL_API mkcl_object mk_cl_pairlis(MKCL, mkcl_narg narg, mkcl_object keys, mkcl_object data, ...);
  extern MKCL_API mkcl_object mk_cl_rassoc(MKCL, mkcl_narg narg, mkcl_object item, mkcl_object alist, ...);
  extern MKCL_API mkcl_object mk_cl_assoc(MKCL, mkcl_narg narg, mkcl_object item, mkcl_object alist, ...);

  extern MKCL_API mkcl_object mkcl_last(MKCL, mkcl_object x, mkcl_index n);
  extern MKCL_API mkcl_object mkcl_butlast(MKCL, mkcl_object x, mkcl_index n);
  extern MKCL_API mkcl_object mkcl_nbutlast(MKCL, mkcl_object x, mkcl_index n);
  extern MKCL_API mkcl_object mkcl_list_length(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_append(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API bool mkcl_endp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_nth(MKCL, mkcl_word n, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_nthcdr(MKCL, mkcl_word n, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_nconc(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API bool mkcl_member_eq(MKCL, mkcl_object x, mkcl_object l);
  extern MKCL_API mkcl_object mkcl_memq(MKCL, mkcl_object x, mkcl_object l);
  extern MKCL_API mkcl_object mkcl_memql(MKCL, mkcl_object x, mkcl_object l);
  extern MKCL_API mkcl_object mkcl_member(MKCL, mkcl_object x, mkcl_object l);
  extern MKCL_API mkcl_object mkcl_adjoin_eq(MKCL, mkcl_object item, mkcl_object list);
  extern MKCL_API mkcl_object mkcl_adjoin(MKCL, mkcl_object item, mkcl_object list);
  extern MKCL_API mkcl_object mkcl_assq(MKCL, mkcl_object x, mkcl_object l);
  extern MKCL_API mkcl_object mkcl_assql(MKCL, mkcl_object x, mkcl_object l);
  extern MKCL_API mkcl_object mkcl_assoc(MKCL, mkcl_object x, mkcl_object l);
  extern MKCL_API mkcl_object mkcl_assqlp(MKCL, mkcl_object x, mkcl_object l);
  extern MKCL_API mkcl_object mkcl_remove_eq(MKCL, mkcl_object x, mkcl_object l);
  extern MKCL_API mkcl_object mkcl_copy_proper_list(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_reverse_proper_list(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_nreverse_proper_list(MKCL, mkcl_object x);

  extern MKCL_API mkcl_object mk_si_dyn_cons(MKCL, mkcl_object car, mkcl_object cdr);
  extern MKCL_API mkcl_object mk_si_trim_dynamic_cons_stack(MKCL);


  /* load.c */

  extern MKCL_API mkcl_object mk_si_load_source(MKCL, mkcl_object file, mkcl_object verbose, mkcl_object print, mkcl_object external_format);
  extern MKCL_API mkcl_object mk_si_load_binary(MKCL, mkcl_object file, mkcl_object verbose, mkcl_object print, mkcl_object external_format);
  extern MKCL_API mkcl_object mk_cl_load(MKCL, mkcl_narg narg, mkcl_object pathname, ...);

  extern MKCL_API mkcl_object mk_si_list_libraries(MKCL);

  /* macros.c */

  extern MKCL_API mkcl_object mk_cl_macroexpand(MKCL, mkcl_narg narg, mkcl_object form, ...);
  extern MKCL_API mkcl_object mk_cl_macroexpand_1(MKCL, mkcl_narg narg, mkcl_object form, ...);
  extern MKCL_API mkcl_object mk_cl_macro_function(MKCL, mkcl_narg narg, mkcl_object sym, ...);



  /* main.c */

  extern MKCL_API mkcl_index mkcl_argc(void);
  extern MKCL_API mkcl_object mk_mkcl_argc(MKCL);
  extern MKCL_API mkcl_object mkcl_argv(MKCL, mkcl_index index);
  extern MKCL_API mkcl_object mk_mkcl_argv(MKCL, mkcl_object index);
#if MKCL_WINDOWS
  extern MKCL_API void mkcl_get_commandline_args_from_Windows(int * argc_ref, char *** argv_ref);
  extern MKCL_API bool mkcl_has_console(void);
#endif
  extern MKCL_API mkcl_object mk_mkcl_getenv(MKCL, mkcl_object var);
  extern MKCL_API mkcl_object mkcl_getenv(MKCL, mkcl_object var);
  extern MKCL_API mkcl_object mk_mkcl_setenv(MKCL, mkcl_object var, mkcl_object value);
  extern MKCL_API mkcl_object mkcl_setenv(MKCL, mkcl_object var, mkcl_object value);
  extern MKCL_API mkcl_object mk_si_shutdown_mkcl(MKCL, mkcl_object code, mkcl_object watchdog_thread, mkcl_object verbose, mkcl_object clean);
  extern MKCL_API mkcl_object mk_si_shutdown_mkcl_threads(MKCL, mkcl_object code, mkcl_object watchdog_thread, mkcl_object verbose, mkcl_object clean);
  extern MKCL_API mkcl_object mk_si_gdb(MKCL);

  typedef enum mkcl_option {
    MKCL_OPT_INCREMENTAL_GC = 0,
    MKCL_OPT_CHAIN_SIGSEGV,
    MKCL_OPT_CHAIN_SIGFPE,
    MKCL_OPT_CHAIN_SIGINT,
    MKCL_OPT_CHAIN_SIGILL,
    MKCL_OPT_CHAIN_SIGBUS,
    MKCL_OPT_THREAD_INTERRUPT_SIGNAL,
    MKCL_OPT_THREAD_RESUME_SIGNAL,
    MKCL_OPT_THREAD_WAKE_UP_SIGNAL,
    MKCL_OPT_GC_THREAD_SUSPEND_SIGNAL,
    MKCL_OPT_GC_THREAD_RESTART_SIGNAL,
    MKCL_OPT_SET_GMP_MEMORY_FUNCTIONS,
    /* Options here above this one cannot be changed once MKCL is booted,
       which is signaled by setting option MKCL_OPT_BOOTED to true. JCB
     */
    MKCL_OPT_BOOTED,
    MKCL_OPT_BINDING_STACK_INITIAL_SIZE,
    MKCL_OPT_BINDING_STACK_OVERFLOW_SIZE,
    MKCL_OPT_FRAME_STACK_INITIAL_SIZE,
    MKCL_OPT_FRAME_STACK_OVERFLOW_SIZE,
    MKCL_OPT_LISP_TEMP_STACK_INITIAL_SIZE,
    MKCL_OPT_LISP_TEMP_STACK_OVERFLOW_SIZE,
    MKCL_OPT_INTERRUPT_THREAD_CALL_STACK_SIZE,
    MKCL_OPT_CALL_STACK_OVERFLOW_SIZE,
    MKCL_OPT_HEAP_SIZE,
    MKCL_OPT_HEAP_SAFETY_AREA,
    MKCL_OPT_MAXIMUM /* Not a real option, just an end of enum sequence marker. */
  } mkcl_option;

#define MKCL_UNBOUND_OPTION -1
#define MKCL_BAD_OPTION -2
#define MKCL_IMMUTABLE_OPTION -3


  struct mkcl_thread_init_parameters
  {
    mkcl_index lisp_temp_stack_initial_size;
    mkcl_index lisp_temp_stack_size_limit;
    mkcl_index binding_stack_initial_size;
    mkcl_index binding_stack_size_limit;
    mkcl_index frame_stack_initial_size;
    mkcl_index frame_stack_size_limit;
    mkcl_index sigaltstack_size;
    size_t call_stack_size;
    void * call_stack_addr; /* Posix only. */
    void * stack_mark_address;
  };

#define MKCL_UNLIMITED 0


  extern MKCL_API mkcl_env mkcl_boot(int argc, char **argv, struct mkcl_thread_init_parameters * params);
  extern MKCL_API int mkcl_set_option(mkcl_option option, mkcl_word value);
  extern MKCL_API mkcl_word mkcl_get_option(mkcl_option option);
  extern MKCL_API mkcl_object mk_si_shutdown_in_progress_p(MKCL);
  extern MKCL_API mkcl_object mk_si_register_shutdown_thread(MKCL, mkcl_object shutdown_thread);
  extern MKCL_API mkcl_object mk_si_register_shutdown_watchdog_thread(MKCL, mkcl_object watchdog_thread, mkcl_object will_clean_up);
  extern MKCL_API mkcl_object mk_si_shutdown_watchdog_thread(MKCL);
  extern MKCL_API int mkcl_shutdown_watchdog(MKCL);
  extern MKCL_API long mkcl_exit_status(MKCL);
  extern MKCL_API mkcl_object mk_si_self_truename(MKCL);



  /* mapfun.c */

  extern MKCL_API mkcl_object mk_cl_mapcar(MKCL, mkcl_narg narg, mkcl_object fun, ...);
  extern MKCL_API mkcl_object mk_cl_maplist(MKCL, mkcl_narg narg, mkcl_object fun, ...);
  extern MKCL_API mkcl_object mk_cl_mapc(MKCL, mkcl_narg narg, mkcl_object fun, ...);
  extern MKCL_API mkcl_object mk_cl_mapl(MKCL, mkcl_narg narg, mkcl_object fun, ...);
  extern MKCL_API mkcl_object mk_cl_mapcan(MKCL, mkcl_narg narg, mkcl_object fun, ...);
  extern MKCL_API mkcl_object mk_cl_mapcon(MKCL, mkcl_narg narg, mkcl_object fun, ...);


  /* multival.c */

  extern MKCL_API mkcl_object mk_cl_values_list(MKCL, mkcl_object list);
  extern MKCL_API mkcl_object mk_cl_values(MKCL, mkcl_narg narg, ...);


  /* num_arith.c */

  extern MKCL_API mkcl_object mk_cl_conjugate(MKCL, mkcl_object c);
  extern MKCL_API mkcl_object mk_cl_1P(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_1M(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_X(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_P(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_M(MKCL, mkcl_narg narg, mkcl_object num, ...);
  extern MKCL_API mkcl_object mk_cl_N(MKCL, mkcl_narg narg, mkcl_object num, ...);
  extern MKCL_API mkcl_object mk_cl_gcd(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_lcm(MKCL, mkcl_narg narg, ...);

  extern MKCL_API mkcl_object mkcl_word_times(MKCL, mkcl_word i, mkcl_word j);
  extern MKCL_API mkcl_object mkcl_times(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object number_to_complex(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_plus(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mkcl_minus(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mkcl_negate(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_divide(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mkcl_integer_divide(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mkcl_gcd(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mkcl_one_plus(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_one_minus(MKCL, mkcl_object x);


  /* number.c */

  extern MKCL_API mkcl_word mkcl_integer_to_word(MKCL, mkcl_object x);
  extern MKCL_API mkcl_index mkcl_integer_to_index(MKCL, mkcl_object x);
  extern MKCL_API mkcl_word mkcl_fixnum_in_range(MKCL, mkcl_object fun,
					       const char *what, mkcl_object value,
					       mkcl_word min, mkcl_word max);
  extern MKCL_API mkcl_object mkcl_make_big_integer(MKCL, mkcl_word i);
  extern MKCL_API mkcl_object mkcl_make_big_unsigned_integer(MKCL, mkcl_index i);
  extern MKCL_API mkcl_uint8_t mkcl_to_uint8_t(MKCL, mkcl_object o);
  extern MKCL_API mkcl_int8_t mkcl_to_int8_t(MKCL, mkcl_object o);
#define mkcl_make_uint8_t(e,i) MKCL_MAKE_FIXNUM(i)
#define mkcl_make_int8_t(e,i) MKCL_MAKE_FIXNUM(i)

#if MKCL_WORD_BITS < 32
# error "Unsupported platform with MKCL_WORD_BITS < 32"
#endif

  extern MKCL_API mkcl_uint16_t mkcl_to_uint16_t(MKCL, mkcl_object o);
  extern MKCL_API mkcl_int16_t mkcl_to_int16_t(MKCL, mkcl_object o);
# define mkcl_make_uint16_t(e,i) MKCL_MAKE_FIXNUM(i)
# define mkcl_make_int16_t(e,i) MKCL_MAKE_FIXNUM(i)


# if MKCL_WORD_BITS == 32
#  define mkcl_to_uint32_t mkcl_integer_to_index
#  define mkcl_to_int32_t mkcl_integer_to_word
#  define mkcl_make_uint32_t mkcl_make_unsigned_integer
#  define mkcl_make_int32_t mkcl_make_integer
# elif MKCL_WORD_BITS >= (32 + 2)
#  define mkcl_make_uint32_t(e,i) MKCL_MAKE_FIXNUM(i)
#  define mkcl_make_int32_t(e,i) MKCL_MAKE_FIXNUM(i)
  extern MKCL_API mkcl_uint32_t mkcl_to_uint32_t(MKCL, mkcl_object o);
  extern MKCL_API mkcl_int32_t mkcl_to_int32_t(MKCL, mkcl_object o);
# else
# error Cannot convert from int32 to fixnum!
# endif


# if (MKCL_WORD_BITS >= 64) && (MKCL_LONG_BITS >= MKCL_WORD_BITS)
#  define mkcl_to_uint64_t mkcl_integer_to_index
#  define mkcl_to_int64_t mkcl_integer_to_word
#  define mkcl_make_uint64_t mkcl_make_unsigned_integer
#  define mkcl_make_int64_t mkcl_make_integer
# else
  extern MKCL_API mkcl_uint64_t mkcl_to_uint64_t(MKCL, mkcl_object p);
  extern MKCL_API mkcl_int64_t mkcl_to_int64_t(MKCL, mkcl_object p);
  extern MKCL_API mkcl_object mkcl_make_uint64_t(MKCL, mkcl_uint64_t i);
  extern MKCL_API mkcl_object mkcl_make_int64_t(MKCL, mkcl_int64_t i);
# endif


#if MKCL_INT_BITS == 32
# define mkcl_to_uint mkcl_to_uint32_t
# define mkcl_to_int mkcl_to_int32_t
# define mkcl_make_uint mkcl_make_uint32_t
# define mkcl_make_int mkcl_make_int32_t
#elif MKCL_INT_BITS == 64
# define mkcl_to_uint mkcl_to_uint64_t
# define mkcl_to_int mkcl_to_int64_t
# define mkcl_make_uint mkcl_make_uint64_t
# define mkcl_make_int mkcl_make_int64_t
#else
# error "Type 'int' must be either 32 or 64 bits wide"
#endif

#if MKCL_LONG_BITS == 32
# define mkcl_to_ulong mkcl_to_uint32_t
# define mkcl_to_long mkcl_to_int32_t
# define mkcl_make_ulong mkcl_make_uint32_t
# define mkcl_make_long mkcl_make_int32_t
#elif MKCL_LONG_BITS == 64
# define mkcl_to_ulong mkcl_to_uint64_t
# define mkcl_to_long mkcl_to_int64_t
# define mkcl_make_ulong mkcl_make_uint64_t
# define mkcl_make_long mkcl_make_int64_t
#else
# error "Type 'long' must be either 32 or 64 bits wide"
#endif

  extern MKCL_API mkcl_ulong_long_t mkcl_to_ulong_long(MKCL, mkcl_object p);
  extern MKCL_API mkcl_long_long_t mkcl_to_long_long(MKCL, mkcl_object p);
  extern MKCL_API mkcl_object mkcl_make_ulong_long(MKCL, mkcl_ulong_long_t i);
  extern MKCL_API mkcl_object mkcl_make_long_long(MKCL, mkcl_long_long_t i);

  extern MKCL_API mkcl_object mkcl_make_ratio(MKCL, mkcl_object num, mkcl_object den);
  extern MKCL_API mkcl_object mkcl_make_singlefloat(MKCL, float f);
  extern MKCL_API mkcl_object mkcl_make_doublefloat(MKCL, double f);
  extern MKCL_API mkcl_object mkcl_make_complex(MKCL, mkcl_object r, mkcl_object i);
  extern MKCL_API mkcl_object mk_cl_rational(MKCL, mkcl_object x);
#define mk_cl_rationalize mk_cl_rational  /* FIXME. This short cut is too lazy. JCB */

  extern MKCL_API double mkcl_to_double(MKCL, mkcl_object x);

  extern MKCL_API long double mkcl_to_long_double(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_make_longfloat(MKCL, long double f);

  extern MKCL_API mkcl_object mkcl_double_to_integer(MKCL, double d);
  extern MKCL_API mkcl_object mkcl_float_to_integer(MKCL, float d);

  extern MKCL_API mkcl_object mkcl_long_double_to_integer(MKCL, long double d);


  /* num_co.c */

  extern MKCL_API mkcl_object mk_cl_numerator(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_denominator(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_mod(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_rem(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_decode_float(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_scale_float(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_float_radix(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_float_digits(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_float_precision(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_integer_decode_float(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_realpart(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_imagpart(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_float(MKCL, mkcl_narg narg, mkcl_object x, ...);
  extern MKCL_API mkcl_object mk_cl_floor(MKCL, mkcl_narg narg, mkcl_object x, ...);
  extern MKCL_API mkcl_object mk_cl_ceiling(MKCL, mkcl_narg narg, mkcl_object x, ...);
  extern MKCL_API mkcl_object mk_cl_truncate(MKCL, mkcl_narg narg, mkcl_object x, ...);
  extern MKCL_API mkcl_object mk_cl_round(MKCL, mkcl_narg narg, mkcl_object x, ...);
  extern MKCL_API mkcl_object mk_cl_float_sign(MKCL, mkcl_narg narg, mkcl_object x, ...);
  extern MKCL_API mkcl_object mk_cl_complex(MKCL, mkcl_narg narg, mkcl_object r, ...);

  extern MKCL_API mkcl_object mkcl_floor1(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_ceiling1(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_truncate1(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_round1(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_floor2(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mkcl_ceiling2(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mkcl_truncate2(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mkcl_round2(MKCL, mkcl_object x, mkcl_object y);


  /* num_comp.c */

  extern MKCL_API mkcl_object mk_cl_E(MKCL, mkcl_narg narg, mkcl_object num, ...);
  extern MKCL_API mkcl_object mk_cl_NE(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_L(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_G(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_GE(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_LE(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_max(MKCL, mkcl_narg narg, mkcl_object max, ...);
  extern MKCL_API mkcl_object mk_cl_min(MKCL, mkcl_narg narg, mkcl_object min, ...);

  extern MKCL_API int mkcl_number_equalp(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API int mkcl_number_compare(MKCL, mkcl_object x, mkcl_object y);


  /* num_log.c */

#define MKCL_BOOLCLR		0
#define MKCL_BOOLAND		01
#define MKCL_BOOLANDC2		02
#define MKCL_BOOL1		03
#define MKCL_BOOLANDC1		04
#define MKCL_BOOL2		05
#define MKCL_BOOLXOR		06
#define MKCL_BOOLIOR		07
#define MKCL_BOOLNOR		010
#define MKCL_BOOLEQV		011
#define MKCL_BOOLC2		012
#define MKCL_BOOLORC2		013
#define MKCL_BOOLC1		014
#define MKCL_BOOLORC1		015
#define MKCL_BOOLNAND		016
#define MKCL_BOOLSET		017

  extern MKCL_API mkcl_object mk_cl_lognand(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_lognor(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_logandc1(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_logandc2(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_logorc1(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_logorc2(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_lognot(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_boole(MKCL, mkcl_object o, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_logbitp(MKCL, mkcl_object p, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_ash(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_logcount(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_integer_length(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_bit_array_op(MKCL, mkcl_object o, mkcl_object x, mkcl_object y, mkcl_object r);
  extern MKCL_API mkcl_object mk_cl_logior(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_logxor(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_logand(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_logeqv(MKCL, mkcl_narg narg, ...);

  extern MKCL_API mkcl_object mkcl_boole(MKCL, int op, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mkcl_ash(MKCL, mkcl_object x, mkcl_word w);
  extern MKCL_API int mkcl_word_bit_length(mkcl_word l);
  extern MKCL_API mkcl_index mkcl_integer_length(MKCL, mkcl_object i);


  /* num_pred.c */

  extern MKCL_API mkcl_object mk_cl_zerop(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_plusp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_minusp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_oddp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_evenp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_float_nan_p(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_float_infinity_p(MKCL, mkcl_object x);

  extern MKCL_API int mkcl_zerop(MKCL, mkcl_object x);
  extern MKCL_API int mkcl_plusp(MKCL, mkcl_object x);
  extern MKCL_API int mkcl_minusp(MKCL, mkcl_object x);
  extern MKCL_API int mkcl_oddp(MKCL, mkcl_object x);
  extern MKCL_API int mkcl_evenp(MKCL, mkcl_object x);
  extern MKCL_API bool mkcl_float_nan_p(MKCL, mkcl_object x);
  extern MKCL_API bool mkcl_float_infinity_p(MKCL, mkcl_object x);


  /* num_rand.c */

  extern MKCL_API mkcl_object mk_cl_random_state_p(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_random(MKCL, mkcl_narg narg, mkcl_object x, ...);
  extern MKCL_API mkcl_object mk_cl_make_random_state(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mkcl_make_random_state(MKCL, mkcl_object rs);


  /* num_sfun.c */

  extern MKCL_API mkcl_word mkcl_word_expt(mkcl_word x, mkcl_word y);
  extern MKCL_API mkcl_object mk_cl_abs(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_exp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_expt(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_sqrt(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_sin(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cos(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_tan(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_sinh(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_cosh(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_tanh(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_atan(MKCL, mkcl_narg narg, mkcl_object x, ...);
  extern MKCL_API mkcl_object mk_cl_log(MKCL, mkcl_narg narg, mkcl_object x, ...);
  extern MKCL_API mkcl_object mk_si_log1p(MKCL, mkcl_object x);

  extern MKCL_API mkcl_object mkcl_log1p(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_log1(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_log2(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mkcl_atan2(MKCL, mkcl_object y, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_atan1(MKCL, mkcl_object y);


  /* package.c */

  extern MKCL_API void mkcl_CEpackage_error(MKCL, mkcl_object package, char *message, char *continue_message, int narg, ...);
  extern MKCL_API mkcl_object mk_si_select_package(MKCL, mkcl_object pack_name);
  extern MKCL_API mkcl_object mk_cl_find_package(MKCL, mkcl_object p);
  extern MKCL_API mkcl_object mk_cl_package_name(MKCL, mkcl_object p);
  extern MKCL_API mkcl_object mk_cl_package_nicknames(MKCL, mkcl_object p);
  extern MKCL_API mkcl_object mk_cl_package_use_list(MKCL, mkcl_object p);
  extern MKCL_API mkcl_object mk_cl_package_used_by_list(MKCL, mkcl_object p);
  extern MKCL_API mkcl_object mk_cl_package_shadowing_symbols(MKCL, mkcl_object p);
  extern MKCL_API mkcl_object mk_cl_list_all_packages(MKCL);
  extern MKCL_API mkcl_object mk_si_package_hash_tables(MKCL, mkcl_object p);
  extern MKCL_API mkcl_object mk_si_close_package(MKCL, mkcl_object p);
  extern MKCL_API mkcl_object mk_si_reopen_package(MKCL, mkcl_object p);
  extern MKCL_API mkcl_object mk_si_package_closed_p(MKCL, mkcl_object p);
  extern MKCL_API mkcl_object mk_cl_delete_package(MKCL, mkcl_object p);
  extern MKCL_API mkcl_object mk_cl_make_package(MKCL, mkcl_narg narg, mkcl_object pack_name, ...);
  extern MKCL_API mkcl_object mk_cl_intern(MKCL, mkcl_narg narg, mkcl_object strng, ...);
  extern MKCL_API mkcl_object mk_cl_find_symbol(MKCL, mkcl_narg narg, mkcl_object strng, ...);
  extern MKCL_API mkcl_object mk_cl_unintern(MKCL, mkcl_narg narg, mkcl_object symbl, ...);
  extern MKCL_API mkcl_object mk_cl_export(MKCL, mkcl_narg narg, mkcl_object symbols, ...);
  extern MKCL_API mkcl_object mk_cl_unexport(MKCL, mkcl_narg narg, mkcl_object symbols, ...);
  extern MKCL_API mkcl_object mk_cl_import(MKCL, mkcl_narg narg, mkcl_object symbols, ...);
  extern MKCL_API mkcl_object mk_cl_rename_package(MKCL, mkcl_narg narg, mkcl_object pack, mkcl_object new_name, ...);
  extern MKCL_API mkcl_object mk_cl_shadowing_import(MKCL, mkcl_narg narg, mkcl_object symbols, ...);
  extern MKCL_API mkcl_object mk_cl_shadow(MKCL, mkcl_narg narg, mkcl_object symbols, ...);
  extern MKCL_API mkcl_object mk_cl_use_package(MKCL, mkcl_narg narg, mkcl_object pack, ...);
  extern MKCL_API mkcl_object mk_cl_unuse_package(MKCL, mkcl_narg narg, mkcl_object pack, ...);

  extern MKCL_API mkcl_object mkcl_make_package(MKCL, mkcl_object n, mkcl_object ns, mkcl_object ul);
  extern MKCL_API mkcl_object mkcl_rename_package(MKCL, mkcl_object x, mkcl_object n, mkcl_object ns);
  extern MKCL_API mkcl_object mkcl_find_package_nolock(MKCL, mkcl_object n);
  extern MKCL_API mkcl_object mk_si_coerce_to_package(MKCL, mkcl_object p);
  extern MKCL_API mkcl_object mkcl_current_package(MKCL);
  extern MKCL_API mkcl_object mkcl_find_symbol(MKCL, mkcl_object n, mkcl_object p, int *intern_flag);
  extern MKCL_API mkcl_object mkcl_intern(MKCL, mkcl_object name, mkcl_object p, int *intern_flag);
  extern MKCL_API mkcl_object _mkcl_intern(MKCL, const char *s, mkcl_object p);
  extern MKCL_API bool mkcl_unintern(MKCL, mkcl_object s, mkcl_object p);
  extern MKCL_API void mkcl_export2(MKCL, mkcl_object s, mkcl_object p);
  extern MKCL_API void mkcl_unexport2(MKCL, mkcl_object s, mkcl_object p);
  extern MKCL_API void mkcl_import2(MKCL, mkcl_object s, mkcl_object p);
  extern MKCL_API void mkcl_shadowing_import(MKCL, mkcl_object s, mkcl_object p);
  extern MKCL_API void mkcl_shadow(MKCL, mkcl_object s, mkcl_object p);
  extern MKCL_API void mkcl_use_package(MKCL, mkcl_object x0, mkcl_object p);
  extern MKCL_API void mkcl_unuse_package(MKCL, mkcl_object x0, mkcl_object p);

  extern MKCL_API mkcl_object mk_si_packages_in_waiting(MKCL);


  /* pathname.c */

  extern MKCL_API mkcl_object mk_cl_pathname(MKCL, mkcl_object name);
  extern MKCL_API mkcl_object mk_cl_logical_pathname(MKCL, mkcl_object pname);
  extern MKCL_API mkcl_object mk_cl_pathnamep(MKCL, mkcl_object pname);
  extern MKCL_API mkcl_object mk_cl_pathname_host(MKCL, mkcl_narg narg, mkcl_object pname, ...);
  extern MKCL_API mkcl_object mk_cl_pathname_device(MKCL, mkcl_narg narg, mkcl_object pname, ...);
  extern MKCL_API mkcl_object mk_cl_pathname_directory(MKCL, mkcl_narg narg, mkcl_object pname, ...);
  extern MKCL_API mkcl_object mk_cl_pathname_name(MKCL, mkcl_narg narg, mkcl_object pname, ...);
  extern MKCL_API mkcl_object mk_cl_pathname_type(MKCL, mkcl_narg narg, mkcl_object pname, ...);
  extern MKCL_API mkcl_object mk_cl_pathname_version(MKCL, mkcl_object pname);
  extern MKCL_API mkcl_object mk_cl_namestring(MKCL, mkcl_object pname);
  extern MKCL_API mkcl_object mk_cl_file_namestring(MKCL, mkcl_object pname);
  extern MKCL_API mkcl_object mk_cl_directory_namestring(MKCL, mkcl_object pname);
  extern MKCL_API mkcl_object mk_cl_host_namestring(MKCL, mkcl_object pname);
  extern MKCL_API mkcl_object mk_mkcl_logical_pathname_p(MKCL, mkcl_object pname);
  extern MKCL_API mkcl_object mk_cl_pathname_match_p(MKCL, mkcl_object path, mkcl_object mask);
  extern MKCL_API mkcl_object mk_cl_translate_pathname(MKCL, mkcl_narg narg, mkcl_object source, mkcl_object from, mkcl_object to, ...);
  extern MKCL_API mkcl_object mk_cl_translate_logical_pathname(MKCL, mkcl_narg narg, mkcl_object source, ...);
  extern MKCL_API mkcl_object mk_cl_parse_namestring(MKCL, mkcl_narg narg, mkcl_object thing, ...);
  extern MKCL_API mkcl_object mk_cl_parse_logical_namestring(MKCL, mkcl_narg narg, mkcl_object thing, ...);
  extern MKCL_API mkcl_object mk_cl_merge_pathnames(MKCL, mkcl_narg narg, mkcl_object path, ...);
  extern MKCL_API mkcl_object mk_cl_make_pathname(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_enough_namestring(MKCL, mkcl_narg narg, mkcl_object path, ...);
  extern MKCL_API mkcl_object mk_si_pathname_translations(MKCL, mkcl_narg narg, mkcl_object host, ...);
  extern MKCL_API mkcl_object mk_si_all_logical_pathname_translations(MKCL);
  extern MKCL_API mkcl_object mk_si_default_pathname_defaults(MKCL);
  extern MKCL_API mkcl_object mk_cl_wild_pathname_p(MKCL, mkcl_narg narg, mkcl_object pathname, ...);

  extern MKCL_API mkcl_object mkcl_make_pathname(MKCL, mkcl_object host, mkcl_object device, mkcl_object directory, mkcl_object name, mkcl_object type, mkcl_object version);
  enum mkcl_namestring_specificity { mkcl_may_be_wild_namestring, mkcl_specific_namestring };
  extern MKCL_API mkcl_object mkcl_parse_namestring(MKCL, mkcl_object s, mkcl_index start, mkcl_index end, mkcl_index *ep, mkcl_object default_host, enum mkcl_namestring_specificity specificity);
  extern MKCL_API mkcl_object mkcl_coerce_to_physical_pathname(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_coerce_to_file_pathname(MKCL, mkcl_object pathname);
  extern MKCL_API mkcl_object mkcl_namestring(MKCL, mkcl_object pname, int truncate_if_impossible);
  extern MKCL_API mkcl_object mk_si_coerce_to_filename(MKCL, mkcl_object pathname);
  extern MKCL_API mkcl_object mkcl_merge_pathnames(MKCL, mkcl_object path, mkcl_object defaults, mkcl_object default_version);
  extern MKCL_API mkcl_object mkcl_meld_pathnames(MKCL, mkcl_object path, mkcl_object defaults, mkcl_object default_version);
  extern MKCL_API bool mkcl_logical_hostname_p(MKCL, mkcl_object host);

  extern MKCL_API mkcl_object mk_mkcl_pathname_complete_p(MKCL, mkcl_object pathname);
  extern MKCL_API mkcl_object mk_mkcl_meld_pathnames(MKCL, mkcl_narg narg, mkcl_object path, ...);


  /* predicate.c */

  extern MKCL_API mkcl_object mk_cl_identity(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_null(MKCL, mkcl_object x);
#define mk_cl_not mk_cl_null
  extern MKCL_API mkcl_object mk_cl_symbolp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_atom(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_consp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_listp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_numberp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_integerp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_rationalp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_floatp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_realp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_complexp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_characterp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_stringp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_bit_vector_p(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_vectorp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_simple_string_p(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_simple_bit_vector_p(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_simple_vector_p(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_arrayp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_packagep(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_functionp(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_compiled_function_p(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_bytecodep(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_eq(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_eql(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_equal(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_equalp(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_mkcl_fixnump(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_simple_base_string_p(MKCL, mkcl_object x);

  extern MKCL_API mkcl_object mk_si_unbound_value_p(MKCL, mkcl_object arg);

  extern MKCL_API bool mkcl_eql_unboxable_numbers(MKCL, mkcl_object x, mkcl_object y, mkcl_type t);
  static inline bool mkcl_eql(MKCL, mkcl_object x, mkcl_object y)
  {
    mkcl_type t;

    if (x == y)
      return(TRUE);
    else if (MKCL_IMMEDIATE(x) || MKCL_IMMEDIATE(y) || mkcl_Null(x) ||  mkcl_Null(y))
      return(FALSE);
    else if ((t = x->d.t) != y->d.t)
      return(FALSE);
    else if (t > mkcl_t_reserved_bin0_7) /* last type tag for numbers */
      return(FALSE);
    else
      return(mkcl_eql_unboxable_numbers(env, x, y, t));
  }

  extern MKCL_API bool mkcl_equal(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API bool mkcl_equalp(MKCL, mkcl_object x, mkcl_object y);

  static inline bool mkcl_rationalp(MKCL, mkcl_object x)
  {
    const mkcl_type t = mkcl_type_of(x);
    return (t == mkcl_t_fixnum || t == mkcl_t_bignum || t == mkcl_t_ratio);
  }

  static inline bool mkcl_realp(MKCL, mkcl_object x)
  {
    const mkcl_type t = mkcl_type_of(x);
    return MKCL_REAL_TYPE_P(t);
  }

  static inline bool mkcl_floatp(MKCL, mkcl_object x)
  {
    const mkcl_type t = mkcl_type_of(x);
    return ((t == mkcl_t_singlefloat) || (t == mkcl_t_doublefloat)
#ifdef MKCL_LONG_FLOAT
	    || (t == mkcl_t_longfloat)
#endif
	    );
  }

  static inline bool mkcl_numberp(MKCL, mkcl_object x)
  {
    const mkcl_type t = mkcl_type_of(x);
    return MKCL_NUMBER_TYPE_P(t);
  }

  static inline bool mkcl_stringp(MKCL, mkcl_object x)
  {
    const mkcl_type t = mkcl_type_of(x);
    return (t == mkcl_t_base_string || t == mkcl_t_string
	    || (t == mkcl_t_vector
		&& (x->vector.elttype == mkcl_aet_bc || x->vector.elttype == mkcl_aet_ch || x->vector.elttype == mkcl_aet_nil)));
  }

  static inline bool mkcl_simple_base_string_p(MKCL, mkcl_object x)
  {
    const mkcl_type t = mkcl_type_of(x);
    return ((t == mkcl_t_base_string 
	     || (t == mkcl_t_vector && (x->vector.elttype == mkcl_aet_bc || x->vector.elttype == mkcl_aet_nil)))
	    &&
	    !x->base_string.adjustable &&
	    !x->base_string.hasfillp &&
	    mkcl_Null(mk_cl_car(env, x->base_string.displaced)));
  }

  static inline bool mkcl_simple_string_p(MKCL, mkcl_object x)
  {
    const mkcl_type t = mkcl_type_of(x);
    return ((t == mkcl_t_base_string || (t == mkcl_t_string)
	     || (t == mkcl_t_vector && (x->vector.elttype == mkcl_aet_bc
					|| x->vector.elttype == mkcl_aet_ch
					|| x->vector.elttype == mkcl_aet_nil)))
	    &&
	    !x->string.adjustable &&
	    !x->string.hasfillp &&
	    mkcl_Null(mk_cl_car(env, x->string.displaced)));
  }

  static inline bool mkcl_simple_bit_vector_p(MKCL, mkcl_object x)
  {
    return (mkcl_type_of(x) == mkcl_t_bitvector
	    && !x->vector.adjustable && !x->vector.hasfillp
	    && mkcl_Null(mk_cl_car(env, x->vector.displaced)));
  }

  static inline bool mkcl_simple_vector_p(MKCL, mkcl_object x)
  {
    return (mkcl_type_of(x) == mkcl_t_vector
	    && !x->vector.adjustable
	    && !x->vector.hasfillp
	    && mkcl_Null(mk_cl_car(env, x->vector.displaced))
	    && x->vector.elttype == mkcl_aet_object);
  }

  static inline bool mkcl_functionp(MKCL, mkcl_object x)
  {
    const mkcl_type t = mkcl_type_of(x);
    return (t == mkcl_t_bytecode || t == mkcl_t_bclosure || t == mkcl_t_cfun
	    || t == mkcl_t_cclosure || (t == mkcl_t_instance && x->instance.isgf));
  }

  static inline bool mkcl_compiled_function_p(MKCL, mkcl_object x)
  {
    const mkcl_type t = mkcl_type_of(x);
    return (t == mkcl_t_bytecode || t == mkcl_t_bclosure || t == mkcl_t_cfun || t == mkcl_t_cclosure);
  }

  static inline bool mkcl_bytecodep(MKCL, mkcl_object x)
  {
    const mkcl_type t = mkcl_type_of(x);
    return (t == mkcl_t_bytecode || t == mkcl_t_bclosure);
  }


  /* print.c */

  extern MKCL_API mkcl_object mk_cl_write_byte(MKCL, mkcl_object integer, mkcl_object binary_output_stream);
  extern MKCL_API mkcl_object mk_cl_write_sequence(MKCL, mkcl_narg narg, mkcl_object seq, mkcl_object stream, ...);
  extern MKCL_API mkcl_object mk_cl_write(MKCL, mkcl_narg narg, mkcl_object x, ...);
  extern MKCL_API mkcl_object mk_cl_prin1(MKCL, mkcl_narg narg, mkcl_object obj, ...);
  extern MKCL_API mkcl_object mk_cl_print(MKCL, mkcl_narg narg, mkcl_object obj, ...);
  extern MKCL_API mkcl_object mk_cl_pprint(MKCL, mkcl_narg narg, mkcl_object obj, ...);
  extern MKCL_API mkcl_object mk_cl_princ(MKCL, mkcl_narg narg, mkcl_object obj, ...);
  extern MKCL_API mkcl_object mk_cl_write_char(MKCL, mkcl_narg narg, mkcl_object c, ...);
  extern MKCL_API mkcl_object mk_cl_write_string(MKCL, mkcl_narg narg, mkcl_object strng, ...);
  extern MKCL_API mkcl_object mk_cl_write_line(MKCL, mkcl_narg narg, mkcl_object strng, ...);
  extern MKCL_API mkcl_object mk_cl_terpri(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_finish_output(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_fresh_line(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_force_output(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_clear_output(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_si_write_object(MKCL, mkcl_object object, mkcl_object stream);
  extern MKCL_API mkcl_object mk_si_write_ugly_object(MKCL, mkcl_object object, mkcl_object stream);

  extern MKCL_API mkcl_object mkcl_princ(MKCL, mkcl_object obj, mkcl_object strm);
  extern MKCL_API mkcl_object mkcl_prin1(MKCL, mkcl_object obj, mkcl_object strm);
  extern MKCL_API mkcl_object mkcl_print(MKCL, mkcl_object obj, mkcl_object strm);
  extern MKCL_API mkcl_object mkcl_println(MKCL, mkcl_object obj, mkcl_object strm);
  extern MKCL_API mkcl_object mkcl_println_T(MKCL, mkcl_object obj);
  extern MKCL_API mkcl_object mkcl_prin1ln_T(MKCL, mkcl_object obj);
  extern MKCL_API mkcl_object mkcl_terpri(MKCL, mkcl_object strm);
  extern MKCL_API void mkcl_write_string(MKCL, mkcl_object strng, mkcl_object strm);
  extern MKCL_API void mkcl_princ_str(MKCL, const char *s, mkcl_object sym);
  extern MKCL_API void mkcl_princ_char(MKCL, int c, mkcl_object sym);


  /* read.c */

  extern MKCL_API mkcl_object mk_si_get_buffer_string(MKCL);
  extern MKCL_API mkcl_object mk_si_put_buffer_string(MKCL, mkcl_object string);
  extern MKCL_API mkcl_object mk_cl_read_sequence(MKCL, mkcl_narg narg, mkcl_object seq, mkcl_object stream, ...);
  extern MKCL_API mkcl_object mk_cl_readtablep(MKCL, mkcl_object readtable);
  extern MKCL_API mkcl_object mk_si_fast_read_from_base_string(MKCL, mkcl_object str);
  extern MKCL_API mkcl_object mk_si_standard_readtable(MKCL);
  extern MKCL_API mkcl_object mk_cl_read(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_read_preserving_whitespace(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_read_delimited_list(MKCL, mkcl_narg narg, mkcl_object d, ...);
  extern MKCL_API mkcl_object mk_cl_read_line(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_read_char(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_unread_char(MKCL, mkcl_narg narg, mkcl_object c, ...);
  extern MKCL_API mkcl_object mk_cl_peek_char(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_listen(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_read_char_no_hang(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_clear_input(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_parse_integer(MKCL, mkcl_narg narg, mkcl_object strng, ...);
  extern MKCL_API mkcl_object mk_cl_read_byte(MKCL, mkcl_narg narg, mkcl_object binary_input_stream, ...);
  extern MKCL_API mkcl_object mk_cl_copy_readtable(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_readtable_case(MKCL, mkcl_object r);
  extern MKCL_API mkcl_object mk_si_readtable_case_set(MKCL, mkcl_object r, mkcl_object mode);
  extern MKCL_API mkcl_object mk_cl_set_syntax_from_char(MKCL, mkcl_narg narg, mkcl_object tochr, mkcl_object fromchr, ...);
  extern MKCL_API mkcl_object mk_cl_set_macro_character(MKCL, mkcl_narg narg, mkcl_object chr, mkcl_object fnc, ...);
  extern MKCL_API mkcl_object mk_cl_get_macro_character(MKCL, mkcl_narg narg, mkcl_object chr, ...);
  extern MKCL_API mkcl_object mk_cl_make_dispatch_macro_character(MKCL, mkcl_narg narg, mkcl_object chr, ...);
  extern MKCL_API mkcl_object mk_cl_set_dispatch_macro_character(MKCL, mkcl_narg narg, mkcl_object dspchr, mkcl_object subchr, mkcl_object fnc, ...);
  extern MKCL_API mkcl_object mk_cl_get_dispatch_macro_character(MKCL, mkcl_narg narg, mkcl_object dspchr, mkcl_object subchr, ...);
  extern MKCL_API mkcl_object mk_si_read_object_or_ignore(MKCL, mkcl_object stream, mkcl_object eof);

  extern MKCL_API enum mkcl_chattrib mkcl_readtable_get(MKCL, mkcl_object rdtbl, mkcl_character c, mkcl_object *macro);
  extern MKCL_API void mkcl_readtable_set(MKCL, mkcl_object rdtbl, mkcl_character c, enum mkcl_chattrib cat, mkcl_object macro_or_table);
  extern MKCL_API mkcl_object mkcl_read_object_non_recursive(MKCL, mkcl_object in);
  extern MKCL_API mkcl_object mkcl_read_object(MKCL, mkcl_object in);
  extern MKCL_API mkcl_object mkcl_parse_number(MKCL, mkcl_object s, mkcl_index start, mkcl_index end, mkcl_index *ep, unsigned int radix);
  extern MKCL_API mkcl_object mkcl_parse_integer(MKCL, mkcl_object s, mkcl_index start, mkcl_index end, mkcl_index *ep, unsigned int radix);
  extern MKCL_API bool mkcl_invalid_constituent_character_p(mkcl_character c);
  extern MKCL_API mkcl_object mkcl_copy_readtable(MKCL, mkcl_object from, mkcl_object to);
  extern MKCL_API mkcl_object mkcl_current_readtable(MKCL);
  extern MKCL_API int mkcl_current_read_base(MKCL);
  extern MKCL_API char mkcl_current_read_default_float_format(MKCL);
  extern MKCL_API mkcl_object mkcl_fast_read_from_cstring(MKCL, char *s);
  extern MKCL_API mkcl_object mkcl_read_VV(MKCL, mkcl_object block, void (*entry)(MKCL, mkcl_object, mkcl_object), mkcl_object filename);

  /* reference.c */

  extern MKCL_API mkcl_object mk_cl_fboundp(MKCL, mkcl_object sym);
  extern MKCL_API mkcl_object mk_cl_symbol_function(MKCL, mkcl_object sym);
  extern MKCL_API mkcl_object mk_cl_fdefinition(MKCL, mkcl_object fname);
  extern MKCL_API mkcl_object mk_si_coerce_to_function(MKCL, mkcl_object form);
  extern MKCL_API mkcl_object mk_cl_symbol_value(MKCL, mkcl_object sym);
  extern MKCL_API mkcl_object mk_cl_boundp(MKCL, mkcl_object sym);
  extern MKCL_API mkcl_object mk_cl_special_operator_p(MKCL, mkcl_object form);
  extern MKCL_API mkcl_object mkcl_fdefinition(MKCL, mkcl_object fname);

  /* sequence.c */

  extern MKCL_API mkcl_object mk_cl_elt(MKCL, mkcl_object x, mkcl_object i);
  extern MKCL_API mkcl_object mk_si_elt_set(MKCL, mkcl_object seq, mkcl_object index, mkcl_object val);
  extern MKCL_API mkcl_object mk_cl_copy_seq(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_length(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_reverse(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_nreverse(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_subseq(MKCL, mkcl_narg narg, mkcl_object sequence, mkcl_object start, ...);

  extern MKCL_API mkcl_object mkcl_elt(MKCL, mkcl_object seq, mkcl_word index);
  extern MKCL_API mkcl_object mkcl_elt_set(MKCL, mkcl_object seq, mkcl_word index, mkcl_object val);
  extern MKCL_API mkcl_index mkcl_length(MKCL, mkcl_object x);
  extern MKCL_API mkcl_index mkcl_string_length(MKCL, mkcl_object x);
  extern MKCL_API mkcl_index mkcl_base_string_length(MKCL, mkcl_object x);


  /* string.c */

  extern MKCL_API mkcl_object mk_cl_char(MKCL, mkcl_object s, mkcl_object i);
#define mk_cl_schar mk_cl_char
  extern MKCL_API mkcl_object mk_si_char_set(MKCL, mkcl_object str, mkcl_object index, mkcl_object c);
#define mk_si_schar_set mk_si_char_set
  extern MKCL_API mkcl_object mk_cl_string_trim(MKCL, mkcl_object char_bag, mkcl_object strng);
  extern MKCL_API mkcl_object mk_cl_string_left_trim(MKCL, mkcl_object char_bag, mkcl_object strng);
  extern MKCL_API mkcl_object mk_cl_string_right_trim(MKCL, mkcl_object char_bag, mkcl_object strng);
  extern MKCL_API mkcl_object mk_cl_string(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_cl_make_string(MKCL, mkcl_narg narg, mkcl_object size, ...);
  extern MKCL_API mkcl_object mk_cl_stringE(MKCL, mkcl_narg narg, mkcl_object string1, mkcl_object string2, ...);
  extern MKCL_API mkcl_object mk_cl_string_equal(MKCL, mkcl_narg narg, mkcl_object string1, mkcl_object string2, ...);
  extern MKCL_API mkcl_object mk_cl_stringL(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_stringG(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_stringLE(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_stringGE(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_stringNE(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_string_lessp(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_string_greaterp(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_string_not_greaterp(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_string_not_lessp(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_string_not_equal(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_string_upcase(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_string_downcase(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_string_capitalize(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_nstring_upcase(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_nstring_downcase(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_nstring_capitalize(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_si_concatenate_base_strings(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_si_concatenate_strings(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_si_copy_to_simple_base_string(MKCL, mkcl_object s);
  extern MKCL_API mkcl_object mkcl_coerce_to_adjustable_base_string(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_copy_to_simple_string(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_coerce_to_adjustable_string(MKCL, mkcl_object x);

  extern MKCL_API mkcl_object mkcl_concatenate_2_base_strings(MKCL, mkcl_object str1, mkcl_object str2);
  extern MKCL_API mkcl_object mkcl_concatenate_3_base_strings(MKCL, mkcl_object str1, mkcl_object str2, mkcl_object str3);
  extern MKCL_API mkcl_object mkcl_concatenate_2_strings(MKCL, mkcl_object str1, mkcl_object str2);
  extern MKCL_API mkcl_object mkcl_concatenate_3_strings(MKCL, mkcl_object str1, mkcl_object str2, mkcl_object str3);
  extern MKCL_API mkcl_object mkcl_alloc_simple_base_string(MKCL, mkcl_index l);
  extern MKCL_API mkcl_object mkcl_alloc_adjustable_base_string(MKCL, mkcl_index l);
  extern MKCL_API mkcl_object mkcl_make_simple_base_string(MKCL, char *s);

  extern MKCL_API mkcl_object mkcl_copy_string(MKCL, mkcl_object s);
  extern MKCL_API mkcl_object mkcl_copy_base_string(MKCL, mkcl_object s);
  extern MKCL_API mkcl_object mkcl_make_base_string_copy(MKCL, const char *s);
  extern MKCL_API mkcl_object mkcl_cstring_to_base_string(MKCL, const char *s);
  extern MKCL_API bool mkcl_string_E(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API bool mkcl_member_char(MKCL, mkcl_character c, mkcl_object char_bag);
  extern MKCL_API void mkcl_get_string_start_end(MKCL, mkcl_object s, mkcl_object start, mkcl_object end, mkcl_index *ps, mkcl_index *pe);
  extern MKCL_API bool mkcl_fits_in_base_string(MKCL, mkcl_object s);
  extern MKCL_API mkcl_character mkcl_char(MKCL, mkcl_object s, mkcl_index i);
  extern MKCL_API mkcl_character mkcl_char_set(MKCL, mkcl_object s, mkcl_index i, mkcl_character c);

  extern MKCL_API mkcl_object mkcl_fill_base_string(MKCL, mkcl_object str, mkcl_base_char ch);
  extern MKCL_API mkcl_object mkcl_fill_base_string_k(MKCL, mkcl_object str, mkcl_base_char ch, mkcl_index start, mkcl_index end);
  extern MKCL_API mkcl_object mkcl_fill_string(MKCL, mkcl_object str, mkcl_character ch);
  extern MKCL_API mkcl_object mkcl_fill_string_k(MKCL, mkcl_object str, mkcl_character ch, mkcl_index start, mkcl_index end);

  extern MKCL_API mkcl_object mkcl_replace_in_base_string(MKCL, mkcl_object str1, mkcl_object str2);
  extern MKCL_API mkcl_object mkcl_replace_in_base_string_k(MKCL, mkcl_object str1, mkcl_object str2,
							    mkcl_index start1, mkcl_index end1,
							    mkcl_index start2, mkcl_index end2);
  extern MKCL_API mkcl_object mkcl_replace_in_string(MKCL, mkcl_object str1, mkcl_object str2);
  extern MKCL_API mkcl_object mkcl_replace_in_string_k(MKCL, mkcl_object str1, mkcl_object str2,
						       mkcl_index start1, mkcl_index end1,
						       mkcl_index start2, mkcl_index end2);
  
  extern MKCL_API mkcl_object mkcl_search_in_base_string(MKCL, mkcl_object str1, mkcl_object str2);
  extern MKCL_API mkcl_object mkcl_search_in_string(MKCL, mkcl_object str1, mkcl_object str2);
  extern MKCL_API mkcl_base_char mkcl_base_string_last(MKCL, mkcl_object str);
  extern MKCL_API mkcl_character mkcl_string_last(MKCL, mkcl_object x);

  extern MKCL_API mkcl_object mkcl_fill_base_string_from_string(MKCL, mkcl_object bstr, mkcl_index prefix_size, mkcl_object str);

  extern MKCL_API void mkcl_fill_utf_8_from_string(MKCL, mkcl_object utf_8, mkcl_index prefix_size, mkcl_object str);
  extern MKCL_API void mkcl_fill_utf_8_from_base_string(MKCL, mkcl_object utf_8, mkcl_index prefix_size, mkcl_object str);
  extern MKCL_API mkcl_object mkcl_utf_8_to_string(MKCL, mkcl_object utf_8);
  extern MKCL_API mkcl_object mkcl_utf_8_to_base_string(MKCL, mkcl_object utf_8);
  extern MKCL_API mkcl_character mkcl_utf_8_char(MKCL, mkcl_object utf_8, mkcl_index index, mkcl_index * next, bool * invalid);
  extern MKCL_API mkcl_object mk_si_utf_8(MKCL, mkcl_object string);
  extern MKCL_API mkcl_object mk_si_utf_8_p(MKCL, mkcl_object utf_8);
  extern MKCL_API mkcl_object mk_si_utf_8_length(MKCL, mkcl_object utf_8);
  extern MKCL_API mkcl_object mk_si_utf_8_as_is(MKCL, mkcl_object string);
  extern MKCL_API mkcl_object mk_si_utf_8_char(MKCL, mkcl_object utf_8, mkcl_object index_fix);
  extern MKCL_API mkcl_object mk_si_utf_8Plus(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_si_utf_8E(MKCL, mkcl_object u1, mkcl_object u2);
  extern MKCL_API mkcl_char8 * mkcl_extend_utf_8(MKCL, mkcl_object utf_8);
  extern MKCL_API mkcl_index mkcl_utf_8_push_extend(MKCL, mkcl_object utf_8, mkcl_character ch, bool * invalid);
  extern MKCL_API mkcl_object mk_si_utf_8_push_extend(MKCL, mkcl_object utf_8, mkcl_object ch);
  extern MKCL_API mkcl_object mkcl_utf_8_nconc(MKCL, mkcl_object base, mkcl_object new);
  extern MKCL_API mkcl_character mkcl_utf_8_last(MKCL, mkcl_object utf_8, bool * invalid);
  extern MKCL_API mkcl_object mk_si_utf_8_last(MKCL, mkcl_object utf_8);
  extern MKCL_API mkcl_object mkcl_cstring_to_utf_8(MKCL, const char * str);
  extern MKCL_API mkcl_object mkcl_cstring_copy_to_utf_8(MKCL, const char * str);


  extern MKCL_API void mkcl_fill_utf_16_from_string(MKCL, mkcl_object utf_16, mkcl_index prefix_size, mkcl_object str);
  extern MKCL_API void mkcl_fill_utf_16_from_base_string(MKCL, mkcl_object utf_16, mkcl_index prefix_size, mkcl_object str);
  extern MKCL_API mkcl_object mkcl_utf_16_to_string(MKCL, mkcl_object utf_16);
  extern MKCL_API mkcl_object mkcl_utf_16_to_base_string(MKCL, mkcl_object utf_16);
  extern MKCL_API mkcl_character mkcl_utf_16_char(MKCL, mkcl_object utf_16, mkcl_index index, mkcl_index * next, bool * invalid);
  extern MKCL_API mkcl_object mk_si_utf_16(MKCL, mkcl_object string);
  extern MKCL_API mkcl_object mk_si_utf_16_p(MKCL, mkcl_object utf_16);
  extern MKCL_API mkcl_object mk_si_utf_16_length(MKCL, mkcl_object utf_16);
  extern MKCL_API mkcl_object mk_si_utf_16_as_is(MKCL, mkcl_object string);
  extern MKCL_API mkcl_object mk_si_utf_16_char(MKCL, mkcl_object utf_16, mkcl_object index_fix);
  extern MKCL_API mkcl_object mk_si_utf_16Plus(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_si_utf_16E(MKCL, mkcl_object u1, mkcl_object u2);
  extern MKCL_API mkcl_char16 * mkcl_extend_utf_16(MKCL, mkcl_object utf_16);
  extern MKCL_API mkcl_index mkcl_utf_16_push_extend(MKCL, mkcl_object utf_16, mkcl_character ch, bool * invalid);
  extern MKCL_API mkcl_object mk_si_utf_16_push_extend(MKCL, mkcl_object utf_16, mkcl_object ch);
  extern MKCL_API mkcl_object mkcl_utf_16_nconc(MKCL, mkcl_object base, mkcl_object new);
  extern MKCL_API mkcl_character mkcl_utf_16_last(MKCL, mkcl_object utf_16, bool * invalid);
  extern MKCL_API mkcl_object mk_si_utf_16_last(MKCL, mkcl_object utf_16);
  extern MKCL_API mkcl_object mkcl_cstring_to_utf_16(MKCL, const char * str);
  extern MKCL_API mkcl_object mkcl_cstring16_to_utf_16(MKCL, const mkcl_char16 * str);
#define mkcl_cstring16_to_OSstring mkcl_cstring16_to_utf_16
  extern MKCL_API mkcl_object mkcl_cstring16_copy_to_utf_16(MKCL, const mkcl_char16 * str);
#define mkcl_cstring16_copy_to_OSstring mkcl_cstring16_copy_to_utf_16

  extern MKCL_API bool mkcl_os_string_format_is_utf_8_p(MKCL);
  extern MKCL_API mkcl_object mkcl_cstring_to_string(MKCL, char * str);
  extern MKCL_API mkcl_object mkcl_cstring16_to_string(MKCL, mkcl_char16 * str);

#if MKCL_WINDOWS
#define mkcl_alloc_OSstring mkcl_alloc_utf_16
#define mkcl_string_to_OSstring mk_si_utf_16
#define mkcl_cstring_to_OSstring mkcl_cstring_to_utf_16
#define mkcl_cstring_copy_to_OSstring mkcl_cstring_to_utf_16
#define mkcl_OSstring_nconc mkcl_utf_16_nconc
#define mkcl_OSstring_push_extend(env, str, ch) mkcl_utf_16_push_extend(env, str, ch, NULL)
#define mkcl_OSstring_self(str) (str->UTF_16.self)
#define mkcl_OSstring_size(str) (str->UTF_16.fillp)
#define mkcl_OSstring_set_fillp(str, new_fillp) (str->UTF_16.fillp = new_fillp)
#define mkcl_OSstring_to_string(env, str) mkcl_utf_16_to_string(env, str)
  static inline mkcl_character mkcl_OSstring_last(MKCL, mkcl_object str)
  {
    bool invalid = FALSE;
    
    return mkcl_utf_16_last(env, str, &invalid);
  }
#define mkcl_rawOSstring_to_string(env, str) mkcl_cstring16_to_string(env, str)

  typedef mkcl_char16 * mkcl_OSstring_raw_type;
#define MKCL_OSSTRINGP(str) MKCL_UTF_16_P(str)
#else
  extern MKCL_API mkcl_object mkcl_alloc_OSstring(MKCL, mkcl_index size);
  extern MKCL_API mkcl_object mkcl_string_to_OSstring(MKCL, mkcl_object string);
  extern MKCL_API mkcl_object mkcl_cstring_to_OSstring(MKCL, char * str);
  extern MKCL_API mkcl_object mkcl_cstring_copy_to_OSstring(MKCL, char * str);
  extern MKCL_API mkcl_index mkcl_OSstring_push_extend(MKCL, mkcl_object str, mkcl_character ch);
  extern MKCL_API mkcl_object mkcl_OSstring_nconc(MKCL, mkcl_object base, mkcl_object new);
  extern MKCL_API mkcl_object mkcl_OSstring_nconc_cstring(MKCL, mkcl_object base, char * new);
  static inline mkcl_char8 * mkcl_OSstring_self(mkcl_object str)
  { return (MKCL_UTF_8_P(str) ? str->UTF_8.self : str->base_string.self); }
  static inline mkcl_index mkcl_OSstring_size(mkcl_object str)
  { return (MKCL_UTF_8_P(str) ? str->UTF_8.fillp : str->base_string.fillp); }
  static inline mkcl_index mkcl_OSstring_set_fillp(mkcl_object str, mkcl_index fillp)
  { return (MKCL_UTF_8_P(str) ? (str->UTF_8.fillp = fillp) : (str->base_string.fillp = fillp)); }
  static inline mkcl_object mkcl_OSstring_to_string(MKCL, mkcl_object str)
  { return (MKCL_UTF_8_P(str) ? mkcl_utf_8_to_string(env, str) : str); }
  static inline mkcl_character mkcl_OSstring_last(MKCL, mkcl_object str)
  {
    bool invalid = FALSE;
    
    return (MKCL_UTF_8_P(str) ? mkcl_utf_8_last(env, str, &invalid) : mkcl_base_string_last(env, str));
  }
#define mkcl_rawOSstring_to_string(env, str) mkcl_cstring_to_string(env, ((char *) str))

  typedef mkcl_char8 * mkcl_OSstring_raw_type;
#define MKCL_OSSTRINGP(str) (MKCL_UTF_8_P(str) || MKCL_BASE_STRING_P(str))
#endif

  extern MKCL_API mkcl_object mk_mkcl_octets(MKCL, mkcl_object obj);
  extern MKCL_API mkcl_object mk_mkcl_double_octets(MKCL, mkcl_object obj);

  /* structure.c */

  extern MKCL_API mkcl_object mk_si_structure_subtype_p(MKCL, mkcl_object x, mkcl_object y);
  extern MKCL_API mkcl_object mk_cl_copy_structure(MKCL, mkcl_object s);
  extern MKCL_API mkcl_object mk_si_structure_name(MKCL, mkcl_object s);
  extern MKCL_API mkcl_object mk_si_structure_length(MKCL, mkcl_object s);
  extern MKCL_API mkcl_object mk_si_structure_ref(MKCL, mkcl_object x, mkcl_object name, mkcl_object index);
  extern MKCL_API mkcl_object mk_si_structure_set(MKCL, mkcl_object x, mkcl_object name, mkcl_object index, mkcl_object val);
  extern MKCL_API mkcl_object mk_si_structurep(MKCL, mkcl_object s);
  extern MKCL_API mkcl_object mk_si_make_structure(MKCL, mkcl_narg narg, mkcl_object type, ...);


  /* symbol.c */

  extern MKCL_API mkcl_object mk_cl_make_symbol(MKCL, mkcl_object str);
  extern MKCL_API mkcl_object mk_cl_remprop(MKCL, mkcl_object sym, mkcl_object prop);
  extern MKCL_API mkcl_object mk_cl_symbol_plist(MKCL, mkcl_object sym);
  extern MKCL_API mkcl_object mk_cl_get_properties(MKCL, mkcl_object place, mkcl_object indicator_list);
  extern MKCL_API mkcl_object mk_cl_symbol_name(MKCL, mkcl_object sym);
  extern MKCL_API mkcl_object mk_cl_symbol_package(MKCL, mkcl_object sym);
  extern MKCL_API mkcl_object mk_cl_keywordp(MKCL, mkcl_object sym);
  extern MKCL_API mkcl_object mk_si_put_f(MKCL, mkcl_object plist, mkcl_object value, mkcl_object indicator);
  extern MKCL_API mkcl_object mk_si_rem_f(MKCL, mkcl_object plist, mkcl_object indicator);
  extern MKCL_API mkcl_object mk_si_set_symbol_plist(MKCL, mkcl_object sym, mkcl_object plist);
  extern MKCL_API mkcl_object mk_si_putprop(MKCL, mkcl_object sym, mkcl_object value, mkcl_object indicator);
  extern MKCL_API mkcl_object mk_si_Xmake_special(MKCL, mkcl_object sym);
  extern MKCL_API mkcl_object mk_si_Xmake_constant(MKCL, mkcl_object sym, mkcl_object val);
  extern MKCL_API mkcl_object mk_cl_get(MKCL, mkcl_narg narg, mkcl_object sym, mkcl_object indicator, ...);
  extern MKCL_API mkcl_object mk_cl_getf(MKCL, mkcl_narg narg, mkcl_object place, mkcl_object indicator, ...);
  extern MKCL_API mkcl_object mk_cl_copy_symbol(MKCL, mkcl_narg narg, mkcl_object sym, ...);
  extern MKCL_API mkcl_object mk_cl_gensym(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_gentemp(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_si_put_properties(MKCL, mkcl_narg narg, mkcl_object sym, ...);

  extern MKCL_API mkcl_object mkcl_make_keyword(MKCL, const char *s);
  extern MKCL_API mkcl_object mkcl_symbol_name(MKCL, mkcl_object s);
  extern MKCL_API mkcl_object mkcl_symbol_package(MKCL, mkcl_object s);
  extern MKCL_API int mkcl_symbol_type(MKCL, mkcl_object s);
  extern MKCL_API void mkcl_symbol_type_set(MKCL, mkcl_object s, int t);
  extern MKCL_API mkcl_object mkcl_getf(MKCL, mkcl_object place, mkcl_object indicator, mkcl_object deflt);
  extern MKCL_API mkcl_object mkcl_get(MKCL, mkcl_object s, mkcl_object p, mkcl_object d);
  extern MKCL_API bool mkcl_keywordp(mkcl_object s);


  /* threads.c */

  extern MKCL_API mkcl_object mk_mt_all_threads(MKCL);
  extern MKCL_API mkcl_object mk_mt_abandon_thread(MKCL, mkcl_object result_value) mkcl_noreturn;
  extern MKCL_API mkcl_object mk_mt_exit_thread(MKCL, mkcl_object result_value) mkcl_noreturn;
  extern MKCL_API mkcl_object mk_mt_terminate_thread(MKCL) mkcl_noreturn;
  extern MKCL_API mkcl_object mk_mt_cancel_thread(MKCL) mkcl_noreturn;
  extern MKCL_API mkcl_object mk_mt_abort_thread(MKCL) mkcl_noreturn;
  extern MKCL_API mkcl_object mk_mt_interrupt_thread(MKCL, mkcl_narg narg, mkcl_object thread, mkcl_object function, ...);
  extern MKCL_API mkcl_object mk_mt_make_thread(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_mt_thread_active_p(MKCL, mkcl_object thread);
  extern MKCL_API mkcl_object mk_mt_thread_enable(MKCL, mkcl_object thread);
  extern MKCL_API mkcl_object mk_mt_thread_yield(MKCL);
  extern MKCL_API mkcl_object mk_mt_thread_interrupt(MKCL, mkcl_object thread, mkcl_object function);
  extern MKCL_API mkcl_object mk_mt_thread_kill(MKCL, mkcl_object thread);
  extern MKCL_API mkcl_object mk_mt_thread_detach(MKCL, mkcl_object thread);
  extern MKCL_API mkcl_object mk_mt_thread_join(MKCL, mkcl_object thread);
  extern MKCL_API mkcl_object mk_mt_thread_name(MKCL, mkcl_object thread);
  extern MKCL_API mkcl_object mk_mt_show_sigmask(MKCL);
  extern MKCL_API mkcl_object mk_mt_reset_sigmask(MKCL);
  extern MKCL_API mkcl_object mk_mt_block_signals(MKCL);
  extern MKCL_API mkcl_object mk_mt_unblock_signals(MKCL);
  extern MKCL_API mkcl_object mk_mt_thread_preset(MKCL, mkcl_narg narg, mkcl_object thread, mkcl_object function, ...);
  extern MKCL_API mkcl_object mk_mt_thread_run_function(MKCL, mkcl_narg narg, mkcl_object name, mkcl_object function, ...);
  extern MKCL_API mkcl_object mk_mt_thread_status(MKCL, mkcl_object thread);
  extern MKCL_API mkcl_object mk_mt_make_lock(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_mt_recursive_lock_p(MKCL, mkcl_object lock);
  extern MKCL_API mkcl_object mk_mt_lock_name(MKCL, mkcl_object lock);
  extern MKCL_API mkcl_object mk_mt_lock_holder(MKCL, mkcl_object lock);
  extern MKCL_API mkcl_object mk_mt_get_lock(MKCL, mkcl_narg narg, mkcl_object lock, ...);
  extern MKCL_API mkcl_object mk_mt_giveup_lock(MKCL, mkcl_object lock);
  extern MKCL_API mkcl_object mk_mt_make_rwlock(MKCL);
  extern MKCL_API mkcl_object mk_mt_giveup_rwlock(MKCL, mkcl_narg narg, mkcl_object rwlock, ...);
  extern MKCL_API mkcl_object mk_mt_get_read_rwlock(MKCL, mkcl_narg narg, mkcl_object rwlock, ...);
  extern MKCL_API mkcl_object mk_mt_get_write_rwlock(MKCL, mkcl_narg narg, mkcl_object rwlock, ...);
  extern MKCL_API mkcl_object mk_mt_make_semaphore(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_mt_semaphore_count(MKCL, mkcl_object sem);
  extern MKCL_API mkcl_object mk_mt_semaphore_signal(MKCL, mkcl_narg narg, mkcl_object sem, ...);
  extern MKCL_API mkcl_object mk_mt_semaphore_wait(MKCL, mkcl_narg narg, mkcl_object sem, ...);
  extern MKCL_API mkcl_object mk_mt_make_condition_variable(MKCL);
  extern MKCL_API mkcl_object mk_mt_condition_wait(MKCL, mkcl_narg narg, mkcl_object cv, mkcl_object lock, ...);
  extern MKCL_API mkcl_object mk_mt_condition_signal(MKCL, mkcl_object cv);
  extern MKCL_API mkcl_object mk_mt_condition_broadcast(MKCL, mkcl_object cv);
  extern MKCL_API mkcl_object mk_mt_current_thread(MKCL);
  extern MKCL_API mkcl_object mk_mt_test_for_thread_shutdown(MKCL);
  extern MKCL_API mkcl_object mk_mt_request_thread_shutdown(MKCL, mkcl_object thread);
  extern MKCL_API mkcl_object mk_mt_thread_shutdown_requested_p(MKCL, mkcl_object thread);

  extern MKCL_API mkcl_object mkcl_top_apply(MKCL, mkcl_object function, mkcl_object args);
  extern MKCL_API mkcl_object mkcl_make_thread(MKCL, mkcl_object name, mkcl_object initial_bindings, struct mkcl_thread_init_parameters * params);
  typedef void (* mkcl_thread_import_failure_handler)(void * handler_data, int errno_rc, char * msg);
  extern MKCL_API const mkcl_env mkcl_import_current_thread(mkcl_object thread_name, mkcl_object thread_binding,
							    mkcl_thread_import_failure_handler * handler, void * handler_data);
  extern MKCL_API void mkcl_release_current_thread(MKCL);
  extern MKCL_API void mkcl_setup_thread_lisp_context(MKCL, char * const stack_mark_address);
  extern MKCL_API void mkcl_cleanup_thread_lisp_context(MKCL);
  extern MKCL_API void mkcl_register_thread_as_active(MKCL, mkcl_object thread);
  extern MKCL_API void mkcl_remove_thread_from_global_thread_list(MKCL, mkcl_object thread);

  extern MKCL_API mkcl_object mk_mt_thread_plist(MKCL, mkcl_object thread);
  extern MKCL_API mkcl_object mk_mt_set_thread_plist(MKCL, mkcl_object thread, mkcl_object plist);

  extern MKCL_API void mkcl_thread_exit(MKCL, long status_code) mkcl_noreturn;

#define MKCL_THREAD_CANCELED (-1) /* same as PTHREAD_CANCELED. JCB */
#define MKCL_THREAD_TERMINATED (-2)
#define MKCL_THREAD_ABORTED (-3)
#define MKCL_THREAD_INVALID_VALUE (-4)
#define MKCL_GC_EXIT (-5)
#define MKCL_GC_ABORT (-6)
#define MKCL_THREAD_UNKNOWN_ERROR (-7)
#define MKCL_THREAD_NORMAL_EXIT (0)

  extern MKCL_API mkcl_object mk_si_non_interactive_thread_debugger_trap(MKCL, mkcl_object condition, mkcl_object old_hook);

  /* time.c */

  extern MKCL_API mkcl_object mk_cl_sleep(MKCL, mkcl_object z);
  extern MKCL_API mkcl_object mk_cl_get_internal_run_time(MKCL);
  extern MKCL_API mkcl_object mk_cl_get_internal_real_time(MKCL);
  extern MKCL_API mkcl_object mk_cl_get_universal_time(MKCL);
  extern MKCL_API mkcl_object mk_si_get_local_time_zone(MKCL);

  /* typespec.c */

  extern MKCL_API void mkcl_assert_type_integer(MKCL, mkcl_object p);
  extern MKCL_API void mkcl_assert_type_non_negative_integer(MKCL, mkcl_object p);
  extern MKCL_API void mkcl_assert_type_package(MKCL, mkcl_object p);
  extern MKCL_API void mkcl_assert_type_string(MKCL, mkcl_object p);
  extern MKCL_API void mkcl_assert_type_cons(MKCL, mkcl_object p);
  extern MKCL_API void mkcl_assert_type_readtable(MKCL, mkcl_object p);
  extern MKCL_API void mkcl_assert_type_hash_table(MKCL, mkcl_object p);
  extern MKCL_API void mkcl_assert_type_array(MKCL, mkcl_object p);
  extern MKCL_API void mkcl_assert_type_vector(MKCL, mkcl_object p);
  extern MKCL_API void mkcl_assert_type_list(MKCL, mkcl_object p);
  extern MKCL_API void mkcl_assert_type_proper_list(MKCL, mkcl_object p);
  extern MKCL_API mkcl_object mk_cl_type_of(MKCL, mkcl_object x);

  extern MKCL_API void mkcl_FEtype_error_character(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_base_char(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_cons(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_number(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_real(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_float(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_integer(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_list(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_proper_list(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_alist(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_string(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_string_with_fill_pointer(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_base_string(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_base_string_with_fill_pointer(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_vector(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_vector_with_fill_pointer(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_sequence(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_proper_sequence(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_array(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_stream(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_instance(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEcircular_list(MKCL, mkcl_object x) mkcl_noreturn;
  extern MKCL_API void mkcl_FEtype_error_seq_index(MKCL, mkcl_object seq, mkcl_object ndx) mkcl_noreturn;
  extern MKCL_API void mkcl_FEdivision_by_zero(MKCL, mkcl_object x, mkcl_object y) mkcl_noreturn;
  extern MKCL_API mkcl_object mkcl_type_error(MKCL, mkcl_object function, const char *place, mkcl_object o, mkcl_object type);
  extern MKCL_API mkcl_object mkcl_check_cl_type(MKCL, mkcl_object fun, mkcl_object p, mkcl_type t);
  extern MKCL_API mkcl_object mkcl_check_type_string(MKCL, mkcl_object fun, mkcl_object p);


  /* unixfsys.c */

  extern MKCL_API mkcl_object mk_cl_truename(MKCL, mkcl_object file);
  extern MKCL_API mkcl_object mk_cl_rename_file(MKCL, mkcl_object old_filespec, mkcl_object new_name);
  extern MKCL_API mkcl_object mk_cl_delete_file(MKCL, mkcl_object file);
  extern MKCL_API mkcl_object mk_cl_probe_file(MKCL, mkcl_object file);
  extern MKCL_API mkcl_object mk_cl_file_write_date(MKCL, mkcl_object file);
  extern MKCL_API mkcl_object mk_cl_file_author(MKCL, mkcl_object file);
  extern MKCL_API mkcl_object mk_si_file_kind(MKCL, mkcl_narg narg, mkcl_object filespec, ...);
  extern MKCL_API mkcl_object mk_mkcl_getcwd(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_mkcl_getpid(MKCL);
  extern MKCL_API mkcl_object mk_mkcl_gettid(MKCL);
  extern MKCL_API mkcl_object mk_mkcl_getuid(MKCL);
  extern MKCL_API mkcl_object mk_mkcl_chdir(MKCL, mkcl_narg narg, mkcl_object directory, ...);
  extern MKCL_API mkcl_object mk_mkcl_mkdir(MKCL, mkcl_object directory, mkcl_object mode);
  extern MKCL_API mkcl_object mk_cl_directory(MKCL, mkcl_narg narg, mkcl_object directory, ...);
  extern MKCL_API mkcl_object mk_cl_user_homedir_pathname(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_mkcl_mkstemp(MKCL, mkcl_narg narg, mkcl_object template, ...);
  extern MKCL_API mkcl_object mk_mkcl_rmdir(MKCL, mkcl_object directory);

  extern MKCL_API int mkcl_backup_open(MKCL, mkcl_object filename, int option, int mode);
  extern MKCL_API mkcl_object mkcl_file_len(MKCL, int f);
  extern MKCL_API mkcl_object mkcl_homedir_pathname(MKCL, mkcl_object user);
  extern MKCL_API mkcl_object mk_si_get_SYS_library_pathname(MKCL);
  extern MKCL_API mkcl_object mk_mkcl_copy_file(MKCL, mkcl_object orig, mkcl_object end);

  extern MKCL_API pid_t mkcl_gettid(void);

  extern MKCL_API bool mkcl_probe_file(MKCL, mkcl_object os_filename, bool follow_links);
  extern MKCL_API mkcl_object mk_mkcl_probe_file_p(MKCL, mkcl_object filename);
  extern MKCL_API mkcl_object mk_mkcl_stream_filename(MKCL, mkcl_object x);

  extern MKCL_API bool mkcl_pathname_component_string_is_wild_p(MKCL, mkcl_object comp_str);

  /* unixint.c */

#if MKCL_DEBUG_INTERRUPT_MASK
# define mkcl_disable_interrupts(env) ((env)->disable_interrupts=TRUE,	\
				       (env)->interrupt_disabler_file = __FILE__, \
				       (env)->interrupt_disabler_lineno = __LINE__)
# define mkcl_enable_interrupts(env) ((env)->disable_interrupts=FALSE,	\
				      (env)->interrupt_disabler_file = __FILE__, \
				      (env)->interrupt_disabler_lineno = __LINE__)
#else
# define mkcl_disable_interrupts(env) ((env)->disable_interrupts=TRUE)
# define mkcl_enable_interrupts(env) ((env)->disable_interrupts=FALSE)
#endif

#define MKCL_NO_INTR(env, expr) {		 \
    mkcl_interrupt_status __old_intr;		 \
    mkcl_get_interrupt_status(env, &__old_intr); \
    mkcl_disable_interrupts(env);		 \
    (expr);					 \
    mkcl_set_interrupt_status(env, &__old_intr); \
  }

#define MKCL_REALLY_NO_INTR(env, expr) {	 \
    mkcl_interrupt_status __old_intr;		 \
    mkcl_get_interrupt_status(env, &__old_intr); \
    (env)->disable_interrupts = 2;		 \
    (expr);					 \
    mkcl_set_interrupt_status(env, &__old_intr); \
  }
  
#define MKCL_Zzz(env, zzz_obj, expr) {		 \
    mkcl_interrupt_status __old_intr;		 \
    int __rc;					 \
    mkcl_get_interrupt_status(env, &__old_intr); \
    mkcl_disable_interrupts(env);		 \
    env->sleeping_on = (zzz_obj);		 \
    mk_mt_test_for_thread_shutdown(env);	 \
    __rc = (expr);				 \
    env->sleeping_on = mk_cl_Cnil;		 \
    mkcl_set_interrupt_status(env, &__old_intr); \
  }

#define MKCL_LIBC_NO_INTR(env, expr) {			\
    mkcl_ensure_call_stack(env, MKCL_LIBC_STACK_DEPTH);	\
    MKCL_NO_INTR(env, expr);				\
  }

#define MKCL_LIBC_REALLY_NO_INTR(env, expr) {		\
    mkcl_ensure_call_stack(env, MKCL_LIBC_STACK_DEPTH);	\
    MKCL_REALLY_NO_INTR(env, expr);			\
  }

#define MKCL_LIBC_Zzz(env, zzz_obj, expr) {		\
    mkcl_ensure_call_stack(env, MKCL_LIBC_STACK_DEPTH);	\
    MKCL_Zzz(env, zzz_obj, expr);			\
  }

#define MKCL_GC_NO_INTR(env, expr) {			\
    mkcl_ensure_call_stack(env, MKCL_GC_STACK_DEPTH);	\
    MKCL_REALLY_NO_INTR(env, expr);			\
  }

  extern MKCL_API mkcl_object mk_mt_try_to_wake_up_thread(MKCL, mkcl_object thread);

  extern MKCL_API mkcl_object mk_si_initial_floating_point_exception_set(MKCL);
  extern MKCL_API mkcl_object mk_si_disable_fpe(MKCL, mkcl_object exception);
  extern MKCL_API mkcl_object mk_si_enable_fpe(MKCL, mkcl_object exception);
  extern MKCL_API mkcl_object mk_si_all_enabled_fpe(MKCL);
  extern MKCL_API mkcl_object mk_si_fpe_enabled_p(MKCL, mkcl_object exception);
  extern MKCL_API mkcl_object mk_si_all_raised_fpe(MKCL);
  extern MKCL_API mkcl_object mk_si_fpe_raised_p(MKCL, mkcl_object exception);
  extern MKCL_API mkcl_object mk_si_raise_fpe(MKCL, mkcl_object exception);
  extern MKCL_API mkcl_object mk_si_clear_all_fpe(MKCL);
  extern MKCL_API mkcl_object mk_si_clear_fpe(MKCL, mkcl_object exception);
  extern MKCL_API void mkcl_clear_fpe(MKCL, int except);
  extern MKCL_API void mkcl_reactivate_fpe_set(MKCL);

  extern MKCL_API mkcl_object mkcl_signum_to_signal_name(MKCL, mkcl_word signum);
  extern MKCL_API mkcl_object mk_si_signum_to_signal_name(MKCL, mkcl_object signum);
  extern MKCL_API mkcl_object mk_si_do_sigsegv(MKCL);
  extern MKCL_API mkcl_object mk_si_objnull(MKCL);
  extern MKCL_API mkcl_object mk_si_objnull_value_p(MKCL, mkcl_object val);

  extern MKCL_API mkcl_object mk_si_display_signal_dispositions(MKCL);

  extern MKCL_API mkcl_object mk_si_install_sigsegv_monitor(MKCL);

  extern MKCL_API mkcl_object mk_si_setup_for_gdb(MKCL, mkcl_narg narg, ...);

  /* unixsys.c */

  extern MKCL_API void mkcl_safe_close(MKCL, int fd, mkcl_object stream);
  extern MKCL_API void mkcl_safe_fclose(MKCL, void *, mkcl_object stream);
  extern MKCL_API mkcl_object mk_mkcl_system(MKCL, mkcl_object command);
  extern MKCL_API mkcl_object mk_mkcl_make_pipe(MKCL);
  extern MKCL_API mkcl_object mk_mkcl_run_program_1(MKCL, mkcl_narg narg, mkcl_object command, mkcl_object args, ...);
  extern MKCL_API mkcl_object mk_mkcl_run_command(MKCL, mkcl_narg narg, mkcl_object cmd_string, mkcl_object directory, ...);
  extern MKCL_API mkcl_object mk_mkcl_process_p(MKCL, mkcl_object proc);
  extern MKCL_API mkcl_object mk_mkcl_process_id(MKCL, mkcl_object proc);
  extern MKCL_API mkcl_object mk_mkcl_process_command(MKCL, mkcl_object proc);
  extern MKCL_API mkcl_object mk_mkcl_process_argv(MKCL, mkcl_object proc);
  extern MKCL_API mkcl_object mk_mkcl_process_input(MKCL, mkcl_object proc);
  extern MKCL_API mkcl_object mk_mkcl_process_output(MKCL, mkcl_object proc);
  extern MKCL_API mkcl_object mk_mkcl_process_error(MKCL, mkcl_object proc);
  extern MKCL_API mkcl_object mk_mkcl_process_status(MKCL, mkcl_object proc);
  extern MKCL_API mkcl_object mk_mkcl_process_exit_code(MKCL, mkcl_object proc);
  extern MKCL_API mkcl_object mk_mkcl_join_process(MKCL, mkcl_object proc);
  extern MKCL_API mkcl_object mk_mkcl_terminate_process(MKCL, mkcl_narg narg, mkcl_object proc, ...);
  extern MKCL_API mkcl_object mk_mkcl_process_plist(MKCL, mkcl_object proc);
  extern MKCL_API mkcl_object mk_mkcl_process_to_worker(MKCL, mkcl_object proc);
  extern MKCL_API mkcl_object mk_mkcl_process_from_worker(MKCL, mkcl_object proc);
  extern MKCL_API mkcl_object mk_mkcl_process_error_from_worker(MKCL, mkcl_object proc);
  extern MKCL_API mkcl_object mk_mkcl_set_process_plist(MKCL, mkcl_object proc, mkcl_object plist);
  extern MKCL_API mkcl_object mk_mkcl_set_process_to_worker(MKCL, mkcl_object proc, mkcl_object to_worker);
  extern MKCL_API mkcl_object mk_mkcl_set_process_from_worker(MKCL, mkcl_object proc, mkcl_object from_worker);
  extern MKCL_API mkcl_object mk_mkcl_set_process_error_from_worker(MKCL, mkcl_object proc, mkcl_object error_from_worker);
  extern MKCL_API mkcl_object mk_mkcl_detach_process(MKCL, mkcl_object proc);
  extern MKCL_API mkcl_object mk_mkcl_process_detached_p(MKCL, mkcl_object proc);
  extern MKCL_API void mkcl_finalize_process(MKCL, mkcl_object proc);
  extern MKCL_API mkcl_object mk_si_uname(MKCL);

  extern MKCL_API mkcl_object mk_si_describe_console_mode(MKCL);

  extern MKCL_API mkcl_object mk_si_list_all_children(MKCL); /* debug JCB */
  extern MKCL_API mkcl_object mk_si_trace_specials(MKCL); /* debug JCB */
  extern MKCL_API mkcl_object mk_si_untrace_specials(MKCL); /* debug JCB */


  /* unicode -- no particular file, but we group these changes here */

  extern MKCL_API mkcl_object mk_mkcl_base_char_p(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_coerce_to_base_string(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_base_string_p(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mk_si_coerce_to_character_string(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_alloc_simple_character_string(MKCL, mkcl_index l);
  extern MKCL_API mkcl_object mkcl_alloc_adjustable_character_string(MKCL, mkcl_index l);
  extern MKCL_API mkcl_object mkcl_coerce_to_simple_base_string(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_coerce_to_simple_character_string(MKCL, mkcl_object x);

  static inline mkcl_object mkcl_coerce_to_base_string(MKCL, mkcl_object x)
  { return ((mkcl_type_of(x) != mkcl_t_base_string) ? mkcl_coerce_to_simple_base_string(env, x) : x); }

  /**********************************************************************
   * FUNCTIONS GENERATED BY THE LISP COMPILER
   *
   * Warning: These cannot be called before mkcl_boot() has completed its business.
   *
   */

  /* arraylib.lsp */
  extern MKCL_API mkcl_object mk_cl_make_array(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_vector(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_si_fill_array_with_seq(MKCL, mkcl_object V1, mkcl_object V2);
  extern MKCL_API mkcl_object mk_cl_array_dimensions(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_cl_array_in_bounds_p(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_bit(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_sbit(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_bit_and(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_bit_ior(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_bit_xor(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_bit_eqv(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_bit_nand(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_bit_nor(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_bit_andc1(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_bit_andc2(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_bit_orc1(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_bit_orc2(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_bit_not(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_vector_push(MKCL, mkcl_object V1, mkcl_object V2);
  extern MKCL_API mkcl_object mk_cl_vector_push_extend(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_vector_pop(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_cl_adjust_array(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);

  /* config.lsp */

  extern MKCL_API mkcl_object mk_si_mkcl_version(MKCL);
  extern MKCL_API mkcl_object mk_si_mkcl_major_version(MKCL);
  extern MKCL_API mkcl_object mk_si_mkcl_minor_version(MKCL);
  extern MKCL_API mkcl_object mk_si_mkcl_patch_level(MKCL);

  /* iolib.lsp */

  extern MKCL_API mkcl_object mk_cl_read_from_string(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_write_to_string(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_prin1_to_string(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_cl_princ_to_string(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_mkcl_write_to_base_string(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_mkcl_prin1_to_base_string(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_mkcl_princ_to_base_string(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_y_or_n_p(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_yes_or_no_p(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_mkcl_run_program(MKCL, mkcl_narg narg, mkcl_object command, mkcl_object args, ...);

  /* listlib.lsp */

  extern MKCL_API mkcl_object mk_cl_union(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_nunion(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_intersection(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_nintersection(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_set_difference(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_nset_difference(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_set_exclusive_or(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_nset_exclusive_or(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_subsetp(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_rassoc_if(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_rassoc_if_not(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_assoc_if(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_assoc_if_not(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_member_if(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_member_if_not(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_subst_if(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, mkcl_object V3, ...);
  extern MKCL_API mkcl_object mk_cl_subst_if_not(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, mkcl_object V3, ...);
  extern MKCL_API mkcl_object mk_cl_nsubst_if(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, mkcl_object V3, ...);
  extern MKCL_API mkcl_object mk_cl_nsubst_if_not(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, mkcl_object V3, ...);

  /* mislib.lsp */

  extern MKCL_API mkcl_object mk_cl_logical_pathname_translations(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_cl_load_logical_pathname_translations(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_cl_decode_universal_time(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_encode_universal_time(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, mkcl_object V3, mkcl_object V4, mkcl_object V5, mkcl_object V6, ...);
  extern MKCL_API mkcl_object mk_cl_get_decoded_time(MKCL);
  extern MKCL_API mkcl_object mk_cl_ensure_directories_exist(MKCL, mkcl_narg narg, mkcl_object V1, ...);

  /* module.lsp */

  extern MKCL_API mkcl_object mk_cl_provide(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_cl_require(MKCL, mkcl_narg narg, mkcl_object V1, ...);

  /* numlib.lsp */

  extern MKCL_API mkcl_object mk_cl_isqrt(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_cl_phase(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_cl_signum(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_cl_cis(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_cl_asin(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_cl_acos(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_cl_asinh(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_cl_acosh(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_cl_atanh(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_cl_ffloor(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_fceiling(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_ftruncate(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_fround(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_logtest(MKCL, mkcl_object V1, mkcl_object V2);
  extern MKCL_API mkcl_object mk_cl_byte(MKCL, mkcl_object V1, mkcl_object V2);
  extern MKCL_API mkcl_object mk_cl_byte_size(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_cl_byte_position(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_cl_ldb(MKCL, mkcl_object V1, mkcl_object V2);
  extern MKCL_API mkcl_object mk_cl_ldb_test(MKCL, mkcl_object V1, mkcl_object V2);
  extern MKCL_API mkcl_object mk_cl_mask_field(MKCL, mkcl_object V1, mkcl_object V2);
  extern MKCL_API mkcl_object mk_cl_dpb(MKCL, mkcl_object V1, mkcl_object V2, mkcl_object V3);
  extern MKCL_API mkcl_object mk_cl_deposit_field(MKCL, mkcl_object V1, mkcl_object V2, mkcl_object V3);

  /* packlib.lsp */

  extern MKCL_API mkcl_object mk_cl_apropos(MKCL, mkcl_narg arg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_apropos_list(MKCL, mkcl_narg arg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_find_all_symbols(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_si_find_relative_package(MKCL, mkcl_object pack_name);
  extern MKCL_API mkcl_object mk_si_package_parent(MKCL, mkcl_object package_specifier);
  extern MKCL_API mkcl_object mk_si_package_children(MKCL, mkcl_narg narg, mkcl_object package_specifier, ...);

  /* predlib.lsp */

  extern MKCL_API mkcl_object mk_si_subclassp(MKCL, mkcl_object V1, mkcl_object V2);
  extern MKCL_API mkcl_object mk_si_of_class_p(MKCL, mkcl_object V1, mkcl_object V2);
  extern MKCL_API mkcl_object mk_si_do_deftype(MKCL, mkcl_object V1, mkcl_object V2, mkcl_object V3);
  extern MKCL_API mkcl_object mk_cl_upgraded_array_element_type(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_upgraded_complex_part_type(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_typep(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_coerce(MKCL, mkcl_object V1, mkcl_object V2);
  extern MKCL_API mkcl_object mk_cl_subtypep(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);

  /* seq.lsp */

  extern MKCL_API mkcl_object mk_cl_make_sequence(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_concatenate(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_map(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, mkcl_object V3, ...);
  extern MKCL_API mkcl_object mk_cl_some(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_every(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_notany(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_notevery(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_map_into(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);

  /* seqlib.lsp */

  extern MKCL_API mkcl_object mk_cl_reduce(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_fill(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_replace(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_remove(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_remove_if(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_remove_if_not(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_delete(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_delete_if(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_delete_if_not(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_count(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_count_if(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_count_if_not(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_substitute(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, mkcl_object V3, ...);
  extern MKCL_API mkcl_object mk_cl_substitute_if(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, mkcl_object V3, ...);
  extern MKCL_API mkcl_object mk_cl_substitute_if_not(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, mkcl_object V3, ...);
  extern MKCL_API mkcl_object mk_cl_nsubstitute(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, mkcl_object V3, ...);
  extern MKCL_API mkcl_object mk_cl_nsubstitute_if(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, mkcl_object V3, ...);
  extern MKCL_API mkcl_object mk_cl_nsubstitute_if_not(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, mkcl_object V3, ...);
  extern MKCL_API mkcl_object mk_cl_find(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_find_if(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_find_if_not(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_position(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_position_if(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_position_if_not(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_remove_duplicates(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_delete_duplicates(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_mismatch(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_search(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_sort(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_stable_sort(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_merge(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, mkcl_object V3, mkcl_object V4, ...);
  extern MKCL_API mkcl_object mk_cl_constantly(MKCL, mkcl_object V1);
  extern MKCL_API mkcl_object mk_cl_complement(MKCL, mkcl_object V1);

  /* pprint.lsp */

  extern MKCL_API mkcl_object mk_cl_pprint_newline(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_pprint_indent(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_pprint_tab(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, mkcl_object V3, ...);
  extern MKCL_API mkcl_object mk_cl_pprint_fill(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_pprint_linear(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_pprint_tabular(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);
  extern MKCL_API mkcl_object mk_cl_copy_pprint_dispatch(MKCL, mkcl_narg narg, ...);
  extern MKCL_API mkcl_object mk_cl_pprint_dispatch(MKCL, mkcl_narg narg, mkcl_object V1, ...);
  extern MKCL_API mkcl_object mk_cl_set_pprint_dispatch(MKCL, mkcl_narg narg, mkcl_object V1, mkcl_object V2, ...);


  /* combin.lsp */
  extern MKCL_API mkcl_object mk_cl_method_combination_error(MKCL, mkcl_narg narg, mkcl_object format, ...);
  extern MKCL_API mkcl_object mk_cl_invalid_method_error(MKCL, mkcl_narg narg, mkcl_object method, mkcl_object format, ...);


  /* uni-cond.lsp */

  extern MKCL_API mkcl_object mk_si_stream_encoding_error(MKCL, mkcl_object stream, mkcl_object external_format, mkcl_object character_codepoint);
  extern MKCL_API mkcl_object mk_si_stream_decoding_error(MKCL, mkcl_object stream, mkcl_object external_format, mkcl_object octets);

  /* conditions.lsp */
  extern MKCL_API mkcl_object mk_cl_continue(MKCL, mkcl_narg narg, ...);


  /* *********************** */

  /* Inlined type converters */

  static inline mkcl_word mkcl_safe_fixnum_to_word(MKCL, mkcl_object obj)
  {
    if (mkcl_likely(MKCL_FIXNUMP(obj)))
      return mkcl_fixnum_to_word(obj);
    else
      mkcl_FEnot_fixnum_type(env, obj);
  }

  static inline mkcl_object mkcl_make_integer(MKCL, mkcl_word l)
  {
    if (mkcl_likely(MKCL_MOST_NEGATIVE_FIXNUM <= l && l <= MKCL_MOST_POSITIVE_FIXNUM))
      return MKCL_MAKE_FIXNUM(l);
    else
      return mkcl_make_big_integer(env, l);
  }

  static inline mkcl_object mkcl_make_unsigned_integer(MKCL, mkcl_index l)
  {
    if (mkcl_likely(l <= MKCL_MOST_POSITIVE_FIXNUM))
      return MKCL_MAKE_FIXNUM(l);
    else
      return mkcl_make_big_unsigned_integer(env, l);
  }

  /* Array and vector inlined accessors */

  extern MKCL_API mkcl_object mkcl_ensure_vector_type(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_ensure_specialized_vector_type(MKCL, mkcl_object x, mkcl_elttype elem_type);
  extern MKCL_API mkcl_object mkcl_ensure_array_type(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_ensure_specialized_array_type(MKCL, mkcl_object x, mkcl_elttype elem_type);

  static inline mkcl_object mkcl_aref_index_object(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_object)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_object);
    mkcl_object * self = x->array.self.t;
    return (mkcl_likely(i < x->array.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }

  static inline mkcl_object mkcl_aref_index_object_raw(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_object)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_object);
    mkcl_object * self = x->array.self.t;
    return (mkcl_likely(i < x->array.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }

  static inline mkcl_object mkcl_aref_index_fixnum(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_fixnum)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_fixnum);
    mkcl_word * self = x->array.self.word;
    return MKCL_MAKE_FIXNUM(mkcl_likely(i < x->array.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }

  static inline mkcl_word mkcl_aref_index_fixnum_raw(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_fixnum)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_fixnum);
    mkcl_word * self = x->array.self.word;
    return (mkcl_likely((i < x->array.dim)) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }

  static inline mkcl_object mkcl_aref_index_bc(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_bc)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_bc);
    mkcl_base_char * self = x->array.self.bc;
    return MKCL_CODE_CHAR((i < x->array.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }
  
  static inline mkcl_base_char mkcl_aref_index_bc_raw(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_bc)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_bc);
    mkcl_base_char * self = x->array.self.bc;
    return (mkcl_likely(i < x->array.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }
  
  static inline mkcl_object mkcl_aref_index_ch(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_ch)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_ch);
    mkcl_character * self = x->array.self.c;
    return MKCL_CODE_CHAR(mkcl_likely(i < x->array.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }
  
  static inline mkcl_character mkcl_aref_index_ch_raw(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_ch)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_ch);
    mkcl_character * self = x->array.self.c;
    return (mkcl_likely(i < x->array.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }
  
  static inline mkcl_object mkcl_aref_index_bit(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_bit)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_bit);
    mkcl_byte * self = x->vector.self.bit;
    mkcl_index o = x->vector.bit_offset;

    return ((mkcl_likely(i < x->vector.dim)
             ? (mkcl_bit_bundle(self, i+o) & mkcl_bundle_bit_mask(i+o))
             : (i = mkcl_ensure_valid_array_index(env, x, i),
                (mkcl_bit_bundle(self, i+o) & mkcl_bundle_bit_mask(i+o))))
            ? MKCL_MAKE_FIXNUM(1) : MKCL_MAKE_FIXNUM(0));
  }

  static inline mkcl_word mkcl_aref_index_bit_raw(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_bit)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_bit);
    mkcl_byte * self = x->vector.self.bit;
    mkcl_index o = x->vector.bit_offset;

    return ((mkcl_likely(i < x->vector.dim)
             ? (mkcl_bit_bundle(self, i+o) & mkcl_bundle_bit_mask(i+o))
             : (i = mkcl_ensure_valid_array_index(env, x, i),
                (mkcl_bit_bundle(self, i+o) & mkcl_bundle_bit_mask(i+o))))
            ? 1 : 0);
  }

  static inline mkcl_object mkcl_aref_index_word(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_word)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_word);
    mkcl_word * self = x->array.self.word;
    return mkcl_make_integer(env, (mkcl_likely(i < x->array.dim)
				   ? self[i]
				   : (i = mkcl_ensure_valid_array_index(env, x, i), self[i])));
  }

  static inline mkcl_word mkcl_aref_index_word_raw(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_word)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_word);
    mkcl_word * self = x->array.self.word;
    return (mkcl_likely(i < x->array.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }

  static inline mkcl_object mkcl_aref_index_index(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_index)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_index);
    mkcl_index * self = x->array.self.index;
    return mkcl_make_unsigned_integer(env, (mkcl_likely(i < x->array.dim)
					    ? self[i]
					    : (i = mkcl_ensure_valid_array_index(env, x, i), self[i])));
  }

  static inline mkcl_index mkcl_aref_index_index_raw(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_index)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_index);
    mkcl_index * self = x->array.self.index;
    return (mkcl_likely(i < x->array.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }

  static inline mkcl_object mkcl_aref_index_sf(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_sf)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_sf);
    float * self = x->array.self.sf;
    return mkcl_make_singlefloat(env, (mkcl_likely(i < x->array.dim)
				       ? self[i]
				       : (i = mkcl_ensure_valid_array_index(env, x, i), self[i])));
  }

  static inline float mkcl_aref_index_sf_raw(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_sf)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_sf);
    float * self = x->array.self.sf;
    return (mkcl_likely(i < x->array.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }

  static inline mkcl_object mkcl_aref_index_df(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_df)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_df);
    double * self = x->array.self.df;
    return mkcl_make_doublefloat(env, (mkcl_likely(i < x->array.dim)
				       ? self[i]
				       : (i = mkcl_ensure_valid_array_index(env, x, i), self[i])));
  }

  static inline double mkcl_aref_index_df_raw(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_df)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_df);
    double * self = x->array.self.df;
    return (mkcl_likely(i < x->array.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }

  static inline mkcl_object mkcl_aref_index_b8(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_b8)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_b8);
    uint8_t * self = x->array.self.b8;
    return mkcl_make_uint8_t(env, (mkcl_likely(i < x->array.dim)
				   ? self[i]
				   : (i = mkcl_ensure_valid_array_index(env, x, i), self[i])));
  }

  static inline uint8_t mkcl_aref_index_b8_raw(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_b8)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_b8);
    uint8_t* self = x->array.self.b8;
    return ((i < x->array.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }

  static inline mkcl_object mkcl_aref_index_i8(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_i8)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_i8);
    int8_t * self = x->array.self.i8;
    return mkcl_make_int8_t(env, (mkcl_likely(i < x->array.dim)
				  ? self[i]
				  : (i = mkcl_ensure_valid_array_index(env, x, i), self[i])));
  }

  static inline int8_t mkcl_aref_index_i8_raw(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_i8)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_i8);
    int8_t * self = x->array.self.i8;
    return (mkcl_likely(i < x->array.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }

  static inline mkcl_object mkcl_aref_index_b16(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_b16)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_b16);
    uint16_t * self = x->array.self.b16;
    return mkcl_make_uint16_t(env, (mkcl_likely(i < x->array.dim)
				    ? self[i]
				    : (i = mkcl_ensure_valid_array_index(env, x, i), self[i])));
  }

  static inline uint16_t mkcl_aref_index_b16_raw(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_b16)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_b16);
    uint16_t * self = x->array.self.b16;
    return (mkcl_likely(i < x->array.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }

  static inline mkcl_object mkcl_aref_index_i16(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_i16)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_i16);
    int16_t * self = x->array.self.i16;
    return mkcl_make_int16_t(env, (mkcl_likely(i < x->array.dim)
				   ? self[i]
				   : (i = mkcl_ensure_valid_array_index(env, x, i), self[i])));
  }

  static inline int16_t mkcl_aref_index_i16_raw(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_i16)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_i16);
    int16_t * self = x->array.self.i16;
    return (mkcl_likely(i < x->array.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }

  static inline mkcl_object mkcl_aref_index_b32(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_b32)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_b32);
    uint32_t * self = x->array.self.b32;
    return mkcl_make_uint32_t(env, (mkcl_likely(i < x->array.dim)
				    ? self[i]
				    : (i = mkcl_ensure_valid_array_index(env, x, i), self[i])));
  }

  static inline uint32_t mkcl_aref_index_b32_raw(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_b32)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_b32);
    uint32_t * self = x->array.self.b32;
    return (mkcl_likely(i < x->array.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }

  static inline mkcl_object mkcl_aref_index_i32(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_i32)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_i32);
    int32_t * self = x->array.self.i32;
    return mkcl_make_int32_t(env, (mkcl_likely(i < x->array.dim)
				   ? self[i]
				   : (i = mkcl_ensure_valid_array_index(env, x, i), self[i])));
  }

  static inline int32_t mkcl_aref_index_i32_raw(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_i32)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_i32);
    int32_t * self = x->array.self.i32;
    return (mkcl_likely(i < x->array.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }

  static inline mkcl_object mkcl_aref_index_b64(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_b64)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_b64);
    uint64_t * self = x->array.self.b64;
    return mkcl_make_uint64_t(env, (mkcl_likely(i < x->array.dim)
				    ? self[i]
				    : (i = mkcl_ensure_valid_array_index(env, x, i), self[i])));
  }

  static inline uint64_t mkcl_aref_index_b64_raw(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_b64)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_b64);
    uint64_t * self = x->array.self.b64;
    return (mkcl_likely(i < x->array.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }

  static inline mkcl_object mkcl_aref_index_i64(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_i64)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_i64);
    int64_t * self = x->array.self.i64;
    return mkcl_make_int64_t(env, (mkcl_likely(i < x->array.dim)
				   ? self[i]
				   : (i = mkcl_ensure_valid_array_index(env, x, i), self[i])));
  }

  static inline int64_t mkcl_aref_index_i64_raw(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_i64)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_i64);
    int64_t * self = x->array.self.i64;
    return (mkcl_likely(i < x->array.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }

  static inline mkcl_object mkcl_aset_index_object(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_object)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_object);
    mkcl_object * self = x->array.self.t;
    return (mkcl_likely(i < x->array.dim) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
  }

  static inline mkcl_object mkcl_aset_index_object_raw(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_object)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_object);
    mkcl_object * self = x->array.self.t;
    return (mkcl_likely(i < x->array.dim) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
  }

  static inline mkcl_object mkcl_aset_index_fixnum(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_fixnum)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_fixnum);
    mkcl_word * self = x->array.self.word;
    mkcl_word word = mkcl_fixnum_to_word(v);

    (mkcl_likely(i < x->array.dim) ? self[i] = word : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = word));
    return v;
  }

  static inline mkcl_word mkcl_aset_index_fixnum_raw(MKCL, mkcl_object x, mkcl_index i, mkcl_word v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_fixnum)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_fixnum);
    mkcl_word * self = x->array.self.word;

    (mkcl_likely((i < x->array.dim)) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
    return v;
  }

  static inline mkcl_object mkcl_aset_index_bc(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_bc)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_bc);
    mkcl_base_char * self = x->array.self.bc;
    mkcl_base_char bc = mkcl_char_code(env, v);

    (mkcl_likely(i < x->array.dim) ? self[i] = bc : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = bc));
    return v;
  }

  static inline mkcl_base_char mkcl_aset_index_bc_raw(MKCL, mkcl_object x, mkcl_index i, mkcl_base_char v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_bc)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_bc);
    mkcl_base_char * self = x->array.self.bc;

    (mkcl_likely(i < x->array.dim) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
    return v;
  }

  static inline mkcl_object mkcl_aset_index_ch(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_ch)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_ch);
    mkcl_character * self = x->array.self.c;
    mkcl_character ch = mkcl_char_code(env, v);

    (mkcl_likely(i < x->array.dim) ? self[i] = ch : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = ch));
    return v;
  }

  static inline mkcl_character mkcl_aset_index_ch_raw(MKCL, mkcl_object x, mkcl_index i, mkcl_character v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_ch)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_ch);
    mkcl_character * self = x->array.self.c;

    (mkcl_likely(i < x->array.dim) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
    return v;
  }

  extern MKCL_API mkcl_object mkcl_ensure_bit_type_for_aset(MKCL, mkcl_object v);

  static inline mkcl_object mkcl_aset_index_bit(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_bit)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_bit);
    mkcl_byte * self = x->vector.self.bit;
    mkcl_word b;

    if (i >= x->vector.dim)
      i = mkcl_ensure_valid_array_index(env, x, i);

    i += x->vector.bit_offset;

    if (!((v == MKCL_MAKE_FIXNUM(0)) || (v == MKCL_MAKE_FIXNUM(1))))
      {
	v = mkcl_ensure_bit_type_for_aset(env, v);
      }
    b = mkcl_fixnum_to_word(v);

    if (b)
      mkcl_bit_bundle(self, i) |= mkcl_bundle_bit_mask(i);
    else
      mkcl_bit_bundle(self, i) &= ~mkcl_bundle_bit_mask(i);

    return v;
  }

  static inline mkcl_word mkcl_aset_index_bit_raw(MKCL, mkcl_object x, mkcl_index i, mkcl_word v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_bit)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_bit);
    mkcl_byte * self = x->vector.self.bit;

    if (i >= x->vector.dim)
      i = mkcl_ensure_valid_array_index(env, x, i);

    i += x->vector.bit_offset;

    if (v)
      mkcl_bit_bundle(self, i) |= mkcl_bundle_bit_mask(i);
    else
      mkcl_bit_bundle(self, i) &= ~mkcl_bundle_bit_mask(i);

    return v;
  }

  static inline mkcl_object mkcl_aset_index_word(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_word)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_word);
    mkcl_word * self = x->array.self.word;
    mkcl_word word = mkcl_integer_to_word(env, v);

    (mkcl_likely(i < x->array.dim) ? self[i] = word : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = word));
    return v;
  }

  static inline mkcl_word mkcl_aset_index_word_raw(MKCL, mkcl_object x, mkcl_index i, mkcl_word v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_word)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_word);
    mkcl_word * self = x->array.self.word;
    (mkcl_likely(i < x->array.dim) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
    return v;
  }

  static inline mkcl_object mkcl_aset_index_index(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_index)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_index);
    mkcl_index * self = x->array.self.index;
    mkcl_index index = mkcl_integer_to_index(env, v);

    (mkcl_likely(i < x->array.dim) ? self[i] = index : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = index));
    return v;
  }

  static inline mkcl_index mkcl_aset_index_index_raw(MKCL, mkcl_object x, mkcl_index i, mkcl_index v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_index)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_index);
    mkcl_index * self = x->array.self.index;
    (mkcl_likely(i < x->array.dim) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
    return v;
  }

  static inline mkcl_object mkcl_aset_index_sf(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_sf)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_sf);
    float * self = x->array.self.sf;
    float sf = mkcl_to_float(env, v);

    (mkcl_likely(i < x->array.dim) ? self[i] = sf : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = sf));
    return v;
  }

  static inline float mkcl_aset_index_sf_raw(MKCL, mkcl_object x, mkcl_index i, float v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_sf)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_sf);
    float * self = x->array.self.sf;
    (mkcl_likely(i < x->array.dim) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
    return v;
  }

  static inline mkcl_object mkcl_aset_index_df(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_df)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_df);
    double * self = x->array.self.df;
    double df = mkcl_to_double(env, v);

    (mkcl_likely(i < x->array.dim) ? self[i] = df : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = df));
    return v;
  }

  static inline double mkcl_aset_index_df_raw(MKCL, mkcl_object x, mkcl_index i, double v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_df)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_df);
    double * self = x->array.self.df;
    (mkcl_likely(i < x->array.dim) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
    return v;
  }

  static inline mkcl_object mkcl_aset_index_b8(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_b8)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_b8);
    uint8_t * self = x->array.self.b8;
    uint8_t b8 = mkcl_to_uint8_t(env, v);

    (mkcl_likely(i < x->array.dim) ? self[i] = b8 : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = b8));
    return v;
  }

  static inline uint8_t mkcl_aset_index_b8_raw(MKCL, mkcl_object x, mkcl_index i, uint8_t v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_b8)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_b8);
    uint8_t * self = x->array.self.b8;
    (mkcl_likely(i < x->array.dim) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
    return v;
  }

  static inline mkcl_object mkcl_aset_index_i8(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_i8)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_i8);
    int8_t * self = x->array.self.i8;
    int8_t i8 = mkcl_to_int8_t(env, v);

    (mkcl_likely(i < x->array.dim) ? self[i] = i8 : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = i8));
    return v;
  }

  static inline int8_t mkcl_aset_index_i8_raw(MKCL, mkcl_object x, mkcl_index i, int8_t v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_i8)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_i8);
    int8_t * self = x->array.self.i8;
    (mkcl_likely(i < x->array.dim) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
    return v;
  }

  static inline mkcl_object mkcl_aset_index_b16(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_b16)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_b16);
    uint16_t * self = x->array.self.b16;
    uint16_t b16 = mkcl_to_uint16_t(env, v);

    (mkcl_likely(i < x->array.dim) ? self[i] = b16 : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = b16));
    return v;
  }

  static inline uint16_t mkcl_aset_index_b16_raw(MKCL, mkcl_object x, mkcl_index i, uint16_t v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_b16)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_b16);
    uint16_t * self = x->array.self.b16;
    (mkcl_likely(i < x->array.dim) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
    return v;
  }

  static inline mkcl_object mkcl_aset_index_i16(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_i16)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_i16);
    int16_t * self = x->array.self.i16;
    int16_t i16 = mkcl_to_int16_t(env, v);

    (mkcl_likely(i < x->array.dim) ? self[i] = i16 : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = i16));
    return v;
  }

  static inline int16_t mkcl_aset_index_i16_raw(MKCL, mkcl_object x, mkcl_index i, int16_t v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_i16)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_i16);
    int16_t * self = x->array.self.i16;
    (mkcl_likely(i < x->array.dim) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
    return v;
  }

  static inline mkcl_object mkcl_aset_index_b32(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_b32)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_b32);
    uint32_t * self = x->array.self.b32;
    uint32_t b32 = mkcl_to_uint32_t(env, v);

    (mkcl_likely(i < x->array.dim) ? self[i] = b32 : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = b32));
    return v;
  }

  static inline uint32_t mkcl_aset_index_b32_raw(MKCL, mkcl_object x, mkcl_index i, uint32_t v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_b32)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_b32);
    uint32_t * self = x->array.self.b32;
    (mkcl_likely(i < x->array.dim) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
    return v;
  }

  static inline mkcl_object mkcl_aset_index_i32(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_i32)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_i32);
    int32_t * self = x->array.self.i32;
    int32_t i32 = mkcl_to_int32_t(env, v);

    (mkcl_likely(i < x->array.dim) ? self[i] = i32 : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = i32));
    return v;
  }

  static inline int32_t mkcl_aset_index_i32_raw(MKCL, mkcl_object x, mkcl_index i, int32_t v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_i32)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_i32);
    int32_t * self = x->array.self.i32;
    (mkcl_likely(i < x->array.dim) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
    return v;
  }

  static inline mkcl_object mkcl_aset_index_b64(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_b64)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_b64);
    uint64_t * self = x->array.self.b64;
    uint64_t b64 = mkcl_to_uint64_t(env, v);

    (mkcl_likely(i < x->array.dim) ? self[i] = b64 : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = b64));
    return v;
  }

  static inline uint64_t mkcl_aset_index_b64_raw(MKCL, mkcl_object x, mkcl_index i, uint64_t v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_b64)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_b64);
    uint64_t * self = x->array.self.b64;
    (mkcl_likely(i < x->array.dim) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
    return v;
  }

  static inline mkcl_object mkcl_aset_index_i64(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_i64)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_i64);
    int64_t * self = x->array.self.i64;
    int64_t i64 = mkcl_to_int64_t(env, v);

    (mkcl_likely(i < x->array.dim) ? self[i] = i64 : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = i64));
    return v;
  }

  static inline int64_t mkcl_aset_index_i64_raw(MKCL, mkcl_object x, mkcl_index i, int64_t v)
  {
    while (mkcl_unlikely(!(MKCL_ARRAYP(x) && x->array.elttype == mkcl_aet_i64)))
      x = mkcl_ensure_specialized_array_type(env, x, mkcl_aet_i64);
    int64_t * self = x->array.self.i64;
    ((i < x->array.dim) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
    return v;
  }

  static inline mkcl_object mkcl_aref_index(MKCL, mkcl_object x, mkcl_index index)
  {
    while (mkcl_unlikely(!MKCL_ARRAYP(x))) x = mkcl_ensure_array_type(env, x);
    return x->array.elem(env, x, index);
  }
  
  static inline mkcl_object mkcl_aset_index(MKCL, mkcl_object x, mkcl_index index, mkcl_object value)
  {
    while (mkcl_unlikely(!MKCL_ARRAYP(x))) x = mkcl_ensure_array_type(env, x);
    return x->array.set(env, x, index, value);
  }

  static inline mkcl_object mkcl_vref_index(MKCL, mkcl_object v, mkcl_index index)
  {
    while (mkcl_unlikely(!MKCL_VECTORP(v))) v = mkcl_ensure_vector_type(env, v);
    return v->vector.elem(env, v, index);
  }

  static inline mkcl_object mkcl_vset_index(MKCL, mkcl_object v, mkcl_index index, mkcl_object val)
  {
    while (mkcl_unlikely(!MKCL_VECTORP(v))) v = mkcl_ensure_vector_type(env, v);
    return v->vector.set(env, v, index, val);
  }

# define mkcl_svref_index mkcl_aref_index_object
# define mkcl_svset_index mkcl_aset_index_object

# define mkcl_bvref_index     mkcl_aref_index_bit
# define mkcl_bvref_index_raw mkcl_aref_index_bit_raw
# define mkcl_bvset_index     mkcl_aset_index_bit
# define mkcl_bvset_index_raw mkcl_aset_index_bit_raw


  extern MKCL_API mkcl_object mkcl_ensure_string_type(MKCL, mkcl_object x);
  extern MKCL_API mkcl_object mkcl_ensure_base_string_type(MKCL, mkcl_object x);

  static inline mkcl_object mkcl_base_char_index(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!MKCL_BASE_STRING_P(x))) x = mkcl_ensure_base_string_type(env, x);
    mkcl_base_char * self = x->base_string.self;
    return MKCL_CODE_CHAR(mkcl_likely(i < x->base_string.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }
  
  static inline mkcl_base_char mkcl_base_char_index_raw(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!MKCL_BASE_STRING_P(x))) x = mkcl_ensure_base_string_type(env, x);
    mkcl_base_char * self = x->base_string.self;
    return (mkcl_likely(i < x->base_string.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
  }
  
  static inline mkcl_object mkcl_base_char_set_index(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!MKCL_BASE_STRING_P(x))) x = mkcl_ensure_base_string_type(env, x);
    mkcl_base_char * self = x->base_string.self;
    mkcl_base_char bc = mkcl_base_char_code(env, v);

    (mkcl_likely(i < x->base_string.dim) ? self[i] = bc : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = bc));
    return v;
  }

  static inline mkcl_base_char mkcl_base_char_set_index_raw(MKCL, mkcl_object x, mkcl_index i, mkcl_base_char v)
  {
    while (mkcl_unlikely(!MKCL_BASE_STRING_P(x))) x = mkcl_ensure_base_string_type(env, x);
    mkcl_base_char * self = x->base_string.self;

    (mkcl_likely(i < x->base_string.dim) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
    return v;
  }

  static inline mkcl_object mkcl_character_index(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!MKCL_STRINGP(x))) x = mkcl_ensure_string_type(env, x);
    if (x->d.t == mkcl_t_base_string) {
      mkcl_base_char * self = x->base_string.self;
      return MKCL_CODE_CHAR(mkcl_likely(i < x->base_string.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
    } else {
      mkcl_character * self = x->string.self;
      return MKCL_CODE_CHAR(mkcl_likely(i < x->string.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
    }
  }
  
  static inline mkcl_character mkcl_character_index_raw(MKCL, mkcl_object x, mkcl_index i)
  {
    while (mkcl_unlikely(!MKCL_STRINGP(x))) x = mkcl_ensure_string_type(env, x);
    if (x->d.t == mkcl_t_base_string) {
      mkcl_base_char * self = x->base_string.self;
      return (mkcl_likely(i < x->base_string.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
    } else {
      mkcl_character * self = x->string.self;
      return (mkcl_likely(i < x->string.dim) ? self[i] : (i = mkcl_ensure_valid_array_index(env, x, i), self[i]));
    }
  }
  
  static inline mkcl_object mkcl_character_set_index(MKCL, mkcl_object x, mkcl_index i, mkcl_object v)
  {
    while (mkcl_unlikely(!MKCL_STRINGP(x))) x = mkcl_ensure_string_type(env, x);
    if (x->d.t == mkcl_t_base_string) {
      mkcl_base_char * self = x->base_string.self;
      mkcl_base_char bc = mkcl_base_char_code(env, v);

      (mkcl_likely(i < x->base_string.dim) ? self[i] = bc : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = bc));
      return v;
    } else {
      mkcl_character * self = x->string.self;
      mkcl_character ch = mkcl_char_code(env, v);

      (mkcl_likely(i < x->string.dim) ? self[i] = ch : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = ch));
      return v;
    }
  }

  static inline mkcl_character mkcl_character_set_index_raw(MKCL, mkcl_object x, mkcl_index i, mkcl_character v)
  {
    while (mkcl_unlikely(!MKCL_STRINGP(x))) x = mkcl_ensure_string_type(env, x);
    if (x->d.t == mkcl_t_base_string) {
      mkcl_base_char * self = x->base_string.self;

      if (mkcl_unlikely(!MKCL_BASE_CHAR_CODE_P(v))) mkcl_FEtype_error_base_char(env, MKCL_CODE_CHAR(v));
      (mkcl_likely(i < x->base_string.dim) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
      return v;
    } else {
      mkcl_character * self = x->string.self;

      (mkcl_likely(i < x->string.dim) ? self[i] = v : (i = mkcl_ensure_valid_array_index(env, x, i), self[i] = v));
      return v;
    }
  }



  extern MKCL_API mkcl_index mkcl_ensure_index_for_array_row_major_index(MKCL, mkcl_index i, mkcl_index dim);

  static inline mkcl_index mkcl_array_row_major_index_2_index(MKCL, mkcl_object a, mkcl_index i, mkcl_index j)
  {
    mkcl_index dim0;
    mkcl_index dim1;

    while (mkcl_unlikely(!MKCL_ARRAYP(a))) a = mkcl_ensure_array_type(env, a);

    /* "a" must be an array but this function will survive being passed any
       sub-type of array for that argument. That is thanks to the fact that
       field "rank" has the same offset as field "hasfillp" of vectors and
       that "hasfillp" can have only two different values, either 1 (TRUE)
       of 0 (FALSE).
    */
    if (2 != a->array.rank)
      mkcl_FEerror(env, "Wrong number of indices.", 0);

    dim0 = a->array.dims[0];
    dim1 = a->array.dims[1];

    if (mkcl_likely((i < dim0) && (j < dim1)))
      return (i * dim1) + j;
    else
      {
	i = mkcl_ensure_index_for_array_row_major_index(env, i, dim0);
	j = mkcl_ensure_index_for_array_row_major_index(env, j, dim1);

	return (i * dim1) + j;
      }
  }

  static inline mkcl_index mkcl_array_row_major_index_3_index(MKCL, mkcl_object a, mkcl_index i, mkcl_index j, mkcl_index k)
  {
    mkcl_index dim0;
    mkcl_index dim1;
    mkcl_index dim2;

    while (mkcl_unlikely(!MKCL_ARRAYP(a))) a = mkcl_ensure_array_type(env, a);

    /* "a" must be an array but this function will survive being passed any
       sub-type of array for that argument. That is thanks to the fact that
       field "rank" has the same offset as field "hasfillp" of vectors and
       that "hasfillp" can have only two different values, either 1 (TRUE)
       of 0 (FALSE).
    */
    if (3 != a->array.rank)
      mkcl_FEerror(env, "Wrong number of indices.", 0);

    dim0 = a->array.dims[0];
    dim1 = a->array.dims[1];
    dim2 = a->array.dims[2];

    if (mkcl_likely((i < dim0) && (j < dim1) && (k < dim2)))
      return (((i * dim1) + j) * dim2) + k;
    else
      {
	i = mkcl_ensure_index_for_array_row_major_index(env, i, dim0);
	j = mkcl_ensure_index_for_array_row_major_index(env, j, dim1);
	k = mkcl_ensure_index_for_array_row_major_index(env, k, dim2);

	return (((i * dim1) + j) * dim2) + k;
      }
  }

  static inline mkcl_index mkcl_vector_fill_pointer(MKCL, mkcl_object v)
  {
    if (mkcl_unlikely(!(MKCL_VECTORP(v) && v->vector.hasfillp)))
      mkcl_FEtype_error_vector_with_fill_pointer(env, v);
    
    return(v->vector.fillp);
  }
  
  static inline mkcl_index mkcl_string_fill_pointer(MKCL, mkcl_object v)
  {
    if (mkcl_unlikely(!(MKCL_STRINGP(v) && v->string.hasfillp)))
      mkcl_FEtype_error_string_with_fill_pointer(env, v);
    
    return(v->string.fillp);
  }
  
  static inline mkcl_index mkcl_base_string_fill_pointer(MKCL, mkcl_object v)
  {
    if (mkcl_unlikely(!(MKCL_BASE_STRING_P(v) && v->base_string.hasfillp)))
      mkcl_FEtype_error_base_string_with_fill_pointer(env, v);
    
    return(v->base_string.fillp);
  }

  extern MKCL_API mkcl_index mkcl_ensure_index_for_fill_pointer_set(MKCL, mkcl_index fillp, mkcl_index dim);

  static inline mkcl_index mkcl_vector_fill_pointer_set(MKCL, mkcl_object v, mkcl_index fillp)
  {
    extern const mkcl_object mk_si_fill_pointer_set_symbol;

    if (mkcl_unlikely(!(MKCL_VECTORP(v) && v->vector.hasfillp)))
      mkcl_FEtype_error_vector_with_fill_pointer(env, v);

    if (mkcl_likely(fillp <= v->vector.dim))
      v->vector.fillp = fillp;
    else
      v->vector.fillp = mkcl_ensure_index_for_fill_pointer_set(env, fillp, v->vector.dim);

    return fillp;
  }

  static inline mkcl_index mkcl_string_fill_pointer_set(MKCL, mkcl_object v, mkcl_index fillp)
  {
    extern const mkcl_object mk_si_fill_pointer_set_symbol;

    if (mkcl_unlikely(!(MKCL_STRINGP(v) && v->string.hasfillp)))
      mkcl_FEtype_error_string_with_fill_pointer(env, v);

    if (mkcl_likely(fillp <= v->string.dim))
      v->string.fillp = fillp;
    else
      v->string.fillp = mkcl_ensure_index_for_fill_pointer_set(env, fillp, v->string.dim);

    return fillp;
  }

  static inline mkcl_index mkcl_base_string_fill_pointer_set(MKCL, mkcl_object v, mkcl_index fillp)
  {
    extern const mkcl_object mk_si_fill_pointer_set_symbol;

    if (mkcl_unlikely(!(MKCL_BASE_STRING_P(v) && v->base_string.hasfillp)))
      mkcl_FEtype_error_base_string_with_fill_pointer(env, v);

    if (mkcl_likely(fillp <= v->base_string.dim))
      v->base_string.fillp = fillp;
    else
      v->base_string.fillp = mkcl_ensure_index_for_fill_pointer_set(env, fillp, v->base_string.dim);

    return fillp;
  }

  static inline mkcl_index mkcl_array_total_size(MKCL, mkcl_object a)
  {
    if (mkcl_unlikely(!MKCL_ARRAYP(a))) mkcl_FEtype_error_array(env, a);
    return(a->array.dim);
  }

  static inline mkcl_index mkcl_vector_total_size(MKCL, mkcl_object a)
  {
    if (mkcl_unlikely(!MKCL_VECTORP(a))) mkcl_FEtype_error_vector(env, a);
    return(a->vector.dim);
  }

  static inline mkcl_index mkcl_string_total_size(MKCL, mkcl_object a)
  {
    if (mkcl_unlikely(!MKCL_STRINGP(a))) mkcl_FEtype_error_string(env, a);
    return(a->string.dim);
  }

  static inline mkcl_index mkcl_base_string_total_size(MKCL, mkcl_object a)
  {
    if (mkcl_unlikely(!MKCL_BASE_STRING_P(a))) mkcl_FEtype_error_base_string(env, a);
    return(a->base_string.dim);
  }

  extern MKCL_API mkcl_object mkcl_ensure_vector_type_for_vector_push(MKCL, mkcl_object vec);
  extern MKCL_API mkcl_object mkcl_ensure_string_type_for_vector_push(MKCL, mkcl_object s);
  extern MKCL_API mkcl_object mkcl_ensure_base_string_type_for_vector_push(MKCL, mkcl_object s);

  static inline mkcl_object mkcl_vector_push(MKCL, mkcl_object vec, mkcl_object new_element)
  {
    while (!MKCL_VECTORP(vec))
      vec = mkcl_ensure_vector_type_for_vector_push(env, vec);

    const mkcl_index fillp = vec->vector.fillp;

    if (mkcl_unlikely(fillp >= vec->vector.dim)) return mk_cl_Cnil;

    vec->vector.set(env, vec, fillp, new_element);
    vec->vector.fillp = fillp + 1;
    return MKCL_MAKE_FIXNUM(fillp);
  }

  static inline mkcl_object mkcl_base_string_push(MKCL, mkcl_object s, mkcl_base_char c)
  {
    while (!MKCL_BASE_STRING_P(s)) s = mkcl_ensure_base_string_type_for_vector_push(env, s);

    mkcl_base_char * const self = s->base_string.self;
    const mkcl_index fillp = s->base_string.fillp;
    if (mkcl_unlikely(fillp >= s->base_string.dim))
      return mk_cl_Cnil;
    else
      {
	const mkcl_index new_fillp = fillp + 1;
	self[new_fillp] = 0; /* Make sure the string is NULL terminated for C sake. */
	self[fillp] = c;
	s->base_string.fillp = new_fillp;
	return MKCL_MAKE_FIXNUM(fillp);
      }
  }

  static inline mkcl_object mkcl_string_push(MKCL, mkcl_object s, mkcl_base_char c)
  {
    for (;;)
      switch (mkcl_type_of(s)) {
      case mkcl_t_base_string: {
	mkcl_base_char * const self = s->base_string.self;
	const mkcl_index fillp = s->base_string.fillp;
	if (mkcl_unlikely(fillp >= s->base_string.dim))
	  return mk_cl_Cnil;
	else
	  {
	    const mkcl_index new_fillp = fillp + 1;
	    self[new_fillp] = 0; /* Make sure the string is NULL terminated for C sake. */
	    self[fillp] = c;
	    s->base_string.fillp = new_fillp;
	    return MKCL_MAKE_FIXNUM(fillp);
	  }
      }
      case mkcl_t_string: {
	mkcl_character * const self = s->string.self;
	const mkcl_index fillp = s->string.fillp;
	if (mkcl_unlikely(fillp >= s->string.dim))
	  return mk_cl_Cnil;
	else
	  {
	    const mkcl_index new_fillp = fillp + 1;
	    self[new_fillp] = 0; /* Make sure the string is NULL terminated for C sake. */
	    self[fillp] = c;
	    s->string.fillp = new_fillp;
	    return MKCL_MAKE_FIXNUM(fillp);
	  }
      }
      default:
	s = mkcl_ensure_string_type_for_vector_push(env, s);
	break;
      }	
  }

  extern MKCL_API mkcl_base_char * mkcl_extend_base_string(MKCL, mkcl_object s);
  extern MKCL_API mkcl_character * mkcl_extend_string(MKCL, mkcl_object s);
  extern MKCL_API union mkcl_array_data mkcl_extend_vector(MKCL, mkcl_object s);

  extern MKCL_API mkcl_object mkcl_ensure_vector_type_for_vector_push_extend(MKCL, mkcl_object vec);
  extern MKCL_API mkcl_object mkcl_ensure_string_type_for_vector_push_extend(MKCL, mkcl_object s);
  extern MKCL_API mkcl_object mkcl_ensure_base_string_type_for_vector_push_extend(MKCL, mkcl_object s);

  static inline mkcl_index mkcl_vector_push_extend(MKCL, mkcl_object vec, mkcl_object new_element)
  {
    while (!MKCL_VECTORP(vec))
      vec = mkcl_ensure_vector_type_for_vector_push_extend(env, vec);

    const mkcl_index fillp = vec->vector.fillp;

    if (mkcl_unlikely(fillp >= vec->vector.dim)) mkcl_extend_vector(env, vec);

    vec->vector.set(env, vec, fillp, new_element);
    vec->vector.fillp = fillp + 1;
    return fillp;
  }

  static inline mkcl_index mkcl_base_string_push_extend(MKCL, mkcl_object s, mkcl_base_char c)
  {
    while (!MKCL_BASE_STRING_P(s))
      s = mkcl_ensure_base_string_type_for_vector_push_extend(env, s);

    const mkcl_index fillp = s->base_string.fillp;
    const mkcl_index new_fillp = fillp + 1;
    mkcl_base_char * const self
      = (mkcl_unlikely(fillp >= s->base_string.dim) ? mkcl_extend_base_string(env, s) : s->base_string.self);

    self[new_fillp] = 0; /* Make sure the string is NULL terminated for C sake. */
    self[fillp] = c;
    s->base_string.fillp = new_fillp;
    return fillp;
  }

  static inline mkcl_index mkcl_string_push_extend(MKCL, mkcl_object s, mkcl_character c)
  {
    for (;;)
      switch(mkcl_type_of(s)) {
      case mkcl_t_base_string: {
	const mkcl_index fillp = s->base_string.fillp;
	const mkcl_index new_fillp = fillp + 1;
	mkcl_base_char * const self
	  = (mkcl_unlikely(fillp >= s->base_string.dim) ? mkcl_extend_base_string(env, s) : s->base_string.self);

	self[new_fillp] = 0; /* Make sure the string is NULL terminated for C sake. */
	self[fillp] = c;
	s->base_string.fillp = new_fillp;
	return fillp;
      }
      case mkcl_t_string: {
	const mkcl_index fillp = s->string.fillp;
	const mkcl_index new_fillp = fillp + 1;
	mkcl_character * const self
	  = (mkcl_unlikely(fillp >= s->string.dim) ? mkcl_extend_string(env, s) : s->string.self);

	self[new_fillp] = 0; /* Make sure the string is NULL terminated for C sake. */
	self[fillp] = c;
	s->string.fillp = new_fillp;
	return fillp;
      }
      default:
	s = mkcl_ensure_string_type_for_vector_push_extend(env, s);
	break;
      }
  }

  /*********************************/

  extern MKCL_API bool _mkcl_structure_subtypep(mkcl_object x, mkcl_object y);
  extern MKCL_API void mkcl_FEtype_error_structure_index(MKCL, mkcl_object s, mkcl_object ndx);

  static inline mkcl_object mkcl_structure_ref(MKCL, mkcl_object x, mkcl_object name, mkcl_index n)
  {
    if (mkcl_unlikely(!(mkcl_type_of(x) == MKCL_T_STRUCTURE
			&& (MKCL_SNAME(x) == name || _mkcl_structure_subtypep(MKCL_STYPE(x), name)))))
      mkcl_FEwrong_type_argument(env, name, x);
    else if (mkcl_unlikely(n >= MKCL_SLENGTH(x)))
      mkcl_FEtype_error_structure_index(env, x, MKCL_MAKE_FIXNUM(n));

    return(MKCL_SLOT(x, n));
  }

  static inline mkcl_object mkcl_structure_set(MKCL, mkcl_object x, mkcl_object name, mkcl_index n, mkcl_object v)
  {
    if (mkcl_unlikely(!(mkcl_type_of(x) == MKCL_T_STRUCTURE
			&& (MKCL_SNAME(x) == name || _mkcl_structure_subtypep(MKCL_STYPE(x), name)))))
      mkcl_FEwrong_type_argument(env, name, x);
    else if (mkcl_unlikely(n >= MKCL_SLENGTH(x)))
      mkcl_FEtype_error_structure_index(env, x, MKCL_MAKE_FIXNUM(n));

    { MKCL_SLOT(x, n) = v; return(v); }
  }


  extern MKCL_API void mkcl_FEtype_error_instance_index(MKCL, mkcl_object instance, mkcl_object ndx);

  static inline mkcl_object mkcl_instance_ref(MKCL, mkcl_object x, mkcl_word i)
  {
    if (mkcl_unlikely(!MKCL_INSTANCEP(x)))
      mkcl_FEtype_error_instance(env, x);
    else if (mkcl_unlikely(i < 0 || i >= x->instance.length))
      mkcl_FEtype_error_instance_index(env, x, MKCL_MAKE_FIXNUM(i));

    return(x->instance.slots[i]);
  }

  static inline mkcl_object mkcl_instance_set(MKCL, mkcl_object x, mkcl_word i, mkcl_object v)
  {
    if (mkcl_unlikely(!MKCL_INSTANCEP(x)))
      mkcl_FEtype_error_instance(env, x);
    else if (mkcl_unlikely(i < 0 || i >= x->instance.length))
      mkcl_FEtype_error_instance_index(env, x, MKCL_MAKE_FIXNUM(i));

    { x->instance.slots[i] = v; return(v); }
  }

  /*********************************/

#if MKCL_WINDOWS
  extern MKCL_API char * mkcl_handle_debug_name(MKCL, char * prefix);
#endif

#define MKCL_STRINGIZE(token) #token

#ifdef __cplusplus
}
#endif

#endif /* MKCL_EXTERNAL_H */
