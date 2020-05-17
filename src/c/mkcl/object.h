/*
    object.h  -- Data structure definitions.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2016, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file './Copyright' for full details.
*/

#ifndef MKCL_OBJECT_H
#define MKCL_OBJECT_H

/* Each time a new, incompatible, FASL layout is created the value
   of MKCL_FASL_VERSION should be bumped by 1 in order to identify
   the new layout family.
   The structures here below, the content of mkcl_core, the content
   of mkcl_root_symbols and the set of MKCL's "extern" C symbols
   are the key inputs to the FASL layout.  A change
   in the layout of any of them is a change of FASL layout.
   Some controlled changes can be made that maintain their
   layout upward compatibility. This usually requires the
   avoidance of any deletion, and modification of the structure
   from the tail end of it only.
   Changes in FASL layout should be rare and well motivated
   since they destroy binary upward compatibility of FASLs.
*/
#define MKCL_FASL_VERSION 6 /* for MKCL 1.1.10 */

#ifdef __cplusplus
extern "C" {
#endif

#define MKCL_NB_ELEMS(a) (sizeof(a)/sizeof(a[0]))

  /*
    Integer and boolean types (see config.h)
  */

#define	TRUE		1	/*  boolean true value  */
#define	FALSE		0	/*  boolean false value  */

#if !defined(__cplusplus)
#include <stdbool.h> /* bool has a standard definition in C99. */
#endif

  typedef unsigned char mkcl_byte;

  /*
    Implementation types.

    The lower 2 bits are used as a discriminant to distinguish immediate
    values (nil, character, fixnum) from memory objects (all other types).
    In the object type bitfield (upper 6 bits of the type byte)
    the numerical values have been assigned in groups of 8 or subgroups
    of 4 (0-7 for numbers, 12-15 for vectors, 16-23 for functions).
  */
  typedef enum {
    mkcl_t_start = 0,
    mkcl_t_null = 0,
    mkcl_t_object = 1,
    mkcl_t_character = 2,  /* immediate character */
    mkcl_t_fixnum = 3,	   /* immediate fixnum */     /* number */
    /* The most specific numeric types come first. Assumed by
       some routines, like mk_cl_expt */
    mkcl_t_bignum             = ( 1 << 2 ) | mkcl_t_object,   /* number */ /* bin0, 0 to 7 */
    mkcl_t_ratio              = ( 2 << 2 ) | mkcl_t_object,   /* number */
    mkcl_t_singlefloat        = ( 3 << 2 ) | mkcl_t_object,   /* number */
    mkcl_t_doublefloat        = ( 4 << 2 ) | mkcl_t_object,   /* number */
    mkcl_t_longfloat          = ( 5 << 2 ) | mkcl_t_object,   /* number */
    mkcl_t_complex            = ( 6 << 2 ) | mkcl_t_object,   /* number */
    mkcl_t_reserved_bin0_7    = ( 7 << 2 ) | mkcl_t_object,   /* number */
    mkcl_t_symbol             = ( 8 << 2 ) | mkcl_t_object,   /* bin1, 8 to 15 */
    mkcl_t_cons               = ( 9 << 2 ) | mkcl_t_object,
    mkcl_t_package            = ( 10 << 2 ) | mkcl_t_object,
    mkcl_t_array              = ( 11 << 2 ) | mkcl_t_object,
    mkcl_t_vector             = ( 12 << 2 ) | mkcl_t_object,  /* vectorial */
    mkcl_t_string             = ( 13 << 2 ) | mkcl_t_object,  /* vectorial */
    mkcl_t_base_string        = ( 14 << 2 ) | mkcl_t_object,  /* vectorial */
    mkcl_t_bitvector          = ( 15 << 2 ) | mkcl_t_object,  /* vectorial */
    mkcl_t_cfun               = ( 16 << 2 ) | mkcl_t_object,  /* functional */ /* bin2, 16 to 23 */
    mkcl_t_cclosure           = ( 17 << 2 ) | mkcl_t_object,  /* functional */
    mkcl_t_bytecode           = ( 18 << 2 ) | mkcl_t_object,  /* functional */
    mkcl_t_bclosure           = ( 19 << 2 ) | mkcl_t_object,  /* functional */
    mkcl_t_instance           = ( 20 << 2 ) | mkcl_t_object,  /* functional */
    mkcl_t_old_cfunfixed      = ( 21 << 2 ) | mkcl_t_object,  /* functional */ /* deprecated */
    mkcl_t_reserved_bin2_6    = ( 22 << 2 ) | mkcl_t_object,  /* functional */
    mkcl_t_reserved_bin2_7    = ( 23 << 2 ) | mkcl_t_object,  /* functional */
    mkcl_t_hashtable          = ( 24 << 2 ) | mkcl_t_object,  /* bin3, 24 to 31 */
    mkcl_t_stream             = ( 25 << 2 ) | mkcl_t_object,
    mkcl_t_random             = ( 26 << 2 ) | mkcl_t_object,
    mkcl_t_readtable          = ( 27 << 2 ) | mkcl_t_object,
    mkcl_t_pathname           = ( 28 << 2 ) | mkcl_t_object,
    mkcl_t_structure          = ( 29 << 2 ) | mkcl_t_object,
    mkcl_t_thread             = ( 30 << 2 ) | mkcl_t_object,
    mkcl_t_lock               = ( 31 << 2 ) | mkcl_t_object,
    mkcl_t_rwlock             = ( 32 << 2 ) | mkcl_t_object,  /* bin4, 32 to 39 */
    mkcl_t_semaphore          = ( 33 << 2 ) | mkcl_t_object,
    mkcl_t_condition_variable = ( 34 << 2 ) | mkcl_t_object,
    mkcl_t_codeblock          = ( 35 << 2 ) | mkcl_t_object,
    mkcl_t_foreign            = ( 36 << 2 ) | mkcl_t_object,
    mkcl_t_temp_stack_frame   = ( 37 << 2 ) | mkcl_t_object,
    mkcl_t_cdisplay           = ( 38 << 2 ) | mkcl_t_object,
    mkcl_t_clevel_block       = ( 39 << 2 ) | mkcl_t_object,
    mkcl_t_cmp_dbg_lex_level  = ( 40 << 2 ) | mkcl_t_object,  /* bin5, 40 to 47 */
    mkcl_t_pin_bag            = ( 41 << 2 ) | mkcl_t_object,  /* not really used yet. JCB */
    mkcl_t_pin                = ( 42 << 2 ) | mkcl_t_object,  /* not really used yet. JCB */
#if 1
    mkcl_t_UTF_8              = ( 43 << 2 ) | mkcl_t_object,
    mkcl_t_UTF_16             = ( 44 << 2 ) | mkcl_t_object,
    mkcl_t_process            = ( 45 << 2 ) | mkcl_t_object,
#else
    mkcl_t_reserved_bin5_3    = ( 43 << 2 ) | mkcl_t_object,
    mkcl_t_reserved_bin5_4    = ( 44 << 2 ) | mkcl_t_object,
    mkcl_t_reserved_bin5_5    = ( 45 << 2 ) | mkcl_t_object,
#endif
    mkcl_t_reserved_bin5_6    = ( 46 << 2 ) | mkcl_t_object,
    mkcl_t_reserved_bin5_7    = ( 47 << 2 ) | mkcl_t_object,
    /* bin6, 48 to 55 */
    /* bin7, 56 to 63 */
    /* Marks the end of the range of valid type tags. */
    mkcl_t_end                = ( 63 << 2 ) | mkcl_t_object
  } mkcl_type;


  struct mkcl_env_struct;
#define __MKCL struct mkcl_env_struct * const env

  /*
    Definition of the type of LISP objects.
  */
  typedef union mkcl_lispunion * mkcl_object;
  typedef const union mkcl_lispunion * mkcl_const_object;
  typedef mkcl_object mkcl_return;
  typedef mkcl_word mkcl_narg;
  typedef mkcl_object (*mkcl_objectfn)(__MKCL, mkcl_narg narg, ...);
  typedef mkcl_object (*mkcl_objectfn_fixed)();

  /*
    OBJect NULL value.
    It should not coincide with any legal object value.
  */

  /* This used to be NULL (a.k.a 0) but we want 
     to reserve NULL to be a special representation
     of the "nil" value. The value ~0x03 is also an
     immediate value that can designate no legal object. */
#define MKCL_OBJNULL         ((mkcl_object) ~((uintptr_t)0x03))

  /*
    Definition of each implementation type.
  */

#define MKCL_TAG_BITS 2
#define MKCL_IMMEDIATE(o)		(((mkcl_index)(o)) & 3)
#define MKCL_IMMEDIATE_TAG		3

  /* Immediate fixnums:		*/
#define MKCL_FIXNUM_TAG		mkcl_t_fixnum
#if __clang__
# define MKCL_MAKE_FIXNUM(n)	((mkcl_object)(((mkcl_word)(n) * 4) | MKCL_FIXNUM_TAG))
# define mkcl_fixnum_to_word(obje) ((((mkcl_word)(obje)) & ~mkcl_t_fixnum) / 4)
#else
# define MKCL_MAKE_FIXNUM(n)	((mkcl_object)(((mkcl_word)(n) << 2) | MKCL_FIXNUM_TAG))
# define mkcl_fixnum_to_word(obje) (((mkcl_word)(obje)) >> 2)
#endif
#define MKCL_FIXNUM_MINUSP(n)	((mkcl_word)(n) < 0)
#define MKCL_FIXNUM_PLUSP(n)	((mkcl_word)(n) >= (mkcl_word)MKCL_MAKE_FIXNUM(0))
#define MKCL_FIXNUMP(o)		(MKCL_IMMEDIATE(o) == mkcl_t_fixnum)

  
  /* Immediate characters:	*/
#define MKCL_CHARACTER_TAG	  mkcl_t_character
#define MKCL_CHARACTERP(o)	  (MKCL_IMMEDIATE(o) == mkcl_t_character)

# define MKCL_BASE_CHAR_P(c)      (MKCL_CHARACTER_TAG == (((mkcl_index)(c)) & 0xffff0003))
# define MKCL_BASE_CHAR_CODE_P(x) (((mkcl_index)(x)) <= 255)
# define MKCL_CODE_CHAR(c)	  ((mkcl_object) ((((mkcl_index) (c)) << 8) | MKCL_CHARACTER_TAG))
# define MKCL_CHAR_CODE(obje)	  (((mkcl_index)(obje)) >> 8)


#define MKCL_CHAR_CODE_RETURN	13   /* This is ASCII character CR. */
#define MKCL_CHAR_CODE_NEWLINE	10   /* This is ASCII character LF. */
#define MKCL_CHAR_CODE_LINEFEED	10   /* This is ASCII character LF. */


#define MKCL_NUMBER_TYPE_P(t)	(t >= mkcl_t_fixnum && t <= mkcl_t_complex)
#define MKCL_REAL_TYPE_P(t)	(t >= mkcl_t_fixnum && t < mkcl_t_complex)
#define MKCL_ARRAY_TYPE_P(t)	(t >= mkcl_t_array && t <= mkcl_t_bitvector)
#define MKCL_ARRAYP(x)		((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) && (x)->d.t >= mkcl_t_array && (x)->d.t <= mkcl_t_bitvector)
#define MKCL_VECTOR_TYPE_P(t)   (t >= mkcl_t_vector && t <= mkcl_t_bitvector)
#define MKCL_VECTORP(x)		((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) && (x)->d.t >= mkcl_t_vector && (x)->d.t <= mkcl_t_bitvector)

#define MKCL_BIT_VECTOR_P(x)     ((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) && ((x)->d.t == mkcl_t_bitvector))
#define MKCL_STRINGP(x)	         ((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) \
				  && ((x)->d.t == mkcl_t_base_string || (x)->d.t == mkcl_t_string))
#define MKCL_CHARACTER_STRING_P(x) ((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) && (x)->d.t == mkcl_t_string)
#define MKCL_BASE_STRING_P(x) 	 ((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) && ((x)->d.t == mkcl_t_base_string))
#define MKCL_UTF_8_P(x) 	 ((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) && ((x)->d.t == mkcl_t_UTF_8))
#define MKCL_UTF_16_P(x) 	 ((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) && ((x)->d.t == mkcl_t_UTF_16))
#define MKCL_HASH_TABLE_P(x)     ((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) && ((x)->d.t == mkcl_t_hashtable))
#define MKCL_BIGNUMP(x)          ((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) && ((x)->d.t == mkcl_t_bignum))
#define MKCL_RATIOP(x)           ((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) && ((x)->d.t == mkcl_t_ratio))
#define MKCL_COMPLEXP(x)         ((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) && ((x)->d.t == mkcl_t_complex))
#define MKCL_RANDOM_STATE_P(x)   ((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) && ((x)->d.t == mkcl_t_random))
#define MKCL_SINGLE_FLOAT_P(x)   ((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) && ((x)->d.t == mkcl_t_singlefloat))
#define MKCL_DOUBLE_FLOAT_P(x)   ((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) && ((x)->d.t == mkcl_t_doublefloat))
#ifdef MKCL_LONG_FLOAT
#define MKCL_LONG_FLOAT_P(x)     ((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) && ((x)->d.t == mkcl_t_longfloat))
#endif
#define MKCL_PACKAGEP(x)         ((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) && ((x)->d.t == mkcl_t_package))
#define MKCL_PATHNAMEP(x)        ((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) && ((x)->d.t == mkcl_t_pathname))
#define MKCL_READTABLEP(x)       ((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) && ((x)->d.t == mkcl_t_readtable))
#define MKCL_FOREIGN_P(x)        ((MKCL_IMMEDIATE(x) == 0) && !mkcl_Null(x) && ((x)->d.t == mkcl_t_foreign))


  /* Note that NO atomicity can be guaranteed on header fields. Therefore, MKCL_HEADER[234] may contain only
     one mutable field with the other fields being constant for the entire life of the object. JCB */
#ifdef MKCL_FORWARD
# define MKCL_HEADER			uint8_t t, m, _pad0, _pad1; mkcl_object f
# define MKCL_HEADER1(field)		uint8_t t, m; int8_t field, _pad; mkcl_object f
# define MKCL_HEADER2(field1,field2)	uint8_t t, m; int8_t field1, field2; mkcl_object f
# define MKCL_HEADER3(field1,flag2,flag3) uint8_t t, m; int8_t field1; unsigned flag2:4, flag3:4; mkcl_object f
# define MKCL_HEADER4(field1,flag2,flag3,flag4) uint8_t t, m; int8_t field1; unsigned flag2:4, flag3:2, flag4:2; mkcl_object f
#else
# define MKCL_HEADER			uint8_t t, m, _pad0, _pad1
# define MKCL_HEADER1(field)		uint8_t t, m; int8_t field, _pad
# define MKCL_HEADER2(field1,field2)	uint8_t t, m; int8_t field1, field2
# define MKCL_HEADER3(field1,flag2,flag3) uint8_t t, m; int8_t field1; unsigned flag2:4, flag3:4
# define MKCL_HEADER4(field1,flag2,flag3,flag4) uint8_t t, m; int8_t field1; unsigned flag2:4, flag3:2, flag4:2
#endif

  struct mkcl_singlefloat {
    MKCL_HEADER;
    float SFVAL;	/*  singlefloat value  */
  };
#define	mkcl_single_float(obje)	((obje)->SF.SFVAL)

  struct mkcl_doublefloat {
    MKCL_HEADER;
    double DFVAL;	/*  doublefloat value  */
  };
#define	mkcl_double_float(obje)	((obje)->DF.DFVAL)

  struct mkcl_long_float {
    MKCL_HEADER;
    long double value;
  };
#define mkcl_long_float(o) ((o)->longfloat.value)


  struct mkcl_bignum {
    MKCL_HEADER;
    mpz_t big_num;
  };

  struct mkcl_ratio {
    MKCL_HEADER;
    mkcl_object den;		/*  denominator, must be an integer  */
    mkcl_object num;		/*  numerator, must be an integer  */
  };

  struct mkcl_complex {
    MKCL_HEADER;
    mkcl_object real;		/*  real part, must be a real number  */
    mkcl_object imag;		/*  imaginary part, must be a real number  */
  };

  enum mkcl_stype {		/*  symbol type  */
    mkcl_stp_ordinary = 0,
    mkcl_stp_constant = 1,
    mkcl_stp_special = 2,
    mkcl_stp_macro = 4,
    mkcl_stp_special_form = 8
  };

#define mkcl_check_symbol(e, x) if (!MKCL_SYMBOLP(x)) mkcl_FEtype_error_symbol(e, x);


#define mk_cl_Cnil              ((mkcl_object) NULL)

#define	mk_cl_Cnil_symbol	((mkcl_object) (mkcl_root_symbols+0))
#define	mk_cl_Ct		((mkcl_object) (mkcl_root_symbols+1))
#define MKCL_UNBOUND		((mkcl_object) (mkcl_root_symbols+2))
#define MKCL_PROTECT_TAG	((mkcl_object) (mkcl_root_symbols+3))

#define MKCL_NOT_A_SPECIAL_INDEX (~((mkcl_index)0))

  struct mkcl_symbol {
    MKCL_HEADER1(stype); /* symbol type, see enum mkcl_stype just here above. */
    mkcl_object value;	               /* global value of the symbol */
    mkcl_object gfdef;	               /* global function definition */
                                       /* For a macro, its expansion function is to be stored. */
    mkcl_object plist;	               /* property list */
    mkcl_object name;		       /* print name */
    mkcl_object hpack;	               /* home package, mk_cl_Cnil for uninterned symbols */
    mkcl_object properly_named_class;
    mkcl_object sys_plist;             /* system property list */
    mkcl_index special_index;
  };
#define MKCL_SYM_FUN(sym)	((sym)->symbol.gfdef)

  struct mkcl_package {
    MKCL_HEADER1(closed);
    mkcl_object name;		/*  package name, a string  */
    mkcl_object nicknames;	/*  nicknames, list of strings  */
    mkcl_object shadowings;	/*  shadowing symbol list  */
    mkcl_object uses;		/*  use-list of packages  */
    mkcl_object usedby;	        /*  used-by-list of packages  */
    mkcl_object internal;	/*  hashtable for internal symbols  */
    mkcl_object external;	/*  hashtable for external symbols  */
#if MKCL_WINDOWS
    CRITICAL_SECTION lock;	/*  thread safe packages  */
#else
    pthread_mutex_t lock;	/*  thread safe packages  */
#endif
  };

  /*
    The values returned by intern and find_symbol.
    File_symbol may return 0.
  */
#define	MKCL_SYMBOL_IS_INTERNAL	1
#define	MKCL_SYMBOL_IS_EXTERNAL	2
#define	MKCL_SYMBOL_IS_INHERITED 3


  /***********************/

#define mkcl_Null(x)	((x)==mk_cl_Cnil)

#define MKCL_REF_P(x) (MKCL_IMMEDIATE(x) == 0)

#define MKCL_OBJTYPE(x) ((x)->d.t)

#ifdef MKCL_FORWARD
# if WITH_FORWARD_SLOT
#  define MKCL_FORWARDP(x) ((x) != (x)->d.f)
#  define MKCL_FORWARD(x) ((x)->d.f)

#  define MKCL_EQ(a,b)							\
  ((a) == (b) ? true							\
   : ((MKCL_FORWARD(a) == (b)) ? true					\
      : (((a) == MKCL_FORWARD(b)) ? true				\
	 : false)))

#  define MKCL_EQ(a,b) (((a) == (b)) || (MKCL_FORWARD(a) == (b)) || ((a) == MKCL_FORWARD(b)))
# else /* WITH_FORWARD_SLOT */
#  define MKCL_FORWARDP(x) (*((intptr_t *) (x)) & 0x04)
#  define MKCL_FORWARD(x) ((mkcl_object) (*((intptr_t *) (x)) - 0x04))
#  define MKCL_EQ(a,b)					     \
  ((a) == (b)) || (MKCL_FORWARDP(a)			     \
		   ? (MKCL_FORWARDP(b)			     \
		      ? (MKCL_FORWARD(a) == MKCL_FORWARD(b)) \
		      : (MKCL_FORWARD(a) == (b)))	     \
		   : (MKCL_FORWARDP(b)			     \
		      ? ((a) == MKCL_FORWARD(b))	     \
		      : false))
# endif /* WITH_FORWARD_SLOT */
#else /* def MKCL_FORWARD */
# define MKCL_EQ(a,b) ((a) == (b))
#endif /* def MKCL_FORWARD */

#define MKCL_LISTP(x)   (MKCL_REF_P(x) && (mkcl_Null(x) || (MKCL_OBJTYPE(x) == mkcl_t_cons)))
#define MKCL_CONSP(x)   (MKCL_REF_P(x) && !mkcl_Null(x) && (MKCL_OBJTYPE(x) == mkcl_t_cons))
#define MKCL_F_CONSP(x) (MKCL_REF_P(x) && (MKCL_OBJTYPE(x) == mkcl_t_cons))
#define MKCL_ATOM(x)	(!MKCL_CONSP(x))
#define MKCL_SYMBOLP(x) (MKCL_REF_P(x) && (mkcl_Null(x) || (MKCL_OBJTYPE(x) == mkcl_t_symbol)))


#define MKCL_CONS_CAR(x)	((x)->cons.car)
#define MKCL_CONS_CDR(x)	((x)->cons.cdr)
#define MKCL_RPLACA(x,v)	(MKCL_CONS_CAR(x)=(v))
#define MKCL_RPLACD(x,v)	(MKCL_CONS_CDR(x)=(v))

  struct mkcl_cons {
    MKCL_HEADER;
    mkcl_object car;		/*  car  */
    mkcl_object cdr;		/*  cdr  */
    mkcl_object _pad;
  };

  /***********************/


  enum mkcl_httest {		/*  hash table key test function  */
    mkcl_htt_eq,		/*  eq  */
    mkcl_htt_eql,		/*  eql  */
    mkcl_htt_equal,		/*  equal  */
    mkcl_htt_equalp,		/*  equalp  */
    mkcl_htt_package		/*  symbol hash  */
  };

  struct mkcl_hashtable_entry {	/*  hash table entry  */
    struct mkcl_hashtable_entry * next;
    mkcl_object key;		/*  key  */
    mkcl_object value;	        /*  value  */
  };

  struct mkcl_hashtable {	/*  hash table header  */
    MKCL_HEADER2(test,lockable);
    struct mkcl_hashtable_entry **data; /*  pointer to a vector of entry chains */
    struct mkcl_hashtable_entry * (*search_fun)(__MKCL, mkcl_object key, mkcl_object hashtable);
    mkcl_index entries;	        /*  number of entries  */
    mkcl_index size;		/*  hash table size  */
    mkcl_object rehash_size;	/*  rehash size  */
    mkcl_object threshold;	/*  rehash threshold  */
    mkcl_index factor_of_16th;  /*  numerator of a ratio of 16th derived from threshold. */

    struct mkcl_hashtable_entry * free_bucket; /* a free list. */

#ifdef HASHTABLE_STATS
    long nb_searches;
    long probes;
    long shortest_probe_chain;
    long longest_probe_chain;
    long shortest_failed_probe_chain;
    long longest_failed_probe_chain;
    long longest_static_chain;
#endif
  };

  typedef enum {		/*  array element type  */
    mkcl_aet_object = 0,	/*  t                */
    mkcl_aet_fixnum,            /*  fixnum           */
    mkcl_aet_sf,		/*  single-float     */
    mkcl_aet_df,		/*  double-float     */
    mkcl_aet_bit,		/*  bit              */
    mkcl_aet_word,		/*  mkcl_word        */
    mkcl_aet_index,		/*  mkcl_index       */
    /* Below here, list types accepted by streams (i.e. OPEN) */
    mkcl_aet_b8,		/*  natural8	     */
    mkcl_aet_i8,		/*  integer8	     */
    mkcl_aet_b16,
    mkcl_aet_i16,
    mkcl_aet_b32,
    mkcl_aet_i32,
    mkcl_aet_b64,
    mkcl_aet_i64,
    mkcl_aet_ch,		/*  character        */
    mkcl_aet_bc,		/*  base-char        */
    mkcl_aet_nil,
    mkcl_aet_last_type = mkcl_aet_nil
  } mkcl_elttype;

  typedef unsigned char mkcl_char8;
  typedef unsigned short mkcl_char16;
  typedef unsigned int mkcl_char32;

  typedef mkcl_char8 mkcl_base_char;
  typedef uint32_t mkcl_character;

  union mkcl_array_data {
    mkcl_object    *t;
    mkcl_base_char *bc;
    mkcl_character *c;
    uint8_t        *b8;
    int8_t         *i8;
    mkcl_uint16_t  *b16;
    mkcl_int16_t   *i16;
    mkcl_uint32_t  *b32;
    mkcl_int32_t   *i32;
    mkcl_uint64_t  *b64;
    mkcl_int64_t   *i64;
    float          *sf;
    double         *df;
    mkcl_word      *word;
    mkcl_index     *index;
    mkcl_byte      *bit;
  };

#if CHAR_BIT == 8
# define mkcl_bit_bundle(v,i) (v[(i)>>3])
#else
# define mkcl_bit_bundle(v,i) (v[(i)/CHAR_BIT])
#endif

#if MKCL_LSB_FIRST && CHAR_BIT == 8
# define mkcl_bundle_bit_mask(i) (1 << ((i) & 0x07))
#elif CHAR_BIT == 8
# define mkcl_bundle_bit_mask(i) (0200 >> ((i) & 0x07))
#elif MKCL_LSB_FIRST
# define mkcl_bundle_bit_mask(i) (1 << ((i) % CHAR_BIT))
#else
# define mkcl_bundle_bit_mask(i) (0200 >> ((i) % CHAR_BIT))
#endif

  /* The uncanny similarities between the layout of mkcl_array and mkcl_vector
     is not a coincidence. More than one function counts on it in order to
     work properly! Caveat emptor! JCB
   */
  /* Note on "hasfillp": Vector field "hasfillp" must be strictly a boolean
     and not take any other values than either 0 (FALSE) or 1 (TRUE). That
     is in order to prevent any ambiguity with array field "rank" which can
     take values of 2 and more (up to MKCL_ARANKLIM).
   */

  struct mkcl_array {		/*  array header  */
				/*  adjustable flag  */
				/*  has-fill-pointer flag  */
    MKCL_HEADER2(adjustable,rank);
    mkcl_object displaced;	/*  displaced  */
    mkcl_index dim;		/*  dimension  */
    mkcl_index *dims;		/*  table of dimensions  */
    union mkcl_array_data self;	/*  pointer to the array  */
    mkcl_object (*elem)(__MKCL, mkcl_object array, mkcl_index i);
    mkcl_object (*set)(__MKCL, mkcl_object array, mkcl_index i, mkcl_object val);
    mkcl_elttype elttype;	/*  element type  */
    mkcl_index	bit_offset;	/*  bitvector displacement bit offset  */
  };

  struct mkcl_vector {		/*  vector header  */
				/*  adjustable flag  */
				/*  has-fill-pointer flag  */
    MKCL_HEADER2(adjustable,hasfillp);
    mkcl_object displaced;	/*  displaced  */
    mkcl_index dim;		/*  dimension  */
    mkcl_index fillp;		/*  fill pointer  */
				/*  For simple vectors,  */
				/*  v_fillp is equal to v_dim.  */
    union mkcl_array_data self;	/*  pointer to the vector  */
    mkcl_object (*elem)(__MKCL, mkcl_object array, mkcl_index i);
    mkcl_object (*set)(__MKCL, mkcl_object array, mkcl_index i, mkcl_object val);
    mkcl_elttype elttype;	/*  element type  */
    mkcl_index	bit_offset;	/*  bitvector displacement bit offset  */
  };

  /* Among others, function mkcl_string_push_extend() counts on the fact that
     mkcl_string and mkcl_base_string have the same slot layout
     in order to work! Caveat emptor! JCB
   */
  struct mkcl_base_string {	/*  string header  */
				/*  adjustable flag  */
				/*  has-fill-pointer flag  */
    MKCL_HEADER2(adjustable,hasfillp);
    mkcl_object displaced;	/*  displaced  */
    mkcl_index dim;       	/*  dimension  */
				/*  string length  */
    mkcl_index fillp;		/*  fill pointer  */
    mkcl_base_char *self;	/*  pointer to the string  */
    mkcl_object (*elem)(__MKCL, mkcl_object array, mkcl_index i);
    mkcl_object (*set)(__MKCL, mkcl_object array, mkcl_index i, mkcl_object val);
  };

  struct mkcl_string {		/*  string header  */
				/*  adjustable flag  */
				/*  has-fill-pointer flag  */
    MKCL_HEADER2(adjustable,hasfillp);
    mkcl_object displaced;	/*  displaced  */
    mkcl_index dim;       	/*  dimension  */
				/*  string length  */
    mkcl_index fillp;		/*  fill pointer  */
    mkcl_character *self;	/*  pointer to the string  */
    mkcl_object (*elem)(__MKCL, mkcl_object array, mkcl_index i);
    mkcl_object (*set)(__MKCL, mkcl_object array, mkcl_index i, mkcl_object val);
  };

  struct mkcl_UTF_8 {
    MKCL_HEADER;
    mkcl_index dim;       	/*  dimension  */
    mkcl_index fillp;		/*  fill pointer  */
    mkcl_char8 *self;	        /*  pointer to the string  */
  };

  struct mkcl_UTF_16 {
    MKCL_HEADER;
    mkcl_index dim;       	/*  dimension  */
    mkcl_index fillp;		/*  fill pointer  */
    mkcl_char16 *self;	        /*  pointer to the string  */
  };



  enum mkcl_smmode {		/*  stream mode  */
    mkcl_smm_input,		/*  input  */
    mkcl_smm_input_file,	/*  input  */
    mkcl_smm_output,		/*  output  */
    mkcl_smm_output_file,	/*  output  */
    mkcl_smm_io,		/*  input-output  */
    mkcl_smm_io_file,		/*  input-output  */
    mkcl_smm_input_socket,	/*  input socket  */
    mkcl_smm_output_socket,	/*  output socket  */
    mkcl_smm_io_socket,		/*  input-output socket  */
    mkcl_smm_synonym,		/*  synonym  */
    mkcl_smm_broadcast,		/*  broadcast  */
    mkcl_smm_concatenated,	/*  concatenated  */
    mkcl_smm_two_way,		/*  two way  */
    mkcl_smm_echo,		/*  echo  */
    mkcl_smm_string_input,	/*  string input  */
    mkcl_smm_string_output,	/*  string output  */
    mkcl_smm_probe		/*  probe (only used in open_stream())  */
  };

#define MKCL_STREAM_IS_C_STDIO_BASED_P(strm)	\
  ((strm)->stream.mode == mkcl_smm_output	\
   || (strm)->stream.mode == mkcl_smm_io	\
   || (strm)->stream.mode == mkcl_smm_input)
  
#define MKCL_STREAM_IS_FD_BASED_P(strm)			\
  ((strm)->stream.mode == mkcl_smm_output_file		\
   || (strm)->stream.mode == mkcl_smm_io_file		\
   || (strm)->stream.mode == mkcl_smm_input_file)
  
#define MKCL_STREAM_IS_COMPOSITE_P(strm)		\
  ((strm)->stream.mode == mkcl_smm_synonym		\
   || (strm)->stream.mode == mkcl_smm_broadcast		\
   || (strm)->stream.mode == mkcl_smm_concatenated	\
   || (strm)->stream.mode == mkcl_smm_two_way		\
   || (strm)->stream.mode == mkcl_smm_echo)


  struct mkcl_file_ops {
    mkcl_index (*write_octet)(__MKCL, mkcl_object strm, unsigned char *c, mkcl_index n);
    mkcl_index (*read_octet)(__MKCL, mkcl_object strm, unsigned char *c, mkcl_index n);

    void (*write_byte)(__MKCL, mkcl_object c, mkcl_object strm);
    mkcl_object (*read_byte)(__MKCL, mkcl_object strm);

    mkcl_character (*read_char)(__MKCL, mkcl_object strm);
    mkcl_character (*write_char)(__MKCL, mkcl_object strm, mkcl_character c);
    void (*unread_char)(__MKCL, mkcl_object strm, mkcl_character c);
    mkcl_character (*peek_char)(__MKCL, mkcl_object strm);

    mkcl_index (*read_vector)(__MKCL, mkcl_object strm, mkcl_object data, mkcl_index start, mkcl_index end);
    mkcl_index (*write_vector)(__MKCL, mkcl_object strm, mkcl_object data, mkcl_index start, mkcl_index end);

    int (*listen)(__MKCL, mkcl_object strm);
    void (*clear_input)(__MKCL, mkcl_object strm);
    void (*clear_output)(__MKCL, mkcl_object strm);
    void (*finish_output)(__MKCL, mkcl_object strm);
    void (*force_output)(__MKCL, mkcl_object strm);

    bool (*input_p)(__MKCL, mkcl_object strm);
    bool (*output_p)(__MKCL, mkcl_object strm);
    bool (*interactive_p)(__MKCL, mkcl_object strm);
    mkcl_object (*element_type)(__MKCL, mkcl_object strm);

    mkcl_object (*length)(__MKCL, mkcl_object strm);
    mkcl_object (*get_position)(__MKCL, mkcl_object strm);
    mkcl_object (*set_position)(__MKCL, mkcl_object strm, mkcl_object pos);
    int (*column)(__MKCL, mkcl_object strm);

    mkcl_object (*close)(__MKCL, mkcl_object strm);
  };


  typedef mkcl_index (*mkcl_eformat_encoder)(__MKCL, mkcl_object stream,
					     unsigned char *buffer, mkcl_character c);
  typedef mkcl_index (*mkcl_eformat_read_octet)(__MKCL, mkcl_object object,
						unsigned char *buffer, mkcl_index n);
  typedef mkcl_character (*mkcl_eformat_decoder)(__MKCL, mkcl_object stream,
						 mkcl_eformat_read_octet read_octet, mkcl_object source);

  struct mkcl_stream {
    MKCL_HEADER2(mode,closed);	/*  stream mode of enum smmode  */
				/*  closed stream?  */
    struct mkcl_file_ops *ops;  /*  dispatch table  */
    union {
#if 0
      FILE *stream;		/* ANSI C streams */
#else
      void *stream;		/* ANSI C streams */
#endif
      mkcl_word descriptor;	/* POSIX files */
    } file;
    mkcl_object object0;	/*  some object  */
    mkcl_object object1;	/*  some object */
    mkcl_object byte_stack;	/*  buffer for unread bytes  */
    mkcl_character last_char;	/*  last character read  */
    mkcl_character last_code[2];/*  actual composition of last character  */
    mkcl_word int0;		/*  some int  */
    mkcl_word int1;		/*  some int  */
    mkcl_index byte_size;	/*  size of byte in binary streams  */
    mkcl_word last_op;  	/*  0: unknown, 1: reading, -1: writing */
    char *buffer;		/*  buffer for FILE  */
    mkcl_object format;	        /*  external format  */
    mkcl_eformat_encoder encoder;
    mkcl_eformat_decoder decoder;
    mkcl_object format_table;
    int flags;		        /*  character table, flags, etc  */
    mkcl_object buffering_mode;
    mkcl_object character_position;
  };

  struct mkcl_random {
    MKCL_HEADER;
    mkcl_object value;	/*  random state value  */
  };

  enum mkcl_chattrib {		/*  character attribute  */
    mkcl_cat_whitespace,	/*  whitespace  */
    mkcl_cat_terminating,	/*  terminating macro  */
    mkcl_cat_non_terminating,	/*  non-terminating macro  */
    mkcl_cat_single_escape,	/*  single-escape  */
    mkcl_cat_multiple_escape,	/*  multiple-escape  */
    mkcl_cat_constituent	/*  constituent  */
  };

  struct mkcl_readtable_entry {		/*  read table entry  */
    enum mkcl_chattrib syntax_type;	/*  character attribute  */
    mkcl_object dispatch;		/*  a macro, a hash or NIL  */
  };

  enum mkcl_readtable_case {
    mkcl_case_upcase,
    mkcl_case_downcase,
    mkcl_case_invert,
    mkcl_case_preserve
  };

  struct mkcl_readtable {	        /*  read table  */
    MKCL_HEADER;
    enum mkcl_readtable_case read_case; /*  readtable-case  */
    struct mkcl_readtable_entry *table; /*  read table itself  */
    mkcl_object hash;		        /*  hash for values outside base-char range */
#if MKCL_WINDOWS
    CRITICAL_SECTION lock;	        /*  thread safe readtable  */
#else
    pthread_mutex_t lock;	        /*  thread safe readtable  */
#endif
  };

  struct mkcl_pathname {
    MKCL_HEADER2(logical,complete);  /*  logical pathname?  */
    mkcl_object host;	    /*  host  */
    mkcl_object device;     /*  device  */
    mkcl_object directory;  /*  directory  */
    mkcl_object name;	    /*  name  */
    mkcl_object type;	    /*  type  */
    mkcl_object version;    /*  version  */
    mkcl_object namestring; /*  namestring cache */
  };

  /* The layout of struct mkcl_codeblock must match the layout of its corresponding stub in cmp/cmpmain.lsp 
     otherwise mayhem may follow. */
  struct mkcl_codeblock {
    MKCL_HEADER2(self_destruct,locked);	/* delete DLL after gc */
					/* do not garbage collect this library */
    mkcl_object next;			/* next codeblock within same library */
    const char *data_text;      	/* string with objects to be defined */
    int	data_text_size;
    mkcl_object *data;	                /* data vector */
    int	data_size;
    mkcl_object *temp_data;		/* data vector for toplevel forms */
    int	temp_data_size;
    void *handle;		        /* handle returned by dlopen */
    void (*entry)(__MKCL, mkcl_object, mkcl_object); /* entry point */
    mkcl_object name;
    mkcl_object links;		        /* list of symbols with linking calls */
    mkcl_index cfuns_size;		/* number of functions defined by this block. */
    const struct mkcl_cfun *cfuns;
    mkcl_object source;		        /* common debug information for this block */
    mkcl_index nb_fun_refs;     /* number of referenced functions, size of following 2 vectors. */
    mkcl_object * fun_ref_syms; /* pointer to a vector of symbols denoting referenced functions. */
    mkcl_object * fun_refs;     /* pointer to a vector of function objects */
    mkcl_object * cfun_objs;    /* pointer to a vector of cfun objects defined by this block. */
  };

#define MKCL_MAX_FAST_FUNC_DISPATCH 5  /* for 0 to 4 arguments. Covers 98%+ of cases. */

  struct mkcl_function_entry_points {
    mkcl_objectfn entry;	/*  general entry address  */
    mkcl_objectfn_fixed _[MKCL_MAX_FAST_FUNC_DISPATCH];  /* fast entry addresses */
  };

  struct mkcl_bytecode {
    MKCL_HEADER;
    struct mkcl_function_entry_points f;
    mkcl_object name;		/*  function name  */
    mkcl_object definition;	/*  function definition in list form  */
    mkcl_index code_size;	/*  number of bytecode  */
    mkcl_index data_size;	/*  number of constants  */
    char *code;		        /*  the intermediate language  */
    mkcl_object *data;	        /*  non-inmediate constants used in the code  */
    mkcl_object file;		/*  file where it was defined...  */
    mkcl_object file_position;  /*  and where it was created  */
    mkcl_object owner;
  };

  struct mkcl_bclosure {
    MKCL_HEADER;
    struct mkcl_function_entry_points f;
    mkcl_object name;
    mkcl_object code;
    mkcl_object lex;
    mkcl_object producer;
    mkcl_object owner;
  };

  /* Beware that the layout of struct mkcl_cfun is related to the one
     of struct mkcl_instance when it is used as generic function object.
     Please read note just above definition of struct mkcl_instance.
  */
  /* The layout of mkcl_cfun is also hardcoded inside output-cfuns of CMP!
     Any change to it requires adjustment over there otherwise loading
     of compiled functions will be wrecked.
  */
  struct mkcl_cfun {		/*  compiled function header  */
    MKCL_HEADER;
    struct mkcl_function_entry_points f;
    mkcl_object name;		/*  compiled function name  */
    mkcl_object block;	        /*  descriptor of C code block for GC  */
    mkcl_objectfn_fixed old_entry_fixed;	/*  entry address  */
    mkcl_object file;		/*  file where it was defined...  */
    mkcl_object file_position;  /*  and where it was created  */
    mkcl_narg narg;
    mkcl_object * anchor;
    mkcl_index nb_fun_refs;     /* number of referenced functions, size of following 2 vectors. */
    mkcl_object * fun_ref_syms; /* pointer to a vector of symbols denoting referenced functions. */
    mkcl_object * fun_refs;     /* pointer to a vector of function objects */
    mkcl_object owner;          /* ignored by output-cfuns */
  };

  struct mkcl_cclosure {	/*  compiled closure header  */
    MKCL_HEADER;
    struct mkcl_function_entry_points f;
    mkcl_object name;
    mkcl_object cenv;           /*  closure environment display */
    mkcl_object block;	        /*  descriptor of C code block for GC  */
    mkcl_object file;		/*  file where it was defined...  */
    mkcl_object file_position;  /*  and where it was created  */
    mkcl_narg narg;
    mkcl_object producer;
    mkcl_object syms_cenv;
    mkcl_index nb_fun_refs;     /* number of referenced functions, size of following 2 vectors. */
    mkcl_object * fun_ref_syms; /* pointer to a vector of symbols denoting referenced functions. */
    mkcl_object * fun_refs;     /* pointer to a vector of function objects*/
    mkcl_object owner;
  };

  struct mkcl_cdisplay {
    MKCL_HEADER;
    mkcl_index nb_levels;
    mkcl_object level[];
  };

  struct mkcl_clevel_block {
    MKCL_HEADER;
    mkcl_object producer;  /* The function where this set of bindings were produced. */
    mkcl_object outer;     /* The closure block including this one or NIL if at top-level. */
    mkcl_index nb_vars;
    volatile mkcl_object var[]; /* volatile is required even if we stay within the same thread. */
  };


  enum mkcl_ffi_tag {
    MKCL_FFI_CHAR = 0,
    MKCL_FFI_UNSIGNED_CHAR,
    MKCL_FFI_BYTE,
    MKCL_FFI_UNSIGNED_BYTE,
    MKCL_FFI_SHORT,
    MKCL_FFI_UNSIGNED_SHORT,
    MKCL_FFI_INT,
    MKCL_FFI_UNSIGNED_INT,
    MKCL_FFI_LONG,
    MKCL_FFI_UNSIGNED_LONG,
    MKCL_FFI_INT16_T,
    MKCL_FFI_UINT16_T,
    MKCL_FFI_INT32_T,
    MKCL_FFI_UINT32_T,
    MKCL_FFI_INT64_T,
    MKCL_FFI_UINT64_T,
    MKCL_FFI_LONG_LONG,
    MKCL_FFI_UNSIGNED_LONG_LONG,
    MKCL_FFI_POINTER_VOID,
    MKCL_FFI_CSTRING,
    MKCL_FFI_OBJECT, /* This tag must be the last one of the "integral" types. */
    MKCL_FFI_FLOAT,
    MKCL_FFI_DOUBLE,
    MKCL_FFI_LONG_DOUBLE,
#if 0
    /* The other C99 types. */
    MKCL_FFI_FLOAT_COMPLEX,
    MKCL_FFI_DOUBLE_COMPLEX,
    MKCL_FFI_LONG_DOUBLE_COMPLEX,
    MKCL_FFI_FLOAT_IMAGINARY,
    MKCL_FFI_DOUBLE_IMAGINARY,
    MKCL_FFI_LONG_DOUBLE_IMAGINARY,
#endif
    MKCL_FFI_VOID
  };
  /* enum MKCL_FFI_VOID is used as a limit marker in a number of locations, beware!
     Note also that the order of enums declared in enum mkcl_ffi_tag must
     carefully match the content of mkcl_foreign_type_table[] and mkcl_foreign_type_size[].
     JCB
   */


  struct mkcl_foreign {	/*  wrapper for a pointer to some foreign data  */
    MKCL_HEADER;
    mkcl_object tag;	/*  a tag identifying the foreign type pointed to,
			    taken from the mkcl_foreign_type_table[] of c/ffi.d  */
    mkcl_index size;	/*  the size of the memory region pointed to, in bytes  */
    char *data;		/*  the data itself, pointer to a foreign location  */
  };

  struct mkcl_temp_stack_frame {
    MKCL_HEADER;
    mkcl_object *stack;	/*  Is this relative to the lisp temporaries stack? Yes.  */
    mkcl_object *base;	/*  Start of frame  */
    mkcl_index size;	/*  Number of arguments  */
    struct mkcl_env_struct *env;
  };






#define MKCL_T_STRUCTURE mkcl_t_instance
#define MKCL_STYPE(x)	 MKCL_CLASS_OF(x)
#define MKCL_SLOTS(x)	 (x)->instance.slots
#define MKCL_SLENGTH(x)	 (x)->instance.length
#define MKCL_SLOT(x,i)	 (x)->instance.slots[i]
#define MKCL_SNAME(x)	 MKCL_CLASS_NAME(MKCL_CLASS_OF(x))

#if 0 /* !CLOS */
# define MKCL_T_STRUCTURE mkcl_t_structure
# define MKCL_STYPE(x)	  (x)->str.name
# define MKCL_SLOTS(x)	  (x)->str.self
# define MKCL_SLENGTH(x)  (x)->str.length
# define MKCL_SLOT(x,i)	  (x)->str.self[i]
# define MKCL_SNAME(x) 	  (x)->str.name
#endif

  struct mkcl_structure { /*  structure header  */
    MKCL_HEADER;
    mkcl_object name;	  /*  structure name  */
    mkcl_object *self;	  /*  structure self  */
    mkcl_index length;	  /*  structure length  */
  };


#define MKCL_CLASS_OF(x)	(x)->instance.clas
#define MKCL_CLASS_NAME(x)	(x)->instance.slots[0] 
#define MKCL_CLASS_SUPERIORS(x)	(x)->instance.slots[1] /* hardcoded from a few lisp files. JCB */
#define MKCL_CLASS_INFERIORS(x)	(x)->instance.slots[2]
#define MKCL_CLASS_SLOTS(x)	(x)->instance.slots[3]
#define MKCL_CLASS_CPL(x)	(x)->instance.slots[4] /* hardcoded from a few lisp files. JCB */
#define MKCL_INSTANCEP(x)	((MKCL_IMMEDIATE(x)==0) && !mkcl_Null(x) && ((x)->d.t==mkcl_t_instance))
#define MKCL_NOT_FUNCALLABLE	0
#define MKCL_STANDARD_DISPATCH	1
#define MKCL_USER_DISPATCH	2


  /* Note that in struct mkcl_instance the offset position of field "f.entry" has
     to be the same as the one of field "f.entry" in struct mkcl_cfun otherwise
     calls to generic functions get hopelessly broken!
  */
  struct mkcl_instance {/*  instance header  */
    MKCL_HEADER1(isgf);
    struct mkcl_function_entry_points f;
    mkcl_index length;	/*  instance length, in number of slots  */
    mkcl_object clas;	/*  instance class  */
    mkcl_object sig;	/*  generation signature  */
    mkcl_object *slots;	/*  instance slots  */
  };


  struct mkcl_pin { /* experimental */
    MKCL_HEADER;
    mkcl_object bag;
    mkcl_object left;
    mkcl_object right;
    mkcl_object this;
  };

  struct mkcl_pin_bag { /* experimental */
    MKCL_HEADER;
    mkcl_object pins;
    mkcl_os_mutex_ref lock;
  };

  /*
    dummy type
  */
  struct mkcl_dummy {
    MKCL_HEADER;
  };

  struct mkcl_process {
    MKCL_HEADER1(detached);
    mkcl_object command;
    mkcl_object argv;
    mkcl_os_process_t ident;
    mkcl_object input;
    mkcl_object output;
    mkcl_object error;
    mkcl_exit_code_t exit_code;
    mkcl_object status;
    mkcl_object plist;
    mkcl_object to_worker;
    mkcl_object from_worker;
    mkcl_object error_from_worker;
  };

  enum {
    mkcl_thread_done,
    mkcl_thread_active,
    mkcl_thread_set,
    mkcl_thread_initialized
  };


#define MKCL_MAX_INTERRUPTS 5

  struct mkcl_thread {
    MKCL_HEADER1(volatile status);
    mkcl_object name; 
    mkcl_object function;
    mkcl_object args;
    mkcl_object result_value;
    volatile mkcl_object interrupt; /* private */
    mkcl_object plist;
    mkcl_object initial_bindings;
    bool detached;
    mkcl_os_thread_t thread;
    mkcl_os_thread_t base_thread;
    pid_t tid;
    struct mkcl_env_struct *env;
    volatile long shutdown_requested;
#if __unix
    pthread_mutex_t * running_lock; /* shared */
    sigset_t saved_sigmask; /* private */
#endif
    struct mkcl_frame * sigmask_frs_marker; /* private */
    int resume_handler_ran; /* private */
    int interrupt_count; /* stack index of following stack. */ /* private */
    struct interrupted_thread_ctrl
    {
      mkcl_os_thread_t thread_ident;
      int disable_interrupts;
      
      char * cs_limit;
      char * cs_org;
      mkcl_index cs_size;
      mkcl_index cs_overflow_size;
      bool cs_overflowing;

      char * interrupt_disabler_file; /* to support MKCL_DEBUG_INTERRUPT_MASK */
      size_t interrupt_disabler_lineno; /* to support MKCL_DEBUG_INTERRUPT_MASK */

      mkcl_index nvalues;
      mkcl_object values[MKCL_MULTIPLE_VALUES_LIMIT];

    } interrupted_threads[MKCL_MAX_INTERRUPTS]; /* a stack. */ /* private */
  };

  struct mkcl_lock {
    MKCL_HEADER1(recursive);
    mkcl_object name;
    mkcl_object holder;       /* thread holding the lock or NIL */
    mkcl_index counter;
    volatile mkcl_os_mutex_ref mutex;
#if __unix
    pthread_mutex_t mutex_obj;
#endif
  };

  struct mkcl_rwlock {
    MKCL_HEADER;
    mkcl_object name;
    volatile mkcl_os_rwlock_ref rwlock;
#if __unix
    pthread_rwlock_t rwlock_obj;
#endif
  };

  struct mkcl_semaphore {
    MKCL_HEADER;
    mkcl_object name;
    mkcl_word count;
    mkcl_word max_count;
#if MKCL_WINDOWS
    HANDLE sem;
#else
    sem_t sem_obj;
    sem_t * sem;
#endif
  };

  struct mkcl_condition_variable {
    MKCL_HEADER;
    mkcl_object name;
#if MKCL_WINDOWS
    volatile bool event_needs_reset;
    HANDLE event;
#else
    pthread_cond_t cv;
#endif
  };


  /* This object is allocated by the compiler with strict dynamic extent. */
  struct mkcl_cmp_debug_lexical_level
  {
    MKCL_HEADER;
    mkcl_object parent;

    unsigned long nb_vars;
    const struct mkcl_lex_var_info * var_descriptors;
    unsigned long nb_locations;
    void * const * var_locations;
  };


  /*
    Definition of lispunion.
  */
  union mkcl_lispunion {
    struct mkcl_cons	    cons;		/*  cons  */
    struct mkcl_bignum	    big;		/*  bignum  */
    struct mkcl_ratio	    ratio;		/*  ratio  */
    struct mkcl_singlefloat SF; 		/*  single floating-point number  */
    struct mkcl_doublefloat DF; 		/*  double floating-point number  */
    struct mkcl_long_float  longfloat;  	/*  long-float */
    struct mkcl_complex	    _complex;    	/*  complex number  */
    struct mkcl_symbol	    symbol;		/*  symbol  */
    struct mkcl_package	    pack;		/*  package  */
    struct mkcl_hashtable   hash;		/*  hash table  */
    struct mkcl_array	    array;		/*  array  */
    struct mkcl_vector	    vector;		/*  vector  */
    struct mkcl_base_string base_string;	/*  base-string  */
    struct mkcl_string	    string;		/*  string  */
    struct mkcl_stream	    stream;		/*  stream  */
    struct mkcl_random	    random;		/*  random-states  */
    struct mkcl_readtable   readtable;  	/*  read table  */
    struct mkcl_pathname    pathname;   	/*  path name  */
    struct mkcl_bytecode    bytecode;  	        /*  bytecompiled function / code */
    struct mkcl_bclosure    bclosure;   	/*  bytecompiled closure */
    struct mkcl_cfun	    cfun;		/*  compiled function  */
    struct mkcl_cclosure    cclosure;   	/*  compiled closure  */
    struct mkcl_cdisplay    display;    	/*  compiled closure display */
    struct mkcl_clevel_block lblock;     	/*  compiled closure block */
    struct mkcl_pin         pin;                /*  GC pin */
    struct mkcl_pin_bag     pin_bag;            /*  bag of GC pins */

    struct mkcl_dummy	    d;   		/*  dummy  */
    struct mkcl_instance    instance;    	/*  clos instance */
    struct mkcl_structure   str;		/*  structure  */
    struct mkcl_thread      thread;    	        /*  thread  */
    struct mkcl_lock	    lock; 		/*  lock  */
    struct mkcl_rwlock	    rwlock; 		/*  rwlock  */
    struct mkcl_semaphore   semaphore; 		/*  semaphore  */
    struct mkcl_condition_variable condition_variable; /*  condition-variable */

    struct mkcl_codeblock   cblock;		/*  codeblock  */
    struct mkcl_foreign	    foreign;    	/*  user defined data type */
    struct mkcl_temp_stack_frame frame;		/*  temp stack frame  */
    struct mkcl_cmp_debug_lexical_level cmp_dbg_lex;  /* compiler debug lexical info */

    struct mkcl_UTF_8       UTF_8;
    struct mkcl_UTF_16      UTF_16;
    struct mkcl_process     process;
  };

  /*
    mkcl_type_of.
  */
#define	mkcl_type_of(o) ((mkcl_type)(MKCL_IMMEDIATE(o) ? MKCL_IMMEDIATE(o) : (o == mk_cl_Cnil) ? mkcl_t_null : ((o)->d.t)))

  /*
    This is used to retrieve optional arguments
  */
  typedef struct {
    va_list args;
    mkcl_object *sp;
    int narg;
  } mkcl_va_list[1]; /* What is that subscript doing here!?! JCB
			It turns mkcl_va_list into an array type of 1 element.
			Among other things this allows some fine control during
			argument passing of objects of the type.
			Ex.: { mkcl_va_list t; foo(t); bar(t[0]); }
			     to foo t is passed by reference,
			     to bar t is passed by value.
		      */

  /* Misc */

  /* The following macros are meant to be used as "type-specifier" in declarations without initialization. */
  /* They allow declaration of lisp objects outside the normal GC controled heap. */

#define mkcl_singlefloat_object(name, val)				\
  struct mkcl_singlefloat name = { (int8_t)mkcl_t_singlefloat, 0, 0, 0, (val) }

#define mkcl_doublefloat_object(name, val)				\
  struct mkcl_doublefloat name = { (int8_t)mkcl_t_doublefloat, 0, 0, 0, (val) }

#define mkcl_longfloat_object(name, val)				\
  struct mkcl_long_float name = { (int8_t)mkcl_t_longfloat, 0, 0, 0, (val) }

#define mkcl_ratio_object(name, num, den)				\
  struct mkcl_ratio name = { (int8_t)mkcl_t_ratio, 0, 0, 0, (num), (den) }

#define mkcl_complex_object(name, real, imag)				\
  struct mkcl_complex name = { (int8_t)mkcl_t_complex, 0, 0, 0, (real), (imag) }

#define mkcl_cons_object(name, car, cdr)				\
  struct mkcl_cons name = { (int8_t)mkcl_t_cons, 0, 0, 0, (car), (cdr), mk_cl_Cnil }

#define mkcl_UTF_8_object(name, data_array)				\
  struct mkcl_UTF_8 name = { (int8_t)mkcl_t_UTF_8, 0, 0, 0,		\
			     (sizeof(data_array)-1), (sizeof(data_array)-1), ((mkcl_char8 *) (data_array)) }

#define mkcl_UTF_8_object_sized(name, data_ptr, dim)			\
  struct mkcl_UTF_8 name = { (int8_t)mkcl_t_UTF_8, 0, 0, 0, (dim), (dim), ((mkcl_char8 *) (data_ptr)) }

#define mkcl_UTF_16_object(name, data_array)				\
  struct mkcl_UTF_16 name = { (int8_t)mkcl_t_UTF_16, 0, 0, 0,		\
			      (sizeof(data_array)-1), (sizeof(data_array)-1), ((mkcl_char16 *) (data_array)) }

#define mkcl_UTF_16_object_sized(name, data_ptr, dim)		\
  struct mkcl_UTF_16 name = { (int8_t)mkcl_t_UTF_16, 0, 0, 0, (dim), (dim), ((mkcl_char16 *) (data_ptr)) }


#define MKCL_MAXIMUM_DYNAMIC_OBJECT_SIZE (16 * 1024)

#define mkcl_VLA(_env, base_type, name, size)				\
  const size_t __##name##_sizeof__ = (sizeof(base_type) * (size));	\
  base_type * name							\
  = ((__##name##_sizeof__ <= MKCL_MAXIMUM_DYNAMIC_OBJECT_SIZE)		\
     ? alloca(__##name##_sizeof__) : mkcl_alloc(_env, __##name##_sizeof__));

#if MKCL_WINDOWS
# define mkcl_UTF_buffer(_env, name, dim)				\
  /* mkcl_char16 name##_utf_16_raw_data[((dim) * 2) + 1]; */ /* VLA */	\
  mkcl_VLA(_env, mkcl_char16, name##_utf_16_raw_data, ((dim) * 2) + 1);	\
  mkcl_UTF_16_object_sized(name##_utf_16_obj, (dim) * 2, name##_utf_16_raw_data); \
  mkcl_object name = (mkcl_object) &(name##_utf_16_obj);
#else
# define mkcl_UTF_buffer(_env, name, dim)				\
  /* mkcl_char8 name##_utf_8_raw_data[((dim) * 4) + 1]; */ /* VLA */	\
  mkcl_VLA(_env, mkcl_char8, name##_utf_8_raw_data, ((dim) * 4) + 1);	\
  mkcl_UTF_8_object_sized(name##_utf_8_obj, (dim) * 4, name##_utf_8_raw_data); \
  mkcl_object name = (mkcl_object) &(name##_utf_8_obj);
#endif

#if MKCL_WINDOWS
# define mkcl_dynamic_extent_OSstring(_env, name, str)			\
  const size_t name##_raw_len = (mkcl_string_length(_env, str) * 2);	\
  /* mkcl_char16 name##_buf_raw_data[name##_raw_len + 1]; */ /* VLA */	\
  mkcl_VLA(_env, mkcl_char16, name##_buf_raw_data, name##_raw_len + 1);	\
  mkcl_UTF_16_object_sized(name##_utf_16_obj, name##_buf_raw_data, name##_raw_len); \
  mkcl_object name = (mkcl_object) &name##_utf_16_obj;			\
  if (MKCL_BASE_STRING_P(str))						\
    mkcl_fill_utf_16_from_base_string(_env, name, 0, str);		\
  else									\
    mkcl_fill_utf_16_from_string(_env, name, 0, str);
#else
# define mkcl_dynamic_extent_OSstring(_env, name, str)			\
  const bool name##_in_utf8_p = mkcl_os_string_format_is_utf_8_p(_env);	\
  const size_t name##_raw_len = (name##_in_utf8_p ? (mkcl_string_length(_env, str) * 4) : mkcl_string_length(_env, str)); \
  /* mkcl_char8 name##_buf_raw_data[name##_raw_len + 1]; */ /* VLA */	\
  mkcl_VLA(_env, mkcl_char8, name##_buf_raw_data, name##_raw_len + 1);	\
  mkcl_UTF_8_object_sized(name##_utf_8_obj, name##_buf_raw_data, name##_raw_len); \
  mkcl_base_string_object_sized(name##_bstr_obj, name##_buf_raw_data, name##_raw_len); \
  mkcl_object name = (name##_in_utf8_p ? (mkcl_object) &name##_utf_8_obj : (mkcl_object) &name##_bstr_obj); \
  if (MKCL_BASE_STRING_P(str))						\
    if (name##_in_utf8_p)						\
      mkcl_fill_utf_8_from_base_string(_env, name, 0, str);		\
    else name = (str);							\
  else									\
    if (name##_in_utf8_p)						\
      mkcl_fill_utf_8_from_string(_env, name, 0, str);			\
    else mkcl_fill_base_string_from_string(_env, name, 0, str);
#endif


#define mkcl_base_string_object(name, chars)		\
  struct mkcl_base_string name = {			\
    (int8_t)mkcl_t_base_string, 0, FALSE, FALSE,	\
    mk_cl_Cnil,						\
    (sizeof(chars)-1),					\
    (sizeof(chars)-1),					\
    ((mkcl_base_char *) chars),                         \
    mkcl_base_char_index, mkcl_base_char_set_index,	\
  }

#define mkcl_base_string_object_sized(name, chars, nb_char)	\
  struct mkcl_base_string name = {				\
    (int8_t)mkcl_t_base_string, 0, FALSE, FALSE,		\
    mk_cl_Cnil,							\
    (nb_char),							\
    (nb_char),							\
    ((mkcl_base_char *) chars),                                 \
    mkcl_base_char_index, mkcl_base_char_set_index,		\
  }


#define mkcl_string_object(name, characters)		\
  struct mkcl_string name = {				\
    (int8_t)mkcl_t_string, 0, FALSE, FALSE,		\
    mk_cl_Cnil,						\
    (sizeof(characters)/4)-1,				\
    (sizeof(characters)/4)-1,				\
    (characters),					\
    mkcl_character_index, mkcl_character_set_index,	\
  }
  
#define mkcl_string_object_sized(name, characters, nb_char)		\
  struct mkcl_string name = {						\
    (int8_t)mkcl_t_string, 0, FALSE, FALSE,				\
    mk_cl_Cnil,								\
    ((nb_char)/4),							\
    ((nb_char)/4),							\
    (characters),							\
    mkcl_character_index, mkcl_character_set_index,			\
  }


  /********************/



#ifdef __cplusplus
}
#endif

#endif /* MKCL_OBJECT_H */

