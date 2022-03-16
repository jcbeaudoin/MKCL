
#include <string.h>
#include <stdio.h>
#include <pthread.h>
#include <libgen.h>

#include <mkcl/os_context.h>
#include <mkcl/object.h>


mkcl_hash_value hash_base_string(const mkcl_base_char *s, const mkcl_index len, mkcl_hash_value h);
void add_entry_to_hash(const mkcl_hash_value hashed_key,
		       struct mkcl_hashtable * ht,
		       struct mkcl_hashtable_entry * entry);
struct mkcl_symbol * find_symbol(const struct mkcl_base_string * const name, struct mkcl_package * const package, bool * internalp_ptr);


#define VAL_DENOT(it) {					\
    (int8_t)mkcl_t_base_string, 0, FALSE, FALSE,	\
      mk_cl_Cnil,					\
      (sizeof(it)-1),					\
      (sizeof(it)-1),					\
      (it),						\
      NULL, NULL,					\
      }

#define FUN_DENOT(it) {					\
    (int8_t)mkcl_t_base_string, 0, FALSE, FALSE,	\
      mk_cl_Cnil,					\
      (sizeof(it)-1),					\
      (sizeof(it)-1),					\
      (it),						\
      NULL, NULL,					\
      }

struct mkcl_package mkcl_package_cl;
struct mkcl_package mkcl_package_si;
struct mkcl_package mkcl_package_keyword;
struct mkcl_package mkcl_package_mt;
struct mkcl_package mkcl_package_clos;
struct mkcl_package mkcl_package_mkcl;
struct mkcl_package mkcl_package_gray;
struct mkcl_package mkcl_package_ffi;

#define BLANK_SYMBOL_INITIALIZER {					\
    mkcl_t_symbol, 0, mkcl_stp_ordinary, 0, /* MKCL_HEADER(stype) */	\
      MKCL_OBJNULL, /* value */						\
      mk_cl_Cnil, /* gfdef */						\
      mk_cl_Cnil, /* plist */						\
      mk_cl_Cnil, /* name */						\
      mk_cl_Cnil, /* hpack */						\
      mk_cl_Cnil, /* properly_named_class */				\
      mk_cl_Cnil, /* sys_plist */					\
      MKCL_NOT_A_SPECIAL_INDEX, /* special_index */			\
      0, /* hashed_name */						\
      mk_cl_Cnil, /* C_name */						\
      NULL, /* _C_name */						\
      NULL /* _name */							\
      }
