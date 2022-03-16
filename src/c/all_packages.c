/* -*- mode: c -*- */
/*
    main.c --
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2016,2022, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

/******************************** GLOBALS *****************************/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>


void mkcl_init_all_packages(MKCL)
{

  /*
   * 1) Initialize symbols and packages
   */
  {
#if 0
    mkcl_object sym_name = mkcl_make_simple_base_string(env, "NIL");
    mkcl_object sym_C_name = mkcl_make_simple_base_string(env, "mk_cl_Cnil_symbol");
#else
    static struct mkcl_base_string sym_name_object = MKCL_BASE_STRING_INIT("NIL");
    static struct mkcl_base_string sym_C_name_object = MKCL_BASE_STRING_INIT("mk_cl_Cnil_symbol");
    mkcl_object sym_name = (mkcl_object) &sym_name_object;
    mkcl_object sym_C_name = (mkcl_object) &sym_C_name_object;
#endif
    mkcl_hash_value sym_hashed_name = mkcl_hash_base_string(sym_name->base_string.self, sym_name->base_string.fillp, 0);

    mk_cl_Cnil_symbol.t = mkcl_t_symbol;
    mk_cl_Cnil_symbol.special_index = MKCL_NOT_A_SPECIAL_INDEX;
    mk_cl_Cnil_symbol.value = mk_cl_Cnil;
    mk_cl_Cnil_symbol.name = sym_name;
    mk_cl_Cnil_symbol.hashed_name = sym_hashed_name;
    mk_cl_Cnil_symbol.gfdef = mk_cl_Cnil;
    mk_cl_Cnil_symbol.plist = mk_cl_Cnil;
    mk_cl_Cnil_symbol.sys_plist = mk_cl_Cnil;
    mk_cl_Cnil_symbol.hpack = mk_cl_Cnil;
    mk_cl_Cnil_symbol.properly_named_class = mk_cl_Cnil;
    mk_cl_Cnil_symbol.stype = mkcl_stp_constant;
    mk_cl_Cnil_symbol.C_name = sym_C_name;
  }

  {
#if 0
    mkcl_object sym_name = mkcl_make_simple_base_string(env, "T");
    mkcl_object sym_C_name = mkcl_make_simple_base_string(env, "mk_cl_Ct_symbol");
#else
    static struct mkcl_base_string sym_name_object = MKCL_BASE_STRING_INIT("T");
    static struct mkcl_base_string sym_C_name_object = MKCL_BASE_STRING_INIT("mk_cl_Ct_symbol");
    mkcl_object sym_name = (mkcl_object) &sym_name_object;
    mkcl_object sym_C_name = (mkcl_object) &sym_C_name_object;
#endif
    mkcl_hash_value sym_hashed_name = mkcl_hash_base_string(sym_name->base_string.self, sym_name->base_string.fillp, 0);

#if !__clang__
  mk_cl_Ct->symbol.t = mkcl_t_symbol;
  mk_cl_Ct->symbol.special_index = MKCL_NOT_A_SPECIAL_INDEX;
  mk_cl_Ct->symbol.value = mk_cl_Ct;
  mk_cl_Ct->symbol.name = sym_name;
  mk_cl_Ct->symbol.hashed_name = sym_hashed_name;
  mk_cl_Ct->symbol.gfdef = mk_cl_Cnil;
  mk_cl_Ct->symbol.plist = mk_cl_Cnil;
  mk_cl_Ct->symbol.sys_plist = mk_cl_Cnil;
  mk_cl_Ct->symbol.hpack = mk_cl_Cnil;
  mk_cl_Ct->symbol.properly_named_class = mk_cl_Cnil;
  mk_cl_Ct->symbol.stype = mkcl_stp_constant;
  mk_cl_Ct->symbol.C_name = sym_C_name;
#else
  mk_cl_Ct_symbol.t = mkcl_t_symbol;
  mk_cl_Ct_symbol.special_index = MKCL_NOT_A_SPECIAL_INDEX;
  mk_cl_Ct_symbol.value = mk_cl_Ct;
  mk_cl_Ct_symbol.name = sym_name;
  mk_cl_Ct_symbol.hashed_name = sym_hashed_name;
  mk_cl_Ct_symbol.gfdef = mk_cl_Cnil;
  mk_cl_Ct_symbol.plist = mk_cl_Cnil;
  mk_cl_Ct_symbol.sys_plist = mk_cl_Cnil;
  mk_cl_Ct_symbol.hpack = mk_cl_Cnil;
  mk_cl_Ct_symbol.properly_named_class = mk_cl_Cnil;
  mk_cl_Ct_symbol.stype = mkcl_stp_constant;
  mk_cl_Ct_symbol.C_name = sym_C_name;
#endif
  }

  {
#if 0
    mkcl_object sym_name = mkcl_make_simple_base_string(env, "UNBOUND");
    mkcl_object sym_C_name = mkcl_make_simple_base_string(env, "mk_si_unbound_symbol");
#else
    static struct mkcl_base_string sym_name_object = MKCL_BASE_STRING_INIT("UNBOUND");
    static struct mkcl_base_string sym_C_name_object = MKCL_BASE_STRING_INIT("mk_si_unbound_symbol");
    mkcl_object sym_name = (mkcl_object) &sym_name_object;
    mkcl_object sym_C_name = (mkcl_object) &sym_C_name_object;
#endif
    mkcl_hash_value sym_hashed_name = mkcl_hash_base_string(sym_name->base_string.self, sym_name->base_string.fillp, 0);

    mk_si_unbound_symbol.t = mkcl_t_symbol;
    mk_si_unbound_symbol.special_index = MKCL_NOT_A_SPECIAL_INDEX;
    mk_si_unbound_symbol.value = MKCL_UNBOUND;
    mk_si_unbound_symbol.name = sym_name;
    mk_si_unbound_symbol.hashed_name = sym_hashed_name;
    mk_si_unbound_symbol.gfdef = ((mkcl_object) &mk_si_unbound_cfunobj);
    mk_si_unbound_symbol.plist = mk_cl_Cnil;
    mk_si_unbound_symbol.sys_plist = mk_cl_Cnil;
    mk_si_unbound_symbol.hpack = mk_cl_Cnil;
    mk_si_unbound_symbol.properly_named_class = mk_cl_Cnil;
    mk_si_unbound_symbol.stype = mkcl_stp_constant;
    mk_si_unbound_symbol.C_name = sym_C_name;
  }

  {
#if 0
    mkcl_object sym_name = mkcl_make_simple_base_string(env, "PROTECT_TAG");
    mkcl_object sym_C_name = mkcl_make_simple_base_string(env, "mk_si_protect_tag_symbol");
#else
    static struct mkcl_base_string sym_name_object = MKCL_BASE_STRING_INIT("PROTECT_TAG");
    static struct mkcl_base_string sym_C_name_object = MKCL_BASE_STRING_INIT("mk_si_protect_tag_symbol");
    mkcl_object sym_name = (mkcl_object) &sym_name_object;
    mkcl_object sym_C_name = (mkcl_object) &sym_C_name_object;
#endif
    mkcl_hash_value sym_hashed_name = mkcl_hash_base_string(sym_name->base_string.self, sym_name->base_string.fillp, 0);

    mk_si_protect_tag_symbol.t = mkcl_t_symbol;
    mk_si_protect_tag_symbol.special_index = MKCL_NOT_A_SPECIAL_INDEX;
    mk_si_protect_tag_symbol.value = MKCL_OBJNULL;
    mk_si_protect_tag_symbol.name = sym_name;
    mk_si_protect_tag_symbol.hashed_name = sym_hashed_name;
    mk_si_protect_tag_symbol.gfdef = mk_cl_Cnil;
    mk_si_protect_tag_symbol.plist = mk_cl_Cnil;
    mk_si_protect_tag_symbol.sys_plist = mk_cl_Cnil;
    mk_si_protect_tag_symbol.hpack = mk_cl_Cnil;
    mk_si_protect_tag_symbol.properly_named_class = mk_cl_Cnil;
    mk_si_protect_tag_symbol.stype = mkcl_stp_ordinary;
    mk_si_protect_tag_symbol.C_name = sym_C_name;
  }

  mkcl_core.packages = mk_cl_Cnil;
  mkcl_core.packages_to_be_created = mk_cl_Cnil;

  {
    static struct mkcl_base_string pack_name_obj = MKCL_BASE_STRING_INIT("COMMON-LISP");
    mkcl_object pack_name = (mkcl_object) &pack_name_obj;

    static struct mkcl_base_string pack_nick1_obj = MKCL_BASE_STRING_INIT("CL");
    static struct mkcl_base_string pack_nick2_obj = MKCL_BASE_STRING_INIT("LISP");
    static struct mkcl_cons nicknames[] = {
      MKCL_CONS_INIT(&pack_nick1_obj, &nicknames[1]),
      MKCL_CONS_INIT(&pack_nick2_obj, mk_cl_Cnil),

    };

    mkcl_core.lisp_package = mkcl_make_sized_package(env, pack_name,
						     (mkcl_object) nicknames, mk_cl_Cnil,
						     MKCL_MAKE_FIXNUM(1400), MKCL_MAKE_FIXNUM(16));
  }
  
  {
    static struct mkcl_base_string pack_name_obj = MKCL_BASE_STRING_INIT("MKCL");
    mkcl_object pack_name = (mkcl_object) &pack_name_obj;

    static struct mkcl_base_string pack_nick1_obj = MKCL_BASE_STRING_INIT("MKCL-EXTENSIONS");
    static struct mkcl_base_string pack_nick2_obj = MKCL_BASE_STRING_INIT("MK-EXT");
    static struct mkcl_cons nicknames[] = {
      MKCL_CONS_INIT(&pack_nick1_obj, &nicknames[1]),
      MKCL_CONS_INIT(&pack_nick2_obj, mk_cl_Cnil),
    };
    static struct mkcl_cons use_list[] = {
      MKCL_CONS_INIT(mk_cl_Cnil, mk_cl_Cnil),
    };

    use_list[0].car = mkcl_core.lisp_package;
    mkcl_core.mkcl_ext_package = mkcl_make_sized_package(env, pack_name,
							 (mkcl_object) nicknames, (mkcl_object) use_list,
							 MKCL_MAKE_FIXNUM(200), MKCL_MAKE_FIXNUM(16));
  }

  {
    static struct mkcl_base_string pack_name_obj = MKCL_BASE_STRING_INIT("COMMON-LISP-USER");
    mkcl_object pack_name = (mkcl_object) &pack_name_obj;

    static struct mkcl_base_string pack_nick1_obj = MKCL_BASE_STRING_INIT("CL-USER");
    static struct mkcl_base_string pack_nick2_obj = MKCL_BASE_STRING_INIT("USER");
    static struct mkcl_cons nicknames[] = {
      MKCL_CONS_INIT(&pack_nick1_obj, &nicknames[1]),
      MKCL_CONS_INIT(&pack_nick2_obj, mk_cl_Cnil),
    };
    static struct mkcl_cons use_list[] = {
      MKCL_CONS_INIT(mk_cl_Cnil, &use_list[1]),
      MKCL_CONS_INIT(mk_cl_Cnil, mk_cl_Cnil),
    };

    use_list[0].car = mkcl_core.lisp_package;
    use_list[1].car = mkcl_core.mkcl_ext_package;
    mkcl_core.user_package = mkcl_make_package(env, pack_name, (mkcl_object) nicknames, (mkcl_object) use_list);
  }
  
  {
    static struct mkcl_base_string pack_name_obj = MKCL_BASE_STRING_INIT("KEYWORD");
    mkcl_object pack_name = (mkcl_object) &pack_name_obj;
    
    mkcl_core.keyword_package = mkcl_make_sized_package(env, pack_name,
							mk_cl_Cnil, mk_cl_Cnil,
							MKCL_MAKE_FIXNUM(1000), MKCL_MAKE_FIXNUM(16));
  }
    
  {
    static struct mkcl_base_string pack_name_obj = MKCL_BASE_STRING_INIT("SI");
    mkcl_object pack_name = (mkcl_object) &pack_name_obj;

    static struct mkcl_base_string pack_nick1_obj = MKCL_BASE_STRING_INIT("SYSTEM");
    static struct mkcl_base_string pack_nick2_obj = MKCL_BASE_STRING_INIT("SYS");
    static struct mkcl_cons nicknames[] = {
      MKCL_CONS_INIT(&pack_nick1_obj, &nicknames[1]),
      MKCL_CONS_INIT(&pack_nick2_obj, mk_cl_Cnil),
    };
    static struct mkcl_cons use_list[] = {
      MKCL_CONS_INIT(mk_cl_Cnil, &use_list[1]),
      MKCL_CONS_INIT(mk_cl_Cnil, mk_cl_Cnil),
    };

    use_list[0].car = mkcl_core.lisp_package;
    use_list[1].car = mkcl_core.mkcl_ext_package;
    mkcl_core.system_package = mkcl_make_sized_package(env, pack_name,
						       (mkcl_object) nicknames, (mkcl_object) use_list,
						       MKCL_MAKE_FIXNUM(580), MKCL_MAKE_FIXNUM(1800));
  }
  
  {
    static struct mkcl_base_string pack_name_obj = MKCL_BASE_STRING_INIT("CLOS");
    mkcl_object pack_name = (mkcl_object) &pack_name_obj;
    static struct mkcl_cons use_list[] = {
      MKCL_CONS_INIT(mk_cl_Cnil, mk_cl_Cnil),
    };

    use_list[0].car = mkcl_core.lisp_package;
    mkcl_core.clos_package = mkcl_make_sized_package(env, pack_name,
						     mk_cl_Cnil, (mkcl_object) use_list,
						     MKCL_MAKE_FIXNUM(16), MKCL_MAKE_FIXNUM(650));
  }
  
  {
    static struct mkcl_base_string pack_name_obj = MKCL_BASE_STRING_INIT("FFI");
    mkcl_object pack_name = (mkcl_object) &pack_name_obj;
    static struct mkcl_cons use_list[] = {
      MKCL_CONS_INIT(mk_cl_Cnil, &use_list[1]),
      MKCL_CONS_INIT(mk_cl_Cnil, mk_cl_Cnil),
    };

    use_list[0].car = mkcl_core.lisp_package;
    use_list[1].car = mkcl_core.mkcl_ext_package;
    mkcl_core.ffi_package = mkcl_make_sized_package(env, pack_name,
						    mk_cl_Cnil, (mkcl_object) use_list,
						    MKCL_MAKE_FIXNUM(75), MKCL_MAKE_FIXNUM(30));
  }
  
  {
    static struct mkcl_base_string pack_name_obj = MKCL_BASE_STRING_INIT("MT");
    mkcl_object pack_name = (mkcl_object) &pack_name_obj;

    static struct mkcl_base_string pack_nick1_obj = MKCL_BASE_STRING_INIT("MULTI-THREADING");
    static struct mkcl_base_string pack_nick2_obj = MKCL_BASE_STRING_INIT("MP");
    static struct mkcl_base_string pack_nick3_obj = MKCL_BASE_STRING_INIT("MULTIPROCESSING");
    static struct mkcl_cons nicknames[] = {
      MKCL_CONS_INIT(&pack_nick1_obj, &nicknames[1]),
      MKCL_CONS_INIT(&pack_nick2_obj, &nicknames[2]),
      MKCL_CONS_INIT(&pack_nick3_obj, mk_cl_Cnil),
    };
    static struct mkcl_cons use_list[] = {
      MKCL_CONS_INIT(mk_cl_Cnil, mk_cl_Cnil),
    };

    use_list[0].car = mkcl_core.lisp_package;
    mkcl_core.mt_package = mkcl_make_sized_package(env, pack_name,
						   (mkcl_object) nicknames, (mkcl_object) use_list,
						   MKCL_MAKE_FIXNUM(90), MKCL_MAKE_FIXNUM(40));
  }
  
  {
    static struct mkcl_base_string pack_name_obj = MKCL_BASE_STRING_INIT("GRAY");
    mkcl_object pack_name = (mkcl_object) &pack_name_obj;
    static struct mkcl_cons use_list[] = {
      MKCL_CONS_INIT(mk_cl_Cnil, mk_cl_Cnil),
    };

    use_list[0].car = mkcl_core.lisp_package;
    mkcl_core.gray_package = mkcl_make_sized_package(env, pack_name,
						     mk_cl_Cnil, (mkcl_object) use_list,
						     MKCL_MAKE_FIXNUM(1400), MKCL_MAKE_FIXNUM(140));
  }

  
  mk_cl_Cnil_symbol.hpack = mkcl_core.lisp_package;
  mkcl_import2(env, mk_cl_Cnil, mkcl_core.lisp_package);
  mkcl_export2(env, mk_cl_Cnil, mkcl_core.lisp_package);

#if !__clang__
  mk_cl_Ct->symbol.hpack = mkcl_core.lisp_package;
#else
  mk_cl_Ct_symbol.hpack = mkcl_core.lisp_package;
#endif
  mkcl_import2(env, mk_cl_Ct, mkcl_core.lisp_package);
  mkcl_export2(env, mk_cl_Ct, mkcl_core.lisp_package);

  mk_si_unbound_symbol.hpack = mkcl_core.system_package;
  mkcl_import2(env, MK_SI_UNBOUND, mkcl_core.system_package);
  mkcl_export2(env, MK_SI_UNBOUND, mkcl_core.system_package);

  mk_si_protect_tag_symbol.hpack = mkcl_core.system_package;
  mkcl_import2(env, MK_SI_PROTECT_TAG, mkcl_core.system_package);
  mkcl_export2(env, MK_SI_PROTECT_TAG, mkcl_core.system_package);

  /* These must come _after_ the packages and NIL/T have been created */
  mkcl_init_all_symbols(env);

}

