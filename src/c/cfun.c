/* -*- mode: c -*- */
/*
    cfun.c -- Compiled functions.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2016,2021 Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>
#if 0
# include <stdio.h>
#endif

#include "cfun_dispatch.c"

static mkcl_object wrong_num_args_cfun_0(MKCL)
{
  mkcl_object fun = env->function;

  mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, 0);
  return mk_cl_Cnil;
}

static mkcl_object wrong_num_args_cfun_1(MKCL)
{
  mkcl_object fun = env->function;

  mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, 1);
  return mk_cl_Cnil;
}

static mkcl_object wrong_num_args_cfun_2(MKCL)
{
  mkcl_object fun = env->function;

  mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, 2);
  return mk_cl_Cnil;
}

static mkcl_object wrong_num_args_cfun_3(MKCL)
{
  mkcl_object fun = env->function;

  mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, 3);
  return mk_cl_Cnil;
}

static mkcl_object wrong_num_args_cfun_4(MKCL)
{
  mkcl_object fun = env->function;

  mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, 4);
  return mk_cl_Cnil;
}

static mkcl_object wrong_num_args_cfun_va(MKCL, mkcl_narg narg, ...)
{
  mkcl_object fun = env->function;

  mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return mk_cl_Cnil;
}



mkcl_object
mkcl_make_cfun(MKCL, mkcl_objectfn_fixed c_function, mkcl_object name,
	       mkcl_object cblock, int narg, mkcl_object * anchor)
{
  mkcl_object cf;

  if (narg < 0 || narg >= MKCL_C_ARGUMENTS_LIMIT)
    mkcl_FEprogram_error(env, "mkcl_make_cfun: function ~S requires too many arguments. ~D",
			 2, name, MKCL_MAKE_FIXNUM(narg));

  cf = mkcl_alloc_raw_cfun(env);
  cf->cfun.f.entry = (((narg < 0) || (narg > 4)) ? dispatch_table[narg] : (mkcl_objectfn) wrong_num_args_cfun_va);
  cf->cfun.f._[0] = ((narg == 0) ? c_function : (mkcl_objectfn_fixed) wrong_num_args_cfun_0);
  cf->cfun.f._[1] = ((narg == 1) ? c_function : (mkcl_objectfn_fixed) wrong_num_args_cfun_1);
  cf->cfun.f._[2] = ((narg == 2) ? c_function : (mkcl_objectfn_fixed) wrong_num_args_cfun_2);
  cf->cfun.f._[3] = ((narg == 3) ? c_function : (mkcl_objectfn_fixed) wrong_num_args_cfun_3);
  cf->cfun.f._[4] = ((narg == 4) ? c_function : (mkcl_objectfn_fixed) wrong_num_args_cfun_4);
  cf->cfun.old_entry_fixed = c_function;
  cf->cfun.name = name;
  cf->cfun.block = cblock;
  cf->cfun.file = mk_cl_Cnil;
  cf->cfun.file_position = MKCL_MAKE_FIXNUM(-1);
  cf->cfun.narg = narg;
  cf->cfun.anchor = anchor;
  cf->cfun.nb_fun_refs = 0;
  cf->cfun.fun_ref_syms = NULL;
  cf->cfun.fun_refs = NULL;
  cf->cfun.owner = mk_cl_Cnil;

  if (anchor != NULL)
    *anchor = cf;

  return cf;
}

static mkcl_object f0(MKCL)
{
  return env->function->cfun.f.entry(env, 0);
}

static mkcl_object f1(MKCL, mkcl_object x1)
{
  return env->function->cfun.f.entry(env, 1, x1);
}

static mkcl_object f2(MKCL, mkcl_object x1, mkcl_object x2)
{
  return env->function->cfun.f.entry(env, 2, x1, x2);
}

static mkcl_object f3(MKCL, mkcl_object x1, mkcl_object x2, mkcl_object x3)
{
  return env->function->cfun.f.entry(env, 3, x1, x2, x3);
}

static mkcl_object f4(MKCL, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4)
{
  return env->function->cfun.f.entry(env, 4, x1, x2, x3, x4);
}

mkcl_object
mkcl_make_cfun_va(MKCL, mkcl_objectfn c_function, mkcl_object name,
		  mkcl_object cblock, mkcl_object * anchor)
{
  mkcl_object cf;

  cf = mkcl_alloc_raw_cfun(env);
  cf->cfun.f.entry = c_function;
  cf->cfun.f._[0] = f0;
  cf->cfun.f._[1] = f1;
  cf->cfun.f._[2] = f2;
  cf->cfun.f._[3] = f3;
  cf->cfun.f._[4] = f4;
  cf->cfun.name = name;
  cf->cfun.block = cblock;
  cf->cfun.old_entry_fixed = NULL;
  cf->cfun.narg = -1;
  cf->cfun.file = mk_cl_Cnil;
  cf->cfun.file_position = MKCL_MAKE_FIXNUM(-1);
  cf->cfun.anchor = anchor;
  cf->cfun.nb_fun_refs = 0;
  cf->cfun.fun_ref_syms = NULL;
  cf->cfun.fun_refs = NULL;
  cf->cfun.owner = mk_cl_Cnil;

  if (anchor != NULL)
    *anchor = cf;

  return cf;
}

void mkcl_build_named_cfun_fun_ref_syms(MKCL, mkcl_object fun, mkcl_object * VV,
					mkcl_object * fun_ref_sym_locs, mkcl_index nb_fun_refs)
{
  fun->cfun.nb_fun_refs = nb_fun_refs;
  fun->cfun.fun_ref_syms 
    = mkcl_build_fun_ref_syms_from_locs(env, VV, fun_ref_sym_locs, nb_fun_refs);
}


mkcl_object
mkcl_build_cdisplay(MKCL, mkcl_object producer, mkcl_object cenv, mkcl_index depth)
{
  mkcl_index i;
  mkcl_object this = cenv;
  mkcl_object dis = mkcl_alloc_cdisplay(env, depth);

  for (i = depth; i > 0; i--)
    {
      if (mkcl_Null(this))
	{
	  if (!mkcl_Null(producer) && (mkcl_type_of(producer) == mkcl_t_cclosure))
	    {
	      mkcl_object outer_env = producer->cclosure.cenv;
	      if (i == outer_env->display.nb_levels)
		{
		  mkcl_index j;
		  const mkcl_index max = outer_env->display.nb_levels;
		  mkcl_object * const level = dis->display.level;
		  mkcl_object * const outer_level = outer_env->display.level;
		  for (j = 0; j < max; j++) level[j] = outer_level[j];
		}
	      else
		{
#if 0
		  printf("\nError in closure creation, producer expected depth = %d, producer effective depth = %d.\n",
			 (int) i, (int) outer_env->display.nb_levels);
#endif
		  mkcl_lose(env, "Closure environment botch! Invalid producer depth.");
		}
	    }
	  else
	    {
#if 0
	      printf("\n+++ Closure environment is too short (%d) for depth %d\n", (int) i, (int) depth);
#endif
	      mkcl_lose(env, "Closure environment botch! Too few levels.");
	    }
	  break;
	}

      dis->display.level[i-1] = this;

      this = this->lblock.outer;
    }
  if (!mkcl_Null(this) || ((i == 0) && !mkcl_Null(producer) && (mkcl_type_of(producer) == mkcl_t_cclosure)))
    {
#if 0
      printf("\n+++ Closure environment is too long (%d) for depth %d\n", (int) i, (int) depth);
#endif
      mkcl_lose(env, "Closure environment botch! Too many levels.");
    }
  return dis;
}

#if 0
bool mkcl_trace_closure_creation = FALSE;

mkcl_object
mk_si_trace_closure_creation(MKCL)
{
  mkcl_trace_closure_creation = TRUE;
  mkcl_return_value(mk_cl_Ct);
}

mkcl_object
mk_si_untrace_closure_creation(MKCL)
{
  mkcl_trace_closure_creation = FALSE;
  mkcl_return_value(mk_cl_Cnil);
}
#endif



static mkcl_object wrong_num_args_cclosure_0(MKCL)
{
  mkcl_object fun = env->function;

  mkcl_FEwrong_num_arguments(env, fun, fun->cclosure.narg, fun->cclosure.narg, 0);
  return mk_cl_Cnil;
}

static mkcl_object wrong_num_args_cclosure_1(MKCL)
{
  mkcl_object fun = env->function;

  mkcl_FEwrong_num_arguments(env, fun, fun->cclosure.narg, fun->cclosure.narg, 1);
  return mk_cl_Cnil;
}

static mkcl_object wrong_num_args_cclosure_2(MKCL)
{
  mkcl_object fun = env->function;

  mkcl_FEwrong_num_arguments(env, fun, fun->cclosure.narg, fun->cclosure.narg, 2);
  return mk_cl_Cnil;
}

static mkcl_object wrong_num_args_cclosure_3(MKCL)
{
  mkcl_object fun = env->function;

  mkcl_FEwrong_num_arguments(env, fun, fun->cclosure.narg, fun->cclosure.narg, 3);
  return mk_cl_Cnil;
}

static mkcl_object wrong_num_args_cclosure_4(MKCL)
{
  mkcl_object fun = env->function;

  mkcl_FEwrong_num_arguments(env, fun, fun->cclosure.narg, fun->cclosure.narg, 4);
  return mk_cl_Cnil;
}

static mkcl_object wrong_num_args_cclosure_va(MKCL, mkcl_narg narg, ...)
{
  mkcl_object fun = env->function;

  mkcl_FEwrong_num_arguments(env, fun, fun->cclosure.narg, fun->cclosure.narg, narg);
  return mk_cl_Cnil;
}


mkcl_object
mkcl_make_cclosure(MKCL, mkcl_object producer, mkcl_objectfn_fixed c_function, int narg,
		   mkcl_index depth, mkcl_object syms_cenv, mkcl_object cenv,
		   mkcl_object block, int position)
{
  mkcl_object cc;

  cc = mkcl_alloc_raw_cclosure(env);
  cc->cclosure.f.entry = wrong_num_args_cclosure_va;
  cc->cclosure.f._[0] = ((narg == 0) ? c_function : (mkcl_objectfn_fixed) wrong_num_args_cclosure_0);
  cc->cclosure.f._[1] = ((narg == 1) ? c_function : (mkcl_objectfn_fixed) wrong_num_args_cclosure_1);
  cc->cclosure.f._[2] = ((narg == 2) ? c_function : (mkcl_objectfn_fixed) wrong_num_args_cclosure_2);
  cc->cclosure.f._[3] = ((narg == 3) ? c_function : (mkcl_objectfn_fixed) wrong_num_args_cclosure_3);
  cc->cclosure.f._[4] = ((narg == 4) ? c_function : (mkcl_objectfn_fixed) wrong_num_args_cclosure_4);
  cc->cclosure.syms_cenv = syms_cenv;
  cc->cclosure.block = block;
  cc->cclosure.name = mk_cl_Cnil;
  cc->cclosure.file = block->cblock.source;
  cc->cclosure.file_position = MKCL_MAKE_FIXNUM(position);
  cc->cclosure.narg = narg;
  cc->cclosure.producer = producer;
  cc->cclosure.owner = mk_cl_Cnil;
  if (mkcl_Null(producer))
    {
      if (!mkcl_Null(block) && mkcl_type_of(block) == mkcl_t_codeblock)
	{
	  cc->cclosure.nb_fun_refs = block->cblock.nb_fun_refs;
	  cc->cclosure.fun_ref_syms = block->cblock.fun_ref_syms;
	  cc->cclosure.fun_refs = block->cblock.fun_refs;
	}
      else
	{ /* This case is possible if the closure is produced 
	     directly by the C runtime code, like in mk_si_hash_table_iterator(). */
	  cc->cclosure.nb_fun_refs = 0;
	  cc->cclosure.fun_ref_syms = NULL;
	  cc->cclosure.fun_refs = NULL;
	}
    }
  else if (mkcl_type_of(producer) == mkcl_t_cclosure)
    {
      cc->cclosure.nb_fun_refs = producer->cclosure.nb_fun_refs;
      cc->cclosure.fun_ref_syms = producer->cclosure.fun_ref_syms;
      cc->cclosure.fun_refs = producer->cclosure.fun_refs;
    }
  else if (mkcl_type_of(producer) == mkcl_t_cfun)
    {
      cc->cclosure.nb_fun_refs = producer->cfun.nb_fun_refs;
      cc->cclosure.fun_ref_syms = producer->cfun.fun_ref_syms;
      cc->cclosure.fun_refs = producer->cfun.fun_refs;
    }
  else
    mkcl_lose(env, "In mkcl_make_cclosure(): Invalid producer type");

  cc->cclosure.cenv = mkcl_build_cdisplay(env, producer, cenv, depth);

#if 0
  if (mkcl_trace_closure_creation)
    {
      if (!mkcl_Null(producer))
	{
	  mkcl_object producer_name = mk_si_coerce_to_base_string(env, producer->cfun.name);
	  fprintf(stderr, "\nclosure created by: %s\n", producer_name->base_string.self);
	}
      else
	fprintf(stderr, "\nclosure created by: nil\n");
      fflush(stderr);
    }
#endif

  return cc;
}

mkcl_object
mkcl_make_cclosure_va(MKCL, mkcl_object producer, mkcl_objectfn c_function,
		      mkcl_index depth, mkcl_object syms_cenv, mkcl_object cenv,
		      mkcl_object block, int position)
{
  mkcl_object cc;

  cc = mkcl_alloc_raw_cclosure(env);
  cc->cclosure.f.entry = c_function;
  cc->cclosure.f._[0] = f0;
  cc->cclosure.f._[1] = f1;
  cc->cclosure.f._[2] = f2;
  cc->cclosure.f._[3] = f3;
  cc->cclosure.f._[4] = f4;
  cc->cclosure.syms_cenv = syms_cenv;
  cc->cclosure.block = block;
  cc->cclosure.name = mk_cl_Cnil;
  cc->cclosure.file = block->cblock.source;
  cc->cclosure.file_position = MKCL_MAKE_FIXNUM(position);
  cc->cclosure.narg = -1;
  cc->cclosure.producer = producer;
  cc->cclosure.owner = mk_cl_Cnil;
  if (mkcl_Null(producer))
    {
      if (!mkcl_Null(block) && mkcl_type_of(block) == mkcl_t_codeblock)
	{
	  cc->cclosure.nb_fun_refs = block->cblock.nb_fun_refs;
	  cc->cclosure.fun_ref_syms = block->cblock.fun_ref_syms;
	  cc->cclosure.fun_refs = block->cblock.fun_refs;
	}
      else
	{ /* This case is possible if the closure is produced 
	     directly by the C runtime code, like in mk_si_hash_table_iterator(). */
	  cc->cclosure.nb_fun_refs = 0;
	  cc->cclosure.fun_ref_syms = NULL;
	  cc->cclosure.fun_refs = NULL;
	}
    }
  else if (mkcl_type_of(producer) == mkcl_t_cclosure)
    {
      cc->cclosure.nb_fun_refs = producer->cclosure.nb_fun_refs;
      cc->cclosure.fun_ref_syms = producer->cclosure.fun_ref_syms;
      cc->cclosure.fun_refs = producer->cclosure.fun_refs;
    }
  else if (mkcl_type_of(producer) == mkcl_t_cfun)
    {
      cc->cclosure.nb_fun_refs = producer->cfun.nb_fun_refs;
      cc->cclosure.fun_ref_syms = producer->cfun.fun_ref_syms;
      cc->cclosure.fun_refs = producer->cfun.fun_refs;
    }
  else
    mkcl_lose(env, "In mkcl_make_cclosure_va(): Invalid producer type");

  cc->cclosure.cenv = mkcl_build_cdisplay(env, producer, cenv, depth);

#if 0
  if (mkcl_trace_closure_creation)
    {
      if (!mkcl_Null(producer))
	{
	  mkcl_object producer_name = mk_si_coerce_to_base_string(env, producer->cfun.name);
	  fprintf(stderr, "\nclosure(va) created by: %s\n", producer_name->base_string.self);
	}
      else
	fprintf(stderr, "\nclosure(va) created by: nil\n");
      fflush(stderr);
    }
#endif

  return cc;
}

static mkcl_object
clone_cenv(MKCL, mkcl_object cenv)
{
  mkcl_index depth = cenv->display.nb_levels;
  mkcl_object dis = mkcl_alloc_cdisplay(env, depth);
  mkcl_object outer_block = mk_cl_Cnil;
  mkcl_index i = 0;

  while (i < depth)  /* clone each level. */
    {
      mkcl_object b = cenv->display.level[i];
      mkcl_index nb_vars = b->lblock.nb_vars;
      mkcl_object new_b = mkcl_alloc_clevel_block(env, b->lblock.producer, outer_block, nb_vars);
      mkcl_index j;

      for (j = 0; j < nb_vars; j++)
	new_b->lblock.var[j] = b->lblock.var[j];  /* copy each variable of this level. */

      dis->display.level[i] = outer_block = new_b;
      i++;
    }
  return dis;
}

static mkcl_object
mkcl_clone_cclosure(MKCL, mkcl_object c0, mkcl_object new_cenv)
{
  mkcl_object cc;

  if (mkcl_Null(new_cenv))
    new_cenv = clone_cenv(env, c0->cclosure.cenv);

  cc = mkcl_alloc_raw_cclosure(env);

  cc->cclosure = c0->cclosure;
  cc->cclosure.cenv = new_cenv;

  return cc;
}

/* For si:clone-closure to work as intended all closures given to it as argument
   must be twin closures (ie: they were closed over the same environment).
   Any other object will be passed through as is to the value list without any cloning.
*/
mkcl_object mk_si_clone_closure(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object head = mk_cl_Cnil;
    mkcl_setup_for_rest(env, MK_SI_clone_closure, 0, narg, narg, args);
    {
      mkcl_object tail;
      mkcl_object c0;
      mkcl_object c0_env = mk_cl_Cnil;
      mkcl_object clone;
      mkcl_object clone_env;
      mkcl_object (* cloner)(MKCL, mkcl_object c0, mkcl_object new_env);

      while (narg && mkcl_Null(c0_env))
        {
          c0 = mkcl_va_arg(args);
          switch(mkcl_type_of(c0))
            {
            case mkcl_t_bclosure:
              c0_env = c0->bclosure.lex;
              cloner = mkcl_clone_bclosure;
              clone = cloner(env, c0, mk_cl_Cnil);
              clone_env = clone->bclosure.lex;
              break;
            case mkcl_t_cclosure:
              c0_env = c0->cclosure.cenv;
              cloner = mkcl_clone_cclosure;
              clone = cloner(env, c0, mk_cl_Cnil);
              clone_env = clone->cclosure.cenv;
              break;
            default:
              clone = c0;
              break;
            }
          if (mkcl_Null(head))
            {
              tail = head = mkcl_list1(env, clone);
            }
          else
            {
              mkcl_object cons = mkcl_list1(env, clone);
              MKCL_RPLACD(tail, cons);
              tail = cons;
            }
          narg--;
        }
    
      while (narg--) {
        mkcl_object c1 = mkcl_va_arg(args);
        mkcl_object c1_env;

        switch(mkcl_type_of(c1))
          {
          case mkcl_t_bclosure: c1_env = c1->bclosure.lex; break;
          case mkcl_t_cclosure: c1_env = c1->cclosure.cenv; break;
          default: c1_env = mk_cl_Cnil; break;
          }
      
        if ( c0_env == c1_env ) /* Twin closures? */
          clone = cloner(env, c1, clone_env);
        else
          clone = c1;

        mkcl_object cons = mkcl_list1(env, clone);
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(args);
    mkcl_return_value(head);
  }
}

mkcl_object
mk_si_closure_siblings_p(MKCL, mkcl_object c1, mkcl_object c2)
{
  mkcl_call_stack_check(env);
  mkcl_object c1_env = mk_si_closure_env(env, c1);

  if (mkcl_Null(c1_env))
    { mkcl_return_value(mk_cl_Cnil); }
  else
    { mkcl_return_value(((c1_env == mk_si_closure_env(env, c2)) ? mk_cl_Ct : mk_cl_Cnil)); }
}


void
mkcl_FEinvalid_cdisplay(MKCL, mkcl_object obj)
{
  mkcl_FEwrong_type_argument(env, MK_SI_compiled_closure_display, obj);
}


mkcl_object
mk_si_closure_depth(MKCL, mkcl_object disp)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(disp) != mkcl_t_cdisplay)
    mkcl_FEinvalid_cdisplay(env, disp);
  mkcl_return_value(mkcl_make_unsigned_integer(env, disp->display.nb_levels));
}

mkcl_object
mk_si_closure_level(MKCL, mkcl_object disp, mkcl_object i)
{
  mkcl_call_stack_check(env);
  mkcl_index index = mkcl_integer_to_index(env, i);

  if (mkcl_type_of(disp) != mkcl_t_cdisplay)
    mkcl_FEinvalid_cdisplay(env, disp);

  if (disp->display.nb_levels > index)
    { mkcl_return_value(disp->display.level[index]); }
  else
    { mkcl_return_value(mk_cl_Cnil); }
}

mkcl_object
mk_si_closure_level_size(MKCL, mkcl_object level)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(level) != mkcl_t_clevel_block)
    mkcl_FEwrong_type_argument(env, MK_SI_compiled_closure_level, level);
  mkcl_return_value(mkcl_make_unsigned_integer(env, level->lblock.nb_vars));
}

mkcl_object
mk_si_closure_level_var(MKCL, mkcl_object level, mkcl_object i)
{
  mkcl_call_stack_check(env);
  mkcl_index index = mkcl_integer_to_index(env, i);

  if (mkcl_type_of(level) != mkcl_t_clevel_block)
    mkcl_FEwrong_type_argument(env, MK_SI_compiled_closure_level, level);

  if (level->lblock.nb_vars > index)
    { mkcl_return_value(level->lblock.var[index]); }
  else
    { mkcl_return_value(MKCL_OBJNULL); }
}

mkcl_object
mk_si_closure_level_set_var(MKCL, mkcl_object level, mkcl_object i, mkcl_object val)
{
  mkcl_call_stack_check(env);
  mkcl_index index = mkcl_integer_to_index(env, i);

  if (mkcl_type_of(level) != mkcl_t_clevel_block)
    mkcl_FEwrong_type_argument(env, MK_SI_compiled_closure_level, level);

  if (level->lblock.nb_vars > index)
    level->lblock.var[index] = val;
    
  mkcl_return_value(val);
}

mkcl_object
mk_si_closure_level_outer_level(MKCL, mkcl_object level)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(level) != mkcl_t_clevel_block)
    mkcl_FEwrong_type_argument(env, MK_SI_compiled_closure_level, level);
  mkcl_return_value(level->lblock.outer);
}

void
mkcl_def_c_function(MKCL, mkcl_object sym, mkcl_objectfn_fixed c_function, int narg)
{
  mk_si_fset(env, 2, sym, mkcl_make_cfun(env, c_function, sym, mkcl_symbol_value(env, MK_SI_DYNVAR_cblock), narg, NULL));
}

void
mkcl_def_c_macro(MKCL, mkcl_object sym, mkcl_objectfn_fixed c_function, int narg)
{
  mk_si_fset(env, 3, sym, mkcl_make_cfun(env, c_function, sym, mkcl_symbol_value(env, MK_SI_DYNVAR_cblock), 2, NULL), mk_cl_Ct);
}

void
mkcl_def_c_macro_va(MKCL, mkcl_object sym, mkcl_objectfn c_function)
{
  mk_si_fset(env, 3, sym, mkcl_make_cfun_va(env, c_function, sym, mkcl_symbol_value(env, MK_SI_DYNVAR_cblock), NULL), mk_cl_Ct);
}

void
mkcl_def_c_function_va(MKCL, mkcl_object sym, mkcl_objectfn c_function)
{
  mk_si_fset(env, 2, sym, mkcl_make_cfun_va(env, c_function, sym, mkcl_symbol_value(env, MK_SI_DYNVAR_cblock), NULL));
}

mkcl_object
mk_si_compiled_function_name(MKCL, mkcl_object fun)
{
  mkcl_object output;

  mkcl_call_stack_check(env);
  switch(mkcl_type_of(fun)) {
  case mkcl_t_bclosure:
    output = fun->bclosure.name; break;
  case mkcl_t_bytecode:
    output = fun->bytecode.name; break;
  case mkcl_t_cfun:
    output = fun->cfun.name; break;
  case mkcl_t_cclosure:
    output = fun->cclosure.name; break;
  default:
    mkcl_FEinvalid_function(env, fun);
  }
  mkcl_return_value(output);
}

mkcl_object
mk_si_set_compiled_function_name(MKCL, mkcl_object fun, mkcl_object name)
{
  mkcl_call_stack_check(env);
  switch(mkcl_type_of(fun)) {
  case mkcl_t_bclosure:
    fun->bclosure.name = name; break;
  case mkcl_t_bytecode:
    fun->bytecode.name = name; break;
  case mkcl_t_cfun:
    fun->cfun.name = name; break;
  case mkcl_t_cclosure:
    fun->cclosure.name = name; break;
  default:
    mkcl_FEinvalid_function(env, fun);
  }
  mkcl_return_value(name);
}

mkcl_object
mk_cl_function_lambda_expression(MKCL, mkcl_object fun)
{
  mkcl_object lambda_expr, name = mk_cl_Cnil, closure_p = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  switch(mkcl_type_of(fun)) {
  case mkcl_t_bclosure:
    closure_p = mk_cl_Ct;
    fun = fun->bclosure.code;
    goto mkcl_t_bytecode_case; /* fallthrough is such bad taste! */
  case mkcl_t_bytecode:
  mkcl_t_bytecode_case:
    name = fun->bytecode.name;
    lambda_expr = fun->bytecode.definition;
    if (name == mk_cl_Cnil)
      lambda_expr = mk_cl_cons(env, MK_CL_lambda, lambda_expr);
    else if (name != MK_SI_bytecode)
      lambda_expr = mk_cl_listX(env, 3, MK_SI_lambda_block, name, lambda_expr);
    break;
  case mkcl_t_cfun:
    name = fun->cfun.name;
    closure_p = mk_cl_Cnil;
    lambda_expr = mk_cl_Cnil;
    break;
  case mkcl_t_cclosure:
    name = fun->cclosure.name;
    closure_p = mk_cl_Ct;
    lambda_expr = mk_cl_Cnil;
    break;
  case mkcl_t_instance:
    if (fun->instance.isgf)
      {
      name = mk_cl_Cnil;
      closure_p = mk_cl_Cnil;
      lambda_expr = mk_cl_Cnil;
      }
    else
      mkcl_FEinvalid_function(env, fun);
    break;
  default:
    mkcl_FEinvalid_function(env, fun);
  }
  mkcl_return_3_values(lambda_expr, closure_p, name);
}

mkcl_object
mk_si_closurep(MKCL, mkcl_object fun)
{
  mkcl_call_stack_check(env);
  switch(mkcl_type_of(fun))
    {
    case mkcl_t_bclosure:
    case mkcl_t_cclosure:
      mkcl_return_value(mk_cl_Ct);
    default: mkcl_return_value(mk_cl_Cnil);
    }
}

mkcl_object
mk_si_closure_env(MKCL, mkcl_object fun)
{
  mkcl_call_stack_check(env);
  switch(mkcl_type_of(fun))
    {
    case mkcl_t_bclosure:
      mkcl_return_2_values(fun->bclosure.lex, mk_cl_Cnil);
    case mkcl_t_cclosure:
      mkcl_return_2_values(fun->cclosure.cenv, fun->cclosure.syms_cenv);
    default: mkcl_return_2_values(mk_cl_Cnil, mk_cl_Cnil);
    }
}

mkcl_object
mk_si_closure_producer(MKCL, mkcl_object fun)
{
  mkcl_call_stack_check(env);
  switch(mkcl_type_of(fun))
    {
    case mkcl_t_bclosure:
      mkcl_return_value(fun->bclosure.producer);
    case mkcl_t_cclosure:
      mkcl_return_value(fun->cclosure.producer);
    default: mkcl_return_value(mk_cl_Cnil);
    }
}

mkcl_object
mk_si_compiled_function_block(MKCL, mkcl_object fun)
{
  mkcl_object output;

  mkcl_call_stack_check(env);
  switch(mkcl_type_of(fun)) {
  case mkcl_t_cfun:
    output = fun->cfun.block; break;
  case mkcl_t_cclosure:
    output = fun->cclosure.block; break;
  default:
    mkcl_FEerror(env, "~S is not a C compiled function.", 1, fun);
  }
  mkcl_return_value(output);
}

mkcl_object
mk_si_compiled_function_file(MKCL, mkcl_object b)
{
  mkcl_call_stack_check(env);
 BEGIN:
  switch (mkcl_type_of(b)) {
  case mkcl_t_bclosure:
    b = b->bclosure.code;
    goto BEGIN;
  case mkcl_t_bytecode:
    mkcl_return_2_values(b->bytecode.file, b->bytecode.file_position);
  case mkcl_t_cfun:
    mkcl_return_2_values(b->cfun.file, b->cfun.file_position);
  case mkcl_t_cclosure:
    mkcl_return_2_values(b->cclosure.file, b->cclosure.file_position);
  default:
    mkcl_return_2_values(mk_cl_Cnil, mk_cl_Cnil);
  }
}

void
mkcl_set_function_source_file_info(MKCL, mkcl_object b, mkcl_object source, mkcl_object position)
{
 BEGIN:
  switch (mkcl_type_of(b)) {
  case mkcl_t_bclosure:
    b = b->bclosure.code;
    goto BEGIN;
  case mkcl_t_bytecode:
    b->bytecode.file = source;
    b->bytecode.file_position = position;
    break;
  case mkcl_t_cfun:
    b->cfun.file = source;
    b->cfun.file_position = position;
    break;
  case mkcl_t_cclosure:
    b->cclosure.file = source;
    b->cclosure.file_position = position;
    break;
  default:
    mkcl_FEerror(env, "~S is not a compiled function.", 1, b);
  }
}

mkcl_object
mk_si_compiled_function_owner(MKCL, mkcl_object fun)
{
  mkcl_object owner;

  mkcl_call_stack_check(env);
  switch(mkcl_type_of(fun)) {
  case mkcl_t_bclosure:
    owner = fun->bclosure.owner; break;
  case mkcl_t_bytecode:
    owner = fun->bytecode.owner; break;
  case mkcl_t_cfun:
    owner = fun->cfun.owner; break;
  case mkcl_t_cclosure:
    owner = fun->cclosure.owner; break;
  default:
    mkcl_FEinvalid_function(env, fun);
  }
  mkcl_return_value(owner);
}

mkcl_object
mk_si_set_compiled_function_owner(MKCL, mkcl_object fun, mkcl_object owner)
{
  mkcl_call_stack_check(env);
  switch(mkcl_type_of(fun)) {
  case mkcl_t_bclosure:
    fun->bclosure.owner = owner; break;
  case mkcl_t_bytecode:
    fun->bytecode.owner = owner; break;
  case mkcl_t_cfun:
    fun->cfun.owner = owner; break;
  case mkcl_t_cclosure:
    fun->cclosure.owner = owner; break;
  default:
    mkcl_FEinvalid_function(env, fun);
  }
  mkcl_return_value(owner);
}

void
mkcl_cmp_defmacro(MKCL, mkcl_object fun)
{
  switch (mkcl_type_of(fun))
    {
    case mkcl_t_cfun:
    case mkcl_t_cclosure:
      break;
    default:
      mkcl_lose(env, "\nmkcl_cmp_defmacro received an invalid function object!\n");
      mkcl_FEerror(env, "In mkcl_cmp_defmacro ~S is not a compiled function.", 1, fun);
    }
  mk_si_fset(env, 3, fun->cfun.name, fun, mk_cl_Ct);

  if (mkcl_type_of(fun) == mkcl_t_cfun)
    fun->cfun.fun_refs = mkcl_build_fun_refs_from_syms(env, fun, fun->cfun.fun_ref_syms, fun->cfun.nb_fun_refs);
}

void
mkcl_cmp_defun(MKCL, mkcl_object fun)
{
  switch (mkcl_type_of(fun))
    {
    case mkcl_t_cfun:
    case mkcl_t_cclosure:
      break;
    default:
      mkcl_lose(env, "\nmkcl_cmp_defun received an invalid function object!\n");
      mkcl_FEerror(env, "In mkcl_cmp_defun ~S is not a compiled function.", 1, fun);
    }
  mk_si_fset(env, 2, fun->cfun.name, fun);

  if (mkcl_type_of(fun) == mkcl_t_cfun)
    fun->cfun.fun_refs = mkcl_build_fun_refs_from_syms(env, fun, fun->cfun.fun_ref_syms, fun->cfun.nb_fun_refs);
}

mkcl_object mkcl_fix_lambda_fun_refs(MKCL, mkcl_object * VV, mkcl_object * fun_ref_sym_locs,
				     mkcl_index nb_fun_refs, mkcl_object fun)
{
  if (mkcl_type_of(fun) == mkcl_t_cfun && (fun->cfun.fun_refs == NULL))
    {
      fun->cfun.nb_fun_refs = nb_fun_refs;
      fun->cfun.fun_ref_syms 
	= mkcl_build_fun_ref_syms_from_locs(env, VV, fun_ref_sym_locs, nb_fun_refs);
      fun->cfun.fun_refs 
	= mkcl_build_fun_refs_from_syms(env, fun, fun->cfun.fun_ref_syms, fun->cfun.nb_fun_refs);
    }

  return fun;
}

mkcl_object
mkcl_debug_make_cfun(MKCL, mkcl_objectfn_fixed c_function,
		     mkcl_object name, mkcl_object cblock, int narg,
		     mkcl_object * anchor, char * source, int position)
{
  mkcl_object fun = mkcl_make_cfun(env, c_function, name, cblock, narg, anchor);

  mkcl_set_function_source_file_info(env, fun, mkcl_make_simple_base_string(env, source), MKCL_MAKE_FIXNUM(position));
  return fun;
}


mkcl_object mkcl_fix_lex_local_fun_refs(MKCL, mkcl_object producer, mkcl_object fun)
{
  if (mkcl_type_of(fun) == mkcl_t_cfun && (fun->cfun.fun_refs == NULL))
    {
      if (mkcl_Null(producer))
	{
	  mkcl_object cblock = fun->cfun.block;

	  if (!mkcl_Null(cblock) && mkcl_type_of(cblock) == mkcl_t_codeblock)
	    {
	      fun->cfun.nb_fun_refs = cblock->cblock.nb_fun_refs;
	      fun->cfun.fun_ref_syms = cblock->cblock.fun_ref_syms;
	      fun->cfun.fun_refs = cblock->cblock.fun_refs;
	    }
	  else
	    { /* This case is possible if the closure is produced 
		 directly by the C runtime code, like in mk_si_hash_table_iterator(). */
	      fun->cfun.nb_fun_refs = 0;
	      fun->cfun.fun_ref_syms = NULL;
	      fun->cfun.fun_refs = NULL;
	    }
	}
      else if (mkcl_type_of(producer) == mkcl_t_cclosure)
	{
	  fun->cfun.nb_fun_refs = producer->cclosure.nb_fun_refs;
	  fun->cfun.fun_ref_syms = producer->cclosure.fun_ref_syms;
	  fun->cfun.fun_refs = producer->cclosure.fun_refs;
	}
      else if (mkcl_type_of(producer) == mkcl_t_cfun)
	{
	  fun->cfun.nb_fun_refs = producer->cfun.nb_fun_refs;
	  fun->cfun.fun_ref_syms = producer->cfun.fun_ref_syms;
	  fun->cfun.fun_refs = producer->cfun.fun_refs;
	}
      else if (mkcl_type_of(producer) == mkcl_t_codeblock)
	{
	  if (producer == fun->cfun.block)
	    {
	      fun->cfun.nb_fun_refs = producer->cblock.nb_fun_refs;
	      fun->cfun.fun_ref_syms = producer->cblock.fun_ref_syms;
	      fun->cfun.fun_refs = producer->cblock.fun_refs;
	    }
	  else
	    mkcl_lose(env, "In mkcl_fix_lex_local_fun_refs(): Inconsistent producer");
	}
      else
	mkcl_lose(env, "In mkcl_fix_lex_local_fun_refs(): Invalid producer type");
    }

  return fun;
}

mkcl_object
mkcl_debug_make_cfun_va(MKCL, mkcl_objectfn c_function, mkcl_object name,
			mkcl_object cblock, mkcl_object * anchor, char * source, int position)
{
  mkcl_object fun = mkcl_make_cfun_va(env, c_function, name, cblock, anchor);

  mkcl_set_function_source_file_info(env, fun, mkcl_make_simple_base_string(env, source), MKCL_MAKE_FIXNUM(position)); 
  return fun;
}


mkcl_object * mkcl_build_fun_ref_syms_from_locs(MKCL, mkcl_object * VV, mkcl_object * locs, mkcl_index size)
{
  mkcl_object * fun_ref_syms = mkcl_alloc(env, size * sizeof(mkcl_object));
  mkcl_index i;

  for (i = 0; i < size; i++)
    {
      mkcl_object loc = locs[i];
      if (MKCL_FIXNUMP(loc))
	fun_ref_syms[i] = VV[mkcl_fixnum_to_word(loc)];
      else
	fun_ref_syms[i] = loc;
    }
  return fun_ref_syms;
}

static mkcl_object find_fun_ref_in_cblock_locals(MKCL, mkcl_object cblock, mkcl_object sym)
{
  const mkcl_index cfuns_size = cblock->cblock.cfuns_size;
  mkcl_object *cfun_objs = cblock->cblock.cfun_objs;
  mkcl_index i;
  
  for (i = 0; i < cfuns_size; i++)
    {
      mkcl_object cfun = cfun_objs[i];
      if (!mkcl_Null(cfun) && (sym == cfun->cfun.name))
	return cfun;
    }
  return mk_cl_Cnil;
}

mkcl_object * mkcl_build_fun_refs_from_syms(MKCL, mkcl_object fun_or_cblock, mkcl_object * syms, mkcl_index size)
{
  mkcl_object * fun_refs = mkcl_alloc(env, size * sizeof(mkcl_object));
  mkcl_index i;

  for (i = 0; i < size; i++)
    {
      mkcl_object sym = syms[i];

      if (mkcl_Null(mk_cl_fboundp(env, sym))) /* sym is not fboundp. */
	{
	  mkcl_object fun_ref = mk_cl_Cnil;
	  mkcl_object cblock = mk_cl_Cnil;

	  switch (mkcl_type_of(fun_or_cblock))
	    {
	    case mkcl_t_codeblock: cblock = fun_or_cblock; break;
	    case mkcl_t_cfun: cblock = fun_or_cblock->cfun.block; break;
	    case mkcl_t_cclosure: cblock = fun_or_cblock->cclosure.block; break;
	    default: break; /* this case should never happen */
	    }
	  if (!mkcl_Null(cblock))
	    fun_ref = find_fun_ref_in_cblock_locals(env, cblock, sym);
	  if (mkcl_Null(fun_ref))
	    fun_refs[i] = mkcl_funcall2(env, MK_SI_generate_forward_fun_ref_handler->symbol.gfdef, fun_or_cblock, MKCL_MAKE_FIXNUM(i));
	  else
	    fun_refs[i] = fun_ref;
	}
      else 
	{
	  mkcl_object fun = mkcl_fdefinition(env, sym);

	  if ( mkcl_t_cfun == ((fun->d.t) & (((~0UL) << 5) + 3)) ) {
	    fun_refs[i] = fun;
	  } else
	    mkcl_FEinvalid_function(env, fun);
	}
    }
  return fun_refs;
}


mkcl_object mk_si_get_fun_ref_sym(MKCL, mkcl_object fun, mkcl_object index)
{
  mkcl_object output;
  mkcl_index i;

  mkcl_call_stack_check(env);
  i = mkcl_integer_to_index(env, index);

  switch(mkcl_type_of(fun)) {
  case mkcl_t_bclosure:
    output = mk_cl_Cnil; break;
  case mkcl_t_bytecode:
    output = mk_cl_Cnil; break;
  case mkcl_t_cfun:
    output = fun->cfun.fun_ref_syms[i]; break;
  case mkcl_t_cclosure:
    output = fun->cclosure.fun_ref_syms[i]; break;
  case mkcl_t_codeblock:
    output = fun->cblock.fun_ref_syms[i]; break;
  default:
    mkcl_FEinvalid_function(env, fun);
  }
  mkcl_return_value(output);
}

int mkcl_fun_refs_trap(MKCL, mkcl_object fun, const mkcl_object * const fun_refs, mkcl_index i) /* debug JCB */
{
  if (fun != fun_refs[i])
    {
      if ( mkcl_type_of(fun_refs[i]) == mkcl_t_cclosure
	   && !mkcl_Null(fun_refs[i]->cclosure.producer)
	   && mkcl_type_of(fun_refs[i]->cclosure.producer) == mkcl_t_cfun
	   && (MK_SI_generate_forward_fun_ref_handler == fun_refs[i]->cclosure.producer->cfun.name))
	mkcl_funcall1(env, fun_refs[i], fun); /* Invoke forward handler */
    }
  return 1;
}

mkcl_object mkcl_fun_ref_fdefinition(MKCL, const mkcl_object * const fun_refs, mkcl_index i)
{
  const mkcl_object fun_ref = fun_refs[i];

  if (mkcl_type_of(fun_ref) == mkcl_t_cclosure
      && !mkcl_Null(fun_ref->cclosure.producer) 
      && mkcl_type_of(fun_ref->cclosure.producer) == mkcl_t_cfun
      && (MK_SI_generate_forward_fun_ref_handler == fun_ref->cclosure.producer->cfun.name))
    {
      /* We are in the forward reference case. */
      volatile mkcl_object * closure_aux_var = fun_ref->cclosure.cenv->display.level[1]->lblock.var;
      mkcl_object cached_fun_ref = closure_aux_var[0];

      if (mkcl_likely(!mkcl_Null(cached_fun_ref)))
	return cached_fun_ref;
      else
	{
	  /* Here we need to grab the mt::+forward-reference-lock+ */
	  mkcl_object f_r_lock = mkcl_symbol_value(env, MK_MT_CONSTANT_forward_reference_lock);
	  volatile mkcl_object locked = mk_cl_Cnil;
          mkcl_object fname;

	  MKCL_UNWIND_PROTECT_BEGIN(env) {
	    mkcl_interrupt_status old_intr;

	    mkcl_get_interrupt_status(env, &old_intr);
	    mkcl_disable_interrupts(env);
	    locked = mk_mt_get_lock(env, 1, f_r_lock);
	    fname = closure_aux_var[1];
	    mkcl_set_interrupt_status(env, &old_intr);


	    if (mkcl_Null(cached_fun_ref = closure_aux_var[0]))
	      {
		if (mkcl_Null(mk_cl_fboundp(env, fname))) /* fname is not fboundp. */
		  {
		    /* mkcl_FEundefined_function(env, fname); */
		    cached_fun_ref = mk_cl_Cnil;
		  }
		else
		  {
		    cached_fun_ref = mkcl_fdefinition(env, fname);
		    closure_aux_var[0] = cached_fun_ref;
		  }
	      }
	  } MKCL_UNWIND_PROTECT_EXIT {
	    if (!mkcl_Null(locked)) mk_mt_giveup_lock(env, f_r_lock);
	  } MKCL_UNWIND_PROTECT_END;
          if (mkcl_likely(!mkcl_Null(cached_fun_ref)))
            return cached_fun_ref;
          else
            mkcl_FEundefined_function(env, fname);
	}
    }

  return fun_ref;
}


mkcl_object mk_si_update_function_references(MKCL, mkcl_object fun)
{
  mkcl_call_stack_check(env);
  switch (mkcl_type_of(fun))
    {
    case mkcl_t_bclosure:
    case mkcl_t_bytecode:
      mkcl_return_value(fun);
    case mkcl_t_cfun:
      fun->cfun.fun_refs = mkcl_build_fun_refs_from_syms(env, fun, fun->cfun.fun_ref_syms, fun->cfun.nb_fun_refs);
      mkcl_return_value(fun);      
    case mkcl_t_cclosure:
      fun->cclosure.fun_refs = mkcl_build_fun_refs_from_syms(env, fun, fun->cclosure.fun_ref_syms, fun->cclosure.nb_fun_refs);
      mkcl_return_value(fun);
    default:
      mkcl_FEerror(env, "In mk_si_update_function_references ~S is not a function.", 1, fun);
    }
}
