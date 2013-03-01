/* -*- mode: c -*- */
/*
    compiler.c -- Bytecode compiler
*/
/*
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2012, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

/*  Remarks:

    [1] The virtual machine has a word size of 16 bits. Operands and arguments
    have this very size, so that for instance, a jump

		OP_JMP increment

    takes two words of memory: one for the operator and one for the argument.
    The interpreter is written with this assumption in mind, but it should be
    easily modifed, because arguments are retrieved with "next_arg" and
    operators with "next_op".  Parts which will require a careful modification
    are marked with flag [1].
*/
#include <mkcl/mkcl.h>
#include <mkcl/mkcl-inl.h>
#include <mkcl/internal.h>
#include <mkcl/bytecode.h>

#include <string.h>

/********************* EXPORTS *********************/

#define REGISTER_SPECIALS	1
#define IGNORE_DECLARATIONS	0

/* Flags for the compilation routines: */
/* + Push the output of this form */
#define FLAG_PUSH		1
/* + Set the output of this form in VALUES */
#define FLAG_VALUES		2
/* + Set the output of this form in REG0 */
#define FLAG_REG0		4
/* + Search function binding in the global environment */
#define FLAG_GLOBAL		8
/* + Ignore this form */
#define FLAG_IGNORE		0
#define FLAG_USEFUL		(FLAG_PUSH | FLAG_VALUES | FLAG_REG0)

#define MODE_EXECUTE		0
#define MODE_LOAD		1
#define MODE_COMPILE		2
#define MODE_ONLY_LOAD		3

#define ENV_RECORD_LOCATION(r)	MKCL_CADDDR(r)

#define MKCL_SPECIAL_VAR_REF	-2
#define MKCL_UNDEFINED_VAR_REF	-1

/********************* PRIVATE ********************/

typedef struct mkcl_compiler_env *mk_cl_compiler_ptr;

#define asm_begin(env) current_pc(env)
#define current_pc(env) MKCL_TEMP_STACK_INDEX(env)
#define set_pc(env,n) asm_clear(env,n)
#define asm_ref(env,n) (mkcl_word)((env)->temp_stack[n])
static void asm_clear(MKCL, mkcl_index h);
static void asm_op(MKCL, mkcl_word op);
static void asm_op2(MKCL, int op, int arg);
static mkcl_object asm_end(MKCL, mkcl_index handle, mkcl_object definition);
static mkcl_index asm_jmp(MKCL, register int op);
static void asm_complete(MKCL, register int op, register mkcl_index original);

static mkcl_word c_var_ref(MKCL, mkcl_object var, int allow_symbol_macro, bool ensure_defined);

static int c_block(MKCL, mkcl_object args, int flags);
static int c_case(MKCL, mkcl_object args, int flags);
static int c_catch(MKCL, mkcl_object args, int flags);
static int c_compiler_let(MKCL, mkcl_object args, int flags);
static int c_cond(MKCL, mkcl_object args, int flags);
static int c_eval_when(MKCL, mkcl_object args, int flags);
static int c_flet(MKCL, mkcl_object args, int flags);
static int c_funcall(MKCL, mkcl_object args, int flags);
static int c_function(MKCL, mkcl_object args, int flags);
static int c_go(MKCL, mkcl_object args, int flags);
static int c_if(MKCL, mkcl_object args, int flags);
static int c_labels(MKCL, mkcl_object args, int flags);
static int c_let(MKCL, mkcl_object args, int flags);
static int c_leta(MKCL, mkcl_object args, int flags);
static int c_load_time_value(MKCL, mkcl_object args, int flags);
static int c_locally(MKCL, mkcl_object args, int flags);
static int c_macrolet(MKCL, mkcl_object args, int flags);
static int c_multiple_value_bind(MKCL, mkcl_object args, int flags);
static int c_multiple_value_call(MKCL, mkcl_object args, int flags);
static int c_multiple_value_prog1(MKCL, mkcl_object args, int flags);
static int c_multiple_value_setq(MKCL, mkcl_object args, int flags);
static int c_not(MKCL, mkcl_object args, int flags);
static int c_nth_value(MKCL, mkcl_object args, int flags);
static int c_prog1(MKCL, mkcl_object args, int flags);
static int c_progv(MKCL, mkcl_object args, int flags);
static int c_psetq(MKCL, mkcl_object args, int flags);
static int c_values(MKCL, mkcl_object args, int flags);
static int c_setq(MKCL, mkcl_object args, int flags);
static int c_return(MKCL, mkcl_object args, int flags);
static int c_return_from(MKCL, mkcl_object args, int flags);
static int c_symbol_macrolet(MKCL, mkcl_object args, int flags);
static int c_tagbody(MKCL, mkcl_object args, int flags);
static int c_throw(MKCL, mkcl_object args, int flags);
static int c_unwind_protect(MKCL, mkcl_object args, int flags);
static int c_while(MKCL, mkcl_object args, int flags);
static int c_until(MKCL, mkcl_object args, int flags);
static void eval_form(MKCL, mkcl_object form);
static int compile_body(MKCL, mkcl_object args, int flags);
static int compile_form(MKCL, mkcl_object args, int push);

static int c_cons(MKCL, mkcl_object args, int push);
static int c_endp(MKCL, mkcl_object args, int push);
static int c_car(MKCL, mkcl_object args, int push);
static int c_cdr(MKCL, mkcl_object args, int push);
static int c_list(MKCL, mkcl_object args, int push);
static int c_listA(MKCL, mkcl_object args, int push);

static mkcl_object mkcl_make_lambda(MKCL, mkcl_object name, mkcl_object lambda);

static void mkcl_FEill_formed_input(MKCL) /*__attribute__((noreturn))*/;

/* -------------------- SAFE LIST HANDLING -------------------- */

static mkcl_object
pop(MKCL, mkcl_object *l) {
	mkcl_object head, list = *l;
	if (MKCL_ATOM(list))
		mkcl_FEill_formed_input(env);
	head = MKCL_CONS_CAR(list);
	*l = MKCL_CONS_CDR(list);
	return head;
}

static mkcl_object
pop_maybe_nil(MKCL, mkcl_object *l) {
	mkcl_object head, list = *l;
	if (list == mk_cl_Cnil)
		return mk_cl_Cnil;
	if (MKCL_ATOM(list))
		mkcl_FEill_formed_input(env);
	head = MKCL_CONS_CAR(list);
	*l = MKCL_CONS_CDR(list);
	return head;
}

/* ------------------------------ ASSEMBLER ------------------------------ */

static mkcl_object
asm_end(MKCL, mkcl_index beginning, mkcl_object definition) {
        const mk_cl_compiler_ptr c_env = env->c_env;
	mkcl_object bytecode;
	mkcl_index code_size, data_size, i;
	mkcl_opcode *code;
	mkcl_object file = MKCL_SYM_VAL(env,@'*load-truename*');
	mkcl_object end_position = mk_cl_cdr(env, MKCL_SYM_VAL(env,@'si::*source-location*'));

	/* Save bytecode from this session in a new vector */
	code_size = current_pc(env) - beginning;
	data_size = mkcl_length(env, c_env->constants);
	bytecode = mkcl_alloc_raw_bytecode(env);
	bytecode->bytecode.name = @'si::bytecode';
	bytecode->bytecode.definition = definition;
	bytecode->bytecode.code_size = code_size;
	bytecode->bytecode.data_size = data_size;
#if 0
	bytecode->bytecode.code = mkcl_alloc_atomic(env, code_size * sizeof(mkcl_opcode));
#else
	bytecode->bytecode.code = mkcl_alloc(env, code_size * sizeof(mkcl_opcode));
#endif
	bytecode->bytecode.data = (mkcl_object*)mkcl_alloc(env, data_size * sizeof(mkcl_object));
	for (i = 0, code = (mkcl_opcode *)bytecode->bytecode.code; i < code_size; i++) {
		code[i] = (mkcl_opcode)(mkcl_word)(env->temp_stack[beginning+i]);
	}
	for (i=0; i < data_size; i++) {
		bytecode->bytecode.data[i] = MKCL_CONS_CAR(c_env->constants);
		c_env->constants = MKCL_CONS_CDR(c_env->constants);
	}

        bytecode->bytecode.f.entry =  _mkcl_bytecode_dispatch_vararg;
	bytecode->bytecode.f._[0] = _mkcl_bytecode_dispatch_f0;
	bytecode->bytecode.f._[1] = _mkcl_bytecode_dispatch_f1;
	bytecode->bytecode.f._[2] = _mkcl_bytecode_dispatch_f2;
	bytecode->bytecode.f._[3] = _mkcl_bytecode_dispatch_f3;
	bytecode->bytecode.f._[4] = _mkcl_bytecode_dispatch_f4;

        mkcl_set_function_source_file_info(env, bytecode, (file == MKCL_OBJNULL)? mk_cl_Cnil : file,
                                          (file == MKCL_OBJNULL)? mk_cl_Cnil : end_position);
	asm_clear(env, beginning);
	return bytecode;
}

#if defined(MKCL_SMALL_BYTECODE)
static void
asm_arg(MKCL, int n) {
#ifdef MKCL_WORDS_BIGENDIAN
	asm_op(env, (n >> 8) & 0xFF);
	asm_op(env, n & 0xFF);
#else
	asm_op(env, n & 0xFF);
	asm_op(env, (n >> 8) & 0xFF);
#endif
}
#else
#define asm_arg(env,n) asm_op(env,n)
#endif

static void
asm_op(MKCL, mkcl_word code) {
        mkcl_object v = (mkcl_object)code;
        MKCL_TEMP_STACK_PUSH(env,v);
}

static void
asm_clear(MKCL, mkcl_index h) {
        MKCL_TEMP_STACK_SET_INDEX(env, h);
}

static void
asm_op2(MKCL, int code, int n) {
	if (n < -MAX_OPARG || MAX_OPARG < n)
	  mkcl_FEprogram_error(env, "Argument to bytecode is too large", 0);
	asm_op(env, code);
	asm_arg(env, n);
}

static void
asm_constant(MKCL, mkcl_object c)
{
        const mk_cl_compiler_ptr c_env = env->c_env;
	c_env->constants = mkcl_nconc(env, c_env->constants, mkcl_list1(env, c));
}

static mkcl_index
asm_jmp(MKCL, int op) {
	mkcl_index output;
	asm_op(env, op);
	output = current_pc(env);
	asm_arg(env, 0);
	return output;
}

static void
asm_complete(MKCL, int op, mkcl_index pc) {
	mkcl_word delta = current_pc(env) - pc;  /* [1] */
	if (op && (asm_ref(env, pc-1) != op))
	  mkcl_FEprogram_error(env, "Non matching codes in ASM-COMPLETE2", 0);
	else if (delta < -MAX_OPARG || delta > MAX_OPARG)
	  mkcl_FEprogram_error(env, "Too large jump", 0);
	else {
#ifdef MKCL_SMALL_BYTECODE
		unsigned char low = delta & 0xFF;
		char high = delta >> 8;
# ifdef MKCL_WORDS_BIGENDIAN
		env->temp_stack[pc] = (mkcl_object)(mkcl_word)high;
		env->temp_stack[pc+1] = (mkcl_object)(mkcl_word)low;
# else
		env->temp_stack[pc] = (mkcl_object)(mkcl_word)low;
		env->temp_stack[pc+1] = (mkcl_object)(mkcl_word)high;
# endif
#else
		env->temp_stack[pc] = (mkcl_object)(mkcl_word)delta;
#endif
	}
}

/* ------------------------------ COMPILER ------------------------------ */

typedef struct {
  void *symbol;
  int (*compiler)(mkcl_env, mkcl_object, int);
  int lexical_increment;
} compiler_record;

#define DATABASE_SIZE (sizeof(database)/sizeof(database[0]))

static compiler_record const database[] = {
  {@'block', c_block, 1},
  {@'case', c_case, 1},
  {@'catch', c_catch, 1},
  {@'mkcl::compiler-let', c_compiler_let, 0},
  {@'cond', c_cond, 1},
  {@'eval-when', c_eval_when, 0},
  {@'flet', c_flet, 1},
  {@'function', c_function, 1},
  {@'funcall', c_funcall, 0},
  {@'go', c_go, 1},
  {@'if', c_if, 1},
  {@'labels', c_labels, 1},
  {@'let', c_let, 1},
  {@'let*', c_leta, 1},
  {@'locally', c_locally, 0},
  {@'load-time-value', c_load_time_value, 1},
  {@'macrolet', c_macrolet, 0},
  {@'multiple-value-bind', c_multiple_value_bind, 1},
  {@'multiple-value-call', c_multiple_value_call, 1},
  {@'multiple-value-prog1', c_multiple_value_prog1, 1},
  {@'multiple-value-setq', c_multiple_value_setq, 1},
  {@'not', c_not, 1},
  {@'nth-value', c_nth_value, 1},
  {@'null', c_not, 1},
  {@'progn', compile_body, 0},
  {@'prog1', c_prog1, 1},
  {@'progv', c_progv, 1},
  {@'psetq', c_psetq, 1},
  {@'return', c_return, 1},
  {@'return-from', c_return_from, 1},
  {@'setq', c_setq, 1},
  {@'symbol-macrolet', c_symbol_macrolet, 0},
  {@'tagbody', c_tagbody, 1},
  {@'throw', c_throw, 1},
  {@'unwind-protect', c_unwind_protect, 1},
  {@'values', c_values, 1},
  {@'si::while', c_while, 0},
  {@'si::until', c_until, 0},

  /* Extras */

  {@'cons', c_cons, 0},
  {@'car', c_car, 0},
  {@'cdr', c_cdr, 0},
  {@'first', c_car, 0},
  {@'rest', c_cdr, 0},
  {@'list', c_list, 0},
  {@'list*', c_listA, 0},
  {@'endp', c_endp, 0}
};

/* ----------------- LEXICAL ENVIRONMENT HANDLING -------------------- */

static void
mkcl_assert_type_symbol(MKCL, mkcl_object v)
{
	if (mkcl_type_of(v) != mkcl_t_symbol)
	  mkcl_FEprogram_error(env, "Expected a symbol, found ~S.", 1, v);
}

static void
mkcl_FEill_formed_input(MKCL)
{
  mkcl_FEprogram_error(env, "Syntax error: list with too few elements or improperly terminated.", 0);
}

static int
c_register_constant(MKCL, mkcl_object c)
{
        const mk_cl_compiler_ptr c_env = env->c_env;
	mkcl_object p = c_env->constants;
	int n;
	for (n = 0; !mkcl_Null(p); n++, p = MKCL_CONS_CDR(p)) {
	  if (c_env->coalesce && mkcl_eql(env, MKCL_CONS_CAR(p), c)) {
			return n;
		}
	}
	asm_constant(env, c);
	return n;
}

static void
asm_c(MKCL, mkcl_object o) {
	asm_arg(env, c_register_constant(env, o));
}

static void
asm_op2c(MKCL, int code, mkcl_object o) {
	asm_op2(env, code, c_register_constant(env, o));
}

/*
 * Note: the following should match the definitions in cmp/cmpenv.lsp, as
 * well as CMP-ENV-REGISTER-MACROLET (lsp/defmacro.lsp)
 *
 * The compiler environment consists of two lists, one stored in
 * env->variables, the other one stored in env->macros.
 *
 * variable-record =	(:block block-name [used-p | block-object] location) |
 *			(:tag ({tag-name}*) [NIL | tag-object] location) |
 *			(:function function-name used-p [location]) |
 *			(var-name {:special | nil} bound-p [location]) |
 *			(symbol si::symbol-macro macro-function) |
 *			CB | LB | UNWIND-PROTECT |
 *			(:declare declaration-arguments*)
 * macro-record =	(function-name FUNCTION [| function-object]) |
 *			(macro-name si::macro macro-function)
 *			CB | LB | UNWIND-PROTECT
 *
 * A *-NAME is a symbol. A TAG-ID is either a symbol or a number. A
 * MACRO-FUNCTION is a function that provides us with the expansion
 * for that local macro or symbol macro. BOUND-P is true when the
 * variable has been bound by an enclosing form, while it is NIL if
 * the variable-record corresponds just to a special declaration.
 * CB, LB and UNWIND-PROTECT are only used by the C compiler and they
 * denote closure, lexical environment and unwind-protect boundaries.
 *
 * The brackets [] denote differences between the bytecode and C
 * compiler environments, with the first option belonging to the
 * interpreter and the second alternative to the compiler.
 *
 * A LOCATION object is proper to the bytecode compiler and denotes
 * the position of this variable, block, tag or function, in the
 * lexical environment. Currently, it is a CONS with two integers
 * (DEPTH . ORDER), denoting the depth of the nested environments and
 * the position in the environment (from the beginning, not from the
 * tail).
 *
 * The BLOCK-, TAG- and FUNCTION- objects are proper of the compiler
 * and carry further information.
 *
 * The last variable records are devoted to declarations and are only
 * used by the C compiler. Read cmpenv.lsp for more details on the
 * structure of these declaration forms, as they do not completely
 * match those of Common-Lisp.
 */

static mkcl_object
new_location(MKCL, mkcl_object name)
{
        const mk_cl_compiler_ptr c_env = env->c_env;
	return MKCL_CONS(env, MKCL_MAKE_FIXNUM(c_env->env_depth),
                    MKCL_MAKE_FIXNUM(c_env->env_size++));
}

static mkcl_index
c_register_block(MKCL, mkcl_object name)
{
	mkcl_object loc = new_location(env, name);
        const mk_cl_compiler_ptr c_env = env->c_env;
	c_env->variables = MKCL_CONS(env, mk_cl_list(env, 4, @':block', name, mk_cl_Cnil, loc),
                                c_env->variables);
	return mkcl_fixnum_to_word(MKCL_CONS_CDR(loc));
}

static mkcl_index
c_register_tags(MKCL, mkcl_object all_tags)
{
	mkcl_object loc = new_location(env, @':tag');
        const mk_cl_compiler_ptr c_env = env->c_env;
	c_env->variables = MKCL_CONS(env, mk_cl_list(env, 4, @':tag', all_tags, mk_cl_Cnil, loc),
                                c_env->variables);
	return mkcl_fixnum_to_word(MKCL_CONS_CDR(loc));
}

static void
c_register_function(MKCL, mkcl_object name)
{
        const mk_cl_compiler_ptr c_env = env->c_env;
	c_env->variables = MKCL_CONS(env, mk_cl_list(env, 4, @':function', name, mk_cl_Cnil,
                                        new_location(env, name)),
                                c_env->variables);
	c_env->macros = MKCL_CONS(env, mk_cl_list(env, 2, name, @'function'), c_env->macros);
}

static mkcl_object
c_macro_expand1(MKCL, mkcl_object stmt)
{
        const mk_cl_compiler_ptr c_env = env->c_env;
	return mk_cl_macroexpand_1(env, 2, stmt, MKCL_CONS(env, c_env->variables, c_env->macros));
}

static void
c_register_symbol_macro(MKCL, mkcl_object name, mkcl_object exp_fun)
{
        const mk_cl_compiler_ptr c_env = env->c_env;
	c_env->variables = MKCL_CONS(env, mk_cl_list(env, 3, name, @'si::symbol-macro', exp_fun),
                                c_env->variables);
}

/* UNUSED
static void
c_register_macro(MKCL, mkcl_object name, mkcl_object exp_fun)
{
        const mk_cl_compiler_ptr c_env = env->c_env;
	c_env->macros = MKCL_CONS(mk_cl_list(3, name, @'si::macro', exp_fun), c_env->macros);
}
*/

static void
c_register_var(MKCL, mkcl_object var, bool special, bool bound)
{
	/* If this is just a declaration, ensure that the variable was not
	 * declared before as special, to save memory. */
	if (bound || (c_var_ref(env, var, 0, FALSE) >= MKCL_UNDEFINED_VAR_REF)) {
                const mk_cl_compiler_ptr c_env = env->c_env;
		c_env->variables = MKCL_CONS(env, mk_cl_list(env, 4,
							     var,
							     special ? @':special' : mk_cl_Cnil,
							     bound ? mk_cl_Ct : mk_cl_Cnil,
							     new_location(env, var)),
					     c_env->variables);
	}
}

static void
guess_environment(MKCL, mkcl_object interpreter_env)
{
        if (!MKCL_LISTP(interpreter_env))
                return;
	/*
	 * Given the environment of an interpreted function, we guess a
	 * suitable compiler enviroment to compile forms that access the
	 * variables and local functions of this interpreted code.
	 */
	for (interpreter_env = @revappend(env, interpreter_env, mk_cl_Cnil);
	     !mkcl_Null(interpreter_env);
	     interpreter_env = MKCL_CONS_CDR(interpreter_env))
	{
		mkcl_object record = MKCL_CONS_CAR(interpreter_env);
                if (!MKCL_LISTP(record)) {
			c_register_function(env, record);
                } else {
                        mkcl_object record0 = MKCL_CONS_CAR(record);
                        mkcl_object record1 = MKCL_CONS_CDR(record);
                        if (MKCL_SYMBOLP(record0)) {
                                c_register_var(env, record0, FALSE, TRUE);
                        } else if (record1 == MKCL_MAKE_FIXNUM(0)) {
                                c_register_tags(env, mk_cl_Cnil);
                        } else {
                                c_register_block(env, record1);
                        }
                }
	}
}

static void
c_new_env(mkcl_env the_env, mkcl_compiler_env_ptr new, mkcl_object env,
          mkcl_compiler_env_ptr old)
{
	the_env->c_env = new;
	new->stepping = 0;
	new->coalesce = TRUE;
	new->lexical_level = 0;
	new->constants = mk_cl_Cnil;
	new->env_depth = 0;
	new->env_size = 0;
	if (old) {
		if (!mkcl_Null(env))
			mkcl_lose(the_env, "c_new_env with both ENV and OLD");
		new->variables = old->variables;
		new->macros = old->macros;
		new->lexical_level = old->lexical_level;
		new->constants = old->constants;
		new->lex_env = old->lex_env;
		new->env_depth = old->env_depth + 1;
		new->coalesce = old->coalesce;
		new->stepping = old->stepping;
                new->mode = old->mode;
	} else {
		new->variables = MKCL_CAR(env);
		new->macros = MKCL_CDR(env);
		for (env = new->variables; !mkcl_Null(env); env = MKCL_CDR(env)) {
			mkcl_object record = MKCL_CAR(env);
			if (MKCL_ATOM(record))
				continue;
			if (MKCL_SYMBOLP(MKCL_CAR(record)) && MKCL_CADR(record) != @'si::symbol-macro') {
				continue;
			} else {
				new->lexical_level = 1;
				break;
			}
		}
                new->mode = MODE_EXECUTE;
	}
}

static mkcl_object
c_tag_ref(MKCL, mkcl_object the_tag, mkcl_object the_type)
{
	mkcl_word n = 0;
	mkcl_object l;
        const mk_cl_compiler_ptr c_env = env->c_env;
	for (l = c_env->variables; MKCL_CONSP(l); l = MKCL_CONS_CDR(l)) {
		mkcl_object type, name, record = MKCL_CONS_CAR(l);
		if (MKCL_ATOM(record))
			continue;
		type = MKCL_CONS_CAR(record);
                record = MKCL_CONS_CDR(record);
		name = MKCL_CONS_CAR(record);
		if (type == @':tag') {
			if (type == the_type) {
			  mkcl_object label = mkcl_assql(env, the_tag, name);
				if (!mkcl_Null(label)) {
				  return MKCL_CONS(env, MKCL_MAKE_FIXNUM(n), MKCL_CONS_CDR(label));
				}
			}
			n++;
		} else if (type == @':block' || type == @':function') {
			/* We compare with EQUAL, because of (SETF fname) */
		  if (type == the_type && mkcl_equal(env, name, the_tag)) {
				/* Mark as used */
                                record = MKCL_CONS_CDR(record);
				MKCL_RPLACA(record, mk_cl_Ct);
				return MKCL_MAKE_FIXNUM(n);
			}
			n++;
		} else if (mkcl_Null(name)) {
			n++;
		} else {
			/* We are counting only locals and ignore specials
			 * and other declarations */
		}
	}
	return mk_cl_Cnil;
}

static mkcl_word
c_var_ref(MKCL, mkcl_object var, int allow_symbol_macro, bool ensure_defined)
{
	mkcl_word n = 0;
	mkcl_object l, record, special, name;
        const mk_cl_compiler_ptr c_env = env->c_env;
	for (l = c_env->variables; MKCL_CONSP(l); l = MKCL_CONS_CDR(l)) {
		record = MKCL_CONS_CAR(l);
		if (MKCL_ATOM(record))
			continue;
		name = MKCL_CONS_CAR(record);
                record = MKCL_CONS_CDR(record);
		special = MKCL_CONS_CAR(record);
		if (name == @':block' || name == @':tag' || name == @':function') {
			n++;
		} else if (name == @':declare') {
			/* Ignored */
		} else if (name != var) {
			/* Symbol not yet found. Only count locals. */
			if (mkcl_Null(special)) n++;
		} else if (special == @'si::symbol-macro') {
			/* We can only get here when we try to redefine a
			   symbol macro */
			if (allow_symbol_macro)
				return -1;
			mkcl_FEprogram_error(env, "Internal error: symbol macro ~S used as variable",
					1, var);
		} else if (mkcl_Null(special)) {
			return n;
		} else {
			return MKCL_SPECIAL_VAR_REF;
		}
	}
	if (ensure_defined) {
	  l = mkcl_symbol_value(env, @'si::*action-on-undefined-variable*');
		if (l != mk_cl_Cnil) {
		  mkcl_funcall2(env, l,
				mkcl_make_simple_base_string(env,
							     "Undefined variable referenced"
							     " in interpreted code.~%Name: ~A"),
				var);
		}
	}
	return MKCL_UNDEFINED_VAR_REF;
}

static bool
c_declared_special(MKCL, register mkcl_object var, register mkcl_object specials)
{
  return ((mkcl_symbol_type(env, var) & mkcl_stp_special) || mkcl_member_eq(env, var, specials));
}

static void
c_declare_specials(MKCL, mkcl_object specials)
{
	while (!mkcl_Null(specials)) {
		int ndx;
		mkcl_object var = pop(env, &specials);
		ndx = c_var_ref(env, var,0,FALSE);
		if (ndx >= 0 || ndx == MKCL_UNDEFINED_VAR_REF)
			c_register_var(env, var, TRUE, FALSE);
	}
}

static mkcl_object
c_process_declarations(MKCL, mkcl_object body)
{
  @si::process-declarations(env, 1, body);
  body = MKCL_VALUES(1); /* No check on MKCL_NVALUES? JCB*/
  return body;
}

static bool
c_pbind(MKCL, mkcl_object var, mkcl_object specials)
{
	bool special;
	if (!MKCL_SYMBOLP(var))
	  mkcl_FEillegal_variable_name(env, var);
	else if ((special = c_declared_special(env, var, specials))) {
		c_register_var(env, var, TRUE, TRUE);
		asm_op2c(env, OP_PBINDS, var);
	} else {
		c_register_var(env, var, FALSE, TRUE);
		asm_op2c(env, OP_PBIND, var);
	}
	return special;
}

static bool
c_bind(MKCL, mkcl_object var, mkcl_object specials)
{
	bool special;
	if (!MKCL_SYMBOLP(var))
	  mkcl_FEillegal_variable_name(env, var);
	else if ((special = c_declared_special(env, var, specials))) {
		c_register_var(env, var, TRUE, TRUE);
		asm_op2c(env, OP_BINDS, var);
	} else {
		c_register_var(env, var, FALSE, TRUE);
		asm_op2c(env, OP_BIND, var);
	}
	return special;
}

static void
c_undo_bindings(mkcl_env the_env, mkcl_object old_vars, int only_specials)
{
	mkcl_object env;
	mkcl_index num_lexical = 0;
	mkcl_index num_special = 0;
        const mk_cl_compiler_ptr c_env = the_env->c_env;

	for (env = c_env->variables; env != old_vars && !mkcl_Null(env); env = MKCL_CONS_CDR(env))
	{
                mkcl_object record, name, special;
                record = MKCL_CONS_CAR(env);
		name = MKCL_CONS_CAR(record);
                record = MKCL_CONS_CDR(record);
		special = MKCL_CONS_CAR(record);
		if (name == @':block' || name == @':tag') {
			(void)0;
		} else if (name == @':function' || mkcl_Null(special)) {
			if (only_specials == 0) num_lexical++;
		} else if (name == @':declare') {
			/* Ignored */
		} else if (special != @'si::symbol-macro') {
			/* If (third special) = NIL, the variable was declared
			   special, but there is no binding! */
                        record = MKCL_CONS_CDR(record);
			if (!mkcl_Null(MKCL_CONS_CAR(record))) {
				num_special++;
			}
		}
	}
	c_env->variables = env;
	if (num_lexical) asm_op2(the_env, OP_UNBIND, num_lexical);
	if (num_special) asm_op2(the_env, OP_UNBINDS, num_special);
}

static void
compile_setq(MKCL, int op, mkcl_object var)
{
  mkcl_word ndx;

  if (!MKCL_SYMBOLP(var))
    mkcl_FEillegal_variable_name(env, var);
  ndx = c_var_ref(env, var,0,TRUE);
  if (ndx < 0) { /* Not a lexical variable */
    if (mkcl_symbol_type(env, var) & mkcl_stp_constant) {
      mkcl_FEassignment_to_constant(env, var);
    }
    ndx = c_register_constant(env, var);
    if (op == OP_SETQ)
      op = OP_SETQS;
    else if (op == OP_PSETQ)
      op = OP_PSETQS;
    else if (op == OP_VSETQ)
      op = OP_VSETQS;
  }
  asm_op2(env, op, ndx);
}

/*
 * This routine is used to change the compilation flags in optimizers
 * that do not want to push values onto the stack.  Its purpose is to
 * keep ignorable forms ignored, while preserving the value of useful
 * forms. Qualitative behavior:
 *	FLAG_PUSH		-> FLAG_VALUES
 *	FLAG_VALUES		-> FLAG_VALUES
 *	FLAG_REG0		-> FLAG_REG0
 *	FLAG_IGNORE		-> FLAG_IGNORE
 */
static int
maybe_values_or_reg0(int flags) {
	if (flags & FLAG_PUSH)
		return (flags | FLAG_VALUES) & ~FLAG_PUSH;
	else
		return flags;
}

/*
 * This routine is used to change the compilation flags in optimizers
 * that do not want to push values onto the stack, but also do not want
 * to use REG0 (maybe because the call a nested mkcl_interpret()). Ignorable
 * forms are kept ignored:
 *	FLAG_PUSH		-> FLAG_VALUES
 *	FLAG_VALUES		-> FLAG_VALUES
 *	FLAG_REG0		-> FLAG_VALUES
 *	FLAG_IGNORE		-> FLAG_IGNORE
 */
static int
maybe_values(int flags) {
	if (flags & FLAG_USEFUL)
		return (flags & ~(FLAG_PUSH | FLAG_REG0)) | FLAG_VALUES;
	else
		return flags;
}

/*
 * This routine is used to change the compilation flags in optimizers
 * that do not want to push values onto the stack.  Its purpose is to
 * keep ignorable forms ignored, while preserving the value of useful
 * forms. Qualitative behavior:
 *	FLAG_PUSH		-> FLAG_REG0
 *	FLAG_VALUES		-> FLAG_REG0
 *	FLAG_REG0		-> FLAG_REG0
 *	FLAG_IGNORE		-> FLAG_IGNORE
 */
static int
maybe_reg0(int flags) {
	if (flags & FLAG_USEFUL)
		return (flags & ~(FLAG_VALUES | FLAG_PUSH)) | FLAG_REG0;
	else
		return flags;
}

/* -------------------- THE COMPILER -------------------- */

/*
	The OP_BLOCK operator encloses several forms within a block
	named BLOCK_NAME, thus catching any OP_RETFROM whose argument
	matches BLOCK_NAME. The end of this block is marked both by
	the OP_EXIT operator and the LABELZ which is packed within
	the OP_BLOCK operator.

		[OP_BLOCK + name + labelz]
		....
		OP_EXIT_FRAME
	labelz:	...
*/

static int
c_block(MKCL, mkcl_object body, int old_flags) {
	struct mkcl_compiler_env old_env;
	mkcl_object name = pop(env, &body);
	mkcl_object block_record;
	mkcl_index labelz, pc, loc;
	int flags;

	if (!MKCL_SYMBOLP(name))
	  mkcl_FEprogram_error(env, "BLOCK: Not a valid block name, ~S", 1, name);

	old_env = *(env->c_env);
	pc = current_pc(env);

	flags = maybe_values_or_reg0(old_flags);
	loc = c_register_block(env, name);
	block_record = MKCL_CONS_CAR(env->c_env->variables);
	if (mkcl_Null(name)) {
		asm_op(env, OP_DO);
	} else {
		asm_op2c(env, OP_BLOCK, name);
	}
	labelz = asm_jmp(env, OP_FRAME);
	compile_body(env, body, flags);
	if (MKCL_CADDR(block_record) == mk_cl_Cnil) {
		/* Block unused. We remove the enclosing OP_BLOCK/OP_DO */
		*(env->c_env) = old_env;
		set_pc(env, pc);
		return compile_body(env, body, old_flags);
	} else {
		c_undo_bindings(env, old_env.variables, 0);
		asm_op(env, OP_EXIT_FRAME);
		asm_complete(env, 0, labelz);
		return flags;
	}
}

/*
	There are several ways to invoke functions and to handle the
	output arguments. These are

		[OP_CALL + nargs]
		function_name

		[OP_FCALL + nargs]

	 OP_CALL and OP_FCALL leave all arguments in the MKCL_VALUES() array,
	 while OP_PCALL and OP_PFCALL leave the first argument in the
	 stack.

	 OP_CALL and OP_PCALL use the value in MKCL_VALUES(0) to retrieve the
	 function, while OP_FCALL and OP_PFCALL use a value from the
	 stack.
 */
static int
c_arguments(MKCL, mkcl_object args) {
	mkcl_index nargs;
	for (nargs = 0; !mkcl_endp(env, args); nargs++) {
	  compile_form(env, pop(env, &args), FLAG_PUSH);
	}
	return nargs;
}

static int asm_function(MKCL, mkcl_object args, int flags);

static int
c_call(MKCL, mkcl_object args, int flags) {
	mkcl_object name;
	mkcl_index nargs;

	name = pop(env, &args);
	nargs = c_arguments(env, args);
	if (env->c_env->stepping) {
		/* When stepping, we only have one opcode to do function
		 * calls: OP_STEPFCALL. */
		asm_function(env, name, (flags & FLAG_GLOBAL) | FLAG_REG0);
		asm_op2(env, OP_STEPCALL, nargs);
/* 		asm_op(env, OP_POP1); */
		flags = FLAG_VALUES;
	} else if (MKCL_SYMBOLP(name) &&
		   ((flags & FLAG_GLOBAL) || mkcl_Null(c_tag_ref(env, name, @':function'))))
	{
		asm_op2(env, OP_CALLG, nargs);
		asm_c(env, name);
		flags = FLAG_VALUES;
	} else {
		/* Fixme!! We can optimize the case of global functions! */
		asm_function(env, name, (flags & FLAG_GLOBAL) | FLAG_REG0);
		asm_op2(env, OP_CALL, nargs);
		flags = FLAG_VALUES;
	}
	return flags;
}

static int
c_funcall(MKCL, mkcl_object args, int flags) {
	mkcl_object name;
	mkcl_index nargs;

	name = pop(env, &args);
	if (MKCL_CONSP(name)) {
                mkcl_object kind = MKCL_CONS_CAR(name);
		if (kind == @'function') {
		  if (mk_cl_list_length(env, name) != MKCL_MAKE_FIXNUM(2))
		    mkcl_FEprogram_error(env, "FUNCALL: Invalid function name ~S",
						1, name);
		  return c_call(env, MKCL_CONS(env, MKCL_CADR(name), args), flags);
		}
		if (kind == @'quote' && /* JCB */ MKCL_SYMBOLP(MKCL_CADR(name))) {
		  if (mk_cl_list_length(env, name) != MKCL_MAKE_FIXNUM(2))
		    mkcl_FEprogram_error(env, "FUNCALL: Invalid function name ~S",
						1, name);
		  return c_call(env, MKCL_CONS(env, MKCL_CADR(name), args), flags | FLAG_GLOBAL);
		}
	}
	compile_form(env, name, FLAG_PUSH);
	nargs = c_arguments(env, args);
	if (env->c_env->stepping) {
		asm_op2(env, OP_STEPCALL, nargs);
		flags = FLAG_VALUES;
	} else {
		asm_op2(env, OP_FCALL, nargs);
		flags = FLAG_VALUES;
	}
	asm_op(env, OP_POP1);
	return flags;
}

static int
perform_c_case(MKCL, mkcl_object args, int flags) {
	mkcl_object test, clause;

	do {
		if (mkcl_Null(args))
			return compile_body(env, mk_cl_Cnil, flags);
		clause = pop(env, &args);
		if (MKCL_ATOM(clause))
		  mkcl_FEprogram_error(env, "CASE: Illegal clause ~S.", 1, clause);
		test = pop(env, &clause);
	} while (test == mk_cl_Cnil);

	if (@'otherwise' == test || test == mk_cl_Ct) {
	  if (!mkcl_Null(args)) /* JCB */
	    mkcl_FEprogram_error(env, "CASE: otherwise-clause must appear last in list of clauses.", 0); /* JCB */
	  compile_body(env, clause, flags);
	} else {
		mkcl_index labeln, labelz;
		if (MKCL_CONSP(test)) {
		  mkcl_index n = mkcl_length(env, test);
			while (n-- > 1) {
			  mkcl_object v = pop(env, &test);
				asm_op(env, OP_JEQL);
				asm_c(env, v);
				asm_arg(env, n * (OPCODE_SIZE + OPARG_SIZE * 2)
					+ OPARG_SIZE);
			}
			test = MKCL_CONS_CAR(test);
		}
		asm_op(env, OP_JNEQL);
		asm_c(env, test);
		labeln = current_pc(env);
		asm_arg(env, 0);
		compile_body(env, clause, flags);
		if (mkcl_endp(env, args) && !(flags & FLAG_USEFUL)) {
			/* Ther is no otherwise. The test has failed and
			   we need no output value. We simply close jumps. */
			asm_complete(env, 0 & OP_JNEQL, labeln);
		} else {
			labelz = asm_jmp(env, OP_JMP);
			asm_complete(env, 0 & OP_JNEQL, labeln);
			perform_c_case(env, args, flags);
			asm_complete(env, OP_JMP, labelz);
		}
	}
	return flags;
}

static int
c_case(MKCL, mkcl_object clause, int flags) {
  compile_form(env, pop(env, &clause), FLAG_REG0);
	return perform_c_case(env, clause, maybe_values_or_reg0(flags));
}

/*
	The OP_CATCH takes the object in MKCL_VALUES(0) and uses it to catch
	any OP_THROW operation which uses that value as argument. If a
	catch occurs, or when all forms have been properly executed, it
	jumps to LABELZ. LABELZ is packed within the OP_CATCH operator.
		[OP_CATCH + labelz]
		...
		"forms to be caught"
		...
	       	OP_EXIT_FRAME
	labelz:	...
*/

static int
c_catch(MKCL, mkcl_object args, int flags) {
	mkcl_index labelz, loc;
	mkcl_object old_env;

	/* Compile evaluation of tag */
	compile_form(env, pop(env, &args), FLAG_REG0);

	/* Compile binding of tag */
	old_env = env->c_env->variables;
	loc = c_register_block(env, MKCL_MAKE_FIXNUM(0));
	asm_op(env, OP_CATCH);

	/* Compile jump point */
	labelz = asm_jmp(env, OP_FRAME);

	/* Compile body of CATCH */
	compile_body(env, args, FLAG_VALUES);

	c_undo_bindings(env, old_env, 0);
	asm_op(env, OP_EXIT_FRAME);
	asm_complete(env, 0, labelz);

	return FLAG_VALUES;
}

static int
c_compiler_let(MKCL, mkcl_object args, int flags)
{
  mkcl_object bindings;
  mkcl_index old_bds_top_index = env->bds_top - env->bds_org;

  for (bindings = pop(env, &args); !mkcl_endp(env, bindings); ) {
    mkcl_object form = pop(env, &bindings);
    mkcl_object var = pop(env, &form);
    mkcl_object value = pop_maybe_nil(env, &form);
    mkcl_bds_bind(env, var, value);
  }
  flags = compile_body(env, args, flags);
  mkcl_bds_unwind(env, old_bds_top_index);
  return flags;
}

/*
	There are three operators which perform explicit jumps, but
	almost all other operators use labels in one way or
	another.

	1) Jumps are always relative to the place where the jump label
	is retrieved so that if the label is in vector[0], then the
	destination is roughly vector + vector[0].

	2) The three jump forms are

		[OP_JMP + label]	; Unconditional jump
		[OP_JNIL + label]	; Jump if VALUES(0) == mk_cl_Cnil
		[OP_JT + label]		; Jump if VALUES(0) != mk_cl_Cnil

	It is important to remark that both OP_JNIL and OP_JT truncate
	the values stack, so that always MKCL_NVALUES = 1 after performing
	any of these operations.
*/
static int
c_cond(MKCL, mkcl_object args, int flags) {
	mkcl_object test, clause;
	mkcl_index label_nil, label_exit;

	if (mkcl_Null(args))
		return compile_form(env, mk_cl_Cnil, flags);
	clause = pop(env, &args);
	if (MKCL_ATOM(clause))
	  mkcl_FEprogram_error(env, "COND: Illegal clause ~S.",1,clause);
	test = pop(env, &clause);
	flags = maybe_values_or_reg0(flags);
	if (mk_cl_Ct == test) {
		/* Default sentence. If no forms, just output T. */
		if (mkcl_Null(clause))
			compile_form(env, mk_cl_Ct, flags);
		else
			compile_body(env, clause, flags);
	} else {
		/* Compile the test. If no more forms, just output
		   the first value (this is guaranteed by OP_JT), but make
		   sure it is stored in the appropriate place. */
		if (mkcl_Null(args)) {
			if (mkcl_Null(clause)) {
			  c_values(env, mk_cl_list(env, 1,test), flags);
			} else {
				compile_form(env, test, FLAG_REG0);
				if (flags & FLAG_VALUES) asm_op(env, OP_VALUEREG0);
				label_nil = asm_jmp(env, OP_JNIL);
				compile_body(env, clause, flags);
				asm_complete(env, OP_JNIL, label_nil);
			}
		} else if (mkcl_Null(clause)) {
			compile_form(env, test, FLAG_REG0);
			if (flags & FLAG_VALUES) asm_op(env, OP_VALUEREG0);
			label_exit = asm_jmp(env, OP_JT);
			c_cond(env, args, flags);
			asm_complete(env, OP_JT, label_exit);
		} else {
			compile_form(env, test, FLAG_REG0);
			label_nil = asm_jmp(env, OP_JNIL);
			compile_body(env, clause, flags);
			label_exit = asm_jmp(env, OP_JMP);
			asm_complete(env, OP_JNIL, label_nil);
			c_cond(env, args, flags);
			asm_complete(env, OP_JMP, label_exit);
		}
	}
	return flags;
}

/*	The OP_DO operator saves the lexical environment and establishes
	a NIL block to execute the enclosed forms, which are typically
	like the ones shown below. At the exit of the block, either by
	means of a OP_RETFROM jump or because of normal termination,
	the lexical environment is restored, and all bindings undone.

		[OP_DO + labelz]
		...	; bindings
		[JMP + labelt]
	labelb:	...	; body
		...	; stepping forms
	labelt:	...	; test form
		[JNIL + label]
		...	; output form
		OP_EXIT_FRAME
	labelz:

*/
static int
c_while_until(MKCL, mkcl_object body, int flags, bool is_while) {
  mkcl_object test = pop(env, &body);
	mkcl_index labelt, labelb;

	flags = maybe_reg0(flags);

	/* Jump to test */
	labelt = asm_jmp(env, OP_JMP);

	/* Compile body */
	labelb = current_pc(env);
	c_tagbody(env, body, flags);

	/* Compile test */
	asm_complete(env, OP_JMP, labelt);
	compile_form(env, test, FLAG_REG0);
	asm_op(env, is_while? OP_JT : OP_JNIL);
	asm_arg(env, labelb - current_pc(env));

	return flags;
}

static int
c_while(MKCL, mkcl_object body, int flags) {
	return c_while_until(env, body, flags, 1);
}

static int
c_until(MKCL, mkcl_object body, int flags) {
	return c_while_until(env, body, flags, 0);
}

static int
when_execute_p(MKCL, mkcl_object situation)
{
  return mkcl_member_eq(env, @'eval', situation) ||
    mkcl_member_eq(env, @':execute', situation);
}

static int
when_compile_p(MKCL, mkcl_object situation)
{
  return mkcl_member_eq(env, @'compile', situation) ||
    mkcl_member_eq(env, @':compile-toplevel', situation);
}

static int
when_load_p(MKCL, mkcl_object situation)
{
  return mkcl_member_eq(env, @'load', situation) ||
    mkcl_member_eq(env, @':load-toplevel', situation);
}

static int
c_eval_when(MKCL, mkcl_object args, int flags) {
  mkcl_object situation = pop(env, &args);
        int mode = env->c_env->mode;
        if (mode == MODE_EXECUTE) {
	  if (!when_execute_p(env, situation))
                        args = mk_cl_Cnil;
        } else if (mode == MODE_LOAD) {
	  if (when_compile_p(env, situation)) {
                        env->c_env->mode = MODE_COMPILE;
                        eval_form(env, MKCL_CONS(env, @'progn', args));
                        env->c_env->mode = MODE_LOAD;
                        if (!when_load_p(env, situation))
                                args = mk_cl_Cnil;
	  } else if (when_load_p(env, situation)) {
                        env->c_env->mode = MODE_ONLY_LOAD;
                        mode = compile_body(env, args, flags);
                        env->c_env->mode = MODE_LOAD;
                        return mode;
                } else {
                        args = mk_cl_Cnil;
                }
        } else if (mode == MODE_ONLY_LOAD) {
	  if (!when_load_p(env, situation))
                        args = mk_cl_Cnil;
        } else {
	  if (!when_execute_p(env, situation) && !when_compile_p(env, situation))
                        args = mk_cl_Cnil;
        }
        return compile_body(env, args, flags);
}


/*
	The OP_FLET/OP_FLABELS operators change the lexical environment
	to add a few local functions.

		[OP_FLET/OP_FLABELS + nfun + fun1]
		...
		OP_UNBIND nfun
	labelz:
*/
static mkcl_index
c_register_functions(MKCL, mkcl_object l)
{
	mkcl_index nfun;
	for (nfun = 0; !mkcl_endp(env, l); nfun++) {
	  mkcl_object definition = pop(env, &l);
	  mkcl_object name = pop(env, &definition);
		c_register_function(env, name);
	}
	return nfun;
}

static int
c_labels_flet(MKCL, int op, mkcl_object args, int flags) {
  mkcl_object l, def_list = pop(env, &args);
	mkcl_object old_vars = env->c_env->variables;
	mkcl_object old_funs = env->c_env->macros;
	mkcl_index nfun, first = 0;

	if (mkcl_length(env, def_list) == 0) {
		return c_locally(env, args, flags);
	}

	/* If compiling a LABELS form, add the function names to the lexical
	   environment before compiling the functions */
	if (op == OP_FLET)
	  nfun = mkcl_length(env, def_list);
	else
		nfun = c_register_functions(env, def_list);

	/* Push the operator (OP_LABELS/OP_FLET) with the number of functions */
	asm_op2(env, op, nfun);

	/* Compile the local functions now. */
	for (l = def_list; !mkcl_endp(env, l); ) {
	  mkcl_object definition = pop(env, &l);
	  mkcl_object name = pop(env, &definition);
		mkcl_object lambda = mkcl_make_lambda(env, name, definition);
		mkcl_index c = c_register_constant(env, lambda);
		if (first == 0) {
			asm_arg(env, c);
			first = 1;
		}
	}

	/* If compiling a FLET form, add the function names to the lexical
	   environment after compiling the functions */
	if (op == OP_FLET)
		c_register_functions(env, def_list);

	/* Compile the body of the form with the local functions in the lexical
	   environment. */
	flags = c_locally(env, args, flags);

	/* Restore and return */
	c_undo_bindings(env, old_vars, 0);
	env->c_env->macros = old_funs;

	return flags;
}


static int
c_flet(MKCL, mkcl_object args, int flags) {
	return c_labels_flet(env, OP_FLET, args, flags);
}


/*
	There are two operators that produce functions. The first one
	is
		[OP_FUNCTION + name]
	which takes the function binding of SYMBOL. The second one is
		OP_CLOSE
		interpreted
	which encloses the INTERPRETED function in the current lexical
	environment.
*/
static int
c_function(MKCL, mkcl_object args, int flags) {
  mkcl_object function = pop(env, &args);
	if (!mkcl_endp(env, args))
	  mkcl_FEprogram_error(env, "FUNCTION: Too many arguments.", 0);
	return asm_function(env, function, flags);
}

static int
asm_function(MKCL, mkcl_object function, int flags) {
  if (!mkcl_Null(mk_si_valid_function_name_p(env, function))) {
    mkcl_object ndx = c_tag_ref(env, function, @':function');
    if (mkcl_Null(ndx)) {
      /* Globally defined function */
      asm_op2c(env, OP_FUNCTION, function);
      return FLAG_REG0;
    } else {
      /* Function from a FLET/LABELS form */
      asm_op2(env, OP_LFUNCTION, mkcl_fixnum_to_word(ndx));
      return FLAG_REG0;
    }
  }
  if (MKCL_CONSP(function)) {
    mkcl_object kind = MKCL_CONS_CAR(function);
    mkcl_object form = MKCL_CONS_CDR(function);
    if (kind == @'lambda') {
      asm_op2c(env, OP_CLOSE, mkcl_make_lambda(env, mk_cl_Cnil, form));
      return FLAG_REG0;
    } else if (kind == @'si::lambda-block') {
      mkcl_object name = MKCL_CONS_CAR(form);
      mkcl_object body = MKCL_CONS_CDR(form);
      asm_op2c(env, OP_CLOSE, mkcl_make_lambda(env, name, body));
      return FLAG_REG0;
    }
  }
  mkcl_FEprogram_error(env, "FUNCTION: Not a valid argument ~S.", 1, function);
  return FLAG_REG0;
}


static int
c_go(MKCL, mkcl_object args, int flags) {
  mkcl_object tag = pop(env, &args);
	mkcl_object info = c_tag_ref(env, tag, @':tag');
	if (mkcl_Null(info))
	  mkcl_FEprogram_error(env, "GO: Unknown tag ~S.", 1, tag);
	if (!mkcl_Null(args))
	  mkcl_FEprogram_error(env, "GO: Too many arguments.",0);
	asm_op2(env, OP_GO, mkcl_fixnum_to_word(MKCL_CAR(info)));
	asm_arg(env, mkcl_fixnum_to_word(MKCL_CDR(info)));
	return flags;
}


/*
	(if a b) -> (cond (a b))
	(if a b c) -> (cond (a b) (t c))
*/
static int
c_if(MKCL, mkcl_object form, int flags) {
  mkcl_object test = pop(env, &form);
  mkcl_object then = pop(env, &form);
	then = mk_cl_list(env, 2, test, then);
	if (mkcl_Null(form)) {
	  return c_cond(env, mkcl_list1(env, then), flags);
	} else {
	  return c_cond(env, mk_cl_list(env, 2, then, MKCL_CONS(env, mk_cl_Ct, form)), flags);
	}
}


static int
c_labels(MKCL, mkcl_object args, int flags) {
	return c_labels_flet(env, OP_LABELS, args, flags);
}


/*
	The OP_PUSHENV saves the current lexical environment to allow
	several bindings.
		OP_PUSHENV
		...		; binding forms
		...		; body
		OP_EXIT

	There are four forms which perform bindings
		OP_PBIND name	; Bind NAME in the lexical env. using
				; a value from the stack
		OP_PBINDS name	; Bind NAME as special variable using
				; a value from the stack
		OP_BIND name	; Bind NAME in the lexical env. using
				; VALUES(0)
		OP_BINDS name	; Bind NAME as special variable using
				; VALUES(0)

	After a variable has been bound, there are several ways to
	refer to it.

	1) Refer to the n-th variable in the lexical environment
		[SYMVAL + n]

	2) Refer to the value of a special variable or constant
		SYMVALS
		name

        3) Push the value of the n-th variable of the lexical environment
		[PUSHV + n]

	4) Push the value of a special variable or constant
		PUSHVS
		name
*/

static int
c_let_leta(MKCL, int op, mkcl_object args, int flags) {
	mkcl_object bindings, specials, body, l, vars;
	mkcl_object old_variables = env->c_env->variables;
	bool is_let = (op == OP_PBIND);

	bindings = mk_cl_car(env, args);
	body = c_process_declarations(env, MKCL_CDR(args));
	specials = MKCL_VALUES(3);

	/* Optimize some common cases */
	switch(mkcl_length(env, bindings)) {
	case 0:		return c_locally(env, MKCL_CDR(args), flags);
	case 1:		op = OP_BIND; break;
	}

	for (vars=mk_cl_Cnil, l=bindings; !mkcl_endp(env, l); ) {
	  mkcl_object aux = pop(env, &l);
		mkcl_object var, value;
		if (MKCL_ATOM(aux)) {
			var = aux;
			value = mk_cl_Cnil;
		} else {
		  var = pop(env, &aux);
		  value = pop_maybe_nil(env, &aux);
			if (!mkcl_Null(aux))
			  mkcl_FEprogram_error(env, "LET: Ill formed declaration.",0);
		}
		if (!MKCL_SYMBOLP(var))
		  mkcl_FEillegal_variable_name(env, var);
		if (mkcl_symbol_type(env, var) & mkcl_stp_constant) {
		  mkcl_FEprogram_error(env,
				       (is_let
					? "LET: Tried to bind a value to the constant ~S."
					: "LET*: Tried to bind a value to the constant ~S."),
				       1, var);
		}

		if (op == OP_PBIND) {
			compile_form(env, value, FLAG_PUSH);
			vars = MKCL_CONS(env, var, vars);
		} else {
			compile_form(env, value, FLAG_REG0);
			c_bind(env, var, specials);
		}
	}
	while (!mkcl_endp(env, vars))
	  c_pbind(env, pop(env, &vars), specials);

	/* We have to register all specials, because in the list
	 * there might be some variable that is not bound by this LET form
	 */
	c_declare_specials(env, specials);

	flags = compile_body(env, body, flags);

	c_undo_bindings(env, old_variables, 0);
	return flags;
}

static int
c_let(MKCL, mkcl_object args, int flags) {
	return c_let_leta(env, OP_PBIND, args, flags);
}

static int
c_leta(MKCL, mkcl_object args, int flags) {
	return c_let_leta(env, OP_BIND, args, flags);
}

static int
c_load_time_value(MKCL, mkcl_object args, int flags)
{
  if (mk_cl_rest(env, args) != mk_cl_Cnil)
    mkcl_FEprogram_error(env, "LOAD-TIME-VALUE: Too many arguments.", 0);
	return c_values(env, args, flags);
}

static int
c_locally(MKCL, mkcl_object args, int flags) {
	mkcl_object old_env = env->c_env->variables;

	/* First use declarations by declaring special variables... */
	args = c_process_declarations(env, args);
	c_declare_specials(env, MKCL_VALUES(3));

	/* ...and then process body */
	flags = compile_body(env, args, flags);

	c_undo_bindings(env, old_env, 0);

	return flags;
}

/*
	MACROLET

	The current lexical environment is saved. A new one is prepared with
	the definitions of these macros, and this environment is used to
	compile the body.
 */
static int
c_macrolet(MKCL, mkcl_object args, int flags)
{
        const mk_cl_compiler_ptr c_env = env->c_env;
	mkcl_object old_env = c_env->macros;
	mkcl_object m_env = mkcl_funcall2(env,
					  @+'si::cmp-env-register-macrolet',
					  pop(env, &args),
					  MKCL_CONS(env, c_env->variables, c_env->macros));
	c_env->macros = MKCL_CDR(m_env);
	flags = c_locally(env, args, flags);
	c_env->macros = old_env;
	return flags;
}

static void
c_vbind(MKCL, mkcl_object var, int n, mkcl_object specials)
{
  if (c_declared_special(env, var, specials)) {
                c_register_var(env, var, FLAG_PUSH, TRUE);
                if (n) {
                        asm_op2(env, OP_VBINDS, n);
                } else {
                        asm_op(env, OP_BINDS);
                }
        } else {
                c_register_var(env, var, FALSE, TRUE);
                if (n) {
                        asm_op2(env, OP_VBIND, n);
                } else {
                        asm_op(env, OP_BIND);
                }
        }
        asm_c(env, var);
}

static int
c_multiple_value_bind(MKCL, mkcl_object args, int flags)
{
	mkcl_object old_env = env->c_env->variables;
	mkcl_object vars, value, body, specials;
	mkcl_index n;

	vars = pop(env, &args);
	value = pop(env, &args);
	body = c_process_declarations(env, args);
	specials = MKCL_VALUES(3);

	compile_form(env, value, FLAG_VALUES);
	n = mkcl_length(env, vars);
	if (n == 0) {
		c_declare_specials(env, specials);
		flags = compile_body(env, body, flags);
		c_undo_bindings(env, old_env, 0);
	} else {
		mkcl_object old_variables = env->c_env->variables;
		for (vars=mk_cl_reverse(env, vars); n--; ) {
		  mkcl_object var = pop(env, &vars);
			if (!MKCL_SYMBOLP(var))
			  mkcl_FEillegal_variable_name(env, var);
                        c_vbind(env, var, n, specials);
		}
		c_declare_specials(env, specials);
		flags = compile_body(env, body, flags);
		c_undo_bindings(env, old_variables, 0);
	}
	return flags;
}


static int
c_multiple_value_call(MKCL, mkcl_object args, int flags) {
	mkcl_object name;
	int op;

	name = pop(env, &args);
	if (mkcl_endp(env, args)) {
		/* If no arguments, just use ordinary call */
	  return c_funcall(env, mk_cl_list(env, 1, name), flags);
	}
	compile_form(env, name, FLAG_PUSH);
	for (op = OP_PUSHVALUES; !mkcl_endp(env, args); op = OP_PUSHMOREVALUES) {
	  compile_form(env, pop(env, &args), FLAG_VALUES);
		asm_op(env, op);
	}
	asm_op(env, OP_MCALL);
	asm_op(env, OP_POP1);

	return FLAG_VALUES;
}


static int
c_multiple_value_prog1(MKCL, mkcl_object args, int flags) {
  compile_form(env, pop(env, &args), FLAG_VALUES);
  if (!mkcl_endp(env, args)) {
		asm_op(env, OP_PUSHVALUES);
		compile_body(env, args, FLAG_IGNORE);
		asm_op(env, OP_POPVALUES);
	}
	return FLAG_VALUES;
}


static int
c_multiple_value_setq(MKCL, mkcl_object orig_args, int flags) {
	mkcl_object args = orig_args;
	mkcl_object orig_vars;
	mkcl_object vars = mk_cl_Cnil, values;
	mkcl_object old_variables = env->c_env->variables;
	mkcl_index nvars = 0;

	/* Look for symbol macros, building the list of variables
	   and the list of late assignments. */
	for (orig_vars = pop(env, &args); !mkcl_endp(env, orig_vars); ) {
	  mkcl_object v = pop(env, &orig_vars);
		if (!MKCL_SYMBOLP(v))
		  mkcl_FEillegal_variable_name(env, v);
		v = c_macro_expand1(env, v);
		if (!MKCL_SYMBOLP(v)) {
			/* If any of the places to be set is not a variable,
			 * transform MULTIPLE-VALUE-SETQ into (SETF (VALUES ...))
			 */
			args = orig_args;
			return compile_form(env, mk_cl_listX(env, 3,
							     @'setf',
							     MKCL_CONS(env, @'values', MKCL_CAR(args)),
							     MKCL_CDR(args)),
					    flags);
		}
		vars = MKCL_CONS(env, v, vars);
		nvars++;
	}

	/* Compile values */
	values = pop(env, &args);
	if (args != mk_cl_Cnil)
	  mkcl_FEprogram_error(env, "MULTIPLE-VALUE-SETQ: Too many arguments.", 0);
	if (nvars == 0) {
		/* No variables */
	  return compile_form(env, mk_cl_list(env, 2, @'values', values), flags);
	}
	compile_form(env, values, FLAG_VALUES);

	/* Compile variables */
	for (nvars = 0, vars = mk_cl_nreverse(env, vars); vars != mk_cl_Cnil; nvars++, vars = MKCL_CONS_CDR(vars)) {
		if (nvars) {
			compile_setq(env, OP_VSETQ, MKCL_CONS_CAR(vars));
			asm_arg(env, nvars);
		} else {
			compile_setq(env, OP_SETQ, MKCL_CONS_CAR(vars));
		}
	}

	c_undo_bindings(env, old_variables, 0);

	return FLAG_REG0;
}

/*
	The OP_NOT operator reverses the boolean value of VALUES(0).
*/
static int
c_not(MKCL, mkcl_object args, int flags) {
	flags = maybe_reg0(flags);
	if (flags & FLAG_USEFUL) {
		/* The value is useful */
	  compile_form(env, pop(env, &args), FLAG_REG0);
		asm_op(env, OP_NOT);
	} else {
		/* The value may be ignored. */
	  flags = compile_form(env, pop(env, &args), flags);
	}
	if (!mkcl_Null(args))
	  mkcl_FEprogram_error(env, "NOT/NULL: Too many arguments.", 0);
	return flags;
}

/*
	The OP_NTHVAL operator moves a value from VALUES(ndx) to
	VALUES(0). The index NDX is taken from the stack.

		OP_NTHVAL
*/
static int
c_nth_value(MKCL, mkcl_object args, int flags) {
  compile_form(env, pop(env, &args), FLAG_PUSH);	/* INDEX */
  compile_form(env, pop(env, &args), FLAG_VALUES);	/* VALUES */
	if (args != mk_cl_Cnil)
	  mkcl_FEprogram_error(env, "NTH-VALUE: Too many arguments.",0);
	asm_op(env, OP_NTHVAL);
	return FLAG_REG0;
}


static int
c_prog1(MKCL, mkcl_object args, int flags) {
  mkcl_object form = pop(env, &args);
	if (!(flags & FLAG_USEFUL) || (flags & FLAG_PUSH)) {
		flags = compile_form(env, form, flags);
		compile_body(env, args, FLAG_IGNORE);
	} else {
		flags = FLAG_REG0;
		compile_form(env, form, FLAG_PUSH);
		compile_body(env, args, FLAG_IGNORE);
		asm_op(env, OP_POP);
	}
	return flags;
}


/*
	The OP_PROGV operator exectures a set of statements in a lexical
	environment that has been extended with special variables. The
	list of special variables is taken from the top of the stack,
	while the list of values is in VALUES(0).

		...		; list of variables
		OP_PUSH
		...		; list of values
		OP_PROGV
		...		; body of progv
		OP_EXIT
*/
static int
c_progv(MKCL, mkcl_object args, int flags) {
  mkcl_object vars = pop(env, &args);
  mkcl_object values = pop(env, &args);

	/* The list of variables is in the stack */
	compile_form(env, vars, FLAG_PUSH);

	/* The list of values is in reg0 */
	compile_form(env, values, FLAG_REG0);

	/* The body is interpreted within an extended lexical
	   environment. However, as all the new variables are
	   special, the compiler need not take care of them
	*/
	asm_op(env, OP_PROGV);
	flags = compile_body(env, args, FLAG_VALUES);
	asm_op(env, OP_EXIT_PROGV);

	return flags;
}


/*
	There are four assignment operators. They are

	1) Assign VALUES(0) to the lexical variable which occupies the
	   N-th position
		[OP_SETQ + n]

	2) Assign VALUES(0) to the special variable NAME
		[OP_SETQS + name]

	3) Pop a value from the stack and assign it to the lexical
	   variable in the N-th position.
		[OP_PSETQ + n]

	4) Pop a value from the stack and assign it to the special
	   variable denoted by NAME
		[OP_PSETQS + name]
*/
static int
c_psetq(MKCL, mkcl_object old_args, int flags) {
	mkcl_object args = mk_cl_Cnil, vars = mk_cl_Cnil;
	bool use_psetf = FALSE;
	mkcl_index nvars = 0;

	if (mkcl_endp(env, old_args))
		return compile_body(env, mk_cl_Cnil, flags);
	/* We have to make sure that non of the variables which
	   are to be assigned is actually a symbol macro. If that
	   is the case, we invoke (PSETF ...) to handle the
	   macro expansions.
	*/
	while (!mkcl_endp(env, old_args)) {
	  mkcl_object var = pop(env, &old_args);
	  mkcl_object value = pop(env, &old_args);
		if (!MKCL_SYMBOLP(var))
		  mkcl_FEillegal_variable_name(env, var);
		var = c_macro_expand1(env, var);
		if (!MKCL_SYMBOLP(var))
			use_psetf = TRUE;
		args = mkcl_nconc(env, args, mk_cl_list(env, 2, var, value));
		nvars++;
	}
	if (use_psetf) {
	  return compile_form(env, MKCL_CONS(env, @'psetf', args), flags);
	}
	while (!mkcl_endp(env, args)) {
	  mkcl_object var = pop(env, &args);
	  mkcl_object value = pop(env, &args);
	  vars = MKCL_CONS(env, var, vars);
		compile_form(env, value, FLAG_PUSH);
	}
	while (!mkcl_endp(env, vars))
	  compile_setq(env, OP_PSETQ, pop(env, &vars));
	return compile_form(env, mk_cl_Cnil, flags);
}


/*
	The OP_RETFROM operator returns from a block using the objects
	in VALUES() as output values.

		...		; output form
		OP_RETFROM
		tag		; object which names the block
*/
static int
c_return_aux(MKCL, mkcl_object name, mkcl_object stmt, int flags)
{
	mkcl_object ndx = c_tag_ref(env, name, @':block');
	mkcl_object output = pop_maybe_nil(env, &stmt);

	if (!MKCL_SYMBOLP(name) || mkcl_Null(ndx))
	  mkcl_FEprogram_error(env, "RETURN-FROM: Unknown block name ~S.", 1, name);
	if (stmt != mk_cl_Cnil)
	  mkcl_FEprogram_error(env, "RETURN-FROM: Too many arguments.", 0);
	compile_form(env, output, FLAG_VALUES);
	asm_op2(env, OP_RETURN, mkcl_fixnum_to_word(ndx));
	return FLAG_VALUES;
}

static int
c_return(MKCL, mkcl_object stmt, int flags) {
	return c_return_aux(env, mk_cl_Cnil, stmt, flags);
}


static int
c_return_from(MKCL, mkcl_object stmt, int flags) {
  mkcl_object name = pop(env, &stmt);
	return c_return_aux(env, name, stmt, flags);
}


static int
c_setq(MKCL, mkcl_object args, int flags) {
  if (mkcl_endp(env, args))
		return compile_form(env, mk_cl_Cnil, flags);
	do {
	  mkcl_object var = pop(env, &args);
	  mkcl_object value = pop(env, &args);
		if (!MKCL_SYMBOLP(var))
		  mkcl_FEillegal_variable_name(env, var);
		var = c_macro_expand1(env, var);
		if (MKCL_SYMBOLP(var)) {
			flags = FLAG_REG0;
			compile_form(env, value, FLAG_REG0);
			compile_setq(env, OP_SETQ, var);
		} else {
		  flags = mkcl_endp(env, args) ? FLAG_VALUES : FLAG_REG0;
		  compile_form(env, mk_cl_list(env, 3, @'setf', var, value), flags);
		}
	} while (!mkcl_endp(env, args));
	return flags;
}


static int
c_symbol_macrolet(MKCL, mkcl_object args, int flags)
{
	mkcl_object def_list, specials, body;
	mkcl_object old_variables = env->c_env->variables;

	def_list = pop(env, &args);
	body = c_process_declarations(env, args);
	specials = MKCL_VALUES(3);

	/* Scan the list of definitions */
	for (; !mkcl_endp(env, def_list); ) {
	  mkcl_object definition = pop(env, &def_list);
	  mkcl_object name = pop(env, &definition);
	  mkcl_object expansion = pop(env, &definition);
	  mkcl_object arglist = mk_cl_list(env, 2, @gensym(env, 0), @gensym(env, 0));
		mkcl_object function;
		if ((mkcl_symbol_type(env, name) & (mkcl_stp_special | mkcl_stp_constant)) ||
		    c_var_ref(env, name,1,FALSE) == -2)
		{
		  mkcl_FEprogram_error(env, "SYMBOL-MACROLET: Symbol ~A cannot be \
declared special and appear in a symbol-macrolet.", 1, name);
		}
		definition = mk_cl_list(env, 2, arglist, mk_cl_list(env, 2, @'quote', expansion));
		function = mkcl_make_lambda(env, name, definition);
		c_register_symbol_macro(env, name, function);
	}
	c_declare_specials(env, specials);
	flags = compile_body(env, body, flags);
	c_undo_bindings(env, old_variables, 0);
	return flags;
}

static int
c_tagbody(MKCL, mkcl_object args, int flags)
{
	mkcl_object old_env = env->c_env->variables;
	mkcl_index tag_base;
	mkcl_object labels = mk_cl_Cnil, label, body;
	mkcl_type item_type;
	int nt, i;

	/* count the tags */
	for (nt = 0, body = args; !mkcl_endp(env, body); body = MKCL_CONS_CDR(body)) {
		label = MKCL_CONS_CAR(body);
		item_type = mkcl_type_of(label);
		if (item_type == mkcl_t_symbol || item_type == mkcl_t_fixnum ||
	            item_type == mkcl_t_bignum) {
		  labels = MKCL_CONS(env, MKCL_CONS(env, label,MKCL_MAKE_FIXNUM(nt)), labels);
			nt += 1;
		}
	}
	if (nt == 0) {
		compile_body(env, args, 0);
		return compile_form(env, mk_cl_Cnil, flags);
	}
	asm_op2c(env, OP_BLOCK, MKCL_MAKE_FIXNUM(0));
	c_register_tags(env, labels);
	asm_op2(env, OP_TAGBODY, nt);
	tag_base = current_pc(env);
	for (i = nt; i; i--)
		asm_arg(env, 0);

	for (body = args; !mkcl_endp(env, body); body = MKCL_CONS_CDR(body)) {
		label = MKCL_CONS_CAR(body);
		item_type = mkcl_type_of(label);
		if (item_type == mkcl_t_symbol || item_type == mkcl_t_fixnum ||
	            item_type == mkcl_t_bignum) {
			asm_complete(env, 0, tag_base);
			tag_base += OPARG_SIZE;
		} else {
			compile_form(env, label, FLAG_IGNORE);
		}
	}
	asm_op(env, OP_EXIT_TAGBODY);
	c_undo_bindings(env, old_env, 0);
	return FLAG_REG0;
}


/*
	The OP_THROW jumps to an enclosing OP_CATCH whose tag
	matches the one of the throw. The tag is taken from the
	stack, while the output values are left in VALUES().
*/
static int
c_throw(MKCL, mkcl_object stmt, int flags) {
  mkcl_object tag = pop(env, &stmt);
  mkcl_object form = pop(env, &stmt);
	if (stmt != mk_cl_Cnil)
	  mkcl_FEprogram_error(env, "THROW: Too many arguments.",0);
	compile_form(env, tag, FLAG_PUSH);
	compile_form(env, form, FLAG_VALUES);
	asm_op(env, OP_THROW);
	return flags;
}


static int
c_unwind_protect(MKCL, mkcl_object args, int flags) {
	mkcl_index label = asm_jmp(env, OP_PROTECT);

	flags = maybe_values(flags);

	/* Compile form to be protected */
	flags = compile_form(env, pop(env, &args), flags);
	asm_op(env, OP_PROTECT_NORMAL);

	/* Compile exit clause */
	asm_complete(env, OP_PROTECT, label);
	compile_body(env, args, FLAG_IGNORE);
	asm_op(env, OP_PROTECT_EXIT);

	return flags;
}


/*
	The OP_VALUES moves N values from the stack to VALUES().

		[OP_VALUES + n]
*/
static int
c_values(MKCL, mkcl_object args, int flags) {
	if (!(flags & FLAG_USEFUL)) {
		/* This value will be discarded. We do not care to
		   push it or to save it in VALUES */
	  if (mkcl_endp(env, args))
			return flags;
		return compile_body(env, args, flags);
	} else if (flags & FLAG_PUSH) {
		/* We only need the first value. However, the rest
		   of arguments HAVE to be be evaluated */
	  if (mkcl_endp(env, args))
			return compile_form(env, mk_cl_Cnil, flags);
	  flags = compile_form(env, pop(env, &args), FLAG_PUSH);
		compile_body(env, args, FLAG_IGNORE);
		return flags;
	} else if (mkcl_endp(env, args)) {
		asm_op(env, OP_NOP);
	} else {
		int n = 0;
		while (!mkcl_endp(env, args)) {
		  compile_form(env, pop_maybe_nil(env, &args), FLAG_PUSH);
			n++;
		}
		asm_op2(env, OP_VALUES, n);
	}
	return FLAG_VALUES;
}


static int
compile_form(MKCL, mkcl_object stmt, int flags)
{
  const mk_cl_compiler_ptr c_env = env->c_env;
  mkcl_object code_walker = MKCL_SYM_VAL(env, @'si::*code-walker*');
  mkcl_object function;
  bool push = flags & FLAG_PUSH;
  int new_flags;

  mkcl_bds_bind(env, @'si::*current-form*', stmt);
 BEGIN:
  /* This hook is used by CLOS during processing of defmethod to decide on some optimizations. JCB */
  if (code_walker != MKCL_OBJNULL && !mkcl_Null(code_walker)) {
    stmt = mkcl_funcall2(env, MKCL_SYM_VAL(env,@'si::*code-walker*'), stmt,
			 MKCL_CONS(env, c_env->variables, c_env->macros));
  }
  /*
   * First try with variable references and quoted constants
   */
  if (MKCL_ATOM(stmt)) {
    mkcl_word index;
    if (MKCL_SYMBOLP(stmt) && stmt != mk_cl_Cnil) {
      mkcl_object stmt1 = c_macro_expand1(env, stmt);
      if (stmt1 != stmt) {
	stmt = stmt1;
	goto BEGIN;
      }
      index = c_var_ref(env, stmt,0,FALSE);
      if (index >= 0) {
	asm_op2(env, push? OP_PUSHV : OP_VAR, index);
      } else {
	asm_op2c(env, push? OP_PUSHVS : OP_VARS, stmt);
      }
    } else
    QUOTED:
      if ((flags & FLAG_USEFUL)) {
	mkcl_word n;
	if (stmt == mk_cl_Cnil) {
	  asm_op(env, push? OP_PUSHNIL : OP_NIL);
	} else if (MKCL_FIXNUMP(stmt) && (n = mkcl_fixnum_to_word(stmt)) <= MAX_OPARG
		   && n >= -MAX_OPARG) {
	  asm_op2(env, push? OP_PINT : OP_INT, n);
	} else {
	  asm_op2c(env, push? OP_PUSHQ : OP_QUOTE, stmt);
	}
      }

    if (flags & FLAG_VALUES)
      new_flags = (flags & ~FLAG_VALUES) | FLAG_REG0;
    else
      new_flags = flags;
    goto OUTPUT;
  }
  /*
   * Next try with special forms.
   */
  function = MKCL_CONS_CAR(stmt);
  if (!MKCL_SYMBOLP(function))
    goto ORDINARY_CALL;
  if (function == @'quote') {
    stmt = MKCL_CONS_CDR(stmt);
    if (MKCL_ATOM(stmt) || MKCL_CONS_CDR(stmt) != mk_cl_Cnil)
      mkcl_FEprogram_error(env, "QUOTE: Ill formed.",0);
    stmt = MKCL_CONS_CAR(stmt);
    goto QUOTED; /* This back flip is jumping into the middle of a nested block above. JCB */
  }
  {
    int i;
    for (i = 0; i < DATABASE_SIZE; i++) {
      const compiler_record * l = &(database[i]);
      /*cl_print(1, l->symbol);*/
      if (l->symbol == function) {
	c_env->lexical_level += l->lexical_increment;
	if (c_env->stepping && function != @'function' &&
	    c_env->lexical_level)
	  asm_op2c(env, OP_STEPIN, stmt);
	new_flags = (*(l->compiler))(env, MKCL_CONS_CDR(stmt), flags);
	if (c_env->stepping && function != @'function' &&
	    c_env->lexical_level)
	  asm_op(env, OP_STEPOUT);
	goto OUTPUT;
      }
    }
  }
  /*
   * Next try to macroexpand
   */
  {
    mkcl_object new_stmt = c_macro_expand1(env, stmt);
    if (new_stmt != stmt){
      stmt = new_stmt;
      goto BEGIN;
    }
  }
  if (mkcl_symbol_type(env, function) & mkcl_stp_special_form)
    mkcl_FEprogram_error(env, "BYTECOMPILE-FORM: Found no macroexpander for special form ~S.", 1, function);
 ORDINARY_CALL:
  /*
   * Finally resort to ordinary function calls.
   */
  if (c_env->stepping)
    asm_op2c(env, OP_STEPIN, stmt);
  if (function >= (mkcl_object)mkcl_root_symbols
      && function < (mkcl_object)(mkcl_root_symbols + mkcl_root_symbols_count))
    {
      mkcl_object f = MKCL_SYM_FUN(function);
      mkcl_type t = (f == MKCL_OBJNULL) ? mkcl_t_end : mkcl_type_of(f);
      if (t == mkcl_t_cfun)
	{
	  mkcl_object args = MKCL_CONS_CDR(stmt);
	  mkcl_index n = mkcl_length(env, args);
	  if (f->cfun.narg == 1 && n == 1) {
	    compile_form(env, MKCL_CONS_CAR(args), FLAG_REG0);
	    asm_op2c(env, OP_CALLG1, function);
	    new_flags = FLAG_VALUES;
	    goto OUTPUT;
	  } else if (f->cfun.narg == 2 && n == 2) {
	    compile_form(env, MKCL_CONS_CAR(args), FLAG_PUSH);
	    args = MKCL_CONS_CDR(args);
	    compile_form(env, MKCL_CONS_CAR(args), FLAG_REG0);
	    asm_op2c(env, OP_CALLG2, function);
	    new_flags = FLAG_VALUES;
	    goto OUTPUT;
	  }
	}
    }
  new_flags = c_call(env, stmt, flags);
 OUTPUT:
  /*
    flags		new_flags		action
    PUSH		PUSH			---
    PUSH		VALUES			OP_PUSH
    PUSH		REG0			OP_PUSH
    VALUES		PUSH			Impossible
    VALUES		VALUES			---
    VALUES		REG0			OP_VALUEREG0
    REG0		PUSH			Impossible
    REG0		VALUES			---
    REG0		REG0			---
  */
  if (push) {
    if (new_flags & (FLAG_REG0 | FLAG_VALUES))
      asm_op(env, OP_PUSH);
  } else if (flags & FLAG_VALUES) {
    if (new_flags & FLAG_REG0) {
      asm_op(env, OP_VALUEREG0);
    } else if (new_flags & FLAG_PUSH) {
      mkcl_FEerror(env, "Internal error in bytecode compiler", 0);
    }
  } else if (new_flags & FLAG_PUSH) {
    mkcl_FEerror(env, "Internal error in bytecode compiler", 0);
  }
  mkcl_bds_unwind1(env);
  return flags;
}

static void
eval_form(MKCL, mkcl_object form) {
        const mk_cl_compiler_ptr old_c_env = env->c_env;
        struct mkcl_compiler_env new_c_env = *old_c_env;
        mkcl_index handle;
        mkcl_object bytecode;
        struct mkcl_temp_stack_frame frame;
        frame.t = mkcl_t_temp_stack_frame;
        frame.stack = frame.base = 0;
        frame.size = 0;
        frame.env = env;
        env->c_env = &new_c_env;
        handle = asm_begin(env);
        compile_form(env, form, FLAG_VALUES);
        asm_op(env, OP_EXIT);
        MKCL_VALUES(0) = mk_cl_Cnil;
        MKCL_NVALUES = 0;
        bytecode = asm_end(env, handle, form);
	env->function = mk_cl_Cnil;
        mkcl_interpret(env, (mkcl_object)&frame, new_c_env.lex_env, bytecode);
        asm_clear(env, handle);
        env->c_env = old_c_env;
        mkcl_dealloc(env, bytecode->bytecode.code);
        mkcl_dealloc(env, bytecode->bytecode.data);
        mkcl_dealloc(env, bytecode);
}

static int
compile_body(MKCL, mkcl_object body, int flags) {
        const mk_cl_compiler_ptr old_c_env = env->c_env;
	if (mkcl_endp(env, body)) {
		return compile_form(env, mk_cl_Cnil, flags);
	}
        if ((old_c_env->lexical_level == 0) && (old_c_env->mode == MODE_EXECUTE)) {
                do {
                        mkcl_object form = MKCL_CONS_CAR(body);
                        body = MKCL_CONS_CDR(body);
                        if (mkcl_endp(env, body)) {
                                return compile_form(env, form, flags);
                        }
                        eval_form(env, form);
                } while (1);
        } else {
                do {
                        mkcl_object form = MKCL_CONS_CAR(body);
                        body = MKCL_CONS_CDR(body);
                        if (mkcl_endp(env, body)) {
                                return compile_form(env, form, flags);
                        }
                        compile_form(env, form, FLAG_IGNORE);
                } while (1);
        }
}

/* ------------------------ INLINED FUNCTIONS -------------------------------- */

static int
c_cons(MKCL, mkcl_object args, int flags)
{
  if (mkcl_length(env, args) != 2) {
    mkcl_FEprogram_error(env, "CONS: Wrong number of arguments", 0);
	}
  compile_form(env, mk_cl_first(env, args), FLAG_PUSH);
  compile_form(env, mk_cl_second(env, args), FLAG_REG0);
	asm_op(env, OP_CONS);
	return FLAG_REG0;
}

static int
c_endp(MKCL, mkcl_object args, int flags)
{
  mkcl_object list = pop(env, &args);
	if (args != mk_cl_Cnil) {
	  mkcl_FEprogram_error(env, "ENDP: Too many arguments", 0);
	}
	compile_form(env, list, FLAG_REG0);
	asm_op(env, OP_ENDP);
	return FLAG_REG0;
}

static int
c_car(MKCL, mkcl_object args, int flags)
{
  mkcl_object list = pop(env, &args);
	if (args != mk_cl_Cnil) {
	  mkcl_FEprogram_error(env, "CAR: Too many arguments", 0);
	}
	compile_form(env, list, FLAG_REG0);
	asm_op(env, OP_CAR);
	return FLAG_REG0;
}

static int
c_cdr(MKCL, mkcl_object args, int flags)
{
  mkcl_object list = pop(env, &args);
	if (args != mk_cl_Cnil) {
	  mkcl_FEprogram_error(env, "CDR: Too many arguments", 0);
	}
	compile_form(env, list, FLAG_REG0);
	asm_op(env, OP_CDR);
	return FLAG_REG0;
}

static int
c_list_listA(MKCL, mkcl_object args, int flags, int op)
{
  mkcl_index n = mkcl_length(env, args);
	if (n == 0) {
		return compile_form(env, mk_cl_Cnil, flags);
	} else {
		while (MKCL_CONS_CDR(args) != mk_cl_Cnil) {
			compile_form(env, MKCL_CONS_CAR(args), FLAG_PUSH);
			args = MKCL_CONS_CDR(args);
		}
		compile_form(env, MKCL_CONS_CAR(args), FLAG_REG0);
		asm_op2(env, op, n);
		return FLAG_REG0;
	}
}

static int
c_list(MKCL, mkcl_object args, int flags)
{
	return c_list_listA(env, args, flags, OP_LIST);
}

static int
c_listA(MKCL, mkcl_object args, int flags)
{
	return c_list_listA(env, args, flags, OP_LISTA);
}


/* ----------------------------- PUBLIC INTERFACE ---------------------------- */

/* ------------------------------------------------------------
   LAMBDA OBJECTS: An interpreted function is a vector made of
	the following components

      #(LAMBDA
	{block-name | NIL}
	{variable-env | NIL}
	{function-env | NIL}
	{block-env | NIL}
	(list of variables declared special)
	Nreq {var}*			; required arguments
	Nopt {var value flag}*		; optional arguments
	{rest-var NIL}			; rest variable
	{T | NIL}			; allow other keys?
	Nkey {key var value flag}*	; keyword arguments
	Naux {var init}			; auxiliary variables
	documentation-string
	list-of-declarations
	{form}*				; body)

   ------------------------------------------------------------ */

#define push(e,v,l) l = MKCL_CONS(e, v, l)
#define push_var(e, v, list)					\
  if (context == @'function') {					\
    if (mkcl_symbol_type(e, v) & mkcl_stp_constant)		\
      mkcl_FEillegal_variable_name(e, v); }			\
  push(e, v, list)

/*
  Handles special declarations, removes declarations from body
 */
@(defun si::process_declarations (body &optional doc)
	mkcl_object documentation = mk_cl_Cnil, declarations = mk_cl_Cnil, specials = mk_cl_Cnil;
	mkcl_object decls, vars, v;
@
	/* BEGIN: SEARCH DECLARE */
  for (; !mkcl_endp(env, body); body = MKCL_CONS_CDR(body)) {
	  mkcl_object form = MKCL_CONS_CAR(body);

	  if (!mkcl_Null(doc) && mkcl_type_of(form) == mkcl_t_base_string && !mkcl_endp(env, MKCL_CDR(body))) {
	    if (documentation == mk_cl_Cnil)
	      documentation = form;
	    else
	      break;
	    continue;
	  }

	  if (MKCL_ATOM(form) || (MKCL_CONS_CAR(form) != @'declare'))
	    break;

	  for (decls = MKCL_CONS_CDR(form); !mkcl_endp(env, decls); decls = MKCL_CONS_CDR(decls)) {
	    mkcl_object sentence = MKCL_CONS_CAR(decls);
	    if (MKCL_ATOM(sentence))
	      mkcl_FEill_formed_input(env);
	    push(env, sentence, declarations);
	    if (MKCL_CONS_CAR(sentence) == @'special')
	      for (vars = MKCL_CONS_CDR(sentence); !mkcl_endp(env, vars); vars = MKCL_CONS_CDR(vars)) {
		v = MKCL_CONS_CAR(vars);
		mkcl_assert_type_symbol(env, v);
		push(env, v,specials);
	      }
	  }
	}
	/* END: SEARCH DECLARE */

	@(return declarations body documentation specials)
@)

#if 0
static size_t mk_si_process_lambda_ctr = 0;
#endif

mkcl_object
mk_si_process_lambda(MKCL, mkcl_object lambda)
{
	mkcl_object documentation, declarations, specials;
	mkcl_object lambda_list, body;

	mkcl_call_stack_check(env); /* JCB */
	if (MKCL_ATOM(lambda))
	  mkcl_FEprogram_error(env, "LAMBDA: No lambda list.", 0);
	lambda_list = MKCL_CONS_CAR(lambda);

	declarations = @si::process-declarations(env, 2, MKCL_CDR(lambda), mk_cl_Ct);
	body = MKCL_VALUES(1);
	documentation = MKCL_VALUES(2);
	specials = MKCL_VALUES(3);

	/* mk_si_process_lambda_ctr++; */

	MKCL_VALUES(0) = mk_si_process_lambda_list(env, lambda_list, @'function');
	MKCL_VALUES(MKCL_NVALUES++) = documentation;
	MKCL_VALUES(MKCL_NVALUES++) = specials;
	MKCL_VALUES(MKCL_NVALUES++) = declarations;
	MKCL_VALUES(MKCL_NVALUES++) = body;
	return MKCL_VALUES(0);
}

/*
 * (si::process-lambda-list lambda-list context)
 *
 * Parses different types of lambda lists. CONTEXT may be MACRO,
 * FTYPE, FUNCTION, METHOD or DESTRUCTURING-BIND, and determines the
 * valid sytax. The output is made of several values:
 *
 * MKCL_VALUES(0) = (N req1 ... )			; required values
 * MKCL_VALUES(1) = (N opt1 init1 flag1 ... )	; optional values
 * MKCL_VALUES(2) = rest-var				; rest-variable, if any
 * MKCL_VALUES(3) = key-flag				; T if &key was supplied
 * MKCL_VALUES(4) = (N key1 var1 init1 flag1 ... )	; keyword arguments
 * MKCL_VALUES(5) = allow-other-keys			; flag &allow-other-keys
 * MKCL_VALUES(6) = (N aux1 init1 ... )		; auxiliary variables
 *
 * 1) The prefix "N" is an integer value denoting the number of
 * variables which are declared within this section of the lambda
 * list.
 *
 * 2) The INIT* arguments are lisp forms which are evaluated when
 * no value is provided.
 *
 * 3) The FLAG* arguments is the name of a variable which holds a
 * boolean value in case an optional or keyword argument was
 * provided. If it is NIL, no such variable exists.
 */

mkcl_object
mk_si_process_lambda_list(MKCL, mkcl_object org_lambda_list, mkcl_object context)
{
#define AT_REQUIREDS	0
#define AT_OPTIONALS	1
#define AT_REST		2
#define AT_KEYS		3
#define AT_OTHER_KEYS	4
#define AT_AUXS		5

  mkcl_object v, key, init, spp, lambda_list = org_lambda_list;
  mkcl_object reqs = mk_cl_Cnil, opts = mk_cl_Cnil, keys = mk_cl_Cnil, rest = mk_cl_Cnil, auxs = mk_cl_Cnil;
  int nreq = 0, nopt = 0, nkey = 0, naux = 0, stage = 0;
  mkcl_object allow_other_keys = mk_cl_Cnil;
  mkcl_object key_flag = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  if (!MKCL_CONSP(lambda_list) && lambda_list != mk_cl_Cnil)
    goto ILLEGAL_LAMBDA;
 LOOP:
  if (MKCL_ATOM(lambda_list)) {
    if (lambda_list == mk_cl_Cnil)
      goto OUTPUT;
    else if (context == @'function')
      goto ILLEGAL_LAMBDA;
    else {
      v = lambda_list;
      lambda_list = mk_cl_Cnil;
      goto REST; /* an other jump into the middle of a neighboring block! JCB */
    }
  }
  v = MKCL_CONS_CAR(lambda_list);
  lambda_list = MKCL_CONS_CDR(lambda_list);
  if (v == @'&optional') {
    if (stage >= AT_OPTIONALS)
      goto ILLEGAL_LAMBDA;
    stage = AT_OPTIONALS;
    goto LOOP;
  }
  if (v == @'&rest' || (v == @'&body' && (context == @'si::macro' || context == @'destructuring-bind'))) {
    if (MKCL_ATOM(lambda_list))
      goto ILLEGAL_LAMBDA;
    v = MKCL_CONS_CAR(lambda_list);
    lambda_list = MKCL_CONS_CDR(lambda_list);
  REST:	   /* a jump target in the middle of a block targetted from outside of its block! JCB */
    if (stage >= AT_REST)
      goto ILLEGAL_LAMBDA;
    stage = AT_REST;
    rest = v;
    goto LOOP;
  }
  if (v == @'&key') {
    if (stage >= AT_KEYS)
      goto ILLEGAL_LAMBDA;
    key_flag = mk_cl_Ct;
    stage = AT_KEYS;
    goto LOOP;
  }
  if (v == @'&aux') {
    if (stage >= AT_AUXS)
      goto ILLEGAL_LAMBDA;
    stage = AT_AUXS;
    goto LOOP;
  }
  if (v == @'&allow-other-keys') {
    allow_other_keys = mk_cl_Ct;
    if (stage != AT_KEYS)
      goto ILLEGAL_LAMBDA;
    stage = AT_OTHER_KEYS;
    goto LOOP;
  }
  switch (stage) {
  case AT_REQUIREDS:
    nreq++;
    push_var(env, v, reqs);
    break;
  case AT_OPTIONALS:
    spp = mk_cl_Cnil;
    init = mk_cl_Cnil;
    if (!MKCL_ATOM(v)) {
      mkcl_object x = v;
      v = MKCL_CONS_CAR(x);
      x = MKCL_CONS_CDR(x);
      if (!mkcl_endp(env, x)) {
	init = MKCL_CONS_CAR(x);
	x = MKCL_CONS_CDR(x);
	if (!mkcl_endp(env, x)) {
	  spp = MKCL_CONS_CAR(x);
	  if (!mkcl_endp(env, MKCL_CONS_CDR(x)))
	    goto ILLEGAL_LAMBDA;
	}
      }
    }
    nopt++;
    push_var(env, v, opts);
    push(env, init, opts);
    if (spp != mk_cl_Cnil) {
      push_var(env, spp, opts);
    } else {
      push(env, mk_cl_Cnil, opts);
    }
    break;
  case AT_REST:
    /* If we get here, the user has declared more than one
     * &rest variable, as in (lambda (&rest x y) ...) */
    goto ILLEGAL_LAMBDA;
  case AT_KEYS:
    init = mk_cl_Cnil;
    spp = mk_cl_Cnil;
    if (!MKCL_ATOM(v)) {
      mkcl_object x = v;
      v = MKCL_CONS_CAR(x);
      x = MKCL_CONS_CDR(x);
      if (!mkcl_endp(env, x)) {
	init = MKCL_CONS_CAR(x);
	x = MKCL_CONS_CDR(x);
	if (!mkcl_endp(env, x)) {
	  spp = MKCL_CONS_CAR(x);
	  if (!mkcl_endp(env, MKCL_CONS_CDR(x)))
	    goto ILLEGAL_LAMBDA;
	}
      }
    }
    if (MKCL_CONSP(v)) {
      key = MKCL_CONS_CAR(v);
      if (mkcl_endp(env, MKCL_CONS_CDR(v)) || !mkcl_endp(env, MKCL_CDDR(v)))
	goto ILLEGAL_LAMBDA;
      v = MKCL_CADR(v);
      if (context == @'function')
	mkcl_assert_type_symbol(env, v);
      mkcl_assert_type_symbol(env, key);
    } else {
      int intern_flag;
      key = mkcl_intern(env, mkcl_symbol_name(env, v),
			mkcl_core.keyword_package,
			&intern_flag);
    }
    nkey++;
    push(env, key, keys);
    push_var(env, v, keys);
    push(env, init, keys);
    if (mkcl_Null(spp)) {
      push(env, mk_cl_Cnil, keys);
    } else {
      push_var(env, spp, keys);
    }
    break;
  default:
    if (MKCL_ATOM(v)) {
      init = mk_cl_Cnil;
    } else if (mkcl_endp(env, MKCL_CDDR(v))) {
      mkcl_object x = v;
      v = MKCL_CONS_CAR(x);
      init = MKCL_CADR(x);
    } else
      goto ILLEGAL_LAMBDA;
    naux++;
    push_var(env, v, auxs);
    push(env, init, auxs);
  }
  goto LOOP;

 OUTPUT:
  if ((nreq+nopt+(!mkcl_Null(rest))+nkey) >= MKCL_CALL_ARGUMENTS_LIMIT)
    mkcl_FEprogram_error(env, "LAMBDA: Argument list ist too long, ~S.", 1, org_lambda_list);

  @(return
    mk_cl_nreverse(env, reqs)
    MKCL_MAKE_FIXNUM(nreq)
    mk_cl_nreverse(env, opts)
    MKCL_MAKE_FIXNUM(nopt)
    rest
    key_flag
    mk_cl_nreverse(env, keys)
    MKCL_MAKE_FIXNUM(nkey)
    allow_other_keys
    mk_cl_nreverse(env, auxs)
    MKCL_MAKE_FIXNUM(naux));

 ILLEGAL_LAMBDA:
  mkcl_FEprogram_error(env, "LAMBDA: Illegal lambda list ~S.", 1, org_lambda_list);
}

static void
c_default(MKCL, mkcl_object var, mkcl_object stmt, mkcl_object flag, mkcl_object specials)
{
        /* Flag is in REG0, value, if it exists, in stack */
        mkcl_index label;
        label = asm_jmp(env, OP_JT);
        compile_form(env, stmt, FLAG_PUSH);
        if (mkcl_Null(flag)) {
                asm_complete(env, OP_JT, label);
        } else {
                compile_form(env, mk_cl_Cnil, FLAG_REG0);
                asm_complete(env, OP_JT, label);
                c_bind(env, flag, specials);
        }
        c_pbind(env, var, specials);
}

static mkcl_object
mkcl_make_lambda(MKCL, mkcl_object name, mkcl_object lambda)
{
  mkcl_object reqs, opts, rest, key, keys, auxs, allow_other_keys;
  mkcl_object nreq, nopt, nkey;
  mkcl_object specials, doc, decl, body, output;
  mkcl_index handle;
  struct mkcl_compiler_env *old_c_env, new_c_env;

  mkcl_bds_bind(env, @'si::*current-form*',
		@list*(env, 3, @'si::lambda-block', name, lambda));

  old_c_env = env->c_env;
  c_new_env(env, &new_c_env, mk_cl_Cnil, old_c_env);

  new_c_env.lexical_level++;
  new_c_env.coalesce = 0;

  reqs = mk_si_process_lambda(env, lambda);
  nreq = MKCL_VALUES(1);
  opts = MKCL_VALUES(2);
  nopt = MKCL_VALUES(3);
  rest = MKCL_VALUES(4);
  key  = MKCL_VALUES(5);
  keys = MKCL_VALUES(6);
  nkey = MKCL_VALUES(7);
  allow_other_keys = MKCL_VALUES(8);
  auxs = MKCL_VALUES(9);
  /* MKCL_VALUES(10) is a dummy marker. JCB */
  doc  = MKCL_VALUES(11);
  specials = MKCL_VALUES(12);
  decl = MKCL_VALUES(13);
  body = MKCL_VALUES(14);

  handle = asm_begin(env);

  /* Transform (SETF fname) => fname */
  if (!mkcl_Null(name) && mkcl_Null(mk_si_valid_function_name_p(env, name)))
    mkcl_FEprogram_error(env, "LAMBDA: Not a valid function name ~S",1,name);

  /* We register as special variable a symbol which is not
   * to be used. We use this to mark the boundary of a function
   * environment and when code-walking */
  c_register_var(env,
		 mk_cl_make_symbol(env, mkcl_make_simple_base_string(env, "FUNCTION")),
		 TRUE, FALSE);

  new_c_env.constants = mk_cl_Cnil;
  new_c_env.coalesce = TRUE;
  asm_constant(env, doc);
  asm_constant(env, decl);

  while (!mkcl_endp(env, reqs)) {
    mkcl_object var = pop(env, &reqs);
    asm_op(env, OP_POPREQ);
    c_bind(env, var, specials);
  }
  while (!mkcl_endp(env, opts)) {		/* Optional arguments */
    mkcl_object var = pop(env, &opts);
    mkcl_object stmt = pop(env, &opts);
    mkcl_object flag = pop(env, &opts);
    asm_op(env, OP_POPOPT);
    c_default(env, var, stmt, flag, specials);
  }
  if (mkcl_Null(rest) && mkcl_Null(key)) {		/* Check no excess arguments */
    asm_op(env, OP_NOMORE);
  }
  if (!mkcl_Null(rest)) {			/* &rest argument */
    asm_op(env, OP_POPREST);
    c_bind(env, rest, specials);
  }
  if (!mkcl_Null(key)) {
    mkcl_object aux = MKCL_CONS(env, allow_other_keys,mk_cl_Cnil);
    mkcl_object names = mk_cl_Cnil;
    asm_op2c(env, OP_PUSHKEYS, aux);
    while (!mkcl_endp(env, keys)) {
      mkcl_object name = pop(env, &keys);
      mkcl_object var = pop(env, &keys);
      mkcl_object stmt = pop(env, &keys);
      mkcl_object flag = pop(env, &keys);
      names = MKCL_CONS(env, name, names);
      asm_op(env, OP_POP);
      c_default(env, var, stmt, flag, specials);
    }
    MKCL_RPLACD(aux, names);
  }

  while (!mkcl_endp(env, auxs)) {		/* Local bindings */
    mkcl_object var = pop(env, &auxs);
    mkcl_object value = pop(env, &auxs);
    compile_form(env, value, FLAG_REG0);
    c_bind(env, var, specials);
  }
  c_declare_specials(env, specials);

  if (!mkcl_Null(name)) {
    compile_form(env, @list*(env, 3, @'block', mk_si_function_block_name(env, name),
			     body), FLAG_VALUES);
  } else {
    compile_body(env, body, FLAG_VALUES);
  }

  /* Only undo special bindings */
  c_undo_bindings(env, old_c_env->variables, 1);
  asm_op(env, OP_EXIT);

  if ( mkcl_Null(mkcl_symbol_value(env, @'si::*keep-definitions*')) )
    lambda = mk_cl_Cnil;
  output = asm_end(env, handle, lambda);
  output->bytecode.name = name;
  output->bytecode.definition 
    = mkcl_Null(mkcl_symbol_value(env, @'si::*keep-definitions*'))
    ? mk_cl_Cnil : lambda;

  env->c_env = old_c_env;

  mkcl_bds_unwind1(env);
  
  return output;
}

static mkcl_object
mkcl_function_block_name(mkcl_object name)
{
  if (MKCL_SYMBOLP(name)) {
    return name;
  } else if (MKCL_CONSP(name) && MKCL_CONS_CAR(name) == @'setf') {
    name = MKCL_CONS_CDR(name);
    if (MKCL_CONSP(name)) {
      mkcl_object output = MKCL_CONS_CAR(name);
      if (MKCL_SYMBOLP(output) && mkcl_Null(MKCL_CONS_CDR(name)))
	return output;
    }
  }
  return MKCL_OBJNULL;
}

mkcl_object
mk_si_function_block_name(MKCL, mkcl_object name)
{
  mkcl_call_stack_check(env);
  mkcl_object output = mkcl_function_block_name(name);
  if (output == MKCL_OBJNULL)
    mkcl_FEinvalid_function_name(env, name);
  @(return output);
}

mkcl_object
mk_si_valid_function_name_p(MKCL, mkcl_object name)
{
  mkcl_call_stack_check(env);
  @(return ((MKCL_OBJNULL != mkcl_function_block_name(name)) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_si_make_lambda(MKCL, mkcl_object name, mkcl_object rest)
{
  mkcl_object lambda = mk_cl_Cnil;
  const mkcl_env the_env = env;
  volatile mkcl_compiler_env_ptr old_c_env = the_env->c_env;
  struct mkcl_compiler_env new_c_env;

  mkcl_call_stack_check(env);
  c_new_env(the_env, &new_c_env, mk_cl_Cnil, 0);
  MKCL_UNWIND_PROTECT_BEGIN(the_env) {
    lambda = mkcl_make_lambda(the_env, name, rest);
  } MKCL_UNWIND_PROTECT_EXIT {
    the_env->c_env = old_c_env;
  } MKCL_UNWIND_PROTECT_END;
  @(return lambda);
}

@(defun si::eval-in-env (form 
			   &optional
			   (lex_env mk_cl_Cnil)
			   (stepping mk_cl_Cnil)
                           (compiler_env_p mk_cl_Cnil)
			   (execute mk_cl_Ct))
  volatile mkcl_compiler_env_ptr old_c_env;
  struct mkcl_compiler_env new_c_env;
  volatile mkcl_index handle;
  mkcl_object bytecode = mk_cl_Cnil, interpreter_env, compiler_env;
  mkcl_env the_env;
@
  the_env = env;
  /*
   * Compile to bytecode.
   */
  if (compiler_env_p == mk_cl_Cnil) {
    interpreter_env = lex_env;
    compiler_env = mk_cl_Cnil;
  } else {
    interpreter_env = mk_cl_Cnil;
    compiler_env = lex_env;
  }
  old_c_env = the_env->c_env;
  c_new_env(the_env, &new_c_env, compiler_env, 0);
  guess_environment(the_env, interpreter_env);
  new_c_env.lex_env = lex_env;
  new_c_env.stepping = stepping != mk_cl_Cnil;
  new_c_env.mode = mkcl_Null(execute)? MODE_LOAD : MODE_EXECUTE;
  handle = asm_begin(the_env);
  MKCL_UNWIND_PROTECT_BEGIN(the_env) {
    compile_form(the_env, form, FLAG_VALUES);
    asm_op(the_env, OP_EXIT);
    bytecode = asm_end(the_env, handle, form);
  } MKCL_UNWIND_PROTECT_EXIT {
    /* Clear up */
    the_env->c_env = old_c_env;
    memset(&new_c_env, 0, sizeof(new_c_env));
  } MKCL_UNWIND_PROTECT_END;
  if (mkcl_Null(execute)) {
    @(return bytecode);
  }
  /*
   * Interpret using the given lexical environment.
   */
  MKCL_VALUES(0) = mk_cl_Cnil;
  MKCL_NVALUES = 0;
  {
    struct mkcl_temp_stack_frame frame;
    mkcl_object output;
    frame.t = mkcl_t_temp_stack_frame;
    frame.stack = frame.base = 0;
    frame.size = 0;
    frame.env = the_env;
    env->function = mk_cl_Cnil;
    output = mkcl_interpret(env, (mkcl_object)&frame, interpreter_env, bytecode);
    mkcl_dealloc(env, bytecode->bytecode.code);
    mkcl_dealloc(env, bytecode->bytecode.data);
    mkcl_dealloc(env, bytecode);
    return output;
  }
@)
