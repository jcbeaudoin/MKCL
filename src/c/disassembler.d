/* -*- mode: c -*- */
/*
    disassembler.c -- Byte compiler and function evaluator
*/
/*
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/mkcl-inl.h>
#include <mkcl/bytecode.h>

static mkcl_opcode *disassemble(MKCL, mkcl_object bytecode, mkcl_opcode *vector);

static mkcl_opcode *base = NULL;

static void
print_noarg(MKCL, const char *s) {
  mkcl_princ_str(env, s, mk_cl_Cnil);
}

static void
print_oparg(MKCL, const char *s, mkcl_word n) {
  mkcl_princ_str(env, s, mk_cl_Cnil);
  mkcl_princ(env, MKCL_MAKE_FIXNUM(n), mk_cl_Cnil);
}

static void
print_arg(MKCL, const char *s, mkcl_object x) {
  mkcl_princ_str(env, s, mk_cl_Cnil);
  mkcl_princ(env, x, mk_cl_Cnil);
}

static void
print_oparg_arg(MKCL, const char *s, mkcl_word n, mkcl_object x) {
  mkcl_princ_str(env, s, mk_cl_Cnil);
  mkcl_princ(env, MKCL_MAKE_FIXNUM(n), mk_cl_Cnil);
  mkcl_princ_str(env, ",", mk_cl_Cnil);
  mkcl_princ(env, x, mk_cl_Cnil);
}

static void
disassemble_lambda(MKCL, mkcl_object bytecode)
{
  mkcl_object *data;
  mkcl_opcode *vector;

  mkcl_bds_bind(env, @'*print-pretty*', mk_cl_Cnil);

  /* Print required arguments */
  data = bytecode->bytecode.data;

  /* Name of LAMBDA */
  print_arg(env, "\nName:\t\t", bytecode->bytecode.name);
  if (bytecode->bytecode.name == MKCL_OBJNULL ||
      bytecode->bytecode.name == @'si::bytecode') {
    print_noarg(env, "\nEvaluated form:");
    goto NO_ARGS;
  }

  /* Print aux arguments */
  print_arg(env, "\nDocumentation:\t", *(data++));
  print_arg(env, "\nDeclarations:\t", *(data++));

 NO_ARGS:
  base = vector = (mkcl_opcode *)bytecode->bytecode.code;
  disassemble(env, bytecode, vector);

  mkcl_bds_unwind1(env);
}

/* -------------------- DISASSEMBLER CORE -------------------- */

/* OP_FLET	nfun{arg}, fun1{object}
   ...

	Executes the enclosed code in a lexical enviroment extended with
	the functions "fun1" ... "funn".
*/
static mkcl_opcode *
disassemble_flet(MKCL, mkcl_object bytecode, mkcl_opcode *vector) {
	mkcl_index nfun, first;
	mkcl_object *data;
	GET_OPARG(nfun, vector);
	GET_OPARG(first, vector);
	data = bytecode->bytecode.data + first;
	print_noarg(env, "FLET");
	while (nfun--) {
		mkcl_object fun = *(data++);
		print_arg(env, "\n\tFLET\t", fun->bytecode.name);
	}
	return vector;
}

/* OP_LABELS	nfun{arg}, fun1{object}
   ...

	Executes the enclosed code in a lexical enviroment extended with
	the functions "fun1" ... "funn".
*/
static mkcl_opcode *
disassemble_labels(MKCL, mkcl_object bytecode, mkcl_opcode *vector) {
	mkcl_index nfun, first;
	mkcl_object *data;
	GET_OPARG(nfun, vector);
	GET_OPARG(first, vector);
	data = bytecode->bytecode.data + first;
	print_noarg(env, "LABELS");
	while (nfun--) {
		mkcl_object fun = *(data++);
		print_arg(env, "\n\tLABELS\t", fun->bytecode.name);
	}
	return vector;
}

/* OP_PROGV	bindings{list}
   ...
   OP_EXIT
	Execute the code enclosed with the special variables in BINDINGS
	set to the values in the list which was passed in MKCL_VALUES(0).
*/
static mkcl_opcode *
disassemble_progv(MKCL, mkcl_object bytecode, mkcl_opcode *vector) {
  print_noarg(env, "PROGV");
  vector = disassemble(env, bytecode, vector);
  print_noarg(env, "\t\t; progv");
	return vector;
}

/* OP_TAGBODY	n{arg}
   label1
   ...
   labeln
label1:
   ...
labeln:
   ...
   OP_EXIT

	High level construct for the TAGBODY form.
*/
static mkcl_opcode *
disassemble_tagbody(MKCL, mkcl_object bytecode, mkcl_opcode *vector) {
	mkcl_index i, ntags;
	mkcl_opcode *destination;
	GET_OPARG(ntags, vector);
	print_noarg(env, "TAGBODY");
	for (i=0; i<ntags; i++) {
		GET_LABEL(destination, vector);
		mkcl_princ_str(env, "\n\tTAG\t", mk_cl_Ct);
		mkcl_princ(env, MKCL_MAKE_FIXNUM(i), mk_cl_Ct);
		mkcl_princ_str(env, " @@ ", mk_cl_Ct);
		mkcl_princ(env, MKCL_MAKE_FIXNUM(destination - base), mk_cl_Ct);
	}
	vector = disassemble(env, bytecode, vector);
	print_noarg(env, "\t\t; tagbody");

	return vector;
}

static mkcl_opcode *
disassemble(MKCL, mkcl_object bytecode, mkcl_opcode *vector) {
	const char *string;
	mkcl_object o;
	mkcl_word n, m;
	mkcl_object line_format;
	mkcl_object *data = bytecode->bytecode.data;
	mkcl_object line_no;

	if (mk_cl_fboundp(env, @'si::formatter-aux') != mk_cl_Cnil)
	  line_format = mkcl_make_simple_base_string(env, "~%~4d\t");
	else
		line_format = mk_cl_Cnil;
 BEGIN:
	if (1) {
		line_no = MKCL_MAKE_FIXNUM(vector-base);
	} else {
		line_no = @'*';
	}
	if (line_format != mk_cl_Cnil) {
	  mk_cl_format(env, 3, mk_cl_Ct, line_format, line_no);
	} else {
	  mkcl_princ_char(env, '\n', mk_cl_Ct);
	  mkcl_princ(env, line_no, mk_cl_Ct);
	  mkcl_princ_char(env, '\t', mk_cl_Ct);
	}
	switch (GET_OPCODE(vector)) {

	/* OP_NOP
		Sets MKCL_VALUES(0) = NIL and MKCL_NVALUES = 1
	*/
	case OP_NOP:		string = "NOP";	goto NOARG;

	case OP_INT:		string = "QUOTE\t";
				GET_OPARG(n, vector);
				goto OPARG;

	case OP_PINT:		string = "PUSH\t";
				GET_OPARG(n, vector);
				goto OPARG;

	/* OP_QUOTE
		Sets MKCL_VALUES(0) to an immediate value.
	*/
	case OP_QUOTE:		string = "QUOTE\t";
				GET_DATA(o, vector, data);
				goto ARG;

	/* OP_VAR	n{arg}
		Sets MKCL_NVALUES=1 and MKCL_VALUES(0) to the value of the n-th local.
	*/
	case OP_VAR:		string = "VAR\t";
				GET_OPARG(n, vector);
				goto OPARG;

	/* OP_VARS	var{symbol}
		Sets MKCL_NVALUES=1 and MKCL_VALUES(0) to the value of the symbol VAR.
		VAR should be either a special variable or a constant.
	*/
	case OP_VARS:		string = "VARS\t";
				GET_DATA(o, vector, data);
				goto ARG;

	/* OP_PUSH
		Pushes the object in MKCL_VALUES(0).
	*/
	case OP_PUSH:		string = "PUSH\tMKCL_VALUES(0)";
				goto NOARG;

	case OP_VALUEREG0:	string = "SET\tMKCL_VALUES(0),REG0";
				goto NOARG;

	/* OP_PUSHV	n{arg}
		Pushes the value of the n-th local onto the stack.
	*/
	case OP_PUSHV:		string = "PUSHV\t";
				GET_OPARG(n, vector);
				goto OPARG;

	/* OP_PUSHVS	var{symbol}
		Pushes the value of the symbol VAR onto the stack.
		VAR should be either a special variable or a constant.
	*/
	case OP_PUSHVS:		string = "PUSHVS\t";
				GET_DATA(o, vector, data);
				goto ARG;

	/* OP_PUSHQ	value{object}
		Pushes "value" onto the stack.
	*/
	case OP_PUSHQ:		string = "PUSH\t'";
				GET_DATA(o, vector, data);
				goto ARG;

	/* OP_PUSHVALUES
		Pushes the values output by the last form, plus the number
		of values.
	*/
	case OP_PUSHVALUES:	string = "PUSH\tVALUES";
				goto NOARG;
	/* OP_PUSHMOREVALUES
		Adds more values to the ones pushed by OP_PUSHVALUES.
	*/
	case OP_PUSHMOREVALUES:	string = "PUSH\tMORE VALUES";
				goto NOARG;
	/* OP_POP
		Pops a single value pushed by a OP_PUSH[V[S]] operator.
	*/
	case OP_POP:		string = "POP";
				goto NOARG;
	/* OP_POP1
		Pops a single value pushed by a OP_PUSH[V[S]] operator.
	*/
	case OP_POP1:		string = "POP1";
				goto NOARG;
	/* OP_POPVALUES
		Pops all values pushed by a OP_PUSHVALUES operator.
	*/
	case OP_POPVALUES:	string = "POP\tVALUES";
				goto NOARG;

	case OP_BLOCK:		string = "BLOCK\t";
				GET_DATA(o, vector, data);
				goto ARG;
	case OP_CATCH:		string = "CATCH\tREG0";
				goto NOARG;
	case OP_DO:		string = "BLOCK\t";
				o = mk_cl_Cnil;
				goto ARG;
	case OP_FRAME:		string = "FRAME\t";
				goto JMP;

	/* OP_CALL	n{arg}
		Calls the function in MKCL_VALUES(0) with N arguments which
		have been deposited in the stack. The output values
		are left in MKCL_VALUES(...)
	*/
	case OP_CALL:		string = "CALL\t";
				GET_OPARG(n, vector);
				goto OPARG;

	/* OP_CALLG	n{arg}, name{arg}
		Calls the function NAME with N arguments which have been
		deposited in the stack. The output values are left in VALUES.
	*/
	case OP_CALLG:		string = "CALLG\t";
				GET_OPARG(n, vector);
				GET_DATA(o, vector, data);
				goto OPARG_ARG;

	/* OP_FCALL	n{arg}
		Calls the function in the stack with N arguments which
		have been also deposited in the stack. The output values
		are left in MKCL_VALUES(...)
	*/
	case OP_STEPCALL:
	case OP_FCALL:		string = "FCALL\t";
				GET_OPARG(n, vector);
				goto OPARG;

	/* OP_MCALL
		Similar to FCALL, but gets the number of arguments from
		the stack (They all have been deposited by OP_PUSHVALUES)
	*/
	case OP_MCALL:		string = "MCALL";
				goto NOARG;

	/* OP_POPREQ
		Extracts next required argument.
	*/
	case OP_POPREQ:		string = "POP\tREQ";
				goto NOARG;
	/* OP_NOMORE
		Ensure there are no more arguments.
	*/
	case OP_NOMORE:		string = "NOMORE";
				goto NOARG;
	/* OP_POPOPT
		Extracts next optional argument.
	*/
	case OP_POPOPT:		string = "POP\tOPT";
				goto NOARG;
	/* OP_POPREST
		Extracts list of remaining arguments.
	*/
	case OP_POPREST:	string = "POP\tREST";
				goto NOARG;
        /* OP_PUSHKEYS
        	Parses the keyword arguments
        */
        case OP_PUSHKEYS:	string = "PUSH\tKEYS ";
				GET_DATA(o, vector, data);
                		goto ARG;

	/* OP_EXIT
		Marks the end of a high level construct
	*/
	case OP_EXIT:		print_noarg(env, "EXIT");
				return vector;
	/* OP_EXIT_FRAME
		Marks the end of a high level construct (BLOCK, CATCH...)
	*/
	case OP_EXIT_FRAME:	string = "EXIT\tFRAME";
				goto NOARG;
	/* OP_EXIT_TAGBODY
		Marks the end of a high level construct (TAGBODY)
	*/
	case OP_EXIT_TAGBODY:	print_noarg(env, "EXIT\tTAGBODY");
				return vector;

	case OP_FLET:		vector = disassemble_flet(env, bytecode, vector);
				break;
	case OP_LABELS:		vector = disassemble_labels(env, bytecode, vector);
				break;

	/* OP_LFUNCTION	name{symbol}
		Extracts the function associated to a symbol. The function
		may be defined in the global environment or in the local
		environment. This last value takes precedence.
	*/
	case OP_LFUNCTION:	string = "LOCFUNC\t";
				GET_OPARG(n, vector);
				goto OPARG;

	/* OP_FUNCTION	name{symbol}
		Extracts the function associated to a symbol. The function
		may be defined in the global environment or in the local
		environment. This last value takes precedence.
	*/
	case OP_FUNCTION:	string = "SYMFUNC\t";
				GET_DATA(o, vector, data);
				goto ARG;

	/* OP_CLOSE	name{arg}
		Extracts the function associated to a symbol. The function
		may be defined in the global environment or in the local
		environment. This last value takes precedence.
	*/
	case OP_CLOSE:		string = "CLOSE\t";
				GET_DATA(o, vector, data);
				goto ARG;

	/* OP_GO	n{arg}
	   OP_QUOTE	tag-name{symbol}
		Jumps to the tag which is defined at the n-th position in
		the lexical environment. TAG-NAME is kept for debugging
		purposes.
	*/
	case OP_GO:		string = "GO\t";
				GET_OPARG(n, vector);
				GET_DATA(o, vector, data);
				goto OPARG_ARG;

	/* OP_RETURN	n{arg}
		Returns from the block whose record in the lexical environment
		occuppies the n-th position.
	*/
	case OP_RETURN:		string = "RETFROM";
				GET_OPARG(n, vector);
				goto OPARG;

	/* OP_THROW
		Jumps to an enclosing CATCH form whose tag matches the one
		of the THROW. The tag is taken from the stack, while the
		output values are left in MKCL_VALUES(...).
	*/
	case OP_THROW:		string = "THROW";
				goto NOARG;

	/* OP_JMP	label{arg}
	   OP_JNIL	label{arg}
	   OP_JT	label{arg}
	   OP_JEQ	label{arg}, value{object}
	   OP_JNEQ	label{arg}, value{object}
		Direct or conditional jumps. The conditional jumps are made
		comparing with the value of MKCL_VALUES(0).
	*/
	case OP_JMP:		string = "JMP\t";
				goto JMP;
	case OP_JNIL:		string = "JNIL\t";
				goto JMP;
	case OP_JT:		string = "JT\t";
	JMP: {			GET_OPARG(m, vector);
				n = vector + m - OPARG_SIZE - base;
				goto OPARG;
	}
	case OP_JEQL:		string = "JEQL\t";
				goto JEQL;
	case OP_JNEQL:		string = "JNEQL\t";
	JEQL: {			GET_DATA(o, vector, data);
				GET_OPARG(m, vector);
				n = vector + m - OPARG_SIZE - base;
				goto OPARG_ARG;
	}
	case OP_NOT:		string = "NOT";
				goto NOARG;

	/* OP_UNBIND	n{arg}
		Undo "n" bindings of lexical variables.
	*/
	case OP_UNBIND:		string = "UNBIND\t";
				GET_OPARG(n, vector);
				goto OPARG;
	/* OP_UNBINDS	n{arg}
		Undo "n" bindings of special variables.
	*/
	case OP_UNBINDS:	string = "UNBINDS\t";
				GET_OPARG(n, vector);
				goto OPARG;
	/* OP_BIND	name{symbol}
	   OP_PBIND	name{symbol}
	   OP_BINDS	name{symbol}
	   OP_PBINDS	name{symbol}
		Binds a lexical or special variable to the either the
		value of MKCL_VALUES(0), to the first value of the stack, or
		to the n-th value of MKCL_VALUES(...).
	*/
	case OP_BIND:		string = "BIND\t";
				GET_DATA(o, vector, data);
				goto ARG;
	case OP_PBIND:		string = "PBIND\t";
				GET_DATA(o, vector, data);
				goto ARG;
	case OP_VBIND:		string = "VBIND\t";
				GET_OPARG(n, vector);
				GET_DATA(o, vector, data);
				goto OPARG_ARG;
	case OP_BINDS:		string = "BINDS\t";
				GET_DATA(o, vector, data);
				goto ARG;
	case OP_PBINDS:		string = "PBINDS\t";
				GET_DATA(o, vector, data);
				goto ARG;
	case OP_VBINDS:		string = "VBINDS\t";
				GET_OPARG(n, vector);
				GET_DATA(o, vector, data);
				goto OPARG_ARG;
	/* OP_SETQ	n{arg}
	   OP_PSETQ	n{arg}
	   OP_SETQS	var-name{symbol}
	   OP_PSETQS	var-name{symbol}
		Sets either the n-th local or a special variable VAR-NAME,
		to either the value in MKCL_VALUES(0) (OP_SETQ[S]) or to the 
		first value on the stack (OP_PSETQ[S]).
	*/
	case OP_SETQ:		string = "SETQ\t";
				GET_OPARG(n, vector);
				goto OPARG;
	case OP_PSETQ:		string = "PSETQ\t";
				GET_OPARG(n, vector);
				goto OPARG;
	case OP_VSETQ:		string = "VSETQ\t";
				GET_OPARG(m, vector);
				o = MKCL_MAKE_FIXNUM(m);
				GET_OPARG(n, vector);
				goto OPARG_ARG;
	case OP_SETQS:		string = "SETQS\t";
				GET_DATA(o, vector, data);
				goto ARG;
	case OP_PSETQS:		string = "PSETQS\t";
				GET_DATA(o, vector, data);
				goto ARG;
	case OP_VSETQS:		string = "VSETQS\t";
				GET_DATA(o, vector, data);
				GET_OPARG(n, vector);
				goto OPARG_ARG;

	case OP_PROGV:		vector = disassemble_progv(env, bytecode, vector);
				break;

	/* OP_VALUES	n{arg}
		Pop N values from the stack and store them in MKCL_VALUES(...)
	*/
	case OP_VALUES:		string = "VALUES\t";
				GET_OPARG(n, vector);
				goto OPARG;
	/* OP_NTHVAL
		Set MKCL_VALUES(0) to the N-th value of the MKCL_VALUES(...) list.
		The index N-th is extracted from the top of the stack.
	*/
	case OP_NTHVAL:		string = "NTHVAL\t";
				goto NOARG;
	case OP_TAGBODY:	vector = disassemble_tagbody(env, bytecode, vector);
				break;
	case OP_PROTECT:	string = "PROTECT\t";
				goto JMP;
	case OP_PROTECT_NORMAL:	string = "PROTECT\tNORMAL";
				goto NOARG;
	case OP_PROTECT_EXIT:	string = "PROTECT\tEXIT";
				goto NOARG;
	case OP_NIL:		string = "QUOTE\tNIL";
				goto NOARG;
	case OP_PUSHNIL:	string = "PUSH\t'NIL";
		    		goto NOARG;
	case OP_STEPIN:		string = "STEP\tIN,";
				GET_DATA(o, vector, data);
				goto ARG;
	case OP_STEPOUT:	string = "STEP\tOUT";
				goto NOARG;

	case OP_CONS:		string = "CONS"; goto NOARG;
	case OP_ENDP:		string = "ENDP\tREG0"; goto NOARG;
	case OP_CAR:		string = "CAR\tREG0"; goto NOARG;
	case OP_CDR:		string = "CDR\tREG0"; goto NOARG;
	case OP_LIST:		string = "LIST\t";
				GET_OPARG(n, vector);
				goto OPARG;
	case OP_LISTA:		string = "LIST*\t";
				GET_OPARG(n, vector);
				goto OPARG;
	case OP_CALLG1:		string = "CALLG1\t";
				GET_DATA(o, vector, data);
				goto ARG;
	case OP_CALLG2:		string = "CALLG2\t";
				GET_DATA(o, vector, data);
				goto ARG;

	default:
	  mkcl_FEerror(env, "Unknown code ~S", 1, MKCL_MAKE_FIXNUM(*(vector-1)));
		return vector;
	NOARG:			print_noarg(env, string);
				break;
	ARG:			print_noarg(env, string);
				@prin1(env, 1, o);
				break;
	OPARG:			print_oparg(env, string, n);
				break;
	OPARG_ARG:		print_oparg_arg(env, string, n, o);
				break;
	}
	goto BEGIN;
}

mkcl_object
mk_si_bc_disassemble(MKCL, mkcl_object v)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(v) == mkcl_t_bclosure) {
    v = v->bclosure.code;
  }
  if (mkcl_type_of(v) == mkcl_t_bytecode) {
    disassemble_lambda(env, v);
    @(return v);
  }
  @(return mk_cl_Cnil);
}

mkcl_object
mk_si_bc_split(MKCL, mkcl_object b)
{
     	mkcl_object vector;
	mkcl_object data;
	mkcl_object lex = mk_cl_Cnil;

  mkcl_call_stack_check(env);
	if (mkcl_type_of(b) == mkcl_t_bclosure) {
		lex = b->bclosure.lex;
		b = b->bclosure.code;
	}
	if (mkcl_type_of(b) != mkcl_t_bytecode)
	  @(return mk_cl_Cnil mk_cl_Cnil);
	vector = mkcl_alloc_simple_vector(env, b->bytecode.code_size, mkcl_aet_b8);
	vector->vector.self.b8 = (uint8_t*)b->bytecode.code;
	data = mkcl_alloc_simple_vector(env, b->bytecode.data_size, mkcl_aet_object);
	data->vector.self.t = b->bytecode.data;
	@(return lex vector data)
}

