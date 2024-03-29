/* -*- mode: c -*- */
/*
    dpp.c -- Defun preprocessor.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2012, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


/*
	Usage:
		dpp [in-file [out-file]]

	The file named in-file is preprocessed and the output will be
	written to the file whose name is out-file. If in-file is "-"
	program is read from standard input, while if out-file is "-"
	C-program is written to standard output.


	The function definition:

	@(defun name ({var}*
		      [&optional {var | (var [initform [svar]])}*]
		      [&rest var]
		      [&key {var |
			     ({var | (keyword var)} [initform [svar]])}*
			    [&allow_other_keys]]
		      [&aux {var | (var [initform])}*])

		C-declaration

	@

		C-body

	@)

	name can be either an identifier or a full C procedure header
	enclosed in quotes (').

	&optional may be abbreviated as &o.
	&rest may be abbreviated as &r.
	&key may be abbreviated as &k.
	&allow_other_keys may be abbreviated as &aok.
	&aux may be abbreviated as &a.

	Each variable becomes a C variable.

	Each supplied-p parameter becomes a boolean C variable.

	Initforms are C expressions.
	If an expression contains non-alphanumeric characters,
	it should be surrounded by backquotes (`).


	Function return:

		@(return {form}*)




	Other facilities:


            Symbol lookup:  @'foobar' will be replaced by a direct reference to the symbol object

	    Runtime function lookup:

	        @si::snafu(arg0, ..., argN) or @"si::snafu"(arg0, ..., argN)
		
		is turned into a direct call to the C function implementing the function si::snafu

            Symbol function value lookup:  @+'foobar'

*/

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define DPP

#include "symbols_list2.h"


/* #define POOLSIZE        2048 */
#define POOLSIZE        4096
#define MAXREQ          16
#define MAXOPT          16
#define MAXKEY          16
#define MAXAUX          16
#define MAXRES          16

#define TRUE            1
#define FALSE           0

typedef int bool;

FILE *in, *out;

char filename[BUFSIZ];
int lineno;
int tab;
int tab_save;

char pool[POOLSIZE];
char *poolp;

char *function;
int   function_code;
char *function_symbol;
char *function_c_name;

char *required[MAXREQ];
int nreq;

struct optional {
  char *o_var;
  char *o_init;
  char *o_svar;
} optional[MAXOPT];
int nopt;

bool rest_flag;
char *rest_var;

bool key_flag;
struct keyword {
  char *k_key;
  char *k_var;
  char *k_init;
  char *k_svar;
} keyword[MAXKEY];
int nkey;
bool allow_other_keys_flag;

struct aux {
  char *a_var;
  char *a_init;
} aux[MAXAUX];
int naux;

char *result[MAXRES];
int nres;

void
put_lineno(void)
{
  static int flag = 0;
  if (flag)
    fprintf(out, "#line %d\n", lineno);
  else {
    flag++;
    fprintf(out, "#line %d \"%s\"\n", lineno, filename);
  }
}

void
error(char *s)
{
  printf("\n%s:%d: Error: %s.\n", filename, lineno, s);
  exit(1);
}

void
error_fmt1(char *fmt, char * arg)
{
  printf("\n%s:%d: Error: ", filename, lineno);
  printf(fmt, arg);
  printf(".\n");
  exit(1);
}

void
error_symbol(char *s)
{
  printf("\n%s:%d: Error: illegal symbol %s.\n", filename, lineno, s);
  exit(1);
}

int
readc(void)
{
  int c;

  c = getc(in);
  if (feof(in)) {
    if (function != NULL)
      error("unexpected end of file");
    exit(0);
  }
  if (c == '\n') {
    lineno++;
    tab = 0;
  } else if (c == '\t')
    tab++;
  return(c);
}

int
nextc(void)
{
  int c;

  while (isspace(c = readc()))
    ;
  return(c);
}

void
unreadc(int c)
{
  if (c == '\n')
    --lineno;
  else if (c == '\t')
    --tab;
  ungetc(c, in);
}

void
put_tabs(int n)
{
  put_lineno();
  while (n--)
    putc('\t', out);
}

void
pushc(int c)
{
  if (poolp >= &pool[POOLSIZE])
    error("buffer pool overflow");
  *poolp++ = c;
}

void
pushstr(const char *s)
{
  while (*s)
    pushc(*(s++));
}

int
search_keyword(const char *name)
{
  int i;
  char c[256];

  for (i=0; name[i] && i<255; i++)
    if (name[i] == '_')
      c[i] = '-';
    else
      c[i] = name[i];
  if (i == 255)
    error("Too long keyword");
  c[i] = 0;
  for (i = 0; mkcl_root_symbol_inits[i].name != NULL; i++) {
    if (mkcl_root_symbol_inits[i].name[0] == ':')
      if (!strcasecmp(c, mkcl_root_symbol_inits[i].name+1))
	return i;
  }
  error_fmt1("Keyword not found: %s.\n", c);
  return 0;
}

char *
search_symbol(char *name, int *symbol_code)
{
  int i;
  for (i = 0; mkcl_root_symbol_inits[i].name != NULL; i++) {
    if (!strcasecmp(name, mkcl_root_symbol_inits[i].name)) {
      name = poolp;
      if (i == 0) {
	pushstr("mk_cl_Cnil");
	pushc(0);
      } else {
	pushstr("MKCL_SYM(\"");
	pushstr(mkcl_root_symbol_inits[i].name);
	pushstr("\",");
	if (i >= 10000)  /* This stuff here introduces a hard-coded upper limit of 19999 */
	  pushc((i / 10000) % 10 + '0');  /* on the number of symbols! JCB  FIXME! */
	if (i >= 1000)
	  pushc((i / 1000) % 10 + '0');
	if (i >= 100)
	  pushc((i / 100) % 10 + '0');
	if (i >= 10)
	  pushc((i / 10) % 10 + '0');
	pushc(i % 10 + '0');
	pushstr(")");
	pushc(0);
      }
      if (symbol_code)
	*symbol_code = i;
      return name;
    }
  }
  return NULL;
}

char *
read_symbol()
{
  char c, *name = poolp;

  c = readc();
  while (c != '\'') {
    if (c == '_') c = '-';
    pushc(c); 
    c = readc();
  }
  pushc(0);

  name = search_symbol(poolp = name, 0);
  if (name == NULL) {
    name = poolp;
    error_fmt1("Unknown symbol: %s", name);
    pushstr("unknown");
  }
  return name;
}

char *
search_function(char *name)
{
  int i;
  for (i = 0; mkcl_root_symbol_inits[i].name != NULL; i++) {
    if (mkcl_root_symbol_inits[i].translation != NULL &&
	!strcasecmp(name, mkcl_root_symbol_inits[i].name)) {
      name = poolp;
      pushstr(mkcl_root_symbol_inits[i].translation);
      pushc(0);
      return name;
    }
  }
  return name;
}

char *
read_function()
{
  char c, *name = poolp;

  c = readc();
  if (c == '"') {
    c = readc();
    while (c != '"') {
      pushc(c);
      c = readc();
    }
    pushc(0);
    return name;
  }
  while (c != '(' && !isspace(c) && c != ')' && c != ',') {
    if (c == '_') c = '-';
    pushc(c); 
    c = readc();
  }
  unreadc(c);
  pushc(0);
  return name;
}

char *
translate_function(char *name)
{
  char *output = search_function(name);
  if (output == NULL) {
    error_fmt1("Unknown function: %s", name);
    pushstr("unknown");
    output = poolp;
  }
  return output;
}

char *
read_token(void)
{
  int c;
  int left_paren = 0;
  char *p;

  p = poolp;
  c = readc();
  while (isspace(c))
    c = readc();
  do {
    if (c == '(') {
      left_paren++;
      pushc(c);
    } else if (c == ')') {
      if (left_paren == 0) {
	break;
      } else {
	left_paren--;
	pushc(c);
      }
    } else if (isspace(c) && left_paren == 0) {
      do
	c = readc();
      while (isspace(c));
      break;
    } else if (c == '@') {
      c = readc();
      if (c == '\'') {
	(void)read_symbol();
	poolp--;
      } else if (c == '+') {
	c = readc();
	if (c == '\'') {
	  (void)read_symbol();
	  pushstr("->symbol.gfdef");
	  poolp--;
	} else unreadc(c);
      } else if (c == '@') {
	pushc(c);
      } else {
	char *name;
	unreadc(c);
	poolp = name = read_function();
	(void)translate_function(poolp);
      }
    } else {
      pushc(c);
    }
    c = readc();
  } while (1);
  unreadc(c);
  pushc('\0');
  return(p);
}

void
reset(void)
{
  int i;

  poolp = pool;
  function = NULL;
  function_symbol = "";
  function_c_name = "";
  nreq = 0;
  for (i = 0;  i < MAXREQ;  i++)
    required[i] = NULL;
  nopt = 0;
  for (i = 0;  i < MAXOPT;  i++)
    optional[i].o_var
      = optional[i].o_init
      = optional[i].o_svar
      = NULL;
  rest_flag = FALSE;
  rest_var = "ARGS";
  key_flag = FALSE;
  nkey = 0;
  for (i = 0;  i < MAXKEY;  i++)
    keyword[i].k_key
      = keyword[i].k_var
      = keyword[i].k_init
      = keyword[i].k_svar
      = NULL;
  allow_other_keys_flag = FALSE;
  naux = 0;
  for (i = 0;  i < MAXAUX;  i++)
    aux[i].a_var
      = aux[i].a_init
      = NULL;
}

void
get_function(void)
{
  function = read_function();
  function_symbol = search_symbol(function, &function_code);
  if (function_symbol == NULL) {
    function_symbol = poolp;
    pushstr("mk_cl_Cnil");
    pushc('\0');
  }
  function_c_name = translate_function(function);
}

void
get_lambda_list(void)
{
  int c;
  char *p;

  if ((c = nextc()) != '(')
    error("( expected");
  for (;;) {
    if ((c = nextc()) == ')')
      return;
    if (c == '&') {
      p = read_token();
      goto OPTIONAL;
    }
    unreadc(c);
    p = read_token();
    if (nreq >= MAXREQ)
      error("too many required variables");
    required[nreq++] = p;
  }

 OPTIONAL:
  if (strcmp(p, "optional") != 0 && strcmp(p, "o") != 0)
    goto REST;
  for (;;  nopt++) {
    if ((c = nextc()) == ')')
      return;
    if (c == '&') {
      p = read_token();
      goto REST;
    }
    if (nopt >= MAXOPT)
      error("too many optional argument");
    if (c == '(') {
      optional[nopt].o_var = read_token();
      if ((c = nextc()) == ')')
	continue;
      unreadc(c);
      optional[nopt].o_init = read_token();
      if ((c = nextc()) == ')')
	continue;
      unreadc(c);
      optional[nopt].o_svar = read_token();
      if (nextc() != ')')
	error(") expected");
    } else {
      unreadc(c);
      optional[nopt].o_var = read_token();
    }
  }

 REST:
  if (strcmp(p, "rest") != 0 && strcmp(p, "r") != 0)
    goto KEY;
  rest_flag = TRUE;
  if ((c = nextc()) == ')' || c == '&')
    error("&rest var missing");
  unreadc(c);
  rest_var = read_token();
  if ((c = nextc()) == ')')
    return;
  if (c != '&')
    error("& expected");
  p = read_token();
  goto KEY;

 KEY:
  if (strcmp(p, "key") != 0 && strcmp(p, "k") != 0)
    goto AUX;
  key_flag = TRUE;
  for (;;  nkey++) {
    if ((c = nextc()) == ')')
      return;
    if (c == '&') {
      p = read_token();
      if (strcmp(p, "allow_other_keys") == 0 ||
	  strcmp(p, "aok") == 0) {
	allow_other_keys_flag = TRUE;
	if ((c = nextc()) == ')')
	  return;
	if (c != '&')
	  error("& expected");
	p = read_token();
      }
      goto AUX;
    }
    if (nkey >= MAXKEY)
      error("too many optional argument");
    if (c == '(') {
      if ((c = nextc()) == '(') {
	p = read_token();
	if (p[0] != ':' || p[1] == '\0')
	  error("keyword expected");
	keyword[nkey].k_key = p + 1;
	keyword[nkey].k_var = read_token();
	if (nextc() != ')')
	  error(") expected");
      } else {
	unreadc(c);
	keyword[nkey].k_key
	  = keyword[nkey].k_var
	  = read_token();
      }
      if ((c = nextc()) == ')')
	continue;
      unreadc(c);
      keyword[nkey].k_init = read_token();
      if ((c = nextc()) == ')')
	continue;
      unreadc(c);
      keyword[nkey].k_svar = read_token();
      if (nextc() != ')')
	error(") expected");
    } else {
      unreadc(c);
      keyword[nkey].k_key
	= keyword[nkey].k_var
	= read_token();
    }
  }

 AUX:
  if (strcmp(p, "aux") != 0 && strcmp(p, "a") != 0)
    error("illegal lambda-list keyword");
  for (;;) {
    if ((c = nextc()) == ')')
      return;
    if (c == '&')
      error("illegal lambda-list keyword");
    if (naux >= MAXAUX)
      error("too many auxiliary variable");
    if (c == '(') {
      aux[naux].a_var = read_token();
      if ((c = nextc()) == ')')
	continue;
      unreadc(c);
      aux[naux].a_init = read_token();
      if (nextc() != ')')
	error(") expected");
    } else {
      unreadc(c);
      aux[naux].a_var = read_token();
    }
    naux++;
  }
}

void
get_return(void)
{
  int c;

  nres = 0;
  for (;;) {
    if ((c = nextc()) == ')')
      return;
    unreadc(c);
    result[nres++] = read_token();
  }
}

void
put_fhead(void)
{
  int i;

  put_lineno();
  fprintf(out, "mkcl_object %s(MKCL, mkcl_narg narg", function_c_name);
  for (i = 0; i < nreq; i++)
    fprintf(out, ", mkcl_object %s", required[i]);
  if (nopt > 0 || rest_flag || key_flag)
    fprintf(out, ", ...");
  fprintf(out, ")\n{\n");
  put_lineno(); /* JCB */
  fprintf(out, "  mkcl_call_stack_check(env);\n{\n"); /* JCB */
}

void
put_declaration(void)
{
  int i;
  int simple_varargs;

  for (i = 0;  i < nopt;  i++) {
    put_lineno();
    fprintf(out, "\tmkcl_object %s;\n", optional[i].o_var);
  }
  for (i = 0;  i < nopt;  i++)
    if (optional[i].o_svar != NULL) {
      put_lineno();
      fprintf(out, "\tbool %s;\n", optional[i].o_svar);
    }
  if (key_flag) {
    put_lineno();
    if (nkey) {
      fprintf(out, "\tstatic mkcl_object KEYS[%d] = {", nkey);
      for (i = 0; i < nkey; i++) {
	if (i > 0)
	  fprintf(out, ", ");
	fprintf(out, "(mkcl_object)(mkcl_root_symbols+%d)", search_keyword(keyword[i].k_key));
      }
      fprintf(out, "};\n");
    } else {
      fprintf(out, "\tmkcl_object *KEYS = NULL;\n");
    }
  }
  for (i = 0;  i < nkey;  i++) {
    fprintf(out, "\tmkcl_object %s;\n", keyword[i].k_var);
    if (keyword[i].k_svar != NULL)
      fprintf(out, "\tbool %s;\n", keyword[i].k_svar);
  }
  for (i = 0;  i < naux;  i++) {
    put_lineno();
    fprintf(out, "\tmkcl_object %s;\n", aux[i].a_var);
  }
  if (nopt == 0 && !rest_flag && !key_flag) {
    put_lineno();
    fprintf(out, "\tif (narg!=%d) mkcl_FEwrong_num_arguments(env, MKCL_MAKE_FIXNUM(%d), %d, %d, narg);\n", nreq, function_code, nreq, nreq);
  } else {
    simple_varargs = !rest_flag && !key_flag && ((nreq + nopt) < 32);
    if (key_flag) {
      put_lineno();
      /* We do this because Microsoft VC++ does not support arrays of zero size */
      if (nkey) {
	fprintf(out, "\tmkcl_object KEY_VARS[%d];\n", 2*nkey);
      } else {
	fprintf(out, "\tmkcl_object *KEY_VARS = NULL;\n");
      }
    }
    put_lineno();
    if (simple_varargs)
	fprintf(out,"\tva_list %s;\n\tva_start(%s, %s);\n",
		rest_var, rest_var, ((nreq > 0) ? required[nreq-1] : "narg"));
    else
	fprintf(out,"\tmkcl_va_list %s;\n\tmkcl_va_start(env, %s, %s, narg, %d);\n",
		rest_var, rest_var, ((nreq > 0) ? required[nreq-1] : "narg"),
		nreq);
    put_lineno();
    {
      int n_min = nreq;
      int n_max = -1;

      fprintf(out, "\tif (narg < %d", nreq);
      if (nopt > 0 && !rest_flag && !key_flag) {
        fprintf(out, "|| narg > %d", nreq + nopt);
        n_max = nreq + nopt;
      }
      fprintf(out, ") mkcl_FEwrong_num_arguments(env, MKCL_MAKE_FIXNUM(%d), %d, %d, narg);\n",
              function_code, n_min, n_max);
    }
    for (i = 0;  i < nopt;  i++) {
      put_lineno();
      fprintf(out, "\tif (narg > %d) {\n", nreq+i);
      put_lineno();
      fprintf(out, simple_varargs?
	      "\t\t%s = va_arg(%s,mkcl_object);\n":
	      "\t\t%s = mkcl_va_arg(%s);\n",
	      optional[i].o_var, rest_var);
      if (optional[i].o_svar) {
	put_lineno();
	fprintf(out, "\t\t%s = TRUE;\n", optional[i].o_svar);
      }
      put_lineno();
      fprintf(out, "\t} else {\n");
      put_lineno();
      fprintf(out, "\t\t%s = %s;\n",
	      optional[i].o_var,
	      optional[i].o_init == NULL ? "mk_cl_Cnil" : optional[i].o_init);
      if (optional[i].o_svar) {
	put_lineno();
	fprintf(out, "\t\t%s = FALSE;\n", optional[i].o_svar);
      }
      put_lineno();
      fprintf(out, "\t}\n");
    }
    if (key_flag) {
      put_lineno();
      fprintf(out, "\tmkcl_parse_key(env, ARGS, %d, KEYS, KEY_VARS, NULL, %d, FALSE);\n",
	      nkey, allow_other_keys_flag);
      for (i = 0;  i < nkey;  i++) {
	put_lineno();
	fprintf(out, "\tif (KEY_VARS[%d]==mk_cl_Cnil) {\n", nkey+i);
	if (keyword[i].k_init != NULL) {
	  put_lineno();
	  fprintf(out, "\t  %s = %s;\n", keyword[i].k_var, keyword[i].k_init);
	} else {
	  put_lineno();
	  fprintf(out, "\t  %s = mk_cl_Cnil;\n", keyword[i].k_var);
	}
	if (keyword[i].k_svar != NULL) {
	  put_lineno();
	  fprintf(out, "\t  %s = FALSE;\n", keyword[i].k_svar);
	}
	fprintf(out, "\t} else {\n");
	if (keyword[i].k_svar != NULL) {
	  put_lineno();
	  fprintf(out, "\t  %s = TRUE;\n", keyword[i].k_svar);
	}
	put_lineno();
	fprintf(out, "\t  %s = KEY_VARS[%d];\n\t}\n", keyword[i].k_var, i);
      }
    }
    if (!rest_flag)
      {
        put_lineno();
        if (simple_varargs)
          fprintf(out,"\tva_end(%s);\n", rest_var);
        else
          fprintf(out,"\tmkcl_va_end(%s);\n", rest_var);
      }
  }
  for (i = 0;  i < naux;  i++) {
    put_lineno();
    fprintf(out, "\t%s = %s;\n", aux[i].a_var,
	    aux[i].a_init == NULL ? "mk_cl_Cnil" : aux[i].a_init);
  }
}

void
put_return(void)
{
  int i, t;

  t = tab_save+1;
  if (nres == 0) {
    fprintf(out, "mkcl_return0();");
#if 0
  } else if (nres == 1) {
    fprintf(out, "mkcl_return1(%s);", result[0]);
#endif
  } else {
    fprintf(out, "{\n");
    put_tabs(t);
    for (i = 0;  i < nres;  i++) {
      put_tabs(t);
      fprintf(out, "const mkcl_object __value%d = %s;\n", i, result[i]);
    }
    put_tabs(t);
    fprintf(out, "env->nvalues = %d;\n", nres);
    for (i = nres-1;  i > 0;  i--) {
      put_tabs(t);
      fprintf(out, "env->values[%d] = __value%d;\n", i, i);
    }
    put_tabs(t);
    fprintf(out, "return env->values[0] = __value0;\n");
    put_tabs(tab_save);
    fprintf(out, "}");
  }
}

int
jump_to_at(void)
{
  int c;
 GO_ON:
  while ((c = readc()) != '@')
    putc(c, out);
  if ((c = readc()) == '@') {
    putc(c, out);
    goto GO_ON;
  }
  return c;
}

void
main_loop(void)
{
  int c;
  int in_defun=0;
  char *p;

  lineno = 1;

  reset();
  put_lineno();
 LOOP:
  c = jump_to_at();
  if (c == ')') {
    if (!in_defun)
      error("unmatched @) found");
    in_defun = 0;
    putc('}',out); /* JCB */
    putc('}',out);
    reset();
    goto LOOP;
  } else if (c == '\'') {
    char *p;
    poolp = pool;
    p = read_symbol();
    pushc('\0');
    fprintf(out,"%s",p);
    goto LOOP;
  } else if (c == '+') {
    c = readc();
    if (c == '\'') {
      char *p;
      poolp = pool;
      p = read_symbol();
      pushc('\0');
      fprintf(out,"%s->symbol.gfdef",p);
    } else unreadc(c);
    goto LOOP;
  } else if (c != '(') {
    char *p;
    unreadc(c);
    poolp = pool;
    poolp = p = read_function();
    fprintf(out,"%s",translate_function(poolp));
    goto LOOP;
  }
  p = read_token();
  if (strcmp(p, "defun") == 0) {
    if (in_defun)
      error("@) expected before new function definition");
    in_defun = 1;
    get_function();
    get_lambda_list();
    put_fhead();
    /* put_lineno(); */ /* JCB */
    c = jump_to_at();
    unreadc(c);
    put_declaration();
    put_lineno();
  } else if (strcmp(p, "return") == 0) {
    tab_save = tab;
    get_return();
    put_return();
  } else
    error_symbol(p);
  goto LOOP;
}

int
main(int argc, char **argv)
{
  char outfile[BUFSIZ];
#ifdef _MSC_VER
  char *p;
#endif
  if (argc < 2 || !strcmp(argv[1],"-")) {
    in = stdin;
    strcpy(filename, "-");
  } else {
    in = fopen(argv[1],"r");
    strncpy(filename, argv[1], BUFSIZ-1);
    filename[BUFSIZ-1] = '\0';
  }
#ifdef _MSC_VER
  /* Convert all backslashes in filename into slashes,
   * to avoid warnings when compiling with MSVC
   */
  for ( p=filename; *p; p++ )
    if ( *p == '\\' )
      *p = '/';
#endif
  if (argc < 3 || !strcmp(argv[2],"-")) {
    out = stdout;
    strcpy(outfile, "-");
  } else {
    out = fopen(argv[2],"w");
    strncpy(outfile, argv[2], BUFSIZ-1);
    outfile[BUFSIZ-1] = '\0';
  }
  if (in == NULL)
    error("can't open input file");
  if (out == NULL)
    error("can't open output file");
  printf("dpp: %s -> %s\n", filename, outfile);
  main_loop();
  return 0;
}
