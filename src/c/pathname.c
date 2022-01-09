/* -*- mode: c -*- */
/*
    pathname.d -- Pathnames.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2017,2021, Jean-Claude Beaudoin

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

/*
	This file contains those functions that interpret namestrings.
*/

#include <mkcl/mkcl.h>
#include <mkcl/mkcl-inl.h>
#include <mkcl/internal.h>
#include <string.h>
#include <ctype.h>

static void
push_substring(MKCL, mkcl_object buffer, mkcl_object string, mkcl_index start, mkcl_index end)
{
  string = mk_cl_string(env, string);
  while (start < end) {
    mkcl_string_push_extend(env, buffer, mkcl_char(env, string, start));
    start++;
  }
}

static void
push_string(MKCL, mkcl_object buffer, mkcl_object string)
{
  push_substring(env, buffer, string, 0, mkcl_length(env, string));
}

static mkcl_object
canonicalize_directory_destructively(MKCL, mkcl_object directory, bool logical)
{
  /* This function performs the following tasks:
   * 1) It ensures that the list is a well formed directory list.
   * 2) It ensures on non-logical pathnames that all "." strings in the list are removed.
   * 3) It ensures on non-logical pathnames that all ".." strings in the list are replaced by their corresponding keyword.
   */
  /* INV: directory is always a list */
  mkcl_object ptr;
  int i;

  if (mkcl_Null(directory) || directory == MK_KEY_unspecific)
    return directory;
  if (!MKCL_LISTP(directory))
    return MK_KEY_error;
  if (MKCL_CONS_CAR(directory) != MK_KEY_absolute &&
      MKCL_CONS_CAR(directory) != MK_KEY_relative)
    return MK_KEY_error;
 BEGIN:
  for (i=0, ptr=directory; MKCL_CONSP(ptr); ptr = MKCL_CONS_CDR(ptr), i++) {
    mkcl_object item = MKCL_CONS_CAR(ptr);
    if (item == MK_KEY_back) {
      if (i == 0)
	return MK_KEY_error;
      item = mkcl_nth(env, i-1, directory);
      if (item == MK_KEY_absolute || item == MK_KEY_wild_inferiors)
	return MK_KEY_error;
      if (i >= 2)
	MKCL_RPLACD(mkcl_nthcdr(env, i-2, directory), MKCL_CONS_CDR(ptr));
    } else if (item == MK_KEY_up) {
      if (i == 0)
	return MK_KEY_error;
      item = mkcl_nth(env, i-1, directory);
      if (item == MK_KEY_absolute || item == MK_KEY_wild_inferiors)
	return MK_KEY_error;
    } else if (item == MK_KEY_relative || item == MK_KEY_absolute) {
      if (i > 0)
	return MK_KEY_error;
    } else if (mkcl_stringp(env, item)) {
      mkcl_word l = mkcl_string_length(env, item);
      if (logical)
        {
          MKCL_RPLACA(ptr, mk_cl_string_upcase(env, 1, item)); /* logical pathnames are uppercase only. */
          continue; /* In logical pathnames, "." and ".." have no special meaning and are valid directory names. */
        }
      if (l && mkcl_char(env, item, 0) == '.') {
	if (l == 1) {
	  /* Single dot */
	  if (i == 0)
	    return MK_KEY_error;
	  MKCL_RPLACD(mkcl_nthcdr(env, i-1, directory), MKCL_CONS_CDR(ptr));
	  goto BEGIN;
	} else if (l == 2 && mkcl_char(env, item,1) == '.') {
	  MKCL_RPLACA(ptr, MK_KEY_back); /* Why :back and not rather :up here? JCB */
	  goto BEGIN;
	}
      }
    } else if (item != MK_KEY_wild && item != MK_KEY_wild_inferiors) {
      return MK_KEY_error;
    }
  }
  return directory;
}

static mkcl_object _make_pathname(MKCL, bool logical, mkcl_object host, mkcl_object device, mkcl_object directory,
				  mkcl_object name, mkcl_object type, mkcl_object version)
{
  mkcl_object p = mkcl_alloc_raw_pathname(env);

  p->pathname.logical   = logical;
  p->pathname.complete  = FALSE;
  p->pathname.host      = host;
  p->pathname.device    = device;
  p->pathname.directory = directory;
  p->pathname.name      = name;
  p->pathname.type      = type;
  p->pathname.version   = version;
  p->pathname.namestring = mk_cl_Cnil;
  return p;
}

mkcl_object
mkcl_make_pathname(MKCL, mkcl_object host, mkcl_object device, mkcl_object directory,
		   mkcl_object name, mkcl_object type, mkcl_object version)
{
  mkcl_object bad_value = mk_cl_Cnil, bad_component = mk_cl_Cnil;
  bool logical = FALSE;
  bool a_component_is_nil = FALSE;

  if (mkcl_stringp(env, host))
    logical = mkcl_logical_hostname_p(env, host);
  else if (host == mk_cl_Cnil)
    {
      logical = FALSE;
      a_component_is_nil = TRUE;
    }
  else
    {
      bad_value = host;
      bad_component = MK_KEY_host;
      goto _MKCL_ERROR;
    }

  if (device == MKCL_OBJNULL)
    device = (logical ? MK_KEY_unspecific : mk_cl_Cnil);

  if (logical)
    device = MK_KEY_unspecific;
  else if (mkcl_Null(device))
    a_component_is_nil = TRUE;
  else if (!(mkcl_stringp(env, device) || (device == MK_KEY_unspecific) || (device == MK_KEY_wild)))
    {
      bad_value = device;
      bad_component = MK_KEY_device;
      goto _MKCL_ERROR;
    }

  if (mkcl_Null(name))
    a_component_is_nil = TRUE;
  else if ((logical && name == MK_KEY_unspecific)
	   || (!mkcl_stringp(env, name) && name != MK_KEY_wild && name != MK_KEY_unspecific )) {
    bad_value = name;
    bad_component = MK_KEY_name;
    goto _MKCL_ERROR;
  }
  if (logical && mkcl_stringp(env, name)) name = mk_cl_string_upcase(env, 1, name);

  if (mkcl_Null(type))
    a_component_is_nil = TRUE;
  else if ((logical && type == MK_KEY_unspecific)
	   || (!mkcl_stringp(env, type) && type != MK_KEY_wild && type != MK_KEY_unspecific)) {
    bad_value = type;
    bad_component = MK_KEY_type;
    goto _MKCL_ERROR;
  }
  if (logical && mkcl_stringp(env, type)) type = mk_cl_string_upcase(env, 1, type);

  if (mkcl_Null(version))
    a_component_is_nil = TRUE;
  else if ((logical && version == MK_KEY_unspecific)
	   || (version != MK_KEY_newest && version != MK_KEY_unspecific && version != MK_KEY_wild
	       && !(MKCL_FIXNUMP(version) && mkcl_plusp(env, version))))
    {
      bad_value = version;
      bad_component = MK_KEY_version;
      goto _MKCL_ERROR;
    }

  switch (mkcl_type_of(directory)) {
  case mkcl_t_string:
  case mkcl_t_base_string:
    directory = mkcl_cons(env, MK_KEY_absolute, mkcl_list1(env, directory));
    break;
  case mkcl_t_symbol:
    if (mkcl_Null(directory)) {
      a_component_is_nil = TRUE;
      break;
    }
    if (!logical && directory == MK_KEY_unspecific)
      break;
    if (directory == MK_KEY_wild) {
      directory = mkcl_cons(env, MK_KEY_absolute, mkcl_list1(env, MK_KEY_wild_inferiors));
      break;
    }
    bad_value = directory;
    bad_component = MK_KEY_directory;
    goto _MKCL_ERROR;
  case mkcl_t_cons:
    /* validity of list content checked during canonicalization just here after. */
    /* Since we may destructively canonicalize the directory list, we copy it first. JCB */
    directory = mk_cl_copy_list(env, directory);
    break;
  case mkcl_t_null:
    a_component_is_nil = TRUE;
    break;
  default:
    bad_value = directory;
    bad_component = MK_KEY_directory;
    goto _MKCL_ERROR;
  }

  {
    mkcl_object p = _make_pathname(env, logical, host, device, directory, name, type, version);
    p->pathname.logical = logical;
    if (canonicalize_directory_destructively(env, directory, logical) == MK_KEY_error) {
      mk_cl_error(env, 3, MK_CL_file_error, MK_KEY_pathname, p);
    }

    if (a_component_is_nil)
      p->pathname.complete = FALSE;
    else if (MKCL_CONSP(p->pathname.directory))
      p->pathname.complete = (MKCL_CONS_CAR(p->pathname.directory) == MK_KEY_absolute);
    else
      p->pathname.complete = TRUE;
    return(p);
  }
 _MKCL_ERROR:
  if (logical)
    mkcl_FEerror(env, "~s is not a valid logical pathname pathname-~a component, logical host = ~S",
		 3, bad_value, bad_component, host);
  else
    mkcl_FEerror(env, "~s is not a valid pathname-~a component", 2, bad_value, bad_component);
  return(mk_cl_Cnil);
}

static mkcl_object
tilde_expand(MKCL, mkcl_object pathname)
{
  /*
   * If the pathname is a physical one, without hostname, without device
   * and the first element is either a tilde '~' or '~' followed by
   * a user name, we merge the user homedir pathname with this one.
   */
  mkcl_object directory, head;
  if (pathname->pathname.logical 
      || ((pathname->pathname.host != mk_cl_Cnil) && !mkcl_string_E(env, pathname->pathname.host, mkcl_core.localhost_string))
      || ((pathname->pathname.device != mk_cl_Cnil) && (pathname->pathname.device != MK_KEY_unspecific))
      )
    return pathname;

  directory = pathname->pathname.directory;
  if (!MKCL_CONSP(directory) 
      || MKCL_CONS_CAR(directory) != MK_KEY_relative
      || MKCL_CONS_CDR(directory) == mk_cl_Cnil) {
    return pathname;
  }
  head = MKCL_CADR(directory);
  if (mkcl_stringp(env, head)
      && mkcl_length(env, head) > 0
      && mkcl_char(env, head, 0) == '~') {
    mkcl_object homedir = mkcl_homedir_pathname(env, head);
    if (!mkcl_Null(homedir))
      {	/* Remove the tilde component */
	MKCL_RPLACD(directory, MKCL_CDDR(directory));
	pathname = mkcl_merge_pathnames(env, pathname, homedir, MK_KEY_newest);
      }
  }
  return pathname;
}

static mkcl_object
make_one(MKCL, mkcl_object s, mkcl_index start, mkcl_index end)
{
  return mk_cl_subseq(env, 3, s, MKCL_MAKE_FIXNUM(start), MKCL_MAKE_FIXNUM(end));
}

/*
 * Translates a string into the host's preferred case.
 * See CLHS 19.2.2.1.2.2 Common Case in Pathname Components.
 */

static mkcl_object
common_transcase(MKCL, mkcl_object str)
{
  if (!MKCL_STRINGP(str)) {
    /* Pathnames may contain some other objects, such as symbols,
     * numbers, etc, which need not be translated */
    return str;
  }
  else { /* We use UN*X conventions, so lower case is the customary case. */
    enum mkcl_string_case string_case = mkcl_string_case(str);

    if (string_case == mkcl_uppercase_string) 
      return mk_cl_string_downcase(env, 1, str);
    else if (string_case == mkcl_lowercase_string)
      return mk_cl_string_upcase(env, 1, str); /* opposite customary case */
    else
      return str; /* Mixed case goes unchanged */
  }
}

static mkcl_object
common_transcase_list(MKCL, mkcl_object list)
{
  /* If the argument is really a list, translate all strings in it and
   * return this new list, else assume it is a string and translate it.
   */
  if (!MKCL_CONSP(list)) {
    return common_transcase(env, list);
  } else {
    mkcl_object l = list = mk_cl_copy_list(env, list);

    for (; MKCL_CONSP(l); l = MKCL_CONS_CDR(l)) {
      /* It is safe to pass anything to common_transcase,
       * because it will only transform strings, leaving other
       * object (such as symbols) unchanged.*/
      mkcl_object name = MKCL_CONS_CAR(l);
      name = common_transcase(env, name);
      MKCL_RPLACA(l, name);
    }
    return list;
  }
}


#define WORD_INCLUDE_DELIM 1
#define WORD_ALLOW_ASTERISK  2
#define WORD_EMPTY_IS_NIL 4
#define WORD_LOGICAL 8
#define WORD_SEARCH_LAST_DOT 16
#define WORD_ALLOW_LEADING_DOT 32
#define WORD_DISALLOW_SLASH 64
#define WORD_DISALLOW_SEMICOLON 128

typedef bool (*delim_fn)(mkcl_character);
static bool is_colon(mkcl_character c) { return c == ':'; }
static bool is_slash(mkcl_character c) { return MKCL_IS_DIR_SEPARATOR(c); }
static bool is_semicolon(mkcl_character c) { return c == ';'; }
static bool is_dot(mkcl_character c) { return c == '.'; }
static bool is_null(mkcl_character c) { return c == '\0'; }


/*
 * Parses a word from string `S' until either:
 *	1) character `DELIM' is found
 *	2) end of string is reached
 *	3) a non valid character is found
 * Output is either
 *	1) :error in case (3) above
 *	2) :wild, :wild-inferiors, :up
 *	3) "" or mk_cl_Cnil when word has no elements
 *	5) A non empty string
 */
static mkcl_object
parse_word(MKCL, mkcl_object s, delim_fn delim, int flags,
           mkcl_index start, mkcl_index end, mkcl_index *end_of_word,
           enum mkcl_namestring_specificity specificity)
{
  mkcl_index i, j, last_delim = end;
  bool wild_inferiors = FALSE;

  i = j = start;
  for (; i < end; i++) {
    bool valid_char;
    mkcl_character c = mkcl_char(env, s, i);
    if (delim(c)) {
      if ((i == start) && (flags & WORD_ALLOW_LEADING_DOT)) {
	/* Leading dot is included */
	continue;
      }
      last_delim = i;
      if (!(flags & WORD_SEARCH_LAST_DOT)) {
	break;
      }
    }
    if (c == '*') {
      if (!(flags & WORD_ALLOW_ASTERISK) && (specificity == mkcl_may_be_wild_namestring))
	valid_char = FALSE; /* Asterisks not allowed in this word */
      else {
	wild_inferiors = (i > start && mkcl_char(env, s, i-1) == '*');
	valid_char = TRUE; /* single "*" */
      }
    } else if (is_semicolon(c) && (flags & (WORD_DISALLOW_SEMICOLON | WORD_LOGICAL))) {
      valid_char = FALSE;
    } else if (is_slash(c) && (flags & (WORD_DISALLOW_SLASH | WORD_LOGICAL))) {
      valid_char = FALSE;
    } else {
      if (flags & WORD_LOGICAL)
	{
          if (c == '-')
            valid_char = TRUE;
	  else if (mkcl_alphanumericp(c))
	    valid_char = TRUE;
	  else
	    valid_char = FALSE;
	}
      else
	valid_char = c != 0; /* What is wrong with character code 0? JCB */
    }
    if (!valid_char) {
      *end_of_word = start;
      return MK_KEY_error;
    }
  }
  if (i > last_delim) {
    /* Go back to the position of the last delimiter */
    i = last_delim;
  }
  if (i < end) {
    *end_of_word = i+1;
  } else {
    *end_of_word = end;
    /* We have reached the end of the string without finding
       the proper delimiter */
    if (flags & WORD_INCLUDE_DELIM) {
      *end_of_word = start;
      return mk_cl_Cnil;
    }
  }
  switch(i-j)
    {
    case 0:
      if (flags & WORD_EMPTY_IS_NIL)
        return mk_cl_Cnil;
      return mkcl_core.empty_base_string;
    case 1:
      if ((mkcl_char(env, s,j) == '*') && (specificity == mkcl_may_be_wild_namestring))
        return MK_KEY_wild;
      break;
    case 2:
      {
        mkcl_character c0 = mkcl_char(env, s,j);
        mkcl_character c1 = mkcl_char(env, s,j+1);

        if ((c0 == '*' && c1 == '*') && (specificity == mkcl_may_be_wild_namestring))
          return MK_KEY_wild_inferiors;
        if (!(flags & (WORD_LOGICAL | WORD_ALLOW_LEADING_DOT)) && c0 == '.' && c1 == '.')
          return MK_KEY_up;
      }
      break;
    default:
      if (wild_inferiors && (specificity == mkcl_may_be_wild_namestring)) /* '**' surrounded by other characters */
        return MK_KEY_error;
    }
  {
    mkcl_object word = make_one(env, s, j, i);

    if (flags & WORD_LOGICAL)
      return mk_cl_nstring_upcase(env, 1, word);
    else
      return word;
  }
}

/*
 * Parses a logical or physical directory tree. Output is always a
 * list of valid directory components, which may be just NIL.
 *
 * INV: When parsing of directory components has failed, a valid list
 * is also returned, and it will be later in the parsing of
 * pathname-name or pathname-type when the same error is detected.
 */

static mkcl_object
parse_directories(MKCL, mkcl_object s, int flags,
                  mkcl_index start, mkcl_index end, mkcl_index *end_of_dir,
                  enum mkcl_namestring_specificity specificity)
{
  mkcl_index i, j;
  mkcl_object path = mk_cl_Cnil;
  delim_fn delim = (flags & WORD_LOGICAL) ? is_semicolon : is_slash;

  flags |= WORD_INCLUDE_DELIM | WORD_ALLOW_ASTERISK;
  *end_of_dir = start;
  for (i = j = start; i < end; j = i) {
    mkcl_object part = parse_word(env, s, delim, flags, j, end, &i, specificity);
    if (part == MK_KEY_error || part == mk_cl_Cnil)
      break;
    if (part == mkcl_core.empty_base_string) {  /* "/", ";" */
      if (j != start) {
	if (flags & WORD_LOGICAL)
	  return MK_KEY_error;
	*end_of_dir = i;
	continue;
      }
      part = (flags & WORD_LOGICAL) ? MK_KEY_relative : MK_KEY_absolute;
    }
    *end_of_dir = i;
    path = mkcl_cons(env, part, path);
  }
  return mk_cl_nreverse(env, path);
}

bool
mkcl_logical_hostname_p(MKCL, mkcl_object host)
{
  if ((host == mkcl_core.localhost_string) || !mkcl_stringp(env, host) || mkcl_string_E(env, host, mkcl_core.localhost_string))
    return FALSE;
  return !mkcl_Null(mk_cl_assoc(env, 4, host, mkcl_core.pathname_translations, MK_KEY_test, MK_CL_string_equal));
}

/*
 * Parses a lisp namestring until the whole substring is parsed or an
 * error is found. It returns a valid pathname or NIL, plus the place
 * where parsing ended in *END_OF_PARSING.
 *
 * The rules are as follows:
 *
 * 1) If a hostname is supplied it determines whether the namestring
 *    will be parsed as logical or as physical.
 *
 * 2) If no hostname is supplied, first it tries parsing using logical
 *    pathname rules and, if no logical hostname is found, then it
 *    tries the physical pathname format.
 *
 * 3) Logical pathname syntax:
 *	[logical-hostname:][;][logical-directory-component;][pathname-name][.pathname-type]
 *
#if 0
 * 4) Physical pathname syntax:
 *	[device:][[//hostname]/][directory-component/]*[pathname-name][.pathname-type]
#else
 * 4a) Physical pathname syntax on Microsoft Windows:
 *	[device:[/]|//hostname/share/|/][directory-component/]*[pathname-name][.pathname-type]
 *
 * 4b) Physical pathname syntax on Unix (POSIX):
 *	[hostname:][/][directory-component/]*[pathname-name][.pathname-type]
#endif
 *
 *	logical-hostname, device, hostname = word
 *	logical-directory-component = word | wildcard-word
 *	directory-component = word | wildcard-word | '..' | '.'
 *	pathname-name, pathname-type = word | wildcard-word | ""
 *
 */
mkcl_object
mkcl_parse_namestring(MKCL, mkcl_object s, mkcl_index start, mkcl_index end, mkcl_index *ep,
                      mkcl_object default_host, enum mkcl_namestring_specificity specificity)
{
  mkcl_object host = mk_cl_Cnil;
  mkcl_object device = mk_cl_Cnil;
  mkcl_object dir = mk_cl_Cnil;
  mkcl_object name = mk_cl_Cnil;
  mkcl_object type = mk_cl_Cnil;
  mkcl_object version = mk_cl_Cnil;
  bool logical = FALSE;

  if ((start == end) || (mkcl_string_length(env, s) == 0)) {
    goto make_it;
  }

  /* We first try parsing as logical-pathname. In case of
   * failure, physical-pathname parsing is performed only when
   * there is no supplied *logical* host name. All other failures
   * result in mk_cl_Cnil as output.
   */
  host = parse_word(env, s, is_colon,
		    WORD_LOGICAL | WORD_INCLUDE_DELIM | WORD_DISALLOW_SEMICOLON,
		    start, end, ep, specificity);
  if (default_host != mk_cl_Cnil) {
    if (host == mk_cl_Cnil || host == MK_KEY_error)
      host = default_host;
  }

  if ((host != MK_KEY_error) && mkcl_logical_hostname_p(env, host))
    {
      /*
       * Logical pathname format:
       *	[logical-hostname :][;][logical-directory-component ;][pathname-name][. pathname-type [. version]]
       */
      logical = TRUE;
      device = MK_KEY_unspecific;
      dir = parse_directories(env, s, WORD_LOGICAL, *ep, end, ep, specificity);
      if (dir == MK_KEY_error)
	return mk_cl_Cnil;
      if (MKCL_CONSP(dir)) {
	if (MKCL_CONS_CAR(dir) != MK_KEY_relative &&
	    MKCL_CONS_CAR(dir) != MK_KEY_absolute)
	  dir = MKCL_CONS(env, MK_KEY_absolute, dir);
	dir = canonicalize_directory_destructively(env, dir, TRUE);
      } else {
	dir = MKCL_CONS(env, MK_KEY_absolute, dir);
      }
      if (dir == MK_KEY_error)
	return mk_cl_Cnil;
      name = parse_word(env, s, is_dot,
			WORD_LOGICAL | WORD_ALLOW_ASTERISK | WORD_EMPTY_IS_NIL,
			*ep, end, ep, specificity);
      if (name == MK_KEY_error)
	return mk_cl_Cnil;
      type = mk_cl_Cnil;
      version = mk_cl_Cnil;
      if (*ep == start || mkcl_char(env, s, *ep-1) != '.')
	goto make_it;
      type = parse_word(env, s, is_dot,
			WORD_LOGICAL | WORD_ALLOW_ASTERISK | WORD_EMPTY_IS_NIL,
			*ep, end, ep, specificity);
      if (type == MK_KEY_error)
	return mk_cl_Cnil;
      if (*ep == start || mkcl_char(env, s, *ep-1) != '.')
	goto make_it;

      mkcl_object aux;

      aux = parse_word(env, s, is_null,
		       WORD_LOGICAL | WORD_ALLOW_ASTERISK | WORD_EMPTY_IS_NIL,
		       *ep, end, ep, specificity);
      if (aux == MK_KEY_error) {
	return mk_cl_Cnil;
      } else if (mkcl_Null(aux) || aux == MK_KEY_wild) {
	version = aux;
      } else {
	version = mk_cl_parse_integer(env, 3, aux, MK_KEY_junk_allowed, mk_cl_Ct);
	if (mk_cl_integerp(env, version) != mk_cl_Cnil && mkcl_plusp(env, version) &&
	    mkcl_fixnum_to_word(MKCL_VALUES(1)) == mkcl_length(env, aux))
	  ;
	else if (mk_cl_string_equal(env, 2, aux, MK_KEY_newest) != mk_cl_Cnil)
	  version = MK_KEY_newest;
	else
	  return mk_cl_Cnil;
      }
    }
  else
    {
    /* physical: */
      /*
#if 0
       * Physical pathname format:
       *	[[device:[//hostname]]/][directory-component/]*[pathname-name][.pathname-type]
#endif
       *
       * We would be a lot happier with this one on MS-Windows. JCB
       *  [device:[/]|//hostname/|/]{directory/}[.name|{name}+[.type]]
       *
       * And this one on Unix.
       *  [hostname:][/]{directory/}[.name|{name}+[.type]]
       *
       * And give up on the idea of a single format!
       * If the CL ANSI standard renounced that idea
       * long ago as most probably impossible why should
       * we even try? JCB
       */
      logical = FALSE;
#if MKCL_WINDOWS
      if ((start+1 <= end) && is_slash(mkcl_char(env, s, start))) {
	device = mk_cl_Cnil;
	goto maybe_parse_host;
      }
      device = parse_word(env, s, is_colon,
			  WORD_INCLUDE_DELIM | WORD_EMPTY_IS_NIL | WORD_DISALLOW_SLASH,
			  start, end, ep, specificity);
      if (device == MK_KEY_error)
	{
	  device = mk_cl_Cnil;
	  host = mk_cl_Cnil;
	  goto done_device_and_host;
	}
      else if (device == mk_cl_Cnil)
	{
	  start = *ep;
	  host = mk_cl_Cnil;
	  goto done_device_and_host;
	}
      if (!mkcl_stringp(env, device)) {
	return mk_cl_Cnil;
      }
      else if (mkcl_length(env, device) == 0) /* This case (":/foobar") should not be accepted! JCB */
	device = mk_cl_Cnil;

    maybe_parse_host:
      if (!mkcl_Null(device))
	{ /* On MS-Windows host and device are mutually exclusive. */
	  host = mkcl_core.localhost_string;
	}
      else if ((start+2) <= end
	       && is_slash(mkcl_char(env, s, start))
	       && is_slash(mkcl_char(env, s, start+1))) /* Is this a UNC path? */
	{
	  mkcl_index head = start + 2;

	  if ((head + 2) <= end
	      && (mkcl_char(env, s, start) == '\\')
	      && (mkcl_char(env, s, start+1) == '\\')
	      && (mkcl_char(env, s, head) == '?')
	      && (mkcl_char(env, s, head+1) == '\\'))
	    {
	      /* This is an extended path string prefix. */
	      mkcl_character ch;

	      head += 2;
	      if ((head + 4) <= end
		  && (mkcl_char(env, s, head) == 'U')
		  && (mkcl_char(env, s, head+1) == 'N')
		  && (mkcl_char(env, s, head+2) == 'C')
		  && (mkcl_char(env, s, head+3) == '\\'))
		head += 4; /* This is the UNC form of the prefix. */
	      else if ((head + 2) <= end
		       && ((((ch = mkcl_char(env, s, head)) >= 'A')
			    && (ch <= 'Z'))
			   || ((ch >= 'a') && (ch <= 'z')))
		       && is_colon(mkcl_char(env, s, head+1)))
		{ /* This is the local drive form of prefix. */
		  device = mk_cl_make_string(env, 3, MKCL_MAKE_FIXNUM(1), MK_KEY_initial_element, MKCL_CODE_CHAR(ch));
		  host = mkcl_core.localhost_string;
		  *ep = /* start = */ head + 2;
		  goto done_device_and_host;
		}
	    }
	  host = parse_word(env, s, is_slash, WORD_EMPTY_IS_NIL, head, end, ep, specificity);
	  if (host == MK_KEY_error) {
	    host = mk_cl_Cnil;
	  } else if (host != mk_cl_Cnil) {
	    if (!mkcl_stringp(env, host))
	      return mk_cl_Cnil;
	    /* parse share name */
	    device = parse_word(env, s, is_slash, WORD_EMPTY_IS_NIL, *ep, end, ep, specificity);
	    start = *ep - 1;
	    if (is_slash(mkcl_char(env, s, start)))
	      *ep = start;
	  }
	}
      else
	host = mkcl_core.localhost_string;

#elif MKCL_UNIX
      host = mkcl_core.localhost_string;
      device = MK_KEY_unspecific;       /* Files have no effective device on Unix. */
      {
	mkcl_object maybe_host = parse_word(env, s, is_colon,
					    WORD_INCLUDE_DELIM | WORD_EMPTY_IS_NIL | WORD_DISALLOW_SLASH,
					    start, end, ep, specificity);
	if (maybe_host == mk_cl_Cnil)
	  start = *ep;
	else if (maybe_host != MK_KEY_error)
	  host = maybe_host;
      }
#else
# error Incomplete mkcl_parse_namestring().
#endif /* elif MKCL_UNIX */

      
    done_device_and_host:
      dir = parse_directories(env, s, 0, *ep, end, ep, specificity);
      if (MKCL_CONSP(dir)) {
	if (!(MKCL_CONS_CAR(dir) == MK_KEY_relative
	      || MKCL_CONS_CAR(dir) == MK_KEY_absolute))
	  dir = MKCL_CONS(env, MK_KEY_relative, dir);
	dir = canonicalize_directory_destructively(env, dir, FALSE);
      }
      if (dir == MK_KEY_error)
	return mk_cl_Cnil;
      start = *ep;
      name = parse_word(env, s, is_dot,
			WORD_ALLOW_LEADING_DOT | WORD_SEARCH_LAST_DOT |
			WORD_ALLOW_ASTERISK | WORD_EMPTY_IS_NIL,
			start, end, ep, specificity);
      if (name == MK_KEY_error)
	return mk_cl_Cnil;
      if ((*ep - start) <= 1 || mkcl_char(env, s, *ep-1) != '.') {
	type = mk_cl_Cnil;
	if (mkcl_Null(dir) && !mkcl_Null(mk_cl_stringE(env, 2, name, mkcl_core.dot_string)))
	  {
	    dir = mkcl_list1(env, MK_KEY_relative);
	    name = mk_cl_Cnil;
	  }
      } else {
	type = parse_word(env, s, is_null, WORD_ALLOW_ASTERISK, *ep, end, ep, specificity);
	if (type == MK_KEY_error)
	  return mk_cl_Cnil;
	if (!mkcl_Null(mk_cl_stringE(env, 2, type, mkcl_core.empty_base_string))
	    && !mkcl_Null(mk_cl_stringE(env, 2, name, mkcl_core.dot_string)))
	  {
	    if (mkcl_Null(dir))
	      dir = MKCL_CONS(env, MK_KEY_relative, mkcl_list1(env, MK_KEY_up));
	    else
	      dir = mkcl_nconc(env, dir, mkcl_list1(env, MK_KEY_up));
	    type = name = mk_cl_Cnil;
	  }
      }
      version = (name != mk_cl_Cnil || type != mk_cl_Cnil) ? MK_KEY_newest : mk_cl_Cnil;
    }

 make_it:
  if (*ep >= end) *ep = end;

  {
    mkcl_object path = _make_pathname(env, logical, host, device, dir, name, type, version);
    
    path->pathname.complete = (host != mk_cl_Cnil && device != mk_cl_Cnil && dir != mk_cl_Cnil
			       && name != mk_cl_Cnil && type != mk_cl_Cnil && version != mk_cl_Cnil
			       && MKCL_CONSP(dir) && MKCL_CONS_CAR(dir) == MK_KEY_absolute);
    return tilde_expand(env, path);
  }
}

mkcl_object
mk_mkcl_pathname_complete_p(MKCL, mkcl_object pathname)
{
  bool full;

  if (mkcl_type_of(pathname) == mkcl_t_pathname)
    full = pathname->pathname.complete;
  else
    full = FALSE;

  mkcl_return_value((full ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_si_default_pathname_defaults(MKCL)
{
  mkcl_call_stack_check(env);
  /* This routine outputs the value of *default-pathname-defaults*
   * coerced to type PATHNAME. Special care is taken so that we do
   * not enter an infinite loop when using PARSE-NAMESTRING, because
   * this routine might itself try to use the value of this variable. */
  mkcl_object path = mkcl_symbol_value(env, MK_CL_DYNVAR_default_pathname_defaults);
  while (mkcl_type_of(path) != mkcl_t_pathname) {
      mkcl_bds_bind(env, MK_CL_DYNVAR_default_pathname_defaults, mkcl_core.empty_default_pathname_defaults);
      path = mkcl_type_error(env, MK_CL_pathname, "*default-pathname-defaults*", path, MK_CL_pathname);
      mkcl_bds_unwind1(env);
  }
  mkcl_return_value(path);
}

mkcl_object
mk_cl_pathname(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
 L:
  switch (mkcl_type_of(x)) {
  case mkcl_t_string:
  case mkcl_t_base_string:
    { mkcl_return_value(mk_cl_parse_namestring(env, 1, x)); }
  case mkcl_t_pathname:
    { mkcl_return_value(x); }
  case mkcl_t_stream:
    switch ((enum mkcl_smmode)x->stream.mode) {
    case mkcl_smm_input:
    case mkcl_smm_output:
    case mkcl_smm_io:
      x = MKCL_IO_STREAM_FILENAME(x);
      goto L;
    case mkcl_smm_input_file:
    case mkcl_smm_output_file:
    case mkcl_smm_io_file:
    case mkcl_smm_probe:
      x = MKCL_IO_FILE_FILENAME(x);
      goto L;
    case mkcl_smm_synonym:
      x = MKCL_SYNONYM_STREAM_STREAM(env, x);
      goto L;
    default:
      ;/* Fall through to error message */
    }
  default:
    mkcl_FEwrong_type_argument(env, mk_cl_list(env, 4, MK_CL_or, MK_CL_file_stream, MK_CL_string, MK_CL_pathname), x);
  }
  mkcl_return_value(x);
}

mkcl_object
mk_cl_logical_pathname(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  x = mk_cl_pathname(env, x);
  if (!x->pathname.logical) {
    mk_cl_error(env, 9, MK_CL_simple_type_error, MK_KEY_format_control,
		mkcl_make_simple_base_string(env, "~S cannot be coerced to a logical pathname."),
		MK_KEY_format_arguments, mk_cl_list(env, 1, x),
		MK_KEY_expected_type, MK_CL_logical_pathname,
		MK_KEY_datum, x);
  }
  mkcl_return_value(x);
}

mkcl_object mk_cl_wild_pathname_p(MKCL, mkcl_narg narg, mkcl_object pathname, ...)
{
  mkcl_call_stack_check(env);
  {
    bool checked = 0;
    mkcl_object component = mk_cl_Cnil;
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_CL_wild_pathname_p, narg, 1, pathname, &component);

    pathname = mk_cl_pathname(env, pathname);
    if (component == mk_cl_Cnil || component == MK_KEY_host) {
      if (pathname->pathname.host == MK_KEY_wild)
        mkcl_return_value(mk_cl_Ct);
      checked = 1;
    }
    if (component == mk_cl_Cnil || component == MK_KEY_device) {
      if (pathname->pathname.device == MK_KEY_wild)
        mkcl_return_value(mk_cl_Ct);
      checked = 1;
    }
    if (component == mk_cl_Cnil || component == MK_KEY_version) {
      if (pathname->pathname.version == MK_KEY_wild)
        mkcl_return_value(mk_cl_Ct);
      checked = 1;
    }
    if (component == mk_cl_Cnil || component == MK_KEY_name) {
      mkcl_object name = pathname->pathname.name;
      if (name != mk_cl_Cnil
          && (name == MK_KEY_wild || mkcl_pathname_component_string_is_wild_p(env, name)))
        mkcl_return_value(mk_cl_Ct);
      checked = 1;
    }
    if (component == mk_cl_Cnil || component == MK_KEY_type) {
      mkcl_object name = pathname->pathname.type;
      if (name != mk_cl_Cnil
          && (name == MK_KEY_wild || mkcl_pathname_component_string_is_wild_p(env, name)))
        mkcl_return_value(mk_cl_Ct);
      checked = 1;
    }
    if (component == mk_cl_Cnil || component == MK_KEY_directory) {
      mkcl_object list = pathname->pathname.directory;
      checked = 1;
      mkcl_loop_for_on_unsafe(list) {
        mkcl_object name = MKCL_CONS_CAR(list);
        if (name != mk_cl_Cnil
            && (name == MK_KEY_wild || name == MK_KEY_wild_inferiors
                || mkcl_pathname_component_string_is_wild_p(env, name)))
          {
            mkcl_return_value(mk_cl_Ct);
          }
      } mkcl_end_loop_for_on;
    }
    if (checked == 0) {
      mkcl_FEerror(env, "~A is not a valid pathname component", 1, component);
    }
    mkcl_return_value(mk_cl_Cnil);
  }
}


/*
 * mkcl_coerce_to_file_pathname(P) converts P to a physical pathname,
 * for a file which is accesible in our filesystem.
 * INV: Wildcards are allowed.
 * INV: A fresh new copy of the pathname is created.
 * INV: The pathname is absolute. (And why should that be so? JCB)
 */
mkcl_object
mkcl_coerce_to_file_pathname(MKCL, mkcl_object pathname)
{
  mkcl_object orig_pathname = pathname;

  pathname = mkcl_coerce_to_physical_pathname(env, pathname);
  if (pathname == orig_pathname)
    pathname = mkcl_merge_pathnames(env, pathname, mk_si_default_pathname_defaults(env), MK_KEY_newest);
  else
    pathname = mkcl_meld_pathnames(env, pathname, mk_si_default_pathname_defaults(env), MK_KEY_newest);
  return pathname;
}

/*
 * mkcl_coerce_to_physical_pathname(P) converts P to a physical pathname,
 * performing the appropriate transformation if P was a logical pathname.
 */
mkcl_object
mkcl_coerce_to_physical_pathname(MKCL, mkcl_object x)
{
  x = mk_cl_pathname(env, x);
  if (x->pathname.logical)
    return mk_cl_translate_logical_pathname(env, 1, x);
  return x;
}

/*
 * mk_si_coerce_to_filename(P) converts P to a physical pathname and then to
 * a namestring. The output must always be a new simple-string which can
 * be used by the C library.
 * INV: No wildcards are allowed.
 */
mkcl_object
mk_si_coerce_to_filename(MKCL, mkcl_object pathname_orig)
{
  mkcl_object namestring, pathname;

  mkcl_call_stack_check(env);
  /* We always go through the pathname representation and thus
   * mk_cl_namestring() always outputs a fresh new string */ /* And thus conses like mad! JCB */
  pathname = mkcl_coerce_to_file_pathname(env, pathname_orig);
  if (mk_cl_wild_pathname_p(env, 1,pathname) != mk_cl_Cnil)
    mk_cl_error(env, 3, MK_CL_file_error, MK_KEY_pathname, pathname_orig);
  namestring = mk_cl_namestring(env, pathname);
  if (namestring == mk_cl_Cnil) {
    mkcl_FEerror(env, "Pathname ~A does not have a physical namestring", 1, pathname_orig);
  }
  return namestring;
}

#if MKCL_UNIX
#define default_device(host) MK_KEY_unspecific
#else
#define default_device(host) mk_cl_Cnil
#endif

mkcl_object
mkcl_meld_pathnames(MKCL, mkcl_object path, mkcl_object defaults, mkcl_object default_version)
{
  if (path->pathname.complete) return(path); /* We are already complete. */

  /* We do NOT merge a physical pathname with a logical pathname, or vice versa;
     doing so would be a semantic nightmare at best.
     In this we follow the behavior of Allegro CL of Franz.
  */
  if (!path->pathname.logical && defaults->pathname.logical)
    defaults = mk_cl_translate_logical_pathname(env, 1, defaults);
  else if (path->pathname.logical && !defaults->pathname.logical)
    return path;

  struct mkcl_pathname old_path_automatic = path->pathname;
  mkcl_object old_path = (mkcl_object) &old_path_automatic;
  mkcl_object new_path = path;
  bool a_component_is_nil = FALSE;

  new_path->pathname.namestring = mk_cl_Cnil; /* Let's be conservative on the namestring cache. */

  if (mkcl_Null(new_path->pathname.host = old_path->pathname.host))
    {
      new_path->pathname.host = defaults->pathname.host;
      new_path->pathname.logical = defaults->pathname.logical;
    }
  else
    new_path->pathname.logical = old_path->pathname.logical;
  if (mkcl_Null(new_path->pathname.host)) a_component_is_nil = TRUE;

  if (mkcl_Null(old_path->pathname.device))
    if (mkcl_Null(old_path->pathname.host))
      new_path->pathname.device = defaults->pathname.device;
    else if (old_path->pathname.host == defaults->pathname.host)
      new_path->pathname.device = defaults->pathname.device;
    else
      new_path->pathname.device = default_device(old_path->pathname.host);
  else
    new_path->pathname.device = old_path->pathname.device;
  if (mkcl_Null(new_path->pathname.device)) a_component_is_nil = TRUE;

  if (old_path->pathname.directory == MK_KEY_unspecific)
    new_path->pathname.directory = MK_KEY_unspecific;
  if (mkcl_Null(old_path->pathname.directory))
    new_path->pathname.directory = defaults->pathname.directory;
  else if (MKCL_CONSP(old_path->pathname.directory) && MKCL_CONS_CAR(old_path->pathname.directory) == MK_KEY_absolute)
    new_path->pathname.directory = old_path->pathname.directory;
  else if (MKCL_CONSP(defaults->pathname.directory))
    {
      new_path->pathname.directory = mkcl_append(env, defaults->pathname.directory,
						 mkcl_copy_proper_list(env, MKCL_CDR(old_path->pathname.directory)));
      /* The tail of the previous append has to be a fresh list because of the following destructive canonicalization. */
    }
  else
    new_path->pathname.directory = old_path->pathname.directory;
  if (mkcl_Null(new_path->pathname.directory)) a_component_is_nil = TRUE;

  if (mkcl_Null(new_path->pathname.name = old_path->pathname.name))
    new_path->pathname.name = defaults->pathname.name;
  if (mkcl_Null(new_path->pathname.name)) a_component_is_nil = TRUE;

  if (mkcl_Null(new_path->pathname.type = old_path->pathname.type))
    new_path->pathname.type = defaults->pathname.type;
  if (mkcl_Null(new_path->pathname.type)) a_component_is_nil = TRUE;

  new_path->pathname.version = old_path->pathname.version;
  if (mkcl_Null(old_path->pathname.name)) {
    if (mkcl_Null(new_path->pathname.version))
      new_path->pathname.version = defaults->pathname.version;
  }
  if (mkcl_Null(new_path->pathname.version))
    new_path->pathname.version = default_version;
  if (mkcl_Null(new_path->pathname.version)) a_component_is_nil = TRUE;

  /*
    In this implementation, version is not considered for physical pathnames.
  */
  if (MK_KEY_error == canonicalize_directory_destructively(env, new_path->pathname.directory, new_path->pathname.logical))
    mk_cl_error(env, 3, MK_CL_file_error, MK_KEY_pathname, new_path);

  if (a_component_is_nil)
    new_path->pathname.complete = FALSE;
  else if (MKCL_CONSP(new_path->pathname.directory))
    new_path->pathname.complete = (MKCL_CONS_CAR(new_path->pathname.directory) == MK_KEY_absolute);
  else
    new_path->pathname.complete = TRUE;
  
  return new_path;
}

mkcl_object
mkcl_merge_pathnames(MKCL, mkcl_object path, mkcl_object defaults, mkcl_object default_version)
{
  if (path->pathname.complete)
    return(path); /* nothing to be merged. */
  else
    {
      mkcl_object p = mkcl_alloc_raw_pathname(env);

      p->pathname = path->pathname;
      return mkcl_meld_pathnames(env, p, defaults, default_version);
    }
}

static bool is_strictly_absolute_pathname(MKCL, mkcl_object pathname)
{
  mkcl_object path = pathname->pathname.directory;

  if (mkcl_endp(env, path))
    return FALSE;
  else
    {
      mkcl_object dir = mk_cl_car(env, path);

      if (dir == MK_KEY_relative)
	return FALSE;

      path = mk_cl_cdr(env, path);
      mkcl_loop_for_in(env, path) {
	dir = MKCL_CONS_CAR(path);
	if (dir == MK_KEY_up || dir == MK_KEY_wild || dir == MK_KEY_wild_inferiors || dir == MK_KEY_back)
	  return FALSE;
      } mkcl_end_loop_for_in;
      return TRUE;
    }
}

/*
	mkcl_namestring(x, flag) converts a pathname to a namestring.
	if flag is true, then the pathname may be coerced to the requirements
	of the filesystem, removing fields that have no meaning (such as
	version, or type, etc); otherwise, when it is not possible to
	produce a readable representation of the pathname, NIL is returned.
*/
mkcl_object
mkcl_namestring(MKCL, mkcl_object x, int truncate_if_unreadable)
{
  bool logical;
  mkcl_object l, y;
  mkcl_object buffer, host;

  x = mk_cl_pathname(env, x);

  if (!mkcl_Null(x->pathname.namestring))
    return mkcl_copy_string(env, x->pathname.namestring);

  /* INV: Pathnames can only be created by merging or parsing namestrings
   * or using mkcl_make_pathname(). In all of these cases MKCL will complain
   * at creation time if the pathname has wrong components.
   */
  buffer = mkcl_make_string_output_stream(env, 128, TRUE, MK_KEY_default);
  logical = x->pathname.logical;
  host = x->pathname.host;
  if (logical) {
    if ((y = x->pathname.device) != MK_KEY_unspecific && truncate_if_unreadable)
      return mk_cl_Cnil; /* FIXME: This is in fact a breach of invariant.
			    Logical pathnames device cannot have any other
			    value than :unspecific and thus by definition.
			    This should be flagged as an internal error.
			    JCB
			 */
    if (host != mk_cl_Cnil) {
      mk_si_do_write_sequence(env, host, buffer, MKCL_MAKE_FIXNUM(0), mk_cl_Cnil);
      mkcl_write_cstr(env, ":", buffer);
    }
  } else {
#if MKCL_UNIX
    /* Physical "device" is a nonsense on Unix. JCB */
    if (((y = x->pathname.device) != mk_cl_Cnil) && (y != MK_KEY_unspecific)) {
      mk_si_do_write_sequence(env, y, buffer, MKCL_MAKE_FIXNUM(0), mk_cl_Cnil);
      mkcl_write_cstr(env, ":", buffer);
    }
    if (host != mk_cl_Cnil
	&& (host !=  MK_KEY_unspecific)
	&& !mkcl_string_E(env, host, mkcl_core.localhost_string)) {
      mk_si_do_write_sequence(env, host, buffer, MKCL_MAKE_FIXNUM(0), mk_cl_Cnil);
      mkcl_write_cstr(env, ":", buffer);
    }
#elif MKCL_WINDOWS
    if ((host != mk_cl_Cnil)
	&& (host != MK_KEY_unspecific)
	&& !mkcl_string_E(env, host, mkcl_core.localhost_string))
      {
	mkcl_write_char(env, MKCL_DIR_SEPARATOR, buffer);
	mkcl_write_char(env, MKCL_DIR_SEPARATOR, buffer);
	mk_si_do_write_sequence(env, host, buffer, MKCL_MAKE_FIXNUM(0), mk_cl_Cnil);
	mkcl_write_char(env, MKCL_DIR_SEPARATOR, buffer);
	y = x->pathname.device;
	mk_si_do_write_sequence(env, y, buffer, MKCL_MAKE_FIXNUM(0), mk_cl_Cnil);
      }
    else if (((y = x->pathname.device) != mk_cl_Cnil) && (y != MK_KEY_unspecific))
      {
	mk_si_do_write_sequence(env, y, buffer, MKCL_MAKE_FIXNUM(0), mk_cl_Cnil);
	mkcl_write_cstr(env, ":", buffer);
      }
#else
# error Incomplete mkcl_namestring().
#endif
  }
  l = x->pathname.directory;
  if (l == MK_KEY_unspecific || mkcl_endp(env, l))
    goto NO_DIRECTORY;
  y = MKCL_CONS_CAR(l);
  if (y == MK_KEY_relative) {
    if (logical)
      mkcl_write_char(env, ';', buffer);
    else
      {
	mkcl_write_char(env, '.', buffer);
	mkcl_write_char(env, MKCL_DIR_SEPARATOR, buffer);
      }
  } else {
    if (!logical)
      mkcl_write_char(env, MKCL_DIR_SEPARATOR, buffer);
  }
  l = MKCL_CONS_CDR(l);
  mkcl_loop_for_in(env, l) {
    y = MKCL_CONS_CAR(l);
    if (y == MK_KEY_up) {
      mkcl_write_cstr(env, "..", buffer);
    } else if (y == MK_KEY_wild) {
      mkcl_write_cstr(env, "*", buffer);
    } else if (y == MK_KEY_wild_inferiors) {
      mkcl_write_cstr(env, "**", buffer);
    } else if (y != MK_KEY_back) {
      mk_si_do_write_sequence(env, y, buffer, MKCL_MAKE_FIXNUM(0), mk_cl_Cnil);
    } else {
      /* Directory :back has no namestring representation */
      return mk_cl_Cnil; /* This is not true, :back does have a representation
			    at least according to CLTL2 and ANSI-CL!
			    This is the syntactic vs semantic distinguo.
			    Fix me! JCB
			 */
    }
    mkcl_write_char(env, (logical ? ';' : MKCL_DIR_SEPARATOR), buffer);
  } mkcl_end_loop_for_in;
 NO_DIRECTORY:
  y = x->pathname.name;
  if (y != mk_cl_Cnil && y != MK_KEY_unspecific) {
    if (y == MK_KEY_wild) {
      mkcl_write_cstr(env, "*", buffer);
    } else {
      mk_si_do_write_sequence(env, y, buffer, MKCL_MAKE_FIXNUM(0), mk_cl_Cnil);
    }
  }
  y = x->pathname.type;
  if (y != mk_cl_Cnil && y != MK_KEY_unspecific) {
    if (y == MK_KEY_wild) {
      mkcl_write_cstr(env, ".*", buffer);
    } else {
      mkcl_write_cstr(env, ".", buffer);
      mk_si_do_write_sequence(env, y, buffer, MKCL_MAKE_FIXNUM(0), mk_cl_Cnil);
    }
  }
  y = x->pathname.version;
  if (logical) {
    if (y != mk_cl_Cnil) {
      mkcl_write_cstr(env, ".", buffer);
      if (y == MK_KEY_wild) {
	mkcl_write_cstr(env, "*", buffer);
      } else if (y == MK_KEY_newest) {
	mk_si_do_write_sequence(env, mkcl_symbol_name(env, y), buffer, MKCL_MAKE_FIXNUM(0), mk_cl_Cnil);
      } else if (MKCL_FIXNUMP(y)) {
	/* Since the printer is not reentrant,
	 * we cannot use mk_cl_write and friends.
	 */
	mkcl_index n = mkcl_fixnum_to_word(y), i;
	char b[MKCL_WORD_BITS/2];
	for (i = 0; n; i++) {
	  b[i] = n%10 + '0';
	  n = n/10;
	}
	if (i == 0)
	  b[i++] = '0';
	while (i--) {
	  mkcl_write_char(env, b[i], buffer);
	}
      } else {
	return mk_cl_Cnil; /* The version component is not valid for a logical pathname. */
      }
    }
  } else if (!truncate_if_unreadable) {
    /* Namestrings of physical pathnames have restrictions... */
#if !(MKCL_UNIX || MKCL_WINDOWS)
    /*
      Either version matters and we do this or
      it does not matter (as stated in merge-pathnames!)
      and then simply drop all of this business.
      I go with merge-pathnames for sake of coherence. JCB
     */
    if (mkcl_Null(x->pathname.name) && mkcl_Null(x->pathname.type)) {
      /* Directories cannot have a version number */
      if (y != mk_cl_Cnil)
	return mk_cl_Cnil;
    } else if (y != MK_KEY_newest) {
      /* Filenames have an implicit version :newest */
      return mk_cl_Cnil;
    }
#endif
  }
  /* we copy to get a simple-string. */
  /* And why does it have to be a simple string? A fresh string is not good enough? JCB */
  {
    mkcl_object str = mk_cl_get_output_stream_string(env, buffer);

#if MKCL_WINDOWS
    if (!logical && (mkcl_string_length(env, str) >= mkcl_core.path_max) && is_strictly_absolute_pathname(env, x))
      {
	static mkcl_character raw_prefix[] = { '\\', '\\', '?', '\\', 0 };
	static const mkcl_string_object(prefix_obj, raw_prefix);
	static mkcl_character raw_UNC_prefix[] = { '\\', '\\', '?', '\\', 'U', 'N', 'C', 0 };
	static const mkcl_string_object(UNC_prefix_obj, raw_UNC_prefix);
	mkcl_object prefix;
	mkcl_character * str_self = str->string.self; /* scoped for GC's sake. */
	
	if (str_self[0] == '\\')
	  {
	    str->string.fillp--;
	    str->string.dim--;
	    str->string.self = str_self + 1;
	  }
    
	if ((x->pathname.host != mk_cl_Cnil)
	    && (x->pathname.host != MK_KEY_unspecific)
	    && !mkcl_string_E(env, x->pathname.host, mkcl_core.localhost_string))
	  prefix = (mkcl_object) &UNC_prefix_obj;
	else
	  prefix = (mkcl_object) &prefix_obj;
	  
	str = mkcl_concatenate_2_strings(env, prefix, str);
      }
#endif
    x->pathname.namestring = str;
    return mkcl_copy_string(env, str);
  }
}


mkcl_object
mk_cl_namestring(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  mkcl_return_value(mkcl_namestring(env, x, TRUE));
}

mkcl_object mk_cl_parse_namestring(MKCL, const mkcl_narg narg, mkcl_object thing, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object output = mk_cl_Cnil;
    mkcl_index s, e, ee;

    mkcl_object host = mk_cl_Cnil;
    mkcl_object defaults = mk_cl_Cnil;
    mkcl_object start = MKCL_MAKE_FIXNUM(0);
    mkcl_object end = mk_cl_Cnil;
    mkcl_object junk_allowed = mk_cl_Cnil;
    mkcl_check_minimal_arg_count(env, MK_CL_parse_namestring, narg, 1);
    if (narg > 1) {
      mkcl_va_list ARGS;
      mkcl_va_start(env, ARGS, thing, narg, 1);
      host = mkcl_va_arg(ARGS);
      if (narg > 2)
        defaults = mkcl_va_arg(ARGS);
      else
        defaults = mk_si_default_pathname_defaults(env);
      if (narg > 3)
        mkcl_receive_3_keyword_arguments(env, MK_CL_parse_namestring, ARGS, MK_KEY_start, &start, MK_KEY_end, &end, MK_KEY_junk_allowed, &junk_allowed);
      mkcl_va_end(ARGS);
    }
  
    if (host != mk_cl_Cnil) {
      host = mk_cl_string(env, host);
    }
    if (!mkcl_stringp(env, thing)) {
      output = mk_cl_pathname(env, thing);
    } else {
      mkcl_object default_host = host;
      if (default_host == mk_cl_Cnil && defaults != mk_cl_Cnil) {
        if (!MKCL_PATHNAMEP(defaults))
          defaults = mk_cl_pathname(env, defaults);
        default_host = defaults->pathname.host;
      }
      mkcl_get_string_start_end(env, thing, start, end, &s, &e);
      output = mkcl_parse_namestring(env, thing, s, e, &ee, default_host, mkcl_may_be_wild_namestring);
      start = MKCL_MAKE_FIXNUM(ee);
      if (output == mk_cl_Cnil || ee != e) {
        if (mkcl_Null(junk_allowed)) {
          mkcl_FEparse_error(env, "Cannot parse the namestring ~S~%"
                             "from ~S to ~S.", mk_cl_Cnil,
                             3, thing, start, end);
        }
        goto OUTPUT;
      }
    }
    if (host != mk_cl_Cnil && !mkcl_equal(env, output->pathname.host, host)) {
      mkcl_FEerror(env, "The pathname ~S does not contain the required host ~S.", 2, thing, host);
    }
  OUTPUT:
    mkcl_return_2_values(output, start);
  }
}

mkcl_object mk_cl_merge_pathnames(MKCL, mkcl_narg narg, mkcl_object path, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object defaults = MKCL_OBJNULL;
    mkcl_object default_version = MK_KEY_newest;
    MKCL_RECEIVE_2_OPTIONAL_ARGUMENTS(env, MK_CL_merge_pathnames, narg, 1, path, &defaults, &default_version);

    mkcl_object default_host;

    if (defaults == MKCL_OBJNULL)
      defaults = mk_si_default_pathname_defaults(env);
    else
      defaults = mk_cl_pathname(env, defaults);
    default_host = defaults->pathname.host;

    if (default_version != MK_KEY_newest && default_version != mk_cl_Cnil
        && default_version != MK_KEY_unspecific && default_version != MK_KEY_wild
        && !(MKCL_FIXNUMP(default_version) && mkcl_plusp(env, default_version)))
      mkcl_FEerror(env, "In merge-pathnames: ~s is not a valid default-version value", 1, default_version);

    if (mkcl_stringp(env, path) && defaults->pathname.logical)
      {
        mkcl_index s = 0, e = mkcl_string_length(env, path), ep = 0;
        mkcl_object new_path = mkcl_parse_namestring(env, path, s, e, &ep, defaults->pathname.host, mkcl_may_be_wild_namestring);
      
        if (mkcl_Null(new_path) || ep != e)
          mkcl_FEparse_error(env, "Cannot parse the namestring ~S~%from ~S to ~S.",
                             mk_cl_Cnil, 3, path, MKCL_MAKE_FIXNUM(ep), MKCL_MAKE_FIXNUM(e));
        else
          path = new_path;
      }
    else
      path = mk_cl_pathname(env, path); /* Not good enough for logical pathname handling! */

    mkcl_return_value(mkcl_merge_pathnames(env, path, defaults, default_version));
  }
}

mkcl_object mk_mkcl_meld_pathnames(MKCL, mkcl_narg narg, mkcl_object path, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object defaults = MKCL_OBJNULL;
    mkcl_object default_version = MK_KEY_newest;
    MKCL_RECEIVE_2_OPTIONAL_ARGUMENTS(env, MK_MKCL_meld_pathnames, narg, 1, path, &defaults, &default_version);

  mkcl_object default_host;

  if (defaults == MKCL_OBJNULL)
    defaults = mk_si_default_pathname_defaults(env);
  else
    defaults = mk_cl_pathname(env, defaults);
  default_host = defaults->pathname.host;

  if (default_version != MK_KEY_newest && default_version != mk_cl_Cnil
      && default_version != MK_KEY_unspecific && default_version != MK_KEY_wild
      && !(MKCL_FIXNUMP(default_version) && mkcl_plusp(env, default_version)))
    mkcl_FEerror(env, "In meld-pathnames: ~s is not a valid default-version value", 1, default_version);

  if (mkcl_stringp(env, path) && defaults->pathname.logical)
    {
      mkcl_index s = 0, e = mkcl_string_length(env, path), ep = 0;
      mkcl_object new_path = mkcl_parse_namestring(env, path, s, e, &ep, defaults->pathname.host, mkcl_may_be_wild_namestring);
      
      if (mkcl_Null(new_path) || ep != e)
	mkcl_FEparse_error(env, "Cannot parse the namestring ~S~%from ~S to ~S.",
			   mk_cl_Cnil, 3, path, MKCL_MAKE_FIXNUM(ep), MKCL_MAKE_FIXNUM(e));
      else
	path = new_path;
    }
  else
    path = mk_cl_pathname(env, path); /* Not good enough for logical pathname handling! */

  mkcl_return_value(mkcl_meld_pathnames(env, path, defaults, default_version));
  }
}

mkcl_object mk_cl_make_pathname(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {

    mkcl_object x = mk_cl_Cnil;
    
    mkcl_object host = MKCL_OBJNULL;
    mkcl_object device = MKCL_OBJNULL;
    mkcl_object directory = MKCL_OBJNULL;
    mkcl_object name = MKCL_OBJNULL;
    mkcl_object type = MKCL_OBJNULL;
    mkcl_object version = MKCL_OBJNULL;
    mkcl_object scase = MK_KEY_local;
    mkcl_object defaults = mk_cl_Cnil;
    struct mkcl_key_param_spec key_params[] =
      {
       { MK_KEY_host, &host, false },
       { MK_KEY_device, &device, false },
       { MK_KEY_directory, &directory, false },
       { MK_KEY_name, &name, false },
       { MK_KEY_type, &type, false },
       { MK_KEY_version, &version, false },
       { MK_KEY_case, &scase, false },
       { MK_KEY_defaults, &defaults, false }
      };
    MKCL_RECEIVE_N_KEYWORD_ARGUMENTS(env, MK_CL_make_pathname, narg, 0, narg, key_params);

    bool logical = FALSE;
    bool verbatim = MKCL_EQ(scase, MK_KEY_common) ? FALSE : TRUE;

    if (!mkcl_Null(defaults)) {
      defaults = mk_cl_pathname(env, defaults);
    }

    mkcl_object new_host = ((host != MKCL_OBJNULL)
                            ? (((logical = mkcl_logical_hostname_p(env, host)), (verbatim |= logical))
                               ? host : common_transcase(env, host))
                            : (mkcl_Null(defaults)
                               ? mk_si_default_pathname_defaults(env)->pathname.host : defaults->pathname.host));

    x = mkcl_make_pathname(env,
                           new_host,
                           ((device != MKCL_OBJNULL)
                            ? (verbatim ? device : common_transcase(env, device))
                            : (mkcl_Null(defaults) ? device : defaults->pathname.device)),
                           ((directory != MKCL_OBJNULL)
                            ? (verbatim ? directory : common_transcase_list(env, directory))
                            : (mkcl_Null(defaults) ? mk_cl_Cnil : defaults->pathname.directory)),
                           ((name != MKCL_OBJNULL)
                            ? (verbatim ? name : common_transcase(env, name))
                            : (mkcl_Null(defaults) ? mk_cl_Cnil : defaults->pathname.name)),
                           ((type != MKCL_OBJNULL)
                            ? (verbatim ? type : common_transcase(env, type))
                            : (mkcl_Null(defaults) ? mk_cl_Cnil : defaults->pathname.type)),
                           ((version != MKCL_OBJNULL) 
                            ? version : (mkcl_Null(defaults) ? mk_cl_Cnil : defaults->pathname.version)));
    mkcl_return_value(x);
  }
 }

mkcl_object
mk_cl_pathnamep(MKCL, mkcl_object pname)
{
  mkcl_call_stack_check(env);
  mkcl_return_value(((mkcl_type_of(pname) == mkcl_t_pathname) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_mkcl_logical_pathname_p(MKCL, mkcl_object pname)
{
  mkcl_return_value(((mkcl_type_of(pname) == mkcl_t_pathname && pname->pathname.logical) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object mk_cl_pathname_host(MKCL, mkcl_narg narg, mkcl_object pname, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object scase = MK_KEY_local;
  
    MKCL_RECEIVE_1_KEYWORD_ARGUMENT(env, MK_CL_pathname_host, narg, 1, pname, MK_KEY_case, &scase);
  
    if (!MKCL_PATHNAMEP(pname))
      pname = mk_cl_pathname(env, pname);
    mkcl_return_value(((MKCL_EQ(scase, MK_KEY_common) && !pname->pathname.logical)
                       ? common_transcase(env, pname->pathname.host)
                       : pname->pathname.host));
  }
}

mkcl_object mk_cl_pathname_device(MKCL, mkcl_narg narg, mkcl_object pname, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object scase = MK_KEY_local;
  
    MKCL_RECEIVE_1_KEYWORD_ARGUMENT(env, MK_CL_pathname_device, narg, 1, pname, MK_KEY_case, &scase);

    if (!MKCL_PATHNAMEP(pname))
      pname = mk_cl_pathname(env, pname);
    mkcl_return_value(((MKCL_EQ(scase, MK_KEY_common) && !pname->pathname.logical)
                       ? common_transcase(env, pname->pathname.device)
                       : pname->pathname.device));
  }
}

mkcl_object mk_cl_pathname_directory(MKCL, mkcl_narg narg, mkcl_object pname, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object scase = MK_KEY_local;
  
    MKCL_RECEIVE_1_KEYWORD_ARGUMENT(env, MK_CL_pathname_directory, narg, 1, pname, MK_KEY_case, &scase);
    
    if (!MKCL_PATHNAMEP(pname))
      pname = mk_cl_pathname(env, pname);
    mkcl_return_value(((MKCL_EQ(scase, MK_KEY_common) && !pname->pathname.logical)
                       ? common_transcase_list(env, pname->pathname.directory)
                       : pname->pathname.directory));
  }
}

mkcl_object mk_cl_pathname_name(MKCL, mkcl_narg narg, mkcl_object pname, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object scase = MK_KEY_local;
  
    MKCL_RECEIVE_1_KEYWORD_ARGUMENT(env, MK_CL_pathname_name, narg, 1, pname, MK_KEY_case, &scase);
    
  if (!MKCL_PATHNAMEP(pname))
    pname = mk_cl_pathname(env, pname);
  mkcl_return_value(((MKCL_EQ(scase, MK_KEY_common) && !pname->pathname.logical)
            ? common_transcase(env, pname->pathname.name)
            : pname->pathname.name));
  }
}

mkcl_object mk_cl_pathname_type(MKCL, mkcl_narg narg, mkcl_object pname, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object scase = MK_KEY_local;
  
    MKCL_RECEIVE_1_KEYWORD_ARGUMENT(env, MK_CL_pathname_type, narg, 1, pname, MK_KEY_case, &scase);

    if (!MKCL_PATHNAMEP(pname))
      pname = mk_cl_pathname(env, pname);
    mkcl_return_value(((MKCL_EQ(scase, MK_KEY_common) && !pname->pathname.logical)
                       ? common_transcase(env, pname->pathname.type)
                       : pname->pathname.type));
  }
}

mkcl_object
mk_cl_pathname_version(MKCL, mkcl_object pname)
{
  mkcl_call_stack_check(env);
  if (!MKCL_PATHNAMEP(pname))
    pname = mk_cl_pathname(env, pname);
  mkcl_return_value(pname->pathname.version);
}

mkcl_object
mk_cl_file_namestring(MKCL, mkcl_object pname)
{
  mkcl_call_stack_check(env);
  pname = mk_cl_pathname(env, pname);
  mkcl_return_value(mkcl_namestring(env,
			   _make_pathname(env, pname->pathname.logical,
					  mk_cl_Cnil,
					  pname->pathname.logical ? MK_KEY_unspecific : mk_cl_Cnil,
					  mk_cl_Cnil,
					  pname->pathname.name,
					  pname->pathname.type,
					  pname->pathname.version),
			   TRUE));
}

mkcl_object
mk_cl_directory_namestring(MKCL, mkcl_object pname)
{
  mkcl_call_stack_check(env);
  pname = mk_cl_pathname(env, pname);
  mkcl_return_value(mkcl_namestring(env,
			   _make_pathname(env, pname->pathname.logical,
					  mk_cl_Cnil,
					  pname->pathname.logical ? MK_KEY_unspecific : mk_cl_Cnil,
					  pname->pathname.directory,
					  mk_cl_Cnil, mk_cl_Cnil, mk_cl_Cnil),
			   TRUE));
}

mkcl_object
mk_cl_host_namestring(MKCL, mkcl_object pname)
{
  mkcl_call_stack_check(env);
  pname = mk_cl_pathname(env, pname);
  pname = pname->pathname.host;
  if (mkcl_Null(pname) || pname == MK_KEY_wild)
    pname = mkcl_core.empty_base_string;
  mkcl_return_value(pname);
}

#define EN_MATCH(e,p1,p2,el) (mkcl_equalp(e,p1->pathname.el, p2->pathname.el) ? mk_cl_Cnil : p1->pathname.el)

mkcl_object mk_cl_enough_namestring(MKCL, mkcl_narg narg, mkcl_object path, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object newpath, pathdir, defaultdir, fname;
    mkcl_object defaults = mk_si_default_pathname_defaults(env);
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_CL_enough_namestring, narg, 1, path, &defaults);

    defaults = mk_cl_pathname(env, defaults);
    path = mk_cl_pathname(env, path);
    pathdir = path->pathname.directory;
    defaultdir = defaults->pathname.directory;
    if (mkcl_Null(pathdir)) {
      pathdir = mkcl_list1(env, MK_KEY_relative);
    } else if (mkcl_Null(defaultdir)) {
      /* The defaults pathname does not have a directory. */
    } else if (MKCL_CONSP(pathdir) && (MKCL_CONS_CAR(pathdir) == MK_KEY_relative)) {
      /* The pathname is relative to the default one, so we just output the original one. */
    } else {
      /* The new pathname is an absolute one. We compare it with the defaults
         and if they have some common elements, we just output the remaining ones. */
      mkcl_object dir_begin = mkcl_funcall4(env, MK_CL_mismatch->symbol.gfdef, pathdir, defaultdir, MK_KEY_test, MK_CL_equal->symbol.gfdef);
      if (dir_begin == mk_cl_Cnil) {
        pathdir = mk_cl_Cnil;
      } else if (dir_begin == mk_cl_length(env, defaultdir)) {
        pathdir = mkcl_funcall2(env, MK_CL_subseq->symbol.gfdef, pathdir, dir_begin);
        pathdir = MKCL_CONS(env, MK_KEY_relative, pathdir);
      }
    }
    fname = EN_MATCH(env, path, defaults, name);
    if (fname == mk_cl_Cnil) fname = path->pathname.name;
    /* Create a path with all elements that do not match the default */
    newpath = _make_pathname(env,
                             path->pathname.logical,
                             EN_MATCH(env, path, defaults, host),
                             path->pathname.logical ? MK_KEY_unspecific : EN_MATCH(env, path, defaults, device),
                             pathdir,
                             fname,
                             EN_MATCH(env, path, defaults, type),
                             EN_MATCH(env, path, defaults, version));
    mkcl_return_value(mkcl_namestring(env, newpath, TRUE));
  }
}

#undef EN_MATCH

/* --------------- PATHNAME MATCHING ------------------ */

static bool
do_path_item_match(MKCL, mkcl_object s, mkcl_index j, mkcl_object p, mkcl_index i)
{
  mkcl_index ls = mkcl_length(env, s), lp = mkcl_length(env, p);
  while (i < lp) {
    mkcl_character cp = mkcl_char(env, p, i);
    if (cp == '*') {
      /* An asterisk in the pattern matches any number
       * of characters. We try the shortest sequence
       * that matches. */
      mkcl_character cn = '\0';
      mkcl_index next;
      for (next = i+1;
	   next < lp && ((cn = mkcl_char(env, p, next)) == '*');
	   next++)
	;
      if (next == lp) {
	return TRUE;
      }
      while (j < ls) {
	if (do_path_item_match(env, s, j, p, next)) {
	  return TRUE;
	}
	j++;
      }
      return FALSE;
    }
    if ((j >= ls) || (cp != mkcl_char(env, s, j))) {
      /* Either there are no characters left in "s"
       * or the next character does not match. */
      return FALSE;
    }
    i++; j++;
  }
  return (j >= ls);
}

static bool
path_item_match(MKCL, mkcl_object a, mkcl_object wildcard)
{
  if (wildcard == MK_KEY_wild)
    return TRUE;
  /* If a component in the tested path is a wildcard field, this
     can only be matched by the same wildcard field in the wildcard */
  if (!mkcl_stringp(env, a) || wildcard == mk_cl_Cnil)
    return (a == wildcard);
  if (!mkcl_stringp(env, wildcard))
    mkcl_FEerror(env, "~S is not supported as wildcard for pathname-match-p", 1, wildcard);
  return do_path_item_match(env, a, 0, wildcard, 0);
}

static bool
path_list_match(MKCL, mkcl_object a, mkcl_object wildcard)
{
  mkcl_object sub_wildcard;
  while (!mkcl_endp(env, wildcard)) {
    sub_wildcard = MKCL_CAR(wildcard);
    wildcard = MKCL_CDR(wildcard);
    if (sub_wildcard == MK_KEY_wild_inferiors) {
      if (mkcl_endp(env, wildcard))
	return TRUE;
      while (!mkcl_endp(env, a)) {
	if (path_list_match(env, a, wildcard))
	  return TRUE;
	a = MKCL_CDR(a);
      }
      return FALSE;
    } else if (mkcl_endp(env, a)) {
      /* A NIL directory should match against :absolute
	 or :relative, in order to perform suitable translations. */
      if (sub_wildcard != MK_KEY_absolute && sub_wildcard != MK_KEY_relative)
	return FALSE;
    } else if (!path_item_match(env, MKCL_CAR(a), sub_wildcard)) {
      return FALSE;
    } else {
      a = MKCL_CDR(a);
    }
  }
  if (!mkcl_endp(env, a))
    return FALSE;
  return TRUE;
}

struct mkcl_cfun mk_cl_pathname_match_p_cfunobj = MKCL_CFUN2(mk_cl_pathname_match_p, MK_CL_pathname_match_p);

mkcl_object
mk_cl_pathname_match_p(MKCL, mkcl_object path, mkcl_object wildcard)
{
  mkcl_call_stack_check(env);
  path = mk_cl_pathname(env, path);
  wildcard = mk_cl_pathname(env, wildcard);
  if (path->pathname.logical != wildcard->pathname.logical)
    { mkcl_return_value(mk_cl_Cnil); }

  /* Missing components default to :WILD */
  if (!mkcl_Null(wildcard->pathname.host)
      && ((wildcard->pathname.logical
           && (mk_cl_string_equal(env, 2, path->pathname.host, wildcard->pathname.host) == mk_cl_Cnil))
	  || (!wildcard->pathname.logical && !mkcl_string_E(env, path->pathname.host, wildcard->pathname.host)))
      )
    { mkcl_return_value(mk_cl_Cnil); }
  if (!wildcard->pathname.logical
      && !(mkcl_Null(wildcard->pathname.device) || (wildcard->pathname.device == MK_KEY_unspecific))
      && (mk_cl_string_equal(env, 2, path->pathname.device, wildcard->pathname.device) == mk_cl_Cnil))
    { mkcl_return_value(mk_cl_Cnil); }

  if (!mkcl_Null(wildcard->pathname.directory)
      && !path_list_match(env, path->pathname.directory, wildcard->pathname.directory))
    { mkcl_return_value(mk_cl_Cnil); }
  if (!mkcl_Null(wildcard->pathname.name)
      && !path_item_match(env, path->pathname.name, wildcard->pathname.name))
    { mkcl_return_value(mk_cl_Cnil); }
  if (!mkcl_Null(wildcard->pathname.type)
      && !path_item_match(env, path->pathname.type, wildcard->pathname.type))
    { mkcl_return_value(mk_cl_Cnil); }
#if (MKCL_UNIX || MKCL_WINDOWS) /* physical pathname version is meaningless on Unix or MS-Windows */
  if (wildcard->pathname.logical)
#endif
    if (!(mkcl_Null(wildcard->pathname.version)
	  || wildcard->pathname.version == MK_KEY_unspecific
	  || path_item_match(env, path->pathname.version, wildcard->pathname.version)))
    { mkcl_return_value(mk_cl_Cnil); }

  mkcl_return_value(mk_cl_Ct);
}

/* --------------- PATHNAME TRANSLATIONS ------------------ */

static mkcl_object
coerce_to_from_pathname(MKCL, mkcl_object x, mkcl_object host)
{
  switch (mkcl_type_of(x)) {
  default:
    mkcl_FEerror(env, "~S is not a valid from-pathname translation", 1, x);
  case mkcl_t_string:
  case mkcl_t_base_string:
    x = mk_cl_parse_namestring(env, 2, x, host);
    goto mkcl_t_pathname_case;  /* fallthrough is such bad taste! */
  case mkcl_t_pathname:
  mkcl_t_pathname_case:
    if (x->pathname.logical)
      return x;
    else
      mkcl_FEerror(env, "~S is not a logical pathname as it must to be a valid from-pathname translation", 1, x);
  }
}

mkcl_object mk_si_pathname_translations(MKCL, mkcl_narg narg, mkcl_object host, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_index parsed_len, len;
    mkcl_object pair, l;
    mkcl_object set = MKCL_OBJNULL;
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_SI_pathname_translations, narg, 1, host, &set);

    /* Check that host is a valid host name */
    host = mkcl_check_type_string(env, MK_SI_pathname_translations, host);
    len = mkcl_length(env, host);
    parse_word(env, host, is_null, WORD_LOGICAL, 0, len, &parsed_len, mkcl_may_be_wild_namestring);
    if (parsed_len < len) {
      mkcl_FEerror(env, "Wrong host syntax ~S", 1, host);
    }
    /* Find its translation list */
    pair = mk_cl_assoc(env, 4, host, mkcl_core.pathname_translations, MK_KEY_test, MK_CL_string_equal);
    if (set == MKCL_OBJNULL) {
      mkcl_return_value(((pair == mk_cl_Cnil)? mk_cl_Cnil : MKCL_CADR(pair)));
    }
    /* Set the new translation list */
    mkcl_assert_type_list(env, set);
    if (pair == mk_cl_Cnil) {
      pair = MKCL_CONS(env, host, MKCL_CONS(env, mk_cl_Cnil, mk_cl_Cnil));
      mkcl_core.pathname_translations = MKCL_CONS(env, pair, mkcl_core.pathname_translations);
    }
    for (l = set, set = mk_cl_Cnil; !mkcl_endp(env, l); l = MKCL_CDR(l)) {
      mkcl_object item = MKCL_CAR(l);
      mkcl_object from = coerce_to_from_pathname(env, mk_cl_car(env, item), host);
      mkcl_object to = mk_cl_pathname(env, mk_cl_cadr(env, item));
      set = MKCL_CONS(env, MKCL_CONS(env, from, MKCL_CONS(env, to, mk_cl_Cnil)), set);
    }
    set = mk_cl_nreverse(env, set);
    MKCL_RPLACA(MKCL_CONS_CDR(pair), set);
    mkcl_return_value(set);
  }
}

struct mkcl_cfun mk_si_all_logical_pathname_translations_cfunobj = MKCL_CFUN0(mk_si_all_logical_pathname_translations, MK_SI_all_logical_pathname_translations);

mkcl_object mk_si_all_logical_pathname_translations(MKCL)
{
  mkcl_call_stack_check(env);
  mkcl_object x = mk_cl_copy_alist(env, mkcl_core.pathname_translations);
  mkcl_return_value(x);
}

enum case_map { preserve_case, to_lowercase, to_uppercase };

static mkcl_object
find_wilds(MKCL, mkcl_object match_list, mkcl_object source, mkcl_object wildcard, enum case_map cmap)
{
  mkcl_index i, j, k, ls, lw;

  if (wildcard == MK_KEY_wild)
    {
      mkcl_object it = source;
      if (MKCL_STRINGP(it))
        {
          if (cmap == to_lowercase) it = mk_cl_string_downcase(env, 1, it);
          else if (cmap == to_uppercase) it = mk_cl_string_upcase(env, 1, it);
          else it = mk_cl_copy_seq(env, it);
        }
      return mkcl_list1(env, it);
    }
  if (!mkcl_stringp(env, wildcard) || !mkcl_stringp(env, source)) {
    if (wildcard != source)
      return MK_KEY_error;
    return match_list;
  }
  ls = mkcl_length(env, source);
  lw = mkcl_length(env, wildcard);
  for(i = j = 0; i < ls && j < lw; ) {
    mkcl_character pattern_char = mkcl_char(env, wildcard,j);
    if (pattern_char == '*') {
      for (j++, k = i;
	   k < ls && mkcl_char(env, source,k) != pattern_char;
	   k++)
	;
      {
        mkcl_object it = make_one(env, source, i, k);
        if (cmap == to_lowercase) it = mk_cl_nstring_downcase(env, 1, it);
        if (cmap == to_uppercase) it = mk_cl_nstring_upcase(env, 1, it);
        match_list = MKCL_CONS(env, it, match_list);
      }
      i = k;
      continue;
    }
    if (mkcl_char(env, source,i) != pattern_char)
      return MK_KEY_error;
    i++, j++;
  }
  if (i < ls || j < lw)
    return MK_KEY_error;
  return match_list;
}

static mkcl_object
find_list_wilds(MKCL, mkcl_object list_source, mkcl_object wildcard, enum case_map cmap)
{
  mkcl_object match_list = mk_cl_Cnil;

  while (!mkcl_endp(env, wildcard)) {
    mkcl_object sub_wildcard = MKCL_CAR(wildcard);
    wildcard = MKCL_CDR(wildcard);
    if (sub_wildcard == MK_KEY_wild_inferiors) {
      mkcl_object l2 = mk_cl_Cnil;
      while (!path_list_match(env, list_source, wildcard)) {
	if (mkcl_endp(env, list_source))
	  return MK_KEY_error;
        {
          mkcl_object it = MKCL_CAR(list_source);

          if (MKCL_STRINGP(it))
            {
              if (cmap == to_lowercase) it = mk_cl_string_downcase(env, 1, it);
              else if (cmap == to_uppercase) it = mk_cl_string_upcase(env, 1, it);
              else it = mk_cl_copy_seq(env, it);
            }
          l2 = MKCL_CONS(env, it, l2);
        }
	list_source = MKCL_CDR(list_source);
      }
      match_list = MKCL_CONS(env, l2, match_list);
    } else if (mkcl_endp(env, list_source)) {
      /* A NIL directory should match against :absolute
	 or :relative, in order to perform suitable translations. */
      if (sub_wildcard != MK_KEY_absolute && sub_wildcard != MK_KEY_relative)
	return MK_KEY_error;
    } else {
      mkcl_object l2 = find_wilds(env, match_list, MKCL_CAR(list_source), sub_wildcard, cmap);
      if (match_list == MK_KEY_error)
	return MK_KEY_error;
      if (!mkcl_Null(l2))
	match_list = MKCL_CONS(env, l2, match_list);
      list_source = MKCL_CDR(list_source);
    }
  }
  return mk_cl_nreverse(env, match_list);
}

static mkcl_object
copy_wildcards(MKCL, mkcl_object *wilds_list, mkcl_object pattern)
{
  mkcl_index i, l, j;
  bool new_string;
  mkcl_object wilds = *wilds_list, token;

  if (pattern == MK_KEY_wild) {
    if (mkcl_endp(env, wilds))
      return MK_KEY_error;
    pattern = MKCL_CAR(wilds);
    *wilds_list = MKCL_CDR(wilds);
    return pattern;
  }
  if (pattern == MK_KEY_wild_inferiors)
    return MK_KEY_error;
  if (!mkcl_stringp(env, pattern))
    return pattern;

  new_string = FALSE;
  l = mkcl_length(env, pattern);
  token = mk_si_get_buffer_string(env);
  for (j = i = 0; i < l; ) {
    mkcl_character c = mkcl_char(env, pattern, i);
    if (c != '*') {
      i++;
      continue;
    }
    if (i != j) {
      push_substring(env, token, pattern, j, i);
    }
    new_string = TRUE;
    if (mkcl_endp(env, wilds)) {
      return MK_KEY_error;
    }
    push_string(env, token, MKCL_CAR(wilds));
    wilds = MKCL_CDR(wilds);
    j = i++;
  }
  /* Only create a new string when needed */
  if (new_string) {
    if (mkcl_fits_in_base_string(env, token)) {
      pattern = mk_si_copy_to_simple_base_string(env, token);
    } else {
      pattern = mk_cl_copy_seq(env, token);
    }
  }
  mk_si_put_buffer_string(env, token);
  *wilds_list = wilds;
  return pattern;
}

static mkcl_object
copy_list_wildcards(MKCL, mkcl_object *wilds, mkcl_object to)
{
  mkcl_object l = mk_cl_Cnil;

  while (!mkcl_endp(env, to)) {
    mkcl_object d, wildcard = MKCL_CAR(to);
    if (wildcard == MK_KEY_wild_inferiors) {
      mkcl_object list = *wilds;
      if (mkcl_endp(env, list))
	return MK_KEY_error;
      else {
	mkcl_object dirlist = MKCL_CAR(list);
	if (MKCL_CONSP(dirlist))
	  l = mkcl_append(env, MKCL_CAR(list), l);
	else if (!mkcl_Null(MKCL_CAR(list)))
	  return MK_KEY_error;
      }
      *wilds = MKCL_CDR(list);
    } else {
      d = copy_wildcards(env, wilds, MKCL_CAR(to));
      if (d == MK_KEY_error)
	return d;
      l = MKCL_CONS(env, d, l);
    }
    to = MKCL_CDR(to);
  }
  if (MKCL_CONSP(l))
    l = mk_cl_nreverse(env, l);
  return l;
}

mkcl_object mk_cl_translate_pathname(MKCL, mkcl_narg narg, mkcl_object source, mkcl_object from, mkcl_object to, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object wilds, out, d;
    MKCL_RECEIVE_0_KEYWORD_ARGUMENTS(env, MK_CL_translate_pathname, narg, 3, to);

    enum case_map case_mapping = preserve_case;
    mkcl_object host, device, dir, name, type, version;
    /* The pathname from which we get the data */
    source = mk_cl_pathname(env, source);
    /* The wildcard applied to the source pathname */
    from = mk_cl_pathname(env, from);
    /* The pattern which says what the output should look like */
    to = mk_cl_pathname(env, to);
  
    if (source->pathname.logical != from->pathname.logical)
      goto error;
    out = mkcl_alloc_raw_pathname(env);
    out->pathname.namestring = mk_cl_Cnil;
    out->pathname.logical = to->pathname.logical;
#if MKCL_UNIX || MKCL_WINDOWS
    /* The customary case of filenames on Unix is usually lowercase so we force it.
       MS-Windows is usually case indifferent so we do the same in case it may matter. */
    if (from->pathname.logical && !(to->pathname.logical)) case_mapping = to_lowercase;
    else if (!(from->pathname.logical) && to->pathname.logical) case_mapping = to_uppercase;
#endif
  
    /* Match host names */
    if (mk_cl_string_equal(env, 2, source->pathname.host, from->pathname.host) == mk_cl_Cnil)
      goto error;
    host = out->pathname.host = to->pathname.host;
  
    /* Logical pathnames do not have devices. We just overwrite it. */ /* FIXME: What about normal pathnames? JCB */
    device = out->pathname.device = to->pathname.device;
  
    /* Match directories */
    wilds = find_list_wilds(env, source->pathname.directory, from->pathname.directory, case_mapping);
    if (wilds == MK_KEY_error)  goto error;
    d = copy_list_wildcards(env, &wilds, to->pathname.directory);
    if (d == MK_KEY_error) goto error;
    if (wilds != mk_cl_Cnil) goto error2;
    dir = out->pathname.directory = d;
  
    /* Match name */
    wilds = find_wilds(env, mk_cl_Cnil, source->pathname.name, from->pathname.name, case_mapping);
    if (wilds == MK_KEY_error) goto error2;
    d = copy_wildcards(env, &wilds, to->pathname.name);
    if (d == MK_KEY_error) goto error;
    if (wilds != mk_cl_Cnil) goto error2;
    name = out->pathname.name = d;
  
    /* Match type */
    wilds = find_wilds(env, mk_cl_Cnil, source->pathname.type, from->pathname.type, case_mapping);
    if (wilds == MK_KEY_error) goto error2;
    d = copy_wildcards(env, &wilds, to->pathname.type);
    if (d == MK_KEY_error) goto error;
    if (wilds != mk_cl_Cnil) goto error2;
    type = out->pathname.type = d;
  
    /* Match version */
    out->pathname.version = to->pathname.version;
    if (from->pathname.version == MK_KEY_wild) {
      if (to->pathname.version == MK_KEY_wild) {
        out->pathname.version = source->pathname.version;
      }
    }
    if (mkcl_Null(name) && mkcl_Null(type)) /* version on a directory has no meaning. JCB */
      out->pathname.version = mk_cl_Cnil;

    version = out->pathname.version;

    dir = canonicalize_directory_destructively(env, dir, out->pathname.logical);
    if (MK_KEY_error == dir)
      mk_cl_error(env, 3, MK_CL_file_error, MK_KEY_pathname, out);
    else
      out->pathname.directory = dir;

    out->pathname.complete = (host != mk_cl_Cnil && device != mk_cl_Cnil && dir != mk_cl_Cnil
                              && name != mk_cl_Cnil && type != mk_cl_Cnil && version != mk_cl_Cnil
                              && MKCL_CONSP(dir) && (MKCL_CONS_CAR(dir) == MK_KEY_absolute));
    mkcl_return_value(out);
  
  error:
    mkcl_FEerror(env, "~S is not a specialization of path ~S", 2, source, from);
  error2:
    mkcl_FEerror(env, "Number of wildcards in ~S do not match  ~S", 2, from, to);
  }
}

mkcl_object mk_cl_translate_logical_pathname(MKCL, mkcl_narg narg, mkcl_object source, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object pathname = mk_cl_pathname(env, source);
    MKCL_RECEIVE_0_KEYWORD_ARGUMENTS(env, MK_CL_translate_pathname, narg, 1, source);

  begin:
    if (!pathname->pathname.logical)
      { mkcl_return_value(pathname); }
    else
      {
        mkcl_object l = mk_si_pathname_translations(env, 1, pathname->pathname.host);
        for(; MKCL_CONSP(l); l = MKCL_CONS_CDR(l)) {
          mkcl_object pair = MKCL_CONS_CAR(l);
          if (!mkcl_Null(mk_cl_pathname_match_p(env, pathname, mk_cl_car(env, pair)))) {
            pathname = mk_cl_translate_pathname(env, 3, pathname, mk_cl_car(env, pair), mk_cl_cadr(env, pair));
            goto begin;
          }
        }
      }
    mkcl_FEerror(env, "~S admits no logical pathname translations", 1, source);
  }
}

;;;;;;

