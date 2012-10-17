/* -*- mode: c -*- */
/*
    file.d -- File interface.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2012, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

/*
	OS-PLATFORM-DEPENDENT

	The file contains code to reclaim the I/O buffer
	by accessing the FILE structure of C, among other things.
*/

#define _FILE_OFFSET_BITS 64

#include <mkcl/mkcl.h>
#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <mkcl/mkcl-inl.h>
#include <mkcl/internal.h>

#ifdef __unix
# include <sys/socket.h>
# include <unistd.h>
# include <sys/select.h>
typedef int SOCKET;
# define INVALID_SOCKET ((SOCKET)(~0)) /* a Windowsism */
# define SOCKET_ERROR (-1)
#elif defined(MKCL_WINDOWS)
# if 0
#  include <winsock2.h> /* Had to be included before windows.h */
# endif
# include <sys/stat.h>
# define STDIN_FILENO 0
# define STDOUT_FILENO 1
# define STDERR_FILENO 2
# define HAVE_SELECT
#elif defined(HAVE_SYS_IOCTL_H) && !defined(cygwin)
# include <sys/ioctl.h>
#endif

#include <sys/stat.h>

#ifdef HAVE_FSEEKO
# define mkcl_off_t off_t
# define mkcl_fseeko fseeko
# define mkcl_ftello ftello
#elif defined(MKCL_WINDOWS)
# define mkcl_off_t off64_t
# define mkcl_fseeko fseeko64
# define mkcl_ftello ftello64
#else
# define mkcl_off_t int
# define mkcl_fseeko fseek
# define mkcl_ftello ftell
#endif

/* Maximum number of bytes required to encode a character.
 * This currently corresponds to (4 + 2) for the ISO-2022-JP-* encodings
 * with 4 being the charset prefix, 2 for the character.
 */
#define ENCODING_BUFFER_MAX_SIZE 6

static mkcl_index mkcl_read_octet(MKCL, mkcl_object stream, unsigned char *c, mkcl_index n);
static mkcl_index mkcl_write_octet(MKCL, mkcl_object stream, unsigned char *c, mkcl_index n);

static struct mkcl_file_ops *duplicate_dispatch_table(MKCL, const struct mkcl_file_ops *ops);
static const struct mkcl_file_ops *stream_dispatch_table(MKCL, mkcl_object strm);

static int flisten(MKCL, FILE *);
static int file_listen(MKCL, int fileno);
static mkcl_object mkcl_off_t_to_integer(MKCL, mkcl_off_t offset);
static mkcl_off_t mkcl_integer_to_off_t(MKCL, mkcl_object offset);

static mkcl_object alloc_stream(MKCL);

static mkcl_object not_a_file_stream(MKCL, mkcl_object fn);
static void not_an_input_stream(MKCL, mkcl_object fn);
static void not_an_output_stream(MKCL, mkcl_object fn);
static void not_a_character_stream(MKCL, mkcl_object s);
static void not_a_binary_stream(MKCL, mkcl_object s);
static bool restartable_io_error(MKCL, mkcl_object strm, mkcl_interrupt_status * old_intr_ptr);
static void unread_error(MKCL, mkcl_object strm);
static void unread_twice(MKCL, mkcl_object strm);
static void io_error(MKCL, mkcl_object strm);
static void character_size_overflow(MKCL, mkcl_object strm, mkcl_character c);
static void wrong_file_handler(MKCL, mkcl_object strm) __attribute__((noreturn));

static mkcl_index encoding_error(MKCL, mkcl_object stream, unsigned char *buffer, mkcl_character ch);
static mkcl_character decoding_error(MKCL, mkcl_object stream, unsigned char *buffer, int length);

static void socket_error(MKCL, const char *err_msg, mkcl_object strm);

#ifdef MKCL_WINDOWS
static BOOL is_a_console(MKCL, HANDLE hnd);
static BOOL should_try_to_read_again(MKCL, int fd);
#endif

static mkcl_word normalize_stream_element_type(MKCL, mkcl_object element_type);

static const struct mkcl_file_ops socket_stream_input_ops;
static const struct mkcl_file_ops socket_stream_output_ops;
static const struct mkcl_file_ops socket_stream_io_ops;

/*********************************************************************/

static const struct mkcl_file_ops mk_clos_stream_ops;

static inline const struct mkcl_file_ops *
stream_dispatch_table(MKCL, mkcl_object strm)
{
  if (MKCL_INSTANCEP(strm)) {
    return &mk_clos_stream_ops;
  }
  if (mkcl_type_of(strm) != mkcl_t_stream)
    mkcl_FEtype_error_stream(env, strm);
  return (const struct mkcl_file_ops *)strm->stream.ops;
}

static inline struct mkcl_file_ops *
duplicate_dispatch_table(MKCL, const struct mkcl_file_ops *ops)
{
  struct mkcl_file_ops *new_ops = mkcl_alloc(env, sizeof(*ops));
  *new_ops = *ops;
  return new_ops;
}




/**********************************************************************
 * NOT IMPLEMENTED or NOT APPLICABLE OPERATIONS
 */

static mkcl_index
not_output_write_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  not_an_output_stream(env, strm);
  return 0;
}

static mkcl_index
not_input_read_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  not_an_input_stream(env, strm);
  return 0;
}

static mkcl_index
not_binary_read_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  not_a_binary_stream(env, strm);
  return 0;
}

static void
not_output_write_byte(MKCL, mkcl_object c, mkcl_object strm)
{
  not_an_output_stream(env, strm);
}

static mkcl_object
not_input_read_byte(MKCL, mkcl_object strm)
{
  not_an_input_stream(env, strm);
  return MKCL_OBJNULL;
}

static void
not_binary_write_byte(MKCL, mkcl_object c, mkcl_object strm)
{
  not_a_binary_stream(env, strm);
}

static mkcl_object
not_binary_read_byte(MKCL, mkcl_object strm)
{
  not_a_binary_stream(env, strm);
  return MKCL_OBJNULL;
}

static mkcl_character
not_input_read_char(MKCL, mkcl_object strm)
{
  not_an_input_stream(env, strm);
  return -1;
}

static mkcl_character
not_output_write_char(MKCL, mkcl_object strm, mkcl_character c)
{
  not_an_output_stream(env, strm);
  return c;
}

static void
not_input_unread_char(MKCL, mkcl_object strm, mkcl_character c)
{
  not_an_input_stream(env, strm);
}

static int
not_input_listen(MKCL, mkcl_object strm)
{
  not_an_input_stream(env, strm);
  return -1;
}

static mkcl_character
not_character_read_char(MKCL, mkcl_object strm)
{
  not_a_character_stream(env, strm);
  return -1;
}

static mkcl_character
not_character_write_char(MKCL, mkcl_object strm, mkcl_character c)
{
  not_a_character_stream(env, strm);
  return c;
}

static void
not_input_clear_input(MKCL, mkcl_object strm)
{
  not_an_input_stream(env, strm);
  return;
}

static void
not_output_clear_output(MKCL, mkcl_object strm)
{
  not_an_output_stream(env, strm);
}

static void
not_output_force_output(MKCL, mkcl_object strm)
{
  not_an_output_stream(env, strm);
}

static void
not_output_finish_output(MKCL, mkcl_object strm)
{
  not_an_output_stream(env, strm);
}

static mkcl_object
not_implemented_get_position(MKCL, mkcl_object strm)
{
  mkcl_FEerror(env, "file-position not implemented for stream ~S", 1, strm);
  return mk_cl_Cnil;
}

static mkcl_object
not_implemented_set_position(MKCL, mkcl_object strm, mkcl_object pos)
{
  mkcl_FEerror(env, "file-position not implemented for stream ~S", 1, strm);
  return mk_cl_Cnil;
}

/**********************************************************************
 * CLOSED STREAM OPS
 */

static mkcl_index
closed_stream_read_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  mkcl_FEclosed_stream(env, strm);
  return 0;
}

static mkcl_index
closed_stream_write_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  mkcl_FEclosed_stream(env, strm);
  return 0;
}

static void
closed_stream_write_byte(MKCL, mkcl_object c, mkcl_object strm)
{
  mkcl_FEclosed_stream(env, strm);
}

static mkcl_object
closed_stream_read_byte(MKCL, mkcl_object strm)
{
  mkcl_FEclosed_stream(env, strm);
  return mk_cl_Cnil;
}

static mkcl_character
closed_stream_read_char(MKCL, mkcl_object strm)
{
  mkcl_FEclosed_stream(env, strm);
  return 0;
}

static mkcl_character
closed_stream_write_char(MKCL, mkcl_object strm, mkcl_character c)
{
  mkcl_FEclosed_stream(env, strm);
  return c;
}

static void
closed_stream_unread_char(MKCL, mkcl_object strm, mkcl_character c)
{
  mkcl_FEclosed_stream(env, strm);
}

static mkcl_character
closed_stream_peek_char(MKCL, mkcl_object strm)
{
  mkcl_FEclosed_stream(env, strm);
  return 0;
}

static mkcl_index
closed_stream_read_vector(MKCL, mkcl_object strm, mkcl_object data, mkcl_index start, mkcl_index end)
{
  mkcl_FEclosed_stream(env, strm);
  return 0;
}

static mkcl_index
closed_stream_write_vector(MKCL, mkcl_object strm, mkcl_object data, mkcl_index start, mkcl_index end)
{
  mkcl_FEclosed_stream(env, strm);
  return 0;
}

static int
closed_stream_listen(MKCL, mkcl_object strm)
{
  mkcl_FEclosed_stream(env, strm);
  return 0;
}

static void
closed_stream_clear_input(MKCL, mkcl_object strm)
{
  mkcl_FEclosed_stream(env, strm);
}

#define closed_stream_clear_output closed_stream_clear_input
#define closed_stream_force_output closed_stream_clear_input
#define closed_stream_finish_output closed_stream_clear_input

static mkcl_object
closed_stream_length(MKCL, mkcl_object strm)
{
  mkcl_FEclosed_stream(env, strm);
}

#define closed_stream_get_position closed_stream_length

static mkcl_object
closed_stream_set_position(MKCL, mkcl_object strm, mkcl_object position)
{
  mkcl_FEclosed_stream(env, strm);
}

static int
closed_stream_column(MKCL, mkcl_object strm)
{
  mkcl_FEclosed_stream(env, strm);
  return 0;
}


/**********************************************************************
 * GENERIC OPERATIONS
 *
 * Versions of the methods which are defined in terms of others
 */
/*
 * Byte operations based on octet operators.
 */
static mkcl_object
generic_read_byte_unsigned8(MKCL, mkcl_object strm)
{
  unsigned char c;
  if (strm->stream.ops->read_octet(env, strm, &c, 1) < 1) {
    return mk_cl_Cnil;
  }
  return MKCL_MAKE_FIXNUM(c);
}

static void
generic_write_byte_unsigned8(MKCL, mkcl_object byte, mkcl_object strm)
{
  unsigned char c = mkcl_integer_to_index(env, byte);
  strm->stream.ops->write_octet(env, strm, &c, 1);
}

static mkcl_object
generic_read_byte_signed8(MKCL, mkcl_object strm)
{
  signed char c;
  if (strm->stream.ops->read_octet(env, strm, (unsigned char *)&c, 1) < 1)
    return mk_cl_Cnil;
  return MKCL_MAKE_FIXNUM(c);
}

static void
generic_write_byte_signed8(MKCL, mkcl_object byte, mkcl_object strm)
{
  signed char c = mkcl_integer_to_word(env, byte);
  strm->stream.ops->write_octet(env, strm, (unsigned char *)&c, 1);
}

static mkcl_object
generic_read_byte_le(MKCL, mkcl_object strm)
{
  mkcl_index (*read_octet)(MKCL, mkcl_object, unsigned char *, mkcl_index);
  unsigned char c;
  mkcl_index nb, bs;
  mkcl_object output = MKCL_MAKE_FIXNUM(0);
  read_octet = strm->stream.ops->read_octet;
  bs = strm->stream.byte_size;
  for (nb = 0; bs >= 8; bs -= 8, nb += 8) {
    mkcl_object aux;
    if (read_octet(env, strm, &c, 1) < 1)
      return mk_cl_Cnil;
    if (bs <= 8 && (strm->stream.flags & MKCL_STREAM_SIGNED_BYTES))
      aux = MKCL_MAKE_FIXNUM((signed char)c);
    else
      aux = MKCL_MAKE_FIXNUM((unsigned char)c);
    output = mk_cl_logior(env, 2, output, mk_cl_ash(env, aux, MKCL_MAKE_FIXNUM(nb)));
  }
  return output;
}

static void
generic_write_byte_le(MKCL, mkcl_object c, mkcl_object strm)
{
  mkcl_index (*write_octet)(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n);
  mkcl_index bs;
  write_octet = strm->stream.ops->write_octet;
  bs = strm->stream.byte_size;
  do {
    mkcl_object b = mk_cl_logand(env, 2, c, MKCL_MAKE_FIXNUM(0xFF));
    unsigned char aux = (unsigned char)mkcl_fixnum_to_word(b);
    if (write_octet(env, strm, &aux, 1) < 1)
      break;
    c = mk_cl_ash(env, c, MKCL_MAKE_FIXNUM(-8));
    bs -= 8;
  } while (bs);
}

static mkcl_object
generic_read_byte_be(MKCL, mkcl_object strm)
{
  mkcl_index (*read_octet)(MKCL, mkcl_object, unsigned char *, mkcl_index);
  unsigned char c;
  mkcl_object output = NULL;
  mkcl_index bs;
  read_octet = strm->stream.ops->read_octet;
  bs = strm->stream.byte_size;
  for (; bs >= 8; bs -= 8) {
    if (read_octet(env, strm, &c, 1) < 1)
      return mk_cl_Cnil;
    if (output) {
      output = mk_cl_logior(env, 2, MKCL_MAKE_FIXNUM(c),
			    mk_cl_ash(env, output, MKCL_MAKE_FIXNUM(8)));
    } else if (strm->stream.flags & MKCL_STREAM_SIGNED_BYTES) {
      output = MKCL_MAKE_FIXNUM((signed char)c);
    } else {
      output = MKCL_MAKE_FIXNUM((unsigned char)c);
    }
  }
  return output;
}

static void
generic_write_byte_be(MKCL, mkcl_object c, mkcl_object strm)
{
  mkcl_index (*write_octet)(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n);
  mkcl_index bs;
  write_octet = strm->stream.ops->write_octet;
  bs = strm->stream.byte_size;
  do {
    unsigned char aux;
    mkcl_object b;
    bs -= 8;
    b = mk_cl_logand(env, 2,
		     MKCL_MAKE_FIXNUM(0xFF),
		     bs ? mk_cl_ash(env, c, MKCL_MAKE_FIXNUM(-bs)) : c);
    aux = (unsigned char)mkcl_fixnum_to_word(b);
    if (write_octet(env, strm, &aux, 1) < 1)
      break;
  } while (bs);
}

static mkcl_character
generic_peek_char(MKCL, mkcl_object strm)
{
  mkcl_character out = mkcl_read_char(env, strm);

  if (out != EOF) mkcl_unread_char(env, out, strm);
  return out;
}

static void
generic_void(MKCL, mkcl_object strm)
{
}

static bool
generic_always_true(MKCL, mkcl_object strm)
{
  return TRUE;
}

static bool
generic_always_false(MKCL, mkcl_object strm)
{
  return FALSE;
}

static mkcl_object
generic_always_nil(MKCL, mkcl_object strm)
{
  return mk_cl_Cnil;
}

static int
generic_column(MKCL, mkcl_object strm)
{
  return 0;
}

static mkcl_object
generic_set_position(MKCL, mkcl_object strm, mkcl_object pos)
{
  return mk_cl_Cnil;
}

static mkcl_object
generic_close(MKCL, mkcl_object strm)
{
  struct mkcl_file_ops *ops = strm->stream.ops;
  if (mkcl_input_stream_p(env, strm)) {
    ops->read_octet = closed_stream_read_octet;
    ops->read_byte = closed_stream_read_byte;
    ops->read_char = closed_stream_read_char;
    ops->unread_char = closed_stream_unread_char;
    ops->peek_char = closed_stream_peek_char;
    ops->read_vector = closed_stream_read_vector;
    ops->listen = closed_stream_listen;
    ops->clear_input = closed_stream_clear_input;
  }
  if (mkcl_output_stream_p(env, strm)) {
    ops->write_octet = closed_stream_write_octet;
    ops->write_byte = closed_stream_write_byte;
    ops->write_char = closed_stream_write_char;
    ops->write_vector = closed_stream_write_vector;
    ops->clear_output = closed_stream_clear_output;
    ops->force_output = closed_stream_force_output;
    ops->finish_output = closed_stream_finish_output;
  }
  ops->get_position = closed_stream_get_position;
  ops->set_position = closed_stream_set_position;
  ops->length = closed_stream_length;
  ops->column = closed_stream_column;
  ops->close = generic_close;
  strm->stream.closed = TRUE;
  return mk_cl_Ct;
}

static mkcl_index
generic_write_vector(MKCL, mkcl_object strm, mkcl_object data, mkcl_index start, mkcl_index end)
{
  mkcl_elttype elttype;
  const struct mkcl_file_ops *ops;
  if (start >= end)
    return start;
  ops = stream_dispatch_table(env, strm);

  mkcl_type t = mkcl_type_of(data);
  if (t == mkcl_t_base_string)
    {
      mkcl_character (*write_char)(MKCL, mkcl_object, mkcl_character) = ops->write_char;
      for (; start < end; start++)
	write_char(env, strm, mkcl_base_char_index_raw(env, data, start));
    }
  else if (t == mkcl_t_string)
    {
      mkcl_character (*write_char)(MKCL, mkcl_object, mkcl_character) = ops->write_char;
      for (; start < end; start++)
	write_char(env, strm, mkcl_character_index_raw(env, data, start));
    }
  else if (MKCL_VECTOR_TYPE_P(t))
    {
      mkcl_elttype elttype = mkcl_array_elttype(env, data);

      /* FIXME: Why does the first element of "data" play a special role here? JCB */
      if (elttype == mkcl_aet_object && MKCL_CHARACTERP(mkcl_vref_index(env, data, 0))) {
	mkcl_character (*write_char)(MKCL, mkcl_object, mkcl_character) = ops->write_char;
	for (; start < end; start++) {
	  write_char(env, strm, mkcl_char_code(env, mkcl_vref_index(env, data, start)));
	}
      } else {
	void (*write_byte)(MKCL, mkcl_object, mkcl_object) = ops->write_byte;
	for (; start < end; start++) {
	  write_byte(env, mkcl_vref_index(env, data, start), strm);
	}
      }
    }
  else mkcl_FEwrong_type_argument(env, @'array', data);

  return start;
}

static mkcl_index
generic_read_vector(MKCL, mkcl_object strm, mkcl_object data, mkcl_index start, mkcl_index end)
{
  const struct mkcl_file_ops *ops;
  mkcl_object expected_type;
  if (start >= end)
    return start;
  expected_type = mkcl_stream_element_type(env, strm);
  ops = stream_dispatch_table(env, strm);
  if (expected_type == @'base-char' || expected_type == @'character') {
    mkcl_character (*read_char)(MKCL, mkcl_object) = ops->read_char;
    for (; start < end; start++) {
      mkcl_word c = read_char(env, strm);
      if (c == EOF) break;
      mkcl_elt_set(env, data, start, MKCL_CODE_CHAR(c));
    }
  } else {
    mkcl_object (*read_byte)(MKCL, mkcl_object) = ops->read_byte;
    for (; start < end; start++) {
      mkcl_object x = read_byte(env, strm);
      if (mkcl_Null(x)) break;
      mkcl_elt_set(env, data, start, x);
    }
  }
  return start;
}

/**********************************************************************
 * CHARACTER AND EXTERNAL FORMAT SUPPORT
 */

static void
eformat_unread_char(MKCL, mkcl_object strm, mkcl_character c)
{
  if (c != strm->stream.last_char) {
    unread_twice(env, strm);
  }
  {
    mkcl_object l = mk_cl_Cnil;
    unsigned char buffer[2*ENCODING_BUFFER_MAX_SIZE];
    mkcl_index ndx = 0;
    mkcl_word i = strm->stream.last_code[0];
    if (i != EOF) {
      ndx += strm->stream.encoder(env, strm, buffer, i);
    }
    i = strm->stream.last_code[1];
    if (i != EOF) {
      ndx += strm->stream.encoder(env, strm, buffer, i);
    }
    while (ndx != 0) {
      l = MKCL_CONS(env, MKCL_MAKE_FIXNUM(buffer[--ndx]), l);
    }
    strm->stream.byte_stack = mkcl_nconc(env, strm->stream.byte_stack, l);
    strm->stream.last_char = EOF;
  }
}

static mkcl_character
eformat_read_char(MKCL, mkcl_object strm)
{
  mkcl_character c = strm->stream.decoder(env, strm, strm->stream.ops->read_octet, strm);
  if (c != EOF) {
    strm->stream.last_char = c;
    strm->stream.last_code[0] = c;
    strm->stream.last_code[1] = EOF;
  }
  return c;
}

static mkcl_character
eformat_write_char(MKCL, mkcl_object strm, mkcl_character c)
{
  unsigned char buffer[ENCODING_BUFFER_MAX_SIZE];
  mkcl_index nbytes = strm->stream.encoder(env, strm, buffer, c);
  if (nbytes == 0) {
    character_size_overflow(env, strm, c);
  }
  strm->stream.ops->write_octet(env, strm, buffer, nbytes);
  if (c == '\n')
    MKCL_IO_STREAM_COLUMN(strm) = 0;
  else if (c == '\t')
    MKCL_IO_STREAM_COLUMN(strm) = (MKCL_IO_STREAM_COLUMN(strm)&~((mkcl_word) 07)) + 8;
  else
    MKCL_IO_STREAM_COLUMN(strm)++;
  return c;
}

static mkcl_character
eformat_read_char_cr(MKCL, mkcl_object strm)
{
  mkcl_character c = eformat_read_char(env, strm);
  if (c == MKCL_CHAR_CODE_RETURN) {
    c = MKCL_CHAR_CODE_NEWLINE;
    strm->stream.last_char = c;
  }
  return c;
}

static mkcl_character
eformat_write_char_cr(MKCL, mkcl_object strm, mkcl_character c)
{
  if (c == MKCL_CHAR_CODE_NEWLINE) {
    eformat_write_char(env, strm, MKCL_CHAR_CODE_RETURN);
    MKCL_IO_STREAM_COLUMN(strm) = 0;
    return c;
  }
  return eformat_write_char(env, strm, c);
}

static mkcl_character
eformat_read_char_crlf(MKCL, mkcl_object strm)
{
  mkcl_character c = eformat_read_char(env, strm);
  if (c == MKCL_CHAR_CODE_RETURN) {
    c = eformat_read_char(env, strm);
    if (c == MKCL_CHAR_CODE_LINEFEED) {
      strm->stream.last_code[1] = c;
      c = MKCL_CHAR_CODE_NEWLINE;
    } else {
      eformat_unread_char(env, strm, c);
      c = MKCL_CHAR_CODE_RETURN;
      strm->stream.last_code[0] = c;
      strm->stream.last_code[1] = EOF;
    }
    strm->stream.last_char = c;
  }
  return c;
}

static mkcl_character
eformat_write_char_crlf(MKCL, mkcl_object strm, mkcl_character c)
{
  if (c == MKCL_CHAR_CODE_NEWLINE) {
    eformat_write_char(env, strm, MKCL_CHAR_CODE_RETURN);
    eformat_write_char(env, strm, MKCL_CHAR_CODE_LINEFEED);
    MKCL_IO_STREAM_COLUMN(strm) = 0;
    return c;
  }
  return eformat_write_char(env, strm, c);
}

/*
 * If we use Unicode, this is LATIN-1, ISO-8859-1, that is the 256
 * lowest codes of Unicode.
 */

static mkcl_character
passthrough_decoder(MKCL, mkcl_object stream, mkcl_eformat_read_octet read_octet, mkcl_object source)
{
  unsigned char aux;

  if (read_octet(env, source, &aux, 1) < 1)
    return EOF;
  else
    return aux;
}

static mkcl_index
passthrough_encoder(MKCL, mkcl_object stream, unsigned char *buffer, mkcl_character c)
{
  if (c <= 0xFF) {
    buffer[0] = c;
    return 1;
  }
  else
    return encoding_error(env, stream, buffer, c);
}

/*
 * US ASCII, that is the 128 (0 to 127) lowest codes of Unicode
 */

static mkcl_character
ascii_decoder(MKCL, mkcl_object stream, mkcl_eformat_read_octet read_octet, mkcl_object source)
{
  unsigned char aux;

  if (read_octet(env, source, &aux, 1) < 1) {
    return EOF;
  } else if (aux > 0x7F) {
    return decoding_error(env, stream, &aux, 1);
  } else {
    return aux;
  }
}

static mkcl_index
ascii_encoder(MKCL, mkcl_object stream, unsigned char *buffer, mkcl_character c)
{
  if (c <= 0x7F) {
    buffer[0] = c;
    return 1;
  }
  else
    return encoding_error(env, stream, buffer, c);
}

/*
 * UTF-32 BIG ENDIAN
 */

static mkcl_character
utf_32be_decoder(MKCL, mkcl_object stream, mkcl_eformat_read_octet read_octet, mkcl_object source)
{
  unsigned char buffer[4] = { '\0', '\0', '\0', '\0' };
  mkcl_index nb_ch = 0;

  if ((nb_ch = read_octet(env, source, buffer, 4)) < 4) {
    if (nb_ch)
      return decoding_error(env, stream, buffer, 4);
    else    
      return EOF;
  } else {
    return buffer[3] | (buffer[2]<<8) | (buffer[1]<<16) | (buffer[0]<<24);
  }
}

static mkcl_index
utf_32be_encoder(MKCL, mkcl_object stream, unsigned char *buffer, mkcl_character c)
{
  if (c <= 0x10FFFF) {
    buffer[3] = c & 0xFF; c >>= 8;
    buffer[2] = c & 0xFF; c >>= 8;
    buffer[1] = c & 0xFF; c >>= 8;
    buffer[0] = c;
    return 4;
  } else
    return encoding_error(env, stream, buffer, c);
}

/*
 * UTF-32 LITTLE ENDIAN
 */

static mkcl_character
utf_32le_decoder(MKCL, mkcl_object stream, mkcl_eformat_read_octet read_octet, mkcl_object source)
{
  unsigned char buffer[4] = { '\0', '\0', '\0', '\0' };
  mkcl_index nb_ch = 0;

  if ((nb_ch = read_octet(env, source, buffer, 4)) < 4) {
    if (nb_ch)
      return decoding_error(env, stream, buffer, 4);
    else    
      return EOF;
  } else {
    return buffer[0] | (buffer[1]<<8) | (buffer[2]<<16) | (buffer[3]<<24);
  }
}

static mkcl_index
utf_32le_encoder(MKCL, mkcl_object stream, unsigned char *buffer, mkcl_character c)
{
  if (c <= 0x10FFFF) {
    buffer[0] = c & 0xFF; c >>= 8;
    buffer[1] = c & 0xFF; c >>= 8;
    buffer[2] = c & 0xFF; c >>= 8;
    buffer[3] = c;
    return 4;
  } else
    return encoding_error(env, stream, buffer, c);
}

/*
 * UTF-32 BOM ENDIAN
 */

static mkcl_character
utf_32_decoder(MKCL, mkcl_object stream, mkcl_eformat_read_octet read_octet, mkcl_object source)
{
  mkcl_word c = utf_32be_decoder(env, stream, read_octet, source); /* What about EOF? JCB */
  if (c == 0x0000FEFF) {
    stream->stream.decoder = utf_32be_decoder;
    stream->stream.encoder = utf_32be_encoder;
    return utf_32be_decoder(env, stream, read_octet, source);
  } else if (c == 0xFFFE0000) {
    stream->stream.decoder = utf_32le_decoder;
    stream->stream.encoder = utf_32le_encoder;
    return utf_32le_decoder(env, stream, read_octet, source);
  } else {
    stream->stream.decoder = utf_32be_decoder;
    stream->stream.encoder = utf_32be_encoder;
    return c;
  }
}

static mkcl_index
utf_32_encoder(MKCL, mkcl_object stream, unsigned char *buffer, mkcl_character c)
{
  stream->stream.decoder = utf_32be_decoder; /* This way an unspecified UTF-32 stream always goes big-endian. */
  stream->stream.encoder = utf_32be_encoder;
  /* Output a big-endian BOM. */
  buffer[0] = 0xFF;
  buffer[1] = 0xFE;
  buffer[2] = buffer[3] = 0;
  return 4 + utf_32be_encoder(env, stream, buffer+4, c);
}


/*
 * UTF-16 BIG ENDIAN
 */

static mkcl_character
utf_16be_decoder(MKCL, mkcl_object stream, mkcl_eformat_read_octet read_octet, mkcl_object source)
{
  unsigned char buffer[2] = { '\0', '\0' };
  mkcl_index nb_ch = 0;

  if ((nb_ch = read_octet(env, source, buffer, 2)) < 2) {
    if (nb_ch)
      return decoding_error(env, stream, buffer, 2);
    else
      return EOF;
  } else {
    mkcl_character c = ((mkcl_character)buffer[0] << 8) | buffer[1];

    if ((buffer[0] & 0xFC) == 0xD8) {
      if ((nb_ch = read_octet(env, source, buffer, 2)) < 2) {
	if (nb_ch)
	  return decoding_error(env, stream, buffer, 2);
	else
	  return EOF;
      } else {
	mkcl_character aux = ((mkcl_character)buffer[0] << 8) | buffer[1];

	if ((buffer[0] & 0xFC) != 0xDC)
	  return decoding_error(env, stream, buffer, 2);
	else
	  return ((c & 0x3FF) << 10) + (aux & 0x3FF) + 0x10000;
      }
    } else {
      return c;
    }
  }
}

static mkcl_index
utf_16be_encoder(MKCL, mkcl_object stream, unsigned char *buffer, mkcl_character c)
{
  if (c < 0x10000)  {
    buffer[1] = c;
    buffer[0] = c >> 8;
    return 2;
  } else if (c <= 0x10FFFF) {
    c -= 0x10000;
    unsigned short high_surrogate = ((c >> 10) & 0x3FF) | 0xD800;
    buffer[1] = high_surrogate;
    buffer[0] = high_surrogate >> 8;
    buffer += 2;
    unsigned short low_surrogate = (c & 0x3FF) | 0xDC00;
    buffer[1] = low_surrogate;
    buffer[0] = low_surrogate >> 8;
    return 4;
  } else
    return encoding_error(env, stream, buffer, c);
}

/*
 * UTF-16 LITTLE ENDIAN
 */

static mkcl_character
utf_16le_decoder(MKCL, mkcl_object stream, mkcl_eformat_read_octet read_octet, mkcl_object source)
{
  unsigned char buffer[2] = { '\0', '\0' };
  mkcl_index nb_ch = 0;

  if ((nb_ch = read_octet(env, source, buffer, 2)) < 2) {
    if (nb_ch)
      return decoding_error(env, stream, buffer, 2);
    else
      return EOF;
  } else {
    mkcl_character c = ((mkcl_character)buffer[1] << 8) | buffer[0];

    if ((buffer[1] & 0xFC) == 0xD8) {
      if ((nb_ch = read_octet(env, source, buffer, 2)) < 2) {
	if (nb_ch)
	  return decoding_error(env, stream, buffer, 2);
	else
	  return EOF;
      } else {
	mkcl_character aux = ((mkcl_character)buffer[1] << 8) | buffer[0];

	if ((buffer[1] & 0xFC) != 0xDC)
	  return decoding_error(env, stream, buffer, 2);
	else
	  return ((c & 0x3FF) << 10) + (aux & 0x3FF) + 0x10000;
      }
    } else {
      return c;
    }
  }
}

static mkcl_index
utf_16le_encoder(MKCL, mkcl_object stream, unsigned char *buffer, mkcl_character c)
{
  if (c < 0x10000)  {
    buffer[0] = c;
    buffer[1] = c >> 8;
    return 2;
  } else if (c <= 0x10FFFF) {
    c -= 0x10000;
    unsigned short high_surrogate = ((c >> 10) & 0x3FF) | 0xD800;
    buffer[0] = high_surrogate;
    buffer[1] = high_surrogate >> 8;
    buffer += 2;
    unsigned short low_surrogate = (c & 0x3FF) | 0xDC00;
    buffer[0] = low_surrogate;
    buffer[1] = low_surrogate >> 8;
    return 4;
  } else
    return encoding_error(env, stream, buffer, c);
}

/*
 * UTF-16 BOM ENDIAN
 */

static mkcl_character
utf_16_decoder(MKCL, mkcl_object stream, mkcl_eformat_read_octet read_octet, mkcl_object source)
{
  mkcl_character c = utf_16be_decoder(env, stream, read_octet, source); /* What about EOF? JCB */
  if (c == 0xFEFF) {
    stream->stream.decoder = utf_16be_decoder;
    stream->stream.encoder = utf_16be_encoder;
    return utf_16be_decoder(env, stream, read_octet, source);
  } else if (c == 0xFFFE) {
    stream->stream.decoder = utf_16le_decoder;
    stream->stream.encoder = utf_16le_encoder;
    return utf_16le_decoder(env, stream, read_octet, source);
  } else {
    /* On MS-Windows, being this much by-the-book is most probably to be looking for trouble. JCB */
    stream->stream.decoder = utf_16be_decoder;
    stream->stream.encoder = utf_16be_encoder;
    return c;
  }
}

static mkcl_index
utf_16_encoder(MKCL, mkcl_object stream, unsigned char *buffer, mkcl_character c)
{
  stream->stream.decoder = utf_16be_decoder; /* This way an unspecified UTF-16 stream always goes big-endian. */
  stream->stream.encoder = utf_16be_encoder;
  /* Output a big-endian BOM. */
  buffer[0] = 0xFF;
  buffer[1] = 0xFE;
  return 2 + utf_16be_encoder(env, stream, buffer+2, c);
}

/*
 * USER DEFINED ENCODINGS. SIMPLE CASE.
 */

static mkcl_character
user_decoder(MKCL, mkcl_object stream, mkcl_eformat_read_octet read_octet, mkcl_object source)
{
  mkcl_object table = stream->stream.format_table;
  mkcl_object character;
  unsigned char buffer[2];
  if (read_octet(env, source, buffer, 1) < 1) {
    return EOF;
  }
  character = mkcl_gethash_safe(env, MKCL_MAKE_FIXNUM(buffer[0]), table, mk_cl_Cnil);
  if (mkcl_Null(character)) {
    return decoding_error(env, stream, buffer, 1);
  }
  if (character == mk_cl_Ct) {
    if (read_octet(env, source, buffer+1, 1) < 1) {
      return EOF;
    } else {
      mkcl_word byte = (buffer[0]<<8) + buffer[1];
      character = mkcl_gethash_safe(env, MKCL_MAKE_FIXNUM(byte), table, mk_cl_Cnil);
      if (mkcl_Null(character)) {
	return decoding_error(env, stream, buffer, 2);
      }
    }
  }
  return MKCL_CHAR_CODE(character);
}

static mkcl_index
user_encoder(MKCL, mkcl_object stream, unsigned char *buffer, mkcl_character c)
{
  mkcl_object byte = mkcl_gethash_safe(env, MKCL_CODE_CHAR(c), stream->stream.format_table, mk_cl_Cnil);
  if (mkcl_Null(byte)) {
    return encoding_error(env, stream, buffer, c);
  } else {
    mkcl_word code = mkcl_fixnum_to_word(byte);
    if (code > 0xFF) {
      buffer[1] = code & 0xFF; code >>= 8;
      buffer[0] = code;
      return 2;
    } else {
      buffer[0] = code;
      return 1;
    }
  }
}

/*
 * USER DEFINED ENCODINGS. SIMPLE CASE.
 */

static mkcl_character
user_multistate_decoder(MKCL, mkcl_object stream, mkcl_eformat_read_octet read_octet, mkcl_object source)
{
  mkcl_object table_list = stream->stream.format_table;
  mkcl_object table = MKCL_CONS_CAR(table_list);
  mkcl_object character;
  mkcl_word i, j;
  unsigned char buffer[ENCODING_BUFFER_MAX_SIZE];
  for (i = j = 0; i < ENCODING_BUFFER_MAX_SIZE; i++) {
    if (read_octet(env, source, buffer+i, 1) < 1) {
      return EOF;
    }
    j = (j << 8) | buffer[i];
    character = mkcl_gethash_safe(env, MKCL_MAKE_FIXNUM(j), table, mk_cl_Cnil);
    if (MKCL_CHARACTERP(character)) {
      return MKCL_CHAR_CODE(character);
    }
    if (mkcl_Null(character)) {
      return decoding_error(env, stream, buffer, i);
    }
    if (character == mk_cl_Ct) {
      /* Need more characters */
      continue;
    }
    if (MKCL_CONSP(character)) {
      /* Changed the state. */
      stream->stream.format_table = table_list = character;
      table = MKCL_CONS_CAR(table_list);
      i = j = 0;
      continue;
    }
    break;
  }
  mkcl_FEerror(env, "Internal error in decoder table.", 0);
}

static mkcl_index
user_multistate_encoder(MKCL, mkcl_object stream, unsigned char *buffer, mkcl_character c)
{
  mkcl_object table_list = stream->stream.format_table;
  mkcl_object p = table_list;
  do {
    mkcl_object table = MKCL_CONS_CAR(p);
    mkcl_object byte = mkcl_gethash_safe(env, MKCL_CODE_CHAR(c), table, mk_cl_Cnil);
    if (!mkcl_Null(byte)) {
      mkcl_word code = mkcl_fixnum_to_word(byte);
      mkcl_index n = 0;
      if (p != table_list) {
	/* Must output a escape sequence */
	mkcl_object x = mkcl_gethash_safe(env, mk_cl_Ct, table, mk_cl_Cnil);
	while (!mkcl_Null(x)) {
	  buffer[0] = mkcl_fixnum_to_word(MKCL_CONS_CAR(x));
	  buffer++;
	  x = MKCL_CONS_CDR(x);
	  n++;
	}
	stream->stream.format_table = p;
      }
      if (code > 0xFF) {
	buffer[1] = code & 0xFF; code >>= 8;
	buffer[0] = code;
	return n+2;
      } else {
	buffer[0] = code;
	return n+1;
      }
    }
    p = MKCL_CONS_CDR(p);
  } while (p != table_list);
  /* Exhausted all lists */
  return encoding_error(env, stream, buffer, c);
}

/*
 * UTF-8
 */

static mkcl_character
utf_8_decoder(MKCL, mkcl_object stream, mkcl_eformat_read_octet read_octet, mkcl_object source)
{
  /* In understanding this code:
   * 0x8 = 1000, 0xC = 1100, 0xE = 1110, 0xF = 1111
   * 0x1 = 0001, 0x3 = 0011, 0x7 = 0111, 0xF = 1111
   */
  mkcl_character cum = 0;
  unsigned char buffer[4] = { '\0', '\0', '\0', '\0' };
  mkcl_index nbytes, i;

  if (read_octet(env, source, buffer, 1) < 1)
    return EOF;

  cum = buffer[0];
  if ((cum & 0x80) == 0) {
    return cum;
  }
  if ((cum & 0x40) == 0)
    return decoding_error(env, stream, buffer, 1);
  if ((cum & 0x20) == 0) {
    cum &= 0x1F;
    nbytes = 1;
  } else if ((cum & 0x10) == 0) {
    cum &= 0x0F;
    nbytes = 2;
  } else if ((cum & 0x08) == 0) {
    cum &= 0x07;
    nbytes = 3;
  } else {
    return decoding_error(env, stream, buffer, 1);
  }
  if (read_octet(env, source, buffer+1, nbytes) < nbytes)
    return EOF;

  switch (nbytes)
    {
    case 1:
      {
	unsigned char c = buffer[1];

	if ((c & 0xC0) != 0x80) /* not continuation byte? */
	  return decoding_error(env, stream, buffer, nbytes+1);
	if (cum == 0)  /* illegal overlong sequence? */
	  return decoding_error(env, stream, buffer, nbytes+1);
	cum = (cum << 6) | (c & 0x3F);
      }
      break;
    case 2:
      {
	unsigned char c1 = buffer[1];
	unsigned char c2 = buffer[2];

	if (((c1 & 0xC0) != 0x80) || ((c2 & 0xC0) != 0x80))  /* not continuation byte? */
	  return decoding_error(env, stream, buffer, nbytes+1);
	if ((cum == 0) && ((c1 & 0x20) == 0))  /* illegal overlong sequence? */
	  return decoding_error(env, stream, buffer, nbytes+1);
	cum = (cum << 12) | ((c1 & 0x3F) << 6) | (c2 & 0x3F);
      }
      break;
    case 3:
      {
	unsigned char c1 = buffer[1];
	unsigned char c2 = buffer[2];
	unsigned char c3 = buffer[3];

	if (((c1 & 0xC0) != 0x80) || ((c2 & 0xC0) != 0x80) || ((c3 & 0xC0) != 0x80))  /* not continuation byte? */
	  return decoding_error(env, stream, buffer, nbytes+1);
	if ((cum == 0) && ((c1 & 0x30) == 0))  /* illegal overlong sequence? */
	  return decoding_error(env, stream, buffer, nbytes+1);
	cum = (cum << 18) | ((c1 & 0x3F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F);
      }
      break;
    }

  if (cum == 0) return decoding_error(env, stream, buffer, nbytes+1);
  if (cum >= 0xd800) {
    if (cum <= 0xdfff)
      return decoding_error(env, stream, buffer, nbytes+1);
    if (cum >= 0xFFFE && cum <= 0xFFFF)
      return decoding_error(env, stream, buffer, nbytes+1);
  }
  /*printf("; %04x ;", cum);*/
  return cum;
}

static mkcl_index
utf_8_encoder(MKCL, mkcl_object stream, unsigned char *buffer, mkcl_character c)
{
  mkcl_index nbytes;

  if (c <= 0x7F) {
    buffer[0] = c;
    nbytes = 1;
  } else if (c <= 0x7FF) {
    buffer[1] = (c & 0x3f) | 0x80; c >>= 6;
    buffer[0] = c | 0xC0;
    /*printf("\n; %04x ;: %04x :: %04x :\n", c_orig, buffer[0], buffer[1]);*/
    nbytes = 2;
  } else if (c <= 0xD7FF || (c > 0xDFFF && c < 0xFFFE)) {
    /* This here would have happily produced invalid UTF-8 for UFFF? or UD8??-UDF?? */
    buffer[2] = (c & 0x3f) | 0x80; c >>= 6;
    buffer[1] = (c & 0x3f) | 0x80; c >>= 6;
    buffer[0] = c | 0xE0;
    nbytes = 3;
  } else if (c <= 0xFFFF) {
    nbytes = encoding_error(env, stream, buffer, c);
  } else if (c <= 0x10FFFF) {
    buffer[3] = (c & 0x3f) | 0x80; c >>= 6;
    buffer[2] = (c & 0x3f) | 0x80; c >>= 6;
    buffer[1] = (c & 0x3f) | 0x80; c >>= 6;
    buffer[0] = c | 0xF0;
    nbytes = 4;
  }
  else
    nbytes = encoding_error(env, stream, buffer, c);
  return nbytes;
}

/********************************************************************************
 * CLOS STREAMS
 */

static mkcl_index
mk_clos_stream_read_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  mkcl_index i;
  for (i = 0; i < n; i++) {
    mkcl_object byte = mkcl_funcall1(env, @+'gray::stream-read-byte', strm);
    if (!MKCL_FIXNUMP(byte))
      break;
    c[i] = mkcl_fixnum_to_word(byte);
  }
  return i;
}

static mkcl_index
mk_clos_stream_write_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  mkcl_index i;
  for (i = 0; i < n; i++) {
    mkcl_object byte = mkcl_funcall2(env, @+'gray::stream-write-byte', strm, MKCL_MAKE_FIXNUM(c[i]));
    if (!MKCL_FIXNUMP(byte))
      break;
  }
  return i;
}

static mkcl_object
mk_clos_stream_read_byte(MKCL, mkcl_object strm)
{
  return mkcl_funcall1(env, @+'gray::stream-read-byte', strm);
}

static void
mk_clos_stream_write_byte(MKCL, mkcl_object c, mkcl_object strm)
{
  mkcl_funcall2(env, @+'gray::stream-write-byte', strm, c);
}

static mkcl_character
mk_clos_stream_read_char(MKCL, mkcl_object strm)
{
  mkcl_object output = mkcl_funcall1(env, @+'gray::stream-read-char', strm);
  mkcl_word value;
  if (MKCL_CHARACTERP(output))
    value = MKCL_CHAR_CODE(output);
  else if (MKCL_FIXNUMP(output))
    value = mkcl_fixnum_to_word(output);
  else if (output == mk_cl_Cnil)
    return EOF;
  else
    value = -1;
  if (value < 0 || value > MKCL_CHAR_CODE_LIMIT)
    mkcl_FEerror(env, "Unknown character ~A", 1, output);
  return value;
}

static mkcl_character
mk_clos_stream_write_char(MKCL, mkcl_object strm, mkcl_character c)
{
  mkcl_funcall2(env, @+'gray::stream-write-char', strm, MKCL_CODE_CHAR(c));
  return c;
}

static void
mk_clos_stream_unread_char(MKCL, mkcl_object strm, mkcl_character c)
{
  mkcl_funcall2(env, @+'gray::stream-unread-char', strm, MKCL_CODE_CHAR(c));
}

static mkcl_character
mk_clos_stream_peek_char(MKCL, mkcl_object strm)
{
  mkcl_object out = mkcl_funcall1(env, @+'gray::stream-peek-char', strm);
  if (out == @':eof') return EOF;
  return mkcl_char_code(env, out);
}

static int
mk_clos_stream_listen(MKCL, mkcl_object strm)
{
  return !mkcl_Null(mkcl_funcall1(env, @+'gray::stream-listen', strm));
}

static void
mk_clos_stream_clear_input(MKCL, mkcl_object strm)
{
  mkcl_funcall1(env, @+'gray::stream-clear-input', strm);
}

static void
mk_clos_stream_clear_output(MKCL, mkcl_object strm)
{
  mkcl_funcall1(env, @+'gray::stream-clear-output', strm);
}

static void
mk_clos_stream_force_output(MKCL, mkcl_object strm)
{
  mkcl_funcall1(env, @+'gray::stream-force-output', strm);
}

static void
mk_clos_stream_finish_output(MKCL, mkcl_object strm)
{
  mkcl_funcall1(env, @+'gray::stream-finish-output', strm);
}

static bool
mk_clos_stream_input_p(MKCL, mkcl_object strm)
{
  return !mkcl_Null(mkcl_funcall1(env, @+'gray::input-stream-p', strm));
}

static bool
mk_clos_stream_output_p(MKCL, mkcl_object strm)
{
  return !mkcl_Null(mkcl_funcall1(env, @+'gray::output-stream-p', strm));
}

static bool
mk_clos_stream_interactive_p(MKCL, mkcl_object strm)
{
  return !mkcl_Null(mkcl_funcall1(env, @+'gray::stream-interactive-p', strm));

}

static mkcl_object
mk_clos_stream_element_type(MKCL, mkcl_object strm)
{
  return mkcl_funcall1(env, @+'gray::stream-element-type', strm);
}

#define mk_clos_stream_length not_a_file_stream
#define mk_clos_stream_get_position not_implemented_get_position
#define mk_clos_stream_set_position not_implemented_set_position

static int
mk_clos_stream_column(MKCL, mkcl_object strm)
{
  mkcl_object col = mkcl_funcall1(env, @+'gray::stream-line-column', strm);
  /* FIXME! The Gray streams specifies NIL is a valid
   * value but means "unknown". Should we make it
   * zero? */
  return mkcl_Null(col) ? 0 : mkcl_integer_to_index(env, col);
}

static mkcl_object
mk_clos_stream_close(MKCL, mkcl_object strm)
{
  return mkcl_funcall1(env, @+'gray::close', strm);
}

static const struct mkcl_file_ops mk_clos_stream_ops = {
  mk_clos_stream_write_octet,
  mk_clos_stream_read_octet,

  mk_clos_stream_write_byte,
  mk_clos_stream_read_byte,

  mk_clos_stream_read_char,
  mk_clos_stream_write_char,
  mk_clos_stream_unread_char,
  mk_clos_stream_peek_char,

  generic_read_vector,
  generic_write_vector,

  mk_clos_stream_listen,
  mk_clos_stream_clear_input,
  mk_clos_stream_clear_output,
  mk_clos_stream_finish_output,
  mk_clos_stream_force_output,

  mk_clos_stream_input_p,
  mk_clos_stream_output_p,
  mk_clos_stream_interactive_p,
  mk_clos_stream_element_type,

  mk_clos_stream_length,
  mk_clos_stream_get_position,
  mk_clos_stream_set_position,
  mk_clos_stream_column,
  mk_clos_stream_close
};

/**********************************************************************
 * STRING OUTPUT STREAMS
 */

static mkcl_character
str_out_write_char(MKCL, mkcl_object strm, mkcl_character c)
{
  mkcl_word column = MKCL_STRING_OUTPUT_STREAM_COLUMN(strm);
  mkcl_eformat_encoder encoder = strm->stream.encoder;

  if (encoder)
    {
      unsigned char buffer[ENCODING_BUFFER_MAX_SIZE];
      mkcl_index i, nbytes = encoder(env, strm, buffer, c);
      if (nbytes == 0) {
	character_size_overflow(env, strm, c);
      }
      for (i = 0; i < nbytes; i++)
	mkcl_string_push_extend(env, MKCL_STRING_OUTPUT_STREAM_STRING(strm), buffer[i]);
    }
  else
    {
      mkcl_string_push_extend(env, MKCL_STRING_OUTPUT_STREAM_STRING(strm), c);
    }

  if (c == '\n')
    MKCL_STRING_OUTPUT_STREAM_COLUMN(strm) = 0;
  else if (c == '\t')
    MKCL_STRING_OUTPUT_STREAM_COLUMN(strm) = (column&~((mkcl_word)07)) + 8;
  else
    MKCL_STRING_OUTPUT_STREAM_COLUMN(strm) = column+1;
  return c;
}

static mkcl_object
str_out_element_type(MKCL, mkcl_object strm)
{
  mkcl_object string = MKCL_STRING_OUTPUT_STREAM_STRING(strm);
  if (mkcl_type_of(string) == mkcl_t_base_string)
    return @'base-char';
  return @'character';
}

static mkcl_object
str_out_get_position(MKCL, mkcl_object strm)
{
  return mkcl_make_unsigned_integer(env, MKCL_STRING_OUTPUT_STREAM_STRING(strm)->base_string.fillp);
}

static mkcl_object
str_out_set_position(MKCL, mkcl_object strm, mkcl_object pos)
{
  mkcl_object string = MKCL_STRING_OUTPUT_STREAM_STRING(strm);
  mkcl_word disp;
  if (mkcl_Null(pos)) {
    disp = strm->base_string.dim;
  } else {
    disp = mkcl_integer_to_index(env, pos);
  }
  if (disp < string->base_string.fillp) {
    string->base_string.fillp = disp;
  } else {
    disp -= string->base_string.fillp;
    while (disp-- > 0)
      mkcl_write_char(env, ' ', strm);
  }
  return mk_cl_Ct;
}

static int
str_out_column(MKCL, mkcl_object strm)
{
  return MKCL_STRING_OUTPUT_STREAM_COLUMN(strm);
}

static const struct mkcl_file_ops str_out_ops = {
  not_output_write_octet,
  not_binary_read_octet,

  not_binary_write_byte,
  not_input_read_byte,

  not_input_read_char,
  str_out_write_char,
  not_input_unread_char,
  generic_peek_char,

  generic_read_vector,
  generic_write_vector,

  not_input_listen,
  not_input_clear_input,
  generic_void, /* clear-output */
  generic_void, /* finish-output */
  generic_void, /* force-output */

  generic_always_false, /* input_p */
  generic_always_true, /* output_p */
  generic_always_false,
  str_out_element_type,

  not_a_file_stream, /* length */
  str_out_get_position,
  str_out_set_position,
  str_out_column,
  generic_close
};

mkcl_object
mk_si_make_string_output_stream_from_string(MKCL, mkcl_object s, mkcl_object encoding)
{
  mkcl_call_stack_check(env);
  mkcl_object strm = alloc_stream(env);
  if (!mkcl_stringp(env, s) || !s->base_string.hasfillp)
    mkcl_FEerror(env, "~S is not a string with a fill-pointer.", 1, s);
  strm->stream.ops = duplicate_dispatch_table(env, &str_out_ops);
  strm->stream.mode = (short)mkcl_smm_string_output;
  MKCL_STRING_OUTPUT_STREAM_STRING(strm) = s;
  MKCL_STRING_OUTPUT_STREAM_COLUMN(strm) = 0;
  if (mkcl_type_of(s) == mkcl_t_base_string) {
    if (encoding == @':UTF-8')
      {
	strm->stream.format = @':UTF-8';
	strm->stream.encoder = utf_8_encoder;
	strm->stream.decoder = utf_8_decoder;
      }
    else
      strm->stream.format = @':ISO-8859-1';
    strm->stream.flags = MKCL_STREAM_TEXT;
    strm->stream.byte_size = 8;
  } else {
    strm->stream.format = @':UTF-32';
    strm->stream.flags = MKCL_STREAM_TEXT;
    strm->stream.byte_size = 32;
  }
  @(return strm);
}

mkcl_object
mkcl_make_string_output_stream(MKCL, mkcl_index line_length, bool extended, mkcl_object encoding)
{
  mkcl_object s = (extended 
		   ? mkcl_alloc_adjustable_extended_string(env, line_length)
		   : mkcl_alloc_adjustable_base_string(env, line_length));

  return mk_si_make_string_output_stream_from_string(env, s, encoding);
}

#define DEFAULT_OUTPUT_STRING_LENGTH 128

@(defun make-string-output-stream (&key (element_type @'character') (encoding @':default'))
  bool extended = FALSE;
@
  {
    if (element_type == @'base-char') {
      extended = FALSE;
    } else if (element_type == @'character') {
      extended = TRUE;
      encoding = @':UTF-32';
    } else if (!mkcl_Null(mkcl_funcall2(env, @+'subtypep', element_type, @'base-char'))) {
      extended = FALSE;
    } else if (!mkcl_Null(mkcl_funcall2(env, @+'subtypep', element_type, @'character'))) {
      extended = TRUE;
      encoding = @':UTF-32';
    } else {
      mkcl_FEerror(env,
		   "In MAKE-STRING-OUTPUT-STREAM, the argument :ELEMENT-TYPE "
		   "(~A) must be a subtype of character",
		   1, element_type);
    }
    @(return mkcl_make_string_output_stream(env, DEFAULT_OUTPUT_STRING_LENGTH, extended, encoding));
  }
@)

mkcl_object
mk_cl_get_output_stream_string(MKCL, mkcl_object strm)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(strm) != mkcl_t_stream || (enum mkcl_smmode)strm->stream.mode != mkcl_smm_string_output)
    mkcl_FEerror(env, "~S is not a string-output stream.", 1, strm);

  mkcl_object strng = MKCL_STRING_OUTPUT_STREAM_STRING(strm);
  mkcl_object new = (mkcl_type_of(strng) == mkcl_t_string
		     ? mkcl_alloc_adjustable_extended_string(env, DEFAULT_OUTPUT_STRING_LENGTH)
		     : mkcl_alloc_adjustable_base_string(env, DEFAULT_OUTPUT_STRING_LENGTH));

  MKCL_STRING_OUTPUT_STREAM_STRING(strm) = new;
  @(return strng);
}

/**********************************************************************
 * STRING INPUT STREAMS
 */

static mkcl_index
str_in_read_octet(MKCL, mkcl_object strm, unsigned char * c, mkcl_index n)
{
  mkcl_index out = 0;
  mkcl_word curr_pos = MKCL_STRING_INPUT_STREAM_POSITION(strm);
  for (out = 0; out < n; out++)
    {
      if (curr_pos >= MKCL_STRING_INPUT_STREAM_LIMIT(strm)) {
	return out;
      } else {
	c[out] = mkcl_char(env, MKCL_STRING_INPUT_STREAM_STRING(strm), curr_pos);
	MKCL_STRING_INPUT_STREAM_POSITION(strm) = (curr_pos += 1);
      }      
    }
  return out;
}

static mkcl_character
str_in_read_char(MKCL, mkcl_object strm)
{
  mkcl_character c;
  mkcl_eformat_decoder decoder = strm->stream.decoder;

  if (decoder)
    {
      c = decoder(env, strm, str_in_read_octet, strm);
      if (c != EOF) {
	strm->stream.last_char = c;
	strm->stream.last_code[0] = c;
	strm->stream.last_code[1] = EOF;
      }
    }
  else
    {
      mkcl_word curr_pos = MKCL_STRING_INPUT_STREAM_POSITION(strm);
      if (curr_pos >= MKCL_STRING_INPUT_STREAM_LIMIT(strm)) {
	c = EOF;
      } else {
	c = mkcl_char(env, MKCL_STRING_INPUT_STREAM_STRING(strm), curr_pos);
	MKCL_STRING_INPUT_STREAM_POSITION(strm) = curr_pos+1;
      }
    }

  return c;
}

static void
str_in_unread_char(MKCL, mkcl_object strm, mkcl_character c)
{
  mkcl_eformat_encoder encoder = strm->stream.encoder;

  if (encoder)
    {
      if (c != strm->stream.last_char) {
	unread_twice(env, strm);
      } else {
	unsigned char buffer[2*ENCODING_BUFFER_MAX_SIZE];
	mkcl_index nbytes = encoder(env, strm, buffer, c);
	mkcl_word curr_pos = MKCL_STRING_INPUT_STREAM_POSITION(strm);

	if (curr_pos < nbytes) {
	  unread_error(env, strm);
	}
	MKCL_STRING_INPUT_STREAM_POSITION(strm) = curr_pos - nbytes;
	strm->stream.last_char = EOF;
      }
    }
  else
    {
      mkcl_word curr_pos = MKCL_STRING_INPUT_STREAM_POSITION(strm);
      if (curr_pos <= 0) {
	unread_error(env, strm);
      }
      MKCL_STRING_INPUT_STREAM_POSITION(strm) = curr_pos - 1;
    }
}

static mkcl_character
str_in_peek_char(MKCL, mkcl_object strm)
{
  mkcl_index pos = MKCL_STRING_INPUT_STREAM_POSITION(strm);
  if (pos >= MKCL_STRING_INPUT_STREAM_LIMIT(strm)) {
    return EOF;
  } else {
    return mkcl_char(env, MKCL_STRING_INPUT_STREAM_STRING(strm), pos);
  }
}

static int
str_in_listen(MKCL, mkcl_object strm)
{
  if (MKCL_STRING_INPUT_STREAM_POSITION(strm) < MKCL_STRING_INPUT_STREAM_LIMIT(strm))
    return MKCL_LISTEN_AVAILABLE;
  else
    return MKCL_LISTEN_EOF;
}

static mkcl_object
str_in_element_type(MKCL, mkcl_object strm)
{
  mkcl_object string = MKCL_STRING_INPUT_STREAM_STRING(strm);
  if (mkcl_type_of(string) == mkcl_t_base_string)
    return @'base-char';
  return @'character';
}

static mkcl_object
str_in_get_position(MKCL, mkcl_object strm)
{
  return mkcl_make_unsigned_integer(env, MKCL_STRING_INPUT_STREAM_POSITION(strm));
}

static mkcl_object
str_in_set_position(MKCL, mkcl_object strm, mkcl_object pos)
{
  mkcl_word disp;
  if (mkcl_Null(pos)) {
    disp = MKCL_STRING_INPUT_STREAM_LIMIT(strm);
  }  else {
    disp = mkcl_integer_to_index(env, pos);
    if (disp >= MKCL_STRING_INPUT_STREAM_LIMIT(strm)) {
      disp = MKCL_STRING_INPUT_STREAM_LIMIT(strm);
    }
  }
  MKCL_STRING_INPUT_STREAM_POSITION(strm) = disp;
  return mk_cl_Ct;
}

static const struct mkcl_file_ops str_in_ops = {
  not_output_write_octet,
  not_binary_read_octet,

  not_output_write_byte,
  not_binary_read_byte,

  str_in_read_char,
  not_output_write_char,
  str_in_unread_char,
  str_in_peek_char,

  generic_read_vector,
  generic_write_vector,

  str_in_listen,
  generic_void, /* clear-input */
  not_output_clear_output,
  not_output_finish_output,
  not_output_force_output,

  generic_always_true, /* input_p */
  generic_always_false, /* output_p */
  generic_always_false,
  str_in_element_type,

  not_a_file_stream, /* length */
  str_in_get_position,
  str_in_set_position,
  generic_column,
  generic_close
};

mkcl_object
mkcl_make_string_input_stream(MKCL, mkcl_object strng, mkcl_index istart, mkcl_index iend, mkcl_object encoding)
{
  mkcl_object strm;

  strm = alloc_stream(env);
  strm->stream.ops = duplicate_dispatch_table(env, &str_in_ops);
  strm->stream.mode = (short)mkcl_smm_string_input;
  MKCL_STRING_INPUT_STREAM_STRING(strm) = strng;
  MKCL_STRING_INPUT_STREAM_POSITION(strm) = istart;
  MKCL_STRING_INPUT_STREAM_LIMIT(strm) = iend;
  if (mkcl_type_of(strng) == mkcl_t_base_string) {
    if (encoding == @':UTF-8')
      {
	strm->stream.format = @':UTF-8';
	strm->stream.encoder = utf_8_encoder;
	strm->stream.decoder = utf_8_decoder;
      }
    else
      strm->stream.format = @':ISO-8859-1';
    strm->stream.flags = MKCL_STREAM_TEXT;
    strm->stream.byte_size = 8;
  } else {
    strm->stream.format = @':UTF-32';
    strm->stream.flags = MKCL_STREAM_TEXT;
    strm->stream.byte_size = 32;
  }
  return strm;
}

@(defun make_string_input_stream (strng &o (istart MKCL_MAKE_FIXNUM(0)) (iend mk_cl_Cnil) &key (encoding @':default'))
  mkcl_index s, e;
@
  strng = mk_cl_string(env, strng);
  if (!MKCL_FIXNUMP(istart) || MKCL_FIXNUM_MINUSP(istart))
    goto E;
  else
    s = (mkcl_index)mkcl_fixnum_to_word(istart);
  if (mkcl_Null(iend))
    e = strng->base_string.fillp;
  else if (!MKCL_FIXNUMP(iend) || MKCL_FIXNUM_MINUSP(iend))
    goto E;
  else
    e = (mkcl_index)mkcl_fixnum_to_word(iend);
  if (e > strng->base_string.fillp || s > e)
    goto E;
  @(return (mkcl_make_string_input_stream(env, strng, s, e, encoding)));

 E:
  mkcl_FEerror(env,
	       "~S and ~S are illegal as :START and :END~%"
	       "for the string ~S.",
	       3, istart, iend, strng);
@)

/**********************************************************************
 * TWO WAY STREAM
 */

static mkcl_index
two_way_read_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  if (strm == mkcl_core.terminal_io)
    mkcl_force_output(env, MKCL_TWO_WAY_STREAM_OUTPUT(mkcl_core.terminal_io));
  return mkcl_read_octet(env, MKCL_TWO_WAY_STREAM_INPUT(strm), c, n);
}

static mkcl_index
two_way_write_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  return mkcl_write_octet(env, MKCL_TWO_WAY_STREAM_OUTPUT(strm), c, n);
}

static void
two_way_write_byte(MKCL, mkcl_object byte, mkcl_object stream)
{
  mkcl_write_byte(env, byte, MKCL_TWO_WAY_STREAM_OUTPUT(stream));
}

static mkcl_object
two_way_read_byte(MKCL, mkcl_object stream)
{
  return mkcl_read_byte(env, MKCL_TWO_WAY_STREAM_INPUT(stream));
}

static mkcl_character
two_way_read_char(MKCL, mkcl_object strm)
{
  return mkcl_read_char(env, MKCL_TWO_WAY_STREAM_INPUT(strm));
}

static mkcl_character
two_way_write_char(MKCL, mkcl_object strm, mkcl_character c)
{
  return mkcl_write_char(env, c, MKCL_TWO_WAY_STREAM_OUTPUT(strm));
}

static void
two_way_unread_char(MKCL, mkcl_object strm, mkcl_character c)
{
  mkcl_unread_char(env, c, MKCL_TWO_WAY_STREAM_INPUT(strm));
}

static mkcl_character
two_way_peek_char(MKCL, mkcl_object strm)
{
  return mkcl_peek_char(env, MKCL_TWO_WAY_STREAM_INPUT(strm));
}

static mkcl_index
two_way_read_vector(MKCL, mkcl_object strm, mkcl_object data, mkcl_index start, mkcl_index n)
{
  strm = MKCL_TWO_WAY_STREAM_INPUT(strm);
  return stream_dispatch_table(env, strm)->read_vector(env, strm, data, start, n);
}

static mkcl_index
two_way_write_vector(MKCL, mkcl_object strm, mkcl_object data, mkcl_index start, mkcl_index n)
{
  strm = MKCL_TWO_WAY_STREAM_OUTPUT(strm);
  return stream_dispatch_table(env, strm)->write_vector(env, strm, data, start, n);
}

static int
two_way_listen(MKCL, mkcl_object strm)
{
  return mkcl_listen_stream(env, MKCL_TWO_WAY_STREAM_INPUT(strm));
}

static void
two_way_clear_input(MKCL, mkcl_object strm)
{
  mkcl_clear_input(env, MKCL_TWO_WAY_STREAM_INPUT(strm));
}

static void
two_way_clear_output(MKCL, mkcl_object strm)
{
  mkcl_clear_output(env, MKCL_TWO_WAY_STREAM_OUTPUT(strm));
}

static void
two_way_force_output(MKCL, mkcl_object strm)
{
  mkcl_force_output(env, MKCL_TWO_WAY_STREAM_OUTPUT(strm));
}

static void
two_way_finish_output(MKCL, mkcl_object strm)
{
  mkcl_finish_output(env, MKCL_TWO_WAY_STREAM_OUTPUT(strm));
}

static bool
two_way_interactive_p(MKCL, mkcl_object strm)
{
  return mkcl_interactive_stream_p(env, MKCL_TWO_WAY_STREAM_INPUT(strm));
}

static mkcl_object
two_way_element_type(MKCL, mkcl_object strm)
{
  return mkcl_stream_element_type(env, MKCL_TWO_WAY_STREAM_INPUT(strm));
}

static int
two_way_column(MKCL, mkcl_object strm)
{
  return mkcl_file_column(env, MKCL_TWO_WAY_STREAM_OUTPUT(strm));
}


static const struct mkcl_file_ops two_way_ops = {
  two_way_write_octet,
  two_way_read_octet,

  two_way_write_byte,
  two_way_read_byte,

  two_way_read_char,
  two_way_write_char,
  two_way_unread_char,
  two_way_peek_char,

  two_way_read_vector,
  two_way_write_vector,

  two_way_listen,
  two_way_clear_input,
  two_way_clear_output,
  two_way_finish_output,
  two_way_force_output,

  generic_always_true, /* input_p */
  generic_always_true, /* output_p */
  two_way_interactive_p,
  two_way_element_type,

  not_a_file_stream, /* length */
  generic_always_nil, /* get_position */
  generic_set_position,
  two_way_column,
  generic_close
};


mkcl_object
mk_cl_make_two_way_stream(MKCL, mkcl_object istrm, mkcl_object ostrm)
{
  mkcl_object strm;

  mkcl_call_stack_check(env);
  if (!mkcl_input_stream_p(env, istrm))
    not_an_input_stream(env, istrm);
  if (!mkcl_output_stream_p(env, ostrm))
    not_an_output_stream(env, ostrm);
  strm = alloc_stream(env);
  strm->stream.format = mk_cl_stream_external_format(env, istrm);
  strm->stream.mode = mkcl_smm_two_way;
  strm->stream.ops = duplicate_dispatch_table(env, &two_way_ops);
  MKCL_TWO_WAY_STREAM_INPUT(strm) = istrm;
  MKCL_TWO_WAY_STREAM_OUTPUT(strm) = ostrm;
  @(return strm);
}

mkcl_object
mk_cl_two_way_stream_input_stream(MKCL, mkcl_object strm)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(strm) != mkcl_t_stream || strm->stream.mode != mkcl_smm_two_way)
    mkcl_FEwrong_type_argument(env, @'two-way-stream', strm);
  @(return MKCL_TWO_WAY_STREAM_INPUT(strm));
}

mkcl_object
mk_cl_two_way_stream_output_stream(MKCL, mkcl_object strm)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(strm) != mkcl_t_stream || strm->stream.mode != mkcl_smm_two_way)
    mkcl_FEwrong_type_argument(env, @'two-way-stream', strm);
  @(return MKCL_TWO_WAY_STREAM_OUTPUT(strm));
}

/**********************************************************************
 * BROADCAST STREAM
 */

static mkcl_index
broadcast_write_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  mkcl_object l;
  mkcl_index out = n;
  for (l = MKCL_BROADCAST_STREAM_LIST(strm); !mkcl_Null(l); l = MKCL_CONS_CDR(l)) {
    out = mkcl_write_octet(env, MKCL_CONS_CAR(l), c, n);
  }
  return out;
}

static mkcl_character
broadcast_write_char(MKCL, mkcl_object strm, mkcl_character c)
{
  mkcl_object l;
  for (l = MKCL_BROADCAST_STREAM_LIST(strm); !mkcl_Null(l); l = MKCL_CONS_CDR(l)) {
    mkcl_write_char(env, c, MKCL_CONS_CAR(l));
  }
  return c;
}

static void
broadcast_write_byte(MKCL, mkcl_object c, mkcl_object strm)
{
  mkcl_object l;
  for (l = MKCL_BROADCAST_STREAM_LIST(strm); !mkcl_Null(l); l = MKCL_CONS_CDR(l)) {
    mkcl_write_byte(env, c, MKCL_CONS_CAR(l));
  }
}

static void
broadcast_clear_output(MKCL, mkcl_object strm)
{
  mkcl_object l;
  for (l = MKCL_BROADCAST_STREAM_LIST(strm); !mkcl_Null(l); l = MKCL_CONS_CDR(l)) {
    mkcl_clear_output(env, MKCL_CONS_CAR(l));
  }
}

static void
broadcast_force_output(MKCL, mkcl_object strm)
{
  mkcl_object l;
  for (l = MKCL_BROADCAST_STREAM_LIST(strm); !mkcl_Null(l); l = MKCL_CONS_CDR(l)) {
    mkcl_force_output(env, MKCL_CONS_CAR(l));
  }
}

static void
broadcast_finish_output(MKCL, mkcl_object strm)
{
  mkcl_object l;
  for (l = MKCL_BROADCAST_STREAM_LIST(strm); !mkcl_Null(l); l = MKCL_CONS_CDR(l)) {
    mkcl_finish_output(env, MKCL_CONS_CAR(l));
  }
}

static mkcl_object
broadcast_element_type(MKCL, mkcl_object strm)
{
  mkcl_object l = MKCL_BROADCAST_STREAM_LIST(strm);
  if (mkcl_Null(l))
    return mk_cl_Ct;
  return mkcl_stream_element_type(env, MKCL_CONS_CAR(l));
}

static mkcl_object
broadcast_length(MKCL, mkcl_object strm)
{
  mkcl_object l = MKCL_BROADCAST_STREAM_LIST(strm);
  if (mkcl_Null(l))
    return MKCL_MAKE_FIXNUM(0);
  return mkcl_file_length(env, MKCL_CONS_CAR(l));
}

static mkcl_object
broadcast_get_position(MKCL, mkcl_object strm)
{
  mkcl_object l = MKCL_BROADCAST_STREAM_LIST(strm);
  if (mkcl_Null(l))
    return MKCL_MAKE_FIXNUM(0);
  return mkcl_file_position(env, MKCL_CONS_CAR(l));
}

static mkcl_object
broadcast_set_position(MKCL, mkcl_object strm, mkcl_object pos)
{
  mkcl_object l = MKCL_BROADCAST_STREAM_LIST(strm);
  if (mkcl_Null(l))
    return mk_cl_Cnil;
  return mkcl_file_position_set(env, MKCL_CONS_CAR(l), pos);
}

static int
broadcast_column(MKCL, mkcl_object strm)
{
  mkcl_object l = MKCL_BROADCAST_STREAM_LIST(strm);
  if (mkcl_Null(l))
    return 0;
  return mkcl_file_column(env, MKCL_CONS_CAR(l));
}

static const struct mkcl_file_ops broadcast_ops = {
  broadcast_write_octet,
  not_input_read_octet,

  broadcast_write_byte,
  not_input_read_byte,

  not_input_read_char,
  broadcast_write_char,
  not_input_unread_char,
  generic_peek_char,

  generic_read_vector,
  generic_write_vector,

  not_input_listen,
  broadcast_force_output, /* clear_input */ /* FIXME! This is legacy behaviour */
  broadcast_clear_output,
  broadcast_finish_output,
  broadcast_force_output,

  generic_always_false, /* input_p */
  generic_always_true, /* output_p */
  generic_always_false,
  broadcast_element_type,

  broadcast_length,
  broadcast_get_position,
  broadcast_set_position,
  broadcast_column,
  generic_close
};

@(defun make_broadcast_stream (&rest ap)
  mkcl_object x, streams;
  int i;
@
  streams = mk_cl_Cnil;
  for (i = 0; i < narg; i++) {
    x = mkcl_va_arg(ap);
    if (!mkcl_output_stream_p(env, x))
      not_an_output_stream(env, x);
    streams = MKCL_CONS(env, x, streams);
  }
  x = alloc_stream(env);
  if (mkcl_Null(streams)) {
    x->stream.format = @':default';
  } else {
    x->stream.format = mk_cl_stream_external_format(env, MKCL_CONS_CAR(streams));
  }
  x->stream.ops = duplicate_dispatch_table(env, &broadcast_ops);
  x->stream.mode = mkcl_smm_broadcast;
  MKCL_BROADCAST_STREAM_LIST(x) = mk_cl_nreverse(env, streams);
  @(return x);
@)

mkcl_object
mk_cl_broadcast_stream_streams(MKCL, mkcl_object strm)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(strm) != mkcl_t_stream || strm->stream.mode != mkcl_smm_broadcast)
    mkcl_FEwrong_type_argument(env, @'broadcast-stream', strm);
  return mk_cl_copy_list(env, MKCL_BROADCAST_STREAM_LIST(strm));
}

/**********************************************************************
 * ECHO STREAM
 */

static mkcl_index
echo_read_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  mkcl_index out = mkcl_read_octet(env, MKCL_ECHO_STREAM_INPUT(strm), c, n);
  return mkcl_write_octet(env, MKCL_ECHO_STREAM_OUTPUT(strm), c, out);
}

static mkcl_index
echo_write_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  return mkcl_write_octet(env, MKCL_ECHO_STREAM_OUTPUT(strm), c, n);
}

static void
echo_write_byte(MKCL, mkcl_object c, mkcl_object strm)
{
  mkcl_write_byte(env, c, MKCL_ECHO_STREAM_OUTPUT(strm));
}

static mkcl_object
echo_read_byte(MKCL, mkcl_object strm)
{
  mkcl_object out = mkcl_read_byte(env, MKCL_ECHO_STREAM_INPUT(strm));
  if (!mkcl_Null(out)) mkcl_write_byte(env, out, MKCL_ECHO_STREAM_OUTPUT(strm));
  return out;
}

static mkcl_character
echo_read_char(MKCL, mkcl_object strm)
{
  mkcl_character c = strm->stream.last_code[0];
  if (c == EOF) {
    c = mkcl_read_char(env, MKCL_ECHO_STREAM_INPUT(strm));
    if (c != EOF)
      mkcl_write_char(env, c, MKCL_ECHO_STREAM_OUTPUT(strm));
  } else {
    strm->stream.last_code[0] = EOF;
  }
  return c;
}

static mkcl_character
echo_write_char(MKCL, mkcl_object strm, mkcl_character c)
{
  return mkcl_write_char(env, c, MKCL_ECHO_STREAM_OUTPUT(strm));
}

static void
echo_unread_char(MKCL, mkcl_object strm, mkcl_character c)
{
  if (strm->stream.last_code[0] != EOF) {
    unread_twice(env, strm);
  }
  strm->stream.last_code[0] = c;
}

static mkcl_character
echo_peek_char(MKCL, mkcl_object strm)
{
  mkcl_character c = strm->stream.last_code[0];
  if (c == EOF) {
    c = mkcl_peek_char(env, MKCL_ECHO_STREAM_INPUT(strm));
  }
  return c;
}

static int
echo_listen(MKCL, mkcl_object strm)
{
  return mkcl_listen_stream(env, MKCL_ECHO_STREAM_INPUT(strm));
}

static void
echo_clear_input(MKCL, mkcl_object strm)
{
  mkcl_clear_input(env, MKCL_ECHO_STREAM_INPUT(strm));
}

static void
echo_clear_output(MKCL, mkcl_object strm)
{
  mkcl_clear_output(env, MKCL_ECHO_STREAM_OUTPUT(strm));
}

static void
echo_force_output(MKCL, mkcl_object strm)
{
  mkcl_force_output(env, MKCL_ECHO_STREAM_OUTPUT(strm));
}

static void
echo_finish_output(MKCL, mkcl_object strm)
{
  mkcl_finish_output(env, MKCL_ECHO_STREAM_OUTPUT(strm));
}

static mkcl_object
echo_element_type(MKCL, mkcl_object strm)
{
  return mkcl_stream_element_type(env, MKCL_ECHO_STREAM_INPUT(strm));
}

static int
echo_column(MKCL, mkcl_object strm)
{
  return mkcl_file_column(env, MKCL_ECHO_STREAM_OUTPUT(strm));
}

static const struct mkcl_file_ops echo_ops = {
  echo_write_octet,
  echo_read_octet,

  echo_write_byte,
  echo_read_byte,

  echo_read_char,
  echo_write_char,
  echo_unread_char,
  echo_peek_char,

  generic_read_vector,
  generic_write_vector,

  echo_listen,
  echo_clear_input,
  echo_clear_output,
  echo_finish_output,
  echo_force_output,

  generic_always_true, /* input_p */
  generic_always_true, /* output_p */
  generic_always_false,
  echo_element_type,

  not_a_file_stream, /* length */
  generic_always_nil, /* get_position */
  generic_set_position,
  echo_column,
  generic_close
};

mkcl_object
mk_cl_make_echo_stream(MKCL, mkcl_object strm1, mkcl_object strm2)
{
  mkcl_object strm;

  mkcl_call_stack_check(env);
  if (!mkcl_input_stream_p(env, strm1))
    not_an_input_stream(env, strm1);
  if (!mkcl_output_stream_p(env, strm2))
    not_an_output_stream(env, strm2);
  strm = alloc_stream(env);
  strm->stream.format = mk_cl_stream_external_format(env, strm1);
  strm->stream.mode = mkcl_smm_echo;
  strm->stream.ops = duplicate_dispatch_table(env, &echo_ops);
  MKCL_ECHO_STREAM_INPUT(strm) = strm1;
  MKCL_ECHO_STREAM_OUTPUT(strm) = strm2;
  @(return strm);
}

mkcl_object
mk_cl_echo_stream_input_stream(MKCL, mkcl_object strm)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(strm) != mkcl_t_stream || strm->stream.mode != mkcl_smm_echo)
    mkcl_FEwrong_type_argument(env, @'echo-stream', strm);
  @(return MKCL_ECHO_STREAM_INPUT(strm));
}

mkcl_object
mk_cl_echo_stream_output_stream(MKCL, mkcl_object strm)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(strm) != mkcl_t_stream || strm->stream.mode != mkcl_smm_echo)
    mkcl_FEwrong_type_argument(env, @'echo-stream', strm);
  @(return MKCL_ECHO_STREAM_OUTPUT(strm));
}

/**********************************************************************
 * CONCATENATED STREAM
 */

static mkcl_index
concatenated_read_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  mkcl_object l = MKCL_CONCATENATED_STREAM_LIST(strm);
  mkcl_index out = 0;
  while (out < n && !mkcl_Null(l)) {
    mkcl_index delta = mkcl_read_octet(env, MKCL_CONS_CAR(l), c + out, n - out);
    out += delta;
    if (out == n) break;
    MKCL_CONCATENATED_STREAM_LIST(strm) = l = MKCL_CONS_CDR(l);
  }
  return out;
}

static mkcl_object
concatenated_read_byte(MKCL, mkcl_object strm)
{
  mkcl_object l = MKCL_CONCATENATED_STREAM_LIST(strm);
  mkcl_object c = mk_cl_Cnil;
  while (!mkcl_Null(l)) {
    c = mkcl_read_byte(env, MKCL_CONS_CAR(l));
    if (c != mk_cl_Cnil) break;
    MKCL_CONCATENATED_STREAM_LIST(strm) = l = MKCL_CONS_CDR(l);
  }
  return c;
}

static mkcl_character
concatenated_read_char(MKCL, mkcl_object strm)
{
  mkcl_object l = MKCL_CONCATENATED_STREAM_LIST(strm);
  mkcl_character c = EOF;
  while (!mkcl_Null(l)) {
    c = mkcl_read_char(env, MKCL_CONS_CAR(l));
    if (c != EOF) break;
    MKCL_CONCATENATED_STREAM_LIST(strm) = l = MKCL_CONS_CDR(l);
  }
  return c;
}

static void
concatenated_unread_char(MKCL, mkcl_object strm, mkcl_character c)
{
  mkcl_object l = MKCL_CONCATENATED_STREAM_LIST(strm);
  if (mkcl_Null(l))
    unread_error(env, strm);
  mkcl_unread_char(env, c, MKCL_CONS_CAR(l));
}

static int
concatenated_listen(MKCL, mkcl_object strm)
{
  mkcl_object l = MKCL_CONCATENATED_STREAM_LIST(strm);
  while (!mkcl_Null(l)) {
    int f = mkcl_listen_stream(env, MKCL_CONS_CAR(l));
    l = MKCL_CONS_CDR(l);
    if (f == MKCL_LISTEN_EOF) {
      MKCL_CONCATENATED_STREAM_LIST(strm) = l;
    } else {
      return f;
    }
  }
  return MKCL_LISTEN_EOF;
}

static const struct mkcl_file_ops concatenated_ops = {
  not_output_write_octet,
  concatenated_read_octet,
  
  not_output_write_byte,
  concatenated_read_byte,
  
  concatenated_read_char,
  not_output_write_char,
  concatenated_unread_char,
  generic_peek_char,

  generic_read_vector,
  generic_write_vector,

  concatenated_listen,
  generic_void, /* clear_input */
  not_output_clear_output,
  not_output_finish_output,
  not_output_force_output,

  generic_always_true, /* input_p */
  generic_always_false, /* output_p */
  generic_always_false,
  broadcast_element_type,

  not_a_file_stream, /* length */
  generic_always_nil, /* get_position */
  generic_set_position,
  generic_column,
  generic_close
};

@(defun make_concatenated_stream (&rest ap)
  mkcl_object x, streams;
  int i;
@
  streams = mk_cl_Cnil;
  for (i = 0; i < narg; i++) {
    x = mkcl_va_arg(ap);
    if (!mkcl_input_stream_p(env, x))
      not_an_input_stream(env, x);
    streams = MKCL_CONS(env, x, streams);
  }
  x = alloc_stream(env);
  if (mkcl_Null(streams)) {
    x->stream.format = @':default';
  } else {
    x->stream.format = mk_cl_stream_external_format(env, MKCL_CONS_CAR(streams));
  }
  x->stream.mode = mkcl_smm_concatenated;
  x->stream.ops = duplicate_dispatch_table(env, &concatenated_ops);
  MKCL_CONCATENATED_STREAM_LIST(x) = mk_cl_nreverse(env, streams);
  @(return x);
@)

mkcl_object
mk_cl_concatenated_stream_streams(MKCL, mkcl_object strm)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(strm) != mkcl_t_stream || strm->stream.mode != mkcl_smm_concatenated)
    mkcl_FEwrong_type_argument(env, @'concatenated-stream', strm);
  return mk_cl_copy_list(env, MKCL_CONCATENATED_STREAM_LIST(strm));
}

/**********************************************************************
 * SYNONYM STREAM
 */

static mkcl_index
synonym_read_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  return mkcl_read_octet(env, MKCL_SYNONYM_STREAM_STREAM(env, strm), c, n);
}

static mkcl_index
synonym_write_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  return mkcl_write_octet(env, MKCL_SYNONYM_STREAM_STREAM(env, strm), c, n);
}

static void
synonym_write_byte(MKCL, mkcl_object c, mkcl_object strm)
{
  mkcl_write_byte(env, c, MKCL_SYNONYM_STREAM_STREAM(env, strm));
}

static mkcl_object
synonym_read_byte(MKCL, mkcl_object strm)
{
  return mkcl_read_byte(env, MKCL_SYNONYM_STREAM_STREAM(env, strm));
}

static mkcl_character
synonym_read_char(MKCL, mkcl_object strm)
{
  return mkcl_read_char(env, MKCL_SYNONYM_STREAM_STREAM(env, strm));
}

static mkcl_character
synonym_write_char(MKCL, mkcl_object strm, mkcl_character c)
{
  return mkcl_write_char(env, c, MKCL_SYNONYM_STREAM_STREAM(env, strm));
}

static void
synonym_unread_char(MKCL, mkcl_object strm, mkcl_character c)
{
  mkcl_unread_char(env, c, MKCL_SYNONYM_STREAM_STREAM(env, strm));
}

static mkcl_character
synonym_peek_char(MKCL, mkcl_object strm)
{
  return mkcl_peek_char(env, MKCL_SYNONYM_STREAM_STREAM(env, strm));
}

static mkcl_index
synonym_read_vector(MKCL, mkcl_object strm, mkcl_object data, mkcl_index start, mkcl_index n)
{
  strm = MKCL_SYNONYM_STREAM_STREAM(env, strm);
  return stream_dispatch_table(env, strm)->read_vector(env, strm, data, start, n);
}

static mkcl_index
synonym_write_vector(MKCL, mkcl_object strm, mkcl_object data, mkcl_index start, mkcl_index n)
{
  strm = MKCL_SYNONYM_STREAM_STREAM(env, strm);
  return stream_dispatch_table(env, strm)->write_vector(env, strm, data, start, n);
}

static int
synonym_listen(MKCL, mkcl_object strm)
{
  return mkcl_listen_stream(env, MKCL_SYNONYM_STREAM_STREAM(env, strm));
}

static void
synonym_clear_input(MKCL, mkcl_object strm)
{
  mkcl_clear_input(env, MKCL_SYNONYM_STREAM_STREAM(env, strm));
}

static void
synonym_clear_output(MKCL, mkcl_object strm)
{
  mkcl_clear_output(env, MKCL_SYNONYM_STREAM_STREAM(env, strm));
}

static void
synonym_force_output(MKCL, mkcl_object strm)
{
  mkcl_force_output(env, MKCL_SYNONYM_STREAM_STREAM(env, strm));
}

static void
synonym_finish_output(MKCL, mkcl_object strm)
{
  mkcl_finish_output(env, MKCL_SYNONYM_STREAM_STREAM(env, strm));
}

static bool
synonym_input_p(MKCL, mkcl_object strm)
{
  return mkcl_input_stream_p(env, MKCL_SYNONYM_STREAM_STREAM(env, strm));
}

static bool
synonym_output_p(MKCL, mkcl_object strm)
{
  return mkcl_output_stream_p(env, MKCL_SYNONYM_STREAM_STREAM(env, strm));
}

static bool
synonym_interactive_p(MKCL, mkcl_object strm)
{
  return mkcl_interactive_stream_p(env, MKCL_SYNONYM_STREAM_STREAM(env, strm));
}

static mkcl_object
synonym_element_type(MKCL, mkcl_object strm)
{
  return mkcl_stream_element_type(env, MKCL_SYNONYM_STREAM_STREAM(env, strm));
}

static mkcl_object
synonym_length(MKCL, mkcl_object strm)
{
  return mkcl_file_length(env, MKCL_SYNONYM_STREAM_STREAM(env, strm));
}

static mkcl_object
synonym_get_position(MKCL, mkcl_object strm)
{
  return mkcl_file_position(env, MKCL_SYNONYM_STREAM_STREAM(env, strm));
}

static mkcl_object
synonym_set_position(MKCL, mkcl_object strm, mkcl_object pos)
{
  return mkcl_file_position_set(env, MKCL_SYNONYM_STREAM_STREAM(env, strm), pos);
}

static int
synonym_column(MKCL, mkcl_object strm)
{
  return mkcl_file_column(env, MKCL_SYNONYM_STREAM_STREAM(env, strm));
}

static const struct mkcl_file_ops synonym_ops = {
  synonym_write_octet,
  synonym_read_octet,

  synonym_write_byte,
  synonym_read_byte,

  synonym_read_char,
  synonym_write_char,
  synonym_unread_char,
  synonym_peek_char,

  synonym_read_vector,
  synonym_write_vector,

  synonym_listen,
  synonym_clear_input,
  synonym_clear_output,
  synonym_finish_output,
  synonym_force_output,

  synonym_input_p,
  synonym_output_p,
  synonym_interactive_p,
  synonym_element_type,

  synonym_length,
  synonym_get_position,
  synonym_set_position,
  synonym_column,
  generic_close
};

mkcl_object
mk_cl_make_synonym_stream(MKCL, mkcl_object sym)
{
  mkcl_object x;

  mkcl_call_stack_check(env);
  sym = mkcl_check_cl_type(env, @'make-synonym-stream',sym,mkcl_t_symbol);
  x = alloc_stream(env);
  x->stream.ops = duplicate_dispatch_table(env, &synonym_ops);
  x->stream.mode = mkcl_smm_synonym;
  MKCL_SYNONYM_STREAM_SYMBOL(x) = sym;
  @(return x);
}

mkcl_object
mk_cl_synonym_stream_symbol(MKCL, mkcl_object strm)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(strm) != mkcl_t_stream || strm->stream.mode != mkcl_smm_synonym)
    mkcl_FEwrong_type_argument(env, @'synonym-stream', strm);
  @(return MKCL_SYNONYM_STREAM_SYMBOL(strm));
}

/**********************************************************************
 * POSIX FILE STREAM
 */

static mkcl_index
io_file_read_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  mkcl_index peek_out = 0;
  mkcl_index out = 0;
  mkcl_object l = strm->stream.byte_stack;

  for(; l != mk_cl_Cnil && n > 0; peek_out++, c++, n--)
    {
      *c = mkcl_fixnum_to_word(MKCL_CONS_CAR(l));
      strm->stream.byte_stack = l = MKCL_CONS_CDR(l);
    }

  if (n > 0) /* Do we really need this test? A read with (n == 0) may be meaningful. JCB */
    {
#if __linux
      int retry = 0;
#endif
      int f = MKCL_IO_FILE_DESCRIPTOR(strm);
      ssize_t nread;

      do {
#ifdef MKCL_WINDOWS
	/* Synchronous read cannot be interrupted in MS-Windows. */
	MKCL_LIBC_NO_INTR(env, nread = read(f, (c + out), sizeof(char)*(n - out)));
#else
	MKCL_LIBC_Zzz(env, @':io', nread = read(f, (c + out), sizeof(char)*(n - out)));
#endif
	if (nread > 0) out += nread;
      } while (((nread < 0) && restartable_io_error(env, strm, NULL))
#ifdef MKCL_WINDOWS
	       /* MS-Windows produces false EOF on consoles
		  if there is a console control event. JCB
	       */
	       || ((nread == 0) && should_try_to_read_again(env, f))
#elif __linux
	       /* Linux also produces false EOF on a (pseudo?) tty
		  for some specific line discipline as the one used
		  by emacs in inferior shell mode. The EOF seems
		  to happen on every boundary of 256 characters! JCB
		  Linux 2.6.31
	       */
	       || ((nread == 0) && isatty(f) && (++retry < 3))
#endif
	       || ((nread > 0) && (out < n)) /* incomplete read. */
	       );
      mk_mt_test_for_thread_shutdown(env);
    }
  return (peek_out + out);
}

static mkcl_index
output_file_write_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  mkcl_interrupt_status old_intr;
  int f = MKCL_IO_FILE_DESCRIPTOR(strm);
  mkcl_index out = 0;
  ssize_t nwritten;

  mkcl_get_interrupt_status(env, &old_intr);
  mkcl_disable_interrupts(env);
  do {
    nwritten = write(f, (c + out), sizeof(char)*(n - out));
    if (nwritten >= 0)
      out += nwritten;
  } while (((nwritten < 0) && restartable_io_error(env, strm, &old_intr))
	   || ((nwritten >= 0) && (out < n)) /* incomplete write. */
	   );
  mkcl_set_interrupt_status(env, &old_intr);
  return out;
}

static mkcl_index
io_file_write_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  if (strm->stream.byte_stack != mk_cl_Cnil) {
    /* Try to move to the beginning of the unread characters */
    mkcl_object aux = mkcl_file_position(env, strm);
    if (!mkcl_Null(aux))
      mkcl_file_position_set(env, strm, aux);
    strm->stream.byte_stack = mk_cl_Cnil; /* Beware that this destroys the last unread-char. JCB */
  }
  return output_file_write_octet(env, strm, c, n);
}

static int
io_file_listen(MKCL, mkcl_object strm)
{
  if (strm->stream.byte_stack != mk_cl_Cnil)
    return MKCL_LISTEN_AVAILABLE;
  if (strm->stream.flags & MKCL_STREAM_SEEKABLE) {
    int f = MKCL_IO_FILE_DESCRIPTOR(strm);
    mkcl_off_t disp, new, it;

    MKCL_LIBC_NO_INTR(env, disp = lseek(f, 0, SEEK_CUR));
    if (disp != (mkcl_off_t)-1) {
      MKCL_LIBC_NO_INTR(env, new = lseek(f, 0, SEEK_END));
      if (new == (mkcl_off_t)-1) io_error(env, strm);
      MKCL_LIBC_NO_INTR(env, it = lseek(f, disp, SEEK_SET));
      if (it == (mkcl_off_t)-1) io_error(env, strm);
      if (new == disp) {
	return MKCL_LISTEN_NO_CHAR;
      } else if (new != (mkcl_off_t)-1) {
	return MKCL_LISTEN_AVAILABLE;
      } else io_error(env, strm);
    }
    else io_error(env, strm);
  }
  return file_listen(env, MKCL_IO_FILE_DESCRIPTOR(strm));
}

static void
io_file_clear_input(MKCL, mkcl_object strm)
{
  int f = MKCL_IO_FILE_DESCRIPTOR(strm);
#ifdef MKCL_WINDOWS
  BOOL good;
  int igood;

  MKCL_LIBC_NO_INTR(env, igood = isatty(f));
  if (igood) {
    /* Flushes Win32 console */
    MKCL_LIBC_NO_INTR(env, good = FlushConsoleInputBuffer((HANDLE)_get_osfhandle(f)));
    if (!good)
      mkcl_FEwin32_error(env, "FlushConsoleInputBuffer() failed", 0);
    /* Do not stop here: the FILE structure needs also to be flushed */
  }
#endif
  while (file_listen(env, f) == MKCL_LISTEN_AVAILABLE) {
    eformat_read_char(env, strm);
  }
}

#define io_file_clear_output generic_void
#define io_file_force_output generic_void
#define io_file_finish_output io_file_force_output

static bool
io_file_interactive_p(MKCL, mkcl_object strm)
{
  int f = MKCL_IO_FILE_DESCRIPTOR(strm);
  return isatty(f);
}

static mkcl_object
io_file_element_type(MKCL, mkcl_object strm)
{
  return MKCL_IO_FILE_ELT_TYPE(strm);
}

static mkcl_object
io_file_length(MKCL, mkcl_object strm)
{
  int f = MKCL_IO_FILE_DESCRIPTOR(strm);
  mkcl_object output = mkcl_file_len(env, f);
  if (strm->stream.byte_size != 8) {
    mkcl_index bs = strm->stream.byte_size;
    output = mkcl_floor2(env, output, MKCL_MAKE_FIXNUM(bs/8));
    if (MKCL_VALUES(1) != MKCL_MAKE_FIXNUM(0)) {
      mkcl_FEerror(env, "File length is not on byte boundary", 0);
    }
  }
  return output;
}

static mkcl_object
io_file_get_position(MKCL, mkcl_object strm)
{
  int f = MKCL_IO_FILE_DESCRIPTOR(strm);
  mkcl_object output;
  mkcl_off_t offset;

  MKCL_LIBC_NO_INTR(env, offset = lseek(f, 0, SEEK_CUR));

  if (offset < 0)
    {
      if (errno == ESPIPE)
	return mk_cl_Cnil; /* This was a socket or a pipe, cannot seek! */
      else
	io_error(env, strm);
    }

  if (sizeof(mkcl_off_t) == sizeof(long)) {
    output = mkcl_make_integer(env, offset);
  } else {
    output = mkcl_off_t_to_integer(env, offset);
  }
  {
    /* If there are unread octets, we return the position at which
     * these bytes begin! */
    mkcl_object l = strm->stream.byte_stack;
    while (MKCL_CONSP(l)) {
      output = mkcl_one_minus(env, output);
      l = MKCL_CONS_CDR(l);
    }
  }
  if (strm->stream.byte_size != 8) {
    output = mkcl_floor2(env, output, MKCL_MAKE_FIXNUM(strm->stream.byte_size / 8));
  }
  return output;
}

static mkcl_object
io_file_set_position(MKCL, mkcl_object strm, mkcl_object large_disp)
{
  int f = MKCL_IO_FILE_DESCRIPTOR(strm);
  mkcl_off_t disp;
  int mode;
  if (mkcl_Null(large_disp)) {
    disp = 0;
    mode = SEEK_END;
  } else {
    if (strm->stream.byte_size != 8) {
      large_disp = mkcl_times(env, large_disp,
			      MKCL_MAKE_FIXNUM(strm->stream.byte_size / 8));
    }
    disp = mkcl_integer_to_off_t(env, large_disp);
    mode = SEEK_SET;
  }
  disp = lseek(f, disp, mode);
  return (disp == (mkcl_off_t)-1)? mk_cl_Cnil : mk_cl_Ct;
}

static int
io_file_column(MKCL, mkcl_object strm)
{
  return MKCL_IO_FILE_COLUMN(strm);
}

static mkcl_object
io_file_close(MKCL, mkcl_object strm)
{
  int f = MKCL_IO_FILE_DESCRIPTOR(strm);
  int failed;

  mkcl_safe_close(env, f, strm);
  MKCL_IO_FILE_DESCRIPTOR(strm) = -1;
  return generic_close(env, strm);
}

static mkcl_index
io_file_read_vector(MKCL, mkcl_object strm, mkcl_object data, mkcl_index start, mkcl_index end)
{
  mkcl_elttype t = mkcl_array_elttype(env, data);
  if (start >= end)
    return start;
  if (t == mkcl_aet_b8 || t == mkcl_aet_i8) {
    if (strm->stream.byte_size == 8) {
      void *aux = data->vector.self.bc + start;
      return start + strm->stream.ops->read_octet(env, strm, aux, end-start);
    }
  } else if (t == mkcl_aet_word || t == mkcl_aet_index) {
    if (strm->stream.byte_size == sizeof(mkcl_word)*8) {
      /* Savage CPU natural endianness IO! Ignores stream's declared external format. JCB */
      void *aux = data->vector.self.word + start;
      mkcl_index bytes = (end - start) * sizeof(mkcl_word);
      bytes = strm->stream.ops->read_octet(env, strm, aux, bytes);
      return start + bytes / sizeof(mkcl_word);
    }
  }
  return generic_read_vector(env, strm, data, start, end);
}

static mkcl_index
io_file_write_vector(MKCL, mkcl_object strm, mkcl_object data, mkcl_index start, mkcl_index end)
{
  mkcl_elttype t = mkcl_array_elttype(env, data);
  if (start >= end)
    return start;
  if (t == mkcl_aet_b8 || t == mkcl_aet_i8) {
    if (strm->stream.byte_size == 8) {
      void *aux = data->vector.self.bc + start;

      return strm->stream.ops->write_octet(env, strm, aux, end-start);
    }
  } else if (t == mkcl_aet_word || t == mkcl_aet_index) {
    if (strm->stream.byte_size == sizeof(mkcl_word)*8) {
      /* Savage CPU natural endianness IO! Ignores stream's declared external format.  JCB */
      void *aux = data->vector.self.word + start;
      mkcl_index bytes = (end - start) * sizeof(mkcl_word);
      bytes = strm->stream.ops->write_octet(env, strm, aux, bytes);
      return start + bytes / sizeof(mkcl_word);
    }
  }
  return generic_write_vector(env, strm, data, start, end);
}

static const struct mkcl_file_ops io_file_ops = {
  io_file_write_octet,
  io_file_read_octet,

  generic_write_byte_be,
  generic_read_byte_be,

  eformat_read_char,
  eformat_write_char,
  eformat_unread_char,
  generic_peek_char,

  io_file_read_vector,
  io_file_write_vector,

  io_file_listen,
  io_file_clear_input,
  io_file_clear_output,
  io_file_finish_output,
  io_file_force_output,

  generic_always_true, /* input_p */
  generic_always_true, /* output_p */
  io_file_interactive_p,
  io_file_element_type,

  io_file_length,
  io_file_get_position,
  io_file_set_position,
  io_file_column,
  io_file_close
};

static const struct mkcl_file_ops output_file_ops = {
  output_file_write_octet,
  not_input_read_octet,

  generic_write_byte_be,
  not_input_read_byte,

  not_input_read_char,
  eformat_write_char,
  not_input_unread_char,
  not_input_read_char,

  generic_read_vector,
  io_file_write_vector,

  not_input_listen,
  not_input_clear_input,
  io_file_clear_output,
  io_file_finish_output,
  io_file_force_output,

  generic_always_false, /* input_p */
  generic_always_true, /* output_p */
  generic_always_false,
  io_file_element_type,

  io_file_length,
  io_file_get_position,
  io_file_set_position,
  io_file_column,
  io_file_close
};

static const struct mkcl_file_ops input_file_ops = {
  not_output_write_octet,
  io_file_read_octet,

  not_output_write_byte,
  generic_read_byte_be,

  eformat_read_char,
  not_output_write_char,
  eformat_unread_char,
  generic_peek_char,

  io_file_read_vector,
  generic_write_vector,

  io_file_listen,
  io_file_clear_input,
  not_output_clear_output,
  not_output_finish_output,
  not_output_force_output,

  generic_always_true, /* input_p */
  generic_always_false, /* output_p */
  io_file_interactive_p,
  io_file_element_type,

  io_file_length,
  io_file_get_position,
  io_file_set_position,
  generic_column,
  io_file_close
};

/******************************************/


static void
set_stream_elt_type(MKCL, mkcl_object stream, mkcl_word byte_size, mkcl_stream_flag_set flags, mkcl_object external_format)
{
  if (byte_size == 0) flags |= MKCL_STREAM_TEXT;

  if ((flags & MKCL_STREAM_FORMAT_MASK) != MKCL_STREAM_TEXT)
    { /* Binary stream*/
      mkcl_object element_type;

      if (byte_size < 0) {
	byte_size = -byte_size;
	flags |= MKCL_STREAM_SIGNED_BYTES;
	element_type = @'signed-byte';
      } else if (byte_size > 0) {
	flags &= ~MKCL_STREAM_SIGNED_BYTES;
	element_type = @'unsigned-byte';
      } else {/* byte_size == 0 */
	mkcl_FEerror(env, "Binary stream of unspecified element-type", 0);
      }

      MKCL_IO_STREAM_ELT_TYPE(stream) = mk_cl_list(env, 2, element_type, MKCL_MAKE_FIXNUM(byte_size));
      stream->stream.format = element_type;
      stream->stream.ops->read_char = not_character_read_char;
      stream->stream.ops->write_char = not_character_write_char;

      if (external_format == @':LITTLE-ENDIAN')
	flags |= MKCL_STREAM_LITTLE_ENDIAN;
      else if (external_format == @':BIG-ENDIAN')
	flags &= ~MKCL_STREAM_LITTLE_ENDIAN;
      else if (external_format == @':default')
	{  /* What should we do here? JCB */
	  if (byte_size > 8)
	    flags |= MKCL_STREAM_LITTLE_ENDIAN; /* Good for x86 and x86_64. JCB */
	}
      else if (!mkcl_Null(external_format))
	mkcl_FEerror(env, "Invalid binary stream external-format specifier: ~S", 1, external_format);
    }
  else
    { /* Text stream */
      mkcl_object line_termination;

      stream->stream.ops->read_char = eformat_read_char;
      stream->stream.ops->write_char = eformat_write_char;
  
      if (external_format == @':default')
	{
	  external_format = mkcl_symbol_value(env, @'si::*default-external-format*');
	  if (external_format == @':default')
	    { /* This is the hardcoded fallback. */
	      external_format = mkcl_core.default_default_external_format;
	    }
	}

      if (MKCL_CONSP(external_format)) {
	mkcl_object format_spec = external_format;

	external_format = mk_cl_car(env, format_spec);
	line_termination = mk_cl_cadr(env, format_spec);

	if (!mkcl_Null(mk_cl_cddr(env, format_spec)))
	  mkcl_FEerror(env, "Invalid external-format specifier: ~S", 1, external_format);

	if (line_termination == @':CR')
	  flags = (flags | MKCL_STREAM_CR) & ~MKCL_STREAM_LF;
	else if (line_termination == @':LF')
	  flags = (flags | MKCL_STREAM_LF) & ~MKCL_STREAM_CR;
	else if (line_termination == @':CRLF')
	  flags = flags | (MKCL_STREAM_CR | MKCL_STREAM_LF);
	else
	  mkcl_FEerror(env, "Invalid line termination specifier: ~S", 1, line_termination);
      } else {
#ifdef MKCL_WINDOWS
	line_termination = @':CRLF'; /* default line termination */
#else
	line_termination = @':LF'; /* default line termination */
#endif
      }

      if (external_format == @':ISO-8859-1' || external_format == @':LATIN-1')
	{
	  MKCL_IO_STREAM_ELT_TYPE(stream) = @'base-char';
	  byte_size = 8;
	  stream->stream.format = @':iso-8859-1';
	  stream->stream.encoder = passthrough_encoder;
	  stream->stream.decoder = passthrough_decoder;
	}
      else if (external_format == @':UTF-8')
	{
	  MKCL_IO_STREAM_ELT_TYPE(stream) = @'character';
	  byte_size = 8;
	  stream->stream.format = @':utf-8';
	  stream->stream.encoder = utf_8_encoder;
	  stream->stream.decoder = utf_8_decoder;
	}
      else if (external_format == @':UTF-16')
	{
	  MKCL_IO_STREAM_ELT_TYPE(stream) = @'character';
	  byte_size = 8*2;
	  stream->stream.format = @':utf-16';
	  stream->stream.encoder = utf_16_encoder;
	  stream->stream.decoder = utf_16_decoder;
	}
      else if (external_format == @':UTF-16BE')
	{
	  MKCL_IO_STREAM_ELT_TYPE(stream) = @'character';
	  byte_size = 8*2;
	  stream->stream.format = @':utf-16be';
	  stream->stream.encoder = utf_16be_encoder;
	  stream->stream.decoder = utf_16be_decoder;

	  if (flags | MKCL_STREAM_LITTLE_ENDIAN)
	    mkcl_FEerror(env, "Incoherent stream format :UTF-16BE on a little-endian stream", 0);
	}
      else if (external_format == @':UTF-16LE')
	{
	  MKCL_IO_STREAM_ELT_TYPE(stream) = @'character';
	  byte_size = 8*2;
	  stream->stream.format = @':utf-16le';
	  stream->stream.encoder = utf_16le_encoder;
	  stream->stream.decoder = utf_16le_decoder;

	  flags |= MKCL_STREAM_LITTLE_ENDIAN;
	}
      else if (external_format == @':UTF-32')
	{
	  MKCL_IO_STREAM_ELT_TYPE(stream) = @'character';
	  byte_size = 8*4;
	  stream->stream.format = @':utf-32';
	  stream->stream.encoder = utf_32_encoder;
	  stream->stream.decoder = utf_32_decoder;
	}
      else if (external_format == @':UTF-32BE')
	{
	  MKCL_IO_STREAM_ELT_TYPE(stream) = @'character';
	  byte_size = 8*4;
	  stream->stream.format = @':utf-32be';
	  stream->stream.encoder = utf_32be_encoder;
	  stream->stream.decoder = utf_32be_decoder;

	  if (flags | MKCL_STREAM_LITTLE_ENDIAN)
	    mkcl_FEerror(env, "Incoherent stream format :UTF-32BE on a little-endian stream", 0);
	}
      else if (external_format == @':UTF-32LE')
	{
	  MKCL_IO_STREAM_ELT_TYPE(stream) = @'character';
	  byte_size = 8*4;
	  stream->stream.format = @':utf-32le';
	  stream->stream.encoder = utf_32le_encoder;
	  stream->stream.decoder = utf_32le_decoder;

	  flags |= MKCL_STREAM_LITTLE_ENDIAN;
	}
      else if (external_format == @':US-ASCII' || external_format == @':ASCII')
	{
	  MKCL_IO_STREAM_ELT_TYPE(stream) = @'base-char';
	  byte_size = 8;
	  stream->stream.format = @':us-ascii';
	  stream->stream.encoder = ascii_encoder;
	  stream->stream.decoder = ascii_decoder;
	}
      else if (MKCL_SYMBOLP(external_format))
	{
	  mkcl_object format_table = mkcl_funcall1(env, @+'si::make-encoding', external_format);

	  MKCL_IO_STREAM_ELT_TYPE(stream) = @'character';
	  byte_size = 8;
	  stream->stream.format_table = format_table;
	  stream->stream.format = external_format;
	  if (MKCL_CONSP(format_table))
	    {
	      stream->stream.encoder = user_multistate_encoder;
	      stream->stream.decoder = user_multistate_decoder;
	    }
	  else
	    {
	      stream->stream.encoder = user_encoder;
	      stream->stream.decoder = user_decoder;
	    }
	}
      else if (MKCL_HASH_TABLE_P(external_format))
	{
	  MKCL_IO_STREAM_ELT_TYPE(stream) = @'character';
	  byte_size = 8;
	  stream->stream.format = stream->stream.format_table = external_format;
	  stream->stream.encoder = user_encoder;
	  stream->stream.decoder = user_decoder;
	}
      else
	mkcl_FEerror(env, "Invalid or unsupported stream :external-format ~S with flags #x~X",
		     2, external_format, MKCL_MAKE_FIXNUM(flags));

      if (stream->stream.ops->write_char == eformat_write_char && (flags & MKCL_STREAM_CR)) {
	if (flags & MKCL_STREAM_LF) {
	  stream->stream.ops->read_char = eformat_read_char_crlf;
	  stream->stream.ops->write_char = eformat_write_char_crlf;
	  line_termination = @':CRLF';
	} else {
	  stream->stream.ops->read_char = eformat_read_char_cr;
	  stream->stream.ops->write_char = eformat_write_char_cr;
	  line_termination = @':CR';
	}
      }
      stream->stream.format = mkcl_cons(env, stream->stream.format, mkcl_list1(env, line_termination));

    }

  {
    mkcl_object (*read_byte)(MKCL, mkcl_object);
    void (*write_byte)(MKCL, mkcl_object, mkcl_object);

    byte_size = (byte_size+7)&~((mkcl_word)7); /* round up on boundary of 8. */
    if (byte_size == 8) {
      if (flags & MKCL_STREAM_SIGNED_BYTES) {
	read_byte = generic_read_byte_signed8;
	write_byte = generic_write_byte_signed8;
      } else {
	read_byte = generic_read_byte_unsigned8;
	write_byte = generic_write_byte_unsigned8;
      }
    } else if (flags & MKCL_STREAM_LITTLE_ENDIAN) {
      read_byte = generic_read_byte_le;
      write_byte = generic_write_byte_le;
    } else {
      read_byte = generic_read_byte_be;
      write_byte = generic_write_byte_be;
    }
    if (mkcl_input_stream_p(env, stream)) {
      stream->stream.ops->read_byte = read_byte;
    }
    if (mkcl_output_stream_p(env, stream)) {
      stream->stream.ops->write_byte = write_byte;
    }
  }
  stream->stream.flags = flags;
  stream->stream.byte_size = byte_size;
}

mkcl_object
mk_si_stream_external_format_set(MKCL, mkcl_object stream, mkcl_object format)
{
  mkcl_call_stack_check(env);
  if (mkcl_unlikely(MKCL_INSTANCEP(stream))) {
    mkcl_FEerror(env, "Cannot change external format of stream ~A", 1, stream);
    @(return);
  }
  switch (stream->stream.mode) 
    {
    case mkcl_smm_input:
    case mkcl_smm_input_file:
    case mkcl_smm_output:
    case mkcl_smm_output_file:
    case mkcl_smm_io:
    case mkcl_smm_io_file:
    case mkcl_smm_input_socket:
    case mkcl_smm_output_socket:
    case mkcl_smm_io_socket:
      {
	mkcl_object elt_type = mkcl_stream_element_type(env, stream);
	if (mkcl_unlikely(!(elt_type == @'character' || elt_type == @'base-char')))
	  mkcl_FEerror(env, "Cannot change external format of binary stream ~A", 1, stream);
	set_stream_elt_type(env, stream, stream->stream.byte_size, stream->stream.flags, format);
      }
      break;
    default:
      mkcl_FEerror(env, "Cannot change external format of stream ~A", 1, stream);
      break;
    }
  @(return);
}


static mkcl_object
make_file_stream_from_fd(MKCL, mkcl_object fname, int fd, enum mkcl_smmode smm,
			 mkcl_word byte_size, mkcl_object external_format)
{
  mkcl_object stream = alloc_stream(env);

  stream->stream.mode = smm;
  stream->stream.closed = 0;
  MKCL_IO_FILE_FILENAME(stream) = fname;
  MKCL_IO_FILE_COLUMN(stream) = 0;
  MKCL_IO_FILE_DESCRIPTOR(stream) = fd;
  switch(smm) {
  case mkcl_smm_probe:
  case mkcl_smm_input_file:
    stream->stream.ops = duplicate_dispatch_table(env, &input_file_ops);
    break;
  case mkcl_smm_output_file:
    stream->stream.ops = duplicate_dispatch_table(env, &output_file_ops);
    break;
  case mkcl_smm_io_file:
    stream->stream.ops = duplicate_dispatch_table(env, &io_file_ops);
    break;
  case mkcl_smm_input_socket:
    stream->stream.ops = duplicate_dispatch_table(env, &socket_stream_input_ops);
    break;
  case mkcl_smm_output_socket:
    stream->stream.ops = duplicate_dispatch_table(env, &socket_stream_output_ops);
    break;
  case mkcl_smm_io_socket:
    stream->stream.ops = duplicate_dispatch_table(env, &socket_stream_io_ops);
    break;
  default:
    mkcl_FEerror(env, "Not a valid mode ~D for make_file_stream_from_fd()", 1, MKCL_MAKE_FIXNUM(smm));
  }
  int flags = 0;
  set_stream_elt_type(env, stream, byte_size, flags, external_format);
  stream->stream.last_op = 0;
  mk_si_set_finalizer(env, stream, mk_cl_Ct);
  return stream;
}

/**********************************************************************
 * C STREAMS
 */

static mkcl_index
input_stream_read_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  mkcl_index peek_out = 0;
  mkcl_index out = 0;
  mkcl_object l = strm->stream.byte_stack;

  for (; l != mk_cl_Cnil && n > 0; peek_out++, c++, n--)
    {
      *c = mkcl_fixnum_to_word(MKCL_CONS_CAR(l));
      strm->stream.byte_stack = l = MKCL_CONS_CDR(l);
    }

  if (n > 0)
    {
      FILE * f = MKCL_IO_STREAM_FILE(strm);

      do {
#ifdef MKCL_WINDOWS
	/* Synchronous read cannot be interrupted in MS-Windows. */
	MKCL_LIBC_NO_INTR(env, out += fread((c + out), sizeof(char), (n - out), f));
#else
	MKCL_LIBC_Zzz(env, @':io', out += fread((c + out), sizeof(char), (n - out), f));
#endif
      } while ((out < n)
	       && ((ferror(f) && restartable_io_error(env, strm, NULL))
		   || !feof(f) /* incomplete fread? */
#ifdef MKCL_WINDOWS
		   || (feof(f) && should_try_to_read_again(env, fileno(f)))
#endif	     
		   ));
      mk_mt_test_for_thread_shutdown(env);
    }
  return (peek_out + out);
}

static mkcl_index
output_stream_write_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  mkcl_index out = 0;
  size_t nwritten;
  FILE * f = MKCL_IO_STREAM_FILE(strm);
  mkcl_interrupt_status old_intr;

  mkcl_get_interrupt_status(env, &old_intr);
  mkcl_disable_interrupts(env);
  do {
    out += fwrite((c + out), sizeof(char), (n - out), f); 
  } while (out < n && (ferror(f) && restartable_io_error(env, strm, &old_intr)));
  mkcl_set_interrupt_status(env, &old_intr);
  return out;
}

static mkcl_index
io_stream_write_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  /* When using the same stream for input and output operations, we have to
   * use some file position operation before reading again. Besides this, if
   * there were unread octets, we have to move to the position at the
   * begining of them.
   */
  if (strm->stream.byte_stack != mk_cl_Cnil) {
    mkcl_object aux = mkcl_file_position(env, strm);
    if (!mkcl_Null(aux))
      mkcl_file_position_set(env, strm, aux);
  } else if (strm->stream.last_op > 0) {
    mkcl_fseeko(MKCL_IO_STREAM_FILE(strm), 0, SEEK_CUR);
  }
  strm->stream.last_op = -1;
  return output_stream_write_octet(env, strm, c, n);
}

static void io_stream_force_output(MKCL, mkcl_object strm);

static mkcl_index
io_stream_read_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  /* When using the same stream for input and output operations, we have to
   * flush the stream before reading.
   */
  if (strm->stream.last_op < 0) {
    io_stream_force_output(env, strm);
  }
  strm->stream.last_op = +1;
  return input_stream_read_octet(env, strm, c, n);
}

static int
io_stream_listen(MKCL, mkcl_object strm)
{
  if (strm->stream.byte_stack != mk_cl_Cnil)
    return MKCL_LISTEN_AVAILABLE;
  return flisten(env, MKCL_IO_STREAM_FILE(strm));
}

static void
io_stream_clear_input(MKCL, mkcl_object strm)
{
  FILE *fp = MKCL_IO_STREAM_FILE(strm);
#ifdef MKCL_WINDOWS
  int f = fileno(fp);
  int igood;

  MKCL_LIBC_NO_INTR(env, igood = isatty(f));
  if (igood) {
    /* Flushes Win32 console */
    BOOL good;

    MKCL_LIBC_NO_INTR(env, good = FlushConsoleInputBuffer((HANDLE)_get_osfhandle(f)));
    if (!good)
      mkcl_FEwin32_error(env, "FlushConsoleInputBuffer() failed", 0);
    /* Do not stop here: the FILE structure needs also to be flushed */
  }
#endif
  while (flisten(env, fp) == MKCL_LISTEN_AVAILABLE) {
    MKCL_LIBC_NO_INTR(env, getc(fp));
  }
  if (ferror(fp))
    {
      mkcl_FElibc_stream_error(env, strm, "clear-input failed.", 0);
    }
}

#define io_stream_clear_output generic_void

static void
io_stream_force_output(MKCL, mkcl_object strm)
{
  FILE *f = MKCL_IO_STREAM_FILE(strm);
  mkcl_interrupt_status old_intr;

  mkcl_get_interrupt_status(env, &old_intr);
  mkcl_disable_interrupts(env);
  while ((fflush(f) == EOF) && restartable_io_error(env, strm, &old_intr));
  mkcl_set_interrupt_status(env, &old_intr);
}

#define io_stream_finish_output io_stream_force_output

static bool
io_stream_interactive_p(MKCL, mkcl_object strm)
{
  FILE *f = MKCL_IO_STREAM_FILE(strm);
  return isatty(fileno(f));
}

static mkcl_object
io_stream_length(MKCL, mkcl_object strm)
{
  FILE *f = MKCL_IO_STREAM_FILE(strm);
  mkcl_object output = mkcl_file_len(env, fileno(f));
  if (strm->stream.byte_size != 8) {
    mkcl_index bs = strm->stream.byte_size;
    output = mkcl_floor2(env, output, MKCL_MAKE_FIXNUM(bs/8));
    if (MKCL_VALUES(1) != MKCL_MAKE_FIXNUM(0)) {
      mkcl_FEerror(env, "File length is not on byte boundary", 0);
    }
  }
  return output;
}

static mkcl_object
io_stream_get_position(MKCL, mkcl_object strm)
{
  FILE *f = MKCL_IO_STREAM_FILE(strm);
  mkcl_object output;
  mkcl_off_t offset;

  MKCL_LIBC_NO_INTR(env, offset = mkcl_ftello(f));
  if (offset < 0)
    io_error(env, strm);
  if (sizeof(mkcl_off_t) == sizeof(long)) {
    output = mkcl_make_integer(env, offset);
  } else {
    output = mkcl_off_t_to_integer(env, offset);
  }
  {
    /* If there are unread octets, we return the position at which
     * these bytes begin! */
    mkcl_object l = strm->stream.byte_stack;
    while (MKCL_CONSP(l)) {
      output = mkcl_one_minus(env, output);
      l = MKCL_CONS_CDR(l);
    }
  }
  if (strm->stream.byte_size != 8) {
    output = mkcl_floor2(env, output, MKCL_MAKE_FIXNUM(strm->stream.byte_size / 8));
  }
  return output;
}

static mkcl_object
io_stream_set_position(MKCL, mkcl_object strm, mkcl_object large_disp)
{
  FILE *f = MKCL_IO_STREAM_FILE(strm);
  mkcl_off_t disp;
  int mode;
  if (mkcl_Null(large_disp)) {
    disp = 0;
    mode = SEEK_END;
  } else {
    if (strm->stream.byte_size != 8) {
      large_disp = mkcl_times(env, large_disp,
			      MKCL_MAKE_FIXNUM(strm->stream.byte_size / 8));
    }
    disp = mkcl_integer_to_off_t(env, large_disp);
    mode = SEEK_SET;
  }
  MKCL_LIBC_NO_INTR(env, mode = mkcl_fseeko(f, disp, mode));
  return mode ? mk_cl_Cnil : mk_cl_Ct;
}

static int
io_stream_column(MKCL, mkcl_object strm)
{
  return MKCL_IO_STREAM_COLUMN(strm);
}

static mkcl_object
io_stream_close(MKCL, mkcl_object strm)
{
  FILE *f = MKCL_IO_STREAM_FILE(strm);
  int failed;

  if (f == NULL)
    wrong_file_handler(env, strm);
  if (mkcl_output_stream_p(env, strm)) {
    mkcl_force_output(env, strm);
  }
  mkcl_safe_fclose(env, f, strm);

  return generic_close(env, strm);
}

/*
 * Specialized sequence operations
 */

#define io_stream_read_vector io_file_read_vector
#define io_stream_write_vector io_file_write_vector

static const struct mkcl_file_ops io_stream_ops = {
  io_stream_write_octet,
  io_stream_read_octet,

  generic_write_byte_be,
  generic_read_byte_be,

  eformat_read_char,
  eformat_write_char,
  eformat_unread_char,
  generic_peek_char,

  io_file_read_vector,
  io_file_write_vector,

  io_stream_listen,
  io_stream_clear_input,
  io_stream_clear_output,
  io_stream_finish_output,
  io_stream_force_output,

  generic_always_true, /* input_p */
  generic_always_true, /* output_p */
  io_stream_interactive_p,
  io_file_element_type,

  io_stream_length,
  io_stream_get_position,
  io_stream_set_position,
  io_stream_column,
  io_stream_close
};

static const struct mkcl_file_ops output_stream_ops = {
  output_stream_write_octet,
  not_input_read_octet,

  generic_write_byte_be,
  not_input_read_byte,

  not_input_read_char,
  eformat_write_char,
  not_input_unread_char,
  not_input_read_char,

  generic_read_vector,
  io_file_write_vector,

  not_input_listen,
  generic_void,
  io_stream_clear_output,
  io_stream_finish_output,
  io_stream_force_output,

  generic_always_false, /* input_p */
  generic_always_true, /* output_p */
  generic_always_false,
  io_file_element_type,

  io_stream_length,
  io_stream_get_position,
  io_stream_set_position,
  io_stream_column,
  io_stream_close
};

static const struct mkcl_file_ops input_stream_ops = {
  not_output_write_octet,
  input_stream_read_octet,

  not_output_write_byte,
  generic_read_byte_be,

  eformat_read_char,
  not_output_write_char,
  eformat_unread_char,
  generic_peek_char,

  io_file_read_vector,
  generic_write_vector,

  io_stream_listen,
  io_stream_clear_input,
  generic_void,
  generic_void,
  generic_void,

  generic_always_true, /* input_p */
  generic_always_false, /* output_p */
  io_stream_interactive_p,
  io_file_element_type,

  io_stream_length,
  io_stream_get_position,
  io_stream_set_position,
  generic_column,
  io_stream_close
};

/**********************************************************************
 * SOCKET STREAMS  
 */

#define socket_stream_element_type io_file_element_type


#if defined(MKCL_WINDOWS)
/* This callback is used as IO completion routine by WSARecv() and WSASend() here below. */
static void CALLBACK _mkcl_socket_io_done(DWORD dwError, DWORD cbTransferred, LPWSAOVERLAPPED lpOverlapped, DWORD dwFlags)
{
  lpOverlapped->hEvent = (HANDLE) (mkcl_index) cbTransferred;
}
#endif

static mkcl_index
socket_stream_read_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  mkcl_index out = 0;
  mkcl_object l;

  for (l = strm->stream.byte_stack; l != mk_cl_Cnil && n > 0; ++out, ++c, --n) {
    *c = mkcl_fixnum_to_word(MKCL_CONS_CAR(l));
    strm->stream.byte_stack = l = MKCL_CONS_CDR(l);
  }
	
  if(n > 0) {
    SOCKET s = (SOCKET) MKCL_IO_FILE_DESCRIPTOR(strm);

    if (INVALID_SOCKET == s) {
      wrong_file_handler(env, strm);
    } else {
#if __unix
      ssize_t len = 0;

      MKCL_LIBC_Zzz(env, @':io', len = recv(s, (char *) c, n, MSG_WAITALL));
      if ((len == SOCKET_ERROR) && (errno != EINTR))
	{
#if 0
	  fprintf(stderr, "\n;; MKCL: recv() on (%d) failed, errno = %d!\n", env->own_thread->thread.tid, errno);
	  fflush(stderr);
#endif	  
	  len = 0; 
	  socket_error(env, "Cannot read bytes from socket", strm);
	}
      mk_mt_test_for_thread_shutdown(env);
      out += len;

#elif defined(MKCL_WINDOWS)
      int rc;
      BOOL ok;
      mkcl_index len = 0;
      WSABUF DataBuf = { n, c };
      DWORD BytesRecv = 0;
      DWORD Flags = 0;
      WSAOVERLAPPED RecvOverlapped = { 0 };

      MKCL_LIBC_NO_INTR(env, rc = WSARecv(s, &DataBuf, 1, &BytesRecv, &Flags, &RecvOverlapped, _mkcl_socket_io_done));

      if (rc == 0)
	{ len = BytesRecv; }
      else if (rc == SOCKET_ERROR)
	{
	  DWORD wait_val;

	  if (WSAGetLastError() != WSA_IO_PENDING)
	    socket_error(env, "WSARecv() failed on socket", strm); /* Something went wrong with WSARecv(). */

	  do {
	    MKCL_LIBC_Zzz(env, @':io', wait_val = SleepEx(INFINITE, TRUE));
	  } while ((wait_val == WAIT_IO_COMPLETION) && (RecvOverlapped.hEvent == NULL));
	  if (wait_val != WAIT_IO_COMPLETION)
	    mkcl_FEwin32_error(env, "WSARecv() failed to complete properly on socket ~S", 1, strm);

	  mk_mt_test_for_thread_shutdown(env);

	  len = (mkcl_index) RecvOverlapped.hEvent;
	}
      else
	socket_error(env, "WSARecv() failed unexpectedly on socket", strm); /* Something went really wrong with WSARecv(). */
      out +=len;

#if 0
      fprintf(stderr, "\n;; MKCL: WSARecv() on (%d) is done, len = %d, BytesRecv = %d, InternalHigh = %d, rc = %d!",
	      env->own_thread->thread.tid, len, BytesRecv, RecvOverlapped.InternalHigh, rc);
      fflush(stderr);
#endif
#else
#error "Don't know how to read from a socket."
#endif
    }
  }

  return out;
}


static mkcl_index
socket_stream_write_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  mkcl_index out = 0;

  SOCKET s = (SOCKET)MKCL_IO_FILE_DESCRIPTOR(strm);
  if(INVALID_SOCKET == s) {
    wrong_file_handler(env, strm);
  } else {
#if __unix
    do {
      ssize_t res;

      MKCL_LIBC_Zzz(env, @':io', res = send(s, ((char *) c) + out, n, 0));
      if ((res == SOCKET_ERROR) && (errno != EINTR)) {
	socket_error(env, "Cannot write bytes to socket", strm);
	break; /* stop writing */
      } else {			
	out += res;
	n -= res;
      }
      mk_mt_test_for_thread_shutdown(env);
    } while (n > 0);
#elif defined(MKCL_WINDOWS)
      int rc;
      BOOL ok; 
      WSABUF DataBuf = { n, c };
      DWORD BytesSent = 0;
      DWORD Flags = 0;
      WSAOVERLAPPED SendOverlapped = { 0 };

      MKCL_LIBC_NO_INTR(env, rc = WSASend(s, &DataBuf, 1, &BytesSent, Flags, &SendOverlapped, _mkcl_socket_io_done));

      if (rc == 0)
	{ out = BytesSent; }
      else if (rc == SOCKET_ERROR)
	{
	  DWORD wait_val;

	  if (WSAGetLastError() != WSA_IO_PENDING)
	    socket_error(env, "WSASend() failed on socket", strm); /* Something went wrong with WSASend(). */

	  do {
	    MKCL_LIBC_Zzz(env, @':io', wait_val = SleepEx(INFINITE, TRUE));
	  } while ((wait_val == WAIT_IO_COMPLETION) && (SendOverlapped.hEvent == NULL));
	  if (wait_val != WAIT_IO_COMPLETION)
	    mkcl_FEwin32_error(env, "WSASend() failed to complete properly on socket ~S", 1, strm);

	  mk_mt_test_for_thread_shutdown(env);

	  out = (mkcl_index) SendOverlapped.hEvent;
	}
      else
	socket_error(env, "WSASend() failed unexpectedly on socket", strm); /* Something went really wrong with WSASend(). */

#if 0
      fprintf(stderr, "\n;; MKCL: WSASend() on (%d) is done, out = %d, BytesSent = %d, InternalHigh = %d, rc = %d!",
	      env->own_thread->thread.tid, out, BytesSent, SendOverlapped.InternalHigh, rc);
      fflush(stderr);
#endif
#else
#error "Don't know how to write to a socket."
#endif
  }
  return out;
}

static int
socket_stream_listen(MKCL, mkcl_object strm) 
{
  SOCKET s = (SOCKET)MKCL_IO_FILE_DESCRIPTOR(strm);
  if (INVALID_SOCKET == s) {
    wrong_file_handler(env, strm);
  } else {
    if (MKCL_CONSP(strm->stream.object0))
      return MKCL_LISTEN_AVAILABLE;
    else {
      struct timeval tv = { 0, 0 };
      fd_set fds;
      mkcl_index result;
			
      FD_ZERO( &fds );
      FD_SET(s, &fds);
      MKCL_LIBC_NO_INTR(env, (result = select(0, &fds, NULL, NULL,  &tv)));
      if (result == SOCKET_ERROR)
	socket_error(env, "Cannot listen on socket", strm);
      return ((result > 0) ? MKCL_LISTEN_AVAILABLE : MKCL_LISTEN_NO_CHAR );
    }
  }
}

static void
socket_stream_clear_input(MKCL, mkcl_object strm)
{
  while (socket_stream_listen(env, strm) == MKCL_LISTEN_AVAILABLE) {
    eformat_read_char(env, strm);
  }
}

static mkcl_object
socket_stream_close(MKCL, mkcl_object strm)
{
  SOCKET s = (SOCKET) MKCL_IO_FILE_DESCRIPTOR(strm);
  int failed;

#if defined(MKCL_WINDOWS)
  MKCL_LIBC_NO_INTR(env, failed = closesocket(s));
  if (failed == SOCKET_ERROR)
    socket_error(env, "Cannot close socket", strm);
#else
  MKCL_LIBC_NO_INTR(env, failed = close(s));
#endif
  if (failed < 0){
    mkcl_FElibc_stream_error(env, strm, "Cannot close socket stream.", 0);
  }
  MKCL_IO_FILE_DESCRIPTOR(strm) = INVALID_SOCKET;
  return generic_close(env, strm);
}

static const struct mkcl_file_ops socket_stream_io_ops = {
  socket_stream_write_octet,
  socket_stream_read_octet,

  generic_write_byte_be,
  generic_read_byte_be,

  eformat_read_char,
  eformat_write_char,
  eformat_unread_char,
  generic_peek_char,

  generic_read_vector,
  generic_write_vector,

  socket_stream_listen,
  socket_stream_clear_input,
  generic_void,
  generic_void,
  generic_void,

  generic_always_true, /* input_p */
  generic_always_true, /* output_p */
  generic_always_false,
  socket_stream_element_type,

  not_a_file_stream,
  generic_always_nil, /* get_position */
  generic_set_position,
  generic_column,

  socket_stream_close
};

static const struct mkcl_file_ops socket_stream_output_ops = {
  socket_stream_write_octet,
  not_input_read_octet,

  generic_write_byte_be,
  not_input_read_byte,

  not_input_read_char,
  eformat_write_char,
  not_input_unread_char,
  generic_peek_char,

  generic_read_vector,
  generic_write_vector,

  not_input_listen,
  not_input_clear_input,
  generic_void,
  generic_void,
  generic_void,

  generic_always_false, /* input_p */
  generic_always_true, /* output_p */
  generic_always_false,
  socket_stream_element_type,

  not_a_file_stream,
  generic_always_nil, /* get_position */
  generic_set_position,
  generic_column,

  socket_stream_close
};

static const struct mkcl_file_ops socket_stream_input_ops = {
  not_output_write_octet,
  socket_stream_read_octet,

  not_output_write_byte,
  generic_read_byte_be,

  eformat_read_char,
  not_output_write_char,
  eformat_unread_char,
  generic_peek_char,

  generic_read_vector,
  generic_write_vector,

  socket_stream_listen,
  socket_stream_clear_input,
  not_output_clear_output,
  not_output_finish_output,
  not_output_force_output,

  generic_always_true, /* input_p */
  generic_always_false, /* output_p */
  generic_always_false,
  socket_stream_element_type,

  not_a_file_stream,
  generic_always_nil, /* get_position */
  generic_set_position,
  generic_column,

  socket_stream_close
};



mkcl_object
mk_si_get_buffering_mode(MKCL, mkcl_object stream)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(stream) != mkcl_t_stream) {
    mkcl_FEtype_error_stream(env, stream);
  }
  if (MKCL_STREAM_IS_C_STDIO_BASED_P(stream))  
    { @(return stream->stream.buffering_mode); }
  else
    { @(return @':invalid'); }
}

mkcl_object
mk_si_set_buffering_mode(MKCL, mkcl_object stream, mkcl_object buffer_mode_symbol)
{
  int buffer_mode;

  mkcl_call_stack_check(env);
  if (mkcl_type_of(stream) != mkcl_t_stream) {
    mkcl_FEtype_error_stream(env, stream);
  }
  if (buffer_mode_symbol == @':none' || mkcl_Null(buffer_mode_symbol)) {
    buffer_mode = _IONBF;
  } else if (buffer_mode_symbol == @':full' || buffer_mode_symbol == mk_cl_Ct
	     || buffer_mode_symbol == @':fully-buffered') {
    buffer_mode = _IOFBF;
  } else if (buffer_mode_symbol == @':line' || buffer_mode_symbol == @':line-buffered') {
    buffer_mode = _IOLBF;
  } else {
    mkcl_FEerror(env, "Not a valid buffering mode: ~A", 1, buffer_mode_symbol);
  }
  if (MKCL_STREAM_IS_C_STDIO_BASED_P(stream))
    {
      FILE *fp = MKCL_IO_STREAM_FILE(stream);

      if (buffer_mode != _IONBF)
	{
	  mkcl_index buffer_size = BUFSIZ;
	  char *new_buffer = mkcl_alloc_atomic(env, buffer_size);
	  stream->stream.buffer = new_buffer;
	  errno = 0;
	  if (setvbuf(fp, new_buffer, buffer_mode, buffer_size))
	    mkcl_FElibc_stream_error(env, stream, "setvbuf failed in si:set-buffering-mode for mode ~S", 1, buffer_mode_symbol);
	  stream->stream.buffering_mode = buffer_mode_symbol;
	}
      else
	{
	  errno = 0;
	  if (setvbuf(fp, 0, _IONBF, 0))
	    mkcl_FElibc_stream_error(env, stream, "setvbuf failed in si:set-buffering-mode for mode ~S", 1, buffer_mode_symbol);
	  stream->stream.buffering_mode = buffer_mode_symbol;
	}
    }
  else
    mkcl_FEerror(env, "Do not know how to set buffering mode on stream: ~S", 1, stream);

  @(return stream);
}

static mkcl_object
make_stream_from_FILE(MKCL, mkcl_object fname, FILE *f, enum mkcl_smmode smm,
		      mkcl_word byte_size, mkcl_object external_format)
{
  mkcl_stream_flag_set flags;
  mkcl_object stream = alloc_stream(env);

  stream->stream.mode = smm;
  stream->stream.closed = 0;
  switch (smm) {
  case mkcl_smm_io:
    stream->stream.ops = duplicate_dispatch_table(env, &io_stream_ops);
    break;
  case mkcl_smm_input:
    stream->stream.ops = duplicate_dispatch_table(env, &input_stream_ops);
    break;
  case mkcl_smm_output:
    stream->stream.ops = duplicate_dispatch_table(env, &output_stream_ops);
    break;
  default:
    mkcl_FEerror(env, "Not a valid mode ~D for make_stream_from_FILE()", 1, MKCL_MAKE_FIXNUM(smm));
  }

  flags = MKCL_STREAM_C_STDIO_STREAM;

  set_stream_elt_type(env, stream, byte_size, flags, external_format);
  MKCL_IO_STREAM_FILENAME(stream) = fname; /* not really used */
  MKCL_IO_STREAM_COLUMN(stream) = 0;
  MKCL_IO_STREAM_FILE(stream) = f;
  stream->stream.last_op = 0;
  mk_si_set_finalizer(env, stream, mk_cl_Ct);
  return stream;
}

  /*
   * POSIX specifies that the "b" flag is ignored. This is good, because
   * under MSDOS and Apple's OS we need to open text files in binary mode,
   * so that we get both the carriage return and the linefeed characters.
   * Otherwise, it would be complicated to implement file-position and
   * seek operations.
   */
#ifdef MKCL_WINDOWS
#define OPEN_R	L"rb"
#define OPEN_W	L"wb"
#define OPEN_RW	L"w+b"
#define OPEN_A	L"ab"
#define OPEN_RA	L"a+b"
#else
#define OPEN_R	"rb"
#define OPEN_W	"wb"
#define OPEN_RW	"w+b"
#define OPEN_A	"ab"
#define OPEN_RA	"a+b"
#endif


mkcl_object
mkcl_make_stream_from_fd(MKCL, mkcl_object fname, mkcl_index fd, enum mkcl_smmode smm,
			 mkcl_object element_type, mkcl_object external_format)
{

  mkcl_word byte_size = normalize_stream_element_type(env, element_type);

  if (smm == mkcl_smm_input || smm == mkcl_smm_output || smm == mkcl_smm_io) /* C stdio based */
    {
#ifdef MKCL_WINDOWS
      const wchar_t *mode;	/* file open mode */
#else
      const char *mode;		/* file open mode */
#endif
      FILE *fp;			/* file pointer */

      switch(smm) {
      case mkcl_smm_input: mode = OPEN_R; break;
      case mkcl_smm_output: mode = OPEN_W; break;
      case mkcl_smm_io: mode = OPEN_RW; break;
      default:
	mkcl_FEerror(env, "mkcl_make_stream_from_fd(): wrong mode", 0);
      }

#ifdef MKCL_WINDOWS
      MKCL_LIBC_NO_INTR(env, (fp = _wfdopen(fd, mode)));
#else
      MKCL_LIBC_NO_INTR(env, (fp = fdopen(fd, mode)));
#endif
      if (fp == NULL)
	mkcl_FElibc_error(env, "mkcl_make_stream_from_fd(): Unable to create stream for file descriptor ~D",
			  1, mkcl_make_integer(env, fd));
    
      return make_stream_from_FILE(env, fname, fp, smm, byte_size, external_format);
    }
  else
    {
      return make_file_stream_from_fd(env, fname, fd, smm, byte_size, external_format);
    }
}


int
mkcl_stream_to_handle(MKCL, mkcl_object s, bool output)
{
 BEGIN:
  if (mkcl_type_of(s) != mkcl_t_stream)
    return -1;
  switch ((enum mkcl_smmode)s->stream.mode) {
  case mkcl_smm_input:
    if (output) return -1;
    else return fileno((FILE *) MKCL_IO_STREAM_FILE(s));
  case mkcl_smm_input_file:
    if (output) return -1;
    else return MKCL_IO_FILE_DESCRIPTOR(s);
  case mkcl_smm_output:
    if (!output) return -1;
    else return fileno((FILE *) MKCL_IO_STREAM_FILE(s));
  case mkcl_smm_output_file:
    if (!output) return -1;
    return MKCL_IO_FILE_DESCRIPTOR(s);
  case mkcl_smm_io:
    return fileno((FILE *) MKCL_IO_STREAM_FILE(s));
  case mkcl_smm_io_file:
    return MKCL_IO_FILE_DESCRIPTOR(s);
  case mkcl_smm_synonym:
    s = MKCL_SYNONYM_STREAM_STREAM(env, s);
    goto BEGIN;
  case mkcl_smm_two_way:
    s = output ? MKCL_TWO_WAY_STREAM_OUTPUT(s) : MKCL_TWO_WAY_STREAM_INPUT(s);
    goto BEGIN;
  case mkcl_smm_echo:
    s = output ? MKCL_ECHO_STREAM_OUTPUT(s) : MKCL_ECHO_STREAM_INPUT(s);
    goto BEGIN;
  case mkcl_smm_input_socket:
    if (output) return -1;
    else return MKCL_IO_FILE_DESCRIPTOR(s);
  case mkcl_smm_output_socket:
    if (!output) return -1;
    else return MKCL_IO_FILE_DESCRIPTOR(s);
  case mkcl_smm_io_socket:
    return MKCL_IO_FILE_DESCRIPTOR(s);

  case mkcl_smm_concatenated: /* many to one */
  case mkcl_smm_broadcast: /* one to many */
  case mkcl_smm_string_input:
  case_mkcl_smm_string_output:
  case mkcl_smm_probe:
  default:
    mkcl_FEerror(env, "mkcl_stream_to_handle: invalid stream type: ~S", 1, s);
  }
}


/**********************************************************************
 * MEDIUM LEVEL INTERFACE
 */

static mkcl_index
mkcl_read_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  return stream_dispatch_table(env, strm)->read_octet(env, strm, c, n);
}

static mkcl_index
mkcl_write_octet(MKCL, mkcl_object strm, unsigned char *c, mkcl_index n)
{
  return stream_dispatch_table(env, strm)->write_octet(env, strm, c, n);
}

mkcl_character
mkcl_read_char(MKCL, mkcl_object strm)
{
  return stream_dispatch_table(env, strm)->read_char(env, strm);
}

mkcl_character
mkcl_read_char_noeof(MKCL, mkcl_object strm)
{
  mkcl_character c = mkcl_read_char(env, strm);
  if (c == EOF)
    mkcl_FEend_of_file(env, strm);
  return c;
}

mkcl_object
mkcl_read_byte(MKCL, mkcl_object strm)
{
  return stream_dispatch_table(env, strm)->read_byte(env, strm);
}

void
mkcl_write_byte(MKCL, mkcl_object c, mkcl_object strm)
{
  stream_dispatch_table(env, strm)->write_byte(env, c, strm);
}

mkcl_character
mkcl_write_char(MKCL, mkcl_character c, mkcl_object strm)
{
  return stream_dispatch_table(env, strm)->write_char(env, strm, c);
}

void
mkcl_unread_char(MKCL, mkcl_character c, mkcl_object strm)
{
  stream_dispatch_table(env, strm)->unread_char(env, strm, c);
}

int
mkcl_listen_stream(MKCL, mkcl_object strm)
{
  return stream_dispatch_table(env, strm)->listen(env, strm);
}

void
mkcl_clear_input(MKCL, mkcl_object strm)
{
  stream_dispatch_table(env, strm)->clear_input(env, strm);
}

void
mkcl_clear_output(MKCL, mkcl_object strm)
{
  stream_dispatch_table(env, strm)->clear_output(env, strm);
}

void
mkcl_force_output(MKCL, mkcl_object strm)
{
  stream_dispatch_table(env, strm)->force_output(env, strm);
}

void
mkcl_finish_output(MKCL, mkcl_object strm)
{
  stream_dispatch_table(env, strm)->finish_output(env, strm);
}

int
mkcl_file_column(MKCL, mkcl_object strm)
{
  return stream_dispatch_table(env, strm)->column(env, strm);
}

mkcl_object
mkcl_file_length(MKCL, mkcl_object strm)
{
  return stream_dispatch_table(env, strm)->length(env, strm);
}

mkcl_object
mkcl_file_position(MKCL, mkcl_object strm)
{
  return stream_dispatch_table(env, strm)->get_position(env, strm);
}

mkcl_object
mkcl_file_position_set(MKCL, mkcl_object strm, mkcl_object pos)
{
  return stream_dispatch_table(env, strm)->set_position(env, strm, pos);
}

bool
mkcl_input_stream_p(MKCL, mkcl_object strm)
{
  return stream_dispatch_table(env, strm)->input_p(env, strm);
}

bool
mkcl_output_stream_p(MKCL, mkcl_object strm)
{
  return stream_dispatch_table(env, strm)->output_p(env, strm);
}

mkcl_object
mkcl_stream_element_type(MKCL, mkcl_object strm)
{
  return stream_dispatch_table(env, strm)->element_type(env, strm);
}

bool
mkcl_interactive_stream_p(MKCL, mkcl_object strm)
{
  return stream_dispatch_table(env, strm)->interactive_p(env, strm);
}

/*
 * mkcl_read_char(s) tries to read a character from the stream S. It outputs
 * either the code of the character read, or EOF. Whe compiled with
 * CLOS-STREAMS and S is an instance object, STREAM-READ-CHAR is invoked
 * to retrieve the character. Then STREAM-READ-CHAR should either
 * output the character, or NIL, indicating EOF.
 *
 * INV: mkcl_read_char(strm) checks the type of STRM.
 */
mkcl_character
mkcl_peek_char(MKCL, mkcl_object strm)
{
  return stream_dispatch_table(env, strm)->peek_char(env, strm);
}

/*******************************tl***************************************
 * SEQUENCES I/O
 */

void
mkcl_write_cstr(MKCL, const char *s, mkcl_object strm)
{
  while (*s != '\0')
    mkcl_write_char(env, *s++, strm);
}

static mkcl_index
compute_char_size(MKCL, mkcl_object stream, mkcl_character c)
{
  unsigned char buffer[5];
  mkcl_index l = 0;
  if (c == MKCL_CHAR_CODE_NEWLINE) {
    int flags = stream->stream.flags;
    if (flags & MKCL_STREAM_CR) {
      l += stream->stream.encoder(env, stream, buffer, MKCL_CHAR_CODE_RETURN);
      if (flags & MKCL_STREAM_LF)
	l += stream->stream.encoder(env, stream, buffer,
				    MKCL_CHAR_CODE_LINEFEED);
    } else {
      l += stream->stream.encoder(env, stream, buffer, MKCL_CHAR_CODE_LINEFEED);
    }
  } else {
    l += stream->stream.encoder(env, stream, buffer, c);
  }
  return l;
}

mkcl_object
mk_cl_file_string_length(MKCL, mkcl_object stream, mkcl_object string)
{
  mkcl_word l = 0;

  mkcl_call_stack_check(env);
 BEGIN:
  if (MKCL_INSTANCEP(stream)) {
    @(return mk_cl_Cnil);
  }

  if (mkcl_type_of(stream) != mkcl_t_stream) {
    not_a_file_stream(env, stream);
  }
  if (stream->stream.mode == mkcl_smm_broadcast) {
    stream = MKCL_BROADCAST_STREAM_LIST(stream);
    if (mkcl_Null(stream)) {
      @(return MKCL_MAKE_FIXNUM(1));
    } else {
      goto BEGIN;
    }
  }
  if (!MKCL_BASIC_STREAM_P(stream)) {
    not_a_file_stream(env, stream);
  }
  switch (mkcl_type_of(string)) {
  case mkcl_t_string:
  case mkcl_t_base_string: {
    mkcl_index i;
    for (i = 0; i < string->base_string.fillp; i++) {
      l += compute_char_size(env, stream, mkcl_char(env, string, i));
    }
    break;
  }
  case mkcl_t_character:
    l = compute_char_size(env, stream, MKCL_CHAR_CODE(string));
    break;
  default:
    mkcl_FEwrong_type_argument(env, @'string', string);
  }
  @(return MKCL_MAKE_FIXNUM(l));
}

mkcl_object
mk_si_do_write_sequence(MKCL, mkcl_object seq, mkcl_object stream, mkcl_object s, mkcl_object e)
{
  const struct mkcl_file_ops *ops;
  mkcl_index start,limit,end;

  mkcl_call_stack_check(env);
  /* Since we have called mkcl_length(), we know that SEQ is a valid
     sequence. Therefore, we only need to check the type of the
     object, and seq == mk_cl_Cnil i.f.f. t = mkcl_t_symbol */
  limit = mkcl_length(env, seq);
  start = mkcl_fixnum_in_range(env, @'write-sequence',"start",s,0,limit); /* probably wrong on large seq. JCB */
  if (e == mk_cl_Cnil) {
    end = limit;
  } else {
    end = mkcl_fixnum_in_range(env, @'write-sequence',"end",e,0,limit); /* probably wrong on large seq. JCB */
  }
  if (end <= start) {
    goto OUTPUT;
  }
  ops = stream_dispatch_table(env, stream);
  if (MKCL_LISTP(seq)) {
    mkcl_object elt_type = mk_cl_stream_element_type(env, stream);
    bool ischar = (elt_type == @'base-char') || (elt_type == @'character');
    mkcl_object s = mkcl_nthcdr(env, start, seq);
    mkcl_loop_for_in(env, s) {
      if (start < end) {
	mkcl_object elt = MKCL_CAR(s);
	if (ischar)
	  ops->write_char(env, stream, mkcl_char_code(env, elt));
	else
	  ops->write_byte(env, elt, stream);
	start++;
      } else {
	goto OUTPUT;
      }
    } mkcl_end_loop_for_in;
  } else {
    ops->write_vector(env, stream, seq, start, end);
  }
 OUTPUT:
  @(return seq);
}

mkcl_object
mk_si_do_read_sequence(MKCL, mkcl_object seq, mkcl_object stream, mkcl_object s, mkcl_object e)
{
  const struct mkcl_file_ops *ops;
  mkcl_index start,limit,end;

  mkcl_call_stack_check(env);
  /* Since we have called mkcl_length(), we know that SEQ is a valid
     sequence. Therefore, we only need to check the type of the
     object, and seq == mk_cl_Cnil i.f.f. t = mkcl_t_symbol */
  limit = mkcl_length(env, seq);
  start = mkcl_fixnum_in_range(env, @'read-sequence',"start",s,0,limit); /* probably wrong on large seq. JCB */
  if (e == mk_cl_Cnil) {
    end = limit;
  } else {
    end = mkcl_fixnum_in_range(env, @'read-sequence',"end",e,0,limit); /* probably wrong on large seq. JCB */
  }
  if (end <= start) {
    goto OUTPUT;
  }
  ops = stream_dispatch_table(env, stream);
  if (MKCL_LISTP(seq)) {
    mkcl_object elt_type = mk_cl_stream_element_type(env, stream);
    bool ischar = (elt_type == @'base-char') || (elt_type == @'character');
    seq = mkcl_nthcdr(env, start, seq);
    mkcl_loop_for_in(env, seq) {
      if (start >= end) {
	goto OUTPUT;
      } else {
	mkcl_object c;
	if (ischar) {
	  int i = ops->read_char(env, stream);
	  if (i < 0) goto OUTPUT;
	  c = MKCL_CODE_CHAR(i);
	} else {
	  c = ops->read_byte(env, stream);
	  if (c == mk_cl_Cnil) goto OUTPUT;
	}
	MKCL_RPLACA(seq, c);
	start++;
      }
    } mkcl_end_loop_for_in;
  } else {
    start = ops->read_vector(env, stream, seq, start, end);
  }
 OUTPUT:
  @(return MKCL_MAKE_FIXNUM(start));
}

/**********************************************************************
 * LISP LEVEL INTERFACE
 */

mkcl_object
mk_si_file_column(MKCL, mkcl_object strm)
{
  mkcl_call_stack_check(env);
  @(return MKCL_MAKE_FIXNUM(mkcl_file_column(env, strm)));
}

mkcl_object
mk_cl_file_length(MKCL, mkcl_object strm)
{
  mkcl_call_stack_check(env);
  @(return mkcl_file_length(env, strm));
}

@(defun file-position (file_stream &o position)
  mkcl_object output;
@
  if (mkcl_Null(position)) {
    output = mkcl_file_position(env, file_stream);
  } else {
    if (position == @':start') {
      position = MKCL_MAKE_FIXNUM(0);
    } else if (position == @':end') {
      position = mk_cl_Cnil;
    }
    output = mkcl_file_position_set(env, file_stream, position);
  }
  @(return output);
@)

mkcl_object
mk_cl_input_stream_p(MKCL, mkcl_object strm)
{
  mkcl_call_stack_check(env);
  @(return (mkcl_input_stream_p(env, strm) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_output_stream_p(MKCL, mkcl_object strm)
{
  mkcl_call_stack_check(env);
  @(return (mkcl_output_stream_p(env, strm) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_interactive_stream_p(MKCL, mkcl_object strm)
{
  mkcl_call_stack_check(env);
  @(return (stream_dispatch_table(env, strm)->interactive_p(env, strm) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_open_stream_p(MKCL, mkcl_object strm)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(strm) != mkcl_t_stream)
    mkcl_FEwrong_type_argument(env, @'stream', strm);
  @(return (strm->stream.closed ? mk_cl_Cnil : mk_cl_Ct));
}

mkcl_object
mk_cl_stream_element_type(MKCL, mkcl_object strm)
{
  mkcl_call_stack_check(env);
  @(return mkcl_stream_element_type(env, strm));
}

mkcl_object
mk_cl_stream_external_format(MKCL, mkcl_object strm)
{
  mkcl_object output;
  mkcl_type t;

  mkcl_call_stack_check(env);
 AGAIN:
  t= mkcl_type_of(strm);
  if (t == mkcl_t_instance)
    { @(return @':default'); }
  else
    if (t != mkcl_t_stream)
      mkcl_FEwrong_type_argument(env, @'stream', strm);
  if (strm->stream.mode == mkcl_smm_synonym) {
    strm = MKCL_SYNONYM_STREAM_STREAM(env, strm);
    goto AGAIN;
  }
  output = strm->stream.format;
  @(return output);
}

mkcl_object
mk_cl_streamp(MKCL, mkcl_object strm)
{
  mkcl_call_stack_check(env);
  if (MKCL_INSTANCEP(strm)) {
    return mkcl_funcall1(env, @+'gray::streamp', strm);
  }
  @(return ((mkcl_type_of(strm) == mkcl_t_stream) ? mk_cl_Ct : mk_cl_Cnil));
}

/**********************************************************************
 * OTHER TOOLS
 */

mkcl_object
mk_si_copy_stream(MKCL, mkcl_object in, mkcl_object out)
{
  mkcl_character c;

  mkcl_call_stack_check(env);
  for (c = mkcl_read_char(env, in); c != EOF; c = mkcl_read_char(env, in)) {
    mkcl_write_char(env, c, out);
  }
  mkcl_force_output(env, out);
  @(return mk_cl_Ct);
}


/**********************************************************************
 * FILE OPENING AND CLOSING
 */

static mkcl_word
normalize_stream_element_type(MKCL, mkcl_object element_type)
{
  mkcl_word sign = 0;
  mkcl_index size;

  if (element_type == @'signed-byte') {
    return -8;
  } else if (element_type == @'unsigned-byte' || element_type == @':default') {
    return 8;
  } else if (mkcl_Null(element_type)) {
    return 0; /* Text stream */
  } else if (element_type == @'base-char' || element_type == @'character') {
    return 0; /* Text stream */
  } else if (mkcl_funcall2(env, @+'subtypep', element_type, @'character') != mk_cl_Cnil) {
    return 0; /* Text stream */
  } else if (mkcl_funcall2(env, @+'subtypep', element_type, @'unsigned-byte') != mk_cl_Cnil) {
    sign = +1;
  } else if (mkcl_funcall2(env, @+'subtypep', element_type, @'signed-byte') != mk_cl_Cnil) {
    sign = -1;
  } else {
    mkcl_FEerror(env, "Not a valid stream element type: ~A", 1, element_type);
  }
  if (MKCL_CONSP(element_type)) {
    if (MKCL_CAR(element_type) == @'unsigned-byte')
      return mkcl_integer_to_index(env, mk_cl_cadr(env, element_type));
    if (MKCL_CAR(element_type) == @'signed-byte')
      return -mkcl_integer_to_index(env, mk_cl_cadr(env, element_type));
  }
  for (size = 8; 1; size++) {
    mkcl_object type
      = mk_cl_list(env, 2, ((sign>0) ? @'unsigned-byte' : @'signed-byte'), MKCL_MAKE_FIXNUM(size));

    if (mkcl_funcall2(env, @+'subtypep', element_type, type) != mk_cl_Cnil) {
      return size * sign;
    }
  }
  mkcl_FEerror(env, "Not a valid stream element type: ~A", 1, element_type);
}

mkcl_object
mkcl_open_stream(MKCL, mkcl_object fn, enum mkcl_smmode smm,
		 mkcl_object if_exists, mkcl_object if_does_not_exist,
		 mkcl_object element_type, mkcl_object external_format)
{
  int flags = 0;
  mkcl_word byte_size;
  mkcl_object x;
  int f;
#if defined(MKCL_WINDOWS)
  int mode = _S_IREAD | _S_IWRITE;
#else
  mode_t mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
#endif
  mkcl_object filename = mk_si_coerce_to_filename(env, fn);
  mkcl_dynamic_extent_OSstring(env, os_filename, filename);
  bool appending = 0;
  bool file_already_exists = mkcl_probe_file(env, os_filename, TRUE);

  if (smm == mkcl_smm_input || smm == mkcl_smm_input_file || smm == mkcl_smm_probe) {
    if (!file_already_exists)
      {
	if (if_does_not_exist == @':error') {
	  mkcl_FEcannot_open(env, fn);
	} else if (if_does_not_exist == @':create') {
#ifdef MKCL_WINDOWS
	  MKCL_LIBC_NO_INTR(env, f = _wopen(mkcl_OSstring_self(os_filename), O_WRONLY|O_CREAT|_O_BINARY, mode));
#else
	  MKCL_LIBC_NO_INTR(env, f = open(mkcl_OSstring_self(os_filename), O_WRONLY|O_CREAT, mode));
#endif
	  if (f < 0) mkcl_FEcannot_open(env, fn);
	  mkcl_safe_close(env, f, mk_cl_Cnil);
	} else if (mkcl_Null(if_does_not_exist)) {
	  return mk_cl_Cnil;
	} else {
	  mkcl_FEerror(env, "Invalid value op option ~A: ~A", 2, @':if-does-not-exist', if_does_not_exist);
	}
      }
#ifdef MKCL_WINDOWS
    MKCL_LIBC_NO_INTR(env, f = _wopen(mkcl_OSstring_self(os_filename), O_RDONLY|_O_BINARY, mode));
#else
    MKCL_LIBC_NO_INTR(env, f = open(mkcl_OSstring_self(os_filename), O_RDONLY, mode));
#endif
    if (f < 0) mkcl_FEcannot_open(env, fn);
  } else if (smm == mkcl_smm_output || smm == mkcl_smm_output_file || smm == mkcl_smm_io || smm == mkcl_smm_io_file) {
    int base = (smm == mkcl_smm_output || smm == mkcl_smm_output_file) ? O_WRONLY : O_RDWR;
#ifdef MKCL_WINDOWS
    base |= _O_BINARY; /* On Windows we force open into binary mode since the default is _O_TEXT. */
#endif
    if (if_exists == @':new_version' && if_does_not_exist == @':create')
      file_already_exists = FALSE; /* We need to create anew no matter what. */

    if (file_already_exists) {
      if (if_exists == @':error') {
	mkcl_FEcannot_open(env, fn);
      } else if (if_exists == @':rename') {
	f = mkcl_backup_open(env, filename, base|O_CREAT, mode);
	if (f < 0) mkcl_FEcannot_open(env, fn);
      } else if (if_exists == @':rename_and_delete' ||
		 if_exists == @':new_version' ||
		 if_exists == @':supersede') {
#ifdef MKCL_WINDOWS
	MKCL_LIBC_NO_INTR(env, f = _wopen(mkcl_OSstring_self(os_filename), base|O_TRUNC, mode));
#else
	MKCL_LIBC_NO_INTR(env, f = open(mkcl_OSstring_self(os_filename), base|O_TRUNC, mode));
#endif
	if (f < 0) mkcl_FEcannot_open(env, fn);
      } else if (if_exists == @':overwrite' || if_exists == @':append') {
#ifdef MKCL_WINDOWS
	MKCL_LIBC_NO_INTR(env, f = _wopen(mkcl_OSstring_self(os_filename), base, mode));
#else
	MKCL_LIBC_NO_INTR(env, f = open(mkcl_OSstring_self(os_filename), base, mode));
#endif
	if (f < 0) mkcl_FEcannot_open(env, fn);
	appending = (if_exists == @':append');
      } else if (mkcl_Null(if_exists)) {
	return mk_cl_Cnil;
      } else {
	mkcl_FEerror(env, "Invalid value op option ~A: ~A", 2, @':if-exists', if_exists);
      }
    } else {
      if (if_does_not_exist == @':error') {
	mkcl_FEcannot_open(env, fn);
      } else if (if_does_not_exist == @':create') {
      /* CREATE:	 */
#ifdef MKCL_WINDOWS
	MKCL_LIBC_NO_INTR(env, f = _wopen(mkcl_OSstring_self(os_filename), base | O_CREAT | O_TRUNC, mode));
#else
	MKCL_LIBC_NO_INTR(env, f = open(mkcl_OSstring_self(os_filename), base | O_CREAT | O_TRUNC, mode));
#endif
	if (f < 0) mkcl_FEcannot_open(env, fn);
      } else if (mkcl_Null(if_does_not_exist)) {
	return mk_cl_Cnil;
      } else {
	mkcl_FEerror(env, "Invalid value op option ~A: ~A", 2, @':if-does-not-exist', if_does_not_exist);
      }
    }
  } else {
    mkcl_FEerror(env, "Illegal stream mode for open: ~S", 1, MKCL_MAKE_FIXNUM(smm));
  }

  byte_size = normalize_stream_element_type(env, element_type);

  if (byte_size == 0) flags |= MKCL_STREAM_TEXT;

  switch (smm) {
  case mkcl_smm_input_file:
  case mkcl_smm_output_file:
  case mkcl_smm_io_file:
  case mkcl_smm_probe:
    x = make_file_stream_from_fd(env, fn, f, smm, byte_size, external_format);
    break;
  default:
    {
      FILE *fp;

      switch (smm) {
      case mkcl_smm_input:
#ifdef MKCL_WINDOWS
	MKCL_LIBC_NO_INTR(env, fp = _wfdopen(f, OPEN_R));
#else
	MKCL_LIBC_NO_INTR(env, fp = fdopen(f, OPEN_R));
#endif
	if (fp == NULL) mkcl_FEcannot_open(env, fn);
	break;
      case mkcl_smm_output:
#ifdef MKCL_WINDOWS
	MKCL_LIBC_NO_INTR(env, fp = _wfdopen(f, (appending ? OPEN_A : OPEN_W)));
#else
	MKCL_LIBC_NO_INTR(env, fp = fdopen(f, (appending ? OPEN_A : OPEN_W)));
#endif
	if (fp == NULL) mkcl_FEcannot_open(env, fn);
	break;
      case mkcl_smm_io:
#ifdef MKCL_WINDOWS
	MKCL_LIBC_NO_INTR(env, fp = _wfdopen(f, (appending ? OPEN_RA : OPEN_RW)));
#else
	MKCL_LIBC_NO_INTR(env, fp = fdopen(f, (appending ? OPEN_RA : OPEN_RW)));
#endif
	if (fp == NULL) mkcl_FEcannot_open(env, fn);
	break;
      default: mkcl_FEerror(env, "Illegal stream mode ~S", 1, MKCL_MAKE_FIXNUM(smm));
      }
      x = make_stream_from_FILE(env, fn, fp, smm, byte_size, external_format);
      mk_si_set_buffering_mode(env, x, byte_size ? @':fully-buffered' : @':line-buffered');
    }
  }

  x->stream.flags |= MKCL_STREAM_SEEKABLE; /* Since it came from a pathname naming a file it must be seekable. */

  if (smm == mkcl_smm_probe) {
    mk_cl_close(env, 1, x);
  } else {
    /* Set file pointer to the correct position */
    mkcl_file_position_set(env, x, appending ? mk_cl_Cnil : MKCL_MAKE_FIXNUM(0));
  }
  return x;
}

/* shouldn't it be "character" instead as default value for element-type? JCB */
@(defun open (filename
	      &key
	      (direction @':input')
	      (element_type @'base-char')
	      (if_exists mk_cl_Cnil iesp)
	      (if_does_not_exist mk_cl_Cnil idnesp)
	      (external_format @':default')
	      (stdio_stream mk_cl_Ct)
	      &aux strm)
  enum mkcl_smmode smm;
  int flags = 0;
@
  /* INV: mkcl_open_stream() checks types */
  if (direction == @':input') {
    if (mkcl_Null(stdio_stream))
      smm = mkcl_smm_input_file;
    else
      smm = mkcl_smm_input;
    if (!idnesp)
      if_does_not_exist = @':error';
  } else if (direction == @':output') {
    if (mkcl_Null(stdio_stream))
      smm = mkcl_smm_output_file;
    else
      smm = mkcl_smm_output;
    if (!iesp)
      if_exists = @':new_version';
    if (!idnesp) {
      if (if_exists == @':overwrite' ||
	  if_exists == @':append')
	if_does_not_exist = @':error';
      else
	if_does_not_exist = @':create';
    }
  } else if (direction == @':io') {
    if (mkcl_Null(stdio_stream))
      smm = mkcl_smm_io_file;
    else
      smm = mkcl_smm_io;
    if (!iesp)
      if_exists = @':new_version';
    if (!idnesp) {
      if (if_exists == @':overwrite' ||
	  if_exists == @':append')
	if_does_not_exist = @':error';
      else
	if_does_not_exist = @':create';
    }
  } else if (direction == @':probe') {
    smm = mkcl_smm_probe;
    if (!idnesp)
      if_does_not_exist = mk_cl_Cnil;
  } else {
    mkcl_FEerror(env, "~S is an illegal DIRECTION for OPEN.", 1, direction);
  }

  strm = mkcl_open_stream(env, filename, smm, if_exists, if_does_not_exist, element_type, external_format);

  @(return strm);
@)


@(defun close (strm &key (abort @'nil'))
@
  @(return stream_dispatch_table(env, strm)->close(env, strm));
@)

/**********************************************************************
 * BACKEND
 */

static int
file_listen(MKCL, int fileno)
{
#ifndef MKCL_WINDOWS
# if defined(HAVE_SELECT)
  fd_set fds;
  int retv;
  do {
    struct timeval tv = { 0, 0 };
    FD_ZERO(&fds);
    FD_SET(fileno, &fds);
    MKCL_LIBC_Zzz(env, @':io', retv = select(fileno + 1, &fds, NULL, NULL, &tv));
  } while ((retv < 0) && errno == EINTR);
  mk_mt_test_for_thread_shutdown(env);
  if (retv < 0)
    mkcl_FElibc_error(env, "select() failed in file_listen()", 0);
  else if (retv > 0)
    return MKCL_LISTEN_AVAILABLE;
  else
    return MKCL_LISTEN_NO_CHAR;
# elif defined(FIONREAD)
  {
    long c = 0;
    int val;

    MKCL_LIBC_NO_INTR(env, val = ioctl(fileno, FIONREAD, &c));
    if (val == -1)
      mkcl_FElibc_error(env, "ioctl() failed in file_listen()", 0);
    return (c > 0) ? MKCL_LISTEN_AVAILABLE : MKCL_LISTEN_NO_CHAR;
  }
# endif /* FIONREAD */
#else /* ndef MKCL_WINDOWS */
  HANDLE hnd;
  DWORD f_type;
  BOOL good;

  MKCL_LIBC_NO_INTR(env, f_type = GetFileType(hnd = (HANDLE)_get_osfhandle(fileno)));
  switch (f_type) {
  case FILE_TYPE_CHAR: {
    DWORD dw, dw_read, cm;
    MKCL_LIBC_NO_INTR(env, good = GetNumberOfConsoleInputEvents(hnd, &dw));
    if (good) {
      MKCL_LIBC_NO_INTR(env, good = GetConsoleMode(hnd, &cm));
      if (!good)
	mkcl_FEwin32_error(env, "GetConsoleMode() failed", 0);
      if (dw > 0) {
	PINPUT_RECORD recs 
	  = (PINPUT_RECORD)mkcl_alloc(env, sizeof(INPUT_RECORD)*dw);
	int i;

	MKCL_LIBC_NO_INTR(env, good = PeekConsoleInputW(hnd, recs, dw, &dw_read));
	if (!good)
	  mkcl_FEwin32_error(env, "PeekConsoleInput failed()", 0);
	if (dw_read > 0) {
	  if (cm & ENABLE_LINE_INPUT) {
	    for (i=0; i<dw_read; i++)
	      if (recs[i].EventType == KEY_EVENT &&
		  recs[i].Event.KeyEvent.bKeyDown &&
		  recs[i].Event.KeyEvent.uChar.AsciiChar == 13)
		return MKCL_LISTEN_AVAILABLE;
	  } else {
	    for (i=0; i<dw_read; i++)
	      if (recs[i].EventType == KEY_EVENT &&
		  recs[i].Event.KeyEvent.bKeyDown &&
		  recs[i].Event.KeyEvent.uChar.AsciiChar != 0)
		return MKCL_LISTEN_AVAILABLE;
	  }
	}
      }
      return MKCL_LISTEN_NO_CHAR;
    } else
      mkcl_FEwin32_error(env, "GetNumberOfConsoleInputEvents() failed", 0);
    break;
  }
  case FILE_TYPE_DISK:
    /* use regular file code below */
    break;
  case FILE_TYPE_PIPE: {
    DWORD dw;

    MKCL_LIBC_NO_INTR(env, good = PeekNamedPipe(hnd, NULL, 0, NULL, &dw, NULL));
    if (good)
      return (dw > 0 ? MKCL_LISTEN_AVAILABLE : MKCL_LISTEN_NO_CHAR);
    else 
      {
	DWORD err_val;

	MKCL_LIBC_NO_INTR(env, err_val = GetLastError());
	if (err_val == ERROR_BROKEN_PIPE)
	  return MKCL_LISTEN_EOF;
	else
	  mkcl_FEwin32_error(env, "PeekNamedPipe() failed", 0);
      }
    break;
  }
  default:
    mkcl_FEerror(env, "Unsupported Windows file type: ~A", 1, MKCL_MAKE_FIXNUM(GetFileType(hnd)));
    break;
  }
#endif /* not MKCL_WINDOWS */
  return MKCL_LISTEN_ERROR;
}

static int
flisten(MKCL, FILE *fp)
{
  int aux;
  if (feof(fp))
    return MKCL_LISTEN_EOF;
  else if (ferror(fp))
    return MKCL_LISTEN_ERROR;
#ifdef FILE_CNT
  if (FILE_CNT(fp) > 0)
    return MKCL_LISTEN_AVAILABLE;
#endif
  aux = file_listen(env, fileno(fp));
  if (aux != MKCL_LISTEN_ERROR)
    return aux;
  /* This code is portable, and implements the expected behavior for regular files.
     It will fail on non-seekable streams. */
  {
    /* regular file */
    mkcl_off_t old_pos = mkcl_ftello(fp), end_pos;
    if (mkcl_fseeko(fp, 0, SEEK_END) != 0)
      mkcl_FElibc_error(env, "fseek() returned an error value", 0);
    end_pos = mkcl_ftello(fp);
    if (mkcl_fseeko(fp, old_pos, SEEK_SET) != 0)
      mkcl_FElibc_error(env, "fseek() returned an error value", 0);
    return (end_pos > old_pos ? MKCL_LISTEN_AVAILABLE : MKCL_LISTEN_EOF);
  }
  return MKCL_LISTEN_ERROR; /* This is normally never reached. JCB */
}

static mkcl_object
mkcl_off_t_to_integer(MKCL, mkcl_off_t offset)
{
  return mkcl_make_int64_t(env, offset);
}

static mkcl_off_t
mkcl_integer_to_off_t(MKCL, mkcl_object offset)
{
  return mkcl_to_int64_t(env, offset);
}

static mkcl_object
alloc_stream(MKCL)
{
  mkcl_object x = mkcl_alloc_raw_stream(env);

  x->stream.closed = 0;
  x->stream.file.descriptor = -1;
  x->stream.object0 =
    x->stream.object1 = MKCL_OBJNULL;
  x->stream.int0 = x->stream.int1 = 0;
  x->stream.format = mk_cl_Cnil;
  x->stream.flags = 0;
  x->stream.byte_size = 8;
  x->stream.buffer = NULL;
  x->stream.encoder = NULL;
  x->stream.decoder = NULL;
  x->stream.last_char = EOF;
  x->stream.byte_stack = mk_cl_Cnil;
  x->stream.last_code[0] = x->stream.last_code[1] = EOF;
  x->stream.buffering_mode = mk_cl_Cnil;
  /* format_table ??? */ /* used only in user defined encoder/decoder. */
  /* ops ??? */ /* done by caller of this function. */
  return x;
}

/**********************************************************************
 * ERROR MESSAGES
 */

static mkcl_object
not_a_file_stream(MKCL, mkcl_object strm)
{
  return mk_cl_error(env, 9, @'simple-type-error', @':format-control',
		     mkcl_make_simple_base_string(env, "~A is not an file stream"),
		     @':format-arguments', mk_cl_list(env, 1, strm),
		     @':expected-type', @'file-stream',
		     @':datum', strm);
}

static void
not_an_input_stream(MKCL, mkcl_object strm)
{
  mk_cl_error(env, 9, @'simple-type-error', @':format-control',
	      mkcl_make_simple_base_string(env, "~A is not an input stream"),
	      @':format-arguments', mk_cl_list(env, 1, strm),
	      @':expected-type',
	      mk_cl_list(env, 2, @'satisfies', @'input-stream-p'),
	      @':datum', strm);
}

static void
not_an_output_stream(MKCL, mkcl_object strm)
{
  mk_cl_error(env, 9, @'simple-type-error', @':format-control',
	      mkcl_make_simple_base_string(env, "~A is not an output stream"),
	      @':format-arguments', mk_cl_list(env, 1, strm),
	      @':expected-type', mk_cl_list(env, 2, @'satisfies', @'output-stream-p'),
	      @':datum', strm);
}

static void
not_a_character_stream(MKCL, mkcl_object s)
{
  mk_cl_error(env, 9, @'simple-type-error', @':format-control',
	      mkcl_make_simple_base_string(env, "~A is not a character stream"),
	      @':format-arguments', mk_cl_list(env, 1, s),
	      @':expected-type', @'character',
	      @':datum', mk_cl_stream_element_type(env, s));
}

static void
not_a_binary_stream(MKCL, mkcl_object s)
{
  mk_cl_error(env, 9, @'simple-type-error', @':format-control',
	      mkcl_make_simple_base_string(env, "~A is not a binary stream"),
	      @':format-arguments', mk_cl_list(env, 1, s),
	      @':expected-type', @'integer',
	      @':datum', mk_cl_stream_element_type(env, s));
}

static void
unread_error(MKCL, mkcl_object s)
{
  mkcl_CEerror(env, mk_cl_Ct, "Error when using UNREAD-CHAR on stream ~D", 1, s);
}

static void
unread_twice(MKCL, mkcl_object s)
{
  mkcl_CEerror(env, mk_cl_Ct, "Used UNREAD-CHAR twice on stream ~D", 1, s);
}

static void
maybe_clearerr(MKCL, mkcl_object strm)
{
  int t = strm->stream.mode;
  if (MKCL_STREAM_IS_C_STDIO_BASED_P(strm))
    {
      FILE *f = MKCL_IO_STREAM_FILE(strm);
      if (f != NULL) { MKCL_LIBC_NO_INTR(env, clearerr(f)); }
    }
}

static bool
restartable_io_error(MKCL, mkcl_object strm, mkcl_interrupt_status * old_intr_ptr)
{
  int old_errno = errno;

  maybe_clearerr(env, strm);
  errno = old_errno;
  if (old_errno == EINTR) { /* should we add EAGAIN and EWOULDBLOCK? JCB */
    return true;
  } else {
    if (old_intr_ptr != NULL)
      mkcl_set_interrupt_status(env, old_intr_ptr);
    /* not likely to come back from next one. */
    /* maybe we should call mkcl_FElibc_file_error instead? JCB */
    mkcl_FElibc_stream_error(env, strm, "Read or write operation signaled an error.", 0);
    return false;
  }
}

static void
io_error(MKCL, mkcl_object strm)
{
  int old_errno = errno;

  maybe_clearerr(env, strm);
  errno = old_errno;
  mkcl_FElibc_stream_error(env, strm, "Read or write operation on stream ~S signaled an error.", 0);
}

static void
character_size_overflow(MKCL, mkcl_object strm, mkcl_character c)
{
  mkcl_FEerror(env, "Tried to write a character of code ~D in a ~A stream.",
	       2, MKCL_MAKE_FIXNUM(c), mk_cl_stream_external_format(env, strm));
}

static void
wrong_file_handler(MKCL, mkcl_object strm)
{
  mkcl_FEerror(env, "Internal error: stream ~S has no valid C file handler.", 1, strm);
}


static mkcl_index
encoding_error(MKCL, mkcl_object stream, unsigned char *buffer, mkcl_character ch)
{
  mkcl_object replacement_ch = mkcl_funcall3(env,
					     @'si::stream-encoding-error',
					     stream,
					     mk_cl_stream_external_format(env, stream),
					     MKCL_MAKE_FIXNUM(ch));
  /* Try with supplied replacement character */
  return stream->stream.encoder(env, stream, buffer, mkcl_char_code(env, replacement_ch));
}

static mkcl_character
decoding_error(MKCL, mkcl_object stream, unsigned char *buffer, int length)
{
  mkcl_object octets = mk_cl_Cnil;
  mkcl_object replacement_ch;

  while (length > 0) {
    octets = mkcl_cons(env, MKCL_MAKE_FIXNUM(buffer[--length]), octets);
  }
  replacement_ch = mkcl_funcall3(env,
				 @'si::stream-decoding-error',
				 stream,
				 mk_cl_stream_external_format(env, stream),
				 octets);

  /* Return supplied replacement character */
  return mkcl_char_code(env, replacement_ch);
}

static mkcl_object stream_encoding_error_boot_stub(MKCL, mkcl_object stream, mkcl_object external_format,
						   mkcl_object character_codepoint)
{
  mkcl_object ch;

  if (MKCL_CONSP(external_format)) external_format = mk_cl_car(env, external_format);

  if (external_format == @':ISO-8859-1' || external_format == @':LATIN-1')
    ch = MKCL_CODE_CHAR(0x00bf); /* Inverted question mark */
  else if (external_format == @':US-ASCII' || external_format == @':ASCII')
    ch = MKCL_CODE_CHAR(((mkcl_base_char) '?')); 
  else
    ch = MKCL_CODE_CHAR(0xfffd);  /* Unicode standard replacement character */
  @(return ch);
}

static mkcl_object stream_decoding_error_boot_stub(MKCL, mkcl_object stream, mkcl_object external_format, mkcl_object octets)
{
  mkcl_object ch;

  if (MKCL_CONSP(external_format)) external_format = mk_cl_car(env, external_format);

  if (external_format == @':ISO-8859-1' || external_format == @':LATIN-1')
    ch = MKCL_CODE_CHAR(0x00bf); /* Inverted question mark */
  else if (external_format == @':US-ASCII' || external_format == @':ASCII')
    ch = MKCL_CODE_CHAR(((mkcl_base_char) '?')); 
  else
    ch = MKCL_CODE_CHAR(0xfffd);  /* Unicode standard replacement character */
  @(return ch);
}



static void
socket_error(MKCL, const char *err_msg, mkcl_object strm)
{
#ifdef MKCL_WINDOWS
  DWORD error_code = WSAGetLastError();
  mkcl_object win_msg_obj;
  wchar_t *win_msg;

  if (FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_ALLOCATE_BUFFER,
		     0, error_code, 0, (void*)&win_msg, 0, NULL) == 0)
    win_msg_obj = mkcl_make_simple_base_string(env, "[Unable to get OS error message]");
  else {
    win_msg_obj = mkcl_cstring16_to_string(env, win_msg);
    LocalFree(win_msg);
  }

  mk_cl_error(env, 7,
	      @'si::OS-stream-error',
	      @':stream',
	      strm,
	      @':format-control',
	      mkcl_make_simple_base_string(env, "~?~%OS Explanation: ~A."),
	      @':format-arguments',
	      mk_cl_list(env, 3,
			 mkcl_make_simple_base_string(env, (char *) err_msg),
			 mk_cl_Cnil,
			 win_msg_obj)
	      );
#else
  mkcl_FElibc_stream_error(env, strm, err_msg, 0);
#endif
}


#ifdef MKCL_WINDOWS

extern BOOL mkcl_saw_console_ctrl_event(void);

static BOOL is_a_console(MKCL, HANDLE hnd)
{
  DWORD cm;
  BOOL good;

  MKCL_LIBC_NO_INTR(env, good = GetConsoleMode(hnd, &cm));
  if (!good)
    {
      DWORD err_val;

      MKCL_LIBC_NO_INTR(env, err_val = GetLastError());
      if (ERROR_INVALID_HANDLE != err_val)
	mkcl_FEwin32_error(MKCL_ENV(), "GetConsoleMode() failed", 0);
      else
	return FALSE;
    }
  else
    return TRUE;
}

static BOOL should_try_to_read_again(MKCL, int fd)
{
  DWORD err_val;
  BOOL retry = FALSE;

  MKCL_LIBC_NO_INTR(env, err_val = GetLastError());
  switch (err_val)
    {
    case ERROR_OPERATION_ABORTED:
      SetLastError(ERROR_SUCCESS); /* Clear error. */
      retry = TRUE;
      break;
    case ERROR_SUCCESS: break;
    default: break;
    }
  fflush(NULL);

  HANDLE hnd = (HANDLE)_get_osfhandle(fd);

  if (is_a_console(env, hnd))
    {
      BOOL ctrl_event;

      SwitchToThread();
      ctrl_event = mkcl_saw_console_ctrl_event();

      if (retry || ctrl_event)
	Sleep(100); /* Is 0.1 seconds enough? JCB */
      return retry;
    }
  return FALSE;
}

static const struct code_page_ident
{
  UINT id;
  char * name;
} code_page_ident[] = {
  { 437, "DOS-CP437" },
  { 737, "DOS-CP737" },
  { 775, "DOS-CP775" },
  { 850, "DOS-CP850" },
  { 852, "DOS-CP852" },
  { 855, "DOS-CP855" },
  { 857, "DOS-CP857" },
  { 860, "DOS-CP860" },
  { 861, "DOS-CP861" },
  { 862, "DOS-CP862" },
  { 863, "DOS-CP863" },
  { 864, "DOS-CP864" },
  { 865, "DOS-CP865" },
  { 866, "DOS-CP866" },
  { 869, "DOS-CP869" },
  { 874, "DOS-CP874" },
  { 932, "WINDOWS-CP932" },
  { 936, "WINDOWS-CP936" },
  { 949, "WINDOWS-CP949" },
  { 950, "WINDOWS-CP950" },
  { 1250, "WINDOWS-CP1250" },
  { 1251, "WINDOWS-CP1251" },
  { 1252, "WINDOWS-CP1252" },
  { 1253, "WINDOWS-CP1253" },
  { 1254, "WINDOWS-CP1254" },
  { 1255, "WINDOWS-CP1255" },
  { 1256, "WINDOWS-CP1256" },
  { 1257, "WINDOWS-CP1257" },
  { 1258, "WINDOWS-CP1258" },
  { 20127, "US-ASCII" },
  { 20866, "KOI8-R" },
  { 21866, "KOI8-U" },
  { 28591, "ISO-8859-1" },
  { 28592, "ISO-8859-2" },
  { 28593, "ISO-8859-3" },
  { 28594, "ISO-8859-4" },
  { 28595, "ISO-8859-5" },
  { 28596, "ISO-8859-6" },
  { 28597, "ISO-8859-7" },
  { 28598, "ISO-8859-8" },
  { 28599, "ISO-8859-9" },
  { 28603, "ISO-8859-13" },
  { 28605, "ISO-8859-15" },
  { 65001, "UTF-8" }
};

mkcl_object
mkcl_external_format_from_codepage(MKCL, UINT codepage)
{
  char * codepage_name = NULL;
  int i;
  

  for (i = 0; i < MKCL_NB_ELEMS(code_page_ident); i++)
    if (code_page_ident[i].id == codepage)
      { codepage_name = code_page_ident[i].name; break; }

  if (codepage_name)
    {
      size_t codepage_name_len = strlen(codepage_name);
      mkcl_base_string_object_sized(codepage_name_obj, codepage_name, codepage_name_len);
      int intern_flag;
      mkcl_object external_format = mkcl_intern(env, (mkcl_object) &codepage_name_obj,
						mkcl_core.keyword_package, &intern_flag);

      return external_format;
    }
  else
    return @':ISO-8859-1';
}

#endif /* MKCL_WINDOWS */

void
mkcl_init_file(MKCL)
{
  mkcl_object standard_input;
  mkcl_object standard_output;
  mkcl_object error_output;
  mkcl_object null_stream;
#ifdef MKCL_WINDOWS
  /* We presume the 3 standard streams were opened in TEXT mode. */
  mkcl_object external_format = mk_cl_list(env, 2, @':UTF-8', @':LF'); /* temporary default to be adjusted in late init */
  /* WSAStartUp() to be called here? */
  WSADATA wsadata;

  if (WSAStartup(MAKEWORD(2,2), &wsadata) != NO_ERROR) /* We demand WinSock 2.2 */
    mkcl_FEerror(env, "Unable to initialize Windows Socket library", 0);
  /* Microsoft's documentation says that we should have a matching call to WSACleanup(), but when? JCB */
#else
  mkcl_object external_format = @':default';
#endif

  null_stream = make_stream_from_FILE(env, 
				      mkcl_make_simple_base_string(env, "/dev/null"),
				      NULL, mkcl_smm_io, 8, mk_cl_Cnil); /* Binary stream? JCB */
  generic_close(env, null_stream);
  null_stream = mk_cl_make_two_way_stream(env, null_stream, mk_cl_make_broadcast_stream(env, 0));
  mkcl_core.null_stream = null_stream;

  standard_input = make_file_stream_from_fd(env, 
					    mkcl_make_simple_base_string(env, "stdin"),
					    STDIN_FILENO, mkcl_smm_input_file,
					    0, external_format);
  standard_output = make_file_stream_from_fd(env, 
					     mkcl_make_simple_base_string(env, "stdout"),
					     STDOUT_FILENO, mkcl_smm_output_file,
					     0, external_format);
  error_output = make_file_stream_from_fd(env, 
					  mkcl_make_simple_base_string(env, "stderr"),
					  STDERR_FILENO, mkcl_smm_output_file,
					  0, external_format);
  
  mkcl_core.standard_input = standard_input;
  MKCL_SET(@'*standard-input*', standard_input);
  mkcl_core.standard_output = standard_output;
  MKCL_SET(@'*standard-output*', standard_output);
  MKCL_SET(@'*trace-output*', standard_output);
  mkcl_core.error_output = error_output;
  MKCL_SET(@'*error-output*', error_output);

  {
    mkcl_object aux = mk_cl_make_two_way_stream(env, standard_input, standard_output);

    mkcl_core.terminal_io = aux;
    MKCL_SET(@'*terminal-io*', aux);

    aux = mk_cl_make_synonym_stream(env, @'*terminal-io*');
    MKCL_SET(@'*query-io*', aux);
    MKCL_SET(@'*debug-io*', aux);
  }

  mkcl_def_c_function(env, @'si::stream-encoding-error', /*(mkcl_objectfn_fixed)*/ stream_encoding_error_boot_stub, 3);
  mkcl_def_c_function(env, @'si::stream-decoding-error', /*(mkcl_objectfn_fixed)*/ stream_decoding_error_boot_stub, 3);
}

void
mkcl_init_late_file(MKCL)
{
#ifdef MKCL_WINDOWS
  if (mk_cl_fboundp(env, @'si::make-encoding'))
    {
      mkcl_object external_format = mkcl_external_format_from_codepage(env, GetACP());
      mkcl_object stdin_external_format = mkcl_external_format_from_codepage(env, GetConsoleCP());
      mkcl_object stdout_external_format = mkcl_external_format_from_codepage(env, GetConsoleOutputCP());

      mkcl_core.default_default_external_format = external_format;
      MKCL_SET(@'si::*default-external-format*', external_format);
      MKCL_SETQ(env, @'si::*default-external-format*', external_format);

      mk_si_stream_external_format_set(env, mkcl_core.standard_input, stdin_external_format);
      mk_si_stream_external_format_set(env, mkcl_core.standard_output, stdout_external_format);
      mk_si_stream_external_format_set(env, mkcl_core.error_output, stdout_external_format);
    }
#endif
}

