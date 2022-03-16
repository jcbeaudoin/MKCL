/* -*- mode: c  -*- */
/*
    Copyright (c) 2022, Jean-Claude Beaudoin.

    This program is under GNU LGPL, you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../../Copyright' for full details.
*/

struct mkcl_symbol mkcl_mkcl_external_symbols[external_count];


struct mkcl_symbol mkcl_mkcl_internal_symbols[internal_count];

static struct mkcl_hashtable_entry internal_entries[internal_count] = {
  /* { NULL, mk_cl_Cnil, 0, mk_cl_Cnil } */
};

static struct mkcl_hashtable_entry external_entries[external_count] = {
  { NULL, mk_cl_Cnil, 0, mk_cl_Cnil }
};

static struct mkcl_hashtable_entry * internal_vector[internal_size] = { NULL };
static struct mkcl_hashtable_entry * external_vector[external_size] = { NULL };
