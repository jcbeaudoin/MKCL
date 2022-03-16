/* -*- mode: c  -*- */
/*
    Copyright (c) 2022, Jean-Claude Beaudoin.

    This program is under GNU LGPL, you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../../Copyright' for full details.
*/

#include "build_package.h"

#include "../newhash.h"



mkcl_hash_value hash_base_string(const mkcl_base_char *s, const mkcl_index len, mkcl_hash_value h)
{ HASH_XXXX_STRING_BODY(s, len, h); }


void add_entry_to_hash(const mkcl_hash_value hashed_key, struct mkcl_hashtable * ht, struct mkcl_hashtable_entry * entry)
{
  const mkcl_index hsize = ht->size;
  struct mkcl_hashtable_entry ** bin_root = &(ht->data[hashed_key % hsize]);

  entry->next = *bin_root;
  *bin_root = entry;
}

static bool base_string_equal(const struct mkcl_base_string * const s1, const struct mkcl_base_string * const s2)
{
  const mkcl_index l1 = s1->fillp;
  const mkcl_index l2 = s2->fillp;

  if (l1 != l2) return FALSE;
  return (0 == memcmp(s1->self, s2->self, l1));
}

struct mkcl_symbol * find_symbol(const struct mkcl_base_string * const name, struct mkcl_package * const package, bool * internalp_ptr)
{
  mkcl_hash_value hashed_name = hash_base_string(name->self, name->fillp, 0);

  if ( internalp_ptr ) *internalp_ptr = TRUE;
  {
    struct mkcl_hashtable * const internals = (struct mkcl_hashtable *) package->internal;
    const mkcl_index hsize = internals->size;
    struct mkcl_hashtable_entry * e = hsize ? internals->data[hashed_name % hsize] : NULL;
    
    while (e != NULL)
      if ( base_string_equal(name, ((struct mkcl_base_string *) e->key)) )
	return ((struct mkcl_symbol *) e->value);
      else
	e = e->next;
  }

  if ( internalp_ptr ) *internalp_ptr = FALSE;
  {
    struct mkcl_hashtable * const externals = (struct mkcl_hashtable *) package->external;
    const mkcl_index hsize = externals->size;
    struct mkcl_hashtable_entry * e = hsize ? externals->data[hashed_name % hsize] : NULL;
    
    while (e != NULL)
      if ( base_string_equal(name, ((struct mkcl_base_string *) e->key)) )
	return ((struct mkcl_symbol *) e->value);
      else
	e = e->next;
  }
  return NULL;
}

