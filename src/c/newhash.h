/* -*- mode: c; c-basic-offset: 8 -*- */
/********************
 * HASHING ROUTINES *
 ********************/

/*
 * SBCL's newest algorithm. Leads to few collisions, and it is faster.
 */

#if MKCL_WORD_BITS > 32
/*
 * 64 bit version
 */
#if MKCL_LONG_BITS >= 64
#define GOLDEN_RATIO 0x9e3779b97f4a7c13L
#else
#define GOLDEN_RATIO 0x9e3779b97f4a7c13LL
#endif
#define mix(a,b,c)				\
	{					\
		a=a-b;  a=a-c;  a=a^(c>>43);	\
		b=b-c;  b=b-a;  b=b^(a<<9);	\
		c=c-a;  c=c-b;  c=c^(b>>8);	\
		a=a-b;  a=a-c;  a=a^(c>>38);	\
		b=b-c;  b=b-a;  b=b^(a<<23);	\
		c=c-a;  c=c-b;  c=c^(b>>5);	\
		a=a-b;  a=a-c;  a=a^(c>>35);	\
		b=b-c;  b=b-a;  b=b^(a<<49);	\
		c=c-a;  c=c-b;  c=c^(b>>11);	\
		a=a-b;  a=a-c;  a=a^(c>>12);	\
		b=b-c;  b=b-a;  b=b^(a<<18);	\
		c=c-a;  c=c-b;  c=c^(b>>22);	\
	}

#define extract_word(k)							\
	(k[0]+((mkcl_index)k[1]<<8)+((mkcl_index)k[2]<<16)+((mkcl_index)k[3]<<24)+ \
	 ((mkcl_index)k[4]<<32)+((mkcl_index)k[5]<<40)+((mkcl_index)k[6]<<48)+ \
	 ((mkcl_index)k[7]<<52))

static mkcl_hash_value
hash_mem_region(mkcl_index initval, const unsigned char *k, mkcl_index length)
{
	register mkcl_index a = GOLDEN_RATIO, b = GOLDEN_RATIO, c = initval;
	register mkcl_index len;
	for (len = length; len >= 24; len -= 24) {
		a += extract_word(k); k+=8;
		b += extract_word(k); k+=8;
		c += extract_word(k); k+=8;
		mix(a,b,c);
	}

	/*------------------------------------- handle the last 11 bytes */
	c += length;
	switch(len) {
		/* all the case statements fall through */
	case 23: c+=((mkcl_index)k[22]<<52);
	case 22: c+=((mkcl_index)k[21]<<48);
	case 21: c+=((mkcl_index)k[20]<<40);
	case 20: c+=((mkcl_index)k[19]<<32);
	case 19: c+=((mkcl_index)k[18]<<24);
	case 18: c+=((mkcl_index)k[17]<<16);
	case 17: c+=((mkcl_index)k[16]<<8);
		/* the first byte of c is reserved for the length */
	case 16: b+=((mkcl_index)k[15]<<52);
	case 15: b+=((mkcl_index)k[14]<<48);
	case 14: b+=((mkcl_index)k[13]<<40);
	case 13: b+=((mkcl_index)k[12]<<32);
	case 12: b+=((mkcl_index)k[11]<<24);
	case 11: b+=((mkcl_index)k[10]<<16);
	case 10: b+=((mkcl_index)k[9]<<8);
	case 9 : b+=k[8];
	case 8 : a+=((mkcl_index)k[7]<<52);
	case 7 : a+=((mkcl_index)k[6]<<48);
	case 6 : a+=((mkcl_index)k[5]<<40);
	case 5 : a+=((mkcl_index)k[4]<<32);
	case 4 : a+=((mkcl_index)k[3]<<24);
	case 3 : a+=((mkcl_index)k[2]<<16);
	case 2 : a+=((mkcl_index)k[1]<<8);
	case 1 : a+=k[0];
		/* case 0: nothing left to add */
	}
	mix(a,b,c);
	/*-------------------------------------------- report the result */
	return c;
}

#else
/*
 * 32 bit version
 */

#define GOLDEN_RATIO 0x9e3779b9L
#define mix(a,b,c)				\
	{					\
		a -= b; a -= c; a ^= (c>>13);	\
		b -= c; b -= a; b ^= (a<<8);	\
		c -= a; c -= b; c ^= (b>>13);	\
		a -= b; a -= c; a ^= (c>>12);	\
		b -= c; b -= a; b ^= (a<<16);	\
		c -= a; c -= b; c ^= (b>>5);	\
		a -= b; a -= c; a ^= (c>>3);	\
		b -= c; b -= a; b ^= (a<<10);	\
		c -= a; c -= b; c ^= (b>>15);	\
	}
#define extract_word(k)							\
	(k[0]+((mkcl_index)k[1]<<8)+((mkcl_index)k[2]<<16)+((mkcl_index)k[3]<<24))

static mkcl_hash_value
hash_mem_region(mkcl_index initval, const unsigned char *k, mkcl_index length)
{
	register mkcl_index a = GOLDEN_RATIO, b = GOLDEN_RATIO, c = initval;
	register mkcl_index len;
	for (len = length; len >= 12; len -= 12) {
		a += extract_word(k); k += 4;
		b += extract_word(k); k += 4;
		c += extract_word(k); k += 4;
		mix(a,b,c);
	}

	/*------------------------------------- handle the last 11 bytes */
	c += length;
	switch(len) {
		/* all the case statements fall through */
	case 11: c+=((mkcl_index)k[10]<<24);
	case 10: c+=((mkcl_index)k[9]<<16);
	case 9 : c+=((mkcl_index)k[8]<<8);
		/* the first byte of c is reserved for the length */
	case 8 : b+=((mkcl_index)k[7]<<24);
	case 7 : b+=((mkcl_index)k[6]<<16);
	case 6 : b+=((mkcl_index)k[5]<<8);
	case 5 : b+=k[4];
	case 4 : a+=((mkcl_index)k[3]<<24);
	case 3 : a+=((mkcl_index)k[2]<<16);
	case 2 : a+=((mkcl_index)k[1]<<8);
	case 1 : a+=k[0];
		/* case 0: nothing left to add */
	}
	mix(a,b,c);
	/*-------------------------------------------- report the result */
	return c;
}
#endif

static mkcl_hash_value hash_word(mkcl_hash_value c, mkcl_index w)
{
	mkcl_index a = w + GOLDEN_RATIO, b = GOLDEN_RATIO;
	mix(a, b, c);
	return c;
}

#define HASH_XXXX_STRING_BODY(S, LEN, H) \
{\
	mkcl_index a = GOLDEN_RATIO, b = GOLDEN_RATIO, i;\
	mkcl_character ch;\
	for (i = LEN; i >= 3; i -= 3) {\
		ch = *S;\
		a += ch; S++;\
		ch = *S;\
		b += ch; S++;\
		ch = *S;\
		H += ch; S++;\
		mix(a, b, H);\
	}\
	switch (i) {\
		/* all the case statements fall through */\
	case 2: ch = *S; a += ch; S++;\
	case 1: ch = *S; b += ch;\
	case 0: H += LEN;\
	}\
	mix(a, b, H);\
	return H;\
}

static mkcl_hash_value hash_base_string(const mkcl_base_char *s, const mkcl_index len, mkcl_hash_value h)
{ HASH_XXXX_STRING_BODY(s, len, h); }

static mkcl_hash_value hash_full_string(const mkcl_character *s, const mkcl_index len, mkcl_hash_value h)
{ HASH_XXXX_STRING_BODY(s, len, h); }

