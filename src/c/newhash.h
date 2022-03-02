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

#endif

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

