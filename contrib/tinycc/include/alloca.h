#ifndef	_ALLOCA_H
#define	_ALLOCA_H

#include <stddef.h>

#define alloca(size) __mkcc_alloca(size)
void * __mkcc_alloca(size_t size);

#endif /* alloca.h */


