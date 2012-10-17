#ifndef MK_GC_AMIGA_REDIRECTS_H

# define MK_GC_AMIGA_REDIRECTS_H

# if ( defined(_AMIGA) && !defined(MK_GC_AMIGA_MAKINGLIB) )
    extern void *MK_GC_amiga_realloc(void *old_object,size_t new_size_in_bytes);
#   define MK_GC_realloc(a,b) MK_GC_amiga_realloc(a,b)
    extern void MK_GC_amiga_set_toany(void (*func)(void));
    extern int MK_GC_amiga_free_space_divisor_inc;
    extern void *(*MK_GC_amiga_allocwrapper_do) \
	(size_t size,void *(*AllocFunction)(size_t size2));
#   define MK_GC_malloc(a) \
	(*MK_GC_amiga_allocwrapper_do)(a,MK_GC_malloc)
#   define MK_GC_malloc_atomic(a) \
	(*MK_GC_amiga_allocwrapper_do)(a,MK_GC_malloc_atomic)
#   define MK_GC_malloc_uncollectable(a) \
	(*MK_GC_amiga_allocwrapper_do)(a,MK_GC_malloc_uncollectable)
#   define MK_GC_malloc_stubborn(a) \
	(*MK_GC_amiga_allocwrapper_do)(a,MK_GC_malloc_stubborn)
#   define MK_GC_malloc_atomic_uncollectable(a) \
	(*MK_GC_amiga_allocwrapper_do)(a,MK_GC_malloc_atomic_uncollectable)
#   define MK_GC_malloc_ignore_off_page(a) \
	(*MK_GC_amiga_allocwrapper_do)(a,MK_GC_malloc_ignore_off_page)
#   define MK_GC_malloc_atomic_ignore_off_page(a) \
	(*MK_GC_amiga_allocwrapper_do)(a,MK_GC_malloc_atomic_ignore_off_page)
# endif /* _AMIGA && !MK_GC_AMIGA_MAKINGLIB */

#endif /* MK_GC_AMIGA_REDIRECTS_H */


