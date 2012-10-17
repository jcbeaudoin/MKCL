# list of entry points to Boehm's GC allocator
break GC_malloc
break GC_malloc_ignore_off_page
break GC_malloc_uncollectable
#break GC_malloc_stubborn
break GC_malloc_atomic
break GC_malloc_atomic_ignore_off_page
break GC_malloc_atomic_uncollectable
