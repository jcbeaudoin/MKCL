/*
 * Copyright (c) 1996-1997
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 * Copyright (c) 2002
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */

/*
 * This implements standard-conforming allocators that interact with
 * the garbage collector.  Gc_allocator<T> allocates garbage-collectible
 * objects of type T.  Traceable_allocator<T> allocates objects that
 * are not themselves garbage collected, but are scanned by the
 * collector for pointers to collectible objects.  Traceable_alloc
 * should be used for explicitly managed STL containers that may
 * point to collectible objects.
 *
 * This code was derived from an earlier version of the GNU C++ standard
 * library, which itself was derived from the SGI STL implementation.
 *
 * Ignore-off-page allocator: George T. Talbot
 */

#ifndef MK_GC_ALLOCATOR_H

#define MK_GC_ALLOCATOR_H

#include "gc.h"
#include <new> // for placement new

#if defined(__GNUC__)
#  define MK_GC_ATTR_UNUSED __attribute__((__unused__))
#else
#  define MK_GC_ATTR_UNUSED
#endif

/* First some helpers to allow us to dispatch on whether or not a type
 * is known to be pointer-free.
 * These are private, except that the client may invoke the
 * MK_GC_DECLARE_PTRFREE macro.
 */

struct MK_GC_true_type {};
struct MK_GC_false_type {};

template <class MK_GC_tp>
struct MK_GC_type_traits {
  MK_GC_false_type MK_GC_is_ptr_free;
};

# define MK_GC_DECLARE_PTRFREE(T) \
template<> struct MK_GC_type_traits<T> { MK_GC_true_type MK_GC_is_ptr_free; }

MK_GC_DECLARE_PTRFREE(char);
MK_GC_DECLARE_PTRFREE(signed char);
MK_GC_DECLARE_PTRFREE(unsigned char);
MK_GC_DECLARE_PTRFREE(signed short);
MK_GC_DECLARE_PTRFREE(unsigned short);
MK_GC_DECLARE_PTRFREE(signed int);
MK_GC_DECLARE_PTRFREE(unsigned int);
MK_GC_DECLARE_PTRFREE(signed long);
MK_GC_DECLARE_PTRFREE(unsigned long);
MK_GC_DECLARE_PTRFREE(float);
MK_GC_DECLARE_PTRFREE(double);
MK_GC_DECLARE_PTRFREE(long double);
/* The client may want to add others.   */

// In the following MK_GC_Tp is MK_GC_true_type if we are allocating a
// pointer-free object.
template <class MK_GC_Tp>
inline void * MK_GC_selective_alloc(size_t n, MK_GC_Tp, bool ignore_off_page) {
    return ignore_off_page?MK_GC_MALLOC_IGNORE_OFF_PAGE(n):MK_GC_MALLOC(n);
}

template <>
inline void * MK_GC_selective_alloc<MK_GC_true_type>(size_t n, MK_GC_true_type,
                                               bool ignore_off_page) {
    return ignore_off_page? MK_GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE(n)
                          : MK_GC_MALLOC_ATOMIC(n);
}

/* Now the public gc_allocator<T> class:
 */
template <class MK_GC_Tp>
class gc_allocator {
public:
  typedef size_t     size_type;
  typedef ptrdiff_t  difference_type;
  typedef MK_GC_Tp*       pointer;
  typedef const MK_GC_Tp* const_pointer;
  typedef MK_GC_Tp&       reference;
  typedef const MK_GC_Tp& const_reference;
  typedef MK_GC_Tp        value_type;

  template <class MK_GC_Tp1> struct rebind {
    typedef gc_allocator<MK_GC_Tp1> other;
  };

  gc_allocator()  {}
    gc_allocator(const gc_allocator&) throw() {}
# if !(MK_GC_NO_MEMBER_TEMPLATES || 0 < _MSC_VER && _MSC_VER <= 1200)
  // MSVC++ 6.0 do not support member templates
  template <class MK_GC_Tp1> gc_allocator(const gc_allocator<MK_GC_Tp1>&) throw() {}
# endif
  ~gc_allocator() throw() {}

  pointer address(reference MK_GC_x) const { return &MK_GC_x; }
  const_pointer address(const_reference MK_GC_x) const { return &MK_GC_x; }

  // MK_GC_n is permitted to be 0.  The C++ standard says nothing about what
  // the return value is when MK_GC_n == 0.
  MK_GC_Tp* allocate(size_type MK_GC_n, const void* = 0) {
    MK_GC_type_traits<MK_GC_Tp> traits;
    return static_cast<MK_GC_Tp *>
            (MK_GC_selective_alloc(MK_GC_n * sizeof(MK_GC_Tp),
                                traits.MK_GC_is_ptr_free, false));
  }

  // __p is not permitted to be a null pointer.
  void deallocate(pointer __p, size_type MK_GC_ATTR_UNUSED MK_GC_n)
    { MK_GC_FREE(__p); }

  size_type max_size() const throw()
    { return size_t(-1) / sizeof(MK_GC_Tp); }

  void construct(pointer __p, const MK_GC_Tp& __val) { new(__p) MK_GC_Tp(__val); }
  void destroy(pointer __p) { __p->~MK_GC_Tp(); }
};

template<>
class gc_allocator<void> {
  typedef size_t      size_type;
  typedef ptrdiff_t   difference_type;
  typedef void*       pointer;
  typedef const void* const_pointer;
  typedef void        value_type;

  template <class MK_GC_Tp1> struct rebind {
    typedef gc_allocator<MK_GC_Tp1> other;
  };
};


template <class MK_GC_T1, class MK_GC_T2>
inline bool operator==(const gc_allocator<MK_GC_T1>&, const gc_allocator<MK_GC_T2>&)
{
  return true;
}

template <class MK_GC_T1, class MK_GC_T2>
inline bool operator!=(const gc_allocator<MK_GC_T1>&, const gc_allocator<MK_GC_T2>&)
{
  return false;
}


/* Now the public gc_allocator_ignore_off_page<T> class:
 */
template <class MK_GC_Tp>
class gc_allocator_ignore_off_page {
public:
  typedef size_t     size_type;
  typedef ptrdiff_t  difference_type;
  typedef MK_GC_Tp*       pointer;
  typedef const MK_GC_Tp* const_pointer;
  typedef MK_GC_Tp&       reference;
  typedef const MK_GC_Tp& const_reference;
  typedef MK_GC_Tp        value_type;

  template <class MK_GC_Tp1> struct rebind {
    typedef gc_allocator_ignore_off_page<MK_GC_Tp1> other;
  };

  gc_allocator_ignore_off_page()  {}
    gc_allocator_ignore_off_page(const gc_allocator_ignore_off_page&) throw() {}
# if !(MK_GC_NO_MEMBER_TEMPLATES || 0 < _MSC_VER && _MSC_VER <= 1200)
  // MSVC++ 6.0 do not support member templates
  template <class MK_GC_Tp1>
    gc_allocator_ignore_off_page(const gc_allocator_ignore_off_page<MK_GC_Tp1>&)
        throw() {}
# endif
  ~gc_allocator_ignore_off_page() throw() {}

  pointer address(reference MK_GC_x) const { return &MK_GC_x; }
  const_pointer address(const_reference MK_GC_x) const { return &MK_GC_x; }

  // MK_GC_n is permitted to be 0.  The C++ standard says nothing about what
  // the return value is when MK_GC_n == 0.
  MK_GC_Tp* allocate(size_type MK_GC_n, const void* = 0) {
    MK_GC_type_traits<MK_GC_Tp> traits;
    return static_cast<MK_GC_Tp *>
            (MK_GC_selective_alloc(MK_GC_n * sizeof(MK_GC_Tp),
                                traits.MK_GC_is_ptr_free, true));
  }

  // __p is not permitted to be a null pointer.
  void deallocate(pointer __p, size_type MK_GC_ATTR_UNUSED MK_GC_n)
    { MK_GC_FREE(__p); }

  size_type max_size() const throw()
    { return size_t(-1) / sizeof(MK_GC_Tp); }

  void construct(pointer __p, const MK_GC_Tp& __val) { new(__p) MK_GC_Tp(__val); }
  void destroy(pointer __p) { __p->~MK_GC_Tp(); }
};

template<>
class gc_allocator_ignore_off_page<void> {
  typedef size_t      size_type;
  typedef ptrdiff_t   difference_type;
  typedef void*       pointer;
  typedef const void* const_pointer;
  typedef void        value_type;

  template <class MK_GC_Tp1> struct rebind {
    typedef gc_allocator_ignore_off_page<MK_GC_Tp1> other;
  };
};

template <class MK_GC_T1, class MK_GC_T2>
inline bool operator==(const gc_allocator_ignore_off_page<MK_GC_T1>&, const gc_allocator_ignore_off_page<MK_GC_T2>&)
{
  return true;
}

template <class MK_GC_T1, class MK_GC_T2>
inline bool operator!=(const gc_allocator_ignore_off_page<MK_GC_T1>&, const gc_allocator_ignore_off_page<MK_GC_T2>&)
{
  return false;
}

/*
 * And the public traceable_allocator class.
 */

// Note that we currently don't specialize the pointer-free case, since a
// pointer-free traceable container doesn't make that much sense,
// though it could become an issue due to abstraction boundaries.
template <class MK_GC_Tp>
class traceable_allocator {
public:
  typedef size_t     size_type;
  typedef ptrdiff_t  difference_type;
  typedef MK_GC_Tp*       pointer;
  typedef const MK_GC_Tp* const_pointer;
  typedef MK_GC_Tp&       reference;
  typedef const MK_GC_Tp& const_reference;
  typedef MK_GC_Tp        value_type;

  template <class MK_GC_Tp1> struct rebind {
    typedef traceable_allocator<MK_GC_Tp1> other;
  };

  traceable_allocator() throw() {}
    traceable_allocator(const traceable_allocator&) throw() {}
# if !(MK_GC_NO_MEMBER_TEMPLATES || 0 < _MSC_VER && _MSC_VER <= 1200)
  // MSVC++ 6.0 do not support member templates
  template <class MK_GC_Tp1> traceable_allocator
          (const traceable_allocator<MK_GC_Tp1>&) throw() {}
# endif
  ~traceable_allocator() throw() {}

  pointer address(reference MK_GC_x) const { return &MK_GC_x; }
  const_pointer address(const_reference MK_GC_x) const { return &MK_GC_x; }

  // MK_GC_n is permitted to be 0.  The C++ standard says nothing about what
  // the return value is when MK_GC_n == 0.
  MK_GC_Tp* allocate(size_type MK_GC_n, const void* = 0) {
    return static_cast<MK_GC_Tp*>(MK_GC_MALLOC_UNCOLLECTABLE(MK_GC_n * sizeof(MK_GC_Tp)));
  }

  // __p is not permitted to be a null pointer.
  void deallocate(pointer __p, size_type MK_GC_ATTR_UNUSED MK_GC_n)
    { MK_GC_FREE(__p); }

  size_type max_size() const throw()
    { return size_t(-1) / sizeof(MK_GC_Tp); }

  void construct(pointer __p, const MK_GC_Tp& __val) { new(__p) MK_GC_Tp(__val); }
  void destroy(pointer __p) { __p->~MK_GC_Tp(); }
};

template<>
class traceable_allocator<void> {
  typedef size_t      size_type;
  typedef ptrdiff_t   difference_type;
  typedef void*       pointer;
  typedef const void* const_pointer;
  typedef void        value_type;

  template <class MK_GC_Tp1> struct rebind {
    typedef traceable_allocator<MK_GC_Tp1> other;
  };
};


template <class MK_GC_T1, class MK_GC_T2>
inline bool operator==(const traceable_allocator<MK_GC_T1>&, const traceable_allocator<MK_GC_T2>&)
{
  return true;
}

template <class MK_GC_T1, class MK_GC_T2>
inline bool operator!=(const traceable_allocator<MK_GC_T1>&, const traceable_allocator<MK_GC_T2>&)
{
  return false;
}

#endif /* MK_GC_ALLOCATOR_H */
