/*
 * NEC LE-IT: For 64-bit OS we extend the double type to hold two int64's
 *
 *  x86-64: __m128 serves as placeholder which also requires the compiler
 *          to align it on 16 byte boundary (as required by cmpxchg16).
 * Similar things could be done for PowerPC 64-bit using a VMX data type...
 */

#if (defined(__x86_64__) && __GNUC__ >= 4) || defined(_WIN64)
# include <xmmintrin.h>
  typedef __m128 double_ptr_storage;
#elif defined(_WIN32) && !defined(__GNUC__)
  typedef unsigned __int64 double_ptr_storage;
#else
  typedef unsigned long long double_ptr_storage;
#endif

# define MK_AO_HAVE_DOUBLE_PTR_STORAGE

typedef union {
    double_ptr_storage MK_AO_whole;
    struct {MK_AO_t MK_AO_v1; MK_AO_t MK_AO_v2;} MK_AO_parts;
} MK_AO_double_t;

#define MK_AO_HAVE_double_t
#define MK_AO_val1 MK_AO_parts.MK_AO_v1
#define MK_AO_val2 MK_AO_parts.MK_AO_v2
