
#include <mkcl/mkcl.h>
#include <mkcl/internal.h>


#include "FFI_package.h"


mkcl_object mkcl_setup_ffi(void)
{
  return (mkcl_object) &mkcl_package_ffi;
}

