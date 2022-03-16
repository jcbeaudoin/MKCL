
#include <mkcl/mkcl.h>
#include <mkcl/internal.h>


#include "packages/FFI_package.h"


mkcl_object mkcl_setup_package_ffi(void)
{
  return (mkcl_object) &mkcl_package_ffi;
}

