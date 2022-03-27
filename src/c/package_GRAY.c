
#include <mkcl/mkcl.h>
#include <mkcl/internal.h>


#include "packages/GRAY_package.h"


mkcl_object mkcl_setup_package_gray(void)
{
#if MKCL_WINDOWS
  InitializeCriticalSection(&(mkcl_package_gray.lock));
#endif
  return (mkcl_object) &mkcl_package_gray;
}

