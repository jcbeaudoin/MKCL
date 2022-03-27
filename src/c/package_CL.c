
#include <mkcl/mkcl.h>
#include <mkcl/internal.h>


#include "packages/CL_package.h"


mkcl_object mkcl_setup_package_cl(void)
{
#if MKCL_WINDOWS
  InitializeCriticalSection(&(mkcl_package_cl.lock));
#endif
  return (mkcl_object) &mkcl_package_cl;
}

