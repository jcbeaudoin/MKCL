
#include <mkcl/mkcl.h>
#include <mkcl/internal.h>


#include "packages/CLOS_package.h"


mkcl_object mkcl_setup_package_clos(void)
{
#if MKCL_WINDOWS
  InitializeCriticalSection(&(mkcl_package_clos.lock));
#endif
  return (mkcl_object) &mkcl_package_clos;
}

