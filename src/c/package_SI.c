
#include <mkcl/mkcl.h>
#include <mkcl/internal.h>


#include "packages/SI_package.h"


mkcl_object mkcl_setup_package_si(void)
{
#if MKCL_WINDOWS
  InitializeCriticalSection(&(mkcl_package_si.lock));
#endif
  return (mkcl_object) &mkcl_package_si;
}

