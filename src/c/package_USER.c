
#include <mkcl/mkcl.h>
#include <mkcl/internal.h>


#include "packages/USER_package.h"


mkcl_object mkcl_setup_package_user(void)
{
#if MKCL_WINDOWS
  InitializeCriticalSection(&(mkcl_package_user.lock));
#endif
  return (mkcl_object) &mkcl_package_user;
}

