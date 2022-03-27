
#include <mkcl/mkcl.h>
#include <mkcl/internal.h>


#include "packages/KEYWORD_package.h"


mkcl_object mkcl_setup_package_keyword(void)
{
#if MKCL_WINDOWS
  InitializeCriticalSection(&(mkcl_package_keyword.lock));
#endif
  return (mkcl_object) &mkcl_package_keyword;
}

