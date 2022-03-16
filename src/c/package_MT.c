
#include <mkcl/mkcl.h>
#include <mkcl/internal.h>


#include "packages/MT_package.h"


mkcl_object mkcl_setup_package_mt(void)
{
  return (mkcl_object) &mkcl_package_mt;
}

