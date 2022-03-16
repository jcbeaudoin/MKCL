
#include <mkcl/mkcl.h>
#include <mkcl/internal.h>


#include "packages/CL_package.h"


mkcl_object mkcl_setup_package_cl(void)
{
  return (mkcl_object) &mkcl_package_cl;
}

