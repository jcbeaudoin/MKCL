
#include <mkcl/mkcl.h>
#include <mkcl/internal.h>


#include "packages/MKCL_package.h"


mkcl_object mkcl_setup_package_mkcl(void)
{
  return (mkcl_object) &mkcl_package_mkcl;
}

