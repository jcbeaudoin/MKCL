
#include <mkcl/mkcl.h>
#include <mkcl/internal.h>


#include "packages/CLOS_package.h"


mkcl_object mkcl_setup_package_clos(void)
{
  return (mkcl_object) &mkcl_package_clos;
}

