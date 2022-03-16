
#include <mkcl/mkcl.h>
#include <mkcl/internal.h>


#include "MKCL_package.h"


mkcl_object mkcl_setup_mkcl(void)
{
  return (mkcl_object) &mkcl_package_mkcl;
}

