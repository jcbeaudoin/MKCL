
#include <mkcl/mkcl.h>
#include <mkcl/internal.h>


#include "MT_package.h"


mkcl_object mkcl_setup_mt(void)
{
  return (mkcl_object) &mkcl_package_mt;
}

