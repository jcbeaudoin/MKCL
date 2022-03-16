
#include <mkcl/mkcl.h>
#include <mkcl/internal.h>


#include "packages/KEYWORD_package.h"


mkcl_object mkcl_setup_package_keyword(void)
{
  return (mkcl_object) &mkcl_package_keyword;
}

