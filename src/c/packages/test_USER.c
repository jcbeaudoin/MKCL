
#include <mkcl/mkcl.h>
#include <mkcl/internal.h>


#include "USER_package.h"


mkcl_object mkcl_setup_user(void)
{
  return (mkcl_object) &mkcl_package_user;
}

