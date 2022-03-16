/* -*- mode: c  -*- */
/*
    Copyright (c) 2022, Jean-Claude Beaudoin.

    This program is under GNU LGPL, you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../../Copyright' for full details.
*/


#include "KEYWORD_package_base.h"


#ifdef MKCL_PACKAGE_BUILDER
# include "KEYWORD_package_core_stubs.h"
#else
# include "KEYWORD_package_core.h"
#endif
