#
#  Makefile for MKCL core library package generators.
#
#  Copyright (c) 2022, Jean-Claude Beaudoin.
#
#  MKCL is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Lesser General Public
#  License as published by the Free Software Foundation; either
#  version 3 of the License, or (at your option) any later version.
#
#  See file '../../../Copyright' for full details.
#

CFLAGS= -I../ -D_GNU_SOURCE -DMKCL_PACKAGE_BUILDER -fno-strict-aliasing -fPIC -O2

all: build_package_FFI expose_package_FFI build_package_MT expose_package_MT build_package_GRAY expose_package_GRAY build_package_MKCL expose_package_MKCL build_package_CLOS expose_package_CLOS build_package_KEYWORD expose_package_KEYWORD build_package_SI expose_package_SI build_package_CL expose_package_CL

LIB_OBJS = 


build_package_FFI: build_package_FFI.o hash.o 
	$(CC) $(LDFLAGS) -o $@ build_package_FFI.o hash.o

expose_package_FFI: build_package_FFI.o hash.o
	$(CC) $(LDFLAGS) -o $@ build_package_FFI.o hash.o

build_package_MT: build_package_MT.o hash.o
	$(CC) $(LDFLAGS) -o $@ build_package_MT.o hash.o

expose_package_MT: build_package_MT.o hash.o
	$(CC) $(LDFLAGS) -o $@ build_package_MT.o hash.o

build_package_GRAY: build_package_GRAY.o hash.o
	$(CC) $(LDFLAGS) -o $@ build_package_GRAY.o hash.o

expose_package_GRAY: build_package_GRAY.o hash.o
	$(CC) $(LDFLAGS) -o $@ build_package_GRAY.o hash.o

build_package_MKCL: build_package_MKCL.o hash.o
	$(CC) $(LDFLAGS) -o $@ build_package_MKCL.o hash.o

expose_package_MKCL: build_package_MKCL.o hash.o
	$(CC) $(LDFLAGS) -o $@ build_package_MKCL.o hash.o

build_package_CLOS: build_package_CLOS.o hash.o
	$(CC) $(LDFLAGS) -o $@ build_package_CLOS.o hash.o

expose_package_CLOS: build_package_CLOS.o hash.o
	$(CC) $(LDFLAGS) -o $@ build_package_CLOS.o hash.o

build_package_KEYWORD: build_package_KEYWORD.o hash.o
	$(CC) $(LDFLAGS) -o $@ build_package_KEYWORD.o hash.o

expose_package_KEYWORD: build_package_KEYWORD.o hash.o
	$(CC) $(LDFLAGS) -o $@ build_package_KEYWORD.o hash.o

build_package_SI: build_package_SI.o hash.o
	$(CC) $(LDFLAGS) -o $@ build_package_SI.o hash.o

expose_package_SI: build_package_SI.o hash.o
	$(CC) $(LDFLAGS) -o $@ build_package_SI.o hash.o

build_package_CL: build_package_CL.o hash.o
	$(CC) $(LDFLAGS) -o $@ build_package_CL.o hash.o

expose_package_CL: build_package_CL.o hash.o
	$(CC) $(LDFLAGS) -o $@ build_package_CL.o hash.o

test_FFI.o: test_FFI.c FFI_package.h FFI_package_core.h
	$(CC) -I../ -D_GNU_SOURCE -fno-strict-aliasing -fPIC -O0 -g -c test_FFI.c -o test_FFI.o

test_MT.o: test_MT.c MT_package.h MT_package_core.h
	$(CC) -I../ -D_GNU_SOURCE -fno-strict-aliasing -fPIC -O0 -g -c test_MT.c -o test_MT.o

test_GRAY.o: test_GRAY.c GRAY_package.h GRAY_package_core.h
	$(CC) -I../ -D_GNU_SOURCE -fno-strict-aliasing -fPIC -O0 -g -c test_GRAY.c -o test_GRAY.o

test_MKCL.o: test_MKCL.c MKCL_package.h MKCL_package_core.h
	$(CC) -I../ -D_GNU_SOURCE -fno-strict-aliasing -fPIC -O0 -g -c test_MKCL.c -o test_MKCL.o

test_CLOS.o: test_CLOS.c CLOS_package.h CLOS_package_core.h
	$(CC) -I../ -D_GNU_SOURCE -fno-strict-aliasing -fPIC -O0 -g -c test_CLOS.c -o test_CLOS.o

test_KEYWORD.o: test_KEYWORD.c KEYWORD_package.h KEYWORD_package_core.h
	$(CC) -I../ -D_GNU_SOURCE -fno-strict-aliasing -fPIC -O0 -g -c test_KEYWORD.c -o test_KEYWORD.o

test_SI.o: test_SI.c SI_package.h SI_package_core.h
	$(CC) -I../ -D_GNU_SOURCE -fno-strict-aliasing -fPIC -O0 -g -c test_SI.c -o test_SI.o

test_CL.o: test_CL.c CL_package.h CL_package_core.h
	$(CC) -I../ -D_GNU_SOURCE -fno-strict-aliasing -fPIC -O0 -g -c test_CL.c -o test_CL.o

test_USER.o: test_USER.c USER_package.h
	$(CC) -I../ -D_GNU_SOURCE -fno-strict-aliasing -fPIC -O0 -g -c test_USER.c -o test_USER.o

test: test_FFI.o test_MT.o test_GRAY.o test_MKCL.o test_CLOS.o test_KEYWORD.o test_SI.o test_CL.o test_USER.o

clean:
	rm -f *.o $(LIB_OBJS) TAGS
	rm -f build_package_FFI expose_package_FFI
	rm -f build_package_MT expose_package_MT
	rm -f build_package_GRAY expose_package_GRAY
	rm -f build_package_MKCL expose_package_MKCL
	rm -f build_package_CLOS expose_package_CLOS
	rm -f build_package_KEYWORD expose_package_KEYWORD
	rm -f build_package_SI expose_package_SI
	rm -f build_package_CL expose_package_CL
	rm -f expose_symbols_in_FFI.h expose_symbols_in_MT.h expose_symbols_in_MKCL.h expose_symbols_in_CLOS.h expose_symbols_in_GRAY.h expose_symbols_in_KEYWORD.h expose_symbols_in_SI.h expose_symbols_in_CL.h
	rm -f FFI_package_core.h MT_package_core.h MKCL_package_core.h CLOS_package_core.h GRAY_package_core.h KEYWORD_package_core.h SI_package_core.h CL_package_core.h

distclean: clean

realclean: distclean
	rm -f *~



TAGS: hash.c build_package_FFI.c build_package_MT.c build_package_GRAY.c build_package_MKCL.c build_package_CLOS.c build_package_KEYWORD.c build_package_SI.c build_package_CL.c build_package.h FFI_package_base.h FFI_package_core_stubs.h FFI_package.h MT_package_base.h MT_package_core_stubs.h MT_package.h MKCL_package_base.h MKCL_package_core_stubs.h MKCL_package.h CLOS_package_base.h CLOS_package_core_stubs.h CLOS_package.h GRAY_package_base.h GRAY_package_core_stubs.h GRAY_package.h KEYWORD_package_base.h KEYWORD_package_core_stubs.h KEYWORD_package.h SI_package_base.h SI_package_core_stubs.h SI_package.h CL_package_base.h CL_package_core_stubs.h CL_package.h
	etags hash.c build_package_FFI.c build_package_MT.c build_package_GRAY.c build_package_MKCL.c build_package_CLOS.c build_package_KEYWORD.c build_package_SI.c build_package_CL.c build_package.h FFI_package_base.h FFI_package_core_stubs.h FFI_package.h MT_package_base.h MT_package_core_stubs.h MT_package.h MKCL_package_base.h MKCL_package_core_stubs.h MKCL_package.h CLOS_package_base.h CLOS_package_core_stubs.h CLOS_package.h GRAY_package_base.h GRAY_package_core_stubs.h GRAY_package.h KEYWORD_package_base.h KEYWORD_package_core_stubs.h KEYWORD_package.h SI_package_base.h SI_package_core_stubs.h SI_package.h CL_package_base.h CL_package_core_stubs.h CL_package.h


CL_package.h: CL_package_core_stubs.h CL_package_base.h

SI_package.h: SI_package_core_stubs.h SI_package_base.h

KEYWORD_package.h: KEYWORD_package_core_stubs.h KEYWORD_package_base.h

GRAY_package.h: GRAY_package_core_stubs.h GRAY_package_base.h

CLOS_package.h: CLOS_package_core_stubs.h CLOS_package_base.h

MKCL_package.h: MKCL_package_core_stubs.h MKCL_package_base.h

MT_package.h: MT_package_core_stubs.h MT_package_base.h

FFI_package.h: FFI_package_core_stubs.h FFI_package_base.h

build_package_FFI.o: build_package.h FFI_package.h FFI_package_core_stubs.h FFI_package_base.h

build_package_MT.o: build_package.h MT_package.h MT_package_core_stubs.h MT_package_base.h

build_package_MKCL.o: build_package.h MKCL_package.h MKCL_package_core_stubs.h MKCL_package_base.h

build_package_CLOS.o: build_package.h CLOS_package.h CLOS_package_core_stubs.h CLOS_package_base.h

build_package_GRAY.o: build_package.h GRAY_package.h GRAY_package_core_stubs.h GRAY_package_base.h

build_package_KEYWORD.o: build_package.h KEYWORD_package.h KEYWORD_package_core_stubs.h KEYWORD_package_base.h

build_package_SI.o: build_package.h SI_package.h SI_package_core_stubs.h SI_package_base.h

build_package_CL.o: build_package.h CL_package.h CL_package_core_stubs.h CL_package_base.h

hash.o: build_package.h ../newhash.h

CL_package_core.h: build_package_CL
	./build_package_CL >$@

SI_package_core.h: build_package_SI
	./build_package_SI >$@

KEYWORD_package_core.h: build_package_KEYWORD
	./build_package_KEYWORD >$@

GRAY_package_core.h: build_package_GRAY
	./build_package_GRAY >$@

CLOS_package_core.h: build_package_CLOS
	./build_package_CLOS >$@

MKCL_package_core.h: build_package_MKCL
	./build_package_MKCL >$@

MT_package_core.h: build_package_MT
	./build_package_MT >$@

FFI_package_core.h: build_package_FFI
	./build_package_FFI >$@


expose_symbols_in_FFI.h: expose_package_FFI
	./expose_package_FFI >expose_symbols_in_FFI.h

expose_symbols_in_MT.h: expose_package_MT
	./expose_package_MT >expose_symbols_in_MT.h

expose_symbols_in_MKCL.h: expose_package_MKCL
	./expose_package_MKCL >expose_symbols_in_MKCL.h

expose_symbols_in_CLOS.h: expose_package_CLOS
	./expose_package_CLOS >expose_symbols_in_CLOS.h

expose_symbols_in_GRAY.h: expose_package_GRAY
	./expose_package_GRAY >expose_symbols_in_GRAY.h

expose_symbols_in_KEYWORD.h: expose_package_KEYWORD
	./expose_package_KEYWORD >expose_symbols_in_KEYWORD.h

expose_symbols_in_SI.h: expose_package_SI
	./expose_package_SI >expose_symbols_in_SI.h

expose_symbols_in_CL.h: expose_package_CL
	./expose_package_CL >expose_symbols_in_CL.h

