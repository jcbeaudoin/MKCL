#
# Tiny C Compiler Makefile - tests
#

TOP = ..
include $(TOP)/Makefile
SRCDIR = $(top_srcdir)/tests
VPATH = $(SRCDIR) $(top_srcdir)

# clear CFLAGS and LDFLAGS
CFLAGS :=
LDFLAGS :=

# what tests to run
TESTS = \
 hello-exe \
 hello-run \
 libtest \
 test3 \
 abitest \
 vla_test-run \
 tests2-dir pp-dir

BTESTS = test1b test3b btest

ifdef CONFIG_CROSS
 TESTS += hello-cross
endif

# test4 -- problem with -static
# asmtest -- minor differences with gcc
# btest -- works on i386 (including win32)

# bounds-checking is supported only on i386
ifneq ($(ARCH),i386)
 TESTS := $(filter-out $(BTESTS),$(TESTS))
endif
ifdef CONFIG_WIN32
 TESTS := $(filter-out $(BTESTS),$(TESTS))
endif
ifeq ($(TARGETOS),Darwin)
 TESTS := $(filter-out hello-exe test3 $(BTESTS),$(TESTS))
endif
ifeq (,$(filter arm64 i386 x86-64,$(ARCH)))
 TESTS := $(filter-out vla_test-run,$(TESTS))
endif
ifeq ($(CONFIG_arm_eabi),yes)
 TESTS := $(filter-out test3,$(TESTS))
endif

ifdef DISABLE_STATIC
 export LD_LIBRARY_PATH:=$(CURDIR)/..
endif

ifeq ($(TARGETOS),Darwin)
 CFLAGS+=-Wl,-flat_namespace,-undefined,warning
 export MACOSX_DEPLOYMENT_TARGET:=10.2
 NATIVE_DEFINES+=-D_ANSI_SOURCE
endif

# run local version of mkcc with local libraries and includes
MKCCFLAGS = -B$(TOP) -I$(TOP) -I$(top_srcdir) -I$(top_srcdir)/include -L$(TOP)
ifdef CONFIG_WIN32
 MKCCFLAGS = -B$(top_srcdir)/win32 -I$(top_srcdir) -I$(top_srcdir)/include -L$(TOP)
endif
XMKCCFLAGS = -B$(TOP) -B$(top_srcdir)/win32 -I$(TOP) -I$(top_srcdir) -I$(top_srcdir)/include

MKCC = $(TOP)/mkcc $(MKCCFLAGS)
RUN_MKCC = $(NATIVE_DEFINES) -DONE_SOURCE -run $(top_srcdir)/mkcc.c $(MKCCFLAGS)

DISAS = objdump -d

# libmkcc test
ifdef LIBMKCC1
 ifdef CONFIG_WIN32
    LIBMKCC1:=$(TOP)/win32/libmkcc/libmkcc.a
 else
    LIBMKCC1:=$(TOP)/$(LIBMKCC1)
 endif
endif

all test : $(TESTS)

hello-exe: ../examples/ex1.c
	@echo ------------ $@ ------------
	$(MKCC) $< -o hello$(EXESUF) || ($(TOP)/mkcc -vv; exit 1) && ./hello$(EXESUF)

hello-cross: ../examples/ex1.c
	@echo ------------ $@ ------------
	for XMKCC in $(PROGS_CROSS) ; \
		do echo -n "Test of $$XMKCC...  "; \
		out=$$($(TOP)/$$XMKCC $(XMKCCFLAGS) -c $< 2>&1); \
		test $$? -ne 0 && { echo "Failed\n$$out\n" ; $(TOP)/$$XMKCC -vv; exit 1; } ; \
		echo "Success"; \
	done

hello-run: ../examples/ex1.c
	@echo ------------ $@ ------------
	$(MKCC) -run $<

libtest: libmkcc_test$(EXESUF) $(LIBMKCC1)
	@echo ------------ $@ ------------
	./libmkcc_test$(EXESUF) $(MKCCFLAGS)

libmkcc_test$(EXESUF): libmkcc_test.c $(top_builddir)/$(LIBMKCC)
	$(CC) -o $@ $^ $(CPPFLAGS) $(CFLAGS) $(NATIVE_DEFINES) $(LIBS) $(LINK_LIBMKCC) $(LDFLAGS) -I$(top_srcdir)

%-dir:
	@echo ------------ $@ ------------
	$(MAKE) -k -C $*

# test.ref - generate using cc
test.ref: mkcctest.c
	$(CC) -o mkcctest.cc $< -I$(top_srcdir) $(CPPFLAGS) -w $(CFLAGS) $(NATIVE_DEFINES) -std=gnu99 -O0 -fno-omit-frame-pointer $(LDFLAGS)
	./mkcctest.cc > $@

# auto test
test1 test1b: mkcctest.c test.ref
	@echo ------------ $@ ------------
	$(MKCC) -run $< > test.out1
	@diff -u test.ref test.out1 && echo "Auto Test OK"

# iterated test2 (compile mkcc then compile mkcctest.c !)
test2 test2b: mkcctest.c test.ref
	@echo ------------ $@ ------------
	$(MKCC) $(RUN_MKCC) $(RUN_MKCC) -run $< > test.out2
	@diff -u test.ref test.out2 && echo "Auto Test2 OK"

# iterated test3 (compile mkcc then compile mkcc then compile mkcctest.c !)
test3 test3b: mkcctest.c test.ref
	@echo ------------ $@ ------------
	$(MKCC) $(RUN_MKCC) $(RUN_MKCC) $(RUN_MKCC) -run $< > test.out3
	@diff -u test.ref test.out3 && echo "Auto Test3 OK"

test%b : MKCCFLAGS += -b

# binary output test
test4: mkcctest.c test.ref
	@echo ------------ $@ ------------
# object + link output
	$(MKCC) -c -o mkcctest3.o $<
	$(MKCC) -o mkcctest3 mkcctest3.o
	./mkcctest3 > test3.out
	@if diff -u test.ref test3.out ; then echo "Object Auto Test OK"; fi
# dynamic output
	$(MKCC) -o mkcctest1 $<
	./mkcctest1 > test1.out
	@if diff -u test.ref test1.out ; then echo "Dynamic Auto Test OK"; fi
# dynamic output + bound check
	$(MKCC) -b -o mkcctest4 $<
	./mkcctest4 > test4.out
	@if diff -u test.ref test4.out ; then echo "BCheck Auto Test OK"; fi
# static output
	$(MKCC) -static -o mkcctest2 $<
	./mkcctest2 > test2.out
	@if diff -u test.ref test2.out ; then echo "Static Auto Test OK"; fi

# memory and bound check auto test
BOUNDS_OK  = 1 4 8 10 14
BOUNDS_FAIL= 2 5 7 9 11 12 13 15

btest: boundtest.c
	@echo ------------ $@ ------------
	@for i in $(BOUNDS_OK); do \
	   echo ; echo --- boundtest $$i ---; \
	   if $(MKCC) -b -run $< $$i ; then \
	       echo succeeded as expected; \
	   else\
	       echo Failed positive test $$i ; exit 1 ; \
	   fi ;\
	done ;\
	for i in $(BOUNDS_FAIL); do \
	   echo ; echo --- boundtest $$i ---; \
	   if $(MKCC) -b -run $< $$i ; then \
	       echo Failed negative test $$i ; exit 1 ;\
	   else\
	       echo failed as expected; \
	   fi ;\
	done ;\
	echo; echo Bound test OK

# speed test
speedtest: ex2 ex3
	@echo ------------ $@ ------------
	time ./ex2 1238 2 3 4 10 13 4
	time $(MKCC) -run $(top_srcdir)/examples/ex2.c 1238 2 3 4 10 13 4
	time ./ex3 35
	time $(MKCC) -run $(top_srcdir)/examples/ex3.c 35

weaktest: mkcctest.c test.ref
	$(MKCC) -c $< -o weaktest.mkcc.o $(CPPFLAGS) $(CFLAGS)
	 $(CC) -c $< -o weaktest.cc.o -I. $(CPPFLAGS) -w $(CFLAGS)
	objdump -t weaktest.mkcc.o | grep ' w ' | sed -e 's/.* \([a-zA-Z0-9_]*\)$$/\1/' | LC_ALL=C sort > weaktest.mkcc.o.txt
	objdump -t weaktest.cc.o | grep ' w ' | sed -e 's/.* \([a-zA-Z0-9_]*\)$$/\1/' | LC_ALL=C sort > weaktest.cc.o.txt
	diff weaktest.cc.o.txt weaktest.mkcc.o.txt && echo "Weak Auto Test OK"

ex%: $(top_srcdir)/examples/ex%.c
	$(CC) -o $@ $< $(CPPFLAGS) $(CFLAGS) $(LDFLAGS)

# tiny assembler testing
asmtest.ref: asmtest.S
	$(CC) -Wa,-W -o asmtest.ref.o -c asmtest.S
	objdump -D asmtest.ref.o > asmtest.ref

asmtest: asmtest.ref
	@echo ------------ $@ ------------
	$(MKCC) -c asmtest.S
	objdump -D asmtest.o > asmtest.out
	@if diff -u --ignore-matching-lines="file format" asmtest.ref asmtest.out ; then echo "ASM Auto Test OK"; fi

# Check that code generated by libmkcc is binary compatible with
# that generated by CC
abitest-cc$(EXESUF): abitest.c $(top_builddir)/$(LIBMKCC)
	$(CC) -o $@ $^ $(CPPFLAGS) $(CFLAGS) $(NATIVE_DEFINES) $(LIBS) $(LINK_LIBMKCC) $(LDFLAGS) -I$(top_srcdir)

abitest-mkcc$(EXESUF): abitest.c libmkcc.c
	$(MKCC) -o $@ $^ $(CPPFLAGS) $(CFLAGS) $(NATIVE_DEFINES) -DONE_SOURCE $(LIBS) $(LDFLAGS) -I$(top_srcdir)

ABITESTS := abitest-cc$(EXESUF)
ifneq ($(CONFIG_arm_eabi),yes) # not ARM soft-float
	ABITESTS += abitest-mkcc$(EXESUF)
endif

abitest: $(ABITESTS)
	@echo ------------ $@ ------------
	./abitest-cc$(EXESUF) $(MKCCFLAGS)
	if [ "$(CONFIG_arm_eabi)" != "yes" ]; then ./abitest-mkcc$(EXESUF) $(MKCCFLAGS); fi

vla_test$(EXESUF): vla_test.c
	$(MKCC) -o $@ $^ $(CPPFLAGS) $(CFLAGS)
vla_test-run: vla_test$(EXESUF)
	@echo ------------ $@ ------------
	./vla_test$(EXESUF)

# targets for development
%.bin: %.c mkcc
	$(MKCC) -g -o $@ $<
	$(DISAS) $@

instr: instr.o
	objdump -d instr.o

instr.o: instr.S
	$(CC) -o $@ -c $< -O2 -Wall -g

cache: mkcc_g
	cachegrind ./mkcc_g -o /tmp/linpack -lm bench/linpack.c
	vg_annotate mkcc.c > /tmp/linpack.cache.log

# clean
clean:
	$(MAKE) -C tests2 $@
	$(MAKE) -C pp $@
	rm -vf *~ *.o *.a *.bin *.i *.ref *.out *.out? *.out?b *.cc \
		*-cc *-mkcc *.exe \
		hello libmkcc_test vla_test mkcctest[1234] ex? mkcc_g

