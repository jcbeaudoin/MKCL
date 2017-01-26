#
# Tiny C Compiler Makefile
#

TOP ?= .
include $(TOP)/config.mak
VPATH = $(top_srcdir)

CPPFLAGS += -I$(TOP) # for config.h

ifneq (-$(findstring gcc,$(CC))-,-gcc-)
 ifeq (-$(findstring clang,$(CC))-,-clang-)
 # make clang accept gnuisms in libmkcc1.c
  CFLAGS+=-fheinous-gnu-extensions
 endif
endif

CPPFLAGS_P=$(CPPFLAGS) -DCONFIG_MKCC_STATIC
CFLAGS_P=$(CFLAGS) -pg -static
LIBS_P=
LDFLAGS_P=$(LDFLAGS)

ifdef CONFIG_WIN64
 CONFIG_WIN32=yes
endif

ifndef CONFIG_WIN32
 LIBS=-lm
 ifndef CONFIG_NOLDL
  LIBS+=-ldl
 endif
endif

# make libmkcc as static or dynamic library?
ifdef DISABLE_STATIC
 ifndef CONFIG_WIN32
 LIBMKCC=libmkcc.so.1.0
 else
 LIBMKCC=libmkcc.dll
 LIBMKCC_DLL=yes
 LIBMKCC_EXTRA=libmkcc.def libmkcc.a
 endif
 LINK_LIBMKCC=-Wl,-rpath,"$(libdir)"
 ifdef DISABLE_RPATH
 LINK_LIBMKCC=
 endif
else
 LIBMKCC=libmkcc.a
 LINK_LIBMKCC=
endif

CONFIG_$(ARCH) = yes
NATIVE_DEFINES_$(CONFIG_i386) += -DMKCC_TARGET_I386
NATIVE_DEFINES_$(CONFIG_x86-64) += -DMKCC_TARGET_X86_64
NATIVE_DEFINES_$(CONFIG_WIN32) += -DMKCC_TARGET_PE
NATIVE_DEFINES_$(CONFIG_uClibc) += -DMKCC_UCLIBC
NATIVE_DEFINES_$(CONFIG_arm) += -DMKCC_TARGET_ARM
NATIVE_DEFINES_$(CONFIG_arm_eabihf) += -DMKCC_ARM_EABI -DMKCC_ARM_HARDFLOAT
NATIVE_DEFINES_$(CONFIG_arm_eabi) += -DMKCC_ARM_EABI
NATIVE_DEFINES_$(CONFIG_arm_vfp) += -DMKCC_ARM_VFP
NATIVE_DEFINES_$(CONFIG_arm64) += -DMKCC_TARGET_ARM64
NATIVE_DEFINES += $(NATIVE_DEFINES_yes)

ifeq ($(TOP),.)

PROGS=mkcc$(EXESUF)
I386_CROSS = i386-linux-gnu-mkcc$(EXESUF)
WIN32_CROSS = i386-win-mingw32-mkcc$(EXESUF)
WIN64_CROSS = x86_64-win-mingw32-mkcc$(EXESUF)
WINCE_CROSS = arm-win-mingw32ce-mkcc$(EXESUF)
X64_CROSS = x86_64-linux-gnu-mkcc$(EXESUF)
ARM_FPA_CROSS = arm-linux-fpa-mkcc$(EXESUF)
ARM_FPA_LD_CROSS = arm-linux-fpa-ld-mkcc$(EXESUF)
ARM_VFP_CROSS = arm-linux-gnu-mkcc$(EXESUF)
ARM_EABI_CROSS = arm-linux-gnueabi-mkcc$(EXESUF)
ARM_EABIHF_CROSS = arm-linux-gnueabihf-mkcc$(EXESUF)
ARM_CROSS = $(ARM_FPA_CROSS) $(ARM_FPA_LD_CROSS) $(ARM_VFP_CROSS) $(ARM_EABI_CROSS)
ARM64_CROSS = arm64-mkcc$(EXESUF)
C67_CROSS = c67-mkcc$(EXESUF)

# Legacy symlinks for cross compilers
$(I386_CROSS)_LINK = i386-mkcc$(EXESUF)
$(WIN32_CROSS)_LINK = i386-win-mkcc$(EXESUF)
$(WIN64_CROSS)_LINK = x86_64-win-mkcc$(EXESUF)
$(WINCE_CROSS)_LINK = arm-win-mkcc$(EXESUF)
$(X64_CROSS)_LINK = x86_64-mkcc$(EXESUF)
$(ARM_FPA_CROSS)_LINK = arm-fpa-mkcc$(EXESUF)
$(ARM_FPA_LD_CROSS)_LINK = arm-fpa-ld-mkcc$(EXESUF)
$(ARM_VFP_CROSS)_LINK = arm-vfp-mkcc$(EXESUF)
$(ARM_EABI_CROSS)_LINK = arm-eabi-mkcc$(EXESUF)

ifdef CONFIG_CROSS

 ifeq ($(TARGETOS),Windows)
  ifeq ($(ARCH),i386)
   PROGS:=$($(WIN32_CROSS)_LINK)
   $($(WIN32_CROSS)_LINK)_MKCC = yes
  endif
  ifeq ($(ARCH),x86-64)
   PROGS:=$($(WIN64_CROSS)_LINK)
   $($(WIN64_CROSS)_LINK)_MKCC = yes
  endif
 endif

 ifeq ($(TARGETOS),Linux)
  ifeq ($(ARCH),i386)
   PROGS:=$($(I386_CROSS)_LINK)
   $($(I386_CROSS)_LINK)_MKCC = yes
  endif
  ifeq ($(ARCH),x86-64)
   PROGS:=$($(X64_CROSS)_LINK)
   $($(X64_CROSS)_LINK)_MKCC = yes
  endif
 endif

endif # def CONFIG_CROSS

CORE_FILES = mkcc.c libmkcc.c mkccpp.c mkccgen.c mkccelf.c mkccasm.c mkccrun.c
CORE_FILES += mkcc.h config.h libmkcc.h mkcctok.h
I386_FILES = $(CORE_FILES) i386-gen.c i386-asm.c i386-asm.h i386-tok.h
WIN32_FILES = $(CORE_FILES) i386-gen.c i386-asm.c i386-asm.h i386-tok.h mkccpe.c
WIN64_FILES = $(CORE_FILES) x86_64-gen.c i386-asm.c x86_64-asm.h mkccpe.c
WINCE_FILES = $(CORE_FILES) arm-gen.c mkccpe.c
X86_64_FILES = $(CORE_FILES) x86_64-gen.c i386-asm.c x86_64-asm.h
ARM_FILES = $(CORE_FILES) arm-gen.c
ARM64_FILES = $(CORE_FILES) arm64-gen.c
C67_FILES = $(CORE_FILES) c67-gen.c mkcccoff.c

ifdef CONFIG_WIN64
PROGS+=tiny_impdef$(EXESUF)
NATIVE_FILES=$(WIN64_FILES)
PROGS_CROSS=$(WIN32_CROSS) $(I386_CROSS) $(X64_CROSS) $(ARM_CROSS) $(ARM64_CROSS) $(C67_CROSS) $(WINCE_CROSS)
LIBMKCC1_CROSS=lib/i386-win/libmkcc1.a
LIBMKCC1=libmkcc1.a
else ifdef CONFIG_WIN32
PROGS+=tiny_impdef$(EXESUF)
NATIVE_FILES=$(WIN32_FILES)
PROGS_CROSS=$(WIN64_CROSS) $(I386_CROSS) $(X64_CROSS) $(ARM_CROSS) $(ARM64_CROSS) $(C67_CROSS) $(WINCE_CROSS)
LIBMKCC1_CROSS=lib/x86_64-win/libmkcc1.a
LIBMKCC1=libmkcc1.a
else ifeq ($(ARCH),i386)
NATIVE_FILES=$(I386_FILES)
PROGS_CROSS=$($(X64_CROSS)_LINK) $($(WIN32_CROSS)_LINK) $($(WIN64_CROSS)_LINK) $(ARM_CROSS) $(ARM64_CROSS) $(C67_CROSS) $(WINCE_CROSS)
LIBMKCC1_CROSS=lib/i386-win/libmkcc1.a lib/x86_64-win/libmkcc1.a lib/i386/libmkcc1.a lib/x86_64/libmkcc1.a \
    lib/arm64/libmkcc1.a
LIBMKCC1=libmkcc1.a
else ifeq ($(ARCH),x86-64)
 ifeq ($(TARGETOS),Darwin)
  NATIVE_FILES=$(X86_64_FILES)
  PROGS_CROSS=$($(I386_CROSS)_LINK) $($(WIN32_CROSS)_LINK) $($(WIN64_CROSS)_LINK) $(ARM_CROSS) $(C67_CROSS) $(WINCE_CROSS)
  LIBMKCC1_CROSS=lib/i386-win/libmkcc1.a lib/x86_64-win/libmkcc1.a lib/i386/libmkcc1.a lib/x86_64/libmkcc1.a
  LIBMKCC1=libmkcc1.a
 else
  NATIVE_FILES=$(X86_64_FILES)
  PROGS_CROSS=$($(I386_CROSS)_LINK) $($(WIN32_CROSS)_LINK) $($(WIN64_CROSS)_LINK) $(ARM_CROSS) $(ARM64_CROSS) $(C67_CROSS) $(WINCE_CROSS)
  LIBMKCC1_CROSS=lib/i386-win/libmkcc1.a lib/x86_64-win/libmkcc1.a lib/i386/libmkcc1.a lib/x86_64/libmkcc1.a \
    lib/arm64/libmkcc1.a
  LIBMKCC1=libmkcc1.a
 endif
else ifeq ($(ARCH),arm)
 NATIVE_FILES=$(ARM_FILES)
 PROGS_CROSS=$(I386_CROSS) $(X64_CROSS) $(WIN32_CROSS) $(WIN64_CROSS) $(ARM64_CROSS) $(C67_CROSS) $(WINCE_CROSS)
 LIBMKCC1=libmkcc1.a
 LIBMKCC1_CROSS=lib/i386-win/libmkcc1.a lib/x86_64-win/libmkcc1.a lib/i386/libmkcc1.a
else ifeq ($(ARCH),arm64)
 NATIVE_FILES=$(ARM64_FILES)
 PROGS_CROSS=$(I386_CROSS) $(X64_CROSS) $(WIN32_CROSS) $(WIN64_CROSS) $(ARM_CROSS) $(C67_CROSS) $(WINCE_CROSS)
 LIBMKCC1=libmkcc1.a
 LIBMKCC1_CROSS=lib/i386-win/libmkcc1.a lib/x86_64-win/libmkcc1.a lib/i386/libmkcc1.a
endif
PROGS_CROSS_LINK=$(foreach PROG_CROSS,$(PROGS_CROSS),$($(PROG_CROSS)_LINK))

PROGS+=tiny_libmaker$(EXESUF)

MKCCLIBS = $(LIBMKCC1) $(LIBMKCC) $(LIBMKCC_EXTRA)
MKCCDOCS = mkcc.1 mkcc-doc.html mkcc-doc.info

ifdef CONFIG_CROSS
 PROGS+=$(PROGS_CROSS)
 MKCCLIBS+=$(LIBMKCC1_CROSS)
endif

all: $(PROGS) $(MKCCLIBS) $(MKCCDOCS)

ifdef ONE_SOURCE
 NATIVE_MKCC_REQUISITES=mkcc.o
else
 NATIVE_MKCC_REQUISITES=mkcc.o $(LIBMKCC)
endif

# Host Tiny C Compiler
mkcc$(EXESUF): $(NATIVE_MKCC_REQUISITES)
	$(CC) -o $@ $^ $(LIBS) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $(LINK_LIBMKCC)

# Cross Tiny C Compilers
%-mkcc$(EXESUF): mkcc.c
	$(CC) -o $@ $< -DONE_SOURCE $(if $($@_MKCC),$(NATIVE_DEFINES),$(DEFINES)) $(CPPFLAGS) $(CFLAGS) $(LIBS) $(LDFLAGS)
	$(if $($@_LINK),ln -sf $@ $($@_LINK))
	$(if $($@_MKCC),ln -sf $@ mkcc$(EXESUF))

# profiling version
mkcc_p$(EXESUF): $(NATIVE_FILES)
	$(CC) -o $@ $< -DONE_SOURCE $(NATIVE_DEFINES) $(CPPFLAGS_P) $(CFLAGS_P) $(LIBS_P) $(LDFLAGS_P)

$(I386_CROSS) $($(I386_CROSS)_LINK): DEFINES = -DMKCC_TARGET_I386
$(X64_CROSS) $($(X64_CROSS)_LINK): DEFINES = -DMKCC_TARGET_X86_64
$(WIN32_CROSS) $($(WIN32_CROSS)_LINK): DEFINES = -DMKCC_TARGET_I386 -DMKCC_TARGET_PE \
    -DCONFIG_MKCCDIR="\"$(mkccdir)/win32\"" \
    -DCONFIG_MKCC_LIBPATHS="\"{B}/lib/32;{B}/lib\""
$(WIN64_CROSS) $($(WIN64_CROSS)_LINK): DEFINES = -DMKCC_TARGET_X86_64 -DMKCC_TARGET_PE \
    -DCONFIG_MKCCDIR="\"$(mkccdir)/win32\"" \
    -DCONFIG_MKCC_LIBPATHS="\"{B}/lib/64;{B}/lib\""
$(WINCE_CROSS): DEFINES = -DMKCC_TARGET_PE
$(C67_CROSS): DEFINES = -DMKCC_TARGET_C67
$(ARM_FPA_CROSS): DEFINES = -DMKCC_TARGET_ARM
$(ARM_FPA_LD_CROSS)$(EXESUF): DEFINES = -DMKCC_TARGET_ARM -DLDOUBLE_SIZE=12
$(ARM_VFP_CROSS): DEFINES = -DMKCC_TARGET_ARM -DMKCC_ARM_VFP
$(ARM_EABI_CROSS): DEFINES = -DMKCC_TARGET_ARM -DMKCC_ARM_EABI -DMKCC_ARM_VFP
$(ARM64_CROSS): DEFINES = -DMKCC_TARGET_ARM64

$(I386_CROSS) $($(I386_CROSS)_LINK): $(I386_FILES)
$(X64_CROSS)  $($(X64_CROSS)_LINK): $(X86_64_FILES)
$(WIN32_CROSS) $($(WIN32_CROSS)_LINK): $(WIN32_FILES)
$(WIN64_CROSS) $($(WIN64_CROSS)_LINK): $(WIN64_FILES)
$(WINCE_CROSS) $($(WINCE_CROSS)_LINK): $(WINCE_FILES)
$(C67_CROSS)  $($(C67_CROSS)_LINK): $(C67_FILES)
$(ARM_FPA_CROSS) $(ARM_FPA_LD_CROSS) $(ARM_VFP_CROSS) $(ARM_EABI_CROSS): $(ARM_FILES)
$($(ARM_FPA_CROSS)_LINK) $($(ARM_FPA_LD_CROSS)_LINK) $($(ARM_VFP_CROSS)_LINK) $($(ARM_EABI_CROSS)_LINK): $(ARM_FILES)
$(ARM64_CROSS): $(ARM64_FILES)

# libmkcc generation and test
ifndef ONE_SOURCE
 LIBMKCC_OBJ = $(filter-out mkcc.o,$(patsubst %.c,%.o,$(filter %.c,$(NATIVE_FILES))))
 LIBMKCC_INC = $(filter %.h,$(CORE_FILES)) $(filter-out $(CORE_FILES),$(NATIVE_FILES))
else
 LIBMKCC_OBJ = libmkcc.o
 LIBMKCC_INC = $(NATIVE_FILES)
 libmkcc.o : NATIVE_DEFINES += -DONE_SOURCE
 mkcc.o : NATIVE_DEFINES += -DONE_SOURCE
endif

$(LIBMKCC_OBJ) mkcc.o : %.o : %.c $(LIBMKCC_INC)
	$(CC) -o $@ -c $< $(NATIVE_DEFINES) $(CPPFLAGS) $(CFLAGS)

ifndef LIBMKCC_DLL
 libmkcc.a: $(LIBMKCC_OBJ)
	$(AR) rcs $@ $^
endif

libmkcc.so.1.0: $(LIBMKCC_OBJ)
	$(CC) -shared -Wl,-soname,$@ -o $@ $^ $(LDFLAGS)

libmkcc.so.1.0: CFLAGS+=-fPIC

ifdef LIBMKCC_DLL
 libmkcc.dll libmkcc.def libmkcc.a: $(LIBMKCC_OBJ)
	$(CC) -shared $^ -o $@ $(LDFLAGS) -Wl,--output-def,libmkcc.def,--out-implib,libmkcc.a
endif

# windows utilities
tiny_impdef$(EXESUF): win32/tools/tiny_impdef.c
	$(CC) -o $@ $< $(CPPFLAGS) $(CFLAGS) $(LDFLAGS)
tiny_libmaker$(EXESUF): win32/tools/tiny_libmaker.c
	$(CC) -o $@ $< $(CPPFLAGS) $(CFLAGS) $(LDFLAGS)

# TinyCC runtime libraries
libmkcc1.a : FORCE $(PROGS)
	$(MAKE) -C lib native
	if test ! -d $(ARCH); then mkdir $(ARCH); fi
	if test ! -L $(ARCH)/$@; then ln -sf ../$@ $(ARCH)/$@; fi
lib/%/libmkcc1.a : FORCE $(PROGS_CROSS)
	$(MAKE) -C lib cross TARGET=$*

FORCE:

# install
MKCC_INCLUDES = stdarg.h stddef.h stdbool.h float.h varargs.h
INSTALL=install
ifdef STRIP_BINARIES
 INSTALLBIN=$(INSTALL) -s
else
 INSTALLBIN=$(INSTALL)
endif

ifdef CONFIG_WIN32
 CONFIG_WIN=yes
endif
ifdef CONFIG_WIN64
 CONFIG_WIN=yes
endif

install-strip: install
	strip $(foreach PROG,$(PROGS),"$(bindir)"/$(PROG))

ifndef CONFIG_WIN
install: $(PROGS) $(MKCCLIBS) $(MKCCDOCS)
	mkdir -p "$(bindir)"
	$(INSTALLBIN) -m755 $(PROGS) "$(bindir)"
	cp -P mkcc$(EXESUF) "$(bindir)"
	mkdir -p "$(mandir)/man1"
	-$(INSTALL) -m644 mkcc.1 "$(mandir)/man1"
	mkdir -p "$(infodir)"
	-$(INSTALL) -m644 mkcc-doc.info "$(infodir)"
	mkdir -p "$(mkccdir)"
	mkdir -p "$(mkccdir)/include"
ifneq ($(LIBMKCC1),)
	mkdir -p "$(mkccdir)/$(ARCH)"
	$(INSTALL) -m644 $(LIBMKCC1) "$(mkccdir)/$(ARCH)"
endif
	$(INSTALL) -m644 $(addprefix $(top_srcdir)/include/,$(MKCC_INCLUDES)) $(top_srcdir)/mkcclib.h "$(mkccdir)/include"
	mkdir -p "$(libdir)"
	$(INSTALL) -m644 $(LIBMKCC) "$(libdir)"
ifdef DISABLE_STATIC
	ln -sf "$(ln_libdir)/libmkcc.so.1.0" "$(libdir)/libmkcc.so.1"
	ln -sf "$(ln_libdir)/libmkcc.so.1.0" "$(libdir)/libmkcc.so"
endif
	mkdir -p "$(includedir)"
	$(INSTALL) -m644 $(top_srcdir)/libmkcc.h "$(includedir)"
	mkdir -p "$(docdir)"
	-$(INSTALL) -m644 mkcc-doc.html "$(docdir)"
ifdef CONFIG_CROSS
	mkdir -p "$(mkccdir)/win32/lib/32"
	mkdir -p "$(mkccdir)/win32/lib/64"
	mkdir -p "$(mkccdir)/i386"
	mkdir -p "$(mkccdir)/x86-64"
ifneq ($(HOST_OS),Darwin)
	mkdir -p "$(mkccdir)/arm64"
	$(INSTALL) -m644 lib/arm64/libmkcc1.a "$(mkccdir)/arm64"
endif
	$(INSTALL) -m644 lib/i386/libmkcc1.a "$(mkccdir)/i386"
	$(INSTALL) -m644 lib/x86_64/libmkcc1.a "$(mkccdir)/x86-64"
	$(INSTALL) -m644 $(top_srcdir)/win32/lib/*.def "$(mkccdir)/win32/lib"
	$(INSTALL) -m644 lib/i386-win/libmkcc1.a "$(mkccdir)/win32/lib/32"
	$(INSTALL) -m644 lib/x86_64-win/libmkcc1.a "$(mkccdir)/win32/lib/64"
	cp -r $(top_srcdir)/win32/include/. "$(mkccdir)/win32/include"
	cp -r "$(mkccdir)/include" "$(mkccdir)/win32"
endif

uninstall:
	rm -fv $(foreach P,$(PROGS),"$(bindir)/$P")
	rm -fv "$(bindir)/mkcc$(EXESUF)"
	rm -fv $(foreach P,$(LIBMKCC1),"$(mkccdir)/$P")
	rm -fv $(foreach P,$(MKCC_INCLUDES),"$(mkccdir)/include/$P")
	rm -fv "$(mandir)/man1/mkcc.1" "$(infodir)/mkcc-doc.info"
	rm -fv "$(libdir)/$(LIBMKCC)" "$(includedir)/libmkcc.h"
	rm -fv "$(libdir)/libmkcc.so*"
	rm -rv "$(mkccdir)"
	rm -rv "$(docdir)"
else
# on windows
install: $(PROGS) $(MKCCLIBS) $(MKCCDOCS)
	mkdir -p "$(mkccdir)"
	mkdir -p "$(mkccdir)/lib"
	mkdir -p "$(mkccdir)/include"
	mkdir -p "$(mkccdir)/examples"
	mkdir -p "$(mkccdir)/doc"
	mkdir -p "$(mkccdir)/libmkcc"
	$(INSTALLBIN) -m755 $(PROGS) "$(mkccdir)"
	$(INSTALLBIN) -m755 mkcc.exe "$(mkccdir)"
	$(INSTALL) -m644 $(LIBMKCC1) $(top_srcdir)/win32/lib/*.def "$(mkccdir)/lib"
	cp -r $(top_srcdir)/win32/include/. "$(mkccdir)/include"
	cp -r $(top_srcdir)/win32/examples/. "$(mkccdir)/examples"
	$(INSTALL) -m644 $(addprefix $(top_srcdir)/include/,$(MKCC_INCLUDES)) $(top_srcdir)/mkcclib.h "$(mkccdir)/include"
	$(INSTALL) -m644 mkcc-doc.html $(top_srcdir)/win32/mkcc-win32.txt "$(mkccdir)/doc"
	$(INSTALL) -m644 $(top_srcdir)/libmkcc.h $(LIBMKCC_EXTRA) "$(mkccdir)/libmkcc"
	$(INSTALL) -m644 $(LIBMKCC) "$(mkccdir)"
ifdef CONFIG_CROSS
	mkdir -p "$(mkccdir)/lib/32"
	mkdir -p "$(mkccdir)/lib/64"
	-$(INSTALL) -m644 lib/i386-win/libmkcc1.a "$(mkccdir)/lib/32"
	-$(INSTALL) -m644 lib/x86_64-win/libmkcc1.a "$(mkccdir)/lib/64"
endif

uninstall:
	rm -rfv "$(mkccdir)/*"
endif

# documentation and man page
mkcc-doc.html: mkcc-doc.texi
	-makeinfo --no-split --html --number-sections -o $@ $<

mkcc.1: mkcc-doc.texi
	-$(top_srcdir)/texi2pod.pl $< mkcc.pod
	-version=`cat $(top_srcdir)/VERSION`; \
         pod2man --section=1 --center="ManKai Tiny C Compiler" --release="$$version" mkcc.pod > $@

mkcc-doc.info: mkcc-doc.texi
	-makeinfo $<

# in tests subdir
export LIBMKCC1

%est:
	$(MAKE) -C tests $@ 'PROGS_CROSS=$(PROGS_CROSS)'

clean:
	rm -vf $(PROGS) mkcc_p$(EXESUF) mkcc.pod *~ *.o *.a *.so* *.out *.log \
		*.exe a.out tags TAGS libmkcc_test$(EXESUF) mkcc$(EXESUF)
	-rm -r $(ARCH) arm64
ifeq ($(HOST_OS),Linux)
	-rm -r ./C:
endif
	-rm *-mkcc$(EXESUF)
	$(MAKE) -C tests $@
ifneq ($(LIBMKCC1),)
	$(MAKE) -C lib $@
endif

distclean: clean
	rm -vf config.h config.mak config.texi mkcc.1 mkcc-doc.info mkcc-doc.html

config.mak:
	@echo "Please run ./configure."
	@exit 1

tags:
	ctags $(top_srcdir)/*.[ch] $(top_srcdir)/include/*.h $(top_srcdir)/lib/*.[chS]

TAGS:
	etags $(top_srcdir)/*.[ch] $(top_srcdir)/include/*.h $(top_srcdir)/lib/*.[chS]

# create release tarball from *current* git branch (including mkcc-doc.html
# and converting two files to CRLF)
MKCC-VERSION := mkcc-$(shell cat $(top_srcdir)/VERSION)
tar:    mkcc-doc.html
	mkdir $(MKCC-VERSION)
	( cd $(MKCC-VERSION) && git --git-dir ../.git checkout -f )
	cp mkcc-doc.html $(MKCC-VERSION)
	for f in mkcc-win32.txt build-mkcc.bat ; do \
	    cat win32/$$f | sed 's,\(.*\),\1\r,g' > $(MKCC-VERSION)/win32/$$f ; \
	done
	tar cjf $(MKCC-VERSION).tar.bz2 $(MKCC-VERSION)
	rm -rf $(MKCC-VERSION)
	git reset

.PHONY: all clean tar tags TAGS distclean install uninstall FORCE

endif # ifeq ($(TOP),.)
