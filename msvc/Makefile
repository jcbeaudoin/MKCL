#
#		Makefile for ECoLisp
#
top_srcdir= ..\src
srcdir	= ..\src

# =============================================================
# ECL configuration
# <BEGIN>

SHORT_SITE_NAME   =
LONG_SITE_NAME    =
ECL_VERSION       = 9.6.2
ECL_VERSION_NUMBER= 90602
ARCHITECTURE      = PENTIUM4
SOFTWARE_TYPE     = NT
SOFTWARE_VERSION  = 5.0
THEHOST           = win32

# Define here the processor type to compile GMP library
# with maximum optimization. Possible values are:
# 	gc -> generic implementation
# 	p0 -> Pentium processor
# 	p3 -> Pentium III processor
# 	p4 -> Pentium IV processor
GMP_TYPE      = gc

# Set it to non-empty to include Win32 thread support
ECL_THREADS   =

# Set it to non-empty to include support for Unicode characters
ECL_UNICODE   =

# Set it to non-empty to enable Win32 debug support
#ECL_DEBUG   = 1

# Add the extensions to include in the build process. Comment any
# of the following lines to remove a feature from the build process
# LISP->C compiled
ECL_CMP     =
# ASDF support
ECL_ASDF    =
# TCP support
ECL_SOCKETS =
# X Windows support
# ECL_CLX     = 1
# Regression Tests support
ECL_RT      =
# Defsystem support
ECL_DEFSYS  =
# Profiling
ECL_PROFILE =

# <END> (ECL configuration)
# =============================================================

TAR_DIR = %CD%\ecl-$(ECL_VERSION)

# Programs used by "make":
#

CC	= cl
LIBS	= eclgc.lib eclgmp.lib user32.lib ws2_32.lib shell32.lib
RM	= del
RMDIR   = rmdir /Q /S
MKDIR   = mkdir
EXE	= .exe
CP      = copy /Y
MV      = move /Y
MSDEV   = msdev
MKNSI   = makensis.exe

# ==================== Flags ====================

#
# Configuration-specific (Debug/Release) options
#
!if "$(ECL_DEBUG)" != ""
CFLAGS_OPTIMIZE = /Od
CFLAGS_CONFIG = /Zi /D_DEBUG /MDd $(CFLAGS_OPTIMIZE)
LDFLAGS_CONFIG = /debug /nodefaultlib:msvcrt.lib
SHARED_LDFLAGS = /LDd
GCFLAGS =
!else
CFLAGS_OPTIMIZE = /O2
CFLAGS_CONFIG = /DNDEBUG /MD $(CFLAGS_OPTIMIZE)
LDFLAGS_CONFIG = /nodefaultlib:msvcrtd.lib
SHARED_LDFLAGS = /LD
GCFLAGS = nodebug=1
!endif

CFLAGS  = /EHsc /DGC_DLL /DGC_BUILD /nologo /D_CRT_SECURE_NO_DEPRECATE $(CFLAGS_CONFIG)
LDFLAGS	= /link /nologo /verbose:lib /nodefaultlib:libcmt /nodefaultlib:libcmtd /nodefaultlib:libc /nodefaultlib:libcd $(LDFLAGS_CONFIG)


# Additional configuration for thread support
#
!if "$(ECL_THREADS)" == ""
ENV_EXPORT = cl_env,DATA
!else
ENV_EXPORT = ecl_process_env
CFLAGS     = $(CFLAGS) /DECL_THREADS
DEF        = ecl-threads.def
!endif

# Additional configuration for Unicode support
#
!if "$(ECL_UNICODE)" != ""
CFLAGS     = $(CFLAGS) /DECL_UNICODE
UCDDAT     = ucd.dat
!endif

!MESSAGE C++ compiler flags: $(CFLAGS)
!MESSAGE C++ linker flags: $(LDFLAGS)

# ==================== Where To Install Things ====================

# The default location for installation.  Everything is placed in
# subdirectories of this directory.  The default values for many of
# the variables below are expressed in terms of this one, so you may
# not need to change them.  This defaults to /usr/local.
prefix=%CD%\package
exec_prefix=$(prefix)
bindir=$(prefix)
libdir=$(prefix)
includedir=$(prefix)
docdir=$(prefix)\doc

# Programs used by "make install":
#
SHELL = @SHELL@
INSTALL	= @INSTALL@
INSTALL_PROGRAM = @INSTALL_PROGRAM@
INSTALL_SCRIPT = @INSTALL_SCRIPT@
INSTALL_DATA = @INSTALL_DATA@
mkinstalldirs = $(SHELL) $(top_srcdir)/gc/mkinstalldirs

# Files
#
SUBDIR        = c gc gmp
LIBRARIES     =
TARGETS       = ecl2$(EXE)
DEF           = ecl.def

# Additional modules
# 
ECL_MODULES  =
ECL_FEATURES = (cons :wants-dlopen *features*)
!ifdef ECL_CMP
ECL_MODULES  = $(ECL_MODULES) cmp
ECL_FEATURES = (cons :wants-cmp $(ECL_FEATURES))
!endif
!ifdef ECL_ASDF
ECL_MODULES  = $(ECL_MODULES) asdf
ECL_FEATURES = (cons :wants-asdf $(ECL_FEATURES))
!endif
!ifdef ECL_SOCKETS
ECL_MODULES  = $(ECL_MODULES) sockets
ECL_FEATURES = (cons :wants-sockets $(ECL_FEATURES))
!endif
!ifdef ECL_CLX
ECL_MODULES  = $(ECL_MODULES) clx
ECL_FEATURES = (cons :wants-clx $(ECL_FEATURES))
!endif
!ifdef ECL_RT
ECL_MODULES  = $(ECL_MODULES) rt
ECL_FEATURES = (cons :wants-rt $(ECL_FEATURES))
!endif
!ifdef ECL_DEFSYS
ECL_MODULES  = $(ECL_MODULES) defsystem
ECL_FEATURES = (cons :wants-defsystem $(ECL_FEATURES))
!endif
!ifdef ECL_PROFILE
ECL_MODULES  = $(ECL_MODULES) profile
ECL_FEATURES = (cons :wants-profile $(ECL_FEATURES))
!endif

!MESSAGE ECL Modules: $(ECL_MODULES)
!MESSAGE ECL Features: $(ECL_FEATURES)

# Build rules
# 
all:	$(TARGETS) ecl-config.bat ecl-cc.bat
.PHONY:	all

%Makefile: $(srcdir)/%Makefile.in config.status
	./config.status

c\cut$(EXE): $(top_srcdir)\util\cut.c
	cd c
	$(MAKE) cut$(EXE)
	cd ..

$(TARGETS): $(UCDDAT) ecl_min$(EXE) compile.lsp sysfun.lsp BUILD-STAMP
	set ECLDIR=./
	ecl_min < compile.lsp
BUILD-STAMP: Makefile
	date /t > $@

ecl-static.lib: $(TARGETS)
	link /lib /out:$@ eclmin.lib c\all_symbols2.obj lsp.lib eclgmp.lib eclgc.lib

ucd.dat: $(top_srcdir)\..\contrib\unicode\ucd.dat
	copy $(top_srcdir)\..\contrib\unicode\ucd.dat .

ecl_min$(EXE): $(LIBRARIES) eclmin.lib $(UCDDAT)
	$(CC) /Fe$@ cinit.obj c\all_symbols.obj eclmin.lib $(LDFLAGS) $(LIBS)

.gdbinit: $(srcdir)\util\gdbinit
	$(CP) $(srcdir)\util\gdbinit $@

lsp/config.lsp: $(srcdir)/lsp/config.lsp.in Makefile c\cut$(EXE)
	if not exist lsp $(MKDIR) lsp
	c\cut "@ecldir\@" "$(libdir:\=/)" \
	    "@SHORT_SITE_NAME@" "$(SHORT_SITE_NAME)" \
	    "@LONG_SITE_NAME@" "$(LONG_SITE_NAME)" \
	    "@PACKAGE_VERSION@" "$(ECL_VERSION)" \
	    "@ARCHITECTURE@" "$(ARCHITECTURE)" \
	    "@SOFTWARE_TYPE@" "$(SOFTWARE_TYPE)" \
	    "@SOFTWARE_VERSION@" "$(SOFTWARE_VERSION)" \
	    "@thehost@" "$(THEHOST)" \
	    < $(srcdir)\lsp\config.lsp.in > lsp\config.lsp
compile.lsp: bare.lsp $(srcdir)/compile.lsp.in Makefile
	c\cut "@ecldir\@" "$(libdir:\=/)" \
	    "@libdir\@" "$(libdir:\=/)" \
	    "@true_srcdir@" "$(srcdir:\=/)" \
	    "@true_builddir@" "$(MAKEDIR:\=/)" \
	    "@CFLAGS@" "$(CFLAGS) -DGC_BUILD" \
	    "@ECL_CFLAGS@" "" \
	    "@CPPFLAGS@" "" \
	    "@LDRPATH@" "" \
	    "@LDFLAGS@" "$(LDFLAGS)" \
	    "@SHARED_LDFLAGS@" "$(SHARED_LDFLAGS)" \
	    "@BUNDLE_LDFLAGS@" "$(SHARED_LDFLAGS)" \
	    "@CLIBS@" "user32.lib ws2_32.lib shell32.lib" \
	    "@STATICLIBS@" "eclgmp.lib eclgc.lib" \
	    "@LIBS@" "user32.lib ws2_32.lib shell32.lib" \
	    "@CORE_LIBS@" "" \
	    "@FASL_LIBS@" "" \
	    "@OBJEXT@" "obj" \
	    "@SHAREDPREFIX@" "" \
	    "@SHAREDEXT@" "lib" \
	    "@LIBPREFIX@" "" \
	    "@LIBEXT@" "lib" \
	    "@EXEEXT@" ".exe" \
	    "@LDINSTALLNAME@" "" \
	    "@DEF@" "$(DEF)" \
            "@RANLIB@" "ranlib" \
	    "@LSP_FEATURES@" "$(ECL_FEATURES)" \
	    < $(srcdir)\compile.lsp.in > compile.lsp
bare.lsp: $(srcdir)/bare.lsp.in lsp/load.lsp clos/load.lsp cmp/load.lsp cmp/cmpdefs.lsp
	c\cut "@true_srcdir@" "$(srcdir:\=/)" \
	    "@true_builddir@" "$(MAKEDIR:\=/)" < $(srcdir)\bare.lsp.in > bare.lsp
lsp/load.lsp: $(srcdir)/lsp/load.lsp.in
	$(CP) $(srcdir)\lsp\load.lsp.in lsp\load.lsp
clos/load.lsp: $(srcdir)/clos/load.lsp.in
	if not exist clos $(MKDIR) clos
	$(CP) $(srcdir)\clos\load.lsp.in clos\load.lsp
cmp/load.lsp: $(srcdir)/cmp/load.lsp.in
	if not exist cmp $(MKDIR) cmp
	$(CP) $(srcdir)\cmp\load.lsp.in cmp\load.lsp
cmp/cmpdefs.lsp: $(srcdir)/cmp/cmpdefs.lsp Makefile
	c\cut "@ECL_CC@" "$(CC)" \
	    "@CFLAGS@" "$(CFLAGS)" \
        "@CFLAGS_OPTIMIZE@" "$(CFLAGS_OPTIMIZE)" \
	    "@ECL_CFLAGS@" "" \
	    "@CPPFLAGS@" "" \
	    "@LDRPATH@" "" \
	    "@LDFLAGS@" "$(LDFLAGS)" \
	    "@SHARED_LDFLAGS@" "$(SHARED_LDFLAGS)" \
	    "@BUNDLE_LDFLAGS@" "$(SHARED_LDFLAGS)" \
	    "@CLIBS@" "user32.lib ws2_32.lib shell32.lib" \
	    "@STATICLIBS@" "eclgmp.lib eclgc.lib" \
	    "@OBJEXT@" "obj" \
	    "@SHAREDPREFIX@" "" \
	    "@SHAREDEXT@" "dll" \
	    "@LIBPREFIX@" "" \
	    "@LIBEXT@" "lib" \
	    "@EXEEXT@" ".exe" \
	    "@ecldir\@" "NIL" \
	    "@libdir\@" "NIL" \
	    "@includedir\@" "NIL" \
	    < $(srcdir)\cmp\cmpdefs.lsp > cmp\cmpdefs.lsp
ecl-config.bat: util\ecl-config.bat Makefile
	c\cut "~A" "$(libdir:\=/)"\
	    "~*" "" \
	    "@ECL_CFLAGS@" "$(CFLAGS)" \
	    "@LDFLAGS@" "$(LDFLAGS)" \
	    "@CLIBS@" "" \
	    "@libdir@" "$(prefix:\=/)" \
	    "@includedir@" "$(prefix:\=/)/ecl" \
	    < util\ecl-config.bat > ecl-config.bat
ecl-cc.bat: util\ecl-cc.bat Makefile
	c\cut "@ECL_CFLAGS@" "$(CFLAGS)" \
	      "@LDFLAGS@" "$(LDFLAGS)" \
	      "@CLIBS@" "" \
	      "@libdir@" "$(prefix:\=/)" \
	      "@includedir@" "$(prefix:\=/)/ecl" \
	      < util\ecl-cc.bat > ecl-cc.bat


eclmin.lib: eclgmp.lib eclgc.lib lsp/config.lsp
	cd c
	$(MAKE) ECL_VERSION_NUMBER=$(ECL_VERSION_NUMBER) \
	        ECL_THREADS=$(ECL_THREADS) "ECL_CFLAGS=$(CFLAGS) -DGC_BUILD"
	cd ..
eclgc.lib:
	cd gc
	$(MAKE) $(GCFLAGS) ECL_THREADS=$(ECL_THREADS) "CFLAGS_CONFIG=$(CFLAGS_CONFIG)" gc.lib
	$(CP) gc.lib ..\eclgc.lib
	cd ..
	if not exist ecl\gc $(MKDIR) ecl\gc
	if not exist ecl\gc\private $(MKDIR) ecl\gc\private
	for %h in (gc.h gc_version.h gc_local_alloc.h gc_pthread_redirects.h \
	           gc_config_macros.h leak_detector.h gc_typed.h \
	           private\gc_priv.h private\gcconfig.h gc_mark.h \
		   new_gc_alloc.h weakpointer.h gc_pthread_redirects.h) \
	    do $(CP) $(srcdir)\gc\include\%h ecl\gc\%h
	$(CP) $(srcdir)\gc\libatomic_ops-1.2\src\atomic_ops.h ecl\gc
eclgmp.lib:
	cd gmp
	$(MAKE) "MPN_TYPE = $(GMP_TYPE)" "CFLAGS_CONFIG=$(CFLAGS_CONFIG)"
	$(CP) gmp.lib ..\eclgmp.lib
	$(CP) gmp.h ..\ecl\gmp.h
	cd ..
sysfun.lsp:
	$(CP) $(srcdir)\cmp\sysfun.lsp .\

rt.lisp:
	$(CP) $(srcdir)\..\contrib\rt\rt.lisp .\

install:
	IF NOT EXIST "$(prefix)" $(MKDIR) "$(prefix)"
	IF NOT EXIST "$(bindir)" $(MKDIR) "$(bindir)"
	for %i in ($(TARGETS) ecl.dll $(UCDDAT)) do $(CP) %i "$(bindir)\%i"
	IF EXIST "$(bindir)\ecl2$(EXE)" $(MV) "$(bindir)\ecl2$(EXE)" "$(bindir)\ecl$(EXE)"
	IF EXIST ecl2$(EXE).manifest $(CP) ecl2$(EXE).manifest "$(bindir)\ecl$(EXE).manifest"
	IF EXIST ecl.dll.manifest $(CP) ecl.dll.manifest "$(bindir)"
	$(CP) ecl-config.bat "$(bindir)\ecl-config.bat"
	$(CP) ecl-cc.bat "$(bindir)\ecl-cc.bat"
	IF NOT EXIST "$(includedir)\ecl" $(MKDIR) "$(includedir)\ecl"
	IF NOT EXIST "$(includedir)\ecl\gc" $(MKDIR) $(includedir)\ecl\gc
	IF NOT EXIST "$(includedir)\ecl\gc\private" $(MKDIR) "$(includedir)\ecl\gc\private"
	for %i in (ecl\*.h ecl\gc\*.h ecl\gc\private\*.h) do $(CP) %i $(includedir)\%i
	cd c
	$(MAKE) cut.exe
	cd ..
	c\cut.exe < ecl\config.h > $(includedir)\ecl\config.h
	IF NOT EXIST "$(libdir)" $(MKDIR) "$(libdir)"
	IF NOT EXIST "$(libdir)\ecl" $(MKDIR) "$(libdir)\ecl"
	for %i in ($(LIBRARIES) c\dpp.exe BUILD-STAMP help.doc ecl.lib) do $(CP) %i "$(libdir)"
	for /f %i in ('type MODULES') do $(CP) %i "$(libdir)"
	for %i in (ecl-static.lib) do IF EXIST %i $(CP) %i "$(libdir)"
	IF NOT EXIST "$(docdir)" $(MKDIR) "$(docdir)"
	for %i in (..\Copyright ..\ANNOUNCEMENT ..\LGPL ..\README.1st ..\src\CHANGELOG) do $(CP) %i "$(docdir)"

windows-nsi:
	IF EXIST "$(TAR_DIR)" $(RMDIR) "$(TAR_DIR)"
	$(MKDIR) "$(TAR_DIR)"
	$(MAKE) prefix="$(TAR_DIR)" install
	util\ecl_nsi.bat %%CD%%\"$(srcdir)"\util\ecl.nsi "$(TAR_DIR)" $(ECL_VERSION)
	"$(MKNSI)" "$(TAR_DIR)/ecl.nsi"
	$(MV) $(TAR_DIR)\Setup.exe ecl-$(ECL_VERSION).exe
	$(RMDIR) $(TAR_DIR)

clean: clean_ecl clean_lisp
	-$(RM) .gdbinit cinit.lib ecl_min.lib
	-$(RM) gc\*.pdb
	cd gc
	-$(MAKE) ECL_THREADS=$(ECL_THREADS) clean
	cd ..
	-for %h in (gc.h gc_local_alloc.h gc_pthread_redirects.h \
	            gc_config_macros.h leak_detector.h gc_typed.h \
		    gc_mark.h private\gc_priv.h private\gcconfig.h) \
	     do $(RM) ecl\gc\%h
	-$(RMDIR) ecl\gc\private
	-$(RMDIR) ecl\gc
	cd gmp
	-$(MAKE) ECL_THREADS=$(ECL_THREADS) clean
	cd ..
	-$(RM) ecl\gmp.h
clean_ecl:
	-for %i in (eclgc.lib eclgmp.lib lsp\config.lsp compile.lsp bare.lsp \
	            lsp\load.lsp clos\load.lsp cmp\load.lsp cmp\cmpdefs.lsp \
		    ecl.lib ecl.dll ecl_min$(EXE) eclmin.lib help.doc sysfun.lsp \
		    BUILD-STAMP $(TARGETS) *.exp *.ilk *.manifest *.pdb *.c *.obj \
		    ecl-config.bat ecl-static.lib $(UCDDAT)) \
	     do $(RM) %i
	cd c
	-$(MAKE) ECL_THREADS=$(ECL_THREADS) clean
	cd ..
clean_lisp:
	-for %i in (lsp clos $(ECL_MODULES)) do for %k in (%i.lib %i.fas %i.ilk %i.c %i.obj %i.pdb) do $(RM) %k
	-for %i in (lsp clos ext $(ECL_MODULES)) do $(RMDIR) %i
	-$(RM) help.doc
	-$(RM) MODULES
distclean: clean
realclean: distclean
test1:
	cd c; $(MAKE)
	$(MAKE) ecl_min
	$(MAKE) ecl
	cd tests; $(MAKE)
	diff tests tests2
test2:
	$(MAKE) clean_lisp
	cd c; $(MAKE)
	$(MAKE) ecl_min
	$(RM) ecl
	$(MAKE) ecl
	for i in lsp clos cmp; do diff --exclude=\*.o $$i old/$$i; done
test3:
	-mkdir stage2
	cp -rf lsp clos cmp stage2
	-for i in lsp cmp clos clx tk; do test -f lib$$i.a && mv lib$$i.a stage2; done
	$(MAKE) clean_lisp
	./ecl < compile.lsp
	-for i in lsp clos cmp clx tk; do test -d $$i && diff --exclude=\*.o $$i stage2/$$i; done | less
test:
	$(MAKE) -C tests
	$(MAKE) -C ansi-tests > ansi-tests/log
#	-(diff tests ~/src/tests; diff --exclude log ansi-tests ~/src/ansi-tests) | less
