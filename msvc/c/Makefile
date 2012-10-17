#
#			Makefile for ECL core library
#
top_srcdir = ..\..\src
srcdir	= ..\..\src\c

!if "$(ECL_THREADS)" != ""
THREADS_OBJ= threads.obj
THREADS_FLAGS= -DECL_THREADS
!else
THREADS_OBJ=
THREADS_FLAGS=
!endif

# Programs used by "make":
#
TRUE_CC = cl
CC	= cl
CFLAGS	= -c $(ECL_CFLAGS) -DECL_API="__declspec(dllexport)" -I./ -I../ -I$(srcdir) -I$(top_srcdir)/gc/include  -I$(top_srcdir)/gc/include/private
DEFS    = $(THREADS_FLAGS)

SHELL	= /bin/sh
RM	= del
CP      = copy /Y
MV      = move /Y
LINK    = link
EXE	= .exe
DPP	= .\dpp$(EXE)

# Data for installation
#
INSTALL = @INSTALL@
INSTALL_DATA = @INSTALL_DATA@
prefix=@prefix@
exec_prefix=$(prefix)
libdir=$(prefix)\lib\ecl

# Files

HDIR	= $(top_srcdir)\h
HFILES	= ..\ecl\config.h $(HDIR)\ecl.h $(HDIR)\ecl-cmp.h\
	$(HDIR)\object.h $(HDIR)\cs.h $(HDIR)\stacks.h\
	$(HDIR)\external.h $(HDIR)\eval.h\
	$(HDIR)\number.h $(HDIR)\page.h $(HDIR)\unify.h\
	$(HDIR)\internal.h $(HDIR)\ecl-inl.h $(HDIR)\bytecodes.h

OBJS	= main.obj symbol.obj package.obj list.obj\
	apply.obj eval.obj interpreter.obj compiler.obj disassembler.obj \
	instance.obj gfun.obj reference.obj character.obj\
	file.obj read.obj print.obj error.obj string.obj cfun.obj\
	typespec.obj assignment.obj \
	predicate.obj big.obj number.obj\
	num_pred.obj num_comp.obj num_arith.obj num_sfun.obj num_co.obj\
	num_log.obj num_rand.obj array.obj sequence.obj cmpaux.obj\
	macros.obj backq.obj stacks.obj \
	time.obj unixint.obj\
	mapfun.obj multival.obj hash.obj format.obj pathname.obj\
	structure.obj load.obj unixfsys.obj unixsys.obj \
	ffi.obj alloc_2.obj tcp.obj $(THREADS_OBJ) ffi_x86.obj

all:	$(DPP) ..\eclmin.lib ..\cinit.obj

.SUFFIXES: .obj .c .d

{$(srcdir:\=/)}.d{}.c:
	$(DPP) $< $@

{$(srcdir:\=/)/arch}.d{}.c:
	$(DPP) $< $@

#.d.c:
#	$(DPP) $< $@
.c.obj:
	$(CC) $(CFLAGS) /Fo$@ $<

.PHONY:	all

cut.exe: $(top_srcdir)/util/cut.c
	$(CC) $(LDFLAGS) /Fecut.exe $(top_srcdir)/util/cut.c

..\eclmin.lib: $(OBJS:.obj=.c) all_symbols.c $(OBJS) all_symbols.obj all_symbols2.obj
	-$(RM) $@
	$(LINK) -lib -out:$@ $(OBJS)

clean:
	-for %f in ($(OBJS:.obj=.c) all_symbols.c) do $(RM) %f
	-for %f in ($(OBJS) all_symbols.obj all_symbols2.obj) do $(RM) %f
	-$(RM) *.pdb
	-for %f in (..\ecl\config.h dpp dpp.obj $(DPP) cut.exe cut.obj \
	            ..\eclmin.lib ..\cinit.obj cinit.c symbols_list2.h) \
	     do $(RM) %f
	-for %f in (..\ecl\*.h) do $(RM) %f
	-for %f in (cut.exe cut.obj dpp.exe dpp.obj) do $(RM) %f

# Build rules

$(DPP): $(srcdir)/dpp.c $(srcdir)/symbols_list2.h ../ecl/config.h
	$(TRUE_CC) -I.. -I./ $(DEFS) $(srcdir)/dpp.c  -o $@
../ecl/config.h: ../ecl/config.h.msvc6 Makefile
	for %i in ($(HFILES)) do $(CP) %i ..\ecl
	cut.exe "@ECL_FPE_CODE@" "$(srcdir:\=/)/arch/fpe_x86.c" \
		"@ECL_VERSION_NUMBER@" "$(ECL_VERSION_NUMBER)" \
	        < ..\ecl\config.h.msvc6 > $@

#
# GCC might break this code
#
gbc.o: gbc.c $(HFILES)
	$(CC) $(CFLAGS) -O0 gbc.c /Fo$@
#
# This reduces the overhead of jumping to other functions
#
apply.o: apply.c $(HFILES) $(HDIR)/cs.h
	$(CC) $(CFLAGS) apply.c /Fo$@
#
# These files are interrelated
#
all_symbols.obj: all_symbols.c
	$(CC) $(CFLAGS) -Fo$@ -I../ -I./ all_symbols.c
all_symbols2.obj: all_symbols.c
	$(CC) $(CFLAGS) -DECL_FINAL -Fo$@ -I../ -I./ all_symbols.c
#
# This is in another directory
#
../cinit.obj: cinit.c
	$(CC) $(CFLAGS) -I../ -I./ /Focinit.obj cinit.c
	$(MV) cinit.obj ..\
