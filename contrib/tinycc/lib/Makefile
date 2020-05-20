#
# Tiny C Compiler Makefile for libmkcc1.a
#

TOP = ..
include $(TOP)/Makefile
VPATH = $(top_srcdir)/lib $(top_srcdir)/win32/lib

ifndef TARGET # native library
 ifdef CONFIG_WIN64
  TARGET = x86_64-win
 else
 ifdef CONFIG_WIN32
  TARGET = i386-win
 else
 ifeq ($(ARCH),i386)
  TARGET = i386
 else
 ifeq ($(ARCH),x86-64)
  TARGET = x86_64
 else
 ifeq ($(ARCH),arm)
  TARGET = arm
  XCC = $(CC)
 else
 ifeq ($(ARCH),arm64)
  TARGET = arm64
 else
 endif
 endif
 endif
 endif
 endif
 endif
endif

ifeq ($(ADD_BCHECK),yes)
 BCHECK_O = bcheck.o
else
 BCHECK_O =
endif

DIR = $(TARGET)

native : ../libmkcc1.a
cross : $(DIR)/libmkcc1.a

native : MKCC = $(TOP)/mkcc$(EXESUF)
cross : MKCC = $(TOP)/$(TARGET)-mkcc$(EXESUF)

ifeq ($(ADD_BCHECK),yes)
 I386_O = libmkcc1.o alloca86.o alloca86-bt.o $(BCHECK_O)
 X86_64_O = libmkcc1.o alloca86_64.o alloca86_64-bt.o $(BCHECK_O)
else
 I386_O = libmkcc1.o alloca86.o
 X86_64_O = libmkcc1.o alloca86_64.o
endif
#ARM_O = libmkcc1.o armeabi.o alloca-arm.o
ARM_O = libmkcc1.o alloca-arm.o
WIN_CRT_O = crt1.o wincrt1.o dllcrt1.o dllmain.o chkstk.o
ARM64_O = lib-arm64.o

# build MKCC runtime library to contain PIC code, so it can be linked
# into shared libraries
PICFLAGS = -fPIC

# don't compile with -fstack-protector-strong, MKCC doesn't handle it
# correctly
CFLAGS := $(filter-out -fstack-protector-strong,$(CFLAGS))

ifeq "$(TARGET)" "i386-win"
 OBJ_CRT = $(addprefix $(DIR)/,$(WIN_CRT_O))
 OBJ = $(addprefix $(DIR)/,$(I386_O))
 TGT = -DMKCC_TARGET_I386 -DMKCC_TARGET_PE
 XCC ?= $(CC) -I$(top_srcdir)/include
# XCC ?= $(MKCC) -B$(top_srcdir)/win32 -I$(top_srcdir)/include
# XAR ?= $(DIR)/tiny_libmaker$(EXESUF)
 PICFLAGS =
else
ifeq "$(TARGET)" "x86_64-win"
 OBJ_CRT = $(addprefix $(DIR)/,$(WIN_CRT_O))
 OBJ = $(addprefix $(DIR)/,$(X86_64_O))
 TGT = -DMKCC_TARGET_X86_64 -DMKCC_TARGET_PE
 XCC ?= $(CC) -I$(top_srcdir)/include
# XCC ?= $(MKCC) -B$(top_srcdir)/win32 -I$(top_srcdir)/include
# XAR ?= $(DIR)/tiny_libmaker$(EXESUF)
 PICFLAGS =
else
ifeq "$(TARGET)" "i386"
 OBJ = $(addprefix $(DIR)/,$(I386_O))
 TGT = -DMKCC_TARGET_I386
 XCC = $(CC) -I$(top_srcdir)/include
# XCC ?= $(MKCC) -B$(top_srcdir)
else
ifeq "$(TARGET)" "x86_64"
 OBJ = $(addprefix $(DIR)/,$(X86_64_O))
 TGT = -DMKCC_TARGET_X86_64
 XCC = $(CC) -I$(top_srcdir)/include
# XCC ?= $(MKCC) -B$(top_srcdir)
else
ifeq "$(TARGET)" "arm"
 OBJ = $(addprefix $(DIR)/,$(ARM_O))
 TGT = -DMKCC_TARGET_ARM
 XCC = $(CC) -I$(top_srcdir)/include
# XCC ?= $(MKCC) -B$(top_srcdir)
else
ifeq "$(TARGET)" "arm64"
 OBJ = $(addprefix $(DIR)/,$(ARM64_O))
 TGT = -DMKCC_TARGET_ARM64
 XCC = $(CC) -I$(top_srcdir)/include
# XCC ?= $(MKCC) -B$(top_srcdir)
else
 $(error libmkcc1.a not supported on target '$(TARGET)')
endif
endif
endif
endif
endif
endif

XFLAGS = $(filter-out -b,$(CPPFLAGS) $(CFLAGS) $(PICFLAGS) $(TGT))

ifeq ($(TARGETOS),Darwin)
 XAR = $(DIR)/tiny_libmaker$(EXESUF)
 XFLAGS += -D_ANSI_SOURCE
 BCHECK_O =
endif

XAR ?= $(AR)

$(DIR)/libmkcc1.a ../libmkcc1.a : $(OBJ) $(XAR)
	$(XAR) rcs $@ $(OBJ)
$(DIR)/%.o : %.c
	$(XCC) -c $< -o $@ $(XFLAGS)
$(DIR)/%.o : %.S
	$(XCC) -c $< -o $@ $(XFLAGS)
$(DIR)/%$(EXESUF) : $(TOP)/win32/tools/%.c
	$(CC) -o $@ $< $(XFLAGS) $(LDFLAGS)

$(OBJ) $(XAR) : $(DIR)/exists
$(DIR)/exists :
	mkdir -p $(DIR)
	@echo $@ > $@

clean :
	rm -rfv i386-win x86_64-win i386 x86_64 arm64
