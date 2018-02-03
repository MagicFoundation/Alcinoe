#
# makefile to make pcre .obj files using Microsoft C++ compiler (cl.exe)
#
# if pcre source directory is different from ..\..\..\..\..\..\thirdparty\pcre\pcre-8.31, use
# "make -Dpcresrc=<path to pcre sources>" to tell make where to find the 
# source files
#
# Make.exe needs to reside in the same directory as bcc32.exe.
# For example, if you have Borlands free C++ v. 5.5 compiler (available from
# http://www.borland.com/products/downloads/download_cbuilder.html#) installed:
#
# >C:\Program Files\Borland\BCC55\Bin\make
#
# or, if you want to use C++ Builder 6:
#
# >C:\Program Files\Borland\CBuilder6\Bin\make
#
# or, if you want to use Borland Developer Studio 2006:
#
# >C:\Program files\Borland\BDS\4.0\bin\make

!if !$d(BCB)
BCB = $(MAKEDIR)\..
!endif

BCC = $(BCB)

!if !$d(pcresrc)
pcresrc = ..\..\..\..\..\..\thirdparty\pcre\pcre-8.31
!endif

# ---------------------------------------------------------------------------
OBJ32FILES = .\pcre_compile.obj .\pcre_config.obj .\pcre_dfa_exec.obj \
  .\pcre_exec.obj .\pcre_fullinfo.obj .\pcre_get.obj \
  .\pcre_jit_compile.obj .\pcre_maketables.obj \
  .\pcre_newline.obj .\pcre_ord2utf8.obj .\pcre_refcount.obj .\pcre_study.obj \
  .\pcre_tables.obj .\pcre_ucd.obj \
  .\pcre_valid_utf8.obj .\pcre_version.obj .\pcre_xclass.obj \
  .\pcre_chartables.obj

OBJ64FILES = .\pcre16_compile.obj .\pcre16_config.obj .\pcre16_dfa_exec.obj \
  .\pcre16_exec.obj .\pcre16_fullinfo.obj .\pcre16_get.obj \
  .\pcre16_jit_compile.obj .\pcre16_maketables.obj \
  .\pcre16_newline.obj .\pcre16_ord2utf16.obj .\pcre16_refcount.obj \
  .\pcre16_study.obj .\pcre16_tables.obj .\pcre16_ucd.obj \
  .\pcre16_valid_utf16.obj .\pcre16_version.obj .\pcre16_xclass.obj \
  .\pcre16_chartables.obj .\pcre16_string_utils.obj

OBJFILES = $(OBJ32FILES) $(OBJ64FILES)

# ---------------------------------------------------------------------------
USERDEFINES = SUPPORT_UTF;SUPPORT_UCP;SUPPORT_JIT;SUPPORT_PCRE8;SUPPORT_PCRE16
SYSDEFINES = NO_STRICT;_NO_VCL;_RTLDLL
INCLUDEPATH = $(pcresrc);$(BCC)\include;$(BCB)\include\vcl
LIBPATH = $(BCB)\lib\obj;$(BCB)\lib
PATHC = .;$(pcresrc)
ALLLIB = import32.lib cw32i.lib
INCLUDES = $(pcresrc)\pcre.h $(pcresrc)\config.h
TABLES = $(pcresrc)\pcre_chartables.c
# ---------------------------------------------------------------------------
CFLAG1 = -O2 -Ve -X- -a8 -5 -b -d -k- -vi -tWM- -DHAVE_CONFIG_H

LFLAGS = -D"" -ap -Tpe -x -Gn
# ---------------------------------------------------------------------------
# MAKE SECTION
# ---------------------------------------------------------------------------
# This section of the project file is not used by the BCB IDE.  It is for
# the benefit of building from the command-line using the MAKE utility.
# ---------------------------------------------------------------------------

.autodepend
# ---------------------------------------------------------------------------

!if !$d(BCC32)
BCC32 = bcc32
!endif

!if !$d(LINKER)
LINKER = ilink32
!endif

# ---------------------------------------------------------------------------
!if $d(PATHC)
.PATH.C   = $(PATHC)
!endif

# ---------------------------------------------------------------------------
pcre: $(INCLUDES) $(TABLES) $(OBJFILES)

# ---------------------------------------------------------------------------
.c.obj:
    cl -c -nologo -D_KERNEL32_ -GS- -Z7 -wd4068 -I$(pcresrc) -D$(SYSDEFINES) -DSUPPORT_UTF8 -DSUPPORT_UCP -DSUPPORT_JIT -DHAVE_CONFIG_H -Gs999999 -Fo$@ $<

$(pcresrc)\pcre.h: $(pcresrc)\pcre.h.generic
    copy /Y $? $@

$(pcresrc)\config.h: $(pcresrc)\config.h.generic
    copy /Y $? $@

$(pcresrc)\pcre_chartables.c: $(pcresrc)\pcre_chartables.c.dist
    copy /Y $? $@

# ---------------------------------------------------------------------------




