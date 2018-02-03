#
# makefile to make zlib .obj files using Microsoft C++ compiler (cl.exe)
#
# if zlib source directory is different from ..\..\..\..\..\..\thirdparty\zlib\zlib-1.2.7, use
# "make -Dzlibsrc=<path to zlib sources>" to tell make where to find the
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

!if !$d(zlibsrc)
zlibsrc = ..\..\..\..\..\..\thirdparty\zlib\zlib-1.2.7
!endif

# ---------------------------------------------------------------------------
OBJFILES = .\zutil.obj .\compress.obj .\crc32.obj .\deflate.obj \
    .\infback.obj .\inffast.obj .\inflate.obj .\inftrees.obj .\trees.obj \
    .\uncompr.obj .\adler32.obj
# ---------------------------------------------------------------------------
SYSDEFINES = NO_STRICT
INCLUDEPATH = $(zlibsrc);$(BCC)\include;$(BCB)\include\vcl
PATHC = .;$(zlibsrc)
# ---------------------------------------------------------------------------
CFLAG1 = -O2 -Ve -X- -a8 -$(CPU) -b -d -k- -vi -tWM $(CallingConvention)

# ---------------------------------------------------------------------------
# MAKE SECTION
# ---------------------------------------------------------------------------
# This section of the project file is not used by the BCB IDE.  It is for
# the benefit of building from the command-line using the MAKE utility.
# ---------------------------------------------------------------------------

.autodepend
# ---------------------------------------------------------------------------
!if $d(PATHC)
.PATH.C   = $(PATHC)
!endif

# ---------------------------------------------------------------------------
zlib: $(OBJFILES)

# ---------------------------------------------------------------------------
.c.obj:
    cl -c -nologo -D_KERNEL32_ -GS- -Z7 -wd4068 -I$(pcresrc) -D$(SYSDEFINES) -D$(USERDEFINES) -DHAVE_CONFIG_H -Gs999999 -Fo$@ $<
# ---------------------------------------------------------------------------




