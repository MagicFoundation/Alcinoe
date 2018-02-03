#
# makefile to make bzip2 .obj files using Microsoft C++ compiler (cl.exe)
#
# if bzip2 source directory is different from ..\..\..\..\..\..\thirdparty\bzip2\bzip2-1.0.6, use
# "make -Dbzip2src=<path to bzip2 sources>" to tell make where to find the 
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
#

!if !$d(bzip2src)
bzip2src = ..\..\..\..\..\..\thirdparty\bzip2\bzip2-1.0.6
!endif

# ---------------------------------------------------------------------------
OBJFILES = .\bzlib.obj .\randtable.obj .\crctable.obj .\compress.obj \
           .\decompress.obj .\huffman.obj .\blocksort.obj
# ---------------------------------------------------------------------------
USERDEFINES = BZ_EXPORT
SYSDEFINES = BZ_NO_STDIO
PATHC = .;$(bzip2src)
# ---------------------------------------------------------------------------
# MAKE SECTION
# ---------------------------------------------------------------------------
# This section of the project file is not used by the BCB IDE.  It is for
# the benefit of building from the command-line using the MAKE utility.
# ---------------------------------------------------------------------------

.autodepend
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
!if $d(PATHC)
.PATH.C   = $(PATHC)
!endif

# ---------------------------------------------------------------------------
bzip2: $(OBJFILES)

# ---------------------------------------------------------------------------
.c.obj:
    cl -c -nologo -D_KERNEL32_ -GS- -Z7 -wd4068 -D$(SYSDEFINES) -D$(USERDEFINES) -Gs999999 -Fo$@ $<
# ---------------------------------------------------------------------------




