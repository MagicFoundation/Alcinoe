#ifndef _MAGICKCORE_MAGICK_BASECONFIG_H
#define _MAGICKCORE_MAGICK_BASECONFIG_H 1
/*
  ImageMagick build feature configuration.  Please note that
  disabling a feature via this header file may not be sufficient to
  remove any library dependencies from the build.  The VisualMagick
  project files may need to be edited to remove libraries the feature
  depends on. When building a static ImageMagick, coder
  registrations are made via magick\static.c so if a format is
  removed, the call to its registration function should be commented
  out in static.c.  Note that VisualMagick configure updates
  magick\static.c so re-running configure may cause local changes to
  be lost.

  Note that by default ImageMagick is configured with a
  MAGICKCORE_QUANTUM_DEPTH of 8 and looks for all files in the directory
  where the executable is located.   The installed configuration (i.e. the
  setup.exe-style installer) is  modifying by defining
  "MAGICKCORE_INSTALLED_SUPPORT".  If you would like to install ImageMagick
  using hard-coded paths, or want to use the Windows registry to install
  ImageMagick, then "MAGICKCORE_INSTALLED_SUPPORT" should be defined.

  Enabled options are of the form:

    #define option

  while disabled options are initially in the form

    // #undef option

  so it is necessary to remove the comment, and change "undef" to "define"
  in order for the option to be enabled.
*/

/*
  When building ImageMagick using DLLs, include a DllMain()
  function which automatically invokes MagickCoreGenesis(NULL), and
  MagickCoreTerminus() so that the user doesn't need to. This is disabled
  by default.
*/
//#define ProvideDllMain

/*
  Permit enciphering and deciphering image pixels.
*/
#define MAGICKCORE_CIPHER_SUPPORT

/*
  Define to use the Windows GDI32 library (for clipboard, emf and screenshot modules)
*/
#define MAGICKCORE_WINGDI32_DELEGATE

/*
  Define to build a ImageMagick which uses registry settings or
  hard-coded paths to locate installed components.  This supports
  using the "setup.exe" style installer, or using hard-coded path
  definitions (see below).  If you want to be able to simply copy
  the built ImageMagick to any directory on any directory on any machine,
  then do not use this setting.
*/
#define MAGICKCORE_INSTALLED_SUPPORT

/*
  Specify size of PixelPacket color Quantums (8, 16, or 32).
  A value of 8 uses half the memory than 16 and typically runs 30% faster,
  but provides 256 times less color resolution than a value of 16.
*/
#define MAGICKCORE_QUANTUM_DEPTH 16

/*
  Channel mask depth
*/
#define MAGICKCORE_CHANNEL_MASK_DEPTH 32

/*
  Define to enable high dynamic range imagery (HDRI)
*/
#define MAGICKCORE_HDRI_ENABLE 1

/*
  Define to enable OpenCL
*/
#define MAGICKCORE_HAVE_CL_CL_H

/*
  Define to enable Distributed Pixel Cache
*/
#define MAGICKCORE_DPC_SUPPORT

/*
  Exclude deprecated methods in MagickCore API
*/
#undef MAGICKCORE_EXCLUDE_DEPRECATED

/*
  Define to only use the built-in (in-memory) settings.
*/
#define MAGICKCORE_ZERO_CONFIGURATION_SUPPORT 0

/*
Define to use the bzip2 compression library
*/
#define MAGICKCORE_BZLIB_DELEGATE

/*
Define to use the CAIRO library
*/
#define MAGICKCORE_CAIRO_DELEGATE

/*
Define to use the OpenEXR library
*/
#define MAGICKCORE_OPENEXR_DELEGATE

/*
Define to use the FreeType (TrueType & Postscript font support) library
*/
#define MAGICKCORE_FREETYPE_DELEGATE

/*
Define to use the libheif library
*/
#define MAGICKCORE_HEIC_DELEGATE

/*
Define to use the TurboJPEG library
*/
#define MAGICKCORE_JPEG_DELEGATE

/*
Define to use the jpeg-xl library
*/
#define MAGICKCORE_JXL_DELEGATE

/*
Define to use the "little" Color Management System (LCMS) library
*/
#define MAGICKCORE_LCMS_DELEGATE
#define MAGICKCORE_HAVE_LCMS2_H

/*
Define to use the Liquid Rescale library
*/
#define MAGICKCORE_LQR_DELEGATE

/*
Define to use the lzma compression library
*/
#define MAGICKCORE_LZMA_DELEGATE

/*
Define to use the OpenJPEG library
*/
#define MAGICKCORE_LIBOPENJP2_DELEGATE

/*
Define to use the Pango/Cairo library
*/
#define MAGICKCORE_PANGOCAIRO_DELEGATE

/*
Define to use the PNG library
*/
#define MAGICKCORE_PNG_DELEGATE

/*
Define to use the raqm library
*/
#define MAGICKCORE_RAQM_DELEGATE

/*
Define to use the Raw library
*/
#define MAGICKCORE_RAW_R_DELEGATE

/*
Define to use the RSVG library
*/
#define MAGICKCORE_RSVG_DELEGATE

/*
Define to use the TIFF library
*/
#define MAGICKCORE_TIFF_DELEGATE

/*
Define to use the WebP library
*/
#define MAGICKCORE_WEBP_DELEGATE
#define MAGICKCORE_WEBPMUX_DELEGATE

/*
Define to use the GNOME XML library
*/
#define MAGICKCORE_XML_DELEGATE

/*
Define if you have ZIP library
*/
#define MAGICKCORE_ZIP_DELEGATE

/*
Define to use the zlib ZIP compression library
*/
#define MAGICKCORE_ZLIB_DELEGATE


/*
  Hard Coded Paths

  If hard-coded paths are defined via the the following define
  statements, then they will override any values from the Windows
  registry. It is unusual to use hard-coded paths under Windows.
*/

/*
  Optional: Specify where convert.exe and support executables are installed
*/
//#define MAGICKCORE_EXECUTABLE_PATH "c:\\ImageMagick\\"

/*
  Optional: Specify where operating system specific files are installed
*/
//#define MAGICKCORE_LIBRARY_PATH  "c:\\ImageMagick\\"

/*
  Optional: Specify name of the library that contains the xml resource files
*/
//#define MAGICKCORE_LIBRARY_NAME "MyImageMagick.dll"

/*
  Optional: Specify where operating system independent files are installed
*/
//#define MAGICKCORE_SHARE_PATH  "c:\\ImageMagick\\"

/*
  Optional: Specify where coder modules (DLLs) are installed
*/
//#define MAGICKCORE_CODER_PATH  "c:\\ImageMagick\\"

/*
  Optional: Specify where filter modules (DLLs) are installed
*/
//#define MAGICKCORE_FILTER_PATH  "c:\\ImageMagick\\"

/*
  The remaining defines should not require user modification.
*/

/*
  Define the package name.
*/
#define MAGICKCORE_PACKAGE_NAME  "ImageMagick"

#define _magickcore_inline __inline
#define _magickcore_restrict __restrict

/*
  The 64-bit channel mask requires a C++ compiler
*/
#if MAGICKCORE_CHANNEL_MASK_DEPTH == 64
#  if !defined(__cplusplus) && !defined(c_plusplus)
#    error ImageMagick was build with a 64 channel bit mask and that requires a C++ compiler
#  endif
#endif

/*
  Visual C++ does not define ssize_t by default.
*/
#if !defined(ssize_t) && !defined(__MINGW32__)
#if defined(_WIN64)
typedef __int64 ssize_t;
#else
typedef long ssize_t;
#endif
#endif

#if !defined(__FUNCTION__)
  #define __FUNCTION__  "unknown"
#endif
#define __func__  __FUNCTION__

#define MAGICKCORE_SIZEOF_DOUBLE 8
#define MAGICKCORE_SIZEOF_DOUBLE_T 8
#define MAGICKCORE_SIZEOF_FLOAT 4
#define MAGICKCORE_SIZEOF_FLOAT_T 4

#if defined(_WIN64)
#define MAGICKCORE_SIZEOF_VOID_P 8
#else
#define MAGICKCORE_SIZEOF_VOID_P 4
#endif

#define MAGICKCORE_HAVE_ACOSH 1
#define MAGICKCORE_HAVE_ASINH 1
#define MAGICKCORE_HAVE_ATANH 1
#define MAGICKCORE_HAVE_ATEXIT 1
#define MAGICKCORE_HAVE_ATOLL 1
#define MAGICKCORE_HAVE_DECL_VSNPRINTF 1
#define MAGICKCORE_HAVE_ERF 1
#define MAGICKCORE_HAVE_GETTIMEOFDAY 1
#define MAGICKCORE_HAVE_INTTYPES_H 1
#define MAGICKCORE_HAVE_J0 1
#define MAGICKCORE_HAVE_J1 1
#define MAGICKCORE_HAVE_LOCALE_H 1
#define MAGICKCORE_HAVE_LOCALE_T 1
#define MAGICKCORE_HAVE_PCLOSE 1
#define MAGICKCORE_HAVE_POPEN 1
#define MAGICKCORE_HAVE_POW 1
#define MAGICKCORE_HAVE_PROCESS_H 1
#define MAGICKCORE_HAVE_RAISE 1
#define MAGICKCORE_HAVE_SQRT 1
#define MAGICKCORE_HAVE_STDINT_H 1
#define MAGICKCORE_HAVE_STDIO_H 1
#define MAGICKCORE_HAVE_STRING_H 1
#define MAGICKCORE_HAVE_STRTOD 1
#define MAGICKCORE_HAVE_STRTOD_L 1
#define MAGICKCORE_HAVE_TELLDIR 1
#define MAGICKCORE_HAVE_UINTPTR_T 1
#define MAGICKCORE_HAVE_VFPRINTF 1
#define MAGICKCORE_HAVE_VFPRINTF_L 1
#define MAGICKCORE_HAVE_VSNPRINTF 1
#define MAGICKCORE_HAVE_VSNPRINTF_L 1
#define MAGICKCORE_HAVE__ALIGNED_MALLOC */
#define MAGICKCORE_HAVE__EXIT 1
#define MAGICKCORE_SETJMP_IS_THREAD_SAFE 1
#define MAGICKCORE_STDC_HEADERS 1

/*
  Disable specific warnings.
*/
#if _MSC_VER < 1920
#pragma warning(disable: 4101) /* 'identifier' : unreferenced local variable */
#pragma warning(disable: 4130) /* 'operator' : logical operation on address of string constant */
#pragma warning(disable: 4204) /* nonstandard extension used: non-constant aggregate initializer */
#pragma warning(disable: 4459) /* 'identifier' : declaration of 'foo' hides global declaration */
#endif
#if _MSC_VER < 1930
#pragma warning(disable: 4201) /* nonstandard extension used : nameless struct/union */
#endif
#pragma warning(disable: 4611) /* interaction between '_setjmp' and C++ object destruction is non-portable */
#pragma warning(disable: 6993) /* code analysis ignores OpenMP constructs; analyzing single-threaded code */

#endif
