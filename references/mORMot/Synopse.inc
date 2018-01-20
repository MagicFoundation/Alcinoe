{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2011 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2018
  the Initial Developer. All Rights Reserved.

  Contributor(s):
   Alfred Glaenzer (alf)
  
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Version 1.7
  - first public release, corresponding to SQLite3 Framework 1.7

  Version 1.8
  - force no Range Checking and other compilation settings

  Version 1.10
  - code modifications to support Delphi 5 / Delphi 6 compilers

  Version 1.11
  - allow to fix Delphi 2009 specific compilation issues :(

  Version 1.13
  - updated conditionals for FPC
  - code modifications for Delphi 5 compiler
  - new WITHLOG conditional (defined by default)

  Version 1.15
  - now handles Delphi XE2 (32 Bit)

  Version 1.16
  - added USEPACKAGES conditional to help compiling the unit within packages
  - added ISDELPHIXE conditional for fixing some compilation warnings
  - added DOPATCHTRTL conditional (not set by default, for compatibility)

  Version 1.18
  - added SQLITE3_FASTCALL conditional (shared by SQLite3 related units)
  - added NEWRTTINOTUSED conditional (unset by default, for compatibility) - see
    https://synopse.info/forum/viewtopic.php?id=1394
  - enhanced FPC compatibility
  - now handles Delphi XE3 and up (32 and 64 bit)

}

{.$define PUREPASCAL}
// define this if your compiler doesn't support Delphi's x86 asm
// - is set automaticaly in case of a 64 bits compiler (only FPC exists now)

{$define USENORMTOUPPER}
// if defined, text functions will use the NormToUpper[] array, as defined
// in our custom SysUtils.pas (not the LVCL version) -> when using LVCL,
// define the global LVCL compiler directive, and this unit will initialize
// its own NormToUpper[] array
// -> define ENHANCEDRTL conditional below if our Enhanced RTL IS installed
// -> in practice, this conditional is ALWAYS DEFINED, since needed by SQLite3

{.$define ENHANCEDRTL}
// define this if you DID install our Enhanced Runtime library or the LVCL:
// - it's better to define this conditional globaly in the Project/Options window
// - we need to hack the "legacy" LoadResString() procedure and add a
//   LoadResStringTranslate() function, for on the fly resourcestring i18n
// - it will also define the TwoDigitLookup[] array and some very fast x86 asm
//   IntToStr() and other functions, available in our Enhanced Runtime library
//   (and our LVCL library)
// - it will be unset automaticaly (see below) for Delphi 2009 and up
// - this conditional must be defined in both SQLite3Commons and SQLite3i18n units,
//   or (even better) globally in the Project options

{.$define USEPACKAGES}
// define this if you compile the unit within a Delphi package
// - it will avoid error like "[DCC Error] E2201 Need imported data reference ($G)
//   to access 'VarCopyProc' from unit 'SynCommons'"
// - shall be set at the package options level, and left untouched by default 

{$define WITHLOG}
// if defined, logging will be supported via the TSQLLog family
// - should be left defined: TSQLog.Family.Level default setting won't log
// anything, so there won't be any noticeable performance penalty to have
// this WITHLOG conditional defined, which is expected by high-level units
// of the framework, like DDD or UI

{.$define DOPATCHTRTL}
// if defined, the low-level patches made to RecordCopy() low-level function
// as defined in SynCommons.pas will be applied (if applicable to your Delphi
// version) - you should better use it, but we have unset it by default

{.$define NEWRTTINOTUSED}
// if defined, the new RTTI (available since Delphi 2010) won't be linked to
// the executable: resulting file size will be much smaller, and mORMot won't
// be affected (unless you use the enhanced RTTI for record/dynamic array JSON
// serialization) - left undefined by default to ensure minimal impact

{.$define NOSETTHREADNAME}
// if defined, SetThreadName() would not raise the exception used to set the
// thread name: to be defined if you have issues when debugging your application

{.$define USELOCKERDEBUG}
// by default, some IAutoLocker instances would use TAutoLocker, unless this
// conditional is defined to use more verbose TAutoLockerDebug
// (may be used for race condition debugging, in multi-threaded apps)

{.$define OLDTEXTWRITERFORMAT}
// force TTextWriter.Add(Format) to handle the alternate deprecated $ % tags

{.$define OPT4AMD}
// you may define this to optimize for AMD CPUs - e.g. to use "set of byte"
// which will use BT[mem] opcodes, which are slow on Intel, but fast on AMD
// (with the Delphi x86 compiler, may not be the case for LLVM or FPC)

{.$define WITH_ERMS}
// you may define this to enable REP MOVSB/STOSB for Fillchar/Move if cfERMS
// flag is available in Intel's CpuFeatures 
// -> disabled by default, since in practice it is much slower for small blocks

{$ifdef LVCL}
   // NormToUpper[] exists only in our enhanced RTL
  {$define OWNNORMTOUPPER}
  // LVCL does not support variants
  {$define NOVARIANTS}
{$endif}

{$ifdef UNICODE}
  {$undef ENHANCEDRTL} // Delphi 2009 and up don't have our Enhanced Runtime library
  {$define HASVARUSTRING}
  {$define HASCODEPAGE}
  {$define FPC_OR_UNICODE}
  { due to a bug in Delphi 2009+, we need to fake inheritance of record,
    since TDynArrayHashed = object(TDynArray) fails to initialize
    http://blog.synopse.info/post/2011/01/29/record-and-object-issue-in-Delphi-2010 }
  {$define UNDIRECTDYNARRAY}
{$endif}

{$define INCLUDE_FTS3}
// define this if you want to include the FTS3/FTS4 feature into the library
// - FTS3 is an SQLite module implementing full-text search
// - will include also FTS4 extension module since 3.7.4
// - see http://www.sqlite.org/fts3.html for documentation
// - is defined by default, but can be unset by defining EXCLUDE_FTS3 conditional
//   to save about 50 KB of code size (is it worth it nowdays?)
// - should be defined for SynSQLite3, SynSQLite3Static and mORMotSQLite3 units,
//   so a global condition in this Synopse.inc does make sense

{$ifdef EXCLUDE_FTS3}
  {$undef INCLUDE_FTS3}
{$endif}

{ Free Pascal adaptation notes:
  - we use the Delphi compatibility mode
  - from system.pp use these constants (Win32/64 values listed):
      LineEnding = #13#10;
      DirectorySeparator = '\';
  - for Cross-Platform and all CPU:
      integer is NOT CPU-dependent (thanks to objpas), i.e. always 32 bits
      cardinal is NOT CPU-dependent (thanks to objpas), i.e. always 32 bits
      PtrUInt is an unsigned integer type of same size as a pointer / register
        -> must be used for pointer arithmetic
        -> may be used in loops
      PtrInt is a signed integer type of same size as a pointer / register
        -> must be used for pointer arithmetic
        -> may be used in loops
      all 32 bits x86 asm code can be replaced by a pascal only version, with
      if the conditional define PUREPASCAL is set (defined below e.g. for CPUX64)
}

{$ifdef FPC}

  {$ifndef FPC_DELPHI}
    {$MODE DELPHI} // e.g. for asm syntax - disabled for FPC 2.6 compatibility
  {$endif}

  {$INLINE ON}
  {$MINENUMSIZE 1}
  {$PACKSET 1}
  {$PACKENUM 1}
  {$CODEPAGE UTF8} // otherwise unexpected behavior occurs in most cases

  {$undef ENHANCEDRTL}    // there is no version of our Enhanced RTL for FPC
  {$undef DOPATCHTRTL}
  {$define DELPHI5ORFPC}
  {$define USETYPEINFO}  // will use SynFPCTypInfo.pas wrapper
  {$define HASINLINE}
  {$define HASUINT64}
  {$define HASINLINENOTX86}
  {$define NODELPHIASM}   // ignore low-level System.@LStrFromPCharLen calls
  {$define HASAESNI}
  {$define HASTTHREADSTART}
  {$define HASINTERFACEASTOBJECT}
  {$define FPC_OR_UNICODE}
  {$define FPC_ENUMHASINNER}
  {$ifdef VER3_1_1} // if FPC_FULLVERSION>30100 ... ifend is not Delphi 5 compatible :(
    {$define FPC_NEWRTTI} // RTTI has been enhanced in latest trunk 
  {$endif}
  {$ifdef FPC_NEWRTTI}
    {$define ISDELPHI2010_OR_FPC_NEWRTTI}
  {$else}
    {$define DELPHI_OR_FPC_OLDRTTI}
    {$define FPC_OLDRTTI}
  {$endif}

  // define FPCSQLITE3STATIC to enable static-linked SQLite3 engine for FPC
  // -> expect *.o files download from https://synopse.info/files/sqlite3fpc.7z
  // -> could be disabled to force external .so/.dll linking
  {$ifdef MSWINDOWS}
  {$ifdef CPUX86}
    {$define FPCSQLITE3STATIC} // we supply Win32 .obj
  {$else}
    {$define FPCSQLITE3STATIC} // we supply Win64 .o (compiled with -O1)
  {$endif}
  {$endif}
  {$ifdef LINUX}
    {$ifdef CPUX86}
      {$define FPCSQLITE3STATIC} // we supply Linux 32-bit x86 .o
    {$endif}
    {$ifdef CPUX64}
      {$define FPCSQLITE3STATIC} // we supply Linux 64-bit x86_64 .o
    {$endif}
    {$ifdef CPUARM}
      {$define FPCSQLITE3STATIC} // we supply Linux 32-bit ARM .o
    {$endif}
  {$endif}

  {$ifdef ANDROID}
    {$define LINUX} // a Linux-based system
  {$endif}

  {$ifdef BSD}
    // this includes Darwin and BSD family like FreeBSD
    {$define LINUX} // not true, but a POSIX/BSD system
    {$define PUREPASCAL} // e.g. low-level stack layout differs
    {$ifdef DARWIN}
      {$define FPCSQLITE3STATIC} // we supply Darwin static libs
    {$else}
      {$define BSDNOTDARWIN}
    {$endif}
  {$else}
    {$ifdef LINUX}
      {$define LINUXNOTBSD} // e.g. to disable epoll API
    {$endif}
  {$endif}

  {$ifdef CPU64}
    {$define PUREPASCAL} // e.g. x64, AARCH64
    {$ifdef CPUX64}
      {$define CPUINTEL}
      {$ASMMODE INTEL} // as Delphi expects
    {$endif CPUX64}
  {$else}
    {$ifdef CPUARM}
      {$define PUREPASCAL} // ARM32
    {$endif CPUARM}
    {$ifdef CPUX86}
      {$define CPUINTEL}
      {$ASMMODE INTEL} // as Delphi expects
    {$endif CPUX86}
  {$endif CPU64}

  // FPC has its own RTTI layout only since late 3.x
  // when http://bugs.freepascal.org/view.php?id=26774 has been fixed
  {$ifdef FPC_NEWRTTI} // use dedicated branch conditional
    {$ifdef CPUINTEL}
      {$define HASINTERFACERTTI}
    {$endif}
    {$ifdef CPUARM}
      {$define HASINTERFACERTTI}
    {$endif}
    {$ifdef CPUAARCH64}
      {$define HASINTERFACERTTI}
    {$endif}
  {$endif FPC_NEWRTTI}

  {$define FPC_OR_PUREPASCAL}
  {$define FPC_OR_KYLIX}
  // exceptions interception code in FPC differs from Delphi
  {$define NOEXCEPTIONINTERCEPT}

  // $if FPC_FULLVERSION>20700 breaks Delphi 6-7 and SynProject :(
  {$ifdef VER2_7}
    {$define ISFPC27}
  {$endif}
  {$ifdef VER3_0}
    {$define ISFPC27}
    {$define ISFPC30}
    {$define HASDIRECTTYPEINFO}
    // PTypeInfo would be stored with no pointer de-reference
    // => Delphi and newer FPC uses a pointer reference to ease exe linking
  {$endif}
  {$ifdef VER3_1}
    {$define ISFPC27}
    {$define ISFPC30}
    {.$define HASDIRECTTYPEINFO}
    // define this for trunk revisions older than June 2016 - see
    // http://wiki.freepascal.org/User_Changes_Trunk#RTTI_Binary_format_change
  {$endif}
  {$ifdef FPC_HAS_CPSTRING}
    // see http://wiki.freepascal.org/FPC_Unicode_support
    {$define HASCODEPAGE} // UNICODE means {$mode delphiunicode}
  {$endif}
  {$ifdef ISFPC27}
    {$define ISFPC271}
    {$define HASVARUSTRING}
    {$define HASVARUSTRARG}
    // defined if the http://mantis.freepascal.org/view.php?id=26773 bug is fixed
    // you should use 2.7.1/trunk branch in revision 28995 from 2014-11-05T22:17:54
    // => this will change the TInvokeableVariantType.SetProperty() signature
    {$define FPC_VARIANTSETVAR}
  {$endif}
  {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
    {$define FPC_ENUMHASINNER}
  {$endif}

  {$ifdef FPC_HAS_MANAGEMENT_OPERATORS}
    {$define ISDELPHI2006ANDUP}
  {$endif FPC_HAS_MANAGEMENT_OPERATORS}

  {$ifdef ISFPC30}
    {$ifndef MSWINDOWS}
    // define FPCUSEVERSIONINFO to link low-level executable file information
    // units in SynCommons.pas
    // => disabled by default, to reduce the executable overhead
    {.$define FPCUSEVERSIONINFO}
    {$endif MSWINDOWS}
  {$endif ISFPC30}


{$else FPC}

  {$define DELPHI_OR_FPC_OLDRTTI}
  {$ifndef PUREPASCAL}
    {$define CPUINTEL} // Delphi only for Intel by now
  {$endif}
  {$ifdef CPUX64}
    {$define CPU64} // Delphi compiler for 64 bit CPU
    {$define CPU64DELPHI}
    {$undef CPU32}
    {$define PUREPASCAL}   // no x86 32 bit asm to be used
  {$else CPUX64}
    {$define CPU32} // Delphi compiler for 32 bit CPU
    {$define CPU32DELPHI}
    {$undef CPU64}
    {$define CPUX86} // for compatibility with older versions of Delphi
  {$endif CPUX64}

  // defines if exceptions shall not be intercepted
  {.$define NOEXCEPTIONINTERCEPT}

  {$IFDEF CONDITIONALEXPRESSIONS}  // Delphi 6 or newer
    {$define HASINTERFACERTTI} // interface RTTI (not FPC)
    {$ifdef LINUX}
      {$if RTLVersion = 14.5}
        {$define KYLIX3}
        {$define FPC_OR_KYLIX}
        // Kylix 3 will be handled just like Delphi 7
        {$undef ENHANCEDRTL}   // Enhanced Runtime library not fully tested yet
        {$define DOPATCHTRTL}  // nice speed up for server apps
        {$define NOVARCOPYPROC}
        {$define NOSQLITE3STATIC} // Kylix will use external sqlite3.so
        {$define LINUXNOTBSD}     // e.g. to disable epoll API
      {$else}
      Kylix1/2 or Delphi Tokyo/ARC are unsupported
      {$ifend}
    {$else}
      {$ifdef VER140}
        {$define ISDELPHI6ANDUP} // Delphi 6 or newer
        {$define DELPHI6OROLDER}
        {$define NOVARCOPYPROC}
        {$undef ENHANCEDRTL} // Delphi 6 doesn't have our Enhanced Runtime library
      {$else}
        {$define ISDELPHI7ANDUP} // Delphi 7 or newer
        {$define WITHUXTHEME}   // VCL handle UI theming
        {$define HASUINT64}
        {$warn UNSAFE_CODE OFF} // Delphi for .Net does not exist any more!
        {$warn UNSAFE_TYPE OFF}
        {$warn UNSAFE_CAST OFF}
        {$warn DUPLICATE_CTOR_DTOR OFF} // avoid W1029 unneeded hints
      {$endif}
      {$ifdef USEPACKAGES}
        {$undef DOPATCHTRTL}
      {$endif}
    {$endif LINUX}
    {$if CompilerVersion >= 17}
      {$define ISDELPHI2005ANDUP} // Delphi 2005 or newer
    {$ifend}
    {$if CompilerVersion >= 18}
      {$define ISDELPHI2006ANDUP} // Delphi 2006 or newer
      {$define HASNEWFILEAGE}
      {$define HASINLINE}
      {$define HASINLINEDELPHI}
      {$define HASINLINENOTX86}
      {$define HASREGION}
      {$define HASFASTMM4}
    {$ifend}
    {$ifdef VER180}
      {$define ISDELPHI20062007} // to circumvent some specific bugs
    {$endif}
    {$ifdef VER185}
      {$define ISDELPHI20062007}
    {$endif}
    {$if CompilerVersion > 18}
      {$define ISDELPHI2007ANDUP} // Delphi 2007 or newer
    {$ifend}
    {$if CompilerVersion = 20}
      {$define ISDELPHI2009} // Delphi 2009 has specific compilation issues :(
      // for Delphi 2009 and up, use UNICODE conditional :)
      {$define FPC_OR_UNICODE}
    {$ifend}
    {$if CompilerVersion >= 21.0}
      // Delphi 2010/XE: Reduce EXE size by disabling much RTTI
      {$define ISDELPHI2010}
      {$define ISDELPHI2010_OR_FPC_NEWRTTI}
      {$define FPC_OR_UNICODE}
      {$define HASTTHREADSTART}
      {$define HASINTERFACEASTOBJECT}
      {$ifdef NEWRTTINOTUSED}
        {$WEAKLINKRTTI ON}
        {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
      {$endif NEWRTTINOTUSED}
    {$ifend}
    {$if CompilerVersion >= 22.0}
      {$define ISDELPHIXE}
    {$ifend}
    {$if CompilerVersion >= 23.0}
      // Delphi XE2 has some cross-platform features
      // e.g. {$ifdef ISDELPHIXE2}VCL.Graphics{$else}Graphics{$endif}
      {$define ISDELPHIXE2}
      {$define HASVARUSTRARG}
      {$define HASTTHREADTERMINATESET} // introduced TThread.TerminateSet
    {$ifend}
    {$if CompilerVersion >= 24.0}
      {$define ISDELPHIXE3}
    {$ifend}
    {$if CompilerVersion >= 25.0}
      {$define ISDELPHIXE4}
      {$define HASAESNI}
    {$ifend}
    {$if CompilerVersion >= 26.0}
      {$define ISDELPHIXE5}
      {$define PUBLISHRECORD}
      // if defined, will handle RTTI available only since Delphi XE5 for
      // record published properties 
    {$ifend}
    {$if CompilerVersion >= 27.0}
      {$define ISDELPHIXE6}
    {$ifend}
    {$if CompilerVersion >= 28.0}
      {$define ISDELPHIXE7}
    {$ifend}
    {$if CompilerVersion >= 29.0}
      {$define ISDELPHIXE8}
    {$ifend}
    {$if CompilerVersion >= 30.0}
      {$define ISDELPHI10}
    {$ifend}
    {$if CompilerVersion >= 31.0}
      {$define ISDELPHI101}
    {$ifend}
    {$if CompilerVersion >= 32.0}
      {$define ISDELPHI102}
      {$ifdef CPUX64}
      {$ifdef VER320withoutAprilUpdate}
        {$undef HASINLINE} // circumvent Delphi 10.2 Tokyo Win64 compiler bug
        {$define HASINLINENOTX86}
      {$endif}
      {$endif}
    {$ifend}
    {$ifopt O-} // if we don't expect fast code, don't optimize the framework
      {$undef ENHANCEDRTL}
      {$undef DOPATCHTRTL}
    {$endif}
  {$ELSE}
    // Delphi 5 or older
    {$define DELPHI6OROLDER}
    {$define DELPHI5OROLDER}
    {$define DELPHI5ORFPC}
    {$define MSWINDOWS}
    {$define NOVARIANTS}
    {$define NOVARCOPYPROC}
    {$undef ENHANCEDRTL} // Delphi 5 doesn't have our Enhanced Runtime library
    {$undef DOPATCHTRTL}
  {$ENDIF}

{$endif FPC}

{$ifdef PUREPASCAL}
  {$define NODELPHIASM}
  {$define FPC_OR_PUREPASCAL}
{$else}
{$endif PUREPASCAL}

{$H+} // we use long strings
{$R-} // disable Range checking in our code
{$S-} // disable Stack checking in our code
{$X+} // expect extended syntax
{$W-} // disable stack frame generation
{$Q-} // disable overflow checking in our code
{$B-} // expect short circuit boolean
{$V-} // disable Var-String Checking
{$T-} // Typed @ operator
{$Z1} // enumerators stored as byte by default
{$IFNDEF FPC}
  {$P+} // Open string params
{$ENDIF FPC}

{$ifdef VER150}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$ifdef CONDITIONALEXPRESSIONS}  // Delphi 6 or newer
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}

// see https://synopse.info/fossil/tktview?name=6593f0fbd1
{$ifndef WITHUXTHEME}
  {$define EXTENDEDTOSTRING_USESTR} // no TFormatSettings before Delphi 6
{$endif}
{$ifdef LVCL}
  {$define EXTENDEDTOSTRING_USESTR} // no FloatToText implemented in LVCL
{$endif}
{$ifdef FPC}
  {$define EXTENDEDTOSTRING_USESTR} // FloatToText uses str() in FPC
{$endif}
{$ifdef CPU64}
  {$define EXTENDEDTOSTRING_USESTR} // FloatToText() much slower in x64 mode
{$endif}

// global conditional to use SQLite3 with fastcall calling convention
// used by SynSQLite3, SynSQLite3Static, SynSQlite3RegEx and mORMotSQLite3 units
{$ifdef CPU64}
  // only one calling convention in the Win64 world
  {$ifndef FPC}
  {$define SQLITE3_FASTCALL}
  {$endif}
{$else}
  {.$define SQLITE3_FASTCALL}
  // undefined by default: BCC32 -pr fastcall (=Delphi resgister) is broken
  // because of issues with BCC32 itself, or some obfuscated calls in source?
  // -> allow to use external SQlite3 libraries in addition to static version
{$endif}

{$ifdef CPUX86} // safest to reset x87 exceptions
{$ifndef PUREPASCAL}
{$ifndef DELPHI5OROLDER}
  {$define RESETFPUEXCEPTION}
{$endif}
{$endif}
{$endif}

{$ifdef FPC}
  {$ifndef FPCSQLITE3STATIC} // see above about this FPC-specific conditional
    {$define NOSQLITE3STATIC}
  {$endif}
{$else}
  // Only Win32+Linux32 do support static linked library yet with Delphi/Kylix
  {$ifdef CPU64}
    {$define NOSQLITE3STATIC}
  {$endif}
{$endif}

{$ifdef NOSQLITE3STATIC}
  // our proprietary crypto expects a patched SQlite3.c statically linked
  {$define NOSQLITE3ENCRYPT}
{$endif}

{.$define SQLVIRTUALLOGS}
// enable low-level logging of SQlite3 virtual table query planner costs
// -> to be used only for internal debugging

{$ifdef MSWINDOWS}
  {$define USEWININET}         // publish TWinINet/TWinHttp/TWinHttpAPI classes
  {.$define ONLYUSEHTTPSOCKET} // for testing (no benefit vs http.sys)
  {.$define USELIBCURL}        // for testing (no benefit vs WinHTTP)
{$else}
  {$define ONLYUSEHTTPSOCKET}  // http.sys server is Windows-specific
  {$ifndef ANDROID}
    // cross-platform libcurl for https -> TCurlHttp and TSQLHttpClientCurl
    {$define USELIBCURL}
    // for Android, consider using https://github.com/gcesarmza/curl-android-ios
    // static libraries and force USELIBCURL in the project conditionals
  {$endif}
{$endif}

// define this to avoid sending "X-Powered-By: Synopse mORMot" HTTP header
{.$define NOXPOWEREDNAME}
