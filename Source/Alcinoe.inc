//https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Compiler_Versions
{$IF (CompilerVersion <= 35.0) and (not declared(RTLVersion114))} // This mean <= Delphi Alexandria 11.3
  //When updating this block then first create the define ALCompilerVersionSupported_TMP that
  //is set for the current compiler version, then progressively replace in all source
  //files ALCompilerVersionSupported by ALCompilerVersionSupported_TMP and when everything done
  //delete ALCompilerVersionSupported_TMP from here and replace in all files ALCompilerVersionSupported_TMP
  //by ALCompilerVersionSupported. This must be done for all the MagicFoundation projects, not only
  //for the Alcinoe project
  {$DEFINE ALCompilerVersionSupported}
{$ENDIF}

{$IF defined(CPUX86) or defined(CPUX64)} // CPU is an Intel 386 or later on any platform.
  {$DEFINE ALCPUX} // The CPU is in an Intel 386 or supports the x86-64 instruction set on any platform.
{$ENDIF}

{$IF defined(ALCPUX) and defined(MSWINDOWS)}
  {$DEFINE ALCPUXASM} // The CPU is in an Intel 386 or supports the x86-64 instruction set on any platform AND the compiler support the inline Assembly Code (ASM)
{$ENDIF}

{$IF Low(string) = 0}
  {$DEFINE ALZeroBasedStringsON}
{$ENDIF}

{$IFOPT R+}
  {$DEFINE ALRangeCheckingON}
{$ENDIF}

{$IFOPT Q+}
  {$DEFINE ALOverflowCheckingON}
{$ENDIF}

//http://docwiki.embarcadero.com/Libraries/Sydney/en/System.Extended
{$IF SizeOf(Extended) >= 10} // 10,12,16
  {$DEFINE  EXTENDEDHAS10BYTES} // Used in original delphi source code
{$ENDIF}
{$IF SizeOf(Extended) = 16}
  {$DEFINE EXTENDEDIS16BYTES} // Used in original delphi source code
{$ELSEIF SizeOf(Extended) = 12}
  {$DEFINE EXTENDEDIS12BYTES} // Used in original delphi source code
{$ELSEIF SizeOf(Extended) = 10}
  {$DEFINE EXTENDEDIS10BYTES} // Used in original delphi source code
{$ELSEIF SizeOf(Extended) = 8}
  {$DEFINE EXTENDEDIS8BYTES} // Used in original delphi source code
{$ELSE}
  {$MESSAGE ERROR 'Unknown Extended type'}
{$ENDIF}

{$IF Defined(ANDROID) or defined(IOS)}
  {$DEFINE ALUseTexture}
{$ENDIF}

{$IF defined(MACOS) and not defined(IOS)}
  {$DEFINE ALMacOS} 
{$ENDIF}

{$IF not defined(MSWINDOWS)}
  {$DEFINE ALHttpGzipAuto}
{$ENDIF}

{$SCOPEDENUMS ON}