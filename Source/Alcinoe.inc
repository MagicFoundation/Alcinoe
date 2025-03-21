//https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Compiler_Versions
{$IF (CompilerVersion <= 36.0) and (not declared(RTLVersion124))} // Delphi Athens 12.3
  {$DEFINE ALCompilerVersionSupported123}
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

{$IF Defined(SKIA)}
  {$DEFINE ALSkiaEngine}
  {$DEFINE ALSkiaCanvas}
{$ELSEIF Defined(ALDPK)}
  {$DEFINE ALSkiaEngine}
{$ELSEIF Defined(ANDROID) or defined(IOS)}
  {$DEFINE ALGpuCanvas}
{$ENDIF}

{$IF Defined(ALSkiaEngine)}
  {$DEFINE ALSkiaAvailable}
{$ENDIF}

{$IF defined(MACOS) and not defined(IOS)}
  {$DEFINE ALMacOS} 
{$ENDIF}

{$IF defined(ALMacOS) or defined(IOS)}
  {$DEFINE ALAppleOS} 
{$ENDIF}

{$IF not defined(MSWINDOWS)}
  {$DEFINE ALHttpGzipAuto}
{$ENDIF}

{$SCOPEDENUMS ON}

{$IF Defined(ALDPK)}
  {$DEFINE ALBackwardCompatible}
{$ENDIF}

{.$Define ALUIAutomationEnabled}
