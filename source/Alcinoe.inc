{$IF CompilerVersion < 29} {Delphi XE8}
  {$IF defined(CPUX64)} // The CPU supports the x86-64 instruction set, and is in a 64-bit environment. *New* in XE2/x64
    {$DEFINE CPU64BITS} // The CPU is in a 64-bit environment, such as DCC64.EXE. *New* in XE8
  {$ENDIF}
  {$IF defined(CPUX86)} // CPU is an Intel 386 or later on any platform. *New* in XE2/x64
    {$DEFINE CPU32BITS} // The CPU is in a 32-bit environment, such as DCC32.EXE. *New* in XE8
  {$ENDIF}
{$ENDIF}

{$IF defined(CPUX64) or defined(CPUX86)} // CPU is an Intel 386 or later on any platform.
  {$DEFINE ALCPUX} // The CPU is in an Intel 386 or supports the x86-64 instruction set on any platform.
{$ENDIF}

{$IF defined(ALCPUX) and defined(MSWINDOWS)}
  {$DEFINE ALCPUXASM} // The CPU is in an Intel 386 or supports the x86-64 instruction set on any platform AND the compiler support the inline Assembly Code
{$ENDIF}

{$IFDEF CPUX86} // CPU is an Intel 386 or later on any platform. *New* in XE2/x64
  {$DEFINE X86ASM} // Duplicate of CPUX86 used in original delphi source code to activate usage of ASM code
{$ELSE}
  {$DEFINE PUREPASCAL} // Used in original delphi source code to deactivate usage of ASM code
{$ENDIF}

{$IF not defined(MSWINDOWS)}
  {$DEFINE ALHideAnsiString} 
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