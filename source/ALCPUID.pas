{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/
Author(s):    The Original Code is the FastCode CPUID code (version 3.0.3)

              The Initial Developer of the Original Code is
              Roelof Engelbrecht <roelof@cox-internet.com>. Portions
              created by the Initial Developer are Copyright (C)
              2004 by the Initial Developer. All Rights Reserved.

              Contributor(s):
              Dennis Passmore <Dennis_Passmore@ ultimatesoftware.com>,
              Dennis Christensen <marianndkc@home3.gvdnet.dk>,
              Jouni Turunen <jouni.turunen@NOSPAM.xenex.fi>.

Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALCPUID
Version:      3.50

Description:  A list of function to detect CPU type

Legal issues: Copyright (C) 1999-2009 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History :     01/04/2007: Update the function from FastCode CPUID code
                          (version 3.0.3)
Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALCPUID;

{$IFDEF VER170}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 18.0}
    {$WARN UNSAFE_CAST OFF}
  {$IFEND}
{$ENDIF}

{.$DEFINE ALCPUTargetSizePenalties}
{.$DEFINE ALCPUTargetPascal}
{.$DEFINE ALCPUTargetPascalSizePenalty}

interface

type
  TALCPUVendor = (
                  cvUnknown,cvAMD, cvCentaur, cvCyrix, cvIntel,
                  cvTransmeta, cvNexGen, cvRise, cvUMC, cvNSC, cvSiS
                 );
  {Note: when changing TALCPUVendor, also change cALVendorStr array below}

  TALCPUInstructions =(
                       isFPU, {80x87}
                       isTSC, {RDTSC}
                       isCX8, {CMPXCHG8B}
                       isSEP, {SYSENTER/SYSEXIT}
                       isCMOV, {CMOVcc, and if isFPU, FCMOVcc/FCOMI}
                       isMMX, {MMX}
                       isFXSR, {FXSAVE/FXRSTOR}
                       isSSE, {SSE}
                       isSSE2, {SSE2}
                       isSSE3, {SSE3*}
                       isMONITOR, {MONITOR/MWAIT*}
                       isCX16, {CMPXCHG16B*}
                       isX64, {AMD AMD64* or Intel EM64T*}
                       isExMMX, {MMX+ - AMD only}
                       isEx3DNow, {3DNow!+ - AMD only}
                       is3DNow {3DNow! - AMD only}
                      );
  {Note: when changing TALCPUInstruction, also change cALCPUInstructionSupportStr below
         * - instruction(s) not supported in Delphi 7 assembler}

  TALCPUInstructionSupport = set of TALCPUInstructions;

  TALCPUInfo = record
    Vendor: TALCPUVendor;
    Signature: Cardinal;
    EffFamily: Byte; {ExtendedFamily + Family}
    EffModel: Byte; {(ExtendedModel shl 4) + Model}
    EffModelBasic: Byte; {Just Model (not ExtendedModel shl 4) + Model)}
    CodeL1CacheSize, {KB or micro-ops for Pentium 4}
      DataL1CacheSize, {KB}
      L2CacheSize, {KB}
      L3CacheSize: Word; {KB}
    InstructionSupport: TALCPUInstructionSupport;
  end;

  TALCPUTarget = (
                  fctIA32,            {not specific to any CPU}
                  fctIA32SizePenalty, {not specific to any CPU, In library routines with size penalties used This target was called "fctRTLReplacement" earlier}
                  fctMMX,             {not specific to any CPU, requires FPU, MMX and CMOV  "Old fctBlended target"}
                  fctSSE,             {not specific to any CPU, requires FPU, MMX, CMOV and SSE}
                  fctSSESizePenalty,  {not specific to any CPU, requires FPU, MMX, CMOV and SSE. In library routines with size penalties used}
                  fctSSE2,            {not specific to any CPU, requires FPU, MMX, CMOV, SSE, SSE2}
                  fctPascal,           {use Pascal routines in library}
                  fctPascalSizePenalty,{use Pascal routines with size penalty in library}
                  fctPMD, {Dothan}
                  fctPMY, {Yonah}
                  fctP4N, {Northwood}
                  fctP4R, {Presler}
                  fctAmd64, {AMD 64}
                  fctAmd64_SSE3 {X2/Opteron/Athlon FX/Athlon 64 with SSE3}
                 );
  {Note: when changing TALCPUTarget, also change cALCPUTargetStr array below}


const
  cALCPUVendorStr: array[Low(TALCPUVendor)..High(TALCPUVendor)] of ShortString =
  ('Unknown', 'AMD', 'Centaur (VIA)', 'Cyrix', 'Intel', 'Transmeta',
    'NexGen', 'Rise', 'UMC', 'National Semiconductor', 'SiS');

  cALCPUInstructionSupportStr: array[Low(TALCPUInstructions)..High(TALCPUInstructions)] of ShortString =
    ('FPU', 'TSC', 'CX8', 'SEP', 'CMOV', 'MMX', 'FXSR', 'SSE', 'SSE2', 'SSE3',
    'MONITOR', 'CX16', 'X64', 'MMX+', '3DNow!+', '3DNow!');

  cALCPUTargetStr: array[Low(TALCPUTarget)..High(TALCPUTarget)] of ShortString =
    ('IA32', 'IA32_SizePenalty', 'MMX', 'SSE', 'SSE_SizePenalty', 'SSE2', 'Pascal', 'Pascal_SizePenalty',
    'Dothan', 'Yonah', 'Northwood', 'Presler', 'AMD_64', 'AMD_64X2');


{--------------------------------}
Function ALGetCPUInfo: TALCPUinfo;
Function ALGetCPUTarget: TALCPUTarget;

implementation

var
  vALCPUInfo: TALCPUInfo;
  vALCPUTarget: TALCPUTarget;

type
  TALCPURegisters = record
    EAX,
      EBX,
      ECX,
      EDX: Cardinal;
  end;

  TALCPUVendorStr = string[12];

  TALCpuFeatures =
    ({in EDX}
    cfFPU, cfVME, cfDE, cfPSE, cfTSC, cfMSR, cfPAE, cfMCE,
    cfCX8, cfAPIC, cf_d10, cfSEP, cfMTRR, cfPGE, cfMCA, cfCMOV,
    cfPAT, cfPSE36, cfPSN, cfCLFSH, cf_d20, cfDS, cfACPI, cfMMX,
    cfFXSR, cfSSE, cfSSE2, cfSS, cfHTT, cfTM, cfIA_64, cfPBE,
    {in ECX}
    cfSSE3, cf_c1, cf_c2, cfMON, cfDS_CPL, cf_c5, cf_c6, cfEIST,
    cfTM2, cf_c9, cfCID, cf_c11, cf_c12, cfCX16, cfxTPR, cf_c15,
    cf_c16, cf_c17, cf_c18, cf_c19, cf_c20, cf_c21, cf_c22, cf_c23,
    cf_c24, cf_c25, cf_c26, cf_c27, cf_c28, cf_c29, cf_c30, cf_c31);
  TALCpuFeatureSet = set of TALCpuFeatures;

  TALCpuExtendedFeatures =
    (cefFPU, cefVME, cefDE, cefPSE, cefTSC, cefMSR, cefPAE, cefMCE,
    cefCX8, cefAPIC, cef_10, cefSEP, cefMTRR, cefPGE, cefMCA, cefCMOV,
    cefPAT, cefPSE36, cef_18, ceMPC, ceNX, cef_21, cefExMMX, cefMMX,
    cefFXSR, cef_25, cef_26, cef_27, cef_28, cefLM, cefEx3DNow, cef3DNow);
  TALCpuExtendedFeatureSet = set of TALCpuExtendedFeatures;

const
  cALCPUVendorIDString: array[Low(TALCpuVendor)..High(TALCpuVendor)] of TALCpuVendorStr =
  ('',
    'AuthenticAMD', 'CentaurHauls', 'CyrixInstead', 'GenuineIntel',
    'GenuineTMx86', 'NexGenDriven', 'RiseRiseRise', 'UMC UMC UMC ',
    'Geode by NSC', 'SiS SiS SiS');

  {CPU signatures}

  cALCPUIntelLowestSEPSupportSignature = $633;
  cALCPUK7DuronA0Signature = $630;
  cALCPUC3Samuel2EffModel = 7;
  cALCPUC3EzraEffModel = 8;
  cALCPUPMBaniasEffModel = 9;
  cALCPUPMDothanEffModel = $D;
  cALCPUPMYonahEffModel = $E;
  cALCPUP3LowestEffModel = 7;

{********************************}
Function ALGetCPUInfo: TALCPUinfo;
Begin
  Result := VALCPUInfo;
end;

{************************************}
Function ALGetCPUTarget: TALCPUTarget;
Begin
  Result := VALCPUTarget;
end;

{**********************************************}
function ALIsCPUID_Available: Boolean; register;
asm
  PUSHFD                 {save EFLAGS to stack}
  POP     EAX            {store EFLAGS in EAX}
  MOV     EDX, EAX       {save in EDX for later testing}
  XOR     EAX, $200000;  {flip ID bit in EFLAGS}
  PUSH    EAX            {save new EFLAGS value on stack}
  POPFD                  {replace current EFLAGS value}
  PUSHFD                 {get new EFLAGS}
  POP     EAX            {store new EFLAGS in EAX}
  XOR     EAX, EDX       {check if ID bit changed}
  JZ      @exit          {no, CPUID not available}
  MOV     EAX, True      {yes, CPUID is available}
@exit:
end;

{**********************************}
function ALIsFPU_Available: Boolean;
var
  _FCW, _FSW: Word;
asm
  MOV     EAX, False     {initialize return register}
  MOV     _FSW, $5A5A    {store a non-zero value}
  FNINIT                 {must use non-wait form}
  FNSTSW  _FSW           {store the status}
  CMP     _FSW, 0        {was the correct status read?}
  JNE     @exit          {no, FPU not available}
  FNSTCW  _FCW           {yes, now save control word}
  MOV     DX, _FCW       {get the control word}
  AND     DX, $103F      {mask the proper status bits}
  CMP     DX, $3F        {is a numeric processor installed?}
  JNE     @exit          {no, FPU not installed}
  MOV     EAX, True      {yes, FPU is installed}
@exit:
end;

{********************************************************************}
procedure ALGetCPUID(Param: Cardinal; var Registers: TALCPURegisters);
asm
  PUSH    EBX                         {save affected registers}
  PUSH    EDI
  MOV     EDI, Registers
  XOR     EBX, EBX                    {clear EBX register}
  XOR     ECX, ECX                    {clear ECX register}
  XOR     EDX, EDX                    {clear EDX register}
  DB $0F, $A2                         {CPUID opcode}
  MOV     TALCPURegisters(EDI).&EAX, EAX   {save EAX register}
  MOV     TALCPURegisters(EDI).&EBX, EBX   {save EBX register}
  MOV     TALCPURegisters(EDI).&ECX, ECX   {save ECX register}
  MOV     TALCPURegisters(EDI).&EDX, EDX   {save EDX register}
  POP     EDI                         {restore registers}
  POP     EBX
end;

{***********************}
procedure ALGetCPUVendor;
var
  VendorStr: TALCPUVendorStr;
  Registers: TALCPURegisters;
begin
  {call CPUID function 0}
  ALGetCPUID(0, Registers);

  {get vendor string}
  SetLength(VendorStr, 12);
  Move(Registers.EBX, VendorStr[1], 4);
  Move(Registers.EDX, VendorStr[5], 4);
  Move(Registers.ECX, VendorStr[9], 4);

  {get CPU vendor from vendor string}
  vALCPUInfo.Vendor := High(TALCPUVendor);
  while (VendorStr <> cALCPUVendorIDString[vALCPUInfo.Vendor]) and
    (vALCPUInfo.Vendor > Low(TALCPUVendor)) do
    Dec(vALCPUInfo.Vendor);
end;

{*************************}
procedure ALGetCPUFeatures;
{preconditions: 1. maximum CPUID must be at least $00000001
                2. GetCPUVendor must have been called}
type
  _Int64 = packed record
    Lo: Longword;
    Hi: Longword;
  end;
var
  Registers: TALCPURegisters;
  CpuFeatures: TALCpuFeatureSet;
begin
  {call CPUID function $00000001}
  ALGetCPUID($00000001, Registers);

  {get CPU signature}
  vALCPUInfo.Signature := Registers.EAX;

  {extract effective processor family and model}
  vALCPUInfo.EffFamily := vALCPUInfo.Signature and $00000F00 shr 8;
  vALCPUInfo.EffModel := vALCPUInfo.Signature and $000000F0 shr 4;
  vALCPUInfo.EffModelBasic := vALCPUInfo.EffModel;
  if vALCPUInfo.EffFamily = $F then
  begin
    vALCPUInfo.EffFamily := vALCPUInfo.EffFamily + (vALCPUInfo.Signature and $0FF00000 shr 20);
    vALCPUInfo.EffModel := vALCPUInfo.EffModel + (vALCPUInfo.Signature and $000F0000 shr 12);
  end;

  {get CPU features}
  Move(Registers.EDX, _Int64(CpuFeatures).Lo, 4);
  Move(Registers.ECX, _Int64(CpuFeatures).Hi, 4);

  {get instruction support}
  if cfFPU in CpuFeatures then
    Include(vALCPUInfo.InstructionSupport, isFPU);
  if cfTSC in CpuFeatures then
    Include(vALCPUInfo.InstructionSupport, isTSC);
  if cfCX8 in CpuFeatures then
    Include(vALCPUInfo.InstructionSupport, isCX8);
  if cfSEP in CpuFeatures then
  begin
    Include(vALCPUInfo.InstructionSupport, isSEP);
    {for Intel CPUs, qualify the processor family and model to ensure that the
     SYSENTER/SYSEXIT instructions are actually present - see Intel Application
     Note AP-485}
    if (vALCPUInfo.Vendor = cvIntel) and
      (vALCPUInfo.Signature and $0FFF3FFF < cALCPUIntelLowestSEPSupportSignature) then
      Exclude(vALCPUInfo.InstructionSupport, isSEP);
  end;
  if cfCMOV in CpuFeatures then
    Include(vALCPUInfo.InstructionSupport, isCMOV);
  if cfFXSR in CpuFeatures then
    Include(vALCPUInfo.InstructionSupport, isFXSR);
  if cfMMX in CpuFeatures then
    Include(vALCPUInfo.InstructionSupport, isMMX);
  if cfSSE in CpuFeatures then
    Include(vALCPUInfo.InstructionSupport, isSSE);
  if cfSSE2 in CpuFeatures then
    Include(vALCPUInfo.InstructionSupport, isSSE2);
  if cfSSE3 in CpuFeatures then
    Include(vALCPUInfo.InstructionSupport, isSSE3);
  if (vALCPUInfo.Vendor = cvIntel) and (cfMON in CpuFeatures) then
    Include(vALCPUInfo.InstructionSupport, isMONITOR);
  if cfCX16 in CpuFeatures then
    Include(vALCPUInfo.InstructionSupport, isCX16);
end;

{*********************************}
procedure ALGetCPUExtendedFeatures;
{preconditions: maximum extended CPUID >= $80000001}
var
  Registers: TALCPURegisters;
  CpuExFeatures: TALCpuExtendedFeatureSet;
begin
  {call CPUID function $80000001}
  ALGetCPUID($80000001, Registers);

  {get CPU extended features}
  CPUExFeatures := TALCPUExtendedFeatureSet(Registers.EDX);

  {get instruction support}
  if cefLM in CpuExFeatures then
    Include(vALCPUInfo.InstructionSupport, isX64);
  if cefExMMX in CpuExFeatures then
    Include(vALCPUInfo.InstructionSupport, isExMMX);
  if cefEx3DNow in CpuExFeatures then
    Include(vALCPUInfo.InstructionSupport, isEx3DNow);
  if cef3DNow in CpuExFeatures then
    Include(vALCPUInfo.InstructionSupport, is3DNow);
end;

{********************************}
procedure ALGetProcessorCacheInfo;
{preconditions: 1. maximum CPUID must be at least $00000002
                2. GetCPUVendor must have been called}
type
  TConfigDescriptor = packed array[0..15] of Byte;
var
  Registers: TALCPURegisters;
  i, j: Integer;
  QueryCount: Byte;
begin
  {call CPUID function 2}
  ALGetCPUID($00000002, Registers);
  QueryCount := Registers.EAX and $FF;
  for i := 1 to QueryCount do
  begin
    for j := 1 to 15 do
      with vALCPUInfo do
        {decode configuration descriptor byte}
        case TConfigDescriptor(Registers)[j] of
          $06: CodeL1CacheSize := 8;
          $08: CodeL1CacheSize := 16;
          $0A: DataL1CacheSize := 8;
          $0C: DataL1CacheSize := 16;
          $22: L3CacheSize := 512;
          $23: L3CacheSize := 1024;
          $25: L3CacheSize := 2048;
          $29: L3CacheSize := 4096;
          $2C: DataL1CacheSize := 32;
          $30: CodeL1CacheSize := 32;
          $39: L2CacheSize := 128;
          $3B: L2CacheSize := 128;
          $3C: L2CacheSize := 256;
          $40: {no 2nd-level cache or, if processor contains a valid 2nd-level
                cache, no 3rd-level cache}
            if L2CacheSize <> 0 then
              L3CacheSize := 0;
          $41: L2CacheSize := 128;
          $42: L2CacheSize := 256;
          $43: L2CacheSize := 512;
          $44: L2CacheSize := 1024;
          $45: L2CacheSize := 2048;
          $60: DataL1CacheSize := 16;
          $66: DataL1CacheSize := 8;
          $67: DataL1CacheSize := 16;
          $68: DataL1CacheSize := 32;
          $70: if not (vALCPUInfo.Vendor in [cvCyrix, cvNSC]) then
              CodeL1CacheSize := 12; {K micro-ops}
          $71: CodeL1CacheSize := 16; {K micro-ops}
          $72: CodeL1CacheSize := 32; {K micro-ops}
          $78: L2CacheSize := 1024;
          $79: L2CacheSize := 128;
          $7A: L2CacheSize := 256;
          $7B: L2CacheSize := 512;
          $7C: L2CacheSize := 1024;
          $7D: L2CacheSize := 2048;
          $7F: L2CacheSize := 512;
          $80: if vALCPUInfo.Vendor in [cvCyrix, cvNSC] then
            begin {Cyrix and NSC only - 16 KB unified L1 cache}
              CodeL1CacheSize := 8;
              DataL1CacheSize := 8;
            end;
          $82: L2CacheSize := 256;
          $83: L2CacheSize := 512;
          $84: L2CacheSize := 1024;
          $85: L2CacheSize := 2048;
          $86: L2CacheSize := 512;
          $87: L2CacheSize := 1024;
        end;
    if i < QueryCount then
      ALGetCPUID(2, Registers);
  end;
end;

{****************************************}
procedure ALGetExtendedProcessorCacheInfo;
{preconditions: 1. maximum extended CPUID must be at least $80000006
                2. GetCPUVendor and GetCPUFeatures must have been called}
var
  Registers: TALCPURegisters;
begin
  {call CPUID function $80000005}
  ALGetCPUID($80000005, Registers);

  {get L1 cache size}
  {Note: Intel does not support function $80000005 for L1 cache size, so ignore.
         Cyrix returns CPUID function 2 descriptors (already done), so ignore.}
  if not (vALCPUInfo.Vendor in [cvIntel, cvCyrix]) then
  begin
    vALCPUInfo.CodeL1CacheSize := Registers.EDX shr 24;
    vALCPUInfo.DataL1CacheSize := Registers.ECX shr 24;
  end;

  {call CPUID function $80000006}
  ALGetCPUID($80000006, Registers);

  {get L2 cache size}
  if (vALCPUInfo.Vendor = cvAMD) and (vALCPUInfo.Signature and $FFF = cALCPUK7DuronA0Signature) then
    {workaround for AMD Duron Rev A0 L2 cache size erratum - see AMD Technical
     Note TN-13}
    vALCPUInfo.L2CacheSize := 64
  else if (vALCPUInfo.Vendor = cvCentaur) and (vALCPUInfo.EffFamily = 6) and
    (vALCPUInfo.EffModel in [cALCPUC3Samuel2EffModel, cALCPUC3EzraEffModel]) then
    {handle VIA (Centaur) C3 Samuel 2 and Ezra non-standard encoding}
    vALCPUInfo.L2CacheSize := Registers.ECX shr 24
  else {standard encoding}
    vALCPUInfo.L2CacheSize := Registers.ECX shr 16;
end;

{*****************************************}
procedure ALVerifyOSSupportForXMMRegisters;
begin
  {try a SSE instruction that operates on XMM registers}
  try
    asm
      DB $0F, $54, $C0  // ANDPS XMM0, XMM0
    end
  except
    begin
      {if it fails, assume that none of the SSE instruction sets are available}
      Exclude(vALCPUInfo.InstructionSupport, isSSE);
      Exclude(vALCPUInfo.InstructionSupport, isSSE2);
      Exclude(vALCPUInfo.InstructionSupport, isSSE3);
    end;
  end;
end;

{**********************}
procedure ALInitCPUInfo;
var
  Registers: TALCPURegisters;
  MaxCPUID: Cardinal;
  MaxExCPUID: Cardinal;
begin
  {initialize - just to be sure}
  FillChar(vALCPUInfo, SizeOf(vALCPUInfo), 0);

  try
    if not ALIsCPUID_Available then
    begin
      if ALIsFPU_Available then
        Include(vALCPUInfo.InstructionSupport, isFPU);
    end
    else
    begin
      {get maximum CPUID input value}
      ALGetCPUID($00000000, Registers);
      MaxCPUID := Registers.EAX;

      {get CPU vendor - Max CPUID will always be >= 0}
      ALGetCPUVendor;

      {get CPU features if available}
      if MaxCPUID >= $00000001 then
        ALGetCPUFeatures;

      {get cache info if available}
      if MaxCPUID >= $00000002 then
        ALGetProcessorCacheInfo;

      {get maximum extended CPUID input value}
      ALGetCPUID($80000000, Registers);
      MaxExCPUID := Registers.EAX;

      {get CPU extended features if available}
      if MaxExCPUID >= $80000001 then
        ALGetCPUExtendedFeatures;

      {verify operating system support for XMM registers}
      if isSSE in vALCPUInfo.InstructionSupport then
        ALVerifyOSSupportForXMMRegisters;

      {get extended cache features if available}
      {Note: ignore processors that only report L1 cache info,
             i.e. have a MaxExCPUID = $80000005}
      if MaxExCPUID >= $80000006 then
        ALGetExtendedProcessorCacheInfo;
    end;
  except
      {silent exception - should not occur, just ignore}
  end;
end;

{************************}
procedure ALInitCPUTarget;
{precondition: GetCPUInfo must have been called}
begin
 {$IFDEF ALCPUTargetSizePenalties}
   vALCPUTarget := fctIA32SizePenalty;
 {$ELSE}
   vALCPUTarget := fctIA32;
 {$ENDIF}

  if (isSSE2 in vALCPUInfo.InstructionSupport) then
    vALCPUTarget := fctSSE2 else
  if (isSSE in vALCPUInfo.InstructionSupport) then
   {$IFDEF ALCPUTargetSizePenalties}
    vALCPUTarget := fctSSESizePenalty
   {$ELSE}
    vALCPUTarget := fctSSE
   {$ENDIF}
   else
  if ([isFPU, isMMX, isCMOV] <= vALCPUInfo.InstructionSupport) then
    vALCPUTarget := fctMMX;

  case vALCPUInfo.Vendor of
    cvIntel:
      case vALCPUInfo.EffFamily of
        6: {Intel P6, P2, P3, PM}
           case vALCPUInfo.EffModel of
            cALCPUPMDothanEffModel : vALCPUTarget := fctPMD; // Dothan
            cALCPUPMYonahEffModel  : vALCPUTarget := fctPMY; // Yonah
           end;
        $F: {Intel P4}
           case vALCPUInfo.EffModel of
            0,1,2 : vALCPUTarget := fctP4N; // Northwood
            6     : vALCPUTarget := fctP4R; // Presler
           end;
      end;
    cvAMD:
      case vALCPUInfo.EffFamily of
        $F: {AMD K8}
           if ((vALCPUInfo.EffModelBasic=$B) or (vALCPUInfo.EffModelBasic=$3)) and (isSSE3 in vALCPUInfo.InstructionSupport) then
            vALCPUTarget := fctAmd64_SSE3 //AMD X2 dual core CPU
           else
            vALCPUTarget := fctAmd64;
      end;
  end;

 {$IFDEF ALCPUTargetPascal}
   vALCPUTarget := fctPascal;
 {$ENDIF}
 {$IFDEF ALCPUTargetPascalSizePenalty}
   vALCPUTarget := fctPascalSizePenalty;
 {$ENDIF}

end;

initialization
  ALInitCPUInfo;
  ALInitCPUTarget;
end.

