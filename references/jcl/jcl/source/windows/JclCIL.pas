{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclCIL.pas.                                                                 }
{                                                                                                  }
{ The Initial Developer of the Original Code is Flier Lu (<flier_lu att yahoo dott com dott cn>).  }
{ Portions created by Flier Lu are Copyright (C) Flier Lu. All Rights Reserved.                    }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Flier Lu (flier)                                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Microsoft .Net CIL Instruction Set information support routines and classes.                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclCIL;

interface

{$I jcl.inc}
{$I windowsonly.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF MSWINDOWS}
  System.Classes, System.SysUtils, System.Contnrs,
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils, Contnrs,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase, JclSysUtils, JclMetadata;

type
  TJclOpCode =
   (opNop, opBreak,
    opLdArg_0, opLdArg_1, opLdArg_2, opLdArg_3,
    opLdLoc_0, opLdLoc_1, opLdLoc_2, opLdLoc_3,
    opStLoc_0, opStLoc_1, opStLoc_2, opStLoc_3,
    opldArg_s, opLdArga_s, opStArg_s,
    opLdLoc_s, opLdLoca_s, opStLoc_s,
    opLdNull, opLdc_I4_M1,
    opLdc_I4_0, opLdc_I4_1, opLdc_I4_2, opLdc_I4_3, opLdc_I4_4,
    opLdc_I4_5, opLdc_I4_6, opLdc_I4_7, opLdc_I4_8, opLdc_I4_s,
    opLdc_i4, opLdc_i8, opLdc_r4, opLdc_r8,
    opUnused49,
    opDup, opPop, opJmp, opCall, opCalli, opRet,
    opBr_s, opBrFalse_s, opBrTrue_s,
    opBeq_s, opBge_s, opBgt_s, opBle_s, opBlt_s,
    opBne_un_s, opBge_un_s, opBgt_un_s, opBle_un_s, opBlt_un_s,
    opBr, opBrFalse, opBrTrue,
    opBeq, opBge, opBgt, opBle, opBlt,
    opBne_un, opBge_un, opBgt_un, opBle_un, opBlt_un,
    opSwitch,
    opLdInd_i1, opLdInd_i2, opLdInd_u1, opLdInd_u2,
    opLdInd_i4, opLdInd_u4, opLdInd_i8, opLdInd_i,
    opLdInd_r4, opLdInd_r8, opLdInd_ref, opStInd_ref,
    opStInd_i1, opStInd_i2, opStInd_i4, opStInd_i8,
    opStInd_r4, opStInd_r8,
    opAdd, opSub, opMul, opDiv, opDiv_un, opRem, opRem_un,
    opAnd, opOr, opXor, opShl, opShr, opShr_un, opNeg, opNot,
    opConv_i1, opConv_i2, opConv_i4, opConv_i8,
    opConv_r4, opConv_r8, opConv_u4, opConv_u8,
    opCallVirt, opCpObj, opLdObj, opLdStr, opNewObj,
    opCastClass, opIsInst, opConv_r_un,
    opUnused58, opUnused1,
    opUnbox, opThrow,
    opLdFld, opLdFlda, opStFld, opLdsFld, opLdsFlda, opStsFld, opStObj,
    opConv_ovf_i1_un, opConv_ovf_i2_un, opConv_ovf_i4_un, opConv_ovf_i8_un,
    opConv_ovf_u1_un, opConv_ovf_u2_un, opConv_ovf_u4_un, opConv_ovf_u8_un,
    opConv_ovf_i_un, opConv_ovf_u_un,
    opBox, opNewArr, opLdLen,
    opLdElema, opLdElem_i1, opLdElem_u1, opLdElem_i2, opLdElem_u2,
    opLdElem_i4, opLdElem_u4, opLdElem_i8, opLdElem_i,
    opLdElem_r4, opLdElem_r8, opLdElem_ref,
    opStElem_i, opStElem_i1, opStElem_i2, opStElem_i4, opStElem_i8,
    opStElem_r4, opStElem_r8, opStElem_ref,
    opUnused2, opUnused3, opUnused4, opUnused5,
    opUnused6, opUnused7, opUnused8, opUnused9,
    opUnused10, opUnused11, opUnused12, opUnused13,
    opUnused14, opUnused15, opUnused16, opUnused17,
    opConv_ovf_i1, opConv_ovf_u1, opConv_ovf_i2, opConv_ovf_u2,
    opConv_ovf_i4, opConv_ovf_u4, opConv_ovf_i8, opConv_ovf_u8,
    opUnused50, opUnused18, opUnused19, opUnused20,
    opUnused21, opUnused22, opUnused23,
    opRefAnyVal, opCkFinite,
    opUnused24, opUnused25,
    opMkRefAny,
    opUnused59, opUnused60, opUnused61, opUnused62, opUnused63,
    opUnused64, opUnused65, opUnused66, opUnused67,
    opLdToken,
    opConv_u2, opConv_u1, opConv_i, opConv_ovf_i, opConv_ovf_u,
    opAdd_ovf, opAdd_ovf_un, opMul_ovf, opMul_ovf_un, opSub_ovf, opSub_ovf_un,
    opEndFinally, opLeave, opLeave_s, opStInd_i, opConv_u,
    opUnused26, opUnused27, opUnused28, opUnused29, opUnused30,
    opUnused31, opUnused32, opUnused33, opUnused34, opUnused35,
    opUnused36, opUnused37, opUnused38, opUnused39, opUnused40,
    opUnused41, opUnused42, opUnused43, opUnused44, opUnused45,
    opUnused46, opUnused47, opUnused48,
    opPrefix7, opPrefix6, opPrefix5, opPrefix4,
    opPrefix3, opPrefix2, opPrefix1, opPrefixRef,

    opArgLlist, opCeq, opCgt, opCgt_un, opClt, opClt_un,
    opLdFtn, opLdVirtFtn, optUnused56,
    opLdArg, opLdArga, opStArg, opLdLoc, opLdLoca, opStLoc,
    opLocalLoc, opUnused57, opEndFilter, opUnaligned, opVolatile,
    opTail, opInitObj, opUnused68, opCpBlk, opInitBlk, opUnused69,
    opRethrow, opUnused51, opSizeOf, opRefAnyType,
    opUnused52, opUnused53, opUnused54, opUnused55, opUnused70);

  TJclInstructionDumpILOption =
    (doLineNo, doRawBytes, doIL, doTokenValue, doComment);
  TJclInstructionDumpILOptions = set of TJclInstructionDumpILOption;

  TJclInstructionParamType =
   (ptVoid, ptI1, ptI2, ptI4, ptI8, ptU1, ptU2, ptU4, ptU8, ptR4, ptR8,
    ptToken, ptSOff, ptLOff, ptArray);

const
  InstructionDumpILAllOption =
    [doLineNo, doRawBytes, doIL, doTokenValue, doComment];

type
  TJclClrILGenerator = class;

  TJclInstruction = class(TObject)
  private
    FOpCode: TJclOpCode;
    FOffset: DWORD;
    FParam: Variant;
    FOwner: TJclClrILGenerator;
    function GetWideOpCode: Boolean;
    function GetRealOpCode: Byte;
    function GetName: string;
    function GetFullName: string;
    function GetDescription: string;
    function GetParamType: TJclInstructionParamType;
    function FormatLabel(Offset: Integer): string;
  protected
    function GetSize: DWORD; virtual;
    function DumpILOption(Option: TJclInstructionDumpILOption): string; virtual;
  public
    constructor Create(AOwner: TJclClrILGenerator; AOpCode: TJclOpCode);
    procedure Load(Stream: TStream); virtual;
    procedure Save(Stream: TStream); virtual;
    function DumpIL(Options: TJclInstructionDumpILOptions = [doIL]): string;
    property Owner: TJclClrILGenerator read FOwner;
    property OpCode: TJclOpCode read FOpCode;
    property WideOpCode: Boolean read GetWideOpCode;
    property RealOpCode: Byte read GetRealOpCode;
    property Param: Variant read FParam write FParam;
    property ParamType: TJclInstructionParamType read GetParamType;
    property Name: string read GetName;
    property FullName: string read GetFullName;
    property Description: string read GetDescription;
    property Size: DWORD read GetSize;
    property Offset: DWORD read FOffset;
  end;

  TJclUnaryInstruction = class(TJclInstruction);

  TJclBinaryInstruction = class(TJclInstruction);

  TJclClrILGenerator = class(TObject)
  private
    FMethod: TJclClrMethodBody;
    FInstructions: TObjectList;
    function GetInstructionCount: Integer;
    function GetInstruction(const Idx: Integer): TJclInstruction;
  public
    constructor Create(AMethod: TJclClrMethodBody = nil);
    destructor Destroy; override;
    function DumpIL(Options: TJclInstructionDumpILOptions): string;
    property Method: TJclClrMethodBody read FMethod;
    property Instructions[const Idx: Integer]: TJclInstruction read GetInstruction;
    property InstructionCount: Integer read GetInstructionCount;
  end;

  EJclCliInstructionError = class(EJclError);
  EJclCliInstructionStreamInvalid = class(EJclCliInstructionError);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\windows';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  System.Variants,
  {$ELSE ~HAS_UNITSCOPE}
  Variants,
  {$ENDIF ~HAS_UNITSCOPE}
  JclCLR,
  JclPeImage,
  JclStrings, JclResources;

type
  TJclOpCodeInfoType = (itName, itFullName, itDescription);

const
  STP1 = $FE;

  OpCodeInfos: array [TJclOpCode, TJclOpCodeInfoType] of PResStringRec =
   (
    (@RsCILNamenop,         @RsCILCmdnop,         @RsCILDescrnop),
    (@RsCILNamebreak,       @RsCILCmdbreak,       @RsCILDescrbreak),
    (@RsCILNameldarg0,      @RsCILCmdldarg0,      @RsCILDescrldarg0),
    (@RsCILNameldarg1,      @RsCILCmdldarg1,      @RsCILDescrldarg1),
    (@RsCILNameldarg2,      @RsCILCmdldarg2,      @RsCILDescrldarg2),
    (@RsCILNameldarg3,      @RsCILCmdldarg3,      @RsCILDescrldarg3),
    (@RsCILNameldloc0,      @RsCILCmdldloc0,      @RsCILDescrldloc0),
    (@RsCILNameldloc1,      @RsCILCmdldloc1,      @RsCILDescrldloc1),
    (@RsCILNameldloc2,      @RsCILCmdldloc2,      @RsCILDescrldloc2),
    (@RsCILNameldloc3,      @RsCILCmdldloc3,      @RsCILDescrldloc3),
    (@RsCILNamestloc0,      @RsCILCmdstloc0,      @RsCILDescrstloc0),
    (@RsCILNamestloc1,      @RsCILCmdstloc1,      @RsCILDescrstloc1),
    (@RsCILNamestloc2,      @RsCILCmdstloc2,      @RsCILDescrstloc2),
    (@RsCILNamestloc3,      @RsCILCmdstloc3,      @RsCILDescrstloc3),
    (@RsCILNameldargs,      @RsCILCmdldargs,      @RsCILDescrldargs),
    (@RsCILNameldargas,     @RsCILCmdldargas,     @RsCILDescrldargas),
    (@RsCILNamestargs,      @RsCILCmdstargs,      @RsCILDescrstargs),
    (@RsCILNameldlocs,      @RsCILCmdldlocs,      @RsCILDescrldlocs),
    (@RsCILNameldlocas,     @RsCILCmdldlocas,     @RsCILDescrldlocas),
    (@RsCILNamestlocs,      @RsCILCmdstlocs,      @RsCILDescrstlocs),
    (@RsCILNameldnull,      @RsCILCmdldnull,      @RsCILDescrldnull),
    (@RsCILNameldci4m1,     @RsCILCmdldci4m1,     @RsCILDescrldci4m1),
    (@RsCILNameldci40,      @RsCILCmdldci40,      @RsCILDescrldci40),
    (@RsCILNameldci41,      @RsCILCmdldci41,      @RsCILDescrldci41),
    (@RsCILNameldci42,      @RsCILCmdldci42,      @RsCILDescrldci42),
    (@RsCILNameldci43,      @RsCILCmdldci43,      @RsCILDescrldci43),
    (@RsCILNameldci44,      @RsCILCmdldci44,      @RsCILDescrldci44),
    (@RsCILNameldci45,      @RsCILCmdldci45,      @RsCILDescrldci45),
    (@RsCILNameldci46,      @RsCILCmdldci46,      @RsCILDescrldci46),
    (@RsCILNameldci47,      @RsCILCmdldci47,      @RsCILDescrldci47),
    (@RsCILNameldci48,      @RsCILCmdldci48,      @RsCILDescrldci48),
    (@RsCILNameldci4s,      @RsCILCmdldci4s,      @RsCILDescrldci4s),
    (@RsCILNameldci4,       @RsCILCmdldci4,       @RsCILDescrldci4),
    (@RsCILNameldci8,       @RsCILCmdldci8,       @RsCILDescrldci8),
    (@RsCILNameldcr4,       @RsCILCmdldcr4,       @RsCILDescrldcr4),
    (@RsCILNameldcr8,       @RsCILCmdldcr8,       @RsCILDescrldcr8),
    (@RsCILNameunused1,     @RsCILCmdunused1,     @RsCILDescrunused1),
    (@RsCILNamedup,         @RsCILCmddup,         @RsCILDescrdup),
    (@RsCILNamepop,         @RsCILCmdpop,         @RsCILDescrpop),
    (@RsCILNamejmp,         @RsCILCmdjmp,         @RsCILDescrjmp),
    (@RsCILNamecall,        @RsCILCmdcall,        @RsCILDescrcall),
    (@RsCILNamecalli,       @RsCILCmdcalli,       @RsCILDescrcalli),
    (@RsCILNameret,         @RsCILCmdret,         @RsCILDescrret),
    (@RsCILNamebrs,         @RsCILCmdbrs,         @RsCILDescrbrs),
    (@RsCILNamebrfalses,    @RsCILCmdbrfalses,    @RsCILDescrbrfalses),
    (@RsCILNamebrtrues,     @RsCILCmdbrtrues,     @RsCILDescrbrtrues),
    (@RsCILNamebeqs,        @RsCILCmdbeqs,        @RsCILDescrbeqs),
    (@RsCILNamebges,        @RsCILCmdbges,        @RsCILDescrbges),
    (@RsCILNamebgts,        @RsCILCmdbgts,        @RsCILDescrbgts),
    (@RsCILNamebles,        @RsCILCmdbles,        @RsCILDescrbles),
    (@RsCILNameblts,        @RsCILCmdblts,        @RsCILDescrblts),
    (@RsCILNamebneuns,      @RsCILCmdbneuns,      @RsCILDescrbneuns),
    (@RsCILNamebgeuns,      @RsCILCmdbgeuns,      @RsCILDescrbgeuns),
    (@RsCILNamebgtuns,      @RsCILCmdbgtuns,      @RsCILDescrbgtuns),
    (@RsCILNamebleuns,      @RsCILCmdbleuns,      @RsCILDescrbleuns),
    (@RsCILNamebltuns,      @RsCILCmdbltuns,      @RsCILDescrbltuns),
    (@RsCILNamebr,          @RsCILCmdbr,          @RsCILDescrbr),
    (@RsCILNamebrfalse,     @RsCILCmdbrfalse,     @RsCILDescrbrfalse),
    (@RsCILNamebrtrue,      @RsCILCmdbrtrue,      @RsCILDescrbrtrue),
    (@RsCILNamebeq,         @RsCILCmdbeq,         @RsCILDescrbeq),
    (@RsCILNamebge,         @RsCILCmdbge,         @RsCILDescrbge),
    (@RsCILNamebgt,         @RsCILCmdbgt,         @RsCILDescrbgt),
    (@RsCILNameble,         @RsCILCmdble,         @RsCILDescrble),
    (@RsCILNameblt,         @RsCILCmdblt,         @RsCILDescrblt),
    (@RsCILNamebneun,       @RsCILCmdbneun,       @RsCILDescrbneun),
    (@RsCILNamebgeun,       @RsCILCmdbgeun,       @RsCILDescrbgeun),
    (@RsCILNamebgtun,       @RsCILCmdbgtun,       @RsCILDescrbgtun),
    (@RsCILNamebleun,       @RsCILCmdbleun,       @RsCILDescrbleun),
    (@RsCILNamebltun,       @RsCILCmdbltun,       @RsCILDescrbltun),
    (@RsCILNameswitch,      @RsCILCmdswitch,      @RsCILDescrswitch),
    (@RsCILNameldindi1,     @RsCILCmdldindi1,     @RsCILDescrldindi1),
    (@RsCILNameldindu1,     @RsCILCmdldindu1,     @RsCILDescrldindu1),
    (@RsCILNameldindi2,     @RsCILCmdldindi2,     @RsCILDescrldindi2),
    (@RsCILNameldindu2,     @RsCILCmdldindu2,     @RsCILDescrldindu2),
    (@RsCILNameldindi4,     @RsCILCmdldindi4,     @RsCILDescrldindi4),
    (@RsCILNameldindu4,     @RsCILCmdldindu4,     @RsCILDescrldindu4),
    (@RsCILNameldindi8,     @RsCILCmdldindi8,     @RsCILDescrldindi8),
    (@RsCILNameldindi,      @RsCILCmdldindi,      @RsCILDescrldindi),
    (@RsCILNameldindr4,     @RsCILCmdldindr4,     @RsCILDescrldindr4),
    (@RsCILNameldindr8,     @RsCILCmdldindr8,     @RsCILDescrldindr8),
    (@RsCILNameldindref,    @RsCILCmdldindref,    @RsCILDescrldindref),
    (@RsCILNamestindref,    @RsCILCmdstindref,    @RsCILDescrstindref),
    (@RsCILNamestindi1,     @RsCILCmdstindi1,     @RsCILDescrstindi1),
    (@RsCILNamestindi2,     @RsCILCmdstindi2,     @RsCILDescrstindi2),
    (@RsCILNamestindi4,     @RsCILCmdstindi4,     @RsCILDescrstindi4),
    (@RsCILNamestindi8,     @RsCILCmdstindi8,     @RsCILDescrstindi8),
    (@RsCILNamestindr4,     @RsCILCmdstindr4,     @RsCILDescrstindr4),
    (@RsCILNamestindr8,     @RsCILCmdstindr8,     @RsCILDescrstindr8),
    (@RsCILNameadd,         @RsCILCmdadd,         @RsCILDescradd),
    (@RsCILNamesub,         @RsCILCmdsub,         @RsCILDescrsub),
    (@RsCILNamemul,         @RsCILCmdmul,         @RsCILDescrmul),
    (@RsCILNamediv,         @RsCILCmddiv,         @RsCILDescrdiv),
    (@RsCILNamedivun,       @RsCILCmddivun,       @RsCILDescrdivun),
    (@RsCILNamerem,         @RsCILCmdrem,         @RsCILDescrrem),
    (@RsCILNameremun,       @RsCILCmdremun,       @RsCILDescrremun),
    (@RsCILNameand,         @RsCILCmdand,         @RsCILDescrand),
    (@RsCILNameor,          @RsCILCmdor,          @RsCILDescror),
    (@RsCILNamexor,         @RsCILCmdxor,         @RsCILDescrxor),
    (@RsCILNameshl,         @RsCILCmdshl,         @RsCILDescrshl),
    (@RsCILNameshr,         @RsCILCmdshr,         @RsCILDescrshr),
    (@RsCILNameshrun,       @RsCILCmdshrun,       @RsCILDescrshrun),
    (@RsCILNameneg,         @RsCILCmdneg,         @RsCILDescrneg),
    (@RsCILNamenot,         @RsCILCmdnot,         @RsCILDescrnot),
    (@RsCILNameconvi1,      @RsCILCmdconvi1,      @RsCILDescrconvi1),
    (@RsCILNameconvi2,      @RsCILCmdconvi2,      @RsCILDescrconvi2),
    (@RsCILNameconvi4,      @RsCILCmdconvi4,      @RsCILDescrconvi4),
    (@RsCILNameconvi8,      @RsCILCmdconvi8,      @RsCILDescrconvi8),
    (@RsCILNameconvr4,      @RsCILCmdconvr4,      @RsCILDescrconvr4),
    (@RsCILNameconvr8,      @RsCILCmdconvr8,      @RsCILDescrconvr8),
    (@RsCILNameconvu4,      @RsCILCmdconvu4,      @RsCILDescrconvu4),
    (@RsCILNameconvu8,      @RsCILCmdconvu8,      @RsCILDescrconvu8),
    (@RsCILNamecallvirt,    @RsCILCmdcallvirt,    @RsCILDescrcallvirt),
    (@RsCILNamecpobj,       @RsCILCmdcpobj,       @RsCILDescrcpobj),
    (@RsCILNameldobj,       @RsCILCmdldobj,       @RsCILDescrldobj),
    (@RsCILNameldstr,       @RsCILCmdldstr,       @RsCILDescrldstr),
    (@RsCILNamenewobj,      @RsCILCmdnewobj,      @RsCILDescrnewobj),
    (@RsCILNamecastclass,   @RsCILCmdcastclass,   @RsCILDescrcastclass),
    (@RsCILNameisinst,      @RsCILCmdisinst,      @RsCILDescrisinst),
    (@RsCILNameconvrun,     @RsCILCmdconvrun,     @RsCILDescrconvrun),
    (@RsCILNameunused2,     @RsCILCmdunused2,     @RsCILDescrunused2),
    (@RsCILNameunused3,     @RsCILCmdunused3,     @RsCILDescrunused3),
    (@RsCILNameunbox,       @RsCILCmdunbox,       @RsCILDescrunbox),
    (@RsCILNamethrow,       @RsCILCmdthrow,       @RsCILDescrthrow),
    (@RsCILNameldfld,       @RsCILCmdldfld,       @RsCILDescrldfld),
    (@RsCILNameldflda,      @RsCILCmdldflda,      @RsCILDescrldflda),
    (@RsCILNamestfld,       @RsCILCmdstfld,       @RsCILDescrstfld),
    (@RsCILNameldsfld,      @RsCILCmdldsfld,      @RsCILDescrldsfld),
    (@RsCILNameldsflda,     @RsCILCmdldsflda,     @RsCILDescrldsflda),
    (@RsCILNamestsfld,      @RsCILCmdstsfld,      @RsCILDescrstsfld),
    (@RsCILNamestobj,       @RsCILCmdstobj,       @RsCILDescrstobj),
    (@RsCILNameconvovfi1un, @RsCILCmdconvovfi1un, @RsCILDescrconvovfi1un),
    (@RsCILNameconvovfi2un, @RsCILCmdconvovfi2un, @RsCILDescrconvovfi2un),
    (@RsCILNameconvovfi4un, @RsCILCmdconvovfi4un, @RsCILDescrconvovfi4un),
    (@RsCILNameconvovfi8un, @RsCILCmdconvovfi8un, @RsCILDescrconvovfi8un),
    (@RsCILNameconvovfu1un, @RsCILCmdconvovfu1un, @RsCILDescrconvovfu1un),
    (@RsCILNameconvovfu2un, @RsCILCmdconvovfu2un, @RsCILDescrconvovfu2un),
    (@RsCILNameconvovfu4un, @RsCILCmdconvovfu4un, @RsCILDescrconvovfu4un),
    (@RsCILNameconvovfu8un, @RsCILCmdconvovfu8un, @RsCILDescrconvovfu8un),
    (@RsCILNameconvovfiun,  @RsCILCmdconvovfiun,  @RsCILDescrconvovfiun),
    (@RsCILNameconvovfuun,  @RsCILCmdconvovfuun,  @RsCILDescrconvovfuun),
    (@RsCILNamebox,         @RsCILCmdbox,         @RsCILDescrbox),
    (@RsCILNamenewarr,      @RsCILCmdnewarr,      @RsCILDescrnewarr),
    (@RsCILNameldlen,       @RsCILCmdldlen,       @RsCILDescrldlen),
    (@RsCILNameldelema,     @RsCILCmdldelema,     @RsCILDescrldelema),
    (@RsCILNameldelemi1,    @RsCILCmdldelemi1,    @RsCILDescrldelemi1),
    (@RsCILNameldelemu1,    @RsCILCmdldelemu1,    @RsCILDescrldelemu1),
    (@RsCILNameldelemi2,    @RsCILCmdldelemi2,    @RsCILDescrldelemi2),
    (@RsCILNameldelemu2,    @RsCILCmdldelemu2,    @RsCILDescrldelemu2),
    (@RsCILNameldelemi4,    @RsCILCmdldelemi4,    @RsCILDescrldelemi4),
    (@RsCILNameldelemu4,    @RsCILCmdldelemu4,    @RsCILDescrldelemu4),
    (@RsCILNameldelemi8,    @RsCILCmdldelemi8,    @RsCILDescrldelemi8),
    (@RsCILNameldelemi,     @RsCILCmdldelemi,     @RsCILDescrldelemi),
    (@RsCILNameldelemr4,    @RsCILCmdldelemr4,    @RsCILDescrldelemr4),
    (@RsCILNameldelemr8,    @RsCILCmdldelemr8,    @RsCILDescrldelemr8),
    (@RsCILNameldelemref,   @RsCILCmdldelemref,   @RsCILDescrldelemref),
    (@RsCILNamestelemi,     @RsCILCmdstelemi,     @RsCILDescrstelemi),
    (@RsCILNamestelemi1,    @RsCILCmdstelemi1,    @RsCILDescrstelemi1),
    (@RsCILNamestelemi2,    @RsCILCmdstelemi2,    @RsCILDescrstelemi2),
    (@RsCILNamestelemi4,    @RsCILCmdstelemi4,    @RsCILDescrstelemi4),
    (@RsCILNamestelemi8,    @RsCILCmdstelemi8,    @RsCILDescrstelemi8),
    (@RsCILNamestelemr4,    @RsCILCmdstelemr4,    @RsCILDescrstelemr4),
    (@RsCILNamestelemr8,    @RsCILCmdstelemr8,    @RsCILDescrstelemr8),
    (@RsCILNamestelemref,   @RsCILCmdstelemref,   @RsCILDescrstelemref),
    (@RsCILNameunused4,     @RsCILCmdunused4,     @RsCILDescrunused4),
    (@RsCILNameunused5,     @RsCILCmdunused5,     @RsCILDescrunused5),
    (@RsCILNameunused6,     @RsCILCmdunused6,     @RsCILDescrunused6),
    (@RsCILNameunused7,     @RsCILCmdunused7,     @RsCILDescrunused7),
    (@RsCILNameunused8,     @RsCILCmdunused8,     @RsCILDescrunused8),
    (@RsCILNameunused9,     @RsCILCmdunused9,     @RsCILDescrunused9),
    (@RsCILNameunused10,    @RsCILCmdunused10,    @RsCILDescrunused10),
    (@RsCILNameunused11,    @RsCILCmdunused11,    @RsCILDescrunused11),
    (@RsCILNameunused12,    @RsCILCmdunused12,    @RsCILDescrunused12),
    (@RsCILNameunused13,    @RsCILCmdunused13,    @RsCILDescrunused13),
    (@RsCILNameunused14,    @RsCILCmdunused14,    @RsCILDescrunused14),
    (@RsCILNameunused15,    @RsCILCmdunused15,    @RsCILDescrunused15),
    (@RsCILNameunused16,    @RsCILCmdunused16,    @RsCILDescrunused16),
    (@RsCILNameunused17,    @RsCILCmdunused17,    @RsCILDescrunused17),
    (@RsCILNameunused18,    @RsCILCmdunused18,    @RsCILDescrunused18),
    (@RsCILNameunused19,    @RsCILCmdunused19,    @RsCILDescrunused19),
    (@RsCILNameconvovfi1,   @RsCILCmdconvovfi1,   @RsCILDescrconvovfi1),
    (@RsCILNameconvovfu1,   @RsCILCmdconvovfu1,   @RsCILDescrconvovfu1),
    (@RsCILNameconvovfi2,   @RsCILCmdconvovfi2,   @RsCILDescrconvovfi2),
    (@RsCILNameconvovfu2,   @RsCILCmdconvovfu2,   @RsCILDescrconvovfu2),
    (@RsCILNameconvovfi4,   @RsCILCmdconvovfi4,   @RsCILDescrconvovfi4),
    (@RsCILNameconvovfu4,   @RsCILCmdconvovfu4,   @RsCILDescrconvovfu4),
    (@RsCILNameconvovfi8,   @RsCILCmdconvovfi8,   @RsCILDescrconvovfi8),
    (@RsCILNameconvovfu8,   @RsCILCmdconvovfu8,   @RsCILDescrconvovfu8),
    (@RsCILNameunused20,    @RsCILCmdunused20,    @RsCILDescrunused20),
    (@RsCILNameunused21,    @RsCILCmdunused21,    @RsCILDescrunused21),
    (@RsCILNameunused22,    @RsCILCmdunused22,    @RsCILDescrunused22),
    (@RsCILNameunused23,    @RsCILCmdunused23,    @RsCILDescrunused23),
    (@RsCILNameunused24,    @RsCILCmdunused24,    @RsCILDescrunused24),
    (@RsCILNameunused25,    @RsCILCmdunused25,    @RsCILDescrunused25),
    (@RsCILNameunused26,    @RsCILCmdunused26,    @RsCILDescrunused26),
    (@RsCILNamerefanyval,   @RsCILCmdrefanyval,   @RsCILDescrrefanyval),
    (@RsCILNameckfinite,    @RsCILCmdckfinite,    @RsCILDescrckfinite),
    (@RsCILNameunused27,    @RsCILCmdunused27,    @RsCILDescrunused27),
    (@RsCILNameunused28,    @RsCILCmdunused28,    @RsCILDescrunused28),
    (@RsCILNamemkrefany,    @RsCILCmdmkrefany,    @RsCILDescrmkrefany),
    (@RsCILNameunused29,    @RsCILCmdunused29,    @RsCILDescrunused29),
    (@RsCILNameunused30,    @RsCILCmdunused30,    @RsCILDescrunused30),
    (@RsCILNameunused31,    @RsCILCmdunused31,    @RsCILDescrunused31),
    (@RsCILNameunused32,    @RsCILCmdunused32,    @RsCILDescrunused32),
    (@RsCILNameunused33,    @RsCILCmdunused33,    @RsCILDescrunused33),
    (@RsCILNameunused34,    @RsCILCmdunused34,    @RsCILDescrunused34),
    (@RsCILNameunused35,    @RsCILCmdunused35,    @RsCILDescrunused35),
    (@RsCILNameunused36,    @RsCILCmdunused36,    @RsCILDescrunused36),
    (@RsCILNameunused37,    @RsCILCmdunused37,    @RsCILDescrunused37),
    (@RsCILNameldtoken,     @RsCILCmdldtoken,     @RsCILDescrldtoken),
    (@RsCILNameconvu2,      @RsCILCmdconvu2,      @RsCILDescrconvu2),
    (@RsCILNameconvu1,      @RsCILCmdconvu1,      @RsCILDescrconvu1),
    (@RsCILNameconvi,       @RsCILCmdconvi,       @RsCILDescrconvi),
    (@RsCILNameconvovfi,    @RsCILCmdconvovfi,    @RsCILDescrconvovfi),
    (@RsCILNameconvovfu,    @RsCILCmdconvovfu,    @RsCILDescrconvovfu),
    (@RsCILNameaddovf,      @RsCILCmdaddovf,      @RsCILDescraddovf),
    (@RsCILNameaddovfun,    @RsCILCmdaddovfun,    @RsCILDescraddovfun),
    (@RsCILNamemulovf,      @RsCILCmdmulovf,      @RsCILDescrmulovf),
    (@RsCILNamemulovfun,    @RsCILCmdmulovfun,    @RsCILDescrmulovfun),
    (@RsCILNamesubovf,      @RsCILCmdsubovf,      @RsCILDescrsubovf),
    (@RsCILNamesubovfun,    @RsCILCmdsubovfun,    @RsCILDescrsubovfun),
    (@RsCILNameendfinally,  @RsCILCmdendfinally,  @RsCILDescrendfinally),
    (@RsCILNameleave,       @RsCILCmdleave,       @RsCILDescrleave),
    (@RsCILNameleaves,      @RsCILCmdleaves,      @RsCILDescrleaves),
    (@RsCILNamestindi,      @RsCILCmdstindi,      @RsCILDescrstindi),
    (@RsCILNameconvu,       @RsCILCmdconvu,       @RsCILDescrconvu),
    (@RsCILNameunused38,    @RsCILCmdunused38,    @RsCILDescrunused38),
    (@RsCILNameunused39,    @RsCILCmdunused39,    @RsCILDescrunused39),
    (@RsCILNameunused40,    @RsCILCmdunused40,    @RsCILDescrunused40),
    (@RsCILNameunused41,    @RsCILCmdunused41,    @RsCILDescrunused41),
    (@RsCILNameunused42,    @RsCILCmdunused42,    @RsCILDescrunused42),
    (@RsCILNameunused43,    @RsCILCmdunused43,    @RsCILDescrunused43),
    (@RsCILNameunused44,    @RsCILCmdunused44,    @RsCILDescrunused44),
    (@RsCILNameunused45,    @RsCILCmdunused45,    @RsCILDescrunused45),
    (@RsCILNameunused46,    @RsCILCmdunused46,    @RsCILDescrunused46),
    (@RsCILNameunused47,    @RsCILCmdunused47,    @RsCILDescrunused47),
    (@RsCILNameunused48,    @RsCILCmdunused48,    @RsCILDescrunused48),
    (@RsCILNameunused49,    @RsCILCmdunused49,    @RsCILDescrunused49),
    (@RsCILNameunused50,    @RsCILCmdunused50,    @RsCILDescrunused50),
    (@RsCILNameunused51,    @RsCILCmdunused51,    @RsCILDescrunused51),
    (@RsCILNameunused52,    @RsCILCmdunused52,    @RsCILDescrunused52),
    (@RsCILNameunused53,    @RsCILCmdunused53,    @RsCILDescrunused53),
    (@RsCILNameunused54,    @RsCILCmdunused54,    @RsCILDescrunused54),
    (@RsCILNameunused55,    @RsCILCmdunused55,    @RsCILDescrunused55),
    (@RsCILNameunused56,    @RsCILCmdunused56,    @RsCILDescrunused56),
    (@RsCILNameunused57,    @RsCILCmdunused57,    @RsCILDescrunused57),
    (@RsCILNameunused58,    @RsCILCmdunused58,    @RsCILDescrunused58),
    (@RsCILNameunused59,    @RsCILCmdunused59,    @RsCILDescrunused59),
    (@RsCILNameunused60,    @RsCILCmdunused60,    @RsCILDescrunused60),
    (@RsCILNameprefix7,     @RsCILCmdprefix7,     @RsCILDescrprefix7),
    (@RsCILNameprefix6,     @RsCILCmdprefix6,     @RsCILDescrprefix6),
    (@RsCILNameprefix5,     @RsCILCmdprefix5,     @RsCILDescrprefix5),
    (@RsCILNameprefix4,     @RsCILCmdprefix4,     @RsCILDescrprefix4),
    (@RsCILNameprefix3,     @RsCILCmdprefix3,     @RsCILDescrprefix3),
    (@RsCILNameprefix2,     @RsCILCmdprefix2,     @RsCILDescrprefix2),
    (@RsCILNameprefix1,     @RsCILCmdprefix1,     @RsCILDescrprefix1),
    (@RsCILNameprefixref,   @RsCILCmdprefixref,   @RsCILDescrprefixref),

    (@RsCILNamearglist,     @RsCILCmdarglist,     @RsCILDescrarglist),
    (@RsCILNameceq,         @RsCILCmdceq,         @RsCILDescrceq),
    (@RsCILNamecgt,         @RsCILCmdcgt,         @RsCILDescrcgt),
    (@RsCILNamecgtun,       @RsCILCmdcgtun,       @RsCILDescrcgtun),
    (@RsCILNameclt,         @RsCILCmdclt,         @RsCILDescrclt),
    (@RsCILNamecltun,       @RsCILCmdcltun,       @RsCILDescrcltun),
    (@RsCILNameldftn,       @RsCILCmdldftn,       @RsCILDescrldftn),
    (@RsCILNameldvirtftn,   @RsCILCmdldvirtftn,   @RsCILDescrldvirtftn),
    (@RsCILNameunused61,    @RsCILCmdunused61,    @RsCILDescrunused61),
    (@RsCILNameldarg,       @RsCILCmdldarg,       @RsCILDescrldarg),
    (@RsCILNameldarga,      @RsCILCmdldarga,      @RsCILDescrldarga),
    (@RsCILNamestarg,       @RsCILCmdstarg,       @RsCILDescrstarg),
    (@RsCILNameldloc,       @RsCILCmdldloc,       @RsCILDescrldloc),
    (@RsCILNameldloca,      @RsCILCmdldloca,      @RsCILDescrldloca),
    (@RsCILNamestloc,       @RsCILCmdstloc,       @RsCILDescrstloc),
    (@RsCILNamelocalloc,    @RsCILCmdlocalloc,    @RsCILDescrlocalloc),
    (@RsCILNameunused62,    @RsCILCmdunused62,    @RsCILDescrunused62),
    (@RsCILNameendfilter,   @RsCILCmdendfilter,   @RsCILDescrendfilter),
    (@RsCILNameunaligned,   @RsCILCmdunaligned,   @RsCILDescrunaligned),
    (@RsCILNamevolatile,    @RsCILCmdvolatile,    @RsCILDescrvolatile),
    (@RsCILNametail,        @RsCILCmdtail,        @RsCILDescrtail),
    (@RsCILNameinitobj,     @RsCILCmdinitobj,     @RsCILDescrinitobj),
    (@RsCILNameunused63,    @RsCILCmdunused63,    @RsCILDescrunused63),
    (@RsCILNamecpblk,       @RsCILCmdcpblk,       @RsCILDescrcpblk),
    (@RsCILNameinitblk,     @RsCILCmdinitblk,     @RsCILDescrinitblk),
    (@RsCILNameunused64,    @RsCILCmdunused64,    @RsCILDescrunused64),
    (@RsCILNamerethrow,     @RsCILCmdrethrow,     @RsCILDescrrethrow),
    (@RsCILNameunused65,    @RsCILCmdunused65,    @RsCILDescrunused65),
    (@RsCILNamesizeof,      @RsCILCmdsizeof,      @RsCILDescrsizeof),
    (@RsCILNamerefanytype,  @RsCILCmdrefanytype,  @RsCILDescrrefanytype),
    (@RsCILNameunused66,    @RsCILCmdunused66,    @RsCILDescrunused66),
    (@RsCILNameunused67,    @RsCILCmdunused67,    @RsCILDescrunused67),
    (@RsCILNameunused68,    @RsCILCmdunused68,    @RsCILDescrunused68),
    (@RsCILNameunused69,    @RsCILCmdunused69,    @RsCILDescrunused69),
    (@RsCILNameunused70,    @RsCILCmdunused70,    @RsCILDescrunused70)
   );

  OpCodeParamTypes: array [TJclOpCode] of TJclInstructionParamType =
   (ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {00}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptU1,     ptU1,    {08}
    ptU1,     ptU1,     ptU1,     ptU1,     ptVoid,   ptVoid,   ptVoid,   ptVoid,  {10}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptI1,    {18}
    ptI4,     ptI8,     ptR4,     ptR8,     ptVoid,   ptVoid,   ptVoid,   ptToken, {20}
    ptToken,  ptVoid,   ptVoid,   ptSOff,   ptSOff,   ptSOff,   ptSOff,   ptSOff,  {28}
    ptSOff,   ptSOff,   ptSOff,   ptSOff,   ptSOff,   ptSOff,   ptSOff,   ptSOff,  {30}
    ptLOff,   ptLOff,   ptLOff,   ptLOff,   ptLOff,   ptLOff,   ptLOff,   ptLOff,  {38}
    ptLOff,   ptLOff,   ptLOff,   ptLOff,   ptLOff,   ptVoid,   ptVoid,   ptVoid,  {40}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {48}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {50}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {58}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {60}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptToken, {68}
    ptToken,  ptToken,  ptToken,  ptToken,  ptToken,  ptToken,  ptVoid,   ptVoid,  {70}
    ptVoid,   ptToken,  ptVoid,   ptToken,  ptToken,  ptToken,  ptToken,  ptToken, {78}
    ptToken,  ptToken,  ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {80}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptToken,  ptToken,  ptVoid,   ptToken, {88}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {90}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {98}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {A0}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {A8}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {B0}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {B8}
    ptVoid,   ptVoid,   ptToken,  ptVoid,   ptVoid,   ptVoid,   ptToken,   ptVoid, {C0}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {C8}
    ptToken,  ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {D0}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptI4,     ptI1,     ptVoid,  {D8}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {E0}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {E8}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {F0}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {F8}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptToken,  ptToken, {00}
    ptVoid,   ptU2,     ptU2,     ptU2,     ptU2,     ptU2,     ptU2,     ptVoid,  {08}
    ptVoid,   ptVoid,   ptI1,     ptVoid,   ptVoid,   ptToken,  ptVoid,   ptVoid,  {10}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptToken,  ptVoid,   ptVoid,   ptVoid,  {18}
    ptVoid,   ptVoid,   ptVoid);                                                   {20}

//===  { TJclClrILGenerator } ================================================

constructor TJclClrILGenerator.Create(AMethod: TJclClrMethodBody = nil);
var
  OpCode: Byte;
  Stream: TMemoryStream;
  Instruction: TJclInstruction;
begin
  inherited Create;
  FMethod := AMethod;
  FInstructions := TObjectList.Create;
  if Assigned(AMethod) then
  begin
    Stream := TMemoryStream.Create;
    try
      Stream.Write(Method.Code^, Method.Size);
      Stream.Seek(0, soFromBeginning);
      while Stream.Position < Stream.Size do
      begin
        OpCode := PByte(PAnsiChar(Stream.Memory) + Stream.Position)^;
        if OpCode = STP1 then
        begin
          OpCode := PByte(PAnsiChar(Stream.Memory) + Stream.Position + 1)^;
          Instruction := TJclInstruction.Create(Self, TJclOpCode(MaxByte + 1 + OpCode));
        end
        else
          Instruction := TJclInstruction.Create(Self, TJclOpCode(OpCode));
        if Assigned(Instruction) then
        begin
          FInstructions.Add(Instruction);
          Instruction.Load(Stream);
        end;
      end;
    finally
      FreeAndNil(Stream);
    end;
  end;
end;

destructor TJclClrILGenerator.Destroy;
begin
  FreeAndNil(FInstructions);
  inherited Destroy;
end;

function TJclClrILGenerator.DumpIL(Options: TJclInstructionDumpILOptions): string;
var
  I, J, Indent: Integer;

  function FlagsToName(Flags: TJclClrExceptionClauseFlags): string;
  begin
    if cfFinally in Flags then
      Result := 'finally'
    else
    if cfFilter in Flags then
      Result := 'filter'
    else
    if cfFault in Flags then
      Result := 'fault'
    else
      Result := 'catch';
  end;

  function IndentStr: string;
  begin
    Result := StrRepeat('  ', Indent);
  end;

var
  SL: TStrings;
  EH: TJclClrExceptionHandler;
begin
  Indent := 0;
  SL := TStringList.Create;
  try
    for I := 0 to InstructionCount-1 do
    begin
      for J := 0 to Method.ExceptionHandlerCount - 1 do
      begin
        EH := Method.ExceptionHandlers[J];
        if Instructions[I].Offset = EH.TryBlock.Offset then
        begin
          SL.Add(IndentStr + '.try');
          SL.Add(IndentStr + '{');
          Inc(Indent);
        end;
        if Instructions[I].Offset = (EH.TryBlock.Offset + EH.TryBlock.Length) then
        begin
          Dec(Indent);
          SL.Add(IndentStr + '}  // end .try');
        end;
        if Instructions[I].Offset = EH.HandlerBlock.Offset then
        begin
          SL.Add(IndentStr + FlagsToName(EH.Flags));
          SL.Add(IndentStr + '{');
          Inc(Indent);
        end;
        if Instructions[I].Offset = (EH.HandlerBlock.Offset + EH.HandlerBlock.Length) then
        begin
          Dec(Indent);
          SL.Add(IndentStr + '}  // end ' + FlagsToName(EH.Flags));
        end;
      end;
      SL.Add(IndentStr + Instructions[I].DumpIL(Options));
    end;
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TJclClrILGenerator.GetInstructionCount: Integer;
begin
  Result := FInstructions.Count;
end;

function TJclClrILGenerator.GetInstruction(const Idx: Integer): TJclInstruction;
begin
  Result := TJclInstruction(FInstructions[Idx]);
end;

//=== { TJclInstruction } ====================================================

constructor TJclInstruction.Create(AOwner :TJclClrILGenerator; AOpCode: TJclOpCode);
begin
  inherited Create;
  FOwner := AOwner;
  FOpCode := AOpCode;
end;

function TJclInstruction.GetWideOpCode: Boolean;
begin
  Result := Integer(OpCode) > MaxByte;
end;

function TJclInstruction.GetRealOpCode: Byte;
begin
  if WideOpCode then
    Result := Integer(OpCode) mod (MaxByte + 1)
  else
    Result := Integer(OpCode);
end;

function TJclInstruction.GetParamType: TJclInstructionParamType;
begin
  Result := OpCodeParamTypes[OpCode];
end;

function TJclInstruction.GetName: string;
begin
  Result := LoadResString(OpCodeInfos[OpCode, itName]);
end;

function TJclInstruction.GetFullName: string;
begin
  Result := LoadResString(OpCodeInfos[OpCode, itFullName]);
end;

function TJclInstruction.GetDescription: string;
begin
  Result := LoadResString(OpCodeInfos[OpCode, itDescription])
end;

function TJclInstruction.GetSize: DWORD;
const
  OpCodeSize: array [Boolean] of DWORD = (1, 2);
begin
  case ParamType of
    ptSOff, ptI1, ptU1:
      Result := SizeOf(Byte);
    ptI2, ptU2:
      Result := SizeOf(Word);
    ptLOff, ptI4, ptToken, ptU4, ptR4:
      Result := SizeOf(DWORD);
    ptI8, ptU8, ptR8:
      Result := SizeOf(Int64);
    ptArray:
      Result := (VarArrayHighBound(FParam, 1) - VarArrayLowBound(FParam, 1) + 1 + 1) * SizeOf(Integer);
  else
    Result := 0;
  end;
  Inc(Result, OpCodeSize[OpCode in [opNop..opPrefixRef]]);
end;

procedure TJclInstruction.Load(Stream: TStream);
var
  Code: Byte;
  I, ArraySize: DWORD;   { TODO : I, ArraySize = DWORD create a serious problem }
  Value: Integer;
begin
  FOffset := Stream.Position;
  try
    Code := 0;
    Stream.Read(Code, SizeOf(Code));
    if WideOpCode then
    begin
      if Code <> STP1 then
        raise EJclCliInstructionStreamInvalid.CreateRes(@RsInstructionStreamInvalid);
      Stream.Read(Code, SizeOf(Code));
    end;

    if Code <> RealOpCode then
      raise EJclCliInstructionStreamInvalid.CreateRes(@RsInstructionStreamInvalid);

    with TVarData(FParam) do
    case ParamType of
      ptU1:
        begin
          Stream.Read(VByte, SizeOf(Byte));
          VType := varByte;
        end;
      ptI2:
        begin
          Stream.Read(VSmallInt, SizeOf(SmallInt));
          VType := varSmallInt;
        end;
      ptLOff, ptI4:
        begin
          Stream.Read(VInteger, SizeOf(Integer));
          VType := varInteger;
        end;
      ptR4:
        begin
          Stream.Read(VSingle, SizeOf(Single));
          VType := varSingle;
        end;
      ptR8:                                       
        begin
          Stream.Read(VDouble, SizeOf(Double));
          VType := varDouble;
        end;
      ptArray:
        begin
          ArraySize := 0;
          Stream.Read(ArraySize, SizeOf(ArraySize));
          FParam := VarArrayCreate([0, ArraySize-1], varInteger);
          for I := 0 to ArraySize-1 do  { TODO : ArraySize = 0 and we have a nearly endless loop }
          begin
            Value := 0;
            Stream.Read(Value, SizeOf(Value));
            FParam[I] := Value;
          end;
        end;
      ptSOff, ptI1:
        begin
          Stream.Read(VShortInt, SizeOf(ShortInt));
          VType := varShortInt;
        end;
      ptU2:
        begin
          Stream.Read(VWord, SizeOf(Word));
          VType := varWord;
        end;
      ptToken, ptU4:
        begin
          Stream.Read(VLongWord, SizeOf(LongWord));
          VType := varLongWord;
        end;
      ptI8, ptU8:
        begin
          Stream.Read(VInt64, SizeOf(Int64));
          VType := varInt64;
        end;
    end;
  except
    Stream.Position := FOffset;
    raise;
  end;
end;

procedure TJclInstruction.Save(Stream: TStream);
var
  Code: Byte;
  ArraySize: DWORD;
  I, Value: Integer;
begin
  if WideOpCode then
  begin
    Code := STP1;
    Stream.Write(Code, SizeOf(Code));
  end;

  Code := RealOpCode;;
  Stream.Write(Code, SizeOf(Code));

  case ParamType of
    ptU1:
      Stream.Write(TVarData(FParam).VByte, SizeOf(Byte));
    ptI2:
      Stream.Write(TVarData(FParam).VSmallInt, SizeOf(SmallInt));
    ptLOff, ptI4:
      Stream.Write(TVarData(FParam).VInteger, SizeOf(Integer));
    ptR4:
      Stream.Write(TVarData(FParam).VSingle, SizeOf(Single));
    ptR8:
      Stream.Write(TVarData(FParam).VDouble, SizeOf(Double));
    ptSOff, ptI1:
      Stream.Write(TVarData(FParam).VShortInt, SizeOf(ShortInt));
    ptU2:
      Stream.Write(TVarData(FParam).VWord, SizeOf(Word));
    ptToken, ptU4:
      Stream.Write(TVarData(FParam).VLongWord, SizeOf(LongWord));
    ptI8, ptU8:
      Stream.Write(TVarData(FParam).VInt64, SizeOf(Int64));
    ptArray:
      begin
        ArraySize := VarArrayHighBound(FParam, 1) - VarArrayLowBound(FParam, 1) + 1;
        Stream.Write(ArraySize, SizeOf(ArraySize));
        { TODO : VarArrayHighBound to VarArrayLowBound very likely wrong }
        for I := VarArrayHighBound(FParam, 1) to VarArrayLowBound(FParam, 1) do
        begin
          Value := VarArrayGet(FParam, [I]);
          Stream.Write(Value, SizeOf(Value));
        end;
      end;
  end;
end;

function TJclInstruction.DumpIL(Options: TJclInstructionDumpILOptions): string;
var
  Opt: TJclInstructionDumpILOption;
begin
  if doLineNo in Options then
    Result := DumpILOption(doLineNo) + ': ';
  if doRawBytes in Options then
    Result := Result + Format(' /* %.24s */ ', [DumpILOption(doRawBytes)]);
  for Opt := doIL to doTokenValue do
    Result := Result + DumpILOption(Opt) + ' ';
  if (doComment in Options) and ((FullName <> '') or (Description <> '')) then
    Result := Result + ' // ' + DumpILOption(doComment);
end;

function TJclInstruction.FormatLabel(Offset: Integer): string;
begin
  Result := 'IL_' + IntToHex(Offset, 4);
end;

function TJclInstruction.DumpILOption(Option: TJclInstructionDumpILOption): string;

  function TokenToString(Token: DWORD): string;
  begin
    Result := '(' + IntToHex(Token shr 24, 2) + ')' + IntToHex(Token mod (1 shl 24), 6);
  end;

var
  I: Integer;
  Row: TJclClrTableRow;
  CodeStr, ParamStr: string;
begin
  case Option of
    doLineNo:
      Result := 'IL_' + IntToHex(Offset, 4);
    doRawBytes:
      begin
        if WideOpCode then
          CodeStr := IntToHex(STP1, 2);

        CodeStr := CodeStr + IntToHex(RealOpCode, 2);
        CodeStr := CodeStr + StrRepeat(' ', 4 - Length(CodeStr));

        case ParamType of
          ptSOff, ptI1, ptU1:
            ParamStr := IntToHex(TVarData(FParam).VByte, 2);
          ptArray:
            ParamStr := 'Array';
          ptI2, ptU2:
            ParamStr := IntToHex(TVarData(FParam).VWord, 4);
          ptLOff, ptI4, ptU4, ptR4:
            ParamStr := IntToHex(TVarData(FParam).VLongWord, 8);
          ptI8, ptU8, ptR8:
            ParamStr := IntToHex(TVarData(FParam).VInt64, 16);
          ptToken:
            ParamStr := TokenToString(TVarData(FParam).VLongWord);
        else
          ParamStr := '';
        end;
        ParamStr := ParamStr + StrRepeat(' ', 10 - Length(ParamStr));
        Result := CodeStr + ' | ' + ParamStr;
      end;
    doIL:
      begin
        case ParamType of
        ptVoid:
          ; // do nothing
        ptLOff:
          Result := FormatLabel(Integer(Offset) + + Integer(Size) + TVarData(Param).VInteger - 1);
        ptToken:
          begin
            if Byte(TJclPeMetadata.TokenTable(TVarData(Param).VLongWord)) = $70 then
              Result := '"' + Owner.Method.Method.Table.Stream.Metadata.UserStringAt(TJclPeMetadata.TokenIndex(TVarData(Param).VLongWord)) + '"'
            else
            begin
              Row := Owner.Method.Method.Table.Stream.Metadata.Tokens[TVarData(Param).VLongWord];
              if Assigned(Row) then
              begin
                if Row is TJclClrTableTypeDefRow then
                  Result := TJclClrTableTypeDefRow(Row).FullName
                else
                if Row is TJclClrTableTypeRefRow then
                  Result := TJclClrTableTypeRefRow(Row).FullName
                else
                if Row is TJclClrTableMethodDefRow then
                  with TJclClrTableMethodDefRow(Row) do
                    Result := ParentToken.FullName + '.' + Name
                else
                if Row is TJclClrTableMemberRefRow then
                  Result := TJclClrTableMemberRefRow(Row).FullName
                else
                if Row is TJclClrTableFieldDefRow then
                  with TJclClrTableFieldDefRow(Row) do
                    Result := ParentToken.FullName + '.' + Name
                else
                  Result := Row.DumpIL;
              end
              else
                Result := '';
            end;
            Result := Result + ' /* ' + IntToHex(TVarData(FParam).VLongWord, 8) + ' */';
          end;
        ptSOff:
          Result := FormatLabel(Integer(Offset + Size) + TVarData(Param).VShortInt - 1);
        ptArray:
          begin
            for I := VarArrayHighBound(FParam, 1) to VarArrayLowBound(FParam, 1) do
            begin
              Result := Result + FormatLabel(Offset + Size + VarArrayGet(FParam, [I]));
              if I <> VarArrayLowBound(FParam, 1) then
                Result := Result + ', ';
            end;
            Result := ' (' + Result + ')';
          end;
        else
          Result := VarToStr(Param);
        end;
        Result := GetName + StrRepeat(' ', 10 - Length(GetName)) + ' ' + Result;
        Result := Result + StrRepeat(' ', 20 - Length(Result));
      end;
    doTokenValue:
      Result := ''; // do nothing
    doComment:
      if FullName = '' then
        Result := Description
      else
      if Description = '' then
        Result := FullName
      else
        Result := FullName + ' - ' + Description;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
