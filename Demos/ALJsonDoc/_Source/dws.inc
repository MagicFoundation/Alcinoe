{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at                                          }
{                                                                      }
{    http://www.mozilla.org/MPL/                                       }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    The Original Code is DelphiWebScriptII source code, released      }
{    January 1, 2001                                                   }
{                                                                      }
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. Portions created by Matthias Ackermann are             }
{    Copyright (C) 2000 Matthias Ackermann, Switzerland. All           }
{    Rights Reserved.                                                  }
{                                                                      }
{    Contributor(s): Willibald Krenn                                   }
{                                                                      }
{**********************************************************************}

// DWS include file

{global compiler options}

{$A+} // align on
{$B-} // BoolEval off
{$X+} // extended syntax
{$H+} // long strings
{$Q-} // overflow checks off
{$J-} // assignable typed constants off

{$T-} // typed addr off - otherwise you'll get problems with ComSupport
      // <<.CreateResFmt(@SNoMethod, [Names]);>>

{$IFNDEF FPC}
   {$IFNDEF VER200}  // D2009
      {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
   {$ENDIF}
   {$IFDEF VER240}
      {$DEFINE DELPHI_XE3}
   {$ENDIF}
   {$IF CompilerVersion>21.0}
      {$DEFINE DELPHI_XE_PLUS}
   {$ELSE}
      {$DEFINE DELPHI_2010_MINUS}
      {$DEFINE WINDOWS}
   {$ENDIF}
   {$IF CompilerVersion>22.0}
      {$DEFINE DELPHI_XE2_PLUS}
   {$ENDIF}
   {$IF CompilerVersion>23.0}
      {$DEFINE DELPHI_XE3_PLUS}
   {$ENDIF}
   {$IF CompilerVersion>=32.0}
      {$DEFINE DELPHI_TOKYO_PLUS}
   {$ENDIF}
   {$IF CompilerVersion>=35.0}
      {$DEFINE DELPHI_ALEXANDRIA_PLUS}
   {$ENDIF}
   {$IF CompilerVersion>=36.0}
      {$DEFINE DELPHI_ATHENS_PLUS}
   {$ENDIF}
{$ELSE}
   {$mode delphi}
   {$DEFINE PUREPASCAL}
   {$ASMMODE INTEL}
{$ENDIF}

{.$DEFINE PUREPASCAL}
{.$DEFINE UNICODE_IDENTIFIERS}
{.$DEFINE COALESCE_VAR_INITIALIZATION}
{$define JS_BIGINTEGER}

{$IFNDEF PUREPASCAL}
   {$IF Defined(WIN32)}
      {$DEFINE WIN32_ASM}
   {$ELSEIF Defined(WIN64)}
      {$DEFINE WIN64_ASM}
   {$IFEND}
{$ENDIF}

{$IFNDEF FPC}
  {$IFNDEF DELPHI_XE2_PLUS}
     {$DEFINE CPUX86}
  {$ENDIF}
{$ENDIF}

{$IF Defined(WIN32)}
   {$DEFINE WINDOWS}
   {$DEFINE WIN32}
{$ELSEIF Defined(WIN64)}
   {$DEFINE WINDOWS}
   {$DEFINE WIN64}
{$IFEND}

{$IFOPT R+}
   {$DEFINE RANGEON}
{$ELSE}
   {$UNDEF RANGEON}
{$ENDIF}
