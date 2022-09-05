(********************************************************************************)
(*                        UNIFIED INTERBASE (UIB)                               *)
(*                                                                              *)
(* The contents of this file are subject to the Mozilla Public License Version  *)
(* 1.1 (the "License"); you may not use this file except in compliance with the *)
(* License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ *)
(*                                                                              *)
(* Software distributed under the License is distributed on an "AS IS" basis,   *)
(* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for *)
(* the specific language governing rights and limitations under the License.    *)
(*                                                                              *)
(* Unit owner : Henri Gourvest <hgourvest@progdigy.com>                         *)
(* Contributor: Olivier Guilbaud <oguilb@free.fr>                               *)
(*                                                                              *)
(********************************************************************************)
(* $Id: uib.inc,v 1.13 2005/07/11 22:34:53 progdigy Exp $ *)
(********************************************************************************)

{$I uibcommon.inc}

{$IFDEF FREEBSD}
  {$DEFINE UNIX}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE UNIX}
{$ENDIF}

// *****************
// BEGIN USER DEFINE
// *****************

//----------
// FIREBIRD
//----------
{.$DEFINE FB102}
{.$DEFINE FB103}
{.$DEFINE FB15}
{.$DEFINE FB20}
{.$DEFINE FB21}
{.$DEFINE FB25}
{.$DEFINE FBEMBED}

//----------
// INTERBASE
//----------
{.$DEFINE IB65}
{.$DEFINE IB7}
{.$DEFINE IB71}
{.$DEFINE IB75}

//----------
// Languages
//----------
{$DEFINE UIBLANG_EN}  // English
{.$DEFINE UIBLANG_FR} // French
{.$DEFINE UIBLANG_CZ} // Czech
{.$DEFINE UIBLANG_DE} // Deutch
{.$DEFINE UIBLANG_RU} // Russian
{.$DEFINE UIBLANG_ES} // Espanol
{.$DEFINE UIBLANG_TR} // Turkish

// use SYNEDIT (sql editor)
{$IFNDEF UNICODE}
{$DEFINE HAVE_SYNEDIT}
{$ENDIF}

// no components
{.$DEFINE UIB_NO_COMPONENT}

// support VARCHAR(16) CHARSET OCTET COLLATION OCTET as TGUID
{.$DEFINE GUID_TYPE}

// set this to handle GUID as VARCHAR(38) instead of standard VARCHAR(16) "octets"
{.$DEFINE GUID_AS_TEXT}

// ***************
// END USER DEFINE
// ***************

{$IFDEF COMPILER7_UP}
 {$WARN UNSAFE_TYPE OFF}
 {$WARN UNSAFE_CODE OFF}
 {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFDEF FREEBSD}
  {$UNDEF UIBTHREADSAFE}
{$ENDIF}

{$IFDEF FPC}
  {$IFDEF FREEBSD}
    {$THREADING OFF}
  {$ELSE}
    {.$THREADING ON}
  {$ENDIF}
{$ENDIF}

{$IFDEF UNIX}
  {$DEFINE SET_TCP_NO_DELAY}
{$ENDIF}

// FIREBIRD

{$IFDEF FB25}
  {$DEFINE FIREBIRD}
  {$DEFINE FB25_UP}
  {$DEFINE FB21_UP}
  {$DEFINE FB20_UP}
  {$DEFINE FB15_UP}
  {$DEFINE FB103_UP}
  {$DEFINE FB102_UP}

  {$DEFINE IB7ORFB15}
{$ENDIF}

{$IFDEF FB21}
  {$DEFINE FIREBIRD}
  {$DEFINE FB21_UP}
  {$DEFINE FB20_UP}
  {$DEFINE FB15_UP}
  {$DEFINE FB103_UP}
  {$DEFINE FB102_UP}

  {$DEFINE IB7ORFB15}
{$ENDIF}

{$IFDEF FB20}
  {$DEFINE FIREBIRD}
  {$DEFINE FB20_UP}
  {$DEFINE FB15_UP}
  {$DEFINE FB103_UP}
  {$DEFINE FB102_UP}

  {$DEFINE IB7ORFB15}
{$ENDIF}

{$IFDEF FB15}
  {$DEFINE FIREBIRD}
  {$DEFINE FB15_UP}
  {$DEFINE FB103_UP}
  {$DEFINE FB102_UP}

  {$DEFINE IB7ORFB15}
{$ENDIF}

{$IFDEF FB103}
  {$DEFINE FIREBIRD}
  {$DEFINE FB103_UP}
  {$DEFINE FB102_UP}
{$ENDIF}

{$IFDEF FB102}
  {$DEFINE FIREBIRD}
  {$DEFINE FB102_UP}
{$ENDIF}

// INTERBASE

{$IFDEF IB75}
  {$DEFINE INTERBASE}
  {$DEFINE IB75_UP}
  {$DEFINE IB71_UP}
  {$DEFINE IB7_UP}
  {$DEFINE IB65_UP}

  {$DEFINE IB7ORFB15}
{$ENDIF}

{$IFDEF IB71}
  {$DEFINE INTERBASE}
  {$DEFINE IB71_UP}
  {$DEFINE IB7_UP}
  {$DEFINE IB65_UP}

  {$DEFINE IB7ORFB15}
{$ENDIF}

{$IFDEF IB7}
  {$DEFINE INTERBASE}
  {$DEFINE IB7_UP}
  {$DEFINE IB65_UP}

  {$DEFINE IB7ORFB15}
{$ENDIF}

{$IFDEF IB65}
  {$DEFINE INTERBASE}
  {$DEFINE IB65_UP}
{$ENDIF}

////////////////////////

{$IFDEF INTERBASE}
  {$DEFINE INTERBASEORYAFFIL}
  {$DEFINE INTERBASEORFIREBIRD}
{$ENDIF}

{$IFDEF FIREBIRD}
  {$DEFINE INTERBASEORFIREBIRD}
  {$DEFINE FIREBIRDORYAFFIL}
{$ENDIF}

{$IFDEF YAFFIL}
  {$DEFINE INTERBASEORYAFFIL}
  {$DEFINE FIREBIRDORYAFFIL}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE SUPPORTS_WIDESTRING}
  {$DEFINE SUPPORTS_INTERFACE}
  {$DEFINE SUPPORTS_INT64}
  {$DEFINE SUPPORTS_DYNAMICARRAYS}
  {$DEFINE SUPPORTS_DEFAULTPARAMS}
  {$DEFINE SUPPORTS_OVERLOAD}
  {.$ASMMODE Intel}
  {.$UNDEF UseRegAsm}
  {$GOTO ON}
{$ENDIF}

{$IFDEF FB15_UP}
{$IFDEF MSWINDOWS}
  {$DEFINE DLLREGISTRY}
{$ENDIF}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE PUREPASCAL}
{$ENDIF}

{$IFDEF KYLIX}
  {$DEFINE PUREPASCAL}
{$ENDIF}

{$IFNDEF GUID_TYPE}
  {$UNDEF GUID_AS_TEXT}
{$ENDIF}

