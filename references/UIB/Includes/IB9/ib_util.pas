{***********************************************************}
{                                                           }
{ 	PROGRAM:	UDF and Blob filter Utilities library         }
{ 	MODULE:		ib_util.h                                     }
{ 	DESCRIPTION:	Prototype header file for ib_util.c       }
{                                                           }
{  copyright (c) 1998 by InterBase Software Corporation     }
{                                                           }
{***********************************************************}
unit ib_util;

interface

function ib_util_malloc(l: integer): pointer; cdecl; external 'ib_util.dll';

implementation

end.

