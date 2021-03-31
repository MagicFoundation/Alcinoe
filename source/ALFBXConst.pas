{*******************************************************************************
Author(s):
Henri Gourvest <hgourvest@progdigy.com>
Olivier Guilbaud <oguilb@free.fr>
The Original Code is the UIB code (version 2.1)

The Initial Developer of the Original Code is
Henri Gourvest <hgourvest@progdigy.com>. Portions
created by the Initial Developer are Copyright (C)
by the Initial Developer. All Rights Reserved.

Description:
ALFBX (Alcinoe FireBird Express) does for the Firebird
API what Delphi does for the WINDOWS API! Create high
performance client/server applications based on FireBird
without the BDE or ODBC.

Link :
https://uib.svn.sourceforge.net/svnroot/uib (current code is from the trunk rev 382)
http://www.progdigy.com/modules.php?name=UIB
*******************************************************************************}

unit ALFBXConst;

interface

const
  cALFBXBreakLine = #13;
  cALFBXNewLine = #13#10;
  cALFBXTrue  = 'True';
  cALFBXFalse = 'False';

  // UIB Errors
  cALFBX_INVALIDEIBVERSION   = 'Incorrect Database Server version, check compiler options.';
  cALFBX_CANTLOADLIB         = 'Can''t load library: %s.';
  cALFBX_DBHANDLEALREADYSET  = 'Database handle already assigned, first disconnect database.';
  cALFBX_TRANSACTIONNOTDEF   = 'Transaction not assigned.';
  cALFBX_DATABASENOTDEF      = 'Database not assigned.';
  cALFBX_QUERYNOTOPEN        = 'Query not open.';
  cALFBX_CASTERROR           = 'Cast error.';
  cALFBX_UNEXPECTEDERROR     = 'Unexpected error.';
  cALFBX_FIELDNUMNOTFOUND    = 'Field num: %d not found.';
  cALFBX_FIELDSTRNOTFOUND    = 'Field "%s" not found.';
  cALFBX_PARAMSTRNOTFOUND    = 'Parameter "%s" not found.';
  cALFBX_BLOBFIELDNOTFOUND   = 'Blob field num: %d not found.';
  cALFBX_FETCHBLOBNOTSET     = 'FetchBlob property must be set to use this method.';
  cALFBX_INDEXERROR          = 'Index out of bound (%d)';
  cALFBX_SIZENAME            = 'Size name too big (%s)';
  cALFBX_MUSTBEPREPARED      = 'The query must be prepared first.';
  cALFBX_MUSTBEOPEN          = 'The query must be opened first.';
  cALFBX_EXPLICITTRANS       = 'Transaction must be started explicitly.';
  cALFBX_EXCEPTIONNOTFOUND   = 'Exception name %s, not found.';
  cALFBX_EXPTIONREGISTERED   = 'Exception: %d already registered';
  cALFBX_NOAUTOSTOP          = 'Transaction must be closed explicitly.';
  cALFBX_NOGENERATOR         = 'Generator %s not found.';
  cALFBX_NOFIELD             = 'Field not found.';
  cALFBX_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  cALFBX_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  cALFBX_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  cALFBX_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  cALFBX_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  cALFBX_PARSESETNAMES       = 'Parse error: SET NAMES';
  cALFBX_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  cALFBX_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  cALFBX_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  cALFBX_SERVICESPARSING     = 'Error while parsing Services API output.';
  cALFBX_NOT_NULLABLE        = 'Field "%s" is not nullable.';

implementation

end.
