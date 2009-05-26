{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/
Author(s):    Henri Gourvest <hgourvest@progdigy.com>
              Olivier Guilbaud <oguilb@free.fr>
              Stéphane Vander Clock (svanderclock@arkadia.com)
              The Original Code is the UIB code (version 2.1)

              The Initial Developer of the Original Code is
              Henri Gourvest <hgourvest@progdigy.com>. Portions
              created by the Initial Developer are Copyright (C)
              by the Initial Developer. All Rights Reserved.

Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALFBX (Alcinoe FireBird Express) - ALFBXConst
Version:      3.50

Description:  ALFBX (Alcinoe FireBird Express) does for the Firebird
              API what Delphi does for the WINDOWS API! Create high
              performance client/server applications based on FireBird
              without the BDE or ODBC.

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

History :

Link :        http://www.progdigy.com/modules.php?name=UIB

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}

unit ALFBXConst;

interface

const
  cALFBXBreakLine = #13;
  cALFBXNewLine = #13#10;
  cALFBXTrue  = 'True';
  cALFBXFalse = 'False';

  // FBX Errors
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

implementation

end.
