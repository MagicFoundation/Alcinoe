{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/
Author(s):    Henri Gourvest <hgourvest@progdigy.com>
              Olivier Guilbaud <oguilb@free.fr>
              St�phane Vander Clock (svanderclock@arkadia.com)
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

{$I ALFBX.inc}

unit ALFBXconst;

interface

{$IFNDEF DELPHI6_UP}
{$IFNDEF BCB}
const
  S_OK    = $00000000;
  S_FALSE = $00000001;
{$ENDIF BCB}
{$ENDIF DELPHI6_UP}


type
  // ALFBX Server Commands
  TServerCommand = (scGetClassObject, scInvokeMethod);

  // Metadata Object Identifiers
  TOIDDatabase = (OIDDomain, OIDTable, OIDView, OIDProcedure, OIDGenerator,
    OIDException, OIDUDF, OIDRole, OIDDBCharset);
  TOIDDatabases = set of TOIDDatabase;

  TOIDTable = (OIDTableField, OIDPrimary, OIDForeign, OIDTableTrigger,
    OIDUnique, OIDIndex, OIDCheck, OIDTableGrant, OIDTableFieldGrant);
  TOIDTables = set of TOIDTable;

  TOIDView = (OIDViewField, OIDViewTrigers, OIDViewGrant, OIDViewFieldGrant);
  TOIDViews = set of TOIDView;

  TOIDProcedure = (OIDProcFieldIn, OIDProcFieldOut, OIDProcedureGrant);
  TOIDProcedures = set of TOIDProcedure;

  TOIDUDF = (OIDUDFField);
  TOIDUDFs = set of TOIDUDF;

  TOIDRole = (OIDRoleGrant);
  TOIDRoles = set of TOIDRole;

  { Table Grants Privileges }
  TTablePrivilege = (tpSelect, tpInsert, tpUpdate, tpDelete, tpReference);
  TTablePrivileges = set of TTablePrivilege;

  { Field Grants Privileges }
  TFieldPrivilege = (fpUpdate, fpReference);
  TFieldPrivileges = set of TFieldPrivilege;

const
  ALLOBjects = [OIDDomain, OIDTable, OIDView, OIDProcedure, OIDGenerator,
    OIDException, OIDUDF, OIDRole, OIDDBCharset];
  ALLTables = [OIDTableField, OIDPrimary, OIDForeign, OIDTableTrigger,
    OIDUnique,OIDIndex, OIDCheck, OIDTableGrant, OIDTableFieldGrant];
  ALLViews = [OIDViewField, OIDViewTrigers, OIDViewGrant, OIDViewFieldGrant];
  ALLProcedures = [OIDProcFieldIn, OIDProcFieldOut, OIDProcedureGrant];
  ALLUDFs = [OIDUDFField];
  ALLRoles = [OIDRoleGrant];
  { All Grantable Privileges }
  ALLTablePrivileges = [tpSelect, tpInsert, tpUpdate, tpDelete, tpReference];
  ALLFieldPrivileges = [fpUpdate, fpReference];
{$IFDEF UNIX}
  BreakLine = #10;
  NewLine = #10#10; //BreakLine + BreakLine;
{$ELSE}
  BreakLine = #13;
  NewLine = #13#10;
{$ENDIF}

const
  {$IFDEF ALFBXLANG_EN}
  sALFBXTrue  = 'True';
  sALFBXFalse = 'False';

  // ALFBX Errors
  EALFBX_INVALIDEIBVERSION   = 'Incorrect Database Server version, check compiler options.';
  EALFBX_CANTLOADLIB         = 'Can''t load library: %s.';
  EALFBX_DBHANDLEALREADYSET  = 'Database handle already assigned, first disconnect database.';
  EALFBX_TRANSACTIONNOTDEF   = 'Transaction not assigned.';
  EALFBX_DATABASENOTDEF      = 'Database not assigned.';
  EALFBX_QUERYNOTOPEN        = 'Query not open.';
  EALFBX_CASTERROR           = 'Cast error.';
  EALFBX_UNEXPECTEDERROR     = 'Unexpected error.';
  EALFBX_FIELDNUMNOTFOUND    = 'Field num: %d not found.';
  EALFBX_FIELDSTRNOTFOUND    = 'Field "%s" not found.';
  EALFBX_PARAMSTRNOTFOUND    = 'Parameter "%s" not found.';
  EALFBX_BLOBFIELDNOTFOUND   = 'Blob field num: %d not found.';
  EALFBX_FETCHBLOBNOTSET     = 'FetchBlob property must be set to use this method.';
  EALFBX_INDEXERROR          = 'Index out of bound (%d)';
  EALFBX_SIZENAME            = 'Size name too big (%s)';
  EALFBX_MUSTBEPREPARED      = 'The query must be prepared first.';
  EALFBX_MUSTBEOPEN          = 'The query must be opened first.';
  EALFBX_EXPLICITTRANS       = 'Transaction must be started explicitly.';
  EALFBX_EXCEPTIONNOTFOUND   = 'Exception name %s, not found.';
  EALFBX_EXPTIONREGISTERED   = 'Exception: %d already registered';
  EALFBX_NOAUTOSTOP          = 'Transaction must be closed explicitly.';
  EALFBX_NOGENERATOR         = 'Generator %s not found.';
  EALFBX_NOFIELD             = 'Field not found.';
  EALFBX_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EALFBX_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EALFBX_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EALFBX_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EALFBX_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EALFBX_PARSESETNAMES       = 'Parse error: SET NAMES';
  EALFBX_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EALFBX_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EALFBX_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EALFBX_SERVICESPARSING     = 'Error while parsing Services API output.';
  {$ENDIF ALFBXLANG_EN}

  {$IFDEF ALFBXLANG_FR}
  sALFBXTrue  = 'Vrai';
  sALFBXFalse = 'Faux';

  // ALFBX Errors
  EALFBX_INVALIDEIBVERSION   = 'Version de base de donn�es incorrecte, v�rifiez les options de compilation.';
  EALFBX_CANTLOADLIB         = 'Impossible de charger la DLL: %s.';
  EALFBX_DBHANDLEALREADYSET  = 'Le handle de la base de donn�es est d�j� d�fini, d�connectez d''abord la base de donn�es.';
  EALFBX_TRANSACTIONNOTDEF   = 'La transaction n''est pas d�finie';
  EALFBX_DATABASENOTDEF      = 'La base de donn�es n''est pas d�finie.';
  EALFBX_QUERYNOTOPEN        = 'La requ�te n''est pas encore ouverte.';
  EALFBX_CASTERROR           = 'Transtypage incorrect.';
  EALFBX_UNEXPECTEDERROR     = 'Erreur inattendue.';
  EALFBX_FIELDNUMNOTFOUND    = 'Le champ num�ro: %d n''existe pas.';
  EALFBX_FIELDSTRNOTFOUND    = 'Le champ "%s" n''existe pas.';
  EALFBX_PARAMSTRNOTFOUND    = 'Le Parametre "%s" n''existe pas.';
  EALFBX_BLOBFIELDNOTFOUND   = 'Le champ Blob num�ro: %d n''existe pas.';
  EALFBX_FETCHBLOBNOTSET     = 'La propri�t� FetchBlob doit �tre activ�e pour utiliser cette m�thode.';
  EALFBX_INDEXERROR          = 'Indice de liste hors limites (%d)';
  EALFBX_SIZENAME            = 'La taille du nom est trop grande (%s)';
  EALFBX_MUSTBEPREPARED      = 'La requ�te doit d''abord �tre pr�par�e.';
  EALFBX_MUSTBEOPEN          = 'La requ�te doit d''abord �tre ouverte.';
  EALFBX_EXPLICITTRANS       = 'La transaction doit �tre d�marr�e explicitement.';
  EALFBX_EXCEPTIONNOTFOUND   = 'L''exception %s, n''existe pas.';
  EALFBX_EXPTIONREGISTERED   = 'L''exception %d a d�j� �t� enregistr�e.';
  EALFBX_NOAUTOSTOP          = 'La transaction doit �tre ferm�e explicitement.';
  EALFBX_NOGENERATOR         = 'Generator %s not found.';
  EALFBX_NOFIELD             = 'Field not found.';
  EALFBX_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EALFBX_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EALFBX_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EALFBX_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EALFBX_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EALFBX_PARSESETNAMES       = 'Parse error: SET NAMES';
  EALFBX_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EALFBX_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EALFBX_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EALFBX_SERVICESPARSING     = 'Error while parsing Services API output.';

  {$ENDIF ALFBXLANG_FR}

  {$IFDEF ALFBXLANG_CZ}
  sALFBXTrue  = 'Ano';
  sALFBXFalse = 'Ne';

  // ALFBX Errors
  EALFBX_INVALIDEIBVERSION   = 'Nekorektn� verze datab�zov�ho serveru, zkontrolujte nastaven� kompileru.';
  EALFBX_CANTLOADLIB         = 'Nelze na��st knihovnu: %s.';
  EALFBX_DBHANDLEALREADYSET  = 'Kan�l datab�ze je ji� p�ipraven, nejd��ve odpojte datab�zi.';
  EALFBX_TRANSACTIONNOTDEF   = 'Transakce nen� p�i�azena.';
  EALFBX_DATABASENOTDEF      = 'Datab�ze nen� p�i�azena.';
  EALFBX_QUERYNOTOPEN        = 'Dotaz nen� otev�en.';
  EALFBX_CASTERROR           = 'Chyba bsazen�.';
  EALFBX_UNEXPECTEDERROR     = 'Nezn�m� chyba.';
  EALFBX_FIELDNUMNOTFOUND    = 'Polo�ka ��slo: %d neexistuje.';
  EALFBX_FIELDSTRNOTFOUND    = 'Polo�ka "%s" neexistuje.';
  EALFBX_PARAMSTRNOTFOUND    = 'Parametr "%s" neexistuje.';
  EALFBX_BLOBFIELDNOTFOUND   = 'Blob z�znam ��slo: %d neexistuje.';
  EALFBX_FETCHBLOBNOTSET     = 'Vlastnost FetchBlob mus� b�t nastavena pro pou�it� t�to metody.';
  EALFBX_INDEXERROR          = 'Index je mimo rozsah (%d)';
  EALFBX_SIZENAME            = 'Velikost n�zvu je p��li� velk� (%s)';
  EALFBX_MUSTBEPREPARED      = 'Dotaz mus� b�t nejd��ve p�ipraven (prepared).';
  EALFBX_MUSTBEOPEN          = 'The query must be opened first.';
  EALFBX_EXPLICITTRANS       = 'Transaction must be started explicitly.';
  EALFBX_EXCEPTIONNOTFOUND   = 'Exception name %s, not found.';
  EALFBX_EXPTIONREGISTERED   = 'Exception: %d already registered';
  EALFBX_NOAUTOSTOP          = 'Transaction must be closed explicitly.';
  EALFBX_NOGENERATOR         = 'Generator %s not found.';
  EALFBX_NOFIELD             = 'Field not found.';
  EALFBX_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EALFBX_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EALFBX_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EALFBX_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EALFBX_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EALFBX_PARSESETNAMES       = 'Parse error: SET NAMES';
  EALFBX_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EALFBX_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EALFBX_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EALFBX_SERVICESPARSING     = 'Error while parsing Services API output.';

  {$ENDIF ALFBXLANG_CZ}

  {$IFDEF ALFBXLANG_DE}
  sALFBXTrue  = 'Wahr';
  sALFBXFalse = 'Falsch';

  // ALFBX Errors
  EALFBX_INVALIDEIBVERSION   = 'Falsche Version des Datenbankservers. Bitte ueberpr�fen sie die Compileroptionen.';
  EALFBX_CANTLOADLIB         = 'Kann Bibliothek %s nicht laden.';
  EALFBX_DBHANDLEALREADYSET  = 'Datenbank-Handle bereits zugewiesen. Bitte erst Verbindung zur Datenbank trennen.';
  EALFBX_TRANSACTIONNOTDEF   = 'Transaktion nicht zugewiesen.';
  EALFBX_DATABASENOTDEF      = 'Datenbank nicht zugewiesen.';
  EALFBX_QUERYNOTOPEN        = 'Abfrage nicht ge�ffnet.';
  EALFBX_CASTERROR           = 'Fehler bei Typumwandlung.';
  EALFBX_UNEXPECTEDERROR     = 'Unerwarteter Fehler.';
  EALFBX_FIELDNUMNOTFOUND    = 'Feld Nummer %d nicht gefunden.';
  EALFBX_FIELDSTRNOTFOUND    = 'Feld "%s" nicht gefunden.';
  EALFBX_PARAMSTRNOTFOUND    = 'Parameter "%s" nicht gefunden.';
  EALFBX_BLOBFIELDNOTFOUND   = 'Blob-Feld Nummer: %d nicht gefunden.';
  EALFBX_FETCHBLOBNOTSET     = 'Die Eigenschaft FetchBlob muss gesetzt sein um diese Methode zu nutzen.';
  EALFBX_INDEXERROR          = 'Index ausserhalb des g�ltigen Bereichs (%d).';
  EALFBX_SIZENAME            = 'Name ist zu lang (%s).';
  EALFBX_MUSTBEPREPARED      = 'Die Abfrage muss erst vorbereitet werden.';
  EALFBX_MUSTBEOPEN          = 'Die Abfrage muss erst ge�ffnet werden.';
  EALFBX_EXPLICITTRANS       = 'Die Transaktion muss explizit gestartet werden.';
  EALFBX_EXCEPTIONNOTFOUND   = 'Ausnahme "%s" nicht gefunden.';
  EALFBX_EXPTIONREGISTERED   = 'Ausnahme %d bereits registriert.';
  EALFBX_NOAUTOSTOP          = 'Die Transaktion muss explizit beendet werden.';
  EALFBX_NOGENERATOR         = 'Generator %s nicht gefunden.';
  EALFBX_NOFIELD             = 'Feld nicht gefunden.';
  EALFBX_TABLESTRNOTFOUND    = 'Tabelle "%s" nicht gefunden.';
  EALFBX_DOMAINSTRNOTFOUND   = 'Dom�ne %s nicht gefunden.';
  EALFBX_PROCSTRNOTFOUND     = 'Prozedur %s nicht gefunden.';
  EALFBX_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EALFBX_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EALFBX_PARSESETNAMES       = 'Parse error: SET NAMES';
  EALFBX_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EALFBX_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EALFBX_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EALFBX_SERVICESPARSING     = 'Error while parsing Services API output.';

  {$ENDIF ALFBXLANG_DE}

  {$IFDEF ALFBXLANG_RU}
  sALFBXTrue  = '��';
  sALFBXFalse = '���';

  // ALFBX Errors
  EALFBX_INVALIDEIBVERSION   = '������������ ������ ������� ��, ��������� ����� �����������.';
  EALFBX_CANTLOADLIB         = '�� ���� ��������� ���������� DLL: %s.';
  EALFBX_DBHANDLEALREADYSET  = '���������� ���� ������ ��� ����������, ������� ��������� ������� ���������� � ����� ������.';
  EALFBX_TRANSACTIONNOTDEF   = '�������� Transaction �� �����������.';
  EALFBX_DATABASENOTDEF      = '�������� Database �� �����������.';
  EALFBX_QUERYNOTOPEN        = '������ �� ������.';
  EALFBX_CASTERROR           = '������ ���������� ����.';
  EALFBX_UNEXPECTEDERROR     = '����������� ������.';
  EALFBX_FIELDNUMNOTFOUND    = '���� �����: %d �� �������.';
  EALFBX_FIELDSTRNOTFOUND    = '���� "%s" �� �������.';
  EALFBX_PARAMSTRNOTFOUND    = '�������� "%s" �� ������.';
  EALFBX_BLOBFIELDNOTFOUND   = 'Blob ���� �����: %d �� �������.';
  EALFBX_FETCHBLOBNOTSET     = '�������� FetchBlob ������ ���� ����������� ��� ���������� ����� ������.';
  EALFBX_INDEXERROR          = 'Index �� ��������� ���������� �������� (%d)';
  EALFBX_SIZENAME            = '������� ������� �������� (%s)';
  EALFBX_MUSTBEPREPARED      = '���������� ������� ����������� (prepare) ������.';
  EALFBX_MUSTBEOPEN          = '���������� ������� ������� ������.';
  EALFBX_EXPLICITTRANS       = '�� �������� ����� ����������.';
  EALFBX_EXCEPTIONNOTFOUND   = '���������� %s �� �������.';
  EALFBX_EXPTIONREGISTERED   = '����������: %d ��� ����������������';
  EALFBX_NOAUTOSTOP          = 'Transaction must be closed explicitly.';
  EALFBX_NOGENERATOR         = 'Generator %s not found.';
  EALFBX_NOFIELD             = 'Field not found.';
  EALFBX_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EALFBX_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EALFBX_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EALFBX_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EALFBX_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EALFBX_PARSESETNAMES       = 'Parse error: SET NAMES';
  EALFBX_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EALFBX_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EALFBX_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EALFBX_SERVICESPARSING     = 'Error while parsing Services API output.';

  {$ENDIF ALFBXLANG_RU}

  {$IFDEF ALFBXLANG_ES}
  sALFBXTrue             = 'Si';
  sALFBXFalse            = 'No';

  // ALFBX Errors
  EALFBX_INVALIDEIBVERSION   = 'Versi�n incorrecta del Servidor de Base de Datos, verifica las opciones del compilador.';
  EALFBX_CANTLOADLIB         = 'Imposible cargar la DLL: %s.';
  EALFBX_DBHANDLEALREADYSET  = 'El handle de la base de datos est� asignado, primero desconecte la base de datos.';
  EALFBX_TRANSACTIONNOTDEF   = 'La transacci�n no est� asignada.';
  EALFBX_DATABASENOTDEF      = 'La Base de Datos no est� asignada.';
  EALFBX_QUERYNOTOPEN        = 'El query est� cerrado.';
  EALFBX_CASTERROR           = 'Error en conversi�n.';
  EALFBX_UNEXPECTEDERROR     = 'Error inesperado.';
  EALFBX_FIELDNUMNOTFOUND    = 'Campo n�mero: %d no encontrado.';
  EALFBX_FIELDSTRNOTFOUND    = 'Campo "%s" no encontrado.';
  EALFBX_PARAMSTRNOTFOUND    = 'Parametro "%s" no  encontrado.';
  EALFBX_BLOBFIELDNOTFOUND   = 'Campo Blob n�mero: %d no  encontrado.';
  EALFBX_FETCHBLOBNOTSET     = 'La propiedad FetchBlob debe habilitarse para usar est� m�todo';
  EALFBX_INDEXERROR          = '�ndice fuera de l�mite (%d)';
  EALFBX_SIZENAME            = 'Nombre demasiado largo (%s)';
  EALFBX_MUSTBEPREPARED      = 'El query debe prepararse primero.';
  EALFBX_MUSTBEOPEN          = 'El query debe ser abierto primero.';
  EALFBX_EXPLICITTRANS       = 'La transaccisn debe ser iniciada explmcitamente.';
  EALFBX_EXCEPTIONNOTFOUND   = 'Excepcion con nombre %s, no fue encontrada.';
  EALFBX_EXPTIONREGISTERED   = 'Excepcion: %d ya esta registrada.';
  EALFBX_NOAUTOSTOP          = 'La transaccisn debe ser cerrada explmcitamente.';
  EALFBX_NOGENERATOR         = 'Generator %s not found.';
  EALFBX_NOFIELD             = 'Field not found.';
  EALFBX_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EALFBX_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EALFBX_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EALFBX_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EALFBX_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EALFBX_PARSESETNAMES       = 'Parse error: SET NAMES';
  EALFBX_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EALFBX_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EALFBX_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EALFBX_SERVICESPARSING     = 'Error while parsing Services API output.';

  {$ENDIF ALFBXLANG_ES}

  {$IFDEF ALFBXLANG_TR}
  sALFBXTrue  = 'Do�ru';
  sALFBXFalse = 'Yanl��';

  // ALFBX Errors
  EALFBX_INVALIDEIBVERSION   = 'Hatal� Veritaban� Sunucusu s�r�m�, derleyici se�eneklerini kontrol ediniz.';
  EALFBX_CANTLOADLIB         = 'Kitapl�k y�klenemiyor: %s.';
  EALFBX_DBHANDLEALREADYSET  = 'Veritaban� tan�t�c�s� atanm�� durumda, �ncelikle veritaban� ba�lant�s�n� kesiniz.';
  EALFBX_TRANSACTIONNOTDEF   = '��lem grubu atanmam��.';
  EALFBX_DATABASENOTDEF      = 'Veritaban� atanmam��.';
  EALFBX_QUERYNOTOPEN        = 'Sorgu a��k de�il.';
  EALFBX_CASTERROR           = 'Tip atama hatas�.';
  EALFBX_UNEXPECTEDERROR     = 'Beklenmeyen hata.';
  EALFBX_FIELDNUMNOTFOUND    = '%d numaral� saha bulunamad�.';
  EALFBX_FIELDSTRNOTFOUND    = '"%s" sahas� bulunamad�.';
  EALFBX_PARAMSTRNOTFOUND    = '"%s" parametresi bulunamad�.';
  EALFBX_BLOBFIELDNOTFOUND   = '%d numaral� Blob sahas� bulunamad�.';
  EALFBX_FETCHBLOBNOTSET     = 'Bu metodu kullanmak i�in FetchBlob niteli�i ayarlanmal�d�r.';
  EALFBX_INDEXERROR          = 'Indeks s�n�r d���nda (%d)';
  EALFBX_SIZENAME            = 'Ad b�y�kl��� �ok y�ksek (%s)';
  EALFBX_MUSTBEPREPARED      = 'Sorgu �ncelikle haz�rlanmal�d�r.';
  EALFBX_MUSTBEOPEN          = 'Sorgu �ncelikle a��lmal�d�r.';
  EALFBX_EXPLICITTRANS       = '��lem grubu elle ba�lat�lmal�d�r.';
  EALFBX_EXCEPTIONNOTFOUND   = 'Kural d��� nesnesi "%s" bulunamad�.';
  EALFBX_EXPTIONREGISTERED   = 'Kural d���: %d zaten kay�tl�.';
  EALFBX_NOAUTOSTOP          = '��lem grubu elle kapat�lmal�d�r.';
  EALFBX_NOGENERATOR         = '%s �reteci bulunamad�.';
  EALFBX_NOFIELD             = 'Saha bulunamad�.';
  EALFBX_TABLESTRNOTFOUND    = '"%s" tablosu bulunamad�.';
  EALFBX_DOMAINSTRNOTFOUND   = '"%s" etki alan� bulunamad�.';
  EALFBX_PROCSTRNOTFOUND     = '%s yordam� bulunamad�.';
  EALFBX_CACHEDFETCHNOTSET   = 'CachedFetch niteli�i True olarak belirlenmemi�.';
  EALFBX_PARSESQLDIALECT     = 'Ayr��t�rma hatas�: SET SQL DIALECT';
  EALFBX_PARSESETNAMES       = 'Ayr��t�rma hatas�: SET NAMES';
  EALFBX_CHARSETNOTFOUND     = '%s karakter seti bulunamad�.';
  EALFBX_UNEXPECTEDCASTERROR = 'Beklenmeyen tip atama hatas�.';
  EALFBX_INVALIDUSERNAME     = 'Ge�ersiz kullan�c� ad� : "%s".';
  EALFBX_SERVICESPARSING     = 'Services API ��kt�s� ayr��t�r�l�rken hata olu�tu.';

  {$ENDIF ALFBXLANG_TR}

implementation

end.

