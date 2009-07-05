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
  EALFBX_INVALIDEIBVERSION   = 'Version de base de données incorrecte, vérifiez les options de compilation.';
  EALFBX_CANTLOADLIB         = 'Impossible de charger la DLL: %s.';
  EALFBX_DBHANDLEALREADYSET  = 'Le handle de la base de données est déjà défini, déconnectez d''abord la base de données.';
  EALFBX_TRANSACTIONNOTDEF   = 'La transaction n''est pas définie';
  EALFBX_DATABASENOTDEF      = 'La base de données n''est pas définie.';
  EALFBX_QUERYNOTOPEN        = 'La requête n''est pas encore ouverte.';
  EALFBX_CASTERROR           = 'Transtypage incorrect.';
  EALFBX_UNEXPECTEDERROR     = 'Erreur inattendue.';
  EALFBX_FIELDNUMNOTFOUND    = 'Le champ numéro: %d n''existe pas.';
  EALFBX_FIELDSTRNOTFOUND    = 'Le champ "%s" n''existe pas.';
  EALFBX_PARAMSTRNOTFOUND    = 'Le Parametre "%s" n''existe pas.';
  EALFBX_BLOBFIELDNOTFOUND   = 'Le champ Blob numéro: %d n''existe pas.';
  EALFBX_FETCHBLOBNOTSET     = 'La propriété FetchBlob doit être activée pour utiliser cette méthode.';
  EALFBX_INDEXERROR          = 'Indice de liste hors limites (%d)';
  EALFBX_SIZENAME            = 'La taille du nom est trop grande (%s)';
  EALFBX_MUSTBEPREPARED      = 'La requète doit d''abord être préparée.';
  EALFBX_MUSTBEOPEN          = 'La requète doit d''abord être ouverte.';
  EALFBX_EXPLICITTRANS       = 'La transaction doit être démarrée explicitement.';
  EALFBX_EXCEPTIONNOTFOUND   = 'L''exception %s, n''existe pas.';
  EALFBX_EXPTIONREGISTERED   = 'L''exception %d a déjà été enregistrée.';
  EALFBX_NOAUTOSTOP          = 'La transaction doit être fermée explicitement.';
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
  EALFBX_INVALIDEIBVERSION   = 'Nekorektní verze databázového serveru, zkontrolujte nastavení kompileru.';
  EALFBX_CANTLOADLIB         = 'Nelze naèíst knihovnu: %s.';
  EALFBX_DBHANDLEALREADYSET  = 'Kanál databáze je již pøipraven, nejdøíve odpojte databázi.';
  EALFBX_TRANSACTIONNOTDEF   = 'Transakce není pøiøazena.';
  EALFBX_DATABASENOTDEF      = 'Databáze není pøiøazena.';
  EALFBX_QUERYNOTOPEN        = 'Dotaz není otevøen.';
  EALFBX_CASTERROR           = 'Chyba bsazení.';
  EALFBX_UNEXPECTEDERROR     = 'Neznámá chyba.';
  EALFBX_FIELDNUMNOTFOUND    = 'Položka èíslo: %d neexistuje.';
  EALFBX_FIELDSTRNOTFOUND    = 'Položka "%s" neexistuje.';
  EALFBX_PARAMSTRNOTFOUND    = 'Parametr "%s" neexistuje.';
  EALFBX_BLOBFIELDNOTFOUND   = 'Blob záznam èíslo: %d neexistuje.';
  EALFBX_FETCHBLOBNOTSET     = 'Vlastnost FetchBlob musí být nastavena pro použití této metody.';
  EALFBX_INDEXERROR          = 'Index je mimo rozsah (%d)';
  EALFBX_SIZENAME            = 'Velikost názvu je pøíliš velká (%s)';
  EALFBX_MUSTBEPREPARED      = 'Dotaz musí být nejdøíve pøipraven (prepared).';
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
  EALFBX_INVALIDEIBVERSION   = 'Falsche Version des Datenbankservers. Bitte ueberprüfen sie die Compileroptionen.';
  EALFBX_CANTLOADLIB         = 'Kann Bibliothek %s nicht laden.';
  EALFBX_DBHANDLEALREADYSET  = 'Datenbank-Handle bereits zugewiesen. Bitte erst Verbindung zur Datenbank trennen.';
  EALFBX_TRANSACTIONNOTDEF   = 'Transaktion nicht zugewiesen.';
  EALFBX_DATABASENOTDEF      = 'Datenbank nicht zugewiesen.';
  EALFBX_QUERYNOTOPEN        = 'Abfrage nicht geöffnet.';
  EALFBX_CASTERROR           = 'Fehler bei Typumwandlung.';
  EALFBX_UNEXPECTEDERROR     = 'Unerwarteter Fehler.';
  EALFBX_FIELDNUMNOTFOUND    = 'Feld Nummer %d nicht gefunden.';
  EALFBX_FIELDSTRNOTFOUND    = 'Feld "%s" nicht gefunden.';
  EALFBX_PARAMSTRNOTFOUND    = 'Parameter "%s" nicht gefunden.';
  EALFBX_BLOBFIELDNOTFOUND   = 'Blob-Feld Nummer: %d nicht gefunden.';
  EALFBX_FETCHBLOBNOTSET     = 'Die Eigenschaft FetchBlob muss gesetzt sein um diese Methode zu nutzen.';
  EALFBX_INDEXERROR          = 'Index ausserhalb des gültigen Bereichs (%d).';
  EALFBX_SIZENAME            = 'Name ist zu lang (%s).';
  EALFBX_MUSTBEPREPARED      = 'Die Abfrage muss erst vorbereitet werden.';
  EALFBX_MUSTBEOPEN          = 'Die Abfrage muss erst geöffnet werden.';
  EALFBX_EXPLICITTRANS       = 'Die Transaktion muss explizit gestartet werden.';
  EALFBX_EXCEPTIONNOTFOUND   = 'Ausnahme "%s" nicht gefunden.';
  EALFBX_EXPTIONREGISTERED   = 'Ausnahme %d bereits registriert.';
  EALFBX_NOAUTOSTOP          = 'Die Transaktion muss explizit beendet werden.';
  EALFBX_NOGENERATOR         = 'Generator %s nicht gefunden.';
  EALFBX_NOFIELD             = 'Feld nicht gefunden.';
  EALFBX_TABLESTRNOTFOUND    = 'Tabelle "%s" nicht gefunden.';
  EALFBX_DOMAINSTRNOTFOUND   = 'Domäne %s nicht gefunden.';
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
  sALFBXTrue  = 'Äà';
  sALFBXFalse = 'Íåò';

  // ALFBX Errors
  EALFBX_INVALIDEIBVERSION   = 'Íåêîððåêòíàÿ âåðñèÿ ñåðâåðà ÁÄ, ïðîâåðüòå îïöèè êîìïèëÿòîðà.';
  EALFBX_CANTLOADLIB         = 'Íå ìîãó çàãðóçèòü áèáëèîòåêó DLL: %s.';
  EALFBX_DBHANDLEALREADYSET  = 'Äåñêðèïòîð áàçû äàííûõ óæå óñòàíîâëåí, ñíà÷àëà ðàçîðâèòå òåêóùåå ñîåäèíåíèå ñ áàçîé äàííûõ.';
  EALFBX_TRANSACTIONNOTDEF   = 'Ñâîéñòâî Transaction íå óñòàíîâëåíî.';
  EALFBX_DATABASENOTDEF      = 'Ñâîéñòâî Database íå óñòàíîâëåíî.';
  EALFBX_QUERYNOTOPEN        = 'Çàïðîñ íå îòêðûò.';
  EALFBX_CASTERROR           = 'Îøèáêà ïðèâåäåíèÿ òèïà.';
  EALFBX_UNEXPECTEDERROR     = 'Íåèçâåñòíàÿ îøèáêà.';
  EALFBX_FIELDNUMNOTFOUND    = 'Ïîëå íîìåð: %d íå íàéäåíî.';
  EALFBX_FIELDSTRNOTFOUND    = 'Ïîëå "%s" íå íàéäåíî.';
  EALFBX_PARAMSTRNOTFOUND    = 'Ïàðàìåòð "%s" íå íàéäåí.';
  EALFBX_BLOBFIELDNOTFOUND   = 'Blob ïîëå íîìåð: %d íå íàéäåíî.';
  EALFBX_FETCHBLOBNOTSET     = 'Ñâîéñòâî FetchBlob äîëæíî áûòü óñòàíîâëåíî äëÿ âûïîëíåíèÿ ýòîãî ìåòîäà.';
  EALFBX_INDEXERROR          = 'Index çà ïðåäåëàìè äîïóñòèìûõ çíà÷åíèé (%d)';
  EALFBX_SIZENAME            = 'Ñëèøêîì äëèííîå íàçâàíèå (%s)';
  EALFBX_MUSTBEPREPARED      = 'Íåîáõîäèìî ñíà÷àëà ïîäãîòîâèòü (prepare) çàïðîñ.';
  EALFBX_MUSTBEOPEN          = 'Íåîáõîäèìî ñíà÷àëà îòêðûòü çàïðîñ.';
  EALFBX_EXPLICITTRANS       = 'Íå âûïîëíåí ñòàðò òðàíçàêöèè.';
  EALFBX_EXCEPTIONNOTFOUND   = 'Èñêëþ÷åíèå %s íå íàéäåíî.';
  EALFBX_EXPTIONREGISTERED   = 'Èñêëþ÷åíèå: %d óæå çàðåãèñòðèðîâàíî';
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
  EALFBX_INVALIDEIBVERSION   = 'Versión incorrecta del Servidor de Base de Datos, verifica las opciones del compilador.';
  EALFBX_CANTLOADLIB         = 'Imposible cargar la DLL: %s.';
  EALFBX_DBHANDLEALREADYSET  = 'El handle de la base de datos está asignado, primero desconecte la base de datos.';
  EALFBX_TRANSACTIONNOTDEF   = 'La transacción no está asignada.';
  EALFBX_DATABASENOTDEF      = 'La Base de Datos no está asignada.';
  EALFBX_QUERYNOTOPEN        = 'El query está cerrado.';
  EALFBX_CASTERROR           = 'Error en conversión.';
  EALFBX_UNEXPECTEDERROR     = 'Error inesperado.';
  EALFBX_FIELDNUMNOTFOUND    = 'Campo número: %d no encontrado.';
  EALFBX_FIELDSTRNOTFOUND    = 'Campo "%s" no encontrado.';
  EALFBX_PARAMSTRNOTFOUND    = 'Parametro "%s" no  encontrado.';
  EALFBX_BLOBFIELDNOTFOUND   = 'Campo Blob número: %d no  encontrado.';
  EALFBX_FETCHBLOBNOTSET     = 'La propiedad FetchBlob debe habilitarse para usar esté método';
  EALFBX_INDEXERROR          = 'Índice fuera de límite (%d)';
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
  sALFBXTrue  = 'Doðru';
  sALFBXFalse = 'Yanlýþ';

  // ALFBX Errors
  EALFBX_INVALIDEIBVERSION   = 'Hatalý Veritabaný Sunucusu sürümü, derleyici seçeneklerini kontrol ediniz.';
  EALFBX_CANTLOADLIB         = 'Kitaplýk yüklenemiyor: %s.';
  EALFBX_DBHANDLEALREADYSET  = 'Veritabaný tanýtýcýsý atanmýþ durumda, öncelikle veritabaný baðlantýsýný kesiniz.';
  EALFBX_TRANSACTIONNOTDEF   = 'Ýþlem grubu atanmamýþ.';
  EALFBX_DATABASENOTDEF      = 'Veritabaný atanmamýþ.';
  EALFBX_QUERYNOTOPEN        = 'Sorgu açýk deðil.';
  EALFBX_CASTERROR           = 'Tip atama hatasý.';
  EALFBX_UNEXPECTEDERROR     = 'Beklenmeyen hata.';
  EALFBX_FIELDNUMNOTFOUND    = '%d numaralý saha bulunamadý.';
  EALFBX_FIELDSTRNOTFOUND    = '"%s" sahasý bulunamadý.';
  EALFBX_PARAMSTRNOTFOUND    = '"%s" parametresi bulunamadý.';
  EALFBX_BLOBFIELDNOTFOUND   = '%d numaralý Blob sahasý bulunamadý.';
  EALFBX_FETCHBLOBNOTSET     = 'Bu metodu kullanmak için FetchBlob niteliði ayarlanmalýdýr.';
  EALFBX_INDEXERROR          = 'Indeks sýnýr dýþýnda (%d)';
  EALFBX_SIZENAME            = 'Ad büyüklüðü çok yüksek (%s)';
  EALFBX_MUSTBEPREPARED      = 'Sorgu öncelikle hazýrlanmalýdýr.';
  EALFBX_MUSTBEOPEN          = 'Sorgu öncelikle açýlmalýdýr.';
  EALFBX_EXPLICITTRANS       = 'Ýþlem grubu elle baþlatýlmalýdýr.';
  EALFBX_EXCEPTIONNOTFOUND   = 'Kural dýþý nesnesi "%s" bulunamadý.';
  EALFBX_EXPTIONREGISTERED   = 'Kural dýþý: %d zaten kayýtlý.';
  EALFBX_NOAUTOSTOP          = 'Ýþlem grubu elle kapatýlmalýdýr.';
  EALFBX_NOGENERATOR         = '%s üreteci bulunamadý.';
  EALFBX_NOFIELD             = 'Saha bulunamadý.';
  EALFBX_TABLESTRNOTFOUND    = '"%s" tablosu bulunamadý.';
  EALFBX_DOMAINSTRNOTFOUND   = '"%s" etki alaný bulunamadý.';
  EALFBX_PROCSTRNOTFOUND     = '%s yordamý bulunamadý.';
  EALFBX_CACHEDFETCHNOTSET   = 'CachedFetch niteliði True olarak belirlenmemiþ.';
  EALFBX_PARSESQLDIALECT     = 'Ayrýþtýrma hatasý: SET SQL DIALECT';
  EALFBX_PARSESETNAMES       = 'Ayrýþtýrma hatasý: SET NAMES';
  EALFBX_CHARSETNOTFOUND     = '%s karakter seti bulunamadý.';
  EALFBX_UNEXPECTEDCASTERROR = 'Beklenmeyen tip atama hatasý.';
  EALFBX_INVALIDUSERNAME     = 'Geçersiz kullanýcý adý : "%s".';
  EALFBX_SERVICESPARSING     = 'Services API çýktýsý ayrýþtýrýlýrken hata oluþtu.';

  {$ENDIF ALFBXLANG_TR}

implementation

end.

