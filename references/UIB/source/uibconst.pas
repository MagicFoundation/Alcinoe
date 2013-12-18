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
(* contributor: Olivier Guilbaud <oguilb@free.fr>                               *)
(*                                                                              *)
(********************************************************************************)

{$I uib.inc}

unit uibconst;

interface

{$IFNDEF DELPHI6_UP}
{$IFNDEF BCB}
const
  S_OK    = $00000000;
  S_FALSE = $00000001;
{$ENDIF BCB}
{$ENDIF DELPHI6_UP}


type
  // uib Server Commands
  TServerCommand = (scGetClassObject, scInvokeMethod);

  // Metadata Object Identifiers
  TOIDDatabase = (OIDDomain, OIDTable, OIDView, OIDProcedure, OIDGenerator,
    OIDException, OIDUDF, OIDRole, OIDDBCharset, OIDDependencies);
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
    OIDException, OIDUDF, OIDRole, OIDDBCharset, OIDDependencies];
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
  {$IFDEF UIBLANG_EN}
  sUIBTrue  = 'True';
  sUIBFalse = 'False';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Incorrect Database Server version, check compiler options.';
  EUIB_CANTLOADLIB         = 'Can''t load library: %s.';
  EUIB_DBHANDLEALREADYSET  = 'Database handle already assigned, first disconnect database.';
  EUIB_TRANSACTIONNOTDEF   = 'Transaction not assigned.';
  EUIB_DATABASENOTDEF      = 'Database not assigned.';
  EUIB_QUERYNOTOPEN        = 'Query not open.';
  EUIB_CASTERROR           = 'Cast error.';
  EUIB_UNEXPECTEDERROR     = 'Unexpected error.';
  EUIB_FIELDNUMNOTFOUND    = 'Field num: %d not found.';
  EUIB_FIELDSTRNOTFOUND    = 'Field "%s" not found.';
  EUIB_PARAMSTRNOTFOUND    = 'Parameter "%s" not found.';
  EUIB_BLOBFIELDNOTFOUND   = 'Blob field num: %d not found.';
  EUIB_FETCHBLOBNOTSET     = 'FetchBlob property must be set to use this method.';
  EUIB_INDEXERROR          = 'Index out of bound (%d)';
  EUIB_SIZENAME            = 'Size name too big (%s)';
  EUIB_MUSTBEPREPARED      = 'The query must be prepared first.';
  EUIB_MUSTBEOPEN          = 'The query must be opened first.';
  EUIB_EXPLICITTRANS       = 'Transaction must be started explicitly.';
  EUIB_EXCEPTIONNOTFOUND   = 'Exception name %s, not found.';
  EUIB_EXPTIONREGISTERED   = 'Exception: %d already registered';
  EUIB_NOAUTOSTOP          = 'Transaction must be closed explicitly.';
  EUIB_NOGENERATOR         = 'Generator %s not found.';
  EUIB_NOFIELD             = 'Field not found.';
  EUIB_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EUIB_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EUIB_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EUIB_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Parse error: SET NAMES';
  EUIB_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EUIB_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EUIB_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EUIB_SERVICESPARSING     = 'Error while parsing Services API output.';
  EUIB_NOT_NULLABLE        = 'Field "%s" is not nullable.';
  {$ENDIF UIBLANG_EN}

  {$IFDEF UIBLANG_FR}
  sUIBTrue  = 'Vrai';
  sUIBFalse = 'Faux';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Version de base de données incorrecte, vérifiez les options de compilation.';
  EUIB_CANTLOADLIB         = 'Impossible de charger la DLL: %s.';
  EUIB_DBHANDLEALREADYSET  = 'Le handle de la base de données est déjà défini, déconnectez d''abord la base de données.';
  EUIB_TRANSACTIONNOTDEF   = 'La transaction n''est pas définie';
  EUIB_DATABASENOTDEF      = 'La base de données n''est pas définie.';
  EUIB_QUERYNOTOPEN        = 'La requête n''est pas encore ouverte.';
  EUIB_CASTERROR           = 'Transtypage incorrect.';
  EUIB_UNEXPECTEDERROR     = 'Erreur inattendue.';
  EUIB_FIELDNUMNOTFOUND    = 'Le champ numéro: %d n''existe pas.';
  EUIB_FIELDSTRNOTFOUND    = 'Le champ "%s" n''existe pas.';
  EUIB_PARAMSTRNOTFOUND    = 'Le Parametre "%s" n''existe pas.';
  EUIB_BLOBFIELDNOTFOUND   = 'Le champ Blob numéro: %d n''existe pas.';
  EUIB_FETCHBLOBNOTSET     = 'La propriété FetchBlob doit être activée pour utiliser cette méthode.';
  EUIB_INDEXERROR          = 'Indice de liste hors limites (%d)';
  EUIB_SIZENAME            = 'La taille du nom est trop grande (%s)';
  EUIB_MUSTBEPREPARED      = 'La requète doit d''abord être préparée.';
  EUIB_MUSTBEOPEN          = 'La requète doit d''abord être ouverte.';
  EUIB_EXPLICITTRANS       = 'La transaction doit être démarrée explicitement.';
  EUIB_EXCEPTIONNOTFOUND   = 'L''exception %s, n''existe pas.';
  EUIB_EXPTIONREGISTERED   = 'L''exception %d a déjà été enregistrée.';
  EUIB_NOAUTOSTOP          = 'La transaction doit être fermée explicitement.';
  EUIB_NOGENERATOR         = 'Generator %s not found.';
  EUIB_NOFIELD             = 'Field not found.';
  EUIB_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EUIB_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EUIB_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EUIB_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Parse error: SET NAMES';
  EUIB_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EUIB_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EUIB_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EUIB_SERVICESPARSING     = 'Error while parsing Services API output.';
  EUIB_NOT_NULLABLE        = 'Field "%s" is not nullable.';

  {$ENDIF UIBLANG_FR}

  {$IFDEF UIBLANG_CZ}
  sUIBTrue  = 'Ano';
  sUIBFalse = 'Ne';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Nekorektní verze databázového serveru, zkontrolujte nastavení kompileru.';
  EUIB_CANTLOADLIB         = 'Nelze naèíst knihovnu: %s.';
  EUIB_DBHANDLEALREADYSET  = 'Kanál databáze je již pøipraven, nejdøíve odpojte databázi.';
  EUIB_TRANSACTIONNOTDEF   = 'Transakce není pøiøazena.';
  EUIB_DATABASENOTDEF      = 'Databáze není pøiøazena.';
  EUIB_QUERYNOTOPEN        = 'Dotaz není otevøen.';
  EUIB_CASTERROR           = 'Chyba bsazení.';
  EUIB_UNEXPECTEDERROR     = 'Neznámá chyba.';
  EUIB_FIELDNUMNOTFOUND    = 'Položka èíslo: %d neexistuje.';
  EUIB_FIELDSTRNOTFOUND    = 'Položka "%s" neexistuje.';
  EUIB_PARAMSTRNOTFOUND    = 'Parametr "%s" neexistuje.';
  EUIB_BLOBFIELDNOTFOUND   = 'Blob záznam èíslo: %d neexistuje.';
  EUIB_FETCHBLOBNOTSET     = 'Vlastnost FetchBlob musí být nastavena pro použití této metody.';
  EUIB_INDEXERROR          = 'Index je mimo rozsah (%d)';
  EUIB_SIZENAME            = 'Velikost názvu je pøíliš velká (%s)';
  EUIB_MUSTBEPREPARED      = 'Dotaz musí být nejdøíve pøipraven (prepared).';
  EUIB_MUSTBEOPEN          = 'The query must be opened first.';
  EUIB_EXPLICITTRANS       = 'Transaction must be started explicitly.';
  EUIB_EXCEPTIONNOTFOUND   = 'Exception name %s, not found.';
  EUIB_EXPTIONREGISTERED   = 'Exception: %d already registered';
  EUIB_NOAUTOSTOP          = 'Transaction must be closed explicitly.';
  EUIB_NOGENERATOR         = 'Generator %s not found.';
  EUIB_NOFIELD             = 'Field not found.';
  EUIB_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EUIB_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EUIB_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EUIB_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Parse error: SET NAMES';
  EUIB_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EUIB_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EUIB_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EUIB_SERVICESPARSING     = 'Error while parsing Services API output.';
  EUIB_NOT_NULLABLE        = 'Field "%s" is not nullable.';

  {$ENDIF UIBLANG_CZ}

  {$IFDEF UIBLANG_DE}
  sUIBTrue  = 'Wahr';
  sUIBFalse = 'Falsch';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Falsche Version des Datenbankservers. Bitte ueberprüfen sie die Compileroptionen.';
  EUIB_CANTLOADLIB         = 'Kann Bibliothek %s nicht laden.';
  EUIB_DBHANDLEALREADYSET  = 'Datenbank-Handle bereits zugewiesen. Bitte erst Verbindung zur Datenbank trennen.';
  EUIB_TRANSACTIONNOTDEF   = 'Transaktion nicht zugewiesen.';
  EUIB_DATABASENOTDEF      = 'Datenbank nicht zugewiesen.';
  EUIB_QUERYNOTOPEN        = 'Abfrage nicht geöffnet.';
  EUIB_CASTERROR           = 'Fehler bei Typumwandlung.';
  EUIB_UNEXPECTEDERROR     = 'Unerwarteter Fehler.';
  EUIB_FIELDNUMNOTFOUND    = 'Feld Nummer %d nicht gefunden.';
  EUIB_FIELDSTRNOTFOUND    = 'Feld "%s" nicht gefunden.';
  EUIB_PARAMSTRNOTFOUND    = 'Parameter "%s" nicht gefunden.';
  EUIB_BLOBFIELDNOTFOUND   = 'Blob-Feld Nummer: %d nicht gefunden.';
  EUIB_FETCHBLOBNOTSET     = 'Die Eigenschaft FetchBlob muss gesetzt sein um diese Methode zu nutzen.';
  EUIB_INDEXERROR          = 'Index ausserhalb des gültigen Bereichs (%d).';
  EUIB_SIZENAME            = 'Name ist zu lang (%s).';
  EUIB_MUSTBEPREPARED      = 'Die Abfrage muss erst vorbereitet werden.';
  EUIB_MUSTBEOPEN          = 'Die Abfrage muss erst geöffnet werden.';
  EUIB_EXPLICITTRANS       = 'Die Transaktion muss explizit gestartet werden.';
  EUIB_EXCEPTIONNOTFOUND   = 'Ausnahme "%s" nicht gefunden.';
  EUIB_EXPTIONREGISTERED   = 'Ausnahme %d bereits registriert.';
  EUIB_NOAUTOSTOP          = 'Die Transaktion muss explizit beendet werden.';
  EUIB_NOGENERATOR         = 'Generator %s nicht gefunden.';
  EUIB_NOFIELD             = 'Feld nicht gefunden.';
  EUIB_TABLESTRNOTFOUND    = 'Tabelle "%s" nicht gefunden.';
  EUIB_DOMAINSTRNOTFOUND   = 'Domäne %s nicht gefunden.';
  EUIB_PROCSTRNOTFOUND     = 'Prozedur %s nicht gefunden.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EUIB_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Parse error: SET NAMES';
  EUIB_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EUIB_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EUIB_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EUIB_SERVICESPARSING     = 'Error while parsing Services API output.';
  EUIB_NOT_NULLABLE        = 'Field "%s" is not nullable.';

  {$ENDIF UIBLANG_DE}

  {$IFDEF UIBLANG_RU}
  sUIBTrue  = 'Äà';
  sUIBFalse = 'Íåò';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Íåêîððåêòíàÿ âåðñèÿ ñåðâåðà ÁÄ, ïðîâåðüòå îïöèè êîìïèëÿòîðà.';
  EUIB_CANTLOADLIB         = 'Íå ìîãó çàãðóçèòü áèáëèîòåêó DLL: %s.';
  EUIB_DBHANDLEALREADYSET  = 'Äåñêðèïòîð áàçû äàííûõ óæå óñòàíîâëåí, ñíà÷àëà ðàçîðâèòå òåêóùåå ñîåäèíåíèå ñ áàçîé äàííûõ.';
  EUIB_TRANSACTIONNOTDEF   = 'Ñâîéñòâî Transaction íå óñòàíîâëåíî.';
  EUIB_DATABASENOTDEF      = 'Ñâîéñòâî Database íå óñòàíîâëåíî.';
  EUIB_QUERYNOTOPEN        = 'Çàïðîñ íå îòêðûò.';
  EUIB_CASTERROR           = 'Îøèáêà ïðèâåäåíèÿ òèïà.';
  EUIB_UNEXPECTEDERROR     = 'Íåèçâåñòíàÿ îøèáêà.';
  EUIB_FIELDNUMNOTFOUND    = 'Ïîëå íîìåð: %d íå íàéäåíî.';
  EUIB_FIELDSTRNOTFOUND    = 'Ïîëå "%s" íå íàéäåíî.';
  EUIB_PARAMSTRNOTFOUND    = 'Ïàðàìåòð "%s" íå íàéäåí.';
  EUIB_BLOBFIELDNOTFOUND   = 'Blob ïîëå íîìåð: %d íå íàéäåíî.';
  EUIB_FETCHBLOBNOTSET     = 'Ñâîéñòâî FetchBlob äîëæíî áûòü óñòàíîâëåíî äëÿ âûïîëíåíèÿ ýòîãî ìåòîäà.';
  EUIB_INDEXERROR          = 'Index çà ïðåäåëàìè äîïóñòèìûõ çíà÷åíèé (%d)';
  EUIB_SIZENAME            = 'Ñëèøêîì äëèííîå íàçâàíèå (%s)';
  EUIB_MUSTBEPREPARED      = 'Íåîáõîäèìî ñíà÷àëà ïîäãîòîâèòü (prepare) çàïðîñ.';
  EUIB_MUSTBEOPEN          = 'Íåîáõîäèìî ñíà÷àëà îòêðûòü çàïðîñ.';
  EUIB_EXPLICITTRANS       = 'Íå âûïîëíåí ñòàðò òðàíçàêöèè.';
  EUIB_EXCEPTIONNOTFOUND   = 'Èñêëþ÷åíèå %s íå íàéäåíî.';
  EUIB_EXPTIONREGISTERED   = 'Èñêëþ÷åíèå: %d óæå çàðåãèñòðèðîâàíî';
  EUIB_NOAUTOSTOP          = 'Transaction must be closed explicitly.';
  EUIB_NOGENERATOR         = 'Generator %s not found.';
  EUIB_NOFIELD             = 'Field not found.';
  EUIB_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EUIB_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EUIB_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EUIB_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Parse error: SET NAMES';
  EUIB_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EUIB_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EUIB_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EUIB_SERVICESPARSING     = 'Error while parsing Services API output.';
  EUIB_NOT_NULLABLE        = 'Field "%s" is not nullable.';

  {$ENDIF UIBLANG_RU}

  {$IFDEF UIBLANG_ES}
  sUIBTrue             = 'Si';
  sUIBFalse            = 'No';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Versión incorrecta del Servidor de Base de Datos, verifica las opciones del compilador.';
  EUIB_CANTLOADLIB         = 'Imposible cargar la DLL: %s.';
  EUIB_DBHANDLEALREADYSET  = 'El handle de la base de datos está asignado, primero desconecte la base de datos.';
  EUIB_TRANSACTIONNOTDEF   = 'La transacción no está asignada.';
  EUIB_DATABASENOTDEF      = 'La Base de Datos no está asignada.';
  EUIB_QUERYNOTOPEN        = 'El query está cerrado.';
  EUIB_CASTERROR           = 'Error en conversión.';
  EUIB_UNEXPECTEDERROR     = 'Error inesperado.';
  EUIB_FIELDNUMNOTFOUND    = 'Campo número: %d no encontrado.';
  EUIB_FIELDSTRNOTFOUND    = 'Campo "%s" no encontrado.';
  EUIB_PARAMSTRNOTFOUND    = 'Parametro "%s" no  encontrado.';
  EUIB_BLOBFIELDNOTFOUND   = 'Campo Blob número: %d no  encontrado.';
  EUIB_FETCHBLOBNOTSET     = 'La propiedad FetchBlob debe habilitarse para usar esté método';
  EUIB_INDEXERROR          = 'Índice fuera de límite (%d)';
  EUIB_SIZENAME            = 'Nombre demasiado largo (%s)';
  EUIB_MUSTBEPREPARED      = 'El query debe prepararse primero.';
  EUIB_MUSTBEOPEN          = 'El query debe ser abierto primero.';
  EUIB_EXPLICITTRANS       = 'La transaccisn debe ser iniciada explmcitamente.';
  EUIB_EXCEPTIONNOTFOUND   = 'Excepcion con nombre %s, no fue encontrada.';
  EUIB_EXPTIONREGISTERED   = 'Excepcion: %d ya esta registrada.';
  EUIB_NOAUTOSTOP          = 'La transaccisn debe ser cerrada explmcitamente.';
  EUIB_NOGENERATOR         = 'Generator %s not found.';
  EUIB_NOFIELD             = 'Field not found.';
  EUIB_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EUIB_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EUIB_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EUIB_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Parse error: SET NAMES';
  EUIB_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EUIB_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EUIB_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EUIB_SERVICESPARSING     = 'Error while parsing Services API output.';
  EUIB_NOT_NULLABLE        = 'Field "%s" is not nullable.';

  {$ENDIF UIBLANG_ES}

  {$IFDEF UIBLANG_TR}
  sUIBTrue  = 'Doðru';
  sUIBFalse = 'Yanlýþ';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Hatalý Veritabaný Sunucusu sürümü, derleyici seçeneklerini kontrol ediniz.';
  EUIB_CANTLOADLIB         = 'Kitaplýk yüklenemiyor: %s.';
  EUIB_DBHANDLEALREADYSET  = 'Veritabaný tanýtýcýsý atanmýþ durumda, öncelikle veritabaný baðlantýsýný kesiniz.';
  EUIB_TRANSACTIONNOTDEF   = 'Ýþlem grubu atanmamýþ.';
  EUIB_DATABASENOTDEF      = 'Veritabaný atanmamýþ.';
  EUIB_QUERYNOTOPEN        = 'Sorgu açýk deðil.';
  EUIB_CASTERROR           = 'Tip atama hatasý.';
  EUIB_UNEXPECTEDERROR     = 'Beklenmeyen hata.';
  EUIB_FIELDNUMNOTFOUND    = '%d numaralý saha bulunamadý.';
  EUIB_FIELDSTRNOTFOUND    = '"%s" sahasý bulunamadý.';
  EUIB_PARAMSTRNOTFOUND    = '"%s" parametresi bulunamadý.';
  EUIB_BLOBFIELDNOTFOUND   = '%d numaralý Blob sahasý bulunamadý.';
  EUIB_FETCHBLOBNOTSET     = 'Bu metodu kullanmak için FetchBlob niteliði ayarlanmalýdýr.';
  EUIB_INDEXERROR          = 'Indeks sýnýr dýþýnda (%d)';
  EUIB_SIZENAME            = 'Ad büyüklüðü çok yüksek (%s)';
  EUIB_MUSTBEPREPARED      = 'Sorgu öncelikle hazýrlanmalýdýr.';
  EUIB_MUSTBEOPEN          = 'Sorgu öncelikle açýlmalýdýr.';
  EUIB_EXPLICITTRANS       = 'Ýþlem grubu elle baþlatýlmalýdýr.';
  EUIB_EXCEPTIONNOTFOUND   = 'Kural dýþý nesnesi "%s" bulunamadý.';
  EUIB_EXPTIONREGISTERED   = 'Kural dýþý: %d zaten kayýtlý.';
  EUIB_NOAUTOSTOP          = 'Ýþlem grubu elle kapatýlmalýdýr.';
  EUIB_NOGENERATOR         = '%s üreteci bulunamadý.';
  EUIB_NOFIELD             = 'Saha bulunamadý.';
  EUIB_TABLESTRNOTFOUND    = '"%s" tablosu bulunamadý.';
  EUIB_DOMAINSTRNOTFOUND   = '"%s" etki alaný bulunamadý.';
  EUIB_PROCSTRNOTFOUND     = '%s yordamý bulunamadý.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch niteliði True olarak belirlenmemiþ.';
  EUIB_PARSESQLDIALECT     = 'Ayrýþtýrma hatasý: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Ayrýþtýrma hatasý: SET NAMES';
  EUIB_CHARSETNOTFOUND     = '%s karakter seti bulunamadý.';
  EUIB_UNEXPECTEDCASTERROR = 'Beklenmeyen tip atama hatasý.';
  EUIB_INVALIDUSERNAME     = 'Geçersiz kullanýcý adý : "%s".';
  EUIB_SERVICESPARSING     = 'Services API çýktýsý ayrýþtýrýlýrken hata oluþtu.';
  EUIB_NOT_NULLABLE        = 'Field "%s" is not nullable.';

  {$ENDIF UIBLANG_TR}

implementation

end.
