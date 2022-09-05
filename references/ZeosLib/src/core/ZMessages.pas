{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Constant messages used by Zeos              }
{                                                         }
{ This unit contains all the messages that are output by  }
{ ZEOS methods. One of the given language can be activated}
{ by setting the language in ->                           }
{ ZEOS.inc (e.g.: $DEFINE GERMAN).                        }
{ If no language is defined english will be used.         }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZMessages;

interface

{$I ZCore.inc}

resourcestring

// -> ms, 09/05/2005
{$IFDEF PORTUGUESE}
  SSQLError1 = 'Erro SQL: %s';
  SSQLError2 = 'Erro SQL: %s C�digo: %d';
  SSQLError3 = 'Erro SQL: %s C�digo: %d SQL: %s';
  SSQLError4 = 'Erro SQL: %s C�digo: %d Mensagem: %s';

  SListCapacityError = 'Capacidade da Lista fora do limite (%d)';
  SListCountError = 'Contagem da Lista fora do limite (%d)';
  SListIndexError = '�ndice da Lista fora do limite (%d)';

  SClonningIsNotSupported = 'Clonagem n�o � suportada por esta classe';
  SImmutableOpIsNotAllowed = 'A opera��o n�o � permitida para cole��o imut�vel';
  SStackIsEmpty = 'Pilha est� vazia';
  SVariableWasNotFound = 'Vari�vel "%s" n�o foi encontrada';
  SFunctionWasNotFound = 'Function "%s" n�o foi encontrada';
  SInternalError = 'Erro interno';
  SSyntaxErrorNear = 'Erro de sintaxe pr�ximo a "%s"';
  SSyntaxError = 'Erro de sintaxe';
  SUnknownSymbol = 'S�mbolo desconhecido "%s"';
  SUnexpectedExprEnd = 'Final inesperado de express�o';
  SRightBraceExpected = ') esperado';
  SParametersError = 'Esperado %d par�metros mas foi encontrado %d';
  SExpectedMoreParams = 'Esperado mais que 2 par�metros';
  SInvalidVarByteArray = 'VarByte array inv�lido';
  SVariableAlreadyExists = 'Vari�vel "%s" j� existe';
  STypesMismatch = 'Tipos n�o combinam';
  SUnsupportedVariantType = 'Tipo variante n�o suportado';
  SUnsupportedOperation = 'Opera��o n�o suportada';

  STokenizerIsNotDefined = 'Sinalizador n�o definido';
  SLibraryNotFound = 'Nenhuma biblioteca din�mica da lista %s foi encontrada';
  SEncodeDateIsNotSupported = 'Esta vers�o n�o suporta isc_encode_sql_date';
  SEncodeTimeIsNotSupported = 'Esta vers�o n�o suporta supported isc_encode_sql_time';
  SEncodeTimestampIsNotSupported = 'Esta vers�o n�o suporta supported isc_encode_sql_timestamp';
  SDecodeDateIsNotSupported = 'Esta vers�o n�o suporta isc_decode_sql_date';
  SDecodeTimeIsNotSupported = 'Esta vers�o n�o suporta isc_decode_sql_time';
  SDecodeTimestampIsNotSupported = 'Esta vers�o n�o suporta isc_decode_sql_timestamp';

  SCanNotRetrieveResultSetData = 'N�o foi poss�vel obter os dados do ResultSet';
  SRowBufferIsNotAssigned = 'Buffer da Linha n�o atribu�do';
  SColumnIsNotAccessable = 'Coluna com �ndice %d n�o � acess�vel';
  SConvertionIsNotPossible = 'A convers�o da coluna %d de %s para %s n�o � poss�vel';
  SCanNotAccessBlobRecord = 'N�o � poss�vel acessar um registro BLOB na coluna %d com o tipo %s';
  SRowDataIsNotAvailable = 'Dados na Linha n�o dispon�veis';
  SResolverIsNotSpecified = 'Resolver n�o foi especificado para este ResultSet';
  SResultsetIsAlreadyOpened = 'ResultSet j� est� aberto';
  SCanNotUpdateEmptyRow = 'N�o � poss�vel atualizar uma linha vazia';
  SCanNotUpdateDeletedRow = 'N�o � poss�vel atualizar uma linha apagada';
  SCanNotDeleteEmptyRow = 'N�o � poss�vel apagar uma linha vazia';
  SCannotUseCommit = 'Voc� n�o pode usar Commit no modo AutoCommit';
  SCannotUseRollBack = 'Voc� n�o pode usar Rollback no modo AutoCommit';
  SCanNotUpdateComplexQuery = 'N�o � poss�vel atualizar uma query complexa com mais de uma tabela';
  SCanNotUpdateThisQueryType = 'N�o � poss�vel atualizar este tipo de query';
  SDriverWasNotFound = 'O driver de banco de dados requisitado n�o foi encontrado';
  SCanNotConnectToServer = 'N�o foi poss�vel conectar ao servidor SQL';
  STableIsNotSpecified = 'Tabela n�o especificada';
  SLiveResultSetsAreNotSupported = 'Live query n�o � suportado por esta classe';
  SInvalidInputParameterCount = 'A contagem do par�metro de entrada � menor que o esperado';
  SIsolationIsNotSupported = 'O n�vel de isolamento da Transa��o n�o � suportado';
  SColumnWasNotFound = 'Coluna com o nome "%s" n�o foi encontrada';
  SWrongTypeForBlobParameter = 'Tipo errado para par�metro Blob';
  SIncorrectConnectionURL = 'Conex�o incorreta URL: %s';
  SUnsupportedProtocol = 'Protocolo n�o suportado: %s';
  SUnsupportedByDriver    = 'O Driver n�o suporta este recurso nativamente: [%s]';

  SConnectionIsNotOpened = 'Conex�o ainda n�o est� aberta.';
  SInvalidOpInAutoCommit = 'Opera��o inv�lida no modo AutoCommit.';
  SInvalidOpInNonAutoCommit = 'Opera��o inv�lida quando o modo AutoCommit � False.';
  SInvalidOpPrepare = 'Prepare transaction somente � poss�vel ap�s comandar StartTransaction';

  SConnectionIsNotAssigned = 'Componente de conex�o de banco de dados n�o atribu�do';
  SQueryIsEmpty = 'A consulta SQL est� vazia';
  SCanNotExecuteMoreQueries = 'N�o � poss�vel executar mais que uma query';
  SOperationIsNotAllowed1 = 'Opera��o n�o permitida no modo FORWARD ONLY';
  SOperationIsNotAllowed2 = 'Opera��o n�o permitida no modo READ ONLY';
  SOperationIsNotAllowed3 = 'Opera��o n�o permitida no modo %s';
  SOperationIsNotAllowed4 = 'Opera��o n�o permitida para DataSet fechado';
  SNoMoreRecords = 'Nenhum registro no ResultSet';
  SCanNotOpenResultSet = 'N�o foi poss�vel abrir o ResultSet';
  SCanNotOpenDataSetWhenDestroying ='Translate : Cannot open a dataset when the componentstate is dsDestroying';
  SCircularLink = 'DataSource possui um link circular';
  SBookmarkWasNotFound = 'Bookmark n�o foi encontrado';
  SIncorrectSearchFieldsNumber = 'N�mero incorreto de valores de campos de procura';
  SInvalidOperationInTrans = 'Opera��o inv�lida no modo de transa��o expl�cita';
  SIncorrectSymbol = 'S�mbolo incorreto na lista de campos "%s".';
  SIncorrectToken = 'Sinal incorreto seguido por ":"';
  SIncorrectParamChar = 'TRANSLATE : Invalid value for ParamChar';

  SSelectedTransactionIsolation = 'O n�vel selecionado do isolamento da transa��o n�o � suportado';
  SDriverNotSupported = 'Driver n�o suportado %s';
  SPattern2Long = 'Padr�o � muito longo';
  SDriverNotCapableOutParameters = 'O Driver n�o suporta a passagem de par�metros';
  SStatementIsNotAllowed = 'Declara��o n�o permitida';
  SStoredProcIsNotAllowed = 'A stored procedure n�o � permitida';
  SCannotPerformOperation = 'N�o � poss�vel executar a opera��o num ResultSet fechado';
  SInvalidState = 'Estado inv�lido';
  SErrorConvertion = 'Erro de convers�o';
  SDataTypeDoesNotSupported = 'Tipo de dado n�o suportado';
  SUnsupportedParameterType = 'Tipo de par�metro n�o suportado';
  SUnsupportedDataType = 'Tipo de dado n�o suportado';
  SErrorConvertionField = 'Erro de convers�o para do campo "%s" para SQLType "%s"';
  SBadOCI = 'Vers�o de OCI incompat�vel [% s]. Requer 8.0.3 ou mais antigo';
  SConnect2AsUser = 'Conecte "% s" como usu�rio "% s"';
  SUnknownError = 'Erro desconhecido';
  SFieldNotFound1 = 'Campo "%s" n�o foi encontrado';
  SFieldNotFound2 = 'Campo %d n�o foi encontrado';

  SLoginPromptFailure = 'N�o foi poss�vel encontrar o di�logo padr�o de login. Por favor adicione DBLogDlg para a se��o uses de seu arquivo principal.';

  SPropertyQuery = 'A Query poder� demorar em bancos de dados grandes!';
  SPropertyTables = 'Voc� deveria limitar por Catalogo e/ou Esquema.';
  SPropertyColumns = 'Voc� deveria limitar por Catalogo, Esquema e/ou Tabela.';
  SPropertyProcedures = 'Voc� deveria limitar por Catalogo e/ou Esquema.';
  SPropertySequences = 'Voc� deveria limitar por Catalogo e/ou Esquema..';
  SPropertyExecute = 'Executar a Query de qualquer maneira?';

  SFormTest = 'Teste Editor ZEOS SQL';
  SButtonClose = '&Fechar';
  SFormEditor = 'Editor ZEOS SQL';
  STabSheetSelect = 'SQL Select';
  SMenuLoad = 'Carregar';
  SMenuSave = 'Salvar';
  SButtonGenerate = '&Gerar';
  SButtonCheck = '&Verificar';
  SButtonTest = '&Testar';
  SButtonOk = '&OK';
  SButtonCancel = '&Cancelar';
  STableAlias = '&Alias Tabela';
  SReplaceSQL = '&Substituir SQL';
  SDialogOpenTitle = 'Abrir Arquivo SQL';
  SDialogSaveTitle = 'Salvar Arquivo SQL';
  SSQLEditor = 'Editor SQL';
  SDatabaseDialog = 'Abrir Banco de Dados existente';

  SUpdateSQLNoResult = 'SQL Update Refresh resultou num conjunto vazio';
  SUpdateSQLRefreshStatementcount ='Usar somente 1 declara��o SQL para Update Refresh';
  {$IFDEF FPC}
  SNotEditing = 'Dataset n�o est� em modo de edi��o ou inser��o';
  SFieldTypeMismatch = 'Tipo inv�lido para o campo ''%s'', esperado: %s atual: %s';
  SFieldSizeMismatch = 'Tamanho Inv�lido para o campo ''%s'', esperado: %d atual: %d';
  {$ENDIF}
  SNeedField               = 'O campo %s � obrigat�rio, mas n�o foi preenchido.';

  SFailedtoInitPrepStmt   = 'A declara��o preparada falhou ao inicializar'; 
  SFailedtoPrepareStmt    = 'A declara��o falhou durante o processo de preparo'; 
  SFailedToBindAllValues  = 'A Aplica��o falhou na tradu��o de todos os valores';
  SAttemptExecOnBadPrep   = 'Tentativa de executar uma declara��o que n�o foi corretamente preparada';
  SBindingFailure         = 'Falha ao traduzir o conjunto de par�metros';
  SPreparedStmtExecFailure = 'A declara��o preparada falhou ao executar';
  SBoundVarStrIndexMissing = '�ndice de texto "%s" da vari�vel de limite n�o existe';
  SBindVarOutOfRange      = '�ndice da vari�vel de limite fora de alcance: %d';
  SFailedToBindResults    = 'A Aplica��o falhou ao tratar o result set';
  

  SRefreshRowOnlySupportedWithUpdateObject = 'O m�todo RefreshRow somente � suportado com um update object';
  SMustBeInBrowseMode = 'A Opera��o � permitida somente no modo dsBrowse';

  SUnKnownParamDataType = 'Param.DataType � de tipo desconhecido';
  SFieldReadOnly        = 'O campo %d � somente leitura e n�o p�de receber dados';
  SInvalidUpdateCount   = '%d registro(s) atualizados. Apenas um registro deveria ter sido atualizado.'; 

  SRowBufferWidthExceeded ='O tamanho do buffer para linhas (Rows) foi excedido. Tente usar menos ou mais colunas na query SQL';
{$ELSE}

{$IFDEF DUTCH}
  SSQLError1 = 'SQL Fout: %s';
  SSQLError2 = 'SQL Fout: %s Code: %d';
  SSQLError3 = 'SQL Fout: %s Code: %d SQL: %s';
  SSQLError4 = 'SQL Fout: %s Code: %d Bericht: %s';

  SListCapacityError = 'Lijst capaciteit buiten bereik (%d)';
  SListCountError = 'Lijst aantal buiten bereik (%d)';
  SListIndexError = 'Lijst index buiten bereik (%d)';

  SClonningIsNotSupported = 'Kloonen worden niet ondersteund in deze klasse';
  SImmutableOpIsNotAllowed = 'Deze operatie is niet ondersteund voor immutable collection';
  SStackIsEmpty = 'Stack is leeg';
  SVariableWasNotFound = 'Variabele "%s" niet gevonden';
  SFunctionWasNotFound = 'Functie "%s" niet gevonden';
  SInternalError = 'Interne fout';
  SSyntaxErrorNear = 'Syntaxis fout bij "%s"';
  SSyntaxError = 'Syntaxis fout';
  SUnknownSymbol = 'Onbekend symbool "%s"';
  SUnexpectedExprEnd = 'Onverwacht einde van de expressie';
  SRightBraceExpected = ') verwacht';
  SParametersError = 'Verwacht worden %d parameters maar er zijn er %d gevonden';
  SExpectedMoreParams = 'Meer dan 2 parameters werden verwacht';
  SInvalidVarByteArray = 'Ongeldig VarByte array';
  SVariableAlreadyExists = 'Variabele "%s" bestaat al';
  STypesMismatch = 'Types komen niet overeen';
  SUnsupportedVariantType = 'Niet ondersteund variant type';
  SUnsupportedOperation = 'Niet ondersteunde operatie';

  STokenizerIsNotDefined = 'Tokenizer is niet gedefinieerd';
  SLibraryNotFound = 'DLL van de lijst %s werd niet gevonden';
  SEncodeDateIsNotSupported = 'Deze versie ondersteunt isc_encode_sql_date niet';
  SEncodeTimeIsNotSupported = 'Deze versie ondersteunt isc_encode_sql_time niet';
  SEncodeTimestampIsNotSupported = 'Deze versie ondersteunt isc_encode_sql_timestamp niet';
  SDecodeDateIsNotSupported = 'Deze versie ondersteunt isc_decode_sql_date niet';
  SDecodeTimeIsNotSupported = 'Deze versie ondersteunt isc_decode_sql_time niet';
  SDecodeTimestampIsNotSupported = 'Deze versie ondersteunt isc_decode_sql_timestamp niet';

  SCanNotRetrieveResultSetData = 'Kan ResultSet data niet ophalen';
  SRowBufferIsNotAssigned = 'Row buffer is niet toegekend';
  SColumnIsNotAccessable = 'Kolom met index %d is niet bereikbaar';
  SConvertionIsNotPossible = 'Conversie is niet mogelijk voor kolom %d van %s tot %s';
  SCanNotAccessBlobRecord = 'Kan het blob record in kolom %d met type %s niet benaderen';
  SRowDataIsNotAvailable = 'Rij data is niet beschikbaar';
  SResolverIsNotSpecified = 'Resolver is niet gespecificeerd voor deze ResultSet';
  SResultsetIsAlreadyOpened = 'ResultSet is al geopend';
  SCanNotUpdateEmptyRow = 'Kan een lege rij niet updaten';
  SCanNotUpdateDeletedRow = 'Kan een verwijderde rij niet updaten';
  SCanNotDeleteEmptyRow = 'Kan een lege rij niet verwijderen';
  SCannotUseCommit = 'Commit in autocommit mode is niet mogelijk';
  SCannotUseRollBack = 'Rollback in autocommit mode is niet mogelijk';
  SCanNotUpdateComplexQuery = 'Kan een complexe query met meerdere tabellen niet updaten';
  SCanNotUpdateThisQueryType = 'Kan dit query type niet updaten';
  SDriverWasNotFound = 'Gevraagde database driver is niet gevonden';
  SCanNotConnectToServer = 'Kan geen verbinding maken met de SQL server';
  STableIsNotSpecified = 'Tabel is niet gespecifieerd';
  SLiveResultSetsAreNotSupported = 'Live query is niet ondersteund door deze klasse';
  SInvalidInputParameterCount = 'Input parameter aantal is lager dan verwacht';
  SIsolationIsNotSupported = 'Transactie isolatie niveau wordt niet ondersteund';
  SColumnWasNotFound = 'Kolom met naam "%s" bestaat niet';
  SWrongTypeForBlobParameter = 'Verkeerde type voor Blob parameter';
  SIncorrectConnectionURL = 'Ongeldige connectie URL: %s';
  SUnsupportedProtocol = 'Niet ondersteund protocol: %s';
  SUnsupportedByDriver    = 'De driver ondersteunt deze functie niet: [%s]';

  SConnectionIsNotOpened = 'Verbinding is niet gemaakt.';
  SInvalidOpInAutoCommit = 'Ongeldige operatie in AutoCommit mode.';
  SInvalidOpInNonAutoCommit = 'Ongeldige operatie in non AutoCommit mode.';
  SInvalidOpPrepare = 'Transactie voorbereiden is enkel mogelijk bij de eerste aanroep van Starttransaction!';

  SConnectionIsNotAssigned = 'Database connectie component is niet toegekend';
  SQueryIsEmpty = 'SQL Query is leeg';
  SCanNotExecuteMoreQueries = 'Kan niet meerdere queries uitvoeren';
  SOperationIsNotAllowed1 = 'Bewerking is niet toegestaan in FORWARD ONLY mode';
  SOperationIsNotAllowed2 = 'Bewerking is niet toegestaan in READ ONLY mode';
  SOperationIsNotAllowed3 = 'Bewerking is niet toegestaan in %s mode';
  SOperationIsNotAllowed4 = 'Bewerking is niet toegestaan voor gesloten dataset';
  SNoMoreRecords = 'Geen records meer aanwezig in ResultSet';
  SCanNotOpenResultSet = 'Kan een ResultSet niet openen';
  SCanNotOpenDataSetWhenDestroying ='Kan een Dataset niet openen wanneer de componentstate=dsDestroying';
  SCircularLink = 'Databron maakt een oneindige verbindingslus';
  SBookmarkWasNotFound = 'Bookmark niet gevonden';
  SIncorrectSearchFieldsNumber = 'Incorrect aantal zoekvelden';
  SInvalidOperationInTrans = 'Ongeldige operatie in explicit transaction mode';
  SIncorrectSymbol = 'Ongeldig symbool in veld lijst "%s".';
  SIncorrectToken = 'Ongeldig teken gevolgd door ":"';
  SIncorrectParamChar = 'TRANSLATE : Invalid value for ParamChar';

  SSelectedTransactionIsolation = 'Geselecteerd transactie isolatie niveau niet ondersteund';
  SDriverNotSupported = 'Driver niet ondersteund %s';
  SPattern2Long = 'Patroon is te lang';
  SDriverNotCapableOutParameters = 'Driver ondersteunt geen out parameters';
  SStatementIsNotAllowed = 'Statement is niet toegestaan';
  SStoredProcIsNotAllowed = 'Stored procedures zijn niet toegestaan';
  SCannotPerformOperation = 'Kan operatie niet uitvoeren op een gesloten ResultSet';
  SInvalidState = 'Ongeldige status';
  SErrorConvertion = 'Conversiefout';
  SDataTypeDoesNotSupported = 'Data type is niet onderstuend';
  SUnsupportedParameterType = 'Niet ondersteund parameter type';
  SUnsupportedDataType = 'Niet ondersteund data type';
  SErrorConvertionField = 'Conversie fout voor veld "%s" naar SQLType "%s"';
  SBadOCI = 'Ongeschikte OCI version [%s]. Vereist is 8.0.3 of nieuwer';
  SConnect2AsUser = 'Verbinden met "%s" als gebruiker "%s"';
  SUnknownError = 'Onbekende fout';
  SFieldNotFound1 = 'Veld "%s" niet gevonden';
  SFieldNotFound2 = 'Veld %d niet gevonden';

  SLoginPromptFailure = 'Kan de standaard login prompt niet vinden.  Voeg DBLogDlg toe aan de uses sectie.';

  SPropertyQuery = 'De Query kan enige tijd duren bij grote databases!';
  SPropertyTables = 'Limiet op Catalog en/of Schema is vereist.';
  SPropertyColumns = 'Limiet op Catalog, Schema en/of tablenaam is vereist.';
  SPropertyProcedures = 'Limiet op Catalog en/of Schema is vereist.';
  SPropertySequences = 'Limiet op Catalog en/of Schema is vereist.';
  SPropertyExecute = 'Dient de Query toch te worden uitgevoerd?';

  SFormTest = 'ZEOS SQL Editor Test';
  SButtonClose = '&Sluiten';
  SFormEditor = 'ZEOS SQL Editor';
  STabSheetSelect = 'Select SQL';
  SMenuLoad = 'Laden';
  SMenuSave = 'Opslaan';
  SButtonGenerate = '&Genereren';
  SButtonCheck = 'C&heck';
  SButtonTest = '&Test';
  SButtonOk = '&OK';
  SButtonCancel = '&Annuleren';
  STableAlias = 'Tabel al&ias';
  SReplaceSQL = '&Vervang SQL';
  SDialogOpenTitle = 'SQL Bestand Openen';
  SDialogSaveTitle = 'SQL Bestand Opslaan';
  SSQLEditor = 'SQL Editor';
  SDatabaseDialog = 'Open bestaande database';

  SUpdateSQLNoResult = 'Der zuvor aktualisierte SQL liefert kein Resultset zur�ck';
  SUpdateSQLRefreshStatementcount ='Update Refresh SQL Statement count moet 1 zijn';

  {$IFDEF FPC}
  SNotEditing = 'Dataset is niet in edit of insert modus';
  SFieldTypeMismatch = 'Type mismatch voor veld ''%s'', verwacht: %s actueel: %s';
  SFieldSizeMismatch = 'Size mismatch voor veld ''%s'', verwacht: %d actueel: %d';
  {$ENDIF}
  SNeedField               = 'Veld %s is verplicht, maar niet ingevuld.';

  SFailedtoInitPrepStmt   = 'Initialisatie van Prepared statement mislukt';
  SFailedtoPrepareStmt    = 'Statement mislukt tijdens prepare';
  SFailedToBindAllValues  = 'Pre-bind van alle waarden is mislukt';
  SAttemptExecOnBadPrep   = 'Poging om een statement uit te voeren voor een succesvolle prepare';
  SBindingFailure         = 'Binding van parameterset mislukt';
  SPreparedStmtExecFailure = 'Uitvoeren van Prepared statement mislukt';
  SBoundVarStrIndexMissing = 'Tekst index van bound variable bestaat niet: "%s"';
  SBindVarOutOfRange      = 'Bound variable index buiten bereik: %d';
  SFailedToBindResults    = 'Binding van resultaat mislukt';

  SRefreshRowOnlySupportedWithUpdateObject = 'De refreshrow methode is enkel ondersteund vooreen update object';
  SMustBeInBrowseMode = 'Bewerking is enkel toegestaan in dsBROWSE status';

  SUnKnownParamDataType = 'Param.DataType is onbekend';
  SFieldReadOnly        = 'Readonly veld kan geen waarde toegewezen krijgen: %d';
  SInvalidUpdateCount     = '%d record(s) gewijzigd. Slechts 1 record had gewijzigd mogen zijn.'; 

  SRowBufferWidthExceeded ='Rij buffer grootte overschreden. Probeer minder kolommen te gebruiken in je SQL query.';
{$ELSE}
// <- ms, 09/05/2005

// -> ms, 03/05/2005
{$IFDEF GERMAN}
  SSQLError1 = 'SQL Fehler: %s';
  SSQLError2 = 'SQL Fehler: %s Code: %d';
  SSQLError3 = 'SQL Fehler: %s Code: %d SQL: %s';
  SSQLError4 = 'SQL Fehler: %s Code: %d Meldung: %s';

  SListCapacityError = 'Die Listenkapazit�t �bersteigt die definierte Grenze (%d)';
  SListCountError = 'Der Listenz�hler ist au�erhalb seiner definierten Grenzen (%d)';
  SListIndexError = 'Der Listenindex ist au�erhalb der definierten Grenzen (%d)';

  SClonningIsNotSupported = 'Diese Klasse kann nicht geklont werden';
  SImmutableOpIsNotAllowed = 'Diese Operation ist bei nicht �nderbaren Collections nicht erlaubt';
  SStackIsEmpty = 'Der Stack ist leer';
  SVariableWasNotFound = 'Die Variable "%s" wurde nicht gefunden';
  SFunctionWasNotFound = 'Die Funktion "%s" wurde nicht gefunden';
  SInternalError = 'Interner Fehler';
  SSyntaxErrorNear = 'Syntax Fehler bei "%s"';
  SSyntaxError = 'Syntax Fehler';
  SUnknownSymbol = 'Unbekanntes Symbol "%s"';
  SUnexpectedExprEnd = 'Unerwartetes Ende des Ausdrucks';
  SRightBraceExpected = ') erwartet';
  SParametersError = 'Es werden %d Parameter erwartet, aber nur %d Parameter gefunden';
  SExpectedMoreParams = 'Es werden mehr als zwei Parameter erwartet';
  SInvalidVarByteArray = 'Ung�ltiges VarByte Array';
  SVariableAlreadyExists = 'Die Variable "%s" existiert bereits';
  STypesMismatch = 'Inkompatible Typen';
  SUnsupportedVariantType = 'Nicht unterst�tzter Variant-Typ';
  SUnsupportedOperation = 'Nicht unterst�tzte Operation';
  SUnsupportedByDriver    = 'Der Treiber unterst�tzt dieses Feature nicht von haus aus: [%s]';

  STokenizerIsNotDefined = 'Tokenizer wurde nicht definiert';
  SLibraryNotFound = 'Es wurde keine der in %s gelisteten DLL''s gefunden';
  SEncodeDateIsNotSupported = 'Diese Version unterst�tzt "isc_encode_sql_date" nicht';
  SEncodeTimeIsNotSupported = 'Diese Version unterst�tzt "isc_encode_sql_time" nicht';
  SEncodeTimestampIsNotSupported = 'Diese Version unterst�tzt "isc_encode_sql_timestamp" nicht';
  SDecodeDateIsNotSupported = 'Diese Version unterst�tzt "isc_decode_sql_date" nicht';
  SDecodeTimeIsNotSupported = 'Diese Version unterst�tzt "isc_decode_sql_time" nicht';
  SDecodeTimestampIsNotSupported = 'Diese Version unterst�tzt "isc_decode_sql_timestamp" nicht';

  SCanNotRetrieveResultSetData = 'Die Ergebnismenge kann nicht ermittelt werden';
  SRowBufferIsNotAssigned = 'Der Zeilen-Buffer ist nicht zugewiesen';
  SColumnIsNotAccessable = 'Auf die Spalte (Tabellenfeld) mit dem Index %d kann nicht zugegriffen werden';
  SConvertionIsNotPossible = 'Eine Konvertierung der Spalte (Tabellenfeld) %d von %s bis %s kann nicht durchgef�hrt werden';
  SCanNotAccessBlobRecord = 'Auf den BLOB-Datensatz in Spalte (Tabellenfeld) %d vom Typ %s kann nicht zugegriffen werden';
  SRowDataIsNotAvailable = 'Die Zeilendaten (Datensatzdaten) sind nicht verf�gbar';
  SResolverIsNotSpecified = 'F�r diese Ergebnismenge wurde kein sog. "Resolver" angegeben';
  SResultsetIsAlreadyOpened = 'Die Ergebnismenge ist bereits ge�ffnet';
  SCanNotUpdateEmptyRow = 'Eine leere Datenzeile kann nicht aktualisiert werden';
  SCanNotUpdateDeletedRow = 'Eine gel�schte Datenzeile kann nicht aktualisiert werden';
  SCanNotDeleteEmptyRow = 'Eine leere Datenzeile kann nicht gel�scht werden';
  SCannotUseCommit = 'COMMIT kann im AUTOCOMMIT-Modus nicht verwendet werden';
  SCannotUseRollBack = 'ROLLBACK kann im AUTOCOMMIT-Modus nicht verwendet werden';
  SCanNotUpdateComplexQuery = 'Ein Query, dessen Ergebnismenge aus mehr als einer Tabelle stammt, kann nicht aktualisiert werden';
  SCanNotUpdateThisQueryType = 'Diese Art von Queries kann nicht aktualisiert werden';
  SDriverWasNotFound = 'Der angegebene Datenbanktreiber wurde nicht gefunden';
  SCanNotConnectToServer = 'Kann keine Verbindung zum SQL Server herstellen';
  STableIsNotSpecified = 'Tabelle ist nicht spezifiziert';
  SLiveResultSetsAreNotSupported = 'Ein "Live Query" wird von dieser Klasse nicht unterst�tzt';
  SInvalidInputParameterCount = 'Es wurden weniger Eingabeparameter angegeben, als erwartet';
  SIsolationIsNotSupported = 'Der gew�hlte Trasaktions-Isolationslevel wird nicht unterst�tzt';
  SColumnWasNotFound = 'Eine Tabellenspalte namens "%s" wurde nicht gefunden';
  SWrongTypeForBlobParameter = 'Falscher Typ f�r einen BLOB-Parameter';
  SIncorrectConnectionURL = 'Falsche Verbindungs-URL: %s';
  SUnsupportedProtocol = 'Nicht unterst�tztes Protokoll: %s';

  SConnectionIsNotOpened = 'Die Verbindung zur Datenbank ist noch nicht hergestellt';
  SInvalidOpInAutoCommit = 'Ung�ltige Operation im AUTOCOMMIT-Modus';
  SInvalidOpInNonAutoCommit = 'Ung�ltige Operation au�erhalb des AUTOCOMMIT-Modus';
  SInvalidOpPrepare = 'Transaktion vorzubereiten ist nur beim ersten Aufruf von Starttransaction m�glich!';

  SConnectionIsNotAssigned = 'Die Datenbank-Verbindungskomponente ist nicht angegeben';
  SQueryIsEmpty = 'SQL Query leer';
  SCanNotExecuteMoreQueries = 'Mehr als ein Query kann nicht abgearbeitet werden';
  SOperationIsNotAllowed1 = 'Die Operation ist im FORWARD ONLY Modus nicht erlaubt';
  SOperationIsNotAllowed2 = 'Die Operation ist im READ ONLY Modus nicht erlaubt';
  SOperationIsNotAllowed3 = 'Die Operation ist im %s Modus nicht erlaubt';
  SOperationIsNotAllowed4 = 'Die Operation ist bei einem geschlossenen DataSet nicht erlaubt';
  SNoMoreRecords = 'Es gibt keine weiteren Datens�tze in der Ergebnismenge';
  SCanNotOpenResultSet = 'Die Ergebnismenge kann nicht ge�ffnet werden';
  SCanNotOpenDataSetWhenDestroying ='Translate : Cannot open a dataset when the componentstate is dsDestroying';
  SCircularLink = 'Die DataSource hat einen zirkul�ren Verweis';
  SBookmarkWasNotFound = 'Das Lesezeichen (Bookmark) wurde nicht gefunden';
  SIncorrectSearchFieldsNumber = 'Die Anzahl der Suchfeldwerte ist nicht korrekt';
  SInvalidOperationInTrans = 'Ung�ltige Operatio im Zustand einer expliziten Transaktion';
  SIncorrectSymbol = 'Falsches Symbol in der Feldliste "%s".';
  SIncorrectToken = 'Falsches Token gefolgt von ":"';
  SIncorrectParamChar = 'TRANSLATE : Invalid value for ParamChar';

  SSelectedTransactionIsolation = 'Der gew�hlte Transaktions-Isolationslevel wird nicht unterst�tzt';
  SDriverNotSupported = 'Der Treiber wird nicht unterst�tzt: %s';
  SPattern2Long = 'Das Muster (Pattern) ist zu lang';
  SDriverNotCapableOutParameters = 'Der Treiber beherrscht keine Parameter';
  SStatementIsNotAllowed = 'Diese Anweisung ist nicht erlaubt';
  SStoredProcIsNotAllowed = 'Diese Stored Procedure ist nicht erlaubt';
  SCannotPerformOperation = 'Auf eine geschlossene Ergebnismenge k�nnen keine Operationen ausgef�hrt werden';
  SInvalidState = 'Ung�ltiger Status';
  SErrorConvertion = 'Konvertierungsfehler';
  SDataTypeDoesNotSupported = 'Der Datentyp wird nicht unterst�tzt';
  SUnsupportedParameterType = 'Der Parametertyp wird nicht unterst�tzt';
  SUnsupportedDataType = 'Der Datentyp wird nicht unterst�tzt';
  SErrorConvertionField = 'Konvertierungsfehler bei Feld "%s" nach SQL-Typ "%s"';
  SBadOCI = 'Die OCI Version 8.0.3 (oder �lter) wird ben�tigt! Aktuelle Version: %s';
  SConnect2AsUser = 'Verbinde zu "%s" als User "%s"';
  SUnknownError = 'Unbekannter Fehler';
  SFieldNotFound1 = 'Das Feld "%s" wurde nicht gefunden';
  SFieldNotFound2 = 'Das Feld %d wurde nicht gefunden';

  SLoginPromptFailure = 'Der Standard-Login-Dialog konnte nicht gefunden werden. Bitte DBLogDlg in die USES-Sektion der Haupt-Unit hinzuf�gen';

  SPropertyQuery = 'Die Abfrage kann bei gro�en Datenbanken eine Weile dauern!';
  SPropertyTables = 'Sie sollte durch die Angabe von Catalog und/oder Schema eingeschr�nkt werden.';
  SPropertyColumns = 'Sie sollte durch die Angabe von Catalog, Schema und/oder Tabellenname eingeschr�nkt werden.';
  SPropertyProcedures = 'Sie sollte durch die Angabe von Catalog und/oder Schema eingeschr�nkt werden.';
  SPropertySequences = 'Sie sollte durch die Angabe von Catalog und/oder Schema eingeschr�nkt werden.';
  SPropertyExecute = 'Soll die Abfrage trotzdem ausgef�hrt werden?';

  SFormTest = 'ZEOS SQL Editor Test';
  SButtonClose = '&Schlie�en';
  SFormEditor = 'ZEOS SQL Editor';
  STabSheetSelect = 'SQL aus&w�hlen';
  SMenuLoad = '�ffnen';
  SMenuSave = 'Speichern';
  SButtonGenerate = '&Generieren';
  SButtonCheck = 'Syntax &Pr�fen';
  SButtonTest = 'Befehl &Testen';
  SButtonOk = '&OK';
  SButtonCancel = '&Abbruch';
  STableAlias = 'Tabllen-Alias';
  SReplaceSQL = 'SQL &ersetzen';
  SDialogOpenTitle = 'SQL Script �ffnen';
  SDialogSaveTitle = 'SQL Script speichern';
  SSQLEditor = 'SQL Editor';
  SDatabaseDialog = 'Existierende Datenbank �ffnen';

  SUpdateSQLNoResult = 'Translate : Update Refresh SQL delivered no resultset';
  SUpdateSQLRefreshStatementcount ='Translate : Update Refresh SQL Statement count must be 1';

  {$IFDEF FPC}
  SNotEditing = 'Das DataSet ist nicht im "edit" oder "insert" Modus.';
  SFieldTypeMismatch = 'Der Typ f�r Feld ''%s'' stimmt nicht. Erwartet wird %s der Typ ist aber momentan %s';
  SFieldSizeMismatch = 'Die Gr��e des Feldes ''%s'' stimmt nicht. Erwartet wird  %d die Gr��e ist aber momentan %d';
  {$ENDIF}
  SNeedField               = 'Translate: Field %s is required, but not supplied.';

  SFailedtoInitPrepStmt   = 'Translate: Prepared statement failed to initialize';
  SFailedtoPrepareStmt    = 'Translate: Statement failed during prepare process';
  SFailedToBindAllValues  = 'Translate: Application failed to pre-bind all values';
  SAttemptExecOnBadPrep   = 'Translate: Attempt made to execute a statement before a successful preparation.';
  SBindingFailure         = 'Translate: Failed to bind parameter set';
  SPreparedStmtExecFailure = 'Translate: Prepared statement failed to execute';
  SBoundVarStrIndexMissing = 'Translate: Bound variable text index "%s" does not exist';
  SBindVarOutOfRange      = 'Translate: Bound variable index out of range: %d';
  SFailedToBindResults    = 'Translate: Application failed to bind to the result set';

  SRefreshRowOnlySupportedWithUpdateObject = 'TRANSLATE: The refreshrow method is only supported with an update object';
  SMustBeInBrowseMode = 'TRANSLATE: Operation is only allowed in dsBROWSE state';

  SUnKnownParamDataType = 'TRANSLATE: Unknown Param.DataType';
  SFieldReadOnly          = 'Translate : Readonly field can''t be assigned a value: %d';
  SInvalidUpdateCount     = 'Translate : %d record(s) updated. Only one record should have been updated.'; 

  SRowBufferWidthExceeded ='Translate: Row buffer width exceeded. Try using fewer or longer columns in SQL query.';
{$ELSE}
  // -> fduenas, 28/06/2005
{$IFDEF SPANISH} //Spanish translations
  SSQLError1 = 'Error SQL: %s';
  SSQLError2 = 'Error SQL: %s C�digo: %d';
  SSQLError3 = 'Error SQL: %s C�digo: %d SQL: %s';
  SSQLError4 = 'Error SQL: %s C�digo: %d Mensage: %s';

  SListCapacityError = 'List capacity fuera de l�mites (%d)';
  SListCountError = 'List count fuera de l�mites (%d)';
  SListIndexError = 'List index fuera de l�mites (%d)';

  SClonningIsNotSupported = 'La Clonaci�n no est� soportada por esta clase';
  SImmutableOpIsNotAllowed = 'Operaci�n no permitida en colecciones no modificables';
  SStackIsEmpty = 'La Pila (Stack) est� vac�a';
  SVariableWasNotFound = 'Variable "%s" no encontrada';
  SFunctionWasNotFound = 'Funci�n "%s" no encontrada';
  SInternalError = 'Error interno';
  SSyntaxErrorNear = 'Error de sintaxis cerca de "%s"';
  SSyntaxError = 'Error de sintaxis';
  SUnknownSymbol = 'S�mbolo "%s" desconocido';
  SUnexpectedExprEnd = 'Fin de expresi�n inesperado';
  SRightBraceExpected = ') esperado';
  SParametersError = 'Se esperaban %d par�metros pero solo %d fueron encontrados';
  SExpectedMoreParams = 'Se esperaban m�s de dos par�metros';
  SInvalidVarByteArray = 'Arreglo VarByte inv�lido';
  SVariableAlreadyExists = 'La variable "%s" ya existe';
  STypesMismatch = 'Los Tipos no coinciden';
  SUnsupportedVariantType = 'Tipo de Variant no soportando';
  SUnsupportedOperation = 'Operaci�n no soportada';

  STokenizerIsNotDefined = 'El objeto Tokenizer no est� definido';
  SLibraryNotFound = 'Ninguna librer�a din�mica de la lista %s fue encontrada';
  SEncodeDateIsNotSupported = 'Esta versi�n no soporta isc_encode_sql_date';
  SEncodeTimeIsNotSupported = 'Esta versi�n no soporta isc_encode_sql_time';
  SEncodeTimestampIsNotSupported = 'Esta versi�n no soporta isc_encode_sql_timestamp';
  SDecodeDateIsNotSupported = 'Esta versi�n no soporta isc_decode_sql_date';
  SDecodeTimeIsNotSupported = 'Esta versi�n no soporta isc_decode_sql_time';
  SDecodeTimestampIsNotSupported = 'Esta versi�n no soporta isc_decode_sql_timestamp';

  SCanNotRetrieveResultSetData = 'No se pueden obtener datos del Resultset';
  SRowBufferIsNotAssigned = 'Buffer de l�nea no asignado';
  SColumnIsNotAccessable = 'La columna con �ndice %d no est� accesible';
  SConvertionIsNotPossible = 'La conversi�n no es posible para la columna %d de %s a %s';
  SCanNotAccessBlobRecord = 'No se puede accesar al registro del blob en la columna %d con tipo %s';
  SRowDataIsNotAvailable = 'Datos de l�nea no disponibles';
  SResolverIsNotSpecified = 'El objeto Resolver no est� especificado para este ResultSet';
  SResultsetIsAlreadyOpened = 'El Resultset ya est� abierto';
  SCanNotUpdateEmptyRow = 'No se puede actualizar una l�nea vac�a';
  SCanNotUpdateDeletedRow = 'No se puede actualizar una l�nea borrada';
  SCanNotDeleteEmptyRow = 'No se puede borrar una l�nea vac�a';
  SCannotUseCommit = 'No se puede usar COMMIT en modo AUTOCOMMIT';
  SCannotUseRollBack = 'No se puede usar ROLLBACK en modo AUTOCOMMIT';
  SCanNotUpdateComplexQuery = 'No se puede actualizar una consulta compleja que haga referencia a m�s de una tabla';
  SCanNotUpdateThisQueryType = 'No se puede actualizar este tipo de consulta';
  SDriverWasNotFound = 'No se encontr� el controlador de base de datos solicitado';
  SCanNotConnectToServer = 'No puede conectarse al servidor SQL';
  STableIsNotSpecified = 'La Tabla no est� especificada';
  SLiveResultSetsAreNotSupported = 'La consulta actualizable no es soportada por esta clase';
  SInvalidInputParameterCount = 'El n�mero de par�metros de tipo Input es menor al esperado';
  SIsolationIsNotSupported = 'Nivel de aislamiento de transacci�n no soportado';
  SColumnWasNotFound = 'Columna con nombre "%s" no encontrada';
  SWrongTypeForBlobParameter = 'Tipo incorrecto para el par�metro Blob';
  SIncorrectConnectionURL = 'URL de conexi�n incorrecta: %s';
  SUnsupportedProtocol = 'Protocolo no soportado: %s';
  SUnsupportedByDriver    = 'Translate: Driver can not support this feature natively: [%s]';

  SConnectionIsNotOpened = 'La conexi�n no ha sido abierta todav�a';
  SInvalidOpInAutoCommit = 'Operaci�n inv�lida en modo AutoCommit';
  SInvalidOpInNonAutoCommit = 'Operaci�n inv�lida en modo No-AutoCommit';
  SInvalidOpPrepare = 'Translate : Prepare transaction only possible on matching first(!) Starttransaction';

  SConnectionIsNotAssigned = 'El componente de conexi�n a base de datos no est� asigando';
  SQueryIsEmpty = 'La Consulta SQL est� vac�a';
  SCanNotExecuteMoreQueries = 'No se puede ejecutar m�s de una consulta';
  SOperationIsNotAllowed1 = 'Operaci�n no permitida en modo FORWARD ONLY';
  SOperationIsNotAllowed2 = 'Operaci�n no permitida en modo READ ONLY (Solo lectura)';
  SOperationIsNotAllowed3 = 'Operaci�n no permitida en modo %s';
  SOperationIsNotAllowed4 = 'Operaci�n no permitida en un dataset cerrado';
  SNoMoreRecords = 'No hay m�s registros en el Resultset';
  SCanNotOpenResultSet = 'No se puede abrir el Resultset';
  SCanNotOpenDataSetWhenDestroying ='Translate : Cannot open a dataset when the componentstate is dsDestroying';
  SCircularLink = 'Datasource hace una referencia c�clica';
  SBookmarkWasNotFound = 'Bookmark no encontrado';
  SIncorrectSearchFieldsNumber = 'N�mero incorrecto de valores de b�squeda';
  SInvalidOperationInTrans = 'Operaci�n inv�lida en modo de transacci�n expl�cita';
  SIncorrectSymbol = 'S�mbolo incorrecto en la lista de campos "%s".';
  SIncorrectToken = 'Token incorrecto seguido de ":"';
  SIncorrectParamChar = 'TRANSLATE : Invalid value for ParamChar';

  SSelectedTransactionIsolation = 'El Nivel seleccionado de aislamiento de transacci�n no est� soportado';
  SDriverNotSupported = 'Controlador %s no soportado';
  SPattern2Long = 'Patr�n de b�squeda demasiado largo';
  SDriverNotCapableOutParameters = 'El controlador no tiene cualidades para manejar par�metros';
  SStatementIsNotAllowed = 'Sentencia no permitida';
  SStoredProcIsNotAllowed = 'El procedimiento alamacenado no est� permitido';
  SCannotPerformOperation = 'No se puede efectuar la operaci�n en un resultset cerrado';
  SInvalidState = 'Estado Inv�lido';
  SErrorConvertion = 'Error de conversi�n';
  SDataTypeDoesNotSupported = 'Tipo de datos no soportado';
  SUnsupportedParameterType = 'Tipo de par�metro no soportado';
  SUnsupportedDataType = 'Tipo de datos no soportado';
  SErrorConvertionField = 'Error de conversi�n del campo "%s" al Tipo SQL "%s"';
  SBadOCI = 'Versi�n de OCI [%s] no aceptable. Se requiere versi�n 8.0.3 o menor';
  SConnect2AsUser = 'Conectando a "%s" como usuario "%s"';
  SUnknownError = 'Error desconocido';
  SFieldNotFound1 = 'Campo "%s" no encontrado';
  SFieldNotFound2 = 'Campo %d no encontrado';

  SLoginPromptFailure = 'Cuadro de Di�logo por omisi�n para autenticaci�n no encontrado.'+#10#13+
                        'Por favor agregue la unidad DBLogDlg a la secci�n uses de la unidad principal de su proyecto.';

  SPropertyQuery = '�La Consulta puede tardar un poco en bases de datos extensas!';
  SPropertyTables = 'Deber�a limitarlas mediante Catalog y/o Schema.';
  SPropertyColumns = 'Deber�a limitarlas mediante Catalog, Schema y/o TableName.';
  SPropertyProcedures = 'Deber�a limitarlos mediante Catalog y/or Schema.';
  SPropertySequences = 'Deber�a limitarlos mediante Catalog y/or Schema.';
  SPropertyExecute = '�Desea ejecutar la consulta de todos modos?';

  SFormTest = 'Prueba del Editor ZEOS SQL';
  SButtonClose = '&Cerrar';
  SFormEditor = 'Editor ZEOS SQL';
  STabSheetSelect = 'Seleccionar SQL';
  SMenuLoad = 'Cargar...';
  SMenuSave = 'Guardar...';
  SButtonGenerate = '&Generar';
  SButtonCheck = 'C&hecar';
  SButtonTest = 'Pro&bar';
  SButtonOk = '&Aceptar';
  SButtonCancel = '&Cancelar';
  STableAlias = 'A&lias de la tabla';
  SReplaceSQL = '&Reemplazar SQL';
  SDialogOpenTitle = 'Abrir archivo SQL';
  SDialogSaveTitle = 'Guardar archivo SQL';
  SSQLEditor = 'Editor SQL';
  SDatabaseDialog = 'Abrir base de datos existente';

  SUpdateSQLNoResult = 'Translate : Update Refresh SQL delivered no resultset';
  SUpdateSQLRefreshStatementcount ='Translate : Update Refresh SQL Statement count must be 1';

  {$IFDEF FPC}
  SNotEditing = 'El Dataset no se encuentra en modo de edici�n o inserci�n';
  SFieldTypeMismatch = 'El Tipo de dato no coincide para el campo ''%s'', se espera: %s, actual: %s';
  SFieldSizeMismatch = 'El Tama�o de dato no coincide para el campo ''%s'', se espera: %d, actual: %d';
  {$ENDIF}
  SNeedField               = 'Translate: Field %s is required, but not supplied.';

  SFailedtoInitPrepStmt   = 'Translate: Prepared statement failed to initialize';
  SFailedtoPrepareStmt    = 'Translate: Statement failed during prepare process';
  SFailedToBindAllValues  = 'Translate: Application failed to pre-bind all values';
  SAttemptExecOnBadPrep   = 'Translate: Attempt made to execute a statement before a successful preparation.';
  SBindingFailure         = 'Translate: Failed to bind parameter set';
  SPreparedStmtExecFailure = 'Translate: Prepared statement failed to execute';
  SBoundVarStrIndexMissing = 'Translate: Bound variable text index "%s" does not exist';
  SBindVarOutOfRange      = 'Translate: Bound variable index out of range: %d';
  SFailedToBindResults    = 'Translate: Application failed to bind to the result set';

  SRefreshRowOnlySupportedWithUpdateObject = 'TRANSLATE: The refreshrow method is only supported with an update object';
  SMustBeInBrowseMode = 'TRANSLATE: Operation is only allowed in dsBROWSE state';

  SUnKnownParamDataType = 'TRANSLATE: Unknown Param.DataType';
  SFieldReadOnly          = 'Translate : Readonly field can''t be assigned a value: %d';
  SInvalidUpdateCount     = 'Translate : %d record(s) updated. Only one record should have been updated.'; 

  SRowBufferWidthExceeded ='Translate: Row buffer width exceeded. Try using fewer or longer columns in SQL query.';
{$ELSE}

{$IFDEF ROMANA}


 SSQLError1 = 'SQL Eroare: %s';
  SSQLError2 = 'SQL Eroare: %s Cod: %d';
  SSQLError3 = 'SQL Eroare: %s Cod: %d SQL: %s';
  SSQLError4 = 'SQL Eroare: %s Cod: %d Mesaj: %s';

  SListCapacityError = 'Capacitatea listei este �n afara limitelor (%d)';
  SListCountError = 'Contorul listei este �n afara limitelor (%d)';
  SListIndexError = 'Indexul listei este �n afara limitelor (%d)';

  SClonningIsNotSupported = 'Clonning nu este suportat de aceast� clas�';
  SImmutableOpIsNotAllowed = 'Opera�ia nu este permis� ori colec�ia nu este modificabil�';
  SStackIsEmpty = 'Stiva este goal�';
  SVariableWasNotFound = 'Variabila "%s" nu a fost g�sit�';
  SFunctionWasNotFound = 'Func�ia "%s" nu a fost g�sit�';
  SInternalError = 'Eroare Intern�';
  SSyntaxErrorNear = 'Eroare de sintax� l�ng� "%s"';
  SSyntaxError = 'Eroare de sintax�';
  SUnknownSymbol = 'Simbol necunoscut "%s"';
  SUnexpectedExprEnd = 'Final nea�teptat pentru expresie';
  SRightBraceExpected = ') a�teptat';
  SParametersError = 'parametrul %d a fost a�teptat dar %d a fost g�sit';
  SExpectedMoreParams = 'Mai nult de doi parametrii sunt a�tepta�i';
  SInvalidVarByteArray = 'Arie VarByte invalid�';
  SVariableAlreadyExists = 'Variabila "%s" deja exist�';
  STypesMismatch = 'Tip nepotrivit';
  SUnsupportedVariantType = 'Tip variant neasteptat';
  SUnsupportedOperation = 'Opera�ie nesuportat�';

  STokenizerIsNotDefined = 'Simbolistica nu este definit�';
  SLibraryNotFound = 'None of the dynamic libraries can be found: %s';
  SEncodeDateIsNotSupported = 'Aceast� versiune nu suport� isc_encode_sql_date';
  SEncodeTimeIsNotSupported = 'Aceast� versiune nu suport� isc_encode_sql_time';
  SEncodeTimestampIsNotSupported = 'Aceast� versiune nu suport� isc_encode_sql_timestamp';
  SDecodeDateIsNotSupported = 'Aceast� versiune nu suport� isc_decode_sql_date';
  SDecodeTimeIsNotSupported = 'Aceast� versiune nu suport� isc_decode_sql_time';
  SDecodeTimestampIsNotSupported = 'Aceast� versiune nu suport� isc_decode_sql_timestamp';

  SCanNotRetrieveResultSetData = 'Nu pot returna  Resultset data';
  SRowBufferIsNotAssigned = 'Row buffer nu este asignat';
  SColumnIsNotAccessable = 'Column with index %d nu este accesibil';
  SConvertionIsNotPossible = 'Conversia nu este posibil� pentru coloana %d din %s �n %s';
  SCanNotAccessBlobRecord = 'Nu pot aceesa �nregistrarea blob �n coloana %d cu tipul %s';
  SRowDataIsNotAvailable = 'Row data nu este disponibil';
  SResolverIsNotSpecified = 'Resolver nu este specificat pentru acest ResultSet';
  SResultsetIsAlreadyOpened = 'Resultset este deja deschis�';
  SCanNotUpdateEmptyRow = 'Nu pot updata o �nregistrare goal�';
  SCanNotUpdateDeletedRow = 'Nu pot updata o �nregistrare �tears�';
  SCanNotDeleteEmptyRow = 'Nu pot �terge o �nregistrare goal�';
  SCannotUseCommit = 'Nu po�i folosi COMMIT �n modul AUTOCOMMIT ';
  SCannotUseRollBack = 'Nu po�i folosi ROLLBACK �n modul AUTOCOMMIT ';
  SCanNotUpdateComplexQuery = 'Nu pot updata un query complex cu mai mult de un tabel';
  SCanNotUpdateThisQueryType = 'Nu pot updata acest tip de query';
  SDriverWasNotFound = 'Driverul pentru baza de date nu a fost g�sit';
  SCanNotConnectToServer = 'Nu ma pot conecta la serverul SQL';
  STableIsNotSpecified = 'Tbelul nu este specificat';
  SLiveResultSetsAreNotSupported = 'Live query is not supported by this class';
  SInvalidInputParameterCount = 'Input parameter count is less then expected';
  SIsolationIsNotSupported = 'Transaction isolation level nu este suportat';
  SColumnWasNotFound = 'Coloana cu numele "%s" nu a fost f�sit�';
  SWrongTypeForBlobParameter = 'Tip gre�it pentru parametru Blob';
  SIncorrectConnectionURL = 'Conexiune URL incorect�: %s';
  SUnsupportedProtocol = 'Protocol nesuportat: %s';
  SUnsupportedByDriver    = 'Driver nu poate suporta aceast� facilitate : [%s]';

  SConnectionIsNotOpened = 'Conexiune nu este deschis� inc�';
  SInvalidOpInAutoCommit = 'Opera�ie invalid� �n modul AutoCommit';
  SInvalidOpInNonAutoCommit = 'Opera�ie invalid� �n modul non AutoCommit ';
  SInvalidOpPrepare = 'Prepare transaction only possible on matching first(!) Starttransaction';

  SConnectionIsNotAssigned = 'Nu este asignat� o component� Database connection';
  SQueryIsEmpty = 'SQL Query este gol';
  SCanNotExecuteMoreQueries = 'Nu pot executa mai mult de un query';
  SOperationIsNotAllowed1 = 'Opera�ia nu este permis� �n modul FORWARD ONLY ';
  SOperationIsNotAllowed2 = 'Opera�ia nu este permis� �n modul READ ONLY';
  SOperationIsNotAllowed3 = 'Opera�ia nu este permis� �n modul %s ';
  SOperationIsNotAllowed4 = 'Opera�ia nu este permis� pentru �n dataset �nchis';
  SNoMoreRecords = 'Nu mai sunt �nregistr�ri �n Resultset';
  SCanNotOpenResultSet = 'Nu pot deschide Resultset';
  SCanNotOpenDataSetWhenDestroying ='Translate : Cannot open a dataset when the componentstate is dsDestroying';
  SCircularLink = 'Datasource makes a circular link';
  SBookmarkWasNotFound = 'Bookmark nu a fost g�sit';
  SIncorrectSearchFieldsNumber = 'Num�r incorect of search field values';
  SInvalidOperationInTrans = 'Opera�ie invalid� �n modul explicit transaction';
  SIncorrectSymbol = 'Simbol incorect �n lista de c�mpuri  "%s".';
  SIncorrectToken = 'Incorect token dup� ":"';
  SIncorrectParamChar = 'TRANSLATE : Invalid value for ParamChar';

  SSelectedTransactionIsolation = 'Selected transaction isolation level is not supported';
  SDriverNotSupported = 'Driver nesuportat %s';
  SPattern2Long = 'Pattern is too long';
  SDriverNotCapableOutParameters = 'Driver nu este capabil s� m�nuie parametrii';
  SStatementIsNotAllowed = 'Statement nu sunt permise';
  SStoredProcIsNotAllowed = 'The stored proc nu sunt permise';
  SCannotPerformOperation = 'Nu se pot face opera�ii cu Resultset �nchis';
  SInvalidState = 'Stare invalid�';
  SErrorConvertion = 'Eroare de conversie';
  SDataTypeDoesNotSupported = 'Tip de dat� nesuportat';
  SUnsupportedParameterType = 'Tip parametru nesuportat';
  SUnsupportedDataType = 'Tip dat� nesuportat';
  SErrorConvertionField = 'Eroare de conversie pentru c�mpul "%s" �n TipSQL "%s"';
  SBadOCI = 'Bad OCI version [%s]. Version 8.0.3 or older is required';
  SConnect2AsUser = 'Conectare la "%s" ca utlizator "%s"';
  SUnknownError = 'Eroare necunoscut�';
  SFieldNotFound1 = 'C�mpul "%s" nu a fost g�sit';
  SFieldNotFound2 = 'C�mpul %d nu a fost g�sit';

  SLoginPromptFailure = 'Nu g�sesc fereastra de dialog implicit� pentru login. V� rog ad�uga�i DBLogDlg �n sec�iunea uses.';

  SPropertyQuery = 'The Query may last a while on large databases!';
  SPropertyTables = 'You should limit it by Catalog and/or Schema.';
  SPropertyColumns = 'You should limit it by Catalog, Schema and/or TableName.';
  SPropertyProcedures = 'You should limit it by Catalog and/or Schema.';
  SPropertySequences = 'You should limit it by Catalog and/or Schema.';
  SPropertyExecute = 'Query va fi executat� oricum?';

  SFormTest = 'ZEOS SQL Editor Test';
  SButtonClose = '�n&chide';
  SFormEditor = 'ZEOS SQL Editor';
  STabSheetSelect = 'Select SQL';
  SMenuLoad = 'Deschide';
  SMenuSave = 'Salvare';
  SButtonGenerate = '&Generare';
  SButtonCheck = 'Verificare';
  SButtonTest = '&Test';
  SButtonOk = '&OK';
  SButtonCancel = 'Revo&care';
  STableAlias = 'T&able alias';
  SReplaceSQL = '&Replace SQL';
  SDialogOpenTitle = 'Deschide Fi�ier SQL';
  SDialogSaveTitle = 'Salveaz� Fi�ier SQL';
  SSQLEditor = 'SQL Editor';
  SDatabaseDialog = 'Deschide baz� date existent�';

  SUpdateSQLNoResult = '"Update Refresh SQL" furnizat nu este un recordset';
  SUpdateSQLRefreshStatementcount ='Declara�ia "Update Refresh SQL" ca num�r trebuie s� fie una';

  {$IFDEF FPC}
  SNotEditing = 'Dataset nu este �n modul de editare sau inserare';
  SFieldTypeMismatch = 'Tip nepotrivit pentru c�mpul ''%s'', a�teptat: %s actual: %s';
  SFieldSizeMismatch = 'Dimensiune nepotrivit� pentru c�mpul  ''%s'', a�teptat: %d actual: %d';
  {$ENDIF}
  SNeedField               = 'Translate: Field %s is required, but not supplied.';

  SFailedtoInitPrepStmt   = 'Translate: Prepared statement failed to initialize';
  SFailedtoPrepareStmt    = 'Translate: Statement failed during prepare process';
  SFailedToBindAllValues  = 'Translate: Application failed to pre-bind all values';
  SAttemptExecOnBadPrep   = 'Translate: Attempt made to execute a statement before a successful preparation.';
  SBindingFailure         = 'Translate: Failed to bind parameter set';
  SPreparedStmtExecFailure = 'Translate: Prepared statement failed to execute';
  SBoundVarStrIndexMissing = 'Translate: Bound variable text index "%s" does not exist';
  SBindVarOutOfRange      = 'Translate: Bound variable index out of range: %d';
  SFailedToBindResults    = 'Translate: Application failed to bind to the result set';

  SRefreshRowOnlySupportedWithUpdateObject = 'TRANSLATE: The refreshrow method is only supported with an update object';
  SMustBeInBrowseMode = 'TRANSLATE: Operation is only allowed in dsBROWSE state';

  SUnKnownParamDataType = 'TRANSLATE: Unknown Param.DataType';

  SRowBufferWidthExceeded ='Translate: Row buffer width exceeded. Try using fewer or longer columns in SQL query.';
  // <-- added by tohenk
  {$ELSE}
  {$IFDEF INDONESIAN}
  SSQLError1 = 'Kesalahan SQL: %s';
  SSQLError2 = 'Kesalahan SQL: %s Kode: %d';
  SSQLError3 = 'Kesalahan SQL: %s Kode: %d SQL: %s';
  SSQLError4 = 'Kesalahan SQL: %s Kode: %d Pesan: %s';

  SListCapacityError = 'Kapasitas List diluar jangkauan (%d)';
  SListCountError = 'Jumlah List diluar jangkauan (%d)';
  SListIndexError = 'Indeks List diluar jangkauan (%d)';

  SClonningIsNotSupported = 'Class ini tidak mendukung kloning';
  SImmutableOpIsNotAllowed = 'Operasi tidak diperkenankan pada koleksi yang tidak dapat diubah';
  SStackIsEmpty = 'Stack kosong';
  SVariableWasNotFound = 'Variabel "%s" tidak ada';
  SFunctionWasNotFound = 'Fungsi "%s" tidak ada';
  SInternalError = 'Kesalahan internal';
  SSyntaxErrorNear = 'Kesalahan Syntax di dekat "%s"';
  SSyntaxError = 'Kesalahan Syntax';
  SUnknownSymbol = 'Simbol tidak dikenali "%s"';
  SUnexpectedExprEnd = 'Tidak dibutuhkan, akhir dari ekspresi';
  SRightBraceExpected = ') dibutuhkan';
  SParametersError = '%d parameter dibutuhkan tapi terdapat %d parameter';
  SExpectedMoreParams = 'Dibutuhkan lebih dari dua parameter';
  SInvalidVarByteArray = 'array VarByte tidak valid';
  SVariableAlreadyExists = 'Variabel "%s" sudah ada';
  STypesMismatch = 'Tipe tidak sesuai';
  SUnsupportedVariantType = 'Tipe variant tidak didukung';
  SUnsupportedOperation = 'Operasi tidak didukung';

  STokenizerIsNotDefined = 'Tokenizer belum ditentukan';
  SLibraryNotFound = 'Tidak ada library ditemukan: %s';
  SEncodeDateIsNotSupported = 'Versi ini tidak mendukung isc_encode_sql_date';
  SEncodeTimeIsNotSupported = 'Versi ini tidak mendukung isc_encode_sql_time';
  SEncodeTimestampIsNotSupported = 'Versi ini tidak mendukung isc_encode_sql_timestamp';
  SDecodeDateIsNotSupported = 'Versi ini tidak mendukung isc_decode_sql_date';
  SDecodeTimeIsNotSupported = 'Versi ini tidak mendukung isc_decode_sql_time';
  SDecodeTimestampIsNotSupported = 'Versi ini tidak mendukung isc_decode_sql_timestamp';

  SCanNotRetrieveResultSetData = 'Tidak dapat mengambil data Resultset';
  SRowBufferIsNotAssigned = 'Row buffer tidak disediakan';
  SColumnIsNotAccessable = 'Kolom dengan indeks %d tidak dapat diakses';
  SConvertionIsNotPossible = 'Konversi tidak dimungkinkan pada kolom %d dari %s ke %s';
  SCanNotAccessBlobRecord = 'Tidak dapat mengakses rekord `blob` pada kolom %d dengan tipe %s';
  SRowDataIsNotAvailable = 'Data Row tidak tersedia';
  SResolverIsNotSpecified = 'Resolver belum ditentukan pada ResultSet ini';
  SResultsetIsAlreadyOpened = 'Resultset sudah terbuka';
  SCanNotUpdateEmptyRow = 'Tidak dapat meng-update row kosong';
  SCanNotUpdateDeletedRow = 'Tidak dapat meng-update row terhapus';
  SCanNotDeleteEmptyRow = 'Tidak dapat meng-hapus row kosong';
  SCannotUseCommit = 'COMMIT tidak dapat digunakan pada mode AUTOCOMMIT';
  SCannotUseRollBack = 'ROLLBACK tidak dapat digunakan pada mode AUTOCOMMIT';
  SCanNotUpdateComplexQuery = 'Tidak dapat meng-update query kompleks dengan lebih dari satu tabel';
  SCanNotUpdateThisQueryType = 'Tidak dapat meng-update query dengan tipe ini';
  SDriverWasNotFound = 'Driver database yang diminta tidak ada';
  SCanNotConnectToServer = 'Tidak dapat terhubung ke server SQL';
  STableIsNotSpecified = 'Tabel belum ditentukan';
  SLiveResultSetsAreNotSupported = 'Live query tidak didukung oleh Class ini';
  SInvalidInputParameterCount = 'Jumlah parameter Input kurang dari yang dibutuhkan';
  SIsolationIsNotSupported = 'Level Isolasi Transaksi tidak didukung';
  SColumnWasNotFound = 'Kolom dengan nama "%s" tidak ada';
  SWrongTypeForBlobParameter = 'Salah tipe untuk parameter Blob';
  SIncorrectConnectionURL = 'Salah koneksi URL: %s';
  SUnsupportedProtocol = 'Protokol tidak didukung: %s';
  SUnsupportedByDriver    = 'Driver tidak mendukung fitur: [%s]';

  SConnectionIsNotOpened = 'Koneksi belum dibuka';
  SInvalidOpInAutoCommit = 'Operasi tidak valid pada mode AUTOCOMMIT';
  SInvalidOpInNonAutoCommit = 'Operasi tidak valid pada mode non AUTOCOMMIT';
  SInvalidOpPrepare = 'Persiapan transaksi hanya mungkin pada (!) Starttransaction pertama';

  SConnectionIsNotAssigned = 'Komponen koneksi Database tidak ditentukan';
  SQueryIsEmpty = 'Query SQL kosong';
  SCanNotExecuteMoreQueries = 'Tidak dapat meng-eksekusi lebih dari satu query';
  SOperationIsNotAllowed1 = 'Operasi tidak diperkenankan pada mode FORWARD ONLY';
  SOperationIsNotAllowed2 = 'Operasi tidak diperkenankan pada mode READ ONLY';
  SOperationIsNotAllowed3 = 'Operasi tidak diperkenankan pada mode %s';
  SOperationIsNotAllowed4 = 'Operasi tidak diperkenankan pada dataset tertutup';
  SNoMoreRecords = 'Tidak ada rekord lagi pada Resultset';
  SCanNotOpenResultSet = 'Tidak dapat membuka Resultset';
  SCanNotOpenDataSetWhenDestroying ='Translate : Cannot open a dataset when the componentstate is dsDestroying';
  SCircularLink = 'Terjadi hubungan Datasource circular';
  SBookmarkWasNotFound = 'Bookmark tidak ada';
  SIncorrectSearchFieldsNumber = 'Salah jumlah nilai field pada pencarian';
  SInvalidOperationInTrans = 'Operasi tidak valid pada mode explicit transaction';
  SIncorrectSymbol = 'Simbol salah pada daftar field "%s".';
  SIncorrectToken = 'Token salah setelah ":"';
  SIncorrectParamChar = 'TRANSLATE : Invalid value for ParamChar';

  SSelectedTransactionIsolation = 'Level Isolasi Transaksi terpilih tidak didukung';
  SDriverNotSupported = 'Driver tidak mendukung %s';
  SPattern2Long = 'Pola terlalu panjang';
  SDriverNotCapableOutParameters = 'Driver tidak mampu menangani parameter';
  SStatementIsNotAllowed = 'Statement tidak diperbolehkan';
  SStoredProcIsNotAllowed = 'StoredProc tidak diperbolehkan';
  SCannotPerformOperation = 'Tidak dapat melakukan operasi pada Resultset tertutup';
  SInvalidState = 'Sate tidak valid';
  SErrorConvertion = 'Kesalahan konversi';
  SDataTypeDoesNotSupported = 'Tipe Data tidak didukung';
  SUnsupportedParameterType = 'Tidak mendukung tipe parameter';
  SUnsupportedDataType = 'Tidak mendukung tipe data';
  SErrorConvertionField = 'Kesalahan konversi field "%s" ke Tipe SQL "%s"';
  SBadOCI = 'OCI version [%s] tidak sah. Dibutuhkan versi 8.0.3 atau terdahulu';
  SConnect2AsUser = 'Koneksi ke "%s" dengan user "%s"';
  SUnknownError = 'Kesalahan tidak diketahui';
  SFieldNotFound1 = 'Field "%s" tidak ada';
  SFieldNotFound2 = 'Field %d tidak ada';

  SLoginPromptFailure = 'Tidak ada dialog Login default. Silahkan tambahkan DBLogDlg ke klausula `uses` pada file utama.';

  SPropertyQuery = 'Query mungkin berlangsung lama pada database besar!';
  SPropertyTables = 'Batasi dengan Katalog data/atau Skema.';
  SPropertyColumns = 'Batasi dengan Katalog, Skema dan/atau Nama Tabel.';
  SPropertyProcedures = 'Batasi dengan Katalog dan/atau Skema.';
  SPropertySequences = 'Batasi dengan Katalog dan/atau Skema.';
  SPropertyExecute = 'Apakah Query jadi dieksekusi?';

  SFormTest = 'Tes Editor SQLZEOS';
  SButtonClose = '&Tutup';
  SFormEditor = 'Editor SQL ZEOS';
  STabSheetSelect = 'SQL Select';
  SMenuLoad = 'Ambil';
  SMenuSave = 'Simpan';
  SButtonGenerate = '&Generate';
  SButtonCheck = '&Cek';
  SButtonTest = 'T&es';
  SButtonOk = '&OK';
  SButtonCancel = '&Batal';
  STableAlias = 'Alias T&abel';
  SReplaceSQL = 'SQL &Replace';
  SDialogOpenTitle = 'Buka File SQL';
  SDialogSaveTitle = 'Simpan File SQL';
  SSQLEditor = 'Editor SQL';
  SDatabaseDialog = 'Buka database yang tersedia';

  SUpdateSQLNoResult = 'Tidak ada Resultset pada Update Refresh SQL';
  SUpdateSQLRefreshStatementcount ='Jumlah Statement pada Update Refresh SQL harus 1';

  {$IFDEF FPC}
  SNotEditing = 'Dataset tidak dalam mode edit atau sisip';
  SFieldTypeMismatch = 'Tipe tidak sesuai pada field ''%s'', seharusnya: %s aktual: %s';
  SFieldSizeMismatch = 'Ukuran tidak sesuai pada field ''%s'', seharusnya: %d aktual: %d';
  {$ENDIF}
  SNeedField               = 'Field %s diperlukan, namun tidak disediakan.';

  SFailedtoInitPrepStmt   = 'Gagal inisialisasi Prepared statement';
  SFailedtoPrepareStmt    = 'Statemen gagal sewaktu proses persiapan';
  SFailedToBindAllValues  = 'Aplikasi gagal dalam penggabungan pendahuluan semua nilai';
  SAttemptExecOnBadPrep   = 'Percobaan eksekusi statemen dilakukan sebelum persiapan berhasil.';
  SBindingFailure         = 'Gagal menggabungkan parameter';
  SPreparedStmtExecFailure = 'Prepared Statement gagal dieksekusi';
  SBoundVarStrIndexMissing = 'Teks variabel indeks "%s" tidak ada';
  SBindVarOutOfRange      = 'Variabel indeks diluar jangkauan: %d';
  SFailedToBindResults    = 'Aplikasi gagal pada penggabungan ke Resultset';

  SRefreshRowOnlySupportedWithUpdateObject = 'Metode RefreshRow hanya didukung oleh obyek Update';
  SMustBeInBrowseMode = 'Operasi hanya diperbolehkan pada status dsBrowse';

  SUnKnownParamDataType = 'Param.DataType tidak dikenal';
  SFieldReadOnly          = 'Field readonly tidak dapat diberikan nilai: %d';
  SInvalidUpdateCount     = '%d rekord terupdate. Seharusnya hanya satu rekord yang terupdate.';

  SRowBufferWidthExceeded = 'Lebar buffer baris terlampaui. Coba kurangi atau gunakan kolom yang lebih panjang dalam query SQL.';
  // <--- end added by tohenk
  //--- begin added by ORMADA --------------------------------------------------
{$ELSE}
{$IFDEF RUSSIAN}
  SSQLError1                               = '������ � SQL ���������: %s';
  SSQLError2                               = '������ � SQL ���������: %s ��� ������: %d';
  SSQLError3                               = '������ � SQL ���������: %s ��� ������: %d SQL: %s';
  SSQLError4                               = '������ � SQL ���������: %s ��� ������: %d ���������: %s';

  SListCapacityError                       = '������ ������ ����� �� ������� (%d)';
  SListCountError                          = '������� ������ ����� �� ������� (%d)';
  SListIndexError                          = '������ ������ ����� �� ������� (%d)';

  SClonningIsNotSupported                  = '������ ����� �� ������������ ������������';
  SImmutableOpIsNotAllowed                 = '�������� �� �������������� �� ���������� ����������';
  SStackIsEmpty                            = '���� ����';
  SVariableWasNotFound                     = '�������� "%s" �� �������';
  SFunctionWasNotFound                     = '������� "%s" �� �������';
  SInternalError                           = '��������� ������';
  SSyntaxErrorNear                         = '������ � ���������� "%s"';
  SSyntaxError                             = '������ � ����������';
  SUnknownSymbol                           = '����������� ������ "%s"';
  SUnexpectedExprEnd                       = '����������� ����� ���������';
  SRightBraceExpected                      = ') ���������';
  SParametersError                         = '��������� %d ����������, ������� %d';
  SExpectedMoreParams                      = '��������� ����� 2-� ����������';
  SInvalidVarByteArray                     = '�������� ������ (VarByte)';
  SVariableAlreadyExists                   = '�������� "%s" ��� ����������';
  STypesMismatch                           = '������������ �����';
  SUnsupportedVariantType                  = '���������������� ���������� (variant) ���';
  SUnsupportedOperation                    = '���������������� ��������';

  STokenizerIsNotDefined                   = '����� �� ����������';
  SLibraryNotFound                         = '�� ����� ������������ ���������� �� �������: %s';
  SEncodeDateIsNotSupported                = '��� ������ �� ������������ isc_encode_sql_date';
  SEncodeTimeIsNotSupported                = '��� ������ �� ������������ isc_encode_sql_time';
  SEncodeTimestampIsNotSupported           = '��� ������ �� ������������ isc_encode_sql_timestamp';
  SDecodeDateIsNotSupported                = '��� ������ �� ������������ isc_decode_sql_date';
  SDecodeTimeIsNotSupported                = '��� ������ �� ������������ isc_decode_sql_time';
  SDecodeTimestampIsNotSupported           = '��� ������ �� ������������ isc_decode_sql_timestamp';

  SCanNotRetrieveResultSetData             = '���������� �������� ����� ������ (Resultset)';
  SRowBufferIsNotAssigned                  = '�� �������� ������ ������';
  SColumnIsNotAccessable                   = '���������� ������� � �������� %d';
  SConvertionIsNotPossible                 = '����������� ���������� ��� ������� %d � %s �� %s';
  SCanNotAccessBlobRecord                  = '���������� �������� ������ � blob ������ � ������� %d � ����� %s';
  SRowDataIsNotAvailable                   = '���������� ������ ������';
  SResolverIsNotSpecified                  = '��� ������� ������ ������ (ResultSet) �� ����� Resolver';
  SResultsetIsAlreadyOpened                = '����� ������ (Resultset) ��� ������';
  SCanNotUpdateEmptyRow                    = '���������� �������� ������ ������';
  SCanNotUpdateDeletedRow                  = '���������� �������� �������� ������';
  SCanNotDeleteEmptyRow                    = '���������� ������� ������ ������';
  SCannotUseCommit                         = '���������� ������������ COMMIT � AUTOCOMMIT ������';
  SCannotUseRollBack                       = '���������� ������������ ROLLBACK � AUTOCOMMIT ������';
  SCanNotUpdateComplexQuery                = '���������� �������� ����������� ������ � ����� ��� ����� ��������';
  SCanNotUpdateThisQueryType               = '���������� �������� ���� ��� �������';
  SDriverWasNotFound                       = '��������� ������� �� �� ������';
  SCanNotConnectToServer                   = '���������� ������������ � SQL �������';
  STableIsNotSpecified                     = '������� �� ������';
  SLiveResultSetsAreNotSupported           = '����� ����� ������ �� �������������� ���� �������';
  SInvalidInputParameterCount              = '���������� ������� ���������� is ������ ��� ���������';
  SIsolationIsNotSupported                 = '������� �������� ����������� �� ��������������';
  SColumnWasNotFound                       = '�� ������ ������� � ������ "%s"';
  SWrongTypeForBlobParameter               = '�������� ��� ��� Blob ����������';
  SIncorrectConnectionURL                  = '�������� ���� (URL) ��� �����������: %s';
  SUnsupportedProtocol                     = '���������������� ��������: %s';
  SUnsupportedByDriver                     = '������� �� ������������ ������ ����������� : [%s]';

  SConnectionIsNotOpened                   = '����������� �� �������';
  SInvalidOpInAutoCommit                   = '�������� �������� � ������ ����������������� (AutoCommit)';
  SInvalidOpInNonAutoCommit                = '�������� �������� � ������ �� ����������������� (non AutoCommit)';
  SInvalidOpPrepare                        = '���������� ����������� �������� ������ ��� ������ �������������(!) StartTransaction';

  SConnectionIsNotAssigned                 = '����������� � �� �� ������';
  SQueryIsEmpty                            = 'SQL ������ ����';
  SCanNotExecuteMoreQueries                = '���������� ��������� ����� ������ �������';
  SOperationIsNotAllowed1                  = '�������� �� �������������� � ������ ������ ����� (FORWARD ONLY)';
  SOperationIsNotAllowed2                  = '�������� �� �������������� � ������ ������ ��� ������ (READ ONLY)';
  SOperationIsNotAllowed3                  = '�������� �� �������������� � %s ������';
  SOperationIsNotAllowed4                  = '�������� �� �������������� �� �������� ������ ������';
  SNoMoreRecords                           = '� ������ ������ (Resultset) ��� �������';
  SCanNotOpenResultSet                     = '���������� ������� ����� ������ (Resultset)';
  SCanNotOpenDataSetWhenDestroying ='Translate : Cannot open a dataset when the componentstate is dsDestroying';
  SCircularLink                            = '�������� ������ (Datasource) ����� ����������� ������';
  SBookmarkWasNotFound                     = '������� (Bookmark) �� �������';
  SIncorrectSearchFieldsNumber             = '������������ �����  Incorrect number of search field values';
  SInvalidOperationInTrans                 = '�������� �������� � ������ �����������';
  SIncorrectSymbol                         = '�������� ������ � ������ ����� "%s".';
  SIncorrectToken                          = '�������� ���� ����� ":"';
  SIncorrectParamChar = 'TRANSLATE : Invalid value for ParamChar';

  SSelectedTransactionIsolation            = '��������� ������� �������� ����������� �� ��������������';
  SDriverNotSupported                      = '������� �� �������������� %s';
  SPattern2Long                            = '������� ������� �������';
  SDriverNotCapableOutParameters           = '������� �� �������� ��������� �����������';
  SStatementIsNotAllowed                   = '��������� �� ��������������';
  SStoredProcIsNotAllowed                  = '�������� ��������� �� ���������';
  SCannotPerformOperation                  = '���������� ��������� �������� �� �������� ������ ������ (Resultset)';
  SInvalidState                            = '�������� ���������';
  SErrorConvertion                         = '������ ��������������';
  SDataTypeDoesNotSupported                = '��� ������ �� ��������������';
  SUnsupportedParameterType                = '���������������� ��� ���������';
  SUnsupportedDataType                     = '���������������� ��� ������';
  SErrorConvertionField                    = '������ ����������� ��� ���� "%s" � SQLType "%s"';
  SBadOCI                                  = '�������� ������ OCI [%s]. ����������� ������ 8.0.3 ��� ����';
  SConnect2AsUser                          = '���������� ������������ � "%s" ������������� "%s"';
  SUnknownError                            = '����������� ������';
  SFieldNotFound1                          = '���� "%s" �� �������';
  SFieldNotFound2                          = '���� %d �� �������';

  SLoginPromptFailure                      = '���������� ����� ������ ����������� �� ����������. �������� ������ DBLogDlg � ������ uses �������� ������������ ������.';

  SPropertyQuery                           = '��� ����� ���� ��������� ������ ���� �� ������� The Query may last a while on large databases!';
  SPropertyTables                          = '������� ���������� ���������(Catalog) �/��� ������ (Schema)';
  SPropertyColumns                         = '������� ���������� ��������� (Catalog), ������ (Schema) �/��� �������� (TableName).';
  SPropertyProcedures                      = '������� ���������� ���������(Catalog) �/��� ������ (Schema).';
  SPropertySequences                       = '������� ���������� ���������(Catalog) �/��� ������ (Schema).';
  SPropertyExecute                         = '�� ����� ��������� ������ ?';

  SFormTest                                = 'ZEOS SQL ���� ���������';
  SButtonClose                             = '&�������';
  SFormEditor                              = 'ZEOS SQL ��������';
  STabSheetSelect                          = '����� SQL';
  SMenuLoad                                = '���������';
  SMenuSave                                = '���������';
  SButtonGenerate                          = '&�������������';
  SButtonCheck                             = '�&��������';
  SButtonTest                              = '&����';
  SButtonOk                                = '&��';
  SButtonCancel                            = '&������';
  STableAlias                              = '�&�������� �������';
  SReplaceSQL                              = '&�������� SQL';
  SDialogOpenTitle                         = '������� SQL ����';
  SDialogSaveTitle                         = '��������� SQL ����';
  SSQLEditor                               = 'SQL ��������';
  SDatabaseDialog                          = '������� ������������ ��';

  SUpdateSQLNoResult                       = '� ���������� ���������� (Refresh) ������ �� ��������';
  SUpdateSQLRefreshStatementcount          = 'Refresh ������ ������ ���� ������ ����';

{$IFDEF FPC}
  SNotEditing                              = '����� ������ (Dataset) �� � ������ �������������� ��� �������';
  SFieldTypeMismatch                       = '������������ ���� ��� ���� ''%s'', ��������� %s ������: %s';
  SFieldSizeMismatch                       = '������ ���� ''%s'' �� ���������, ���������: %d ������: %d';
{$ENDIF}
  SNeedField               = 'Translate: Field %s is required, but not supplied.';

  SFailedtoInitPrepStmt                    = '��������� ���������������� �������������� ���������';
  SFailedtoPrepareStmt                     = '������ ���������� ��������� � �������� ����������';
  SFailedToBindAllValues                   = '������ ��� ���-����������� ��������';
  SAttemptExecOnBadPrep                    = '������� ��������� ��������� �� �������� ����������.';
  SBindingFailure                          = '������ ��� ���������� ���������';
  SPreparedStmtExecFailure                 = '��������� ��������� �������������� ���������';
  SBoundVarStrIndexMissing                 = '����������� �� ����� � �������� "%s" �� ����������';
  SBindVarOutOfRange                       = '������ ����������� ����� �� ������� : %d';
  SFailedToBindResults                     = '��������� �������(bind) ��������� ����������';

  SRefreshRowOnlySupportedWithUpdateObject = '����� ���������� ������ (RefreshRow) �������������� ������ ��� ���������� �������';
  SMustBeInBrowseMode                      = '�������� ������������ ������ � ������ ��������� (dsBROWSE)';

  SUnKnownParamDataType                    = '����������� ���� ��������� (Param.DataType)';
  //--- end added by ORMADA ----------------------------------------------------
  SFieldReadOnly          = 'Translate : Readonly field can''t be assigned a value: %d';
  SInvalidUpdateCount     = 'Translate : %d record(s) updated. Only one record should have been updated.';

  SRowBufferWidthExceeded ='Translate: Row buffer width exceeded. Try using fewer or longer columns in SQL query.';
{$ELSE}

//--- added by Petr Stasiak - pestasoft.com ------------------------------------
{$IFDEF CZECH}
  SSQLError1 = 'SQL chyba: %s';
  SSQLError2 = 'SQL chyba: %s k�d: %d';
  SSQLError3 = 'SQL chyba: %s k�d: %d SQL: %s';
  SSQLError4 = 'SQL chyba: %s k�d: %d Hl�en�: %s';

  SListCapacityError = 'Kapacita seznamu je mimo rozsah (%d)';
  SListCountError = 'Po�et seznam� je mimo rozsah (%d)';
  SListIndexError = 'Index v seznamu je mimo rozsah (%d)';

  SClonningIsNotSupported = 'Klonov�n� nen� v t�to t��d� podporov�no';
  SImmutableOpIsNotAllowed = 'Tato operace nen� povolena na nem�niteln� "collections"';
  SStackIsEmpty = 'Z�sobn�k je pr�zdn�';
  SVariableWasNotFound = 'Prom�n� "%s" neexistuje';
  SFunctionWasNotFound = 'Funkce "%s" neexistuje';
  SInternalError = 'Intern� chyba';
  SSyntaxErrorNear = 'Chybn� syntaxe "%s"';
  SSyntaxError = 'Chybn� syntaxe';
  SUnknownSymbol = 'Nezn�m� symbol "%s"';
  SUnexpectedExprEnd = 'Neo�ek�van� konec v�razu';
  SRightBraceExpected = ') o�ek�v�n(o/a/y)';
  SParametersError = '%d parametr� o�ek�v�no, ale %d existuje';
  SExpectedMoreParams = 'Je o�ek�v�no v�ce, ne� 2 parametry';
  SInvalidVarByteArray = 'Nespr�vn� VarByte array';
  SVariableAlreadyExists = 'Prom�n� "%s" ji� existuje';
  STypesMismatch = 'Nesouhlasn� typy';
  SUnsupportedVariantType = 'Nepodporovan� typ variant';
  SUnsupportedOperation = 'Nepodporovan� operace';

  STokenizerIsNotDefined = 'Nen� definov�n "Tokenizer"';
  SLibraryNotFound = 'Neexistuje dll knihovna(y): %s';
  SEncodeDateIsNotSupported = 'Tato verze nepodporuje isc_encode_sql_date';
  SEncodeTimeIsNotSupported = 'Tato verze nepodporuje isc_encode_sql_time';
  SEncodeTimestampIsNotSupported = 'Tato verze nepodporuje isc_encode_sql_timestamp';
  SDecodeDateIsNotSupported = 'Tato verze nepodporuje isc_decode_sql_date';
  SDecodeTimeIsNotSupported = 'Tato verze nepodporuje isc_decode_sql_time';
  SDecodeTimestampIsNotSupported = 'Tato verze nepodporuje isc_decode_sql_timestamp';

  SCanNotRetrieveResultSetData = 'Nelze z�skat data "Resultset"';
  SRowBufferIsNotAssigned = 'Nen� p�i�azen ��dkov� buffer';
  SColumnIsNotAccessable = 'Sloupec s indexem %d nen� p��stupn�';
  SConvertionIsNotPossible = 'P�evod sloupce %d  nen� mo�n� z %s na %s';
  SCanNotAccessBlobRecord = 'Nelze p�istupovat k blob z�znamu ze zloupce %d p�es typ %s';
  SRowDataIsNotAvailable = '��dkov� data nejsou p��stupn�';
  SResolverIsNotSpecified = 'Nen� specifikov�n "rozklada�" pro tento v�sledek';
  SResultsetIsAlreadyOpened = '"Resultset" byl ji� otev�en';
  SCanNotUpdateEmptyRow = 'Nelze aktualizovat pr�zdn� ��dek';
  SCanNotUpdateDeletedRow = 'Nelze aktualizovat smazan� ��dek';
  SCanNotDeleteEmptyRow = 'Nelze vymazat pr�zdn� ��dek';
  SCannotUseCommit = 'Nepou��vejte COMMIT v m�du AUTOCOMMIT';
  SCannotUseRollBack = 'Nelze pou��t ROLLBACK v AUTOCOMMIT m�du';
  SCanNotUpdateComplexQuery = 'Nelze aktualizovat komplexn� dotaz pro v�ce, ne� jednu tabulku';
  SCanNotUpdateThisQueryType = 'Nelze aktualizovat tento typ dotazu';
  SDriverWasNotFound = 'Po�adovan� datab�zov� ovlada� nenalezen';
  SCanNotConnectToServer = 'Nezda�ilo se p�ipojen� k SQL serveru';
  STableIsNotSpecified = 'Tabulka nen� specifikov�na';
  SLiveResultSetsAreNotSupported = '"�iv�" dotaz nen� podporov�n v t�to t��d�';
  SInvalidInputParameterCount = 'Po�et vstupn�ch parametr� neodpov�d� o�ek�van�mu po�tu';
  SIsolationIsNotSupported = 'M�ra izolace transakce nen� podporov�na';
  SColumnWasNotFound = 'Sloupec s n�zvem "%s" neexistuje';
  SWrongTypeForBlobParameter = 'Nespr�vn� typ pro Blob parametr';
  SIncorrectConnectionURL = 'Nespr�vn� tvar URL adresy: %s';
  SUnsupportedProtocol = 'Nepodporovan� protokol: %s';
  SUnsupportedByDriver    = 'Ovlada� nepodporuje tuto vlastnost: [%s]';

  SConnectionIsNotOpened = 'Spojen� nen� otev�eno';
  SInvalidOpInAutoCommit = 'Nespr�vn� operace v m�du AutoCommit';
  SInvalidOpInNonAutoCommit = 'Nespr�vn� operace v m�du NE AutoCommit';
  SInvalidOpPrepare = '"Prepare" transakce je mo�n� pouze jako prvn�! Starttransaction';

  SConnectionIsNotAssigned = 'Nen� p�i�azen komponent "connection"';
  SQueryIsEmpty = 'SQL dotaz je pr�zdn�';
  SCanNotExecuteMoreQueries = 'Nelze spustit v�ce, ne� 1 dotaz';
  SOperationIsNotAllowed1 = 'Operace nen� povolena v m�du "FORWARD ONLY"';
  SOperationIsNotAllowed2 = 'Operace nen� povolena v m�du "READ ONLY"';
  SOperationIsNotAllowed3 = 'Operace nen� povolena v m�du "%s"';
  SOperationIsNotAllowed4 = 'Operace nen� povolena pro zav�en� zdroj dat (dataset)';
  SNoMoreRecords = 'Nejsou dal�� z�znamy';
  SCanNotOpenResultSet = 'Nelze otev��t v�sledek dotazu';
  SCanNotOpenDataSetWhenDestroying ='Translate : Cannot open a dataset when the componentstate is dsDestroying';
  SCircularLink = 'Datasource vytv��� cyklick� dotaz';
  SBookmarkWasNotFound = 'Z�lo�ka neexistuje';
  SIncorrectSearchFieldsNumber = 'Nespr�vn� po�et vyhled�van�ch polo�ek';
  SInvalidOperationInTrans = 'Nespr�vn� operace v explicitn�m transak�n�m m�du';
  SIncorrectSymbol = 'Nespr�vn� symbol v seznamu polo�ek "%s".';
  SIncorrectToken = 'Za ":" n�sleduje nespr�vn� znak';
  SIncorrectParamChar = 'TRANSLATE : Invalid value for ParamChar';

  SSelectedTransactionIsolation = 'Vybran� m�ra izolace transakc� nen� podporov�na';
  SDriverNotSupported = 'Ovlada� %s nen� podporov�n';
  SPattern2Long = 'Pattern je p��li� dlouh�';
  SDriverNotCapableOutParameters = 'Ovlada� nen� schopen p�ij�mat parametry';
  SStatementIsNotAllowed = 'P��kaz nen� povolen';
  SStoredProcIsNotAllowed = '"stored proc" nen� povolena';
  SCannotPerformOperation = 'Nelze prov�st operaci na uzav�en�m v�sledku dotazu (Resultset)';
  SInvalidState = 'Nespr�vn� stav';
  SErrorConvertion = 'Chyba p�evodu';
  SDataTypeDoesNotSupported = 'Tento typ dat nen� podporov�n';
  SUnsupportedParameterType = 'Nepodporovan� typ parametru';
  SUnsupportedDataType = 'Nepodporovan� typ dat';
  SErrorConvertionField = 'Chyba p�evodu sloupce "%s" na SQLTyp "%s"';
  SBadOCI = '�patn� verze OCI [%s]. Je vy�adov�na 8.0.3 nebo star��';
  SConnect2AsUser = 'P�ipojit k "%s" jako "%s"';
  SUnknownError = 'Nezn�m� chyba';
  SFieldNotFound1 = 'Sloupec "%s" neexistuje';
  SFieldNotFound2 = 'Sloupec %d neexistuje';

  SLoginPromptFailure = 'Nelze naj�t v�choz� p�ihla�ovac� dialog. Pros�m p�idejte DBLogDlg do sekce USES va�eho zdrojov�ho souboru.';

  SPropertyQuery = 'Dotaz m��e b�t posledn� u vlelk�ch datab�z�!';
  SPropertyTables = 'M�lo by b�t limitov�no katalogen a/nebo sch�matem.';
  SPropertyColumns = 'M�lo by b�t limitov�no katalogem, sch�matem a/nebo n�zvem tabulky.';
  SPropertyProcedures = 'M�lo by b�t limitov�no katalogen a/nebo sch�matem.';
  SPropertySequences = 'M�lo by b�t limitov�no katalogen a/nebo sch�matem.';
  SPropertyExecute = 'M� se dotaz p�esto vykonat?';

  SFormTest = 'ZEOS SQL Editor Test';
  SButtonClose = '&Zav��t';
  SFormEditor = 'ZEOS SQL Editor';
  STabSheetSelect = 'Select SQL';
  SMenuLoad = 'Na��st';
  SMenuSave = 'Ulo�it';
  SButtonGenerate = '&Generovat';
  SButtonCheck = '&Kontrola';
  SButtonTest = '&Test';
  SButtonOk = '&OK';
  SButtonCancel = 'Z&ru�it';
  STableAlias = '&Alias tabulky';
  SReplaceSQL = 'Nah&radit SQL';
  SDialogOpenTitle = 'Otev��t SQL soubor';
  SDialogSaveTitle = 'Ulo�it SQL soubor';
  SSQLEditor = 'SQL Editor';
  SDatabaseDialog = 'Otev��t existuj�c� datab�zi';

  SUpdateSQLNoResult = 'Update Refresh SQL nevr�tilo ��dn� v�sledek';
  SUpdateSQLRefreshStatementcount ='Po�et Update Refresh SQL p��kaz� mus� b�t 1';

  {$IFDEF FPC}
  SNotEditing = 'Dataset nen� v edita�n�m (edit), ani vkl�dac�m (insert) re�imu';
  SFieldTypeMismatch = 'Nespr�vn� typ pro sloupec ''%s'', o�ek�v�no: %s aktu�ln�: %s';
  SFieldSizeMismatch = 'Nespr�vn� velikost sloupce ''%s'', o�ek�v�no: %d aktu�ln�: %d';
  {$ENDIF}
  SNeedField               = 'Sloupce %s je po�adov�n, ale nezad�n.';

  SFailedtoInitPrepStmt   = 'P�ipravovan� p��kaz nelze inicializovat';
  SFailedtoPrepareStmt    = 'P��kaz selhal b�hem p��pravy procesu';
  SFailedToBindAllValues  = 'Aplikace zkolabovala p�ed p��pravou v�ech hodnot';
  SAttemptExecOnBadPrep   = 'Pokou��te sespustit p��kaz p�ed dokon�en�m jeho p��pravy.';
  SBindingFailure         = 'Chyba p�i z�sk�v�n� sady parametr�';
  SPreparedStmtExecFailure = 'P�ipravovan� p��kaz selhal p�i vykon�v�n�';
  SBoundVarStrIndexMissing = 'Index textov� prom�n� "%s" neexistuje';
  SBindVarOutOfRange      = 'Index promen� je mimo rozsah: %d';
  SFailedToBindResults    = 'Aplikace selhala p�i z�sk�v�n� v�sledk� dotazu';

//FOS+ 07112006
  SRefreshRowOnlySupportedWithUpdateObject = 'Metoda "refreshrow" je podporov�na pouze v "update object"';
  SMustBeInBrowseMode = 'Operace je povolena pouze ve stavu dsBROWSE';

  SUnKnownParamDataType = 'Nezn�m� parametr.typ dat (Param.DataType)';
  SFieldReadOnly        = 'Sloupec pouze pro �ten� nem��e b�t p�i�azen k hodnot�: %d';
  SInvalidUpdateCount     = '%d z�znam(�) aktualizov�no. Pouze jeden z�znam byl zm�n�n.';

  SRowBufferWidthExceeded ='Translate: Row buffer width exceeded. Try using fewer or longer columns in SQL query.';
//--- end added by Petr Stasiak - pestasoft.com ------------------------------------

{$ELSE}

//--- added by pawelsel --------------------------------------------------------
{$IFDEF POLISH}

  SSQLError1 = 'B��d SQL: %s';
  SSQLError2 = 'B��d SQL: %s Kod: %d';
  SSQLError3 = 'B��d SQL: %s Kod: %d SQL: %s';
  SSQLError4 = 'B��d SQL: %s Kod: %d Komunikat: %s';

  SListCapacityError = 'Przekroczona pojemno�� listy (%d)';
  SListCountError = 'Licznik listy poza zakresem (%d)';
  SListIndexError = 'Indeks listy poza zakresem (%d)';

  SClonningIsNotSupported = 'Ta klasa nie obs�uguje klonowania';
  SImmutableOpIsNotAllowed = 'Niedozwolona operacja na niezmienialnych kolekcjach';
  SStackIsEmpty = 'Stos jest pusty';
  SVariableWasNotFound = 'Nie znaleziono zmiennej "%s"';
  SFunctionWasNotFound = 'Nie znaleziono funkcji "%s"';
  SInternalError = 'B��d wewn�trzny';
  SSyntaxErrorNear = 'B��d sk�adni przy "%s"';
  SSyntaxError = 'B��d sk�adni';
  SUnknownSymbol = 'Nieznany symbol "%s"';
  SUnexpectedExprEnd = 'Nieoczekiwany koniec wyra�enia';
  SRightBraceExpected = 'Oczekiwano znaku )';
  SParametersError = 'Oczekiwana ilo�� parametr�w: %d, znaleziono: %d';
  SExpectedMoreParams = 'Oczekiwano wi�cej ni� dwa parametry';
  SInvalidVarByteArray = 'B��dna tablica VarByte';
  SVariableAlreadyExists = 'Zmienna "%s" ju� istnieje';
  STypesMismatch = 'Niezgodno�� typ�w';
  SUnsupportedVariantType = 'Nieznany typ danych';
  SUnsupportedOperation = 'Nieznana operacja';

  STokenizerIsNotDefined = 'Nie zdefiniowano tokenizera';
  SLibraryNotFound = 'Nie znaleziono �adnej z bibliotek dynamicznych: %s';
  SEncodeDateIsNotSupported = 'Ta wersja nie obs�uguje isc_encode_sql_date';
  SEncodeTimeIsNotSupported = 'Ta wersja nie obs�uguje isc_encode_sql_time';
  SEncodeTimestampIsNotSupported = 'Ta wersja nie obs�uguje isc_encode_sql_timestamp';
  SDecodeDateIsNotSupported = 'Ta wersja nie obs�uguje isc_decode_sql_date';
  SDecodeTimeIsNotSupported = 'Ta wersja nie obs�uguje isc_decode_sql_time';
  SDecodeTimestampIsNotSupported = 'Ta wersja nie obs�uguje isc_decode_sql_timestamp';

  SCanNotRetrieveResultSetData = 'Nie mo�na pobra� danych wynikowych';
  SRowBufferIsNotAssigned = 'Nie przypisano bufora wiersza';
  SColumnIsNotAccessable = 'Kolumna o numerze %d jest niedost�pna';
  SConvertionIsNotPossible = 'Konwersja kolumny o numerze %d z %s na %s jest niemo�liwa';
  SCanNotAccessBlobRecord = 'Brak dost�pu do rekordu typu blob w kolumnie %d z typem %s';
  SRowDataIsNotAvailable = 'Dane wiersza s� niedost�pne';
  SResolverIsNotSpecified = 'Ten ResultSet nie ma okre�lonego Resolver-a';
  SResultsetIsAlreadyOpened = 'ResultSet jest ju� otwarty';
  SCanNotUpdateEmptyRow = 'Nie mo�na aktualizowa� pustego wiersza';
  SCanNotUpdateDeletedRow = 'Nie mo�na aktualizowa� usuni�tego wiersza';
  SCanNotDeleteEmptyRow = 'Nie mo�na usun�� pustego wiersza';
  SCannotUseCommit = 'Nie mo�na u�y� COMMIT w trybie AUTOCOMMIT';
  SCannotUseRollBack = 'Nie mo�na u�y� ROLLBACK w trybie AUTOCOMMIT';
  SCanNotUpdateComplexQuery = 'Nie mo�na aktualizowa� zapytania z�o�onego z wi�cej ni� jednej tabeli';
  SCanNotUpdateThisQueryType = 'Nie mo�na aktualizowa� tego typu zapytania';
  SDriverWasNotFound = 'Nie znaleziono wymaganego sterownika bazy danych';
  SCanNotConnectToServer = 'Nie mo�na po��czy� si� z serwerem SQL';
  STableIsNotSpecified = 'Nie okre�lono tabeli';
  SLiveResultSetsAreNotSupported = '"Live query" nie jest obs�ugiwane przez t� klas�';
  SInvalidInputParameterCount = 'Liczba parametr�w wejsciowych jest mniejsza ni� oczekiwana';
  SIsolationIsNotSupported = 'Poziom izolacji transakcji nie jest obs�ugiwany';
  SColumnWasNotFound = 'Nie znaleziono kolumny o nazwie "%s"';
  SWrongTypeForBlobParameter = 'B��dny typ parametru Blob';
  SIncorrectConnectionURL = 'B��dny URL po��czenia: %s';
  SUnsupportedProtocol = 'Nieobs�ugiwany protok�: %s';
  SUnsupportedByDriver    = 'Sterownik nie obs�uguje tej w�a�ciwo�ci natywnie: [%s]';

  SConnectionIsNotOpened = 'Jeszcze nie nawi�zano po��czenia';
  SInvalidOpInAutoCommit = 'B��dna operacja w trybie AutoCommit';
  SInvalidOpInNonAutoCommit = 'B��dna operacja przy wy��czonym AutoCommit';
  SInvalidOpPrepare = 'Przygotowanie transakcji mo�liwe jest tylko przy pierwszym(!) Starttransaction';

  SConnectionIsNotAssigned = 'Nie przypisano komponentu po��czenia do bazy danych';
  SQueryIsEmpty = 'Zapytanie SQL jest puste';
  SCanNotExecuteMoreQueries = 'Nie mo�na wykona� wi�cej ni� jednego zapytania';
  SOperationIsNotAllowed1 = 'Niedozwolona operacja w trybie FORWARD ONLY';
  SOperationIsNotAllowed2 = 'Niedozwolona operacja w trybie READ ONLY';
  SOperationIsNotAllowed3 = 'Niedozwolona operacja w trybie %s';
  SOperationIsNotAllowed4 = 'Niedozwolona operacja przy zamni�tym �r�dle danych';
  SNoMoreRecords = 'Nie ma ju� wi�cej rekord�w wynikowych';
  SCanNotOpenResultSet = 'Nie mozna otworzy� danych wynikowych';
  SCanNotOpenDataSetWhenDestroying ='Translate : Cannot open a dataset when the componentstate is dsDestroying';
  SCircularLink = 'Datasource tworzy powi�zanie cykliczne';
  SBookmarkWasNotFound = 'Nie znaleziono zak�adki (Bookmark)';
  SIncorrectSearchFieldsNumber = 'B��dna liczba p�l do wyszukiwania';
  SInvalidOperationInTrans = 'B��dna operacja w trybie transakcji';
  SIncorrectSymbol = 'B��dny symbol w li�cie p�l "%s".';
  SIncorrectToken = 'B��dny wyraz za ":"';
  SIncorrectParamChar = 'TRANSLATE : Invalid value for ParamChar';

  SSelectedTransactionIsolation = 'Wybrany poziom izolacji transakcji nie jest obs�ugiwany';
  SDriverNotSupported = 'Nie obs�ugiwany sterownik %s';
  SPattern2Long = 'Wzorzec jest zbyt d�ugi';
  SDriverNotCapableOutParameters = 'Sterownik nie potrafi obs�u�y� parametr�w';
  SStatementIsNotAllowed = 'Niedozwolone wyra�enie';
  SStoredProcIsNotAllowed = 'Niedozwolona procedura sk�adowana';
  SCannotPerformOperation = 'Nie mo�na wykona� operacji na zamkni�tym zbiorze danych';
  SInvalidState = 'B��dny stan';
  SErrorConvertion = 'B��d konwersji';
  SDataTypeDoesNotSupported = 'Nieobs�ugiwany typ dannych';
  SUnsupportedParameterType = 'Nieobs�ugiwany typ parametru';
  SUnsupportedDataType = 'Nieobs�ugiwany typ danych';
  SErrorConvertionField = 'B��d konwersji pola "%s" na SQLType "%s"';
  SBadOCI = 'Z�a wersja OCI [%s]. Wymagana wersja 8.0.3 lub starsza';
  SConnect2AsUser = 'Po��czenie z "%s" jako u�ytkownik "%s"';
  SUnknownError = 'Nieznany b��d';
  SFieldNotFound1 = 'Nie znaleziono pola "%s"';
  SFieldNotFound2 = 'Nie znaleziono pola %d';

  SLoginPromptFailure = 'Nie znaleziono domy�lnego dialogu logowania. Prosz� doda� DBLogDlg do sekcji uses g��wnego pliku aplikacji.';

  SPropertyQuery = 'Zapytanie mo�e chwil� potrwa� na wi�kszej bazie danych!';
  SPropertyTables = 'Powiniene� u�ci�li� Katalog i/lub Schemat.';
  SPropertyColumns = 'Powiniene� u�ci�li� Katalog, Schemat i/lub Nazw�Tabeli.';
  SPropertyProcedures = 'Powiniene� u�ci�li� Katalog i/lub Schemat.';
  SPropertySequences = 'Powiniene� u�ci�li� Katalog i/lub Schemat.';
  SPropertyExecute = 'Czy mimo to wykona� zapytanie?';

  SFormTest = 'Test Edytora SQL ZEOS';
  SButtonClose = '&Zamknij';
  SFormEditor = 'Edytor SQL ZEOS';
  STabSheetSelect = 'Wyb�r SQL';
  SMenuLoad = '�aduj';
  SMenuSave = 'Zapisz';
  SButtonGenerate = '&Generuj';
  SButtonCheck = '&Sprawd�';
  SButtonTest = '&Test';
  SButtonOk = '&OK';
  SButtonCancel = 'A&nuluj';
  STableAlias = '&Alias tabeli';
  SReplaceSQL = 'Za&mie� SQL';
  SDialogOpenTitle = 'Otw�rz plik SQL';
  SDialogSaveTitle = 'Zapisz plik SQL';
  SSQLEditor = 'Edytor SQL';
  SDatabaseDialog = 'Otw�rz istniej�c� baz�';

  SUpdateSQLNoResult = 'Update Refresh SQL nie zwr�ci�o �adnych danych';
  SUpdateSQLRefreshStatementcount ='Wyra�enie Update Refresh SQL musi zwr�ci� 1 rekord danych';

  {$IFDEF FPC}
  SNotEditing = 'Dataset nie jest w trybie "edit" lub "insert"';
  SFieldTypeMismatch = 'Niezgodno�� typ�w dla pola ''%s'', oczekiwano: %s otrzymano: %s';
  SFieldSizeMismatch = 'Niezgodno�� rozmiar�w pola ''%s'', oczekiwano: %d otrzymano: %d';
  {$ENDIF}
  SNeedField               = 'Pole %s jest wymagane.';

  SFailedtoInitPrepStmt   = 'Nie uda�o si� zainicjalizowa� przygotowanego zapytania';
  SFailedtoPrepareStmt    = 'B��d w wyra�eniu podczas procesu przygotowania';
  SFailedToBindAllValues  = 'B��d aplikacji podczas przypisywania danych';
  SAttemptExecOnBadPrep   = 'Pr�ba uruchomienia wyra�enia przed zako�czeniem przygotowywania.';
  SBindingFailure         = 'B��d przypisywania zbioru parametr�w';
  SPreparedStmtExecFailure = 'B��d wykonania przygotowanego zapytania';
  SBoundVarStrIndexMissing = 'Nie istnieje zmienna licznikowa "%s"';
  SBindVarOutOfRange      = 'Warto�� zmiennej licznikowej poza zakresem: %d';
  SFailedToBindResults    = 'B��d aplikacji podczas ��czenia do wynik�w zapytania';

//FOS+ 07112006
  SRefreshRowOnlySupportedWithUpdateObject = 'Metoda refreshrow jest obs�ugiwana tylko przez obiekt typu "update"';
  SMustBeInBrowseMode = 'Operacja jest dozwolona tylko w stanie dsBROWSE';

  SUnKnownParamDataType = 'Nieznany Param.DataType';
  SFieldReadOnly        = 'Nie mo�na przypisa� do pola tylko do odczytu warto�ci: %d';
  SInvalidUpdateCount     = 'Liczba zaktualizowanych rekord�w: %d. tylko jeden rekord powinien by� zaktualizowany.';

  SRowBufferWidthExceeded ='Translate: Row buffer width exceeded. Try using fewer or longer columns in SQL query.';

{$ELSE} // default: ENGLISH


  SSQLError1 = 'SQL Error: %s';
  SSQLError2 = 'SQL Error: %s Code: %d';
  SSQLError3 = 'SQL Error: %s Code: %d SQL: %s';
  SSQLError4 = 'SQL Error: %s Code: %d Message: %s';

  SListCapacityError = 'List capacity out of bounds (%d)';
  SListCountError = 'List count out of bounds (%d)';
  SListIndexError = 'List index out of bounds (%d)';

  SClonningIsNotSupported = 'Clonning is not supported by this class';
  SImmutableOpIsNotAllowed = 'The operation is not allowed on not changeable collections';
  SStackIsEmpty = 'Stack is empty';
  SVariableWasNotFound = 'Variable "%s" was not found';
  SFunctionWasNotFound = 'Function "%s" was not found';
  SInternalError = 'Internal error';
  SSyntaxErrorNear = 'Syntax error near "%s"';
  SSyntaxError = 'Syntax error';
  SUnknownSymbol = 'Unknown symbol "%s"';
  SUnexpectedExprEnd = 'Unexpected end of expression';
  SRightBraceExpected = ') expected';
  SParametersError = '%d parameters were expected but %d were found';
  SExpectedMoreParams = 'More than two parameters are expected';
  SInvalidVarByteArray = 'Invalid VarByte array';
  SVariableAlreadyExists = 'Variable "%s" already exists';
  STypesMismatch = 'Types mismatch';
  SUnsupportedVariantType = 'Unsupported variant type';
  SUnsupportedOperation = 'Unsupported operation';

  STokenizerIsNotDefined = 'Tokenizer is not defined';
  SLibraryNotFound = 'None of the dynamic libraries can be found: %s';
  SEncodeDateIsNotSupported = 'This version does not support isc_encode_sql_date';
  SEncodeTimeIsNotSupported = 'This version does not support isc_encode_sql_time';
  SEncodeTimestampIsNotSupported = 'This version does not support isc_encode_sql_timestamp';
  SDecodeDateIsNotSupported = 'This version does not support isc_decode_sql_date';
  SDecodeTimeIsNotSupported = 'This version does not support isc_decode_sql_time';
  SDecodeTimestampIsNotSupported = 'This version does not support isc_decode_sql_timestamp';

  SCanNotRetrieveResultSetData = 'Cannot retrieve Resultset data';
  SRowBufferIsNotAssigned = 'Row buffer is not assigned';
  SColumnIsNotAccessable = 'Column with index %d is not accessable';
  SConvertionIsNotPossible = 'Convertion is not possible for column %d from %s to %s';
  SCanNotAccessBlobRecord = 'Cannot access blob record in column %d with type %s';
  SRowDataIsNotAvailable = 'Row data is not available';
  SResolverIsNotSpecified = 'Resolver is not specified for this ResultSet';
  SResultsetIsAlreadyOpened = 'Resultset is already open';
  SCanNotUpdateEmptyRow = 'Cannot update an empty row';
  SCanNotUpdateDeletedRow = 'Cannot update a deleted row';
  SCanNotDeleteEmptyRow = 'Cannot delete an empty row';
  SCannotUseCommit = 'You cannot use COMMIT in AUTOCOMMIT mode';
  SCannotUseRollBack = 'You cannot use ROLLBACK in AUTOCOMMIT mode';
  SCanNotUpdateComplexQuery = 'Cannot update a complex query with more then one table';
  SCanNotUpdateThisQueryType = 'Cannot update this query type';
  SDriverWasNotFound = 'Requested database driver was not found';
  SCanNotConnectToServer = 'Cannot connect to SQL server';
  STableIsNotSpecified = 'Table is not specified';
  SLiveResultSetsAreNotSupported = 'Live query is not supported by this class';
  SInvalidInputParameterCount = 'Input parameter count is less then expected';
  SIsolationIsNotSupported = 'Transaction isolation level is not supported';
  SColumnWasNotFound = 'Column with name "%s" was not found';
  SWrongTypeForBlobParameter = 'Wrong type for Blob parameter';
  SIncorrectConnectionURL = 'Incorrect connection URL: %s';
  SUnsupportedProtocol = 'Unsupported protocol: %s';
  SUnsupportedByDriver    = 'Driver can not support this feature natively: [%s]';

  SConnectionIsNotOpened = 'Connection is not opened yet';
  SInvalidOpInAutoCommit = 'Invalid operation in AutoCommit mode';
  SInvalidOpInNonAutoCommit = 'Invalid operation in non AutoCommit mode';
  SInvalidOpPrepare = 'Prepare transaction only possible on matching first(!) Starttransaction';

  SConnectionIsNotAssigned = 'Database connection component is not assigned';
  SQueryIsEmpty = 'SQL Query is empty';
  SCanNotExecuteMoreQueries = 'Cannot execute more then one query';
  SOperationIsNotAllowed1 = 'Operation is not allowed in FORWARD ONLY mode';
  SOperationIsNotAllowed2 = 'Operation is not allowed in READ ONLY mode';
  SOperationIsNotAllowed3 = 'Operation is not allowed in %s mode';
  SOperationIsNotAllowed4 = 'Operation is not allowed for closed dataset';
  SNoMoreRecords = 'No more records in the Resultset';
  SCanNotOpenResultSet = 'Can not open a Resultset';
  SCanNotOpenDataSetWhenDestroying ='Cannot open a dataset when the componentstate is dsDestroying';
  SCircularLink = 'Datasource makes a circular link';
  SBookmarkWasNotFound = 'Bookmark was not found';
  SIncorrectSearchFieldsNumber = 'Incorrect number of search field values';
  SInvalidOperationInTrans = 'Invalid operation in explicit transaction mode';
  SIncorrectSymbol = 'Incorrect symbol in field list "%s".';
  SIncorrectToken = 'Incorrect token followed by ":"';
  SIncorrectParamChar = 'Invalid value for ParamChar';

  SSelectedTransactionIsolation = 'Selected transaction isolation level is not supported';
  SDriverNotSupported = 'Driver not supported %s';
  SPattern2Long = 'Pattern is too long';
  SDriverNotCapableOutParameters = 'Driver is not capable to handle parameters';
  SStatementIsNotAllowed = 'Statement is not allowed';
  SStoredProcIsNotAllowed = 'The stored proc is not allowed';
  SCannotPerformOperation = 'Can not perform operation on closed Resultset';
  SInvalidState = 'Invalid state';
  SErrorConvertion = 'Convertion error';
  SDataTypeDoesNotSupported = 'Data type is not supported';
  SUnsupportedParameterType = 'Unsupported parameter type';
  SUnsupportedDataType = 'Unsupported data type';
  SErrorConvertionField = 'Conversion error for field "%s" to SQLType "%s"';
  SBadOCI = 'Bad OCI version [%s]. Version 8.0.3 or older is required';
  SConnect2AsUser = 'Connect to "%s" as user "%s"';
  SUnknownError = 'Unknown error';
  SFieldNotFound1 = 'Field "%s" was not found';
  SFieldNotFound2 = 'Field %d was not found';

  SLoginPromptFailure = 'Can not find default login prompt dialog. Please add DBLogDlg to the uses section of your main file.';

  SPropertyQuery = 'The Query may last a while on large databases!';
  SPropertyTables = 'You should limit it by Catalog and/or Schema.';
  SPropertyColumns = 'You should limit it by Catalog, Schema and/or TableName.';
  SPropertyProcedures = 'You should limit it by Catalog and/or Schema.';
  SPropertySequences = 'You should limit it by Catalog and/or Schema.';
  SPropertyExecute = 'Should the Query be executed anyway?';

  SFormTest = 'ZEOS SQL Editor Test';
  SButtonClose = '&Close';
  SFormEditor = 'ZEOS SQL Editor';
  STabSheetSelect = 'Select SQL';
  SMenuLoad = 'Load';
  SMenuSave = 'Save';
  SButtonGenerate = '&Generate';
  SButtonCheck = 'C&heck';
  SButtonTest = '&Test';
  SButtonOk = '&OK';
  SButtonCancel = '&Cancel';
  STableAlias = 'T&able alias';
  SReplaceSQL = '&Replace SQL';
  SDialogOpenTitle = 'Open SQL File';
  SDialogSaveTitle = 'Save SQL File';
  SSQLEditor = 'SQL Editor';
  SDatabaseDialog = 'Open existing database';

  SUpdateSQLNoResult = 'Update Refresh SQL delivered no resultset';
  SUpdateSQLRefreshStatementcount ='Update Refresh SQL Statement count must be 1';

  {$IFDEF FPC}
  SNotEditing = 'Dataset not in edit or insert mode';
  SFieldTypeMismatch = 'Type mismatch for field ''%s'', expecting: %s actual: %s';
  SFieldSizeMismatch = 'Size mismatch for field ''%s'', expecting: %d actual: %d';
  {$ENDIF}
  SNeedField               = 'Field %s is required, but not supplied.';

  SFailedtoInitPrepStmt   = 'Prepared statement failed to initialize';
  SFailedtoPrepareStmt    = 'Statement failed during prepare process';
  SFailedToBindAllValues  = 'Application failed to pre-bind all values';
  SAttemptExecOnBadPrep   = 'Attempt made to execute a statement before a successful preparation.';
  SBindingFailure         = 'Failed to bind parameter set';
  SPreparedStmtExecFailure = 'Prepared statement failed to execute';
  SBoundVarStrIndexMissing = 'Bound variable text index "%s" does not exist';
  SBindVarOutOfRange      = 'Bound variable index out of range: %d';
  SFailedToBindResults    = 'Application failed to bind to the result set';

//FOS+ 07112006
  SRefreshRowOnlySupportedWithUpdateObject = 'The refreshrow method is only supported with an update object';
  SMustBeInBrowseMode = 'Operation is only allowed in dsBROWSE state';

  SUnKnownParamDataType = 'Unknown Param.DataType';
  SFieldReadOnly        = 'Readonly field can''t be assigned a value: %d';
  SInvalidUpdateCount     = '%d record(s) updated. Only one record should have been updated.';

  SRowBufferWidthExceeded ='Row buffer width exceeded. Try using fewer or longer columns in SQL query.';

{$ENDIF} // POLISH

{$ENDIF} // CZECH

{$ENDIF} // RUSSIAN

{$ENDIF}   // INDONESIAN <--- added by tohenk

{$ENDIF}   // ROMANA

{$ENDIF} //SPANISH

{$ENDIF} // GERMAN

{$ENDIF} // DUTCH

{$ENDIF} // PORTUGUESE

implementation

end.




