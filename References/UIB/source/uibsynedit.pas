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
(*                                                                              *)
(********************************************************************************)

unit uibsynedit;

{$I uib.inc}

interface
uses
{$IFDEF MSWINDOWS}
  Classes, Graphics, SysUtils, SynEditHighlighter, SynEditKeyCmds,
  SynEditMiscClasses, SynEditSearch, SynAutoCorrect, SynEditTypes,
  SynHighlighterHashEntries, SynEditStrConst, uibmetadata;
{$ELSE}
  Classes, QGraphics, SysUtils, QSynEditHighlighter, QSynEditKeyCmds,
  QSynEditMiscClasses, QSynEditSearch, QSynAutoCorrect, QSynEditTypes,
  QSynHighlighterHashEntries, QSynEditStrConst, uibmetadata;
{$ENDIF}

type
  TRangeState = (rsUnknown, rsComment, rsString);

  TProcTableProc = procedure of object;
  TUIBTokenKind = (tkComment, tkDatatype,
    tkFunction, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace, tkString,
    tkSymbol, tkUnknown, tkVariable, tkException, tkGenerator, tkUDF, tkView,
    tkProcedure, tkRole, tkTable, tkDomain, tkTrigger);

  PIdentifierTable = ^TIdentifierTable;
  TIdentifierTable = array[Char] of ByteBool;

  PHashTable = ^THashTable;
  THashTable = array[Char] of Integer;

  TSQLCompInfo = record
    Token: string;
    Comp: string;
  end;

  TUIBSQLHighliter = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FLine: PChar;
    FLineNumber: Integer;
    FProcTable: array[#0..#255] of TProcTableProc;
    FRun: LongInt;
    FStringLen: Integer;
    FToIdent: PChar;
    FTokenPos: Integer;
    FTokenID: TUIBTokenKind;
    FKeywords: TSynHashEntryList;
    FTokenAttri: array[TUIBTokenKind] of TSynHighlighterAttributes;
    FIdentifiersPtr: PIdentifierTable;
    FHashTablePtr: PHashTable;
    FIdentifiers: TIdentifierTable;
    FHashTable: THashTable;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure SymbolAssignProc;
    procedure VariableProc;
    procedure UnknownProc;
    function IdentKind(MayBe: PChar): TUIBTokenKind;
    procedure MakeMethodTables;
    procedure AnsiCProc;
    procedure DoAddKeyword(AKeyword: string; AKind: integer);
    procedure MakeIdentTable;
  protected
    function GetIdentChars: TSynIdentChars; override;
  public
    class function GetLanguageName: string; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddMetaKeyWord(const AKeyword: string; AKind: TUIBTokenKind);
    procedure BeginUpdate;
    procedure EnUpdate;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TUIBTokenKind;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    function IsKeyword(const AKeyword: string): boolean; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetLine(NewValue: string; LineNumber: Integer); override;
    procedure SetRange(Value: Pointer); override;
  end;

type
  TNodeInfo = record
    Icon: Integer;
    color: TColor;
    token: TUIBTokenKind;
  end;

const
   NodeInfos: array[TMetaNodeType] of TNodeInfo = (
     (icon: 0;  color : clBlack;   Token: tkUnknown),   // MetaNode,
     (icon: 1;  color : clBlack;   Token: tkUnknown),   // MetaDatabase
     (icon: 7;  color : clRed;     Token: tkException), // MetaException
     (icon: 6;  color : clMaroon;  Token: tkGenerator), // MetaGenerator
     (icon: 15; color : clBlack;   Token: tkUnknown),   // MetaCheck
     (icon: 14; color : clTeal;    Token: tkTrigger),   // MetaTrigger
     (icon: 8;  color : clBlue;    Token: tkUDF),       // MetaUDF
     (icon: 4;  color : clGreen;   Token: tkView),      // MetaView
     (icon: 5;  color : clTeal;    Token: tkProcedure), // MetaProcedure
     (icon: 9;  color : clBlack;   Token: tkRole),      // MetaRole
     (icon: 3;  color : clGreen;   Token: tkTable),     // MetaTable
     (icon: 10; color : clNavy;    Token: tkUnknown),   // MetaBaseField
     (icon: 10; color : clNavy;    Token: tkUnknown),   //   MetaUDFField
     (icon: 10; color : clNavy;    Token: tkUnknown),   //   MetaField
     (icon: 10; color : clNavy;    Token: tkUnknown),   //     MetaProcInField
     (icon: 10; color : clNavy;    Token: tkUnknown),   //     MetaProcOutField
     (icon: 10; color : clNavy;    Token: tkUnknown),   //     MetaTableField
     (icon: 2;  color : clOlive;   Token: tkDomain),    //       MetaDomain
     (icon: 0;  color : clBlack;   Token: tkUnknown),   // MetaConstraint
     (icon: 12; color : clBlack;   Token: tkUnknown),   //   MetaForeign
     (icon: 17;  color : clBlack;  Token: tkUnknown),   //   MetaIndex
     (icon: 11; color : clBlack;   Token: tkUnknown),   //   MetaPrimary
     (icon: 16; color : clBlack;   Token: tkUnknown),   //   MetaUnique
     (icon: 9;  color : clBlack;   Token: tkUnknown),   // MetaGrant,
     (icon: 9;  color : clBlack;   Token: tkUnknown),   //   MetaRoleGrant,
     (icon: 9;  color : clBlack;   Token: tkUnknown),   //   MetaTableGrant,
     (icon: 9;  color : clBlack;   Token: tkUnknown),   //   MetaFieldGrant,
     (icon: 9;  color : clBlack;   Token: tkUnknown),   //   MetaProcedureGrant,
     (icon: 9;  color : clBlack;   Token: tkUnknown),   // MetaGrantee,
     (icon: 9;  color : clBlack;   Token: tkUnknown),   //   MetaUserGrantee,
     (icon: 9;  color : clBlack;   Token: tkUnknown),   //   MetaRoleGrantee,
     (icon: 9;  color : clBlack;   Token: tkUnknown),   //   MetaProcedureGrantee,
     (icon: 9;  color : clBlack;   Token: tkUnknown),   //   MetaTriggerGrantee,
     (icon: 9;  color : clBlack;   Token: tkUnknown)    //   MetaViewGrantee
   );

  (* See uibkeywords.pas
  SQLToKens: array[0..273] of string =
   ('ACTION','ACTIVE','ADD','ADMIN','AFTER','ALL','ALTER','AND','ANY','AS','ASC',
    'ASCENDING','AT','AUTO','AVG','BACKUP','BASE_NAME','BEFORE','BEGIN','BETWEEN',
    'BIGINT','BIT_LENGTH','BLOB','BLOCK','BOOLEAN','BOTH','BREAK','BY','CACHE',
    'CASCADE','CASE','CAST','CHAR','CHAR_LENGTH','CHARACTER','CHARACTER_LENGTH',
    'CHECK','CHECK_POINT_LENGTH','CLOSE','COALESCE','COLLATE','COLLATION','COLUMN',
    'COMMENT','COMMIT','COMMITTED','COMPUTED','CONDITIONAL','CONSTRAINT','CONTAINING',
    'COUNT','CREATE','CROSS','CSTRING','CURRENT','CURRENT_CONNECTION','CURRENT_DATE',
    'CURRENT_ROLE','CURRENT_TIME','CURRENT_TIMESTAMP','CURRENT_TRANSACTION',
    'CURRENT_USER','CURSOR','DATABASE','DATE','DAY','DEBUG','DEC','DECIMAL','DECLARE',
    'DEFAULT','DELETE','DELETING','DESC','DESCENDING','DESCRIPTOR','DIFFERENCE',
    'DISTINCT','DO','DOMAIN','DOUBLE','DROP','ELSE','END','ENTRY_POINT','ESCAPE',
    'EXCEPTION','EXECUTE','EXISTS','EXIT','EXTERNAL','EXTRACT','FETCH','FILE',
    'FILTER','FIRST','FLOAT','FOR','FOREIGN','FREE_IT','FROM','FULL','FUNCTION',
    'GDSCODE','GEN_ID','GENERATOR','GLOBAL','GRANT','GROUP','GROUP_COMMIT_WAIT_TIME',
    'HAVING','HOUR','IF','IIF','IN','INACTIVE','INDEX','INNER','INPUT_TYPE','INSERT',
    'INSERTING','INT','INTEGER','INTO','IS','ISOLATION','JOIN','KEY','LAST','LEADING',
    'LEAVE','LEFT','LENGTH','LEVEL','LIKE','LOCK','LOG_BUFFER_SIZE','LOGFILE','LONG',
    'LOWER','MANUAL','MAX','MAXIMUM_SEGMENT','MERGE','MESSAGE','MIN','MINUTE',
    'MODULE_NAME','MONTH','NAMES','NATIONAL','NATURAL','NCHAR','NEXT','NO','NOT',
    'NULL','NULLIF','NULLS','NUM_LOG_BUFFERS','NUMERIC','OCTET_LENGTH','OF','ON',
    'ONLY','OPEN','OPTION','OR','ORDER','OUTER','OUTPUT_TYPE','OVERFLOW','PAGE',
    'PAGE_SIZE','PAGES','PARAMETER','PASSWORD','PERCENT','PLAN','POSITION','POST_EVENT',
    'PRECISION','PRESERVE','PRIMARY','PRIVILEGES','PROCEDURE','PROTECTED',
    'RAW_PARTITIONS','RDB$DB_KEY','READ','REAL','RECORD_VERSION','RECREATE','REFERENCES',
    'RELEASE','RESERV','RESERVING','RESTART','RESTRICT','RETAIN','RETURNING',
    'RETURNING_VALUES','RETURNS','REVOKE','RIGHT','ROLE','ROLLBACK','ROW_COUNT',
    'ROWS','SAVEPOINT','SCALAR_ARRAY','SCHEMA','SECOND','SEGMENT','SELECT','SEQUENCE',
    'SET','SHADOW','SHARED','SINGULAR','SIZE','SKIP','SMALLINT','SNAPSHOT','SOME',
    'SORT','SQLCODE','STABILITY','STARTING','STARTS','STATEMENT','STATISTICS',
    'SUB_TYPE','SUBSTRING','SUM','SUSPEND','TABLE','TEMPORARY','THEN','TIES','TIME',
    'TIMESTAMP','TO','TRAILING','TRANSACTION','TRIGGER','TRIM','TYPE','UNCOMMITTED',
    'UNION','UNIQUE','UPDATE','UPDATING','UPPER','USER','USING','VALUE','VALUES',
    'VARCHAR','VARIABLE','VARYING','VIEW','WAIT','WEEKDAY','WHEN','WHERE','WHILE',
    'WITH','WORK','WRITE','YEAR','YEARDAY','FALSE','TRUE');
   *)

   SQLComp: array[0..20] of TSQLCompInfo =
   ((Token: 'GEN_ID'; Comp: '"<generator>,", "<step>"'),
    (Token: 'CAST'; Comp: '"<value> AS <datatype>"'),
    (Token: 'DECIMAL'; Comp: '"<precision>,", "<scale>"'),
    (Token: 'NUMERIC'; Comp: '"<precision>,", "<scale>"'),
    (Token: 'CHAR'; Comp: '"<count>"'),
    (Token: 'VARCHAR'; Comp: '"<count>"'),
    (Token: 'CSTRING'; Comp: '"<count>"'),
    (Token: 'AVG'; Comp: '"[ALL] <value>"'#13'"DISTINCT <value>"'),
    (Token: 'MAX'; Comp: '"[ALL] <value>"'#13'"DISTINCT <value>"'),
    (Token: 'MIN'; Comp: '"[ALL] <value>"'#13'"DISTINCT <value>"'),
    (Token: 'SUM'; Comp: '"[ALL] <value>"'#13'"DISTINCT <value>"'),
    (Token: 'COUNT'; Comp: '"*"'#13'"[ALL] <value>"'#13'"DISTINCT <value>"'),
    (Token: 'UPPER'; Comp: '"<string expr>"'),
    (Token: 'LOWER'; Comp: '"<string expr>"'),
    (Token: 'BIT_LENGTH'; Comp: '"<string expr>"'),
    (Token: 'OCTET_LENGTH'; Comp: '"<string expr>"'),
    (Token: 'CHARACTER_LENGTH'; Comp: '"<string expr>"'),
    (Token: 'CHAR_LENGTH'; Comp: '"<string expr>"'),
    (Token: 'TRIM'; comp: '"<left paren> [[LEADING | TRAILING | BOTH] [<trim char>] FROM]"'#13 +
       '"<value expr> <right paren> <trim spec> ::= <trim char> ::= <value expr>"'),
    (Token: 'EXTRACT'; Comp: '"part FROM value"'),
    (Token: 'SUBSTRING'; Comp: '"<string expr> FROM <pos> [FOR <length>]"'));

  FIELDSFORMAT = '\image{%d} \color{clNavy}Field\color{clBlack} \style{+B}%s\style{-B} %s';
  PARAMSFORMAT = '\image{%d} \color{clNavy}Param\color{clBlack} \style{+B}%s\style{-B} %s';
  UDFPARAM = '\image{%d} \color{clNavy}Param\color{clBlack} %s';
  SQLITEMSTR     = 'SQL \style{+B}%s\style{-B}';
  CHARSETITEMSTR = 'Charset \style{+B}%s\style{-B}';


  function GetParams(Node: TMetaNode): string;
  function AliasToTable(var sql, alias, table: string): boolean;

implementation

uses
  uibkeywords;

const
  TokenName: array[TUIBTokenKind] of string = ('Comment', 'Datatype', 'Function',
    'Identifier', 'Key', 'Null', 'Number', 'Space', 'String', 'Symbol',
    'Unknown', 'Variable', 'Exception', 'Generator', 'UDF', 'View', 'Procedure',
    'Role', 'Table', 'Domain', 'Trigger');

  SQLFunctions : array[0..13] of String = (
    'AVG','CAST','COUNT','GEN_ID','MAX','MIN','SUM','UPPER','LOWER','TRIM',
    'BIT_LENGTH','OCTET_LENGTH','CHARACTER_LENGTH','CHAR_LENGTH');

  // types
  SQLTypes : array[0..14] of String = ('BLOB','CHAR','CHARACTER','DATE',
    'DECIMAL','DOUBLE','FLOAT','INTEGER','NUMERIC','SMALLINT','TIME','TIMESTAMP',
    'VARCHAR','BIGINT','BOOLEAN');

(*

  // functions
  InterbaseFunctions = 'AVG,CAST,COUNT,GEN_ID,MAX,MIN,SUM,UPPER,LOWER,TRIM,BIT_LENGTH,OCTET_LENGTH,CHARACTER_LENGTH,CHAR_LENGTH';

  // types
  InterbaseTypes = 'BLOB,CHAR,CHARACTER,DATE,DECIMAL,DOUBLE,FLOAT,INTEGER,' +
    'NUMERIC,SMALLINT,TIME,TIMESTAMP,VARCHAR,BIGINT,BOOLEAN';

  InterbaseKW =
    'ACTION,ACTIVE,ADD,ADMIN,AFTER,ALL,ALTER,AND,ANY,AS,ASC,'+
    'ASCENDING,AT,AUTO,BACKUP,BASE_NAME,BEFORE,BEGIN,BETWEEN,'+
    'BLOCK,BOTH,BREAK,BY,CACHE,'+
    'CASCADE,CASE,'+
    'CHECK,CHECK_POINT_LENGTH,CLOSE,COALESCE,COLLATE,COLLATION,COLUMN,'+
    'COMMENT,COMMIT,COMMITTED,COMPUTED,CONDITIONAL,CONSTRAINT,CONTAINING,'+
    'CREATE,CROSS,CSTRING,CURRENT,CURRENT_CONNECTION,CURRENT_DATE,'+
    'CURRENT_ROLE,CURRENT_TIME,CURRENT_TIMESTAMP,CURRENT_TRANSACTION,'+
    'CURRENT_USER,CURSOR,DATABASE,DAY,DEBUG,DEC,DECLARE,'+
    'DEFAULT,DELETE,DELETING,DESC,DESCENDING,DESCRIPTOR,DIFFERENCE,'+
    'DISTINCT,DO,DOMAIN,DROP,ELSE,END,ENTRY_POINT,ESCAPE,'+
    'EXCEPTION,EXECUTE,EXISTS,EXIT,EXTERNAL,EXTRACT,FETCH,FILE,'+
    'FILTER,FIRST,FOR,FOREIGN,FREE_IT,FROM,FULL,FUNCTION,'+
    'GDSCODE,GENERATOR,GLOBAL,GRANT,GROUP,GROUP_COMMIT_WAIT_TIME,'+
    'HAVING,HOUR,IF,IIF,IN,INACTIVE,INDEX,INNER,INPUT_TYPE,INSERT,'+
    'INSERTING,INT,INTO,IS,ISOLATION,JOIN,KEY,LAST,LEADING,'+
    'LEAVE,LEFT,LENGTH,LEVEL,LIKE,LOCK,LOG_BUFFER_SIZE,LOGFILE,LONG,'+
    'MANUAL,MAXIMUM_SEGMENT,MERGE,MESSAGE,MINUTE,'+
    'MODULE_NAME,MONTH,NAMES,NATIONAL,NATURAL,NCHAR,NEXT,NO,NOT,'+
    'NULL,NULLIF,NULLS,NUM_LOG_BUFFERS,OF,ON,'+
    'ONLY,OPEN,OPTION,OR,ORDER,OUTER,OUTPUT_TYPE,OVERFLOW,PAGE,'+
    'PAGE_SIZE,PAGES,PARAMETER,PASSWORD,PERCENT,PLAN,POSITION,POST_EVENT,'+
    'PRECISION,PRESERVE,PRIMARY,PRIVILEGES,PROCEDURE,PROTECTED,'+
    'RAW_PARTITIONS,RDB$DB_KEY,READ,REAL,RECORD_VERSION,RECREATE,REFERENCES,'+
    'RELEASE,RESERV,RESERVING,RESTART,RESTRICT,RETAIN,RETURNING,'+
    'RETURNING_VALUES,RETURNS,REVOKE,RIGHT,ROLE,ROLLBACK,ROW_COUNT,'+
    'ROWS,SAVEPOINT,SCALAR_ARRAY,SCHEMA,SECOND,SEGMENT,SELECT,SEQUENCE,'+
    'SET,SHADOW,SHARED,SINGULAR,SIZE,SKIP,SNAPSHOT,SOME,'+
    'SORT,SQLCODE,STABILITY,STARTING,STARTS,STATEMENT,STATISTICS,'+
    'SUB_TYPE,SUBSTRING,SUSPEND,TABLE,TEMPORARY,THEN,TIES,'+
    'TO,TRAILING,TRANSACTION,TRIGGER,TYPE,UNCOMMITTED,'+
    'UNION,UNIQUE,UPDATE,UPDATING,USER,USING,VALUE,VALUES,'+
    'VARIABLE,VARYING,VIEW,WAIT,WEEKDAY,WHEN,WHERE,WHILE,'+
    'WITH,WORK,WRITE,YEAR,YEARDAY,FALSE,TRUE';
  *)

  function AliasToTable(var sql, alias, table: string): boolean;
  var
    Src, p: PChar;
    tmp: PChar;
    len, plen: Integer;
    procedure SkipParams;
    begin
      inc(src);
      while (src^ <> #0) do
        case src^ of
          '(': SkipParams;
          ')': begin
                 inc(src);
                 Break;
               end;
        else
          inc(src);
        end;
    end;
  begin
    result := False;
    Len := Length(alias);
    Src := PChar(sql);
    p := nil;
    plen := 0;
    while (Src^ <> #0) do
      case Src^ of
        '/' : if Src[1] = '*' then
              begin
                inc(Src, 2);
                while (Src^ <> #0) do
                  if (Src^ = '*') and (Src[1] = '/') then
                  begin
                    inc(Src, 2);
                    Break;
                  end else
                    inc(Src);
              end else
                inc(src);
        '-' : if Src[1] = '-' then
              begin
                inc(Src, 2);
                while not(Src^ in [#0, #13, #10]) do
                  inc(Src);
              end else
                inc(Src);
        'a'..'z', 'A'..'Z', '_':
              begin
                tmp := src;
                while (src^ in ['a'..'z', 'A'..'Z', '0'..'9', '_', '$','.']) do inc(src);
                if ((src - tmp) = len) and (StrLIComp(PChar(alias), tmp, len) = 0) then
                begin
                  setlength(Table, plen);
                  Move(p^, PChar(Table)^, plen);
                  result := true;
                  exit;
                end;
                p := tmp;
                plen := (src - tmp);
              end;
        '(':  SkipParams; // skip procedure parametters
        else
          Inc(Src);
        end;

  end;

  function GetParams(Node: TMetaNode): string;
  var i: Integer;
  begin
    result := '';
    if (node is TMetaProcedure) then
    begin
      if TMetaProcedure(Node).InputFieldsCount > 0 then
      with TMetaProcedure(Node) do
      begin
        Result := format('(%s %s', [name, InputFields[0].ShortFieldType]);;
        for i := 1 to TMetaProcedure(Node).InputFieldsCount - 1 do
          Result := format('%s, %s %s', [Result, Name, InputFields[i].ShortFieldType]);
        Result := Result + ')';
      end
    end else
    if (node is TMetaUDF) then
    begin
      if TMetaUDF(Node).FieldsCount > 0 then
      begin
        if TMetaUDF(Node).Return = 0 then
          Result := '(' else
          Result := ' (' + TMetaUDF(Node).Fields[0].ShortFieldType + ',';
        for i := 1 to TMetaUDF(Node).FieldsCount - 1 do
          Result := Result + TMetaUDF(Node).Fields[i].ShortFieldType + ', ';
        setlength(result, length(result) - 2);
        Result := Result + ')';
        if TMetaUDF(Node).Return = 0 then
          Result := Result + ': ' + TMetaUDF(Node).Fields[0].ShortFieldType;
      end;
    end;
    Result := LowerCase(Result);
  end;

{ TUIBSQLHighliter }

procedure TUIBSQLHighliter.MakeIdentTable;
var
  c: char;
begin
  FillChar(FIdentifiers, SizeOf(FIdentifiers), #0);
  for c := 'a' to 'z' do
    FIdentifiers[c] := TRUE;
  for c := 'A' to 'Z' do
    FIdentifiers[c] := TRUE;
  for c := '0' to '9' do
    FIdentifiers[c] := TRUE;
  FIdentifiers['_'] := TRUE;
  FIdentifiers['#'] := TRUE;
  FIdentifiers['$'] := TRUE;

  FillChar(FHashTable, SizeOf(FHashTable), #0);
  FHashTable['_'] := 1;
  for c := 'a' to 'z' do
    FHashTable[c] := 2 + Ord(c) - Ord('a');
  for c := 'A' to 'Z' do
    FHashTable[c] := 2 + Ord(c) - Ord('A');
end;

function TUIBSQLHighliter.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while FIdentifiersPtr[ToHash^] do
  begin
{$IFOPT Q-}
    Result := 2 * Result + FHashTablePtr[ToHash^];
{$ELSE}
    Result := (2 * Result + FHashTablePtr[ToHash^]) and $FFFFFF;
{$ENDIF}
    inc(ToHash);
  end;
  Result := Result and $FF;
  FStringLen := ToHash - FToIdent;
end;

function TUIBSQLHighliter.KeyComp(const aKey: string): Boolean;
var
  i: integer;
  Key1, Key2: PChar;
begin
  Key1 := FToIdent;
  // Note: FStringLen is always > 0 !
  Key2 := pointer(aKey);
  for i := 1 to FStringLen do
  begin
    if FHashTable[Key1^] <> FHashTable[Key2^] then
    begin
      Result := FALSE;
      exit;
    end;
    Inc(Key1);
    Inc(Key2);
  end;
  Result := TRUE;
end;

function TUIBSQLHighliter.IdentKind(MayBe: PChar): TUIBTokenKind;
var
  Entry: TSynHashEntry;
begin
  FToIdent := MayBe;
  Entry := FKeywords[KeyHash(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > FStringLen then
      break else
    if Entry.KeywordLen = FStringLen then
      if KeyComp(Entry.Keyword) then
      begin
        Result := TUIBTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

procedure TUIBSQLHighliter.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0 : FProcTable[I] := NullProc;
      #10: FProcTable[I] := LFProc;
      #13: FProcTable[I] := CRProc;
      #39: FProcTable[I] := AsciiCharProc;
      '=': FProcTable[I] := EqualProc;
      '>': FProcTable[I] := GreaterProc;
      '<': FProcTable[I] := LowerProc;
      '-': FProcTable[I] := MinusProc;
      '|': FProcTable[I] := OrSymbolProc;
      '+': FProcTable[I] := PlusProc;
      '/': FProcTable[I] := SlashProc;
      '&': FProcTable[I] := AndSymbolProc;
      #34: FProcTable[I] := StringProc;
      ':', '@':
        FProcTable[I] := VariableProc;
      'A'..'Z', 'a'..'z', '_':
        FProcTable[I] := IdentProc;
      '0'..'9':
        FProcTable[I] := NumberProc;
      #1..#9, #11, #12, #14..#32:
        FProcTable[I] := SpaceProc;
      '^', '%', '*', '!':
        FProcTable[I] := SymbolAssignProc;
      '{', '}', '.', ',', ';', '?', '(', ')', '[', ']', '~':
        FProcTable[I] := SymbolProc;
      else
        FProcTable[I] := UnknownProc;
    end;
end;

constructor TUIBSQLHighliter.Create(AOwner: TComponent);
var t: TUIBTokenKind;
begin
  inherited Create(AOwner);
  MakeIdentTable;
  FKeywords := TSynHashEntryList.Create;

  for t := low(TUIBTokenKind) to High(TUIBTokenKind) do
  begin
    FTokenAttri[t] := TSynHighlighterAttributes.Create(TokenName[t]);
    AddAttribute(FTokenAttri[t]);
    with FTokenAttri[t] do
    case t of
      tkComment:
        begin
          Style := [fsItalic];
          Foreground := clNavy;
        end;
      tkDatatype, tkFunction, tkKey:
        Style := [fsBold];
      tkString:
        Foreground := clNavy;
      tkException : Foreground := NodeInfos[MetaException].color;
      tkGenerator : Foreground := NodeInfos[MetaGenerator].color;
      tkUDF       : Foreground := NodeInfos[MetaUDF].color;
      tkView      : Foreground := NodeInfos[MetaView].color;
      tkProcedure : Foreground := NodeInfos[MetaProcedure].color;
      tkRole      : Foreground := NodeInfos[MetaRole].color;
      tkTable     : Foreground := NodeInfos[MetaTable].color;
      tkDomain    : Foreground := NodeInfos[MetaDomain].color;
      tkTrigger   : Foreground := NodeInfos[MetaTrigger].color;
    end;
  end;

  SetAttributesOnChange(DefHighlightChange);
  MakeMethodTables;
  FDefaultFilter := SYNS_FilterSQL;
  FRange := rsUnknown;
  BeginUpdate;
  EndUpdate;
end;

destructor TUIBSQLHighliter.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

procedure TUIBSQLHighliter.SetLine(NewValue: string; LineNumber: Integer);
begin
  FLine := PChar(NewValue);
  FRun := 0;
  FLineNumber := LineNumber;
  Next;
end;

procedure TUIBSQLHighliter.AndSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if FLine[FRun] in ['=', '&'] then Inc(FRun);
end;

procedure TUIBSQLHighliter.AsciiCharProc;
begin
  // Oracle SQL allows strings to go over multiple lines
  if FLine[FRun] = #0 then
    NullProc else
  begin
    FTokenID := tkString;
    if (FRun > 0) or (FRange <> rsString) or (FLine[FRun] <> #39) then
    begin
      FRange := rsString;
      repeat
        Inc(FRun);
      until FLine[FRun] in [#0, #10, #13, #39];
    end;
    if (FLine[FRun] = #39) then
    begin
      Inc(FRun);
      FRange := rsUnknown;
    end;
  end;
end;

procedure TUIBSQLHighliter.CRProc;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then
    Inc(FRun);
end;

procedure TUIBSQLHighliter.EqualProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if FLine[FRun] in ['=', '>'] then
    Inc(FRun);
end;

procedure TUIBSQLHighliter.GreaterProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if (FLine[FRun] in ['=', '>']) then
    Inc(FRun);
end;

procedure TUIBSQLHighliter.IdentProc;
begin
  FTokenID := IdentKind((FLine + FRun));
  inc(FRun, FStringLen);
  if FTokenID = tkComment then
  begin
    while not (FLine[FRun] in [#0, #10, #13]) do
      Inc(FRun);
  end else
    while FIdentifiersPtr[FLine[FRun]] do inc(FRun);
end;

procedure TUIBSQLHighliter.LFProc;
begin
  FTokenID := tkSpace;
  inc(FRun);
end;

procedure TUIBSQLHighliter.LowerProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  case FLine[FRun] of
  '=': Inc(FRun);
  '<': begin
         Inc(FRun);
         if (FLine[FRun] = '=') then
           Inc(FRun);
       end;
  end;
end;

procedure TUIBSQLHighliter.MinusProc;
begin
  Inc(FRun);
  if FLine[FRun] = '-' then
  begin
    FTokenID := tkComment;
    repeat
      Inc(FRun);
    until (FLine[FRun] in [#0, #10, #13]);
  end else
    FTokenID := tkSymbol;
end;

procedure TUIBSQLHighliter.NullProc;
begin
  FTokenID := tkNull;
end;

procedure TUIBSQLHighliter.NumberProc;
begin
  inc(FRun);
  FTokenID := tkNumber;
  while FLine[FRun] in ['0'..'9', '.', '-'] do
  begin
    if (FLine[FRun] = '.') and (FLine[FRun + 1] = '.') then
      Break;
    inc(FRun);
  end;
end;

procedure TUIBSQLHighliter.OrSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if (FLine[FRun] in ['=', '|']) then
    Inc(FRun);
end;

procedure TUIBSQLHighliter.PlusProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if (FLine[FRun] in ['=', '+']) then
    Inc(FRun);
end;

procedure TUIBSQLHighliter.SlashProc;
begin
  Inc(FRun);
  case FLine[FRun] of
  '*':
    begin
      FRange := rsComment;
      FTokenID := tkComment;
      repeat
        Inc(FRun);
        if (FLine[FRun] = '*') and (FLine[FRun + 1] = '/') then
        begin
          FRange := rsUnknown;
          Inc(FRun, 2);
          break;
        end;
      until (FLine[FRun] in [#0, #10, #13]);
    end;
  '=':
    begin
      Inc(FRun);
      FTokenID := tkSymbol;
    end;
  else
    FTokenID := tkSymbol;
  end;
end;

procedure TUIBSQLHighliter.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(FRun);
  until (FLine[FRun] > #32) or (FLine[FRun] in [#0, #10, #13]);
end;

procedure TUIBSQLHighliter.StringProc;
begin
  FTokenID := tkString;
  Inc(FRun);
  while not (FLine[FRun] in [#0, #10, #13]) do
  begin
    case FLine[FRun] of
    '\': if (FLine[FRun + 1] = #34) then
           Inc(FRun);
    #34: if (FLine[FRun + 1] <> #34) then
         begin
           Inc(FRun);
           break;
         end;
    end;
    Inc(FRun);
  end;
end;

procedure TUIBSQLHighliter.SymbolProc;
begin
  Inc(FRun);
  FTokenID := tkSymbol;
end;

procedure TUIBSQLHighliter.SymbolAssignProc;
begin
  FTokenID := tkSymbol;
  Inc(FRun);
  if (FLine[FRun] = '=') then
    Inc(FRun);
end;

procedure TUIBSQLHighliter.VariableProc;
var
  i: integer;
begin
  if (FLine[FRun] = ':') then
    SymbolProc else
  begin
    FTokenID := tkVariable;
    i := FRun;
    repeat
      Inc(i);
    until not (FIdentifiersPtr[FLine[i]]);
    FRun := i;
  end;
end;

procedure TUIBSQLHighliter.UnknownProc;
begin
{$IFNDEF UNIX}
  if (FLine[FRun] in LeadBytes) then
    Inc(FRun,2) else
{$ENDIF}
    inc(FRun);
  FTokenID := tkUnknown;
end;

procedure TUIBSQLHighliter.AnsiCProc;
begin
  case FLine[FRun] of
    #0 : NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      FTokenID := tkComment;
      repeat
        if (FLine[FRun] = '*') and (FLine[FRun + 1] = '/') then
        begin
          FRange := rsUnknown;
          Inc(FRun, 2);
          break;
        end;
        Inc(FRun);
      until (FLine[FRun] in [#0, #10, #13]);
    end;
  end;
end;

function TUIBSQLHighliter.IsKeyword(const AKeyword: string): boolean;
var
  tk: TUIBTokenKind;
begin
  tk := IdentKind(PChar(AKeyword));
  Result := (tk in [tkDatatype, tkFunction, tkKey]);
end;

procedure TUIBSQLHighliter.Next;
begin
  FTokenPos := FRun;
  case FRange of
    rsComment: AnsiCProc;
    rsString : AsciiCharProc;
  else
    FProcTable[FLine[FRun]];
  end;
end;

function TUIBSQLHighliter.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT    : Result := FTokenAttri[tkComment];
    SYN_ATTR_IDENTIFIER : Result := FTokenAttri[tkIdentifier];
    SYN_ATTR_KEYWORD    : Result := FTokenAttri[tkKey];
    SYN_ATTR_STRING     : Result := FTokenAttri[tkString];
    SYN_ATTR_WHITESPACE : Result := FTokenAttri[tkSpace];
    SYN_ATTR_SYMBOL     : Result := FTokenAttri[tkSymbol];
  else
    Result := nil;
  end;
end;

function TUIBSQLHighliter.GetEol: Boolean;
begin
  Result := (FTokenID = tkNull);
end;

function TUIBSQLHighliter.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TUIBSQLHighliter.GetToken: string;
var
  Len: LongInt;
begin
  Len := (FRun - FTokenPos);
  Setstring(Result, (FLine + FTokenPos), Len);
end;

function TUIBSQLHighliter.GetTokenID: TUIBTokenKind;
begin
  Result := FTokenID;
end;

function TUIBSQLHighliter.GetTokenAttribute: TSynHighlighterAttributes;
begin
  Result := FTokenAttri[GetTokenID];
end;

function TUIBSQLHighliter.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TUIBSQLHighliter.GetTokenPos: Integer;
begin
  Result := FTokenPos;
end;

procedure TUIBSQLHighliter.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TUIBSQLHighliter.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TUIBSQLHighliter.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

class function TUIBSQLHighliter.GetLanguageName: string;
begin
  Result := SYNS_LangSQL;
end;

procedure TUIBSQLHighliter.DoAddKeyword(AKeyword: string; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := KeyHash(PChar(AKeyword));
  if FKeywords[HashValue] = nil then
    FKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

procedure TUIBSQLHighliter.BeginUpdate;
var
  i: Integer;
begin
  FKeywords.Clear;
  FIdentifiersPtr := @FIdentifiers;
  FHashTablePtr := @FHashTable;

  for i := Low(SQLTypes) to High(SQLTypes) do
    DoAddKeyword(SQLTypes[I],Ord(tkDatatype));

  for i := Low(SQLFunctions) to High(SQLFunctions) do
    DoAddKeyword(SQLFunctions[I],Ord(tkFunction));

  for i := Low(SQLToKens) to High(SQLToKens) do
    DoAddKeyword(SQLToKens[I],Ord(tkKey));

(*
  EnumerateKeywords(Ord(tkDatatype), InterbaseTypes, IdentChars, DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), InterbaseFunctions, IdentChars, DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), InterbaseKW, IdentChars, DoAddKeyword);
*)
end;

procedure TUIBSQLHighliter.EnUpdate;
begin
  DefHighlightChange(Self);
end;

procedure TUIBSQLHighliter.AddMetaKeyWord(const AKeyword: string;
  AKind: TUIBTokenKind);
var
  Entry: TSynHashEntry;
begin
  Entry := FKeywords[KeyHash(PChar(AKeyword))];
  while Assigned(Entry) do
  begin
    if (UpperCase(Entry.Keyword) = Uppercase(AKeyword)) then
      Break;
    Entry := Entry.Next;
  end;
  if not Assigned(Entry) then
    DoAddKeyword(AKeyword, Ord(AKind));
end;

initialization
  RegisterPlaceableHighlighter(TUIBSQLHighliter);
end.
