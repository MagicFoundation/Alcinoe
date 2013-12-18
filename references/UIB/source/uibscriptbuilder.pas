unit uibscriptbuilder;

(* 
  Génère un script d'insertion des données respectant les contraintes des
  clés étrangères.  
  Par Pierre Y. - pierrey@users.sourceforge.net
  
  Cas d'utilisation : Réplication de portions d'une base de données. 
  Pré-conditions : La base contient des tables, les relations entre les
    tables ont été créées. Les données exportées peuvent être filtrées
    en utilisant la propriété filter (clause "WHERE").    
*)

{$I uib.inc}

interface

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  SysUtils, Classes, Contnrs,
  uib, uiblib, uibase, uibmetadata;

type
  TOnProgressEvent = procedure(Sender: TObject; const TableName: String; Done: Integer) of object;

type
  TUIBScriptBuilder = class(TUIBComponent)
  private
    FDB: TUIBDataBase;

    FMetaDB: TMetaDataBase;

    FScript: TStrings;
    fFilter: String;

    FTables: TStack;
    FTablesDone: TList;

    function GetMetadatas: TMetaDataBase;
    function GetScript: TStrings;

    procedure ExportTable(Table: TMetaTable);
    procedure BuildInsertSQL(Table: TMetaTable; const SQL: String);

    function MakeFBDate(ADate: TDateTime): String;
    function MakeFBTimeStamp(ADateTime: TDateTime): String;
    function MakeFBTime(ATime: TDateTime): String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BuildInsertScript(const TableName: String);

    property Metadatas: TMetaDataBase read GetMetadatas;
  published
    property Database: TUIBDatabase read FDB write FDB;
    property Script: TStrings read GetScript;
    property Filter: String read fFilter write fFilter;
  end;

implementation

{ TUIBScriptBuilder }

constructor TUIBScriptBuilder.Create(AOwner: TComponent);
begin
  inherited;
  FScript := TStringList.Create;
  FTables := TStack.Create;
  FTablesDone := TList.Create;
end;

destructor TUIBScriptBuilder.Destroy;
begin
  if Assigned(FMetaDB) then
    FreeAndNil(FMetaDB);
  FreeAndNil(FTablesDone);
  FreeAndNil(FTables);
  FreeAndNil(FScript);
  inherited;
end;

procedure TUIBScriptBuilder.BuildInsertScript(const TableName: String);
var
  OrgTable: TMetaTable;
begin
  FScript.Add('/* -- Start of data insertion script -- */');
  FScript.Add('');

  { Efface la liste des tables exportées }
  FTablesDone.Clear;
  
  { Recherche la table dans les métadonnées de la base }
  with Metadatas do
    OrgTable := FindTableName(TableName);

  { Exporte la table d'après ses métadonnées }
  ExportTable(OrgTable);

  FScript.Add('/* -- End of data insertion script -- */');
end;

{ Renvoie les métadonnées associées à la base de données }
function TUIBScriptBuilder.GetMetadatas: TMetaDataBase;
var
  T: TUIBTransaction;
begin
  T := TUIBTransaction.Create(nil);
  try
    T.DataBase := FDB;
    Result := TMetaDataBase.Create(nil, -1);
    Result.LoadFromDatabase(T);
  finally
    T.Commit;
    FreeAndNil(T);
  end;
end;

{ Renvoie le script généré }
function TUIBScriptBuilder.GetScript: TStrings;
begin
  Result := FScript;
end;

function TUIBScriptBuilder.MakeFBDate(ADate: TDateTime): String;
begin
  Result := FormatDateTime('yyyy-mm-dd',ADate);
end;

function TUIBScriptBuilder.MakeFBTime(ATime: TDateTime): String;
begin
  Result := FormatDateTime('hh:nn:ss.zzz',ATime);
end;

function TUIBScriptBuilder.MakeFBTimeStamp(ADateTime: TDateTime): String;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',ADateTime);
end;

procedure TUIBScriptBuilder.BuildInsertSQL(Table: TMetaTable; const SQL: String);
var
  RdrTransaction: TUIBTransaction;
  RdrQuery: TUIBQuery;
  f: Integer;
  ins, vals: String;
begin
  FScript.Add('/* Data for table : ' + Table.Name + '*/');
  FScript.Add('');

  RdrTransaction := TUIBTransaction.Create(nil);
  { Requête qui lit dans la base d'origine }
  RdrQuery := TUIBQuery.Create(nil);
  try
    { Paramètrage des transactions }
    RdrTransaction.DataBase := FDB;

    { Initialise et ouvre la requête de lecture }
    RdrQuery.Transaction := RdrTransaction;
    RdrQuery.SQL.Text := SQL;
    RdrQuery.Open;

    if not RdrQuery.Eof then
    begin
      { Construit la requête d'écriture d'après les champs qui sont utilisés
        dans la requête de lecture }
      ins := format('INSERT INTO %s (%s', [Table.Name, RdrQuery.Fields.SqlName[0]]);
      for f := 1 to RdrQuery.Fields.FieldCount - 1 do
         ins := ins + ', ' + RdrQuery.Fields.SqlName[f];
      ins := ins + ')';

      while not RdrQuery.Eof do
      begin
        { remplir la partie VALUES en fonction du type des champs,
          quoter les strings par exemple et les champs blob de type 1 }
        vals := 'VALUES (';

        for f := 0 to RdrQuery.Fields.FieldCount - 1 do
        begin
          if RdrQuery.Fields.IsNull[f] then
            vals := vals + 'NULL'
          else
          begin
            case RdrQuery.Fields.FieldType[f] of
            uftNumeric,
            uftInteger,
            uftSmallint,
            uftFloat,
            uftDoublePrecision,
            uftInt64 :     vals := vals + RdrQuery.Fields.AsString[f];
            uftChar,
            uftVarchar,
            uftCstring :   vals := vals + QuotedStr(Trim(RdrQuery.Fields.AsString[f]));
            uftTimestamp : vals := vals + QuotedStr(MakeFBTimeStamp(RdrQuery.Fields.AsDateTime[f]));
            uftDate :      vals := vals + QuotedStr(MakeFBDate(RdrQuery.Fields.AsDateTime[f]));
            uftTime :      vals := vals + QuotedStr(MakeFBTime(RdrQuery.Fields.AsDateTime[f]));
            else
              vals := vals + 'NULL';
            end;
          end;

          vals := vals + ',';
        end;

        vals[Length(vals)] := ')';
        vals := vals + ';';

        { puis envoyer le tout dans le script }
        FScript.Add(ins);
        FScript.Add(vals);
        FScript.Add('');

        RdrQuery.Next;
      end;

    end;
    RdrQuery.Close(etmCommit);
  finally
    FreeAndNil(RdrQuery);
    FreeAndNil(RdrTransaction);
  end;
end;

procedure TUIBScriptBuilder.ExportTable(Table: TMetaTable);
var
  sql: String;
  i, f: Integer;
  TablePile: TMetaTable;
  iFK: Integer;
  OrgTable: TMetaTable;
  TempStack: TStack;
begin
  OrgTable := Table;

  for i := 0 to Table.ForeignCount - 1 do
  begin
    { On n'exporte pas les jointures auto-réflexives ni les tables qui
      ont déjà été exportées }
    if (Table.Foreign[i].ForTable.Name <> Table.Name) and
        (FTablesDone.IndexOf(Table.Foreign[i].ForTable) = -1) then
    begin
      FTables.Push(Table);
      ExportTable(Table.Foreign[i].ForTable);
      FTables.Pop;
    end;
  end;

  sql := 'select distinct ';

  i := 0;
  for f := 0 to Table.FieldsCount - 1 do
    if Table.Fields[f].ComputedSource = '' then
    begin
      if (i = 0) then
        sql := sql + Table.Name + '.' + Table.Fields[f].Name
      else
        sql := sql + ', ' + Table.Name + '.' + Table.Fields[f].Name;
      inc(i);
    end;
  sql := sql + ' from ' + Table.Name;

  TempStack := TStack.Create;
  while FTables.Count > 0 do
  begin
    TablePile := TMetaTable(FTables.Pop);
    iFK := 0;
    while TablePile.Foreign[iFK].ForTable.Name <> Table.Name do
      Inc(iFK);
    sql := sql + ' join ' + TablePile.Name + ' on (';
    for i := 0 to TablePile.Foreign[iFK].ForFieldsCount - 1 do
    begin
      with TablePile.Foreign[iFK] do
        sql := sql + TablePile.Name + '.' + Fields[i].Name + '=' + ForTable.Name + '.' + ForFields[i].Name + ' and ';
    end;
    Delete(sql,Length(sql) - 4,5);
    sql := sql + ')';

    { Sauvegarde la pile à l'envers }
    TempStack.Push(TablePile);
    Table := TablePile;
  end;

  { Réempile les tables jointes pour la prochaine utilisation }
  while TempStack.Count > 0 do
    FTables.Push(TempStack.Pop);

  TempStack.Free;

  if fFilter <> '' then
    sql := sql + ' where ' + fFilter;

  BuildInsertSQL(OrgTable, sql);

  { Ajoute la tabla a la liste des tables déjà exportées }
  FTablesDone.Add(OrgTable);
end;

end.

