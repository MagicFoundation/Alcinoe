unit uibcloner;

(* 
  Clone les données d'une base dans une structure équivalente 
  Par Pierre Y. - pierrey@users.sourceforge.net

  Ne duplique pas la structure de la base de données, voir l'exemple
  CloneDatabase fourni avec UIB. Basé sur le code de CloneDatabase.
  
  Cas d'utilisation : Archivage de données. 
  Pré-conditions : La base de données remplie, une base de données vide de même structure existe.
  
  Exemple d'utilisation :
  
  with TUIBCloner.Create(nil) do
  begin
    OrgDatabase := uibDbPleine;
    DestDatabasqe := uibDbArchive;
    
    DeActivateTriggers;
    try
      CloneTable('NOM_TABLE');
      CloneTable('FACTURES','select * from FACTURES where DATE_FACTURE between ''2006-01-01'' and ''2006-04-30''');
    finally
      ActivateTriggers;
      Free;
    end;
  end;
*)

{$I uib.inc}

interface

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  SysUtils,
  uib, uiblib, uibase, uibmetadata;

type
  TOnProgressEvent = procedure(Sender: TObject; const TableName: String; Done: Integer) of object;

type
  TUIBCloner = class(TUIBComponent)
  private
    FOrgDB: TUIBDataBase;
    FDestDB: TUIBDataBase;
    FOnProgress: TOnProgressEvent;

    FDestMetaDB: TMetaDataBase;
    FOrgMetaDB: TMetaDataBase;

    function GetDBMetadatas(ADB: TUIBDatabase): TMetaDataBase;

    function GetDestMetadatas: TMetaDataBase;
    function GetOrgMetadatas: TMetaDataBase;
  public
    destructor Destroy; override;
    
    procedure DeActivateTriggers;
    procedure ActivateTriggers;

    procedure CloneTable(const TableName: String;
                         const FromSQL: String = '');

    property OrgMetadatas: TMetaDataBase read GetOrgMetadatas;
    property DestMetadatas: TMetaDataBase read GetDestMetadatas;
  published
    property OrgDatabase: TUIBDatabase read FOrgDB write FOrgDB;
    property DestDatabase: TUIBDatabase read FDestDB write FDestDB;
    property OnProgress: TOnProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

{ TUIBCloner }

destructor TUIBCloner.Destroy;
begin
  if Assigned(FDestMetaDB) then
    FreeAndNil(FDestMetaDB);
  if Assigned(FOrgMetaDB) then
    FreeAndNil(FOrgMetaDB);

  inherited;
end;

{ Clone les données de TableName depuis OrgDB vers DestDB. On peut surcharger
  la requête SQL générée automatiquement en utilisant le paramètre FromSQL }
procedure TUIBCloner.CloneTable(const TableName, FromSQL: String);
var
  OrgTable: TMetaTable;
  sql: String;
  i,f,d: Integer;
  StHandle: IscStmtHandle;
  DbHandle: IscDbHandle;
  BlHandle: IscBlobHandle;
  TrHandle: IscTrHandle;
  RdrQuery: TUIBQuery;
  RdrTransaction: TUIBTransaction;
  WrtTransaction: TUIBTransaction;
begin
  { Objets transaction }
  RdrTransaction := TUIBTransaction.Create(nil);
  WrtTransaction := TUIBTransaction.Create(nil);
  { Requête qui lit dans la base d'origine }
  RdrQuery := TUIBQuery.Create(nil);
  try
    { Paramètrage des transactions }
    RdrTransaction.DataBase := FOrgDB;

    WrtTransaction.Database := FDestDB;
    WrtTransaction.StartTransaction;

    { Handles d'accès aux objets de la base de données }
    DbHandle := FDestDB.DbHandle;
    TrHandle := WrtTransaction.TrHandle;

    if FromSQL = '' then
    begin
     { Lit les métadonnées de la base pour construire les requêtes }
      with OrgMetadatas do
      begin
        { Construit la requête select d'après les métadonnées }
        OrgTable := FindTableName(TableName);

        sql := 'select ';
        i := 0;
        for f := 0 to OrgTable.FieldsCount - 1 do
          if OrgTable.Fields[f].ComputedSource = '' then
          begin
            if (i = 0) then
              sql := sql + OrgTable.Fields[f].Name
            else
              sql := sql + ', ' + OrgTable.Fields[f].Name;
            inc(i);
          end;
        sql := sql + ' from ' + TableName;
      end;
    end
    else
      sql := FromSQL;

    { Initialise et ouvre la requête de lecture }
    RdrQuery.Transaction := RdrTransaction;
    RdrQuery.SQL.Text := sql;
    RdrQuery.FetchBlobs := true;
    RdrQuery.Open;

    if not RdrQuery.Eof then
    begin
      { Construit la requête d'écriture d'après les champs qui sont utilisés
        dans la requête de lecture }
      sql := format('INSERT INTO %s (%s', [TableName, RdrQuery.Fields.SqlName[0]]);
      for f := 1 to RdrQuery.Fields.FieldCount - 1 do
         sql := sql + ', ' + RdrQuery.Fields.SqlName[f];
      sql := sql + ') VALUES (?';
      for f := 1 to RdrQuery.Fields.FieldCount - 1 do
        sql := sql + ',?';
      sql := sql + ');';

      { Ecrit les données en utilisant directement les API de Firebird }
      with FDestDB.Lib do
      begin
        { Alloue une requête d'écriture }
        StHandle := nil;
        DSQLAllocateStatement(DbHandle, StHandle);
        DSQLPrepare(DbHandle, TrHandle, StHandle, AnsiString(sql), 3, nil);
        { Ecrit les données }
        d := 0;
        while not RdrQuery.Eof do
        begin
          inc(d);
          { Commence par recréer les blobs }
          for f := 0 to RdrQuery.Fields.FieldCount - 1 do
            case RdrQuery.Fields.FieldType[f] of
              uftBlob, uftBlobId:
                begin
                  if (not RdrQuery.Fields.IsNull[f]) then
                  begin
                    BlHandle := nil;
                    TSQLDA(RdrQuery.Fields).AsQuad[f] := BlobCreate(DbHandle, TrHandle, BlHandle);
                    BlobWriteSegment(BlHandle, RdrQuery.Fields.BlobData[f].Size, RdrQuery.Fields.BlobData[f].Buffer);
                    BlobClose(BlHandle);
                  end;
                end;
            end;
          { Puis définit les champs tableau }
          for f := 0 to RdrQuery.Fields.ArrayCount - 1 do
            if (not RdrQuery.Fields.IsNull[RdrQuery.Fields.ArrayInfos[f].index]) then
            begin
              i := RdrQuery.Fields.ArrayInfos[f].index;
              TSQLDA(RdrQuery.Fields).AsQuad[i] := QuadNull;
              TSQLDA(RdrQuery.Fields).IsNull[i] := false;
              ArrayPutSlice(
                DbHandle,
                TrHandle,
                PGDSQuad(RdrQuery.Fields.Data.sqlvar[i].SqlData)^,
                RdrQuery.Fields.ArrayInfos[f].info,
                RdrQuery.Fields.ArrayData[i],
                RdrQuery.Fields.ArrayInfos[f].size);
            end;
          { Et enfin exécute la requête d'écriture directement avec les
            données lues par la requête de lecture }
          DSQLExecute(TrHandle, StHandle, 3, RdrQuery.Fields);
          { Tous les 500 enregistrements on poste un message pour indiquer où
            on en est }
          if ((d mod 500) = 0) then
          begin
            WrtTransaction.CommitRetaining;
            if Assigned(FOnProgress) then
              FOnProgress(Self,TableName,d);
          end;
          { Passe à l'enregistrement suivant }
          RdrQuery.Next;
        end;
        { Valide la transaction d'écriture }
        WrtTransaction.Commit;
        if Assigned(FOnProgress) then
          FOnProgress(Self,TableName,d);
        { Libère la requête d'écriture }
        DSQLFreeStatement(StHandle, DSQL_drop);
      end;
    end;
    RdrQuery.Close(etmCommit);
  finally
    FreeAndNil(RdrQuery);
    FreeAndNil(WrtTransaction);
    FreeAndNil(RdrTransaction);
  end;
end;

{ Active tous les triggers de DestDB }
procedure TUIBCloner.ActivateTriggers;
var
  T: TUIBTransaction;
  i,j: Integer;

  procedure ActivateTrigger(const TriggerName: String);
  var
    sql: String;
  begin
    sql := Format('ALTER TRIGGER %s ACTIVE',[TriggerName]);
    T.ExecuteImmediate(sql);
  end;

begin
  T := TUIBTransaction.Create(nil);
  try
    T.Database := FDestDB;
    T.StartTransaction;
    with DestMetadatas do
    begin
      for i := 0 to TablesCount - 1 do
        for j := 0 to Tables[i].TriggersCount - 1 do
          ActivateTrigger(Tables[i].Triggers[j].Name);
    end;
  finally
    T.Commit;
    FreeAndNil(T);
  end;
end;

{ Désactive tous les triggers de DestDB }
procedure TUIBCloner.DeActivateTriggers;
var
  T: TUIBTransaction;
  i,j: Integer;

  procedure DeactivateTrigger(const TriggerName: String);
  var
    sql: String;
  begin
    sql := Format('ALTER TRIGGER %s INACTIVE',[TriggerName]);
    T.ExecuteImmediate(sql);
  end;

begin
  T := TUIBTransaction.Create(nil);
  try
    T.Database := FDestDB;
    T.StartTransaction;
    with DestMetadatas do
    begin
      for i := 0 to TablesCount - 1 do
        for j := 0 to Tables[i].TriggersCount - 1 do
          DeactivateTrigger(Tables[i].Triggers[j].Name);
    end;
  finally
    T.Commit;
    FreeAndNil(T);
  end;
end;

{ Renvoie les métadonnées de DestDB }
function TUIBCloner.GetDestMetadatas: TMetaDataBase;
begin
  if not Assigned(FDestMetaDB) then
    FDestMetaDB := GetDBMetadatas(FDestDB);
  Result := FDestMetaDB;
end;

{ Renvoie les métadonnées de OrgDB }
function TUIBCloner.GetOrgMetadatas: TMetaDataBase;
begin
  if not Assigned(FOrgMetaDB) then
    FOrgMetaDB := GetDBMetadatas(FOrgDB);
  Result := FOrgMetaDB;
end;

{ Renvoie les métadonnées associées à une base de données }
function TUIBCloner.GetDBMetadatas(ADB: TUIBDatabase): TMetaDataBase;
var
  T: TUIBTransaction;
begin
  T := TUIBTransaction.Create(nil);
  try
    T.DataBase := ADB;
    Result := TMetaDataBase.Create(nil, -1);
    Result.LoadFromDatabase(T);
  finally
    T.Commit;
    FreeAndNil(T);
  end;
end;

end.
