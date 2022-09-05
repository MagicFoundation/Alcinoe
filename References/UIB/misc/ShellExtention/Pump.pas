unit Pump;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uib, StdCtrls;

type
  TPumpForm = class(TForm)
    Source: TUIBDataBase;
    Destination: TUIBDataBase;
    Log: TMemo;
    btStart: TButton;
    btClose: TButton;
    cbSave: TCheckBox;
    SaveDialog: TSaveDialog;
    SrcTransaction: TUIBTransaction;
    DstTransaction: TUIBTransaction;
    SrcQuery: TUIBQuery;
    GroupBox2: TGroupBox;
    cbVerbose: TCheckBox;
    cbCloseWhenDone: TCheckBox;
    edPumpFile: TEdit;
    Label2: TLabel;
    Button1: TButton;
    GroupBox1: TGroupBox;
    cbEmptyTables: TCheckBox;
    cbLocalHost: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btBrowseClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure ConfigChange(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  PumpForm: TPumpForm;

implementation

uses inifiles, uiblib, uibmetadata, uibase;

{$R *.dfm}

procedure TPumpForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  ini: TIniFile;
begin
  if not btStart.Enabled then
  begin
    Action := caNone;
    Exit;
  end;
  if cbSave.Checked then
  begin
    ini := TIniFile.Create(ExtractFilePath(GetModuleName(HInstance)) + 'config.ini');
    try
      ini.WriteBool('PUMP', 'CloseWhenDone', cbCloseWhenDone.Checked);
      ini.WriteBool('PUMP', 'Verbose', cbVerbose.Checked);
      ini.WriteBool('PUMP', 'EmptyTables', cbEmptyTables.Checked);
      ini.WriteBool('PUMP', 'Localhost', cbLocalHost.Checked);
    finally
      ini.Free;
    end;
  end;
  Action := caFree;
end;

procedure TPumpForm.FormCreate(Sender: TObject);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(ExtractFilePath(GetModuleName(HInstance)) + 'config.ini');
  try
    Source.DatabaseName := ini.ReadString('DATABASE', 'LIBRARY', 'gds32.dll');
    Source.UserName := ini.ReadString('DATABASE', 'USERNAME', 'SYSDBA');
    Source.PassWord := ini.ReadString('DATABASE', 'PASSWORD', 'masterkey');

    Destination.UserName := ini.ReadString('DATABASE', 'USERNAME', 'SYSDBA');
    Destination.PassWord := ini.ReadString('DATABASE', 'PASSWORD', 'masterkey');

    cbCloseWhenDone.Checked := ini.ReadBool('PUMP', 'CloseWhenDone', false);
    cbVerbose.Checked := ini.ReadBool('PUMP', 'Verbose', true);
    cbEmptyTables.Checked := ini.ReadBool('PUMP', 'EmptyTables', true);
    cbLocalHost.Checked := ini.ReadBool('PUMP', 'Localhost', true);
  finally
    ini.Free;
  end;
end;

procedure TPumpForm.btBrowseClick(Sender: TObject);
begin
  SaveDialog.InitialDir := ExtractFilePath(edPumpFile.Text);
  if SaveDialog.Execute then
    edPumpFile.Text := SaveDialog.FileName;
end;

procedure TPumpForm.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TPumpForm.ConfigChange(Sender: TObject);
begin
  Destination.DatabaseName := edPumpFile.Text;
end;

procedure TPumpForm.btStartClick(Sender: TObject);
var
  metadb: TMetaDataBase;

  i, j, k, l: integer;
  errorscount: integer;
  sql: string;
  dbhandle: IscDbHandle;
  trhandle: IscTrHandle;
  sthandle: IscStmtHandle;
  blhandle: IscBlobHandle;

  procedure AddLog(const str: string);
  begin
    if cbVerbose.Checked then
    begin
      Log.Lines.Add(str);
   // Log.ScrollBy ItemIndex := Log.Items.Count - 1;
      Application.ProcessMessages;
    end;
  end;

  procedure ExecuteImmediate(const sql: string);
  begin
    try
      DstTransaction.ExecuteImmediate(sql);
    except
      on e: Exception do
      begin
        AddLog('--- failed ---');
        Log.Lines.Add(sql);
        AddLog('---  exception  ---');
        Log.Lines.Add(e.Message);
        AddLog('--------------');        
        inc(errorscount);
      end;
    end;
  end;
  
begin
  errorscount := 0;
  Destination.Connected := false;
  if not FileExists(Destination.DatabaseName) then
  begin
    AddLog('Destination file does not exists !');
    Exit;
  end;
  log.Clear;
  Screen.Cursor := crHourGlass;


  if cbLocalHost.Checked then
  begin
    if Pos(':', Source.DatabaseName) <> 1 then
      Source.DatabaseName := ':' + Source.DatabaseName;
    if Pos(':', Destination.DatabaseName) <> 1 then
      Destination.DatabaseName := ':' + Destination.DatabaseName
  end else
  begin
    if Pos(':', Source.DatabaseName) <> 1 then
      Source.DatabaseName := copy(Source.DatabaseName, 2, Length(Source.DatabaseName) - 1);
    if Pos(':', Destination.DatabaseName) <> 1 then
      Destination.DatabaseName := copy(Destination.DatabaseName, 2, Length(Destination.DatabaseName) - 1);
  end;

  metadb := TMetaDataBase.Create(nil,-1);
  btStart.Enabled := false;
  try
    metadb.LoadFromDatabase(SrcTransaction);

    Destination.CharacterSet := metadb.DefaultCharset;
    Destination.SQLDialect := Source.InfoDbSqlDialect;
    Destination.Connected := true;
    dbhandle := Destination.DbHandle;
    DstTransaction.Commit;

    for i := 0 to metadb.GeneratorsCount - 1 do
    begin
      AddLog('Synchronizing Generator: ' + metadb.Generators[i].Name);
      ExecuteImmediate(metadb.Generators[i].AsAlterDDL);
    end;

    for i := 0 to metadb.TablesCount - 1 do
    for j := 0 to metadb.Tables[i].TriggersCount - 1 do
    begin
      AddLog('Inactivating Trigger: ' + metadb.Tables[i].Name + '.' + metadb.Tables[i].Triggers[j].Name);
      ExecuteImmediate(metadb.Tables[i].Triggers[j].AsAlterToInactiveDDL);
    end;

    if cbEmptyTables.Checked then
    begin
      DstTransaction.StartTransaction;
      trhandle := DstTransaction.TrHandle;
      for i := metadb.SortedTablesCount - 1 downto 0 do
      try
        AddLog('Emptying Table: ' + metadb.SortedTables[i].Name);
        sql := 'delete from ' + metadb.SortedTables[i].Name + ';';

        with Destination.Lib do
        begin
          sthandle := nil;
          DSQLAllocateStatement(dbhandle, sthandle);
          DSQLPrepare(dbhandle, trhandle, sthandle, sql, 3, nil);
          DSQLExecute(trhandle, sthandle, 3, nil);
          DSQLFreeStatement(sthandle, DSQL_drop);
        end;
      except
        on E: Exception do
        begin
          AddLog('--- failed ---');
          AddLog(e.Message);
          AddLog('--------------');
          inc(errorscount);
          continue;
        end;
      end;
      DstTransaction.Commit;
    end;

    DstTransaction.StartTransaction;
    trhandle := DstTransaction.TrHandle;
    for i := 0 to metadb.SortedTablesCount - 1 do
    try
      AddLog('Fill Table: ' + metadb.SortedTables[i].Name);
      sql := 'select ';
      k := 0;
      for j := 0 to metadb.SortedTables[i].FieldsCount - 1 do
        if metadb.SortedTables[i].Fields[j].ComputedSource = '' then
        begin
          if (k = 0) then
            sql := sql + metadb.SortedTables[i].Fields[j].Name else
            sql := sql + ', ' + metadb.SortedTables[i].Fields[j].Name;
          inc(k);
        end;
      sql := sql + ' from ' + metadb.SortedTables[i].Name;
      if metadb.SortedTables[i].PrimaryCount > 0 then
      begin
        k := 0;
        for j := 0 to metadb.SortedTables[i].Primary[0].FieldsCount - 1 do
        begin
          if (k = 0) then
            sql := sql + ' order by '
          else
            sql := sql + ', ';
          sql := sql + metadb.SortedTables[i].Primary[0].Fields[j].Name;
          Inc(k);
        end;
      end;
      SrcQuery.SQL.Text := sql;
      SrcQuery.Open;

      if not (SrcQuery.Eof) then
      begin
        sql := format('INSERT INTO %s (%s', [metadb.SortedTables[i].Name, SrcQuery.Fields.SqlName[0]]);
        for j := 1 to SrcQuery.Fields.FieldCount - 1 do
           sql := sql + ', ' + SrcQuery.Fields.SqlName[j];
        sql := sql + ') VALUES (?';
        for j := 1 to SrcQuery.Fields.FieldCount - 1 do
          sql := sql + ',?';
        sql := sql + ');';

        with Destination.Lib do
        begin
          sthandle := nil;
          DSQLAllocateStatement(dbhandle, sthandle);
          DSQLPrepare(dbhandle, trhandle, sthandle, sql, 3, nil);
          while not SrcQuery.Eof do
          begin
            // recreate blobs
            for k := 0 to SrcQuery.Fields.FieldCount - 1 do
              case SrcQuery.Fields.FieldType[k] of
                uftBlob, uftBlobId:
                  begin
                    if (not SrcQuery.Fields.IsNull[k]) then
                    begin
                      blhandle := nil;
                      TSQLDA(SrcQuery.Fields).AsQuad[k] := BlobCreate(dbhandle, trhandle, blhandle);
                      BlobWriteSegment(blhandle, SrcQuery.Fields.BlobData[k].Size, SrcQuery.Fields.BlobData[k].Buffer);
                      BlobClose(blhandle);
                    end;
                  end;
              end;
            // recreate array
            for k := 0 to SrcQuery.Fields.ArrayCount - 1 do
              if (not SrcQuery.Fields.IsNull[SrcQuery.Fields.ArrayInfos[k].index]) then
              begin
                l := SrcQuery.Fields.ArrayInfos[k].index;
                TSQLDA(SrcQuery.Fields).AsQuad[l] := QuadNull;
                TSQLDA(SrcQuery.Fields).IsNull[l] := false;
                ArrayPutSlice(
                  dbhandle,
                  trhandle,
                  PGDSQuad(SrcQuery.Fields.Data.sqlvar[l].SqlData)^,
                  SrcQuery.Fields.ArrayInfos[k].info,
                  SrcQuery.Fields.ArrayData[l],
                  SrcQuery.Fields.ArrayInfos[k].size);
              end;
            try
              DSQLExecute(trhandle, sthandle, 3, SrcQuery.Fields);
              DstTransaction.CommitRetaining;
            except
              on E: EUIBError do
              begin
                AddLog('--- failed ---');
                AddLog(Format('ErrorCode = %d' + #13#10 + 'SQLCode = %d',[E.ErrorCode,E.SQLCode]));
                AddLog(e.Message);
                AddLog('--- source fields values ---');

                for k := 0 to SrcQuery.Fields.FieldCount - 1 do
                  case SrcQuery.Fields.FieldType[k] of
                  uftBlob, uftBlobId:
                    AddLog(SrcQuery.Fields.AliasName[k] + ' = [BLOB]');
                  uftArray:
                    AddLog(SrcQuery.Fields.AliasName[k] + ' = [ARRAY]');
                  else
                    AddLog(SrcQuery.Fields.AliasName[k] + ' = ' + SrcQuery.Fields.AsString[k]);                  
                  end;

                AddLog('--- rolling back record and continue ---');
                DstTransaction.RollBackRetaining;
              end;
            end;

            SrcQuery.Next;

          end;
          DSQLFreeStatement(sthandle, DSQL_drop);
        end;
      end;
      SrcQuery.Close(etmStayIn);
    except
      on E: Exception do
      begin
        AddLog('--- failed ---');
        AddLog(e.Message);
        AddLog('--------------');
        inc(errorscount);
        continue;
      end;
    end;

    for i := 0 to metadb.TablesCount - 1 do
    for j := 0 to metadb.Tables[i].TriggersCount - 1 do
    begin
      if metadb.Tables[i].Triggers[j].Active then
      begin
        AddLog('Activating Trigger: ' + metadb.Tables[i].Name + '.' + metadb.Tables[i].Triggers[j].Name);
        ExecuteImmediate(metadb.Tables[i].Triggers[j].AsAlterToActiveDDL);
      end
      else
        AddLog('NOT activating Trigger: ' + metadb.Tables[i].Name + '.' + metadb.Tables[i].Triggers[j].Name);
    end;

    DstTransaction.Commit;
  finally
    btStart.Enabled := true;
    metadb.Free;
    Screen.Cursor := crDefault;
    Destination.Connected := false;
    Source.Connected := False;
    if errorscount > 0 then
      AddLog(format('--- There is %d errors ! ---', [errorscount]));
  end;
  AddLog('done :)');
  if cbCloseWhenDone.Checked then
    Close;
end;

procedure TPumpForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case key of
    #13: if btStart.Enabled then btStart.Click;
    #27: Close;
  end;
  if (sender is TEdit) and (Key = #13) then abort;
end;

end.
