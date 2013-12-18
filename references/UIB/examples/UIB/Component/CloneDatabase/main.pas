unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, XPMan,
  uibase, uiblib, uib, uibmetadata, ComCtrls;

type
  TMainForm = class(TForm)
    SrcDatabase: TUIBDataBase;
    SrcTransaction: TUIBTransaction;
    SrcQuery: TUIBQuery;
    DstDatabase: TUIBDataBase;
    DstTransaction: TUIBTransaction;
    GroupBox2: TGroupBox;
    cbVerbose: TCheckBox;
    cbCloseWhenDone: TCheckBox;
    Log: TMemo;
    GroupBox4: TGroupBox;
    btDstDatabase: TButton;
    btSrcDatabase: TButton;
    edSrcDatabase: TEdit;
    edDstDatabase: TEdit;
    Label1: TLabel;
    XPManifest1: TXPManifest;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    cbEmptyTables: TCheckBox;
    cbReplace: TCheckBox;
    cbMetadataOnly: TCheckBox;
    cbIgnoreConstraints: TCheckBox;
    cbPageSize: TComboBox;
    cbOverrideSourcePageSize: TCheckBox;
    cbInternalNames: TCheckBox;
    cbFailsafeDataPump: TCheckBox;
    btStartClone: TButton;
    btStartPump: TButton;
    cbSkipGrants: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btStartCloneClick(Sender: TObject);
    procedure btSrcDatabaseClick(Sender: TObject);
    procedure btDstDatabaseClick(Sender: TObject);
    procedure edSrcDatabaseChange(Sender: TObject);
    procedure edDstDatabaseChange(Sender: TObject);
    procedure btStartPumpClick(Sender: TObject);
    procedure cbOverrideSourcePageSizeClick(Sender: TObject);
  private
    { Déclarations privées }
    FErrorsCount: Integer;
    procedure ActivateControls(Container: TWinControl; Enable: Boolean);
    procedure AddLog(const What: String); overload;
    procedure AddLog(const FmtStr: String; const Args: array of const); overload;
    function GetDestPageSize: Integer;
    procedure ExecuteImmediate(const SQL: String);
    procedure EmptyTables(dbhandle: IscDbHandle; mdb: TMetaDataBase; cs: TCharacterSet);
    procedure PumpData(dbhandle: IscDbHandle; mdb: TMetaDataBase; failsafe: Boolean; sorttables: Boolean; cs: TCharacterSet);
  public
    { Déclarations publiques }
  end;

type
  TPageSize = record
    PageSize: Integer;
    Comment: String;
  end;

const
  PAGE_SIZES : array[0..4] of TPageSize = (
    (PageSize: 1024;  Comment: 'deprecated'),
    (PageSize: 2048;  Comment: 'default'),
    (PageSize: 4096;  Comment: 'recommanded'),
    (PageSize: 8192;  Comment: ''),
    (PageSize: 16384; Comment: '')
  );
  DEFAULT_PAGE_SIZE = 4096;

var
  MainForm: TMainForm;

implementation

uses
  uibdatabaseedit, uibkeywords;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
var  
  Selected: Integer;
  I: Integer;
begin
  Selected := 1;
  cbPageSize.Clear;
  for I := 0 to High(PAGE_SIZES) do
  begin
    if PAGE_SIZES[I].Comment <> '' then
      cbPageSize.AddItem(Format('%d (%s)', [PAGE_SIZES[I].PageSize, PAGE_SIZES[I].Comment]), TObject(PAGE_SIZES[I].PageSize))
    else
      cbPageSize.AddItem(Format('%d', [PAGE_SIZES[I].PageSize]), TObject(PAGE_SIZES[I].PageSize));

    if PAGE_SIZES[I].PageSize = DEFAULT_PAGE_SIZE then
      Selected := I;
  end;
  cbPageSize.ItemIndex := Selected;
end;

procedure TMainForm.btSrcDatabaseClick(Sender: TObject);
begin
  with TUIBDatabaseEditForm.Create(self) do
  begin
    Database := Self.SrcDatabase;
    ShowModal;
    Free;
  end;
  edSrcDatabase.Text := SrcDatabase.DatabaseName;
end;

procedure TMainForm.ActivateControls(Container: TWinControl; Enable: Boolean);
var
  I: Integer;
  C: TControl;
begin
  for I := 0 to Container.ControlCount - 1 do
  begin
    C := Container.Controls[I];

    if (C is TButton) or (C is TCheckBox) or (C is TRadioButton)
      or (C is TEdit) or (C is TComboBox) then
      C.Enabled := Enable
    else if C is TWinControl then
      ActivateControls(TWinControl(Container.Controls[I]), Enable);
  end;
end;

procedure TMainForm.AddLog(const FmtStr: String; const Args: array of const);
begin
  AddLog(Format(FmtStr, Args));
end;

procedure TMainForm.btDstDatabaseClick(Sender: TObject);
var
  N,E: String;
begin
  if edDstDatabase.Text = '' then
  begin
    N := edSrcDatabase.Text;
    E := ExtractFileExt(N);
    edDstDatabase.Text := Copy(N,1, Length(N) - Length(E)) + '_Clone' + E;
    DstDatabase.CharacterSet := SrcDatabase.CharacterSet;
  end;

  with TUIBDatabaseEditForm.Create(self) do
  begin
    Database := Self.DstDatabase;
    ShowModal;
    Free;
  end;
  edDstDatabase.Text := DstDatabase.DatabaseName;  
end;

procedure TMainForm.AddLog(const What: string);
begin
  if cbVerbose.Checked then
  begin
    Log.Lines.Add(What);
    Application.ProcessMessages;
  end;
end;

procedure TMainForm.ExecuteImmediate(const SQL: string);
begin
  try
    DstTransaction.ExecuteImmediate(sql);
    DstTransaction.Commit;
  except
    on e: Exception do
    begin
      AddLog('--- failed ---');
      Log.Lines.Add(sql);
      AddLog('---  exception  ---');
      Log.Lines.Add(e.Message);
      AddLog('--------------');
      inc(FErrorsCount);
    end;
  end;
end;

procedure TMainForm.btStartCloneClick(Sender: TObject);
var
  metadb: TMetaDataBase;
  i, j: integer;
  dbhandle: IscDbHandle;
begin
  FErrorsCount := 0;
  DstDatabase.Connected := false;
  if FileExists(DstDatabase.DatabaseName) then
    if cbReplace.Checked or (MessageDlg('Destination file already exist, delete ?', mtWarning, [mbOK, mbCancel], 0) = 1) then
    begin
      if not DeleteFile(DstDatabase.DatabaseName) then
        RaiseLastOSError;
    end else
      Exit;
  log.Clear;
  Screen.Cursor := crHourGlass;
  metadb := TMetaDataBase.Create(nil,-1);
  ActivateControls(Self, false);
  try
    metadb.LoadFromDatabase(SrcTransaction);
    DstDatabase.CharacterSet := metadb.DefaultCharset;
    DstDatabase.SQLDialect := SrcDatabase.InfoDbSqlDialect;

    AddLog('Create database (page_size %d)', [GetDestPageSize]);
    DstDatabase.CreateDatabase(GetDestPageSize);

    // ROLES
    for i := 0 to metadb.RolesCount - 1 do
    begin
      AddLog('Create role: %s', [metadb.Roles[i].Name]);
      if cbInternalNames.Checked then
        ExecuteImmediate(metadb.Roles[i].AsFullDDL) else
        ExecuteImmediate(metadb.Roles[i].AsDDL);
    end;

    // UDF
    for i := 0 to metadb.UDFSCount - 1 do
    begin
      AddLog('Create UDF: %s', [metadb.UDFS[i].Name]);
      if cbInternalNames.Checked then
        ExecuteImmediate(metadb.UDFS[i].AsFullDDL) else
        ExecuteImmediate(metadb.UDFS[i].AsDDL);
    end;

    // DOMAINS
    for i := 0 to metadb.DomainsCount - 1 do
    begin
      AddLog('Create Domain: %s', [metadb.Domains[i].Name]);
      if cbInternalNames.Checked then
        ExecuteImmediate(metadb.Domains[i].AsFullDDL) else
        ExecuteImmediate(metadb.Domains[i].AsDDL);
    end;

    // GENERATORS
    for i := 0 to metadb.GeneratorsCount - 1 do
    begin
      AddLog('Create Generator: %s', [metadb.Generators[i].Name]);
      ExecuteImmediate(metadb.Generators[i].AsCreateDLL);
      if not cbMetadataOnly.Checked then
      ExecuteImmediate(metadb.Generators[i].AsAlterDDL);
    end;

    // EXEPTIONS
    for i := 0 to metadb.ExceptionsCount - 1 do
    begin
      AddLog('Create Exception: %s', [metadb.Exceptions[i].Name]);
      if cbInternalNames.Checked then
        ExecuteImmediate(metadb.Exceptions[i].AsFullDDL) else
        ExecuteImmediate(metadb.Exceptions[i].AsDDL);
    end;

    // EMPTY PROCEDURES
    for i := 0 to metadb.ProceduresCount - 1 do
    begin
      AddLog('Create Empty Procedure: %s', [metadb.Procedures[i].Name]);
      ExecuteImmediate(metadb.Procedures[i].AsCreateEmptyDDL);
    end;

    // TABLES
    for i := 0 to metadb.TablesCount - 1 do
    begin
      AddLog('Create Table: %s', [metadb.Tables[i].Name]);
      if cbInternalNames.Checked then
        ExecuteImmediate(metadb.Tables[i].AsFullDDLNode) else
        ExecuteImmediate(metadb.Tables[i].AsDDLNode);
    end;

    // VIEWS
    for i := 0 to metadb.ViewsCount - 1 do
    begin
      AddLog('Create View: %s', [metadb.Views[i].Name]);
      if cbInternalNames.Checked then
        ExecuteImmediate(metadb.Views[i].AsFullDDLNode) else
        ExecuteImmediate(metadb.Views[i].AsDDLNode);
    end;

    // TABLES DATA
    if not cbMetadataOnly.Checked then
    begin
      dbhandle := DstDatabase.DbHandle;
      DstTransaction.Commit;
      PumpData(dbhandle, metadb, cbFailsafeDataPump.Checked, false, DstDatabase.CharacterSet);
    end;

    if not cbIgnoreConstraints.Checked then
    begin
      // UNIQUE
      for i := 0 to metadb.TablesCount - 1 do
      for j := 0 to metadb.Tables[i].UniquesCount - 1 do
      begin
        AddLog('Create Unique: %s', [metadb.Tables[i].Uniques[j].Name]);
        if cbInternalNames.Checked then
          ExecuteImmediate(metadb.Tables[i].Uniques[j].AsFullDDL) else
          ExecuteImmediate(metadb.Tables[i].Uniques[j].AsDDL);
      end;

      // PRIMARY
      for i := 0 to metadb.TablesCount - 1 do
      for j := 0 to metadb.Tables[i].PrimaryCount - 1 do
      begin
        AddLog('Create Primary: %s', [metadb.Tables[i].Primary[j].Name]);
        if cbInternalNames.Checked then
          ExecuteImmediate(metadb.Tables[i].Primary[j].AsFullDDL) else
          ExecuteImmediate(metadb.Tables[i].Primary[j].AsDDL);
      end;

      // FOREIGN
      for i := 0 to metadb.TablesCount - 1 do
      for j := 0 to metadb.Tables[i].ForeignCount - 1 do
      begin
        AddLog('Create Foreign: %s', [metadb.Tables[i].Foreign[j].Name]);
        if cbInternalNames.Checked then
          ExecuteImmediate(metadb.Tables[i].Foreign[j].AsFullDDL) else
          ExecuteImmediate(metadb.Tables[i].Foreign[j].AsDDL);
      end;

      // INDICES
      for i := 0 to metadb.TablesCount - 1 do
      for j := 0 to metadb.Tables[i].IndicesCount - 1 do
      begin
        AddLog('Create Indice: %s', [metadb.Tables[i].Indices[j].Name]);
        if cbInternalNames.Checked then
          ExecuteImmediate(metadb.Tables[i].Indices[j].AsFullDDL) else
          ExecuteImmediate(metadb.Tables[i].Indices[j].AsDDL)
      end;

      // CHECKS
      for i := 0 to metadb.TablesCount - 1 do
      for j := 0 to metadb.Tables[i].ChecksCount - 1 do
      begin
        AddLog('Create Check: %s', [metadb.Tables[i].Checks[j].Name]);
        if cbInternalNames.Checked then
          ExecuteImmediate(metadb.Tables[i].Checks[j].AsFullDDL) else
          ExecuteImmediate(metadb.Tables[i].Checks[j].AsDDL);
      end;
    end; // IgnoreConstraints

    // TABLE TRIGGERS
    for i := 0 to metadb.TablesCount - 1 do
    for j := 0 to metadb.Tables[i].TriggersCount - 1 do
    begin
      AddLog('Create Trigger: %s', [metadb.Tables[i].Triggers[j].Name]);
      if cbInternalNames.Checked then
        ExecuteImmediate(metadb.Tables[i].Triggers[j].AsFullDDL) else
        ExecuteImmediate(metadb.Tables[i].Triggers[j].AsDDL);
    end;

    // VIEW TRIGGERS
    for i := 0 to metadb.ViewsCount - 1 do
    for j := 0 to metadb.Views[i].TriggersCount - 1 do
    begin
      AddLog('Create Trigger: %s', [metadb.Views[i].Triggers[j].Name]);
      if cbInternalNames.Checked then
        ExecuteImmediate(metadb.Views[i].Triggers[j].AsFullDDL) else
        ExecuteImmediate(metadb.Views[i].Triggers[j].AsDDL);
    end;

    // ALTER PROCEDURES
    for i := 0 to metadb.ProceduresCount - 1 do
    begin
      AddLog('Alter Procedure: %s', [metadb.Procedures[i].Name]);
      ExecuteImmediate(metadb.Procedures[i].AsAlterDDL);
    end;

    if not cbSkipGrants.Checked then
    begin
      // GRANTS
      for i := 0 to metadb.RolesCount - 1 do
      begin
        for j := 0 to metadb.Roles[i].GrantsCount - 1 do
        begin
           AddLog('Grant To Role: %s', [metadb.Roles[i].Grants[j].Name]);
           if cbInternalNames.Checked then
             ExecuteImmediate(metadb.Roles[i].Grants[j].AsFullDDL) else
             ExecuteImmediate(metadb.Roles[i].Grants[j].AsDDL);
        end;
      end;

      for i := 0 to metadb.TablesCount - 1 do
      begin
        for j := 0 to metadb.Tables[i].GrantsCount - 1 do
        begin
          AddLog('Grant To Table: %s', [metadb.Tables[i].Grants[j].Name]);
          if cbInternalNames.Checked then
            ExecuteImmediate(metadb.Tables[i].Grants[j].AsFullDDL) else
            ExecuteImmediate(metadb.Tables[i].Grants[j].AsDDL);
        end;
        for j := 0 to metadb.Tables[i].FieldsGrantsCount - 1 do
        begin
          AddLog('Grant To TableField: %s', [metadb.Tables[i].FieldsGrants[j].Name]);
          if cbInternalNames.Checked then
            ExecuteImmediate(metadb.Tables[i].FieldsGrants[j].AsFullDDL) else
            ExecuteImmediate(metadb.Tables[i].FieldsGrants[j].AsDDL);
        end;
      end;

      for i := 0 to metadb.ViewsCount - 1 do
      begin
        for j := 0 to metadb.Views[i].GrantsCount - 1 do
        begin
          AddLog('Grant To View: %s', [metadb.Views[i].Grants[j].Name]);
          if cbInternalNames.Checked then
            ExecuteImmediate(metadb.Views[i].Grants[j].AsFullDDL) else
            ExecuteImmediate(metadb.Views[i].Grants[j].AsDDL);
        end;
        for j := 0 to metadb.Views[i].FieldsGrantsCount - 1 do
        begin
          AddLog('Grant To ViewField: %s', [metadb.Views[i].FieldsGrants[j].Name]);
          if cbInternalNames.Checked then
            ExecuteImmediate(metadb.Tables[i].FieldsGrants[j].AsFullDDL) else
            ExecuteImmediate(metadb.Tables[i].FieldsGrants[j].AsDDL);
        end;
      end;

      for i := 0 to metadb.ProceduresCount - 1 do
      begin
        for j := 0 to metadb.Procedures[i].GrantsCount - 1 do
        begin
          AddLog('Grant To Procedure: %s', [metadb.Procedures[i].Grants[j].Name]);
          if cbInternalNames.Checked then
            ExecuteImmediate(metadb.Procedures[i].Grants[j].AsFullDDL) else
            ExecuteImmediate(metadb.Procedures[i].Grants[j].AsDDL);
        end;
      end;
    end;
  finally
    ActivateControls(Self, true);
    metadb.Free;
    Screen.Cursor := crDefault;
    DstDatabase.Connected := false;
    SrcDatabase.Connected := False;
    if FErrorsCount > 0 then
      AddLog('--- %d error(s) ! ---', [FErrorsCount]);
  end;
  AddLog('done :)');

  if cbCloseWhenDone.Checked then
    Close;
end;

procedure TMainForm.btStartPumpClick(Sender: TObject);
var
  metadb: TMetaDataBase;
  i, j: integer;
  dbhandle: IscDbHandle;
begin
  log.Clear;
  FErrorsCount := 0;

  try
    SrcDatabase.Connected := true;
  except
    AddLog('Cannot connect to source database !');
    Exit;
  end;

  try
    DstDatabase.SQLDialect := SrcDatabase.InfoDbSqlDialect;
    DstDatabase.Connected := true;
    dbhandle := DstDatabase.DbHandle;
  except
    AddLog('Cannot connect to destination database !');
    Exit;
  end;

  ActivateControls(Self, false);
  Screen.Cursor := crHourGlass;

  metadb := TMetaDataBase.Create(nil,-1);
  try
    metadb.LoadFromDatabase(SrcTransaction);

    DstTransaction.Commit;

    for i := 0 to metadb.GeneratorsCount - 1 do
    begin
      AddLog('Synchronizing Generator: %s', [metadb.Generators[i].Name]);
      ExecuteImmediate(metadb.Generators[i].AsAlterDDL);
    end;

    for i := 0 to metadb.TablesCount - 1 do
    for j := 0 to metadb.Tables[i].TriggersCount - 1 do
    begin
      AddLog('Inactivating Trigger: %s.%s',[metadb.Tables[i].Name, metadb.Tables[i].Triggers[j].Name]);
      ExecuteImmediate(metadb.Tables[i].Triggers[j].AsAlterToInactiveDDL);
    end;

    if cbEmptyTables.Checked then
      EmptyTables(dbhandle, metadb, DstDatabase.CharacterSet);

    PumpData(dbhandle, metadb, cbFailsafeDataPump.Checked, true, DstDatabase.CharacterSet);

    for i := 0 to metadb.TablesCount - 1 do
    for j := 0 to metadb.Tables[i].TriggersCount - 1 do
    begin
      if metadb.Tables[i].Triggers[j].Active then
      begin
        AddLog('Activating Trigger: %s', [metadb.Tables[i].Name, metadb.Tables[i].Triggers[j].Name]);
        ExecuteImmediate(metadb.Tables[i].Triggers[j].AsAlterToActiveDDL);
      end
      else
        AddLog('NOT activating Trigger: %s.%s', [metadb.Tables[i].Name, metadb.Tables[i].Triggers[j].Name]);
    end;

    DstTransaction.Commit;
  finally
    ActivateControls(Self, true);
    metadb.Free;
    Screen.Cursor := crDefault;
    DstDatabase.Connected := false;
    SrcDatabase.Connected := False;
    if FErrorsCount > 0 then
      AddLog(format('--- There is %d errors ! ---', [FErrorsCount]));
  end;

  AddLog('done :)');

  if cbCloseWhenDone.Checked then
    Close;
end;

procedure TMainForm.cbOverrideSourcePageSizeClick(Sender: TObject);
begin
  cbPageSize.Enabled := cbOverrideSourcePageSize.Checked;
end;

procedure TMainForm.edDstDatabaseChange(Sender: TObject);
begin
  DstDatabase.DatabaseName := edDstDatabase.Text;
end;

procedure TMainForm.edSrcDatabaseChange(Sender: TObject);
begin
  SrcDatabase.DatabaseName := edSrcDatabase.Text;
end;

function TMainForm.GetDestPageSize: Integer;
begin
  if cbOverrideSourcePageSize.Checked then
    Result := Integer(cbPageSize.Items.Objects[cbPageSize.ItemIndex])
  else
    Result := SrcDatabase.InfoPageSize;
end;

procedure TMainForm.EmptyTables(dbhandle: IscDbHandle; mdb: TMetaDataBase; cs: TCharacterSet);
var
  sthandle: PPointer;
  sql: string;
  trhandle: PPointer;
  I: Integer;
begin
  DstTransaction.StartTransaction;
  trhandle := DstTransaction.TrHandle;
  for I :=  mdb.SortedTablesCount - 1 downto 0 do
    try
      AddLog('Emptying Table: %s', [mdb.SortedTables[I].Name]);
      sql := 'delete from ' + mdb.SortedTables[I].Name + ';';
      with DstDatabase.Lib do
      begin
        sthandle := nil;
        DSQLAllocateStatement(dbhandle, sthandle);
{$IFDEF UNICODE}
        DSQLPrepare(dbhandle, trhandle, sthandle, MBUEncode(sql, CharacterSetCP[cs]), 3, nil);
{$ELSE}
        DSQLPrepare(dbhandle, trhandle, sthandle, sql, 3, nil);
{$ENDIF}
        DSQLExecute(trhandle, sthandle, 3, nil);
        DSQLFreeStatement(sthandle, DSQL_drop);
      end;
    except
      on E: Exception do
      begin
        AddLog('--- failed ---');
        AddLog(e.Message);
        AddLog('--------------');
        inc(FErrorsCount);
        continue;
      end;
    end;
  DstTransaction.Commit;
end;

procedure TMainForm.PumpData(dbhandle: IscDbHandle; mdb: TMetaDataBase;
  failsafe: Boolean; sorttables: Boolean; cs: TCharacterSet);
var
  T,F,c,l: Integer;
  done: Integer;
  sql: string;
  trhandle: IscTrHandle;
  sthandle: IscStmtHandle;
  blhandle: IscBlobHandle;
  Table: TMetaTable;

  function TablesCount: Integer;
  begin
    if sorttables then
      Result := mdb.SortedTablesCount
    else
      Result := mdb.TablesCount;
  end;

  function Tables(const Index: Integer): TMetaTable;
  begin
    if sorttables then
      Result := mdb.SortedTables[Index]
    else
      Result := mdb.Tables[Index];
  end;

begin
  DstTransaction.StartTransaction;
  trhandle := DstTransaction.TrHandle;

  for T := 0 to TablesCount - 1 do
  try
    Table := Tables(T);
    AddLog('Filling Table: %s', [Table.Name]);
    sql := 'select ';
    c := 0;
    for F := 0 to Table.FieldsCount - 1 do
      if Table.Fields[F].ComputedSource = '' then
      begin
        if (c = 0) then
          sql := sql + Table.Fields[F].Name
        else
          sql := sql + ', ' + Table.Fields[F].Name;
        inc(c);
      end;
    sql := sql + ' from ' + Table.Name;
    if Table.PrimaryCount > 0 then
    begin
      c := 0;
      for F := 0 to Table.Primary[0].FieldsCount - 1 do
      begin
        if (c = 0) then
          sql := sql + ' order by '
        else
          sql := sql + ', ';
        sql := sql + Table.Primary[0].Fields[F].Name;
        Inc(c);
      end;
    end;
    SrcQuery.SQL.Text := sql;
    SrcQuery.Open;

    if not (SrcQuery.Eof) then
    begin
      sql := format('INSERT INTO %s (%s', [Table.Name, Table.MetaQuote(SrcQuery.Fields.SqlName[0])]);
      for F := 1 to SrcQuery.Fields.FieldCount - 1 do
        sql := sql + ', ' + Table.MetaQuote(SrcQuery.Fields.SqlName[F]);
      sql := sql + ') VALUES (?';
      for F := 1 to SrcQuery.Fields.FieldCount - 1 do
        sql := sql + ',?';
      sql := sql + ');';

      with DstDatabase.Lib do
      begin
        sthandle := nil;
        DSQLAllocateStatement(dbhandle, sthandle);
{$IFDEF UNICODE}
        DSQLPrepare(dbhandle, trhandle, sthandle, MBUEncode(sql, CharacterSetCP[cs]), 3, nil);
{$ELSE}
        DSQLPrepare(dbhandle, trhandle, sthandle, sql, 3, nil);
{$ENDIF}
        done := 0;
        while not SrcQuery.Eof do
        begin
          // recreate blobs
          for F := 0 to SrcQuery.Fields.FieldCount - 1 do
            case SrcQuery.Fields.FieldType[F] of
              uftBlob, uftBlobId:
                begin
                  if (not SrcQuery.Fields.IsNull[F]) then
                  begin
                    blhandle := nil;
                    TSQLDA(SrcQuery.Fields).AsQuad[F] := BlobCreate(dbhandle, trhandle, blhandle);
                    BlobWriteSegment(blhandle, SrcQuery.Fields.BlobData[F].Size, SrcQuery.Fields.BlobData[F].Buffer);
                    BlobClose(blhandle);
                  end;
                end
            end;

          // recreate array
          for F := 0 to SrcQuery.Fields.ArrayCount - 1 do
            if (not SrcQuery.Fields.IsNull[SrcQuery.Fields.ArrayInfos[F].index]) then
            begin
              l := SrcQuery.Fields.ArrayInfos[F].index;
              TSQLDA(SrcQuery.Fields).AsQuad[l] := QuadNull;
              TSQLDA(SrcQuery.Fields).IsNull[l] := false;
              ArrayPutSlice(dbhandle, trhandle, PGDSQuad(SrcQuery.Fields.Data.sqlvar[l].SqlData)^, SrcQuery.Fields.ArrayInfos[F].info, SrcQuery.Fields.ArrayData[l], SrcQuery.Fields.ArrayInfos[F].size);
            end;

          try
            DSQLExecute(trhandle, sthandle, 3, SrcQuery.Fields);
            Inc(done);
            if failsafe or (done mod 500 = 0) then
              DstTransaction.CommitRetaining;
            if (done mod 10000 = 0) then
              AddLog('Pumped %d records',[done]);
          except
            on E: EUIBError do
            begin
              AddLog('--- failed ---');
              AddLog('ErrorCode = %d' + ''#13''#10'' + 'SQLCode = %d', [E.ErrorCode, E.SQLCode]);
              AddLog(e.Message);
              AddLog('--- source fields values ---');
              for c := 0 to SrcQuery.Fields.FieldCount - 1 do
                case SrcQuery.Fields.FieldType[c] of
                  uftBlob, uftBlobId:
                    AddLog('%s = [BLOB]', [SrcQuery.Fields.AliasName[c]]);
                  uftArray:
                    AddLog('%s = [ARRAY]', [SrcQuery.Fields.AliasName[c]])
                  else
                    AddLog('%s = %s', [SrcQuery.Fields.AliasName[c], SrcQuery.Fields.AsString[c]]);
                end;
              AddLog('--- rolling back record and continue ---');
              DstTransaction.RollBackRetaining;
              Inc(FErrorsCount);
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
      Inc(FErrorsCount);
      Continue;
    end;
  end;
end;

end.
