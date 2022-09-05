unit Clone;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uib, StdCtrls;

type
  TCloneForm = class(TForm)
    Source: TUIBDataBase;
    Destination: TUIBDataBase;
    Log: TMemo;
    btStart: TButton;
    btClose: TButton;
    cbSave: TCheckBox;
    edCloneFile: TEdit;
    Label1: TLabel;
    btBrowse: TButton;
    SaveDialog: TSaveDialog;
    SrcTransaction: TUIBTransaction;
    DstTransaction: TUIBTransaction;
    SrcQuery: TUIBQuery;
    GroupBox1: TGroupBox;
    cbReplace: TCheckBox;
    cbMetadataOnly: TCheckBox;
    GroupBox2: TGroupBox;
    cbVerbose: TCheckBox;
    cbCloseWhenDone: TCheckBox;
    cbPageSize: TComboBox;
    cbOverrideSourcePageSize: TCheckBox;
    cbIgnoreConstraints: TCheckBox;
    cbLocalHost: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btBrowseClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure ConfigChange(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure cbOverrideSourcePageSizeClick(Sender: TObject);
  private
    { Déclarations privées }
    function GetDestPageSize: Integer;
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

var
  CloneForm: TCloneForm;

implementation

uses inifiles, uiblib, uibmetadata, uibase;

{$R *.dfm}

procedure TCloneForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  ini: TIniFile;
  pageSize: Integer;
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
      ini.WriteBool('CLONE', 'Replace', cbReplace.Checked);
      ini.WriteBool('CLONE', 'CloseWhenDone', cbCloseWhenDone.Checked);
      ini.WriteBool('CLONE', 'Verbose', cbVerbose.Checked);
      ini.WriteBool('CLONE', 'MetadataOnly', cbMetadataOnly.Checked);
      ini.WriteBool('CLONE', 'OverridePageSize', cbOverrideSourcePageSize.Checked);
      pageSize := Integer(cbPageSize.Items.Objects[cbPageSize.ItemIndex]);
      ini.WriteInteger('CLONE', 'PageSize', pageSize);
      ini.WriteBool('CLONE', 'IgnoreConstraints', cbIgnoreConstraints.Checked);
      ini.WriteBool('CLONE', 'Localhost', cbLocalHost.Checked);

    finally
      ini.Free;
    end;
  end;
  Action := caFree;
end;


procedure TCloneForm.FormCreate(Sender: TObject);
var
  ini: TIniFile;
  defaultPageSize: Integer;
  selected: Integer;
  I: Integer;
begin
  ini := TIniFile.Create(ExtractFilePath(GetModuleName(HInstance)) + 'config.ini');
  try
    Source.DatabaseName := ini.ReadString('DATABASE', 'LIBRARY', 'gds32.dll');
    Source.UserName := ini.ReadString('DATABASE', 'USERNAME', 'SYSDBA');
    Source.PassWord := ini.ReadString('DATABASE', 'PASSWORD', 'masterkey');

    Destination.UserName := ini.ReadString('DATABASE', 'USERNAME', 'SYSDBA');
    Destination.PassWord := ini.ReadString('DATABASE', 'PASSWORD', 'masterkey');

    cbReplace.Checked := ini.ReadBool('CLONE', 'Replace', false);
    cbCloseWhenDone.Checked := ini.ReadBool('CLONE', 'CloseWhenDone', false);
    cbVerbose.Checked := ini.ReadBool('CLONE', 'Verbose', true);
    cbMetadataOnly.Checked := ini.ReadBool('CLONE', 'MetadataOnly', false);
    cbIgnoreConstraints.Checked := ini.ReadBool('CLONE', 'IgnoreConstraints', false);
    cbReplace.Checked := ini.ReadBool('CLONE', 'Localhost', false);

    cbOverrideSourcePageSize.Checked := ini.ReadBool('CLONE','OverridePageSize',false);
    defaultPageSize := ini.ReadInteger('CLONE','PageSize', 2048);
  finally
    ini.Free;
  end;

  selected := 1;
  cbPageSize.Clear;
  for I := 0 to High(PAGE_SIZES) do
  begin
    if PAGE_SIZES[I].Comment <> '' then
      cbPageSize.AddItem(Format('%d (%s)', [PAGE_SIZES[I].PageSize, PAGE_SIZES[I].Comment]), TObject(PAGE_SIZES[I].PageSize))
    else
      cbPageSize.AddItem(Format('%d', [PAGE_SIZES[I].PageSize]), TObject(PAGE_SIZES[I].PageSize));

    if PAGE_SIZES[I].PageSize = defaultPageSize then
      selected := I;
  end;
  cbPageSize.ItemIndex := selected;

end;

procedure TCloneForm.btBrowseClick(Sender: TObject);
begin
  SaveDialog.InitialDir := ExtractFilePath(edCloneFile.Text);
  if SaveDialog.Execute then
    edCloneFile.Text := SaveDialog.FileName;
end;

procedure TCloneForm.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TCloneForm.ConfigChange(Sender: TObject);
begin
  Destination.DatabaseName := edCloneFile.Text;
end;

procedure TCloneForm.btStartClick(Sender: TObject);
var
  metadb: TMetaDataBase;
  i, j, k, l, m: integer;
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
        inc(errorscount);
      end;
    end;
  end;
begin
  errorscount := 0;
  Destination.Connected := false;
  if FileExists(Destination.DatabaseName) then
    if cbReplace.Checked or (MessageDlg('Destination file allready exist, delete ?', mtWarning, [mbOK, mbCancel], 0) = 1) then
    begin
      if not DeleteFile(Destination.DatabaseName) then
        RaiseLastOSError;
    end else
      Exit;


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

  log.Clear;
  Screen.Cursor := crHourGlass;
  metadb := TMetaDataBase.Create(nil,-1);
  btStart.Enabled := false;
  try
    metadb.LoadFromDatabase(SrcTransaction);
    Destination.CharacterSet := metadb.DefaultCharset;
    Destination.SQLDialect := Source.InfoDbSqlDialect;

    AddLog('Create database (page_size ' + IntToStr(GetDestPageSize) + ')');
    Destination.CreateDatabase(GetDestPageSize);

    // ROLES
    for i := 0 to metadb.RolesCount - 1 do
    begin
      AddLog('Create role: ' + metadb.Roles[i].Name);
      ExecuteImmediate(metadb.Roles[i].AsDDL);
    end;

    // UDF
    for i := 0 to metadb.UDFSCount - 1 do
    begin
      AddLog('Create UDF: ' + metadb.UDFS[i].Name);
      ExecuteImmediate(metadb.UDFS[i].AsDDL);
    end;

    // DOMAINS
    for i := 0 to metadb.DomainsCount - 1 do
    begin
      AddLog('Create Domain: ' + metadb.Domains[i].Name);
      ExecuteImmediate(metadb.Domains[i].AsDDL);
    end;

    // GENERATORS
    for i := 0 to metadb.GeneratorsCount - 1 do
    begin
      AddLog('Create Generator: ' + metadb.Generators[i].Name);
      ExecuteImmediate(metadb.Generators[i].AsCreateDLL);
      if not cbMetadataOnly.Checked then
        ExecuteImmediate(metadb.Generators[i].AsAlterDDL);
    end;

    // EXEPTIONS
    for i := 0 to metadb.ExceptionsCount - 1 do
    begin
      AddLog('Create Exception: ' + metadb.Exceptions[i].Name);
      ExecuteImmediate(metadb.Exceptions[i].AsDDL);
    end;

    // EMPTY PROCEDURES
    for i := 0 to metadb.ProceduresCount - 1 do
    begin
      AddLog('Create Empty Procedure: ' + metadb.Procedures[i].Name);
      ExecuteImmediate(metadb.Procedures[i].AsCreateEmptyDDL);
    end;

    // TABLES
    for i := 0 to metadb.TablesCount - 1 do
    begin
      AddLog('Create Table: ' + metadb.Tables[i].Name);
      ExecuteImmediate(metadb.Tables[i].AsDDLNode);
    end;

    // VIEWS
    for i := 0 to metadb.ViewsCount - 1 do
    begin
      AddLog('Create View: ' + metadb.Views[i].Name);
      ExecuteImmediate(metadb.Views[i].AsDDLNode);
    end;

    // TABLES DATA
    if not cbMetadataOnly.Checked then
    begin
      dbhandle := Destination.DbHandle;
      DstTransaction.Commit;
      DstTransaction.StartTransaction;
      trhandle := DstTransaction.TrHandle;
      for i := 0 to metadb.TablesCount - 1 do
      try
        AddLog('Fill Table: ' + metadb.Tables[i].Name);
        sql := 'select ';
        k := 0;
        for j := 0 to metadb.Tables[i].FieldsCount - 1 do
          if metadb.Tables[i].Fields[j].ComputedSource = '' then
          begin
            if (k = 0) then
              sql := sql + metadb.Tables[i].Fields[j].Name else
              sql := sql + ', ' + metadb.Tables[i].Fields[j].Name;
            inc(k);
          end;
        sql := sql + ' from ' + metadb.Tables[i].Name;
        SrcQuery.SQL.Text := sql;
        SrcQuery.Open;

        if not (SrcQuery.Eof) then
        begin
          sql := format('INSERT INTO %s (%s', [metadb.Tables[i].Name, SrcQuery.Fields.SqlName[0]]);
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
            m := 0;
            while not SrcQuery.Eof do
            begin
              inc(m);
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
              DSQLExecute(trhandle, sthandle, 3, SrcQuery.Fields);
              if ((m mod 500) = 0) then
                DstTransaction.CommitRetaining;
              SrcQuery.Next;

            end;
            DSQLFreeStatement(sthandle, DSQL_drop);
          end;
        end;
        SrcQuery.Close(etmStayIn);
      except
        AddLog('--- failed ---');
        inc(errorscount);
        continue;
      end;
    end;

    if not cbIgnoreConstraints.Checked then
    begin

      // UNIQUE
      for i := 0 to metadb.TablesCount - 1 do
      for j := 0 to metadb.Tables[i].UniquesCount - 1 do
      begin
        AddLog('Create Unique: ' + metadb.Tables[i].Uniques[j].Name);
        ExecuteImmediate(metadb.Tables[i].Uniques[j].AsDDL);
      end;

      // PRIMARY
      for i := 0 to metadb.TablesCount - 1 do
      for j := 0 to metadb.Tables[i].PrimaryCount - 1 do
      begin
        AddLog('Create Primary: ' + metadb.Tables[i].Primary[j].Name);
        ExecuteImmediate(metadb.Tables[i].Primary[j].AsDDL);
      end;

      // FOREIGN
      for i := 0 to metadb.TablesCount - 1 do
      for j := 0 to metadb.Tables[i].ForeignCount - 1 do
      begin
        AddLog('Create Foreign: ' + metadb.Tables[i].Foreign[j].Name);
        ExecuteImmediate(metadb.Tables[i].Foreign[j].AsDDL);
      end;

      // INDICES
      for i := 0 to metadb.TablesCount - 1 do
      for j := 0 to metadb.Tables[i].IndicesCount - 1 do
      begin
        AddLog('Create Indice: ' + metadb.Tables[i].Indices[j].Name);
        ExecuteImmediate(metadb.Tables[i].Indices[j].AsDDL);
      end;

      // CHECKS
      for i := 0 to metadb.TablesCount - 1 do
      for j := 0 to metadb.Tables[i].ChecksCount - 1 do
      begin
        AddLog('Create Check: ' + metadb.Tables[i].Checks[j].Name);
        ExecuteImmediate(metadb.Tables[i].Checks[j].AsDDL);
      end;

    end; // IgnoreConstraints

    // TABLE TRIGGERS
    for i := 0 to metadb.TablesCount - 1 do
    for j := 0 to metadb.Tables[i].TriggersCount - 1 do
    begin
      AddLog('Create Trigger: ' + metadb.Tables[i].Triggers[j].Name);
      ExecuteImmediate(metadb.Tables[i].Triggers[j].AsDDL);
    end;

    // VIEW TRIGGERS
    for i := 0 to metadb.ViewsCount - 1 do
    for j := 0 to metadb.Views[i].TriggersCount - 1 do
    begin
      AddLog('Create Trigger: ' + metadb.Views[i].Triggers[j].Name);
      ExecuteImmediate(metadb.Views[i].Triggers[j].AsDDL);
    end;

    // ALTER PROCEDURES
    for i := 0 to metadb.ProceduresCount - 1 do
    begin
      AddLog('Alter Procedure: ' + metadb.Procedures[i].Name);
      ExecuteImmediate(metadb.Procedures[i].AsAlterDDL);
    end;

    // GRANTS
    for i := 0 to metadb.RolesCount - 1 do
    begin
      for j := 0 to metadb.Roles[i].GrantsCount - 1 do
      begin
         AddLog('Grant To Role: ' + metadb.Roles[i].Grants[j].Name);
         ExecuteImmediate(metadb.Roles[i].Grants[j].AsDDL);
      end;
    end;

    for i := 0 to metadb.TablesCount - 1 do
    begin
      for j := 0 to metadb.Tables[i].GrantsCount - 1 do
      begin
        AddLog('Grant To Table: ' + metadb.Tables[i].Grants[j].Name);
        ExecuteImmediate(metadb.Tables[i].Grants[j].AsDDL);
      end;
      for j := 0 to metadb.Tables[i].FieldsGrantsCount - 1 do
      begin
        AddLog('Grant To TableField: ' + metadb.Tables[i].FieldsGrants[j].Name);
        ExecuteImmediate(metadb.Tables[i].FieldsGrants[j].AsDDL);
      end;
    end;

    for i := 0 to metadb.ViewsCount - 1 do
    begin
      for j := 0 to metadb.Views[i].GrantsCount - 1 do
      begin
        AddLog('Grant To View: ' + metadb.Views[i].Grants[j].Name);
        ExecuteImmediate(metadb.Views[i].Grants[j].AsDDL);
      end;
      for j := 0 to metadb.Views[i].FieldsGrantsCount - 1 do
      begin
        AddLog('Grant To ViewField: ' + metadb.Views[i].FieldsGrants[j].Name);
        ExecuteImmediate(metadb.Tables[i].FieldsGrants[j].AsDDL);
      end;
    end;

    for i := 0 to metadb.ProceduresCount - 1 do
    begin
      for j := 0 to metadb.Procedures[i].GrantsCount - 1 do
      begin
        AddLog('Grant To Procedure: ' + metadb.Procedures[i].Grants[j].Name);
        ExecuteImmediate(metadb.Procedures[i].Grants[j].AsDDL);
      end;
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

procedure TCloneForm.cbOverrideSourcePageSizeClick(Sender: TObject);
begin
  cbPageSize.Enabled := cbOverrideSourcePageSize.Checked;
end;

procedure TCloneForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case key of
    #13: if btStart.Enabled then btStart.Click;
    #27: Close;
  end;
  if (sender is TEdit) and (Key = #13) then abort;
end;

function TCloneForm.GetDestPageSize: Integer;
begin
  if cbOverrideSourcePageSize.Checked then
    Result := Integer(cbPageSize.Items.Objects[cbPageSize.ItemIndex])
  else
    Result := Source.InfoPageSize;
end;

end.
