unit dddToolsAdminDB;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Grids,
  StdCtrls,
  ExtCtrls,
  Menus,
  SynMemoEx,
  SynCommons,
  mORMot,
  mORMotDDD,
  mORMotHttpClient,
  mORMotUI,
  SynMustache;

type
  TDBFrame = class;
  TOnExecute = function(Sender: TDBFrame; const SQL, Content: RawUTF8): boolean of object;
  
  TDBFrame = class(TFrame)
    pnlRight: TPanel;
    pnlTop: TPanel;
    mmoSQL: TMemo;
    btnExec: TButton;
    drwgrdResult: TDrawGrid;
    spl1: TSplitter;
    spl2: TSplitter;
    btnHistory: TButton;
    btnCmd: TButton;
    pmCmd: TPopupMenu;
    pnlLeft: TPanel;
    lstTables: TListBox;
    pnlLeftTop: TPanel;
    edtLabels: TEdit;
    chkTables: TCheckBox;
    procedure lstTablesDblClick(Sender: TObject); virtual;
    procedure btnExecClick(Sender: TObject); virtual;
    procedure drwgrdResultClick(Sender: TObject); virtual;
    procedure btnHistoryClick(Sender: TObject); virtual;
    procedure btnCmdClick(Sender: TObject); virtual;
    procedure edtLabelsChange(Sender: TObject);
  protected
    fGridToCellRow: integer;
    fGridToCellVariant: variant;
    fJson: RawJSON;
    fSQL, fPreviousSQL: RawUTF8;
    fSQLLogFile: TFileName;
    function ExecSQL(const SQL: RawUTF8): RawUTF8;
    function OnText(Sender: TSQLTable; FieldIndex, RowIndex: Integer;
      var Text: string): boolean;
    procedure OnCommandsToGridAdd(const Item: TSynNameValueItem; Index: PtrInt);
    function OnGridToCell(Sender: TSQLTable; Row, Field: integer;
      HumanFriendly: boolean): RawJSON;
    procedure LogClick(Sender: TObject);
    procedure LogDblClick(Sender: TObject);
    procedure LogSearch(Sender: TObject);
  public
    DatabaseName: RawUTF8;
    mmoResult: TMemoEx; // initialized by code from SynMemoEx.pas
    Grid: TSQLTableToGrid;
    GridLastTableName: RawUTF8;
    Client: TSQLHttpClientWebsockets;
    Admin: IAdministratedDaemon;
    Tables: TStringList;
    AssociatedModel: TSQLModel;
    AssociatedServices: TInterfaceFactoryObjArray;
    // Add(cmdline/table,nestedobject,-1=text/0..N=nestedarray#)
    CommandsToGrid: TSynNameValue;
    TableDblClickSelect: TSynNameValue;
    TableDblClickOrderByIdDesc: boolean;
    TableDblClickOrderByIdDescCSV: string;
    SavePrefix: TFileName;
    OnBeforeExecute: TOnExecute;
    OnAfterExecute: TOnExecute;
    constructor Create(AOwner: TComponent); override;
    procedure EnableChkTables(const aCaption: string);
    procedure Open; virtual;
    procedure FillTables(const customcode: string); virtual;
    procedure AddSQL(SQL: string; AndExec: boolean);
    procedure SetResult(const JSON: RawUTF8); virtual;
    function NewCmdPopup(const c: string; NoCmdTrim: boolean): TMenuItem;
    destructor Destroy; override;
  end;

  TDBFrameClass = class of TDBFrame;

  TDBFrameDynArray = array of TDBFrame;

implementation

{$R *.dfm}

const
  WRAPPER_TEMPLATE = '{{#soa.services}}'#13#10'{{#methods}}'#13#10 +
    '#get {{uri}}/{{methodName}}{{#hasInParams}}?{{#args}}{{#dirInput}}{{argName}}={{typeSource}}' +
    '{{#commaInSingle}}&{{/commaInSingle}}{{/dirInput}}{{/args}}{{/hasInParams}}'#13#10 +
    '{{#hasOutParams}}'#13#10' { {{#args}}{{#dirOutput}}{{jsonQuote argName}}: {{typeSource}}' +
    '{{#commaOutResult}},{{/commaOutResult}} {{/dirOutput}}{{/args}} }'#13#10 +
    '{{/hasOutParams}}{{/methods}}'#13#10'{{/soa.services}}'#13#10'{{#enumerates}}{{name}}: ' +
    '{{#values}}{{EnumTrim .}}={{-index0}}{{^-last}}, {{/-last}}{{/values}}'#13#10'{{/enumerates}}';

{ TDBFrame }

constructor TDBFrame.Create(AOwner: TComponent);
begin
  inherited;
  fSQLLogFile := ChangeFileExt(ExeVersion.ProgramFileName, '.history');
  mmoResult := TMemoEx.Create(self);
  mmoResult.Name := 'mmoResult';
  mmoResult.Parent := pnlRight;
  mmoResult.Align := alClient;
  mmoResult.Font.Height := -11;
  mmoResult.Font.Name := 'Consolas';
  mmoResult.ReadOnly := true;
  mmoResult.ScrollBars := ssVertical;
  mmoResult.Text := '';
  mmoResult.RightMargin := 130;
  mmoResult.RightMarginVisible := true;
  mmoResult.OnGetLineAttr := mmoResult.JSONLineAttr;
  pnlLeftTop.Height := 30;
  Tables := TStringList.Create;
  TableDblClickSelect.Init(false);
  CommandsToGrid.Init(false);
  CommandsToGrid.OnAfterAdd := OnCommandsToGridAdd;
end;

procedure TDBFrame.Open;
begin
  FillTables('');
  edtLabelsChange(nil);
  mmoSQL.Text := '#help';
  btnExecClick(nil);
  mmoSQL.Text := '';
  mmoResult.Text := '';
end;

procedure TDBFrame.FillTables(const customcode: string);
var
  i: integer;
  aTables: TRawUTF8DynArray;
begin
  drwgrdResult.Align := alClient;
  aTables := Admin.DatabaseTables(DatabaseName);
  Tables.Clear;
  Tables.BeginUpdate;
  try
    for i := 0 to high(aTables) do
      Tables.Add(UTF8ToString(aTables[i]));
  finally
    Tables.EndUpdate;
  end;
end;

procedure TDBFrame.lstTablesDblClick(Sender: TObject);
var
  i: integer;
  table, fields, sql, orderby: string;
begin
  i := lstTables.ItemIndex;
  if i < 0 then
    exit;
  table := lstTables.Items[i];
  fields := string(TableDblClickSelect.Value(RawUTF8(table)));
  if fields='' then
    fields := '*' else begin
    i := Pos(' order by ', fields);
    if i > 0 then begin
      orderby := copy(fields, i, maxInt);
      Setlength(fields, i - 1);
    end;
  end;
  sql := 'select '+fields+' from ' + table;
  if orderby <> '' then
    sql := sql + orderby
  else begin
    if TableDblClickOrderByIdDesc or ((TableDblClickOrderByIdDescCSV <> '') and
       (Pos(table + ',', TableDblClickOrderByIdDescCSV + ',') > 0)) then
      sql := sql + ' order by id desc';
    sql := sql + ' limit 1000';
  end;
  AddSQL(sql, true);
end;

procedure TDBFrame.SetResult(const JSON: RawUTF8);
begin
  FreeAndNil(Grid);
  drwgrdResult.Hide;
  mmoResult.Align := alClient;
  mmoResult.WordWrap := false;
  mmoResult.ScrollBars := ssBoth;
  mmoResult.RightMarginVisible := false;
  if (JSON = '') or (JSON[1] in ['A'..'Z', '#']) then
    mmoResult.OnGetLineAttr := nil
  else
    mmoResult.OnGetLineAttr := mmoResult.JSONLineAttr;
  mmoResult.Text := UTF8ToString(StringReplaceTabs(JSON, '    '));
  mmoResult.SetCaret(0, 0);
  mmoResult.TopRow := 0;
  fJson := '';
end;

procedure TDBFrame.OnCommandsToGridAdd(const Item: TSynNameValueItem;
  Index: PtrInt);
begin
  pmCmd.Items.Insert(0, NewCmdPopup(UTF8ToString(Item.Name), true));
end;

function TDBFrame.NewCmdPopup(const c: string; NoCmdTrim: boolean): TMenuItem;
var
  cmd, name, lastname: string;
  i, ext, num: integer;
  res: TDocVariantData;
  sub, subpar, subarch: TMenuItem;
begin
  result := TMenuItem.Create(self);
  if length(c) > 40 then
    result.Caption := copy(c, 1, 37) + '...'
  else
    result.Caption := c;
  if NoCmdTrim then
    cmd := c
  else begin
    i := Pos(' ', c);
    if i > 0 then
      cmd := copy(c, 1, i) + '*'
    else begin
      i := Pos('(', c);
      if i > 0 then
        cmd := copy(c, 1, i) + '*)'
      else
        cmd := c;
    end;
  end;
  result.Hint := cmd;
  if (cmd = '#log *') or (cmd = '#db *') then begin // log/db files in sub-menus
    res.InitJSON(ExecSQL(StringToUTF8(cmd)), JSON_OPTIONS_FAST);
    SetLength(cmd, length(cmd) - 1);
    subpar := result;
    subarch := nil;
    if res.Kind = dvArray then
      for i := 0 to res.Count - 1 do begin
        name := res.Values[i].Name;
        if name = lastname then
          continue; // circumvent FindFiles() bug with *.dbs including *.dbsynlz
        lastname := name;
        case GetFileNameExtIndex(name, 'dbs,dbsynlz') of
          0: begin // group sharded database files by 20 in sub-menus    
            ext := Pos('.dbs', name);
            if (ext > 4) and TryStrToInt(Copy(name, ext - 4, 4), num) then
              if (subpar = result) or (num mod 20 = 0) then begin
                subpar := NewCmdPopup(cmd + name + ' ...', true);
                subpar.OnClick := nil;
                result.Add(subpar);
              end;
          end;
          1: begin // group database backup files in a dedicated sub-menu
            if subarch = nil then begin
              subarch := NewCmdPopup(cmd + '*.dbsynlz ...', true);
              subarch.OnClick := nil;
              result.Add(subarch);
            end;
            subpar := subarch;
          end;
          else
            subpar := result;
        end;
        sub := NewCmdPopup(cmd + name, true);
        if cmd = '#log ' then
          sub.Caption := sub.Caption + '  ' + res.Values[i].TimeStamp
        else
          sub.Caption := format('%s  %s', [sub.Caption, KB(res.Values[i].Size)]);
        subpar.Add(sub);
      end;
  end
  else
    result.OnClick := btnExecClick;
end;

procedure TDBFrame.btnExecClick(Sender: TObject);
var
  res, ctyp, execTime: RawUTF8;
  mmo, cmd, fn, local: string;
  SelStart, SelLength, cmdToGrid, i: integer;
  table: TSQLTable;
  tables: TSQLRecordClassDynArray;
  P: PUTF8Char;
  exec: TServiceCustomAnswer;
  ctxt: variant;
  timer: TPrecisionTimer;
begin
  if (Sender <> nil) and Sender.InheritsFrom(TMenuItem) then begin
    mmo := TMenuItem(Sender).Hint;
    mmoSQL.Text := mmo;
    i := Pos('*', mmo);
    if (i > 0) and (mmo[1] = '#') then begin
      mmoSQL.SelStart := i - 1;
      mmoSQL.SelLength := 1;
      mmoSQL.SetFocus;
      exit;
    end;
  end;
  SelStart := mmoSQL.SelStart;
  SelLength := mmoSQL.SelLength;
  if SelLength > 10 then
    mmo := mmoSQL.SelText
  else
    mmo := mmoSQL.Lines.Text;
  fSQL := Trim(StringToUTF8(mmo));
  if fSQL = '' then
    exit;
  if IdemPropNameU(fSQL, '#client') then begin
    fJson := ObjectToJSON(Client);
  end
  else if Assigned(OnBeforeExecute) and not OnBeforeExecute(self, fSQL, '') then
    fJson := '"You are not allowed to execute this command for security reasons"'
  else begin
    Screen.Cursor := crHourGlass;
    try
      try
        timer.Start;
        exec := Admin.DatabaseExecute(DatabaseName, fSQL);
        execTime := timer.Stop;
        ctyp := FindIniNameValue(pointer(exec.Header), HEADER_CONTENT_TYPE_UPPER);
        if IdemPChar(pointer(exec.Content), '<HEAD>') then begin // HTML in disguise
          i := PosI('<BODY>', exec.content);
          if i = 0 then
            fJson := exec.Content
          else
            fJson := copy(exec.Content, i, maxInt);
        end
        else
        if (ctyp = '') or IdemPChar(pointer(ctyp), JSON_CONTENT_TYPE_UPPER) then
          fJson := exec.Content
        else
        if IdemPropNameU(ctyp, BINARY_CONTENT_TYPE) then begin
          fn := UTF8ToString(trim(FindIniNameValue(pointer(exec.Header), 'FILENAME:')));
          if (fn <> '') and (exec.Content <> '') then
            with TSaveDialog.Create(self) do
            try
              Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing];
              InitialDir := GetShellFolderPath(CSIDL_DOCUMENTS);
              FileName := SavePrefix + fn;
              if Execute then begin
                local := FileName;
                FileFromString(exec.Content, local);
              end;
            finally
              Free;
            end;
          fJson := JSONEncode(['file', fn, 'size', length(exec.Content),
            'type', ctyp, 'localfile', local]);
        end
        else
          fJson := FormatUTF8('"Unknown content-type: %"', [ctyp]);
      except
        on E: Exception do
          fJson := ObjectToJSON(E);
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
  FreeAndNil(Grid);
  GridLastTableName := '';
  fGridToCellRow := 0;
  cmdToGrid := CommandsToGrid.Find(fSQL);
  if (fSQL[1] = '#') and
     ((cmdToGrid < 0) or (CommandsToGrid.List[cmdToGrid].Tag < 0))  then begin
    if fJson <> '' then
      if IdemPropNameU(fSQL, '#help') then begin
        fJson := Trim(UnQuoteSQLString(fJson)) + '|#client'#13#10;
        res := StringReplaceAll(fJson, '|', #13#10' ');
        if pmCmd.Items.Count = 0 then begin
          P := pointer(res);
          while P <> nil do begin
            cmd := UTF8ToString(Trim(GetNextLine(P, P)));
            if (cmd <> '') and (cmd[1] = '#') then
              pmCmd.Items.Add(NewCmdPopup(cmd, false));
          end;
        end;
      end
      else if IdemPropNameU(fSQL, '#wrapper') then begin
        _Json(fJson,ctxt,JSON_OPTIONS_FAST);
        res := TSynMustache.Parse(WRAPPER_TEMPLATE).Render(ctxt, nil,
          TSynMustache.HelpersGetStandardList, nil, true);
      end
      else begin
        JSONBufferReformat(pointer(fJson), res, jsonUnquotedPropName);
        if (res = '') or (res = 'null') then
          res := fJson;
      end;
    if Assigned(OnAfterExecute) then
      OnAfterExecute(self,fSQL,res);
    SetResult(res);
  end
  else begin
    mmoResult.Text := '';
    mmoResult.SetCaret(0, 0);
    mmoResult.TopRow := 0;
    mmoResult.Align := alBottom;
    mmoResult.WordWrap := true;
    mmoResult.ScrollBars := ssVertical;
    mmoResult.Height := 100;
    if AssociatedModel <> nil then
      tables := AssociatedModel.Tables;
    if cmdToGrid >= 0 then begin
      GridLastTableName := CommandsToGrid.List[cmdToGrid].Name;
      if isSelect(pointer(GridLastTableName)) then
        GridLastTableName := GetTableNameFromSQLSelect(GridLastTableName,false);
      if CommandsToGrid.List[cmdToGrid].Value <> '' then begin
        // display a nested object in the grid
        P := JsonObjectItem(pointer(fJson), CommandsToGrid.List[cmdToGrid].Value);
        if CommandsToGrid.List[cmdToGrid].Tag > 0 then
          P := JSONArrayItem(P, CommandsToGrid.List[cmdToGrid].Tag - 1);
        if P <> nil then
          GetJSONItemAsRawJSON(P, RawJSON(fJSON));
      end;
    end
    else
      GridLastTableName := GetTableNameFromSQLSelect(fSQL, false);
    table := TSQLTableJSON.CreateFromTables(tables, fSQL, pointer(fJson), length(fJson));
    Grid := TSQLTableToGrid.Create(drwgrdResult, table, nil);
    Grid.SetAlignedByType(sftCurrency, alRight);
    Grid.SetFieldFixedWidth(100);
    Grid.FieldTitleTruncatedNotShownAsHint := true;
    Grid.OnValueText := OnText;
    Grid.Table.OnExportValue := OnGridToCell;
    if Assigned(OnAfterExecute) then
      OnAfterExecute(self, fSQL, fJSON);
    drwgrdResult.Options := drwgrdResult.Options - [goRowSelect];
    drwgrdResult.Show;
    mmoResult.OnGetLineAttr := mmoResult.JSONLineAttr;
    mmoResult.Text := Format(#13#10' Returned %d row(s), as %s in %s',
      [table.RowCount, KB(Length(fJson)), execTime]);
  end;
  if Sender <> nil then begin
    mmoSQL.SelStart := SelStart;
    mmoSQL.SelLength := SelLength;
    mmoSQL.SetFocus;
  end;
  if ((fJson <> '') or ((fSQL[1] = '#') and (PosEx(' ', fSQL) > 0))) and
     (fSQL <> fPreviousSQL) then begin
    AppendToTextFile(fSQL, fSQLLogFile);
    fPreviousSQL := fSQL;
  end;
end;

destructor TDBFrame.Destroy;
begin
  FreeAndNil(Grid);
  FreeAndNil(AssociatedModel);
  FreeAndNil(Tables);
  inherited;
end;

function TDBFrame.OnText(Sender: TSQLTable; FieldIndex, RowIndex: Integer;
  var Text: string): boolean;
begin
  if Sender.FieldType(FieldIndex) in [sftBoolean] then
    result := false
  else begin
    Text := Sender.GetString(RowIndex, FieldIndex); // display the value as such
    result := true;
  end;
end;

function TDBFrame.OnGridToCell(Sender: TSQLTable; Row, Field: integer;
  HumanFriendly: boolean): RawJSON;
var
  methodName: RawUTF8;
  serv, m: integer;
begin
  if fGridToCellRow <> Row then begin
    Sender.ToDocVariant(Row, fGridToCellVariant, JSON_OPTIONS_FAST, true, true, true);
    fGridToCellRow := Row;
    if AssociatedServices <> nil then
    with _Safe(fGridToCellVariant)^ do
      if GetAsRawUTF8('Method', methodName) then
        for serv := 0 to high(AssociatedServices) do begin
          m := AssociatedServices[serv].FindFullMethodIndex(methodName, true);
          if m >= 0 then
            with AssociatedServices[serv].Methods[m] do begin
              ArgsAsDocVariantFix(GetAsDocVariantSafe('Input')^, true);
              ArgsAsDocVariantFix(GetAsDocVariantSafe('Output')^, false);
              break;
            end;
        end;
  end;
  with _Safe(fGridToCellVariant)^ do
    if cardinal(Field)>=cardinal(Count) then
      result := '' else
      if HumanFriendly and (_Safe(Values[Field])^.Kind = dvUndefined) then
        VariantToUTF8(Values[field], RawUTF8(result))
      else
        result := VariantSaveJSON(Values[Field]);
end;

procedure TDBFrame.drwgrdResultClick(Sender: TObject);
var
  R: integer;
  json: RawUTF8;
begin
  R := drwgrdResult.Row;
  if (R > 0) and (R <> fGridToCellRow) and (Grid <> nil) then begin
    OnGridToCell(Grid.Table,R,0,false);
    JSONBufferReformat(pointer(VariantToUTF8(fGridToCellVariant)), json, jsonUnquotedPropNameCompact);
    mmoResult.OnGetLineAttr := mmoResult.JSONLineAttr;
    mmoResult.Text := UTF8ToString(json);
    mmoResult.SetCaret(0, 0);
    mmoResult.TopRow := 0;
  end;
end;

procedure TDBFrame.btnHistoryClick(Sender: TObject);
var
  F: TForm;
  List: TListBox;
  Search: TEdit;
  Details: TMemo;
begin
  F := TForm.Create(Application);
  try
    F.Caption := ' ' + btnHistory.Hint;
    F.Font := Font;
    F.Width := 800;
    F.Height := 600;
    F.Position := poMainFormCenter;
    Search := TEdit.Create(F);
    Search.Parent := F;
    Search.Align := alTop;
    Search.Height := 24;
    Search.OnChange := LogSearch;
    Details := TMemo.Create(F);
    Details.Parent := F;
    Details.Align := alBottom;
    Details.Height := 200;
    Details.readonly := true;
    Details.Font.Name := 'Consolas';
    List := TListBox.Create(F);
    with List do begin
      Parent := F;
      Align := alClient;
      Tag := PtrInt(Details);
      OnClick := LogClick;
      OnDblClick := LogDblClick;
    end;
    Search.Tag := PtrInt(List);
    LogSearch(Search);
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TDBFrame.LogClick(Sender: TObject);
var
  List: TListBox absolute Sender;
  ndx: integer;
begin
  ndx := cardinal(List.ItemIndex);
  if ndx >= 0 then
    TMemo(List.Tag).Text := copy(List.Items[ndx], 21, maxInt)
  else
    TMemo(List.Tag).Clear;
end;

procedure TDBFrame.LogDblClick(Sender: TObject);
var
  List: TListBox absolute Sender;
  SQL: string;
  ndx: integer;
begin
  ndx := cardinal(List.ItemIndex);
  if ndx >= 0 then begin
    SQL := copy(List.Items[ndx], 21, maxInt);
    AddSQL(SQL, IsSelect(pointer(StringToAnsi7(SQL))));
    TForm(List.Owner).Close;
  end;
end;

procedure TDBFrame.LogSearch(Sender: TObject);
const
  MAX_LINES_IN_HISTORY = 500;
var
  Edit: TEdit absolute Sender;
  List: TListBox;
  i: integer;
  s: RawUTF8;
begin
  s := SynCommons.UpperCase(StringToUTF8(Edit.Text));
  List := pointer(Edit.Tag);
  with TMemoryMapText.Create(fSQLLogFile) do
  try
    List.Items.BeginUpdate;
    List.Items.Clear;
    for i := Count - 1 downto 0 do
      if (s = '') or LineContains(s, i) then
        if List.Items.Add(Strings[i]) > MAX_LINES_IN_HISTORY then
          break; // read last 500 lines from UTF-8 file
  finally
    Free;
    List.Items.EndUpdate;
  end;
  List.ItemIndex := 0;
  LogClick(List);
end;

procedure TDBFrame.AddSQL(SQL: string; AndExec: boolean);
var
  len: integer;
  orig: string;
begin
  SQL := SysUtils.Trim(SQL);
  len := Length(SQL);
  if len = 0 then
    exit;
  orig := mmoSQL.Lines.Text;
  if orig <> '' then
    SQL := #13#10#13#10 + SQL;
  SQL := orig + SQL;
  mmoSQL.Lines.Text := SQL;
  mmoSQL.SelStart := length(SQL) - len;
  mmoSQL.SelLength := len;
  if AndExec then
    btnExecClick(btnExec)
  else
    mmoSQL.SetFocus;
end;

procedure TDBFrame.btnCmdClick(Sender: TObject);
begin
  with ClientToScreen(btnCmd.BoundsRect.TopLeft) do
    pmCmd.Popup(X, Y + btnCmd.Height);
end;

function TDBFrame.ExecSQL(const SQL: RawUTF8): RawUTF8;
var
  exec: TServiceCustomAnswer;
begin
  exec := Admin.DatabaseExecute(DatabaseName, sql);
  result := exec.Content;
end;

procedure TDBFrame.EnableChkTables(const aCaption: string);
begin
  pnlLeftTop.Height := 44;
  chkTables.Show;
  chkTables.Caption := aCaption;
end;

procedure TDBFrame.edtLabelsChange(Sender: TObject);
var
  i, index: integer;
  match, previous: string;
begin
  i := lstTables.ItemIndex;
  if i >= 0 then
    previous := lstTables.Items[i];
  index := -1;
  match := SysUtils.Trim(SysUtils.UpperCase(edtLabels.Text));
  if (length(match) > 5) and (match[1] = '%') then begin
    FillTables(match);
    match := '';
  end;
  with lstTables.Items do
  try
    BeginUpdate;
    Clear;
    for i := 0 to Tables.Count - 1 do
      if (match = '') or (Pos(match, SysUtils.UpperCase(Tables[i])) > 0) then begin
        AddObject(Tables[i], Tables.Objects[i]);
        if previous = Tables[i] then
          index := Count - 1;
      end;
  finally
    EndUpdate;
  end;
  if index >= 0 then
    lstTables.ItemIndex := index;
end;


end.

