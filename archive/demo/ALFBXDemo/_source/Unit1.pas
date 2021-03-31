unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, ALFbxClient, Shellapi;

type

  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    Label2: TLabel;
    Label4: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    ALButtonFirebirdStartTransaction: TButton;
    ALEditFirebirdLogin: TEdit;
    ALEditFirebirdPassword: TEdit;
    ALEditFirebirdCharset: TEdit;
    ALEditFirebirdLib: TEdit;
    ALMemoFireBirdQuery: TMemo;
    ALEditFirebirdDatabase: TEdit;
    ALButtonFirebirdCommit: TButton;
    ALButtonFirebirdSelect: TButton;
    ALButtonFirebirdRollBack: TButton;
    ALEditFireBirdNum_buffers: TEdit;
    ALButtonFirebirdCreateDatabase: TButton;
    ALComboBoxFirebirdapiVer: TCombobox;
    Label1: TLabel;
    ALMemoFirebirdTPB: TMemo;
    ALButtonFirebirdUpdate: TButton;
    ALButtonFirebirdOpenConnection: TButton;
    ALButtonCloseConnection: TButton;
    ALButtonFirebirdPrepare: TButton;
    Label3: TLabel;
    Label5: TLabel;
    Panel3: TPanel;
    ALMemoFireBirdParams: TMemo;
    ALMemoFirebirdResult: TMemo;
    ALMemoFirebirdStats: TMemo;
    Splitter1: TSplitter;
    ALButtonFirebirdCommitRetaining: TButton;
    ALButtonFirebirdRollBackRetaining: TButton;
    Button1: TButton;
    Button2: TButton;
    procedure ALButtonFirebirdUpdateClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ALButtonFirebirdCreateDatabaseClick(Sender: TObject);
    procedure ALButtonFirebirdStartTransactionClick(Sender: TObject);
    procedure ALButtonFirebirdCommitClick(Sender: TObject);
    procedure ALButtonFirebirdRollBackClick(Sender: TObject);
    procedure ALButtonFirebirdSelectClick(Sender: TObject);
    procedure ALButtonFirebirdOpenConnectionClick(Sender: TObject);
    procedure ALButtonCloseConnectionClick(Sender: TObject);
    procedure ALEditFirebirdLibButtonClick(Sender: TObject);
    procedure ALButtonFirebirdPrepareClick(Sender: TObject);
    procedure ALButtonFirebirdCommitRetainingClick(Sender: TObject);
    procedure ALButtonFirebirdRollBackRetainingClick(Sender: TObject);
  private
    fAlFbxClient: TalFBXclient;
  public
  end;

var Form1: TForm1;

implementation

uses AlFbxBase,
     ALWindows,
     AlFbxLib,
     ALString,
     alStringList,
     ALHTML,
     ALXmlDoc;

{$R *.dfm}

{****************************************************************************************************************************}
function SQLFastTagReplaceFunct(const TagString: AnsiString; TagParams: TALStrings; ExtData: pointer; Var Handled: Boolean): AnsiString;
Var aMin, aMax, aIndex: integer;
    aLstSavedData: TALStringList;
begin

  Handled := True;
  if ALSameText(TagString,'randomchar') then begin
    if not ALTryStrToInt(TagParams.Values['Index'], aIndex) then aIndex := -1;
    if aIndex >= 0 then begin
      aLstSavedData := TALStringList(ExtData^);
      result := aLstSavedData.Values['randomchar_'+ALIntToStr(aIndex)];
      if result = '' then begin
        result := AlRandomStr(1);
        aLstSavedData.Values['randomchar_'+ALIntToStr(aIndex)] := result;
      end;
    end
    else result := AlRandomStr(1);
  end
  else if ALSameText(TagString,'randomstring') then begin
    if not ALTryStrToInt(TagParams.Values['MinLength'], aMin) then aMin := 1;
    if not ALTryStrToInt(TagParams.Values['MaxLength'], aMax) then aMax := 255;
    if not ALTryStrToInt(TagParams.Values['Index'], aIndex) then aIndex := -1;
    if aIndex >= 0 then begin
      aLstSavedData := TALStringList(ExtData^);
      result := aLstSavedData.Values['randomstring_'+ALIntToStr(aIndex)];
      if result = '' then begin
        result := AlRandomStr(aMin + random(aMax - aMin + 1));
        aLstSavedData.Values['randomstring_'+ALIntToStr(aIndex)] := result;
      end;
    end
    else result := AlRandomStr(aMin + random(aMax - aMin + 1));
  end
  else if ALSameText(TagString,'randomnumber') then begin
    if not ALTryStrToInt(TagParams.Values['Min'], aMin) then aMin := 1;
    if not ALTryStrToInt(TagParams.Values['Max'], aMax) then aMax := Maxint;
    if not ALTryStrToInt(TagParams.Values['Index'], aIndex) then aIndex := -1;
    if aIndex >= 0 then begin
      aLstSavedData := TALStringList(ExtData^);
      result := aLstSavedData.Values['randomnumber_'+ALIntToStr(aIndex)];
      if result = '' then begin
        result := ALIntToStr(aMin + random(aMax - aMin + 1));
        aLstSavedData.Values['randomnumber_'+ALIntToStr(aIndex)] := result;
      end;
    end
    else result := ALIntToStr(aMin + random(aMax - aMin + 1));
  end
  else Handled := False;

end;

{********************************************************************}
procedure TForm1.ALButtonFirebirdCreateDatabaseClick(Sender: TObject);
Var aFBXClient: TalFBXClient;
    aFBAPiVersion: TALFBXVersion_API;
    aTickCount: UInt64;
begin

  case ALComboBoxFirebirdapiVer.ItemIndex of
    1: aFBAPiVersion := FB103;
    2: aFBAPiVersion := FB15;
    3: aFBAPiVersion := FB20;
    4: aFBAPiVersion := FB25;
    else aFBAPiVersion := FB102;
  end;

  aFBXClient := TALFbxClient.Create(aFBAPiVersion,AnsiString(ALEditFirebirdLib.Text));
  Try

    aTickCount := GetTickCount64;
    aFBXClient.CreateDatabase(AnsiString(AlMemoFireBirdQuery.Lines.Text));
    ALMemoFirebirdStats.Clear;
    ALMemoFirebirdStats.Lines.Add('Time Taken: ' + IntToStr(GetTickCount64 - aTickCount) + ' ms');

    ALMemoFirebirdResult.Lines.Text := 'Create DataBase Done';

  Finally
    aFBXClient.free;
  End;

end;

{********************************************************************}
procedure TForm1.ALButtonFirebirdOpenConnectionClick(Sender: TObject);
Var aFBAPiVersion: TALFBXVersion_API;
    aTickCount: UInt64;
begin

  ALButtonFirebirdOpenConnection.Enabled := False;
  ALButtonFirebirdStartTransaction.Enabled := True;
  ALButtonFirebirdSelect.Enabled := False;
  ALButtonFirebirdPrepare.Enabled := False;
  ALButtonFirebirdUpdate.Enabled := False;
  ALButtonFirebirdCommit.Enabled := False;
  ALButtonFirebirdCommitRetaining.Enabled := False;
  ALButtonFirebirdRollBack.Enabled := False;
  ALButtonFirebirdRollBackRetaining.Enabled := False;
  ALButtonCloseConnection.Enabled := True;
  Try

    if not assigned(fAlFbxClient) then begin

      case ALComboBoxFirebirdapiVer.ItemIndex of
        1: aFBAPiVersion := FB103;
        2: aFBAPiVersion := FB15;
        3: aFBAPiVersion := FB20;
        4: aFBAPiVersion := FB25;
        else aFBAPiVersion := FB102;
      end;

      fAlFbxClient := TALFbxClient.Create(aFBAPiVersion,AnsiString(ALEditFirebirdLib.Text));
      Try

        aTickCount := GetTickCount64;
        fAlFbxClient.connect(AnsiString(ALEditFireBirdDatabase.Text),
                             AnsiString(ALEditFireBirdLogin.text),
                             AnsiString(ALEditFireBirdPassword.text),
                             AnsiString(ALEditFireBirdCharset.Text),
                             StrtoInt(ALEditFireBirdNum_buffers.Text));
        ALMemoFirebirdStats.Clear;
        ALMemoFirebirdStats.Lines.Add('Time Taken: ' + IntToStr(GetTickCount64 - aTickCount) + ' ms');

        ALMemoFirebirdResult.Lines.Text := 'Open Connection Done';

      Except
        fAlFbxClient.Free;
        fAlFbxClient := Nil;
        raise;
      end;

    end;

  except
    ALButtonFirebirdOpenConnection.Enabled := True;
    ALButtonFirebirdStartTransaction.Enabled := False;
    ALButtonFirebirdSelect.Enabled := False;
    ALButtonFirebirdPrepare.Enabled := False;
    ALButtonFirebirdUpdate.Enabled := False;
    ALButtonFirebirdCommit.Enabled := False;
    ALButtonFirebirdCommitRetaining.Enabled := False;
    ALButtonFirebirdRollBack.Enabled := False;
    ALButtonFirebirdRollBackRetaining.Enabled := False;
    ALButtonCloseConnection.Enabled := False;
    Raise;
  End;

end;

{**********************************************************************}
procedure TForm1.ALButtonFirebirdStartTransactionClick(Sender: TObject);
Var aTPB: AnsiString;
    aIOStats_1: TALFBXClientMonitoringIOStats;
    aRecordStats_1: TALFBXClientMonitoringRecordStats;
    aIOStats_2: TALFBXClientMonitoringIOStats;
    aRecordStats_2: TALFBXClientMonitoringRecordStats;
    aMemoryUsage: TALFBXClientMonitoringMemoryUsage;
    aTickCount: UInt64;
begin

  ALButtonFirebirdOpenConnection.Enabled := False;
  ALButtonFirebirdStartTransaction.Enabled := False;
  ALButtonFirebirdSelect.Enabled := True;
  ALButtonFirebirdPrepare.Enabled := true;
  ALButtonFirebirdUpdate.Enabled := True;
  ALButtonFirebirdCommit.Enabled := True;
  ALButtonFirebirdCommitRetaining.Enabled := True;
  ALButtonFirebirdRollBack.Enabled := True;
  ALButtonFirebirdRollBackRetaining.Enabled := True;
  ALButtonCloseConnection.Enabled := True;
  Try

    aTPB:= ALTrim(AnsiString(ALMemoFireBirdTPB.Lines.Text));
    aTPB := AlStringReplace(aTPB, 'isc_tpb_version3', isc_tpb_version3, [rfIgnoreCase]);
    aTPB := AlStringReplace(aTPB, 'isc_tpb_read_committed', isc_tpb_read_committed, [rfIgnoreCase]);
    aTPB := AlStringReplace(aTPB, 'isc_tpb_concurrency', isc_tpb_concurrency, [rfIgnoreCase]);
    aTPB := AlStringReplace(aTPB, 'isc_tpb_consistency', isc_tpb_consistency, [rfIgnoreCase]);
    aTPB := AlStringReplace(aTPB, 'isc_tpb_no_rec_version', isc_tpb_no_rec_version, [rfIgnoreCase]);
    aTPB := AlStringReplace(aTPB, 'isc_tpb_rec_version', isc_tpb_rec_version, [rfIgnoreCase]);
    aTPB := AlStringReplace(aTPB, 'isc_tpb_write', isc_tpb_write, [rfIgnoreCase]);
    aTPB := AlStringReplace(aTPB, 'isc_tpb_read', isc_tpb_read, [rfIgnoreCase]);
    aTPB := AlStringReplace(aTPB, 'isc_tpb_nowait', isc_tpb_nowait, [rfIgnoreCase]);
    aTPB := AlStringReplace(aTPB, 'isc_tpb_wait', isc_tpb_wait, [rfIgnoreCase]);
    aTPB := AlStringReplace(aTPB, #13#10, '', [rfReplaceALL]);
    aTPB := AlStringReplace(aTPB, ' ', '', [rfReplaceALL]);

    FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                    -1,
                                    '',
                                    aIOStats_1,
                                    aRecordStats_1,
                                    aMemoryUsage,
                                    False,
                                    False,
                                    True);
    aTickCount := GetTickCount64;
    fAlFbxClient.TransactionStart(aTPB);
    ALMemoFirebirdStats.Clear;
    ALMemoFirebirdStats.Lines.Add('Time Taken: ' + IntToStr(GetTickCount64 - aTickCount) + ' ms');
    FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                    -1,
                                    '',
                                    aIOStats_2,
                                    aRecordStats_2,
                                    aMemoryUsage);
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('page_reads:   ' + IntToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
    ALMemoFirebirdStats.Lines.Add('page_writes:  ' + IntToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
    ALMemoFirebirdStats.Lines.Add('page_fetches: ' + IntToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
    ALMemoFirebirdStats.Lines.Add('page_marks:   ' + IntToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('record_idx_reads: ' + IntToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
    ALMemoFirebirdStats.Lines.Add('record_seq_reads: ' + IntToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
    ALMemoFirebirdStats.Lines.Add('record_inserts:   ' + IntToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
    ALMemoFirebirdStats.Lines.Add('record_updates:   ' + IntToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
    ALMemoFirebirdStats.Lines.Add('record_deletes:   ' + IntToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
    ALMemoFirebirdStats.Lines.Add('record_backouts:  ' + IntToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
    ALMemoFirebirdStats.Lines.Add('record_purges:    ' + IntToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
    ALMemoFirebirdStats.Lines.Add('record_expunges:  ' + IntToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('memory_used:          ' + IntToStr(aMemoryUsage.memory_used));
    ALMemoFirebirdStats.Lines.Add('memory_allocated:     ' + IntToStr(aMemoryUsage.memory_allocated));
    ALMemoFirebirdStats.Lines.Add('max_memory_used:      ' + IntToStr(aMemoryUsage.max_memory_used));
    ALMemoFirebirdStats.Lines.Add('max_memory_allocated: ' + IntToStr(aMemoryUsage.max_memory_allocated));

    ALMemoFirebirdResult.Lines.Text := 'Start Transaction Done';

  except
    ALButtonFirebirdOpenConnection.Enabled := False;
    ALButtonFirebirdStartTransaction.Enabled := True;
    ALButtonFirebirdSelect.Enabled := False;
    ALButtonFirebirdPrepare.Enabled := False;
    ALButtonFirebirdUpdate.Enabled := False;
    ALButtonFirebirdCommit.Enabled := False;
    ALButtonFirebirdCommitRetaining.Enabled := False;
    ALButtonFirebirdRollBack.Enabled := False;
    ALButtonFirebirdRollBackRetaining.Enabled := False;
    ALButtonCloseConnection.Enabled := True;
    Raise;
  End;

end;


{*************************************************************}
procedure TForm1.ALButtonFirebirdPrepareClick(Sender: TObject);
var aIOStats_1: TALFBXClientMonitoringIOStats;
    aRecordStats_1: TALFBXClientMonitoringRecordStats;
    aIOStats_2: TALFBXClientMonitoringIOStats;
    aRecordStats_2: TALFBXClientMonitoringRecordStats;
    aMemoryUsage: TALFBXClientMonitoringMemoryUsage;
    aTickCount: UInt64;
    aSQl: AnsiString;
    aLst: TALStringList;
begin

  aLst := TALStringList.create;
  try
    aSQl := ALFastTagReplace(AnsiString(AlMemoFireBirdQuery.Lines.Text),
                             '<#',
                             '>',
                             SQLFastTagReplaceFunct,
                             True,
                             pointer(aLst))
  finally
    aLst.free;
  end;

  FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                  -1,
                                  '',
                                  aIOStats_1,
                                  aRecordStats_1,
                                  aMemoryUsage,
                                  False,
                                  False,
                                  True);
  aTickCount := GetTickCount64;
  FAlFBXClient.Prepare(aSQl);
  ALMemoFirebirdStats.Clear;
  ALMemoFirebirdStats.Lines.Add('Time Taken: ' + IntToStr(GetTickCount64 - aTickCount) + ' ms');
  FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                  -1,
                                  '',
                                  aIOStats_2,
                                  aRecordStats_2,
                                  aMemoryUsage);
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('page_reads:   ' + IntToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
  ALMemoFirebirdStats.Lines.Add('page_writes:  ' + IntToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
  ALMemoFirebirdStats.Lines.Add('page_fetches: ' + IntToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
  ALMemoFirebirdStats.Lines.Add('page_marks:   ' + IntToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('record_idx_reads: ' + IntToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
  ALMemoFirebirdStats.Lines.Add('record_seq_reads: ' + IntToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
  ALMemoFirebirdStats.Lines.Add('record_inserts:   ' + IntToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
  ALMemoFirebirdStats.Lines.Add('record_updates:   ' + IntToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
  ALMemoFirebirdStats.Lines.Add('record_deletes:   ' + IntToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
  ALMemoFirebirdStats.Lines.Add('record_backouts:  ' + IntToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
  ALMemoFirebirdStats.Lines.Add('record_purges:    ' + IntToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
  ALMemoFirebirdStats.Lines.Add('record_expunges:  ' + IntToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('memory_used:          ' + IntToStr(aMemoryUsage.memory_used));
  ALMemoFirebirdStats.Lines.Add('memory_allocated:     ' + IntToStr(aMemoryUsage.memory_allocated));
  ALMemoFirebirdStats.Lines.Add('max_memory_used:      ' + IntToStr(aMemoryUsage.max_memory_used));
  ALMemoFirebirdStats.Lines.Add('max_memory_allocated: ' + IntToStr(aMemoryUsage.max_memory_allocated));

  ALMemoFirebirdResult.Lines.Text := 'Prepare Done';

end;

{************************************************************}
procedure TForm1.ALButtonFirebirdSelectClick(Sender: TObject);
var aXMLDATA: TalXmlDocument;
    aFormatSettings: TALFormatSettings;
    aQuery: TALFBXClientSelectDataQuery;
    aIOStats_1: TALFBXClientMonitoringIOStats;
    aRecordStats_1: TALFBXClientMonitoringRecordStats;
    aIOStats_2: TALFBXClientMonitoringIOStats;
    aRecordStats_2: TALFBXClientMonitoringRecordStats;
    aMemoryUsage: TALFBXClientMonitoringMemoryUsage;
    aTickCount: UInt64;
    aLst1: TALStringList;
    i: integer;
begin

  aFormatSettings := ALDefaultFormatSettings;
  aXMLDATA := TALXmlDocument.create('root');
  Try

    With aXMLDATA Do Begin
      Options := [doNodeAutoIndent];
      ParseOptions := [poPreserveWhiteSpace];
    end;

    aLst1 := TALStringList.create;
    try
      aQuery := TALFBXClientSelectDataQuery.Create;
      aQuery.SQL := ALFastTagReplace(AnsiString(AlMemoFireBirdQuery.Lines.Text),
                                     '<#',
                                     '>',
                                     SQLFastTagReplaceFunct,
                                     True,
                                     @aLst1)
    finally
      aLst1.free;
    end;

    if ALMemoFireBirdParams.Lines.Count > 0 then begin
      Setlength(aQuery.Params, ALMemoFireBirdParams.Lines.Count);
      for I := 0 to ALMemoFireBirdParams.Lines.Count - 1 do begin
        aLst1 := TALStringList.create;
        try
          aQuery.Params[i].Value := ALFastTagReplace(AnsiString(ALMemoFireBirdParams.Lines[i]),
                                                     '<#',
                                                     '>',
                                                     SQLFastTagReplaceFunct,
                                                     True,
                                                     @aLst1)
        finally
          aLst1.free;
        end;
        aQuery.Params[i].isnull := False;
      end;
    end
    else Setlength(aQuery.Params, 0);
    aQuery.RowTag := 'rec';
    aQuery.ViewTag := '';
    aQuery.Skip := 0;
    aQuery.First := 200;

    FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                    -1,
                                    '',
                                    aIOStats_1,
                                    aRecordStats_1,
                                    aMemoryUsage,
                                    False,
                                    False,
                                    True);
    aTickCount := GetTickCount64;
    FAlFBXClient.SelectData(aQuery,
                            aXMLDATA.DocumentElement,
                            aFormatSettings);
    ALMemoFirebirdStats.Clear;
    ALMemoFirebirdStats.Lines.Add('Time Taken: ' + IntToStr(GetTickCount64 - aTickCount) + ' ms');
    FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                    -1,
                                    '',
                                    aIOStats_2,
                                    aRecordStats_2,
                                    aMemoryUsage);
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('page_reads:   ' + IntToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
    ALMemoFirebirdStats.Lines.Add('page_writes:  ' + IntToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
    ALMemoFirebirdStats.Lines.Add('page_fetches: ' + IntToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
    ALMemoFirebirdStats.Lines.Add('page_marks:   ' + IntToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('record_idx_reads: ' + IntToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
    ALMemoFirebirdStats.Lines.Add('record_seq_reads: ' + IntToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
    ALMemoFirebirdStats.Lines.Add('record_inserts:   ' + IntToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
    ALMemoFirebirdStats.Lines.Add('record_updates:   ' + IntToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
    ALMemoFirebirdStats.Lines.Add('record_deletes:   ' + IntToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
    ALMemoFirebirdStats.Lines.Add('record_backouts:  ' + IntToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
    ALMemoFirebirdStats.Lines.Add('record_purges:    ' + IntToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
    ALMemoFirebirdStats.Lines.Add('record_expunges:  ' + IntToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('memory_used:          ' + IntToStr(aMemoryUsage.memory_used));
    ALMemoFirebirdStats.Lines.Add('memory_allocated:     ' + IntToStr(aMemoryUsage.memory_allocated));
    ALMemoFirebirdStats.Lines.Add('max_memory_used:      ' + IntToStr(aMemoryUsage.max_memory_used));
    ALMemoFirebirdStats.Lines.Add('max_memory_allocated: ' + IntToStr(aMemoryUsage.max_memory_allocated));

    ALMemoFirebirdResult.Lines.Text := String(aXMLDATA.XML);

  Finally
    aXMLDATA.free;
  End;

end;

{************************************************************}
procedure TForm1.ALButtonFirebirdUpdateClick(Sender: TObject);
var aQuery: TALFBXClientUpdateDataQuery;
    aIOStats_1: TALFBXClientMonitoringIOStats;
    aRecordStats_1: TALFBXClientMonitoringRecordStats;
    aIOStats_2: TALFBXClientMonitoringIOStats;
    aRecordStats_2: TALFBXClientMonitoringRecordStats;
    aMemoryUsage: TALFBXClientMonitoringMemoryUsage;
    aTickCount: UInt64;
    aLst1: TALStringList;
    i: integer;
begin

  aLst1 := TALStringList.create;
  try
    aQuery := TALFBXClientUpdateDataQuery.Create;
    aQuery.SQL := ALFastTagReplace(AnsiString(AlMemoFireBirdQuery.Lines.Text),
                                   '<#',
                                   '>',
                                   SQLFastTagReplaceFunct,
                                   True,
                                   @aLst1)
  finally
    aLst1.free;
  end;

  if ALMemoFireBirdParams.Lines.Count > 0 then begin
    Setlength(aQuery.Params, ALMemoFireBirdParams.Lines.Count);
    for I := 0 to ALMemoFireBirdParams.Lines.Count - 1 do begin
      aLst1 := TALStringList.create;
      try
        aQuery.Params[i].Value := ALFastTagReplace(AnsiString(ALMemoFireBirdParams.Lines[i]),
                                                   '<#',
                                                   '>',
                                                   SQLFastTagReplaceFunct,
                                                   True,
                                                   @aLst1)
      finally
        aLst1.free;
      end;
      aQuery.Params[i].isnull := False;
    end;
  end
  else Setlength(aQuery.Params, 0);
  FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                  -1,
                                  '',
                                  aIOStats_1,
                                  aRecordStats_1,
                                  aMemoryUsage,
                                  False,
                                  False,
                                  True);
  aTickCount := GetTickCount64;
  FAlFBXClient.UpdateData(aQuery);
  ALMemoFirebirdStats.Clear;
  ALMemoFirebirdStats.Lines.Add('Time Taken: ' + IntToStr(GetTickCount64 - aTickCount) + ' ms');
  FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                  -1,
                                  '',
                                  aIOStats_2,
                                  aRecordStats_2,
                                  aMemoryUsage);
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('page_reads:   ' + IntToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
  ALMemoFirebirdStats.Lines.Add('page_writes:  ' + IntToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
  ALMemoFirebirdStats.Lines.Add('page_fetches: ' + IntToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
  ALMemoFirebirdStats.Lines.Add('page_marks:   ' + IntToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('record_idx_reads: ' + IntToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
  ALMemoFirebirdStats.Lines.Add('record_seq_reads: ' + IntToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
  ALMemoFirebirdStats.Lines.Add('record_inserts:   ' + IntToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
  ALMemoFirebirdStats.Lines.Add('record_updates:   ' + IntToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
  ALMemoFirebirdStats.Lines.Add('record_deletes:   ' + IntToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
  ALMemoFirebirdStats.Lines.Add('record_backouts:  ' + IntToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
  ALMemoFirebirdStats.Lines.Add('record_purges:    ' + IntToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
  ALMemoFirebirdStats.Lines.Add('record_expunges:  ' + IntToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('memory_used:          ' + IntToStr(aMemoryUsage.memory_used));
  ALMemoFirebirdStats.Lines.Add('memory_allocated:     ' + IntToStr(aMemoryUsage.memory_allocated));
  ALMemoFirebirdStats.Lines.Add('max_memory_used:      ' + IntToStr(aMemoryUsage.max_memory_used));
  ALMemoFirebirdStats.Lines.Add('max_memory_allocated: ' + IntToStr(aMemoryUsage.max_memory_allocated));

  ALMemoFirebirdResult.Lines.Text := 'Update Done';

end;

{************************************************************}
procedure TForm1.ALButtonFirebirdCommitClick(Sender: TObject);
var aIOStats_1: TALFBXClientMonitoringIOStats;
    aRecordStats_1: TALFBXClientMonitoringRecordStats;
    aIOStats_2: TALFBXClientMonitoringIOStats;
    aRecordStats_2: TALFBXClientMonitoringRecordStats;
    aMemoryUsage: TALFBXClientMonitoringMemoryUsage;
    aTickCount: UInt64;
begin

  ALButtonFirebirdOpenConnection.Enabled := False;
  ALButtonFirebirdStartTransaction.Enabled := True;
  ALButtonFirebirdSelect.Enabled := False;
  ALButtonFirebirdPrepare.Enabled := False;
  ALButtonFirebirdUpdate.Enabled := False;
  ALButtonFirebirdCommit.Enabled := False;
  ALButtonFirebirdCommitRetaining.Enabled := False;
  ALButtonFirebirdRollBack.Enabled := False;
  ALButtonFirebirdRollBackRetaining.Enabled := False;
  ALButtonCloseConnection.Enabled := True;
  Try

    FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                    -1,
                                    '',
                                    aIOStats_1,
                                    aRecordStats_1,
                                    aMemoryUsage,
                                    False,
                                    False,
                                    True);
    aTickCount := GetTickCount64;
    fAlFbxClient.TransactionCommit;
    ALMemoFirebirdStats.Clear;
    ALMemoFirebirdStats.Lines.Add('Time Taken: ' + IntToStr(GetTickCount64 - aTickCount) + ' ms');
    FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                    -1,
                                    '',
                                    aIOStats_2,
                                    aRecordStats_2,
                                    aMemoryUsage);
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('page_reads:   ' + IntToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
    ALMemoFirebirdStats.Lines.Add('page_writes:  ' + IntToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
    ALMemoFirebirdStats.Lines.Add('page_fetches: ' + IntToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
    ALMemoFirebirdStats.Lines.Add('page_marks:   ' + IntToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('record_idx_reads: ' + IntToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
    ALMemoFirebirdStats.Lines.Add('record_seq_reads: ' + IntToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
    ALMemoFirebirdStats.Lines.Add('record_inserts:   ' + IntToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
    ALMemoFirebirdStats.Lines.Add('record_updates:   ' + IntToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
    ALMemoFirebirdStats.Lines.Add('record_deletes:   ' + IntToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
    ALMemoFirebirdStats.Lines.Add('record_backouts:  ' + IntToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
    ALMemoFirebirdStats.Lines.Add('record_purges:    ' + IntToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
    ALMemoFirebirdStats.Lines.Add('record_expunges:  ' + IntToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('memory_used:          ' + IntToStr(aMemoryUsage.memory_used));
    ALMemoFirebirdStats.Lines.Add('memory_allocated:     ' + IntToStr(aMemoryUsage.memory_allocated));
    ALMemoFirebirdStats.Lines.Add('max_memory_used:      ' + IntToStr(aMemoryUsage.max_memory_used));
    ALMemoFirebirdStats.Lines.Add('max_memory_allocated: ' + IntToStr(aMemoryUsage.max_memory_allocated));

    ALMemoFirebirdResult.Lines.Text := 'Commit Transaction Done';

  except
    ALButtonFirebirdOpenConnection.Enabled := False;
    ALButtonFirebirdStartTransaction.Enabled := False;
    ALButtonFirebirdSelect.Enabled := True;
    ALButtonFirebirdPrepare.Enabled := True;
    ALButtonFirebirdUpdate.Enabled := True;
    ALButtonFirebirdCommit.Enabled := True;
    ALButtonFirebirdCommitRetaining.Enabled := True;
    ALButtonFirebirdRollBack.Enabled := True;
    ALButtonFirebirdRollBackRetaining.Enabled := True;
    ALButtonCloseConnection.Enabled := True;
    Raise;
  End;

end;

{*********************************************************************}
procedure TForm1.ALButtonFirebirdCommitRetainingClick(Sender: TObject);
var aIOStats_1: TALFBXClientMonitoringIOStats;
    aRecordStats_1: TALFBXClientMonitoringRecordStats;
    aIOStats_2: TALFBXClientMonitoringIOStats;
    aRecordStats_2: TALFBXClientMonitoringRecordStats;
    aMemoryUsage: TALFBXClientMonitoringMemoryUsage;
    aTickCount: UInt64;
begin

  FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                  -1,
                                  '',
                                  aIOStats_1,
                                  aRecordStats_1,
                                  aMemoryUsage,
                                  False,
                                  False,
                                  True);
  aTickCount := GetTickCount64;
  fAlFbxClient.TransactionCommitRetaining;
  ALMemoFirebirdStats.Clear;
  ALMemoFirebirdStats.Lines.Add('Time Taken: ' + IntToStr(GetTickCount64 - aTickCount) + ' ms');
  FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                  -1,
                                  '',
                                  aIOStats_2,
                                  aRecordStats_2,
                                  aMemoryUsage);
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('page_reads:   ' + IntToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
  ALMemoFirebirdStats.Lines.Add('page_writes:  ' + IntToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
  ALMemoFirebirdStats.Lines.Add('page_fetches: ' + IntToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
  ALMemoFirebirdStats.Lines.Add('page_marks:   ' + IntToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('record_idx_reads: ' + IntToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
  ALMemoFirebirdStats.Lines.Add('record_seq_reads: ' + IntToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
  ALMemoFirebirdStats.Lines.Add('record_inserts:   ' + IntToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
  ALMemoFirebirdStats.Lines.Add('record_updates:   ' + IntToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
  ALMemoFirebirdStats.Lines.Add('record_deletes:   ' + IntToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
  ALMemoFirebirdStats.Lines.Add('record_backouts:  ' + IntToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
  ALMemoFirebirdStats.Lines.Add('record_purges:    ' + IntToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
  ALMemoFirebirdStats.Lines.Add('record_expunges:  ' + IntToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('memory_used:          ' + IntToStr(aMemoryUsage.memory_used));
  ALMemoFirebirdStats.Lines.Add('memory_allocated:     ' + IntToStr(aMemoryUsage.memory_allocated));
  ALMemoFirebirdStats.Lines.Add('max_memory_used:      ' + IntToStr(aMemoryUsage.max_memory_used));
  ALMemoFirebirdStats.Lines.Add('max_memory_allocated: ' + IntToStr(aMemoryUsage.max_memory_allocated));

  ALMemoFirebirdResult.Lines.Text := 'Commit Transaction Done';

end;

{**************************************************************}
procedure TForm1.ALButtonFirebirdRollBackClick(Sender: TObject);
var aIOStats_1: TALFBXClientMonitoringIOStats;
    aRecordStats_1: TALFBXClientMonitoringRecordStats;
    aIOStats_2: TALFBXClientMonitoringIOStats;
    aRecordStats_2: TALFBXClientMonitoringRecordStats;
    aMemoryUsage: TALFBXClientMonitoringMemoryUsage;
    aTickCount: UInt64;
begin

  ALButtonFirebirdOpenConnection.Enabled := False;
  ALButtonFirebirdStartTransaction.Enabled := True;
  ALButtonFirebirdSelect.Enabled := False;
  ALButtonFirebirdPrepare.Enabled := False;
  ALButtonFirebirdUpdate.Enabled := False;
  ALButtonFirebirdCommit.Enabled := False;
  ALButtonFirebirdCommitRetaining.Enabled := False;
  ALButtonFirebirdRollBack.Enabled := False;
  ALButtonFirebirdRollBackRetaining.Enabled := False;
  ALButtonCloseConnection.Enabled := True;
  Try

    Try
      FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                      -1,
                                      '',
                                      aIOStats_1,
                                      aRecordStats_1,
                                      aMemoryUsage,
                                      False,
                                      False,
                                      True);
    Except
      //hide the error
    End;
    aTickCount := GetTickCount64;
    fAlFbxClient.TransactionRollback;
    ALMemoFirebirdStats.Clear;
    ALMemoFirebirdStats.Lines.Add('Time Taken: ' + IntToStr(GetTickCount64 - aTickCount) + ' ms');
    Try
      FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                      -1,
                                      '',
                                      aIOStats_2,
                                      aRecordStats_2,
                                      aMemoryUsage);
    Except
      //hide the error
    End;
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('page_reads:   ' + IntToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
    ALMemoFirebirdStats.Lines.Add('page_writes:  ' + IntToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
    ALMemoFirebirdStats.Lines.Add('page_fetches: ' + IntToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
    ALMemoFirebirdStats.Lines.Add('page_marks:   ' + IntToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('record_idx_reads: ' + IntToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
    ALMemoFirebirdStats.Lines.Add('record_seq_reads: ' + IntToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
    ALMemoFirebirdStats.Lines.Add('record_inserts:   ' + IntToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
    ALMemoFirebirdStats.Lines.Add('record_updates:   ' + IntToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
    ALMemoFirebirdStats.Lines.Add('record_deletes:   ' + IntToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
    ALMemoFirebirdStats.Lines.Add('record_backouts:  ' + IntToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
    ALMemoFirebirdStats.Lines.Add('record_purges:    ' + IntToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
    ALMemoFirebirdStats.Lines.Add('record_expunges:  ' + IntToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('memory_used:          ' + IntToStr(aMemoryUsage.memory_used));
    ALMemoFirebirdStats.Lines.Add('memory_allocated:     ' + IntToStr(aMemoryUsage.memory_allocated));
    ALMemoFirebirdStats.Lines.Add('max_memory_used:      ' + IntToStr(aMemoryUsage.max_memory_used));
    ALMemoFirebirdStats.Lines.Add('max_memory_allocated: ' + IntToStr(aMemoryUsage.max_memory_allocated));

    ALMemoFirebirdResult.Lines.Text := 'RollBack Transaction Done';

  except
    ALButtonFirebirdOpenConnection.Enabled := False;
    ALButtonFirebirdStartTransaction.Enabled := False;
    ALButtonFirebirdSelect.Enabled := True;
    ALButtonFirebirdPrepare.Enabled := true;
    ALButtonFirebirdUpdate.Enabled := True;
    ALButtonFirebirdCommit.Enabled := True;
    ALButtonFirebirdCommitRetaining.Enabled := True;
    ALButtonFirebirdRollBack.Enabled := True;
    ALButtonFirebirdRollBackRetaining.Enabled := True;
    ALButtonCloseConnection.Enabled := True;
    Raise;
  End;

end;

{***********************************************************************}
procedure TForm1.ALButtonFirebirdRollBackRetainingClick(Sender: TObject);
var aIOStats_1: TALFBXClientMonitoringIOStats;
    aRecordStats_1: TALFBXClientMonitoringRecordStats;
    aIOStats_2: TALFBXClientMonitoringIOStats;
    aRecordStats_2: TALFBXClientMonitoringRecordStats;
    aMemoryUsage: TALFBXClientMonitoringMemoryUsage;
    aTickCount: UInt64;
begin

  FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                  -1,
                                  '',
                                  aIOStats_1,
                                  aRecordStats_1,
                                  aMemoryUsage,
                                  False,
                                  False,
                                  True);
  aTickCount := GetTickCount64;
  fAlFbxClient.TransactionRollbackRetaining;
  ALMemoFirebirdStats.Clear;
  ALMemoFirebirdStats.Lines.Add('Time Taken: ' + IntToStr(GetTickCount64 - aTickCount) + ' ms');
  FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                  -1,
                                  '',
                                  aIOStats_2,
                                  aRecordStats_2,
                                  aMemoryUsage);
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('page_reads:   ' + IntToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
  ALMemoFirebirdStats.Lines.Add('page_writes:  ' + IntToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
  ALMemoFirebirdStats.Lines.Add('page_fetches: ' + IntToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
  ALMemoFirebirdStats.Lines.Add('page_marks:   ' + IntToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('record_idx_reads: ' + IntToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
  ALMemoFirebirdStats.Lines.Add('record_seq_reads: ' + IntToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
  ALMemoFirebirdStats.Lines.Add('record_inserts:   ' + IntToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
  ALMemoFirebirdStats.Lines.Add('record_updates:   ' + IntToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
  ALMemoFirebirdStats.Lines.Add('record_deletes:   ' + IntToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
  ALMemoFirebirdStats.Lines.Add('record_backouts:  ' + IntToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
  ALMemoFirebirdStats.Lines.Add('record_purges:    ' + IntToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
  ALMemoFirebirdStats.Lines.Add('record_expunges:  ' + IntToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('memory_used:          ' + IntToStr(aMemoryUsage.memory_used));
  ALMemoFirebirdStats.Lines.Add('memory_allocated:     ' + IntToStr(aMemoryUsage.memory_allocated));
  ALMemoFirebirdStats.Lines.Add('max_memory_used:      ' + IntToStr(aMemoryUsage.max_memory_used));
  ALMemoFirebirdStats.Lines.Add('max_memory_allocated: ' + IntToStr(aMemoryUsage.max_memory_allocated));

  ALMemoFirebirdResult.Lines.Text := 'RollBack Transaction Done';

end;


{*************************************************************}
procedure TForm1.ALButtonCloseConnectionClick(Sender: TObject);
var aTickCount: UInt64;
begin

  ALButtonFirebirdOpenConnection.Enabled := True;
  ALButtonFirebirdStartTransaction.Enabled := False;
  ALButtonFirebirdSelect.Enabled := False;
  ALButtonFirebirdPrepare.Enabled := False;
  ALButtonFirebirdUpdate.Enabled := False;
  ALButtonFirebirdCommit.Enabled := False;
  ALButtonFirebirdCommitRetaining.Enabled := False;
  ALButtonFirebirdRollBack.Enabled := False;
  ALButtonFirebirdRollBackRetaining.Enabled := False;
  ALButtonCloseConnection.Enabled := False;

  try

    aTickCount := GetTickCount64;
    fAlFbxClient.Disconnect;
    ALMemoFirebirdStats.Clear;
    ALMemoFirebirdStats.Lines.Add('Time Taken: ' + IntToStr(GetTickCount64 - aTickCount) + ' ms');

    ALMemoFirebirdResult.Lines.Text := 'Close Transaction Done';

  finally
    fAlFbxClient.Free;
    fAlFbxClient := nil;
  end;

end;

{*************************************************************}
procedure TForm1.ALEditFirebirdLibButtonClick(Sender: TObject);
begin
  if OpenDialog1.Execute then (Sender as TEdit).Text := OpenDialog1.FileName;
end;

{********************************************************************}
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Try
    if Assigned(fAlFbxClient) then fAlFbxClient.free;
  Except
  End;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
