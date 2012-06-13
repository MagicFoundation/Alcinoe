unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, AlScrollBar, ALMemo, ALButton,
  ALEdit, ALComboBox, OleCtrls, SHDocVw, ComObj, ALFbxClient;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Label8: TLabel;
    Label12: TLabel;
    Panel2: TPanel;
    PanelWebBrowser: TPanel;
    Label2: TLabel;
    Label4: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    ALButtonFirebirdStartTransaction: TALButton;
    ALEditFirebirdLogin: TALEdit;
    ALEditFirebirdPassword: TALEdit;
    ALEditFirebirdCharset: TALEdit;
    ALEditFirebirdLib: TALEdit;
    ALMemoFireBirdQuery: TALMemo;
    ALEditFirebirdDatabase: TALEdit;
    ALButtonFirebirdCommit: TALButton;
    ALButtonFirebirdSelect: TALButton;
    ALButtonFirebirdRollBack: TALButton;
    ALEditFireBirdNum_buffers: TALEdit;
    ALButtonFirebirdCreateDatabase: TALButton;
    ALComboBoxFirebirdapiVer: TALComboBox;
    Label1: TLabel;
    ALMemoFirebirdTPB: TALMemo;
    ALButtonFirebirdUpdate: TALButton;
    ALButtonFirebirdOpenConnection: TALButton;
    ALButtonCloseConnection: TALButton;
    ALButtonFirebirdPrepare: TALButton;
    Label3: TLabel;
    Label5: TLabel;
    Panel3: TPanel;
    ALMemoFireBirdParams: TALMemo;
    ALMemoFirebirdResult: TALMemo;
    ALMemoFirebirdStats: TALMemo;
    Splitter1: TSplitter;
    ALButtonFirebirdCommitRetaining: TALButton;
    ALButtonFirebirdRollBackRetaining: TALButton;
    procedure ALButtonFirebirdUpdatePaint(Sender: TObject; var continue: Boolean);
    procedure Memo_SQLPaint(Sender: TObject; var continue: Boolean);
    procedure Memo_SQLPaintScrollBar(Sender: TObject; var continue: Boolean; Area: TALScrollbarArea);
    procedure ALEdit1Paint(Sender: TObject; var continue: Boolean);
    procedure ComboBox_apiVerPaint(Sender: TObject; var continue: Boolean);
    procedure ALButtonFirebirdUpdateClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
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

uses alFcnSkin,
     AlFbxBase,
     ALWindows,
     AlFbxLib,
     ALfcnString,
     alStringList,
     ALFcnHTML,
     ALXmlDoc;

{$R *.dfm}

{****************************************************************************************************************************}
function SQLFastTagReplaceFunct(const TagString: string; TagParams: TStrings; ExtData: pointer; Var Handled: Boolean): string;
Var aMin, aMax, aIndex: integer;
    aLstSavedData: TstringList;
begin

  Handled := True;
  if sametext(TagString,'randomchar') then begin
    if not trystrtoint(TagParams.Values['Index'], aIndex) then aIndex := -1;
    if aIndex >= 0 then begin
      aLstSavedData := TstringList(ExtData^);
      result := aLstSavedData.Values['randomchar_'+inttostr(aIndex)];
      if result = '' then begin
        result := AlRandomStr(1);
        aLstSavedData.Values['randomchar_'+inttostr(aIndex)] := result;
      end;
    end
    else result := AlRandomStr(1);
  end
  else if sametext(TagString,'randomstring') then begin
    if not trystrtoint(TagParams.Values['MinLength'], aMin) then aMin := 1;
    if not trystrtoint(TagParams.Values['MaxLength'], aMax) then aMax := 255;
    if not trystrtoint(TagParams.Values['Index'], aIndex) then aIndex := -1;
    if aIndex >= 0 then begin
      aLstSavedData := TstringList(ExtData^);
      result := aLstSavedData.Values['randomstring_'+inttostr(aIndex)];
      if result = '' then begin
        result := AlRandomStr(aMin + random(aMax - aMin + 1));
        aLstSavedData.Values['randomstring_'+inttostr(aIndex)] := result;
      end;
    end
    else result := AlRandomStr(aMin + random(aMax - aMin + 1));
  end
  else if sametext(TagString,'randomnumber') then begin
    if not trystrtoint(TagParams.Values['Min'], aMin) then aMin := 1;
    if not trystrtoint(TagParams.Values['Max'], aMax) then aMax := Maxint;
    if not trystrtoint(TagParams.Values['Index'], aIndex) then aIndex := -1;
    if aIndex >= 0 then begin
      aLstSavedData := TstringList(ExtData^);
      result := aLstSavedData.Values['randomnumber_'+inttostr(aIndex)];
      if result = '' then begin
        result := inttostr(aMin + random(aMax - aMin + 1));
        aLstSavedData.Values['randomnumber_'+inttostr(aIndex)] := result;
      end;
    end
    else result := inttostr(aMin + random(aMax - aMin + 1));
  end
  else Handled := False;

end;

{********************************************************************}
procedure TForm1.ALButtonFirebirdCreateDatabaseClick(Sender: TObject);
Var aFBXClient: TalFBXClient;
    aFBAPiVersion: TALFBXVersion_API;
    aTickCount: Int64;
begin

  case ALComboBoxFirebirdapiVer.ItemIndex of
    1: aFBAPiVersion := FB103;
    2: aFBAPiVersion := FB15;
    3: aFBAPiVersion := FB20;
    4: aFBAPiVersion := FB25;
    else aFBAPiVersion := FB102;
  end;

  aFBXClient := TALFbxClient.Create(aFBAPiVersion,ALEditFirebirdLib.Text);
  Try

    aTickCount := ALGetTickCount64;
    aFBXClient.CreateDatabase(AlMemoFireBirdQuery.Lines.Text);
    ALMemoFirebirdStats.Clear;
    ALMemoFirebirdStats.Lines.Add('Time Taken: ' + inttostr(ALGetTickCount64 - aTickCount) + ' ms');

    ALMemoFirebirdResult.Lines.Text := 'Create DataBase Done';

  Finally
    aFBXClient.free;
  End;

end;

{********************************************************************}
procedure TForm1.ALButtonFirebirdOpenConnectionClick(Sender: TObject);
Var aFBAPiVersion: TALFBXVersion_API;
    aTickCount: Int64;
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

      fAlFbxClient := TALFbxClient.Create(aFBAPiVersion,ALEditFirebirdLib.Text);
      Try

        aTickCount := ALGetTickCount64;
        fAlFbxClient.connect(ALEditFireBirdDatabase.Text,
                             ALEditFireBirdLogin.text,
                             ALEditFireBirdPassword.text,
                             ALEditFireBirdCharset.Text,
                             StrtoInt(ALEditFireBirdNum_buffers.Text));
        ALMemoFirebirdStats.Clear;
        ALMemoFirebirdStats.Lines.Add('Time Taken: ' + inttostr(ALGetTickCount64 - aTickCount) + ' ms');

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
Var aTPB: String;
    aIOStats_1: TALFBXClientMonitoringIOStats;
    aRecordStats_1: TALFBXClientMonitoringRecordStats;
    aIOStats_2: TALFBXClientMonitoringIOStats;
    aRecordStats_2: TALFBXClientMonitoringRecordStats;
    aMemoryUsage: TALFBXClientMonitoringMemoryUsage;
    aTickCount: Int64;
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

    aTPB:= trim(ALMemoFireBirdTPB.Lines.Text);
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
    aTickCount := ALGetTickCount64;
    fAlFbxClient.TransactionStart(aTPB);
    ALMemoFirebirdStats.Clear;
    ALMemoFirebirdStats.Lines.Add('Time Taken: ' + inttostr(ALGetTickCount64 - aTickCount) + ' ms');
    FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                    -1,
                                    '',
                                    aIOStats_2,
                                    aRecordStats_2,
                                    aMemoryUsage);
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('page_reads:   ' + intToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
    ALMemoFirebirdStats.Lines.Add('page_writes:  ' + intToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
    ALMemoFirebirdStats.Lines.Add('page_fetches: ' + intToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
    ALMemoFirebirdStats.Lines.Add('page_marks:   ' + intToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('record_idx_reads: ' + intToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
    ALMemoFirebirdStats.Lines.Add('record_seq_reads: ' + intToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
    ALMemoFirebirdStats.Lines.Add('record_inserts:   ' + intToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
    ALMemoFirebirdStats.Lines.Add('record_updates:   ' + intToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
    ALMemoFirebirdStats.Lines.Add('record_deletes:   ' + intToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
    ALMemoFirebirdStats.Lines.Add('record_backouts:  ' + intToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
    ALMemoFirebirdStats.Lines.Add('record_purges:    ' + intToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
    ALMemoFirebirdStats.Lines.Add('record_expunges:  ' + intToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('memory_used:          ' + intToStr(aMemoryUsage.memory_used));
    ALMemoFirebirdStats.Lines.Add('memory_allocated:     ' + intToStr(aMemoryUsage.memory_allocated));
    ALMemoFirebirdStats.Lines.Add('max_memory_used:      ' + intToStr(aMemoryUsage.max_memory_used));
    ALMemoFirebirdStats.Lines.Add('max_memory_allocated: ' + intToStr(aMemoryUsage.max_memory_allocated));

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
    aTickCount: Int64;
    aSQl: string;
    aLst: TStringList;
begin

  aLst := TstringList.create;
  try
    aSQl := ALFastTagReplace(AlMemoFireBirdQuery.Lines.Text,
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
  aTickCount := ALGetTickCount64;
  FAlFBXClient.Prepare(aSQl);
  ALMemoFirebirdStats.Clear;
  ALMemoFirebirdStats.Lines.Add('Time Taken: ' + inttostr(ALGetTickCount64 - aTickCount) + ' ms');
  FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                  -1,
                                  '',
                                  aIOStats_2,
                                  aRecordStats_2,
                                  aMemoryUsage);
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('page_reads:   ' + intToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
  ALMemoFirebirdStats.Lines.Add('page_writes:  ' + intToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
  ALMemoFirebirdStats.Lines.Add('page_fetches: ' + intToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
  ALMemoFirebirdStats.Lines.Add('page_marks:   ' + intToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('record_idx_reads: ' + intToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
  ALMemoFirebirdStats.Lines.Add('record_seq_reads: ' + intToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
  ALMemoFirebirdStats.Lines.Add('record_inserts:   ' + intToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
  ALMemoFirebirdStats.Lines.Add('record_updates:   ' + intToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
  ALMemoFirebirdStats.Lines.Add('record_deletes:   ' + intToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
  ALMemoFirebirdStats.Lines.Add('record_backouts:  ' + intToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
  ALMemoFirebirdStats.Lines.Add('record_purges:    ' + intToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
  ALMemoFirebirdStats.Lines.Add('record_expunges:  ' + intToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('memory_used:          ' + intToStr(aMemoryUsage.memory_used));
  ALMemoFirebirdStats.Lines.Add('memory_allocated:     ' + intToStr(aMemoryUsage.memory_allocated));
  ALMemoFirebirdStats.Lines.Add('max_memory_used:      ' + intToStr(aMemoryUsage.max_memory_used));
  ALMemoFirebirdStats.Lines.Add('max_memory_allocated: ' + intToStr(aMemoryUsage.max_memory_allocated));

  ALMemoFirebirdResult.Lines.Text := 'Prepare Done';

end;

{************************************************************}
procedure TForm1.ALButtonFirebirdSelectClick(Sender: TObject);
var aXMLDATA: TalXmlDocument;
    aFormatSettings: TformatSettings;
    aSQL: TALFBXClientSelectDataSQL;
    aIOStats_1: TALFBXClientMonitoringIOStats;
    aRecordStats_1: TALFBXClientMonitoringRecordStats;
    aIOStats_2: TALFBXClientMonitoringIOStats;
    aRecordStats_2: TALFBXClientMonitoringRecordStats;
    aMemoryUsage: TALFBXClientMonitoringMemoryUsage;
    aTickCount: Int64;
    aLst1: TStringList;
    i: integer;
begin

  GetLocaleFormatSettings(1033, aFormatSettings);
  aXMLDATA := ALCreateEmptyXMLDocument('root');
  Try

    With aXMLDATA Do Begin
      Options := [doNodeAutoIndent];
      ParseOptions := [poPreserveWhiteSpace];
    end;

    aLst1 := TstringList.create;
    try
      aSQL.SQL := ALFastTagReplace(AlMemoFireBirdQuery.Lines.Text,
                                   '<#',
                                   '>',
                                   SQLFastTagReplaceFunct,
                                   True,
                                   @aLst1)
    finally
      aLst1.free;
    end;

    if ALMemoFireBirdParams.Lines.Count > 0 then begin
      Setlength(aSQL.Params, 1);
      Setlength(aSQL.Params[0].fields, ALMemoFireBirdParams.Lines.Count);
      for I := 0 to ALMemoFireBirdParams.Lines.Count - 1 do begin
        aLst1 := TstringList.create;
        try
          aSQL.Params[0].fields[i].Value := ALFastTagReplace(ALMemoFireBirdParams.Lines[i],
                                                             '<#',
                                                             '>',
                                                             SQLFastTagReplaceFunct,
                                                             True,
                                                             @aLst1)
        finally
          aLst1.free;
        end;
        aSQL.Params[0].fields[i].isnull := False;
        aSQL.Params[0].fields[i].isblob := False;
      end;
    end
    else Setlength(aSQL.Params, 0);
    aSQL.RowTag := 'rec';
    aSQL.ViewTag := '';
    aSQL.Skip := 0;
    aSQL.First := 200;

    FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                    -1,
                                    '',
                                    aIOStats_1,
                                    aRecordStats_1,
                                    aMemoryUsage,
                                    False,
                                    False,
                                    True);
    aTickCount := ALGetTickCount64;
    FAlFBXClient.SelectData(aSQL,
                            aXMLDATA.DocumentElement,
                            aFormatSettings);
    ALMemoFirebirdStats.Clear;
    ALMemoFirebirdStats.Lines.Add('Time Taken: ' + inttostr(ALGetTickCount64 - aTickCount) + ' ms');
    FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                    -1,
                                    '',
                                    aIOStats_2,
                                    aRecordStats_2,
                                    aMemoryUsage);
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('page_reads:   ' + intToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
    ALMemoFirebirdStats.Lines.Add('page_writes:  ' + intToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
    ALMemoFirebirdStats.Lines.Add('page_fetches: ' + intToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
    ALMemoFirebirdStats.Lines.Add('page_marks:   ' + intToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('record_idx_reads: ' + intToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
    ALMemoFirebirdStats.Lines.Add('record_seq_reads: ' + intToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
    ALMemoFirebirdStats.Lines.Add('record_inserts:   ' + intToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
    ALMemoFirebirdStats.Lines.Add('record_updates:   ' + intToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
    ALMemoFirebirdStats.Lines.Add('record_deletes:   ' + intToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
    ALMemoFirebirdStats.Lines.Add('record_backouts:  ' + intToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
    ALMemoFirebirdStats.Lines.Add('record_purges:    ' + intToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
    ALMemoFirebirdStats.Lines.Add('record_expunges:  ' + intToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('memory_used:          ' + intToStr(aMemoryUsage.memory_used));
    ALMemoFirebirdStats.Lines.Add('memory_allocated:     ' + intToStr(aMemoryUsage.memory_allocated));
    ALMemoFirebirdStats.Lines.Add('max_memory_used:      ' + intToStr(aMemoryUsage.max_memory_used));
    ALMemoFirebirdStats.Lines.Add('max_memory_allocated: ' + intToStr(aMemoryUsage.max_memory_allocated));

    ALMemoFirebirdResult.Lines.Text := aXMLDATA.XML.Text;

  Finally
    aXMLDATA.free;
  End;

end;

{************************************************************}
procedure TForm1.ALButtonFirebirdUpdateClick(Sender: TObject);
var aSQL: TALFBXClientUpdateDataSQL;
    aIOStats_1: TALFBXClientMonitoringIOStats;
    aRecordStats_1: TALFBXClientMonitoringRecordStats;
    aIOStats_2: TALFBXClientMonitoringIOStats;
    aRecordStats_2: TALFBXClientMonitoringRecordStats;
    aMemoryUsage: TALFBXClientMonitoringMemoryUsage;
    aTickCount: Int64;
    aLst1: TStringList;
    i: integer;
begin

  aLst1 := TstringList.create;
  try
    aSQL.SQL := ALFastTagReplace(AlMemoFireBirdQuery.Lines.Text,
                                 '<#',
                                 '>',
                                 SQLFastTagReplaceFunct,
                                 True,
                                 @aLst1)
  finally
    aLst1.free;
  end;

  if ALMemoFireBirdParams.Lines.Count > 0 then begin
    Setlength(aSQL.Params, 1);
    Setlength(aSQL.Params[0].fields, ALMemoFireBirdParams.Lines.Count);
    for I := 0 to ALMemoFireBirdParams.Lines.Count - 1 do begin
      aLst1 := TstringList.create;
      try
        aSQL.Params[0].fields[i].Value := ALFastTagReplace(ALMemoFireBirdParams.Lines[i],
                                                           '<#',
                                                           '>',
                                                           SQLFastTagReplaceFunct,
                                                           True,
                                                           @aLst1)
      finally
        aLst1.free;
      end;
      aSQL.Params[0].fields[i].isnull := False;
      aSQL.Params[0].fields[i].isblob := False;
    end;
  end
  else Setlength(aSQL.Params, 0);
  FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                  -1,
                                  '',
                                  aIOStats_1,
                                  aRecordStats_1,
                                  aMemoryUsage,
                                  False,
                                  False,
                                  True);
  aTickCount := ALGetTickCount64;
  FAlFBXClient.UpdateData(aSQL);
  ALMemoFirebirdStats.Clear;
  ALMemoFirebirdStats.Lines.Add('Time Taken: ' + inttostr(ALGetTickCount64 - aTickCount) + ' ms');
  FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                  -1,
                                  '',
                                  aIOStats_2,
                                  aRecordStats_2,
                                  aMemoryUsage);
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('page_reads:   ' + intToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
  ALMemoFirebirdStats.Lines.Add('page_writes:  ' + intToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
  ALMemoFirebirdStats.Lines.Add('page_fetches: ' + intToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
  ALMemoFirebirdStats.Lines.Add('page_marks:   ' + intToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('record_idx_reads: ' + intToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
  ALMemoFirebirdStats.Lines.Add('record_seq_reads: ' + intToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
  ALMemoFirebirdStats.Lines.Add('record_inserts:   ' + intToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
  ALMemoFirebirdStats.Lines.Add('record_updates:   ' + intToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
  ALMemoFirebirdStats.Lines.Add('record_deletes:   ' + intToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
  ALMemoFirebirdStats.Lines.Add('record_backouts:  ' + intToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
  ALMemoFirebirdStats.Lines.Add('record_purges:    ' + intToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
  ALMemoFirebirdStats.Lines.Add('record_expunges:  ' + intToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('memory_used:          ' + intToStr(aMemoryUsage.memory_used));
  ALMemoFirebirdStats.Lines.Add('memory_allocated:     ' + intToStr(aMemoryUsage.memory_allocated));
  ALMemoFirebirdStats.Lines.Add('max_memory_used:      ' + intToStr(aMemoryUsage.max_memory_used));
  ALMemoFirebirdStats.Lines.Add('max_memory_allocated: ' + intToStr(aMemoryUsage.max_memory_allocated));

  ALMemoFirebirdResult.Lines.Text := 'Update Done';

end;

{************************************************************}
procedure TForm1.ALButtonFirebirdCommitClick(Sender: TObject);
var aIOStats_1: TALFBXClientMonitoringIOStats;
    aRecordStats_1: TALFBXClientMonitoringRecordStats;
    aIOStats_2: TALFBXClientMonitoringIOStats;
    aRecordStats_2: TALFBXClientMonitoringRecordStats;
    aMemoryUsage: TALFBXClientMonitoringMemoryUsage;
    aTickCount: Int64;
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
    aTickCount := ALGetTickCount64;
    fAlFbxClient.TransactionCommit;
    ALMemoFirebirdStats.Clear;
    ALMemoFirebirdStats.Lines.Add('Time Taken: ' + inttostr(ALGetTickCount64 - aTickCount) + ' ms');
    FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                    -1,
                                    '',
                                    aIOStats_2,
                                    aRecordStats_2,
                                    aMemoryUsage);
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('page_reads:   ' + intToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
    ALMemoFirebirdStats.Lines.Add('page_writes:  ' + intToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
    ALMemoFirebirdStats.Lines.Add('page_fetches: ' + intToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
    ALMemoFirebirdStats.Lines.Add('page_marks:   ' + intToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('record_idx_reads: ' + intToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
    ALMemoFirebirdStats.Lines.Add('record_seq_reads: ' + intToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
    ALMemoFirebirdStats.Lines.Add('record_inserts:   ' + intToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
    ALMemoFirebirdStats.Lines.Add('record_updates:   ' + intToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
    ALMemoFirebirdStats.Lines.Add('record_deletes:   ' + intToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
    ALMemoFirebirdStats.Lines.Add('record_backouts:  ' + intToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
    ALMemoFirebirdStats.Lines.Add('record_purges:    ' + intToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
    ALMemoFirebirdStats.Lines.Add('record_expunges:  ' + intToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('memory_used:          ' + intToStr(aMemoryUsage.memory_used));
    ALMemoFirebirdStats.Lines.Add('memory_allocated:     ' + intToStr(aMemoryUsage.memory_allocated));
    ALMemoFirebirdStats.Lines.Add('max_memory_used:      ' + intToStr(aMemoryUsage.max_memory_used));
    ALMemoFirebirdStats.Lines.Add('max_memory_allocated: ' + intToStr(aMemoryUsage.max_memory_allocated));

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
    aTickCount: Int64;
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
  aTickCount := ALGetTickCount64;
  fAlFbxClient.TransactionCommitRetaining;
  ALMemoFirebirdStats.Clear;
  ALMemoFirebirdStats.Lines.Add('Time Taken: ' + inttostr(ALGetTickCount64 - aTickCount) + ' ms');
  FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                  -1,
                                  '',
                                  aIOStats_2,
                                  aRecordStats_2,
                                  aMemoryUsage);
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('page_reads:   ' + intToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
  ALMemoFirebirdStats.Lines.Add('page_writes:  ' + intToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
  ALMemoFirebirdStats.Lines.Add('page_fetches: ' + intToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
  ALMemoFirebirdStats.Lines.Add('page_marks:   ' + intToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('record_idx_reads: ' + intToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
  ALMemoFirebirdStats.Lines.Add('record_seq_reads: ' + intToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
  ALMemoFirebirdStats.Lines.Add('record_inserts:   ' + intToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
  ALMemoFirebirdStats.Lines.Add('record_updates:   ' + intToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
  ALMemoFirebirdStats.Lines.Add('record_deletes:   ' + intToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
  ALMemoFirebirdStats.Lines.Add('record_backouts:  ' + intToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
  ALMemoFirebirdStats.Lines.Add('record_purges:    ' + intToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
  ALMemoFirebirdStats.Lines.Add('record_expunges:  ' + intToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('memory_used:          ' + intToStr(aMemoryUsage.memory_used));
  ALMemoFirebirdStats.Lines.Add('memory_allocated:     ' + intToStr(aMemoryUsage.memory_allocated));
  ALMemoFirebirdStats.Lines.Add('max_memory_used:      ' + intToStr(aMemoryUsage.max_memory_used));
  ALMemoFirebirdStats.Lines.Add('max_memory_allocated: ' + intToStr(aMemoryUsage.max_memory_allocated));

  ALMemoFirebirdResult.Lines.Text := 'Commit Transaction Done';

end;

{**************************************************************}
procedure TForm1.ALButtonFirebirdRollBackClick(Sender: TObject);
var aIOStats_1: TALFBXClientMonitoringIOStats;
    aRecordStats_1: TALFBXClientMonitoringRecordStats;
    aIOStats_2: TALFBXClientMonitoringIOStats;
    aRecordStats_2: TALFBXClientMonitoringRecordStats;
    aMemoryUsage: TALFBXClientMonitoringMemoryUsage;
    aTickCount: Int64;
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
    aTickCount := ALGetTickCount64;
    fAlFbxClient.TransactionRollback;
    ALMemoFirebirdStats.Clear;
    ALMemoFirebirdStats.Lines.Add('Time Taken: ' + inttostr(ALGetTickCount64 - aTickCount) + ' ms');
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
    ALMemoFirebirdStats.Lines.Add('page_reads:   ' + intToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
    ALMemoFirebirdStats.Lines.Add('page_writes:  ' + intToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
    ALMemoFirebirdStats.Lines.Add('page_fetches: ' + intToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
    ALMemoFirebirdStats.Lines.Add('page_marks:   ' + intToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('record_idx_reads: ' + intToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
    ALMemoFirebirdStats.Lines.Add('record_seq_reads: ' + intToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
    ALMemoFirebirdStats.Lines.Add('record_inserts:   ' + intToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
    ALMemoFirebirdStats.Lines.Add('record_updates:   ' + intToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
    ALMemoFirebirdStats.Lines.Add('record_deletes:   ' + intToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
    ALMemoFirebirdStats.Lines.Add('record_backouts:  ' + intToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
    ALMemoFirebirdStats.Lines.Add('record_purges:    ' + intToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
    ALMemoFirebirdStats.Lines.Add('record_expunges:  ' + intToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
    ALMemoFirebirdStats.Lines.Add('');
    ALMemoFirebirdStats.Lines.Add('memory_used:          ' + intToStr(aMemoryUsage.memory_used));
    ALMemoFirebirdStats.Lines.Add('memory_allocated:     ' + intToStr(aMemoryUsage.memory_allocated));
    ALMemoFirebirdStats.Lines.Add('max_memory_used:      ' + intToStr(aMemoryUsage.max_memory_used));
    ALMemoFirebirdStats.Lines.Add('max_memory_allocated: ' + intToStr(aMemoryUsage.max_memory_allocated));

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
    aTickCount: Int64;
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
  aTickCount := ALGetTickCount64;
  fAlFbxClient.TransactionRollbackRetaining;
  ALMemoFirebirdStats.Clear;
  ALMemoFirebirdStats.Lines.Add('Time Taken: ' + inttostr(ALGetTickCount64 - aTickCount) + ' ms');
  FAlFBXClient.GetMonitoringInfos(FAlFBXClient.ConnectionID,
                                  -1,
                                  '',
                                  aIOStats_2,
                                  aRecordStats_2,
                                  aMemoryUsage);
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('page_reads:   ' + intToStr(aIOStats_2.page_reads   - aIOStats_1.page_reads));
  ALMemoFirebirdStats.Lines.Add('page_writes:  ' + intToStr(aIOStats_2.page_writes  - aIOStats_1.page_writes));
  ALMemoFirebirdStats.Lines.Add('page_fetches: ' + intToStr(aIOStats_2.page_fetches - aIOStats_1.page_fetches));
  ALMemoFirebirdStats.Lines.Add('page_marks:   ' + intToStr(aIOStats_2.page_marks   - aIOStats_1.page_marks));
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('record_idx_reads: ' + intToStr(aRecordStats_2.record_idx_reads - aRecordStats_1.record_idx_reads));
  ALMemoFirebirdStats.Lines.Add('record_seq_reads: ' + intToStr(aRecordStats_2.record_seq_reads - aRecordStats_1.record_seq_reads));
  ALMemoFirebirdStats.Lines.Add('record_inserts:   ' + intToStr(aRecordStats_2.record_inserts   - aRecordStats_1.record_inserts));
  ALMemoFirebirdStats.Lines.Add('record_updates:   ' + intToStr(aRecordStats_2.record_updates   - aRecordStats_1.record_updates));
  ALMemoFirebirdStats.Lines.Add('record_deletes:   ' + intToStr(aRecordStats_2.record_deletes   - aRecordStats_1.record_deletes));
  ALMemoFirebirdStats.Lines.Add('record_backouts:  ' + intToStr(aRecordStats_2.record_backouts  - aRecordStats_1.record_backouts));
  ALMemoFirebirdStats.Lines.Add('record_purges:    ' + intToStr(aRecordStats_2.record_purges    - aRecordStats_1.record_purges));
  ALMemoFirebirdStats.Lines.Add('record_expunges:  ' + intToStr(aRecordStats_2.record_expunges  - aRecordStats_1.record_expunges));
  ALMemoFirebirdStats.Lines.Add('');
  ALMemoFirebirdStats.Lines.Add('memory_used:          ' + intToStr(aMemoryUsage.memory_used));
  ALMemoFirebirdStats.Lines.Add('memory_allocated:     ' + intToStr(aMemoryUsage.memory_allocated));
  ALMemoFirebirdStats.Lines.Add('max_memory_used:      ' + intToStr(aMemoryUsage.max_memory_used));
  ALMemoFirebirdStats.Lines.Add('max_memory_allocated: ' + intToStr(aMemoryUsage.max_memory_allocated));

  ALMemoFirebirdResult.Lines.Text := 'RollBack Transaction Done';

end;


{*************************************************************}
procedure TForm1.ALButtonCloseConnectionClick(Sender: TObject);
var aTickCount: Int64;
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

    aTickCount := ALGetTickCount64;
    fAlFbxClient.Disconnect;
    ALMemoFirebirdStats.Clear;
    ALMemoFirebirdStats.Lines.Add('Time Taken: ' + inttostr(ALGetTickCount64 - aTickCount) + ' ms');

    ALMemoFirebirdResult.Lines.Text := 'Close Transaction Done';

  finally
    fAlFbxClient.Free;
    fAlFbxClient := nil;
  end;

end;

{***********************************************************************************}
procedure TForm1.ALButtonFirebirdUpdatePaint(Sender: TObject; var continue: Boolean);
begin
  PaintAlButtonBlueSkin(Sender, Continue);
end;

{********************************************************************}
procedure TForm1.Memo_SQLPaint(Sender: TObject; var continue: Boolean);
begin
  paintAlMemoBlueSkin(sender, Continue);
end;

{*****************************************************************************************************}
procedure TForm1.Memo_SQLPaintScrollBar(Sender: TObject; var continue: Boolean; Area: TALScrollbarArea);
begin
  paintAlMemoScrollBarBlueSkin(sender, Continue, area);
end;

{********************************************************************}
procedure TForm1.ALEdit1Paint(Sender: TObject; var continue: Boolean);
begin
  PaintAlEditBlueSkin(Sender, Continue);
end;

{*************************************************************}
procedure TForm1.ALEditFirebirdLibButtonClick(Sender: TObject);
begin
  if OpenDialog1.Execute then (Sender as TalEdit).Text := OpenDialog1.FileName;
end;

{****************************************************************************}
procedure TForm1.ComboBox_apiVerPaint(Sender: TObject; var continue: Boolean);
begin
  PaintAlComboBoxBlueSkin(Sender, Continue);
end;

{-------------------}
var ie: IWebBrowser2;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
var Url, Flags, TargetFrameName, PostData, Headers: OleVariant;
begin
  ie := CreateOleObject('InternetExplorer.Application') as IWebBrowser2;
  SetWindowLong(ie.hwnd, GWL_STYLE, GetWindowLong(ie.hwnd, GWL_STYLE) and not WS_BORDER and not WS_SIZEBOX and not WS_DLGFRAME );
  SetWindowPos(ie.hwnd, HWND_TOP, Left, Top, Width, Height, SWP_FRAMECHANGED);
  windows.setparent(ie.hwnd, PanelWebBrowser.handle);
  ie.Left := maxint; // don't understand why it's look impossible to setup the position
  ie.Top  := maxint; // don't understand why it's look impossible to setup the position
  ie.Width := 100;
  ie.Height := 300;
  ie.MenuBar := false;
  ie.AddressBar := false;
  ie.Resizable := false;
  ie.StatusBar := false;
  ie.ToolBar := 0;
  Url := 'http://www.arkadia.com/html/alcinoe_like.html';
  ie.Navigate2(Url,Flags,TargetFrameName,PostData,Headers);
  ie.Visible := true;
end;

{********************************************************************}
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    ie.quit;
  except
  end;
  Try
    if Assigned(fAlFbxClient) then fAlFbxClient.free;
  Except
  End;
  sleep(500);
end;

{$IFDEF DEBUG}
initialization
  ReportMemoryleaksOnSHutdown := True;
{$ENDIF}

end.
