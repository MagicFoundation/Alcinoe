unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, AlScrollBar, ALMemo, ALButton,
  ALComboBox, ALEdit;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Label5: TLabel;
    ALButtonSelectFirebird: TALButton;
    Label3: TLabel;
    ALEditMySqlHostName: TALEdit;
    Label6: TLabel;
    ALEditMySqlLogin: TALEdit;
    Label7: TLabel;
    ALEditMySqlPassword: TALEdit;
    ALEditMySqlPortNumber: TALEdit;
    Label8: TLabel;
    ALEditMySqlCharset: TALEdit;
    Label10: TLabel;
    ALEditMysqlLybMsqldll: TALEdit;
    Label11: TLabel;
    ALMemoMySqlQuery: TALMemo;
    Label9: TLabel;
    ALButtonMySQLExecute: TALButton;
    OpenDialog1: TOpenDialog;
    Bevel2: TBevel;
    Label12: TLabel;
    ALEditMySqlDatabase: TALEdit;
    ALMemoResult: TALMemo;
    Label13: TLabel;
    Label2: TLabel;
    ALEditFirebirdLogin: TALEdit;
    Label4: TLabel;
    ALEditFirebirdPassword: TALEdit;
    ALEditFirebirdCharset: TALEdit;
    Label15: TLabel;
    ALEditFirebirdFBClientDLL: TALEdit;
    Label16: TLabel;
    ALMemoFireBirdQuery: TALMemo;
    Label17: TLabel;
    Label18: TLabel;
    ALEditFirebirdDatabase: TALEdit;
    Label1: TLabel;
    Label14: TLabel;
    ALButtonLoopSelectFirebird: TALButton;
    ALButtonUpdateFirebird: TALButton;
    ALButtonLoopUpdateFirebird: TALButton;
    procedure ALButtonPaint(Sender: TObject; var continue: Boolean);
    procedure FormClick(Sender: TObject);
    procedure ALButtonMySqlClick(Sender: TObject);
    procedure ALEditButtonFindFileClick(Sender: TObject);
    procedure ALEditPaint(Sender: TObject; var continue: Boolean);
    procedure ALMemoPaint(Sender: TObject; var continue: Boolean);
    procedure ALButtonSelectFirebirdClick(Sender: TObject);
    procedure ALButtonLoopSelectFirebirdClick(Sender: TObject);
    procedure ALButtonUpdateFirebirdClick(Sender: TObject);
    procedure ALButtonLoopUpdateFirebirdClick(Sender: TObject);
  private
  public
  end;

var Form1: TForm1;

implementation

uses alFcnSkin,
     alFbxClient,
     ALFBXBase,
     almysqlClient,
     ALMySqlWrapper,
     AlXmlDoc,
     alFcnString;

{$R *.dfm}

{************************************************************}
procedure TForm1.ALButtonSelectFirebirdClick(Sender: TObject);
Var aFBXClient: TALFbxClient;
    aXMLDATA1: TalXmlDocument;
    aXMLDATA2: TalXmlDocument;
    aStartDate: Cardinal;
    aFormatSettings: TFormatSettings;
begin
  GetLocaleFormatSettings(1033, aFormatSettings);
  AlMemoResult.Lines.text := 'Loading...';
  Screen.Cursor := CrHourGlass;
  try

    aFBXClient := TALFbxClient.Create(FB20,ALEditFirebirdFBClientdll.Text);
    aFBXClient.connect(
                       ALEditFireBirdDatabase.Text,
                       ALEditFireBirdLogin.text,
                       ALEditFireBirdPassword.text,
                       ALEditFireBirdCharset.Text
                      );
    Try

      aXMLDATA1 := ALCreateEmptyXMLDocument('root');
      aXMLDATA2 := ALCreateEmptyXMLDocument('root');
      Try
        With aXMLDATA1 Do Begin
          Options := [doNodeAutoIndent];
          ParseOptions := [poPreserveWhiteSpace];
        end;

        aStartDate := GetTickCount;
        aFBXClient.TransactionStart(True);
        try
          aFBXClient.SelectData(
                                AlMemoFirebirdQuery.Lines.Text,
                                'rec',
                                aXMLDATA1.DocumentElement,
                                aFormatSettings
                               );

          AlMemoResult.Lines.Text := 'Time to execute the sql: ' + inttostr(GetTickCount - aStartDate) + ' ms';
          aFBXClient.SelectData(
                                'SELECT '+
                                  'MON$RECORD_IDX_READS as IDX_READS, '+
                                  'MON$RECORD_SEQ_READS as SEQ_READS '+
                                'FROM '+
                                  'MON$RECORD_STATS '+
                                'JOIN MON$TRANSACTIONS ON MON$TRANSACTIONS.MON$STAT_ID=MON$RECORD_STATS.MON$STAT_ID '+
                                'WHERE '+
                                  'MON$TRANSACTIONS.MON$TRANSACTION_ID=current_transaction',
                                aXMLDATA2.DocumentElement,
                                aFormatSettings
                               );
          AlMemoResult.Lines.add('Indexed Read: ' + aXMLDATA2.DocumentElement.ChildNodes[0].ChildNodes['idx_reads'].Text);
          AlMemoResult.Lines.add('Non Indexed Read: ' + aXMLDATA2.DocumentElement.ChildNodes[0].ChildNodes['seq_reads'].Text);
        finally
          aFBXClient.TransactionCommit;
        end;
        AlMemoResult.Lines.add('');
        AlMemoResult.Lines.add('');
        AlMemoResult.Lines.add('**************');
        AlMemoResult.Lines.add('');

        if aXMLDATA1.DocumentElement.ChildNodes.Count <= 1000 then AlMemoResult.Lines.Text := AlMemoResult.Lines.Text + trim(aXMLDATA1.XML.Text)
        else AlMemoResult.Lines.Add('More than 1000 records returned!');

      Finally
        aXMLDATA1.free;
        aXMLDATA2.free;
      End;

    Finally
      aFBXClient.disconnect;
      aFBXClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{****************************************************************}
procedure TForm1.ALButtonLoopSelectFirebirdClick(Sender: TObject);
Var aFBXClient: TALFbxClient;
    aXMLDATA: TalXmlDocument;
    aStartDate: Cardinal;
    aFormatSettings: TFormatSettings;
    Count: integer;
begin
  GetLocaleFormatSettings(1033, aFormatSettings);
  if ALButtonLoopSelectFirebird.Tag = 1 then begin
    ALButtonLoopSelectFirebird.Tag := 0;
    ALButtonLoopSelectFirebird.Caption := 'Loop SELECT via FireBird';
    exit;
  end
  else begin
    ALButtonLoopSelectFirebird.Tag := 1;
    ALButtonLoopSelectFirebird.Caption := 'STOP';
  end;

  Count := 0;
  AlMemoResult.Lines.Clear;
  aFBXClient := TALFbxClient.Create(FB20,ALEditFirebirdFBClientdll.Text);
  aFBXClient.connect(
                     ALEditFireBirdDatabase.Text,
                     ALEditFireBirdLogin.text,
                     ALEditFireBirdPassword.text,
                     ALEditFireBirdCharset.Text
                    );
  Try

    while ALButtonLoopSelectFirebird.Tag = 1 do begin

      aXMLDATA := ALCreateEmptyXMLDocument('root');
      Try
        With aXMLDATA Do Begin
          Options := [doNodeAutoIndent];
          ParseOptions := [poPreserveWhiteSpace];
        end;

        aStartDate := GetTickCount;
        aFBXClient.TransactionStart(True);
        try
          aFBXClient.SelectData(
                                AlMemoFirebirdQuery.Lines.Text,
                                'rec',
                                aXMLDATA.DocumentElement,
                                aFormatSettings
                               );
        finally
          aFBXClient.TransactionCommit;
        end;
        inc(Count);
        if count mod 100 = 0 then AlMemoResult.Clear;
        AlMemoResult.Lines.Insert(0,inttostr(Count) + '. Time to load the data: ' + inttostr(GetTickCount - aStartDate) + ' ms');
        application.ProcessMessages;

      Finally
        aXMLDATA.free;
      End;

    end;

  Finally
    aFBXClient.disconnect;
    aFBXClient.free;
  End;

end;

{************************************************************}
procedure TForm1.ALButtonUpdateFirebirdClick(Sender: TObject);
Var aFBXClient: TALFbxClient;
    aXMLDATA: TalXmlDocument;
    aStartDate: Cardinal;
    aFormatSettings: TformatSettings;
    S1: String;
begin
  GetLocaleFormatSettings(1033, aFormatSettings);
  AlMemoResult.Lines.text := 'Loading...';
  application.processmessages;
  Screen.Cursor := CrHourGlass;
  try

    aFBXClient := TALFbxClient.Create(FB20,ALEditFirebirdFBClientdll.Text);
    aFBXClient.connect(
                       ALEditFireBirdDatabase.Text,
                       ALEditFireBirdLogin.text,
                       ALEditFireBirdPassword.text,
                       ALEditFireBirdCharset.Text
                      );
    Try

      aXMLDATA:= ALCreateEmptyXMLDocument('root');
      Try

        S1 := AlMemoFirebirdQuery.Lines.Text;
        while AlPos('<#randomchar>', AlLowerCase(S1)) > 0 do
          S1 := AlStringReplace(S1, '<#randomchar>',AlRandomStr(1),[rfIgnoreCase]);
        while AlPos('<#randomnumber>', AlLowerCase(S1)) > 0 do
          S1 := AlStringReplace(S1, '<#randomnumber>',inttostr(random(10)),[rfIgnoreCase]);
        aStartDate := GetTickCount;
        aFBXClient.TransactionStart(False);
        try
          aFBXClient.UpdateData(S1, []);
          AlMemoResult.Lines.Text := 'Time to execute the sql: ' + inttostr(GetTickCount - aStartDate) + ' ms';
          aFBXClient.SelectData(
                                'SELECT '+
                                  'MON$RECORD_IDX_READS as IDX_READS, '+
                                  'MON$RECORD_SEQ_READS as SEQ_READS, '+
                                  'MON$RECORD_INSERTS as INSERTS, '+
                                  'MON$RECORD_UPDATES as UPDATES, '+
                                  'MON$RECORD_DELETES as DELETES '+
                                'FROM '+
                                  'MON$RECORD_STATS '+
                                'JOIN MON$TRANSACTIONS ON MON$TRANSACTIONS.MON$STAT_ID=MON$RECORD_STATS.MON$STAT_ID '+
                                'WHERE '+
                                  'MON$TRANSACTIONS.MON$TRANSACTION_ID=current_transaction',
                                aXMLDATA.DocumentElement,
                                aFormatSettings
                               );
          AlMemoResult.Lines.add('Indexed Read: ' + aXMLDATA.DocumentElement.ChildNodes[0].ChildNodes['idx_reads'].Text);
          AlMemoResult.Lines.add('Non Indexed Read: ' + aXMLDATA.DocumentElement.ChildNodes[0].ChildNodes['seq_reads'].Text);
          AlMemoResult.Lines.add('Inserts: ' + aXMLDATA.DocumentElement.ChildNodes[0].ChildNodes['inserts'].Text);
          AlMemoResult.Lines.add('Updates: ' + aXMLDATA.DocumentElement.ChildNodes[0].ChildNodes['updates'].Text);
          AlMemoResult.Lines.add('Deletes: ' + aXMLDATA.DocumentElement.ChildNodes[0].ChildNodes['deletes'].Text);
          aStartDate := GetTickCount;
        finally
          aFBXClient.TransactionCommit;
        end;
        AlMemoResult.Lines.add('Time to commit the data: ' + inttostr(GetTickCount - aStartDate) + ' ms');

      Finally
        aXMLDATA.free;
      End;

    Finally
      aFBXClient.disconnect;
      aFBXClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{****************************************************************}
procedure TForm1.ALButtonLoopUpdateFirebirdClick(Sender: TObject);
Var aFBXClient: TALFbxClient;
    aStartDate: Cardinal;
    aEndDate: Cardinal;
    Count: integer;
    TotalExecuteTime: Cardinal;
    S1: String;
begin
  if ALButtonLoopUpdateFirebird.Tag = 1 then begin
    ALButtonLoopUpdateFirebird.Tag := 0;
    ALButtonLoopUpdateFirebird.Caption := 'Loop UPDATE via FireBird';
    exit;
  end
  else begin
    ALButtonLoopUpdateFirebird.Tag := 1;
    ALButtonLoopUpdateFirebird.Caption := 'STOP';
  end;

  Count := 0;
  AlMemoResult.Lines.Clear;  
  aFBXClient := TALFbxClient.Create(FB20,ALEditFirebirdFBClientdll.Text);
  aFBXClient.connect(
                     ALEditFireBirdDatabase.Text,
                     ALEditFireBirdLogin.text,
                     ALEditFireBirdPassword.text,
                     ALEditFireBirdCharset.Text
                    );
  Try

    TotalExecuteTime := 0;
    AlMemoResult.Lines.Text := 'Average time to update the data: 0 ms';
    while ALButtonLoopUpdateFirebird.Tag = 1 do begin

      S1 := AlMemoFirebirdQuery.Lines.Text;
      while AlPos('<#randomchar>', AlLowerCase(S1)) > 0 do
        S1 := AlStringReplace(S1, '<#randomchar>',AlRandomStr(1),[rfIgnoreCase]);
      while AlPos('<#randomnumber>', AlLowerCase(S1)) > 0 do
        S1 := AlStringReplace(S1, '<#randomnumber>',inttostr(random(10)),[rfIgnoreCase]);
      aStartDate := GetTickCount;
      aFBXClient.TransactionStart(False);
      try
        aFBXClient.UpdateData(S1, []);
      finally
        aFBXClient.TransactionCommit;
      end;
      aEndDate := GetTickCount;
      inc(Count);
      if count mod 100 = 0 then AlMemoResult.Lines.Text := AlMemoResult.Lines[0];
      AlMemoResult.Lines.Insert(1,inttostr(Count) + '. Time to update the data: ' + inttostr(aEndDate - aStartDate) + ' ms');
      TotalExecuteTime := TotalExecuteTime + (aEndDate - aStartDate);
      AlMemoResult.Lines[0] := 'Average time to update the data: '+inttostr(round(TotalExecuteTime/Count))+' ms';
      application.ProcessMessages;

    end;

  Finally
    aFBXClient.disconnect;
    aFBXClient.free;
  End;

end;

{***************************************************}
procedure TForm1.ALButtonMySqlClick(Sender: TObject);
Var aMysqlClient: TalmysqlClient;
    aXMLDATA: TalXmlDocument;
    aStartDate: Cardinal;
begin
  AlMemoResult.Lines.text := 'Loading...';
  Screen.Cursor := CrHourGlass;
  try
  
    aMysqlClient := TalMySqlClient.Create(MYSQL5,ALEditMysqlLybMsqldll.Text);
    aMysqlClient.connect(
                         ALEditMySqlHostName.Text,
                         ALEditMysqlLogin.text,
                         ALEditMysqlPassword.text,
                         ALEditMysqlDatabase.text,
                         strtoint(ALEditMysqlPortNumber.text),
                         ALEditMysqlCharset.Text,
                         0
                        );
    Try

      aXMLDATA := ALCreateEmptyXMLDocument('root');
      Try
        With aXMLDATA Do Begin
          Options := [doNodeAutoIndent];
          ParseOptions := [poPreserveWhiteSpace];
        end;

        aStartDate := GetTickCount;
        aMysqlClient.SelectData(
                                AlMemoMySqlQuery.Lines.Text,
                                aXMLDATA.DocumentElement,
                                'rec',
                                -1,
                                -1
                               );
        if aXMLDATA.DocumentElement.ChildNodes.Count <= 1000 then AlMemoResult.Lines.assign(aXMLDATA.XML)
        else AlMemoResult.Lines.Text := 'More than 1000 records returned!';
        AlMemoResult.Lines.Insert(0,'');
        AlMemoResult.Lines.Insert(0,'**************');
        AlMemoResult.Lines.Insert(0,'');
        AlMemoResult.Lines.Insert(0,'Time to load the data: ' + inttostr(GetTickCount - aStartDate) + ' ms');

      Finally
        aXMLDATA.free;
      End;

    Finally
      aMysqlClient.disconnect;
      aMysqlClient.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{*********************************************************************}
procedure TForm1.ALButtonPaint(Sender: TObject; var continue: Boolean);
begin
  paintAlButtonBlueSkin(sender, Continue);
end;

{**********************************************************}
procedure TForm1.ALEditButtonFindFileClick(Sender: TObject);
begin
  If OpenDialog1.Execute then (Sender as TALEdit).Text := OpenDialog1.FileName;
end;


{*******************************************************************}
procedure TForm1.ALEditPaint(Sender: TObject; var continue: Boolean);
begin
  PaintAlEditBlueSkin(Sender, Continue);
end;

{*******************************************************************}
procedure TForm1.ALMemoPaint(Sender: TObject; var continue: Boolean);
begin
  PaintALMemoBlueSkin(Sender, continue);
end;

{******************************************}
procedure TForm1.FormClick(Sender: TObject);
begin
  Windows.SetFocus(0);
end;


end.
