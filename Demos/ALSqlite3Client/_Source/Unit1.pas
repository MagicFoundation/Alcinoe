unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, Shellapi;

type
  TForm1 = class(TForm)
    ALMemoResult: TMemo;
    Label6: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label19: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    ALEditSqlite3Lib: TEdit;
    ALMemoSqlite3Query: TMemo;
    ALButtonSqlLite3Select: TButton;
    ALEditSqlite3Database: TEdit;
    ALButtonSqlite3Update: TButton;
    RadioGroupSqlite3Journal_Mode: TRadioGroup;
    RadioGroupSQLite3Temp_Store: TRadioGroup;
    RadioGroupSqlite3Synhcronous: TRadioGroup;
    ALEditSqlite3Cache_Size: TEdit;
    ALEditSqlite3Page_Size: TEdit;
    ALCheckBoxSqlite3SharedCache: TCheckbox;
    ALCheckBoxSqlite3ReadUncommited: TCheckbox;
    OpenDialog1: TOpenDialog;
    ALButtonSqlLite3Vacuum: TButton;
    OpenDialog2: TOpenDialog;
    Button1: TButton;
    Button2: TButton;
    procedure ALButtonSqlLite3SelectClick(Sender: TObject);
    procedure ALButtonSqlite3UpdateClick(Sender: TObject);
    procedure ALButtonSqlLite3VacuumClick(Sender: TObject);
    procedure ALEditSqlite3DatabaseButtonClick(Sender: TObject);
    procedure ALEditSqlite3LibButtonClick(Sender: TObject);
  private
  public
  end;

var Form1: TForm1;

implementation

uses
  System.AnsiStrings,
  Alcinoe.Sqlite3.Client,
  Alcinoe.StringList,
  Alcinoe.HTML,
  Alcinoe.XMLDoc,
  Alcinoe.WinApi.Common,
  Alcinoe.StringUtils;

{$R *.dfm}

{*****************************************************************}
procedure TForm1.ALEditSqlite3DatabaseButtonClick(Sender: TObject);
begin
  If OpenDialog1.Execute then ALEditSqlite3Database.Text := OpenDialog1.FileName;
end;

{************************************************************}
procedure TForm1.ALEditSqlite3LibButtonClick(Sender: TObject);
begin
  If OpenDialog2.Execute then ALEditSqlite3Lib.Text := OpenDialog2.FileName;
end;

{***********************************************************}
procedure TForm1.ALButtonSqlite3UpdateClick(Sender: TObject);
Var aSqlite3Client: TalSqlite3Client;
    aStartDate: int64;
    aEndDate: int64;
    aStartCommitDate: int64;
    aEndCommitDate: int64;
    LstSql: TALStringListA;
    S1: AnsiString;
    i: integer;
begin
  Screen.Cursor := CrHourGlass;
  try

    aSqlite3Client := TalSqlite3Client.Create(AnsiString(ALEditSqlite3lib.Text));
    LstSql := TALStringListA.Create;
    Try

      //enable or disable the shared cache
      aSqlite3Client.enable_shared_cache(ALCheckBoxSqlite3SharedCache.Checked);

      //connect
      aSqlite3Client.connect(AnsiString(ALEditSqlite3Database.text));

      //the pragma
      aSqlite3Client.UpdateData('PRAGMA page_size = '+AnsiString(ALEditSqlite3Page_Size.Text));
      aSqlite3Client.UpdateData('PRAGMA encoding = "UTF-8"');
      aSqlite3Client.UpdateData('PRAGMA legacy_file_format = 0');
      aSqlite3Client.UpdateData('PRAGMA auto_vacuum = NONE');
      aSqlite3Client.UpdateData('PRAGMA cache_size = '+AnsiString(ALEditSqlite3Cache_Size.Text));
      case RadioGroupSqlite3Journal_Mode.ItemIndex of
        0: aSqlite3Client.UpdateData('PRAGMA journal_mode = DELETE');
        1: aSqlite3Client.UpdateData('PRAGMA journal_mode = TRUNCATE');
        2: aSqlite3Client.UpdateData('PRAGMA journal_mode = PERSIST');
        3: aSqlite3Client.UpdateData('PRAGMA journal_mode = MEMORY');
        4: aSqlite3Client.UpdateData('PRAGMA journal_mode = WAL');
        5: aSqlite3Client.UpdateData('PRAGMA journal_mode = OFF');
      end;
      aSqlite3Client.UpdateData('PRAGMA locking_mode = NORMAL');
      If ALCheckBoxSqlite3ReadUncommited.Checked then aSqlite3Client.UpdateData('PRAGMA read_uncommitted = 1');
      case RadioGroupSqlite3Synhcronous.ItemIndex of
        0: aSqlite3Client.UpdateData('PRAGMA synchronous = OFF');
        1: aSqlite3Client.UpdateData('PRAGMA synchronous = NORMAL');
        2: aSqlite3Client.UpdateData('PRAGMA synchronous = FULL');
      end;
      case RadioGroupSQLite3Temp_Store.ItemIndex of
        0: aSqlite3Client.UpdateData('PRAGMA temp_store = DEFAULT');
        1: aSqlite3Client.UpdateData('PRAGMA temp_store = FILE');
        2: aSqlite3Client.UpdateData('PRAGMA temp_store = MEMORY');
      end;

      //the sql
      S1 := AnsiString(AlMemoSQLite3Query.Lines.Text);
      while ALPosA('<#randomchar>', AlLowerCase(S1)) > 0 do S1 := ALStringReplaceA(S1, '<#randomchar>',ALRandomStrA(1),[rfIgnoreCase]);
      while ALPosA('<#randomnumber>', AlLowerCase(S1)) > 0 do S1 := ALStringReplaceA(S1, '<#randomnumber>',ALIntToStrA(random(10)),[rfIgnoreCase]);
      for i := 1 to maxint do begin
        if ALPosA('<#randomnumber'+ALIntToStrA(i)+'>', AlLowerCase(S1)) > 0 then S1 := ALStringReplaceA(S1, '<#randomnumber'+ALIntToStrA(i)+'>',ALIntToStrA(random(10)),[rfIgnoreCase, rfReplaceAll])
        else break;
      end;
      S1 := ALStringReplaceA(S1,#13#10,' ',[RfReplaceALL]);
      LstSql.Text := ALTrim(ALStringReplaceA(S1,';',#13#10,[RfReplaceALL]));

      //do the job
      aStartDate := GetTickCount64;
      aSqlite3Client.TransactionStart;
      try
        aSqlite3Client.UpdateData(LstSql);
        aEndDate := GetTickCount64;
        aStartCommitDate := GetTickCount64;
        aSqlite3Client.TransactionCommit;
        aendCommitDate := GetTickCount64;
      Except
        aSqlite3Client.TransactionRollBack;
        raise;
      end;

      ALMemoResult.Lines.clear;
      ALMemoResult.Lines.add('Time Taken to Update the data: ' + IntToStr(aEndDate - aStartDate) + ' ms');
      ALMemoResult.Lines.add('Time Taken to commit the modifications: ' + IntToStr(aendCommitDate - aStartCommitDate) + ' ms');

    Finally
      aSqlite3Client.disconnect;
      aSqlite3Client.free;
      LstSql.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{************************************************************}
procedure TForm1.ALButtonSqlLite3SelectClick(Sender: TObject);
Var aSqlite3Client: TalSqlite3Client;
    aXMLDATA: TalXmlDocument;
    aStartDate: int64;
    aEndDate: int64;
    aFormatSettings: TALFormatSettingsA;
    S1: AnsiString;
    i: integer;
begin
  aFormatSettings := ALDefaultFormatSettingsA;
  Screen.Cursor := CrHourGlass;
  try

    aSqlite3Client := TalSqlite3Client.Create(AnsiString(ALEditSqlite3lib.Text));
    Try

      //enable or disable the shared cache
      aSqlite3Client.enable_shared_cache(ALCheckBoxSqlite3SharedCache.Checked);

      //connect
      aSqlite3Client.connect(AnsiString(ALEditSqlite3Database.text));

      //the pragma
      aSqlite3Client.UpdateData('PRAGMA page_size = '+AnsiString(ALEditSqlite3Page_Size.Text));
      aSqlite3Client.UpdateData('PRAGMA encoding = "UTF-8"');
      aSqlite3Client.UpdateData('PRAGMA legacy_file_format = 0');
      aSqlite3Client.UpdateData('PRAGMA auto_vacuum = NONE');
      aSqlite3Client.UpdateData('PRAGMA cache_size = '+AnsiString(ALEditSqlite3Cache_Size.Text));
      case RadioGroupSqlite3Journal_Mode.ItemIndex of
        0: aSqlite3Client.UpdateData('PRAGMA journal_mode = DELETE');
        1: aSqlite3Client.UpdateData('PRAGMA journal_mode = TRUNCATE');
        2: aSqlite3Client.UpdateData('PRAGMA journal_mode = PERSIST');
        3: aSqlite3Client.UpdateData('PRAGMA journal_mode = MEMORY');
        4: aSqlite3Client.UpdateData('PRAGMA journal_mode = WAL');
        5: aSqlite3Client.UpdateData('PRAGMA journal_mode = OFF');
      end;
      aSqlite3Client.UpdateData('PRAGMA locking_mode = NORMAL');
      If ALCheckBoxSqlite3ReadUncommited.Checked then aSqlite3Client.UpdateData('PRAGMA read_uncommitted = 1');
      case RadioGroupSqlite3Synhcronous.ItemIndex of
        0: aSqlite3Client.UpdateData('PRAGMA synchronous = OFF');
        1: aSqlite3Client.UpdateData('PRAGMA synchronous = NORMAL');
        2: aSqlite3Client.UpdateData('PRAGMA synchronous = FULL');
      end;
      case RadioGroupSQLite3Temp_Store.ItemIndex of
        0: aSqlite3Client.UpdateData('PRAGMA temp_store = DEFAULT');
        1: aSqlite3Client.UpdateData('PRAGMA temp_store = FILE');
        2: aSqlite3Client.UpdateData('PRAGMA temp_store = MEMORY');
      end;

      //the sql
      aXMLDATA := TALXmlDocument.create('root');
      Try

        With aXMLDATA Do Begin
          Options := [doNodeAutoIndent];
          ParseOptions := [poPreserveWhiteSpace];
        end;

        S1 := AnsiString(AlMemoSQLite3Query.Lines.Text);
        while ALPosA('<#randomchar>', AlLowerCase(S1)) > 0 do S1 := ALStringReplaceA(S1, '<#randomchar>',ALRandomStrA(1),[rfIgnoreCase]);
        while ALPosA('<#randomnumber>', AlLowerCase(S1)) > 0 do S1 := ALStringReplaceA(S1, '<#randomnumber>',ALIntToStrA(random(10)),[rfIgnoreCase]);
        for i := 1 to maxint do begin
          if ALPosA('<#randomnumber'+ALIntToStrA(i)+'>', AlLowerCase(S1)) > 0 then S1 := ALStringReplaceA(S1, '<#randomnumber'+ALIntToStrA(i)+'>',ALIntToStrA(random(10)),[rfIgnoreCase, rfReplaceAll])
          else break;
        end;

        aStartDate := GetTickCount64;
        aSqlite3Client.SelectData(
          S1,
          'rec',
           0,
           200,
          aXMLDATA.DocumentElement,
          aFormatSettings);
        aEndDate := GetTickCount64;

        ALMemoResult.Lines.Text := 'Time Taken to select the data: ' + IntToStr(aEndDate - aStartDate) + ' ms' + #13#10 +
                                   #13#10 +
                                   String(ALTrim(aXMLDATA.XML));

      Finally
        aXMLDATA.free;
      End;

    Finally
      aSqlite3Client.disconnect;
      aSqlite3Client.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

{************************************************************}
procedure TForm1.ALButtonSqlLite3VacuumClick(Sender: TObject);
Var aSqlite3Client: TalSqlite3Client;
    aStartDate: int64;
    aEndDate: int64;
begin
  Screen.Cursor := CrHourGlass;
  try

    aSqlite3Client := TalSqlite3Client.Create(AnsiString(ALEditSqlite3lib.Text));
    Try

      //enable or disable the shared cache
      aSqlite3Client.enable_shared_cache(ALCheckBoxSqlite3SharedCache.Checked);

      //connect
      aSqlite3Client.connect(AnsiString(ALEditSqlite3Database.text));

      //the pragma
      aSqlite3Client.UpdateData('PRAGMA page_size = '+AnsiString(ALEditSqlite3Page_Size.Text));
      aSqlite3Client.UpdateData('PRAGMA encoding = "UTF-8"');
      aSqlite3Client.UpdateData('PRAGMA legacy_file_format = 0');
      aSqlite3Client.UpdateData('PRAGMA auto_vacuum = NONE');
      aSqlite3Client.UpdateData('PRAGMA cache_size = '+AnsiString(ALEditSqlite3Cache_Size.Text));
      case RadioGroupSqlite3Journal_Mode.ItemIndex of
        0: aSqlite3Client.UpdateData('PRAGMA journal_mode = DELETE');
        1: aSqlite3Client.UpdateData('PRAGMA journal_mode = TRUNCATE');
        2: aSqlite3Client.UpdateData('PRAGMA journal_mode = PERSIST');
        3: aSqlite3Client.UpdateData('PRAGMA journal_mode = MEMORY');
        4: aSqlite3Client.UpdateData('PRAGMA journal_mode = WAL');
        5: aSqlite3Client.UpdateData('PRAGMA journal_mode = OFF');
      end;
      aSqlite3Client.UpdateData('PRAGMA locking_mode = NORMAL');
      If ALCheckBoxSqlite3ReadUncommited.Checked then aSqlite3Client.UpdateData('PRAGMA read_uncommitted = 1');
      case RadioGroupSqlite3Synhcronous.ItemIndex of
        0: aSqlite3Client.UpdateData('PRAGMA synchronous = OFF');
        1: aSqlite3Client.UpdateData('PRAGMA synchronous = NORMAL');
        2: aSqlite3Client.UpdateData('PRAGMA synchronous = FULL');
      end;
      case RadioGroupSQLite3Temp_Store.ItemIndex of
        0: aSqlite3Client.UpdateData('PRAGMA temp_store = DEFAULT');
        1: aSqlite3Client.UpdateData('PRAGMA temp_store = FILE');
        2: aSqlite3Client.UpdateData('PRAGMA temp_store = MEMORY');
      end;

      //do the job
      aStartDate := GetTickCount64;
      aSqlite3Client.UpdateData('VACUUM');
      aEndDate := GetTickCount64;

      ALMemoResult.Lines.clear;
      ALMemoResult.Lines.add('Time Taken to VACUUM the database: ' + IntToStr(aEndDate - aStartDate) + ' ms');

    Finally
      aSqlite3Client.disconnect;
      aSqlite3Client.free;
    End;

  Finally
    Screen.Cursor := CrDefault;
  End;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
