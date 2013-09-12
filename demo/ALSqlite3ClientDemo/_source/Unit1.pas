unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, OleCtrls, SHDocVw, ComObj;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Label5: TLabel;
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
    Label1: TLabel;
    Panel2: TPanel;
    PanelWebBrowser: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure ALButtonSqlLite3SelectClick(Sender: TObject);
    procedure ALButtonSqlite3UpdateClick(Sender: TObject);
    procedure ALButtonSqlLite3VacuumClick(Sender: TObject);
    procedure ALEditSqlite3DatabaseButtonClick(Sender: TObject);
    procedure ALEditSqlite3LibButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  public
  end;

var Form1: TForm1;

implementation

uses AlSqlite3client,
     alStringList,
     ALHTML,
     alXmlDoc,
     ALWindows,
     ALString;

{$R *.dfm}

{*****************************************************************}
procedure TForm1.ALEditSqlite3DatabaseButtonClick(Sender: TObject);
begin
  If OpenDialog1.Execute then (Sender as TEdit).Text := OpenDialog1.FileName;
end;

{************************************************************}
procedure TForm1.ALEditSqlite3LibButtonClick(Sender: TObject);
begin
  If OpenDialog2.Execute then (Sender as TEdit).Text := OpenDialog2.FileName;
end;

{***********************************************************}
procedure TForm1.ALButtonSqlite3UpdateClick(Sender: TObject);
Var aSqlite3Client: TalSqlite3Client;
    aStartDate: int64;
    aEndDate: int64;
    aStartCommitDate: int64;
    aEndCommitDate: int64;
    LstSql: TALStringList;
    S1: AnsiString;
    i: integer;
begin
  Screen.Cursor := CrHourGlass;
  try

    aSqlite3Client := TalSqlite3Client.Create(AnsiString(ALEditSqlite3lib.Text));
    LstSql := TALStringList.Create;
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
      while AlPos('<#randomchar>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomchar>',AlRandomStr(1),[rfIgnoreCase]);
      while AlPos('<#randomnumber>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomnumber>',ALIntToStr(random(10)),[rfIgnoreCase]);
      for i := 1 to maxint do begin
        if AlPos('<#randomnumber'+ALIntToStr(i)+'>', AlLowerCase(S1)) > 0 then S1 := AlStringReplace(S1, '<#randomnumber'+ALIntToStr(i)+'>',ALIntToStr(random(10)),[rfIgnoreCase, rfReplaceAll])
        else break;
      end;
      S1 := AlStringReplace(S1,#13#10,' ',[RfReplaceALL]);
      LstSql.Text := ALTrim(AlStringReplace(S1,';',#13#10,[RfReplaceALL]));

      //do the job
      aStartDate := ALGetTickCount64;
      aSqlite3Client.TransactionStart;
      try
        aSqlite3Client.UpdateData(LstSql);
        aEndDate := ALGetTickCount64;
        aStartCommitDate := ALGetTickCount64;
        aSqlite3Client.TransactionCommit;
        aendCommitDate := ALGetTickCount64;
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
    aFormatSettings: TALFormatSettings;
    S1: AnsiString;
    i: integer;
begin
  ALGetLocaleFormatSettings(1033, aFormatSettings);
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
        while AlPos('<#randomchar>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomchar>',AlRandomStr(1),[rfIgnoreCase]);
        while AlPos('<#randomnumber>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomnumber>',ALIntToStr(random(10)),[rfIgnoreCase]);
        for i := 1 to maxint do begin
          if AlPos('<#randomnumber'+ALIntToStr(i)+'>', AlLowerCase(S1)) > 0 then S1 := AlStringReplace(S1, '<#randomnumber'+ALIntToStr(i)+'>',ALIntToStr(random(10)),[rfIgnoreCase, rfReplaceAll])
          else break;
        end;

        aStartDate := ALGetTickCount64;
        aSqlite3Client.SelectData(S1,
                                  'rec',
                                   0,
                                   200,
                                  aXMLDATA.DocumentElement,
                                  aFormatSettings);
        aEndDate := ALGetTickCount64;

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
      aStartDate := ALGetTickCount64;
      aSqlite3Client.UpdateData('VACUUM');
      aEndDate := ALGetTickCount64;

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
  ie.MenuBar := false;
  ie.AddressBar := false;
  ie.Resizable := false;
  ie.StatusBar := false;
  ie.ToolBar := 0;
  ie.Width := 100;
  ie.Height := 300;
  Url := 'http://static.arkadia.com/html/alcinoe_like.html';
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
  sleep(500);
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.


