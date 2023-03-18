unit Unit1;

interface

uses Windows,
     Messages,
     SysUtils,
     Variants,
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     StdCtrls,
     shellapi,
     ExtCtrls,
     ComCtrls,
     SyncObjs,
     Alcinoe.NNTP.Client;

type
  TForm1 = class(TForm)
    MsgMemo: TMemo;
    DisplayMemo: TMemo;
    ToolsPanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Subject: TLabel;
    Label4: TLabel;
    HostEdit: TEdit;
    NewsGroupEdit: TEdit;
    FromEdit: TEdit;
    SubjectEdit: TEdit;
    PortEdit: TEdit;
    Label5: TLabel;
    AttachPanel: TPanel;
    Label6: TLabel;
    FileAttachMemo: TMemo;
    InfoPanel: TPanel;
    Label7: TLabel;
    NextButton: TButton;
    ConnectButton: TButton;
    AuthInfoButton: TButton;
    ArticleByIDButton: TButton;
    HeadByIDButton: TButton;
    BodyByIDButton: TButton;
    QuitButton: TButton;
    Label9: TLabel;
    Label10: TLabel;
    UsernameEdit: TEdit;
    PasswordEdit: TEdit;
    ListButton: TButton;
    GroupButton: TButton;
    StatByIDButton: TButton;
    Label11: TLabel;
    ArticleIDEdit: TEdit;
    ButtonLast: TButton;
    ButtonPost: TButton;
    procedure FormCreate(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure AuthInfoButtonClick(Sender: TObject);
    procedure ArticleByIDButtonClick(Sender: TObject);
    procedure HeadByIDButtonClick(Sender: TObject);
    procedure BodyByIDButtonClick(Sender: TObject);
    procedure QuitButtonClick(Sender: TObject);
    procedure ListButtonClick(Sender: TObject);
    procedure GroupButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StatByIDButtonClick(Sender: TObject);
    procedure ButtonLastClick(Sender: TObject);
    procedure ButtonPostClick(Sender: TObject);
  private
    FNNTPCLient: TALNNTPCLient;
  end;

var
  Form1: TForm1;

implementation

Uses Alcinoe.StringUtils,
     Alcinoe.InternetMessages,
     Alcinoe.MultiPartParser;

{$R *.DFM}

{********************************************}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  FNNTPCLient.Free;
end;

{************************************************}
procedure TForm1.NextButtonClick(Sender: TObject);
begin
 if FNNTPClient.Next then
   DisplayMemo.Lines.Add('NEXT SUCESS')
end;

{***************************************************}
procedure TForm1.ConnectButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(String(ALTrim(FNNTPClient.Connect(AnsiString(HostEdit.Text), StrToInt(PortEdit.Text)))));
end;

{****************************************************}
procedure TForm1.AuthInfoButtonClick(Sender: TObject);
begin
  FNNTPCLient.AUTHINFO(AnsiString(UsernameEdit.Text), AnsiString(PasswordEdit.text));
  DisplayMemo.Lines.Add('AUTHINFO SUCESS');
end;

{************************************************}
procedure TForm1.ListButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(String(ALTrim(FNNTPCLient.List)));
end;

{*************************************************}
procedure TForm1.GroupButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(String(ALTrim(FNNTPClient.Group(AnsiString(NewsGroupEdit.Text)))));
end;

{*******************************************************}
procedure TForm1.ArticleByIDButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(String(ALTrim(FNNTPClient.ArticleByID(AnsiString(ArticleIDEdit.Text)))));
end;

{****************************************************}
procedure TForm1.HeadByIDButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(String(ALTrim(FNNTPClient.HeadByID(AnsiString(ArticleIDEdit.Text)))));
end;

{****************************************************}
procedure TForm1.BodyByIDButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(String(ALTrim(FNNTPClient.BodyByID(AnsiString(ArticleIDEdit.Text)))));
end;

{************************************************}
procedure TForm1.QuitButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(String(ALTrim(FNNTPClient.quit)));

end;

{****************************************************}
procedure TForm1.StatByIDButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(String(ALTrim(FNNTPClient.StatByID(AnsiString(ArticleIDEdit.Text)))));
end;

{************************************************}
procedure TForm1.ButtonLastClick(Sender: TObject);
begin
  if FNNTPClient.Last then
     DisplayMemo.Lines.Add('LAST SUCESS')
end;

{************************************************}
procedure TForm1.ButtonPostClick(Sender: TObject);
Var aNewsArticleHeader: TALNewsArticleHeader;
    AMultiPartMixedAttachments : TALMultiPartMixedContents;
    i : integer;
begin
  aNewsArticleHeader := TALNewsArticleHeader.Create;
  Try

    aNewsArticleHeader.From := AnsiString(FromEdit.Text);
    aNewsArticleHeader.Newsgroups := AnsiString(NewsGroupEdit.Text);
    aNewsArticleHeader.Subject := AnsiString(SubjectEdit.Text);
    If Trim(FileAttachMemo.Lines.text) <> '' then begin
      AMultiPartMixedAttachments := TALMultiPartMixedContents.Create(true);
      Try
        For i := 0 to FileAttachMemo.Lines.Count - 1 do
          If FileAttachMemo.Lines[i] <> '' then
            AMultiPartMixedAttachments.Add.LoadDataFromFileAsAttachmentBase64Encode(ALTrim(AnsiString(FileAttachMemo.Lines[i])));
          FNnTPClient.PostMultipartMixed(aNewsArticleHeader, AnsiString(MsgMemo.Lines.Text), 'text/plain', AMultiPartMixedAttachments);
      finally
        AMultiPartMixedAttachments.Free;
      end;
    end
    else FNNTPClient.Post(aNewsArticleHeader, AnsiString(MsgMemo.Lines.Text));
    DisplayMemo.Lines.Add('POST SUCCESS');

  finally
    aNEwsArticleHeader.Free;
  end;
end;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin
  DisplayMemo.Clear;
  FNNTPClient := TAlNNTPClient.Create;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
