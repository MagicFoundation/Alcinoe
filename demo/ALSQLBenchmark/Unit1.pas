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
    ALButtonFirebird: TALButton;
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
    procedure ALButtonPaint(Sender: TObject; var continue: Boolean);
    procedure FormClick(Sender: TObject);
    procedure ALButtonMySqlClick(Sender: TObject);
    procedure ALEditButtonFindFileClick(Sender: TObject);
    procedure ALEditPaint(Sender: TObject; var continue: Boolean);
    procedure ALMemoPaint(Sender: TObject; var continue: Boolean);
    procedure ALButtonFirebirdClick(Sender: TObject);
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

{******************************************************}
procedure TForm1.ALButtonFirebirdClick(Sender: TObject);
Var aFBXClient: TALFbxClient;
    aXMLDATA: TalXmlDocument;
    aStartDate: Cardinal;
    aFormatSettings: TFormatSettings;
begin
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

      aXMLDATA:= TALXmlDocument.Create(nil);
      Try
        With aXMLDATA Do Begin
          Options := [doNodeAutoIndent];
          ParseOptions := [poPreserveWhiteSpace];
          Active := true;
          version := '1.0';
          standalone := 'yes';
          Encoding := 'UTF-8';
          aXMLDATA.AddChild('root');
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
        messageDlg('Time to load the data: ' + inttostr(GetTickCount - aStartDate) + ' ms', mtInformation, [mbOK], 0);
        if aXMLDATA.DocumentElement.ChildNodes.Count <= 1000 then
          AlMemoResult.Lines.assign(aXMLDATA.XML);

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

      aXMLDATA:= TALXmlDocument.Create(nil);
      Try
        With aXMLDATA Do Begin
          Options := [doNodeAutoIndent];
          ParseOptions := [poPreserveWhiteSpace];
          Active := true;
          version := '1.0';
          standalone := 'yes';
          Encoding := 'UTF-8';
          aXMLDATA.AddChild('root');
        end;

        aStartDate := GetTickCount;
        aMysqlClient.SelectData(
                                AlMemoMySqlQuery.Lines.Text,
                                aXMLDATA.DocumentElement,
                                'rec',
                                -1,
                                -1
                               );
        messageDlg('Time to load the data: ' + inttostr(GetTickCount - aStartDate) + ' ms', mtInformation, [mbOK], 0);
        if aXMLDATA.DocumentElement.ChildNodes.Count <= 1000 then
          AlMemoResult.Lines.assign(aXMLDATA.XML);

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
