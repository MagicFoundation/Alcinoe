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
    ALMemoFirebirdResult: TALMemo;
    Label1: TLabel;
    ALMemoFirebirdTPB: TALMemo;
    ALButtonFirebirdUpdate: TALButton;
    ALButtonFirebirdOpenConnection: TALButton;
    ALButtonCloseConnection: TALButton;
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
  private
    fAlFbxClient: TalFBXclient;
  public
  end;

var Form1: TForm1;

implementation

uses alFcnSkin,
     AlFbxBase,
     AlFbxLib,
     alStringList,
     ALFcnHTML,
     ALXmlDoc,
     alFcnString;

{$R *.dfm}

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

{************************************************************}
procedure TForm1.ALButtonFirebirdUpdateClick(Sender: TObject);
begin
  fAlFbxClient.UpdateData(ALMemoFireBirdQuery.Lines.Text);
  ALMemoFirebirdResult.Lines.Text := 'Update SQL Done';  
end;

{********************************************************************}
procedure TForm1.ALButtonFirebirdCreateDatabaseClick(Sender: TObject);
Var aFBXClient: TalFBXClient;
    aFBAPiVersion: TALFBXVersion_API;
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
    aFBXClient.CreateDatabase(AlMemoFireBirdQuery.Lines.Text);
  Finally
    aFBXClient.free;
  End;

  ALMemoFirebirdResult.Lines.Text := 'Create DataBase Done';
end;

{********************************************************************}
procedure TForm1.ALButtonFirebirdOpenConnectionClick(Sender: TObject);
Var aFBAPiVersion: TALFBXVersion_API;
begin

  ALButtonFirebirdOpenConnection.Enabled := False;
  ALButtonFirebirdStartTransaction.Enabled := True;
  ALButtonFirebirdSelect.Enabled := False;
  ALButtonFirebirdUpdate.Enabled := False;
  ALButtonFirebirdCommit.Enabled := False;
  ALButtonFirebirdRollBack.Enabled := False;
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
        fAlFbxClient.connect(ALEditFireBirdDatabase.Text,
                             ALEditFireBirdLogin.text,
                             ALEditFireBirdPassword.text,
                             ALEditFireBirdCharset.Text,
                             StrtoInt(ALEditFireBirdNum_buffers.Text));
      Except
        fAlFbxClient.Free;
        fAlFbxClient := Nil;
        raise;
      end;

      ALMemoFirebirdResult.Lines.Text := 'Open Connection Done';
    end;

  except
    ALButtonFirebirdOpenConnection.Enabled := True;
    ALButtonFirebirdStartTransaction.Enabled := False;
    ALButtonFirebirdSelect.Enabled := False;
    ALButtonFirebirdUpdate.Enabled := False;
    ALButtonFirebirdCommit.Enabled := False;
    ALButtonFirebirdRollBack.Enabled := False;
    ALButtonCloseConnection.Enabled := False;
    Raise;
  End;

end;

{*************************************************************}
procedure TForm1.ALButtonCloseConnectionClick(Sender: TObject);
begin
  ALButtonFirebirdOpenConnection.Enabled := True;
  ALButtonFirebirdStartTransaction.Enabled := False;
  ALButtonFirebirdSelect.Enabled := False;
  ALButtonFirebirdUpdate.Enabled := False;
  ALButtonFirebirdCommit.Enabled := False;
  ALButtonFirebirdRollBack.Enabled := False;
  ALButtonCloseConnection.Enabled := False;

  try
    fAlFbxClient.Disconnect;
    ALMemoFirebirdResult.Lines.Text := 'Close Transaction Done';
  finally
    fAlFbxClient.Free;
    fAlFbxClient := nil;
  end;
end;

{************************************************************}
procedure TForm1.ALButtonFirebirdCommitClick(Sender: TObject);
begin
  ALButtonFirebirdOpenConnection.Enabled := False;
  ALButtonFirebirdStartTransaction.Enabled := True;
  ALButtonFirebirdSelect.Enabled := False;
  ALButtonFirebirdUpdate.Enabled := False;
  ALButtonFirebirdCommit.Enabled := False;
  ALButtonFirebirdRollBack.Enabled := False;
  ALButtonCloseConnection.Enabled := True;
  Try

    fAlFbxClient.TransactionCommit;
    ALMemoFirebirdResult.Lines.Text := 'Commit Transaction Done';    

  except
    ALButtonFirebirdOpenConnection.Enabled := False;
    ALButtonFirebirdStartTransaction.Enabled := False;
    ALButtonFirebirdSelect.Enabled := True;
    ALButtonFirebirdUpdate.Enabled := True;
    ALButtonFirebirdCommit.Enabled := True;
    ALButtonFirebirdRollBack.Enabled := True;
    ALButtonCloseConnection.Enabled := True;
    Raise;
  End;
end;

{**************************************************************}
procedure TForm1.ALButtonFirebirdRollBackClick(Sender: TObject);
begin
  ALButtonFirebirdOpenConnection.Enabled := False;
  ALButtonFirebirdStartTransaction.Enabled := True;
  ALButtonFirebirdSelect.Enabled := False;
  ALButtonFirebirdUpdate.Enabled := False;
  ALButtonFirebirdCommit.Enabled := False;
  ALButtonFirebirdRollBack.Enabled := False;
  ALButtonCloseConnection.Enabled := True;
  Try

    fAlFbxClient.TransactionRollback;
    ALMemoFirebirdResult.Lines.Text := 'RollBack Transaction Done';

  except
    ALButtonFirebirdOpenConnection.Enabled := False;
    ALButtonFirebirdStartTransaction.Enabled := False;
    ALButtonFirebirdSelect.Enabled := True;
    ALButtonFirebirdUpdate.Enabled := True;
    ALButtonFirebirdCommit.Enabled := True;
    ALButtonFirebirdRollBack.Enabled := True;
    ALButtonCloseConnection.Enabled := True;
    Raise;
  End;
end;

{**********************************************************************}
procedure TForm1.ALButtonFirebirdStartTransactionClick(Sender: TObject);
Var aTPB: String;
begin
  ALButtonFirebirdOpenConnection.Enabled := False;
  ALButtonFirebirdStartTransaction.Enabled := False;
  ALButtonFirebirdSelect.Enabled := True;
  ALButtonFirebirdUpdate.Enabled := True;
  ALButtonFirebirdCommit.Enabled := True;
  ALButtonFirebirdRollBack.Enabled := True;
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
    fAlFbxClient.TransactionStart(False, aTPB);

    ALMemoFirebirdResult.Lines.Text := 'Start Transaction Done';

  except
    ALButtonFirebirdOpenConnection.Enabled := False;
    ALButtonFirebirdStartTransaction.Enabled := True;
    ALButtonFirebirdSelect.Enabled := False;
    ALButtonFirebirdUpdate.Enabled := False;
    ALButtonFirebirdCommit.Enabled := False;
    ALButtonFirebirdRollBack.Enabled := False;
    ALButtonCloseConnection.Enabled := True;
    Raise;
  End;
end;

{************************************************************}
procedure TForm1.ALButtonFirebirdSelectClick(Sender: TObject);
var aXMLDATA: TalXmlDocument;
    aFormatSettings: TformatSettings;
    S1: String;
    i: integer;
begin

  GetLocaleFormatSettings(1033, aFormatSettings);
  aXMLDATA := ALCreateEmptyXMLDocument('root');
  Try

    With aXMLDATA Do Begin
      Options := [doNodeAutoIndent];
      ParseOptions := [poPreserveWhiteSpace];
    end;

    S1 := AlMemoFireBirdQuery.Lines.Text;
    while AlPos('<#randomchar>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomchar>',AlRandomStr(1),[rfIgnoreCase]);
    while AlPos('<#randomnumber>', AlLowerCase(S1)) > 0 do S1 := AlStringReplace(S1, '<#randomnumber>',inttostr(random(10)),[rfIgnoreCase]);
    for i := 1 to 100 do begin
      if AlPos('<#randomnumber'+inttostr(i)+'>', AlLowerCase(S1)) > 0 then S1 := AlStringReplace(S1, '<#randomnumber'+inttostr(i)+'>',inttostr(random(10)),[rfIgnoreCase, rfReplaceAll]);
    end;
    for i := 101 to maxint do begin
      if AlPos('<#randomnumber'+inttostr(i)+'>', AlLowerCase(S1)) > 0 then S1 := AlStringReplace(S1, '<#randomnumber'+inttostr(i)+'>',inttostr(random(10)),[rfIgnoreCase, rfReplaceAll])
      else break;
    end;

    FAlFBXClient.SelectData(S1,
                            'rec',
                             0,
                             200,
                            aXMLDATA.DocumentElement,
                            aFormatSettings);
    ALMemoFirebirdResult.Lines.Text := aXMLDATA.XML.Text;

  Finally
    aXMLDATA.free;
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
