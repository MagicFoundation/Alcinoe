unit Shutdown;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uib;

type
  TShutDownForm = class(TForm)
    edWait: TEdit;
    GroupBox1: TGroupBox;
    rbForced: TRadioButton;
    rbDenyTransaction: TRadioButton;
    rbDenyAttachment: TRadioButton;
    Label1: TLabel;
    btShutdown: TButton;
    Config: TUIBConfig;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btShutdownClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  ShutDownForm: TShutDownForm;

implementation
uses inifiles;

{$R *.dfm}

procedure TShutDownForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TShutDownForm.FormCreate(Sender: TObject);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(ExtractFilePath(GetModuleName(HInstance)) + 'config.ini');
  try
    Config.LibraryName := ini.ReadString('DATABASE', 'LIBRARY', 'gds32.dll');
    Config.UserName := ini.ReadString('DATABASE', 'USERNAME', 'SYSDBA');
    Config.PassWord := ini.ReadString('DATABASE', 'PASSWORD', 'masterkey');
  finally
    ini.Free;
  end;
end;

procedure TShutDownForm.btShutdownClick(Sender: TObject);
var
  wait: integer;
begin
  if not TryStrToInt(edWait.Text, wait) then
  begin
    MessageDlg('Invalid wait value', mtError, [mbOK], 0);
    Exit;
  end;
  if rbForced.Checked then
    Config.ShutdownDatabase(smForced, wait)
  else
    if rbDenyTransaction.Checked then
      Config.ShutdownDatabase(smDenyTransaction, wait)
    else
      if rbDenyAttachment.Checked then
        Config.ShutdownDatabase(smDenyAttachment, wait);
end;

end.

