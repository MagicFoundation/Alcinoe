unit Options;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TOptionsForm = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edLibrary: TEdit;
    edUSER: TEdit;
    edPASS: TEdit;
    btSave: TButton;
    btCancel: TButton;
    procedure btCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  OptionsForm: TOptionsForm;

implementation
uses Inifiles;

{$R *.dfm}

procedure TOptionsForm.btCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(ExtractFilePath(GetModuleName(HInstance)) + 'config.ini');
  try
    edLibrary.Text := ini.ReadString('DATABASE', 'LIBRARY', 'gds32.dll');
    edUSER.Text := ini.ReadString('DATABASE', 'USERNAME', 'SYSDBA');
    edPASS.Text := ini.ReadString('DATABASE', 'PASSWORD', 'masterkey');
  finally
    ini.Free;
  end;
end;

procedure TOptionsForm.btSaveClick(Sender: TObject);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(ExtractFilePath(GetModuleName(HInstance)) + 'config.ini');
  try
    ini.WriteString('DATABASE', 'LIBRARY', edLibrary.Text);
    ini.WriteString('DATABASE', 'USERNAME', edUSER.Text);
    ini.WriteString('DATABASE', 'PASSWORD', edPASS.Text);
  finally
    ini.Free;
  end;
  Close;
end;

procedure TOptionsForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #13: btSave.Click;
    #27: btCancel.Click;
  end;
  if (sender is TEdit) and (Key = #13) then abort;
end;

procedure TOptionsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
