unit Restore;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uib, StdCtrls;

type
  TRestoreForm = class(TForm)
    edRestoreFile: TEdit;
    btBrowse: TButton;
    SaveDialog: TSaveDialog;
    Label1: TLabel;
    log: TMemo;
    cbVerbose: TCheckBox;
    btGO: TButton;
    btClose: TButton;
    GroupBox1: TGroupBox;
    cbSave: TCheckBox;
    UIBRestore: TUIBRestore;
    cbDeactivateIndexes: TCheckBox;
    cbNoShadow: TCheckBox;
    cbNoValidityCheck: TCheckBox;
    cbOneRelationAtATime: TCheckBox;
    cbReplace: TCheckBox;
    cbCreateNewDB: TCheckBox;
    cbUseAllSpace: TCheckBox;
    cbCloseWhenDone: TCheckBox;
    Label5: TLabel;
    edPageSize: TEdit;
    cbLocalHost: TCheckBox;
    procedure btBrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ConfigChange(Sender: TObject);
    procedure UIBRestoreVerbose(Sender: TObject; Message: String);
    procedure btGOClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btCloseClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  RestoreForm: TRestoreForm;

implementation
uses inifiles;

{$R *.dfm}

procedure TRestoreForm.btBrowseClick(Sender: TObject);
begin
  SaveDialog.InitialDir := ExtractFilePath(edRestoreFile.Text);
  if SaveDialog.Execute then
    edRestoreFile.Text := SaveDialog.FileName;
end;

procedure TRestoreForm.FormCreate(Sender: TObject);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(ExtractFilePath(GetModuleName(HInstance)) + 'config.ini');
  try
    UIBRestore.Database := ini.ReadString('DATABASE', 'LIBRARY', 'gds32.dll');
    UIBRestore.UserName := ini.ReadString('DATABASE', 'USERNAME', 'SYSDBA');
    UIBRestore.PassWord := ini.ReadString('DATABASE', 'PASSWORD', 'masterkey');

    cbDeactivateIndexes.Checked := ini.ReadBool('RESTORE', 'DeactivateIndexes', false);
    cbNoShadow.Checked := ini.ReadBool('RESTORE', 'NoShadow', false);
    cbNoValidityCheck.Checked := ini.ReadBool('RESTORE', 'NoValidityCheck', false);
    cbOneRelationAtATime.Checked := ini.ReadBool('RESTORE', 'OneRelationAtATime', false);
    cbReplace.Checked := ini.ReadBool('RESTORE', 'Replace', false);
    cbCreateNewDB.Checked := ini.ReadBool('RESTORE', 'CreateNewDB', true);
    cbUseAllSpace.Checked := ini.ReadBool('RESTORE', 'UseAllSpace', false);
    edPageSize.Text := ini.ReadString('RESTORE', 'PageSize', '4096');
    cbVerbose.Checked := ini.ReadBool('RESTORE', 'Verbose', true);
    cbCloseWhenDone.Checked := ini.ReadBool('RESTORE', 'CloseWhenDone', false);
    cbLocalHost.Checked := ini.ReadBool('RESTORE', 'Localhost', false);
  finally
    ini.Free;
  end;
end;

procedure TRestoreForm.ConfigChange(Sender: TObject);
var
  Options: TRestoreOptions;
  ps: Integer;
begin
  UIBRestore.Database := edRestoreFile.Text;
  options := [];
  if cbDeactivateIndexes.Checked then Include(options, roDeactivateIndexes);
  if cbNoShadow.Checked then Include(options, roNoShadow);
  if cbNoValidityCheck.Checked then Include(options, roNoValidityCheck);
  if cbOneRelationAtATime.Checked then Include(options, roOneRelationAtATime);
  if cbReplace.Checked then Include(options, roReplace);
  if cbCreateNewDB.Checked then Include(options, roCreateNewDB);
  if cbUseAllSpace.Checked then Include(options, roUseAllSpace);
  UIBRestore.Options := options;

  if TryStrToInt(edPageSize.Text, ps) then
    UIBRestore.PageSize := ps;
end;

procedure TRestoreForm.UIBRestoreVerbose(Sender: TObject; Message: String);
begin
  if cbVerbose.Checked then
  begin
    log.Lines.Add(Message);
    Application.ProcessMessages;
  end;
end;

procedure TRestoreForm.btGOClick(Sender: TObject);
begin
  if cbLocalHost.Checked then
    UIBRestore.Protocol := proTCPIP else
    UIBRestore.Protocol := proLocalHost;

  log.Clear;
  btGO.Enabled := false;
  Screen.Cursor := crHourGlass;
  try
    UIBRestore.Run;
  finally
    Screen.Cursor := crDefault;
    btGO.Enabled := true;
  end;
  if cbCloseWhenDone.Checked then Close;
end;

procedure TRestoreForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  ini: TIniFile;
begin
  if not btGO.Enabled then
  begin
    Action := caNone;
    Exit;
  end;

  if cbSave.Checked then
  begin
    ini := TIniFile.Create(ExtractFilePath(GetModuleName(HInstance)) + 'config.ini');
    try
      ini.WriteBool('RESTORE', 'DeactivateIndexes', cbDeactivateIndexes.Checked);
      ini.WriteBool('RESTORE', 'NoShadow', cbNoShadow.Checked);
      ini.WriteBool('RESTORE', 'NoValidityCheck', cbNoValidityCheck.Checked);
      ini.WriteBool('RESTORE', 'OneRelationAtATime', cbOneRelationAtATime.Checked);
      ini.WriteBool('RESTORE', 'Replace', cbReplace.Checked);
      ini.WriteBool('RESTORE', 'CreateNewDB', cbCreateNewDB.Checked);
      ini.WriteBool('RESTORE', 'UseAllSpace', cbUseAllSpace.Checked);
      ini.WriteInteger('RESTORE', 'PageSize', UIBRestore.PageSize);
      ini.WriteBool('RESTORE', 'Verbose', cbVerbose.Checked);
      ini.WriteBool('RESTORE', 'CloseWhenDone', cbCloseWhenDone.Checked);
      ini.WriteBool('RESTORE', 'Localhost', cbLocalHost.Checked);
    finally
      ini.Free;
    end;
  end;
  Action := caFree;
end;

procedure TRestoreForm.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TRestoreForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #13: if btGO.Enabled then btGO.Click;
    #27: Close;
  end;
  if (sender is TEdit) and (Key = #13) then abort;
end;

end.


