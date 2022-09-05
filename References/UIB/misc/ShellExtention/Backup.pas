unit Backup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uib, StdCtrls, ExtCtrls;

type
  TBackupForm = class(TForm)
    UIBBackup: TUIBBackup;
    edBackupFile: TEdit;
    btBrowse: TButton;
    SaveDialog: TSaveDialog;
    Label1: TLabel;
    log: TMemo;
    cbVerbose: TCheckBox;
    btGO: TButton;
    btClose: TButton;
    GroupBox1: TGroupBox;
    cbIgnoreChecksums: TCheckBox;
    cbIgnoreLimbo: TCheckBox;
    cbMetadataOnly: TCheckBox;
    cbNoGarbageCollection: TCheckBox;
    cbOldMetadataDesc: TCheckBox;
    cbNonTransportable: TCheckBox;
    cbConvertExtTables: TCheckBox;
    cbExpand: TCheckBox;
    cbSave: TCheckBox;
    cbCloseWhenDone: TCheckBox;
    rgCompression: TRadioGroup;
    cbLocalHost: TCheckBox;
    procedure btBrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ConfigChange(Sender: TObject);
    procedure UIBBackupVerbose(Sender: TObject; Message: String);
    procedure btGOClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btCloseClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Déclarations privées }
    procedure Run7Zip;
    procedure RunWinRar;
    procedure RunWinAce;
    function FindComObjectPath(guid: TGUID): string;
  public
    { Déclarations publiques }
  end;

var
  BackupForm: TBackupForm;

implementation
uses inifiles, shellapi, registry;

const
  CLSID_Winrar: TGUID = '{B41DB860-8EE4-11D2-9906-E49FADC173CA}';
  CLSID_7Zip: TGUID = '{23170F69-40C1-278A-1000-000100020000}';
  CLSID_WinAce: TGUID = '{8FF88D21-7BD0-11D1-BFB7-00AA00262A11}';

{$R *.dfm}

procedure TBackupForm.btBrowseClick(Sender: TObject);
begin
  SaveDialog.InitialDir := ExtractFilePath(edBackupFile.Text);
  if SaveDialog.Execute then
    edBackupFile.Text := SaveDialog.FileName;
end;

procedure TBackupForm.FormCreate(Sender: TObject);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(ExtractFilePath(GetModuleName(HInstance)) + 'config.ini');
  try
    UIBBackup.Database := ini.ReadString('DATABASE', 'LIBRARY', 'gds32.dll');
    UIBBackup.UserName := ini.ReadString('DATABASE', 'USERNAME', 'SYSDBA');
    UIBBackup.PassWord := ini.ReadString('DATABASE', 'PASSWORD', 'masterkey');

    cbIgnoreChecksums.Checked := ini.ReadBool('BACKUP', 'IgnoreChecksums', false);
    cbIgnoreLimbo.Checked := ini.ReadBool('BACKUP', 'IgnoreLimbo', false);
    cbMetadataOnly.Checked := ini.ReadBool('BACKUP', 'MetadataOnly', false);
    cbNoGarbageCollection.Checked := ini.ReadBool('BACKUP', 'NoGarbageCollection', false);
    cbOldMetadataDesc.Checked := ini.ReadBool('BACKUP', 'OldMetadataDesc', false);
    cbNonTransportable.Checked := ini.ReadBool('BACKUP', 'NonTransportable', false);
    cbConvertExtTables.Checked := ini.ReadBool('BACKUP', 'ConvertExtTables', false);
    cbExpand.Checked := ini.ReadBool('BACKUP', 'Expand', false);
    cbVerbose.Checked := ini.ReadBool('BACKUP', 'Verbose', true);
    cbCloseWhenDone.Checked := ini.ReadBool('BACKUP', 'CloseWhenDone', false);
    rgCompression.ItemIndex := ini.ReadInteger('BACKUP', 'Compression', 0);
    cbLocalHost.Checked := ini.ReadBool('BACKUP', 'Localhost', false);
  finally
    ini.Free;
  end;
end;

procedure TBackupForm.ConfigChange(Sender: TObject);
var
  Options: TBackupOptions;
begin
  UIBBackup.BackupFiles.Text := edBackupFile.Text;
  options := [];
  if cbIgnoreChecksums.Checked then Include(options, boIgnoreChecksums);
  if cbIgnoreLimbo.Checked then Include(options, boIgnoreLimbo);
  if cbMetadataOnly.Checked then Include(options, boMetadataOnly);
  if cbNoGarbageCollection.Checked then Include(options, boNoGarbageCollection);
  if cbOldMetadataDesc.Checked then Include(options, boOldMetadataDesc);
  if cbNonTransportable.Checked then Include(options, boNonTransportable);
  if cbConvertExtTables.Checked then Include(options, boConvertExtTables);
  if cbExpand.Checked then Include(options, boExpand);
  UIBBackup.Options := options;

end;

procedure TBackupForm.UIBBackupVerbose(Sender: TObject; Message: String);
begin
  if cbVerbose.Checked then
  begin
    log.Lines.Add(Message);
    Application.ProcessMessages;
  end;
end;

procedure TBackupForm.btGOClick(Sender: TObject);
begin
  if cbLocalHost.Checked then
    UIBBackup.Protocol := proTCPIP else
    UIBBackup.Protocol := proLocalHost;

  log.Clear;
  btGO.Enabled := false;
  Screen.Cursor := crHourGlass;
  try
    UIBBackup.Run;
  finally
    Screen.Cursor := crDefault;
    btGO.Enabled := true;
  end;
  case rgCompression.ItemIndex of
    1: Run7Zip;
    2: RunWinRar;
    3: RunWinAce;
  end;
  if cbCloseWhenDone.Checked then Close;
end;

procedure TBackupForm.FormClose(Sender: TObject; var Action: TCloseAction);
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
      ini.WriteBool('BACKUP', 'IgnoreChecksums', cbIgnoreChecksums.Checked);
      ini.WriteBool('BACKUP', 'IgnoreLimbo', cbIgnoreLimbo.Checked);
      ini.WriteBool('BACKUP', 'MetadataOnly', cbMetadataOnly.Checked);
      ini.WriteBool('BACKUP', 'NoGarbageCollection', cbNoGarbageCollection.Checked);
      ini.WriteBool('BACKUP', 'OldMetadataDesc', cbOldMetadataDesc.Checked);
      ini.WriteBool('BACKUP', 'NonTransportable', cbNonTransportable.Checked);
      ini.WriteBool('BACKUP', 'ConvertExtTables', cbConvertExtTables.Checked);
      ini.WriteBool('BACKUP', 'Expand', cbExpand.Checked);
      ini.WriteBool('BACKUP', 'Verbose', cbVerbose.Checked);
      ini.WriteBool('BACKUP', 'CloseWhenDone', cbCloseWhenDone.Checked);
      ini.WriteInteger('BACKUP', 'Compression', rgCompression.ItemIndex);
      ini.WriteBool('BACKUP', 'Localhost', cbLocalHost.Checked);
    finally
      ini.Free;
    end;
  end;
  Action := caFree;
end;

procedure TBackupForm.btCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TBackupForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #13: if btGO.Enabled then btGO.Click;
    #27: Close;
  end;
  if (sender is TEdit) and (Key = #13) then abort;
end;

procedure TBackupForm.Run7Zip;
var
  exe, path, params: string;
begin
  path := ExtractFilePath(GetModuleName(HInstance));
  if FileExists(path + '7zG.exe') then
    exe := path + '7zG.exe' else
    begin
      path := FindComObjectPath(CLSID_7Zip);
      if FileExists(path + '7zG.exe') then
        exe := path + '7zG.exe' else
    end;
  if exe <> '' then
  begin
    path := ExtractFilePath(edBackupFile.Text);
    params := format('a "%s.7z" "%:0s" -ad', [edBackupFile.Text]);
    ShellExecute(Handle, 'open', PChar(exe), PChar(params), pchar(path), SW_SHOWNORMAL);
  end else
    MessageDlg('7-zip not found', mtWarning, [mbOK], 0);
end;

procedure TBackupForm.RunWinRar;
var
  exe, path, params: string;
begin
  path := FindComObjectPath(CLSID_Winrar);
  if FileExists(path + 'Winrar.exe') then
     exe := path + 'Winrar.exe';
  if exe <> '' then
  begin
    path := ExtractFilePath(edBackupFile.Text);
    params := format('a -ep1 -r0 -- . "%s"', [edBackupFile.Text]);
    ShellExecute(Handle, 'open', PChar(exe), PChar(params), PChar(path), SW_SHOWNORMAL);
  end else
    MessageDlg('Winrar not found', mtWarning, [mbOK], 0);
end;

procedure TBackupForm.RunWinAce;
var
  exe, path, params: string;
begin
  path := FindComObjectPath(CLSID_WinAce);
  if FileExists(path + 'winace.exe') then
     exe := path + 'winace.exe';
  if exe <> '' then
  begin
     path := ExtractFilePath(edBackupFile.Text);
    params := format('a "?" "%s"', [edBackupFile.Text]);
    ShellExecute(Handle, 'open', PChar(exe), PChar(params), PChar(path), SW_SHOWNORMAL);
  end else
    MessageDlg('Winace not found', mtWarning, [mbOK], 0);
end;

function TBackupForm.FindComObjectPath(guid: TGUID): string;
var
  reg: TRegistry;
begin
  Result := '';
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKey(format('\SOFTWARE\Classes\CLSID\%s\InProcServer32', [GUIDToString(guid)]), false) then
      Result := ExtractFilePath(reg.ReadString(''));
  finally
    reg.Free;
  end;
end;


end.


