unit UProperties;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JclCompression, ActnList;

type
  TFormArchiveSettings = class(TForm)
    GroupBoxGeneralSettings: TGroupBox;
    EditPassword: TEdit;
    LabelPassword: TLabel;
    EditNumberOfThreads: TEdit;
    LabelNumberOfThreads: TLabel;
    GroupBoxCompressionProperties: TGroupBox;
    EditCompressionLevel: TEdit;
    LabelCompressionLevel: TLabel;
    ComboBoxCompressionMethod: TComboBox;
    LabelCompressionMethod: TLabel;
    ComboBoxEncryptionMethod: TComboBox;
    LabelEncryptionMethod: TLabel;
    EditDictionarySize: TEdit;
    LabelDictionarySize: TLabel;
    EditNumberOfPasses: TEdit;
    LabelNumberOfPasses: TLabel;
    GroupBox1: TGroupBox;
    CheckBoxRemoveSfxBlock: TCheckBox;
    CheckBoxCompressHeader: TCheckBox;
    CheckBoxCompressHeaderFull: TCheckBox;
    CheckBoxEncryptHeader: TCheckBox;
    CheckBoxSaveCreationDateTime: TCheckBox;
    CheckBoxSaveLastAccessDateTime: TCheckBox;
    CheckBoxSaveLastSaveDateTime: TCheckBox;
    ButtonClose: TButton;
    CheckBoxSolidExtension: TCheckBox;
    LabelSolidBlockSize: TLabel;
    EditSolidBlockSize: TEdit;
    procedure EditPasswordExit(Sender: TObject);
    procedure EditNumberOfThreadsExit(Sender: TObject);
    procedure EditCompressionLevelExit(Sender: TObject);
    procedure ComboBoxCompressionMethodExit(Sender: TObject);
    procedure ComboBoxEncryptionMethodChange(Sender: TObject);
    procedure EditDictionarySizeExit(Sender: TObject);
    procedure EditNumberOfPassesExit(Sender: TObject);
    procedure CheckBoxRemoveSfxBlockExit(Sender: TObject);
    procedure CheckBoxCompressHeaderExit(Sender: TObject);
    procedure CheckBoxCompressHeaderFullExit(Sender: TObject);
    procedure CheckBoxEncryptHeaderExit(Sender: TObject);
    procedure CheckBoxSaveCreationDateTimeExit(Sender: TObject);
    procedure CheckBoxSaveLastAccessDateTimeExit(Sender: TObject);
    procedure CheckBoxSaveLastSaveDateTimeExit(Sender: TObject);
    procedure CheckBoxSolidExtensionExit(Sender: TObject);
    procedure EditSolidBlockSizeExit(Sender: TObject);
  protected
    FArchive: TJclCompressionArchive;
    FNumberOfThreads: IJclArchiveNumberOfThreads;
    FCompressionLevel: IJclArchiveCompressionLevel;
    FCompressionMethod: IJclArchiveCompressionMethod;
    FEncryptionMethod: IJclArchiveEncryptionMethod;
    FDictionarySize: IJclArchiveDictionarySize;
    FNumberOfPasses: IJclArchiveNumberOfPasses;
    FRemoveSfxBlock: IJclArchiveRemoveSfxBlock;
    FCompressHeader: IJclArchiveCompressHeader;
    FEncryptHeader: IJclArchiveEncryptHeader;
    FSaveCreationDateTime: IJclArchiveSaveCreationDateTime;
    FSaveLastAccessDateTime: IJclArchiveSaveLastAccessDateTime;
    FSaveLastWriteDateTime: IJclArchiveSaveLastWriteDateTime;
    FAlgoritm: IJclArchiveAlgorithm;
    FSolid: IJclArchiveSolid;
  public
    class procedure Execute(Archive: TJclCompressionArchive);
    procedure RefreshValues;
  end;

implementation

{$R *.dfm}

uses
  TypInfo;

procedure TFormArchiveSettings.CheckBoxCompressHeaderExit(Sender: TObject);
begin
  FCompressHeader.CompressHeader := CheckBoxCompressHeader.Checked;
  RefreshValues;
end;

procedure TFormArchiveSettings.CheckBoxCompressHeaderFullExit(Sender: TObject);
begin
  FCompressHeader.CompressHeaderFull := CheckBoxCompressHeaderFull.Checked;
  RefreshValues;
end;

procedure TFormArchiveSettings.CheckBoxEncryptHeaderExit(Sender: TObject);
begin
  FEncryptHeader.EncryptHeader := CheckBoxEncryptHeader.Checked;
  RefreshValues;
end;

procedure TFormArchiveSettings.CheckBoxRemoveSfxBlockExit(Sender: TObject);
begin
  FRemoveSfxBlock.RemoveSfxBlock := CheckBoxRemoveSfxBlock.Checked;
  RefreshValues;
end;

procedure TFormArchiveSettings.CheckBoxSaveCreationDateTimeExit(Sender: TObject);
begin
  FSaveCreationDateTime.SaveCreationDateTime := CheckBoxSaveCreationDateTime.Checked;
  RefreshValues;
end;

procedure TFormArchiveSettings.CheckBoxSaveLastAccessDateTimeExit(Sender: TObject);
begin
  FSaveLastAccessDateTime.SaveLastAccessDateTime := CheckBoxSaveLastAccessDateTime.Checked;
  RefreshValues;
end;

procedure TFormArchiveSettings.CheckBoxSaveLastSaveDateTimeExit(Sender: TObject);
begin
  FSaveLastWriteDateTime.SaveLastWriteDateTime := CheckBoxSaveLastSaveDateTime.Checked;
  RefreshValues;
end;

procedure TFormArchiveSettings.CheckBoxSolidExtensionExit(Sender: TObject);
begin
  FSolid.SolidExtension := CheckBoxSolidExtension.Checked;
  RefreshValues;
end;

procedure TFormArchiveSettings.ComboBoxCompressionMethodExit(Sender: TObject);
begin
  FCompressionMethod.CompressionMethod := TJclCompressionMethod(GetEnumValue(TypeInfo(TJclCompressionMethod),ComboBoxCompressionMethod.Text));
  RefreshValues;
end;

procedure TFormArchiveSettings.ComboBoxEncryptionMethodChange(Sender: TObject);
begin
  FEncryptionMethod.EncryptionMethod := TJclEncryptionMethod(GetEnumValue(TypeInfo(TJclEncryptionMethod),ComboBoxEncryptionMethod.Text));
  RefreshValues;
end;

procedure TFormArchiveSettings.EditCompressionLevelExit(Sender: TObject);
begin
  FCompressionLevel.CompressionLevel := StrToInt(EditCompressionLevel.Text);
  RefreshValues;
end;

procedure TFormArchiveSettings.EditDictionarySizeExit(Sender: TObject);
begin
  FDictionarySize.DictionarySize := StrToInt(EditDictionarySize.Text);
  RefreshValues;
end;

procedure TFormArchiveSettings.EditNumberOfPassesExit(Sender: TObject);
begin
  FNumberOfPasses.NumberOfPasses := StrToInt(EditNumberOfPasses.Text);
  RefreshValues;
end;

procedure TFormArchiveSettings.EditNumberOfThreadsExit(Sender: TObject);
begin
  FNumberOfThreads.NumberOfThreads := StrToInt(EditNumberOfThreads.Text);
  RefreshValues;
end;

procedure TFormArchiveSettings.EditPasswordExit(Sender: TObject);
begin
  FArchive.Password := EditPassword.Text;
  RefreshValues;
end;

procedure TFormArchiveSettings.EditSolidBlockSizeExit(Sender: TObject);
begin
  FSolid.SolidBlockSize := StrToInt64(EditSolidBlockSize.Text);
  RefreshValues;
end;

class procedure TFormArchiveSettings.Execute(Archive: TJclCompressionArchive);
var
  AFormSettings: TFormArchiveSettings;
  CompressionMethod: TJclCompressionMethod;
  EncryptionMethod: TJclEncryptionMethod;
begin
  AFormSettings := TFormArchiveSettings.Create(Application);
  try
    Supports(IUnknown(Archive),IJclArchiveNumberOfThreads,AFormSettings.FNumberOfThreads);
    Supports(IUnknown(Archive),IJclArchiveCompressionLevel,AFormSettings.FCompressionLevel);
    Supports(IUnknown(Archive),IJclArchiveCompressionMethod,AFormSettings.FCompressionMethod);
    Supports(IUnknown(Archive),IJclArchiveEncryptionMethod,AFormSettings.FEncryptionMethod);
    Supports(IUnknown(Archive),IJclArchiveDictionarySize,AFormSettings.FDictionarySize);
    Supports(IUnknown(Archive),IJclArchiveNumberOfPasses,AFormSettings.FNumberOfPasses);
    Supports(IUnknown(Archive),IJclArchiveRemoveSfxBlock,AFormSettings.FRemoveSfxBlock);
    Supports(IUnknown(Archive),IJclArchiveCompressHeader,AFormSettings.FCompressHeader);
    Supports(IUnknown(Archive),IJclArchiveEncryptHeader,AFormSettings.FEncryptHeader);
    Supports(IUnknown(Archive),IJclArchiveSaveCreationDateTime,AFormSettings.FSaveCreationDateTime);
    Supports(IUnknown(Archive),IJclArchiveSaveLastAccessDateTime,AFormSettings.FSaveLastAccessDateTime);
    Supports(IUnknown(Archive),IJclArchiveSaveLastWriteDateTime,AFormSettings.FSaveLastWriteDateTime);
    Supports(IUnknown(Archive),IJclArchiveSolid,AFormSettings.FSolid);
    AFormSettings.FArchive := Archive;

    if Assigned(AFormSettings.FCompressionLevel) then
      AFormSettings.LabelCompressionLevel.Caption := Format(AFormSettings.LabelCompressionLevel.Caption,
        [AFormSettings.FCompressionLevel.CompressionLevelMin,AFormSettings.FCompressionLevel.CompressionLevelMax])
    else
      AFormSettings.LabelCompressionLevel.Caption := Format(AFormSettings.LabelCompressionLevel.Caption,[0,0]);

    if Assigned(AFormSettings.FCompressionMethod) then
      for CompressionMethod := Low(TJclCompressionMethod) to High(TJclCompressionMethod) do
        if CompressionMethod in AFormSettings.FCompressionMethod.SupportedCompressionMethods then
          AFormSettings.ComboBoxCompressionMethod.Items.Add(GetEnumName(TypeInfo(TJclCompressionMethod),Integer(CompressionMethod)));

    if Assigned(AFormSettings.FEncryptionMethod) then
      for EncryptionMethod := Low(TJclEncryptionMethod) to High(TJclEncryptionMethod) do
        if EncryptionMethod in AFormSettings.FEncryptionMethod.SupportedEncryptionMethods then
          AFormSettings.ComboBoxEncryptionMethod.Items.Add(GetEnumName(TypeInfo(TJclEncryptionMethod),Integer(EncryptionMethod)));

    AFormSettings.RefreshValues;
    AFormSettings.ShowModal;
  finally
    AFormSettings.Free;
  end;
end;

procedure TFormArchiveSettings.RefreshValues;
begin
  // password
  EditPassword.Text := FArchive.Password;
  // number of threads
  EditNumberOfThreads.Enabled := Assigned(FNumberOfThreads);
  if Assigned(FNumberOfThreads) then
    EditNumberOfThreads.Text := IntToStr(FNumberOfThreads.NumberOfThreads);
  // compression level
  EditCompressionLevel.Enabled := Assigned(FCompressionLevel);
  if Assigned(FCompressionLevel) then
    EditCompressionLevel.Text := IntToStr(FCompressionLevel.CompressionLevel);
  // compression method
  if Assigned(FCompressionMethod) then
    ComboBoxCompressionMethod.ItemIndex := ComboBoxCompressionMethod.Items.IndexOf(GetEnumName(TypeInfo(TJclCompressionMethod),Integer(FCompressionMethod.CompressionMethod)))
  else
    ComboBoxCompressionMethod.Enabled := False;
  // encryption method
  if Assigned(FEncryptionMethod) then
    ComboBoxEncryptionMethod.ItemIndex := ComboBoxEncryptionMethod.Items.IndexOf(GetEnumName(TypeInfo(TJclEncryptionMethod),Integer(FEncryptionMethod.EncryptionMethod)))
  else
    ComboBoxEncryptionMethod.Enabled := False;
  // dictionary size
  if Assigned(FDictionarySize) then
    EditDictionarySize.Text := IntToStr(FDictionarySize.DictionarySize)
  else
    EditDictionarySize.Enabled := False;
  // number of passes
  if Assigned(FNumberOfPasses) then
    EditNumberOfPasses.Text := IntToStr(FNumberOfPasses.NumberOfPasses)
  else
    EditNumberOfPasses.Enabled := False;
  // solid block size
  if Assigned(FSolid) then
    EditSolidBlockSize.Text := IntToStr(FSolid.SolidBlockSize)
  else
    EditSolidBlockSize.Enabled := False;
  // remove sfx
  CheckBoxRemoveSfxBlock.Enabled := Assigned(FRemoveSfxBlock);
  CheckBoxRemoveSfxBlock.Checked := Assigned(FRemoveSfxBlock) and FRemoveSfxBlock.RemoveSfxBlock;
  // compress header
  CheckBoxCompressHeader.Enabled := Assigned(FCompressHeader);
  CheckBoxCompressHeader.Checked := Assigned(FCompressHeader) and FCompressHeader.CompressHeader;
  // compress header full
  CheckBoxCompressHeaderFull.Enabled := Assigned(FCompressHeader);
  CheckBoxCompressHeaderFull.Checked := Assigned(FCompressHeader) and FCompressHeader.CompressHeaderFull;
  // encrypt header
  CheckBoxEncryptHeader.Enabled := Assigned(FEncryptHeader);
  CheckBoxEncryptHeader.Checked := Assigned(FEncryptHeader) and FEncryptHeader.EncryptHeader;
  // save creation date time
  CheckBoxSaveCreationDateTime.Enabled := Assigned(FSaveCreationDateTime);
  CheckBoxSaveCreationDateTime.Checked := Assigned(FSaveCreationDateTime) and FSaveCreationDateTime.SaveCreationDateTime;
  // save last access date time
  CheckBoxSaveLastAccessDateTime.Enabled := Assigned(FSaveLastAccessDateTime);
  CheckBoxSaveLastAccessDateTime.Checked := Assigned(FSaveLastAccessDateTime) and FSaveLastAccessDateTime.SaveLastAccessDateTime;
  // save last write date time
  CheckBoxSaveLastSaveDateTime.Enabled := Assigned(FSaveLastWriteDateTime);
  CheckBoxSaveLastSaveDateTime.Checked := Assigned(FSaveLastWriteDateTime) and FSaveLastWriteDateTime.SaveLastWriteDateTime;
  // solid by extension
  CheckBoxSolidExtension.Enabled := Assigned(FSolid);
  CheckBoxSolidExtension.Checked := Assigned(FSolid) and FSolid.SolidExtension;
end;

end.

