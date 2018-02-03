unit UMain;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ActnList, ComCtrls, ImgList, Contnrs,
  JclCompression;

const
  WM_SYNC_ARCHIVE_PROGRESS = WM_USER + 153;

type
  TFormMain = class(TForm)
    ActionList1: TActionList;
    ActionOpenRO: TAction;
    ActionExtractSelectedRO: TAction;
    ActionExtractAllRO: TAction;
    ActionNewWO: TAction;
    ActionAddFile: TAction;
    ActionAddDirectory: TAction;
    ActionSave: TAction;
    ListView1: TListView;
    OpenDialogArchiveRO: TOpenDialog;
    SaveDialogArchiveWO: TSaveDialog;
    OpenDialogFile: TOpenDialog;
    ProgressBar1: TProgressBar;
    PageControl1: TPageControl;
    TabSheetReadOnly: TTabSheet;
    TabSheetWriteOnly: TTabSheet;
    TabSheetReadWrite: TTabSheet;
    ButtonOpen: TButton;
    ButtonExtractSelected: TButton;
    ButtonExtractAll: TButton;
    ButtonNew: TButton;
    ButtonAddFile: TButton;
    ButtonAddDirectory: TButton;
    ButtonSave: TButton;
    ActionDeleteRW: TAction;
    ActionNewRW: TAction;
    ActionOpenRW: TAction;
    ButtonNewRW: TButton;
    ButtonOpenRW: TButton;
    ButtonDeleteRW: TButton;
    ButtonAddFileRW: TButton;
    ButtonAddDirectoryRW: TButton;
    ButtonExtractSelectedRW: TButton;
    ButtonExtractAllRW: TButton;
    ButtonSaveRW: TButton;
    OpenDialogArchiveRW: TOpenDialog;
    SaveDialogArchiveRW: TSaveDialog;
    ButtonROProperties: TButton;
    ActionProperties: TAction;
    ButtonPropertiesWO: TButton;
    ButtonPropertiesRW: TButton;
    ButtonDescend: TButton;
    ActionDescendRO: TAction;
    ActionLevelUpRO: TAction;
    ButtonLevelUp: TButton;
    procedure ActionAlwaysEnabled(Sender: TObject);
    procedure ActionExtractSelectedROUpdate(Sender: TObject);
    procedure ActionExtractAllROUpdate(Sender: TObject);
    procedure ActionAddFileUpdate(Sender: TObject);
    procedure ActionAddDirectoryUpdate(Sender: TObject);
    procedure ActionSaveUpdate(Sender: TObject);
    procedure ActionNewWOExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionAddFileExecute(Sender: TObject);
    procedure ActionAddDirectoryExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionOpenROExecute(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ActionExtractAllROExecute(Sender: TObject);
    procedure ActionExtractSelectedROExecute(Sender: TObject);
    procedure ActionDeleteRWUpdate(Sender: TObject);
    procedure ActionDeleteRWExecute(Sender: TObject);
    procedure ActionNewRWExecute(Sender: TObject);
    procedure ActionOpenRWExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ActionPropertiesUpdate(Sender: TObject);
    procedure ActionPropertiesExecute(Sender: TObject);
    procedure ActionDescendROUpdate(Sender: TObject);
    procedure ActionLevelUpROUpdate(Sender: TObject);
    procedure ActionDescendROExecute(Sender: TObject);
    procedure ActionLevelUpROExecute(Sender: TObject);
  private
    FArchiveStack: TObjectList;
    FArchive: TJclCompressionArchive;
    procedure WmSyncArchiveProgress(var Message: TMessage); message WM_SYNC_ARCHIVE_PROGRESS;
  public
    procedure CloseArchive;
    procedure CloseAllArchive;
    procedure ArchiveProgress(Sender: TObject; const Value, MaxValue: Int64);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  Sevenzip, FileCtrl,
  JclAnsiStrings,
  UProperties;

function FileTimeToString(const FileTime: TFileTime): string;
var
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  if FileTimeToLocalFileTime(FileTime, LocalFileTime)
    and FileTimeToSystemTime(LocalFileTime, SystemTime) then
    Result := DateTimeToStr(EncodeDate(SystemTime.wYear, SystemTime.wMonth, SystemTime.wDay)
      + EncodeTime(SystemTime.wHour, SystemTime.wMinute, SystemTime.wSecond, SystemTime.wMilliseconds))
  else
    Result := '';
end;

procedure TFormMain.ActionAddDirectoryExecute(Sender: TObject);
var
  Directory: string;
begin
  if FileCtrl.SelectDirectory('Select directory', '', Directory {$IFDEF COMPILER9_UP} , [sdNewUI], Self {$ENDIF}) then
  begin
    (FArchive as TJclCompressArchive).AddDirectory(ExtractFileName(Directory), Directory, True, True);
    ListView1.Items.BeginUpdate;
    try
      while ListView1.Items.Count < FArchive.ItemCount do
        ListView1.Items.Add;
    finally
      ListView1.Items.EndUpdate;
    end;
  end;
end;

procedure TFormMain.ActionAddDirectoryUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FArchive is TJclCompressArchive) and FArchive.MultipleItemContainer;
end;

procedure TFormMain.ActionAddFileExecute(Sender: TObject);
begin
  if OpenDialogFile.Execute then
  begin
    (FArchive as TJclCompressArchive).AddFile(ExtractFileName(OpenDialogFile.FileName), OpenDialogFile.FileName);
    ListView1.Items.Add;
  end;
end;

procedure TFormMain.ActionAddFileUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FArchive is TJclCompressArchive)
    and (FArchive.MultipleItemContainer or (ListView1.Items.Count = 0));
end;

procedure TFormMain.ActionAlwaysEnabled(Sender: TObject);
begin
  (Sender as TAction).Enabled := True;
end;

procedure TFormMain.ActionDeleteRWExecute(Sender: TObject);
var
  Index: Integer;
begin
  for Index := ListView1.Items.Count - 1 downto 0 do
    if ListView1.Items[Index].Selected then
  begin
    (FArchive as TJclUpdateArchive).DeleteItem(Index);
    Break;
  end;

  ListView1.Items.Count := FArchive.ItemCount;
  ListView1.Invalidate;
end;

procedure TFormMain.ActionDeleteRWUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FArchive is TJclUpdateArchive) and (ListView1.SelCount = 1);
end;

procedure TFormMain.ActionDescendROExecute(Sender: TObject);
var
  ArchiveItem: TJclCompressionItem;
  ArchiveFileName: WideString;
  AFormat: TJclDecompressArchiveClass;
begin
  ArchiveItem := FArchive.Items[ListView1.ItemIndex];
  ArchiveFileName := ArchiveItem.NestedArchiveName;
  AFormat := GetArchiveFormats.FindDecompressFormat(ArchiveFileName);
  if AFormat <> nil then
  begin
    ListView1.Items.Clear;
    FArchiveStack.Add(FArchive);

    FArchive := AFormat.Create(ArchiveItem.NestedArchiveStream, 0, True);
    (FArchive as TJclDecompressArchive).ListFiles;

    ListView1.Items.BeginUpdate;
    try
      while ListView1.Items.Count < FArchive.ItemCount do
        ListView1.Items.Add;
    finally
      ListView1.Items.EndUpdate;
    end;
  end
  else
    ShowMessage('not a supported format');
end;

procedure TFormMain.ActionDescendROUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FArchive) and (FArchive.SupportsNestedArchive) and (ListView1.SelCount = 1);
end;

procedure TFormMain.ActionExtractAllROExecute(Sender: TObject);
var
  Directory: string;
begin
  if FileCtrl.SelectDirectory('Target directory', '', Directory {$IFDEF COMPILER9_UP} , [sdNewUI], Self {$ENDIF}) then
  begin
    if FArchive is TJclDecompressArchive then
      TJclDecompressArchive(FArchive).ExtractAll(Directory, True)
    else
    if FArchive is TJclUpdateArchive then
      TJclUpdateArchive(FArchive).ExtractAll(Directory, True);
  end;
end;

procedure TFormMain.ActionExtractAllROUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FArchive is TJclDecompressArchive) or (FArchive is TJclUpdateArchive);
end;

procedure TFormMain.ActionExtractSelectedROExecute(Sender: TObject);
var
  Directory: string;
  Index: Integer;
begin
  if FileCtrl.SelectDirectory('Target directory', '', Directory {$IFDEF COMPILER9_UP} , [sdNewUI], Self {$ENDIF}) then
  begin
    for Index := 0 to ListView1.Items.Count - 1 do
      FArchive.Items[Index].Selected := ListView1.Items.Item[Index].Selected;

    if FArchive is TJclDecompressArchive then
      TJclDecompressArchive(FArchive).ExtractSelected(Directory, True)
    else
    if FArchive is TJclUpdateArchive then
      TJclUpdateArchive(FArchive).ExtractSelected(Directory, True);
  end;
end;

procedure TFormMain.ActionExtractSelectedROUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ((FArchive is TJclDecompressArchive) or (FArchive is TJclUpdateArchive))
    and (ListView1.SelCount > 0);
end;

procedure TFormMain.ActionLevelUpROExecute(Sender: TObject);
begin
  CloseArchive;
end;

procedure TFormMain.ActionLevelUpROUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FArchiveStack.Count > 0;
end;

procedure TFormMain.ActionNewWOExecute(Sender: TObject);
var
  ArchiveFileName, VolumeSizeStr, Password: string;
  AFormat: TJclCompressArchiveClass;
  VolumeSize: Int64;
  Code: Integer;
begin
  if SaveDialogArchiveWO.Execute then
  begin
    CloseAllArchive;

    ArchiveFileName := SaveDialogArchiveWO.FileName;

    AFormat := GetArchiveFormats.FindCompressFormat(ArchiveFileName);

    if AFormat <> nil then
    begin
      VolumeSizeStr := '0';
      repeat
        if InputQuery('Split archive?', 'Volume size in byte:', VolumeSizeStr) then
          Val(VolumeSizeStr, VolumeSize, Code)
        else
        begin
          VolumeSize := 0;
          Code := 0;
        end;
      until Code = 0;

      InputQuery('Archive password', 'Value', Password);

      if VolumeSize <> 0 then
        ArchiveFileName := ArchiveFileName + '.%.3d';

      FArchive := AFormat.Create(ArchiveFileName, VolumeSize, VolumeSize <> 0);
      FArchive.Password := Password;
      FArchive.OnProgress := ArchiveProgress;
    end
    else
      ShowMessage('not a supported format');
  end;
end;

procedure TFormMain.ActionNewRWExecute(Sender: TObject);
var
  ArchiveFileName, VolumeSizeStr, Password: string;
  AFormat: TJclUpdateArchiveClass;
  VolumeSize: Int64;
  Code: Integer;
begin
  if SaveDialogArchiveRW.Execute then
  begin
    CloseAllArchive;

    ArchiveFileName := SaveDialogArchiveRW.FileName;

    AFormat := GetArchiveFormats.FindUpdateFormat(ArchiveFileName);

    if AFormat <> nil then
    begin
      VolumeSizeStr := '0';
      repeat
        if InputQuery('Split archive?', 'Volume size in byte:', VolumeSizeStr) then
          Val(VolumeSizeStr, VolumeSize, Code)
        else
        begin
          VolumeSize := 0;
          Code := 0;
        end;
      until Code = 0;

      InputQuery('Archive password', 'Value', Password);

      if VolumeSize <> 0 then
        ArchiveFileName := ArchiveFileName + '.%.3d';

      FArchive := AFormat.Create(ArchiveFileName, VolumeSize, VolumeSize <> 0);
      FArchive.Password := Password;
      FArchive.OnProgress := ArchiveProgress;
    end
    else
      ShowMessage('not a supported format');
  end;
end;

procedure TFormMain.ActionOpenROExecute(Sender: TObject);
var
  ArchiveFileName, Password: string;
  AFormats: TJclDecompressArchiveClassArray;
  SplitArchive: Boolean;
  Index: Integer;
begin
  if OpenDialogArchiveRO.Execute then
  begin
    CloseAllArchive;

    ArchiveFileName := OpenDialogArchiveRO.FileName;
    SplitArchive := AnsiSameText(ExtractFileExt(ArchiveFileName), '.001');
    if SplitArchive then
      ArchiveFileName := ChangeFileExt(ArchiveFileName, '');

    AFormats := GetArchiveFormats.FindDecompressFormats(ArchiveFileName);

    if Length(AFormats) > 0 then
    begin
      if SplitArchive then
        ArchiveFileName := ArchiveFileName + '.%.3d';

      InputQuery('Archive password', 'Value', Password);
    end;

    for Index := Low(AFormats) to High(AFormats) do
    begin
      FArchive := AFormats[Index].Create(ArchiveFileName, 0, SplitArchive);
      try
        FArchive.Password := Password;
        FArchive.OnProgress := ArchiveProgress;

        if FArchive is TJclDecompressArchive then
          TJclDecompressArchive(FArchive).ListFiles
        else
        if FArchive is TJclUpdateArchive then
          TJclUpdateArchive(FArchive).ListFiles;

        ListView1.Items.BeginUpdate;
        try
          while ListView1.Items.Count < FArchive.ItemCount do
            ListView1.Items.Add;
        finally
          ListView1.Items.EndUpdate;
        end;
        Break;
      except
        CloseAllArchive;
      end;
    end;

    if not Assigned(FArchive) then
      ShowMessage('not a supported format');
  end;
end;

procedure TFormMain.ActionOpenRWExecute(Sender: TObject);
var
  ArchiveFileName, Password: string;
  AFormats: TJclUpdateArchiveClassArray;
  SplitArchive: Boolean;
  Index: Integer;
begin
  if OpenDialogArchiveRW.Execute then
  begin
    CloseAllArchive;

    ArchiveFileName := OpenDialogArchiveRW.FileName;
    SplitArchive := AnsiSameText(ExtractFileExt(ArchiveFileName), '.001');
    if SplitArchive then
      ArchiveFileName := ChangeFileExt(ArchiveFileName, '');

    AFormats := GetArchiveFormats.FindUpdateFormats(ArchiveFileName);

    if Length(AFormats) > 0 then
    begin
      if SplitArchive then
        ArchiveFileName := ArchiveFileName + '.%.3d';

      InputQuery('Archive password', 'Value', Password);
    end;

    for Index := Low(AFormats) to High(AFormats) do
    begin
      FArchive := AFormats[Index].Create(ArchiveFileName, 0, SplitArchive);
      try
        FArchive.Password := Password;
        FArchive.OnProgress := ArchiveProgress;

        if FArchive is TJclDecompressArchive then
          TJclDecompressArchive(FArchive).ListFiles
        else
        if FArchive is TJclUpdateArchive then
          TJclUpdateArchive(FArchive).ListFiles;

        ListView1.Items.BeginUpdate;
        try
          while ListView1.Items.Count < FArchive.ItemCount do
            ListView1.Items.Add;
        finally
          ListView1.Items.EndUpdate;
        end;
        Break;
      except
        CloseAllArchive;
      end;
    end;
    if not Assigned(FArchive) then
      ShowMessage('not a supported format');
  end;
end;

procedure TFormMain.ActionPropertiesExecute(Sender: TObject);
begin
  TFormArchiveSettings.Execute(FArchive);
end;

procedure TFormMain.ActionPropertiesUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FArchive);
end;

procedure TFormMain.ActionSaveExecute(Sender: TObject);
begin
  (FArchive as TJclCompressArchive).Compress;
  CloseAllArchive;
end;

procedure TFormMain.ActionSaveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FArchive is TJclCompressArchive) and (ListView1.Items.Count > 0);
end;

procedure TFormMain.ArchiveProgress(Sender: TObject; const Value, MaxValue: Int64);
var
  MyValue, MyMaxValue: Int64;
begin
  MyValue := Value;
  MyMaxValue := MaxValue;

  while MyMaxValue > High(Byte) do
  begin
    MyMaxValue := MyMaxValue shr 8;
    MyValue := MyValue shr 8;
  end;

  PostMessage(Handle, WM_SYNC_ARCHIVE_PROGRESS, MyValue, MyMaxValue);
end;

procedure TFormMain.CloseAllArchive;
begin
  while Assigned(FArchive) do
    CloseArchive;
end;

procedure TFormMain.CloseArchive;
begin
  ListView1.Items.Clear;
  FreeAndNil(FArchive);
  if FArchiveStack.Count > 0 then
  begin
    FArchive := FArchiveStack.Items[FArchiveStack.Count - 1] as TJclCompressionArchive;
    FArchiveStack.Count := FArchiveStack.Count - 1;
  end;
  if Assigned(FArchive) then
  begin
    ListView1.Items.BeginUpdate;
    try
      while ListView1.Items.Count < FArchive.ItemCount do
        ListView1.Items.Add;
    finally
      ListView1.Items.EndUpdate;
    end;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
  procedure MergeFilters(var AFilter, AllExtensions: string; AFormat: TJclCompressionArchiveClass);
  var
    AName, AExtensions: string;
  begin
    AName := AFormat.ArchiveName;
    AExtensions := AFormat.ArchiveExtensions;
    if AFilter = '' then
      AFilter := Format('%0:s (%1:s)|%1:s', [AName, AExtensions])
    else
      AFilter := Format('%0:s|%1:s (%2:s)|%2:s', [AFilter, AName, AExtensions]);
    if AllExtensions = '' then
      AllExtensions := AExtensions
    else
      AllExtensions := Format('%s;%s', [AllExtensions, AExtensions]);
  end;
  function AddStandardFilters(const AFilter, AllExtensions: string): string;
  begin
    if AFilter = '' then
      Result := ''
    else
      Result := Format('All supported formats|(%0:s)|%1:s', [AllExtensions, AFilter]);
  end;
var
  AFilter, AllExtensions: string;
  AFormats: TJclCompressionArchiveFormats;
  Index: Integer;
begin
  FArchiveStack := TObjectList.Create(False);

  AFormats := GetArchiveFormats;

  AFilter := '';
  AllExtensions := '';
  for Index := 0 to AFormats.CompressFormatCount - 1 do
    MergeFilters(AFilter, AllExtensions, AFormats.CompressFormats[Index]);
  SaveDialogArchiveWO.Filter := AFilter;

  AFilter := '';
  AllExtensions := '';
  for Index := 0 to AFormats.UpdateFormatCount - 1 do
    MergeFilters(AFilter, AllExtensions, AFormats.UpdateFormats[Index]);
  SaveDialogArchiveRW.Filter := AFilter;

  AFilter := '';
  AllExtensions := '';
  for Index := 0 to AFormats.DecompressFormatCount - 1 do
    MergeFilters(AFilter, AllExtensions, AFormats.DecompressFormats[Index]);
  OpenDialogArchiveRO.Filter := AddStandardFilters(AFilter, AllExtensions);

  AFilter := '';
  AllExtensions := '';
  for Index := 0 to AFormats.UpdateFormatCount - 1 do
    MergeFilters(AFilter, AllExtensions, AFormats.UpdateFormats[Index]);
  OpenDialogArchiveRW.Filter := AddStandardFilters(AFilter, AllExtensions);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  CloseAllArchive;
  FArchiveStack.Free;
end;

procedure TFormMain.ListView1Data(Sender: TObject; Item: TListItem);
var
  CompressionItem: TJclCompressionItem;
begin
  if not Assigned(FArchive) then
  begin
    Item.Caption := '';
    Item.SubItems.Clear;
    Exit;
  end;

  CompressionItem := FArchive.Items[Item.Index];

  Item.Caption := CompressionItem.FileName;
  Item.SubItems.Clear;
  if ipPackedName in CompressionItem.ValidProperties then
    Item.SubItems.Add(CompressionItem.PackedName)
  else
  if ipPackedExtension in CompressionItem.ValidProperties then
    Item.SubItems.Add('(Extension)' + CompressionItem.PackedExtension)
  else
    Item.SubItems.Add('(Auto)');
  if ipFileSize in CompressionItem.ValidProperties then
    Item.SubItems.Add(IntToStr(CompressionItem.FileSize))
  else
    Item.SubItems.Add('');
  if ipPackedSize in CompressionItem.ValidProperties then
    Item.SubItems.Add(IntToStr(CompressionItem.PackedSize))
  else
    Item.SubItems.Add('');
  if ipCreationTime in CompressionItem.ValidProperties then
    Item.SubItems.Add(FileTimeToString(CompressionItem.CreationTime))
  else
    Item.SubItems.Add('');
  if ipLastAccessTime in CompressionItem.ValidProperties then
    Item.SubItems.Add(FileTimeToString(CompressionItem.LastAccessTime))
  else
    Item.SubItems.Add('');
  if ipLastWriteTime in CompressionItem.ValidProperties then
    Item.SubItems.Add(FileTimeToString(CompressionItem.LastWriteTime))
  else
    Item.SubItems.Add('');
  if ipComment in CompressionItem.ValidProperties then
    Item.SubItems.Add(CompressionItem.Comment)
  else
    Item.SubItems.Add('');
  if ipHostOS in CompressionItem.ValidProperties then
    Item.SubItems.Add(CompressionItem.HostOS)
  else
    Item.SubItems.Add('');
  if ipHostFS in CompressionItem.ValidProperties then
    Item.SubItems.Add(CompressionItem.HostFS)
  else
    Item.SubItems.Add('');
  if ipUser in CompressionItem.ValidProperties then
    Item.SubItems.Add(CompressionItem.User)
  else
    Item.SubItems.Add('');
  if ipGroup in CompressionItem.ValidProperties then
    Item.SubItems.Add(CompressionItem.Group)
  else
    Item.SubItems.Add('');
  if ipCRC in CompressionItem.ValidProperties then
    Item.SubItems.Add(IntToHex(CompressionItem.CRC, 8))
  else
    Item.SubItems.Add('');
end;

procedure TFormMain.WMSyncArchiveProgress(var Message: TMessage);
begin
  ProgressBar1.Max := Message.LParam;
  ProgressBar1.Position := Message.WParam;
end;

initialization

  if not Load7Zip then
    raise EJclCompressionError.Create('Cannot load sevenzip library');

end.
