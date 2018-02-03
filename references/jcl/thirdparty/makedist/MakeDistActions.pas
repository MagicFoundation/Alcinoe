unit MakeDistActions;

// unit containing standard actions
// Copyright (c) Florent Ouchet, February 2008

interface

uses
  Windows, SysUtils, Classes,
  JclSysUtils, JclStreams,
  MakeDistMain;

type
  // to be assumed:
  //  - all configuration values may contain environment variables to be expanded (ex: $(SYSTEMDIR)\cmd.exe)
  //  - all execute functions are jailed in a try..except..end block trapping exceptions
  //  - all execute functions are executed with working directory set to the directory where the configuration file is

  // change end of line style for all files in a directory whose name matches Filter
  // Filter may be a semi-colon-separated of masks (ex: *.pas;*.html)
  // NewEOL is CRLF (Windows style), LF (old mac style) or CR (unix style)
  TEolConverter = class(TDistAction)
  private
    FDirectory: string;
    FFilter: string;
    FNewEOL: string;
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

  // copy files matching Filter from a directory to an other
  // Filter may be a semi-colon-separated of masks (ex: *.pas;*.html)
  TFileCopier = class(TDistAction)
  private
    FFromDirectory: string;
    FFilter: string;
    FToDirectory: string;
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

  // create a file with the specified content
  // if the file already exists, it's erased and the rewritten
  TFileCreator = class(TDistAction)
  private
    FFileName: string;
    FContent: string;
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

  // rename a file
  TFileMover = class(TDistAction)
  private
    FOldFileName: string;
    FNewFileName: string;
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

  // remove files matching Filter from a directory
  // if MoveToRecycleBin is set to 'yes', they are moved to the recyclebin, otherwise they are deleted
  // Filter may be a semi-colon-separated of masks (ex: *.pas;*.html)
  TFileRemover = class(TDistAction)
  private
    FDirectory: string;
    FFilter: string;
    FMoveToRecycleBin: string;
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

  // touch this file (set last modification time to now)
  // if the file doesn't exist, it's created
  // this function also support existing directories
  TFileTouch = class(TDistAction)
  private
    FPath: string;
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

  // create a directory
  TDirectoryCreator = class(TDistAction)
  private
    FDirectory: string;
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

  // remove a directory and all its files/subdirectories
  // if MoveToRecycleBin is set to 'yes', they are moved to the recyclebin, otherwise they are deleted
  TDirectoryRemover = class(TDistAction)
  private
    FDirectory: string;
    FMoveToRecycleBin: string;
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

  // parse a source file looking for true constant declaration (ex: MyConst = 256)
  // supported types are numeric (either in decimal or hexadecimal radix
  // comments are ignored (ie if the constant pattern is matched in a comment it will be returned)
  TConstantParser = class(TDistAction)
  private
    FSourceFile: string;
    FConstantName: string;
    FEnvironmentVariable: string;
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

  // load an environment variable from a file
  TVariableReader = class(TDistAction)
  private
    FSourceFile: string;
    FEnvironmentVariable: string;
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

  // compute an environment variable
  TVariableSetter = class(TDistAction)
  private
    FExpression: string;
    FEnvironmentVariable: string;
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

  // save an environment variable to a file
  TVariableWriter = class(TDistAction)
  private
    FDestinationFile: string;
    FExpression: string;
    FAppend: string;
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

  // compute the number of days between now and a start date
  TBuildCalculator = class(TDistAction)
  private
    FYear: string;
    FMonth: string;
    FDay: string;
    FEnvironmentVariable: string;
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

  // retreive a value from an xml file or the xml output of an executable file
  // WorkingDirectory and Arguments are used in case of an executable file
  // ValidExitCodes is a semicolon-separated list of integer values
  // the xml content is parsed according to the key parameter
  // ie: node1\node2&3\node3\\prop1 will return the value of the "prop1" property
  //   of the node named "node3" located in the third node named "node2" of the root node named "node1"
  // node1\node2\node3 will load the value of the node named "node3" located in the first node named "node2"
  //   of the root node named "node1"
  // the result is stored in the specificed environment variable
  TXmlGetter = class(TDistAction)
  private
    FXmlExe: string;
    FWorkingDirectory: string;
    FArguments: string;
    FValidExitCodes: string;
    FKey: string;
    FEnvironmentVariable: string;
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

  // call the specified program with arguments in a working directory
  // ValidExitCodes is a semicolon-separated list of integer values
  TCommandLineCaller = class(TDistAction)
  private
    FApplication: string;
    FWorkingDirectory: string;
    FParameters: string;
    FValidExitCodes: string;
    FResultFile: string;
    FResultStringStream: TJclAnsiStream;
    procedure WriteResult(const Text: string);
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

  // create an archive, the compressor is determined based on the extension of the archive
  // all items matching Filter in Directory will be added
  TArchiveMaker = class(TDistAction)
  private
    FDirectory: string;
    FFilter: string;
    FArchive: string;
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

  // clean the log
  TLogCleaner = class(TDistAction)
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

  // save the log to filename, if append is "yes", then the log is appended to the content of the file if any
  TLogSaver = class(TDistAction)
  private
    FFileName: string;
    FAppend: string;
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

  // arbitrary delay
  TDelay = class(TDistAction)
  private
    FDelay: string;
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

  // call an other task
  TSubTask = class(TDistAction)
  private
    FTaskName: string;
  protected
    function GetCaption: string; override;
    function GetConfigCount: Integer; override;
    function GetConfigCaption(Index: Integer): string; override;
    function GetConfigValue(Index: Integer): string; override;
    procedure SetConfigValue(Index: Integer; const Value: string); override;
  public
    class function GetDescription: string; override;
    function Execute(const AMessageHandler: TTextHandler): Boolean; override;
  end;

implementation

uses
  DateUtils, JclDateTime, JclStrings, JclFileUtils, JclSysInfo, JclSimpleXml, JclCompression;

const
  StdActionsClasses: array [0..19] of TDistActionClass =
    ( TBuildCalculator, TConstantParser,
      TVariableReader, TVariableSetter, TVariableWriter,
      TDirectoryCreator, TDirectoryRemover, TEolConverter,
      TFileCopier, TFileCreator, TFileMover, TFileRemover, TFileTouch,
      TXmlGetter, TCommandLineCaller, TArchiveMaker,
      TLogSaver, TLogCleaner, TDelay, TSubTask );

procedure RegisterStandardActions;
var
  DistActions: TDistActions;
  Index: Integer;
begin
  DistActions := GetDistActions;

  for Index := Low(StdActionsClasses) to High(StdActionsClasses) do
    DistActions.RegisterAction(StdActionsClasses[Index]);
end;

procedure UnregisterStandardActions;
var
  DistActions: TDistActions;
  Index: Integer;
begin
  DistActions := GetDistActions;

  for Index := Low(StdActionsClasses) to High(StdActionsClasses) do
    DistActions.UnregisterAction(StdActionsClasses[Index]);
end;

//=== { TEolConverter } ======================================================

function TEolConverter.Execute(const AMessageHandler: TTextHandler): Boolean;
var
  FileList: TStrings;
  Index: Integer;
  TextFile: TJclAnsiMappedTextReader;
  OutputStream: TFileStream;
  EOL, Content: AnsiString;
  NewEOL, Directory, Filter, FileName: string;
begin
  NewEOL := FNewEOL;
  ExpandEnvironmentVar(NewEOL);
  Directory := FDirectory;
  ExpandEnvironmentVar(Directory);
  Filter := FFilter;
  ExpandEnvironmentVar(Filter);

  Result := True;

  if AnsiSameText(NewEOL, 'CRLF') then
    EOL := NativeCrLf
  else
  if AnsiSameText(NewEOL, 'CR') then
    EOL := NativeCarriageReturn
  else
  if AnsiSameText(NewEOL, 'LF') then
    EOL := NativeLineFeed
  else
  begin
    AMessageHandler('Invalid EOL configuration');
    Result := False;
    Exit;
  end;

  FileList := TStringList.Create;
  try
    BuildFileList(PathAddSeparator(Directory)+Filter, faAnyFile and (not faDirectory), FileList);
    for Index := 0 to FileList.Count - 1 do
    begin
      FileName := PathAddSeparator(Directory) + FileList.Strings[Index];
      
      Content := '';
      TextFile := TJclAnsiMappedTextReader.Create(FileName);
      try
        while not TextFile.Eof do
        begin
          if Content <> '' then
            Content := Content + EOL + TextFile.ReadLn
          else
            Content := TextFile.ReadLn;
        end;
      finally
        TextFile.Free;
      end;

      OutputStream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
      try
        OutputStream.WriteBuffer(Content[1], Length(Content));
      finally
        OutputStream.Free;
      end;

      if FileList.Count < 50 then
        AMessageHandler('Converted ' + FileName + ' to end of line ' + NewEOL);
    end;
    if FileList.Count >= 50 then
      AMessageHandler('Converted ' + IntToStr(FileList.Count) + ' to end of line ' + NewEOL);
  finally
    FileList.Free;
  end;
end;

function TEolConverter.GetCaption: string;
begin
  Result := Format('Change EOL for %s in %s to %s', [FFilter, FDirectory, FNewEOL]);
end;

function TEolConverter.GetConfigCaption(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Directory';
    1:
      Result := 'Filters';
    2:
      Result := 'New EOL';
  else
    Result := '';
  end;
end;

function TEolConverter.GetConfigCount: Integer;
begin
  Result := 3;
end;

function TEolConverter.GetConfigValue(Index: Integer): string;
begin
  case Index of
    0:
      Result := FDirectory;
    1:
      Result := FFilter;
    2:
      Result := FNewEOL;
  else
    Result := '';
  end;
end;

class function TEolConverter.GetDescription: string;
begin
  Result := 'Change EOL of text files';
end;

procedure TEolConverter.SetConfigValue(Index: Integer; const Value: string);
begin
  case Index of
    0:
      FDirectory := Value;
    1:
      FFilter := Value;
    2:
      FNewEOL := Value;
  end;
end;

//=== { TFileCopier } ========================================================

function TFileCopier.Execute(const AMessageHandler: TTextHandler): Boolean;
var
  FileList: TStrings;
  Index: Integer;
  FromDirectory, Filter, ToDirectory, FileName: string;
begin
  FromDirectory := FFromDirectory;
  ExpandEnvironmentVar(FromDirectory);
  Filter := FFilter;
  ExpandEnvironmentVar(Filter);
  ToDirectory := FToDirectory;
  ExpandEnvironmentVar(ToDirectory);

  FileList := TStringList.Create;
  try
    BuildFileList(PathAddSeparator(FromDirectory) + Filter, faAnyFile, FileList);
    if FileList.Count > 0 then
    begin
      Result := True;
      for Index := 0 to FileList.Count - 1 do
      begin
        FileName := FileList.Strings[Index];
        Result := FileCopy(PathAddSeparator(FromDirectory) + FileName,
                           PathAddSeparator(ToDirectory) + FileName, True);
        if not Result then
        begin
          AMessageHandler('Failed to copy ' + PathAddSeparator(FromDirectory) + FileName
            + ' to ' + ToDirectory + ', destination may exist');
          Break;
        end;
      end;
      if Result then
        AMessageHandler(IntToStr(FileList.Count) + ' files were copied')
      else
        AMessageHandler('Failed to copy ' + IntToStr(FileList.Count) + ' files');
    end
    else
    begin
      AMessageHandler('No files were found');
      Result := False;
    end;
  finally
    FileList.Free;
  end;
end;

function TFileCopier.GetCaption: string;
begin
  Result := 'Copy ' + FFilter + ' from ' + FFromDirectory + ' to ' + FToDirectory;
end;

function TFileCopier.GetConfigCaption(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'From directory';
    1:
      Result := 'Filter';
    2:
      Result := 'To directory';
  else
    Result := '';
  end;
end;

function TFileCopier.GetConfigCount: Integer;
begin
  Result := 3;
end;

function TFileCopier.GetConfigValue(Index: Integer): string;
begin
  case Index of
    0:
      Result := FFromDirectory;
    1:
      Result := FFilter;
    2:
      Result := FToDirectory;
  else
    Result := '';
  end;
end;

class function TFileCopier.GetDescription: string;
begin
  Result := 'Copy files from one directory to an other';
end;

procedure TFileCopier.SetConfigValue(Index: Integer; const Value: string);
begin
  case Index of
    0:
      FFromDirectory := Value;
    1:
      FFilter := Value;
    2:
      FToDirectory := Value;
  end;
end;

//=== { TFileCreator } =======================================================

function TFileCreator.Execute(const AMessageHandler: TTextHandler): Boolean;
var
  FileName, Content: string;
  AFileStream: TStream;
begin
  FileName := FFileName;
  ExpandEnvironmentVar(FileName);
  Content := FContent;
  ExpandEnvironmentVar(Content);

  Result := True;

  AFileStream := TFileStream.Create(FileName, fmCreate and fmShareDenyWrite);
  try
    if Content <> '' then
    begin
      AFileStream.WriteBuffer(Content[1], Length(Content) * SizeOf(Content[1]));
      AMessageHandler('Created file ' + FileName + ' with content ' + Content);
    end
    else
      AMessageHandler('Created empty file ' + FileName);
  finally
    AFileStream.Free;
  end;
end;

function TFileCreator.GetCaption: string;
begin
  if FContent <> '' then
    Result := 'Create file ' + FFileName + ' and set its content to ' + FContent
  else
    Result := 'Create empty file ' + FFileName;
end;

function TFileCreator.GetConfigCaption(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'File name';
    1:
      Result := 'Content';
  else
    Result := '';
  end;
end;

function TFileCreator.GetConfigCount: Integer;
begin
  Result := 2;
end;

function TFileCreator.GetConfigValue(Index: Integer): string;
begin
  case Index of
    0:
      Result := FFileName;
    1:
      Result := FContent;
  else
    Result := '';
  end;
end;

class function TFileCreator.GetDescription: string;
begin
  Result := 'Create a file and write its content';
end;

procedure TFileCreator.SetConfigValue(Index: Integer; const Value: string);
begin
  case Index of
    0:
      FFileName := Value;
    1:
      FContent := Value;
  end;
end;

//=== { TFileRenamer } =======================================================

function TFileMover.Execute(const AMessageHandler: TTextHandler): Boolean;
var
  OldFileName, NewFileName: string;
begin
  OldFileName := FOldFileName;
  ExpandEnvironmentVar(OldFileName);
  NewFileName := FNewFileName;
  ExpandEnvironmentVar(NewFileName);

  AMessageHandler('Renaming file ' + OldFileName + ' to ' + NewFileName);
  Result := FileMove(OldFileName, NewFileName, True);
  if Result then
    AMessageHandler('Rename success')
  else
    AMessageHandler('Rename failure');
end;

function TFileMover.GetCaption: string;
begin
  Result := 'Rename ' + FOldFileName + ' to ' + FNewFileName;
end;

function TFileMover.GetConfigCaption(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Old file name';
    1:
      Result := 'New file name';
  else
    Result := '';
  end;
end;

function TFileMover.GetConfigCount: Integer;
begin
  Result := 2;
end;

function TFileMover.GetConfigValue(Index: Integer): string;
begin
  case Index of
    0:
      Result := FOldFileName;
    1:
      Result := FNewFileName;
  else
    Result := '';
  end;
end;

class function TFileMover.GetDescription: string;
begin
  Result := 'Rename a file';
end;

procedure TFileMover.SetConfigValue(Index: Integer; const Value: string);
begin
  case Index of
    0:
      FOldFileName := Value;
    1:
      FNewFileName := Value;
  end;
end;

//=== { TFileRemover } =======================================================

function TFileRemover.Execute(const AMessageHandler: TTextHandler): Boolean;
var
  FileList: TStrings;
  Index: Integer;
  Directory, Filter, MoveToRecycleBin, FileName: string;
  MoveToRecBin: Boolean;
begin
  Directory := FDirectory;
  ExpandEnvironmentVar(Directory);
  Filter := FFilter;
  ExpandEnvironmentVar(Filter);
  MoveToRecycleBin := FMoveToRecycleBin;
  ExpandEnvironmentVar(MoveToRecycleBin);
  MoveToRecBin := AnsiSameText(MoveToRecycleBin, 'yes');

  Result := True;
  FileList := TStringList.Create;
  try
    BuildFileList(PathAddSeparator(Directory) + Filter, faAnyFile and (not faDirectory), FileList);
    if FileList.Count > 0 then
    begin
      for Index := 0 to FileList.Count - 1 do
      begin
        FileName := FileList.Strings[Index];
        Result := FileDelete(PathAddSeparator(Directory) + FileName, MoveToRecBin);
        if not Result then
        begin
          AMessageHandler('Failed to remove ' + PathAddSeparator(Directory) + FileName);
          Break;
        end;
      end;
      if Result then
        AMessageHandler(IntToStr(FileList.Count) + ' files were deleted')
      else
        AMessageHandler('Failed to remove ' + IntToStr(FileList.Count) + ' files');
    end;
  finally
    FileList.Free;
  end;
end;

function TFileRemover.GetCaption: string;
begin
  Result := 'Remove ' + FFilter + ' in ' + FDirectory;
end;

function TFileRemover.GetConfigCaption(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Directory';
    1:
      Result := 'Filter';
    2:
      Result := 'Move to recycle bin';
  else
    Result := '';
  end;
end;

function TFileRemover.GetConfigCount: Integer;
begin
  Result := 3;
end;

function TFileRemover.GetConfigValue(Index: Integer): string;
begin
  case Index of
    0:
      Result := FDirectory;
    1:
      Result := FFilter;
    2:
      Result := FMoveToRecycleBin;
  else
    Result := '';
  end;
end;

class function TFileRemover.GetDescription: string;
begin
  Result := 'Remove files from one directory';
end;

procedure TFileRemover.SetConfigValue(Index: Integer; const Value: string);
begin
  case Index of
    0:
      FDirectory := Value;
    1:
      FFilter := Value;
    2:
      FMoveToRecycleBin := Value;
  end;
end;

//=== { TFileTouch } =========================================================

function TFileTouch.Execute(const AMessageHandler: TTextHandler): Boolean;
const
  FILE_WRITE_ATTRIBUTES = $00000100;
var
  Path: string;
  PathHandle: THandle;
  AFileTime: TFileTime;
begin
  Path := FPath;
  ExpandEnvironmentVar(Path);

  PathHandle := CreateFile(PChar(Path), FILE_WRITE_ATTRIBUTES, FILE_SHARE_READ, nil, OPEN_ALWAYS, 0, 0);
  if PathHandle <> INVALID_HANDLE_VALUE then
  begin
    try
      AFileTime := LocalDateTimeToFileTime(Date);
      Result := SetFileTime(PathHandle, @AFileTime, @AFileTime, @AFileTime);
    finally
      CloseHandle(PathHandle);
    end;
    if Result then
      AMessageHandler('Touched path ' + Path)
    else
      AMessageHandler('Failed to touch path ' + Path);
  end
  else
  begin
    Result := False;
    AMessageHandler('Failed to open path ' + Path + ' for a touch');
  end;
end;

function TFileTouch.GetCaption: string;
begin
  Result := 'Touch ' + FPath;
end;

function TFileTouch.GetConfigCaption(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Path name';
  else
    Result := '';
  end;
end;

function TFileTouch.GetConfigCount: Integer;
begin
  Result := 1;
end;

function TFileTouch.GetConfigValue(Index: Integer): string;
begin
  case Index of
    0:
      Result := FPath;
  else
    Result := '';
  end;
end;

class function TFileTouch.GetDescription: string;
begin
  Result := 'Touch a file or a path';
end;

procedure TFileTouch.SetConfigValue(Index: Integer; const Value: string);
begin
  case Index of
    0:
      FPath := Value;  
  end;
end;

//=== { TDirectoryCreator } ==================================================

function TDirectoryCreator.Execute(const AMessageHandler: TTextHandler): Boolean;
var
  Directory: string;
begin
  Directory := FDirectory;
  ExpandEnvironmentVar(Directory);

  Result := ForceDirectories(Directory);
  if Result then
    AMessageHandler('Created directory ' + Directory)
  else
    AMessageHandler('Failed to create directory ' + Directory);
end;

function TDirectoryCreator.GetCaption: string;
begin
  Result := 'Create directory ' + FDirectory;
end;

function TDirectoryCreator.GetConfigCaption(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Directory';
  else
    Result := '';
  end;
end;

function TDirectoryCreator.GetConfigCount: Integer;
begin
  Result := 1;
end;

function TDirectoryCreator.GetConfigValue(Index: Integer): string;
begin
  case Index of
    0:
      Result := FDirectory;
  else
    Result := '';
  end;
end;

class function TDirectoryCreator.GetDescription: string;
begin
  Result := 'Create a directory';
end;

procedure TDirectoryCreator.SetConfigValue(Index: Integer; const Value: string);
begin
  case Index of
    0:
      FDirectory := Value;
  end;
end;

//=== { TDirectoryRemover } ==================================================

function TDirectoryRemover.Execute(const AMessageHandler: TTextHandler): Boolean;
var
  Directory, MoveToRecycleBin: string;
  MoveToRecBin: Boolean;
begin
  Directory := FDirectory;
  ExpandEnvironmentVar(Directory);
  MoveToRecycleBin := FMoveToRecycleBin;
  ExpandEnvironmentVar(MoveToRecycleBin);
  MoveToRecBin := AnsiSameText(MoveToRecycleBin, 'yes');

  Result := (not DirectoryExists(Directory)) or DeleteDirectory(Directory, MoveToRecBin);

  if Result then
    AMessageHandler('Removed directory ' + Directory)
  else
    AMessageHandler('Failed to remove directory ' + Directory);
end;

function TDirectoryRemover.GetCaption: string;
begin
  Result := 'Delete directory ' + FDirectory;
end;

function TDirectoryRemover.GetConfigCaption(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Directory';
    1:
      Result := 'Move to recycle bin';
  else
    Result := '';
  end;
end;

function TDirectoryRemover.GetConfigCount: Integer;
begin
  Result := 2;
end;

function TDirectoryRemover.GetConfigValue(Index: Integer): string;
begin
  case Index of
    0:
      Result := FDirectory;
    1:
      Result := FMoveToRecycleBin;
  else
    Result := '';
  end;
end;

class function TDirectoryRemover.GetDescription: string;
begin
  Result := 'Delete a directory';
end;

procedure TDirectoryRemover.SetConfigValue(Index: Integer; const Value: string);
begin
  case Index of
    0:
      FDirectory := Value;
    1:
      FMoveToRecycleBin := Value;
  end;
end;

//=== { TConstantParser } ====================================================

function TConstantParser.Execute(const AMessageHandler: TTextHandler): Boolean;
var
  SourceFile, ConstantName, EnvironmentVariable, Line, Value: string;
  FileContent: TJclAnsiMappedTextReader;
  LinePos, ConstantStart: Integer;
begin
  SourceFile := FSourceFile;
  ExpandEnvironmentVar(SourceFile);
  ConstantName := FConstantName;
  ExpandEnvironmentVar(ConstantName);
  EnvironmentVariable := FEnvironmentVariable;
  ExpandEnvironmentVar(EnvironmentVariable);

  FileContent := TJclAnsiMappedTextReader.Create(SourceFile);
  try
    Result := False;

    while not FileContent.Eof do
    begin
      Line := FileContent.ReadLn;
      LinePos := StrIPos(ConstantName, Line);

      if LinePos > 0 then
      begin
        // found constant name
        Inc(LinePos, Length(ConstantName));
        if (LinePos <= Length(Line)) and not CharIsValidIdentifierLetter(Line[LinePos]) then
        begin
          // make sure it's a whole word
          while (LinePos <= Length(Line)) and CharIsWhiteSpace(Line[LinePos]) do
            Inc(LinePos);
          if (LinePos <= Length(Line)) and (Line[LinePos] = '=') then
          begin
            // is on a constant declaration line
            Inc(LinePos);
            // goto start of constant value
            while (LinePos <= Length(Line)) and CharIsWhiteSpace(Line[LinePos]) do
              Inc(LinePos);

            if (LinePos <= Length(Line)) and (Line[LinePos] in ['0'..'9', '$', '+', '-']) then
            begin
              // numeric constant
              ConstantStart := LinePos;
              Inc(LinePos);
              while (LinePos <= Length(Line)) and (Line[LinePos] in ['0'..'9', '.', 'e', 'E', '+', '-']) do
                Inc(LinePos);
              Value := Copy(Line, ConstantStart, LinePos - ConstantStart);
              Result := True;
            end
            else
            if (LinePos <= Length(Line)) and (Line[LinePos] = '''') then
            begin
              // string constant
              ConstantStart := LinePos;
              Inc(LinePos);
              while (LinePos <= Length(Line)) and (Line[LinePos] <> '''') do
                Inc(LinePos);
              Value := Copy(Line, ConstantStart, LinePos - ConstantStart);
              Result := True;
            end
            else
            if (LinePos <= Length(Line)) and CharIsValidIdentifierLetter(Line[LinePos]) then
            begin
              // identifier
              ConstantStart := LinePos;
              Inc(LinePos);
              while (LinePos <= Length(Line)) and CharIsValidIdentifierLetter(Line[LinePos]) do
                Inc(LinePos);
              Value := Copy(Line, ConstantStart, LinePos - ConstantStart);
              Result := True;
            end;

            if Result then
            begin
              SetEnvironmentVar(EnvironmentVariable, Value);
              AMessageHandler('Found constant ' + ConstantName + ' with value ' + Value);
              Break;
            end;
          end;
        end;
      end;
    end;
    if not Result then
      AMessageHandler('Failed to find constant ' + ConstantName);
  finally
    FileContent.Free;
  end;
end;

function TConstantParser.GetCaption: string;
begin
  Result := 'Load constant ' + FConstantName + ' in file ' + FSourceFile + ' and save result in '
    + FEnvironmentVariable;
end;

function TConstantParser.GetConfigCaption(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Source file';
    1:
      Result := 'Constant name';
    2:
      Result := 'Environment variable';
  else
    Result := '';
  end;
end;

function TConstantParser.GetConfigCount: Integer;
begin
  Result := 3;
end;

function TConstantParser.GetConfigValue(Index: Integer): string;
begin
  case Index of
    0:
      Result := FSourceFile;
    1:
      Result := FConstantName;
    2:
      Result := FEnvironmentVariable;
  else
    Result := '';
  end;
end;

class function TConstantParser.GetDescription: string;
begin
  Result := 'Load a constant from a source file and save result to an environment variable';
end;

procedure TConstantParser.SetConfigValue(Index: Integer; const Value: string);
begin
  case Index of
    0:
      FSourceFile := Value;
    1:
      FConstantName := Value;
    2:
      FEnvironmentVariable := Value;
  end;
end;

//=== { TVariableReader } ====================================================

function TVariableReader.Execute(const AMessageHandler: TTextHandler): Boolean;
var
  SourceFile, EnvironmentVariable: string;
  Reader: TJclAnsiMappedTextReader;
begin
  SourceFile := FSourceFile;
  ExpandEnvironmentVar(SourceFile);
  EnvironmentVariable := FEnvironmentVariable;
  ExpandEnvironmentVar(EnvironmentVariable);

  AMessageHandler('Read ' + EnvironmentVariable + ' from ' + SourceFile);

  if FileGetSize(SourceFile) <> 0 then
  begin
    Reader := TJclAnsiMappedTextReader.Create(SourceFile);
    try
      SetEnvironmentVar(EnvironmentVariable, string(Reader.ReadLn));
    finally
      Reader.Free;
    end;
  end
  else
    SetEnvironmentVar(EnvironmentVariable, '');
  Result := True;
end;

function TVariableReader.GetCaption: string;
begin
  Result := 'Load environment variable ' + FEnvironmentVariable + ' from file ' + FSourceFile;
end;

function TVariableReader.GetConfigCaption(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Source file';
    1:
      Result := 'Environment variable';
  else
    Result := '';
  end;
end;

function TVariableReader.GetConfigCount: Integer;
begin
  Result := 2;
end;

function TVariableReader.GetConfigValue(Index: Integer): string;
begin
  case Index of
    0:
      Result := FSourceFile;
    1:
      Result := FEnvironmentVariable;
  else
    Result := '';
  end;
end;

class function TVariableReader.GetDescription: string;
begin
  Result := 'Load an environment variable from a file';
end;

procedure TVariableReader.SetConfigValue(Index: Integer; const Value: string);
begin
  case Index of
    0:
      FSourceFile := Value;
    1:
      FEnvironmentVariable := Value;
  end;
end;

//=== { TVariableSetter } ====================================================

function TVariableSetter.Execute(const AMessageHandler: TTextHandler): Boolean;
var
  EnvironmentVariable, Expression: string;
begin
  EnvironmentVariable := FEnvironmentVariable;
  ExpandEnvironmentVar(EnvironmentVariable);
  Expression := FExpression;
  ExpandEnvironmentVar(Expression);

  AMessageHandler('set ' + EnvironmentVariable + '=' + Expression);
  SetEnvironmentVar(EnvironmentVariable, Expression);
  Result := True;
end;

function TVariableSetter.GetCaption: string;
begin
  Result := 'Set environment variable ' + FEnvironmentVariable + ' to ' + FExpression;  
end;

function TVariableSetter.GetConfigCaption(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Expression';
    1:
      Result := 'Environment variable';
  else
    Result := '';
  end;
end;

function TVariableSetter.GetConfigCount: Integer;
begin
  Result := 2;
end;

function TVariableSetter.GetConfigValue(Index: Integer): string;
begin
  case Index of
    0:
      Result := FExpression;
    1:
      Result := FEnvironmentVariable;
  else
    Result := '';
  end;
end;

class function TVariableSetter.GetDescription: string;
begin
  Result := 'set an environment variable to an expression';
end;

procedure TVariableSetter.SetConfigValue(Index: Integer; const Value: string);
begin
  case Index of
    0:
      FExpression := Value;
    1:
      FEnvironmentVariable := Value;
  end;
end;

//=== { TVariableWriter } ====================================================

function TVariableWriter.Execute(const AMessageHandler: TTextHandler): Boolean;
var
  DestinationFile, Expression, Append: string;
  DestinationStream: TFileStream;
  StringStream: TJclAnsiStream;
begin
  DestinationFile := FDestinationFile;
  ExpandEnvironmentVar(DestinationFile);
  Expression := FExpression;
  ExpandEnvironmentVar(Expression);
  Append := FAppend;
  ExpandEnvironmentVar(Append);

  if AnsiSameText(Append, 'yes') and FileExists(DestinationFile) then
  begin
    AMessageHandler('Append ' + Expression + ' to ' + DestinationFile);
    DestinationStream := TFileStream.Create(DestinationFile, fmOpenReadWrite);
    DestinationStream.Seek(0, soEnd);
  end
  else
  begin
    AMessageHandler('Write ' + Expression + ' to ' + DestinationFile);
    DestinationStream := TFileStream.Create(DestinationFile, fmCreate);
  end;
  try
    StringStream := TJclAnsiStream.Create(DestinationStream, False);
    try
      StringStream.WriteString(Expression, 1, Length(Expression));
    finally
      StringStream.Free;
    end;
  finally
    DestinationStream.Free;
  end;
  Result := True;
end;

function TVariableWriter.GetCaption: string;
var
  Append: string;
begin
  Append := FAppend;
  ExpandEnvironmentVar(Append);
  if AnsiSameText(Append, 'yes') then
    Result := 'Append ' + FExpression + ' to file ' + FDestinationFile
  else
    Result := 'Save ' + FExpression + ' to file ' + FDestinationFile;
end;

function TVariableWriter.GetConfigCaption(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Destination file';
    1:
      Result := 'Expression';
    2:
      Result := 'Append';
  else
    Result := '';
  end;
end;

function TVariableWriter.GetConfigCount: Integer;
begin
  Result := 3;
end;

function TVariableWriter.GetConfigValue(Index: Integer): string;
begin
  case Index of
    0:
      Result := FDestinationFile;
    1:
      Result := FExpression;
    2:
      Result := FAppend;
  else
    Result := '';
  end;
end;

class function TVariableWriter.GetDescription: string;
begin
  Result := 'Save an environment variable to a file';
end;

procedure TVariableWriter.SetConfigValue(Index: Integer; const Value: string);
begin
  case Index of
    0:
      FDestinationFile := Value;
    1:
      FExpression := Value;
    2:
      FAppend := Value;
  end;
end;

{ TBuildCalculator }

function TBuildCalculator.Execute(const AMessageHandler: TTextHandler): Boolean;
var
  Year, Month, Day, EnvironmentVariable, Value: string;
  Today, StartDay: TDateTime;
begin
  Year := FYear;
  ExpandEnvironmentVar(Year);
  Month := FMonth;
  ExpandEnvironmentVar(Month);
  Day := FDay;
  ExpandEnvironmentVar(Day);
  EnvironmentVariable := FEnvironmentVariable;
  ExpandEnvironmentVar(EnvironmentVariable);

  Result := True;

  Today := Date;
  StartDay := EncodeDate(StrToInt(Year), StrToInt(Month), StrToInt(Day));

  Value := IntToStr(DaysBetween(Today, StartDay));
  SetEnvironmentVar(EnvironmentVariable, Value);
  AMessageHandler('Evaluated build number to ' + Value + ' and set environment variable ' + EnvironmentVariable);
end;

function TBuildCalculator.GetCaption: string;
begin
  Result := 'Evaluate days between now and ' + FYear + '-' + FMonth + '-' + FDay + ' and save result to '
    + FEnvironmentVariable;
end;

function TBuildCalculator.GetConfigCaption(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Year';
    1:
      Result := 'Month';
    2:
      Result := 'Day';
    3:
      Result := 'Environment variable';
  else
    Result := '';
  end;
end;

function TBuildCalculator.GetConfigCount: Integer;
begin
  Result := 4;
end;

function TBuildCalculator.GetConfigValue(Index: Integer): string;
begin
  case Index of
    0:
      Result := FYear;
    1:
      Result := FMonth;
    2:
      Result := FDay;
    3:
      Result := FEnvironmentVariable;
  else
    Result := '';
  end;
end;

class function TBuildCalculator.GetDescription: string;
begin
  Result := 'Evaluate number of days between now and a predefined date';
end;

procedure TBuildCalculator.SetConfigValue(Index: Integer; const Value: string);
begin
  case Index of
    0:
      FYear := Value;
    1:
      FMonth := Value;
    2:
      FDay := Value;
    3:
      FEnvironmentVariable := Value;
  end;
end;

//=== { TXmlGetter } =========================================================

function TXmlGetter.Execute(const AMessageHandler: TTextHandler): Boolean;
var
  XmlExe, WorkingDirectory, Arguments, ValidExitCodes, Key, EnvironmentVariable, ProgramOutput, NodeName, PropName: string;
  XmlContent: TJclSimpleXML;
  Node: TJclSimpleXMLElem;
  Prop: TJclSimpleXMLProp;
  NamedNodes: TJclSimpleXMLNamedElems;
  Index, AmpPos, NodeIndex, ReturnValue: Integer;
  Keys: TStrings;
begin
  XmlExe := FXmlExe;
  ExpandEnvironmentVar(XmlExe);
  WorkingDirectory := FWorkingDirectory;
  ExpandEnvironmentVar(WorkingDirectory);
  Arguments := FArguments;
  ExpandEnvironmentVar(Arguments);
  ValidExitCodes := FValidExitCodes;
  ExpandEnvironmentVar(ValidExitCodes);
  Key := FKey;
  ExpandEnvironmentVar(Key);
  EnvironmentVariable := FEnvironmentVariable;
  ExpandEnvironmentVar(EnvironmentVariable);

  XmlContent := TJclSimpleXML.Create;
  try
    // load xml content
    if AnsiSameText(ExtractFileExt(XmlExe), '.exe') then
    begin
      AMessageHandler('Executing ' + XmlExe + ' ' + Arguments + ' in ' + WorkingDirectory);
      if WorkingDirectory <> '' then
        SetCurrentDir(WorkingDirectory);
      ReturnValue := JclSysUtils.Execute(XmlExe + ' ' + Arguments, ProgramOutput);
      if ValidExitCodes = '' then
        Result := ReturnValue = 0
      else
        Result := ListItemIndex(ValidExitCodes, ';', IntToStr(ReturnValue)) >= 0;
      if Result then
        AMessageHandler('Execution success, return value was: ' + IntToStr(ReturnValue))
      else
      begin
        AMessageHandler('Execution failure, return value was: ' + IntToStr(ReturnValue));
        Exit;
      end;
      XmlContent.LoadFromString(ProgramOutput);
    end
    else
    begin
      AMessageHandler('Loading xml content from ' + XmlExe);
      XmlContent.LoadFromFile(XmlExe);
    end;

    XmlContent.Options := XmlContent.Options - [sxoAutoCreate];

    // locate key
    Keys := TStringList.Create;
    try
      Result := False;
      StrToStrings(Key, NativeBackslash, Keys, True);
      if Keys.Count > 0 then
      begin
        Node := XmlContent.Root;
        NodeName := Keys.Strings[0];
        if AnsiSameText(Node.Name, NodeName) then
        begin
          for Index := 1 to Keys.Count - 1 do
          begin
            NodeName := Keys.Strings[Index];
            if NodeName = '' then
            begin
              // next item is the last and is the name of the property
              if Index = (Keys.Count - 2) then
              begin
                PropName := Keys.Strings[Keys.Count - 1];
                Prop := Node.Properties.ItemNamed[PropName];
                if Prop <> nil then
                begin
                  SetEnvironmentVar(EnvironmentVariable, Prop.Value);
                  Result := True;
                  AMessageHandler('Property found: saved value ' + Prop.Value + ' to environment variable '
                    + EnvironmentVariable);
                end
                else
                  AMessageHandler('No such property: ' + PropName);
              end
              else
                AMessageHandler('Invalid key: property name has to be latest item in the string');
              Exit;
            end;
            AmpPos := Pos('&', NodeName);
            if AmpPos > 0 then
            begin
              // load indexed node
              NodeIndex := StrToInt(Copy(NodeName, AmpPos + 1, Length(NodeName) - AmpPos));
              SetLength(NodeName, AmpPos - 1);
              NamedNodes := Node.Items.NamedElems[NodeName];
              Node := NamedNodes.Item[NodeIndex];
              if Node = nil then
              begin
                AMessageHandler('Node not found: parent node doesn''t have ' + IntToStr(NodeIndex) + ' node(s) named '
                  + NodeName);
                Exit;
              end;
            end
            else
            begin
              // load named node
              Node := Node.Items.ItemNamed[NodeName];
              if Node = nil then
              begin
                AMessageHandler('Node not found: parent node doesn''t have a node named ' + NodeName);
                Exit;
              end;
            end;
            // last item: load its value
            if Index = Keys.Count - 1 then
            begin
              SetEnvironmentVar(EnvironmentVariable, Node.Value);
              Result := True;
              AMessageHandler('Node found: saved value ' + Node.Value + ' to environment variable '
                + EnvironmentVariable);
              Exit;
            end;
          end;
        end
        else
          AMessageHandler('Node not found: name of root node is not ' + NodeName);
      end
      else
        AMessageHandler('Invalid key: empty key');
    finally
      Keys.Free;
    end;
  finally
    XmlContent.Free;
  end;
end;

function TXmlGetter.GetCaption: string;
var
  XmlExe: string;
begin
  XmlExe := FXmlExe;
  ExpandEnvironmentVar(XmlExe);
  if AnsiSameText(ExtractFileExt(XmlExe), '.exe') then
    Result := 'Execute ' + XmlExe + ' with arguments ' + FArguments + ' in directory ' + FWorkingDirectory
      + ' and save xml value ' + FKey + ' in environment variable ' + FEnvironmentVariable
  else
    Result := 'Parse xml file ' + FXmlExe + ' and save value ' + FKey + ' in environment variable '
      + FEnvironmentVariable;
end;

function TXmlGetter.GetConfigCaption(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Xml or command file';
    1:
      Result := 'Working directory';
    2:
      Result := 'Arguments';
    3:
      Result := 'Valid exit codes';
    4:
      Result := 'Key';
    5:
      Result := 'Environment variable';
  else
    Result := '';
  end;
end;

function TXmlGetter.GetConfigCount: Integer;
begin
  Result := 6;
end;

function TXmlGetter.GetConfigValue(Index: Integer): string;
begin
  case Index of
    0:
      Result := FXmlExe;
    1:
      Result := FWorkingDirectory;
    2:
      Result := FArguments;
    3:
      Result := FValidExitCodes;
    4:
      Result := FKey;
    5:
      Result := FEnvironmentVariable;
  else
    Result := '';
  end;
end;

class function TXmlGetter.GetDescription: string;
begin
  Result := 'Extract a value from an xml file or from the output of a executable command';
end;

procedure TXmlGetter.SetConfigValue(Index: Integer; const Value: string);
begin
  case Index of
    0:
      FXmlExe := Value;
    1:
      FWorkingDirectory := Value;
    2:
      FArguments := Value;
    3:
      FValidExitCodes := Value;
    4:
      FKey := Value;
    5:
      FEnvironmentVariable := Value;
  end;
end;

//=== { TCommandLineCaller } =================================================

function TCommandLineCaller.Execute(const AMessageHandler: TTextHandler): Boolean;
var
  Application, WorkingDirectory, Parameters, ValidExitCodes, ResultFile: string;
  ReturnValue: Integer;
  ResultFileStream: TFileStream;
begin
  Application := FApplication;
  ExpandEnvironmentVar(Application);
  WorkingDirectory := FWorkingDirectory;
  ExpandEnvironmentVar(WorkingDirectory);
  Parameters := FParameters;
  ExpandEnvironmentVar(Parameters);
  ValidExitCodes := FValidExitCodes;
  ExpandEnvironmentVar(ValidExitCodes);
  ResultFile := FResultFile;
  ExpandEnvironmentVar(ResultFile);

  AMessageHandler('Executing ' + Application + ' ' + Parameters + ' in directory ' + WorkingDirectory);
  if ResultFile <> '' then
  begin
    ResultFileStream := TFileStream.Create(ResultFile, fmCreate);
    try
      FResultStringStream := TJclAnsiStream.Create(ResultFileStream, False);
      try
        if WorkingDirectory <> '' then
          SetCurrentDir(WorkingDirectory);
        ReturnValue := JclSysUtils.Execute(Application + ' ' + Parameters, WriteResult, True);
      finally
        FreeAndNil(FResultStringStream);
      end;
    finally
      ResultFileStream.Free;
    end;
  end
  else
  begin
    if WorkingDirectory <> '' then
      SetCurrentDir(WorkingDirectory);
    ReturnValue := JclSysUtils.Execute(Application + ' ' + Parameters, AMessageHandler, False);
  end;
  if ValidExitCodes = '' then
    Result := ReturnValue = 0
  else
    Result := ListItemIndex(ValidExitCodes, ';', IntToStr(ReturnValue)) >= 0;
  if Result then
    AMessageHandler('Execution success, return value was: ' + IntToStr(ReturnValue))
  else
    AMessageHandler('Execution failure, return value was: ' + IntToStr(ReturnValue));
end;

function TCommandLineCaller.GetCaption: string;
begin
  Result := 'Execute ' + FApplication + ' in directory ' + FWorkingDirectory + ' with parameters ' + FParameters;
end;

function TCommandLineCaller.GetConfigCaption(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Application';
    1:
      Result := 'Working directory';
    2:
      Result := 'Parameters';
    3:
      Result := 'Valid exit codes';
    4:
      Result := 'Result file';
  else
    Result := '';
  end;
end;

function TCommandLineCaller.GetConfigCount: Integer;
begin
  Result := 5;
end;

function TCommandLineCaller.GetConfigValue(Index: Integer): string;
begin
  case Index of
    0:
      Result := FApplication;
    1:
      Result := FWorkingDirectory;
    2:
      Result := FParameters;
    3:
      Result := FValidExitCodes;
    4:
      Result := FResultFile;
  else
    Result := '';
  end;
end;

class function TCommandLineCaller.GetDescription: string;
begin
  Result := 'Execute an external application';
end;

procedure TCommandLineCaller.SetConfigValue(Index: Integer; const Value: string);
begin
  case Index of
    0:
      FApplication := Value;
    1:
      FWorkingDirectory := Value;
    2:
      FParameters := Value;
    3:
      FValidExitCodes := Value;
    4:
      FResultFile := Value;
  end;
end;

procedure TCommandLineCaller.WriteResult(const Text: string);
begin
  if Assigned(FResultStringStream) then
    FResultStringStream.WriteString(Text,1,Length(Text));
end;

//=== { TArchiveMaker } ======================================================

function TArchiveMaker.Execute(const AMessageHandler: TTextHandler): Boolean;
var
  Directory, Filter, Archive, FileName: string;
  CompressArchiveClass: TJclCompressArchiveClass;
  CompressArchive: TJclCompressArchive;
  FileList: TStrings;
  Index: Integer;
begin
  Directory := FDirectory;
  ExpandEnvironmentVar(Directory);
  Filter := FFilter;
  ExpandEnvironmentVar(Filter);
  Archive := FArchive;
  ExpandEnvironmentVar(Archive);

  CompressArchiveClass := GetArchiveFormats.FindCompressFormat(Archive);
  Result := CompressArchiveClass <> nil;
  if Result then
  begin
    Directory := PathAddSeparator(PathGetRelativePath(GetCurrentDir, Directory));

    CompressArchive := CompressArchiveClass.Create(Archive);
    try
      FileList := TStringList.Create;
      try
        BuildFileList(Directory + Filter, faAnyFile, FileList);
        for Index := 0 to FileList.Count - 1 do
        begin
          FileName := FileList.Strings[Index];
          if DirectoryExists(Directory + FileName) then
            CompressArchive.AddDirectory(FileName, Directory + FileName, True, True)
          else
            CompressArchive.AddFile(FileName, Directory + FileName);
        end;
      finally
        FileList.Free;
      end;

      AMessageHandler('Adding ' + IntToStr(CompressArchive.ItemCount) + ' items to archive ' + Archive);

      CompressArchive.Compress;
      Result := True;
    finally
      CompressArchive.Free;
    end;
  end
  else
    AMessageHandler('No compressor available for archive ' + Archive);
end;

function TArchiveMaker.GetCaption: string;
begin
  Result := 'Add ' + FFilter + ' from ' + FDirectory + ' to archive ' + FArchive;
end;

function TArchiveMaker.GetConfigCaption(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Directory';
    1:
      Result := 'Filter';
    2:
      Result := 'Archive name';
  else
    Result := '';
  end;
end;

function TArchiveMaker.GetConfigCount: Integer;
begin
  Result := 3;
end;

function TArchiveMaker.GetConfigValue(Index: Integer): string;
begin
  case Index of
    0:
      Result := FDirectory;
    1:
      Result := FFilter;
    2:
      Result := FArchive;
  else
    Result := '';
  end;
end;

class function TArchiveMaker.GetDescription: string;
begin
  Result := 'Create an archive';
end;

procedure TArchiveMaker.SetConfigValue(Index: Integer; const Value: string);
begin
  case Index of
    0:
      FDirectory := Value;
    1:
      FFilter := Value;
    2:
      FArchive := Value;
  end;
end;

//=== { TLogCleaner } ========================================================

function TLogCleaner.Execute(const AMessageHandler: TTextHandler): Boolean;
begin
  AMessageHandler(LogClearCommand);
  Result := True;
end;

function TLogCleaner.GetCaption: string;
begin
  Result := 'Clear log';
end;

function TLogCleaner.GetConfigCaption(Index: Integer): string;
begin
  Result := '';
end;

function TLogCleaner.GetConfigCount: Integer;
begin
  Result := 0;
end;

function TLogCleaner.GetConfigValue(Index: Integer): string;
begin
  Result := '';
end;

class function TLogCleaner.GetDescription: string;
begin
  Result := 'Clear log';
end;

procedure TLogCleaner.SetConfigValue(Index: Integer; const Value: string);
begin
  // nothing to configure
end;

//=== { TLogSaver } ==========================================================

function TLogSaver.Execute(const AMessageHandler: TTextHandler): Boolean;
var
  FileName, Append: string;
begin
  FileName := FFileName;
  ExpandEnvironmentVar(FileName);
  Append := FAppend;
  ExpandEnvironmentVar(Append);
  Result := True;
  
  if AnsiSameText(Append, 'yes') then
  begin
    AMessageHandler('Appending log to file ' + FileName);
    AMessageHandler(LogAppendCommand + FileName);
  end
  else
  begin
    AMessageHandler('Saving log to file ' + FileName);
    AMessageHandler(LogSaveCommand + FileName);
  end;
end;

function TLogSaver.GetCaption: string;
var
  Append: string;
begin
  Append := FAppend;
  ExpandEnvironmentVar(Append);
  if AnsiSameText(Append, 'yes') then
    Result := 'Append log to file ' + FFileName
  else
    Result := 'Save log to file ' + FFileName;
end;

function TLogSaver.GetConfigCaption(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'File name';
    1:
      Result := 'Append';
  else
    Result := '';
  end;
end;

function TLogSaver.GetConfigCount: Integer;
begin
  Result := 2;
end;

function TLogSaver.GetConfigValue(Index: Integer): string;
begin
  case Index of
    0:
      Result := FFileName;
    1:
      Result := FAppend;
  else
    Result := '';
  end;
end;

class function TLogSaver.GetDescription: string;
begin
  Result := 'Save log';
end;

procedure TLogSaver.SetConfigValue(Index: Integer; const Value: string);
begin
  case Index of
    0:
      FFileName := Value;
    1:
      FAppend := Value;
  end;
end;

//=== { TDelay } =============================================================

function TDelay.Execute(const AMessageHandler: TTextHandler): Boolean;
var
  Delay: string;
  DelayInt: Integer;
begin
  Delay := FDelay;
  ExpandEnvironmentVar(Delay);
  Result := TryStrToInt(Delay, DelayInt);
  if Result then
  begin
    AMessageHandler(Format('Sleep for %d s', [DelayInt]));
    while DelayInt > 0 do
    begin
      Sleep(1000);
      Dec(DelayInt);
    end;
  end
  else
    AMessageHandler('invalid numeric value');
end;

function TDelay.GetCaption: string;
var
  Delay: string;
begin
  Delay := FDelay;
  ExpandEnvironmentVar(Delay);
  Result := Format('Sleep for %s s', [Delay]);
end;

function TDelay.GetConfigCaption(Index: Integer): string;
begin
  case Index of
    0: Result := 'Delay';
  else
    Result := '';
  end;
end;

function TDelay.GetConfigCount: Integer;
begin
  Result := 1;
end;

function TDelay.GetConfigValue(Index: Integer): string;
begin
  case Index of
    0: Result := FDelay;
  else
    Result := '';
  end;
end;

class function TDelay.GetDescription: string;
begin
  Result := 'Sleep for an arbitrary delay';
end;

procedure TDelay.SetConfigValue(Index: Integer; const Value: string);
begin
  case Index of
    0: FDelay := Value;
  end;
end;

//=== { TSubTask } ===========================================================

function TSubTask.Execute(const AMessageHandler: TTextHandler): Boolean;
var
  TaskName: string;
  TaskIndex: Integer;
  Task: TDistTask;
begin
  Result := False;
  TaskName := FTaskName;
  ExpandEnvironmentVar(TaskName);
  for TaskIndex := 0 to Distribution.TaskCount - 1 do
  begin
    Task := Distribution.Tasks[TaskIndex];
    if StrMatches(TaskName, Task.Name) then
    begin
      AMessageHandler(Format('Execute task %d', [TaskIndex]));
      Result := Distribution.ExecuteTask(TaskIndex);
      if not Result then
        Exit;
    end;
  end;
end;

function TSubTask.GetCaption: string;
var
  TaskName: string;
begin
  TaskName := FTaskName;
  ExpandEnvironmentVar(TaskName);
  Result := Format('Execute task "%s"', [TaskName]);
end;

function TSubTask.GetConfigCaption(Index: Integer): string;
begin
  case Index of
    0: Result := 'Task name';
  else
    Result := '';
  end;
end;

function TSubTask.GetConfigCount: Integer;
begin
  Result := 1;
end;

function TSubTask.GetConfigValue(Index: Integer): string;
begin
  case Index of
    0: Result := FTaskName;
  else
    Result := '';
  end;
end;

class function TSubTask.GetDescription: string;
begin
  Result := 'Execute an other task';
end;

procedure TSubTask.SetConfigValue(Index: Integer; const Value: string);
begin
  case Index of
    0: FTaskName := Value;
  end;
end;

initialization

  RegisterStandardActions;

finalization

  UnregisterStandardActions;

end.

