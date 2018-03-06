{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit System.IOUtils;

{$WARNINGS OFF}

{$SCOPEDENUMS ON}

interface
{$HPPEMIT LEGACYHPP}

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.SysTypes, Posix.Errno, Posix.Unistd,
{$ENDIF}
{$IFDEF MACOS}
{$IFDEF IOS}
  iOSApi.Foundation,
{$ELSE !IOS}
  Macapi.CocoaTypes,
{$ENDIF IOS}
{$ENDIF MACOS}
  System.RTLConsts, System.SysUtils, System.Classes, System.Types, System.Masks;

type
  TSearchOption = (soTopDirectoryOnly, soAllDirectories);

{$IFDEF MSWINDOWS}
  TFileAttribute = (faReadOnly, faHidden, faSystem, faDirectory, faArchive,
    faDevice, faNormal, faTemporary, faSparseFile, faReparsePoint, faCompressed,
    faOffline, faNotContentIndexed, faEncrypted, faSymLink) platform;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  TFileAttribute = (faNamedPipe, faCharacterDevice, faDirectory, faBlockDevice,
    faNormal, faSymLink, faSocket, faWhiteout, faOwnerRead, faOwnerWrite,
    faOwnerExecute, faGroupRead, faGroupWrite, faGroupExecute, faOthersRead,
    faOthersWrite, faOthersExecute, faUserIDExecution, faGroupIDExecution,
    faStickyBit) platform;
{$ENDIF POSIX}

  TFileAttributes = set of TFileAttribute platform;

  TFileMode = (fmCreateNew, fmCreate, fmOpen, fmOpenOrCreate, fmTruncate,
    fmAppend);

  TFileAccess = (faRead, faWrite, faReadWrite);

  TFileShare = (fsNone, fsRead, fsWrite, fsReadWrite);

  TDirectory = record
  public
    type
      TFilterPredicate = reference to function(const Path: string;
          const SearchRec: TSearchRec): Boolean;
  private
    type
      TDirectoryWalkProc = reference to function (const Path: string;
          const FileInfo: TSearchRec): Boolean;

    class procedure InternalCheckDirPathParam(const Path: string;
        const ExistsCheck: Boolean); static;
    class procedure CheckCreateDirectoryParameters(const Path: string); inline; static;
    class procedure CheckCopyParameters(SourceDirName, DestDirName: string); static;
    class procedure CheckDeleteParameters(const Path: string;
        const Recursive: Boolean); static;
    class procedure CheckGetAttributesParameters(const Path: string); inline; static;
    class procedure CheckGetDirectoriesParameters(const Path: string;
        const SearchPattern: string); static;
    class procedure CheckGetDirectoryRootParameters(const Path: string); inline; static;
    class procedure CheckGetFilesParameters(Path: string;
        const SearchPattern: string); static;
    class procedure CheckGetFileSystemEntriesParameters(Path: string;
        const SearchPattern: string); static;
    class procedure CheckGetCreationTimeParameters(const Path: string); inline; static;
    class procedure CheckGetCreationTimeUtcParameters(const Path: string); inline; static;
    class procedure CheckGetLastAccessTimeParameters(const Path: string); inline; static;
    class procedure CheckGetLastAccessTimeUtcParameters(const Path: string); inline; static;
    class procedure CheckGetLastWriteTimeParameters(const Path: string); inline; static;
    class procedure CheckGetLastWriteTimeUtcParameters(const Path: string); inline; static;
    class procedure CheckGetParentParameters(const Path: string); inline; static;
    class procedure CheckMoveParameters(SourceDirName, DestDirName: string); static;
    class procedure CheckSetAttributesParameters(const Path: string); inline; static;
    class procedure CheckSetCurrentDirectoryParameters(const Path: string); inline; static;
    class procedure CheckSetCreationTimeParameters(const Path: string); static;
    class procedure CheckSetCreationTimeUtcParameters(const Path: string); static;
    class procedure CheckSetLastAccessTimeParameters(const Path: string); static;
    class procedure CheckSetLastAccessTimeUtcParameters(const Path: string); static;
    class procedure CheckSetLastWriteTimeParameters(const Path: string); static;
    class procedure CheckSetLastWriteTimeUtcParameters(const Path: string); static;

    class procedure GetDateTimeInfo(const Path: string; out CreationTime,
        LastAccessTime, LastWriteTime: TDateTime;
        const UseLocalTimeZone: Boolean); static;
    class procedure SetDateTimeInfo(const Path: string; const CreationTime,
        LastAccessTime, LastWriteTime: PDateTime;
        const UseLocalTimeZone: Boolean); static;

{$IFDEF MSWINDOWS}
    class function ConvertDateTimeToFileTime(const DateTime: TDateTime;
        const UseLocalTimeZone: Boolean): TFileTime; static;
{$ENDIF}
{$IFDEF POSIX}
    class function ConvertDateTimeToFileTime(const DateTime: TDateTime;
        const UseLocalTimeZone: Boolean): time_t; static;
{$ENDIF}

    class function DoGetCreationTime(const Path: string): TDateTime; inline; static;
    class function DoGetCreationTimeUtc(const Path: string): TDateTime; inline; static;
    class function DoGetLastAccessTime(const Path: string): TDateTime; inline; static;
    class function DoGetLastAccessTimeUtc(const Path: string): TDateTime; inline; static;
    class function DoGetLastWriteTime(const Path: string): TDateTime; inline; static;
    class function DoGetLastWriteTimeUtc(const Path: string): TDateTime; inline; static;
    class procedure DoSetCreationTime(const Path: string;
        const CreationTime: TDateTime); inline; static;
    class procedure DoSetCreationTimeUtc(const Path: string;
        const CreationTime: TDateTime); inline; static;
    class procedure DoSetLastAccessTime(const Path: string;
        const LastAccessTime: TDateTime); inline; static;
    class procedure DoSetLastAccessTimeUtc(const Path: string;
        const LastAccessTime: TDateTime); inline; static;
    class procedure DoSetLastWriteTime(const Path: string;
        const LastWriteTime: TDateTime); inline; static;
    class procedure DoSetLastWriteTimeUtc(const Path: string;
        const LastWriteTime: TDateTime); inline; static;

    class function DoGetFiles(const Path, SearchPattern: string;
        const SearchOption: TSearchOption;
        const Predicate: TFilterPredicate): TStringDynArray; static;
    class function DoGetDirectories(const Path, SearchPattern: string;
        const SearchOption: TSearchOption;
        const Predicate: TFilterPredicate): TStringDynArray; static;
    class function DoGetFileSystemEntries(const Path, SearchPattern: string;
        const SearchOption: TSearchOption;
        const Predicate: TFilterPredicate): TStringDynArray; overload; static;

    class procedure WalkThroughDirectory(const Path, Pattern: string;
        const PreCallback, PostCallback: TDirectoryWalkProc;
        const Recursive: Boolean); static;
  public
    class procedure Copy(const SourceDirName, DestDirName: string); static;
    class procedure CreateDirectory(const Path: string); static;
    class procedure Delete(const Path: string); overload; inline; static;
    class procedure Delete(const Path: string; const Recursive: Boolean);
        overload; static;
    class function Exists(const Path: string; FollowLink: Boolean = True): Boolean; inline; static;
    class function GetAttributes(const Path: string; FollowLink: Boolean = True): TFileAttributes; inline; static;
    class function GetCurrentDirectory: string; {$IFDEF MSWINDOWS} inline; {$ENDIF} static;
    class procedure SetCurrentDirectory(const Path: string); static;
    class function GetLogicalDrives: TStringDynArray; static;
    class function GetCreationTime(const Path: string): TDateTime; static;
    class function GetCreationTimeUtc(const Path: string): TDateTime; static;
    class function GetLastAccessTime(const Path: string): TDateTime; static;
    class function GetLastAccessTimeUtc(const Path: string): TDateTime; static;
    class function GetLastWriteTime(const Path: string): TDateTime; static;
    class function GetLastWriteTimeUtc(const Path: string): TDateTime; static;
    class procedure SetAttributes(const Path: string;
        const Attributes: TFileAttributes); inline; static;
    class procedure SetCreationTime(const Path: string;
        const CreationTime: TDateTime); static;
    class procedure SetCreationTimeUtc(const Path: string;
        const CreationTime: TDateTime); static;
    class procedure SetLastAccessTime(const Path: string;
        const LastAccessTime: TDateTime); static;
    class procedure SetLastAccessTimeUtc(const Path: string;
        const LastAccessTime: TDateTime); static;
    class procedure SetLastWriteTime(const Path: string;
        const LastWriteTime: TDateTime); static;
    class procedure SetLastWriteTimeUtc(const Path: string;
        const LastWriteTime: TDateTime); static;
    class function GetParent(const Path: string): string; static;

    class function GetDirectories(const Path: string): TStringDynArray;
        overload; inline; static;
    class function GetDirectories(const Path: string;
        const Predicate: TFilterPredicate): TStringDynArray;
        overload; inline; static;
    class function GetDirectories(const Path,
        SearchPattern: string): TStringDynArray; overload; inline; static;
    class function GetDirectories(const Path, SearchPattern: string;
        const Predicate: TFilterPredicate): TStringDynArray;
        overload; inline; static;
    class function GetDirectories(const Path, SearchPattern: string;
        const SearchOption: TSearchOption): TStringDynArray; overload; static;
    class function GetDirectories(const Path, SearchPattern: string;
        const SearchOption: TSearchOption;
        const Predicate: TFilterPredicate): TStringDynArray; overload; static;
    class function GetDirectories(const Path: string;
        const SearchOption: TSearchOption;
        const Predicate: TFilterPredicate): TStringDynArray; overload; static;

    class function GetDirectoryRoot(const Path: string): string; static;

    class function GetFiles(const Path: string): TStringDynArray;
        overload; inline; static;
    class function GetFiles(const Path: string;
        const Predicate: TFilterPredicate): TStringDynArray;
        overload; inline; static;
    class function GetFiles(const Path, SearchPattern: string): TStringDynArray;
        overload; inline; static;
    class function GetFiles(const Path, SearchPattern: string;
        const Predicate: TFilterPredicate): TStringDynArray;
        overload; inline; static;
    class function GetFiles(const Path, SearchPattern: string;
        const SearchOption: TSearchOption): TStringDynArray; overload; static;
    class function GetFiles(const Path, SearchPattern: string;
        const SearchOption: TSearchOption;
        const Predicate: TFilterPredicate): TStringDynArray; overload; static;
    class function GetFiles(const Path: string;
        const SearchOption: TSearchOption;
        const Predicate: TFilterPredicate): TStringDynArray; overload; static;

    class function GetFileSystemEntries(const Path: string): TStringDynArray;
        overload; inline; static;
    class function GetFileSystemEntries(const Path: string;
        const Predicate: TFilterPredicate): TStringDynArray;
        overload; inline; static;
    class function GetFileSystemEntries(const Path,
        SearchPattern: string): TStringDynArray; overload; static;
    class function GetFileSystemEntries(const Path, SearchPattern: string;
        const Predicate: TFilterPredicate): TStringDynArray; overload; static;
    class function GetFileSystemEntries(const Path: string;
        const SearchOption: TSearchOption;
        const Predicate: TFilterPredicate): TStringDynArray; overload; static;

    class function IsEmpty(const Path: string): Boolean; static;
    class function IsRelativePath(const Path: string): Boolean; inline; static;

    class procedure Move(const SourceDirName, DestDirName: string); static;
  end;

  TPathPrefixType = (pptNoPrefix, pptExtended, pptExtendedUNC);

  TPath = record
  private
    const
      FCCurrentDir: string = '.'; // DO NOT LOCALIZE
      FCParentDir: string = '..'; // DO NOT LOCALIZE
      FCExtendedPrefix: string = '\\?\'; // DO NOT LOCALIZE
      FCExtendedUNCPrefix: string = '\\?\UNC\'; // DO NOT LOCALIZE
    class var
      FAltDirectorySeparatorChar: Char;
      FDirectorySeparatorChar: Char;
      FPathSeparator: Char;
      FVolumeSeparatorChar: Char;
      FExtensionSeparatorChar: Char;
      FInvalidPathChars: TCharArray;
      FInvalidFileNameChars: TCharArray;
      FPathWildcardChars: TCharArray;
      FFileNameWildCardChars: TCharArray;

    class procedure CheckPathLength(const Path: string; const MaxLength: Integer); static;
    class function GetExtensionSeparatorPos(const FileName: string): Integer; static;
{$IFDEF MSWINDOWS}
    class function HasPathValidColon(const Path: string): Boolean; static;
{$ENDIF MSWINDOWS}
    class procedure InternalCheckPathParam(const Path: string; const ExistsCheck: Boolean); static;
    class function IsCharInOrderedArray(const AChar: Char;
        const AnArray: TCharArray): Boolean; static;
    class function IsPathWildcardChar(const AChar: Char): Boolean; inline; static;
    class function IsPathSeparator(const AChar: Char): Boolean; inline; static;
    class function IsFileNameWildcardChar(const AChar: Char): Boolean; inline; static;

{$IFDEF MSWINDOWS}
    class function GetPosAfterExtendedPrefix(const Path: string): Integer;
        overload; inline; static;
    class function GetPosAfterExtendedPrefix(const Path: string;
        out Prefix: TPathPrefixType): Integer; overload; static;

    class function PrefixExtendsPath(const Prefix: TPathPrefixType): Boolean; inline; static;
{$ENDIF MSWINDOWS}

    class function DoCombine(const Path1, Path2: string;
        const ValidateParams: Boolean): string; static;
    class function DoGetDirectoryName(FileName: string): string; static;
    class function DoGetFileName(const FileName: string;
        const ValidateParam: Boolean): string; static;
    class function DoGetFullPath(const Path: string): string; static;
    class function DoGetPathRoot(const Path: string): string; static;
    class function DoIsPathRooted(const Path: string;
        const ValidateParam: Boolean): Boolean; static;
    class function DoMatchesPattern(const FileName, Pattern: string): Boolean; inline; static;
{$IFDEF MACOS}
    class function InternalGetMACOSPath(const SearchedPath: NSSearchPathDirectory; const SearchMask: NSSearchPathDomainMask): string; static;
{$ENDIF MACOS}
{$IFDEF LINUX}
  private
    type
      TXDGUserDirType = (Desktop, Downloads, Templates, PublicShare, Documents,
        Music, Pictures, Videos);
  private
    class function InternalXDGGetUserDir(const AType: TXDGUserDirType): string; static;
{$ENDIF LINUX}
  public
    class constructor Create;
    class function IsValidPathChar(const AChar: Char): Boolean; inline; static;
    class function IsValidFileNameChar(const AChar: Char): Boolean; inline; static;
    class function HasValidPathChars(const Path: string;
      const UseWildcards: Boolean): Boolean; static;
    class function HasValidFileNameChars(const FileName: string;
      const UseWildcards: Boolean): Boolean; static;

    class function GetExtendedPrefix(const Path: string): TPathPrefixType; static;
    class function IsDriveRooted(const Path: string): Boolean; static;
    class function IsExtendedPrefixed(const Path: string): Boolean; inline; static;
    class function IsRelativePath(const Path: string): Boolean; static;
    class function IsUNCPath(const Path: string): Boolean; inline; static;
    class function IsUNCRooted(const Path: string): Boolean; static;
    class function GetGUIDFileName(const UseSeparator: Boolean = False): string; static;
    class function DriveExists(const Path: string): Boolean; static;
    class function MatchesPattern(const FileName, Pattern: string;
        const CaseSensitive: Boolean): Boolean; static;
    class function ChangeExtension(const Path, Extension: string): string; static;
    class function Combine(const Path1, Path2: string): string; inline; static;
    class function GetDirectoryName(FileName: string): string; static;
    class function GetExtension(const FileName: string): string; static;
    class function GetFileName(const FileName: string): string; inline; static;
    class function GetFileNameWithoutExtension(const FileName: string): string; static;
    class function GetFullPath(const Path: string): string; static;
    class function GetInvalidFileNameChars: TCharArray; inline; static;
    class function GetInvalidPathChars: TCharArray; inline; static;
    class function GetPathRoot(const Path: string): string; static;
    class function GetRandomFileName: string; static;
    class function GetTempFileName: string; static;
    class function GetTempPath: string; static;
    class function GetHomePath: string; static;
    class function GetDocumentsPath: string; static;
    class function GetSharedDocumentsPath: string; static;
    class function GetLibraryPath: string; static;
    class function GetCachePath: string; static;
    class function GetPublicPath: string; static;

    class function GetPicturesPath: string; static;
    class function GetSharedPicturesPath: string; static;
    class function GetCameraPath: string; static;
    class function GetSharedCameraPath: string; static;
    class function GetMusicPath: string; static;
    class function GetSharedMusicPath: string; static;
    class function GetMoviesPath: string; static;
    class function GetSharedMoviesPath: string; static;
    class function GetAlarmsPath: string; static;
    class function GetSharedAlarmsPath: string; static;
    class function GetDownloadsPath: string; static;
    class function GetSharedDownloadsPath: string; static;
    class function GetRingtonesPath: string; static;
    class function GetSharedRingtonesPath: string; static;

    class function GetAttributes(const Path: string; FollowLink: Boolean = True): TFileAttributes; inline; static;
    class procedure SetAttributes(const Path: string;
        const Attributes: TFileAttributes); inline; static;
    class function HasExtension(const Path: string): Boolean; static;
    class function IsPathRooted(const Path: string): Boolean; inline; static;

    class property ExtensionSeparatorChar: Char read FExtensionSeparatorChar;
    class property AltDirectorySeparatorChar: Char read FAltDirectorySeparatorChar;
    class property DirectorySeparatorChar: Char read FDirectorySeparatorChar;
    class property PathSeparator: Char read FPathSeparator;
    class property VolumeSeparatorChar: Char read FVolumeSeparatorChar;
  end;

  TFile = record
  private
    const
      FCMinFileNameLen = 12;

    class procedure InternalCheckFilePathParam(const Path: string;
        const FileExistsCheck: Boolean); static;

    class procedure CheckAppendAllTextParameters(const Path: string;
        const Encoding: TEncoding; const NeedEncoding: Boolean); static;
    class procedure CheckAppendTextParameters(const Path: string); inline; static;
    class procedure CheckCopyParameters(const SourceFileName,
        DestFileName: string; const Overwrite: Boolean); static;
    class procedure CheckCreateParameters(const Path: string); inline; static;
    class procedure CheckCreateTextParameters(const Path: string); inline; static;
{$IFDEF MSWINDOWS}
    class procedure CheckDecryptParameters(const Path: string); inline; static;
{$ENDIF MSWINDOWS}
    class procedure CheckDeleteParameters(const Path: string); inline; static;
{$IFDEF MSWINDOWS}
    class procedure CheckEncryptParameters(const Path: string); inline; static;
{$ENDIF MSWINDOWS}
    class procedure CheckGetAttributesParameters(const Path: string); inline; static;
    class procedure CheckGetCreationTimeParameters(const Path: string); inline; static;
    class procedure CheckGetCreationTimeUtcParameters(const Path: string); inline; static;
    class procedure CheckGetLastAccessTimeParameters(const Path: string); inline; static;
    class procedure CheckGetLastAccessTimeUtcParameters(const Path: string); inline; static;
    class procedure CheckGetLastWriteTimeParameters(const Path: string); inline; static;
    class procedure CheckGetLastWriteTimeUtcParameters(const Path: string); inline; static;
    class procedure CheckMoveParameters(const SourceFileName,
        DestFileName: string); static;
    class procedure CheckOpenParameters(const Path: string); inline; static;
    class procedure CheckOpenReadParameters(const Path: string); inline; static;
    class procedure CheckOpenTextParameters(const Path: string); inline; static;
    class procedure CheckOpenWriteParameters(const Path: string); inline; static;
    class procedure CheckReadAllBytesParameters(const Path: string); inline; static;
    class procedure CheckReadAllLinesParameters(const Path: string;
        const Encoding: TEncoding; const NeedEncoding: Boolean); static;
    class procedure CheckReadAllTextParameters(const Path: string;
        const Encoding: TEncoding; const NeedEncoding: Boolean); static;
{$IFDEF MSWINDOWS}
    class procedure CheckReplaceParameters(const SourceFileName,
        DestinationFileName, DestinationBackupFileName: string); static;
{$ENDIF MSWINDOWS}
    class procedure CheckSetAttributesParameters(const Path: string); inline; static;
    class procedure CheckSetCreationTimeParameters(const Path: string); inline; static;
    class procedure CheckSetCreationTimeUtcParameters(const Path: string); inline; static;
    class procedure CheckSetLastAccessTimeParameters(const Path: string); inline; static;
    class procedure CheckSetLastAccessTimeUtcParameters(const Path: string); inline; static;
    class procedure CheckSetLastWriteTimeParameters(const Path: string); inline; static;
    class procedure CheckSetLastWriteTimeUtcParameters(const Path: string); inline; static;
    class procedure CheckWriteAllBytesParameters(const Path: string); inline; static;
    class procedure CheckWriteAllLinesParameters(const Path: string;
        const Encoding: TEncoding; const NeedEncoding: Boolean); static;
    class procedure CheckWriteAllTextParameters(const Path: string;
        const Encoding: TEncoding; const NeedEncoding: Boolean); static;

    class function GetEncoding(const Stream: TStream): TEncoding; static;
    class function GetStringArrayFromText(const Text: string): TStringDynArray; static;
    class function GetStringListFromArray(const AnArray: TStringDynArray): TStringList; static;

    class function DoCreateOpenFile(const Path: string): TFileStream; static;
    class function DoCopy(const SourceFileName, DestFileName: string;
        const Overwrite: Boolean): Boolean; {$IFDEF MSWINDOWS} inline; {$ENDIF} static;
    class function DoGetAttributes(const Path: string; FollowLink: Boolean = True): TFileAttributes; {$IFDEF MSWINDOWS}inline;{$ENDIF} static;
    class function DoReadAllBytes(const Path: string): TBytes; static;
    class function DoReadAllText(const Path: string): string; overload; static;
    class function DoReadAllText(const Path: string;
        const Encoding: TEncoding): string; overload; static;
    class procedure DoSetAttributes(const Path: string;
        const Attributes: TFileAttributes); {$IFDEF MSWINDOWS}inline;{$ENDIF} static;
    class procedure DoWriteAllText(const Path, Contents: string;
        const Encoding: TEncoding; const WriteBOM: Boolean); static;
    class procedure DoWriteAllLines(const Path: string;
        const Contents: TStringDynArray; const Encoding: TEncoding;
        const WriteBOM: Boolean); static;
  public
    class function IntegerToFileAttributes(const Attributes: Integer): TFileAttributes; static;
    class function FileAttributesToInteger(const Attributes: TFileAttributes): Integer; static;

    class function Create(const Path: string): TFileStream; overload; inline; static;
    class function Create(const Path: string; const BufferSize: Integer): TFileStream; overload; static;

    class procedure AppendAllText(const Path, Contents: string); overload; static;
    class procedure AppendAllText(const Path, Contents: string;
        const Encoding: TEncoding); overload; static;
    class function AppendText(const Path: string): TStreamWriter; static;
    class procedure Copy(const SourceFileName, DestFileName: string);
        overload; inline; static;
    class procedure Copy(const SourceFileName, DestFileName: string;
        const Overwrite: Boolean); overload; static;
    class function CreateSymLink(const Link, Target: string): Boolean; static;
    class function CreateText(const Path: string): TStreamWriter; static;
{$IFDEF MSWINDOWS}
    class procedure Decrypt(const Path: string); static;
{$ENDIF MSWINDOWS}
    class procedure Delete(const Path: string); static;
{$IFDEF MSWINDOWS}
    class procedure Encrypt(const Path: string); static;
{$ENDIF MSWINDOWS}
    class function Exists(const Path: string; FollowLink: Boolean = True): Boolean; inline; static;
    class function GetAttributes(const Path: string; FollowLink: Boolean = True): TFileAttributes; inline; static;
    class function GetCreationTime(const Path: string): TDateTime; inline; static;
    class function GetCreationTimeUtc(const Path: string): TDateTime; inline; static;
    class function GetLastAccessTime(const Path: string): TDateTime; inline; static;
    class function GetLastAccessTimeUtc(const Path: string): TDateTime; inline; static;
    class function GetLastWriteTime(const Path: string): TDateTime; inline; static;
    class function GetLastWriteTimeUtc(const Path: string): TDateTime; inline; static;
    class function GetSymLinkTarget(const FileName: string;
      var SymLinkRec: TSymLinkRec): Boolean; overload; static;
    class function GetSymLinkTarget(const FileName: string;
      var TargetName: string): Boolean; overload; static;
    class procedure Move(SourceFileName, DestFileName: string); static;
    class function Open(const Path: string;
      const Mode: TFileMode): TFileStream; overload; inline; static;
    class function Open(const Path: string;
        const Mode: TFileMode; const Access: TFileAccess): TFileStream;
        overload; inline; static;
    class function Open(const Path: string;
        const Mode: TFileMode; const Access: TFileAccess;
        const Share: TFileShare): TFileStream; overload; static;
    class function OpenRead(const Path: string): TFileStream; static;
    class function OpenText(const Path: string): TStreamReader; static;
    class function OpenWrite(const Path: string): TFileStream; static;
    class function ReadAllBytes(const Path: string): TBytes; static;
    class function ReadAllLines(const Path: string): TStringDynArray;
        overload; static;
    class function ReadAllLines(const Path: string;
        const Encoding: TEncoding): TStringDynArray; overload; static;
    class function ReadAllText(const Path: string): string; overload; inline; static;
    class function ReadAllText(const Path: string;
      const Encoding: TEncoding): string; overload; inline; static;

    class procedure Replace(const SourceFileName, DestinationFileName,
        DestinationBackupFileName: string); overload; {$IFDEF MSWINDOWS}inline; {$ENDIF} static;

{$IFDEF MSWINDOWS}
    class procedure Replace(SourceFileName, DestinationFileName,
        DestinationBackupFileName: string; const IgnoreMetadataErrors: Boolean);
        overload; static;
{$ENDIF MSWINDOWS}

    class procedure SetAttributes(const Path: string;
        const Attributes: TFileAttributes); inline; static;
    class procedure SetCreationTime(const Path: string;
        const CreationTime: TDateTime); inline; static;
    class procedure SetCreationTimeUtc(const Path: string;
        const CreationTime: TDateTime); inline; static;
    class procedure SetLastAccessTime(const Path: string;
        const LastAccessTime: TDateTime); inline; static;
    class procedure SetLastAccessTimeUtc(const Path: string;
        const LastAccessTime: TDateTime); inline; static;
    class procedure SetLastWriteTime(const Path: string;
        const LastWriteTime: TDateTime); inline; static;
    class procedure SetLastWriteTimeUtc(const Path: string;
        const LastWriteTime: TDateTime); inline; static;
    class procedure WriteAllBytes(const Path: string; const Bytes: TBytes); static;
    class procedure WriteAllLines(const Path: string;
        const Contents: TStringDynArray); overload; inline; static;
    class procedure WriteAllLines(const Path: string;
        const Contents: TStringDynArray; const Encoding: TEncoding); overload; static;
    class procedure WriteAllText(const Path, Contents: string); overload; static;
    class procedure WriteAllText(const Path, Contents: string;
        const Encoding: TEncoding); overload; static;
  end;

implementation

uses
{$IFDEF POSIX}
  Posix.Base, Posix.Stdio, Posix.Stdlib, Posix.SysStat, Posix.Time,
  Posix.Utime,
{$ENDIF}
{$IFDEF MACOS}
  Macapi.Helpers,
{$IFDEF IOS}
{$ELSE !IOS}
  Macapi.Foundation,
{$ENDIF IOS}
{$ENDIF MACOS}
{$IFDEF MSWINDOWS}
  Winapi.SHFolder,
{$ENDIF MSWINDOWS}
{$IFDEF ANDROID}
  Androidapi.IOUtils,
{$ENDIF}
  System.Generics.Collections,
  System.StrUtils, System.DateUtils, System.Math;

{ TDirectory }

class procedure TDirectory.CheckDeleteParameters(const Path: string;
    const Recursive: Boolean);
var
  LPath: string;
begin
  LPath := TPath.DoGetFullPath(Path);

  if TFile.Exists(LPath) then
    raise EInOutError.CreateRes(@SDirectoryInvalid);

  InternalCheckDirPathParam(LPath, True);

  if SameFileName(ParamStr(0), LPath) then
    raise EInOutError.CreateRes(@SDirectoryInvalid);
  if (not Recursive) and (not IsEmpty(LPath)) then
    raise EInOutError.CreateRes(@SDirectoryNotEmpty);
end;

class procedure TDirectory.CheckGetAttributesParameters(const Path: string);
begin
  InternalCheckDirPathParam(Path, True);
end;

class procedure TDirectory.CheckGetCreationTimeParameters(const Path: string);
begin
  InternalCheckDirPathParam(Path, True);
end;

class procedure TDirectory.CheckGetCreationTimeUtcParameters(
  const Path: string);
begin
  InternalCheckDirPathParam(Path, True);
end;

class procedure TDirectory.CheckGetDirectoriesParameters(const Path: string;
  const SearchPattern: string);
var
  LPath: string;
begin
  LPath := TPath.DoGetFullPath(Path);

  if Trim(SearchPattern) = '' then // DO NOT LOCALIZE
    raise EArgumentException.CreateRes(@SInvalidCharsInSearchPattern);
  if not TPath.HasValidFileNameChars(SearchPattern, True) then
    raise EArgumentException.CreateRes(@SInvalidCharsInSearchPattern);

  InternalCheckDirPathParam(Path, True);
end;

class procedure TDirectory.CheckGetDirectoryRootParameters(const Path: string);
begin
  InternalCheckDirPathParam(Path, False);
end;

class procedure TDirectory.CheckGetFilesParameters(Path: string;
  const SearchPattern: string);
begin
  Path := TPath.DoGetFullPath(Path);

  if Trim(SearchPattern) = '' then // DO NOT LOCALIZE
    raise EArgumentException.CreateRes(@SInvalidCharsInSearchPattern);
  if not TPath.HasValidFileNameChars(SearchPattern, True) then
    raise EArgumentException.CreateRes(@SInvalidCharsInSearchPattern);

  InternalCheckDirPathParam(Path, True);
end;

class procedure TDirectory.CheckGetFileSystemEntriesParameters(Path: string;
  const SearchPattern: string);
begin
  Path := TPath.DoGetFullPath(Path);

  if Trim(SearchPattern) = '' then // DO NOT LOCALIZE
    raise EArgumentException.CreateRes(@SInvalidCharsInSearchPattern);
  if not TPath.HasValidFileNameChars(SearchPattern, True) then
    raise EArgumentException.CreateRes(@SInvalidCharsInSearchPattern);

  InternalCheckDirPathParam(Path, True);
end;

class procedure TDirectory.CheckGetLastAccessTimeParameters(const Path: string);
begin
  InternalCheckDirPathParam(Path, True);
end;

class procedure TDirectory.CheckGetLastAccessTimeUtcParameters(
  const Path: string);
begin
  InternalCheckDirPathParam(Path, True);
end;

class procedure TDirectory.CheckGetLastWriteTimeParameters(const Path: string);
begin
  InternalCheckDirPathParam(Path, True);
end;

class procedure TDirectory.CheckGetLastWriteTimeUtcParameters(
  const Path: string);
begin
  InternalCheckDirPathParam(Path, True);
end;

class procedure TDirectory.CheckGetParentParameters(const Path: string);
begin
  InternalCheckDirPathParam(Path, False);
end;

class procedure TDirectory.CheckCopyParameters(SourceDirName,
  DestDirName: string);
begin
  SourceDirName := TPath.DoGetFullPath(SourceDirName);
  DestDirName := TPath.DoGetFullPath(DestDirName);
  InternalCheckDirPathParam(SourceDirName, True);
  InternalCheckDirPathParam(DestDirName, False);

  if SameFileName(SourceDirName, DestDirName) then
    raise EInOutError.CreateRes(@SSourceDirIsDestDir);
end;

class procedure TDirectory.CheckCreateDirectoryParameters(const Path: string);
begin
  InternalCheckDirPathParam(Path, False);

{$IFDEF MSWINDOWS}
  { Windows-only: check if the drive of the given path actually exists }
  if not TPath.IsUNCRooted(Path) then
    if not TPath.DriveExists(TPath.DoGetPathRoot(Path)) then
      raise EDirectoryNotFoundException.CreateRes(@SDriveNotFound);
{$ENDIF}
end;

class procedure TDirectory.CheckMoveParameters(SourceDirName,
  DestDirName: string);
begin
  SourceDirName := TPath.DoGetFullPath(SourceDirName);
  DestDirName := TPath.DoGetFullPath(DestDirName);
  InternalCheckDirPathParam(SourceDirName, True);
  InternalCheckDirPathParam(DestDirName, False);

  if Exists(DestDirName) then
    raise EInOutError.CreateRes(@SDirectoryAlreadyExists);

{$IFDEF MSWINDOWS}
  { Windows-only: check that drives/roots of both names is the same. }
  if not SameFileName(TPath.GetPathRoot(SourceDirName),
    TPath.DoGetPathRoot(DestDirName)) then
    raise EInOutError.CreateRes(@SSameRootDrive);
{$ENDIF}

  if SameFileName(SourceDirName, DestDirName) then
    raise EInOutError.CreateRes(@SSourceDirIsDestDir);
end;

class procedure TDirectory.CheckSetAttributesParameters(const Path: string);
begin
  InternalCheckDirPathParam(Path, True);
end;

class procedure TDirectory.CheckSetCreationTimeParameters(const Path: string);
begin
  InternalCheckDirPathParam(Path, True);
end;

class procedure TDirectory.CheckSetCreationTimeUtcParameters(
  const Path: string);
begin
  InternalCheckDirPathParam(Path, True);
end;

class procedure TDirectory.CheckSetCurrentDirectoryParameters(const Path: string);
begin
  InternalCheckDirPathParam(Path, True);
end;

class procedure TDirectory.CheckSetLastAccessTimeParameters(const Path: string);
begin
  InternalCheckDirPathParam(Path, True);
end;

class procedure TDirectory.CheckSetLastAccessTimeUtcParameters(
  const Path: string);
begin
  InternalCheckDirPathParam(Path, True);
end;

class procedure TDirectory.CheckSetLastWriteTimeParameters(const Path: string);
begin
  InternalCheckDirPathParam(Path, True);
end;

class procedure TDirectory.CheckSetLastWriteTimeUtcParameters(
  const Path: string);
begin
  InternalCheckDirPathParam(Path, True);
end;

{$IFDEF MSWINDOWS}
class function TDirectory.ConvertDateTimeToFileTime(const DateTime: TDateTime;
  const UseLocalTimeZone: Boolean): TFileTime;
var
  LFileTime: TFileTime;
  SysTime: TSystemTime;
begin
  Result.dwLowDateTime := 0;
  Result.dwLowDateTime := 0;
  DecodeDateTime(DateTime, SysTime.wYear, SysTime.wMonth, SysTime.wDay,
    SysTime.wHour, SysTime.wMinute, SysTime.wSecond, SysTime.wMilliseconds);

  if SystemTimeToFileTime(SysTime, LFileTime) then
    if UseLocalTimeZone then
      LocalFileTimeToFileTime(LFileTime, Result)
    else
      Result := LFileTime;
end;
{$ENDIF}
{$IFDEF POSIX}
class function TDirectory.ConvertDateTimeToFileTime(const DateTime: TDateTime;
  const UseLocalTimeZone: Boolean): time_t;
begin
  { Use the time zone if necessary }
  if not UseLocalTimeZone then
    Result := DateTimeToFileDate(TTimeZone.Local.ToLocalTime(DateTime))
  else
    Result := DateTimeToFileDate(DateTime);
end;
{$ENDIF}

class procedure TDirectory.Copy(const SourceDirName, DestDirName: string);
var
  PreCallback: TDirectoryWalkProc;
begin
  CheckCopyParameters(SourceDirName, DestDirName);

  PreCallback :=
    function (const Path: string; const FileInfo: TSearchRec): Boolean
      function StuffPath: string;
      var
        Ch: Char;
      begin
        Ch := SourceDirName.Chars[SourceDirName.Length - 1];

        if not TPath.IsPathSeparator(Ch) then
          Result := TPath.DoCombine(DestDirName,
            StuffString(Path, 1, Length(SourceDirName) + Length(TPath.DirectorySeparatorChar), ''), False) // DO NOT LOCALIZE
        else
          Result := TPath.DoCombine(DestDirName,
            StuffString(Path, 1, Length(SourceDirName), ''), False); // DO NOT LOCALIZE
      end;
    var
      CompletePath: string;
      CompleteSrc: string;
      CompleteDest: string;
    begin
      Result := True;

      // mirror each directory at the destination
      case FileInfo.Attr and System.SysUtils.faDirectory of
        // create a directory copy of the source
        System.SysUtils.faDirectory:
          if (FileInfo.Name <> TPath.FCCurrentDir) and (FileInfo.Name <> TPath.FCParentDir) then
          begin
            // the destination is the one given by DestDirName
            if SameFileName(SourceDirName, Path) then
              CompletePath := DestDirName
            // get the difference between Path and SourceDirName
            else
              CompletePath := StuffPath;
            CompletePath := TPath.DoCombine(CompletePath, FileInfo.Name, False);

            CreateDir(CompletePath);
          end;
        0: // move files from source to destination
            begin
              // determine the complete source and destination paths
              CompleteSrc := TPath.DoCombine(Path, FileInfo.Name, False);

              // the destination is the one given by DestDirName
              if SameFileName(SourceDirName, Path) then
                CompletePath := DestDirName
              // get the difference between Path and SourceDirName
              else
                CompletePath := StuffPath;
              // add the file name to the destination
              CompleteDest := TPath.DoCombine(CompletePath, FileInfo.Name, False);

              // copy the file from source to destination
              TFile.DoCopy(CompleteSrc, CompleteDest, True);
            end;
      end;
    end;

  // create the destination directory
  TDirectory.CreateDirectory(DestDirName);

  // move all directories and files
  WalkThroughDirectory(SourceDirName, '*', PreCallback, nil, True); // DO NOT LOCALIZE
end;

class procedure TDirectory.CreateDirectory(const Path: string);
var
  FullPath: string;
begin
  FullPath := TPath.DoGetFullPath(Path);
  CheckCreateDirectoryParameters(FullPath);
  ForceDirectories(FullPath);
end;

class procedure TDirectory.Delete(const Path: string);
begin
  Delete(Path, False);
end;

class procedure TDirectory.Delete(const Path: string; const Recursive: Boolean);
var
  PostCallback: TDirectoryWalkProc;
  LastDirInfo: TSearchRec;
begin
  CheckDeleteParameters(Path, Recursive);

  if Recursive then
  begin
    PostCallback :=
      function (const Path: string; const FileInfo: TSearchRec): Boolean
      var
        CompletePath: string;
      begin
        Result := True;

        if (FileInfo.Name <> TPath.FCParentDir) and (FileInfo.Name <> TPath.FCCurrentDir) then
        begin
          CompletePath := TPath.DoCombine(Path, FileInfo.Name, False);

          // clear read-only, system and hidden attributes that can compromise
          // the deletion
{$IFDEF MSWINDOWS}
          FileSetAttr(CompletePath, System.SysUtils.faNormal);
{$ENDIF MSWINDOWS}

          case FileInfo.Attr and System.SysUtils.faDirectory of
            System.SysUtils.faDirectory: // remove empty directories
              RemoveDir(CompletePath);
            0: // remove files
              DeleteFile(CompletePath);
          end;
        end;
      end;

    // delete all the files and subdirectories
    WalkThroughDirectory(Path, '*', nil, PostCallback, Recursive); // DO NOT LOCALIZE

    // delete the directory itself
    LastDirInfo.Name := TPath.DoGetFileName(Path, False);
    LastDirInfo.Attr := System.SysUtils.faDirectory;
    PostCallback(GetParent(Path), LastDirInfo);
  end
  else
  begin
{$IFDEF MSWINDOWS}
    FileSetAttr(Path, System.SysUtils.faNormal);
{$ENDIF}
    RemoveDir(Path);
  end;
end;

class function TDirectory.DoGetCreationTime(const Path: string): TDateTime;
var
  LastAccessTime: TDateTime;
  LastWriteTime: TDateTime;
begin
  GetDateTimeInfo(Path, Result, LastAccessTime, LastWriteTime, True);
end;

class function TDirectory.DoGetCreationTimeUtc(const Path: string): TDateTime;
var
  LastAccessTime: TDateTime;
  LastWriteTime: TDateTime;
begin
  GetDateTimeInfo(Path, Result, LastAccessTime, LastWriteTime, False);
end;

class function TDirectory.DoGetDirectories(const Path, SearchPattern: string;
  const SearchOption: TSearchOption;
  const Predicate: TFilterPredicate): TStringDynArray;
var
  PreCallback: TDirectoryWalkProc;
  ResultArray: TStringDynArray;
begin
  ResultArray := nil;
  PreCallback :=
    function (const Path: string; const FileInfo: TSearchRec): Boolean
    var
      CanAdd: Boolean;
    begin
      Result := True;

      if (FileInfo.Attr and System.SysUtils.faDirectory <> 0) and
         (FileInfo.Name <> TPath.FCParentDir) and (FileInfo.Name <> TPath.FCCurrentDir) then
      begin
        CanAdd := (not Assigned(Predicate)) or
                  (Assigned(Predicate) and Predicate(Path, FileInfo));

        if CanAdd then
        begin
          SetLength(ResultArray, Length(ResultArray) + 1);
          ResultArray[Length(ResultArray) - 1] := TPath.DoCombine(Path, FileInfo.Name, False);
        end;
      end;
    end;

  WalkThroughDirectory(Path, SearchPattern, PreCallback, nil,
    SearchOption = TSearchOption.soAllDirectories);
{$IFDEF LINUX}
  TArray.Sort<string>(ResultArray);
{$ENDIF}
  Result := ResultArray;
end;

class function TDirectory.DoGetFiles(const Path, SearchPattern: string;
  const SearchOption: TSearchOption;
  const Predicate: TFilterPredicate): TStringDynArray;
var
  PreCallback: TDirectoryWalkProc;
  ResultArray: TStringDynArray;
begin
  ResultArray := nil;
  PreCallback :=
    function (const Path: string; const FileInfo: TSearchRec): Boolean
    var
      CanAdd: Boolean;
    begin
      Result := True;

      if FileInfo.Attr and System.SysUtils.faDirectory = 0 then
      begin
        CanAdd := (not Assigned(Predicate)) or
                  (Assigned(Predicate) and Predicate(Path, FileInfo));

        if CanAdd then
        begin
          SetLength(ResultArray, Length(ResultArray) + 1);
          ResultArray[Length(ResultArray) - 1] := TPath.DoCombine(Path, FileInfo.Name, False);
        end;
      end;
    end;

  WalkThroughDirectory(Path, SearchPattern, PreCallback, nil,
    SearchOption = TSearchOption.soAllDirectories);

{$IFDEF LINUX}
  TArray.Sort<string>(ResultArray);
{$ENDIF}
  Result := ResultArray;
end;

class function TDirectory.DoGetFileSystemEntries(const Path, SearchPattern: string;
  const SearchOption: TSearchOption;
  const Predicate: TFilterPredicate): TStringDynArray;
var
  PreCallback: TDirectoryWalkProc;
  ResultArray: TStringDynArray;
begin
  ResultArray := nil;
  PreCallback :=
    function (const Path: string; const FileInfo: TSearchRec): Boolean
    var
      CanAdd: Boolean;
    begin
      Result := True;

      if (FileInfo.Name <> TPath.FCParentDir) and (FileInfo.Name <> TPath.FCCurrentDir) then
      begin
        CanAdd := (not Assigned(Predicate)) or
                  (Assigned(Predicate) and Predicate(Path, FileInfo));

        if CanAdd then
        begin
          SetLength(ResultArray, Length(ResultArray) + 1);
          ResultArray[Length(ResultArray) - 1] := TPath.DoCombine(Path, FileInfo.Name, False);
        end;
      end;
    end;

  WalkThroughDirectory(Path, SearchPattern, PreCallback, nil,
    SearchOption = TSearchOption.soAllDirectories);

  Result := ResultArray;
end;

class function TDirectory.DoGetLastAccessTime(const Path: string): TDateTime;
var
  CreationTime: TDateTime;
  LastWriteTime: TDateTime;
begin
  GetDateTimeInfo(Path, CreationTime, Result, LastWriteTime, True);
end;

class function TDirectory.DoGetLastAccessTimeUtc(const Path: string): TDateTime;
var
  CreationTime: TDateTime;
  LastWriteTime: TDateTime;
begin
  GetDateTimeInfo(Path, CreationTime, Result, LastWriteTime, False);
end;

class function TDirectory.DoGetLastWriteTime(const Path: string): TDateTime;
var
  CreationTime: TDateTime;
  LastAccessTime: TDateTime;
begin
  GetDateTimeInfo(Path, CreationTime, LastAccessTime, Result, True);
end;

class function TDirectory.DoGetLastWriteTimeUtc(const Path: string): TDateTime;
var
  CreationTime: TDateTime;
  LastAccessTime: TDateTime;
begin
  GetDateTimeInfo(Path, CreationTime, LastAccessTime, Result, False);
end;

class procedure TDirectory.DoSetCreationTime(const Path: string;
  const CreationTime: TDateTime);
begin
  SetDateTimeInfo(Path, @CreationTime, nil, nil, True);
end;

class procedure TDirectory.DoSetCreationTimeUtc(const Path: string;
  const CreationTime: TDateTime);
begin
  SetDateTimeInfo(Path, @CreationTime, nil, nil, False);
end;

class procedure TDirectory.DoSetLastAccessTime(const Path: string;
  const LastAccessTime: TDateTime);
begin
  SetDateTimeInfo(Path, nil, @LastAccessTime, nil, True);
end;

class procedure TDirectory.DoSetLastAccessTimeUtc(const Path: string;
  const LastAccessTime: TDateTime);
begin
  SetDateTimeInfo(Path, nil, @LastAccessTime, nil, False);
end;

class procedure TDirectory.DoSetLastWriteTime(const Path: string;
  const LastWriteTime: TDateTime);
begin
  SetDateTimeInfo(Path, nil, nil, @LastWriteTime, True);
end;

class procedure TDirectory.DoSetLastWriteTimeUtc(const Path: string;
  const LastWriteTime: TDateTime);
begin
  SetDateTimeInfo(Path, nil, nil, @LastWriteTime, False);
end;

class procedure TDirectory.WalkThroughDirectory(const Path, Pattern: string;
  const PreCallback, PostCallback: TDirectoryWalkProc;
  const Recursive: Boolean);
var
  SearchRec: TSearchRec;
  Match: Boolean;
  Stop: Boolean;
begin
  if FindFirst(TPath.DoCombine(Path, '*', False), faAnyFile, SearchRec) = 0 then // DO NOT LOCALIZE
  try
    Stop := False;

    repeat
      Match := TPath.DoMatchesPattern(SearchRec.Name, Pattern);

      // call the preorder callback method
      if Match and Assigned(PreCallback) then
        Stop := not PreCallback(Path, SearchRec);

      if not Stop then
      begin
        // go recursive in subdirectories
        if Recursive and (SearchRec.Attr and System.SysUtils.faDirectory <> 0) and
           (SearchRec.Name <> TPath.FCCurrentDir) and
           (SearchRec.Name <> TPath.FCParentDir) then
          WalkThroughDirectory(TPath.DoCombine(Path, SearchRec.Name, False),
            Pattern, PreCallback, PostCallback, Recursive);

        // call the post-order callback method
        if Match and Assigned(PostCallback) then
          Stop := not PostCallback(Path, SearchRec);
      end;
    until Stop or (FindNext(SearchRec) <> 0);
  finally
    FindClose(SearchRec);
  end;
end;

class function TDirectory.Exists(const Path: string; FollowLink: Boolean = True): Boolean;
begin
  Result := DirectoryExists(Path, FollowLink);
end;

class function TDirectory.GetAttributes(const Path: string; FollowLInk: Boolean = True): TFileAttributes;
begin
  CheckGetAttributesParameters(Path);

  Result := TFile.DoGetAttributes(Path, FollowLink);
end;

class function TDirectory.GetCreationTime(const Path: string): TDateTime;
begin
  CheckGetCreationTimeParameters(Path);

  Result := DoGetCreationTime(Path);
end;

class function TDirectory.GetCreationTimeUtc(const Path: string): TDateTime;
begin
  CheckGetCreationTimeUtcParameters(Path);

  Result := DoGetCreationTimeUtc(Path);
end;

class function TDirectory.GetCurrentDirectory: string;
{$IFDEF MSWINDOWS}
begin
  SetLastError(ERROR_SUCCESS);
  Result := GetCurrentDir;
end;
{$ENDIF}
{$IFDEF POSIX}
var
  Buff: TBytes;
  CurrDir: MarshaledAString;
begin
  SetLastError(0);
  SetLength(Buff, MAX_PATH + 1);
  CurrDir := MarshaledAString(Buff);

  { Get current directory }
  if getcwd(CurrDir, Length(Buff)) <> nil then
    Result := UTF8ToString(CurrDir)
  else
    raise EInOutError.Create(SysErrorMessage(GetLastError));
end;
{$ENDIF}

class procedure TDirectory.GetDateTimeInfo(const Path: string; out CreationTime,
  LastAccessTime, LastWriteTime: TDateTime; const UseLocalTimeZone: Boolean);
var
  DateTime: TDateTimeInfoRec;
begin
  CreationTime := 0;
  LastAccessTime := 0;
  LastWriteTime := 0;

  if FileGetDateTimeInfo(Path, DateTime) then
  begin
    if not UseLocalTimeZone then
    begin
      CreationTime := TTimeZone.Local.ToUniversalTime(DateTime.CreationTime);
      LastAccessTime := TTimeZone.Local.ToUniversalTime(DateTime.LastAccessTime);
      LastWriteTime := TTimeZone.Local.ToUniversalTime(DateTime.TimeStamp);
    end
    else
    begin
      CreationTime := DateTime.CreationTime;
      LastAccessTime := DateTime.LastAccessTime;
      LastWriteTime := DateTime.TimeStamp;
    end;
  end;
end;

class function TDirectory.GetDirectories(const Path,
  SearchPattern: string): TStringDynArray;
begin
  Result := GetDirectories(Path, SearchPattern, TSearchOption.soTopDirectoryOnly);
end;

class function TDirectory.GetDirectories(const Path, SearchPattern: string;
  const SearchOption: TSearchOption): TStringDynArray;
begin
  CheckGetDirectoriesParameters(Path, SearchPattern);

  Result := DoGetDirectories(Path, SearchPattern, SearchOption, nil);
end;

class function TDirectory.GetDirectories(const Path: string;
  const SearchOption: TSearchOption;
  const Predicate: TFilterPredicate): TStringDynArray;
begin
  CheckGetDirectoriesParameters(Path, '*'); // DO NOT LOCALIZE

  Result := DoGetDirectories(Path, '*', SearchOption, Predicate); // DO NOT LOCALIZE
end;

class function TDirectory.GetDirectories(const Path: string;
  const Predicate: TFilterPredicate): TStringDynArray;
begin
  Result := GetDirectories(Path, '*', TSearchOption.soTopDirectoryOnly, Predicate); // DO NOT LOCALIZE
end;

class function TDirectory.GetDirectories(const Path, SearchPattern: string;
  const Predicate: TFilterPredicate): TStringDynArray;
begin
  Result := GetDirectories(Path, SearchPattern, TSearchOption.soTopDirectoryOnly,
    Predicate);
end;

class function TDirectory.GetDirectories(const Path, SearchPattern: string;
  const SearchOption: TSearchOption;
  const Predicate: TFilterPredicate): TStringDynArray;
begin
  CheckGetDirectoriesParameters(Path, SearchPattern);

  Result := DoGetDirectories(Path, SearchPattern, SearchOption, Predicate);
end;

class function TDirectory.GetDirectoryRoot(const Path: string): string;
begin
  CheckGetDirectoryRootParameters(Path);

  Result := TPath.DoGetPathRoot(TPath.DoGetFullPath(Path));
end;

class function TDirectory.GetFiles(const Path: string): TStringDynArray;
begin
  Result := GetFiles(Path, '*', TSearchOption.soTopDirectoryOnly); // DO NOT LOCALIZE
end;

class function TDirectory.GetFiles(const Path,
  SearchPattern: string): TStringDynArray;
begin
  Result := GetFiles(Path, SearchPattern, TSearchOption.soTopDirectoryOnly);
end;

class function TDirectory.GetFiles(const Path, SearchPattern: string;
  const SearchOption: TSearchOption): TStringDynArray;
begin
  CheckGetFilesParameters(Path, SearchPattern);

  Result := DoGetFiles(Path, SearchPattern, SearchOption, nil);
end;

class function TDirectory.GetFileSystemEntries(
  const Path: string): TStringDynArray;
begin
  Result := GetFileSystemEntries(Path, '*'); // DO NOT LOCALIZE
end;

class function TDirectory.GetFiles(const Path: string;
  const SearchOption: TSearchOption;
  const Predicate: TFilterPredicate): TStringDynArray;
begin
  CheckGetFilesParameters(Path, '*'); // DO NOT LOCALIZE

  Result := DoGetFiles(Path, '*', SearchOption, Predicate); // DO NOT LOCALIZE
end;

class function TDirectory.GetFileSystemEntries(const Path: string;
  const Predicate: TFilterPredicate): TStringDynArray;
begin
  Result := GetFileSystemEntries(Path, '*', Predicate); // DO NOT LOCALIZE
end;

class function TDirectory.GetFileSystemEntries(const Path,
  SearchPattern: string; const Predicate: TFilterPredicate): TStringDynArray;
begin
  CheckGetFileSystemEntriesParameters(Path, SearchPattern);

  Result := DoGetFileSystemEntries(Path, SearchPattern,
    TSearchOption.soTopDirectoryOnly, Predicate);
end;

class function TDirectory.GetFileSystemEntries(const Path,
  SearchPattern: string): TStringDynArray;
begin
  CheckGetFileSystemEntriesParameters(Path, SearchPattern);

  Result := DoGetFileSystemEntries(Path, SearchPattern,
    TSearchOption.soTopDirectoryOnly, nil);
end;

class function TDirectory.GetDirectories(const Path: string): TStringDynArray;
begin
  Result := GetDirectories(Path, '*', TSearchOption.soTopDirectoryOnly); // DO NOT LOCALIZE
end;

class function TDirectory.GetLastAccessTime(const Path: string): TDateTime;
begin
  CheckGetLastAccessTimeParameters(Path);

  Result := DoGetLastAccessTime(Path);
end;

class function TDirectory.GetLastAccessTimeUtc(const Path: string): TDateTime;
begin
  CheckGetLastAccessTimeUtcParameters(Path);

  Result := DoGetLastAccessTimeUtc(Path);
end;

class function TDirectory.GetLastWriteTime(const Path: string): TDateTime;
begin
  CheckGetLastWriteTimeParameters(Path);

  Result := DoGetLastWriteTime(Path);
end;

class function TDirectory.GetLastWriteTimeUtc(const Path: string): TDateTime;
begin
  CheckGetLastWriteTimeUtcParameters(Path);

  Result := DoGetLastWriteTimeUtc(Path);
end;

class function TDirectory.GetLogicalDrives: TStringDynArray;
{$IFDEF MSWINDOWS}
var
  Buff: PChar;
  CurrDrive: PChar;
  BuffLen: Integer;
  ErrCode: Cardinal;
begin
  Result := nil;

  // get the drive strings in a PChar buffer
  SetLastError(ERROR_SUCCESS);
  BuffLen := GetLogicalDriveStrings(0, nil);
  Buff := StrAlloc(BuffLen);
  try
    ErrCode := GetLogicalDriveStrings(BuffLen, Buff);

    // extract the drive strings from the PChar buffer into the Result array
    if ErrCode <> 0 then
    begin
      CurrDrive := Buff;
      repeat
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := CurrDrive;

        CurrDrive := StrEnd(CurrDrive) + 1;
      until CurrDrive^ = #0;
    end;
  finally
    StrDispose(Buff);
  end;
end;
{$ENDIF}
{$IFDEF POSIX}
begin
  { Posix does not support file drives }
  SetLength(Result, 0);
end;
{$ENDIF}

class function TDirectory.GetParent(const Path: string): string;
begin
  CheckGetParentParameters(Path);

  Result := TPath.DoGetDirectoryName(TPath.DoGetFullPath(Path));
end;

class procedure TDirectory.InternalCheckDirPathParam(const Path: string;
    const ExistsCheck: Boolean);
begin
  TPath.CheckPathLength(Path, MAX_PATH {$IFDEF MSWINDOWS}- TFile.FCMinFileNameLen{$ENDIF});
{$IFDEF MSWINDOWS}
  { Windows-only: Check for valid colon char in the path }
  if not TPath.HasPathValidColon(Path) then
    raise ENotSupportedException.CreateRes(@SPathFormatNotSupported);
{$ENDIF MSWINDOWS}

  if Trim(Path) = '' then // DO NOT LOCALIZE
    raise EArgumentException.CreateRes(@SInvalidCharsInPath);
  if not TPath.HasValidPathChars(Path, False) then
    raise EArgumentException.CreateRes(@SInvalidCharsInPath);
  if ExistsCheck and (not Exists(Path)) then
    raise EDirectoryNotFoundException.CreateRes(@SPathNotFound);
end;

class function TDirectory.IsEmpty(const Path: string): Boolean;
var
  PreCallback: TDirectoryWalkProc;
  Empty: Boolean;
begin
  Empty := True;
  InternalCheckDirPathParam(Path, True);

  PreCallback :=
    function (const Path: string; const FileInfo: TSearchRec): Boolean
    begin
      // the directory is empty if it only contains '.' and '..'
      Empty := (FileInfo.Name = TPath.FCParentDir) or (FileInfo.Name = TPath.FCCurrentDir);

      // stop the search as soon as the first file/directory is found
      Result := Empty;
    end;

  WalkThroughDirectory(Path, '*', PreCallback, nil, False); // DO NOT LOCALIZE

  Result := Empty;
end;

class function TDirectory.IsRelativePath(const Path: string): Boolean;
begin
  Result := System.SysUtils.IsRelativePath(Path);
end;

class procedure TDirectory.Move(const SourceDirName, DestDirName: string);
var
  PreCallback: TDirectoryWalkProc;
  PostCallback: TDirectoryWalkProc;
begin
  CheckMoveParameters(SourceDirName, DestDirName);

  PreCallback :=
    function (const Path: string; const FileInfo: TSearchRec): Boolean
    var
      CompletePath: string;
    begin
      Result := True;

      // mirror each directory at the destination
      if (FileInfo.Attr and System.SysUtils.faDirectory <> 0) and
         (FileInfo.Name <> TPath.FCCurrentDir) and (FileInfo.Name <> TPath.FCParentDir) then
      begin
        // the destination is the one given by DestDirName
        if SameFileName(SourceDirName, Path) then
          CompletePath := DestDirName
        // get the difference between Path and SourceDirName
        else
          CompletePath := TPath.DoCombine(DestDirName,
            StuffString(Path, 1, Length(SourceDirName) + Length(TPath.DirectorySeparatorChar), ''), False); // DO NOT LOCALIZE
        CompletePath := TPath.DoCombine(CompletePath, FileInfo.Name, False);

        CreateDir(CompletePath);
      end;
    end;

  PostCallback :=
    function (const Path: string; const FileInfo: TSearchRec): Boolean
    var
      CompleteSrc: string;
      CompleteDest: string;
    begin
      Result := True;

      if (FileInfo.Name <> TPath.FCParentDir) and (FileInfo.Name <> TPath.FCCurrentDir) then
      begin
        case FileInfo.Attr and System.SysUtils.faDirectory of
          System.SysUtils.faDirectory: // remove directories at source
            begin
              CompleteSrc := TPath.DoCombine(Path, FileInfo.Name, False);

              // clear read-only, system and hidden attributes that can compromise
              // the deletion and then remove the directory at source
{$IFDEF MSWINDOWS}
              FileSetAttr(CompleteSrc, System.SysUtils.faNormal);
{$ENDIF}
              RemoveDir(CompleteSrc);
            end;

          0: // move files from source to destination
            begin
              // determine the complete source and destination paths
              CompleteSrc := TPath.DoCombine(Path, FileInfo.Name, False);

              // the destination is the one given by DestDirName
              if SameFileName(SourceDirName, Path) then
                CompleteDest := DestDirName
              // get the difference between Path and SourceDirName
              else
                CompleteDest := TPath.DoCombine(DestDirName,
                  StuffString(Path, 1, Length(SourceDirName) + Length(TPath.DirectorySeparatorChar), ''), False); // DO NOT LOCALIZE
              // add the file name to the destination
              CompleteDest := TPath.DoCombine(CompleteDest, FileInfo.Name, False);

              // clear read-only, system and hidden attributes that can compromise
              // the file displacement, move the file and reset the original
              // file attributes
{$IFDEF MSWINDOWS}
              FileSetAttr(CompleteSrc, System.SysUtils.faNormal);
{$ENDIF MSWINDOWS}
              RenameFile(CompleteSrc, CompleteDest);
{$IFDEF MSWINDOWS}
              FileSetAttr(CompleteDest, FileInfo.Attr);
{$ENDIF MSWINDOWS}
            end;
        end;
      end;
    end;

    // create the destination directory
    TDirectory.CreateDirectory(DestDirName);

    // move all directories and files
    WalkThroughDirectory(SourceDirName, '*', PreCallback, PostCallback, True); // DO NOT LOCALIZE

    // delete the remaining source directory
{$IFDEF MSWINDOWS}
    FileSetAttr(SourceDirName, System.SysUtils.faDirectory);
{$ENDIF MSWINDOWS}
    RemoveDir(SourceDirName);
end;

class procedure TDirectory.SetAttributes(const Path: string;
  const Attributes: TFileAttributes);
begin
  CheckSetAttributesParameters(Path);

  TFile.DoSetAttributes(Path, Attributes);
end;

class procedure TDirectory.SetCreationTime(const Path: string;
  const CreationTime: TDateTime);
begin
  CheckSetCreationTimeParameters(Path);

  DoSetCreationTime(Path, CreationTime);
end;

class procedure TDirectory.SetCreationTimeUtc(const Path: string;
  const CreationTime: TDateTime);
begin
  CheckSetCreationTimeUtcParameters(Path);

  DoSetCreationTimeUtc(Path, CreationTime);
end;

class procedure TDirectory.SetCurrentDirectory(const Path: string);
{$IFDEF MSWINDOWS}
var
  ErrCode: Cardinal;
begin
  CheckSetCurrentDirectoryParameters(Path);

  SetLastError(ERROR_SUCCESS);
  if not SetCurrentDir(Path) then
  begin
    ErrCode := GetLastError;
    if (ErrCode <> ERROR_FILE_NOT_FOUND) and
       (ErrCode <> ERROR_PATH_NOT_FOUND) then
      raise EInOutError.Create(SysErrorMessage(ErrCode));
  end;
end;
{$ENDIF}
{$IFDEF POSIX}
var
  ErrCode: Cardinal;
  M: TMarshaller;
begin
  CheckSetCurrentDirectoryParameters(Path);

{ Try to change the directory }
  ErrCode := __chdir(M.AsAnsi(Path, CP_UTF8).ToPointer);

  { Verify the error code. Fail if not 0 and ENOENT }
  if (ErrCode <> 0) and (ErrCode <> ENOENT) then
      raise EInOutError.Create(SysErrorMessage(ErrCode));
end;
{$ENDIF}

class procedure TDirectory.SetDateTimeInfo(const Path: string;
  const CreationTime, LastAccessTime, LastWriteTime: PDateTime;
  const UseLocalTimeZone: Boolean);
{$IFDEF MSWINDOWS}
var
  LFileHnd: THandle;
  LFileAttr: Cardinal;
  LFileCreationTime: PFileTime;
  LFileLastAccessTime: PFileTime;
  LFileLastWriteTime: PFileTime;
begin
  // establish what date-times must be set to the directory
  LFileHnd := 0;
  LFileCreationTime := nil;
  LFileLastAccessTime := nil;
  LFileLastWriteTime := nil;

  try
    try
      if Assigned(CreationTime) then
      begin
        New(LFileCreationTime);
        LFileCreationTime^ := ConvertDateTimeToFileTime(CreationTime^, UseLocalTimeZone);
      end;
      if Assigned(LastAccessTime) then
      begin
        New(LFileLastAccessTime);
        LFileLastAccessTime^ := ConvertDateTimeToFileTime(LastAccessTime^, UseLocalTimeZone);
      end;
      if Assigned(LastWriteTime) then
      begin
        New(LFileLastWriteTime);
        LFileLastWriteTime^ := ConvertDateTimeToFileTime(LastWriteTime^, UseLocalTimeZone);
      end;

      // determine if Path points to a directory or a file
      SetLastError(ERROR_SUCCESS);
      LFileAttr := FileGetAttr(Path);
      if LFileAttr and System.SysUtils.faDirectory <> 0 then
        LFileAttr := FILE_FLAG_BACKUP_SEMANTICS
      else
        LFileAttr := FILE_ATTRIBUTE_NORMAL;

      // set the new date-times to the directory or file
      LFileHnd := CreateFile(PChar(Path), GENERIC_WRITE, FILE_SHARE_WRITE, nil,
        OPEN_EXISTING, LFileAttr, 0);

      if LFileHnd <> INVALID_HANDLE_VALUE then
        SetFileTime(LFileHnd, LFileCreationTime, LFileLastAccessTime, LFileLastWriteTime);
    except
      on E: EConvertError do
        raise EArgumentOutOfRangeException.Create(E.Message);
    end;
  finally
    CloseHandle(LFileHnd);
    SetLastError(ERROR_SUCCESS);

    Dispose(LFileCreationTime);
    Dispose(LFileLastAccessTime);
    Dispose(LFileLastWriteTime);
  end;
end;
{$ENDIF}
{$IFDEF POSIX}
var
  LFileName: Pointer;
  LStatBuf: _stat;
  LBuf: utimbuf;
  ErrCode: Integer;
  M: TMarshaller;
begin
  { Do nothing if no date/time passed. Ignore CreationTime. Unixes do not support creation times for files. }
  if (LastAccessTime = nil) and (LastWriteTime = nil) then
    Exit;

  LFileName := M.AsAnsi(Path, CP_UTF8).ToPointer;

  { Obtain the file times. lstat may fail }
  if ((LastAccessTime = nil) or (LastWriteTime = nil)) then
  begin
    ErrCode := stat(LFileName, LStatBuf);

    { Fail if we can't access the file properly }
    if ErrCode <> 0 then
      Exit; // Fail here prematurely. Do not chnage file times if we failed to fetch the old ones.
  end;

  try
    { Preserve of set the new value }
    if LastAccessTime <> nil then
      LBuf.actime := ConvertDateTimeToFileTime(LastAccessTime^, UseLocalTimeZone)
    else
      LBuf.actime := LStatBuf.st_atime;

    { Preserve of set the new value }
    if LastWriteTime <> nil then
      LBuf.modtime := ConvertDateTimeToFileTime(LastWriteTime^, UseLocalTimeZone)
    else
      LBuf.modtime := LStatBuf.st_mtime;

    { Call utime to set the file times }
    utime(LFileName, LBuf);
  except
    on E: EConvertError do // May rise in ConvertDateTimeToFileTime
      raise EArgumentOutOfRangeException.Create(E.Message);
  end;
end;
{$ENDIF}

class procedure TDirectory.SetLastAccessTime(const Path: string;
  const LastAccessTime: TDateTime);
begin
  CheckSetLastAccessTimeParameters(Path);

  DoSetLastAccessTime(Path, LastAccessTime);
end;

class procedure TDirectory.SetLastAccessTimeUtc(const Path: string;
  const LastAccessTime: TDateTime);
begin
  CheckSetLastAccessTimeUtcParameters(Path);

  DoSetLastAccessTimeUtc(Path, LastAccessTime);
end;

class procedure TDirectory.SetLastWriteTime(const Path: string;
  const LastWriteTime: TDateTime);
begin
  CheckSetLastWriteTimeParameters(Path);

  DoSetLastWriteTime(PAth, LastWriteTime);
end;

class procedure TDirectory.SetLastWriteTimeUtc(const Path: string;
  const LastWriteTime: TDateTime);
begin
  CheckSetLastWriteTimeUtcParameters(Path);

  DoSetLastWriteTimeUtc(Path, LastWriteTime);
end;

class function TDirectory.GetFiles(const Path, SearchPattern: string;
  const Predicate: TFilterPredicate): TStringDynArray;
begin
  Result := GetFiles(Path, SearchPattern, TSearchOption.soTopDirectoryOnly, Predicate);
end;

class function TDirectory.GetFiles(const Path: string;
  const Predicate: TFilterPredicate): TStringDynArray;
begin
  Result := GetFiles(Path, '*', TSearchOption.soTopDirectoryOnly, Predicate); // DO NOT LOCALIZE
end;

class function TDirectory.GetFiles(const Path, SearchPattern: string;
  const SearchOption: TSearchOption;
  const Predicate: TFilterPredicate): TStringDynArray;
begin
  CheckGetFilesParameters(Path, SearchPattern);

  Result := DoGetFiles(Path, SearchPattern, SearchOption, Predicate);
end;

class function TDirectory.GetFileSystemEntries(const Path: string;
  const SearchOption: TSearchOption;
  const Predicate: TFilterPredicate): TStringDynArray;
begin
  CheckGetFileSystemEntriesParameters(Path, '*'); // DO NOT LOCALIZE

  Result := DoGetFileSystemEntries(Path, '*', SearchOption, Predicate); // DO NOT LOCALIZE
end;

{ TFile }

class procedure TFile.AppendAllText(const Path, Contents: string;
  const Encoding: TEncoding);
var
  LFileStream: TFileStream;
  Buff: TBytes;
begin
  CheckAppendAllTextParameters(Path, Encoding, True);

  LFileStream := nil;
  try
    try
      LFileStream := DoCreateOpenFile(Path);
      Buff := Encoding.GetBytes(Contents);

      LFileStream.Seek(0, TSeekOrigin.soEnd);
      LFileStream.WriteBuffer(Buff, Length(Buff));
    except
      on E: EFileStreamError do
        raise EInOutError.Create(E.Message);
    end;
  finally
    LFileStream.Free;
  end;
end;

class function TFile.AppendText(const Path: string): TStreamWriter;
begin
  CheckAppendTextParameters(Path);

  try
    Result := TStreamWriter.Create(Path, True);
  except
    on E: EFileStreamError do
      raise EInOutError.Create(E.Message);
  end;
end;

class procedure TFile.Copy(const SourceFileName, DestFileName: string);
begin
  Copy(SourceFileName, DestFileName, False);
end;

class procedure TFile.CheckAppendAllTextParameters(const Path: string;
    const Encoding: TEncoding; const NeedEncoding: Boolean);
begin
  if NeedEncoding and (not Assigned(Encoding)) then
    raise EArgumentException.CreateResFmt(@SParamIsNil, ['Encoding']); // DO NOT LOCALIZE

  InternalCheckFilePathParam(Path, False);
end;

class procedure TFile.CheckAppendTextParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, False);
end;

class procedure TFile.CheckCopyParameters(const SourceFileName,
  DestFileName: string; const Overwrite: Boolean);
begin
  InternalCheckFilePathParam(SourceFileName, True);
  InternalCheckFilePathParam(DestFileName, False);

  if (not Overwrite) and Exists(DestFileName) then
    raise EInOutError.CreateRes(@SFileAlreadyExists);
end;

class procedure TFile.CheckCreateParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, False);
end;

class procedure TFile.CheckCreateTextParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, False);
end;

{$IFDEF MSWINDOWS}
class procedure TFile.CheckDecryptParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;
{$ENDIF MSWINDOWS}

class procedure TFile.CheckDeleteParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, False);
end;

{$IFDEF MSWINDOWS}
class procedure TFile.CheckEncryptParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;
{$ENDIF MSWINDOWS}

class procedure TFile.CheckGetAttributesParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;

class procedure TFile.CheckGetCreationTimeParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;

class procedure TFile.CheckGetCreationTimeUtcParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;

class procedure TFile.CheckGetLastAccessTimeParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;

class procedure TFile.CheckGetLastAccessTimeUtcParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;

class procedure TFile.CheckGetLastWriteTimeParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;

class procedure TFile.CheckGetLastWriteTimeUtcParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;

class procedure TFile.CheckMoveParameters(const SourceFileName,
  DestFileName: string);
begin
  InternalCheckFilePathParam(SourceFileName, True);
  InternalCheckFilePathParam(DestFileName, False);

  if Exists(DestFileName) then
    raise EInOutError.CreateRes(@SFileAlreadyExists);
end;

class procedure TFile.CheckOpenParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, False);
end;

class procedure TFile.CheckOpenReadParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;

class procedure TFile.CheckOpenTextParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;

class procedure TFile.CheckOpenWriteParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;

class procedure TFile.CheckReadAllBytesParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;

class procedure TFile.CheckReadAllLinesParameters(const Path: string;
    const Encoding: TEncoding; const NeedEncoding: Boolean);
begin
  if NeedEncoding and (not Assigned(Encoding)) then
    raise EArgumentException.CreateResFmt(@SParamIsNil, ['Encoding']); // DO NOT LOCALIZE

  InternalCheckFilePathParam(Path, True);
end;

class procedure TFile.CheckReadAllTextParameters(const Path: string;
  const Encoding: TEncoding; const NeedEncoding: Boolean);
begin
  if NeedEncoding and (not Assigned(Encoding)) then
    raise EArgumentException.CreateResFmt(@SParamIsNil, ['Encoding']); // DO NOT LOCALIZE

  InternalCheckFilePathParam(Path, True);
end;

{$IFDEF MSWINDOWS}
class procedure TFile.CheckReplaceParameters(const SourceFileName,
  DestinationFileName, DestinationBackupFileName: string);
begin
  InternalCheckFilePathParam(SourceFileName, True);
  InternalCheckFilePathParam(DestinationFileName, True);
  if not (DestinationBackupFileName = '') then
    InternalCheckFilePathParam(DestinationBackupFileName, False);

  if not SameFileName(TPath.DoGetPathRoot(SourceFileName),
    TPath.DoGetPathRoot(DestinationFileName)) then
    raise EInOutError.CreateRes(@SSameRootDrive);

  if SameFileName(SourceFileName, DestinationFileName) then
    raise EInOutError.CreateRes(@SSourceFileIsDestFile);
end;
{$ENDIF MSWINDOWS}

class procedure TFile.CheckSetAttributesParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;

class procedure TFile.CheckSetCreationTimeParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;

class procedure TFile.CheckSetCreationTimeUtcParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;

class procedure TFile.CheckSetLastAccessTimeParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;

class procedure TFile.CheckSetLastAccessTimeUtcParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;

class procedure TFile.CheckSetLastWriteTimeParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;

class procedure TFile.CheckSetLastWriteTimeUtcParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, True);
end;

class procedure TFile.CheckWriteAllBytesParameters(const Path: string);
begin
  InternalCheckFilePathParam(Path, False);
end;

class procedure TFile.CheckWriteAllLinesParameters(const Path: string;
    const Encoding: TEncoding; const NeedEncoding: Boolean);
begin
  if NeedEncoding and (not Assigned(Encoding)) then
    raise EArgumentException.CreateResFmt(@SParamIsNil, ['Encoding']); // DO NOT LOCALIZE

  InternalCheckFilePathParam(Path, False);
end;

class procedure TFile.CheckWriteAllTextParameters(const Path: string;
  const Encoding: TEncoding; const NeedEncoding: Boolean);
begin
  if NeedEncoding and (not Assigned(Encoding)) then
    raise EArgumentException.CreateResFmt(@SParamIsNil, ['Encoding']); // DO NOT LOCALIZE

  InternalCheckFilePathParam(Path, False);
end;

class procedure TFile.Copy(const SourceFileName, DestFileName: string;
  const Overwrite: Boolean);
begin
  CheckCopyParameters(SourceFileName, DestFileName, Overwrite);

  if not DoCopy(SourceFileName, DestFileName, Overwrite) then
    raise EInOutError.Create(SysErrorMessage(GetLastError));
end;

class function TFile.Create(const Path: string;
    const BufferSize: Integer): TFileStream;
begin
  CheckCreateParameters(Path);

  try
    Result := TFileStream.Create(Path, System.Classes.fmCreate);
  except
    on E: EFileStreamError do
      raise EInOutError.Create(E.Message);
  end;
end;

class function TFile.Create(const Path: string): TFileStream;
begin
  Result := Create(Path, 0); // buffer size doesn't matter
end;

class function TFile.CreateSymLink(const Link, Target: string): Boolean;
begin
  Result := FileCreateSymLink(Link, Target);
end;

class function TFile.CreateText(const Path: string): TStreamWriter;
begin
  CheckCreateTextParameters(Path);

  try
    Result := TStreamWriter.Create(Path);
  except
    on E: EFileStreamError do
      raise EInOutError.Create(E.Message);
  end;
end;

class procedure TFile.AppendAllText(const Path, Contents: string);
var
  LFileStream: TFileStream;
  LFileEncoding: TEncoding; // encoding of the file
  Buff: TBytes;
  Preamble: TBytes;
  UTFStr: TBytes;
  UTF8Str: TBytes;
begin
  CheckAppendAllTextParameters(Path, nil, False);

  LFileStream := nil;
  try
    try
      LFileStream := DoCreateOpenFile(Path);
      // detect the file encoding
      LFileEncoding := GetEncoding(LFileStream);

      // file is written is ASCII (default ANSI code page)
      if LFileEncoding = TEncoding.ANSI then
      begin
        // Contents can be represented as ASCII;
        // append the contents in ASCII

        UTFStr := TEncoding.ANSI.GetBytes(Contents);
        UTF8Str := TEncoding.UTF8.GetBytes(Contents);

        if TEncoding.UTF8.GetString(UTFStr) = TEncoding.UTF8.GetString(UTF8Str) then
        begin
          LFileStream.Seek(0, TSeekOrigin.soEnd);
          Buff := TEncoding.ANSI.GetBytes(Contents);
        end
        // Contents can be represented only in UTF-8;
        // convert file and Contents encodings to UTF-8
        else
        begin
          // convert file contents to UTF-8
          LFileStream.Seek(0, TSeekOrigin.soBeginning);
          SetLength(Buff, LFileStream.Size);
          LFileStream.ReadBuffer(Buff, Length(Buff));
          Buff := TEncoding.Convert(LFileEncoding, TEncoding.UTF8, Buff);

          // prepare the stream to rewrite the converted file contents
          LFileStream.Size := Length(Buff);
          LFileStream.Seek(0, TSeekOrigin.soBeginning);
          Preamble := TEncoding.UTF8.GetPreamble;
          LFileStream.WriteBuffer(Preamble, Length(Preamble));
          LFileStream.WriteBuffer(Buff, Length(Buff));

          // convert Contents in UTF-8
          Buff := TEncoding.UTF8.GetBytes(Contents);
        end;
      end
      // file is written either in UTF-8 or Unicode (BE or LE);
      // append Contents encoded in UTF-8 to the file
      else
      begin
        LFileStream.Seek(0, TSeekOrigin.soEnd);
        Buff := TEncoding.UTF8.GetBytes(Contents);
      end;

      // write Contents to the stream
      LFileStream.WriteBuffer(Buff, Length(Buff));
    except
      on E: EFileStreamError do
        raise EInOutError.Create(E.Message);
    end;
  finally
    LFileStream.Free;
  end;
end;

{$IFDEF MSWINDOWS}
class procedure TFile.Decrypt(const Path: string);
begin
  CheckDecryptParameters(Path);

  SetLastError(ERROR_SUCCESS);
  if not DecryptFile(PChar(Path), 0) then
    raise EInOutError.Create(SysErrorMessage(GetLastError));
end;
{$ENDIF}

class procedure TFile.Delete(const Path: string);
{$IFDEF MSWINDOWS}
var
  DeleteCode: Boolean;
  ErrCode: Integer;
begin
  CheckDeleteParameters(Path);

  DeleteCode := DeleteFile(Path);

  ErrCode:=GetLastError;

  if (not DeleteCode) and ((ErrCode = ERROR_SHARING_VIOLATION) or
    (ErrCode = ERROR_FILE_NOT_FOUND) or (ErrCode = ERROR_ACCESS_DENIED)) then
    raise EInOutError.Create(SysErrorMessage(ErrCode));
end;
{$ENDIF}
{$IFDEF POSIX}
var
  ErrCode: Cardinal;
  M: TMarshaller;
begin
  CheckDeleteParameters(Path);

  { Try to delete the file }
  if unlink(M.AsAnsi(Path, CP_UTF8).ToPointer) = -1 then
    ErrCode := GetLastError()
  else
    ErrCode := 0;

  { Throw an error only for EACCES, EBUSY and EPERM}
  if (ErrCode = EACCES) or (ErrCode = EBUSY) or (ErrCode = EPERM) or (ErrCode = ENOENT) then
    raise EInOutError.Create(SysErrorMessage(ErrCode));
end;
{$ENDIF}

class function TFile.DoCopy(const SourceFileName, DestFileName: string;
  const Overwrite: Boolean): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := CopyFile(PChar(SourceFileName), PChar(DestFileName), not Overwrite);
end;
{$ENDIF}
{$IFDEF POSIX}
var
  LSource, LDest: TFileStream;
begin
  { Consider success by default }
  Result := true;

  { If overwrite not supported and the file exists, fail }
  if (not Overwrite) and Exists(DestFileName) then
  begin
    SetLastError(EEXIST);
    Exit(false);
  end;

  { Check source file }
  if not Exists(SourceFileName) then
  begin
    SetLastError(ENOENT);
    Exit(false);
  end;

  { Create the source file stream }
  try
    LSource := TFileStream.Create(SourceFileName, fmOpenRead or fmShareDenyWrite);
  except
    SetLastError(EACCES);
    Exit(false);
  end;

  try
    { Open the file in write/create modes }
    LDest := TFileStream.Create(DestFileName, fmOpenWrite or fmCreate);
    try
      { Here is where the magic happens. }
      LDest.CopyFrom(LSource, LSource.Size);
    except
      SetLastError(EACCES);
      Result := false;
    end;

    { Free destination stream }
    LDest.Free;
  except
    SetLastError(EACCES);
    Result := false;
  end;

  { Free source stream }
  LSource.Free;
end;

{$ENDIF}

class function TFile.DoCreateOpenFile(const Path: string): TFileStream;
var
  Mode: Word;
begin
  if Exists(Path) then
    Mode := System.SysUtils.fmOpenReadWrite or System.SysUtils.fmShareDenyWrite
  else
    Mode := System.Classes.fmCreate;

  Result := TFileStream.Create(Path, Mode);
end;

                                                                                                                     
class function TFile.DoGetAttributes(const Path: string; FollowLink: Boolean = True): TFileAttributes;

{$IFDEF POSIX}
  // This version is here to return POSIX file attributes. The version
  // in SysUtils maps POSIX file attributes to Windows file attributes which
  // is what IntegerToFileAttributes will do.
  function FileGetAttr(const FileName: string; FollowLink: Boolean): Integer;
  var
    st: _stat;
    LFileName: Pointer;
    M: TMarshaller;
  begin
    LFileName := M.AsAnsi(FileName, CP_UTF8).ToPointer;
    if (FollowLink and (stat(LFileName, st) = 0)) or
      (not FollowLink and (lstat(LFileName, st) = 0)) then
      Result := st.st_mode
    else
      Result := 0;
  end;
{$ENDIF POSIX}

begin
  Result := IntegerToFileAttributes(FileGetAttr(Path, FollowLink));
end;

class function TFile.DoReadAllBytes(const Path: string): TBytes;
var
  LFileStream: TFileStream;
begin
  LFileStream := nil;
  try
    LFileStream := OpenRead(Path);
    SetLength(Result, LFileStream.Size);
    LFileStream.ReadBuffer(Result, Length(Result));
  finally
    LFileStream.Free;
  end;
end;

class function TFile.DoReadAllText(const Path: string): string;
var
  Buff: TBytes;
  Encoding: TEncoding;
  BOMLength: Integer;
begin
  Encoding := nil;
  Buff := DoReadAllBytes(Path);
  BOMLength := TEncoding.GetBufferEncoding(Buff, Encoding);
  Result := Encoding.GetString(Buff, BOMLength, Length(Buff) - BOMLength);
end;

class function TFile.DoReadAllText(const Path: string;
  const Encoding: TEncoding): string;
var
  Buff: TBytes;
  BOMLength: Integer;
  FoundEncoding: TEncoding;
begin
  FoundEncoding := Encoding;
  Buff := DoReadAllBytes(Path);
  BOMLength := TEncoding.GetBufferEncoding(Buff, FoundEncoding);
  Result := FoundEncoding.GetString(Buff, BOMLength, Length(Buff) - BOMLength);
end;

                                                                                                                                        
class procedure TFile.DoSetAttributes(const Path: string;
  const Attributes: TFileAttributes);

{$IFDEF POSIX}
  function FileSetAttr(const FileName: string; Attr: Integer): Integer;
  var
    M: TMarshaller;
  begin
    Result := chmod(M.AsAnsi(FileName, CP_UTF8).ToPointer, Attr);
  end;
{$ENDIF POSIX}

begin
  FileSetAttr(Path, FileAttributesToInteger(Attributes));
end;

class procedure TFile.DoWriteAllLines(const Path: string;
  const Contents: TStringDynArray; const Encoding: TEncoding;
  const WriteBOM: Boolean);
var
  StrList: TStringList;
begin
  StrList := nil;
  try
    StrList := GetStringListFromArray(Contents);
    DoWriteAllText(Path, StrList.Text, Encoding, WriteBOM);
  finally
    StrList.Free;
  end;
end;

class procedure TFile.DoWriteAllText(const Path, Contents: string;
  const Encoding: TEncoding; const WriteBOM: Boolean);
var
  LFileStream: TFileStream;
  Buff: TBytes;
begin
  LFileStream := nil;
  try
    LFileStream := Create(Path);
    if WriteBOM then
    begin
      Buff := Encoding.GetPreamble;
      LFileStream.WriteBuffer(Buff, Length(Buff));
    end;
    Buff := Encoding.GetBytes(Contents);
    LFileStream.WriteBuffer(Buff, Length(Buff));
  finally
    LFileStream.Free;
  end;
end;

{$IFDEF MSWINDOWS}
class procedure TFile.Encrypt(const Path: string);
begin
  CheckEncryptParameters(Path);

  SetLastError(ERROR_SUCCESS);
  if not EncryptFile(PChar(Path)) then
    raise EInOutError.Create(SysErrorMessage(GetLastError));
end;
{$ENDIF}

class function TFile.Exists(const Path: string; FollowLink: Boolean = True): Boolean;
begin
  Result := FileExists(Path, FollowLink);
end;

class function TFile.FileAttributesToInteger(
  const Attributes: TFileAttributes): Integer;
{$IFDEF MSWINDOWS}
begin
  Result := 0;

  if TFileAttribute.faReadOnly in Attributes then
    Result := Result or FILE_ATTRIBUTE_READONLY;
  if TFileAttribute.faHidden in Attributes then
    Result := Result or FILE_ATTRIBUTE_HIDDEN;
  if TFileAttribute.faSystem in Attributes then
    Result := Result or FILE_ATTRIBUTE_SYSTEM;
  if TFileAttribute.faDirectory in Attributes then
    Result := Result or FILE_ATTRIBUTE_DIRECTORY;
  if TFileAttribute.faArchive in Attributes then
    Result := Result or FILE_ATTRIBUTE_ARCHIVE;
  if TFileAttribute.faDevice in Attributes then
    Result := Result or FILE_ATTRIBUTE_DEVICE;
  if TFileAttribute.faSymLink in Attributes then
    Result := Result or FILE_ATTRIBUTE_DEVICE;
  if TFileAttribute.faNormal in Attributes then
    Result := Result or FILE_ATTRIBUTE_NORMAL;
  if TFileAttribute.faTemporary in Attributes then
    Result := Result or FILE_ATTRIBUTE_TEMPORARY;
  if TFileAttribute.faSparseFile in Attributes then
    Result := Result or FILE_ATTRIBUTE_SPARSE_FILE;
  if TFileAttribute.faReparsePoint in Attributes then
    Result := Result or FILE_ATTRIBUTE_REPARSE_POINT;
  if TFileAttribute.faCompressed in Attributes then
    Result := Result or FILE_ATTRIBUTE_COMPRESSED;
  if TFileAttribute.faOffline in Attributes then
    Result := Result or FILE_ATTRIBUTE_OFFLINE;
  if TFileAttribute.faNotContentIndexed in Attributes then
    Result := Result or FILE_ATTRIBUTE_NOT_CONTENT_INDEXED;
  if TFileAttribute.faEncrypted in Attributes then
    Result := Result or FILE_ATTRIBUTE_ENCRYPTED;
end;

{$ENDIF}
{$IFDEF POSIX}
begin
  Result := 0;

  if TFileAttribute.faNamedPipe in Attributes then
    Result := Result or S_IFIFO;
  if TFileAttribute.faCharacterDevice in Attributes then
    Result := Result or S_IFCHR;
  if TFileAttribute.faDirectory in Attributes then
    Result := Result or S_IFDIR;
  if TFileAttribute.faBlockDevice in Attributes then
    Result := Result or S_IFBLK;
  if TFileAttribute.faNormal in Attributes then
    Result := Result or S_IFREG;
  if TFileAttribute.faSymLink in Attributes then
    Result := Result or S_IFLNK;
  if TFileAttribute.faSocket in Attributes then
    Result := Result or S_IFSOCK;
{$IFNDEF ANDROID}
  if TFileAttribute.faWhiteout in Attributes then
    Result := Result or S_IFWHT;
{$ENDIF}
  if TFileAttribute.faOwnerRead in Attributes then
    Result := Result or S_IRUSR;
  if TFileAttribute.faOwnerWrite in Attributes then
    Result := Result or S_IWUSR;
  if TFileAttribute.faOwnerExecute in Attributes then
    Result := Result or S_IXUSR;
  if TFileAttribute.faGroupRead in Attributes then
    Result := Result or S_IRGRP;
  if TFileAttribute.faGroupWrite in Attributes then
    Result := Result or S_IWGRP;
  if TFileAttribute.faGroupExecute in Attributes then
    Result := Result or S_IXGRP;
  if TFileAttribute.faOthersRead in Attributes then
    Result := Result or S_IROTH;
  if TFileAttribute.faOthersWrite in Attributes then
    Result := Result or S_IWOTH;
  if TFileAttribute.faOthersExecute in Attributes then
    Result := Result or S_IXOTH;
  if TFileAttribute.faUserIDExecution in Attributes then
    Result := Result or S_ISUID;
  if TFileAttribute.faGroupIDExecution in Attributes then
    Result := Result or S_ISGID;
  if TFileAttribute.faStickyBit in Attributes then
    Result := Result or S_ISVTX;
end;
{$ENDIF}

class function TFile.GetAttributes(const Path: string; FollowLink: Boolean = True): TFileAttributes;
begin
  CheckGetAttributesParameters(Path);

  Result := DoGetAttributes(Path, FollowLink);
end;
class function TFile.GetCreationTime(const Path: string): TDateTime;
begin
  CheckGetCreationTimeParameters(Path);

  Result := TDirectory.DoGetCreationTime(Path);
end;

class function TFile.GetCreationTimeUtc(const Path: string): TDateTime;
begin
  CheckGetCreationTimeUtcParameters(Path);

  Result := TDirectory.DoGetCreationTimeUtc(Path);
end;

class function TFile.GetEncoding(const Stream: TStream): TEncoding;
const
  CMaxPreambleLen = 4;
var
  Buff: TBytes;
begin
  Result := nil;
  Stream.Seek(0, TSeekOrigin.soBeginning);
  SetLength(Buff, Min(Stream.Size, CMaxPreambleLen));
  Stream.ReadBuffer(Buff, Length(Buff));
  TEncoding.GetBufferEncoding(Buff, Result);
end;

class function TFile.GetLastAccessTime(const Path: string): TDateTime;
begin
  CheckGetLastAccessTimeParameters(Path);

  Result := TDirectory.DoGetLastAccessTime(Path);
end;

class function TFile.GetLastAccessTimeUtc(const Path: string): TDateTime;
begin
  CheckGetLastAccessTimeUtcParameters(Path);

  Result := TDirectory.DoGetLastAccessTimeUtc(Path);
end;

class function TFile.GetLastWriteTime(const Path: string): TDateTime;
begin
  CheckGetLastWriteTimeParameters(Path);

  Result := TDirectory.DoGetLastWriteTime(Path);
end;

class function TFile.GetLastWriteTimeUtc(const Path: string): TDateTime;
begin
  CheckGetLastWriteTimeUtcParameters(Path);

  Result := TDirectory.DoGetLastWriteTimeUtc(Path);
end;

class function TFile.GetStringArrayFromText(const Text: string): TStringDynArray;
var
  StrList: TStringList;
  Line: string;
begin
  Result := nil;

  // create a string list that will receive the text and separate it
  // on lines with respect to sLineBreak;
  // add the lines from the string list to the resulting string array
  StrList := TStringList.Create;

  StrList.Text := Text;
  for Line in StrList do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := Line;
  end;

  StrList.Free;
end;

class function TFile.GetStringListFromArray(
  const AnArray: TStringDynArray): TStringList;
var
  Line: string;
begin
  Result := TStringList.Create;

  for Line in AnArray do
    Result.Add(Line);
end;

class function TFile.GetSymLinkTarget(const FileName: string;
  var SymLinkRec: TSymLinkRec): Boolean;
begin
  Result := FileGetSymLinkTarget(FileName, SymLinkRec);
end;

class function TFile.GetSymLinkTarget(const FileName: string;
  var TargetName: string): Boolean;
var
  SymLinkRec: TSymLinkRec;
begin
  Result := FileGetSymLinkTarget(FileName, SymLinkRec);
  TargetName := SymLinkRec.TargetName;
end;

class function TFile.IntegerToFileAttributes(
  const Attributes: Integer): TFileAttributes;
{$IFDEF MSWINDOWS}
begin
  Result := [];

  if Attributes and FILE_ATTRIBUTE_READONLY <> 0 then
    Include(Result, TFileAttribute.faReadOnly);
  if Attributes and FILE_ATTRIBUTE_HIDDEN <> 0 then
    Include(Result, TFileAttribute.faHidden);
  if Attributes and FILE_ATTRIBUTE_SYSTEM <> 0 then
    Include(Result, TFileAttribute.faSystem);
  if Attributes and FILE_ATTRIBUTE_DIRECTORY <> 0 then
    Include(Result, TFileAttribute.faDirectory);
  if Attributes and FILE_ATTRIBUTE_ARCHIVE <> 0 then
    Include(Result, TFileAttribute.faArchive);
  if Attributes and FILE_ATTRIBUTE_DEVICE <> 0 then
    Include(Result, TFileAttribute.faSymLink);
  if Attributes and FILE_ATTRIBUTE_NORMAL <> 0 then
    Include(Result, TFileAttribute.faNormal);
  if Attributes and FILE_ATTRIBUTE_TEMPORARY <> 0 then
    Include(Result, TFileAttribute.faTemporary);
  if Attributes and FILE_ATTRIBUTE_SPARSE_FILE <> 0 then
    Include(Result, TFileAttribute.faSparseFile);
  if Attributes and FILE_ATTRIBUTE_COMPRESSED <> 0 then
    Include(Result, TFileAttribute.faCompressed);
  if Attributes and FILE_ATTRIBUTE_OFFLINE <> 0 then
    Include(Result, TFileAttribute.faOffline);
  if Attributes and FILE_ATTRIBUTE_NOT_CONTENT_INDEXED <> 0 then
    Include(Result, TFileAttribute.faNotContentIndexed);
  if Attributes and FILE_ATTRIBUTE_ENCRYPTED <> 0 then
    Include(Result, TFileAttribute.faEncrypted);
end;
{$ENDIF}
{$IFDEF POSIX}
begin
  Result := [];

  case Attributes and S_IFMT of
    S_IFIFO: Include(Result, TFileAttribute.faNamedPipe);
    S_IFCHR: Include(Result, TFileAttribute.faCharacterDevice);
    S_IFDIR: Include(Result, TFileAttribute.faDirectory);
    S_IFBLK: Include(Result, TFileAttribute.faBlockDevice);
    S_IFREG: Include(Result, TFileAttribute.faNormal);
    S_IFLNK: Include(Result, TFileAttribute.faSymLink);
    S_IFSOCK: Include(Result, TFileAttribute.faSocket);
{$IFNDEF ANDROID}
    S_IFWHT: Include(Result, TFileAttribute.faWhiteout);
{$ENDIF}
  end;
  if Attributes and S_IRUSR <> 0 then
    Include(Result, TFileAttribute.faOwnerRead);
  if Attributes and S_IWUSR <> 0 then
    Include(Result, TFileAttribute.faOwnerWrite);
  if Attributes and S_IXUSR <> 0 then
    Include(Result, TFileAttribute.faOwnerExecute);
  if Attributes and S_IRGRP <> 0 then
    Include(Result, TFileAttribute.faGroupRead);
  if Attributes and S_IWGRP <> 0 then
    Include(Result, TFileAttribute.faGroupWrite);
  if Attributes and S_IXGRP <> 0 then
    Include(Result, TFileAttribute.faGroupExecute);
  if Attributes and S_IROTH <> 0 then
    Include(Result, TFileAttribute.faOthersRead);
  if Attributes and S_IWOTH <> 0 then
    Include(Result, TFileAttribute.faOthersWrite);
  if Attributes and S_IXOTH <> 0 then
    Include(Result, TFileAttribute.faOthersExecute);
  if Attributes and S_ISUID <> 0 then
    Include(Result, TFileAttribute.faUserIDExecution);
  if Attributes and S_ISGID <> 0 then
    Include(Result, TFileAttribute.faGroupIDExecution);
  if Attributes and S_ISVTX <> 0 then
    Include(Result, TFileAttribute.faStickyBit);
end;
{$ENDIF}

class procedure TFile.InternalCheckFilePathParam(const Path: string;
    const FileExistsCheck: Boolean);
begin
  TPath.CheckPathLength(Path, MAX_PATH); // Check path length
{$IFDEF MSWINDOWS}
  { Windows-only: Check for valid colon char in the path }
  if not TPath.HasPathValidColon(Path) then
    raise ENotSupportedException.CreateRes(@SPathFormatNotSupported);
{$ENDIF MSWINDOWS}
  if Trim(Path) = '' then // DO NOT LOCALIZE
    raise EArgumentException.CreateRes(@SInvalidCharsInPath);
  if not TPath.HasValidPathChars(Path, False) then
    raise EArgumentException.CreateRes(@SInvalidCharsInPath);
  if not TDirectory.Exists(TPath.DoGetDirectoryName(TPath.DoGetFullPath(Path))) then
    raise EDirectoryNotFoundException.CreateRes(@SPathNotFound);
  if FileExistsCheck and (not Exists(Path)) then
    raise EFileNotFoundException.CreateRes(@SFileNotFound);
end;

class procedure TFile.Move(SourceFileName, DestFileName: string);
begin
  // get the full source and destination paths
  SourceFileName := TPath.DoGetFullPath(SourceFileName);
  DestFileName := TPath.DoGetFullPath(DestFileName);

  // check the parameters
  CheckMoveParameters(SourceFileName, DestFileName);

{$IFDEF MSWINDOWS}
  { If the source file must be moved somewhere on the same drive. Otherwise copy. }
  if TPath.DoGetPathRoot(SourceFileName) = TPath.DoGetPathRoot(DestFileName) then
  begin
    if not RenameFile(SourceFileName, DestFileName) then
      raise EInOutError.Create(SysErrorMessage(GetLastError));
  end
{$ENDIF}
{$IFDEF POSIX}
  // On POSIX try to rename the file.
  if RenameFile(SourceFileName, DestFileName) then
    Exit;
  // Cannot rename. Check for EXDEV error (diff file systems) and copy/delete then
  if GetLastError <> EXDEV then
    raise EInOutError.Create(SysErrorMessage(GetLastError))
{$ENDIF}
  else
  begin
    { Try to copy. If that succeeds, remove the file }
    if DoCopy(SourceFileName, DestFileName, false) then
      Delete(SourceFileName)
    else
      raise EInOutError.Create(SysErrorMessage(GetLastError));
  end;
end;

class function TFile.Open(const Path: string; const Mode: TFileMode;
  const Access: TFileAccess): TFileStream;
begin
  Result := Open(Path, Mode, Access, TFileShare.fsNone);
end;

class function TFile.Open(const Path: string; const Mode: TFileMode;
  const Access: TFileAccess; const Share: TFileShare): TFileStream;
var
  LFileStrmAccess: Word;
  LFileStrmShare: Word;
begin
  CheckOpenParameters(Path);

  Result := nil;
  LFileStrmAccess := 0;
  LFileStrmShare := 0;

  // determine the access for the file stream
  case Access of
    TFileAccess.faRead:
      LFileStrmAccess := fmOpenRead;
    TFileAccess.faWrite:
      LFileStrmAccess := fmOpenWrite;
    TFileAccess.faReadWrite:
      LFileStrmAccess := fmOpenReadWrite;
  end;

  // determine the sharing for the file stream
  case Share of
    TFileShare.fsNone:
      LFileStrmShare := fmShareExclusive;
    TFileShare.fsRead:
      LFileStrmShare := fmShareDenyWrite;
                                                                                                                           
                                                                      
{$IFDEF MSWINDOWS}
    TFileShare.fsWrite:
      LFileStrmShare := fmShareDenyRead;
{$ENDIF}
    TFileSHare.fsReadWrite:
      LFileStrmShare := fmShareDenyNone;
  end;

  // take the specific action (create, open etc file),
  // depending on the value of Mode
  case Mode of
    TFileMode.fmCreateNew:
      begin
        if Exists(Path) then
          raise EInOutError.CreateRes(@SFileAlreadyExists);

        try
          Result := TFileStream.Create(Path, LFileStrmAccess or LFileStrmShare);
        except
          on E: EFileStreamError do
            raise EInOutError.Create(E.Message);
        end;
      end;
    TFileMode.fmCreate:
      try
        Result := TFileStream.Create(Path, System.Classes.fmCreate or LFileStrmShare);
      except
        on E: EFileStreamError do
          raise EInOutError.Create(E.Message);
      end;
    TFileMode.fmOpen:
      begin
        if not Exists(Path) then
          raise EFileNotFoundException.CreateRes(@SFileNotFound);

        try
          Result := TFileStream.Create(Path, LFileStrmAccess or LFileStrmShare);
        except
          on E: EFileStreamError do
            raise EInOutError.Create(E.Message);
        end;
      end;
    TFileMode.fmOpenOrCreate:
      try
        if Exists(Path) then
          Result := TFileStream.Create(Path, LFileStrmAccess or LFileStrmShare)
        else
          Result := TFileStream.Create(Path, fmCreate or LFileStrmShare);
      except
        on E: EFileStreamError do
          raise EInOutError.Create(E.Message);
      end;
    TFileMode.fmTruncate:
      begin
        if not Exists(Path) then
          raise EFileNotFoundException.CreateRes(@SFileNotFound);

        try
          Result := TFileStream.Create(Path, LFileStrmAccess or LFileStrmShare);
          Result.Size := 0;
        except
          on E: EFileStreamError do
            raise EInOutError.Create(E.Message);
        end;
      end;
    TFileMode.fmAppend:
      try
        if Exists(Path) then
        begin
          Result := TFileStream.Create(Path, LFileStrmAccess or LFileStrmShare);
          Result.Seek(0, TSeekOrigin.soEnd);
        end
        else
          Result := TFileStream.Create(Path, fmCreate or LFileStrmShare);
      except
        on E: EFileStreamError do
          raise EInOutError.Create(E.Message);
      end;
  end;
end;

class function TFile.OpenRead(const Path: string): TFileStream;
begin
  CheckOpenReadParameters(Path);

  try
    Result := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
  except
    on E: EFileStreamError do
      raise EInOutError.Create(E.Message);
  end;
end;

class function TFile.OpenText(const Path: string): TStreamReader;
begin
  CheckOpenTextParameters(Path);

  try
    Result := TStreamReader.Create(Path);
  except
    on E: EFileStreamError do
      raise EInOutError.Create(E.Message);
  end;
end;

class function TFile.OpenWrite(const Path: string): TFileStream;
begin
  CheckOpenWriteParameters(Path);

  try
    Result := TFileStream.Create(Path, fmOpenWrite);
  except
    on E: EFileStreamError do
      raise EInOutError.Create(E.Message);
  end;
end;

class function TFile.ReadAllBytes(const Path: string): TBytes;
begin
  CheckReadAllBytesParameters(Path);

  Result := DoReadAllBytes(Path);
end;

class function TFile.ReadAllLines(const Path: string;
  const Encoding: TEncoding): TStringDynArray;
var
  Text: string;
begin
  CheckReadAllLinesParameters(Path, Encoding, True);

  Text := DoReadAllText(Path, Encoding);
  Result := GetStringArrayFromText(Text);
end;

class function TFile.ReadAllLines(const Path: string): TStringDynArray;
var
  Encoding: TEncoding;
  Buff: TBytes;
  Text: string;
  BOMLength: Integer;
begin
  CheckReadAllLinesParameters(Path, nil, False);

  Encoding := nil;
  Buff := DoReadAllBytes(Path);
  BOMLength := TEncoding.GetBufferEncoding(Buff, Encoding);
  Text := Encoding.GetString(Buff, BOMLength, Length(Buff) - BOMLength);
  Result := GetStringArrayFromText(Text);
end;

class function TFile.ReadAllText(const Path: string): string;
begin
  CheckReadAllTextParameters(Path, nil, False);

  Result := DoReadAllText(Path);
end;

class function TFile.ReadAllText(const Path: string;
  const Encoding: TEncoding): string;
begin
  CheckReadAllTextParameters(Path, Encoding, True);

  Result := DoReadAllText(Path, Encoding);
end;

{$IFDEF MSWINDOWS}
class procedure TFile.Replace(SourceFileName, DestinationFileName,
  DestinationBackupFileName: string; const IgnoreMetadataErrors: Boolean);
var
  Flags: Cardinal;
begin
  SourceFileName := TPath.DoGetFullPath(SourceFileName);
  DestinationFileName := TPath.DoGetFullPath(DestinationFileName);
  DestinationBackupFileName := TPath.DoGetFullPath(DestinationBackupFileName);
  CheckReplaceParameters(SourceFileName, DestinationFileName,
    DestinationBackupFileName);

  Flags := REPLACEFILE_WRITE_THROUGH;
  if IgnoreMetadataErrors then
    Flags := Flags or REPLACEFILE_IGNORE_MERGE_ERRORS;

  SetLastError(ERROR_SUCCESS);
  ReplaceFile(PChar(DestinationFileName), PChar(SourceFileName),
    PChar(DestinationBackupFileName), Flags, nil, nil);
end;
{$ENDIF MSWINDOWS}

class procedure TFile.Replace(const SourceFileName, DestinationFileName,
  DestinationBackupFileName: string);
{$IFDEF MSWINDOWS}
begin
  Replace(SourceFileName, DestinationFileName, DestinationBackupFileName, True);
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
  LSourceFileName, LDestinationFileName, LDestinationBackupFileName: string;
begin
  LSourceFileName := TPath.DoGetFullPath(SourceFileName);
  LDestinationFileName := TPath.DoGetFullPath(DestinationFileName);
  LDestinationBackupFileName := TPath.DoGetFullPath(DestinationBackupFileName);
  if DoCopy(LDestinationFileName,LDestinationBackupFileName,true) then
    if DoCopy(LSourceFileName,LDestinationFileName,true) then
      Delete(LSourceFileName);
end;
{$ENDIF}


class function TFile.Open(const Path: string;
  const Mode: TFileMode): TFileStream;
begin
  Result := Open(Path, Mode, TFileAccess.faReadWrite, TFileShare.fsNone);
end;

class procedure TFile.SetAttributes(const Path: string;
  const Attributes: TFileAttributes);
begin
  CheckSetAttributesParameters(Path);

  DoSetAttributes(Path, Attributes);
end;
class procedure TFile.SetCreationTime(const Path: string;
  const CreationTime: TDateTime);
begin
  CheckSetCreationTimeParameters(Path);

  TDirectory.DoSetCreationTime(Path, CreationTime);
end;

class procedure TFile.SetCreationTimeUtc(const Path: string;
  const CreationTime: TDateTime);
begin
  CheckSetCreationTimeUtcParameters(Path);

  TDirectory.DoSetCreationTimeUtc(Path, CreationTime);
end;

class procedure TFile.SetLastAccessTime(const Path: string;
  const LastAccessTime: TDateTime);
begin
  CheckSetLastAccessTimeParameters(Path);

  TDirectory.DoSetLastAccessTime(Path, LastAccessTime);
end;

class procedure TFile.SetLastAccessTimeUtc(const Path: string;
  const LastAccessTime: TDateTime);
begin
  CheckSetLastAccessTimeUtcParameters(Path);

  TDirectory.DoSetLastAccessTimeUtc(Path, LastAccessTime);
end;

class procedure TFile.SetLastWriteTime(const Path: string;
  const LastWriteTime: TDateTime);
begin
  CheckSetLastWriteTimeParameters(Path);

  TDirectory.DoSetLastWriteTime(Path, LastWriteTime);
end;

class procedure TFile.SetLastWriteTimeUtc(const Path: string;
  const LastWriteTime: TDateTime);
begin
  CheckSetLastWriteTimeUtcParameters(Path);

  TDirectory.DoSetLastWriteTimeUtc(Path, LastWriteTime);
end;

class procedure TFile.WriteAllBytes(const Path: string; const Bytes: TBytes);
var
  LFileStream: TFileStream;
begin
  CheckWriteAllBytesParameters(Path);

  LFileStream := nil;
  try
    LFileStream := Create(Path);
    LFileStream.Size := Length(Bytes);
    LFileStream.Seek(0, TSeekOrigin.soBeginning);
    LFileStream.WriteBuffer(Bytes, Length(Bytes));
  finally
    LFileStream.Free;
  end;
end;

class procedure TFile.WriteAllLines(const Path: string;
  const Contents: TStringDynArray; const Encoding: TEncoding);
var
  StrList: TStringList;
begin
  CheckWriteAllLinesParameters(Path, Encoding, True);

  StrList := GetStringListFromArray(Contents);
  StrList.SaveToFile(Path, Encoding);
  StrList.Free;
end;

class procedure TFile.WriteAllLines(const Path: string;
  const Contents: TStringDynArray);
begin
  CheckWriteAllLinesParameters(Path, nil, False);

  DoWriteAllLines(Path, Contents, TEncoding.UTF8, False);
end;

class procedure TFile.WriteAllText(const Path, Contents: string;
  const Encoding: TEncoding);
begin
  CheckWriteAllTextParameters(Path, Encoding, True);

  DoWriteAllText(Path, Contents, Encoding, True);
end;

class procedure TFile.WriteAllText(const Path, Contents: string);
begin
  CheckWriteAllTextParameters(Path, nil, False);

  DoWriteAllText(Path, Contents, TEncoding.UTF8, False);
end;

{ TPath }

class function TPath.ChangeExtension(const Path, Extension: string): string;
var
  SeparatorIdx: Integer;
  CopyIdx: Integer;
begin
  Result := ''; // DO NOT LOCALIZE
  if Path <> '' then // DO NOT LOCALIZE
  begin
    if not HasValidPathChars(Path, True) then
      raise EArgumentException.CreateRes(@SInvalidCharsInPath);

    // get the separator position in Path; if it does not exist add it
    // set SeparatorIdx points to the index of the separator char in Path
    Result := Path;
    SeparatorIdx := GetExtensionSeparatorPos(Result);

    if SeparatorIdx = 0 then
    begin
      Result := Result + ExtensionSeparatorChar;
      SeparatorIdx := Length(Result);
    end;

    // replace the characters following the Path separator char with Extension
    // if Extension contains a separator char, do not include it
    if Extension <> '' then // DO NOT LOCALIZE
      if Extension.Chars[0] <> ExtensionSeparatorChar then
        CopyIdx := 1
      else
        CopyIdx := 2
    else // Extension is an empty string; value of CopyIdx does not matter
      CopyIdx := 1;

    Result := StuffString(Result, SeparatorIdx + 1, Length(Result) - SeparatorIdx,
      Copy(Extension, CopyIdx, Length(Extension) - CopyIdx + 1));
  end;
end;

class procedure TPath.CheckPathLength(const Path: string; const MaxLength: Integer);
begin
{$IFDEF MSWINDOWS}
  if (Length(Path) >= MaxLength) and (not TPath.IsExtendedPrefixed(Path)) then // Check the length in Chars on Win32
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  if (Length(UTF8Encode(Path)) >= MaxLength) then // Check the length in bytes on POSIX
{$ENDIF POSIX}
    raise EPathTooLongException.CreateRes(@SPathTooLong);
end;

class function TPath.Combine(const Path1, Path2: string): string;
begin
  Result := DoCombine(Path1, Path2, True);
end;

class constructor TPath.Create;
begin
  Randomize;

  { Common on all platforms }
  FAltDirectorySeparatorChar := '/'; // DO NOT LOCALIZE;
  FExtensionSeparatorChar := '.';    // DO NOT LOCALIZE;
  FFileNameWildcardChars := TCharArray.Create('*', '?'); // DO NOT LOCALIZE;

{$IFDEF MSWINDOWS}
  FDirectorySeparatorChar := '\';    // DO NOT LOCALIZE;
  FPathSeparator := ';';             // DO NOT LOCALIZE;
  FVolumeSeparatorChar := ':';       // DO NOT LOCALIZE;

  FInvalidPathChars := TCharArray.Create(
    #0, #1, #2, #3, #4, #5, #6, #7, #8, #9, #10, #11, #12,
    #13, #14, #15, #16, #17, #18, #19, #20, #21, #22, #23, #24,
    #25, #26, #27, #28, #29, #30, #31,
    '"', '<', '>', '|');            // DO NOT LOCALIZE;

  FInvalidFileNameChars := TCharArray.Create(
    #0, #1, #2, #3, #4, #5, #6, #7, #8, #9, #10, #11, #12,
    #13, #14, #15, #16, #17, #18, #19, #20, #21, #22, #23, #24,
    #25, #26, #27, #28, #29, #30, #31,
    '"', '*', '/', ':', '<', '>', '?', '\', '|');  // DO NOT LOCALIZE;

  FPathWildcardChars := TCharArray.Create('*', '/', ':', '?', '\'); // DO NOT LOCALIZE;
{$ENDIF}
{$IFDEF POSIX}
  FDirectorySeparatorChar := '/';    // DO NOT LOCALIZE;
  FPathSeparator := ':';             // DO NOT LOCALIZE;
  FVolumeSeparatorChar := #0;        // Not supported on Unix;

  FInvalidPathChars := TCharArray.Create(
    #0, #1, #2, #3, #4, #5, #6, #7, #8, #9, #10, #11, #12,
    #13, #14, #15, #16, #17, #18, #19, #20, #21, #22, #23, #24,
    #25, #26, #27, #28, #29, #30, #31);

  FInvalidFileNameChars := TCharArray.Create(
    #0, #1, #2, #3, #4, #5, #6, #7, #8, #9, #10, #11, #12,
    #13, #14, #15, #16, #17, #18, #19, #20, #21, #22, #23, #24,
    #25, #26, #27, #28, #29, #30, #31, '/', '~');

  FPathWildcardChars := TCharArray.Create('*', '?', '/'); // DO NOT LOCALIZE;
{$ENDIF}
end;

class function TPath.DoCombine(const Path1, Path2: string;
  const ValidateParams: Boolean): string;
var
  Ch: Char;
begin
  // if one path is empty, return the other one
  if Path1 = '' then // DO NOT LOCALIZE
    Result := Path2
  else
    if Path2 = '' then // DO NOT LOCALIZE
      Result := Path1
    else
    begin
      // paths are not empty strings; check if they have invalid chars
      if ValidateParams then
      begin
        if not HasValidPathChars(Path1, True) then
          raise EArgumentException.CreateRes(@SInvalidCharsInPath);
        if not HasValidPathChars(Path2, True) then
          raise EArgumentException.CreateRes(@SInvalidCharsInPath);
      end;

      // if Path2 is absolute, return it; if not, combine the paths
      if IsPathRooted(Path2) or IsExtendedPrefixed(Path2) then
        Result := Path2
      else
      begin
        Ch := Path1[High(Path1)];

        if not IsPathSeparator(Ch) then
          Result := Path1 + DirectorySeparatorChar + Path2
        else
          Result := Path1 + Path2;
      end;
    end;
end;

class function TPath.DoGetDirectoryName(FileName: string): string;
{$IFDEF MSWINDOWS}
var
  Prefix: TPathPrefixType;
  Separators: string;
  SeparatorIdx: Integer;
  StartIdx: Integer;
  Count: Integer;
  SepFound: Boolean;
  FileNameLen: Integer;
begin
  Result := ''; // DO NOT LOCALIZE
  FileName := Trim(FileName);

  // search the last separator and copy the directory string up to it
  Separators := DirectorySeparatorChar + AltDirectorySeparatorChar;
  SeparatorIdx := LastDelimiter(Separators, FileName);
  //SeparatorIdx := Filename.LastDelimiter(Separators);
  StartIdx := GetPosAfterExtendedPrefix(FileName, Prefix);
  SepFound := SeparatorIdx > 0;
  FileNameLen := FileName.Length;
  Count := 0;

  // there is a directory separator in the path
  if SepFound then
  begin
    // the path is UNC rooted
    if IsUNCRooted(FileName) then
    begin
      // path is UNC rooted without prefix; jump over the 2 leading backslashes \\
      if Prefix = TPathPrefixType.pptNoPrefix then
        Inc(StartIdx, 2);

      // search for the separator between the server name and share name;
      // add 1 to StartIdx because the server name should be at least one char
      StartIdx := FindDelimiter(Separators, FileName, StartIdx + 1);

      // separator found; search for the separator after the share name
      if StartIdx > 0 then
        StartIdx := FindDelimiter(Separators, FileName, StartIdx + 1);

      // if either separator was not found, then copy nothing from the path
      if StartIdx = 0 then
        Count := 0
      // the separator was found; copy everything up to the last separator
      else
        Count := SeparatorIdx - 1;
    end
    else
      // the path is drive rooted
      if IsDriveRooted(FileName) then
        // the directory separator is part of the root
        if (FileName.Chars[SeparatorIdx - 2] = VolumeSeparatorChar) then
          // FileName has a prefixed root drive
          if PrefixExtendsPath(Prefix) then
            Count := SeparatorIdx - 1
          // it is a normal unprefixed path
          else
            // FileName does not contain only the root drive; copy the root drive
            if (SeparatorIdx < FileNameLen) then
              Count := SeparatorIdx
            else // it contains only the root drive; copy nothing
              Count := 0
        // the directory separator is not part of the root (C:\)
        else
          // the separator is not part of the prefix; it separates directories
          if SeparatorIdx > StartIdx then
            Count := SeparatorIdx - 1
          else // the separator is part of the prefix; copy nothing
            Count := 0
      // the path is not rooted either by a drive or UNC;
      // check if the path is rooted by a backslash
      else
        // the path must not be prefixed for the following cases
        if not PrefixExtendsPath(Prefix) then
          // the only delimiter is the first char that is a path separator
          if SeparatorIdx = StartIdx then
            // a directory name is specified after the separator; copy only the delimiter
            if SeparatorIdx < FileNameLen then
              Count := 1
            else // only the separator is present; copy nothing
              Count := 0
          // the path starts with a directory name; copy everything up to the separator
          else
            Count := SeparatorIdx - 1
        else // the path is prefixed; copy nothing
          Count := 0;
  end
  // there is no directory separator
  else
    // the path is drive rooted using the current directory
    if IsDriveRooted(FileName) then
    begin
      Count := PosEx(VolumeSeparatorChar, FileName, StartIdx);

      // the path ends in the VolumeSeparatorChar; return nothing
      if Count = FileNameLen then
        Count := 0;
    end;

  //Result := Copy(FileName, 1, Count);
  Result := FileName.SubString(0, Count);
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  { Re-route to System.SysUtils.ExtractFileDir for POSIX }
  Result := System.SysUtils.ExtractFileDir(FileName);
end;
{$ENDIF POSIX}

class function TPath.DoGetFileName(const FileName: string;
  const ValidateParam: Boolean): string;
var
  SeparatorIdx: Integer;
begin
  Result := ''; // DO NOT LOCALIZE
  if FileName <> '' then // DO NOT LOCALIZE
  begin
    if ValidateParam and (not HasValidPathChars(FileName, True)) then
      raise EArgumentException.CreateRes(@SInvalidCharsInPath);

    // search for the first separator
{$IFDEF MSWINDOWS}
    SeparatorIdx := LastDelimiter(DirectorySeparatorChar + AltDirectorySeparatorChar + VolumeSeparatorChar, FileName);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
    SeparatorIdx := LastDelimiter(DirectorySeparatorChar, FileName);
{$ENDIF POSIX}

    // cut the file name on the right of the separator
    if SeparatorIdx > 0 then
      Result := Copy(FileName, SeparatorIdx + 1, Length(FileName) - SeparatorIdx)
    else
      Result := FileName;
  end;
end;


class function TPath.DoGetFullPath(const Path: string): string;
{$IFDEF MSWINDOWS}
var
  ErrCode: Integer;
begin
  SetLastError(ERROR_SUCCESS);
  Result := ExpandFileName(Path);
  ErrCode := GetLastError;
  if (ErrCode <> ERROR_SUCCESS) and (ErrCode <> ERROR_FILE_NOT_FOUND) then
    raise ENotSupportedException.Create(SysErrorMessage(ErrCode));
end;
{$ENDIF}
{$IFDEF POSIX}
begin
  { Simply call the SysUtils-implemented version }
  Result := ExpandFileName(Path);
end;
{$ENDIF}

class function TPath.DoGetPathRoot(const Path: string): string;
{$IFDEF MSWINDOWS}
var
  Prefix: TPathPrefixType;
  Separators: string;
  NeedSeparator: Boolean;
  StartIdx: Integer;
  Count: Integer;
begin
  Separators := DirectorySeparatorChar + AltDirectorySeparatorChar;
  NeedSeparator := False;
  StartIdx := GetPosAfterExtendedPrefix(Path, Prefix);
  Count := 0;

  // Path is rooted by a drive
  if IsDriveRooted(Path) then
  begin
    Count := StartIdx + 1;
    NeedSeparator := (Length(Path) > 2) and IsDelimiter(Separators, Path, StartIdx + 2);

    // if the root is prefixed and needs a separator at the end, do not add it
    if PrefixExtendsPath(Prefix) and NeedSeparator then
      NeedSeparator := False;
  end
  else
    // Path has root in UNC style
    if IsUNCRooted(Path) then
    begin
      // for both prefixed and unprefixed paths, StartIdx must jump over the
      // first 2 backslashes \\
      StartIdx := 3;

      // search for the delimiter between server name and share name
      StartIdx := FindDelimiter(Separators, Path, StartIdx);

      // delimiter found; data that must be copied is up to the separator (including)
      if StartIdx > 0 then
      begin
        Count := StartIdx;

        // search for the delimiter after the share name
        StartIdx := FindDelimiter(Separators, Path, StartIdx + 1);
        // delimiter found; copy data up to it
        if StartIdx > 0 then
          Count := StartIdx - 1
      end;

      // delimiter not found; copy all remaining data
      if StartIdx = 0 then
        Count := Length(Path);
    end
    else
      // path is rooted in Linux style or has two \\
      Count := Ord(IsDelimiter(Separators, Path, StartIdx)) +
               Ord(IsDelimiter(Separators, Path, StartIdx + 1));

  Result := Copy(Path, 1, Count);
  if NeedSeparator then
    Result := Result + DirectorySeparatorChar;
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  { Check the first char of the path (must be /) for rooted paths on Unixes }
  if (Path <> '') and (Path.Chars[0] = DirectorySeparatorChar) then // DO NOT LOCALIZE
    Result := DirectorySeparatorChar
  else
    Result := '';
end;
{$ENDIF POSIX}

class function TPath.DoIsPathRooted(const Path: string;
  const ValidateParam: Boolean): Boolean;
{$IFDEF MSWINDOWS}
var
  PPath: PChar;
  Prefix: TPathPrefixType;
  StartIdx: Integer;
  Len: Integer;
begin
  Result := False;
  if Path <> '' then // DO NOT LOCALIZE
  begin
    if ValidateParam and (not HasValidPathChars(Path, False)) then
      raise EArgumentException.CreateRes(@SInvalidCharsInPath);

    // Path is rooted if either it starts with Directory-/AltDirectory- SeparatorChar,
    // either the second char is a VolumeSeparatorChar
    StartIdx := GetPosAfterExtendedPrefix(Path, Prefix);
    PPath := PChar(Path);
    Len := Length(Path) - StartIdx + 1;
    // check if drive rooted
    Result := (Len >= 2) and
              (PPath[StartIdx] = VolumeSeparatorChar);
    // check if backslash rooted without prefix (so it is valid)
    Result := Result or
              (Prefix = TPathPrefixType.pptNoPrefix) and
              (Len >= 1) and ((PPath[StartIdx - 1] = DirectorySeparatorChar) or
                              (PPath[StartIdx - 1] = AltDirectorySeparatorChar));
    // check if UNC rooted with prefix
    Result := Result or
              (Prefix = TPathPrefixType.pptExtendedUNC) and
              IsValidPathChar(PPath[StartIdx - 1]);
  end;
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  { Check the first char of the path (must be /) for rooted paths on Unixes }
  Result := (Path <> '') and (Path[Low(string)] = DirectorySeparatorChar); // DO NOT LOCALIZE
end;
{$ENDIF POSIX}

class function TPath.DoMatchesPattern(const FileName, Pattern: string): Boolean;
begin
  if Pattern = '*.*' then
    Result := True // Matches everything
  else
    Result := MatchesMask(FileName, Pattern);
end;

class function TPath.DriveExists(const Path: string): Boolean;
{$IFDEF MSWINDOWS}
const
  CFirstDrive = 'A'; // DO NOT LOCALIZE
  CLastDrive = 'Z'; // DO NOT LOCALIZE
var
  Drives: Cardinal;
  StartIdx: Integer;
  PathDrive: Char;
  BitNo: Byte;
begin
  Result := False;
  if (Path <> '') and IsDriveRooted(Path) then // DO NOT LOCALIZE
  begin
    StartIdx := GetPosAfterExtendedPrefix(Path);
    Drives := GetLogicalDrives;
    PathDrive := UpCase(Path.Chars[StartIdx - 1]);

    if (PathDrive >= CFirstDrive) and (PathDrive <= CLastDrive) then
    begin
      BitNo := Ord(PathDrive) - Ord(CFirstDrive);
      Result := (Drives shr BitNo) and 1 = 1;
    end;
  end;
end;
{$ENDIF}
{$IFDEF POSIX}
begin
  { Unixes do not implement drive letters }
  Result := False;
end;
{$ENDIF}

{$IFDEF MACOS}
class function TPath.InternalGetMACOSPath(const SearchedPath: NSSearchPathDirectory; const SearchMask: NSSearchPathDomainMask): string;
var
  nsFile: NSFileManager;
  URL: NSURL;
begin
  nsFile := TNSFileManager.Wrap(TNSFileManager.OCClass.defaultManager);
  URL := nsFile.URLForDirectory(SearchedPath, SearchMask, nil, true, nil);
  if (URL <> nil) then
    Result := UTF8ToString(URL.path.UTF8String)
  else
    Result := ''; // iOS. Return an empty folder if we do not have permissions.
end;
{$ENDIF MACOS}

{$IFDEF LINUX}
class function TPath.InternalXDGGetUserDir(const AType: TXDGUserDirType): string;
const
  USER_DIRS_STRINGS: array[TXDGUserDirType] of string = (
    'DESKTOP', 'DOWNLOAD', 'TEMPLATES', 'PUBLICSHARE', 'DOCUMENTS', 'MUSIC', 'PICTURES', 'VIDEOS'); // DO NOT LOCALIZE
var
  Idx: Integer;
  UserDir: string;
  HomeDir: string;
  ConfigHome: string;
  ConfigFile: string;
  ConfigList: TStringList;
begin
  Result := '';

  HomeDir := GetHomePath;
  if HomeDir.IsEmpty then
    Exit;

  ConfigHome := GetEnvironmentVariable('XDG_CONFIG_HOME'); // Do not localize
  if ConfigHome.IsEmpty then
    ConfigFile := TPath.Combine(HomeDir, '.config/user-dirs.dirs') // Do not localize
  else
    ConfigFile := TPath.Combine(ConfigHome, 'user-dirs.dirs'); // Do not localize

  if not FileExists(ConfigFile) then
    Exit;

  ConfigList := TStringList.Create;
  try
    ConfigList.LoadFromFile(ConfigFile);

    Idx := ConfigList.IndexOfName('XDG_' + USER_DIRS_STRINGS[AType] + '_DIR'); // Do not localize
    if Idx < 0 then
      Exit;

    UserDir := ConfigList.ValueFromIndex[Idx];
    if UserDir.IsEmpty then
      Exit;

    Result := UserDir.Replace('$HOME', HomeDir, [rfIgnoreCase]).DeQuotedString('"'); // Do not localize
  finally
    FreeAndNil(ConfigList);
  end;
end;
{$ENDIF LINUX}

class function TPath.GetAttributes(const Path: string; FollowLink: Boolean = True): TFileAttributes;
begin
  InternalCheckPathParam(Path, True);

  Result := TFile.DoGetAttributes(Path, FollowLink);
end;

class function TPath.GetCachePath: string;
{$IFDEF MSWINDOWS}
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);

  if SHGetFolderPath(0, CSIDL_LOCAL_APPDATA , 0, 0, @LStr) = S_OK then
    Result := LStr;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.InternalGetMACOSPath(NSCachesDirectory, NSUserDomainMask);
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  Result := Androidapi.IOUtils.GetCacheDir;
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  Result := Combine(GetHomePath, '.cache'); // DO NOT LOCALIZE
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

class function TPath.GetDirectoryName(FileName: string): string;
begin
  TPath.CheckPathLength(FileName, MAX_PATH); // Check path length

  if Trim(FileName) = '' then // DO NOT LOCALIZE
    raise EArgumentException.CreateRes(@SInvalidCharsInFileName);
  if not HasValidPathChars(FileName, True) then
    raise EArgumentException.CreateRes(@SInvalidCharsInFileName);

  Result := DoGetDirectoryName(FileName);
end;

class function TPath.GetDocumentsPath: string;
{$IFDEF MSWINDOWS}
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);

  if SHGetFolderPath(0, CSIDL_PERSONAL, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.InternalGetMACOSPath(NSDocumentDirectory, NSUserDomainMask);
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  Result := GetFilesDir;
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  Result := InternalXDGGetUserDir(TXDGUserDirType.Documents);
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

class function TPath.GetSharedDocumentsPath: string;
{$IFDEF MSWINDOWS}
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);

  if SHGetFolderPath(0, CSIDL_COMMON_DOCUMENTS, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.InternalGetMACOSPath(NSSharedPublicDirectory, NSUserDomainMask);
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  Result := GetSharedDocumentsDir;
  ForceDirectories(Result);
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  Result := InternalXDGGetUserDir(TXDGUserDirType.Documents);
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

class function TPath.GetExtension(const FileName: string): string;
var
  SeparatorIdx: Integer;
begin
  Result := ''; // DO NOT LOCALIZE
  if FileName <> '' then // DO NOT LOCALIZE
  begin
    if not HasValidPathChars(FileName, True) then
      raise EArgumentException.CreateRes(@SInvalidCharsInFileName);

    SeparatorIdx := GetExtensionSeparatorPos(FileName);
    if SeparatorIdx > 0 then
      Result := Copy(FileName, SeparatorIdx, Length(FileName) - SeparatorIdx + 1);
  end;
end;

class function TPath.GetExtensionSeparatorPos(const FileName: string): Integer;
type
  TStateType = (stFound, stError, stCycle);
var
  State: TStateType;
  Ch: Char;
begin
  Result := Length(FileName);
  State := TStateType.stCycle;
  while (State = TStateType.stCycle) and (Result >= 1) do
  begin
    Ch := FileName.Chars[Result - 1];

    if Ch = ExtensionSeparatorChar then
      State := TStateType.stFound
    else
      if (Ch = DirectorySeparatorChar) or (Ch = AltDirectorySeparatorChar) then
        State := TStateType.stError
      else
        Dec(Result);
  end;

  if State in [TStateType.stCycle, TStateType.stError] then
    Result := 0;
end;

class function TPath.GetFileName(const FileName: string): string;
begin
  Result := DoGetFileName(FileName, True);
end;

class function TPath.GetFileNameWithoutExtension(const FileName: string): string;
var
  SeparatorIdx: Integer;
begin
  Result := ''; // DO NOT LOCALIZE
  if FileName <> '' then // DO NOT LOCALIZE
  begin
    // the checking for invalid characters is done in GetFileName
    Result := GetFileName(FileName);

    SeparatorIdx := GetExtensionSeparatorPos(Result);
    if SeparatorIdx > 0 then
      Result := Copy(Result, 1, SeparatorIdx - 1);
  end;
end;

class function TPath.GetFullPath(const Path: string): string;
begin
  TPath.CheckPathLength(Path, MAX_PATH); // Check path length

  if (Trim(Path) = '') then // DO NOT LOCALIZE
    raise EArgumentException.CreateRes(@SInvalidCharsInPath);
  if not HasValidPathChars(Path, True) then
    raise EArgumentException.CreateRes(@SInvalidCharsInPath);

  Result := DoGetFullPath(Path);
end;

class function TPath.GetGUIDFileName(const UseSeparator: Boolean = False): string;
var
  Guid: TGUID;
begin
  Result := ''; // DO NOT LOCALIZE
  if CreateGUID(Guid) = S_OK then
  begin
    Result := GUIDToString(Guid);
    Result := Copy(Result, 2, Length(Result) - 1 - 1); // without { and }
    if not UseSeparator then
      Result := ReplaceStr(Result, '-', ''); // DO NOT LOCALIZE
  end;
end;

class function TPath.GetInvalidFileNameChars: TCharArray;
begin
  Result := FInvalidFileNameChars;
end;

class function TPath.GetInvalidPathChars: TCharArray;
begin
  Result := FInvalidPathChars;
end;

class function TPath.GetLibraryPath: string;
{$IFDEF MSWINDOWS}
begin
  Result := ExtractFilePath(ParamStr(0));
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.InternalGetMACOSPath(NSLibraryDirectory, NSUserDomainMask);
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  Result := AndroidApi.IOUtils.GetLibraryPath;
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  Result := GetCurrentDir;
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

class function TPath.GetPathRoot(const Path: string): string;
begin
  if Trim(Path) = '' then // DO NOT LOCALIZE
    raise EArgumentException.CreateRes(@SInvalidCharsInPath);
  if not HasValidPathChars(Path, True) then
    raise EArgumentException.CreateRes(@SInvalidCharsInPath);

  Result := DoGetPathRoot(Path);
end;

class function TPath.GetPicturesPath: string;
{$IFDEF MSWINDOWS}
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);

  if SHGetFolderPath(0, CSIDL_MYPICTURES, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.InternalGetMACOSPath(NSPicturesDirectory, NSUserDomainMask);
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  Result := GetExternalPicturesDir;
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  Result := InternalXDGGetUserDir(TXDGUserDirType.Pictures);
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

class function TPath.GetSharedPicturesPath: string;
{$IFDEF MSWINDOWS}
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);

  if SHGetFolderPath(0, CSIDL_COMMON_PICTURES, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.InternalGetMACOSPath(NSSharedPublicDirectory, NSUserDomainMask);
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  Result := GetSharedPicturesDir;
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  Result := InternalXDGGetUserDir(TXDGUserDirType.Pictures);
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

class function TPath.GetCameraPath: string;
{$IFDEF MSWINDOWS}
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);

  if SHGetFolderPath(0, CSIDL_MYPICTURES, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.InternalGetMACOSPath(NSPicturesDirectory, NSUserDomainMask);
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  Result := GetExternalCameraDir;
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  Result := InternalXDGGetUserDir(TXDGUserDirType.Pictures);
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

class function TPath.GetSharedCameraPath: string;
{$IFDEF MSWINDOWS}
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);

  if SHGetFolderPath(0, CSIDL_COMMON_PICTURES, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.InternalGetMACOSPath(NSSharedPublicDirectory, NSUserDomainMask);
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  Result := GetSharedCameraDir;
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  Result := InternalXDGGetUserDir(TXDGUserDirType.Pictures);
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

class function TPath.GetMusicPath: string;
{$IFDEF MSWINDOWS}
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);

  if SHGetFolderPath(0, CSIDL_MYMUSIC, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.InternalGetMACOSPath(NSMusicDirectory, NSUserDomainMask);
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  Result := GetExternalMusicDir;
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  Result := InternalXDGGetUserDir(TXDGUserDirType.Music);
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

class function TPath.GetSharedMusicPath: string;
{$IFDEF MSWINDOWS}
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);

  if SHGetFolderPath(0, CSIDL_COMMON_MUSIC, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.InternalGetMACOSPath(NSSharedPublicDirectory, NSUserDomainMask);
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  Result := GetSharedMusicDir;
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  Result := InternalXDGGetUserDir(TXDGUserDirType.Music);
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

class function TPath.GetMoviesPath: string;
{$IFDEF MSWINDOWS}
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);

  if SHGetFolderPath(0, CSIDL_MYVIDEO, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.InternalGetMACOSPath(NSMoviesDirectory, NSUserDomainMask);
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  Result := GetExternalMoviesDir;
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  Result := InternalXDGGetUserDir(TXDGUserDirType.Videos);
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

class function TPath.GetSharedMoviesPath: string;
{$IFDEF MSWINDOWS}
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);

  if SHGetFolderPath(0, CSIDL_COMMON_VIDEO, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.InternalGetMACOSPath(NSSharedPublicDirectory, NSUserDomainMask);
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  Result := GetSharedMoviesDir;
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  Result := InternalXDGGetUserDir(TXDGUserDirType.Videos);
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

class function TPath.GetAlarmsPath: string;
{$IFDEF MSWINDOWS}
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);

  if SHGetFolderPath(0, CSIDL_MYMUSIC, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.InternalGetMACOSPath(NSMusicDirectory, NSUserDomainMask);
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  Result := GetExternalAlarmsDir;
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  Result := InternalXDGGetUserDir(TXDGUserDirType.Music);
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

class function TPath.GetSharedAlarmsPath: string;
{$IFDEF MSWINDOWS}
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);

  if SHGetFolderPath(0, CSIDL_COMMON_MUSIC, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.InternalGetMACOSPath(NSSharedPublicDirectory, NSUserDomainMask);
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  Result := GetSharedAlarmsDir;
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  Result := InternalXDGGetUserDir(TXDGUserDirType.Music);
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

class function TPath.GetDownloadsPath: string;
{$IFDEF MSWINDOWS}
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);
  if SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.InternalGetMACOSPath(NSDownloadsDirectory, NSUserDomainMask);
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  Result := GetExternalDownloadsDir;
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  Result := InternalXDGGetUserDir(TXDGUserDirType.Downloads);
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

class function TPath.GetSharedDownloadsPath: string;
{$IFDEF MSWINDOWS}
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);
  if SHGetFolderPath(0, CSIDL_COMMON_APPDATA, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.InternalGetMACOSPath(NSSharedPublicDirectory, NSUserDomainMask);
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  Result := GetSharedDownloadsDir;
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  Result := InternalXDGGetUserDir(TXDGUserDirType.Downloads);
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

class function TPath.GetRingtonesPath: string;
{$IFDEF MSWINDOWS}
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);

  if SHGetFolderPath(0, CSIDL_MYMUSIC, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.InternalGetMACOSPath(NSMusicDirectory, NSUserDomainMask);
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  Result := GetExternalRingtonesDir;
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  Result := InternalXDGGetUserDir(TXDGUserDirType.Music);
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

class function TPath.GetSharedRingtonesPath: string;
{$IFDEF MSWINDOWS}
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);

  if SHGetFolderPath(0, CSIDL_COMMON_MUSIC, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.InternalGetMACOSPath(NSSharedPublicDirectory, NSUserDomainMask);
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  Result := GetSharedRingtonesDir;
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  Result := InternalXDGGetUserDir(TXDGUserDirType.Music);
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

class function TPath.GetPublicPath: string;
{$IFDEF MSWINDOWS}
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);
  if SHGetFolderPath(0, CSIDL_COMMON_APPDATA, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF MACOS}
begin
  Result := TPath.InternalGetMACOSPath(NSSharedPublicDirectory, NSUserDomainMask);
end;
{$ENDIF MACOS}
{$IFDEF ANDROID}
begin
  Result := GetExternalFilesDir;
end;
{$ENDIF ANDROID}
{$IFDEF LINUX}
begin
  Result := InternalXDGGetUserDir(TXDGUserDirType.PublicShare);
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

{$IFDEF MSWINDOWS}
class function TPath.GetPosAfterExtendedPrefix(const Path: string;
  out Prefix: TPathPrefixType): Integer;
begin
  Prefix := GetExtendedPrefix(Path);
  case Prefix of
    TPathPrefixType.pptNoPrefix:
      Result := 1;
    TPathPrefixType.pptExtended:
      Result := Length(FCExtendedPrefix) + 1;
    TPathPrefixType.pptExtendedUNC:
      Result := Length(FCExtendedUNCPrefix) + 1;
  else
    Result := 1;
  end;
end;

class function TPath.GetPosAfterExtendedPrefix(const Path: string): Integer;
var
  Prefix: TPathPrefixType;
begin
  Result := GetPosAfterExtendedPrefix(Path, Prefix);
end;
{$ENDIF MSWINDOWS}

class function TPath.GetRandomFileName: string;
type
  TIntervalType = (iDigit, iUpperLetter, iLowerLetter);
  TIntervalRec = record
    BaseCharCode: Byte;
    RandomCount: Byte;
  end;
  TIntervals = array [TIntervalType] of TIntervalRec;
const
  CIntervals: TIntervals = (
    (BaseCharCode: Ord('0'); RandomCount: Ord('9') - Ord('0') + 1), // DO NOT LOCALIZE
    (BaseCharCode: Ord('A'); RandomCount: Ord('Z') - Ord('A') + 1), // DO NOT LOCALIZE
    (BaseCharCode: Ord('a'); RandomCount: Ord('z') - Ord('a') + 1)); // DO NOT LOCALIZE
  CFileNameLen = TFile.FCMinFileNameLen;
  CDotIndex: Byte = 9;
var
  Interval: TIntervalType;
  i: Byte;
begin
  Result := ''; // DO NOT LOCALIZE
  for i := 1 to CFileNameLen do
    if i <> CDotIndex then
    begin
      Interval := TIntervalType(Random(Ord(High(Interval)) + 1));
      Result := Result + Chr(CIntervals[Interval].BaseCharCode + Random(CIntervals[Interval].RandomCount) );
    end
    else
      Result := Result + ExtensionSeparatorChar;
end;

class function TPath.GetTempFileName: string;
{$IFDEF MSWINDOWS}
var
  TempPath: string;
  ErrCode: UINT;
begin
  TempPath := GetTempPath;
  SetLength(Result, MAX_PATH);

  SetLastError(ERROR_SUCCESS);
  ErrCode := Winapi.Windows.GetTempFileName(PChar(TempPath), 'tmp', 0, PChar(Result)); // DO NOT LOCALIZE
  if ErrCode = 0 then
    raise EInOutError.Create(SysErrorMessage(GetLastError));

  SetLength(Result, StrLen(PChar(Result)));
end;
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF LINUX}
  function RandomName: string;
  var
    LRand: TGUID;
  begin
    LRand := TGUID.NewGuid;
    Result := Format('File_%8x%4x%4x%16x_tmp', [LRand.D1, LRand.D2, LRand.D3, PInt64(@LRand.D4[0])^]);
  end;
begin
  { Obtain a temporary file name }
  repeat
    Result := TPath.Combine(GetTempPath, RandomName);
  until (not FileExists(Result, False));
end;
{$ELSE !LINUX}
var
  LTempPath: TBytes;
  M: TMarshaller;
  LRet: MarshaledAString;
begin
//   char * tempnam(const char *dir, const char *pfx);

  { Obtain a temporary file name }
  // This code changed from getting the temp name from the temp path via system
  // to get the temp name inside the specified temp path. We get the system temp path.
//  LTempPath := TEncoding.UTF8.GetBytes(string(tmpnam(nil)));
  LRet := tempnam(MarshaledAString(M.AsUTF8(GetTempPath).ToPointer),nil);
  LTempPath := TEncoding.UTF8.GetBytes(string(LRet));
  free(LRet);

  { Convert to UTF16 or leave blank on possible error }
  if LTempPath <> nil then
    Result := TEncoding.UTF8.GetString(LTempPath)
  else
    Result := '';
end;
{$ENDIF LINUX}
{$ENDIF POSIX}

class function TPath.GetTempPath: string;
{$IFDEF MSWINDOWS}
var
  Len: Integer;
begin
  SetLastError(ERROR_SUCCESS);

  // get memory for the buffer retaining the temp path (plus null-termination)
  SetLength(Result, MAX_PATH);
  Len := Winapi.Windows.GetTempPath(MAX_PATH, PChar(Result));
  if Len <> 0 then
  begin
    Len := GetLongPathName(PChar(Result), nil, 0);
    GetLongPathName(PChar(Result), PChar(Result), Len);
    SetLength(Result, Len - 1);
  end
  else
    Result := '';
end;
{$ENDIF}
{$IFDEF POSIX}
{$IF defined(IOS) and defined(CPUARM)}
begin
  // This is the only temporary path that we can write under IOS Device
  result := ExpandFileName('~/tmp/');
end;
{$ELSEIF defined(ANDROID)}
begin
  Result := GetExternalFilesDir+'/tmp';
  // Ensure that the folder exists.
  ForceDirectories(Result);
end;
{$ELSE}
const
  CTmpDir = '/tmp'; // Do not localize

var
  LTempPathVar: MarshaledAString;
begin
  Result := CTmpDir;

  LTempPathVar := getenv('TMP');
  if LTempPathVar = nil then
  begin
    LTempPathVar := getenv('TMPDIR');
    if LTempPathVar = nil then
      LTempPathVar := getenv('TEMP');
  end;

  if LTempPathVar <> nil then
    Result := UTF8ToString(LTempPathVar);
end;
{$ENDIF defined(IOS) and defined(CPUARM)}
{$ENDIF POSIX}

class function TPath.GetHomePath: string;
begin
  Result := System.SysUtils.GetHomePath;
end;

class function TPath.HasExtension(const Path: string): Boolean;
begin
  Result := False;
  if Path <> '' then // DO NOT LOCALIZE
  begin
    if not HasValidPathChars(Path, True) then
      raise EArgumentException.CreateRes(@SInvalidCharsInPath);

    Result := GetExtensionSeparatorPos(Path) > 0;
  end;
end;

class function TPath.HasValidFileNameChars(const FileName: string;
  const UseWildcards: Boolean): Boolean;
var
  PFileName: PChar;
  FileNameLen: Integer;
  Ch: Char;
  I: Integer;
begin
  // Result will become True if an invalid file name char is found
{$IFDEF MSWINDOWS}
  I := GetPosAfterExtendedPrefix(FileName) - 1;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  I := 0;
{$ENDIF POSIX}

  PFileName := PChar(FileName);
  FileNameLen := Length(FileName);
  Result := False;
  while (not Result) and (I < FileNameLen) do
  begin
    Ch := PFileName[I];

    if not IsValidFileNameChar(Ch) then
      if UseWildcards then
        if not IsFileNameWildcardChar(Ch) then
          Result := True
        else
          Inc(I)
      else
        Result := True
    else
      Inc(I);
  end;

  Result := not Result;
end;

class function TPath.HasValidPathChars(const Path: string;
  const UseWildcards: Boolean): Boolean;
var
  PPath: PChar;
  PathLen: Integer;
  Ch: Char;
  I: Integer;
begin
  // Result will become True if an invalid path char is found
{$IFDEF MSWINDOWS}
  I := GetPosAfterExtendedPrefix(Path) - 1;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  I := 0;
{$ENDIF POSIX}

  PPath := PChar(Path);
  PathLen := Length(Path);
  Result := False;

  while (not Result) and (i < PathLen) do
  begin
    Ch := PPath[i];
    if not IsValidPathChar(Ch) then
      if UseWildcards then
        if not IsPathWildcardChar(Ch) then
          Result := True
        else
          Inc(i)
      else
        Result := True
    else
      Inc(i);
  end;

  Result := not Result;
end;

{$IFDEF MSWINDOWS}
class function TPath.HasPathValidColon(const Path: string): Boolean;
var
  StartIdx: Integer;
begin
  Result := True;
  if Trim(Path) <> '' then // DO NOT LOCALIZE
  begin
    StartIdx := GetPosAfterExtendedPrefix(Path);
    if TPath.IsDriveRooted(Path) then
      Inc(StartIdx, 2);

    Result := PosEx(TPath.VolumeSeparatorChar, Path, StartIdx) = 0;
  end;
end;
{$ENDIF MSWINDOWS}

class procedure TPath.InternalCheckPathParam(const Path: string; const ExistsCheck: Boolean);
begin
  TPath.CheckPathLength(Path, MAX_PATH);
{$IFDEF MSWINDOWS}
  { Windows-only: Check for valid colon char in the path }
  if not TPath.HasPathValidColon(Path) then
    raise ENotSupportedException.CreateRes(@SPathFormatNotSupported);
{$ENDIF MSWINDOWS}

  if Trim(Path) = '' then // DO NOT LOCALIZE
    raise EArgumentException.CreateRes(@SInvalidCharsInPath);
  if not TPath.HasValidPathChars(Path, False) then
    raise EArgumentException.CreateRes(@SInvalidCharsInPath);
  if ExistsCheck and (not TFile.Exists(Path)) and (not TDirectory.Exists(Path)) then
      raise EPathNotFoundException.CreateRes(@SPathNotFound);
end;

class function TPath.IsCharInOrderedArray(const AChar: Char; const AnArray: TCharArray): Boolean;
var
  LeftIdx, RightIdx: Integer;
  MidIdx: Integer;
  MidChar: Char;
begin
  // suppose AChar is not present in AnArray
  Result := False;

  // the code point of AChar is in the range of the chars bounding the string;
  // use divide-et-impera to search AChar in AnArray
  LeftIdx := 0;
  RightIdx := Length(AnArray) - 1;
  if (RightIdx >= 0) and (AnArray[LeftIdx] <= AChar) and (AChar <= AnArray[RightIdx]) then
    repeat
      MidIdx := LeftIdx + (RightIdx - LeftIdx) div 2;
      MidChar := AnArray[MidIdx];
      if AChar < MidChar then
        RightIdx := MidIdx - 1
      else
        if AChar > MidChar then
          LeftIdx := MidIdx + 1
        else
          Result := True;
    until (Result) or (LeftIdx > RightIdx);
end;

class function TPath.IsDriveRooted(const Path: string): Boolean;
{$IFDEF MSWINDOWS}
type
  TDriveSet = set of Byte;
const
  CDriveSet: TDriveSet = [Ord('A')..Ord('Z')];
var
  PPath: PChar;
  StartIdx: Integer;
begin
  Result := False;
  if Length(Path) >= 2 then
  begin
    StartIdx := GetPosAfterExtendedPrefix(Path);
    PPath := PChar(Path) + StartIdx - 1;
    Result := (Ord(UpCase(PPath[0])) in CDriveSet) and (PPath[1] = VolumeSeparatorChar);
  end;
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  Result := false;  // No drives on Unixes
end;
{$ENDIF POSIX}

class function TPath.IsExtendedPrefixed(const Path: string): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := PrefixExtendsPath(GetExtendedPrefix(Path));
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  Result := false;  // No support for extended prefixes on Unixes
end;
{$ENDIF POSIX}

class function TPath.GetExtendedPrefix(const Path: string): TPathPrefixType;
{$IFDEF MSWINDOWS}
begin
  Result := TPathPrefixType.pptNoPrefix;
  if Path <> '' then
  begin
    if Path.StartsWith(FCExtendedUNCPrefix) then
      Result := TPathPrefixType.pptExtendedUNC
    else
      if Path.StartsWith(FCExtendedPrefix) then
        Result := TPathPrefixType.pptExtended;
  end;
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  Result := TPathPrefixType.pptNoPrefix;  // No support for extended prefixes on Unixes
end;
{$ENDIF POSIX}

class function TPath.IsFileNameWildcardChar(const AChar: Char): Boolean;
begin
  Result := IsCharInOrderedArray(AChar, FFileNameWildcardChars);
end;

class function TPath.IsPathWildcardChar(const AChar: Char): Boolean;
begin
  Result := IsCharInOrderedArray(AChar, FPathWildcardChars);
end;

class function TPath.IsRelativePath(const Path: string): Boolean;
begin
  Result := System.SysUtils.IsRelativePath(Path);
end;

class function TPath.IsUNCPath(const Path: string): Boolean;
begin
  Result := IsUNCRooted(Path) and (HasValidPathChars(Path, True));
end;

class function TPath.IsUNCRooted(const Path: string): Boolean;
{$IFDEF MSWINDOWS}
var
  PPath: PChar;
begin
  Result := False;
  if Length(Path) >= 3 then
  begin
    PPath := PChar(Path);
    if (PPath[0] = DirectorySeparatorChar) and
       (PPath[1] = DirectorySeparatorChar) then
      case PPath[2] of
        '?': // DO NOT LOCALIZE
          Result := GetExtendedPrefix(Path) = TPathPrefixType.pptExtendedUNC;
      else
        Result := IsValidPathChar(PPath[2]);
      end;
  end;
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  Result := false;  // No support for Windows-isms on Unixes
end;
{$ENDIF POSIX}

class function TPath.IsValidFileNameChar(const AChar: Char): Boolean;
begin
  Result := not IsCharInOrderedArray(AChar, FInvalidFileNameChars);
end;

class function TPath.IsValidPathChar(const AChar: Char): Boolean;
begin
  Result := not IsCharInOrderedArray(AChar, FInvalidPathChars);
end;

class function TPath.MatchesPattern(const FileName, Pattern: string;
  const CaseSensitive: Boolean): Boolean;
begin
  if not HasValidFileNameChars(FileName, False) then
    raise EArgumentException.CreateRes(@SInvalidCharsInPath);

  Result := DoMatchesPattern(FileName, Pattern);
end;

{$IFDEF MSWINDOWS}
class function TPath.PrefixExtendsPath(const Prefix: TPathPrefixType): Boolean;
begin
  Result := Prefix in [TPathPrefixType.pptExtended, TPathPrefixType.pptExtendedUNC];
end;
{$ENDIF MSWINDOWS}

class procedure TPath.SetAttributes(const Path: string;
  const Attributes: TFileAttributes);
begin
  InternalCheckPathParam(Path, False);

  TFile.DoSetAttributes(Path, Attributes);
end;

class function TPath.IsPathRooted(const Path: string): Boolean;
begin
  Result := DoIsPathRooted(Path, True);
end;

class function TPath.IsPathSeparator(const AChar: Char): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := (AChar = DirectorySeparatorChar) or
            (AChar = AltDirectorySeparatorChar) or
            (AChar = VolumeSeparatorChar);
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  Result := AChar = DirectorySeparatorChar;
end;
{$ENDIF}

end.
