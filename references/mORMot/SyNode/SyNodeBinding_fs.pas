/// `fs` module support bindings for SyNode
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SyNodeBinding_fs;

interface

{$I Synopse.inc}
{$I SyNode.inc}

uses
  SynCommons,
  SpiderMonkey,
  SyNode;

function os_realpath(const FileName: SynUnicode; var TargetName: SynUnicode): Boolean;

implementation

uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE}
  Unix, BaseUnix, DateUtils,
{$ENDIF}
  Classes;

/// decode text file to string using BOM
//  if BOM not fount - use current system code page to convert ANSI content to unicode
//  if file not found - return empty string
//  internaly use AnyTextFileToRawUTF8
//  accept one parameter  - "path to file"
function fs_loadFile(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
const
  USAGE = 'usage loadFile(pathToFile: String, [forceUFT8: boolean])';
var
  in_argv: PjsvalVector;
  forceUTF8: boolean;
  name: String;
begin
  forceUTF8 := true;
  result := true;
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString or ((argc > 1) and not in_argv[1].isBoolean )then
      raise ESMException.Create(USAGE);

    if argc > 1 then
      forceUTF8 := in_argv[1].asBoolean;

    name := in_argv[0].asJSString.ToString(cx);
    if not FileExists(name) then
      raise ESMException.Create('file  not exists');
    // implementation below dont work if called in the same time from differnt thread
    // TFileStream.Create(name, fmOpenRead).Free; // Check that file exists and can be opened;
    vp.rval := cx.NewJSString(AnyTextFileToRawUTF8(name, forceUTF8)).ToJSVal;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

/// call RelToAbs
function fs_relToAbs(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
const
  USAGE = 'usage: relToAbs(ABase, ATail: string;)';
var
  in_argv: PjsvalVector;
  baseDir, fileName, resPath: TFileName;
begin
  try
    in_argv := vp.argv;
    if (argc <> 2) or not in_argv[0].isString or not in_argv[1].isString then
      raise ESMException.Create(USAGE);
    baseDir := in_argv[0].asJSString.ToString(cx);
    fileName := in_argv[1].asJSString.ToString(cx);
    resPath := RelToAbs(baseDir, fileName);
    vp.rval := cx.NewJSString(StringToSynUnicode(resPath)).ToJSVal;
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

/// return object{
//    atime: Date, //access time
//    mtime: Date, //modify time
//    ctime: Date  // create time
//    size: Number
//  }
// or null is file not exists
function fs_fileStat(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
const
  USAGE = 'usage: fileStat(filePath: string;)';
var
  in_argv: PjsvalVector;
  fn: TFileName;
  obj: PJSRootedObject;
  val: jsval;
{$IFNDEF MSWINDOWS}
  info: stat;
{$ELSE}
  {$ifdef ISDELPHIXE2}
  infoRec: TDateTimeInfoRec;
  {$else}
  fad: TWin32FileAttributeData;

  function fileTimeToDateTime(ft: TFileTime): TDateTime;
  var ST,LT: TSystemTime;
  begin
    if FileTimeToSystemTime(ft,ST) and
     SystemTimeToTzSpecificLocalTime(nil,ST,LT) then
    result := SystemTimeToDateTime(LT) else
    result := 0;
  end;
  {$endif}
{$ENDIF}
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString then
      raise ESMException.Create(USAGE);
    try
      fn := in_argv[0].asJSString.ToSynUnicode(cx);
    except
      raise
    end;
    obj := cx.NewRootedObject(cx.NewObject(nil));
    try
      {$IFNDEF MSWINDOWS}
      if fpstat(fn, info) <> 0 then
        raise EOSError.CreateFmt('fstat failed with error no #', [fpgeterrno]);
      val.asDate[cx] := UnixToDateTime(info.st_atime);
      obj.ptr.DefineProperty(cx, 'atime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
      val.asDate[cx] := UnixToDateTime(info.st_mtime);
      obj.ptr.DefineProperty(cx, 'mtime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
      val.asDate[cx] := UnixToDateTime(info.st_ctime);
      obj.ptr.DefineProperty(cx, 'ctime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
      val.asInt64 := info.st_size;
      obj.ptr.DefineProperty(cx, 'size', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
      vp.rval := obj.ptr.ToJSValue;
      {$ELSE}
      {$ifdef ISDELPHIXE2}
      if FileGetDateTimeInfo(fn, infoRec, true) then begin
        val.asDate[cx] := infoRec.LastAccessTime;
        obj.ptr.DefineProperty(cx, 'atime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asDate[cx] := infoRec.TimeStamp;
        obj.ptr.DefineProperty(cx, 'mtime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asDate[cx] := infoRec.CreationTime;
        obj.ptr.DefineProperty(cx, 'ctime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
          if TWin32FindData(infoRec).nFileSizeHigh > 0 then
            val := JSVAL_NAN
          else
            val.asInt64 := TWin32FindData(infoRec).nFileSizeLow;
        obj.ptr.DefineProperty(cx, 'size', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        vp.rval := obj.ptr.ToJSValue;
      end else
        vp.rval := JSVAL_NULL;
      {$else ISDELPHIXE2}
      if GetFileAttributesEx(PChar(fn), GetFileExInfoStandard, @fad) then begin
        val.asDate[cx] := fileTimeToDateTime(fad.ftLastAccessTime);
        obj.ptr.DefineProperty(cx, 'atime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asDate[cx] := fileTimeToDateTime(fad.ftLastWriteTime);
        obj.ptr.DefineProperty(cx, 'mtime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asDate[cx] := fileTimeToDateTime(fad.ftCreationTime);
        obj.ptr.DefineProperty(cx, 'ctime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        if fad.nFileSizeHigh > 0 then
          val := JSVAL_NAN
        else
          val.asInt64 := fad.nFileSizeLow;
        obj.ptr.DefineProperty(cx, 'size', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        vp.rval := obj.ptr.ToJSValue;
      end else
        vp.rval := JSVAL_NULL;
      {$endif ISDELPHIXE2}
      {$ENDIF MSWINDOWS}
    finally
      cx.FreeRootedObject(obj);
    end;
    Result := True;
  except
    on E: Exception do begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

/// check directory exist
//  accept one parameter  - "path to dir"
function fs_directoryExists(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
const
  USAGE = 'usage: directoryExists(dirPath: string;)';
var
  in_argv: PjsvalVector;
  filePath: string;
  val: jsval;
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString then
      raise ESMException.Create(USAGE);
    filePath := in_argv[0].asJSString.ToSynUnicode(cx);
    val.asBoolean := DirectoryExists(filePath);
    vp.rval := val;
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

/// check file exist
//  accept one parameter  - "path to file"
function fs_fileExists(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
const
  USAGE = 'usage: fileExists(filePath: string;)';
var
  in_argv: PjsvalVector;
  filePath: string;
  val: jsval;
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString then
      raise ESMException.Create(USAGE);
    filePath := in_argv[0].asJSString.ToSynUnicode(cx);
    val.asBoolean := FileExists(filePath);
    vp.rval := val;
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

/// Used to speed up module loading.  Returns the contents of the file as
// a string or undefined when the file cannot be opened.  The speedup
// comes from not creating Error objects on failure.
function fs_internalModuleReadFile(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
const
  USAGE = 'usage: internalModuleReadFile(filePath: string;)';
var
  in_argv: PjsvalVector;
  filePath: string;
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString then
      raise ESMException.Create(USAGE);
    filePath := in_argv[0].asJSString.ToString(cx);
    if FileExists(filePath) then
      vp.rval := cx.NewJSString(AnyTextFileToRawUTF8(filePath, true)).ToJSVal
    else
      vp.rval := JSVAL_VOID;

    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

/// Used to speed up module loading.  Returns 0 if the path refers to
// a file, 1 when it's a directory or < 0 on error (usually -ENOENT.)
// The speedup comes from not creating thousands of Stat and Error objects.
function fs_internalModuleStat(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
const
  USAGE = 'usage: internalModuleStat(filePath: string;)';
var
  in_argv: PjsvalVector;
  fn: TFileName;
  Code: Integer;
  val: jsval;
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString then
      raise ESMException.Create(USAGE);
    try
      fn := in_argv[0].asJSString.ToSynUnicode(cx);
    except
      raise
    end;
    {$IFDEF MSWINDOWS}
    Code := GetFileAttributes(PChar(fn));
    if Code <> -1 then
      if Code and FILE_ATTRIBUTE_DIRECTORY = 0 then
        Code := 0
      else
        Code := 1;
    {$ELSE} // this can be used in Windows too
    Code := FileGetAttr(fn);
    if Code <> -1 then
      if Code and faDirectory = 0 then
        Code := 0
      else
        Code := 1;
    {$ENDIF}
    val.asInteger := Code;
    vp.rval := val;

    Result := True;
  except
    on E: Exception do begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

/// read content of directory. return array of file names witout started from dot '.'
/// in case of includeDirNames - return sub direcrory with trailing slash
function fs_readDir(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  dir, founded: TFileName;
  F: TSearchRec;
  res: PJSRootedObject;
  cNum, searchAttr: integer;
  includeFolders: boolean;
const
  USAGE = 'usage: readDir(dirPath: String; [includeFolders: boolean = false]): Array';
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString then
      raise ESMException.Create(USAGE);
    if (argc = 2) and in_argv[1].isBoolean then
      includeFolders := in_argv[1].asBoolean
    else
      includeFolders := false;

    dir := in_argv[0].asJSString.ToSynUnicode(cx);
    if not DirectoryExists(Dir) then
    begin
      vp.rval := JSVAL_NULL;
      Result := true;
      Exit;
    end;

    Dir := IncludeTrailingPathDelimiter(Dir);

    res := cx.NewRootedObject(cx.NewArrayObject(0));
    try
      {$WARN SYMBOL_PLATFORM OFF}
      searchAttr := faAnyFile;
      if not includeFolders then
        searchAttr := searchAttr - faDirectory;
      if FindFirst(Dir + '*.*', searchAttr, F) = 0 then begin
        cNum := 0;
        repeat
          if (F.Name <> '.') and (F.Name <> '..') then begin
            founded := F.Name;
            res.ptr.SetElement(cx, cNum, cx.NewJSString(founded).ToJSVal);
            inc(cNum);
          end;
        until FindNext(F) <> 0;
      end;
      {$WARN SYMBOL_PLATFORM ON}
      {$ifdef ISDELPHIXE2}System.{$ENDIF}SysUtils.FindClose(F);
      vp.rval := res.ptr.ToJSValue;
    finally
      cx.FreeRootedObject(res);
    end;
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

{$ifdef MSWINDOWS}
{$ifdef FPC}
const VOLUME_NAME_DOS = $0;
function GetFinalPathNameByHandleW(hFile: THandle; lpszFilePath: LPWSTR;
  cchFilePath, dwFlags: DWORD): DWORD; stdcall; external kernel32;
{$endif}

const LONG_PATH_PREFIX: PWideChar = '\\?\';
const UNC_PATH_PREFIX: PWideChar = '\\?\UNC\';
{$else}
function realpath(name: PChar; resolved: PChar): PChar; cdecl; external 'c' name 'realpath';
{$endif}

/// Implementation of realpath as in libuv
function os_realpath(const FileName: SynUnicode; var TargetName: SynUnicode): Boolean;
{$ifdef MSWINDOWS}
var
  Handle: THandle;
  w_realpath_len: Cardinal;
begin
  Result := False;
  if not CheckWin32Version(6, 0) then
    exit;
  Handle := CreateFileW(pointer(FileName), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_BACKUP_SEMANTICS, 0);
  if Handle = INVALID_HANDLE_VALUE then
    exit;
  try
    w_realpath_len := GetFinalPathNameByHandleW(Handle, nil, 0, VOLUME_NAME_DOS);
    if (w_realpath_len = 0) then
      exit;
    SetLength(TargetName, w_realpath_len);
    if GetFinalPathNameByHandleW(Handle, PWideChar(TargetName), w_realpath_len, VOLUME_NAME_DOS) > 0 then begin
      if StrLComp(PWideChar(TargetName), UNC_PATH_PREFIX, length(UNC_PATH_PREFIX)) = 0 then begin
        // convert \\?\UNC\host\folder -> \\host\folder
        TargetName := Copy(TargetName, 7, length(TargetName)-7);
        TargetName[1] := '\';
      end else if StrLComp(PWideChar(TargetName), LONG_PATH_PREFIX, length(LONG_PATH_PREFIX)) = 0 then
        TargetName := Copy(TargetName, length(LONG_PATH_PREFIX)+1, length(TargetName)-length(LONG_PATH_PREFIX)-1)
      else
       Exit; // error
      Result := True;
    end;
  finally
    CloseHandle(Handle);
  end;
end;
{$else}
var
  Buf: AnsiString;
begin
  SetLength(Buf, PATH_MAX);
  Result := realpath(PChar(SynUnicodeToString(FileName)), PChar(Buf)) <> nil;
  if Result then
    TargetName := PChar(Buf)
  else
    TargetName := '';
end;
{$endif}

/// return the canonicalized absolute pathname
// expands all symbolic links and resolves references to /./,
// /../ and extra '/' characters in the string named by path
// to produce a canonicalized absolute pathname
function fs_realPath(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  dir, target: SynUnicode;
const
  USAGE = 'usage: realpath(dirPath: String): string';
begin
  try
    if (argc < 1) or not vp.argv[0].isString then
      raise ESMException.Create(USAGE);

    dir := vp.argv[0].asJSString.ToSynUnicode(cx);
    //if not FileGetSymLinkTarget(dir, target) then
    if not os_realpath(dir, target) then
      target := dir;

    vp.rval := cx.NewJSString(target).ToJSVal;
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function fs_rename(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
const
  USAGE = 'usage: rename(fromPath, toPath: String)';
var
  fromPath, toPath: SynUnicode;
  f : file;
begin
  try
    if (argc <> 2) or not vp.argv[0].isString or not vp.argv[1].isString then
      raise ESMException.Create(USAGE);
    fromPath := vp.argv[0].asJSString.ToSynUnicode(cx);
    toPath := vp.argv[1].asJSString.ToSynUnicode(cx);
    AssignFile(f, fromPath);
    // libc rename implementation rewrite destination if it exists
    // Rename in delphi fail with Access deny. let's fix it
    if FileExists(toPath) then
      SysUtils.DeleteFile(toPath);
    Rename(f, toPath);
    Result := True;
  except
    on E: Exception do begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function SyNodeBindingProc_fs(const Engine: TSMEngine;
  const bindingNamespaceName: SynUnicode): jsval;
const
  attrs = JSPROP_READONLY or JSPROP_PERMANENT;
var
  obj: PJSRootedObject;
  cx: PJSContext;
begin
  cx := Engine.cx;
  obj := cx.NewRootedObject(cx.NewObject(nil));
  try
    obj.ptr.DefineFunction(cx, 'loadFile', fs_loadFile, 1, attrs);
    obj.ptr.DefineFunction(cx, 'relToAbs', fs_relToAbs, 2, attrs);
    obj.ptr.DefineFunction(cx, 'fileStat', fs_fileStat, 1, attrs);
    obj.ptr.DefineFunction(cx, 'directoryExists', fs_directoryExists, 1, attrs);
    obj.ptr.DefineFunction(cx, 'fileExists', fs_fileExists, 1, attrs);
    obj.ptr.DefineFunction(cx, 'internalModuleReadFile', fs_internalModuleReadFile, 1, attrs);
    obj.ptr.DefineFunction(cx, 'internalModuleStat', fs_internalModuleStat, 1, attrs);
    obj.ptr.DefineFunction(cx, 'readDir', fs_readDir, 2, attrs);
    obj.ptr.DefineFunction(cx, 'realpath', fs_realPath, 1, attrs);
    obj.ptr.DefineFunction(cx, 'rename', fs_rename, 2, attrs);

    Result := obj.ptr.ToJSValue;
  finally
    cx.FreeRootedObject(obj);
  end;
end;

initialization
  TSMEngineManager.RegisterBinding('fs', SyNodeBindingProc_fs);

end.
