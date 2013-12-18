{******************************************************************************}
{                       Firebird BLOB Filers Samples                           }
{                       zLib compression Blob Filter                           }
{                                                                              }
{ Original port from C code written at Inprise Corporation                     }
{                                                                              }
{ Unit owner:    Pierre Y.          pierre [at] levosgien [dot] net            }
{ Last modified: May 27, 2004                                                  }
{******************************************************************************}

library zipf;

uses
  Windows, SysUtils,
  uibase, uibError,
  ZLibEx;

{$R *.res}

const
  FB_SUCCESS = 0;
  FB_FAILURE = 1;

const
  BLB_TEXT = 1;
  BLB_ZIPPED = -3;

const
  BUFFER_LENGTH =	1024;

{ Forward declarations }

function caller(action : Word; control: PISCBlobCtl;
                buffer_length: Word; buffer: PChar;
                var return_length: Word): Integer; forward;

function make_file (control: PISCBlobCtl; UniqueID: Cardinal): integer; forward;
procedure set_statistics (control: PISCBlobCtl); forward;

function zip_file (control: PISCBlobCtl): integer; forward;
function file_to_blob (control: PISCBlobCtl): integer; forward;

function unzip_blob (control: PISCBlobCtl): integer; forward;
function blob_to_file (control: PISCBlobCtl): integer; forward;

function read_file (control: PISCBlobCtl): integer; forward;

function write_file (control: PISCBlobCtl): integer; forward;

function zip (control: PISCBlobCtl): integer; forward;
function unzip (control: PISCBlobCtl): integer; forward;

{ ZIP a blob and store it }
function zip_filter(action: Word; control: PISCBlobCtl): Integer; cdecl;
var
  status: Integer;
begin
  status := FB_SUCCESS;

  case action of
  isc_blob_filter_open :
    begin
      status := make_file(control,0);
      if status = FB_SUCCESS then
        status := unzip_blob(control);
    end;
  isc_blob_filter_create :
    begin
      status := make_file(control,1);
    end;
  isc_blob_filter_get_segment :
    begin
      status := read_file(control);
    end;
  isc_blob_filter_put_segment :
    begin
      status := write_file(control);
    end;
  isc_blob_filter_close :
    begin
      if (control.ctl_to_sub_type = BLB_ZIPPED) then
        status := zip_file(control);

      if (status = FB_SUCCESS) and (control.ctl_data[0] > 0) then
      begin
        CloseHandle(control.ctl_data[0]);
        control.ctl_data[0] := 0;
      end;
    end;
  end;

  Result := status;
end;

{ Read a  zipped blob and translate it to an unzipped blob. }
function unzip_filter(action: Word; control: PISCBlobCtl): Integer; cdecl;
begin
  Result := zip_filter(action, control);
end;

{ Call next source filter.  This is a useful service routine for
  all blob filters. }
function caller(action : Word; control: PISCBlobCtl; buffer_length: Word;
                buffer: PChar; var return_length: Word): Integer;
var
  source: PISCBlobCtl;
begin
  source := control^.ctl_source_handle;
  source^.ctl_status := control^.ctl_status;
  source^.ctl_buffer := buffer;
  source^.ctl_buffer_length := buffer_length;

  Result := source^.ctl_source(action,source);

  return_length := source^.ctl_segment_length;
end;

{ Sets up the statistical fields in the passed in ctl structure.
  These fields are:
   - ctl_max_segment      ->  length of longest seg in blob (in bytes)
   - ctl_number_segments  ->  # of segments in blob
   - ctl_total_length     ->  total length of blob in bytes.
  we should reset the ctl structure, so that blob_info calls get
  the right values. }
procedure set_statistics(control: PISCBlobCtl);
var
  F: Integer;
  c: char;
  bytes_read: Cardinal;
  len, cur_len: Integer;
  max_seg_len, num_segs: integer;
begin
  num_segs := 0;
  len := 0;
  max_seg_len := 0;
  cur_len := 0;

  F := control.ctl_data[0];
  SetFilePointer(F,0,nil,FILE_BEGIN);

  while true do
  begin
    ReadFile(F,c,SizeOf(c),bytes_read,nil);

    if bytes_read = 0 then
      Break;

    Inc(len);
    Inc(cur_len);

    if c= #10 then
    begin
      if cur_len > max_seg_len then
        max_seg_len := cur_len;
      Inc(num_segs);
      cur_len := 0;
    end;
  end;

  control.ctl_max_segment := max_seg_len;
  control.ctl_number_segments := num_segs;
  control.ctl_total_length := len;
end;

{ create a temp file and store the handle and a pointer to the file name
  in ctl_data }
function make_file(control: PISCBlobCtl; UniqueID: Cardinal): integer;
var
  temp_dir: array[0..MAX_PATH - 1] of char;
  file_name: String;
  temp_file: Integer;
begin
  Result := FB_SUCCESS;

  if GetTempPath(MAX_PATH, temp_dir) = 0 then
    Result := FB_FAILURE
  else
  begin
    file_name := temp_dir + 'zbf_' + IntToHex(UniqueID,8) + '.blb';

    temp_file := CreateFile(PChar(file_name),
                            GENERIC_READ or GENERIC_WRITE,
                            FILE_SHARE_READ,
                            nil,
                            OPEN_ALWAYS,
                            FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE,
                            0);

    if temp_file = -1 then
      Result := FB_FAILURE
    else
      control.ctl_data[0] := temp_file;
  end;
end;

{ Reads a file one line at a time and puts the data out as if it
  were coming from a blob. }
function read_file(control: PISCBlobCtl): integer;
var
  P: PChar;
  Len: Word;
  F: Integer;
  c: Char;
  bytes_read: Cardinal;
begin
  if control.ctl_to_sub_type <> BLB_TEXT then
    Result := isc_uns_ext
  else
  begin
    P := control.ctl_buffer;
    Len := control.ctl_buffer_length;

    F := control.ctl_data[0];

    while true do
    begin
      ReadFile(F,c,1,bytes_read,nil);

      if bytes_read = 0 then
        Break;

      P^ := c;
      Inc(P);

      if (c=#10) or (P >= control.ctl_buffer + len) then
      begin
        control.ctl_segment_length := P - control.ctl_buffer;
        if (c=#10) then
          Result := FB_SUCCESS
        else
          Result := isc_segment;
        Exit;
      end;
    end;

    Result := isc_segstr_eof;
  end;
end;

{ Takes the blob data and saves it in a temporary file. }
function write_file(control: PISCBlobCtl): integer;
var
  P: PChar;
  F: Integer;
  Len: Word;
  bytes_written: Cardinal;
begin
  if (control.ctl_to_sub_type <> BLB_ZIPPED) then
    Result := isc_uns_ext
  else
  begin
    P := control.ctl_buffer;
    Len := control.ctl_buffer_length;

    F := control.ctl_data[0];

    WriteFile(F,P^,Len,bytes_written,nil);

    Result := FB_SUCCESS;
  end;
end;

{ Read the blob into the temporary file and call unzip }
function unzip_blob(control: PISCBlobCtl): integer;
begin
  blob_to_file(control);
  Result := unzip(control);
end;

{ Dump a blob into the temp file.  Here we need to call back into the
  engine - or whoever it was who called us - and ask for the segments of
  the blob.  We set up our own buffer and call to caller. }
function blob_to_file(control: PISCBlobCtl): integer;
const
  c : char = #10;
var
  F: Integer;
  Len: Word;
  Buffer: array[0..1024] of char;
  Status: Integer;
  bytes_written: Cardinal;
begin
  F := control.ctl_data[0];

  while true do
  begin
    ZeroMemory(@Buffer[0],Length(Buffer));
    status := caller(isc_blob_filter_get_segment,control,Length(Buffer),Buffer,Len);
    if (status = FB_SUCCESS) or (status = isc_segment) then
      WriteFile(F,Buffer[0],Len,bytes_written,nil)
    else
      Break;
  end;
//  WriteFile(F,C,1,bytes_written,nil);

  Result := FB_SUCCESS;
end;

{ ZIPs the temporary file and put it into the blob }
function zip_file(control: PISCBlobCtl): integer;
begin
  if (control.ctl_to_sub_type <> BLB_ZIPPED) then
    Result := isc_uns_ext
  else
  begin
    zip(control);
    file_to_blob(control);

    Result := FB_SUCCESS;
  end;
end;

{ Copy a file to a blob, including the last little bit.  Since we
  just finished writing the file, rewind before start. }
function file_to_blob(control: PISCBlobCtl): integer;
var
  F: Integer;
  Len: Word;
  Buffer: PChar;
  bytes_read: Cardinal;
  P: PChar;
  c: char;
begin
  GetMem(Buffer,control.ctl_buffer_length);
  ZeroMemory(Buffer,control.ctl_buffer_length);

  F := control.ctl_data[0];
  SetFilePointer(F,0,nil,FILE_BEGIN);

  P := Buffer;

  while true do
  begin
    ReadFile(F,c,1,bytes_read,nil);

    if bytes_read = 0 then
      Break;

    P^ := c;
    Inc(P);

    if (P > Buffer + control.ctl_buffer_length) then
    begin
      caller(isc_blob_filter_put_segment, control, control.ctl_buffer_length, Buffer, Len);
      P := Buffer;
    end;
  end;

  caller(isc_blob_filter_put_segment, control, Word(P - Buffer), Buffer, Len);

  FreeMem(Buffer);

  Result := FB_SUCCESS;
end;

{ Zips the temporary file }
function zip(control: PISCBlobCtl): integer;
var
  in_file, out_file: Integer;
  Buffer: PChar;
  ZBuffer: PByte;
  ZBuffLen: Integer;
  bytes_count: Cardinal;
begin
  set_statistics (control);
  in_file := control.ctl_data[0];

  SetFilePointer(in_file,0,nil,FILE_BEGIN);

  if (make_file(control,3) <> FB_SUCCESS) then
    Result := FB_FAILURE
  else
  begin
    out_file := control.ctl_data[0];
    GetMem(Buffer,control.ctl_total_length + 1);
    ZeroMemory(Buffer,control.ctl_total_length + 1);

    ReadFile(in_file,Buffer^,control.ctl_total_length,bytes_count,nil);

    ZCompress(Buffer,control.ctl_total_length,Pointer(ZBuffer),ZBuffLen,zcMax);

    CloseHandle(in_file);
    FreeMem(Buffer);

    WriteFile(out_file,ZBuffer^,ZBuffLen,bytes_count,nil);
    FreeMem(ZBuffer,ZBuffLen);

    SetFilePointer(out_file,0,nil,FILE_BEGIN);

    Result := FB_SUCCESS;
  end;
end;

{ Unzip the temporary file }
function unzip(control: PISCBlobCtl): integer;
var
  in_file, out_file: Integer;
  Buffer: PChar;
  ZBuffer: PByte;
  ZBuffLen: Integer;
  bytes_count: Cardinal;
begin
  in_file := control.ctl_data[0];

  SetFilePointer(in_file,0,nil,FILE_BEGIN);
  bytes_count := GetFileSize(in_file,nil);

  if (make_file(control,3) <> FB_SUCCESS) then
    Result := FB_FAILURE
  else
  begin
    out_file := control.ctl_data[0];
    GetMem(Buffer, bytes_count + 1);
    ZeroMemory(Buffer, bytes_count + 1);

    ReadFile(in_file,Buffer^,bytes_count,bytes_count,nil);

    ZDecompress(Buffer,bytes_count,Pointer(ZBuffer),ZBuffLen);

    CloseHandle(in_file);
    FreeMem(Buffer);

    WriteFile(out_file,ZBuffer^,ZBuffLen,bytes_count,nil);
    FreeMem(ZBuffer,ZBuffLen);

    set_statistics(control);

    SetFilePointer(out_file,0,nil,FILE_BEGIN);

    Result := FB_SUCCESS;
  end;
end;

exports
  zip_filter,
  unzip_filter;

begin
end.
