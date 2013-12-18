{******************************************************************************}
{                       Firebird BLOB Filers Samples                           }
{                           Invert Blob Filter                                 }
{                                                                              }
{ Original port from C code written at Inprise Corporation                     }
{                                                                              }
{ Unit owner:    Pierre Y.          pierre [at] levosgien [dot] net            }
{ Last modified: May 27, 2004                                                  }
{******************************************************************************}

library invf;

uses
  Windows, SysUtils,
  uibase, uibError;

{$R *.res}

{Oo..............................BLOB FILTERS................................oO}
{
    Blob Filters are called by Firebird engine when you specify an adequate
     Blob Parameter Block while calling isc_blob_open2 or isc_blob_create2
     as soon as they are declared into the database with the subsequent DDL
     statement :

       DECLARE FILTER <filter_name>
       INPUT_TYPE <#sub_type> OUTPUT_TYPE <#sub_type>
       ENTRY_POINT <function_name> MODULE_NAME <dll_name>;

       eg: DECLARE FILTER invert_filter
           INPUT_TYPE 1 OUTPUT_TYPE -2
           ENTRY_POINT 'invert_filter' MODULE_NAME 'invf.dll';

     In order to make this filter working you have to :

      0 - Compile the invf.dll library and drop it to the Firebird\UDF path (see
          firebird.conf to see where you can put UDFs)

      1 - Add a BLOB field SUB_TYPE -2 to a table

      2 - Declare the two BLOB FILTERS with the above SQL DDL Statement :
            invert_filter   : from sub_type  1 to sub_type -2
            uninvert_filter : from sub_type -2 to sub_type  1

      3 - Create a Blob Parameter Block (BPB) using predefined constants :
            inv_bpb : string = 'target_type=-2;source_type=1';
            uninv_bpb : string = 'target_type=1;source_type=-2';

      4 - Call BlobCreate before an INSERT statement to create a blob an get
          its BlobID from the engine. With the BPB you tells the engine you will
          give it a sub_type 1 blob and it has to store it as sub_type -2 blob.
          
      5 - Call BlobOpen after a SELECT statement to open a blob passing it
          through the blob filter as soon as you tells the engine to filter
          sub_type -2 blobs to sub_type 1 blobs.

     Explanations:

      Blob Filter Main Entry Point functions are called many times during blob
      creation or blob retrievement. The Action parameter determines what the
      filter has to do.

      If you creates a new blob to store data in :

       1 - Action = isc_blob_filter_create
        while not (result = FB_SUCCESS) do
         2 - Action = isc_blob_filter_put_segment
       3 - Action = isc_blob_filter_close

      If you want to retrieve the data from an existing one :

       1 - Action = isc_blob_filter_open
        while not (result = FB_SUCESS) do
         2 - Action = isc_blob_filter_get_segment
       3 - Action = isc_blob_filter_close

      Each time the blob filter is called, a Blob Control Structure is passed
      where you get a pointer on the preceding or the following filter function
      in the filters list.

      Since the filter function is called many times before the blob filtering
      is complete. You have to store the blob temporary datas into a temporary
      file. As incredible as it can be, you can store the temporary file handle
      into the Blob Control Structure in order to avoid file opening/closing
      during the filtering process. You can store 8 integer values into the
      control.ctl_data[0..7] array.

      Synoptics:

                    Oo...I N V E R T _ F I L T E R...oO


      Action                    Function calls   Comments
      ..........................................................................
      blob_filter_open          make_file        Creates a temporary file
                                uninvert_blob
                                blob_to_file     Calls previous filter with get_segment action
                                uninvert

      blob_filter_get_segment   read_file        Fills control.ctl_buffer with
                                                 control.ctl_buffer_length bytes
                                                 from temp. file

      blob_filter_close         close_file       Destroys the temp file
      ..........................................................................


                   Oo...U N I N V E R T _ F I L T E R...oO


      Action                    Function calls   Comments
      ..........................................................................
      blob_filter_create        make_file        Creates a temporary file

      blob_filter_put_segment   write_file       Fills temp.file with
                                                 control.ctl_buffer_length bytes
                                                 from control.ctl_buffer

      blob_filter_close         invert_file
                                invert
                                file_to_blob     Calls next filter with put_segment action
                                close_file       Destroys the temp file
      ..........................................................................
oO}

const
  FB_SUCCESS = 0;
  FB_FAILURE = 1;

const
  BLB_TEXT = 1;
  BLB_INVERTED = -2;

  const
  BUFFER_LENGTH =	1024;

(*
 * FORWARD DECLARATIONS
 *)

{ Utilities }
function caller(action : Word; control: PISCBlobCtl;
                buffer_length: Word; buffer: PChar;
                var return_length: Word): Integer; forward;
procedure set_statistics (control: PISCBlobCtl); forward;

{ Creates temporary file }
function make_file (control: PISCBlobCtl; UniqueID: Cardinal): integer; forward;

{ Functions called by invert_filter }
function invert_file (control: PISCBlobCtl): integer; forward;
function file_to_blob (control: PISCBlobCtl): integer; forward;

{ Functions called by uninvert filter }
function uninvert_blob (control: PISCBlobCtl): integer; forward;
function blob_to_file (control: PISCBlobCtl): integer; forward;

{ Function called by isc_blob_filter_get_segment }
function read_file (control: PISCBlobCtl): integer; forward;

{ Function called by isc_blob_put_segment }
function write_file (control: PISCBlobCtl): integer; forward;

{ Filter engine to invert and invert_back the temporary file }
function uninvert (control: PISCBlobCtl): integer; forward;
function invert (control: PISCBlobCtl): integer; forward;

(*
 * ENTRY POINTS
 *)

{ Invert filter entry point :
  - Invert a blob and store it. Most blob filters start out with a
    switch statement like this. }
function invert_filter(action: Word; control: PISCBlobCtl): Integer; cdecl;
var
  status: Integer;
begin
  status := FB_SUCCESS;

  case action of
  isc_blob_filter_open :
    begin
      status := make_file(control,0);
      if status = FB_SUCCESS then
        status := uninvert_blob(control);
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
      if (control.ctl_to_sub_type = BLB_INVERTED) then
        status := invert_file(control);

      if (status = FB_SUCCESS) and (control.ctl_data[0] > 0) then
      begin
        CloseHandle(control.ctl_data[0]);
        control.ctl_data[0] := 0;
      end;
    end;
  end;

  Result := status;
end;

{ Uninvert filter entry point :
  - Read an inverted blob and translate it to a normal blob.  Since this
    particular filter is bijective boths sides are the same. }
function uninvert_filter(action: Word; control: PISCBlobCtl): Integer; cdecl;
begin
  Result := invert_filter(action, control);
end;

(*
 * UTILITIES
 *)

{ Calls next source filter.  This is a useful service routine for
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
   - ctl_max_segment     - length of longest seg in blob (in bytes)
   - ctl_number_segments - # of segments in blob
   - ctl_total_length    - total length of blob in bytes.
  we should reset the ctl structure, so that blob_info calls get the right
  values.}
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

(*
 * FILTER FUNCTIONS
 *)

{ Creates a temp file and store the handle in ctl_data membre of Blob
  Control Structure }
function make_file(control: PISCBlobCtl; UniqueID: Cardinal): integer;
var
  temp_dir: array[0..MAX_PATH-1] of char;
  file_name: String;
  temp_file: Integer;
begin
  Result := FB_SUCCESS;

  if GetTempPath(MAX_PATH, temp_dir) = 0 then
    Result := FB_FAILURE
  else
  begin
    file_name := temp_dir + 'ibf_' + IntToHex(UniqueID,8) + '.blb';

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

{ Inverts the temporary file and puts it into the blob }
function invert_file (control: PISCBlobCtl): integer;
begin
  if (control.ctl_to_sub_type <> BLB_INVERTED) then
    Result := isc_uns_ext
  else
  begin
    invert(control);
    file_to_blob(control);

    Result := FB_SUCCESS;
  end;
end;

{ Copy a file to a blob, including the last little bit.  Since we
  just finished writing the file, rewind before start. }
function file_to_blob (control: PISCBlobCtl): integer;
var
  F: Integer;
  Len: Word;
  Buffer: PChar;
  bytes_read: Cardinal;
  p: PChar;
  c: Char;
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

{ Read a blob into a file and call uninvert to invert it back }
function uninvert_blob (control: PISCBlobCtl): integer;
begin
  blob_to_file(control);
  Result := uninvert(control);
end;

{ Dump a blob into the temp file.  Here we need to call back into the
  engine - or whoever it was who called us - and ask for the segments of
  the blob.  We set up our own buffer and call to caller. }
function blob_to_file (control: PISCBlobCtl): integer;
var
  F: Integer;
  Len: Word;
  Buffer: array[0..BUFFER_LENGTH] of char;
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

  Result := FB_SUCCESS;
end;

{ Invert back the file - This function is bijective so we just have to call
  invert again }
function uninvert(control: PISCBlobCtl): integer;
begin
  Result := invert(control);
end;

{ Reads a file one line at a time and puts the data out as if it
  were coming from a blob. }
function read_file (control: PISCBlobCtl): integer;
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

{ Takes blob data and saves it in the temporary file. }
function write_file (control: PISCBlobCtl): integer;
var
  P: PChar;
  F: Integer;
  Len: Word;
  bytes_written: Cardinal;
begin
  if (control.ctl_to_sub_type <> BLB_INVERTED) then
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

{ Inverts the temporary file }
function invert (control: PISCBlobCtl): integer;
var
  in_file, out_file: Integer;
  Buffer, P: PChar;
  c: char;
  bytes_count: Cardinal;
begin
  set_statistics (control);
  in_file := control.ctl_data[0];

  SetFilePointer(in_file,0,nil,FILE_BEGIN);

  if (make_file(control,2) <> FB_SUCCESS) then
    Result := FB_FAILURE
  else
  begin
     out_file := control.ctl_data[0];
     GetMem(Buffer,control.ctl_total_length + 1);
     ZeroMemory(Buffer,control.ctl_total_length + 1);

     P := PChar(Integer(Buffer) + control.ctl_total_length);
     P^ := #10;

     while ReadFile(in_file,c,1,bytes_count,nil) = true do
     begin
      Dec(P);
      P^ := c;

      if (P <= Buffer) then
        Break;
     end;

     CloseHandle(in_file);

     WriteFile(out_file,P^,control.ctl_total_length,bytes_count,nil);

     c := #10;
     WriteFile(out_file,c,1,bytes_count,nil);

     SetFilePointer(out_file,0,nil,FILE_BEGIN);

     FreeMem(Buffer);

     Result := FB_SUCCESS;
  end;
end;

exports
  invert_filter,
  uninvert_filter;

begin
end.
