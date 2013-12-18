 (*
 *    Program type:  BLOB Filter Sample
 *
 *    Description:
 *        This program uses a filter to display a blob data type.
 *        Job descriptions are read through a filter, which formats
 *        the text into 40-character-wide paragraphs.
 *
 *    IMPORTANT NOTE!
 *        The server side file, api9f.c, must have been compiled
 *        and linked and must be running on the server before
 *        this example can be run.
 *
 *
 * The contents of this file are subject to the Interbase Public
 * License Version 1.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy
 * of the License at:
 * http://www.borland.com/devsupport/interbase/opensource/IPL.html
 *
 * Software distributed under the License is distributed on an
 * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express
 * or implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code was created by Pierre Yager.
 *
 * Contributor(s):
 *   Borland : Aaron Ruddick InterBase QA, Borland Software Corp.
 *             Dan Mikhayltsa  InterBase QA, Borland Software Corp.
 *)
 
 library api9f;

uses
  SysUtils,
  Classes,
  uibase,
  uibError;

{$R *.res}

const
  FB_SUCCESS = 0;
  FB_FAILURE = 1;

const
  BUFFER_LENGTH =	512;

const
  WIDTH = 40;
  TMP_FILE = 'desc.txt';

function dump_text(action: Word; control: PISCBlobCtl): integer; forward;
procedure set_statistics(filename: string; control: PISCBlobCtl); forward;
function read_text(action: Word; control: PISCBlobCtl): integer; forward;
function caller(action : Word; control: PISCBlobCtl; buffer_length: Word;
                buffer: PChar; var return_length: Word): Integer; forward;


function desc_filter(action: Word; control: PISCBlobCtl): Integer; cdecl;
var
  status: Integer;
  text_file: Integer;
begin
 (*****************************************************************************
  *
  * Functional description
  *	Format a blob into 40-character-wide paragraphs.
  *  Read the blob into a file and process it on open, then read it back line
  *  by line in the get_segment loop.
  *
  *****************************************************************************)

  Result := FB_SUCCESS;

  case action of
  isc_blob_filter_open :
    begin
      status := dump_text(action, control);
      if status <> FB_SUCCESS then
        Result := status;
      set_statistics(TMP_FILE, control); { set up stats in ctl struct }
    end;
  isc_blob_filter_get_segment :
    begin
     { open the file first time through and save the file pointer }
     if control.ctl_data[0] = 0 then
     begin
      text_file := FileOpen(TMP_FILE, fmOpenRead);
      control.ctl_data[0] := text_file;
     end;

     status := read_text(action, control);
     if status <> FB_SUCCESS then
      Result := status;
    end;
  isc_blob_filter_close :
    begin
     if control.ctl_data[0] <> 0 then
       FileClose(control.ctl_data[0]);

{
      DeleteFile(TMP_FILE);
}

    end;
  isc_blob_filter_create,
  isc_blob_filter_put_segment :
    Result := isc_uns_ext;
  end;
end;

function caller(action : Word; control: PISCBlobCtl; buffer_length: Word;
                buffer: PChar; var return_length: Word): Integer;
var
  source: PISCBlobCtl;
begin
 (*****************************************************************************
  *
  * Functional description
  *        Call next source filter.  This is a useful service routine for
  *        all blob filters.
  *
  *****************************************************************************)

  source := control^.ctl_source_handle;
  source^.ctl_status := control^.ctl_status;
  source^.ctl_buffer := buffer;
  source^.ctl_buffer_length := buffer_length;

  Result := source^.ctl_source(action,source);

  return_length := source^.ctl_segment_length;
end;

function dump_text(action: Word; control: PISCBlobCtl): integer;
var
  F: Integer;
  status: Integer;
  buffer: array[0..BUFFER_LENGTH] of char;
  len: Word;
  prevbuf, tbuf: string;
  i, j: Integer;
  LF: Char;
begin
 (*****************************************************************************
  *
  * Functional description
  *	Open a blob and write the contents to a file
  *
  *****************************************************************************)

  LF := #10;

  F := FileCreate(TMP_FILE);
  if F = -1 then
  begin
    Result := FB_FAILURE;
    Exit;
  end;

  status := FB_SUCCESS;
  while true do
  begin
    status := caller(isc_blob_filter_get_segment, control, SizeOf(buffer), buffer, len);
    if status <> FB_SUCCESS then
      Break;

    buffer[len] := #0;
    tbuf := prevbuf + buffer;
    len := Length(tbuf);

    { replace any new-lines with space }
    for i := 1 to len do
      if tbuf[i] = #10 then
        tbuf[i] := ' ';

    {* break the line up into width-length pieces *}
    i := 1;
    while i < len do
    begin
      { save the remainder }
      if Length(Copy(tbuf,i,maxint)) <= 40 then
      begin
        prevbuf := Copy(tbuf,i,maxint);
        Break;
      end;

      { find end-of-word to break the line at }
      j := WIDTH;
      while j >= 0 do
      begin
        if tbuf[i + j] in [' ',#13,#10,#255] then
          Break;

        Dec(j);
      end;

      if (j < 0) then
        j := WIDTH;

      FileWrite(F,tbuf[i],j);
      FileWrite(F,LF,SizeOf(LF));

      i := i+j+1;
    end;
  end;

	{ print the remainder }
  FileWrite(F,prevbuf[1],Length(prevbuf));
  FileWrite(F,LF,SizeOf(LF));

  FileClose(F);

	if (status <> isc_segstr_eof) then
		Result :=  status
  else
    Result := FB_SUCCESS;
end;

procedure set_statistics(filename: string; control: PISCBlobCtl);
var
  F: Integer;
  c: char;
  len, cur_len: Integer;
  max_seg_len, num_segs: integer;
begin
  (****************************************************************************
   *
   * Functional description
   *	Sets up the statistical fields in the passed in ctl structure.
   *	These fields are:
   *	  ctl_max_segment - length of longest seg in blob (in bytes)
   *	  ctl_number_segments - # of segments in blob
   *	  ctl_total_length - total length of blob in bytes.
   *	we should reset the ctl structure, so that blob_info calls get the right
   *  values.
   *
   ****************************************************************************)

  num_segs := 0;
  len := 0;
  max_seg_len := 0;
  cur_len := 0;

  F := FileOpen(TMP_FILE,fmOpenRead);

  while true do
  begin
    if FileRead(F,c,SizeOf(c)) = 0 then
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

  FileClose(F);
end;

function read_text(action: Word; control: PISCBlobCtl): integer;
var
  F: Cardinal;
  p: PChar;
  len: Word;
  c: char;
begin
 (*****************************************************************************
  *
  * Functional description Reads a file one line at a time
  *	and puts the data out as if it were coming from a blob.
  *
  *****************************************************************************)

  p := control.ctl_buffer;
  len := control.ctl_buffer_length;
  F := control.ctl_data[0];

  while true do
  begin
    if FileRead(F,c,1) = 0 then
      Break;
    p^ := c;
    Inc(p);

    if (c = #10) or (p >= control.ctl_buffer + len) then
    begin
      control.ctl_segment_length := p - control.ctl_buffer;
      if (c = #10) then
        result := FB_SUCCESS
      else
        result := isc_segment;

      Exit;
    end;
  end;

 result := isc_segstr_eof;
end;

exports
  desc_filter;

begin
end.
