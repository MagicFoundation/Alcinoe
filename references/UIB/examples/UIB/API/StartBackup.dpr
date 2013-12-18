program StartBackup;

{$APPTYPE CONSOLE}

uses
  SysUtils, uibase, uibLib;

const
  EMP_DB_PATH = 'C:\Program Files\Firebird\examples\employee.fdb';

const
  user = 'SYSDBA';
  pass = 'masterkey';

var
  dbfile, bkfile: string;
  svc_handle: IscSvcHandle = nil;
  respbuf: string;
  spb, thd: string;
  Len: Word;

  UL : TUIBLIbrary;

begin
  if (ParamCount = 0) then
  begin
    dbfile := EMP_DB_PATH;
    bkfile := EMP_DB_PATH + '.fbk';
  end
  else
  begin
    dbfile := ParamStr(1);
    if ParamCount > 1 then
      bkfile := ParamStr(2)
    else
      bkfile := dbfile + '.fbk';
  end;

  spb := spb + isc_spb_version;
  spb := spb + isc_spb_current_version;

  spb := spb + isc_spb_user_name;
  spb := spb + char(Length(user));
  spb := spb + user;

  spb := spb + isc_spb_password;
  spb := spb + char(strlen (pass));
  spb := spb + pass;

  UL := TUIBLibrary.Create;
  try
    Ul.Load;
    UL.ServiceAttach('service_mgr', svc_handle, spb);
    try
      thd := thd + isc_action_svc_backup;

      thd := thd + isc_spb_dbname;
      thd := thd + Char(Length(dbfile));
      thd := thd + Char(Length(dbfile) shr 8); // second part of smallint
      thd := thd + dbfile;

      thd := thd + isc_spb_bkp_file;
      thd := thd + Char(Length(bkfile));
      thd := thd + Char(Length(bkfile)shr 8);
      thd := thd + bkfile;

      thd := thd + isc_spb_verbose;

      Writeln('Attach succeed');
      UL.ServiceStart(svc_handle, thd);
      SetLength(respbuf, 1024);

      while true do
      begin
        UL.ServiceQuery(svc_handle, '', isc_info_svc_line, respbuf);
        if (respbuf[1] <> isc_info_svc_line) then
        begin
          WriteLn('Invalid line.');
          Exit;
        end;
        Len := PWord(@respbuf[2])^;
        if len > 0 then
          Writeln(copy(respbuf, 4, len))
        else
          Break;
      end;
    finally
      UL.ServiceDetach(svc_handle);
      WriteLn('Press any key to continue.');
    end;
  finally
    FreeAndNil(UL);
  end;
  ReadLn;
end.
