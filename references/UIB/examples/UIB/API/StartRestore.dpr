program StartRestore;

{$APPTYPE CONSOLE}

uses
  SysUtils, uibLib, uibase;

const
  EMP_DB_PATH = 'C:\Program Files\Firebird\examples\employee.fdb';

const
  user = 'SYSDBA';
  pass = 'masterkey';

var
  dbfile, bkfile: string;
  spb, thd, respbuf: string;
  svc_handle: IscSvcHandle;
  Len: Word;

  UL: TUIBLibrary;

begin

  if (ParamCount = 0) then
  begin
    dbfile := EMP_DB_PATH;
    bkfile := EMP_DB_PATH + '.fbk';
  end
  else
  begin
    bkfile := ParamStr(1);
    if ParamCount > 1 then
      dbfile := ParamStr(2)
    else
      dbfile := bkfile + '.fdb';
  end;

  spb := isc_spb_version + isc_spb_current_version;

  spb := spb + isc_spb_user_name;
  spb := spb + Char(Length(user));
  spb := spb + user;

  spb := spb + isc_spb_password;
  spb := spb + Char(Length(pass));
  spb := spb + pass;

  UL := TUIBLibrary.Create;
  try
    UL.Load;
    UL.ServiceAttach('service_mgr', svc_handle, spb);

    try
      thd := isc_action_svc_restore;

      thd := thd + isc_spb_bkp_file;
      thd := thd + Char(Length(bkfile));
      thd := thd + Char(Length(bkfile) shr 8);
      thd := thd + bkfile;

      thd := thd + isc_spb_dbname;
      thd := thd + Char(Length(dbfile));
      thd := thd + Char(Length(dbfile) shr 8);
      thd := thd + dbfile;

      thd := thd + isc_spb_verbose;
      thd := thd + isc_spb_options;
      thd := thd +
        Char(isc_spb_res_replace) +
        Char(isc_spb_res_replace shr 8) +
        Char(isc_spb_res_replace shr 16) +
        Char(isc_spb_res_replace shr 32);

      try
        UL.ServiceStart(svc_handle, thd);
      except
        on E: Exception do
        begin
          WriteLn('Error: ', E.Message);
          ReadLn;
          raise;
        end;
      end;

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
          WriteLn(copy(respbuf, 4, len))
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
