program Install;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, Registry, Inifiles;

type
  TDriverInfo = record
    Name: string;
    DLL: string;
    LIB: string;
    EXT: string;
  end;

const
  DBExpressKey = '\SOFTWARE\Borland\DBExpress';
  DriversKey = 'Driver Registry File';
  ConnectionsKey = 'Connection Registry File';
  InstalledDrivers = 'Installed Drivers';

  DriverInfos: array[0..7] of TDriverInfo = (
   (Name: 'UIB Interbase6';  DLL: 'dbexpUIBint6.dll';    LIB: 'GDS32.DLL';    EXT: '.ib'),
   (Name: 'UIB Interbase65'; DLL: 'dbexpUIBint65.dll';   LIB: 'GDS32.DLL';    EXT: '.ib'),
   (Name: 'UIB Interbase7';  DLL: 'dbexpUIBint7.dll';    LIB: 'GDS32.DLL';    EXT: '.ib'),
   (Name: 'UIB Interbase71'; DLL: 'dbexpUIBint71.dll';   LIB: 'GDS32.DLL';    EXT: '.ib'),
   (Name: 'UIB FireBird102'; DLL: 'dbexpUIBfire102.dll'; LIB: 'GDS32.DLL';    EXT: '.fdb'),
   (Name: 'UIB FireBird103'; DLL: 'dbexpUIBfire103.dll'; LIB: 'GDS32.DLL';    EXT: '.fdb'),
   (Name: 'UIB FireBird15';  DLL: 'dbexpUIBfire15.dll';  LIB: 'fbclient.dll'; EXT: '.fdb'),
   (Name: 'UIB Yaffil';      DLL: 'dbexpUIByaffil.dll';  LIB: 'GDS32.DLL';    EXT: '.gdb')
  );

var
  Reg: TRegistry;
  dbxconnections: TIniFile;
  dbxdrivers: TIniFile;
  i: Integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if not Reg.OpenKey(DBExpressKey, False) then
    begin
      Reg.RootKey := HKEY_CURRENT_USER;
      if not Reg.OpenKey(DBExpressKey, False) then
        Exit;
    end;

    begin
      dbxdrivers := TIniFile.Create(Reg.ReadString(DriversKey));
      dbxconnections := TIniFile.Create(Reg.ReadString(ConnectionsKey));
      try
        for i := low(DriverInfos) to high(DriverInfos) do
          if (dbxdrivers.ReadInteger(InstalledDrivers, DriverInfos[i].Name, 0) <> 1) then
          begin
            with dbxdrivers do
            begin
              WriteInteger(InstalledDrivers, DriverInfos[i].Name, 1);
              WriteString(DriverInfos[i].Name, 'GetDriverFunc', 'getSQLDriverINTERBASE');
              WriteString(DriverInfos[i].Name, 'LibraryName', DriverInfos[i].DLL);
              WriteString(DriverInfos[i].Name, 'VendorLib', DriverInfos[i].LIB);
              WriteInteger(DriverInfos[i].Name, 'BlobSize', -1);
              WriteString(DriverInfos[i].Name, 'CommitRetain', 'False');
              WriteString(DriverInfos[i].Name, 'Database', 'database'+ DriverInfos[i].EXT);
              WriteString(DriverInfos[i].Name, 'ErrorResourceFile', '');
              WriteString(DriverInfos[i].Name, 'LocaleCode', '0000');
              WriteString(DriverInfos[i].Name, 'Password', 'masterkey');
              WriteString(DriverInfos[i].Name, 'RoleName', 'RoleName');
              WriteString(DriverInfos[i].Name, 'ServerCharSet', '');
              WriteInteger(DriverInfos[i].Name, 'SQLDialect', 3);
              WriteString(DriverInfos[i].Name, 'Interbase TransIsolation', 'ReadCommited');
              WriteString(DriverInfos[i].Name, 'User_Name', 'SYSDBA');
              WriteString(DriverInfos[i].Name, 'WaitOnLocks', 'True');
            end;

            with dbxconnections do
            begin
              WriteInteger(DriverInfos[i].Name + ' Connection', 'BlobSize', -1);
              WriteString(DriverInfos[i].Name + ' Connection', 'CommitRetain', 'False');
              WriteString(DriverInfos[i].Name + ' Connection', 'Database', 'database'+ DriverInfos[i].EXT);
              WriteString(DriverInfos[i].Name + ' Connection', 'DriverName', DriverInfos[i].Name);
              WriteString(DriverInfos[i].Name + ' Connection', 'ErrorResourceFile', '');
              WriteString(DriverInfos[i].Name + ' Connection', 'LocaleCode', '0000');
              WriteString(DriverInfos[i].Name + ' Connection', 'Password', 'masterkey');
              WriteString(DriverInfos[i].Name + ' Connection', 'RoleName', 'RoleName');
              WriteString(DriverInfos[i].Name + ' Connection', 'ServerCharSet', '');
              WriteInteger(DriverInfos[i].Name + ' Connection', 'SQLDialect', 3);
              WriteString(DriverInfos[i].Name + ' Connection', 'Interbase TransIsolation', 'ReadCommited');
              WriteString(DriverInfos[i].Name + ' Connection', 'User_Name', 'SYSDBA');
              WriteString(DriverInfos[i].Name + ' Connection', 'WaitOnLocks', 'True');
            end;

            Writeln(DriverInfos[i].Name + ' installed');
          end;
      finally
        dbxdrivers.Free;
        dbxconnections.Free;
      end;
    end;
  finally
    Reg.Free;
  end;
end.
