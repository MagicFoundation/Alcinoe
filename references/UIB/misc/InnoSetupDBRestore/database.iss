#define database "EMPLOYEE"
#define sourceext "FBK";
#define destext "FDB";
#define username "SYSDBA"
#define password "masterkey";
#define gds32 "gds32.dll"

[Setup]
AppName=The database {#database}
AppVerName=The database {#database}
DefaultDirName={src}
OutputBaseFilename={#database}
Compression=zip/9
SolidCompression=yes
Uninstallable=false
OutputDir=.
;DiskSpanning=yes
;DiskSliceSize=2500000
;password=xxxxxxxx

[Languages]
Name: "french"; MessagesFile: "compiler:Languages\French.isl"

[Files]
Source: "{#database}.{#sourceext}"; DestDir: "{src}"; AfterInstall: Restore('{#username}', '{#password}', '{#database}.{#sourceext}', '{#database}.{#destext}', 0);

[Code]
type
  TStatusVector = array[0..19] of integer;

function isc_service_attach(var status_vector: TStatusVector; service_length: Word; service_name: String; var handle: integer; spb_length: Word; spb: String): Integer; external 'isc_service_attach@{#gds32} stdcall';
function isc_service_detach(var status_vector: TStatusVector; var handle: integer): integer; external 'isc_service_detach@{#gds32} stdcall';
function isc_service_start(var status_vector: TStatusVector; var handle: Integer; reserved: integer; spb_length: Word; spb: string): integer; external 'isc_service_start@{#gds32} stdcall';
function isc_service_query(var status_vector: TStatusVector; var handle: integer; reserved: integer; send_spb_length: Word; send_spb: string; request_spb_length: Word; request_spb: string; buffer_length: Word; buffer: string): integer; external 'isc_service_query@{#gds32} stdcall';
function isc_interprete(buffer: string; var status_vector: integer): integer; external 'isc_interprete@{#gds32} stdcall';

function GetIntAddr(var Target: Integer; var Value: Integer): Integer; external 'InterlockedExchange@kernel32.dll stdcall';

const
  isc_spb_current_version = #2;
  isc_spb_version = isc_spb_current_version;
  isc_spb_user_name = #28;
  isc_spb_password = #29;
  isc_spb_bkp_file = #5;
  isc_spb_dbname = #106;
  isc_spb_res_replace = $1000;
  isc_spb_res_create = $2000;
  isc_spb_verbose = #107;
  isc_spb_options = #108;
  isc_action_svc_restore = #2;
  isc_spb_res_page_size = #10;
  isc_info_svc_line = #62;

var
  StatusVector: TStatusVector;

procedure CheckAPICall(code: integer);
var
  len: Integer;
  error, buffer: string;
  Vector: integer;
begin
  if code <> 0 then
  begin
    error := '';
    GetIntAddr(Vector, StatusVector[0]);
    setlength(buffer, 513);
    repeat
      len := isc_interprete(buffer, Vector);
      if len > 0 then
        error := error + copy(buffer, 0, len) + ' ' else
        Break;
    until False;
    RaiseException(error);
  end;
end;

procedure ServiceAttach(const ServiceName: string; var SvcHandle: Integer; const Spb: string);
begin
  CheckAPICall(isc_service_attach(StatusVector, Length(ServiceName), ServiceName, SvcHandle, Length(Spb), Spb));
end;

procedure ServiceDetach(var handle: Integer);
begin
  CheckAPICall(isc_service_detach(StatusVector, handle));
end;

procedure ServiceStart(var handle: Integer; spb: string);
begin
  CheckAPICall(isc_service_start(StatusVector, handle, 0, Length(Spb), Spb));
end;

procedure ServiceQuery(var handle: integer; const SendSpb, RequestSpb: string; var Buffer: string);
begin
  CheckApiCall(isc_service_query(StatusVector, Handle, 0, Length(SendSpb), SendSpb, Length(RequestSpb), RequestSpb, Length(Buffer), Buffer));
end;

function AddString(id: char; const Value: string): string;
begin
  if (Value <> '') then
    Result := id + chr(length(Value)) + Value else
    Result := '';
end;

function WordToString(w: word): string;
begin
  Result := chr(w and $FF) + chr(w shr 8);
end;

function IntegerToString(i: integer): string;
begin
  Result := chr(i and $FFFFFF) + chr((i shr 8) and $FFFFFF) + chr((i shr 16) and $FFFFFF) + chr(i shr 24);
end;

procedure Restore(const Username, Password, SourceFile, Destfile: string; PageSize: Integer);
var
  SPB, buffer: string;
  handle: Integer;
  Len: Word;
  Form: TSetupForm;
  Memo: TMemo;
begin
  SourceFile := WizardDirValue + '\' + SourceFile;
  DestFile := WizardDirValue + '\' + DestFile;

  SPB := isc_spb_version + isc_spb_current_version;
  SPB := SPB + AddString(isc_spb_user_name, Username);
  SPB := SPB + AddString(isc_spb_password, Password);
  ServiceAttach('service_mgr', handle, SPB);
  try
    // Restore
    SPB := isc_action_svc_restore;
    // Backup file
    SPB := SPB + isc_spb_bkp_file + WordToString(Length(Sourcefile)) + Sourcefile;
    // Restore file
    SPB := SPB + isc_spb_dbname + WordToString(Length(Destfile)) + Destfile;
    // Verbose
    SPB := SPB + isc_spb_verbose;
    // Options
    SPB := SPB + isc_spb_options + IntegerToString(isc_spb_res_create or isc_spb_res_replace);
    // PageSize
    if PageSize > 0 then
      SPB := SPB + isc_spb_res_page_size + IntegerToString(PageSize);

    ServiceStart(handle, SPB);
    
    Form := CreateCustomForm;
    try
      Form.ClientWidth := WizardForm.ClientWidth;
      Form.ClientHeight := WizardForm.ClientHeight;
      Form.CenterInsideControl(WizardForm, False);
      Form.Caption := 'Restauration de la base de données';
      Memo := TMemo.Create(Form);
      Memo.Parent := Form;
      Memo.Align := alClient;
      Form.show;

      SetLength(Buffer, 1024);
      while true do
      begin
        ServiceQuery(Handle, '', isc_info_svc_line, Buffer);
        if (Buffer[1] <> isc_info_svc_line) then
          RaiseException('Restore error.');
        Len := ord(buffer[2]) or (ord(buffer[3]) shl 8);
        if (len > 0)  then
          Memo.Lines.add(copy(Buffer, 4, len)) else
          Break;
      end;
    finally
      Form.Free;
    end;
  finally
    ServiceDetach(handle);
  end;
end;
