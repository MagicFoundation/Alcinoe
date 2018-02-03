unit StackTraceViewerFastMMReg;

interface

procedure Register;

implementation

uses
  SysUtils, Forms, Dialogs, ToolsAPI, JclStackTraceViewerAPI, StackTraceViewerFastMMUnit;

type
  TIOTAProjectTestWizard = class(TNotifierObject, IOTAMenuWizard, IOTAWizard)
  private
    FFastMMReportData: TFastMMReportData;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    function GetIDString: string;
    function GetMenuText: string;
    function GetName: string;
    function GetState: TWizardState;
  end;

procedure Register;
begin
  RegisterPackageWizard(TIOTAProjectTestWizard.Create);
end;

constructor TIOTAProjectTestWizard.Create;
begin
  inherited Create;
  FFastMMReportData := TFastMMReportData.Create;
end;

destructor TIOTAProjectTestWizard.Destroy;
begin
  FFastMMReportData.Free;
  inherited Destroy;
end;

procedure TIOTAProjectTestWizard.Execute;
var
  OpenDialog: TOpenDialog;
  FastMMFile: string;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    if OpenDialog.Execute then
      FastMMFile := OpenDialog.FileName;
  finally
    OpenDialog.Free;
  end;
  if FastMMFile <> '' then
    FFastMMReportData.LoadFastMMFile(FastMMFile);
end;

function TIOTAProjectTestWizard.GetIDString: string;
begin
  Result := 'PROJECT JEDI.JclStackTraceViewerFastMM';
end;

function TIOTAProjectTestWizard.GetMenuText: string;
begin
  Result := '&Load FastMM Logfile';
end;

function TIOTAProjectTestWizard.GetName: string;
begin
  Result := 'JCL Stack Trace Viewer Extension for FastMM';
end;

function TIOTAProjectTestWizard.GetState: TWizardState;
begin
  if Assigned(StackTraceViewerStackProcessorServices) then
    Result := [wsEnabled]
  else
    Result := [];
end;

end.
