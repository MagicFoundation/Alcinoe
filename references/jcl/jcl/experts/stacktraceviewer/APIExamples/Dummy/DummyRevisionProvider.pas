unit DummyRevisionProvider;

interface

uses
  SysUtils, Classes, ActiveX, JclStackTraceViewerAPI;

type
  TDummyRevisionProvider = class(TInterfacedObject, IJclRevisionProvider)
    function GetIDString: string;
    function GetName: string;
    function GetRevisionContent(const AFileName, ARevision: string; AContent: IStream): Boolean;
  end;

implementation

{ TDummyRevisionProvider }

function TDummyRevisionProvider.GetIDString: string;
begin
  Result := 'Project JEDI.DummyRevisionProvider';
end;

function TDummyRevisionProvider.GetName: string;
begin
  Result := 'Dummy Revision Provider';
end;

function TDummyRevisionProvider.GetRevisionContent(const AFileName, ARevision: string;
   AContent: IStream): Boolean;
var
  FS: TFileStream;
  SA: TStreamAdapter;
  R, W: Int64;
begin
  Result := True;
  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SA := TStreamAdapter.Create(FS);
    SA.CopyTo(AContent, FS.Size, R, W);
  finally
    FS.Free;
  end;
end;

end.
