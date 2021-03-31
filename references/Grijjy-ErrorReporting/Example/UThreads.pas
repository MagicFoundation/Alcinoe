unit UThreads;

interface

uses
  System.Classes,
  System.SysUtils;

type
  { Thread that raises an Access Violation }
  TBadThread = class(TThread)
  protected
    procedure Execute; override;
  end;

type
  { Thread that raises a File not Found exception, but eats it }
  TEatExceptionThread = class(TThread)
  protected
    procedure Execute; override;
  end;

implementation

{ TBadThread }

procedure TBadThread.Execute;
var
  P: PInteger;
  I: Integer;
begin
  { Create an Access Violation by accessing an invalid address. }
  P := Pointer(-1);
  I := P^;
  P^ := I + 1;
end;

{ TEatExceptionThread }

procedure TEatExceptionThread.Execute;
begin
  { Create File not Found exception, but eat it. }
  try
    TFileStream.Create('NonExistingFile', fmOpenRead);
  except
    { Eat it }
  end;
end;

end.
