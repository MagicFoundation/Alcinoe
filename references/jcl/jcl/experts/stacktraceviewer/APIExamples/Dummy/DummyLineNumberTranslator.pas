unit DummyLineNumberTranslator;

interface

uses
  Classes, ActiveX, JclStackTraceViewerAPI;

type
  TDummyLineNumberTranslator = class(TInterfacedObject, IJclLineNumberTranslator)
    function GetIDString: string;
    function GetName: string;
    function TranslateLineNumbers(ARevisionContent, ACurrentContent: IStream;
      ARevisionLineNumbers: TList; ACurrentLineNumbers: TList): Integer;
  end;

implementation

{ TDummyLineNumberTranslator }

function TDummyLineNumberTranslator.GetIDString: string;
begin
  Result := 'Project JEDI.DummyLineNumberTranslator';
end;

function TDummyLineNumberTranslator.GetName: string;
begin
  Result := 'Dummy LineNumber Translator';
end;

function TDummyLineNumberTranslator.TranslateLineNumbers(ARevisionContent,
  ACurrentContent: IStream; ARevisionLineNumbers, ACurrentLineNumbers: TList): Integer;
var
  I: Integer;
begin
  Result := 0;
  if Assigned(ARevisionContent) and Assigned(ACurrentContent) and (ARevisionLineNumbers.Count > 0) then
  begin
    ACurrentLineNumbers.Clear;
    for I := 0 to ARevisionLineNumbers.Count - 1 do
    begin
      ACurrentLineNumbers.Add(Pointer(Integer(ARevisionLineNumbers[I]) + 1));
      Inc(Result);
    end;
  end;
end;

end.
