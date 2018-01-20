unit uQueryHistory;

interface
uses
  SynCommons,
  mORMot;

type
  TSQLQueryHistory = class(TSQLRecord)
  private
    fSQL: RawUTF8;
    fLastUsed: TDateTime;
  published
    property SQL: RawUTF8 read fSQL write fSQL;
    property LastUsed: TDateTime read fLastUsed write fLastUsed;
  public
    procedure FillHistory();
  end;

  
implementation

uses SysUtils, uCustomer;

procedure TSQLQueryHistory.FillHistory();
var
  fData: TSQLTable;
begin
  if globalClient = nil then
    exit;
    
  fData:= globalClient.MultiFieldValues(RecordClass, '');
  fData.SortFields(fData.FieldIndex('LastUsed'), false, nil, sftDateTime);
  fData.OwnerMustFree:= true;
  FillPrepare(fData);
end;



end.
