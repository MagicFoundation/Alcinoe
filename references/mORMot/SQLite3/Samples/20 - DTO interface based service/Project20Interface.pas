/// some common definitions shared by both client and server side
unit Project20Interface;

interface

uses
  Classes,
  SynCommons,
  mORMot;

type
  TDTOAirport = class(TCollectionItem)
  private
    FDCS: RawUTF8;
    FLocation: RawUTF8;
    FBHS: RawUTF8;
    FGate: TRawUTF8DynArray;
    FTerminal: TRawUTF8DynArray;
  published
    property Location: RawUTF8 read FLocation write FLocation;
    property Terminal: TRawUTF8DynArray read FTerminal write FTerminal;
    property Gate: TRawUTF8DynArray read FGate write FGate;
    property BHS: RawUTF8 read FBHS write FBHS;
    property DCS: RawUTF8 read FDCS write FDCS;
  end;

  TDTOAirports = class(TInterfacedCollection)
  private
    function GetCollItem(aIndex: Integer): TDTOAirport;
  protected
    class function GetClass: TCollectionItemClass; override;
  public
    function Add: TDTOAirport;
    property Item[aIndex: Integer]: TDTOAirport read GetCollItem; default;
  end;
  
  TDTOAirline = class(TCollectionItem)
  private
    FSQ: RawUTF8;
    FET: RawUTF8;
    FQR: TRawUTF8DynArray;
    FCX: TRawUTF8DynArray;
  published
    property CX: TRawUTF8DynArray read FCX write FCX;
    property QR: TRawUTF8DynArray read FQR write FQR;
    property ET: RawUTF8 read FET write FET;
    property SQ: RawUTF8 read FSQ write FSQ;
  end;

  TDTOAirlines = class(TInterfacedCollection)
  private
    function GetCollItem(aIndex: Integer): TDTOAirline;
  protected
    class function GetClass: TCollectionItemClass; override;
  public
    function Add: TDTOAirline;
    property Item[aIndex: Integer]: TDTOAirline read GetCollItem; default;
  end;
  
  TDTOAirportDefinition = class(TPersistentWithCustomCreate)
  private
    fAirline: TDTOAirlines;
    fAirport: TDTOAirports;
    fGroundHandler: TRawUTF8DynArray;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Airport: TDTOAirports read fAirport;
    property Airline: TDTOAirlines read fAirline;
    property GroundHandler: TRawUTF8DynArray read fGroundHandler write fGroundHandler;
  end;

  IAirportService = interface(IInvokable)
    ['{4A613FCE-3B0D-4582-97C5-4244B06C2006}']
    procedure GetAirportDefinition(const AirPortID: integer; out Definition: TDTOAirportDefinition);
  end;

const
  ROOT_NAME = 'project20';
  PORT_NAME = '888';

implementation


{ TDTOAirports }

function TDTOAirports.Add: TDTOAirport;
begin
  result := TDTOAirport(inherited Add);
end;

class function TDTOAirports.GetClass: TCollectionItemClass;
begin
  result := TDTOAirport;
end;

function TDTOAirports.GetCollItem(aIndex: Integer): TDTOAirport;
begin
  result := TDTOAirport(GetItem(aIndex));
end;


{ TDTOAirlines }

function TDTOAirlines.Add: TDTOAirline;
begin
  result := TDTOAirline(inherited Add);
end;

class function TDTOAirlines.GetClass: TCollectionItemClass;
begin
  result := TDTOAirline;
end;

function TDTOAirlines.GetCollItem(aIndex: Integer): TDTOAirline;
begin
  result := TDTOAirline(GetItem(aIndex));
end;


{ TDTOAirportDefinition }

constructor TDTOAirportDefinition.Create;
begin
  fAirport := TDTOAirports.Create;
  fAirline := TDTOAirlines.Create;
end;

destructor TDTOAirportDefinition.Destroy;
begin
  fAirline.Free;
  fAirport.Free;
  inherited;
end;

end.
