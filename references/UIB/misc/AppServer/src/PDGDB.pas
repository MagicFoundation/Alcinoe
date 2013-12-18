unit PDGDB;
{$ifdef FPC}
{$mode ObjFpc}{$H+}
{$endif}
interface
uses superobject, classes, PDGUtils;

type
  IPDGConnectionPool = interface;
  IPDGConnection = interface;
  IPDGContext = interface;
  IPDGCommand = interface;
  IPDGBlob = interface;

  IPDGConnectionPool = interface
    ['{27621D9A-AAE9-4E24-82F5-A18D84E415F3}']
    function GetConnection: IPDGConnection;
    function GetSize: Integer;
  end;

  IPDGConnection = interface
  ['{843E105A-B8E0-42A9-AFA0-CF5AA843DB8B}']
    function newContext(const Options: ISuperObject = nil): IPDGContext; overload;
    function newCommand(const Options: ISuperObject = nil): IPDGCommand; overload;
    function newContext(const Options: SOString): IPDGContext; overload;
    function newCommand(const Options: SOString): IPDGCommand; overload;
    procedure ExecuteImmediate(const Options: SOString); overload;
  end;

  IPDGContext = interface
  ['{51992399-2D1A-47EF-9DB1-C5654325F41B}']
    function newCommand(const Options: ISuperObject = nil): IPDGCommand; overload;
    function newCommand(const Options: SOString): IPDGCommand; overload;
    procedure ExecuteImmediate(const Options: SOString); overload;
    function Execute(const Command: IPDGCommand; const params: ISuperObject = nil): ISuperObject; overload;
    function Execute(const Command: IPDGCommand; const params: array of const): ISuperObject; overload;
    function Execute(const Command: IPDGCommand; const params: SOString): ISuperObject; overload;
    function Execute(const Command: IPDGCommand; const params: Variant): ISuperObject; overload;
  end;

  IPDGCommand = interface
  ['{A39B974A-96EA-4047-A57B-A2B3EBE7BABD}']
    function Execute(const params: ISuperObject = nil; const context: IPDGContext = nil): ISuperObject; overload;
    function Execute(const params: array of const; const context: IPDGContext = nil): ISuperObject; overload;
    function Execute(const params: SOString; const context: IPDGContext = nil): ISuperObject; overload;
    function Execute(const params: Variant; const context: IPDGContext = nil): ISuperObject; overload;
    function GetInputMeta: ISuperObject;
    function GetOutputMeta: ISuperObject;
  end;

  IPDGBlob = interface
  ['{F478FC21-00B3-49C7-8531-85572AD3C98E}']
    function getData: TStream;
  end;

  IPDGDateTime = interface
  ['{11B92F12-7E04-4442-A84E-9252FDAE2C37}']
    function AsDateTime: Double;
  end;

  // Abstact classes

  TPDGConnection = class(TSuperObject, IPDGConnection)
  protected
    procedure ExecuteImmediate(const Options: SOString); virtual;
    function newContext(const Options: ISuperObject = nil): IPDGContext; overload; virtual; abstract;
    function newContext(const Options: SOString): IPDGContext; overload; virtual;
    function newCommand(const Options: ISuperObject = nil): IPDGCommand; overload; virtual;
    function newCommand(const Options: SOString): IPDGCommand; overload; virtual;
  end;

  TPDGContext = class(TSuperObject, IPDGContext)
  protected
    procedure ExecuteImmediate(const Options: SOString); virtual; abstract;
    function newCommand(const Options: ISuperObject = nil): IPDGCommand; overload; virtual; abstract;
    function newCommand(const Options: SOString): IPDGCommand; overload; virtual;
    function Execute(const Command: IPDGCommand; const params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Execute(const Command: IPDGCommand; const params: array of const): ISuperObject; overload; virtual;
    function Execute(const Command: IPDGCommand; const params: SOString): ISuperObject; overload; virtual;
    function Execute(const Command: IPDGCommand; const params: Variant): ISuperObject; overload; virtual;
  end;

  TPDGCommand = class(TSuperObject, IPDGCommand)
  protected
    function Execute(const params: ISuperObject = nil; const context: IPDGContext = nil): ISuperObject; overload; virtual; abstract;
    function Execute(const params: array of const; const context: IPDGContext = nil): ISuperObject; overload; virtual;
    function Execute(const params: SOString; const context: IPDGContext = nil): ISuperObject; overload; virtual;
    function Execute(const params: Variant; const context: IPDGContext = nil): ISuperObject; overload; virtual;
    function GetInputMeta: ISuperObject; virtual; abstract;
    function GetOutputMeta: ISuperObject; virtual; abstract;
  end;

  TPDGBinary = class(TSuperObject, IPDGBlob)
  private
    FStream: TPooledMemoryStream;
  public
    constructor Create(stream: TStream = nil); reintroduce; overload;
    constructor Create(const filename: string); reintroduce; overload;
    constructor Create(buffer: Pointer; len: Integer); reintroduce; overload;
    destructor Destroy; override;
    function Clone: ISuperObject; override;
    function Write(writer: TSuperWriter; format: boolean; escape: boolean; level: integer): Integer; override;
    function getData: TStream;

    function AsBoolean: Boolean; override; // true if length > 0
    function AsInteger: SuperInt; override; // stream length
  end;

  TPDGDateTime = class(TSuperObject, IPDGDateTime)
  protected
    function AsDateTime: Double;
  end;

  function blob(stream: TStream = nil): ISuperObject; overload;
  function blob(const filename: string): ISuperObject; overload;
  function blob(buffer: Pointer; len: Integer): ISuperObject; overload;

implementation

function blob(stream: TStream = nil): ISuperObject; overload;
begin
  Result := TPDGBinary.Create(stream);
end;

function blob(const filename: string): ISuperObject; overload;
begin
  Result := TPDGBinary.Create(filename);
end;

function blob(buffer: Pointer; len: Integer): ISuperObject; overload;
begin
  Result := TPDGBinary.Create(buffer, len);
end;

{ TPDGConnection }

function TPDGConnection.newCommand(const Options: ISuperObject): IPDGCommand;
begin
  Result := newContext.newCommand(Options);
end;

procedure TPDGConnection.ExecuteImmediate(const Options: SOString);
begin
  newContext.ExecuteImmediate(Options);
end;

function TPDGConnection.newCommand(const Options: SOString): IPDGCommand;
begin
  Result := newContext.newCommand(Options);
end;

function TPDGConnection.newContext(const Options: SOString): IPDGContext;
begin
  Result := newContext(TSuperObject.ParseString(PSOChar(Options), false));
end;

{ TPDGContext }

function TPDGContext.Execute(const Command: IPDGCommand;
  const params: Variant): ISuperObject;
begin
  Result := Command.Execute(so(params), Self);
end;

function TPDGContext.newCommand(const Options: SOString): IPDGCommand;
begin
  Result := newCommand(SO(Options));
end;

function TPDGContext.Execute(const Command: IPDGCommand;
  const params: SOString): ISuperObject;
begin
  Result := Command.Execute(TSuperObject.ParseString(PSOChar(params), false), Self);
end;

function TPDGContext.Execute(const Command: IPDGCommand;
  const params: array of const): ISuperObject;
begin
  Result := Command.Execute(SA(params), Self);
end;

function TPDGContext.Execute(const Command: IPDGCommand;
  const params: ISuperObject = nil): ISuperObject;
begin
  Result := Command.Execute(params, Self);
end;

{ TPDGCommand }

function TPDGCommand.Execute(const params: Variant;
  const context: IPDGContext): ISuperObject;
begin
  Result := Execute(SO(params), context);
end;

function TPDGCommand.Execute(const params: SOString;
  const context: IPDGContext): ISuperObject;
begin
  Result := Execute(TSuperObject.ParseString(PSOChar(params), false), context);
end;

function TPDGCommand.Execute(const params: array of const;
  const context: IPDGContext): ISuperObject;
begin
  Result := Execute(SA(params), context);
end;

{ TPDGBinary }

function TPDGBinary.AsBoolean: Boolean;
begin
  Result := FStream.Size > 0;
end;

function TPDGBinary.AsInteger: SuperInt;
begin
  Result := FStream.Size;
end;

function TPDGBinary.Clone: ISuperObject;
var
  blob: TPDGBinary;
begin
  blob := TPDGBinary.Create;
  blob.FStream.LoadFromStream(FStream);
  Result := blob;
end;

constructor TPDGBinary.Create(stream: TStream);
begin
  inherited Create('[BINARY]');
  FStream := TPooledMemoryStream.Create;
  if Stream <> nil then
    FStream.LoadFromStream(stream);
end;

constructor TPDGBinary.Create(const filename: string);
begin
  inherited Create('[BINARY]');
  FStream := TPooledMemoryStream.Create;
  if filename <> '' then
    FStream.LoadFromFile(filename);
end;

constructor TPDGBinary.Create(buffer: Pointer; len: Integer);
begin
  inherited Create('[BINARY]');
  FStream := TPooledMemoryStream.Create;
  if (buffer <> nil) and (len > 0) then
    FStream.Write(buffer^, len);
end;

destructor TPDGBinary.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TPDGBinary.getData: TStream;
begin
  Result := FStream;
end;

function TPDGBinary.Write(writer: TSuperWriter; format: boolean; escape: boolean;
  level: integer): Integer;
const
  Base64Code: PSOChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  QUOTE: SOChar = '"';
  EQ2: PSOChar = '==';
  EQ4: PSOChar = 'A===';
var
  V: array[0..2] of byte;
  C: array[0..3] of SOChar;
begin
  FStream.Seek(0, soFromBeginning);
  Result := 0;
  inc(Result, writer.Append(@QUOTE, 1));
  while true do
    case FStream.Read(V, 3) of
    3: begin
         C[0] := Base64Code[(V[0] shr 2) and $3F];
         C[1] := Base64Code[((V[0] shl 4) and $3F) or V[1] shr 4];
         C[2] := Base64Code[((V[1] shl 2) and $3F) or V[2] shr 6];
         C[3] := Base64Code[V[2] and $3F];
         inc(Result, writer.Append(@C, 4));
       end;
    2: begin
         C[0] := Base64Code[(V[0] shr 2) and $3F];
         C[1] := Base64Code[((V[0] shl 4) and $3F) or V[1] shr 4];
         C[2] := Base64Code[((V[1] shl 2) and $3F) or 0    shr 6];
         inc(Result, writer.Append(@C, 3));
         inc(Result, writer.Append(EQ2, 1));
         Break;
       end;
    1: begin
         C[0] := Base64Code[(V[0] shr 2) and $3F];
         C[1] := Base64Code[((V[0] shl 4) and $3F) or 0 shr 4];
         inc(Result, writer.Append(@C, 2));
         inc(Result, writer.Append(EQ2, 2));
         Break;
       end;
    0: begin
         if FStream.Position = 0 then
           inc(Result, writer.Append(EQ4, 4));
         Break;
       end;
    end;
  inc(Result, writer.Append(@QUOTE, 1));
end;

{ TPDGDateTime }

function TPDGDateTime.AsDateTime: Double;
begin
  Result := JavaToDelphiDateTime(AsInteger);
end;

end.
