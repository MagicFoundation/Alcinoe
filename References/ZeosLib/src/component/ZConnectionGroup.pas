{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{              Database Connection Component              }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{              modification by una.bicicleta              }
{*********************************************************}

unit ZConnectionGroup;

interface
{$I ZComponent.inc}

uses
  Types, SysUtils, Classes, ZDbcIntfs, DB, Forms,
  ZCompatibility, ZConnection, ZSequence, Dialogs;

const
  mask = 'æææ#2ææ0#ææ39æ-V„–FFVæææ';  { define your own mask  }

type
  TZConnectionGroup = class;
  TZConnectionGroupLink = class;

  TZConnectionGroupLink = class(TObject)
  private
    FSender: TObject;
    FOnChange: TNotifyEvent;
  public
    destructor Destroy; override;
    procedure Change;
    dynamic;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Sender: TObject read FSender write FSender;
  end;

  TZConnectionGroup = class(TComponent)
  private
    FOnChange: TNotifyEvent;
    procedure DoChange(Sender: TObject);
    procedure Change;
  protected
    FClients: tList;
    FProtocol: string;
    FHostName: string;
    FPort: Integer;
    FDatabase: string;
    FUser: string;
    FPassword: string;
    //FCatalog:string;

    procedure UnregisterAllDataSets;
    procedure SetUser(const Value: string);
    procedure SetPassword(const Value: string);
    procedure DefineProperties(filer: tfiler);override;

    procedure SetProtocol(const Value: string);
    procedure SetHostName(const Value: string);
    procedure SetPort(const Value: integer);
    procedure SetDatabase(const Value: string);
    //procedure SetCatalog(const Value: string);

    function Encrypt(const str: string): string; //virtual;
    function Decrypt(const str: string): string; //virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadPass(reader:treader);
    procedure WritePass(writer:twriter);
    procedure ReadUser(reader:treader);
    procedure WriteUser(writer:twriter);
    procedure RegisterChanges(Value: TZConnectionGroupLink);
    procedure UnregisterChanges(Value: TZConnectionGroupLink);
  published
    property Protocol: string read FProtocol write SetProtocol;
    property HostName: string read FHostName write SetHostName;
    property Port: Integer read FPort write SetPort default 0;
    property Database: string read FDatabase write SetDatabase;
    property User: string read FUser write SetUser  stored false;
    property Password: string read FPassword write SetPassword  stored false;
    //property Catalog: string read FCatalog write SetCatalog;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    // -- todo ----
    // add another property or event ?
  end;

implementation

// === { TZConnectionGroupLink } ====================================================
destructor TZConnectionGroupLink.Destroy;
begin
  if Sender is TZConnectionGroup then
    TZConnectionGroup(Sender).UnregisterChanges(Self);
  inherited Destroy;
end;

procedure TZConnectionGroupLink.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Sender);
end;


// === { TZConnectionGroup } =============================================
constructor TZConnectionGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClients := tList.Create;
end;

destructor TZConnectionGroup.Destroy;
begin
  UnregisterAllDataSets;
  FClients := nil;
  FClients.Free;
  inherited Destroy;
end;

procedure TZConnectionGroup.DoChange(Sender: TObject);
begin
  Change;
end;

procedure TZConnectionGroup.Change;
  var i:Integer;
      Current: TZConnectionGroupLink;
begin
  if Assigned(FOnChange) then
  begin
    FOnChange(Self);
  end;
  if FClients <> nil then
    for I := 0 to FClients.Count - 1 do
      TZConnectionGroupLink(FClients[I]).Change;
end;

// === { TZConnectionGroup } =============================================
function TZConnectionGroup.Decrypt(const str: string): string;
var n: integer;
    t: word;
    l: word;
begin
   result:='';
   for n:=1 to length(str) do
      result:=result+
        chr(ord(str[n]) xor ord(mask[((n-1) mod length(mask)) +1]));
end;

function TZConnectionGroup.Encrypt(const str: string): string;
begin
   result:=Decrypt(str); { symmetrical encryption }
end;

procedure TZConnectionGroup.ReadPass(reader:treader);
begin
   reader.readlistbegin;
   FPassword := Decrypt(reader.readstring);
   reader.readlistend;
end;

procedure TZConnectionGroup.WritePass(writer:twriter);
var n: integer;
begin
   writer.writelistbegin;
   writer.writestring(Encrypt(FPassword));
   writer.writelistend;
end;

procedure TZConnectionGroup.ReadUser(reader:treader);
begin
   reader.readlistbegin;
   FUser := Decrypt(reader.readstring);
   reader.readlistend;
end;

procedure TZConnectionGroup.WriteUser(writer:twriter);
var n: integer;
begin
   writer.writelistbegin;
   writer.writestring(Encrypt(FUser));
   writer.writelistend;
end;

procedure TZConnectionGroup.DefineProperties(filer: tfiler);
begin
   inherited defineproperties(filer);
   filer.DefineProperty('str1',ReadUser,WriteUser,true);
   filer.DefineProperty('str2',ReadPass,WritePass,true);
end;


procedure TZConnectionGroup.SetUser(const Value: string);
begin
  if FUser <> Value then
  begin
    FUser := Value;
    Change;
  end;
end;

procedure TZConnectionGroup.SetPassword(const Value: string);
begin
  if FPassword <> Value then
  begin
    FPassword := Value;
    Change;
  end;
end;

procedure TZConnectionGroup.SetProtocol(const Value: string);
begin
  if FProtocol <> Value then
  begin
    FProtocol := Value;
    Change;
  end;
end;

procedure TZConnectionGroup.SetHostName(const Value: string);
begin
  if FHostName <> Value then
  begin
    FHostName := Value;
    Change;
  end;
end;

procedure TZConnectionGroup.SetDatabase(const Value: string);
begin
  if FDatabase <> Value then
  begin
    FDatabase := Value;
    Change;
  end;
end;

{
procedure TZConnectionGroup.SetCatalog(const Value: string);
begin
  if FCatalog <> Value then
  begin
    FCatalog := Value;
    Change;
  end;
end;
}

procedure TZConnectionGroup.SetPort(const Value: integer);
begin
  if FPort <> Value then
  begin
    FPort := Value;
    Change;
  end;
end;

procedure TZConnectionGroup.UnregisterAllDataSets;
var
  I: Integer;
  Current: TZConnectionGroupLink;
begin
  for I := FClients.Count - 1 downto 0 do
  begin
    Current := TZConnectionGroupLink(FClients[I]);
    FClients.Remove(Current);
//    try
//       Current := nil;
//    except
//       //Ignore
//    end;
  end;
end;

procedure TZConnectionGroup.RegisterChanges(Value: TZConnectionGroupLink);
begin
  Value.Sender := Self;
  if FClients <> nil then
    FClients.Add(Value);
end;

procedure TZConnectionGroup.UnregisterChanges(Value: TZConnectionGroupLink);
var
  I: Integer;
begin
  if FClients <> nil then
    for I := 0 to FClients.Count - 1 do
      if FClients[I] = Value then
      begin
        Value.Sender := nil;
        FClients.Delete(I);
        Break;
      end;
end;

end.
