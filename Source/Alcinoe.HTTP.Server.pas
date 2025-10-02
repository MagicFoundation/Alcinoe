unit Alcinoe.HTTP.Server;

interface

{$I Alcinoe.inc}

uses
  System.SysUtils,
  Alcinoe.Net,
  Alcinoe.HTTP;

type

  EALHttpServerBodySizeTooBig = Class(Exception);
  EALHttpServerConnectionDropped = class(Exception);

  {-------------------------------------------}
  TALHttpServerRequest = class(TALHTTPRequest)
  protected
    function GetRemoteAddress: TALNetEndpoint; virtual; abstract;
    procedure SetRemoteAddress(const AValue: TALNetEndpoint); virtual; abstract;
    function GetLocalAddress: TALNetEndpoint; virtual; abstract;
    procedure SetLocalAddress(const AValue: TALNetEndpoint); virtual; abstract;
    function GetIsSecure: Boolean; virtual; abstract;
  public
    property RemoteAddress: TALNetEndpoint read GetRemoteAddress write SetRemoteAddress;
    property LocalAddress: TALNetEndpoint read GetLocalAddress write SetLocalAddress;
    property IsSecure: Boolean read GetIsSecure;
  end;

  {--------------------------------------------}
  TALHttpServerResponse = class(TALHTTPResponse)
  public
    type
      TCachePolicyType = (
        Nocache,
        UserInvalidates,
        TimeToLive);
  protected
    function GetBodyFileHandle: THandle; virtual; abstract;
    procedure SetBodyFileHandle(const AValue: THandle); virtual; abstract;
    function GetBodyByteRangeStartingOffset: UInt64; virtual; abstract;
    procedure SetBodyByteRangeStartingOffset(const AValue: UInt64); virtual; abstract;
    function GetBodyByteRangeLength: UInt64; virtual; abstract;
    procedure SetBodyByteRangeLength(const AValue: UInt64); virtual; abstract;
    function GetCachePolicyType: TCachePolicyType; virtual; abstract;
    procedure SetCachePolicyType(const AValue: TCachePolicyType); virtual; abstract;
    function GetCacheSecondsToLive: Cardinal; virtual; abstract;
    procedure SetCacheSecondsToLive(const AValue: Cardinal); virtual; abstract;
  public
    property BodyFileHandle: THandle read GetBodyFileHandle write SetBodyFileHandle;
    property BodyByteRangeStartingOffset: UInt64 read GetBodyByteRangeStartingOffset write SetBodyByteRangeStartingOffset;
    property BodyByteRangeLength: UInt64 read GetBodyByteRangeLength write SetBodyByteRangeLength;
    property CachePolicyType: TCachePolicyType read GetCachePolicyType write SetCachePolicyType;
    property CacheSecondsToLive: Cardinal read GetCacheSecondsToLive write SetCacheSecondsToLive;
  end;

  {-------------------}
  TALHttpServer = class
  public
    Type
      TRequestEvent = procedure(Const ARequest: TALHttpServerRequest; Const AResponse: TALHttpServerResponse) of object;
  private
    FMaxBodySize: Int64;
    FOnRequest: TRequestEvent;
  protected
    FRunning: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;
    property Running: Boolean read FRunning;
    property MaxBodySize: Int64 read FMaxBodySize write FMaxBodySize;
    property OnRequest: TRequestEvent read FOnRequest write FOnRequest;
  end;

implementation

{*******************************}
constructor TALHttpServer.Create;
begin
  inherited;
  FRunning := False;
  FMaxBodySize := 33554432; // 32 MiB (binary; 1 MiB = 1,048,576 bytes)
  FOnRequest := nil;
end;

{*******************************}
destructor TALHttpServer.Destroy;
begin
  Stop;
  inherited;
end;

end.
