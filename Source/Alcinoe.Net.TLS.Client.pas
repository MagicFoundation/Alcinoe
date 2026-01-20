unit Alcinoe.Net.TLS.Client;

interface

{$I Alcinoe.inc}

Uses
  Alcinoe.net.Socket.Client;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTlsClient = class(TALSocketClient)
  private
    FRequireValidServerCertificate: Boolean;
  public
    constructor Create; override;
    /// <summary>
    ///   Enables or disables server certificate validation during the TLS handshake.
    /// </summary>
    /// <remarks>
    ///   When True (recommended), the connection fails if the server certificate is invalid
    ///   (untrusted, expired, or hostname mismatch). When False, all certificates are accepted.
    ///   You can test this behavior using https://badssl.com/
    /// </remarks>
    property RequireValidServerCertificate: Boolean read FRequireValidServerCertificate write FRequireValidServerCertificate;
    procedure Connect(const AHost: String; const APort: Integer; const AStartTls: Boolean = true); reintroduce; virtual; abstract;
    procedure StartTls; virtual; abstract;
    procedure ShutdownTls; virtual; abstract;
  end;

implementation

{******************************}
constructor TALTlsClient.Create;
begin
  inherited Create;
  FRequireValidServerCertificate := True;
end;

end.