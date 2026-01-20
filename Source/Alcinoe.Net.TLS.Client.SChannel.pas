unit Alcinoe.Net.TLS.Client.SChannel;

interface

{$I Alcinoe.inc}
{$SCOPEDENUMS OFF}

Uses
  System.SysUtils,
  Winapi.WinSock2,
  Alcinoe.net.TLS.Client,
  Alcinoe.WinApi.SSPI;

type


  {----------------------------------------}
  TAlSChannelTlsClient = class(TALTlsClient)
  private
    FSocket: TSocket;
    FCredHandle: CredHandle;
    FCtxtHandle: CtxtHandle;
    FRecvEncryptedBufferBytes: TBytes;
    FRecvEncryptedBufferUsed: Integer;
    FRecvPlainBufferBytes: TBytes;
    FRecvPlainBufferPos: Integer;
    FRecvPlainBufferUsed: Integer;
    FStreamSizes: SecPkgContext_StreamSizes;
    FHost: String;
    procedure HandshakeLoop;
  protected
    procedure SetSendTimeout(const Value: integer); override;
    procedure SetReceiveTimeout(const Value: integer); override;
    procedure SetKeepAlive(const Value: boolean); override;
    procedure SetTCPNoDelay(const Value: boolean); override;
    function GetConnected: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Connect(const AHost: String; const APort: Integer; const AStartTls: Boolean = true); override;
    procedure Disconnect; override;
    function Send(const Buf; Len: Integer): Integer; override;
    function Receive(var Buf; Len: Integer): Integer; override;
    procedure StartTls; override;
    procedure ShutdownTls; override;
  end;

implementation

Uses
  Winapi.Windows,
  System.Math,
  Alcinoe.Net.Socket.Client.Win,
  Alcinoe.WinApi.Windows,
  Alcinoe.WinApi.WinError,
  Alcinoe.WinApi.SChannel;

{**}
Type
  _TALWinSocketClientProtectedAccess = class(TALWinSocketClient);

{**************************************}
constructor TAlSChannelTlsClient.Create;
begin
  inherited Create;
  FSocket := INVALID_SOCKET;
  FCredHandle.dwLower := 0;
  FCredHandle.dwUpper := 0;
  FCtxtHandle.dwLower := 0;
  FCtxtHandle.dwUpper := 0;
  FRecvEncryptedBufferBytes := [];
  FRecvEncryptedBufferUsed := 0;
  FRecvPlainBufferBytes := [];
  FRecvPlainBufferPos := 0;
  FRecvPlainBufferUsed := 0;
  ZeroMemory(@FStreamSizes, SizeOf(FStreamSizes));
  FHost := '';
end;

{**************************************}
destructor TAlSChannelTlsClient.Destroy;
begin
  Disconnect;
  Inherited;
end;

{*****************************************************************************************************************}
procedure TAlSChannelTlsClient.Connect(const AHost: String; const APort: Integer; const AStartTls: Boolean = true);
begin
  try
    _TALWinSocketClientProtectedAccess.Connect(FSocket, AHost, APort);
    _TALWinSocketClientProtectedAccess.SetSendTimeout(FSocket, GetSendTimeout);
    _TALWinSocketClientProtectedAccess.SetReceiveTimeout(FSocket, GetReceiveTimeout);
    _TALWinSocketClientProtectedAccess.SetKeepAlive(FSocket, GetKeepAlive);
    _TALWinSocketClientProtectedAccess.SetTCPNoDelay(FSocket, GetTCPNoDelay);
    FHost := AHost;
    if AStartTls then StartTLs;
  Except
    Disconnect;
    Raise;
  end;
end;

{****************************************}
procedure TAlSChannelTlsClient.Disconnect;
begin
  ShutdownTls;
  _TALWinSocketClientProtectedAccess.Disconnect(FSocket);
  FHost := '';
end;

{**************************************}
procedure TAlSChannelTlsClient.SetSendTimeout(const Value: integer);
begin
  inherited;
  if FSocket <> INVALID_SOCKET then
    _TALWinSocketClientProtectedAccess.SetSendTimeout(FSocket, Value);
end;

{**************************************}
procedure TAlSChannelTlsClient.SetReceiveTimeout(const Value: integer);
begin
  inherited;
  if FSocket <> INVALID_SOCKET then
    _TALWinSocketClientProtectedAccess.SetReceiveTimeout(FSocket, Value);
end;

{**************************************}
procedure TAlSChannelTlsClient.SetKeepAlive(const Value: boolean);
begin
  inherited;
  if FSocket <> INVALID_SOCKET then
    _TALWinSocketClientProtectedAccess.SetKeepAlive(FSocket, Value);
end;

{**************************************}
procedure TAlSChannelTlsClient.SetTCPNoDelay(const Value: boolean);
begin
  inherited;
  if FSocket <> INVALID_SOCKET then
    _TALWinSocketClientProtectedAccess.SetTCPNoDelay(FSocket, Value);
end;

{**************************************************}
function TAlSChannelTlsClient.GetConnected: Boolean;
begin
  Result := FSocket <> INVALID_SOCKET;
end;

{*******************************************}
procedure TAlSChannelTlsClient.HandshakeLoop;
begin

  //
  // loop
  //

  var LSecurityStatus: SECURITY_STATUS := SEC_I_CONTINUE_NEEDED;
  While (LSecurityStatus = SEC_I_CONTINUE_NEEDED) or
        (LSecurityStatus = SEC_E_INCOMPLETE_MESSAGE) do begin

    // recv more encrypted handshake bytes
    if (LSecurityStatus = SEC_E_INCOMPLETE_MESSAGE) or
       (FRecvEncryptedBufferUsed <= 0) then begin
      if Length(FRecvEncryptedBufferBytes) - FRecvEncryptedBufferUsed <= 0 then SetLength(FRecvEncryptedBufferBytes, Round(Length(FRecvEncryptedBufferBytes) * 1.5));
      var LGot: Integer := _TALWinSocketClientProtectedAccess.Receive(FSocket, FRecvEncryptedBufferBytes[FRecvEncryptedBufferUsed], Length(FRecvEncryptedBufferBytes) - FRecvEncryptedBufferUsed);
      if LGot <= 0 then raise Exception.Create('Connection closed during handshake loop');
      Inc(FRecvEncryptedBufferUsed, LGot);
    end;

    var LContextAttr: ULONG := 0;

    var LInBuffers: array[0..1] of SecBuffer;
    LInBuffers[0].cbBuffer := FRecvEncryptedBufferUsed; // Size of the buffer, in bytes
    LInBuffers[0].BufferType := SECBUFFER_TOKEN; // Type of the buffer (below)
    LInBuffers[0].pvBuffer := @FRecvEncryptedBufferBytes[0]; // Pointer to the buffer
    LInBuffers[1].cbBuffer := 0; // Size of the buffer, in bytes
    LInBuffers[1].BufferType := SECBUFFER_EMPTY; // Type of the buffer (below)
    LInBuffers[1].pvBuffer := nil; // Pointer to the buffer

    var LInDesc: SecBufferDesc;
    LInDesc.ulVersion := SECBUFFER_VERSION; // Version number
    LInDesc.cBuffers := 2; // Number of buffers
    LInDesc.pBuffers := @LInBuffers[0]; // Pointer to array of buffers

    var LOutBuffer: SecBuffer;
    LOutBuffer.cbBuffer := 0; // Size of the buffer, in bytes
    LOutBuffer.BufferType := SECBUFFER_TOKEN; // Type of the buffer (below)
    LOutBuffer.pvBuffer := nil; // Pointer to the buffer

    var LOutDesc: SecBufferDesc;
    LOutDesc.ulVersion := SECBUFFER_VERSION; // Version number
    LOutDesc.cBuffers := 1; // Number of buffers
    LOutDesc.pBuffers := @LOutBuffer; // Pointer to array of buffers

    LSecurityStatus :=
      InitializeSecurityContextW(
        @FCredHandle, // phCredential: PCredHandle; // Cred to base context
        @FCtxtHandle, // phContext: PCtxtHandle; // Existing context (OPT)
        PSEC_WCHAR(FHost), // pszTargetName: PSEC_WCHAR; // Name of target
        ISC_REQ_SEQUENCE_DETECT or
        ISC_REQ_REPLAY_DETECT or
        ISC_REQ_CONFIDENTIALITY or
        ISC_REQ_ALLOCATE_MEMORY or
        ISC_REQ_STREAM or
        ISC_REQ_EXTENDED_ERROR, // fContextReq: ULONG; // Context Requirements
        0, // Reserved1: ULONG; // Reserved, MBZ
        SECURITY_NATIVE_DREP, // TargetDataRep: ULONG; // Data rep of target
        @LInDesc, // pInput: PSecBufferDesc; // Input Buffers
        0, // Reserved2: ULONG; // Reserved, MBZ
        @FCtxtHandle, // phNewContext: PCtxtHandle; // (out) New Context handle
        @LOutDesc, // pOutput: PSecBufferDesc; // (inout) Output Buffers
        @LContextAttr, // pfContextAttr: PULONG; // (out) Context attrs
        nil); // ptsExpiry: PTimeStamp; // (out) Life span (OPT)
    if (LSecurityStatus <> SEC_I_CONTINUE_NEEDED) and
       (LSecurityStatus <> SEC_E_INCOMPLETE_MESSAGE) then
      ALCheckWinApiSecurityStatus('InitializeSecurityContextW(loop)', LSecurityStatus);

    // send output token if any
    if LOutBuffer.pvBuffer <> nil then begin
      try
        if LOutBuffer.cbBuffer > 0 then
          _TALWinSocketClientProtectedAccess.SendAll(FSocket, LOutBuffer.pvBuffer^, LOutBuffer.cbBuffer);
      finally
        ALCheckWinApiSecurityStatus(
          'FreeContextBuffer',
          FreeContextBuffer(LOutBuffer.pvBuffer));
      end;
    end;
    LOutBuffer.cbBuffer := 0;
    LOutBuffer.pvBuffer := nil;

    // https://learn.microsoft.com/en-us/windows/win32/secauthn/extra-buffers-returned-by-schannel
    // If the input buffer contains too little information, the functions return
    // SEC_E_INCOMPLETE_MESSAGE. The caller must obtain additional data from the
    // remote party and call the function again.
    if LSecurityStatus = SEC_E_INCOMPLETE_MESSAGE then
      Continue;

    // https://learn.microsoft.com/en-us/windows/win32/secauthn/extra-buffers-returned-by-schannel
    // When the input buffers contains too much information, Schannel does not
    // treat this as an error. The function processes as much of the input as it
    // can, and returns the status code for that processing activity. In addition,
    // Schannel indicates the presence of unprocessed information in the input
    // buffers by returning an output buffer of type SECBUFFER_EXTRA. Testing
    // the output buffers for this type of buffer is the only way to detect this
    // situation. The cbBuffer field of the extra buffer structure indicates how
    // many bytes of input were not processed.
    Var LFound := False;
    for var I := High(LInBuffers) downto Low(LInBuffers) do begin
      if (LInBuffers[I].BufferType = SECBUFFER_EXTRA) and (LInBuffers[I].cbBuffer > 0) then begin
        {$If defined(debug)}
        if (integer(LInBuffers[I].cbBuffer) > FRecvEncryptedBufferUsed) then
          Raise Exception.Create('Error 1551C638-91B6-408A-82D3-9C7AB7A7FF2B');
        {$ENDIf}
        Move(FRecvEncryptedBufferBytes[FRecvEncryptedBufferUsed - Integer(LInBuffers[I].cbBuffer)], FRecvEncryptedBufferBytes[0], LInBuffers[I].cbBuffer);
        FRecvEncryptedBufferUsed := LInBuffers[I].cbBuffer;
        LFound := True;
        Break;
      end
    end;
    If not LFound then
      FRecvEncryptedBufferUsed := 0;

  end;
  ALCheckWinApiSecurityStatus('InitializeSecurityContextW(loop)', LSecurityStatus);

end;

{**************************************}
procedure TAlSChannelTlsClient.StartTls;
begin

  //
  // Ensure a valid socket is attached before
  // starting the TLS handshake
  //

  if FSocket = INVALID_SOCKET then
    raise Exception.Create('No socket attached');
  if (FCredHandle.dwLower <> 0) or (FCredHandle.dwUpper <> 0) then
    raise Exception.Create('Credentials already initialized');
  if (FCtxtHandle.dwLower <> 0) or (FCtxtHandle.dwUpper <> 0) then
    raise Exception.Create('Security context already initialized');

  try

    //
    // Acquires a handle to preexisting credentials of
    // a security principal.
    //

    var LSchannelCred: SCHANNEL_CRED;
    var LSchCredentials: SCH_CREDENTIALS;
    var LAuthData: PVoid;
    if TOSVersion.Build < 17763 then begin // Windows 10 - 1809
      // fail with error SEC_E_ALGORITHM_MISMATCH if you specify TLS 1.3
      // https://learn.microsoft.com/en-us/answers/questions/708734/tls-1-3-doesnt-work-on-windows-11-through-schannel
      ZeroMemory(@LSchannelCred, SizeOf(LSchannelCred));
      LSchannelCred.dwVersion := SCHANNEL_CRED_VERSION;
      // LSchannelCred.cCreds: DWORD;
      // LSchannelCred.paCred: PPCCERT_CONTEXT;
      // LSchannelCred.hRootStore: HCERTSTORE;
      // LSchannelCred.cMappers: DWORD;
      // LSchannelCred.aphMappers: PP_HMAPPER;
      // LSchannelCred.cSupportedAlgs: DWORD;
      // LSchannelCred.palgSupportedAlgs: PALG_ID;
      // LSchannelCred.grbitEnabledProtocols: DWORD;
      // LSchannelCred.dwMinimumCipherStrength: DWORD;
      // LSchannelCred.dwMaximumCipherStrength: DWORD;
      // LSchannelCred.dwSessionLifespan: DWORD;
      LSchannelCred.dwFlags := SCH_CRED_NO_DEFAULT_CREDS or SCH_USE_STRONG_CRYPTO;
      if not RequireValidServerCertificate then
        LSchannelCred.dwFlags := LSchannelCred.dwFlags or SCH_CRED_MANUAL_CRED_VALIDATION;
      // LSchannelCred.dwCredFormat: DWORD;
      LAuthData := @LSchannelCred;
    end
    else begin
      ZeroMemory(@LSchCredentials, SizeOf(LSchCredentials));
      LSchCredentials.dwVersion := SCH_CREDENTIALS_VERSION;
      // LSchCredentials.dwCredFormat: DWORD;
      // LSchCredentials.cCreds: DWORD;
      // LSchCredentials.paCred: PPCCERT_CONTEXT;
      // LSchCredentials.hRootStore: HCERTSTORE;
      // LSchCredentials.cMappers: DWORD;
      // LSchCredentials.aphMappers: PP_HMAPPER;
      // LSchCredentials.dwSessionLifespan: DWORD;
      LSchCredentials.dwFlags := SCH_CRED_NO_DEFAULT_CREDS or SCH_USE_STRONG_CRYPTO;
      if not RequireValidServerCertificate then
        LSchCredentials.dwFlags := LSchCredentials.dwFlags or SCH_CRED_MANUAL_CRED_VALIDATION;
      // LSchCredentials.cTlsParameters: DWORD;
      // LSchCredentials.pTlsParameters: PTLS_PARAMETERS;
      LAuthData := @LSchCredentials;
    end;

    var LSecurityStatus: SECURITY_STATUS :=
          AcquireCredentialsHandleW(
            nil, // pszPrincipal: LPWSTR; // Name of principal
            UNISP_NAME_W, // pszPackage: LPWSTR; // Name of package
            SECPKG_CRED_OUTBOUND, // fCredentialUse: ULONG; // Flags indicating use
            nil, // pvLogonId: Pvoid; // Pointer to logon ID
            LAuthData, // pAuthData: Pvoid; // Package specific data
            nil, // pGetKeyFn: SEC_GET_KEY_FN; // Pointer to GetKey() func
            nil, // pvGetKeyArgument: Pvoid; // Value to pass to GetKey()
            @FCredHandle, // phCredential: PCredHandle; // (out) Cred Handle
            nil);  // ptsExpiry: PTimeStamp; // (out) Lifetime (optional)
    ALCheckWinApiSecurityStatus('AcquireCredentialsHandleW', LSecurityStatus);

    //
    // Initiates the client side, outbound security context
    // from a credential handle (build a security context between
    // the client application and a remote peer
    //

    var LContextAttr: ULONG := 0;

    var LOutBuffer: SecBuffer;
    LOutBuffer.cbBuffer := 0; // Size of the buffer, in bytes
    LOutBuffer.BufferType := SECBUFFER_TOKEN; // Type of the buffer (below)
    LOutBuffer.pvBuffer := nil; // Pointer to the buffer

    var LOutDesc: SecBufferDesc;
    LOutDesc.ulVersion := SECBUFFER_VERSION; // Version number
    LOutDesc.cBuffers := 1; // Number of buffers
    LOutDesc.pBuffers := @LOutBuffer; // Pointer to array of buffers

    LSecurityStatus :=
      InitializeSecurityContextW(
        @FCredHandle, // phCredential: PCredHandle; // Cred to base context
        nil, // phContext: PCtxtHandle; // Existing context (OPT)
        PSEC_WCHAR(FHost), // pszTargetName: PSEC_WCHAR; // Name of target
        ISC_REQ_SEQUENCE_DETECT or
        ISC_REQ_REPLAY_DETECT or
        ISC_REQ_CONFIDENTIALITY or
        ISC_REQ_ALLOCATE_MEMORY or
        ISC_REQ_STREAM or
        ISC_REQ_EXTENDED_ERROR, // fContextReq: ULONG; // Context Requirements
        0, // Reserved1: ULONG; // Reserved, MBZ
        SECURITY_NATIVE_DREP, // TargetDataRep: ULONG; // Data rep of target
        nil, // pInput: PSecBufferDesc; // Input Buffers
        0, // Reserved2: ULONG; // Reserved, MBZ
        @FCtxtHandle, // phNewContext: PCtxtHandle; // (out) New Context handle
        @LOutDesc, // pOutput: PSecBufferDesc; // (inout) Output Buffers
        @LContextAttr, // pfContextAttr: PULONG; // (out) Context attrs
        nil); // ptsExpiry: PTimeStamp; // (out) Life span (OPT)
    if (LSecurityStatus <> SEC_I_CONTINUE_NEEDED) then
      ALCheckWinApiSecurityStatus('InitializeSecurityContextW(StartTls)', LSecurityStatus);
    if (LSecurityStatus = SEC_E_OK) then
      raise Exception.Create('Unexpected SEC_E_OK on initial InitializeSecurityContextW call');

    //
    // Send initial token back to the server
    //

    if LOutBuffer.pvBuffer <> nil then begin
      try
        if LOutBuffer.cbBuffer > 0 then
          _TALWinSocketClientProtectedAccess.SendAll(FSocket, LOutBuffer.pvBuffer^, LOutBuffer.cbBuffer);
      finally
        ALCheckWinApiSecurityStatus(
          'FreeContextBuffer',
          FreeContextBuffer(LOutBuffer.pvBuffer));
      end;
    end;
    LOutBuffer.cbBuffer := 0;
    LOutBuffer.pvBuffer := nil;

    //
    // Prepare EncryptedBufferBytes
    //

    SetLength(FRecvEncryptedBufferBytes, 64 * 1024);
    FRecvEncryptedBufferUsed := 0;

    //
    // loop
    //

    HandshakeLoop;

    //
    // Queries the sizes of the various parts of a stream used in
    // the per-message functions
    //

    ALCheckWinApiSecurityStatus(
      'QueryContextAttributes(SECPKG_ATTR_STREAM_SIZES)',
      QueryContextAttributesW(
        @FCtxtHandle,
        SECPKG_ATTR_STREAM_SIZES,
        @FStreamSizes));

  Except
    ShutdownTls;
    raise;
  end;
end;

{*****************************************}
procedure TAlSChannelTlsClient.ShutdownTls;
begin

  if (FCredHandle.dwLower = 0) and
     (FCredHandle.dwUpper = 0) and
     (FCtxtHandle.dwLower = 0) and
     (FCtxtHandle.dwUpper = 0) then exit;

  Try

    try

      //
      // https://learn.microsoft.com/en-us/windows/win32/secauthn/shutting-down-an-schannel-connection
      // When a client or server is finished with a connection, it must shut it down.
      // The other party, in turn, must recognize the shutdown and delete the connection.
      //
      // To shut down an Schannel connection
      //
      // 1. Call the ApplyControlToken function, specifying the SCHANNEL_SHUTDOWN
      //    control token.
      // 2. After receiving an SEC_E_OK return value from ApplyControlToken, call
      //    the InitializeSecurityContext (Schannel) (clients) or
      //    AcceptSecurityContext (Schannel) (servers) function, passing in
      //    empty buffers.
      // 3. Proceed as though your application were creating a new connection
      //    until the function returns SEC_I_CONTEXT_EXPIRED or SEC_E_OK to
      //    indicate that the connection is shut down.
      // 4. Send the final output information, if any, to the remote party.
      // 5. Call DeleteSecurityContext to free resources held by the connection.
      //

      //
      // Notify schannel that we are about to close the connection.
      //

      var LInBuffers: array[0..1] of SecBuffer;
      var LToken: Integer := SCHANNEL_SHUTDOWN;
      LInBuffers[0].cbBuffer := SizeOf(LToken); // Size of the buffer, in bytes
      LInBuffers[0].BufferType := SECBUFFER_TOKEN; // Type of the buffer (below)
      LInBuffers[0].pvBuffer := @LToken; // Pointer to the buffer

      var LInDesc: SecBufferDesc;
      LInDesc.ulVersion := SECBUFFER_VERSION; // Version number
      LInDesc.cBuffers := 1; // Number of buffers
      LInDesc.pBuffers := @LInBuffers[0]; // Pointer to array of buffers

      ALCheckWinApiSecurityStatus(
        'ApplyControlToken',
        ApplyControlToken(
          @FCtxtHandle, // phContext: PCtxtHandle;
          @LInDesc)); // pInput: PSecBufferDesc

      //
      // Build an SSL close notify message.
      //

      var LContextAttr: ULONG := 0;

      var LOutBuffer: SecBuffer;
      LOutBuffer.cbBuffer := 0; // Size of the buffer, in bytes
      LOutBuffer.BufferType := SECBUFFER_TOKEN; // Type of the buffer (below)
      LOutBuffer.pvBuffer := nil; // Pointer to the buffer

      var LOutDesc: SecBufferDesc;
      LOutDesc.ulVersion := SECBUFFER_VERSION; // Version number
      LOutDesc.cBuffers := 1; // Number of buffers
      LOutDesc.pBuffers := @LOutBuffer; // Pointer to array of buffers

      var LSecurityStatus: SECURITY_STATUS :=
            InitializeSecurityContextW(
              @FCredHandle, // phCredential: PCredHandle; // Cred to base context
              @FCtxtHandle, // phContext: PCtxtHandle; // Existing context (OPT)
              PSEC_WCHAR(FHost), // pszTargetName: PSEC_WCHAR; // Name of target
              ISC_REQ_SEQUENCE_DETECT   or
              ISC_REQ_REPLAY_DETECT     or
              ISC_REQ_CONFIDENTIALITY   or
              ISC_RET_EXTENDED_ERROR    or
              ISC_REQ_ALLOCATE_MEMORY   or
              ISC_REQ_STREAM, // fContextReq: ULONG; // Context Requirements
              0, // Reserved1: ULONG; // Reserved, MBZ
              SECURITY_NATIVE_DREP, // TargetDataRep: ULONG; // Data rep of target
              nil, // pInput: PSecBufferDesc; // Input Buffers
              0, // Reserved2: ULONG; // Reserved, MBZ
              @FCtxtHandle, // phNewContext: PCtxtHandle; // (out) New Context handle
              @LOutDesc, // pOutput: PSecBufferDesc; // (inout) Output Buffers
              @LContextAttr, // pfContextAttr: PULONG; // (out) Context attrs
              nil); // ptsExpiry: PTimeStamp; // (out) Life span (OPT)
      if (LSecurityStatus <> SEC_I_CONTEXT_EXPIRED) then
        ALCheckWinApiSecurityStatus('InitializeSecurityContextW(SHUTDOWN)', LSecurityStatus);

      //
      // Send the close notify message to the server.
      //

      if LOutBuffer.pvBuffer <> nil then begin
        try
          if LOutBuffer.cbBuffer > 0 then
            _TALWinSocketClientProtectedAccess.SendAll(FSocket, LOutBuffer.pvBuffer^, LOutBuffer.cbBuffer);
        finally
          ALCheckWinApiSecurityStatus(
            'FreeContextBuffer',
            FreeContextBuffer(LOutBuffer.pvBuffer));
        end;
      end;
      LOutBuffer.cbBuffer := 0;
      LOutBuffer.pvBuffer := nil;

    except
      // best-effort shutdown; ignore errors
    end;

    try
      DeleteSecurityContext(@FCtxtHandle);
      FreeCredentialsHandle(@FCredHandle);
    except
      // best-effort shutdown; ignore errors
    end;

  Finally
    FCredHandle.dwLower := 0;
    FCredHandle.dwUpper := 0;
    FCtxtHandle.dwLower := 0;
    FCtxtHandle.dwUpper := 0;
    FRecvEncryptedBufferBytes := [];
    FRecvEncryptedBufferUsed := 0;
    FRecvPlainBufferBytes := [];
    FRecvPlainBufferPos := 0;
    FRecvPlainBufferUsed := 0;
    ZeroMemory(@FStreamSizes, SizeOf(FStreamSizes));
  End;
end;

{*******************************************************************}
function TAlSChannelTlsClient.Send(const Buf; Len: Integer): Integer;
begin

  //
  // Nothing to write if the buffer length
  // is zero or negative
  //

  Result := 0;
  if Len <= 0 then Exit;

  //
  // If TLS has not been initialized (StartTls not called),
  // the security context and credentials are not set.
  // In this case, send the data directly over the socket
  // without applying any encryption.
  //

  if (FCredHandle.dwLower = 0) and
     (FCredHandle.dwUpper = 0) and
     (FCtxtHandle.dwLower = 0) and
     (FCtxtHandle.dwUpper = 0) then begin
    Result := _TALWinSocketClientProtectedAccess.Send(FSocket, Buf, Len);
    Exit;
  end;

  //
  // EncryptMessage requires three writable regions:
  //   - STREAM_HEADER  (cbHeader bytes)
  //   - DATA           (plaintext bytes)
  //   - STREAM_TRAILER (cbTrailer bytes)
  //
  // The caller buffer is read-only here (const Buf) and does not include
  // space for header/trailer, so we copy plaintext into a temporary work
  // buffer laid out as: [header][plaintext][trailer].
  //
  // We allocate the work buffer once and reuse it for each chunk.
  //

  var PBuf: PByte := @Buf;
  var LWork: TBytes;
  SetLength(LWork, FStreamSizes.cbHeader + Min(Len, FStreamSizes.cbMaximumMessage) + FStreamSizes.cbTrailer);

  //
  // Encrypt and send in chunks of at most cbMaximumMessage plaintext bytes.
  //

  while Len > 0 do begin

    var LCount := Min(Len, FStreamSizes.cbMaximumMessage);
    Move(PBuf^, LWork[FStreamSizes.cbHeader], LCount);

    var LBuffers: array[0..3] of SecBuffer;
    LBuffers[0].cbBuffer := FStreamSizes.cbHeader; // Size of the buffer, in bytes
    LBuffers[0].BufferType := SECBUFFER_STREAM_HEADER; // Type of the buffer (below)
    LBuffers[0].pvBuffer := @LWork[0]; // Pointer to the buffer
    LBuffers[1].cbBuffer := LCount;  // Size of the buffer, in bytes
    LBuffers[1].BufferType := SECBUFFER_DATA; // Type of the buffer (below)
    LBuffers[1].pvBuffer := @LWork[FStreamSizes.cbHeader]; // Pointer to the buffer
    LBuffers[2].cbBuffer := FStreamSizes.cbTrailer; // Size of the buffer, in bytes
    LBuffers[2].BufferType := SECBUFFER_STREAM_TRAILER; // Type of the buffer (below)
    LBuffers[2].pvBuffer := @LWork[FStreamSizes.cbHeader + LCount]; // Pointer to the buffer
    LBuffers[3].cbBuffer := 0; // Size of the buffer, in bytes
    LBuffers[3].BufferType := SECBUFFER_EMPTY;
    LBuffers[3].pvBuffer := nil; // Pointer to the buffer

    var LDesc: SecBufferDesc;
    LDesc.ulVersion := SECBUFFER_VERSION; // Version number
    LDesc.cBuffers := 4; // Number of buffers
    LDesc.pBuffers := @LBuffers[0]; // Pointer to array of buffers

    // EncryptMessage writes the TLS record into the provided buffers and may
    // adjust the final sizes in cbBuffer (especially DATA and TRAILER).
    ALCheckWinApiSecurityStatus(
      'EncryptMessage',
      EncryptMessage(
        @FCtxtHandle, // phContext: PCtxtHandle;
        0, // fQOP: ULONG;
        @LDesc, // pMessage: PSecBufferDesc;
        0)); // MessageSeqNo: ULONG);

    // Send the produced TLS record (header + data + trailer).
    _TALWinSocketClientProtectedAccess.SendAll(FSocket, LWork[0], LBuffers[0].cbBuffer + LBuffers[1].cbBuffer + LBuffers[2].cbBuffer);

    Inc(Result, LCount);
    Inc(PBuf, LCount);
    Dec(Len, LCount);

  end;
end;

{********************************************************************}
function TAlSChannelTlsClient.Receive(var Buf; Len: Integer): Integer;
begin

  //
  // Nothing to read if the buffer length
  // is zero or negative
  //

  Result := 0;
  if Len <= 0 then Exit;

  //
  // If TLS has not been initialized (StartTls not called),
  // the security context and credentials are not set.
  // In this case, send the data directly over the socket
  // without applying any encryption.
  //

  if (FCredHandle.dwLower = 0) and
     (FCredHandle.dwUpper = 0) and
     (FCtxtHandle.dwLower = 0) and
     (FCtxtHandle.dwUpper = 0) then begin
    Result := _TALWinSocketClientProtectedAccess.Receive(FSocket, Buf, Len);
    Exit;
  end;

  //
  // Fill the caller's buffer with plaintext (decrypted) bytes.
  // This function may return fewer bytes than requested (e.g. a TLS
  // record boundary, non-blocking socket, orderly shutdown, etc.).
  //

  var LDidSocketReceive := False;
  var PBuf: PByte := @Buf;
  while Len > 0 do begin

    //
    // If we already have decrypted plaintext buffered from a previous
    // call/record, serve it first before touching the socket again.
    //

    if (FRecvPlainBufferUsed > FRecvPlainBufferPos) then begin
      var LCopyLen: Integer := Min(Len, FRecvPlainBufferUsed - FRecvPlainBufferPos);
      Move(FRecvPlainBufferBytes[FRecvPlainBufferPos], PBuf^, LCopyLen);
      Inc(FRecvPlainBufferPos, LCopyLen);
      Inc(PBuf, LCopyLen);
      Dec(Len, LCopyLen);
      Inc(Result, LCopyLen);
      Continue;
    end;
    FRecvPlainBufferPos := 0;
    FRecvPlainBufferUsed := 0;

    //
    // No plaintext pending -> make sure we have encrypted bytes to feed
    // into DecryptMessage (TLS is record-based; we may need to accumulate
    // multiple recv() to get a full record).
    //

    if FRecvEncryptedBufferUsed = 0 then begin
      // Avoid performing multiple socket receives in the same read cycle.
      if (LDidSocketReceive) and (PBuf <> PByte(@Buf)) then exit;
      LDidSocketReceive := true;
      // Note: Using ioctlsocket(FSocket, FIONREAD, ...) was considered but rejected
      // because FIONREAD returns only immediately available bytes (up to 65536),
      // but recv can return much more as data continues to arrive.
      if length(FRecvEncryptedBufferBytes) < Len then setlength(FRecvEncryptedBufferBytes, Len);
      var LGot: Integer := _TALWinSocketClientProtectedAccess.Receive(FSocket, FRecvEncryptedBufferBytes[0], Len);
      if LGot <= 0 then break;
      FRecvEncryptedBufferUsed := LGot;
    end;

    //
    // Ask SChannel to decrypt what we currently have.
    // DecryptMessage may:
    //   - return plaintext in one of the output buffers (SECBUFFER_DATA)
    //   - report extra unused encrypted bytes (SECBUFFER_EXTRA)
    //   - ask for more encrypted bytes (SEC_E_INCOMPLETE_MESSAGE)
    //

    var LBuffers: array[0..3] of SecBuffer;
    LBuffers[0].cbBuffer := FRecvEncryptedBufferUsed; // Size of the buffer, in bytes
    LBuffers[0].BufferType := SECBUFFER_DATA; // Type of the buffer (below)
    LBuffers[0].pvBuffer := @FRecvEncryptedBufferBytes[0]; // Pointer to the buffer
    LBuffers[1].cbBuffer := 0;  // Size of the buffer, in bytes
    LBuffers[1].BufferType := SECBUFFER_EMPTY; // Type of the buffer (below)
    LBuffers[1].pvBuffer := nil; // Pointer to the buffer
    LBuffers[2].cbBuffer := 0; // Size of the buffer, in bytes
    LBuffers[2].BufferType := SECBUFFER_EMPTY; // Type of the buffer (below)
    LBuffers[2].pvBuffer := nil; // Pointer to the buffer
    LBuffers[3].cbBuffer := 0; // Size of the buffer, in bytes
    LBuffers[3].BufferType := SECBUFFER_EMPTY;
    LBuffers[3].pvBuffer := nil; // Pointer to the buffer

    var LDesc: SecBufferDesc;
    LDesc.ulVersion := SECBUFFER_VERSION; // Version number
    LDesc.cBuffers := 4; // Number of buffers
    LDesc.pBuffers := @LBuffers[0]; // Pointer to array of buffers

    var LSecurityStatus: SECURITY_STATUS :=
          DecryptMessage(
            @FCtxtHandle, // phContext: PCtxtHandle;
            @LDesc, // pMessage: PSecBufferDesc;
            0, // MessageSeqNo: ULONG;
            nil); // pfQOP: PULONG

    //
    // Not enough encrypted data for a full TLS record.
    // Grow the buffer if needed, recv more bytes, and retry decrypt.
    //

    if LSecurityStatus = SEC_E_INCOMPLETE_MESSAGE then begin
      if FRecvEncryptedBufferUsed = Length(FRecvEncryptedBufferBytes) then Setlength(FRecvEncryptedBufferBytes, Round(length(FRecvEncryptedBufferBytes) * 1.5));
      var LGot: Integer := _TALWinSocketClientProtectedAccess.Receive(FSocket, FRecvEncryptedBufferBytes[FRecvEncryptedBufferUsed], Length(FRecvEncryptedBufferBytes) - FRecvEncryptedBufferUsed);
      if LGot <= 0 then Raise Exception.Create('Connection closed while waiting for more encrypted data');
      Inc(FRecvEncryptedBufferUsed, LGot);
      Continue;
    end;

    //
    // Peer performed an orderly TLS shutdown (close_notify) and the
    // security context is no longer usable for application data.
    //

    if (LSecurityStatus = SEC_I_CONTEXT_EXPIRED) then begin
      ShutdownTls;
      Break;
    end;

    //
    // Any other status is either success or a hard failure.
    //

    if LSecurityStatus <> SEC_I_RENEGOTIATE then
      ALCheckWinApiSecurityStatus('DecryptMessage', LSecurityStatus);

    // Extract returned buffers:
    // - SECBUFFER_DATA  : decrypted plaintext
    // - SECBUFFER_EXTRA : encrypted bytes belonging to the next record
    //   (must be preserved and passed to the next DecryptMessage call)
    //

    var LDataPtr: PByte := nil;
    var LDataLen: Integer := 0;
    var LExtraPtr: PByte := nil;
    var LExtraLen: Integer := 0;

    // LBuffers can contain DATA and EXTRA in any slot 1..3
    // We'll scan all returned buffers.
    var I: Integer;
    for I := low(LBuffers) to high(LBuffers) do begin
      if LBuffers[I].BufferType = SECBUFFER_DATA then begin
        if LDataPtr <> nil then raise Exception.Create('Error 9721629E-50B5-4830-8013-2A4AAD1DEDCE');
        LDataPtr := LBuffers[i].pvBuffer;
        LDataLen := LBuffers[i].cbBuffer;
      end
      else if LBuffers[i].BufferType = SECBUFFER_EXTRA then begin
        if LExtraPtr <> nil then raise Exception.Create('Error 01E837F6-C976-4CB4-90D0-0C5850FFECF9');
        LExtraPtr := LBuffers[i].pvBuffer;
        LExtraLen := LBuffers[i].cbBuffer;
      end;
    end;

    //
    // Copy plaintext to the caller. If the TLS record produced more
    // plaintext than the caller asked for, return what fits and stash
    // the remainder for the next Read() call.
    //

    if (LDataPtr <> nil) and (LDataLen > 0) then begin
      if LDataLen <= Len then begin
        Move(LDataPtr^, PBuf^, LDataLen);
        Inc(PBuf, LDataLen);
        dec(Len, LDataLen);
        inc(Result, LDataLen);
      end
      else begin
        Move(LDataPtr^, PBuf^, Len);
        if length(FRecvPlainBufferBytes) < LDataLen - Len then SetLength(FRecvPlainBufferBytes, LDataLen - Len);
        Move((LDataPtr + Len)^, FRecvPlainBufferBytes[0], LDataLen - Len);
        FRecvPlainBufferUsed := LDataLen - Len;
        Inc(PBuf, Len);
        Inc(Result, Len);
        Len := 0;
      end;
    end;

    //
    // Preserve SECBUFFER_EXTRA (unused encrypted bytes) for the next decrypt.
    // If there is no EXTRA, we consumed the entire encrypted input.
    //

    if (LExtraPtr <> nil) and (LExtraLen > 0) then begin
      if length(FRecvEncryptedBufferBytes) < LExtraLen then SetLength(FRecvEncryptedBufferBytes, LExtraLen);
      Move(LExtraPtr^, FRecvEncryptedBufferBytes[0], LExtraLen);
      FRecvEncryptedBufferUsed := LExtraLen;
    end
    else
      FRecvEncryptedBufferUsed := 0;

    //
    // TLS post-handshake / Schannel “renegotiate needed”
    //

    if LSecurityStatus = SEC_I_RENEGOTIATE then begin

      // https://learn.microsoft.com/en-us/windows/win32/secauthn/recognizing-a-request-to-renegotiate-a-connection
      // The DecryptMessage (General) function traps requests for renegotiation
      // coming from the message sender. It notifies your application by
      // decrypting the message data and returning the SEC_I_RENEGOTIATE value.
      // Your application must handle such requests by calling AcceptSecurityContext
      // (General) (servers) or InitializeSecurityContext (General) (clients)
      // and passing the contents of SECBUFFER_EXTRA returned from DecryptMessage
      // in the SECBUFFER_TOKEN. After this initial call returns a value,
      // proceed as though your application were creating a new connection.

      var LContextAttr: ULONG := 0;

      var LInBuffers: array[0..1] of SecBuffer;
      LInBuffers[0].cbBuffer := FRecvEncryptedBufferUsed; // Size of the buffer, in bytes
      LInBuffers[0].BufferType := SECBUFFER_TOKEN; // Type of the buffer (below)
      LInBuffers[0].pvBuffer := @FRecvEncryptedBufferBytes[0]; // Pointer to the buffer
      LInBuffers[1].cbBuffer := 0; // Size of the buffer, in bytes
      LInBuffers[1].BufferType := SECBUFFER_EMPTY; // Type of the buffer (below)
      LInBuffers[1].pvBuffer := nil; // Pointer to the buffer

      var LInDesc: SecBufferDesc;
      LInDesc.ulVersion := SECBUFFER_VERSION; // Version number
      LInDesc.cBuffers := 2; // Number of buffers
      LInDesc.pBuffers := @LInBuffers[0]; // Pointer to array of buffers

      var LOutBuffer: SecBuffer;
      LOutBuffer.cbBuffer := 0; // Size of the buffer, in bytes
      LOutBuffer.BufferType := SECBUFFER_TOKEN; // Type of the buffer (below)
      LOutBuffer.pvBuffer := nil; // Pointer to the buffer

      var LOutDesc: SecBufferDesc;
      LOutDesc.ulVersion := SECBUFFER_VERSION; // Version number
      LOutDesc.cBuffers := 1; // Number of buffers
      LOutDesc.pBuffers := @LOutBuffer; // Pointer to array of buffers

      LSecurityStatus :=
        InitializeSecurityContextW(
          @FCredHandle, // phCredential: PCredHandle; // Cred to base context
          @FCtxtHandle, // phContext: PCtxtHandle; // Existing context (OPT)
          PSEC_WCHAR(FHost), // pszTargetName: PSEC_WCHAR; // Name of target
          ISC_REQ_SEQUENCE_DETECT or
          ISC_REQ_REPLAY_DETECT or
          ISC_REQ_CONFIDENTIALITY or
          ISC_REQ_ALLOCATE_MEMORY or
          ISC_REQ_STREAM or
          ISC_REQ_EXTENDED_ERROR, // fContextReq: ULONG; // Context Requirements
          0, // Reserved1: ULONG; // Reserved, MBZ
          SECURITY_NATIVE_DREP, // TargetDataRep: ULONG; // Data rep of target
          @LInDesc, // pInput: PSecBufferDesc; // Input Buffers
          0, // Reserved2: ULONG; // Reserved, MBZ
          @FCtxtHandle, // phNewContext: PCtxtHandle; // (out) New Context handle
          @LOutDesc, // pOutput: PSecBufferDesc; // (inout) Output Buffers
          @LContextAttr, // pfContextAttr: PULONG; // (out) Context attrs
          nil); // ptsExpiry: PTimeStamp; // (out) Life span (OPT)
      if (LSecurityStatus <> SEC_I_CONTINUE_NEEDED) then
        ALCheckWinApiSecurityStatus('InitializeSecurityContextW(RENEGOTIATE)', LSecurityStatus);
      if (LSecurityStatus = SEC_E_OK) then
        raise Exception.Create('Unexpected SEC_E_OK on initial InitializeSecurityContextW call');

      //
      // Send initial token back to the server
      //

      if LOutBuffer.pvBuffer <> nil then begin
        try
          if LOutBuffer.cbBuffer > 0 then
            _TALWinSocketClientProtectedAccess.SendAll(FSocket, LOutBuffer.pvBuffer^, LOutBuffer.cbBuffer);
        finally
          ALCheckWinApiSecurityStatus(
            'FreeContextBuffer',
            FreeContextBuffer(LOutBuffer.pvBuffer));
        end;
      end;
      LOutBuffer.cbBuffer := 0;
      LOutBuffer.pvBuffer := nil;

      //
      // loop
      //

      HandshakeLoop;

      //
      // Queries the sizes of the various parts of a stream used in
      // the per-message functions
      //

      ALCheckWinApiSecurityStatus(
        'QueryContextAttributes(SECPKG_ATTR_STREAM_SIZES)',
        QueryContextAttributesW(
          @FCtxtHandle,
          SECPKG_ATTR_STREAM_SIZES,
          @FStreamSizes));

    end;

  end;

end;

end.