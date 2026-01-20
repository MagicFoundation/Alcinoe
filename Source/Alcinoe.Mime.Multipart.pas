unit Alcinoe.Mime.Multipart;

interface

{$I Alcinoe.inc}

Uses
  System.SysUtils,
  System.Classes,
  Alcinoe.Common,
  Alcinoe.StringUtils;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALMultipartBaseEncoderA = class(TObject)
  private
    FOwnsOutputStream: Boolean;
    FBoundary: AnsiString;
    FStream: TProxyAggregateStream;
    FSection: TALStringStreamA;
    FLastBoundaryWritten: Boolean;
    procedure WriteString(const AString: AnsiString);
    function GetPayloadStream: TStream;
    function GetPayloadString: AnsiString;
    procedure AdjustLastBoundary;
    procedure BeginPart(const ASize: Int64);
    procedure EndPart;
  protected
    function GetMimeTypeHeader: AnsiString; virtual; abstract;
    function GenerateBoundary: AnsiString; virtual;
  public
    constructor Create(AOwnsOutputStream: Boolean = True); virtual;
    destructor Destroy; override;
    procedure SaveToFile(const AFileName: String);
    property PayloadStream: TStream read GetPayloadStream;
    property PayloadString: AnsiString read GetPayloadString;
    property MimeTypeHeader: AnsiString read GetMimeTypeHeader;
    property Boundary: AnsiString read FBoundary;
  end;

  { Example of a multipart/form-data HTTP message:
  ////////////////////////////////////////////////
  -----------------------------7d728842d0b36
  Content-Disposition: form-data; name="picture"; filename="C:\ud964D.tmp.jpg"
  Content-Type: image/pjpeg

  ÿØÿà ...
  -----------------------------7d728842d0b36
  Content-Disposition: form-data; name="field1"

  avalue
  -----------------------------7d728842d0b36
  Content-Disposition: form-data; name="field2"

  aValue
  -----------------------------7d728842d0b36--
  ////////////////////////////////////////////////}

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  {$IFNDEF ALCompilerVersionSupported130}
    {$MESSAGE WARN 'Check if System.Net.Mime.TMultipartFormData.pas is still the same and adjust the IFDEF'}
  {$ENDIF}
  TALMultipartFormDataEncoderA = class(TALMultipartBaseEncoderA)
  protected
    function GetMimeTypeHeader: AnsiString; override;
  public
    procedure AddText(
                const AFieldName: AnsiString;
                const AText: AnsiString;
                const AContentType: AnsiString = '';
                const AHeaders: TALNameValueArrayA = nil);
    procedure AddStream(
                const AFieldName: AnsiString;
                const AStream: TStream;
                const AOwnsStream: Boolean;
                const AFileName: AnsiString = '';
                const AContentType: AnsiString = '';
                const AHeaders: TALNameValueArrayA = nil); overload;
    procedure AddFile(
                const AFieldName: AnsiString;
                const AFilePath: String;
                const AContentType: AnsiString = '';
                const AHeaders: TALNameValueArrayA = nil);
    procedure AddBytes(
                const AFieldName: AnsiString;
                const ABytes: TBytes;
                const AFileName: AnsiString = '';
                const AContentType: AnsiString = '';
                const AHeaders: TALNameValueArrayA = nil);
  end;

  { Example of a multipart/alternative MIME message:
  ////////////////////////////////////////////////
  -----------------------------7d728842d0b36
  Content-Type: text/plain; charset=utf-8
  Content-Transfer-Encoding: 7bit

  Hello,
  This is the plain text version.

  -----------------------------7d728842d0b36
  Content-Type: text/html; charset=utf-8
  Content-Transfer-Encoding: 7bit

  <html>
    <body>
      <p>Hello,</p>
      <p>This is the <b>HTML</b> version.</p>
    </body>
  </html>

  -----------------------------7d728842d0b36--
  //////////////////////////////////////////////// }

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALMultipartAlternativeEncoderA = class(TALMultipartBaseEncoderA)
  protected
    function GetMimeTypeHeader: AnsiString; override;
  public
    /// <summary>
    ///   Add a text variant (commonly text/plain or text/html).
    ///   The order matters: put the least rich first (plain), then richer (html).
    ///   The part body will be Base64-encoded and the header
    ///   "Content-Transfer-Encoding: base64" will be emitted (unless overridden
    ///   via AHeaders).
    /// </summary>
    procedure AddText(
                const AText: AnsiString;
                const AContentType: AnsiString; // e.g. 'text/plain; charset=utf-8' or 'text/html; charset=utf-8'
                const AHeaders: TALNameValueArrayA = nil);
  end;

  { Example of a multipart/mixed MIME message (typical email with attachments):
  ////////////////////////////////////////////////

  -----------------------------7d728842d0b36
  Content-Type: text/plain; charset=utf-8
  Content-Transfer-Encoding: 7bit

  Hello,
  This is the message body.

  -----------------------------7d728842d0b36
  Content-Type: image/jpeg
  Content-Disposition: attachment; filename="C:\ud964D.tmp.jpg"
  Content-Transfer-Encoding: base64

  /9j/4AAQSkZJRgABAQAAAQABAAD...   (base64 data)

  -----------------------------7d728842d0b36
  Content-Type: application/pdf
  Content-Disposition: attachment; filename="document.pdf"
  Content-Transfer-Encoding: base64

  JVBERi0xLjQKJcTl8uXr...          (base64 data)

  -----------------------------7d728842d0b36--
  //////////////////////////////////////////////// }

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALMultipartMixedEncoderA = class(TALMultipartBaseEncoderA)
  protected
    function GetMimeTypeHeader: AnsiString; override;
  public
    /// <summary>
    ///   The part body will be Base64-encoded and the header
    ///   "Content-Transfer-Encoding: base64" will be emitted (unless overridden
    ///   via AHeaders).
    /// </summary>
    procedure AddText(
                const AText: AnsiString;
                const AContentType: AnsiString; // e.g. 'text/plain; charset=utf-8' or 'text/html; charset=utf-8'
                const AHeaders: TALNameValueArrayA = nil);
    /// <summary>
    ///   Adds a binary part (typically an attachment) to the multipart/mixed body.
    ///   The part body will be Base64-encoded and the header
    ///   "Content-Transfer-Encoding: base64" will be emitted (unless overridden
    ///   via AHeaders). In addition, "Content-Disposition: attachment" will be
    ///   emitted (unless overridden via AHeaders); when AFileName is provided,
    ///   it will be included as the attachment filename.
    /// </summary>
    procedure AddStream(
                const AStream: TStream;
                const AFileName: AnsiString;
                const AContentType: AnsiString = '';
                const AHeaders: TALNameValueArrayA = nil); overload;
    /// <summary>
    ///   Adds a file as a binary part (attachment) to the multipart/mixed body.
    ///   The part body will be Base64-encoded and the header
    ///   "Content-Transfer-Encoding: base64" will be emitted (unless overridden
    ///   via AHeaders). In addition, "Content-Disposition: attachment" will be
    ///   emitted (unless overridden via AHeaders); the file name will be used
    ///   as the attachment filename.
    /// </summary>
    procedure AddFile(
                const AFilePath: String;
                const AContentType: AnsiString = '';
                const AHeaders: TALNameValueArrayA = nil);
    /// <summary>
    ///   Adds a byte buffer as a binary part (typically an attachment) to the
    ///   multipart/mixed body. The part body will be Base64-encoded and the header
    ///   "Content-Transfer-Encoding: base64" will be emitted (unless overridden
    ///   via AHeaders). In addition, "Content-Disposition: attachment" will be
    ///   emitted (unless overridden via AHeaders); when AFileName is provided,
    ///   it will be included as the attachment filename.
    /// </summary>
    procedure AddBytes(
                const ABytes: TBytes;
                const AFileName: AnsiString;
                const AContentType: AnsiString = '';
                const AHeaders: TALNameValueArrayA = nil);
  end;

implementation

Uses
  System.IOUtils,
  System.AnsiStrings,
  System.NetConsts,
  ALcinoe.Cipher,
  Alcinoe.Mime.ContentTypes;

{**********************************************************************}
constructor TALMultipartBaseEncoderA.Create(AOwnsOutputStream: Boolean);
begin
  inherited Create;
  FOwnsOutputStream := AOwnsOutputStream;
  FBoundary := GenerateBoundary;
  FStream := TProxyAggregateStream.Create;
  FSection := nil;
  FLastBoundaryWritten := False;
end;

{******************************************}
destructor TALMultipartBaseEncoderA.Destroy;
begin
  ALFreeAndNil(FSection);
  if FOwnsOutputStream then ALFreeAndNil(FStream)
  // Check that last boundary is written
  else GetPayloadStream;
  inherited;
end;

{*************************************************************}
function TALMultipartBaseEncoderA.GenerateBoundary: AnsiString;
begin
  Result := '----------------------' + ALNewGUIDStringA(True{WithoutBracket}, true{WithoutHyphen});
end;

{*********************************************************************}
procedure TALMultipartBaseEncoderA.SaveToFile(const AFileName: String);
begin
  If TFile.Exists(AFilename) then
    TFile.Delete(AFileName);
  var LFileStream := TFileStream.Create(AFileName, fmCreate or fmShareExclusive);
  try
    LFileStream.CopyFrom(PayloadStream);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{***************************************************************}
procedure TALMultipartBaseEncoderA.BeginPart(const ASize: Int64);
begin
  FSection := TALStringStreamA.Create('');
  FSection.Size := ASize;
end;

{*****************************************}
procedure TALMultipartBaseEncoderA.EndPart;
begin
  try
    if FSection.Size <> FSection.Position then
      FSection.Size := FSection.Position;
    FStream.AddStream(FSection, True);
  finally
    FSection := nil;
  end;
end;

{****************************************************}
procedure TALMultipartBaseEncoderA.AdjustLastBoundary;
begin
  if FLastBoundaryWritten then
  begin
    FStream.RemoveStream(FStream.Count - 1);
    FLastBoundaryWritten := False;
  end;
end;

{**********************************************************}
function TALMultipartBaseEncoderA.GetPayloadStream: TStream;
begin
  if not FLastBoundaryWritten then
  begin
    BeginPart(2{--} + length(FBoundary) + 2{--} + 2{#13#10});
    try
      WriteString('--'); WriteString(FBoundary); WriteString('--'); WriteString(#13#10);
    finally
      EndPart;
    end;
    FLastBoundaryWritten := True;
  end;
  Result := FStream;
  Result.Position := 0;
end;

{*************************************************************}
function TALMultipartBaseEncoderA.GetPayloadString: AnsiString;
begin
  var LStream := GetPayloadStream;
  if LStream.Size <= 0 then Exit('');
  SetLength(Result, LStream.Size);
  LStream.Position:= 0;
  LStream.ReadBuffer(Result[low(Result)], LStream.Size);
end;

{************************************************************************}
procedure TALMultipartBaseEncoderA.WriteString(const AString: AnsiString);
begin
  FSection.WriteString(AString);
end;

{******************************************************************}
function TALMultipartFormDataEncoderA.GetMimeTypeHeader: AnsiString;
begin
  Result := 'multipart/form-data; boundary=' + FBoundary; // do not localize
end;

{*********************************************}
procedure TALMultipartFormDataEncoderA.AddText(
            const AFieldName: AnsiString;
            const AText: AnsiString;
            const AContentType: AnsiString = '';
            const AHeaders: TALNameValueArrayA = nil);
begin

  AdjustLastBoundary;

  Var LContentLength: AnsiString := ALInttostrA(length(AText));

  var LSize: int64 := 0;
  LSize := LSize + 2{--} + Length(FBoundary) + 2{#13#10};
  LSize := LSize + Length(sContentDisposition) + 19{: form-data; name="} + Length(AFieldName) + 1{"} + 2{#13#10};
  if AContentType <> '' then begin
    LSize := LSize + Length(sContentType) + 2{: } + Length(AContentType) + 2{#13#10};
  end;
  LSize := LSize + Length(sContentLength) + 2{: } + Length(LContentLength) + 2{#13#10};
  if AHeaders <> nil then
    for var I := low(AHeaders) to high(AHeaders) do
      if AHeaders[I].Value <> '' then begin
        LSize := LSize + Length(AHeaders[I].Name) + 2{: } + Length(AHeaders[I].Value) + 2{#13#10};
      end;
  LSize := LSize + 2{#13#10};
  LSize := LSize + Length(AText);
  LSize := LSize + 2{#13#10};

  BeginPart(LSize);
  try
    WriteString('--'); WriteString(FBoundary); WriteString(#13#10);
    WriteString(sContentDisposition); WriteString(': form-data; name="'); WriteString(AFieldName); WriteString('"'); WriteString(#13#10);
    if AContentType <> '' then begin
      WriteString(sContentType); WriteString(': '); WriteString(AContentType); WriteString(#13#10);
    end;
    WriteString(sContentLength); WriteString(': '); WriteString(LContentLength); WriteString(#13#10);
    if AHeaders <> nil then
      for var I := low(AHeaders) to high(AHeaders) do
        if AHeaders[I].Value <> '' then begin
          WriteString(AHeaders[I].Name); WriteString(': '); WriteString(AHeaders[I].Value); WriteString(#13#10);
        end;
    WriteString(#13#10);
    WriteString(AText);
    WriteString(#13#10);
  finally
    EndPart;
  end;

end;

{***********************************************}
procedure TALMultipartFormDataEncoderA.AddStream(
            const AFieldName: AnsiString;
            const AStream: TStream;
            const AOwnsStream: Boolean;
            const AFileName: AnsiString = '';
            const AContentType: AnsiString = '';
            const AHeaders: TALNameValueArrayA = nil);
begin

  AdjustLastBoundary;

  Var LContentLength: AnsiString := ALInttostrA(AStream.Size);

  var LContentType := AContentType;
  if LContentType = '' then LContentType := ALGetDefaultMIMEContentTypeFromExt(ALExtractFileExt(AFileName));

  var LSize: int64 := 0;
  LSize := LSize + 2{--} + Length(FBoundary) + 2{#13#10};
  LSize := LSize + Length(sContentDisposition) + 19{: form-data; name="} + Length(AFieldName) + Length('"');
  if AFileName <> '' then begin
    LSize := LSize + 12{; filename="} + Length(AFileName) + 1{"};
  end;
  LSize := LSize + 2{#13#10};
  if LContentType <> '' then begin
    LSize := LSize + Length(sContentType) + Length(': ') + Length(LContentType) + 2{#13#10};
  end;
  LSize := LSize + Length(sContentLength) + 2{: } + Length(LContentLength) + 2{#13#10};
  if AHeaders <> nil then
    for var I := low(AHeaders) to high(AHeaders) do
      if AHeaders[I].Value <> '' then begin
        LSize := LSize + Length(AHeaders[I].Name) + 2{: } + Length(AHeaders[I].Value) + 2{#13#10};
      end;
  LSize := LSize + 2{#13#10};

  BeginPart(Lsize);
  try
    WriteString('--'); WriteString(FBoundary); WriteString(#13#10);
    WriteString(sContentDisposition); WriteString(': form-data; name="'); WriteString(AFieldName); WriteString('"');
    if AFileName <> '' then begin
      WriteString('; filename="'); WriteString(AFileName); WriteString('"');
    end;
    WriteString(#13#10);
    if LContentType <> '' then begin
      WriteString(sContentType); WriteString(': '); WriteString(LContentType); WriteString(#13#10);
    end;
    WriteString(sContentLength); WriteString(': '); WriteString(LContentLength); WriteString(#13#10);
    if AHeaders <> nil then
      for var I := low(AHeaders) to high(AHeaders) do
        if AHeaders[I].Value <> '' then begin
          WriteString(AHeaders[I].Name); WriteString(': '); WriteString(AHeaders[I].Value); WriteString(#13#10);
        end;
    WriteString(#13#10);
  finally
    EndPart;
  end;

  FStream.AddStream(AStream, AOwnsStream);

  BeginPart(2{#13#10});
  try
    WriteString(#13#10);
  finally
    EndPart;
  end;

end;

{*********************************************}
procedure TALMultipartFormDataEncoderA.AddFile(
            const AFieldName: AnsiString;
            const AFilePath: String;
            const AContentType: AnsiString = '';
            const AHeaders: TALNameValueArrayA = nil);
begin
  var LFileStream := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyWrite);
  AddStream(AFieldName, LFileStream, True, AnsiString(ALExtractFileName(AFilePath)), AContentType, AHeaders);
end;

{**********************************************}
procedure TALMultipartFormDataEncoderA.AddBytes(
            const AFieldName: AnsiString;
            const ABytes: TBytes;
            const AFileName: AnsiString = '';
            const AContentType: AnsiString = '';
            const AHeaders: TALNameValueArrayA = nil);
begin
  var LBytesStream := TBytesStream.Create(ABytes);
  AddStream(AFieldName, LBytesStream, True, AFileName, AContentType, AHeaders);
end;

{*********************************************************************}
function TALMultipartAlternativeEncoderA.GetMimeTypeHeader: AnsiString;
begin
  Result := 'multipart/alternative; boundary=' + Boundary; // do not localize
end;

{************************************************}
procedure TALMultipartAlternativeEncoderA.AddText(
            const AText: AnsiString;
            const AContentType: AnsiString; // e.g. 'text/plain; charset=utf-8' or 'text/html; charset=utf-8'
            const AHeaders: TALNameValueArrayA = nil);
begin

  AdjustLastBoundary;

  var LText := AText;
  var LContentTransferEncodingInHeaders: Boolean := False;
  for var I := Low(AHeaders) to High(AHeaders) do
    if AlSameTextA(AHeaders[I].Name, 'Content-Transfer-Encoding') then begin
      LContentTransferEncodingInHeaders := True;
      Break;
    end;

  if not LContentTransferEncodingInHeaders then
    LText := ALBase64EncodeStringMIME(AText);

  var LSize: int64 := 0;
  LSize := LSize + 2{--} + Length(FBoundary) + 2{#13#10};
  if AContentType <> '' then begin
    LSize := LSize + Length(sContentType) + 2{: } + Length(AContentType) + 2{#13#10};
  end;
  if not LContentTransferEncodingInHeaders then begin
    LSize := LSize + 33{Content-Transfer-Encoding: base64} + 2{#13#10};
  end;
  if AHeaders <> nil then
    for var I := low(AHeaders) to high(AHeaders) do
      if AHeaders[I].Value <> '' then begin
        LSize := LSize + Length(AHeaders[I].Name) + 2{: } + Length(AHeaders[I].Value) + 2{#13#10};
      end;
  LSize := LSize + 2{#13#10};
  LSize := LSize + Length(LText);
  LSize := LSize + 2{#13#10};

  BeginPart(LSize);
  try
    WriteString('--'); WriteString(FBoundary); WriteString(#13#10);
    if AContentType <> '' then begin
      WriteString(sContentType); WriteString(': '); WriteString(AContentType); WriteString(#13#10);
    end;
    if not LContentTransferEncodingInHeaders then begin
      WriteString('Content-Transfer-Encoding: base64'); WriteString(#13#10);
    end;
    if AHeaders <> nil then
      for var I := low(AHeaders) to high(AHeaders) do
        if AHeaders[I].Value <> '' then begin
          WriteString(AHeaders[I].Name); WriteString(': '); WriteString(AHeaders[I].Value); WriteString(#13#10);
        end;
    WriteString(#13#10);
    WriteString(LText);
    WriteString(#13#10);
  finally
    EndPart;
  end;

end;

{***************************************************************}
function TALMultipartMixedEncoderA.GetMimeTypeHeader: AnsiString;
begin
  Result := 'multipart/mixed; boundary=' + Boundary; // do not localize
end;

{******************************************}
procedure TALMultipartMixedEncoderA.AddText(
            const AText: AnsiString;
            const AContentType: AnsiString;
            const AHeaders: TALNameValueArrayA = nil);
begin

  AdjustLastBoundary;

  var LText := AText;
  var LContentTransferEncodingInHeaders: Boolean := False;
  for var I := Low(AHeaders) to High(AHeaders) do
    if AlSameTextA(AHeaders[I].Name, 'Content-Transfer-Encoding') then begin
      LContentTransferEncodingInHeaders := True;
      Break;
    end;

  if not LContentTransferEncodingInHeaders then
    LText := ALBase64EncodeStringMIME(AText);

  var LSize: int64 := 0;
  LSize := LSize + 2{--} + Length(FBoundary) + 2{#13#10};
  if AContentType <> '' then begin
    LSize := LSize + Length(sContentType) + 2{: } + Length(AContentType) + 2{#13#10};
  end;
  if not LContentTransferEncodingInHeaders then begin
    LSize := LSize + 33{Content-Transfer-Encoding: base64} + 2{#13#10};
  end;
  if AHeaders <> nil then
    for var I := low(AHeaders) to high(AHeaders) do
      if AHeaders[I].Value <> '' then begin
        LSize := LSize + Length(AHeaders[I].Name) + 2{: } + Length(AHeaders[I].Value) + 2{#13#10};
      end;
  LSize := LSize + 2{#13#10};
  LSize := LSize + Length(LText);
  LSize := LSize + 2{#13#10};

  BeginPart(LSize);
  try
    WriteString('--'); WriteString(FBoundary); WriteString(#13#10);
    if AContentType <> '' then begin
      WriteString(sContentType); WriteString(': '); WriteString(AContentType); WriteString(#13#10);
    end;
    if not LContentTransferEncodingInHeaders then begin
      WriteString('Content-Transfer-Encoding: base64'); WriteString(#13#10);
    end;
    if AHeaders <> nil then
      for var I := low(AHeaders) to high(AHeaders) do
        if AHeaders[I].Value <> '' then begin
          WriteString(AHeaders[I].Name); WriteString(': '); WriteString(AHeaders[I].Value); WriteString(#13#10);
        end;
    WriteString(#13#10);
    WriteString(LText);
    WriteString(#13#10);
  finally
    EndPart;
  end;

end;

{********************************************}
procedure TALMultipartMixedEncoderA.AddStream(
            const AStream: TStream;
            const AFileName: AnsiString;
            const AContentType: AnsiString = '';
            const AHeaders: TALNameValueArrayA = nil);
begin

  var LContentType := AContentType;
  if LContentType = '' then LContentType := ALGetDefaultMIMEContentTypeFromExt(ALExtractFileExt(AFileName));

  var LHeaders := AHeaders;
  var LContentDispositionInHeaders: Boolean := False;
  for var I := Low(LHeaders) to High(LHeaders) do
    if AlSameTextA(LHeaders[I].Name, sContentDisposition) then begin
      LContentDispositionInHeaders := True;
      Break;
    end;

  if not LContentDispositionInHeaders then begin
    setlength(LHeaders, length(LHeaders) + 1);
    if AFileName <> '' then
      LHeaders[high(LHeaders)] := TALNameValuePairA.Create(sContentDisposition, 'attachment; filename="'+AFileName+'"')
    else
      LHeaders[high(LHeaders)] := TALNameValuePairA.Create(sContentDisposition, 'attachment')
  end;

  var LStringStream := TALStringStreamA.Create('');
  try
    AStream.Position := 0;
    LStringStream.CopyFrom(AStream, AStream.Size);
    AddText(
      LStringStream.DataString, // const AText: AnsiString;
      LContentType, // const AContentType: AnsiString;
      LHeaders); // const AHeaders: TALNameValueArrayA = nil)
  finally
    AlFreeAndNil(LStringStream);
  end;

end;

{******************************************}
procedure TALMultipartMixedEncoderA.AddFile(
            const AFilePath: String;
            const AContentType: AnsiString = '';
            const AHeaders: TALNameValueArrayA = nil);
begin
  var LFileStream := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyWrite);
  try
    AddStream(LFileStream, AnsiString(ALExtractFileName(AFilePath)), AContentType, AHeaders);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{*******************************************}
procedure TALMultipartMixedEncoderA.AddBytes(
            const ABytes: TBytes;
            const AFileName: AnsiString;
            const AContentType: AnsiString = '';
            const AHeaders: TALNameValueArrayA = nil);
begin
  var LBytesStream := TBytesStream.Create(ABytes);
  try
    AddStream(LBytesStream, AFileName, AContentType, AHeaders);
  finally
    ALFreeAndNil(LBytesStream);
  end;
end;

end.