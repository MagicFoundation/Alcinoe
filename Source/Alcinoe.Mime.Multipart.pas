unit Alcinoe.Mime.Multipart;

interface

{$I Alcinoe.inc}

Uses
  System.SysUtils,
  System.Classes,
  Alcinoe.Common,
  Alcinoe.StringUtils;

type

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
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if System.Net.Mime.TMultipartFormData.pas is still the same and adjust the IFDEF'}
  {$ENDIF}
  TALMultipartFormDataEncoderA = class (TObject)
  private
    FOwnsOutputStream: Boolean;
    FBoundary: AnsiString;
    FStream: TProxyAggregateStream;
    FSection: TALStringStreamA;
    FLastBoundaryWrited: Boolean;
    function GetMimeTypeHeader: AnsiString;
    function GenerateBoundary: AnsiString;
    procedure WriteStringLn(const AString: AnsiString);
    function GetStream: TStream;
    procedure AdjustLastBoundary;
    procedure BeginPart;
    procedure EndPart;
  public
    /// <summary>Create a multipart form data object</summary>
    constructor Create(AOwnsOutputStream: Boolean = True);
    destructor Destroy; override;
    /// <summary>Add a form data field</summary>
    procedure AddField(
                const AField, AValue: AnsiString;
                const AContentType: AnsiString = '';
                const AHeaders: TALNameValueArrayA = nil);
    /// <summary>Add a form data stream. Allows to specify ownership of a stream</summary>
    procedure AddStream(
                const AField: AnsiString;
                const AStream: TStream;
                const AOwnsStream: Boolean;
                const AFileName: AnsiString = '';
                const AContentType: AnsiString = '';
                const AHeaders: TALNameValueArrayA = nil); overload;
    /// <summary>Add a form data file</summary>
    procedure AddFile(
                const AField: AnsiString;
                const AFilePath: String;
                const AContentType: AnsiString = '';
                const AHeaders: TALNameValueArrayA = nil);
    /// <summary>Add a form data bytes</summary>
    procedure AddBytes(
                const AField: AnsiString;
                const ABytes: TBytes;
                const AFileName: AnsiString = '';
                const AContentType: AnsiString = '';
                const AHeaders: TALNameValueArrayA = nil);
    /// <summary>Returns a stream to be sent in http body</summary>
    property Stream: TStream read GetStream;
    /// <summary>Returns a mime type to be sent in http headers</summary>
    property MimeTypeHeader: AnsiString read GetMimeTypeHeader;
    /// <summary>Returns a form data boundary AnsiString</summary>
    property Boundary: AnsiString read FBoundary;
  end;

implementation

Uses
  System.AnsiStrings,
  System.NetConsts,
  ALcinoe.Cipher,
  Alcinoe.Mime.ContentTypes;

{**************************************************************************}
constructor TALMultipartFormDataEncoderA.Create(AOwnsOutputStream: Boolean);
begin
  inherited Create;
  FOwnsOutputStream := AOwnsOutputStream;
  FBoundary := GenerateBoundary;
  FStream := TProxyAggregateStream.Create;
  FSection := nil;
  FLastBoundaryWrited := False;
end;

{**********************************************}
destructor TALMultipartFormDataEncoderA.Destroy;
begin
  ALFreeAndNil(FSection);
  if FOwnsOutputStream then ALFreeAndNil(FStream)
  // Check that last boundary is written
  else GetStream;
  inherited;
end;

{**********************************************}
procedure TALMultipartFormDataEncoderA.BeginPart;
begin
  FSection := TALStringStreamA.Create('');
end;

{**********************************************}
procedure TALMultipartFormDataEncoderA.EndPart;
begin
  try
    FStream.AddStream(FSection, True);
  finally
    FSection := nil;
  end;
end;

{**********************************************}
procedure TALMultipartFormDataEncoderA.AddField(
            const AField, AValue: AnsiString;
            const AContentType: AnsiString = '';
            const AHeaders: TALNameValueArrayA = nil);
begin
  AdjustLastBoundary;
  BeginPart;
  try
    WriteStringLn('--' + FBoundary);
    WriteStringLn(sContentDisposition + ': form-data; name="' + AField + '"'); // do not localize
    if AContentType <> '' then
      WriteStringLn(sContentType + ': ' + AContentType);
    if AHeaders <> nil then
      for var I := low(AHeaders) to high(AHeaders) do
        WriteStringLn(AHeaders[I].Name + ': ' + AHeaders[I].Value);
    // We need 2 line breaks here
    WriteStringLn('');
    WriteStringLn(AValue);
  finally
    EndPart;
  end;
end;

{**********************************************}
procedure TALMultipartFormDataEncoderA.AddStream(
            const AField: AnsiString;
            const AStream: TStream;
            const AOwnsStream: Boolean;
            const AFileName: AnsiString = '';
            const AContentType: AnsiString = '';
            const AHeaders: TALNameValueArrayA = nil);
begin
  AdjustLastBoundary;
  BeginPart;
  try
    WriteStringLn('--' + FBoundary);
    var LLine: AnsiString := sContentDisposition + ': form-data; name="' + AField + '"'; // do not localize
    if AFileName <> '' then
      LLine := LLine + '; filename="' + AFileName + '"'; // do not localize
    WriteStringLn(LLine);
    var LType := AContentType;
    if LType = '' then LType := ALGetDefaultMIMEContentTypeFromExt(ALExtractFileExt(AFileName));
    WriteStringLn(sContentType + ': ' + LType);
    if AHeaders <> nil then
      for var I := low(AHeaders) to high(AHeaders) do
        WriteStringLn(AHeaders[I].Name + ': ' + AHeaders[I].Value);
    // We need 2 line breaks here
    WriteStringLn('');
  finally
    EndPart;
  end;
  FStream.AddStream(AStream, AOwnsStream);
  BeginPart;
  try
    WriteStringLn('');
  finally
    EndPart;
  end;
end;

{**********************************************}
procedure TALMultipartFormDataEncoderA.AddFile(
            const AField: AnsiString;
            const AFilePath: String;
            const AContentType: AnsiString = '';
            const AHeaders: TALNameValueArrayA = nil);
begin
  var LFileStream := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyWrite);
  AddStream(AField, LFileStream, True, AnsiString(ALExtractFileName(AFilePath)), AContentType, AHeaders);
end;

{**********************************************}
procedure TALMultipartFormDataEncoderA.AddBytes(
            const AField: AnsiString;
            const ABytes: TBytes;
            const AFileName: AnsiString = '';
            const AContentType: AnsiString = '';
            const AHeaders: TALNameValueArrayA = nil);
begin
  var LBytesStream := TBytesStream.Create(ABytes);
  AddStream(AField, LBytesStream, True, AFileName, AContentType, AHeaders);
end;

{**********************************************}
procedure TALMultipartFormDataEncoderA.AdjustLastBoundary;
begin
  if FLastBoundaryWrited then
  begin
    FStream.RemoveStream(FStream.Count - 1);
    FLastBoundaryWrited := False;
  end;
end;

{**********************************************}
function TALMultipartFormDataEncoderA.GetMimeTypeHeader: AnsiString;
begin
  Result := 'multipart/form-data; boundary=' + FBoundary; // do not localize
end;

{**********************************************}
function TALMultipartFormDataEncoderA.GetStream: TStream;
begin
  if not FLastBoundaryWrited then
  begin
    BeginPart;
    try
      WriteStringLn('--' + FBoundary + '--');
    finally
      EndPart;
    end;
    FLastBoundaryWrited := True;
  end;
  Result := FStream;
end;

{**********************************************}
procedure TALMultipartFormDataEncoderA.WriteStringLn(const AString: AnsiString);
begin
  FSection.WriteString(AString + #13#10);
end;

{**********************************************}
function TALMultipartFormDataEncoderA.GenerateBoundary: AnsiString;
begin
  Result := '----------------------' + ALIntToHexA(ALRandom64(ALMaxInt64), 16);
end;

end.
