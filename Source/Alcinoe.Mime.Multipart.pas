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
  {$IFNDEF ALCompilerVersionSupported130}
    {$MESSAGE WARN 'Check if System.Net.Mime.TMultipartFormData.pas is still the same and adjust the IFDEF'}
  {$ENDIF}
  TALMultipartFormDataEncoderA = class (TObject)
  private
    FOwnsOutputStream: Boolean;
    FBoundary: AnsiString;
    FStream: TProxyAggregateStream;
    FSection: TALStringStreamA;
    FLastBoundaryWritten: Boolean;
    function GetMimeTypeHeader: AnsiString;
    function GenerateBoundary: AnsiString;
    procedure WriteString(const AString: AnsiString);
    function GetStream: TStream;
    procedure AdjustLastBoundary;
    procedure BeginPart(const ASize: Int64);
    procedure EndPart;
  public
    /// <summary>Create a multipart form data object</summary>
    constructor Create(AOwnsOutputStream: Boolean = True);
    destructor Destroy; override;
    procedure SaveToFile(const AFileName: String);
    /// <summary>Add a form data field</summary>
    procedure AddField(
                const AFieldName: AnsiString;
                const AValue: AnsiString;
                const AContentType: AnsiString = '';
                const AHeaders: TALNameValueArrayA = nil);
    /// <summary>Add a form data stream. Allows to specify ownership of a stream</summary>
    procedure AddStream(
                const AFieldName: AnsiString;
                const AStream: TStream;
                const AOwnsStream: Boolean;
                const AFileName: AnsiString = '';
                const AContentType: AnsiString = '';
                const AHeaders: TALNameValueArrayA = nil); overload;
    /// <summary>Add a form data file</summary>
    procedure AddFile(
                const AFieldName: AnsiString;
                const AFilePath: String;
                const AContentType: AnsiString = '';
                const AHeaders: TALNameValueArrayA = nil);
    /// <summary>Add a form data bytes</summary>
    procedure AddBytes(
                const AFieldName: AnsiString;
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
  System.IOUtils,
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
  FLastBoundaryWritten := False;
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

{*************************************************************************}
procedure TALMultipartFormDataEncoderA.SaveToFile(const AFileName: String);
begin
  If TFile.Exists(AFilename) then
    TFile.Delete(AFileName);
  var LFileStream := TFileStream.Create(AFileName, fmCreate or fmShareExclusive);
  try
    LFileStream.CopyFrom(Stream);
  finally
    ALFreeAndNil(LFileStream);
  end;
end;

{*******************************************************************}
procedure TALMultipartFormDataEncoderA.BeginPart(const ASize: Int64);
begin
  FSection := TALStringStreamA.Create('');
  FSection.Size := ASize;
end;

{*********************************************}
procedure TALMultipartFormDataEncoderA.EndPart;
begin
  try
    if FSection.Size <> FSection.Position then
      FSection.Size := FSection.Position;
    FStream.AddStream(FSection, True);
  finally
    FSection := nil;
  end;
end;

{**********************************************}
procedure TALMultipartFormDataEncoderA.AddField(
            const AFieldName: AnsiString;
            const AValue: AnsiString;
            const AContentType: AnsiString = '';
            const AHeaders: TALNameValueArrayA = nil);
begin

  AdjustLastBoundary;

  Var LContentLength: AnsiString := ALInttostrA(length(AValue));

  var LSize: int64 := 0;
  LSize := LSize + 2{--} + Length(FBoundary) + 2{#13#10};
  LSize := LSize + Length(sContentDisposition) + 19{: form-data; name="} + Length(AFieldName) + 1{"} + 2{#13#10};
  if AContentType <> '' then begin
    LSize := LSize + Length(sContentType) + 2{: } + Length(AContentType) + 2{#13#10};
  end;
  LSize := LSize + Length(sContentLength) + 2{: } + Length(LContentLength) + 2{#13#10};
  if AHeaders <> nil then
    for var I := low(AHeaders) to high(AHeaders) do begin
      LSize := LSize + Length(AHeaders[I].Name) + 2{: } + Length(AHeaders[I].Value) + 2{#13#10};
    end;
  LSize := LSize + 2{#13#10};
  LSize := LSize + Length(AValue);
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
      for var I := low(AHeaders) to high(AHeaders) do begin
        WriteString(AHeaders[I].Name); WriteString(': '); WriteString(AHeaders[I].Value); WriteString(#13#10);
      end;
    WriteString(#13#10);
    WriteString(AValue);
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
    for var I := low(AHeaders) to high(AHeaders) do begin
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
      for var I := low(AHeaders) to high(AHeaders) do begin
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

{********************************************************}
procedure TALMultipartFormDataEncoderA.AdjustLastBoundary;
begin
  if FLastBoundaryWritten then
  begin
    FStream.RemoveStream(FStream.Count - 1);
    FLastBoundaryWritten := False;
  end;
end;

{******************************************************************}
function TALMultipartFormDataEncoderA.GetMimeTypeHeader: AnsiString;
begin
  Result := 'multipart/form-data; boundary=' + FBoundary; // do not localize
end;

{*******************************************************}
function TALMultipartFormDataEncoderA.GetStream: TStream;
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
end;

{****************************************************************************}
procedure TALMultipartFormDataEncoderA.WriteString(const AString: AnsiString);
begin
  FSection.WriteString(AString);
end;

{*****************************************************************}
function TALMultipartFormDataEncoderA.GenerateBoundary: AnsiString;
begin
  Result := '----------------------' + ALNewGUIDStringA(True{WithoutBracket}, true{WithoutHyphen});
end;

end.