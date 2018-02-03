{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclRTF.pas.                                             }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2000 of these individuals.                                                   }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ Unit owner: Marcel van Brakel                                                }
{ Last modified: January 30, 2001                                              }
{                                                                              }
{******************************************************************************}

unit JclRTF;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  Windows, Classes, Graphics, SysUtils;

type
  TJclRTFProducer = class (TObject)
  private
    FCodePage: Word;
    FColorTable: array of TColor;
    FFont: TFont;
    FFontChanged: Boolean;
    FFontCharsetTable: array of TFontCharset;
    FFontTable: array of string;
    FRTFStream: TStream;
    FText: string;
    function GetRTFStream: TStream;
    function GetRTFText: string;
    procedure FontChanged(Sender: TObject);
    function RTFHeader: string;
    procedure SetFont(const Value: TFont);
  protected
    function FindColor(Color: TColor): Integer;
    function FindFontNameCharset(const FontName: string; Charset: TFontCharset): Integer;
    procedure WriteChanges;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear(AllocSize: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 0 {$ENDIF});
    procedure SaveToFile(const FileName: TFileName);
    procedure SaveToStream(S: TStream);
    procedure WriteLn;
    procedure WriteParagraph(Alignment: TAlignment {$IFDEF SUPPORTS_DEFAULTPARAMS} = taLeftJustify {$ENDIF});
    procedure WriteRaw(const RawText: string);
    procedure WriteText(const Text: string);
    property CodePage: Word read FCodePage write FCodePage;
    property Font: TFont read FFont write SetFont;
    property RTFText: string read GetRTFText;
    property RTFStream: TStream read GetRTFStream;
  end;

function JclRTFToPlainText(RtfStream: TCustomMemoryStream; TextStream: TStream; var CodePage: Word): Boolean; overload;
function JclRTFToPlainText(const RtfFileName, TextFileName: TFileName; var CodePage: Word): Boolean; overload;
function JclRTFToPlainText(RtfStream: TCustomMemoryStream; TextStream: TStream): Boolean; overload;
function JclRTFToPlainText(const RtfFileName, TextFileName: TFileName): Boolean; overload;

implementation

uses
  Consts,
  JclBase, JclFileUtils, JclLogic, JclStrings, JclSysUtils;

//==============================================================================
// TJclRTFProducer
//==============================================================================

procedure TJclRTFProducer.Clear(AllocSize: Integer);
begin
  FCodePage := GetACP;
  FFontChanged := True;
  SetLength(FText, AllocSize);
  FText := '';
  FColorTable := nil;
  FFontTable := nil;
  FFontCharsetTable := nil;
  FreeAndNil(FRTFStream);
end;

//------------------------------------------------------------------------------

constructor TJclRTFProducer.Create;
begin
  inherited Create;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  Clear(0);
end;

//------------------------------------------------------------------------------

destructor TJclRTFProducer.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FRTFStream);
  inherited;
end;

//------------------------------------------------------------------------------

function TJclRTFProducer.FindColor(Color: TColor): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(FColorTable) to High(FColorTable) do
    if Color = FColorTable[I] then
    begin
      Result := I;
      Break;
    end;
  if Result = -1 then
  begin
    Result := Length(FColorTable);
    SetLength(FColorTable, Result + 1);
    FColorTable[Result] := Color;
  end;
end;

//------------------------------------------------------------------------------

function TJclRTFProducer.FindFontNameCharset(const FontName: string; Charset: TFontCharset): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(FFontTable) to High(FFontTable) do
    if StrSame(FFontTable[I], FontName) and (FFontCharsetTable[I] = Charset) then
    begin
      Result := I;
      Break;
    end;
  if Result = -1 then
  begin
    Result := Length(FFontTable);
    SetLength(FFontTable, Result + 1);
    SetLength(FFontCharsetTable, Result + 1);
    FFontTable[Result] := FontName;
    FFontCharsetTable[Result] := Charset;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclRTFProducer.FontChanged(Sender: TObject);
begin
  FFontChanged := True;
end;

//------------------------------------------------------------------------------

function TJclRTFProducer.GetRTFStream: TStream;
begin
  FreeAndNil(FRTFStream);
  FRTFStream := TStringStream.Create(RTFText);
  Result := FRTFStream;
end;

//------------------------------------------------------------------------------

function TJclRTFProducer.GetRTFText: string;
begin
  Result := RTFHeader + FText + '\par }';
end;

//------------------------------------------------------------------------------

function TJclRTFProducer.RTFHeader: string;
var
  I: Integer;

  function FontString(Index: Integer): string;
  begin
    Result := Format('{\f%d\fcharset%d %s;}', [Index, FFontCharsetTable[Index], FFontTable[Index]]);
  end;

  function ColorString(Index: Integer): string;
  var
    Color: TColorRef;
  begin
    Color := ColorToRGB(FColorTable[Index]);
    Result := Format('\red%d\green%d\blue%d;', [GetRValue(Color), GetGValue(Color), GetBValue(Color)]);
  end;

begin
  Result := Format('{\rtf1\ansi\ansicpg%d\deff0\deftab720', [FCodePage]);
  Result := Result + '{\fonttbl';
  for I := 0 to Length(FFontTable) - 1 do
    Result := Result + FontString(I);
  Result := Result + '}';
  Result := Result + '{\colortbl';
  for I := 0 to Length(FColorTable) - 1 do
    Result := Result + ColorString(I);
  Result := Result + '}';
end;

//------------------------------------------------------------------------------

procedure TJclRTFProducer.SaveToFile(const FileName: TFileName);
var
  S: TFileStream;
begin
  S := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclRTFProducer.SaveToStream(S: TStream);
var
  LocalText: string;
begin
  LocalText := RTFText;
  S.WriteBuffer(Pointer(LocalText)^, Length(LocalText) + 1);
end;

//------------------------------------------------------------------------------

procedure TJclRTFProducer.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TJclRTFProducer.WriteChanges;
const
  FontStyleStrs: array [TFontStyle] of PChar = ('\b', '\i', '\ul', '\strike');
var
  ColorIndex, FontIndex: Integer;
  S: string;
  FS: TFontStyle;
begin
  if FFontChanged then
  begin
    ColorIndex := FindColor(FFont.Color);
    FontIndex := FindFontNameCharset(FFont.Name, FFont.Charset);
    S := Format('\plain\f%d\fs%d\cf%d', [FontIndex, FFont.Size * 2, ColorIndex]);
    for FS := Low(FS) to High(FS) do
      if FS in FFont.Style then
        S := S + FontStyleStrs[FS];
    S := S + ' ';
    FText := FText + S;
    FFontChanged := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclRTFProducer.WriteLn;
begin
  WriteText(AnsiLineFeed);
end;

//------------------------------------------------------------------------------

procedure TJclRTFProducer.WriteParagraph(Alignment: TAlignment);
var
  S: string;
begin
  S := '\pard';
  case Alignment of
    taLeftJustify:
      S := S + ' ';
    taRightJustify:
      S := S + '\qr ';
    taCenter:
      S := S + '\qc ';
  end;
  FText := FText + S;
end;

//------------------------------------------------------------------------------

procedure TJclRTFProducer.WriteRaw(const RawText: string);
begin
  FText := FText + RawText;
end;

//------------------------------------------------------------------------------

procedure TJclRTFProducer.WriteText(const Text: string);
var
  I: Integer;
  S: string;
  C: Char;
begin
  SetLength(S, Length(Text));
  S := '';
  WriteChanges;
  for I := 1 to Length(Text) do
  begin
    C := Text[I];
    case C of
      AnsiLineFeed:
        S := S + '\par ' + AnsiCrLf;
      #32..#127:
        case C of
          '{', '}', '\':
            S := '\' + C;
        else
          S := S + C;
        end;
      #128..#255:
        S := S + LowerCase(Format('\''%.2x', [Byte(C)]));
    end;
  end;
  FText := FText + S;
end;

//==============================================================================
// RTF to Plain text conversion
//==============================================================================

function JclRTFToPlainText(RtfStream: TCustomMemoryStream; TextStream: TStream; var CodePage: Word): Boolean;
var
  P, EndText, StartText, StartParamText: PChar;
  BracketLevel, HexCharsNeeded, HexValue: Integer;
  SlashParam: Boolean;
  C: Char;

  procedure ProcessParam;
  var
    CodePageNum: array[0..4] of Char;
  begin
    if StartParamText <> nil then
    begin
      if StrLComp(StartParamText, '\par', P - StartParamText) = 0 then
        TextStream.WriteBuffer(AnsiCrLf, Length(AnsiCrLf));
      if (CodePage = 0) and (StrLComp('\ansicpg', StartParamText, 8) = 0) then
      begin
        Inc(StartParamText, 8);
        StrLCopy(CodePageNum, StartParamText, Min(P - StartParamText, 4));
        CodePage := StrToIntDef(CodePageNum, 0);
      end;
    end;
    StartParamText := nil;
  end;

  procedure MarkTextStart;
  begin
    if (StartText = nil) and (BracketLevel = 1) then
      StartText := P;
  end;

  procedure WriteChar(C: Char);
  begin
    TextStream.WriteBuffer(C, 1);
  end;

  procedure WriteText;
  begin
    if StartText <> nil then
      TextStream.WriteBuffer(StartText^, P - StartText);
    StartText := nil;
  end;

begin
  P := RtfStream.Memory;
  Result := (P <> nil) or (P^ <> '{');
  CodePage := 0;
  if not Result then
    Exit;
  StartText := nil;
  StartParamText := nil;
  EndText := P + RtfStream.Size;
  Inc(P);
  BracketLevel := 1;
  HexCharsNeeded := 0;
  SlashParam := False;
  HexValue := 0;
  while P < EndText do
  begin
    C := P^;
    if HexCharsNeeded > 0 then
    begin
      C := UpCase(C);
      if not (C in AnsiHexDigits) then
      begin
        Result := False;
        Break;
      end;
      HexValue := HexValue shl 4;
      case C of
        '0'..'9':
          Inc(HexValue, Ord(C) - Ord('0'));
        'A'..'F':
          Inc(HexValue, Ord(C) - Ord('A') + 10);
      end;
      Dec(HexCharsNeeded);
      if HexCharsNeeded = 0 then
        WriteChar(Chr(HexValue));
    end
    else
      case C of
        '{':
          begin
            if SlashParam then
            begin
              WriteChar(C);
              StartParamText := nil;
            end
            else
            begin
              ProcessParam;
              Inc(BracketLevel);
            end;
            SlashParam := False;
          end;
        '}':
          begin
            if SlashParam then
            begin
              WriteChar(C);
              StartParamText := nil;
            end
            else
            begin
              ProcessParam;
              Dec(BracketLevel);
              if BracketLevel = 0 then
                Break;
            end;
            SlashParam := False;
          end;
        '\':
          begin
            if SlashParam then
            begin
              WriteChar(C);
              SlashParam := False;
              StartParamText := nil;
            end
            else
            begin
              WriteText;
              ProcessParam;
              StartParamText := P;
              SlashParam := True;
            end;
          end;
        AnsiCarriageReturn, AnsiLineFeed:
          begin
            WriteText;
            ProcessParam;
            SlashParam := False;
          end;
        AnsiSpace:
          begin
            if StartParamText <> nil then
              ProcessParam
            else
              MarkTextStart;
            SlashParam := False;
          end;
        AnsiSingleQuote:
          begin
            if SlashParam then
            begin
              HexCharsNeeded := 2;
              HexValue := 0;
            end;
            SlashParam := False;
          end;
      else
        if StartParamText = nil then
          MarkTextStart;
        SlashParam := False;
      end;
    Inc(P);
  end;
end;

//------------------------------------------------------------------------------

function JclRTFToPlainText(const RtfFileName, TextFileName: TFileName; var CodePage: Word): Boolean;
var
  S: TJclFileMappingStream;
  D: TFileStream;
begin
  S := TJclFileMappingStream.Create(RtfFileName, fmOpenRead or fmShareDenyWrite);
  try
    D := TFileStream.Create(TextFileName, fmCreate);
    try
      Result := JclRTFToPlainText(S, D, CodePage);
    finally
      D.Free;
    end;
  finally
    S.Free;
  end;
end;

//------------------------------------------------------------------------------

function JclRTFToPlainText(RtfStream: TCustomMemoryStream; TextStream: TStream): Boolean;
var
  C: Word;
begin
  Result := JclRTFToPlainText(RtfStream, TextStream, C);
end;

//------------------------------------------------------------------------------

function JclRTFToPlainText(const RtfFileName, TextFileName: TFileName): Boolean;
var
  C: Word;
begin
  Result := JclRTFToPlainText(RtfFileName, TextFileName, C);
end;

end.

