unit FastMMMemoryVisualizerFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Math, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfrmMemoryVisualizer = class(TFrame)
    memString: TMemo;
    pnlTop: TPanel;
    Label4: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    lbCodePage: TLabel;
    Label3: TLabel;
    lbRefCount: TLabel;
    lbLength: TLabel;
    lbElemSize: TLabel;
  private
    { Private-Deklarationen }
    FMemory: Pointer;
    FMemorySize: Integer;
    FReportCompilerVersion: Double;
    FTypeStr: string;
  public
    { Public-Deklarationen }
    function Decode: Boolean;
    property Memory: Pointer read FMemory write FMemory;
    property MemorySize: Integer read FMemorySize write FMemorySize;
    property ReportCompilerVersion: Double read FReportCompilerVersion write FReportCompilerVersion;
    property TypeStr: string read FTypeStr write FTypeStr;
  end;

function IsVisualizable(const ATypeStr: string; AReportCompilerVersion: Double; AMemory: Pointer; AMemorySize: Integer): Boolean;

implementation

{$R *.dfm}

const
  cAnsiString = 'AnsiString';
  cUnicodeString = 'UnicodeString';

function GetStringHeaderLength(const ATypeStr: string; AReportCompilerVersion: Double): Integer;
begin
  if (ATypeStr = cAnsiString) or (ATypeStr = cUnicodeString) then
  begin
    if AReportCompilerVersion >= 20 then
      Result := 12
    else
      Result := 8;
  end
  else
    Result := -1;
end;

function IsVisualizable(const ATypeStr: string; AReportCompilerVersion: Double; AMemory: Pointer; AMemorySize: Integer): Boolean;
begin
  if (ATypeStr = cAnsiString) or (ATypeStr = cUnicodeString) then
    Result := AMemorySize >= GetStringHeaderLength(ATypeStr, AReportCompilerVersion)
  else
    Result := False;
end;

{ TfrmMemoryVisualizer }

type
  PAnsiStrRec = ^AnsiStrRec;
  AnsiStrRec = packed record
    refCnt: Longint;
    length: Longint;
  end;

  PUnicodeStrRec = ^UnicodeStrRec;
  UnicodeStrRec = packed record
    codePage: Word;
    elemSize: Word;
    refCnt: Longint;
    length: Longint;
  end;

function TfrmMemoryVisualizer.Decode: Boolean;
var
  StringHeaderLength, StartIntPtr, DecodableLength: Integer;
begin
  Result := IsVisualizable(FTypeStr, FReportCompilerVersion, FMemory, FMemorySize);
  if Result then
  begin
    StringHeaderLength := GetStringHeaderLength(FTypeStr, FReportCompilerVersion);
    StartIntPtr := Integer(FMemory) + StringHeaderLength;
    if (StringHeaderLength = 12) and (FMemorySize >= StringHeaderLength) then
    begin
      DecodableLength := Min(FMemorySize - StringHeaderLength, PUnicodeStrRec(FMemory)^.length);
      if FTypeStr = cUnicodeString then
        memString.Text := Copy(PWideChar(StartIntPtr), 1, DecodableLength)
      else
        memString.Text := string(Copy(PAnsiChar(StartIntPtr), 1, DecodableLength));
      with PUnicodeStrRec(FMemory)^ do
      begin
        lbCodePage.Caption := IntToStr(codePage);
        lbElemSize.Caption := IntToStr(elemSize);
        lbRefCount.Caption := IntToStr(refCnt);
        lbLength.Caption := IntToStr(length);
      end;
    end
    else
    if FMemorySize >= 8 then
    begin
      DecodableLength := Min(FMemorySize - StringHeaderLength, PAnsiStrRec(FMemory)^.length);
      memString.Text := string(Copy(PAnsiChar(StartIntPtr), 1, DecodableLength));
      lbCodePage.Caption := 'N/A';
      lbElemSize.Caption := 'N/A';
      with PAnsiStrRec(FMemory)^ do
      begin
        lbRefCount.Caption := IntToStr(refCnt);
        lbLength.Caption := IntToStr(length);
      end;
    end;
  end;
end;

end.
