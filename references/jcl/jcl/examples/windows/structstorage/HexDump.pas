unit HexDump;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

const
  MAXDIGITS = 16;

{ THexDump }

type

  THexStr = array[0..2] of Char;
  THexStrArray = array[0..MAXDIGITS-1] of THexStr;

  THexDump = class(TCustomControl)
  private
    FActive: Boolean;
    FAddress: Pointer;
    FDataSize: Integer;
    FTopLine: Integer;
    FCurrentLine: Integer;
    FVisibleLines: Integer;
    FLineCount: Integer;
    FBytesPerLine: Integer;
    FItemHeight: Integer;
    FItemWidth: Integer;
    FFileColors: array[0..2] of TColor;
    FShowCharacters: Boolean;
    FShowAddress: Boolean;
    FBorder: TBorderStyle;
    FHexData: THexStrArray;
    FLineAddr: array[0..15] of char;
    FStream:TMemoryStream;

    procedure CalcPaintParams;
    procedure SetTopLine(Value: Integer);
    procedure SetCurrentLine(Value: Integer);
    procedure SetFileColor(Index: Integer; Value: TColor);
    function GetFileColor(Index: Integer): TColor;
    procedure SetShowCharacters(Value: Boolean);
    procedure SetShowAddress(Value: Boolean);
    procedure SetBorder(Value: TBorderStyle);
    procedure SetAddress(Value: Pointer);
    procedure SetDataSize(Value: Integer);
    procedure AdjustScrollBars;
    function LineAddr(Index: Integer): PChar;
    function LineData(Index: Integer): PChar;
    function LineChars(Index: Integer): PChar;
    function ScrollIntoView: Boolean;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMLostFocus); message CM_EXIT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CurrentLine: Integer read FCurrentLine write SetCurrentLine;
    procedure LoadFromStream(Stream:TStream);
    procedure Clear;
    property Address: Pointer read FAddress write SetAddress;
    property DataSize: Integer read FDataSize write SetDataSize;
  published
    property Align;

    property Border: TBorderStyle read FBorder write SetBorder;
    property Color default clWhite;
    property Ctl3D;
    property Font;
    property TabOrder;
    property TabStop;
    property ShowAddress: Boolean read FShowAddress write SetShowAddress default True;
    property ShowCharacters: Boolean read FShowCharacters write SetShowCharacters default True;
    property AddressColor: TColor index 0 read GetFileColor write SetFileColor default clBlack;
    property HexDataColor: TColor index 1 read GetFileColor write SetFileColor default clBlack;
    property AnsiCharColor: TColor index 2 read GetFileColor write SetFileColor default clBlack;
  end;

function CreateHexDump(AOwner: TWinControl): THexDump;

implementation

{ Form Methods }

function CreateHexDump(AOwner: TWinControl): THexDump;
begin
  Result := THexDump.Create(AOwner);
  with Result do
  begin
    Parent := AOwner;
    Font.Name := 'FixedSys';
    ShowCharacters := True;
    Align := alClient;
  end;
end;

{ THexDump }

constructor THexDump.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  ControlStyle := [csFramed];
  Ctl3D := true;
  FBorder := bsSingle;
  Color := clWhite;
  FShowAddress := True;
  FShowCharacters := True;
  Width := 300;
  Height := 200;
  FillChar(FHexData, SizeOf(FHexData), #9);
end;

destructor THexDump.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure THexDump.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    ExStyle := ExStyle and not WS_EX_CONTROLPARENT;
    if (FBorder = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    Style := Style or WS_VSCROLL;
  end;
end;

{ VCL Command Messages }

procedure THexDump.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font := Self.Font;
  FItemHeight := Canvas.TextHeight('A') + 2;
  FItemWidth := Canvas.TextWidth('D') + 1;
  CalcPaintParams;
  AdjustScrollBars;
end;

procedure THexDump.CMEnter;
begin
  inherited;
{  InvalidateLineMarker; }
end;

procedure THexDump.CMExit;
begin
  inherited;
{  InvalidateLineMarker; }
end;

{ Windows Messages }

procedure THexDump.WMSize(var Message: TWMSize);
begin
  inherited;
  CalcPaintParams;
  AdjustScrollBars;
end;

procedure THexDump.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure THexDump.WMVScroll(var Message: TWMVScroll);
var
  NewTopLine: Integer;
  LinesMoved: Integer;
  R: TRect;
begin
  inherited;
  NewTopLine := FTopLine;
  case Message.ScrollCode of
    SB_LINEDOWN: Inc(NewTopLine);
    SB_LINEUP: Dec(NewTopLine);
    SB_PAGEDOWN: Inc(NewTopLine, FVisibleLines - 1);
    SB_PAGEUP: Dec(NewTopLine, FVisibleLines - 1);
    SB_THUMBPOSITION, SB_THUMBTRACK: NewTopLine := Message.Pos;
  end;

  if NewTopLine < 0 then NewTopLine := 0;
  if NewTopLine >= FLineCount then
    NewTopLine := FLineCount - 1;

  if NewTopLine <> FTopLine then
  begin
    LinesMoved := FTopLine - NewTopLine;
    FTopLine := NewTopLine;
    SetScrollPos(Handle, SB_VERT, FTopLine, True);

    if Abs(LinesMoved) = 1 then
    begin
      R := Bounds(0, 0, ClientWidth, ClientHeight - FItemHeight);
      if LinesMoved = 1 then OffsetRect(R, 0, FItemHeight);

      ScrollWindow(Handle, 0, FItemHeight * LinesMoved, @R, nil);

      if LinesMoved = -1 then
      begin
        R.Top := ClientHeight - FItemHeight;
        R.Bottom := ClientHeight;
      end
      else
      begin
        R.Top := 0;
        R.Bottom := FItemHeight;
      end;

      Windows.InvalidateRect(Handle, @R, False);

    end
    else Invalidate;
  end;
end;

{ Painting Related }

procedure THexDump.CalcPaintParams;
const
  Divisor: array[boolean] of Integer = (3,4);
var
  CharsPerLine: Integer;

begin
  if FItemHeight < 1 then Exit;
  FVisibleLines := (ClientHeight div FItemHeight) + 1;
  CharsPerLine := ClientWidth div FItemWidth;
  if FShowAddress then Dec(CharsPerLine, 10);
  FBytesPerLine := CharsPerLine div Divisor[FShowCharacters];
  if FBytesPerLine < 1 then
    FBytesPerLine := 1
  else if FBytesPerLine > MAXDIGITS then
    FBytesPerLine := MAXDIGITS;
  FLineCount := (DataSize div FBytesPerLine);
  if Boolean(DataSize mod FBytesPerLine) then Inc(FLineCount);
end;

procedure THexDump.AdjustScrollBars;
begin
  SetScrollRange(Handle, SB_VERT, 0, FLineCount - 1, True);
end;

function THexDump.ScrollIntoView: Boolean;
begin
  Result := False;
  if FCurrentLine < FTopLine then
  begin
    Result := True;
    SetTopLine(FCurrentLine);
  end
  else if FCurrentLine >= (FTopLine + FVisibleLines) - 1 then
  begin
    SetTopLine(FCurrentLine - (FVisibleLines - 2));
    Result := True;
  end;
end;

procedure THexDump.SetTopLine(Value: Integer);
var
  LinesMoved: Integer;
  R: TRect;
begin
  if Value <> FTopLine then
  begin
    if Value < 0 then Value := 0;
    if Value >= FLineCount then Value := FLineCount - 1;

    LinesMoved := FTopLine - Value;
    FTopLine := Value;
    SetScrollPos(Handle, SB_VERT, FTopLine, True);

    if Abs(LinesMoved) = 1 then
    begin
      R := Bounds(1, 0, ClientWidth, ClientHeight - FItemHeight);
      if LinesMoved = 1 then OffsetRect(R, 0, FItemHeight);

      ScrollWindow(Handle, 0, FItemHeight * LinesMoved, @R, nil);

      if LinesMoved = -1 then
      begin
        R.Top := ClientHeight - FItemHeight;
        R.Bottom := ClientHeight;
      end
      else
      begin
        R.Top := 0;
        R.Bottom := FItemHeight;
      end;

      InvalidateRect(Handle, @R, False);

    end
    else Invalidate;
  end;
end;

procedure THexDump.SetCurrentLine(Value: Integer);
var
  R: TRect;
begin
  if Value <> FCurrentLine then
  begin
    if Value < 0 then Value := 0;
    if Value >= FLineCount then Value := FLineCount - 1;

    if (FCurrentLine >= FTopLine) and (FCurrentLine < FTopLine + FVisibleLines - 1) then
    begin
      R := Bounds(0, 0, 1, FItemHeight);
      OffsetRect(R, 0, (FCurrentLine - FTopLine) * FItemHeight);
      Windows.InvalidateRect(Handle, @R, True);
    end;
    FCurrentLine := Value;

    R := Bounds(0, 0, 1, FItemHeight);
    OffsetRect(R, 0, (FCurrentLine - FTopLine) * FItemHeight);
    Windows.InvalidateRect(Handle, @R, True);
    ScrollIntoView;
  end;
end;

procedure THexDump.Paint;
var
  R: TRect;
  I: Integer;
  AddressWidth: Integer;
  TabStop: Integer;
  ByteCnt: Integer;
begin
//  inherited Paint;
  Canvas.Brush.Color := Self.Color;
  Canvas.FillRect(ClientRect);
  if FShowAddress then
    AddressWidth := FItemWidth*10
  else
    AddressWidth := 0;
  R := Bounds(1, 0, ClientWidth, FItemHeight);
  TabStop := FItemWidth*3;
  Canvas.Font.Color := FFileColors[1];
  ByteCnt := FBytesPerLine;
  for I := 0 to FVisibleLines - 1 do
  begin
    R.Left := 1;
    if I + FTopLine < FLineCount then
    begin
      if FShowAddress then
      begin
        Canvas.Font.Color := FFileColors[0];
        R.Right := R.Left + AddressWidth;
        ExtTextOut(Canvas.Handle, R.Left, R.Top, ETO_OPAQUE or ETO_CLIPPED, @R, LineAddr(I+FTopLine), 9, nil);
        R.Left := R.Right;
        R.Right := ClientWidth;
        Canvas.Font.Color := FFileColors[1];
      end;
      if (I+FTopLine = FLineCount-1) and ((DataSize mod FBytesPerLine) > 0) then
        ByteCnt := DataSize mod FBytesPerLine;
      TabbedTextOut(Canvas.Handle, R.Left, R.Top, LineData(I+FTopLine),
        (ByteCnt*3)-1, 1, TabStop, R.Left);
      if FShowCharacters then
      begin
        R.Left := AddressWidth+(FItemWidth*(FBytesPerLine*3));
        Canvas.Font.Color := FFileColors[2];
        ExtTextOut(Canvas.Handle, R.Left, R.Top, ETO_OPAQUE or ETO_CLIPPED, @R, LineChars(I+FTopLine), ByteCnt, nil);
      end;
    end
    else ExtTextOut(Canvas.Handle, R.Left, R.Top, ETO_OPAQUE or ETO_CLIPPED,
      @R, nil, 0, nil);
    OffsetRect(R, 0, FItemHeight);
  end;
end;

{ Event Overrides }

procedure THexDump.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if not FActive then Exit;

  case Key of
    VK_DOWN: CurrentLine := CurrentLine + 1;
    VK_UP: CurrentLine := CurrentLine - 1;
    VK_NEXT: CurrentLine := CurrentLine + FVisibleLines;
    VK_PRIOR: CurrentLine := CurrentLine - FVisibleLines;
    VK_HOME: CurrentLine := 0;
    VK_END: CurrentLine := FLineCount - 1;
  end;
end;

procedure THexDump.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not Focused then SetFocus;
  if (Button = mbLeft) and FActive then
    CurrentLine := FTopLine + (Y div FItemHeight);
end;

{ Property Set/Get Routines }

procedure THexDump.SetBorder(Value: TBorderStyle);
begin
  if Value <> FBorder then
  begin
    FBorder := Value;
    RecreateWnd;
  end;
end;

procedure THexDump.SetShowAddress(Value: Boolean);
begin
  if FShowAddress <> Value then
  begin
    FShowAddress := Value;
    Invalidate;
  end;
end;

procedure THexDump.SetShowCharacters(Value: Boolean);
begin
  if Value <> FShowCharacters then
  begin
    FShowCharacters := Value;
    Invalidate;
  end;
end;

procedure THexDump.SetFileColor(Index: Integer; Value: TColor);
begin
  if FFileColors[Index] <> Value then
  begin
    FFileColors[Index] := Value;
    Invalidate;
  end;
end;

function THexDump.GetFileColor(Index: Integer): TColor;
begin
  Result := FFileColors[Index];
end;

procedure THexDump.SetAddress(Value: Pointer);
begin
  FActive := Value <> nil;
  FAddress := Value;
  Invalidate;
end;

procedure THexDump.SetDataSize(Value: Integer);
begin
  FDataSize := Value;
  CalcPaintParams;
  Invalidate;
  AdjustScrollBars;
end;

function THexDump.LineAddr(Index: Integer): PChar;
begin
  Result := StrFmt(FLineAddr, '%p:', [Pointer(PChar(Address)+Index*FBytesPerLine)]);
end;

function THexDump.LineData(Index: Integer): PChar;

  procedure SetData(P: PChar);
  const
    HexDigits : array[0..15] of Char = '0123456789ABCDEF';
  var
    I: Integer;
    B: Byte;
  begin
    for I := 0 to FBytesPerLine-1 do
    begin
      try 
        B := Byte(P[I]);
        FHexData[I][0] := HexDigits[B SHR $04];
        FHexData[I][1] := HexDigits[B AND $0F];
      except
        FHexData[I][0] := '?';
        FHexData[I][1] := '?';
      end;

    end;
  end;

begin
  SetData(PChar(FAddress) + Index*FBytesPerLine);
  Result := FHexData[0];
end;

function THexDump.LineChars(Index: Integer): PChar;
begin
  Result := PChar(FAddress) + Index*FBytesPerLine;
end;

procedure THexDump.LoadFromStream(Stream: TStream);
begin
  Clear;
  if Stream <> nil then
  begin
    FStream := TMemoryStream.Create;
    FStream.CopyFrom(Stream,0);
    Address := FStream.Memory;
    DataSize := FStream.Size;
  end;
end;

procedure THexDump.Clear;
begin
  if Parent <> nil then
  begin
    FVisibleLines := 0;
    SetTopLine(0);
    SetCurrentLine(0);
    Address := nil;
    DataSize := 0;
  end;
  FreeAndNil(FStream);
end;

end.
