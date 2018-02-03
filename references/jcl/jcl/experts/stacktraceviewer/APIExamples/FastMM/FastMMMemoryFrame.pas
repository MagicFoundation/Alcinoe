unit FastMMMemoryFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FastMMParser, Grids, StdCtrls, ExtCtrls,
  JclBase;

type
  TfrmMemory = class(TFrame)
    Panel2: TPanel;
    lbMemoryAddr: TLabel;
    Label6: TLabel;
    sgMemory: TStringGrid;
  private
    { Private-Deklarationen }
    FMemoryArray: TFastMMMemoryArray;
    procedure SetMemoryArray(const Value: TFastMMMemoryArray);
    procedure SetAddress(const Value: TJclAddr);
  public
    { Public-Deklarationen }
    property Address: TJclAddr write SetAddress;
    property MemoryArray: TFastMMMemoryArray write SetMemoryArray;
  end;

implementation

{$R *.dfm}

{ TfrmMemory }

procedure TfrmMemory.SetAddress(const Value: TJclAddr);
begin
  lbMemoryAddr.Caption := Format(HexDigitFmt, [Value]);
end;

procedure TfrmMemory.SetMemoryArray(const Value: TFastMMMemoryArray);
var
  I, J: Integer;
begin
  FMemoryArray := Value;
  for I := 0 to 7 do
    for J := 0 to 31 do
      sgMemory.Cells[J, I] := Format('%.2x', [FMemoryArray[I * 32 + J]]);
  for I := 0 to 7 do
    for J := 0 to 31 do
      sgMemory.Cells[J, I + 8] := string(AnsiChar(Chr(FMemoryArray[I * 32 + J])));
end;

end.
