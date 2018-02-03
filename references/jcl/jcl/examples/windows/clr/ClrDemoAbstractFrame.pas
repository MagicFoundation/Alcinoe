unit ClrDemoAbstractFrame;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JclCLR;

type
  TfrmAbstract = class(TFrame)
  public
    procedure ShowInfo(const ACLR: TJclCLRHeaderEx); virtual; abstract;

    class procedure DumpBuf(const Ptr: Pointer; const Size: Integer;
      const memDump: TMemo; const Base: DWORD = 0;
      const AutoClear: Boolean = True); overload;
    class procedure DumpBuf(const Blob: TJclCLRBlobRecord; const memDump: TMemo;
      const AutoClear: Boolean = False); overload;
  end;

implementation

{$R *.DFM}

uses
  JclStrings;

{ TfrmAbstract }

class procedure TfrmAbstract.DumpBuf(const Ptr: Pointer; const Size: Integer;
  const memDump: TMemo; const Base: DWORD; const AutoClear: Boolean);
const
  WIDE_LINE_WIDTH = 76;
  THIN_LINE_WIDTH = 44;
var
  I, ByteCount, LineWidth: Integer;
  pch: PChar;
  DumpStr: string;
begin
  if AutoClear then memDump.Clear;

  ByteCount := 0;
  pch   := Ptr;

  with TCanvas.Create do
  try
    Handle    := GetDC(memDump.Handle);
    Font.Name := 'Fixedsys';
    Font.Size := 12;
    if (TextWidth('?')*WIDE_LINE_WIDTH) < memDump.ClientWidth then
      LineWidth := 16
    else if (TextWidth('?')*THIN_LINE_WIDTH) < memDump.ClientWidth then
      LineWidth := 8
    else
      LineWidth := 4;
  finally
    Free;
  end;

  with memDump.Lines do
  try
    BeginUpdate;

    while ByteCount < Size do
    begin
      DumpStr := IntToHex(Base + DWord(ByteCount), 8) + ': ';
      for I:=0 to LineWidth-1 do
      begin
        if ((Size - ByteCount) > LineWidth) or ((Size - ByteCount) > I) then
          DumpStr := DumpStr + IntToHex(Integer(pch[ByteCount+I]), 2) + ' '
        else
          DumpStr := DumpStr + '   ';
      end;

      DumpStr := DumpStr + '; ';

      for I:=0 to LineWidth-1 do
      begin
        if ((Size - ByteCount) > LineWidth) or ((Size - ByteCount) > I) then
        begin
          if CharIsAlphaNum(Char(pch[ByteCount+I])) then
            DumpStr := DumpStr + pch[ByteCount+I]
          else
            DumpStr := DumpStr + '.'
        end
        else
          DumpStr := DumpStr + ' ';
      end;

      Add(DumpStr);
      Inc(ByteCount, LineWidth);
    end;
  finally
    EndUpdate;
  end;
  memDump.Perform(WM_VSCROLL, SB_TOP, 0);
end;

class procedure TfrmAbstract.DumpBuf(const Blob: TJclCLRBlobRecord;
  const memDump: TMemo; const AutoClear: Boolean);
begin
  TfrmAbstract.DumpBuf(Blob.Memory, Blob.Size, memDump, Blob.Offset, AutoClear);
end;

end.
