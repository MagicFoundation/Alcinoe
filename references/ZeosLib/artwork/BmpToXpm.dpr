program BmpToXpm;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
   SysUtils, Graphics, Classes;

procedure Convert(const BmpFile, XpmFile: string);
const
  Max = 64;
var
  Bitmap: TBitmap;
  OutFile: TextFile;
  Palette: TList;
  Height, Width: Integer;
  Image: PInteger;
  pInt: PInteger;
  X, Y: Integer;
  Color: TColor;
  pCol: PColor;
  Found: Boolean;
  Count: Integer;
  Bytes: Byte;
  Chars: array[0..91] of Char;
  Transparency_Index: Integer;
begin
  Transparency_Index := -1;
  StrPCopy(Chars, ' .+@#$%&*=-;>,'')!~{]^/(_:<[}|1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ`');
  Bitmap := TBitmap.Create;
  Palette := TList.Create;
  Bitmap.LoadFromFile(BmpFile);
  if Bitmap.Height > 180 then
    Height := 180
  else
    Height := Bitmap.Height;
  if Bitmap.Width > 180 then
    Width := 180
  else
    Width := Bitmap.Width;
  GetMem(Image, SizeOf(Integer) * Height * Width);
  pInt := @Image^;
  for Y := 0 to Pred(Height) do
  begin
    for X := 0 to Pred(Width)  do
    begin
      Color := Bitmap.Canvas.Pixels[X, Y];
      Found := False;
      for Count := 0 to Pred(Palette.Count) do
      begin
        Found := Color = TColor(Palette.Items[Count]^);
        if Found then
          Break;
      end;
      if not Found then
      begin
        New(pCol);
        pCol^ := Color;
        Palette.Add(pCol);
        pInt^ := Pred(Palette.Count);
      end
      else
        pInt^ := Count;
      Inc(pInt);
    end;
  end;

  for Count := 0 to Pred(Palette.Count) do
  begin
    if Bitmap.Canvas.Pixels[0, Pred(Height)] = TColor(Palette.Items[Count]^) then
      Transparency_Index := Count;
  end;

  FreeAndNil(Bitmap);

  AssignFile(OutFile, XpmFile);
  Rewrite(OutFile);
  WriteLn(OutFile, '/* XPM */');
  WriteLn(OutFile, Format('static char * %s_xpm[] = {', [XpmFile]));
  if Palette.Count > Max then
    Bytes := 2
  else
    Bytes := 1;
  WriteLn(OutFile, Format('"%d %d %d %d",', [Width, Height, Palette.Count, Bytes]));
  for Count := 0 to Pred(Palette.Count) do
  begin
    Write(OutFile, Format('"%s', [Chars[Count mod 92]]));
    if Bytes > 1 then
      Write(OutFile, Format('%s', [Chars[Count div 92]]));
    Color := TColor(Palette.Items[Count]^) mod (256 * 256 * 256);
    if Transparency_Index = Count then
      WriteLn(OutFile, #9+'c None",')
    else
      WriteLn(OutFile, Format(#9+'c #%s%s%s",', [
        IntToHex(Color mod 256, 2),
        IntToHex((Color div 256) mod 256, 2),
        IntToHex(Color div (256 * 256), 2)]));
  end;
  pInt := @Image^;
  for Y := 0 to Pred(Height) do
  begin
    Write(OutFile, '"');
    for X := 0 to Pred(Width) do
    begin
      Write(OutFile, Chars[pInt^ mod 92]);
      if Bytes > 1 then
        Write(OutFile, Chars[pInt^ div 92]);
      Inc(pInt);
    end;
    if Y = Pred(Height) then
      WriteLn(OutFile, '"};')
    else
      WriteLn(OutFile, '",');
  end;

  CloseFile(OutFile);
  FreeAndNil(Palette);

end;

var
  FileName: string;
begin
  if ParamCount < 2 then
    FileName := ChangeFileExt(ParamStr(1), '.xpm')
  else
    FileName := ParamStr(2);
  Convert(ParamStr(1), FileName);
end.

