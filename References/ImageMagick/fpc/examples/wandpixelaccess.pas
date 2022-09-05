{
  Demonstration program for the ImageMagick Library

  Usage: Just execute the program. It will change all black pixels
  in the image.png image on it's directory to be transparent
  and then it will save it as image2.png
  The idea is to demonstrate pixel access using MagickWand.
}
program wandpixelaccess;

{$mode objfpc}{$H+}

uses SysUtils, magick_wand, ImageMagick, ctypes;

procedure ThrowWandException(wand: PMagickWand);
var
  description: PChar;
  severity: ExceptionType;
begin
  description := MagickGetException(wand, @severity);
  WriteLn(Format('An error ocurred. Description: %s', [description]));
  description := MagickRelinquishMemory(description);
  Abort;
end;

var
  status: MagickBooleanType;
  wand: PMagickWand = nil;
  pixel: MagickPixelPacket;
  iterator: PPixelIterator;
  pixels: PPPixelWand = nil;
  x, y: Integer;
  width: culong;
begin
  { Read an image. }

  MagickWandGenesis;

  wand := NewMagickWand();

  try
    status := MagickReadImage(wand, 'image.png');
    if (status = MagickFalse) then ThrowWandException(wand);

    iterator := NewPixelIterator(wand);
    if (iterator = nil) then ThrowWandException(wand);

    for y := 0 to MagickGetImageHeight(wand) - 1 do
    begin
//      WriteLn(' Line ', y, ' from ', MagickGetImageHeight(wand) - 1);
      pixels := PixelGetNextIteratorRow(iterator, width);
      if (pixels = nil) then Break;

      for x := 0 to width - 1 do
      begin
//        WriteLn(Format(' x %d y %d r %f g %f b %f o %f',
//         [x, y, pixel.red, pixel.green, pixel.blue, pixel.opacity]));
        PixelGetMagickColor(pixels[x], @pixel);
        if (pixel.red = 0.0) and
           (pixel.green = 0.0) and
           (pixel.blue = 0.0) then
        begin
          pixel.opacity := QuantumRange;
          pixel.matte := QuantumRange; // matte=alpha
        end;
        PixelSetMagickColor(pixels[x], @pixel);
      end;
      PixelSyncIterator(iterator);
    end;

//    if y < MagickGetImageHeight(wand) then ThrowWandException(wand);
    iterator := DestroyPixelIterator(iterator);

    { Write the image }

    status := MagickWriteImage(wand, 'image2.png');
    if (status = MagickFalse) then ThrowWandException(wand);
  finally
    { Clean-up }
    if wand <> nil then wand := DestroyMagickWand(wand);
    MagickWandTerminus;
  end;
end.

