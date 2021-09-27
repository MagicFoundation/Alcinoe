{
  Demonstration program for the ImageMagick Library

  This program was converted from c by: Felipe Monteiro de Carvalho

  Usage: Just execute the program. It will resize the image.png image
  on it's directory to fit (106, 80) and convert it to a jpg.

  Dez/2005
}
program wanddemo;

{$mode objfpc}{$H+}

uses SysUtils, magick_wand, ImageMagick;

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
  wand: PMagickWand;

begin
  { Read an image. }

  MagickWandGenesis;

  wand := NewMagickWand;

  try
    status := MagickReadImage(wand, 'image.png');
    if (status = MagickFalse) then ThrowWandException(wand);

    { Turn the images into a thumbnail sequence. }

    MagickResetIterator(wand);

    while (MagickNextImage(wand) <> MagickFalse) do
     MagickResizeImage(wand, 106, 80, LanczosFilter, 1.0);

    { Write the image as MIFF and destroy it. }

    status := MagickWriteImages(wand, 'image.jpg', MagickTrue);
    if (status = MagickFalse) then ThrowWandException(wand);

  finally
    wand := DestroyMagickWand(wand);

    MagickWandTerminus;
  end;
end.
