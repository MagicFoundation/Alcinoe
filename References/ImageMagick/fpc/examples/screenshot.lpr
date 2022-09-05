{
  Demonstration software for image resizing and screenshot using PascalMagick

  Created by: Felipe Monteiro de Carvalho

  This software takes a screenshot of the screen and enlarges it using anti-aliasing
 or not depending on what the user select.

  Notes: This software uses X11 to take the screenshot so it will only work on UNIXes

  More information on this page:
  http://wiki.lazarus.freepascal.org/index.php/PascalMagick

  April/2006
}
program screenshot;

{$mode objfpc}{$H+}

uses SysUtils, magick_wand, ImageMagick, Unix;

type
  TCommand = (cmdQuit, cmdSample, cmdAntiAliase);

{
  Catches exceptions from ImageMagick
}
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

{
  Shows the main screen
}
function MainScreen: TCommand;
var
  i: Integer;
  Continuar: Boolean;
begin
  Continuar := False;

  WriteLn('=========================================================');
  WriteLn('         Welcome to PascalMagick demo software 2');
  WriteLn('=========================================================');

  while not Continuar do
  begin
    WriteLn('');
    WriteLn('The following commands are available:');
    WriteLn('   0 - Quit');
    WriteLn('   1 - Capture screenshot and resize it to 2024x1536');
    WriteLn('   2 - Same as #1 except that uses Anti-Aliasing');
    Write(': ');
    ReadLn(i);

    case i of
     0:
     begin
       Result := cmdQuit;
       Continuar := True;
     end;

     1:
     begin
       Result := cmdSample;
       Continuar := True;
     end;

     2:
     begin
       Result := cmdAntiAliase;
       Continuar := True;
     end;

    else
      WriteLn('Wrong Command!!');
    end;
  end;
end;

{
  Main procedure
}
var
  status: MagickBooleanType;
  wand: PMagickWand;
  TempDir, shellStr: string;
  Antes: TTimeStamp;
  Command: TCommand;
begin
  { Presentation screen and user options }

  Command := MainScreen;

  if Command = cmdQuit then Exit;


  { Create the image }

  Antes := DateTimeToTimeStamp(Now);

  TempDir := GetTempDir(False);

  shellStr := 'xwd -root -out ' + TempDir + 'display.xwd';

  WriteLn(shellStr);

  shell(shellStr);

  { Read an image. }

  MagickWandGenesis;

  wand := NewMagickWand;

  try
    status := MagickReadImage(wand, PChar(TempDir + 'display.xwd'));
    if (status = MagickFalse) then ThrowWandException(wand);

    { Enlarge the Image }

    WriteLn('Enlarging');

    if Command = cmdAntiAliase then MagickResizeImage(wand, 2024, 1536, BoxFilter, 1.0)
    else MagickSampleImage(wand, 2024, 1536);

    WriteLn(IntToStr(DateTimeToTimeStamp(Now).Time - Antes.Time));

    WriteLn('Saving');

    { Write the image as MIFF and destroy it. }

    status := MagickWriteImages(wand, PChar(TempDir + 'enlarged.jpg'), MagickTrue);
    if (status = MagickFalse) then ThrowWandException(wand);

    WriteLn(IntToStr(DateTimeToTimeStamp(Now).Time - Antes.Time));

  finally
    wand := DestroyMagickWand(wand);

    MagickWandTerminus;
  end;
end.

