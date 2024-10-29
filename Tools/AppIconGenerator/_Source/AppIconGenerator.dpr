program AppIconGenerator;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.math,
  System.AnsiStrings,
  System.IOUtils,
  Alcinoe.Files,
  Alcinoe.StringList,
  Alcinoe.Common,
  Alcinoe.StringUtils,
  Alcinoe.ImageMagick;

{***********************************}
procedure ResizeIconSquareBackGround(
            const aSrcIconFilename: String;
            const aDstImgFilename: String;
            const aDstImgformat: String;
            const aDstImgSizes: TArray<Integer>;
            const aDstIconSizes: TArray<Integer>;
            const aDstBackGroundColor: String); overload;
begin

  // Ensure that the number of destination image sizes matches the number of icon sizes
  if length(aDstImgSizes) <> length(aDstIconSizes) then
    Raise Exception.Create('Error: Mismatch between image sizes and icon sizes');

  // Create and initialize MagickWand for container (for multiple image variants)
  var LContainerWand: PMagickWand := nil;
  If length(aDstImgSizes) > 1 then
    LContainerWand := ALImageMagickLib.NewMagickWand;
  try
    For var I := low(aDstImgSizes) to high(aDstImgSizes) do begin

      // Initialize MagickWand and PixelWand objects
      var LWand: PMagickWand := ALImageMagickLib.NewMagickWand;
      var LPixelWand: PPixelWand := ALImageMagickLib.NewPixelWand;
      try

        // Ensure the destination image does not already exist
        if (TFile.Exists(aDstImgFilename)) then Raise Exception.Create('Destination file already exists');

        // Load the source image into the MagickWand
        if ALImageMagickLib.MagickReadImage(LWand, pansiChar(AnsiString(aSrcIconFilename))) <> MagickTrue then RaiseLastMagickWandError(LWand);

        // Resize the image to the specified icon size
        if (ALImageMagickLib.MagickGetImageWidth(LWand) < aDstIconSizes[I]) or
           (ALImageMagickLib.MagickGetImageHeight(LWand) < aDstIconSizes[I]) then raise Exception.Create('Icon dimensions are too small for the requested resize operation');
        if ALImageMagickLib.MagickResizeImage(LWand, aDstIconSizes[I], aDstIconSizes[I], Lanczos2SharpFilter) <> MagickTrue then RaiseLastMagickWandError(LWand);

        // Set the background color of the image
        If ALImageMagickLib.PixelSetColor(LPixelWand, PAnsiChar(AnsiString(aDstBackGroundColor))) <> MagickTrue then RaiseLastPixelWandError(LWand);
        if ALImageMagickLib.MagickSetImageBackgroundColor(LWand, LPixelWand) <> MagickTrue then RaiseLastMagickWandError(LWand);

        // Adjust the image canvas size to the destination size
        var LOffset: integer := (aDstIconSizes[I] - aDstImgSizes[I]) div 2;
        if LOffset > 0 then raise Exception.Create('Invalid size configuration: Image size must be larger than the icon size');
        if ALImageMagickLib.MagickExtentImage(LWand, aDstImgSizes[I], aDstImgSizes[I], LOffset, LOffset) <> MagickTrue then RaiseLastMagickWandError(LWand);

        // If there's only one size variant, save the image immediately
        If length(aDstImgSizes) = 1 then begin

          // Set the output image format (e.g., PNG, ICO)
          if ALImageMagickLib.MagickSetImageFormat(LWand, PAnsiChar(AnsiString(aDstImgformat))) <> MagickTrue then RaiseLastMagickWandError(LWand);

          // Save the composited image to the specified file location
          TDirectory.CreateDirectory(ExtractFilePath(aDstImgFilename));
          if ALImageMagickLib.MagickWriteImage(LWand, pansiChar(AnsiString(aDstImgFilename))) <> MagickTrue then RaiseLastMagickWandError(LWand);

        end

        // Add the image to the container wand for later saving
        else if ALImageMagickLib.MagickAddImage(LContainerWand, LWand) <> MagickTrue then RaiseLastMagickWandError(LContainerWand);

      finally
        ALImageMagickLib.DestroyMagickWand(LWand);
      end;

    end;

    // If there are multiple variants, save them together as a multi-frame image (e.g., ICO)
    If length(aDstImgSizes) > 1 then begin

      // Set the output format for the multi-image container
      if ALImageMagickLib.MagickSetImageFormat(LContainerWand, PAnsiChar(AnsiString(aDstImgformat))) <> MagickTrue then RaiseLastMagickWandError(LContainerWand);

      // Save the composited image to the specified file location
      TDirectory.CreateDirectory(ExtractFilePath(aDstImgFilename));
      if ALImageMagickLib.MagickWriteImages(LContainerWand, pansiChar(AnsiString(aDstImgFilename)), MagickTrue{adjoin}) <> MagickTrue then RaiseLastMagickWandError(LContainerWand);

    end

  finally
    If length(aDstImgSizes) > 1 then
      ALImageMagickLib.DestroyMagickWand(LContainerWand);
  end;

end;

{***********************************}
procedure ResizeIconSquareBackGround(
            const aSrcIconFilename: String;
            const aDstImgFilename: String;
            const aDstImgformat: String;
            const aDstImgSizes: Integer;
            const aDstIconSizes: Integer;
            const aDstBackGroundColor: String); overload;
begin
  ResizeIconSquareBackGround(
    aSrcIconFilename, // const aSrcIconFilename: String;
    aDstImgFilename, // const aDstImgFilename: String;
    aDstImgformat, // const aDstImgformat: String;
    [aDstImgSizes], // const aDstImgSizes: TArray<Integer>;
    [aDstIconSizes], // const aDstIconSizes: TArray<Integer>;
    aDstBackGroundColor); // const aDstBackGroundColor: String);
end;

{*************************************}
procedure ResizeIconSquircleBackGround(
            const aSrcIconFilename: String;
            const aDstImgFilename: String;
            const aDstImgformat: String;
            const aDstImgSizes: TArray<Integer>;
            const aDstIconSizes: TArray<Integer>;
            const aDstBackGroundColor: String); overload;
begin

  // Ensure that the number of destination image sizes matches the number of icon sizes
  if length(aDstImgSizes) <> length(aDstIconSizes) then
    Raise Exception.Create('Error: Mismatch between image sizes and icon sizes');

  // Create and initialize MagickWand for container (for multiple image variants)
  var LContainerWand: PMagickWand := nil;
  If length(aDstImgSizes) > 1 then
    LContainerWand := ALImageMagickLib.NewMagickWand;
  try
    For var I := low(aDstImgSizes) to high(aDstImgSizes) do begin

      // Create and initialize MagickWand for squircle mask and icon
      var LSquircleWand: PMagickWand := ALImageMagickLib.NewMagickWand;
      var LIconWand: PMagickWand := ALImageMagickLib.NewMagickWand;
      try

        // Verify the destination image file does not exist to prevent overwriting
        if (TFile.Exists(aDstImgFilename)) then Raise Exception.Create('Destination file already exists');

        // Set transparent background for the squircle mask
        var LPixelWand := ALImageMagickLib.MagickGetBackgroundColor(LSquircleWand);
        if ALImageMagickLib.PixelSetColor(LPixelWand, 'transparent') <> MagickTrue then RaiseLastPixelWandError(LSquircleWand);
        if ALImageMagickLib.MagickSetBackgroundColor(LSquircleWand, LPixelWand) <> MagickTrue then RaiseLastMagickWandError(LSquircleWand);

        // Load the SVG squircle mask with the specified background color
        var LSquircleMaskSvg: ansiString := '<svg viewBox="0 0 2048 2048" xmlns="http://www.w3.org/2000/svg" version="1.1"><path d="M 0 1024 C 0 256, 256 0, 1024 0 S 2048 256, 2048 1024, 1792 2048 1024 2048, 0 1792, 0 1024" transform="rotate(0,1024,1024)translate(0,0)" fill="'+AnsiString(aDstBackGroundColor)+'"></path></svg>';
        if ALImageMagickLib.MagickReadImageBlob(
             LSquircleWand,
             @LSquircleMaskSvg[low(LSquircleMaskSvg)],
             length(LSquircleMaskSvg)) <> MagickTrue then RaiseLastMagickWandError(LSquircleWand);

        // Resize the squircle mask to the destination image size
        if ALImageMagickLib.MagickResizeImage(LSquircleWand, aDstImgSizes[I], aDstImgSizes[I], LanczosFilter) <> MagickTrue then RaiseLastMagickWandError(LSquircleWand);

        // Load the source icon image
        if ALImageMagickLib.MagickReadImage(LIconWand, pansiChar(AnsiString(aSrcIconFilename))) <> MagickTrue then RaiseLastMagickWandError(LSquircleWand);

        // Ensure the icon is large enough for resizing
        if (ALImageMagickLib.MagickGetImageWidth(LIconWand) < aDstIconSizes[I]) or
           (ALImageMagickLib.MagickGetImageHeight(LIconWand) < aDstIconSizes[I]) then raise Exception.Create('Icon dimensions are too small for the requested resize operation');
        if ALImageMagickLib.MagickResizeImage(LIconWand, aDstIconSizes[I], aDstIconSizes[I], LanczosFilter) <> MagickTrue then RaiseLastMagickWandError(LSquircleWand);

        // Composite the resized icon onto the squircle mask with centered alignment
        if ALImageMagickLib.MagickCompositeImageGravity(LSquircleWand, LIconWand, OverCompositeOp, CenterGravity) <> MagickTrue then RaiseLastMagickWandError(LSquircleWand);

        // If there's only one size variant, save the image immediately
        If length(aDstImgSizes) = 1 then begin

          // Set the output image format (e.g., PNG, ICO)
          if ALImageMagickLib.MagickSetImageFormat(LSquircleWand, PAnsiChar(AnsiString(aDstImgformat))) <> MagickTrue then RaiseLastMagickWandError(LSquircleWand);

          // Save the composited image to the specified file location
          TDirectory.CreateDirectory(ExtractFilePath(aDstImgFilename));
          if ALImageMagickLib.MagickWriteImage(LSquircleWand, pansiChar(AnsiString(aDstImgFilename))) <> MagickTrue then RaiseLastMagickWandError(LSquircleWand);

        end

        // Add the image to the container wand for later saving
        else if ALImageMagickLib.MagickAddImage(LContainerWand, LSquircleWand) <> MagickTrue then RaiseLastMagickWandError(LContainerWand);

      finally
        ALImageMagickLib.DestroyMagickWand(LSquircleWand);
        ALImageMagickLib.DestroyMagickWand(LIconWand);
      end;

    end;

    // If there are multiple variants, save them together as a multi-frame image (e.g., ICO)
    If length(aDstImgSizes) > 1 then begin

      // Set the output format for the multi-image container
      if ALImageMagickLib.MagickSetImageFormat(LContainerWand, PAnsiChar(AnsiString(aDstImgformat))) <> MagickTrue then RaiseLastMagickWandError(LContainerWand);

      // Save the composited image to the specified file location
      TDirectory.CreateDirectory(ExtractFilePath(aDstImgFilename));
      if ALImageMagickLib.MagickWriteImages(LContainerWand, pansiChar(AnsiString(aDstImgFilename)), MagickTrue{adjoin}) <> MagickTrue then RaiseLastMagickWandError(LContainerWand);

    end

  finally
    If length(aDstImgSizes) > 1 then
      ALImageMagickLib.DestroyMagickWand(LContainerWand);
  end;

end;

{*************************************}
procedure ResizeIconSquircleBackGround(
            const aSrcIconFilename: String;
            const aDstImgFilename: String;
            const aDstImgformat: String;
            const aDstImgSizes: Integer;
            const aDstIconSizes: Integer;
            const aDstBackGroundColor: String); overload;
begin
  ResizeIconSquircleBackGround(
    aSrcIconFilename, // const aSrcIconFilename: String;
    aDstImgFilename, // const aDstImgFilename: String;
    aDstImgformat, // const aDstImgformat: String;
    [aDstImgSizes], // const aDstImgSizes: TArray<Integer>;
    [aDstIconSizes], // const aDstIconSizes: TArray<Integer>;
    aDstBackGroundColor); // const aDstBackGroundColor: String); overload;
end;

{****************************}
procedure CreateBackgroundImg(
            const aDstImgFilename: String;
            const aDstImgBackGroundColor: String;
            const aDstImgSize: integer);
begin

  // Initialize MagickWand and PixelWand objects for image creation
  var LWand: PMagickWand := ALImageMagickLib.NewMagickWand;
  var LPixelWand: PPixelWand := ALImageMagickLib.NewPixelWand;
  try

    // Ensure the destination image file does not exist to prevent overwriting
    if (TFile.Exists(aDstImgFilename)) then Raise Exception.Create('Destination file already exists');

    // Create a new image with the specified background color and dimensions
    If ALImageMagickLib.PixelSetColor(LPixelWand, PansiChar(AnsiString(aDstImgBackGroundColor))) <> MagickTrue then RaiseLastPixelWandError(LWand);
    if ALImageMagickLib.MagickNewImage(LWand, aDstImgSize, aDstImgSize, LPixelWand) <> MagickTrue then RaiseLastMagickWandError(LWand);

    // Define the output image format as PNG
    if ALImageMagickLib.MagickSetImageFormat(LWand, 'png') <> MagickTrue then RaiseLastMagickWandError(LWand);

    // Save the newly created image to the specified file path
    TDirectory.CreateDirectory(ExtractFilePath(aDstImgFilename));
    if ALImageMagickLib.MagickWriteImage(LWand, pansiChar(AnsiString(aDstImgFilename))) <> MagickTrue then RaiseLastMagickWandError(LWand);

  finally
    ALImageMagickLib.DestroyMagickWand(LWand);
    ALImageMagickLib.DestroyPixelWand(LPixelWand);
  end;

end;

{********************************************************************************************}
function GetReducedSizeByPadding(Const ASize: Single; const APaddingPercent: single): integer;
begin
  Result := Round(ASize - (((ASize/100)*APaddingPercent) * 2));
end;

{**************************************************************************************************************}
function GetReducedSizesByPadding(Const ASizes: TArray<Single>; const APaddingPercent: single): TArray<Integer>;
begin
  Setlength(Result, length(ASizes));
  For var I := Low(ASizes) to High(ASizes) do
    Result[I] := GetReducedSizeByPadding(ASizes[i], APaddingPercent);
end;

begin

  try

    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    //Create the ImageMagick Library
    {$IFDEF WIN32}
    alCreateImageMagickLibrary(TPath.GetAppPath + '\..\..\Libraries\dll\imagemagick\win32\');
    {$ELSE}
    alCreateImageMagickLibrary(TPath.GetAppPath + '\..\..\Libraries\dll\imagemagick\win64\');
    {$ENDIF}
    try

      var LOutputDir := TPath.GetAppPath + '\Output\';
      if (TDirectory.Exists(LOutputDir)) then
        TDirectory.Delete(LOutputDir, true{recursive});
      if (TDirectory.Exists(LOutputDir)) then
        Raise Exception.Create('Failed to delete output directory');

      Writeln('Enter the path of the launcher icon:');
      var LLauncherIconFilename: String;
      readln(LLauncherIconFilename);
      If not TFile.Exists(LLauncherIconFilename) then
        Raise Exception.Create('Icon file does not exist at the specified path');

      Writeln('');
      Writeln('Enter the color of the launcher icon background (#RRGGBB):');
      var LLauncherIconBackgroundColorStr: String;
      readln(LLauncherIconBackgroundColorStr);

      var LLauncherIconPaddingPercentStr: String := '';
      var LLauncherIconPaddingPercentInt: Integer := 0;
      while not ALTryStrToInt(LLauncherIconPaddingPercentStr, LLauncherIconPaddingPercentInt) do begin
        Writeln('');
        Writeln('Enter the padding of the launcher icon (in percentage):');
        readln(LLauncherIconPaddingPercentStr);
      end;

      Writeln('');
      Writeln('Enter the path of the splashscreen icon:');
      var LsplashscreenIconFilename: String;
      readln(LsplashscreenIconFilename);
      If not TFile.Exists(LsplashscreenIconFilename) then
        Raise Exception.Create('Icon file does not exist at the specified path');

      var LSplashscreenIconHaveBackground: String := '';
      while (not ALSameTextW(LSplashscreenIconHaveBackground, 'Y')) and
            (not ALSameTextW(LSplashscreenIconHaveBackground, 'N')) do begin
        Writeln('');
        Writeln('Does the splashscreen icon have a background (Y/N)?');
        readln(LSplashscreenIconHaveBackground);
      end;

      Writeln('');
      Writeln('Enter the color of the splashscreen background (#RRGGBB):');
      var LsplashscreenBackgroundColorStr: String;
      readln(LsplashscreenBackgroundColorStr);

      Writeln('');
      Writeln('Enter the path of the splashscreen icon for dark mode (leave empty to skip):');
      var LDarkModesplashscreenIconFilename: String;
      readln(LDarkModesplashscreenIconFilename);
      If (LDarkModesplashscreenIconFilename <> '') and (not TFile.Exists(LDarkModesplashscreenIconFilename)) then
        Raise Exception.Create('Icon file does not exist at the specified path');

      var LDarkModeSplashscreenIconHaveBackground: String := '';
      if LDarkModesplashscreenIconFilename <> '' then begin
        while (not ALSameTextW(LDarkModeSplashscreenIconHaveBackground, 'Y')) and
              (not ALSameTextW(LDarkModeSplashscreenIconHaveBackground, 'N')) do begin
          Writeln('');
          Writeln('Does the splashscreen icon for dark mode have a background (Y/N)?');
          readln(LDarkModeSplashscreenIconHaveBackground);
        end;
      end;

      Writeln('');
      Writeln('Enter the color of the splashscreen background for dark mode (#RRGGBB, leave empty to skip):');
      var LDarkModesplashscreenBackgroundColorStr: String := '';
      readln(LDarkModesplashscreenBackgroundColorStr);

      // https://developer.android.com/develop/ui/views/launch/icon_design_adaptive
      CreateBackgroundImg(LOutputDir + '\android\res\mipmap-mdpi\ic_launcher_background.png',    LLauncherIconBackgroundColorStr, Round(108*1){aDstImgSize});
      CreateBackgroundImg(LOutputDir + '\android\res\mipmap-hdpi\ic_launcher_background.png',    LLauncherIconBackgroundColorStr, Round(108*1.5){aDstImgSize});
      CreateBackgroundImg(LOutputDir + '\android\res\mipmap-xhdpi\ic_launcher_background.png',   LLauncherIconBackgroundColorStr, Round(108*2){aDstImgSize});
      CreateBackgroundImg(LOutputDir + '\android\res\mipmap-xxhdpi\ic_launcher_background.png',  LLauncherIconBackgroundColorStr, Round(108*3){aDstImgSize});
      CreateBackgroundImg(LOutputDir + '\android\res\mipmap-xxxhdpi\ic_launcher_background.png', LLauncherIconBackgroundColorStr, Round(108*4){aDstImgSize});

      // https://developer.android.com/develop/ui/views/launch/icon_design_adaptive
      ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + '\android\res\mipmap-mdpi\ic_launcher_foreground.png',    'png'{aDstImgformat}, Round(108*1){aDstImgSize},   Round(66*1){aDstIconSize},   'transparent'{aDstBackGroundColor});
      ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + '\android\res\mipmap-hdpi\ic_launcher_foreground.png',    'png'{aDstImgformat}, Round(108*1.5){aDstImgSize}, Round(66*1.5){aDstIconSize}, 'transparent'{aDstBackGroundColor});
      ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + '\android\res\mipmap-xhdpi\ic_launcher_foreground.png',   'png'{aDstImgformat}, Round(108*2){aDstImgSize},   Round(66*2){aDstIconSize},   'transparent'{aDstBackGroundColor});
      ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + '\android\res\mipmap-xxhdpi\ic_launcher_foreground.png',  'png'{aDstImgformat}, Round(108*3){aDstImgSize},   Round(66*3){aDstIconSize},   'transparent'{aDstBackGroundColor});
      ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + '\android\res\mipmap-xxxhdpi\ic_launcher_foreground.png', 'png'{aDstImgformat}, Round(108*4){aDstImgSize},   Round(66*4){aDstIconSize},   'transparent'{aDstBackGroundColor});

      // https://iconhandbook.co.uk/reference/chart/android/
      ResizeIconSquircleBackGround(LLauncherIconFilename, LOutputDir + '\android\res\mipmap-mdpi\ic_launcher.png',    'png'{aDstImgformat}, Round(48*1){aDstImgSize},   GetReducedSizeByPadding(48*1,   LLauncherIconPaddingPercentInt){aDstIconSize}, LLauncherIconBackgroundColorStr);
      ResizeIconSquircleBackGround(LLauncherIconFilename, LOutputDir + '\android\res\mipmap-hdpi\ic_launcher.png',    'png'{aDstImgformat}, Round(48*1.5){aDstImgSize}, GetReducedSizeByPadding(48*1.5, LLauncherIconPaddingPercentInt){aDstIconSize}, LLauncherIconBackgroundColorStr);
      ResizeIconSquircleBackGround(LLauncherIconFilename, LOutputDir + '\android\res\mipmap-xhdpi\ic_launcher.png',   'png'{aDstImgformat}, Round(48*2){aDstImgSize},   GetReducedSizeByPadding(48*2,   LLauncherIconPaddingPercentInt){aDstIconSize}, LLauncherIconBackgroundColorStr);
      ResizeIconSquircleBackGround(LLauncherIconFilename, LOutputDir + '\android\res\mipmap-xxhdpi\ic_launcher.png',  'png'{aDstImgformat}, Round(48*3){aDstImgSize},   GetReducedSizeByPadding(48*3,   LLauncherIconPaddingPercentInt){aDstIconSize}, LLauncherIconBackgroundColorStr);
      ResizeIconSquircleBackGround(LLauncherIconFilename, LOutputDir + '\android\res\mipmap-xxxhdpi\ic_launcher.png', 'png'{aDstImgformat}, Round(48*4){aDstImgSize},   GetReducedSizeByPadding(48*4,   LLauncherIconPaddingPercentInt){aDstIconSize}, LLauncherIconBackgroundColorStr);

      // https://developer.android.com/develop/ui/views/launch/splash-screen
      if ALSameTextW(LSplashscreenIconHaveBackground, 'Y') then begin
        ResizeIconSquareBackGround(LsplashscreenIconFilename, LOutputDir + '\android\res\drawable-mdpi\splashscreen_icon.png',    'png'{aDstImgformat}, Round(240*1){aDstImgSize},   Round(160*1){aDstIconSize},   'transparent'{aDstBackGroundColor});
        ResizeIconSquareBackGround(LsplashscreenIconFilename, LOutputDir + '\android\res\drawable-hdpi\splashscreen_icon.png',    'png'{aDstImgformat}, Round(240*1.5){aDstImgSize}, Round(160*1.5){aDstIconSize}, 'transparent'{aDstBackGroundColor});
        ResizeIconSquareBackGround(LsplashscreenIconFilename, LOutputDir + '\android\res\drawable-xhdpi\splashscreen_icon.png',   'png'{aDstImgformat}, Round(240*2){aDstImgSize},   Round(160*2){aDstIconSize},   'transparent'{aDstBackGroundColor});
        ResizeIconSquareBackGround(LsplashscreenIconFilename, LOutputDir + '\android\res\drawable-xxhdpi\splashscreen_icon.png',  'png'{aDstImgformat}, Round(240*3){aDstImgSize},   Round(160*3){aDstIconSize},   'transparent'{aDstBackGroundColor});
        ResizeIconSquareBackGround(LsplashscreenIconFilename, LOutputDir + '\android\res\drawable-xxxhdpi\splashscreen_icon.png', 'png'{aDstImgformat}, Round(240*4){aDstImgSize},   Round(160*4){aDstIconSize},   'transparent'{aDstBackGroundColor});
      end
      else begin
        ResizeIconSquareBackGround(LsplashscreenIconFilename, LOutputDir + '\android\res\drawable-mdpi\splashscreen_icon.png',    'png'{aDstImgformat}, Round(288*1){aDstImgSize},   Round(192*1){aDstIconSize},   'transparent'{aDstBackGroundColor});
        ResizeIconSquareBackGround(LsplashscreenIconFilename, LOutputDir + '\android\res\drawable-hdpi\splashscreen_icon.png',    'png'{aDstImgformat}, Round(288*1.5){aDstImgSize}, Round(192*1.5){aDstIconSize}, 'transparent'{aDstBackGroundColor});
        ResizeIconSquareBackGround(LsplashscreenIconFilename, LOutputDir + '\android\res\drawable-xhdpi\splashscreen_icon.png',   'png'{aDstImgformat}, Round(288*2){aDstImgSize},   Round(192*2){aDstIconSize},   'transparent'{aDstBackGroundColor});
        ResizeIconSquareBackGround(LsplashscreenIconFilename, LOutputDir + '\android\res\drawable-xxhdpi\splashscreen_icon.png',  'png'{aDstImgformat}, Round(288*3){aDstImgSize},   Round(192*3){aDstIconSize},   'transparent'{aDstBackGroundColor});
        ResizeIconSquareBackGround(LsplashscreenIconFilename, LOutputDir + '\android\res\drawable-xxxhdpi\splashscreen_icon.png', 'png'{aDstImgformat}, Round(288*4){aDstImgSize},   Round(192*4){aDstIconSize},   'transparent'{aDstBackGroundColor});
      end;

      // https://developer.android.com/develop/ui/views/launch/splash-screen
      if LDarkModesplashscreenIconFilename <> '' then begin
        if ALSameTextW(LDarkModeSplashscreenIconHaveBackground, 'Y') then begin
          ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, LOutputDir + '\android\res\drawable-night-mdpi\splashscreen_icon.png',    'png'{aDstImgformat}, Round(240*1){aDstImgSize},   Round(160*1){aDstIconSize},   'transparent'{aDstBackGroundColor});
          ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, LOutputDir + '\android\res\drawable-night-hdpi\splashscreen_icon.png',    'png'{aDstImgformat}, Round(240*1.5){aDstImgSize}, Round(160*1.5){aDstIconSize}, 'transparent'{aDstBackGroundColor});
          ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, LOutputDir + '\android\res\drawable-night-xhdpi\splashscreen_icon.png',   'png'{aDstImgformat}, Round(240*2){aDstImgSize},   Round(160*2){aDstIconSize},   'transparent'{aDstBackGroundColor});
          ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, LOutputDir + '\android\res\drawable-night-xxhdpi\splashscreen_icon.png',  'png'{aDstImgformat}, Round(240*3){aDstImgSize},   Round(160*3){aDstIconSize},   'transparent'{aDstBackGroundColor});
          ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, LOutputDir + '\android\res\drawable-night-xxxhdpi\splashscreen_icon.png', 'png'{aDstImgformat}, Round(240*4){aDstImgSize},   Round(160*4){aDstIconSize},   'transparent'{aDstBackGroundColor});
        end
        else begin
          ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, LOutputDir + '\android\res\drawable-night-mdpi\splashscreen_icon.png',    'png'{aDstImgformat}, Round(288*1){aDstImgSize},   Round(192*1){aDstIconSize},   'transparent'{aDstBackGroundColor});
          ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, LOutputDir + '\android\res\drawable-night-hdpi\splashscreen_icon.png',    'png'{aDstImgformat}, Round(288*1.5){aDstImgSize}, Round(192*1.5){aDstIconSize}, 'transparent'{aDstBackGroundColor});
          ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, LOutputDir + '\android\res\drawable-night-xhdpi\splashscreen_icon.png',   'png'{aDstImgformat}, Round(288*2){aDstImgSize},   Round(192*2){aDstIconSize},   'transparent'{aDstBackGroundColor});
          ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, LOutputDir + '\android\res\drawable-night-xxhdpi\splashscreen_icon.png',  'png'{aDstImgformat}, Round(288*3){aDstImgSize},   Round(192*3){aDstIconSize},   'transparent'{aDstBackGroundColor});
          ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, LOutputDir + '\android\res\drawable-night-xxxhdpi\splashscreen_icon.png', 'png'{aDstImgformat}, Round(288*4){aDstImgSize},   Round(192*4){aDstIconSize},   'transparent'{aDstBackGroundColor});
        end;
      end;

      // splashscreen.xml
      ALSaveStringTofile(
        '<?xml version="1.0" encoding="utf-8"?>'+#13#10+
        '<layer-list xmlns:android="http://schemas.android.com/apk/res/android" android:opacity="opaque">'+#13#10+
        '  <item android:drawable="@color/splashscreen_background" />'+#13#10+
        '  <item>'+#13#10+
        '    <bitmap android:src="@drawable/splashscreen_icon"'+#13#10+
        '            android:antialias="true"'+#13#10+
        '            android:dither="true"'+#13#10+
        '            android:filter="true"'+#13#10+
        '            android:gravity="center"'+#13#10+
        '            android:tileMode="disabled"/>'+#13#10+
        '  </item>'+#13#10+
        '</layer-list>',
        LOutputDir + '\android\res\drawable\splashscreen.xml');

       // ic_launcher.xml
       ALSaveStringTofile(
         '<?xml version="1.0" encoding="utf-8"?>'+#13#10+
         '<adaptive-icon xmlns:android="http://schemas.android.com/apk/res/android">'+#13#10+
         '  <background android:drawable="@mipmap/ic_launcher_background"/>'+#13#10+
         '  <foreground android:drawable="@mipmap/ic_launcher_foreground"/>'+#13#10+
         '</adaptive-icon>',
         LOutputDir + '\android\res\mipmap-anydpi-v26\ic_launcher.xml');

       // colors.xml
       ALSaveStringTofile(
         '<?xml version="1.0" encoding="utf-8"?>'+#13#10+
         '<resources xmlns:android="http://schemas.android.com/apk/res/android">'+#13#10+
         '  <color name="splashscreen_background">'+ansiString(LsplashscreenBackgroundColorStr)+'</color>'+#13#10+
         '</resources>',
         LOutputDir + '\android\res\values\colors.xml');

       // values-night/colors.xml
       ALSaveStringTofile(
         '<?xml version="1.0" encoding="utf-8"?>'+#13#10+
         '<resources xmlns:android="http://schemas.android.com/apk/res/android">'+#13#10+
         '  <color name="splashscreen_background">'+ansiString(LDarkModeSplashscreenBackgroundColorStr)+'</color>'+#13#10+
         '</resources>',
         LOutputDir + '\android\res\values-night\colors.xml');

         // styles.xml
       ALSaveStringTofile(
         '<?xml version="1.0" encoding="utf-8"?>'+#13#10+
         '<resources>'+#13#10+
         ''+#13#10+
         '  <!-- default Theme -->'+#13#10+
         '  <style name="AppTheme" parent="@android:style/Theme.Material.Light.NoActionBar">'+#13#10+
         '    <item name="android:windowBackground">@drawable/splashscreen</item>'+#13#10+
         '  </style>'+#13#10+
         ''+#13#10+
         '</resources>',
         LOutputDir + '\android\res\values\styles.xml');

         // values-v31/styles.xml
       ALSaveStringTofile(
         '<?xml version="1.0" encoding="utf-8"?>'+#13#10+
         '<resources>'+#13#10+
         ''+#13#10+
         '  <!-- default Theme -->'+#13#10+
         '  <style name="AppTheme" parent="@android:style/Theme.Material.Light.NoActionBar">'+#13#10+
         '    <item name="android:windowSplashScreenBackground">@color/splashscreen_background</item>'+#13#10+
         '    <item name="android:windowSplashScreenAnimatedIcon">@drawable/splashscreen_icon</item>'+#13#10+
         '  </style>'+#13#10+
         ''+#13#10+
         '</resources>',
         LOutputDir + '\android\res\values-v31\styles.xml');

       ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + '\ios\iphone_application_icon_120x120.png',      'png'{aDstImgformat},  120{aDstImgSize},  GetReducedSizeByPadding(120, LLauncherIconPaddingPercentInt){aDstIconSize},  LLauncherIconBackgroundColorStr{aDstBackGroundColor});
       ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + '\ios\iphone_application_icon_180x180.png',      'png'{aDstImgformat},  180{aDstImgSize},  GetReducedSizeByPadding(180, LLauncherIconPaddingPercentInt){aDstIconSize},  LLauncherIconBackgroundColorStr{aDstBackGroundColor});
       ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + '\ios\iphone_application_icon_1024x1024.png',    'png'{aDstImgformat}, 1024{aDstImgSize},  GetReducedSizeByPadding(1024, LLauncherIconPaddingPercentInt){aDstIconSize}, LLauncherIconBackgroundColorStr{aDstBackGroundColor});
       ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + '\ios\iphone_spotlight_search_icon_80x80.png',   'png'{aDstImgformat},   80{aDstImgSize},  GetReducedSizeByPadding(80, LLauncherIconPaddingPercentInt){aDstIconSize},   LLauncherIconBackgroundColorStr{aDstBackGroundColor});
       ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + '\ios\iphone_spotlight_search_icon_120x120.png', 'png'{aDstImgformat},  120{aDstImgSize},  GetReducedSizeByPadding(120, LLauncherIconPaddingPercentInt){aDstIconSize},  LLauncherIconBackgroundColorStr{aDstBackGroundColor});
       ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + '\ios\iphone_setting_icon_58x58.png',            'png'{aDstImgformat},   58{aDstImgSize},  GetReducedSizeByPadding(58, LLauncherIconPaddingPercentInt){aDstIconSize},   LLauncherIconBackgroundColorStr{aDstBackGroundColor});
       ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + '\ios\iphone_setting_icon_87x87.png',            'png'{aDstImgformat},   87{aDstImgSize},  GetReducedSizeByPadding(87, LLauncherIconPaddingPercentInt){aDstIconSize},   LLauncherIconBackgroundColorStr{aDstBackGroundColor});
       ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + '\ios\iphone_notification_icon_40x40.png',       'png'{aDstImgformat},   40{aDstImgSize},  GetReducedSizeByPadding(40, LLauncherIconPaddingPercentInt){aDstIconSize},   LLauncherIconBackgroundColorStr{aDstBackGroundColor});
       ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + '\ios\iphone_notification_icon_60x60.png',       'png'{aDstImgformat},   60{aDstImgSize},  GetReducedSizeByPadding(60, LLauncherIconPaddingPercentInt){aDstIconSize},   LLauncherIconBackgroundColorStr{aDstBackGroundColor});
       ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + '\ios\ipad_application_icon_152x152.png',    'png'{aDstImgformat},  152{aDstImgSize},  GetReducedSizeByPadding(152, LLauncherIconPaddingPercentInt){aDstIconSize},  LLauncherIconBackgroundColorStr{aDstBackGroundColor});
       ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + '\ios\ipad_application_icon_167x167.png',    'png'{aDstImgformat},  167{aDstImgSize},  GetReducedSizeByPadding(167, LLauncherIconPaddingPercentInt){aDstIconSize},  LLauncherIconBackgroundColorStr{aDstBackGroundColor});
       ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + '\ios\ipad_spotlight_search_icon_80x80.png', 'png'{aDstImgformat},   80{aDstImgSize},  GetReducedSizeByPadding(80, LLauncherIconPaddingPercentInt){aDstIconSize},   LLauncherIconBackgroundColorStr{aDstBackGroundColor});
       ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + '\ios\ipad_setting_icon_58x58.png',          'png'{aDstImgformat},   58{aDstImgSize},  GetReducedSizeByPadding(58, LLauncherIconPaddingPercentInt){aDstIconSize},   LLauncherIconBackgroundColorStr{aDstBackGroundColor});
       ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + '\ios\ipad_notification_icon_40x40.png',     'png'{aDstImgformat},   40{aDstImgSize},  GetReducedSizeByPadding(40, LLauncherIconPaddingPercentInt){aDstIconSize},   LLauncherIconBackgroundColorStr{aDstBackGroundColor});

       ResizeIconSquareBackGround(LsplashscreenIconFilename,         LOutputDir + '\ios\iphone_launch_image_2x.png',        'png'{aDstImgformat},  192*2{aDstImgSize},  192*2{aDstIconSize}, 'transparent'{aDstBackGroundColor});
       ResizeIconSquareBackGround(LsplashscreenIconFilename,         LOutputDir + '\ios\iphone_launch_image_3x.png',        'png'{aDstImgformat},  192*3{aDstImgSize},  192*3{aDstIconSize}, 'transparent'{aDstBackGroundColor});
       ResizeIconSquareBackGround(LsplashscreenIconFilename,         LOutputDir + '\ios\ipad_launch_image_2x.png',      'png'{aDstImgformat},  347*2{aDstImgSize},  347*2{aDstIconSize}, 'transparent'{aDstBackGroundColor});
       ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, LOutputDir + '\ios\iphone_launch_image_dark_2x.png',   'png'{aDstImgformat},  192*2{aDstImgSize},  192*2{aDstIconSize}, 'transparent'{aDstBackGroundColor});
       ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, LOutputDir + '\ios\iphone_launch_image_dark_3x.png',   'png'{aDstImgformat},  192*3{aDstImgSize},  192*3{aDstIconSize}, 'transparent'{aDstBackGroundColor});
       ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, LOutputDir + '\ios\ipad_launch_image_dark_2x.png', 'png'{aDstImgformat},  347*2{aDstImgSize},  347*2{aDstIconSize}, 'transparent'{aDstBackGroundColor});

       ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + 'app.ico', 'ico'{aDstImgformat},  [16, 24, 32, 48, 64, 128, 256, 512, 1024]{aDstImgSize}, [16, 24, 32, 48, 64, 128, 256, 512, 1024]{aDstIconSizes}, 'transparent'{aDstBackGroundColor});

       ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + 'uwp_logo_150x150.png', 'png'{aDstImgformat}, 150{aDstImgSize}, 150{aDstIconSize}, 'transparent'{aDstBackGroundColor});
       ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + 'uwp_logo_44x44.png',   'png'{aDstImgformat}, 44{aDstImgSize},  44{aDstIconSize},  'transparent'{aDstBackGroundColor});

       ResizeIconSquareBackGround(LLauncherIconFilename, LOutputDir + 'play_store_512.png', 'png'{aDstImgformat}, 512{aDstImgSize},  GetReducedSizeByPadding(512, LLauncherIconPaddingPercentInt){aDstIconSize}, LLauncherIconBackgroundColorStr{aDstBackGroundColor});

       Writeln('');
       Writeln('Finished');
       Writeln('Press <Enter> key to quit');
       Readln;

    finally
      //free the ImageMagickLibrary
      alFreeImageMagickLibrary;
    end;

  except
    on E: Exception do begin
      Writeln(E.Message);
      ExitCode := 1;
    end;
  end;

end.
