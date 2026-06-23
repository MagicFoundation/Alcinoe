program AppIconGenerator;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.math,
  System.AnsiStrings,
  System.IOUtils,
  Alcinoe.FileUtils,
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
    LContainerWand := NewMagickWand;
  try
    For var I := low(aDstImgSizes) to high(aDstImgSizes) do begin

      // Initialize MagickWand and PixelWand objects
      var LWand: PMagickWand := NewMagickWand;
      var LPixelWand: PPixelWand := NewPixelWand;
      try

        // Ensure the destination image does not already exist
        if (TFile.Exists(aDstImgFilename)) then Raise Exception.Create('Destination file already exists');

        // Load the source image into the MagickWand
        ALCheckMagickWandResult(MagickReadImage(LWand, pansiChar(AnsiString(aSrcIconFilename))), LWand);

        // Ensure the source image is square by adding transparent padding if needed
        if (MagickGetImageWidth(LWand) <> MagickGetImageHeight(LWand)) then begin
          ALCheckPixelWandResult(PixelSetColor(LPixelWand, 'transparent'), LPixelWand);
          ALCheckMagickWandResult(MagickSetImageBackgroundColor(LWand, LPixelWand), LWand);
          var LTargetSize: integer;
          var LOffsetX: Integer;
          var LOffsetY: Integer;
          if MagickGetImageWidth(LWand) > MagickGetImageHeight(LWand) then begin
            LTargetSize := MagickGetImageWidth(LWand);
            LOffsetX := 0;
            LOffsetY := (integer(MagickGetImageHeight(LWand)) - LTargetSize) div 2;
          end
          else begin
            LTargetSize := MagickGetImageHeight(LWand);
            LOffsetX := (integer(MagickGetImageWidth(LWand)) - LTargetSize) div 2;
            LOffsetY := 0;
          end;
          ALCheckMagickWandResult(MagickExtentImage(LWand, LTargetSize, LTargetSize, LOffsetX, LOffsetY), LWand);
        end;

        //
        // References:
        //   https://imagick-filters-comparison.netlify.app/
        //   https://usage.imagemagick.org/filter/
        //
        // There is no universally "best" resize filter. For example, Lanczos is
        // excellent for downscaling photographs because it preserves fine detail,
        // but it often produces visible halos or ringing on flat artwork such as
        // monochrome or cartoon-style icons.
        //
        // After comparing against Photoshop's bilinear downscale, the closest match
        // I found is the Box filter:
        //   https://entropymine.com/resamplescope/notes/photoshop/
        //
        // The Box filter has two advantages for icons:
        //   - It avoids excessive blurring.
        //   - It does not introduce harsh edge artifacts.
        //
        // This is particularly useful because the image will be resized again later
        // by the application.
        //
        // However, the Box filter only works well when the source image is much
        // larger than the target size, because it can average many source pixels
        // into each destination pixel. When the resize ratio is small, for example
        // 112x112 -> 104x104, there are too few pixels to average. In that case,
        // Box can behave almost like nearest-neighbor sampling and may destroy
        // anti-aliased edges, especially on diagonal strokes.
        //
        // This produced visually unpleasant, blocky results on some small icons
        // (see: alcinoe}\References\ImageMagickFilterSamples\1_boxfilter.png),
        // so the implementation was reverted to MitchellFilter, which is also the
        // default filter used by ImageMagick when downscaling images with an
        // alpha (transparency) channel.
        //

        // Resize the image to the specified icon size
        if (MagickGetImageWidth(LWand) < aDstIconSizes[I]) or
           (MagickGetImageHeight(LWand) < aDstIconSizes[I]) then raise Exception.Create('Icon dimensions are too small ('+AlIntToStrW(MagickGetImageWidth(LWand))+'x'+AlIntToStrW(MagickGetImageHeight(LWand))+') for the requested resize operation ('+AlIntToStrW(aDstIconSizes[I])+'x'+AlIntToStrW(aDstIconSizes[I])+')');
        ALCheckMagickWandResult(MagickResizeImage(LWand, aDstIconSizes[I], aDstIconSizes[I], MitchellFilter), LWand);

        // Set the background color of the image
        ALCheckPixelWandResult(PixelSetColor(LPixelWand, PAnsiChar(AnsiString(aDstBackGroundColor))), LPixelWand);
        ALCheckMagickWandResult(MagickSetImageBackgroundColor(LWand, LPixelWand), LWand);

        // Adjust the image canvas size to the destination size
        var LOffset: integer := (aDstIconSizes[I] - aDstImgSizes[I]) div 2;
        if LOffset > 0 then raise Exception.Create('Invalid size configuration: Image size must be larger than the icon size');
        ALCheckMagickWandResult(MagickExtentImage(LWand, aDstImgSizes[I], aDstImgSizes[I], LOffset, LOffset), LWand);

        // If there's only one size variant, save the image immediately
        If length(aDstImgSizes) = 1 then begin

          // Set the output image format (e.g., PNG, ICO)
          ALCheckMagickWandResult(MagickSetImageFormat(LWand, PAnsiChar(AnsiString(aDstImgformat))), LWand);

          // Save the composited image to the specified file location
          TDirectory.CreateDirectory(ExtractFilePath(aDstImgFilename));
          ALCheckMagickWandResult(MagickSetOption(LWand, 'png:exclude-chunks', 'date,time'), LWand);
          ALCheckMagickWandResult(MagickWriteImage(LWand, pansiChar(AnsiString(aDstImgFilename))), LWand);

        end

        // Add the image to the container wand for later saving
        else ALCheckMagickWandResult(MagickAddImage(LContainerWand, LWand), LContainerWand);

      finally
        DestroyMagickWand(LWand);
      end;

    end;

    // If there are multiple variants, save them together as a multi-frame image (e.g., ICO)
    If length(aDstImgSizes) > 1 then begin

      // Set the output format for the multi-image container
      ALCheckMagickWandResult(MagickSetImageFormat(LContainerWand, PAnsiChar(AnsiString(aDstImgformat))), LContainerWand);

      // Save the composited image to the specified file location
      TDirectory.CreateDirectory(ExtractFilePath(aDstImgFilename));
      ALCheckMagickWandResult(MagickSetOption(LContainerWand, 'png:exclude-chunks', 'date,time'), LContainerWand);
      ALCheckMagickWandResult(MagickWriteImages(LContainerWand, pansiChar(AnsiString(aDstImgFilename)), MagickTrue{adjoin}), LContainerWand);

    end

  finally
    If length(aDstImgSizes) > 1 then
      DestroyMagickWand(LContainerWand);
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
    LContainerWand := NewMagickWand;
  try
    For var I := low(aDstImgSizes) to high(aDstImgSizes) do begin

      // Create and initialize MagickWand for squircle mask and icon
      var LSquircleWand: PMagickWand := NewMagickWand;
      var LIconWand: PMagickWand := NewMagickWand;
      try

        // Verify the destination image file does not exist to prevent overwriting
        if (TFile.Exists(aDstImgFilename)) then Raise Exception.Create('Destination file already exists');

        // Set transparent background for the squircle mask
        var LPixelWand := MagickGetBackgroundColor(LSquircleWand);
        ALCheckPixelWandResult(PixelSetColor(LPixelWand, 'transparent'), LPixelWand);
        ALCheckMagickWandResult(MagickSetBackgroundColor(LSquircleWand, LPixelWand), LSquircleWand);

        // Load the SVG squircle mask with the specified background color
        var LSquircleMaskSvg: ansiString := '<svg viewBox="0 0 2048 2048" xmlns="http://www.w3.org/2000/svg" version="1.1"><path d="M 0 1024 C 0 256, 256 0, 1024 0 S 2048 256, 2048 1024, 1792 2048 1024 2048, 0 1792, 0 1024" transform="rotate(0,1024,1024)translate(0,0)" fill="'+AnsiString(aDstBackGroundColor)+'"></path></svg>';
        ALCheckMagickWandResult(
          MagickReadImageBlob(
            LSquircleWand,
            @LSquircleMaskSvg[low(LSquircleMaskSvg)],
            length(LSquircleMaskSvg)), LSquircleWand);

        // Resize the squircle mask to the destination image size
        ALCheckMagickWandResult(MagickResizeImage(LSquircleWand, aDstImgSizes[I], aDstImgSizes[I], MitchellFilter), LSquircleWand);

        // Load the source icon image
        ALCheckMagickWandResult(MagickReadImage(LIconWand, pansiChar(AnsiString(aSrcIconFilename))), LIconWand);

        // Ensure the source image is square by adding transparent padding if needed
        if (MagickGetImageWidth(LIconWand) <> MagickGetImageHeight(LIconWand)) then begin
          ALCheckPixelWandResult(PixelSetColor(LPixelWand, 'transparent'), LPixelWand);
          ALCheckMagickWandResult(MagickSetImageBackgroundColor(LIconWand, LPixelWand), LIconWand);
          var LTargetSize: integer;
          var LOffsetX: Integer;
          var LOffsetY: Integer;
          if MagickGetImageWidth(LIconWand) > MagickGetImageHeight(LIconWand) then begin
            LTargetSize := MagickGetImageWidth(LIconWand);
            LOffsetX := 0;
            LOffsetY := (integer(MagickGetImageHeight(LIconWand)) - LTargetSize) div 2;
          end
          else begin
            LTargetSize := MagickGetImageHeight(LIconWand);
            LOffsetX := (integer(MagickGetImageWidth(LIconWand)) - LTargetSize) div 2;
            LOffsetY := 0;
          end;
          ALCheckMagickWandResult(MagickExtentImage(LIconWand, LTargetSize, LTargetSize, LOffsetX, LOffsetY), LIconWand);
        end;

        // Ensure the icon is large enough for resizing
        if (MagickGetImageWidth(LIconWand) < aDstIconSizes[I]) or
           (MagickGetImageHeight(LIconWand) < aDstIconSizes[I]) then raise Exception.Create('Icon dimensions are too small ('+AlIntToStrW(MagickGetImageWidth(LIconWand))+'x'+AlIntToStrW(MagickGetImageHeight(LIconWand))+') for the requested resize operation ('+AlIntToStrW(aDstIconSizes[I])+'x'+AlIntToStrW(aDstIconSizes[I])+')');
        ALCheckMagickWandResult(MagickResizeImage(LIconWand, aDstIconSizes[I], aDstIconSizes[I], MitchellFilter), LIconWand);

        // Composite the resized icon onto the squircle mask with centered alignment
        ALCheckMagickWandResult(MagickCompositeImageGravity(LSquircleWand, LIconWand, OverCompositeOp, CenterGravity), LSquircleWand);

        // If there's only one size variant, save the image immediately
        If length(aDstImgSizes) = 1 then begin

          // Set the output image format (e.g., PNG, ICO)
          ALCheckMagickWandResult(MagickSetImageFormat(LSquircleWand, PAnsiChar(AnsiString(aDstImgformat))), LSquircleWand);

          // Save the composited image to the specified file location
          TDirectory.CreateDirectory(ExtractFilePath(aDstImgFilename));
          ALCheckMagickWandResult(MagickSetOption(LSquircleWand, 'png:exclude-chunks', 'date,time'), LSquircleWand);
          ALCheckMagickWandResult(MagickWriteImage(LSquircleWand, pansiChar(AnsiString(aDstImgFilename))), LSquircleWand);

        end

        // Add the image to the container wand for later saving
        else ALCheckMagickWandResult(MagickAddImage(LContainerWand, LSquircleWand), LContainerWand);

      finally
        DestroyMagickWand(LSquircleWand);
        DestroyMagickWand(LIconWand);
      end;

    end;

    // If there are multiple variants, save them together as a multi-frame image (e.g., ICO)
    If length(aDstImgSizes) > 1 then begin

      // Set the output format for the multi-image container
      ALCheckMagickWandResult(MagickSetImageFormat(LContainerWand, PAnsiChar(AnsiString(aDstImgformat))), LContainerWand);

      // Save the composited image to the specified file location
      TDirectory.CreateDirectory(ExtractFilePath(aDstImgFilename));
      ALCheckMagickWandResult(MagickSetOption(LContainerWand, 'png:exclude-chunks', 'date,time'), LContainerWand);
      ALCheckMagickWandResult(MagickWriteImages(LContainerWand, pansiChar(AnsiString(aDstImgFilename)), MagickTrue{adjoin}), LContainerWand);

    end

  finally
    If length(aDstImgSizes) > 1 then
      DestroyMagickWand(LContainerWand);
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
  var LWand: PMagickWand := NewMagickWand;
  var LPixelWand: PPixelWand := NewPixelWand;
  try

    // Ensure the destination image file does not exist to prevent overwriting
    if (TFile.Exists(aDstImgFilename)) then Raise Exception.Create('Destination file already exists');

    // Create a new image with the specified background color and dimensions
    ALCheckPixelWandResult(PixelSetColor(LPixelWand, PansiChar(AnsiString(aDstImgBackGroundColor))), LPixelWand);
    ALCheckMagickWandResult(MagickNewImage(LWand, aDstImgSize, aDstImgSize, LPixelWand), LWand);

    // Define the output image format as PNG
    ALCheckMagickWandResult(MagickSetImageFormat(LWand, 'png'), LWand);

    // Save the newly created image to the specified file path
    TDirectory.CreateDirectory(ExtractFilePath(aDstImgFilename));
    ALCheckMagickWandResult(MagickSetOption(LWand, 'png:exclude-chunks', 'date,time'), LWand);
    ALCheckMagickWandResult(MagickWriteImage(LWand, pansiChar(AnsiString(aDstImgFilename))), LWand);

  finally
    DestroyMagickWand(LWand);
    DestroyPixelWand(LPixelWand);
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

{*******************************************************}
procedure GenerateLauncherIcon(const aOutputDir: String);
begin

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
    Writeln('https://developer.android.com/develop/ui/views/launch/splash-screen');
    Writeln('* App icon with an icon background: Resize the icon to 240x240 dp');
    Writeln('* App icon without an icon background: Resize the icon to 288x288 dp');
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
      Writeln('https://developer.android.com/develop/ui/views/launch/splash-screen');
      Writeln('* App icon with an icon background: Resize the icon to 240x240 dp');
      Writeln('* App icon without an icon background: Resize the icon to 288x288 dp');
      Writeln('Does the splashscreen icon for dark mode have a background (Y/N)?');
      readln(LDarkModeSplashscreenIconHaveBackground);
    end;
  end;

  Writeln('');
  Writeln('Enter the color of the splashscreen background for dark mode (#RRGGBB, leave empty to skip):');
  var LDarkModesplashscreenBackgroundColorStr: String := '';
  readln(LDarkModesplashscreenBackgroundColorStr);

  // https://developer.android.com/develop/ui/views/launch/icon_design_adaptive
  CreateBackgroundImg(aOutputDir + '\android\res\mipmap-mdpi\ic_launcher_background.png',    LLauncherIconBackgroundColorStr, Round(108*1){aDstImgSize});
  CreateBackgroundImg(aOutputDir + '\android\res\mipmap-hdpi\ic_launcher_background.png',    LLauncherIconBackgroundColorStr, Round(108*1.5){aDstImgSize});
  CreateBackgroundImg(aOutputDir + '\android\res\mipmap-xhdpi\ic_launcher_background.png',   LLauncherIconBackgroundColorStr, Round(108*2){aDstImgSize});
  CreateBackgroundImg(aOutputDir + '\android\res\mipmap-xxhdpi\ic_launcher_background.png',  LLauncherIconBackgroundColorStr, Round(108*3){aDstImgSize});
  CreateBackgroundImg(aOutputDir + '\android\res\mipmap-xxxhdpi\ic_launcher_background.png', LLauncherIconBackgroundColorStr, Round(108*4){aDstImgSize});

  // https://developer.android.com/develop/ui/views/launch/icon_design_adaptive
  ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\android\res\mipmap-mdpi\ic_launcher_foreground.png',    'png'{aDstImgformat}, Round(108*1){aDstImgSize},   Round(66*1){aDstIconSize},   'transparent'{aDstBackGroundColor});
  ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\android\res\mipmap-hdpi\ic_launcher_foreground.png',    'png'{aDstImgformat}, Round(108*1.5){aDstImgSize}, Round(66*1.5){aDstIconSize}, 'transparent'{aDstBackGroundColor});
  ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\android\res\mipmap-xhdpi\ic_launcher_foreground.png',   'png'{aDstImgformat}, Round(108*2){aDstImgSize},   Round(66*2){aDstIconSize},   'transparent'{aDstBackGroundColor});
  ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\android\res\mipmap-xxhdpi\ic_launcher_foreground.png',  'png'{aDstImgformat}, Round(108*3){aDstImgSize},   Round(66*3){aDstIconSize},   'transparent'{aDstBackGroundColor});
  ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\android\res\mipmap-xxxhdpi\ic_launcher_foreground.png', 'png'{aDstImgformat}, Round(108*4){aDstImgSize},   Round(66*4){aDstIconSize},   'transparent'{aDstBackGroundColor});

  // https://iconhandbook.co.uk/reference/chart/android/
  ResizeIconSquircleBackGround(LLauncherIconFilename, aOutputDir + '\android\res\mipmap-mdpi\ic_launcher.png',    'png'{aDstImgformat}, Round(48*1){aDstImgSize},   GetReducedSizeByPadding(48*1,   LLauncherIconPaddingPercentInt){aDstIconSize}, LLauncherIconBackgroundColorStr);
  ResizeIconSquircleBackGround(LLauncherIconFilename, aOutputDir + '\android\res\mipmap-hdpi\ic_launcher.png',    'png'{aDstImgformat}, Round(48*1.5){aDstImgSize}, GetReducedSizeByPadding(48*1.5, LLauncherIconPaddingPercentInt){aDstIconSize}, LLauncherIconBackgroundColorStr);
  ResizeIconSquircleBackGround(LLauncherIconFilename, aOutputDir + '\android\res\mipmap-xhdpi\ic_launcher.png',   'png'{aDstImgformat}, Round(48*2){aDstImgSize},   GetReducedSizeByPadding(48*2,   LLauncherIconPaddingPercentInt){aDstIconSize}, LLauncherIconBackgroundColorStr);
  ResizeIconSquircleBackGround(LLauncherIconFilename, aOutputDir + '\android\res\mipmap-xxhdpi\ic_launcher.png',  'png'{aDstImgformat}, Round(48*3){aDstImgSize},   GetReducedSizeByPadding(48*3,   LLauncherIconPaddingPercentInt){aDstIconSize}, LLauncherIconBackgroundColorStr);
  ResizeIconSquircleBackGround(LLauncherIconFilename, aOutputDir + '\android\res\mipmap-xxxhdpi\ic_launcher.png', 'png'{aDstImgformat}, Round(48*4){aDstImgSize},   GetReducedSizeByPadding(48*4,   LLauncherIconPaddingPercentInt){aDstIconSize}, LLauncherIconBackgroundColorStr);

  // https://developer.android.com/develop/ui/views/launch/splash-screen
  if ALSameTextW(LSplashscreenIconHaveBackground, 'Y') then begin
    ResizeIconSquareBackGround(LsplashscreenIconFilename, aOutputDir + '\android\res\drawable-mdpi\splashscreen_icon.png',    'png'{aDstImgformat}, Round(240*1){aDstImgSize},   Round(160*1){aDstIconSize},   'transparent'{aDstBackGroundColor});
    ResizeIconSquareBackGround(LsplashscreenIconFilename, aOutputDir + '\android\res\drawable-hdpi\splashscreen_icon.png',    'png'{aDstImgformat}, Round(240*1.5){aDstImgSize}, Round(160*1.5){aDstIconSize}, 'transparent'{aDstBackGroundColor});
    ResizeIconSquareBackGround(LsplashscreenIconFilename, aOutputDir + '\android\res\drawable-xhdpi\splashscreen_icon.png',   'png'{aDstImgformat}, Round(240*2){aDstImgSize},   Round(160*2){aDstIconSize},   'transparent'{aDstBackGroundColor});
    ResizeIconSquareBackGround(LsplashscreenIconFilename, aOutputDir + '\android\res\drawable-xxhdpi\splashscreen_icon.png',  'png'{aDstImgformat}, Round(240*3){aDstImgSize},   Round(160*3){aDstIconSize},   'transparent'{aDstBackGroundColor});
    ResizeIconSquareBackGround(LsplashscreenIconFilename, aOutputDir + '\android\res\drawable-xxxhdpi\splashscreen_icon.png', 'png'{aDstImgformat}, Round(240*4){aDstImgSize},   Round(160*4){aDstIconSize},   'transparent'{aDstBackGroundColor});
  end
  else begin
    ResizeIconSquareBackGround(LsplashscreenIconFilename, aOutputDir + '\android\res\drawable-mdpi\splashscreen_icon.png',    'png'{aDstImgformat}, Round(288*1){aDstImgSize},   Round(192*1){aDstIconSize},   'transparent'{aDstBackGroundColor});
    ResizeIconSquareBackGround(LsplashscreenIconFilename, aOutputDir + '\android\res\drawable-hdpi\splashscreen_icon.png',    'png'{aDstImgformat}, Round(288*1.5){aDstImgSize}, Round(192*1.5){aDstIconSize}, 'transparent'{aDstBackGroundColor});
    ResizeIconSquareBackGround(LsplashscreenIconFilename, aOutputDir + '\android\res\drawable-xhdpi\splashscreen_icon.png',   'png'{aDstImgformat}, Round(288*2){aDstImgSize},   Round(192*2){aDstIconSize},   'transparent'{aDstBackGroundColor});
    ResizeIconSquareBackGround(LsplashscreenIconFilename, aOutputDir + '\android\res\drawable-xxhdpi\splashscreen_icon.png',  'png'{aDstImgformat}, Round(288*3){aDstImgSize},   Round(192*3){aDstIconSize},   'transparent'{aDstBackGroundColor});
    ResizeIconSquareBackGround(LsplashscreenIconFilename, aOutputDir + '\android\res\drawable-xxxhdpi\splashscreen_icon.png', 'png'{aDstImgformat}, Round(288*4){aDstImgSize},   Round(192*4){aDstIconSize},   'transparent'{aDstBackGroundColor});
  end;

  // https://developer.android.com/develop/ui/views/launch/splash-screen
  if LDarkModesplashscreenIconFilename <> '' then begin
    if ALSameTextW(LDarkModeSplashscreenIconHaveBackground, 'Y') then begin
      ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, aOutputDir + '\android\res\drawable-night-mdpi\splashscreen_icon.png',    'png'{aDstImgformat}, Round(240*1){aDstImgSize},   Round(160*1){aDstIconSize},   'transparent'{aDstBackGroundColor});
      ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, aOutputDir + '\android\res\drawable-night-hdpi\splashscreen_icon.png',    'png'{aDstImgformat}, Round(240*1.5){aDstImgSize}, Round(160*1.5){aDstIconSize}, 'transparent'{aDstBackGroundColor});
      ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, aOutputDir + '\android\res\drawable-night-xhdpi\splashscreen_icon.png',   'png'{aDstImgformat}, Round(240*2){aDstImgSize},   Round(160*2){aDstIconSize},   'transparent'{aDstBackGroundColor});
      ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, aOutputDir + '\android\res\drawable-night-xxhdpi\splashscreen_icon.png',  'png'{aDstImgformat}, Round(240*3){aDstImgSize},   Round(160*3){aDstIconSize},   'transparent'{aDstBackGroundColor});
      ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, aOutputDir + '\android\res\drawable-night-xxxhdpi\splashscreen_icon.png', 'png'{aDstImgformat}, Round(240*4){aDstImgSize},   Round(160*4){aDstIconSize},   'transparent'{aDstBackGroundColor});
    end
    else begin
      ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, aOutputDir + '\android\res\drawable-night-mdpi\splashscreen_icon.png',    'png'{aDstImgformat}, Round(288*1){aDstImgSize},   Round(192*1){aDstIconSize},   'transparent'{aDstBackGroundColor});
      ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, aOutputDir + '\android\res\drawable-night-hdpi\splashscreen_icon.png',    'png'{aDstImgformat}, Round(288*1.5){aDstImgSize}, Round(192*1.5){aDstIconSize}, 'transparent'{aDstBackGroundColor});
      ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, aOutputDir + '\android\res\drawable-night-xhdpi\splashscreen_icon.png',   'png'{aDstImgformat}, Round(288*2){aDstImgSize},   Round(192*2){aDstIconSize},   'transparent'{aDstBackGroundColor});
      ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, aOutputDir + '\android\res\drawable-night-xxhdpi\splashscreen_icon.png',  'png'{aDstImgformat}, Round(288*3){aDstImgSize},   Round(192*3){aDstIconSize},   'transparent'{aDstBackGroundColor});
      ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, aOutputDir + '\android\res\drawable-night-xxxhdpi\splashscreen_icon.png', 'png'{aDstImgformat}, Round(288*4){aDstImgSize},   Round(192*4){aDstIconSize},   'transparent'{aDstBackGroundColor});
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
    aOutputDir + '\android\res\drawable\splashscreen.xml');

   // ic_launcher.xml
   ALSaveStringTofile(
     '<?xml version="1.0" encoding="utf-8"?>'+#13#10+
     '<adaptive-icon xmlns:android="http://schemas.android.com/apk/res/android">'+#13#10+
     '  <background android:drawable="@mipmap/ic_launcher_background"/>'+#13#10+
     '  <foreground android:drawable="@mipmap/ic_launcher_foreground"/>'+#13#10+
     '</adaptive-icon>',
     aOutputDir + '\android\res\mipmap-anydpi-v26\ic_launcher.xml');

   // colors.xml
   ALSaveStringTofile(
     '<?xml version="1.0" encoding="utf-8"?>'+#13#10+
     '<resources xmlns:android="http://schemas.android.com/apk/res/android">'+#13#10+
     '  <color name="splashscreen_background">'+ansiString(LsplashscreenBackgroundColorStr)+'</color>'+#13#10+
     '</resources>',
     aOutputDir + '\android\res\values\colors.xml');

   // values-night/colors.xml
   if LDarkModesplashscreenBackgroundColorStr <> '' then begin
     ALSaveStringTofile(
       '<?xml version="1.0" encoding="utf-8"?>'+#13#10+
       '<resources xmlns:android="http://schemas.android.com/apk/res/android">'+#13#10+
       '  <color name="splashscreen_background">'+ansiString(LDarkModesplashscreenBackgroundColorStr)+'</color>'+#13#10+
       '</resources>',
       aOutputDir + '\android\res\values-night\colors.xml');
    end;

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
     aOutputDir + '\android\res\values\styles.xml');

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
     aOutputDir + '\android\res\values-v31\styles.xml');

   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\ios\iphone_application_icon_120x120.png',      'png'{aDstImgformat},  120{aDstImgSize},  GetReducedSizeByPadding(120, LLauncherIconPaddingPercentInt){aDstIconSize},  LLauncherIconBackgroundColorStr{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\ios\iphone_application_icon_180x180.png',      'png'{aDstImgformat},  180{aDstImgSize},  GetReducedSizeByPadding(180, LLauncherIconPaddingPercentInt){aDstIconSize},  LLauncherIconBackgroundColorStr{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\ios\iphone_application_icon_1024x1024.png',    'png'{aDstImgformat}, 1024{aDstImgSize},  GetReducedSizeByPadding(1024, LLauncherIconPaddingPercentInt){aDstIconSize}, LLauncherIconBackgroundColorStr{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\ios\iphone_spotlight_search_icon_80x80.png',   'png'{aDstImgformat},   80{aDstImgSize},  GetReducedSizeByPadding(80, LLauncherIconPaddingPercentInt){aDstIconSize},   LLauncherIconBackgroundColorStr{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\ios\iphone_spotlight_search_icon_120x120.png', 'png'{aDstImgformat},  120{aDstImgSize},  GetReducedSizeByPadding(120, LLauncherIconPaddingPercentInt){aDstIconSize},  LLauncherIconBackgroundColorStr{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\ios\iphone_setting_icon_58x58.png',            'png'{aDstImgformat},   58{aDstImgSize},  GetReducedSizeByPadding(58, LLauncherIconPaddingPercentInt){aDstIconSize},   LLauncherIconBackgroundColorStr{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\ios\iphone_setting_icon_87x87.png',            'png'{aDstImgformat},   87{aDstImgSize},  GetReducedSizeByPadding(87, LLauncherIconPaddingPercentInt){aDstIconSize},   LLauncherIconBackgroundColorStr{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\ios\iphone_notification_icon_40x40.png',       'png'{aDstImgformat},   40{aDstImgSize},  GetReducedSizeByPadding(40, LLauncherIconPaddingPercentInt){aDstIconSize},   LLauncherIconBackgroundColorStr{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\ios\iphone_notification_icon_60x60.png',       'png'{aDstImgformat},   60{aDstImgSize},  GetReducedSizeByPadding(60, LLauncherIconPaddingPercentInt){aDstIconSize},   LLauncherIconBackgroundColorStr{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\ios\ipad_application_icon_152x152.png',        'png'{aDstImgformat},  152{aDstImgSize},  GetReducedSizeByPadding(152, LLauncherIconPaddingPercentInt){aDstIconSize},  LLauncherIconBackgroundColorStr{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\ios\ipad_application_icon_167x167.png',        'png'{aDstImgformat},  167{aDstImgSize},  GetReducedSizeByPadding(167, LLauncherIconPaddingPercentInt){aDstIconSize},  LLauncherIconBackgroundColorStr{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\ios\ipad_spotlight_search_icon_80x80.png',     'png'{aDstImgformat},   80{aDstImgSize},  GetReducedSizeByPadding(80, LLauncherIconPaddingPercentInt){aDstIconSize},   LLauncherIconBackgroundColorStr{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\ios\ipad_setting_icon_58x58.png',              'png'{aDstImgformat},   58{aDstImgSize},  GetReducedSizeByPadding(58, LLauncherIconPaddingPercentInt){aDstIconSize},   LLauncherIconBackgroundColorStr{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\ios\ipad_notification_icon_40x40.png',         'png'{aDstImgformat},   40{aDstImgSize},  GetReducedSizeByPadding(40, LLauncherIconPaddingPercentInt){aDstIconSize},   LLauncherIconBackgroundColorStr{aDstBackGroundColor});

   ResizeIconSquareBackGround(LsplashscreenIconFilename,         aOutputDir + '\ios\iphone_launch_image_2x.png',  'png'{aDstImgformat},  192*2{aDstImgSize},  192*2{aDstIconSize}, 'transparent'{aDstBackGroundColor});
   ResizeIconSquareBackGround(LsplashscreenIconFilename,         aOutputDir + '\ios\iphone_launch_image_3x.png',  'png'{aDstImgformat},  192*3{aDstImgSize},  192*3{aDstIconSize}, 'transparent'{aDstBackGroundColor});
   ResizeIconSquareBackGround(LsplashscreenIconFilename,         aOutputDir + '\ios\ipad_launch_image_2x.png',    'png'{aDstImgformat},  347*2{aDstImgSize},  347*2{aDstIconSize}, 'transparent'{aDstBackGroundColor});
   if LDarkModesplashscreenIconFilename <> '' then begin
     ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, aOutputDir + '\ios\iphone_launch_image_dark_2x.png',  'png'{aDstImgformat},  192*2{aDstImgSize},  192*2{aDstIconSize}, 'transparent'{aDstBackGroundColor});
     ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, aOutputDir + '\ios\iphone_launch_image_dark_3x.png',  'png'{aDstImgformat},  192*3{aDstImgSize},  192*3{aDstIconSize}, 'transparent'{aDstBackGroundColor});
     ResizeIconSquareBackGround(LDarkModesplashscreenIconFilename, aOutputDir + '\ios\ipad_launch_image_dark_2x.png',    'png'{aDstImgformat},  347*2{aDstImgSize},  347*2{aDstIconSize}, 'transparent'{aDstBackGroundColor});
   end
   else begin
     ResizeIconSquareBackGround(LsplashscreenIconFilename, aOutputDir + '\ios\iphone_launch_image_dark_2x.png',  'png'{aDstImgformat},  192*2{aDstImgSize},  192*2{aDstIconSize}, 'transparent'{aDstBackGroundColor});
     ResizeIconSquareBackGround(LsplashscreenIconFilename, aOutputDir + '\ios\iphone_launch_image_dark_3x.png',  'png'{aDstImgformat},  192*3{aDstImgSize},  192*3{aDstIconSize}, 'transparent'{aDstBackGroundColor});
     ResizeIconSquareBackGround(LsplashscreenIconFilename, aOutputDir + '\ios\ipad_launch_image_dark_2x.png',    'png'{aDstImgformat},  347*2{aDstImgSize},  347*2{aDstIconSize}, 'transparent'{aDstBackGroundColor});
   end;

   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + 'app.ico', 'ico'{aDstImgformat},  [16, 24, 32, 48, 64, 128, 256]{aDstImgSize}, [16, 24, 32, 48, 64, 128, 256]{aDstIconSizes}, 'transparent'{aDstBackGroundColor});

   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\icon.iconset\icon_16x16.png',      'png'{aDstImgformat},    16{aDstImgSize},   16{aDstIconSize},   'transparent'{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\icon.iconset\icon_32x32.png',      'png'{aDstImgformat},    32{aDstImgSize},   32{aDstIconSize},   'transparent'{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\icon.iconset\icon_64x64.png',      'png'{aDstImgformat},    64{aDstImgSize},   64{aDstIconSize},   'transparent'{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\icon.iconset\icon_128x128.png',    'png'{aDstImgformat},   128{aDstImgSize},  128{aDstIconSize},   'transparent'{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\icon.iconset\icon_256x256.png',    'png'{aDstImgformat},   256{aDstImgSize},  256{aDstIconSize},   'transparent'{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\icon.iconset\icon_512x512.png',    'png'{aDstImgformat},   512{aDstImgSize},  512{aDstIconSize},   'transparent'{aDstBackGroundColor});

   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\icon.iconset\icon_16x16@2x.png',   'png'{aDstImgformat},    32{aDstImgSize},    32{aDstIconSize},  'transparent'{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\icon.iconset\icon_32x32@2x.png',   'png'{aDstImgformat},    64{aDstImgSize},    64{aDstIconSize},  'transparent'{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\icon.iconset\icon_64x64@2x.png',   'png'{aDstImgformat},   128{aDstImgSize},   128{aDstIconSize},  'transparent'{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\icon.iconset\icon_128x128@2x.png', 'png'{aDstImgformat},   256{aDstImgSize},   256{aDstIconSize},  'transparent'{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\icon.iconset\icon_256x256@2x.png', 'png'{aDstImgformat},   512{aDstImgSize},   512{aDstIconSize},  'transparent'{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + '\icon.iconset\icon_512x512@2x.png', 'png'{aDstImgformat},  1024{aDstImgSize},  1024{aDstIconSize},  'transparent'{aDstBackGroundColor});

   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + 'uwp_logo_150x150.png', 'png'{aDstImgformat}, 150{aDstImgSize}, 150{aDstIconSize}, 'transparent'{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + 'uwp_logo_44x44.png',   'png'{aDstImgformat},  44{aDstImgSize},  44{aDstIconSize}, 'transparent'{aDstBackGroundColor});

   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + 'play_store_512.png',   'png'{aDstImgformat}, 512{aDstImgSize}, GetReducedSizeByPadding(512, LLauncherIconPaddingPercentInt){aDstIconSize}, LLauncherIconBackgroundColorStr{aDstBackGroundColor});
   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + 'apple-touch-icon.png', 'png'{aDstImgformat}, 180{aDstImgSize}, 180{aDstIconSize}, 'transparent'{aDstBackGroundColor});

   ResizeIconSquareBackGround(LLauncherIconFilename, aOutputDir + 'favicon.ico', 'ico'{aDstImgformat},  [16, 32, 48]{aDstImgSize}, [16, 32, 48]{aDstIconSizes}, 'transparent'{aDstBackGroundColor});

   ALSaveStringTofile(
     'How to Generate an .icns File'+#13#10+
     '-----------------------------'+#13#10+
     ''+#13#10+
     'On macOS, open a terminal and run:'+#13#10+
     ''+#13#10+
     'iconutil -c icns icon.iconset'+#13#10+
     ''+#13#10+
     'This will generate the .icns file'+#13#10+
     'in the same directory as icon.iconset.',
     aOutputDir + '\Readme.txt');

end;

{***********************************************************}
procedure GenerateNotificationIcon(const aOutputDir: String);
begin

  // https://developer.android.com/develop/ui/views/notifications
  // The notification small icon must be a white/alpha-only image (API 21+).
  // The system ignores all color information and renders only the alpha channel,
  // tinting it with the app accent color. Provide a flat, simple silhouette.

  Writeln('Enter the path of the notification icon (white/alpha-only PNG with transparent background):');
  var LNotificationIconFilename: String;
  readln(LNotificationIconFilename);
  If not TFile.Exists(LNotificationIconFilename) then
    Raise Exception.Create('Icon file does not exist at the specified path');

  // Notification small icon base size: 24dp x 24dp
  // https://developer.android.com/develop/ui/views/notifications/notification-anatomy
  ResizeIconSquareBackGround(LNotificationIconFilename, aOutputDir + '\android\res\drawable-mdpi\ic_notification.png',    'png'{aDstImgformat}, Round(24*1){aDstImgSize},   Round(24*1){aDstIconSize},   'transparent'{aDstBackGroundColor});
  ResizeIconSquareBackGround(LNotificationIconFilename, aOutputDir + '\android\res\drawable-hdpi\ic_notification.png',    'png'{aDstImgformat}, Round(24*1.5){aDstImgSize}, Round(24*1.5){aDstIconSize}, 'transparent'{aDstBackGroundColor});
  ResizeIconSquareBackGround(LNotificationIconFilename, aOutputDir + '\android\res\drawable-xhdpi\ic_notification.png',   'png'{aDstImgformat}, Round(24*2){aDstImgSize},   Round(24*2){aDstIconSize},   'transparent'{aDstBackGroundColor});
  ResizeIconSquareBackGround(LNotificationIconFilename, aOutputDir + '\android\res\drawable-xxhdpi\ic_notification.png',  'png'{aDstImgformat}, Round(24*3){aDstImgSize},   Round(24*3){aDstIconSize},   'transparent'{aDstBackGroundColor});
  ResizeIconSquareBackGround(LNotificationIconFilename, aOutputDir + '\android\res\drawable-xxxhdpi\ic_notification.png', 'png'{aDstImgformat}, Round(24*4){aDstImgSize},   Round(24*4){aDstIconSize},   'transparent'{aDstBackGroundColor});

end;

begin

  try

    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    //Create the ImageMagick Library
    {$IFDEF WIN32}
    alCreateImageMagickLibrary(TPath.GetAppPath + '\..\..\Libraries\dll\ImageMagick\Win32\');
    {$ELSE}
    alCreateImageMagickLibrary(TPath.GetAppPath + '\..\..\Libraries\dll\ImageMagick\Win64\');
    {$ENDIF}
    try

      var LOutputDir := TPath.GetAppPath + '\Output\';
      if (TDirectory.Exists(LOutputDir)) then
        TDirectory.Delete(LOutputDir, true{recursive});
      if (TDirectory.Exists(LOutputDir)) then
        Raise Exception.Create('Failed to delete output directory');

      var LChoice: String := '';
      while (LChoice <> '1') and (LChoice <> '2') do begin
        Writeln('What do you want to generate?');
        Writeln('  1. Launcher icon');
        Writeln('  2. Notification icon');
        readln(LChoice);
      end;
      Writeln('');

      if LChoice = '1' then GenerateLauncherIcon(LOutputDir)
      else GenerateNotificationIcon(LOutputDir);

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
      Writeln('');
      Writeln(E.Message);
      Writeln('Press <Enter> key to quit');
      Readln;
      ExitCode := 1;
    end;
  end;

end.