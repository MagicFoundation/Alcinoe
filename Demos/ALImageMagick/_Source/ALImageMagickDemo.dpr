program AlcinoeImageMagickDemo;

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.IOUtils,
  System.StrUtils,
  System.Rtti,
  System.TypInfo,
  Alcinoe.FileUtils,
  Alcinoe.ImageMagick;

type
  TPAnsiCharArray  = array[0..MaxInt div SizeOf(PAnsiChar) - 1] of PAnsiChar;
  PPAnsiCharArray = ^TPAnsiCharArray;

{********************************************************************}
function ReadLineWithDefault(const APrompt, ADefault: string): string;
begin
  if ADefault <> '' then Write(APrompt + ' (Default: ', ADefault, '): ')
  else Write(APrompt + ': ');
  Readln(Result);
  Result := Trim(Result);
  if (Result = '') and (ADefault <> '') then Result := ADefault;
end;

{***********************************}
procedure ReadWidthHeightWithDefault(
            const APrompt: string;
            const ADefaultWidth: Integer;
            const ADefaultHeight: Integer;
            out AWidth, AHeight: Integer);
begin
  var LDefault := Format('%dx%d', [ADefaultWidth, ADefaultHeight]);
  while True do begin
    var LInput := ReadLineWithDefault(APrompt + ' (format: WIDTHxHEIGHT)', LDefault);
    var LParts := LInput.Split(['x', 'X', '*']);
    if Length(LParts) = 2 then begin
      var LWidth := StrToIntDef(Trim(LParts[0]), 0);
      var LHeight := StrToIntDef(Trim(LParts[1]), 0);
      if (LWidth > 0) and (LHeight > 0) then begin
        AWidth := LWidth;
        AHeight := LHeight;
        Exit;
      end;
    end;
    Writeln('Invalid size, please enter e.g. 800x600 (or leave empty for ', LDefault, ').');
  end;
end;

{*************************************************************************}
function ReadFilterTypeWithDefault(const ADefault: FilterType): FilterType;
begin
  Writeln;
  Writeln('  Available filters:');
  for var I := Ord(Low(FilterType)) to Ord(High(FilterType)) do begin
    var LFilter := FilterType(I);
    if LFilter = UndefinedFilter then Writeln(Format('%4d) %s', [I, 'All']))
    else begin
      if LFilter = SentinelFilter then Continue;
      var LName := GetEnumName(System.TypeInfo(FilterType), I);
      Writeln(Format('%4d) %s', [I, string(LName)]));
    end;
  end;
  Writeln;

  while True do begin
    Write(Format('Enter filter number (default: %d = %s): ', [Ord(ADefault), string(GetEnumName(System.TypeInfo(FilterType), Ord(ADefault)))]));

    var LLine: string;
    Readln(LLine);
    LLine := LLine.Trim;

    if LLine = '' then Exit(ADefault);

    var LIndex: Integer;
    if TryStrToInt(LLine, LIndex) and
       (LIndex >= Ord(Low(FilterType))) and
       (LIndex <= Ord(High(FilterType))) and
       (FilterType(LIndex) <> SentinelFilter) then
      Exit(FilterType(LIndex));

    Writeln('Invalid filter selection, please try again.');
  end;
end;

{**********************************************************************}
function ReadDoubleWithDefault(const APrompt, ADefault: string): Double;
begin
  var LInput := ReadLineWithDefault(APrompt, ADefault);
  Result := StrToFloatDef(LInput, StrToFloatDef(ADefault, 1.0));
end;

{**********************}
procedure ConvertFormat(
            const ASrcFilename: string;
            const ADstDir: string;
            const ADstFormat: string);
begin
  if not FileExists(ASrcFilename) then raise Exception.CreateFmt('Source file not found: %s', [ASrcFilename]);

  // Create a new MagickWand instance and check that it was allocated correctly
  var LWand := ALCheckMagickWandResult(NewMagickWand);
  try

    // Load the source image into the wand
    ALCheckMagickWandResult(MagickReadImage(LWand, PAnsiChar(AnsiString(ASrcFilename))), LWand);

    // Set the desired output format (png, jpg, webp, pdf, ...)
    ALCheckMagickWandResult(MagickSetImageFormat(LWand, PAnsiChar(AnsiString(ADstFormat))), LWand);

    // Build the full output filename and write the converted image to disk
    var LBase := TPath.GetFileNameWithoutExtension(ASrcFilename);
    var LFullOut := TPath.Combine(ADstDir, LBase + '.' + LowerCase(ADstFormat));
    if TFile.Exists(LFullOut) then TFile.Delete(LFullOut);
    ALCheckMagickWandResult(MagickWriteImage(LWand, PAnsiChar(AnsiString(LFullOut))), LWand);

    Writeln('Created: ', LFullOut);

  finally
    // Free the MagickWand and release all associated resources
    DestroyMagickWand(LWand);
  end;
end;

{************************}
procedure ResizeImageDemo(
            const ASrcFilename: string;
            const ADstDir: string;
            const ADstFormat: string;
            const AWidth, AHeight: Integer;
            const AFilterType: FilterType);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure DoResize(const AFilter: FilterType);
  begin
    // Create a new MagickWand instance and check that it was allocated correctly
    var LWand := ALCheckMagickWandResult(NewMagickWand);
    try

      // Load the source image into the wand
      ALCheckMagickWandResult(MagickReadImage(LWand, PAnsiChar(AnsiString(ASrcFilename))), LWand);

      // Get original size
      var LSrcWidth := MagickGetImageWidth(LWand);
      var LSrcHeight := MagickGetImageHeight(LWand);
      if (LSrcWidth = 0) or (LSrcHeight = 0) then raise Exception.Create('Source image has invalid dimensions.');

      // Compute scale to fit inside AWidth x AHeight while keeping ratio
      var LScaleW := Double(AWidth) / Double(LSrcWidth);
      var LScaleH := Double(AHeight) / Double(LSrcHeight);
      var LScale := LScaleW;
      if LScaleH < LScale then LScale := LScaleH;
      if LScale <= 0 then raise Exception.Create('Computed scale factor is not positive.');
      var LTargetWidth := Round(Double(LSrcWidth) * LScale);
      var LTargetHeight := Round(Double(LSrcHeight) * LScale);
      if LTargetWidth < 1 then LTargetWidth := 1;
      if LTargetHeight < 1 then LTargetHeight := 1;

      // Resize
      ALCheckMagickWandResult(MagickResizeImage(LWand, LTargetWidth, LTargetHeight, AFilter), LWand);

      // Set output format
      ALCheckMagickWandResult(MagickSetImageFormat(LWand, PAnsiChar(AnsiString(ADstFormat))),LWand);

      // Build output filename
      var LBase := TPath.GetFileNameWithoutExtension(ASrcFilename);
      var LFullOut := TPath.Combine(ADstDir,Format('%s_%dx%d_%s%s', [LBase, LTargetWidth, LTargetHeight, GetEnumName(System.TypeInfo(FilterType), Ord(AFilter)), '.' + LowerCase(ADstFormat)]));
      var LDstAnsi := AnsiString(LFullOut);

      // Save
      if TFile.Exists(LFullOut) then TFile.Delete(LFullOut);
      ALCheckMagickWandResult(MagickWriteImage(LWand, PAnsiChar(LDstAnsi)), LWand);

      Writeln('Created: ', LFullOut);

    finally
      DestroyMagickWand(LWand);
    end;
  end;

begin
  if not TFile.Exists(ASrcFilename) then raise Exception.CreateFmt('Source file not found: %s', [ASrcFilename]);
  if AFilterType = UndefinedFilter then begin
    for var I := Ord(Low(FilterType)) to Ord(High(FilterType)) do begin
      var LFilter := FilterType(I);
      if (LFilter = UndefinedFilter) or (LFilter = SentinelFilter) then Continue;
      DoResize(LFilter);
    end;
  end
  else
    DoResize(AFilterType);
end;

{***************************}
procedure CombineImagesToPdf(
            const ASrcFiles: TArray<string>;
            const ADstDir: string);
begin
  if Length(ASrcFiles) = 0 then raise Exception.Create('No source files specified.');

  // Create a new MagickWand to hold all images
  var LWand := ALCheckMagickWandResult(NewMagickWand);
  try

    // Read each source image and append it to the wand
    for var I := 0 to High(ASrcFiles) do begin
      var LFile := ASrcFiles[I].Trim;
      if LFile = '' then Continue;
      if not TFile.Exists(LFile) then raise Exception.CreateFmt('Source file not found: %s', [LFile]);
      ALCheckMagickWandResult(MagickReadImage(LWand, PAnsiChar(AnsiString(LFile))), LWand);
    end;

    // Set output format to PDF for the whole wand
    var LFormatAnsi := AnsiString('pdf');
    ALCheckMagickWandResult(MagickSetImageFormat(LWand, PAnsiChar(LFormatAnsi)), LWand);

    // Write all images in the wand as a multi-page PDF
    var LOutFile := 'combined.pdf';
    var LFullOut := TPath.Combine(ADstDir, LOutFile);
    var LDstAnsi := AnsiString(LFullOut);
    if TFile.Exists(LFullOut) then TFile.Delete(LFullOut);
    ALCheckMagickWandResult(MagickWriteImages(LWand, PAnsiChar(LDstAnsi), MagickTrue), LWand);

    Writeln('Created: ', LFullOut);

  finally
    // Always destroy the wand to free resources
    DestroyMagickWand(LWand);
  end;
end;

{**********************}
procedure ApplyOilPaint(
            const ASrcFilename: string;
            const ADstDir: string;
            const ARadius: Double;
            const ASigma: Double);
begin
  if not TFile.Exists(ASrcFilename) then raise Exception.CreateFmt('Source file not found: %s', [ASrcFilename]);

  // Create a new wand to hold the image
  var LWand := ALCheckMagickWandResult(NewMagickWand);
  try

    // Load source image into the wand
    ALCheckMagickWandResult(MagickReadImage(LWand, PAnsiChar(AnsiString(ASrcFilename))), LWand);

    // Apply oil paint effect with the given radius and sigma
    ALCheckMagickWandResult(MagickOilPaintImage(LWand, ARadius, ASigma), LWand);

    // Set the output format to PNG
    ALCheckMagickWandResult(MagickSetImageFormat(LWand, 'png'), LWand);

    // Overwrite the output file if it already exists
    var LBase := TPath.GetFileNameWithoutExtension(ASrcFilename);
    var LFullOut := TPath.Combine(ADstDir, LBase + '_oilpaint.png');
    if TFile.Exists(LFullOut) then TFile.Delete(LFullOut);
    ALCheckMagickWandResult(MagickWriteImage(LWand, PAnsiChar(AnsiString(LFullOut))), LWand);

    Writeln('Created: ', LFullOut);

  finally
    // Always destroy the wand to free memory
    DestroyMagickWand(LWand);
  end;
end;

{****************************************************}
procedure PrintImageMetadata(const AFilename: string);
begin
  if not TFile.Exists(AFilename) then raise Exception.CreateFmt('Source file not found: %s', [AFilename]);

  // Create a new MagickWand
  var LWand := ALCheckMagickWandResult(NewMagickWand);
  try

    // Load the image into the wand
    ALCheckMagickWandResult(MagickReadImage(LWand, PAnsiChar(AnsiString(AFilename))), LWand);

    // --- Basic info ---
    var LWidth := MagickGetImageWidth(LWand);
    var LHeight := MagickGetImageHeight(LWand);

    Writeln;
    Writeln('Basic information:');
    Writeln('  Width : ', LWidth);
    Writeln('  Height: ', LHeight);

    // --- Image properties (EXIF, etc.) ---
    Writeln;
    Writeln('Image properties (metadata):');

    // Query all properties: pattern "*" means "all"
    var LCount: size_t := 0;
    var LProps := MagickGetImageProperties(LWand, PAnsiChar(AnsiString('*')), @LCount);

    if (LProps = nil) or (LCount = 0) then Writeln('  (No image properties found)')
    else begin

      // LProps is an array of char* ⇒ use same PPAnsiCharArray pattern as MagickQueryFormats
      for var I := 0 to Integer(LCount) - 1 do begin
        var LName := string(AnsiString(PPAnsiCharArray(LProps)^[I]));

        // Get the property value
        var LValuePtr := MagickGetImageProperty(LWand, PAnsiChar(AnsiString(LName)));
        var LValue := string('');
        if LValuePtr <> nil then begin
          LValue := string(AnsiString(LValuePtr));
          // Free the memory allocated by MagickGetImageProperty
          MagickRelinquishMemory(LValuePtr);
        end;

        Writeln('  ', LName, ' = ', LValue);
      end;

      // Free the array of property names
      MagickRelinquishMemory(LProps);

    end;

    Writeln;
    Writeln('Metadata dump finished.');

  finally
    // Always destroy the wand
    DestroyMagickWand(LWand);
  end;
end;

{****************}
procedure Kickoff;
begin
  {$IF defined(Win64)}
  ALCreateImageMagickLibrary(ALGetModulePathW + '..\..\..\..\Libraries\dll\ImageMagick\Win64');
  {$ELSE}
  ALCreateImageMagickLibrary(ALGetModulePathW + '..\..\..\..\Libraries\dll\ImageMagick\Win32');
  {$ENDIF}
  try

    MagickWandGenesis;
    try

      Writeln('Alcinoe / ImageMagick demo');

      var LCount: size_t := 0;
      var LFormats := MagickQueryFormats('*', @LCount);
      if (LFormats = nil) or (LCount = 0) then raise Exception.Create('No formats returned by MagickQueryFormats');
      var LFormatList: string := '';
      for var I := 0 to Integer(LCount) - 1 do
        LFormatList := LFormatList + IFThen(LFormatList <> '', ', ') + string(AnsiString(PPAnsiCharArray(LFormats)^[I]));
      LFormatList := WrapText(LFormatList, 100);

      Writeln;
      Writeln('Supported formats:');
      Writeln(LFormatList);

      Writeln;
      Writeln('Choose an option:');
      Writeln('  1) Convert one format to another format');
      Writeln('  2) Resize an image');
      Writeln('  3) Combine several images into one PDF file');
      Writeln('  4) Apply MagickOilPaintImage to an image');
      Writeln('  5) Print all metadata information of an image');
      Writeln;

      var LChoice: string;
      while True do begin
       LChoice := ReadLineWithDefault('Enter option number', '1');
       if StrToIntDef(LChoice, 0) in [1, 2, 3, 4, 5] then Break;
       Writeln('Invalid choice, please enter a number between 1 and 5.');
      end;

      // --- Convert one format to another ---
      if LChoice = '1' then begin
        var LSrc := ReadLineWithDefault('Enter source image path', '..\..\sample1.svg');
        var LFormat := ReadLineWithDefault('Enter destination format (any format supported, e.g. png, jpg, webp, pdf, svg, ico...)', 'png');
        var LDir := ReadLineWithDefault('Enter destination directory', ALGetModulePathW);
        ConvertFormat(LSrc, LDir, LFormat);
      end

      // --- Resize an image ---
      else if LChoice = '2' then begin
        var LSrc := ReadLineWithDefault('Enter source image path', '..\..\sample2.jpg');
        var LFormat := ReadLineWithDefault('Enter destination format (any format supported, e.g. png, jpg, webp, svg...)', 'png');
        var LWidth, LHeight: Integer;
        ReadWidthHeightWithDefault('Enter destination size', 150, 150, LWidth, LHeight);
        var LFilter := ReadFilterTypeWithDefault(LanczosFilter);
        var LDir := ReadLineWithDefault('Enter destination directory', ALGetModulePathW);
        ResizeImageDemo(LSrc, LDir, LFormat, LWidth, LHeight, LFilter);
      end

      // --- Combine several images into one PDF file ---
      else if LChoice = '3' then begin
        var LImagesStr := ReadLineWithDefault('Enter source images separated by ";"', '..\..\sample2.jpg;..\..\sample3.jpg');
        var LFiles := LImagesStr.Split([';']);
        var LDir := ReadLineWithDefault('Enter destination directory', ALGetModulePathW);
        CombineImagesToPdf(LFiles, LDir);
      end

      // --- Apply MagickOilPaintImage ---
      else if LChoice = '4' then begin
        var LSrc := ReadLineWithDefault('Enter source image path', '..\..\sample2.jpg');
        var LRadius := ReadDoubleWithDefault('Enter oil paint radius', '20.0');
        var LSigma := ReadDoubleWithDefault('Enter oil paint sigma', '2.0');
        var LDir := ReadLineWithDefault('Enter destination directory', ALGetModulePathW);
        ApplyOilPaint(LSrc, LDir, LRadius, LSigma);
      end

      // --- Print all metadata info of an image ---
      else if LChoice = '5' then begin
        var LSrc := ReadLineWithDefault('Enter image path', '..\..\sample2.jpg');
        PrintImageMetadata(LSrc);
      end

      // --- Unknown option: ---
      else
        Writeln('Unknown option: ', LChoice);

      Writeln;
      Writeln('Done. Press <Enter> to exit...');
      Readln;

    finally
      MagickWandTerminus;
    end;
  finally
    ALFreeImageMagickLibrary;
  end;
end;

begin
  try
    KickOff;
  except
    on E: Exception do
    begin
      Writeln;
      Writeln('*** ERROR ***');
      Writeln(E.ClassName, ': ', E.Message);
      Writeln;
      Writeln('Press <Enter> to exit...');
      Readln;
    end;
  end;
end.